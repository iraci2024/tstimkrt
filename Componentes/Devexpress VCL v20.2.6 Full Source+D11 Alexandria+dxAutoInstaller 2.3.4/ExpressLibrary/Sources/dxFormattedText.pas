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

unit dxFormattedText;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, Classes, Generics.Collections, SysUtils,
  cxGraphics, cxDrawTextUtils, dxTextViewInfoCache, dxFontHelpers, cxGeometry,
  dxDPIAwareUtils;

type
  TdxFormattedTextRuns = class;
  TdxFormattedTextLayout = class;
  TdxFormattedTextLayoutRow = class;
  TdxFormattedTextLayoutBox = class;
  TdxFormattedCRLayoutBox = class;
  TdxFormattedTextSpaceLayoutBox = class;
  TdxFormattedTextTabLayoutBox = class;
  TdxFormattedTextTextLayoutBox = class;
  TdxFormattedEmptyLayoutBox = class;

  TdxFormattedTextConverterClass = class of TdxFormattedTextConverter;

  IdxFormattedTextOwner = interface // for internal use
    ['{C33BD809-0400-46A8-9029-E9C72579FFA1}']
    function GetFormattedTextFont: TFont;
    function GetFormattedText: string;
    function GetUseFormattedText: Boolean;
    procedure SetFormattedText(const AValue: string);
  end;

  { TdxFormattedTextCharacterProperties }

  TdxFormattedTextCharacterProperties = record // for internal use
  strict private
    function GetDefaultBackgroundColor: TColor;

    function GetBold: Boolean;
    function GetFontSize: Integer;
    function GetItalic: Boolean;
    function GetUnderline: Boolean;
    function GetStrikeout: Boolean;

    procedure SetBold(AValue: Boolean);
    procedure SetFontSize(AValue: Integer);
    procedure SetItalic(AValue: Boolean);
    procedure SetUnderline(AValue: Boolean);
    procedure SetStrikeout(AValue: Boolean);
  public
    BackgroundColor: TColor;
    CharacterFormattingScript: TdxCharacterFormattingScript;
    FontColor: TColor;
    FontName: string;
    FontDoubleSize: Integer;
    FontStyle: TFontStyles;

    function Compare(ACharacterProperties
      : TdxFormattedTextCharacterProperties): Boolean;
    procedure Initialize(AFont: TFont); overload;
    procedure Initialize(AFont: TFont; AFontDoubleSize: Integer); overload;

    property Bold: Boolean read GetBold write SetBold;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property Italic: Boolean read GetItalic write SetItalic;
    property Underline: Boolean read GetUnderline write SetUnderline;
    property Strikeout: Boolean read GetStrikeout write SetStrikeout;
  end;

  { TdxFormattedTextHyperlinkData }

  TdxFormattedTextHyperlinkData = record
    Hyperlink: string;
    BeginBoxIndex: Integer;
    BeginRowIndex: Integer;
    BoxCount: Integer;
  end;

  { TdxFormattedTextCustomPainter }

  TdxFormattedTextCustomPainter = class abstract // for internal use
  public
    procedure ExtTextOut(const ABounds: TRect; AFontHandle: THandle;
      AFontColor: TColor; const AText: string; AGlyphs: PdxWordArray;
      AGlyphCount: Integer; AGlyphWidths: PdxIntegerArray); virtual; abstract;
    procedure FillRect(const ARect: TRect; AColor: TColor); virtual; abstract;
    procedure IntersectClipRect(ARect: TRect); virtual; abstract;
    procedure Restore; virtual; abstract;
    procedure Store; virtual; abstract;
  end;

  { TdxFormattedTextPainter }

  TdxFormattedTextPainter = class(TdxFormattedTextCustomPainter)
  // for internal use
  strict private
    FDCHandle: HDC;
    FSaveDC: HDC;
  public
    constructor Create(ADCHandle: HDC); virtual;

    procedure ExtTextOut(const ABounds: TRect; AFontHandle: THandle;
      AFontColor: TColor; const AText: string; AGlyphs: PdxWordArray;
      AGlyphCount: Integer; AGlyphWidths: PdxIntegerArray); override;
    procedure FillRect(const ARect: TRect; AColor: TColor); override;
    procedure IntersectClipRect(ARect: TRect); override;
    procedure Restore; override;
    procedure Store; override;
  end;

  { TdxFormattedTextLayoutBuilderInitialData }

  TdxFormattedTextLayoutBuilderInitialData = record // for internal use
    Bounds: TRect;
    CharacterProperties: TdxFormattedTextCharacterProperties;
    TextOutFormat: TcxTextOutFormat;
    ScaleFactor: TdxScaleFactor;
    ZoomFactor: Single;
    HyperlinkColor: TColor;

    function Compare(AInitialData
      : TdxFormattedTextLayoutBuilderInitialData): Boolean;
    procedure Initialize(AFont: TFont; const ABounds: TRect;
      ATextOutFormat: TcxTextOutFormat; AScaleFactor: TdxScaleFactor;
      AZoomFactor: Single; const AHyperlinkColor: TColor);
  end;

  { TdxFormattedText }

  TdxFormattedText = class // for internal use
  strict private
    FHyperlinkColor: TColor;
    FInternalRuns: TdxFormattedTextRuns;
    FLayout: TdxFormattedTextLayout;
    FMeasureTrailingSpaces: Boolean;
    FIsTextChanged: Boolean;
    FRuns: TdxFormattedTextRuns;
    FShowAccelChar: Boolean;
    FText: string;
    FUseOfficeFonts: Boolean;

    function GetHasFormatting: Boolean;
    function GetTextSize: TSize;
    procedure InternalDraw(APainter: TdxFormattedTextCustomPainter;
      const APosition: TPoint; AClip: Boolean);
    procedure SetText(const AValue: string);
  protected
    FHasAccelCharRun: Boolean;

    procedure CreateInternalRuns(ADefaultFont: TFont);

    function GetActualFontCache(const ADpi: Single): TdxFontCache;
    property HyperlinkColor: TColor read FHyperlinkColor write FHyperlinkColor;
    property InternalRuns: TdxFormattedTextRuns read FInternalRuns;
    property Layout: TdxFormattedTextLayout read FLayout;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Import(const ASource: string; ADefaultFont: TFont = nil);
      overload;
    procedure Import(const ASource: Variant;
      ADefaultFont: TFont = nil); overload;
    function Export(AConverterClass: TdxFormattedTextConverterClass;
      ADefaultFont: TFont = nil): string;

    procedure CalculateLayout(ACanvas: TCanvas; AFont: TFont;
      const ABounds: TRect; AFormat: TcxTextOutFormat;
      AScaleFactor: TdxScaleFactor = nil; AZoomFactor: Single = 1;
      const AHyperlinkColor: TColor = clDefault);
    procedure Draw(ACanvas: TCanvas; const APosition: TPoint;
      AClip: Boolean = True); overload; inline;
    procedure Draw(APainter: TdxFormattedTextCustomPainter;
      const APosition: TPoint; AClip: Boolean = True); overload; inline;
    function GetBaseLine(ARowIndex: Integer): Integer;
    function GetDisplayText: string;
    function GetUrl(AIndex: Integer): string;
    function HitTest(const APoint: TPoint): TdxFormattedTextLayoutBox;

    property HasFormatting: Boolean read GetHasFormatting;
    property MeasureTrailingSpaces: Boolean read FMeasureTrailingSpaces
      write FMeasureTrailingSpaces;
    property Runs: TdxFormattedTextRuns read FRuns;
    property ShowAccelChar: Boolean read FShowAccelChar write FShowAccelChar;
    property Text: string read FText write SetText;
    property TextSize: TSize read GetTextSize;
    property UseOfficeFonts: Boolean read FUseOfficeFonts write FUseOfficeFonts;
  end;

  { TdxFormattedTextRun }

  TdxFormattedTextRunAction = (traOpen, traClose); // for internal use

  { TdxFormattedTextRun }

  TdxFormattedTextRunClass = class of TdxFormattedTextRun; // for internal use

  TdxFormattedTextRun = class
  strict private
    FAction: TdxFormattedTextRunAction;

    function GetTextFinish: PChar;
  protected
    FTextLength: Integer;
    FTextStart: PChar;

    function Clone: TdxFormattedTextRun; virtual;
    function GetText: string;
  public
    constructor Create(AAction: TdxFormattedTextRunAction);
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); virtual;

    property Action: TdxFormattedTextRunAction read FAction;
    property Text: string read GetText;
    property TextFinish: PChar read GetTextFinish;
    property TextLength: Integer read FTextLength;
    property TextStart: PChar read FTextStart;
  end;

  { TdxFormattedTextNoCodeRun }

  TdxFormattedTextNoCodeRun = class(TdxFormattedTextRun); // for internal use

  { TdxFormattedTextNoParseRun }

  TdxFormattedTextNoParseRun = class(TdxFormattedTextRun); // for internal use

  { TdxFormattedTextBoldRun }

  TdxFormattedTextBoldRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextItalicRun }

  TdxFormattedTextItalicRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextUnderlineRun }

  TdxFormattedTextUnderlineRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextStrikeoutRun }

  TdxFormattedTextStrikeoutRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextColorRun }

  TdxFormattedTextColorRunClass = class of TdxFormattedTextColorRun;
  // for internal use

  TdxFormattedTextColorRun = class(TdxFormattedTextRun) // for internal use
  protected
    FColor: TColor;

    function Clone: TdxFormattedTextRun; override;
  public
    constructor Create(AAction: TdxFormattedTextRunAction;
      const AColor: TColor); virtual;

    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;

    property Color: TColor read FColor;
  end;

  { TdxFormattedTextURLRun }

  TdxFormattedTextURLRun = class(TdxFormattedTextRun) // for internal use
  protected
    FHyperlink: string;
    FColor: TColor;

    function Clone: TdxFormattedTextRun; override;
  public
    constructor Create(AAction: TdxFormattedTextRunAction;
      const AHyperlink: string); virtual;
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;

    property Hyperlink: string read FHyperlink;
  end;

  { TdxFormattedTextBackgroundColorRun }

  TdxFormattedTextBackgroundColorRun = class(TdxFormattedTextColorRun)
  // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextSizeRun }

  TdxFormattedTextSizeRun = class(TdxFormattedTextRun) // for internal use
  protected
    FSize: Integer;

    function Clone: TdxFormattedTextRun; override;
  public
    constructor Create(AAction: TdxFormattedTextRunAction;
      ASize: Integer); virtual;
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;

    property Size: Integer read FSize;
  end;

  { TdxFormattedTextSupRun }

  TdxFormattedTextSupRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextSubRun }

  TdxFormattedTextSubRun = class(TdxFormattedTextRun) // for internal use
  public
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;
  end;

  { TdxFormattedTextFontRun }

  TdxFormattedTextFontRun = class(TdxFormattedTextRun) // for internal use
  protected
    FFontName: string;

    function Clone: TdxFormattedTextRun; override;
  public
    constructor Create(AAction: TdxFormattedTextRunAction;
      const AFontName: string); virtual;
    procedure ApplyStyle(var AStyle
      : TdxFormattedTextCharacterProperties); override;

    property FontName: string read FFontName;
  end;

  { TdxFormattedTextImgRun }

  TdxFormattedTextImgRun = class(TdxFormattedTextRun); // for internal use

  { TdxFormattedTextRuns }

  TdxFormattedTextRuns = class(TObjectList<TdxFormattedTextRun>)
  // for internal use
  public
    function ExtractURLText(const ARun: TdxFormattedTextURLRun): string;
  end;

  { TdxFormattedTextRunStack }

  TdxFormattedTextRunStack = class // for internal use
  strict private
    FStack: TList<TdxFormattedTextRun>;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure CalculateStyle(var AStyle: TdxFormattedTextCharacterProperties);
    procedure Clear;
    procedure ProcessRun(ARun: TdxFormattedTextRun);
  end;

  { TdxFormattedTextLayoutBuilder }

  TdxFormattedTextLayoutBuilder = class // for internal use
  type
    TdxRollbackInfo = record
      TextPosition: PChar;
      RunIndex: Integer;
    end;
  strict private
  const
    dxEndEllipsisChars: PChar = '...';
    dxTabWidth: Integer = 48;
  strict private
    FFontCache: TdxFontCache;
    FRunStack: TdxFormattedTextRunStack;

    FAbilityBreakLineBoxRollbackInfo: TdxRollbackInfo;
    FPreviousRollbackInfo: TdxRollbackInfo;
    FLastRowRollbackInfo: TdxRollbackInfo;

    FHyperlinkIndex: Integer;

    FLayout: TdxFormattedTextLayout;
    FMeasurer: TdxCustomBoxMeasurer;
    FDCHandle: HDC; // todo remove

    FAbilityBreakLineBoxIndex: Integer;
    FCurrentCharacterProperties: TdxFormattedTextCharacterProperties;
    FMeasureTrailingSpaces: Boolean;
    FIsClippedText: Boolean;
    FIsEndEllipsisRow: Boolean;
    FRow: TdxFormattedTextLayoutRow;
    FRowTextWidth: Integer;
    FRunIndex: Integer;
    FRuns: TdxFormattedTextRuns;
    FRun: TdxFormattedTextRun;
    FTextStart: PChar;
    FTextFinish: PChar;
    FTextParams: TcxTextParams;

    procedure Align;

    function CreateCRLayoutBox: TdxFormattedCRLayoutBox;
    function CreateSpaceLayoutBox: TdxFormattedTextSpaceLayoutBox;
    function CreateTabLayoutBox: TdxFormattedTextTabLayoutBox;
    function CreateTextLayoutBox(const AText: string; AFontInfo: TdxFontInfo)
      : TdxFormattedTextTextLayoutBox;
    function CreateEmptyLayoutBox: TdxFormattedEmptyLayoutBox;

    function GetTextFromCache(const AText: string; AFontInfo: TdxFontInfo)
      : TdxTextViewInfo;

    function GetDefaultCharacterProperties: TdxFormattedTextCharacterProperties;
    function GetHyperlinkColor: TColor;
    function GetScaleFactor: TdxScaleFactor;
    function GetTextOutFormat: TcxTextOutFormat;
    function GetZoomFactor: Single;

    procedure AddNewRow;
    procedure DeleteLastRow;
    function GetAdjustedWidth(AWidth, ATabWidth: Integer): Integer;
    function GetAvailableRowWidth: Integer;
    function GetBounds: TRect;
    function GetFontInfo: TdxFontInfo;
    function GetInitialData: TdxFormattedTextLayoutBuilderInitialData;
    function GetTextSize: TSize;
    procedure GoToNextRow;
    procedure GoToNextRun;
    procedure InitializeDefaultParameters;
    procedure InitializeLayoutBox(ABox: TdxFormattedTextLayoutBox);
    procedure InitializePossibleBreakLineInfo;
    procedure InitializeRollbackInfo(ATextPosition: PChar; ARunIndex: Integer);
    function IsExcessRow: Boolean;
    procedure PopulateRunStack(ARunIndex: Integer);
    procedure ProcessSpecialCharacters;
    procedure ProcessText;
    procedure ReturnToRowBeginning;
    procedure SetRunIndex(AValue: Integer);
  protected
    property InitialData: TdxFormattedTextLayoutBuilderInitialData
      read GetInitialData;

    property Bounds: TRect read GetBounds;
    property DefaultCharacterProperties: TdxFormattedTextCharacterProperties
      read GetDefaultCharacterProperties;
    property TextOutFormat: TcxTextOutFormat read GetTextOutFormat;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ZoomFactor: Single read GetZoomFactor;
    property HyperlinkColor: TColor read GetHyperlinkColor;

    property AvailableRowWidth: Integer read GetAvailableRowWidth;
    property Measurer: TdxCustomBoxMeasurer read FMeasurer;
    property RunIndex: Integer read FRunIndex write SetRunIndex;
    property TextSize: TSize read GetTextSize;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Calculate(ACanvas: TCanvas; ALayout: TdxFormattedTextLayout;
      AFormattedText: TdxFormattedText;
      const AInitialData: TdxFormattedTextLayoutBuilderInitialData);
  end;

  { TdxFormattedTextLayout }

  TdxFormattedTextLayout = class(TObjectList<TdxFormattedTextLayoutRow>)
  // for internal use
  strict private
    FBounds: TRect;
    FHyperlinks: TList<TdxFormattedTextHyperlinkData>;
    FInitialData: TdxFormattedTextLayoutBuilderInitialData;
    FMeasureTrailingSpaces: Boolean;

    function GetTextSize: TSize;
    procedure SetInitialData(const AValue
      : TdxFormattedTextLayoutBuilderInitialData);
  protected
    function HitTest(const APoint: TPoint): TdxFormattedTextLayoutBox;

    property Bounds: TRect read FBounds write FBounds;
    property Hyperlinks: TList<TdxFormattedTextHyperlinkData> read FHyperlinks;
    property InitialData: TdxFormattedTextLayoutBuilderInitialData
      read FInitialData write SetInitialData;
    property TextSize: TSize read GetTextSize;
  public
    constructor Create(AMeasureTrailingSpaces: Boolean = False); virtual;
    destructor Destroy; override;

    procedure Draw(APainter: TdxFormattedTextCustomPainter;
      const AOffset: TPoint; AClip: Boolean);
  end;

  { TdxFormattedTextLayoutRow }

  TdxFormattedTextLayoutRow = class(TObjectList<TdxFormattedTextLayoutBox>)
  // for internal use
  strict private
    FAscentAndFree: Integer;
    FBounds: TRect;

    procedure AlignBox(AIndex: Integer; ASpaceFactor: Single;
      var ASpaceResidue: Single; var ABoxBounds: TRect);
    function GetAscentAndFree: Integer;
    function GetFirstBoxHorizontalPosition: Integer;
    function GetSpaceFactor: Single;
    function GetSpacesWidth(AStartIndex, AFinishIndex: Integer): Integer;
    procedure CalculateUnderline(AIndex: Integer;
      var AUnderlineBoxIndex, AUnderlineAverageThickness,
      AUnderlineAveragePosition, AUnderlineWidth: Integer);
    function SkipLeftSpaceBlocks: Integer;
    function SkipRightSpaceBlocks: Integer;
  private
    FExpandTabs: Boolean;
    FMeasureTrailingSpaces: Boolean;
    FTextAlignX: TcxTextAlignX;
    FTextAlignY: TcxTextAlignY;
  protected
    function GetTextSize(ABoxCount: Integer; AIsLast: Boolean): TSize;
    function HitTest(const APoint: TPoint): TdxFormattedTextLayoutBox;

    property AscentAndFree: Integer read FAscentAndFree;
    property Bounds: TRect read FBounds;
  public
    constructor Create(const ATextParams: TcxTextParams);

    procedure Calculate(ABounds: TRect);
    procedure Draw(APainter: TdxFormattedTextCustomPainter;
      const AOffset: TPoint);
  end;

  TdxLayoutBoxType = (lbtText, lbtSpace, lbtTab, lbtCR, lbtEmpty);
  // for internal use

  { TdxCustomFormattedTextLayoutBox }

  TdxFormattedTextLayoutBox = class // for internal use
  strict private
    FBackgroundColor: TColor;
    FBounds: TRect;
    FCharacterFormattingScript: TdxCharacterFormattingScript;
    FFontColor: TColor;
    FFontInfo: TdxFontInfo;
    FHyperlinkIndex: Integer;
    FStrikeout: Boolean;
    FUnderline: Boolean;
    FUnderlinePosition: Integer;
    FUnderlineThickness: Integer;
  protected
    function GetBoxType: TdxLayoutBoxType; virtual;
    function GetSize: TSize; virtual;

    property Bounds: TRect read FBounds;
    property BoxType: TdxLayoutBoxType read GetBoxType;
    property BackgroundColor: TColor read FBackgroundColor
      write FBackgroundColor;
    property CharacterFormattingScript: TdxCharacterFormattingScript
      read FCharacterFormattingScript write FCharacterFormattingScript;
    property FontColor: TColor read FFontColor write FFontColor;
    property FontInfo: TdxFontInfo read FFontInfo write FFontInfo;
    property Size: TSize read GetSize;
    property Strikeout: Boolean read FStrikeout write FStrikeout;
    property Underline: Boolean read FUnderline write FUnderline;
    property UnderlinePosition: Integer read FUnderlinePosition
      write FUnderlinePosition;
    property UnderlineThickness: Integer read FUnderlineThickness
      write FUnderlineThickness;
  public
    procedure Calculate(const ABounds: TRect); virtual;
    procedure Draw(APainter: TdxFormattedTextCustomPainter;
      const AOffset: TPoint); virtual;
    function IsHyperlink: Boolean;

    property HyperlinkIndex: Integer read FHyperlinkIndex write FHyperlinkIndex;
  end;

  { TdxFormattedEmptyLayoutBox }

  TdxFormattedEmptyLayoutBox = class(TdxFormattedTextLayoutBox)
  // for internal use
  protected
    function GetBoxType: TdxLayoutBoxType; override;
  end;

  { TdxFormattedCRLayoutBox }

  TdxFormattedCRLayoutBox = class(TdxFormattedTextLayoutBox) // for internal use
  protected
    function GetBoxType: TdxLayoutBoxType; override;
  end;

  { TdxFormattedTextSpaceLayoutBox }

  TdxFormattedTextSpaceLayoutBox = class(TdxFormattedTextLayoutBox)
  // for internal use
  strict private
    FCount: Integer;
  protected
    function GetSize: TSize; override;
    function GetBoxType: TdxLayoutBoxType; override;

    property Count: Integer read FCount write FCount;
  end;

  { TdxFormattedTextTabLayoutBox }

  TdxFormattedTextTabLayoutBox = class(TdxFormattedTextSpaceLayoutBox)
  // for internal use
  protected
    FTabWidth: Integer;

    function GetSize: TSize; override;
    function GetBoxType: TdxLayoutBoxType; override;
  end;

  { TdxFormattedTextLayoutBox }

  TdxFormattedTextTextLayoutBox = class(TdxFormattedTextLayoutBox)
  // for internal use
  strict private
    FText: string;
    FTextViewInfo: TdxTextViewInfo;
  protected
    function GetSize: TSize; override;

    property Text: string read FText;
    property TextViewInfo: TdxTextViewInfo read FTextViewInfo;
  public
    constructor Create(const AText: string; ATextViewInfo: TdxTextViewInfo);
    procedure Draw(APainter: TdxFormattedTextCustomPainter;
      const AOffset: TPoint); override;
  end;

  { TdxFormattedTextConverter }

  TdxFormattedTextConverter = class // for internal use
  protected
    class procedure AdjustInternalRuns(ASource: TdxFormattedText;
      ADefaultFont: TFont);
  public
    class function CanImport(const ASource: string): Boolean; virtual;
    class procedure Import(ATarget: TdxFormattedText; const ASource: string;
      ADefaultFont: TFont = nil); virtual;
    class function Export(ASource: TdxFormattedText; ADefaultFont: TFont = nil)
      : string; virtual;
  end;

  { TdxFormattedTextConverters }

  TdxFormattedTextConverters = class // for internal use
  strict private
    class var FList: TList;
  public
    class function GetConverterClass(const ASource: string)
      : TdxFormattedTextConverterClass;
    class procedure Import(ATarget: TdxFormattedText; const ASource: string;
      ADefaultFont: TFont = nil); overload;
    class procedure Import(ATarget: TdxFormattedText; const ASource: Variant;
      ADefaultFont: TFont = nil); overload;
    class function Export(ASource: TdxFormattedText;
      AConverterClass: TdxFormattedTextConverterClass;
      ADefaultFont: TFont = nil): string;
    //
    class procedure Register(AConverterClass: TdxFormattedTextConverterClass);
    class procedure Unregister(AConverterClass: TdxFormattedTextConverterClass);
  end;

function dxGetStringLength(AFirstChar, ALastChar: PChar): Integer;
// for internal use
function dxGetString(AFirstChar, ALastChar: PChar): string; overload; inline;
// for internal use
function dxGetString(AFirstChar: PChar; ALength: Integer): string; overload;
  inline; // for internal use

implementation

uses
  Math, RTLConsts, Variants,
  dxDocumentLayoutUnitConverter,
  dxCore, dxCoreClasses, dxCoreGraphics, dxTypeHelpers, dxStringHelper;

const
  dxDefaultHyperlinkColor: TColor = clBlue;

var
  FFormattedTextLayoutBuilder: TdxFormattedTextLayoutBuilder;

function FormattedTextLayoutBuilder: TdxFormattedTextLayoutBuilder;
begin
  if FFormattedTextLayoutBuilder = nil then
    FFormattedTextLayoutBuilder := TdxFormattedTextLayoutBuilder.Create;
  Result := FFormattedTextLayoutBuilder;
end;

function dxGetStringLength(AFirstChar, ALastChar: PChar): Integer;
begin
  Result := (NativeUInt(ALastChar) - NativeUInt(AFirstChar)) div SizeOf(Char);
end;

function dxGetString(AFirstChar: PChar; ALength: Integer): string;
begin
  if ALength > 0 then
    SetString(Result, AFirstChar, ALength)
  else
    Result := EmptyStr;
end;

function dxGetString(AFirstChar, ALastChar: PChar): string;
var
  ALength: Integer;
begin
  if ALastChar > AFirstChar then
  begin
    ALength := dxGetStringLength(AFirstChar, ALastChar);
    SetString(Result, AFirstChar, ALength)
  end
  else
    Result := '';
end;

type
  TdxFormattedTextBoxMeasurer = class(TdxCustomBoxMeasurer)
  public
    function CreateTextViewInfo(ADC: HDC; const AText: string;
      AFontInfo: TdxFontInfo): TdxTextViewInfo; override;
  end;

  { TdxFormattedTextSpaceLayoutBox }

function TdxFormattedTextBoxMeasurer.CreateTextViewInfo(ADC: HDC;
  const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo;
begin
  SelectObject(ADC, AFontInfo.GdiFontHandle);
  Result := CreateTextViewInfoCore(ADC, AText, AFontInfo);
end;

{ TdxFormattedTextCharacterProperties }

function TdxFormattedTextCharacterProperties.Compare(ACharacterProperties
  : TdxFormattedTextCharacterProperties): Boolean;
begin
  Result := (FontName = ACharacterProperties.FontName) and
    (FontDoubleSize = ACharacterProperties.FontDoubleSize) and
    (FontStyle = ACharacterProperties.FontStyle) and
    (FontColor = ACharacterProperties.FontColor) and
    (BackgroundColor = ACharacterProperties.BackgroundColor) and
    (CharacterFormattingScript = ACharacterProperties.
    CharacterFormattingScript);
end;

procedure TdxFormattedTextCharacterProperties.Initialize(AFont: TFont);
begin
  FontName := AFont.Name;
  FontSize := AFont.Size;
  FontStyle := AFont.Style;
  FontColor := ColorToRGB(AFont.Color);
  BackgroundColor := GetDefaultBackgroundColor;
  CharacterFormattingScript := TdxCharacterFormattingScript.Normal;
end;

procedure TdxFormattedTextCharacterProperties.Initialize(AFont: TFont;
  AFontDoubleSize: Integer);
begin
  Initialize(AFont);
  FontDoubleSize := AFontDoubleSize;
end;

function TdxFormattedTextCharacterProperties.GetDefaultBackgroundColor: TColor;
begin
  Result := clDefault;
end;

function TdxFormattedTextCharacterProperties.GetBold: Boolean;
begin
  Result := fsBold in FontStyle;
end;

function TdxFormattedTextCharacterProperties.GetFontSize: Integer;
begin
  Result := FontDoubleSize div 2;
end;

function TdxFormattedTextCharacterProperties.GetItalic: Boolean;
begin
  Result := fsItalic in FontStyle;
end;

function TdxFormattedTextCharacterProperties.GetUnderline: Boolean;
begin
  Result := fsUnderline in FontStyle;
end;

function TdxFormattedTextCharacterProperties.GetStrikeout: Boolean;
begin
  Result := fsStrikeOut in FontStyle;
end;

procedure TdxFormattedTextCharacterProperties.SetBold(AValue: Boolean);
begin
  if AValue then
    FontStyle := FontStyle + [fsBold]
  else
    FontStyle := FontStyle - [fsBold];
end;

procedure TdxFormattedTextCharacterProperties.SetFontSize(AValue: Integer);
begin
  FontDoubleSize := AValue * 2;
end;

procedure TdxFormattedTextCharacterProperties.SetItalic(AValue: Boolean);
begin
  if AValue then
    FontStyle := FontStyle + [fsItalic]
  else
    FontStyle := FontStyle - [fsItalic];
end;

procedure TdxFormattedTextCharacterProperties.SetUnderline(AValue: Boolean);
begin
  if AValue then
    FontStyle := FontStyle + [fsUnderline]
  else
    FontStyle := FontStyle - [fsUnderline];
end;

procedure TdxFormattedTextCharacterProperties.SetStrikeout(AValue: Boolean);
begin
  if AValue then
    FontStyle := FontStyle + [fsStrikeOut]
  else
    FontStyle := FontStyle - [fsStrikeOut];
end;

{ TdxFormattedText }

constructor TdxFormattedText.Create;
begin
  inherited Create;
  FHyperlinkColor := clDefault;
  FRuns := TdxFormattedTextRuns.Create;
  FInternalRuns := TdxFormattedTextRuns.Create;
  FLayout := TdxFormattedTextLayout.Create;
  FMeasureTrailingSpaces := False;
  FUseOfficeFonts := False;
end;

destructor TdxFormattedText.Destroy;
begin
  FreeAndNil(FLayout);
  FreeAndNil(FInternalRuns);
  FreeAndNil(FRuns);
  inherited;
end;

procedure TdxFormattedText.Import(const ASource: string;
  ADefaultFont: TFont = nil);
begin
  TdxFormattedTextConverters.Import(Self, ASource, ADefaultFont);
end;

procedure TdxFormattedText.Import(const ASource: Variant;
  ADefaultFont: TFont = nil);
begin
  TdxFormattedTextConverters.Import(Self, ASource, ADefaultFont);
end;

function TdxFormattedText.Export(AConverterClass
  : TdxFormattedTextConverterClass; ADefaultFont: TFont = nil): string;
begin
  Result := TdxFormattedTextConverters.Export(Self, AConverterClass,
    ADefaultFont);
end;

procedure TdxFormattedText.CalculateLayout(ACanvas: TCanvas; AFont: TFont;
  const ABounds: TRect; AFormat: TcxTextOutFormat;
  AScaleFactor: TdxScaleFactor = nil; AZoomFactor: Single = 1;
  const AHyperlinkColor: TColor = clDefault);
var
  AInitialData: TdxFormattedTextLayoutBuilderInitialData;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;
  AInitialData.Initialize(AFont, ABounds, AFormat, AScaleFactor, AZoomFactor,
    AHyperlinkColor);
  if FIsTextChanged or not Layout.InitialData.Compare(AInitialData) then
    FormattedTextLayoutBuilder.Calculate(ACanvas, FLayout, Self, AInitialData);
  FIsTextChanged := False;
end;

procedure TdxFormattedText.Draw(ACanvas: TCanvas; const APosition: TPoint;
  AClip: Boolean = True);
var
  APainter: TdxFormattedTextPainter;
begin
  APainter := TdxFormattedTextPainter.Create(ACanvas.Handle);
  try
    InternalDraw(APainter, APosition, AClip);
  finally
    APainter.Free;
  end;
end;

procedure TdxFormattedText.Draw(APainter: TdxFormattedTextCustomPainter;
  const APosition: TPoint; AClip: Boolean = True);
begin
  InternalDraw(APainter, APosition, AClip);
end;

function TdxFormattedText.GetBaseLine(ARowIndex: Integer): Integer;
begin
  if ARowIndex < Layout.Count then
    Result := Layout[ARowIndex].AscentAndFree
  else
    Result := 0;
end;

function TdxFormattedText.GetDisplayText: string;
var
  ABuffer: TStringBuilder;
  ARun: TdxFormattedTextRun;
  I: Integer;
begin
  ABuffer := TdxStringBuilderManager.Get(Length(FText));
  try
    for I := 0 to Runs.Count - 1 do
    begin
      ARun := Runs[I];
      ABuffer.Append(dxGetString(ARun.TextStart, ARun.TextLength));
    end;
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

function TdxFormattedText.GetUrl(AIndex: Integer): string;
begin
  Result := Layout.Hyperlinks[AIndex].Hyperlink;
end;

function TdxFormattedText.HitTest(const APoint: TPoint)
  : TdxFormattedTextLayoutBox;
begin
  Result := FLayout.HitTest(APoint);
end;

procedure TdxFormattedText.CreateInternalRuns(ADefaultFont: TFont);

  function GetFirstSplitChar(AFontCharacterSet: TdxFontCharacterSet;
    ATextStart, ATextFinish: PChar): PChar;
  var
    ACursor: PChar;
  begin
    Result := ATextFinish;
    ACursor := ATextStart;
    while ACursor < ATextFinish do
    begin
      if not AFontCharacterSet.ContainsChar(ACursor^) then
      begin
        Result := ACursor;
        Break;
      end;
      ACursor := ACursor + 1;
    end;
  end;

  function GetLastSplitChar(ADefaultFontCharacterSet, AFontCharacterSet
    : TdxFontCharacterSet; ATextStart, ATextFinish: PChar): PChar;
  var
    ACursor: PChar;
  begin
    Result := ATextFinish;
    ACursor := ATextStart;
    while ACursor < ATextFinish do
    begin
      if not AFontCharacterSet.ContainsChar(ACursor^) or
        ADefaultFontCharacterSet.ContainsChar(ACursor^) then
      begin
        Result := ACursor;
        Break;
      end;
      ACursor := ACursor + 1;
    end;
  end;

  function GetDefaultFontName: string;
  begin
    if ADefaultFont <> nil then
      Result := ADefaultFont.Name
    else
      Result := dxShortStringToString(DefFontData.Name);
  end;

var
  I: Integer;
  ARun, ANewRun: TdxFormattedTextRun;
  AFontCache: TdxFontCache;
  ASourceFontCharacterSet, ADefaultFontCharacterSet: TdxFontCharacterSet;
  ASplitTextPosition: PChar;
  AFontNameStack: TList<string>;
begin
  FInternalRuns.Clear;
  AFontCache := GetActualFontCache(96);
  AFontNameStack := TList<string>.Create;
  try
    AFontNameStack.Add(GetDefaultFontName);
    for I := 0 to Runs.Count - 1 do
    begin
      ARun := Runs[I];
      if ARun is TdxFormattedTextFontRun then
      begin
        if ARun.Action = traOpen then
          AFontNameStack.Add(TdxFormattedTextFontRun(ARun).FFontName)
        else
          AFontNameStack.Delete(AFontNameStack.Count - 1);
      end;
      ANewRun := ARun.Clone;
      FInternalRuns.Add(ANewRun);
      ADefaultFontCharacterSet := AFontCache.GetFontCharacterSet
        (AFontNameStack.Last);
      ASplitTextPosition := GetFirstSplitChar(ADefaultFontCharacterSet,
        ARun.FTextStart, ARun.TextFinish);
      while ASplitTextPosition < ARun.TextFinish do
      begin
        ANewRun.FTextLength := ASplitTextPosition - ANewRun.TextStart;
        ASourceFontCharacterSet := ADefaultFontCharacterSet;
        AFontNameStack.Add(AFontCache.FindSubstituteFont(AFontNameStack.Last,
          ASplitTextPosition^, ASourceFontCharacterSet));
        ANewRun := TdxFormattedTextFontRun.Create(traOpen, AFontNameStack.Last);
        ANewRun.FTextStart := ASplitTextPosition;
        FInternalRuns.Add(ANewRun);
        ASourceFontCharacterSet := AFontCache.GetFontCharacterSet
          (AFontNameStack.Last);
        ASplitTextPosition := GetLastSplitChar(ADefaultFontCharacterSet,
          ASourceFontCharacterSet, ASplitTextPosition + 1, ARun.TextFinish);
        ANewRun.FTextLength := ASplitTextPosition - ANewRun.FTextStart;
        ANewRun := TdxFormattedTextFontRun.Create(traClose, '');
        FInternalRuns.Add(ANewRun);
        AFontNameStack.Delete(AFontNameStack.Count - 1);
        ANewRun.FTextStart := ASplitTextPosition;
        ASplitTextPosition := GetFirstSplitChar(ADefaultFontCharacterSet,
          ASplitTextPosition, ARun.TextFinish);
      end;
      ANewRun.FTextLength := ASplitTextPosition - ANewRun.FTextStart;
    end;
  finally
    AFontNameStack.Free;
  end;
end;

function TdxFormattedText.GetActualFontCache(const ADpi: Single): TdxFontCache;
begin
  if FUseOfficeFonts then
    Result := TdxFontCacheManager.GetOfficeFontCache
      (TdxDocumentLayoutUnit.Pixel, ADpi)
  else
    Result := TdxFontCacheManager.GetVCLFontCache
      (TdxDocumentLayoutUnit.Pixel, ADpi);
end;

function TdxFormattedText.GetHasFormatting: Boolean;
var
  I: Integer;
  AAlreadyAccelChar: Boolean;
begin
  Result := False;
  AAlreadyAccelChar := False;
  for I := 1 to Runs.Count - 1 do
    if FHasAccelCharRun and not AAlreadyAccelChar and
      (Runs[I] is TdxFormattedTextUnderlineRun) then
      AAlreadyAccelChar := Runs[I].Action = traClose
    else if not(Runs[I] is TdxFormattedTextNoCodeRun) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxFormattedText.GetTextSize: TSize;
begin
  Result := Layout.TextSize;
end;

procedure TdxFormattedText.InternalDraw(APainter: TdxFormattedTextCustomPainter;
  const APosition: TPoint; AClip: Boolean);
begin
  FLayout.Draw(APainter, APosition, AClip);
end;

procedure TdxFormattedText.SetText(const AValue: string);
begin
  FText := AValue;
  FIsTextChanged := True;
end;

{ TdxFormattedTextRun }

constructor TdxFormattedTextRun.Create(AAction: TdxFormattedTextRunAction);
begin
  inherited Create;
  FAction := AAction;
end;

procedure TdxFormattedTextRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  // do nothing
end;

function TdxFormattedTextRun.Clone: TdxFormattedTextRun;
begin
  Result := TdxFormattedTextRunClass(ClassType).Create(Action);
  Result.FTextStart := FTextStart;
  Result.FTextLength := FTextLength;
end;

function TdxFormattedTextRun.GetText: string;
begin
  SetString(Result, TextStart, TextLength);
end;

function TdxFormattedTextRun.GetTextFinish: PChar;
begin
  Result := TextStart + TextLength;
end;

{ TdxFormattedTextBoldRun }

procedure TdxFormattedTextBoldRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  AStyle.Bold := True;
end;

{ TdxFormattedTextItalicRun }

procedure TdxFormattedTextItalicRun.ApplyStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
begin
  AStyle.Italic := True;
end;

{ TdxFormattedTextUnderlineRun }

procedure TdxFormattedTextUnderlineRun.ApplyStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
begin
  AStyle.Underline := True;
end;

{ TdxFormattedTextStrikeoutRun }

procedure TdxFormattedTextStrikeoutRun.ApplyStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
begin
  AStyle.Strikeout := True;
end;

{ TdxFormattedTextColorRun }

constructor TdxFormattedTextColorRun.Create(AAction: TdxFormattedTextRunAction;
  const AColor: TColor);
begin
  inherited Create(AAction);
  FColor := AColor;
end;

procedure TdxFormattedTextColorRun.ApplyStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
begin
  AStyle.FontColor := ColorToRGB(FColor);
end;

function TdxFormattedTextColorRun.Clone: TdxFormattedTextRun;
begin
  Result := inherited Clone;
  TdxFormattedTextColorRun(Result).FColor := FColor;
end;

{ TdxFormattedTextURLRun }

constructor TdxFormattedTextURLRun.Create(AAction: TdxFormattedTextRunAction;
  const AHyperlink: string);
begin
  inherited Create(AAction);
  FHyperlink := AHyperlink;
  FColor := clDefault;
end;

procedure TdxFormattedTextURLRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  if FColor = clDefault then
    AStyle.FontColor := dxDefaultHyperlinkColor
  else
    AStyle.FontColor := FColor;

  AStyle.Underline := True;
end;

function TdxFormattedTextURLRun.Clone: TdxFormattedTextRun;
begin
  Result := inherited Clone;
  TdxFormattedTextURLRun(Result).FColor := FColor;
  TdxFormattedTextURLRun(Result).FHyperlink := FHyperlink;
end;

{ TdxFormattedTextBackgroundColorRun }

procedure TdxFormattedTextBackgroundColorRun.ApplyStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
begin
  AStyle.BackgroundColor := FColor;
end;

{ TdxFormattedTextSizeRun }

constructor TdxFormattedTextSizeRun.Create(AAction: TdxFormattedTextRunAction;
  ASize: Integer);
begin
  inherited Create(AAction);
  FSize := ASize;
end;

procedure TdxFormattedTextSizeRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  AStyle.FontSize := FSize;
end;

function TdxFormattedTextSizeRun.Clone: TdxFormattedTextRun;
begin
  Result := inherited Clone;
  TdxFormattedTextSizeRun(Result).FSize := FSize;
end;

{ TdxFormattedTextSupRun }

procedure TdxFormattedTextSupRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  AStyle.CharacterFormattingScript := TdxCharacterFormattingScript.Superscript
end;

{ TdxFormattedTextSubRun }

procedure TdxFormattedTextSubRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  AStyle.CharacterFormattingScript := TdxCharacterFormattingScript.Subscript
end;

{ TdxFormattedTextFontRun }

constructor TdxFormattedTextFontRun.Create(AAction: TdxFormattedTextRunAction;
  const AFontName: string);
begin
  inherited Create(AAction);
  FFontName := AFontName;
end;

procedure TdxFormattedTextFontRun.ApplyStyle(var AStyle
  : TdxFormattedTextCharacterProperties);
begin
  AStyle.FontName := FFontName;
end;

function TdxFormattedTextFontRun.Clone: TdxFormattedTextRun;
begin
  Result := inherited Clone;
  TdxFormattedTextFontRun(Result).FFontName := FFontName;
end;

{ TdxFormattedTextRuns }

function TdxFormattedTextRuns.ExtractURLText(const ARun
  : TdxFormattedTextURLRun): string;
var
  ABuffer: TStringBuilder;
  AIndex: Integer;
  AItem: TdxFormattedTextRun;
  I: Integer;
begin
  AIndex := IndexOf(ARun);
  if (AIndex < 0) or (ARun.Action <> traOpen) then
    Exit('');

  ABuffer := TdxStringBuilderManager.Get;
  try
    for I := AIndex to Count - 1 do
    begin
      AItem := Items[I];
      if (AItem.Action = traClose) and AItem.InheritsFrom(TdxFormattedTextURLRun)
      then
        Break;
      ABuffer.Append(AItem.Text);
    end;
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer)
  end;
end;

{ TdxFormattedTextRunStack }

constructor TdxFormattedTextRunStack.Create;
begin
  inherited;
  FStack := TList<TdxFormattedTextRun>.Create;
end;

destructor TdxFormattedTextRunStack.Destroy;
begin
  FreeAndNil(FStack);
  inherited;
end;

procedure TdxFormattedTextRunStack.CalculateStyle
  (var AStyle: TdxFormattedTextCharacterProperties);
var
  I: Integer;
begin
  for I := 0 to FStack.Count - 1 do
    FStack[I].ApplyStyle(AStyle);
end;

procedure TdxFormattedTextRunStack.Clear;
begin
  FStack.Clear;
end;

procedure TdxFormattedTextRunStack.ProcessRun(ARun: TdxFormattedTextRun);
var
  I: Integer;
begin
  if ARun.Action = traOpen then
    FStack.Add(ARun)
  else
  begin
    for I := FStack.Count - 1 downto 0 do
      if FStack[I].ClassType = ARun.ClassType then
      begin
        FStack.Extract(FStack[I]);
        Break;
      end;
  end;
end;

{ TdxFormattedTextLayoutBuilder }

constructor TdxFormattedTextLayoutBuilder.Create;
begin
  inherited Create;
  FMeasurer := TdxFormattedTextBoxMeasurer.Create;
  FRunStack := TdxFormattedTextRunStack.Create;
end;

destructor TdxFormattedTextLayoutBuilder.Destroy;
begin
  FreeAndNil(FRunStack);
  FreeAndNil(FMeasurer);
  inherited;
end;

procedure TdxFormattedTextLayoutBuilder.Calculate(ACanvas: TCanvas;
  ALayout: TdxFormattedTextLayout; AFormattedText: TdxFormattedText;
  const AInitialData: TdxFormattedTextLayoutBuilderInitialData);
var
  ALastRowRecalculating: Boolean;
begin
  if AFormattedText.Runs.Count = 0 then
    Exit;
  FLayout := ALayout;
  FLayout.InitialData := AInitialData;
  FFontCache := AFormattedText.GetActualFontCache(ScaleFactor.TargetDPI);
  FDCHandle := ACanvas.Handle;
  FRuns := AFormattedText.InternalRuns;
  FMeasureTrailingSpaces := AFormattedText.MeasureTrailingSpaces;
  FTextParams := cxCalcTextParams(FDCHandle, TextOutFormat);
  FTextParams.CharBreak := TextOutFormat and CXTO_CHARBREAK <> 0;
  InitializeDefaultParameters;
  ALastRowRecalculating := False;
  while RunIndex < FRuns.Count do
  begin
    while (FTextStart < FTextFinish) and not FIsClippedText do
    begin
      ProcessSpecialCharacters;
      if (FTextStart < FTextFinish) and not FIsClippedText then
        ProcessText;
    end;
    if not FIsClippedText and (FRunIndex = FRuns.Count - 1) and IsExcessRow then
    begin
      FIsClippedText := True;
      DeleteLastRow;
    end;
    if FTextParams.EndEllipsis and
      (FIsClippedText or (FRunIndex = FRuns.Count - 1) and
      (FRowTextWidth > AvailableRowWidth)) then
      FIsEndEllipsisRow := not FIsEndEllipsisRow;
    if FIsClippedText and not FIsEndEllipsisRow then
      Break;
    GoToNextRun;
    if FIsEndEllipsisRow and not ALastRowRecalculating and
      (FLastRowRollbackInfo.TextPosition <> nil) then
    begin
      ALastRowRecalculating := True;
      FIsClippedText := False;
      ReturnToRowBeginning;
    end;
  end;
  if (FLayout.Count > 0) and (FRow.Count = 0) then
    FRow.Add(CreateEmptyLayoutBox);
  Align;
end;

procedure TdxFormattedTextLayoutBuilder.Align;

  function GetFirstRowPositionY: Integer;
  begin
    case FTextParams.TextAlignY of
      taBottom:
        Result := Bounds.Height - TextSize.cy;
      taCenterY:
        Result := (Bounds.Height - TextSize.cy) div 2;
    else
      Result := 0;
    end;
  end;

var
  ANextLinePosition: Integer;
  ALayoutRow: TdxFormattedTextLayoutRow;
  ASpaceLineFactor: Single;
  ASpaceLineResidue: Single;
  I: Integer;
begin
  ANextLinePosition := GetFirstRowPositionY;
  ASpaceLineResidue := 0;
  if (FTextParams.TextAlignY = taDistributeY) and (FLayout.Count > 1) then
    ASpaceLineFactor :=
      Max(1, (Bounds.Height - FLayout.Last.GetTextSize(FLayout.Last.Count,
      False).cy) / (TextSize.cy - FLayout.Last.GetTextSize(FLayout.Last.Count,
      False).cy))
  else
    ASpaceLineFactor := 1;
  for I := 0 to FLayout.Count - 1 do
  begin
    ALayoutRow := FLayout[I];
    if (I = FLayout.Count - 1) and (ALayoutRow.FTextAlignX = taJustifyX) then
      ALayoutRow.FTextAlignX := taLeft;

    ALayoutRow.Calculate(cxRectBounds(0, ANextLinePosition, cxRectWidth(Bounds),
      ALayoutRow.GetTextSize(ALayoutRow.Count, False).cy));

    ASpaceLineResidue := ASpaceLineResidue +
      (ALayoutRow.Bounds.Bottom - ANextLinePosition) * ASpaceLineFactor -
      Trunc((ALayoutRow.Bounds.Bottom - ANextLinePosition) * ASpaceLineFactor);

    if ASpaceLineResidue > 1 then
    begin
      ANextLinePosition := ANextLinePosition + 1;
      ASpaceLineResidue := ASpaceLineResidue - 1;
    end;

    if (FTextParams.TextAlignY = taDistributeY) and (I = FLayout.Count - 2) and
      (ASpaceLineFactor > 1) then
      ANextLinePosition := Bounds.Height - FLayout.Last.GetTextSize
        (FLayout.Last.Count, False).cy
    else
      ANextLinePosition := ANextLinePosition +
        Trunc((ALayoutRow.Bounds.Bottom - ANextLinePosition) *
        ASpaceLineFactor);
  end;
end;

function TdxFormattedTextLayoutBuilder.CreateCRLayoutBox
  : TdxFormattedCRLayoutBox;
begin
  Result := TdxFormattedCRLayoutBox.Create;
  InitializeLayoutBox(Result);
  if FTextStart^ = dxCR then
  begin
    Inc(FTextStart);
    if (FTextStart < FTextFinish) and (FTextStart^ = dxLF) then
      Inc(FTextStart);
  end
  else if FTextStart^ = dxLF then
  begin
    Inc(FTextStart);
    if (FTextStart < FTextFinish) and (FTextStart^ = dxCR) then
      Inc(FTextStart);
  end;
end;

function TdxFormattedTextLayoutBuilder.CreateSpaceLayoutBox
  : TdxFormattedTextSpaceLayoutBox;
begin
  Result := TdxFormattedTextSpaceLayoutBox.Create;
  InitializeLayoutBox(Result);
  while (FTextStart^ = dxSpace) and
    not(FIsEndEllipsisRow and (FRowTextWidth + Result.Size.cx >
    AvailableRowWidth)) do
  begin
    Result.Count := Result.Count + 1;
    Inc(FTextStart);
  end;
  FRowTextWidth := FRowTextWidth + Result.Size.cx;
end;

function TdxFormattedTextLayoutBuilder.CreateTabLayoutBox
  : TdxFormattedTextTabLayoutBox;

  function NeedTabBreak(ATabSize, ACount: Integer): Boolean;
  begin
    Result := ((FRow.Count > 0) or (ACount > 1)) and FTextParams.WordBreak and
      not FTextParams.SingleLine and FTextParams.ExpandTabs and
      (GetAdjustedWidth(FRowTextWidth + ATabSize * ACount, ATabSize) >
      AvailableRowWidth);
  end;

var
  ATabWidth: Integer;
begin
  if FTextParams.ExpandTabs then
    ATabWidth := ScaleFactor.Apply(dxTabWidth)
  else
    ATabWidth := 0;

  Result := TdxFormattedTextTabLayoutBox.Create;
  Result.FTabWidth := ATabWidth;
  InitializeLayoutBox(Result);
  while (FTextStart^ = dxTab) and not NeedTabBreak(ATabWidth, Result.Count) do
  begin
    Result.Count := Result.Count + 1;
    Inc(FTextStart);
  end;
  FRowTextWidth := GetAdjustedWidth(FRowTextWidth + Result.Size.cx, ATabWidth);
end;

function TdxFormattedTextLayoutBuilder.CreateTextLayoutBox(const AText: string;
  AFontInfo: TdxFontInfo): TdxFormattedTextTextLayoutBox;
begin
  Result := TdxFormattedTextTextLayoutBox.Create(AText,
    GetTextFromCache(AText, AFontInfo));
  InitializeLayoutBox(Result);
end;

function TdxFormattedTextLayoutBuilder.CreateEmptyLayoutBox
  : TdxFormattedEmptyLayoutBox;
begin
  Result := TdxFormattedEmptyLayoutBox.Create;
  InitializeLayoutBox(Result);
end;

function TdxFormattedTextLayoutBuilder.GetTextFromCache(const AText: string;
  AFontInfo: TdxFontInfo): TdxTextViewInfo;
begin
  Result := Measurer.TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo);
end;

function TdxFormattedTextLayoutBuilder.GetDefaultCharacterProperties
  : TdxFormattedTextCharacterProperties;
begin
  Result := FLayout.InitialData.CharacterProperties;
end;

function TdxFormattedTextLayoutBuilder.GetHyperlinkColor: TColor;
begin
  Result := FLayout.InitialData.HyperlinkColor;
end;

function TdxFormattedTextLayoutBuilder.GetScaleFactor: TdxScaleFactor;
begin
  Result := FLayout.InitialData.ScaleFactor;
end;

function TdxFormattedTextLayoutBuilder.GetTextOutFormat: TcxTextOutFormat;
begin
  Result := FLayout.InitialData.TextOutFormat;
end;

function TdxFormattedTextLayoutBuilder.GetZoomFactor: Single;
begin
  Result := FLayout.InitialData.ZoomFactor;
end;

function TdxFormattedTextLayoutBuilder.GetAdjustedWidth(AWidth,
  ATabWidth: Integer): Integer;
begin
  if ATabWidth > 0 then
    Result := (AWidth div ATabWidth) * ATabWidth
  else
    Result := AWidth;
end;

procedure TdxFormattedTextLayoutBuilder.AddNewRow;
begin
  FRowTextWidth := 0;
  FRow := TdxFormattedTextLayoutRow.Create(FTextParams);
  FRow.FMeasureTrailingSpaces := FMeasureTrailingSpaces;
  FLayout.Add(FRow);
end;

procedure TdxFormattedTextLayoutBuilder.DeleteLastRow;
begin
  FLastRowRollbackInfo := FPreviousRollbackInfo;
  FLayout.Delete(FLayout.Count - 1);
  FRow := FLayout.Last;
end;

function TdxFormattedTextLayoutBuilder.GetAvailableRowWidth: Integer;
begin
  Result := Bounds.Width;
  if FIsEndEllipsisRow then
    Result := Result - Measurer.MeasureText(FDCHandle, dxEndEllipsisChars,
      GetFontInfo).cx;
  if FTextParams.NoClip then
    Result := Max(Result, TextSize.cx);
end;

function TdxFormattedTextLayoutBuilder.GetBounds: TRect;
begin
  Result := FLayout.Bounds;
end;

function TdxFormattedTextLayoutBuilder.GetFontInfo: TdxFontInfo;
var
  AFontInfoIndex: Integer;
begin
  AFontInfoIndex := FFontCache.CalcFontIndex
    (FCurrentCharacterProperties.FontName,
    Round(FCurrentCharacterProperties.FontDoubleSize * ZoomFactor),
    FCurrentCharacterProperties.FontStyle - [fsStrikeOut, fsUnderline],
    FCurrentCharacterProperties.CharacterFormattingScript);
  Result := FFontCache.Items[AFontInfoIndex];
end;

function TdxFormattedTextLayoutBuilder.GetInitialData
  : TdxFormattedTextLayoutBuilderInitialData;
begin
  Result := FLayout.InitialData;
end;

function TdxFormattedTextLayoutBuilder.GetTextSize: TSize;
begin
  Result := FLayout.TextSize;
end;

procedure TdxFormattedTextLayoutBuilder.GoToNextRow;
begin
  if FIsEndEllipsisRow then
  begin
    FIsClippedText := True;
    Exit;
  end;
  FIsClippedText := IsExcessRow;
  if FIsClippedText then
    DeleteLastRow;
  FIsClippedText := (FIsClippedText or ((GetTextSize.cy >= Bounds.Height) and
    not FTextParams.NoClip) or ((FLayout.Count > 0) and FTextParams.SingleLine))
    and not(FTextParams.TextAlignY in [taCenterY, taBottom]);
  if not FIsClippedText and (FTextStart <> nil) then
    AddNewRow;
end;

procedure TdxFormattedTextLayoutBuilder.GoToNextRun;
begin
  RunIndex := RunIndex + 1;
  if FRunIndex < FRuns.Count then
  begin
    FTextStart := FRun.TextStart;
    FTextFinish := FRun.TextStart + FRun.TextLength;
  end;
end;

procedure TdxFormattedTextLayoutBuilder.InitializeDefaultParameters;
begin
  FLayout.Clear;
  FLayout.Hyperlinks.Clear;
  FHyperlinkIndex := -1;
  FAbilityBreakLineBoxIndex := -1;
  FIsClippedText := False;
  FIsEndEllipsisRow := False;
  FRowTextWidth := 0;
  FRunIndex := -1;
  FRunStack.Clear;
  GoToNextRun;
  FCurrentCharacterProperties := DefaultCharacterProperties;
  GoToNextRow;
end;

procedure TdxFormattedTextLayoutBuilder.InitializeLayoutBox
  (ABox: TdxFormattedTextLayoutBox);
var
  AHyperlink: TdxFormattedTextHyperlinkData;
begin
  if FHyperlinkIndex > -1 then
  begin
    AHyperlink := FLayout.Hyperlinks[FHyperlinkIndex];
    if AHyperlink.BoxCount = 0 then
    begin
      AHyperlink.BeginBoxIndex := FRow.Count;
      AHyperlink.BeginRowIndex := FLayout.Count - 1;
    end;
    AHyperlink.BoxCount := FLayout.Hyperlinks[FHyperlinkIndex].BoxCount + 1;
  end;
  ABox.HyperlinkIndex := FHyperlinkIndex;
  ABox.FontInfo := GetFontInfo;
  ABox.CharacterFormattingScript :=
    FCurrentCharacterProperties.CharacterFormattingScript;
  ABox.Underline := FCurrentCharacterProperties.Underline;
  ABox.Strikeout := FCurrentCharacterProperties.Strikeout;
  ABox.BackgroundColor := FCurrentCharacterProperties.BackgroundColor;
  ABox.FontColor := FCurrentCharacterProperties.FontColor;
end;

procedure TdxFormattedTextLayoutBuilder.InitializePossibleBreakLineInfo;
begin
  FAbilityBreakLineBoxIndex := FRow.Count - 1;
  FAbilityBreakLineBoxRollbackInfo.TextPosition := FTextStart;
  FAbilityBreakLineBoxRollbackInfo.RunIndex := RunIndex;
end;

procedure TdxFormattedTextLayoutBuilder.InitializeRollbackInfo
  (ATextPosition: PChar; ARunIndex: Integer);
begin
  if ATextPosition <> FLastRowRollbackInfo.TextPosition then
  begin
    FPreviousRollbackInfo := FLastRowRollbackInfo;
    FLastRowRollbackInfo.TextPosition := ATextPosition;
    FLastRowRollbackInfo.RunIndex := ARunIndex;
  end;
end;

function TdxFormattedTextLayoutBuilder.IsExcessRow: Boolean;
begin
  Result := (FLayout.Count > 1) and not FTextParams.NoClip and
    not(FTextParams.TextAlignX in [taJustifyX, taDistributeX]) and
    (FTextParams.TextAlignY = taTop) and not FTextParams.CalcRect and
    not FTextParams.CalcRowCount and
    (FTextParams.EditControl and (GetTextSize.cy > Bounds.Height));
end;

procedure TdxFormattedTextLayoutBuilder.PopulateRunStack(ARunIndex: Integer);
var
  I: Integer;
begin
  FRunStack.Clear;
  for I := 0 to ARunIndex do
    FRunStack.ProcessRun(FRuns[I]);
end;

procedure TdxFormattedTextLayoutBuilder.ProcessSpecialCharacters;
var
  ATextBox: TdxFormattedTextLayoutBox;
begin
  while not FIsClippedText and CharInSet(FTextStart^, dxSpaceCharacters) do
  begin
    if FRow.Count = 0 then
      InitializeRollbackInfo(FTextStart, RunIndex);
    ATextBox := nil;
    if CharInSet(FTextStart^, [dxCR, dxLF]) then
    begin
      if FIsEndEllipsisRow then
      begin
        Measurer.MeasureText(FDCHandle, dxEndEllipsisChars, GetFontInfo);
        FRow.Add(CreateTextLayoutBox(dxEndEllipsisChars, GetFontInfo));
        FIsClippedText := True;
        Break;
      end
      else
      begin
        if FRow.Count = 0 then
          FRowTextWidth := cxSize(0, GetFontInfo.LineSpacing).cx
        else
          FRowTextWidth := FRow.GetTextSize(FRow.Count, False).cx;

        FRow.Add(CreateCRLayoutBox);
        if FRow.FTextAlignX = taJustifyX then
          FRow.FTextAlignX := taLeft;
        GoToNextRow;
        if FIsClippedText then
          Break;
        FAbilityBreakLineBoxIndex := -1;
      end;
    end
    else
    begin
      if FTextStart^ = dxSpace then
        ATextBox := CreateSpaceLayoutBox
      else if FTextStart^ = dxTab then
        ATextBox := CreateTabLayoutBox;

      if FMeasureTrailingSpaces and (FRowTextWidth > AvailableRowWidth) and
        FTextParams.WordBreak and not FTextParams.SingleLine then
      begin
        FRowTextWidth := FRowTextWidth - ATextBox.Size.cx;
        TdxFormattedTextSpaceLayoutBox(ATextBox).Count :=
          TdxFormattedTextSpaceLayoutBox(ATextBox).Count - 1;
        if TdxFormattedTextSpaceLayoutBox(ATextBox).Count > 0 then
        begin
          Dec(FTextStart, TdxFormattedTextSpaceLayoutBox(ATextBox).Count);
          ATextBox.Free;
          GoToNextRow;
          if FIsClippedText then
            Break;
          ATextBox := CreateSpaceLayoutBox;
        end;
      end;

      FRow.Add(ATextBox);
      if FRowTextWidth > AvailableRowWidth then
      begin
        if FIsEndEllipsisRow then
        begin
          FRowTextWidth := FRowTextWidth - ATextBox.Size.cx;
          TdxFormattedTextSpaceLayoutBox(ATextBox).Count :=
            TdxFormattedTextSpaceLayoutBox(ATextBox).Count - 1;
          Dec(FTextStart);
          FRowTextWidth := FRowTextWidth + ATextBox.Size.cx;
          Break;
        end
        else if (ATextBox.BoxType = lbtTab) and FTextParams.ExpandTabs and
          FTextParams.WordBreak and not FTextParams.SingleLine and
          ((FRow.Count > 1) or (TdxFormattedTextTabLayoutBox(ATextBox).Count
          > 1)) then
        begin
          TdxFormattedTextTabLayoutBox(ATextBox).Count :=
            TdxFormattedTextTabLayoutBox(ATextBox).Count - 1;
          FRowTextWidth := GetAdjustedWidth(FRowTextWidth - ATextBox.Size.cx,
            TdxFormattedTextTabLayoutBox(ATextBox).FTabWidth);
          Dec(FTextStart);
          if TdxFormattedTextTabLayoutBox(ATextBox).Count < 1 then
            FRow.Delete(FRow.Count - 1);
          GoToNextRow;
        end;
      end;
      if FTextParams.WordBreak and not FTextParams.SingleLine then
        InitializePossibleBreakLineInfo;
    end;
  end;
end;

procedure TdxFormattedTextLayoutBuilder.ProcessText;

  function BreakAWord(AText: PChar; ATextLength, AWidth: Integer): PChar;
  var
    ALow, AHigh, ANew: Integer;
  begin
    ALow := 0;
    AHigh := ATextLength;
    while (AHigh - ALow) > 1 do
    begin
      ANew := ALow + (AHigh - ALow) div 2;
      if cxCalcTextExtents(FDCHandle, AText, ANew, True).cx > AWidth then
        AHigh := ANew
      else
        ALow := ANew;
    end;
    if FRow.Count = 0 then
      ALow := Max(1, ALow);
    Result := AText + ALow;
  end;

  function IsEndRow: Boolean;
  begin
    Result := FIsClippedText and (FRow <> nil) and
      not(FTextParams.EditControl and (GetTextSize.cy < Bounds.Height));
  end;

  function CanWordBreak: Boolean;
  begin
    Result := (FAbilityBreakLineBoxIndex > -1) and FTextParams.WordBreak and
      not FTextParams.SingleLine;
  end;

  function NeedRollback: Boolean;
  begin
    Result := (FLayout.Count > 1) and
      (FLayout[FLayout.Count - 2].Count > FAbilityBreakLineBoxIndex + 1) and
      not FIsClippedText;
  end;

  procedure SplitText(ATextStart: PChar; var AStr: string);
  var
    APreviousRow: TdxFormattedTextLayoutRow;
    ATextBox: TdxFormattedTextLayoutBox;
  begin
    if FIsEndEllipsisRow then
    begin
      FRowTextWidth := FRowTextWidth - Measurer.MeasureText(FDCHandle, AStr,
        GetFontInfo).cx;
      FTextStart := BreakAWord(ATextStart, FTextStart - ATextStart,
        AvailableRowWidth - FRowTextWidth);
      if FTextStart > ATextStart then
      begin
        AStr := dxGetString(ATextStart, FTextStart);
        FRowTextWidth := FRowTextWidth + Measurer.MeasureText(FDCHandle, AStr,
          GetFontInfo).cx + Measurer.MeasureText(FDCHandle, dxEndEllipsisChars,
          GetFontInfo).cx;
        ATextBox := CreateTextLayoutBox(AStr, GetFontInfo);
        FRow.Add(ATextBox);
      end;
      ATextBox := CreateTextLayoutBox(dxEndEllipsisChars, GetFontInfo);
      FRow.Add(ATextBox);
      FIsClippedText := True;
    end
    else if CanWordBreak then
    begin
      GoToNextRow;
      FTextStart := ATextStart;
      if IsEndRow then
        while FRow.Count > FAbilityBreakLineBoxIndex + 1 do
          FRow.Remove(FRow[FAbilityBreakLineBoxIndex + 1])
      else if NeedRollback then
      begin
        InitializeRollbackInfo(FAbilityBreakLineBoxRollbackInfo.TextPosition,
          FAbilityBreakLineBoxRollbackInfo.RunIndex);
        APreviousRow := FLayout[FLayout.Count - 2];
        while APreviousRow.Count > FAbilityBreakLineBoxIndex + 1 do
        begin
          FRow.Add(APreviousRow[FAbilityBreakLineBoxIndex + 1]);
          FRowTextWidth := FRowTextWidth + APreviousRow
            [FAbilityBreakLineBoxIndex + 1].Size.cx;
          APreviousRow.Extract(APreviousRow[FAbilityBreakLineBoxIndex + 1]);
        end;
      end;
      FAbilityBreakLineBoxIndex := -1;
    end
    else
    begin
      if FTextParams.CharBreak then
      begin
        FRowTextWidth := FRowTextWidth - Measurer.MeasureText(FDCHandle, AStr,
          GetFontInfo).cx;
        FTextStart := BreakAWord(ATextStart, FTextStart - ATextStart,
          AvailableRowWidth - FRowTextWidth);
        if FTextStart > ATextStart then
        begin
          AStr := dxGetString(ATextStart, FTextStart);
          FRowTextWidth := FRowTextWidth + Measurer.MeasureText(FDCHandle, AStr,
            GetFontInfo).cx;
          ATextBox := CreateTextLayoutBox(AStr, GetFontInfo);
          FRow.Add(ATextBox);
        end;
        GoToNextRow;
      end
      else
      begin
        ATextBox := CreateTextLayoutBox(AStr, GetFontInfo);
        FRow.Add(ATextBox);
      end;
    end;
  end;

  function NeedSplitText: Boolean;
  begin
    Result := (FRowTextWidth > AvailableRowWidth) and
      (FTextParams.WordBreak and not FTextParams.SingleLine or
      FIsEndEllipsisRow);
  end;

var
  AStr: string;
  ATextStart: PChar;
  ATextBox: TdxFormattedTextLayoutBox;
begin
  if FRow.Count = 0 then
    InitializeRollbackInfo(FTextStart, RunIndex);
  ATextStart := FTextStart;
  FTextStart := cxGetNextWordBreak(CP_ACP, ATextStart, FTextFinish);
  AStr := dxGetString(ATextStart, FTextStart);
  FRowTextWidth := FRowTextWidth + Measurer.MeasureText(FDCHandle, AStr,
    GetFontInfo).cx;
  if NeedSplitText then
    SplitText(ATextStart, AStr)
  else
  begin
    ATextBox := CreateTextLayoutBox(AStr, GetFontInfo);
    FRow.Add(ATextBox);
  end;
end;

procedure TdxFormattedTextLayoutBuilder.ReturnToRowBeginning;
var
  I: Integer;
begin
  FAbilityBreakLineBoxIndex := -1;
  RunIndex := FLastRowRollbackInfo.RunIndex;
  FTextStart := FLastRowRollbackInfo.TextPosition;
  FTextFinish := FRun.TextStart + FRun.TextLength;
  FRowTextWidth := 0;
  for I := FRow.Count - 1 downto 0 do
    FRow.Delete(0);
end;

procedure TdxFormattedTextLayoutBuilder.SetRunIndex(AValue: Integer);

  procedure InitializeHyperlink;
  var
    AHyperlinkData: TdxFormattedTextHyperlinkData;
  begin
    if (FHyperlinkIndex = -1) and (FRun.Action = traOpen) and
      (FRun is TdxFormattedTextURLRun) then
    begin
      TdxFormattedTextURLRun(FRun).FColor := ColorToRGB(HyperlinkColor);
      AHyperlinkData.Hyperlink := TdxFormattedTextURLRun(FRun).Hyperlink;
      AHyperlinkData.BeginRowIndex := -1;
      AHyperlinkData.BeginBoxIndex := -1;
      AHyperlinkData.BoxCount := 0;
      FHyperlinkIndex := FLayout.Hyperlinks.Add(AHyperlinkData);
    end
    else if (FRun.Action = traClose) and (FRun is TdxFormattedTextURLRun) then
      FHyperlinkIndex := -1;
  end;

begin
  if AValue < FRuns.Count then
  begin
    FRun := FRuns[AValue];
    InitializeHyperlink;
    if AValue = FRunIndex + 1 then
      FRunStack.ProcessRun(FRun)
    else
      PopulateRunStack(AValue);
    FCurrentCharacterProperties := DefaultCharacterProperties;
    FRunStack.CalculateStyle(FCurrentCharacterProperties);
    SelectObject(FDCHandle, GetFontInfo.GdiFontHandle);
  end;
  FRunIndex := AValue;
end;

{ TdxFormattedTextPainter }

constructor TdxFormattedTextPainter.Create(ADCHandle: HDC);
begin
  inherited Create;
  FDCHandle := ADCHandle;
end;

procedure TdxFormattedTextPainter.ExtTextOut(const ABounds: TRect;
  AFontHandle: THandle; AFontColor: TColor; const AText: string;
  AGlyphs: PdxWordArray; AGlyphCount: Integer; AGlyphWidths: PdxIntegerArray);
begin
  SelectObject(FDCHandle, AFontHandle);
  SetTextColor(FDCHandle, AFontColor);
  SetBkMode(FDCHandle, TRANSPARENT);
  Windows.ExtTextOut(FDCHandle, ABounds.Left, ABounds.Top, ETO_GLYPH_INDEX or
    ETO_IGNORELANGUAGE, @ABounds, @AGlyphs[0], AGlyphCount, @AGlyphWidths[0]);
end;

procedure TdxFormattedTextPainter.FillRect(const ARect: TRect; AColor: TColor);
begin
  FillRectByColor(FDCHandle, ARect, AColor);
end;

procedure TdxFormattedTextPainter.IntersectClipRect(ARect: TRect);
begin
  Windows.IntersectClipRect(FDCHandle, ARect.Left, ARect.Top, ARect.Right,
    ARect.Bottom);
end;

procedure TdxFormattedTextPainter.Restore;
begin
  RestoreDC(FDCHandle, FSaveDC);
end;

procedure TdxFormattedTextPainter.Store;
begin
  FSaveDC := SaveDC(FDCHandle);
end;

{ TdxFormattedTextLayoutBuilderInitialData }

function TdxFormattedTextLayoutBuilderInitialData.Compare
  (AInitialData: TdxFormattedTextLayoutBuilderInitialData): Boolean;
begin
  Result := CharacterProperties.Compare(AInitialData.CharacterProperties) and
    (ScaleFactor <> nil) and
    (ScaleFactor.TargetDPI = AInitialData.ScaleFactor.TargetDPI) and
    cxSizeIsEqual(Bounds.Size, AInitialData.Bounds.Size) and
    (ZoomFactor = AInitialData.ZoomFactor) and
    (TextOutFormat = AInitialData.TextOutFormat) and
    (HyperlinkColor = AInitialData.HyperlinkColor);
end;

procedure TdxFormattedTextLayoutBuilderInitialData.Initialize(AFont: TFont;
  const ABounds: TRect; ATextOutFormat: TcxTextOutFormat;
  AScaleFactor: TdxScaleFactor; AZoomFactor: Single;
  const AHyperlinkColor: TColor);
begin
  ScaleFactor := AScaleFactor;
  Bounds := cxRectSetNullOrigin(ABounds);
  CharacterProperties.Initialize(AFont,
    Abs(MulDiv(AFont.Height * 2, 72, AScaleFactor.TargetDPI)));
  TextOutFormat := ATextOutFormat;
  ZoomFactor := AZoomFactor;
  HyperlinkColor := AHyperlinkColor;
end;

{ TdxFormattedTextLayout }

constructor TdxFormattedTextLayout.Create(AMeasureTrailingSpaces: Boolean);
begin
  inherited Create;
  FHyperlinks := TList<TdxFormattedTextHyperlinkData>.Create;
  FMeasureTrailingSpaces := AMeasureTrailingSpaces;
end;

destructor TdxFormattedTextLayout.Destroy;
begin
  FreeAndNil(FHyperlinks);
  inherited;
end;

procedure TdxFormattedTextLayout.Draw(APainter: TdxFormattedTextCustomPainter;
  const AOffset: TPoint; AClip: Boolean);
var
  I: Integer;
begin
  APainter.Store;
  try
    if AClip then
      APainter.IntersectClipRect(cxRectOffset(cxRectSetWidth(FBounds,
        FBounds.Width + 1), AOffset));
    for I := 0 to Count - 1 do
      Items[I].Draw(APainter, AOffset);
  finally
    APainter.Restore;
  end;
end;

function TdxFormattedTextLayout.HitTest(const APoint: TPoint)
  : TdxFormattedTextLayoutBox;
var
  I: Integer;
begin
  Result := nil;
  if not cxRectPtIn(Bounds, APoint) then
    Exit;

  for I := 0 to Count - 1 do
    if cxRectPtIn(Items[I].Bounds, APoint) then
    begin
      Result := Items[I].HitTest(APoint);
      Break;
    end;
end;

function TdxFormattedTextLayout.GetTextSize: TSize;
var
  ARow: TdxFormattedTextLayoutRow;
  ARowSize: TSize;
  I: Integer;
begin
  Result := cxNullSize;
  for I := 0 to Count - 1 do
  begin
    ARow := Items[I];
    ARowSize := ARow.GetTextSize(ARow.Count, I = Count - 1);
    Result.cx := Max(Result.cx, ARowSize.cx);
    Result.cy := Result.cy + ARowSize.cy;
  end;
end;

procedure TdxFormattedTextLayout.SetInitialData(const AValue
  : TdxFormattedTextLayoutBuilderInitialData);
begin
  FInitialData := AValue;
  Bounds := FInitialData.Bounds;
end;

{ TdxFormattedTextLayoutRow }

constructor TdxFormattedTextLayoutRow.Create(const ATextParams: TcxTextParams);
begin
  inherited Create;
  FTextAlignX := ATextParams.TextAlignX;
  FTextAlignY := ATextParams.TextAlignY;
  FExpandTabs := ATextParams.ExpandTabs;
end;

procedure TdxFormattedTextLayoutRow.Calculate(ABounds: TRect);
var
  ABoxBounds: TRect;
  ALayoutBox: TdxFormattedTextLayoutBox;
  APosition: TPoint;
  ASpaceFactor: Single;
  ASpaceResidue: Single;
  AUnderlineAveragePosition: Integer;
  AUnderlineAverageThickness: Integer;
  AUnderlineBoxIndex: Integer;
  AUnderlineWidth: Integer;
  I: Integer;
begin
  FBounds := ABounds;
  AUnderlineBoxIndex := -1;
  FAscentAndFree := GetAscentAndFree;
  APosition := Point(GetFirstBoxHorizontalPosition, Bounds.Top);
  ASpaceResidue := 0;
  ASpaceFactor := GetSpaceFactor;

  for I := 0 to Count - 1 do
  begin
    ALayoutBox := Items[I];
    APosition.Y := Bounds.Top;
    if FAscentAndFree <> 0 then
      APosition.Y := APosition.Y + FAscentAndFree -
        ALayoutBox.FontInfo.AscentAndFree;

    if (ALayoutBox.BoxType = lbtTab) and FExpandTabs then
      APosition.X := GetFirstBoxHorizontalPosition +
        ((APosition.X - GetFirstBoxHorizontalPosition)
        div TdxFormattedTextTabLayoutBox(ALayoutBox).FTabWidth) *
        TdxFormattedTextTabLayoutBox(ALayoutBox).FTabWidth;

    ABoxBounds := cxRectSetOrigin(cxRect(ALayoutBox.Size), APosition);
    case ALayoutBox.CharacterFormattingScript of
      TdxCharacterFormattingScript.Subscript:
        ABoxBounds := cxRectOffset(ABoxBounds,
          ALayoutBox.FontInfo.SubscriptOffset);
      TdxCharacterFormattingScript.Superscript:
        ABoxBounds := cxRectOffset(ABoxBounds,
          ALayoutBox.FontInfo.SuperscriptOffset);
    end;
    AlignBox(I, ASpaceFactor, ASpaceResidue, ABoxBounds);
    ALayoutBox.Calculate(ABoxBounds);
    APosition.X := ALayoutBox.Bounds.Right;
    CalculateUnderline(I, AUnderlineBoxIndex, AUnderlineAverageThickness,
      AUnderlineAveragePosition, AUnderlineWidth);
  end;
end;

procedure TdxFormattedTextLayoutRow.Draw
  (APainter: TdxFormattedTextCustomPainter; const AOffset: TPoint);

  function GetFillRect(AIndex: Integer): TRect;
  begin
    if AIndex < Count - 1 then
      Result := Rect(Items[AIndex].Bounds.Left, Bounds.Top,
        Items[AIndex + 1].Bounds.Left, Bounds.Bottom)
    else
      Result := Rect(Items[AIndex].Bounds.Left, Bounds.Top,
        Items[AIndex].Bounds.Right, Bounds.Bottom);
    Result := cxRectOffset(Result, AOffset);
  end;

var
  ABox: TdxFormattedTextLayoutBox;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    ABox := Items[I];
    if ABox.BackgroundColor <> clDefault then
      APainter.FillRect(GetFillRect(I), ABox.BackgroundColor);
  end;
  for I := 0 to Count - 1 do
    Items[I].Draw(APainter, AOffset);
end;

function TdxFormattedTextLayoutRow.GetTextSize(ABoxCount: Integer;
  AIsLast: Boolean): TSize;
var
  ALayoutBox: TdxFormattedTextLayoutBox;
  AStartIndex, AFinishIndex: Integer;
  I: Integer;
begin
  Result := cxNullSize;
  AStartIndex := 0;
  AFinishIndex := AStartIndex + ABoxCount - 1;
  while (AStartIndex <= AFinishIndex) and
    (Items[AStartIndex].BoxType in [lbtSpace, lbtCR]) do
  begin
    Inc(Result.cx, Items[AStartIndex].Size.cx);
    Inc(AStartIndex);
  end;

  while (AFinishIndex >= AStartIndex) and
    (Items[AFinishIndex].BoxType in [lbtSpace, lbtCR]) do
  begin
    if FMeasureTrailingSpaces and ((FTextAlignX = taLeft) or AIsLast) then
      Inc(Result.cx, Items[AFinishIndex].Size.cx);
    Dec(AFinishIndex);
  end;

  for I := AStartIndex to AFinishIndex do
  begin
    ALayoutBox := Items[I];
    Inc(Result.cx, ALayoutBox.Size.cx);
    if (ALayoutBox.BoxType in [lbtTab]) and
      (TdxFormattedTextTabLayoutBox(ALayoutBox).FTabWidth > 0) then
      Result.cx := (Result.cx div TdxFormattedTextTabLayoutBox(ALayoutBox)
        .FTabWidth) * TdxFormattedTextTabLayoutBox(ALayoutBox).FTabWidth;
    if not(ALayoutBox.BoxType in [lbtSpace, lbtCR]) then
      Result.cy := Max(Result.cy, ALayoutBox.Size.cy);
  end;

  if (Result.cy = 0) and (ABoxCount > 0) then
    Result.cy := Items[ABoxCount - 1].Size.cy;
end;

function TdxFormattedTextLayoutRow.HitTest(const APoint: TPoint)
  : TdxFormattedTextLayoutBox;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if cxRectPtIn(Items[I].Bounds, APoint) then
      Exit(Items[I]);
  end;
  Result := nil;
end;

procedure TdxFormattedTextLayoutRow.AlignBox(AIndex: Integer;
  ASpaceFactor: Single; var ASpaceResidue: Single; var ABoxBounds: TRect);
begin
  if (FTextAlignX in [taJustifyX, taDistributeX]) and (ASpaceFactor > 1) then
  begin
    if (Items[AIndex].BoxType = lbtSpace) and (AIndex >= SkipLeftSpaceBlocks)
      and (AIndex <= SkipRightSpaceBlocks) then
    begin
      ASpaceResidue := ASpaceResidue + ABoxBounds.Width * ASpaceFactor -
        Trunc(ABoxBounds.Width * ASpaceFactor);
      if ASpaceResidue >= 1 then
      begin
        ABoxBounds := cxRectSetWidth(ABoxBounds,
          Trunc(ABoxBounds.Width * ASpaceFactor) + 1);
        ASpaceResidue := ASpaceResidue - 1;
      end
      else
        ABoxBounds := cxRectSetWidth(ABoxBounds,
          Trunc(ABoxBounds.Width * ASpaceFactor));
    end;
    if (AIndex = SkipRightSpaceBlocks) and (AIndex > SkipLeftSpaceBlocks) then
      ABoxBounds := cxRectSetOrigin(ABoxBounds,
        Point(Bounds.Right - ABoxBounds.Width, ABoxBounds.Top));
  end;
end;

function TdxFormattedTextLayoutRow.GetAscentAndFree: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].BoxType = lbtText then
      Result := Max(Result, Items[I].FontInfo.AscentAndFree);
end;

function TdxFormattedTextLayoutRow.GetFirstBoxHorizontalPosition: Integer;
begin
  if FTextAlignX in [taRight, taCenterX] then
  begin
    Result := Bounds.Width - GetTextSize(Count, False).cx;
    if FTextAlignX = taCenterX then
      Result := Result div 2;
  end
  else
    Result := Bounds.Left;
end;

function TdxFormattedTextLayoutRow.GetSpaceFactor: Single;
begin
  Result := 1;
  if (FTextAlignX in [taJustifyX, taDistributeX]) and
    (GetSpacesWidth(SkipLeftSpaceBlocks, SkipRightSpaceBlocks) > 0) then
    Result := (Bounds.Width - GetTextSize(Count, False).cx +
      GetSpacesWidth(SkipLeftSpaceBlocks, SkipRightSpaceBlocks)) /
      GetSpacesWidth(SkipLeftSpaceBlocks, SkipRightSpaceBlocks);
end;

procedure TdxFormattedTextLayoutRow.CalculateUnderline(AIndex: Integer;
  var AUnderlineBoxIndex, AUnderlineAverageThickness, AUnderlineAveragePosition,
  AUnderlineWidth: Integer);
var
  I: Integer;
begin
  if Items[AIndex].Underline and (Items[AIndex].Bounds.Width > 0) then
  begin
    if AUnderlineBoxIndex = -1 then
    begin
      AUnderlineBoxIndex := AIndex;
      AUnderlineAveragePosition := Items[AIndex].FontInfo.UnderlinePosition *
        Items[AIndex].Bounds.Width;
      AUnderlineAverageThickness := Items[AIndex].FontInfo.UnderlineThickness *
        Items[AIndex].Bounds.Width;
      AUnderlineWidth := Items[AIndex].Bounds.Width;
    end
    else
    begin
      AUnderlineAveragePosition := AUnderlineAveragePosition + Items[AIndex]
        .FontInfo.UnderlinePosition * Items[AIndex].Bounds.Width;
      AUnderlineAverageThickness := AUnderlineAverageThickness + Items[AIndex]
        .FontInfo.UnderlineThickness * Items[AIndex].Bounds.Width;
      AUnderlineWidth := AUnderlineWidth + Items[AIndex].Bounds.Width;
    end;
  end;
  if (AUnderlineBoxIndex > -1) and
    (not Items[AIndex].Underline or (AIndex = Count - 1)) then
  begin
    AUnderlineAveragePosition :=
      Round(AUnderlineAveragePosition / AUnderlineWidth);
    AUnderlineAverageThickness :=
      Round(AUnderlineAverageThickness / AUnderlineWidth);
    for I := AUnderlineBoxIndex to AIndex do
    begin
      Items[I].UnderlinePosition := AUnderlineAveragePosition;
      Items[I].UnderlineThickness := AUnderlineAverageThickness;
    end;
    AUnderlineBoxIndex := -1;
  end;
end;

function TdxFormattedTextLayoutRow.GetSpacesWidth(AStartIndex,
  AFinishIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := SkipLeftSpaceBlocks to SkipRightSpaceBlocks do
    if Items[I].BoxType = lbtSpace then
      Inc(Result, Items[I].Size.cx);
end;

function TdxFormattedTextLayoutRow.SkipLeftSpaceBlocks: Integer;
begin
  Result := 0;
  while (Result <= Count - 1) and (Items[Result].BoxType = lbtSpace) do
    Inc(Result);
end;

function TdxFormattedTextLayoutRow.SkipRightSpaceBlocks: Integer;
begin
  Result := Count - 1;
  while (Result > 0) and (Items[Result].BoxType in [lbtSpace, lbtCR]) do
    Dec(Result);
end;

{ TdxFormattedTextLayoutBox }

procedure TdxFormattedTextLayoutBox.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxFormattedTextLayoutBox.Draw
  (APainter: TdxFormattedTextCustomPainter; const AOffset: TPoint);
var
  ABounds: TRect;
begin
  ABounds := cxRectOffset(Bounds, AOffset);
  if Underline then
    APainter.FillRect(cxRectSetHeight(cxRectOffsetVert(ABounds,
      UnderlinePosition + FontInfo.AscentAndFree), UnderlineThickness),
      FontColor);

  if Strikeout then
    APainter.FillRect(cxRectSetHeight(cxRectOffsetVert(ABounds,
      FontInfo.AscentAndFree - FontInfo.StrikeoutPosition),
      FontInfo.StrikeoutThickness), FontColor);
end;

function TdxFormattedTextLayoutBox.IsHyperlink: Boolean;
begin
  Result := FHyperlinkIndex > -1;
end;

function TdxFormattedTextLayoutBox.GetBoxType: TdxLayoutBoxType;
begin
  Result := lbtText;
end;

function TdxFormattedTextLayoutBox.GetSize: TSize;
begin
  Result := cxSize(0, FontInfo.LineSpacing);
end;

{ TdxFormattedEmptyLayoutBox }

function TdxFormattedEmptyLayoutBox.GetBoxType: TdxLayoutBoxType;
begin
  Result := lbtEmpty;
end;

{ TdxFormattedCRLayoutBox }

function TdxFormattedCRLayoutBox.GetBoxType: TdxLayoutBoxType;
begin
  Result := lbtCR;
end;

{ TdxFormattedTextSpaceLayoutBox }

function TdxFormattedTextSpaceLayoutBox.GetSize: TSize;
begin
  Result := cxSize(FontInfo.SpaceWidth * Count, FontInfo.LineSpacing);
end;

function TdxFormattedTextSpaceLayoutBox.GetBoxType: TdxLayoutBoxType;
begin
  Result := lbtSpace;
end;

{ TdxFormattedTextTabLayoutBox }

function TdxFormattedTextTabLayoutBox.GetBoxType: TdxLayoutBoxType;
begin
  Result := lbtTab;
end;

function TdxFormattedTextTabLayoutBox.GetSize: TSize;
begin
  Result := cxSize(FTabWidth * Count, FontInfo.LineSpacing);
end;

{ TdxFormattedTextLayoutBox }

constructor TdxFormattedTextTextLayoutBox.Create(const AText: string;
  ATextViewInfo: TdxTextViewInfo);
begin
  FText := AText;
  FTextViewInfo := ATextViewInfo;
end;

procedure TdxFormattedTextTextLayoutBox.Draw
  (APainter: TdxFormattedTextCustomPainter; const AOffset: TPoint);
var
  ABounds: TRect;
begin
  inherited;
  ABounds := cxRectOffset(Bounds, AOffset.X, AOffset.Y - FontInfo.GdiOffset +
    (FontInfo._Free - FontInfo.DrawingOffset));
  APainter.ExtTextOut(ABounds, FontInfo.GdiFontHandle, FontColor, Text,
    TextViewInfo.Glyphs, TextViewInfo.GlyphCount, TextViewInfo.CharacterWidths);
end;

function TdxFormattedTextTextLayoutBox.GetSize: TSize;
begin
  Result := cxSize(FTextViewInfo.Size.cx, FontInfo.LineSpacing);
end;

{ TdxFormattedTextConverter }

class function TdxFormattedTextConverter.CanImport(const ASource
  : string): Boolean;
begin
  Result := False;
end;

class procedure TdxFormattedTextConverter.Import(ATarget: TdxFormattedText;
  const ASource: string; ADefaultFont: TFont = nil);
begin
  ATarget.Text := ASource;
end;

class function TdxFormattedTextConverter.Export(ASource: TdxFormattedText;
  ADefaultFont: TFont = nil): string;
begin
  Result := ASource.Text;
end;

class procedure TdxFormattedTextConverter.AdjustInternalRuns
  (ASource: TdxFormattedText; ADefaultFont: TFont);
begin
  ASource.CreateInternalRuns(ADefaultFont);
end;

{ TdxFormattedTextConverters }

class function TdxFormattedTextConverters.GetConverterClass
  (const ASource: string): TdxFormattedTextConverterClass;
var
  I: Integer;
begin
  Result := nil;
  for I := FList.Count - 1 downto 0 do
  begin
    Result := FList.List[I];
    if TdxFormattedTextConverterClass(FList.List[I]).CanImport(ASource) then
    begin
      Result := FList.List[I];
      Break;
    end;
  end;
end;

class procedure TdxFormattedTextConverters.Import(ATarget: TdxFormattedText;
  const ASource: string; ADefaultFont: TFont);
var
  AConverterClass: TdxFormattedTextConverterClass;
begin
  AConverterClass := GetConverterClass(ASource);
  if AConverterClass <> nil then
    AConverterClass.Import(ATarget, ASource, ADefaultFont);
end;

class procedure TdxFormattedTextConverters.Import(ATarget: TdxFormattedText;
  const ASource: Variant; ADefaultFont: TFont);
begin
  Import(ATarget, VarToStr(ASource), ADefaultFont);
end;

class function TdxFormattedTextConverters.Export(ASource: TdxFormattedText;
  AConverterClass: TdxFormattedTextConverterClass;
  ADefaultFont: TFont = nil): string;
begin
  Result := AConverterClass.Export(ASource, ADefaultFont);
end;

class procedure TdxFormattedTextConverters.Register(AConverterClass
  : TdxFormattedTextConverterClass);
begin
  if FList = nil then
    FList := TList.Create;
  FList.Add(AConverterClass);
end;

class procedure TdxFormattedTextConverters.Unregister(AConverterClass
  : TdxFormattedTextConverterClass);
begin
  if FList <> nil then
  begin
    FList.Remove(AConverterClass);
    if FList.Count = 0 then
      FreeAndNil(FList);
  end;
end;

initialization

TdxFormattedTextConverters.Register(TdxFormattedTextConverter);

finalization

TdxFormattedTextConverters.Unregister(TdxFormattedTextConverter);
FreeAndNil(FFormattedTextLayoutBuilder);

end.
