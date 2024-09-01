{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit VCL.TMSFNCPDFLib.General;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

procedure RegisterPDFLibGeneralService;
procedure UnRegisterPDFLibGeneralService;

implementation

uses
  Classes, Math, DateUtils, Types, SysUtils, VCL.TMSFNCPDFGraphicsLib, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes
  ,VCL.TMSFNCPDFLib, VCL.TMSFNCUtils, VCL.TMSFNCPDFCoreLibBase, VCL.Graphics, VCL.TMSFNCTypes,
  VCL.TMSFNCBitmapContainer
  {$IFDEF WEBLIB}
  ,Contnrs, Web
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections, Generics.Defaults
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  ,VCL.TMSFNCPDFLib.General.iOS
  {$ELSE}
  ,VCL.TMSFNCPDFLib.General.Mac
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  ,VCL.TMSFNCPDFLib.General.Android
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,VCL.TMSFNCPDFLib.General.Win
  {$ENDIF}
  {$IFDEF UNIX}
  ,VCL.TMSFNCPDFLib.General.Unix
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,VCL.TMSFNCPDFLib.General.WEB
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFNDEF MSWINDOWS}
  {$IFNDEF UNIX}
  ,VCL.TMSFNCPDFLib.General.Default
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

type
  TTMSFNCGeneralPDFLibService = class;

  TTMSFNCGeneralPDFLibService = class(TTMSFNCPDFLibFactoryService)
  protected
    function DoCreatePDFLib: ITMSFNCCustomPDFLib; override;
  end;

  {$IFDEF WEBLIB}
  UInt32 = Cardinal;
  {$ENDIF}

const
  DefaultMediaBox: TRectF = (Left: 0; Top: 0; Right: 612; Bottom: 792);
  {%H-}PDFStart17 = '%PDF-1.7'#13#10'%'#249#250#251#252;
  {%H-}PDFStart16 = '%PDF-1.6'#13#10'%'#245#246#247#248;
  {%H-}PDFStart15 = '%PDF-1.5'#13#10'%'#241#242#243#244;
  {%H-}PDFStart14 = '%PDF-1.4'#13#10'%'#228#229#230#240;
  {%H-}PDFStart13 = '%PDF-1.3';
  PDFStart = PDFStart17;
  PDFEnd = '%%EOF';
  PDFCR = #13;
  PDFLF = #10;
  PDFLB = PDFCR + PDFLF;
  PDFMaxXref = 65535;
  PDFDestOutputProfileRef = '{PDFDESTOUTPUTPROFILEREFERENCE}';
  PDFDestOutputProfileLengthRef = '{PDFDESTOUTPUTPROFILELENGTHREFERENCE}';
  PDFPageFontTextRef = '{PDFPAGEFONTTEXTREFERENCE%s}';
  PDFPageFontGlyphTextRef = '{PDFPAGEFONTGLYPHTEXTREFERENCE%s}';
  PDFPageRef = '{PDFPAGEREFERENCE}';
  PDFPageChildRef = '{PDFPAGECHILDREFERENCE}';
  PDFPageAcroFormRef = '{PDFPAGEACROFORMREFERENCE}';
  PDFPageAcroFormFieldsRef = '{PDFPAGEACROFORMFIELDSREFERENCE}';
  PDFPageAcroFormFieldContentRef = '{PDFPAGEACROFORMFIELDCONTENTREFERENCE}';
  PDFPageContentRef = '{PDFPAGECONTENTREFERENCE}';
  PDFPageContentLengthRef = '{PDFPAGECONTENTLENGTHREFERENCE}';
  PDFPageCountRef = '{PDFPAGECOUNTREFERENCE}';
  PDFPageFontRef = '{PDFPAGEFONTREFERENCE}';
  PDFPagePatternRef = '{PDFPAGEPATTERNREFERENCE}';
  PDFPageAnnotationsRef = '{PDFPAGEANNOTATIONSREFERENCE}';
  PDFPageShadingObjectRef = '{PDFPAGESHADINGOBJECTREFERENCE%s}';
  PDFPageBitmapObjectRef = '{PDFPAGEBITMAPOBJECTREFERENCE%s}';
  PDFPageXObjectRef = '{PDFPAGEXOBJECTREFERENCE}';

  ICC: array[0..139] of UInt32 = (
    805437440,1161970753,4098,1920233069,541214546,542792024,134270983,318769920,989868800,
    1886610273,1280331841,0,1701736302,0,0,0,0,3606446080,256,768802816,1161970753,
    0,0,0,0,0,0,0,0,0,0,0,167772160,1953656931,4227858432,838860800,1668506980,805371904,
    1795162112,1953526903,2617311232,335544320,1953524578,2952855552,335544320,1129469042,
    3288399872,234881024,1129469031,3556835328,234881024,1129469026,3825270784,234881024,
    1515804786,4093706240,335544320,1515804775,134348800,335544320,1515804770,469893120,
    335544320,1954047348,0,2037411651,1751607666,808591476,1092628528,1700949860,1937330976,
    1936549236,1668172064,1869640303,1702125938,100,1668506980,0,285212672,1651467329,
    1196564581,824713282,691550521,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,542792024,0,
    1374879744,256,3423994112,542792024,0,0,0,0,1987212643,0,16777216,13058,1987212643,0,
    16777216,13058,1987212643,0,16777216,13058,542792024,0,412876800,2773417984,4228120576,
    542792024,0,2368995328,748683264,2500788224,542792024,0,824573952,789577728,2629697536);

type
  TTMSFNCGeneralPDFLib = class;

  TTMSFNCGeneralPDFLibObjectType = (lotDirect, lotIndirect);

  TTMSFNCGeneralPDFLibXRefObject = class;

  TTMSFNCGeneralPDFLibObject = class
  private
    FPDFLib: TTMSFNCGeneralPDFLib;
    FXRefNumber: integer;
    FNumber: integer;
    FType: TTMSFNCGeneralPDFLibObjectType;
    FXRefObject: TTMSFNCGeneralPDFLibXRefObject;
    FRefNameOriginal: string;
    FInactive: Boolean;
    FRefNameGlyphOriginal: string;
    procedure SetNumber(const Value: integer);
    function GetRef: string;
    function GetRefName: string;
    function GetRefNameNotEscaped: string;
    function GetRefNameBase: String; virtual;
    function GetRefIndex(ACheckClassType: Boolean): Integer; virtual;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const ANumber: Integer = -1; const AXRefNumber: Integer = 0); virtual;
    destructor Destroy; override;
    property Inactive: Boolean read FInactive write FInactive;
    property Number: integer read FNumber write SetNumber;
    property &Type: TTMSFNCGeneralPDFLibObjectType read FType;
    property XRefNumber: integer read FXRefNumber write FXRefNumber;
    property XRefObject: TTMSFNCGeneralPDFLibXRefObject read FXRefObject write FXRefObject;
    property PDFLib: TTMSFNCGeneralPDFLib read FPDFLib;
    property Ref: string read GetRef;
    property RefName: string read GetRefName;
    property RefNameNotEscaped: string read GetRefNameNotEscaped;
    property RefNameBase: String read GetRefNameBase;
    property RefNameOriginal: string read FRefNameOriginal write FRefNameOriginal;
    property RefNameGlyphOriginal: string read FRefNameGlyphOriginal write FRefNameGlyphOriginal;
  end;

  TTMSFNCGeneralPDFLibObjectClass = class of TTMSFNCGeneralPDFLibObject;

  TTMSFNCGeneralPDFLibDestOutputProfile = class(TTMSFNCGeneralPDFLibObject);
  TTMSFNCGeneralPDFLibInfo = class(TTMSFNCGeneralPDFLibObject);
  TTMSFNCGeneralPDFLibCatalog = class(TTMSFNCGeneralPDFLibObject);
  TTMSFNCGeneralPDFLibPages = class(TTMSFNCGeneralPDFLibObject);
  TTMSFNCGeneralPDFLibOutputIntent = class(TTMSFNCGeneralPDFLibObject);
  TTMSFNCGeneralPDFLibFontDescriptor = class;
  TTMSFNCGeneralPDFLibFontFile = class;
  TTMSFNCGeneralPDFLibFontDescendant = class;
  TTMSFNCGeneralPDFLibFontUnicode = class;
  TTMSFNCGeneralPDFLibFont = class;
  TTMSFNCGeneralPDFLibXObject = class;
  TTMSFNCGeneralPDFLibShading = class;
  TTMSFNCGeneralPDFLibPattern = class;
  TTMSFNCGeneralPDFLibBitmap = class;
  TTMSFNCGeneralPDFLibContent = class;
  TTMSFNCGeneralPDFLibAcroForm = class;
  TTMSFNCGeneralPDFLibAcroFormField = class;
  TTMSFNCGeneralPDFLibAcroFormFieldContent = class;
  TTMSFNCGeneralPDFLibAnnotation = class;
  TTMSFNCGeneralPDFLibPage = class;

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibPageList = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibPage;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibPage);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibPage read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibPageList = class(TList<TTMSFNCGeneralPDFLibPage>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibFonts = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibFont;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibFont);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibFont read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibFonts = class(TList<TTMSFNCGeneralPDFLibFont>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibXObjects = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibXObject;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibXObject);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibXObject read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibXObjects = class(TList<TTMSFNCGeneralPDFLibXObject>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibShadings = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibShading;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibShading);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibShading read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibShadings = class(TList<TTMSFNCGeneralPDFLibShading>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibPatterns = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibPattern;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibPattern);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibPattern read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibPatterns = class(TList<TTMSFNCGeneralPDFLibPattern>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibAnnotations = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibAnnotation;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibAnnotation);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibAnnotation read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibAnnotations = class(TList<TTMSFNCGeneralPDFLibAnnotation>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibBitmaps = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibBitmap;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibBitmap);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibBitmap read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibBitmaps = class(TList<TTMSFNCGeneralPDFLibBitmap>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibAcroFormFields = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibAcroForm;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibAcroForm);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibAcroForm read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibAcroFormFields = class(TList<TTMSFNCGeneralPDFLibAcroForm>);
  {$ENDIF}

  TTMSFNCGeneralPDFLibPage = class(TTMSFNCGeneralPDFLibObject)
  private
    FPageNumber: Integer;
    FFontList: TTMSFNCGeneralPDFLibFonts;
    FXObjectList: TTMSFNCGeneralPDFLibXObjects;
    FShadings: TTMSFNCGeneralPDFLibShadings;
    FPatterns: TTMSFNCGeneralPDFLibPatterns;
    FBitmaps: TTMSFNCGeneralPDFLibBitmaps;
    FContent: TTMSFNCGeneralPDFLibContent;
    FAnnotations: TTMSFNCGeneralPDFLibAnnotations;
    FInsertPageNumber: Integer;
  protected
    property InsertPageNumber: Integer read FInsertPageNumber write FInsertPageNumber;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const APageNumber: Integer); reintroduce; virtual;
    destructor Destroy; override;
    property PageNumber: Integer read FPageNumber write FPageNumber;
    property FontList: TTMSFNCGeneralPDFLibFonts read FFontList;
    property XObjectList: TTMSFNCGeneralPDFLibXObjects read FXObjectList;
    property Shadings: TTMSFNCGeneralPDFLibShadings read FShadings;
    property Bitmaps: TTMSFNCGeneralPDFLibBitmaps read FBitmaps;
    property Patterns: TTMSFNCGeneralPDFLibPatterns read FPatterns;
    property Annotations: TTMSFNCGeneralPDFLibAnnotations read FAnnotations;
    property Content: TTMSFNCGeneralPDFLibContent read FContent write FContent;
  end;

  TTMSFNCGeneralPDFLibAcroFormField = class(TTMSFNCGeneralPDFLibObject)
  private
    FContent: TTMSFNCGeneralPDFLibAcroFormFieldContent;
  public
    destructor Destroy; override;
    property Content: TTMSFNCGeneralPDFLibAcroFormFieldContent read FContent write FContent;
  end;

  TTMSFNCGeneralPDFLibContent = class(TTMSFNCGeneralPDFLibObject)
  private
    FPage: TTMSFNCGeneralPDFLibPage;
  public
    destructor Destroy; override;
    property Page: TTMSFNCGeneralPDFLibPage read FPage write FPage;
  end;

  TTMSFNCGeneralPDFLibAcroForm = class(TTMSFNCGeneralPDFLibObject);

  TTMSFNCGeneralPDFLibFont = class(TTMSFNCGeneralPDFLibObject)
  private
    FInitializer: TTMSFNCGeneralPDFLibFontInitializer;
    FSize: Single;
    FName: string;
    FStyle: TFontStyles;
    FBase: string;
    FUnicode: Boolean;
    FDescent: Integer;
    FAscent: Integer;
    FBox: TRect;
    FItalicAngle: Integer;
    FFontFile: TTMSFNCGeneralPDFLibFontFile;
    FFontDescriptor: TTMSFNCGeneralPDFLibFontDescriptor;
    FFontDescendant: TTMSFNCGeneralPDFLibFontDescendant;
    FFontUnicode: TTMSFNCGeneralPDFLibFontUnicode;
    FTrueType: Boolean;
    FCapHeight: Integer;
    FUnitsPerEm: Integer;
    function GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    function GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    function GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetRefNameBase: string; override;
  protected
    function BoxAsString: string;
    function WidthsAsString: string;
    function GlyphsAndWidthsAsString: string;
    function FirstGlyphAsString: string;
    function LastGlyphAsString: string;
    function FirstGlyph: Integer;
    function LastGlyph: Integer;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AName: String; const ASize: Single; const AStyle: TFontStyles); reintroduce; virtual;
    destructor Destroy; override;
    property Base: string read FBase write FBase;
    property Name: string read FName write FName;
    property Size: Single read FSize write FSize;
    property Style: TFontStyles read FStyle write FStyle;
    property Ascent: Integer read FAscent write FAscent;
    property CapHeight: Integer read FCapHeight write FCapHeight;
    property UnitsPerEm: Integer read FUnitsPerEm write FUnitsPerEm;
    property Descent: Integer read FDescent write FDescent;
    property ItalicAngle: Integer read FItalicAngle write FItalicAngle;
    property Box: TRect read FBox write FBox;
    property CharWidths: TTMSFNCPDFGraphicsLibFontCharWidths read GetCharWidths;
    property GlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray read GetGlyphIDs;
    property FontFile: TTMSFNCGeneralPDFLibFontFile read FFontFile write FFontFile;
    property FontDescriptor: TTMSFNCGeneralPDFLibFontDescriptor read FFontDescriptor write FFontDescriptor;
    property FontDescendant: TTMSFNCGeneralPDFLibFontDescendant read FFontDescendant write FFontDescendant;
    property FontUnicode: TTMSFNCGeneralPDFLibFontUnicode read FFontUnicode write FFontUnicode;
    property CharArray: TTMSFNCPDFGraphicsLibFontCharArray read GetCharArray;
    property UsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray read GetUsedCharArray;
    property Unicode: Boolean read FUnicode write FUnicode;
    property TrueType: Boolean read FTrueType write FTrueType;
    property Initializer: TTMSFNCGeneralPDFLibFontInitializer read FInitializer;
  end;

//  TTMSFNCGeneralPDFLibTextField = class(TTMSFNCGeneralPDFLibAcroFormField);

  TTMSFNCGeneralPDFLibFontDescriptor = class(TTMSFNCGeneralPDFLibObject)
  private
    FFont: TTMSFNCGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TTMSFNCGeneralPDFLibFont read FFont write FFont;
  end;

  TTMSFNCGeneralPDFLibFontFile = class(TTMSFNCGeneralPDFLibObject)
  private
    FFont: TTMSFNCGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TTMSFNCGeneralPDFLibFont read FFont write FFont;
    procedure InitializeFontFile;
  end;

  TTMSFNCGeneralPDFLibFontDescendant = class(TTMSFNCGeneralPDFLibObject)
  private
    FFont: TTMSFNCGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TTMSFNCGeneralPDFLibFont read FFont write FFont;
  end;

  TTMSFNCGeneralPDFLibFontUnicode = class(TTMSFNCGeneralPDFLibObject)
  private
    FFont: TTMSFNCGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TTMSFNCGeneralPDFLibFont read FFont write FFont;
  end;

  TTMSFNCGeneralPDFLibXObject = class(TTMSFNCGeneralPDFLibObject);

  TTMSFNCGeneralPDFLibBitmap = class(TTMSFNCGeneralPDFLibXObject)
  private
    FStream: TMemoryStream;
    FBitmap: TTMSFNCBitmap;
    FImageType: TTMSFNCPDFGraphicsLibImageType;
    FQuality: Single;
    FBackgroundColor: TTMSFNCGraphicsColor;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
    function GetRefNameBase: String; override;
    procedure SetStream(const Value: TMemoryStream);
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const ABitmap: TTMSFNCBitmapHelperClass; const AStream: TMemoryStream; const AImageType: TTMSFNCPDFGraphicsLibImageType; const AQuality: Single; const ABackgroundColor: TTMSFNCGraphicsColor); reintroduce; virtual;
    destructor Destroy; override;
    property Stream: TMemoryStream read FStream write SetStream;
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property ImageType: TTMSFNCPDFGraphicsLibImageType read FImageType write FImageType;
    property Quality: Single read FQuality write FQuality;
    property BackgroundColor: TTMSFNCGraphicsColor read FBackgroundColor write FBackgroundColor;
  end;

  TTMSFNCGeneralPDFLibShadingSubFunction = class(TTMSFNCGeneralPDFLibObject)
  private
    FFillColor: TTMSFNCGraphicsColor;
    FFillColorTo: TTMSFNCGraphicsColor;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFillColor: TTMSFNCGraphicsColor; const AFillColorTo: TTMSFNCGraphicsColor); reintroduce; virtual;
    property FillColor: TTMSFNCGraphicsColor read FFillColor write FFillColor;
    property FillColorTo: TTMSFNCGraphicsColor read FFillColorTo write FFillColorTo;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibShadingSubFunctions = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibShadingSubFunction;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibShadingSubFunction);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibShadingSubFunction read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibShadingSubFunctions = class(TList<TTMSFNCGeneralPDFLibShadingSubFunction>);
  {$ENDIF}

  TTMSFNCGeneralPDFLibShadingFunction = class(TTMSFNCGeneralPDFLibObject)
  private
    FSubFunctions: TTMSFNCGeneralPDFLibShadingSubFunctions;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib); reintroduce; virtual;
    destructor Destroy; override;
    property SubFunctions: TTMSFNCGeneralPDFLibShadingSubFunctions read FSubFunctions;
    function SubFunctionsRef: String;
  end;

  TTMSFNCGeneralPDFLibShading = class(TTMSFNCGeneralPDFLibObject)
  private
    FFunction: TTMSFNCGeneralPDFLibShadingFunction;
    FRect: TRectF;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib); reintroduce; virtual;
    destructor Destroy; override;
    function RectAsString: string; virtual;
    property &Function: TTMSFNCGeneralPDFLibShadingFunction read FFunction write FFunction;
    property Rect: TRectF read FRect write FRect;
  end;

  TTMSFNCGeneralPDFLibAnnotation = class(TTMSFNCGeneralPDFLibObject)
  private
    FAcroFormField: TTMSFNCGeneralPDFLibAcroFormField;
    FRect: TRectF;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const ARect: TRectF); reintroduce; virtual;
    destructor Destroy; override;
    function RectAsString: String;
    property AcroFormField: TTMSFNCGeneralPDFLibAcroFormField read FAcroFormField write FAcroFormField;
    property Rect: TRectF read FRect write FRect;
  end;

  TTMSFNCGeneralPDFLibURL = class(TTMSFNCGeneralPDFLibAnnotation)
  private
    FURL: UnicodeString;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const ARect: TRectF; const AURL: UnicodeString); reintroduce; virtual;
    property URL: UnicodeString read FURL write FURL;
  end;

  TTMSFNCGeneralPDFLibGoTo = class(TTMSFNCGeneralPDFLibAnnotation)
  private
    FDestination: UnicodeString;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const ARect: TRectF; const ADestination: UnicodeString); reintroduce; virtual;
    property Destination: UnicodeString read FDestination write FDestination;
  end;

  TTMSFNCGeneralPDFLibAcroFormFieldContent = class(TTMSFNCGeneralPDFLibObject);

  TTMSFNCGeneralPDFLibPattern = class(TTMSFNCGeneralPDFLibObject)
  private
    FShading: TTMSFNCGeneralPDFLibShading;
    function GetRefNameBase: string; override;
  public
    destructor Destroy; override;
    property Shading: TTMSFNCGeneralPDFLibShading read FShading write FShading;
  end;

  TTMSFNCGeneralPDFLibLinearGradient = class(TTMSFNCGeneralPDFLibShading)
  private
    FFillOrientation: TTMSFNCGraphicsFillOrientation;
  public
    constructor Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFillOrientation: TTMSFNCGraphicsFillOrientation); reintroduce; virtual;
    property FillOrientation: TTMSFNCGraphicsFillOrientation read FFillOrientation write FFillOrientation;
  end;

  TTMSFNCGeneralPDFLibXRefObject = class
  private
    FXRefOffset: integer;
    FXRefNumber: integer;
    FXRefType: string;
    FXRefObject: TTMSFNCGeneralPDFLibObject;
    FInXRefList: Boolean;
  public
    constructor Create(const AValue: TTMSFNCGeneralPDFLibObject = nil);
    destructor Destroy; override;
    class function FormatValue(AValue: Integer; ALength: Integer): string;
    function GenerateXRefValue: string;
    property XRefOffset: integer read FXRefOffset write FXRefOffset;
    property XRefNumber: integer read FXRefNumber write FXRefNumber;
    property XRefType: string read FXRefType write FXRefType;
    property XRefObject: TTMSFNCGeneralPDFLibObject read FXRefObject write FXRefObject;
    property InXRefList: Boolean read FInXRefList write FInXRefList;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCGeneralPDFLibXRefObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibXRefObject;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibXRefObject);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibXRefObject read GetItem write SetItem; default;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCGeneralPDFLibXRefObjects = class(TObjectList<TTMSFNCGeneralPDFLibXRefObject>)
  public
  {$ENDIF}
    function AddObjectClass(const AObjectClass: TTMSFNCGeneralPDFLibObjectClass = nil): TTMSFNCGeneralPDFLibXRefObject;
    function AddObject(const AObject: TTMSFNCGeneralPDFLibObject = nil): TTMSFNCGeneralPDFLibXRefObject;
    function IndexOfObject(const AObject: TTMSFNCGeneralPDFLibObject; const ACheckClassType: Boolean = False): Integer;
  end;

  TTMSFNCGeneralPDFLib = class(TInterfacedObject, ITMSFNCCustomPDFLib)
  private
    {$IFNDEF LCLWEBLIB}
    FComparePageNumbers: IComparer<TTMSFNCGeneralPDFLibPage>;
    {$ENDIF}
    FOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
    FOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    FOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
    FOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    FOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    FOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
    FOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    FPageHeight, FPageWidth: Single;
    FBlockFontUpdate: Boolean;
    FOSFontList: TStringList;
    FInfo: TTMSFNCGeneralPDFLibInfo;
    FDestOutputProfile: TTMSFNCGeneralPDFLibDestOutputProfile;
    FInitializer: TTMSFNCGeneralPDFLibInitializer;
    FAcroForm: TTMSFNCGeneralPDFLibAcroForm;
    FOutputIntent: TTMSFNCGeneralPDFLibOutputIntent;
    FActivePage: TTMSFNCGeneralPDFLibPage;
    FActiveFont: TTMSFNCGeneralPDFLibFont;
    FActiveShading: TTMSFNCGeneralPDFLibShading;
    XRefAddress: Int64;
    FXRefObjects: TTMSFNCGeneralPDFLibXRefObjects;
    FFontList: TTMSFNCGeneralPDFLibFonts;
    FAcroFormFields: TTMSFNCGeneralPDFLibAcroFormFields;
    FPageCount: Integer;
    FOutput: TTMSFNCPDFGraphicsLibOutputWriter;
    FPDFFileName: string;
    FDocumentStarted, FPageStarted: Boolean;
    FPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    FPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    FPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
    FMediaBox: TRectF;
    FPageOrientation: TTMSFNCPDFLibPageOrientation;
    FAuthor: String;
    FSubject: String;
    FAllowsPrinting: Boolean;
    FTitle: String;
    FCreator: String;
    FUserPassword: String;
    FAllowsCopying: Boolean;
    FKeywords: TStrings;
    FOwnerPassword: String;
    FBleedBox: TRectF;
    FCropBox: TRectF;
    FArtBox: TRectF;
    FTrimBox: TRectF;
    FPageSize: TTMSFNCPDFLibPageSize;
    FPageNumberFormat: UnicodeString;
    FPageNumber: TTMSFNCPDFLibPageNumber;
    FPageNumberSize: Single;
    FPageNumberMargins: TTMSFNCMargins;
    FHeader: UnicodeString;
    FFooter: UnicodeString;
    FHeaderSize: Single;
    FHeaderMargins: TTMSFNCMargins;
    FFooterMargins: TTMSFNCMargins;
    FFooterSize: Single;
    FFooterAlignment: TTMSFNCGraphicsTextAlign;
    FHeaderAlignment: TTMSFNCGraphicsTextAlign;
    FPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    FModificationDate: string;
    FProducer: String;
    FCreationDate: String;
    FEmbedFonts: Boolean;
    {$IFDEF WEBLIB}
    FFullFontEmbedding: Boolean;
    {$ENDIF}
    FFontFallBackList: TStrings;
    FFooterFont: TTMSFNCPDFGraphicsLibFont;
    FHeaderFont: TTMSFNCPDFGraphicsLibFont;
    FPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    FPDFStandard: TTMSFNCPDFLibStandard;
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    procedure SetPageOrientation(
      const Value: TTMSFNCPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
    procedure SetFooterMargins(const Value: TTMSFNCMargins);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetFooter(const Value: UnicodeString);
    procedure SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeader(const Value: UnicodeString);
    procedure SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetHeaderSize(const Value: Single);
    procedure SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetPageNumberSize(const Value: Single);
    procedure SetKeywords(const Value: TStrings);
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetOwnerPassword(const Value: String);
    procedure SetSubject(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetUserPassword(const Value: String);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetEmbedFonts(const Value: Boolean);
    {$IFDEF WEBLIB}
    procedure SetFullFontEmbedding(const Value: Boolean);
    {$ENDIF}
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetOnAfterDrawFooter(const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
    procedure SetOnAfterDrawHeader(const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
    procedure SetOnBeforeDrawHeader(const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
    procedure SetOnBeforeDrawPageNumber(const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnNewPageStarted(const Value: TTMSFNCPDFLibNewPageStartedEvent);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
    function GetPDFStandard: TTMSFNCPDFLibStandard;
    function GetAllowsCopying: Boolean;
    function GetAllowsPrinting: Boolean;
    function GetAuthor: String;
    function GetCreator: String;
    function GetFooter: UnicodeString;
    function GetFooterAlignment: TTMSFNCGraphicsTextAlign;
    function GetFooterMargins: TTMSFNCMargins;
    function GetFooterSize: Single;
    function GetHeader: UnicodeString;
    function GetHeaderAlignment: TTMSFNCGraphicsTextAlign;
    function GetHeaderMargins: TTMSFNCMargins;
    function GetHeaderSize: Single;
    function GetPageNumber: TTMSFNCPDFLibPageNumber;
    function GetPageNumberFormat: UnicodeString;
    function GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    function GetPageNumberMargins: TTMSFNCMargins;
    function GetPageNumberSize: Single;
    function GetKeywords: TStrings;
    function GetFontFallBackList: TStrings;
    function GetOwnerPassword: String;
    function GetPageOrientation: TTMSFNCPDFLibPageOrientation;
    function GetPageSize: TTMSFNCPDFLibPageSize;
    function GetSubject: String;
    function GetTitle: String;
    function GetUserPassword: String;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
    function GetEmbedFonts: Boolean;
    {$IFDEF WEBLIB}
    function GetFullFontEmbedding: Boolean;
    {$ENDIF}
    function GetFooterFont: TTMSFNCPDFGraphicsLibFont;
    function GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
    function GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    function GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    function GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
    function GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    function GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
    function GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
    function GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
  protected
    procedure DoNewPageStarted(APageIndex: Integer);
    procedure DoBeforeDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure FontChanged(Sender: TObject);
    procedure NotifyURL(Sender: TObject; ARect: TRectF; AURL: UnicodeString);
    procedure NotifyGoTo(Sender: TObject; ARect: TRectF; ADestination: UnicodeString);
    procedure NotifyUnicode(Sender: TObject; AValue: UnicodeString);
    procedure NotifyText(Sender: TObject; {%H-}AValue: UnicodeString);
    procedure NotifyShading(Sender: TObject; AFillColor: TTMSFNCGraphicsColor; AFillColorTo: TTMSFNCGraphicsColor; AFillOrientation: TTMSFNCGraphicsFillOrientation; var AShadingReference: string);
    procedure NotifyShadingRect(Sender: TObject; ARect: TRectF);
    procedure NotifyBitmap(Sender: TObject; AValue: TTMSFNCBitmapHelperClass; AImageType: TTMSFNCPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TTMSFNCGraphicsColor; var ABitmapReference: string);
    procedure DestroyFont(AFont: TTMSFNCGeneralPDFLibFont);
    procedure UpdateBoxRect;
    procedure FinishPage;
    procedure CleanUpUnusedFonts;
    procedure UpdateFontList(ASearchForUnicodeFont: Boolean = False);
    procedure WriteFontList;
    procedure InitializeFontList;
    procedure UpdateGlyphIds;
    procedure WriteBitmapList;
    procedure WriteAcroFormFieldList;
    procedure WriteAnnotationList;
    procedure WritePatternList;
    procedure WriteShadingList;
    procedure WriteXRefTable;
    procedure SetPDFGraphicsLib(const Value: ITMSFNCCustomPDFGraphicsLib);
    procedure SetPDFGraphicsExLib(const Value: ITMSFNCCustomPDFGraphicsExLib);
    procedure SetPDFInitializationLib(const Value: ITMSFNCCustomPDFInitializationLib);
    procedure BeginDocument(FileName: String = '');
    procedure OpenDocument({%H-}FileName: String); overload;
    procedure OpenDocument({%H-}FileStream: TMemoryStream); overload;
    procedure SaveDocumentFromStream({%H-}FileStream: TMemoryStream; {%H-}FileName: String);
    procedure GetDocumentInfo;
    procedure GetPageInfo({%H-}PageIndex: Integer);
    procedure CloseDocument;
    procedure NewPage;
    procedure InsertPage(PageIndex: Integer);
    procedure DrawPage({%H-}PageIndex: Integer);
    procedure DrawHeader;
    procedure DrawFooter;
    procedure DrawPageNumber;
    function GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
    function CompareStreams(AStream1, AStream2: TMemoryStream): Boolean;
    function VerifyFontName(AFontName: string): string;
    function SearchForFont(ABaseFontName: string; AUnicodeFont: Boolean): TTMSFNCGeneralPDFLibFont;
    function GetPageReferences: String;
    function GetAcroFormFieldReferences: string;
    function GetKeywordsString: string;
    function GetFontRefs(APage: TTMSFNCGeneralPDFLibPage): String;
    function GetXObjectRefs(APage: TTMSFNCGeneralPDFLibPage): string;
    function GetShadingRef(APage: TTMSFNCGeneralPDFLibPage): string;
    function GetAnnotationsRef(APage: TTMSFNCGeneralPDFLibPage): string;
    function GetPatternRef(APage: TTMSFNCGeneralPDFLibPage): string;
    function GetContentRef(APage: TTMSFNCGeneralPDFLibPage): string;
    function GetAcroFormRef: string;
    function GetDestOutputProfileRef: String;
    function GetPageRef(APage: TTMSFNCGeneralPDFLibPage): string;
    function UnlockWithPassword({%H-}Password: String): Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function IsDocumentOpened: Boolean;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
    property MediaBox: TRectF read FMediaBox write FMediaBox;
    property TrimBox: TRectF read FTrimBox write FTrimBox;
    property ArtBox: TRectF read FArtBox write FArtBox;
    property BleedBox: TRectF read FBleedBox write FBleedBox;
    property CropBox: TRectF read FCropBox write FCropBox;
    property EmbedFonts: Boolean read FEmbedFonts write FEmbedFonts;
    {$IFDEF WEBLIB}
    property FullFontEmbedding: Boolean read FFullFontEmbedding write FFullFontEmbedding;
    {$ENDIF}
    property ModificationDate: string read FModificationDate;
    property Producer: String read FProducer;
    property CreationDate: String read FCreationDate;
    property Initializer: TTMSFNCGeneralPDFLibInitializer read FInitializer;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageSize: TTMSFNCPDFLibPageSize read FPageSize write FPageSize default psLetter;
    property Orientation: TTMSFNCPDFLibPageOrientation read FPageOrientation write FPageOrientation default poPortrait;
    property Author: String read FAuthor write FAuthor;
    property Creator: String read FCreator write FCreator;
    property Header: UnicodeString read FHeader write FHeader;
    property HeaderSize: Single read FHeaderSize write FHeaderSize;
    property HeaderMargins: TTMSFNCMargins read FHeaderMargins write FHeaderMargins;
    property HeaderAlignment: TTMSFNCGraphicsTextAlign read FHeaderAlignment write FHeaderAlignment default gtaCenter;
    property HeaderFont: TTMSFNCPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property PageNumberFormat: UnicodeString read FPageNumberFormat write FPageNumberFormat;
    property PageNumber: TTMSFNCPDFLibPageNumber read FPageNumber write FPageNumber;
    property PageNumberSize: Single read FPageNumberSize write FPageNumberSize;
    property PageNumberMargins: TTMSFNCMargins read FPageNumberMargins write FPageNumberMargins;
    property PageNumberAlignment: TTMSFNCGraphicsTextAlign read FPageNumberAlignment write FPageNumberAlignment default gtaTrailing;
    property PageNumberFont: TTMSFNCPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property PDFStandard: TTMSFNCPDFLibStandard read FPDFStandard write FPDFStandard default pdfNone;
    property Footer: UnicodeString read FFooter write FFooter;
    property FooterSize: Single read FFooterSize write FFooterSize;
    property FooterMargins: TTMSFNCMargins read FFooterMargins write FFooterMargins;
    property FooterAlignment: TTMSFNCGraphicsTextAlign read FFooterAlignment write FFooterAlignment default gtaCenter;
    property FooterFont: TTMSFNCPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read FTitle write FTitle;
    property OwnerPassword: String read FOwnerPassword write FOwnerPassword;
    property UserPassword: String read FUserPassword write FUserPassword;
    property AllowsPrinting: Boolean read FAllowsPrinting write FAllowsPrinting default True;
    property AllowsCopying: Boolean read FAllowsCopying write FAllowsCopying default True;
    property Subject: String read FSubject write FSubject;
    property Keywords: TStrings read FKeywords write FKeywords;
    property FontFallBackList: TStrings read FFontFallBackList write FFontFallBackList;
    property OnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PDFLibService: ITMSFNCPDFLibGeneralService;

procedure RegisterPDFLibGeneralService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFLibGeneralService, IInterface(PDFLibService)) then
  begin
    PDFLibService := TTMSFNCGeneralPDFLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFLibGeneralService, PDFLibService);
  end;
end;

procedure UnregisterPDFLibGeneralService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFLibGeneralService);
end;

{ TTMSFNCGeneralPDFLibService }

function TTMSFNCGeneralPDFLibService.DoCreatePDFLib: ITMSFNCCustomPDFLib;
begin
  Result := TTMSFNCGeneralPDFLib.Create;
end;

procedure TTMSFNCGeneralPDFLib.BeginDocument(FileName: String = '');
var
  r: TTMSFNCGeneralPDFLibXRefObject;
  d: TDateTime;
  dt, dtiso: string;
  ms: TStringStream;
  s: String;
begin
  FPDFFileName := FileName;
  FDocumentStarted := True;

  r := FXRefObjects.AddObject;
  r.XRefNumber := PDFMaxXref;
  FOutput.StartNewStream;

  case PDFStandard of
    pdfNone: FOutput.WriteString(PDFStart + PDFLB);
    pdfA1: FOutput.WriteString(PDFStart14 + PDFLB);
  end;

  r := FXRefObjects.AddObjectClass(TTMSFNCGeneralPDFLibCatalog);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Catalog');
  FOutput.WriteString('/Pages 2 0 R');
  case PDFStandard of
    pdfA1:
    begin
      FOutput.WriteString('/OutputIntents[3 0 R]');
      FOutput.WriteString('/MarkInfo <</Marked true>>/Metadata 5 0 R/StructTreeRoot<<>>');
    end;
  end;

//  FOutput.WriteString('/AcroForm ' + PDFPageAcroFormRef + ' 0 R');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  r := FXRefObjects.AddObjectClass(TTMSFNCGeneralPDFLibPages);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Pages');
  FOutput.WriteString('/Kids['+PDFPageChildRef+']');
  FOutput.WriteString('/Count ' + PDFPageCountRef);
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  d := Now;
  dt := ConvertToPDFDate(d);
  dtiso := DateTimeToIso(d);

  case PDFStandard of
    pdfA1:
    begin
      FOutputIntent := TTMSFNCGeneralPDFLibOutputIntent.Create(Self);
      r := FXRefObjects.AddObject(FOutputIntent);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');
      FOutput.WriteString('/Type/OutputIntent');
      FOutput.WriteString('/OutputCondition()');
      FOutput.WriteString('/OutputConditionIdentifier(sRGB)');
      FOutput.WriteString('/RegistryName(http://www.color.org)');
      FOutput.WriteString('/Info(sRGB IEC61966-2.1)');
      FOutput.WriteString('/S/GTS_PDFA1');
      FOutput.WriteString('/DestOutputProfile ' + PDFDestOutputProfileRef + ' 0 R');
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);

      FDestOutputProfile := TTMSFNCGeneralPDFLibDestOutputProfile.Create(Self);
      r := FXRefObjects.AddObject(FDestOutputProfile);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');
      FOutput.WriteString(PDFDestOutputProfileLengthRef);
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('stream' + PDFLB);
      FOutput.StartContentStream(r);

      {$IFDEF WEBLIB}
      raise Exception.Create('Write ICC Array');
      {$ELSE}
      FOutput.Write(@ICC, SizeOf(ICC));
      {$ENDIF}

      ms := FOutput.FinishContentStream(PDFDestOutputProfileLengthRef, True, '/N 3');
      if Assigned(ms) then
        FOutput.Streams.Add(ms);

      FOutput.StartNewStream;
      FOutput.WriteString(PDFLB);
      FOutput.WriteString('endstream' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);

      FOutputIntent := TTMSFNCGeneralPDFLibOutputIntent.Create(Self);
      r := FXRefObjects.AddObject(FOutputIntent);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');

      s := Format(PDFMetaData, [Producer, dtiso, dtiso, Creator, Title, Author, Subject, Keywords.Text, Producer]);

      FOutput.WriteString('/Length '+IntToStr(Length(s))+'/Subtype/XML/Type/Metadata');
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('stream' + PDFLB);
      FOutput.WriteString(s + PDFLB);
      FOutput.WriteString('endstream' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);
    end;
  end;

  FInfo := TTMSFNCGeneralPDFLibInfo.Create(Self);
  r := FXRefObjects.AddObject(FInfo);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Producer <>');
  FOutput.WriteString('/Author ('+Author+')');
  FOutput.WriteString('/CreationDate (D:'+dt+')');
  FOutput.WriteString('/Creator ('+Creator+')');
  FOutput.WriteString('/Keywords ('+GetKeywordsString+')');
  FOutput.WriteString('/Subject ('+Subject+')');
  FOutput.WriteString('/Title ('+Title+')');
  FOutput.WriteString('/ModDate (D:'+dt+')');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);
end;

procedure TTMSFNCGeneralPDFLib.NewPage;
var
  ab, mb, bb, cb, tb: TRectF;
  r: TTMSFNCGeneralPDFLibXRefObject;
  cnt: TTMSFNCGeneralPDFLibContent;
begin
  mb := MediaBox;
  ab := ArtBox;
  bb := BleedBox;
  cb := CropBox;
  tb := TrimBox;

  {$IFDEF WEBLIB}
  InitializeFontList;
  UpdateGlyphIds;
  {$ENDIF}
  FinishPage;
  FActivePage := TTMSFNCGeneralPDFLibPage.Create(Self, FPageCount);
  r := FXRefObjects.AddObject(FActivePage);
  FOutput.StartNewStream(r);
  FOutput.WriteString(PDFPageRef + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Page');
  FOutput.WriteString('/Parent 2 0 R');
  FOutput.WriteString('/MediaBox[' + IntToStr(Round(mb.Left))+ ' '+IntToStr(Round(mb.Top)) + ' ' + IntToStr(Round(mb.Right)) + ' ' + IntToStr(Round(mb.Bottom)) + ']');
  FOutput.WriteString('/ArtBox[' + IntToStr(Round(ab.Left))+ ' '+IntToStr(Round(ab.Top)) + ' ' + IntToStr(Round(ab.Right)) + ' ' + IntToStr(Round(ab.Bottom)) + ']');
  FOutput.WriteString('/BleedBox[' + IntToStr(Round(bb.Left))+ ' '+IntToStr(Round(bb.Top)) + ' ' + IntToStr(Round(bb.Right)) + ' ' + IntToStr(Round(bb.Bottom)) + ']');
  FOutput.WriteString('/CropBox[' + IntToStr(Round(cb.Left))+ ' '+IntToStr(Round(cb.Top)) + ' ' + IntToStr(Round(cb.Right)) + ' ' + IntToStr(Round(cb.Bottom)) + ']');
  FOutput.WriteString('/Trimbox[' + IntToStr(Round(tb.Left))+ ' '+IntToStr(Round(tb.Top)) + ' ' + IntToStr(Round(tb.Right)) + ' ' + IntToStr(Round(tb.Bottom)) + ']');
  FOutput.WriteString('/Contents ' + PDFPageContentRef +' 0 R');
  FOutput.WriteString('/ProcSet[/PDF/Text/ImageC]');
  FOutput.WriteString('/Annots['+PDFPageAnnotationsRef+']');
  FOutput.WriteString('/Resources');
  FOutput.WriteString('<<');
  FOutput.WriteString('/XObject');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageXObjectRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('/Font');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageFontRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('/Pattern');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPagePatternRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('>>');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  UpdateFontList;

//  FAcroForm := TTMSFNCGeneralPDFLibAcroForm.Create(Self);
//  r := FXRefObjects.AddObject(FAcroForm);
//  FOutput.StartNewStream(r);
//  FOutput.WriteString(PDFPageAcroFormRef + ' 0 obj' + PDFLB);
//  FOutput.WriteString('<<');
//  FOutput.WriteString('/Fields['+PDFPageAcroFormFieldsRef+']');
//  FOutput.WriteString('>>' + PDFLB);
//  FOutput.WriteString('endobj' + PDFLB);

  cnt := TTMSFNCGeneralPDFLibContent.Create(Self);
  cnt.Page := FActivePage;
  r := FXRefObjects.AddObject(cnt);
  FActivePage.Content := cnt;
  FOutput.StartNewStream(r);
  FOutput.WriteString(PDFPageContentRef + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageContentLengthRef);
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('stream' + PDFLB);
  FOutput.StartContentStream(r);

  if Assigned(FPDFInitializationLib) then
  begin
    FPDFInitializationLib.SetCanvas(FOutput);
    FPDFInitializationLib.SetPageWidth(mb.Right - mb.Left);
    FPDFInitializationLib.SetPageHeight(mb.Bottom - mb.Top);
    FPDFInitializationLib.InitializeAppearance;
  end;

  DoNewPageStarted(GetPageIndex);

  DrawHeader;
  DrawFooter;
  DrawPageNumber;

  Inc(FPageCount);
  FPageStarted := True;
end;

procedure TTMSFNCGeneralPDFLib.NotifyBitmap(Sender: TObject; AValue: TTMSFNCBitmapHelperClass; AImageType: TTMSFNCPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TTMSFNCGraphicsColor; var ABitmapReference: string);
var
  bmp, bmpc: TTMSFNCGeneralPDFLibBitmap;
  ms: TMemoryStream;
  {$IFDEF WEBLIB}
  v: JSValue;
  s: string;
  {$ENDIF}
  x: TTMSFNCGeneralPDFLibXRefObject;

  {$IFDEF WEBLIB}
  function GetBase64Image(AImage: TJSHTMLElement; AQL: Single; AC: TTMSFNCGraphicsColor): string;
  var
    s: string;
    m: TJSHTMLElement;
    qt: Single;
    c: string;
  begin
    s := '';
    m := AImage;
    qt := AQL;
    c := TTMSFNCGraphics.ColorToHTML(AC);

    asm
      function getBase64Image(img, q, ct) {
        var canvas = document.createElement("canvas");
        canvas.width = img.width;
        canvas.height = img.height;
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = ct;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0);
        var dataURL = canvas.toDataURL("image/jpeg", q);
        return dataURL.replace(/^data:image\/(png|jpg|jpeg);base64,/, "");
      }
      s = getBase64Image(m, qt, c);
    end;
    Result := s;
  end;
  {$ENDIF}

begin
  if Assigned(FActivePage) and Assigned(AValue) then
  begin
    ms := TMemoryStream.Create;
    try
      {$IFDEF WEBLIB}
      s := GetBase64Image(TJSHTMLElement(AValue.Image), AQuality, ABackgroundColor);
      asm
        var binary_string = window.atob(s);
        var len = binary_string.length;
        var bytes = new Uint8Array( len );
        for (var i = 0; i < len; i++)        {
            bytes[i] = binary_string.charCodeAt(i);
        }
        ms.Write(bytes, bytes.length);
      end;
      {$ELSE}
      AValue.SaveToStream(ms);
      {$ENDIF}
      bmp := nil;
      {$IFDEF WEBLIB}
      for v in FXRefObjects do
      begin
        x := TTMSFNCGeneralPDFLibXRefObject(v);
      {$ELSE}
      for x in FXRefObjects do
      begin
      {$ENDIF}
        if Assigned(x.XRefObject) and (x.XRefObject is TTMSFNCGeneralPDFLibBitmap) then
        begin
          bmpc := x.XRefObject as TTMSFNCGeneralPDFLibBitmap;
          if Assigned(bmpc.Stream) and (AQuality = bmpc.Quality) and (ABackgroundColor = bmpc.BackgroundColor) and CompareStreams(bmpc.Stream, ms) then
          begin
            bmp := bmpc;
            Break;
          end;
        end;
      end;

      if not Assigned(bmp) then
      begin
        bmp := TTMSFNCGeneralPDFLibBitmap.Create(Self, AValue, ms, AImageType, AQuality, ABackgroundColor);
        FXRefObjects.AddObject(bmp);
      end;

      if FActivePage.Bitmaps.IndexOf(bmp) < 0 then
        FActivePage.Bitmaps.Add(bmp);

      if FActivePage.XObjectList.IndexOf(bmp) < 0 then
        FActivePage.XObjectList.Add(bmp);

      ABitmapReference := Format(PDFPageBitmapObjectRef, [bmp.RefNameNotEscaped]);
      bmp.RefNameOriginal := ABitmapReference;
    finally
      ms.Free;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.NotifyShading(Sender: TObject; AFillColor: TTMSFNCGraphicsColor; AFillColorTo: TTMSFNCGraphicsColor; AFillOrientation: TTMSFNCGraphicsFillOrientation; var AShadingReference: string);
var
  lg: TTMSFNCGeneralPDFLibLinearGradient;
  f: TTMSFNCGeneralPDFLibShadingFunction;
  sf: TTMSFNCGeneralPDFLibShadingSubFunction;
  p: TTMSFNCGeneralPDFLibPattern;
begin
  if Assigned(FActivePage) then
  begin
    lg := TTMSFNCGeneralPDFLibLinearGradient.Create(Self, AFillOrientation);
    FXRefObjects.AddObject(lg);
    f := TTMSFNCGeneralPDFLibShadingFunction.Create(Self);
    FXRefObjects.AddObject(f);
    lg.&Function := f;

    sf := TTMSFNCGeneralPDFLibShadingSubFunction.Create(Self, AFillColor, AFillColorTo);
    FXRefObjects.AddObject(sf);
    f.SubFunctions.Add(sf);

    sf := TTMSFNCGeneralPDFLibShadingSubFunction.Create(Self, AFillColor, AFillColorTo);
    FXRefObjects.AddObject(sf);
    f.SubFunctions.Add(sf);

    FActivePage.Shadings.Add(lg);

    p := TTMSFNCGeneralPDFLibPattern.Create(Self);
    p.Shading := lg;
    FXRefObjects.AddObject(p);
    FActivePage.Patterns.Add(p);

    AShadingReference := Format(PDFPageShadingObjectRef, [p.RefNameNotEscaped]);
    p.RefNameOriginal := AShadingReference;

    FActiveShading := lg;
  end;
end;

procedure TTMSFNCGeneralPDFLib.NotifyShadingRect(Sender: TObject;
  ARect: TRectF);
begin
  if Assigned(FActiveShading) then
    FActiveShading.Rect := ARect;
end;

procedure TTMSFNCGeneralPDFLib.NotifyText(Sender: TObject;
  AValue: UnicodeString);
begin
  UpdateFontList(False);
end;

procedure TTMSFNCGeneralPDFLib.NotifyUnicode(Sender: TObject; AValue: UnicodeString);
var
  I, v: Integer;
  vu: Boolean;
  fnff, fns, fn: string;
  cr: Boolean;
begin
  if Assigned(FOutput) and Assigned(FActiveFont) then
  begin
    fnff := FActiveFont.Base;
    for fnff in FontFallBackList do
    begin
      cr := (FActiveFont.UsedCharArray.Count = 0);
      vu := True;
      {$IFDEF ZEROSTRINGINDEX}
      for I := 0 to Length(AValue) - 1 do
      {$ENDIF}
      {$IFNDEF ZEROSTRINGINDEX}
      for I := 1 to Length(AValue) do
      {$ENDIF}
      begin
        v := Ord(AValue[I]);
        if (v >= 32) and (FActiveFont.CharArray.IndexOf(v) < 0) then
        begin
          vu := False;
          Break;
        end;
      end;

      if not vu then
      begin
        if cr then
        begin
          FActiveFont.Inactive := True;
          FActiveFont := nil;
        end;

        fns := VerifyFontName(fnff);
        FBlockFontUpdate := True;
        FPDFGraphicsLib.Font.Name := fns;
        FBlockFontUpdate := False;
        UpdateFontList(True);
      end
      else
        Break;
    end;

    if Assigned(FActiveFont) and not FActiveFont.Unicode then
    begin
      fn := FActiveFont.Base;
      cr := (FActiveFont.UsedCharArray.Count = 0);
      if cr then
      begin
        FActiveFont.Inactive := True;
        FActiveFont := nil;
      end;

      FBlockFontUpdate := True;
      FPDFGraphicsLib.Font.Name := VerifyFontName(fn);
      FBlockFontUpdate := False;
      UpdateFontList(True);
      FActiveFont.Unicode := True;
      UpdateFontList(True);
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.NotifyURL(Sender: TObject; ARect: TRectF; AURL: UnicodeString);
var
  u: TTMSFNCGeneralPDFLibURL;
begin
  if Assigned(FActivePage) then
  begin
    u := TTMSFNCGeneralPDFLibURL.Create(Self, ARect, AURL);
    FXRefObjects.AddObject(u);
    FActivePage.Annotations.Add(u);
  end;
end;

procedure TTMSFNCGeneralPDFLib.NotifyGoTo(Sender: TObject; ARect: TRectF; ADestination: UnicodeString);
var
  u: TTMSFNCGeneralPDFLibGoTo;
begin
  if Assigned(FActivePage) then
  begin
    u := TTMSFNCGeneralPDFLibGoTo.Create(Self, ARect, ADestination);
    FXRefObjects.AddObject(u);
    FActivePage.Annotations.Add(u);
  end;
end;

procedure TTMSFNCGeneralPDFLib.OpenDocument(FileStream: TMemoryStream);
begin
  if IsDocumentOpened then
    CloseDocument;
end;

procedure TTMSFNCGeneralPDFLib.OpenDocument(FileName: String);
begin
  if IsDocumentOpened then
    CloseDocument;
end;

procedure TTMSFNCGeneralPDFLib.CleanUpUnusedFonts;
var
  I: Integer;
  ft: TTMSFNCGeneralPDFLibFont;
  cr: Boolean;
begin
  for I := FFontList.Count - 1 downto 0 do
  begin
    ft := FFontList[I];
    cr := (ft.UsedCharArray.Count = 0);
    if cr then
      DestroyFont(ft);
  end;
end;

procedure TTMSFNCGeneralPDFLib.CloseDocument;
begin
end;

function TTMSFNCGeneralPDFLib.CompareStreams(AStream1, AStream2: TMemoryStream): Boolean;
const
  Block_Size = 4096;
var
  {$IFDEF WEBLIB}
  Buffer_1: TBytes;
  Buffer_2: TBytes;
  {$ELSE}
  Buffer_1: array[0..Block_Size-1] of byte;
  Buffer_2: array[0..Block_Size-1] of byte;
  {$ENDIF}
  Buffer_Length: integer;
begin
  Result := False;

  AStream1.Position := 0;
  AStream2.Position := 0;

  if AStream1.Size <> AStream2.Size then
    Exit;

  while AStream1.Position < AStream1.Size do
  begin
    Buffer_Length := AStream1.Read({%H-}Buffer_1, Block_Size);
    AStream2.Read({%H-}Buffer_2, Block_Size);

    {$IFDEF WEBLIB}
    if not (Buffer_1 = Buffer_2) then
      Exit;
    {$ELSE}
    if not CompareMem(@Buffer_1, @Buffer_2, Buffer_Length) then
      Exit;
    {$ENDIF}
  end;

  Result := True;
end;

constructor TTMSFNCGeneralPDFLib.Create;
var
  r: TRectF;
begin
  inherited;
  {$IFNDEF LCLWEBLIB}
  FComparePageNumbers := TDelegatedComparer<TTMSFNCGeneralPDFLibPage>.Create(
    function(const Item1, Item2: TTMSFNCGeneralPDFLibPage): Integer
    begin
      Result := CompareValue(Item1.InsertPageNumber, Item2.InsertPageNumber);
    end
    );
  {$ENDIF}

  FPageHeight := 0;
  FPageWidth := 0;
  FFontFallBackList := TStringList.Create;
  FOSFontList := TStringList.Create;
  FOSFontList.CaseSensitive := False;
  TTMSFNCUtils.GetFonts(FOSFontList);
  FEmbedFonts := True;
  {$IFDEF WEBLIB}
  FFullFontEmbedding := False;
  {$ENDIF}
  FInitializer := TTMSFNCGeneralPDFLibInitializer.Create;
  FInitializer.InitializeFontFallBackList(FFontFallBackList);
  FPageCount := 0;
  FOutput := TTMSFNCPDFGraphicsLibOutputWriter.Create;
  FOutput.OnFontChanged := FontChanged;
  FOutput.OnNotifyURL := NotifyURL;
  FOutput.OnNotifyGoTo := NotifyGoTo;
  FOutput.OnNotifyText := NotifyText;
  FOutput.OnNotifyUnicode := NotifyUnicode;
  FOutput.OnNotifyBitmap := NotifyBitmap;
  FOutput.OnNotifyShading := NotifyShading;
  FOutput.OnNotifyShadingRect := NotifyShadingRect;
  FXRefObjects := TTMSFNCGeneralPDFLibXRefObjects.Create;
  FFontList := TTMSFNCGeneralPDFLibFonts.Create;
  FAcroFormFields := TTMSFNCGeneralPDFLibAcroFormFields.Create;
  FMediaBox := DefaultMediaBox;
  FCropBox := DefaultMediaBox;
  FTrimBox := DefaultMediaBox;
  FArtBox := DefaultMediaBox;
  FBleedBox := DefaultMediaBox;
  FPageOrientation := poPortrait;
  FPageSize := psLetter;
  FKeywords := TStringList.Create;
  FAllowsPrinting := True;
  FAllowsCopying := True;
  FPDFStandard := pdfNone;

  FHeader := 'Header';
  FHeaderSize := 30;
  r := RectF(5, 5, 5, 5);
  FHeaderMargins := TTMSFNCMargins.Create(r);
  FHeaderAlignment := gtaCenter;
  FHeaderFont := TTMSFNCPDFGraphicsLibFont.Create;

  FFooter := 'Footer';
  FFooterSize := 30;
  r := RectF(5, 5, 5, 5);
  FFooterMargins := TTMSFNCMargins.Create(r);
  FFooterAlignment := gtaCenter;
  FFooterFont := TTMSFNCPDFGraphicsLibFont.Create;

  FPageNumber := pnNone;
  FPageNumberFormat := '%d';
  FPageNumberSize := 30;
  r := RectF(5, 5, 5, 5);
  FPageNumberMargins := TTMSFNCMargins.Create(r);
  FPageNumberAlignment := gtaTrailing;
  FPageNumberFont := TTMSFNCPDFGraphicsLibFont.Create;
end;

destructor TTMSFNCGeneralPDFLib.Destroy;
begin
  FAcroForm := nil;
  FOutputIntent := nil;

  if Assigned(FFontFallBackList) then
  begin
    FFontFallBackList.Free;
    FFontFallBackList := nil;
  end;

  if Assigned(FOSFontList) then
  begin
    FOSFontList.Free;
    FOSFontList := nil;
  end;

  if Assigned(FInitializer) then
  begin
    FInitializer.Free;
    FInitializer := nil;
  end;

  if Assigned(FFontList) then
  begin
    FFontList.Free;
    FFontList := nil;
  end;

  if Assigned(FAcroFormFields) then
  begin
    FAcroFormFields.Free;
    FAcroFormFields := nil;
  end;

  if Assigned(FXRefObjects) then
  begin
    FXRefObjects.Free;
    FXRefObjects := nil;
  end;

  if Assigned(FOutput) then
  begin
    FOutput.Free;
    FXRefObjects := nil;
  end;

  if Assigned(FFooterMargins) then
  begin
    FFooterMargins.Free;
    FFooterMargins := nil;
  end;

  if Assigned(FHeaderMargins) then
  begin
    FHeaderMargins.Free;
    FHeaderMargins := nil;
  end;

  if Assigned(FPageNumberMargins) then
  begin
    FPageNumberMargins.Free;
    FPageNumberMargins := nil;
  end;

  if Assigned(FKeywords) then
  begin
    FKeywords.Free;
    FKeywords := nil;
  end;

  if Assigned(FHeaderFont) then
  begin
    FHeaderFont.Free;
    FHeaderFont := nil;
  end;

  if Assigned(FPageNumberFont) then
  begin
    FPageNumberFont.Free;
    FPageNumberFont := nil;
  end;

  if Assigned(FFooterFont) then
  begin
    FFooterFont.Free;
    FFooterFont := nil;
  end;

  inherited;
end;

procedure TTMSFNCGeneralPDFLib.DestroyFont(AFont: TTMSFNCGeneralPDFLibFont);
var
  I: Integer;
  ftd: TTMSFNCGeneralPDFLibFontDescriptor;
  ftds: TTMSFNCGeneralPDFLibFontDescendant;
  ftff: TTMSFNCGeneralPDFLibFontFile;
  ftu: TTMSFNCGeneralPDFLibFontUnicode;
  x: TTMSFNCGeneralPDFLibXRefObject;
begin
  if Assigned(AFont) then
  begin
    FFontList.Remove(AFont);
    ftd := AFont.FontDescriptor;
    ftds := AFont.FontDescendant;
    ftff := AFont.FontFile;
    ftu := AFont.FontUnicode;

    for I := FXRefObjects.Count - 1 downto 0 do
    begin
      x := FXRefObjects[I];
      if Assigned(x.XRefObject) and (x.XRefObject = ftd) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftds) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftff) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftu) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = AFont) then
        FXRefObjects.Delete(I)
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TTMSFNCGeneralPDFLib.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TTMSFNCGeneralPDFLib.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TTMSFNCGeneralPDFLib.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCGeneralPDFLib.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCGeneralPDFLib.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCGeneralPDFLib.DoNewPageStarted(APageIndex: Integer);
begin
  if Assigned(OnNewPageStarted) then
    OnNewPageStarted(Self, APageIndex);
end;

procedure TTMSFNCGeneralPDFLib.DrawFooter;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r, rc: TRectF;
  df: Boolean;
begin
  if not Assigned(FPDFGraphicsLib) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TTMSFNCPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := FooterAlignment;
  FPDFGraphicsLib.Font.Assign(FooterFont);
  r := GetFooterRect;

  df := True;
  DoBeforeDrawFooter(Self, GetPageIndex, Footer, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TTMSFNCUtils.IsHTMLUnicode(Footer) then
    begin
      rc := FPDFGraphicsLib.DrawHTMLText(Footer, r, False, 1, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawHTMLText(Footer, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + (r.Right - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy));
    end
    else
    begin
      rc := FPDFGraphicsLib.DrawText(Footer, r, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawText(Footer, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + (r.Right - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy
      ));
    end;

    DoAfterDrawFooter(Self, GetPageIndex, Footer, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCGeneralPDFLib.DrawHeader;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r, rc: TRectF;
  df: Boolean;
begin
  if not Assigned(FPDFGraphicsLib) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TTMSFNCPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := HeaderAlignment;
  FPDFGraphicsLib.Font.Assign(HeaderFont);
  r := GetHeaderRect;

  df := True;
  DoBeforeDrawHeader(Self, GetPageIndex, Header, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TTMSFNCUtils.IsHTMLUnicode(Header) then
    begin
      rc := FPDFGraphicsLib.DrawHTMLText(Header, r, False, 1, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawHTMLText(Header, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + (r.Right - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy));
    end
    else
    begin
      rc := FPDFGraphicsLib.DrawText(Header, r, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawText(Header, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + (r.Right - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy));
    end;

    DoAfterDrawHeader(Self, GetPageIndex, Header, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCGeneralPDFLib.DrawPageNumber;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r, rc: TRectF;
  df: Boolean;
  s: UnicodeString;
begin
  if not Assigned(FPDFGraphicsLib) or (PageNumber = pnNone) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TTMSFNCPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := PageNumberAlignment;
  FPDFGraphicsLib.Font.Assign(PageNumberFont);
  r := GetPageNumberRect;

  df := True;
  {$IFDEF LCLLIB}
  s := UTF8Decode(Format(UTF8Encode(PageNumberFormat), [GetPageIndex + 1]));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  s := Format(PageNumberFormat, [GetPageIndex + 1]);
  {$ENDIF}
  DoBeforeDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TTMSFNCUtils.IsHTMLUnicode(s) then
    begin
      rc := FPDFGraphicsLib.DrawHTMLText(s, r, False, 1, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawHTMLText(s, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + (r.Right - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy));
    end
    else
    begin
      rc := FPDFGraphicsLib.DrawText(s, r, True);
      sz.cx := rc.Right - rc.Left;
      sz.cy := rc.Bottom - rc.Top;
      FPDFGraphicsLib.DrawText(s, RectF(R.Left, R.Top + (r.Bottom - r.Top - sz.cy) / 2, r.Left + ((r.Right - r.Left) - r.Left), (R.Top + (r.Bottom - r.Top - sz.cy) / 2) + sz.cy));
    end;

    DoAfterDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCGeneralPDFLib.DrawPage(PageIndex: Integer);
begin
end;

function TTMSFNCGeneralPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
var
  ms: TMemoryStream;
  I: Integer;
  st: TStream;
  sts: TTMSFNCPDFGraphicsLibOutputWriterStream;
  x: TTMSFNCGeneralPDFLibXRefObject;
  pg: TTMSFNCGeneralPDFLibPage;
  acf: TTMSFNCGeneralPDFLibAcroFormField;
  fid: array[0..3] of UInt32;
  IDs: UnicodeString;
  P: PChar;
begin
  Result := nil;
  if FDocumentStarted then
  begin
    InitializeFontList;
    UpdateGlyphIds;
    FinishPage;
    WriteFontList;
    WriteBitmapList;
    WriteAcroFormFieldList;
    WriteAnnotationList;
    WriteShadingList;
    WritePatternList;

    for I := 0 to FOutput.Streams.Count - 1 do
    begin
      st := TStream(FOutput.Streams[I]);
      st.Position := 0;
      if st is TTMSFNCPDFGraphicsLibOutputWriterStream then
      begin
        sts := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
        if Assigned(sts.Reference) and (sts.Reference is TTMSFNCGeneralPDFLibXRefObject) then
        begin
          x := sts.Reference as TTMSFNCGeneralPDFLibXRefObject;
          if x.XRefObject is TTMSFNCGeneralPDFLibPages then
          begin
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageChildRef, PDFPageCountRef], [GetPageReferences, IntToStr(FPageCount)]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibOutputIntent then
          begin
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFDestOutputProfileRef], [GetDestOutputProfileRef]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibCatalog then
          begin
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageAcroFormRef], [GetAcroFormRef]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibPage then
          begin
            pg := x.XRefObject as TTMSFNCGeneralPDFLibPage;
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageFontRef, PDFPageXObjectRef, PDFPageContentRef, PDFPagePatternRef, PDFPageRef, PDFPageAnnotationsRef],
              [GetFontRefs(pg), GetXObjectRefs(pg), GetContentRef(pg), GetPatternRef(pg), GetPageRef(pg), GetAnnotationsRef(pg)]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibContent then
          begin
            pg := (x.XRefObject as TTMSFNCGeneralPDFLibContent).Page;
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageContentRef], [GetContentRef(pg)]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibAcroForm then
          begin
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageAcroFormRef, PDFPageAcroFormFieldsRef], [GetAcroFormRef, GetAcroFormFieldReferences]);
          end
          else if x.XRefObject is TTMSFNCGeneralPDFLibAcroFormField then
          begin
            acf := (x.XRefObject as TTMSFNCGeneralPDFLibAcroFormField);
            FOutput.Stream := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
            if Assigned(acf.Content) then
              FOutput.ReplaceStrings([PDFPageAcroFormFieldContentRef], [acf.Content.Ref]);
          end;
        end;
      end;
    end;

    FOutput.Stream := CreateStringStream;
    for I := 0 to FOutput.Streams.Count - 1 do
    begin
      st := TStream(FOutput.Streams[I]);
      st.Position := 0;
      if st is TTMSFNCPDFGraphicsLibOutputWriterStream then
      begin
        sts := st as TTMSFNCPDFGraphicsLibOutputWriterStream;
        if Assigned(sts.Reference) and (sts.Reference is TTMSFNCGeneralPDFLibXRefObject) then
        begin
          x := sts.Reference as TTMSFNCGeneralPDFLibXRefObject;
          x.XRefOffset := FOutput.Stream.Position;
        end;
      end;

      FOutput.Stream.CopyFrom(st, st.Size);
    end;

    FOutput.Streams.Add(FOutput.Stream);
    FOutput.StreamPosition := FOutput.Stream.Size;
    XRefAddress := FOutput.StreamPosition;
    WriteXRefTable;
    FOutput.WriteString('trailer' + PDFLB);
    FOutput.WriteString('<<');
    FOutput.WriteString('/Root 1 0 R');
    FOutput.WriteString('/Info ' + IntToStr(FXRefObjects.IndexOf(FInfo.XRefObject)) + ' 0 R');
    FOutput.WriteString('/Size ' + IntToStr(FXRefObjects.Count));
    case PDFStandard of
      pdfA1:
      begin
        {$IFDEF WEBLIB}
        raise Exception.Create('PDF Standard A1');
        {$ELSE}
        Randomize;
        for i := 0 to high(fid) do
          fid[i] := UInt32(Random(MaxInt));

        Inc(fid[0], GetTickCountX);
        SetLength(IDs,34);
        P := pointer(IDs);
        P[0] := '<';
        {$IFNDEF FMXMOBILE}
        {$IFNDEF LINUX}
        BinToHex(@fid[0], p + 1, 16);
        {$ENDIF}
        {$ENDIF}
        P[33] := '>';
        FOutput.WriteString('/ID[' + p + ' ' + p +']');
        {$ENDIF}
      end;
    end;
    FOutput.WriteString('>>'+ PDFLB);
    FOutput.WriteString('startxref' + PDFLB);
    FOutput.WriteString(IntToStr(XRefAddress) + PDFLB);
    FOutput.WriteString(PDFEnd + PDFLB);

    FOutput.StreamPosition := 0;
    ms := TMemoryStream.Create;
    ms.CopyFrom(FOutput.Stream, FOutput.Stream.Size);
    if FPDFFileName <> '' then
    begin
      try
        {$IFDEF WEBLIB}
        if AOpenInPDFReader then
          ms.OpenFile('application/pdf');
        {$ELSE}
        ms.SaveToFile(FPDFFileName);
        if AOpenInPDFReader then
          TTMSFNCUtils.OpenFile(FPDFFileName);
        {$ENDIF}
      finally
        ms.Free;
      end;
    end
    else
      Result := ms;

    FFontList.Clear;
    FActivePage := nil;
    FXRefObjects.Clear;
    FOutput.ClearProperties;
    FOutput.Streams.Clear;
    FDocumentStarted := False;
    FPageCount := 0;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteFontList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  ft: TTMSFNCGeneralPDFLibFont;
  ftd: TTMSFNCGeneralPDFLibFontDescriptor;
  ftds: TTMSFNCGeneralPDFLibFontDescendant;
  ftff: TTMSFNCGeneralPDFLibFontFile;
  ftu: TTMSFNCGeneralPDFLibFontUnicode;
  os: integer;
  s: String;
  ms: TStringStream;
  ls: Integer;
  I: Integer;
  idx: Integer;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibFont then
      begin
        ft := x.XRefObject as TTMSFNCGeneralPDFLibFont;
        if not ft.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/Font');
          FOutput.WriteString('/BaseFont/' + ft.Name);
          if ft.Unicode then
          begin
            FOutput.WriteString('/Subtype/Type0');
            FOutput.WriteString('/Encoding/Identity-H');
          end
          else
          begin
            FOutput.WriteString('/Subtype/TrueType');
            FOutput.WriteString('/Encoding/WinAnsiEncoding');
          end;

          if Assigned(ft.FontDescriptor) then
            FOutput.WriteString('/FontDescriptor ' + IntToStr(FXRefObjects.IndexOf(ft.FontDescriptor.XRefObject)) + ' 0 R');

          FOutput.WriteString('/Name' + ft.RefName);

          if Assigned(ft.FontDescendant) then
            FOutput.WriteString('/DescendantFonts[' + IntToStr(FXRefObjects.IndexOf(ft.FontDescendant.XRefObject)) + ' 0 R]');

          if ft.Unicode then
          begin
            if Assigned(ft.FontUnicode) and EmbedFonts then
              FOutput.WriteString('/ToUnicode ' + IntToStr(FXRefObjects.IndexOf(ft.FontUnicode.XRefObject)) + ' 0 R')
          end
          else
          begin
            FOutput.WriteString('/FirstChar ' + ft.FirstGlyphAsString);
            FOutput.WriteString('/LastChar ' + ft.LastGlyphAsString);
            FOutput.WriteString('/Widths['+ft.WidthsAsString+']');
          end;

          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibFontDescriptor then
      begin
        ftd := (x.XRefObject as TTMSFNCGeneralPDFLibFontDescriptor);
        if not ftd.Font.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/FontDescriptor');
          FOutput.WriteString('/FontName/' + ftd.Font.Name);
          FOutput.WriteString('/Ascent ' + IntToStr(ftd.Font.Ascent));
          FOutput.WriteString('/CapHeight ' + IntToStr(ftd.Font.CapHeight));
          FOutput.WriteString('/Descent ' + IntToStr(ftd.Font.Descent));
          FOutput.WriteString('/ItalicAngle ' + IntToStr(ftd.Font.ItalicAngle));
          FOutput.WriteString('/StemV 87');
          FOutput.WriteString('/Flags 32');
          FOutput.WriteString('/MissingWidth 600');
          FOutput.WriteString('/FontBBox[' + ftd.Font.BoxAsString+']');
          if Assigned(ftd.Font) and Assigned(ftd.Font.FontFile) and EmbedFonts then
            FOutput.WriteString('/FontFile2 ' + IntToStr(FXRefObjects.IndexOf(ftd.Font.FontFile.XRefObject)) + ' 0 R');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibFontFile then
      begin
        ftff := (x.XRefObject as TTMSFNCGeneralPDFLibFontFile);
        if not ftff.Font.Inactive then
        begin
          os := ftff.Font.Initializer.GetTTFDataLength;
          ftff.Font.Initializer.CompressTTFData;
          ls := ftff.Font.Initializer.GetTTFDataCompressedLength;

          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Length ' + IntToStr(ls));
          FOutput.WriteString('/Length1 ' + IntToStr(os));
          FOutput.WriteString('/Filter');
          FOutput.WriteString('/FlateDecode');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('stream' + PDFLB);
          ms := ftff.Font.Initializer.GetTTFDataCompressed;
          if Assigned(ms) then
            FOutput.Streams.Add(ms);
          FOutput.StartNewStream;
          FOutput.WriteString(PDFLB);
          FOutput.WriteString('endstream' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibFontDescendant then
      begin
        ftds := (x.XRefObject as TTMSFNCGeneralPDFLibFontDescendant);
        if not ftds.Font.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/Font');
          FOutput.WriteString('/Subtype/CIDFontType2');
          case PDFStandard of
            pdfA1: FOutput.WriteString('/CIDToGIDMap/Identity');
          end;
          FOutput.WriteString('/BaseFont/' + ftds.Font.Name);
          FOutput.WriteString('/CIDSystemInfo<</Supplement 0/Ordering(Identity)/Registry(Adobe)>>');
          FOutput.WriteString('/DW 600');
          FOutput.WriteString('/W[' + ftds.Font.GlyphsAndWidthsAsString + ']');

          if Assigned(ftds.Font) and Assigned(ftds.Font.FontDescriptor) then
            FOutput.WriteString('/FontDescriptor ' + IntToStr(FXRefObjects.IndexOf(ftds.Font.FontDescriptor.XRefObject)) + ' 0 R');

          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibFontUnicode then
      begin
        ftu := (x.XRefObject as TTMSFNCGeneralPDFLibFontUnicode);
        if not ftu.Font.Inactive then
        begin
          s := '/CIDInit /ProcSet findresource begin'+ PDFLF+
          '12 dict begin'+ PDFLF + 'begincmap'+ PDFLF + '/CIDSystemInfo'+ PDFLF + '<<'+ PDFLF + '/Registry ('+
          ftu.Font.RefNameNotEscaped+'+0)'+ PDFLF + '/Ordering (UCS)'+ PDFLF + '/Supplement 0'+ PDFLF + '>> def'+ PDFLF+
          '/CMapName /'+ftu.font.RefNameNotEscaped+'+0 def'+ PDFLF + '/CMapType 2 def'+ PDFLF+
          '1 begincodespacerange'+ PDFLF + '<'+ FOutput.AddHex4(ftu.Font.FirstGlyph)+'> <'+
          FOutput.AddHex4(ftu.Font.LastGlyph)+'>'+ PDFLF + 'endcodespacerange'+ PDFLF;

          s := s + IntToStr(ftu.Font.UsedCharArray.Count) + ' beginbfchar' + PDFLF;
          for i := 0 to ftu.Font.UsedCharArray.Count - 1 do
          begin
            idx := ftu.Font.CharArray.IndexOf(ftu.Font.UsedCharArray[I]);
            if (idx > -1) then
            begin
              {$IFDEF WEBLIB}
              if EmbedFonts and not FullFontEmbedding then
                s := s + '<' + FOutput.AddHex4(ftu.Font.GlyphIDs.IndexOf(ftu.Font.CharWidths[idx].g))+'><'+ FOutput.AddHex4(ftu.Font.CharArray.v[idx])+ '>'+PDFLF
              else
              {$ENDIF}
                s := s + '<' + FOutput.AddHex4(ftu.Font.CharWidths[idx].g)+'><'+ FOutput.AddHex4(ftu.Font.CharArray.v[idx])+ '>'+PDFLF;
            end;
          end;
          s := s + 'endbfchar' + PDFLF;
          s := s + 'endcmap' + PDFLF + 'CMapName currentdict /CMap defineresource pop'+PDFLF+'end'+PDFLF+'end';

          ms := FOutput.CompressString(s);
          ls := 0;
          if Assigned(ms) then
            ls := ms.Size;

          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Length ' + IntToStr(ls));
          FOutput.WriteString('/Filter');
          FOutput.WriteString('/FlateDecode');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('stream' + PDFLB);
          if Assigned(ms) then
            FOutput.Streams.Add(ms);
          FOutput.StartNewStream;
          FOutput.WriteString(PDFLB);
          FOutput.WriteString('endstream' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteAcroFormFieldList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibAcroFormField then
      begin
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
//        FOutput.WriteString('/DA (/Helv 12 Tf 0 g)');
//        FOutput.WriteString('/F 4');
//        FOutput.WriteString('/FT Tx');
//        FOutput.WriteString('/Rect [9, 680, 297, 702]');
//        FOutput.WriteString('/Subtype /Widget');
//        FOutput.WriteString('/Type /Annot');
//        FOutput.WriteString('/T (SimpleText)');
//        FOutput.WriteString('/V (A Single line of text in one style)');
//        FOutput.WriteString('/AP <</N '+PDFPageAcroFormFieldContentRef+'>>');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibAcroFormFieldContent then
      begin
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('stream' + PDFLB);
//        FOutput.WriteString('/Tx BMC' + PDFLB);
//        FOutput.WriteString('q' + PDFLB);
//        FOutput.WriteString('BT' + PDFLB);
//        FOutput.WriteString('0 0 1 rg' + PDFLB);
//        FOutput.WriteString('/Ti 12 Tf' + PDFLB);
//        FOutput.WriteString('1 0 0 1 100 100 Tm' + PDFLB);
//        FOutput.WriteString('0 0 Td' + PDFLB);
//        FOutput.WriteString('( The quick brown fox ) Tj' + PDFLB);
//        FOutput.WriteString('0 −13 Td' + PDFLB);
//        FOutput.WriteString('( ate the lazy mouse. ) Tj' + PDFLB);
//        FOutput.WriteString('ET' + PDFLB);
//        FOutput.WriteString('Q' + PDFLB);
//        FOutput.WriteString('EMC' + PDFLB);
        FOutput.WriteString('endstream' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteAnnotationList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  an: TTMSFNCGeneralPDFLibAnnotation;
  u: TTMSFNCGeneralPDFLibURL;
  gt: TTMSFNCGeneralPDFLibGoTo;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibAnnotation then
      begin
        an := x.XRefObject as TTMSFNCGeneralPDFLibAnnotation;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/Annot');
        if (an is TTMSFNCGeneralPDFLibURL) or (an is TTMSFNCGeneralPDFLibGoto) then
        begin
          FOutput.WriteString('/Subtype/Link');
          FOutput.WriteString('/A');
          FOutput.WriteString('<<');
          if an is TTMSFNCGeneralPDFLibURL then
          begin
            u := an as TTMSFNCGeneralPDFLibURL;
            {$IFDEF LCLLIB}
            FOutput.WriteString('/Type/Action/S/URI/URI('+UTF8Encode(u.URL) + ')');
            {$ENDIF}
            {$IFNDEF LCLLIB}
            FOutput.WriteString('/Type/Action/S/URI/URI('+u.URL + ')');
            {$ENDIF}
          end
          else if an is TTMSFNCGeneralPDFLibGoTo then
          begin
            gt := an as TTMSFNCGeneralPDFLibGoTo;
            {$IFDEF LCLLIB}
            FOutput.WriteString('/Type/Action/S/GoTo/D['+UTF8Encode(gt.Destination) + ']');
            {$ENDIF}
            {$IFNDEF LCLLIB}
            FOutput.WriteString('/Type/Action/S/GoTo/D['+gt.Destination + ']');
            {$ENDIF}
          end;
          FOutput.WriteString('>>');
          FOutput.WriteString('/BS');
          FOutput.WriteString('<<');
          FOutput.WriteString('/W 0/S/S');
          FOutput.WriteString('>>');
        end;

        FOutput.WriteString('/Rect['+ an.RectAsString +']');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteBitmapList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  bmp: TTMSFNCGeneralPDFLibBitmap;
  ms: TMemoryStream;
  ls: Integer;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibBitmap then
      begin
        bmp := x.XRefObject as TTMSFNCGeneralPDFLibBitmap;

        {$IFDEF WEBLIB}
        ms := TMemoryStream.Create;
        bmp.Stream.Position := 0;
        ms.CopyFrom(bmp.Stream, bmp.Stream.Size);
        {$ELSE}
        ms := TTMSFNCUtils.ConvertBitmapToJPEGStream(bmp.Bitmap, bmp.Quality, bmp.BackgroundColor);
        {$ENDIF}
        ls := 0;
        if Assigned(ms) then
          ls := ms.Size;

        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/XObject');
        FOutput.WriteString('/Subtype/Image');
        FOutput.WriteString('/Filter/DCTDecode');
        FOutput.WriteString('/Width ' + IntToStr(bmp.Bitmap.Width));
        FOutput.WriteString('/Height ' + IntToStr(bmp.Bitmap.Height));
        FOutput.WriteString('/ColorSpace/DeviceRGB');
        FOutput.WriteString('/BitsPerComponent 8');
        FOutput.WriteString('/Length ' + IntToStr(ls));
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('stream' + PDFLB);
        if Assigned(ms) then
          FOutput.Streams.Add(ms);
        FOutput.StartNewStream;
        FOutput.WriteString(PDFLB);
        FOutput.WriteString('endstream' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WritePatternList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  p: TTMSFNCGeneralPDFLibPattern;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibPattern then
      begin
        p := x.XRefObject as TTMSFNCGeneralPDFLibPattern;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/Pattern');
        FOutput.WriteString('/PatternType 2');
        if Assigned(p.Shading) then
          FOutput.WriteString('/Shading ' + IntToStr(FXRefObjects.IndexOf(p.Shading.XRefObject)) + ' 0 R');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteShadingList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  f: TTMSFNCGeneralPDFLibShadingFunction;
  sf: TTMSFNCGeneralPDFLibShadingSubFunction;
  s: TTMSFNCGeneralPDFLibShading;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibShading then
      begin
        s := x.XRefObject as TTMSFNCGeneralPDFLibShading;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        if s is TTMSFNCGeneralPDFLibLinearGradient then
          FOutput.WriteString('/ShadingType 2');

        FOutput.WriteString('/Extend[false false]');
        FOutput.WriteString('/ColorSpace/DeviceRGB');
        FOutput.WriteString('/Coords['+s.RectAsString+']');
        if Assigned(s.&Function) then
          FOutput.WriteString('/Function ' + IntToStr(FXRefObjects.IndexOf(s.&Function.XRefObject)) + ' 0 R');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibShadingFunction then
      begin
        f := x.XRefObject as TTMSFNCGeneralPDFLibShadingFunction;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/FunctionType 3');
        FOutput.WriteString('/Domain[0 1]');
        FOutput.WriteString('/Encode[0 1 0 1]');
        FOutput.WriteString('/Bounds[1]');
        FOutput.WriteString('/Functions['+ f.SubFunctionsRef +']');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TTMSFNCGeneralPDFLibShadingSubFunction then
      begin
        sf := x.XRefObject as TTMSFNCGeneralPDFLibShadingSubFunction;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/FunctionType 2');
        FOutput.WriteString('/Domain[0 1]');
        FOutput.WriteString('/C0['+FOutput.ConvertColorToString(sf.FillColor)+']');
        FOutput.WriteString('/C1['+FOutput.ConvertColorToString(sf.FillColorTo)+']');
        FOutput.WriteString('/N 1');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.WriteXRefTable;
var
  s, sref: string;
  cnt: integer;
  x: TTMSFNCGeneralPDFLibXRefObject;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  sref := '';
  cnt := 0;
  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if x.InXRefList then
    begin
      Inc(cnt);
      sref := sref + x.GenerateXRefValue + PDFLB;
    end;
  end;

  s := 'xref' + PDFLB + '0 ' + IntToStr(cnt) + PDFLB;
  s := s + sref;
  FOutput.WriteString(S);
end;

procedure TTMSFNCGeneralPDFLib.FinishPage;
var
  I: Integer;
  {$IFDEF WEBLIB}
  J, idx: Integer;
  {$ENDIF}
  ft: TTMSFNCGeneralPDFLibFont;
  p: TTMSFNCGeneralPDFLibPattern;
  bmp: TTMSFNCGeneralPDFLibBitmap;
  ms: TStringStream;
  a: TTMSFNCGeneralPDFLibAnnotation;
begin
  if FPageStarted then
  begin
    FPageStarted := False;

    for I := 0 to FActivePage.FontList.Count - 1 do
    begin
      ft := FActivePage.FontList[I];
      FOutput.ReplaceString(ft.RefNameOriginal, ft.RefName);
    end;

    for I := 0 to FActivePage.Patterns.Count - 1 do
    begin
      p := FActivePage.Patterns[I];
      FOutput.ReplaceString(p.RefNameOriginal, p.RefName);
    end;

    for I := 0 to FActivePage.Bitmaps.Count - 1 do
    begin
      bmp := FActivePage.Bitmaps[I];
      FOutput.ReplaceString(bmp.RefNameOriginal, bmp.RefName);
    end;

    for I := 0 to FActivePage.Annotations.Count - 1 do
    begin
      a := FActivePage.Annotations[I];
      FOutput.ReplaceString(a.RefNameOriginal, a.RefName);
    end;

    ms := FOutput.FinishContentStream(PDFPageContentLengthRef, True);
    if Assigned(ms) then
      FOutput.Streams.Add(ms);
    FOutput.StartNewStream;
    FOutput.WriteString(PDFLB);
    FOutput.WriteString('endstream' + PDFLB);
    FOutput.WriteString('endobj' + PDFLB);
    FOutput.ClearProperties;
  end;
end;

procedure TTMSFNCGeneralPDFLib.FontChanged(Sender: TObject);
begin
  UpdateFontList;
end;

function  TTMSFNCGeneralPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := False;
end;

procedure TTMSFNCGeneralPDFLib.InitializeFontList;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  ftff: TTMSFNCGeneralPDFLibFontFile;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  if not Assigned(FOutput) then
    Exit;

  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TTMSFNCGeneralPDFLibFontFile then
      begin
        ftff := (x.XRefObject as TTMSFNCGeneralPDFLibFontFile);
        if not ftff.Font.Inactive then
          ftff.InitializeFontFile;
      end
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.InsertPage(PageIndex: Integer);
begin
  NewPage;
  if Assigned(FActivePage) then
    FActivePage.InsertPageNumber := PageIndex;
end;

function TTMSFNCGeneralPDFLib.IsDocumentOpened: Boolean;
begin
  Result := False;
end;

function TTMSFNCGeneralPDFLib.GetAcroFormFieldReferences: string;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  Result := '';
  {$IFDEF WEBLIB}
  for v in FXRefObjects do
  begin
    x := TTMSFNCGeneralPDFLibXRefObject(v);
  {$ELSE}
  for x in FXRefObjects do
  begin
  {$ENDIF}
    if Assigned(x.XRefObject) then
      if x.XRefObject is TTMSFNCGeneralPDFLibAcroFormField then
        Result := Result + IntToStr(FXRefObjects.IndexOf(x)) + ' 0 R ';
  end;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetAcroFormRef: string;
begin
  Result := '';
  if Assigned(FAcroForm) and Assigned(FAcroForm.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(FAcroForm.XRefObject));

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetAllowsCopying: Boolean;
begin
  Result := AllowsCopying;
end;

function TTMSFNCGeneralPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := AllowsPrinting;
end;

function TTMSFNCGeneralPDFLib.GetArtBox: TRectF;
begin
  Result := ArtBox;
end;

function TTMSFNCGeneralPDFLib.GetAuthor: String;
begin
  Result := Author;
end;

function TTMSFNCGeneralPDFLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.BitmapContainer;
end;

function TTMSFNCGeneralPDFLib.GetBleedBox: TRectF;
begin
  Result := BleedBox;
end;

function TTMSFNCGeneralPDFLib.GetContentRef(APage: TTMSFNCGeneralPDFLibPage): string;
begin
  Result := '';
  if Assigned(APage.Content) and Assigned(APage.Content.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(APage.Content.XRefObject));

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetCreationDate: String;
begin
  Result := CreationDate;
end;

function TTMSFNCGeneralPDFLib.GetCreator: String;
begin
  Result := Creator;
end;

function TTMSFNCGeneralPDFLib.GetCropBox: TRectF;
begin
  Result := CropBox;
end;

procedure TTMSFNCGeneralPDFLib.GetDocumentInfo;
begin
end;

function TTMSFNCGeneralPDFLib.GetEmbedFonts: Boolean;
begin
  Result := EmbedFonts;
end;

{$IFDEF WEBLIB}
function TTMSFNCGeneralPDFLib.GetFullFontEmbedding: Boolean;
begin
  Result := FullFontEmbedding;
end;
{$ENDIF}

function TTMSFNCGeneralPDFLib.GetFontFallBackList: TStrings;
begin
  Result := FontFallBackList;
end;

function TTMSFNCGeneralPDFLib.GetFontRefs(APage: TTMSFNCGeneralPDFLibPage): String;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
  begin
    for I := 0 to APage.FontList.Count - 1 do
    begin
      if not APage.FontList[I].Inactive then
        Result := Result + APage.FontList[I].Ref;
    end;
  end;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetFooter: UnicodeString;
begin
  Result := Footer;
end;

function TTMSFNCGeneralPDFLib.GetFooterAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := FooterAlignment;
end;

function TTMSFNCGeneralPDFLib.GetFooterFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FFooterFont;
end;

function TTMSFNCGeneralPDFLib.GetFooterMargins: TTMSFNCMargins;
begin
  Result := FooterMargins;
end;

function TTMSFNCGeneralPDFLib.GetFooterRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FFooterMargins.Left, FMediaBox.Bottom - FFooterSize - FFooterMargins.Bottom, FMediaBox.Right - FMediaBox.Left - FFooterMargins.Right, FMediaBox.Bottom - FFooterMargins.Bottom);
end;

function TTMSFNCGeneralPDFLib.GetFooterSize: Single;
begin
  Result := FooterSize;
end;

function TTMSFNCGeneralPDFLib.GetHeader: UnicodeString;
begin
  Result := Header;
end;

function TTMSFNCGeneralPDFLib.GetHeaderAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := HeaderAlignment;
end;

function TTMSFNCGeneralPDFLib.GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FHeaderFont;
end;

function TTMSFNCGeneralPDFLib.GetHeaderMargins: TTMSFNCMargins;
begin
  Result := HeaderMargins;
end;

function TTMSFNCGeneralPDFLib.GetHeaderRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FHeaderMargins.Left, FMediaBox.Top + FHeaderMargins.Top, FMediaBox.Right - FMediaBox.Left - FHeaderMargins.Right, FMediaBox.Top + FHeaderMargins.Top + FHeaderSize);
end;

function TTMSFNCGeneralPDFLib.GetHeaderSize: Single;
begin
  Result := HeaderSize;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := PageNumberFormat;
end;

function TTMSFNCGeneralPDFLib.GetPageNumber: TTMSFNCPDFLibPageNumber;
begin
  Result := PageNumber;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := PageNumberAlignment;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FPageNumberFont;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberMargins: TTMSFNCMargins;
begin
  Result := PageNumberMargins;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  case PageNumber of
    pnHeader: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Top + FPageNumberMargins.Top, FMediaBox.Right - FMediaBox.Left - FPageNumberMargins.Right, FMediaBox.Top + FPageNumberMargins.Top + FPageNumberSize);
    pnFooter: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Bottom - FPageNumberSize - FPageNumberMargins.Bottom, FMediaBox.Right - FMediaBox.Left - FPageNumberMargins.Right, FMediaBox.Bottom - FPageNumberMargins.Bottom);
  end;
end;

function TTMSFNCGeneralPDFLib.GetPageNumberSize: Single;
begin
  Result := PageNumberSize;
end;

function TTMSFNCGeneralPDFLib.GetKeywordsString: string;
var
  s: string;
begin
  Result := '';
  for s in FKeywords do
    Result := Result + s + ' ';

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

function TTMSFNCGeneralPDFLib.GetMediaBox: TRectF;
begin
  Result := MediaBox;
end;

function TTMSFNCGeneralPDFLib.GetModificationDate: string;
begin
  Result := ModificationDate;
end;

function TTMSFNCGeneralPDFLib.GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
begin
  Result := FOnAfterDrawFooter;
end;

function TTMSFNCGeneralPDFLib.GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
begin
  Result := FOnAfterDrawHeader;
end;

function TTMSFNCGeneralPDFLib.GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
begin
  Result := FOnAfterDrawPageNumber;
end;

function TTMSFNCGeneralPDFLib.GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
begin
  Result := FOnBeforeDrawFooter;
end;

function TTMSFNCGeneralPDFLib.GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
begin
  Result := FOnBeforeDrawHeader
end;

function TTMSFNCGeneralPDFLib.GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
begin
  Result := FOnBeforeDrawPageNumber
end;

function TTMSFNCGeneralPDFLib.GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
begin
  Result := FOnNewPageStarted;
end;

function TTMSFNCGeneralPDFLib.GetDestOutputProfileRef: String;
begin
  Result := '';
  if Assigned(FDestOutputProfile) and Assigned(FDestOutputProfile.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(FDestOutputProfile.XRefObject));

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetOwnerPassword: String;
begin
  Result := OwnerPassword;
end;

function TTMSFNCGeneralPDFLib.GetPageCount: Integer;
begin
  Result := FPageCount;
end;

function TTMSFNCGeneralPDFLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCGeneralPDFLib.GetPageIndex: Integer;
begin
  Result := 0;
  if Assigned(FActivePage) then
    Result := FActivePage.PageNumber;
end;

procedure TTMSFNCGeneralPDFLib.GetPageInfo(PageIndex: Integer);
begin
end;

function TTMSFNCGeneralPDFLib.GetPageOrientation: TTMSFNCPDFLibPageOrientation;
begin
  Result := Orientation;
end;

function TTMSFNCGeneralPDFLib.GetPageRef(APage: TTMSFNCGeneralPDFLibPage): string;
begin
  Result := '';
  if Assigned(APage) and Assigned(APage.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(APage.XRefObject));

  Result := Trim(Result);
end;

function ComparePageNumbers(const Item1, Item2: TTMSFNCGeneralPDFLibPage): Integer;
begin
  Result := CompareValue(Item1.InsertPageNumber, Item2.InsertPageNumber);
end;

function TTMSFNCGeneralPDFLib.GetPageReferences: String;
var
  x: TTMSFNCGeneralPDFLibXRefObject;
  l: TTMSFNCGeneralPDFLibPageList;
  I: Integer;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  Result := '';

  l := TTMSFNCGeneralPDFLibPageList.Create;
  try
    {$IFDEF WEBLIB}
    for v in FXRefObjects do
    begin
      x := TTMSFNCGeneralPDFLibXRefObject(v);
    {$ELSE}
    for x in FXRefObjects do
    begin
    {$ENDIF}
      if Assigned(x.XRefObject) then
        if x.XRefObject is TTMSFNCGeneralPDFLibPage then
          l.Add(x.XRefObject as TTMSFNCGeneralPDFLibPage);
    end;

    {$IFNDEF LCLWEBLIB}
    l.Sort(FComparePageNumbers);
    {$ENDIF}
    {$IFDEF LCLWEBLIB}
    {$IFDEF LCLLIB}
    l.Sort(@ComparePageNumbers);
    {$ENDIF}
    {$IFNDEF LCLLIB}
    l.Sort(TListSortCompare(@ComparePageNumbers));
    {$ENDIF}
    {$ENDIF}

    for I := 0 to l.Count - 1 do
      Result := Result + IntToStr(FXRefObjects.IndexOf(l[I].XRefObject)) + ' 0 R ';

    Result := Trim(Result);
  finally
    l.Free;
  end;
end;

function TTMSFNCGeneralPDFLib.GetPageSize: TTMSFNCPDFLibPageSize;
begin
  Result := PageSize;
end;

function TTMSFNCGeneralPDFLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCGeneralPDFLib.GetShadingRef(APage: TTMSFNCGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.Shadings.Count - 1 do
      Result := Result + APage.Shadings[I].Ref;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetAnnotationsRef(APage: TTMSFNCGeneralPDFLibPage): string;
var
  I: Integer;
  ft: TTMSFNCGeneralPDFLibAcroFormField;
  an: TTMSFNCGeneralPDFLibAnnotation;
begin
  Result := '';
  if Assigned(APage) then
  begin
    for I := 0 to APage.Annotations.Count - 1 do
    begin
      an := APage.Annotations[I];
      ft := an.AcroFormField;
      if Assigned(ft) then
        Result := Result + ft.Ref + ' '
      else
        Result := Result + an.Ref + ' ';
    end;
  end;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetPatternRef(APage: TTMSFNCGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.Patterns.Count - 1 do
      Result := Result + APage.Patterns[I].Ref;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLib.GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TTMSFNCGeneralPDFLib.GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TTMSFNCGeneralPDFLib.GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

function TTMSFNCGeneralPDFLib.GetPDFStandard: TTMSFNCPDFLibStandard;
begin
  Result := PDFStandard;
end;

function TTMSFNCGeneralPDFLib.GetProducer: String;
begin
  Result := Producer;
end;

function TTMSFNCGeneralPDFLib.GetSubject: String;
begin
  Result := Subject;
end;

function TTMSFNCGeneralPDFLib.GetTitle: String;
begin
  Result := Title;
end;

function TTMSFNCGeneralPDFLib.GetTrimBox: TRectF;
begin
  Result := TrimBox;
end;

function TTMSFNCGeneralPDFLib.GetUserPassword: String;
begin
  Result := UserPassword;
end;

function TTMSFNCGeneralPDFLib.GetXObjectRefs(APage: TTMSFNCGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.XObjectList.Count - 1 do
      Result := Result + APage.XObjectList[I].Ref;

  Result := Trim(Result);
end;

procedure TTMSFNCGeneralPDFLib.SaveDocumentFromStream(FileStream: TMemoryStream;
  FileName: String);
begin
end;

function TTMSFNCGeneralPDFLib.SearchForFont(ABaseFontName: string; AUnicodeFont: Boolean): TTMSFNCGeneralPDFLibFont;
var
  ft: TTMSFNCGeneralPDFLibFont;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  Result := nil;
  {$IFDEF WEBLIB}
  for v in FFontList do
  begin
    ft := TTMSFNCGeneralPDFLibFont(v);
  {$ELSE}
  for ft in FFontList do
  begin
  {$ENDIF}
    if not ft.Inactive and (ft.Name = ABaseFontName) and (ft.Unicode = AUnicodeFont) then
    begin
      Result := ft;
      Break;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  AllowsCopying := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  AllowsPrinting := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetArtBox(const Value: TRectF);
begin
  ArtBox := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetAuthor(const Value: String);
begin
  Author := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib.BitmapContainer := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetBleedBox(const Value: TRectF);
begin
  BleedBox := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetCreator(const Value: String);
begin
  Creator := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetCropBox(const Value: TRectF);
begin
  CropBox := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  EmbedFonts := Value;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGeneralPDFLib.SetFullFontEmbedding(const Value: Boolean);
begin
  FullFontEmbedding := Value;
end;
{$ENDIF}

procedure TTMSFNCGeneralPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  FontFallBackList.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetFooter(const Value: UnicodeString);
begin
  Footer := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FooterAlignment := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetFooterFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  FooterMargins.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetFooterSize(const Value: Single);
begin
  FooterSize := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPDFGraphicsExLib(
  const Value: ITMSFNCCustomPDFGraphicsExLib);
begin
  FPDFGraphicsExLib := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPDFGraphicsLib(
  const Value: ITMSFNCCustomPDFGraphicsLib);
begin
  FPDFGraphicsLib := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPDFInitializationLib(
  const Value: ITMSFNCCustomPDFInitializationLib);
begin
  FPDFInitializationLib := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPDFStandard(
  const Value: TTMSFNCPDFLibStandard);
begin
  PDFStandard := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetHeader(const Value: UnicodeString);
begin
  FHeader := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  HeaderAlignment := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetHeaderFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  HeaderMargins.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetHeaderSize(const Value: Single);
begin
  HeaderSize := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumberAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumberFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetKeywords(const Value: TStrings);
begin
  Keywords.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLib.SetMediaBox(const Value: TRectF);
begin
  MediaBox := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnAfterDrawFooter(
  const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
begin
  FOnAfterDrawFooter := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnAfterDrawHeader(
  const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
begin
  FOnAfterDrawHeader := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnAfterDrawPageNumber(
  const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
begin
  FOnAfterDrawPageNumber := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnBeforeDrawFooter(
  const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
begin
  FOnBeforeDrawFooter := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnBeforeDrawHeader(
  const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
begin
  FOnBeforeDrawHeader := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
begin
  FOnBeforeDrawPageNumber := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOnNewPageStarted(
  const Value: TTMSFNCPDFLibNewPageStartedEvent);
begin
  FOnNewPageStarted := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetOwnerPassword(const Value: String);
begin
  OwnerPassword := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCGeneralPDFLib.SetPageOrientation(
  const Value: TTMSFNCPDFLibPageOrientation);
begin
  Orientation := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCGeneralPDFLib.SetPageSize(
  const Value: TTMSFNCPDFLibPageSize);
begin
  PageSize := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCGeneralPDFLib.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCGeneralPDFLib.SetSubject(const Value: String);
begin
  Subject := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetTitle(const Value: String);
begin
  Title := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetTrimBox(const Value: TRectF);
begin
  TrimBox := Value;
end;

procedure TTMSFNCGeneralPDFLib.SetUserPassword(const Value: String);
begin
  UserPassword := Value;
end;

procedure TTMSFNCGeneralPDFLib.UpdateBoxRect;
var
  w, h: Single;
  g: ITMSFNCCustomPDFInitializationLib;
begin
  case PageSize of
    psA0: FMediaBox := RectF(0, 0, 2384, 3370);
    psA1: FMediaBox := RectF(0, 0, 1684, 2384);
    psA2: FMediaBox := RectF(0, 0, 1190, 1684);
    psA3: FMediaBox := RectF(0, 0, 842, 1190);
    psA4: FMediaBox := RectF(0, 0, 595, 842);
    psA5: FMediaBox := RectF(0, 0, 420, 595);
    psA6: FMediaBox := RectF(0, 0, 298, 420);
    psA7: FMediaBox := RectF(0, 0, 210, 298);
    psA8: FMediaBox := RectF(0, 0, 148, 210);
    psB0: FMediaBox := RectF(0, 0, 2835, 4008);
    psB1: FMediaBox := RectF(0, 0, 2004, 2835);
    psB2: FMediaBox := RectF(0, 0, 1417, 2004);
    psB3: FMediaBox := RectF(0, 0, 1001, 1417);
    psB4: FMediaBox := RectF(0, 0, 709, 1001);
    psB5: FMediaBox := RectF(0, 0, 499, 709);
    psB6: FMediaBox := RectF(0, 0, 354, 499);
    psB7: FMediaBox := RectF(0, 0, 249, 354);
    psB8: FMediaBox := RectF(0, 0, 176, 249);
    psB9: FMediaBox := RectF(0, 0, 125, 176);
    psB10: FMediaBox := RectF(0, 0, 88, 125);
    psC2: FMediaBox := RectF(0, 0, 1837, 578);
    psC3: FMediaBox := RectF(0, 0, 578, 919);
    psC4: FMediaBox := RectF(0, 0, 919, 649);
    psC5: FMediaBox := RectF(0, 0, 649, 459);
    psC6: FMediaBox := RectF(0, 0, 459, 323);
    psD0: FMediaBox := RectF(0, 0, 3090, 2186);
    psSRA0: FMediaBox := RectF(0, 0, 3628, 2551);
    psSRA1: FMediaBox := RectF(0, 0, 2551, 1814);
    psSRA2: FMediaBox := RectF(0, 0, 1814, 1276);
    psSRA3: FMediaBox := RectF(0, 0, 1276, 907);
    psSRA4: FMediaBox := RectF(0, 0, 907, 638);
    psRA0: FMediaBox := RectF(0, 0, 3458, 2438);
    psRA1: FMediaBox := RectF(0, 0, 2438, 1729);
    psRA2: FMediaBox := RectF(0, 0, 1729, 1219);
    psLetter: FMediaBox := RectF(0, 0, 612, 792);
    psLegal: FMediaBox := RectF(0, 0, 612, 1008);
    psLedger: FMediaBox := RectF(0, 0, 792, 1224);
    psTabloid: FMediaBox := RectF(0, 0, 1224, 792);
    psExecutive: FMediaBox := RectF(0, 0, 522, 756);
    psANSIC: FMediaBox := RectF(0, 0, 1584, 1224);
    psANSID: FMediaBox := RectF(0, 0, 2448, 1584);
    psANSIE: FMediaBox := RectF(0, 0, 3168, 2448);
    psFoolscap: FMediaBox := RectF(0, 0, 954, 1188);
    psSmallPost: FMediaBox := RectF(0, 0, 1044, 1332);
    psSheetAnd13Cap: FMediaBox := RectF(0, 0, 954, 1584);
    psSheetAnd12Cap: FMediaBox := RectF(0, 0, 954, 1782);
    psDemy: FMediaBox := RectF(0, 0, 1116, 1440);
    psLargePost: FMediaBox := RectF(0, 0, 1188, 1512);
    psSmallmedium: FMediaBox := RectF(0, 0, 1260, 1584);
    psMedium: FMediaBox := RectF(0, 0, 1296, 1656);
    psSmallRoyal: FMediaBox := RectF(0, 0, 1368, 1728);
    psRoyal: FMediaBox := RectF(0, 0, 1440, 1800);
    psImperial: FMediaBox := RectF(0, 0, 1584, 2160);
    psMetricCrownQuarto: FMediaBox := RectF(0, 0, 536, 697);
    psMetricCrownOctavo: FMediaBox := RectF(0, 0, 349, 527);
    psMetricLargeCrownQuarto: FMediaBox := RectF(0, 0, 570, 731);
    psMetricLargeCrownOctavo: FMediaBox := RectF(0, 0, 366, 561);
    psMetricDemyQuarto: FMediaBox := RectF(0, 0, 621, 782);
    psMetricDemyOctavo: FMediaBox := RectF(0, 0, 391, 612);
    psMetricRoyalQuarto: FMediaBox := RectF(0, 0, 672, 884);
    psMetricRoyalOctavo: FMediaBox := RectF(0, 0, 366, 561);
    psCustom: FMediaBox := RectF(0, 0, PageWidth, PageHeight);
  end;

  if FPageOrientation = poLandscape then
  begin
    w := FMediaBox.Right - FMediaBox.Left;
    h := FMediaBox.Bottom - FMediaBox.Top;
    FMediaBox.Bottom := FMediaBox.Top + w;
    FMediaBox.Right := FMediaBox.Left + h;
  end;

  FCropBox := FMediaBox;
  FBleedBox := FMediaBox;
  FArtBox := FMediaBox;
  FTrimBox := FMediaBox;

  g := FPDFInitializationLib;
  if Assigned(g) then
  begin
    g.SetPageWidth(MediaBox.Right - MediaBox.Left);
    g.SetPageHeight(MediaBox.Bottom - MediaBox.Top);
  end;
end;

procedure TTMSFNCGeneralPDFLib.UpdateFontList(ASearchForUnicodeFont: Boolean = False);
var
  g: ITMSFNCCustomPDFGraphicsLib;
  r: TTMSFNCGeneralPDFLibXRefObject;
  ft: TTMSFNCGeneralPDFLibFont;
  ftd: TTMSFNCGeneralPDFLibFontDescriptor;
  ftff: TTMSFNCGeneralPDFLibFontFile;
  ftu: TTMSFNCGeneralPDFLibFontUnicode;
  ftds: TTMSFNCGeneralPDFLibFontDescendant;
  fontn: string;
  tm: TTMSFNCGeneralPDFLibFontMetrics;
begin
  if FBlockFontUpdate then
    Exit;

  g := FPDFGraphicsLib;
  if Assigned(g) and Assigned(FOutput) then
  begin
    fontn := VerifyFontName(g.Font.Name);
    FBlockFontUpdate := True;
    g.Font.Name := fontn;
    FBlockFontUpdate := False;

    if (FOutput.FontBase <> fontn) or (FOutput.FontStyle <> g.Font.Style) or ASearchForUnicodeFont or (FOutput.FontUnicode and not ASearchForUnicodeFont) then
    begin
      FOutput.FontBase := StringReplace(fontn, ' ', '', [rfReplaceAll]);
      FOutput.FontName := FOutput.FontBase;
      if (TFontStyle.fsBold in g.Font.Style) and (TFontStyle.fsItalic in g.Font.Style) then
        FOutput.FontName := FOutput.FontName +',BoldItalic'
      else if TFontStyle.fsBold in g.Font.Style then
        FOutput.FontName := FOutput.FontName +',Bold'
      else if TFontStyle.fsItalic in g.Font.Style then
        FOutput.FontName := FOutput.FontName +',Italic';

      ft := SearchForFont(FOutput.FontName, ASearchForUnicodeFont);

      if not Assigned(ft) then
      begin
        ft := TTMSFNCGeneralPDFLibFont.Create(Self, FInitializer, fontn, FOutput.FontName, g.Font.Size, g.Font.Style);
        FXRefObjects.AddObject(ft);
        FFontList.Add(ft);
        tm := ft.Initializer.GetFontMetrics;
        ft.Ascent := tm.Ascent;
        ft.CapHeight := tm.CapHeight;
        ft.ItalicAngle := tm.ItalicAngle;
        ft.Descent := tm.Descent;
        ft.Box := tm.FontBox;
        ft.TrueType := tm.TrueType;
        ft.Initializer.IsFixedWidth := tm.Fixed;
        ft.Initializer.InitializeCharWidths;
        ft.UnitsPerEm := ft.Initializer.GetUnitsPerEm;
        ft.RefNameOriginal := Format(PDFPageFontTextRef, [ft.RefNameNotEscaped]);
        ft.RefNameGlyphOriginal := Format(PDFPageFontGlyphTextRef, [ft.RefNameNotEscaped]);
      end;

      if not Assigned(ft.FontDescriptor) then
      begin
        ftd := TTMSFNCGeneralPDFLibFontDescriptor.Create(Self, ft);
        r := FXRefObjects.AddObject(ftd);
        r.XRefType := 'n';
        ft.FontDescriptor := ftd;
      end;

      if not Assigned(ft.FontUnicode) and ft.Unicode then
      begin        
        ftu := TTMSFNCGeneralPDFLibFontUnicode.Create(Self, ft);
        r := FXRefObjects.AddObject(ftu);
        r.XRefType := 'n';
        ft.FontUnicode := ftu;
      end;

      if not Assigned(ft.FontDescendant) and ft.Unicode then
      begin        
        ftds := TTMSFNCGeneralPDFLibFontDescendant.Create(Self, ft);
        r := FXRefObjects.AddObject(ftds);
        r.XRefType := 'n';
        ft.FontDescendant := ftds;
      end;

      if not Assigned(ft.FontFile) and EmbedFonts and ft.TrueType then
      begin
        ftff := TTMSFNCGeneralPDFLibFontFile.Create(Self, ft);
        r := FXRefObjects.AddObject(ftff);
        r.XRefType := 'n';
        ft.FontFile := ftff;
      end;
    end
    else
      ft := SearchForFont(FOutput.FontName, ASearchForUnicodeFont);

    FActiveFont := ft;

    if Assigned(ft) and Assigned(FActivePage) and (FActivePage.FontList.IndexOf(ft) = -1) then
      FActivePage.FontList.Add(ft);

    if Assigned(ft) then
    begin
      FOutput.FontCapHeight := ft.CapHeight;
      FOutput.FontUnitsPerEm := ft.UnitsPerEm;
    end
    else
    begin
      FOutput.FontCapHeight := 0;
      FOutput.FontUnitsPerEm := 0;
    end;

    FOutput.FontSize := g.Font.Size;
    FOutput.FontColor := g.Font.Color;
    FOutput.FontStyle := g.Font.Style;
    FOutput.FontLeading := g.Font.Size;
    FOutput.FontWordSpacing := 0;
    FOutput.FontCharSpacing := 0;
    FOutput.FontUnicode := Assigned(ft) and ft.Unicode;
    FOutput.FontEmbedding := EmbedFonts{$IFDEF WEBLIB} and not FullFontEmbedding{$ENDIF};
    if Assigned(ft) then
    begin
      FOutput.FontRefName := ft.RefNameOriginal;
      FOutput.FontGlyphRefName := ft.RefNameGlyphOriginal;
      FOutput.FontCharWidths := ft.CharWidths;
      FOutput.FontCharArray := ft.CharArray;
      FOutput.FontUsedCharArray := ft.UsedCharArray;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFLib.UpdateGlyphIds;
{$IFDEF WEBLIB}
var
  I, J, Idx: Integer;
  ft: TTMSFNCGeneralPDFLibFont;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  if EmbedFonts and not FullFontEmbedding and Assigned(FActivePage) then
  begin
    for I := 0 to FActivePage.FontList.Count - 1 do
    begin
      ft := FActivePage.FontList[I];
      for J := 0 to ft.UsedCharArray.Count - 1 do
      begin
        idx := ft.CharArray.IndexOf(ft.UsedCharArray[J]);
        if (idx > -1) then
          FOutput.ReplaceString(Format(PDFPageFontGlyphRef, [IntToStr(ft.UsedCharArray[J]), ft.RefNameGlyphOriginal]), FOutput.AddHex4(ft.GlyphIDs.IndexOf(ft.CharWidths[idx].g)));
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLib.VerifyFontName(AFontName: string): string;
var
  idx: Integer;
  fbi: Integer;
begin
  Result := AFontName;
  idx := FOSFontList.IndexOf(Result);
  fbi := 0;
  while (idx = - 1) and (fbi >= 0) and (fbi <= FontFallBackList.Count - 1) do
  begin
    Result := FontFallBackList[fbi];
    idx := FOSFontList.IndexOf(Result);
    if idx = -1 then
      Inc(fbi);
  end;
end;

{ TTMSFNCGeneralPDFLibXRefObject }

constructor TTMSFNCGeneralPDFLibXRefObject.Create(const AValue: TTMSFNCGeneralPDFLibObject = nil);
begin
  if Assigned(AValue) then
  begin
    FXRefType := 'n';
    FXRefNumber := AValue.Number;
    FXRefObject := AValue;
  end
  else
  begin
    FXRefType := 'f';
    FXRefNumber := 0;
  end;
  FXRefOffset := -1;
  FInXRefList := True;
end;

destructor TTMSFNCGeneralPDFLibXRefObject.Destroy;
begin
  if Assigned(FXRefObject) then
  begin
    FXRefObject.Free;
    FXRefObject := nil;
  end;

  inherited;
end;

class function TTMSFNCGeneralPDFLibXRefObject.FormatValue(AValue,
  ALength: Integer): string;
var
  i, K: integer;
  S: string;
begin
  Result := '';
  if AValue > 0 then
    S := IntToStr(AValue)
  else
    S := '0';

  i := ALength - Length(S);

  for K := 0 to i - 1 do
    Result := Result + '0';

  Result := Result + S;
end;

function TTMSFNCGeneralPDFLibXRefObject.GenerateXRefValue: string;
begin
  Result := FormatValue(FXRefOffset, 10) + ' ' + FormatValue(FXRefNumber, 5) + ' ' + FXRefType;
end;

{ TTMSFNCGeneralPDFLibObject }

constructor TTMSFNCGeneralPDFLibObject.Create(const APDFLib: TTMSFNCGeneralPDFLib; const ANumber: Integer = -1; const AXRefNumber: Integer = 0);
begin
  FPDFLib := APDFLib;
  FXRefNumber := AXRefNumber;
  FNumber := ANumber;
  FXRefObject := nil;
end;

destructor TTMSFNCGeneralPDFLibObject.Destroy;
begin
  FXRefObject := nil;
  inherited;
end;

function TTMSFNCGeneralPDFLibObject.GetRefNameBase: String;
begin
  Result := '';
end;

function TTMSFNCGeneralPDFLibObject.GetRef: string;
var
  r: String;
begin
  r := RefName;
  if r <> '' then
    Result := r + ' ' + IntToStr(GetRefIndex(False)) + ' 0 R'
  else
    Result := IntToStr(GetRefIndex(False)) + ' 0 R';
end;

function TTMSFNCGeneralPDFLibObject.GetRefIndex(ACheckClassType: Boolean): Integer;
begin
  Result := -1;
  if Assigned(PDFLib) and Assigned(PDFLib.FXRefObjects) then
    Result := PDFLib.FXRefObjects.IndexOfObject(Self, ACheckClassType);
end;

function TTMSFNCGeneralPDFLibObject.GetRefName: string;
var
  r: String;
begin
  Result := '';
  r := RefNameNotEscaped;
  if r <> '' then
    Result := '/' + r;
end;

function TTMSFNCGeneralPDFLibObject.GetRefNameNotEscaped: string;
var
  r: String;
begin
  Result := '';
  r := RefNameBase;
  if r <> '' then
    Result := r + IntToStr(GetRefIndex(True));
end;

procedure TTMSFNCGeneralPDFLibObject.SetNumber(const Value: integer);
begin
  FNumber := Value;
  if Value < 0 then
    FType := lotIndirect
  else
    FType := lotDirect;
end;

{ TTMSFNCGeneralPDFLibXRefObjects }

function TTMSFNCGeneralPDFLibXRefObjects.AddObject(const AObject: TTMSFNCGeneralPDFLibObject = nil): TTMSFNCGeneralPDFLibXRefObject;
var
  i: Integer;
begin
  Result := TTMSFNCGeneralPDFLibXRefObject.Create(AObject);
  i := Add(Result);
  if Assigned(AObject) then
  begin
    AObject.Number := i;
    AObject.XRefObject := Result;
  end;
end;

function TTMSFNCGeneralPDFLibXRefObjects.AddObjectClass(
  const AObjectClass: TTMSFNCGeneralPDFLibObjectClass): TTMSFNCGeneralPDFLibXRefObject;
var
  i: Integer;
  AObject: TTMSFNCGeneralPDFLibObject;
begin
  AObject := AObjectClass.Create(nil);
  Result := TTMSFNCGeneralPDFLibXRefObject.Create(AObject);
  i := Add(Result);
  if Assigned(AObject) then
  begin
    AObject.Number := i;
    AObject.XRefObject := Result;
  end;
end;

function TTMSFNCGeneralPDFLibXRefObjects.IndexOfObject(
  const AObject: TTMSFNCGeneralPDFLibObject; const ACheckClassType: Boolean = False): Integer;
var
  I, K: Integer;
  x: TTMSFNCGeneralPDFLibObject;
begin
  Result := -1;
  K := -1;
  if Assigned(AObject) then
  begin
    for I := 0 to Count - 1 do
    begin
      x := Items[I].XRefObject;
      if Assigned(x) then
      begin
        if (not ACheckClassType) or (ACheckClassType and (x.ClassType = AObject.ClassType)) then
          Inc(K);

        if Items[I].XRefObject = AObject then
        begin
          Result := K;
          Break;
        end;
      end
      else if not ACheckClassType then
        Inc(K);
    end;
  end;
end;

{ TTMSFNCGeneralPDFLibPage }

constructor TTMSFNCGeneralPDFLibPage.Create(const APDFLib: TTMSFNCGeneralPDFLib; const APageNumber: Integer);
begin
  inherited Create(APDFLib);
  FPageNumber := APageNumber;
  FInsertPageNumber := APageNumber;
  FFontList := TTMSFNCGeneralPDFLibFonts.Create;
  FXObjectList := TTMSFNCGeneralPDFLibXObjects.Create;
  FShadings := TTMSFNCGeneralPDFLibShadings.Create;
  FBitmaps := TTMSFNCGeneralPDFLibBitmaps.Create;
  FAnnotations := TTMSFNCGeneralPDFLibAnnotations.Create;
  FPatterns := TTMSFNCGeneralPDFLibPatterns.Create;
end;

destructor TTMSFNCGeneralPDFLibPage.Destroy;
begin
  FContent := nil;

  if Assigned(FAnnotations) then
  begin
    FAnnotations.Free;
    FAnnotations := nil;
  end;

  if Assigned(FBitmaps) then
  begin
    FBitmaps.Free;
    FBitmaps := nil;
  end;

  if Assigned(FShadings) then
  begin
    FShadings.Free;
    FShadings := nil;
  end;

  if Assigned(FPatterns) then
  begin
    FPatterns.Free;
    FPatterns := nil;
  end;

  if Assigned(FXObjectList) then
  begin
    FXObjectList.Free;
    FXObjectList := nil;
  end;

  if Assigned(FFontList) then
  begin
    FFontList.Free;
    FFontList := nil;
  end;

  inherited;
end;

{ TTMSFNCGeneralPDFLibFont }

constructor TTMSFNCGeneralPDFLibFont.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: String; const AName: String; const ASize: Single; const AStyle: TFontStyles);
begin
  inherited Create(APDFLib);
  FInitializer := TTMSFNCGeneralPDFLibFontInitializer.Create(AMainInitializer, ABase, AStyle, ASize);
  FFontFile := nil;
  FFontDescriptor := nil;
  FFontDescendant := nil;
  FFontUnicode := nil;
  FBase := ABase;
  FSize := ASize;
  FName := AName;
  FStyle := AStyle;
end;

destructor TTMSFNCGeneralPDFLibFont.Destroy;
begin
  if Assigned(FInitializer) then
  begin
    FInitializer.Free;
    FInitializer := nil;
  end;

  FFontFile := nil;
  FFontDescriptor := nil;
  FFontDescendant := nil;
  FFontUnicode := nil;
  inherited;
end;

function TTMSFNCGeneralPDFLibFont.BoxAsString: string;
begin
  Result := IntToStr(Box.Left) + ' ' + IntToStr(Box.Bottom) + ' ' + IntToStr(Box.Right) + ' ' + IntToStr(Box.Top);
end;

function TTMSFNCGeneralPDFLibFont.FirstGlyph: Integer;
begin
  Result := 0;
  if (UsedCharArray.Count > 0) then
    Result := UsedCharArray[0];
end;

function TTMSFNCGeneralPDFLibFont.FirstGlyphAsString: string;
begin
  Result := IntToStr(FirstGlyph);
end;

function TTMSFNCGeneralPDFLibFont.GlyphsAndWidthsAsString: string;
var
  I: Integer;
  v: Integer;
  idx: Integer;
begin
  Result := '';
  for I := 0 to UsedCharArray.Count - 1 do
  begin
    v := UsedCharArray[I];
    idx := CharArray.IndexOf(v);
    if idx > -1 then
    begin
      {$IFDEF WEBLIB}
      if FPDFLib.EmbedFonts and not FPDFLib.FullFontEmbedding then
        Result := Result + IntToStr(GlyphIDs.IndexOf(CharWidths[idx].g)) + '['+ IntToStr(CharWidths[idx].w)  + '] '
      else
      {$ENDIF}
        Result := Result + IntToStr(CharWidths[idx].g) + '['+ IntToStr(CharWidths[idx].w)  + '] '
    end;
  end;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLibFont.LastGlyph: Integer;
begin
  Result := 0;
  if (UsedCharArray.Count > 0) then
    Result := UsedCharArray[UsedCharArray.Count - 1];
end;

function TTMSFNCGeneralPDFLibFont.LastGlyphAsString: string;
begin
  Result := IntToStr(LastGlyph);
end;

function TTMSFNCGeneralPDFLibFont.WidthsAsString: string;
var
  I: Integer;
  idx, idxc: Integer;
begin
  Result := '';
  for I := FirstGlyph to LastGlyph do
  begin
    idx := UsedCharArray.IndexOf(I);
    idxc := CharArray.IndexOf(I);
    if (idx > -1) and (idxc > -1) then
      Result := Result + IntToStr(CharWidths[idxc].w)  + ' '
    else
      Result := Result + '0 '
  end;

  Result := Trim(Result);
end;

function TTMSFNCGeneralPDFLibFont.GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
begin
  Result := Initializer.GetCharArray;
end;

function TTMSFNCGeneralPDFLibFont.GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := Initializer.GetGlyphIDs;
end;

function TTMSFNCGeneralPDFLibFont.GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
begin
  Result := Initializer.GetCharWidths;
end;

function TTMSFNCGeneralPDFLibFont.GetRefNameBase: string;
begin
  Result := 'FT';
end;

function TTMSFNCGeneralPDFLibFont.GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := Initializer.GetUsedCharArray;
end;

{ TTMSFNCGeneralPDFLibFontDescriptor }

constructor TTMSFNCGeneralPDFLibFontDescriptor.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TTMSFNCGeneralPDFLibFontDescriptor.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibFontDescendant }

constructor TTMSFNCGeneralPDFLibFontDescendant.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TTMSFNCGeneralPDFLibFontDescendant.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibFontFile }

constructor TTMSFNCGeneralPDFLibFontFile.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

procedure TTMSFNCGeneralPDFLibFontFile.InitializeFontFile;
begin
  if Assigned(FFont) then
  begin
    {$IFDEF WEBLIB}
    FFont.Initializer.FullFont := FPDFLib.FullFontEmbedding;
    {$ENDIF}
    FFont.Initializer.InitializeFontFile;
  end;
end;

destructor TTMSFNCGeneralPDFLibFontFile.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibFontUnicode }

constructor TTMSFNCGeneralPDFLibFontUnicode.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFont: TTMSFNCGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TTMSFNCGeneralPDFLibFontUnicode.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibShading }

function TTMSFNCGeneralPDFLibShading.RectAsString: string;
begin
  Result := '0 0 0 0';
  if Self is TTMSFNCGeneralPDFLibLinearGradient then
  begin
    case (Self as TTMSFNCGeneralPDFLibLinearGradient).FillOrientation of
      gfoHorizontal: Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Top)) + ' ' + IntToStr(Round(Rect.Right)) + ' ' + IntToStr(Round(Rect.Bottom - (Rect.Bottom - Rect.Top)));
      gfoVertical: Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Bottom)) + ' ' + IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Bottom - (Rect.Bottom - Rect.Top)));
    end;
  end;
end;

constructor TTMSFNCGeneralPDFLibShading.Create(const APDFLib: TTMSFNCGeneralPDFLib);
begin
  inherited Create(APDFLib);
end;

destructor TTMSFNCGeneralPDFLibShading.Destroy;
begin
  FFunction := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibShadingFunction }

constructor TTMSFNCGeneralPDFLibShadingFunction.Create(const APDFLib: TTMSFNCGeneralPDFLib);
begin
  inherited Create(APDFLib);
  FSubFunctions := TTMSFNCGeneralPDFLibShadingSubFunctions.Create;
end;

destructor TTMSFNCGeneralPDFLibShadingFunction.Destroy;
begin
  if Assigned(FSubFunctions) then
  begin
    FSubFunctions.Free;
    FSubFunctions := nil;
  end;
  inherited;
end;

function TTMSFNCGeneralPDFLibShadingFunction.SubFunctionsRef: String;
var
  x: TTMSFNCGeneralPDFLibShadingSubFunction;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  Result := '';
  if Assigned(FSubFunctions) then
  begin
    {$IFDEF WEBLIB}
    for v in FSubFunctions do
    begin
      x := TTMSFNCGeneralPDFLibShadingSubFunction(v);
    {$ELSE}
    for x in FSubFunctions do
    begin
    {$ENDIF}
      Result := Result + IntToStr(PDFLib.FXRefObjects.IndexOf(x.XRefObject)) + ' 0 R ';
    end;

    Result := Trim(Result);
  end;
end;

{ TTMSFNCGeneralPDFLibShadingSubFunction }

constructor TTMSFNCGeneralPDFLibShadingSubFunction.Create(const APDFLib: TTMSFNCGeneralPDFLib; const AFillColor: TTMSFNCGraphicsColor; const AFillColorTo: TTMSFNCGraphicsColor);
begin
  inherited Create(APDFLib);
  FFillColor := AFillColor;
  FFillColorTo := AFillColorTo;
end;

{ TTMSFNCGeneralPDFLibPattern }

destructor TTMSFNCGeneralPDFLibPattern.Destroy;
begin
  FShading := nil;
  inherited;
end;

function TTMSFNCGeneralPDFLibPattern.GetRefNameBase: string;
begin
  Result := 'PT';
end;

{ TTMSFNCGeneralPDFLibBitmap }

constructor TTMSFNCGeneralPDFLibBitmap.Create(const APDFLib: TTMSFNCGeneralPDFLib; const ABitmap: TTMSFNCBitmapHelperClass; const AStream: TMemoryStream; const AImageType: TTMSFNCPDFGraphicsLibImageType; const AQuality: Single; const ABackgroundColor: TTMSFNCGraphicsColor);
begin
  inherited Create(APDFLib);
  FBitmap := TTMSFNCBitmap.Create;
  FBitmap.Assign(ABitmap);
  FImageType := AImageType;
  FQuality := AQuality;
  FBackgroundColor := ABackgroundColor;
  FStream := TMemoryStream.Create;
  FStream.LoadFromStream(AStream);
end;

destructor TTMSFNCGeneralPDFLibBitmap.Destroy;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
    FStream := nil;
  end;

  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
  inherited;
end;

function TTMSFNCGeneralPDFLibBitmap.GetRefNameBase: String;
begin
  Result := 'IM';
end;

procedure TTMSFNCGeneralPDFLibBitmap.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TTMSFNCGeneralPDFLibBitmap.SetStream(const Value: TMemoryStream);
begin
  FStream.LoadFromStream(Value);
end;

{ TTMSFNCGeneralPDFLibContent }

destructor TTMSFNCGeneralPDFLibContent.Destroy;
begin
  FPage := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibAnnotation }

function TTMSFNCGeneralPDFLibAnnotation.RectAsString: String;
begin
  Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Top)) + ' ' + IntToStr(Round(Rect.Right)) + ' ' + IntToStr(Round(Rect.Bottom))
end;

constructor TTMSFNCGeneralPDFLibAnnotation.Create(
  const APDFLib: TTMSFNCGeneralPDFLib; const ARect: TRectF);
begin
  inherited Create(APDFLib);
  FRect := ARect;
end;

destructor TTMSFNCGeneralPDFLibAnnotation.Destroy;
begin
  FAcroFormField := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibAcroFormField }

destructor TTMSFNCGeneralPDFLibAcroFormField.Destroy;
begin
  FContent := nil;
  inherited;
end;

{ TTMSFNCGeneralPDFLibURL }

constructor TTMSFNCGeneralPDFLibURL.Create(const APDFLib: TTMSFNCGeneralPDFLib;
  const ARect: TRectF; const AURL: UnicodeString);
begin
  inherited Create(APDFLib, ARect);
  FURL := AURL;
end;

{ TTMSFNCGeneralPDFLibGoTo }

constructor TTMSFNCGeneralPDFLibGoTo.Create(const APDFLib: TTMSFNCGeneralPDFLib;
  const ARect: TRectF; const ADestination: UnicodeString);
begin
  inherited Create(APDFLib, ARect);
  FDestination := ADestination;
end;

{ TTMSFNCGeneralPDFLibLinearGradient }

constructor TTMSFNCGeneralPDFLibLinearGradient.Create(
  const APDFLib: TTMSFNCGeneralPDFLib;
  const AFillOrientation: TTMSFNCGraphicsFillOrientation);
begin
  inherited Create(APDFLib);
  FFillOrientation := AFillOrientation;
end;

{$IFDEF WEBLIB}
function TTMSFNCGeneralPDFLibPageList.GetItem(Index: Integer): TTMSFNCGeneralPDFLibPage;
begin
  Result := TTMSFNCGeneralPDFLibPage(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibPageList.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibPage);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibFonts.GetItem(Index: Integer): TTMSFNCGeneralPDFLibFont;
begin
  Result := TTMSFNCGeneralPDFLibFont(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibFonts.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibFont);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibShadings.GetItem(Index: Integer): TTMSFNCGeneralPDFLibShading;
begin
  Result := TTMSFNCGeneralPDFLibShading(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibShadings.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibShading);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibXObjects.GetItem(Index: Integer): TTMSFNCGeneralPDFLibXObject;
begin
  Result := TTMSFNCGeneralPDFLibXObject(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibXObjects.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibXObject);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibPatterns.GetItem(Index: Integer): TTMSFNCGeneralPDFLibPattern;
begin
  Result := TTMSFNCGeneralPDFLibPattern(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibPatterns.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibPattern);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibAnnotations.GetItem(Index: Integer): TTMSFNCGeneralPDFLibAnnotation;
begin
  Result := TTMSFNCGeneralPDFLibAnnotation(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibAnnotations.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibAnnotation);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibBitmaps.GetItem(Index: Integer): TTMSFNCGeneralPDFLibBitmap;
begin
  Result := TTMSFNCGeneralPDFLibBitmap(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibBitmaps.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibBitmap);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibAcroFormFields.GetItem(Index: Integer): TTMSFNCGeneralPDFLibAcroForm;
begin
  Result := TTMSFNCGeneralPDFLibAcroForm(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibAcroFormFields.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibAcroForm);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibShadingSubFunctions.GetItem(Index: Integer): TTMSFNCGeneralPDFLibShadingSubFunction;
begin
  Result := TTMSFNCGeneralPDFLibShadingSubFunction(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibShadingSubFunctions.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibShadingSubFunction);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCGeneralPDFLibXRefObjects.GetItem(Index: Integer): TTMSFNCGeneralPDFLibXRefObject;
begin
  Result := TTMSFNCGeneralPDFLibXRefObject(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibXRefObjects.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibXRefObject);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

end.

