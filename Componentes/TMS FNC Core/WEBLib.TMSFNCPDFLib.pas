{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2021                               }
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

unit WEBLib.TMSFNCPDFLib;

{$I WEBLib.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$IFNDEF MSWINDOWS}
{$IFNDEF LINUX}
{$DEFINE USENATIVE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFNDEF LCLWEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 23}
{$DEFINE UNREGISTER}
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE UNREGISTER}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE UNREGISTER}
{$ENDIF}

interface

uses
  Classes, Types
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,web, WEBLib.TMSFNCUtils
  {$ENDIF}
  ,WEBLib.TMSFNCPDFGraphicsLib, WEBLib.TMSFNCPDFCoreLibBase, WEBLib.TMSFNCCustomComponent, WEBLib.TMSFNCGraphicsTypes,
  WEBLib.TMSFNCPDFRichTextLib, WEBLib.TMSFNCTypes, WEBLib.TMSFNCBitmapContainer;

const
  TTMSFNCPDFLibDocURL = 'https://download.tmssoftware.com/doc/tmsfnccore/components/pdflibrary/';
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 4; // Build nr.

  //v1.0.0.0: First release
  //v1.0.0.1: Improved: Image drawing in combination with HTML text
  //        : Fixed: Issue with images being displayed while calculating HTML text
  //        : Fixed: Issue applying URLFont in combination with HTML text
  //v1.0.0.2: Fixed: Issue encoding text with accented characters
  //v1.0.0.3: Fixed: Issue with text break on Android
  //v1.0.0.4: Fixed: Issue with text break on Android with single-line text
  //v1.0.1.0: New: Conversion routines Millimeter <-> Pixel and Inc <-> Pixel
  //        : Improved: Center parameter to draw images centered or top-left
  //v1.0.1.1: Fixed: Issue with displaying unicode text in specific circumstances
  //v1.0.1.2: Fixed: Issue with applying bold and italic text in specific circumstances
  //v1.0.1.3: Fixed: Issues searching for correct font in specific circumstances
  //v1.0.1.4: Fixed: Issue with C++Builder XE2 finalization section
  //v1.0.1.5: Fixed: Issue with Unix PDF generation and font handling
  //v1.0.1.6: Fixed: Issue with referencing correct font after destroying unused fonts
  //v1.0.1.7: Fixed: Issue with fonts referencing in multiple pages
  //v1.0.1.8: Fixed: Issue with special characters in filename on macOS
  //v1.0.2.0: New: Added external TTF file support on Android (via Font.FileName)
  //v1.0.2.1: Improved: Added character wrapping
  //v1.0.2.2: Fixed : Issue with dotted lines
  //v1.0.2.3: Fixed : Issue retrieving correct fonts with Italic style on Linux
  //v1.0.2.4: Fixed : Issue with text in columns drawing/calculation on Android

const
  DefaultMediaBox: TRectF = (Left: 0; Top: 0; Right: 612; Bottom: 792);

resourcestring
  sTMSFNCPDFLibNoPages = 'There are no pages in this document, please add a minimum of one page to create a valid PDF document. Pages can be added by using "%s.NewPage".';

type
  TTMSFNCPDFLibPageSize = (psA0, psA1, psA2, psA3, psA4, psA5, psA6, psA7, psA8,
  psB0, psB1, psB2, psB3, psB4, psB5, psB6, psB7, psB8, psB9, psB10, psC2, psC3, psC4,
  psC5, psC6, psD0,  psSRA0, psSRA1, psSRA2, psSRA3, psSRA4, psRA0, psRA1, psRA2, psLetter,
  psLegal, psLedger, psTabloid, psExecutive, psANSIC, psANSID, psANSIE, psFoolscap,
  psSmallPost, psSheetAnd13Cap, psSheetAnd12Cap, psDemy, psLargePost, psSmallmedium,
  psMedium, psSmallRoyal, psRoyal, psImperial, psMetricCrownQuarto, psMetricCrownOctavo,
  psMetricLargeCrownQuarto, psMetricLargeCrownOctavo, psMetricDemyQuarto, psMetricDemyOctavo,
  psMetricRoyalQuarto, psMetricRoyalOctavo, psCustom);

  TTMSFNCPDFLibStandard = (pdfNone, pdfA1);

  TTMSFNCPDFLibPageOrientation = (poPortrait, poLandscape);
  TTMSFNCPDFLibPageNumber = (pnNone, pnHeader, pnFooter);

  TTMSFNCCustomPDFLib = class;

  TTMSFNCPDFLibBeforeDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFLibBeforeDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFLibBeforeDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFLibAfterDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFLibAfterDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFLibAfterDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFLibNewPageStartedEvent = procedure(Sender: TObject; APageIndex: Integer) of object;

  ITMSFNCCustomPDFLib = interface(IInterface)
  ['{69FD5F00-62C7-48D8-878A-B19F31C9537B}']
    procedure BeginDocument(FileName: String = '');
    procedure OpenDocument(FileName: String); overload;
    procedure OpenDocument({%H-}FileStream: TMemoryStream); overload;
    procedure SaveDocumentFromStream({%H-}FileStream: TMemoryStream; {%H-}FileName: String);
    procedure GetDocumentInfo;
    procedure GetPageInfo({%H-}PageIndex: Integer);
    procedure CloseDocument;
    procedure NewPage;
    procedure InsertPage(PageIndex: Integer);
    procedure DrawPage({%H-}PageIndex: Integer);
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    procedure SetPageOrientation(
      const Value: TTMSFNCPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetFooterMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetPageWidth(const Value: Single);
    procedure SetPageHeight(const Value: Single);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
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
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetEmbedFonts(const Value: Boolean);
    {$IFDEF WEBLIB}
    procedure SetFullFontEmbedding(const Value: Boolean);
    {$ENDIF}
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetOnBeforeDrawHeader(const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
    procedure SetOnAfterDrawHeader(const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetOnBeforeDrawPageNumber(const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
    procedure SetOnAfterDrawFooter(const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
    procedure SetOnNewPageStarted(const Value: TTMSFNCPDFLibNewPageStartedEvent);
    procedure SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
    procedure DrawHeader;
    procedure DrawFooter;
    procedure DrawPageNumber;
    procedure SetPDFGraphicsLib(const Value: ITMSFNCCustomPDFGraphicsLib);
    procedure SetPDFGraphicsExLib(const Value: ITMSFNCCustomPDFGraphicsExLib);
    procedure SetPDFInitializationLib(const Value: ITMSFNCCustomPDFInitializationLib);
    function GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
    function GetPDFStandard: TTMSFNCPDFLibStandard;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    function GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
    function GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    function GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
    function GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
    function GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    function GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    function GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
    function UnlockWithPassword({%H-}Password: String): Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function IsDocumentOpened: Boolean;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
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
    function GetEmbedFonts: Boolean;
    {$IFDEF WEBLIB}
    function GetFullFontEmbedding: Boolean;
    {$ENDIF}
    function GetFooterFont: TTMSFNCPDFGraphicsLibFont;
    function GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
    function GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    property MediaBox: TRectF read GetMediaBox write SetMediaBox;
    property TrimBox: TRectF read GetTrimBox write SetTrimBox;
    property ArtBox: TRectF read GetArtBox write SetArtBox;
    property BleedBox: TRectF read GetBleedBox write SetBleedBox;
    property CropBox: TRectF read GetCropBox write SetCropBox;
    property ModificationDate: string read GetModificationDate;
    property Producer: String read GetProducer;
    property CreationDate: String read GetCreationDate;
    property EmbedFonts: Boolean read GetEmbedFonts write SetEmbedFonts;
    {$IFDEF WEBLIB}
    property FullFontEmbedding: Boolean read GetFullFontEmbedding write SetFullFontEmbedding;
    {$ENDIF}
    property PageSize: TTMSFNCPDFLibPageSize read GetPageSize write SetPageSize;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageOrientation: TTMSFNCPDFLibPageOrientation read GetPageOrientation write SetPageOrientation;
    property Author: String read GetAuthor write SetAuthor;
    property Creator: String read GetCreator write SetCreator;
    property PageNumber: TTMSFNCPDFLibPageNumber read GetPageNumber write SetPageNumber;
    property PageNumberFormat: UnicodeString read GetPageNumberFormat write SetPageNumberFormat;
    property PageNumberSize: Single read GetPageNumberSize write SetPageNumberSize;
    property PageNumberMargins: TTMSFNCMargins read GetPageNumberMargins write SetPageNumberMargins;
    property PageNumberAlignment: TTMSFNCGraphicsTextAlign read GetPageNumberAlignment write SetPageNumberAlignment;
    property PageNumberFont: TTMSFNCPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property Header: UnicodeString read GetHeader write SetHeader;
    property HeaderSize: Single read GetHeaderSize write SetHeaderSize;
    property HeaderMargins: TTMSFNCMargins read GetHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TTMSFNCGraphicsTextAlign read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderFont: TTMSFNCPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property Footer: UnicodeString read GetFooter write SetFooter;
    property FooterSize: Single read GetFooterSize write SetFooterSize;
    property FooterMargins: TTMSFNCMargins read GetFooterMargins write SetFooterMargins;
    property FooterAlignment: TTMSFNCGraphicsTextAlign read GetFooterAlignment write SetFooterAlignment;
    property FooterFont: TTMSFNCPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read GetTitle write SetTitle;
    property OwnerPassword: String read GetOwnerPassword write SetOwnerPassword;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property AllowsPrinting: Boolean read GetAllowsPrinting write SetAllowsPrinting;
    property AllowsCopying: Boolean read GetAllowsCopying write SetAllowsCopying;
    property Subject: String read GetSubject write SetSubject;
    property Keywords: TStrings read GetKeywords write SetKeywords;
    property FontFallBackList: TStrings read GetFontFallBackList write SetFontFallBackList;
    property OnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
    property Graphics: ITMSFNCCustomPDFGraphicsLib read GetPDFGraphicsLib write SetPDFGraphicsLib;
    property GraphicsEx: ITMSFNCCustomPDFGraphicsExLib read GetPDFGraphicsExLib write SetPDFGraphicsExLib;
    property &Initialization: ITMSFNCCustomPDFInitializationLib read GetPDFInitializationLib write SetPDFInitializationLib;
    property PDFStandard: TTMSFNCPDFLibStandard read GetPDFStandard write SetPDFStandard;
  end;

  ITMSFNCPDFLibService = interface(IInterface)
  ['{017EC71B-91BA-4A92-B3B2-67724061A21F}']
    function CreatePDFLib: ITMSFNCCustomPDFLib;
  end;

  ITMSFNCPDFLibGeneralService = interface(IInterface)
  ['{7434C5E1-A00F-4592-B34F-46756818C122}']
    function CreatePDFLib: ITMSFNCCustomPDFLib;
  end;

  { TTMSFNCCustomPDFLib }

  TTMSFNCCustomPDFLib = class(TTMSFNCCustomComponent)
  private
    FPDFLib: ITMSFNCCustomPDFLib;
    FPDFGraphicsLib: TTMSFNCPDFGraphicsLib;
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    procedure SetPageOrientation(const Value: TTMSFNCPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetFooterMargins(const Value: TTMSFNCMargins);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
    procedure SetFooter(const Value: UnicodeString);
    procedure SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeader(const Value: UnicodeString);
    procedure SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetHeaderSize(const Value: Single);
    procedure SetKeywords(const Value: TStrings);
    procedure SetOwnerPassword(const Value: String);
    procedure SetSubject(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetUserPassword(const Value: String);
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetEmbedFonts(const Value: Boolean);
    {$IFDEF WEBLIB}
    procedure SetFullFontEmbedding(const Value: Boolean);
    {$ENDIF}
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetOnAfterDrawFooter(const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
    procedure SetOnAfterDrawHeader(const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
    procedure SetOnBeforeDrawHeader(const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
    procedure SetOnBeforeDrawPageNumber(const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnNewPageStarted(const Value: TTMSFNCPDFLibNewPageStartedEvent);
    function GetVersionNr: Integer;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
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
    function GetKeywords: TStrings;
    function GetOwnerPassword: String;
    function GetPageOrientation: TTMSFNCPDFLibPageOrientation;
    function GetPageSize: TTMSFNCPDFLibPageSize;
    function GetSubject: String;
    function GetTitle: String;
    function GetUserPassword: String;
    function GetEmbedFonts: Boolean;
    {$IFDEF WEBLIB}
    function GetFullFontEmbedding: Boolean;
    {$ENDIF}
    function GetFontFallBackList: TStrings;
    function GetFooterFont: TTMSFNCPDFGraphicsLibFont;
    function GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
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
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    function GetPageNumber: TTMSFNCPDFLibPageNumber;
    function GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    function GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    function GetPageNumberFormat: UnicodeString;
    function GetPageNumberMargins: TTMSFNCMargins;
    function GetPageNumberSize: Single;
    procedure SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
    procedure SetPageNumberAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumberSize(const Value: Single);
    function GetPDFStandard: TTMSFNCPDFLibStandard;
    procedure SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
  protected
    procedure Initialize(AUseNativePDFImplementation: Boolean); virtual;
    procedure DoNotifyNewPage(Sender: TObject);
    procedure SaveDocumentFromStream(FileStream: TMemoryStream; FileName: String);
    procedure GetDocumentInfo;
    procedure InsertPage(PageIndex: Integer);
    procedure OpenDocument(FileName: String); overload;
    procedure OpenDocument(FileStream: TMemoryStream); overload;
    procedure GetPageInfo(PageIndex: Integer);
    procedure CloseDocument;
    procedure DrawPage(PageIndex: Integer);
    procedure DrawHeader;
    procedure DrawFooter;
    {$IFDEF FREEWARE}
    procedure DrawTrial;
    {$ENDIF}
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function RichText: ITMSFNCCustomPDFRichTextLib;
    function UnlockWithPassword(Password: String): Boolean;
    function IsDocumentOpened: Boolean;
    function &Initialization: ITMSFNCCustomPDFInitializationLib;
    function PDFLib: ITMSFNCCustomPDFLib;
    function GetVersion: string; override;
    property ModificationDate: string read GetModificationDate;
    property Producer: String read GetProducer;
    property CreationDate: String read GetCreationDate;
    property OwnerPassword: String read GetOwnerPassword write SetOwnerPassword;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property AllowsPrinting: Boolean read GetAllowsPrinting write SetAllowsPrinting default True;
    property AllowsCopying: Boolean read GetAllowsCopying write SetAllowsCopying default True;
    property PDFStandard: TTMSFNCPDFLibStandard read GetPDFStandard write SetPDFStandard default pdfNone;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure BeginDocument(FileName: String = '');
    procedure NewPage;
    function Graphics: ITMSFNCCustomPDFGraphicsLib;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
    function GetHeaderRect: TRectF;
    function GetFooterRect: TRectF;
    function GetPageNumberRect: TRectF;
    property MediaBox: TRectF read GetMediaBox write SetMediaBox;
    property TrimBox: TRectF read GetTrimBox write SetTrimBox;
    property ArtBox: TRectF read GetArtBox write SetArtBox;
    property BleedBox: TRectF read GetBleedBox write SetBleedBox;
    property CropBox: TRectF read GetCropBox write SetCropBox;
    property EmbedFonts: Boolean read GetEmbedFonts write SetEmbedFonts;
    {$IFDEF WEBLIB}
    property FullFontEmbedding: Boolean read GetFullFontEmbedding write SetFullFontEmbedding;
    {$ENDIF}
    property Version: String read GetVersion;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageSize: TTMSFNCPDFLibPageSize read GetPageSize write SetPageSize default psLetter;
    property PageOrientation: TTMSFNCPDFLibPageOrientation read GetPageOrientation write SetPageOrientation default poPortrait;
    property Author: String read GetAuthor write SetAuthor;
    property Creator: String read GetCreator write SetCreator;
    property Header: UnicodeString read GetHeader write SetHeader;
    property HeaderSize: Single read GetHeaderSize write SetHeaderSize;
    property HeaderMargins: TTMSFNCMargins read GetHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TTMSFNCGraphicsTextAlign read GetHeaderAlignment write SetHeaderAlignment default gtaCenter;
    property HeaderFont: TTMSFNCPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property PageNumber: TTMSFNCPDFLibPageNumber read GetPageNumber write SetPageNumber;
    property PageNumberFormat: UnicodeString read GetPageNumberFormat write SetPageNumberFormat;
    property PageNumberSize: Single read GetPageNumberSize write SetPageNumberSize;
    property PageNumberMargins: TTMSFNCMargins read GetPageNumberMargins write SetPageNumberMargins;
    property PageNumberAlignment: TTMSFNCGraphicsTextAlign read GetPageNumberAlignment write SetPageNumberAlignment default gtaCenter;
    property PageNumberFont: TTMSFNCPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property Footer: UnicodeString read GetFooter write SetFooter;
    property FooterSize: Single read GetFooterSize write SetFooterSize;
    property FooterMargins: TTMSFNCMargins read GetFooterMargins write SetFooterMargins;
    property FooterAlignment: TTMSFNCGraphicsTextAlign read GetFooterAlignment write SetFooterAlignment default gtaCenter;
    property FooterFont: TTMSFNCPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read GetTitle write SetTitle;
    property Subject: String read GetSubject write SetSubject;
    property Keywords: TStrings read GetKeywords write SetKeywords;
    property FontFallBackList: TStrings read GetFontFallBackList write SetFontFallBackList;
    property OnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPDFLibList = class(TList)
  private
    function GetItem(Index: Integer): ITMSFNCCustomPDFLib;
    procedure SetItem(Index: Integer; const Value: ITMSFNCCustomPDFLib);
  public
    property Items[Index: Integer]: ITMSFNCCustomPDFLib read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPDFLibList = class(TList<ITMSFNCCustomPDFLib>);
  {$ENDIF}

  TTMSFNCPDFLibFactoryService = class(TInterfacedObject, ITMSFNCPDFLibService, ITMSFNCPDFLibGeneralService)
  private
    FPDFLibs: TTMSFNCPDFLibList;
  protected
    function DoCreatePDFLib: ITMSFNCCustomPDFLib; virtual; abstract;
    function CreatePDFLib: ITMSFNCCustomPDFLib;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCPDFLib = class(TTMSFNCCustomPDFLib)
  public
    property Version;
  end;

function InchToPixel(AInch: Single; ADPI: Single = 72.0): Single;
function InchPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
function InchRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
function MillimeterToPixel(AMillimeter: Single; ADPI: Single = 72.0): Single;
function MillimeterPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
function MillimeterRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
function PixelToMillimeter(APixel: Single; ADPI: Single = 72.0): Single;
function PixelToInch(APixel: Single; ADPI: Single = 72.0): Single;
function PointToMillimeterPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
function PointToInchPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
function RectToMillimeterRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
function RectToInchRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;

{$IFDEF WEBLIB}
type
  TTMSFNCPDFLibFontCacheItem = class
  private
    FData: TTMSFNCUtilsFile;
    FBlob: TJSBlob;
    FName: string;
    FLoaded: Boolean;
    FURL: string;
  protected
    procedure DoFontFileLoaded(const AFile: TTMSFNCUtilsFile);
  public
    procedure Load;
    property Loaded: Boolean read FLoaded;
    property Data: TTMSFNCUtilsFile read FData;
    property URL: string read FURL;
    property Blob: TJSBlob read FBlob;
  end;

procedure AddFontToCache(AFontURL: string); overload;
procedure AddFontToCache(AFontFile: TJSBlob); overload;
function GetFontCacheCount: Integer;
function GetFontCacheItem(AIndex: Integer): TTMSFNCPDFLibFontCacheItem;
{$ENDIF}

implementation

uses
  {%H-}Math,
{$IFDEF WEBLIB}
  WEBLib.Forms,
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  WEBLib.TMSFNCPDFLib.iOS,
{$ELSE}
  WEBLib.TMSFNCPDFLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  WEBLib.TMSFNCPDFLib.Android,
{$ENDIF}
  WEBLib.TMSFNCPDFLib.General,
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,Contnrs
  {$ENDIF}
  ;

{$R 'TMSFNCPDFLib.res'}

{$IFDEF WEBLIB}
type
  TTMSFNCPDFLibFontCache = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCPDFLibFontCacheItem;
    procedure SetItem(Index: Integer; const Value: TTMSFNCPDFLibFontCacheItem);
  public
    property Items[Index: Integer]: TTMSFNCPDFLibFontCacheItem read GetItem write SetItem; default;
  end;

var
  FFontCache: TTMSFNCPDFLibFontCache;

procedure TTMSFNCPDFLibFontCacheItem.DoFontFileLoaded(const AFile: TTMSFNCUtilsFile);
var
  I: Integer;
begin
  FData := AFile;
  FLoaded := True;
  for I := 0 to FFontCache.Count - 1 do
  begin
    if not FFontCache[I].FLoaded then
      Exit;
  end;

  if Assigned(Application.OnFontCacheReady) then
    Application.OnFontCacheReady(Application);
end;

procedure TTMSFNCPDFLibFontCacheItem.Load;
var
  u: string;
begin
  if Assigned(Blob) then
    TTMSFNCUtils.LoadFile(Blob, @DoFontFileLoaded)
  else
  begin
    u := URL;
    asm
      var me = this;
      var xhr = new XMLHttpRequest();
      xhr.open('GET', u, true);
      xhr.responseType = 'blob';
      xhr.onload = function(e) {
        if (this.status == 200) {
          pas["WEBLib.TMSFNCUtils"].TTMSFNCUtils.LoadFile(this.response,rtl.createCallback(me,"DoFontFileLoaded"));
        }
      };
      xhr.send();
    end;
  end;
end;

procedure AddFontToCache(AFontURL: string);
var
  fci: TTMSFNCPDFLibFontCacheItem;
begin
  if not Assigned(FFontCache) then
    FFontCache := TTMSFNCPDFLibFontCache.Create;

  fci := TTMSFNCPDFLibFontCacheItem.Create;
  fci.FURL := AFontURL;
  fci.Load;
  FFontCache.Add(fci);
end;

procedure AddFontToCache(AFontFile: TJSBlob);
var
  fci: TTMSFNCPDFLibFontCacheItem;
begin
  if not Assigned(FFontCache) then
    FFontCache := TTMSFNCPDFLibFontCache.Create;

  fci := TTMSFNCPDFLibFontCacheItem.Create;
  fci.FBlob := AFontFile;
  fci.Load;
  FFontCache.Add(fci);
end;

function GetFontCacheItem(AIndex: Integer): TTMSFNCPDFLibFontCacheItem;
begin
  Result := nil;
  if Assigned(FFontCache) and (AIndex >= 0) and (AIndex <= FFontCache.Count - 1) then
    Result := FFontCache[AIndex];
end;

function GetFontCacheCount: Integer;
begin
  Result := 0;
  if Assigned(FFontCache) then
    Result := FFontCache.Count;
end;

{$ENDIF}

function MillimeterToPixel(AMillimeter: Single; ADPI: Single = 72.0): Single;
begin
  Result := (AMillimeter * ADPI) / 25.4;
end;

function PixelToMillimeter(APixel: Single; ADPI: Single = 72.0): Single;
begin
  Result := (APixel * 25.4) / ADPI;
end;

function InchToPixel(AInch: Single; ADPI: Single = 72.0): Single;
begin
  Result := AInch * ADPI;
end;

function PixelToInch(APixel: Single; ADPI: Single = 72.0): Single;
begin
  Result := APixel / ADPI;
end;

function MillimeterPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(MillimeterToPixel(AX, ADPI), MillimeterToPixel(AY, ADPI));
end;

function MillimeterRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(MillimeterToPixel(AX, ADPI), MillimeterToPixel(AY, ADPI), MillimeterToPixel(AX + AWidth, ADPI), MillimeterToPixel(AY + AHeight, ADPI));
end;

function InchPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(InchToPixel(AX, ADPI), InchToPixel(AY, ADPI));
end;

function InchRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(InchToPixel(AX, ADPI), InchToPixel(AY, ADPI), InchToPixel(AX + AWidth, ADPI), InchToPixel(AY + AHeight, ADPI));
end;

function RectToMillimeterRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(PixelToMillimeter(ARect.Left, ADPI), PixelToMillimeter(ARect.Top, ADPI), PixelToMillimeter(ARect.Right - ARect.Left, ADPI), PixelToMillimeter(ARect.Bottom - ARect.Top, ADPI));
end;

function RectToInchRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(PixelToInch(ARect.Left, ADPI), PixelToInch(ARect.Top, ADPI), PixelToInch(ARect.Right - ARect.Left, ADPI), PixelToInch(ARect.Bottom - ARect.Top, ADPI));
end;

function PointToMillimeterPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(PixelToMillimeter(APoint.X, ADPI), PixelToMillimeter(APoint.X, ADPI));
end;

function PointToInchPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
begin
  Result := InchPoint(APoint.X, APoint.Y, ADPI);
end;

function Hiword(L: DWORD): integer;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): Integer;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1,b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1,i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

{ TTMSFNCCustomPDFLib }

function TTMSFNCCustomPDFLib.GetAllowsCopying: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.AllowsCopying;
end;

function TTMSFNCCustomPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.AllowsPrinting;
end;

function TTMSFNCCustomPDFLib.GetArtBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.ArtBox;
end;

function TTMSFNCCustomPDFLib.GetAuthor: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Author;
end;

function TTMSFNCCustomPDFLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.BitmapContainer;
end;

function TTMSFNCCustomPDFLib.GetBleedBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.BleedBox;
end;

function TTMSFNCCustomPDFLib.GetCreationDate: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.CreationDate;
end;

function TTMSFNCCustomPDFLib.GetCreator: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Creator;
end;

function TTMSFNCCustomPDFLib.GetCropBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.CropBox;
end;

function TTMSFNCCustomPDFLib.GetPageHeight: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox.Bottom - FPDFLib.MediaBox.Top;
end;

function TTMSFNCCustomPDFLib.GetPageWidth: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox.Right - FPDFLib.MediaBox.Left;
end;

function TTMSFNCCustomPDFLib.GetPDFStandard: TTMSFNCPDFLibStandard;
begin
  Result := pdfNone;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PDFStandard;
end;

procedure TTMSFNCCustomPDFLib.GetDocumentInfo;
begin
  if Assigned(FPDFLib) then
    FPDFLib.GetDocumentInfo;
end;

function TTMSFNCCustomPDFLib.GetDocURL: string;
begin
  Result := TTMSFNCPDFLibDocURL;
end;

function TTMSFNCCustomPDFLib.GetEmbedFonts: Boolean;
begin
  Result := True;
  if Assigned(FPDFLib) then
    Result := FPDFLib.EmbedFonts;
end;

{$IFDEF WEBLIB}
function TTMSFNCCustomPDFLib.GetFullFontEmbedding: Boolean;
begin
  Result := True;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FullFontEmbedding;
end;
{$ENDIF}

function TTMSFNCCustomPDFLib.GetFontFallBackList: TStrings;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FontFallBackList;
end;

function TTMSFNCCustomPDFLib.GetFooter: UnicodeString;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Footer;
end;

function TTMSFNCCustomPDFLib.GetFooterAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterAlignment;
end;

function TTMSFNCCustomPDFLib.GetFooterFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterFont;
end;

function TTMSFNCCustomPDFLib.GetFooterMargins: TTMSFNCMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterMargins;
end;

function TTMSFNCCustomPDFLib.GetFooterRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetFooterRect;
end;

function TTMSFNCCustomPDFLib.GetFooterSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterSize;
end;

function TTMSFNCCustomPDFLib.GetHeader: UnicodeString;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.Header;
end;

function TTMSFNCCustomPDFLib.GetHeaderAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderAlignment;
end;

function TTMSFNCCustomPDFLib.GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderFont;
end;

function TTMSFNCCustomPDFLib.GetHeaderMargins: TTMSFNCMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderMargins;
end;

function TTMSFNCCustomPDFLib.GetHeaderRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetHeaderRect;
end;

function TTMSFNCCustomPDFLib.GetHeaderSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderSize;
end;

function TTMSFNCCustomPDFLib.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomPDFLib.GetPageNumber: TTMSFNCPDFLibPageNumber;
begin
  Result := pnNone;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumber;
end;

function TTMSFNCCustomPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberFormat;
end;

function TTMSFNCCustomPDFLib.GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberAlignment;
end;

function TTMSFNCCustomPDFLib.GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberFont;
end;

function TTMSFNCCustomPDFLib.GetPageNumberMargins: TTMSFNCMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberMargins;
end;

function TTMSFNCCustomPDFLib.GetPageNumberRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageNumberRect;
end;

function TTMSFNCCustomPDFLib.GetPageNumberSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberSize;
end;

function TTMSFNCCustomPDFLib.GetKeywords: TStrings;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.Keywords;
end;

function TTMSFNCCustomPDFLib.GetMediaBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox;
end;

function TTMSFNCCustomPDFLib.GetModificationDate: string;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.ModificationDate;
end;

function TTMSFNCCustomPDFLib.GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawFooter;
end;

function TTMSFNCCustomPDFLib.GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawHeader;
end;

function TTMSFNCCustomPDFLib.GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawPageNumber;
end;

function TTMSFNCCustomPDFLib.GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawFooter;
end;

function TTMSFNCCustomPDFLib.GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawHeader;
end;

function TTMSFNCCustomPDFLib.GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawPageNumber;
end;

function TTMSFNCCustomPDFLib.GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnNewPageStarted;
end;

function TTMSFNCCustomPDFLib.GetOwnerPassword: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.OwnerPassword;
end;

function TTMSFNCCustomPDFLib.GetPageCount: Integer;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageCount;
end;

function TTMSFNCCustomPDFLib.GetPageIndex: Integer;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageIndex;
end;

procedure TTMSFNCCustomPDFLib.GetPageInfo(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.GetPageInfo(PageIndex);
end;

function TTMSFNCCustomPDFLib.GetPageOrientation: TTMSFNCPDFLibPageOrientation;
begin
  Result := poPortrait;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageOrientation;
end;

function TTMSFNCCustomPDFLib.GetPageSize: TTMSFNCPDFLibPageSize;
begin
  Result := psA4;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageSize;
end;

function TTMSFNCCustomPDFLib.GetProducer: String;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.Producer;
end;

function TTMSFNCCustomPDFLib.GetSubject: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Subject;
end;

function TTMSFNCCustomPDFLib.GetTitle: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Title;
end;

function TTMSFNCCustomPDFLib.GetTrimBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.TrimBox;
end;

function TTMSFNCCustomPDFLib.GetUserPassword: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.UserPassword;
end;

function TTMSFNCCustomPDFLib.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TTMSFNCCustomPDFLib.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TTMSFNCCustomPDFLib.&Initialization: ITMSFNCCustomPDFInitializationLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFInitializationLib;
end;

procedure TTMSFNCCustomPDFLib.BeginDocument(FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.BeginDocument(FileName);
end;

procedure TTMSFNCCustomPDFLib.CloseDocument;
begin
  if Assigned(FPDFLib) then
    FPDFLib.CloseDocument;
end;

constructor TTMSFNCCustomPDFLib.Create(AOwner: TComponent);
begin
  inherited;
  Initialize(True);
end;

constructor TTMSFNCCustomPDFLib.Create;
begin
  Create(nil);
end;

procedure TTMSFNCCustomPDFLib.Initialize(AUseNativePDFImplementation: Boolean);
var
  PDFLibServiceGeneral: ITMSFNCPDFLibGeneralService;
  {$IFDEF USENATIVE}
  PDFLibService: ITMSFNCPDFLibService;
  {$ENDIF}
  g: ITMSFNCCustomPDFInitializationLib;
begin
  {$IFDEF USENATIVE}
  if AUseNativePDFImplementation then
  begin
    if TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFLibService, IInterface(PDFLibService)) then
      FPDFLib := PDFLibService.CreatePDFLib;
  end
  else
  {$ENDIF}
  begin
    if TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFLibGeneralService, IInterface(PDFLibServiceGeneral)) then
      FPDFLib := PDFLibServiceGeneral.CreatePDFLib;
  end;

  FPDFGraphicsLib := TTMSFNCPDFGraphicsLib.Create(AUseNativePDFImplementation);
  FPDFLib.SetPDFGraphicsLib(FPDFGraphicsLib.GetPDFGraphicsLib);
  FPDFLib.SetPDFGraphicsExLib(FPDFGraphicsLib.GetPDFGraphicsExLib);
  FPDFLib.SetPDFInitializationLib(FPDFGraphicsLib.GetPDFInitializationLib);
  FPDFLib.&Initialization.SetPDFLib(FPDFLib);
  FPDFLib._Release;

  g := &Initialization;
  if Assigned(g) then
    g.OnNotifyNewPage := DoNotifyNewPage;
end;

function TTMSFNCCustomPDFLib.Graphics: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFGraphicsLib;
end;

function TTMSFNCCustomPDFLib.IsDocumentOpened: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.IsDocumentOpened;
end;

procedure TTMSFNCCustomPDFLib.InsertPage(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
  begin
    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    FPDFLib.InsertPage(PageIndex);
  end;
end;

procedure TTMSFNCCustomPDFLib.NewPage;
begin
  if Assigned(FPDFLib) then
  begin
    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    FPDFLib.NewPage;
  end;
end;

procedure TTMSFNCCustomPDFLib.OpenDocument(FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OpenDocument(FileName);
end;

procedure TTMSFNCCustomPDFLib.OpenDocument(FileStream: TMemoryStream);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OpenDocument(FileStream);
end;

function TTMSFNCCustomPDFLib.PDFLib: ITMSFNCCustomPDFLib;
begin
  Result := FPDFLib;
end;

function TTMSFNCCustomPDFLib.RichText: ITMSFNCCustomPDFRichTextLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFGraphicsExLib.RichText;
end;

destructor TTMSFNCCustomPDFLib.Destroy;
begin
  if Assigned(FPDFLib) then
    FPDFLib := nil;

  if Assigned(FPDFGraphicsLib) then
  begin
    FPDFGraphicsLib.Free;
    FPDFGraphicsLib := nil;
  end;
  inherited;
end;

procedure TTMSFNCCustomPDFLib.DoNotifyNewPage(Sender: TObject);
begin
  NewPage;
end;

procedure TTMSFNCCustomPDFLib.DrawFooter;
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawFooter;
end;

procedure TTMSFNCCustomPDFLib.DrawHeader;
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawHeader;
end;

procedure TTMSFNCCustomPDFLib.DrawPage(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawPage(PageIndex);
end;

{$IFDEF FREEWARE}
procedure TTMSFNCCustomPDFLib.DrawTrial;
var
  f: TTMSFNCPDFGraphicsLibFont;
begin
  if GetPageCount > 0 then
  begin
    f := TTMSFNCPDFGraphicsLibFont.Create;
    try
      f.Assign(Graphics.Font);
      Graphics.Font.BeginUpdate;
      Graphics.Font.Name := DefaultFontName;
      Graphics.Font.Size := 8;
      Graphics.Font.Style := [];
      Graphics.Font.Color := gcRed;
      Graphics.Font.EndUpdate;
      Graphics.DrawText(ClassName + ' TRIAL VERSION ' + Version, PointF(0, 0));
    finally
      f.Free;
    end;
  end;
end;
{$ENDIF}

function TTMSFNCCustomPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
begin
  Result := nil;
  if Assigned(FPDFLib) then
  begin
    if FPDFLib.GetPageCount = 0 then
      raise Exception.Create(Format(sTMSFNCPDFLibNoPages, [ClassName]));

    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    Result := FPDFLib.EndDocument(AOpenInPDFReader);
  end;
end;

procedure TTMSFNCCustomPDFLib.SaveDocumentFromStream(FileStream: TMemoryStream;
  FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.SaveDocumentFromStream(FileStream, FileName);
end;

procedure TTMSFNCCustomPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.AllowsCopying := Value;
end;

procedure TTMSFNCCustomPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.AllowsPrinting := Value;
end;

procedure TTMSFNCCustomPDFLib.SetArtBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.ArtBox := Value;
end;

procedure TTMSFNCCustomPDFLib.SetAuthor(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Author := Value;
end;

procedure TTMSFNCCustomPDFLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.BitmapContainer := Value;
end;

procedure TTMSFNCCustomPDFLib.SetBleedBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.BleedBox := Value;
end;

procedure TTMSFNCCustomPDFLib.SetCreator(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Creator := Value;
end;

procedure TTMSFNCCustomPDFLib.SetCropBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.CropBox := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageHeight(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageHeight := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumber := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberFormat := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageNumberAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberAlignment := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageNumberFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberFont.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberMargins.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetPageNumberSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberSize := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageWidth(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageWidth := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PDFStandard := Value;
end;

procedure TTMSFNCCustomPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.EmbedFonts := Value;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCCustomPDFLib.SetFullFontEmbedding(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FullFontEmbedding := Value;
end;
{$ENDIF}

procedure TTMSFNCCustomPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FontFallBackList.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetFooter(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Footer := Value;
end;

procedure TTMSFNCCustomPDFLib.SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterAlignment := Value;
end;

procedure TTMSFNCCustomPDFLib.SetFooterFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterFont.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterMargins.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetFooterSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterSize := Value;
end;

procedure TTMSFNCCustomPDFLib.SetHeader(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Header := Value;
end;

procedure TTMSFNCCustomPDFLib.SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderAlignment := Value;
end;

procedure TTMSFNCCustomPDFLib.SetHeaderFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderFont.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderMargins.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetHeaderSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderSize := Value;
end;

procedure TTMSFNCCustomPDFLib.SetKeywords(const Value: TStrings);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Keywords.Assign(Value);
end;

procedure TTMSFNCCustomPDFLib.SetMediaBox(const Value: TRectF);
var
  g: ITMSFNCCustomPDFInitializationLib;
begin
  if Assigned(FPDFLib) then
    FPDFLib.MediaBox := Value;

  g := &Initialization;
  if Assigned(g) then
  begin
    g.SetPageWidth(MediaBox.Right - MediaBox.Left);
    g.SetPageHeight(MediaBox.Bottom - MediaBox.Top);
  end;
end;

procedure TTMSFNCCustomPDFLib.SetOnAfterDrawFooter(
  const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawFooter := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnAfterDrawHeader(
  const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawHeader := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnAfterDrawPageNumber(
  const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawPageNumber := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnBeforeDrawFooter(
  const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawFooter := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnBeforeDrawHeader(
  const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawHeader := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawPageNumber := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOnNewPageStarted(
  const Value: TTMSFNCPDFLibNewPageStartedEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnNewPageStarted := Value;
end;

procedure TTMSFNCCustomPDFLib.SetOwnerPassword(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OwnerPassword := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageOrientation(
  const Value: TTMSFNCPDFLibPageOrientation);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageOrientation := Value;
end;

procedure TTMSFNCCustomPDFLib.SetPageSize(const Value: TTMSFNCPDFLibPageSize);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageSize := Value;
end;

procedure TTMSFNCCustomPDFLib.SetSubject(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Subject := Value;
end;

procedure TTMSFNCCustomPDFLib.SetTitle(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Title := Value;
end;

procedure TTMSFNCCustomPDFLib.SetTrimBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.TrimBox := Value;
end;

procedure TTMSFNCCustomPDFLib.SetUserPassword(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.UserPassword := Value;
end;

function TTMSFNCCustomPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := FPDFLib.UnlockWithPassword(Password);
end;

{ TTMSFNCPDFLibFactoryService }

constructor TTMSFNCPDFLibFactoryService.Create;
begin
  inherited Create;
  FPDFLibs := TTMSFNCPDFLibList.Create;
end;

function TTMSFNCPDFLibFactoryService.CreatePDFLib: ITMSFNCCustomPDFLib;
begin
  Result := DoCreatePDFLib;
  FPDFLibs.Add(Result);
end;

destructor TTMSFNCPDFLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFLibs);
  inherited Destroy;
end;

{$IFDEF WEBLIB}
function TTMSFNCPDFLibList.GetItem(Index: Integer): ITMSFNCCustomPDFLib;
begin
  Result := ITMSFNCCustomPDFLib(inherited Items[Index]);
end;

procedure TTMSFNCPDFLibList.SetItem(Index: Integer; const Value: ITMSFNCCustomPDFLib);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCPDFLibFontCache.GetItem(Index: Integer): TTMSFNCPDFLibFontCacheItem;
begin
  Result := TTMSFNCPDFLibFontCacheItem(inherited Items[Index]);
end;

procedure TTMSFNCPDFLibFontCache.SetItem(Index: Integer; const Value: TTMSFNCPDFLibFontCacheItem);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

initialization
begin
  {$IFDEF USENATIVE}
  RegisterPDFLibService;
  {$ENDIF}
  RegisterPDFLibGeneralService;
  {$IFDEF WEBLIB}
  {$ENDIF}
end;

{$IFNDEF WEBLIB}
finalization
begin
  {$IFDEF UNREGISTER}
  UnRegisterPDFLibGeneralService;
  {$IFDEF USENATIVE}
  UnRegisterPDFLibService;
  {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}

end.
