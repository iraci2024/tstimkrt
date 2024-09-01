{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2021                               }
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

unit FMX.TMSFNCPDFLib.Android;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFLibService;
procedure UnRegisterPDFLibService;

implementation

uses
  Classes, Types, SysUtils, FMX.TMSFNCPDFGraphicsLib, FMX.TMSFNCUtils
  ,FMX.TMSFNCPDFLib, FMX.TMSFNCGraphics, FMX.TMSFNCBitmapContainer, FMX.TMSFNCGraphicsTypes
  {$IFDEF ANDROID}
  ,AndroidApi.JNIBridge, AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.JavaTypes, AndroidApi.Helpers
  {$ENDIF}
  ,FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCTypes
  ;

type
  TTMSFNCAndroidPDFLibService = class;

  TTMSFNCAndroidPDFLibService = class(TTMSFNCPDFLibFactoryService)
  protected
    function DoCreatePDFLib: ITMSFNCCustomPDFLib; override;
  end;

const
  DefaultMediaBox: TRectF = (Left: 0; Top: 0; Right: 612; Bottom: 792);

type
  TTMSFNCAndroidPDFLib = class(TInterfacedObject, ITMSFNCCustomPDFLib)
  private
    FPageCount: Integer;
    FOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
    FOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    FOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
    FOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    FOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    FOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
    FOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    FPageHeight, FPageWidth: Single;
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
    FFooterFont: TTMSFNCPDFGraphicsLibFont;
    FHeaderFont: TTMSFNCPDFGraphicsLibFont;
    FPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    FHeaderSize: Single;
    FHeaderMargins: TTMSFNCMargins;
    FFooterMargins: TTMSFNCMargins;
    FFooterSize: Single;
    FFooterAlignment: TTMSFNCGraphicsTextAlign;
    FHeaderAlignment: TTMSFNCGraphicsTextAlign;
    FModificationDate: string;
    FProducer: String;
    FCreationDate: String;
    FEmbedFonts: Boolean;
    FFontFallBackList: TStrings;
    FPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    FPDFStandard: TTMSFNCPDFLibStandard;
    {$IFDEF ANDROID}
    FPDFDocument: JPdfDocument;
    FPDFDocumentPage: JPdfDocument_Page;
    FPDFFileName: String;
    {$ENDIF}
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    procedure SetPageOrientation(const Value: TTMSFNCPDFLibPageOrientation);
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
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
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
    function GetFooterFont: TTMSFNCPDFGraphicsLibFont;
    function GetHeaderFont: TTMSFNCPDFGraphicsLibFont;  
    function GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    function GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    function GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    function GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
    function GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    function GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
    function GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
    function GetPDFStandard: TTMSFNCPDFLibStandard;
    procedure SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
  protected
    constructor Create;
    destructor Destroy; override;
    procedure DoNewPageStarted(APageIndex: Integer);
    procedure DoBeforeDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure UpdateBoxRect;
    procedure FinishPage;
    procedure BeginDocument(FileName: String = '');
    procedure OpenDocument(FileName: String); overload;
    procedure OpenDocument(FileStream: TMemoryStream); overload;
    procedure SaveDocumentFromStream(FileStream: TMemoryStream; FileName: String);
    procedure GetDocumentInfo;
    procedure GetPageInfo(PageIndex: Integer);
    procedure CloseDocument;
    procedure NewPage;
    procedure InsertPage(PageIndex: Integer);
    procedure DrawPage(PageIndex: Integer);
    procedure DrawHeader;
    procedure DrawFooter;
    procedure DrawPageNumber;
    procedure SetPDFGraphicsLib(const Value: ITMSFNCCustomPDFGraphicsLib);
    procedure SetPDFGraphicsExLib(const Value: ITMSFNCCustomPDFGraphicsExLib);
    procedure SetPDFInitializationLib(const Value: ITMSFNCCustomPDFInitializationLib);
    function GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
    function UnlockWithPassword(Password: String): Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function IsDocumentOpened: Boolean;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    property MediaBox: TRectF read FMediaBox write FMediaBox;
    property TrimBox: TRectF read FTrimBox write FTrimBox;
    property ArtBox: TRectF read FArtBox write FArtBox;
    property BleedBox: TRectF read FBleedBox write FBleedBox;
    property CropBox: TRectF read FCropBox write FCropBox;
    property ModificationDate: string read FModificationDate;
    property Producer: String read FProducer;
    property EmbedFonts: Boolean read FEmbedFonts write FEmbedFonts;
    property CreationDate: String read FCreationDate;
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
  end;

var
  PDFLibService: ITMSFNCPDFLibService;

procedure RegisterPDFLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFLibService, IInterface(PDFLibService)) then
  begin
    PDFLibService := TTMSFNCAndroidPDFLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFLibService, PDFLibService);
  end;
end;

procedure UnregisterPDFLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFLibService);
end;

{ TTMSFNCAndroidPDFLibService }

function TTMSFNCAndroidPDFLibService.DoCreatePDFLib: ITMSFNCCustomPDFLib;
begin
  Result := TTMSFNCAndroidPDFLib.Create;
end;

procedure TTMSFNCAndroidPDFLib.BeginDocument(FileName: String = '');
begin
  {$IFDEF ANDROID}
  FPDFDocument := TJPdfDocument.JavaClass.init;
  FPDFFileName := FileName;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.NewPage;
{$IFDEF ANDROID}
var
  ab, mb, bb, cb, tb: TRectF;
  abt, mbt, bbt, cbt, tbt: JRect;
  pageInfo: JPdfDocument_PageInfo;
  pageInfoBldr: JPageInfo_Builder;
  pg: Integer;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  mb := MediaBox;
  mbt := TJRect.JavaClass.init(Round(mb.Left), Round(mb.Top), Round(mb.Right), Round(mb.Bottom));
  ab := ArtBox;
  abt := TJRect.JavaClass.init(Round(ab.Left), Round(ab.Top), Round(ab.Right), Round(ab.Bottom));
  bb := BleedBox;
  bbt := TJRect.JavaClass.init(Round(bb.Left), Round(bb.Top), Round(bb.Right), Round(bb.Bottom));
  cb := CropBox;
  cbt := TJRect.JavaClass.init(Round(cb.Left), Round(cb.Top), Round(cb.Right), Round(cb.Bottom));
  tb := TrimBox;
  tbt := TJRect.JavaClass.init(Round(tb.Left), Round(tb.Top), Round(tb.Right), Round(tb.Bottom));

  pg := 1;
  if Assigned(FPDFDocumentPage) then
    pg := FPDFDocumentPage.getInfo.getPageNumber + 1;


  pageInfoBldr := TJPageInfo_Builder.JavaClass.init(mbt.width, mbt.height, pg);
  pageInfoBldr.setContentRect(mbt);
  pageInfo := pageInfoBldr.create;

  FinishPage;
  FPDFDocumentPage := FPDFDocument.startPage(pageInfo);

  if Assigned(FPDFInitializationLib) then
  begin
    FPDFInitializationLib.SetCanvas((FPDFDocumentPage.getCanvas as ILocalObject).GetObjectID);
    FPDFInitializationLib.SetPageWidth(mb.Width);
    FPDFInitializationLib.SetPageHeight(mb.Height);
    FPDFInitializationLib.InitializeAppearance;
  end;

  DoNewPageStarted(GetPageIndex);

  DrawHeader;
  DrawFooter;
  DrawPageNumber;

  Inc(FPageCount);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.OpenDocument(FileStream: TMemoryStream);
begin
  if IsDocumentOpened then
    CloseDocument;

  {$IFDEF ANDROID}
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.OpenDocument(FileName: String);
begin
  if IsDocumentOpened then
    CloseDocument;

  {$IFDEF ANDROID}
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.CloseDocument;
begin
  {$IFDEF ANDROID}
  if Assigned(FPDFDocument) then
  begin
    FPDFDocument.close;
    FPDFDocument := nil;
  end;
  {$ENDIF}
end;

constructor TTMSFNCAndroidPDFLib.Create;
var
  r: TRectF;
begin
  inherited;
  FPageHeight := 0;
  FPageWidth := 0;
  FPageCount := 0;
  FFontFallBackList := TStringList.Create;
  FEmbedFonts := True;
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

destructor TTMSFNCAndroidPDFLib.Destroy;
begin
  if Assigned(FFontFallBackList) then
  begin
    FFontFallBackList.Free;
    FFontFallBackList := nil;
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

procedure TTMSFNCAndroidPDFLib.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TTMSFNCAndroidPDFLib.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TTMSFNCAndroidPDFLib.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TTMSFNCAndroidPDFLib.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCAndroidPDFLib.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCAndroidPDFLib.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCAndroidPDFLib.DoNewPageStarted(APageIndex: Integer);
begin
  if Assigned(OnNewPageStarted) then
    OnNewPageStarted(Self, APageIndex);
end;

procedure TTMSFNCAndroidPDFLib.DrawFooter;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
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
      sz := FPDFGraphicsLib.DrawHTMLText(Footer, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(Footer, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(Footer, r, True).Size;
      FPDFGraphicsLib.DrawText(Footer, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawFooter(Self, GetPageIndex, Footer, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCAndroidPDFLib.DrawHeader;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
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
      sz := FPDFGraphicsLib.DrawHTMLText(Header, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(Header, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(Header, r, True).Size;
      FPDFGraphicsLib.DrawText(Header, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawHeader(Self, GetPageIndex, Header, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCAndroidPDFLib.DrawPageNumber;
var
  al: TTMSFNCGraphicsTextAlign;
  ft: TTMSFNCPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
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
  s := Format(PageNumberFormat, [GetPageIndex + 1]);
  DoBeforeDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TTMSFNCUtils.IsHTMLUnicode(s) then
    begin
      sz := FPDFGraphicsLib.DrawHTMLText(s, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(s, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(s, r, True).Size;
      FPDFGraphicsLib.DrawText(s, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TTMSFNCAndroidPDFLib.DrawPage(PageIndex: Integer);
begin
  {$IFDEF ANDROID}
  {$ENDIF}
end;

function TTMSFNCAndroidPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
{$IFDEF ANDROID}
var
  os: JOutputStream;
  baos: JByteArrayOutputStream;
  ba: TJavaArray<Byte>;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF ANDROID}
  if Assigned(FPDFDocument) then
  begin
    FinishPage;
    if FPDFFileName <> '' then
    begin
      os := TJFileOutputStream.JavaClass.init(StringToJString(FPDFFileName));
      FPDFDocument.writeTo(os);
      if AOpenInPDFReader then
        TTMSFNCUtils.OpenFile(FPDFFileName);
    end
    else
    begin
      baos := TJByteArrayOutputStream.JavaClass.init;
      FPDFDocument.writeTo(baos);
      ba := baos.toByteArray;
      Result := TMemoryStream.Create;
      Result.WriteBuffer(ba.Data^, baos.size);
    end;
    FPDFDocument.close;
  end;
  FPageCount := 0;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.FinishPage;
begin
  {$IFDEF ANDROID}
  if Assigned(FPDFDocumentPage) then
  begin
    FPDFDocument.finishPage(FPDFDocumentPage);
    FPDFDocumentPage := nil;
  end;
  {$ENDIF}
end;

function  TTMSFNCAndroidPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := False;
  {$IFDEF ANDROID}
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.InsertPage(PageIndex: Integer);
begin

end;

function TTMSFNCAndroidPDFLib.IsDocumentOpened: Boolean;
begin
  {$IFDEF ANDROID}
  Result := Assigned(FPDFDocument);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFLib.GetAllowsCopying: Boolean;
begin
  Result := AllowsCopying;
end;

function TTMSFNCAndroidPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := AllowsPrinting;
end;

function TTMSFNCAndroidPDFLib.GetArtBox: TRectF;
begin
  Result := ArtBox;
end;

function TTMSFNCAndroidPDFLib.GetAuthor: String;
begin
  Result := Author;
end;

function TTMSFNCAndroidPDFLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.BitmapContainer;
end;

function TTMSFNCAndroidPDFLib.GetBleedBox: TRectF;
begin
  Result := BleedBox;
end;

function TTMSFNCAndroidPDFLib.GetCreationDate: String;
begin
  Result := CreationDate;
end;

function TTMSFNCAndroidPDFLib.GetCreator: String;
begin
  Result := Creator;
end;

function TTMSFNCAndroidPDFLib.GetCropBox: TRectF;
begin
  Result := CropBox;
end;

procedure TTMSFNCAndroidPDFLib.GetDocumentInfo;
{$IFDEF ANDROID}
{$ENDIF}
begin
  {$IFDEF ANDROID}
  {$ENDIF}
end;

function TTMSFNCAndroidPDFLib.GetEmbedFonts: Boolean;
begin
  Result := EmbedFonts;
end;

function TTMSFNCAndroidPDFLib.GetFontFallBackList: TStrings;
begin
  Result := FontFallBackList;
end;

function TTMSFNCAndroidPDFLib.GetFooter: UnicodeString;
begin
  Result := Footer;
end;

function TTMSFNCAndroidPDFLib.GetFooterAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := FooterAlignment;
end;

function TTMSFNCAndroidPDFLib.GetFooterFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FFooterFont;
end;

function TTMSFNCAndroidPDFLib.GetFooterMargins: TTMSFNCMargins;
begin
  Result := FooterMargins;
end;

function TTMSFNCAndroidPDFLib.GetFooterRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FFooterMargins.Left, FMediaBox.Bottom - FFooterSize - FFooterMargins.Bottom, FMediaBox.Width - FFooterMargins.Right, FMediaBox.Bottom - FFooterMargins.Bottom);
end;

function TTMSFNCAndroidPDFLib.GetFooterSize: Single;
begin
  Result := FooterSize;
end;

function TTMSFNCAndroidPDFLib.GetHeader: UnicodeString;
begin
  Result := Header;
end;

function TTMSFNCAndroidPDFLib.GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FHeaderFont;
end;

function TTMSFNCAndroidPDFLib.GetHeaderAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := HeaderAlignment;
end;

function TTMSFNCAndroidPDFLib.GetHeaderMargins: TTMSFNCMargins;
begin
  Result := HeaderMargins;
end;

function TTMSFNCAndroidPDFLib.GetHeaderRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FHeaderMargins.Left, FMediaBox.Top + FHeaderMargins.Top, FMediaBox.Width - FHeaderMargins.Right, FMediaBox.Top + FHeaderMargins.Top + FHeaderSize);
end;

function TTMSFNCAndroidPDFLib.GetHeaderSize: Single;
begin
  Result := HeaderSize;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := PageNumberFormat;
end;

function TTMSFNCAndroidPDFLib.GetPageNumber: TTMSFNCPDFLibPageNumber;
begin
  Result := PageNumber;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := PageNumberAlignment;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FPageNumberFont;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberMargins: TTMSFNCMargins;
begin
  Result := PageNumberMargins;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  case PageNumber of
    pnHeader: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Top + FPageNumberMargins.Top, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Top + FPageNumberMargins.Top + FPageNumberSize);
    pnFooter: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Bottom - FPageNumberSize - FPageNumberMargins.Bottom, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Bottom - FPageNumberMargins.Bottom);
  end;
end;

function TTMSFNCAndroidPDFLib.GetPageNumberSize: Single;
begin
  Result := PageNumberSize;
end;

function TTMSFNCAndroidPDFLib.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

function TTMSFNCAndroidPDFLib.GetMediaBox: TRectF;
begin
  Result := MediaBox;
end;

function TTMSFNCAndroidPDFLib.GetModificationDate: string;
begin
  Result := ModificationDate;
end;

function TTMSFNCAndroidPDFLib.GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
begin
  Result := FOnAfterDrawFooter;
end;

function TTMSFNCAndroidPDFLib.GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
begin
  Result := FOnAfterDrawHeader;
end;

function TTMSFNCAndroidPDFLib.GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
begin
  Result := FOnAfterDrawPageNumber;
end;

function TTMSFNCAndroidPDFLib.GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
begin
  Result := FOnBeforeDrawFooter;
end;

function TTMSFNCAndroidPDFLib.GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
begin
  Result := FOnBeforeDrawHeader
end;

function TTMSFNCAndroidPDFLib.GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
begin
  Result := FOnBeforeDrawPageNumber
end;

function TTMSFNCAndroidPDFLib.GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
begin
  Result := FOnNewPageStarted;
end;

function TTMSFNCAndroidPDFLib.GetOwnerPassword: String;
begin
  Result := OwnerPassword;
end;

function TTMSFNCAndroidPDFLib.GetPageCount: Integer;
begin
  Result := FPageCount;
end;

function TTMSFNCAndroidPDFLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCAndroidPDFLib.GetPageIndex: Integer;
begin
  Result := 0;
  {$IFDEF ANDROID}
  if Assigned(FPDFDocumentPage) then
    Result := FPDFDocumentPage.getInfo.getPageNumber - 1;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.GetPageInfo(PageIndex: Integer);
begin
  {$IFDEF ANDROID}
  {$ENDIF}
end;

function TTMSFNCAndroidPDFLib.GetPageOrientation: TTMSFNCPDFLibPageOrientation;
begin
  Result := Orientation;
end;

function TTMSFNCAndroidPDFLib.GetPageSize: TTMSFNCPDFLibPageSize;
begin
  Result := PageSize;
end;

function TTMSFNCAndroidPDFLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCAndroidPDFLib.GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TTMSFNCAndroidPDFLib.GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TTMSFNCAndroidPDFLib.GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

function TTMSFNCAndroidPDFLib.GetPDFStandard: TTMSFNCPDFLibStandard;
begin
  Result := PDFStandard;
end;

function TTMSFNCAndroidPDFLib.GetProducer: String;
begin
  Result := Producer;
end;

function TTMSFNCAndroidPDFLib.GetSubject: String;
begin
  Result := Subject;
end;

function TTMSFNCAndroidPDFLib.GetTitle: String;
begin
  Result := Title;
end;

function TTMSFNCAndroidPDFLib.GetTrimBox: TRectF;
begin
  Result := TrimBox;
end;

function TTMSFNCAndroidPDFLib.GetUserPassword: String;
begin
  Result := UserPassword;
end;

procedure TTMSFNCAndroidPDFLib.SaveDocumentFromStream(FileStream: TMemoryStream;
  FileName: String);
{$IFDEF ANDROID}
{$ENDIF}
begin
  {$IFDEF ANDROID}
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  AllowsCopying := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  AllowsPrinting := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetArtBox(const Value: TRectF);
begin
  ArtBox := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetAuthor(const Value: String);
begin
  Author := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib.BitmapContainer := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetBleedBox(const Value: TRectF);
begin
  BleedBox := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetCreator(const Value: String);
begin
  Creator := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetCropBox(const Value: TRectF);
begin
  CropBox := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  EmbedFonts := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  FontFallBackList.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetFooter(const Value: UnicodeString);
begin
  Footer := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FooterAlignment := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetFooterFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  FooterMargins.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetFooterSize(const Value: Single);
begin
  FooterSize := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPDFGraphicsExLib(
  const Value: ITMSFNCCustomPDFGraphicsExLib);
begin
  FPDFGraphicsExLib := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPDFGraphicsLib(
  const Value: ITMSFNCCustomPDFGraphicsLib);
begin
  FPDFGraphicsLib := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPDFInitializationLib(
  const Value: ITMSFNCCustomPDFInitializationLib);
begin
  FPDFInitializationLib := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPDFStandard(
  const Value: TTMSFNCPDFLibStandard);
begin
  PDFStandard := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetHeader(const Value: UnicodeString);
begin
  FHeader := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetHeaderFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  HeaderAlignment := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  HeaderMargins.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetHeaderSize(const Value: Single);
begin
  HeaderSize := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetKeywords(const Value: TStrings);
begin
  Keywords.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetMediaBox(const Value: TRectF);
begin
  MediaBox := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnAfterDrawFooter(
  const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
begin
  FOnAfterDrawFooter := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnAfterDrawHeader(
  const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
begin
  FOnAfterDrawHeader := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnAfterDrawPageNumber(
  const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
begin
  FOnAfterDrawPageNumber := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnBeforeDrawFooter(
  const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
begin
  FOnBeforeDrawFooter := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnBeforeDrawHeader(
  const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
begin
  FOnBeforeDrawHeader := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
begin
  FOnBeforeDrawPageNumber := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOnNewPageStarted(
  const Value: TTMSFNCPDFLibNewPageStartedEvent);
begin
  FOnNewPageStarted := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetOwnerPassword(const Value: String);
begin
  OwnerPassword := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumberAlignment(
  const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumberFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TTMSFNCAndroidPDFLib.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetPageOrientation(
  const Value: TTMSFNCPDFLibPageOrientation);
begin
  Orientation := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCAndroidPDFLib.SetPageSize(
  const Value: TTMSFNCPDFLibPageSize);
begin
  PageSize := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCAndroidPDFLib.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCAndroidPDFLib.SetSubject(const Value: String);
begin
  Subject := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetTitle(const Value: String);
begin
  Title := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetTrimBox(const Value: TRectF);
begin
  TrimBox := Value;
end;

procedure TTMSFNCAndroidPDFLib.SetUserPassword(const Value: String);
begin
  UserPassword := Value;
end;

procedure TTMSFNCAndroidPDFLib.UpdateBoxRect;
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
    w := FMediaBox.Width;
    h := FMediaBox.Height;
    FMediaBox.Height := w;
    FMediaBox.Width := h;
  end;

  FCropBox := FMediaBox;
  FBleedBox := FMediaBox;
  FArtBox := FMediaBox;
  FTrimBox := FMediaBox;

  g := FPDFInitializationLib;
  if Assigned(g) then
  begin
    g.SetPageWidth(MediaBox.Width);
    g.SetPageHeight(MediaBox.Height);
  end;
end;

end.

