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

unit FMX.TMSFNCPDFLib.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFLibService;
procedure UnRegisterPDFLibService;

implementation

{$IFDEF MACOS}
{$IFNDEF IOS}
{$DEFINE MACOS_OK}
{$ENDIF}
{$ENDIF}

uses
  Classes, Types, SysUtils, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCPDFLib, FMX.TMSFNCPDFGraphicsLib, FMX.TMSFNCBitmapContainer,
  FMX.TMSFNCGraphicsTypes
  {$IFDEF MACOS_OK}
  ,MacApi.CoreFoundation, MacApi.CoreGraphics, MacApi.AppKit
  ,MacApi.CocoaTypes, MacApi.Foundation, MacApi.ObjectiveC
  {$ENDIF}
  ,FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCTypes
  ;

type
  TTMSFNCMacPDFLibService = class;

  TTMSFNCMacPDFLibService = class(TTMSFNCPDFLibFactoryService)
  protected
    function DoCreatePDFLib: ITMSFNCCustomPDFLib; override;
  end;

  TTMSFNCMacPDFLib = class(TInterfacedObject, ITMSFNCCustomPDFLib)
  private
    {$IFDEF MACOS_OK}
    FPageActive: Boolean;
    {$ENDIF}
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
    FModificationDate: string;
    FCreationDate: String;
    FProducer: String;
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
    FEmbedFonts: Boolean;
    FFontFallBackList: TStrings;
    FPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    FPDFStandard: TTMSFNCPDFLibStandard;
    {$IFDEF MACOS_OK}
    FPDFContext: CGContextRef;
    FPDFDocument: CGPDFDocumentRef;
    FPDFData: NSMutableData;
    FFileName: String;
    {$ENDIF}
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    procedure SetPageOrientation(
      const Value: TTMSFNCPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
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
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetOnAfterDrawFooter(const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
    procedure SetOnAfterDrawHeader(const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
    procedure SetOnBeforeDrawHeader(const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
    procedure SetOnBeforeDrawPageNumber(const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnNewPageStarted(const Value: TTMSFNCPDFLibNewPageStartedEvent);
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    function GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
    function GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
    function GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
    function GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
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
    {$IFDEF MACOS_OK}
    function PDFData: NSMutableData;
    {$ENDIF}
    function Context: ITMSFNCCustomPDFGraphicsLib;
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
    property CreationDate: String read FCreationDate;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageSize: TTMSFNCPDFLibPageSize read FPageSize write FPageSize;
    property Orientation: TTMSFNCPDFLibPageOrientation read FPageOrientation write FPageOrientation;
    property Author: String read FAuthor write FAuthor;
    property Creator: String read FCreator write FCreator;
    property Header: UnicodeString read FHeader write FHeader;
    property EmbedFonts: Boolean read FEmbedFonts write FEmbedFonts;
    property HeaderSize: Single read FHeaderSize write FHeaderSize;
    property HeaderMargins: TTMSFNCMargins read FHeaderMargins write FHeaderMargins;
    property HeaderAlignment: TTMSFNCGraphicsTextAlign read FHeaderAlignment write FHeaderAlignment;
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
    property FooterAlignment: TTMSFNCGraphicsTextAlign read FFooterAlignment write FFooterAlignment;
    property FooterFont: TTMSFNCPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read FTitle write FTitle;
    property OwnerPassword: String read FOwnerPassword write FOwnerPassword;
    property UserPassword: String read FUserPassword write FUserPassword;
    property AllowsPrinting: Boolean read FAllowsPrinting write FAllowsPrinting;
    property AllowsCopying: Boolean read FAllowsCopying write FAllowsCopying;
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
    PDFLibService := TTMSFNCMacPDFLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFLibService, PDFLibService);
  end;
end;

procedure UnregisterPDFLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFLibService);
end;

{ TTMSFNCMacPDFLibService }

function TTMSFNCMacPDFLibService.DoCreatePDFLib: ITMSFNCCustomPDFLib;
begin
  Result := TTMSFNCMacPDFLib.Create;
end;

{ TTMSFNCMacPDFLib }

procedure TTMSFNCMacPDFLib.BeginDocument(FileName: String = '');
{$IFDEF MACOS_OK}
var
  m: CGRect;
  mb: TRectF;
  dic: NSMutableDictionary;
  arrKeyWords: array of pointer;
  I: Integer;
  url: CFURLRef;
  cons: CGDataConsumerRef;
  pth: CFStringRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  mb := MediaBox;
  m := CGRectMake(mb.Left, mb.Top, mb.Width, mb.Height);
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  dic.setValue(CFSTR(Author), NSStrEx('kCGPDFContextAuthor'));
  dic.setValue(CFSTR(Subject), NSStrEx('kCGPDFContextSubject'));
  dic.setValue(CFSTR(Creator), NSStrEx('kCGPDFContextCreator'));
  dic.setValue(CFSTR(Title), NSStrEx('kCGPDFContextTitle'));
  if OwnerPassword <> '' then
    dic.setValue(CFSTR(OwnerPassword), NSStrEx('kCGPDFContextOwnerPassword'));

  if UserPassword <> '' then
    dic.setValue(CFSTR(UserPassword), NSStrEx('kCGPDFContextUserPassword'));

  setLength(arrKeyWords, Keywords.Count);
  for I := 0 to Keywords.Count - 1 do
    arrKeyWords[I] := (NSStrEx(Keywords[I]) as ILocalObject).GetObjectID;

  if Length(arrKeyWords) > 0 then
    dic.setValue(TNSArray.OCClass.arrayWithObjects(@arrKeyWords[0], Length(arrKeyWords)), NSStrEx('kCGPDFContextKeywords'));
  dic.setValue(TNSNumber.OCClass.numberWithBool(AllowsPrinting), NSStrEx('kCGPDFContextAllowsPrinting'));
  dic.setValue(TNSNumber.OCClass.numberWithBool(AllowsCopying), NSStrEx('kCGPDFContextAllowsCopying'));
  FFileName := FileName;
  if FileName = '' then
  begin
    FPDFData := TNSMutableData.Wrap(TNSMutableData.OCClass.data);
    cons := CGDataConsumerCreateWithCFData((FPDFData as ILocalObject).GetObjectID);
    FPDFContext := CGPDFContextCreate(cons, @m, (dic as ILocalObject).GetObjectID);
    CGDataConsumerRelease(cons);
  end
  else
  begin
    pth := CFStringCreateWithCString(nil, MarshaledAString(UTF8Encode(FileName)), kCFStringEncodingUTF8);
    if Assigned(pth) then
    begin
      url := CFURLCreateWithFileSystemPath(nil, pth, kCFURLPOSIXPathStyle, false);
      if Assigned(url) then
      begin
        FPDFContext := CGPDFContextCreateWithURL(url, @m, (dic as ILocalObject).GetObjectID);
        CFRelease(url);
      end;
      CFRelease(pth);
    end;
  end;

  if Assigned(FPDFContext) then
  begin
    TNSGraphicsContext.OCClass.saveGraphicsState;
    TNSGraphicsContext.OCClass.setCurrentContext(TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.
      graphicsContextWithGraphicsPort(FPDFContext, True)));
  end;

  dic.release;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.NewPage;
{$IFDEF MACOS_OK}
var
  ab, mb, bb, cb, tb: TRectF;
  abt, mbt, bbt, cbt, tbt: CGRect;
  abref, mbref, bbref, cbref, tbref: CFDataRef;
  pageInfo: NSMutableDictionary;
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    if FPageActive then
    begin
      CGPDFContextEndPage(c);
      FPageActive := False;
    end;

    mb := MediaBox;
    mbt := CGRectMake(mb.Left, mb.Top, mb.Width, mb.Height);
    ab := ArtBox;
    abt := CGRectMake(ab.Left, ab.Top, ab.Width, ab.Height);
    bb := BleedBox;
    bbt := CGRectMake(bb.Left, bb.Top, bb.Width, bb.Height);
    cb := CropBox;
    cbt := CGRectMake(cb.Left, cb.Top, cb.Width, cb.Height);
    tb := TrimBox;
    tbt := CGRectMake(tb.Left, tb.Top, tb.Width, tb.Height);

    pageInfo := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
    mbref := CFDataCreate(nil, @mbt, SizeOf(CGRect));
    pageInfo.setValue(mbref, NSStrEx('MediaBox'));
    CFRelease(mbref);

    bbref := CFDataCreate(nil, @bbt, SizeOf(CGRect));
    pageInfo.setValue(bbref, NSStrEx('BleedBox'));
    CFRelease(bbref);

    abref := CFDataCreate(nil, @abt, SizeOf(CGRect));
    pageInfo.setValue(abref, NSStrEx('ArtBox'));
    CFRelease(abref);

    cbref := CFDataCreate(nil, @cbt, SizeOf(CGRect));
    pageInfo.setValue(cbref, NSStrEx('CropBox'));
    CFRelease(cbref);

    tbref := CFDataCreate(nil, @tbt, SizeOf(CGRect));
    pageInfo.setValue(tbref, NSStrEx('TrimBox'));
    CFRelease(tbref);

    CGPDFContextBeginPage(c, (pageInfo as ILocalObject).GetObjectID);

    CGContextScaleCTM(c, 1, -1);
    CGContextTranslateCTM(c, 0, -mbt.size.height);

    pageInfo.release;

    FPageActive := True;

    if Assigned(FPDFInitializationLib) then
    begin
      FPDFInitializationLib.SetCanvas(c);
      FPDFInitializationLib.SetPageWidth(MediaBox.Width);
      FPDFInitializationLib.SetPageHeight(MediaBox.Height);
      FPDFInitializationLib.InitializeAppearance;
    end;

    DoNewPageStarted(GetPageIndex);

    DrawHeader;
    DrawFooter;
    DrawPageNumber;

    Inc(FPageCount);
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.OpenDocument(FileStream: TMemoryStream);
{$IFDEF MACOS_OK}
var
  dt: Pointer;
  prov: CGDataProviderRef;
{$ENDIF}
begin
  if IsDocumentOpened then
    CloseDocument;

  {$IFDEF MACOS_OK}
  dt := TNSData.OCClass.dataWithBytesNoCopy(FileStream.Memory, FileStream.Size);
  prov := CGDataProviderCreateWithCFData(dt);  
  FPDFDocument := CGPDFDocumentCreateWithProvider(prov);
  CGDataProviderRelease(prov);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.OpenDocument(FileName: String);
{$IFDEF MACOS_OK}
var
  path: CFStringRef;
  url: CFURLRef;
{$ENDIF}
begin
  if IsDocumentOpened then
    CloseDocument;

  {$IFDEF MACOS_OK}
  path := CFStringCreateWithCString(nil, NSStrEx(FileName).UTF8String, kCFStringEncodingUTF8);
  url := CFURLCreateWithFileSystemPath(nil, path, kCFURLPOSIXPathStyle, False);
  CFRelease(path);
  FPDFDocument := CGPDFDocumentCreateWithURL(url);
  CFRelease(url);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.CloseDocument;
begin
  {$IFDEF MACOS_OK}
  if Assigned(FPDFDocument) then
  begin
    CGPDFDocumentRelease(FPDFDocument);
    FPDFDocument := nil;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.Context: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

constructor TTMSFNCMacPDFLib.Create;
var
  r: TRectF;
begin
  inherited;
  FPageCount := 0;
  FPageHeight := 0;
  FPageWidth := 0;
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

destructor TTMSFNCMacPDFLib.Destroy;
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

procedure TTMSFNCMacPDFLib.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TTMSFNCMacPDFLib.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TTMSFNCMacPDFLib.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TTMSFNCMacPDFLib.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCMacPDFLib.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCMacPDFLib.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCMacPDFLib.DoNewPageStarted(APageIndex: Integer);
begin
  if Assigned(OnNewPageStarted) then
    OnNewPageStarted(Self, APageIndex);
end;

procedure TTMSFNCMacPDFLib.DrawFooter;
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

procedure TTMSFNCMacPDFLib.DrawHeader;
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

procedure TTMSFNCMacPDFLib.DrawPageNumber;
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


procedure TTMSFNCMacPDFLib.DrawPage(PageIndex: Integer);
{$IFDEF MACOS_OK}
var
  page: CGPDFPageRef;
  c: CGContextRef;
  r: NSRect;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FPDFDocument) then
  begin
    page := CGPDFDocumentGetPage(FPDFDocument, PageIndex);
    if Assigned(page) then
    begin
      c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
      if Assigned(c) then
      begin
        r := CGPDFPageGetBoxRect(page, kCGPDFMediaBox);
        CGContextSaveGState(c);
        CGContextScaleCTM(c, 1, -1);
        CGContextTranslateCTM(c, 0, -r.size.height);
        CGContextDrawPDFPage(c, page);
        CGContextRestoreGState(c);
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    if FPageActive then
    begin
      CGPDFContextEndPage(c);
      FPageActive := False;
    end;
    
    if Assigned(FPDFContext) then
    begin
      CGPDFContextClose(FPDFContext);
      CFRelease(FPDFContext);
      TNSGraphicsContext.OCClass.restoreGraphicsState;
    end;

    if Assigned(FPDFData) then
    begin
      Result := TMemoryStream.Create;
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Result.Write(FPDFData.bytes^, FPDFData.length);
      {$ELSE}
      Result.Write(FPDFData.bytes, FPDFData.length);
      {$IFEND}
      {$HINTS ON}
    end
    else if AOpenInPDFReader then
      TTMSFNCUtils.OpenFile(FFileName);
  end;

  FPageCount := 0;
  {$ENDIF}
end;

function  TTMSFNCMacPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := False;
  {$IFDEF MACOS_OK}
  if Assigned(FPDFDocument) then
  begin
    {$HINTS OFF}
    {$IF COMPILERVERSION < 34}
    Result := CGPDFDocumentUnlockWithPassword(FPDFDocument, NSStrEx(Password).UTF8String) = 1;
    {$ELSE}
    Result := CGPDFDocumentUnlockWithPassword(FPDFDocument, NSStrEx(Password).UTF8String);
    {$IFEND}
    {$HINTS ON}
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.InsertPage(PageIndex: Integer);
begin

end;

function TTMSFNCMacPDFLib.IsDocumentOpened: Boolean;
begin
  {$IFDEF MACOS_OK}
  Result := Assigned(FPDFDocument);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.GetAllowsCopying: Boolean;
begin
  Result := AllowsCopying;
end;

function TTMSFNCMacPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := AllowsPrinting;
end;

function TTMSFNCMacPDFLib.GetArtBox: TRectF;
begin
  Result := ArtBox;
end;

function TTMSFNCMacPDFLib.GetAuthor: String;
begin
  Result := Author;
end;

function TTMSFNCMacPDFLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.BitmapContainer;
end;

function TTMSFNCMacPDFLib.GetBleedBox: TRectF;
begin
  Result := BleedBox;
end;

function TTMSFNCMacPDFLib.GetCreationDate: String;
begin
  Result := CreationDate;
end;

function TTMSFNCMacPDFLib.GetCreator: String;
begin
  Result := Creator;
end;

function TTMSFNCMacPDFLib.GetCropBox: TRectF;
begin
  Result := CropBox;
end;

procedure TTMSFNCMacPDFLib.GetDocumentInfo;
{$IFDEF MACOS_OK}
var
  dic: CGPDFDictionaryRef;
  I: Integer;
  pdfStr: CGPDFStringRef;
  arr: TArray<String>;
  cfstr: CFStringRef;
  strKeys: string;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FPDFDocument) then
  begin
    dic := CGPDFDocumentGetInfo(FPDFDocument);
    if Assigned(dic) then
    begin
      pdfStr := nil;

      {$HINTS OFF}
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Author').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Author').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FAuthor := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Subject').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Subject').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FSubject := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Creator').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Creator').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FCreator := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Title').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Title').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FTitle := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('ModDate').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('ModDate').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FModificationDate := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('CreationDate').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('CreationDate').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FCreationDate := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfstr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Producer').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Producer').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        cfstr := CGPDFStringCopyTextString(pdfStr);
        FProducer := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        CFRelease(cfstr);
      end;

      pdfStr := nil;
      {$IF COMPILERVERSION < 34}
      if CGPDFDictionaryGetString(dic, NSStrEx('Keywords').UTF8String, @pdfStr) = 1 then
      {$ELSE}
      if CGPDFDictionaryGetString(dic, NSStrEx('Keywords').UTF8String, @pdfStr) then
      {$IFEND}
      begin
        FKeywords.Clear;
        cfstr := CGPDFStringCopyTextString(pdfStr);
        strKeys := UTF8ToString(TNSString.Wrap(cfstr).UTF8String);
        arr := strKeys.Split([',']);
        for I := 0 to Length(arr) - 1 do
          FKeywords.Add(arr[I].Trim);
        CFRelease(cfstr);
      end;
      {$HINTS ON}
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.GetEmbedFonts: Boolean;
begin
  Result := EmbedFonts;
end;

function TTMSFNCMacPDFLib.GetFontFallBackList: TStrings;
begin
  Result := FontFallBackList;
end;

function TTMSFNCMacPDFLib.GetFooter: UnicodeString;
begin
  Result := Footer;
end;

function TTMSFNCMacPDFLib.GetFooterAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := FooterAlignment;
end;

function TTMSFNCMacPDFLib.GetFooterFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FFooterFont;
end;

function TTMSFNCMacPDFLib.GetFooterMargins: TTMSFNCMargins;
begin
  Result := FooterMargins;
end;

function TTMSFNCMacPDFLib.GetFooterRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FFooterMargins.Left, FMediaBox.Bottom - FFooterSize - FFooterMargins.Bottom, FMediaBox.Width - FFooterMargins.Right, FMediaBox.Bottom - FFooterMargins.Bottom);
end;

function TTMSFNCMacPDFLib.GetFooterSize: Single;
begin
  Result := FooterSize;
end;

function TTMSFNCMacPDFLib.GetHeader: UnicodeString;
begin
  Result := Header;
end;

function TTMSFNCMacPDFLib.GetHeaderAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := HeaderAlignment;
end;

function TTMSFNCMacPDFLib.GetHeaderFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FHeaderFont;
end;

function TTMSFNCMacPDFLib.GetHeaderMargins: TTMSFNCMargins;
begin
  Result := HeaderMargins;
end;

function TTMSFNCMacPDFLib.GetHeaderRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FHeaderMargins.Left, FMediaBox.Top + FHeaderMargins.Top, FMediaBox.Width - FHeaderMargins.Right, FMediaBox.Top + FHeaderMargins.Top + FHeaderSize);
end;

function TTMSFNCMacPDFLib.GetHeaderSize: Single;
begin
  Result := HeaderSize;
end;

function TTMSFNCMacPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := PageNumberFormat;
end;

function TTMSFNCMacPDFLib.GetPageNumber: TTMSFNCPDFLibPageNumber;
begin
  Result := PageNumber;
end;

function TTMSFNCMacPDFLib.GetPageNumberAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := PageNumberAlignment;
end;

function TTMSFNCMacPDFLib.GetPageNumberFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := FPageNumberFont;
end;

function TTMSFNCMacPDFLib.GetPageNumberMargins: TTMSFNCMargins;
begin
  Result := PageNumberMargins;
end;

function TTMSFNCMacPDFLib.GetPageNumberRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  case PageNumber of
    pnHeader: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Top + FPageNumberMargins.Top, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Top + FPageNumberMargins.Top + FPageNumberSize);
    pnFooter: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Bottom - FPageNumberSize - FPageNumberMargins.Bottom, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Bottom - FPageNumberMargins.Bottom);
  end;
end;

function TTMSFNCMacPDFLib.GetPageNumberSize: Single;
begin
  Result := PageNumberSize;
end;

function TTMSFNCMacPDFLib.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

function TTMSFNCMacPDFLib.GetMediaBox: TRectF;
begin
  Result := MediaBox;
end;

function TTMSFNCMacPDFLib.GetModificationDate: String;
begin
  Result := ModificationDate;
end;

function TTMSFNCMacPDFLib.GetOnAfterDrawFooter: TTMSFNCPDFLibAfterDrawFooterEvent;
begin
  Result := FOnAfterDrawFooter;
end;

function TTMSFNCMacPDFLib.GetOnAfterDrawHeader: TTMSFNCPDFLibAfterDrawHeaderEvent;
begin
  Result := FOnAfterDrawHeader;
end;

function TTMSFNCMacPDFLib.GetOnAfterDrawPageNumber: TTMSFNCPDFLibAfterDrawPageNumberEvent;
begin
  Result := FOnAfterDrawPageNumber;
end;

function TTMSFNCMacPDFLib.GetOnBeforeDrawFooter: TTMSFNCPDFLibBeforeDrawFooterEvent;
begin
  Result := FOnBeforeDrawFooter;
end;

function TTMSFNCMacPDFLib.GetOnBeforeDrawHeader: TTMSFNCPDFLibBeforeDrawHeaderEvent;
begin
  Result := FOnBeforeDrawHeader
end;

function TTMSFNCMacPDFLib.GetOnBeforeDrawPageNumber: TTMSFNCPDFLibBeforeDrawPageNumberEvent;
begin
  Result := FOnBeforeDrawPageNumber
end;

function TTMSFNCMacPDFLib.GetOnNewPageStarted: TTMSFNCPDFLibNewPageStartedEvent;
begin
  Result := FOnNewPageStarted;
end;

function TTMSFNCMacPDFLib.GetOwnerPassword: String;
begin
  Result := OwnerPassword;
end;

function TTMSFNCMacPDFLib.GetPageCount: Integer;
begin
  {$IFDEF MACOS_OK}
  if Assigned(FPDFDocument) then
    Result := CGPDFDocumentGetNumberOfPages(FPDFDocument)
  else
    Result := FPageCount;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCMacPDFLib.GetPageIndex: Integer;
begin
  Result := GetPageCount;
end;

procedure TTMSFNCMacPDFLib.GetPageInfo(PageIndex: Integer);
{$IFDEF MACOS_OK}
var
  page: CGPDFPageRef;
  mb, cb, bb, tb, ab: NSRect;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  page := CGPDFDocumentGetPage(FPDFDocument, PageIndex);
  if Assigned(page) then
  begin
    mb := CGPDFPageGetBoxRect(page, kCGPDFMediaBox);
    FMediaBox := RectF(mb.origin.x, mb.origin.y, mb.origin.x + mb.size.width, mb.origin.y + mb.size.height);
    cb := CGPDFPageGetBoxRect(page, kCGPDFCropBox);
    FCropBox := RectF(cb.origin.x, cb.origin.y, cb.origin.x + cb.size.width, cb.origin.y + cb.size.height);
    bb := CGPDFPageGetBoxRect(page, kCGPDFBleedBox);
    FBleedBox := RectF(bb.origin.x, bb.origin.y, bb.origin.x + bb.size.width, bb.origin.y + bb.size.height);
    tb := CGPDFPageGetBoxRect(page, kCGPDFTrimBox);
    FTrimBox := RectF(tb.origin.x, tb.origin.y, tb.origin.x + tb.size.width, tb.origin.y + tb.size.height);
    ab := CGPDFPageGetBoxRect(page, kCGPDFArtBox);
    FArtBox := RectF(ab.origin.x, ab.origin.y, ab.origin.x + ab.size.width, ab.origin.y + ab.size.height);
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFLib.GetPageOrientation: TTMSFNCPDFLibPageOrientation;
begin
  Result := Orientation;
end;

function TTMSFNCMacPDFLib.GetPageSize: TTMSFNCPDFLibPageSize;
begin
  Result := PageSize;
end;

function TTMSFNCMacPDFLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCMacPDFLib.GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TTMSFNCMacPDFLib.GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TTMSFNCMacPDFLib.GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

function TTMSFNCMacPDFLib.GetPDFStandard: TTMSFNCPDFLibStandard;
begin
  Result := PDFStandard;
end;

function TTMSFNCMacPDFLib.GetProducer: String;
begin
  Result := Producer;
end;

function TTMSFNCMacPDFLib.GetSubject: String;
begin
  Result := Subject;
end;

function TTMSFNCMacPDFLib.GetTitle: String;
begin
  Result := Title;
end;

function TTMSFNCMacPDFLib.GetTrimBox: TRectF;
begin
  Result := TrimBox;
end;

function TTMSFNCMacPDFLib.GetUserPassword: String;
begin
  Result := UserPassword;
end;

{$IFDEF MACOS_OK}
function TTMSFNCMacPDFLib.PDFData: NSMutableData;
begin
  Result := FPDFData;
end;
{$ENDIF}

procedure TTMSFNCMacPDFLib.SaveDocumentFromStream(
  FileStream: TMemoryStream; FileName: String);
{$IFDEF MACOS_OK}
var
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  dt := TNSData.Wrap(TNSData.OCClass.dataWithBytesNoCopy(FileStream.Memory, FileStream.Size));
  dt.writeToFile(NSStrEx(FileName), True);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  AllowsCopying := Value;
end;

procedure TTMSFNCMacPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  AllowsPrinting := Value;
end;

procedure TTMSFNCMacPDFLib.SetArtBox(const Value: TRectF);
begin
  ArtBox := Value;
end;

procedure TTMSFNCMacPDFLib.SetAuthor(const Value: String);
begin
  Author := Value;
end;

procedure TTMSFNCMacPDFLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib.BitmapContainer := Value;
end;

procedure TTMSFNCMacPDFLib.SetBleedBox(const Value: TRectF);
begin
  BleedBox := Value;
end;

procedure TTMSFNCMacPDFLib.SetCreator(const Value: String);
begin
  Creator := Value;
end;

procedure TTMSFNCMacPDFLib.SetCropBox(const Value: TRectF);
begin
  CropBox := Value;
end;

procedure TTMSFNCMacPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  EmbedFonts := Value;
end;

procedure TTMSFNCMacPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  FontFallBackList.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetFooter(const Value: UnicodeString);
begin
  Footer := Value;
end;

procedure TTMSFNCMacPDFLib.SetFooterAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FooterAlignment := Value;
end;

procedure TTMSFNCMacPDFLib.SetFooterFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  FooterMargins.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetFooterSize(const Value: Single);
begin
  FooterSize := Value;
end;

procedure TTMSFNCMacPDFLib.SetPDFGraphicsExLib(
  const Value: ITMSFNCCustomPDFGraphicsExLib);
begin
  FPDFGraphicsExLib := Value;
end;

procedure TTMSFNCMacPDFLib.SetPDFGraphicsLib(
  const Value: ITMSFNCCustomPDFGraphicsLib);
begin
  FPDFGraphicsLib := Value;
end;

procedure TTMSFNCMacPDFLib.SetPDFInitializationLib(
  const Value: ITMSFNCCustomPDFInitializationLib);
begin
  FPDFInitializationLib := Value;
end;

procedure TTMSFNCMacPDFLib.SetPDFStandard(const Value: TTMSFNCPDFLibStandard);
begin
  PDFStandard := Value;
end;

procedure TTMSFNCMacPDFLib.SetHeader(const Value: UnicodeString);
begin
  Header := Value;
end;

procedure TTMSFNCMacPDFLib.SetHeaderAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  HeaderAlignment := Value;
end;

procedure TTMSFNCMacPDFLib.SetHeaderFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  HeaderMargins.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetHeaderSize(const Value: Single);
begin
  HeaderSize := Value;
end;

procedure TTMSFNCMacPDFLib.SetKeywords(const Value: TStrings);
begin
  Keywords.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetMediaBox(const Value: TRectF);
begin
  MediaBox := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnAfterDrawFooter(
  const Value: TTMSFNCPDFLibAfterDrawFooterEvent);
begin
  FOnAfterDrawFooter := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnAfterDrawHeader(
  const Value: TTMSFNCPDFLibAfterDrawHeaderEvent);
begin
  FOnAfterDrawHeader := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnAfterDrawPageNumber(
  const Value: TTMSFNCPDFLibAfterDrawPageNumberEvent);
begin
  FOnAfterDrawPageNumber := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnBeforeDrawFooter(
  const Value: TTMSFNCPDFLibBeforeDrawFooterEvent);
begin
  FOnBeforeDrawFooter := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnBeforeDrawHeader(
  const Value: TTMSFNCPDFLibBeforeDrawHeaderEvent);
begin
  FOnBeforeDrawHeader := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TTMSFNCPDFLibBeforeDrawPageNumberEvent);
begin
  FOnBeforeDrawPageNumber := Value;
end;

procedure TTMSFNCMacPDFLib.SetOnNewPageStarted(
  const Value: TTMSFNCPDFLibNewPageStartedEvent);
begin
  FOnNewPageStarted := Value;
end;

procedure TTMSFNCMacPDFLib.SetOwnerPassword(const Value: String);
begin
  OwnerPassword := Value;
end;

procedure TTMSFNCMacPDFLib.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCMacPDFLib.SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TTMSFNCMacPDFLib.SetPageNumberAlignment(
  const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TTMSFNCMacPDFLib.SetPageNumberFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TTMSFNCMacPDFLib.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TTMSFNCMacPDFLib.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TTMSFNCMacPDFLib.SetPageOrientation(
  const Value: TTMSFNCPDFLibPageOrientation);
begin
  Orientation := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCMacPDFLib.SetPageSize(
  const Value: TTMSFNCPDFLibPageSize);
begin
  PageSize := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCMacPDFLib.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
  UpdateBoxRect;
end;

procedure TTMSFNCMacPDFLib.SetSubject(const Value: String);
begin
  Subject := Value;
end;

procedure TTMSFNCMacPDFLib.SetTitle(const Value: String);
begin
  Title := Value;
end;

procedure TTMSFNCMacPDFLib.SetTrimBox(const Value: TRectF);
begin
  TrimBox := Value;
end;

procedure TTMSFNCMacPDFLib.SetUserPassword(const Value: String);
begin
  UserPassword := Value;
end;

procedure TTMSFNCMacPDFLib.UpdateBoxRect;
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

