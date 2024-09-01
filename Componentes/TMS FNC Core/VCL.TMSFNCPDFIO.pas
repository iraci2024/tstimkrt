{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2016 - 2021                                    }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit VCL.TMSFNCPDFIO;

{$I VCL.TMSFNCDefines.inc}

interface

uses
  Classes, VCL.TMSFNCCustomComponent, VCL.TMSFNCPDFLib, VCL.TMSFNCPDFCoreLibBase, VCL.TMSFNCTypes,
  VCL.TMSFNCPDFGraphicsLib, VCL.TMSFNCBitmapContainer, VCL.TMSFNCGraphicsTypes
  {$IFNDEF LCLLIB}
  , Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

type
  TTMSFNCCustomPDFIO = class;

  TTMSFNCPDFIOInformation = class(TPersistent)
  private
    FAuthor: String;
    FCreator: String;
    FSubject: String;
    FTitle: String;
    FKeywords: TStringList;
    procedure SetKeywords(const Value: TStringList);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Author: String read FAuthor write FAuthor;
    property Title: String read FTitle write FTitle;
    property Subject: String read FSubject write FSubject;
    property Keywords: TStringList read FKeywords write SetKeywords;
    property Creator: String read FCreator write FCreator;
  end;

  TTMSFNCPDFIOOptions = class(TPersistent)
  private
    FOpenInPDFReader: Boolean;
    FDefaultFont: TTMSFNCPDFGraphicsLibFont;
    FFooter: UnicodeString;
    FHeader: UnicodeString;
    FMargins: TTMSFNCMargins;
    FHeaderFont: TTMSFNCPDFGraphicsLibFont;
    FFooterFont: TTMSFNCPDFGraphicsLibFont;
    FHeaderSize: Single;
    FFooterMargins: TTMSFNCMargins;
    FFooterAlignment: TTMSFNCGraphicsTextAlign;
    FFooterSize: Single;
    FHeaderMargins: TTMSFNCMargins;
    FHeaderAlignment: TTMSFNCGraphicsTextAlign;
    FPageSize: TTMSFNCPDFLibPageSize;
    FPageOrientation: TTMSFNCPDFLibPageOrientation;
    FPageWidth: Single;
    FPageHeight: Single;
    FExportImages: Boolean;
    FFontFallBackList: TStrings;
    FEmbedFonts: Boolean;
    FPageNumberFormat: UnicodeString;
    FPageNumberAlignment: TTMSFNCGraphicsTextAlign;
    FPageNumber: TTMSFNCPDFLibPageNumber;
    FPageNumberFont: TTMSFNCPDFGraphicsLibFont;
    FPageNumberMargins: TTMSFNCMargins;
    FPageNumberSize: Single;
    procedure SetDefaultFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetMargins(const Value: TTMSFNCMargins);
    procedure SetFooterFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetFooterAlignment(
      const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFooterMargins(const Value: TTMSFNCMargins);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeaderAlignment(
      const Value: TTMSFNCGraphicsTextAlign);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetHeaderSize(const Value: Single);
    function IsFooterSizeStored: Boolean;
    function IsHeaderSizeStored: Boolean;
    procedure SetPageOrientation(const Value: TTMSFNCPDFLibPageOrientation);
    procedure SetPageSize(const Value: TTMSFNCPDFLibPageSize);
    function IsPageHeightStored: Boolean;
    function IsPageWidthStored: Boolean;
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetExportImages(const Value: Boolean);
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetPageNumber(const Value: TTMSFNCPDFLibPageNumber);
    procedure SetPageNumberAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetPageNumberFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
    function IsPageNumberSizeStored: Boolean;
    procedure SetPageNumberSize(const Value: Single);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property OpenInPDFReader: Boolean read FOpenInPDFReader write FOpenInPDFReader default False;
    property DefaultFont: TTMSFNCPDFGraphicsLibFont read FDefaultFont write SetDefaultFont;
    property Header: UnicodeString read FHeader write FHeader;
    property Footer: UnicodeString read FFooter write FFooter;
    property Margins: TTMSFNCMargins read FMargins write SetMargins;
    property HeaderFont: TTMSFNCPDFGraphicsLibFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TTMSFNCPDFGraphicsLibFont read FFooterFont write SetFooterFont;
    property HeaderSize: Single read FHeaderSize write SetHeaderSize stored IsHeaderSizeStored nodefault;
    property HeaderMargins: TTMSFNCMargins read FHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TTMSFNCGraphicsTextAlign read FHeaderAlignment write SetHeaderAlignment default gtaCenter;
    property FooterSize: Single read FFooterSize write SetFooterSize stored IsFooterSizeStored nodefault;
    property FooterMargins: TTMSFNCMargins read FFooterMargins write SetFooterMargins;
    property FooterAlignment: TTMSFNCGraphicsTextAlign read FFooterAlignment write SetFooterAlignment default gtaCenter;
    property PageWidth: Single read FPageWidth write SetPageWidth stored IsPageWidthStored nodefault;
    property PageHeight: Single read FPageHeight write SetPageHeight stored IsPageHeightStored nodefault;
    property PageSize: TTMSFNCPDFLibPageSize read FPageSize write SetPageSize default psLetter;
    property PageOrientation: TTMSFNCPDFLibPageOrientation read FPageOrientation write SetPageOrientation default poPortrait;
    property ExportImages: Boolean read FExportImages write SetExportImages default True;
    property FontFallBackList: TStrings read FFontFallBackList write SetFontFallBackList;
    property EmbedFonts: Boolean read FEmbedFonts write SetEmbedFonts default True;
    property PageNumber: TTMSFNCPDFLibPageNumber read FPageNumber write SetPageNumber default pnHeader;
    property PageNumberMargins: TTMSFNCMargins read FPageNumberMargins write SetPageNumberMargins;
    property PageNumberFormat: UnicodeString read FPageNumberFormat write SetPageNumberFormat;
    property PageNumberAlignment: TTMSFNCGraphicsTextAlign read FPageNumberAlignment write SetPageNumberAlignment default gtaTrailing;
    property PageNumberFont: TTMSFNCPDFGraphicsLibFont read FPageNumberFont write SetPageNumberFont;
    property PageNumberSize: Single read FPageNumberSize write SetPageNumberSize stored IsPageNumberSizeStored nodefault;
  end;

  TTMSFNCPDFIOExportObject = TComponent;

  TTMSFNCPDFIOGetHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; var AHeader: UnicodeString) of object;
  TTMSFNCPDFIOGetFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; var AFooter: UnicodeString) of object;
  TTMSFNCPDFIOBeforeDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFIOAfterDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFIOBeforeDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFIOBeforeDrawPageNumberEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFIOBeforeDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TTMSFNCPDFIOAfterDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFIOAfterDrawPageNumberEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;
  TTMSFNCPDFIOAfterDrawEvent = procedure(Sender: TObject; APDFLib: TTMSFNCPDFLib) of object;
  TTMSFNCPDFIOAfterDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib) of object;

  TTMSFNCPDFIOExportObjectArray = array of TTMSFNCPDFIOExportObject;
  TTMSFNCPDFIOExportRectArray = array of TRectF;

  {$IFDEF FNCLIB}
  TTMSFNCCustomPDFIO = class(TTMSFNCCustomComponent, ITMSFNCBitmapContainer)
  {$ELSE}
  TTMSFNCCustomPDFIO = class(TTMSFNCCustomComponent)
  {$ENDIF}
  private
    FBitmapContainer: TTMSFNCBitmapContainer;
    FNewPageStarted: Boolean;
    FInformation: TTMSFNCPDFIOInformation;
    FOptions: TTMSFNCPDFIOOptions;
    FOnGetHeader: TTMSFNCPDFIOGetHeaderEvent;
    FOnGetFooter: TTMSFNCPDFIOGetFooterEvent;
    FOnBeforeDrawHeader: TTMSFNCPDFIOBeforeDrawHeaderEvent;
    FOnBeforeDrawPageNumber: TTMSFNCPDFIOBeforeDrawPageNumberEvent;
    FOnBeforeDrawContent: TTMSFNCPDFIOBeforeDrawContentEvent;
    FOnBeforeDrawFooter: TTMSFNCPDFIOBeforeDrawFooterEvent;
    FOnAfterDrawHeader: TTMSFNCPDFIOAfterDrawHeaderEvent;
    FOnAfterDrawPageNumber: TTMSFNCPDFIOAfterDrawPageNumberEvent;
    FOnAfterDrawFooter: TTMSFNCPDFIOAfterDrawFooterEvent;
    FOnAfterDrawContent: TTMSFNCPDFIOAfterDrawContentEvent;
    FActiveExportObject: TTMSFNCPDFIOExportObject;
    FExportObject: TTMSFNCPDFIOExportObject;
    FOnAfterDraw: TTMSFNCPDFIOAfterDrawEvent;
    procedure SetInformation(const Value: TTMSFNCPDFIOInformation);
    procedure SetOptions(const Value: TTMSFNCPDFIOOptions);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    function GetBitmapContainer: TTMSFNCBitmapContainer;
  protected
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function CreateOptions: TTMSFNCPDFIOOptions; virtual;
    function NewPage(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject): Boolean; virtual;
    function GetContentRect(const APDFLib: TTMSFNCPDFLib): TRectF; virtual;
    function InitializePDFLib: TTMSFNCPDFLib; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EndPage(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject); virtual;
    procedure DoNewPageStarted(Sender: TObject; {%H-}APageIndex: Integer); virtual;
    procedure DoGetHeader(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer; var AHeader: UnicodeString); virtual;
    procedure DoGetFooter(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer; var AFooter: UnicodeString); virtual;
    procedure DoPDFExport(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; const AExportRect: TRectF); overload; virtual; abstract;
    procedure DoAfterDraw(APDFLib: TTMSFNCPDFLib); virtual;
    procedure DoBeforeDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    procedure DoBeforeDrawContent(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawContent(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib); virtual;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
    property Information: TTMSFNCPDFIOInformation read FInformation write SetInformation;
    property Options: TTMSFNCPDFIOOptions read FOptions write SetOptions;
    property OnGetHeader: TTMSFNCPDFIOGetHeaderEvent read FOnGetHeader write FOnGetHeader;
    property OnGetFooter: TTMSFNCPDFIOGetFooterEvent read FOnGetFooter write FOnGetFooter;
    property OnBeforeDrawHeader: TTMSFNCPDFIOBeforeDrawHeaderEvent read FOnBeforeDrawHeader write FOnBeforeDrawHeader;
    property OnBeforeDrawPageNumber: TTMSFNCPDFIOBeforeDrawPageNumberEvent read FOnBeforeDrawPageNumber write FOnBeforeDrawPageNumber;
    property OnAfterDrawHeader: TTMSFNCPDFIOAfterDrawHeaderEvent read FOnAfterDrawHeader write FOnAfterDrawHeader;
    property OnAfterDrawPageNumber: TTMSFNCPDFIOAfterDrawPageNumberEvent read FOnAfterDrawPageNumber write FOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TTMSFNCPDFIOBeforeDrawFooterEvent read FOnBeforeDrawFooter write FOnBeforeDrawFooter;
    property OnAfterDrawFooter: TTMSFNCPDFIOAfterDrawFooterEvent read FOnAfterDrawFooter write FOnAfterDrawFooter;
    property OnBeforeDrawContent: TTMSFNCPDFIOBeforeDrawContentEvent read FOnBeforeDrawContent write FOnBeforeDrawContent;
    property OnAfterDrawContent: TTMSFNCPDFIOAfterDrawContentEvent read FOnAfterDrawContent write FOnAfterDrawContent;
    property OnAfterDraw: TTMSFNCPDFIOAfterDrawEvent read FOnAfterDraw write FOnAfterDraw;
    property ExportObject: TTMSFNCPDFIOExportObject read FExportObject write FExportObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Save(const AFileName: String); overload;
    procedure Save(const AStream: TStream); overload;
    procedure Save(const AFileName: String; AExportObject: TTMSFNCPDFIOExportObject; AExportRect: TRectF); overload;
    procedure Save(const AStream: TStream; AExportObject: TTMSFNCPDFIOExportObject; AExportRect: TRectF); overload;
    procedure Save(const AFileName: String; AExportObjects: TTMSFNCPDFIOExportObjectArray); overload;
    procedure Save(const AStream: TStream; AExportObjects: TTMSFNCPDFIOExportObjectArray); overload;
    procedure Save(const AFileName: String; AExportObjects: TTMSFNCPDFIOExportObjectArray; AExportRects: TTMSFNCPDFIOExportRectArray); overload;
    procedure Save(const AStream: TStream; AExportObjects: TTMSFNCPDFIOExportObjectArray; AExportRects: TTMSFNCPDFIOExportRectArray); overload;
  end;

implementation

uses
  SysUtils, VCL.TMSFNCUtils;

{$IFNDEF FNCLIB}
{$R 'VCL.TMSFNCPDFIO.res'}
{$ENDIF}

{ TTMSFNCCustomPDFIO }

procedure TTMSFNCCustomPDFIO.Save(const AFileName: String; AExportObjects: TTMSFNCPDFIOExportObjectArray);
var
  r: TTMSFNCPDFIOExportRectArray;
begin
  SetLength(r, 0);
  Save(AFileName, AExportObjects, r);
end;

procedure TTMSFNCCustomPDFIO.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCustomPDFIO then
  begin
    FInformation.Assign((Source as TTMSFNCCustomPDFIO).Information);
    FOptions.Assign((Source as TTMSFNCCustomPDFIO).Options);
  end;
end;

constructor TTMSFNCCustomPDFIO.Create(AOwner: TComponent);
begin
  inherited;
  FInformation := TTMSFNCPDFIOInformation.Create;
  FOptions := CreateOptions;
  if IsDesignTime then
  begin
    FOptions.Header := 'TMS PDF Header';
    FOptions.Footer := 'TMS PDF Footer';
    FOptions.PageNumberFormat := '%d';
  end;
end;

function TTMSFNCCustomPDFIO.CreateOptions: TTMSFNCPDFIOOptions;
begin
  Result := TTMSFNCPDFIOOptions.Create;
end;

destructor TTMSFNCCustomPDFIO.Destroy;
begin
  FOptions.Free;
  FInformation.Free;
  inherited;
end;

procedure TTMSFNCCustomPDFIO.DoAfterDraw(
  APDFLib: TTMSFNCPDFLib);
begin
  if Assigned(OnAfterDraw) then
    OnAfterDraw(Self, APDFLib);
end;

procedure TTMSFNCCustomPDFIO.DoAfterDrawContent(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer;
  ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawContent) then
    OnAfterDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics);
end;

procedure TTMSFNCCustomPDFIO.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, FActiveExportObject, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TTMSFNCCustomPDFIO.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, FActiveExportObject, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TTMSFNCCustomPDFIO.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, FActiveExportObject, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TTMSFNCCustomPDFIO.DoBeforeDrawContent(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer;
  ARect: TRectF; AGraphics: ITMSFNCCustomPDFGraphicsLib;
  var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawContent) then
    OnBeforeDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCCustomPDFIO.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, FActiveExportObject, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCCustomPDFIO.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, FActiveExportObject, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCCustomPDFIO.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: ITMSFNCCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, FActiveExportObject, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCCustomPDFIO.DoGetFooter(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer;
  var AFooter: UnicodeString);
begin
  if Assigned(OnGetFooter) then
    OnGetFooter(Self, AExportObject, APageIndex, AFooter);
end;

procedure TTMSFNCCustomPDFIO.DoGetHeader(AExportObject: TTMSFNCPDFIOExportObject; APageIndex: Integer;
  var AHeader: UnicodeString);
begin
  if Assigned(OnGetHeader) then
    OnGetHeader(Self, AExportObject, APageIndex, AHeader);
end;

procedure TTMSFNCCustomPDFIO.DoNewPageStarted(Sender: TObject; APageIndex: Integer);
var
  h, f: UnicodeString;
  pdflib: ITMSFNCCustomPDFLib;
begin
  if Assigned(Sender) and Supports(Sender, ITMSFNCCustomPDFLib, pdflib) then
  begin
    h := Options.Header;
    f := Options.Footer;
    DoGetHeader(FActiveExportObject, pdflib.GetPageIndex, h);
    DoGetFooter(FActiveExportObject, pdflib.GetPageIndex, f);
    pdflib.Header := h;
    pdflib.Footer := f;
  end;
end;

procedure TTMSFNCCustomPDFIO.EndPage(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject);
var
  r: TRectF;
begin
  if FNewPageStarted then
  begin
    FNewPageStarted := False;
    r := GetContentRect(APDFLib);
    DoAfterDrawContent(AExportObject, APDFLib.GetPageIndex, r, APDFLib.Graphics);
  end;
end;

function TTMSFNCCustomPDFIO.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := FBitmapContainer;
end;

function TTMSFNCCustomPDFIO.GetContentRect(const APDFLib: TTMSFNCPDFLib): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if Assigned(APDFLib) then
  begin
    Result := APDFLib.MediaBox;
    Result := RectF(Result.Left + Options.Margins.Left, Result.Top + Options.Margins.Top,
      Result.Right - Options.Margins.Right, Result.Bottom - Options.Margins.Bottom);
  end;
end;

function TTMSFNCCustomPDFIO.GetDocURL: string;
begin
  Result := TTMSFNCPDFLibDocURL;
end;

function TTMSFNCCustomPDFIO.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomPDFIO.InitializePDFLib: TTMSFNCPDFLib;
var
  I: Integer;
begin
  Result := TTMSFNCPDFLib.Create;
  Result.BitmapContainer := BitmapContainer;
  Result.EmbedFonts := Options.EmbedFonts;
  Result.OnNewPageStarted := DoNewPageStarted;
  Result.OnBeforeDrawHeader := DoBeforeDrawHeader;
  Result.OnAfterDrawHeader := DoAfterDrawHeader;
  Result.OnBeforeDrawPageNumber := DoBeforeDrawPageNumber;
  Result.OnAfterDrawPageNumber := DoAfterDrawPageNumber;
  Result.OnBeforeDrawFooter := DoBeforeDrawFooter;
  Result.OnAfterDrawFooter := DoAfterDrawFooter;
  Result.Header := Options.Header;
  Result.PageNumber := Options.PageNumber;
  Result.PageNumberFormat := Options.PageNumberFormat;
  Result.Footer := Options.Footer;
  Result.Author := Information.Author;
  Result.Title := Information.Title;
  Result.Subject := Information.Subject;
  for I := 0 to Options.FontFallBackList.Count - 1 do
    Result.FontFallBackList.Add(Options.FontFallBackList[I]);
  Result.Keywords.Assign(Information.Keywords);
  Result.Creator := Information.Creator;
  Result.PageWidth := Options.PageWidth;
  Result.PageHeight := Options.PageHeight;
  Result.PageSize := Options.PageSize;
  Result.PageOrientation := Options.PageOrientation;
  Result.PageNumberFont.Assign(Options.PageNumberFont);
  Result.HeaderFont.Assign(Options.HeaderFont);
  Result.FooterFont.Assign(Options.FooterFont);
  Result.FooterMargins.Assign(Options.FooterMargins);
  Result.PageNumberMargins.Assign(Options.PageNumberMargins);
  Result.HeaderMargins.Assign(Options.HeaderMargins);
  Result.PageNumberSize := Options.PageNumberSize;
  Result.HeaderSize := Options.HeaderSize;
  Result.FooterSize := Options.FooterSize;
  Result.HeaderAlignment := Options.HeaderAlignment;
  Result.PageNumberAlignment := Options.PageNumberAlignment;
  Result.FooterAlignment := Options.FooterAlignment;
  Result.Graphics.LineBreakMode := bmLineBreakModeClip;
end;

function TTMSFNCCustomPDFIO.NewPage(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject): Boolean;
var
  r: TRectF;
begin
  Result := False;
  if Assigned(APDFLib) then
  begin
    EndPage(APDFLib, AExportObject);
    APDFLib.Graphics.Font.Assign(Options.DefaultFont);
    APDFLib.NewPage;
    Result := True;
    r := GetContentRect(APDFLib);
    DoBeforeDrawContent(AExportObject, APDFLib.GetPageIndex, r, APDFLib.Graphics, Result);
    FNewPageStarted := Result;
  end;
end;

procedure TTMSFNCCustomPDFIO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FExportObject then
      FExportObject := nil;

    if AComponent = FBitmapContainer then
      FBitmapContainer := nil;
  end;
end;

procedure TTMSFNCCustomPDFIO.Save(const AStream: TStream; AExportObjects: TTMSFNCPDFIOExportObjectArray);
var
  r: TTMSFNCPDFIOExportRectArray;
begin
  SetLength(r, 0);
  Save(AStream, AExportObjects, r);
end;

procedure TTMSFNCCustomPDFIO.Save(const AFileName: String);
var
  l: TTMSFNCPDFIOExportObjectArray;
begin
  SetLength(l, 0);
  Save(AFileName, l);
end;

procedure TTMSFNCCustomPDFIO.Save(const AStream: TStream);
var
  l: TTMSFNCPDFIOExportObjectArray;
begin
  SetLength(l, 0);
  Save(AStream, l);
end;

procedure TTMSFNCCustomPDFIO.Save(const AStream: TStream; AExportObject: TTMSFNCPDFIOExportObject; AExportRect: TRectF);
var
  r: TTMSFNCPDFIOExportRectArray;
  o: TTMSFNCPDFIOExportObjectArray;
begin
  SetLength(r, 1);
  SetLength(o, 1);
  r[0] := AExportRect;
  o[0] := AExportObject;
  Save(AStream, o, r);
end;

procedure TTMSFNCCustomPDFIO.Save(const AFileName: String; AExportObject: TTMSFNCPDFIOExportObject; AExportRect: TRectF);
var
  r: TTMSFNCPDFIOExportRectArray;
  o: TTMSFNCPDFIOExportObjectArray;
begin
  SetLength(r, 1);
  SetLength(o, 1);
  r[0] := AExportRect;
  o[0] := AExportObject;
  Save(AFileName, o, r);
end;

procedure TTMSFNCCustomPDFIO.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  FBitmapContainer := Value;
end;

procedure TTMSFNCCustomPDFIO.SetInformation(
  const Value: TTMSFNCPDFIOInformation);
begin
  FInformation.Assign(Value);
end;

procedure TTMSFNCCustomPDFIO.SetOptions(const Value: TTMSFNCPDFIOOptions);
begin
  FOptions.Assign(Value);
end;

procedure TTMSFNCCustomPDFIO.Save(const AFileName: String;
  AExportObjects: TTMSFNCPDFIOExportObjectArray;
  AExportRects: TTMSFNCPDFIOExportRectArray);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    Save(ms, AExportObjects, AExportRects);
    {$IFDEF WEBLIB}
    if Options.OpenInPDFReader then
      ms.OpenFile('application/pdf');
    {$ELSE}
    ms.SaveToFile(AFileName);
    if Options.OpenInPDFReader then
      TTMSFNCUtils.OpenFile(AFileName);
    {$ENDIF}
  finally
    ms.Free;
  end;
end;

procedure TTMSFNCCustomPDFIO.Save(const AStream: TStream;
  AExportObjects: TTMSFNCPDFIOExportObjectArray;
  AExportRects: TTMSFNCPDFIOExportRectArray);
var
  pdflib: TTMSFNCPDFLib;
  I: Integer;
  ms: TMemoryStream;
  r: TRectF;
  x, y, w, h: Single;
begin
  pdflib := InitializePDFLib;
  try
    pdflib.BeginDocument;

    if Length(AExportObjects) = 0 then
    begin
      SetLength(AExportObjects, 1);
      AExportObjects[0] := ExportObject;
    end;

    for I := 0 to Length(AExportObjects) - 1 do
    begin
      FActiveExportObject := AExportObjects[I];
      if I <= Length(AExportRects) - 1 then
        r := AExportRects[I]
      else
      begin
        w := pdflib.MediaBox.Right - pdflib.MediaBox.Left;
        h := pdflib.MediaBox.Bottom - pdflib.MediaBox.Top;
        x := Options.Margins.Left;
        y := Options.Margins.Top;
        r := RectF(x, y, w - Options.Margins.Right, h - Options.Margins.Bottom);
      end;

      DoPDFExport(pdflib, AExportObjects[I], r);
      EndPage(pdflib, AExportObjects[I]);
    end;

    DoAfterDraw(pdflib);

    ms := pdflib.EndDocument;
    if Assigned(ms) then
    begin
      ms.SaveToStream(AStream);
      ms.Free;
    end;
  finally
    pdflib.Free;
  end;
end;

{ TTMSFNCPDFIOInformation }

procedure TTMSFNCPDFIOInformation.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCPDFIOInformation then
  begin
    FAuthor := (Source as TTMSFNCPDFIOInformation).Author;
    FCreator := (Source as TTMSFNCPDFIOInformation).Creator;
    FSubject := (Source as TTMSFNCPDFIOInformation).Subject;
    FTitle := (Source as TTMSFNCPDFIOInformation).Title;
    FKeywords.Assign((Source as TTMSFNCPDFIOInformation).Keywords);
  end;
end;

constructor TTMSFNCPDFIOInformation.Create;
begin
  FKeywords := TStringList.Create;
end;

destructor TTMSFNCPDFIOInformation.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TTMSFNCPDFIOInformation.SetKeywords(const Value: TStringList);
begin
  FKeywords.Assign(Value);
end;

{ TTMSFNCPDFIOOptions }

procedure TTMSFNCPDFIOOptions.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCPDFIOOptions then
  begin
    FMargins.Assign((Source as TTMSFNCPDFIOOptions).Margins);
    FOpenInPDFReader := (Source as TTMSFNCPDFIOOptions).OpenInPDFReader;
    FDefaultFont.Assign((Source as TTMSFNCPDFIOOptions).DefaultFont);
    FFooter := (Source as TTMSFNCPDFIOOptions).Footer;
    FHeader := (Source as TTMSFNCPDFIOOptions).Header;
    FHeaderFont.Assign((Source as TTMSFNCPDFIOOptions).HeaderFont);
    FFooterFont.Assign((Source as TTMSFNCPDFIOOptions).FooterFont);
    FHeaderMargins.Assign((Source as TTMSFNCPDFIOOptions).HeaderMargins);
    FFooterMargins.Assign((Source as TTMSFNCPDFIOOptions).FooterMargins);
    FFooterSize := (Source as TTMSFNCPDFIOOptions).FooterSize;
    FHeaderSize := (Source as TTMSFNCPDFIOOptions).HeaderSize;
    FFooterAlignment := (Source as TTMSFNCPDFIOOptions).FooterAlignment;
    FHeaderAlignment := (Source as TTMSFNCPDFIOOptions).HeaderAlignment;
    FEmbedFonts := (Source as TTMSFNCPDFIOOptions).EmbedFonts;
    FFontFallBackList.Assign((Source as TTMSFNCPDFIOOptions).FontFallBackList);
    FPageNumberFormat := (Source as TTMSFNCPDFIOOptions).PageNumberFormat;
    FPageNumber := (Source as TTMSFNCPDFIOOptions).PageNumber;
    FPageNumberFont.Assign((Source as TTMSFNCPDFIOOptions).PageNumberFont);
    FPageNumberAlignment := (Source as TTMSFNCPDFIOOptions).PageNumberAlignment;
    FPageNumberSize := (Source as TTMSFNCPDFIOOptions).PageNumberSize;
    FPageNumberMargins.Assign((Source as TTMSFNCPDFIOOptions).PageNumberMargins);
  end;
end;

constructor TTMSFNCPDFIOOptions.Create;
var
  r: TRectF;
begin
  r := RectF(5, 5, 5, 5);
  FEmbedFonts := True;
  FFontFallBackList := TStringList.Create;
  FOpenInPDFReader := False;
  FMargins := TTMSFNCMargins.Create(RectF(20, 50, 20, 50));
  FDefaultFont := TTMSFNCPDFGraphicsLibFont.Create;
  FExportImages := True;
  FHeaderFont := TTMSFNCPDFGraphicsLibFont.Create;
  FFooterFont :=  TTMSFNCPDFGraphicsLibFont.Create;
  FHeaderSize := 30;
  FFooterSize := 30;
  FPageNumberSize := 30;
  FHeaderMargins := TTMSFNCMargins.Create(r);
  FFooterMargins := TTMSFNCMargins.Create(r);
  FFooterAlignment := gtaCenter;
  FHeaderAlignment := gtaCenter;
  FPageSize := psLetter;
  FPageOrientation := poPortrait;
  FPageWidth := 0;
  FPageHeight := 0;
  r := RectF(10, 5, 10, 5);
  FPageNumberMargins := TTMSFNCMargins.Create(r);
  FPageNumber := pnHeader;
  FPageNumberFont := TTMSFNCPDFGraphicsLibFont.Create;
  FPageNumberAlignment := gtaTrailing;
end;

destructor TTMSFNCPDFIOOptions.Destroy;
begin
  FPageNumberFont.Free;
  FFontFallBackList.Free;
  FDefaultFont.Free;
  FMargins.Free;
  FFooterFont.Free;
  FHeaderFont.Free;
  FFooterMargins.Free;
  FHeaderMargins.Free;
  FPageNumberMargins.Free;
  inherited;
end;

function TTMSFNCPDFIOOptions.IsFooterSizeStored: Boolean;
begin
  Result := FooterSize <> 30;
end;

function TTMSFNCPDFIOOptions.IsHeaderSizeStored: Boolean;
begin
  Result := HeaderSize <> 30;
end;

function TTMSFNCPDFIOOptions.IsPageHeightStored: Boolean;
begin
  Result := PageHeight <> 0;
end;

function TTMSFNCPDFIOOptions.IsPageNumberSizeStored: Boolean;
begin
  Result := PageNumberSize <> 30;
end;

function TTMSFNCPDFIOOptions.IsPageWidthStored: Boolean;
begin
  Result := PageWidth <> 0;
end;

procedure TTMSFNCPDFIOOptions.SetDefaultFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetEmbedFonts(const Value: Boolean);
begin
  FEmbedFonts := Value;
end;

procedure TTMSFNCPDFIOOptions.SetExportImages(const Value: Boolean);
begin
  FExportImages := Value;
end;

procedure TTMSFNCPDFIOOptions.SetFontFallBackList(const Value: TStrings);
begin
  FFontFallBackList.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetFooterAlignment(
  const Value: TTMSFNCGraphicsTextAlign);
begin
  FFooterAlignment := Value;
end;

procedure TTMSFNCPDFIOOptions.SetFooterFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  FFooterMargins.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetFooterSize(const Value: Single);
begin
  FFooterSize := Value;
end;

procedure TTMSFNCPDFIOOptions.SetHeaderAlignment(
  const Value: TTMSFNCGraphicsTextAlign);
begin
  FHeaderAlignment := Value;
end;

procedure TTMSFNCPDFIOOptions.SetHeaderFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  FHeaderMargins.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetHeaderSize(const Value: Single);
begin
  FHeaderSize := Value;
end;

procedure TTMSFNCPDFIOOptions.SetMargins(const Value: TTMSFNCMargins);
begin
  FMargins.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageNumber(
  const Value: TTMSFNCPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageNumberAlignment(
  const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageNumberFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TTMSFNCPDFIOOptions.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageOrientation(
  const Value: TTMSFNCPDFLibPageOrientation);
begin
  FPageOrientation := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageSize(const Value: TTMSFNCPDFLibPageSize);
begin
  FPageSize := Value;
end;

procedure TTMSFNCPDFIOOptions.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
end;

end.
