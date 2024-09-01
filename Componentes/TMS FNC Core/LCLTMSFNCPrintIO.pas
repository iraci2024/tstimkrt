{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2021                                           }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit LCLTMSFNCPrintIO;

{$I LCLTMSFNCDefines.inc}
{$IFDEF WEBLIB}
  {$DEFINE ANDROIDIOSWEBLIB}
  {$DEFINE ANDROIDWEBLIB}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE ANDROIDIOSWEBLIB}
  {$DEFINE ANDROIDWEBLIB}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE ANDROIDIOSWEBLIB}
{$ENDIF}

interface

uses
  Classes, LCLTMSFNCCustomComponent, LCLTMSFNCTypes,
  LCLTMSFNCBitmapContainer, LCLTMSFNCGraphics, LCLTMSFNCGraphicsTypes, LCLTMSFNCPrinters
  {$IFNDEF LCLLIB}
  , Types, UITypes
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLib.Printers
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Printers
  {$ENDIF}
  ;

const
  {$IFDEF FMXLIB}
  PRINTDPI = 72;
  {$ENDIF}
  {$IFDEF CMNLIB}
  PRINTDPI = 96;
  {$ENDIF}
  {$IFDEF WEBLIB}
  PRINTDPI = 1;
  {$ENDIF}

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

type
  TTMSFNCPrintPageNumber = (pnNone, pnHeader, pnFooter);

  TTMSFNCPrintIOOptions = class(TPersistent)
  private
    FDefaultFont: TTMSFNCGraphicsFont;
    FDevice: string;
    FFooter: string;
    FFooterFont: TTMSFNCGraphicsFont;
    FFooterHorizontalAlignment: TTMSFNCGraphicsTextAlign;
    FFooterMargins: TTMSFNCMargins;
    FFooterSize: Single;
    FFooterVerticalAlignment: TTMSFNCGraphicsTextAlign;
    FHeader: string;
    FHeaderFont: TTMSFNCGraphicsFont;
    FHeaderHorizontalAlignment: TTMSFNCGraphicsTextAlign;
    FHeaderMargins: TTMSFNCMargins;
    FHeaderSize: Single;
    FHeaderVerticalAlignment: TTMSFNCGraphicsTextAlign;
    FMargins: TTMSFNCMargins;
    FPageNumber: TTMSFNCPrintPageNumber;
    FPageNumberFont: TTMSFNCGraphicsFont;
    FPageNumberFormat: UnicodeString;
    FPageNumberHorizontalAlignment: TTMSFNCGraphicsTextAlign;
    FPageNumberMargins: TTMSFNCMargins;
    FPageNumberSize: Single;
    FPageNumberVerticalAlignment: TTMSFNCGraphicsTextAlign;
    function GetDPI: integer;
    function GetPageHeight: integer;
    function GetPageIndex: Integer;
    function GetPageOrientation: TPrinterOrientation;
    function GetPageWidth: integer;
    function IsFooterSizeStored: Boolean;
    function IsHeaderSizeStored: Boolean;
    function IsPageNumberSizeStored: Boolean;
    procedure SetDefaultFont(const Value: TTMSFNCGraphicsFont);
    {$IFNDEF ANDROIDIOSWEBLIB}
    procedure SetDevice(const Value: string);
    {$ENDIF}
    procedure SetFooterFont(const Value: TTMSFNCGraphicsFont);
    procedure SetFooterHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFooterMargins(const Value: TTMSFNCMargins);
    procedure SetFooterSize(const Value: Single);
    procedure SetFooterVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetHeaderFont(const Value: TTMSFNCGraphicsFont);
    procedure SetHeaderHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetHeaderMargins(const Value: TTMSFNCMargins);
    procedure SetHeaderSize(const Value: Single);
    procedure SetHeaderVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumber(const Value: TTMSFNCPrintPageNumber);
    procedure SetPageNumberFont(const Value: TTMSFNCGraphicsFont);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetPageNumberMargins(const Value: TTMSFNCMargins);
    procedure SetPageNumberSize(const Value: Single);
    procedure SetPageNumberVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetPageOrientation(const Value: TPrinterOrientation);
    {$IFDEF ANDROID}
    function GetPageSize: TPrintSize;
    procedure SetPageSize(const Value: TPrintSize);
    {$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
    property DPI: integer read GetDPI;
    property PageHeight: integer read GetPageHeight;
    property PageIndex: Integer read GetPageIndex;
    property PageWidth: integer read GetPageWidth;
  published
    property DefaultFont: TTMSFNCGraphicsFont read FDefaultFont write SetDefaultFont;
    property Device: string read FDevice {$IFNDEF ANDROIDIOSWEBLIB}write SetDevice{$ENDIF};
    property Footer: string read FFooter write FFooter;
    property FooterFont: TTMSFNCGraphicsFont read FFooterFont write SetFooterFont;
    property FooterHorizontalAlignment: TTMSFNCGraphicsTextAlign read FFooterHorizontalAlignment write SetFooterHorizontalAlignment default gtaCenter;
    property FooterMargins: TTMSFNCMargins read FFooterMargins write SetFooterMargins;
    property FooterSize: Single read FFooterSize write SetFooterSize stored IsFooterSizeStored nodefault;
    property FooterVerticalAlignment: TTMSFNCGraphicsTextAlign read FFooterVerticalAlignment write SetFooterVerticalAlignment default gtaCenter;
    property Header: string read FHeader write FHeader;
    property HeaderFont: TTMSFNCGraphicsFont read FHeaderFont write SetHeaderFont;
    property HeaderHorizontalAlignment: TTMSFNCGraphicsTextAlign read FHeaderHorizontalAlignment write SetHeaderHorizontalAlignment default gtaCenter;
    property HeaderMargins: TTMSFNCMargins read FHeaderMargins write SetHeaderMargins;
    property HeaderSize: Single read FHeaderSize write SetHeaderSize stored IsHeaderSizeStored nodefault;
    property HeaderVerticalAlignment: TTMSFNCGraphicsTextAlign read FHeaderVerticalAlignment write SetHeaderVerticalAlignment default gtaCenter;
    property Margins: TTMSFNCMargins read FMargins write SetMargins;
    property PageNumber: TTMSFNCPrintPageNumber read FPageNumber write SetPageNumber default pnNone;
    property PageNumberFont: TTMSFNCGraphicsFont read FPageNumberFont write SetPageNumberFont;
    property PageNumberFormat: UnicodeString read FPageNumberFormat write SetPageNumberFormat;
    property PageNumberHorizontalAlignment: TTMSFNCGraphicsTextAlign read FPageNumberHorizontalAlignment write SetPageNumberHorizontalAlignment default gtaTrailing;
    property PageNumberMargins: TTMSFNCMargins read FPageNumberMargins write SetPageNumberMargins;
    property PageNumberSize: Single read FPageNumberSize write SetPageNumberSize stored IsPageNumberSizeStored nodefault;
    property PageNumberVerticalAlignment: TTMSFNCGraphicsTextAlign read FPageNumberVerticalAlignment write SetPageNumberVerticalAlignment default gtaCenter;
    property PageOrientation: TPrinterOrientation read GetPageOrientation write SetPageOrientation default TPrinterOrientation.poPortrait;
    {$IFDEF ANDROID}
    property PageSize: TPrintSize read GetPageSize write SetPageSize;
    {$ENDIF}
  end;

  TTMSFNCPrintIOExportObject = TComponent;

  TTMSFNCPrintIOBeforeDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var AHeader: string; var ADefaultDraw: Boolean) of object;
  TTMSFNCPrintIOAfterDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; AHeader: string) of object;
  TTMSFNCPrintIOBeforeDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var AFooter: string; var ADefaultDraw: Boolean) of object;
  TTMSFNCPrintIOAfterDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; AFooter: string) of object;
  TTMSFNCPrintIOBeforeDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var ADefaultDraw: Boolean) of object;
  TTMSFNCPrintIOAfterDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics) of object;
  TTMSFNCPrintBeforeDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer;  ARect: TRectF; AGraphics: TTMSFNCGraphics; var APageNumber: UnicodeString; var ADefaultDraw: Boolean) of object;
  TTMSFNCPrintAfterDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer;  ARect: TRectF; AGraphics: TTMSFNCGraphics; APageNumber: UnicodeString) of object;
  TTMSFNCPrintIOAfterDrawEvent = procedure(Sender: TObject; AGraphics: TTMSFNCGraphics) of object;
  TTMSFNCPrintIOExportRectEvent = procedure(Sender: TObject; const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ARect: TRectF) of object;
  TTMSFNCPrintIOCanCreateNewPageEvent = procedure(Sender: TObject; const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ACanCreate: Boolean) of object;

  TTMSFNCPrintIOExportObjectArray = array of TTMSFNCPrintIOExportObject;
  TTMSFNCPrintIOExportRectArray = array of TRectF;

  TTMSFNCCustomPrintIO = class(TTMSFNCCustomComponent, ITMSFNCBitmapContainer)
  private
    FActiveExportObject: TTMSFNCPrintIOExportObject;
    FBitmapContainer: TTMSFNCBitmapContainer;
    FOnCanCreateNewPage: TTMSFNCPrintIOCanCreateNewPageEvent;
    FExportObject: TTMSFNCPrintIOExportObject;
    FExportObjects: TTMSFNCPrintIOExportObjectArray;
    FExportRects: TTMSFNCPrintIOExportRectArray;
    FNewPageStarted: Boolean;
    FOptions: TTMSFNCPrintIOOptions;
    FOnAfterDraw: TTMSFNCPrintIOAfterDrawEvent;
    FOnAfterDrawContent: TTMSFNCPrintIOAfterDrawContentEvent;
    FOnAfterDrawFooter: TTMSFNCPrintIOAfterDrawFooterEvent;
    FOnAfterDrawHeader: TTMSFNCPrintIOAfterDrawHeaderEvent;
    FOnAfterDrawPageNumber: TTMSFNCPrintAfterDrawPageNumberEvent;
    FOnBeforeDrawContent: TTMSFNCPrintIOBeforeDrawContentEvent;
    FOnBeforeDrawFooter: TTMSFNCPrintIOBeforeDrawFooterEvent;
    FOnBeforeDrawHeader: TTMSFNCPrintIOBeforeDrawHeaderEvent;
    FOnBeforeDrawPageNumber: TTMSFNCPrintBeforeDrawPageNumberEvent;
    FOnGetExportRect: TTMSFNCPrintIOExportRectEvent;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    procedure PrintContent;
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetOptions(const Value: TTMSFNCPrintIOOptions);
  protected
    procedure CreateNewPage(const AGraphics: TTMSFNCGraphics);
    function CreateOptions: TTMSFNCPrintIOOptions; virtual;

    procedure DoAfterDraw(AGraphics: TTMSFNCGraphics); virtual;
    procedure DoAfterDrawContent(AExportObject: TTMSFNCPrintIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics); virtual;
    procedure DoAfterDrawFooter(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; AFooter: string);
    procedure DoAfterDrawHeader(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; AHeader: string);
    procedure DoAfterDrawPageNumber(APageIndex: Integer;  ARect: TRectF; AGraphics: TTMSFNCGraphics; APageNumber: string); virtual;
    procedure DoBeforeDrawContent(AExportObject: TTMSFNCPrintIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var AFooter: string; var ADefaultDraw: Boolean);
    procedure DoBeforeDrawHeader(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var AHeader: string; var ADefaultDraw: Boolean);
    procedure DoBeforeDrawPageNumber(APageIndex: Integer;  ARect: TRectF; AGraphics: TTMSFNCGraphics; var APageNumber: UnicodeString; var ADefaultDraw: Boolean); virtual;
    procedure DoCanCreateNewPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ACanCreate: Boolean); virtual;
    procedure DoGetExportRect(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ARect: TRectF); virtual;
    procedure DoPrintExport(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; const AExportRect: TRectF); overload; virtual;
    procedure DrawFooterHeader(const AGraphics: TTMSFNCGraphics); virtual;
    procedure DrawPageNumber(const AGraphics: TTMSFNCGraphics); virtual;
    procedure EndPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject); virtual;
    function GetContentRect(const AGraphics: TTMSFNCGraphics): TRectF; virtual;
    function GetHeaderRect: TRectF;
    function GetFooterRect: TRectF;
    function GetInstance: NativeUInt; override;
    function GetPageNumberRect: TRectF;
    function GetVersion: String; override;
    procedure NewPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
    property ExportObject: TTMSFNCPrintIOExportObject read FExportObject write FExportObject;
    property Options: TTMSFNCPrintIOOptions read FOptions write SetOptions;
    property Version: String read GetVersion;
    property OnAfterDraw: TTMSFNCPrintIOAfterDrawEvent read FOnAfterDraw write FOnAfterDraw;
    property OnAfterDrawContent: TTMSFNCPrintIOAfterDrawContentEvent read FOnAfterDrawContent write FOnAfterDrawContent;
    property OnAfterDrawFooter: TTMSFNCPrintIOAfterDrawFooterEvent read FOnAfterDrawFooter write FOnAfterDrawFooter;
    property OnAfterDrawHeader: TTMSFNCPrintIOAfterDrawHeaderEvent read FOnAfterDrawHeader write FOnAfterDrawHeader;
    property OnAfterDrawPageNumber: TTMSFNCPrintAfterDrawPageNumberEvent read FOnAfterDrawPageNumber write FOnAfterDrawPageNumber;
    property OnBeforeDrawContent: TTMSFNCPrintIOBeforeDrawContentEvent read FOnBeforeDrawContent write FOnBeforeDrawContent;
    property OnBeforeDrawFooter: TTMSFNCPrintIOBeforeDrawFooterEvent read FOnBeforeDrawFooter write FOnBeforeDrawFooter;
    property OnBeforeDrawHeader: TTMSFNCPrintIOBeforeDrawHeaderEvent read FOnBeforeDrawHeader write FOnBeforeDrawHeader;
    property OnBeforeDrawPageNumber: TTMSFNCPrintBeforeDrawPageNumberEvent read FOnBeforeDrawPageNumber write FOnBeforeDrawPageNumber;
    property OnCanCreateNewPage: TTMSFNCPrintIOCanCreateNewPageEvent read FOnCanCreateNewPage write FOnCanCreateNewPage;
    property OnGetExportRect: TTMSFNCPrintIOExportRectEvent read FOnGetExportRect write FOnGetExportRect;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Print; overload;
    procedure Print(AExportObject: TTMSFNCPrintIOExportObject; AExportRect: TRectF); overload;
    procedure Print(AExportObjects: TTMSFNCPrintIOExportObjectArray); overload;
    procedure Print(AExportObjects: TTMSFNCPrintIOExportObjectArray; AExportRects: TTMSFNCPrintIOExportRectArray); overload;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCGraphicsPrintIO = class(TTMSFNCCustomPrintIO)
  protected
    function GetDocURL: string; override;
  published
    property BitmapContainer;
    property ExportObject;
    property Options;
    property Version;
    property OnAfterDraw;
    property OnAfterDrawContent;
    property OnAfterDrawFooter;
    property OnAfterDrawHeader;
    property OnAfterDrawPageNumber;
    property OnBeforeDrawContent;
    property OnBeforeDrawFooter;
    property OnBeforeDrawHeader;
    property OnBeforeDrawPageNumber;
    property OnCanCreateNewPage;
    property OnGetExportRect;
  end;

implementation

uses
  SysUtils, LCLTMSFNCUtils
  {$IFDEF FMXLIB}
  , System.Math.Vectors
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;

{$R 'TMSFNCGraphicsPrintIO.res'}

{ TTMSFNCCustomPrintIO }

procedure TTMSFNCCustomPrintIO.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCustomPrintIO then
  begin
    FOptions.Assign((Source as TTMSFNCCustomPrintIO).Options);
    BitmapContainer:= (Source as TTMSFNCCustomPrintIO).BitmapContainer;
    ExportObject := (Source as TTMSFNCCustomPrintIO).ExportObject;
  end;
end;

constructor TTMSFNCCustomPrintIO.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := CreateOptions;

  setLength(FExportObjects, 0);
  setLength(FExportRects, 0);

  if IsDesignTime then
  begin
    FOptions.PageNumberFormat := '%d';
  end;
end;

procedure TTMSFNCCustomPrintIO.CreateNewPage(const AGraphics: TTMSFNCGraphics);
begin
  if Assigned(AGraphics) then
  begin
    TMSFNCPrinter.NewPage;
    DrawFooterHeader(AGraphics);
  end;
end;

function TTMSFNCCustomPrintIO.CreateOptions: TTMSFNCPrintIOOptions;
begin
  Result := TTMSFNCPrintIOOptions.Create;
end;

destructor TTMSFNCCustomPrintIO.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TTMSFNCCustomPrintIO.DoAfterDraw(AGraphics: TTMSFNCGraphics);
begin
  if Assigned(OnAfterDraw) then
    OnAfterDraw(Self, AGraphics);
end;

procedure TTMSFNCCustomPrintIO.DoAfterDrawContent(AExportObject: TTMSFNCPrintIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics);
begin
  if Assigned(OnAfterDrawContent) then
    OnAfterDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics);
end;

procedure TTMSFNCCustomPrintIO.DoAfterDrawFooter(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics;
  AFooter: string);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, AExportObject, APageIndex, ARect, AGraphics, AFooter);
end;

procedure TTMSFNCCustomPrintIO.DoAfterDrawHeader(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics;
  AHeader: string);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, AExportObject, APageIndex, ARect, AGraphics, AHeader);
end;

procedure TTMSFNCCustomPrintIO.DoAfterDrawPageNumber(APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; APageNumber: string);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, ARect, AGraphics, APageNumber);
end;

procedure TTMSFNCCustomPrintIO.DoBeforeDrawContent(AExportObject: TTMSFNCPrintIOExportObject; APageIndex: Integer;
  ARect: TRectF; AGraphics: TTMSFNCGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawContent) then
    OnBeforeDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics, ADefaultDraw);
end;

procedure TTMSFNCCustomPrintIO.DoBeforeDrawFooter(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics;
  var AFooter: string; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, AExportObject, APageIndex, ARect, AGraphics, AFooter, ADefaultDraw);
end;

procedure TTMSFNCCustomPrintIO.DoBeforeDrawHeader(AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics;
  var AHeader: string; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, AExportObject, APageIndex, ARect, AGraphics, AHeader, ADefaultDraw);
end;

procedure TTMSFNCCustomPrintIO.DoBeforeDrawPageNumber(APageIndex: Integer; ARect: TRectF; AGraphics: TTMSFNCGraphics; var APageNumber: UnicodeString; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, ARect, AGraphics, APageNumber, ADefaultDraw);
end;

procedure TTMSFNCCustomPrintIO.DoCanCreateNewPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ACanCreate: Boolean);
begin
  if Assigned(OnCanCreateNewPage) then
    OnCanCreateNewPage(Self, AGraphics, AExportObject, ACanCreate);
end;

procedure TTMSFNCCustomPrintIO.DoGetExportRect(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; var ARect: TRectF);
begin
  if Assigned(OnGetExportRect) then
    OnGetExportRect(Self, AGraphics, AExportObject, ARect);
end;

procedure TTMSFNCCustomPrintIO.DoPrintExport(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject; const AExportRect: TRectF);
var
  e: ITMSFNCGraphicsExport;
  bmp: ITMSFNCBitmapContainer;
  r: TRectF;
begin
  if Assigned(AExportObject) and Supports(AExportObject, ITMSFNCGraphicsExport, e) then
  begin
    if Supports(AExportObject, ITMSFNCBitmapContainer, bmp) then
      AGraphics.BitmapContainer := bmp.BitmapContainer;

    r := AExportRect;
    DoGetExportRect(AGraphics, AExportObject, r);
    e.Export(AGraphics,r);
  end;
end;

procedure TTMSFNCCustomPrintIO.DrawFooterHeader(const AGraphics: TTMSFNCGraphics);
var
  df: Boolean;
  r: TRectF;
  h, f: string;
  ft: TTMSFNCGraphicsFont;
begin
  ft := TTMSFNCGraphicsFont.Create;
  ft.Assign(AGraphics.Font);
  try
    if (Options.Header <> '') and (Options.HeaderSize > 0) then
    begin
      h := FOptions.Header;
      r := GetHeaderRect;
      AGraphics.Font.Assign(FOptions.HeaderFont);

      df := True;
        DoBeforeDrawHeader(FExportObject, FOptions.PageIndex, r, AGraphics, h, df);
      if df then
      begin
        AGraphics.DrawText(r, h, True, FOptions.HeaderHorizontalAlignment, FOptions.HeaderVerticalAlignment);

        DoAfterDrawHeader(FExportObject, FOptions.PageIndex, r, AGraphics, h);
      end;
    end;

    if (Options.Footer <> '') and (Options.FooterSize > 0) then
    begin
      f := FOptions.Footer;
      r := GetFooterRect;
      AGraphics.Font.Assign(FOptions.FooterFont);

      df := True;
      DoBeforeDrawFooter(FExportObject, FOptions.PageIndex, r, AGraphics, f, df);
      if df then
      begin
        AGraphics.DrawText(r, f, True, FOptions.FooterHorizontalAlignment, FOptions.FooterVerticalAlignment);
        DoAfterDrawFooter(FExportObject, FOptions.PageIndex, r, AGraphics, f);
      end;
    end;

    DrawPageNumber(AGraphics);
  finally
    AGraphics.Font.Assign(ft);
    ft.Free;
  end;
end;

procedure TTMSFNCCustomPrintIO.DrawPageNumber(const AGraphics: TTMSFNCGraphics);
var
  r: TRectF;
  df: Boolean;
  s: UnicodeString;
  pi: integer;
begin
  if not Assigned(AGraphics) or (FOptions.PageNumber = pnNone) then
    Exit;

  AGraphics.Font.Assign(FOptions.PageNumberFont);
  pi := FOptions.GetPageIndex;
  r := GetPageNumberRect;

  df := True;
  {$IFDEF LCLLIB}
  s := UTF8Decode(Format(UTF8Encode(FOptions.PageNumberFormat), [pi]));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  s := Format(FOptions.PageNumberFormat, [pi]);
  {$ENDIF}
  DoBeforeDrawPageNumber(pi, r, AGraphics, s, df);
  if df then
  begin
    AGraphics.DrawText(r, s, True, FOptions.PageNumberHorizontalAlignment, FOptions.PageNumberVerticalAlignment);

    DoAfterDrawPageNumber(pi, r, AGraphics, s);
  end;
end;

procedure TTMSFNCCustomPrintIO.EndPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject);
begin
  if FNewPageStarted then
    FNewPageStarted := False;
end;

function TTMSFNCCustomPrintIO.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := FBitmapContainer;
end;

function TTMSFNCCustomPrintIO.GetContentRect(const AGraphics: TTMSFNCGraphics): TRectF;
begin
  Result := RectF(0, FOptions.PageWidth, 0, FOptions.PageHeight);
  if Assigned(AGraphics) then
  begin
    Result := RectF(Result.Left + Options.Margins.Left, Result.Top + Options.Margins.Top,
      Result.Right - Options.Margins.Right, Result.Bottom - Options.Margins.Bottom);
  end;
end;

function TTMSFNCCustomPrintIO.GetFooterRect: TRectF;
begin
  Result := RectF(FOptions.FFooterMargins.Left, FOptions.PageHeight - FOptions.FFooterSize - FOptions.FFooterMargins.Bottom,
              FOptions.PageWidth - FOptions.FFooterMargins.Right, FOptions.PageHeight - FOptions.FFooterMargins.Bottom);
end;

function TTMSFNCCustomPrintIO.GetHeaderRect: TRectF;
begin
  Result := RectF(FOptions.FHeaderMargins.Left, FOptions.FHeaderMargins.Top,
              FOptions.PageWidth - FOptions.FHeaderMargins.Right, FOptions.FHeaderMargins.Top + FOptions.FHeaderSize);
end;

function TTMSFNCCustomPrintIO.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomPrintIO.GetPageNumberRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  case FOptions.PageNumber of
    pnHeader: Result := RectF(FOptions.FPageNumberMargins.Left, FOptions.FPageNumberMargins.Top,
                          FOptions.PageWidth - FOptions.FPageNumberMargins.Right, FOptions.FPageNumberMargins.Top + FOptions.FPageNumberSize);
    pnFooter: Result := RectF(FOptions.FPageNumberMargins.Left, FOptions.PageHeight - FOptions.FPageNumberSize - FOptions.FPageNumberMargins.Bottom,
                          FOptions.PageWidth - FOptions.FPageNumberMargins.Right, FOptions.PageHeight - FOptions.FPageNumberMargins.Bottom);
  end;
end;

function TTMSFNCCustomPrintIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCCustomPrintIO.NewPage(const AGraphics: TTMSFNCGraphics; const AExportObject: TTMSFNCPrintIOExportObject);
begin
  if Assigned(AGraphics) then
  begin
    EndPage(AGraphics, AExportObject);
    AGraphics.Font.Assign(Options.DefaultFont);
    CreateNewPage(AGraphics);
    FNewPageStarted := True;
  end;
end;

procedure TTMSFNCCustomPrintIO.Notification(AComponent: TComponent;
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

procedure TTMSFNCCustomPrintIO.Print(AExportObject: TTMSFNCPrintIOExportObject;
  AExportRect: TRectF);
var
  r: TTMSFNCPrintIOExportRectArray;
  o: TTMSFNCPrintIOExportObjectArray;
begin
  SetLength(r, 1);
  SetLength(o, 1);
  r[0] := AExportRect;
  o[0] := AExportObject;
  Print(o, r);
end;

procedure TTMSFNCCustomPrintIO.Print(AExportObjects: TTMSFNCPrintIOExportObjectArray);
var
  r: TTMSFNCPrintIOExportRectArray;
begin
  SetLength(r, 0);
  Print(AExportObjects, r);
end;

procedure TTMSFNCCustomPrintIO.Print;
var
  l: TTMSFNCPrintIOExportObjectArray;
begin
  SetLength(l, 0);
  Print(l);
end;

procedure TTMSFNCCustomPrintIO.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  FBitmapContainer := Value;
end;

procedure TTMSFNCCustomPrintIO.SetOptions(const Value: TTMSFNCPrintIOOptions);
begin
  FOptions.Assign(Value);
end;

procedure TTMSFNCCustomPrintIO.Print(AExportObjects: TTMSFNCPrintIOExportObjectArray;
  AExportRects: TTMSFNCPrintIOExportRectArray);
begin
  setLength(FExportObjects, 0);
  setLength(FExportRects, 0);
  {$IFNDEF LCLLIB}
  TMSFNCPrinter.OnDrawContent := PrintContent;
  {$ENDIF}
  {$IFDEF LCLLIB}
  TMSFNCPrinter.OnDrawContent := @PrintContent;
  {$ENDIF}
  FExportObjects := AExportObjects;
  FExportRects := AExportRects;

  TMSFNCPrinter.BeginDoc;
end;

procedure TTMSFNCCustomPrintIO.PrintContent;
var
  g : TTMSFNCGraphics;
  I: integer;
  w, h, x, y: single;
  r: TRectF;
  a: Boolean;
begin
  g := TMSFNCPrinter.Graphics;

  try
    if Length(FExportObjects) = 0 then
    begin
      SetLength(FExportObjects, 1);
      FExportObjects[0] := ExportObject;
    end;

    for I := 0 to Length(FExportObjects) - 1 do
    begin
      FActiveExportObject := FExportObjects[I];
      if I <= Length(FExportRects) - 1 then
        r := FExportRects[I]
      else
      begin
        w := FOptions.PageWidth;
        h := FOptions.PageHeight;
        x := Options.Margins.Left;
        y := Options.Margins.Top;
        r := RectF(x, y, w - Options.Margins.Right, h - Options.Margins.Bottom);
      end;

      if (Length(FExportObjects) > 1) and (I > 0) then
      begin
        a := True;
        DoCanCreateNewPage(g, FExportObjects[I], a);
        if a then
          NewPage(g, FExportObjects[I]);
      end
      else
      begin
        DrawFooterHeader(g);
      end;

      a := True;
      DoBeforeDrawContent(FExportObjects[I], TMSFNCPrinter.PageNumber, r, g, a);
      if a then
        DoPrintExport(g, FExportObjects[I], r);

      DoAfterDrawContent(FExportObjects[I], TMSFNCPrinter.PageNumber, r, g);
      EndPage(g, FExportObjects[I]);
    end;

    DoAfterDraw(g);
  finally
    TMSFNCPrinter.EndDoc;
  end;
end;

{ TTMSFNCPrintIOOptions }

procedure TTMSFNCPrintIOOptions.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCPrintIOOptions then
  begin
    FMargins.Assign((Source as TTMSFNCPrintIOOptions).Margins);
    {$IFNDEF ANDROIDIOSWEBLIB}
    Device := (Source as TTMSFNCPrintIOOptions).Device;
    {$ENDIF}
    {$IFDEF ANDROIDIOSWEBLIB}
    FDevice := (Source as TTMSFNCPrintIOOptions).Device;
    {$ENDIF}
    FDefaultFont.Assign((Source as TTMSFNCPrintIOOptions).DefaultFont);
    FFooter := (Source as TTMSFNCPrintIOOptions).Footer;
    FHeader := (Source as TTMSFNCPrintIOOptions).Header;
    FHeaderFont.Assign((Source as TTMSFNCPrintIOOptions).HeaderFont);
    FFooterFont.Assign((Source as TTMSFNCPrintIOOptions).FooterFont);
    FHeaderMargins.Assign((Source as TTMSFNCPrintIOOptions).HeaderMargins);
    FFooterMargins.Assign((Source as TTMSFNCPrintIOOptions).FooterMargins);
    FFooterSize := (Source as TTMSFNCPrintIOOptions).FooterSize;
    FHeaderSize := (Source as TTMSFNCPrintIOOptions).HeaderSize;
    FFooterVerticalAlignment := (Source as TTMSFNCPrintIOOptions).FooterVerticalAlignment;
    FHeaderVerticalAlignment := (Source as TTMSFNCPrintIOOptions).HeaderVerticalAlignment;
    FFooterHorizontalAlignment := (Source as TTMSFNCPrintIOOptions).FooterHorizontalAlignment;
    FHeaderHorizontalAlignment := (Source as TTMSFNCPrintIOOptions).HeaderHorizontalAlignment;
    FPageNumberFormat := (Source as TTMSFNCPrintIOOptions).PageNumberFormat;
    FPageNumber := (Source as TTMSFNCPrintIOOptions).PageNumber;
    FPageNumberFont.Assign((Source as TTMSFNCPrintIOOptions).PageNumberFont);
    FPageNumberHorizontalAlignment := (Source as TTMSFNCPrintIOOptions).PageNumberHorizontalAlignment;
    FPageNumberVerticalAlignment := (Source as TTMSFNCPrintIOOptions).PageNumberVerticalAlignment;
    FPageNumberSize := (Source as TTMSFNCPrintIOOptions).PageNumberSize;
    FPageNumberMargins.Assign((Source as TTMSFNCPrintIOOptions).PageNumberMargins);
    FMargins := (Source as TTMSFNCPrintIOOptions).Margins;
  end;
end;

constructor TTMSFNCPrintIOOptions.Create;
begin
  FMargins := TTMSFNCMargins.Create(RectF(20, 50, 20, 50));
  FDefaultFont := TTMSFNCGraphicsFont.Create;
  FHeaderFont := TTMSFNCGraphicsFont.Create;
  FFooterFont := TTMSFNCGraphicsFont.Create;
  FHeaderSize := 30;
  FFooterSize := 30;
  FHeaderMargins := TTMSFNCMargins.Create(RectF(20, 10, 20, 10));
  FFooterMargins := TTMSFNCMargins.Create(RectF(20, 10, 20, 10));
  FHeaderVerticalAlignment := gtaCenter;
  FHeaderHorizontalAlignment := gtaCenter;
  FFooterVerticalAlignment := gtaCenter;
  FFooterHorizontalAlignment := gtaCenter;
  FPageNumberSize := 30;
  FPageNumberMargins := TTMSFNCMargins.Create(RectF(20, 10, 20, 10));
  FPageNumber := pnNone;
  FPageNumberFont := TTMSFNCGraphicsFont.Create;
  FPageNumberHorizontalAlignment := gtaTrailing;
  FPageNumberVerticalAlignment := gtaCenter;
end;

destructor TTMSFNCPrintIOOptions.Destroy;
begin
  FDefaultFont.Free;
  FMargins.Free;
  FPageNumberFont.Free;
  FFooterFont.Free;
  FHeaderFont.Free;
  FFooterMargins.Free;
  FHeaderMargins.Free;
  FPageNumberMargins.Free;
  inherited;
end;

function TTMSFNCPrintIOOptions.GetDPI: integer;
begin
  Result:= TMSFNCPrinter.DPI;
end;

function TTMSFNCPrintIOOptions.GetPageHeight: Integer;
begin
  {$IFDEF ANDROIDIOSLIB}
  Result := Round(TMSFNCPrinter.PageHeight * PRINTDPI);
  {$ENDIF}
  {$IFNDEF ANDROIDIOSLIB}
  Result := TMSFNCPrinter.PageHeight;
  {$ENDIF}
end;

function TTMSFNCPrintIOOptions.GetPageIndex: Integer;
begin
  Result := TMSFNCPrinter.PageNumber {$IFDEF LCLLIB} - 1 {$ENDIF};
end;

function TTMSFNCPrintIOOptions.GetPageOrientation: TPrinterOrientation;
begin
  Result := TMSFNCPrinter.Orientation;
end;

{$IFDEF ANDROID}
function TTMSFNCPrintIOOptions.GetPageSize: TPrintSize;
begin
  Result := TMSFNCPrinter.PrintSize;
end;
{$ENDIF}

function TTMSFNCPrintIOOptions.GetPageWidth: Integer;
begin
  {$IFDEF ANDROIDIOSLIB}
  Result := Round(TMSFNCPrinter.PageWidth * PRINTDPI);
  {$ENDIF}
  {$IFNDEF ANDROIDIOSLIB}
  Result := TMSFNCPrinter.PageWidth;
  {$ENDIF}
end;

function TTMSFNCPrintIOOptions.IsFooterSizeStored: Boolean;
begin
  Result := FooterSize <> 30;
end;

function TTMSFNCPrintIOOptions.IsHeaderSizeStored: Boolean;
begin
  Result := HeaderSize <> 30;
end;

function TTMSFNCPrintIOOptions.IsPageNumberSizeStored: Boolean;
begin
  Result := PageNumberSize <> 30;
end;

procedure TTMSFNCPrintIOOptions.SetDefaultFont(const Value: TTMSFNCGraphicsFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetMargins(const Value: TTMSFNCMargins);
begin
  FMargins.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetPageNumber(const Value: TTMSFNCPrintPageNumber);
begin
  FPageNumber := Value;
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberFont(const Value: TTMSFNCGraphicsFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberHorizontalAlignment := Value;
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberMargins(const Value: TTMSFNCMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TTMSFNCPrintIOOptions.SetPageNumberVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FPageNumberVerticalAlignment := Value;
end;

procedure TTMSFNCPrintIOOptions.SetPageOrientation(const Value: TPrinterOrientation);
begin
  TMSFNCPrinter.Orientation := Value;
end;

{$IFDEF ANDROID}
procedure TTMSFNCPrintIOOptions.SetPageSize(const Value: TPrintSize);
begin
  TMSFNCPrinter.PrintSize := Value;
end;
{$ENDIF}

{$IFNDEF ANDROIDIOSWEBLIB}
procedure TTMSFNCPrintIOOptions.SetDevice(const Value: String);
begin
  TMSFNCPrinter.Device := Value;
end;
{$ENDIF}

procedure TTMSFNCPrintIOOptions.SetFooterFont(const Value: TTMSFNCGraphicsFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetFooterHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FFooterHorizontalAlignment := Value;
end;

procedure TTMSFNCPrintIOOptions.SetFooterMargins(const Value: TTMSFNCMargins);
begin
  FFooterMargins.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetFooterSize(const Value: Single);
begin
  if FFooterSize <> Value then
    FFooterSize := Value;
end;

procedure TTMSFNCPrintIOOptions.SetFooterVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FFooterVerticalAlignment := Value;
end;

procedure TTMSFNCPrintIOOptions.SetHeaderFont(const Value: TTMSFNCGraphicsFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetHeaderHorizontalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FHeaderHorizontalAlignment := Value;
end;

procedure TTMSFNCPrintIOOptions.SetHeaderMargins(const Value: TTMSFNCMargins);
begin
  FHeaderMargins.Assign(Value);
end;

procedure TTMSFNCPrintIOOptions.SetHeaderSize(const Value: Single);
begin
  if FHeaderSize <> Value then
    FHeaderSize := Value;
end;

procedure TTMSFNCPrintIOOptions.SetHeaderVerticalAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  FHeaderVerticalAlignment := Value;
end;

{ TTMSFNCGraphicsPrintIO }

function TTMSFNCGraphicsPrintIO.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfnccore/components/ttmsfncgraphicsprintio';
end;

end.
