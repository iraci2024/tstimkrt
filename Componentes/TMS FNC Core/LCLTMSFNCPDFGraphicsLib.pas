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

unit LCLTMSFNCPDFGraphicsLib;

{$I LCLTMSFNCDefines.inc}

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

interface

uses
  Classes, LCLTMSFNCPDFCoreLibBase, LCLTMSFNCPDFRichTextLib, LCLTMSFNCTypes, LCLTMSFNCBitmapContainer,
  LCLTMSFNCGraphicsTypes
  {$IFNDEF LCLWEBLIB}
  ,Types, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  ITMSFNCCustomPDFGraphicsLib = interface(IInterface)
  ['{EDB1C5AC-6E2B-4B0C-BA5C-884837A6DFF2}']
    procedure SetFill(const Value: TTMSFNCPDFGraphicsFill);
    procedure SetStroke(const Value: TTMSFNCPDFGraphicsStroke);
    procedure SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetLineBreakMode(const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawEllipse(Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure AddGoTo(AText: Unicodestring; ADestination: UnicodeString; ARect: TRectF);
    procedure DrawSetTransform(m11, m12, m21, m22, dx, dy: Double); overload;
    procedure DrawSetTransform(m: TTMSFNCGraphicsMatrix); overload;
    procedure DrawSaveState;
    procedure DrawClear(Rect: TRectF); overload;
    procedure DrawRestoreState;
    procedure DrawPathBegin;
    procedure DrawPathBeginClip;
    procedure DrawPathEndClip;
    procedure DrawPathEndLinearGradient(StartPoint, EndPoint: TPointF);
    procedure DrawPathMoveToPoint(Point: TPointF);
    procedure DrawPathAddLineToPoint(Point: TPointF);
    procedure DrawPathAddRectangle(Rect: TRectF);
    procedure DrawPathAddEllipse(Rect: TRectF);
    procedure DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
    procedure DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
    procedure DrawPathAddQuadCurveToPoint(ControlPoint: TPointF; EndPoint: TPointF);
    procedure DrawPathClose;
    procedure DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
    procedure SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
    function CalculateHTMLText(Text: UnicodeString; Scale: Single = 1.0): TRectF; overload;
    function CalculateHTMLText(Text: UnicodeString; Rect: TRectF; Scale: Single = 1.0): TRectF; overload;
    function CalculateText(Text: UnicodeString): TRectF; overload;
    function CalculateText(Text: UnicodeString; Rect: TRectF): TRectF; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0): Integer; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0): Integer; overload;
    function DrawImageWithName(AName: string; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(AName: string; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(AName: string; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageWithName(AName: string; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Point: TPointF; Scale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; Scale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    function GetTextRect: TRectF;
    function GetAlignment: TTMSFNCGraphicsTextAlign;
    function GetFill: TTMSFNCPDFGraphicsFill;
    function GetStroke: TTMSFNCPDFGraphicsStroke;
    function GetFont: TTMSFNCPDFGraphicsLibFont;
    function GetURLFont: TTMSFNCPDFGraphicsLibFont;
    function GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
    property Alignment: TTMSFNCGraphicsTextAlign read GetAlignment write SetAlignment;
    property Fill: TTMSFNCPDFGraphicsFill read GetFill write SetFill;
    property Stroke: TTMSFNCPDFGraphicsStroke read GetStroke write SetStroke;
    property Font: TTMSFNCPDFGraphicsLibFont read GetFont write SetFont;
    property URLFont: TTMSFNCPDFGraphicsLibFont read GetURLFont write SetURLFont;
    property LineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode read GetLineBreakMode write SetLineBreakMode;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
  end;

  ITMSFNCCustomPDFInitializationLib = interface(IInterface)
  ['{AAC3B710-EEBB-46BE-8450-3D70123B49CA}']
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
    procedure InitializeAppearance;
    procedure NotifyNewPage;
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function GetOnNotifyNewPage: TNotifyEvent;
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  end;

  ITMSFNCCustomPDFGraphicsExLib = interface(IInterface)
  ['{4BA4B9BC-CBD2-4852-BE93-573753A37365}']
    procedure SetPDFRichTextLib(const Value: ITMSFNCCustomPDFRichTextLib);
    procedure DrawAddShadow(Offset: TPointF; Blur: Single); overload;
    procedure DrawAddShadow(Offset: TPointF; Blur: Single; Color: TTMSFNCGraphicsColor); overload;
    procedure DrawRoundedRectangle(Rect: TRectF; Rounding: Single);
    procedure DrawPathAddArc(CenterPoint: TPointF; Radius: Single; StartAngle, EndAngle: Single; Clockwise: Boolean = False);
    procedure DrawPathAddArcToPoint(FirstPoint, SecondPoint: TPointF; Radius: Single);
    procedure DrawPathEndRadialGradient(StartCenter, EndCenter: TPointF; StartRadius, EndRadius: Single);
    function DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function RichText: ITMSFNCCustomPDFRichTextLib;
  end;

  ITMSFNCPDFGraphicsLibService = interface(IInterface)
  ['{033FCA68-4F8F-4916-9B59-67FF54E18BCC}']
    function CreatePDFGraphicsLib: TObject;
  end;

  ITMSFNCPDFGraphicsLibGeneralService = interface(IInterface)
  ['{0F974932-4204-489D-95DE-DBEFF4DEF5D7}']
    function CreatePDFGraphicsLib: TObject;
  end;

  { TTMSFNCCustomPDFGraphicsLib }

  TTMSFNCCustomPDFGraphicsLib = class(TPersistent)
  private
    FPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    FPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    FPDFRichTextLib: TTMSFNCPDFRichTextLib;
    FPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
  public
    constructor Create; overload; virtual;
    constructor Create({%H-}AUseNativePDFImplementation: Boolean); reintroduce; overload; virtual;
    destructor Destroy; override;
    function GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPDFGraphicsLibList = class(TList)
  private
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
  public
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPDFGraphicsLibList = class(specialize TFPGList<TObject>);
  {$ENDIF}

  TTMSFNCPDFGraphicsLibFactoryService = class(TInterfacedObject, ITMSFNCPDFGraphicsLibService, ITMSFNCPDFGraphicsLibGeneralService)
  private
    FPDFGraphicsLibs: TTMSFNCPDFGraphicsLibList;
  protected
    function DoCreatePDFGraphicsLib: TObject; virtual; abstract;
    function CreatePDFGraphicsLib: TObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSFNCPDFGraphicsLib = class(TTMSFNCCustomPDFGraphicsLib)
  end;

implementation

uses
{$IFDEF MACOS}
{$IFDEF IOS}
  LCLTMSFNCPDFGraphicsLib.iOS,
{$ELSE}
  LCLTMSFNCPDFGraphicsLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  LCLTMSFNCPDFGraphicsLib.Android,
{$ENDIF}
  LCLTMSFNCPDFGraphicsLibGeneral,
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

{ TTMSFNCPDFGraphicsLibFactoryService }

constructor TTMSFNCPDFGraphicsLibFactoryService.Create;
begin
  inherited Create;
  FPDFGraphicsLibs := TTMSFNCPDFGraphicsLibList.Create;
end;

function TTMSFNCPDFGraphicsLibFactoryService.CreatePDFGraphicsLib: TObject;
begin
  Result := DoCreatePDFGraphicsLib;
  FPDFGraphicsLibs.Add(Result);
end;

destructor TTMSFNCPDFGraphicsLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFGraphicsLibs);
  inherited Destroy;
end;

{ TTMSFNCCustomPDFGraphicsLib }

constructor TTMSFNCCustomPDFGraphicsLib.Create;
begin
  Create(True);
end;

constructor TTMSFNCCustomPDFGraphicsLib.Create(
  AUseNativePDFImplementation: Boolean);
var
  PDFGraphicsLibServiceGeneral: ITMSFNCPDFGraphicsLibGeneralService;
  {$IFDEF USENATIVE}
  PDFGraphicsLibService: ITMSFNCPDFGraphicsLibService;
  {$ENDIF}
  o: TObject;
begin
  {$IFDEF USENATIVE}
  if AUseNativePDFImplementation then
  begin
    if TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibService, IInterface(PDFGraphicsLibService)) then
    begin
      o := PDFGraphicsLibService.CreatePDFGraphicsLib;
      Supports(o, ITMSFNCCustomPDFGraphicsLib, FPDFGraphicsLib);
      Supports(o, ITMSFNCCustomPDFGraphicsExLib, FPDFGraphicsExLib);
      Supports(o, ITMSFNCCustomPDFInitializationLib, FPDFInitializationLib);
    end;
  end
  else
  {$ENDIF}
  begin
    if TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibGeneralService, IInterface(PDFGraphicsLibServiceGeneral)) then
    begin
      o := PDFGraphicsLibServiceGeneral.CreatePDFGraphicsLib;
      Supports(o, ITMSFNCCustomPDFGraphicsLib, FPDFGraphicsLib);
      Supports(o, ITMSFNCCustomPDFGraphicsExLib, FPDFGraphicsExLib);
      Supports(o, ITMSFNCCustomPDFInitializationLib, FPDFInitializationLib);
      {$IFDEF WEBLIB}
      FPDFGraphicsLib._AddRef;
      FPDFGraphicsExLib._AddRef;
      FPDFInitializationLib._AddRef;
      {$ENDIF}
    end;
  end;

  FPDFRichTextLib := TTMSFNCPDFRichTextLib.Create;
  FPDFGraphicsExLib.SetPDFRichTextLib(FPDFRichTextLib.GetPDFRichTextLib);
end;

destructor TTMSFNCCustomPDFGraphicsLib.Destroy;
begin
  if Assigned(FPDFRichTextLib) then
  begin
    FPDFRichTextLib.Free;
    FPDFRichTextLib := nil;
  end;

  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib := nil;

  inherited;
end;

function TTMSFNCCustomPDFGraphicsLib.GetPDFGraphicsExLib: ITMSFNCCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TTMSFNCCustomPDFGraphicsLib.GetPDFGraphicsLib: ITMSFNCCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TTMSFNCCustomPDFGraphicsLib.GetPDFInitializationLib: ITMSFNCCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

{$IFDEF WEBLIB}
function TTMSFNCPDFGraphicsLibList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(inherited Items[Index]);
end;

procedure TTMSFNCPDFGraphicsLibList.SetItem(Index: Integer; const Value: TObject);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

initialization
begin
  {$IFDEF USENATIVE}
  RegisterPDFGraphicsLibService;
  {$ENDIF}
  RegisterPDFGraphicsLibGeneralService;
end;

{$IFNDEF WEBLIB}
finalization
begin
  UnRegisterPDFGraphicsLibGeneralService;
  {$IFDEF USENATIVE}
  UnRegisterPDFGraphicsLibService;
  {$ENDIF}
end;
{$ENDIF}

end.
