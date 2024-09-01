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

unit VCL.TMSFNCPDFRichTextLib;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.TMSFNCPDFCoreLibBase, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCTypes
  {$IFDEF WEBLIB}
  ,Contnrs
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,Types, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  TTMSFNCPDFRichTextLibRange = record
    location: Integer;
    length: Integer;
  end;

  TTMSFNCPDFRichTextLibDataType = (
    dtArchivedXMLDocumentType,
    dtPlainTextDocumentType,
    dtRTFTextDocumentType,
    dtRTFDTextDocumentType,
    dtHTMLTextDocumentType,
    dtSimpleTextDocumentType,
    dtDocFormatTextDocumentType,
    dtWordMLTextDocumentType,
    dtOfficeOpenXMLTextDocumentType,
    dtWebArchiveTextDocumentType,
    dtOpenDocumentTextDocumentType
  );

  TTMSFNCPDFRichTextLibAttributeName = (
     anFontAttributeName,
     anParagraphStyleAttributeName,
     anForegroundColorAttributeName,
     anBackgroundColorAttributeName,
     anLigatureAttributeName,
     anKernAttributeName,
     anStrikethroughStyleAttributeName,
     anUnderlineStyleAttributeName,
     anStrokeColorAttributeName,
     anStrokeWidthAttributeName,
     anShadowAttributeName,
     anTextEffectAttributeName,
     anAttachmentAttributeName,
     anLinkAttributeName,
     anToolTipAttributeName,
     anBaselineOffsetAttributeName,
     anUnderlineColorAttributeName,
     anStrikethroughColorAttributeName,
     anObliquenessAttributeName,
     anExpansionAttributeName,
     anWritingDirectionAttributeName,
     anVerticalGlyphFormAttributeName
  );

  TTMSFNCPDFRichTextLibUnderlineStyle = (
    usUnderlineStyleNone,
    usUnderlineStyleSingle,
    usUnderlineStyleThick,
    usUnderlineStyleDouble,
    usUnderlinePatternSolid,
    usUnderlinePatternDot,
    usUnderlinePatternDash,
    usUnderlinePatternDashDot,
    usUnderlinePatternDashDotDot,
    usUnderlineByWord
  );

  TTMSFNCPDFRichTextLibUnderlineStyles = set of TTMSFNCPDFRichTextLibUnderlineStyle;

  TTMSFNCPDFRichTextLibFontValue = record
    FontFamily: String;
    FontName: String;
    FontSize: Single;
  end;

  TTMSFNCPDFRichTextLibTabStop = record
    Location: Single;
    Alignment: TTMSFNCGraphicsTextAlign;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TTMSFNCPDFRichTextLibTabStop) b : Boolean;
    {$ENDIF}
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPDFRichTextLibTabStops = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCPDFRichTextLibTabStop;
    procedure SetItem(Index: Integer; const Value: TTMSFNCPDFRichTextLibTabStop);
  public
    property Items[Index: Integer]: TTMSFNCPDFRichTextLibTabStop read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPDFRichTextLibTabStops = class(TList<TTMSFNCPDFRichTextLibTabStop>);
  {$ENDIF}

  TTMSFNCPDFRichTextLibParagraphStyle = record
    Alignment: TTMSFNCGraphicsTextAlign;
    FirstLineHeadIndent: Single;
    HeadIndent: Single;
    TailIndent: Single;
    LineHeightMultiple: Single;
    MaximumLineHeight: Single;
    MinimumLineHeight: Single;
    LineSpacing: Single;
    ParagraphSpacing: Single;
    ParagraphSpacingBefore: Single;
    TabStops: TTMSFNCPDFRichTextLibTabStops;
    LineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
    HyphenationFactor: Single;
  end;

  TTMSFNCPDFRichTextLibAttributeValue = record
    ParagraphStyle: TTMSFNCPDFRichTextLibParagraphStyle;
    Bitmap: TTMSFNCBitmap;
    BitmapFile: String;
    ForegroundColor: TTMSFNCGraphicsColor;
    BackgroundColor: TTMSFNCGraphicsColor;
    UnderlineColor: TTMSFNCGraphicsColor;
    StrikethroughColor: TTMSFNCGraphicsColor;
    BaselineOffset: Single;
    StrokeColor: TTMSFNCGraphicsColor;
    StrokeWidth: Single;
    URL: String;
    ToolTip: String;
    Underline: TTMSFNCPDFRichTextLibUnderlineStyles;
    Strikethrough: TTMSFNCPDFRichTextLibUnderlineStyles;
    ApplyBold: Boolean;
    Bold: Boolean;
    ApplyItalic: Boolean;
    Italic: Boolean;
    ApplyFontName: Boolean;
    FontName, FontFamily: String;
    ApplyFontSize: Boolean;
    FontSize: Single;
  end;

  TTMSFNCPDFRichTextLibDataDetectorType = (
   dtDataDetectorTypePhoneNumber,
   dtDataDetectorTypeLink,
   dtDataDetectorTypeAddress,
   dtDataDetectorTypeCalendarEvent,
   dtDataDetectorTypeNone,
   dtDataDetectorTypeAll
  );

  TTMSFNCPDFRichTextLibDataDetectorTypes = set of TTMSFNCPDFRichTextLibDataDetectorType;

  ITMSFNCCustomPDFRichTextLib = interface(IInterface)
  ['{E4E9AE27-E487-4D46-BA6C-33B6BA6E54E4}']
    procedure SetCanvas(ACanvas: Pointer);
    function GetSelection: TTMSFNCPDFRichTextLibRange;
    function Draw(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function Draw(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function GetValues(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibAttributeValue;
    procedure InitializeValues(var AValues: TTMSFNCPDFRichTextLibAttributeValue);
    function GetURL(AStart: Integer = -1; ALength: Integer = -1): String;
    procedure SetURL(AValue: String; AStart: Integer = -1; ALength: Integer = -1);
    procedure AddBitmap(AValue: TTMSFNCBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
    procedure AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
    function GetSubscript(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetSubscript(AValue: Single = -2; AStart: Integer = -1; ALength: Integer = -1);
    function GetBaselineOffset(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetBaselineOffset(AValue: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetSuperscript(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetSuperscript(AValue: Single = 2; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrokeColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor;
    procedure SetStrokeColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrokeWidth(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetStrokeWidth(AWidth: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetUnderline(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibUnderlineStyles;
    procedure SetUnderline(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
    function GetUnderlineColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor;
    procedure SetUnderlineColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrikethrough(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibUnderlineStyles;
    procedure SetStrikethrough(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrikethroughColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor;
    procedure SetStrikethroughColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetFont(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibFontValue;
    procedure SetFont(AName: String; ASize: Single = -1; AStart: Integer = -1; ALength: Integer = -1);
    function GetToolTip(AStart: Integer = -1; ALength: Integer = -1): String;
    procedure SetToolTip(AValue: String; AStart: Integer = -1; ALength: Integer = -1);
    function GetFontSize(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetFontSize(ASize: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetBackgroundColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor;
    procedure SetBackgroundColor(AColor: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetForegroundColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor;
    procedure SetForegroundColor(AColor: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetBold(AStart: Integer = -1; ALength: Integer = -1): Boolean;
    procedure SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
    function GetItalic(AStart: Integer = -1; ALength: Integer = -1): Boolean;
    procedure SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);

    procedure SetParagraphStyle(AValue: TTMSFNCPDFRichTextLibParagraphStyle; AStart: Integer = -1; ALength: Integer = -1);
    function GetParagraphStyle(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibParagraphStyle;
    function ExportToStream: TMemoryStream;
    procedure ImportFromStream(AStream: TMemoryStream);
    procedure ExportData(AFileName: String; ARange: TTMSFNCPDFRichTextLibRange; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ExportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ImportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);

    procedure Clear;

    procedure ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
    procedure DeleteText(AStart: Integer = -1; ALength: Integer = -1);
    function GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetPlainText: String; overload;
    function GetTextLength: Integer;
    function GetRichTextRange(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String; overload;
    procedure SetRichText(ARichText: String; ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
    function GetDataText: String;
    procedure SetDataText(const Value: String);
    function GetText: String;
    procedure SetText(const Value: String);
    property DataText: String read GetDataText write SetDataText;
    property Text: String read GetText write SetText;
  end;

  ITMSFNCPDFRichTextLibService = interface(IInterface)
  ['{6E174355-3C72-46C3-AC61-454E9711B9BD}']
    function CreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
  end;

  { TTMSFNCCustomPDFRichTextLib }

  TTMSFNCCustomPDFRichTextLib = class(TPersistent)
  private
    FPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPDFRichTextLibList = class(TList)
  private
    function GetItem(Index: Integer): ITMSFNCCustomPDFRichTextLib;
    procedure SetItem(Index: Integer; const Value: ITMSFNCCustomPDFRichTextLib);
  public
    property Items[Index: Integer]: ITMSFNCCustomPDFRichTextLib read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPDFRichTextLibList = class(TList<ITMSFNCCustomPDFRichTextLib>);
  {$ENDIF}

  TTMSFNCPDFRichTextLibFactoryService = class(TInterfacedObject, ITMSFNCPDFRichTextLibService)
  private
    FPDFRichTextLibs: TTMSFNCPDFRichTextLibList;
  protected
    function DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib; virtual; abstract;
    function CreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSFNCPDFRichTextLib = class(TTMSFNCCustomPDFRichTextLib)
  end;

implementation

uses
{$IFDEF MACOS}
{$IFDEF IOS}
  VCL.TMSFNCPDFRichTextLib.iOS,
{$ELSE}
  VCL.TMSFNCPDFRichTextLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  VCL.TMSFNCPDFRichTextLib.Android,
{$ENDIF}
{$IFNDEF MACOS}
{$IFNDEF ANDROID}
  VCL.TMSFNCPDFRichTextLib.General,
{$ENDIF}
{$ENDIF}
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

{ TTMSFNCPDFRichTextLibFactoryService }

constructor TTMSFNCPDFRichTextLibFactoryService.Create;
begin
  inherited Create;
  FPDFRichTextLibs := TTMSFNCPDFRichTextLibList.Create;
end;

function TTMSFNCPDFRichTextLibFactoryService.CreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
begin
  Result := DoCreatePDFRichTextLib;
  FPDFRichTextLibs.Add(Result);
end;

destructor TTMSFNCPDFRichTextLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFRichTextLibs);
  inherited Destroy;
end;

{ TTMSFNCCustomPDFRichTextLib }

constructor TTMSFNCCustomPDFRichTextLib.Create;
var
  PDFRichTextLibService: ITMSFNCPDFRichTextLibService;
begin
  if TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFRichTextLibService, IInterface(PDFRichTextLibService)) then
    FPDFRichTextLib := PDFRichTextLibService.CreatePDFRichTextLib;
end;

destructor TTMSFNCCustomPDFRichTextLib.Destroy;
begin
  if Assigned(FPDFRichTextLib) then
    FPDFRichTextLib := nil;

  inherited;
end;

function TTMSFNCCustomPDFRichTextLib.GetPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

{$IFDEF LCLLIB}
class operator TTMSFNCPDFRichTextLibTabStop.=(z1, z2: TTMSFNCPDFRichTextLibTabStop)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

{$IFDEF WEBLIB}
function TTMSFNCPDFRichTextLibList.GetItem(Index: Integer): ITMSFNCCustomPDFRichTextLib;
begin
  Result := ITMSFNCCustomPDFRichTextLib(inherited Items[Index]);
end;

procedure TTMSFNCPDFRichTextLibList.SetItem(Index: Integer; const Value: ITMSFNCCustomPDFRichTextLib);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCPDFRichTextLibTabStops.GetItem(Index: Integer): TTMSFNCPDFRichTextLibTabStop;
begin
  Result := TTMSFNCPDFRichTextLibTabStop(inherited Items[Index]);
end;

procedure TTMSFNCPDFRichTextLibTabStops.SetItem(Index: Integer; const Value: TTMSFNCPDFRichTextLibTabStop);
var
  v: TTMSFNCPDFRichTextLibTabStop;
begin
  v := Value;
  inherited Items[Index] := v;
end;
{$ENDIF}

initialization
  RegisterPDFRichTextLibService;

{$IFNDEF WEBLIB}
finalization
  UnRegisterPDFRichTextLibService;
{$ENDIF}

end.
