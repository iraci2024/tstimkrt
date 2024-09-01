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

unit FMX.TMSFNCPDFRichTextLib.General;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFRichTextLibService;
procedure UnRegisterPDFRichTextLibService;

implementation

uses
  Classes, FMX.TMSFNCPDFRichTextLib, SysUtils, FMX.TMSFNCTypes
  ,FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  ;

type
  TTMSFNCGeneralPDFRichTextLibService = class;
  TTMSFNCGeneralPDFRichTextLib = class;

  TTMSFNCGeneralPDFRichTextLibService = class(TTMSFNCPDFRichTextLibFactoryService)
  protected
    function DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib; override;
  end;

  TTMSFNCGeneralPDFRichTextLib = class(TInterfacedObject, ITMSFNCCustomPDFRichTextLib)
  private
    FText: String;
    procedure SetText(const Value: String);
    function GetText: String;
    function GetDataText: String;
    procedure SetDataText(const Value: String);
  protected
    procedure UpdateText;
    procedure ProcessAllAttributes(var {%H-}AValues: TTMSFNCPDFRichTextLibAttributeValue; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1; {%H-}ARetrieve: Boolean = False);
    procedure ProcessAttribute({%H-}AAtributeName: TTMSFNCPDFRichTextLibAttributeName; var {%H-}AValues: TTMSFNCPDFRichTextLibAttributeValue; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1; {%H-}ARetrieve: Boolean = False);
    procedure SetCanvas({%H-}ACanvas: Pointer);
    function GetSelection: TTMSFNCPDFRichTextLibRange;
    function Draw({%H-}Rect: TRectF; {%H-}Calculate: Boolean = False): TRectF; overload;
    function Draw({%H-}Rect: TRectF; {%H-}Columns: Integer; {%H-}Padding: Single = 5.0; {%H-}DetectOverflow: Boolean = False): Integer; overload;
    function Draw({%H-}Rects: TTMSFNCPDFGraphicsLibRectArray; {%H-}Padding: Single = 5.0; {%H-}DetectOverflow: Boolean = False): Integer; overload;
    function GetValues(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibAttributeValue;
    procedure InitializeValues(var AValues: TTMSFNCPDFRichTextLibAttributeValue);
    function GetURL(AStart: Integer = -1; ALength: Integer = -1): String; virtual;
    procedure SetURL(AValue: String; AStart: Integer = -1; ALength: Integer = -1); virtual;
    procedure AddBitmap(AValue: TTMSFNCBitmap; ALineHeight: Single = -1; ALocation: Integer = -1); virtual;
    procedure AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1); virtual;
    function GetSubscript(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetSubscript(AValue: Single = -2; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBaselineOffset(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetBaselineOffset(AValue: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetSuperscript(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetSuperscript(AValue: Single = 2; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrokeColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor; virtual;
    procedure SetStrokeColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrokeWidth(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetStrokeWidth(AWidth: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetUnderline(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibUnderlineStyles; virtual;
    procedure SetUnderline(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetUnderlineColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor; virtual;
    procedure SetUnderlineColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrikethrough(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibUnderlineStyles; virtual;
    procedure SetStrikethrough(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrikethroughColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor; virtual;
    procedure SetStrikethroughColor(AValue: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetFont(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibFontValue; virtual;
    procedure SetFont(AName: String; ASize: Single = -1; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetToolTip(AStart: Integer = -1; ALength: Integer = -1): String; virtual;
    procedure SetToolTip(AValue: String; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetFontSize(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetFontSize(ASize: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBackgroundColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor; virtual;
    procedure SetBackgroundColor(AColor: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetForegroundColor(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCGraphicsColor; virtual;
    procedure SetForegroundColor(AColor: TTMSFNCGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBold(AStart: Integer = -1; ALength: Integer = -1): Boolean; virtual;
    procedure SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetItalic(AStart: Integer = -1; ALength: Integer = -1): Boolean; virtual;
    procedure SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1); virtual;

    procedure SetParagraphStyle(AValue: TTMSFNCPDFRichTextLibParagraphStyle; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetParagraphStyle(AStart: Integer = -1; ALength: Integer = -1): TTMSFNCPDFRichTextLibParagraphStyle; virtual;
    function ExportToStream: TMemoryStream;
    procedure ImportFromStream({%H-}AStream: TMemoryStream);
    procedure ExportData({%H-}AFileName: String; {%H-}ARange: TTMSFNCPDFRichTextLibRange; {%H-}AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ExportData({%H-}AFileName: String; {%H-}AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ImportData({%H-}AFileName: String; {%H-}AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);

    procedure Clear;

    procedure ReplaceText({%H-}AText: String; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1);
    procedure DeleteText({%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1);
    function GetPlainTextRange({%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1): String; overload;
    function GetPlainText: String; overload;
    function GetTextLength: Integer;
    function GetRichTextRange({%H-}ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1): String; overload;
    function GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String; overload;
    procedure SetRichText({%H-}ARichText: String; {%H-}ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
    property DataText: String read GetDataText write SetDataText;
    property Text: String read GetText write SetText;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PDFRichTextLibService: ITMSFNCPDFRichTextLibService;

procedure RegisterPDFRichTextLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFRichTextLibService, IInterface(PDFRichTextLibService)) then
  begin
    PDFRichTextLibService := TTMSFNCGeneralPDFRichTextLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFRichTextLibService, PDFRichTextLibService);
  end;
end;

procedure UnregisterPDFRichTextLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFRichTextLibService);
end;

{ TTMSFNCGeneralPDFRichTextLibService }

function TTMSFNCGeneralPDFRichTextLibService.DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
begin
  Result := TTMSFNCGeneralPDFRichTextLib.Create;
end;

{ TTMSFNCGeneralPDFRichTextLib }

procedure TTMSFNCGeneralPDFRichTextLib.SetUnderline(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Underline := AValue;
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetUnderlineColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.UnderlineColor := AValue;
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetURL(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.URL := AValue;
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Bold := AValue;
  val.ApplyBold := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetCanvas(ACanvas: Pointer);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetDataText(const Value: String);
begin
  SetRichText(Value);
end;

procedure TTMSFNCGeneralPDFRichTextLib.AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  bmp: TTMSFNCBitmap;
begin
  bmp := TTMSFNCBitmap.Create;
  bmp.LoadFromFile(AValue);
  AddBitmap(bmp, ALineHeight, ALocation);
  bmp.Free;
end;

procedure TTMSFNCGeneralPDFRichTextLib.Clear;
begin
  Text := '';
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetBackgroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BackgroundColor := AColor;
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetBaselineOffset(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.AddBitmap(AValue: TTMSFNCBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
  h: Single;
begin
  InitializeValues({%H-}val);
  val.Bitmap := AValue;
  h := -1;
  if ALineHeight = -1 then
  begin
    if Assigned(val.Bitmap) then
        h := val.Bitmap.Height;
  end
  else
    h := ALineHeight;

  ProcessAttribute(anAttachmentAttributeName, val, ALocation, -1);
  if h > -1 then
  begin
    val.ParagraphStyle.LineHeightMultiple := h;
    val.ParagraphStyle.MinimumLineHeight := h;
    val.ParagraphStyle.MaximumLineHeight := h;
    ProcessAttribute(anParagraphStyleAttributeName, val, ALocation, 1);
  end;
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetFont(AName: String; ASize: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ApplyFontName := AName <> '';
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  val.FontName := AName;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetFontSize(ASize: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetForegroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ForegroundColor := AColor;
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength);
end;

constructor TTMSFNCGeneralPDFRichTextLib.Create;
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.DeleteText(AStart, ALength: Integer);
begin
end;

destructor TTMSFNCGeneralPDFRichTextLib.Destroy;
begin
  inherited;
end;

function TTMSFNCGeneralPDFRichTextLib.Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + (Rect.Right - Rect.Left) / Columns * I, Rect.Top, Rect.Left + (Rect.Right - Rect.Left) / Columns * (I + 1), Rect.Bottom);

  Result := Draw(arr, Padding, DetectOverflow);
end;

function TTMSFNCGeneralPDFRichTextLib.Draw(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFRichTextLib.Draw(Rect: TRectF; Calculate: Boolean = False): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
end;

procedure TTMSFNCGeneralPDFRichTextLib.ExportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

function TTMSFNCGeneralPDFRichTextLib.ExportToStream: TMemoryStream;
begin
  Result := nil;
end;

procedure TTMSFNCGeneralPDFRichTextLib.ExportData(AFileName: String; ARange: TTMSFNCPDFRichTextLibRange; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

function TTMSFNCGeneralPDFRichTextLib.GetBackgroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength, True);
  Result := val.BackgroundColor;
end;

function TTMSFNCGeneralPDFRichTextLib.GetBaselineOffset(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCGeneralPDFRichTextLib.GetBold(AStart, ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Bold;
end;

function TTMSFNCGeneralPDFRichTextLib.GetDataText: String;
begin
  Result := GetRichText;
end;

function TTMSFNCGeneralPDFRichTextLib.GetFont(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibFontValue;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result.FontFamily := val.FontFamily;
  Result.FontName := val.FontName;
  Result.FontSize := val.FontSize
end;

function TTMSFNCGeneralPDFRichTextLib.GetFontSize(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.FontSize;
end;

function TTMSFNCGeneralPDFRichTextLib.GetForegroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength, True);
  Result := val.ForegroundColor;
end;

function TTMSFNCGeneralPDFRichTextLib.GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String;
begin
  Result := GetRichTextRange(ADataType, 0, GetTextLength);
end;

function TTMSFNCGeneralPDFRichTextLib.GetRichTextRange(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String;
begin
  Result := '';
end;

function TTMSFNCGeneralPDFRichTextLib.GetItalic(AStart,
  ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Italic;
end;

function TTMSFNCGeneralPDFRichTextLib.GetSelection: TTMSFNCPDFRichTextLibRange;
begin
  Result.location := 0;
  Result.length := 0;
end;

function TTMSFNCGeneralPDFRichTextLib.GetStrikethrough(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength, True);
  Result := val.Strikethrough;
end;

function TTMSFNCGeneralPDFRichTextLib.GetStrikethroughColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength, True);
  Result := val.StrikethroughColor;
end;

function TTMSFNCGeneralPDFRichTextLib.GetStrokeColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength, True);
  Result := val.StrokeColor;
end;

function TTMSFNCGeneralPDFRichTextLib.GetStrokeWidth(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength, True);
  Result := val.StrokeWidth;
end;

function TTMSFNCGeneralPDFRichTextLib.GetSubscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCGeneralPDFRichTextLib.GetSuperscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCGeneralPDFRichTextLib.GetText: String;
begin
  Result := FText;
end;

function TTMSFNCGeneralPDFRichTextLib.GetTextLength: Integer;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFRichTextLib.GetToolTip(AStart,
  ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength, True);
  Result := val.ToolTip;
end;

function TTMSFNCGeneralPDFRichTextLib.GetUnderline(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength, True);
  Result := val.Underline;
end;

function TTMSFNCGeneralPDFRichTextLib.GetUnderlineColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength, True);
  Result := val.UnderlineColor;
end;

function TTMSFNCGeneralPDFRichTextLib.GetURL(AStart, ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength, True);
  Result := val.URL;
end;

function TTMSFNCGeneralPDFRichTextLib.GetValues(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(Result);
  ProcessAllAttributes(Result, AStart, ALength, True);
end;

procedure TTMSFNCGeneralPDFRichTextLib.ImportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.ImportFromStream(AStream: TMemoryStream);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.InitializeValues(
  var AValues: TTMSFNCPDFRichTextLibAttributeValue);
begin
  AValues.Bitmap := nil;
  AValues.ForegroundColor := 0;
  AValues.BackgroundColor := 0;
  AValues.StrokeColor := 0;
  AValues.UnderlineColor := 0;
  AValues.BaselineOffset := 0;
  AValues.URL := '';
  AValues.BaselineOffset := 0;
  AValues.StrikethroughColor := 0;
  AValues.Strikethrough := [];
  AValues.StrokeWidth := 0;
  AValues.Underline := [];
  AValues.ApplyBold := False;
  AValues.ApplyItalic := False;
  AValues.ApplyFontName := False;
  AValues.ApplyFontSize := False;
  AValues.FontSize := 12;
  AValues.FontName := 'Roboto';
  AValues.Italic := False;
  AValues.Bold := False;

  AValues.ParagraphStyle.Alignment := gtaLeading;
  AValues.ParagraphStyle.FirstLineHeadIndent := 0;
  AValues.ParagraphStyle.HeadIndent := 0;
  AValues.ParagraphStyle.TailIndent := 0;
  AValues.ParagraphStyle.LineHeightMultiple := 0;
  AValues.ParagraphStyle.MaximumLineHeight := 0;
  AValues.ParagraphStyle.MinimumLineHeight := 0;
  AValues.ParagraphStyle.LineSpacing := 0;
  AValues.ParagraphStyle.ParagraphSpacing := 0;
  AValues.ParagraphStyle.ParagraphSpacingBefore := 0;
  AValues.ParagraphStyle.TabStops := nil;
  AValues.ParagraphStyle.LineBreakMode := bmLineBreakModeWordWrap;
  AValues.ParagraphStyle.HyphenationFactor := 0;
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Italic := AValue;
  val.ApplyItalic := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetParagraphStyle(
  AValue: TTMSFNCPDFRichTextLibParagraphStyle; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ParagraphStyle := AValue;
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetRichText(ARichText: String;
  ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.ProcessAllAttributes(var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.ProcessAttribute(
  AAtributeName: TTMSFNCPDFRichTextLibAttributeName;
  var AValues: TTMSFNCPDFRichTextLibAttributeValue;
  AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
begin
end;

procedure TTMSFNCGeneralPDFRichTextLib.ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
begin
end;

function TTMSFNCGeneralPDFRichTextLib.GetParagraphStyle(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibParagraphStyle;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength, True);
  Result := val.ParagraphStyle;
end;

function TTMSFNCGeneralPDFRichTextLib.GetPlainText: String;
begin
  Result := GetPlainTextRange(0, GetTextLength)
end;

function TTMSFNCGeneralPDFRichTextLib.GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String;
begin
  Result := '';
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetStrikethrough(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Strikethrough := AValue;
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetStrikethroughColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrikethroughColor := AValue;
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetStrokeColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrokeColor := AValue;
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetStrokeWidth(AWidth: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrokeWidth := AWidth;
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetSubscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetSuperscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetText(const Value: String);
begin
  FText := Value;
  UpdateText;
end;

procedure TTMSFNCGeneralPDFRichTextLib.SetToolTip(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ToolTip := AValue;
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCGeneralPDFRichTextLib.UpdateText;
begin
end;

end.

