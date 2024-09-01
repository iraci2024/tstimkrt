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

unit FMX.TMSFNCPDFRichTextLib.Android;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFRichTextLibService;
procedure UnRegisterPDFRichTextLibService;

implementation

uses
  Classes, Types, FMX.TMSFNCPDFRichTextLib, Math, SysUtils, FMX.TMSFNCGraphics, FMX.TMSFNCTypes
  {$IFDEF ANDROID}
  ,AndroidApi.JNI
  ,AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Os
  ,AndroidApi.JNIBridge, AndroidApi.JNI.Net, FMX.Helpers.Android, AndroidApi.Helpers
  {$ENDIF}
  ,FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCGraphicsTypes;

type
  TTMSFNCAndroidPDFRichTextLibService = class;
  TTMSFNCAndroidPDFRichTextLib = class;

  {$IFDEF ANDROID}
  TTMSFNCAndroidPDFRichTextLibHtmlImageGetter = class(TJavaLocal, JHtml_ImageGetter)
  public
    function getDrawable(source: JString): JDrawable; cdecl;
  end;

  TTMSFNCAndroidPDFRichTextLibHtmlTagHandler = class(TJavaLocal, JHtml_TagHandler)
  public
    procedure handleTag(opening: Boolean; tag: JString; output: JEditable; xmlReader: JXMLReader); cdecl;
  end;
  {$ENDIF}

  TTMSFNCAndroidPDFRichTextLibService = class(TTMSFNCPDFRichTextLibFactoryService)
  protected
    function DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib; override;
  end;

  TTMSFNCAndroidPDFRichTextLib = class(TInterfacedObject, ITMSFNCCustomPDFRichTextLib)
  private
    FText: String;
    {$IFDEF ANDROID}
    FAttributedText: JSpanned;
    FCanvas: JCanvas;
    FImageGetter: TTMSFNCAndroidPDFRichTextLibHtmlImageGetter;
    FTagHandler: TTMSFNCANDROIDPDFRICHTEXTLIBHTMLTAGHANDLER;
    {$ENDIF}
    procedure SetText(const Value: String);
    function GetText: String;
    function GetDataText: String;
    procedure SetDataText(const Value: String);
    {$IFDEF ANDROID}
    function GetAttributedText: JSpanned;
    procedure SetAttributedText(const Value: JSpanned);
    {$ENDIF}
  protected
    procedure UpdateText;
    procedure ProcessAllAttributes(var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
    procedure ProcessAttribute(AAtributeName: TTMSFNCPDFRichTextLibAttributeName; var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
    {$IFDEF ANDROID}
    function AsSpanned(ASpannableString: JSpannableString): JSpanned;
    function AsSpannableString(ASpanned: JSpanned): JSpannableString;
    function IsSpannableString(ASpanned: JSpanned): Boolean;
    function GetAttributeName(AAttributeName: TTMSFNCPDFRichTextLibAttributeName): JString;
    function GetAttribute(AAttributeName: JString): TTMSFNCPDFRichTextLibAttributeName;
    property AttributedText: JSpanned read GetAttributedText write SetAttributedText;
    {$ENDIF}
    procedure SetCanvas(ACanvas: Pointer);
    function GetSelection: TTMSFNCPDFRichTextLibRange;
    function Draw(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function Draw(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
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
    procedure ImportFromStream(AStream: TMemoryStream);
    procedure ExportData(AFileName: String; ARange: TTMSFNCPDFRichTextLibRange; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ExportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ImportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
    procedure DeleteText(AStart: Integer = -1; ALength: Integer = -1);
    function GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetPlainText: String; overload;
    function GetTextLength: Integer;
    function GetRichTextRange(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String; overload;
    procedure SetRichText(ARichText: String; ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
    property DataText: String read GetDataText write SetDataText;
    property Text: String read GetText write SetText;
  end;

var
  PDFRichTextLibService: ITMSFNCPDFRichTextLibService;

procedure RegisterPDFRichTextLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFRichTextLibService, IInterface(PDFRichTextLibService)) then
  begin
    PDFRichTextLibService := TTMSFNCAndroidPDFRichTextLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFRichTextLibService, PDFRichTextLibService);
  end;
end;

procedure UnregisterPDFRichTextLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFRichTextLibService);
end;

{ TTMSFNCAndroidPDFRichTextLibService }

function TTMSFNCAndroidPDFRichTextLibService.DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
begin
  Result := TTMSFNCAndroidPDFRichTextLib.Create;
end;

{ TTMSFNCAndroidPDFRichTextLib }

procedure TTMSFNCAndroidPDFRichTextLib.SetUnderline(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Underline := AValue;
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetUnderlineColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.UnderlineColor := AValue;
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetURL(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.URL := AValue;
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Bold := AValue;
  val.ApplyBold := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetCanvas(ACanvas: Pointer);
begin
  {$IFDEF ANDROID}
  FCanvas := TJCanvas.Wrap(ACanvas);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetDataText(const Value: String);
begin
  SetRichText(Value);
end;

procedure TTMSFNCAndroidPDFRichTextLib.AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  bmp: TTMSFNCBitmap;
begin
  bmp := TTMSFNCBitmap.Create(0, 0);
  bmp.LoadFromFile(AValue);
  AddBitmap(bmp, ALineHeight, ALocation);
  bmp.Free;
end;

{$IFDEF ANDROID}
function TTMSFNCAndroidPDFRichTextLib.GetAttributedText: JSpanned;
begin
  Result := FAttributedText;
end;
{$ENDIF}

procedure TTMSFNCAndroidPDFRichTextLib.Clear;
begin
  Text := '';
end;

{$IFDEF ANDROID}
procedure TTMSFNCAndroidPDFRichTextLib.SetAttributedText(
  const Value: JSpanned);
begin
  FAttributedText := Value;
end;
{$ENDIF}

procedure TTMSFNCAndroidPDFRichTextLib.SetBackgroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BackgroundColor := AColor;
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetBaselineOffset(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.AddBitmap(AValue: TTMSFNCBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
  h: Single;
begin
  InitializeValues(val);
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

procedure TTMSFNCAndroidPDFRichTextLib.SetFont(AName: String; ASize: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ApplyFontName := AName <> '';
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  val.FontName := AName;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetFontSize(ASize: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetForegroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ForegroundColor := AColor;
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength);
end;

constructor TTMSFNCAndroidPDFRichTextLib.Create;
begin
  {$IFDEF ANDROID}
//  FImageGetter := TTMSFNCAndroidPDFRichTextLibHtmlImageGetter.Create;
//  FTagHandler := TTMSFNCAndroidPDFRichTextLibHtmlTagHandler.Create;
  FAttributedText := AsSpanned(TJSpannableString.JavaClass.init(StrToJCharSequence('')));
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.DeleteText(AStart, ALength: Integer);
{$IFDEF ANDROID}
var
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    FAttributedText := AsSpanned(TJSpannableString.JavaClass.init(FAttributedText.subSequence(sel.location, sel.length)));
  end;
  {$ENDIF}
end;

destructor TTMSFNCAndroidPDFRichTextLib.Destroy;
begin
  {$IFDEF ANDROID}
  FImageGetter := nil;
  FTagHandler := nil;
  FAttributedText := nil;
  {$ENDIF}
  inherited;
end;

function TTMSFNCAndroidPDFRichTextLib.Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := Draw(arr, Padding, DetectOverflow);
end;

function TTMSFNCAndroidPDFRichTextLib.Draw(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
{$IFDEF ANDROID}
var
  I: Integer;
  l: TJavaObjectArray<JStaticLayout>;
  st: JStaticLayout;
  c: JCanvas;
  spstr: JCharSequence;
  tp: JTextPaint;
  al: JLayout_Alignment;
  ste: Integer;
  le: Integer;
  r: TRectF;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF ANDROID}
  if Length(Rects) > 0 then
  begin
    c := FCanvas;
    spstr := FAttributedText;
    if Assigned(c) and Assigned(spstr) then
    begin
      l := TJavaObjectArray<JStaticLayout>.Create(Length(Rects));
      tp := TJTextPaint.JavaClass.init;
      tp.setAntiAlias(True);
      tp.linkColor := AlphaColorToJColor(GetForegroundColor);
      for I := 0 to Length(Rects) - 1 do
      begin
        r := Rects[I];
        InflateRectEx(r, -Padding, 0);
        al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;
        st := TJStaticLayout.JavaClass.init(spstr, 0, spstr.length, tp, Round(r.Width), al, 1.0, 0.0, False);
        ste := st.getLineForVertical(Round(r.Height)) - 1;
        le := st.getLineEnd(ste);
        if st.getLineEnd(st.getLineForVertical(Round(r.Height))) < spstr.length then
        begin
          st := TJStaticLayout.JavaClass.init(spstr, 0, le, tp, Round(r.Width), al, 1.0, 0.0, False);
          spstr := spstr.subSequence(le, spstr.length);
        end;

        l.Items[I] := st;

        if not DetectOverflow then
        begin
          c.save;
          c.translate(r.Left, r.Top);
          st.draw(c);
          c.restore;
        end;
      end;

      Result := spstr.length;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.Draw(Rect: TRectF; Calculate: Boolean = False): TRectF;
{$IFDEF ANDROID}
var
  c: JCanvas;
  tp: JTextPaint;
  sl: JStaticLayout;
  al: JLayout_Alignment;
  resw, resh: Single;
  spstr: JSpanned;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  spstr := FAttributedText;
  if Assigned(c) and Assigned(spstr) then
  begin
    tp := TJTextPaint.JavaClass.init;
    tp.setAntiAlias(True);
    tp.linkColor := AlphaColorToJColor(AlphaColorToJColor(GetForegroundColor));
    al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;
    sl := TJStaticLayout.JavaClass.init(spstr, tp, Round(Rect.Width), al, 1.0, 0.0, False);
    if not Calculate then
    begin
      c.save;
      c.translate(Rect.Left, Rect.Top);
      sl.draw(c);
      c.restore;
    end;

    resw := Min(Rect.Width, tp.measureText(StringToJString(Text), 0, Length(Text)));
    resh := sl.getHeight;
    Result := RectF(Rect.Left, Rect.Top, Rect.Left + resw, Rect.Top + resh);
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.ExportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF ANDROID}
var
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    sel.location := 0;
    sel.length := FAttributedText.length;
    ExportData(AFileName, sel, AType);
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.ExportToStream: TMemoryStream;
begin
  Result := nil;
end;

procedure TTMSFNCAndroidPDFRichTextLib.ExportData(AFileName: String; ARange: TTMSFNCPDFRichTextLibRange; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF ANDROID}
var
  att: JSpanned;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    if AType = dtArchivedXMLDocumentType then
    begin
    end
    else
    begin
      att := FAttributedText;
      case AType of
        dtRTFTextDocumentType: ;
        dtRTFDTextDocumentType: ;
        dtDocFormatTextDocumentType: ;
        else
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.GetBackgroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength, True);
  Result := val.BackgroundColor;
end;

function TTMSFNCAndroidPDFRichTextLib.GetBaselineOffset(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCAndroidPDFRichTextLib.GetBold(AStart, ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Bold;
end;

function TTMSFNCAndroidPDFRichTextLib.GetDataText: String;
begin
  Result := GetRichText;
end;

{$IFDEF ANDROID}
function TTMSFNCAndroidPDFRichTextLib.IsSpannableString(
  ASpanned: JSpanned): Boolean;
var
  o: JObject;
  str: String;
begin
  Result := False;
  o := TJObject.Wrap((ASpanned as ILocalObject).GetObjectID);
  str := JStringToString(o.getClass.getName);
  if str = 'android.text.SpannableString' then
    Result := True;
end;

function TTMSFNCAndroidPDFRichTextLib.AsSpanned(
  ASpannableString: JSpannableString): JSpanned;
begin
  Result := TJSpanned.Wrap((ASpannableString as ILocalObject).GetObjectID);
end;

function TTMSFNCAndroidPDFRichTextLib.AsSpannableString(
  ASpanned: JSpanned): JSpannableString;
begin
  Result := TJSpannableString.Wrap((ASpanned as ILocalObject).GetObjectID);
end;

function TTMSFNCANDROIDPDFRichTextLib.GetAttribute(AAttributeName: JString): TTMSFNCPDFRichTextLibAttributeName;
var
  str: String;
begin
  Result := anFontAttributeName;
  str := JStringToString(AAttributeName);
  if str = '' then Result := anParagraphStyleAttributeName
  else if str = 'android.text.style.ForegroundColorSpan' then Result := anForegroundColorAttributeName
  else if str = 'android.text.style.BackgroundColorSpan' then Result := anBackgroundColorAttributeName
//  else if str = '' then Result := anLigatureAttributeName
//  else if str = '' then Result := anKernAttributeName
  else if str = 'android.text.style.StrikethroughSpan' then Result := anStrikethroughStyleAttributeName
  else if str = 'android.text.style.UnderlineSpan' then Result := anUnderlineStyleAttributeName
//  else if str = '' then Result := anStrokeColorAttributeName
//  else if str = '' then Result := anStrokeWidthAttributeName
//  else if str = '' then Result := anShadowAttributeName
//  else if str = '' then Result := anTextEffectAttributeName
//  else if str = '' then Result := anAttachmentAttributeName
  else if str = 'android.text.style.URLSpan' then Result := anLinkAttributeName
//  else if str = '' then  Result := anToolTipAttributeName
//  else if str = '' then Result := anBaselineOffsetAttributeName
//  else if str = '' then Result := anUnderlineColorAttributeName
//  else if str = '' then Result := anObliquenessAttributeName
//  else if str = '' then Result := anExpansionAttributeName
//  else if str = '' then Result := anWritingDirectionAttributeName
//  else if str = '' then Result := anVerticalGlyphFormAttributeName;
end;

function TTMSFNCANDROIDPDFRichTextLib.GetAttributeName(
  AAttributeName: TTMSFNCPDFRichTextLibAttributeName): JString;
var
  res: String;
begin
  case AAttributeName of
    anFontAttributeName: res := 'android.text.style.TypefaceSpan;android.text.style.AbsoluteSizeSpan;android.text.style.StyleSpan';
    anParagraphStyleAttributeName: res := '';
    anForegroundColorAttributeName, anStrikethroughColorAttributeName, anUnderlineColorAttributeName: res := 'android.text.style.ForegroundColorSpan';
    anBackgroundColorAttributeName: res := 'android.text.style.BackgroundColorSpan';
    anLigatureAttributeName: res := '';
    anKernAttributeName: res := '';
    anStrikethroughStyleAttributeName: res := 'android.text.style.StrikethroughSpan';
    anUnderlineStyleAttributeName: res := 'android.text.style.UnderlineSpan';
    anStrokeColorAttributeName: res := '';
    anStrokeWidthAttributeName: res := '';
    anShadowAttributeName: res := '';
    anTextEffectAttributeName: res := '';
    anAttachmentAttributeName: res := '';
    anLinkAttributeName: res := 'android.text.style.URLSpan';
    anToolTipAttributeName: res := '';
    anBaselineOffsetAttributeName: res := '';
    anObliquenessAttributeName: res := '';
    anExpansionAttributeName: res := '';
    anWritingDirectionAttributeName: res := '';
    anVerticalGlyphFormAttributeName: res := '';
  end;

  Result := StringToJString(res);
end;
{$ENDIF}

function TTMSFNCAndroidPDFRichTextLib.GetFont(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibFontValue;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result.FontFamily := val.FontFamily;
  Result.FontName := val.FontName;
  Result.FontSize := val.FontSize
end;

function TTMSFNCAndroidPDFRichTextLib.GetFontSize(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.FontSize;
end;

function TTMSFNCAndroidPDFRichTextLib.GetForegroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength, True);
  Result := val.ForegroundColor;
end;

function TTMSFNCAndroidPDFRichTextLib.GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String;
begin
  Result := GetRichTextRange(ADataType, 0, GetTextLength);
end;

function TTMSFNCAndroidPDFRichTextLib.GetRichTextRange(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String;
begin
  Result := '';
end;

function TTMSFNCAndroidPDFRichTextLib.GetItalic(AStart,
  ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Italic;
end;

function TTMSFNCAndroidPDFRichTextLib.GetSelection: TTMSFNCPDFRichTextLibRange;
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    Result.location := 0;
    Result.length := FAttributedText.length;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.GetStrikethrough(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength, True);
  Result := val.Strikethrough;
end;

function TTMSFNCAndroidPDFRichTextLib.GetStrikethroughColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength, True);
  Result := val.StrikethroughColor;
end;

function TTMSFNCAndroidPDFRichTextLib.GetStrokeColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength, True);
  Result := val.StrokeColor;
end;

function TTMSFNCAndroidPDFRichTextLib.GetStrokeWidth(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength, True);
  Result := val.StrokeWidth;
end;

function TTMSFNCAndroidPDFRichTextLib.GetSubscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCAndroidPDFRichTextLib.GetSuperscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCAndroidPDFRichTextLib.GetText: String;
begin
  Result := FText;
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
    Result := JStringToString(FAttributedText.toString);
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.GetTextLength: Integer;
{$IFDEF ANDROID}
var
  att: JSpanned;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    att := FAttributedText;
    if Assigned(att) then
      Result := att.length;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFRichTextLib.GetToolTip(AStart,
  ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength, True);
  Result := val.ToolTip;
end;

function TTMSFNCAndroidPDFRichTextLib.GetUnderline(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength, True);
  Result := val.Underline;
end;

function TTMSFNCAndroidPDFRichTextLib.GetUnderlineColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength, True);
  Result := val.UnderlineColor;
end;

function TTMSFNCAndroidPDFRichTextLib.GetURL(AStart, ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength, True);
  Result := val.URL;
end;

function TTMSFNCAndroidPDFRichTextLib.GetValues(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(Result);
  ProcessAllAttributes(Result, AStart, ALength, True);
end;

procedure TTMSFNCAndroidPDFRichTextLib.ImportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

procedure TTMSFNCAndroidPDFRichTextLib.ImportFromStream(AStream: TMemoryStream);
begin
end;

procedure TTMSFNCAndroidPDFRichTextLib.InitializeValues(
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

procedure TTMSFNCAndroidPDFRichTextLib.SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Italic := AValue;
  val.ApplyItalic := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetParagraphStyle(
  AValue: TTMSFNCPDFRichTextLibParagraphStyle; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ParagraphStyle := AValue;
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetRichText(ARichText: String;
  ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF ANDROID}
var
  a: TJavaArray<JObject>;
  c, obj: JObject;
  I: Integer;
  imgSpan: JImageSpan;
  spHtml: JSpanned;
  spStart, spEnd: Integer;
  d: JDrawable;
  pth: JString;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if ARichText <> '' then
  begin
    if ADataType = dtHTMLTextDocumentType then
    begin
      if Assigned(FAttributedText) then
        FAttributedText := nil;

      spHtml := TJHtml.JavaClass.fromHtml(StringToJString(ARichText), FImageGetter, FTagHandler);
      FAttributedText := AsSpanned(TJSpannableString.JavaClass.init(spHtml));
      c := TJObject.JavaClass.init;
      a := FAttributedText.getSpans(0, FAttributedText.length, c.getClass);
      for I := 0 to a.Length - 1 do
      begin
        obj := a[I];
        if JStringToString(obj.getClass.getName) = 'android.text.style.ImageSpan' then
        begin
          imgSpan := TJImageSpan.Wrap((obj as ILocalObject).GetObjectID);
          spStart := FAttributedText.getSpanStart(imgSpan);
          spEnd := FAttributedText.getSpanEnd(imgSpan);
          pth := TJnet_Uri.JavaClass.parse(imgSpan.getSource).getPath;
          d := TJDrawable.JavaClass.createFromPath(pth);
          d.setBounds(0, 0, d.getIntrinsicWidth, d.getIntrinsicHeight);
          imgSpan := TJImageSpan.JavaClass.init(d);
          AsSpannableString(FAttributedText).setSpan(imgSpan, spStart, spEnd, TJSpannable.JavaClass.SPAN_EXCLUSIVE_EXCLUSIVE);
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.ProcessAllAttributes(var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
begin
end;

procedure TTMSFNCAndroidPDFRichTextLib.ProcessAttribute(
  AAtributeName: TTMSFNCPDFRichTextLibAttributeName;
  var AValues: TTMSFNCPDFRichTextLibAttributeValue;
  AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
{$IFDEF ANDROID}
var
  att: JSpanned;
  atts: JSpannableString;
  sel: TTMSFNCPDFRichTextLibRange;
  str: JString;
  sname: string;
  selr, rgn: TTMSFNCPDFGraphicsLibTextRange;
  pValue, p: Pointer;
  o: TJavaObjectArray<JObject>;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    att := FAttributedText;
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    selr := MakeTextRange(sel.location, sel.length);

    str := GetAttributeName(AAtributeName);
    if JStringToString(str) <> '' then
    begin
      if AAtributeName = anAttachmentAttributeName then
      begin
        if Assigned(AValues.Bitmap) then
        begin
  //        t := TNSTextAttachment.Wrap(TNSTextAttachment.Wrap(TNSTextAttachment.OCClass.alloc).init);
  //        img := ImageFromBitmap(AValues.Bitmap);
  //        tcl := TNSTextAttachmentCell.Wrap(TNSTextAttachmentCell.Wrap(TNSTextAttachmentCell.OCClass.alloc).init);
  //        tcl.setImage(img);
  //        t.setAttachmentCell(AndroidApi.AppKit.NSTextAttachmentCell(tcl));
  //        img.release;
  //        tcl.release;
  //
  //        attText := TNSAttributedString.Wrap(TNSAttributedStringEx.OCClass.attributedStringWithAttachment(t));
  //        att.insertAttributedString(attText, sel.location);
        end
        else if AValues.BitmapFile <> '' then
        begin
  //        fw := TNSFileWrapper.Wrap(TNSFileWrapper.Wrap(TNSFileWrapper.OCClass.alloc).initWithPath(NSStrEx(AValues.BitmapFile)));
  //        t := TNSTextAttachment.Wrap(TNSTextAttachment.Wrap(TNSTextAttachment.OCClass.alloc).initWithFileWrapper(fw));
  //        attText := TNSAttributedString.Wrap(TNSAttributedStringEx.OCClass.attributedStringWithAttachment(t));
  //        att.insertAttributedString(attText, sel.location);
  //        t.release;
  //        fw.release;
        end;
      end
      else
      begin
        rgn := MakeTextRange(0, 0);
        pValue := nil;
        while selr.length > 0 do
        begin
          for sname in JStringToString(str).Split([';']) do
          begin
            o := att.getSpans(selr.location, selr.location + selr.length, TJlang_Class.JavaClass.forName(StringToJString(sname)));
            if o.Length > 0 then
            begin
              p := (o.Items[0] as ILocalObject).GetObjectID;
              rgn.location := att.getSpanStart(TJObject.Wrap(p));
              rgn.length := att.getSpanEnd(TJObject.Wrap(p)) - rgn.location;
            end
            else
            begin
              p := nil;
              rgn.location := selr.location;
              rgn.length := selr.length;
            end;

            case AAtributeName of
              anFontAttributeName:
              begin
                if (sname = 'android.text.style.AbsoluteSizeSpan') then
                begin
                  if ARetrieve then
                  begin
                    if Assigned(p) then
                      AValues.FontSize := TJAbsoluteSizeSpan.Wrap(p).getSize;
                  end
                  else if AValues.ApplyFontSize then
                    pValue := (TJAbsoluteSizeSpan.JavaClass.init(Round(AValues.FontSize)) as ILocalObject).GetObjectID;
                end
                else if (sname = 'android.text.style.TypefaceSpan') then
                begin
                  if ARetrieve then
                  begin
                    if Assigned(p) then
                    begin
                      AValues.FontName := JStringToString(TJTypefaceSpan.Wrap(p).getFamily);
                      AValues.FontFamily := AValues.FontName;
                    end;
                  end
                  else if AValues.ApplyFontName then
                    pValue := (TJTypefaceSpan.JavaClass.init(StringToJString(AValues.FontName)) as ILocalObject).GetObjectID;
                end
                else if (sname = 'android.text.style.StyleSpan') then
                begin
                  if ARetrieve then
                  begin
                    if Assigned(p) then
                    begin
                      AValues.Bold := TJStyleSpan.Wrap(p).isBold;
                      AValues.Italic := TJStyleSpan.Wrap(p).isItalic;
                    end;
                  end
                  else if (AValues.ApplyBold or AValues.ApplyItalic) then
                  begin
                    if AValues.Bold and AValues.Italic then
                      pValue := (TJStyleSpan.JavaClass.init(TJTypeface.JavaClass.BOLD_ITALIC) as ILocalObject).GetObjectID
                    else if AValues.Italic then
                      pValue := (TJStyleSpan.JavaClass.init(TJTypeface.JavaClass.ITALIC) as ILocalObject).GetObjectID
                    else if AValues.Bold then
                      pValue := (TJStyleSpan.JavaClass.init(TJTypeface.JavaClass.BOLD) as ILocalObject).GetObjectID;
                  end;
                end;
              end;
              anParagraphStyleAttributeName:
              begin

              end;
              anForegroundColorAttributeName, anStrikethroughColorAttributeName, anUnderlineColorAttributeName:
              begin
                if ARetrieve then
                begin
                  if Assigned(p) then
                  begin
                    case AAtributeName of
                      anForegroundColorAttributeName: AValues.ForegroundColor := JColorToAlphaColor(TJForegroundColorSpan.Wrap(p).getForegroundColor);
                      anUnderlineColorAttributeName: AValues.UnderlineColor := JColorToAlphaColor(TJForegroundColorSpan.Wrap(p).getForegroundColor);
                      anStrikethroughColorAttributeName: AValues.StrikethroughColor := JColorToAlphaColor(TJForegroundColorSpan.Wrap(p).getForegroundColor);
                    end;
                  end;
                end
                else
                begin
                  case AAtributeName of
                    anForegroundColorAttributeName: pValue := (TJForegroundColorSpan.JavaClass.init(JColorToAlphaColor(AValues.ForegroundColor)) as ILocalObject).GetObjectID;
                    anUnderlineColorAttributeName: pValue := (TJForegroundColorSpan.JavaClass.init(JColorToAlphaColor(AValues.UnderlineColor)) as ILocalObject).GetObjectID;
                    anStrikethroughColorAttributeName: pValue := (TJForegroundColorSpan.JavaClass.init(JColorToAlphaColor(AValues.StrikethroughColor)) as ILocalObject).GetObjectID;
                  end;
                end;
              end;
              anBackgroundColorAttributeName:
              begin
                if ARetrieve then
                begin
                  if Assigned(p) then
                    AValues.BackgroundColor := JColorToAlphaColor(TJBackgroundColorSpan.Wrap(p).getBackgroundColor);
                end
                else
                  pValue := (TJBackgroundColorSpan.JavaClass.init(JColorToAlphaColor(AValues.BackgroundColor)) as ILocalObject).GetObjectID;
              end;
              anLigatureAttributeName: ;
              anKernAttributeName: ;
              anStrikethroughStyleAttributeName:
              begin
                if ARetrieve then
                begin
                  if Assigned(p) then
                    AValues.Strikethrough := [usUnderlineStyleSingle];
                end
                else
                  pValue := (TJStrikethroughSpan.JavaClass.init as ILocalObject).GetObjectID;
              end;
              anUnderlineStyleAttributeName:
              begin
                if ARetrieve then
                begin
                  if Assigned(p) then
                    AValues.Underline := [usUnderlineStyleSingle];
                end
                else
                  pValue := (TJUnderlineSpan.JavaClass.init as ILocalObject).GetObjectID;
              end;
              anStrokeColorAttributeName:
              begin
              end;
              anStrokeWidthAttributeName:
              begin
              end;
              anShadowAttributeName: ;
              anTextEffectAttributeName: ;
              anToolTipAttributeName:
              begin
              end;
              anLinkAttributeName:
              begin
                if ARetrieve then
                begin
                  if Assigned(p) then
                    AValues.URL := JStringToString(TJURLSpan.Wrap(p).getURL);
                end
                else
                  pValue := (TJURLSpan.JavaClass.init(StringToJString(AValues.URL)) as ILocalObject).GetObjectID;
              end;
              anBaselineOffsetAttributeName:
              begin
              end;
              anObliquenessAttributeName: ;
              anExpansionAttributeName: ;
              anWritingDirectionAttributeName: ;
              anVerticalGlyphFormAttributeName: ;
            end;

            if Assigned(pValue) and not ARetrieve then
            begin
              atts := AsSpannableString(att);
              atts.setSpan(TJObject.Wrap(pValue), rgn.location, rgn.location + rgn.length, TJSpanned.JavaClass.SPAN_EXCLUSIVE_EXCLUSIVE);
            end;
          end;
          selr := MakeTextRange(MaxRange(rgn), MaxRange(selr) - MaxRange(rgn));
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
begin
end;

function TTMSFNCAndroidPDFRichTextLib.GetParagraphStyle(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibParagraphStyle;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength, True);
  Result := val.ParagraphStyle;
end;

function TTMSFNCAndroidPDFRichTextLib.GetPlainText: String;
begin
  Result := GetPlainTextRange(0, GetTextLength)
end;

function TTMSFNCAndroidPDFRichTextLib.GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String;
{$IFDEF ANDROID}
var
  attsel: JCharSequence;
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  Result := '';
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
  begin
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    attsel := FAttributedText.subSequence(sel.location, sel.length);
    if Assigned(attsel) then
      Result := JCharSequenceToStr(attsel);
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetStrikethrough(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Strikethrough := AValue;
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetStrikethroughColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrikethroughColor := AValue;
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetStrokeColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrokeColor := AValue;
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetStrokeWidth(AWidth: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrokeWidth := AWidth;
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetSubscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetSuperscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetText(const Value: String);
begin
  FText := Value;
  UpdateText;
end;

procedure TTMSFNCAndroidPDFRichTextLib.SetToolTip(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ToolTip := AValue;
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCAndroidPDFRichTextLib.UpdateText;
begin
  {$IFDEF ANDROID}
  if Assigned(FAttributedText) then
    FAttributedText := nil;

  FAttributedText := AsSpanned(TJSpannableString.JavaClass.init(StrToJCharSequence(Text)));
  {$ENDIF}
end;

{$IFDEF ANDROID}
{ TTMSFNCAndroidPDFRichTextLibHtmlImageGetter }

function TTMSFNCAndroidPDFRichTextLibHtmlImageGetter.getDrawable(
  source: JString): JDrawable;
begin
end;

{ TTMSFNCAndroidPDFRichTextLibHtmlTagHandler }

procedure TTMSFNCAndroidPDFRichTextLibHtmlTagHandler.handleTag(opening: Boolean;
  tag: JString; output: JEditable; xmlReader: JXMLReader);
begin
end;

{$ENDIF}


end.

