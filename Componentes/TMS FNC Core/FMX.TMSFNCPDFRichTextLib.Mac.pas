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

unit FMX.TMSFNCPDFRichTextLib.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFRichTextLibService;
procedure UnRegisterPDFRichTextLibService;

implementation

{$IFDEF MACOS}
{$IFNDEF IOS}
{$DEFINE MACOS_OK}
{$ENDIF}
{$ENDIF}

uses
  Classes, Types, FMX.TMSFNCPDFRichTextLib,
  FMX.TMSFNCGraphics
  {$IFNDEF LCLLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF MACOS_OK}
  , MacApi.Foundation, MacApi.AppKit, MacApi.CocoaTypes, MacApi.CoreGraphics,
  MacApi.ObjectiveC, MacApi.ObjcRuntime, MacApi.CoreText, MacApi.CoreFoundation
  {$ENDIF}
  ,FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCPDFGraphicsLib, FMX.TMSFNCTypes, FMX.TMSFNCGraphicsTypes
  ;

type
  TTMSFNCMacPDFRichTextLibService = class(TTMSFNCPDFRichTextLibFactoryService)
  protected
    function DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib; override;
  end;

  TTMSFNCMacPDFRichTextLib = class(TInterfacedObject, ITMSFNCCustomPDFRichTextLib)
  private
    FText: String;
    {$IFDEF MACOS_OK}
    FAttributedText: NSAttributedString;
    {$ENDIF}
    procedure SetText(const Value: String);
    function GetText: String;
    function GetDataText: String;
    procedure SetDataText(const Value: String);
    {$IFDEF MACOS_OK}
    function GetAttributedText: NSAttributedString;
    procedure SetAttributedText(const Value: NSAttributedString);
    {$ENDIF}
  protected
    procedure UpdateText;
    procedure ProcessAllAttributes(var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
    procedure ProcessAttribute(AAtributeName: TTMSFNCPDFRichTextLibAttributeName; var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
    {$IFDEF MACOS_OK}
    function GetDataType(ADataType: TTMSFNCPDFRichTextLibDataType): Pointer;
    function GetAttributeName(AAttributeName: TTMSFNCPDFRichTextLibAttributeName): NSString;
    function GetAttribute(AAttributeName: NSString): TTMSFNCPDFRichTextLibAttributeName;
    function GetUnderlineStyleType(AValue: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
    function GetUnderlineStyleValue(AValue: TTMSFNCPDFRichTextLibUnderlineStyles): Integer;
    property AttributedText: NSAttributedString read GetAttributedText write SetAttributedText;
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
    PDFRichTextLibService := TTMSFNCMacPDFRichTextLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFRichTextLibService, PDFRichTextLibService);
  end;
end;

procedure UnregisterPDFRichTextLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFRichTextLibService);
end;

{ TTMSFNCMacPDFRichTextLibService }

function TTMSFNCMacPDFRichTextLibService.DoCreatePDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
begin
  Result := TTMSFNCMacPDFRichTextLib.Create;
end;

{ TTMSFNCMacPDFRichTextLib }

procedure TTMSFNCMacPDFRichTextLib.SetUnderline(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Underline := AValue;
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetUnderlineColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.UnderlineColor := AValue;
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetURL(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.URL := AValue;
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Bold := AValue;
  val.ApplyBold := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetCanvas(ACanvas: Pointer);
begin

end;

procedure TTMSFNCMacPDFRichTextLib.SetDataText(const Value: String);
begin
  SetRichText(Value);
end;

procedure TTMSFNCMacPDFRichTextLib.AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  bmp: TTMSFNCBitmap;
begin
  bmp := TTMSFNCBitmap.Create(0, 0);
  bmp.LoadFromFile(AValue);
  AddBitmap(bmp, ALineHeight, ALocation);
  bmp.Free;
end;

{$IFDEF MACOS_OK}
function TTMSFNCMacPDFRichTextLib.GetAttributedText: NSAttributedString;
begin
  Result := FAttributedText;
end;
{$ENDIF}

procedure TTMSFNCMacPDFRichTextLib.Clear;
begin
  Text := '';
end;

{$IFDEF MACOS_OK}
procedure TTMSFNCMacPDFRichTextLib.SetAttributedText(
  const Value: NSAttributedString);
begin
  if Assigned(FAttributedText) then
  begin
    FAttributedText.release;
    FAttributedText := nil;
    FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(Value));
  end;
end;
{$ENDIF}

procedure TTMSFNCMacPDFRichTextLib.SetBackgroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BackgroundColor := AColor;
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetBaselineOffset(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.AddBitmap(AValue: TTMSFNCBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
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

procedure TTMSFNCMacPDFRichTextLib.SetFont(AName: String; ASize: Single; AStart,
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

procedure TTMSFNCMacPDFRichTextLib.SetFontSize(ASize: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetForegroundColor(AColor: TTMSFNCGraphicsColor; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ForegroundColor := AColor;
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength);
end;

constructor TTMSFNCMacPDFRichTextLib.Create;
begin
  {$IFDEF MACOS_OK}
  FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).init);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.DeleteText(AStart, ALength: Integer);
{$IFDEF MACOS_OK}
var
  attm: NSMutableAttributedString;
  att: NSAttributedString;
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    att := FAttributedText;
    attm := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Wrap(TNSMutableAttributedString.OCClass.alloc).initWithAttributedString(att));

    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    attm.deleteCharactersInRange(NSMakeRange(sel.location, sel.length));
    FAttributedText.release;
    FAttributedText := nil;
    FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(attm));
  end;
  {$ENDIF}
end;

destructor TTMSFNCMacPDFRichTextLib.Destroy;
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    FAttributedText.release;
    FAttributedText := nil;
  end;
  {$ENDIF}
  inherited;
end;

function TTMSFNCMacPDFRichTextLib.Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := Draw(arr, Padding, DetectOverflow);
end;

function TTMSFNCMacPDFRichTextLib.Draw(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
{$IFDEF MACOS_OK}
var
  textStorage: NSTextStorage;
  layoutManager: NSLayoutManager;
  textContainer: NSTextContainer;
  glyphRange: NSRange;
  c: Integer;
  sz: NSSize;
  pt: NSPoint;
{$ENDIF}
begin
  Result := 0;
  if Length(Rects) > 0 then
  begin
    {$IFDEF MACOS_OK}
    textStorage := TNSTextStorage.Wrap(TNSTextStorage.Wrap(TNSTextStorage.OCClass.alloc).initWithAttributedString(FAttributedText));
    layoutManager := TNSLayoutManager.Wrap(TNSLayoutManager.Wrap(TNSLayoutManager.OCClass.alloc).init);

    for c := 0 to Length(Rects) - 1 do
    begin
      sz.width := Rects[c].Width;
      sz.height := Rects[c].Height;
      textContainer := TNSTextContainer.Wrap(TNSTextContainer.Wrap(TNSTextContainer.OCClass.alloc).
        initWithContainerSize(sz));
      textContainer.setLineFragmentPadding(Padding);
      layoutManager.addTextContainer(textContainer);
      textContainer.release;
    end;

    textStorage.addLayoutManager(layoutManager);
    layoutManager.release;

    for c := 0 to Length(Rects) - 1 do
    begin
      glyphRange := layoutManager.glyphRangeForTextContainer(TNSTextContainer.Wrap(layoutmanager.textContainers.objectAtIndex(c)));
      if not DetectOverflow then
      begin
        pt.x := Rects[c].Left;
        pt.y := Rects[c].Top;
        layoutManager.drawBackgroundForGlyphRange(glyphrange, pt);
        layoutManager.drawGlyphsForGlyphRange(glyphrange, pt);
      end;
    end;

    Result := glyphRange.length;

    textStorage.release;
    {$ENDIF}
  end;
end;

function TTMSFNCMacPDFRichTextLib.Draw(Rect: TRectF; Calculate: Boolean = False): TRectF;
{$IFDEF MACOS_OK}
var
  r: NSRect;
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    CGContextSaveGState(c);
    CGContextTranslateCTM(c, Rect.Left, Rect.Top);
    if Assigned(FAttributedText) then
    begin
      if not Calculate then
        TNSAttributedStringEx.Wrap((FAttributedText as ILocalObject).GetObjectID).drawInRect(CGRectMake(0, 0, Rect.Width, Rect.Height));

      r := TNSAttributedStringEx.Wrap((FAttributedText as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(Rect.Width, Rect.Height), NSStringDrawingUsesLineFragmentOrigin, nil);
      Result := RectF(Rect.Left, Rect.Top, Rect.Left + r.size.width, Rect.Top + r.size.height);
    end;
    CGContextRestoreGState(c);
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ExportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF MACOS_OK}
var
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    sel.location := 0;
    sel.length := FAttributedText.length;
    ExportData(AFileName, sel, AType);
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.ExportToStream: TMemoryStream;
{$IFDEF MACOS_OK}
var
  dt: NSData;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF MACOS_OK}
  dt := TNSKeyedArchiver.OCClass.archivedDataWithRootObject((FAttributedText as ILocalObject).GetObjectID);
  if Assigned(dt) then
  begin
    Result := TMemoryStream.Create;
    {$HINTS OFF}
    {$IF COMPILERVERSION > 31}
    Result.Write(dt.bytes^, dt.length);
    {$ELSE}
    Result.Write(dt.bytes, dt.length);
    {$IFEND}
    {$HINTS ON}
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ExportData(AFileName: String; ARange: TTMSFNCPDFRichTextLibRange; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
  dt: NSData;
  dic: NSDictionary;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    if AType = dtArchivedXMLDocumentType then
    begin
      dt := TNSKeyedArchiver.OCClass.archivedDataWithRootObject((FAttributedText as ILocalObject).GetObjectID);
      if Assigned(dt) then
        dt.writeToFile(NSStrEx(AFileName), True);
    end
    else
    begin
      att := FAttributedText;
      dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(GetDataType(AType),
        (CocoaNSStringConst(AppKitFwk, 'NSDocumentTypeDocumentAttribute') as ILocalObject).GetObjectID));

      case AType of
        dtRTFTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).RTFFromRange(NSMakeRange(ARange.location, ARange.length), dic);
        dtRTFDTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).RTFDFromRange(NSMakeRange(ARange.location, ARange.length), dic);
        dtDocFormatTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).docFormatFromRange(NSMakeRange(ARange.location, ARange.length), dic);
        else dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).dataFromRange(NSMakeRange(ARange.location, ARange.length), dic, nil);
      end;
      if Assigned(dt) then
        dt.writeToFile(NSStrEx(AFileName), True);
    end;
  end;
  {$ENDIF}
end;

{$IFDEF MACOS_OK}
function TTMSFNCMacPDFRichTextLib.GetAttribute(AAttributeName: NSString): TTMSFNCPDFRichTextLibAttributeName;
var
  str: String;
begin
  Result := anFontAttributeName;
  str := UTF8ToString(AAttributeName.UTF8String);
  if str = 'NSParagraphStyle' then Result := anParagraphStyleAttributeName
  else if str = 'NSColor' then Result := anForegroundColorAttributeName
  else if str = 'NSBackgroundColor' then Result := anBackgroundColorAttributeName
  else if str = 'NSLigature' then Result := anLigatureAttributeName
  else if str = 'NSKern' then Result := anKernAttributeName
  else if str = 'NSStrikethrough' then Result := anStrikethroughStyleAttributeName
  else if str = 'NSUnderline' then Result := anUnderlineStyleAttributeName
  else if str = 'NSStrokeColor' then Result := anStrokeColorAttributeName
  else if str = 'NSStrokeWidth' then Result := anStrokeWidthAttributeName
  else if str = 'NSShadow' then Result := anShadowAttributeName
  else if str = 'NSTextEffect' then Result := anTextEffectAttributeName
  else if str = 'NSAttachment' then Result := anAttachmentAttributeName
  else if str = 'NSLink' then Result := anLinkAttributeName
  else if str = 'NSToolTip' then  Result := anToolTipAttributeName
  else if str = 'NSBaselineOffset' then Result := anBaselineOffsetAttributeName
  else if str = 'NSUnderlineColor' then Result := anUnderlineColorAttributeName
  else if str = 'NSStrikethroughColor' then Result := anStrikethroughColorAttributeName
  else if str = 'NSObliqueness' then Result := anObliquenessAttributeName
  else if str = 'NSExpansion' then Result := anExpansionAttributeName
  else if str = 'NSWritingDirection' then Result := anWritingDirectionAttributeName
  else if str = 'NSVerticalGlyphForm' then Result := anVerticalGlyphFormAttributeName;
end;

function TTMSFNCMacPDFRichTextLib.GetAttributeName(
  AAttributeName: TTMSFNCPDFRichTextLibAttributeName): NSString;
var
  res: String;
begin
  case AAttributeName of
    anFontAttributeName: res := 'NSFontAttributeName';
    anParagraphStyleAttributeName: res := 'NSParagraphStyleAttributeName';
    anForegroundColorAttributeName: res := 'NSForegroundColorAttributeName';
    anBackgroundColorAttributeName: res := 'NSBackgroundColorAttributeName';
    anLigatureAttributeName: res := 'NSLigatureAttributeName';
    anKernAttributeName: res := 'NSKernAttributeName';
    anStrikethroughStyleAttributeName: res := 'NSStrikethroughStyleAttributeName';
    anUnderlineStyleAttributeName: res := 'NSUnderlineStyleAttributeName';
    anStrokeColorAttributeName: res := 'NSStrokeColorAttributeName';
    anStrokeWidthAttributeName: res := 'NSStrokeWidthAttributeName';
    anShadowAttributeName: res := 'NSShadowAttributeName';
    anTextEffectAttributeName: res := 'NSTextEffectAttributeName';
    anAttachmentAttributeName: res := 'NSAttachmentAttributeName';
    anLinkAttributeName: res := 'NSLinkAttributeName';
    anToolTipAttributeName: res := 'NSToolTipAttributeName';
    anBaselineOffsetAttributeName: res := 'NSBaselineOffsetAttributeName';
    anUnderlineColorAttributeName: res := 'NSUnderlineColorAttributeName';
    anStrikethroughColorAttributeName: res := 'NSStrikethroughColorAttributeName';
    anObliquenessAttributeName: res := 'NSObliquenessAttributeName';
    anExpansionAttributeName: res := 'NSExpansionAttributeName';
    anWritingDirectionAttributeName: res := 'NSWritingDirectionAttributeName';
    anVerticalGlyphFormAttributeName: res := 'NSVerticalGlyphFormAttributeName';
  end;

  Result := CocoaNSStringConst(AppKitFwk, res);
end;
{$ENDIF}

function TTMSFNCMacPDFRichTextLib.GetBackgroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength, True);
  Result := val.BackgroundColor;
end;

function TTMSFNCMacPDFRichTextLib.GetBaselineOffset(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCMacPDFRichTextLib.GetBold(AStart, ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Bold;
end;

function TTMSFNCMacPDFRichTextLib.GetDataText: String;
begin
  Result := GetRichText;
end;

{$IFDEF MACOS_OK}
function TTMSFNCMacPDFRichTextLib.GetDataType(
  ADataType: TTMSFNCPDFRichTextLibDataType): Pointer;
var
  res: String;
begin
  case ADataType of
    dtPlainTextDocumentType: res := 'NSPlainTextDocumentType';
    dtRTFTextDocumentType: res := 'NSRTFTextDocumentType';
    dtRTFDTextDocumentType: res := 'NSRTFDTextDocumentType';
    dtHTMLTextDocumentType: res := 'NSHTMLTextDocumentType';
    dtSimpleTextDocumentType: res := 'NSSimpleTextDocumentType';
    dtDocFormatTextDocumentType: res := 'NSDocFormatTextDocumentType';
    dtWordMLTextDocumentType: res := 'NSWordMLTextDocumentType';
    dtOfficeOpenXMLTextDocumentType: res := 'NSOfficeOpenXMLTextDocumentType';
    dtWebArchiveTextDocumentType: res := 'NSWebArchiveTextDocumentType';
    dtOpenDocumentTextDocumentType: res := 'NSOpenDocumentTextDocumentType';
  end;

  Result := (CocoaNSStringConst(AppKitFwk, res) as ILocalObject).GetObjectID;
end;
{$ENDIF}

function TTMSFNCMacPDFRichTextLib.GetFont(AStart,
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

function TTMSFNCMacPDFRichTextLib.GetFontSize(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.FontSize;
end;

function TTMSFNCMacPDFRichTextLib.GetForegroundColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength, True);
  Result := val.ForegroundColor;
end;

function TTMSFNCMacPDFRichTextLib.GetRichText(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType): String;
begin
  Result := GetRichTextRange(ADataType, 0, GetTextLength);
end;

function TTMSFNCMacPDFRichTextLib.GetRichTextRange(ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String;
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
  dt: NSData;
  dic: NSDictionary;
  sel: TTMSFNCPDFRichTextLibRange;
  res: NSString;
  dtm: NSMutableData;
  ar: NSKeyedArchiver;
  st: TStringStream;
{$ENDIF}
begin
  Result := '';
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    dt := nil;
    if ADataType = dtArchivedXMLDocumentType then
    begin
      dtm := TNSMutableData.Wrap(TNSMutableData.OCClass.data);
      ar := TNSKeyedArchiver.Wrap(TNSKeyedArchiver.Wrap(TNSKeyedArchiver.OCClass.alloc).initForWritingWithMutableData(dtm));
      ar.setOutputFormat(NSPropertyListXMLFormat_v1_0);
      ar.encodeObject((FAttributedText as ILocalObject).GetObjectID, NSStrEx('root'));
      ar.finishEncoding;
      st := TStringStream.Create;
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      st.Write(dtm.bytes^, dtm.length);
      {$ELSE}
      st.Write(dtm.bytes, dtm.length);
      {$IFEND}
      {$HINTS ON}
      Result := st.DataString;
      st.Free;
      ar.release;
      ar := nil;
    end
    else
    begin
      sel := GetSelection;
      if (AStart > -1) and (ALength > -1) then
      begin
        sel.location := AStart;
        sel.length := ALength;
      end;

      att := FAttributedText;
      dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(GetDataType(ADataType),
        (CocoaNSStringConst(AppKitFwk, 'NSDocumentTypeDocumentAttribute') as ILocalObject).GetObjectID));
      case ADataType of
        dtRTFTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).RTFFromRange(NSMakeRange(sel.location, sel.length), dic);
        dtRTFDTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).RTFDFromRange(NSMakeRange(sel.location, sel.length), dic);
        dtDocFormatTextDocumentType: dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).docFormatFromRange(NSMakeRange(sel.location, sel.length), dic);
        else dt := TNSAttributedStringEx.Wrap((att as ILocalObject).GetObjectID).dataFromRange(NSMakeRange(sel.location, sel.length), dic, nil);
      end;

      if Assigned(dt) then
      begin
        res := TNSString.Wrap(TNSString.Wrap(TNSString.OCClass.alloc).initWithData(dt, NSUTF8StringEncoding));
        Result := UTF8ToString(res.UTF8String);
        res.release;
        res := nil;
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.GetItalic(AStart,
  ALength: Integer): Boolean;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Italic;
end;

function TTMSFNCMacPDFRichTextLib.GetSelection: TTMSFNCPDFRichTextLibRange;
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    Result.location := 0;
    Result.length := FAttributedText.length;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.GetStrikethrough(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength, True);
  Result := val.Strikethrough;
end;

function TTMSFNCMacPDFRichTextLib.GetStrikethroughColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength, True);
  Result := val.StrikethroughColor;
end;

function TTMSFNCMacPDFRichTextLib.GetStrokeColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength, True);
  Result := val.StrokeColor;
end;

function TTMSFNCMacPDFRichTextLib.GetStrokeWidth(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength, True);
  Result := val.StrokeWidth;
end;

function TTMSFNCMacPDFRichTextLib.GetSubscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCMacPDFRichTextLib.GetSuperscript(AStart,
  ALength: Integer): Single;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TTMSFNCMacPDFRichTextLib.GetText: String;
begin
  Result := FText;
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
    Result := UTF8ToString(TNSString.Wrap(objc_msgSend((FAttributedText as IlocalObject).GetObjectID, sel_getUid(MarshaledAString('string')))).UTF8String);
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.GetTextLength: Integer;
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    att := FAttributedText;
    if Assigned(att) then
      Result := att.length;
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.GetToolTip(AStart,
  ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength, True);
  Result := val.ToolTip;
end;

function TTMSFNCMacPDFRichTextLib.GetUnderline(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength, True);
  Result := val.Underline;
end;

function TTMSFNCMacPDFRichTextLib.GetUnderlineColor(AStart,
  ALength: Integer): TTMSFNCGraphicsColor;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength, True);
  Result := val.UnderlineColor;
end;

function TTMSFNCMacPDFRichTextLib.GetURL(AStart, ALength: Integer): String;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength, True);
  Result := val.URL;
end;

{$IFDEF MACOS_OK}
function TTMSFNCMacPDFRichTextLib.GetUnderlineStyleType(
  AValue: Integer): TTMSFNCPDFRichTextLibUnderlineStyles;
begin
  Result := [];
  if AValue and NSUnderlineByWord = NSUnderlineByWord then
    Result := Result + [usUnderlineByWord];

  if AValue = NSUnderlineStyleNone then
    Result := Result + [usUnderlineStyleNone];
  if AValue and NSUnderlineStyleSingle = NSUnderlineStyleSingle then
    Result := Result + [usUnderlineStyleSingle];
  if AValue and NSUnderlineStyleDouble = NSUnderlineStyleDouble then
    Result := Result + [usUnderlineStyleDouble];
  if AValue and NSUnderlineStyleThick = NSUnderlineStyleThick then
    Result := Result + [usUnderlineStyleThick];

  if AValue and NSUnderlinePatternSolid = NSUnderlinePatternSolid then
    Result := Result + [usUnderlinePatternSolid];
  if AValue and NSUnderlinePatternDot = NSUnderlinePatternDot then
    Result := Result + [usUnderlinePatternDot];
  if AValue and NSUnderlinePatternDash = NSUnderlinePatternDash then
    Result := Result + [usUnderlinePatternDash];
  if AValue and NSUnderlinePatternDashDot = NSUnderlinePatternDashDot then
    Result := Result + [usUnderlinePatternDashDot];
  if AValue and NSUnderlinePatternDashDotDot = NSUnderlinePatternDashDotDot then
    Result := Result + [usUnderlinePatternDashDotDot];
end;

function TTMSFNCMacPDFRichTextLib.GetUnderlineStyleValue(
  AValue: TTMSFNCPDFRichTextLibUnderlineStyles): Integer;
begin
  Result := 0;

  if usUnderlineStyleSingle in AValue then
    Result := Result or NSUnderlineStyleSingle;

  if usUnderlineStyleThick in AValue then
    Result := Result or NSUnderlineStyleThick;

  if usUnderlineStyleDouble in AValue then
    Result := Result or NSUnderlineStyleDouble;

  if usUnderlinePatternSolid in AValue then
    Result := Result or NSUnderlinePatternSolid;

  if usUnderlinePatternDot in AValue then
    Result := Result or NSUnderlinePatternDot;

  if usUnderlinePatternDash in AValue then
    Result := Result or NSUnderlinePatternDash;

  if usUnderlinePatternDashDot in AValue then
    Result := Result or NSUnderlinePatternDashDot;

  if usUnderlinePatternDashDotDot in AValue then
    Result := Result or NSUnderlinePatternDashDotDot;

  if usUnderlineByWord in AValue then
    Result := Result or NSUnderlineByWord;
end;
{$ENDIF}

function TTMSFNCMacPDFRichTextLib.GetValues(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(Result);
  ProcessAllAttributes(Result, AStart, ALength, True);
end;

procedure TTMSFNCMacPDFRichTextLib.ImportData(AFileName: String; AType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  dt := TNSData.Wrap(TNSData.OCClass.dataWithContentsOfFile(NSStrEx(AFileName)));
  if Assigned(dt) then
  begin
    if AType = dtArchivedXMLDocumentType then
      att := TNSAttributedString.Wrap(TNSKeyedUnarchiver.OCClass.unarchiveObjectWithFile(NSStrEx(AFileName)))
    else
    begin
      case AType of
        dtRTFTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithRTF(dt, nil));
        dtRTFDTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithRTFD(dt, nil));
        dtHTMLTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithHTML(dt, nil));
        dtDocFormatTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithDocFormat(dt, nil));
        else
          att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithData(dt, nil, nil, nil));
      end;
    end;

    if Assigned(FAttributedText) then
    begin
      FAttributedText.release;
      FAttributedText := nil;
    end;

    FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));

    if AType <> dtArchivedXMLDocumentType then
      att.release;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ImportFromStream(AStream: TMemoryStream);
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  dt := TNSData.Wrap(TNSData.OCClass.dataWithBytes(AStream.Memory, AStream.Size));
  if Assigned(dt) then
  begin
    att := TNSAttributedString.Wrap(TNSKeyedUnarchiver.OCClass.unarchiveObjectWithData(dt));
    if Assigned(FAttributedText) then
    begin
      FAttributedText.release;
      FAttributedText := nil;
    end;
    FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.InitializeValues(
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
  AValues.FontName := 'Helvetica Neue';
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

procedure TTMSFNCMacPDFRichTextLib.SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Italic := AValue;
  val.ApplyItalic := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetParagraphStyle(
  AValue: TTMSFNCPDFRichTextLibParagraphStyle; AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ParagraphStyle := AValue;
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetRichText(ARichText: String;
  ADataType: TTMSFNCPDFRichTextLibDataType = dtArchivedXMLDocumentType);
{$IFDEF MACOS_OK}
var
  att: NSAttributedString;
  dt: NSData;
  dtm: NSMutableData;
  ar: NSKeyedUnarchiver;
  st: TStringStream;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if ARichText <> '' then
  begin
    if ADataType = dtArchivedXMLDocumentType then
    begin
      st := TStringStream.Create(ARichText);
      dtm := TNSMutableData.Wrap(TNSMutableData.OCClass.dataWithBytes(st.Bytes, st.Size));
      st.Free;
      ar := TNSKeyedUnarchiver.Wrap(TNSKeyedUnarchiver.Wrap(TNSKeyedUnarchiver.OCClass.alloc).initForReadingWithData(dtm));
      att := TNSAttributedString.Wrap(ar.decodeObjectForKey(NSStrEx('root')));
      ar.finishDecoding;
      ar.release;
      ar := nil;
      if Assigned(FAttributedText) then
      begin
        FAttributedText.release;
        FAttributedText := nil;
      end;
      FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));
    end
    else
    begin
      dt := NSStrEx(ARichText).dataUsingEncoding(NSUTF8StringEncoding);
      if Assigned(dt) then
      begin
        case ADataType of
          dtRTFTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithRTF(dt, nil));
          dtRTFDTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithRTFD(dt, nil));
          dtHTMLTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithHTML(dt, nil));
          dtDocFormatTextDocumentType: att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithDocFormat(dt, nil));
          else
            att := TNSAttributedString.Wrap(TNSAttributedStringEx.Wrap(TNSAttributedString.OCClass.alloc).initWithData(dt, nil, nil, nil));
        end;

        if Assigned(FAttributedText) then
        begin
          FAttributedText.release;
          FAttributedText := nil;
        end;

        FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));

        att.release;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ProcessAllAttributes(var AValues: TTMSFNCPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
{$IFDEF MACOS_OK}
var
  att: NSMutableAttributedString;
  sel: TTMSFNCPDFRichTextLibRange;
  selr: NSRange;
  str: NSString;
  p: Pointer;
  rgn: NSRange;
  ft, ftNew: NSFont;
  font, fontNew: CTFontRef;
  traits: CTFontSymbolicTraits;
  isBold, isItalic: Boolean;
  ftStr: NSString;
  pValue: Pointer;
  ftSize: Single;
  ftName, ftFamily: NSString;
  pr: NSMutableParagraphStyle;
  pre: NSParagraphStyle;
  AAtributeName: TTMSFNCPDFRichTextLibAttributeName;
  dic: NSDictionary;
  arr: NSArray;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    att := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Wrap(TNSMutableAttributedString.OCClass.alloc).initWithAttributedString(FAttributedText));
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    selr := NSMakeRange(sel.location, sel.length);

    rgn := NSMakeRange(0, 0);
    pValue := nil;

    while selr.length > 0 do
    begin
      dic := att.attributesAtIndex(selr.location, @rgn, selr);

      if rgn.length = 0 then
        rgn := selr;

      if Assigned(dic) then
      begin
        arr := dic.allKeys;
        for I := 0 to arr.count - 1 do
        begin
          str := TNSString.Wrap(arr.objectAtIndex(I));
          p := dic.valueForKey(str);
          AAtributeName := GetAttribute(str);
          case AAtributeName of
            anFontAttributeName:
            begin
              if not Assigned(p) then
                p := TNSFont.OCClass.systemFontOfSize(12);
              if Assigned(p) then
              begin
                ft := TNSFont.Wrap(p);

                if AValues.ApplyFontName then
                  ftName := NSStrEx(AValues.FontName)
                else
                  ftName := ft.fontName;

                ftFamily := ft.familyName;

                if AValues.ApplyFontSize then
                  ftSize := AValues.FontSize
                else
                  ftSize := ft.pointSize;

                fontNew := nil;
                font := CTFontCreateWithName((ftName as ILocalObject).GetObjectID, ftSize, nil);

                if ARetrieve then
                begin
                  traits := CTFontGetSymbolicTraits(font);
                  isBold := (traits and kCTFontBoldTrait) = kCTFontBoldTrait;
                  isItalic := (traits and kCTFontItalicTrait) = kCTFontItalicTrait;
                  AValues.FontName := UTF8ToString(ftName.UTF8String);
                  AValues.FontFamily := UTF8ToString(ftFamily.UTF8String);
                  AValues.FontSize := ftSize;
                  AValues.Bold := isBold;
                  AValues.Italic := isItalic;
                end
                else
                begin
                  if AValues.ApplyBold then
                  begin
                    if AValues.Bold then
                      fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, kCTFontBoldTrait, kCTFontBoldTrait)
                    else
                      fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, 0, kCTFontBoldTrait);
                  end;

                  if AValues.ApplyItalic then
                  begin
                    if AValues.Italic then
                      fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, kCTFontItalicTrait, kCTFontItalicTrait)
                    else
                      fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, 0, kCTFontItalicTrait);
                  end;

                  if not Assigned(fontNew) then
                    ftStr := TNSString.wrap(CTFontCopyPostScriptName(font))
                  else
                    ftStr := TNSString.wrap(CTFontCopyPostScriptName(fontNew));

                  ftNew := TNSFont.Wrap(TNSFont.OCClass.fontWithName(ftStr, ftSize));

                  pValue := (ftNew as ILocalObject).GetObjectID;
                end;

                if Assigned(fontNew) then
                  CFRelease(fontNew);
                CFRelease(font);
              end;
            end;
            anParagraphStyleAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                begin
                  pre := TNSParagraphStyle.Wrap(p);
                  case pre.alignment of
                    0: AValues.ParagraphStyle.Alignment := gtaLeading;
                    1: AValues.ParagraphStyle.Alignment := gtaTrailing;
                    2: AValues.ParagraphStyle.Alignment := gtaCenter;
                  end;
                  AValues.ParagraphStyle.FirstLineHeadIndent := pre.firstLineHeadIndent;
                  AValues.ParagraphStyle.HeadIndent := pre.headIndent;
                  AValues.ParagraphStyle.TailIndent := pre.tailIndent;
                  AValues.ParagraphStyle.LineHeightMultiple := pre.lineHeightMultiple;
                  AValues.ParagraphStyle.MaximumLineHeight := pre.maximumLineHeight;
                  AValues.ParagraphStyle.MinimumLineHeight := pre.minimumLineHeight;
                  AValues.ParagraphStyle.LineSpacing := pre.lineSpacing;
                  AValues.ParagraphStyle.ParagraphSpacing := pre.paragraphSpacing;
                  AValues.ParagraphStyle.ParagraphSpacingBefore := pre.paragraphSpacingBefore;
                  AValues.ParagraphStyle.LineBreakMode := TTMSFNCPDFGraphicsLibLineBreakMode(pre.lineBreakMode);
                  AValues.ParagraphStyle.HyphenationFactor := pre.hyphenationFactor;
                  //Tabstops?
                end;
              end
              else
              begin
                pr := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
                case AValues.ParagraphStyle.Alignment of
                  gtaLeading: pr.setAlignment(0);
                  gtaCenter: pr.setAlignment(2);
                  gtaTrailing: pr.setAlignment(1);
                end;
                pr.setFirstLineHeadIndent(AValues.ParagraphStyle.FirstLineHeadIndent);
                pr.setHeadIndent(AValues.ParagraphStyle.HeadIndent);
                pr.setTailIndent(AValues.ParagraphStyle.TailIndent);
                pr.setLineHeightMultiple(AValues.ParagraphStyle.LineHeightMultiple);
                pr.setMaximumLineHeight(AValues.ParagraphStyle.MaximumLineHeight);
                pr.setMinimumLineHeight(AValues.ParagraphStyle.MinimumLineHeight);
                pr.setLineSpacing(AValues.ParagraphStyle.LineSpacing);
                pr.setParagraphSpacing(AValues.ParagraphStyle.ParagraphSpacing);
                pr.setParagraphSpacingBefore(AValues.ParagraphStyle.ParagraphSpacingBefore);
                pr.setLineBreakMode(Integer(AValues.ParagraphStyle.LineBreakMode));
                pr.setHyphenationFactor(AValues.ParagraphStyle.HyphenationFactor);
                //Tabstops?
                pValue := (pr as ILocalObject).GetObjectID;
              end;
            end;
            anForegroundColorAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.ForegroundColor := NSColorToAlphaColor(TNSColor.Wrap(p));
              end
              else
                pValue := (AlphaColorToNSColor(AValues.ForegroundColor) as ILocalObject).GetObjectID;
            end;
            anBackgroundColorAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.BackgroundColor := NSColorToAlphaColor(TNSColor.Wrap(p));
              end
              else
                pValue := (AlphaColorToNSColor(AValues.BackgroundColor) as ILocalObject).GetObjectID;
            end;
            anLigatureAttributeName: ;
            anKernAttributeName: ;
            anStrikethroughStyleAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.Strikethrough := GetUnderlineStyleType(TNSNumber.Wrap(p).intValue);
              end
              else
                pValue := TNSNumber.OCClass.numberWithInt(GetUnderlineStyleValue(AValues.Strikethrough));
            end;
            anUnderlineStyleAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.Underline := GetUnderlineStyleType(TNSNumber.Wrap(p).intValue);
              end
              else
                pValue := TNSNumber.OCClass.numberWithInt(GetUnderlineStyleValue(AValues.Underline));
            end;
            anStrokeColorAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.StrokeColor := NSColorToAlphaColor(TNSColor.Wrap(p));
              end
              else
                pValue := (AlphaColorToNSColor(AValues.StrokeColor) as ILocalObject).GetObjectID;
            end;
            anStrokeWidthAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.StrokeWidth := TNSNumber.Wrap(p).floatValue;
              end
              else
                pValue := TNSNumber.OCClass.numberWithFloat(AValues.StrokeWidth);
            end;
            anShadowAttributeName: ;
            anTextEffectAttributeName: ;
            anToolTipAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.ToolTip := UTF8ToString(TNSString.Wrap(p).UTF8String);
              end
              else
                pValue := TNSString.OCClass.stringWithString(NSStrEx(AValues.ToolTip));
            end;
            anLinkAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.URL := UTF8ToString(TNSURL.Wrap(p).absoluteString.UTF8String);
              end
              else
                pValue := TNSURL.OCClass.URLWithString(NSStrEx(AValues.URL));
            end;
            anBaselineOffsetAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.BaselineOffset := TNSNumber.Wrap(p).floatValue;
              end
              else
                pValue := TNSNumber.OCClass.numberWithFloat(AValues.BaselineOffset);
            end;
            anUnderlineColorAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.UnderlineColor := NSColorToAlphaColor(TNSColor.Wrap(p));
              end
              else
                pValue := (AlphaColorToNSColor(AValues.UnderlineColor) as ILocalObject).GetObjectID;
            end;
            anStrikethroughColorAttributeName:
            begin
              if ARetrieve then
              begin
                if Assigned(p) then
                  AValues.StrikethroughColor := NSColorToAlphaColor(TNSColor.Wrap(p));
              end
              else
                pValue := (AlphaColorToNSColor(AValues.StrikethroughColor) as ILocalObject).GetObjectID;
            end;
            anObliquenessAttributeName: ;
            anExpansionAttributeName: ;
            anWritingDirectionAttributeName: ;
            anVerticalGlyphFormAttributeName: ;
          end;

          if Assigned(pValue) and not ARetrieve then
            att.addAttribute(str, pValue, rgn);

          selr := NSMakeRange(NSMaxRange(rgn), NSMaxRange(selr) - NSMaxRange(rgn));
        end;
      end;
    end;
    if not ARetrieve then
    begin
      if Assigned(FAttributedText) then
      begin
        FAttributedText.release;
        FAttributedText := nil;
      end;
      FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));
    end;
    att.release;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ProcessAttribute(
  AAtributeName: TTMSFNCPDFRichTextLibAttributeName;
  var AValues: TTMSFNCPDFRichTextLibAttributeValue;
  AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
{$IFDEF MACOS_OK}
var
  attText: NSAttributedString;
  att: NSMutableAttributedString;
  sel: TTMSFNCPDFRichTextLibRange;
  selr: NSRange;
  str: NSString;
  p: Pointer;
  rgn: NSRange;
  ft, ftNew: NSFont;
  font, fontNew: CTFontRef;
  traits: CTFontSymbolicTraits;
  isBold, isItalic: Boolean;
  ftStr: NSString;
  pValue: Pointer;
  ftSize: Single;
  ftName, ftFamily: NSString;
  pr: NSMutableParagraphStyle;
  pre: NSParagraphStyle;
  t: NSTextAttachment;
  img: NSImage;
  tcl: NSTextAttachmentCell;
  fw: NSFileWrapper;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    att := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Wrap(TNSMutableAttributedString.OCClass.alloc).initWithAttributedString(FAttributedText));
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    selr := NSMakeRange(sel.location, sel.length);

    str := GetAttributeName(AAtributeName);

    if AAtributeName = anAttachmentAttributeName then
    begin
      if Assigned(AValues.Bitmap) then
      begin
        t := TNSTextAttachment.Wrap(TNSTextAttachment.Wrap(TNSTextAttachment.OCClass.alloc).init);
        img := ImageFromBitmap(AValues.Bitmap);
        tcl := TNSTextAttachmentCell.Wrap(TNSTextAttachmentCell.Wrap(TNSTextAttachmentCell.OCClass.alloc).init);
        tcl.setImage(img);
        t.setAttachmentCell(MacApi.AppKit.NSTextAttachmentCell(tcl));
        img.release;
        tcl.release;

        attText := TNSAttributedString.Wrap(TNSAttributedStringEx.OCClass.attributedStringWithAttachment(t));
        att.insertAttributedString(attText, sel.location);
      end
      else if AValues.BitmapFile <> '' then
      begin
        fw := TNSFileWrapper.Wrap(TNSFileWrapper.Wrap(TNSFileWrapper.OCClass.alloc).initWithPath(NSStrEx(AValues.BitmapFile)));
        t := TNSTextAttachment.Wrap(TNSTextAttachment.Wrap(TNSTextAttachment.OCClass.alloc).initWithFileWrapper(fw));
        attText := TNSAttributedString.Wrap(TNSAttributedStringEx.OCClass.attributedStringWithAttachment(t));
        att.insertAttributedString(attText, sel.location);
        t.release;
        fw.release;
      end;
    end
    else
    begin
      rgn := NSMakeRange(0, 0);
      pValue := nil;
      while selr.length > 0 do
      begin
        p := att.attribute(str, selr.location, @rgn, selr);

        if rgn.length = 0 then
          rgn := selr;

        case AAtributeName of
          anFontAttributeName:
          begin
            if not Assigned(p) then
              p := TNSFont.OCClass.systemFontOfSize(12);
            if Assigned(p) then
            begin
              ft := TNSFont.Wrap(p);

              if AValues.ApplyFontName then
                ftName := NSStrEx(AValues.FontName)
              else
                ftName := ft.fontName;

              ftFamily := ft.familyName;

              if AValues.ApplyFontSize then
                ftSize := AValues.FontSize
              else
                ftSize := ft.pointSize;

              fontNew := nil;
              font := CTFontCreateWithName((ftName as ILocalObject).GetObjectID, ftSize, nil);

              if ARetrieve then
              begin
                traits := CTFontGetSymbolicTraits(font);
                isBold := (traits and kCTFontBoldTrait) = kCTFontBoldTrait;
                isItalic := (traits and kCTFontItalicTrait) = kCTFontItalicTrait;
                AValues.FontName := UTF8ToString(ftName.UTF8String);
                AValues.FontFamily := UTF8ToString(ftFamily.UTF8String);
                AValues.FontSize := ftSize;
                AValues.Bold := isBold;
                AValues.Italic := isItalic;
              end
              else
              begin
                if AValues.ApplyBold then
                begin
                  if AValues.Bold then
                    fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, kCTFontBoldTrait, kCTFontBoldTrait)
                  else
                    fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, 0, kCTFontBoldTrait);
                end;

                if AValues.ApplyItalic then
                begin
                  if AValues.Italic then
                    fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, kCTFontItalicTrait, kCTFontItalicTrait)
                  else
                    fontNew := CTFontCreateCopyWithSymbolicTraits(font, ft.pointSize, nil, 0, kCTFontItalicTrait);
                end;

                if not Assigned(fontNew) then
                  ftStr := TNSString.wrap(CTFontCopyPostScriptName(font))
                else
                  ftStr := TNSString.wrap(CTFontCopyPostScriptName(fontNew));

                ftNew := TNSFont.Wrap(TNSFont.OCClass.fontWithName(ftStr, ftSize));

                pValue := (ftNew as ILocalObject).GetObjectID;
              end;

              if Assigned(fontNew) then
                CFRelease(fontNew);
              CFRelease(font);
            end;
          end;
          anParagraphStyleAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
              begin
                pre := TNSParagraphStyle.Wrap(p);
                case pre.alignment of
                  0: AValues.ParagraphStyle.Alignment := gtaLeading;
                  1: AValues.ParagraphStyle.Alignment := gtaTrailing;
                  2: AValues.ParagraphStyle.Alignment := gtaCenter;
                end;
                AValues.ParagraphStyle.FirstLineHeadIndent := pre.firstLineHeadIndent;
                AValues.ParagraphStyle.HeadIndent := pre.headIndent;
                AValues.ParagraphStyle.TailIndent := pre.tailIndent;
                AValues.ParagraphStyle.LineHeightMultiple := pre.lineHeightMultiple;
                AValues.ParagraphStyle.MaximumLineHeight := pre.maximumLineHeight;
                AValues.ParagraphStyle.MinimumLineHeight := pre.minimumLineHeight;
                AValues.ParagraphStyle.LineSpacing := pre.lineSpacing;
                AValues.ParagraphStyle.ParagraphSpacing := pre.paragraphSpacing;
                AValues.ParagraphStyle.ParagraphSpacingBefore := pre.paragraphSpacingBefore;
                AValues.ParagraphStyle.LineBreakMode := TTMSFNCPDFGraphicsLibLineBreakMode(pre.lineBreakMode);
                AValues.ParagraphStyle.HyphenationFactor := pre.hyphenationFactor;
                //Tabstops?
              end;
            end
            else
            begin
              pr := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
              case AValues.ParagraphStyle.Alignment of
                gtaLeading: pr.setAlignment(0);
                gtaCenter: pr.setAlignment(2);
                gtaTrailing: pr.setAlignment(1);
              end;
              pr.setFirstLineHeadIndent(AValues.ParagraphStyle.FirstLineHeadIndent);
              pr.setHeadIndent(AValues.ParagraphStyle.HeadIndent);
              pr.setTailIndent(AValues.ParagraphStyle.TailIndent);
              pr.setLineHeightMultiple(AValues.ParagraphStyle.LineHeightMultiple);
              pr.setMaximumLineHeight(AValues.ParagraphStyle.MaximumLineHeight);
              pr.setMinimumLineHeight(AValues.ParagraphStyle.MinimumLineHeight);
              pr.setLineSpacing(AValues.ParagraphStyle.LineSpacing);
              pr.setParagraphSpacing(AValues.ParagraphStyle.ParagraphSpacing);
              pr.setParagraphSpacingBefore(AValues.ParagraphStyle.ParagraphSpacingBefore);
              pr.setLineBreakMode(Integer(AValues.ParagraphStyle.LineBreakMode));
              pr.setHyphenationFactor(AValues.ParagraphStyle.HyphenationFactor);
              //Tabstops?
              pValue := (pr as ILocalObject).GetObjectID;
            end;
          end;
          anForegroundColorAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.ForegroundColor := NSColorToAlphaColor(TNSColor.Wrap(p));
            end
            else
              pValue := (AlphaColorToNSColor(AValues.ForegroundColor) as ILocalObject).GetObjectID;
          end;
          anBackgroundColorAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.BackgroundColor := NSColorToAlphaColor(TNSColor.Wrap(p));
            end
            else
              pValue := (AlphaColorToNSColor(AValues.BackgroundColor) as ILocalObject).GetObjectID;
          end;
          anLigatureAttributeName: ;
          anKernAttributeName: ;
          anStrikethroughStyleAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.Strikethrough := GetUnderlineStyleType(TNSNumber.Wrap(p).intValue);
            end
            else
              pValue := TNSNumber.OCClass.numberWithInt(GetUnderlineStyleValue(AValues.Strikethrough));
          end;
          anUnderlineStyleAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.Underline := GetUnderlineStyleType(TNSNumber.Wrap(p).intValue);
            end
            else
              pValue := TNSNumber.OCClass.numberWithInt(GetUnderlineStyleValue(AValues.Underline));
          end;
          anStrokeColorAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.StrokeColor := NSColorToAlphaColor(TNSColor.Wrap(p));
            end
            else
              pValue := (AlphaColorToNSColor(AValues.StrokeColor) as ILocalObject).GetObjectID;
          end;
          anStrokeWidthAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.StrokeWidth := TNSNumber.Wrap(p).floatValue;
            end
            else
              pValue := TNSNumber.OCClass.numberWithFloat(AValues.StrokeWidth);
          end;
          anShadowAttributeName: ;
          anTextEffectAttributeName: ;
          anToolTipAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.ToolTip := UTF8ToString(TNSString.Wrap(p).UTF8String);
            end
            else
              pValue := TNSString.OCClass.stringWithString(NSStrEx(AValues.ToolTip));
          end;
          anLinkAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.URL := UTF8ToString(TNSURL.Wrap(p).absoluteString.UTF8String);
            end
            else
              pValue := TNSURL.OCClass.URLWithString(NSStrEx(AValues.URL));
          end;
          anBaselineOffsetAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.BaselineOffset := TNSNumber.Wrap(p).floatValue;
            end
            else
              pValue := TNSNumber.OCClass.numberWithFloat(AValues.BaselineOffset);
          end;
          anUnderlineColorAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.UnderlineColor := NSColorToAlphaColor(TNSColor.Wrap(p));
            end
            else
              pValue := (AlphaColorToNSColor(AValues.UnderlineColor) as ILocalObject).GetObjectID;
          end;
          anStrikethroughColorAttributeName:
          begin
            if ARetrieve then
            begin
              if Assigned(p) then
                AValues.StrikethroughColor := NSColorToAlphaColor(TNSColor.Wrap(p));
            end
            else
              pValue := (AlphaColorToNSColor(AValues.StrikethroughColor) as ILocalObject).GetObjectID;
          end;
          anObliquenessAttributeName: ;
          anExpansionAttributeName: ;
          anWritingDirectionAttributeName: ;
          anVerticalGlyphFormAttributeName: ;
        end;

        if Assigned(pValue) and not ARetrieve then
          att.addAttribute(str, pValue, rgn);

        selr := NSMakeRange(NSMaxRange(rgn), NSMaxRange(selr) - NSMaxRange(rgn));
      end;
    end;

    if not ARetrieve then
    begin
      if Assigned(FAttributedText) then
      begin
        FAttributedText.release;
        FAttributedText := nil;
      end;
      FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(att));
    end;
    att.release;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
{$IFDEF MACOS_OK}
var
  attm: NSMutableAttributedString;
  att: NSAttributedString;
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    att := FAttributedText;
    attm := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Wrap(TNSMutableAttributedString.OCClass.alloc).initWithAttributedString(att));

    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    attm.replaceCharactersInRange(NSMakeRange(sel.location, sel.length), NSStrEx(AText));

    FAttributedText.release;
    FAttributedText := nil;
    FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithAttributedString(attm));
  end;
  {$ENDIF}
end;

function TTMSFNCMacPDFRichTextLib.GetParagraphStyle(AStart,
  ALength: Integer): TTMSFNCPDFRichTextLibParagraphStyle;
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength, True);
  Result := val.ParagraphStyle;
end;

function TTMSFNCMacPDFRichTextLib.GetPlainText: String;
begin
  Result := GetPlainTextRange(0, GetTextLength)
end;

function TTMSFNCMacPDFRichTextLib.GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String;
{$IFDEF MACOS_OK}
var
  attsel: NSAttributedString;
  sel: TTMSFNCPDFRichTextLibRange;
{$ENDIF}
begin
  Result := '';
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    sel := GetSelection;
    if (AStart > -1) and (ALength > -1) then
    begin
      sel.location := AStart;
      sel.length := ALength;
    end;

    attsel := TNSAttributedString.Wrap(FAttributedText.attributedSubstringFromRange(NSMakeRange(sel.location, sel.length)));
    if Assigned(attsel) then
      Result := UTF8ToString(TNSString.Wrap(objc_msgSend((attsel as ILocalObject).GetObjectID, sel_getUid(MarshaledAString('string')))).UTF8String);
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFRichTextLib.SetStrikethrough(AValue: TTMSFNCPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.Strikethrough := AValue;
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetStrikethroughColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrikethroughColor := AValue;
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetStrokeColor(AValue: TTMSFNCGraphicsColor;
  AStart, ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrokeColor := AValue;
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetStrokeWidth(AWidth: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.StrokeWidth := AWidth;
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetSubscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetSuperscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.SetText(const Value: String);
begin
  FText := Value;
  UpdateText;
end;

procedure TTMSFNCMacPDFRichTextLib.SetToolTip(AValue: String; AStart,
  ALength: Integer);
var
  val: TTMSFNCPDFRichTextLibAttributeValue;
begin
  InitializeValues(val);
  val.ToolTip := AValue;
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength);
end;

procedure TTMSFNCMacPDFRichTextLib.UpdateText;
begin
  {$IFDEF MACOS_OK}
  if Assigned(FAttributedText) then
  begin
    FAttributedText.release;
    FAttributedText := nil;
  end;
  FAttributedText := TNSAttributedString.Wrap(TNSAttributedString.Wrap(TNSAttributedString.OCClass.alloc).initWithString(NSStrEx(FText)));
  {$ENDIF}
end;

end.

