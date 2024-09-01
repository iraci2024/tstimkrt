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

unit FMX.TMSFNCPDFGraphicsLib.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFGraphicsLibService;
procedure UnRegisterPDFGraphicsLibService;

implementation

{$IFDEF MACOS}
{$IFNDEF IOS}
{$DEFINE MACOS_OK}
{$ENDIF}
{$ENDIF}

uses
  Classes, Types, Math, FMX.TMSFNCPDFGraphicsLib, SysUtils, FMX.TMSFNCGraphics, FMX.TMSFNCPDFLib,
  FMX.TMSFNCTypes, FMX.TMSFNCBitmapContainer, FMX.TMSFNCPDFGraphicsLibHTMLEngine, FMX.TMSFNCGraphicsTypes
  {$IFDEF MACOS_OK}
  ,MacApi.CoreFoundation, MacApi.Foundation, MacApi.CocoaTypes, MacApi.AppKit,
  MacApi.ObjectiveC, MacApi.CoreGraphics, MacApi.ObjcRuntime
  {$ENDIF}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ,FMX.TMSFNCPDFRichTextLib, FMX.TMSFNCPDFCoreLibBase
  ;

type
  TTMSFNCMacPDFGraphicsLibService = class(TTMSFNCPDFGraphicsLibFactoryService)
  protected
    function DoCreatePDFGraphicsLib: TObject; override;
  end;

  TTMSFNCMacPDFGraphicsLib = class(TTMSFNCPDFGraphicsLibBase, ITMSFNCCustomPDFGraphicsLib, ITMSFNCCustomPDFGraphicsExLib, ITMSFNCCustomPDFInitializationLib)
  private
    {$IFDEF MACOS_OK}
    FCanvas: Pointer;
    {$ENDIF}
    FPathRect: TRectF;
    FPageHeight: Single;
    FPageWidth: Single;
    FTextRect: TRectF;
    FPDFLib: ITMSFNCCustomPDFLib;
    FOnNotifyNewPage: TNotifyEvent;
    FPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
    function GetOnNotifyNewPage: TNotifyEvent;
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
  protected
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFill(const Value: TTMSFNCPDFGraphicsFill);
    procedure SetStroke(const Value: TTMSFNCPDFGraphicsStroke);
    procedure SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetLineBreakMode(const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
    procedure SetPDFRichTextLib(const Value: ITMSFNCCustomPDFRichTextLib);
    procedure InitializeAppearance; override;
    procedure UpdateFill; override;
    procedure UpdateStroke; override;
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure UpdatePathRect(APoint: TPointF);
    procedure NotifyNewPage;
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawAddShadow(Offset: TPointF; Blur: Single); overload;
    procedure DrawAddShadow(Offset: TPointF; Blur: Single; Color: TTMSFNCGraphicsColor); overload;
    procedure DrawRoundedRectangle(Rect: TRectF; Rounding: Single);
    procedure DrawEllipse(Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure AddGoto(AText: UnicodeString; ADestination: UnicodeString; ARect: TRectF);
    procedure DrawSaveState;
    procedure DrawClear(Rect: TRectF); overload;
    procedure DrawSetTransform(m11, m12, m21, m22, dx, dy: Double); overload;
    procedure DrawSetTransform(m: TTMSFNCGraphicsMatrix); overload;
    procedure DrawRestoreState;
    procedure DrawPathBegin;
    procedure DrawPathBeginClip;
    procedure DrawPathEndClip;
    procedure DrawPathEndLinearGradient(StartPoint, EndPoint: TPointF);
    procedure DrawPathEndRadialGradient(StartCenter, EndCenter: TPointF; StartRadius, EndRadius: Single);
    procedure DrawPathMoveToPoint(Point: TPointF);
    procedure DrawPathAddLineToPoint(Point: TPointF);
    procedure DrawPathAddArc(CenterPoint: TPointF; Radius: Single; StartAngle, EndAngle: Single; Clockwise: Boolean = False);
    procedure DrawPathAddArcToPoint(FirstPoint, SecondPoint: TPointF; Radius: Single);
    procedure DrawPathAddRectangle(Rect: TRectF);
    procedure DrawPathAddEllipse(Rect: TRectF);
    procedure DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
    procedure DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
    procedure DrawPathAddQuadCurveToPoint(ControlPoint: TPointF; EndPoint: TPointF);
    procedure DrawPathClose;
    procedure DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
    function CalculateHTMLText(Text: UnicodeString; Scale: Single = 1.0): TRectF; overload;
    function CalculateHTMLText(Text: UnicodeString; Rect: TRectF; Scale: Single = 1.0): TRectF; overload;
    function CalculateText(Text: UnicodeString): TRectF; overload;
    function CalculateText(Text: UnicodeString; Rect: TRectF): TRectF; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0): Integer; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0): Integer; overload;
    function DrawImageWithName(ABitmapName: String; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageWithName(ABitmapName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function GetTextRect: TRectF;
    function GetAlignment: TTMSFNCGraphicsTextAlign;
    function GetFill: TTMSFNCPDFGraphicsFill;
    function GetStroke: TTMSFNCPDFGraphicsStroke;
    function GetFont: TTMSFNCPDFGraphicsLibFont;
    function GetURLFont: TTMSFNCPDFGraphicsLibFont;
    function GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
    function RichText: ITMSFNCCustomPDFRichTextLib;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  end;

var
  PDFGraphicsLibService: ITMSFNCPDFGraphicsLibService;

procedure RegisterPDFGraphicsLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibService, IInterface(PDFGraphicsLibService)) then
  begin
    PDFGraphicsLibService := TTMSFNCMacPDFGraphicsLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFGraphicsLibService, PDFGraphicsLibService);
  end;
end;

procedure UnregisterPDFGraphicsLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFGraphicsLibService);
end;

{ TTMSFNCMacPDFGraphicsLibService }

function TTMSFNCMacPDFGraphicsLibService.DoCreatePDFGraphicsLib: TObject;
begin
  Result := TTMSFNCMacPDFGraphicsLib.Create;
end;

function TTMSFNCMacPDFGraphicsLib.DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF;
{$IFDEF MACOS_OK}
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  ft: NSFont;
  ftms: Cardinal;
{$ENDIF}
begin
  DrawSaveState;
  {$IFDEF MACOS_OK}
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in Font.Style then
    ftms := ftms or NSFontBoldTrait;
  if TFontStyle.fsItalic in Font.Style then
    ftms := ftms or NSFontItalicTrait;
  ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx(Font.Name), ftms, 5, Font.Size);
  if ft = nil then
    ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx('Helvetica Neue'), ftms, 5, Font.Size);

  if Assigned(ft) then
    dic.setValue((ft as ILocalObject).GetObjectID, NSStrEx('NSFont'));
  dic.setValue((AlphaColorToNSColor(Font.Color) as ILocalObject).GetObjectID, NSStrEx('NSColor'));
  if TFontStyle.fsUnderline in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawAtPoint(CGPointMake(Point.X, Point.Y), dic);
  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(10000, 10000), NSStringDrawingUsesLineFragmentOrigin, dic);
  dic.release;
  FTextRect := RectF(Point.X + r.origin.x, Point.Y + r.origin.y, Point.X + r.origin.x + r.size.width, Point.Y + r.origin.y + r.size.height);
  Result := FTextRect;
  {$ENDIF}
  DrawRestoreState;
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathBeginClip;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextClip(c);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextSetShadow(c, CGSizeMake(Offset.X, -Offset.Y), Blur);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.AddGoto(AText, ADestination: UnicodeString;
  ARect: TRectF);
begin

end;

procedure TTMSFNCMacPDFGraphicsLib.AddURL(AText, AURL: UnicodeString;
  ARect: TRectF);
begin
  RichText.Text := AText;
  RichText.SetFont(URLFont.Name, URLFont.Size);
  RichText.SetForegroundColor(URLFont.Color);
  if TFontStyle.fsBold in URLFont.Style then
    RichText.SetBold;
  if TFontStyle.fsItalic in URLFont.Style then
    RichText.SetItalic;
  if TFontStyle.fsStrikeOut in URLFont.Style then
    RichText.SetStrikethrough;
  if TFontStyle.fsUnderline in URLFont.Style then
    RichText.SetUnderline;
  RichText.SetURL(AURL);

  RichText.Draw(ARect);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Rect: TRectF; Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, Rect, False, Scale, True);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, PointF(0, 0), Scale, True);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateText(
  Text: UnicodeString): TRectF;
begin
  Result := DrawText(Text, PointF(0, 0), True);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateText(Text: UnicodeString;
  Rect: TRectF): TRectF;
begin
  Result := DrawText(Text, Rect, True);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rects, Padding, True);
end;

function TTMSFNCMacPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rect: TRectF; Columns: Integer; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rect, Columns, Padding, True);
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single;
  Color: TTMSFNCGraphicsColor);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextSetShadowWithColor(c, CGSizeMake(Offset.X, -Offset.Y), Blur, TNSColorEx.Wrap((AlphaColorToNSColor(Color) as ILocalObject).GetObjectID).CGColor);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawClear(Rect: TRectF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextClearRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawEllipse(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top + Rect.Height / 2));
  DrawPathAddEllipse(Rect);
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
  begin
    DrawPathEnd(dmPathFill);
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    begin
      DrawPathBegin;
      DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top + Rect.Height / 2));
      DrawPathAddEllipse(Rect);
      DrawPathClose;
      DrawPathEnd(dmPathStroke);
    end;
  end
  else
  begin
    if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
      DrawPathEnd(dmPathFillStroke)
    else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      DrawPathEnd(dmPathFill)
    else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      DrawPathEnd(dmPathStroke);
  end;

  DrawRestoreState;
end;

function TTMSFNCMacPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Rect, BitmapContainer, Paging, AScale, Calculate);
  Result := FTextRect;
end;

function TTMSFNCMacPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Point, BitmapContainer, AScale, Calculate);
  Result := FTextRect;
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathEndClip;
begin
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
{$IFDEF MACOS_OK}
var
  img: NSImage;
  imgrep: NSBitmapImageRep;
  dic: NSDictionary;
  imgdraw: NSImage;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  img := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithContentsOfFile(NSStrEx(AFileName)));
  img.setFlipped(True);
  case ImageType of
    itOriginal: img.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
    itPNG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dt := imgrep.representationUsingType(NSPNGFileType, nil);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
    itJPG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(TNSNumber.OCClass.numberWithFloat(Quality),
          (NSStrEx('NSImageCompressionFactor') as ILocalObject).GetObjectID));
        dt := imgrep.representationUsingType(NSJPEGFileType, dic);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
  end;
  Result := RectF(Point.X, Point.Y, Point.X + img.size.width, Point.Y + img.size.height);

  img.release;
  {$ENDIF}
end;

function TTMSFNCMacPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
{$IFDEF MACOS_OK}
var
  img: NSImage;
  imgrep: NSBitmapImageRep;
  dic: NSDictionary;
  imgdraw: NSImage;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  img := ImageFromBitmap(ABitmap);
  img.setFlipped(True);
  case ImageType of
    itOriginal: img.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
    itPNG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dt := imgrep.representationUsingType(NSPNGFileType, nil);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
    itJPG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(TNSNumber.OCClass.numberWithFloat(Quality),
          (NSStrEx('NSImageCompressionFactor') as ILocalObject).GetObjectID));
        dt := imgrep.representationUsingType(NSJPEGFileType, dic);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawAtPoint(CGPointMake(Point.X, Point.Y), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
  end;
  Result := RectF(Point.X, Point.Y, Point.X + img.size.width, Point.Y + img.size.height);
  img.release;
  {$ENDIF}
end;

function TTMSFNCMacPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
{$IFDEF MACOS_OK}
var
  img, imgdraw: NSImage;
  imgrep: NSBitmapImageRep;
  x, y, w, h: Single;
  rsrc, rdest: TRectF;
  dic: NSDictionary;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  img := ImageFromBitmap(ABitmap);
  img.setFlipped(True);

  GetAspectSize(x, y, w, h, img.size.width, img.size.height, Rect.Width, Rect.Height, AspectRatio, Stretch, False);
  rsrc := RectF(0, 0, img.size.Width, img.size.Height);
  if Center then
  begin
    x := Rect.Left + (Rect.Width - w) / 2;
    y := Rect.Top + (Rect.Height - h) / 2;
  end
  else
  begin
    x := Rect.Left;
    y := Rect.Top;
  end;

  rdest := RectF(x, y, x + w, y + h);
  case ImageType of
    itOriginal: img.drawInRect(CGRectMake(rdest.Left, rdest.Top, rdest.Width, rdest.Height), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
    itPNG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dt := imgrep.representationUsingType(NSPNGFileType, nil);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawInRect(CGRectMake(rdest.Left, rdest.Top, rdest.Width, rdest.Height), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
    itJPG:
    begin
      if img.representations.count > 0 then
      begin
        imgrep := TNSBitmapImageRep.Wrap(img.representations.objectAtIndex(0));
        dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(TNSNumber.OCClass.numberWithFloat(Quality),
          (NSStrEx('NSImageCompressionFactor') as ILocalObject).GetObjectID));
        dt := imgrep.representationUsingType(NSJPEGFileType, dic);
        imgdraw := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
        imgdraw.setFlipped(True);
        imgdraw.drawInRect(CGRectMake(rdest.Left, rdest.Top, rdest.Width, rdest.Height), CGRectMake(0, 0, 0, 0), NSCompositeSourceOver, 1);
        imgdraw.release;
      end;
    end;
  end;

  img.release;

  Result := rdest;
  {$ENDIF}
end;

function TTMSFNCMacPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  bmp := TTMSFNCBitmap.Create;
  try
    bmp.LoadFromFile(AFileName);
    Result := DrawImage(bmp, ABackgroundColor, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
  finally
    bmp.Free;
  end;
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageWithName(ABitmapName: String; ABackgroundColor: TTMSFNCGraphicsColor;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageWithName(ABitmapName: String; ABackgroundColor: TTMSFNCGraphicsColor;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageWithName(ABitmapName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCMacPDFGraphicsLib.DrawImageWithName(ABitmapName: String;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Point, ImageType, Quality);
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawLine(StartPoint, EndPoint: TPointF);
begin
  DrawPathBegin;
  DrawPathMoveToPoint(StartPoint);
  DrawPathAddLineToPoint(EndPoint);
  DrawPathEnd(dmPathStroke);
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathEndLinearGradient(StartPoint,
  EndPoint: TPointF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
  gradient: CGGradientRef;
  colorSpace: CGColorSpaceRef;
  colorarr: array[0..1] of CGColorRef;
  colors: CFArrayRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    colorSpace := CGColorSpaceCreateDeviceRGB;
    colorarr[0] := TNSColorEx.Wrap((AlphaColorToNSColor(Fill.Color) as ILocalObject).GetObjectID).CGColor;
    colorarr[1] := TNSColorEx.Wrap((AlphaColorToNSColor(Fill.ColorTo) as ILocalObject).GetObjectID).CGColor;

    colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
    gradient := CGGradientCreateWithColors(colorSpace, colors, nil);

    CGContextDrawLinearGradient(c, gradient, CGPointMake(StartPoint.X, StartPoint.Y),
      CGPointMake(EndPoint.X, EndPoint.Y), 0);

    CGColorSpaceRelease(colorspace);
    CGGradientRelease(gradient);
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddArc(CenterPoint: TPointF; Radius,
  StartAngle, EndAngle: Single; Clockwise: Boolean = False);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    {$HINTS OFF}
    {$IF COMPILERVERSION < 34}
    CGContextAddArc(c, CenterPoint.X, CenterPoint.Y, Radius, DegToRad(StartAngle), DegToRad(EndAngle), Integer(Clockwise));
    {$ELSE}
    CGContextAddArc(c, CenterPoint.X, CenterPoint.Y, Radius, DegToRad(StartAngle), DegToRad(EndAngle), Clockwise);
    {$IFEND}
    {$HINTS ON}
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddArcToPoint(FirstPoint,
  SecondPoint: TPointF; Radius: Single);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddArcToPoint(c, FirstPoint.X, FirstPoint.Y, SecondPoint.X, SecondPoint.Y, Radius);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddCurveToPoint(c, FirstControlPoint.X, FirstControlPoint.Y, SecondControlPoint.X, SecondControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddEllipse(Rect: TRectF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddEllipseInRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
var
  I: Integer;
begin
  if Length(Points) > 1 then
  begin
    DrawPathMoveToPoint(Points[0]);
    for I := 1 to Length(Points) - 1 do
      DrawPathAddLineToPoint(Points[I]);
  end;
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddLineToPoint(Point: TPointF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddLineToPoint(c, Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddQuadCurveToPoint(ControlPoint,
  EndPoint: TPointF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddQuadCurveToPoint(c, ControlPoint.X, ControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathAddRectangle(Rect: TRectF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextAddRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathBegin;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextBeginPath(c);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathClose;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextClosePath(c);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    {$HINTS OFF}
    {$IF COMPILERVERSION < 34}
    CGContextSetAllowsAntialiasing(c, 1);
    {$ELSE}
    CGContextSetAllowsAntialiasing(c, True);
    {$IFEND}
    {$HINTS ON}
    if (Fill.Color <> gcNull) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) and (DrawingMode in [dmPathFill, dmPathEOFill, dmPathFillStroke, dmPathEOFillStroke]) then
    begin
      if DrawingMode in [dmPathEOFill, dmPathEOFillStroke] then
        CGContextClosePath(c);

      CGContextSaveGState(c);
      CGContextClip(c);
      case Fill.Orientation of
        gfoHorizontal: DrawPathEndLinearGradient(PointF(FPathRect.Left, FPathRect.Top), PointF(FPathRect.Right, FPathRect.Top));
        gfoVertical: DrawPathEndLinearGradient(PointF(FPathRect.Left, FPathRect.Top), PointF(FPathRect.Left, FPathRect.Bottom));
      end;
      CGContextRestoreGState(c);
    end
    else
    begin
      case DrawingMode of
        dmPathFill, dmPathEOFill:
        begin
          if DrawingMode = dmPathEOFill then
            CGContextClosePath(c);
          CGContextDrawPath(c, kCGPathFill);
        end;
        dmPathStroke, dmPathEOStroke:
        begin
          if DrawingMode = dmPathEOStroke then
            CGContextClosePath(c);
          CGContextDrawPath(c, kCGPathStroke);
        end;
        dmPathFillStroke, dmPathEOFillStroke:
        begin
          if DrawingMode = dmPathEOFillStroke then
            CGContextClosePath(c);
          CGContextDrawPath(c, kCGPathFillStroke);
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathMoveToPoint(Point: TPointF);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextMoveToPoint(c, Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawPathEndRadialGradient(StartCenter,
  EndCenter: TPointF; StartRadius, EndRadius: Single);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
  gradient: CGGradientRef;
  colorSpace: CGColorSpaceRef;
  colorarr: array[0..1] of CGColorRef;
  colors: CFArrayRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    colorSpace := CGColorSpaceCreateDeviceRGB;
    colorarr[0] := TNSColorEx.Wrap((AlphaColorToNSColor(Fill.Color) as ILocalObject).GetObjectID).CGColor;
    colorarr[1] := TNSColorEx.Wrap((AlphaColorToNSColor(Fill.ColorTo) as ILocalObject).GetObjectID).CGColor;

    colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
    gradient := CGGradientCreateWithColors(colorSpace, colors, nil);

    CGContextDrawRadialGradient(c, gradient, CGPointMake(StartCenter.X, StartCenter.Y),
      StartRadius, CGPointMake(EndCenter.X, EndCenter.Y), EndRadius, 0);

    CGColorSpaceRelease(colorSpace);
    CGGradientRelease(gradient);
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawRectangle(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Bottom));
  DrawPathAddLineToPoint(PointF(Rect.Left, Rect.Bottom));
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
  begin
    DrawPathEnd(dmPathFill);
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    begin
      DrawPathBegin;
      DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top));
      DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Top));
      DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Bottom));
      DrawPathAddLineToPoint(PointF(Rect.Left, Rect.Bottom));
      DrawPathClose;
      DrawPathEnd(dmPathStroke);
    end;
  end
  else
  begin
    if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
      DrawPathEnd(dmPathFillStroke)
    else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      DrawPathEnd(dmPathFill)
    else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      DrawPathEnd(dmPathStroke);
  end;

  DrawRestoreState;
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawRestoreState;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextRestoreGState(c);
  {$ENDIF}
end;

function TTMSFNCMacPDFGraphicsLib.DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := 0;
  if Assigned(FPDFRichTextLib) then
    Result := FPDFRichTextLib.Draw(Rects, Padding, DetectOverflow);
end;

function TTMSFNCMacPDFGraphicsLib.DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := 0;
  if Assigned(FPDFRichTextLib) then
    Result := FPDFRichTextLib.Draw(Rect, Columns, Padding, DetectOverflow);
end;

function TTMSFNCMacPDFGraphicsLib.DrawRichText(Rect: TRectF;
  Calculate: Boolean): TRectF;
begin
  Result := TRectF.Empty;
  if Assigned(FPDFRichTextLib) then
    Result := FPDFRichTextLib.Draw(Rect, Calculate);
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawRoundedRectangle(Rect: TRectF;
  Rounding: Single);
{$IFDEF MACOS_OK}
var
  bez: NSBezierPath;
  {$ENDIF}
begin
  {$IFDEF MACOS_OK}
  bez := TNSBezierPath.Wrap(TNSBezierPath.OCClass.bezierPathWithRoundedRect(CGRectMake(Rect.Left, Rect.Top,
    Rect.Width, Rect.Height), Rounding, Rounding));
  bez.fill;
  bez.stroke;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawSaveState;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextSaveGState(c);
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawSetTransform(m: TTMSFNCGraphicsMatrix);
begin
  DrawSetTransform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
end;

procedure TTMSFNCMacPDFGraphicsLib.DrawSetTransform(m11, m12, m21, m22, dx,
  dy: Double);
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    CGContextConcatCTM(c, CGAffineTransformMake(m11, m12, m21, m22, dx, dy));
  {$ENDIF}
end;

function TTMSFNCMacPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF;
{$IFDEF MACOS_OK}
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  par: NSMutableParagraphStyle;
  ft: NSFont;
  ftms: Cardinal;
{$ENDIF}
begin
  DrawSaveState;
  {$IFDEF MACOS_OK}
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in Font.Style then
    ftms := ftms or NSFontBoldTrait;
  if TFontStyle.fsItalic in Font.Style then
    ftms := ftms or NSFontItalicTrait;
  ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx(Font.Name), ftms, 5, Font.Size);
  if ft = nil then
    ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx('Helvetica Neue'), ftms, 5, Font.Size);

  if Assigned(ft) then
    dic.setValue((ft as ILocalObject).GetObjectID, NSStrEx('NSFont'));

  dic.setValue((AlphaColorToNSColor(Font.Color) as ILocalObject).GetObjectID, NSStrEx('NSColor'));
  if TFontStyle.fsUnderline in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
  case Alignment of
    gtaLeading: par.setAlignment(0);
    gtaCenter: par.setAlignment(2);
    gtaTrailing: par.setAlignment(1);
  end;
  par.setLineBreakMode(Integer(LineBreakMode));
  dic.setValue((par as ILocalObject).GetObjectID, NSStrEx('NSParagraphStyle'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawInRect(CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height), dic);

  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(Rect.Width, Rect.Height), NSStringDrawingUsesLineFragmentOrigin, dic);
  par.release;
  dic.release;
  FTextRect := RectF(Rect.Left + r.origin.x, Rect.Top + r.origin.y, Rect.Left + r.origin.x + r.size.width, Rect.Top + r.origin.y + r.size.height);
  Result := FTextRect;
  {$ENDIF}
  DrawRestoreState;
end;

function TTMSFNCMacPDFGraphicsLib.DrawText(Text: UnicodeString;
  Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
{$IFDEF MACOS_OK}
var
  textStorage: NSTextStorage;
  layoutManager: NSLayoutManager;
  textContainer: NSTextContainer;
  charRange, glyphRange: NSRange;
  c: Integer;
  dic: NSMutableDictionary;
  par: NSMutableParagraphStyle;
  ft: NSFont;
  ftms: Cardinal;
  r: NSRect;
  l: Integer;
{$ENDIF}
begin
  DrawSaveState;
  Result := 0;
  if Length(Rects) > 0 then
  begin
    {$IFDEF MACOS_OK}
    dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
    ftms := 0;
    if TFontStyle.fsBold in Font.Style then
      ftms := ftms or NSFontBoldTrait;
    if TFontStyle.fsItalic in Font.Style then
      ftms := ftms or NSFontItalicTrait;
    ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx(Font.Name), ftms, 5, Font.Size);
    if ft = nil then
      ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx('Helvetica Neue'), ftms, 5, Font.Size);

    if Assigned(ft) then
      dic.setValue((ft as ILocalObject).GetObjectID, NSStrEx('NSFont'));
    dic.setValue((AlphaColorToNSColor(Font.Color) as ILocalObject).GetObjectID, NSStrEx('NSColor'));
    if TFontStyle.fsUnderline in Font.Style then
      dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
    if TFontStyle.fsStrikeOut in Font.Style then
      dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
    par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
    case Alignment of
      gtaLeading: par.setAlignment(0);
      gtaCenter: par.setAlignment(2);
      gtaTrailing: par.setAlignment(1);
    end;
    par.setLineBreakMode(Integer(LineBreakMode));
    dic.setValue((par as ILocalObject).GetObjectID, NSStrEx('NSParagraphStyle'));

    textStorage := TNSTextStorage.Wrap(TNSTextStorage.Wrap(TNSTextStorage.OCClass.alloc).initWithString(NSStrEx(Text)));
    textStorage.setAttributes(dic, NSMakeRange(0, Text.Length));
    par.release;
    dic.release;
    layoutManager := TNSLayoutManager.Wrap(TNSLayoutManager.Wrap(TNSLayoutManager.OCClass.alloc).init);

    for c := 0 to Length(Rects) - 1 do
    begin
      textContainer := TNSTextContainer.Wrap(TNSTextContainer.Wrap(TNSTextContainer.OCClass.alloc).
        initWithContainerSize(CGSizeMake(Rects[c].Width, Rects[c].Height)));
      textContainer.setLineFragmentPadding(Padding);
      layoutManager.addTextContainer(textContainer);
      textContainer.release;
    end;

    textStorage.addLayoutManager(layoutManager);
    layoutManager.release;

    l := 0;
    for c := 0 to Length(Rects) - 1 do
    begin
      r.origin.x := 0;
      r.origin.y := 0;
      r.size.width := Rects[c].Width;
      r.size.height := Rects[c].Height;
      glyphRange := layoutManager.glyphRangeForBoundingRect(r, TNSTextContainer.Wrap(layoutmanager.textContainers.objectAtIndex(c)));
      layoutManager.characterRangeForGlyphRange(glyphRange, @charRange);

      if not DetectOverflow then
      begin
        layoutManager.drawBackgroundForGlyphRange(glyphrange, CGPointMake(Rects[c].Left, Rects[c].Top));
        layoutManager.drawGlyphsForGlyphRange(glyphrange, CGPointMake(Rects[c].Left, Rects[c].Top));
      end;

      l := l + Integer(charRange.length);
    end;

    Result := Length(Text) - l;

    textStorage.release;
    {$ENDIF}
    DrawRestoreState;
  end;
end;

function TTMSFNCMacPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := DrawText(Text, arr, Padding, DetectOverflow);
end;

function TTMSFNCMacPDFGraphicsLib.GetAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := Alignment;
end;

function TTMSFNCMacPDFGraphicsLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := BitmapContainer
end;

function TTMSFNCMacPDFGraphicsLib.GetFill: TTMSFNCPDFGraphicsFill;
begin
  Result := Fill;
end;

function TTMSFNCMacPDFGraphicsLib.GetFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := Font;
end;

function TTMSFNCMacPDFGraphicsLib.GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
begin
  Result := LineBreakMode;
end;

function TTMSFNCMacPDFGraphicsLib.GetOnNotifyNewPage: TNotifyEvent;
begin
  Result := FOnNotifyNewPage;
end;

function TTMSFNCMacPDFGraphicsLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCMacPDFGraphicsLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCMacPDFGraphicsLib.GetStroke: TTMSFNCPDFGraphicsStroke;
begin
  Result := Stroke;
end;

function TTMSFNCMacPDFGraphicsLib.RichText: ITMSFNCCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

function TTMSFNCMacPDFGraphicsLib.GetTextRect: TRectF;
begin
  Result := FTextRect;
end;

function TTMSFNCMacPDFGraphicsLib.GetURLFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := URLFont;
end;

procedure TTMSFNCMacPDFGraphicsLib.InitializeAppearance;
begin
  UpdateFill;
  UpdateStroke;
end;

procedure TTMSFNCMacPDFGraphicsLib.NotifyNewPage;
begin
  if Assigned(OnNotifyNewPage) then
    OnNotifyNewPage(Self);
end;

procedure TTMSFNCMacPDFGraphicsLib.SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  Alignment := Value;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  BitmapContainer := Value;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetCanvas(ACanvas: Pointer);
begin
  {$IFDEF MACOS_OK}
  FCanvas := ACanvas;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.SetFill(const Value: TTMSFNCPDFGraphicsFill);
begin
  Fill.Assign(Value);
end;

procedure TTMSFNCMacPDFGraphicsLib.SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
begin
  Font.Assign(Value);
end;

procedure TTMSFNCMacPDFGraphicsLib.SetLineBreakMode(
  const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
begin
  LineBreakMode := Value;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetOnNotifyNewPage(
  const Value: TNotifyEvent);
begin
  FOnNotifyNewPage := Value;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetPageHeight(APageHeight: Single);
begin
  FPageHeight := APageHeight;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetPageWidth(APageWidth: Single);
begin
  FPageWidth := APageWidth;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetPDFLib(APDFLib: IInterface);
begin
  FPDFLib := APDFLib as ITMSFNCCustomPDFLib;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetPDFRichTextLib(
  const Value: ITMSFNCCustomPDFRichTextLib);
begin
  FPDFRichTextLib := Value;
end;

procedure TTMSFNCMacPDFGraphicsLib.SetStroke(
  const Value: TTMSFNCPDFGraphicsStroke);
begin
  Stroke.Assign(Value);
end;

procedure TTMSFNCMacPDFGraphicsLib.SetURLFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  URLFont.Assign(Value);
end;

procedure TTMSFNCMacPDFGraphicsLib.UpdateFill;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
    AlphaColorToNSColor(Fill.Color).setFill;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.UpdateStroke;
{$IFDEF MACOS_OK}
var
  c: CGContextRef;
  f: array of CGFloat;
{$ENDIF}
begin
  {$IFDEF MACOS_OK}
  c := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
  if Assigned(c) then
  begin
    AlphaColorToNSColor(Stroke.Color).setStroke;
    CGContextSetLineWidth(c, Stroke.Width);
    case Stroke.Kind of
      gskSolid, gskNone: CGContextSetLineDash(c, 0, nil, 0);
      gskDash:
      begin
        SetLength(f, 2);
        f[0] := 3;
        f[1] := 1;
        CGContextSetLineDash(c, 0, @f[0], Length(f));
      end;
      gskDot:
      begin
        SetLength(f, 2);
        f[0] := 1;
        f[1] := 1;
        CGContextSetLineDash(c, 0, @f[0], Length(f));
      end;
      gskDashDot:
      begin
        SetLength(f, 4);
        f[0] := 3;
        f[1] := 1;
        f[2] := 1;
        f[3] := 1;
        CGContextSetLineDash(c, 0, @f[0], Length(f));
      end;
      gskDashDotDot:
      begin
        SetLength(f, 6);
        f[0] := 3;
        f[1] := 1;
        f[2] := 1;
        f[3] := 1;
        f[4] := 1;
        f[5] := 1;
        CGContextSetLineDash(c, 0, @f[0], Length(f));
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacPDFGraphicsLib.UpdatePathRect(APoint: TPointF);
begin
  if (APoint.X < FPathRect.Left) or (FPathRect.Left = -1) then
    FPathRect.Left := APoint.X;

  if (APoint.Y < FPathRect.Top) or (FPathRect.Top = -1) then
    FPathRect.Top := APoint.Y;

  if (APoint.X > FPathRect.Right) or (FPathRect.Right = -1) then
    FPathRect.Right := APoint.X;

  if (APoint.Y > FPathRect.Bottom) or (FPathRect.Bottom = -1) then
    FPathRect.Bottom := APoint.Y;
end;

end.

