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

unit FMX.TMSFNCPDFGraphicsLib.iOS;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFGraphicsLibService;
procedure UnRegisterPDFGraphicsLibService;

implementation

uses
  Classes, Types, Math, FMX.TMSFNCPDFGraphicsLib, SysUtils, FMX.TMSFNCGraphics, FMX.TMSFNCTypes,
  FMX.TMSFNCBitmapContainer, FMX.TMSFNCPDFGraphicsLibHTMLEngine, FMX.TMSFNCPDFLib, FMX.TMSFNCGraphicsTypes
  {$IFDEF IOS}
  ,MacApi.CoreFoundation, MacApi.ObjectiveC, iOSApi.CocoaTypes, iOSApi.Foundation, iOSApi.CoreGraphics, iOSApi.UIKit
  {$ENDIF}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ,FMX.TMSFNCPDFRichTextLib, FMX.TMSFNCPDFCoreLibBase
  ;

type
  TTMSFNCiOSPDFGraphicsLibService = class(TTMSFNCPDFGraphicsLibFactoryService)
  protected
    function DoCreatePDFGraphicsLib: TObject; override;
  end;

  TTMSFNCiOSPDFGraphicsLib = class(TTMSFNCPDFGraphicsLibBase, ITMSFNCCustomPDFGraphicsLib, ITMSFNCCustomPDFGraphicsExLib, ITMSFNCCustomPDFInitializationLib)
  private
    FTextRect: TRectF;
    FPathRect: TRectF;
    FPageHeight: Single;
    FPageWidth: Single;
    FPDFLib: ITMSFNCCustomPDFLib;
    FPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
    FOnNotifyNewPage: TNotifyEvent;
    function GetOnNotifyNewPage: TNotifyEvent;
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
  protected
    procedure SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFill(const Value: TTMSFNCPDFGraphicsFill);
    procedure SetStroke(const Value: TTMSFNCPDFGraphicsStroke);
    procedure SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetLineBreakMode(const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
    procedure SetPDFRichTextLib(const Value: ITMSFNCCustomPDFRichTextLib);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure InitializeAppearance; override;
    procedure UpdateFill; override;
    procedure UpdateStroke; override;
    procedure UpdatePathRect(APoint: TPointF);
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure NotifyNewPage;
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawAddShadow(Offset: TPointF; Blur: Single); overload;
    procedure DrawAddShadow(Offset: TPointF; Blur: Single; Color: TTMSFNCGraphicsColor); overload;
    procedure DrawRoundedRectangle(Rect: TRectF; Rounding: Single);
    procedure DrawEllipse(Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure AddGoto(AText: UnicodeString; ADestination: UnicodeString; ARect: TRectF);
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function CalculateHTMLText(Text: UnicodeString; Scale: Single = 1.0): TRectF; overload;
    function CalculateHTMLText(Text: UnicodeString; Rect: TRectF; Scale: Single = 1.0): TRectF; overload;
    function CalculateText(Text: UnicodeString): TRectF; overload;
    function CalculateText(Text: UnicodeString; Rect: TRectF): TRectF; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0): Integer; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0): Integer; overload;
    function DrawImageWithName(ABitmapName: string; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
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
    procedure DrawSaveState;
    procedure DrawClear(Rect: TRectF); overload;
    procedure DrawSetTransform(m: TTMSFNCGraphicsMatrix); overload;
    procedure DrawSetTransform(m11, m12, m21, m22, dx, dy: Double); overload;
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
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  end;

var
  PDFGraphicsLibService: ITMSFNCPDFGraphicsLibService;

procedure RegisterPDFGraphicsLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibService, IInterface(PDFGraphicsLibService)) then
  begin
    PDFGraphicsLibService := TTMSFNCiOSPDFGraphicsLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFGraphicsLibService, PDFGraphicsLibService);
  end;
end;

procedure UnregisterPDFGraphicsLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFGraphicsLibService);
end;

{ TTMSFNCiOSPDFGraphicsLibService }

function TTMSFNCiOSPDFGraphicsLibService.DoCreatePDFGraphicsLib: TObject;
begin
  Result := TTMSFNCiOSPDFGraphicsLib.Create;
end;

function TTMSFNCiOSPDFGraphicsLib.DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF;
{$IFDEF IOS}
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  ft: UIFont;
  ftdesc: UIFontDescriptor;
  ftms: Cardinal;
{$ENDIF}
begin
  DrawSaveState;
  {$IFDEF IOS}
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in Font.Style then
    ftms := ftms or UIFontDescriptorTraitBold;

  if TFontStyle.fsItalic in Font.Style then
    ftms := ftms or UIFontDescriptorTraitItalic;

  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx(Font.Name), Font.Size));
  if not Assigned(ft.fontDescriptor) then
    ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx('Helvetica Neue'), Font.Size));
  ftdesc := ft.fontDescriptor.fontDescriptorWithSymbolicTraits(ftms);
  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithDescriptor(ftdesc, 0));
  dic.setValue((ft as ILocalObject).GetObjectID, NSSTREx('NSFont'));
  if TFontStyle.fsUnderline in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  dic.setValue((AlphaColorToUIColor(Font.Color) as ILocalObject).GetObjectID, NSSTREx('NSColor'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawAtPoint(CGPointMake(Point.X, Point.Y), dic);
  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(10000, 10000), 1 shl 0, dic, nil);
  dic.release;

  FTextRect := RectF(Point.X + r.origin.x, Point.Y + r.origin.y, Point.X + r.origin.x + r.size.width, Point.Y + r.origin.y + r.size.height);
  Result := FTextRect;
  {$ENDIF}
  DrawRestoreState;
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathBeginClip;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextClip(c);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextSetShadow(c, CGSizeMake(Offset.X, Offset.Y), Blur);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.AddGoto(AText, ADestination: UnicodeString;
  ARect: TRectF);
begin

end;

procedure TTMSFNCiOSPDFGraphicsLib.AddURL(AText, AURL: UnicodeString;
  ARect: TRectF);
var
{$IFDEF IOS}
  r: CGRect;
{$ENDIF}
  tr, trr: TRectF;
  fts: TTMSFNCPDFGraphicsLibFont;
begin
  fts := TTMSFNCPDFGraphicsLibFont.Create;
  try
    fts.Assign(Font);
    Font.Assign(URLFont);
    tr := DrawText(AText, ARect);
    case Alignment of
      gtaLeading: trr := RectF(ARect.Left, FPageHeight - ARect.Top - tr.Height, ARect.Left + tr.Width, FPageHeight - ARect.Top);
      gtaCenter: trr := RectF(ARect.Left + (ARect.Width - tr.Width) / 2, FPageHeight - ARect.Top - tr.Height, ARect.Left + (ARect.Width - tr.Width) / 2 + tr.Width, FPageHeight - ARect.Top);
      gtaTrailing: trr := RectF(ARect.Right - tr.Width, FPageHeight - ARect.Top - tr.Height, ARect.Right, FPageHeight - ARect.Top);
    end;
    {$IFDEF IOS}
    r.origin.x := trr.Left;
    r.origin.y := FPageHeight - trr.Top - trr.Height;
    r.size.width := trr.Width;
    r.size.height := trr.Height;
    UIGraphicsSetPDFContextURLForRect(TNSURL.OCClass.URLWithString(NSStrEx(AURL)), r);
    {$ENDIF}
    Font.Assign(fts);
  finally
    fts.Free;
  end;
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Rect: TRectF; Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, Rect, False, Scale, True);
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, PointF(0, 0), Scale, True);
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateText(
  Text: UnicodeString): TRectF;
begin
  Result := DrawText(Text, PointF(0, 0), True);
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateText(Text: UnicodeString;
  Rect: TRectF): TRectF;
begin
  Result := DrawText(Text, Rect, True);
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rects, Padding, True);
end;

function TTMSFNCiOSPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rect: TRectF; Columns: Integer; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rect, Columns, Padding, True);
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single;
  Color: TTMSFNCGraphicsColor);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextSetShadowWithColor(c, CGSizeMake(Offset.X, Offset.Y), Blur, AlphaColorToUIColor(Color).CGColor);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawClear(Rect: TRectF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextClearRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawEllipse(Rect: TRectF);
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

function TTMSFNCiOSPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Rect, BitmapContainer, Paging, AScale, Calculate);
  Result := FTextRect;
end;

function TTMSFNCiOSPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Point, BitmapContainer, AScale, Calculate);
  Result := FTextRect;
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathEndClip;
begin
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
{$IFDEF IOS}
var
  img: UIImage;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF IOS}
  img := TUIImage.Wrap(TUIImage.OCClass.imageWithContentsOfFile(NSSTREx(AFileName)));
  case ImageType of
    itPNG:
    begin
      dt := TNSData.Wrap(UIImagePNGRepresentation((img as ILocalObject).GetObjectID));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
    itJPG:
    begin
      dt := TNSData.Wrap(UIImageJPEGRepresentation((img as ILocalObject).GetObjectID, Quality));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
  end;

  img.drawAtPoint(CGPointMake(Point.X, Point.Y));

  Result := RectF(Point.X, Point.Y, img.size.width, img.size.height);
  {$ENDIF}
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
{$IFDEF IOS}
var
  img: UIImage;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF IOS}
  img := ImageFromBitmap(ABitmap);
  case ImageType of
    itPNG:
    begin
      dt := TNSData.Wrap(UIImagePNGRepresentation((img as ILocalObject).GetObjectID));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
    itJPG:
    begin
      dt := TNSData.Wrap(UIImageJPEGRepresentation((img as ILocalObject).GetObjectID, Quality));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
  end;

  img.drawAtPoint(CGPointMake(Point.X, Point.Y));
  Result := RectF(Point.X, Point.Y, img.size.width, img.size.height);
  {$ENDIF}
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
{$IFDEF IOS}
var
  img: UIImage;
  x, y, w, h: Single;
  rsrc, rdest: TRectF;
  dt: NSData;
{$ENDIF}
begin
  {$IFDEF IOS}
  img := ImageFromBitmap(ABitmap);

  case ImageType of
    itPNG:
    begin
      dt := TNSData.Wrap(UIImagePNGRepresentation((img as ILocalObject).GetObjectID));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
    itJPG:
    begin
      dt := TNSData.Wrap(UIImageJPEGRepresentation((img as ILocalObject).GetObjectID, Quality));
      img := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
    end;
  end;

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
  img.drawInRect(CGRectMake(rdest.Left, rdest.Top, rdest.Width, rdest.Height));
  Result := rdest;
  {$ENDIF}
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF;
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

function TTMSFNCiOSPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch, AspectRatio: Boolean;
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

function TTMSFNCiOSPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Point, ImageType, Quality);
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawLine(StartPoint, EndPoint: TPointF);
begin
  DrawPathBegin;
  DrawPathMoveToPoint(StartPoint);
  DrawPathAddLineToPoint(EndPoint);
  DrawPathEnd(dmPathStroke);
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathEndLinearGradient(StartPoint,
  EndPoint: TPointF);
{$IFDEF IOS}
var
  c: CGContextRef;
  gradient: CGGradientRef;
  colorSpace: CGColorSpaceRef;
  colorarr: array[0..1] of CGColorRef;
  colors: CFArrayRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
  begin
    colorSpace := CGColorSpaceCreateDeviceRGB;
    colorarr[0] := AlphaColorToUIColor(Fill.Color).CGColor;
    colorarr[1] := AlphaColorToUIColor(Fill.ColorTo).CGColor;

    colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
    gradient := CGGradientCreateWithColors(colorSpace, colors, nil);

    CGContextDrawLinearGradient(c, gradient, CGPointMake(StartPoint.X, StartPoint.Y),
      CGPointMake(EndPoint.X, EndPoint.Y), 0);

    CFRelease(colorSpace);
    CFRelease(gradient);
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddArc(CenterPoint: TPointF; Radius,
  StartAngle, EndAngle: Single; Clockwise: Boolean = False);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
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

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddArcToPoint(FirstPoint,
  SecondPoint: TPointF; Radius: Single);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddArcToPoint(c, FirstPoint.X, FirstPoint.Y, SecondPoint.X, SecondPoint.Y, Radius);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddCurveToPoint(c, FirstControlPoint.X, FirstControlPoint.Y, SecondControlPoint.X, SecondControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddEllipse(Rect: TRectF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddEllipseInRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
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

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddLineToPoint(Point: TPointF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddLineToPoint(c, Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddQuadCurveToPoint(ControlPoint,
  EndPoint: TPointF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddQuadCurveToPoint(c, ControlPoint.X, ControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathAddRectangle(Rect: TRectF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextAddRect(c, CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height));
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathBegin;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextBeginPath(c);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathClose;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextClosePath(c);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
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

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathMoveToPoint(Point: TPointF);
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextMoveToPoint(c, Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawPathEndRadialGradient(StartCenter,
  EndCenter: TPointF; StartRadius, EndRadius: Single);
{$IFDEF IOS}
var
  c: CGContextRef;
  gradient: CGGradientRef;
  colorSpace: CGColorSpaceRef;
  colorarr: array[0..1] of CGColorRef;
  colors: CFArrayRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
  begin
    colorSpace := CGColorSpaceCreateDeviceRGB;
    colorarr[0] := AlphaColorToUIColor(Fill.Color).CGColor;
    colorarr[1] := AlphaColorToUIColor(Fill.ColorTo).CGColor;

    colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
    gradient := CGGradientCreateWithColors(colorSpace, colors, nil);

    CGContextDrawRadialGradient(c, gradient, CGPointMake(StartCenter.X, StartCenter.Y),
      StartRadius, CGPointMake(EndCenter.X, EndCenter.Y), EndRadius, 0);

    CFRelease(colorSpace);
    CFRelease(gradient);
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawRectangle(Rect: TRectF);
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

procedure TTMSFNCiOSPDFGraphicsLib.DrawRestoreState;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextRestoreGState(c);
  {$ENDIF}
end;

function TTMSFNCiOSPDFGraphicsLib.DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rects, Padding, DetectOverflow);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rect, Columns, Padding, DetectOverflow);
end;

function TTMSFNCiOSPDFGraphicsLib.DrawRichText(Rect: TRectF;
  Calculate: Boolean): TRectF;
begin
  Result := RichText.Draw(Rect, Calculate);
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawRoundedRectangle(Rect: TRectF;
  Rounding: Single);
{$IFDEF IOS}
var
  bez: UIBezierPath;
  {$ENDIF}
begin
  {$IFDEF IOS}
  bez := TUIBezierPath.Wrap(TUIBezierPath.OCClass.bezierPathWithRoundedRect(CGRectMake(Rect.Left, Rect.Top,
    Rect.Width, Rect.Height), Rounding));
  bez.fill;
  bez.stroke;
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawSaveState;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    CGContextSaveGState(c);
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawSetTransform(m: TTMSFNCGraphicsMatrix);
begin
  DrawSetTransform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
end;

procedure TTMSFNCiOSPDFGraphicsLib.DrawSetTransform(m11, m12, m21, m22, dx,
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

function TTMSFNCiOSPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF;
{$IFDEF IOS}
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  ft: UIFont;
  ftdesc: UIFontDescriptor;
  ftms: Cardinal;
  par: NSMutableParagraphStyle;
{$ENDIF}
begin
  DrawSaveState;
  {$IFDEF IOS}
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in Font.Style then
    ftms := ftms or UIFontDescriptorTraitBold;

  if TFontStyle.fsItalic in Font.Style then
    ftms := ftms or UIFontDescriptorTraitItalic;

  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx(Font.Name), Font.Size));
  if not Assigned(ft.fontDescriptor) then
    ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx('Helvetica Neue'), Font.Size));

  ftdesc := ft.fontDescriptor.fontDescriptorWithSymbolicTraits(ftms);
  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithDescriptor(ftdesc, 0));
  dic.setValue((ft as ILocalObject).GetObjectID, NSSTREx('NSFont'));
  if TFontStyle.fsUnderline in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in Font.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  dic.setValue((AlphaColorToUIColor(Font.Color) as ILocalObject).GetObjectID, NSSTREx('NSColor'));
  par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
  case Alignment of
    gtaLeading: par.setAlignment(0);
    gtaCenter: par.setAlignment(1);
    gtaTrailing: par.setAlignment(2);
  end;
  par.setLineBreakMode(Integer(LineBreakMode));
  dic.setValue((par as ILocalObject).GetObjectID, NSSTREx('NSParagraphStyle'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawInRect(CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height), dic);

  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(Rect.Width, Rect.Height), 1 shl 0, dic, nil);
  par.release;
  dic.release;
  FTextRect := RectF(Rect.Left + r.origin.x, Rect.Top + r.origin.y, Rect.Left + r.origin.x + r.size.width, Rect.Top + r.origin.y + r.size.height);
  Result := FTextRect;
  {$ENDIF}
  DrawRestoreState;
end;

function TTMSFNCiOSPDFGraphicsLib.DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
{$IFDEF IOS}
var
  textStorage: NSTextStorage;
  layoutManager: NSLayoutManager;
  textContainer: NSTextContainer;
  charRange, glyphRange: NSRange;
  c: Integer;
  dic: NSMutableDictionary;
  ft: UIFont;
  ftms: Cardinal;
  ftdesc: UIFontDescriptor;
  par: NSMutableParagraphStyle;
  l: Integer;
  r: CGRect;
{$ENDIF}
begin
  DrawSaveState;
  Result := 0;
  if Length(Rects) > 0 then
  begin
    {$IFDEF IOS}
    dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
    ftms := 0;
    if TFontStyle.fsBold in Font.Style then
      ftms := ftms or UIFontDescriptorTraitBold;

    if TFontStyle.fsItalic in Font.Style then
      ftms := ftms or UIFontDescriptorTraitItalic;

    ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx(Font.Name), Font.Size));
    if not Assigned(ft.fontDescriptor) then
      ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx('Helvetica Neue'), Font.Size));

    ftdesc := ft.fontDescriptor.fontDescriptorWithSymbolicTraits(ftms);
    ft := TUIFont.Wrap(TUIFont.OCClass.fontWithDescriptor(ftdesc, 0));
    dic.setValue((ft as ILocalObject).GetObjectID, NSSTREx('NSFont'));
    if TFontStyle.fsUnderline in Font.Style then
      dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
    if TFontStyle.fsStrikeOut in Font.Style then
      dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
    dic.setValue((AlphaColorToUIColor(Font.Color) as ILocalObject).GetObjectID, NSSTREx('NSColor'));
    par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
    case Alignment of
      gtaLeading: par.setAlignment(0);
      gtaCenter: par.setAlignment(1);
      gtaTrailing: par.setAlignment(2);
    end;
    par.setLineBreakMode(Integer(LineBreakMode));
    dic.setValue((par as ILocalObject).GetObjectID, NSSTREx('NSParagraphStyle'));
    textStorage := TNSTextStorage.Wrap(TNSTextStorage.Wrap(TNSTextStorage.OCClass.alloc).initWithString(NSSTREx(Text)));
    layoutManager := TNSLayoutManager.Wrap(TNSLayoutManager.Wrap(TNSLayoutManager.OCClass.alloc).init);
    par.release;
    dic.release;

    for c := 0 to Length(Rects) - 1 do
    begin
      textContainer := TNSTextContainer.Wrap(TNSTextContainer.Wrap(TNSTextContainer.OCClass.alloc).
        initWithSize(CGSizeMake(Rects[c].Width, Rects[c].Height)));
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
  end;
  DrawRestoreState;
end;

function TTMSFNCiOSPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := DrawText(Text, arr, Padding, DetectOverflow);
end;

function TTMSFNCiOSPDFGraphicsLib.GetAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := Alignment;
end;

function TTMSFNCiOSPDFGraphicsLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := BitmapContainer;
end;

function TTMSFNCiOSPDFGraphicsLib.GetFill: TTMSFNCPDFGraphicsFill;
begin
  Result := Fill;
end;

function TTMSFNCiOSPDFGraphicsLib.GetFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := Font;
end;

function TTMSFNCiOSPDFGraphicsLib.GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
begin
  Result := LineBreakMode;
end;

function TTMSFNCiOSPDFGraphicsLib.GetOnNotifyNewPage: TNotifyEvent;
begin
  Result := FOnNotifyNewPage;
end;

function TTMSFNCiOSPDFGraphicsLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCiOSPDFGraphicsLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCiOSPDFGraphicsLib.GetStroke: TTMSFNCPDFGraphicsStroke;
begin
  Result := Stroke;
end;

function TTMSFNCiOSPDFGraphicsLib.GetTextRect: TRectF;
begin
  Result := FTextRect;
end;

function TTMSFNCiOSPDFGraphicsLib.GetURLFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := URLFont;
end;

procedure TTMSFNCiOSPDFGraphicsLib.InitializeAppearance;
begin
  UpdateFill;
  UpdateStroke;
end;

procedure TTMSFNCiOSPDFGraphicsLib.NotifyNewPage;
begin
  if Assigned(OnNotifyNewPage) then
    OnNotifyNewPage(Self);
end;

function TTMSFNCiOSPDFGraphicsLib.RichText: ITMSFNCCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  Alignment := Value;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  BitmapContainer := Value;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetCanvas(ACanvas: Pointer);
begin

end;

procedure TTMSFNCiOSPDFGraphicsLib.SetFill(const Value: TTMSFNCPDFGraphicsFill);
begin
  Fill.Assign(Value);
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
begin
  Font.Assign(Value)
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetLineBreakMode(
  const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
begin
  LineBreakMode := Value;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetOnNotifyNewPage(
  const Value: TNotifyEvent);
begin
  FOnNotifyNewPage := Value;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetPageHeight(APageHeight: Single);
begin
  FPageHeight := APageHeight;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetPageWidth(APageWidth: Single);
begin
  FPageWidth := FPageHeight;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetPDFLib(APDFLib: IInterface);
begin
  FPDFLib := APDFLib as ITMSFNCCustomPDFLib;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetPDFRichTextLib(
  const Value: ITMSFNCCustomPDFRichTextLib);
begin
  FPDFRichTextLib := Value;
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetStroke(
  const Value: TTMSFNCPDFGraphicsStroke);
begin
  Stroke.Assign(Value);
end;

procedure TTMSFNCiOSPDFGraphicsLib.SetURLFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  URLFont.Assign(Value);
end;

procedure TTMSFNCiOSPDFGraphicsLib.UpdateFill;
{$IFDEF IOS}
var
  c: CGContextRef;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
    AlphaColorToUIColor(Fill.Color).setFill;
  {$ENDIF}
end;

procedure TTMSFNCiOSPDFGraphicsLib.UpdateStroke;
{$IFDEF IOS}
var
  c: CGContextRef;
  f: array of CGFloat;
{$ENDIF}
begin
  {$IFDEF IOS}
  c := UIGraphicsGetCurrentContext;
  if Assigned(c) then
  begin
    AlphaColorToUIColor(Stroke.Color).setStroke;
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

procedure TTMSFNCiOSPDFGraphicsLib.UpdatePathRect(APoint: TPointF);
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

