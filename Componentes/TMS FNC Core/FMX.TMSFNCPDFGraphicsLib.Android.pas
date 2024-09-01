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

unit FMX.TMSFNCPDFGraphicsLib.Android;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterPDFGraphicsLibService;
procedure UnRegisterPDFGraphicsLibService;

implementation

uses
  Classes, Types, Math, FMX.TMSFNCPDFGraphicsLib, SysUtils, FMX.TMSFNCTypes,
  FMX.TMSFNCBitmapContainer, FMX.TMSFNCUtils, FMX.TMSFNCPDFGraphicsLibHTMLEngine, FMX.TMSFNCPDFLib, FMX.TMSFNCGraphicsTypes
  {$IFDEF ANDROID}
  ,AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNIBridge, FMX.Helpers.Android, AndroidApi.JNI.JavaTypes, AndroidApi.Helpers
  {$ENDIF}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ,FMX.TMSFNCPDFRichTextLib, FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCGraphics
  ;

type
  TTMSFNCAndroidPDFGraphicsLibService = class(TTMSFNCPDFGraphicsLibFactoryService)
  protected
    function DoCreatePDFGraphicsLib: TObject; override;
  end;

  TTMSFNCAndroidPDFGraphicsLib = class(TTMSFNCPDFGraphicsLibBase, ITMSFNCCustomPDFGraphicsLib, ITMSFNCCustomPDFGraphicsExLib, ITMSFNCCustomPDFInitializationLib)
  private
    {$IFDEF ANDROID}
    FCanvas: JCanvas;
    FPath: JPath;
    {$ENDIF}
    FPathRect: TRectF;
    FPageHeight: Single;
    FPageWidth: Single;
    FTextRect: TRectF;
    FPDFLib: ITMSFNCCustomPDFLib;
    FPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
    FOnNotifyNewPage: TNotifyEvent;
    function GetOnNotifyNewPage: TNotifyEvent;
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
  protected
    procedure SetLineBreakMode(const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
    procedure SetPDFRichTextLib(const Value: ITMSFNCCustomPDFRichTextLib);
    procedure SetURLFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetStroke(const Value: TTMSFNCPDFGraphicsStroke);
    procedure SetFill(const Value: TTMSFNCPDFGraphicsFill);
    procedure SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure CreateClasses; override;
    procedure InitializeAppearance; override;
    procedure UpdateFill; override;
    procedure UpdateStroke; override;
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetCanvas(ACanvas: Pointer);
    procedure UpdatePathRect(APoint: TPointF);
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
    {$IFDEF ANDROID}
    function GetPath: JPath;
    {$ENDIF}
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
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    function RichText: ITMSFNCCustomPDFRichTextLib;
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
    destructor Destroy; override;
  end;

var
  PDFGraphicsLibService: ITMSFNCPDFGraphicsLibService;

procedure RegisterPDFGraphicsLibService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibService, IInterface(PDFGraphicsLibService)) then
  begin
    PDFGraphicsLibService := TTMSFNCAndroidPDFGraphicsLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFGraphicsLibService, PDFGraphicsLibService);
  end;
end;

procedure UnregisterPDFGraphicsLibService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFGraphicsLibService);
end;

{ TTMSFNCAndroidPDFGraphicsLibService }

function TTMSFNCAndroidPDFGraphicsLibService.DoCreatePDFGraphicsLib: TObject;
begin
  Result := TTMSFNCAndroidPDFGraphicsLib.Create;
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF;
{$IFDEF ANDROID}
var
  c: JCanvas;
  tp: JTextPaint;
  sl: JStaticLayout;
  al: JLayout_Alignment;
  resw, resh: Single;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
  begin
    tp := TJTextPaint.JavaClass.init;
    tp.setAntiAlias(True);
    FontToJTextPaint(Font, tp);

    al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;

    resw := tp.measureText(StringToJString(Text), 0, Length(Text));

    sl := TJStaticLayout.JavaClass.init(StrToJCharSequence(Text), tp, Round(resw), al, 1.0, 0.0, False);
    if not Calculate then
    begin
      c.save;
      c.translate(Point.X, Point.Y);
      sl.draw(c);
      c.restore;
    end;

    resh := sl.getHeight;

    FTextRect := RectF(Point.X, Point.Y, Point.X + resw, Point.Y + resh);
    Result := FTextRect;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathBeginClip;
{$IFDEF ANDROID}
var
  c: JCanvas;
  p: JPath;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF ANDROID}
  c := FCanvas;
  p := GetPath;
  if Assigned(c) then
    c.clipPath(p);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single);
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.AddGoto(AText,
  ADestination: UnicodeString; ARect: TRectF);
begin

end;

procedure TTMSFNCAndroidPDFGraphicsLib.AddURL(AText, AURL: UnicodeString;
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

function TTMSFNCAndroidPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Rect: TRectF; Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, Rect, False, Scale, True);
end;

function TTMSFNCAndroidPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, PointF(0, 0), Scale, True);
end;

function TTMSFNCAndroidPDFGraphicsLib.CalculateText(
  Text: UnicodeString): TRectF;
begin
  Result := DrawText(Text, PointF(0, 0), True);
end;

function TTMSFNCAndroidPDFGraphicsLib.CalculateText(Text: UnicodeString;
  Rect: TRectF): TRectF;
begin
  Result := DrawText(Text, Rect, True);
end;

function TTMSFNCAndroidPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rects, Padding, True);
end;

function TTMSFNCAndroidPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rect: TRectF; Columns: Integer; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rect, Columns, Padding, True);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.CreateClasses;
begin
  inherited;
end;

destructor TTMSFNCAndroidPDFGraphicsLib.Destroy;
begin
  {$IFDEF ANDROID}
  FCanvas := nil;
  FPath := nil;
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single;
  Color: TTMSFNCGraphicsColor);
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawClear(Rect: TRectF);
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawEllipse(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top + Rect.Height / 2));
  DrawPathAddEllipse(Rect);
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathFillStroke)
  else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    DrawPathEnd(dmPathFill)
  else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathStroke);

  DrawRestoreState;
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Rect, BitmapContainer, Paging, AScale, Calculate);
  Result := FTextRect;
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Point, BitmapContainer, AScale, Calculate);
  Result := FTextRect;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathEndClip;
begin
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
{$IFDEF ANDROID}
var
  bmp: JBitmap;
  opt: JBitmapFactory_Options;
  p: JPaint;
  o: JByteArrayOutputStream;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FCanvas) then
  begin
    opt := TJBitmapFactory_Options.JavaClass.init;
    bmp := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), opt);
    if Assigned(bmp) then
    begin
      case ImageType of
        itPNG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.PNG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
        itJPG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.JPEG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
      end;
      p := TJPaint.JavaClass.init;
      FCanvas.drawBitmap(bmp, Point.X, Point.Y, p);

      Result := RectF(Point.X, Point.Y, Point.X + bmp.getWidth, Point.Y + bmp.getHeight);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
{$IFDEF ANDROID}
var
  bmp: JBitmap;
  p: JPaint;
  o: JByteArrayOutputStream;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FCanvas) then
  begin
    bmp := ImageFromBitmap(ABitmap);
    if Assigned(bmp) then
    begin
      case ImageType of
        itPNG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.PNG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
        itJPG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.JPEG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
      end;
      p := TJPaint.JavaClass.init;
      FCanvas.drawBitmap(bmp, Point.X, Point.Y, p);

      Result := RectF(Point.X, Point.Y, Point.X + bmp.getWidth, Point.Y + bmp.getHeight);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor;
  Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
{$IFDEF ANDROID}
var
  bmp: JBitmap;
  p: JPaint;
  o: JByteArrayOutputStream;
  x, y, w, h: Single;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  if Assigned(FCanvas) then
  begin
    bmp := ImageFromBitmap(ABitmap);
    if Assigned(bmp) then
    begin
      case ImageType of
        itPNG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.PNG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
        itJPG:
        begin
          o := TJByteArrayOutputStream.JavaClass.init;
          bmp.compress(TJBitmap_CompressFormat.JavaClass.JPEG, Round(Quality * 100), o);
          bmp := TJBitmapFactory.JavaClass.decodeStream(TJByteArrayInputStream.JavaClass.init(o.toByteArray));
        end;
      end;
      p := TJPaint.JavaClass.init;
      GetAspectSize(x, y, w, h, bmp.getWidth, bmp.getHeight, Rect.Width, Rect.Height, AspectRatio, Stretch, False);

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

      FCanvas.drawBitmap(bmp, TJRect.JavaClass.init(0, 0, bmp.getWidth, bmp.getHeight), TJRectF.JavaClass.init(x, y, x + w, y + h), p);

      Result := RectF(x, y, x + w, y + h);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF;
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

function TTMSFNCAndroidPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor;
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

function TTMSFNCAndroidPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawLine(StartPoint, EndPoint: TPointF);
begin
  DrawPathBegin;
  DrawPathMoveToPoint(StartPoint);
  DrawPathAddLineToPoint(EndPoint);
  DrawPathEnd(dmPathStroke);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathEndLinearGradient(StartPoint,
  EndPoint: TPointF);
{$IFDEF ANDROID}
var
  c: JCanvas;
  pt: JPaint;
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  p := GetPath;
  if Assigned(c) and Assigned(p) then
  begin
    pt := TJPaint.JavaClass.init;
    pt.setShader(TJLinearGradient.JavaClass.init(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
    c.drawPath(p, pt);
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddArc(CenterPoint: TPointF; Radius,
  StartAngle, EndAngle: Single; Clockwise: Boolean = False);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
  begin
    if Clockwise then    
      p.addArc(TJRectF.JavaClass.init(CenterPoint.X - Radius, CenterPoint.Y - Radius, CenterPoint.X + Radius, CenterPoint.Y + Radius), StartAngle, EndAngle - StartAngle)
    else
      p.addArc(TJRectF.JavaClass.init(CenterPoint.X - Radius, CenterPoint.Y - Radius, CenterPoint.X + Radius, CenterPoint.Y + Radius), StartAngle, -(EndAngle - StartAngle))
  end;    
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddArcToPoint(FirstPoint,
  SecondPoint: TPointF; Radius: Single);
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.cubicTo(FirstControlPoint.X, FirstControlPoint.Y, SecondControlPoint.X, SecondControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddEllipse(Rect: TRectF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.addOval(TJRectF.JavaClass.init(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom), TJPath_Direction.JavaClass.CW);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
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

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddLineToPoint(Point: TPointF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.lineTo(Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddQuadCurveToPoint(ControlPoint,
  EndPoint: TPointF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.quadTo(ControlPoint.X, ControlPoint.Y, EndPoint.X, EndPoint.Y);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathAddRectangle(Rect: TRectF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  UpdatePathRect(PointF(Rect.Left, Rect.Top));
  UpdatePathRect(PointF(Rect.Right, Rect.Bottom));
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.addRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, TJPath_Direction.JavaClass.CW);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathBegin;
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  FPathRect := RectF(-1, -1, -1, -1);
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.reset;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathClose;
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.close;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
{$IFDEF ANDROID}
var
  c: JCanvas;
  pt: JPaint;
  p: JPath;
  ja: TJavaArray<Single>;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  p := GetPath;
  if Assigned(c) and Assigned(p) then
  begin
    case DrawingMode of
      dmPathFill, dmPathEOFill:
      begin
        if DrawingMode = dmPathEOFill then
          p.close;
        
        pt := TJPaint.JavaClass.init;
        if (Fill.Color <> gcNull) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
        begin
          case Fill.Orientation of
            gfoHorizontal: pt.setShader(TJLinearGradient.JavaClass.init(FPathRect.Left, FPathRect.Top, FPathRect.Right, FPathRect.Top, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
            gfoVertical: pt.setShader(TJLinearGradient.JavaClass.init(FPathRect.Left, FPathRect.Top, FPathRect.Left, FPathRect.Bottom, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
          end;
          c.drawPath(p, pt);
        end
        else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
        begin
          pt.setColor(AlphaColorToJColor(Fill.Color));
          pt.setStyle(TJPaint_Style.JavaClass.FILL);
          c.drawPath(p, pt);
        end;
      end;
      dmPathStroke, dmPathEOStroke:
      begin
        if DrawingMode = dmPathEOStroke then
          p.close;

        if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
        begin
          pt := TJPaint.JavaClass.init;
          pt.setColor(AlphaColorToJColor(Stroke.Color));
          pt.setStrokeWidth(Stroke.Width);
          case Stroke.Kind of
            gskDash:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 4;
              ja.Items[1] := 2;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDot:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 2;
              ja.Items[1] := 1;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDashDot:
            begin
              ja := TJavaArray<Single>.Create(4);
              ja.Items[0] := 4;
              ja.Items[1] := 2;
              ja.Items[2] := 1;
              ja.Items[3] := 2;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDashDotDot:
            begin
              ja := TJavaArray<Single>.Create(6);
              ja.Items[0] := 4;
              ja.Items[1] := 2;
              ja.Items[2] := 1;
              ja.Items[3] := 1;
              ja.Items[4] := 1;
              ja.Items[5] := 2;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
          end;
          pt.setStyle(TJPaint_Style.JavaClass.STROKE);
          c.drawPath(p, pt);
        end;
      end;
      dmPathFillStroke, dmPathEOFillStroke:
      begin
        if DrawingMode = dmPathEOFill then
          p.close;

        pt := TJPaint.JavaClass.init;
        if (Fill.Color <> gcNull) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
        begin
          case Fill.Orientation of
            gfoHorizontal: pt.setShader(TJLinearGradient.JavaClass.init(FPathRect.Left, FPathRect.Top, FPathRect.Right, FPathRect.Top, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
            gfoVertical: pt.setShader(TJLinearGradient.JavaClass.init(FPathRect.Left, FPathRect.Top, FPathRect.Left, FPathRect.Bottom, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
          end;
          c.drawPath(p, pt);
        end
        else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
        begin
          pt.setColor(AlphaColorToJColor(Fill.Color));
          pt.setStyle(TJPaint_Style.JavaClass.FILL);
          c.drawPath(p, pt);
        end;

        if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
        begin
          pt := TJPaint.JavaClass.init;
          pt.setColor(AlphaColorToJColor(Stroke.Color));
          pt.setStrokeWidth(Stroke.Width);
          case Stroke.Kind of
            gskDash:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 0;
              ja.Items[1] := 10;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDot:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 0;
              ja.Items[1] := 10;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDashDot:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 0;
              ja.Items[1] := 10;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
            gskDashDotDot:
            begin
              ja := TJavaArray<Single>.Create(2);
              ja.Items[0] := 0;
              ja.Items[1] := 10;
              pt.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
            end;
          end;
          pt.setStyle(TJPaint_Style.JavaClass.STROKE);
          c.drawPath(p, pt);
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathMoveToPoint(Point: TPointF);
{$IFDEF ANDROID}
var
  p: JPath;
{$ENDIF}
begin
  UpdatePathRect(Point);
  {$IFDEF ANDROID}
  p := GetPath;
  if Assigned(p) then
    p.moveTo(Point.X, Point.Y);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawPathEndRadialGradient(StartCenter,
  EndCenter: TPointF; StartRadius, EndRadius: Single);
{$IFDEF ANDROID}
var
  c: JCanvas;
  pt: JPaint;
  p: JPath;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  p := GetPath;
  if Assigned(c) and Assigned(p) then
  begin
    pt := TJPaint.JavaClass.init;
    pt.setShader(TJRadialGradient.JavaClass.init(StartCenter.X, StartCenter.Y, StartRadius, AlphaColorToJColor(Fill.Color), AlphaColorToJColor(Fill.ColorTo), TJShader_TileMode.JavaClass.MIRROR));
    c.drawPath(p, pt);
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawRectangle(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Bottom));
  DrawPathAddLineToPoint(PointF(Rect.Left, Rect.Bottom));
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathFillStroke)
  else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    DrawPathEnd(dmPathFill)
  else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathStroke);

  DrawRestoreState;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawRestoreState;
{$IFDEF ANDROID}
var
  c: JCanvas;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
    c.restore;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rects, Padding, DetectOverflow);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rect, Columns, Padding, DetectOverflow);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawRichText(Rect: TRectF;
  Calculate: Boolean): TRectF;
begin
  Result := RichText.Draw(Rect, Calculate);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawRoundedRectangle(Rect: TRectF;
  Rounding: Single);
{$IFDEF ANDROID}
var
  r: TRectF;
  c: JCanvas;
  p: JPaint;
  ja: TJavaArray<Single>;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
  begin
    r := Rect;    
    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    begin
      p := TJPaint.JavaClass.init;
      p.setStyle(TJPaint_Style.JavaClass.FILL);
      p.setColor(AlphaColorToJColor(Fill.Color));
      c.drawRoundRect(TJRectF.JavaClass.init(r.Left, r.Top, r.Right, r.Bottom), Rounding, Rounding, p);
    end;

    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    begin
      p := TJPaint.JavaClass.init;
      p.setStyle(TJPaint_Style.JavaClass.STROKE);
      case Stroke.Kind of
        gskDash:
        begin
          ja := TJavaArray<Single>.Create(2);
          ja.Items[0] := 0;
          ja.Items[1] := 10;
          p.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
        end;
        gskDot:
        begin
          ja := TJavaArray<Single>.Create(2);
          ja.Items[0] := 0;
          ja.Items[1] := 10;
          p.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
        end;
        gskDashDot:
        begin
          ja := TJavaArray<Single>.Create(2);
          ja.Items[0] := 0;
          ja.Items[1] := 10;
          p.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
        end;
        gskDashDotDot:
        begin
          ja := TJavaArray<Single>.Create(2);
          ja.Items[0] := 0;
          ja.Items[1] := 10;
          p.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
        end;
      end;
      p.setColor(AlphaColorToJColor(Stroke.Color));
      p.setStrokeWidth(Stroke.Width);
      c.drawRoundRect(TJRectF.JavaClass.init(r.Left, r.Top, r.Right, r.Bottom), Rounding, Rounding, p);
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawSaveState;
{$IFDEF ANDROID}
var
  c: JCanvas;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
    c.save;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawSetTransform(m11, m12, m21, m22, dx,
  dy: Double);
{$IFDEF ANDROID}
var
  c: JCanvas;
  m: JMatrix;
  a: TJavaArray<Single>;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
  begin
    m := TJMatrix.JavaClass.init;
    a := TJavaArray<Single>.Create(9);
    a[TJMatrix.JavaClass.MSCALE_X] := m11;
    a[TJMatrix.JavaClass.MSKEW_X] := m21;
    a[TJMatrix.JavaClass.MTRANS_X] := dx;
    a[TJMatrix.JavaClass.MSKEW_Y] := m12;
    a[TJMatrix.JavaClass.MSCALE_Y] := m22;
    a[TJMatrix.JavaClass.MTRANS_Y] := dy;
    a[TJMatrix.JavaClass.MPERSP_0] := 0;
    a[TJMatrix.JavaClass.MPERSP_1] := 0;
    a[TJMatrix.JavaClass.MPERSP_2] := 1;

    m.setValues(a);
    c.setMatrix(m);
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.DrawSetTransform(
  m: TTMSFNCGraphicsMatrix);
begin
  DrawSetTransform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF;
{$IFDEF ANDROID}
var
  c: JCanvas;
  tp: JTextPaint;
  sl: JStaticLayout;
  al: JLayout_Alignment;
  resw, resh: Single;
  mw: TJavaArray<Single>;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  c := FCanvas;
  if Assigned(c) then
  begin
    tp := TJTextPaint.JavaClass.init;
    tp.setAntiAlias(True);
    FontToJTextPaint(Font, tp);

    case Alignment of
      gtaLeading: al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;
      gtaCenter: al := TJLayout_Alignment.JavaClass.ALIGN_CENTER;
      gtaTrailing: al := TJLayout_Alignment.JavaClass.ALIGN_OPPOSITE;
    end;

    sl := TJStaticLayout.JavaClass.init(StrToJCharSequence(Text), tp, Round(Rect.Width), al, 1.0, 0.0, False);
    if not Calculate then
    begin
      c.save;
      c.clipRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      c.translate(Rect.Left, Rect.Top);
      sl.draw(c);
      c.restore;
    end;

    mw := TJavaArray<Single>.Create(1);
    tp.breakText(StringToJString(Text), True, Rect.Width, mw);
    resw := mw[0];
    resh := sl.getHeight;
    FTextRect := RectF(Rect.Left, Rect.Top, Rect.Left + resw, Rect.Top + resh);
    Result := FTextRect;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
{$IFDEF ANDROID}
var
  I: Integer;
  st: JStaticLayout;
  c: JCanvas;
  spstr: JCharSequence;
  tp: JTextPaint;
  al: JLayout_Alignment;
  r: TRectF;
  p: Integer;
  stb: JStaticLayout_Builder;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF ANDROID}
  if Length(Rects) > 0 then
  begin
    c := FCanvas;
    spstr := StrToJCharSequence(Text);
    if Assigned(c) and Assigned(spstr) then
    begin
      Result := Length(Text);

      tp := TJTextPaint.JavaClass.init;
      tp.setAntiAlias(True);
      FontToJTextPaint(Font, tp);

      case Alignment of
        gtaLeading: al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;
        gtaCenter: al := TJLayout_Alignment.JavaClass.ALIGN_CENTER;
        gtaTrailing: al := TJLayout_Alignment.JavaClass.ALIGN_OPPOSITE;
      end;

      for I := 0 to Length(Rects) - 1 do
      begin
        r := Rects[I];
        InflateRectEx(r, -Padding, 0);

        stb := TJStaticLayout_Builder.JavaClass.obtain(spstr, 0, spstr.length, tp, Round(r.Width));
        stb := stb.setIncludePad(False);
        stb := stb.setAlignment(al);
        stb := stb.setLineSpacing(0.0, 1.0);
        st := stb.build;

        if st.getHeight > r.Height then
        begin
          p := 0;
          while st.getHeight > r.Height do
          begin
            p := JCharSequenceToStr(spstr).LastIndexOf(' ');

            spstr := spstr.subSequence(0, p);

            stb := TJStaticLayout_Builder.JavaClass.obtain(spstr, 0, spstr.length, tp, Round(r.Width));
            stb := stb.setIncludePad(False);
            stb := stb.setAlignment(al);
            stb := stb.setLineSpacing(0.0, 1.0);
            st := stb.build;
          end;

          Result := Result - spstr.length;

          spstr := StrToJCharSequence(Text).subSequence(p + 1, Length(Text));
        end
        else
        begin
          spstr := StrToJCharSequence(Text);
          Result := Result - spstr.length;
        end;

        if not DetectOverflow then
        begin
          c.save;
          c.translate(r.Left, r.Top);
          st.draw(c);
          c.restore;
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := DrawText(Text, arr, Padding, DetectOverflow);
end;

function TTMSFNCAndroidPDFGraphicsLib.GetAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := Alignment;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := BitmapContainer;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetFill: TTMSFNCPDFGraphicsFill;
begin
  Result := Fill;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := Font;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
begin
  Result := LineBreakMode;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetOnNotifyNewPage: TNotifyEvent;
begin
  Result := FOnNotifyNewPage;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

{$IFDEF ANDROID}
function TTMSFNCAndroidPDFGraphicsLib.GetPath: JPath;
begin
  if not Assigned(FPath) then
    FPath := TJPath.JavaClass.init;

  Result := FPath;
end;
{$ENDIF}

function TTMSFNCAndroidPDFGraphicsLib.GetStroke: TTMSFNCPDFGraphicsStroke;
begin
  Result := Stroke;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetTextRect: TRectF;
begin
  Result := FTextRect;
end;

function TTMSFNCAndroidPDFGraphicsLib.GetURLFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := URLFont;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.InitializeAppearance;
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.NotifyNewPage;
begin
  if Assigned(OnNotifyNewPage) then
    OnNotifyNewPage(Self);
end;

function TTMSFNCAndroidPDFGraphicsLib.RichText: ITMSFNCCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  Alignment := Value;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  BitmapContainer := Value;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetCanvas(ACanvas: Pointer);
begin
  {$IFDEF ANDROID}
  FCanvas := TJCanvas.Wrap(ACanvas);
  if Assigned(FPDFRichTextLib) then
    FPDFRichTextLib.SetCanvas(ACanvas);
  {$ENDIF}
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetFill(
  const Value: TTMSFNCPDFGraphicsFill);
begin
  Fill.Assign(Value);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
begin
  Font.Assign(Value)
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetLineBreakMode(
  const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
begin
  LineBreakMode := Value;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetOnNotifyNewPage(
  const Value: TNotifyEvent);
begin
  FOnNotifyNewPage := Value;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetPageHeight(APageHeight: Single);
begin
  FPageHeight := APageHeight;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetPageWidth(APageWidth: Single);
begin

end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetPDFLib(APDFLib: IInterface);
begin
  FPDFLib := APDFLib as ITMSFNCCustomPDFLib;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetPDFRichTextLib(
  const Value: ITMSFNCCustomPDFRichTextLib);
begin
  FPDFRichTextLib := Value;
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetStroke(
  const Value: TTMSFNCPDFGraphicsStroke);
begin
  Stroke.Assign(Value);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.SetURLFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  URLFont.Assign(Value);
end;

procedure TTMSFNCAndroidPDFGraphicsLib.UpdateFill;
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.UpdateStroke;
begin
end;

procedure TTMSFNCAndroidPDFGraphicsLib.UpdatePathRect(APoint: TPointF);
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

