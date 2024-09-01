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

unit FMX.TMSFNCPDFGraphicsLib.General;

{$I FMX.TMSFNCDefines.inc}

{$DEFINE SVGSUPPORT}

interface

procedure RegisterPDFGraphicsLibGeneralService;
procedure UnRegisterPDFGraphicsLibGeneralService;

implementation

uses
  Classes, FMX.TMSFNCPDFGraphicsLib, SysUtils, FMX.TMSFNCTypes, FMX.Graphics, FMX.TMSFNCPDFLib
  ,FMX.TMSFNCPDFRichTextLib, FMX.TMSFNCPDFCoreLibBase, FMX.TMSFNCPDFGraphicsLibHTMLEngine, FMX.TMSFNCUtils,
  FMX.TMSFNCBitmapContainer, FMX.TMSFNCGraphicsTypes
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  ,FMX.TMSFNCGraphicsPDFEngine
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,Types
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  ;

type
  TTMSFNCGeneralPDFGraphicsLibService = class(TTMSFNCPDFGraphicsLibFactoryService)
  protected
    function DoCreatePDFGraphicsLib: TObject; override;
  end;

  TTMSFNCGeneralPDFGraphicsLib = class(TTMSFNCPDFGraphicsLibBase, ITMSFNCCustomPDFGraphicsLib, ITMSFNCCustomPDFInitializationLib, ITMSFNCCustomPDFGraphicsExLib)
  private
    FPathRect: TRectF;
    FOutput: TTMSFNCPDFGraphicsLibOutputWriter;
    FTextRect: TRectF;
    FPDFRichTextLib: ITMSFNCCustomPDFRichTextLib;
    FPDFLib: ITMSFNCCustomPDFLib;
    FPageWidth: Single;
    FPageHeight: Single;
    FOnNotifyNewPage: TNotifyEvent;
    function GetOnNotifyNewPage: TNotifyEvent;
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
  protected
    procedure SetFill(const Value: TTMSFNCPDFGraphicsFill);
    procedure SetStroke(const Value: TTMSFNCPDFGraphicsStroke);
    procedure SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TTMSFNCPDFGraphicsLibFont);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetLineBreakMode(const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
    procedure CreateClasses; override;
    procedure InitializeAppearance; override;
    procedure UpdateFill; override;
    procedure UpdateStroke; override;
    procedure UpdateFont; override;
    procedure UpdatePathRect(APoint: TPointF);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure DrawAddShadow({%H-}Offset: TPointF; {%H-}Blur: Single); overload;
    procedure DrawAddShadow({%H-}Offset: TPointF; {%H-}Blur: Single; {%H-}Color: TTMSFNCGraphicsColor); overload;
    procedure NotifyNewPage;
    procedure DrawPathEndRadialGradient({%H-}StartCenter, {%H-}EndCenter: TPointF; {%H-}StartRadius, {%H-}EndRadius: Single);
    procedure SetPDFRichTextLib(const Value: ITMSFNCCustomPDFRichTextLib);
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawRoundedRectangle({%H-}Rect: TRectF; {%H-}Rounding: Single);
    procedure DrawEllipse({%H-}Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure AddGoTo(AText: Unicodestring; ADestination: UnicodeString; ARect: TRectF);
    procedure DrawSaveState;
    procedure DrawClear({%H-}Rect: TRectF); overload;
    procedure DrawSetTransform(m11, m12, m21, m22, dx, dy: Double); overload;
    procedure DrawSetTransform(m: TTMSFNCGraphicsMatrix); overload;
    procedure DrawRestoreState;
    procedure DrawPathBegin;
    procedure DrawPathBeginClip;
    procedure DrawPathEndClip;
    procedure DrawPathEndLinearGradient({%H-}StartPoint, {%H-}EndPoint: TPointF);
    procedure DrawPathMoveToPoint(Point: TPointF);
    procedure DrawPathAddLineToPoint(Point: TPointF);
    procedure DrawPathAddArc({%H-}CenterPoint: TPointF; {%H-}Radius: Single; {%H-}StartAngle, {%H-}EndAngle: Single; {%H-}Clockwise: Boolean = False);
    procedure DrawPathAddArcToPoint({%H-}FirstPoint, {%H-}SecondPoint: TPointF; {%H-}Radius: Single);
    procedure DrawPathAddRectangle({%H-}Rect: TRectF);
    procedure DrawPathAddEllipse({%H-}Rect: TRectF);
    procedure DrawPathAddCurveToPoint({%H-}FirstControlPoint, {%H-}SecondControlPoint, {%H-}EndPoint: TPointF);
    procedure DrawPathAddLines({%H-}Points: TTMSFNCPDFGraphicsLibPointArray);
    procedure DrawPathAddQuadCurveToPoint({%H-}ControlPoint: TPointF; {%H-}EndPoint: TPointF);
    procedure DrawPathClose;
    procedure DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
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
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function FindNextWord(AText: UnicodeString; var APos: Integer): UnicodeString;
    function MeasureTextWidth(AText: UnicodeString): Single;
    function GetCharacterCount(AText: UnicodeString; AWidth: single): Integer;
    function IsUnicodeText(AText: UnicodeString): Boolean;
    function RichText: ITMSFNCCustomPDFRichTextLib;
    function GetTextRect: TRectF;
    function GetFill: TTMSFNCPDFGraphicsFill;
    function GetStroke: TTMSFNCPDFGraphicsStroke;
    function GetAlignment: TTMSFNCGraphicsTextAlign;
    function GetFont: TTMSFNCPDFGraphicsLibFont;
    function GetURLFont: TTMSFNCPDFGraphicsLibFont;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    function GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  public
    destructor Destroy; override;
  end;

var
  PDFGraphicsLibService: ITMSFNCPDFGraphicsLibGeneralService;

procedure RegisterPDFGraphicsLibGeneralService;
begin
  if not TTMSFNCPDFLibPlatformServices.Current.SupportsPlatformService(ITMSFNCPDFGraphicsLibGeneralService, IInterface(PDFGraphicsLibService)) then
  begin
    PDFGraphicsLibService := TTMSFNCGeneralPDFGraphicsLibService.Create;
    TTMSFNCPDFLibPlatformServices.Current.AddPlatformService(ITMSFNCPDFGraphicsLibGeneralService, PDFGraphicsLibService);
  end;
end;

procedure UnregisterPDFGraphicsLibGeneralService;
begin
  TTMSFNCPDFLibPlatformServices.Current.RemovePlatformService(ITMSFNCPDFGraphicsLibGeneralService);
end;

{ TTMSFNCGeneralPDFGraphicsLibService }

function TTMSFNCGeneralPDFGraphicsLibService.DoCreatePDFGraphicsLib: TObject;
begin
  Result := TTMSFNCGeneralPDFGraphicsLib.Create;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF;
var
  pt: TPointF;
  tw: Single;
  th: Single;
  c: TTMSFNCGraphicsColor;
  lw: Single;
begin
  if (Pos(#13, Text) > 0) or (Pos(#10, Text) > 0) then
    Result := DrawText(Text, RectF(Point.X, Point.Y, Point.X + 10000, Point.Y + 10000), Calculate)
  else
  begin
    if Assigned(FOutput) then
    begin
      if IsUnicodeText(Text) then
        FOutput.NotifyUnicode(Text)
      else
        FOutput.NotifyText(Text);

      tw := MeasureTextWidth(Text);
      th := FOutput.FontSize;

      if not Calculate then
      begin
        pt.X := Point.X;
        pt.Y := FPageHeight - Point.Y - th;
        FOutput.BeginText;
        FOutput.WriteFont;
        FOutput.WriteFontColor;
        FOutput.MoveTextTo(pt);
        FOutput.AddText(Text);
        FOutput.EndText;
        c := Stroke.Color;
        lw := Stroke.Width;
        Stroke.Color := Font.Color;
        Stroke.Width := Font.Size * PDFULLWFACTOR;
        if TFontStyle.fsUnderline in Font.Style then
          DrawLine(PointF(Point.X, Point.Y + th * PDFLHFACTOR * PDFULFACTOR), PointF(Point.X + tw, Point.Y + th * PDFLHFACTOR * PDFULFACTOR));

        if TFontStyle.fsStrikeOut in Font.Style then
          DrawLine(PointF(Point.X, Point.Y + th * PDFLHFACTOR * PDFSTFACTOR), PointF(Point.X + tw, Point.Y + th * PDFLHFACTOR * PDFSTFACTOR));
        Stroke.Color := c;
        Stroke.Width := lw;
      end;

      FTextRect := RectF(Point.X, Point.Y, Point.X + tw, Point.Y + th * PDFLHFACTOR);
      Result := FTextRect;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathBeginClip;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('W' + PDFLB);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.AddURL(AText, AURL: UnicodeString;
  ARect: TRectF);
var
  tr, trr: TRectF;
  fts: TTMSFNCPDFGraphicsLibFont;
begin
  if Assigned(FOutput) then
  begin
    fts := TTMSFNCPDFGraphicsLibFont.Create;
    try
      fts.Assign(Font);
      Font.Assign(URLFont);
      tr := DrawText(AText, ARect);
      case Alignment of
        gtaLeading: trr := RectF(ARect.Left, FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Left + (tr.Right - tr.Left), FPageHeight - ARect.Top);
        gtaCenter: trr := RectF(ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2, FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), FPageHeight - ARect.Top);
        gtaTrailing: trr := RectF(ARect.Right - (tr.Right - tr.Left), FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Right, FPageHeight - ARect.Top);
      end;
      FOutput.NotifyURL(trr, AURL);
      Font.Assign(fts);
    finally
      fts.Free;
    end;
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.AddGoTo(AText, ADestination: UnicodeString;
  ARect: TRectF);
var
  tr, trr: TRectF;
  fts: TTMSFNCPDFGraphicsLibFont;
begin
  if Assigned(FOutput) then
  begin
    fts := TTMSFNCPDFGraphicsLibFont.Create;
    try
      fts.Assign(Font);
      tr := DrawText(AText, ARect);
      case Alignment of
        gtaLeading: trr := RectF(ARect.Left, FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Left + (tr.Right - tr.Left), FPageHeight - ARect.Top);
        gtaCenter: trr := RectF(ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2, FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), FPageHeight - ARect.Top);
        gtaTrailing: trr := RectF(ARect.Right - (tr.Right - tr.Left), FPageHeight - ARect.Top - (tr.Bottom - tr.Top), ARect.Right, FPageHeight - ARect.Top);
      end;
      FOutput.NotifyGoTo(trr, ADestination);
      Font.Assign(fts);
    finally
      fts.Free;
    end;
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Rect: TRectF; Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, Rect, False, Scale, True);
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, PointF(0, 0), Scale, True);
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateText(
  Text: UnicodeString): TRectF;
begin
  Result := DrawText(Text, PointF(0, 0), True);
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateText(Text: UnicodeString;
  Rect: TRectF): TRectF;
begin
  Result := DrawText(Text, Rect, True);
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rects, Padding, True);
end;

function TTMSFNCGeneralPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rect: TRectF; Columns: Integer; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rect, Columns, Padding, True);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.CreateClasses;
begin
  inherited;
end;

destructor TTMSFNCGeneralPDFGraphicsLib.Destroy;
begin
  FOutput := nil;
  inherited;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single;
  Color: TTMSFNCGraphicsColor);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawClear(Rect: TRectF);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawEllipse(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top) / 2));
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

function TTMSFNCGeneralPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Rect, BitmapContainer, Paging, AScale, Calculate);
  Result := FTextRect;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Point, BitmapContainer, AScale, Calculate);
  Result := FTextRect;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathEndClip;
begin
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  bmp := TTMSFNCBitmap.Create;
  try
    bmp.LoadFromFile(AFileName);
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
  finally
    bmp.Free;
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
var
  br: String;
  pt: TPointF;
  sz: TSizeF;
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  g: TTMSFNCGraphicsPDFEngine;
  {$ENDIF}
  {$ENDIF}
begin
  if Assigned(FOutput) and Assigned(ABitmap) then
  begin
    Result := RectF(Point.X, Point.Y, Point.X + ABitmap.Width, Point.Y + ABitmap.Height);
    {$IFDEF SVGSUPPORT}
    {$IFNDEF WEBLIB}
    {$IFDEF FMXLIB}
    if (ABitmap is TTMSFNCBitmap) and (ABitmap as TTMSFNCBitmap).HasSVG then
    {$ENDIF}
    {$IFDEF CMNLIB}
    if Assigned(ABitmap.Graphic) and (ABitmap.Graphic is TTMSFNCSVGBitmap) and (ABitmap.Graphic as TTMSFNCSVGBitmap).HasSVG then
    {$ENDIF}
    begin
      g := TTMSFNCGraphicsPDFEngine.Create(FPDFLib);
      try
        g.DrawBitmap(Result, ABitmap);
      finally
        g.Free;
      end;
      Exit;
    end;
    {$ENDIF}
    {$ENDIF}

    br := '';
    FOutput.NotifyBitmap(ABitmap, ImageType, Quality, ABackgroundColor, br);
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y - ABitmap.Height;
    sz.cx := ABitmap.Width;
    sz.cy := ABitmap.Height;
    DrawSaveState;
    FOutput.WriteMatrix(pt, sz);
    FOutput.WriteString(br + ' Do' + PDFLB);
    DrawRestoreState;
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
var
  br: String;
  pt: TPointF;
  sz: TSizeF;
  x, y, w, h: Single;
  rdest: TRectF;
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  g: TTMSFNCGraphicsPDFEngine;
  {$ENDIF}
  {$ENDIF}
begin
  if Assigned(FOutput) and Assigned(ABitmap) then
  begin
    br := '';
    x := 0;
    y := 0;
    w := 0;
    h := 0;
    GetAspectSize(x, y, w, h, ABitmap.Width, ABitmap.Height, (Rect.Right - Rect.Left), (Rect.Bottom - Rect.Top), AspectRatio, Stretch, False);

    if Center then
    begin
      x := Rect.Left + ((Rect.Right - Rect.Left) - w) / 2;
      y := Rect.Top + ((Rect.Bottom - Rect.Top) - h) / 2;
    end
    else
    begin
      x := Rect.Left;
      y := Rect.Top;
    end;

    rdest := RectF(x, y, x + w, y + h);
    Result := rdest;

    {$IFDEF SVGSUPPORT}
    {$IFNDEF WEBLIB}
    {$IFDEF FMXLIB}
    if (ABitmap is TTMSFNCBitmap) and (ABitmap as TTMSFNCBitmap).HasSVG then
    {$ENDIF}
    {$IFDEF CMNLIB}
    if Assigned(ABitmap.Graphic) and (ABitmap.Graphic is TTMSFNCSVGBitmap) and (ABitmap.Graphic as TTMSFNCSVGBitmap).HasSVG then
    {$ENDIF}
    begin
      g := TTMSFNCGraphicsPDFEngine.Create(FPDFLib);
      try
        g.DrawBitmap(Result, ABitmap);
      finally
        g.Free;
      end;
      Exit;
    end;
    {$ENDIF}
    {$ENDIF}

    FOutput.NotifyBitmap(ABitmap, ImageType, Quality, ABackgroundColor, br);
    pt.X := rdest.Left;
    pt.Y := FPageHeight - rdest.Top - (rdest.Bottom - rdest.Top);
    sz.cx := rdest.Right - rdest.Left;
    sz.cy := rdest.Bottom - rdest.Top;
    DrawSaveState;
    FOutput.WriteMatrix(pt, sz);
    FOutput.WriteString(br + ' Do' + PDFLB);
    DrawRestoreState;
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImage(ABitmap: TTMSFNCBitmapHelperClass;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Point, ImageType, Quality);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF;
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

function TTMSFNCGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TTMSFNCGraphicsColor; Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
var
  bmp: TTMSFNCBitmap;
begin
  if not Assigned(BitmapContainer) then
    Exit;

  bmp := TTMSFNCBitmap(BitmapContainer.FindBitmap(ABitmapName));
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Point: TPointF; ImageType: TTMSFNCPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Point, ImageType, Quality);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawLine(StartPoint, EndPoint: TPointF);
begin
  if Stroke.Kind <> gskNone then
  begin
    DrawPathBegin;
    DrawPathMoveToPoint(StartPoint);
    DrawPathAddLineToPoint(EndPoint);
//    DrawPathClose;
    DrawPathEnd(dmPathStroke);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathEndLinearGradient(StartPoint,
  EndPoint: TPointF);
begin
  if Assigned(FOutput) then
  begin
    FOutput.WriteString('f' + PDFLB);
    if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      FOutput.NotifyShadingRect(RectF(StartPoint.X, FPageHeight - EndPoint.Y, EndPoint.X, FPageHeight - StartPoint.Y));
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddArc(CenterPoint: TPointF; Radius,
  StartAngle, EndAngle: Single; Clockwise: Boolean = False);
begin
  if Assigned(FOutput) then
  begin
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddArcToPoint(FirstPoint,
  SecondPoint: TPointF; Radius: Single);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
var
  pt1, pt2, pt3: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt1.X := FirstControlPoint.X;
    pt1.Y := FPageHeight - FirstControlPoint.Y;
    pt2.X := SecondControlPoint.X;
    pt2.Y := FPageHeight - SecondControlPoint.Y;
    pt3.X := EndPoint.X;
    pt3.Y := FPageHeight - EndPoint.Y;
    FOutput.CurveTo(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddEllipse(Rect: TRectF);
var
  x, y, w2, h2, xw2, yh2, w, h: single;
  bz: Single;
begin
  if Assigned(FOutput) then
  begin
    w := Rect.Right - Rect.Left;
    h := Rect.Bottom - Rect.Top;

    x := Rect.Left;
    y := FPageHeight - Rect.Top - h;

    bz := 4/3 * (sqrt(2) - 1);
    w2 := w / 2;
    h2 := h / 2;
    xw2 := x + w2;
    yh2 := y + h2;

    UpdatePathRect(PointF(x, y));
    UpdatePathRect(PointF(x + w, y + h));

    FOutput.CurveTo(x, yh2 - h2 * bz, xw2 - w2 * bz, y, xw2, y);
    FOutput.CurveTo(xw2 + w2 * bz, y, x + w, yh2 - h2 * bz, x + w, yh2);
    FOutput.CurveTo(x + w, yh2 + h2 * bz, xw2 + w2 * bz, y + h, xw2, y + h);
    FOutput.CurveTo(xw2 - w2 * bz, y + h, x, yh2 + h2 * bz, x, yh2);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddLines(Points: TTMSFNCPDFGraphicsLibPointArray);
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

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddLineToPoint(Point: TPointF);
var
  pt: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y;
    UpdatePathRect(pt);
    FOutput.LineTo(pt);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddQuadCurveToPoint(ControlPoint,
  EndPoint: TPointF);
var
  pt1, pt2: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt1.X := ControlPoint.X;
    pt1.Y := FPageHeight - ControlPoint.Y;
    pt2.X := EndPoint.X;
    pt2.Y := FPageHeight - EndPoint.Y;
    FOutput.CurveTo2(pt1.X, pt1.Y, pt2.X, pt2.Y);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathAddRectangle(Rect: TRectF);
var
  x, y, w, h: Single;
begin
  if Assigned(FOutput) then
  begin
    w := Rect.Right - Rect.Left;
    h := Rect.Bottom - Rect.Top;

    x := Rect.Left;
    y := FPageHeight - Rect.Top - h;

    UpdatePathRect(PointF(x, y));
    UpdatePathRect(PointF(x + w, y + h));

    FOutput.WriteRectangle(RectF(x, y, x + w, y + h));
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathBegin;
var
  p: string;
begin
  if Assigned(FOutput) then
  begin
    FPathRect := RectF(-1, -1, -1, -1);
    FOutput.WriteString('n' + PDFLB);
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      FOutput.WriteStrokeColor(Stroke.Color);

    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    begin
      if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      begin
        p := '';
        FOutput.NotifyShading(Fill.Color, Fill.ColorTo, Fill.Orientation, p);
        FOutput.WriteString('/Pattern cs' + PDFLB);
        FOutput.WriteString(p + ' scn' + PDFLB);
      end
      else
        FOutput.WriteFillColor(Fill.Color);
    end;

    if Stroke.Width > 0 then
      FOutput.WriteStrokeWidth(Stroke.Width);

    FOutput.WriteStrokeKind(Stroke.Kind);

  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathClose;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('h' + PDFLB);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathEnd(DrawingMode: TTMSFNCPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
begin
  if Assigned(FOutput) then
  begin
    case DrawingMode of
      dmPathFill, dmPathEOFill:
      begin
        if DrawingMode = dmPathEOFill then
          DrawPathClose;
        FOutput.WriteString('f' + PDFLB);
      end;
      dmPathStroke, dmPathEOStroke:
      begin
        if DrawingMode = dmPathEOStroke then
          DrawPathClose;
        FOutput.WriteString('S' + PDFLB);
      end;
      dmPathFillStroke, dmPathEOFillStroke:
      begin
        if DrawingMode = dmPathEOFillStroke then
          DrawPathClose;
        FOutput.WriteString('B' + PDFLB);
      end;
    end;

    if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      FOutput.NotifyShadingRect(FPathRect);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathMoveToPoint(Point: TPointF);
var
  pt: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y;
    UpdatePathRect(pt);
    FOutput.MoveTo(pt);
  end;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawPathEndRadialGradient(StartCenter,
  EndCenter: TPointF; StartRadius, EndRadius: Single);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawRectangle(Rect: TRectF);
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

procedure TTMSFNCGeneralPDFGraphicsLib.DrawRestoreState;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('Q' + PDFLB);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawRichText(Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rects, Padding, DetectOverflow);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rect, Columns, Padding, DetectOverflow);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawRichText(Rect: TRectF;
  Calculate: Boolean): TRectF;
begin
  Result := RichText.Draw(Rect, Calculate);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawRoundedRectangle(Rect: TRectF;
  Rounding: Single);
begin
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawSaveState;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('q' + PDFLB);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawSetTransform(
  m: TTMSFNCGraphicsMatrix);
begin
  if Assigned(FOutput) then
    FOutput.WriteMatrix(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.DrawSetTransform(m11, m12, m21, m22, dx,
  dy: Double);
begin
  if Assigned(FOutput) then
    FOutput.WriteMatrix(m11, m12, m21, m22, dx, dy);
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF;
var
  i: Integer;
  s, sn, st: UnicodeString;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  p: Integer;
  tw: Single;
  th: Single;
  lcnt: Integer;
  rs: TRectF;
  c: TTMSFNCGraphicsColor;
  lw: Single;
begin
  rs := Rect;
  if Assigned(FOutput) then
  begin
    if IsUnicodeText(Text) then
      FOutput.NotifyUnicode(Text)
    else
      FOutput.NotifyText(Text);

    Rect.Top := FPageHeight - Rect.Top - FOutput.FontSize;
    Rect.Bottom := FPageHeight - Rect.Bottom - FOutput.FontSize;

    mw := 0;
    {$IFDEF ZEROSTRINGINDEX}
    i := 0;
    {$ENDIF}
    {$IFNDEF ZEROSTRINGINDEX}
    i := 1;
    {$ENDIF}
    lcnt := 0;
    tw := 0;
    s := FindNextWord(Text, i);
    w := MeasureTextWidth(s);
    th := FOutput.FontSize;

    mw := mw + w;
    if (Length(s) > 0) and (s[Length(s)] = ' ') then
      mw := mw + FOutput.FontWordSpacing;

    while i <= Length(Text) do
    begin
      l := Length(s);
      if (l >= 2) and (((s[l] = #10) and (s[l - 1] = #13)) or ((s[l] = #13) and (s[l - 1] = #10))) then
      begin
        s := Copy(s, 1, l - 2);
        f := True;
      end
      else if (l >= 1) and ((s[l] = #10) or (s[l] = #13)) then
      begin
        s := Copy(s, 1, l - 1);
        f := True;
      end
      else
        f := False;

      sn := FindNextWord(Text, i);
      w := MeasureTextWidth(sn);

      if (Rect.Left + mw + w > Rect.Right) or f then
      begin
        if s <> '' then
        begin
          p := GetCharacterCount(s, Rect.Right - Rect.Left);
          st := Copy(s, 1, p);

          Inc(lcnt);
          if mw > tw then
            tw := mw;

          if not Calculate then
          begin
            FOutput.BeginText;
            FOutput.WriteFont;
            FOutput.WriteFontColor;
            FOutput.WriteFontLeading;
            case Alignment of
              gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
              gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, Rect.Top));
              gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
            end;
            FOutput.AddText(st);
            FOutput.EndText;

            c := Stroke.Color;
            lw := Stroke.Width;
            Stroke.Color := Font.Color;
            Stroke.Width := Font.Size * PDFULLWFACTOR;
            if TFontStyle.fsUnderline in Font.Style then
            begin
              case Alignment of
                gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              end;
            end;

            if TFontStyle.fsStrikeOut in Font.Style then
            begin
              case Alignment of
                gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              end;
            end;
            Stroke.Color := c;
            Stroke.Width := lw;
          end;

          mw := 0;
        end;

        s := '';

        Rect.Top := Rect.Top - FOutput.FontSize * PDFLHFACTOR;
        if (Int(Rect.Top) < Int(Rect.Bottom + FOutput.FontSize * PDFLHFACTOR)) and not Calculate then
          Break;
      end;

      mw := mw + w;
      if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
        mw := mw + FOutput.FontWordSpacing;
      s := s + sn;
    end;

    if s <> '' then
    begin
      p := GetCharacterCount(s, Rect.Right - Rect.Left);
      while p > 0 do
      begin
        st := Copy(s, 1, p);
        Delete(s, 1, p);
        Inc(lcnt);
        if mw > tw then
          tw := mw;

        if not Calculate then
        begin
          FOutput.BeginText;
          FOutput.WriteFont;
          FOutput.WriteFontColor;
          FOutput.WriteFontLeading;
          case Alignment of
            gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
            gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, Rect.Top));
            gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
          end;
          FOutput.AddText(st);
          FOutput.EndText;
          c := Stroke.Color;
          lw := Stroke.Width;
          Stroke.Color := Font.Color;
          Stroke.Width := Font.Size * PDFULLWFACTOR;
          if TFontStyle.fsUnderline in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
            end;
          end;

          if TFontStyle.fsStrikeOut in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
            end;
          end;
          Stroke.Color := c;
          Stroke.Width := lw;
        end;

        p := GetCharacterCount(s, Rect.Right - Rect.Left);
        Rect.Top := Rect.Top - FOutput.FontSize * PDFLHFACTOR;
      end;
    end;

    FTextRect := RectF(rs.Left, rs.Top, rs.Left + tw, rs.Top + lcnt * FOutput.FontSize * PDFLHFACTOR);
    Result := FTextRect;
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.FindNextWord(AText: UnicodeString;
  var APos: Integer): UnicodeString;
var
  l: integer;
  i: integer;
  c: Integer;
begin
  Result := '';

  l := Length(AText);
  if APos > l then
    Exit;

  {$IFDEF ZEROSTRINGINDEX}
  c := 0;
  {$ELSE}
  c := 1;
  {$ENDIF}

  i := APos;
  while True do
  begin
    if ((i - 1 >= c) and (AText[i] = #10) and (AText[i - 1] = #13)) or ((i - 1 >= c) and (AText[i] = #13) and (AText[i - 1] = #10)) or (AText[i] = ' ') then
    begin
      if AText[i] = ' ' then
        Result := Copy(AText, APos, i - (APos - 1))
      else
        Result := Copy(AText, APos, i - APos);

      Break;
    end
    else if (AText[i] = #10) or (AText[i] = #13) or (AText[i] = ' ') then
    begin
      result := Copy(AText, APos, i - (APos - 1));
      Break;
    end
    else if i >= l then
    begin
      result := Copy(AText, APos, i - (APos - 1));
      Break;
    end
    else
      inc(i);
  end;

  APos := i + 1;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rects: TTMSFNCPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  K, i: Integer;
  s, sn, st: UnicodeString;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  p: Integer;
  Rect: TRectF;
  th: Single;
  c: TTMSFNCGraphicsColor;
  lw: Single;
  {$IFDEF WEBLIB}
  t: string;
  {$ELSE}
  t: UnicodeString;
  {$ENDIF}
begin
  Result := 0;
  if Assigned(FOutput) then
  begin
    if IsUnicodeText(Text) then
      FOutput.NotifyUnicode(Text)
    else
      FOutput.NotifyText(Text);

    for K := 0 to Length(Rects) - 1 do
    begin
      Rect := Rects[K];
      InflateRectEx(Rect, -Padding, 0);
      Rect.Top := FPageHeight - Rect.Top - FOutput.FontSize;
      Rect.Bottom := FPageHeight - Rect.Bottom - FOutput.FontSize;

      mw := 0;
      {$IFDEF ZEROSTRINGINDEX}
      i := 0;
      {$ENDIF}
      {$IFNDEF ZEROSTRINGINDEX}
      i := 1;
      {$ENDIF}
      s := FindNextWord(Text, i);
      t := Text;
      Delete(t, 1, i - 1);
      Text := t;
      w := MeasureTextWidth(s);
      th := FOutput.FontSize;
      mw := mw + w;
      if (Length(s) > 0) and (s[Length(s)] = ' ') then
        mw := mw + FOutput.FontWordSpacing;

      while Length(Text) > 0 do
      begin
        l := Length(s);
        if (l >= 2) and (((s[l] = #10) and (s[l - 1] = #13)) or ((s[l] = #13) and (s[l - 1] = #10))) then
        begin
          s := Copy(s, 1, l - 2);
          f := True;
        end
        else if (l >= 1) and (s[l] = #10) or (s[l] = #13) then
        begin
          s := Copy(s, 1, l - 1);
          f := True;
        end
        else
          f := False;

        {$IFDEF ZEROSTRINGINDEX}
        i := 0;
        {$ENDIF}
        {$IFNDEF ZEROSTRINGINDEX}
        i := 1;
        {$ENDIF}
        sn := FindNextWord(Text, i);
        t := Text;
        Delete(t, 1, i - 1);
        Text := t;
        w := MeasureTextWidth(sn);

        if (Rect.Left + mw + w > Rect.Right) or f then
        begin
          if s <> '' then
          begin
            p := GetCharacterCount(s, Rect.Right - Rect.Left);
            st := Copy(s, 1, p);

            if not DetectOverflow then
            begin
              FOutput.BeginText;
              FOutput.WriteFont;
              FOutput.WriteFontColor;
              FOutput.WriteFontLeading;
              case Alignment of
                gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
                gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, Rect.Top));
                gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
              end;
              FOutput.AddText(st);
              FOutput.EndText;

              c := Stroke.Color;
              lw := Stroke.Width;
              Stroke.Color := Font.Color;
              Stroke.Width := Font.Size * PDFULLWFACTOR;
              if TFontStyle.fsUnderline in Font.Style then
              begin
                case Alignment of
                  gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                  gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                  gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                end;
              end;

              if TFontStyle.fsStrikeOut in Font.Style then
              begin
                case Alignment of
                  gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                  gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                  gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                end;
              end;
              Stroke.Color := c;
              Stroke.Width := lw;
            end;

            mw := 0;
          end;

          s := '';

          Rect.Top := Rect.Top - FOutput.FontSize * PDFLHFACTOR;
          if Int(Rect.Top) < Int(Rect.Bottom + FOutput.FontSize * PDFLHFACTOR) then
          begin
            Text := sn + Text;
            Break;
          end;
        end;

        mw := mw + w;
        if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
          mw := mw + FOutput.FontWordSpacing;
        s := s + sn;
      end;

      if s <> '' then
      begin
        p := GetCharacterCount(s, Rect.Right - Rect.Left);
        st := Copy(s, 1, p);

        if not DetectOverflow then
        begin
          FOutput.BeginText;
          FOutput.WriteFont;
          FOutput.WriteFontColor;
          FOutput.WriteFontLeading;
          case Alignment of
            gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
            gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, Rect.Top));
            gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
          end;
          FOutput.AddText(st);
          FOutput.EndText;

          c := Stroke.Color;
          lw := Stroke.Width;
          Stroke.Color := Font.Color;
          Stroke.Width := Font.Size * PDFULLWFACTOR;
          if TFontStyle.fsUnderline in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
            end;
          end;

          if TFontStyle.fsStrikeOut in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Right - Rect.Left - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
            end;
          end;
          Stroke.Color := c;
          Stroke.Width := lw;
        end;
      end;
    end;

    Result := Length(Text);
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TTMSFNCPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + (Rect.Right - Rect.Left) / Columns * I, Rect.Top, Rect.Left + (Rect.Right - Rect.Left) / Columns * (I + 1), Rect.Bottom);

  Result := DrawText(Text, arr, Padding, DetectOverflow);
end;

function TTMSFNCGeneralPDFGraphicsLib.GetAlignment: TTMSFNCGraphicsTextAlign;
begin
  Result := Alignment;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := BitmapContainer;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetCharacterCount(AText: UnicodeString;
  AWidth: single): Integer;
var
  i: Integer;
  c: Char;
  w, tw: Single;
begin
  Result := 0;
  tw := 0;

  {$IFDEF ZEROSTRINGINDEX}
  for i := 0 to Length(AText) - 1 do
  {$ENDIF}
  {$IFNDEF ZEROSTRINGINDEX}
  for i := 1 to Length(AText) do
  {$ENDIF}
  begin
    c := AText[i];
    w := FOutput.GetFontCharWidth(AText, i) * FOutput.FontSize / 1000;

    if w > 0 then
      w := w + FOutput.FontCharSpacing
    else
      w := 0;

    if (c = ' ') and (FOutput.FontWordSpacing > 0) and (i <> Length(AText)) then
      w := w + FOutput.FontWordSpacing;

    tw := tw + w;
    if tw > AWidth then
      Break;

    Inc(Result);
  end;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetFill: TTMSFNCPDFGraphicsFill;
begin
  Result := Fill;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := Font;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetLineBreakMode: TTMSFNCPDFGraphicsLibLineBreakMode;
begin
  Result := LineBreakMode;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetOnNotifyNewPage: TNotifyEvent;
begin
  Result := FOnNotifyNewPage;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetStroke: TTMSFNCPDFGraphicsStroke;
begin
  Result := Stroke;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetTextRect: TRectF;
begin
  Result := FTextRect;
end;

function TTMSFNCGeneralPDFGraphicsLib.GetURLFont: TTMSFNCPDFGraphicsLibFont;
begin
  Result := URLFont;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.InitializeAppearance;
begin
end;

function TTMSFNCGeneralPDFGraphicsLib.IsUnicodeText(AText: UnicodeString): Boolean;
begin
  Result := False;
  if Assigned(FOutput) then
    Result := FOutput.IsUnicodeString(AText);
end;

function TTMSFNCGeneralPDFGraphicsLib.MeasureTextWidth(AText: UnicodeString): Single;
var
  i: integer;
  c: char;
  w: Single;
begin
  Result := 0;
  {$IFDEF ZEROSTRINGINDEX}
  for i := 0 to Length(AText) - 1 do
  {$ENDIF}
  {$IFNDEF ZEROSTRINGINDEX}
  for i := 1 to Length(AText) do
  {$ENDIF}
  begin
    if (AText[i] = #10) or (AText[i] = #13) then
      Continue;

    c := AText[i];
    w := FOutput.GetFontCharWidth(AText, i) * FOutput.FontSize / 1000;

    if w > 0 then
      w := w + FOutput.FontCharSpacing
    else
      w := 0;

    if (c = ' ') and (FOutput.FontWordSpacing > 0) and (i <> Length(AText)) then
      w := w + FOutput.FontWordSpacing;

    Result := Result + w;
  end;

  Result := Result - FOutput.FontCharSpacing;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.NotifyNewPage;
begin
  if Assigned(OnNotifyNewPage) then
    OnNotifyNewPage(Self);
end;

function TTMSFNCGeneralPDFGraphicsLib.RichText: ITMSFNCCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetAlignment(const Value: TTMSFNCGraphicsTextAlign);
begin
  Alignment := Value;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  BitmapContainer := Value;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetCanvas(ACanvas: Pointer);
begin
  FOutput := TTMSFNCPDFGraphicsLibOutputWriter(ACanvas);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetFill(
  const Value: TTMSFNCPDFGraphicsFill);
begin
  Fill.Assign(Value);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetFont(const Value: TTMSFNCPDFGraphicsLibFont);
begin
  Font.Assign(Value);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetLineBreakMode(
  const Value: TTMSFNCPDFGraphicsLibLineBreakMode);
begin
  LineBreakMode := Value;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetOnNotifyNewPage(
  const Value: TNotifyEvent);
begin
  FOnNotifyNewPage := Value;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetPageHeight(APageHeight: Single);
begin
  FPageHeight := APageHeight;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetPageWidth(APageWidth: Single);
begin
  FPageWidth := APageWidth;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetPDFLib(APDFLib: IInterface);
begin
  FPDFLib := APDFLib as ITMSFNCCustomPDFLib;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetPDFRichTextLib(
  const Value: ITMSFNCCustomPDFRichTextLib);
begin
  FPDFRichTextLib := Value;
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetStroke(
  const Value: TTMSFNCPDFGraphicsStroke);
begin
  Stroke.Assign(Value);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.SetURLFont(
  const Value: TTMSFNCPDFGraphicsLibFont);
begin
  URLFont.Assign(Value);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.UpdateFill;
begin
  inherited;

end;

procedure TTMSFNCGeneralPDFGraphicsLib.UpdateFont;
begin
  inherited;
  if Assigned(FOutput) and Assigned(FOutput.OnFontChanged) then
    FOutput.OnFontChanged(Self);
end;

procedure TTMSFNCGeneralPDFGraphicsLib.UpdatePathRect(APoint: TPointF);
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

procedure TTMSFNCGeneralPDFGraphicsLib.UpdateStroke;
begin
  inherited;

end;

end.


