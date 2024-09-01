{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
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

unit FMX.TMSFNCGraphics.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF MACOS}
{$IFNDEF IOS}

uses
  Classes, MacApi.ObjectiveC, MacApi.CoreFoundation, MacApi.Foundation, Macapi.CocoaTypes, MacApi.AppKit,
  MacApi.CoreGraphics, MacApi.CoreText, MacApi.ImageIO, FMX.TMSFNCGraphicsTypes, FMX.TMSFNCTypes, {%H-}Types, FMX.TMSFNCGraphics, FMX.Graphics,
  FMX.TMSFNCUtils
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TTMSFNCGraphicsContextMac = class(TTMSFNCGraphicsContext)
  private
    FSaveMatrix: TTMSFNCGraphicsMatrix;
    FScale: Single;
    FActivePath: TTMSFNCGraphicsPath;
    FMapping: Boolean;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FContextSize: TSizeF;
    FCGContext: CGContextRef;
    FBitmap: TBitmap;
    FBitmapData: TBitmapData;
  protected
    function GetNativeCanvas: Pointer; override;
    function GetFlipped(const R: TRectF): TRectF;
    function DrawTextInternal(Text: String; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
    procedure SaveContext;
    procedure RestoreContext;
    procedure DestroyResources;
    procedure ApplyFill;
    procedure ApplyStroke;
    procedure DrawLinearGradient(ARect: TRectF);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
  public
    constructor Create(const AGraphics: TTMSFNCGraphics); override;
    destructor Destroy; override;
    function GetFillColor: TTMSFNCGraphicsColor; override;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; override;
    function CreatePath: Pointer; override;
    function GetMatrix: TTMSFNCGraphicsMatrix; override;
    procedure Render; override;
    procedure PathOpen(APath: Pointer); override;
    procedure PathMoveTo({%H-}APath: Pointer; APoint: TPointF); override;
    procedure PathLineTo(APath: Pointer; APoint: TPointF); override;
    procedure PathClose(APath: Pointer); override;
    procedure ResetClip; override;
    procedure ResetTransform; override;
    procedure ScaleTransform(AX, AY: Single); override;
    procedure RotateTransform(AAngle: Single); override;
    procedure TranslateTransform(AX, AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TTMSFNCGraphicsTextQuality); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); override;
    procedure SetMatrix(AMatrix: TTMSFNCGraphicsMatrix); override;
    procedure SetScale(AScale: Single); override;
    procedure SetSize(AWidth, AHeight: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure BeginPrinting; override;
    procedure EndPrinting; override;
    procedure StartSpecialPen; override;
    procedure StopSpecialPen; override;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState); override;
    procedure SaveState({%H-}AState: TTMSFNCGraphicsSaveState); override;
    procedure SetFontSize(ASize: Integer); override;
    procedure SetFontColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetFontName(AName: string); override;
    procedure SetFont(AFont: TTMSFNCGraphicsFont); override;
    procedure SetFontStyles(AStyle: TFontStyles); override;
    procedure SetFill(AFill: TTMSFNCGraphicsFill); override;
    procedure SetFillKind(AKind: TTMSFNCGraphicsFillKind); override;
    procedure SetFillColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetStroke(AStroke: TTMSFNCGraphicsStroke); override;
    procedure SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind); override;
    procedure SetStrokeColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetStrokeWidth(AWidth: Single); override;
    procedure DrawLine({%H-}AStroke: TTMSFNCGraphicsStroke; AFromPoint: TPointF; AToPoint: TPointF; {%H-}AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); override;
    procedure DrawPolygon({%H-}AStroke: TTMSFNCGraphicsStroke; APolygon: TTMSFNCGraphicsPathPolygon); override;
    procedure FillPolygon({%H-}AFill: TTMSFNCGraphicsFill; APolygon: TTMSFNCGraphicsPathPolygon); override;
    procedure DrawPolyline({%H-}AStroke: TTMSFNCGraphicsStroke; APolyline: TTMSFNCGraphicsPathPolygon); override;
    procedure FillPolyline({%H-}AFill: TTMSFNCGraphicsFill; APolyline: TTMSFNCGraphicsPathPolygon); override;
    procedure FillArc({%H-}AFill: TTMSFNCGraphicsFill; {%H-}ACenter, {%H-}ARadius: TPointF; {%H-}AStartAngle, {%H-}ASweepAngle: Single); override;
    procedure DrawArc({%H-}AStroke: TTMSFNCGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); override;
    procedure FillRect({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRect({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillRoundRect({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRoundRect({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillEllipse({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawEllipse({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawBitmap(ABitmap: TTMSFNCDrawBitmap; {%H-}ASrcRect, ADstRect: TRectF; {%H-}AOpacity: Single); override;
    procedure ClipRect(ARect: TRectF); override;
    procedure ClipPath({%H-}APath: TTMSFNCGraphicsPath); override;
    procedure DrawFocusPath({%H-}AStroke: TTMSFNCGraphicsStroke; {%H-}APath: TTMSFNCGraphicsPath; {%H-}AColor: TTMSFNCGraphicsColor); override;
    procedure DrawFocusRectangle({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TTMSFNCGraphicsTextAlign; ATrimming: TTMSFNCGraphicsTextTrimming; {%H-}AAngle: Single); override;
    procedure DrawPath({%H-}AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
    procedure FillPath({%H-}AFill: TTMSFNCGraphicsFill; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
  end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF MACOS}
{$IFNDEF IOS}

uses
  SysUtils, Math;

type
  NSColorExClass = interface(NSColorClass)
  ['{2DB422CA-10C7-4AEC-B700-A733BC8D5C08}']
  end;
  NSColorEx = interface(NSColor)
  ['{67B0CA9F-50F0-46D4-8600-ED664F389470}']
    function CGColor: CGColorRef; cdecl;
  end;
  TNSColorEx = class(TOCGenericImport<NSColorExClass, NSColorEx>)  end;

  NSStringExClass = interface(NSStringClass)
  ['{E45DC84C-40C8-4F6E-8285-1147E2056364}']
  end;
  NSStringEx = interface(NSString)
  ['{62CF57D3-08BC-4C5B-8B8C-860A5F997CBC}']
    procedure drawAtPoint(aPoint: NSPoint; withAttributes: NSDictionary); cdecl;
    procedure drawInRect(aRect: NSRect; withAttributes: NSDictionary); cdecl;
    function boundingRectWithSize(size: NSSize; options: NSStringDrawingOptions; attributes: NSDictionary): NSRect; cdecl;
  end;
  TNSStringEx = class(TOCGenericImport<NSStringExClass, NSStringEx>)  end;

function NSStrEx(AString: String): NSString;
begin
  Result := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(PChar(AString), AString.Length));
end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextMac;
end;

function ConvertToNSColor(AColor: TTMSFNCGraphicsColor; AOpacity: Single): NSColor;
var
  R: CGFloat;
  G: CGFloat;
  B: CGFloat;
  A: CGFloat;
begin
  if AColor = TAlphaColorRec.Null then
  begin
    Result := TNSColor.Wrap(TNSColor.OCClass.clearColor).colorUsingColorSpace(TNSColorSpace.Wrap(TNSColorSpace.OCClass.deviceRGBColorSpace));
    Exit;
  end;

  A := AOpacity;
  R := TAlphaColorRec(AColor).R / 255;
  G := TAlphaColorRec(AColor).G / 255;
  B := TAlphaColorRec(AColor).B / 255;
  Result := TNSColor.Wrap(TNSColor.OCClass.colorWithDeviceRed(R, G, B, A)).colorUsingColorSpace(TNSColorSpace.Wrap(TNSColorSpace.OCClass.deviceRGBColorSpace));
end;

{ TTMSFNCGraphicsContextMac }

procedure TTMSFNCGraphicsContextMac.ApplyFill;
var
  c: NSColor;
begin
  if not Assigned(FCGContext) then
     Exit;

  case FFill.Kind of
    gfkNone:
    begin
      c := ConvertToNSColor(gcNull, FFill.Opacity);
      CGContextSetRGBFillColor(FCGContext, c.redComponent, c.greenComponent, c.blueComponent, c.alphaComponent);
    end;
    gfkSolid:
    begin
      c := ConvertToNSColor(FFill.Color, FFill.Opacity);
      CGContextSetRGBFillColor(FCGContext, c.redComponent, c.greenComponent, c.blueComponent, c.alphaComponent);
    end;
  end;
end;

procedure TTMSFNCGraphicsContextMac.ApplyStroke;
var
  f: array of CGFloat;
  c: NSColor;
begin
  if not Assigned(FCGContext) then
     Exit;

  CGContextSetLineWidth(FCGContext, FStroke.Width);
  c := ConvertToNSColor(FStroke.Color, FStroke.Opacity);
  CGContextSetRGBStrokeColor(FCGContext, c.redComponent, c.greenComponent, c.blueComponent, c.alphaComponent);

  case FStroke.Kind of
    gskSolid, gskNone: CGContextSetLineDash(FCGContext, 0, nil, 0);
    gskDash:
    begin
      SetLength(f, 2);
      f[0] := 3;
      f[1] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
    gskDot:
    begin
      SetLength(f, 2);
      f[0] := 1;
      f[1] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
    gskDashDot:
    begin
      SetLength(f, 4);
      f[0] := 3;
      f[1] := 1;
      f[2] := 1;
      f[3] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
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
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
  end;
end;

procedure TTMSFNCGraphicsContextMac.BeginPrinting;
begin
end;

procedure TTMSFNCGraphicsContextMac.EndPrinting;
begin
end;

procedure TTMSFNCGraphicsContextMac.BeginScene;
begin
  if not Assigned(FCGContext) then
    Exit;

  Canvas.BeginScene;
  Canvas.Clear(gcNull);
end;

function TTMSFNCGraphicsContextMac.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  Result := DrawTextInternal(AText, ARect, AWordWrapping, gtaLeading, True);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.ClipPath(APath: TTMSFNCGraphicsPath);
var
  pth: CGMutablePathRef;
begin
  if not Assigned(FCGContext) then
    Exit;

  pth := CGMutablePathRef(ConvertToPath(APath));
  try
    SaveContext;
    CGContextBeginPath(FCGContext);
    CGContextAddPath(FCGContext, pth);
    CGContextClip(FCGContext);
  finally
    CGPathRelease(pth);
  end;
end;

procedure TTMSFNCGraphicsContextMac.ClipRect(ARect: TRectF);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddRect(FCGContext, CGRectMake(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
  CGContextClip(FCGContext);
end;

constructor TTMSFNCGraphicsContextMac.Create(const AGraphics: TTMSFNCGraphics);
begin
  inherited;
  FSaveMatrix := MatrixIdentity;
  FScale := TTMSFNCUtils.GetDPIScale;
  FNeedsRendering := True;
  FContextSize.cx := 0;
  FContextSize.cy := 0;
  FFont := TTMSFNCGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  FFill := TTMSFNCGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
end;

function TTMSFNCGraphicsContextMac.CreatePath: Pointer;
begin
  Result := CGPathCreateMutable;
end;

destructor TTMSFNCGraphicsContextMac.Destroy;
begin
  Render;

  if Assigned(FFont) then
  begin
    FFont.Free;
    FFont := nil;
  end;

  if Assigned(FFill) then
  begin
    FFill.Free;
    FFill := nil;
  end;

  if Assigned(FStroke) then
  begin
    FStroke.Free;
    FStroke := nil;
  end;

  DestroyResources;
  inherited;
end;

procedure TTMSFNCGraphicsContextMac.DestroyResources;
begin
  if Assigned(FCGContext) then
  begin
    RestoreContext;
    CGContextRelease(FCGContext);
    FCGContext := nil;
  end;

  if Assigned(FBitmap) then
  begin
    if FMapping then
    begin
      FBitmap.UnMap(FBitmapData);
      FMapping := False;
    end;
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TTMSFNCGraphicsContextMac.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), 0);
  {$ELSE}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), False);
  {$IFEND}
  {$HINTS ON}
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  R: TRectF;
  img: CGImageRef;

  function CreateQImage: CGImageRef;
  var
    src: CGImageSourceRef;
    dt: CFDataRef;
    ms: TMemoryStream;
  begin
    ms := TMemoryStream.Create;
    try
      ABitmap.SaveToStream(ms);
      dt := CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, ms.Memory, ms.Size, kCFAllocatorNull);
      src := CGImageSourceCreateWithData(dt, nil);
      Result := CGImageSourceCreateImageAtIndex(src, 0, nil);
      CFRelease(src);
      CFRelease(dt);
    finally
      ms.Free;
    end;
  end;
begin
  if not Assigned(ABitmap) or not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextTranslateCTM(FCGContext, 0, FContextSize.Height);
  CGContextScaleCTM(FCGContext, 1.0, -1.0);
  R := GetFlipped(ADstRect);
  img := CreateQImage;
  try
    CGContextDrawImage(FCGContext, CGRectMake(R.Left, R.Top, R.Width, r.Height), img);
  finally
    CFRelease(img);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddEllipseInRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextMac.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if not Assigned(FCGContext) then
    Exit;

end;

procedure TTMSFNCGraphicsContextMac.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint, AToPoint: TPointF; AModifyPointModeFrom,
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, AFromPoint.X, AFromPoint.Y);
  CGContextAddLineToPoint(FCGContext, AToPoint.X, AToPoint.Y);
  CGContextStrokePath(FCGContext);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawLinearGradient(ARect: TRectF);
var
  cs: CGColorSpaceRef;
  colorarr: array of CGColorRef;
  positionsarr: array of CGFloat;
  colors, positions: Pointer;
  gradient: CGGradientRef;
  f0, f1: Boolean;
  I: Integer;
  o: Integer;
  a, sx, sy: single;
  c, pt0, pt1: CGPoint;
  st, ist: CGAffineTransform;
  ivs: CGPoint;
  rd: CGFloat;
  it: TTMSFNCGraphicsFillGradientItem;
begin
  if not Assigned(FCGContext) then
    Exit;

  case FFill.GradientMode of
    gfgmDefault:
    begin
      SetLength(positionsarr, 0);
      if (FFill.ColorTo <> gcnull) and (FFill.ColorMirror <> gcNull) then
      begin
        SetLength(colorarr, 4);
        colorarr[0] := TNSColorEx.Wrap((ConvertToNSColor(FFill.Color, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[1] := TNSColorEx.Wrap((ConvertToNSColor(FFill.ColorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[2] := TNSColorEx.Wrap((ConvertToNSColor(FFill.ColorMirror, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[3] := TNSColorEx.Wrap((ConvertToNSColor(FFill.ColorMirrorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
      end
      else if FFill.ColorTo <> gcNull then
      begin
        SetLength(colorarr, 2);
        colorarr[0] := TNSColorEx.Wrap((ConvertToNSColor(FFill.Color, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[1] := TNSColorEx.Wrap((ConvertToNSColor(FFill.ColorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
      end;
    end;
    gfgmCollection:
    begin
      f0 := False;
      f1 := False;
      for I := 0 to FFill.GradientItems.Count - 1 do
      begin
        it := FFill.GradientItems[I];
        if it.Position = 0.0 then
          f0 := True;

        if it.Position = 1.0 then
          f1 := True;
      end;

      SetLength(colorarr, FFill.GradientItems.Count);
      SetLength(positionsarr, FFill.GradientItems.Count);

      o := 0;
      if not f0 and (FFill.GradientItems.Count > 0) then
      begin
        SetLength(colorarr, Length(colorarr) + 1);
        colorarr[0] := TNSColorEx.Wrap((ConvertToNSColor(FFill.GradientItems[0].Color, FFill.GradientItems[0].Opacity) as ILocalObject).GetObjectID).CGColor;
        SetLength(positionsarr, Length(positionsarr) + 1);
        positionsarr[0] := 0;
        o := 1;
      end;

      for I := 0 to FFill.GradientItems.Count - 1 do
      begin
        it := FFill.GradientItems[I];
        colorarr[I + o] := TNSColorEx.Wrap((ConvertToNSColor(it.Color, it.Opacity) as ILocalObject).GetObjectID).CGColor;
        positionsarr[I + o] := it.Position;
      end;

      if not f1 and (FFill.GradientItems.Count > 0) then
      begin
        SetLength(colorarr, Length(colorarr) + 1);
        colorarr[Length(colorarr) - 1] := TNSColorEx.Wrap((ConvertToNSColor(FFill.GradientItems[FFill.GradientItems.Count - 1].Color,
          FFill.GradientItems[FFill.GradientItems.Count - 1].Opacity) as ILocalObject).GetObjectID).CGColor;
        SetLength(positionsarr, Length(positionsarr) + 1);
        positionsarr[Length(positionsarr) - 1] := 1;
      end;
    end;
  end;

  cs := CGColorSpaceCreateDeviceRGB;

  colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
  positions := nil;
  if Length(positionsarr) > 0 then
    positions := @positionsarr[0];

  gradient := CGGradientCreateWithColors(cs, colors, positions);

  case FFill.GradientType of
    gfgtLinear:
    begin
      case FFill.Orientation of
        gfoHorizontal: CGContextDrawLinearGradient(FCGContext, gradient, CGPointMake(ARect.Left, ARect.Top + ARect.Height / 2), CGPointMake(ARect.Right, ARect.Top + ARect.Height / 2), 0);
        gfoVertical: CGContextDrawLinearGradient(FCGContext, gradient, CGPointMake(ARect.Left + ARect.Width / 2, ARect.Top), CGPointMake(ARect.Left + ARect.Width / 2, ARect.Bottom), 0);
        gfoCustom:
        begin
          a := DegToRad(FFill.GradientAngle);
          c := CGPointMake(ARect.Left + ARect.Width / 2, ARect.Top + ARect.Height / 2);
          pt0 := CGPointMake(c.x - Cos(a) * ARect.Width / 2, c.y - Sin(a) * ARect.height / 2);
          pt1 := CGPointMake(c.x + Cos(a) * ARect.Width / 2, c.y + Sin(a) * ARect.height / 2);
          CGContextDrawLinearGradient(FCGContext, gradient, pt0, pt1, 0);
        end;
      end;
    end;
    gfgtRadial:
    begin
      sx := 1;
      sy := 1;
      if (ARect.Width > 0) and (ARect.Height > 0) then
      begin
        if ARect.Width > ARect.Height then
        begin
          sy := 1;
          sx := ARect.Width / ARect.Height;
        end
        else if ARect.Height > ARect.Width then
        begin
          sx := 1;
          sy := ARect.Height / ARect.Width;
        end;
      end;

      st := CGAffineTransformMakeScale(sx, sy);
      ist := CGAffineTransformInvert(st);
      ivs := CGPointMake(ist.a, ist.d);

      c := CGPointMake(FFill.GradientCenterPoint.X * ivs.x, FFill.GradientCenterPoint.Y * ivs.y);

      rd := (ARect.Width / 2) * ivs.x;
      if sy > sx then
        rd := (ARect.Height / 2) * ivs.y;

      CGContextScaleCTM(FCGContext, st.a, st.d);

      CGContextDrawRadialGradient(FCGContext, gradient, c, 0, c, rd, kCGGradientDrawsBeforeStartLocation);

      CGContextScaleCTM(FCGContext, ivs.x, ivs.y);
    end;
  end;
  CGColorSpaceRelease(cs);
  CGGradientRelease(gradient);
end;

procedure TTMSFNCGraphicsContextMac.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: CGMutablePathRef;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      if not Assigned(FCGContext) then
        Exit;

      pth := CGMutablePathRef(ConvertToPath(APath));
      try
        SaveContext;
        CGContextBeginPath(FCGContext);
        CGContextAddPath(FCGContext, pth);
        ApplyStroke;
        CGContextDrawPath(FCGContext, kCGPathStroke);
        RestoreContext;
      finally
        CGPathRelease(pth);
      end;
    end
    else
    begin
      FActivePath := APath;
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: DrawPolygon(AStroke, p);
        pdmPolyline: DrawPolyline(AStroke, p);
      end;
      FActivePath := nil;
    end;
  end;
end;

procedure TTMSFNCGraphicsContextMac.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddLines(FCGContext, PCGPoint(APolygon), Length(APolygon));
  CGContextClosePath(FCGContext);
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddLines(FCGContext, PCGPoint(APolyline), Length(APolyline));
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.DrawRect(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom), PointF(r.Right, r.Bottom));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right, r.Top), PointF(r.Right, r.Bottom));
end;

procedure TTMSFNCGraphicsContextMac.DrawRoundRect(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single;
  ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := ARect;
  rc := ARounding;

  pth := TTMSFNCGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    DrawPath(FStroke, pth);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextMac.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TTMSFNCGraphicsTextAlign;
  ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single);
var
  R: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  R := DrawTextInternal(AText, ARect, AWordWrapping, AHorizontalAlign, True);
  case AVerticalAlign of
    gtaCenter: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top + (ARect.Height - r.Height) / 2, ARect.Right, ARect.Top + (ARect.Height - r.Height) / 2 + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaLeading: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaTrailing: DrawTextInternal(AText, RectF(ARect.Left, ARect.Bottom - r.Height, ARect.Right, ARect.Bottom), AWordWrapping, AHorizontalAlign, False);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.EndScene;
begin
  if not Assigned(FCGContext) then
    Exit;

  Render;
  Canvas.EndScene;
end;

procedure TTMSFNCGraphicsContextMac.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), 0);
  {$ELSE}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), False);
  {$IFEND}
  {$HINTS ON}
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(RectF(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ACenter.X + ARadius.X, ACenter.Y + ARadius.Y));
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.FillChanged(Sender: TObject);
begin
  if not Assigned(FCGContext) then
     Exit;

  ApplyFill;
end;

procedure TTMSFNCGraphicsContextMac.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddEllipseInRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: CGMutablePathRef;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      SaveContext;
      pth := CGMutablePathRef(ConvertToPath(APath));
      CGContextBeginPath(FCGContext);
      CGContextAddPath(FCGContext, pth);
      case AFill.Kind of
        gfkSolid:
        begin
          ApplyFill;
          CGContextDrawPath(FCGContext, kCGPathFill);
        end;
        gfkGradient:
        begin
          CGContextClip(FCGContext);
          DrawLinearGradient(APath.GetBounds);
        end;
      end;
      RestoreContext;
    end
    else
    begin
      FActivePath := APath;
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: FillPolygon(AFill, p);
        pdmPolyline: FillPolyline(AFill, p);
      end;
      FActivePath := nil;
    end;
  end;
end;

procedure TTMSFNCGraphicsContextMac.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddLines(FCGContext, PCGPoint(APolygon), Length(APolygon));
  CGContextClosePath(FCGContext);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddLines(FCGContext, PCGPoint(APolyline), Length(APolyline));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.FillRoundRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := ARect;
  rc := ARounding;

  pth := TTMSFNCGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    FillPath(FFill, pth);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextMac.FontChanged(Sender: TObject);
begin
end;

function TTMSFNCGraphicsContextMac.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextMac.GetFlipped(const R: TRectF): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := FContextSize.Height - R.Bottom;
  Result.Right := R.Right;
  Result.Bottom := FContextSize.Height - R.Top;
end;

function TTMSFNCGraphicsContextMac.GetMatrix: TTMSFNCGraphicsMatrix;
begin
  Result := FSaveMatrix;
end;

function TTMSFNCGraphicsContextMac.GetNativeCanvas: Pointer;
begin
  Result := FCGContext;
end;

procedure TTMSFNCGraphicsContextMac.PathClose(APath: Pointer);
begin
  CGPathCloseSubpath(APath);
end;

procedure TTMSFNCGraphicsContextMac.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  CGPathAddLineToPoint(APath, nil, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextMac.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  CGPathMoveToPoint(APath, nil, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextMac.PathOpen(APath: Pointer);
begin
end;

procedure TTMSFNCGraphicsContextMac.Render;
begin
  if not FNeedsRendering then
    Exit;

  if Assigned(FCGContext) then
  begin
    if FMapping then
    begin
      FBitmap.Unmap(FBitmapData);
      FMapping := False;
    end;
    Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), RectF(0, 0, FContextSize.Width, FContextSize.Height), 1);
  end;

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextMac.ResetClip;
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  if AAngle <> 0 then
    RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.ResetTransform;
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextMac.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextMac.RotateTransform(AAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextRotateCTM(FCGContext, DegToRad(AAngle));
end;

function TTMSFNCGraphicsContextMac.DrawTextInternal(Text: string; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  par: NSMutableParagraphStyle;
  ft: NSFont;
  ftms: Cardinal;
begin
  TNSGraphicsContext.OCClass.saveGraphicsState;
  TNSGraphicsContext.OCClass.setCurrentContext(TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.
    graphicsContextWithGraphicsPort(FCGContext, True)));
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in FFont.Style then
    ftms := ftms or NSFontBoldTrait;
  if TFontStyle.fsItalic in FFont.Style then
    ftms := ftms or NSFontItalicTrait;
  ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx(FFont.Name), ftms, 5, FFont.Size);
  if ft = nil then
    ft := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager).fontWithFamily(NSStrEx('Helvetica Neue'), ftms, 5, FFont.Size);

  if Assigned(ft) then
    dic.setValue((ft as ILocalObject).GetObjectID, NSStrEx('NSFont'));

  dic.setValue((ConvertToNSColor(FFont.Color, 1) as ILocalObject).GetObjectID, NSStrEx('NSColor'));
  if TFontStyle.fsUnderline in FFont.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in FFont.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
  case AHorizontalAlignment of
    gtaLeading: par.setAlignment(0);
    gtaCenter: par.setAlignment(2);
    gtaTrailing: par.setAlignment(1);
  end;

  if AWordWrap then
    par.setLineBreakMode(NSLineBreakByWordWrapping);

  dic.setValue((par as ILocalObject).GetObjectID, NSStrEx('NSParagraphStyle'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawInRect(CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height), dic);

  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(Rect.Width, Rect.Height), NSStringDrawingUsesLineFragmentOrigin, dic);
  par.release;
  dic.release;
  Result := RectF(Rect.Left + r.origin.x, Rect.Top + r.origin.y, Rect.Left + r.origin.x + r.size.width, Rect.Top + r.origin.y + r.size.height);
  TNSGraphicsContext.OCClass.restoreGraphicsState;
end;

procedure TTMSFNCGraphicsContextMac.SaveContext;
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextSaveGState(FCGContext);
end;

procedure TTMSFNCGraphicsContextMac.RestoreContext;
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextRestoreGState(FCGContext);
end;

procedure TTMSFNCGraphicsContextMac.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
end;

procedure TTMSFNCGraphicsContextMac.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextScaleCTM(FCGContext, AX, AY);
end;

procedure TTMSFNCGraphicsContextMac.SetScale(AScale: Single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextMac.SetSize(AWidth, AHeight: Single);
var
  cs: CGColorSpaceRef;
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  FBitmap := TBitmap.Create(Round(AWidth * FScale), Round(AHeight * FScale));
  if FBitmap.Map(TMapAccess.Write, FBitmapData) then
  begin
    FMapping := True;
    cs := CGColorSpaceCreateDeviceRGB;
    FCGContext := CGBitmapContextCreate(FBitmapData.Data, FBitmapData.Width, FBitmapData.Height, 8, FBitmapData.Pitch, cs, kCGImageAlphaPremultipliedLast);
    CGColorSpaceRelease(cs);
    CGContextTranslateCTM(FCGContext, 0, FBitmapData.Height);
    CGContextScaleCTM(FCGContext, FScale, -FScale);
    SaveContext;
  end;
end;

procedure TTMSFNCGraphicsContextMac.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
end;

procedure TTMSFNCGraphicsContextMac.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
end;

procedure TTMSFNCGraphicsContextMac.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FCGContext) then
    Exit;

  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextSetAllowsAntialiasing(FCGContext, Integer(AAntiAliasing));
  {$ELSE}
  CGContextSetAllowsAntialiasing(FCGContext, AAntiAliasing);
  {$IFEND}
  {$HINTS ON}
end;

procedure TTMSFNCGraphicsContextMac.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextMac.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextMac.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextMac.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TTMSFNCGraphicsContextMac.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextMac.SetFontName(AName: string);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextMac.SetFontSize(ASize: Integer);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextMac.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextMac.SetMatrix(AMatrix: TTMSFNCGraphicsMatrix);
begin
  if not Assigned(FCGContext) then
    Exit;

  FSaveMatrix := AMatrix;

  if Assigned(FCGContext) then
    CGContextConcatCTM(FCGContext, CGAffineTransformMake(AMatrix.m11, AMatrix.m12, AMatrix.m21, AMatrix.m22, AMatrix.m31, AMatrix.m32));
end;

procedure TTMSFNCGraphicsContextMac.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextMac.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextMac.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextMac.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextMac.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  w, h, c, s, cw, ch: Single;
begin
  Result := ARect;
  if not Assigned(FCGContext) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := DegToRad(AAngle);
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;

    SaveContext;
    CGContextConcatCTM(FCGContext, CGAffineTransformRotate(CGAffineTransformMakeTranslation(cx.X, cx.Y), ar));

    w := Result.Width;
    h := Result.Height;
    c := Cos(ar);
    s := Sin(ar);

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextMac.StartSpecialPen;
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextMac.StopSpecialPen;
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextMac.StrokeChanged(Sender: TObject);
begin
  if not Assigned(FCGContext) then
     Exit;

  ApplyStroke;
end;

procedure TTMSFNCGraphicsContextMac.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextTranslateCTM(FCGContext, AX, AY);
end;

{$ENDIF}
{$ENDIF}

end.
