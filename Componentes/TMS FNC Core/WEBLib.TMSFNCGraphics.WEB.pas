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

unit WEBLib.TMSFNCGraphics.WEB;

{$I WEBLib.TMSFNCDefines.inc}

interface

{$IFDEF WEBLIB}

uses
  Classes, WEBLib.TMSFNCGraphicsTypes, WEBLib.TMSFNCUtils, {%H-}Types, WEBLib.TMSFNCTypes,
  WEBLib.TMSFNCGraphics, WEBLib.Graphics;

type
  TTMSFNCGraphicsContextWEB = class(TTMSFNCGraphicsContext)
  private
    FActivePath: TTMSFNCGraphicsPath;
    FScale: Single;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FContextSize: TSizeF;
    FNCanvas: Pointer;
    FBitmap: TBitmap;
  protected
    function GetNativeCanvas: Pointer; override;
    procedure DestroyResources;
    function DrawTextInternal(AText: String; ARect: TRectF; {%H-}AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; ACalculate: Boolean): TRectF;
    procedure DrawLinearGradient(ARect: TRectF);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure RestoreContext;
    procedure SaveContext;
    procedure ApplyStroke;
    procedure ApplyFill;
  public
    constructor Create(const AGraphics: TTMSFNCGraphics); override;
    destructor Destroy; override;
    function GetFillColor: TTMSFNCGraphicsColor; override;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; override;
    function CreatePath: Pointer; override;
    function GetMatrix: TTMSFNCGraphicsMatrix; override;
    procedure Render; override;
    procedure PathOpen({%H-}APath: Pointer); override;
    procedure PathMoveTo({%H-}APath: Pointer; APoint: TPointF); override;
    procedure PathLineTo({%H-}APath: Pointer; APoint: TPointF); override;
    procedure PathClose({%H-}APath: Pointer); override;
    procedure ResetClip; override;
    procedure ResetTransform; override;
    procedure ScaleTransform(AX, AY: Single); override;
    procedure RotateTransform(AAngle: Single); override;
    procedure TranslateTransform(AX, AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TTMSFNCGraphicsTextQuality); override;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetSize(AWidth, AHeight: Single); override;
    procedure SetMatrix(AMatrix: TTMSFNCGraphicsMatrix); override;
    procedure SetScale(AScale: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure BeginPrinting; override;
    procedure EndPrinting; override;
    procedure StartSpecialPen; override;
    procedure StopSpecialPen; override;
    procedure RestoreState({%H-}AState: TTMSFNCGraphicsSaveState); override;
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
    procedure DrawFocusRectangle({%H-}AStroke: TTMSFNCGraphicsStroke; {%H-}ARect: TRectF; {%H-}AColor: TTMSFNCGraphicsColor; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TTMSFNCGraphicsTextAlign; {%H-}ATrimming: TTMSFNCGraphicsTextTrimming; {%H-}AAngle: Single); override;
    procedure DrawPath({%H-}AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
    procedure FillPath({%H-}AFill: TTMSFNCGraphicsFill; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
  end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;

{$ENDIF}

implementation

{$IFDEF WEBLIB}

uses
  SysUtils, Math;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextWEB;
end;

{ TTMSFNCGraphicsContextWEB }

procedure TTMSFNCGraphicsContextWEB.ApplyFill;
begin
  if not Assigned(FNCanvas) then
     Exit;

  case FFill.Kind of
    gfkSolid:
    begin
//      cairo_set_source_rgba(FNCanvas, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255,
//        TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
    end;
  end;
end;

function TTMSFNCGraphicsContextWEB.GetMatrix: TTMSFNCGraphicsMatrix;
begin
  Result := MatrixEmpty;
end;

procedure TTMSFNCGraphicsContextWEB.SetScale(AScale: single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextWEB.SetMatrix(AMatrix: TTMSFNCGraphicsMatrix);
begin
end;

procedure TTMSFNCGraphicsContextWEB.ApplyStroke;
//var
//  f: array of Double;
begin
  if not Assigned(FNCanvas) then
     Exit;

//  cairo_set_line_width(FNCanvas, FStroke.Width);
//  cairo_set_source_rgba(FNCanvas, TTMSFNCGraphics.GetColorRed(FStroke.Color) / 255, TTMSFNCGraphics.GetColorGreen(FStroke.Color) / 255,
//    TTMSFNCGraphics.GetColorBlue(FStroke.Color) / 255, FStroke.Opacity);

//  case FStroke.Kind of
//    gskSolid, gskNone: cairo_set_dash(FNCanvas, nil,0, 0);
//    gskDash:
//    begin
//      SetLength(f, 2);
//      f[0] := 3;
//      f[1] := 1;
//      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
//    end;
//    gskDot:
//    begin
//      SetLength(f, 2);
//      f[0] := 1;
//      f[1] := 1;
//      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
//    end;
//    gskDashDot:
//    begin
//      SetLength(f, 4);
//      f[0] := 3;
//      f[1] := 1;
//      f[2] := 1;
//      f[3] := 1;
//      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
//    end;
//    gskDashDotDot:
//    begin
//      SetLength(f, 6);
//      f[0] := 3;
//      f[1] := 1;
//      f[2] := 1;
//      f[3] := 1;
//      f[4] := 1;
//      f[5] := 1;
//      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
//    end;
//  end;
end;

procedure TTMSFNCGraphicsContextWEB.BeginPrinting;
begin
end;

procedure TTMSFNCGraphicsContextWEB.EndPrinting;
begin
end;

procedure TTMSFNCGraphicsContextWEB.BeginScene;
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

function TTMSFNCGraphicsContextWEB.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  Result := DrawTextInternal(AText, ARect, AWordWrapping, gtaLeading, True);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.ClipPath(APath: TTMSFNCGraphicsPath);
//var
//  pth: ^cairo_path_t;
begin
  if not Assigned(FNCanvas) then
    Exit;

//  pth := ConvertToPath(APath);
  try
    SaveContext;
//    cairo_append_path(FNCanvas, pth);
//    cairo_clip(FNCanvas);
  finally

  end;
end;

procedure TTMSFNCGraphicsContextWEB.ClipRect(ARect: TRectF);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_rectangle(FNCanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
//  cairo_clip(FNCanvas);
end;

constructor TTMSFNCGraphicsContextWEB.Create(const AGraphics: TTMSFNCGraphics);
begin
  inherited;
  FScale := TTMSFNCUtils.GetDPIScale;
  FNeedsRendering := True;
  FContextSize.cx := 0;
  FContextSize.cy := 0;
  FFont := TTMSFNCGraphicsFont.Create;
  FFont.OnChanged := @FontChanged;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FStroke.OnChanged := @StrokeChanged;
  FFill := TTMSFNCGraphicsFill.Create;
  FFill.OnChanged := @FillChanged;
end;

function TTMSFNCGraphicsContextWEB.CreatePath: Pointer;
begin
  Result := nil;
//  cairo_new_path(FNCanvas);
//  Result := cairo_copy_path(FNCanvas);
end;

destructor TTMSFNCGraphicsContextWEB.Destroy;
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

procedure TTMSFNCGraphicsContextWEB.DestroyResources;
begin
  if Assigned(FNCanvas) then
  begin
    RestoreContext;
//    cairo_destroy(FNCanvas);
    FNCanvas := nil;
  end;

  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TTMSFNCGraphicsContextWEB.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_arc(FNCanvas, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle));
  ApplyStroke;
//  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
//var
//  R: TRectF;
//  img: Pcairo_surface_t;
//
//  function GraphicToARGB32(Source: TGraphic; buf: PByte): Boolean;
//  var
//    p: PDWord;
//    x, y: Integer;
//    c: TFPColor;
//    Img: TLazIntfImage;
//  begin
//    Img := TRasterImage(Source).CreateIntfImage;
//    try
//      if Img.DataDescription.Format = ricfNone then begin
//        Result := False;
//        Exit;
//      end;
//      p := Pointer(buf);
//      for y := 0 to Source.Height-1 do begin
//        for x := 0 to Source.Width-1 do begin
//          c := Img.Colors[x, y];
//          p^ := Hi(c.alpha) shl 24 + Hi(c.red) shl 16 + Hi(c.green) shl 8 + Hi(c.blue);
//          inc(p);
//        end;
//      end;
//    finally
//      Img.Free;
//    end;
//    Result := True;
//  end;

//  function CreateCairoImage: Pcairo_surface_t;
//  var
//    buf: PByte;
//  begin
//    try
//      buf := GetMem(ABitmap.Width*ABitmap.Height*4);
//
//      if not GraphicToARGB32(ABitmap, buf) then
//        Exit;
//
//      Result := cairo_image_surface_create_for_data(buf, CAIRO_FORMAT_ARGB32, ABitmap.Width, ABitmap.Height, ABitmap.Width * 4);
//    finally
//      Freemem(buf);
//    end;
//  end;
begin
  if not Assigned(ABitmap) or not Assigned(FNCanvas) then
    Exit;

  SaveContext;
//  R := ADstRect;
//  img := CreateCairoImage;
  try
//    cairo_translate(FNCanvas, R.Left, R.Top);
//    cairo_scale(FNCanvas, r.Width / ABitmap.Width, r.Height / ABitmap.Height);
//    cairo_set_source_surface(FNCanvas, img, 0, 0);
//    cairo_paint(FNCanvas);
  finally
//    cairo_surface_destroy(img);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
//var
//  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

//  r := ARect;
  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_arc(FNCanvas, r.Left + r.Width / 2, r.Top + r.Height / 2, r.Width / 2, 0, 2 * PI);
  ApplyStroke;
//  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextWEB.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if not Assigned(FNCanvas) then
    Exit;

end;

procedure TTMSFNCGraphicsContextWEB.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint: TPointF; AToPoint: TPointF;
  AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode;
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_move_to(FNCanvas, AFromPoint.X, AFromPoint.Y);
//  cairo_line_to(FNCanvas, AToPoint.X, AToPoint.Y);
//  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawLinearGradient(ARect: TRectF);
//var
//  pat: Pcairo_pattern_t;
begin
  if not Assigned(FNCanvas) then
    Exit;

  case FFill.Orientation of
    gfoHorizontal: {pat := cairo_pattern_create_linear(ARect.Left, ARect.Top + ARect.Height / 2, ARect.Right, ARect.Top + ARect.Height / 2)};
    gfoVertical: {pat := cairo_pattern_create_linear(ARect.Left + ARect.Width / 2, ARect.Top, ARect.Left + ARect.Width / 2, ARect.Bottom)};
  end;

  if (FFill.ColorTo <> gcnull) and (FFill.ColorMirror <> gcNull) then
  begin
//    cairo_pattern_add_color_stop_rgba(pat, 0, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
//    cairo_pattern_add_color_stop_rgba(pat, 0.5, TTMSFNCGraphics.GetColorRed(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorTo) / 255, FFill.Opacity);
//    cairo_pattern_add_color_stop_rgba(pat, 0.5, TTMSFNCGraphics.GetColorRed(FFill.ColorMirror) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorMirror) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorMirror) / 255, FFill.Opacity);
//    cairo_pattern_add_color_stop_rgba(pat, 1, TTMSFNCGraphics.GetColorRed(FFill.ColorMirrorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorMirrorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorMirrorTo) / 255, FFill.Opacity);
  end
  else if FFill.ColorTo <> gcNull then
  begin
//    cairo_pattern_add_color_stop_rgba(pat, 0, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
//    cairo_pattern_add_color_stop_rgba(pat, 1, TTMSFNCGraphics.GetColorRed(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorTo) / 255, FFill.Opacity);
  end;

//  cairo_rectangle(FNCanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
//  cairo_set_source(FNCanvas, pat);
//  cairo_fill(FNCanvas);
//  cairo_pattern_destroy(pat);
end;

procedure TTMSFNCGraphicsContextWEB.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
begin
  if Assigned(APath) then
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

procedure TTMSFNCGraphicsContextWEB.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
//var
//  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_move_to(FNCanvas, APolygon[0].X, APolygon[0].Y);
//  for I := 1 to Length(APolygon) - 1 do
//    cairo_line_to(FNCanvas, APolygon[I].X, APolygon[I].Y);
//  cairo_close_path(FNCanvas);
  ApplyStroke;
//  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
//var
//  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_move_to(FNCanvas, APolyline[0].X, APolyline[0].Y);
//  for I := 1 to Length(APolyline) - 1 do
//    cairo_line_to(FNCanvas, APolyline[I].X, APolyline[I].Y);
  ApplyStroke;
//  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.DrawRect(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
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

procedure TTMSFNCGraphicsContextWEB.DrawRoundRect(
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

procedure TTMSFNCGraphicsContextWEB.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TTMSFNCGraphicsTextAlign;
  ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single);
var
  R: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  R := DrawTextInternal(AText, ARect, AWordWrapping, AHorizontalAlign, True);
  case AVerticalAlign of
    gtaCenter: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2, ARect.Right, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2 + (r.Bottom - r.Top)), AWordWrapping, AHorizontalAlign, False);
    gtaLeading: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + (r.Bottom - r.Top)), AWordWrapping, AHorizontalAlign, False);
    gtaTrailing: DrawTextInternal(AText, RectF(ARect.Left, ARect.Bottom - (r.Bottom - r.Top), ARect.Right, ARect.Bottom), AWordWrapping, AHorizontalAlign, False);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.EndScene;
begin
  if not Assigned(FNCanvas) then
    Exit;

  Render;
end;

procedure TTMSFNCGraphicsContextWEB.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_arc(FNCanvas, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
//      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
//      cairo_clip(FNCanvas);
      DrawLinearGradient(RectF(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ACenter.X + ARadius.X, ACenter.Y + ARadius.Y));
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.FillChanged(Sender: TObject);
begin
  if not Assigned(FNCanvas) then
     Exit;

  ApplyFill;
end;

procedure TTMSFNCGraphicsContextWEB.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  r := ARect;
  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_arc(FNCanvas, r.Left + r.Width / 2, r.Top + r.Height / 2, r.Width / 2, 0, 2 * PI);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
//      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
//      cairo_clip(FNCanvas);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
begin
  if Assigned(APath) then
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

procedure TTMSFNCGraphicsContextWEB.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
//var
//  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(Apolygon) = 0 then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_move_to(FNCanvas, APolygon[0].X, APolygon[0].Y);
//  for I := 1 to Length(APolygon) - 1 do
//    cairo_line_to(FNCanvas, APolygon[I].X, APolygon[I].Y);
//  cairo_close_path(FNCanvas);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
//      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
//      cairo_clip(FNCanvas);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
//var
//  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_move_to(FNCanvas, APolyline[0].X, APolyline[0].Y);
//  for I := 1 to Length(APolyline) - 1 do
//    cairo_line_to(FNCanvas, APolyline[I].X, APolyline[I].Y);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
//      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
//      cairo_clip(FNCanvas);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  r := ARect;
  SaveContext;
//  cairo_new_path(FNCanvas);
//  cairo_rectangle(FNCanvas, r.Left, r.Top, r.Width, r.Height);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
//      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
//      cairo_clip(FNCanvas);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.FillRoundRect(AFill: TTMSFNCGraphicsFill;
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

procedure TTMSFNCGraphicsContextWEB.FontChanged(Sender: TObject);
begin
end;

function TTMSFNCGraphicsContextWEB.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextWEB.GetNativeCanvas: Pointer;
begin
  Result := FNCanvas;
end;

procedure TTMSFNCGraphicsContextWEB.PathClose(APath: Pointer);
begin
//  cairo_close_path(FNCanvas);
end;

procedure TTMSFNCGraphicsContextWEB.PathLineTo(APath: Pointer; APoint: TPointF);
begin
//  cairo_line_to(FNCanvas, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextWEB.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
//  cairo_move_to(FNCanvas, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextWEB.PathOpen(APath: Pointer);
begin
end;

procedure TTMSFNCGraphicsContextWEB.Render;
begin
  if not FNeedsRendering then
    Exit;

//  if Assigned(FNCanvas) and Assigned(FBitmap) then
//    Canvas.Draw(0, 0, FBitmap);

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextWEB.ResetClip;
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  if AAngle <> 0 then
    RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.ResetTransform;
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextWEB.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextWEB.RotateTransform(AAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

//  cairo_rotate(FNCanvas, DegToRad(AAngle));
end;

function TTMSFNCGraphicsContextWEB.DrawTextInternal(AText: String; ARect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; ACalculate: Boolean): TRectF;
var
  i: Integer;
  s, sn: string;
//  st: string;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  //p: Integer;
  tw: Single;
  th: Single;
  lcnt: Integer;
  rs: TRectF;
  fws: Single;
//  te: cairo_text_extents_t;
//  ths: Single;

  procedure ApplyFont;
//  var
//    slant: cairo_font_slant_t;
//    weight: cairo_font_weight_t;
//    fn: string;
//    sz: Single;
  begin
//    fn := FFont.Name;
//    if (fn = '') or (UpperCase(fn) = 'DEFAULT') then
//      fn := 'sans-serif';
//
//    sz := FFont.Size;
//    if sz = 0 then
//      sz := 12;
//
//    if fsBold in FFont.Style then
//      weight := CAIRO_FONT_WEIGHT_BOLD
//    else
//      weight := CAIRO_FONT_WEIGHT_NORMAL;
//
//    if fsItalic in FFont.Style then
//      slant := CAIRO_FONT_SLANT_ITALIC
//    else
//      slant := CAIRO_FONT_SLANT_NORMAL;
//
//    cairo_select_font_face(FNCanvas, PChar(fn), slant, weight);
//    cairo_set_font_size(FNCanvas, sz * 1.3);
//    cairo_set_source_rgb(FNCanvas, TTMSFNCGraphics.GetColorRed(FFont.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFont.Color) / 255,
//      TTMSFNCGraphics.GetColorBlue(FFont.Color) / 255);
  end;

  function FindNextWord(AText: String; var APos: Integer): String;
  var
    l: integer;
    i: integer;
  begin
    Result := '';

    l := Length(AText);
    if APos > l then
      Exit;

    i := APos;
    while True do
    begin
      if ((AText[i] = #10) and (AText[i - 1] = #13)) or ((AText[i] = #13) and (AText[i - 1] = #10)) or (AText[i] = ' ') then
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
begin
  ApplyFont;
  if not AWordWrap then
  begin
//    cairo_text_extents(FNCanvas, PChar(AText), @te);
    if not ACalculate then
    begin
//      case AHorizontalAlignment of
//        gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - te.x_advance) / 2, ARect.Top + te.height);
//        gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + te.height);
//        gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - te.x_advance, ARect.Top + te.height);
//      end;
//      cairo_show_text(FNCanvas, PChar(AText));
    end;
//    Result := RectF(ARect.Left, ARect.Top, ARect.Left + te.x_advance, ARect.Top + te.Height);
  end
  else
  begin
    rs := ARect;

    mw := 0;
    i := 1;
//    ths := FFont.Size * 0.5;
    lcnt := 0;
    fws := 0;
    tw := 0;
    s := FindNextWord(AText, i);
//    cairo_text_extents(FNCanvas, PChar(s), @te);
//    w := te.x_advance;
//    th := te.Height + ths;

    mw := mw + w;
    if (Length(s) > 0) and (s[Length(s)] = ' ') then
      mw := mw + fws;

    while i <= Length(AText) do
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

      sn := FindNextWord(AText, i);
//      cairo_text_extents(FNCanvas, PChar(sn), @te);
//      w := te.x_advance;
//      th := Max(th, te.Height + ths);

      if (ARect.Left + mw + w > ARect.Right) or f then
      begin
        if s <> '' then
        begin
//          p := Length(s);
//          st := Copy(s, 1, p);

          Inc(lcnt);
          if mw > tw then
            tw := mw;

          if not ACalculate then
          begin
//            case AHorizontalAlignment of
//              gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - mw) / 2, ARect.Top + th);
//              gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + th);
//              gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - mw, ARect.Top + th);
//            end;
//            cairo_show_text(FNCanvas, PChar(st));
          end;

          mw := 0;
        end;

        s := '';

        ARect.Top := ARect.Top + th;
        if (Trunc(ARect.Top) > Trunc(ARect.Bottom - th)) and not ACalculate then
          Break;
      end;

      mw := mw + w;
      if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
        mw := mw + fws;
      s := s + sn;
    end;

    if s <> '' then
    begin
//      p := Length(s);
//      st := Copy(s, 1, p);
      Inc(lcnt);
      if mw > tw then
        tw := mw;

      if not ACalculate then
      begin
//        case AHorizontalAlignment of
//          gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - mw) / 2, ARect.Top + th);
//          gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + th);
//          gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - mw, ARect.Top + th);
//        end;
//        cairo_show_text(FNCanvas, PChar(st));
      end;
    end;

    Result := RectF(rs.Left, rs.Top, rs.Left + tw, rs.Top + lcnt * th);
  end;
end;

procedure TTMSFNCGraphicsContextWEB.SaveContext;
begin
  if not Assigned(FNCanvas) then
    Exit;

//  cairo_save(FNCanvas);
end;

procedure TTMSFNCGraphicsContextWEB.RestoreContext;
begin
  if not Assigned(FNCanvas) then
    Exit;

//  cairo_restore(FNCanvas);
end;

procedure TTMSFNCGraphicsContextWEB.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
end;

procedure TTMSFNCGraphicsContextWEB.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

//  cairo_scale(FNCanvas, AX, AY);
end;

procedure TTMSFNCGraphicsContextWEB.SetSize(AWidth, AHeight: Single);
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  FBitmap := TBitmap.Create;
//  FBitmap.SetSize(Round(AWidth * FScale), Round(AHeight * FScale));
//  FBitmap.Canvas.Brush.Style := bsSolid;
//  FBitmap.Canvas.Brush.Color := gcWhite;
//  FBitmap.Canvas.FillRect(0, 0, FBitmap.Width, FBitmap.Height);

//  {$IFDEF LINUX}
//  {$IFDEF LCLGTK2}
//  FNCanvas := gdk_cairo_create(TGtkDeviceContext(FBitmap.Canvas.Handle).Drawable);
//  {$ENDIF}
//  {$ENDIF}
//  {$IFDEF DARWIN}
//  FNCanvas := nil;
//  {$ENDIF}
  ScaleTransform(FScale, FScale);
  SaveContext;
end;

procedure TTMSFNCGraphicsContextWEB.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
end;

procedure TTMSFNCGraphicsContextWEB.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
end;

procedure TTMSFNCGraphicsContextWEB.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FNCanvas) then
    Exit;

//  if AAntiAliasing then
//  begin
//    {$IFDEF LINUX}
//    cairo_set_antialias(FNCanvas, CAIRO_ANTIALIAS_BEST);
//    {$ENDIF}
//    {$IFDEF DARWIN}
//    cairo_set_antialias(FNCanvas, CAIRO_ANTIALIAS_DEFAULT);
//    {$ENDIF}
//  end
//  else
//    cairo_set_antialias(FNCanvas, CAIRO_ANTIALIAS_NONE);
end;

procedure TTMSFNCGraphicsContextWEB.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextWEB.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWEB.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextWEB.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.AssignSource(AFont);
end;

procedure TTMSFNCGraphicsContextWEB.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWEB.SetFontName(AName: string);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextWEB.SetFontSize(ASize: Integer);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextWEB.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextWEB.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextWEB.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWEB.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextWEB.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextWEB.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  w, h, c, s, cw, ch: Single;
begin
  Result := ARect;
  if not Assigned(FNCanvas) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := DegToRad(AAngle);
    cx.X := Result.Left + (Result.Right - Result.Left) / 2;
    cx.Y := Result.Top + (Result.Bottom - Result.Top) / 2;

    SaveContext;

//    cairo_translate(FNCanvas, cx.X, cx.Y);
//    cairo_rotate(FNCanvas, ar);

    w := Result.Right - Result.Left;
    h := Result.Bottom - Result.Top;
    c := Cos(ar);
    s := Sin(ar);

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextWEB.StartSpecialPen;
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextWEB.StopSpecialPen;
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextWEB.StrokeChanged(Sender: TObject);
begin
  if not Assigned(FNCanvas) then
     Exit;

  ApplyStroke;
end;

procedure TTMSFNCGraphicsContextWEB.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

//  cairo_translate(FNCanvas, AX, AY);
end;

{$ENDIF}

end.
