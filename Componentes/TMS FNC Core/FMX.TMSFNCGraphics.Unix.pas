{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
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

unit FMX.TMSFNCGraphics.Unix;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF UNIX}
{$IFDEF LINUX}

uses
  Classes
  {$IFDEF LCLLIB}
  ,Cairo
  {$IFDEF LCLGTK2}
  ,gdk2, Gtk2Def
  {$ENDIF}
  ,IntfGraphics, GraphType, FPimage
  {$ENDIF}
  ,FMX.TMSFNCGraphicsTypes, FMX.TMSFNCUtils, {%H-}Types, FMX.TMSFNCTypes, FMX.TMSFNCGraphics, FMX.Graphics
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

{$IFDEF FMXLIB}
const
  CairoLib = 'libcairo.so.2';
var
  CairoLoaded: Boolean = False;
  CairoHandle: THandle;
  CairoDLLPath: string = '';
{$ENDIF}

type
  {$IFDEF FMXLIB}
  PGdkDrawable = Pointer;
  Pcairo_t = Pointer;
  cairo_path_t = Pointer;
  Pcairo_path_t = ^cairo_path_t;
  cairo_surface_t = pointer;
  Pcairo_surface_t = ^cairo_surface_t;
  cairo_pattern_t = Pointer;
  Pcairo_pattern_t = ^cairo_pattern_t;
  cairo_text_extents_t = record
     x_bearing : Double;
     y_bearing : Double;
     width : Double;
     height : Double;
     x_advance : Double;
     y_advance : Double;
   end;
  Pcairo_text_extents_t = ^cairo_text_extents_t;
  cairo_font_slant_t = (
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_SLANT_ITALIC,
    CAIRO_FONT_SLANT_OBLIQUE
  );
  cairo_font_weight_t = (
    CAIRO_FONT_WEIGHT_NORMAL,
    CAIRO_FONT_WEIGHT_BOLD
  );
  cairo_antialias_t = (
    CAIRO_ANTIALIAS_DEFAULT,
    CAIRO_ANTIALIAS_NONE,
    CAIRO_ANTIALIAS_GRAY,
    CAIRO_ANTIALIAS_SUBPIXEL,
    CAIRO_ANTIALIAS_FAST,
    CAIRO_ANTIALIAS_GOOD,
    CAIRO_ANTIALIAS_BEST
  );
  cairo_format_t = (
    CAIRO_FORMAT_INVALID,
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1,
    CAIRO_FORMAT_RGB16_565,
    CAIRO_FORMAT_RGB30
  );

var
  cairo_set_source_rgb: procedure(cr: Pcairo_t; red, green, blue: Double); cdecl;
  cairo_set_source_rgba: procedure(cr: Pcairo_t; red, green, blue, alpha: Double); cdecl;
  cairo_set_line_width: procedure(cr: Pcairo_t; width: Double); cdecl;
  cairo_set_dash: procedure(cr: Pcairo_t; dashes: PDouble; num_dashes: LongInt; offset: Double); cdecl;
  cairo_append_path: procedure(cr: Pcairo_t; path: Pcairo_path_t); cdecl;
  cairo_clip: procedure(cr: Pcairo_t); cdecl;
  cairo_new_path: procedure(cr: Pcairo_t); cdecl;
  cairo_rectangle: procedure(cr: Pcairo_t; x, y, width, height: Double); cdecl;
  cairo_copy_path: function(cr: Pcairo_t): Pcairo_path_t; cdecl;
  cairo_destroy: procedure (data: Pointer); cdecl;
  cairo_create: function (data: Pointer): Pointer; cdecl;
  cairo_arc: procedure(cr: Pcairo_t; xc, yc, radius, angle1, angle2: Double); cdecl;
  cairo_stroke: procedure(cr: Pcairo_t); cdecl;
  cairo_move_to: procedure(cr: Pcairo_t; x, y: Double); cdecl;
  cairo_new_sub_path: procedure(cr: Pcairo_t); cdecl;
  cairo_line_to: procedure(cr: Pcairo_t; x, y: Double); cdecl;
  cairo_pattern_create_linear: function(x0, y0, x1, y1: Double): Pcairo_pattern_t; cdecl;
  cairo_pattern_create_radial: function(cx0, cy0, radius0, cx1, cy1, radius1: Double): Pcairo_pattern_t; cdecl;
  cairo_pattern_add_color_stop_rgba: procedure(pattern: Pcairo_pattern_t; offset, red, green, blue, alpha: Double); cdecl;
  cairo_set_source: procedure(cr: Pcairo_t; source: Pcairo_pattern_t); cdecl;
  cairo_fill: procedure(cr: Pcairo_t); cdecl;
  cairo_pattern_destroy: procedure(pattern: Pcairo_pattern_t); cdecl;
  cairo_close_path: procedure(cr: Pcairo_t); cdecl;
  cairo_rotate: procedure(cr: Pcairo_t; angle: Double); cdecl;
  cairo_select_font_face: procedure(cr: Pcairo_t; family: PUTF8Char; slant: cairo_font_slant_t; weight: cairo_font_weight_t); cdecl;
  cairo_set_font_size: procedure(cr: Pcairo_t; size: Double); cdecl;
  cairo_text_extents: procedure(cr: Pcairo_t; utf8: PUTF8Char; extents: Pcairo_text_extents_t); cdecl;
  cairo_show_text: procedure(cr: Pcairo_t; utf8: PUTF8Char); cdecl;
  cairo_save: procedure(cr: Pcairo_t); cdecl;
  cairo_restore: procedure(cr: Pcairo_t); cdecl;
  cairo_scale: procedure(cr: Pcairo_t; sx, sy: Double); cdecl;
  cairo_set_source_surface: procedure(cr: Pcairo_t; surface: Pointer; x: double; y: double); cdecl;
  cairo_paint: procedure(cr: Pcairo_t); cdecl;
  cairo_set_antialias: procedure(cr: Pcairo_t; antialias: cairo_antialias_t); cdecl;
  cairo_translate: procedure(cr: Pcairo_t; tx, ty: Double); cdecl;
  cairo_image_surface_create_for_data: function(data: Pointer; format: cairo_format_t; width: Integer; height: Integer; stride: Integer): Pointer; cdecl;
  cairo_format_stride_for_width: function(format: cairo_format_t; width: Integer): Integer; cdecl;
  cairo_image_surface_create: function(format: cairo_format_t; width: Integer; height: Integer): Pointer; cdecl;
  cairo_surface_destroy: procedure(surface: Pointer); cdecl;
  cairo_image_surface_get_data: function(surface: Pointer): Pointer; cdecl;
  cairo_image_surface_get_width: function(surface: Pointer): Integer; cdecl;
  cairo_image_surface_get_height: function(surface: Pointer): Integer; cdecl;
  cairo_image_surface_get_format: function(surface: Pointer): cairo_format_t; cdecl;
  {$ENDIF}

{$IFDEF FMXLIB}
type
{$ENDIF}
  TTMSFNCGraphicsContextUnix = class(TTMSFNCGraphicsContext)
  private
    FActivePath: TTMSFNCGraphicsPath;
    FScale: Single;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FContextSize: TSizeF;
    FNCanvas: Pcairo_t;
    {$IFDEF FMXLIB}
    FMapping: Boolean;
    FBitmapData: TBitmapData;
    FNSurface: Pointer;
    {$ENDIF}
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
{$ENDIF}

implementation

{$IFDEF UNIX}
{$IFDEF LINUX}

uses
  SysUtils, Math;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextUnix;
end;

{ TTMSFNCGraphicsContextUnix }

procedure TTMSFNCGraphicsContextUnix.ApplyFill;
begin
  if not Assigned(FNCanvas) then
     Exit;

  case FFill.Kind of
    gfkSolid:
    begin
      cairo_set_source_rgba(FNCanvas, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255,
        TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
    end;
  end;
end;

procedure TTMSFNCGraphicsContextUnix.ApplyStroke;
var
  f: array of Double;
begin
  if not Assigned(FNCanvas) then
     Exit;

  cairo_set_line_width(FNCanvas, FStroke.Width);
  cairo_set_source_rgba(FNCanvas, TTMSFNCGraphics.GetColorRed(FStroke.Color) / 255, TTMSFNCGraphics.GetColorGreen(FStroke.Color) / 255,
    TTMSFNCGraphics.GetColorBlue(FStroke.Color) / 255, FStroke.Opacity);

  case FStroke.Kind of
    gskSolid, gskNone: cairo_set_dash(FNCanvas, nil,0, 0);
    gskDash:
    begin
      SetLength(f, 2);
      f[0] := 3;
      f[1] := 1;
      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
    end;
    gskDot:
    begin
      SetLength(f, 2);
      f[0] := 1;
      f[1] := 1;
      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
    end;
    gskDashDot:
    begin
      SetLength(f, 4);
      f[0] := 3;
      f[1] := 1;
      f[2] := 1;
      f[3] := 1;
      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
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
      cairo_set_dash(FNCanvas, @f[0], Length(f), 0);
    end;
  end;
end;

procedure TTMSFNCGraphicsContextUnix.BeginPrinting;
begin
end;

procedure TTMSFNCGraphicsContextUnix.EndPrinting;
begin
end;

procedure TTMSFNCGraphicsContextUnix.BeginScene;
begin
  if not Assigned(FNCanvas) then
    Exit;

  {$IFDEF FMXLIB}
  Canvas.BeginScene;
  Canvas.Clear(gcNull);
  {$ENDIF}
end;

function TTMSFNCGraphicsContextUnix.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  Result := DrawTextInternal(AText, ARect, AWordWrapping, gtaLeading, True);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.ClipPath(APath: TTMSFNCGraphicsPath);
var
  pth: Pcairo_path_t;
begin
  if not Assigned(FNCanvas) then
    Exit;

  pth := ConvertToPath(APath);
  try
    SaveContext;
    cairo_append_path(FNCanvas, pth);
    cairo_clip(FNCanvas);
  finally

  end;
end;

procedure TTMSFNCGraphicsContextUnix.ClipRect(ARect: TRectF);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_rectangle(FNCanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
  cairo_clip(FNCanvas);
end;

constructor TTMSFNCGraphicsContextUnix.Create(const AGraphics: TTMSFNCGraphics);
begin
  inherited;
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

function TTMSFNCGraphicsContextUnix.CreatePath: Pointer;
begin
  cairo_new_path(FNCanvas);
  Result := cairo_copy_path(FNCanvas);
end;

destructor TTMSFNCGraphicsContextUnix.Destroy;
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

procedure TTMSFNCGraphicsContextUnix.DestroyResources;
begin
  if Assigned(FNCanvas) then
  begin
    RestoreContext;
    {$IFNDEF FMXLIB}
    cairo_destroy(FNCanvas);
    {$ENDIF}
    {$IFDEF FMXLIB}
    cairo_destroy(FNCanvas);
    cairo_surface_destroy(FNSurface);
    FNSurface := nil;
    {$ENDIF}
    FNCanvas := nil;
  end;

  if Assigned(FBitmap) then
  begin
    {$IFDEF FMXLIB}
    if FMapping then
    begin
      FBitmap.UnMap(FBitmapData);
      FMapping := False;
    end;
    {$ENDIF}

    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TTMSFNCGraphicsContextUnix.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_arc(FNCanvas, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle));
  ApplyStroke;
  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  R: TRectF;
  img: Pcairo_surface_t;
  {$IFDEF FMXLIB}
  buf: Pointer;
  bd: TBitmapData;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  buf: PByte;
  {$ENDIF}

  {$IFNDEF FMXLIB}
  function GraphicToARGB32(Source: TGraphic; buf: PByte): Boolean;
  var
    p: PDWord;
    x, y: Integer;
    c: TFPColor;
    Img: TLazIntfImage;
  begin
    Img := TRasterImage(Source).CreateIntfImage;
    try
      if Img.DataDescription.Format = ricfNone then begin
        Result := False;
        Exit;
      end;
      p := Pointer(buf);
      for y := 0 to Source.Height-1 do begin
        for x := 0 to Source.Width-1 do begin
          c := Img.Colors[x, y];
          p^ := Hi(c.alpha) shl 24 + Hi(c.red) shl 16 + Hi(c.green) shl 8 + Hi(c.blue);
          inc(p);
        end;
      end;
    finally
      Img.Free;
    end;
    Result := True;
  end;
  {$ENDIF}

begin
  if not Assigned(ABitmap) or not Assigned(FNCanvas) then
    Exit;

  img := nil;
  R := ADstRect;
  try
    SaveContext;
    {$IFNDEF FMXLIB}
    buf := GetMem(ABitmap.Width*ABitmap.Height*4);

    if not GraphicToARGB32(ABitmap, buf) then
    begin
      FreeMem(buf);
      RestoreContext;
      Exit;
    end;
    {$ENDIF}

    {$IFDEF FMXLIB}
    if ABitmap.Map(TMapAccess.Read, bd) then
    begin
      buf := bd.Data;
    {$ENDIF}

    img := cairo_image_surface_create_for_data(buf, CAIRO_FORMAT_ARGB32, ABitmap.Width, ABitmap.Height, ABitmap.Width * 4);

    {$IFDEF FMXLIB}
      ABitmap.Unmap(bd);
    end;
    {$ENDIF}

    if Assigned(img) then
    begin
      cairo_translate(FNCanvas, R.Left, R.Top);
      cairo_scale(FNCanvas, r.Width / ABitmap.Width, r.Height / ABitmap.Height);
      cairo_set_source_surface(FNCanvas, img, 0, 0);
      cairo_paint(FNCanvas);
    end;
  finally
    if Assigned(img) then
      cairo_surface_destroy(img);

    {$IFNDEF FMXLIB}
    Freemem(buf);
    {$ENDIF}
    RestoreContext;
  end;
end;

procedure TTMSFNCGraphicsContextUnix.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  r := ARect;
  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_arc(FNCanvas, r.Left + r.Width / 2, r.Top + r.Height / 2, r.Width / 2, 0, 2 * PI);
  ApplyStroke;
  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextUnix.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if not Assigned(FNCanvas) then
    Exit;

end;

procedure TTMSFNCGraphicsContextUnix.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint: TPointF; AToPoint: TPointF;
  AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode;
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_move_to(FNCanvas, AFromPoint.X, AFromPoint.Y);
  cairo_line_to(FNCanvas, AToPoint.X, AToPoint.Y);
  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.DrawLinearGradient(ARect: TRectF);
var
  pat: Pcairo_pattern_t;
  rd, a, sx, sy: single;
  c: TPointF;
  f0, f1: Boolean;
  I: Integer;
  it: TTMSFNCGraphicsFillGradientItem;
  pt0, pt1: TPointF;
  ist, st: TTMSFNCGraphicsMatrix;
  ivs: TPointF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  pat := nil;

  case FFill.GradientType of
    gfgtLinear:
    begin
      case FFill.Orientation of
        gfoHorizontal: pat := cairo_pattern_create_linear(ARect.Left, ARect.Top + ARect.Height / 2, ARect.Right, ARect.Top + ARect.Height / 2);
        gfoVertical: pat := cairo_pattern_create_linear(ARect.Left + ARect.Width / 2, ARect.Top, ARect.Left + ARect.Width / 2, ARect.Bottom);
        gfoCustom:
        begin
          a := DegToRad(FFill.GradientAngle);
          c := PointF(ARect.Left + ARect.Width / 2, ARect.Top + ARect.Height / 2);
          pt0 := PointF(c.x - Cos(a) * ARect.Width / 2, c.y - Sin(a) * ARect.height / 2);
          pt1 := PointF(c.x + Cos(a) * ARect.Width / 2, c.y + Sin(a) * ARect.height / 2);
          pat := cairo_pattern_create_linear(pt0.X, pt0.Y, pt1.X, pt1.Y);
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

      st := MatrixCreateScaling(sx, sy);
      ist := st.Inverse;

      ivs := PointF(ist.m11, ist.m22);

      c := PointF(FFill.GradientCenterPoint.X * ivs.x, FFill.GradientCenterPoint.Y * ivs.y);

      rd := (ARect.Width / 2) * ivs.x;
      if sy > sx then
        rd := (ARect.Height / 2) * ivs.y;

      pat := cairo_pattern_create_radial(c.X, c.Y, 0, c.X, c.Y, rd);
    end;
  end;

  case FFill.GradientMode of
    gfgmDefault:
    begin
      if (FFill.ColorTo <> gcnull) and (FFill.ColorMirror <> gcNull) then
      begin
        cairo_pattern_add_color_stop_rgba(pat, 0, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
        cairo_pattern_add_color_stop_rgba(pat, 0.5, TTMSFNCGraphics.GetColorRed(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorTo) / 255, FFill.Opacity);
        cairo_pattern_add_color_stop_rgba(pat, 0.5, TTMSFNCGraphics.GetColorRed(FFill.ColorMirror) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorMirror) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorMirror) / 255, FFill.Opacity);
        cairo_pattern_add_color_stop_rgba(pat, 1, TTMSFNCGraphics.GetColorRed(FFill.ColorMirrorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorMirrorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorMirrorTo) / 255, FFill.Opacity);
      end
      else if FFill.ColorTo <> gcNull then
      begin
        cairo_pattern_add_color_stop_rgba(pat, 0, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
        cairo_pattern_add_color_stop_rgba(pat, 1, TTMSFNCGraphics.GetColorRed(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorGreen(FFill.ColorTo) / 255, TTMSFNCGraphics.GetColorBlue(FFill.ColorTo) / 255, FFill.Opacity);
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

      if not f0 and (FFill.GradientItems.Count > 0) then
      begin
        cairo_pattern_add_color_stop_rgba(pat, 0, TTMSFNCGraphics.GetColorRed(FFill.GradientItems[0].Color) / 255,
          TTMSFNCGraphics.GetColorGreen(FFill.GradientItems[0].Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.GradientItems[0].Color) / 255,
          FFill.GradientItems[0].Opacity);
      end;

      for I := 0 to FFill.GradientItems.Count - 1 do
      begin
        it := FFill.GradientItems[I];
        cairo_pattern_add_color_stop_rgba(pat, it.Position, TTMSFNCGraphics.GetColorRed(it.Color) / 255,
          TTMSFNCGraphics.GetColorGreen(it.Color) / 255, TTMSFNCGraphics.GetColorBlue(it.Color) / 255,
          it.Opacity);
      end;

      if not f1 and (FFill.GradientItems.Count > 0) then
      begin
        cairo_pattern_add_color_stop_rgba(pat, 1, TTMSFNCGraphics.GetColorRed(FFill.GradientItems[FFill.GradientItems.Count - 1].Color) / 255,
          TTMSFNCGraphics.GetColorGreen(FFill.GradientItems[FFill.GradientItems.Count - 1].Color) / 255, TTMSFNCGraphics.GetColorBlue(FFill.GradientItems[FFill.GradientItems.Count - 1].Color) / 255,
          FFill.GradientItems[FFill.GradientItems.Count - 1].Opacity);
      end;
    end;
  end;


  cairo_rectangle(FNCanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if FFill.GradientType = gfgtRadial then
    cairo_scale(FNCanvas, st.m11, st.m22);

  cairo_set_source(FNCanvas, pat);

  cairo_fill(FNCanvas);

  if Ffill.GradientType = gfgtRadial then
    cairo_scale(FNCanvas, ivs.x, ivs.y);

  cairo_pattern_destroy(pat);
end;

procedure TTMSFNCGraphicsContextUnix.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: Pcairo_path_t;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      pth := ConvertToPath(APath);
      cairo_append_path(FNCanvas, pth);
      ApplyStroke;
      cairo_stroke(FNCanvas);
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

procedure TTMSFNCGraphicsContextUnix.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_move_to(FNCanvas, APolygon[0].X, APolygon[0].Y);
  for I := 1 to Length(APolygon) - 1 do
    cairo_line_to(FNCanvas, APolygon[I].X, APolygon[I].Y);
  cairo_close_path(FNCanvas);
  ApplyStroke;
  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_move_to(FNCanvas, APolyline[0].X, APolyline[0].Y);
  for I := 1 to Length(APolyline) - 1 do
    cairo_line_to(FNCanvas, APolyline[I].X, APolyline[I].Y);
  ApplyStroke;
  cairo_stroke(FNCanvas);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.DrawRect(AStroke: TTMSFNCGraphicsStroke;
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

procedure TTMSFNCGraphicsContextUnix.DrawRoundRect(
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

procedure TTMSFNCGraphicsContextUnix.DrawText(AText: string; ARect: TRectF;
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
    gtaCenter: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top + (ARect.Height - r.Height) / 2, ARect.Right, ARect.Top + (ARect.Height - r.Height) / 2 + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaLeading: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaTrailing: DrawTextInternal(AText, RectF(ARect.Left, ARect.Bottom - r.Height, ARect.Right, ARect.Bottom), AWordWrapping, AHorizontalAlign, False);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.EndScene;
begin
  if not Assigned(FNCanvas) then
    Exit;

  Render;
  {$IFDEF FMXLIB}
  Canvas.EndScene;
  {$ENDIF}
end;

procedure TTMSFNCGraphicsContextUnix.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_arc(FNCanvas, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
      cairo_clip(FNCanvas);
      DrawLinearGradient(RectF(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ACenter.X + ARadius.X, ACenter.Y + ARadius.Y));
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.FillChanged(Sender: TObject);
begin
  if not Assigned(FNCanvas) then
     Exit;

  ApplyFill;
end;

procedure TTMSFNCGraphicsContextUnix.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  r := ARect;
  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_arc(FNCanvas, r.Left + r.Width / 2, r.Top + r.Height / 2, r.Width / 2, 0, 2 * PI);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
      cairo_clip(FNCanvas);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: Pcairo_path_t;
  r: TRectF;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      pth := ConvertToPath(APath);
      cairo_append_path(FNCanvas, pth);
      case AFill.Kind of
        gfkSolid:
        begin
          ApplyFill;
          cairo_fill(FNCanvas);
        end;
        gfkGradient:
        begin
          cairo_clip(FNCanvas);
          r := APath.GetBounds;
          DrawLinearGradient(r);
        end;
      end;
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

procedure TTMSFNCGraphicsContextUnix.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(Apolygon) = 0 then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_move_to(FNCanvas, APolygon[0].X, APolygon[0].Y);
  for I := 1 to Length(APolygon) - 1 do
    cairo_line_to(FNCanvas, APolygon[I].X, APolygon[I].Y);
  cairo_close_path(FNCanvas);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
      cairo_clip(FNCanvas);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FNCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_move_to(FNCanvas, APolyline[0].X, APolyline[0].Y);
  for I := 1 to Length(APolyline) - 1 do
    cairo_line_to(FNCanvas, APolyline[I].X, APolyline[I].Y);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
      cairo_clip(FNCanvas);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FNCanvas) then
    Exit;

  r := ARect;
  SaveContext;
  cairo_new_path(FNCanvas);
  cairo_rectangle(FNCanvas, r.Left, r.Top, r.Width, r.Height);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      cairo_fill(FNCanvas);
    end;
    gfkGradient:
    begin
      cairo_clip(FNCanvas);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.FillRoundRect(AFill: TTMSFNCGraphicsFill;
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

procedure TTMSFNCGraphicsContextUnix.FontChanged(Sender: TObject);
begin
end;

function TTMSFNCGraphicsContextUnix.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextUnix.GetNativeCanvas: Pointer;
begin
  Result := FNCanvas;
end;

procedure TTMSFNCGraphicsContextUnix.PathClose(APath: Pointer);
begin
  cairo_close_path(FNCanvas);
end;

procedure TTMSFNCGraphicsContextUnix.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  cairo_line_to(FNCanvas, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextUnix.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  cairo_move_to(FNCanvas, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextUnix.PathOpen(APath: Pointer);
begin
end;

procedure TTMSFNCGraphicsContextUnix.Render;
begin
  if not FNeedsRendering then
    Exit;

  {$IFDEF FMXLIB}
  if Assigned(FNCanvas) and Assigned(FBitmap) then
  begin
    if FMapping then
    begin
      FBitmap.Unmap(FBitmapData);
      FMapping := False;
    end;
    Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Width), RectF(0, 0, FBitmap.Width, FBitmap.Width), 1);
  end;
  {$ENDIF}

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextUnix.ResetClip;
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  if AAngle <> 0 then
    RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.ResetTransform;
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextUnix.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FNCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextUnix.RotateTransform(AAngle: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  cairo_rotate(FNCanvas, DegToRad(AAngle));
end;

function TTMSFNCGraphicsContextUnix.DrawTextInternal(AText: String; ARect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; ACalculate: Boolean): TRectF;
var
  i: Integer;
  s, sn, st: string;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  p: Integer;
  tw: Single;
  th: Single;
  lcnt: Integer;
  rs: TRectF;
  fws: Single;
  te: cairo_text_extents_t;
  ths: Single;

  procedure ApplyFont;
  var
    slant: cairo_font_slant_t;
    weight: cairo_font_weight_t;
    fn: string;
    sz: Single;
  begin
    fn := FFont.Name;
    if (fn = '') or (UpperCase(fn) = 'DEFAULT') then
      fn := 'sans-serif';

    sz := FFont.Size;
    if sz = 0 then
      sz := 12;

    if TFontStyle.fsBold in FFont.Style then
      weight := CAIRO_FONT_WEIGHT_BOLD
    else
      weight := CAIRO_FONT_WEIGHT_NORMAL;

    if TFontStyle.fsItalic in FFont.Style then
      slant := CAIRO_FONT_SLANT_ITALIC
    else
      slant := CAIRO_FONT_SLANT_NORMAL;

    cairo_select_font_face(FNCanvas, PUTF8Char(UTF8Encode(fn)), slant, weight);
    cairo_set_font_size(FNCanvas, sz * 1.3);
    cairo_set_source_rgb(FNCanvas, TTMSFNCGraphics.GetColorRed(FFont.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFont.Color) / 255,
      TTMSFNCGraphics.GetColorBlue(FFont.Color) / 255);
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
    cairo_text_extents(FNCanvas, PUTF8Char(UTF8Encode(AText)), @te);
    if not ACalculate then
    begin
      case AHorizontalAlignment of
        gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - te.x_advance) / 2, ARect.Top + te.height);
        gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + te.height);
        gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - te.x_advance, ARect.Top + te.height);
      end;
      cairo_show_text(FNCanvas, PUTF8Char(UTF8Encode(AText)));
    end;
    Result := RectF(ARect.Left, ARect.Top, ARect.Left + te.x_advance, ARect.Top + te.Height);
  end
  else
  begin
    rs := ARect;

    mw := 0;
    i := 1;
    ths := FFont.Size * 0.5;
    lcnt := 0;
    fws := 0;
    tw := 0;
    s := FindNextWord(AText, i);
    cairo_text_extents(FNCanvas, PUTF8Char(UTF8Encode(s)), @te);
    w := te.x_advance;
    th := te.Height + ths;

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
      cairo_text_extents(FNCanvas, PUTF8Char(UTF8Encode(sn)), @te);
      w := te.x_advance;
      th := Max(th, te.Height + ths);

      if (ARect.Left + mw + w > ARect.Right) or f then
      begin
        if s <> '' then
        begin
          p := Length(s);
          st := Copy(s, 1, p);

          Inc(lcnt);
          if mw > tw then
            tw := mw;

          if not ACalculate then
          begin
            case AHorizontalAlignment of
              gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - mw) / 2, ARect.Top + th);
              gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + th);
              gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - mw, ARect.Top + th);
            end;
            cairo_show_text(FNCanvas, PUTF8Char(UTF8Encode(st)));
          end;

          mw := 0;
        end;

        s := '';

        ARect.Top := ARect.Top + th;
        if (Int(ARect.Top) > Int(ARect.Bottom - th)) and not ACalculate then
          Break;
      end;

      mw := mw + w;
      if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
        mw := mw + fws;
      s := s + sn;
    end;

    if s <> '' then
    begin
      p := Length(s);
      st := Copy(s, 1, p);
      Inc(lcnt);
      if mw > tw then
        tw := mw;

      if not ACalculate then
      begin
        case AHorizontalAlignment of
          gtaCenter: cairo_move_to(FNCanvas, ARect.Left + (ARect.Width - mw) / 2, ARect.Top + th);
          gtaLeading: cairo_move_to(FNCanvas, ARect.Left, ARect.Top + th);
          gtaTrailing: cairo_move_to(FNCanvas, ARect.Right - mw, ARect.Top + th);
        end;
        cairo_show_text(FNCanvas, PUTF8Char(UTF8Encode(st)));
      end;
    end;

    Result := RectF(rs.Left, rs.Top, rs.Left + tw, rs.Top + lcnt * th);
  end;
end;

procedure TTMSFNCGraphicsContextUnix.SaveContext;
begin
  if not Assigned(FNCanvas) then
    Exit;

  cairo_save(FNCanvas);
end;

procedure TTMSFNCGraphicsContextUnix.RestoreContext;
begin
  if not Assigned(FNCanvas) then
    Exit;

  cairo_restore(FNCanvas);
end;

procedure TTMSFNCGraphicsContextUnix.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FNCanvas) then
    Exit;

  SaveContext;
end;

procedure TTMSFNCGraphicsContextUnix.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  cairo_scale(FNCanvas, AX, AY);
end;

function TTMSFNCGraphicsContextUnix.GetMatrix: TTMSFNCGraphicsMatrix;
begin
end;

procedure TTMSFNCGraphicsContextUnix.SetScale(AScale: single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextUnix.SetMatrix(AMatrix: TTMSFNCGraphicsMatrix);
begin
end;

procedure TTMSFNCGraphicsContextUnix.SetSize(AWidth, AHeight: Single);
{$IFDEF FMXLIB}
var
  st: Integer;
{$ENDIF}
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  {$IFDEF LINUX}
  {$IFDEF LCLLIB}
  {$IFDEF LCLGTK2}
  FNCanvas := gdk_cairo_create(TGtkDeviceContext(Canvas.Handle).Drawable);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMXLIB}
  FBitmap := TBitmap.Create;
  FBitmap.SetSize(Round(AWidth * FScale), Round(AHeight * FScale));

  if FBitmap.Map(TMapAccess.Write, FBitmapData) then
  begin
    st := cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, FBitmap.Width);
    FNSurface := cairo_image_surface_create_for_data(FBitmapData.Data, CAIRO_FORMAT_ARGB32, FBitmap.Width, FBitmap.Height, st);
    FNCanvas := cairo_create(FNSurface);
    FMapping := True;
  end;

  ScaleTransform(FScale, FScale);
  {$ENDIF}
  {$ENDIF}

  SaveContext;
end;

procedure TTMSFNCGraphicsContextUnix.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
end;

procedure TTMSFNCGraphicsContextUnix.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
end;

procedure TTMSFNCGraphicsContextUnix.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FNCanvas) then
    Exit;

  if AAntiAliasing then
    cairo_set_antialias(FNCanvas, CAIRO_ANTIALIAS_BEST)
  else
    cairo_set_antialias(FNCanvas, CAIRO_ANTIALIAS_NONE);
end;

procedure TTMSFNCGraphicsContextUnix.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextUnix.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextUnix.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextUnix.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TTMSFNCGraphicsContextUnix.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextUnix.SetFontName(AName: string);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextUnix.SetFontSize(ASize: Integer);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextUnix.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextUnix.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextUnix.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextUnix.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextUnix.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextUnix.SetTextAngle(ARect: TRectF;
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
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;

    SaveContext;

    cairo_translate(FNCanvas, cx.X, cx.Y);
    cairo_rotate(FNCanvas, ar);

    w := Result.Width;
    h := Result.Height;
    c := Cos(ar);
    s := Sin(ar);

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextUnix.StartSpecialPen;
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextUnix.StopSpecialPen;
begin
  if not Assigned(FNCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextUnix.StrokeChanged(Sender: TObject);
begin
  if not Assigned(FNCanvas) then
     Exit;

  ApplyStroke;
end;

procedure TTMSFNCGraphicsContextUnix.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FNCanvas) then
    Exit;

  cairo_translate(FNCanvas, AX, AY);
end;

{$IFDEF FMXLIB}
procedure InitializeCairo;
begin
  if not CairoLoaded then
  begin
    CairoHandle := LoadLibrary(PChar(TTMSFNCUtils.AddBackslash(CairoDLLPath) + CairoLib));
    if (CairoHandle = 0) then
      Exit;

    cairo_set_source_rgb := GetProcAddress(CairoHandle, 'cairo_set_source_rgb');
    cairo_set_source_rgba := GetProcAddress(CairoHandle, 'cairo_set_source_rgba');
    cairo_set_line_width := GetProcAddress(CairoHandle, 'cairo_set_line_width');
    cairo_set_dash := GetProcAddress(CairoHandle, 'cairo_set_dash');
    cairo_append_path := GetProcAddress(CairoHandle, 'cairo_append_path');
    cairo_clip := GetProcAddress(CairoHandle, 'cairo_clip');
    cairo_new_path := GetProcAddress(CairoHandle, 'cairo_new_path');
    cairo_rectangle := GetProcAddress(CairoHandle, 'cairo_rectangle');
    cairo_copy_path := GetProcAddress(CairoHandle, 'cairo_copy_path');
    cairo_create := GetProcAddress(CairoHandle, 'cairo_create');
    cairo_destroy := GetProcAddress(CairoHandle, 'cairo_destroy');
    cairo_arc := GetProcAddress(CairoHandle, 'cairo_arc');
    cairo_stroke := GetProcAddress(CairoHandle, 'cairo_stroke');
    cairo_move_to := GetProcAddress(CairoHandle, 'cairo_move_to');
    cairo_new_sub_path := GetProcAddress(CairoHandle, 'cairo_new_sub_path');
    cairo_line_to := GetProcAddress(CairoHandle, 'cairo_line_to');
    cairo_pattern_create_linear := GetProcAddress(CairoHandle, 'cairo_pattern_create_linear');
    cairo_pattern_create_radial := GetProcAddress(CairoHandle, 'cairo_pattern_create_radial');
    cairo_pattern_add_color_stop_rgba := GetProcAddress(CairoHandle, 'cairo_pattern_add_color_stop_rgba');
    cairo_set_source := GetProcAddress(CairoHandle, 'cairo_set_source');
    cairo_fill := GetProcAddress(CairoHandle, 'cairo_fill');
    cairo_pattern_destroy := GetProcAddress(CairoHandle, 'cairo_pattern_destroy');
    cairo_close_path := GetProcAddress(CairoHandle, 'cairo_close_path');
    cairo_rotate := GetProcAddress(CairoHandle, 'cairo_rotate');
    cairo_select_font_face := GetProcAddress(CairoHandle, 'cairo_select_font_face');
    cairo_set_font_size := GetProcAddress(CairoHandle, 'cairo_set_font_size');
    cairo_text_extents := GetProcAddress(CairoHandle, 'cairo_text_extents');
    cairo_show_text := GetProcAddress(CairoHandle, 'cairo_show_text');
    cairo_save := GetProcAddress(CairoHandle, 'cairo_save');
    cairo_restore := GetProcAddress(CairoHandle, 'cairo_restore');
    cairo_scale := GetProcAddress(CairoHandle, 'cairo_scale');
    cairo_set_antialias := GetProcAddress(CairoHandle, 'cairo_set_antialias');
    cairo_translate := GetProcAddress(CairoHandle, 'cairo_translate');
    cairo_image_surface_create_for_data := GetProcAddress(CairoHandle, 'cairo_image_surface_create_for_data');
    cairo_image_surface_create := GetProcAddress(CairoHandle, 'cairo_image_surface_create');
    cairo_surface_destroy := GetProcAddress(CairoHandle, 'cairo_surface_destroy');
    cairo_image_surface_get_data := GetProcAddress(CairoHandle, 'cairo_image_surface_get_data');
    cairo_image_surface_get_width := GetProcAddress(CairoHandle, 'cairo_image_surface_get_width');
    cairo_image_surface_get_height := GetProcAddress(CairoHandle, 'cairo_image_surface_get_height');
    cairo_image_surface_get_format := GetProcAddress(CairoHandle, 'cairo_image_surface_get_format');
    cairo_format_stride_for_width := GetProcAddress(CairoHandle, 'cairo_format_stride_for_width');
    cairo_set_source_surface := GetProcAddress(CairoHandle, 'cairo_set_source_surface');
    cairo_paint := GetProcAddress(CairoHandle, 'cairo_paint');

    CairoLoaded := True;
  end;
end;

procedure UninitializeCairo;
begin
  if CairoLoaded then
  begin
    FreeLibrary(CairoHandle);
    CairoLoaded := false;
  end;
end;

initialization
  InitializeCairo;

finalization
  UninitializeCairo;
{$ENDIF}

{$ENDIF}
{$ENDIF}

end.
