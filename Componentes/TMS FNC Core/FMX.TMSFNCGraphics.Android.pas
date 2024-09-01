{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCGraphics.Android;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF ANDROID}

uses
  Classes, Androidapi.Jni.GraphicsContentViewText, AndroidApi.JNIBridge, AndroidApi.JNI.JavaTypes, FMX.Helpers.Android,
  AndroidApi.Helpers, FMX.Surfaces, FMX.TMSFNCGraphicsTypes, FMX.TMSFNCUtils, FMX.TMSFNCTypes, {%H-}Types, FMX.TMSFNCGraphics, FMX.Graphics
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TTMSFNCGraphicsContextAndroid = class(TTMSFNCGraphicsContext)
  private
    FPath: JPath;
    FActivePath: TTMSFNCGraphicsPath;
    FScale: Single;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FContextSize: TSizeF;
    FJCanvas: JCanvas;
    FJBitmap: JBitmap;
    FJPaint: JPaint;
  protected
    function GetNativeCanvas: Pointer; override;
    function DrawTextInternal(Text: String; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
    procedure DestroyResources;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure RestoreContext;
    procedure SaveContext;
    procedure ApplyStroke;
    procedure ApplyFill(ARect: TRectF);
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
    procedure SetNativeCanvas(ACanvas: JCanvas);
  end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  SysUtils, Math;

type
  JStaticLayout = interface;

  JStaticLayoutClass = interface(JLayoutClass)
  ['{6BBE70C3-EFF4-4F32-A0A8-378DA09BC60F}']
    function init(source: JCharSequence; paint: JTextPaint; width: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean): JStaticLayout; cdecl; overload;
    function init(source: JCharSequence; bufstart: Integer; bufend: Integer; paint: JTextPaint; outerwidth: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean): JStaticLayout; cdecl; overload;
    function init(source: JCharSequence; bufstart: Integer; bufend: Integer; paint: JTextPaint; outerwidth: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean; ellipsize: JTextUtils_TruncateAt; ellipsizedWidth: Integer): JStaticLayout; cdecl; overload;
  end;
  [JavaSignature('android/text/StaticLayout')]
  JStaticLayout = interface(JLayout)
  ['{0862BFE4-3D6D-4BC2-AA49-9E589EA2CB88}']
  end;
  TJStaticLayout = class(TJavaGenericImport<JStaticLayoutClass, JStaticLayout>) end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextAndroid;
end;

function TIntegerArrayToTJavaArray(const AArray: array of Integer): TJavaArray<Integer>;
var
  LLength: Integer;
  I: Integer;
begin
  LLength := Length(AArray);
  Result := TJavaArray<Integer>.Create(LLength);
  for I := 0 to LLength - 1 do
    Result.Items[I] := AArray[I];
end;

function TSingleArrayToTJavaArray(const AArray: array of Single): TJavaArray<Single>;
var
  LLength: Integer;
  I: Integer;
begin
  LLength := Length(AArray);
  Result := TJavaArray<Single>.Create(LLength);
  for I := 0 to LLength - 1 do
    Result.Items[I] := AArray[I];
end;

function ConvertToJColor(AColor: TTMSFNCGraphicsColor; AOpacity: Single): Integer;
var
  r, g, b, a: Integer;
begin
  if AColor = gcNull then
  begin
    Result := TJColor.JavaClass.TRANSPARENT;
    Exit;
  end;

  r := TAlphaColorRec(AColor).R;
  g := TAlphaColorRec(AColor).G;
  b := TAlphaColorRec(AColor).B;
  a := Round(AOpacity * 255);
  Result := TJColor.JavaClass.argb(a, r, g, b);
end;

{ TTMSFNCGraphicsContextAndroid }

procedure TTMSFNCGraphicsContextAndroid.ApplyFill(ARect: TRectF);
var
  shad: JShader;
  colors: TJavaArray<Integer>;
  positions: TJavaArray<single>;
  colorarr: array of Integer;
  positionsarr: array of Single;
  f0, f1: Boolean;
  I: Integer;
  o: Integer;
  a, sx, sy: single;
  c, pt0, pt1: TPointF;
  rd: Double;
  it: TTMSFNCGraphicsFillGradientItem;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if not Assigned(FJPaint) then
    Exit;

  shad := nil;

  case FFill.Kind of
    gfkSolid: FJPaint.setColor(ConvertToJColor(FFill.Color, FFill.Opacity));
    gfkGradient:
    begin
      SetLength(colorarr, 0);
      SetLength(positionsarr, 0);

      case FFill.GradientMode of
        gfgmDefault:
        begin
          if (FFill.ColorTo <> gcnull) and (FFill.ColorMirror <> gcNull) then
          begin
            SetLength(colorarr, 4);
            SetLength(positionsarr, 4);
            colorarr[0] := ConvertToJColor(FFill.Color, FFill.Opacity);
            colorarr[1] := ConvertToJColor(FFill.ColorTo, FFill.Opacity);
            colorarr[2] := ConvertToJColor(FFill.ColorMirror, FFill.Opacity);
            colorarr[3] := ConvertToJColor(FFill.ColorMirrorTo, FFill.Opacity);
            positionsarr[0] := 0;
            positionsarr[1] := 0.5;
            positionsarr[2] := 0.5;
            positionsarr[3] := 1;
          end
          else if FFill.ColorTo <> gcNull then
          begin
            SetLength(colorarr, 2);
            SetLength(positionsarr, 2);
            colorarr[0] := ConvertToJColor(FFill.Color, FFill.Opacity);
            colorarr[1] := ConvertToJColor(FFill.ColorTo, FFill.Opacity);
            positionsarr[0] := 0;
            positionsarr[1] := 1;
          end
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
            colorarr[0] := ConvertToJColor(FFill.GradientItems[0].Color, FFill.GradientItems[0].Opacity);
            SetLength(positionsarr, Length(positionsarr) + 1);
            positionsarr[0] := 0;
            o := 1;
          end;

          for I := 0 to FFill.GradientItems.Count - 1 do
          begin
            it := FFill.GradientItems[I];
            colorarr[I + o] := ConvertToJColor(it.Color, it.Opacity);
            positionsarr[I + o] := it.Position;
          end;

          if not f1 and (FFill.GradientItems.Count > 0) then
          begin
            SetLength(colorarr, Length(colorarr) + 1);
            colorarr[Length(colorarr) - 1] := ConvertToJColor(FFill.GradientItems[FFill.GradientItems.Count - 1].Color,
              FFill.GradientItems[FFill.GradientItems.Count - 1].Opacity);
            SetLength(positionsarr, Length(positionsarr) + 1);
            positionsarr[Length(positionsarr) - 1] := 1;
          end;
        end;
      end;

      colors := TIntegerArrayToTJavaArray(colorarr);
      positions := TSingleArrayToTJavaArray(positionsarr);

      case FFill.GradientType of
        gfgtLinear:
        begin
          case FFill.Orientation of
            gfoHorizontal: shad := TJLinearGradient.JavaClass.init(ARect.Left, ARect.Top, ARect.Right, ARect.Top, colors, positions, TJShader_TileMode.JavaClass.MIRROR);
            gfoVertical: shad := TJLinearGradient.JavaClass.init(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom, colors, positions, TJShader_TileMode.JavaClass.MIRROR);
            gfoCustom:
            begin
              a := DegToRad(FFill.GradientAngle);
              c := PointF(ARect.Left + ARect.Width / 2, ARect.Top + ARect.Height / 2);
              pt0 := PointF(c.x - Cos(a) * ARect.Width / 2, c.y - Sin(a) * ARect.height / 2);
              pt1 := PointF(c.x + Cos(a) * ARect.Width / 2, c.y + Sin(a) * ARect.height / 2);
              shad := TJLinearGradient.JavaClass.init(pt0.X, pt0.Y, pt1.X, pt1.Y, colors, positions, TJShader_TileMode.JavaClass.MIRROR);
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

          c := PointF(FFill.GradientCenterPoint.X, FFill.GradientCenterPoint.Y);

          rd := (ARect.Width / 2);
          if sy > sx then
            rd := (ARect.Height / 2);

          shad := TJRadialGradient.JavaClass.init(c.X, c.Y, rd, colors, positions, TJShader_TileMode.JavaClass.MIRROR);
        end;
      end;
    end;
  end;

  FJPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  FJPaint.setShader(shad);
end;

procedure TTMSFNCGraphicsContextAndroid.ApplyStroke;
var
  ja: TJavaArray<Single>;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if not Assigned(FJPaint) then
    Exit;

  FJPaint.setStrokeWidth(FStroke.Width);

  FJPaint.setColor(ConvertToJColor(FStroke.Color, FStroke.Opacity));
  case FStroke.Kind of
    gskSolid: FJPaint.setPathEffect(nil);
    gskDash:
    begin
      ja := TJavaArray<Single>.Create(2);
      ja.Items[0] := 4;
      ja.Items[1] := 2;
      FJPaint.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
    end;
    gskDot:
    begin
      ja := TJavaArray<Single>.Create(2);
      ja.Items[0] := 2;
      ja.Items[1] := 1;
      FJPaint.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
    end;
    gskDashDot:
    begin
      ja := TJavaArray<Single>.Create(4);
      ja.Items[0] := 4;
      ja.Items[1] := 2;
      ja.Items[2] := 1;
      ja.Items[3] := 2;
      FJPaint.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
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
      FJPaint.setPathEffect(TJDashPathEffect.JavaClass.init(ja, 0));
    end;
  end;
  FJPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
end;

procedure TTMSFNCGraphicsContextAndroid.BeginPrinting;
begin

end;

procedure TTMSFNCGraphicsContextAndroid.BeginScene;
begin
  if not Assigned(FJCanvas) then
    Exit;

  Canvas.BeginScene;
  Canvas.Clear(gcNull);
end;

function TTMSFNCGraphicsContextAndroid.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FJCanvas) then
    Exit;

  SaveContext;
  Result := DrawTextInternal(AText, ARect, AWordWrapping, gtaLeading, True);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextAndroid.ClipPath(APath: TTMSFNCGraphicsPath);
var
  pth: JPath;
begin
  if not Assigned(FJCanvas) then
    Exit;

  pth := TJPath.Wrap(ConvertToPath(APath));
  try
    SaveContext;
    FJCanvas.clipPath(pth);
  finally
    pth := nil;
    FPath := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.ClipRect(ARect: TRectF);
begin
  if not Assigned(FJCanvas) then
    Exit;

  SaveContext;
  FJCanvas.clipRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

constructor TTMSFNCGraphicsContextAndroid.Create(const AGraphics: TTMSFNCGraphics);
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

function TTMSFNCGraphicsContextAndroid.CreatePath: Pointer;
begin
  FPath := TJPath.JavaClass.init;
  Result := (FPath as ILocalObject).GetObjectID;
end;

destructor TTMSFNCGraphicsContextAndroid.Destroy;
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

procedure TTMSFNCGraphicsContextAndroid.DestroyResources;
begin
  if Assigned(FJBitmap) then
    FJBitmap := nil;

  if Assigned(FJPaint) then
    FJPaint := nil;

  if Assigned(FJCanvas) then
  begin
    RestoreContext;
    FJCanvas := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
var
  pth: TTMSFNCGraphicsPath;
begin
  if not Assigned(FJCanvas) then
    Exit;

  pth := TTMSFNCGraphicsPath.Create;
  try
    pth.AddArc(ACenter, ARadius, AStartAngle, ASweepAngle);
    DrawPath(AStroke, pth, pdmPolyline);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  surf: TBitmapSurface;
  bmp: JBitmap;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if not Assigned(ABitmap) then
    Exit;

  surf := TBitmapSurface.Create;
  try
    surf.Assign(ABitmap);
    bmp := TJBitmap.JavaClass.createBitmap(surf.Width, surf.Height, TJBitmap_Config.JavaClass.ARGB_8888);
    if SurfaceToJBitmap(surf, bmp) then
    begin
      FJCanvas.drawBitmap(bmp, TJRect.JavaClass.init(Round(ASrcRect.Left), Round(ASrcRect.Top), Round(ASrcRect.Right), Round(ASrcRect.Bottom)),
        TJRectF.JavaClass.init(ADstRect.Left, ADstRect.Top, ADstRect.Right, ADstRect.Bottom), FJPaint);
    end;
  finally
    surf.Free;
  end
end;

procedure TTMSFNCGraphicsContextAndroid.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FJCanvas) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  ApplyStroke;
  FJCanvas.drawOval(TJRectF.JavaClass.init(r.Left, r.Top, r.Right, r.Bottom), FJPaint);
end;

procedure TTMSFNCGraphicsContextAndroid.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FJCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextAndroid.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if not Assigned(FJCanvas) then
    Exit;

end;

procedure TTMSFNCGraphicsContextAndroid.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint, AToPoint: TPointF; AModifyPointModeFrom,
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FJCanvas) then
    Exit;

  ApplyStroke;
  FJCanvas.drawLine(AFromPoint.X, AFromPoint.Y, AToPoint.X, AToPoint.Y, FJPaint);
end;

procedure TTMSFNCGraphicsContextAndroid.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: JPath;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      pth := TJPath.Wrap(ConvertToPath(APath));
      try
        ApplyStroke;
        FJCanvas.drawPath(pth, FJPaint);
      finally
        pth := nil;
        FPath := nil;
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

procedure TTMSFNCGraphicsContextAndroid.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  pth: JPath;
  I: Integer;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  pth := TJPath.JavaClass.init;
  try
    pth.moveTo(APolygon[0].X, APolygon[0].Y);
    for I := 1 to Length(APolygon) - 1 do
      pth.lineTo(APolygon[I].X, APolygon[I].Y);
    pth.close;
    ApplyStroke;
    FJCanvas.drawPath(pth, FJPaint);
  finally
    pth := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  pth: JPath;
  I: Integer;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  pth := TJPath.JavaClass.init;
  try
    pth.moveTo(APolyline[0].X, APolyline[0].Y);
    for I := 1 to Length(APolyline) - 1 do
      pth.lineTo(APolyline[I].X, APolyline[I].Y);
    ApplyStroke;
    FJCanvas.drawPath(pth, FJPaint);
  finally
    pth := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.DrawRect(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FJCanvas) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom), PointF(r.Right, r.Bottom));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right, r.Top), PointF(r.Right, r.Bottom));
end;

procedure TTMSFNCGraphicsContextAndroid.DrawRoundRect(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single;
  ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
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

procedure TTMSFNCGraphicsContextAndroid.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TTMSFNCGraphicsTextAlign;
  ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single);
var
  R: TRectF;
begin
  if not Assigned(FJCanvas) then
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

procedure TTMSFNCGraphicsContextAndroid.EndPrinting;
begin

end;

procedure TTMSFNCGraphicsContextAndroid.EndScene;
begin
  if not Assigned(FJCanvas) then
    Exit;

  Render;
  Canvas.EndScene;
end;

procedure TTMSFNCGraphicsContextAndroid.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
var
  pth: TTMSFNCGraphicsPath;
begin
  if not Assigned(FJCanvas) then
    Exit;

  pth := TTMSFNCGraphicsPath.Create;
  try
    pth.AddArc(ACenter, ARadius, AStartAngle, ASweepAngle);
    FillPath(AFill, pth, pdmPolyline);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.FillChanged(Sender: TObject);
begin
  if not Assigned(FJCanvas) then
    Exit;

  ApplyFill(RectF(0, 0, FContextSize.Width, FContextSize.Height));
end;

procedure TTMSFNCGraphicsContextAndroid.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FJCanvas) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  ApplyFill(r);
  FJCanvas.drawOval(TJRectF.JavaClass.init(r.Left, r.Top, r.Right, r.Bottom), FJPaint);
end;

procedure TTMSFNCGraphicsContextAndroid.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: JPath;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      pth := TJPath.Wrap(ConvertToPath(APath));
      try
        ApplyFill(APath.GetBounds);
        FJCanvas.drawPath(pth, FJPaint);
      finally
        pth := nil;
        FPath := nil;
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

procedure TTMSFNCGraphicsContextAndroid.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  pth: JPath;
  I: Integer;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  pth := TJPath.JavaClass.init;
  try
    pth.moveTo(APolygon[0].X, APolygon[0].Y);
    for I := 1 to Length(APolygon) - 1 do
      pth.lineTo(APolygon[I].X, APolygon[I].Y);
    pth.close;
    if Assigned(FActivePath) then
      ApplyFill(FActivePath.GetBounds);
    FJCanvas.drawPath(pth, FJPaint);
  finally
    pth := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  pth: JPath;
  I: Integer;
begin
  if not Assigned(FJCanvas) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  pth := TJPath.JavaClass.init;
  try
    pth.moveTo(APolyline[0].X, APolyline[0].Y);
    for I := 1 to Length(APolyline) - 1 do
      pth.lineTo(APolyline[I].X, APolyline[I].Y);
    if Assigned(FActivePath) then
      ApplyFill(FActivePath.GetBounds);
    FJCanvas.drawPath(pth, FJPaint);
  finally
    pth := nil;
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FJCanvas) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  ApplyFill(r);
  FJCanvas.drawRect(r.Left, r.Top, r.Right, r.Bottom, FJPaint);
end;

procedure TTMSFNCGraphicsContextAndroid.FillRoundRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
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

procedure TTMSFNCGraphicsContextAndroid.FontChanged(Sender: TObject);
begin

end;

function TTMSFNCGraphicsContextAndroid.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextAndroid.GetMatrix: TTMSFNCGraphicsMatrix;
var
  m: JMatrix;
  a: TJavaArray<Single>;
begin
  if not Assigned(FJCanvas) then
    Exit;

  m := TJMatrix.JavaClass.init;
  a := TJavaArray<Single>.Create(9);
  FJCanvas.getMatrix(m);
  m.getValues(a);

  Result.m11 := a[TJMatrix.JavaClass.MSCALE_X];
  Result.m21 := a[TJMatrix.JavaClass.MSKEW_X];
  Result.m31 := a[TJMatrix.JavaClass.MTRANS_X];
  Result.m12 := a[TJMatrix.JavaClass.MSKEW_Y];
  Result.m22 := a[TJMatrix.JavaClass.MSCALE_Y];
  Result.m32 := a[TJMatrix.JavaClass.MTRANS_Y];
  Result.m13 := a[TJMatrix.JavaClass.MPERSP_0];
  Result.m23 := a[TJMatrix.JavaClass.MPERSP_1];
  Result.m33 := a[TJMatrix.JavaClass.MPERSP_2];
end;

function TTMSFNCGraphicsContextAndroid.GetNativeCanvas: Pointer;
begin
  Result := (FJCanvas as ILocalObject).GetObjectID;
end;

procedure TTMSFNCGraphicsContextAndroid.PathClose(APath: Pointer);
begin
  TJPath.Wrap(APath).close;
end;

procedure TTMSFNCGraphicsContextAndroid.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  TJPath.Wrap(APath).lineTo(APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextAndroid.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  TJPath.Wrap(APath).moveTo(APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextAndroid.PathOpen(APath: Pointer);
begin
end;

procedure TTMSFNCGraphicsContextAndroid.Render;
var
  surf: TBitmapSurface;
  bmp: TBitmap;
begin
  if not FNeedsRendering then
    Exit;

  if Assigned(FJCanvas) and Assigned(Canvas) then
  begin
    surf := TBitmapSurface.Create;
    bmp := TBitmap.Create;
    try
      JBitmapToSurface(FJBitmap, surf);
      bmp.Assign(surf);
      Canvas.DrawBitmap(bmp, RectF(0, 0, bmp.Width, bmp.Height), RectF(0, 0, FContextSize.Width, FContextSize.Height), 1);
    finally
      bmp.Free;
      surf.Free;
    end;
  end;

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextAndroid.ResetClip;
begin
  if not Assigned(FJCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextAndroid.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FJCanvas) then
    Exit;

  if AAngle <> 0 then
    RestoreContext;
end;

procedure TTMSFNCGraphicsContextAndroid.ResetTransform;
begin
  if not Assigned(FJCanvas) then
    Exit;

  RestoreContext;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextAndroid.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FJCanvas) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextAndroid.RotateTransform(AAngle: Single);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FJCanvas.rotate(AAngle);
end;

function TTMSFNCGraphicsContextAndroid.DrawTextInternal(Text: string; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
var
  c: JCanvas;
  tp: JTextPaint;
  sl: JStaticLayout;
  al: JLayout_Alignment;
  resw, resh: Single;
  flg: Integer;
  mw: TJavaArray<Single>;
const
  MAXTEXTB = 10000;
begin
  c := FJCanvas;
  if Assigned(c) then
  begin
    tp := TJTextPaint.JavaClass.init;
    tp.setAntiAlias(True);
    tp.setColor(ConvertToJColor(FFont.Color, 1));

    if (TFontStyle.fsBold in FFont.Style) and (TFontStyle.fsItalic in FFont.Style) then
      tp.setTypeface(TJTypeface.JavaClass.create(StringToJString(FFont.Name), TJTypeface.JavaClass.BOLD_ITALIC))
    else if TFontStyle.fsBold in FFont.Style then
      tp.setTypeface(TJTypeface.JavaClass.create(StringToJString(FFont.Name), TJTypeface.JavaClass.BOLD))
    else if TFontStyle.fsItalic in FFont.Style then
      tp.setTypeface(TJTypeface.JavaClass.create(StringToJString(FFont.Name), TJTypeface.JavaClass.ITALIC))
    else
      tp.setTypeface(TJTypeface.JavaClass.create(StringToJString(FFont.Name), TJTypeface.JavaClass.NORMAL));

    tp.setTextSize(FFont.Size);

    flg := 0;
    if TFontStyle.fsUnderline in FFont.Style then
      flg := flg or TJPaint.JavaClass.UNDERLINE_TEXT_FLAG;

    if TFontStyle.fsStrikeOut in FFont.Style then
      flg := flg or TJPaint.JavaClass.STRIKE_THRU_TEXT_FLAG;

    tp.setFlags(flg);

    case AHorizontalAlignment of
      gtaLeading: al := TJLayout_Alignment.JavaClass.ALIGN_NORMAL;
      gtaCenter: al := TJLayout_Alignment.JavaClass.ALIGN_CENTER;
      gtaTrailing: al := TJLayout_Alignment.JavaClass.ALIGN_OPPOSITE;
    end;

    if AWordWrap then
      sl := TJStaticLayout.JavaClass.init(StrToJCharSequence(Text), tp, Round(Min(MAXTEXTB, Rect.Width)), al, 1.0, 0.0, False)
    else if Calculate then
      sl := TJStaticLayout.JavaClass.init(StrToJCharSequence(Text), tp, MAXTEXTB, al, 1.0, 0.0, False)
    else
      sl := TJStaticLayout.JavaClass.init(StrToJCharSequence(Text), tp, Round(Min(MAXTEXTB, Rect.Width)), al, 1.0, 0.0, False);

    if not Calculate then
    begin
      c.save;
      c.clipRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      c.translate(Rect.Left, Rect.Top);
      sl.draw(c);
      c.restore;
    end;

    mw := TJavaArray<Single>.Create(1);
    if AWordWrap then
      tp.breakText(StringToJString(Text), True, Rect.Width, mw)
    else
      tp.breakText(StringToJString(Text), True, MAXTEXTB, mw);

    resw := mw[0];
    resh := sl.getHeight;
    Result := RectF(Rect.Left, Rect.Top, Rect.Left + resw, Rect.Top + resh);
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.SaveContext;
begin
  if not Assigned(FJCanvas) then
    Exit;

  FJCanvas.save;
end;

procedure TTMSFNCGraphicsContextAndroid.RestoreContext;
begin
  if not Assigned(FJCanvas) then
    Exit;

  FJCanvas.restore;
end;

procedure TTMSFNCGraphicsContextAndroid.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FJCanvas) then
    Exit;

  SaveContext;
end;

procedure TTMSFNCGraphicsContextAndroid.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FJCanvas.scale(AX, AY);
end;

procedure TTMSFNCGraphicsContextAndroid.SetSize(AWidth, AHeight: Single);
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  FJBitmap := TJBitmap.JavaClass.createBitmap(Round(AWidth * FScale), Round(AHeight * FScale), TJBitmap_Config.JavaClass.ARGB_8888);
  FJCanvas := TJCanvas.JavaClass.init(FJBitmap);
  FJPaint := TJPaint.JavaClass.init;
  ScaleTransform(FScale, FScale);
  SaveContext;
end;

procedure TTMSFNCGraphicsContextAndroid.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
end;

procedure TTMSFNCGraphicsContextAndroid.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
end;

procedure TTMSFNCGraphicsContextAndroid.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FJCanvas) then
    Exit;

  if not Assigned(FJPaint) then
    Exit;

  FJPaint.setAntiAlias(AAntiAliasing);
end;

procedure TTMSFNCGraphicsContextAndroid.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextAndroid.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextAndroid.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextAndroid.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TTMSFNCGraphicsContextAndroid.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextAndroid.SetFontName(AName: string);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextAndroid.SetFontSize(ASize: Integer);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextAndroid.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextAndroid.SetScale(AScale: Single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextAndroid.SetMatrix(
  AMatrix: TTMSFNCGraphicsMatrix);
var
  m: JMatrix;
  a: TJavaArray<Single>;
begin
  if not Assigned(FJCanvas) then
    Exit;

  m := TJMatrix.JavaClass.init;
  a := TJavaArray<Single>.Create(9);
  a[TJMatrix.JavaClass.MSCALE_X] := AMatrix.m11;
  a[TJMatrix.JavaClass.MSKEW_X] := AMatrix.m21;
  a[TJMatrix.JavaClass.MTRANS_X] := AMatrix.m31;
  a[TJMatrix.JavaClass.MSKEW_Y] := AMatrix.m12;
  a[TJMatrix.JavaClass.MSCALE_Y] := AMatrix.m22;
  a[TJMatrix.JavaClass.MTRANS_Y] := AMatrix.m32;
  a[TJMatrix.JavaClass.MPERSP_0] := AMatrix.m13;
  a[TJMatrix.JavaClass.MPERSP_1] := AMatrix.m23;
  a[TJMatrix.JavaClass.MPERSP_2] := AMatrix.m33;

  m.setValues(a);
  FJCanvas.setMatrix(M);
end;

procedure TTMSFNCGraphicsContextAndroid.SetNativeCanvas(ACanvas: JCanvas);
begin
  if Assigned(FJCanvas) then
    FJCanvas.restore;
  FJCanvas := ACanvas;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextAndroid.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextAndroid.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextAndroid.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextAndroid.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextAndroid.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  m: JMatrix;
  w, h, c, s, cw, ch: Single;
begin
  Result := ARect;
  if not Assigned(FJCanvas) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := AAngle;
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;

    SaveContext;
    m := TJMatrix.JavaClass.init;
    m.setTranslate(cx.X, cx.Y);
    m.preRotate(ar);
    FJCanvas.concat(m);

    w := Result.Width;
    h := Result.Height;
    c := Cos(DegToRad(ar));
    s := Sin(DegToRad(ar));

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextAndroid.StartSpecialPen;
begin
  if not Assigned(FJCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextAndroid.StopSpecialPen;
begin
  if not Assigned(FJCanvas) then
    Exit;
end;

procedure TTMSFNCGraphicsContextAndroid.StrokeChanged(Sender: TObject);
begin
  if not Assigned(FJCanvas) then
     Exit;

  ApplyStroke;
end;

procedure TTMSFNCGraphicsContextAndroid.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FJCanvas) then
    Exit;

  FJCanvas.translate(AX, AY);
end;

{$ENDIF}

end.
