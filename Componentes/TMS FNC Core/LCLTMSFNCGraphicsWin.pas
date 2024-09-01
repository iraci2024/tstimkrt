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

unit LCLTMSFNCGraphicsWin;

{$I LCLTMSFNCDefines.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Classes, Windows, ActiveX, LCLTMSFNCGDIPlusApi, LCLTMSFNCGraphicsTypes,
  {%H-}Types, LCLTMSFNCGraphics, Graphics, LCLTMSFNCUtils, LCLTMSFNCTypes
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ,LCLTMSFNCGDIPlusClasses
  ;
{$ENDIF}

{$IFNDEF LCLLIB}
const
  FPC_FULLVERSION = 0;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TTMSFNCGraphicsContextWin = class(TTMSFNCGraphicsContext)
  private
    FShowAcceleratorChar: Boolean;
    FTextMatrix: TGPMatrix;
    FSmoothingMode: SmoothingMode;
    FSaveGP: Cardinal;
    FScale: Single;
    FMovePoint: TPointF;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FGPFont: TGPFont;
    FContextSize: TSizeF;
    FGPStringFormat: TGPStringFormat;
    FGPBrush: TGPBrush;
    FGPPen: TGPPen;
    FGPGraphics: TGPGraphics;
    FGPBitmap: TGPBitmap;
    FBitmap: TBitmap;
    {$IFDEF FMXLIB}
    FMapping: Boolean;
    FBitmapData: TBitmapData;
    {$ENDIF}
  protected
    function GetNativeCanvas: Pointer; override;
    procedure DestroyResources;
    procedure ApplyFill(APath: TGPGraphicsPath);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure SetSmoothingMode(ASmoothingMode: SmoothingMode);
    procedure RestoreSmoothingMode;
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
    procedure SetScale(AScale: Single); override;
    procedure ScaleTransform(AX, AY: Single); override;
    procedure RotateTransform(AAngle: Single); override;
    procedure TranslateTransform(AX, AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TTMSFNCGraphicsTextQuality); override;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetSize(AWidth, AHeight: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure SetMatrix(AMatrix: TTMSFNCGraphicsMatrix); override;
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

implementation

{$IFDEF MSWINDOWS}

uses
  Math, SysUtils;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextWin;
end;

function ConvertToGDIPColor(AColor: TTMSFNCGraphicsColor; AOpacity: Single): Cardinal;
begin
  Result := MakeColor(Round(AOpacity * 255), TTMSFNCGraphics.GetColorRed(AColor), TTMSFNCGraphics.GetColorGreen(AColor), TTMSFNCGraphics.GetColorBlue(AColor));
end;

function CreateGDIPFontFill(AFont: TTMSFNCGraphicsFont): TGPBrush;
begin
  Result := TGPSolidBrush.Create(ConvertToGDIPColor(AFont.Color, 1));
end;

function CreateGDIPFont(AFont: TTMSFNCGraphicsFont): TGPFont;
var
  style: integer;
  fn: string;
  sz: Single;
begin
  fn := AFont.Name;
  if (fn = '') or (UpperCase(fn) = 'DEFAULT') then
    fn := 'Arial';

  sz := AFont.Size;
  if sz = 0 then
    sz := 8;

  style := FontStyleRegular;
  if TFontStyle.fsBold in AFont.Style then
    style := style + FontStyleBold;
  if TFontStyle.fsItalic in AFont.Style then
    style := style + FontStyleItalic;
  if TFontStyle.fsUnderline in AFont.Style then
    style := style + FontStyleUnderline;
  if TFontStyle.fsStrikeOut in AFont.Style then
    style := style + FontStyleStrikeout;

  {$IFDEF FMXLIB}
  Result := TGPFont.Create(PChar(fn), sz, style, UnitPixel);
  {$ENDIF}
  {$IFDEF CMNLIB}
  Result := TGPFont.Create(PChar(fn), sz, style, UnitPoint);
  {$ENDIF}
end;

function CreateGDIPPen(AStroke: TTMSFNCGraphicsStroke): TGPPen;
begin
  Result := TGPPen.Create(ConvertToGDIPColor(AStroke.Color, AStroke.Opacity), AStroke.Width);
  case AStroke.Kind of
    gskSolid: Result.SetDashStyle(DashStyleSolid);
    gskDash: Result.SetDashStyle(DashStyleDash);
    gskDot: Result.SetDashStyle(DashStyleDot);
    gskDashDot: Result.SetDashStyle(DashStyleDashDot);
    gskDashDotDot: Result.SetDashStyle(DashStyleDashDotDot);
  end;

  Result.SetLineCap(LineCapFlat, LineCapFlat, DashCapFlat);
  Result.SetLineJoin(LineJoinMiterClipped);
  Result.SetMiterLimit(4);
end;

function CreateGPStringFormat(AHAlign: TTMSFNCGraphicsTextAlign; AVAlign: TTMSFNCGraphicsTextAlign; AWrap: Boolean; ATrimming: TTMSFNCGraphicsTextTrimming; AShowAcceleratorChar: Boolean): TGPStringFormat;
var
  flags: integer;
begin
  Result := TGPStringFormat.Create;
  case AHAlign of
    gtaLeading:
      Result.SetAlignment(StringAlignmentNear);
    gtaTrailing:
      Result.SetAlignment(StringAlignmentFar);
    gtaCenter:
      Result.SetAlignment(StringAlignmentCenter);
  end;
  case AValign of
    gtaLeading:
      Result.SetLineAlignment(StringAlignmentNear);
    gtaTrailing:
      Result.SetLineAlignment(StringAlignmentFar);
    gtaCenter:
      Result.SetLineAlignment(StringAlignmentCenter);
  end;

  flags := StringFormatFlagsNoClip;
  if not AWrap then
    flags := flags or StringFormatFlagsNoWrap;

  Result.SetFormatFlags(flags);

  case ATrimming of
    gttCharacter: Result.SetTrimming(StringTrimmingCharacter);
    gttWord: Result.SetTrimming(StringTrimmingWord);
  end;

  if AShowAcceleratorChar then
    Result.SetHotkeyPrefix(HotkeyPrefixShow)
  else
    Result.SetHotkeyPrefix(HotkeyPrefixNone);
end;

function CreateGDIPBrush(AFill: TTMSFNCGraphicsFill; APath: TGPGraphicsPath): TGPBrush;
var
  cnt: Integer;
  positions, rpositions: array of Single;
  colors, rcolors: array of TGPColor;
  r: TGPRectF;
  I: Integer;
  it: TTMSFNCGraphicsFillGradientItem;
  f0, f1: Boolean;
  o: Integer;
  M, MS: TGPMatrix;
  mm: TTMSFNCGraphicsMatrix;
begin
  if Assigned(APath) then
    APath.GetBounds(r);

  Result := nil;
  case AFill.Kind of
    gfkSolid: Result := TGPSolidBrush.Create(ConvertToGDIPColor(AFill.Color, AFill.Opacity));
    gfkTexture, gfkNone: Result := TGPSolidBrush.Create(ConvertToGDIPColor(gcNull, 0));
    gfkGradient:
    begin
      r := MakeRect(r.X - 1, r.Y - 1, r.Width + 2, r.Height + 2);

      case AFill.GradientType of
        gfgtLinear:
        begin
          case AFill.Orientation of
            gfoHorizontal: Result := TGPLinearGradientBrush.Create(r, 0, 0, LinearGradientModeHorizontal);
            gfoVertical: Result := TGPLinearGradientBrush.Create(r, 0, 0, LinearGradientModeVertical);
            gfoCustom: Result := TGPLinearGradientBrush.Create(r, 0, 0, AFill.GradientAngle);
          end;
        end;
        gfgtRadial:
        begin
          Result := TGPPathGradientBrush.Create(APath);
          if AFill.GradientCenterColor <> gcNull then
            TGPPathGradientBrush(Result).SetCenterColor(ConvertToGDIPColor(AFill.GradientCenterColor, AFill.Opacity));

          TGPPathGradientBrush(Result).SetCenterPoint(MakePoint(AFill.GradientCenterPoint.X, AFill.GradientCenterPoint.Y));
        end;
      end;

      case AFill.GradientMode of
        gfgmDefault:
        begin
          if (AFill.ColorMirror <> gcNull) and (AFill.ColorMirrorTo <> gcNull) then
          begin
            SetLength(colors, 4);
            colors[0] := ConvertToGDIPColor(AFill.Color, AFill.Opacity);
            colors[1] := ConvertToGDIPColor(AFill.ColorTo, AFill.Opacity);
            colors[2] := ConvertToGDIPColor(AFill.ColorMirror, AFill.Opacity);
            colors[3] := ConvertToGDIPColor(AFill.ColorMirrorTo, AFill.Opacity);

            SetLength(positions, 4);
            positions[0] := 0.0;
            positions[1] := 0.5;
            positions[2] := 0.5;
            positions[3] := 1;
          end
          else
          begin
            SetLength(colors, 2);
            colors[0] := ConvertToGDIPColor(AFill.Color, AFill.Opacity);
            colors[1] := ConvertToGDIPColor(AFill.ColorTo, AFill.Opacity);

            SetLength(positions, 2);
            positions[0] := 0.0;
            positions[1] := 1;
          end;
        end;
        gfgmCollection:
        begin
          f0 := False;
          f1 := False;
          for I := 0 to AFill.GradientItems.Count - 1 do
          begin
            it := AFill.GradientItems[I];
            if it.Position = 0.0 then
              f0 := True;

            if it.Position = 1.0 then
              f1 := True;
          end;

          SetLength(colors, AFill.GradientItems.Count);
          SetLength(positions, AFill.GradientItems.Count);

          o := 0;
          if not f0 and (AFill.GradientItems.Count > 0) then
          begin
            SetLength(colors, Length(colors) + 1);
            colors[0] := AFill.GradientItems[0].Color;
            SetLength(positions, Length(positions) + 1);
            positions[0] := 0;
            o := 1;
          end;

          for I := 0 to AFill.GradientItems.Count - 1 do
          begin
            it := AFill.GradientItems[I];
            colors[I + o] := ConvertToGDIPColor(it.Color, it.Opacity);
            positions[I + o] := it.Position;
          end;

          if not f1 and (AFill.GradientItems.Count > 0) then
          begin
            SetLength(colors, Length(colors) + 1);
            colors[Length(colors) - 1] := AFill.GradientItems[AFill.GradientItems.Count - 1].Color;
            SetLength(positions, Length(positions) + 1);
            positions[Length(positions) - 1] := 1;
          end;
        end;
      end;

      mm := AFill.GradientMatrix;
      M := TGPMatrix.Create(mm.m11, mm.m12, mm.m21, mm.m22, mm.m31, mm.m32);
      MS := TGPMatrix.Create;
      try
        cnt := Length(colors);
        if Result is TGPLinearGradientBrush then
        begin
          if cnt > 0 then
          begin
            TGPLinearGradientBrush(Result).SetInterpolationColors(@colors[0], @positions[0], cnt);
            TGPLinearGradientBrush(Result).GetTransform(MS);
            MS.Multiply(M);
            TGPLinearGradientBrush(Result).SetTransform(MS);
          end;
        end
        else if Result is TGPPathGradientBrush then
        begin
          SetLength(rcolors, Length(colors));
          SetLength(rpositions, Length(positions));
          for I := 0 to Length(colors) - 1 do
            rcolors[I] := colors[Length(colors) - 1 - I];

          for I := 0 to Length(positions) - 1 do
            rpositions[I] := 1 - positions[Length(positions) - I - 1];

          TGPPathGradientBrush(Result).SetInterpolationColors(@rcolors[0], @rpositions[0], cnt);
          TGPLinearGradientBrush(Result).GetTransform(MS);
          MS.Multiply(M);
          TGPPathGradientBrush(Result).SetTransform(M);
        end;
      finally
        M.Free;
        MS.Free;
      end;
    end;
  end;
end;

{ TTMSFNCGraphicsContextWin }

procedure TTMSFNCGraphicsContextWin.BeginScene;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  {$IFDEF FMXLIB}
  Canvas.BeginScene;
  Canvas.Clear(gcNull);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Canvas.Clear;
  {$ENDIF}
end;

function TTMSFNCGraphicsContextWin.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
var
  b: TGPBrush;
  sf: TGPStringFormat;
  rt, bb: TGPRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FGPGraphics) then
    Exit;

  b := CreateGDIPFontFill(FFont);
  sf := CreateGPStringFormat(gtaLeading, gtaLeading, AWordWrapping, gttNone, FShowAcceleratorChar);
  try
    rt := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
    FGPGraphics.MeasureString(PChar(AText), Length(AText), FGPFont, rt, sf, bb);
    Result := RectF(bb.X, bb.Y, bb.X + bb.Width, bb.Y + bb.Height);
  finally
    sf.Free;
    b.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.ClipPath(APath: TTMSFNCGraphicsPath);
var
  pth: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  pth := TGPGraphicsPath(ConvertToPath(APath));
  try
    FGPGraphics.SetClip(pth);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.ClipRect(ARect: TRectF);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.SetClip(MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
end;

constructor TTMSFNCGraphicsContextWin.Create(const AGraphics: TTMSFNCGraphics);
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

function TTMSFNCGraphicsContextWin.CreatePath: Pointer;
begin
  Result := TGPGraphicsPath.Create;
end;

destructor TTMSFNCGraphicsContextWin.Destroy;
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

  if Assigned(FGPPen) then
  begin
    FGPPen.Free;
    FGPPen := nil;
  end;

  if Assigned(FGPBrush) then
  begin
    FGPBrush.Free;
    FGPBrush := nil;
  end;

  if Assigned(FGPFont) then
  begin
    FGPFont.Free;
    FGPFont := nil;
  end;

  if Assigned(FGPStringFormat) then
  begin
    FGPStringFormat.Free;
    FGPStringFormat := nil;
  end;

  DestroyResources;
  inherited;
end;

procedure TTMSFNCGraphicsContextWin.DestroyResources;
begin
  if Assigned(FGPBitmap) then
  begin
    FGPBitmap.Free;
    FGPBitmap := nil;
  end;

  if Assigned(FGPGraphics) then
  begin
    FGPGraphics.Restore(FSaveGP);
    FGPGraphics.Free;
    FGPGraphics := nil;
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

procedure TTMSFNCGraphicsContextWin.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawArc(FGPPen, ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ARadius.X * 2, ARadius.Y * 2, AStartAngle, ASweepAngle);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TTMSFNCGraphicsContextWin.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  img: TGPImage;

  function CreateGPImage: TGPImage;
  var
    pstm: IStream;
    pcbWrite: Longint;
    {$HINTS OFF}
    {$WARNINGS OFF}
    {$IFNDEF LCLLIB}
    {$IF COMPILERVERSION > 28}
    aSize: LargeUint;
    {$IFEND}
    {$IF COMPILERVERSION <= 28}
    aSize: Largeint;
    {$IFEND}
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IF FPC_FULLVERSION < 30002}
    aSize: Int64;
    {$IFEND}
    {$IF FPC_FULLVERSION >= 30002}
    aSize: QWord;
    {$IFEND}
    {$ENDIF}
    {$HINTS ON}
    {$WARNINGS ON}
    TempImage: TGPImage;
    TempGraphics: TGPGraphics;
    hglobal: THandle;
    DataStream: TMemoryStream;
  begin
    Result := nil;
    if Assigned(ABitmap) then
    begin
      DataStream := TMemoryStream.Create;
      ABitmap.SaveToStream(DataStream);
      try
        DataStream.Position := 0;

        hglobal := GlobalAlloc(GMEM_MOVEABLE, DataStream.Size);
        if (hglobal = 0) then
          raise Exception.Create('Could not allocate memory for image');
        try

          pstm := nil;
          // Create IStream* from global memory
          CreateStreamOnHGlobal(hglobal, TRUE, pstm);
      {$WARNINGS OFF}
          pstm.Write(DataStream.Memory, DataStream.Size, @pcbWrite);
      {$WARNINGS ON}
          pstm.Seek(0, STREAM_SEEK_SET, aSize);

          TempImage := TGPImage.Create(pstm);
          if TempImage.GetType = ImageTypeBitmap then
          begin
            Result := TGPBitmap.Create(TempImage.GetWidth, TempImage.GetHeight,
              PixelFormat32bppARGB);
            TempGraphics := TGPGraphics.Create(Result);
            TempGraphics.DrawImage(TempImage, 0, 0, TempImage.GetWidth,
              TempImage.GetHeight);
            TempGraphics.Free;
            TempImage.Free;
          end
          else
            Result := TempImage;

        finally
          GlobalFree(hglobal);
        end;
      finally
        DataStream.Free;
      end;
    end;
  end;
begin
  if not Assigned(ABitmap) or not Assigned(FGPGraphics) then
    Exit;

  img := CreateGPImage;
  try
    FGPGraphics.DrawImage(img, MakeRect(ADstRect.Left, ADstRect.Top, ADstRect.Width, ADstRect.Height));
  finally
    img.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawEllipse(FGPPen, r.Left, r.Top, r.Width, r.Height);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TTMSFNCGraphicsContextWin.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
var
  ds: DashStyle;
  c: TGPColor;
  smt: SmoothingMode;
  w: Single;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  smt := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  ds := FGPPen.GetDashStyle;
  w := FGPPen.GetWidth;
  FGPPen.GetColor(c);
  FGPPen.SetDashStyle(DashStyleDot);
  FGPPen.SetWidth(1);
  FGPPen.SetColor(ConvertToGDIPColor(AColor, 1));
  DrawPath(AStroke, APath);
  FGPPen.SetWidth(w);
  FGPPen.SetDashStyle(ds);
  FGPPen.SetColor(c);
  FGPGraphics.SetSmoothingMode(smt);
end;

procedure TTMSFNCGraphicsContextWin.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  ds: DashStyle;
  c: TGPColor;
  smt: SmoothingMode;
  w: Single;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  smt := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  ds := FGPPen.GetDashStyle;
  w := FGPPen.GetWidth;
  FGPPen.GetColor(c);
  FGPPen.SetDashStyle(DashStyleDot);
  FGPPen.SetWidth(1);
  FGPPen.SetColor(ConvertToGDIPColor(AColor, 1));
  DrawRect(AStroke, ARect, AllSides, AModifyRectMode);
  FGPPen.SetWidth(w);
  FGPPen.SetDashStyle(ds);
  FGPPen.SetColor(c);
  FGPGraphics.SetSmoothingMode(smt);
end;

procedure TTMSFNCGraphicsContextWin.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint, AToPoint: TPointF; AModifyPointModeFrom,
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawLine(FGPPen, AFromPoint.X, AFromPoint.Y, AToPoint.X, AToPoint.Y);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TTMSFNCGraphicsContextWin.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: TGPGraphicsPath;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      if not Assigned(FGPGraphics) then
        Exit;

      if FGPPen.GetDashStyle <> DashStyleSolid then
        SetSmoothingMode(SmoothingModeDefault);

      pth := TGPGraphicsPath(ConvertToPath(APath));
      try
        FGPGraphics.DrawPath(FGPPen, pth);
      finally
        pth.Free;
      end;

      if FGPPen.GetDashStyle <> DashStyleSolid then
        RestoreSmoothingMode;
    end
    else
    begin
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: DrawPolygon(AStroke, p);
        pdmPolyline: DrawPolyline(AStroke, p);
      end;
    end;
  end;
end;

procedure TTMSFNCGraphicsContextWin.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawPolygon(FGPPen, PGPPointF(APolygon), Length(APolygon));

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TTMSFNCGraphicsContextWin.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawLines(FGPPen, PGPPointF(APolyline), Length(APolyline));

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TTMSFNCGraphicsContextWin.DrawRect(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
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

procedure TTMSFNCGraphicsContextWin.DrawRoundRect(
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

procedure TTMSFNCGraphicsContextWin.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TTMSFNCGraphicsTextAlign;
  ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single);
var
  b: TGPBrush;
  sf: TGPStringFormat;
  rt: TGPRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  b := CreateGDIPFontFill(FFont);
  sf := CreateGPStringFormat(AHorizontalAlign, AVerticalAlign, AWordWrapping, ATrimming, FShowAcceleratorChar);
  try
    rt := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
    FGPGraphics.DrawString(AText, FGPFont, rt, sf, b);
  finally
    sf.Free;
    b.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.EndScene;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  Render;
  {$IFDEF FMXLIB}
  Canvas.EndScene;
  {$ENDIF}
end;

procedure TTMSFNCGraphicsContextWin.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
var
  p: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    p.AddArc(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ARadius.X * 2, ARadius.Y * 2, AStartAngle, ASweepAngle);
    ApplyFill(p);
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillChanged(Sender: TObject);
var
  p: TGPGraphicsPath;
begin
  p := TGPGraphicsPath.Create;
  try
    p.AddRectangle(MakeRect(0, 0, FContextSize.Width, FContextSize.Height));
    ApplyFill(p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
  p: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    p.AddEllipse(r.Left, r.Top, r.Width, r.Height);
    ApplyFill(p);
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: TGPGraphicsPath;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      if not Assigned(FGPGraphics) then
        Exit;

      pth := TGPGraphicsPath(ConvertToPath(APath));
      try
        ApplyFill(pth);
        FGPGraphics.FillPath(FGPBrush, pth);
      finally
        pth.Free;
      end;
    end
    else
    begin
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: FillPolygon(AFill, p);
        pdmPolyline: FillPolyline(AFill, p);
      end;
    end;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  p: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    p.AddPolygon(PGPPointF(APolygon), Length(APolygon));
    ApplyFill(p);
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  p: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    p.AddPolygon(PGPPointF(APolyline), Length(APolyline));
    ApplyFill(p);
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
  p: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    p.AddRectangle(MakeRect(r.Left, r.Top, r.Width, r.Height));
    ApplyFill(p);
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.FillRoundRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
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

procedure TTMSFNCGraphicsContextWin.ApplyFill(APath: TGPGraphicsPath);
begin
  if Assigned(FGPBrush) then
  begin
    FGPBrush.Free;
    FGPBrush := nil;
  end;

  FGPBrush := CreateGDIPBrush(FFill, APath);
end;

procedure TTMSFNCGraphicsContextWin.FontChanged(Sender: TObject);
begin
  if Assigned(FGPFont) then
  begin
    FGPFont.Free;
    FGPFont := nil;
  end;

  FGPFont := CreateGDIPFont(FFont);
end;

function TTMSFNCGraphicsContextWin.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextWin.GetNativeCanvas: Pointer;
begin
  Result := FGPGraphics;
end;

procedure TTMSFNCGraphicsContextWin.PathClose(APath: Pointer);
begin
  TGPGraphicsPath(APath).CloseFigure;
end;

procedure TTMSFNCGraphicsContextWin.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  TGPGraphicsPath(APath).AddLine(FMovePoint.X, FMovePoint.Y, APoint.X, APoint.Y);
  FMovePoint := APoint;
end;

procedure TTMSFNCGraphicsContextWin.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  FMovePoint := APoint;
end;

procedure TTMSFNCGraphicsContextWin.PathOpen(APath: Pointer);
begin
  TGPGraphicsPath(APath).StartFigure;
end;

procedure TTMSFNCGraphicsContextWin.Render;
begin
  if not FNeedsRendering then
    Exit;

  {$IFDEF FMXLIB}
  if Assigned(FGPGraphics) then
  begin
    if FMapping then
    begin
      FBitmap.Unmap(FBitmapData);
      FMapping := False;
    end;

    Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), RectF(0, 0, FContextSize.Width, FContextSize.Height), 1)
  end;
  {$ENDIF}

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextWin.ResetClip;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.ResetClip;
end;

procedure TTMSFNCGraphicsContextWin.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if (AAngle <> 0) and Assigned(FTextMatrix) then
  begin
    FGPGraphics.SetTransform(FTextMatrix);
    FTextMatrix.Free;
    FTextMatrix := nil;
  end;
end;

procedure TTMSFNCGraphicsContextWin.ResetTransform;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  FSaveGP := FGPGraphics.Save;
end;

procedure TTMSFNCGraphicsContextWin.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(AState.CustomSaveDC);
end;

procedure TTMSFNCGraphicsContextWin.RotateTransform(AAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.RotateTransform(AAngle, MatrixOrderPrepend);
end;

procedure TTMSFNCGraphicsContextWin.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  AState.CustomSaveDC := FGPGraphics.Save;
end;

procedure TTMSFNCGraphicsContextWin.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.ScaleTransform(AX, AY, MatrixOrderPrepend);
end;

procedure TTMSFNCGraphicsContextWin.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  case ATextQuality of
    gtqDefault: FGPGraphics.SetTextRenderingHint(TextRenderingHintSystemDefault);
    gtqAntiAliasing: FGPGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    gtqClearType: FGPGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;
  FSaveGP := FGPGraphics.Save;
end;

procedure TTMSFNCGraphicsContextWin.SetSmoothingMode(ASmoothingMode: SmoothingMode);
begin
  FSmoothingMode := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(ASmoothingMode)
end;

procedure TTMSFNCGraphicsContextWin.RestoreSmoothingMode;
begin
  FGPGraphics.SetSmoothingMode(FSmoothingMode);
end;

procedure TTMSFNCGraphicsContextWin.SetScale(AScale: Single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextWin.BeginPrinting;
begin

end;

procedure TTMSFNCGraphicsContextWin.EndPrinting;
begin

end;

procedure TTMSFNCGraphicsContextWin.SetSize(AWidth, AHeight: Single);
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  {$IFDEF FMXLIB}
  FBitmap := TBitmap.Create(Round(AWidth * FScale), Round(AHeight * FScale));
  FBitmap.BitmapScale := FScale;
  if FBitmap.Map(TMapAccess.Write, FBitmapData) then
  begin
    FMapping := True;
    FGPBitmap := TGPBitmap.Create(FBitmap.Width, FBitmap.Height, FBitmapData.Pitch, PixelFormat32bppPARGB, FBitmapData.Data);
    FGPGraphics := TGPGraphics.Create(FGPBitmap);
    FGPGraphics.Clear(ConvertToGDIPColor(gcNull, 0));
    ScaleTransform(FScale, FScale);
  {$ENDIF}
  {$IFDEF CMNLIB}
  begin
    FGPGraphics := TGPGraphics.Create(Canvas.Handle);
  {$ENDIF}
  end;

  FSaveGP := FGPGraphics.Save;
end;

function TTMSFNCGraphicsContextWin.GetMatrix: TTMSFNCGraphicsMatrix;
var
  M: TGPMatrix;
  ma: TMatrixArray;
begin
  M := TGPMatrix.Create;
  try
    FGPGraphics.GetTransform(M);
    M.GetElements(ma);
    Result := MatrixIdentity;
    Result.m11 := ma[0];
    Result.m12 := ma[1];
    Result.m21 := ma[2];
    Result.m22 := ma[3];
    Result.m31 := ma[4];
    Result.m32 := ma[5];
  finally
    M.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.SetMatrix(AMatrix: TTMSFNCGraphicsMatrix);
var
  M: TGPMatrix;
begin
  M := TGPMatrix.Create(AMatrix.m11, AMatrix.m12, AMatrix.m21, AMatrix.m22, AMatrix.m31, AMatrix.m32);
  try
    FGPGraphics.SetTransform(M);
  finally
    M.Free;
  end;
end;

procedure TTMSFNCGraphicsContextWin.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FShowAcceleratorChar := AShowAcceleratorChar;
end;

procedure TTMSFNCGraphicsContextWin.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  if AAntiAliasing then
    FGPGraphics.SetSmoothingMode(SmoothingModeAntiAlias)
  else
    FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  FSaveGP := FGPGraphics.Save;
end;

procedure TTMSFNCGraphicsContextWin.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextWin.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWin.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextWin.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TTMSFNCGraphicsContextWin.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWin.SetFontName(AName: string);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextWin.SetFontSize(ASize: Integer);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextWin.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextWin.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextWin.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextWin.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextWin.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextWin.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  rm: TGPMatrix;
  w, h, c, s, cw, ch: Single;
begin
  Result := ARect;
  if not Assigned(FGPGraphics) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := AAngle;
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;
    FTextMatrix := TGPMatrix.Create;
    FGPGraphics.GetTransform(FTextMatrix);
    rm := TGPMatrix.Create;
    rm.Translate(cx.X, cx.Y);
    rm.Rotate(ar);
    FGPGraphics.MultiplyTransform(rm, MatrixOrderPrepend);
    rm.Free;

    w := Result.Width;
    h := Result.Height;
    c := Cos(DegToRad(ar));
    s := Sin(DegToRad(ar));

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextWin.StartSpecialPen;
begin
  if not Assigned(FGPGraphics) then
    Exit;
end;

procedure TTMSFNCGraphicsContextWin.StopSpecialPen;
begin
  if not Assigned(FGPGraphics) then
    Exit;
end;

procedure TTMSFNCGraphicsContextWin.StrokeChanged(Sender: TObject);
begin
  if Assigned(FGPPen) then
  begin
    FGPPen.Free;
    FGPPen := nil;
  end;

  FGPPen := CreateGDIPPen(FStroke);
end;

procedure TTMSFNCGraphicsContextWin.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.TranslateTransform(AX, AY, MatrixOrderPrepend);
end;

{$ENDIF}

end.
