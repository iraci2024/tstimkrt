{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2021                               }
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

unit VCL.TMSFNCGraphicsPDFEngine;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$IFDEF UNIX}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$DEFINE SVGSUPPORT}

interface

uses
  Classes, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCPDFLib,
  VCL.Graphics, VCL.TMSFNCPDFIO, VCL.TMSFNCTypes
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

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release

type
  TTMSFNCGraphicsPDFEngine = class(TTMSFNCGraphics)
  private
    FCanvas: TBitmap;
    FPDFLib: ITMSFNCCustomPDFLib;
    FMatrix: TTMSFNCGraphicsMatrix;
  protected
    function InternalCalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF; override;
    function InternalDrawText(ARect: TRectF; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string;AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; override;
  public
    constructor Create(const APDFLib: ITMSFNCCustomPDFLib); reintroduce; overload; virtual;
    constructor Create(const APDFLib: TTMSFNCCustomPDFLib); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure DrawPDFPath(APath: TTMSFNCGraphicsPath; const Flatness: Single = 0.25);
    function SaveState(ACanvasOnly: Boolean = False): TTMSFNCGraphicsSaveState; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; virtual;
    function GetMatrix: TTMSFNCGraphicsMatrix; override;
    procedure SetMatrix(const AMatrix: TTMSFNCGraphicsMatrix); override;
    procedure ResetTextAngle({%H-}AAngle: Single); virtual;
    procedure SetFill(AFill: TTMSFNCGraphicsFill); override;
    procedure SetStroke(AStroke: TTMSFNCGraphicsStroke); override;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean = False); override;
    procedure SetFillKind(AKind: TTMSFNCGraphicsFillKind); override;
    procedure SetFillColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetFontColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetFontName(AName: string); override;
    procedure SetFont(AFont: TTMSFNCGraphicsFont); override;
    procedure SetFontSize(ASize: Integer); override;
    procedure SetFontStyles(AStyle: TFontStyles); override;
    procedure SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind); override;
    procedure SetStrokeColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetStrokeWidth(AWidth: Single); override;
    procedure ClipRect(ARect: TRectF); override;
    procedure DrawLine(AFromPoint: TPointF; AToPoint: TPointF; AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); override;
    procedure DrawEllipse(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Double; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; override;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Double; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; override;
    procedure DrawPolygon(APolygon: TTMSFNCGraphicsPathPolygon); override;
    procedure DrawPolyline(APolyline: TTMSFNCGraphicsPathPolygon); override;
    procedure DrawPath(APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
    procedure DrawBitmap(ALeft, ATop, ARight, ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); override;
    procedure DrawArc(const Center: TPointF; const Radius: TPointF; StartAngle: Single; SweepAngle: Single); override;
  end;

  TTMSFNCGraphicsPDF = class(TTMSFNCGraphicsPDFEngine);

  TTMSFNCGraphicsPDFIOExportRectEvent = procedure(Sender: TObject; const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; var ARect: TRectF) of object;

  TTMSFNCGraphicsPDFIOCanCreateNewPageEvent = procedure(Sender: TObject; const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; var ACanCreate: Boolean) of object;

  TTMSFNCCustomGraphicsPDFIO = class(TTMSFNCCustomPDFIO)
  private
    FOnGetExportRect: TTMSFNCGraphicsPDFIOExportRectEvent;
    FOnCanCreateNewPage: TTMSFNCGraphicsPDFIOCanCreateNewPageEvent;
  protected
    function GetVersion: String; override;
    procedure DoPDFExport(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; const AExportRect: TRectF); override;
    procedure DoGetExportRect(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; var ARect: TRectF); virtual;
    procedure DoCanCreateNewPage(const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject; var ACanCreate: Boolean); virtual;
    property Version: String read GetVersion;
    property OnGetExportRect: TTMSFNCGraphicsPDFIOExportRectEvent read FOnGetExportRect write FOnGetExportRect;
    property OnCanCreateNewPage: TTMSFNCGraphicsPDFIOCanCreateNewPageEvent read FOnCanCreateNewPage write FOnCanCreateNewPage;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCGraphicsPDFIO = class(TTMSFNCCustomGraphicsPDFIO)
  published
    property BitmapContainer;
    property ExportObject;
    property Version;
    property Options;
    property Information;
    property OnCanCreateNewPage;
    property OnGetHeader;
    property OnGetFooter;
    property OnBeforeDrawHeader;
    property OnAfterDrawHeader;
    property OnBeforeDrawFooter;
    property OnAfterDrawFooter;
    property OnBeforeDrawContent;
    property OnAfterDrawContent;
    property OnGetExportRect;
    property OnAfterDraw;
  end;

implementation

uses
  VCL.TMSFNCUtils, VCL.TMSFNCHTMLEngine, SysUtils, VCL.TMSFNCBitmapContainer, VCL.TMSFNCStyles,
  VCL.TMSFNCPDFCoreLibBase, VCL.TMSFNCPDFGraphicsLib, Math
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  ,VCL.TMSFNCGraphicsSVGEngine
  {$ENDIF}
  {$ENDIF}
  ;

{$R 'TMSFNCGraphicsPDFIO.res'}

type
  TTMSFNCPDFLibOpen = class(TTMSFNCPDFLib);

{ TTMSFNCGraphicsPDFEngine }

function TTMSFNCGraphicsPDFEngine.InternalCalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF;
begin
  Result := ARect;
  if not Assigned(FPDFLib) then
    Exit;

  if AText <> '' then
  begin
    {$IFNDEF LIMITEDGRAPHICSMODE}
    if ASupportHTML and ((Pos('</', AText) > 0) or (Pos('/>', AText)  > 0) or (Pos('<BR>', UpperCase(AText)) > 0)) then
    begin
      Result := inherited InternalCalculateText(AText, ARect, AWordWrapping, ASupportHTML);
    end
    else
    {$ENDIF}
    begin
      if AWordWrapping then
        Result := FPDFLib.Graphics.CalculateText(AText, ARect)
      else
        Result := FPDFLib.Graphics.CalculateText(AText, RectF(0, 0, 10000, 10000));

       Result.Right := Result.Right + 2;
    end;
  end
  else
  begin
    Result.Bottom := Result.Top;
    Result.Right := Result.Left;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.ClipRect(ARect: TRectF);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.DrawPathAddRectangle(ARect);
    FPDFLib.Graphics.DrawPathBeginClip;
    FPDFLib.Graphics.DrawPathBegin;
  end;
end;

constructor TTMSFNCGraphicsPDFEngine.Create(const APDFLib: TTMSFNCCustomPDFLib);
begin
  Create(TTMSFNCPDFLibOpen(APDFLib).PDFLib);
end;

constructor TTMSFNCGraphicsPDFEngine.Create(const APDFLib: ITMSFNCCustomPDFLib);
var
  g: ITMSFNCCustomPDFGraphicsLib;
begin
  FMatrix := MatrixIdentity;
  FCanvas := TBitmap.Create;
  FCanvas.Width := 1;
  FCanvas.Height := 1;
  inherited Create(FCanvas.Canvas);
  SetDefaultGraphicColors;
  FPDFLib := APDFLib;
  if Assigned(FPDFLib) then
  begin
    g := FPDFLib.Graphics;
    if Assigned(g) then
    begin
      Fill.Assign(g.Fill);
      Stroke.Assign(g.Stroke);
      Font.Name := g.Font.Name;
      Font.Size := Round(g.Font.Size);
      Font.Style := g.Font.Style;
      Font.Color := g.Font.Color;
    end;
  end;
end;

destructor TTMSFNCGraphicsPDFEngine.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;

  FPDFLib := nil;
  inherited;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawRectangle(ALeft, ATop, ARight,
  ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if Assigned(FPDFLib) then
  begin
    if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
      FPDFLib.Graphics.DrawRectangle(RectF(ALeft, ATop, ARight, ABottom));
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawArc(const Center, Radius: TPointF;
  StartAngle, SweepAngle: Single);
var
  pth: TTMSFNCGraphicsPath;
begin
  if Assigned(FPDFLib) then
  begin
    pth := TTMSFNCGraphicsPath.Create;
    try
      pth.AddArc(Center, Radius, StartAngle, SweepAngle);
      if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      begin
        if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
          DrawPath(pth, pdmPolygon)
        else
          DrawPath(pth, pdmPolyline)
      end
      else if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
        DrawPath(pth, pdmPolyline)
    finally
      pth.Free;
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawBitmap(ALeft, ATop, ARight,
  ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio, AStretch, ACenter,
  ACropping: Boolean);
var
  r: TRectF;
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  si: TTMSFNCGraphicsSVGImport;
  {$ENDIF}
  {$ENDIF}
  b: Boolean;
  p: TTMSFNCBitmap;
begin
  if not Assigned(ABitmap) or not Assigned(FPDFLib) then
    Exit;

  r := RectF(ALeft, ATop, ARight, ABottom);
  r := GetBitmapDrawRectangle(r, ABitmap, AAspectRatio, AStretch, ACenter, ACropping);

  b := True;

  {$IFDEF SVGSUPPORT}
  {$IFNDEF WEBLIB}
  if (ABitmap is TTMSFNCBitmap) then
  begin
    si := nil;
    {$IFDEF FMXLIB}
    if Assigned((ABitmap as TTMSFNCBitmap).SVG) then
      si := TTMSFNCGraphicsSVGImport((ABitmap as TTMSFNCBitmap).SVG);
    {$ENDIF}
    {$IFDEF CMNLIB}
    if Assigned(ABitmap.Graphic) and (ABitmap.Graphic is TTMSFNCSVGBitmap) then
      si := TTMSFNCGraphicsSVGImport((ABitmap.Graphic as TTMSFNCSVGBitmap).SVG);
    {$ENDIF}

    if Assigned(si) then
    begin
      si.Draw(Self, r, False, False);
      b := False;
    end;
  end;
  {$ENDIF}
  {$ENDIF}

  if b then
  begin
    p := TTMSFNCBitmap.Create;
    try
      p.Assign(ABitmap);
      FPDFLib.Graphics.DrawImage(p, r, AStretch, AAspectRatio, itOriginal, 1, ACenter);
    finally
      p.Free;
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawEllipse(ALeft, ATop, ARight,
  ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if Assigned(FPDFLib) then
    if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
      FPDFLib.Graphics.DrawEllipse(RectF(ALeft, ATop, ARight, ABottom));
end;

procedure TTMSFNCGraphicsPDFEngine.DrawLine(AFromPoint, AToPoint: TPointF;
  AModifyPointModeFrom, AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if Assigned(FPDFLib) then
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      FPDFLib.Graphics.DrawLine(AFromPoint, AToPoint);
end;

procedure TTMSFNCGraphicsPDFEngine.DrawPath(APath: TTMSFNCGraphicsPath;
  APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
begin
  if not Assigned(FPDFLib) then
    Exit;

  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      FPDFLib.Graphics.DrawPathBegin;

      DrawPDFPath(APath);

      if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      begin
        if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
          FPDFLib.Graphics.DrawPathEnd(dmPathFillStroke)
        else
          FPDFLib.Graphics.DrawPathEnd(dmPathFill)
      end
      else if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
        FPDFLib.Graphics.DrawPathEnd(dmPathStroke);
    end
    else
    begin
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: DrawPolygon(p);
        pdmPolyline: DrawPolyline(p);
      end;
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawPDFPath(APath: TTMSFNCGraphicsPath;
  const Flatness: Single);
var
  J, I: Integer;
  BPts: TTMSFNCGraphicsPathPolygon;
  B: TTMSFNCGraphicsPathCubicBezier;
  F, Len: Single;
  SegCount: Integer;
  CurPoint: TPointF;
  x: TPointF;
begin
  if not Assigned(FPDFLib) then
    Exit;

  if APath.Count > 0 then
  begin
    F := Max(Flatness, 0.05);
    J := 0;
    while J < APath.Count do
    begin
      case APath[J].Kind of
        gppMoveTo:
          begin
            FPDFLib.Graphics.DrawPathMoveToPoint(APath[J].Point);
            CurPoint := APath[J].Point;
          end;
        gppLineTo:
          begin
            FPDFLib.Graphics.DrawPathAddLineToPoint(APath[J].Point);
            CurPoint := APath[J].Point;
          end;
        gppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := APath[J].Point;
            Inc(J);
            B[2] := APath[J].Point;
            Inc(J);
            B[3] := APath[J].Point;
            BPts := APath.CreateBezier(B, 6);
            Len := 0;
            for I := 0 to High(BPts) - 1 do
            begin
              x.X := BPts[I].X - BPts[I + 1].X;
              x.Y := BPts[I].Y - BPts[I + 1].Y;
              Len := Len + GetPointLength(x);
            end;
            SegCount := Round(Len / F);
            if SegCount < 2 then
              FPDFLib.Graphics.DrawPathAddLineToPoint(B[3])
            else
            begin
              BPts := APath.CreateBezier(B, SegCount);
              for I := 0 to High(BPts) do
                FPDFLib.Graphics.DrawPathAddLineToPoint(BPts[I]);
              CurPoint := APath[J].Point;
            end;
          end;
        gppClose: FPDFLib.Graphics.DrawPathClose;
      end;
      Inc(J);
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawPolygon(
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if Assigned(FPDFLib) then
  begin
    if (Length(APolygon) > 0) and (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or ((Stroke.Kind <> gskNone) and (Stroke.Width <> 0))) then
    begin
      FPDFLib.Graphics.DrawPathBegin;
      FPDFLib.Graphics.DrawPathMoveToPoint(APolygon[0]);
      for I := 1 to Length(APolygon) - 1 do
        FPDFLib.Graphics.DrawPathAddLineToPoint(APolygon[I]);

      if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      begin
        if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
          FPDFLib.Graphics.DrawPathEnd(dmPathFillStroke)
        else
          FPDFLib.Graphics.DrawPathEnd(dmPathFill)
      end
      else if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
        FPDFLib.Graphics.DrawPathEnd(dmPathStroke);
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawPolyline(
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if Assigned(FPDFLib) then
  begin
    if (Length(APolyline) > 0) and (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
    begin
      FPDFLib.Graphics.DrawPathBegin;
      FPDFLib.Graphics.DrawPathMoveToPoint(APolyline[0]);
      for I := 1 to Length(APolyline) - 1 do
        FPDFLib.Graphics.DrawPathAddLineToPoint(APolyline[I]);
      FPDFLib.Graphics.DrawPathEnd(dmPathStroke);
    end;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.DrawRectangle(ALeft, ATop, ARight,
  ABottom: Double; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if Assigned(FPDFLib) then
  begin
    if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
      FPDFLib.Graphics.DrawRectangle(RectF(ALeft, ATop, ARight, ABottom));
  end;
end;

function TTMSFNCGraphicsPDFEngine.InternalDrawText(ARect: TRectF; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False;AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
{$IFNDEF LIMITEDGRAPHICSMODE}
  a, s: String;
  fa: String;
  XSize, YSize: Single;
  hl, ml: Integer;
  hr, cr: TRectF;
  xs, ys: Single;
  lc: Integer;
  htmlr: TRectF;
  isanchor: boolean;
  st: TTMSFNCGraphicsSaveState;
{$ENDIF}
  r: TRectF;
  p: TPointF;
begin
  {$IFDEF WEBLIB}
  if (AMinHeight > -1) and (ARect.Bottom - ARect.Top < AMinHeight) then
    ARect.Bottom := ARect.Top + AMinHeight;

  if (AMinWidth > -1) and (ARect.Right - ARect.Left < AMinWidth) then
    ARect.Right := ARect.Left + AMinWidth;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if (AMinHeight > -1) and (ARect.Height < AMinHeight) then
    ARect.Height := AMinHeight;

  if (AMinWidth > -1) and (ARect.Width < AMinWidth) then
    ARect.Width := AMinWidth;
  {$ENDIF}

  FPDFLib.Graphics.DrawSaveState;

  ARect := SetTextAngle(ARect, AAngle);

  Result := '';

  {$IFNDEF LIMITEDGRAPHICSMODE}
  if ((Pos('</', AText) > 0) or (Pos('/>', AText)  > 0) or (Pos('<BR>', UpperCase(AText)) > 0)) and ASupportHTML then
  begin
    hr := RectF(0, 0, 0, 0);
    XSize := 0;
    YSize := 0;
    hl := -1;
    ml := -1;
    fa := '';
    s := '';
    a := '';
    lc := 0;
    cr := RectF(0, 0, 0, 0);
    AControlID := '';
    AControlValue := '';
    AControlType := '';
    HTMLDrawEx(Self, AText, ARect,0, 0,-1,-1,0,False,True,False,False,False,False,AWordWrapping,False, '', 1.0,URLColor,
        gcNull,gcNull,gcNull,a,s,fa,XSize,YSize,hl,ml,hr, cr, AControlID, AControlValue, AControlType, lc, 0, BitmapContainer, 1, URLUnderline{$IFDEF CMNLIB}, ImageList{$ENDIF},
        HighlightColor, HighlightTextColor, HighlightFontStyle);

    YSize := YSize + 1;
    XSize := XSize + 1;

    if (AWordWrapping and (lc <= 1)) or (not AWordWrapping) then
    begin
      xs := ARect.Left;
      ys := ARect.Top;

      case AHorizontalAlign of
        gtaCenter: xs := xs + ((ARect.Right - ARect.Left) - XSize) / 2;
        gtaTrailing: xs := ARect.Left + (ARect.Right - ARect.Left) - XSize;
      end;

      case AVerticalAlign of
        gtaCenter: ys := ys + ((ARect.Bottom - ARect.Top) - YSize) / 2;
        gtaTrailing: ys := ys + (ARect.Bottom - ARect.Top) - YSize;
      end;

      htmlr := RectF(xs, ys, xs + XSize, ys + YSize);
    end
    else
      htmlr := ARect;

    st := SaveState(True);
    ClipRect(ARect);
    isanchor := HTMLDrawEx(Self, AText, htmlr,Round(AX), Round(AY),-1,-1,0,ATestAnchor,False,False,False,False,False,AWordWrapping,False, '', 1.0,URLColor,
        gcNull,gcNull,gcNull,a,s,fa,XSize,YSize,hl,ml,hr,cr, AControlID, AControlValue, AControlType, lc, 0, BitmapContainer, 1, URLUnderline{$IFDEF CMNLIB}, ImageList{$ENDIF},
        HighlightColor, HighlightTextColor, HighlightFontStyle);
    RestoreState(st, True);

    if isanchor then
      Result := a;
  end
  else if not ATestAnchor then
  {$ENDIF}
  begin
    if AWordWrapping then
      R := FPDFLib.Graphics.CalculateText(AText, ARect)
    else
      R := FPDFLib.Graphics.CalculateText(AText);

    r.Right := r.Right + 2;

    if AWordWrapping then
    begin
      FPDFLib.Graphics.Alignment := AHorizontalAlign;
      case AVerticalAlign of
        gtaCenter: r := RectF(ARect.Left, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2, ARect.Right, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top) / 2 + r.Bottom - r.Top));
        gtaLeading: r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + (r.Bottom - r.Top));
        gtaTrailing: r := RectF(ARect.Left, ARect.Bottom - (r.Bottom - r.Top), ARect.Right, ARect.Bottom);
      end;
      FPDFLib.Graphics.DrawText(AText, r);
    end
    else
    begin
      case AHorizontalAlign of
        gtaLeading:
        begin
          case AVerticalAlign of
            gtaLeading: p := PointF(ARect.Left, ARect.Top);
            gtaCenter: p := PointF(ARect.Left, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2);
            gtaTrailing: p := PointF(ARect.Left, ARect.Bottom - (r.Bottom - r.Top));
          end;
        end;
        gtaCenter:
        begin
          case AVerticalAlign of
            gtaLeading: p := PointF(ARect.Left + ((ARect.Right - ARect.Left) - (r.Right - r.Left)) / 2, ARect.Top);
            gtaCenter: p := PointF(ARect.Left + ((ARect.Right - ARect.Left) - (r.Right - r.Left)) / 2, ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2);
            gtaTrailing: p := PointF(ARect.Left + ((ARect.Right - ARect.Left) - (r.Right - r.Left)) / 2, ARect.Bottom - (r.Bottom - r.Top));
          end;
        end;
        gtaTrailing:
        begin
          case AVerticalAlign of
            gtaLeading: p := PointF(ARect.Right - (r.Right - r.Left), ARect.Top);
            gtaCenter: p := PointF(ARect.Right - (r.Right - r.Left), ARect.Top + ((ARect.Bottom - ARect.Top) - (r.Bottom - r.Top)) / 2);
            gtaTrailing: p := PointF(ARect.Right - (r.Right - r.Left), ARect.Bottom - (r.Bottom - r.Top));
          end;
        end;
      end;
      FPDFLib.Graphics.DrawText(AText, p);
    end;
  end;

  ResetTextAngle(AAngle);
  FPDFLib.Graphics.DrawRestoreState;
end;

function TTMSFNCGraphicsPDFEngine.GetMatrix: TTMSFNCGraphicsMatrix;
begin
  Result := FMatrix;
end;

procedure TTMSFNCGraphicsPDFEngine.ResetTextAngle(AAngle: Single);
begin
  if Assigned(FPDFLib) and (AAngle <> 0) then
    FPDFLib.Graphics.DrawSetTransform(1, 0, 0, 1, 0, 0);
end;

procedure TTMSFNCGraphicsPDFEngine.RestoreState(
  AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.DrawRestoreState;
end;

function TTMSFNCGraphicsPDFEngine.SaveState(
  ACanvasOnly: Boolean): TTMSFNCGraphicsSaveState;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.DrawSaveState;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Fill.Assign(AFill);
end;

procedure TTMSFNCGraphicsPDFEngine.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Fill.Color := AColor;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Fill.Kind := AKind;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.Font.BeginUpdate;
    FPDFLib.Graphics.Font.Name := AFont.Name;
    FPDFLib.Graphics.Font.Color := AFont.Color;
    FPDFLib.Graphics.Font.Style := AFont.Style;
    FPDFLib.Graphics.Font.Size := AFont.Size;
    FPDFLib.Graphics.Font.EndUpdate;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.Font.BeginUpdate;
    FPDFLib.Graphics.Font.Color := AColor;
    FPDFLib.Graphics.Font.EndUpdate;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFontName(AName: string);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.Font.BeginUpdate;
    FPDFLib.Graphics.Font.Name := AName;
    FPDFLib.Graphics.Font.EndUpdate;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFontSize(ASize: Integer);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.Font.BeginUpdate;
    FPDFLib.Graphics.Font.Size := ASize;
    FPDFLib.Graphics.Font.EndUpdate;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.SetFontStyles(AStyle: TFontStyles);
begin
  if Assigned(FPDFLib) then
  begin
    FPDFLib.Graphics.Font.BeginUpdate;
    FPDFLib.Graphics.Font.Style := AStyle;
    FPDFLib.Graphics.Font.EndUpdate;
  end;
end;

procedure TTMSFNCGraphicsPDFEngine.SetMatrix(
  const AMatrix: TTMSFNCGraphicsMatrix);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.DrawSetTransform(AMatrix);

  FMatrix := AMatrix;
end;

procedure TTMSFNCGraphicsPDFEngine.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Stroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsPDFEngine.SetStrokeColor(AColor: TTMSFNCGraphicsColor);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Stroke.Color := AColor;
end;

procedure TTMSFNCGraphicsPDFEngine.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Stroke.Color := Stroke.Color;
end;

procedure TTMSFNCGraphicsPDFEngine.SetStrokeWidth(AWidth: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Graphics.Stroke.Width := AWidth;
end;

function TTMSFNCGraphicsPDFEngine.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  {$IFDEF APPLYTRANSFORMHEIGHT}
  ph: Single;
  {$ENDIF}
  rm: TTMSFNCGraphicsMatrix;
  h, w, c, s, cw, ch: Single;
begin
  Result := ARect;
  if AAngle <> 0 then
  begin
    ar := DegToRad({$IFDEF APPLYTRANSFORMHEIGHT}-{$ENDIF}AAngle);
    cx := CenterPointEx(Result);

    {$IFDEF APPLYTRANSFORMHEIGHT}
    ph := FPDFLib.MediaBox.Bottom - FPDFLib.MediaBox.Top;
    h := ph - cx.Y;
    {$ELSE}
    h := cx.Y;
    {$ENDIF}

    rm := MatrixMultiply(MatrixCreateRotation(ar), MatrixCreateTranslation(cx.X, h));
    FPDFLib.Graphics.DrawSetTransform(rm);

    w := Result.Right - Result.Left;
    h := Result.Bottom - Result.Top;
    c := Cos(DegToRad(AAngle));
    s := Sin(DegToRad(AAngle));

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);

    {$IFDEF APPLYTRANSFORMHEIGHT}
    Result.Top := Result.Top + ph;
    Result.Bottom := Result.Bottom + ph;
    {$ENDIF}
  end;
end;

{ TTMSFNCCustomGraphicsPDFIO }

procedure TTMSFNCCustomGraphicsPDFIO.DoCanCreateNewPage(
  const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject;
  var ACanCreate: Boolean);
begin
  if Assigned(OnCanCreateNewPage) then
    OnCanCreateNewPage(Self, APDFLib, AExportObject, ACanCreate);
end;

procedure TTMSFNCCustomGraphicsPDFIO.DoGetExportRect(
  const APDFLib: TTMSFNCPDFLib; const AExportObject: TTMSFNCPDFIOExportObject;
  var ARect: TRectF);
begin
  if Assigned(OnGetExportRect) then
    OnGetExportRect(Self, APDFLib, AExportObject, ARect);
end;

procedure TTMSFNCCustomGraphicsPDFIO.DoPDFExport(const APDFLib: TTMSFNCPDFLib;
  const AExportObject: TTMSFNCPDFIOExportObject; const AExportRect: TRectF);
var
  gpdf: TTMSFNCGraphicsPDFEngine;
  g: ITMSFNCGraphicsExport;
  {$IFDEF FNCLIB}
  bmp: ITMSFNCBitmapContainer;
  {$ENDIF}
  r: TRectF;
  a: Boolean;
begin
  gpdf := TTMSFNCGraphicsPDFEngine.Create(TTMSFNCPDFLibOpen(APDFLib).PDFLib);
  try
    a := True;
    DoCanCreateNewPage(APDFLib, AExportObject, a);
    if a then
      NewPage(APDFLib, AExportObject);

    if Assigned(AExportObject) and Supports(AExportObject, ITMSFNCGraphicsExport, g) then
    begin
      {$IFDEF FNCLIB}
      if Supports(AExportObject, ITMSFNCBitmapContainer, bmp) then
        APDFLib.BitmapContainer := bmp.BitmapContainer;
      {$ENDIF}

      r := AExportRect;
      DoGetExportRect(APDFLib, AExportObject, r);
      g.Export(gpdf, r);
    end;
  finally
    gpdf.Free;
  end;
end;

function TTMSFNCCustomGraphicsPDFIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

end.
