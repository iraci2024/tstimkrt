{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2021                               }
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

unit FMX.TMSFNCGraphicsSVGEngine;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$IFDEF UNIX}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$DEFINE APPLYTRANSFORMHEIGHT}
{$ENDIF}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  FMX.TMSFNCGDIPlusClasses, FMX.TMSFNCGDIPlusApi,
  {$ENDIF}
  FMX.TMSFNCPersistence, Classes, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.Graphics, FMX.TMSFNCTypes
  {$IFNDEF LCLLIB}
  ,XmlIntf, XMLDoc, Variants
  {$ELSE}
  ,XMLRead, XMLWrite, DOM
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,Types
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Fixed : Issue with parsing style class in certain situations

type
  TTMSFNCGraphicsSVGEngine = class(TTMSFNCGraphics)
  private
    FCanvas: TBitmap;
    FSVG: TStringList;
    FWidth, FHeight: Single;
  protected
    function GenerateStroke: string;
    function GenerateFill: string;
  public
    constructor Create(AWidth: Single; AHeight: Single); reintroduce; overload; virtual;
    destructor Destroy; override;
    function SaveState(ACanvasOnly: Boolean = False): TTMSFNCGraphicsSaveState; override;
    //function CalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; virtual;
    function Build: string;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure BeginPrinting; override;
    procedure EndPrinting; override;
    procedure ResetTextAngle({%H-}AAngle: Single); virtual;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean = False); override;
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
    function DrawText(ARect: TRectF; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
      AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; override;
  end;

  TTMSFNCGraphicsSVG = class(TTMSFNCGraphicsSVGEngine);

  TTMSFNCGraphicsSVGElementType = (etContainer, etSwitch, etDefs, etUse, etRect, etLine, etPolyline, etPolygon,
    etCircle, etEllipse, etPath, etImage, etText, etTextSpan, etTextSpanChild, etTextPath, etClipPath, etLinearGradient, etRadialGradient,
    etStyle, etStop, etPattern, etUnknown);

  TTMSFNCGraphicsSVGElement = class;

  TTMSFNCGraphicsSVGElementClassList = class(TList<TTMSFNCGraphicsSVGElement>);

  TTMSFNCGraphicsSVGElementList = class(TObjectList<TTMSFNCGraphicsSVGElement>);

  TTMSFNCGraphicsSVGElementPathList = class(TObjectList<TTMSFNCGraphicsPath>);

  TTMSFNCGraphicsSVGElementPreserveAspectRatio = (parNone, parOther);

  TTMSFNCGraphicsSVGElement = class
  private
    FBitmap: TTMSFNCBitmap;
    FCombinedPath: TTMSFNCGraphicsPath;
    FPaths: TTMSFNCGraphicsSVGElementPathList;
    FElements: TTMSFNCGraphicsSVGElementList;
    FType: TTMSFNCGraphicsSVGElementType;
    FWidth: Single;
    FXV: Single;
    FYV: Single;
    FHeight: Single;
    FRx: Single;
    FRy: Single;
    FMatrix: TTMSFNCGraphicsMatrix;
    FFillColor: TTMSFNCGraphicsColor;
    FStrokeColor: TTMSFNCGraphicsColor;
    FFillColorValue: string;
    FStrokeColorValue: string;
    FParent: TTMSFNCGraphicsSVGElement;
    FStrokeWidth: Single;
    FStrokeWidthValue: string;
    FOpacityValue: string;
    FOffsetValue: string;
    FOffset: Single;
    FStopColorValue: string;
    FStopColor: TTMSFNCGraphicsColor;
    FStopOpacityValue: string;
    FStopOpacity: Single;
    FOpacity: Single;
    FID: string;
    FClassRefID: string;
    FClassRef: TTMSFNCGraphicsSVGElement;
    FHRefID: string;
    FURLRefID: string;
    FHRefRef: TTMSFNCGraphicsSVGElement;
    FPatternRef: TTMSFNCGraphicsSVGElement;
    FClipPathRefID: string;
    FClipPathRef: TTMSFNCGraphicsSVGElement;
    FFontFamily: string;
    FFontSizeValue: string;
    FFontSize: Single;
    FFontStyle: TFontStyles;
    FText: string;
    FDisplayValue: string;
    FPreserveAspectRatioValue: string;
    FPreserveAspectRatio: TTMSFNCGraphicsSVGElementPreserveAspectRatio;
    FFR: Single;
    FCX: Single;
    FCY: single;
    FFX: Single;
    FFY: Single;
    FHasCX: Boolean;
    FHasCY: Boolean;
    FGradientMatrix: TTMSFNCGraphicsMatrix;
    function GetElements: TTMSFNCGraphicsSVGElementList;
    function GetPaths: TTMSFNCGraphicsSVGElementPathList;
    function GetActivePath: TTMSFNCGraphicsPath;
    function GetCombinedPath: TTMSFNCGraphicsPath;
    function GetBitmap: TTMSFNCBitmap;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function HasStrokeColor: Boolean;
    function HasStrokeWidth: Boolean;
    function HasFontSize: Boolean;
    function HasOpacity: Boolean;
    function HasOffset: Boolean;
    function HasStopColor: Boolean;
    function HasStopOpacity: Boolean;
    function HasPreserveAspectRatio: Boolean;
    function HasFillColor: Boolean;
    function HasElements: Boolean;
    function HasPaths: Boolean;
    function HasBitmap: Boolean;
    function HasActivePath: Boolean;
    property ID: string read FID;
    property ClassRefID: string read FClassRefID;
    property URLRefID: string read FURLRefID;
    property HRefID: string read FHRefID;
    property ClipPathRefID: string read FClipPathRefID;
    property CombinedPath: TTMSFNCGraphicsPath read GetCombinedPath;
    property ActivePath: TTMSFNCGraphicsPath read GetActivePath;
    property &Type: TTMSFNCGraphicsSVGElementType read FType write FType;
    property Elements: TTMSFNCGraphicsSVGElementList read GetElements;
    property Paths: TTMSFNCGraphicsSVGElementPathList read GetPaths;
    property Opacity: Single read FOpacity;
    property Offset: Single read FOffset;
    property StopColor: TTMSFNCGraphicsColor read FStopColor;
    property StopOpacity: Single read FStopOpacity;
    property X: Single read FXV;
    property Y: Single read FYV;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property Rx: Single read FRx;
    property Ry: Single read FRy;
    property CX: Single read FCX;
    property CY: single read FCY;
    property Matrix: TTMSFNCGraphicsMatrix read FMatrix;
    property GradientMatrix: TTMSFNCGraphicsMatrix read FGradientMatrix;
    property StrokeWidth: Single read FStrokeWidth;
    property DisplayValue: string read FDisplayValue;
    property FillColorValue: string read FFillColorValue;
    property StrokeColorValue: string read FStrokeColorValue;
    property StrokeWidthValue: string read FStrokeWidthValue;
    property OpacityValue: string read FOpacityValue;
    property OffsetValue: string read FOffsetValue;
    property StopColorValue: string read FStopColorValue;
    property StopOpacityValue: string read FStopOpacityValue;
    property FontSizeValue: string read FFontSizeValue;
    property FontSize: Single read FFontSize;
    property FontFamily: string read FFontFamily;
    property FontStyle: TFontStyles read FFontStyle;
    property Text: string read FText;
    property FillColor: TTMSFNCGraphicsColor read FFillColor;
    property StrokeColor: TTMSFNCGraphicsColor read FStrokeColor;
    property Parent: TTMSFNCGraphicsSVGElement read FParent;
    property ClassRef: TTMSFNCGraphicsSVGElement read FClassRef;
    property ClipPathRef: TTMSFNCGraphicsSVGElement read FClipPathRef;
    property Bitmap: TTMSFNCBitmap read GetBitmap;
    property PreserveAspectRatioValue: string read FPreserveAspectRatioValue;
    property PreserveAspectRatio: TTMSFNCGraphicsSVGElementPreserveAspectRatio read FPreserveAspectRatio;
  end;

  {$IFNDEF LCLLIB}
  TTMSFNCGraphicsSVGXMLNode = IXMLNode;
  TTMSFNCGraphicsSVGXMLDocument = IXMLDocument;
  TTMSFNCGraphicsSVGXMLNodeValue = OleVariant;
  {$ELSE}
  TTMSFNCGraphicsSVGXMLNode = TDomNode;
  TTMSFNCGraphicsSVGXMLDocument = TXMLDocument;
  TTMSFNCGraphicsSVGXMLNodeValue = DOMString;
  {$ENDIF}

  TTMSFNCGraphicsSVGImport = class(TTMSFNCSVGImport)
  private
    FXML: TTMSFNCGraphicsSVGXMLDocument;
    FElementClassList: TTMSFNCGraphicsSVGElementClassList;
    FElementPatternList: TTMSFNCGraphicsSVGElementClassList;
    FElementClipPathList: TTMSFNCGraphicsSVGElementClassList;
    FElementList: TTMSFNCGraphicsSVGElementList;
    FDefsList: TTMSFNCGraphicsSVGElementList;
    FCachedBitmap: TTMSFNCBitmap;
    FMatrix, FTextMatrix: TTMSFNCGraphicsMatrix;
  protected
    function NodeToString(ANode: TTMSFNCGraphicsSVGXMLNode): string;
    function ConvertColorToGray(AColor: TTMSFNCGraphicsColor): TTMSFNCGraphicsColor;
    function ApplyTintColor(AColor, ATintColor: TTMSFNCGraphicsColor): TTMSFNCGraphicsColor;
    procedure UpdateViewRect(const AElement: TTMSFNCGraphicsSVGElement; const ARect: TRectF);
    procedure ParseStyleValues(const AElement: TTMSFNCGraphicsSVGElement; const Values: string);
    function ParseRGB(const S: string; var AAlpha: Integer): TTMSFNCGraphicsColor;
    procedure ParseTransform(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AMatrix: TTMSFNCGraphicsMatrix);
    procedure ParseString(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AValue: string);
    procedure ParseLength(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AValue: Single);
    procedure LinkPatternHRef(const AElementClasses: TTMSFNCGraphicsSVGElementClassList; const AElementDefs: TTMSFNCGraphicsSVGElementClassList); 
    procedure ReadNode(const AElements: TTMSFNCGraphicsSVGElementList; const ANode: TTMSFNCGraphicsSVGXMLNode; const AUpdateViewRect: Boolean = True; const AParent: TTMSFNCGraphicsSVGElement = nil);
    procedure LinkHRef(const AElementClasses: TTMSFNCGraphicsSVGElementClassList; AElementDefs: TTMSFNCGraphicsSVGElementList);
    procedure LinkPatterns(const AElementClasses: TTMSFNCGraphicsSVGElementClassList; AElementDefs: TTMSFNCGraphicsSVGElementList);
    procedure LinkClipPaths(const AElementClasses: TTMSFNCGraphicsSVGElementClassList; AElementDefs: TTMSFNCGraphicsSVGElementList);
    procedure LinkClasses(const AElementClasses: TTMSFNCGraphicsSVGElementClassList; AElementDefs: TTMSFNCGraphicsSVGElementList);
    procedure AddArc(const AElement: TTMSFNCGraphicsSVGElement; const APath: TTMSFNCGraphicsPath; AStartX, AStopX, AStartY, AStopY, ARx, ARy, AXRot: Single; ALarge, ASweep: Integer);
    procedure FixCurve(const ACommand: Char; SL: TStrings);
    procedure InternalDraw(const AElements: TTMSFNCGraphicsSVGElementList; const AGraphics: TTMSFNCGraphics; const AScaleX: Single; const AScaleY: Single);
    function ParseImage(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AValue: string): Boolean;
    function IndexOfAny(const AValue: string; const AnyOf: array of Char; StartIndex, Count: Integer): Integer;
    function NameToElementType(const AName: string): TTMSFNCGraphicsSVGElementType;
    function ProcessValues(const ACommand: Char; const S: string): TStrings;
    function SplitPath(const S: string): TStrings;
  public
    procedure Draw(const ACanvas: TCanvas; ARect: TRectF; ANative: Boolean = False); overload; override;
    procedure Draw(const AGraphics: TTMSFNCGraphics; ARect: TRectF; ANative: Boolean = False; ARecreateGraphics: Boolean = True); reintroduce; overload;
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromText(const AText: string); override;
    procedure LoadFromFile(const AFile: string); override;
    procedure LoadFromStream(const AStream: TStream); override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure SaveToFile(const AFile: string); override;
    procedure Clear; override;
    function HasElements: Boolean; override;
    function ElementCount: Integer; override;
    function GenerateBitmap(AWidth: Single = - 1; AHeight: Single = -1): TTMSFNCBitmap; override;
  end;

implementation

uses
  FMX.TMSFNCUtils, StrUtils, SysUtils, FMX.TMSFNCBitmapContainer,
  FMX.TMSFNCStyles, Math
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 23}
  {$IFDEF MSWINDOWS}
  ,XML.Win.MSXMLDom
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

{ TTMSFNCGraphicsSVGEngine }

procedure TTMSFNCGraphicsSVGEngine.BeginPrinting;
begin
  inherited;

end;

procedure TTMSFNCGraphicsSVGEngine.BeginScene;
begin
  FSVG.Add('<svg viewBox=''0 0 ' + FloatToStr(FWidth) + ' ' + FloatToStr(FHeight) + ''' xmlns=''http://www.w3.org/2000/svg''>');
end;

//function TTMSFNCGraphicsSVGEngine.CalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF;
//begin
//  Result := ARect;
//
//  if AText <> '' then
//  begin
//    {$IFNDEF LIMITEDGRAPHICSMODE}
//    if ASupportHTML and ((Pos('</', AText) > 0) or (Pos('/>', AText)  > 0) or (Pos('<BR>', UpperCase(AText)) > 0)) then
//    begin
//      Result := inherited CalculateText(AText, ARect, AWordWrapping, ASupportHTML);
//    end
//    else
//    {$ENDIF}
//    begin
//      raise Exception.Create('Calculate Text');
//    end;
//  end
//  else
//  begin
//    Result.Bottom := Result.Top;
//    Result.Right := Result.Left;
//  end;
//end;

procedure TTMSFNCGraphicsSVGEngine.ClipRect(ARect: TRectF);
begin
end;

constructor TTMSFNCGraphicsSVGEngine.Create(AWidth: Single; AHeight: Single);
begin
  FCanvas := TBitmap.Create;
  FCanvas.Width := 1;
  FCanvas.Height := 1;
  inherited Create(FCanvas.Canvas);
  SetDefaultGraphicColors;
  FSVG := TStringList.Create;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TTMSFNCGraphicsSVGEngine.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;

  if Assigned(FSVG) then
    FSVG.Free;

  inherited;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawRectangle(ALeft, ATop, ARight,
  ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  v: string;
begin
  if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
  begin
    v := '<rect x="' + FloatToStr(ALeft) + '" y="' + FloatToStr(ATop) + '" width="' + FloatToStr(ARight - ALeft) + '" height="' + FloatToStr(ABottom - ATop) + '"';
    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      v := v + ' ' + GenerateFill;
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      v := v + ' ' + GenerateStroke;

    v := v + '/>';

    FSVG.Add(v);
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawArc(const Center, Radius: TPointF;
  StartAngle, SweepAngle: Single);
var
  pth: TTMSFNCGraphicsPath;
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

procedure TTMSFNCGraphicsSVGEngine.DrawBitmap(ALeft, ATop, ARight,
  ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio, AStretch, ACenter,
  ACropping: Boolean);
var
  r: TRectF;
//  {$IFNDEF FMXLIB}
//  p: TTMSFNCBitmap;
//  {$ENDIF}
begin
  if not Assigned(ABitmap) then
    Exit;

  r := RectF(ALeft, ATop, ARight, ABottom);
  r := GetBitmapDrawRectangle(r, ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
  //FSVGLib.Graphics.DrawImage(ABitmap, r);
end;

procedure TTMSFNCGraphicsSVGEngine.DrawEllipse(ALeft, ATop, ARight,
  ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  v: string;
begin
  if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
  begin
    v := '<ellipse cx="' + FloatToStr(ALeft + (ARight - ALeft) / 2) + '" cy="' + FloatToStr(ATop + (ABottom - ATop) / 2) + '" rx="' + FloatToStr((ARight - ALeft) / 2) + '" ry="' + FloatToStr((ABottom - ATop) / 2) + '"';
    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      v := v + ' ' + GenerateFill;
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      v := v + ' ' + GenerateStroke;

    v := v + '/>';

    FSVG.Add(v);
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawLine(AFromPoint, AToPoint: TPointF;
  AModifyPointModeFrom, AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
var
  v: string;
begin
  if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
  begin
    v := '<line x1="' + FloatToStr(AFromPoint.X) + '" y1="' + FloatToStr(AFromPoint.Y) + '" x2="' + FloatToStr(AToPoint.X) + '" y2="' + FloatToStr(AToPoint.Y) + '"';
    v := v + ' ' + GenerateStroke;
    v := v + '/>';
    FSVG.Add(v);
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawPath(APath: TTMSFNCGraphicsPath;
  APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
      raise Exception.Create('Implement Path export in SVG')
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

procedure TTMSFNCGraphicsSVGEngine.DrawPolygon(
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
  v: string;
begin
  if (Length(APolygon) > 0) and (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or ((Stroke.Kind <> gskNone) and (Stroke.Width <> 0))) then
  begin
    v := '<path d="';
    v := v + 'M ' + FloatToStr(APolygon[0].X) + ' ' + FloatToStr(APolygon[0].y) + ' ';
    for I := 1 to Length(APolygon) - 1 do
      v := v + 'L ' + FloatToStr(APolygon[I].X) + ' ' + FloatToStr(APolygon[I].y) + ' ';

    v := v + '" ';

    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
      v := v + GenerateFill;

    if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
      v := v + GenerateStroke;

    v := v + '/>';

    FSVG.Add(v);
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawPolyline(
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
  v: string;
begin
  if (Length(APolyline) > 0) and (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
  begin
    v := '<path d="';
    v := v + 'M ' + FloatToStr(APolyline[0].X) + ' ' + FloatToStr(APolyline[0].y) + ' ';

    for I := 1 to Length(APolyline) - 1 do
      v := v + 'L ' + FloatToStr(APolyline[I].X) + ' ' + FloatToStr(APolyline[I].y) + ' ';

    v := v + '" ';

    if (Stroke.Kind <> gskNone) and (Stroke.Width <> 0) then
      v := v + GenerateStroke;

    v := v + '/>';

    FSVG.Add(v);
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.DrawRectangle(ALeft, ATop, ARight,
  ABottom: Double; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  DrawRectangle(ALeft, ATop, ARight, ABottom, AModifyRectMode);
//  if (((Fill.Color <> gcNull) and (Fill.Kind <> gfkNone)) or (Fill.Kind = gfkTexture)) or ((Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone)) then
    //FSVGLib.Graphics.DrawRectangle(RectF(ALeft, ATop, ARight, ABottom));
end;

function TTMSFNCGraphicsSVGEngine.DrawText(ARect: TRectF; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False;AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  tr, r: TRectF;
  w, h: Single;
  v: string;
begin
  Result := '';

  if (AMinHeight > -1) and ((ARect.Bottom - ARect.Top) < AMinHeight) then
    ARect.Bottom := ARect.Top + AMinHeight;

  if (AMinWidth > -1) and ((ARect.Right - ARect.Left) < AMinWidth) then
    ARect.Right := ARect.Left + AMinWidth;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  if TTMSFNCUtils.IsHTML(AText) and ASupportHTML then
    Result := inherited
  else if not ATestAnchor then
  {$ENDIF}
  begin
    Result := '';
    tr := CalculateText(AText, ARect, AWordWrapping);
    tr.Right := tr.Right + 2;

    case AHorizontalAlign of
      gtaCenter:
      begin
        case AVerticalAlign of
          gtaCenter: tr := RectF(ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2, ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2, ARect.Left  + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2 + (tr.Bottom - tr.Top));
          gtaLeading: tr := RectF(ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2, ARect.Top, ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), ARect.Top + (tr.Bottom - tr.Top));
          gtaTrailing: tr := RectF(ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2, ARect.Bottom - (tr.Bottom - tr.Top), ARect.Left + ((ARect.Right - ARect.Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), ARect.Bottom);
        end;
      end;
      gtaLeading:
      begin
        case AVerticalAlign of
          gtaCenter: tr := RectF(ARect.Left, ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2, ARect.Left + (tr.Right - tr.Left), ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2 + (tr.Bottom - tr.Top));
          gtaLeading: tr := RectF(ARect.Left, ARect.Top, ARect.Left + (tr.Right - tr.Left), ARect.Top + (tr.Bottom - tr.Top));
          gtaTrailing: tr := RectF(ARect.Left, ARect.Bottom - (tr.Bottom - tr.Top), ARect.Left + (tr.Right - tr.Left), ARect.Bottom);
        end;
      end;
      gtaTrailing:
      begin
        case AVerticalAlign of
          gtaCenter: tr := RectF(ARect.Right - (tr.Right - tr.Left), ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2, ARect.Right, ARect.Top + ((ARect.Bottom - ARect.Top) - (tr.Bottom - tr.Top)) / 2 + (tr.Bottom - tr.Top));
          gtaLeading: tr := RectF(ARect.Right - (tr.Right - tr.Left), ARect.Top, ARect.Right, ARect.Top + (tr.Bottom - tr.Top));
          gtaTrailing: tr := RectF(ARect.Right - (tr.Right - tr.Left), ARect.Bottom - (tr.Bottom - tr.Top), ARect.Right, ARect.Bottom);
        end;
      end;
    end;

    //FSVGLib.Graphics.DrawSaveState;
    SetTextAngle(tr, AAngle);
    if AAngle = 0 then
      r := tr
    else
    begin
      w := ARect.Right - ARect.Left;
      h := ARect.Bottom - ARect.Top;

      if w > h then
        r := RectF(ARect.Left, (ARect.Top + h / 2 - w / 2), ARect.Right, (ARect.Top + h / 2 + w / 2))
      else
        r := RectF(ARect.Left + w / 2 - h / 2, ARect.Top, ARect.Left + w / 2 + h / 2, ARect.Bottom);

    end;
    InflateRectEx(r, 2, 2);
//    FSVGLib.Graphics.DrawPathAddRectangle(r);
//    FSVGLib.Graphics.DrawPathBeginClip;
//    FSVGLib.Graphics.DrawPathBegin;
    v := '<text alignment-baseline="hanging" x="' + FloatToStr(tr.Left) + '" y="' + FloatToStr(tr.Top) + '" font-family="' + Font.Name + '" font-size="' +
      FloatToStr(Font.Size) + '" fill="' + ColorToHTML(Font.Color) + '">'+ AText + '</text>';

    FSVG.Add(v);
//    FSVGLib.Graphics.DrawText(AText, tr);
//    FSVGLib.Graphics.DrawRestoreState;
  end;
end;

procedure TTMSFNCGraphicsSVGEngine.EndPrinting;
begin
  inherited;

end;

procedure TTMSFNCGraphicsSVGEngine.EndScene;
begin
  FSVG.Add('</svg>');
end;

function TTMSFNCGraphicsSVGEngine.GenerateFill: string;
begin
  Result := 'fill="' + ColorToHTML(Fill.Color) + '"';
end;

function TTMSFNCGraphicsSVGEngine.GenerateStroke: string;
begin
  Result := 'stroke="' + ColorToHTML(Stroke.Color) + '" stroke-width="' + FloatToStr(Stroke.Width) + '"';
end;

procedure TTMSFNCGraphicsSVGEngine.ResetTextAngle(AAngle: Single);
begin
  if (AAngle <> 0) then
    //FSVGLib.Graphics.DrawSetTransform(1, 0, 0, 1, 0, 0);
end;

procedure TTMSFNCGraphicsSVGEngine.RestoreState(
  AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean);
begin
  //FSVGLib.Graphics.DrawRestoreState;
end;

function TTMSFNCGraphicsSVGEngine.SaveState(
  ACanvasOnly: Boolean): TTMSFNCGraphicsSaveState;
begin
  Result := nil;
  //FSVGLib.Graphics.DrawSaveState;
end;

function TTMSFNCGraphicsSVGEngine.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
//  ar: Single;
  cx: TPointF;
//  rm: TTMSFNCGraphicsMatrix;
//  h: Single;
begin
  Result := ARect;
  if (AAngle <> 0) then
  begin
    cx := CenterPointEx(Result);
//    {$IFDEF APPLYTRANSFORMHEIGHT}
//    ar := DegToRad(-AAngle);
//    h := FSVGLib.MediaBox.Bottom - FSVGLib.MediaBox.Top - cx.Y;
//    {$ELSE}
//    ar := DegToRad(AAngle);
//    h := cx.Y;
//    {$ENDIF}
//
//    rm := MatrixCreateTranslation(-cx.X, - h);
//    rm := MatrixMultiply(rm, MatrixCreateRotation(ar));
//    {$IFDEF APPLYTRANSFORMHEIGHT}
//    if AAngle > 0 then
//      rm := MatrixMultiply(rm, MatrixCreateTranslation(cx.X - (ARect.Bottom - ARect.Top) / 2, + h))
//    else
//    {$ENDIF}
//      rm := MatrixMultiply(rm, MatrixCreateTranslation(cx.X, + h));
//
//    FSVGLib.Graphics.DrawSetTransform(rm);
  end;
end;

function TTMSFNCGraphicsSVGEngine.Build: string;
begin
  Result := FSVG.Text;
end;

{ TTMSFNCGraphicsSVGImport }

procedure TTMSFNCGraphicsSVGImport.AddArc(const AElement: TTMSFNCGraphicsSVGElement; const APath: TTMSFNCGraphicsPath; AStartX, AStopX, AStartY, AStopY, ARx, ARy, AXRot: Single; ALarge, ASweep: Integer);
var
  sinPhi, cosPhi, x1dash, y1dash: Single;
  cxdash, cydash, cx, cy: Single;
  root, num, rx, ry: Single;
  s: Single;
  theta1, theta2, dtheta: Single;
  delta, t: Single;
  stx, sty, dx1, dy1, dxe, dye, endpointX, endpointY: Single;
  seg, I: Integer;
  cosTheta1, sinTheta1: Single;
  cosTheta2, sinTheta2: Single;

  function CalculateVectorAngle(ux, uy, vx, vy: Single): Single;
  var
    ta, tb: Single;
  begin
    ta := ArcTan2(uy, ux);
    tb := ArcTan2(vy, vx);
    if tb >= ta then
      Result := tb - ta
    else
      Result := (Pi * 2) - (ta - tb);
  end;

begin
  if (AStartX = AStopX) and (AStartY = AStopY) then
    Exit;

  if (ARx = 0) and (ARy = 0) then
  begin
    APath.AddLine(PointF(AStartX, AStartY), PointF(AStopX, AStopY));
    Exit;
  end;

  sinPhi := Sin(DegToRad(AXRot));
  cosPhi := Cos(DegToRad(AXRot));
  x1dash := cosPhi * (AStartX - AStopX) / 2 + sinPhi * (AStartY - AStopY) / 2;
  y1dash := -sinPhi * (AStartX - AStopX) / 2 + cosPhi * (AStartY - AStopY) / 2;
  num := ARx * ARx * ARy * ARy - ARx * ARx * y1dash * y1dash - ARy * ARy * x1dash * x1dash;
  rx := ARx;
  ry := ARy;

  if num < 0 then
  begin
    s := Sqrt(1 - num / (ARx * ARx * ARy * ARy));
    rx := rx * s;
    ry := ry * s;
    root := 0;
  end
  else
  begin
    if ((ALarge = 1) and (ASweep = 1)) or ((ALarge = 0) and (ASweep = 0)) then
      root := -1
    else
      root := 1;

    root := root * Sqrt(num / (ARx * ARx * y1dash * y1dash + ARy * ARy * x1dash * x1dash));
  end;

  cxdash := root * rx * y1dash / ry;
  cydash := -root * ry * x1dash / rx;

  cx := cosPhi * cxdash - sinPhi * cydash + (AStartX + AStopX) / 2;
  cy := sinPhi * cxdash + cosPhi * cydash + (AStartY + AStopY) / 2;

  theta1 := CalculateVectorAngle(1, 0, (x1dash - cxdash) / rx, (y1dash - cydash) / ry);
  dtheta := CalculateVectorAngle((x1dash - cxdash) / rx, (y1dash - cydash) / ry, (-x1dash - cxdash) / rx, (-y1dash - cydash) / ry);

  if (ASweep = 0) and (dtheta > 0) then
    dtheta := dtheta - 2 * PI
  else if (ASweep = 1) and (dtheta < 0) then
    dtheta := dtheta + 2 * PI;

  seg := Ceil(Abs(dtheta / (Pi / 2)));
  delta := dtheta / seg;
  t := 8.0 / 3.0 * Sin(delta / 4.0) * Sin(delta / 4.0) / Sin(delta / 2.0);
  stx := AStartX;
  sty := AStartY;

  for I := 0 to seg - 1 do
  begin
    cosTheta1 := Cos(theta1);
    sinTheta1 := Sin(theta1);
    theta2 := theta1 + delta;
    cosTheta2 := Cos(theta2);
    sinTheta2 := Sin(theta2);

    endpointX := cosPhi * rx * cosTheta2 - sinPhi * ry * sinTheta2 + cx;
    endpointY := sinPhi * rx * cosTheta2 + cosPhi * ry * sinTheta2 + cy;

    dx1 := t * (-cosPhi * rx * sinTheta1 - sinPhi * ry * cosTheta1);
    dy1 := t * (-sinPhi * rx * sinTheta1 + cosPhi * ry * cosTheta1);

    dxe := t * (cosPhi * rx * sinTheta2 + sinPhi * ry * cosTheta2);
    dye := t * (sinPhi * rx * sinTheta2 - cosPhi * ry * cosTheta2);

    if APath.Count = 0 then
      APath.MoveTo(PointF(stx, sty))
    else
      APath.LineTo(PointF(stx, sty));

    APath.CurveTo(PointF(stx + dx1, sty + dy1), PointF(endpointx + dxe, endpointy + dye), PointF(endpointx, endpointy));

    theta1 := theta2;
    stx := endpointX;
    sty := endpointY;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.Clear;
begin
  FElementClassList.Clear;
  FElementPatternList.Clear;
  FElementClipPathList.Clear;
  FElementList.Clear;
  FDefsList.Clear;
  {$IFDEF LCLLIB}
  if Assigned(FXML) then
    FXML.Free;
  {$ENDIF}
  FXML := nil;
  FMatrix := MatrixIdentity;
  FTextMatrix := MatrixIdentity;
  ViewRect := RectF(MaxInt, MaxInt, -MaxInt, -MaxInt);
end;

function TTMSFNCGraphicsSVGImport.ApplyTintColor(AColor, ATintColor: TTMSFNCGraphicsColor): TTMSFNCGraphicsColor;
var
  {$IFDEF FMXLIB}
  a: Byte;
  {$ENDIF}
  r, g, b: Byte;
begin
  {$IFDEF FMXLIB}
  a := TTMSFNCGraphics.GetColorAlpha(AColor);
  {$ENDIF}
  r := (TTMSFNCGraphics.GetColorRed(AColor) * TTMSFNCGraphics.GetColorRed(ATintColor)) div 255;
  g := (TTMSFNCGraphics.GetColorGreen(AColor) * TTMSFNCGraphics.GetColorGreen(ATintColor)) div 255;
  b := (TTMSFNCGraphics.GetColorBlue(AColor) * TTMSFNCGraphics.GetColorBlue(ATintColor)) div 255;

  Result := MakeGraphicsColor(r, g, b{$IFDEF FMXLIB}, a{$ENDIF});
end;

function TTMSFNCGraphicsSVGImport.ConvertColorToGray(
  AColor: TTMSFNCGraphicsColor): TTMSFNCGraphicsColor;
var
  {$IFDEF FMXLIB}
  a: Byte;
  {$ENDIF}
  r, g, b, gr: Byte;
begin
  {$IFDEF FMXLIB}
  a := TTMSFNCGraphics.GetColorAlpha(AColor);
  {$ENDIF}
  r := TTMSFNCGraphics.GetColorRed(AColor);
  g := TTMSFNCGraphics.GetColorGreen(AColor);
  b := TTMSFNCGraphics.GetColorBlue(AColor);
  gr := Round(0.3 * r + 0.6 * g + 0.1 * b);
  Result := MakeGraphicsColor(gr, gr, gr{$IFDEF FMXLIB}, a{$ENDIF});
end;

constructor TTMSFNCGraphicsSVGImport.Create;
begin
  inherited;
  FElementList := TTMSFNCGraphicsSVGElementList.Create;
  FElementClassList := TTMSFNCGraphicsSVGElementClassList.Create;
  FElementPatternList := TTMSFNCGraphicsSVGElementClassList.Create;
  FElementClipPathList := TTMSFNCGraphicsSVGElementClassList.Create;
  FDefsList := TTMSFNCGraphicsSVGElementList.Create;
  Clear;
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 23}
  {$IFDEF MSWINDOWS}
  MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
end;

destructor TTMSFNCGraphicsSVGImport.Destroy;
begin
  {$IFDEF LCLLIB}
  FXML.Free;
  {$ENDIF}
  FXML := nil;

  if Assigned(FCachedBitmap) then
    FCachedBitmap.Free;

  if Assigned(FElementClassList) then
    FElementClassList.Free;

  if Assigned(FElementPatternList) then
    FElementPatternList.Free;

  if Assigned(FElementClipPathList) then
    FElementClipPathList.Free;

  if Assigned(FElementList) then
    FElementList.Free;

  if Assigned(FDefsList) then
    FDefsList.Free;

  inherited;
end;

procedure TTMSFNCGraphicsSVGImport.Draw(const ACanvas: TCanvas; ARect: TRectF; ANative: Boolean = False);
var
  rv, r: TRectF;
  sx, sy: Single;
  g: TTMSFNCGraphics;
begin
  if not HasElements then
    Exit;

  r := ARect;
  rv := ViewRect;
  if (rv.Right - rv.Left > 0) and (rv.Bottom - rv.Top > 0) then
  begin
    sx := r.Width / (rv.Right - rv.Left);
    sy := r.Height / (rv.Bottom - rv.Top);

    if (sx = 0) or (sy = 0) then
      Exit;

    FMatrix := MatrixCreateTranslation(-rv.Width / 2, -rv.Height / 2) * MatrixCreateRotation(DegToRad(RotationAngle)) * MatrixCreateTranslation(rv.Width / 2, rv.Height / 2);
    FMatrix := MatrixMultiply(FMatrix, MatrixMultiply(MatrixCreateTranslation(-rv.Left, -rv.Top), MatrixMultiply(MatrixCreateTranslation(Floor(r.Left / sx), Floor(r.Top / sy)), MatrixCreateScaling(sx, sy))));

    g := TTMSFNCGraphics.Create(ACanvas, ANative);
    try
      if ANative then
      begin
        g.Context.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
        g.Context.SetAntiAliasing(True);
        {$IFDEF MSWINDOWS}
        TGPGraphics(g.Context.NativeCanvas).SetPixelOffsetMode(PixelOffsetModeHalf);
        TGPGraphics(g.Context.NativeCanvas).SetCompositingQuality(CompositingQualityHighQuality);
        {$ENDIF}
        g.BeginScene;
      end;

      InternalDraw(FElementList, g, sx, sy);

      if ANative then
        g.EndScene;
    finally
      if ANative then
        g.Context.Render;
      g.Free;
    end;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.Draw(const AGraphics: TTMSFNCGraphics; ARect: TRectF; ANative: Boolean = False; ARecreateGraphics: Boolean = True);
var
  rv, r: TRectF;
  sx, sy: Single;
  g: TTMSFNCGraphics;
begin
  if not HasElements then
    Exit;

  r := ARect;
  rv := ViewRect;
  if (rv.Right - rv.Left > 0) and (rv.Bottom - rv.Top > 0) then
  begin
    sx := r.Width / (rv.Right - rv.Left);
    sy := r.Height / (rv.Bottom - rv.Top);

    if (sx = 0) or (sy = 0) then
      Exit;

    FMatrix := MatrixCreateTranslation(-rv.Width / 2, -rv.Height / 2) * MatrixCreateRotation(DegToRad(RotationAngle)) * MatrixCreateTranslation(rv.Width / 2, rv.Height / 2);
    FMatrix := MatrixMultiply(FMatrix, MatrixMultiply(MatrixCreateTranslation(-rv.Left, -rv.Top), MatrixMultiply(MatrixCreateTranslation(r.Left / sx, r.Top / sy), MatrixCreateScaling(sx, sy))));
    {$IFDEF CMNLIB}
    if ANative and ARecreateGraphics then
      FMatrix := MatrixMultiply(FMatrix, AGraphics.Context.GetMatrix);
    {$ENDIF}

    {$IFDEF FMXLIB}
    FTextMatrix := AGraphics.Context.GetMatrix;
    {$ENDIF}

    if ARecreateGraphics then
      g := TTMSFNCGraphics.Create(AGraphics.Canvas, ANative)
    else
      g := AGraphics;

    try
      if ANative and ARecreateGraphics then
      begin
        g.Context.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
        g.Context.SetAntiAliasing(True);
        {$IFDEF MSWINDOWS}
        TGPGraphics(g.Context.NativeCanvas).SetPixelOffsetMode(PixelOffsetModeHalf);
        TGPGraphics(g.Context.NativeCanvas).SetCompositingQuality(CompositingQualityHighQuality);
        {$ENDIF}
        g.BeginScene;
      end;

      InternalDraw(FElementList, g, sx, sy);

      if ANative and ARecreateGraphics then
        g.EndScene;
    finally
      if ANative and ARecreateGraphics then
        g.Context.Render;

      if ARecreateGraphics then
        g.Free;
    end;
  end;
end;

function TTMSFNCGraphicsSVGImport.ElementCount: Integer;
begin
  Result := FElementList.Count;
end;

procedure TTMSFNCGraphicsSVGImport.InternalDraw(const AElements: TTMSFNCGraphicsSVGElementList; const AGraphics: TTMSFNCGraphics; const AScaleX: Single; const AScaleY: Single);
var
  I: Integer;
  e, est, ec: TTMSFNCGraphicsSVGElement;
  st: TTMSFNCGraphicsSaveState;
  m, ma, ms: TTMSFNCGraphicsMatrix;
  pth, pthc, cp: TTMSFNCGraphicsPath;

  function AngleOfLine(const P1, P2: TPointF): Double;
  begin
    Result := RadToDeg(ArcTan2((P2.Y - P1.Y),(P2.X - P1.X)));
    if Result < 0 then
      Result := Result + 360;
  end;

  procedure ApplyFillColor(AElement: TTMSFNCGraphicsSVGElement);
  var
    es, ep: TTMSFNCGraphicsSVGElement;
    K: Integer;
    r: TRectF;
    rdp, rdpth: TTMSFNCGraphicsPath;
    cx, cy: Double;
  begin
    if Assigned(e.FPatternRef) then
    begin
      ep := e.FPatternRef;
      if Assigned(ep.FHRefRef) then
        ep := ep.FHRefRef;

      AGraphics.Fill.BeginUpdate;
      AGraphics.Fill.Kind := gfkGradient;
      AGraphics.Fill.GradientMode := gfgmCollection;
      AGraphics.Fill.GradientItems.Clear;
//      AGraphics.Fill.GradientMatrix := e.FPatternRef.GradientMatrix;

      case ep.&Type of
        etLinearGradient:
        begin
          AGraphics.Fill.GradientType := gfgtLinear;
          AGraphics.Fill.GradientOrientation := gfoCustom;
          AGraphics.Fill.GradientAngle := AngleOfLine(PointF(ep.X, ep.Y), PointF(ep.Width, ep.Height));
        end;
        etRadialGradient:
        begin
          AGraphics.Fill.GradientType := gfgtRadial;

          rdp := TTMSFNCGraphicsPath.Create;
          try
            case e.&Type of
              etCircle, etEllipse: rdp.AddEllipse(RectF(e.X - e.Rx, e.Y - e.Ry, e.X + e.Rx, e.Y + e.Ry));
              etLine:
              begin
                rdp.MoveTo(PointF(e.X, e.Y));
                rdp.LineTo(PointF(e.Width, e.Height));
              end;
              etRect: rdp.AddRectangle(RectF(e.X, e.Y, e.X + e.Width, e.Y + e.Height), e.Rx, e.Ry);
              etPath, etPolyline, etPolygon:
              begin
                if e.HasPaths then
                begin
                  rdpth := e.CombinedPath;
                  rdp.AddPath(rdpth);
                end;
              end;
            end;

            rdp.ApplyMatrix(ma);
            r := rdp.GetBounds;
          finally
            rdp.Free;
          end;

          cx := 50;
          cy := 50;

          if ep.FHasCX then
            cx := ep.FCX;

          if ep.FHasCY then
            cy := ep.FCY;

          AGraphics.Fill.GradientCenterPoint := PointF(r.Left + r.Width * cx / 100, r.Top + r.Height * cy / 100);
        end;
      end;

      for K := 0 to ep.Elements.Count - 1 do
        if ep.Elements[K].&Type = etStop then
          AGraphics.Fill.AddGradientItem(ep.Elements[K].StopColor, ep.Elements[K].Offset, ep.Elements[K].StopOpacity);

      AGraphics.Fill.EndUpdate;
    end
    else
    begin
      es := AElement;
      while Assigned(es) and Assigned(es.Parent) and (es.FillColorValue = 'default') do
        es := es.Parent;

      if Assigned(es) then
      begin
        if es.HasFillColor then
        begin
          AGraphics.Fill.Kind := gfkSolid;
          if GrayScale then
            AGraphics.Fill.Color := ConvertColorToGray(es.FillColor)
          else if TintColor <> gcNull then
            AGraphics.Fill.Color := ApplyTintColor(es.FillColor, TintColor)
          else if CustomFillColor <> gcNull then
            AGraphics.Fill.Color := CustomFillColor
          else
            AGraphics.Fill.Color := es.FillColor;
        end
        else if es.FillColorValue = 'default' then
        begin
          AGraphics.Fill.Kind := gfkSolid;
          AGraphics.Fill.Color := gcBlack;

          if GrayScale then
            AGraphics.Fill.Color := gcGray
          else if TintColor <> gcNull then
            AGraphics.Fill.Color := TintColor
          else if CustomFillColor <> gcNull then
            AGraphics.Fill.Color := CustomFillColor;
        end
        else
          AGraphics.Fill.Kind := gfkNone;
      end;
    end;
  end;

  procedure ApplyStrokeWidth(AElement: TTMSFNCGraphicsSVGElement);
  var
    es: TTMSFNCGraphicsSVGElement;
  begin
    es := AElement;
    while Assigned(es) and Assigned(es.Parent) and (es.StrokeWidthValue = 'default') do
      es := es.Parent;

    if Assigned(es) then
    begin
      if es.HasStrokeWidth then
      begin
        AGraphics.Stroke.Width := Max(0, es.StrokeWidth * AScaleX);
      end
      else
        AGraphics.Stroke.Width := 1;
    end;
  end;

  procedure ApplyOpacity(AElement: TTMSFNCGraphicsSVGElement);
  var
    es: TTMSFNCGraphicsSVGElement;
  begin
    es := AElement;
    while Assigned(es) and Assigned(es.Parent) and (es.OpacityValue = 'default') do
      es := es.Parent;

    if Assigned(es) then
    begin
      if CustomOpacity = -1 then
      begin
        if es.HasOpacity then
        begin
          AGraphics.Fill.Opacity := es.Opacity;
          AGraphics.Stroke.Opacity := es.Opacity;
        end
        else
        begin
          AGraphics.Fill.Opacity := 1;
          AGraphics.Stroke.Opacity := 1;
        end;
      end
      else
      begin
        AGraphics.Fill.Opacity := CustomOpacity;
        AGraphics.Stroke.Opacity := CustomOpacity;
      end;
    end;
  end;

  procedure ApplyStrokeColor(AElement: TTMSFNCGraphicsSVGElement);
  var
    es: TTMSFNCGraphicsSVGElement;
  begin
    es := AElement;
    while Assigned(es) and Assigned(es.Parent) and (es.StrokeColorValue = 'default') do
      es := es.Parent;

    if Assigned(es) then
    begin
      if es.HasStrokeColor then
      begin
        AGraphics.Stroke.Kind := gskSolid;
        if GrayScale then
          AGraphics.Stroke.Color := ConvertColorToGray(es.StrokeColor)
        else if TintColor <> gcNull then
          AGraphics.Stroke.Color := ApplyTintColor(es.StrokeColor, TintColor)
        else if CustomStrokeColor <> gcNull then
          AGraphics.Stroke.Color := CustomStrokeColor
        else
          AGraphics.Stroke.Color := es.StrokeColor;
      end
      else
        AGraphics.Stroke.Kind := gskNone;
    end;
  end;

  function GetMatrix(AElement: TTMSFNCGraphicsSVGElement): TTMSFNCGraphicsMatrix;
  var
    es: TTMSFNCGraphicsSVGElement;
  begin
    Result := MatrixIdentity;
    es := AElement;

    if Assigned(es) and not IsMatrixEmpty(es.Matrix) then
      Result := MatrixMultiply(Result, es.Matrix);

    while Assigned(es) and Assigned(es.Parent) and (es.Parent.&Type in [etContainer, etTextSpan, etTextPath, etText]) do
    begin
      if not IsMatrixEmpty(es.Parent.Matrix) then
        Result := MatrixMultiply(Result, es.Parent.Matrix);

      es := es.Parent;
    end;
  end;

begin
  for I := 0 to AElements.Count - 1 do
  begin
    e := AElements[I];

    st := AGraphics.SaveState;

    m := GetMatrix(e);
    ma := MatrixMultiply(m, FMatrix);

    est := e;

    if Assigned(e.ClassRef) then
      est := e.ClassRef;

    if est.DisplayValue = 'none' then
      Exit;

    ApplyFillColor(est);
    ApplyStrokeColor(est);
    ApplyStrokeWidth(est);
    ApplyOpacity(est);

    if Assigned(e.ClipPathRef) then
    begin
      ec := e.ClipPathRef;
      cp := TTMSFNCGraphicsPath.Create;
      try
        case ec.&Type of
          etCircle, etEllipse: cp.AddEllipse(RectF(ec.X - ec.Rx, ec.Y - ec.Ry, ec.X + ec.Rx, ec.Y + ec.Ry));
          etLine:
          begin
            cp.MoveTo(PointF(ec.X, ec.Y));
            cp.LineTo(PointF(ec.Width, ec.Height));
          end;
          etRect: cp.AddRectangle(RectF(ec.X, ec.Y, ec.X + ec.Width, ec.Y + ec.Height), ec.Rx, ec.Ry);
          etPath, etPolyline, etPolygon:
          begin
            if ec.HasPaths then
            begin
              pth := ec.CombinedPath;
              cp.AddPath(pth);
            end;
          end;
        end;

        cp.ApplyMatrix(ma);
        AGraphics.Context.ClipPath(cp);
      finally
        cp.Free;
      end;
    end;

    case e.&Type of
      etContainer:
      begin
        if e.HasElements then
          InternalDraw(e.Elements, AGraphics, AScaleX, AScaleY);
      end;
      etRect, etCircle, etEllipse, etLine:
      begin
        pthc := TTMSFNCGraphicsPath.Create;
        try
          case e.&Type of
            etRect: pthc.AddRectangle(RectF(e.X, e.Y, e.X + e.Width, e.Y + e.Height), e.Rx, e.Ry);
            etEllipse, etCircle: pthc.AddEllipse(RectF(e.X - e.Rx, e.Y - e.Ry, e.X + e.Rx, e.Y + e.Ry));
            etLine:
            begin
              pthc.MoveTo(PointF(e.X, e.Y));
              pthc.LineTo(PointF(e.Width, e.Height));
            end;
          end;
          pthc.ApplyMatrix(ma);
          AGraphics.DrawPath(pthc, pdmPath);
        finally
          pthc.Free;
        end;
      end;
      etPath, etPolyline, etPolygon:
      begin
        if e.HasPaths then
        begin
          pth := e.CombinedPath;
          pthc := TTMSFNCGraphicsPath.Create;
          try
            pthc.AddPath(pth);
            pthc.ApplyMatrix(ma);
            AGraphics.DrawPath(pthc, pdmPath);
          finally
            pthc.Free;
          end;
        end;
      end;
      etImage:
      begin
        if e.HasBitmap then
        begin
          ms := AGraphics.GetMatrix;
          AGraphics.SetMatrix(ma);
          case e.PreserveAspectRatio of
            parNone: AGraphics.DrawBitmap(RectF(e.X, e.Y, e.X + e.Width, e.Y + e.Height), e.Bitmap, False, True);
            parOther: AGraphics.DrawBitmap(RectF(e.X, e.Y, e.X + e.Width, e.Y + e.Height), e.Bitmap);
          end;
          AGraphics.SetMatrix(ms);
        end;
      end;
      etText, etTextSpan, etTextSpanChild, etTextPath:
      begin
        if e.&Type = etText then
        begin
          if e.FontFamily <> '' then
            AGraphics.Font.Name := e.FontFamily
          else
            AGraphics.Font.Name := 'Arial';

          if (e.FontSize <> 0) and e.HasFontSize then
            TTMSFNCUtils.SetFontSize(AGraphics.Font, e.FontSize)
          else
            TTMSFNCUtils.SetFontSize(AGraphics.Font, 12);

          AGraphics.Font.Style := e.FontStyle;
          AGraphics.Font.Color := e.FillColor;
        end;

        if e.Text <> '' then
        begin
          ms := AGraphics.GetMatrix;
          AGraphics.SetMatrix(MatrixMultiply(ma, FTextMatrix));
          if Assigned(e.Parent) and (e.Parent.&Type = etTextSpan) and (e.&Type = etTextSpanChild) then
            AGraphics.DrawText(PointF(e.Parent.X, e.Parent.Y - AGraphics.CalculateTextHeight(e.Text)), e.Text)
          else
            AGraphics.DrawText(PointF(e.X, e.Y - AGraphics.CalculateTextHeight(e.Text)), e.Text);

          AGraphics.SetMatrix(ms);
        end;

        if e.HasElements then
          InternalDraw(e.Elements, AGraphics, AScaleX, AScaleY);
      end;
      etClipPath: ;
    end;

    AGraphics.RestoreState(st);
  end;
end;

function FindNode(const ANode: TTMSFNCGraphicsSVGXMLNode; const ANodeName: string): TTMSFNCGraphicsSVGXMLNode;
begin
  {$IFNDEF LCLLIB}
  Result := ANode.AttributeNodes.FindNode(ANodeName);
  {$ELSE}
  Result := ANode.Attributes.GetNamedItem(ANodeName);
  {$ENDIF}
end;

procedure TTMSFNCGraphicsSVGImport.ParseString(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AValue: string);
var
  a: TTMSFNCGraphicsSVGXMLNode;
begin
  a := FindNode(ANode, AString);
  if Assigned(a) then
    AValue := NodeToString(a{$IFDEF LCLLIB}.FirstChild{$ENDIF});
end;

procedure TTMSFNCGraphicsSVGImport.ParseTransform(const ANode: TTMSFNCGraphicsSVGXMLNode;
  const AString: string; var AMatrix: TTMSFNCGraphicsMatrix);
var
  a: TTMSFNCGraphicsSVGXMLNode;
  s: string;
  st, stp: Integer;
  t: string;
  v: string;
  M: TTMSFNCGraphicsMatrix;
  p: TPointF;
  b: Boolean;

  function ParseAngle(const Angle: string): Single;
  var
    D: Single;
    C: Integer;
    S: string;
  begin
    if Angle <> '' then
    begin
      S := Angle;
      C := Pos('deg', S);
      if C <> 0 then
      begin
        S := LeftStr(S, C - 1);
        if TryStrToFloat(S, D) then
          Result := DegToRad(D)
        else
          Result := 0;
        Exit;
      end;

      C := Pos('rad', S);
      if C <> 0 then
      begin
        TryStrToFloat(S, Result);
        Exit;
      end;

      C := Pos('grad', S);
      if C <> 0 then
      begin
        S := LeftStr(S, C - 1);
        if TryStrToFloat(S, D) then
          Result := GradToRad(D)
        else
          Result := 0;
        Exit;
      end;

      if TryStrToFloat(S, D) then
        Result := DegToRad(D)
      else
        Result := 0;
    end else
      Result := 0;
  end;

  function ParseMatrix(const S: string): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
  begin
    Result := MatrixIdentity;
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);

    try
      if SL.Count = 6 then
      begin
        TryStrToFloat(SL[0], Result.m11);
        TryStrToFloat(SL[1], Result.m12);
        TryStrToFloat(SL[2], Result.m21);
        TryStrToFloat(SL[3], Result.m22);
        TryStrToFloat(SL[4], Result.m31);
        TryStrToFloat(SL[5], Result.m32);
      end;
    finally
      SL.Free;
    end;
  end;

  function ParseTranslate(const S: string; var ATranslatePoint: TPointF; var AHasTranslated: Boolean): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
    dx, dy: Single;
  begin
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);

    try
      if SL.Count = 1 then
        SL.Add('0');

      if SL.Count = 2 then
      begin
        TryStrToFloat(SL[0], dx);
        TryStrToFloat(SL[1], dy);
        Result := MatrixCreateTranslation(dx, dy);
        ATranslatePoint := PointF(dx, dy);
        AHasTranslated := True;
      end;
    finally
      SL.Free;
    end;
  end;

  function ParseScale(const S: string): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
    sx, sy: Single;
  begin
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);
    try
      if SL.Count = 1 then
        SL.Add(SL[0]);
      if SL.Count = 2 then
      begin
        TryStrToFloat(SL[0], sx);
        TryStrToFloat(SL[1], sy);
        Result := MatrixCreateScaling(sx, sy);
      end;
    finally
      SL.Free;
    end;
  end;

  function ParseRotation(const S: string; ATranslatePoint: TPointF; ATranslate: Boolean): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
    X, Y, a: Single;
  begin
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);
    try
      a := ParseAngle(SL[0]);

      if SL.Count = 3 then
      begin
        TryStrToFloat(SL[1], x);
        TryStrToFloat(SL[2], y);
      end
      else if ATranslate then
      begin
        X := ATranslatePoint.X;
        Y := ATranslatePoint.Y;
      end
      else
      begin
        X := 0;
        Y := 0;
      end;
    finally
      SL.Free;
    end;

    Result := MatrixCreateTranslation(X, Y);
    Result := MatrixMultiply(MatrixCreateRotation(a), Result);
    Result := MatrixMultiply(MatrixCreateTranslation(-X, -Y), Result);
  end;

  function ParseSkewX(const S: string): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
    t: Single;
  begin
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);
    try
      if SL.Count = 1 then
      begin
        Result := MatrixIdentity;
        TryStrToFloat(SL[0], t);
        Result.m21 := Tan(t);
      end;
    finally
      SL.Free;
    end;
  end;

  function ParseSkewY(const S: string): TTMSFNCGraphicsMatrix;
  var
    SL: TStrings;
    t: Single;
  begin
    SL := TStringList.Create;
    TTMSFNCUtils.Split(',', S, SL, True);
    try
      if SL.Count = 1 then
      begin
        Result := MatrixIdentity;
        TryStrToFloat(SL[0], t);
        Result.m12 := Tan(t);
      end;
    finally
      SL.Free;
    end;
  end;
begin
  a := FindNode(ANode, AString);
  if Assigned(a) then
  begin
    AMatrix := MatrixIdentity;

    s := NodetoString(a{$IFDEF LCLLIB}.FirstChild{$ENDIF});

    Trim(s);

    b := False;
    p := PointF(0, 0);

    while S <> '' do
    begin
      st := Pos('(', S);
      stp := Pos(')', S);
      if (st = 0) or (stp = 0) then
        Exit;

      t := Copy(S, 1, st - 1);
      v := Trim(Copy(S, st + 1, stp - st - 1));
      v := StringReplace(v, ' ', ',', [rfReplaceAll]);
      M := MatrixIdentity;

      if t = 'matrix' then
        M := ParseMatrix(v)
      else if t = 'translate' then
        M := ParseTranslate(v, p, b)
      else if t = 'scale' then
        M := ParseScale(v)
      else if t = 'rotate' then
        M := ParseRotation(v, p, b)
      else if t = 'skewX' then
        M := ParseSkewX(v)
      else if t = 'skewY' then
        M := ParseSkewY(v);

      AMatrix := MatrixMultiply(AMatrix, M);

      S := Trim(Copy(S, stp + 1, Length(S)));
    end;
  end
  else
    AMatrix := MatrixEmpty;
end;

function TTMSFNCGraphicsSVGImport.NameToElementType(const AName: string): TTMSFNCGraphicsSVGElementType;
begin
  Result := etUnknown;
  if AName = 'g' then
    Result := etContainer
  else if AName = 'switch' then
    Result := etSwitch
  else if AName = 'defs' then
    Result := etDefs
  else if AName = 'stop' then
    Result := etStop
  else if AName = 'style' then
    Result := etStyle
  else if AName = 'pattern' then
    Result := etPattern
  else if AName = 'use' then
    Result := etUse
  else if AName = 'rect' then
    Result := etRect
  else if AName = 'line' then
    Result := etLine
  else if AName = 'polyline' then
    Result := etPolyline
  else if AName = 'polygon' then
    Result := etPolygon
  else if AName = 'circle' then
    Result := etCircle
  else if AName = 'ellipse' then
    Result := etEllipse
  else if AName = 'path' then
    Result := etPath
  else if AName = 'image' then
    Result := etImage
  else if AName = 'text' then
    Result := etText
  else if (AName = 'tspan') then
    Result := etTextSpan
  else if (AName = '#text') then
    Result := etTextSpanChild
  else if AName = 'textPath' then
    Result := etTextPath
  else if AName = 'clipPath' then
    Result := etClipPath
  else if AName = 'linearGradient' then
    Result := etLinearGradient
  else if AName = 'radialGradient' then
    Result := etRadialGradient;
end;

function TTMSFNCGraphicsSVGImport.NodeToString(
  ANode: TTMSFNCGraphicsSVGXMLNode): string;
begin
  Result := '';
  if not Assigned(ANode) then
    Exit;

  {$IFNDEF LCLLIB}
  if VarType(ANode.NodeValue) <> varNull then
  {$ENDIF}
    Result := ANode.NodeValue;
end;

procedure TTMSFNCGraphicsSVGImport.FixCurve(const ACommand: Char; SL: TStrings);
var
  C: Integer;
  D: Integer;
  cmd: Char;
  CC: Char;
begin
  CC := ACommand;

  case CC of
    'M': cmd := 'L';
    'm': cmd := 'l';
  else
    cmd := CC;
  end;

  case cmd of
    'A', 'a': D := 7;
    'C', 'c': D := 6;
    'S', 's', 'Q', 'q': D := 4;
    'T', 't', 'M', 'm', 'L', 'l': D := 2;
    'H', 'h', 'V', 'v': D := 1;
  else
    D := 0;
  end;

  if (D = 0) or (SL.Count = D + 1) or ((SL.Count - 1) mod D = 1) then
    Exit;

  for C := SL.Count - D downto (D + 1) do
  begin
    if (C - 1) mod D = 0 then
      SL.Insert(C, cmd);
  end;
end;

function TTMSFNCGraphicsSVGImport.GenerateBitmap(AWidth: Single = - 1; AHeight: Single = -1): TTMSFNCBitmap;
var
  g: TTMSFNCGraphics;
  r: TRectF;
begin
  Result := nil;

  if not HasElements then
    Exit;

  if Assigned(FCachedBitmap) then
    FCachedBitmap.Free;

  FCachedBitmap := TTMSFNCBitmap.Create;

  r := ViewRect;
  if (r.Width > 0) and (r.Height > 0) then
  begin
    if not (CompareValue(r.Left, MaxInt) = EqualsValue) and not (CompareValue(r.Top, MaxInt) = EqualsValue)
      and not (CompareValue(r.Right, -MaxInt) = EqualsValue) and not (CompareValue(r.Bottom, -MaxInt) = EqualsValue) then
    begin
      if (AWidth > -1) and (AHeight > -1) then
         r := RectF(0, 0, AWidth, AHeight);

      g := TTMSFNCGraphics.CreateBitmapCanvas(Round(r.Right), Round(r.Bottom));
      try
        g.BeginScene;
        Draw(g, r);
        FCachedBitmap.Assign(g.Bitmap);
        g.EndScene;
      finally
        g.Free;
      end;
    end;
  end;

  Result := FCachedBitmap;
end;

function TTMSFNCGraphicsSVGImport.HasElements: Boolean;
begin
  Result := ElementCount > 0;
end;

function TTMSFNCGraphicsSVGImport.ProcessValues(const ACommand: Char;
  const S: string): TStrings;
var
  ns: string;
  C: Char;
  hd, he: Boolean;
  CC: Char;
begin
  CC := ACommand;

  ns := '';
  hd := False;
  he := False;

  Result := TStringList.Create;

  for C in S do
  begin
    case C of
      'e', 'E':
      begin
        if he then
        begin
          he := (C = 'e') or (C = 'E');
          Result.Add(ns);
          ns := C;
        end
        else
        begin
          ns := ns + C;
          he := True;
        end;
      end;
      '.':
      begin
        if hd then
        begin
          hd := C = '.';
          Result.Add(ns);
          ns := C;
        end
        else
        begin
          ns := ns + C;
          hd := True;
        end;
      end;
      '0'..'9': ns := ns + C;
      '+', '-':
      begin
        if he then
        begin
          ns := ns + C
        end
        else
        begin
          if ns <> '' then
          begin
            Result.Add(ns);
            hd := False;
            he := False;
          end;
          ns := C;
        end;
      end;
      ' ', #9, #$A, #$D:
      begin
        if ns <> '' then
        begin
          Result.Add(ns);
          ns := '';
          hd := False;
          he := False;
        end;
      end;
    end;
  end;

  if ns <> '' then
    Result.Add(ns);

  Result.Insert(0, CC);

  if Result.Count > 0 then
  begin
    if TTMSFNCUtils.CharInSet(CC, TTMSFNCUtils.CreateCharSet('MmLlHhVvCcSsQqTtAa')) then
      FixCurve(ACommand, Result)
    else if (CC = 'Z') or (CC = 'z') then
    begin
      while Result.Count > 1 do
        Result.Delete(1);
    end;
  end;
end;

function TTMSFNCGraphicsSVGImport.IndexOfAny(const AValue: string; const AnyOf: array of Char; StartIndex, Count: Integer): Integer;
var
  I: Integer;
  C: Char;
  Max: Integer;
begin
  if (StartIndex + Count) >= Length(AValue) then
    Max := Length(AValue)
  else
    Max := StartIndex + Count;

  I := StartIndex;
  while I < Max do
  begin
    for C in AnyOf do
    begin
      if AValue[I{$IFNDEF ZEROSTRINGINDEX} + 1{$ENDIF}] = C then
        Exit(I);
    end;
    Inc(I);
  end;
  Result := -1;
end;

procedure TTMSFNCGraphicsSVGImport.SaveToFile(const AFile: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveToStream(ms);
    ms.SaveToFile(AFile);
  finally
    ms.Free;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.SaveToStream(const AStream: TStream);
begin
  if Assigned(FXML) then
  {$IFNDEF LCLLIB}
    FXML.SaveToStream(AStream);
  {$ENDIF}
  {$IFDEF LCLLIB}
  if Assigned(FXML) then
    WriteXMLFile(FXML, AStream);
  {$ENDIF}
end;

function TTMSFNCGraphicsSVGImport.SplitPath(const S: string): TStrings;
var
  si: Integer;
  SL: TStrings;
  F: Integer;
  P: string;
  sli: Integer;
const
  IDs: array [0..19] of Char = ('M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
    'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z');
begin
  Result := TStringList.Create;

  si := 0;
  sli := Length(S);
  while si < sli do
  begin
    f := IndexOfAny(S, IDs, si + 1, Length(S));
    if f = -1 then
      f := sli;

    p := Trim(Copy(s, si + 2, f - si - 1));
    SL := ProcessValues(S[si{$IFNDEF ZEROSTRINGINDEX}+ 1{$ENDIF}], p);
    Result.AddStrings(SL);
    SL.Free;
    si := f;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.UpdateViewRect(const AElement: TTMSFNCGraphicsSVGElement; const ARect: TRectF);
var
  pth: TTMSFNCGraphicsPath;
  r, rv: TRectF;
  m: TTMSFNCGraphicsMatrix;

  function GetMatrix(AElement: TTMSFNCGraphicsSVGElement): TTMSFNCGraphicsMatrix;
  var
    es: TTMSFNCGraphicsSVGElement;
  begin
    Result := MatrixIdentity;
    es := AElement;

    while Assigned(es) and Assigned(es.Parent) and (es.Parent.&Type in [etContainer, etTextSpan, etTextPath, etText]) do
    begin
      if not IsMatrixEmpty(es.Parent.Matrix) then
        Result := MatrixMultiply(Result, es.Parent.Matrix);

      es := es.Parent;
    end;
  end;

begin
  m := GetMatrix(AElement);

  pth := TTMSFNCGraphicsPath.Create;
  try
    pth.AddRectangle(ARect);
    pth.ApplyMatrix(m);
    r := pth.GetBounds;
  finally
    pth.Free;
  end;

  rv := ViewRect;

  if r.Left < rv.Left then
    rv.Left := r.Left;

  if r.Top < rv.Top then
    rv.Top := r.Top;

  if r.Right > rv.Right then
    rv.Right := r.Right;

  if r.Bottom > rv.Bottom then
    rv.Bottom := r.Bottom;

  ViewRect := rv;
end;

function GetUnitFactor(const AUnit: string): Single;
begin
  Result := 1;
  if AUnit = 'pt' then
    Result := 1.25
  else if AUnit = 'pc' then
    Result := 15
  else if AUnit = 'mm' then
    Result := 10
  else if AUnit = 'cm' then
    Result := 100
  else if AUnit = 'in' then
    Result := 25.4;
end;

function GetUnit(const S: string): string;
begin
  Result := '';
  if Copy(S, Length(S) - 1, 2) = 'px' then
  begin
    Result := 'px';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'pt' then
  begin
    Result := 'pt';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'pc' then
  begin
    Result := 'pc';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'mm' then
  begin
    Result := 'mm';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'cm' then
  begin
    Result := 'cm';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'in' then
  begin
    Result := 'in';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'em' then
  begin
    Result := 'em';
    Exit;
  end;
  if Copy(S, Length(S) - 1, 2) = 'ex' then
  begin
    Result := 'ex';
    Exit;
  end;
  if Copy(S, Length(S), 1) = '%' then
  begin
    Result := '%';
    Exit;
  end;
end;
function GetPreserveAspectRatio(const S: string): TTMSFNCGraphicsSVGElementPreserveAspectRatio;
begin
  Result := parOther;
  if S = 'none' then
    Result := parNone;
end;
function GetRealValue(const S: string): Single;
var
  U: string;
  su: string;
  f: Single;
begin
  if S = '' then
  begin
    Result := 0;
    Exit;
  end;
  su := GetUnit(S);
  if su = '%' then
    U := Copy(S, Length(S), 1)
  else
    if su <> '' then
      U := Copy(S, Length(S) - 1, 2);
  f := GetUnitFactor(su);
  if U = 'px' then
    TryStrToFloat(Copy(S, 1, Length(S) - 2), Result)
  else if U = '%' then
  begin
    TryStrToFloat(Copy(S, 1, Length(S) - 1), Result);
    Result := Result * f;
  end
  else if U <> '' then
  begin
    TryStrToFloat(Copy(S, 1, Length(S) - 2), Result);
    Result := Result * f;
  end
  else
    TryStrToFloat(S, Result);
end;
procedure ParseFont(AValue: string; var AFontName: string; var AFontStyle: TFontStyles);
var
  b, i: Integer;
  FN: string;
  sl: TStringList;
begin
  b := Pos('Bold', AValue);
  i := Pos('Italic', AValue);
  FN := AValue;
  // Check for b
  if b <> 0 then
  begin
    AFontName := Copy(FN, 1, b - 1) + Copy(FN, b + 4, MaxInt);
    if Copy(AFontName, Length(AFontName), 1) = '-' then
      AFontName := Copy(AFontName, 1, Length(AFontName) - 1);
    if Copy(AFontName, Length(AFontName) - 1, 2) = 'MT' then
    begin
      AFontName := Copy(AFontName, 1, Length(AFontName) - 2);
      if Copy(AFontName, Length(AFontName), 1) = '-' then
        AFontName := Copy(AFontName, 1, Length(AFontName) - 1);
    end;
    AFontStyle := AFontStyle + [TFontStyle.fsBold];
  end;
  // Check for i
  if i <> 0 then
  begin
    AFontName := Copy(FN, 1, i - 1) + Copy(FN, i + 6, MaxInt);
    if Copy(AFontName, Length(AFontName), 1) = '-' then
      AFontName := Copy(AFontName, 1, Length(AFontName) - 1);
    if Copy(AFontName, Length(AFontName) - 1, 2) = 'MT' then
    begin
      AFontName := Copy(AFontName, 1, Length(AFontName) - 2);
      if Copy(AFontName, Length(AFontName), 1) = '-' then
        AFontName := Copy(AFontName, 1, Length(AFontName) - 1);
    end;
    AFontStyle := AFontStyle + [TFontStyle.fsItalic];
  end;
  AFontName := FN;
  if Copy(AFontName, Length(AFontName) - 1, 2) = 'MT' then
  begin
    AFontName := Copy(AFontName, 1, Length(AFontName) - 2);
    if Copy(AFontName, Length(AFontName), 1) = '-' then
      AFontName := Copy(AFontName, 1, Length(AFontName) - 1);
  end;
  sl := TStringList.Create;
  try
    TTMSFNCUtils.Split(',', AFontName, sl);
    if sl.Count > 1 then
      AFontName := Trim(sl[1]);
  finally
    sl.Free;
  end;
end;
procedure TTMSFNCGraphicsSVGImport.ParseStyleValues(const AElement: TTMSFNCGraphicsSVGElement; const Values: string);
var
  C: Integer;
  k, kk: string;
  v: string;
  h: string;
  a, xhlr: Integer;
  fn: string;
  fs: TFontStyles;
const
  xhr = 'xlink:href';
begin
  h := Trim(Values);

  while h <> '' do
  begin
    C := Pos(';', h);
    if C = 0 then
      C := Length(h) + 1;
    k := Copy(h, 1, C
     - 1);
    h := Trim(Copy(h, C + 1, MaxInt));
    C := Pos(':', k);
    xhlr := Pos(xhr, k);
    if xhlr > 0 then
    begin
      kk := Copy(k, xhlr + Length(xhr), Length(k));
      C := Pos(':', kk) + Length(xhr);
    end;
      
    if C <> 0 then
    begin
      v := Trim(Copy(k, C + 1, MaxInt));
      k := Trim(Copy(k, 1, C - 1));

      if k = 'display' then
        AElement.FDisplayValue := LowerCase(v)
      else if k = 'fill' then
      begin
        AElement.FFillColorValue := v;
        if AElement.HasFillColor then
        begin
          if Pos('#', v) = 1 then
            AElement.FFillColor := TTMSFNCGraphics.HTMLToColor(v)
          else if Pos('url(#', v) = 1 then
            AElement.FURLRefID := Copy(v, Pos('#', v) + 1, Pos(')', v) - Pos('#', v) - 1)
          else if Pos('url(''#', v) = 1 then
            AElement.FURLRefID := Copy(v, Pos('#', v) + 1, Pos(''')', v) - Pos('#', v) - 1)
          else if Pos('rgb', v) = 1 then
          begin
            a := 255;
            AElement.FFillColor := ParseRGB(v, a);
            if a = 0 then
              AElement.FFillColorValue := 'none';
          end
          else
            AElement.FFillColor := TTMSFNCGraphics.TextToColor(v);
        end
        else
          AElement.FFillColor := gcNull;
      end
      else if k = 'stroke' then
      begin
        AElement.FStrokeColorValue := v;
        if AElement.HasStrokeColor then
        begin
          if Pos('#', v) = 1 then
            AElement.FStrokeColor := TTMSFNCGraphics.HTMLToColor(v)
          else if Pos('url(#', v) = 1 then
            AElement.FURLRefID := Copy(v, Pos('#', v) + 1, Pos(')', v) - Pos('#', v) - 1)
          else if Pos('url(''#', v) = 1 then
            AElement.FURLRefID := Copy(v, Pos('''#', v) + 1, Pos(''')', v) - Pos('''#', v) - 1)
          else if Pos('rgb', v) = 1 then
          begin
            a := 255;
            AElement.FStrokeColor := ParseRGB(v, a);
            if a = 0 then
              AElement.FStrokeColorValue := 'none'
          end
          else
            AElement.FStrokeColor := TTMSFNCGraphics.TextToColor(v);
        end
        else
          AElement.FStrokeColor := gcNull;
      end
      else if k = 'stroke-width' then
      begin
        AElement.FStrokeWidthValue := v;
        if AElement.HasStrokeWidth then
          TryStrToFloat(v, AElement.FStrokeWidth);
      end
      else if k = 'font-family' then
      begin
        fs := [];
        fn := '';
        ParseFont(v, fn, fs);
        AElement.FFontFamily := fn;
        if (AElement.FFontStyle = []) then
          AElement.FFontStyle := fs;
      end
      else if k = 'font-style' then
      begin
        if (v <> '') and (AElement.FFontStyle = []) then
        begin
          if v = 'italic' then
            AElement.FFontStyle := AElement.FFontStyle + [TFontStyle.fsItalic];
        end;
      end
      else if k = 'font-size' then
      begin
        AElement.FFontSizeValue := v;
        if AElement.HasFontSize then
          AElement.FFontSize := GetRealValue(v);
      end
      else if k = 'preserveAspectRatio' then
      begin
        AElement.FPreserveAspectRatioValue := v;
        if AElement.HasPreserveAspectRatio then
          AElement.FPreserveAspectRatio := GetPreserveAspectRatio(v);
      end
      else if k = 'offset' then
      begin
        AElement.FOffsetValue := v;
        if AElement.HasOffset then
        begin
          AElement.FOffset := GetRealValue(v);
          if GetUnit(v) = '%' then
            AElement.FOffset := AElement.FOffset / 100;
        end;
      end
      else if k = 'stop-opacity' then
      begin
        AElement.FStopOpacityValue := v;
        if AElement.HasStopOpacity then
          AElement.FStopOpacity := GetRealValue(v);
      end
      else if k = 'stop-color' then
      begin
        AElement.FStopColorValue := v;
        if AElement.HasStopColor then
        begin
          if Pos('#', v) = 1 then
            AElement.FStopColor := TTMSFNCGraphics.HTMLToColor(v)
          else if Pos('rgb', v) = 1 then
          begin
            a := 255;
            AElement.FStopColor := ParseRGB(v, a);
            if a = 0 then
              AElement.FStopColorValue := 'none'
          end
          else
            AElement.FStopColor := TTMSFNCGraphics.TextToColor(v);
        end
        else
          AElement.FStopColor := gcNull;
      end
      else if k = 'opacity' then
      begin
        AElement.FOpacityValue := v;
        if AElement.HasOpacity then
          TryStrToFloat(v, AElement.FOpacity);
      end
      else if (k = xhr) or (k = 'href') then
      begin
        AElement.FHRefID := Copy(v, Pos('#', v) + 1, Length(v) - Pos('#', v))
      end
      else if k = 'class' then
      begin
        AElement.FClassRefID := v;
        if not Assigned(AElement.Parent) or (Assigned(AElement.Parent) and not (AElement.Parent.&Type in [etPattern, etLinearGradient, etRadialGradient])) then
        begin
          if FElementClassList.IndexOf(AElement) = -1 then
            FElementClassList.Add(AElement);
        end;
      end
      else if k = 'id' then
        AElement.FID := v
      else if k = 'clip-path' then
        AElement.FClipPathRefID := Copy(v, Pos('#', v) + 1, Pos(')', v) - Pos('#', v) - 1)
    end;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.ReadNode(const AElements: TTMSFNCGraphicsSVGElementList; const ANode: TTMSFNCGraphicsSVGXMLNode; const AUpdateViewRect: Boolean = True; const AParent: TTMSFNCGraphicsSVGElement = nil);
var
  I, K, LL: Integer;
  pth: TTMSFNCGraphicsPath;
  n: string;
  e, es: TTMSFNCGraphicsSVGElement;
  t: TTMSFNCGraphicsSVGElementType;
  nc: TTMSFNCGraphicsSVGXMLNode;
  cx, cy, x, y, w, h, rx, ry, px, arx, ary, axr,
  py, c1x, c2x, c1y, c2y: Single;
  al, aswp: Integer;
  s: string;
  ms: TMemoryStream;
  b: TBytes;
  sl: TStrings;
  sc: Integer;
  c, pc: Char;
  CC: Integer;
  SS: string;
  M, gm: TTMSFNCGraphicsMatrix;
  stx, sty, sc2x, sc2y: Single;
  pts: TPointF;
  ptsa: TTMSFNCGraphicsPathPolygon;
begin
  for I := 0 to ANode.ChildNodes.count - 1 do
  begin
    nc := ANode.ChildNodes[I];
    n := nc.NodeName;
    t := NameToElementType(n);

    if (t <> etUnknown) and not ((t = etTextSpanChild) and (Trim(NodeToString(nc)) = '')) then
    begin
      e := nil;
      if not (t in [etDefs, etStyle, etSwitch, etUse]) then
      begin
        e := TTMSFNCGraphicsSVGElement.Create;
        e.FParent := AParent;
        AElements.Add(e);

        x := 0;
        y := 0;
        w := 0;
        h := 0;
        rx := 0;
        ry := 0;

        M := MatrixEmpty;
        ParseTransform(nc, 'transform', M);
        gM := MatrixEmpty;
        ParseTransform(nc, 'gradientTransform', gM);

        if (nc.NodeName = 'circle') or (nc.NodeName = 'ellipse') then
        begin
          ParseLength(nc, 'cx', x);
          ParseLength(nc, 'cy', y);

          if nc.NodeName = 'circle' then
          begin
            ParseLength(nc, 'r', rx);
            ry := rx;
          end
          else
          begin
            ParseLength(nc, 'rx', rx);
            ParseLength(nc, 'ry', ry);
          end;
        end
        else if (nc.NodeName = 'line') or (nc.NodeName = 'linearGradient') then
        begin
          ParseLength(nc, 'x1', x);
          ParseLength(nc, 'y1', y);
          ParseLength(nc, 'x2', w);
          ParseLength(nc, 'y2', h);
        end
        else if (nc.NodeName = 'radialGradient') then
        begin
          e.FHasCX := Assigned(FindNode(nc, 'cx'));
          e.FHasCY := Assigned(FindNode(nc, 'cy'));

          ParseLength(nc, 'cx', cx);
          ParseLength(nc, 'cy', cy);
        end
        else
        begin
          ParseLength(nc, 'x', x);
          ParseLength(nc, 'y', y);
          ParseLength(nc, 'width', w);
          ParseLength(nc, 'height', h);
          ParseLength(nc, 'rx', rx);
          ParseLength(nc, 'ry', ry);
        end;

        e.FGradientMatrix := gm;
        e.FMatrix := m;
        e.FType := t;
        e.FXV := x;
        e.FYV := y;
        e.FWidth := w;
        e.FHeight := h;
        e.FRx := rx;
        e.FRy := ry;
        e.FCX := cx;
        e.FCY := cy;

        S := '';
        ParseString(nc, 'style', S);

        if S <> '' then
          S := S + ';';

        SS := '';
        {$IFNDEF LCLLIB}
        for CC := 0 to nc.AttributeNodes.Count - 1 do
        begin
          SS := nc.AttributeNodes[CC].nodeName;
          S := S + SS + ':' + NodeToString(nc.AttributeNodes[CC]);
          if CC < nc.AttributeNodes.Count - 1 then
            S := S + ';';
        end;
        {$ELSE}
        for CC := 0 to nc.Attributes.Length - 1 do
        begin
          SS := nc.Attributes.Item[CC].nodeName;
          S := S + SS + ':' + NodeToString(nc.Attributes.Item[CC]);
          if CC < nc.Attributes.Length - 1 then
            S := S + ';';
        end;
        {$ENDIF}

        ParseStyleValues(e, s);
      end;

      case t of
        etContainer, etPattern, etLinearGradient, etRadialGradient, etClipPath:
        begin
          ReadNode(e.Elements, nc, (t = etContainer), e);
          if (t in [etPattern, etLinearGradient, etRadialGradient]) then
            FElementPatternList.Add(e);

          if t = etClipPath then
            FElementClipPathList.Add(e);
        end;
        etDefs: ReadNode(FDefsList, nc, False);
        etStyle:
        begin
          s := Trim(NodeToString(nc{$IFDEF LCLLIB}.FirstChild{$ENDIF}));
          while Length(s) > 0 do
          begin
            es := TTMSFNCGraphicsSVGElement.Create;
            es.FType := etStyle;
            es.FClassRefID := Copy(s, Pos('.', s) + 1, Pos('{', s) - 1 - Pos('.', s));
            ParseStyleValues(es, Copy(s, Pos('{', s) + 1, Pos('}', s) - Pos('{', s) - 1));
            if es.FClassRefID <> '' then
              FDefsList.Add(es)
            else
              es.Free;

            Delete(s, Pos('.', s), Pos('}', s));
            s := Trim(s);
            if (Pos('.', s) = 0) or (Pos('}', s) = 0) or (Pos('{', s) = 0) then
              Break;
          end;
        end;
        etRect:
        begin
          if AUpdateViewRect then
            UpdateViewRect(e, RectF(x, y, x + w, y + h));
        end;
        etEllipse, etCircle:
        begin
          if AUpdateViewRect then
            UpdateViewRect(e, RectF(x - rx, Y - ry, x + rx, y + ry));
        end;
        etLine:
        begin
          if AUpdateViewRect then
            UpdateViewRect(e, RectF(x, y, x + w, y + h));
        end;
        etPolyline, etPolygon:
        begin
          S := '';
          ParseString(nc, 'points', s);
          S := StringReplace(S, ',', ' ', [rfReplaceAll]);
          S := StringReplace(S, '-', ' -', [rfReplaceAll]);

          sl := TStringList.Create;
          try
            sl.Delimiter := ' ';
            sl.DelimitedText := s;

            for SC := SL.Count - 1 downto 0 do
              if SL[SC] = '' then
                SL.Delete(SC);

            if not (SL.Count mod 2 = 1) then
            begin
              e.Paths.Add(TTMSFNCGraphicsPath.Create);
              if e.HasActivePath then
              begin
                SetLength(ptsa, SL.Count div 2);
                for SC := 0 to Length(ptsa) - 1 do
                begin
                  pts := PointF(0, 0);
                  TryStrToFloat(SL[SC * 2], pts.X);
                  TryStrToFloat(SL[SC * 2 + 1], pts.Y);
                  ptsa[SC] := pts;
                end;

                for SC := 1 to Length(ptsa) - 1 do
                  e.ActivePath.AddLine(ptsa[SC - 1], ptsa[SC]);

                if t = etPolygon then
                  e.ActivePath.ClosePath;

                if not IsMatrixEmpty(M) and ((Assigned(e.Parent) and IsMatrixEmpty(e.Parent.Matrix)) or not Assigned(e.Parent)) then
                  e.ActivePath.ApplyMatrix(m);
              end;
            end;
          finally
            SL.Free;
          end;

          if e.HasActivePath and AUpdateViewRect then
            UpdateViewRect(e, e.ActivePath.GetBounds);
        end;
        etPath:
        begin
          S := '';
          ParseString(nc, 'd', s);
          s := StringReplace(s, ',', ' ', [rfReplaceAll]);
          sl := SplitPath(s);
          try
            K := 0;
            stx := 0;
            sty := 0;
            sc2x := 0;
            sc2y := 0;
            c := #0;
            while K < sl.Count do
            begin
              pc := c;
              c := sl[K][{$IFDEF ZEROSTRINGINDEX}0{$ELSE}1{$ENDIF}];
              case c of
                'M', 'm':
                begin
                  e.Paths.Add(TTMSFNCGraphicsPath.Create);

                  TryStrToFloat(SL[K + 1], px);
                  TryStrToFloat(SL[K + 2], py);

                  if c = 'm' then
                  begin
                    px := stx + px;
                    py := sty + py;
                  end;

                  stx := px;
                  sty := py;

                  if e.HasActivePath then
                    e.ActivePath.MoveTo(PointF(pX, py));

                  Inc(K, 2);
                end;
                'L', 'l', 'H', 'h', 'V', 'v':
                begin
                  if (C = 'L') or (c = 'l') then
                  begin
                    TryStrToFloat(SL[K + 1], px);
                    TryStrToFloat(SL[K + 2], py);
                  end
                  else if (C = 'V') or (C = 'v') then
                  begin
                    TryStrToFloat(SL[K + 1], py);
                    px := stx;
                  end
                  else if (C = 'H') or (C = 'h') then
                  begin
                    TryStrToFloat(SL[K + 1], px);
                    py := sty;
                  end;

                  if (c = 'h') or (c = 'l') then
                    px := stx + px;

                  if (c = 'v') or (c = 'l') then
                    py := sty + py;

                  stx := px;
                  sty := py;

                  if e.HasActivePath then
                    e.ActivePath.LineTo(PointF(pX, py));

                  if (C = 'L') or (c = 'l') then
                    Inc(K, 2)
                  else if (C = 'V') or (C = 'v') then
                    Inc(K, 1)
                  else if (C = 'H') or (C = 'h') then
                    Inc(K, 1);
                end;
                'C', 'c', 'S', 's', 'Q', 'q', 'T', 't':
                begin
                  if (c = 'C') or (c = 'c') then
                  begin
                    TryStrToFloat(SL[K + 1], c1x);
                    TryStrToFloat(SL[K + 2], c1y);
                    TryStrToFloat(SL[K + 3], c2x);
                    TryStrToFloat(SL[K + 4], c2y);
                    TryStrToFloat(SL[K + 5], px);
                    TryStrToFloat(SL[K + 6], py);

                    if c = 'c' then
                    begin
                      c1x := stx + c1x;
                      c1y := sty + c1y;
                      c2x := stx + c2x;
                      c2y := sty + c2y;
                      px := stx + px;
                      py := sty + py;
                    end;

                    stx := px;
                    sty := py;

                    sc2x := c2x;
                    sc2y := c2y;

                    if e.HasActivePath then
                      e.ActivePath.CurveTo(PointF(c1x, c1y), PointF(c2x, c2y), PointF(px, py));

                    Inc(K, 6);
                  end;

                  if (C = 'S') or (C = 's') then
                  begin
                    c1x := stx;
                    c1y := sty;
                    TryStrToFloat(SL[K + 1], c2x);
                    TryStrToFloat(SL[K + 2], c2y);
                    TryStrToFloat(SL[K + 3], px);
                    TryStrToFloat(SL[K + 4], py);

                    if TTMSFNCUtils.CharInSet(pc, TTMSFNCUtils.CreateCharSet('CcSsQqTt')) then
                    begin
                      c1x := stx + (stx - sc2x);
                      c1y := sty + (sty - sc2y);
                    end;

                    if C = 's' then
                    begin
                      c2x := stx + c2x;
                      c2y := sty + c2y;
                      px := stx + px;
                      py := sty + py;
                    end;

                    stx := px;
                    sty := py;

                    sc2x := c2x;
                    sc2y := c2y;

                    if e.HasActivePath then
                      e.ActivePath.CurveTo(PointF(c1x, c1y), PointF(c2x, c2y), PointF(px, py));

                    Inc(K, 4);
                  end;

                  if (C = 'Q') or (C = 'q') then
                  begin
                    TryStrToFloat(SL[K + 1], c1x);
                    TryStrToFloat(SL[K + 2], c1y);
                    TryStrToFloat(SL[K + 3], px);
                    TryStrToFloat(SL[K + 4], py);
                    c2x := c1x;
                    c2y := c1y;

                    if C = 'q' then
                    begin
                      c1x := stx + c1x;
                      c1y := sty + c1y;
                      c2x := stx + c2x;
                      c2y := sty + c2y;
                      px := stx + px;
                      py := sty + py;
                    end;

                    stx := px;
                    sty := py;

                    sc2x := c2x;
                    sc2y := c2y;

                    if e.HasActivePath then
                      e.ActivePath.CurveTo(PointF(c1x, c1y), PointF(c2x, c2y), PointF(px, py));

                    Inc(K, 4);
                  end;

                  if (C = 'T') or (C = 't') then
                  begin
                    c1x := stx;
                    c1y := sty;
                    TryStrToFloat(SL[K + 1], px);
                    TryStrToFloat(SL[K + 2], py);

                    if TTMSFNCUtils.CharInSet(pc, TTMSFNCUtils.CreateCharSet('CcSsQqTt')) then
                    begin
                      c1x := stx + (stx - sc2x);
                      c1y := sty + (sty - sc2y);
                    end;

                    c2x := c1x;
                    c2y := c1y;

                    if C = 't' then
                    begin
                      px := stx + px;
                      py := sty + py;
                    end;

                    stx := px;
                    sty := py;

                    sc2x := c2x;
                    sc2y := c2y;

                    if e.HasActivePath then
                      e.ActivePath.CurveTo(PointF(c1x, c1y), PointF(c2x, c2y), PointF(px, py));

                    Inc(K, 2);
                  end;
                end;
                'A', 'a':
                begin
                  if (c = 'A') or (c = 'a') then
                  begin
                    TryStrToFloat(SL[K + 1], arx);
                    TryStrToFloat(SL[K + 2], ary);
                    TryStrToFloat(SL[K + 3], axr);
                    TryStrToInt(SL[K + 4], al);
                    TryStrToInt(SL[K + 5], aswp);
                    TryStrToFloat(SL[K + 6], px);
                    TryStrToFloat(SL[K + 7], py);

                    arx := Abs(arx);
                    ary := Abs(ary);

                    if c = 'a' then
                    begin
                      px := stx + px;
                      py := sty + py;
                    end;

                    if e.HasActivePath then
                      AddArc(e, e.ActivePath, stx, px, sty, py, arx, ary, axr, al, aswp);

                    stx := px;
                    sty := py;

                    Inc(K, 7);
                  end;
                end;
                'Z', 'z':
                begin
                  if e.HasActivePath then
                  begin
                    e.ActivePath.ClosePath;
                    stx := e.ActivePath.LastPoint.X;
                    sty := e.ActivePath.LastPoint.Y;
                  end;
                end;
              end;

              Inc(K);
            end;

            if e.HasPaths then
            begin
              for LL := 0 to e.Paths.Count - 1 do
              begin
                pth := TTMSFNCGraphicsPath.Create;
                try
                  pth.AddPath(e.Paths[LL]);
                  if not IsMatrixEmpty(M) then
                    pth.ApplyMatrix(m);

                  if AUpdateViewRect then
                    UpdateViewRect(e, pth.GetBounds);
                finally
                  pth.Free;
                end;
              end;
            end;

          finally
            sl.Free;
          end;
        end;
        etImage:
        begin
          if ParseImage(nc, 'xlink:href', S) or ParseImage(nc, 'href', S) then
          begin
            ms := TMemoryStream.Create;
            try
              b := TTMSFNCUtils.Decode64ToBytes(S);
              ms.Write(b, Length(b));
              ms.Position := 0;
              e.Bitmap.LoadFromStream(ms);
            finally
              ms.Free;
            end;
          end
          else
            e.Bitmap.LoadFromURL(S);

          if AUpdateViewRect then
            UpdateViewRect(e, RectF(x, y, x + w, y + h));
        end;
        etText, etTextSpan, etTextSpanChild, etTextPath:
        begin
          if nc.NodeType = {$IFDEF LCLLIB}TEXT_NODE{$ELSE}TNodeType.ntText{$ENDIF} then
            e.FText := NodeToString(nc)
          else if (nc.NodeType = {$IFDEF LCLLIB}ELEMENT_NODE{$ELSE}TNodeType.ntElement{$ENDIF}) and (nc.NodeName = 'text')
          and (nc.ChildNodes.Count = 1) and (nc.ChildNodes[0].NodeType = {$IFDEF LCLLIB}TEXT_NODE{$ELSE}TNodeType.ntText{$ENDIF}) then
            e.FText := NodeToString(nc)
          else
            ReadNode(e.Elements, nc, True, e);
        end;
      end;
    end;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LinkClasses(const AElementClasses: TTMSFNCGraphicsSVGElementClassList;
  AElementDefs: TTMSFNCGraphicsSVGElementList);
var
  I, J, K: Integer;
  el, eld, elp: TTMSFNCGraphicsSVGElement;
  l: TTMSFNCGraphicsSVGElementClassList;
begin
  if AElementDefs.Count = 0 then
    Exit;

  for I := 0 to AElementClasses.Count - 1 do
  begin
    el := AElementClasses[I];

    for J := 0 to AElementDefs.Count - 1 do
    begin
      eld := AElementDefs[J];
      if (eld.ClassRefID = el.ClassRefID) and (eld.ClassRefID <> '') and (el.ClassRefID <> '') then
      begin
        if eld.ClipPathRefID <> '' then
        begin
          for K := 0 to AElementDefs.Count - 1 do
          begin
            elp := AElementDefs[K];
            if eld.ClipPathRefID = elp.ID then
            begin
              if elp.HasElements then
              begin
                el.FClipPathRef := elp.Elements[elp.Elements.Count - 1];
                el.FClassRef := elp.Elements[elp.Elements.Count - 1].FClassRef;
              end;
            end;
          end;
        end
        else if (eld.URLRefID <> '') then
        begin
          for K := 0 to AElementDefs.Count - 1 do
          begin
            elp := AElementDefs[K];
            if eld.URLRefID = elp.ID then
            begin
              if elp.&Type in [etLinearGradient, etRadialGradient] then
                el.FPatternRef := elp
              else if elp.HasElements then
                el.FClassRef := elp.Elements[elp.Elements.Count - 1].FClassRef;
            end;
          end;
        end
        else
          el.FClassRef := eld;

        Break;
      end
      else if (((eld.URLRefID = el.ID) and (eld.URLRefID <> '')) or ((eld.ClipPathRefID = el.ID) and (eld.ClipPathRefID <> ''))) and (el.ID <> '') then
      begin
        if el.HasElements then
        begin
          l := TTMSFNCGraphicsSVGElementClassList.Create;
          try
            for K := 0 to el.Elements.Count - 1 do
              l.Add(el.Elements[K]);

            LinkClasses(l, AElementDefs);
          finally
            l.Free;
          end;
        end;
        Break;
      end;
    end;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LinkClipPaths(
  const AElementClasses: TTMSFNCGraphicsSVGElementClassList;
  AElementDefs: TTMSFNCGraphicsSVGElementList);
var
  I, J: Integer;
  el, eld: TTMSFNCGraphicsSVGElement;
begin
  if AElementDefs.Count = 0 then
    Exit;

  for I := 0 to AElementClasses.Count - 1 do
  begin
    el := AElementClasses[I];

    for J := 0 to AElementDefs.Count - 1 do
    begin
      eld := AElementDefs[J];
      if (eld.ClipPathRefID = el.ID) and (eld.ClipPathRefID <> '') and (el.ID <> '') then
      begin
        if el.HasElements then
          eld.FClipPathRef := el.Elements[el.Elements.Count - 1];
      end;

      LinkClipPaths(FElementClipPathList, eld.Elements);
    end;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LinkHRef(
  const AElementClasses: TTMSFNCGraphicsSVGElementClassList;
  AElementDefs: TTMSFNCGraphicsSVGElementList);
var
  I, J: Integer;
  el, eld: TTMSFNCGraphicsSVGElement;
begin
  if AElementDefs.Count = 0 then
    Exit;

  for I := 0 to AElementClasses.Count - 1 do
  begin
    el := AElementClasses[I];

    for J := 0 to AElementDefs.Count - 1 do
    begin
      eld := AElementDefs[J];
      if (eld.HRefID = el.ID) and (eld.HRefID <> '') and (el.ID <> '') then
        eld.FHRefRef := el;
    end;
  end
end;

procedure TTMSFNCGraphicsSVGImport.LinkPatternHRef(const AElementClasses,
  AElementDefs: TTMSFNCGraphicsSVGElementClassList);
var
  I, J: Integer;
  el, eld: TTMSFNCGraphicsSVGElement;
begin
  if AElementDefs.Count = 0 then
    Exit;

  for I := 0 to AElementClasses.Count - 1 do
  begin
    el := AElementClasses[I];

    for J := 0 to AElementDefs.Count - 1 do
    begin
      eld := AElementDefs[J];
      if (eld.HRefID = el.ID) and (eld.HRefID <> '') and (el.ID <> '') then
        eld.FHRefRef := el;
    end;
  end
end;

procedure TTMSFNCGraphicsSVGImport.LinkPatterns(
  const AElementClasses: TTMSFNCGraphicsSVGElementClassList;
  AElementDefs: TTMSFNCGraphicsSVGElementList);
var
  I, J: Integer;
  el, eld: TTMSFNCGraphicsSVGElement;
begin
  if AElementDefs.Count = 0 then
    Exit;

  for J := 0 to AElementDefs.Count - 1 do
  begin
    eld := AElementDefs[J];
    for I := 0 to AElementClasses.Count - 1 do
    begin
      el := AElementClasses[I];
      if (eld.URLRefID = el.ID) and (eld.URLRefID <> '') and (el.ID <> '') then
        eld.FPatternRef := el;
    end;

    LinkPatterns(FElementPatternList, eld.Elements);
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LoadFromFile(const AFile: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(AFile);
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LoadFromStream(const AStream: TStream);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    AStream.Position := 0;
    SL.LoadFromStream(AStream{$IFNDEF LCLWEBLIB}, TEncoding.UTF8{$ENDIF});
    LoadFromText(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TTMSFNCGraphicsSVGImport.LoadFromText(const AText: string);
var
  DocNode: TTMSFNCGraphicsSVGXMLNode;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
  {$IFDEF LCLLIB}
  ss: TStringStream;
  {$ENDIF}
  v, vw, vh: string;
  w, h: Single;
  r: TRectF;

  function ParseViewRect(const S: string): TRectF;
  var
    SL: TStrings;
    SC: Integer;
  begin
    sl := TStringList.Create;
    sl.Delimiter := ' ';
    sl.DelimitedText := Trim(s);

    for SC := SL.Count - 1 downto 0 do
      if SL[SC] = '' then
        SL.Delete(SC);

    try
      if SL.Count = 4 then
      begin
        TryStrToFloat(SL[0], Result.Left);
        TryStrToFloat(SL[1], Result.Top);
        TryStrToFloat(SL[2], Result.Right);
        TryStrToFloat(SL[3], Result.Bottom);
      end;
    finally
      SL.Free;
    end;
  end;
begin
  Clear;

  {$IFDEF LCLLIB}
  ss := TStringStream.Create(AText);
  try
    ReadXMLFile(FXML, ss);
  finally
    ss.Free;
  end;
  {$ELSE}
  FXML := TXmlDocument.Create(nil);
  FXML.LoadFromXML(AText);
  {$ENDIF}

  if Assigned(FXML) then
  begin
    DocNode := FXML.documentElement;
    if Assigned(DocNode) and (DocNode.nodeName = 'svg') then
    begin
      t := FormatSettings.ThousandSeparator;
      d := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := '.';
      FormatSettings.ThousandSeparator := ',';

      r := RectF(MaxInt, MaxInt, -MaxInt, -MaxInt);

      v := '';
      ParseString(DocNode, 'viewBox', v);
      if v <> '' then
        r := ParseViewRect(v)
      else
      begin
        ParseString(DocNode, 'width', vw);
        w := GetRealValue(vw);
        ParseString(DocNode, 'height', vh);
        h := GetRealValue(vh);

        if (vw <> '') and (vh <> '') then
          r := RectF(0, 0, w, h);
      end;

      ReadNode(FElementList, DocNode);
      LinkClasses(FElementPatternList, FDefsList);
      LinkClasses(FElementClipPathList, FDefsList);
      LinkClasses(FElementClassList, FDefsList);
      LinkPatterns(FElementPatternList, FElementList);
      LinkPatternHRef(FElementPatternList, FElementPatternList);
//      LinkClipPaths(FElementClipPathList, FElementList);

      if not (CompareValue(r.Left, MaxInt) = EqualsValue) and not (CompareValue(r.Top, MaxInt) = EqualsValue)
        and not (CompareValue(r.Right, -MaxInt) = EqualsValue) and not (CompareValue(r.Bottom, -MaxInt) = EqualsValue) then
          ViewRect := r
      else if (CompareValue(ViewRect.Left, MaxInt) = EqualsValue) and (CompareValue(ViewRect.Top, MaxInt) = EqualsValue)
        and (CompareValue(ViewRect.Right, -MaxInt) = EqualsValue) and (CompareValue(ViewRect.Bottom, -MaxInt) = EqualsValue) then
          ViewRect := RectF(0, 0, 0, 0);

      FormatSettings.DecimalSeparator := d;
      FormatSettings.ThousandSeparator := t;
    end;
  end;
end;

function TTMSFNCGraphicsSVGImport.ParseImage(const ANode: TTMSFNCGraphicsSVGXMLNode;
  const AString: string; var AValue: string): Boolean;
var
  sc: Integer;
  v: string;
begin
  ParseString(ANode, AString, AValue);

  v := AValue;
  Result := False;
  if Pos('data:', v) > 0 then
  begin
    v := Copy(v, 6, MaxInt);
    sc := Pos(';', v);
    if sc > 0 then
    begin
      if Copy(v, sc, 8) = ';base64,' then
      begin
        v := Copy(v, sc + 8, MaxInt);
        Result := True;
      end;
    end;
  end;

  AValue := v;
end;

procedure TTMSFNCGraphicsSVGImport.ParseLength(const ANode: TTMSFNCGraphicsSVGXMLNode; const AString: string; var AValue: Single);
var
  a: TTMSFNCGraphicsSVGXMLNode;
begin
  a := FindNode(ANode, AString);
  if Assigned(a) then
    AValue := GetRealValue(NodeToString(a));
end;

function TTMSFNCGraphicsSVGImport.ParseRGB(
  const S: string; var AAlpha: Integer): TTMSFNCGraphicsColor;
var
  RV, GV, BV, AV: string;
  RGB, RGBA: string;
  R, B, G: Integer;

  function IsDecimal(const S: string): Boolean;
  var
    C: Integer;
  begin
    Result := False;
    for C := 1 to Length(S) do
      if not ((S[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] >= '0') and (S[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] <= '9')) then
        Exit;
    Result := True;
  end;

  function IsHex(const S: string): Boolean;
  var
    C: Integer;
    h: string;
  begin
    Result := False;
    if S[1] = '#' then
      h := Copy(S, 2, Length(S))
    else
      h := S;
    for C := 1 to Length(h) do
      if not (((h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] >= '0') and (h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] <= '9')) or
              ((h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] >= 'A') and (h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] <= 'F')) or
              ((h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] >= 'a') and (h[C{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] <= 'f'))) then
      Exit;

    Result := True;
  end;

  function DecodeToInt(const S: string): Integer;
  var
    C: Integer;
    p: Boolean;
    H: string;
  begin
    Result := -1;
    h := '0' + S;
    p := False;
    if h[Length(h){$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] = '%' then
    begin
      h := Copy(h, 1, Length(h) - 1);
      p := True;
    end;

    C := -1;
    if IsDecimal(h) then
      C := StrToInt(h)
    else
      if IsHex(h) then
        C := StrToInt('$' + h);
    if C = -1 then
      Exit;
    if C > 255 then
      C := 255;
    if p then
    begin
      if C > 100 then
        C := 100;
      C := Round(C * 2.55);
    end;
    Result := C;
  end;

begin
  Result := gcNull;
  AAlpha := 255;

  if not (((Copy(S, 1, 4) = 'rgb(') or (Copy(S, 1, 5) = 'rgba(')) and (S[Length(S){$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}] = ')')) then
    Exit;

  if Pos('rgba(', s) = 1 then
  begin
    RGBA := Copy(S, 6, Length(S) - 6);
    RGBA := Trim(RGBA);

    RV := Copy(RGBA, 1, Pos(',', RGBA) - 1);
    RGBA := Copy(RGBA, Pos(',', RGBA) + 1, Length(RGBA));
    RGBA := Trim(RGBA);

    GV := Copy(RGBA, 1, Pos(',', RGBA) - 1);
    RGBA := Copy(RGBA, Pos(',', RGBA) + 1, Length(RGBA));
    RGBA := Trim(RGBA);

    BV := Copy(RGBA, 1, Pos(',', RGBA) - 1);
    RGBA := Copy(RGBA, Pos(',', RGBA) + 1, Length(RGBA));
    RGBA := Trim(RGBA);

    AV := RGBA;
  end
  else
  begin
    RGB := Copy(S, 5, Length(S) - 5);
    RGB := Trim(RGB);

    RV := Copy(RGB, 1, Pos(',', RGB) - 1);
    RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
    RGB := Trim(RGB);

    GV := Copy(RGB, 1, Pos(',', RGB) - 1);
    RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
    RGB := Trim(RGB);

    BV := RGB;

    AV := '255';
  end;

  R := DecodeToInt(RV);
  G := DecodeToInt(GV);
  B := DecodeToInt(BV);

  AAlpha := DecodeToInt(AV);

  if (R = -1) or (G = -1) or (B = -1) then
    Exit;

  Result := MakeGraphicsColor(R, G, B{$IFDEF FMXLIB}, AAlpha{$ENDIF});
end;

{ TTMSFNCGraphicsSVGElement }

constructor TTMSFNCGraphicsSVGElement.Create;
begin
  FOffsetValue := 'default';
  FStopColorValue := 'default';
  FStopOpacityValue := 'default';
  FOpacityValue := 'default';
  FStrokeColorValue := 'default';
  FFillColorValue := 'default';
  FStrokeWidthValue := 'default';
  FPreserveAspectRatioValue := 'default';
  FPreserveAspectRatio := parOther;
  FFillColor := gcBlack;
  FStrokeColor := gcBlack;
  FType := etContainer;
  FStrokeWidth := 1;
  FOpacity := 1;
  FStopOpacity := 1;
  FStopColor := gcBlack;
  FOffset := 0;
  FXV := 0;
  FYV := 0;
  FCX := 0;
  FCY := 0;
  FFR := 0;
  FFY := 0;
  FFX := 0;
  FWidth := 0;
  FHeight := 0;
  FRx := 0;
  FRy := 0;
  FMatrix := MatrixEmpty;
  FGradientMatrix := MatrixEmpty;
end;

destructor TTMSFNCGraphicsSVGElement.Destroy;
begin
  FParent := nil;
  FClassRef := nil;
  FPatternRef := nil;

  if Assigned(FBitmap) then
    FBitmap.Free;

  if Assigned(FPaths) then
    FPaths.Free;

  if Assigned(FElements) then
    FElements.Free;

  if Assigned(FCombinedPath) then
    FCombinedPath.Free;
  inherited;
end;

function TTMSFNCGraphicsSVGElement.GetElements: TTMSFNCGraphicsSVGElementList;
begin
  if not Assigned(FElements) then
    FElements := TTMSFNCGraphicsSVGElementList.Create;

  Result := FElements;
end;

function TTMSFNCGraphicsSVGElement.GetActivePath: TTMSFNCGraphicsPath;
begin
  Result := nil;
  if Assigned(FPaths) and (FPaths.Count > 0) then
    Result := FPaths[FPaths.Count - 1];
end;

function TTMSFNCGraphicsSVGElement.GetBitmap: TTMSFNCBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TTMSFNCBitmap.Create;

  Result := FBitmap;
end;

function TTMSFNCGraphicsSVGElement.GetCombinedPath: TTMSFNCGraphicsPath;
var
  I: Integer;
  pth: TTMSFNCGraphicsPath;
begin
  if not Assigned(FCombinedPath) and HasPaths then
  begin
    FCombinedPath := TTMSFNCGraphicsPath.Create;
    for I := 0 to FPaths.Count - 1 do
    begin
      pth := FPaths[I];
      FCombinedPath.AddPath(pth);
    end;
  end;

  Result := FCombinedPath;
end;

function TTMSFNCGraphicsSVGElement.GetPaths: TTMSFNCGraphicsSVGElementPathList;
begin
  if not Assigned(FPaths) then
    FPaths := TTMSFNCGraphicsSVGElementPathList.Create;

  Result := FPaths;
end;

function TTMSFNCGraphicsSVGElement.HasElements: Boolean;
begin
  Result := Assigned(FElements);
end;

function TTMSFNCGraphicsSVGElement.HasFillColor: Boolean;
begin
  Result := (FillColorValue <> 'none') and (FillColorValue <> 'transparent') and (FillColorValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasFontSize: Boolean;
begin
  Result := (FontSizeValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasOffset: Boolean;
begin
  Result := (OffsetValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasOpacity: Boolean;
begin
  Result := (OpacityValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasActivePath: Boolean;
begin
  Result := Assigned(ActivePath);
end;

function TTMSFNCGraphicsSVGElement.HasBitmap: Boolean;
begin
  Result := Assigned(FBitmap);
end;

function TTMSFNCGraphicsSVGElement.HasPaths: Boolean;
begin
  Result := Assigned(FPaths);
end;

function TTMSFNCGraphicsSVGElement.HasPreserveAspectRatio: Boolean;
begin
  Result := (PreserveAspectRatioValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasStrokeWidth: Boolean;
begin
  Result := (StrokeWidthValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasStopColor: Boolean;
begin
  Result := (StopColorValue <> 'none') and (StopColorValue <> 'transparent') and (StopColorValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasStopOpacity: Boolean;
begin
  Result := (StopOpacityValue <> 'default');
end;

function TTMSFNCGraphicsSVGElement.HasStrokeColor: Boolean;
begin
  Result := (StrokeColorValue <> 'none') and (StrokeColorValue <> 'transparent') and (StrokeColorValue <> 'default');
end;

end.
