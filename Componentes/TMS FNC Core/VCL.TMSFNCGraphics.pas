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

unit VCL.TMSFNCGraphics;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

{$DEFINE SVGSUPPORT}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, VCL.TMSFNCGraphicsTypes, VCL.Graphics,
  {$IFDEF WEBLIB}
  JS,
  {$ENDIF}
  {$IFNDEF LIMITEDGRAPHICSMODE}
  VCL.TMSFNCBitmapContainer,
  {$ENDIF}
  Types, VCL.TMSFNCTypes
  {$IFDEF CMNLIB}
  ,VCL.ImgList
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TTMSFNCGraphics = class;

  TTMSFNCGraphicsContext = class
  private
    {$IFDEF AUTOREFCOUNT}
    [Weak]FGraphics: TTMSFNCGraphics;
    {$ELSE}
    FGraphics: TTMSFNCGraphics;
    {$ENDIF}
    function GetCanvas: TCanvas;
    function GetGraphics: TTMSFNCGraphics;
  protected
    function GetNativeCanvas: Pointer; virtual; abstract;
  public
    constructor Create(const AGraphics: TTMSFNCGraphics); virtual;
    function CreatePath: Pointer; virtual; abstract;
    function ConvertToPath(APath: TTMSFNCGraphicsPath; const Flatness: Single = 0.25): Pointer;
    function GetFillColor: TTMSFNCGraphicsColor; virtual; abstract;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; virtual; abstract;
    function SetTextAngle(ARect: TRectF; AAngle: Single): TRectF; virtual; abstract;
    function GetMatrix: TTMSFNCGraphicsMatrix; virtual; abstract;
    procedure Render; virtual; abstract;
    procedure PathOpen(APath: Pointer); virtual; abstract;
    procedure PathMoveTo(APath: Pointer; APoint: TPointF); virtual; abstract;
    procedure PathLineTo(APath: Pointer; APoint: TPointF); virtual; abstract;
    procedure SetScale(AScale: Single); virtual; abstract;
    procedure PathClose(APath: Pointer); virtual; abstract;
    procedure ResetClip; virtual; abstract;
    procedure ResetTransform; virtual; abstract;
    procedure ScaleTransform(AX, AY: Single); virtual; abstract;
    procedure RotateTransform(AAngle: Single); virtual; abstract;
    procedure TranslateTransform(AX, AY: Single); virtual; abstract;
    procedure SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality); virtual; abstract;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); virtual; abstract;
    procedure SetShowAcceleratorChar(AShowAcceleratorChar: Boolean); virtual; abstract;
    procedure SetSize(AWidth, AHeight: Single); virtual; abstract;
    procedure ResetTextAngle(AAngle: Single); virtual; abstract;
    procedure BeginScene; virtual; abstract;
    procedure EndScene; virtual; abstract;
    procedure BeginPrinting; virtual; abstract;
    procedure EndPrinting; virtual; abstract;
    procedure StartSpecialPen; virtual; abstract;
    procedure StopSpecialPen; virtual; abstract;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState); virtual; abstract;
    procedure SaveState(AState: TTMSFNCGraphicsSaveState); virtual; abstract;
    procedure SetFontSize(ASize: Integer); virtual; abstract;
    procedure SetFontColor(AColor: TTMSFNCGraphicsColor); virtual; abstract;
    procedure SetFontName(AName: string); virtual; abstract;
    procedure SetFont(AFont: TTMSFNCGraphicsFont); virtual; abstract;
    procedure SetFontStyles(AStyle: TFontStyles); virtual; abstract;
    procedure SetMatrix(AMatrix: TTMSFNCGraphicsMatrix); virtual; abstract;
    procedure SetFill(AFill: TTMSFNCGraphicsFill); virtual; abstract;
    procedure SetFillKind(AKind: TTMSFNCGraphicsFillKind); virtual; abstract;
    procedure SetFillColor(AColor: TTMSFNCGraphicsColor); virtual; abstract;
    procedure SetStroke(AStroke: TTMSFNCGraphicsStroke); virtual; abstract;
    procedure SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind); virtual; abstract;
    procedure SetStrokeColor(AColor: TTMSFNCGraphicsColor); virtual; abstract;
    procedure SetStrokeWidth(AWidth: Single); virtual; abstract;
    procedure DrawLine(AStroke: TTMSFNCGraphicsStroke; AFromPoint: TPointF; AToPoint: TPointF; AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); virtual; abstract;
    procedure DrawPolygon(AStroke: TTMSFNCGraphicsStroke; APolygon: TTMSFNCGraphicsPathPolygon); virtual; abstract;
    procedure FillPolygon(AFill: TTMSFNCGraphicsFill; APolygon: TTMSFNCGraphicsPathPolygon); virtual; abstract;
    procedure DrawPolyline(AStroke: TTMSFNCGraphicsStroke; APolyline: TTMSFNCGraphicsPathPolygon); virtual; abstract;
    procedure FillPolyline(AFill: TTMSFNCGraphicsFill; APolyline: TTMSFNCGraphicsPathPolygon); virtual; abstract;
    procedure FillArc(AFill: TTMSFNCGraphicsFill; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); virtual; abstract;
    procedure DrawArc(AStroke: TTMSFNCGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); virtual; abstract;
    procedure FillRect(AFill: TTMSFNCGraphicsFill; ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure DrawRect(AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ASides: TTMSFNCGraphicsSides; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure FillRoundRect(AFill: TTMSFNCGraphicsFill; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure DrawRoundRect(AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure FillEllipse(AFill: TTMSFNCGraphicsFill; ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure DrawEllipse(AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure DrawBitmap(ABitmap: TTMSFNCDrawBitmap; ASrcRect, ADstRect: TRectF; AOpacity: Single); virtual; abstract;
    procedure ClipRect(ARect: TRectF); virtual; abstract;
    procedure ClipPath(APath: TTMSFNCGraphicsPath); virtual; abstract;
    procedure DrawFocusPath(AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath; AColor: TTMSFNCGraphicsColor); virtual; abstract;
    procedure DrawFocusRectangle(AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); virtual; abstract;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TTMSFNCGraphicsTextAlign; ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single); virtual; abstract;
    procedure DrawPath(AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); virtual; abstract;
    procedure FillPath(AFill: TTMSFNCGraphicsFill; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); virtual; abstract;
    property Canvas: TCanvas read GetCanvas;
    property NativeCanvas: Pointer read GetNativeCanvas;
    property Graphics: TTMSFNCGraphics read GetGraphics;
  end;

  TTMSFNCGraphicsContextClass = class of TTMSFNCGraphicsContext;

  TTMSFNCGraphics = class
  private
    FActiveCanvas: TCanvas;
    FBlockUpdate: Integer;
    FNative: Boolean;
    FContextNative, FContextGeneral: TTMSFNCGraphicsContext;
    FBitmap: TBitmap;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FFont: TTMSFNCGraphicsFont;
    {$IFNDEF LIMITEDGRAPHICSMODE}
    FHighlightColor: TTMSFNCGraphicsColor;
    FOptimizedHTMLDrawing: Boolean;
    FHighlightTextColor: TTMSFNCGraphicsColor;
    FHighlightFontStyles: TFontStyles;
    FURLUnderline: Boolean;
    FURLColor: TTMSFNCGraphicsColor;
    FBitmapContainer: TTMSFNCBitmapContainer;
    {$IFDEF CMNLIB}
    FImageList: TCustomImageList;
    {$ENDIF}
    {$ENDIF}
    function GetCanvas: TCanvas;
    function GetContext: TTMSFNCGraphicsContext;
  protected
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure InitializeDefaultAppearance;
    function InternalDrawBitmapPartSync(ASourceLeft: Double; ASourceTop: Double; ASourceRight: Double; ASourceBottom: Double; ADestinationLeft: Double; ADestinationTop: Double;
    ADestinationRight: Double; ADestinationBottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
    ACenter: Boolean = True; ACropping: Boolean = False): {$IFDEF WEBLIB}TJSPromise{$ELSE}TTMSFNCBitmap{$ENDIF};
    function InternalCalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF; overload; virtual;
    function InternalDrawText(ARect: TRectF; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string;AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    procedure DrawTexture(ARect: TRectF; ATexture: TTMSFNCBitmap; ATextureMode: TTMSFNCGraphicsTextureMode);
    {$IFDEF SVGSUPPORT}
    function DrawSVG(ABitmap: TTMSFNCBitmapHelperClass; ARect: TRectF): Boolean;
    function HasSVG(ABitmap: TTMSFNCBitmapHelperClass): Boolean;
    {$ENDIF}
    class procedure ConvertBitmapToGrayScale(ABitmap: TTMSFNCBitmap);
  public class var
    DefaultSelectionFillColor: TTMSFNCGraphicsColor;
    DefaultTextFontColor: TTMSFNCGraphicsColor;
    DefaultPopupFillColor: TTMSFNCGraphicsColor;
    DefaultPopupStrokeColor: TTMSFNCGraphicsColor;
    DefaultButtonStrokeColorFocused: TTMSFNCGraphicsColor;
    DefaultButtonFillColorFocused: TTMSFNCGraphicsColor;
    DefaultButtonStrokeColorDisabled: TTMSFNCGraphicsColor;
    DefaultButtonFillColorDisabled: TTMSFNCGraphicsColor;
    DefaultButtonStrokeColor: TTMSFNCGraphicsColor;
    DefaultButtonFillColor: TTMSFNCGraphicsColor;
  public
    constructor Create(ACanvas: TCanvas; ANative: Boolean = False); virtual;
    constructor CreateNative(ACanvas: TCanvas); virtual;
    constructor CreateBitmapCanvas(AWidth: Integer = 1; AHeight: Integer = 1; ANative: Boolean = False; {%H-}AHighDPI: Boolean = True); virtual;
    constructor CreateNativeBitmapCanvas(AWidth: Integer = 1; AHeight: Integer = 1); virtual;
    destructor Destroy; override;
    function GetMatrix: TTMSFNCGraphicsMatrix; virtual;
    procedure StartSpecialPen;
    procedure StopSpecialPen;
    procedure BeginPrinting; virtual;
    procedure EndPrinting; virtual;
    procedure BeginScene; virtual;
    procedure EndScene; virtual;
    procedure SetMatrix(const AMatrix: TTMSFNCGraphicsMatrix); virtual;
    procedure Assign(Source: TTMSFNCGraphics); virtual;
    procedure SetFill(AFill: TTMSFNCGraphicsFill); virtual;
    procedure SetStroke(AStroke: TTMSFNCGraphicsStroke); virtual;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean = False); virtual;
    procedure SetFillKind(AKind: TTMSFNCGraphicsFillKind); virtual;
    procedure SetFillColor(AColor: TTMSFNCGraphicsColor); virtual;
    procedure SetFontColor(AColor: TTMSFNCGraphicsColor); virtual;
    procedure SetFontName(AName: string); virtual;
    procedure SetFont(AFont: TTMSFNCGraphicsFont); virtual;
    procedure SetFontSize(ASize: Integer); virtual;
    procedure SetFontStyles(AStyle: TFontStyles); virtual;
    procedure SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind); virtual;
    procedure SetStrokeColor(AColor: TTMSFNCGraphicsColor); virtual;
    procedure SetStrokeWidth(AWidth: Single); virtual;
    procedure DrawLine(AFromPoint: TPoint; AToPoint: TPoint; {%H-}AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); overload; virtual;
    procedure DrawLine(AFromPoint: TPointF; AToPoint: TPointF; {%H-}AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); overload; virtual;
    procedure DrawFocusRectangle(ALeft, ATop, ARight, ABottom: Integer; AColor: TTMSFNCGraphicsColor = gcBlack; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawFocusRectangle(ARect: TRect; AColor: TTMSFNCGraphicsColor = gcBlack; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawFocusRectangle(ALeft, ATop, ARight, ABottom: Double; AColor: TTMSFNCGraphicsColor = gcBlack; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawFocusRectangle(ARect: TRectF; AColor: TTMSFNCGraphicsColor = gcBlack; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawFocusPath(APath: TTMSFNCGraphicsPath; AColor: TTMSFNCGraphicsColor = gcBlack); virtual;
    procedure DrawPolygon(APolygon: TTMSFNCGraphicsPathPolygon); virtual;
    procedure DrawPolyline(APolyline: TTMSFNCGraphicsPathPolygon); virtual;
    procedure DrawPath(APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); virtual;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single); overload; virtual;
    procedure DrawArc(const Center, Radius: TPoint; StartAngle, SweepAngle: Integer); overload; virtual;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Double; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Double; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawEllipse(ALeft, ATop, ARight, ABottom: Double; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ARect: TRectF; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Integer; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ALeft, ATop, ARight, ABottom: Integer; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawEllipse(ALeft, ATop, ARight, ABottom: Integer; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ARect: TRect; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRectangle(ARect: TRect; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRoundRectangle(ALeft, ATop, ARight, ABottom: Double; ARounding: Single = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRoundRectangle(ARect: TRectF; ARounding: Single = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRoundRectangle(ALeft, ATop, ARight, ABottom: Integer; ARounding: Integer = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawRoundRectangle(ARect: TRect; ARounding: Integer = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawEllipse(ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawBitmapPart(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom: Double; ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom: Double;
    ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmapPart(ASourceRect: TRectF; ADestinationRect: TRectF; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmap(ALeft, ATop, ARight, ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmap(ARect: TRectF; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawEllipse(ARect: TRect; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); overload; virtual;
    procedure DrawBitmapPart(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom: Integer; ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom: Integer;
    ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmapPart(ASourceRect: TRect; ADestinationRect: TRect; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmap(ALeft, ATop, ARight, ABottom: Integer; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmap(ARect: TRect; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmap(ALeft, ATop: Integer; ABitmap: TTMSFNCBitmapHelperClass); overload; virtual;
    procedure DrawBitmap(ALeft, ATop: Single; ABitmap: TTMSFNCBitmapHelperClass); overload; virtual;
    procedure DrawCheckBox(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawCloseButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawExpanderButton(ARect: TRectF; AState: TTMSFNCGraphicsExpanderState = gesExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawCompactButton(ARect: TRectF; AState: TTMSFNCGraphicsCompactState = gcsExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawDropDownButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; ACenter: Boolean = False; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawRadioButton(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawProgressBar(ARect: TRectF; AValue: Single; AFormat: string = '%.0f%%'; AMax: Single = 100; AColor: TTMSFNCGraphicsColor = gcYellowgreen; ATextColor: TTMSFNCGraphicsColor = gcBlack; AShowText: Boolean = True; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawCheckBox(ARect: TRect; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawCloseButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawExpanderButton(ARect: TRect; AState: TTMSFNCGraphicsExpanderState = gesExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawCompactButton(ARect: TRect; AState: TTMSFNCGraphicsCompactState = gcsExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawDropDownButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; ACenter: Boolean = False; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawRadioButton(ARect: TRect; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    procedure DrawProgressBar(ARect: TRect; AValue: Single; AFormat: string = '%.0f%%'; AMax: Single = 100; AColor: TTMSFNCGraphicsColor = gcYellowgreen; ATextColor: TTMSFNCGraphicsColor = gcBlack; AShowText: Boolean = True; AEnabled: Boolean = True; AScaleFactor: Single = 1.0); overload; virtual;
    {$IFNDEF LIMITEDGRAPHICSMODE}
    procedure DrawBitmapWithName(ALeft, ATop, ARight, ABottom: Double; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmapWithName(ARect: TRectF; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawScaledBitmap(ARect: TRectF; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawScaledBitmap(ALeft, ATop, ARight, ABottom: Double; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmapWithName(ALeft, ATop, ARight, ABottom: Integer; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawBitmapWithName(ARect: TRect; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawScaledBitmap(ARect: TRect; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    procedure DrawScaledBitmap(ALeft, ATop, ARight, ABottom: Integer; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False); overload; virtual;
    {$ENDIF}
    function GetBitmapDrawRectangle(ALeft, ATop, ARight, ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRectF; overload; virtual;
    function GetBitmapDrawRectangle(ARect: TRectF; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRectF; overload; virtual;
    function GetBitmapDrawRectangle(ALeft, ATop, ARight, ABottom: Integer; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRect; overload; virtual;
    function GetBitmapDrawRectangle(ARect: TRect; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRect; overload; virtual;
    procedure ClipRect(ARect: TRectF); overload; virtual;
    function DrawBitmapPartSync(ASourceLeft: Double; ASourceTop: Double; ASourceRight: Double; ASourceBottom: Double; ADestinationLeft: Double; ADestinationTop: Double;
    ADestinationRight: Double; ADestinationBottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
    ACenter: Boolean = True; ACropping: Boolean = False): TTMSFNCBitmap; virtual; {$IFDEF WEBLIB} async;{$ENDIF}
    function CalculateTextSize(AText: string; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TSizeF; overload; virtual;
    function CalculateTextWidth(AText: string; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Single; overload; virtual;
    function CalculateTextHeight(AText: string; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Single; overload; virtual;
    function CalculateText(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF; overload; virtual;
    function CalculateTextSize(AText: string): TSizeF; overload; virtual;
    function CalculateTextWidth(AText: string): Single; overload; virtual;
    function CalculateTextHeight(AText: string): Single; overload; virtual;
    function CalculateText(AText: String): TRectF; overload; virtual;
    procedure ClipRect(ARect: TRect); overload; virtual;
    function CalculateTextSize(AText: string; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TSize; overload; virtual;
    function CalculateTextWidth(AText: string; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Integer; overload; virtual;
    function CalculateTextHeight(AText: string; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Integer; overload; virtual;
    function CalculateText(AText: String; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRect; overload; virtual;
    function CalculateTextSizeInt(AText: string): TSize; overload; virtual;
    function CalculateTextWidthInt(AText: string): Integer; overload; virtual;
    function CalculateTextHeightInt(AText: string): Integer; overload; virtual;
    function CalculateTextInt(AText: String): TRect; overload; virtual;
    function DrawText(APoint: TPointF; AText: String; AAngle: Single = 0 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ARect: TRectF; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ALeft, ATop, ARight, ABottom: Double; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ARect: TRect; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ALeft, ATop, ARight, ABottom: Integer; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ARect: TRectF; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string;AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ALeft, ATop, ARight, ABottom: Double; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ARect: TRect; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function DrawText(ALeft, ATop, ARight, ABottom: Integer; AText: String; var AControlID: String; var AControlValue: String; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading; AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
      AMinWidth: Single = -1; AMinHeight: Single = -1 {$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String; overload; virtual;
    function SaveState(ACanvasOnly: Boolean = False): TTMSFNCGraphicsSaveState; virtual;
    property Fill: TTMSFNCGraphicsFill read FFill;
    property Stroke: TTMSFNCGraphicsStroke read FStroke;
    property Font: TTMSFNCGraphicsFont read FFont;
    property Canvas: TCanvas read GetCanvas;
    property Context: TTMSFNCGraphicsContext read GetContext;
    property Bitmap: TBitmap read FBitmap;
    {$IFNDEF LIMITEDGRAPHICSMODE}
    property OptimizedHTMLDrawing: Boolean read FOptimizedHTMLDrawing write FOptimizedHTMLDrawing;
    property HighlightColor: TTMSFNCGraphicsColor read FHighlightColor write FHighlightColor;
    property HighlightTextColor: TTMSFNCGraphicsColor read FHighlightTextColor write FHighlightTextColor;
    property HighlightFontStyle: TFontStyles read FHighlightFontStyles write FHighlightFontStyles;
    {$IFDEF CMNLIB}
    property ImageList: TCustomImageList read FImageList write FImageList;
    {$ENDIF}
    property BitmapContainer: TTMSFNCBitmapContainer read FBitmapContainer write FBitmapContainer;
    property URLColor: TTMSFNCGraphicsColor read FURLColor write FURLColor default gcBlue;
    property URLUnderline: Boolean read FURLUnderline write FURLUnderline default True;
    class function ApplyHilight(AText, AHilight, ATag: string; ADoCase: Boolean): string;
    class function GetBitmapFromBitmapContainer(ABitmapContainer: TTMSFNCBitmapContainer; AName: string; AApplyScale: Boolean = False; AScale: Single = 0): TTMSFNCBitmap; virtual;
    class function GetScaledBitmap(ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; ABitmapContainer: TTMSFNCBitmapContainer = nil): TTMSFNCBitmap;
    {$ENDIF}
    class procedure GetAspectSize(var AWidth: Single; var AHeight: Single; AOriginalWidth: Single; AOriginalHeight: Single; ANewWidth: Single; ANewHeight: Single; AAspectRatio: Boolean = True;
      AStretch: Boolean = False; ACropping: Boolean = False); virtual;
    class procedure DrawSample(ACanvas: TCanvas; ARect: TRectF); virtual;
    class procedure SetDefaultGraphicColors; virtual;
    class function PointInCircle(APoint: TPointF; ACenter: TPointF; const ARadius: Single): Boolean; virtual;
    class function PointInPath(APoint: TPointF; APath: TTMSFNCGraphicsPath): Boolean; overload; virtual;
    class function PointInPath(APoint: TPoint; APath: TTMSFNCGraphicsPath): Boolean; overload; virtual;
    class function PointInPolygon(APoint: TPointF; APolygon: TTMSFNCGraphicsPathPolygon): Boolean; overload; virtual;
    class function PointInPolygon(APoint: TPoint; APolygon: TTMSFNCGraphicsPathPolygon): Boolean; overload; virtual;
    class function PointInRect(APoint: TPointF; ARect: TRectF): Boolean; overload; virtual;
    class function PointInRect(APoint: TPoint; ARect: TRect): Boolean; overload; virtual;
    class function GetColorRed(AColor: TTMSFNCGraphicsColor): Byte; virtual;
    class function GetColorGreen(AColor: TTMSFNCGraphicsColor): Byte; virtual;
    class function GetColorBlue(AColor: TTMSFNCGraphicsColor): Byte; virtual;
    {$IFDEF FMXLIB}
    class function GetColorAlpha(AColor: TTMSFNCGraphicsColor): Byte; virtual;
    {$ENDIF}
    class function ColorToHTML(AColor: TTMSFNCGraphicsColor): string; virtual;
    class function HTMLToColor(AHTML: string): TTMSFNCGraphicsColor; virtual;
    class function TextToColor(AText: string): TTMSFNCGraphicsColor; virtual;
  end;

  ITMSFNCGraphicsExport = interface
    ['{481CA803-8B50-4545-B212-57AC0D065D09}']
    procedure &Export({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF);
  end;

{$IFDEF WEBLIB}
const
  TMSFNCGRAPHICSCLOSE = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJ'+
                        'cEhZcwAACxIAAAsSAdLdfvwAAAAYdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuOWwzfk4AAABVSURBVDhPYxh+'+
                        'QBSI/0OYWAFIThjCxA5ACmAYHeCTQwHYFGITwwuQNSBjkgBFmkGAIgNAGv5BaZINQdYMA0Qbgk0zDBBlCCiR4FMA'+
                        'khOCMIcJYGAAAHvVMBv6PIFYAAAAAElFTkSuQmCC';
  TMSFNCGRAPHICSDOWN = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBA'+
                       'JqcGAAAAGdJREFUOI3tzjsOwjAURNGDUqSgTxU5K2AVrJtswjUsgHSR0qdxAZZFPrS+3ZvRzBsqf9MUtBtazJk+oM'+
                       'e0VTriiZCFX8nbpENMgfARjsn74vKj5IFruhfc8d6zIF9S/Hyk5HS4spMVeFcOjszaOwMAAAAASUVORK5CYII=';
  TMSFNCGRAPHICSDOWN2 = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAA'+
                        'RnQU1BAACxjwv8YQUAAAAJcEhZcwAACxIAAAsSAdLdfvwAAAAZdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuMT'+
                        'ZEaa/1AAAAdElEQVQ4T6XMsQ2AMBBD0UxBBwyA2L9jCWpY5bCLSJfgSFgUr+AHX4mIX2R0yOiQ0SGjQ8Ud5q7RCl'+
                        'vX5IEDLlhS4/gGvuV/5YEJTuCAR+qYjW/N/81Hko8Mx/QKST0yHJOMCYfDMcnokNEho0NGh4zfRXkAxSYKjdpwcI'+
                        'UAAAAASUVORK5CYII=';
  TMSFNCGRAPHICSUP = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBAJq'+
                     'cGAAAAEZJREFUOI3tjLEJACAMwLLo5EM+rTjpXV7g0oI4tYKbgYwJfJ6Txet4iu6Jxk10TTSuQACidzKAIrGik24ZpC'+
                     'PeJ8ky+DhZ1JENPrPndiwAAAAASUVORK5CYII=';
  TMSFNCGRAPHICSUP2 = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARn'+
                      'QU1BAACxjwv8YQUAAAAJcEhZcwAACxIAAAsSAdLdfvwAAAAZdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuMTZEaa'+
                      '/1AAAAcklEQVQ4T6XMMQ6AMAiF4S46eSEvremk5/IC+GggKfE1ER2+gZ9AEZFfaMygMYPGDBozaOyshu0aGo0eXmb4'+
                      'hEbw42qGTx4B/HiHCWYYPgmDOWEDPfbmT46uNWEwC/THTp/oLvQwfEFjBo0ZNGbQ+J6UG5Y9CmuVGrAsAAAAAElFTkS'+
                      'uQmCC';
  TMSFNCGRAPHICSPIN = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAAXNSR0IArs4c6QAAAARnQ'+
                      'U1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAZdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuMTZEaa/1'+
                      'AAABKklEQVQ4T2PAB0JDQ5mjoqIEgUxGiAiRIDo6WiY8PHwdEGvW19czQYUJA6AGhbCwsO9Aek1ISIiZvb09C1QKPwB'+
                      'qaAbi/1DNJ4HYiqDNIE1ADZ9AGqGa/wLp6xERESZAaVQ/AyUUgDgIiOdCbQFrQsL/gPgxUM4OFACSQMYdIP4FFPwNxH'+
                      '+gpqNoAortAYayHNSfjAwgNwMFvaCaMTQA8W+g+M7IyEgNiLuQAMgUoNsDgYruoWkCOe1gcHCwHlQpJgA6wwCo6DySJ'+
                      'hD+BDQwCaoEEwA1SQAVzQLiq0BcBMRXgBhk25PAwEBJqDJUkJaWxgr0QzoQbwYqdABGNCeQrQPSDKTv44w3oFPEgYqm'+
                      'Amk9UNqECjMCxWSBeAeUjx1ANWAkZCSD0AADAwDRpKHlJfOjAwAAAABJRU5ErkJggg==';
  TMSFNCGRAPHICSPIN2 = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAAXNSR0IArs4c6QAAAARn'+
                       'QU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAZdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuMTZEaa'+
                       '/1AAAA/UlEQVQ4T33Su2oCQRTGcddLYWmlQhTLCBIsQgoLSW2nj2Bj7yv4DGkCeYOQxiaBNJapbCI2ooU+gY3gbf1/'+
                       'w47MjosHfsvM7DlzY1JhGN6TQQGBM2bEOgke8IU60tGY4SYlqWGHT7wgC/PPT/SNoFDxH1owKyclWyrawsYJczwj8J'+
                       'O1tR4+oFX8OGONtpLLWGCPA47Q7H78ogqd06yoPXeg4qQCTfaDR1x3ZxuapYsl3NDWJnjCtUjcThNTuKHL6cPNM2yj'+
                       'hHfMMMQ/tNoGuoNYkeiTwwBjvCKPBlS8QuzFWPoU8QadQ29TY3qbFXxH/Ru2oYKbhww7kSdMXQD8wIToOQZE/AAAAA'+
                       'BJRU5ErkJggg==';
  TMSFNCGRAPHICSRIGHT = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMB'+
                        'AJqcGAAAAFNJREFUOI3tzzsSQFAQRNFDqSezHUuwYBJLsBuhEomE4/ll3HBq+nY1PxENRrS5xzK4L1jPSiISesxP'+
                        'JcORJJqwU9xthjrX/ko4mpBQocN0tf1rbDeZDIfcSud0AAAAAElFTkSuQmCC';
  TMSFNCGRAPHICSLEFT = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBA'+
                       'JqcGAAAAEpJREFUOI3lzKENgEAQAMEJBkkIDdDTSxp6gqMyCkB8IQgMFdwZAutn+U19Bi84MUbxhZrBawSXB+8RDD'+
                       'MaDkyvmQyZyYYuOvh6N24uDUKEV//MAAAAAElFTkSuQmCC';
  TMSFNCGRAPHICSEXPAND = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOvwAADr8'+
                         'BOAVTJAAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4zjOaXUAAAACVJREFUGFdj+P//P07MEBYW9h8Xxi'+
                         '8JAjglYACnBAzglMAEDAwARZ1DA4NRF38AAAAASUVORK5CYII=';

  TMSFNCGRAPHICSEXPANDSVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d'
    +'3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweC'
    +'IgaGVpZ2h0PSIxMnB4IiB2aWV3Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoI'
    +'HN0eWxlPSIgc3Ryb2tlOm5vbmU7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIg'
    +'ZD0iTSAxMS44Mzk4NDQgMi44ODY3MTkgQyAxMS42Mjg5MDYgMi42NzU3ODEgMTEuMjgxMjUgMi42NzU3ODEgMTEuMDcwMzEyIDI'
    +'uODg2NzE5IEwgNiA3Ljk1NzAzMSBMIDAuOTI5Njg4IDIuODg2NzE5IEMgMC43MTg3NSAyLjY3NTc4MSAwLjM3MTA5NCAyLjY3NT'
    +'c4MSAwLjE2MDE1NiAyLjg4NjcxOSBDIC0wLjA1NDY4NzUgMy4xMDE1NjIgLTAuMDU0Njg3NSAzLjQ0NTMxMiAwLjE2MDE1NiAzL'
    +'jY2MDE1NiBMIDUuNjEzMjgxIDkuMTEzMjgxIEMgNS43MTQ4NDQgOS4yMTQ4NDQgNS44NTU0NjkgOS4yNzM0MzggNiA5LjI3MzQz'
    +'OCBDIDYuMTQ0NTMxIDkuMjczNDM4IDYuMjg1MTU2IDkuMjE0ODQ0IDYuMzg2NzE5IDkuMTEzMjgxIEwgMTEuODM5ODQ0IDMuNjY'
    +'wMTU2IEMgMTIuMDU0Njg4IDMuNDQ1MzEyIDEyLjA1NDY4OCAzLjEwMTU2MiAxMS44Mzk4NDQgMi44ODY3MTkgWiBNIDExLjgzOT'
    +'g0NCAyLjg4NjcxOSAiLz4KPC9nPgo8L3N2Zz4K';

  TMSFNCGRAPHICSDOWNSVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d'
    +'3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweC'
    +'IgaGVpZ2h0PSIxMnB4IiB2aWV3Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoI'
    +'HN0eWxlPSIgc3Ryb2tlOm5vbmU7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIg'
    +'ZD0iTSAxMS44Mzk4NDQgMi44ODY3MTkgQyAxMS42Mjg5MDYgMi42NzU3ODEgMTEuMjgxMjUgMi42NzU3ODEgMTEuMDcwMzEyIDI'
    +'uODg2NzE5IEwgNiA3Ljk1NzAzMSBMIDAuOTI5Njg4IDIuODg2NzE5IEMgMC43MTg3NSAyLjY3NTc4MSAwLjM3MTA5NCAyLjY3NT'
    +'c4MSAwLjE2MDE1NiAyLjg4NjcxOSBDIC0wLjA1NDY4NzUgMy4xMDE1NjIgLTAuMDU0Njg3NSAzLjQ0NTMxMiAwLjE2MDE1NiAzL'
    +'jY2MDE1NiBMIDUuNjEzMjgxIDkuMTEzMjgxIEMgNS43MTQ4NDQgOS4yMTQ4NDQgNS44NTU0NjkgOS4yNzM0MzggNiA5LjI3MzQz'
    +'OCBDIDYuMTQ0NTMxIDkuMjczNDM4IDYuMjg1MTU2IDkuMjE0ODQ0IDYuMzg2NzE5IDkuMTEzMjgxIEwgMTEuODM5ODQ0IDMuNjY'
    +'wMTU2IEMgMTIuMDU0Njg4IDMuNDQ1MzEyIDEyLjA1NDY4OCAzLjEwMTU2MiAxMS44Mzk4NDQgMi44ODY3MTkgWiBNIDExLjgzOT'
    +'g0NCAyLjg4NjcxOSAiLz4KPC9nPgo8L3N2Zz4K';

  TMSFNCGRAPHICSUPSVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3'
    +'dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweCI'
    +'gaGVpZ2h0PSIxMnB4IiB2aWV3Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoIH'
    +'N0eWxlPSIgc3Ryb2tlOm5vbmU7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIgZ'
    +'D0iTSAxMS44Mzk4NDQgOC4zMzk4NDQgTCA2LjM4NjcxOSAyLjg4NjcxOSBDIDYuMjg1MTU2IDIuNzg1MTU2IDYuMTQ0NTMxIDIu'
    +'NzI2NTYyIDYgMi43MjY1NjIgQyA1Ljg1NTQ2OSAyLjcyNjU2MiA1LjcxNDg0NCAyLjc4NTE1NiA1LjYxMzI4MSAyLjg4NjcxOSB'
    +'MIDAuMTYwMTU2IDguMzM5ODQ0IEMgLTAuMDU0Njg3NSA4LjU1NDY4OCAtMC4wNTQ2ODc1IDguODk4NDM4IDAuMTYwMTU2IDkuMT'
    +'EzMjgxIEMgMC4zNzEwOTQgOS4zMjQyMTkgMC43MTg3NSA5LjMyNDIxOSAwLjkyOTY4OCA5LjExMzI4MSBMIDYgNC4wNDI5NjkgT'
    +'CAxMS4wNzAzMTIgOS4xMTMyODEgQyAxMS4xNzU3ODEgOS4yMTg3NSAxMS4zMTY0MDYgOS4yNzM0MzggMTEuNDUzMTI1IDkuMjcz'
    +'NDM4IEMgMTEuNTkzNzUgOS4yNzM0MzggMTEuNzM0Mzc1IDkuMjE4NzUgMTEuODM5ODQ0IDkuMTEzMjgxIEMgMTIuMDU0Njg4IDg'
    +'uODk4NDM4IDEyLjA1NDY4OCA4LjU1NDY4OCAxMS44Mzk4NDQgOC4zMzk4NDQgWiBNIDExLjgzOTg0NCA4LjMzOTg0NCAiLz4KPC'
    +'9nPgo8L3N2Zz4K';

  TMSFNCGRAPHICSRIGHTSVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3'
    +'dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweCI'
    +'gaGVpZ2h0PSIxMnB4IiB2aWV3Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoIH'
    +'N0eWxlPSIgc3Ryb2tlOm5vbmU7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIgZ'
    +'D0iTSA5LjExMzI4MSA1LjYxMzI4MSBMIDMuNjYwMTU2IDAuMTYwMTU2IEMgMy40NDUzMTIgLTAuMDU0Njg3NSAzLjEwMTU2MiAt'
    +'MC4wNTQ2ODc1IDIuODg2NzE5IDAuMTYwMTU2IEMgMi42NzU3ODEgMC4zNzEwOTQgMi42NzU3ODEgMC43MTg3NSAyLjg4NjcxOSA'
    +'wLjkyOTY4OCBMIDcuOTU3MDMxIDYgTCAyLjg4NjcxOSAxMS4wNzAzMTIgQyAyLjY3NTc4MSAxMS4yODEyNSAyLjY3NTc4MSAxMS'
    +'42Mjg5MDYgMi44ODY3MTkgMTEuODM5ODQ0IEMgMi45OTIxODggMTEuOTQ1MzEyIDMuMTMyODEyIDEyIDMuMjczNDM4IDEyIEMgM'
    +'y40MTQwNjIgMTIgMy41NTA3ODEgMTEuOTQ1MzEyIDMuNjYwMTU2IDExLjgzOTg0NCBMIDkuMTEzMjgxIDYuMzg2NzE5IEMgOS4y'
    +'MTQ4NDQgNi4yODUxNTYgOS4yNzM0MzggNi4xNDQ1MzEgOS4yNzM0MzggNiBDIDkuMjczNDM4IDUuODU1NDY5IDkuMjE0ODQ0IDU'
    +'uNzE0ODQ0IDkuMTEzMjgxIDUuNjEzMjgxIFogTSA5LjExMzI4MSA1LjYxMzI4MSAiLz4KPC9nPgo8L3N2Zz4K';

  TMSFNCGRAPHICSLEFTSVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3'
    +'dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweCI'
    +'gaGVpZ2h0PSIxMnB4IiB2aWV3Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoIH'
    +'N0eWxlPSIgc3Ryb2tlOm5vbmU7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIgZ'
    +'D0iTSA0LjA0Mjk2OSA2IEwgOS4xMTMyODEgMC45Mjk2ODggQyA5LjMyNDIxOSAwLjcxODc1IDkuMzI0MjE5IDAuMzcxMDk0IDku'
    +'MTEzMjgxIDAuMTYwMTU2IEMgOC44OTg0MzggLTAuMDU0Njg3NSA4LjU1NDY4OCAtMC4wNTQ2ODc1IDguMzM5ODQ0IDAuMTYwMTU'
    +'2IEwgMi44ODY3MTkgNS42MTMyODEgQyAyLjc4NTE1NiA1LjcxNDg0NCAyLjcyNjU2MiA1Ljg1NTQ2OSAyLjcyNjU2MiA2IEMgMi'
    +'43MjY1NjIgNi4xNDQ1MzEgMi43ODUxNTYgNi4yODUxNTYgMi44ODY3MTkgNi4zODY3MTkgTCA4LjMzOTg0NCAxMS44Mzk4NDQgQ'
    +'yA4LjQ0OTIxOSAxMS45NDUzMTIgOC41ODU5MzggMTIgOC43MjY1NjIgMTIgQyA4Ljg2NzE4OCAxMiA5LjAwNzgxMiAxMS45NDUz'
    +'MTIgOS4xMTMyODEgMTEuODM5ODQ0IEMgOS4zMjQyMTkgMTEuNjI4OTA2IDkuMzI0MjE5IDExLjI4MTI1IDkuMTEzMjgxIDExLjA'
    +'3MDMxMiBaIE0gNC4wNDI5NjkgNiAiLz4KPC9nPgo8L3N2Zz4K';

  TMSFNCGRAPHICSCLOSESVG = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciI'
    +'HhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTJweCIgaGVpZ2h0PSIxMnB4IiB2aWV3'
    +'Qm94PSIwIDAgMTIgMTIiIHZlcnNpb249IjEuMSI+CjxnIGlkPSJzdXJmYWNlMSI+CjxwYXRoIHN0eWxlPSIgc3Ryb2tlOm5vbmU'
    +'7ZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMCUsMCUsMCUpO2ZpbGwtb3BhY2l0eToxOyIgZD0iTSA3LjA2MjUgNiBMIDExLj'
    +'c4MTI1IDEuMjgxMjUgQyAxMi4wNzQyMTkgMC45ODgyODEgMTIuMDc0MjE5IDAuNTExNzE5IDExLjc4MTI1IDAuMjE4NzUgQyAxM'
    +'S40ODgyODEgLTAuMDc0MjE4OCAxMS4wMTE3MTkgLTAuMDc0MjE4OCAxMC43MTg3NSAwLjIxODc1IEwgNiA0LjkzNzUgTCAxLjI4'
    +'MTI1IDAuMjE4NzUgQyAwLjk4ODI4MSAtMC4wNzQyMTg4IDAuNTExNzE5IC0wLjA3NDIxODggMC4yMTg3NSAwLjIxODc1IEMgLTA'
    +'uMDc0MjE4OCAwLjUxMTcxOSAtMC4wNzQyMTg4IDAuOTg4MjgxIDAuMjE4NzUgMS4yODEyNSBMIDQuOTM3NSA2IEwgMC4yMTg3NS'
    +'AxMC43MTg3NSBDIC0wLjA3NDIxODggMTEuMDExNzE5IC0wLjA3NDIxODggMTEuNDg4MjgxIDAuMjE4NzUgMTEuNzgxMjUgQyAwL'
    +'jM2NzE4OCAxMS45MjU3ODEgMC41NTg1OTQgMTIgMC43NSAxMiBDIDAuOTQxNDA2IDEyIDEuMTMyODEyIDExLjkyNTc4MSAxLjI4'
    +'MTI1IDExLjc4MTI1IEwgNiA3LjA2MjUgTCAxMC43MTg3NSAxMS43ODEyNSBDIDEwLjg2NzE4OCAxMS45MjU3ODEgMTEuMDU4NTk'
    +'0IDEyIDExLjI1IDEyIEMgMTEuNDQxNDA2IDEyIDExLjYzMjgxMiAxMS45MjU3ODEgMTEuNzgxMjUgMTEuNzgxMjUgQyAxMi4wNz'
    +'QyMTkgMTEuNDg4MjgxIDEyLjA3NDIxOSAxMS4wMTE3MTkgMTEuNzgxMjUgMTAuNzE4NzUgWiBNIDcuMDYyNSA2ICIvPgo8L2c+C'
    +'jwvc3ZnPgo=';


{$ENDIF}

implementation

uses
  SysUtils, Math
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  ,VCL.TMSFNCGraphicsSVGEngine
  {$ENDIF}
  {$ENDIF}
  ,VCL.TMSFNCUtils
  {$IFNDEF LIMITEDGRAPHICSMODE}
  ,VCL.TMSFNCHTMLEngine
  {$ENDIF}
  ,VCL.TMSFNCGraphics.General
  {$IFDEF WEBLIB}
  ,VCL.TMSFNCGraphics.WEB
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,VCL.TMSFNCGraphics.Win
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  ,VCL.TMSFNCGraphics.iOS
  {$ENDIF}
  {$IFNDEF IOS}
  ,VCL.TMSFNCGraphics.Mac
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  ,VCL.TMSFNCGraphics.Android
  {$ENDIF}
  {$IFDEF UNIX}
  ,VCL.TMSFNCGraphics.Unix
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFNDEF MSWINDOWS}
  ,LCLIntF
  {$ENDIF}
  {$ENDIF}
  ;

{$IFNDEF LIMITEDGRAPHICSMODE}
{$R TMSFNCGraphics.res}
{$ENDIF}

procedure TTMSFNCGraphics.EndPrinting;
begin
  Context.EndPrinting;
end;

procedure TTMSFNCGraphics.EndScene;
begin
  Context.EndScene;
end;

procedure TTMSFNCGraphics.BeginPrinting;
begin
  Context.BeginPrinting;
end;

procedure TTMSFNCGraphics.BeginScene;
begin
  Context.BeginScene;
end;

constructor TTMSFNCGraphics.CreateNativeBitmapCanvas(AWidth: Integer = 1; AHeight: Integer = 1);
begin
  CreateBitmapCanvas(AWidth, AHeight, True);
end;

constructor TTMSFNCGraphics.CreateBitmapCanvas(AWidth: Integer = 1; AHeight: Integer = 1; ANative: Boolean = False; AHighDPI: Boolean = True);
begin
  FNative := ANative;
  FBitmap := TBitmap.Create{$IFDEF FMXLIB}(AWidth, AHeight){$ENDIF};
  {$IFDEF FMXLIB}
  if AHighDPI then
    FBitmap.BitmapScale := TTMSFNCUtils.GetDPIScale;
  {$ENDIF}
  FBitmap.Width := AWidth;
  FBitmap.Height := AHeight;
  FActiveCanvas := FBitmap.Canvas;
  if not ANative then
  begin
    FContextGeneral := TTMSFNCGraphicsContextGeneral.Create(Self);
    FContextGeneral.SetSize(AWidth, AHeight);
  end
  else
  begin
    FContextNative := GetNativeContextClass.Create(Self);
    FContextNative.SetSize(AWidth, AHeight);
  end;
  FFill := TTMSFNCGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  FFont := TTMSFNCGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  InitializeDefaultAppearance;
end;

constructor TTMSFNCGraphics.CreateNative(ACanvas: TCanvas);
begin
  Create(ACanvas, True);
end;

constructor TTMSFNCGraphics.Create(ACanvas: TCanvas; ANative: Boolean = False);
begin
  FNative := ANative;
  FActiveCanvas := ACanvas;
  if not ANative then
  begin
    FContextGeneral := TTMSFNCGraphicsContextGeneral.Create(Self);
    FContextGeneral.SetSize(1, 1);
  end
  else
  begin
    FContextNative := GetNativeContextClass.Create(Self);
    FContextNative.SetSize(1, 1);
  end;
  FFill := TTMSFNCGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  FFont := TTMSFNCGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  {$IFNDEF LIMITEDGRAPHICSMODE}
  FHighlightColor := gcBlue;
  FHighlightTextColor := gcWhite;
  FHighlightFontStyles := [];
  {$eNDIF}

  InitializeDefaultAppearance;
end;

procedure TTMSFNCGraphics.RestoreState(AState: TTMSFNCGraphicsSaveState; ACanvasOnly: Boolean = False);
begin
  if not ACanvasOnly then
  begin
    SetFill(AState.Fill);
    SetStroke(AState.Stroke);
    SetFont(AState.Font);
  end;

  Context.RestoreState(AState);

  AState.Free;
end;

function TTMSFNCGraphics.SaveState(ACanvasOnly: Boolean = False): TTMSFNCGraphicsSaveState;
begin
  Result := TTMSFNCGraphicsSaveState.Create;

  if not ACanvasOnly then
  begin
    Result.Fill.Assign(FFill);
    Result.Stroke.Assign(FStroke);
    Result.Font.AssignSource(FFont);
  end;

  Context.SaveState(Result);
end;

procedure TTMSFNCGraphics.StopSpecialPen;
begin
  Context.StopSpecialPen;
end;

procedure TTMSFNCGraphics.StartSpecialPen;
begin
  Context.StartSpecialPen;
end;

procedure TTMSFNCGraphics.Assign(Source: TTMSFNCGraphics);
begin
  if Source is TTMSFNCGraphics then
  begin
    FFill.BeginUpdate;
    FFill.Assign((Source as TTMSFNCGraphics).Fill);
    FFill.EndUpdate;
    FStroke.BeginUpdate;
    FStroke.Assign((Source as TTMSFNCGraphics).Stroke);
    FStroke.EndUpdate;
    FFont.BeginUpdate;
    FFont.AssignSource((Source as TTMSFNCGraphics).Font);
    FFont.EndUpdate;
  end;
end;

procedure TTMSFNCGraphics.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFill(AFill);
  FFill.Assign(AFill);
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFillKind(AKind);
  FFill.Kind := AKind;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFillColor(AColor);
  FFill.Color := AColor;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetStrokeKind(AKind);
  FStroke.Kind := AKind;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetStroke(AStroke);
  FStroke.Assign(AStroke);
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetStrokeWidth(AWidth: Single);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetStrokeWidth(AWidth);
  FStroke.Width := AWidth;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetStrokeColor(AColor: TTMSFNCGraphicsColor);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetStrokeColor(AColor);
  FStroke.Color := AColor;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.DrawLine(AFromPoint: TPoint; AToPoint: TPoint; AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown;
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown);
begin
  DrawLine(ConvertToPointF(AFromPoint), ConvertToPointF(AToPoint), AModifyPointModeFrom, AModifyPointModeTo);
end;

procedure TTMSFNCGraphics.DrawLine(AFromPoint: TPointF; AToPoint: TPointF; AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown;
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown);
begin
  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawLine(FStroke, AFromPoint, AToPoint, AModifyPointModeFrom, AModifyPointModeTo);
end;

procedure TTMSFNCGraphics.DrawPolyline(APolyline: TTMSFNCGraphicsPathPolygon);
begin
  if (FFill.Color <> gcNull) and (FFill.Kind <> gfkNone) then
    Context.FillPolyline(FFill, APolyline);

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawPolyline(FStroke, APolyline);
end;

procedure TTMSFNCGraphics.DrawPolygon(APolygon: TTMSFNCGraphicsPathPolygon);
var
  pth: TTMSFNCGraphicsPath;
begin
  if ((FFill.Color <> gcNull) and (FFill.Kind <> gfkNone)) or (FFill.Kind = gfkTexture) then
  begin
    case FFill.Kind of
      gfkTexture:
      begin
        pth := TTMSFNCGraphicsPath.Create;
        try
          pth.AddPolygon(APolygon);
          DrawTexture(pth.GetBounds, FFill.Texture, FFill.TextureMode);
        finally
          pth.Free;
        end;
      end;
      else
        Context.FillPolygon(FFill, APolygon);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawPolygon(FStroke, APolygon);
end;

procedure TTMSFNCGraphics.DrawPath(APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon);
begin
  if ((FFill.Color <> gcNull) and (FFill.Kind <> gfkNone)) or (FFill.Kind = gfkTexture) then
  begin
    case FFill.Kind of
      gfkTexture: DrawTexture(APath.GetBounds, FFill.Texture, FFill.TextureMode);
      else
        Context.FillPath(FFill, APath, APathMode);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawPath(FStroke, APath, APathMode);
end;

procedure TTMSFNCGraphics.DrawArc(const Center: TPoint; const Radius: TPoint; StartAngle: Integer; SweepAngle: Integer);
begin
  DrawArc(ConvertToPointF(Center), ConvertToPointF(Radius), StartAngle, SweepAngle);
end;

procedure TTMSFNCGraphics.DrawArc(const Center: TPointF; const Radius: TPointF; StartAngle: Single; SweepAngle: Single);
var
  pth: TTMSFNCGraphicsPath;
begin
  if (FFill.Color <> gcNull) and (FFill.Kind <> gfkNone) then
  begin
    case FFill.Kind of
      gfkTexture:
      begin
        pth := TTMSFNCGraphicsPath.Create;
        try
          pth.AddArc(Center, Radius, StartAngle, SweepAngle);
          DrawTexture(pth.GetBounds, FFill.Texture, FFill.TextureMode);
        finally
          pth.Free;
        end;
      end;
      else
        Context.FillArc(FFill, Center, Radius, StartAngle, SweepAngle);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawArc(FStroke, Center, Radius, StartAngle, SweepAngle);
end;

procedure TTMSFNCGraphics.DrawRoundRectangle(ARect: TRect; ARounding: Integer = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRoundRectangle(ConvertToRectF(ARect), ARounding, ACorners);
end;

procedure TTMSFNCGraphics.DrawRoundRectangle(ARect: TRectF; ARounding: Single = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRoundRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ARounding, ACorners, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(RectF(ALeft, ATop, ARight, ABottom), AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(ALeft, ATop, ARight, ABottom, AllSides, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ASides: TTMSFNCGraphicsSides; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(RectF(ALeft, ATop, ARight, ABottom), ASides, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ASides: TTMSFNCGraphicsSides; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
var
  r: TRectF;
  pth: TTMSFNCGraphicsPath;
begin
  r := RectF(ALeft, ATop, ARight, ABottom);
  if ((FFill.Color <> gcNull) and (FFill.Kind <> gfkNone)) or (FFill.Kind = gfkTexture) then
  begin
    case FFill.Kind of
      gfkTexture:
      begin
        pth := TTMSFNCGraphicsPath.Create;
        try
          pth.AddRectangle(r);
          DrawTexture(r, FFill.Texture, FFill.TextureMode);
        finally
          pth.Free;
        end;
      end;
      else
        Context.FillRect(FFill, r, AModifyRectMode);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawRect(FStroke, r, ASides, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRoundRectangle(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ARounding: Integer = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRoundRectangle(RectF(ALeft, ATop, ARight, ABottom), ARounding, ACorners);
end;

procedure TTMSFNCGraphics.DrawRoundRectangle(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ARounding: Single = 10; ACorners: TTMSFNCGraphicsCorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight]; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
var
  r: TRectF;
  rc: Single;
  pth: TTMSFNCGraphicsPath;
begin
  r := RectF(ALeft, ATop, ARight, ABottom);
  if ((FFill.Color <> gcNull) and (FFill.Kind <> gfkNone)) or (FFill.Kind = gfkTexture) then
  begin
    case FFill.Kind of
      gfkTexture:
      begin
        pth := TTMSFNCGraphicsPath.Create;
        try
          rc := ARounding;
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

          DrawTexture(r, FFill.Texture, FFill.TextureMode);
        finally
          pth.Free;
        end;
      end;
      else
        Context.FillRoundRect(FFill, r, ARounding, ACorners, AModifyRectMode);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawRoundRect(FStroke, r, ARounding, ACorners, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ARect: TRect; ASides: TTMSFNCGraphicsSides; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(ConvertToRectF(ARect), ASides, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ARect: TRectF; ASides: TTMSFNCGraphicsSides; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ASides, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ARect: TRect; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(ConvertToRectF(ARect), AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawRectangle(ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AModifyRectMode);
end;

procedure TTMSFNCGraphics.SetFontSize(ASize: Integer);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFontSize(ASize);
  FFont.Size := ASize;
  Dec(FBlockUpdate);
end;

function TTMSFNCGraphics.GetMatrix: TTMSFNCGraphicsMatrix;
begin
  Result := Context.GetMatrix;
end;

procedure TTMSFNCGraphics.SetMatrix(const AMatrix: TTMSFNCGraphicsMatrix);
begin
  Context.SetMatrix(AMatrix);
end;

procedure TTMSFNCGraphics.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFontColor(AColor);
  FFont.Color := AColor;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetFontName(AName: string);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFontName(AName);
  FFont.Name := AName;
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFont(AFont);
  FFont.AssignSource(AFont);
  Dec(FBlockUpdate);
end;

procedure TTMSFNCGraphics.SetFontStyles(AStyle: TFontStyles);
begin
  if FBlockUpdate > 0 then
    Exit;

  Inc(FBlockUpdate);
  Context.SetFontStyles(AStyle);
  FFont.Style := AStyle;
  Dec(FBlockUpdate);
end;

destructor TTMSFNCGraphics.Destroy;
begin
  if Assigned(FContextNative) then
  begin
    FContextNative.Free;
    FContextNative := nil;
  end;

  if Assigned(FContextGeneral) then
  begin
    FContextGeneral.Free;
    FContextGeneral := nil;
  end;

  if Assigned(FFont) then
  begin
    FFont.Free;
    FFont := nil;
  end;

  if Assigned(FFill) then
  begin
    FFill.OnChanged := nil;
    FFill.Free;
    FFill := nil;
  end;

  if Assigned(FStroke) then
  begin
    FStroke.OnChanged := nil;
    FStroke.Free;
    FStroke := nil;
  end;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  FBitmapContainer := nil;
  {$IFDEF CMNLIB}
  FImageList := nil;
  {$ENDIF}
  {$ENDIF}

  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;

  inherited;
end;

procedure TTMSFNCGraphics.DrawCloseButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawCloseButton(ConvertToRectF(ARect), ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawCloseButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
var
  bmp: TTMSFNCBitmapHelperClass;
  bmpa: TBitmap;
  r: TRectF;
  g: TTMSFNCGraphics;
begin
  DrawButton(ARect, ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
  if AAdaptToStyle then
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(Round(16 * AScaleFactor), Round(16 * AScaleFactor));
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TTMSFNCGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := TTMSFNCGraphics.DefaultSelectionFillColor;
      g.Stroke.Width := 2;
      g.DrawLine(PointF(3 * AScaleFactor, 3 * AScaleFactor), PointF(12 * AScaleFactor, 12 * AScaleFactor), gcpmRightDown);
      g.DrawLine(PointF(3 * AScaleFactor, 12 * AScaleFactor), PointF(12 * AScaleFactor, 3 * AScaleFactor), gcpmRightDown);
    finally
      g.EndScene;
      g.Free;
    end;

    try
      r := ARect;
      bmp := TTMSFNCBitmap.Create;
      try
        bmp.Assign(bmpa);
        DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    finally
      bmpa.Free;
    end;
  end
  else
  begin
   bmp := TTMSFNCBitmap.CreateFromResource('TMSFNCGRAPHICSCLOSESVG');

    try
      r := ARect;
      InflateRectEx(r, - 6 * AScaleFactor, - 6 * AScaleFactor);
      DrawBitmap(r, bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TTMSFNCGraphics.DrawExpanderButton(ARect: TRect; AState: TTMSFNCGraphicsExpanderState = gesExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawExpanderButton(ConvertToRectF(ARect), AState, ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawExpanderButton(ARect: TRectF; AState: TTMSFNCGraphicsExpanderState = gesExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
var
  bmpa: TBitmap;
  bmp: TTMSFNCBitmapHelperClass;
  r: TRectF;
  g: TTMSFNCGraphics;
begin
  DrawButton(ARect, ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
  if AAdaptToStyle then
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(Round(16 * AScaleFactor), Round(16 * AScaleFactor));
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TTMSFNCGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Width := 2;
      g.Stroke.Color := TTMSFNCGraphics.DefaultSelectionFillColor;
      case AState of
        gesCollapsed:
        begin
          g.DrawLine(PointF(5 * AScaleFactor, 6 * AScaleFactor), PointF(8 * AScaleFactor, 9 * AScaleFactor), gcpmRightDown);
          g.DrawLine(PointF(8 * AScaleFactor, 9 * AScaleFactor), PointF(11 * AScaleFactor, 6 * AScaleFactor), gcpmRightDown);
        end;
        gesExpanded:
        begin
          g.DrawLine(PointF(5 * AScaleFactor, 9 * AScaleFactor), PointF(8 * AScaleFactor, 6 * AScaleFactor), gcpmRightDown);
          g.DrawLine(PointF(8 * AScaleFactor, 6 * AScaleFactor), PointF(11 * AScaleFactor, 9 * AScaleFactor), gcpmRightDown);
        end;
      end;
    finally
      g.EndScene;
      g.Free;
    end;

    if Assigned(bmpa) then
    begin
      try
        r := ARect;
        bmp := TTMSFNCBitmap.Create;
        try
          bmp.Assign(bmpa);
          DrawBitmap(r, bmp);
        finally
          bmp.Free;
        end;
      finally
        bmpa.Free;
      end;
    end;
  end
  else
  begin
    case AState of
      gesCollapsed: bmp := TTMSFNCBitmap.CreateFromResource('TMSFNCGRAPHICSDOWNSVG');
      gesExpanded: bmp := TTMSFNCBitmap.CreateFromResource('TMSFNCGRAPHICSUPSVG');
      else
        bmp := nil;
    end;

    if Assigned(bmp) then
    begin
      try
        r := ARect;
        InflateRectEx(r, -4 * AScaleFactor, -4 * AScaleFactor);
        DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    end;
  end;
end;

procedure TTMSFNCGraphics.DrawCompactButton(ARect: TRect; AState: TTMSFNCGraphicsCompactState = gcsExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawCompactButton(ConvertToRectF(ARect), AState, ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawCompactButton(ARect: TRectF; AState: TTMSFNCGraphicsCompactState = gcsExpanded; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
var
  bmpa: TBitmap;
  bmp: TTMSFNCBitmapHelperClass;
  r: TRectF;
  g: TTMSFNCGraphics;
begin
  DrawButton(ARect, ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
  if AAdaptToStyle then
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(Round(16 * AScaleFactor), Round(16 * AScaleFactor));
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TTMSFNCGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Width := 2;
      g.Stroke.Color := TTMSFNCGraphics.DefaultSelectionFillColor;
      case AState of
        gcsCollapsed:
        begin
          g.DrawLine(PointF(6 * AScaleFactor, 5 * AScaleFactor), PointF(9 * AScaleFactor, 8 * AScaleFactor), gcpmRightDown);
          g.DrawLine(PointF(9 * AScaleFactor, 8 * AScaleFactor), PointF(6 * AScaleFactor, 11 * AScaleFactor), gcpmRightDown);
        end;
        gcsExpanded:
        begin
          g.DrawLine(PointF(9 * AScaleFactor, 5 * AScaleFactor), PointF(6 * AScaleFactor, 8 * AScaleFactor), gcpmRightDown);
          g.DrawLine(PointF(6 * AScaleFactor, 8 * AScaleFactor), PointF(9 * AScaleFactor, 11 * AScaleFactor), gcpmRightDown);
        end;
      end;
    finally
      g.EndScene;
      g.Free;
    end;

    if Assigned(bmpa) then
    begin
      try
        r := ARect;
        bmp := TTMSFNCBitmap.Create;
        try
          bmp.Assign(bmpa);
          DrawBitmap(r, bmp);
        finally
          bmp.Free;
        end;
      finally
        bmpa.Free;
      end;
    end;
  end
  else
  begin
    case AState of
      gcsCollapsed: bmp := TTMSFNCBitmap.CreateFromResource('TMSFNCGRAPHICSRIGHTSVG');
      gcsExpanded: bmp := TTMSFNCBitmap.CreateFromResource('TMSFNCGRAPHICSLEFTSVG');
      else
        bmp := nil;
    end;

    if Assigned(bmp) then
    begin
      try
        r := ARect;
        InflateRectEx(r, -4 * AScaleFactor, -4 * AScaleFactor);
        DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    end;
  end;
end;

procedure TTMSFNCGraphics.DrawDropDownButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; ACenter: Boolean = False; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawDropDownButton(ConvertToRectF(ARect), ADown, AFocused, AEnabled, ACenter, AAdaptToStyle, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawDropDownButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; ACenter: Boolean = False; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
var
  bmp: TTMSFNCBitmapHelperClass;
  bmpa: TBitmap;
  r: TRectF;
  g: TTMSFNCGraphics;
begin
  DrawButton(ARect, ADown, AFocused, AEnabled, AAdaptToStyle);
  if AAdaptToStyle then
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(Round(7 * AScaleFactor), Round(7 * AScaleFactor));
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TTMSFNCGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := TTMSFNCGraphics.DefaultSelectionFillColor;
      g.DrawLine(PointF(0, 1 * AScaleFactor), PointF(6 * AScaleFactor, 1 * AScaleFactor), gcpmRightDown);
      g.DrawLine(PointF(0, 2 * AScaleFactor), PointF(6 * AScaleFactor, 2 * AScaleFactor), gcpmRightDown);
      g.DrawLine(PointF(1 * AScaleFactor, 3 * AScaleFactor), PointF(5 * AScaleFactor, 3 * AScaleFactor), gcpmRightDown);
      g.DrawLine(PointF(2 * AScaleFactor, 4 * AScaleFactor), PointF(4 * AScaleFactor, 4 * AScaleFactor), gcpmRightDown);
      g.DrawLine(PointF(3 * AScaleFactor, 5 * AScaleFactor), PointF(3 * AScaleFactor, 5 * AScaleFactor), gcpmRightDown);
    finally
      g.EndScene;
      g.Free;
    end;

    try
      if (ARect.Right - bmpa.Width - 10 * AScaleFactor > ARect.Left) and not ACenter then
        r := RectF(ARect.Right - bmpa.Width - 10 * AScaleFactor, ARect.Top, ARect.Right, ARect.Bottom)
      else
        r := ARect;

      bmp := TTMSFNCBitmap.Create;
      try
        bmp.Assign(bmpa);
        DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    finally
      bmpa.Free;
    end;
  end
  else
  begin
    bmp := TTMSFNCBitmap.Create;
    bmp.LoadFromResource('TMSFNCGRAPHICSEXPANDSVG');

    try
      if not ACenter then
      begin
        r := RectF(ARect.Right - (16 * AScaleFactor), ARect.Top, ARect.Right - 2 * AScaleFactor, ARect.Bottom);
        InflateRectEx(r, - 2 * AScaleFactor, - 2 * AScaleFactor);
      end
      else
      begin
        r := ARect;
        InflateRectEx(r, - 4 * AScaleFactor, - 4 * AScaleFactor);
      end;

      DrawBitmap(r, bmp);
    finally
      bmp.Free;
    end;
  end;
end;


procedure TTMSFNCGraphics.DrawButton(ARect: TRect; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawButton(ConvertToRectF(ARect), ADown, AFocused, AEnabled, AAdaptToStyle, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawButton(ARect: TRectF; ADown: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AAdaptToStyle: Boolean = True; AScaleFactor: Single = 1.0);
var
  r: TRectF;
begin
  r := ARect;
  Stroke.Kind := gskSolid;
  Stroke.Width := 1;
  Fill.Kind := gfkSolid;

  if AAdaptToStyle then
  begin
    if AEnabled then
    begin
      if not AFocused then
      begin
        Stroke.Color := TTMSFNCGraphics.DefaultButtonStrokeColor;
        Fill.Color := TTMSFNCGraphics.DefaultButtonFillColor;
      end
      else
      begin
        Stroke.Color := TTMSFNCGraphics.DefaultButtonStrokeColorFocused;
        Fill.Color := TTMSFNCGraphics.DefaultButtonFillColorFocused;
      end;
    end
    else
    begin
      Stroke.Color := TTMSFNCGraphics.DefaultButtonStrokeColorDisabled;
      Fill.Color := TTMSFNCGraphics.DefaultButtonFillColorDisabled;
    end;
  end
  else
  begin
    if AEnabled then
    begin
      if not AFocused then
      begin
        Stroke.Color := gcDarkGray;
        Fill.Color := MakeGraphicsColor(225, 225, 225);
      end
      else
      begin
        Stroke.Color := MakeGraphicsColor(60, 127, 177);
        Fill.Color := MakeGraphicsColor(229, 241, 251);
      end;
    end
    else
    begin
      Stroke.Color := gcDarkGray;
      Fill.Color := gcLightgray;
    end;
  end;

  DrawRectangle(r);
  InflateRectEx(r, -1, -1);
  Fill.Kind := gfkNone;

  if not ADown then
    Stroke.Color := Fill.Color
  else
    Stroke.Color := Lighter(Stroke.Color, 40);

  DrawRectangle(r);
end;

procedure TTMSFNCGraphics.DrawCheckBox(ARect: TRect; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawCheckBox(ConvertToRectF(ARect), AChecked, AFocused, AEnabled, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawCheckBox(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
var
  c: TTMSFNCGraphicsColor;
  r: TRectF;
begin
  r := ARect;
  InflateRectEx(r, -1, -1);

  if AEnabled then
  begin
    if AFocused then
      c := gcSteelBlue
    else
      c := gcBlack;
  end
  else
    c := gcDarkgray;

  Fill.Kind := gfkSolid;
  if AEnabled then
    Fill.Color := Lighter(gcLightgray, 85)
  else
    Fill.Color := gcLightgray;

  Stroke.Width := 1;
  Stroke.Kind := gskSolid;
  Stroke.Color := c;
  DrawRectangle(r);
  InflateRectEx(r, -(r.Right - r.Left) / 5, -(r.Bottom - r.Top) / 5);
  Stroke.Width := 2;
  Stroke.Color := c;
  if AChecked then
  begin
    {$IFDEF FMXLIB}
    DrawLine(PointF(r.Left + 1, r.Top + 1), PointF(r.Right - 1, r.Bottom - 1));
    DrawLine(PointF(r.Right - 1, r.Top + 1), PointF(r.Left + 1, r.Bottom - 1));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    DrawLine(PointF(r.Left, r.Top), PointF(r.Right - 1, r.Bottom - 1));
    DrawLine(PointF(r.Right - 1, r.Top), PointF(r.Left, r.Bottom - 1));
    {$ENDIF}
  end;
end;

procedure TTMSFNCGraphics.DrawProgressBar(ARect: TRect; AValue: Single; AFormat: string = '%.0f%%'; AMax: Single = 100; AColor: TTMSFNCGraphicsColor = gcYellowgreen; ATextColor: TTMSFNCGraphicsColor = gcBlack; AShowText: Boolean = True; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawProgressBar(ConvertToRectF(ARect), AValue, AFormat, AMax, AColor, ATextColor, AShowText, AEnabled, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawProgressBar(ARect: TRectF; AValue: Single; AFormat: string = '%.0f%%'; AMax: Single = 100; AColor: TTMSFNCGraphicsColor = gcYellowgreen; ATextColor: TTMSFNCGraphicsColor = gcBlack; AShowText: Boolean = True; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
var
  r, rp: TRectF;
  v: Single;
  tc, fc: TTMSFNCGraphicsColor;
begin
  r := ARect;
  rp := r;

  fc := Fill.Color;
  if AEnabled then
    Fill.Color := Lighter(gcLightGray, 75)
  else
    Fill.Color := gcLightGray;

  Fill.Kind := gfkSolid;
  Stroke.Kind := gskSolid;
  Stroke.Color := gcDarkgray;

  DrawRectangle(r);

  v := Max(0, Min(AValue, AMax));

  if (v >= 0) and (v <= AMax) and (AMax > 0) then
  begin
    InflateRectEx(rp, -1, -1);
    rp.Right := rp.Left + (rp.Right - r.Left) * v / AMax;

    Fill.Color := AColor;
    Stroke.Color := Fill.Color;

    DrawRectangle(rp);

    if AShowText then
    begin
      tc := Font.Color;
      Font.Color := ATextColor;
      DrawText(r, Format(AFormat, [v / AMax * 100]), False, gtaCenter, gtaCenter);
      Font.Color := tc;
    end;
  end;

  Fill.Color := fc;

end;

procedure TTMSFNCGraphics.DrawRadioButton(ARect: TRect; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
begin
  DrawRadioButton(ConvertToRectF(ARect), AChecked, AFocused, AEnabled, AScaleFactor);
end;

procedure TTMSFNCGraphics.DrawRadioButton(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True; AScaleFactor: Single = 1.0);
var
  c: TTMSFNCGraphicsColor;
  r: TRectF;
begin
  r := ARect;
  InflateRectEx(r, -1, -1);

  if AEnabled then
  begin
    if AFocused then
      c := gcSteelBlue
    else
      c := gcBlack;
  end
  else
    c := gcDarkgray;

  Fill.Kind := gfkSolid;
  if AEnabled then
    Fill.Color := Lighter(gcLightgray, 85)
  else
    Fill.Color := gcLightgray;

  Stroke.Kind := gskSolid;
  Stroke.Width := 1;
  Stroke.Color := c;
  DrawEllipse(r);
  InflateRectEx(r, -(r.Right - r.Left) / 5, -(r.Bottom - r.Top) / 5);
  Fill.Kind := gfkSolid;
  Fill.Color := c;
  if AChecked then
  begin
    DrawEllipse(r);
  end;
end;

procedure TTMSFNCGraphics.DrawBitmap(ALeft, ATop: Single; ABitmap: TTMSFNCBitmapHelperClass);
begin
  DrawBitmap(RectF(ALeft, ATop, ALeft + ABitmap.Width, ATop + ABitmap.Height), ABitmap);
end;

procedure TTMSFNCGraphics.DrawBitmap(ALeft, ATop: Integer; ABitmap: TTMSFNCBitmapHelperClass);
begin
  DrawBitmap(Rect(ALeft, ATop, ALeft + ABitmap.Width, ATop + ABitmap.Height), ABitmap);
end;

procedure TTMSFNCGraphics.DrawBitmap(ARect: TRect; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmap(ConvertToRectF(ARect), ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmap(ARect: TRectF; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmap(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmap(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmap(RectF(ALeft, ATop, ARight, ABottom), ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmap(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
var
  r: TRectF;
begin
  if not Assigned(ABitmap) then
    Exit;

  r := RectF(ALeft, ATop, ARight, ABottom);
  r := GetBitmapDrawRectangle(r, ABitmap, AAspectRatio, AStretch, ACenter, ACropping);

  {$IFDEF SVGSUPPORT}
  if not DrawSVG(ABitmap, r) then
  {$ENDIF}
    Context.DrawBitmap(BitmapToDrawBitmap(ABitmap), RectF(0, 0, ABitmap.Width, ABitmap.Height), r, 1);
end;

function TTMSFNCGraphics.InternalDrawBitmapPartSync(ASourceLeft: Double; ASourceTop: Double; ASourceRight: Double; ASourceBottom: Double; ADestinationLeft: Double; ADestinationTop: Double;
  ADestinationRight: Double; ADestinationBottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
  ACenter: Boolean = True; ACropping: Boolean = False): {$IFDEF WEBLIB}TJSPromise{$ELSE}TTMSFNCBitmap{$ENDIF};
var
  r, rs: TRectF;
  {$IFNDEF WEBLIB}
  g: TTMSFNCGraphics;
  bmp: TTMSFNCBitmap;
  {$ENDIF}
begin
  Result := nil;
  r := RectF(ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom);
  rs := RectF(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom);
  if RectIsEmpty(r) or RectIsEmpty(rs) then
    Exit;

  {$IFDEF SVGSUPPORT}
  if not HasSVG(ABitmap) then
  {$ENDIF}
  begin
    {$IFDEF WEBLIB}
    Result := TJSPromise.New(
    procedure(AResolve, AReject : TJSPromiseResolver)
    var
      bmp: TTMSFNCBitmap;
      g: TTMSFNCGraphics;
    begin
    {$ENDIF}
      g := TTMSFNCGraphics.CreateBitmapCanvas(Round(ASourceRight - ASourceLeft), Round(ASourceBottom - ASourceTop), FNative);
      try
        g.BeginScene;
        g.DrawBitmap(-ASourceLeft, -ASourceTop, -ASourceLeft + g.Bitmap.Width, -ASourceTop + g.Bitmap.Height, ABitmap, False, False, False, False);
        g.EndScene;
        bmp := TTMSFNCBitmap.Create;
        bmp.Assign(g.Bitmap);
        {$IFDEF WEBLIB}
        AResolve(bmp);
        {$ELSE}
        Result := bmp;
        {$ENDIF}
      finally
        g.Free;
      end;
    {$IFDEF WEBLIB}
    end);
    {$ENDIF}
  end;
end;

function TTMSFNCGraphics.DrawBitmapPartSync(ASourceLeft: Double; ASourceTop: Double; ASourceRight: Double; ASourceBottom: Double; ADestinationLeft: Double; ADestinationTop: Double;
  ADestinationRight: Double; ADestinationBottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
  ACenter: Boolean = True; ACropping: Boolean = False): TTMSFNCBitmap;
begin
  Result := {$IFDEF WEBLIB}await(TTMSFNCBitmap,{$ENDIF} InternalDrawBitmapPartSync(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom,
    ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom, ABitmap, AAspectRatio, AStretch, ACenter, ACropping){$IFDEF WEBLIB}){$ENDIF};
end;

procedure TTMSFNCGraphics.DrawBitmapPart(ASourceRect: TRect; ADestinationRect: TRect; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmapPart(ConvertToRectF(ASourceRect), ConvertToRectF(ADestinationRect), ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapPart(ASourceRect: TRectF; ADestinationRect: TRectF; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True;
  AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmapPart(ASourceRect.Left, ASourceRect.Top, ASourceRect.Right, ASourceRect.Bottom, ADestinationRect.Left, ADestinationRect.Top, ADestinationRect.Right, ADestinationRect.Bottom,
    ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapPart(ASourceLeft: Integer; ASourceTop: Integer; ASourceRight: Integer; ASourceBottom: Integer; ADestinationLeft: Integer; ADestinationTop: Integer;
  ADestinationRight: Integer; ADestinationBottom: Integer; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
  ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmapPart(RectF(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom), RectF(ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom), ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapPart(ASourceLeft: Double; ASourceTop: Double; ASourceRight: Double; ASourceBottom: Double; ADestinationLeft: Double; ADestinationTop: Double;
  ADestinationRight: Double; ADestinationBottom: Double; ABitmap: TTMSFNCBitmapHelperClass; AAspectRatio: Boolean = True; AStretch: Boolean = False;
  ACenter: Boolean = True; ACropping: Boolean = False);
var
  r, rs: TRectF;
  g: TTMSFNCGraphics;
  bmp: TTMSFNCBitmap;
begin
  r := RectF(ADestinationLeft, ADestinationTop, ADestinationRight, ADestinationBottom);
  rs := RectF(ASourceLeft, ASourceTop, ASourceRight, ASourceBottom);
  if RectIsEmpty(r) or RectIsEmpty(rs) then
    Exit;

  {$IFDEF SVGSUPPORT}
  if not DrawSVG(ABitmap, r) then
  {$ENDIF}
  begin
    g := TTMSFNCGraphics.CreateBitmapCanvas(Round(ASourceRight - ASourceLeft), Round(ASourceBottom - ASourceTop), FNative);
    try
      g.BeginScene;
      g.DrawBitmap(-ASourceLeft, -ASourceTop, -ASourceLeft + g.Bitmap.Width, -ASourceTop + g.Bitmap.Height, ABitmap, False, False, False, False);
      g.EndScene;
      bmp := TTMSFNCBitmap.Create;
      try
        bmp.Assign(g.Bitmap);
        DrawBitmap(r, bmp, AAspectRatio, AStretch, ACenter, ACropping);
      finally
        bmp.Free;
      end;
    finally
      g.Free;
    end;
  end;
end;

class procedure TTMSFNCGraphics.GetAspectSize(var AWidth: Single; var AHeight: Single; AOriginalWidth: Single; AOriginalHeight: Single; ANewWidth: Single; ANewHeight: Single;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACropping: Boolean = False);
var
  arc, ar: Single;
begin
  if AAspectRatio then
  begin
    if (AOriginalWidth > 0) and (AOriginalHeight > 0) and (ANewWidth > 0) and (ANewHeight > 0) then
    begin
      if (AOriginalWidth < ANewWidth) and (AOriginalHeight < ANewHeight) and (not AStretch) then
      begin
        AWidth := AOriginalWidth;
        AHeight := AOriginalHeight;
      end
      else
      begin
        if AOriginalWidth / AOriginalHeight < ANewWidth / ANewHeight then
        begin
          AHeight := ANewHeight;
          AWidth := ANewHeight * AOriginalWidth / AOriginalHeight;
        end
        else
        begin
          AWidth := ANewWidth;
          AHeight := ANewWidth * AOriginalHeight / AOriginalWidth;
        end;
      end;
    end
    else
    begin
      AWidth := 0;
      AHeight := 0;
    end;
  end
  else
  begin
    if AStretch then
    begin
      AWidth := ANewWidth;
      AHeight := ANewHeight;
    end
    else
    begin
      AWidth := AOriginalWidth;
      AHeight := AOriginalHeight;

      if ACropping then
      begin
        if (AWidth >= AHeight) and (AWidth > 0) then
        begin
          AHeight := AOriginalWidth / AWidth * AHeight;
          AWidth := AOriginalWidth;
        end
        else
        if (AHeight >= AWidth) and (AHeight > 0) then
        begin
          AWidth := AOriginalHeight / AHeight * AWidth;
          AHeight := AOriginalHeight;
        end;

        if AHeight = 0 then
          ar := 1
        else
          ar := AWidth / AHeight;

        if AOriginalHeight = 0 then
          arc := 1
        else
          arc := AOriginalWidth / AOriginalHeight;

        if (ar < 1) or (arc > ar) then
        begin
          AHeight := AOriginalWidth / ar;
          AWidth := AOriginalWidth;
        end
        else
        begin
          AWidth := ar * AOriginalHeight;
          AHeight := AOriginalHeight;
        end;
      end;
    end;
  end;
end;

function TTMSFNCGraphics.GetBitmapDrawRectangle(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ABitmap: TTMSFNCBitmapHelperClass;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRect;
begin
  Result := ConvertToRect(GetBitmapDrawRectangle(RectF(ALeft, ATop, ARight, ABottom), ABitmap, AAspectRatio, AStretch, ACenter, ACropping));
end;

function TTMSFNCGraphics.GetBitmapDrawRectangle(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ABitmap: TTMSFNCBitmapHelperClass;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRectF;
var
  bmp: TTMSFNCBitmapHelperClass;
  rdest: TRectF;
  w, h: Single;
  x, y: Single;
begin
  Result := RectF(0, 0, 0, 0);
  bmp := ABitmap;
  if Assigned(bmp) then
  begin
    x := 0;
    y := 0;
    w := 0;
    h := 0;
    GetAspectSize(w, h, bmp.Width, bmp.Height, ARight - ALeft, ABottom - ATop, AAspectRatio, AStretch, ACropping);
    if ACenter or ACropping then
    begin
      x := Round((ARight - ALeft - w) / 2);
      y := Round((ABottom - ATop - h) / 2);
    end;

    rdest := RectF(ALeft + x, ATop + y, ALeft + x + w, ATop + y + h);
    Result := rdest;
  end;
end;

function TTMSFNCGraphics.GetBitmapDrawRectangle(ARect: TRect; ABitmap: TTMSFNCBitmapHelperClass;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRect;
begin
  Result := ConvertToRect(GetBitmapDrawRectangle(ConvertToRectF(ARect), ABitmap, AAspectRatio, AStretch, ACenter, ACropping));
end;

function TTMSFNCGraphics.GetBitmapDrawRectangle(ARect: TRectF; ABitmap: TTMSFNCBitmapHelperClass;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False): TRectF;
begin
  Result := GetBitmapDrawRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ABitmap, AAspectRatio, AStretch, ACenter, ACropping);
end;

function TTMSFNCGraphics.GetContext: TTMSFNCGraphicsContext;
begin
  if FNative then
    Result := FContextNative
  else
    Result := FContextGeneral;
end;

function TTMSFNCGraphics.GetCanvas: TCanvas;
begin
  Result := FActiveCanvas;
end;

{$IFNDEF LIMITEDGRAPHICSMODE}
class function TTMSFNCGraphics.GetScaledBitmap(ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0; ABitmapContainer: TTMSFNCBitmapContainer = nil): TTMSFNCBitmap;
var
  b: TTMSFNCScaledBitmap;
  bmp: TTMSFNCBitmap;
begin
  Result := nil;
  if Assigned(ABitmaps) then
  begin
    if AScale > 0 then
      b := ABitmaps.GetItemByScale(AScale)
    else
      b := ABitmaps.GetItemByScale(TTMSFNCUtils.GetDPIScale);

    if Assigned(b) then
    begin
      if Assigned(b.Bitmap) and not IsBitmapEmpty(b.Bitmap) then
        Result := b.Bitmap
      else
      begin
        bmp := GetBitmapFromBitmapContainer(ABitmapContainer, b.BitmapName, True, AScale);
        if Assigned(bmp) and not IsBitmapEmpty(bmp) then
          Result := bmp
        else
        begin
          bmp := GetBitmapFromBitmapContainer(ABitmapContainer, b.BitmapName, False);
          if Assigned(bmp) and not IsBitmapEmpty(bmp) then
            Result := bmp
        end;
      end;
    end;
  end;
end;

class function TTMSFNCGraphics.ApplyHilight(AText: string; AHilight: string; ATag: string; ADoCase: Boolean): String;
begin
  Result := HiLight(AText, AHilight, ATag, ADoCase);
end;

class function TTMSFNCGraphics.GetBitmapFromBitmapContainer(ABitmapContainer: TTMSFNCBitmapContainer; AName: string; AApplyScale: Boolean = False; AScale: Single = 0): TTMSFNCBitmap;
begin
  Result := nil;
  if Assigned(ABitmapContainer) and (AName <> '') then
  begin
    if AApplyScale then
    begin
      if AScale > 0 then
        Result := TTMSFNCBitmap(ABitmapContainer.FindBitmap(AName + '_' + FloatToStr(AScale)))
      else
        Result := TTMSFNCBitmap(ABitmapContainer.FindBitmap(AName + '_' + FloatToStr(TTMSFNCUtils.GetDPIScale)));
    end;

    if not Assigned(Result) then
      Result := TTMSFNCBitmap(ABitmapContainer.FindBitmap(AName));
  end;
end;

procedure TTMSFNCGraphics.DrawBitmapWithName(ARect: TRect; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawBitmapWithName(ConvertToRectF(ARect), ABitmapName, AApplyScale, AScale, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapWithName(ARect: TRectF; ABitmapName: string; AApplyScale: Boolean = False; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
var
  b: TTMSFNCBitmap;
begin
  b := GetBitmapFromBitmapContainer(FBitmapContainer, ABitmapName, AApplyScale, AScale);
  if Assigned(b) then
    DrawBitmap(ARect, b, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapWithName(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ABitmapName: string;
  AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True;
  ACropping: Boolean = False);
begin
  DrawBitmapWithName(RectF(ALeft, ATop, ARight, ABottom), ABitmapName, AApplyScale, AScale, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawBitmapWithName(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ABitmapName: string;
  AApplyScale: Boolean = False; AScale: Single = 0; AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True;
  ACropping: Boolean = False);
var
  b: TTMSFNCBitmap;
begin
  b := GetBitmapFromBitmapContainer(FBitmapContainer, ABitmapName, AApplyScale, AScale);
  if Assigned(b) then
    DrawBitmap(ALeft, ATop, ARight, ABottom, b, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawScaledBitmap(ARect: TRect; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawScaledBitmap(ConvertToRectF(ARect), ABitmaps, AScale, AAspectRatio, AStretch, ACenter, ACropping);
end;

procedure TTMSFNCGraphics.DrawScaledBitmap(ARect: TRectF; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
var
  b: TTMSFNCScaledBitmap;
  bmp: TTMSFNCBitmap;
begin
  if Assigned(ABitmaps) then
  begin
    if AScale > 0 then
      b := ABitmaps.GetItemByScale(AScale)
    else
      b := ABitmaps.GetItemByScale(TTMSFNCUtils.GetDPIScale);

    if Assigned(b) then
    begin
      if Assigned(b.Bitmap) and not IsBitmapEmpty(b.Bitmap) then
        DrawBitmap(ARect, b.Bitmap, AAspectRatio, AStretch, ACenter, ACropping)
      else
      begin
        bmp := GetBitmapFromBitmapContainer(FBitmapContainer, b.BitmapName, True, AScale);
        if Assigned(bmp) and not IsBitmapEmpty(bmp) then
          DrawBitmap(ARect, bmp, AAspectRatio, AStretch, ACenter, ACropping)
        else
          DrawBitmapWithName(ARect, b.BitmapName, False, b.Scale, AAspectRatio, AStretch, ACenter, ACropping);
      end;
    end;
  end;
end;

procedure TTMSFNCGraphics.DrawScaledBitmap(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
begin
  DrawScaledBitmap(RectF(ALeft, ATop, ARight, ABottom), ABitmaps, AScale, AAspectRatio, AStretch, ACenter, ACropping);
end;

{$IFDEF SVGSUPPORT}
function TTMSFNCGraphics.HasSVG(ABitmap: TTMSFNCBitmapHelperClass): Boolean;
{$IFNDEF WEBLIB}
var
  si: TTMSFNCGraphicsSVGImport;
{$ENDIF}
begin
  Result := False;
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

    if Assigned(si) and si.HasElements then
      Result := True;
  end;
  {$ENDIF}
end;

function TTMSFNCGraphics.DrawSVG(ABitmap: TTMSFNCBitmapHelperClass;
  ARect: TRectF): Boolean;
{$IFNDEF WEBLIB}
var
  si: TTMSFNCGraphicsSVGImport;
{$ENDIF}
begin
  Result := False;
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

    if Assigned(si) and si.HasElements then
    begin
      si.Draw(Self, ARect{$IFDEF CMNLIB}, True{$ELSE}, FNative, not FNative{$ENDIF});
      Result := True;
    end;
  end;
  {$ENDIF}
end;
{$ENDIF}

procedure TTMSFNCGraphics.DrawScaledBitmap(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; ABitmaps: TTMSFNCScaledBitmaps; AScale: Single = 0;
  AAspectRatio: Boolean = True; AStretch: Boolean = False; ACenter: Boolean = True; ACropping: Boolean = False);
var
  b: TTMSFNCScaledBitmap;
  bmp: TTMSFNCBitmap;
begin
  if Assigned(ABitmaps) then
  begin
    if AScale > 0 then
      b := ABitmaps.GetItemByScale(AScale)
    else
      b := ABitmaps.GetItemByScale(TTMSFNCUtils.GetDPIScale);

    if Assigned(b) then
    begin
      if Assigned(b.Bitmap) and not IsBitmapEmpty(b.Bitmap) then
        DrawBitmap(ALeft, ATop, ARight, ABottom, b.Bitmap, AAspectRatio, AStretch, ACenter, ACropping)
      else
      begin
        bmp := GetBitmapFromBitmapContainer(FBitmapContainer, b.BitmapName, True, AScale);
        if Assigned(bmp) and not IsBitmapEmpty(bmp) then
          DrawBitmap(ALeft, ATop, ARight, ABottom, bmp, AAspectRatio, AStretch, ACenter, ACropping)
        else
          DrawBitmapWithName(ALeft, ATop, ARight, ABottom, b.BitmapName, False, b.Scale, AAspectRatio, AStretch, ACenter, ACropping);
      end;
    end;
  end;
end;
{$ENDIF}

procedure TTMSFNCGraphics.ClipRect(ARect: TRect);
begin
  ClipRect(ConvertToRect(ARect));
end;

procedure TTMSFNCGraphics.ClipRect(ARect: TRectF);
begin
  Context.ClipRect(ARect);
end;

procedure TTMSFNCGraphics.DrawFocusPath(APath: TTMSFNCGraphicsPath; AColor: TTMSFNCGraphicsColor = gcBlack);
begin
  Context.DrawFocusPath(FStroke, APath, AColor);
end;

procedure TTMSFNCGraphics.DrawFocusRectangle(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; AColor: TTMSFNCGraphicsColor = gcBlack; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawFocusRectangle(Rect(ALeft, ATop, ARight, ABottom), AColor, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawFocusRectangle(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; AColor: TTMSFNCGraphicsColor = gcBlack; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
var
  r: TRectF;
begin
  r := RectF(ALeft, ATop, ARight, ABottom);
  Context.DrawFocusRectangle(FStroke, r, AColor, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawFocusRectangle(ARect: TRect; AColor: TTMSFNCGraphicsColor = gcBlack; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawFocusRectangle(ConvertToRectF(ARect), AColor, AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawFocusRectangle(ARect: TRectF; AColor: TTMSFNCGraphicsColor = gcBlack; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawFocusRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AColor, AModifyRectMode);
end;

procedure TTMSFNCGraphics.FontChanged(Sender: TObject);
begin
  SetFont(Sender as TTMSFNCGraphicsFont);
end;

procedure TTMSFNCGraphics.StrokeChanged(Sender: TObject);
begin
  SetStroke(Sender as TTMSFNCGraphicsStroke);
end;

procedure TTMSFNCGraphics.FillChanged(Sender: TObject);
begin
  SetFill(Sender as TTMSFNCGraphicsFill);
end;

function TTMSFNCGraphics.DrawText(ALeft, ATop, ARight, ABottom: Integer; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  AControlID, AControlValue, AControlType: string;
begin
  AControlID := '';
  AControlValue := '';
  AControlType := '';
  Result := DrawText(RectF(ALeft, ATop, ARight, ABottom), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming,
    AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ALeft, ATop, ARight, ABottom: Double; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  AControlID, AControlValue, AControlType: string;
begin
  AControlID := '';
  AControlValue := '';
  AControlType := '';
  Result := DrawText(RectF(ALeft, ATop, ARight, ABottom), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming,
    AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ALeft, ATop, ARight, ABottom: Integer; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
begin
  Result := DrawText(RectF(ALeft, ATop, ARight, ABottom), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming,
    AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ALeft, ATop, ARight, ABottom: Double; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
begin
  Result := DrawText(RectF(ALeft, ATop, ARight, ABottom), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming,
    AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

procedure TTMSFNCGraphics.InitializeDefaultAppearance;
begin
  {$IFNDEF LIMITEDGRAPHICSMODE}
  FURLColor := gcBlue;
  FURLUnderline := True;
  {$ENDIF}
  SetFill(Fill);
  SetStroke(Stroke);
  SetFont(Font);
end;

{$IFDEF FMXLIB}
class function TTMSFNCGraphics.GetColorAlpha(AColor: TTMSFNCGraphicsColor): Byte;
begin
  Result := TAlphaColorRec(AColor).A;
end;
{$ENDIF}

class function TTMSFNCGraphics.GetColorRed(AColor: TTMSFNCGraphicsColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).R;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetRValue(AColor);
  {$ENDIF}
end;

class function TTMSFNCGraphics.GetColorGreen(AColor: TTMSFNCGraphicsColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).G;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetGValue(AColor);
  {$ENDIF}
end;

class function TTMSFNCGraphics.GetColorBlue(AColor: TTMSFNCGraphicsColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).B;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetBValue(AColor);
  {$ENDIF}
end;

class function TTMSFNCGraphics.TextToColor(AText: string): TTMSFNCGraphicsColor;
var
  i: integer;
  s: string;
  c: TStringList;
begin
  s := AText;

  s := LowerCase(s);
  if pos('cl',s) = 1 then
    Delete(s,1,2);

  if pos('cla',s) = 1 then
    Delete(s,1,3);

  if pos('gc',s) = 1 then
    Delete(s,1,2);

  Result := gcBlack;
  c := ColorLookup;
  if Assigned(c) then
  begin
    i := c.IndexOf(LowerCase(s));
    if (i >= 0) and (i <= c.Count - 1) then
      Result := TTMSFNCGraphicsColorObject(c.Objects[i]).Color;
  end;
end;

class function TTMSFNCGraphics.HTMLToColor(AHTML: string): TTMSFNCGraphicsColor;
var
  r,g,b: Integer;
begin
  Result := gcNull;
  if AHTML = '' then
    Exit;

  if Length(AHTML) = 4 then
  begin
    r := StrToInt('$' + Copy(AHTML,2,1) + Copy(AHTML,2,1));
    g := StrToInt('$' + Copy(AHTML,3,1) + Copy(AHTML,3,1));
    b := StrToInt('$' + Copy(AHTML,4,1) + Copy(AHTML,4,1));
  end
  else
  begin
    r := StrToInt('$' + Copy(AHTML,2,2));
    g := StrToInt('$' + Copy(AHTML,4,2));
    b := StrToInt('$' + Copy(AHTML,6,2));
  end;

{$IFDEF CMNWEBLIB}
  Result := RGB(r, g, b);
{$ELSE}
  Result := MakeGraphicsColor(r, g, b);
{$ENDIF}
end;

class function TTMSFNCGraphics.ColorToHTML(AColor: TTMSFNCGraphicsColor): string;
const
  HTMLHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
var
  c: TTMSFNCGraphicsColor;
  i: Integer;
begin
  {$IFDEF ZEROSTRINGINDEX}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}

  {$IFDEF FMXLIB}
  c := AColor;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c := ColorToRGB(AColor);
  {$ENDIF}
  Result := HtmlHexColor;
  Result[1 + i] := HexDigit[GetColorRed(c) shr 4];
  Result[2 + i] := HexDigit[GetColorRed(c) and $F];
  Result[3 + i] := HexDigit[GetColorGreen(c) shr 4];
  Result[4 + i] := HexDigit[GetColorGreen(c) and $F];
  Result[5 + i] := HexDigit[GetColorBlue(c) shr 4];
  Result[6 + i] := HexDigit[GetColorBlue(c) and $F];
end;

procedure TTMSFNCGraphics.DrawTexture(ARect: TRectF; ATexture: TTMSFNCBitmap; ATextureMode: TTMSFNCGraphicsTextureMode);
var
  LR, R, IR: TRectF;
  I, J: Integer;
begin
  if IsBitmapEmpty(ATexture) then
    Exit;

  LR := ARect;
  case ATextureMode of
    gtmOriginal:
    begin
      R := RectF(ARect.Left, ARect.Top, ARect.Left + ATexture.Width, ARect.Top + ATexture.Height);
      IntersectRectEx(IR, LR, R);
      DrawBitmapPart(RectF(0, 0, (IR.Right - IR.Left), (IR.Bottom - IR.Top)), RectF(R.Left, R.Top, R.Left + (IR.Right - IR.Left), R.Top + (IR.Bottom - IR.Top)), ATexture, False, False, False, False);
    end;
    gtmFit:
    begin
      R := RectF(0, 0, ATexture.Width, ATexture.Height);
      R := RectSnapToPixelEx(RectFitIntoEx(R, ARect), 1, False);
      DrawBitmapPart(RectF(0, 0, ATexture.Width, ATexture.Height), R, ATexture, False, True, False, False);
    end;
    gtmStretch: DrawBitmapPart(RectF(0, 0, ATexture.Width, ATexture.Height), ARect, ATexture, False, True, False, False);
    gtmTile:
    begin
      for I := 0 to Trunc((LR.Right - LR.Left) / ATexture.Width) + 1 do
      begin
        for J := 0 to Trunc((LR.Bottom - LR.Top) / ATexture.Height) + 1 do
        begin
          R := RectF(LR.Left, LR.Top, LR.Left + ATexture.Width, LR.Top + ATexture.Height);
          OffsetRectEx(R, I * ATexture.Width, J * ATexture.Height);
          IntersectRectEx(IR, LR, R);
          if IntersectRectEx(IR, R) then
            DrawBitmap(RectF(R.Left, R.Top, (R.Left + (IR.Right - IR.Left)), (R.Top + (IR.Bottom - IR.Top))), ATexture, False, False, False, False);
        end;
      end;
    end;
    gtmCenter:
    begin
      R := RectF(0, 0, ATexture.Width, ATexture.Height);
      R := RectSnapToPixelEx(RectCenterAtEx(R, ARect), 1, False);
      DrawBitmapPart(RectF(0, 0, ATexture.Width, ATexture.Height), R, ATexture, True, False, True, False);
    end;
  end;
end;

function TTMSFNCGraphics.DrawText(ARect: TRect; AText: String; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  AControlID, AControlValue, AControlType: string;
begin
  AControlID := '';
  AControlValue := '';
  AControlType := '';
  Result := DrawText(ConvertToRectF(ARect), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ARect: TRect; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False; AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
begin
  Result := DrawText(ConvertToRectF(ARect), AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(APoint: TPointF; AText: String; AAngle: Single = 0{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  AControlID, AControlValue, AControlType: string;
begin
  AControlID := '';
  AControlValue := '';
  AControlType := '';

  Result := DrawText(RectF(APoint.X, APoint.Y, APoint.X + 10000, APoint.Y + 10000), AText, AControlID, AControlValue, AControlType, False, gtaLeading, gtaLeading, gttNone, AAngle, -1, -1{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ARect: TRectF; AText: String; AWordWrapping: Boolean = False;AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  AControlID, AControlValue, AControlType: string;
begin
  AControlID := '';
  AControlValue := '';
  AControlType := '';
  Result := DrawText(ARect, AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
end;

function TTMSFNCGraphics.DrawText(ARect: TRectF; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False;AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
  AVerticalAlign: TTMSFNCGraphicsTextAlign = gtaCenter; ATrimming: TTMSFNCGraphicsTextTrimming = gttNone; AAngle: Single = 0;
  AMinWidth: Single = -1; AMinHeight: Single = -1{$IFNDEF LIMITEDGRAPHICSMODE};ASupportHTML: Boolean = True; ATestAnchor: Boolean = False; AX: Single = -1; AY: Single = - 1{$ENDIF}): String;
var
  lst: TStringList;
  i, l, p: Integer;
  b: Boolean;
  r, rd: TRectF;
const
  arr: array[0..1] of string = ('<BR>', '<BR/>');
begin
  rd := ARect;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  b := (not ATestAnchor) and OptimizedHTMLDrawing;
  {$ELSE}
  b := false;
  {$ENDIF}

  b := b and TTMSFNCUtils.IsHTML(AText);

  if b then
  begin
    for I := 0 to Length(arr) - 1 do
    begin
      p := Pos(arr[I], UpperCase(AText));
      l := Length(arr[I]);
    end;

    if (p > 0) and (l > 0) then
    begin
      lst := TStringList.Create;
      try
        lst.LineBreak := Copy(AText, p, l);
        lst.Text := AText;

        for I := 0 to lst.Count - 1 do
        begin
          r := CalculateText(lst[I], rd, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
          InternalDrawText(RectF(r.Left, r.Top, r.Right + 1, r.Bottom), lst[I], AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign,
            ATrimming, AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});

          rd.Top := rd.Top + (r.Bottom - r.Top);
          rd.Bottom := rd.Bottom + (r.Bottom - r.Top);
        end;

      finally
        lst.Free;
      end;
    end
    else
      b := False;
  end;

  if not b then
  begin
    Result := InternalDrawText(ARect, AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign,
      ATrimming, AAngle, AMinWidth, AMinHeight{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML, ATestAnchor, AX, AY{$ENDIF});
  end;
end;

function TTMSFNCGraphics.InternalDrawText(ARect: TRectF; AText: String; var AControlID: string; var AControlValue: string; var AControlType: string; AWordWrapping: Boolean = False;AHorizontalAlign: TTMSFNCGraphicsTextAlign = gtaLeading;
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
  oldc: TTMSFNCGraphicsColor;
begin
  inherited;

  oldc := Context.GetFillColor;
  Context.SetFillColor(FFont.Color);

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

  ARect := Context.SetTextAngle(ARect, AAngle);

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
        gcNull,gcNull,gcNull,a,s,fa,XSize,YSize,hl,ml,hr, cr, AControlID, AControlValue, AControlType, lc, 0, FBitmapContainer, 1, URLUnderline{$IFDEF CMNLIB}, FImageList{$ENDIF},
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
        gcNull,gcNull,gcNull,a,s,fa,XSize,YSize,hl,ml,hr,cr, AControlID, AControlValue, AControlType, lc, 0, FBitmapContainer, 1, URLUnderline{$IFDEF CMNLIB}, FImageList{$ENDIF},
        HighlightColor, HighlightTextColor, HighlightFontStyle);
    RestoreState(st, True);

    if isanchor then
      Result := a;
  end
  else if not ATestAnchor then
  {$ENDIF}
    Context.DrawText(AText, ARect, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle);

  Context.ResetTextAngle(AAngle);
  Context.SetFillColor(oldc);
end;

procedure TTMSFNCGraphics.DrawEllipse(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawEllipse(RectF(ALeft, ATop, ARight, ABottom), AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawEllipse(ALeft: Double; ATop: Double; ARight: Double; ABottom: Double; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
var
  r: TRectF;
  pth: TTMSFNCGraphicsPath;
begin
  r := RectF(ALeft, ATop, ARight, ABottom);
  if ((FFill.Color <> gcNull) and (FFill.Kind <> gfkNone)) or (FFill.Kind = gfkTexture) then
  begin
    case FFill.Kind of
      gfkTexture:
      begin
        pth := TTMSFNCGraphicsPath.Create;
        try
          pth.AddEllipse(r);
          DrawTexture(r, FFill.Texture, FFill.TextureMode);
        finally
          pth.Free;
        end;
      end;
      else
        Context.FillEllipse(FFill, r, AModifyRectMode);
    end;
  end;

  if (FStroke.Color <> gcNull) and (FStroke.Kind <> gskNone) then
    Context.DrawEllipse(FStroke, r, AModifyRectMode);
end;

class procedure TTMSFNCGraphics.SetDefaultGraphicColors;
begin
  TTMSFNCGraphics.DefaultPopupFillColor := gcWhite;
  TTMSFNCGraphics.DefaultPopupStrokeColor := gcSilver;
  TTMSFNCGraphics.DefaultButtonFillColor := MakeGraphicsColor(225, 225, 225);
  TTMSFNCGraphics.DefaultButtonStrokeColor := gcDarkgray;
  TTMSFNCGraphics.DefaultTextFontColor := gcBlack;
  TTMSFNCGraphics.DefaultSelectionFillColor := gcBlack;
  TTMSFNCGraphics.DefaultButtonFillColorFocused := MakeGraphicsColor(229, 241, 251);
  TTMSFNCGraphics.DefaultButtonStrokeColorFocused := MakeGraphicsColor(60, 127, 177);
  TTMSFNCGraphics.DefaultButtonStrokeColorDisabled := gcDarkGray;
  TTMSFNCGraphics.DefaultButtonFillColorDisabled := gcLightgray;
end;

{$HINTS OFF}
class procedure TTMSFNCGraphics.ConvertBitmapToGrayScale(ABitmap: TTMSFNCBitmap);
type
  TTMSFNCGraphicsColorToGrayscale = (ctgLightness, ctgAverage, ctgLuminosity);

  function MinColor(const A, B, C: Integer): Integer;
  begin
    Result := Min(A, Min(B, C));
  end;

  function MaxColor(const A, B, C: Integer): Integer;
  begin
    Result := Max(A, Max(B, C));
  end;

  function ColorToGray(AColor: TTMSFNCGraphicsColor; AMode: TTMSFNCGraphicsColorToGrayscale = ctgLuminosity): TTMSFNCGraphicsColor;
  var
    R, G, B, X: Byte;
    {$IFNDEF CMNLIB}
    A: Byte;
    {$ENDIF}
  begin
    {$IFDEF FMXLIB}
    R := TAlphaColorRec(AColor).R;
    G := TAlphaColorRec(AColor).G;
    B := TAlphaColorRec(AColor).B;
    A := TAlphaColorRec(AColor).A;
    {$ENDIF}
    {$IFDEF CMNLIB}
    R := AColor and $FF;
    G := (AColor and $FF00) shr 8;
    B := (AColor and $FF0000) shr 16;
    {$ENDIF}
    case AMode of
      ctgLightness: X := (MaxColor(R, G, B) + MinColor(R, G, B)) div 2;
      ctgAverage: X := (R + G + B) div 3;
      ctgLuminosity: X := Round(0.21 *R + 0.71*G + 0.07*B);
      else
        X := 0;
    end;
    {$IFDEF FMXLIB}
    Result := MakeGraphicsColor(X, X, X, A);
    {$ENDIF}
    {$IFDEF CMNLIB}
    Result := TTMSFNCGraphicsColor(RGB(X, X, X));
    {$ENDIF}
  end;

var
  I, J: Integer;
  {$IFDEF FMXLIB}
  m: TBitmapData;
  {$ENDIF}
begin
  if Assigned(ABitmap) and not IsBitmapEmpty(ABitmap) then
  begin
    {$IFDEF FMXLIB}
    if ABitmap.Map(TMapAccess.ReadWrite, m) then
    begin
    {$ENDIF}
    {$IFDEF CMNLIB}
    begin
    {$ENDIF}
    {$IFDEF WEBLIB}
    begin
    {$ENDIF}
      for I := 0 to ABitmap.Width - 1 do
      begin
        for J := 0 to ABitmap.Height - 1 do
        begin
          {$IFDEF CMNLIB}
          ABitmap.Bitmap.Canvas.Pixels[I, J] := ColorToGray(ABitmap.Bitmap.Canvas.Pixels[I, J]);
          {$ENDIF}
          {$IFDEF FMXLIB}
          m.SetPixel(I, J, ColorToGray(m.GetPixel(I, J)));
          {$ENDIF}
        end;
      end;
      {$IFDEF FMXLIB}
      ABitmap.Unmap(m);
      {$ENDIF}
    end;
  end;
end;
{$HINTS ON}

class procedure TTMSFNCGraphics.DrawSample(ACanvas: TCanvas; ARect: TRectF);
var
  g: TTMSFNCGraphics;
begin
  g := TTMSFNCGraphics.Create(ACanvas);
  try
    g.Font.Name := 'Courier New';
    g.Font.Size := 20;
    g.Font.Color := gcWhite;
    g.Fill.Kind := gfkGradient;
    g.Fill.Color := gcDarkorange;
    g.Fill.ColorTo := gcSteelblue;
    g.Stroke.Color := gcDarkblue;
    g.DrawRectangle(ARect);
    g.DrawText(ARect, 'Sample', False, gtaCenter);
  finally
    g.Free;
  end;
end;

class function TTMSFNCGraphics.PointInPolygon(APoint: TPoint; APolygon: TTMSFNCGraphicsPathPolygon): Boolean;
begin
  Result := PointInPolygon(ConvertToPointF(APoint), APolygon);
end;

class function TTMSFNCGraphics.PointInPolygon(APoint: TPointF; APolygon: TTMSFNCGraphicsPathPolygon): Boolean;
begin
  Result := PointInPolygon(APoint, APolygon);
end;

class function TTMSFNCGraphics.PointInRect(APoint: TPoint; ARect: TRect): Boolean;
begin
  Result := PointInRect(ConvertToPointF(APoint), ConvertToRectF(ARect));
end;

class function TTMSFNCGraphics.PointInRect(APoint: TPointF; ARect: TRectF): Boolean;
begin
  Result := PtInRectEx(ARect, APoint);
end;

class function TTMSFNCGraphics.PointInCircle(APoint, ACenter: TPointF; const ARadius: Single): Boolean;
begin
  if ARadius > 0 then
    Result := Sqr((APoint.X - ACenter.X) / ARadius) + Sqr((APoint.Y - ACenter.Y) / ARadius) <= 1
  else
    Result := False;
end;

class function TTMSFNCGraphics.PointInPath(APoint: TPoint; APath: TTMSFNCGraphicsPath): Boolean;
begin
  Result := PointInPath(ConvertToPointF(APoint), APath);
end;

class function TTMSFNCGraphics.PointInPath(APoint: TPointF; APath: TTMSFNCGraphicsPath): Boolean;
begin
  Result := APath.IsPointVisible(APoint);
end;

function TTMSFNCGraphics.CalculateTextSizeInt(AText: String): TSize;
begin
  Result := ConvertToSize(CalculateTextSize(AText));
end;

function TTMSFNCGraphics.CalculateTextSize(AText: String): TSizeF;
begin
  Result := CalculateTextSize(AText, RectF(0, 0, 10000, 10000));
end;

function TTMSFNCGraphics.CalculateTextHeightInt(AText: String): Integer;
begin
  Result := Round(CalculateTextHeight(AText));
end;

function TTMSFNCGraphics.CalculateTextHeight(AText: String): Single;
begin
  Result := CalculateTextHeight(AText, RectF(0, 0, 10000, 10000));
end;

function TTMSFNCGraphics.CalculateTextWidthInt(AText: String): Integer;
begin
  Result := Round(CalculateTextWidth(AText));
end;

function TTMSFNCGraphics.CalculateTextWidth(AText: String): Single;
begin
  Result := CalculateTextWidth(AText, RectF(0, 0, 10000, 10000));
end;

function TTMSFNCGraphics.CalculateTextSize(AText: String; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TSize;
begin
  Result := ConvertToSize(CalculateTextSize(AText, ConvertToRectF(ARect), AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF}));
end;

function TTMSFNCGraphics.CalculateTextSize(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TSizeF;
var
  r: TRectF;
begin
  r := CalculateText(AText, ARect, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
  Result.cx := r.Right - r.Left;
  Result.cy := r.Bottom - r.Top;
end;

function TTMSFNCGraphics.CalculateTextHeight(AText: String; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Integer;
begin
  Result := Round(CalculateTextHeight(AText, ConvertToRectF(ARect), AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF}));
end;

function TTMSFNCGraphics.CalculateTextHeight(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Single;
var
  r: TRectF;
begin
  r := CalculateText(AText, ARect, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
  Result := r.Bottom - r.Top;
end;

function TTMSFNCGraphics.CalculateTextWidth(AText: String; ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Integer;
begin
  Result := Round(CalculateTextWidth(AText, ConvertToRectF(ARect), AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF}));
end;

function TTMSFNCGraphics.CalculateTextWidth(AText: String; ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): Single;
var
  r: TRectF;
begin
  r := CalculateText(AText, ARect, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
  Result := r.Right - r.Left;
end;

function TTMSFNCGraphics.CalculateTextInt(AText: String): TRect;
begin
  Result := ConvertToRect(CalculateText(AText));
end;

function TTMSFNCGraphics.CalculateText(AText: String): TRectF;
begin
  Result := CalculateText(AText, RectF(0, 0, 10000, 10000));
end;

function TTMSFNCGraphics.CalculateText(AText: String;
  ARect: TRect; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRect;
begin
  Result := ConvertToRect(CalculateText(AText, ConvertToRectF(ARect), AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF}));
end;

function TTMSFNCGraphics.CalculateText(AText: String;
  ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF;
var
  lst: TStringList;
  i, l, p: Integer;
  b: Boolean;
  r: TRectF;
  y: Single;
const
  arr: array[0..1] of string = ('<BR>', '<BR/>');
begin
  Result := ARect;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  b := OptimizedHTMLDrawing and TTMSFNCUtils.IsHTML(AText);
  {$ELSE}
  b := False;
  {$ENDIF}

  if b then
  begin
    for I := 0 to Length(arr) - 1 do
    begin
      p := Pos(arr[I], UpperCase(AText));
      l := Length(arr[I]);
    end;

    if (p > 0) and (l > 0) then
    begin
      lst := TStringList.Create;
      try
        lst.LineBreak := Copy(AText, p, l);
        lst.Text := AText;

        y := 0;
        for I := 0 to lst.Count - 1 do
        begin
          r := InternalCalculateText(lst[I], ARect, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
          y := y + (r.Bottom - r.Top);
        end;

        Result.Bottom := Result.Top + y;

      finally
        lst.Free;
      end;
    end
    else
      b := False;
  end;

  if not b then
    Result := InternalCalculateText(AText, ARect, AWordWrapping{$IFNDEF LIMITEDGRAPHICSMODE}, ASupportHTML{$ENDIF});
end;

function TTMSFNCGraphics.InternalCalculateText(AText: String;
  ARect: TRectF; AWordWrapping: Boolean = False{$IFNDEF LIMITEDGRAPHICSMODE}; ASupportHTML: Boolean = True{$ENDIF}): TRectF;
{$IFNDEF LIMITEDGRAPHICSMODE}
var
  a, s: String;
  fa: String;
  XSize, YSize: Single;
  hl, ml: Integer;
  hr, cr: TRectF;
  lc: Integer;
  AControlID, AControlType, AControlValue: string;
{$ENDIF}
begin
  Result := ARect;

  if Round(Result.Right - Result.Left) <= 0 then
  begin
    Result.Bottom := Result.Top;
    Exit;
  end;

  if AText <> '' then
  begin
    {$IFNDEF LIMITEDGRAPHICSMODE}
    if ASupportHTML and ((Pos('</', AText) > 0) or (Pos('/>', AText)  > 0) or (Pos('<BR>', UpperCase(AText)) > 0)) then
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
      HTMLDrawEx(Self, AText, ARect, 0, 0,-1,-1,0,False,True,False,False,False,False,AWordWrapping,False, '', 1.0,URLColor,
        gcNull,gcNull,gcNull,a,s,fa,XSize,YSize,hl,ml,hr,cr, AControlID, AControlValue, AControlType, lc, 0, FBitmapContainer, 1, URLUnderline{$IFDEF CMNLIB}, FImageList{$ENDIF},
        HighlightColor, HighlightTextColor, HighlightFontStyle);

      YSize := YSize + 1;

      Result.Right := Result.Left + XSize;
      Result.Bottom := Result.Top + YSize;
    end
    else
    {$ENDIF}
    begin
      Result := Context.CalculateText(AText, ARect, AWordWrapping);
      Result.Bottom := Result.Bottom + 1;
    end;
  end
  else
  begin
    Result.Right := Result.Left;
    Result.Bottom := Result.Top;
  end;
end;

procedure TTMSFNCGraphics.DrawEllipse(ARect: TRect; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawEllipse(ConvertToRectF(ARect), AModifyRectMode);
end;

procedure TTMSFNCGraphics.DrawEllipse(ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll);
begin
  DrawEllipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AModifyRectMode);
end;

{ TTMSFNCGraphicsContext }

function TTMSFNCGraphicsContext.ConvertToPath(APath: TTMSFNCGraphicsPath; const Flatness: Single = 0.25): Pointer;
var
  J, I: Integer;
  BPts: TTMSFNCGraphicsPathPolygon;
  B: TTMSFNCGraphicsPathCubicBezier;
  F, Len: Single;
  SegCount: Integer;
  CurPoint: TPointF;
  x: TPointF;
begin
  Result := CreatePath;
  if APath.Count > 0 then
  begin
    F := Max(Flatness, 0.05);
    J := 0;
    while J < APath.Count do
    begin
      case APath[J].Kind of
        gppMoveTo:
          begin
            PathOpen(Result);
            PathMoveTo(Result, APath[J].Point);
            CurPoint := APath[J].Point;
          end;
        gppLineTo:
          begin
            PathLineTo(Result, APath[J].Point);
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
              PathLineTo(Result, B[3])
            else
            begin
              BPts := APath.CreateBezier(B, SegCount);
              for I := 0 to High(BPts) do
                PathLineTo(Result, BPts[I]);
              CurPoint := APath[J].Point;
            end;
          end;
        gppClose: PathClose(Result);
      end;
      Inc(J);
    end;
  end;
end;

constructor TTMSFNCGraphicsContext.Create(const AGraphics: TTMSFNCGraphics);
begin
  FGraphics := AGraphics;
end;

function TTMSFNCGraphicsContext.GetGraphics: TTMSFNCGraphics;
begin
  Result := FGraphics;
end;

function TTMSFNCGraphicsContext.GetCanvas: TCanvas;
begin
  Result := nil;
  if Assigned(Graphics) then
    Result := Graphics.Canvas;
end;

{$IFDEF WEBLIB}
initialization
begin
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSCLOSE);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSDOWN);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSRIGHT);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSLEFT);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSEXPAND);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSPIN);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSPIN2);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSDOWN);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSDOWN2);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSUP);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSUP2);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSEXPANDSVG);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSUPSVG);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSRIGHTSVG);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSLEFTSVG);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSDOWNSVG);
  TTMSFNCBitmap.CreateFromResource(TMSFNCGRAPHICSCLOSESVG);
end;
{$ENDIF}

end.
