{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit VCL.TMSFNCScrollBar;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.TMSFNCCustomControl, VCL.ExtCtrls,
  VCL.TMSFNCTypes, VCL.TMSFNCGraphics, VCL.Controls,
  VCL.TMSFNCGraphicsTypes
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Types
  {$ENDIF}
  ;

type
  TTMSFNCCustomScrollBar = class;

  TTMSFNCScrollBarAppearance = class(TPersistent)
  private
    FOwner: TTMSFNCCustomScrollBar;
    FScrollButtonSize: Single;
    FThumbButtonSize: Single;
    FOnChange: TNotifyEvent;
    FFixedThumb: Boolean;
    FArrowColor: TTMSFNCGraphicsColor;
    FThumbStroke: TTMSFNCGraphicsStroke;
    FThumbButtonRightDownStroke: TTMSFNCGraphicsStroke;
    FThumbFill: TTMSFNCGraphicsFill;
    FThumbButtonLeftFill: TTMSFNCGraphicsFill;
    FScrollButtonLeftHoverFill: TTMSFNCGraphicsFill;
    FScrollButtonLeftStroke: TTMSFNCGraphicsStroke;
    FScrollButtonRightFill: TTMSFNCGraphicsFill;
    FThumbButtonLeftHoverFill: TTMSFNCGraphicsFill;
    FThumbButtonLeftStroke: TTMSFNCGraphicsStroke;
    FScrollButtonLeftHoverStroke: TTMSFNCGraphicsStroke;
    FThumbButtonRightFill: TTMSFNCGraphicsFill;
    FScrollButtonRightHoverFill: TTMSFNCGraphicsFill;
    FScrollButtonRightStroke: TTMSFNCGraphicsStroke;
    FThumbButtonLeftHoverStroke: TTMSFNCGraphicsStroke;
    FScrollButtonLeftDownFill: TTMSFNCGraphicsFill;
    FThumbButtonRightHoverFill: TTMSFNCGraphicsFill;
    FThumbButtonRightStroke: TTMSFNCGraphicsStroke;
    FScrollButtonRightHoverStroke: TTMSFNCGraphicsStroke;
    FThumbButtonLeftDownFill: TTMSFNCGraphicsFill;
    FScrollButtonLeftDownStroke: TTMSFNCGraphicsStroke;
    FThumbButtonRightHoverStroke: TTMSFNCGraphicsStroke;
    FScrollButtonRightDownFill: TTMSFNCGraphicsFill;
    FThumbButtonLeftDownStroke: TTMSFNCGraphicsStroke;
    FThumbButtonRightDownFill: TTMSFNCGraphicsFill;
    FScrollButtonRightDownStroke: TTMSFNCGraphicsStroke;
    FScrollButtonLeftFill: TTMSFNCGraphicsFill;
    procedure SetThumbButtonSize(const Value: Single);
    procedure SetFixedThumb(const Value: Boolean);
    procedure SetArrowColor(const Value: TTMSFNCGraphicsColor);
    procedure SetScrollButtonLeftDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonLeftDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonLeftFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonLeftHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonLeftHoverStroke(
      const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonLeftStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonRightDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonRightDownStroke(
      const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonRightFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonRightHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetScrollButtonRightHoverStroke(
      const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonRightStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetScrollButtonSize(const Value: Single);
    procedure SetThumbButtonLeftDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonLeftDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbButtonLeftFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonLeftHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonLeftHoverStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbButtonLeftStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbButtonRightDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonRightDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbButtonRightFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonRightHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbButtonRightHoverStroke(
      const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbButtonRightStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetThumbFill(const Value: TTMSFNCGraphicsFill);
    procedure SetThumbStroke(const Value: TTMSFNCGraphicsStroke);
    function IsScrollButtonSizeStored: Boolean;
    function IsThumbButtonSizeStored: Boolean;
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
  public
    constructor Create(AOwner: TTMSFNCCustomScrollBar);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetThumbButtonSize: Single;
  published
    property ThumbFill: TTMSFNCGraphicsFill read FThumbFill write SetThumbFill;
    property ScrollButtonLeftFill: TTMSFNCGraphicsFill read FScrollButtonLeftFill write SetScrollButtonLeftFill;
    property ScrollButtonRightFill: TTMSFNCGraphicsFill read FScrollButtonRightFill write SetScrollButtonRightFill;
    property ScrollButtonLeftHoverFill: TTMSFNCGraphicsFill read FScrollButtonLeftHoverFill write SetScrollButtonLeftHoverFill;
    property ScrollButtonRightHoverFill: TTMSFNCGraphicsFill read FScrollButtonRightHoverFill write SetScrollButtonRightHoverFill;
    property ScrollButtonLeftDownFill: TTMSFNCGraphicsFill read FScrollButtonLeftDownFill write SetScrollButtonLeftDownFill;
    property ScrollButtonRightDownFill: TTMSFNCGraphicsFill read FScrollButtonRightDownFill write SetScrollButtonRightDownFill;
    property ThumbButtonLeftFill: TTMSFNCGraphicsFill read FThumbButtonLeftFill write SetThumbButtonLeftFill;
    property ThumbButtonRightFill: TTMSFNCGraphicsFill read FThumbButtonRightFill write SetThumbButtonRightFill;
    property ThumbButtonLeftHoverFill: TTMSFNCGraphicsFill read FThumbButtonLeftHoverFill write SetThumbButtonLeftHoverFill;
    property ThumbButtonRightHoverFill: TTMSFNCGraphicsFill read FThumbButtonRightHoverFill write SetThumbButtonRightHoverFill;
    property ThumbButtonLeftDownFill: TTMSFNCGraphicsFill read FThumbButtonLeftDownFill write SetThumbButtonLeftDownFill;
    property ThumbButtonRightDownFill: TTMSFNCGraphicsFill read FThumbButtonRightDownFill write SetThumbButtonRightDownFill;
    property ThumbStroke: TTMSFNCGraphicsStroke read FThumbStroke write SetThumbStroke;
    property ScrollButtonLeftStroke: TTMSFNCGraphicsStroke read FScrollButtonLeftStroke write SetScrollButtonLeftStroke;
    property ScrollButtonRightStroke: TTMSFNCGraphicsStroke read FScrollButtonRightStroke write SetScrollButtonRightStroke;
    property ScrollButtonLeftHoverStroke: TTMSFNCGraphicsStroke read FScrollButtonLeftHoverStroke write SetScrollButtonLeftHoverStroke;
    property ScrollButtonRightHoverStroke: TTMSFNCGraphicsStroke read FScrollButtonRightHoverStroke write SetScrollButtonRightHoverStroke;
    property ScrollButtonLeftDownStroke: TTMSFNCGraphicsStroke read FScrollButtonLeftDownStroke write SetScrollButtonLeftDownStroke;
    property ScrollButtonRightDownStroke: TTMSFNCGraphicsStroke read FScrollButtonRightDownStroke write SetScrollButtonRightDownStroke;
    property ThumbButtonLeftStroke: TTMSFNCGraphicsStroke read FThumbButtonLeftStroke write SetThumbButtonLeftStroke;
    property ThumbButtonRightStroke: TTMSFNCGraphicsStroke read FThumbButtonRightStroke write SetThumbButtonRightStroke;
    property ThumbButtonLeftHoverStroke: TTMSFNCGraphicsStroke read FThumbButtonLeftHoverStroke write SetThumbButtonLeftHoverStroke;
    property ThumbButtonRightHoverStroke: TTMSFNCGraphicsStroke read FThumbButtonRightHoverStroke write SetThumbButtonRightHoverStroke;
    property ThumbButtonLeftDownStroke: TTMSFNCGraphicsStroke read FThumbButtonLeftDownStroke write SetThumbButtonLeftDownStroke;
    property ThumbButtonRightDownStroke: TTMSFNCGraphicsStroke read FThumbButtonRightDownStroke write SetThumbButtonRightDownStroke;
    property ThumbButtonSize: Single read FThumbButtonSize write SetThumbButtonSize stored IsThumbButtonSizeStored nodefault;
    property ScrollButtonSize: Single read FScrollButtonSize write SetScrollButtonSize stored IsScrollButtonSizeStored nodefault;
    property FixedThumb: Boolean read FFixedThumb write SetFixedThumb default False;
    property ArrowColor: TTMSFNCGraphicsColor read FArrowColor write SetArrowColor default gcBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTMSFNCScrollBarMode = (sbmNone, sbmScroll, sbmChangePageSize);

  TTMSFNCScrollBarHoveredButton = (shbNone, shbScrollMin, shbScrollMax, shbThumbMin, shbThumbMax);

  TTMSFNCScrollBarDownButton = (sdbNone, sdbScrollMin, sdbScrollMax, sdbThumbMin, sdbThumbMax);

  TTMSFNCScrollButtonChange = (sbcNone, sbcSmallSubstract, sbcSmallAdd, sbcLargeSubstract, sbcLargeAdd);

  TTMSFNCScrollBarValueChanged = procedure(Sender: TObject; Value: Double) of object;

  TTMSFNCScrollBarPageSizeChanged = procedure(Sender: TObject; PageSize: Double) of object;

  TTMSFNCScrollBarKind = (sbkHorizontal, sbkVertical);

  TTMSFNCCustomScrollBar = class(TTMSFNCCustomControl)
  private
    FNeedsChange: Boolean;
    FSavedPageSize: Double;
    FDesignTime, FMinThumb, FMaxThumb: Boolean;
    FMx, FMy, FCx, FCy: Double;
    FTimer: TTimer;
    FTime: integer;
    FScrollBarMode: TTMSFNCScrollBarMode;
    FScrollButtonChange: TTMSFNCScrollButtonChange;
    FHoveredButton: TTMSFNCScrollBarHoveredButton;
    FDownButton: TTMSFNCScrollBarDownButton;
    FKind: TTMSFNCScrollBarKind;
    FValue, FTempValue: Double;
    FMin: Double;
    FMax: Double;
    FPageSize: Double;
    FSmallChange: Double;
    FLargeChange: Double;
    FAppearance: TTMSFNCScrollBarAppearance;
    FOnValueChange: TTMSFNCScrollBarValueChanged;
    FOnPageSizeChanged: TTMSFNCScrollBarPageSizeChanged;
    FTracking: Boolean;
    procedure SetAppearance(const Value: TTMSFNCScrollBarAppearance);
    procedure SetKind(Value: TTMSFNCScrollBarKind);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure SetPageSize(const Value: Double);
    function IsLargeChangeStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsPageSizeStored: Boolean;
    function IsSmallChangeStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetTracking(const Value: Boolean);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
    procedure TimerChanged(Sender: TObject);
    function GetDocURL: string; override;
    function GetMinScrollButton: TRectF;
    function GetMaxScrollButton: TRectF;
    function GetMinThumbButton: TRectF;
    function GetMaxThumbButton: TRectF;
    function GetScrollRectangle: TRectF;
    function GetScrollSize: Double;
    function GetScrollAreaMin: TRectF;
    function GetScrollAreaMax: TRectF;
    function GetThumbRectangle: TRectF;
    function GetValue: Double; overload;
    function GetValue(XYPos: Double): Double; overload;
    function GetRange: Double;
    function MouseOnThumbButtons(X, Y: Double): Boolean;
    function MouseOnThumb(X, Y: Double): Boolean;
    procedure DrawScrollButtons(g: TTMSFNCGraphics);
    procedure DrawThumb(g: TTMSFNCGraphics);
    procedure DrawThumbButtons(g: TTMSFNCGraphics);
    procedure DrawArrow(g: TTMSFNCGraphics; r: TRectF; ALeft: Boolean);
    procedure Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF); override;
    property Appearance: TTMSFNCScrollBarAppearance read FAppearance write SetAppearance;
    property Kind: TTMSFNCScrollBarKind read FKind write SetKind default sbkVertical;
    property LargeChange: Double read FLargeChange write FLargeChange stored IsLargeChangeStored nodefault;
    property Max: Double read FMax write SetMax stored IsMaxStored nodefault;
    property Min: Double read FMin write SetMin stored IsMinStored nodefault;
    property PageSize: Double read FPageSize write SetPageSize stored IsPageSizeStored nodefault;
    property Value: Double read FValue write SetValue stored IsValueStored nodefault;
    property SmallChange: Double read FSmallChange write FSmallChange stored IsSmallChangeStored nodefault;
    property Tracking: Boolean read FTracking write SetTracking default True;
    property OnValueChanged: TTMSFNCScrollBarValueChanged read FOnValueChange write FOnValueChange;
    property OnPageSizeChanged: TTMSFNCScrollBarPageSizeChanged read FOnPageSizeChanged write FOnPageSizeChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCScrollBar = class(TTMSFNCCustomScrollBar)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property Fill;
    property Stroke;
    property Appearance;
    property Kind;
    property LargeChange;
    property Max;
    property Tracking;
    property Min;
    property PageSize;
    property Value;
    property SmallChange;
    property OnValueChanged;
    property OnPageSizeChanged;
  end;

implementation

uses
  Math, VCL.TMSFNCStyles, VCL.TMSFNCUtils;

{ TTMSFNCScrollBarAppearance }

procedure TTMSFNCScrollBarAppearance.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCScrollBarAppearance) then
  begin
    FThumbFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbFill);
    FScrollButtonSize := (Source as TTMSFNCScrollBarAppearance).ScrollButtonSize;
    FThumbButtonSize := (source as TTMSFNCScrollBarAppearance).ThumbButtonSize;
    FThumbButtonLeftFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftFill);
    FThumbButtonLeftHoverFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftHoverFill);
    FThumbButtonLeftDownFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftDownFill);
    FThumbButtonRightFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightFill);
    FThumbButtonRightDownFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightDownFill);
    FThumbButtonRightHoverFill.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightHoverFill);
    FScrollButtonLeftFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftFill);
    FScrollButtonLeftHoverFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftHoverFill);
    FScrollButtonLeftDownFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftDownFill);
    FScrollButtonRightFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightFill);
    FScrollButtonRightDownFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightDownFill);
    FScrollButtonRightHoverFill.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightHoverFill);
    FThumbButtonLeftStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftStroke);
    FThumbButtonLeftHoverStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftHoverStroke);
    FThumbButtonLeftDownStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonLeftDownStroke);
    FThumbButtonRightStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightStroke);
    FThumbButtonRightDownStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightDownStroke);
    FThumbButtonRightHoverStroke.Assign((Source as TTMSFNCScrollBarAppearance).ThumbButtonRightHoverStroke);
    FScrollButtonLeftStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftStroke);
    FScrollButtonLeftHoverStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftHoverStroke);
    FScrollButtonLeftDownStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonLeftDownStroke);
    FScrollButtonRightStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightStroke);
    FScrollButtonRightDownStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightDownStroke);
    FScrollButtonRightHoverStroke.Assign((Source as TTMSFNCScrollBarAppearance).ScrollButtonRightHoverStroke);
    Changed;
  end;
end;

procedure TTMSFNCScrollBarAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TTMSFNCScrollBarAppearance.Create(AOwner: TTMSFNCCustomScrollBar);
begin
  FOwner := AOwner;
  FThumbFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbFill.OnChanged := FillChanged;
  FThumbButtonLeftFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbButtonLeftFill.OnChanged := FillChanged;
  FThumbButtonRightFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbButtonRightFill.OnChanged := FillChanged;
  FThumbButtonLeftHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FThumbButtonLeftHoverFill.OnChanged := FillChanged;
  FThumbButtonRightHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FThumbButtonRightHoverFill.OnChanged := FillChanged;
  FThumbButtonLeftDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FThumbButtonLeftDownFill.OnChanged := FillChanged;
  FThumbButtonRightDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FThumbButtonRightDownFill.OnChanged := FillChanged;

  FScrollButtonLeftFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FScrollButtonLeftFill.OnChanged := FillChanged;
  FScrollButtonRightFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FScrollButtonRightFill.OnChanged := FillChanged;
  FScrollButtonLeftHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FScrollButtonLeftHoverFill.OnChanged := FillChanged;
  FScrollButtonRightHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FScrollButtonRightHoverFill.OnChanged := FillChanged;
  FScrollButtonLeftDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FScrollButtonLeftDownFill.OnChanged := FillChanged;
  FScrollButtonRightDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FScrollButtonRightDownFill.OnChanged := FillChanged;

  FThumbStroke := TTMSFNCGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftStroke := TTMSFNCGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbButtonLeftStroke.OnChanged := StrokeChanged;
  FThumbButtonRightStroke := TTMSFNCGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbButtonRightStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonLeftHoverStroke.OnChanged := StrokeChanged;
  FThumbButtonRightHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonRightHoverStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonLeftDownStroke.OnChanged := StrokeChanged;
  FThumbButtonRightDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonRightDownStroke.OnChanged := StrokeChanged;

  FScrollButtonLeftStroke := TTMSFNCGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FScrollButtonLeftStroke.OnChanged := StrokeChanged;
  FScrollButtonRightStroke := TTMSFNCGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FScrollButtonRightStroke.OnChanged := StrokeChanged;
  FScrollButtonLeftHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonLeftHoverStroke.OnChanged := StrokeChanged;
  FScrollButtonRightHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonRightHoverStroke.OnChanged := StrokeChanged;
  FScrollButtonLeftDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonLeftDownStroke.OnChanged := StrokeChanged;
  FScrollButtonRightDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonRightDownStroke.OnChanged := StrokeChanged;

  FArrowColor := gcBlack;

  FFixedThumb := False;
  FThumbButtonSize := 18;
  FScrollButtonSize := 18;
end;

destructor TTMSFNCScrollBarAppearance.Destroy;
begin
  FThumbFill.Free;
  FScrollButtonLeftFill.Free;
  FScrollButtonRightFill.Free;
  FScrollButtonLeftHoverFill.Free;
  FScrollButtonRightHoverFill.Free;
  FScrollButtonLeftDownFill.Free;
  FScrollButtonRightDownFill.Free;
  FThumbButtonLeftFill.Free;
  FThumbButtonRightFill.Free;
  FThumbButtonLeftHoverFill.Free;
  FThumbButtonRightHoverFill.Free;
  FThumbButtonLeftDownFill.Free;
  FThumbButtonRightDownFill.Free;
  FThumbStroke.Free;
  FScrollButtonLeftStroke.Free;
  FScrollButtonRightStroke.Free;
  FScrollButtonLeftHoverStroke.Free;
  FScrollButtonRightHoverStroke.Free;
  FScrollButtonLeftDownStroke.Free;
  FScrollButtonRightDownStroke.Free;
  FThumbButtonLeftStroke.Free;
  FThumbButtonRightStroke.Free;
  FThumbButtonLeftHoverStroke.Free;
  FThumbButtonRightHoverStroke.Free;
  FThumbButtonLeftDownStroke.Free;
  FThumbButtonRightDownStroke.Free;
  inherited;
end;

procedure TTMSFNCScrollBarAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TTMSFNCScrollBarAppearance.GetThumbButtonSize: Single;
begin
  if FixedThumb then
    Result := 0
  else
    Result := ThumbButtonSize;
end;

function TTMSFNCScrollBarAppearance.IsScrollButtonSizeStored: Boolean;
begin
  Result := ScrollButtonSize <> 18;
end;

function TTMSFNCScrollBarAppearance.IsThumbButtonSizeStored: Boolean;
begin
  Result := ThumbButtonSize <> 18;
end;

procedure TTMSFNCScrollBarAppearance.SetArrowColor(const Value: TTMSFNCGraphicsColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TTMSFNCScrollBarAppearance.SetFixedThumb(const Value: Boolean);
begin
  if FFixedThumb <> value then
  begin
    FFixedThumb := Value;
    Changed;
  end;
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftDownFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonLeftDownFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftDownStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonLeftDownStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonLeftFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftHoverFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonLeftHoverFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftHoverStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonLeftHoverStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonLeftStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonLeftStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightDownFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonRightDownFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightDownStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonRightDownStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonRightFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightHoverFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FScrollButtonRightHoverFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightHoverStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonRightHoverStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonRightStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FScrollButtonRightStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetScrollButtonSize(const Value: Single);
begin
  if FScrollButtonSize <> Value then
  begin
    FScrollButtonSize := Value;
    Changed;
  end;
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftDownFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonLeftDownFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftDownStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonLeftDownStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonLeftFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftHoverFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonLeftHoverFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftHoverStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonLeftHoverStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonLeftStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonLeftStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightDownFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonRightDownFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightDownStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonRightDownStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonRightFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightHoverFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbButtonRightHoverFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightHoverStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonRightHoverStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonRightStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbButtonRightStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbButtonSize(
  const Value: Single);
begin
  if FThumbButtonSize <> value then
  begin
    FThumbButtonSize := Value;
    Changed;
  end;
end;

procedure TTMSFNCScrollBarAppearance.SetThumbFill(
  const Value: TTMSFNCGraphicsFill);
begin
  FThumbFill.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.SetThumbStroke(
  const Value: TTMSFNCGraphicsStroke);
begin
  FThumbStroke.Assign(Value);
end;

procedure TTMSFNCScrollBarAppearance.StrokeChanged(Sender: TObject);
begin
  Changed;
end;

procedure TTMSFNCCustomScrollBar.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TTMSFNCCustomScrollBar.ApplyStyle;
var
  c: TTMSFNCGraphicsColor;
begin
  inherited;

  c := gcNull;
  if TTMSFNCStyles.GetStyleBackgroundFillColor(c) then
  begin
    Appearance.ThumbFill.Color := c;
    Appearance.ThumbButtonLeftFill.Color := c;
    Appearance.ThumbButtonRightFill.Color := c;
    Appearance.ScrollButtonLeftFill.Color := c;
    Appearance.ScrollButtonRightFill.Color := c;
  end;

  c := gcNull;
  if TTMSFNCStyles.GetStyleLineFillColor(c) then
  begin
    Appearance.ThumbStroke.Color := c;
    Appearance.ThumbButtonLeftStroke.Color := c;
    Appearance.ThumbButtonRightStroke.Color := c;
    Appearance.ScrollButtonLeftStroke.Color := c;
    Appearance.ScrollButtonRightStroke.Color := c;
    Appearance.ThumbButtonLeftHoverStroke.Color := c;
    Appearance.ThumbButtonRightHoverStroke.Color := c;
    Appearance.ScrollButtonLeftHoverStroke.Color := c;
    Appearance.ScrollButtonRightHoverStroke.Color := c;
    Appearance.ThumbButtonLeftDownStroke.Color := c;
    Appearance.ThumbButtonRightDownStroke.Color := c;
    Appearance.ScrollButtonLeftDownStroke.Color := c;
    Appearance.ScrollButtonRightDownStroke.Color := c;
  end;

  c := gcNull;
  if TTMSFNCStyles.GetStyleSelectionFillColor(c) then
  begin
    Appearance.ThumbButtonLeftHoverFill.Color := Blend(Fill.Color, c, 70);
    Appearance.ThumbButtonRightHoverFill.Color := Blend(Fill.Color, c, 70);
    Appearance.ScrollButtonLeftHoverFill.Color := Blend(Fill.Color, c, 70);
    Appearance.ScrollButtonRightHoverFill.Color := Blend(Fill.Color, c, 70);
    Appearance.ThumbButtonLeftDownFill.Color := Blend(Fill.Color, c, 50);
    Appearance.ThumbButtonRightDownFill.Color := Blend(Fill.Color, c, 50);
    Appearance.ScrollButtonLeftDownFill.Color := Blend(Fill.Color, c, 50);
    Appearance.ScrollButtonRightDownFill.Color := Blend(Fill.Color, c, 50);
    Appearance.ArrowColor := c;
  end;
end;

procedure TTMSFNCCustomScrollBar.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCScrollBar) then
  begin
    FKind := (Source as TTMSFNCScrollBar).Kind;
    FValue := (Source as TTMSFNCScrollBar).Value;
    FMin := (Source as TTMSFNCScrollBar).Min;
    FMax := (Source as TTMSFNCScrollBar).Max;
    FPageSize := (Source as TTMSFNCScrollBar).PageSize;
    FSmallChange := (Source as TTMSFNCScrollBar).SmallChange;
    FLargeChange := (Source as TTMSFNCScrollBar).LargeChange;
    FAppearance.Assign((Source as TTMSFNCScrollBar).Appearance);
    FTracking := (Source as TTMSFNCScrollBar).Tracking;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.Changed;
begin
  Invalidate;
end;

procedure TTMSFNCCustomScrollBar.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  BeginUpdate;

  FAppearance.FScrollButtonSize := TTMSFNCUtils.MulDivSingle(FAppearance.ScrollButtonSize, M, D);
  FAppearance.FThumbButtonSize := TTMSFNCUtils.MulDivSingle(FAppearance.FThumbButtonSize, M, D);

  EndUpdate;
end;

procedure TTMSFNCCustomScrollBar.HandleMouseLeave;
begin
  inherited;
  FTimer.Enabled := false;
  FTime := 0;
  FScrollBarMode := sbmNone;
  FScrollButtonChange := sbcNone;
  FMinThumb := false;
  FMaxThumb := false;
  FDownButton := sdbNone;
  FHoveredButton := shbNone;
  Changed;
end;

constructor TTMSFNCCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited;

  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  NativeCanvas := True;
  {$ENDIF}
  {$ENDIF}

  FTracking := True;
  FDesignTime := IsDesignTime;
  if FDesignTime then
  begin
    Fill.Color := Lighter(MakeGraphicsColor(225, 225, 225), 40);
    Stroke.Color := Darker(gcDarkGray, 40);
  end;
  Height := 121;
  Width := 18;
  FKind := sbkVertical;
  FValue := 0;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 20;
  FAppearance := TTMSFNCScrollBarAppearance.Create(Self);
  FAppearance.OnChange := AppearanceChanged;
  FPageSize := 20;
  {$IFDEF WEBLIB}
  FTimer := TTimer.Create(nil);
  {$ELSE}
  FTimer := TTimer.Create(Self);
  {$ENDIF}
  FTimer.OnTimer := TimerChanged;
  FTimer.Interval := 100;
end;

destructor TTMSFNCCustomScrollBar.Destroy;
begin
  FAppearance.Free;
  FTimer.Free;
  inherited;
end;

procedure TTMSFNCCustomScrollBar.DrawArrow(g: TTMSFNCGraphics; r: TRectF; ALeft: Boolean);
begin
  g.Stroke.Color := Appearance.ArrowColor;
  if Kind = sbkVertical then
  begin
    InflateRectEx(r, 0, -4);
    if not ALeft then
    begin
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 2), r.Top + ((r.Bottom - R.Top) / 3 * 2)), PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 3)));
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 2), r.Top + ((r.Bottom - R.Top) / 3 * 2)), PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 3)));
    end
    else
    begin
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 2), r.Top + ((r.Bottom - R.Top) / 3)), PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 3 * 2)));
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 2), r.Top + ((r.Bottom - R.Top) / 3)), PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 3 * 2)));
    end;
  end
  else
  begin
    InflateRectEx(r, -4, 0);
    if not ALeft then
    begin
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 3)), PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 2)));
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 3 * 2)), PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 2)));
    end
    else
    begin
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 3)), PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 2)));
      g.DrawLine(PointF(r.Left + ((r.Right - R.Left) / 3 * 2), r.Top + ((r.Bottom - R.Top) / 3 * 2)), PointF(r.Left + ((r.Right - R.Left) / 3), r.Top + ((r.Bottom - R.Top) / 2)));
    end;
  end;
end;


procedure TTMSFNCCustomScrollBar.DrawScrollButtons(g: TTMSFNCGraphics);
var
  flmin, flmax: TTMSFNCGraphicsFill;
  stmin, stmax: TTMSFNCGraphicsStroke;
begin
  if Appearance.ScrollButtonSize <= 0 then
    Exit;

  flmin := Appearance.ScrollButtonLeftFill;
  flmax := Appearance.ScrollButtonRightFill;
  stmin := Appearance.ScrollButtonLeftStroke;
  stmax := Appearance.ScrollButtonRightStroke;

  if FDownButton = sdbScrollMin then
  begin
    flmin := Appearance.ScrollButtonLeftDownFill;
    stmin := Appearance.ScrollButtonLeftDownStroke;
  end
  else if FHoveredButton = shbScrollMin then
  begin
    flmin := Appearance.ScrollButtonLeftHoverFill;
    stmin := Appearance.ScrollButtonLeftHoverStroke;
  end;

  g.Fill.Assign(flmin);
  g.Stroke.Assign(stmin);
  g.DrawRectangle(GetMinScrollButton);

  if FDownButton = sdbScrollMax then
  begin
    flmax := Appearance.ScrollButtonRightDownFill;
    stmax := Appearance.ScrollButtonRightDownStroke;
  end
  else if FHoveredButton = shbScrollMax then
  begin
    flmax := Appearance.ScrollButtonRightHoverFill;
    stmax := Appearance.ScrollButtonRightHoverStroke;
  end;

  g.Fill.Assign(flmax);
  g.Stroke.Assign(stmax);
  g.DrawRectangle(GetMaxScrollButton);

  DrawArrow(g, GetMinScrollButton, true);
  DrawArrow(g, GetMaxScrollButton, false);
end;

procedure TTMSFNCCustomScrollBar.DrawThumb(g: TTMSFNCGraphics);
begin
  g.Fill.Assign(Appearance.ThumbFill);
  g.Stroke.Assign(Appearance.ThumbStroke);
  g.DrawRectangle(GetThumbRectangle);
  if not Appearance.FixedThumb then
    DrawThumbButtons(g);
end;

procedure TTMSFNCCustomScrollBar.DrawThumbButtons(g: TTMSFNCGraphics);
var
  flmin, flmax: TTMSFNCGraphicsFill;
  stmin, stmax: TTMSFNCGraphicsStroke;
begin
  if Appearance.ThumbButtonSize <= 0 then
    Exit;

  flmin := Appearance.ThumbButtonLeftFill;
  flmax := Appearance.ThumbButtonRightFill;
  stmin := Appearance.ThumbButtonLeftStroke;
  stmax := Appearance.ThumbButtonRightStroke;

  if FDownButton = sdbThumbMin then
  begin
    flmin := Appearance.ThumbButtonLeftDownFill;
    stmin := Appearance.ThumbButtonLeftDownStroke;
  end
  else if FHoveredButton = shbThumbMin then
  begin
    flmin := Appearance.ThumbButtonLeftHoverFill;
    stmin := Appearance.ThumbButtonLeftHoverStroke;
  end;

  g.Fill.Assign(flmin);
  g.Stroke.Assign(stmin);
  g.DrawRectangle(GetMinThumbButton);

  if FDownButton = sdbThumbMax then
  begin
    flmax := Appearance.ThumbButtonRightDownFill;
    stmax := Appearance.ThumbButtonRightDownStroke;
  end
  else if FHoveredButton = shbThumbMax then
  begin
    flmax := Appearance.ThumbButtonRightHoverFill;
    stmax := Appearance.ThumbButtonRightHoverStroke;
  end;

  g.Fill.Assign(flmax);
  g.Stroke.Assign(stmax);
  g.DrawRectangle(GetMaxThumbButton);

  DrawArrow(g, GetMinThumbButton, true);
  DrawArrow(g, GetMaxThumbButton, false);
end;

function TTMSFNCCustomScrollBar.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfncuipack';
end;

function TTMSFNCCustomScrollBar.GetMaxScrollButton: TRectF;
var
  r: TRectF;
begin
  r := LocalRect;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Right - Appearance.ScrollButtonSize, r.Top, Appearance.ScrollButtonSize, r.Bottom - r.Top);
    sbkVertical: Result := MakeRectF(r.Left, r.Bottom - Appearance.ScrollButtonSize, r.Right - r.Left, Appearance.ScrollButtonSize);
  end;
end;

function TTMSFNCCustomScrollBar.GetMaxThumbButton: TRectF;
var
  r: TRectF;
begin
  r := GetThumbRectangle;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left + (r.Right - R.Left) - Appearance.GetThumbButtonSize, r.Top, Appearance.GetThumbButtonSize, (r.Bottom - R.Top));
    sbkVertical: Result := MakeRectF(r.Left, r.Top + (r.Bottom - R.Top) - Appearance.GetThumbButtonSize, (r.Right - R.Left), Appearance.GetThumbButtonSize);
  end;
end;

function TTMSFNCCustomScrollBar.GetMinScrollButton: TRectF;
var
  r: TRectF;
begin
  r := LocalRect;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left, r.Top, Appearance.ScrollButtonSize, r.Bottom - r.Top);
    sbkVertical: Result := MakeRectF(r.Left, r.Top, r.Right - r.Left, Appearance.ScrollButtonSize);
  end;
end;

function TTMSFNCCustomScrollBar.GetMinThumbButton: TRectF;
var
  r: TRectF;
begin
  r := GetThumbRectangle;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left, r.Top, Appearance.GetThumbButtonSize, (r.Bottom - R.Top));
    sbkVertical: Result := MakeRectF(r.Left, r.Top, (r.Right - R.Left), Appearance.GetThumbButtonSize);
  end;
end;

function TTMSFNCCustomScrollBar.GetValue(XYPos: Double): Double;
var
  v, s: Double;
  scr: TRectF;
begin
  scr := GetScrollRectangle;
  case Kind of
    sbkHorizontal: s := (scr.Right - scr.Left) - Math.Max(Appearance.GetThumbButtonSize * 2 + 10, ((scr.Right - scr.Left) / GetRange * PageSize));
    sbkVertical: s := (scr.Bottom - scr.Top) - Math.Max(Appearance.GetThumbButtonSize * 2 + 10, ((scr.Bottom - scr.Top) / GetRange * PageSize));
    else
      s := 0;
  end;

  if (max - min > 0) and (s > 0) then
  begin
    v := (XYPos / s) * (max - min) + min;
    Result := Math.Max(Math.Min(v, max), min);
  end
  else
    Result := 0;
end;

function TTMSFNCCustomScrollBar.GetValue: Double;
var
  s: Double;
  scr: TRectF;
begin
  Result := 0;
  scr := GetScrollRectangle;
  case Kind of
    sbkHorizontal: s := (scr.Right - scr.Left) - Math.Max(Appearance.GetThumbButtonSize * 2 + 10, ((scr.Right - scr.Left) / GetRange * PageSize));
    sbkVertical: s := (scr.Bottom - scr.Top) - Math.Max(Appearance.GetThumbButtonSize * 2 + 10, ((scr.Bottom - scr.Top) / GetRange * PageSize));
    else
      s := 0;
  end;

  if s <= 0 then
    Exit;

  if (Max - Min) > 0 then
  begin
    if Tracking then
      Result := Math.Min(((Value - Min) / (Max - Min)) * s, s)
    else
      Result := Math.Min(((FTempValue - Min) / (Max - Min)) * s, s)
  end
  else
    Result := Min;
end;

function TTMSFNCCustomScrollBar.GetRange: Double;
begin
  Result := Math.Max(2, Max - Min);
end;

function TTMSFNCCustomScrollBar.GetScrollAreaMin: TRectF;
var
  scr: TRectF;
  thr: TRectF;
begin
  scr := GetScrollRectangle;
  thr := GetThumbRectangle;
  case Kind of
    sbkHorizontal:
    begin
      Result.Left := scr.Left;
      Result.Top := scr.Top;
      Result.Right := Result.Left + (thr.Left - scr.Left);
      Result.Bottom := Result.Top + (scr.Bottom - scr.Top);
    end;
    sbkVertical:
    begin
      Result.Left := scr.Left;
      Result.Top := scr.Top;
      Result.Right := Result.Left + (scr.Right - scr.Left);
      Result.Bottom := Result.Top + (thr.Top - scr.Top);
    end;
  end;
end;

function TTMSFNCCustomScrollBar.GetScrollAreaMax: TRectF;
var
  scr: TRectF;
  thr: TRectF;
begin
  scr := GetScrollRectangle;
  thr := GetThumbRectangle;
  case Kind of
    sbkHorizontal:
    begin
      Result.Left := thr.Left + (thr.Right - thr.Left);
      Result.Top := scr.Top;
      Result.Right := Result.Left + (scr.Right - scr.Left) - (thr.Right - thr.Left) - thr.Left + scr.Left;
      Result.Bottom := Result.Top + (scr.Bottom - scr.Top);
    end;
    sbkVertical:
    begin
      Result.Left := scr.Left;
      Result.Top := thr.Top + (thr.Bottom - thr.Top);
      Result.Right := Result.Left + (scr.Right - scr.Left);
      result.Bottom := Result.Top + (scr.Bottom - scr.Top) - (thr.Bottom - thr.Top) - thr.Top + scr.Top;
    end;
  end;
end;

function TTMSFNCCustomScrollBar.GetScrollRectangle: TRectF;
begin
  case Kind of
    sbkHorizontal:
    begin
      Result.Left := GetMinScrollButton.Left + (GetMinScrollButton.Right - GetMinScrollButton.Left);
      Result.Top := GetMinScrollButton.Top;
      Result.Bottom := Result.Top + (GetMinScrollButton.Bottom - GetMinScrollButton.Top);
      Result.Right := Result.Left + GetMaxScrollButton.Left - (GetMinScrollbutton.Right - GetMinScrollButton.Left);
    end;
    sbkVertical:
    begin
      Result.Left := GetMinScrollButton.Left;
      Result.Top := GetMinScrollButton.Top + (GetMinScrollButton.Bottom - GetMinScrollButton.Top);
      Result.Bottom := Result.Top + GetMaxScrollButton.Top - (GetMinScrollButton.Bottom - GetMinScrollButton.Top);
      Result.Right := Result.Left + (GetMinScrollButton.Right - GetMinScrollButton.Left);
    end;
  end;
end;

function TTMSFNCCustomScrollBar.GetScrollSize: Double;
begin
  case Kind of
    sbkHorizontal: Result := (GetScrollRectangle.Right - GetScrollRectangle.Left) - (GetThumbRectangle.Right - GetThumbRectangle.Left);
    sbkVertical: Result := (GetScrollRectangle.Bottom - GetScrollRectangle.Top) - (GetThumbRectangle.Bottom - GetThumbRectangle.Top);
    else
      Result := 0;
  end;
end;

function TTMSFNCCustomScrollBar.GetThumbRectangle: TRectF;
var
  scr: TRectF;
  s: Double;
begin
  scr := GetScrollRectangle;
  case Kind of
    sbkHorizontal:
    begin
      s := (scr.Right - scr.Left) / GetRange * PageSize;
      Result.Left := scr.Left + GetValue;
      Result.Top := scr.Top;
      Result.Right := Result.Left + Math.Max(Appearance.GetThumbButtonSize * 2 + ScalePaintValue(10), s);
      Result.Bottom := Result.Top + (scr.Bottom - scr.Top);
    end;
    sbkVertical:
    begin
      s := (scr.Bottom - scr.Top) / GetRange * PageSize;
      Result.Left := scr.Left;
      Result.Top := scr.Top + GetValue;
      Result.Right := Result.Left + (scr.Right - scr.Left);
      Result.Bottom := Result.Top + Math.Max(Appearance.GetThumbButtonSize * 2 + ScalePaintValue(10), s);
    end;
  end;
end;

procedure TTMSFNCCustomScrollBar.HandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    KEY_UP, KEY_LEFT: Value := Value - SmallChange;
    KEY_DOWN, KEY_RIGHT: Value := Value + SmallChange;
    KEY_HOME: Value := Min;
    KEY_END: Value := Max;
    KEY_PRIOR: Value := Value - LargeChange;
    KEY_NEXT: Value := Value + LargeChange;
  end;
end;

procedure TTMSFNCCustomScrollBar.HandleMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  CaptureEx;
  FCx := Abs(X - GetThumbRectangle.Left + GetScrollRectangle.Left);
  FCy := Abs(Y - GetThumbRectangle.Top + GetScrollRectangle.Top);
  FMx := X;
  FMy := Y;
  FSavedPageSize := PageSize;
  FTime := 0;
  FTimer.Enabled := true;
  if MouseOnThumbButtons(X, Y) then
  begin
    FScrollBarMode := sbmChangePageSize;
    FMinThumb := PtInRectEx(GetMinThumbButton, PointF(X, Y));
    FMaxThumb := PtInRectEx(GetMaxThumbButton, PointF(X, Y));
  end
  else if MouseOnThumb(X, Y) then
    FScrollBarMode := sbmScroll
  else if PtInRectEx(GetMinScrollButton, PointF(X, Y)) then
    FScrollButtonChange := sbcSmallSubstract
  else if PtInRectEx(GetMaxScrollButton, PointF(X, Y)) then
    FScrollButtonChange := sbcSmallAdd
  else if PtInRectEx(GetScrollAreaMin, PointF(X, Y)) then
    FScrollButtonChange := sbcLargeSubstract
  else if PtInRectEx(GetScrollAreaMax, PointF(X, Y)) then
    FScrollButtonChange := sbcLargeAdd;

  if PtInRectEx(GetMinThumbButton, PointF(X, Y)) and not (FDownButton = sdbThumbMin) then
  begin
    FDownButton := sdbThumbMin;
    Changed;
  end
  else if PtInRectEx(GetMaxThumbButton, PointF(X, Y)) and not (FDownButton = sdbThumbMax) then
  begin
    FDownButton := sdbThumbMax;
    Changed;
  end
  else if PtInRectEx(GetMinScrollButton, PointF(X, Y)) and not (FDownButton = sdbScrollMin) then
  begin
    FDownButton := sdbScrollMin;
    Changed;
  end
  else if PtInRectEx(GetMaxScrollButton, PointF(X, Y)) and not (FDownButton = sdbScrollMax) then
  begin
    FDownButton := sdbScrollMax;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.HandleMouseEnter;
begin
  inherited;

end;

procedure TTMSFNCCustomScrollBar.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  pos: Double;
  v: Double;
begin
  inherited;
  begin
    if FScrollBarMode = sbmChangePageSize then
    begin
      case Kind of
        sbkHorizontal: Cursor := crSizeWE;
        sbkVertical: Cursor := crSizeNS;
      end;

      case Kind of
        sbkHorizontal: pos := GetRange / (GetScrollRectangle.Right - GetScrollRectangle.Left) * (X - FMx);
        sbkVertical: pos := GetRange / (GetScrollRectangle.Bottom - GetScrollRectangle.Top) * (Y - FMy);
        else
          pos := 0;
      end;

      if FMinThumb then
      begin
        if Value - Min > 0 then
          PageSize := FSavedPageSize - pos * (GetRange / (Value - Min))
        else
          PageSize := FSavedPageSize - pos
      end
      else if FMaxThumb then
      begin
        if Value - Min < GetRange then
          PageSize := FSavedPageSize + pos * (GetRange / (Max - Value))
        else
          PageSize := FSavedPageSize + pos
      end;
    end
    else if (FScrollBarMode = sbmScroll) then
    begin
      case Kind of
        sbkHorizontal: pos := X - FCx;
        sbkVertical: pos := Y - FCy;
        else
          pos := 0;
      end;

      v := GetValue(pos);
      if Tracking then
        Value := v
      else
      begin
        FTempValue := v;
        Changed;
      end;
    end
    else if MouseOnThumbButtons(X, Y) then
    begin
      case Kind of
        sbkHorizontal: Cursor := crSizeWE;
        sbkVertical: Cursor := crSizeNS;
      end;
    end
    else
    begin
      Cursor := crDefault;
    end;
  end;

  if PtInRectEx(GetMinThumbButton, PointF(X, Y)) and not (FHoveredButton = shbThumbMin) then
  begin
    FHoveredButton := shbThumbMin;
    Changed;
  end
  else if PtInRectEx(GetMaxThumbButton, PointF(X, Y)) and not (FHoveredButton = shbThumbMax) then
  begin
    FHoveredButton := shbThumbMax;
    Changed;
  end
  else if PtInRectEx(GetMinScrollButton, PointF(X, Y)) and not (FHoveredButton = shbScrollMin) then
  begin
    FHoveredButton := shbScrollMin;
    Changed;
  end
  else if PtInRectEx(GetMaxScrollButton, PointF(X, Y)) and not (FHoveredButton = shbScrollMax) then
  begin
    FHoveredButton := shbScrollMax;
    Changed;
  end
  else if not PtInRectEx(GetMaxThumbButton, PointF(X, Y)) and not PtInRectEx(GetMinThumbButton, PointF(X, Y)) and not PtInRectEx(GetMaxScrollButton, PointF(X, Y))
    and not PtInRectEx(GetMinScrollButton, PointF(X, Y)) and (FHoveredButton <> shbNone) then
  begin
    FHoveredButton := shbNone;
    Changed;
  end;
end;

function TTMSFNCCustomScrollBar.MouseOnThumb(X, Y: Double): Boolean;
begin
  Result := PtInRectEx(GetThumbRectangle, PointF(X, Y));
end;

function TTMSFNCCustomScrollBar.MouseOnThumbButtons(X, Y: Double): Boolean;
begin
  Result := PtInRectEx(GetMinThumbButton, PointF(X, Y)) or PtInRectEx(GetMaxThumbButton, PointF(X, Y));
  Result := Result and not Appearance.FixedThumb;
end;

procedure TTMSFNCCustomScrollBar.ResetToDefaultStyle;
var
  ia: TTMSFNCScrollBarAppearance;
begin
  inherited;
  ia := TTMSFNCScrollBarAppearance.Create(nil);
  try
    Appearance.Assign(ia);
  finally
    ia.Free;
  end;
end;

procedure TTMSFNCCustomScrollBar.HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  ReleaseCaptureEx;
  if (FScrollBarMode = sbmScroll) and not Tracking then
    Value := FTempValue;

  if (FScrollBarMode = sbmNone) then
  begin
    if PtInRectEx(GetMinScrollButton, PointF(X, Y)) then
      Value := Value - SmallChange
    else if PtInRectEx(GetMaxScrollButton, PointF(X, Y)) then
      Value := Value + SmallChange
    else if PtInRectEx(GetScrollAreaMin, PointF(X, Y)) then
      Value := Value - LargeChange
    else if PtInRectEx(GetScrollAreaMax, PointF(X, Y)) then
      Value := Value + LargeChange;
  end;

  FDownButton := sdbNone;
  FMinThumb := false;
  FMaxThumb := false;
  FTimer.Enabled := false;
  FTime := 0;
  FScrollBarMode := sbmNone;
  Cursor := crDefault;
  FScrollButtonChange := sbcNone;
  Changed;
end;

procedure TTMSFNCCustomScrollBar.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if WheelDelta < 0 then
    Value := Value + LargeChange
  else
    Value := Value - LargeChange;
end;

function TTMSFNCCustomScrollBar.IsLargeChangeStored: Boolean;
begin
  Result := LargeChange <> 20;
end;

function TTMSFNCCustomScrollBar.IsMaxStored: Boolean;
begin
  Result := Max <> 100;
end;

function TTMSFNCCustomScrollBar.IsMinStored: Boolean;
begin
  Result := Min <> 0;
end;

function TTMSFNCCustomScrollBar.IsPageSizeStored: Boolean;
begin
  Result := PageSize <> 20;
end;

function TTMSFNCCustomScrollBar.IsSmallChangeStored: Boolean;
begin
  Result := SmallChange <> 1;
end;

function TTMSFNCCustomScrollBar.IsValueStored: Boolean;
begin
  Result := Value <> 0;
end;

procedure TTMSFNCCustomScrollBar.Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  inherited;
  DrawThumb(AGraphics);
  DrawScrollButtons(AGraphics);
end;

procedure TTMSFNCCustomScrollBar.SetAppearance(
  const Value: TTMSFNCScrollBarAppearance);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetKind(Value: TTMSFNCScrollBarKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width)
    else
      FNeedsChange := True;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetMax(const Value: Double);
begin
  if FMax <> value then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetMin(const Value: Double);
begin
  if FMin <> value then
  begin
    FMin := Value;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetPageSize(const Value: Double);
begin
  if FPageSize <> value then
  begin
    FPageSize := Math.Min(GetRange, Math.Max(0, Value));
    if Assigned(FOnPageSizeChanged) then
      FOnPageSizeChanged(Self, PageSize);
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetTracking(const Value: Boolean);
begin
  if FTracking <> Value then
  begin
    FTracking := Value;
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.SetValue(const Value: Double);
begin
  if FValue <> Value then
  begin
    FValue := Math.Max(Self.Min, Math.Min(Self.Max, Value));
    FTempValue := FValue;
    if Assigned(FOnValueChange) then
        FOnValueChange(Self, Value);
    Changed;
  end;
end;

procedure TTMSFNCCustomScrollBar.TimerChanged(Sender: TObject);
begin
  Inc(FTime);
  if FTime >= 4 then
  begin
    if FScrollButtonChange = sbcSmallSubstract then
    begin
      Value := Value - SmallChange;
      Changed;
    end
    else if FScrollButtonChange = sbcSmallAdd then
    begin
      Value := Value + SmallChange;
      Changed;
    end
    else if FScrollButtonChange = sbcLargeSubstract then
    begin
      Value := Value - LargeChange;
      Changed;
    end
    else if FScrollButtonChange = sbcLargeAdd then
    begin
      Value := Value + LargeChange;
      Changed;
    end;
  end;
end;

{ TTMSFNCScrollBar }

procedure TTMSFNCScrollBar.RegisterRuntimeClasses;
begin
  RegisterClass(TTMSFNCScrollBar);
end;

end.
