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

unit AdvScrollBar;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvCustomControl, ExtCtrls,
  AdvTypes, AdvGraphics, Controls,
  AdvGraphicsTypes
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
  TAdvCustomScrollBar = class;

  TAdvScrollBarAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomScrollBar;
    FScrollButtonSize: Single;
    FThumbButtonSize: Single;
    FOnChange: TNotifyEvent;
    FFixedThumb: Boolean;
    FArrowColor: TAdvGraphicsColor;
    FThumbStroke: TAdvGraphicsStroke;
    FThumbButtonRightDownStroke: TAdvGraphicsStroke;
    FThumbFill: TAdvGraphicsFill;
    FThumbButtonLeftFill: TAdvGraphicsFill;
    FScrollButtonLeftHoverFill: TAdvGraphicsFill;
    FScrollButtonLeftStroke: TAdvGraphicsStroke;
    FScrollButtonRightFill: TAdvGraphicsFill;
    FThumbButtonLeftHoverFill: TAdvGraphicsFill;
    FThumbButtonLeftStroke: TAdvGraphicsStroke;
    FScrollButtonLeftHoverStroke: TAdvGraphicsStroke;
    FThumbButtonRightFill: TAdvGraphicsFill;
    FScrollButtonRightHoverFill: TAdvGraphicsFill;
    FScrollButtonRightStroke: TAdvGraphicsStroke;
    FThumbButtonLeftHoverStroke: TAdvGraphicsStroke;
    FScrollButtonLeftDownFill: TAdvGraphicsFill;
    FThumbButtonRightHoverFill: TAdvGraphicsFill;
    FThumbButtonRightStroke: TAdvGraphicsStroke;
    FScrollButtonRightHoverStroke: TAdvGraphicsStroke;
    FThumbButtonLeftDownFill: TAdvGraphicsFill;
    FScrollButtonLeftDownStroke: TAdvGraphicsStroke;
    FThumbButtonRightHoverStroke: TAdvGraphicsStroke;
    FScrollButtonRightDownFill: TAdvGraphicsFill;
    FThumbButtonLeftDownStroke: TAdvGraphicsStroke;
    FThumbButtonRightDownFill: TAdvGraphicsFill;
    FScrollButtonRightDownStroke: TAdvGraphicsStroke;
    FScrollButtonLeftFill: TAdvGraphicsFill;
    procedure SetThumbButtonSize(const Value: Single);
    procedure SetFixedThumb(const Value: Boolean);
    procedure SetArrowColor(const Value: TAdvGraphicsColor);
    procedure SetScrollButtonLeftDownFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonLeftDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonLeftFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonLeftHoverFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonLeftHoverStroke(
      const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonLeftStroke(const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonRightDownFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonRightDownStroke(
      const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonRightFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonRightHoverFill(const Value: TAdvGraphicsFill);
    procedure SetScrollButtonRightHoverStroke(
      const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonRightStroke(const Value: TAdvGraphicsStroke);
    procedure SetScrollButtonSize(const Value: Single);
    procedure SetThumbButtonLeftDownFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonLeftDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetThumbButtonLeftFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonLeftHoverFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonLeftHoverStroke(const Value: TAdvGraphicsStroke);
    procedure SetThumbButtonLeftStroke(const Value: TAdvGraphicsStroke);
    procedure SetThumbButtonRightDownFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonRightDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetThumbButtonRightFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonRightHoverFill(const Value: TAdvGraphicsFill);
    procedure SetThumbButtonRightHoverStroke(
      const Value: TAdvGraphicsStroke);
    procedure SetThumbButtonRightStroke(const Value: TAdvGraphicsStroke);
    procedure SetThumbFill(const Value: TAdvGraphicsFill);
    procedure SetThumbStroke(const Value: TAdvGraphicsStroke);
    function IsScrollButtonSizeStored: Boolean;
    function IsThumbButtonSizeStored: Boolean;
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvCustomScrollBar);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetThumbButtonSize: Single;
  published
    property ThumbFill: TAdvGraphicsFill read FThumbFill write SetThumbFill;
    property ScrollButtonLeftFill: TAdvGraphicsFill read FScrollButtonLeftFill write SetScrollButtonLeftFill;
    property ScrollButtonRightFill: TAdvGraphicsFill read FScrollButtonRightFill write SetScrollButtonRightFill;
    property ScrollButtonLeftHoverFill: TAdvGraphicsFill read FScrollButtonLeftHoverFill write SetScrollButtonLeftHoverFill;
    property ScrollButtonRightHoverFill: TAdvGraphicsFill read FScrollButtonRightHoverFill write SetScrollButtonRightHoverFill;
    property ScrollButtonLeftDownFill: TAdvGraphicsFill read FScrollButtonLeftDownFill write SetScrollButtonLeftDownFill;
    property ScrollButtonRightDownFill: TAdvGraphicsFill read FScrollButtonRightDownFill write SetScrollButtonRightDownFill;
    property ThumbButtonLeftFill: TAdvGraphicsFill read FThumbButtonLeftFill write SetThumbButtonLeftFill;
    property ThumbButtonRightFill: TAdvGraphicsFill read FThumbButtonRightFill write SetThumbButtonRightFill;
    property ThumbButtonLeftHoverFill: TAdvGraphicsFill read FThumbButtonLeftHoverFill write SetThumbButtonLeftHoverFill;
    property ThumbButtonRightHoverFill: TAdvGraphicsFill read FThumbButtonRightHoverFill write SetThumbButtonRightHoverFill;
    property ThumbButtonLeftDownFill: TAdvGraphicsFill read FThumbButtonLeftDownFill write SetThumbButtonLeftDownFill;
    property ThumbButtonRightDownFill: TAdvGraphicsFill read FThumbButtonRightDownFill write SetThumbButtonRightDownFill;
    property ThumbStroke: TAdvGraphicsStroke read FThumbStroke write SetThumbStroke;
    property ScrollButtonLeftStroke: TAdvGraphicsStroke read FScrollButtonLeftStroke write SetScrollButtonLeftStroke;
    property ScrollButtonRightStroke: TAdvGraphicsStroke read FScrollButtonRightStroke write SetScrollButtonRightStroke;
    property ScrollButtonLeftHoverStroke: TAdvGraphicsStroke read FScrollButtonLeftHoverStroke write SetScrollButtonLeftHoverStroke;
    property ScrollButtonRightHoverStroke: TAdvGraphicsStroke read FScrollButtonRightHoverStroke write SetScrollButtonRightHoverStroke;
    property ScrollButtonLeftDownStroke: TAdvGraphicsStroke read FScrollButtonLeftDownStroke write SetScrollButtonLeftDownStroke;
    property ScrollButtonRightDownStroke: TAdvGraphicsStroke read FScrollButtonRightDownStroke write SetScrollButtonRightDownStroke;
    property ThumbButtonLeftStroke: TAdvGraphicsStroke read FThumbButtonLeftStroke write SetThumbButtonLeftStroke;
    property ThumbButtonRightStroke: TAdvGraphicsStroke read FThumbButtonRightStroke write SetThumbButtonRightStroke;
    property ThumbButtonLeftHoverStroke: TAdvGraphicsStroke read FThumbButtonLeftHoverStroke write SetThumbButtonLeftHoverStroke;
    property ThumbButtonRightHoverStroke: TAdvGraphicsStroke read FThumbButtonRightHoverStroke write SetThumbButtonRightHoverStroke;
    property ThumbButtonLeftDownStroke: TAdvGraphicsStroke read FThumbButtonLeftDownStroke write SetThumbButtonLeftDownStroke;
    property ThumbButtonRightDownStroke: TAdvGraphicsStroke read FThumbButtonRightDownStroke write SetThumbButtonRightDownStroke;
    property ThumbButtonSize: Single read FThumbButtonSize write SetThumbButtonSize stored IsThumbButtonSizeStored nodefault;
    property ScrollButtonSize: Single read FScrollButtonSize write SetScrollButtonSize stored IsScrollButtonSizeStored nodefault;
    property FixedThumb: Boolean read FFixedThumb write SetFixedThumb default False;
    property ArrowColor: TAdvGraphicsColor read FArrowColor write SetArrowColor default gcBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvScrollBarMode = (sbmNone, sbmScroll, sbmChangePageSize);

  TAdvScrollBarHoveredButton = (shbNone, shbScrollMin, shbScrollMax, shbThumbMin, shbThumbMax);

  TAdvScrollBarDownButton = (sdbNone, sdbScrollMin, sdbScrollMax, sdbThumbMin, sdbThumbMax);

  TAdvScrollButtonChange = (sbcNone, sbcSmallSubstract, sbcSmallAdd, sbcLargeSubstract, sbcLargeAdd);

  TAdvScrollBarValueChanged = procedure(Sender: TObject; Value: Double) of object;

  TAdvScrollBarPageSizeChanged = procedure(Sender: TObject; PageSize: Double) of object;

  TAdvScrollBarKind = (sbkHorizontal, sbkVertical);

  TAdvCustomScrollBar = class(TAdvCustomControl)
  private
    FNeedsChange: Boolean;
    FSavedPageSize: Double;
    FDesignTime, FMinThumb, FMaxThumb: Boolean;
    FMx, FMy, FCx, FCy: Double;
    FTimer: TTimer;
    FTime: integer;
    FScrollBarMode: TAdvScrollBarMode;
    FScrollButtonChange: TAdvScrollButtonChange;
    FHoveredButton: TAdvScrollBarHoveredButton;
    FDownButton: TAdvScrollBarDownButton;
    FKind: TAdvScrollBarKind;
    FValue, FTempValue: Double;
    FMin: Double;
    FMax: Double;
    FPageSize: Double;
    FSmallChange: Double;
    FLargeChange: Double;
    FAppearance: TAdvScrollBarAppearance;
    FOnValueChange: TAdvScrollBarValueChanged;
    FOnPageSizeChanged: TAdvScrollBarPageSizeChanged;
    FTracking: Boolean;
    procedure SetAppearance(const Value: TAdvScrollBarAppearance);
    procedure SetKind(Value: TAdvScrollBarKind);
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
    procedure DrawScrollButtons(g: TAdvGraphics);
    procedure DrawThumb(g: TAdvGraphics);
    procedure DrawThumbButtons(g: TAdvGraphics);
    procedure DrawArrow(g: TAdvGraphics; r: TRectF; ALeft: Boolean);
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    property Appearance: TAdvScrollBarAppearance read FAppearance write SetAppearance;
    property Kind: TAdvScrollBarKind read FKind write SetKind default sbkVertical;
    property LargeChange: Double read FLargeChange write FLargeChange stored IsLargeChangeStored nodefault;
    property Max: Double read FMax write SetMax stored IsMaxStored nodefault;
    property Min: Double read FMin write SetMin stored IsMinStored nodefault;
    property PageSize: Double read FPageSize write SetPageSize stored IsPageSizeStored nodefault;
    property Value: Double read FValue write SetValue stored IsValueStored nodefault;
    property SmallChange: Double read FSmallChange write FSmallChange stored IsSmallChangeStored nodefault;
    property Tracking: Boolean read FTracking write SetTracking default True;
    property OnValueChanged: TAdvScrollBarValueChanged read FOnValueChange write FOnValueChange;
    property OnPageSizeChanged: TAdvScrollBarPageSizeChanged read FOnPageSizeChanged write FOnPageSizeChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvScrollBar = class(TAdvCustomScrollBar)
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
  Math, AdvGraphicsStyles, AdvUtils;

{ TAdvScrollBarAppearance }

procedure TAdvScrollBarAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvScrollBarAppearance) then
  begin
    FThumbFill.Assign((Source as TAdvScrollBarAppearance).ThumbFill);
    FScrollButtonSize := (Source as TAdvScrollBarAppearance).ScrollButtonSize;
    FThumbButtonSize := (source as TAdvScrollBarAppearance).ThumbButtonSize;
    FThumbButtonLeftFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftFill);
    FThumbButtonLeftHoverFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftHoverFill);
    FThumbButtonLeftDownFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftDownFill);
    FThumbButtonRightFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightFill);
    FThumbButtonRightDownFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightDownFill);
    FThumbButtonRightHoverFill.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightHoverFill);
    FScrollButtonLeftFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftFill);
    FScrollButtonLeftHoverFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftHoverFill);
    FScrollButtonLeftDownFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftDownFill);
    FScrollButtonRightFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightFill);
    FScrollButtonRightDownFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightDownFill);
    FScrollButtonRightHoverFill.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightHoverFill);
    FThumbButtonLeftStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftStroke);
    FThumbButtonLeftHoverStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftHoverStroke);
    FThumbButtonLeftDownStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonLeftDownStroke);
    FThumbButtonRightStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightStroke);
    FThumbButtonRightDownStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightDownStroke);
    FThumbButtonRightHoverStroke.Assign((Source as TAdvScrollBarAppearance).ThumbButtonRightHoverStroke);
    FScrollButtonLeftStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftStroke);
    FScrollButtonLeftHoverStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftHoverStroke);
    FScrollButtonLeftDownStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonLeftDownStroke);
    FScrollButtonRightStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightStroke);
    FScrollButtonRightDownStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightDownStroke);
    FScrollButtonRightHoverStroke.Assign((Source as TAdvScrollBarAppearance).ScrollButtonRightHoverStroke);
    Changed;
  end;
end;

procedure TAdvScrollBarAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvScrollBarAppearance.Create(AOwner: TAdvCustomScrollBar);
begin
  FOwner := AOwner;
  FThumbFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbFill.OnChanged := FillChanged;
  FThumbButtonLeftFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbButtonLeftFill.OnChanged := FillChanged;
  FThumbButtonRightFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FThumbButtonRightFill.OnChanged := FillChanged;
  FThumbButtonLeftHoverFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FThumbButtonLeftHoverFill.OnChanged := FillChanged;
  FThumbButtonRightHoverFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FThumbButtonRightHoverFill.OnChanged := FillChanged;
  FThumbButtonLeftDownFill := TAdvGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FThumbButtonLeftDownFill.OnChanged := FillChanged;
  FThumbButtonRightDownFill := TAdvGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FThumbButtonRightDownFill.OnChanged := FillChanged;

  FScrollButtonLeftFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FScrollButtonLeftFill.OnChanged := FillChanged;
  FScrollButtonRightFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(225, 225, 225));
  FScrollButtonRightFill.OnChanged := FillChanged;
  FScrollButtonLeftHoverFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FScrollButtonLeftHoverFill.OnChanged := FillChanged;
  FScrollButtonRightHoverFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(229, 241, 251));
  FScrollButtonRightHoverFill.OnChanged := FillChanged;
  FScrollButtonLeftDownFill := TAdvGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FScrollButtonLeftDownFill.OnChanged := FillChanged;
  FScrollButtonRightDownFill := TAdvGraphicsFill.Create(gfkSolid, Darker(MakeGraphicsColor(229, 241, 251), 40));
  FScrollButtonRightDownFill.OnChanged := FillChanged;

  FThumbStroke := TAdvGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftStroke := TAdvGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbButtonLeftStroke.OnChanged := StrokeChanged;
  FThumbButtonRightStroke := TAdvGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FThumbButtonRightStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftHoverStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonLeftHoverStroke.OnChanged := StrokeChanged;
  FThumbButtonRightHoverStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonRightHoverStroke.OnChanged := StrokeChanged;
  FThumbButtonLeftDownStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonLeftDownStroke.OnChanged := StrokeChanged;
  FThumbButtonRightDownStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FThumbButtonRightDownStroke.OnChanged := StrokeChanged;

  FScrollButtonLeftStroke := TAdvGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FScrollButtonLeftStroke.OnChanged := StrokeChanged;
  FScrollButtonRightStroke := TAdvGraphicsStroke.Create(gskSolid, Darker(gcDarkGray, 40));
  FScrollButtonRightStroke.OnChanged := StrokeChanged;
  FScrollButtonLeftHoverStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonLeftHoverStroke.OnChanged := StrokeChanged;
  FScrollButtonRightHoverStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonRightHoverStroke.OnChanged := StrokeChanged;
  FScrollButtonLeftDownStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonLeftDownStroke.OnChanged := StrokeChanged;
  FScrollButtonRightDownStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(60, 127, 177));
  FScrollButtonRightDownStroke.OnChanged := StrokeChanged;

  FArrowColor := gcBlack;

  FFixedThumb := False;
  FThumbButtonSize := 18;
  FScrollButtonSize := 18;
end;

destructor TAdvScrollBarAppearance.Destroy;
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

procedure TAdvScrollBarAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvScrollBarAppearance.GetThumbButtonSize: Single;
begin
  if FixedThumb then
    Result := 0
  else
    Result := ThumbButtonSize;
end;

function TAdvScrollBarAppearance.IsScrollButtonSizeStored: Boolean;
begin
  Result := ScrollButtonSize <> 18;
end;

function TAdvScrollBarAppearance.IsThumbButtonSizeStored: Boolean;
begin
  Result := ThumbButtonSize <> 18;
end;

procedure TAdvScrollBarAppearance.SetArrowColor(const Value: TAdvGraphicsColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TAdvScrollBarAppearance.SetFixedThumb(const Value: Boolean);
begin
  if FFixedThumb <> value then
  begin
    FFixedThumb := Value;
    Changed;
  end;
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftDownFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonLeftDownFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonLeftDownStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonLeftFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftHoverFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonLeftHoverFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftHoverStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonLeftHoverStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonLeftStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonLeftStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightDownFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonRightDownFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonRightDownStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonRightFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightHoverFill(
  const Value: TAdvGraphicsFill);
begin
  FScrollButtonRightHoverFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightHoverStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonRightHoverStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonRightStroke(
  const Value: TAdvGraphicsStroke);
begin
  FScrollButtonRightStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetScrollButtonSize(const Value: Single);
begin
  if FScrollButtonSize <> Value then
  begin
    FScrollButtonSize := Value;
    Changed;
  end;
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftDownFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonLeftDownFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonLeftDownStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonLeftFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftHoverFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonLeftHoverFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftHoverStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonLeftHoverStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonLeftStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonLeftStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightDownFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonRightDownFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonRightDownStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonRightFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightHoverFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbButtonRightHoverFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightHoverStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonRightHoverStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonRightStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbButtonRightStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbButtonSize(
  const Value: Single);
begin
  if FThumbButtonSize <> value then
  begin
    FThumbButtonSize := Value;
    Changed;
  end;
end;

procedure TAdvScrollBarAppearance.SetThumbFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbFill.Assign(Value);
end;

procedure TAdvScrollBarAppearance.SetThumbStroke(
  const Value: TAdvGraphicsStroke);
begin
  FThumbStroke.Assign(Value);
end;

procedure TAdvScrollBarAppearance.StrokeChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvCustomScrollBar.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvCustomScrollBar.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
  begin
    Appearance.ThumbFill.Color := c;
    Appearance.ThumbButtonLeftFill.Color := c;
    Appearance.ThumbButtonRightFill.Color := c;
    Appearance.ScrollButtonLeftFill.Color := c;
    Appearance.ScrollButtonRightFill.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleLineFillColor(c) then
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
  if TAdvGraphicsStyles.GetStyleSelectionFillColor(c) then
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

procedure TAdvCustomScrollBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvScrollBar) then
  begin
    FKind := (Source as TAdvScrollBar).Kind;
    FValue := (Source as TAdvScrollBar).Value;
    FMin := (Source as TAdvScrollBar).Min;
    FMax := (Source as TAdvScrollBar).Max;
    FPageSize := (Source as TAdvScrollBar).PageSize;
    FSmallChange := (Source as TAdvScrollBar).SmallChange;
    FLargeChange := (Source as TAdvScrollBar).LargeChange;
    FAppearance.Assign((Source as TAdvScrollBar).Appearance);
    FTracking := (Source as TAdvScrollBar).Tracking;
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.Changed;
begin
  Invalidate;
end;

procedure TAdvCustomScrollBar.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  BeginUpdate;

  FAppearance.FScrollButtonSize := TAdvUtils.MulDivSingle(FAppearance.ScrollButtonSize, M, D);
  FAppearance.FThumbButtonSize := TAdvUtils.MulDivSingle(FAppearance.FThumbButtonSize, M, D);

  EndUpdate;
end;

procedure TAdvCustomScrollBar.HandleMouseLeave;
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

constructor TAdvCustomScrollBar.Create(AOwner: TComponent);
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
  FAppearance := TAdvScrollBarAppearance.Create(Self);
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

destructor TAdvCustomScrollBar.Destroy;
begin
  FAppearance.Free;
  FTimer.Free;
  inherited;
end;

procedure TAdvCustomScrollBar.DrawArrow(g: TAdvGraphics; r: TRectF; ALeft: Boolean);
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


procedure TAdvCustomScrollBar.DrawScrollButtons(g: TAdvGraphics);
var
  flmin, flmax: TAdvGraphicsFill;
  stmin, stmax: TAdvGraphicsStroke;
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

procedure TAdvCustomScrollBar.DrawThumb(g: TAdvGraphics);
begin
  g.Fill.Assign(Appearance.ThumbFill);
  g.Stroke.Assign(Appearance.ThumbStroke);
  g.DrawRectangle(GetThumbRectangle);
  if not Appearance.FixedThumb then
    DrawThumbButtons(g);
end;

procedure TAdvCustomScrollBar.DrawThumbButtons(g: TAdvGraphics);
var
  flmin, flmax: TAdvGraphicsFill;
  stmin, stmax: TAdvGraphicsStroke;
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

function TAdvCustomScrollBar.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack';
end;

function TAdvCustomScrollBar.GetMaxScrollButton: TRectF;
var
  r: TRectF;
begin
  r := LocalRect;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Right - Appearance.ScrollButtonSize, r.Top, Appearance.ScrollButtonSize, r.Bottom - r.Top);
    sbkVertical: Result := MakeRectF(r.Left, r.Bottom - Appearance.ScrollButtonSize, r.Right - r.Left, Appearance.ScrollButtonSize);
  end;
end;

function TAdvCustomScrollBar.GetMaxThumbButton: TRectF;
var
  r: TRectF;
begin
  r := GetThumbRectangle;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left + (r.Right - R.Left) - Appearance.GetThumbButtonSize, r.Top, Appearance.GetThumbButtonSize, (r.Bottom - R.Top));
    sbkVertical: Result := MakeRectF(r.Left, r.Top + (r.Bottom - R.Top) - Appearance.GetThumbButtonSize, (r.Right - R.Left), Appearance.GetThumbButtonSize);
  end;
end;

function TAdvCustomScrollBar.GetMinScrollButton: TRectF;
var
  r: TRectF;
begin
  r := LocalRect;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left, r.Top, Appearance.ScrollButtonSize, r.Bottom - r.Top);
    sbkVertical: Result := MakeRectF(r.Left, r.Top, r.Right - r.Left, Appearance.ScrollButtonSize);
  end;
end;

function TAdvCustomScrollBar.GetMinThumbButton: TRectF;
var
  r: TRectF;
begin
  r := GetThumbRectangle;
  case Kind of
    sbkHorizontal: Result := MakeRectF(r.Left, r.Top, Appearance.GetThumbButtonSize, (r.Bottom - R.Top));
    sbkVertical: Result := MakeRectF(r.Left, r.Top, (r.Right - R.Left), Appearance.GetThumbButtonSize);
  end;
end;

function TAdvCustomScrollBar.GetValue(XYPos: Double): Double;
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

function TAdvCustomScrollBar.GetValue: Double;
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

function TAdvCustomScrollBar.GetRange: Double;
begin
  Result := Math.Max(2, Max - Min);
end;

function TAdvCustomScrollBar.GetScrollAreaMin: TRectF;
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

function TAdvCustomScrollBar.GetScrollAreaMax: TRectF;
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

function TAdvCustomScrollBar.GetScrollRectangle: TRectF;
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

function TAdvCustomScrollBar.GetScrollSize: Double;
begin
  case Kind of
    sbkHorizontal: Result := (GetScrollRectangle.Right - GetScrollRectangle.Left) - (GetThumbRectangle.Right - GetThumbRectangle.Left);
    sbkVertical: Result := (GetScrollRectangle.Bottom - GetScrollRectangle.Top) - (GetThumbRectangle.Bottom - GetThumbRectangle.Top);
    else
      Result := 0;
  end;
end;

function TAdvCustomScrollBar.GetThumbRectangle: TRectF;
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

procedure TAdvCustomScrollBar.HandleKeyDown(var Key: Word; Shift: TShiftState);
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

procedure TAdvCustomScrollBar.HandleMouseDown(Button: TMouseButton;
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

procedure TAdvCustomScrollBar.HandleMouseEnter;
begin
  inherited;

end;

procedure TAdvCustomScrollBar.HandleMouseMove(Shift: TShiftState; X, Y: Single);
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

function TAdvCustomScrollBar.MouseOnThumb(X, Y: Double): Boolean;
begin
  Result := PtInRectEx(GetThumbRectangle, PointF(X, Y));
end;

function TAdvCustomScrollBar.MouseOnThumbButtons(X, Y: Double): Boolean;
begin
  Result := PtInRectEx(GetMinThumbButton, PointF(X, Y)) or PtInRectEx(GetMaxThumbButton, PointF(X, Y));
  Result := Result and not Appearance.FixedThumb;
end;

procedure TAdvCustomScrollBar.ResetToDefaultStyle;
var
  ia: TAdvScrollBarAppearance;
begin
  inherited;
  ia := TAdvScrollBarAppearance.Create(nil);
  try
    Appearance.Assign(ia);
  finally
    ia.Free;
  end;
end;

procedure TAdvCustomScrollBar.HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
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

procedure TAdvCustomScrollBar.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if WheelDelta < 0 then
    Value := Value + LargeChange
  else
    Value := Value - LargeChange;
end;

function TAdvCustomScrollBar.IsLargeChangeStored: Boolean;
begin
  Result := LargeChange <> 20;
end;

function TAdvCustomScrollBar.IsMaxStored: Boolean;
begin
  Result := Max <> 100;
end;

function TAdvCustomScrollBar.IsMinStored: Boolean;
begin
  Result := Min <> 0;
end;

function TAdvCustomScrollBar.IsPageSizeStored: Boolean;
begin
  Result := PageSize <> 20;
end;

function TAdvCustomScrollBar.IsSmallChangeStored: Boolean;
begin
  Result := SmallChange <> 1;
end;

function TAdvCustomScrollBar.IsValueStored: Boolean;
begin
  Result := Value <> 0;
end;

procedure TAdvCustomScrollBar.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  inherited;
  DrawThumb(AGraphics);
  DrawScrollButtons(AGraphics);
end;

procedure TAdvCustomScrollBar.SetAppearance(
  const Value: TAdvScrollBarAppearance);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.SetKind(Value: TAdvScrollBarKind);
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

procedure TAdvCustomScrollBar.SetMax(const Value: Double);
begin
  if FMax <> value then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.SetMin(const Value: Double);
begin
  if FMin <> value then
  begin
    FMin := Value;
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.SetPageSize(const Value: Double);
begin
  if FPageSize <> value then
  begin
    FPageSize := Math.Min(GetRange, Math.Max(0, Value));
    if Assigned(FOnPageSizeChanged) then
      FOnPageSizeChanged(Self, PageSize);
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.SetTracking(const Value: Boolean);
begin
  if FTracking <> Value then
  begin
    FTracking := Value;
    Changed;
  end;
end;

procedure TAdvCustomScrollBar.SetValue(const Value: Double);
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

procedure TAdvCustomScrollBar.TimerChanged(Sender: TObject);
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

{ TAdvScrollBar }

procedure TAdvScrollBar.RegisterRuntimeClasses;
begin
  RegisterClass(TAdvScrollBar);
end;

end.
