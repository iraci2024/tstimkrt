{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit AdvCustomScrollControl;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, AdvCustomControl, StdCtrls, AdvScrollBar, AdvGraphics,
  AdvTypes, ExtCtrls
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,Controls
  {$ENDIF}
  ;

const
  {$IFDEF ANDROID}
  SCROLLINGDELAY = 40;
  {$ELSE}
  SCROLLINGDELAY = 0;
  {$ENDIF}
  {$IFDEF LCLLIB}
  SWIPECOUNT = 300;
  DOWNCOUNT = 15;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 15;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ELSE}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}

type
  TAdvCustomScrollControlMode = (scmPixelScrolling, scmItemScrolling);
  TAdvCustomScrollControlUpdate = (scuContinuous, scuOnce);

  TAdvCustomScrollControl = class(TAdvCustomControl)
  private
    FDownTime: Integer;
    FMouseUp, FAnimateVerticalPos, FAnimateHorizontalPos: Boolean;
    FAnimating: Boolean;
    FSpX, FSpY: Double;
    FDoubleSelection, FRangeSelection, FScrolling: Boolean;
    FScrollX, FScrollY, FDownX, FDownY, FMouseX, FMouseY: Double;
    FScrollVTo, FScrollHTo: Double;
    FTimeStart, FTimeStop: Double;
    FAnimateTimer, FDownTimer: TTimer;
    FSaveHScrollPos, FSaveVScrollPos: Single;
    FBlockScrollingUpdate, FIsMouseDown, FStretchScrollBars: Boolean;
    FUpdateCount: Integer;
    FVerticalScrollBar, FHorizontalScrollBar: TScrollBar;
    FCustomVerticalScrollBar, FCustomHorizontalScrollBar: TAdvScrollBar;
    FVerticalScrollBarVisible: Boolean;
    FHorizontalScrollBarVisible: Boolean;
    FScrollMode: TAdvCustomScrollControlMode;
    FApplyClipRect: Boolean;
    FScrollUpdate: TAdvCustomScrollControlUpdate;
    FCustomScrollBars: Boolean;
    FTouchScrolling: Boolean;
    procedure SetStretchScrollBars(const Value: Boolean);
    procedure SetScrollMode(const Value: TAdvCustomScrollControlMode);
    procedure SetApplyClipRect(const Value: Boolean);
    procedure SetScrollUpdate(const Value: TAdvCustomScrollControlUpdate);
    procedure SetCustomScrollBars(const Value: Boolean);
  protected
    function GetVersion: string; override;
    {$IFDEF WEBLIB}
    function GetCanvasHeightOffset: Integer; override;
    function GetCanvasWidthOffset: Integer; override;
    {$ENDIF}
    function ColumnStretchingActive: Boolean; virtual; abstract;
    procedure SetHorizontalScrollBarVisible(const Value: Boolean); virtual;
    procedure SetVerticalScrollBarVisible(const Value: Boolean); virtual;
    procedure Animate(Sender: TObject);
    procedure DownTime(Sender: TObject);
    procedure UpdateControlAfterResize; override;
    procedure FillChanged(Sender: TObject); reintroduce; virtual;
    procedure StrokeChanged(Sender: TObject); reintroduce; virtual;
    procedure Scroll(AHorizontalPos, AVerticalPos: Double);
    procedure UpdateControlScroll({%H-}AHorizontalPos, {%H-}AVerticalPos, {%H-}ANewHorizontalPos, {%H-}ANewVerticalPos: Double); virtual;
    procedure VerticalScrollPositionChanged; virtual;
    procedure HorizontalScrollPositionChanged; virtual;
    procedure VScrollChanged(Sender: TObject);
    procedure HScrollChanged(Sender: TObject);
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleSelection(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure HCustomScrollChanged(Sender: TObject; {%H-}AValue: Double);
    procedure VCustomScrollChanged(Sender: TObject; {%H-}AValue: Double);
    procedure UpdateControlScrollBarCalculations; virtual;
    procedure UpdateControlScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
    procedure UpdateControl({%H-}AUpdate: Boolean = True; {%H-}ARealign: Boolean = True); virtual;
    procedure UpdateControlCache; virtual;
    procedure UpdateControlDisplay; virtual;
    procedure StopAnimationTimer; virtual;
    procedure SetHScrollValue(AValue: Single); virtual;
    procedure SetVScrollValue(AValue: Single); virtual;
    procedure Loaded; override;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawContent({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure UpdateScrollingMode(AMode: TAdvCustomScrollControlMode); virtual;
    function GetTotalContentWidth: Double; virtual;
    function GetTotalContentHeight: Double; virtual;
    function GetHorizontalContentViewPortSize: Double; virtual;
    function GetVerticalContentViewPortSize: Double; virtual;
    function GetHorizontalContentCount: Integer; virtual;
    function GetVerticalContentCount: Integer; virtual;
    function GetCalculationRect: TRectF; virtual;
    function GetContentClipRect: TRectF; virtual;
    function GetContentRect: TRectF; override;
    function GetHScrollValue: Single; virtual;
    function GetVScrollValue: Single; virtual;
    function GetVViewPortSize: Single; virtual;
    function GetHViewPortSize: Single; virtual;
    function GetVMax: Single; virtual;
    function GetHMax: Single; virtual;
    function GetHVisible: Boolean; virtual;
    function GetVVisible: Boolean; virtual;
    function ProcessTouchScrolling(X, Y: Single): Boolean; virtual;
    property HorizontalScrollBarVisible: Boolean read FHorizontalScrollBarVisible write SetHorizontalScrollBarVisible default True;
    property VerticalScrollBarVisible: Boolean read FVerticalScrollBarVisible write SetVerticalScrollBarVisible default True;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property StretchScrollBars: Boolean read FStretchScrollBars write SetStretchScrollBars default True;
    property ScrollMode: TAdvCustomScrollControlMode read FScrollMode write SetScrollMode default scmPixelScrolling;
    property ScrollUpdate: TAdvCustomScrollControlUpdate read FScrollUpdate write SetScrollUpdate default scuContinuous;
    property CustomScrollBars: Boolean read FCustomScrollBars write SetCustomScrollBars default False;
    property Version: string read GetVersion;
    property ApplyClipRect: Boolean read FApplyClipRect write SetApplyClipRect default True;
    property TouchScrolling: Boolean read FTouchScrolling write FTouchScrolling default False;
    property Animating: Boolean read FAnimating;
    property IsScrolling: Boolean read FScrolling;
    property IsRangeSelection: Boolean read FRangeSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure SaveScrollPosition; virtual;
    procedure RestoreScrollPosition; virtual;
    function GetVerticalScrollPosition: Double; virtual;
    function GetHorizontalScrollPosition: Double; virtual;
    function GetHorizontalScrollMax: Double; virtual;
    function GetVerticalScrollMax: Double; virtual;
    function HorizontalScrollBar: TScrollBar;
    function VerticalScrollBar: TScrollBar;
    function CustomHorizontalScrollBar: TAdvScrollBar;
    function CustomVerticalScrollBar: TAdvScrollBar;
  end;

implementation

uses
  {$IFNDEF WEBLIB}
  Controls,
  {$ENDIF}
  Forms, Math,
  AdvGraphicsTypes, SysUtils;

function GetTickCountX: DWORD;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := ms + s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000;
end;

function AnimateDouble(var Start, Stop: Double; Delta, Margin: Double): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

{ TAdvCustomScrollControl }

{$IFDEF WEBLIB}
function TAdvCustomScrollControl.GetCanvasHeightOffset: Integer;
var
  hs: TScrollBar;
  hsc: TAdvScrollBar;
begin
  Result := 0;
  hs := HorizontalScrollBar;
  hsc := CustomHorizontalScrollBar;
  if Assigned(hs) and Assigned(hsc) then
  begin
    if CustomScrollBars and hsc.Visible and Assigned(hsc.Parent) then
      Result := hsc.Height
    else if not CustomScrollBars and hs.Visible and Assigned(hs.Parent) then
      Result := hs.Height;
  end;
end;

function TAdvCustomScrollControl.GetCanvasWidthOffset: Integer;
var
  vs: TScrollBar;
  vsc: TAdvScrollBar;
begin
  Result := 0;
  vs := VerticalScrollBar;
  vsc := CustomVerticalScrollBar;
  if Assigned(vs) and Assigned(vsc) then
  begin
    if CustomScrollBars and vsc.Visible and Assigned(vsc.Parent) then
      Result := vsc.Width
    else if not CustomScrollBars and vs.Visible and Assigned(vs.Parent) then
      Result := vs.Width;
  end;
end;
{$ENDIF}

function TAdvCustomScrollControl.GetVersion: string;
begin
  Result := '';
end;

procedure TAdvCustomScrollControl.Animate(Sender: TObject);
var
  dx, dy, posx, posy: Double;
  animh, animv: Boolean;
begin
  posy := GetVScrollValue;
  posx := GetHScrollValue;
  dx := Abs(FScrollHTo - posx) / Max(1, Abs(FSpX) * 6);
  dy := Abs(FScrollVTo - posy) / Max(1, Abs(FSpY) * 6);
  animv := False;
  if FAnimateVerticalPos then
    animv := AnimateDouble(posy, FScrollVTo, dy, 0.01);

  animh := False;
  if FAnimateHorizontalPos then
    animh := AnimateDouble(posx, FScrollHTo, dx, 0.01);

  FAnimating := animv or animh;
  if FAnimating then
    Scroll(posx, posy)
  else
  begin
    FAnimateVerticalPos := False;
    FAnimateTimer.Enabled := False;
    FAnimateHorizontalPos := False;
  end;
end;

procedure TAdvCustomScrollControl.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomScrollControl then
  begin
    FStretchScrollBars := (Source as TAdvCustomScrollControl).StretchScrollBars;
    FScrollMode := (Source as TAdvCustomScrollControl).ScrollMode;
    FScrollUpdate := (Source as TAdvCustomScrollControl).ScrollUpdate;
    FApplyClipRect := (Source as TAdvCustomScrollControl).ApplyClipRect;
  end;
end;

procedure TAdvCustomScrollControl.RestoreScrollPosition;
begin
  Scroll(FSaveHScrollPos, FSaveVScrollPos);
end;

procedure TAdvCustomScrollControl.SaveScrollPosition;
begin
  FSaveHScrollPos := GetHScrollValue;
  FSaveVScrollPos := GetVScrollValue;
end;

procedure TAdvCustomScrollControl.BeginUpdate;
begin
  inherited BeginUpdate;
  Inc(FUpdateCount);
end;

constructor TAdvCustomScrollControl.Create(AOwner: TComponent);
begin
  inherited;
  FTouchScrolling := False;

  FAnimateTimer := TTimer.Create(Self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.Enabled := False;
  FAnimateTimer.OnTimer := Animate;

  FDownTimer := TTimer.Create(Self);
  FDownTimer.Interval := 1;
  FDownTimer.Enabled := False;
  FDownTimer.OnTimer := DownTime;

  FApplyClipRect := True;
  FStretchScrollBars := True;
  FScrollMode := scmPixelScrolling;
  FScrollUpdate := scuContinuous;

  FCustomVerticalScrollBar := TAdvScrollBar.Create(Self);
  FCustomVerticalScrollBar.Parent := Self;
  FCustomHorizontalScrollBar := TAdvScrollBar.Create(Self);
  FCustomHorizontalScrollBar.Parent := Self;

  FVerticalScrollBar := TScrollBar.Create(Self);
  FVerticalScrollBar.Parent := Self;
  FHorizontalScrollBar := TScrollBar.Create(Self);
  FHorizontalScrollBar.Parent := Self;

  FCustomVerticalScrollBar.Appearance.ThumbButtonSize := 0;
  FCustomHorizontalScrollBar.Appearance.ThumbButtonSize := 0;

  {$IFDEF FMXLIB}
  FVerticalScrollBar.Stored := False;
  FHorizontalScrollBar.Stored := False;
  FCustomVerticalScrollBar.Stored := False;
  FCustomHorizontalScrollBar.Stored := False;
  FVerticalScrollBar.Orientation := TOrientation.Vertical;
  {$ENDIF}

  FCustomHorizontalScrollBar.Kind := sbkHorizontal;

  {$IFDEF CMNWEBLIB}
  FVerticalScrollBar.DoubleBuffered := False;
  FHorizontalScrollBar.DoubleBuffered := False;
  FVerticalScrollBar.Kind := sbVertical;
  {$ENDIF}

  FVerticalScrollBar.Visible := True;
  FHorizontalScrollBar.Visible := True;
  FCustomVerticalScrollBar.Visible := False;
  FCustomHorizontalScrollBar.Visible := False;
  FVerticalScrollBar.OnChange := VScrollChanged;
  FHorizontalScrollBar.OnChange := HScrollChanged;
  FCustomHorizontalScrollBar.OnValueChanged := HCustomScrollChanged;
  FCustomVerticalScrollBar.OnValueChanged := VCustomScrollChanged;

  FVerticalScrollBarVisible := True;
  FHorizontalScrollBarVisible := True;
end;

destructor TAdvCustomScrollControl.Destroy;
begin
  FCustomVerticalScrollBar.Free;
  FCustomHorizontalScrollBar.Free;
  FVerticalScrollBar.Free;
  FHorizontalScrollBar.Free;
  inherited;
end;

procedure TAdvCustomScrollControl.DownTime(Sender: TObject);
begin
  Inc(FDownTime);
  if (FDownTime = DOWNCOUNT) or not TouchScrolling then
  begin
    FRangeSelection := True;
    FDownTimer.Enabled := False;
  end;
end;

function TAdvCustomScrollControl.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  if CustomScrollBars then
  begin
    if CustomHorizontalScrollBar.Visible then
      Result.Bottom := Result.Bottom - CustomHorizontalScrollBar.Height - 1;

    if CustomVerticalScrollBar.Visible then
      Result.Right := Result.Right - CustomVerticalScrollBar.Width - 1;
  end
  else
  begin
    if HorizontalScrollBar.Visible then
      Result.Bottom := Result.Bottom - HorizontalScrollBar.Height - 1;

    if VerticalScrollBar.Visible then
      Result.Right := Result.Right - VerticalScrollBar.Width - 1;
  end;
end;

function TAdvCustomScrollControl.GetContentClipRect: TRectF;
begin
  Result := GetContentRect;
end;

function TAdvCustomScrollControl.GetHorizontalScrollMax: Double;
begin
  Result := GetHMax;
end;

function TAdvCustomScrollControl.GetVerticalScrollMax: Double;
begin
  Result := GetVMax;
end;

function TAdvCustomScrollControl.GetHorizontalScrollPosition: Double;
var
  hVal: Double;
begin
  hVal := GetHScrollValue;
  Result := hVal;
end;

function TAdvCustomScrollControl.GetHScrollValue: Single;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.Value
  else
  begin
    {$IFDEF FMXLIB}
    Result := Min(HorizontalScrollBar.Max - HorizontalScrollBar.ViewportSize, Max(0, HorizontalScrollBar.Value));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFNDEF LCLLIB}
    Result := Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(HorizontalScrollBar.Position)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    Result := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(HorizontalScrollBar.Position)));
    {$ELSE}
    case ScrollMode of
      scmPixelScrolling: Result := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(HorizontalScrollBar.Position - HorizontalScrollBar.Position * (HorizontalScrollBar.PageSize / HorizontalScrollBar.Max))));
      scmItemScrolling: Result := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(HorizontalScrollBar.Position)));
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

function TAdvCustomScrollControl.GetHMax: Single;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.Max
  else
    Result := HorizontalScrollBar.Max;
end;

function TAdvCustomScrollControl.GetVMax: Single;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.Max
  else
    Result := VerticalScrollBar.Max
end;

function TAdvCustomScrollControl.GetHViewPortSize: Single;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.PageSize
  else
  begin
    {$IFDEF FMXLIB}
    Result := HorizontalScrollBar.ViewportSize;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result := HorizontalScrollBar.PageSize;
    {$ENDIF}
  end;
end;

function TAdvCustomScrollControl.GetHVisible: Boolean;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.Visible
  else
    Result := HorizontalScrollBar.Visible;
end;

function TAdvCustomScrollControl.GetVerticalScrollPosition: Double;
var
  vVal: Double;
begin
  vVal := GetVScrollValue;
  Result := vVal;
end;

function TAdvCustomScrollControl.GetVViewPortSize: Single;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.PageSize
  else
  begin
    {$IFDEF FMXLIB}
    Result := VerticalScrollBar.ViewportSize;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result := VerticalScrollBar.PageSize;
    {$ENDIF}
  end;
end;

function TAdvCustomScrollControl.GetVVisible: Boolean;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.Visible
  else
    Result := VerticalScrollBar.Visible;
end;

function TAdvCustomScrollControl.GetVScrollValue: Single;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.Value
  else
  begin
    {$IFDEF FMXLIB}
    Result := Min(VerticalScrollBar.Max - VerticalScrollBar.ViewportSize, Max(0, VerticalScrollBar.Value));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFNDEF LCLLIB}
    Result := Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(VerticalScrollBar.Position)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    Result := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(VerticalScrollBar.Position)));
    {$ELSE}
    case ScrollMode of
      scmPixelScrolling: Result := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(VerticalScrollBar.Position - VerticalScrollBar.Position * (VerticalScrollBar.PageSize / VerticalScrollBar.Max))));
      scmItemScrolling: Result := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(VerticalScrollBar.Position)));
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure TAdvCustomScrollControl.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;

  CaptureEx;

  if not ProcessTouchScrolling(X, Y) then
    Exit;

  IsMouseDown := True;
  FTimeStart := GetTickCountX;
  FTimeStop := FTimeStart;
  FScrollVTo := GetVScrollValue;
  FScrollHTo := GetHScrollValue;
  FScrollX := X;
  FScrollY := Y;
  FDownX := X;
  FDownY := Y;
  FMouseX := X;
  FMouseY := Y;
  FMouseUp := False;
  FDownTimer.Enabled := True;
  FDoubleSelection := not FAnimateTimer.Enabled;
  FRangeSelection := False;
  FScrolling := False;
  FDownTime := 0;
end;

procedure TAdvCustomScrollControl.HandleMouseMove(Shift: TShiftState; X,
  Y: Single);
var
  f: Double;
  doscroll: Boolean;
begin
  inherited;
  if not ProcessTouchScrolling(X, Y) then
    Exit;

  if IsMouseDown then
  begin
    doscroll := True;
    if doscroll then
    begin
      f := 1;
      if (FScrolling or (Abs(FMouseX - X) > 3) or (Abs(FMouseY - Y) > 3)) and not FRangeSelection and TouchScrolling then
      begin
        if (Abs(X - FDownX) > SCROLLINGDELAY) or (Abs(Y - FDownY) > SCROLLINGDELAY) then
        begin
          FScrolling := True;
          FDownTimer.Enabled := False;
          FDoubleSelection := False;
          if IsMouseDown and not FMouseUp then
          begin
            Scroll(GetHScrollValue - (X - FDownX) * f, GetVScrollValue - (Y - FDownY) * f);
            FDownY := Y;
            FDownX := X;
          end;
        end;
      end;
    end
    else if FRangeSelection then
    begin

    end;
  end;
end;

procedure TAdvCustomScrollControl.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  f: Double;
begin
  inherited;

  ReleaseCaptureEx;


  if not ProcessTouchScrolling(X, Y) then
    Exit;

  if not IsMouseDown then
    Exit;

  f := 1;

  IsMouseDown := False;
  FMouseUp := True;
  FScrolling := False;
  FDownTimer.Enabled := False;

  if not FDoubleSelection and TouchScrolling then
  begin
    FTimeStop := GetTickCountX;
    if ((FTimeStop - FTimeStart) < SWIPECOUNT) and ((FTimeStop - FTimeStart) > 0) then
    begin
      FSpY := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
      if (FSpY > 0) then
      begin
        if (Y - FScrollY) > 0 then
          FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewPortSize, FScrollVTo - Round(Abs(Y - FScrollY) * FSpY * f * 3)))
        else
          FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewPortSize, FScrollVTo + Round(Abs(Y - FScrollY) * FSpY * f * 3)));

        FAnimateVerticalPos := True;
        FAnimateTimer.Enabled := True;
      end;

      FSpX := Abs(X - FScrollX) / (FTimeStop - FTimeStart);
      if (FSpX > 0) then
      begin
        if (X - FScrollX) > 0 then
          FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewPortSize, FScrollHTo - Round(Abs(X - FScrollX) * FSpX * f * 3)))
        else
          FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewPortSize, FScrollHTo + Round(Abs(X - FScrollX) * FSpX * f * 3)));

        FAnimateHorizontalPos := True;
        FAnimateTimer.Enabled := True;
      end;
    end;
  end
  else
    HandleSelection(Button, Shift, X, Y);

  FRangeSelection := False;
end;

procedure TAdvCustomScrollControl.HandleSelection(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TAdvCustomScrollControl.HCustomScrollChanged(Sender: TObject;
  AValue: Double);
begin
  if FBlockScrollingUpdate or not CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  HorizontalScrollPositionChanged;
  SetHScrollValue(GetHScrollValue);
end;

function TAdvCustomScrollControl.HorizontalScrollBar: TScrollBar;
begin
  Result := FHorizontalScrollBar;
end;

function TAdvCustomScrollControl.CustomHorizontalScrollBar: TAdvScrollBar;
begin
  Result := FCustomHorizontalScrollBar;
end;

procedure TAdvCustomScrollControl.UpdateControl(AUpdate: Boolean = True; ARealign: Boolean = True);
begin
  UpdateControlCache;
  UpdateControlScrollBars;
  UpdateControlDisplay;
end;

procedure TAdvCustomScrollControl.UpdateControlAfterResize;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  SaveScrollPosition;
  UpdateControl(False);
  RestoreScrollPosition;
end;

procedure TAdvCustomScrollControl.UpdateControlScroll(AHorizontalPos: Double; AVerticalPos: Double; ANewHorizontalPos: Double; ANewVerticalPos: Double);
begin

end;

procedure TAdvCustomScrollControl.VCustomScrollChanged(Sender: TObject;
  AValue: Double);
begin
  if FBlockScrollingUpdate or not CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  VerticalScrollPositionChanged;
  SetVScrollValue(GetVScrollValue);
end;

function TAdvCustomScrollControl.VerticalScrollBar: TScrollBar;
begin
  Result := FVerticalScrollBar;
end;

function TAdvCustomScrollControl.CustomVerticalScrollBar: TAdvScrollBar;
begin
  Result := FCustomVerticalScrollBar;
end;

procedure TAdvCustomScrollControl.VerticalScrollPositionChanged;
begin

end;


procedure TAdvCustomScrollControl.HorizontalScrollPositionChanged;
begin

end;

procedure TAdvCustomScrollControl.HScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate or CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  HorizontalScrollPositionChanged;
  SetHScrollValue(GetHScrollValue);
end;

procedure TAdvCustomScrollControl.VScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate or CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  VerticalScrollPositionChanged;
  SetVScrollValue(GetVScrollValue);
end;

procedure TAdvCustomScrollControl.UpdateControlCache;
begin

end;

procedure TAdvCustomScrollControl.SetApplyClipRect(const Value: Boolean);
begin
  if FApplyClipRect <> Value then
  begin
    FApplyClipRect := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomScrollControl.SetCustomScrollBars(const Value: Boolean);
begin
  if FCustomScrollBars <> Value then
  begin
    FCustomScrollBars := Value;
    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomScrollControl.StrokeChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomScrollControl.UpdateControlDisplay;
begin

end;

procedure TAdvCustomScrollControl.UpdateControlScrollBarCalculations;
begin

end;

procedure TAdvCustomScrollControl.StopAnimationTimer;
begin
  FAnimateTimer.Enabled := False;
  FAnimating := False;
end;

function TAdvCustomScrollControl.GetTotalContentWidth: Double;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetTotalContentHeight: Double;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetHorizontalContentViewPortSize: Double;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetVerticalContentViewPortSize: Double;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetHorizontalContentCount: Integer;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetVerticalContentCount: Integer;
begin
  Result := 0;
end;

function TAdvCustomScrollControl.GetCalculationRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TAdvCustomScrollControl.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateControl;
end;

procedure TAdvCustomScrollControl.Loaded;
begin
  inherited;
  UpdateControl;
end;

function TAdvCustomScrollControl.ProcessTouchScrolling(X,
  Y: Single): Boolean;
begin
  Result := TouchScrolling;
end;

procedure TAdvCustomScrollControl.Scroll(AHorizontalPos, AVerticalPos: Double);
var
  AOldHorizontalPos, AOldVerticalPos: Double;
begin
  FBlockScrollingUpdate := True;
  AOldHorizontalPos := GetHScrollValue;
  AOldVerticalPos := GetVScrollValue;
  SetHScrollValue(AHorizontalPos);
  SetVScrollValue(AVerticalPos);
  FBlockScrollingUpdate := False;
  UpdateControlScroll(AOldHorizontalPos, AOldVerticalPos, AHorizontalPos, AVerticalPos);
end;

procedure TAdvCustomScrollControl.SetHorizontalScrollBarVisible(
  const Value: Boolean);
begin
  if FHorizontalScrollBarVisible <> Value then
  begin
    FHorizontalScrollBarVisible := Value;
    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.SetHScrollValue(AValue: Single);
begin
  if CustomScrollBars then
    CustomHorizontalScrollBar.Value := AValue
  else
  begin
    {$IFDEF FMXLIB}
    if HorizontalScrollBar.ViewportSize > 0 then
      HorizontalScrollBar.Value := Min(HorizontalScrollBar.Max - HorizontalScrollBar.ViewportSize, Max(0, AValue));
    {$ENDIF}
    {$IFDEF VCLLIB}
    if HorizontalScrollBar.PageSize > 0 then
      HorizontalScrollBar.Position := Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ENDIF}
    {$IFDEF WEBLIB}
    if HorizontalScrollBar.PageSize > 0 then
      HorizontalScrollBar.Position := Round(Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, AValue)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    if HorizontalScrollBar.PageSize > 0 then
      HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ELSE}
    case ScrollMode of
      scmPixelScrolling: HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(AValue + AValue * (HorizontalScrollBar.PageSize / (HorizontalScrollBar.Max - HorizontalScrollBar.PageSize)))));
      scmItemScrolling: HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    end;
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure TAdvCustomScrollControl.SetScrollMode(
  const Value: TAdvCustomScrollControlMode);
begin
  if FScrollMode <> Value then
  begin
    FScrollMode := Value;
    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.SetScrollUpdate(
  const Value: TAdvCustomScrollControlUpdate);
begin
  if FScrollUpdate <> Value then
  begin
    FScrollUpdate := Value;
    if Assigned(FCustomVerticalScrollBar) then
      FCustomVerticalScrollBar.Tracking := ScrollUpdate = scuContinuous;

    if Assigned(FCustomHorizontalScrollBar) then
      FCustomHorizontalScrollBar.Tracking := ScrollUpdate = scuContinuous;

    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.SetStretchScrollBars(const Value: Boolean);
begin
  if FStretchScrollBars <> Value then
  begin
    FStretchScrollBars := Value;
    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.SetVerticalScrollBarVisible(
  const Value: Boolean);
begin
  if FVerticalScrollBarVisible <> Value then
  begin
    FVerticalScrollBarVisible := Value;
    UpdateControl;
  end;
end;

procedure TAdvCustomScrollControl.SetVScrollValue(AValue: Single);
begin
  if CustomScrollBars then
    CustomVerticalScrollBar.Value := AValue
  else
  begin
    {$IFDEF FMXLIB}
    if VerticalScrollBar.ViewportSize > 0 then
      VerticalScrollBar.Value := Min(VerticalScrollBar.Max - VerticalScrollBar.ViewportSize, Max(0, AValue));
    {$ENDIF}
    {$IFDEF VCLLIB}
    if VerticalScrollBar.PageSize > 0 then
      VerticalScrollBar.Position := Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ENDIF}
    {$IFDEF WEBLIB}
    if VerticalScrollBar.PageSize > 0 then
      VerticalScrollBar.Position := Round(Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, AValue)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    if VerticalScrollBar.PageSize > 0 then
      VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ELSE}
    case ScrollMode of
      scmPixelScrolling: VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(AValue + AValue * (VerticalScrollBar.PageSize / (VerticalScrollBar.Max - VerticalScrollBar.PageSize)))));
      scmItemScrolling: VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    end;
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure TAdvCustomScrollControl.UpdateControlScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
var
  vs, hs: TScrollBar;
  vsc, hsc: TAdvScrollBar;
  w, h: Double;
  cw, ch: Double;
  cr: TRectF;
  vmgr: TAdvMargins;
  hmgr: TAdvMargins;
begin
  if {$IFDEF FMXLIB} FBlockScrollingUpdate or {$ENDIF} (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  FBlockScrollingUpdate := True;

  if ACalculate then
  begin
    UpdateControlScrollBarCalculations;
  end;

  vs := VerticalScrollBar;
  hs := HorizontalScrollBar;
  vsc := CustomVerticalScrollBar;
  hsc := CustomHorizontalScrollBar;
  if Assigned(vs) and Assigned(hs) and Assigned(vsc) and Assigned(hsc) then
  begin
    if ACalculate then
      cr := GetCalculationRect
    else
      cr := GetContentRect;

    cw := cr.Right - cr.Left;
    ch := cr.Bottom - cr.Top;
    w := GetTotalContentWidth;
    h := GetTotalContentHeight;

    hs.Visible := (w > 0) and (CompareValueEx(w, cw) = 1) and HorizontalScrollBarVisible and not ColumnStretchingActive and not CustomScrollBars;
    vs.Visible := (h > 0) and (CompareValueEx(h, ch) = 1) and VerticalScrollBarVisible and not CustomScrollBars;
    hsc.Visible := (w > 0) and (CompareValueEx(w, cw) = 1) and HorizontalScrollBarVisible and not ColumnStretchingActive and CustomScrollBars;
    vsc.Visible := (h > 0) and (CompareValueEx(h, ch) = 1) and VerticalScrollBarVisible and CustomScrollBars;

    {$IFDEF CMNWEBLIB}
    if vsc.Visible and CustomScrollBars then
      vsc.Parent := Self
    else
      vsc.Parent := nil;

    if hsc.Visible and CustomScrollBars then
      hsc.Parent := Self
    else
      hsc.Parent := nil;

    if vs.Visible and not CustomScrollBars then
      vs.Parent := Self
    else
      vs.Parent := nil;

    if hs.Visible and not CustomScrollBars then
      hs.Parent := Self
    else
      hs.Parent := nil;
    {$ENDIF}

    vmgr := TAdvMargins.Create;
    hmgr := TAdvMargins.Create;

    if CustomScrollBars then
    begin
      if vsc.Visible and StretchScrollBars then
        hmgr.Right := vsc.Width + 1
      else
        hmgr.Right := 1;
    end
    else
    begin
      if vs.Visible and StretchScrollBars then
        hmgr.Right := vs.Width + 1
      else
        hmgr.Right := 1;
    end;

    vmgr.Top := 1;
    vmgr.Right := 1;
    hmgr.Bottom := 1;
    hmgr.Left := 1;

    if CustomScrollBars then
    begin
      if hsc.Visible then
        vmgr.Bottom := hsc.Height + 1
      else
        vmgr.Bottom := 1;
    end
    else
    begin
      if hs.Visible then
        vmgr.Bottom := hs.Height + 1
      else
        vmgr.Bottom := 1;
    end;

    if not StretchScrollBars then
    begin
      cr := GetCalculationRect;
      hmgr.Left := hmgr.Left + cr.Left;
      hmgr.Right := hmgr.Right + (Width - cr.Right);
      vmgr.Top := vmgr.Top + cr.Top;
      vmgr.Bottom := vmgr.Bottom + (Height - cr.Bottom);
    end;

    {$IFDEF FMXLIB}
    hs.Position.X := hmgr.Left;
    hs.Position.Y := Height - hmgr.Bottom - hs.Height;
    vs.Position.X := Width - vmgr.Right - vs.Width;
    vs.Position.Y := vmgr.Top;
    hs.Width := Width - hmgr.Right - hmgr.Left;
    vs.Height := Height - vmgr.Bottom - vmgr.Top;
    hsc.Position.X := hmgr.Left;
    hsc.Position.Y := Height - hmgr.Bottom - hsc.Height;
    vsc.Position.X := Width - vmgr.Right - vsc.Width;
    vsc.Position.Y := vmgr.Top;
    hsc.Width := Width - hmgr.Right - hmgr.Left;
    vsc.Height := Height - vmgr.Bottom - vmgr.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    hs.Left := Round(hmgr.Left);
    hs.Top := Round(Height - hmgr.Bottom - hs.Height);
    vs.Left := Round(Width - vmgr.Right - vs.Width);
    vs.Top := Round(vmgr.Top);
    hs.Width := Round(Max(0, Width - hmgr.Right - hmgr.Left));
    {$IFDEF MSWINDOWS}
    vs.Width := GetSystemMetrics(SM_CYVSCROLL);
    hs.Height := GetSystemMetrics(SM_CYHSCROLL);
    {$ENDIF}
    vs.Height := Round(Max(0, Height - vmgr.Bottom - vmgr.Top));
    hsc.Left := Round(hmgr.Left);
    hsc.Top := Round(Height - hmgr.Bottom - hsc.Height);
    vsc.Left := Round(Width - vmgr.Right - vsc.Width);
    vsc.Top := Round(vmgr.Top);
    hsc.Width := Round(Max(0, Width - hmgr.Right - hmgr.Left));
    vsc.Height := Round(Max(0, Height - vmgr.Bottom - vmgr.Top));
    {$ENDIF}

    hmgr.Free;
    vmgr.Free;

    cr := GetContentRect;
    cw := cr.Right - cr.Left;
    ch := cr.Bottom - cr.Top;

    {$IFDEF FMXLIB}
    if ScrollMode = scmItemScrolling then
    begin
      vs.ViewportSize := GetVerticalContentViewPortSize;
      vs.Max := GetVerticalContentCount;
    end
    else
    begin
      vs.ViewPortSize := Min(h, ch);
      vs.Max := h;
      vs.Value := Min(vs.Value, vs.Max - vs.ViewportSize);
    end;

    if ScrollMode = scmItemScrolling then
    begin
      hs.ViewportSize := GetHorizontalContentViewPortSize;
      hs.Max := GetHorizontalContentCount;
    end
    else
    begin
      hs.ViewPortSize := Min(w, cw);
      hs.Max := w;
      hs.Value := Min(hs.Value, hs.Max - hs.ViewportSize);
    end;

    if ScrollMode = scmItemScrolling then
    begin
      vsc.PageSize := GetVerticalContentViewPortSize;
      vsc.Max := GetVerticalContentCount - vsc.PageSize;
    end
    else
    begin
      vsc.PageSize := Min(h, ch);
      vsc.Max := h - ch;
    end;

    vsc.SmallChange := vsc.PageSize / 5;
    vsc.LargeChange := vsc.PageSize;

    if ScrollMode = scmItemScrolling then
    begin
      hsc.PageSize := GetHorizontalContentViewPortSize;
      hsc.Max := GetHorizontalContentCount - hsc.PageSize;
    end
    else
    begin
      hsc.PageSize := Min(w, cw);
      hsc.Max := w - cw;
    end;

    hsc.SmallChange := hsc.PageSize / 5;
    hsc.LargeChange := hsc.PageSize;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    if ScrollMode = scmItemScrolling then
    begin
      vs.PageSize := Round(Max(0, GetVerticalContentViewPortSize));
      vs.Max := Max(vs.PageSize, GetVerticalContentCount);
    end
    else
    begin
      vs.PageSize := Round(Max(0, Min(h, ch)));
      vs.Max := Round(Max(vs.PageSize, h));
      vs.Position := Min(vs.Position, vs.Max);
    end;

    vs.SmallChange := Max(Low(TScrollBarInc), vs.PageSize div 5);
    vs.LargeChange := Max(Low(TScrollBarInc), vs.PageSize);

    if ScrollMode = scmItemScrolling then
    begin
      hs.PageSize := Round(Max(0, GetHorizontalContentViewPortSize));
      hs.Max := Max(hs.PageSize, GetHorizontalContentCount);
    end
    else
    begin
      hs.PageSize := Round(Max(0, Min(w, cw)));
      hs.Max := Round(Max(hs.PageSize, w));
      hs.Position := Min(hs.Position, hs.Max);
    end;

    hs.SmallChange := Max(Low(TScrollBarInc), hs.PageSize div 5);
    hs.LargeChange := Max(Low(TScrollBarInc), hs.PageSize);

    if ScrollMode = scmItemScrolling then
    begin
      vsc.PageSize := Max(0, Round(GetVerticalContentViewPortSize));
      vsc.Max := GetVerticalContentCount - vsc.PageSize;
    end
    else
    begin
      vsc.PageSize := Round(Max(0, Min(h, ch)));
      vsc.Max := Round(h - ch);
    end;

    vsc.SmallChange := vsc.PageSize / 5;
    vsc.LargeChange := vsc.PageSize;

    if ScrollMode = scmItemScrolling then
    begin
      hsc.PageSize := Max(0, Round(GetHorizontalContentViewPortSize));
      hsc.Max := GetHorizontalContentCount - hsc.PageSize;
    end
    else
    begin
      hsc.PageSize := Round(Max(0, Min(w, cw)));
      hsc.Max := Round(w - cw);
    end;

    hsc.SmallChange := hsc.PageSize / 5;
    hsc.LargeChange := hsc.PageSize;
    {$ENDIF}
  end;

  FBlockScrollingUpdate := False;

  if AUpdate then
    UpdateControlScrollBars(False, False);
end;

procedure TAdvCustomScrollControl.UpdateScrollingMode(
  AMode: TAdvCustomScrollControlMode);
begin
  FScrollMode := AMode;
end;

procedure TAdvCustomScrollControl.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  st: TAdvGraphicsSaveState;
  cr: TRectF;
begin
  inherited;
  cr := GetContentClipRect;
  st := nil;
  if ApplyClipRect then
  begin
    st := AGraphics.SaveState;
    AGraphics.ClipRect(cr);
  end;

  DrawContent(AGraphics, cr);

  if ApplyClipRect then
    AGraphics.RestoreState(st);
end;

procedure TAdvCustomScrollControl.DrawContent(AGraphics: TAdvGraphics; ARect: TRectF);
begin

end;

end.
