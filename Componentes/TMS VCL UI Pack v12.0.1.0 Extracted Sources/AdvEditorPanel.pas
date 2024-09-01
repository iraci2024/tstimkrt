{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
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

unit AdvEditorPanel;

{$I TMSDEFS.INC}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types , AdvTypes, Controls, StdCtrls
  , AdvCustomControl, AdvGraphics, AdvGraphicsTypes

  {$IFDEF VCLLIB}
  , Winapi.Messages, Vcl.ActnList
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
  TAdvCustomEditorPanel = class;
  TAdvEditorPanelPosition = (eppAlone, eppLeft, eppTop, eppCenter, eppRight, eppBottom);

  TAdvEditorPanelAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomEditorPanel;
    FFill: TAdvGraphicsFill;
    FSelectedFill: TAdvGraphicsFill;
    FSelectedStroke: TAdvGraphicsStroke;
    FStroke: TAdvGraphicsStroke;
    FRounding: Integer;
    FOnChanged: TNotifyEvent;
    FStrokeSides: TAdvGraphicsSides;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetRounding(const Value: Integer);
    procedure SetStrokeSides(const Value: TAdvGraphicsSides);
  protected
    procedure DoChanged(Sender: TObject);
    procedure DoFillChanged(Sender: TObject); virtual;
    procedure DoStrokeChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TAdvCustomEditorPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property SelectedFill: TAdvGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TAdvGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property Rounding: Integer read FRounding write SetRounding;
    property StrokeSides: TAdvGraphicsSides read FStrokeSides write SetStrokeSides;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAdvCustomEditorPanel = class(TAdvCustomControl)
  private
    FAppearance: TAdvEditorPanelAppearance;
    FPanelPosition: TAdvEditorPanelPosition;
    FPanelPositionCorners: TAdvGraphicsCorners;
    FSelected: Boolean;
    procedure SetAppearance(const Value: TAdvEditorPanelAppearance);
    procedure SetPanelPosition(const Value: TAdvEditorPanelPosition);
    procedure SetPanelPositionCorners;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBackground(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure UpdateControlAfterResize; override;
    property Appearance: TAdvEditorPanelAppearance read FAppearance write SetAppearance;
    property PanelPosition: TAdvEditorPanelPosition read FPanelPosition write SetPanelPosition default eppAlone;
    property Selected: Boolean read FSelected write SetSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvEditorPanel = class(TAdvCustomEditorPanel)
  published
    property Appearance;
    property PanelPosition;
    property Selected;
  end;

implementation

uses
  TypInfo, Forms, SysUtils, AdvUtils, Math
  {$IFDEF VCLLIB}
  , Winapi.Windows
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLIB.Graphics
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Graphics
  {$ENDIF}
  ;

{ TAdvEditorListView }

procedure TAdvCustomEditorPanel.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TAdvCustomEditorPanel.BeginUpdate;
begin
  inherited;
end;

procedure TAdvCustomEditorPanel.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  BeginUpdate;
  FAppearance.Rounding := TAdvUtils.MulDivInt(FAppearance.Rounding, M, D);
  EndUpdate;
end;

constructor TAdvCustomEditorPanel.Create(AOwner: TComponent);
begin
  inherited;

  Fill.Kind := gfkNone;
  Stroke.Kind := gskNone;

  FAppearance := TAdvEditorPanelAppearance.Create(Self);
  FAppearance.OnChanged := DoAppearanceChanged;

  FPanelPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FPanelPosition := eppAlone;

  Width := 200;
  Height := 200;
end;

destructor TAdvCustomEditorPanel.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvCustomEditorPanel.DoAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomEditorPanel.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  drect: TRectF;
  rounding: integer;
begin
  if not Selected then
  begin
    AGraphics.Fill.Assign(FAppearance.Fill);
	AGraphics.Stroke.Assign(FAppearance.Stroke);
  end
  else
  begin
    AGraphics.Fill.Assign(FAppearance.SelectedFill);
	AGraphics.Stroke.Assign(FAppearance.SelectedStroke);
  end;

  drect := ARect;
  rounding := Min(FAppearance.Rounding, Round(Height/2));

  AGraphics.Stroke.Kind := gskNone;
  AGraphics.DrawRoundRectangle(drect, rounding, FPanelPositionCorners);

//  if not Selected then
//    AGraphics.Stroke.Assign(FAppearance.Stroke)
//  else
//    AGraphics.Stroke.Assign(FAppearance.SelectedStroke);
//
//  sw := FAppearance.Stroke.Width;
//
//  if gsLeft in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Left, ARect.Bottom));
//  if gsRight in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Right - sw, ARect.Top), PointF(ARect.Right - sw, ARect.Bottom));
//  if gsTop in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Right, ARect.Top));
//  if gsBottom in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Bottom - sw), PointF(ARect.Right, ARect.Bottom - sw));
end;

procedure TAdvCustomEditorPanel.DrawBackground(AGraphics: TAdvGraphics; ARect: TRectF);
begin
//    inherited;
end;

procedure TAdvCustomEditorPanel.EndUpdate;
begin
  inherited;
  Invalidate;
end;

procedure TAdvCustomEditorPanel.SetPanelPositionCorners;
begin
  case FPanelPosition of
    eppLeft: FPanelPositionCorners := [gcTopLeft, gcBottomLeft];
    eppCenter: FPanelPositionCorners := [];
    eppRight: FPanelPositionCorners := [gcTopRight, gcBottomRight];
    eppTop: FPanelPositionCorners := [gcTopLeft, gcTopRight];
    eppBottom: FPanelPositionCorners := [gcBottomLeft, gcBottomRight];
    else
      FPanelPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  end;
end;

procedure TAdvCustomEditorPanel.SetAppearance(const Value: TAdvEditorPanelAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomEditorPanel.SetPanelPosition(const Value: TAdvEditorPanelPosition);
begin
  if FPanelPosition <> Value then
  begin
    FPanelPosition := Value;
    SetPanelPositionCorners;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorPanel.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorPanel.UpdateControlAfterResize;
begin
  inherited;
end;

{ TAdvEditorPanelAppearance }

procedure TAdvEditorPanelAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvEditorPanelAppearance then
  begin
    FRounding := (Source as TAdvEditorPanelAppearance).Rounding;
    FFill.Assign((Source as TAdvEditorPanelAppearance).Fill);
    FStroke.Assign((Source as TAdvEditorPanelAppearance).Stroke);
    FSelectedFill.Assign((Source as TAdvEditorPanelAppearance).SelectedFill);
    FSelectedStroke.Assign((Source as TAdvEditorPanelAppearance).SelectedStroke);
  end
  else
    inherited;
end;

constructor TAdvEditorPanelAppearance.Create(AOwner: TAdvCustomEditorPanel);
begin
  FOwner := AOwner;
  FRounding := 0;

  FStrokeSides := [gsLeft, gsRight, gsTop, gsBottom];

  FFill := TAdvGraphicsFill.Create;
  FStroke := TAdvGraphicsStroke.Create;
  FSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));

  FFill.OnChanged := DoFillChanged;
  FStroke.OnChanged := DoStrokeChanged;
  FSelectedFill.OnChanged := DoFillChanged;
  FSelectedStroke.OnChanged := DoStrokeChanged;
end;

destructor TAdvEditorPanelAppearance.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FSelectedFill.Free;
  FSelectedStroke.Free;
  inherited;
end;

procedure TAdvEditorPanelAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvEditorPanelAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorPanelAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorPanelAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorPanelAppearance.SetSelectedFill(const Value: TAdvGraphicsFill);
begin
  FSelectedFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorPanelAppearance.SetSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  FSelectedStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorPanelAppearance.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    DoChanged(Self)
  end;
end;

procedure TAdvEditorPanelAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorPanelAppearance.SetStrokeSides(
  const Value: TAdvGraphicsSides);
begin
  if FStrokeSides <> Value then
  begin
    FStrokeSides := Value;
    DoStrokeChanged(Self);
  end;
end;

end.
