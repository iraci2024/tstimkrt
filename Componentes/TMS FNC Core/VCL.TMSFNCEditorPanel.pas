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

unit VCL.TMSFNCEditorPanel;

{$I VCL.TMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types , VCL.TMSFNCTypes, VCL.Controls, VCL.StdCtrls
  , VCL.TMSFNCCustomControl, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes

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
  TTMSFNCCustomEditorPanel = class;
  TTMSFNCEditorPanelPosition = (eppAlone, eppLeft, eppTop, eppCenter, eppRight, eppBottom);

  TTMSFNCEditorPanelAppearance = class(TPersistent)
  private
    FOwner: TTMSFNCCustomEditorPanel;
    FFill: TTMSFNCGraphicsFill;
    FSelectedFill: TTMSFNCGraphicsFill;
    FSelectedStroke: TTMSFNCGraphicsStroke;
    FStroke: TTMSFNCGraphicsStroke;
    FRounding: Integer;
    FOnChanged: TNotifyEvent;
    FStrokeSides: TTMSFNCGraphicsSides;
    procedure SetFill(const Value: TTMSFNCGraphicsFill);
    procedure SetSelectedFill(const Value: TTMSFNCGraphicsFill);
    procedure SetSelectedStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetRounding(const Value: Integer);
    procedure SetStrokeSides(const Value: TTMSFNCGraphicsSides);
  protected
    procedure DoChanged(Sender: TObject);
    procedure DoFillChanged(Sender: TObject); virtual;
    procedure DoStrokeChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TTMSFNCCustomEditorPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TTMSFNCGraphicsFill read FFill write SetFill;
    property Stroke: TTMSFNCGraphicsStroke read FStroke write SetStroke;
    property SelectedFill: TTMSFNCGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TTMSFNCGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property Rounding: Integer read FRounding write SetRounding;
    property StrokeSides: TTMSFNCGraphicsSides read FStrokeSides write SetStrokeSides;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTMSFNCCustomEditorPanel = class(TTMSFNCCustomControl)
  private
    FAppearance: TTMSFNCEditorPanelAppearance;
    FPanelPosition: TTMSFNCEditorPanelPosition;
    FPanelPositionCorners: TTMSFNCGraphicsCorners;
    FSelected: Boolean;
    procedure SetAppearance(const Value: TTMSFNCEditorPanelAppearance);
    procedure SetPanelPosition(const Value: TTMSFNCEditorPanelPosition);
    procedure SetPanelPositionCorners;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure Draw({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBackground(AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure UpdateControlAfterResize; override;
    property Appearance: TTMSFNCEditorPanelAppearance read FAppearance write SetAppearance;
    property PanelPosition: TTMSFNCEditorPanelPosition read FPanelPosition write SetPanelPosition default eppAlone;
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
  TTMSFNCEditorPanel = class(TTMSFNCCustomEditorPanel)
  published
    property Appearance;
    property PanelPosition;
    property Selected;
  end;

implementation

uses
  TypInfo, VCL.Forms, SysUtils, VCL.TMSFNCUtils, Math
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

{ TTMSFNCEditorListView }

procedure TTMSFNCCustomEditorPanel.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TTMSFNCCustomEditorPanel.BeginUpdate;
begin
  inherited;
end;

procedure TTMSFNCCustomEditorPanel.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  BeginUpdate;
  FAppearance.Rounding := TTMSFNCUtils.MulDivInt(FAppearance.Rounding, M, D);
  EndUpdate;
end;

constructor TTMSFNCCustomEditorPanel.Create(AOwner: TComponent);
begin
  inherited;

  Fill.Kind := gfkNone;
  Stroke.Kind := gskNone;

  FAppearance := TTMSFNCEditorPanelAppearance.Create(Self);
  FAppearance.OnChanged := DoAppearanceChanged;

  FPanelPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FPanelPosition := eppAlone;

  Width := 200;
  Height := 200;
end;

destructor TTMSFNCCustomEditorPanel.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TTMSFNCCustomEditorPanel.DoAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTMSFNCCustomEditorPanel.Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF);
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

procedure TTMSFNCCustomEditorPanel.DrawBackground(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
//    inherited;
end;

procedure TTMSFNCCustomEditorPanel.EndUpdate;
begin
  inherited;
  Invalidate;
end;

procedure TTMSFNCCustomEditorPanel.SetPanelPositionCorners;
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

procedure TTMSFNCCustomEditorPanel.SetAppearance(const Value: TTMSFNCEditorPanelAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TTMSFNCCustomEditorPanel.SetPanelPosition(const Value: TTMSFNCEditorPanelPosition);
begin
  if FPanelPosition <> Value then
  begin
    FPanelPosition := Value;
    SetPanelPositionCorners;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorPanel.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorPanel.UpdateControlAfterResize;
begin
  inherited;
end;

{ TTMSFNCEditorPanelAppearance }

procedure TTMSFNCEditorPanelAppearance.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCEditorPanelAppearance then
  begin
    FRounding := (Source as TTMSFNCEditorPanelAppearance).Rounding;
    FFill.Assign((Source as TTMSFNCEditorPanelAppearance).Fill);
    FStroke.Assign((Source as TTMSFNCEditorPanelAppearance).Stroke);
    FSelectedFill.Assign((Source as TTMSFNCEditorPanelAppearance).SelectedFill);
    FSelectedStroke.Assign((Source as TTMSFNCEditorPanelAppearance).SelectedStroke);
  end
  else
    inherited;
end;

constructor TTMSFNCEditorPanelAppearance.Create(AOwner: TTMSFNCCustomEditorPanel);
begin
  FOwner := AOwner;
  FRounding := 0;

  FStrokeSides := [gsLeft, gsRight, gsTop, gsBottom];

  FFill := TTMSFNCGraphicsFill.Create;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FSelectedFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FSelectedStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));

  FFill.OnChanged := DoFillChanged;
  FStroke.OnChanged := DoStrokeChanged;
  FSelectedFill.OnChanged := DoFillChanged;
  FSelectedStroke.OnChanged := DoStrokeChanged;
end;

destructor TTMSFNCEditorPanelAppearance.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FSelectedFill.Free;
  FSelectedStroke.Free;
  inherited;
end;

procedure TTMSFNCEditorPanelAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TTMSFNCEditorPanelAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorPanelAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorPanelAppearance.SetFill(const Value: TTMSFNCGraphicsFill);
begin
  FFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorPanelAppearance.SetSelectedFill(const Value: TTMSFNCGraphicsFill);
begin
  FSelectedFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorPanelAppearance.SetSelectedStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FSelectedStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorPanelAppearance.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    DoChanged(Self)
  end;
end;

procedure TTMSFNCEditorPanelAppearance.SetStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorPanelAppearance.SetStrokeSides(
  const Value: TTMSFNCGraphicsSides);
begin
  if FStrokeSides <> Value then
  begin
    FStrokeSides := Value;
    DoStrokeChanged(Self);
  end;
end;

end.
