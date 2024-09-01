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

unit AdvEditorButton;

{$I TMSDEFS.INC}
{$IFDEF FMXLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}
{$IFDEF VCLLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types , AdvTypes, Controls, StdCtrls, Forms
  , AdvCustomControl, AdvGraphics, AdvGraphicsTypes
  {$IFDEF FMXLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 31}
  , FMX.AcceleratorKey
  {$ENDIF}
  {$HINTS ON}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  , UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LMessages
  {$ENDIF}
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
  TAdvEditorButtonAppearance = class;
  TAdvEditorButtonPosition = (ebpAlone, ebpLeft, ebpTop, ebpCenter, ebpRight, ebpBottom);

  {$IFDEF FMXLIB}
  TAdvCustomEditorButton = class(TAdvCustomControl{$HINTS OFF} {$IF COMPILERVERSION >= 31}, IAcceleratorKeyReceiver{$HINTS ON}{$ENDIF})
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TAdvCustomEditorButton = class(TAdvCustomControl)
  {$ENDIF}
  private
    FHover: Boolean;
    FDown: Boolean;
    FSelected: Boolean;
    FAppearance: TAdvEditorButtonAppearance;
    FText: string;
    FToggle: Boolean;
    FButtonPosition: TAdvEditorButtonPosition;
    FButtonPositionCorners: TAdvGraphicsCorners;
    FModalResult: TModalResult;
    FBitmap: TAdvBitmap;
    FDisabledBitmap: TAdvBitmap;
    FGroupName: string;
    FAcceleratorChar: Char;
    FAcceleratorCharPos: Integer;
    FOnButtonClick: TNotifyEvent;
    FBitmapMargins: TAdvMargins;
    FOnHandleAcceleratorKey: TNotifyEvent;
    procedure SetAppearance(const Value: TAdvEditorButtonAppearance);
    procedure SetText(const Value: string);
    procedure SetToggle(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);
    procedure SetButtonPosition(const Value: TAdvEditorButtonPosition);
    procedure SetButtonPositionCorners;
    procedure SetBitmap(const Value: TAdvBitmap);
    procedure SetDisabledBitmap(const Value: TAdvBitmap);
    procedure SetGroupName(const Value: string);
    procedure UpdateOtherGroupButtons;
    procedure SetAcceleratorChar(AText: string);
    procedure SetBitmapMargins(const Value: TAdvMargins);
    {$IFDEF VCLLIB}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF}
    {$IFDEF LCLLIB}
    function DialogChar(var Message: TLMKey): boolean; override;
    {$ENDIF}
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure SetSelectedByToggle(const AValue: Boolean);
    procedure DoButtonClick; virtual;
    procedure HandleMouseDown({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseUp({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure DoBitmapMarginsChanged(Sender: TObject); virtual;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawStandardBackGround({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawSelectionLine({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawBackground(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawButtonText(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawButtonImage(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure UpdateControlAfterResize; override;
    procedure HandleAcceleratorKey; virtual;
    {$IFDEF FMXLIB}
    procedure TriggerAcceleratorKey;
    function CanTriggerAcceleratorKey: Boolean;
    function GetAcceleratorChar: Char;
    function GetAcceleratorCharIndex: Integer;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure SetEnabled(Value: Boolean); override;
    {$ENDIF}
    property Appearance: TAdvEditorButtonAppearance read FAppearance write SetAppearance;
    property Text: string read FText write SetText;
    property Toggle: Boolean read FToggle write SetToggle;
    property Selected: Boolean read FSelected write SetSelected;
    property ButtonPosition: TAdvEditorButtonPosition read FButtonPosition write SetButtonPosition default ebpAlone;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapMargins: TAdvMargins read FBitmapMargins write SetBitmapMargins;
    property DisabledBitmap: TAdvBitmap read FDisabledBitmap write SetDisabledBitmap;
    property GroupName: string read FGroupName write SetGroupName;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnHandleAcceleratorKey: TNotifyEvent read FOnHandleAcceleratorKey write FOnHandleAcceleratorKey;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TAdvEditorButtonAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomEditorButton;
    FDownStroke: TAdvGraphicsStroke;
    FFont: TAdvGraphicsFont;
    FSelectedFill: TAdvGraphicsFill;
    FHoverFont: TAdvGraphicsFont;
    FFill: TAdvGraphicsFill;
    FSelectedStroke: TAdvGraphicsStroke;
    FHoverFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FDownFont: TAdvGraphicsFont;
    FHoverStroke: TAdvGraphicsStroke;
    FOnChanged: TNotifyEvent;
    FDownFill: TAdvGraphicsFill;
    FSelectedFont: TAdvGraphicsFont;
    FRounding: Integer;
    FTextAlignVertical: TAdvGraphicsTextAlign;
    FTextAlignHorizontal: TAdvGraphicsTextAlign;
    FSelectionLine: Boolean;
    FSelectionLineWidth: Single;
    FDisabledFont: TAdvGraphicsFont;
    FDisabledFill: TAdvGraphicsFill;
    FDisabledStroke: TAdvGraphicsStroke;
    procedure SetDownFill(const Value: TAdvGraphicsFill);
    procedure SetDownFont(const Value: TAdvGraphicsFont);
    procedure SetDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetHoverFill(const Value: TAdvGraphicsFill);
    procedure SetHoverFont(const Value: TAdvGraphicsFont);
    procedure SetHoverStroke(const Value: TAdvGraphicsStroke);
    procedure SetSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetSelectedFont(const Value: TAdvGraphicsFont);
    procedure SetSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetRounding(const Value: Integer);
    procedure SetTextAlignHorizontal(const Value: TAdvGraphicsTextAlign);
    procedure SetTextAlignVertical(const Value: TAdvGraphicsTextAlign);
    procedure SetSelectionLine(const Value: Boolean);
    procedure SetSelectionLineWidth(const Value: Single);
    procedure SetDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetDisabledFont(const Value: TAdvGraphicsFont);
    procedure SetDisabledStroke(const Value: TAdvGraphicsStroke);
  protected
    procedure DoChanged(Sender: TObject);
    procedure DoFillChanged(Sender: TObject); virtual;
    procedure DoFontChanged(Sender: TObject); virtual;
    procedure DoStrokeChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TAdvCustomEditorButton);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property DisabledFill: TAdvGraphicsFill read FDisabledFill write SetDisabledFill;
    property DisabledFont: TAdvGraphicsFont read FDisabledFont write SetDisabledFont;
    property DisabledStroke: TAdvGraphicsStroke read FDisabledStroke write SetDisabledStroke;
    property HoverFill: TAdvGraphicsFill read FHoverFill write SetHoverFill;
    property HoverFont: TAdvGraphicsFont read FHoverFont write SetHoverFont;
    property HoverStroke: TAdvGraphicsStroke read FHoverStroke write SetHoverStroke;
    property DownFill: TAdvGraphicsFill read FDownFill write SetDownFill;
    property DownFont: TAdvGraphicsFont read FDownFont write SetDownFont;
    property DownStroke: TAdvGraphicsStroke read FDownStroke write SetDownStroke;
    property SelectedFill: TAdvGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedFont: TAdvGraphicsFont read FSelectedFont write SetSelectedFont;
    property SelectedStroke: TAdvGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property Rounding: Integer read FRounding write SetRounding;
    property TextAlignHorizontal: TAdvGraphicsTextAlign read FTextAlignHorizontal write SetTextAlignHorizontal default gtaCenter;
    property TextAlignVertical: TAdvGraphicsTextAlign read FTextAlignVertical write SetTextAlignVertical default gtaCenter;
    property SelectionLine: Boolean read FSelectionLine write SetSelectionLine;
    property SelectionLineWidth: Single read FSelectionLineWidth write SetSelectionLineWidth;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvEditorButton = class(TAdvCustomEditorButton)
  published
    property Appearance;
    property DisabledBitmap;
    property GroupName;
    property Text;
    property Toggle;
    property Selected;
    property Bitmap;
    property BitmapMargins;
    property ButtonPosition;
    property ModalResult;
    property Hint;
    property ShowHint;
    property OnClick;
    property OnButtonClick;
    property OnHandleAcceleratorKey;
  end;

implementation

uses
  TypInfo, SysUtils, AdvUtils, Math
  {$IFDEF VCLLIB}
  , Winapi.Windows, VCL.Graphics
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Platform
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLIB.Graphics
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Graphics
  {$ENDIF}
  ;

{ TAdvEditorListView }

procedure TAdvCustomEditorButton.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TAdvCustomEditorButton.BeginUpdate;
begin
  inherited;
end;

{$IFDEF FMXLIB}
function TAdvCustomEditorButton.CanTriggerAcceleratorKey: Boolean;
begin
  if (FAcceleratorChar <> '') and Visible and Enabled then
    Result := True
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF VCLLIB}
procedure TAdvCustomEditorButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FText) and CanFocus then
    begin
      HandleAcceleratorKey;
      Result := 1;
    end else
      inherited;
end;
{$ENDIF}

procedure TAdvCustomEditorButton.ChangeDPIScale(M, D: Integer);
begin
  inherited;

  BeginUpdate;

  FAppearance.FRounding := TAdvUtils.MulDivInt(FAppearance.Rounding, M, D);
  FAppearance.FSelectionLineWidth := TAdvUtils.MulDivSingle(FAppearance.FSelectionLineWidth, M, D);

  FAppearance.FFont.Height := TAdvUtils.MulDivInt(FAppearance.FFont.Height, M, D);
  FAppearance.FHoverFont.Height := TAdvUtils.MulDivInt(FAppearance.FHoverFont.Height, M, D);
  FAppearance.FDownFont.Height := TAdvUtils.MulDivInt(FAppearance.FDownFont.Height, M, D);
  FAppearance.FSelectedFont.Height := TAdvUtils.MulDivInt(FAppearance.FSelectedFont.Height, M, D);
  FAppearance.FDisabledFont.Height := TAdvUtils.MulDivInt(FAppearance.FDisabledFont.Height, M, D);

//  FAppearance.FStroke.Width := TAdvUtils.MulDivSingle(FAppearance.FStroke.Width, M, D);
  FAppearance.FStroke.Width := FAppearance.FStroke.Width * M / D;
  FAppearance.FSelectedStroke.Width := TAdvUtils.MulDivSingle(FAppearance.FSelectedStroke.Width, M, D);
  FAppearance.FHoverStroke.Width := TAdvUtils.MulDivSingle(FAppearance.FHoverStroke.Width, M, D);
  FAppearance.FDownStroke.Width := TAdvUtils.MulDivSingle(FAppearance.FDownStroke.Width, M, D);
  FAppearance.FDisabledStroke.Width := TAdvUtils.MulDivSingle(FAppearance.FDisabledStroke.Width, M, D);

  EndUpdate;
end;

constructor TAdvCustomEditorButton.Create(AOwner: TComponent);
begin
  inherited;

  Fill.Kind := gfkNone;
  Stroke.Kind := gskNone;

  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  NativeCanvas := True;
  TextQuality := gtqClearType;
  {$ENDIF}
  {$ENDIF}

  Transparent := True;

  {$IFDEF VCLLIB}
  ControlStyle := ControlStyle - [csAcceptsControls];
  {$ENDIF}

  FAppearance := TAdvEditorButtonAppearance.Create(Self);
  FAppearance.OnChanged := DoAppearanceChanged;

  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapMargins := TAdvMargins.Create;
  FBitmapMargins.OnChange := DoBitmapMarginsChanged;
  FDisabledBitmap := TAdvBitmap.Create;
  FDisabledBitmap.OnChange := DoBitmapChanged;

  FText := 'Button';

  FButtonPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FButtonPosition := ebpAlone;

  Width := 80;
  Height := 25;
end;

destructor TAdvCustomEditorButton.Destroy;
{$IFDEF FMXLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 31}
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
{$IFEND}
{$HINTS ON}
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 31}
  if (FAcceleratorChar <> '') and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
      AccelKeyService.UnregisterReceiver(Root, Self);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  FAppearance.Free;
  FBitmap.Free;
  FBitmapMargins.Free;
  FDisabledBitmap.Free;
  inherited;
end;

procedure TAdvCustomEditorButton.DoAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomEditorButton.DoBitmapChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomEditorButton.DoBitmapMarginsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomEditorButton.DoButtonClick;
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Self);
end;

procedure TAdvCustomEditorButton.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  DrawStandardBackGround(AGraphics, ARect);
  DrawSelectionLine(AGraphics, ARect);
  DrawButtonImage(AGraphics, ARect);
  DrawButtonText(AGraphics,ARect);
end;

procedure TAdvCustomEditorButton.DrawBackground(AGraphics: TAdvGraphics; ARect: TRectF);
begin
//    inherited;
end;

procedure TAdvCustomEditorButton.DrawButtonImage(AGraphics: TAdvGraphics; ARect: TRectF);
var
  ir: TRectF;
  ar, s: Boolean;
  ro: integer;
  bmp: TAdvBitmap;
begin
  if not Enabled and not IsBitmapEmpty(FDisabledBitmap) then
    bmp := FDisabledBitmap
  else
    bmp := FBitmap;

  if not IsBitmapEmpty(bmp) then
  begin
    ar := True;
    s := True;

    ro := Min(FAppearance.Rounding, Round(Min(Height/2, Width/2)));

    ir := RectF(ARect.Left + Max(FBitmapMargins.Left, ro/8), ARect.Top + Max(FBitmapMargins.Top, ro/8), ARect.Right - Max(FBitmapMargins.Right, ro/8), ARect.Bottom - Max(FBitmapMargins.Bottom, ro/8));

    AGraphics.DrawBitmap(ir, bmp, ar, s);
  end;
end;

procedure TAdvCustomEditorButton.DrawButtonText(AGraphics: TAdvGraphics; ARect: TRectF);
var
  txt: string;
  tr: TRectF;
  va, ha: TAdvGraphicsTextAlign;
  ww: Boolean;
  ro: integer;
  cw, cs, rw, w, h: single;
begin
  if FText <> '' then
  begin
    cs := 0;
    cw := 0;

    ww := False;
    ha := FAppearance.TextAlignHorizontal;
    va := FAppearance.TextAlignVertical;

    tr := ARect;

    if (ShowAcceleratorChar) and (FAcceleratorCharPos > 0) then
    begin
      txt := StringReplace(FText, '&', '', [rfReplaceAll]);

      if FAcceleratorCharPos > 1 then
        cs := AGraphics.CalculateTextWidth(Copy(txt, 0, FAcceleratorCharPos - 1));

      cw := AGraphics.CalculateTextWidth(txt[FAcceleratorCharPos]);
    end
    else
      txt := FText;

    ro := Min(FAppearance.Rounding, Round(Height/2));


    w := AGraphics.CalculateTextWidth(txt) + 2;
    h := AGraphics.CalculateTextHeight('Hg');

    tr.Left := tr.Left + ro / 4;
    tr.Right := tr.Right - ro / 4;
    tr.Top := tr.Top + ro / 4;
    tr.Bottom := tr.Bottom - ro / 4;

    case ha of
      gtaCenter:
      begin
        rw := tr.Right - tr.Left;
        tr.Left := Max(tr.Left, tr.Left + (rw - w)/2);
        tr.Right := Min(tr.Right, tr.Left + w);
      end;
      gtaLeading:
      begin
        tr.Right := tr.Left + w;
      end;
      gtaTrailing:
      begin
        tr.Left := tr.Right - w;
      end;
    end;

    case va of
      gtaCenter:
      begin
        rw := tr.Bottom - tr.Top;
        tr.Top := Max(tr.Top, tr.Top + (rw - h)/2);
        tr.Bottom := Min(tr.Bottom, tr.Top + h);
      end;
      gtaLeading:
      begin
        tr.Bottom := tr.Top + h;
      end;
      gtaTrailing:
      begin
        tr.Top := tr.Bottom - h;
      end;
    end;

    AGraphics.DrawText(tr, txt, ww);

    if (ShowAcceleratorChar) and (FAcceleratorCharPos > 0) and not (FSelected or FDown) then
    begin
      AGraphics.Stroke.Width := Trunc(ScalePaintValue(1.0));
      AGraphics.Stroke.Kind := gskSolid;
      AGraphics.Stroke.Color := AGraphics.font.Color;
      AGraphics.DrawLine(PointF(tr.Left + cs,tr.Bottom), PointF(tr.Left + cs + cw, tr.Bottom));
    end;
  end;
end;

procedure TAdvCustomEditorButton.DrawSelectionLine(AGraphics: TAdvGraphics; ARect: TRectF);
var
  drect: TRectF;
  rounding: integer;
  sw: Single;
begin
  if (FSelected or FDown or FHover) and FAppearance.SelectionLine then
  begin
    if FSelected then
    begin
      AGraphics.Fill.Assign(FAppearance.SelectedFill);
      AGraphics.Font.Assign(FAppearance.SelectedFont);
      AGraphics.Stroke.Assign(FAppearance.SelectedStroke);
    end
    else if FDown then
    begin
      AGraphics.Fill.Assign(FAppearance.DownFill);
      AGraphics.Font.Assign(FAppearance.DownFont);
      AGraphics.Stroke.Assign(FAppearance.DownStroke);
    end
    else if FHover then
    begin
      AGraphics.Fill.Assign(FAppearance.HoverFill);
      AGraphics.Font.Assign(FAppearance.HoverFont);
      AGraphics.Stroke.Assign(FAppearance.HoverStroke);
    end;

    AGraphics.Stroke.Width := Trunc(AGraphics.Stroke.Width);

    if (AGraphics.Stroke.Kind <> gskNone) then
      sw := AGraphics.Stroke.Width
    else
      sw := 0;

    drect := RectF(ARect.Left, ARect.Bottom - sw - Trunc(FAppearance.SelectionLineWidth), ARect.Right, ARect.Bottom - sw);
    rounding := Round(Min(FAppearance.Rounding / 2, (drect.Bottom - drect.Top)/2));

    if FButtonPosition in [ebpAlone, ebpLeft] then
      drect.Left := drect.Left + rounding;
    if FButtonPosition in [ebpAlone, ebpRight] then
      drect.Right := drect.Right - rounding;

    AGraphics.DrawRoundRectangle(drect, rounding, FButtonPositionCorners);
  end;
end;

procedure TAdvCustomEditorButton.DrawStandardBackGround(AGraphics: TAdvGraphics; ARect: TRectF);
var
  drect: TRectF;
  rounding: integer;
begin
  if not Enabled then
  begin
    AGraphics.Fill.Assign(FAppearance.DisabledFill);
    AGraphics.Font.Assign(FAppearance.DisabledFont);
    AGraphics.Stroke.Assign(FAppearance.DisabledStroke);
  end
  else if FAppearance.SelectionLine then
  begin
    AGraphics.Fill.Assign(FAppearance.Fill);
    AGraphics.Font.Assign(FAppearance.Font);
    AGraphics.Stroke.Assign(FAppearance.Stroke);
  end
  else if FSelected then
  begin
    AGraphics.Fill.Assign(FAppearance.SelectedFill);
    AGraphics.Font.Assign(FAppearance.SelectedFont);
    AGraphics.Stroke.Assign(FAppearance.SelectedStroke);
  end
  else if FDown then
  begin
    AGraphics.Fill.Assign(FAppearance.DownFill);
    AGraphics.Font.Assign(FAppearance.DownFont);
    AGraphics.Stroke.Assign(FAppearance.DownStroke);
  end
  else if FHover then
  begin
    AGraphics.Fill.Assign(FAppearance.HoverFill);
    AGraphics.Font.Assign(FAppearance.HoverFont);
    AGraphics.Stroke.Assign(FAppearance.HoverStroke);
  end
  else
  begin
    AGraphics.Fill.Assign(FAppearance.Fill);
    AGraphics.Font.Assign(FAppearance.Font);
    AGraphics.Stroke.Assign(FAppearance.Stroke);
  end;

  AGraphics.Stroke.Width := Trunc(AGraphics.Stroke.Width);

  drect := ARect;
  rounding := Min(FAppearance.Rounding, Round(Height/2));

  AGraphics.DrawRoundRectangle(drect, rounding, FButtonPositionCorners)
end;

procedure TAdvCustomEditorButton.EndUpdate;
begin
  inherited;
  Invalidate;
end;

{$IFDEF FMXLIB}
function TAdvCustomEditorButton.GetAcceleratorChar: Char;
begin
  Result := FAcceleratorChar;
end;

function TAdvCustomEditorButton.GetAcceleratorCharIndex: Integer;
begin
  Result := FAcceleratorCharPos;
end;
{$ENDIF}

procedure TAdvCustomEditorButton.SetButtonPositionCorners;
begin
  case FButtonPosition of
    ebpLeft: FButtonPositionCorners := [gcTopLeft, gcBottomLeft];
    ebpCenter: FButtonPositionCorners := [];
    ebpRight: FButtonPositionCorners := [gcTopRight, gcBottomRight];
    ebpTop: FButtonPositionCorners := [gcTopLeft, gcTopRight];
    ebpBottom: FButtonPositionCorners := [gcBottomLeft, gcBottomRight];
    else
      FButtonPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  end;
end;

procedure TAdvCustomEditorButton.SetDisabledBitmap(const Value: TAdvBitmap);
begin
  FDisabledBitmap.Assign(Value);
  DoBitmapChanged(Self);
end;

{$IFDEF FMXLIB}
procedure TAdvCustomEditorButton.SetEnabled(const Value: Boolean);
{$ENDIF}
{$IFNDEF FMXLIB}
procedure TAdvCustomEditorButton.SetEnabled(Value: Boolean);
{$ENDIF}
begin
  inherited;

  Invalidate;
end;

procedure TAdvCustomEditorButton.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
  end;
end;

procedure TAdvCustomEditorButton.HandleAcceleratorKey;
begin
  if Assigned(FOnHandleAcceleratorKey) then
    OnHandleAcceleratorKey(Self)
  else if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TAdvCustomEditorButton.HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled then
  begin
    FDown := True;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.HandleMouseEnter;
begin
  inherited;
  if Enabled then
  begin
    FHover := True;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.HandleMouseLeave;
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TAdvCustomEditorButton.HandleMouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled and not FHover then
  begin
    FHover := True;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single);
var
  {$IFDEF FMXLIB}
  O: TComponent;
  {$ENDIF}
  {$IFDEF CMNLIB}
  Form: TCustomForm;
  {$ENDIF}
begin
  inherited;

  if Enabled then
  begin
    if FToggle then
      Selected := not FSelected;

    if FDown then
    begin
      if (ModalResult <> mrNone) then
      begin
        {$IFDEF FMXLIB}
        O := Scene.GetObject;
        while O <> nil do
        begin
          if (O is TCommonCustomForm) then
          begin
            TCommonCustomForm(O).ModalResult := FModalResult;
            Break;
          end;
          O := O.Owner;
        end;
        {$ENDIF}
        {$IFDEF CMNLIB}
        Form := GetParentForm(Self);
        if Form <> nil then
          Form.ModalResult := ModalResult;
        {$ENDIF}
      end;

      DoButtonClick;
    end;
  end;

  FDown := False;
  Invalidate;
end;

procedure TAdvCustomEditorButton.SetAcceleratorChar(AText: string);
var
  {$IFDEF FMXLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 31}
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  c: Char;
begin
  FAcceleratorCharPos := Pos('&', FText);
  if FAcceleratorCharPos > 0 then
  begin
    c := LowerCase(FText)[FAcceleratorCharPos + 1];
    if c <> FAcceleratorChar then
    begin
      {$IFDEF FMXLIB}
      {$HINTS OFF}
      {$IF COMPILERVERSION >= 31}
      if (FAcceleratorChar <> '') and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
        AccelKeyService.UnregisterReceiver(Root, Self);
      {$IFEND}
      {$HINTS ON}
      {$ENDIF}

      FAcceleratorChar := LowerCase(FText)[FAcceleratorCharPos + 1];

      {$IFDEF FMXLIB}
      {$HINTS OFF}
      {$IF COMPILERVERSION >= 31}
      if (FAcceleratorChar <> '') and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
        AccelKeyService.RegisterReceiver(Root, Self);
      {$IFEND}
      {$HINTS ON}
      {$ENDIF}
    end;
  end;
end;

{$IFDEF LCLLIB}
function TAdvCustomEditorButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Text) and CanFocus then
  begin
    HandleAcceleratorKey;
    Result := true;
  end else
    Result := inherited;
end;
{$ENDIF}

procedure TAdvCustomEditorButton.SetAppearance(const Value: TAdvEditorButtonAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomEditorButton.SetBitmap(const Value: TAdvBitmap);
begin
  FBitmap.Assign(Value);
  DoBitmapChanged(Self);
end;

procedure TAdvCustomEditorButton.SetBitmapMargins(const Value: TAdvMargins);
begin
  FBitmapMargins.Assign(Value);
end;

procedure TAdvCustomEditorButton.SetButtonPosition(const Value: TAdvEditorButtonPosition);
begin
  if FButtonPosition <> Value then
  begin
    FButtonPosition := Value;
    SetButtonPositionCorners;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    if FGroupName <> '' then
      UpdateOtherGroupButtons;
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.SetSelectedByToggle(const AValue: Boolean);
begin
  FSelected := AValue;
  Invalidate;
end;

procedure TAdvCustomEditorButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    SetAcceleratorChar(FText);
    Invalidate;
  end;
end;

procedure TAdvCustomEditorButton.SetToggle(const Value: Boolean);
begin
  if FToggle <> Value then
  begin
    FToggle := Value;
    Invalidate;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomEditorButton.TriggerAcceleratorKey;
begin
  HandleAcceleratorKey;
end;
{$ENDIF}

procedure TAdvCustomEditorButton.UpdateControlAfterResize;
begin
  inherited;
end;

procedure TAdvCustomEditorButton.UpdateOtherGroupButtons;
var
  I: Integer;
begin
  {$IFDEF FMXLIB}
  for I := 0 to Parent.ChildrenCount - 1 do
  begin
    if (Parent.Children.Items[I] is TAdvCustomEditorButton) and (Parent.Children.Items[I].ComponentIndex <> ComponentIndex) then
    begin
      if ((Parent.Children.Items[I] as TAdvCustomEditorButton).GroupName = FGroupName) and (Parent.Children.Items[I] as TAdvCustomEditorButton).Toggle then
      begin
        (Parent.Children.Items[I] as TAdvCustomEditorButton).SetSelectedByToggle(False);
      end;
    end;
  end;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  for I := 0 to Parent.ControlCount - 1 do
  begin
    if (Parent.Controls[I] is TAdvCustomEditorButton) and (Parent.Controls[I].ComponentIndex <> ComponentIndex) then
    begin
      if ((Parent.Controls[I] as TAdvCustomEditorButton).GroupName = FGroupName) and (Parent.Controls[I] as TAdvCustomEditorButton).Toggle then
      begin
        (Parent.Controls[I] as TAdvCustomEditorButton).SetSelectedByToggle(False);
      end;
    end;
  end;
  {$ENDIF}
end;

{ TAdvEditorButtonAppearance }

procedure TAdvEditorButtonAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvEditorButtonAppearance then
  begin
    FSelectionLine := (Source as TAdvEditorButtonAppearance).SelectionLine;
    FSelectionLineWidth := (Source as TAdvEditorButtonAppearance).SelectionLineWidth;
    FRounding := (Source as TAdvEditorButtonAppearance).Rounding;
    FFill.Assign((Source as TAdvEditorButtonAppearance).Fill);
    FFont.Assign((Source as TAdvEditorButtonAppearance).Font);
    FStroke.Assign((Source as TAdvEditorButtonAppearance).Stroke);
    FHoverFill.Assign((Source as TAdvEditorButtonAppearance).HoverFill);
    FHoverFont.Assign((Source as TAdvEditorButtonAppearance).HoverFont);
    FHoverStroke.Assign((Source as TAdvEditorButtonAppearance).HoverStroke);
    FDownFill.Assign((Source as TAdvEditorButtonAppearance).DownFill);
    FDownFont.Assign((Source as TAdvEditorButtonAppearance).DownFont);
    FDownStroke.Assign((Source as TAdvEditorButtonAppearance).DownStroke);
    FSelectedFill.Assign((Source as TAdvEditorButtonAppearance).SelectedFill);
    FSelectedFont.Assign((Source as TAdvEditorButtonAppearance).SelectedFont);
    FSelectedStroke.Assign((Source as TAdvEditorButtonAppearance).SelectedStroke);
    FDisabledFill.Assign((Source as TAdvEditorButtonAppearance).DisabledFill);
    FDisabledFont.Assign((Source as TAdvEditorButtonAppearance).DisabledFont);
    FDisabledStroke.Assign((Source as TAdvEditorButtonAppearance).DisabledStroke);
  end
  else
    inherited;
end;

constructor TAdvEditorButtonAppearance.Create(AOwner: TAdvCustomEditorButton);
begin
  FOwner := AOwner;

  FSelectionLine := False;
  FSelectionLineWidth := 4;
  FRounding := 6;

  FFill := TAdvGraphicsFill.Create;
  FFont := TAdvGraphicsFont.Create;
  FStroke := TAdvGraphicsStroke.Create;
  FHoverFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FHoverFont := TAdvGraphicsFont.Create;
  FHoverStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FDownFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FDownFont := TAdvGraphicsFont.Create;
  FDownStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FSelectedFont := TAdvGraphicsFont.Create;
  FSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FDisabledFill := TAdvGraphicsFill.Create;
  FDisabledFont := TAdvGraphicsFont.Create;
  FDisabledFont.Color := gcMedGray;
  FDisabledStroke := TAdvGraphicsStroke.Create(gskSolid, gcMedGray);

  FFill.OnChanged := DoFillChanged;
  FFont.OnChanged := DoFontChanged;
  FStroke.OnChanged := DoStrokeChanged;
  FDisabledFill.OnChanged := DoFillChanged;
  FDisabledFont.OnChanged := DoFontChanged;
  FDisabledStroke.OnChanged := DoStrokeChanged;
  FHoverFill.OnChanged := DoFillChanged;
  FHoverFont.OnChanged := DoFontChanged;
  FHoverStroke.OnChanged := DoStrokeChanged;
  FDownFill.OnChanged := DoFillChanged;
  FDownFont.OnChanged := DoFontChanged;
  FDownStroke.OnChanged := DoStrokeChanged;
  FSelectedFill.OnChanged := DoFillChanged;
  FSelectedFont.OnChanged := DoFontChanged;
  FSelectedStroke.OnChanged := DoStrokeChanged;
end;

destructor TAdvEditorButtonAppearance.Destroy;
begin
  FFill.Free;
  FFont.Free;
  FStroke.Free;
  FHoverFill.Free;
  FHoverFont.Free;
  FHoverStroke.Free;
  FDownFill.Free;
  FDownFont.Free;
  FDownStroke.Free;
  FSelectedFill.Free;
  FSelectedFont.Free;
  FSelectedStroke.Free;
  FDisabledFill.Free;
  FDisabledFont.Free;
  FDisabledStroke.Free;
  inherited;
end;

procedure TAdvEditorButtonAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvEditorButtonAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorButtonAppearance.DoFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorButtonAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorButtonAppearance.SetDisabledFill(const Value: TAdvGraphicsFill);
begin
  FDisabledFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetDisabledFont(const Value: TAdvGraphicsFont);
begin
  FDisabledFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetDisabledStroke(const Value: TAdvGraphicsStroke);
begin
  FDisabledStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetDownFill(const Value: TAdvGraphicsFill);
begin
  FDownFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetDownFont(const Value: TAdvGraphicsFont);
begin
  FDownFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetDownStroke(const Value: TAdvGraphicsStroke);
begin
  FDownStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetHoverFill(const Value: TAdvGraphicsFill);
begin
  FHoverFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetHoverFont(const Value: TAdvGraphicsFont);
begin
  FHoverFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetHoverStroke(const Value: TAdvGraphicsStroke);
begin
  FHoverStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetSelectedFill(const Value: TAdvGraphicsFill);
begin
  FSelectedFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetSelectedFont(const Value: TAdvGraphicsFont);
begin
  FSelectedFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  FSelectedStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetSelectionLine(const Value: Boolean);
begin
  if FSelectionLine <> Value then
  begin
    FSelectionLine := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorButtonAppearance.SetSelectionLineWidth(const Value: Single);
begin
  if FSelectionLineWidth <> Value then
  begin
    FSelectionLineWidth := Value;
    DoChanged(Self)
  end;
end;

procedure TAdvEditorButtonAppearance.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    DoChanged(Self)
  end;
end;

procedure TAdvEditorButtonAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorButtonAppearance.SetTextAlignHorizontal(const Value: TAdvGraphicsTextAlign);
begin
  if FTextAlignHorizontal <> Value then
  begin
    FTextAlignHorizontal := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorButtonAppearance.SetTextAlignVertical(const Value: TAdvGraphicsTextAlign);
begin
  if FTextAlignVertical <> Value then
  begin
    FTextAlignVertical := Value;
    DoChanged(Self);
  end;
end;
end.
