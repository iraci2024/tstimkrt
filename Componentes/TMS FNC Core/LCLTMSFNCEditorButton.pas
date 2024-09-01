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

unit LCLTMSFNCEditorButton;

{$I LCLTMSFNCDefines.inc}
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
  Classes, Types , LCLTMSFNCTypes, Controls, StdCtrls, Forms
  , LCLTMSFNCCustomControl, LCLTMSFNCGraphics, LCLTMSFNCGraphicsTypes
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
  TTMSFNCEditorButtonAppearance = class;
  TTMSFNCEditorButtonPosition = (ebpAlone, ebpLeft, ebpTop, ebpCenter, ebpRight, ebpBottom);

  {$IFDEF FMXLIB}
  TTMSFNCCustomEditorButton = class(TTMSFNCCustomControl{$HINTS OFF} {$IF COMPILERVERSION >= 31}, IAcceleratorKeyReceiver{$HINTS ON}{$ENDIF})
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TTMSFNCCustomEditorButton = class(TTMSFNCCustomControl)
  {$ENDIF}
  private
    FHover: Boolean;
    FDown: Boolean;
    FSelected: Boolean;
    FAppearance: TTMSFNCEditorButtonAppearance;
    FText: string;
    FToggle: Boolean;
    FButtonPosition: TTMSFNCEditorButtonPosition;
    FButtonPositionCorners: TTMSFNCGraphicsCorners;
    FModalResult: TModalResult;
    FBitmap: TTMSFNCBitmap;
    FDisabledBitmap: TTMSFNCBitmap;
    FGroupName: string;
    FAcceleratorChar: Char;
    FAcceleratorCharPos: Integer;
    FOnButtonClick: TNotifyEvent;
    FBitmapMargins: TTMSFNCMargins;
    FOnHandleAcceleratorKey: TNotifyEvent;
    procedure SetAppearance(const Value: TTMSFNCEditorButtonAppearance);
    procedure SetText(const Value: string);
    procedure SetToggle(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);
    procedure SetButtonPosition(const Value: TTMSFNCEditorButtonPosition);
    procedure SetButtonPositionCorners;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
    procedure SetDisabledBitmap(const Value: TTMSFNCBitmap);
    procedure SetGroupName(const Value: string);
    procedure UpdateOtherGroupButtons;
    procedure SetAcceleratorChar(AText: string);
    procedure SetBitmapMargins(const Value: TTMSFNCMargins);
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
    procedure HandleMouseDown({%H-}Button: TTMSFNCMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseUp({%H-}Button: TTMSFNCMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure DoBitmapMarginsChanged(Sender: TObject); virtual;
    procedure Draw({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure DrawStandardBackGround({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawSelectionLine({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawBackground(AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure DrawButtonText(AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawButtonImage(AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); virtual;
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
    property Appearance: TTMSFNCEditorButtonAppearance read FAppearance write SetAppearance;
    property Text: string read FText write SetText;
    property Toggle: Boolean read FToggle write SetToggle;
    property Selected: Boolean read FSelected write SetSelected;
    property ButtonPosition: TTMSFNCEditorButtonPosition read FButtonPosition write SetButtonPosition default ebpAlone;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property BitmapMargins: TTMSFNCMargins read FBitmapMargins write SetBitmapMargins;
    property DisabledBitmap: TTMSFNCBitmap read FDisabledBitmap write SetDisabledBitmap;
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

  TTMSFNCEditorButtonAppearance = class(TPersistent)
  private
    FOwner: TTMSFNCCustomEditorButton;
    FDownStroke: TTMSFNCGraphicsStroke;
    FFont: TTMSFNCGraphicsFont;
    FSelectedFill: TTMSFNCGraphicsFill;
    FHoverFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FSelectedStroke: TTMSFNCGraphicsStroke;
    FHoverFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FDownFont: TTMSFNCGraphicsFont;
    FHoverStroke: TTMSFNCGraphicsStroke;
    FOnChanged: TNotifyEvent;
    FDownFill: TTMSFNCGraphicsFill;
    FSelectedFont: TTMSFNCGraphicsFont;
    FRounding: Integer;
    FTextAlignVertical: TTMSFNCGraphicsTextAlign;
    FTextAlignHorizontal: TTMSFNCGraphicsTextAlign;
    FSelectionLine: Boolean;
    FSelectionLineWidth: Single;
    FDisabledFont: TTMSFNCGraphicsFont;
    FDisabledFill: TTMSFNCGraphicsFill;
    FDisabledStroke: TTMSFNCGraphicsStroke;
    procedure SetDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetDownFont(const Value: TTMSFNCGraphicsFont);
    procedure SetDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetFill(const Value: TTMSFNCGraphicsFill);
    procedure SetFont(const Value: TTMSFNCGraphicsFont);
    procedure SetHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetHoverFont(const Value: TTMSFNCGraphicsFont);
    procedure SetHoverStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetSelectedFill(const Value: TTMSFNCGraphicsFill);
    procedure SetSelectedFont(const Value: TTMSFNCGraphicsFont);
    procedure SetSelectedStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetRounding(const Value: Integer);
    procedure SetTextAlignHorizontal(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetTextAlignVertical(const Value: TTMSFNCGraphicsTextAlign);
    procedure SetSelectionLine(const Value: Boolean);
    procedure SetSelectionLineWidth(const Value: Single);
    procedure SetDisabledFill(const Value: TTMSFNCGraphicsFill);
    procedure SetDisabledFont(const Value: TTMSFNCGraphicsFont);
    procedure SetDisabledStroke(const Value: TTMSFNCGraphicsStroke);
  protected
    procedure DoChanged(Sender: TObject);
    procedure DoFillChanged(Sender: TObject); virtual;
    procedure DoFontChanged(Sender: TObject); virtual;
    procedure DoStrokeChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TTMSFNCCustomEditorButton);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TTMSFNCGraphicsFill read FFill write SetFill;
    property Stroke: TTMSFNCGraphicsStroke read FStroke write SetStroke;
    property DisabledFill: TTMSFNCGraphicsFill read FDisabledFill write SetDisabledFill;
    property DisabledFont: TTMSFNCGraphicsFont read FDisabledFont write SetDisabledFont;
    property DisabledStroke: TTMSFNCGraphicsStroke read FDisabledStroke write SetDisabledStroke;
    property HoverFill: TTMSFNCGraphicsFill read FHoverFill write SetHoverFill;
    property HoverFont: TTMSFNCGraphicsFont read FHoverFont write SetHoverFont;
    property HoverStroke: TTMSFNCGraphicsStroke read FHoverStroke write SetHoverStroke;
    property DownFill: TTMSFNCGraphicsFill read FDownFill write SetDownFill;
    property DownFont: TTMSFNCGraphicsFont read FDownFont write SetDownFont;
    property DownStroke: TTMSFNCGraphicsStroke read FDownStroke write SetDownStroke;
    property SelectedFill: TTMSFNCGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedFont: TTMSFNCGraphicsFont read FSelectedFont write SetSelectedFont;
    property SelectedStroke: TTMSFNCGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property Font: TTMSFNCGraphicsFont read FFont write SetFont;
    property Rounding: Integer read FRounding write SetRounding;
    property TextAlignHorizontal: TTMSFNCGraphicsTextAlign read FTextAlignHorizontal write SetTextAlignHorizontal default gtaCenter;
    property TextAlignVertical: TTMSFNCGraphicsTextAlign read FTextAlignVertical write SetTextAlignVertical default gtaCenter;
    property SelectionLine: Boolean read FSelectionLine write SetSelectionLine;
    property SelectionLineWidth: Single read FSelectionLineWidth write SetSelectionLineWidth;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCEditorButton = class(TTMSFNCCustomEditorButton)
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
  TypInfo, SysUtils, LCLTMSFNCUtils, Math
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

{ TTMSFNCEditorListView }

procedure TTMSFNCCustomEditorButton.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TTMSFNCCustomEditorButton.BeginUpdate;
begin
  inherited;
end;

{$IFDEF FMXLIB}
function TTMSFNCCustomEditorButton.CanTriggerAcceleratorKey: Boolean;
begin
  if (FAcceleratorChar <> '') and Visible and Enabled then
    Result := True
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF VCLLIB}
procedure TTMSFNCCustomEditorButton.CMDialogChar(var Message: TCMDialogChar);
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

procedure TTMSFNCCustomEditorButton.ChangeDPIScale(M, D: Integer);
begin
  inherited;

  BeginUpdate;

  FAppearance.FRounding := TTMSFNCUtils.MulDivInt(FAppearance.Rounding, M, D);
  FAppearance.FSelectionLineWidth := TTMSFNCUtils.MulDivSingle(FAppearance.FSelectionLineWidth, M, D);

  FAppearance.FFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FFont.Height, M, D);
  FAppearance.FHoverFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FHoverFont.Height, M, D);
  FAppearance.FDownFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FDownFont.Height, M, D);
  FAppearance.FSelectedFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FSelectedFont.Height, M, D);
  FAppearance.FDisabledFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FDisabledFont.Height, M, D);

//  FAppearance.FStroke.Width := TTMSFNCUtils.MulDivSingle(FAppearance.FStroke.Width, M, D);
  FAppearance.FStroke.Width := FAppearance.FStroke.Width * M / D;
  FAppearance.FSelectedStroke.Width := TTMSFNCUtils.MulDivSingle(FAppearance.FSelectedStroke.Width, M, D);
  FAppearance.FHoverStroke.Width := TTMSFNCUtils.MulDivSingle(FAppearance.FHoverStroke.Width, M, D);
  FAppearance.FDownStroke.Width := TTMSFNCUtils.MulDivSingle(FAppearance.FDownStroke.Width, M, D);
  FAppearance.FDisabledStroke.Width := TTMSFNCUtils.MulDivSingle(FAppearance.FDisabledStroke.Width, M, D);

  EndUpdate;
end;

constructor TTMSFNCCustomEditorButton.Create(AOwner: TComponent);
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

  FAppearance := TTMSFNCEditorButtonAppearance.Create(Self);
  FAppearance.OnChanged := @DoAppearanceChanged;

  FBitmap := TTMSFNCBitmap.Create;
  FBitmap.OnChange := @DoBitmapChanged;
  FBitmapMargins := TTMSFNCMargins.Create;
  FBitmapMargins.OnChange := @DoBitmapMarginsChanged;
  FDisabledBitmap := TTMSFNCBitmap.Create;
  FDisabledBitmap.OnChange := @DoBitmapChanged;

  FText := 'Button';

  FButtonPositionCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FButtonPosition := ebpAlone;

  Width := 80;
  Height := 25;
end;

destructor TTMSFNCCustomEditorButton.Destroy;
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

procedure TTMSFNCCustomEditorButton.DoAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.DoBitmapChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.DoBitmapMarginsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.DoButtonClick;
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Self);
end;

procedure TTMSFNCCustomEditorButton.Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  DrawStandardBackGround(AGraphics, ARect);
  DrawSelectionLine(AGraphics, ARect);
  DrawButtonImage(AGraphics, ARect);
  DrawButtonText(AGraphics,ARect);
end;

procedure TTMSFNCCustomEditorButton.DrawBackground(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
//    inherited;
end;

procedure TTMSFNCCustomEditorButton.DrawButtonImage(AGraphics: TTMSFNCGraphics; ARect: TRectF);
var
  ir: TRectF;
  ar, s: Boolean;
  ro: integer;
  bmp: TTMSFNCBitmap;
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

procedure TTMSFNCCustomEditorButton.DrawButtonText(AGraphics: TTMSFNCGraphics; ARect: TRectF);
var
  txt: string;
  tr: TRectF;
  va, ha: TTMSFNCGraphicsTextAlign;
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

procedure TTMSFNCCustomEditorButton.DrawSelectionLine(AGraphics: TTMSFNCGraphics; ARect: TRectF);
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

procedure TTMSFNCCustomEditorButton.DrawStandardBackGround(AGraphics: TTMSFNCGraphics; ARect: TRectF);
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

procedure TTMSFNCCustomEditorButton.EndUpdate;
begin
  inherited;
  Invalidate;
end;

{$IFDEF FMXLIB}
function TTMSFNCCustomEditorButton.GetAcceleratorChar: Char;
begin
  Result := FAcceleratorChar;
end;

function TTMSFNCCustomEditorButton.GetAcceleratorCharIndex: Integer;
begin
  Result := FAcceleratorCharPos;
end;
{$ENDIF}

procedure TTMSFNCCustomEditorButton.SetButtonPositionCorners;
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

procedure TTMSFNCCustomEditorButton.SetDisabledBitmap(const Value: TTMSFNCBitmap);
begin
  FDisabledBitmap.Assign(Value);
  DoBitmapChanged(Self);
end;

{$IFDEF FMXLIB}
procedure TTMSFNCCustomEditorButton.SetEnabled(const Value: Boolean);
{$ENDIF}
{$IFNDEF FMXLIB}
procedure TTMSFNCCustomEditorButton.SetEnabled(Value: Boolean);
{$ENDIF}
begin
  inherited;

  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
  end;
end;

procedure TTMSFNCCustomEditorButton.HandleAcceleratorKey;
begin
  if Assigned(FOnHandleAcceleratorKey) then
    OnHandleAcceleratorKey(Self)
  else if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TTMSFNCCustomEditorButton.HandleMouseDown(Button: TTMSFNCMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled then
  begin
    FDown := True;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.HandleMouseEnter;
begin
  inherited;
  if Enabled then
  begin
    FHover := True;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.HandleMouseLeave;
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.HandleMouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled and not FHover then
  begin
    FHover := True;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.HandleMouseUp(Button: TTMSFNCMouseButton; Shift: TShiftState; X, Y: Single);
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

procedure TTMSFNCCustomEditorButton.SetAcceleratorChar(AText: string);
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
function TTMSFNCCustomEditorButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Text) and CanFocus then
  begin
    HandleAcceleratorKey;
    Result := true;
  end else
    Result := inherited;
end;
{$ENDIF}

procedure TTMSFNCCustomEditorButton.SetAppearance(const Value: TTMSFNCEditorButtonAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap.Assign(Value);
  DoBitmapChanged(Self);
end;

procedure TTMSFNCCustomEditorButton.SetBitmapMargins(const Value: TTMSFNCMargins);
begin
  FBitmapMargins.Assign(Value);
end;

procedure TTMSFNCCustomEditorButton.SetButtonPosition(const Value: TTMSFNCEditorButtonPosition);
begin
  if FButtonPosition <> Value then
  begin
    FButtonPosition := Value;
    SetButtonPositionCorners;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    if FGroupName <> '' then
      UpdateOtherGroupButtons;
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.SetSelectedByToggle(const AValue: Boolean);
begin
  FSelected := AValue;
  Invalidate;
end;

procedure TTMSFNCCustomEditorButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    SetAcceleratorChar(FText);
    Invalidate;
  end;
end;

procedure TTMSFNCCustomEditorButton.SetToggle(const Value: Boolean);
begin
  if FToggle <> Value then
  begin
    FToggle := Value;
    Invalidate;
  end;
end;

{$IFDEF FMXLIB}
procedure TTMSFNCCustomEditorButton.TriggerAcceleratorKey;
begin
  HandleAcceleratorKey;
end;
{$ENDIF}

procedure TTMSFNCCustomEditorButton.UpdateControlAfterResize;
begin
  inherited;
end;

procedure TTMSFNCCustomEditorButton.UpdateOtherGroupButtons;
var
  I: Integer;
begin
  {$IFDEF FMXLIB}
  for I := 0 to Parent.ChildrenCount - 1 do
  begin
    if (Parent.Children.Items[I] is TTMSFNCCustomEditorButton) and (Parent.Children.Items[I].ComponentIndex <> ComponentIndex) then
    begin
      if ((Parent.Children.Items[I] as TTMSFNCCustomEditorButton).GroupName = FGroupName) and (Parent.Children.Items[I] as TTMSFNCCustomEditorButton).Toggle then
      begin
        (Parent.Children.Items[I] as TTMSFNCCustomEditorButton).SetSelectedByToggle(False);
      end;
    end;
  end;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  for I := 0 to Parent.ControlCount - 1 do
  begin
    if (Parent.Controls[I] is TTMSFNCCustomEditorButton) and (Parent.Controls[I].ComponentIndex <> ComponentIndex) then
    begin
      if ((Parent.Controls[I] as TTMSFNCCustomEditorButton).GroupName = FGroupName) and (Parent.Controls[I] as TTMSFNCCustomEditorButton).Toggle then
      begin
        (Parent.Controls[I] as TTMSFNCCustomEditorButton).SetSelectedByToggle(False);
      end;
    end;
  end;
  {$ENDIF}
end;

{ TTMSFNCEditorButtonAppearance }

procedure TTMSFNCEditorButtonAppearance.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCEditorButtonAppearance then
  begin
    FSelectionLine := (Source as TTMSFNCEditorButtonAppearance).SelectionLine;
    FSelectionLineWidth := (Source as TTMSFNCEditorButtonAppearance).SelectionLineWidth;
    FRounding := (Source as TTMSFNCEditorButtonAppearance).Rounding;
    FFill.Assign((Source as TTMSFNCEditorButtonAppearance).Fill);
    FFont.Assign((Source as TTMSFNCEditorButtonAppearance).Font);
    FStroke.Assign((Source as TTMSFNCEditorButtonAppearance).Stroke);
    FHoverFill.Assign((Source as TTMSFNCEditorButtonAppearance).HoverFill);
    FHoverFont.Assign((Source as TTMSFNCEditorButtonAppearance).HoverFont);
    FHoverStroke.Assign((Source as TTMSFNCEditorButtonAppearance).HoverStroke);
    FDownFill.Assign((Source as TTMSFNCEditorButtonAppearance).DownFill);
    FDownFont.Assign((Source as TTMSFNCEditorButtonAppearance).DownFont);
    FDownStroke.Assign((Source as TTMSFNCEditorButtonAppearance).DownStroke);
    FSelectedFill.Assign((Source as TTMSFNCEditorButtonAppearance).SelectedFill);
    FSelectedFont.Assign((Source as TTMSFNCEditorButtonAppearance).SelectedFont);
    FSelectedStroke.Assign((Source as TTMSFNCEditorButtonAppearance).SelectedStroke);
    FDisabledFill.Assign((Source as TTMSFNCEditorButtonAppearance).DisabledFill);
    FDisabledFont.Assign((Source as TTMSFNCEditorButtonAppearance).DisabledFont);
    FDisabledStroke.Assign((Source as TTMSFNCEditorButtonAppearance).DisabledStroke);
  end
  else
    inherited;
end;

constructor TTMSFNCEditorButtonAppearance.Create(AOwner: TTMSFNCCustomEditorButton);
begin
  FOwner := AOwner;

  FSelectionLine := False;
  FSelectionLineWidth := 4;
  FRounding := 6;

  FFill := TTMSFNCGraphicsFill.Create;
  FFont := TTMSFNCGraphicsFont.Create;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FHoverFont := TTMSFNCGraphicsFont.Create;
  FHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FDownFont := TTMSFNCGraphicsFont.Create;
  FDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FSelectedFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FSelectedFont := TTMSFNCGraphicsFont.Create;
  FSelectedStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FDisabledFill := TTMSFNCGraphicsFill.Create;
  FDisabledFont := TTMSFNCGraphicsFont.Create;
  FDisabledFont.Color := gcMedGray;
  FDisabledStroke := TTMSFNCGraphicsStroke.Create(gskSolid, gcMedGray);

  FFill.OnChanged := @DoFillChanged;
  FFont.OnChanged := @DoFontChanged;
  FStroke.OnChanged := @DoStrokeChanged;
  FDisabledFill.OnChanged := @DoFillChanged;
  FDisabledFont.OnChanged := @DoFontChanged;
  FDisabledStroke.OnChanged := @DoStrokeChanged;
  FHoverFill.OnChanged := @DoFillChanged;
  FHoverFont.OnChanged := @DoFontChanged;
  FHoverStroke.OnChanged := @DoStrokeChanged;
  FDownFill.OnChanged := @DoFillChanged;
  FDownFont.OnChanged := @DoFontChanged;
  FDownStroke.OnChanged := @DoStrokeChanged;
  FSelectedFill.OnChanged := @DoFillChanged;
  FSelectedFont.OnChanged := @DoFontChanged;
  FSelectedStroke.OnChanged := @DoStrokeChanged;
end;

destructor TTMSFNCEditorButtonAppearance.Destroy;
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

procedure TTMSFNCEditorButtonAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorButtonAppearance.DoFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorButtonAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorButtonAppearance.SetDisabledFill(const Value: TTMSFNCGraphicsFill);
begin
  FDisabledFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetDisabledFont(const Value: TTMSFNCGraphicsFont);
begin
  FDisabledFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetDisabledStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FDisabledStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetDownFill(const Value: TTMSFNCGraphicsFill);
begin
  FDownFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetDownFont(const Value: TTMSFNCGraphicsFont);
begin
  FDownFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetDownStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FDownStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetFill(const Value: TTMSFNCGraphicsFill);
begin
  FFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetFont(const Value: TTMSFNCGraphicsFont);
begin
  FFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetHoverFill(const Value: TTMSFNCGraphicsFill);
begin
  FHoverFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetHoverFont(const Value: TTMSFNCGraphicsFont);
begin
  FHoverFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetHoverStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FHoverStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetSelectedFill(const Value: TTMSFNCGraphicsFill);
begin
  FSelectedFill.Assign(Value);
  DoFillChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetSelectedFont(const Value: TTMSFNCGraphicsFont);
begin
  FSelectedFont.Assign(Value);
  DoFontChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetSelectedStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FSelectedStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetSelectionLine(const Value: Boolean);
begin
  if FSelectionLine <> Value then
  begin
    FSelectionLine := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorButtonAppearance.SetSelectionLineWidth(const Value: Single);
begin
  if FSelectionLineWidth <> Value then
  begin
    FSelectionLineWidth := Value;
    DoChanged(Self)
  end;
end;

procedure TTMSFNCEditorButtonAppearance.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    DoChanged(Self)
  end;
end;

procedure TTMSFNCEditorButtonAppearance.SetStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorButtonAppearance.SetTextAlignHorizontal(const Value: TTMSFNCGraphicsTextAlign);
begin
  if FTextAlignHorizontal <> Value then
  begin
    FTextAlignHorizontal := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorButtonAppearance.SetTextAlignVertical(const Value: TTMSFNCGraphicsTextAlign);
begin
  if FTextAlignVertical <> Value then
  begin
    FTextAlignVertical := Value;
    DoChanged(Self);
  end;
end;
end.
