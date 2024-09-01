{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2018                                      }
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

unit AdvColorWheel;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF VCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  UITypes,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  StdCtrls,
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF WINDOWS}
  Messages,
  {$ENDIF}
  {$IFDEF VCLLIB}
  Windows,
  {$ENDIF}
  {$IFDEF LCLLIB}
  LCLIntF, LCLType,
  {$ENDIF}
  ComCtrls, Controls,
  {$ENDIF}
  {$IFDEF WEBLIB}
  WEBLib.ExtCtrls, WEBLib.Controls, Web,
  {$ENDIF}
  Classes, Types, Graphics, AdvCustomControl, AdvImageEx, AdvGraphics,
  AdvGraphicsTypes, AdvTypes, AdvEdit, AdvGraphicsStyles,
  AdvUtils;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  //Version history
  //v1.0.0.0 : First Release
  //v1.0.1.0 : New : Support for high dpi

type
  {$IFDEF FMXLIB}
  TAdvRGBTrackBar = class(TTrackBar)
  private
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    function GetL: Single;
    function GetT: Single;
  public
    constructor Create(AOwner: TComponent); override;
    property Left: Single read GetL write SetLeft;
    property Top: Single read GetT write SetTop;
  end;
  {$ENDIF}

  {$IFDEF CMNLIB}
  TAdvRGBTrackBar = class(TTrackBar)
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvRGBTrackBar = class(TWebTrackBar)
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  private
    {$IFNDEF WEBLIB}
    {$IFDEF WINDOWS}
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    {$ENDIF}
    {$ENDIF}
    procedure SetValue(const Value: Single);
    function GetV: Single;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Single read GetV write SetValue;
  end;
  {$ENDIF}

  {$IFDEF CMNLIB}
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..4095] of TRGBTriple;
  {$ENDIF}

  TAdvImageExColor = class(TAdvImageEx)
  private
    FDPIChanged: Boolean;
    FMouseDown, FImageLoaded: Boolean;
    FPixelColor: TAdvGraphicsColor;
    FOnColorChanged: TNotifyEvent;
    FX: Single;
    FY: Single;
    {$IFDEF FMXLIB}
    FBmpDt: TBitmapData;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FImgDt: TJSImageData;
    FOffS: Integer;
    Fsc: Single;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FBmp: TBitmap;
    FPixels: PRGBTripleArray;
    FRX: Integer;
    {$ENDIF}
    procedure SetPixelColor(const Value: TAdvGraphicsColor);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetCoordinates(AX, AY: Single);
    function PtInColorWheel(X, Y: Single): Boolean;
  protected
    procedure DoAfterDraw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure ColorChanged;
    procedure BitmapLoaded(Sender: TObject);
    procedure HandleMouseDown({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseUp({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    function GetDPIScale: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure MapPixels;
    procedure UnmapPixels;
    function XYToColor(X, Y: Single): TAdvGraphicsColor;
    function GetCenterPoint: TPointF;
    function GetRadius: Single;
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    property PixelColor: TAdvGraphicsColor read FPixelColor write SetPixelColor default gcWhite;
    property OnColorChanged: TNotifyEvent read FOnColorChanged write FOnColorChanged;
  end;

  TAdvColorWheelHSColor = record
    Hue: Double;
    Saturation: Double;
  end;

  TAdvCustomColorWheelBeforeDrawSelectedColorPanelEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARectF: TRectF; ASelectedColor: TAdvGraphicsColor; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvCustomColorWheelAfterDrawSelectedColorPanelEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARectF: TRectF; ASelectedColor: TAdvGraphicsColor) of object;
  TAdvCustomColorWheelSelectedColorChangedEvent = procedure(Sender: TObject; ASelectedColor: TAdvGraphicsColor) of object;
  TAdvCustomColorWheelRValueChangedEvent = procedure(Sender: TObject; AR: Integer; ASelectedColor: TAdvGraphicsColor) of object;
  TAdvCustomColorWheelGValueChangedEvent = procedure(Sender: TObject; AG: Integer; ASelectedColor: TAdvGraphicsColor) of object;
  TAdvCustomColorWheelBValueChangedEvent = procedure(Sender: TObject; AB: Integer; ASelectedColor: TAdvGraphicsColor) of object;

  TAdvCustomColorWheel = class(TAdvCustomControl)
  private
    FImage: TAdvImageExColor;
    FREdit: TAdvEdit;
    FGEdit: TAdvEdit;
    FBEdit: TAdvEdit;
    FHEXEdit: TAdvEdit;
    FRTrackBar: TAdvRGBTrackBar;
    FGTrackBar: TAdvRGBTrackBar;
    FBTrackBar: TAdvRGBTrackBar;
    FSelectedColor: TAdvGraphicsColor;
    FGValue: Integer;
    FRValue: Integer;
    FBValue: Integer;
    FHSColor: TAdvColorWheelHSColor;
    FOnBValueChanged: TAdvCustomColorWheelBValueChangedEvent;
    FOnGValueChanged: TAdvCustomColorWheelGValueChangedEvent;
    FOnAfterDrawSelectedColorPanel: TAdvCustomColorWheelAfterDrawSelectedColorPanelEvent;
    FOnSelectedColorChanged: TAdvCustomColorWheelSelectedColorChangedEvent;
    FOnRValueChanged: TAdvCustomColorWheelRValueChangedEvent;
    FOnBeforeDrawSelectedColorPanel: TAdvCustomColorWheelBeforeDrawSelectedColorPanelEvent;
    FFont: TAdvGraphicsFont;
    procedure SetSelectedColor(const Value: TAdvGraphicsColor);
    procedure SetBValue(const Value: Integer);
    procedure SetGValue(const Value: Integer);
    procedure SetRValue(const Value: Integer);
    procedure SetHEXValue(const Value: string);
    function GetHEXValue: string;
    function IsHEXValueStored: Boolean;
    procedure SetFnt(const Value: TAdvGraphicsFont);
  protected
    procedure Loaded; override;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawColorPanel({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF);
    procedure DrawText({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF);
    {$IFDEF FMXLIB}
    procedure RFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure GFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure BFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure HEXFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure RFieldKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure GFieldKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure BFieldKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure HEXFieldKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    {$ENDIF}
    procedure DoBeforeDrawSelectedColorPanel(AGraphics: TAdvGraphics; ARectF: TRectF; ASelectedColor: TAdvGraphicsColor; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawSelectedColorPanel(AGraphics: TAdvGraphics; ARectF: TRectF; ASelectedColor: TAdvGraphicsColor); virtual;
    procedure DoSelectedColorChanged(ASelectedColor: TAdvGraphicsColor); virtual;
    procedure DoRValueChanged(AR: Integer; ASelectedColor: TAdvGraphicsColor); virtual;
    procedure DoGValueChanged(AG: Integer; ASelectedColor: TAdvGraphicsColor); virtual;
    procedure DoBValueChanged(AB: Integer; ASelectedColor: TAdvGraphicsColor); virtual;
    procedure ColorChanged(Sender: TObject);
    procedure RTrackBarChanged(Sender: TObject);
    procedure GTrackBarChanged(Sender: TObject);
    procedure BTrackBarChanged(Sender: TObject);
    procedure FillColorChanged(Sender: TObject);
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;
    procedure UpdateControlAfterResize; override;
    procedure SetIndicator;
    function GetVersion: string; override;
    function RGBToHS(AR, AG, AB: Integer): TAdvColorWheelHSColor;
    property Font: TAdvGraphicsFont read FFont write SetFnt;
    property SelectedColor: TAdvGraphicsColor read FSelectedColor write SetSelectedColor default gcWhite;
    property RValue: Integer read FRValue write SetRValue default 255;
    property BValue: Integer read FBValue write SetBValue default 255;
    property GValue: Integer read FGValue write SetGValue default 255;
    property HEXValue: string read GetHEXValue write SetHEXValue stored IsHEXValueStored nodefault;
    property OnBeforeDrawSelectedColorPanel: TAdvCustomColorWheelBeforeDrawSelectedColorPanelEvent read FOnBeforeDrawSelectedColorPanel write FOnBeforeDrawSelectedColorPanel;
    property OnAfterDrawSelectedColorPanel: TAdvCustomColorWheelAfterDrawSelectedColorPanelEvent read FOnAfterDrawSelectedColorPanel write FOnAfterDrawSelectedColorPanel;
    property OnSelectedColorChanged: TAdvCustomColorWheelSelectedColorChangedEvent read FOnSelectedColorChanged write FOnSelectedColorChanged;
    property OnRValueChanged: TAdvCustomColorWheelRValueChangedEvent read FOnRValueChanged write FOnRValueChanged;
    property OnGValueChanged: TAdvCustomColorWheelGValueChangedEvent read FOnGValueChanged write FOnGValueChanged;
    property OnBValueChanged: TAdvCustomColorWheelBValueChangedEvent read FOnBValueChanged write FOnBValueChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ColorToRValue(AColor: TAdvGraphicsColor): Integer;
    function ColorToGValue(AColor: TAdvGraphicsColor): Integer;
    function ColorToBValue(AColor: TAdvGraphicsColor): Integer;
    function RGBToGraphicsColor(R, G, B: Integer): TAdvGraphicsColor;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvColorWheel = class(TAdvCustomColorWheel)
  published
    property Font;
    property Fill;
    property Stroke;
    property SelectedColor;
    property RValue;
    property BValue;
    property GValue;
    property HEXValue;
    property OnBeforeDrawSelectedColorPanel;
    property OnAfterDrawSelectedColorPanel;
    property OnSelectedColorChanged;
    property OnRValueChanged;
    property OnGValueChanged;
    property OnBValueChanged;
  end;

implementation

uses
  Math, SysUtils, AdvColorWheelResources;

{$R AdvColorWheel.res}

{ TAdvCustomColorWheel }

function TAdvCustomColorWheel.ColorToBValue(
  AColor: TAdvGraphicsColor): Integer;
begin
  {$IFDEF FMXLIB}
  Result := AColor and $000000FF;
  {$ELSE}
  Result := AColor and $00FF0000 shr 16;
  {$ENDIF}
end;

function TAdvCustomColorWheel.ColorToGValue(
  AColor: TAdvGraphicsColor): Integer;
begin
  Result := AColor and $0000FF00 shr 8;
end;

function TAdvCustomColorWheel.ColorToRValue(
  AColor: TAdvGraphicsColor): Integer;
begin
  {$IFDEF FMXLIB}
  Result := AColor and $00FF0000 shr 16;
  {$ELSE}
  Result := AColor and $000000FF;
  {$ENDIF}
end;

constructor TAdvCustomColorWheel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 350;
  Height := 175;
  FRValue := 255;
  FGValue := 255;
  FBValue := 255;
  FSelectedColor := gcWhite;
  Fill.OnChanged := FillColorChanged;

  FFont := TAdvGraphicsFont.Create;

  FImage := TAdvImageExColor.Create(Self);
  FImage.Parent := Self;
  {$IFDEF FMXLIB}
  FImage.Stored := False;
  {$ENDIF}
  FImage.Bitmaps.AddBitmapFromResource(GetColorWheelResource, HInstance, 1.0);
  {$IFNDEF WEBLIB}
  FImage.Bitmaps.AddBitmapFromResource(GetColorWheelResource125, HInstance, 1.25);
  FImage.Bitmaps.AddBitmapFromResource(GetColorWheelResource150, HInstance, 1.5);
  FImage.Bitmaps.AddBitmapFromResource(GetColorWheelResource175, HInstance, 1.75);
  FImage.Bitmaps.AddBitmapFromResource(GetColorWheelResource200, HInstance, 2);
  {$ENDIF}
  FImage.Top := Round((Height / 2) - (FImage.Height / 2));
  FImage.Left := 10;
  FImage.OnColorChanged := ColorChanged;
  FImage.Stretch := True;

  FREdit := TAdvEdit.Create(Self);
  FREdit.Parent := Self;
  FREdit.Width := 35;
  {$IFDEF FMXLIB}
  FREdit.Stored := False;
  FREdit.Position.X := Width - FREdit.Width - 15;
  FREdit.Position.Y := {$IFDEF FMXMOBILE} 10 {$ELSE} 15 {$ENDIF};
  {$ELSE}
  FREdit.Left := Width - FREdit.Width - 15;
  FREdit.Top := 15;
  {$ENDIF}
  FREdit.EditType := etNumeric;
  FREdit.OnKeyDown := RFieldKeyDown;
  FREdit.MaxLength := 3;
  FREdit.Text := '255';

  FGEdit := TAdvEdit.Create(Self);
  FGEdit.Parent := Self;
  FGEdit.Width := 35;
  {$IFDEF FMXLIB}
  FGEdit.Stored := False;
  FGEdit.Position.X := Width - FGEdit.Width - 15;
  FGEdit.Position.Y := {$IFDEF FMXMOBILE} 40 {$ELSE} 45 {$ENDIF};
  {$ELSE}
  FGEdit.Left := Width - FGEdit.Width - 15;
  FGEdit.Top := 45;
  {$ENDIF}
  FGEdit.EditType := etNumeric;
  FGEdit.OnKeyDown := GFieldKeyDown;
  FGEdit.MaxLength := 3;
  FGEdit.Text := '255';

  FBEdit := TAdvEdit.Create(Self);
  FBEdit.Parent := Self;
  FBEdit.Width := 35;
  {$IFDEF FMXLIB}
  FBEdit.Stored := False;
  FBEdit.Position.X := Width - FBEdit.Width - 15;
  FBEdit.Position.Y := {$IFDEF FMXMOBILE} 70 {$ELSE} 75 {$ENDIF};
  {$ELSE}
  FBEdit.Left := Width - FBEdit.Width - 15;
  FBEdit.Top := 75;
  {$ENDIF}
  FBEdit.EditType := etNumeric;
  FBEdit.OnKeyDown := BFieldKeyDown;
  FBEdit.MaxLength := 3;
  FBEdit.Text := '255';

  FHEXEdit := TAdvEdit.Create(Self);
  FHEXEdit.Parent := Self;
  FHEXEdit.Width := {$IFDEF FMXMOBILE} 105 {$ELSE} 125 {$ENDIF};
  {$IFDEF FMXLIB}
  FHEXEdit.Stored := False;
  FHEXEdit.Position.X := {$IFDEF FMXMOBILE} 230 {$ELSE} 210 {$ENDIF};
  FHEXEdit.Position.Y := {$IFDEF FMXMOBILE} 100 {$ELSE} 105 {$ENDIF};
  {$ELSE}
  FHEXEdit.Left := 210;
  FHEXEdit.Top := 105;
  {$ENDIF}
  FHEXEdit.EditType := etHex;
  FHEXEdit.OnKeyDown := HEXFieldKeyDown;
  FHEXEdit.MaxLength := 6;
  FHEXEdit.Text := 'FFFFFF';

  FRTrackBar := TAdvRGBTrackBar.Create(Self);
  FRTrackBar.Parent := Self;
  FRTrackBar.Left := 195;
  FRTrackBar.Top := {$IFDEF FMXLIB}{$IFNDEF IOS}FREdit.Position.Y + 1{$ENDIF}{$IFDEF IOS}FREdit.Position.Y - 3{$ENDIF}{$ELSE}FREdit.Top - 1{$ENDIF};
  FRTrackBar.Value := 255;
  FRTrackBar.OnChange := RTrackBarChanged;
  {$IFDEF FMXLIB}
  FRTrackBar.Stored := False;
  {$ENDIF}

  FGTrackBar := TAdvRGBTrackBar.Create(Self);
  FGTrackBar.Parent := Self;
  FGTrackBar.Left := 195;
  FGTrackBar.Top := {$IFDEF FMXLIB}{$IFNDEF IOS}FGEdit.Position.Y + 1{$ENDIF}{$IFDEF IOS}FGEdit.Position.Y - 3{$ENDIF}{$ELSE}FGEdit.Top - 1{$ENDIF};
  FGTrackBar.Value := 255;
  FGTrackBar.OnChange := GTrackBarChanged;
  {$IFDEF FMXLIB}
  FGTrackBar.Stored := False;
  {$ENDIF}

  FBTrackBar := TAdvRGBTrackBar.Create(Self);
  FBTrackBar.Parent := Self;
  FBTrackBar.Left := 195;
  FBTrackBar.Top := {$IFDEF FMXLIB}{$IFNDEF IOS}FBEdit.Position.Y + 1{$ENDIF}{$IFDEF IOS}FBEdit.Position.Y - 3{$ENDIF}{$ELSE}FBEdit.Top - 1{$ENDIF};
  FBTrackBar.Value := 255;
  FBTrackBar.OnChange := BTrackBarChanged;
  {$IFDEF FMXLIB}
  FBTrackBar.Stored := False;
  {$ENDIF}
end;

procedure TAdvCustomColorWheel.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
    Fill.Color := c;
end;

procedure TAdvCustomColorWheel.BTrackBarChanged(Sender: TObject);
begin
  BValue := Round(FBTrackBar.Value);
end;

procedure TAdvCustomColorWheel.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  Font.Height := TAdvUtils.MulDivInt(Font.Height, M, D);
  SetIndicator;
end;

procedure TAdvCustomColorWheel.ColorChanged(Sender: TObject);
begin
  SelectedColor := FImage.PixelColor;
end;

destructor TAdvCustomColorWheel.Destroy;
begin
  FImage.Free;
  FREdit.Free;
  FGEdit.Free;
  FBEdit.Free;
  FRTrackBar.Free;
  FGTrackBar.Free;
  FBTrackBar.Free;
  FFont.Free;
  inherited;
end;

procedure TAdvCustomColorWheel.DoAfterDrawSelectedColorPanel(
  AGraphics: TAdvGraphics; ARectF: TRectF;
  ASelectedColor: TAdvGraphicsColor);
begin
  if Assigned(OnAfterDrawSelectedColorPanel) then
    OnAfterDrawSelectedColorPanel(Self, AGraphics, ARectF, ASelectedColor);
end;

procedure TAdvCustomColorWheel.DoBeforeDrawSelectedColorPanel(
  AGraphics: TAdvGraphics; ARectF: TRectF;
  ASelectedColor: TAdvGraphicsColor; var AAllow, ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawSelectedColorPanel) then
    OnBeforeDrawSelectedColorPanel(Self, AGraphics, ARectF, ASelectedColor, AAllow, ADefaultDraw);
end;

procedure TAdvCustomColorWheel.DoBValueChanged(AB: Integer;
  ASelectedColor: TAdvGraphicsColor);
begin
  if Assigned(OnBValueChanged) then
    OnBValueChanged(Self, AB, ASelectedColor);
end;

procedure TAdvCustomColorWheel.DoGValueChanged(AG: Integer;
  ASelectedColor: TAdvGraphicsColor);
begin
  if Assigned(OnGValueChanged) then
    OnGValueChanged(Self, AG, ASelectedColor);
end;

procedure TAdvCustomColorWheel.DoRValueChanged(AR: Integer;
  ASelectedColor: TAdvGraphicsColor);
begin
  if Assigned(OnRValueChanged) then
    OnRValueChanged(Self, AR, ASelectedColor);
end;

procedure TAdvCustomColorWheel.DoSelectedColorChanged(
  ASelectedColor: TAdvGraphicsColor);
begin
  if Assigned(OnSelectedColorChanged) then
    OnSelectedColorChanged(Self, ASelectedColor);
end;

procedure TAdvCustomColorWheel.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  inherited;
  DrawColorPanel(AGraphics, ARect);
  DrawText(AGraphics, ARect);
end;

procedure TAdvCustomColorWheel.DrawColorPanel(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  r: TRectF;
  b, df: Boolean;
  h: Single;
begin
  {$IFDEF FMXLIB}
  h := 135;
  {$ENDIF}
  {$IFDEF VCLLIB}
  h := ScalePaintValue(13) + FHEXEdit.Top + FHEXEdit.Height;
  {$ENDIF}
  {$IFDEF LCLLIB}
  h := 45 + FREdit.Height + FBEdit.Height + FGEdit.Height + FHEXEdit.Height;
  {$ENDIF}
  {$IFDEF WEBLIB}
  h := 35 + FREdit.Height + FBEdit.Height + FGEdit.Height + FHEXEdit.Height;
  {$ENDIF}
  r := RectF(0, 0, ScalePaintValue(25), ScalePaintValue(25));
  OffsetRectEx(r, ScalePaintValue(30) + FImage.Width, h);
  {$IFDEF FMXLIB}
  r.Right := FREdit.Position.X + FREdit.Width;
  {$ELSE}
  r.Right := FREdit.Left + FREdit.Width;
  {$ENDIF}

  AGraphics.Fill.Color := SelectedColor;

  b := True;
  df := True;
  DoBeforeDrawSelectedColorPanel(AGraphics, r, FSelectedColor, b, df);
  if b then
  begin
    if df then
      AGraphics.DrawRectangle(r);

    DoAfterDrawSelectedColorPanel(AGraphics, r, FSelectedColor);
  end;
end;

procedure TAdvCustomColorWheel.DrawText(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  AGraphics.Font.AssignSource(Font);
  {$IFDEF FMXLIB}
  {$IFDEF FMXMOBILE}
  AGraphics.Font.Size := 20;
  AGraphics.DrawText(PointF(30 + FImage.Width, FREdit.Position.Y + 5), 'R');
  AGraphics.DrawText(PointF(30 + FImage.Width, FGEdit.Position.Y + 5), 'G');
  AGraphics.DrawText(PointF(30 + FImage.Width, FBEdit.Position.Y + 5), 'B');
  AGraphics.DrawText(PointF(30 + FImage.Width, FHEXEdit.Position.Y + 5), 'HEX');
  {$ENDIF}
  {$IFNDEF FMXMOBILE}
  AGraphics.DrawText(PointF(30 + FImage.Width, FREdit.Position.Y + 3), 'R');
  AGraphics.DrawText(PointF(30 + FImage.Width, FGEdit.Position.Y + 3), 'G');
  AGraphics.DrawText(PointF(30 + FImage.Width, FBEdit.Position.Y + 3), 'B');
  AGraphics.DrawText(PointF(30 + FImage.Width, FHEXEdit.Position.Y + 3), 'HEX');
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF FMXLIB}
  {$IFDEF WEBLIB}
  AGraphics.DrawText(PointF(30 + FImage.Width, FREdit.Top + 7), 'R');
  AGraphics.DrawText(PointF(30 + FImage.Width, FGEdit.Top + 7), 'G');
  AGraphics.DrawText(PointF(30 + FImage.Width, FBEdit.Top + 7), 'B');
  AGraphics.DrawText(PointF(30 + FImage.Width, FHEXEdit.Top + 7), 'HEX');
  {$ENDIF}
  {$IFNDEF WEBLIB}
  AGraphics.DrawText(PointF(ScalePaintValue(30) + FImage.Width, FREdit.Top + 3), 'R');
  AGraphics.DrawText(PointF(ScalePaintValue(30) + FImage.Width, FGEdit.Top + 3), 'G');
  AGraphics.DrawText(PointF(ScalePaintValue(30) + FImage.Width, FBEdit.Top + 3), 'B');
  AGraphics.DrawText(PointF(ScalePaintValue(30) + FImage.Width, FHEXEdit.Top + 3), 'HEX');
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvCustomColorWheel.FillColorChanged(Sender: TObject);
begin
  FillChanged(Self);
  {$IFDEF CMNWEBLIB}
  FImage.Fill.Color := Color;
  {$ENDIF}
end;

function TAdvCustomColorWheel.GetHEXValue: string;
begin
  Result := FHEXEdit.Text;
end;

function TAdvCustomColorWheel.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomColorWheel.GTrackBarChanged(Sender: TObject);
begin
  GValue := Round(FGTrackBar.Value);
end;

function TAdvCustomColorWheel.IsHEXValueStored: Boolean;
begin
  Result := GetHEXValue <> 'FFFFFF';
end;

procedure TAdvCustomColorWheel.Loaded;
begin
  inherited;
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  FImage.ScaleForPPI(CurrentPPI);
  FREdit.ScaleForPPI(CurrentPPI);
  FGEdit.ScaleForPPI(CurrentPPI);
  FBEdit.ScaleForPPI(CurrentPPI);
  FHEXEdit.ScaleForPPI(CurrentPPI);
  FRTrackBar.ScaleForPPI(CurrentPPI);
  FGTrackBar.ScaleForPPI(CurrentPPI);
  FBTrackBar.ScaleForPPI(CurrentPPI);

  FREdit.AutoSize := True;
  FGEdit.AutoSize := True;
  FBEdit.AutoSize := True;
  FHEXEdit.AutoSize := True;
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
end;

procedure TAdvCustomColorWheel.ResetToDefaultStyle;
begin
  inherited;
  Fill.Color := gcWhite;
end;

function TAdvCustomColorWheel.RGBToGraphicsColor(R, G,
  B: Integer): TAdvGraphicsColor;
{$IFDEF FMXLIB}
var
  col: TAlphaColorRec;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  col.R := R;
  col.G := G;
  col.B := B;
  col.A := 255;
  Result := col.Color;
  {$ELSE}
  Result := RGB(R, G, B);
  {$ENDIF}
end;

function TAdvCustomColorWheel.RGBToHS(AR, AG,
  AB: Integer): TAdvColorWheelHSColor;
var
  minRGB, maxRGB, delta, h, s: Double;
begin
  h := 0.0;
  maxRGB := Max(Max(AR, AG), AB);
  minRGB := Min(Min(AR, AG), AB);
  delta := maxRGB - minRGB;

  if maxRGB = 0 then
    s := 0.0
  else
    s := delta / maxRGB;

  if delta = 0 then
    h := 0.0
  else if maxRGB = AR then
    h := (AG - AB) / delta
  else if maxRGB = AG then
    h := (AB - AR) / delta + 2
  else if maxRGB = AB then
    h := (AR - AG) / delta + 4;

  h := 60 * h;

  with Result do
  begin
    Hue := h;
    Saturation := s;
  end;
end;

procedure TAdvCustomColorWheel.RTrackBarChanged(Sender: TObject);
begin
  RValue := Round(FRTrackBar.Value);
end;

{$IFDEF FMXLIB}
procedure TAdvCustomColorWheel.RFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomColorWheel.RFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  v: Integer;
begin
  if Key = KEY_RETURN then
  begin
    v := FREdit.IntValue;
    if v < 0 then
      v := 0
    else if v > 255 then
      v := 255;

    if v = RValue then
    begin
      FREdit.Text := IntToStr(v);
    end
    else
    begin
      SelectedColor := RGBToGraphicsColor(v, FGValue, FBValue);
      FHSColor := RGBToHS(v, FGValue, FBValue);
      SetIndicator;
    end;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomColorWheel.GFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomColorWheel.GFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  v: Integer;
begin
  if Key = KEY_RETURN then
  begin
    v := FGEdit.IntValue;
    if v < 0 then
      v := 0
    else if v > 255 then
      v := 255;

    if v = GValue then
    begin
      FGEdit.Text := IntToStr(v);
    end
    else
    begin
      SelectedColor := RGBToGraphicsColor(FRValue, v, FBValue);
      FHSColor := RGBToHS(FRValue, v, FBValue);
      SetIndicator;
    end;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomColorWheel.BFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomColorWheel.BFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  v: Integer;
begin
  if Key = KEY_RETURN then
  begin
    v := FBEdit.IntValue;
    if v < 0 then
      v := 0
    else if v > 255 then
      v := 255;

    if v = BValue then
    begin
      FBEdit.Text := IntToStr(v);
    end
    else
    begin
      SelectedColor := RGBToGraphicsColor(FRValue, FGValue, v);
      FHSColor := RGBToHS(FRValue, FGValue, v);
      SetIndicator;
    end;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomColorWheel.HEXFieldKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomColorWheel.HEXFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  str: string;
  r, g, b: Integer;
begin
  if (Key = KEY_RETURN) and (Length(FHEXEdit.Text) = 6) then
  begin
    str := Copy(FHEXEdit.Text, 1, 2);
    r := StrToInt('$' + str);
    str := Copy(FHEXEdit.Text, 3, 2);
    g := StrToInt('$' + str);
    str := Copy(FHEXEdit.Text, 5, 2);
    b := StrToInt('$' + str);
    SelectedColor := RGBToGraphicsColor(r, g, b);
    FHSColor := RGBToHS(r, g, b);
    SetIndicator;
  end;
end;

procedure TAdvCustomColorWheel.SetBValue(const Value: Integer);
begin
  if FBValue <> Value then
  begin
    FBValue := Value;
    FBEdit.Text := IntToStr(Value);
    FHEXEdit.Text := IntToHex(FRValue, 2) + IntToHex(FGValue, 2) + IntToHex(FBValue, 2);
    FBTrackBar.Value := FBValue;
    FSelectedColor := RGBToGraphicsColor(FRValue, FGValue, FBValue);
    FHSColor := RGBToHS(FRValue, FGValue, FBValue);
    SetIndicator;
    DoBValueChanged(FBValue, FSelectedColor);
    DoSelectedColorChanged(FSelectedColor);
    Invalidate;
  end;
end;

procedure TAdvCustomColorWheel.SetFnt(const Value: TAdvGraphicsFont);
begin
  FFont.AssignSource(Value);
end;

procedure TAdvCustomColorWheel.SetGValue(const Value: Integer);
begin
  if FGValue <> Value then
  begin
    FGValue := Value;
    FGEdit.Text := IntToStr(Value);
    FHEXEdit.Text := IntToHex(FRValue, 2) + IntToHex(FGValue, 2) + IntToHex(FBValue, 2);
    FGTrackBar.Value := FGValue;
    FSelectedColor := RGBToGraphicsColor(FRValue, FGValue, FBValue);
    FHSColor := RGBToHS(FRValue, FGValue, FBValue);
    SetIndicator;
    DoGValueChanged(FGValue, FSelectedColor);
    DoSelectedColorChanged(FSelectedColor);
    Invalidate;
  end;
end;

procedure TAdvCustomColorWheel.SetHEXValue(const Value: string);
var
  str: string;
  r, g, b: Integer;
begin
  if Length(Value) = 6 then
  begin
    str := Copy(Value, 1, 2);
    r := StrToInt('$' + str);
    str := Copy(Value, 3, 2);
    g := StrToInt('$' + str);
    str := Copy(Value, 5, 2);
    b := StrToInt('$' + str);
    SelectedColor := RGBToGraphicsColor(r, g, b);
    FHSColor := RGBToHS(r, g, b);
    SetIndicator;
  end;
end;

procedure TAdvCustomColorWheel.SetIndicator;
var
  angle, r: Double;
  cp: TPointF;
begin
  cp := FImage.GetCenterPoint;
  r := FImage.GetRadius - 0.5;
  angle := DegToRad(FHSColor.Hue);
  FImage.X := cp.X + (r * FHSColor.Saturation) * Cos(angle);
  FImage.Y := cp.Y + (r * FHSColor.Saturation) * Sin(angle);
  FImage.Invalidate;
end;

procedure TAdvCustomColorWheel.SetRValue(const Value: Integer);
begin
  if FRValue <> Value then
  begin
    FRValue := Value;
    FREdit.Text := IntToStr(Value);
    FHEXEdit.Text := IntToHex(FRValue, 2) + IntToHex(FGValue, 2) + IntToHex(FBValue, 2);
    FRTrackBar.Value := FRValue;
    FSelectedColor := RGBToGraphicsColor(FRValue, FGValue, FBValue);
    FHSColor := RGBToHS(FRValue, FGValue, FBValue);
    SetIndicator;
    DoRValueChanged(FRValue, FSelectedColor);
    DoSelectedColorChanged(FSelectedColor);
    Invalidate;
  end;
end;

procedure TAdvCustomColorWheel.SetSelectedColor(
  const Value: TAdvGraphicsColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    FRValue := ColorToRValue(FSelectedColor);
    FREdit.Text := IntToStr(FRValue);
    FGValue := ColorToGValue(FSelectedColor);
    FGEdit.Text := IntToStr(FGValue);
    FBValue := ColorToBValue(FSelectedColor);
    FBEdit.Text := IntToStr(FBValue);
    FHEXEdit.Text := IntToHex(FRValue, 2) + IntToHex(FGValue, 2) + IntToHex(FBValue, 2);

    FRTrackBar.OnChange := nil;
    if FRTrackBar.Value <> FRValue then
    begin
      FRTrackBar.Value := FRValue;
      DoRValueChanged(FRValue, FSelectedColor);
    end;
    FRTrackBar.OnChange := RTrackBarChanged;

    FGTrackBar.OnChange := nil;
    if FGTrackBar.Value <> FGValue then
    begin
      FGTrackBar.Value := FGValue;
      DoGValueChanged(FGValue, FSelectedColor);
    end;
    FGTrackBar.OnChange := GTrackBarChanged;

    FBTrackBar.OnChange := nil;
    if FBTrackBar.Value <> FBValue then
    begin
      FBTrackBar.Value := FBValue;
      DoBValueChanged(FBValue, FSelectedColor);
    end;
    FBTrackBar.OnChange := BTrackBarChanged;

    FHSColor := RGBToHS(FRValue, FGValue, FBValue);
    SetIndicator;

    DoSelectedColorChanged(FSelectedColor);
    Invalidate;
  end;
end;

procedure TAdvCustomColorWheel.UpdateControlAfterResize;
begin
  inherited;
  if Assigned(FImage) then
    FImage.Top := Round((Height / 2) - (FImage.Height / 2));
end;

{ TAdvRGBTrackBar }

{$IFDEF FMXLIB}
constructor TAdvRGBTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 255;
end;

function TAdvRGBTrackBar.GetL: Single;
begin
  Result := Position.X;
end;

function TAdvRGBTrackBar.GetT: Single;
begin
  Result := Position.Y;
end;

procedure TAdvRGBTrackBar.SetLeft(const Value: Single);
begin
  Position.X := Value;
end;

procedure TAdvRGBTrackBar.SetTop(const Value: Single);
begin
  Position.Y := Value;
end;
{$ENDIF}

{$IFDEF CMNWEBLIB}
constructor TAdvRGBTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF CMNLIB}
  TickStyle := tsNone;
  Height := 22;
  {$ENDIF}
  Max := 255;
  Width := 100;
end;

{$IFNDEF WEBLIB}
{$IFDEF WINDOWS}
procedure TAdvRGBTrackBar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  Width := Width + 1;
  Width := Width - 1;
end;
{$ENDIF}
{$ENDIF}

function TAdvRGBTrackBar.GetV: Single;
begin
  Result := Position;
end;

procedure TAdvRGBTrackBar.SetValue(const Value: Single);
begin
  Position := Round(Value);
end;
{$ENDIF}

{ TAdvImageExColor }

procedure TAdvImageExColor.BitmapLoaded(Sender: TObject);
begin
  FImageLoaded := True;
end;

procedure TAdvImageExColor.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  FDPIChanged := True;
end;

procedure TAdvImageExColor.ColorChanged;
begin
  if Assigned(OnColorChanged) then
    OnColorChanged(Self);
end;

constructor TAdvImageExColor.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 150;
  X := Width / 2 * GetDPIScale;
  Y := Height / 2 * GetDPIScale;
  FImageLoaded := False;
  FDPIChanged := False;
  Stroke.Kind := gskNone;
  {$IFDEF FMXLIB}
  Fill.Kind := gfkNone;
  {$ENDIF}

  Self.OnBitmapChanged := BitmapLoaded;
end;

destructor TAdvImageExColor.Destroy;
begin
  UnmapPixels;
  inherited;
end;

procedure TAdvImageExColor.DoAfterDraw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  inherited;
  if FDPIChanged then
  begin
    FDPIChanged := False;
    MapPixels;
  end;
end;

procedure TAdvImageExColor.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  inherited;
  if FImageLoaded then
  begin
    FImageLoaded := False;
    MapPixels;
  end;
  AGraphics.Stroke.Kind := gskSolid;
  AGraphics.Fill.Kind := gfkNone;
  AGraphics.Stroke.Color := gcBlack;
  AGraphics.DrawEllipse(X - ScalePaintValue(5), Y - ScalePaintValue(5), X + ScalePaintValue(5), Y + ScalePaintValue(5));
  AGraphics.Stroke.Color := gcWhite;
  AGraphics.DrawEllipse(X - ScalePaintValue(4), Y - ScalePaintValue(4), X + ScalePaintValue(4), Y + ScalePaintValue(4));
end;

function TAdvImageExColor.GetCenterPoint: TPointF;
begin
  Result := PointF(Width / 2, Height / 2);
end;

function TAdvImageExColor.GetDPIScale: Single;
begin
  Result := PaintScaleFactor;
end;

function TAdvImageExColor.GetRadius: Single;
begin
  Result := Width / 2;
end;

procedure TAdvImageExColor.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  CaptureEx;
  FMouseDown := True;
  if PtInColorWheel(X, Y) then
  begin
    PixelColor := XYToColor(X, Y);
    ColorChanged;
    Invalidate;
  end;
end;

procedure TAdvImageExColor.HandleMouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseDown then
  begin
    if PtInColorWheel(X, Y) then
    begin
      PixelColor := XYToColor(X, Y);
    end
    else
    begin
      SetCoordinates(X, Y);
      PixelColor := XYToColor(FX, FY);
    end;
    ColorChanged;
    Invalidate;
  end;
end;

procedure TAdvImageExColor.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseDown := False;
  ReleaseCaptureEx;
end;

procedure TAdvImageExColor.MapPixels;
begin
  {$IFDEF FMXLIB}
  Bitmaps.Items[0].Bitmap.Map(TMapAccess.Read, FBmpDt);
  {$ENDIF}

  {$IFDEF WEBLIB}
  if Width <> 0 then
    Fsc := Round((Canvas.Element.width / Width) * 100) / 100
  else
    Fsc := 1.0;

  FImgDt := Canvas.Context.getImageData(0, 0, Round(Width * Fsc), Round(Height * Fsc));
  {$ENDIF}

  {$IFDEF CMNLIB}
  FBmp := TBitmap.Create;
  FBmp.Width := Width;
  FBmp.Height := Height;
  FBmp.PixelFormat := pf24bit;
  FBmp.Canvas.CopyRect(Rect(0, 0, Width, Height), Canvas, Rect(0, 0, Width, Height));
  {$ENDIF}
end;

function TAdvImageExColor.PtInColorWheel(X, Y: Single): Boolean;
begin
  Result := (Sqr(X - 75 * GetDPIScale) + Sqr(Y - 75 * GetDPIScale)) <= Sqr(74 * GetDPIScale);
end;

procedure TAdvImageExColor.SetCoordinates(AX, AY: Single);
var
  a, b, angle, r: Single;
  cp: TPointF;
begin
  cp := GetCenterPoint;
  r := GetRadius - 1 {$IFDEF WEBLIB}* Fsc{$ENDIF};
  a := Abs(cp.X - AY);
  b := Abs(cp.Y - AX);
  if (a = 0) then
    a := 1;

  if (b = 0) then
    b := 1;

  angle := ArcTan(a / b) * 180 / PI;

  if (AX >= cp.X) and (AY <= cp.Y) then
    angle := 360 - angle
  else if (AX <= cp.X) and (AY >= cp.Y) then
    angle := 180 - angle
  else if (AX <= cp.X) and (AY <= cp.Y) then
    angle := 180 + angle;

  angle := DegToRad(angle);
  FX := (cp.X - 0.5) + r * Cos(angle);
  FY := (cp.Y - 0.5) + r * Sin(angle);
end;

procedure TAdvImageExColor.SetPixelColor(const Value: TAdvGraphicsColor);
begin
  if FPixelColor <> Value then
    FPixelColor := Value;
end;

procedure TAdvImageExColor.SetX(const Value: Single);
begin
  if FX <> Value then
    FX := Value;
end;

procedure TAdvImageExColor.SetY(const Value: Single);
begin
  if FY <> Value then
    FY := Value;
end;

procedure TAdvImageExColor.UnmapPixels;
begin
  {$IFDEF FMXLIB}
  Bitmaps.Items[0].Bitmap.Unmap(FBmpDt);
  {$ENDIF}

  {$IFDEF CMNLIB}
  FBmp.Free;
  {$ENDIF}
end;

function TAdvImageExColor.XYToColor(X, Y: Single): TAdvGraphicsColor;
begin
  {$IFDEF FMXLIB}
  Result := FBmpDt.GetPixel(Round(X), Round(Y))
  {$ENDIF}

  {$IFDEF CMNLIB}
  FRX := Round(X);
  FPixels := FBmp.Scanline[Round(Y)];
  {$IFDEF VCLLIB}
  Result := RGB(FPixels[FRX].rgbtRed, FPixels[FRX].rgbtGreen, FPixels[FRX].rgbtBlue);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := RGB(FPixels^[FRX].rgbtRed, FPixels^[FRX].rgbtGreen, FPixels^[FRX].rgbtBlue);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WEBLIB}
  FOffS := (Round(X * Fsc) + (Round(Y * Fsc) * Round(Width * Fsc))) * 4;
  Result := RGB(FImgDt.data[FOffS + 0], FImgDt.data[FOffS + 1], FImgDt.data[FOffS + 2]);
  {$ENDIF}
end;

end.
