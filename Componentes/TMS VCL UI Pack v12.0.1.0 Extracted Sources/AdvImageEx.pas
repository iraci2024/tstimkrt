{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2021                               }
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

unit AdvImageEx;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvCustomControl, PictureContainer,
  AdvTypes, AdvGraphics, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Types
  {$ENDIF}
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Improved : Changes to setter visibility for TAdvHotSpotImage

type
  {$IFDEF FNCLIB}
  TAdvCustomImage = class(TAdvCustomControl, IAdvPictureContainer)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvCustomImage = class(TAdvCustomControl)
  {$ENDIF}
  private
    FBitmap: TAdvBitmap;
    FPictureContainer: TPictureContainer;
    FStretch: Boolean;
    FBitmaps: TAdvScaledBitmaps;
    FCropping: Boolean;
    FCenter: Boolean;
    FAutoSize: Boolean;
    FAspectRatio: Boolean;
    FOnBitmapChanged: TNotifyEvent;
    FBitmapMargins: TAdvMargins;
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetBitmap(const Value: TAdvBitmap);
    function GetPictureContainer: TPictureContainer;
    procedure SetBitmapMargins(const Value: TAdvMargins);
  protected
    function GetVersion: string; override;
    procedure ApplyAutoSize;
    procedure BitmapMarginsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BitmapChanged(Sender: TObject);
    procedure SetStretch(const Value: Boolean); virtual;
    procedure SetCropping(const Value: Boolean); virtual;
    procedure SetCenter(const Value: Boolean); virtual;
    procedure SetAS(const Value: Boolean); virtual;
    procedure SetAspectRatio(const Value: Boolean); virtual;
    property AspectRatio: Boolean read FAspectRatio write SetAspectRatio default True;
    property Version: String read GetVersion;
    property Bitmaps: TAdvScaledBitmaps read FBitmaps write SetBitmaps;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Cropping: Boolean read FCropping write SetCropping default False;
    property Center: Boolean read FCenter write SetCenter default True;
    property AutoSize: Boolean read FAutoSize write SetAS default False;
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapMargins: TAdvMargins read FBitmapMargins write SetBitmapMargins;
  public
    procedure Clear;
    function GetPaintRectangle: TRectF;
    function GetBitmap: TAdvBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    property OnBitmapChanged: TNotifyEvent read FOnBitmapChanged write FOnBitmapChanged;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvImageEx = class(TAdvCustomImage)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property AspectRatio;
    property Bitmap;
    property BitmapMargins;
    property Version;
    property Bitmaps;
    property PictureContainer;
    property Stretch;
    property Cropping;
    property Center;
    property AutoSize;
    property Fill;
    property Stroke;
  end;

implementation

uses
  Controls, Graphics, AdvUtils;

procedure TAdvCustomImage.ApplyAutoSize;
var
  bmp: TAdvBitmap;
begin
  bmp := GetBitmap;
  if Assigned(bmp) then
  begin
    if FAutoSize and not IsBitmapEmpty(bmp) then
    begin
      Self.Width := Round(bmp.Width + BitmapMargins.Left + BitmapMargins.Right);
      Self.Height := Round(bmp.Height + BitmapMargins.Top + BitmapMargins.Bottom);
    end;
  end;
end;

procedure TAdvCustomImage.BitmapChanged(Sender: TObject);
begin
  if Assigned(OnBitmapChanged) then
    OnBitmapChanged(Self);

  ApplyAutoSize;
  Invalidate;
end;

procedure TAdvCustomImage.BitmapMarginsChanged(Sender: TObject);
begin
  ApplyAutoSize;
  Invalidate;
end;

procedure TAdvCustomImage.Clear;
begin
  Bitmaps.Clear;
end;

constructor TAdvCustomImage.Create(AOwner: TComponent);
begin
  inherited;
  if IsDesignTime then
    DisableBackground;

  FBitmapMargins := TAdvMargins.Create;
  FBitmapMargins.OnChange := BitmapMarginsChanged;
  FCenter := True;
  FStretch := False;
  FAspectRatio := True;
  FCropping := False;
  FBitmaps := TAdvScaledBitmaps.Create(Self);
  FBitmaps.OnChange := BitmapChanged;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  Width := 100;
  Height := 100;
end;

destructor TAdvCustomImage.Destroy;
begin
  FBitmap.Free;
  FBitmaps.Free;
  FBitmapMargins.Free;
  inherited;
end;

function TAdvCustomImage.GetBitmap: TAdvBitmap;
begin
  if not IsBitmapEmpty(FBitmap) then
    Result := FBitmap
  else
    Result := TAdvGraphics.GetScaledBitmap(Bitmaps, 0, FPictureContainer);
end;

function TAdvCustomImage.GetPictureContainer: TPictureContainer;
begin
  Result := FPictureContainer;
end;

function TAdvCustomImage.GetPaintRectangle: TRectF;
var
  bmp: TAdvBitmap;
  rdest: TRectF;
  w, h: Single;
  x, y: Single;
begin
  Result := RectF(0, 0, 0, 0);
  bmp := GetBitmap;
  if Assigned(bmp) then
  begin
    x := BitmapMargins.Left;
    y := BitmapMargins.Top;
    w := 0;
    h := 0;
    TAdvGraphics.GetAspectSize(w, h, bmp.Width, bmp.Height, Width - BitmapMargins.Left - BitmapMargins.Right,
      Height - BitmapMargins.Top - BitmapMargins.Bottom, AspectRatio, Stretch, Cropping);

    if Center or Cropping then
    begin
      x := (width - w) / 2;
      y := (height - h) / 2;
    end;

    rdest := RectF(x, y, x + w, y + h);
    Result := rdest;
  end;
end;

function TAdvCustomImage.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;
end;

procedure TAdvCustomImage.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  bmp: TAdvBitmap;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    AGraphics.Stroke.Kind := gskDot;
    AGraphics.DrawRectangle(ARect);
  end;

  bmp := GetBitmap;
  if Assigned(bmp) then
  begin
    ARect := RectF(ARect.Left + BitmapMargins.Left, ARect.Top + BitmapMargins.Top,
      ARect.Right - BitmapMargins.Right, ARect.Bottom - BitmapMargins.Bottom);

    AGraphics.DrawBitmap(ARect, bmp, AspectRatio, Stretch, Center, Cropping);
  end;
end;

procedure TAdvCustomImage.SetAspectRatio(const Value: Boolean);
begin
  FAspectRatio := Value;
  Invalidate;
end;

procedure TAdvCustomImage.SetAS(const Value: Boolean);
begin
  FAutoSize := Value;
  ApplyAutoSize;
  Invalidate;
end;

procedure TAdvCustomImage.SetBitmaps(const Value: TAdvScaledBitmaps);
begin
  FBitmaps.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomImage.SetBitmap(const Value: TAdvBitmap);
begin
  FBitmap.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomImage.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  ApplyAutoSize;
  Invalidate;
end;

procedure TAdvCustomImage.SetBitmapMargins(const Value: TAdvMargins);
begin
  FBitmapMargins.Assign(Value);
end;

procedure TAdvCustomImage.SetCenter(const Value: Boolean);
begin
  FCenter := Value;
  Invalidate;
end;

procedure TAdvCustomImage.SetCropping(const Value: Boolean);
begin
  FCropping := Value;
  Invalidate;
end;

procedure TAdvCustomImage.SetStretch(const Value: Boolean);
begin
  FStretch := Value;
  Invalidate;
end;

{ TAdvImageEx }

procedure TAdvImageEx.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvImageEx);
end;

end.
