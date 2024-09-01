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

unit VCL.TMSFNCCustomWEBComponent;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.Controls, VCL.TMSFNCTypes, VCL.TMSFNCCustomWEBControl;

type
  TTMSFNCCustomWEBComponent = class(TTMSFNCCustomWEBControl)
  protected
    procedure Loaded; override;
    function GetInstance: NativeUInt; virtual;
    function CanBeVisible: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure SetBounds(X, Y, {%H-}AWidth, {%H-}AHeight: Integer); override;
    {$ENDIF}
    procedure Paint; override;
  published
    property Visible {$IFDEF CMNLIB}default False{$ENDIF};
  end;

  TTMSFNCCustomWEBComponentClass = class of TTMSFNCCustomWEBComponent;

implementation

uses
  VCL.TMSFNCUtils, SysUtils, VCL.TMSFNCGraphics,
  VCL.Graphics, VCL.TMSFNCGraphicsTypes
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,PngImage
  {$ENDIF}
  ;

{ TTMSFNCCustomWEBComponent }

function TTMSFNCCustomWEBComponent.CanBeVisible: Boolean;
begin
  Result := False;
end;

constructor TTMSFNCCustomWEBComponent.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF WEBLIB}
  ControlStyle := ControlStyle - [csAcceptsControls];
  {$ENDIF}
  Width := 26;
  Height := 26;
end;

function TTMSFNCCustomWEBComponent.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCCustomWEBComponent.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Visible := False;
end;

procedure TTMSFNCCustomWEBComponent.Paint;
var
  {$IFDEF VCLLIB}
  png: TPngImage;
  {$ENDIF}
  pic: TTMSFNCBitmap;
  g: TTMSFNCGraphics;
  r: TResourceStream;
  {$IFDEF WEBLIB}
  rc: TRegisteredComponent;
  {$ENDIF}
  c: TClass;
begin
  inherited;
  r := nil;
  pic := TTMSFNCBitmap.Create;
  {$IFDEF VCLLIB}
  png := TPNGImage.Create;
  {$ENDIF}
  g := TTMSFNCGraphics.Create(Canvas);
  try
    g.Stroke.Kind := gskSolid;
    g.Stroke.Color := gcBlack;
    g.DrawRectangle(RectF(0, 0, Width, Height));

    {$IFNDEF WEBLIB}
    r := TTMSFNCUtils.GetResourceStream(UpperCase(ClassName), GetInstance);
    c := ClassParent;
    while Assigned(c) and not Assigned(r) do
    begin
      r := TTMSFNCUtils.GetResourceStream(UpperCase(c.ClassName), GetInstance);
      c := c.ClassParent;
    end;

    if Assigned(r) then
    begin
      {$IFDEF VCLLIB}
      png.LoadFromStream(r);
      pic.Assign(png);
      {$ELSE}
      pic.LoadFromStream(r);
      {$ENDIF}
    end;
    {$ENDIF}

    {$IFDEF WEBLIB}
    rc := GetRegisteredComponent(ClassName);
    if Assigned(rc) then
      pic.LoadFromResource(rc.Icon);
    {$ENDIF}

    g.DrawBitmap(RectF(0, 0, Width, Height), pic);
  finally
    if Assigned(r) then
      r.Free;

    pic.Free;
    {$IFDEF VCLLIB}
    png.Free;
    {$ENDIF}
    g.Free;
  end;
end;

{$IFDEF FMXLIB}
procedure TTMSFNCCustomWEBComponent.SetBounds(X, Y, AWidth, AHeight: Single);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TTMSFNCCustomWEBComponent.SetBounds(X, Y, AWidth, AHeight: Integer);
{$ENDIF}
var 
  sc: Single;
begin
  sc := TTMSFNCUtils.GetDPIScale(Self);
  inherited SetBounds(X, Y, Round(26 * sc), Round(26 * sc));
end;

end.
