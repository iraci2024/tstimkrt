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

unit AdvCustomComponent;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

{$IFDEF FNCLIB}
{$DEFINE USETRIAL}
{$ELSE}
{$IFDEF FMXLIB}
{$DEFINE USETRIAL}
{$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  {%H-}Windows,
  {$ENDIF}
  Classes, Controls, AdvTypes
  ,AdvPersistence
  {$IFNDEF FNCLIB}
  {$IFDEF VCLLIB}
  {$IFDEF FREEWARE}
  ,TMSTrial
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ,TypInfo;

type
  TAdvCustomComponentCanSavePropertyEvent = procedure(Sender: TObject; AObject: TObject; APropertyName: string; APropertyType: TTypeKind; var ACanSave: Boolean) of object;
  TAdvCustomComponentCanLoadPropertyEvent = procedure(Sender: TObject; AObject: TObject; APropertyName: string; APropertyType: TTypeKind; var ACanLoad: Boolean) of object;

  {$IFDEF CMNWEBLIB}
  TAdvCustomComponent = class(TCustomControl, IAdvProductInfo, IAdvPersistence)
  private
    FStored: Boolean;
  {$ELSE}
  TAdvCustomComponent = class(TControl, IAdvProductInfo, IAdvPersistence)
  {$ENDIF}
  private
    class var FBlockPersistenceInterface: Boolean;
  private
    FDesigntimeFormPixelsPerInch: Integer;
    FAppearancePersisting: Boolean;
    FAdaptToStyle: Boolean;
    FOnCanSaveProperty: TAdvCustomComponentCanSavePropertyEvent;
    FOnCanLoadProperty: TAdvCustomComponentCanLoadPropertyEvent;
  protected
    {$IFDEF WEBLIB}
    class function GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
    {$ENDIF}
    procedure DoCanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind; var ACanSave: Boolean); virtual;
    procedure DoCanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind; var ACanLoad: Boolean); virtual;
    function IsAppearanceProperty({%H-}AObject: TObject; {%H-}APropertyName: string; {%H-}APropertyType: TTypeKind): Boolean; virtual;
    function CanSaveProperty({%H-}AObject: TObject; {%H-}APropertyName: string; {%H-}APropertyType: TTypeKind): Boolean; virtual;
    function CanLoadProperty({%H-}AObject: TObject; {%H-}APropertyName: string; {%H-}APropertyType: TTypeKind): Boolean; virtual;
    function GetVersion: string; virtual;
    function GetDocURL: string; virtual;
    function GetTipsURL: string; virtual;
    function GetInstance: NativeUInt; virtual;
    {$IFDEF VCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION > 30}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    procedure ChangeDPIScale({%H-}M, {%H-}D: Integer); virtual;
    procedure SetAdaptToStyle(const Value: Boolean); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure RegisterRuntimeClasses; virtual;
    property AdaptToStyle: Boolean read FAdaptToStyle write SetAdaptToStyle default False;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure SetBounds(X, Y, {%H-}AWidth, {%H-}AHeight: Integer); override;
    property Stored: Boolean read FStored write FStored;
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION = 28}
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFDEF WEBLIB}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    {$ENDIF}
    {$ENDIF}
    function IsDesignTime: Boolean; virtual;
    function IsLoading: Boolean; virtual;
    function IsReading: Boolean; virtual;
    function IsDesigning: Boolean; virtual;
    function IsDestroying: Boolean; virtual;
    procedure SaveSettingsToFile(AFileName: string; AAppearanceOnly: Boolean = False); virtual;
    procedure LoadSettingsFromFile(AFileName: string); virtual;
    procedure SaveSettingsToStream(AStream: TStreamEx; AAppearanceOnly: Boolean = False); virtual;
    procedure LoadSettingsFromStream(AStream: TStreamEx); virtual;
    class property BlockPersistenceInterface: Boolean read FBlockPersistenceInterface write FBlockPersistenceInterface;
    property DesigntimeFormPixelsPerInch: Integer read FDesigntimeFormPixelsPerInch;
  published
    {$IFDEF FMXLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION > 27}
    property Size;
    {$IFEND}
    {$HINTS ON}
    property Position;
    {$ENDIF}
    property Visible {$IFDEF CMNLIB}default False{$ENDIF};
    property Width;
    property Height;
    property OnCanSaveProperty: TAdvCustomComponentCanSavePropertyEvent read FOnCanSaveProperty write FOnCanSaveProperty;
    property OnCanLoadProperty: TAdvCustomComponentCanLoadPropertyEvent read FOnCanLoadProperty write FOnCanLoadProperty;
  end;

  TAdvCustomComponentClass = class of TAdvCustomComponent;

implementation

uses
  AdvUtils, Forms, SysUtils, AdvGraphics,
  Graphics, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,PngImage
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  {$IFDEF USETRIAL}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  {$IFNDEF FMXMOBILE}
  {$IFDEF FREEWARE}
  {$IFDEF MACOS}
  ,MacApi.AppKit, MacApi.Foundation, MacApi.Helpers
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,System.Win.Registry, WinApi.ShellApi
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,UITypes, FMX.Types, StdCtrls, FMX.Objects, UIConsts
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.StdCtrls, VCL.ExtCtrls
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

{ TAdvCustomComponent }

{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
procedure TAdvCustomComponent.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TAdvCustomComponent.ChangeScale(M, D: Integer);
{$IFEND}
begin
  inherited;
  ChangeDPIScale(M, D);
end;
{$HINTS ON}
{$ENDIF}

procedure TAdvCustomComponent.ChangeDPIScale({%H-}M, {%H-}D: Integer);
begin
end;

{$IFDEF WEBLIB}
class function TAdvCustomComponent.GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
begin
  Result := '';
end;
{$ENDIF}

function TAdvCustomComponent.IsAppearanceProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  Result := False;
end;

function TAdvCustomComponent.CanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  if (AObject = Self) and not BlockPersistenceInterface then
    Result := (TAdvUtils.IndexOfTextInArray(APropertyName, ExcludePropertyList) = -1)
  else
    Result := True;

  if FAppearancePersisting then
    Result := Result and IsAppearanceProperty(AObject, APropertyName, APropertyType);

  DoCanSaveProperty(AObject, APropertyName, APropertyType, Result);
end;

function TAdvCustomComponent.CanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  if (AObject = Self) and not BlockPersistenceInterface then
    Result := (TAdvUtils.IndexOfTextInArray(APropertyName, ExcludePropertyList) = -1)
  else
    Result := True;

  DoCanLoadProperty(AObject, APropertyName, APropertyType, Result);
end;

constructor TAdvCustomComponent.Create(AOwner: TComponent);
var
  ppi: Integer;
{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 34}
  f: TCustomForm;
{$IFEND}
{$HINTS ON}
{$ENDIF}
begin
  inherited;
  ppi := 96;
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 34}
  f := TAdvUtils.GetOwnerForm(Self);
  if Assigned(f) then
    ppi := f.PixelsPerInch;
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}

  FDesigntimeFormPixelsPerInch := ppi;

  Width := 26;
  Height := 26;

  if not IsDesigning then
    RegisterRuntimeClasses;
end;

procedure TAdvCustomComponent.DoCanLoadProperty(AObject: TObject;
  APropertyName: string; APropertyType: TTypeKind; var ACanLoad: Boolean);
begin
  if Assigned(OnCanLoadProperty) then
    OnCanLoadProperty(Self, AObject, APropertyName, APropertyType, ACanLoad);
end;

procedure TAdvCustomComponent.DoCanSaveProperty(AObject: TObject;
  APropertyName: string; APropertyType: TTypeKind; var ACanSave: Boolean);
begin
  if Assigned(OnCanSaveProperty) then
    OnCanSaveProperty(Self, AObject, APropertyName, APropertyType, ACanSave);
end;

function TAdvCustomComponent.GetDocURL: string;
begin
  Result := TAdvBaseDocURL;
end;

function TAdvCustomComponent.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TAdvCustomComponent.GetTipsURL: string;
begin
  Result := TAdvBaseTipsURL;
end;

function TAdvCustomComponent.GetVersion: string;
begin
  Result := '';
end;

function TAdvCustomComponent.IsDesignTime: Boolean;
begin
  Result := False;
  if Assigned(Owner) then
    Result := IsDesigning and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

function TAdvCustomComponent.IsReading: Boolean;
begin
  Result := (csReading in Owner.ComponentState);
end;

function TAdvCustomComponent.IsDesigning: Boolean;
begin
  Result := ((csDesigning in ComponentState) {$IFDEF WEBLIB}or Assigned(VSIDE){$ENDIF});
end;

function TAdvCustomComponent.IsDestroying: Boolean;
begin
  Result := (csDestroying in ComponentState);
end;

function TAdvCustomComponent.IsLoading: Boolean;
begin
  Result := (csLoading in Owner.ComponentState);
end;

procedure TAdvCustomComponent.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Visible := False;
end;

procedure TAdvCustomComponent.SaveSettingsToFile(AFileName: string; AAppearanceOnly: Boolean = False);
begin
  FAppearancePersisting := AAppearanceOnly;
  TAdvPersistence.SaveSettingsToFile(Self, AFileName);
  FAppearancePersisting := False;
end;

procedure TAdvCustomComponent.LoadSettingsFromFile(AFileName: string);
begin
  TAdvPersistence.LoadSettingsFromFile(Self, AFileName);
end;

procedure TAdvCustomComponent.SaveSettingsToStream(AStream: TStreamEx; AAppearanceOnly: Boolean = False);
begin
  FAppearancePersisting := AAppearanceOnly;
  TAdvPersistence.SaveSettingsToStream(Self, AStream);
  FAppearancePersisting := False;
end;

procedure TAdvCustomComponent.LoadSettingsFromStream(AStream: TStreamEx);
begin
  TAdvPersistence.LoadSettingsFromStream(Self, AStream);
end;

{$IFDEF FMXLIB}
{$HINTS OFF}
{$IF COMPILERVERSION = 28}
procedure TAdvCustomComponent.BeginUpdate;
begin
end;

procedure TAdvCustomComponent.EndUpdate;
begin
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}

{$IFDEF CMNWEBLIB}
procedure TAdvCustomComponent.BeginUpdate;
begin
  {$IFDEF WEBLIB}
  inherited;
  {$ENDIF}
end;

procedure TAdvCustomComponent.EndUpdate;
begin
  {$IFDEF WEBLIB}
  inherited;
  {$ENDIF}
end;
{$ENDIF}

procedure TAdvCustomComponent.Paint;
var
  {$IFDEF VCLLIB}
  png: TPngImage;
  {$ENDIF}
  pic: TAdvBitmap;
  g: TAdvGraphics;
  r: TResourceStream;
  {$IFDEF WEBLIB}
  rc: TRegisteredComponent;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  c: TClass;
  {$ENDIF}
begin
  inherited;
  r := nil;
  pic := TAdvBitmap.Create;
  {$IFDEF VCLLIB}
  png := TPNGImage.Create;
  {$ENDIF}
  g := TAdvGraphics.Create(Canvas);
  try
    g.Stroke.Kind := gskSolid;
    g.Stroke.Color := gcBlack;
    g.DrawRectangle(RectF(0, 0, Width, Height));

    {$IFNDEF WEBLIB}
    r := TAdvUtils.GetResourceStream(UpperCase(ClassName), GetInstance);
    c := ClassParent;
    while Assigned(c) and not Assigned(r) do
    begin
      r := TAdvUtils.GetResourceStream(UpperCase(c.ClassName), GetInstance);
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

    g.DrawBitmap(RectF(2, 2, Width - 2, Height - 2), pic, True, True);
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

procedure TAdvCustomComponent.RegisterRuntimeClasses;
begin
end;

procedure TAdvCustomComponent.SetAdaptToStyle(const Value: Boolean);
begin
  FAdaptToStyle := Value;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomComponent.SetBounds(X, Y, AWidth, AHeight: Single);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomComponent.SetBounds(X, Y, AWidth, AHeight: Integer);
{$ENDIF}
var 
  sc: Single;
begin
  sc := TAdvUtils.GetDPIScale(Self);
  inherited SetBounds(X, Y, Round(26 * sc), Round(26 * sc));
end;

{$IFDEF USETRIAL}
{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
{$IFNDEF FMXMOBILE}
{$IFDEF FREEWARE}
const TMSPRODUCTOFFSET = 400;
{$I TMSProductTrial.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
