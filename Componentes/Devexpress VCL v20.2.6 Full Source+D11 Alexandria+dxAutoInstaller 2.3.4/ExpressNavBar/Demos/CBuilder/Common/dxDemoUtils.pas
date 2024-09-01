unit dxDemoUtils;

interface

uses Classes, Windows;

(*
TdxCustomDrawRegion
Use TdxCustomDrawRegion to set and then restore clip region.
For instance:
var
  CustomDrawRgn: TdxCustomDrawRegion;
begin
  //Set new Clip region, intersect new clip rect with the old one
  CustomDrawRgn := TdxCustomDrawRegion.Create(ARect);
  try
  ...
  finally
    //Destroy object and restore the old clip region
    CustomDrawRgn.Free;
  end;
*)

type
  TdxCustomDrawRegion = class
  private
    fOldClipRgn: HRgn;
    fCanvasHandle: THandle;
  protected
    procedure SaveClipRegion;
    procedure SetClipRegion(AClipRect: TRect);
    procedure RestoreClipRegion;
  public
    constructor Create(ACanvasHandle: THandle; AClipRect: TRect);
    destructor Destroy; override;
  end;

implementation

constructor TdxCustomDrawRegion.Create(ACanvasHandle: THandle; AClipRect: TRect);
begin
  inherited Create;
  fCanvasHandle := ACanvasHandle;
  SaveClipRegion;
  SetClipRegion(AClipRect);
end;

destructor TdxCustomDrawRegion.Destroy;
begin
  RestoreClipRegion;
  inherited Destroy;
end;

procedure TdxCustomDrawRegion.SaveClipRegion;
begin
  fOldClipRgn := CreateRectRgn(0, 0, 0, 0);
  if (GetClipRgn(fCanvasHandle, fOldClipRgn) <> 1) then
  begin
    DeleteObject(fOldClipRgn);
    fOldClipRgn := 0;
  end;
end;

procedure TdxCustomDrawRegion.SetClipRegion(AClipRect: TRect);
begin
  with AClipRect do
    IntersectClipRect(fCanvasHandle, Left, Top, Right, Bottom);
end;

procedure TdxCustomDrawRegion.RestoreClipRegion;
begin
  SelectClipRgn(fCanvasHandle, fOldClipRgn);
  if (fOldClipRgn <> 0) then
    DeleteObject(fOldClipRgn);
end;

end.
