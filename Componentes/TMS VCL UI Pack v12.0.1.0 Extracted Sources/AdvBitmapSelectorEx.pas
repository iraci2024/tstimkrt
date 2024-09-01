{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016                                      }
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

unit AdvBitmapSelectorEx;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvCustomSelectorEx, AdvTypes,
  PictureContainer, AdvGraphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TAdvCustomBitmapSelector = class;

  TAdvBitmapSelectorExBitmapAlign = (baLeft, baCenter, baRight);

  TAdvBitmapSelectorExItem = class(TAdvCustomSelectorExItem)
  private
    FOwner: TAdvCustomBitmapSelector;
    FBitmap: TAdvBitmap;
    FBitmapAlign: TAdvBitmapSelectorExBitmapAlign;
    FBitmapName: String;
    FBitmapSize: Single;
    procedure SetBitmap(const Value: TAdvBitmap);
    procedure SetBitmapAlign(const Value: TAdvBitmapSelectorExBitmapAlign);
    procedure SetBitmapName(const Value: String);
    procedure SetBitmapSize(const Value: Single);
    function GetPictureContainer: TPictureContainer;
    function IsBitmapSizeStored: Boolean;
  protected
    function GetDisplayBitmap: TAdvBitmap;
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property PictureContainer: TPictureContainer read GetPictureContainer;
  published
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapName: String read FBitmapName write SetBitmapName;
    property BitmapAlign: TAdvBitmapSelectorExBitmapAlign read FBitmapAlign write SetBitmapAlign default TAdvBitmapSelectorExBitmapAlign.baCenter;
    property BitmapSize: Single read FBitmapSize write SetBitmapSize stored IsBitmapSizeStored nodefault;
  end;

  TAdvBitmapSelectorExItems = class(TAdvCustomSelectorExItems)
  private
    FOwner: TAdvCustomBitmapSelector;
    function GetItem(Index: integer): TAdvBitmapSelectorExItem;
    procedure SetItem(Index: integer; const Value: TAdvBitmapSelectorExItem);
  protected
    function CreateItemClass: TCollectionItemClass; override;
  public
    constructor Create(AOwner: TAdvCustomSelectorEx); override;
    property Items[Index: integer]: TAdvBitmapSelectorExItem read GetItem write SetItem; default;
    function Add: TAdvBitmapSelectorExItem;
    function Insert(Index: integer): TAdvBitmapSelectorExItem;
  end;

  TAdvCustomSelectorExBitmapSelected = procedure(Sender: TObject; ABitmap: TAdvBitmap) of object;
  TAdvCustomSelectorExBitmapDeselected = procedure(Sender: TObject; ABitmap: TAdvBitmap) of object;

  {$IFDEF FNCLIB}
  TAdvCustomBitmapSelector = class(TAdvDefaultSelector, IAdvPictureContainer)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvCustomBitmapSelector = class(TAdvDefaultSelector)
  {$ENDIF}
  private
    FOnBitmapSelected: TAdvCustomSelectorExBitmapSelected;
    FOnBitmapDeselected: TAdvCustomSelectorExBitmapDeselected;
    FPictureContainer: TPictureContainer;
    function GetSelectedBitmap: TAdvBitmap;
    procedure SetPictureContainer(const Value: TPictureContainer);
    function GetItems: TAdvBitmapSelectorExItems;
    procedure SetItems(const Value: TAdvBitmapSelectorExItems);
    function GetPictureContainer: TPictureContainer;
  protected
    function GetVersion: String; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoItemSelected(AItemIndex: Integer); override;
    procedure DoItemDeselected(AItemIndex: Integer); override;
    function CreateItemsCollection: TAdvCustomSelectorExItems; override;
    procedure DrawItemContent(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); override;
    procedure DrawItemText(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); override;
    property OnBitmapSelected: TAdvCustomSelectorExBitmapSelected read FOnBitmapSelected write FOnBitmapSelected;
    property OnBitmapDeselected: TAdvCustomSelectorExBitmapDeselected read FOnBitmapDeselected write FOnBitmapDeselected;
    property SelectedBitmap: TAdvBitmap read GetSelectedBitmap;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property Items: TAdvBitmapSelectorExItems read GetItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromPictureContainer; virtual;
    function FindItemByBitmap(ABitmap: TAdvBitmap): Integer;
    function FindBitmapByItem(AItem: Integer): TAdvBitmap;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvBitmapSelectorEx = class(TAdvCustomBitmapSelector)
  protected
    procedure RegisterRuntimeClasses; override;
  public
    property SelectedBitmap;
  published
    property Appearance;
    property Rows;
    property Columns;
    property Items;
    property OnBitmapSelected;
    property OnBitmapDeselected;
    property PictureContainer;
    property OnItemSelected;
    property SelectedItemIndex;
    property OnItemBeforeDrawBackground;
    property OnItemAfterDrawBackground;
    property OnItemBeforeDrawContent;
    property OnItemAfterDrawContent;
    property OnBeforeDraw;
    property OnAfterDraw;
    property OnItemBeforeDrawText;
    property OnItemAfterDrawText;
  end;

procedure GetAspectSize(var w, h: Single; ow, oh, nw, nh, crw, crh: Single; asp, st, cr: Boolean);

implementation

uses
  AdvUtils
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  ;

procedure GetAspectSize(var w, h: Single; ow, oh, nw, nh, crw, crh: Single; asp, st, cr: Boolean);
var
  arc, ar: Single;
begin
  if asp then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) and (not st) then
      begin
        w := ow;
        h := oh;
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := nh;
          w := nh * ow / oh;
        end
        else
        begin
          w := nw;
          h := nw * oh / ow;
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    if st then
    begin
      w := nw;
      h := nh;
    end
    else
    begin
      w := ow;
      h := oh;

      if cr then
      begin
        if (w >= h) and (w > 0) then
        begin
          h := crw / w * h;
          w := crw;
        end
        else
        if (h >= w) and (h > 0) then
        begin
          w := crh / h * w;
          h := crh;
        end;

        if h = 0 then
          ar := 1
        else
          ar := w / h;

        if crh = 0 then
          arc := 1
        else
          arc := crw / crh;

        if (ar < 1) or (arc > ar) then
        begin
          h := crw / ar;
          w := crw;
        end
        else
        begin
          w := ar * crh;
          h := crh;
        end;
      end;
    end;
  end;
end;

{ TAdvCustomBitmapSelector }

constructor TAdvCustomBitmapSelector.Create(AOwner: TComponent);
begin
  inherited;
  Width := 135;
  Height := 135;
end;

function TAdvCustomBitmapSelector.CreateItemsCollection: TAdvCustomSelectorExItems;
begin
  Result := TAdvBitmapSelectorExItems.Create(Self);
end;

procedure TAdvCustomBitmapSelector.DoItemDeselected(AItemIndex: Integer);
begin
  inherited;
  if Assigned(OnBitmapDeselected) then
    OnBitmapDeselected(Self, FindBitmapByItem(AItemIndex));
end;

procedure TAdvCustomBitmapSelector.DoItemSelected(AItemIndex: Integer);
begin
  inherited;
  if Assigned(OnBitmapSelected) then
    OnBitmapSelected(Self, FindBitmapByItem(AItemIndex));
end;

procedure TAdvCustomBitmapSelector.DrawItemContent(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  r: TRectF;
  it: TAdvCustomSelectorExItem;
  itbmp: TAdvBitmapSelectorExItem;
  a: Boolean;
  sz: Single;
  bmp: TAdvBitmap;
  bmpr: TRectF;
  w, h: Single;
  bmpdr: TRectF;
begin
  it := ADisplayItem.Item;
  if Assigned(it) and (it is TAdvBitmapSelectorExItem) then
  begin
    r := ADisplayItem.Rect;
    a := True;
    DoItemBeforeDrawContent(AGraphics, r, it.Index, a);
    if a then
    begin
      itbmp := (it as TAdvBitmapSelectorExItem);
      bmp := itbmp.GetDisplayBitmap;
      if Assigned(bmp) then
      begin
        InflateRectEx(r, -2, -2);
        sz := r.Bottom - r.Top;
        if itbmp.BitmapSize > 0 then
          sz := itbmp.BitmapSize;

        case itbmp.BitmapAlign of
          baLeft: bmpr := RectF(r.Left, r.Top, r.Left + sz, r.Bottom);
          baCenter: bmpr := RectF(r.Left + ((r.Right - r.Left) - sz) / 2, r.Top, r.Left + ((r.Right - r.Left) - sz) / 2 + sz, r.Bottom);
          baRight: bmpr := RectF(r.Right - sz, r.Top, r.Right, r.Bottom);
        end;

        w := 0;
        h := 0;
        GetAspectSize(w, h, bmp.Width, bmp.Height, (bmpr.Right - bmpr.Left), (bmpr.Bottom - bmpr.Top), (bmpr.Right - bmpr.Left), (bmpr.Bottom - bmpr.Top), True, False, False);
        bmpdr := RectF(bmpr.Left + ((bmpr.Right - bmpr.Left) - w) / 2, bmpr.Top + ((bmpr.Bottom - bmpr.Top) - h) / 2, bmpr.Left + ((bmpr.Right - bmpr.Left) - w) / 2 + w, bmpr.Top + ((bmpr.Bottom - bmpr.Top) - h) / 2 + h);
        bmpdr := RectF(Round(bmpdr.Left), Round(bmpdr.Top), Round(bmpdr.Right), Round(bmpdr.Bottom));
//      if it.Enabled then
          AGraphics.DrawBitmap(bmpdr, bmp);
//        else
//          AGraphics.DrawBitmap(bmp, RectF(0, 0, bmp.Width, bmp.Height), bmpdr, 0.25);
      end;

      DoItemAfterDrawContent(AGraphics, r, it.Index);
    end;
  end;
end;

procedure TAdvCustomBitmapSelector.DrawItemText(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  it: TAdvBitmapSelectorExItem;
  bmp: TAdvBitmap;
  dps: TAdvCustomSelectorExDisplayItem;
  sz: Single;
begin
  if Assigned(ADisplayItem.Item) and (ADisplayItem.Item is TAdvBitmapSelectorExItem) then
  begin
    it := ADisplayItem.Item as TAdvBitmapSelectorExItem;
    bmp := it.GetDisplayBitmap;
    if Assigned(bmp) then
    begin
      dps := ADisplayItem;
      sz := dps.Rect.Bottom - dps.Rect.Top;
      if it.BitmapSize > 0 then
        sz := it.BitmapSize;

      case it.BitmapAlign of
        baLeft: dps.Rect.Left := dps.Rect.Left + sz + 4;
        baRight: dps.Rect.Right := dps.Rect.Right - sz - 4;
      end;
      inherited DrawItemText(AGraphics, dps);
    end
    else
      inherited;
  end
  else
    inherited;
end;

function TAdvCustomBitmapSelector.FindBitmapByItem(
  AItem: Integer): TAdvBitmap;
var
  I: Integer;
  it: TAdvBitmapSelectorExItem;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I] as TAdvBitmapSelectorExItem;
    if it.Index = AItem then
    begin
      Result := it.GetDisplayBitmap;
      Break;
    end;
  end;
end;

function TAdvCustomBitmapSelector.FindItemByBitmap(
  ABitmap: TAdvBitmap): Integer;
var
  I: Integer;
  it: TAdvBitmapSelectorExItem;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I] as TAdvBitmapSelectorExItem;
    if it.GetDisplayBitmap = ABitmap then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

procedure TAdvCustomBitmapSelector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;
end;


procedure TAdvCustomBitmapSelector.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  InvalidateItems;
end;

function TAdvCustomBitmapSelector.GetPictureContainer: TPictureContainer;
begin
  Result := FPictureContainer;
end;

function TAdvCustomBitmapSelector.GetItems: TAdvBitmapSelectorExItems;
begin
  Result := TAdvBitmapSelectorExItems(inherited Items);
end;

procedure TAdvCustomBitmapSelector.SetItems(
  const Value: TAdvBitmapSelectorExItems);
begin
  Items.Assign(Value);
end;

function TAdvCustomBitmapSelector.GetSelectedBitmap: TAdvBitmap;
begin
  Result := FindBitmapByItem(SelectedItemIndex);
end;

function TAdvCustomBitmapSelector.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomBitmapSelector.LoadFromPictureContainer;
var
  I: Integer;
  it: TAdvBitmapSelectorExItem;
begin
  if not Assigned(PictureContainer) then
    Exit;

  BeginUpdate;
  Items.Clear;
  for I := 0 to PictureContainer.Items.Count - 1 do
  begin
    it := Items.Add;
    it.BitmapName := PictureContainer.Items[I].Name;
  end;
  EndUpdate;
end;

{ TAdvBitmapSelectorExItems }

function TAdvBitmapSelectorExItems.Add: TAdvBitmapSelectorExItem;
begin
  Result := TAdvBitmapSelectorExItem(inherited Add);
end;

constructor TAdvBitmapSelectorExItems.Create(AOwner: TAdvCustomSelectorEx);
begin
  inherited;
  FOwner := AOwner as TAdvCustomBitmapSelector;
end;

function TAdvBitmapSelectorExItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvBitmapSelectorExItem;
end;

function TAdvBitmapSelectorExItems.GetItem(
  Index: integer): TAdvBitmapSelectorExItem;
begin
  Result := TAdvBitmapSelectorExItem(inherited Items[Index]);
end;

function TAdvBitmapSelectorExItems.Insert(
  Index: integer): TAdvBitmapSelectorExItem;
begin
  Result := TAdvBitmapSelectorExItem(inherited Insert(Index));
end;

procedure TAdvBitmapSelectorExItems.SetItem(Index: integer;
  const Value: TAdvBitmapSelectorExItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvBitmapSelectorExItem }

procedure TAdvBitmapSelectorExItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvBitmapSelectorExItem then
  begin
    FBitmap.Assign((Source as TAdvBitmapSelectorExItem).Bitmap);
    FBitmapName := (Source as TAdvBitmapSelectorExItem).BitmapName;
    FBitmapSize := (Source as TAdvBitmapSelectorExItem).BitmapSize;
    FBitmapAlign := (Source as TAdvBitmapSelectorExItem).BitmapAlign;
  end;
end;

procedure TAdvBitmapSelectorExItem.BitmapChanged(Sender: TObject);
begin
  FOwner.InvalidateItems;
end;

constructor TAdvBitmapSelectorExItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FOwner := (Collection as TAdvBitmapSelectorExItems).FOwner;
  FBitmapAlign := baCenter;
  FBitmapSize := 0;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
end;

destructor TAdvBitmapSelectorExItem.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TAdvBitmapSelectorExItem.GetPictureContainer: TPictureContainer;
begin
  Result := FOwner.PictureContainer;
end;

function TAdvBitmapSelectorExItem.GetDisplayBitmap: TAdvBitmap;
begin
  Result := nil;
  if Assigned(Bitmap) and not IsBitmapEmpty(Bitmap) then
    Result := Bitmap
  else if Assigned(FOwner) and Assigned(FOwner.PictureContainer) and (BitmapName <> '') then
    Result := TAdvBitmap(FOwner.PictureContainer.FindBitmap(BitmapName));
end;

function TAdvBitmapSelectorExItem.IsBitmapSizeStored: Boolean;
begin
  Result := BitmapSize <> 0;
end;

procedure TAdvBitmapSelectorExItem.SetBitmap(const Value: TAdvBitmap);
begin
  if FBitmap <> Value then
  begin
    FBitmap.Assign(Value);
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvBitmapSelectorExItem.SetBitmapAlign(
  const Value: TAdvBitmapSelectorExBitmapAlign);
begin
  if FBitmapAlign <> Value then
  begin
    FBitmapAlign := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvBitmapSelectorExItem.SetBitmapName(const Value: String);
begin
  if FBitmapName <> Value then
  begin
    FBitmapName := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvBitmapSelectorExItem.SetBitmapSize(const Value: Single);
begin
  if FBitmapSize <> Value then
  begin
    FBitmapSize := Value;
    FOwner.InvalidateItems;
  end;
end;

{ TAdvBitmapSelectorEx }

procedure TAdvBitmapSelectorEx.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClasses([TAdvBitmapSelectorEx, TAdvBitmapSelectorExItem]);
end;

end.
