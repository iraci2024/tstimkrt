{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2022                               }
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

unit AdvColorSelectorEx;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvGraphics, AdvCustomControl, AdvCustomSelectorEx, Controls,
  AdvTypes, ExtCtrls, AdvGraphicsTypes, StdCtrls
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  //v1.0.0.0: First release
  //v1.0.0.1 : Improved : Updated initial look

const
  AdvColorSelectorColorSetCount = 15;
  AdvColorSelectorColorSet: array[0..AdvColorSelectorColorSetCount] of TAdvGraphicsColor = (gcBlack, gcMaroon, gcGreen, gcOlive,
    gcNavy, gcPurple, gcTeal, gcSilver, gcGray, gcRed,
    gcLime, gcYellow, gcBlue, gcFuchsia, gcAqua, gcWhite);

  {$IFDEF FMXLIB}
  AdvColorSelectorExtendedColorSetCount = 39;
  AdvColorSelectorExtendedColorSet: array[0..AdvColorSelectorExtendedColorSetCount] of TAdvGraphicsColor = (gcBlack, $FF003399, $FF003333, $FF003300, $FF663300, gcNavy,
    $FF353333, $FF333333, $FF800000, $FFFF6600, $FF808000, $FF008000, $FF008080, $FF0000FF,
    $FF666699, $FF808080, $FFFF0000, $FFFF9900, $FF99CC00, $FF339966, $FF33CCCC,
    $FF3366FF, $FF800080, $FF999999, $FFFF00FF, $FFFFCC00, $FFFFFF00, $FF00FF00,
    $FF00FFFF, $FF00CCFF, $FF993366, $FFC0C0C0, $FFFF99CC, $FFFFCC99, $FFFFFF99,
    $FFCCFFCC, $FFCCFFFF, $FF99CCFF, $FFCC99FF, $FFFFFFFF);
  {$ENDIF}

  {$IFDEF CMNWEBLIB}
  AdvColorSelectorExtendedColorSetCount = 39;
  AdvColorSelectorExtendedColorSet: array[0..AdvColorSelectorExtendedColorSetCount] of TAdvGraphicsColor = (gcBlack, $993300, $333300, $003300, $003366, gcNavy,
    $333335, $333333, $000080, $0066FF, $008080, $008000, $808000, $FF0000,
    $996666, $808080, $0000FF, $0099FF, $00CC99, $669933, $CCCC33,
    $FF6633, $800080, $999999, $FF00FF, $00CCFF, $00FFFF, $00FF00,
    $FFFF00, $FFCC00, $663399, $C0C0C0, $CC99FF, $99CCFF, $99FFFF,
    $CCFFCC, $FFFFCC, $FFCC99, $FF99CC, $FFFFFF);
  {$ENDIF}


type
  TAdvCustomColorSelector = class;

  TAdvColorSelectorExItem = class(TAdvCustomSelectorExItem)
  private
    FOwner: TAdvCustomColorSelector;
    FColor: TAdvGraphicsColor;
    procedure SetColor(const Value: TAdvGraphicsColor);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TAdvGraphicsColor read FColor write SetColor default gcNull;
  end;

  TAdvColorSelectorExItems = class(TAdvCustomSelectorExItems)
  private
    FOwner: TAdvCustomColorSelector;
    function GetItem(Index: integer): TAdvColorSelectorExItem;
    procedure SetItem(Index: integer; const Value: TAdvColorSelectorExItem);
  protected
    function CreateItemClass: TCollectionItemClass; override;
  public
    constructor Create(AOwner: TAdvCustomSelectorEx); override;
    property Items[Index: integer]:TAdvColorSelectorExItem read GetItem write SetItem; default;
    function Add: TAdvColorSelectorExItem;
    function Insert(Index: integer): TAdvColorSelectorExItem;
  end;

  TAdvCustomSelectorExColorSelected = procedure(Sender: TObject; AColor: TAdvGraphicsColor) of object;
  TAdvCustomSelectorExColorDeselected = procedure(Sender: TObject; AColor: TAdvGraphicsColor) of object;
  TAdvCustomSelectorExColorClick = procedure(Sender: TObject; AColor: TAdvGraphicsColor) of object;

  TAdvCustomColorSelectorMode = (csmSimple, csmExtended, csmExtendedMore);

  TAdvCustomColorSelector = class(TAdvDefaultSelector)
  private
    w, h: Single;
    FColorWheel: TAdvCustomControl;
    FPanel: TAdvCustomControl;
    FBackButton: TButton;
    FOnColorSelected: TAdvCustomSelectorExColorSelected;
    FOnColorDeselected: TAdvCustomSelectorExColorDeselected;
    FMode: TAdvCustomColorSelectorMode;
    procedure SetSelectedColor(const Value: TAdvGraphicsColor);
    function GetSelectedColor: TAdvGraphicsColor;
    function GetItems: TAdvColorSelectorExItems;
    procedure SetItems(const Value: TAdvColorSelectorExItems);
    procedure SetMode(const Value: TAdvCustomColorSelectorMode);
  protected
    function GetVersion: String; override;
    procedure BackButtonClicked(Sender: TObject);
    procedure ColorWheelChange(Sender: TObject; AColor: TAdvGraphicsColor);
    procedure DoItemClick(AItemIndex: Integer); override;
    procedure WrapperSizeChanged(AWidth, AHeight: Single); virtual;
    procedure DoItemSelected(AItemIndex: Integer); override;
    procedure DoItemDeselected(AItemIndex: Integer); override;
    function CreateItemsCollection: TAdvCustomSelectorExItems; override;
    procedure DoItemBeforeDrawContent(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean); override;
    procedure DrawItemContent(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); override;
    property OnColorSelected: TAdvCustomSelectorExColorSelected read FOnColorSelected write FOnColorSelected;
    property OnColorDeselected: TAdvCustomSelectorExColorDeselected read FOnColorDeselected write FOnColorDeselected;
    property SelectedColor: TAdvGraphicsColor read GetSelectedColor write SetSelectedColor default gcNull;
    property Items: TAdvColorSelectorExItems read GetItems write SetItems;
    property Mode: TAdvCustomColorSelectorMode read FMode write SetMode default csmSimple;
    procedure AddColors;
    procedure ResetToDefaultStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    function FindItemByColor(AColor: TAdvGraphicsColor): Integer;
    function FindColorByItem(AItem: Integer): TAdvGraphicsColor;
    function ColorWheelActive: Boolean;
    procedure InitializeDefault; override;
    procedure InitSample; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvColorSelectorEx = class(TAdvCustomColorSelector)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property Appearance;
    property Rows;
    property Columns;
    property Items;
    property Mode;
    property OnColorSelected;
    property OnColorDeselected;
    property SelectedColor;
    property OnItemSelected;
    property OnItemDeselected;
    property OnItemClick;
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

implementation

uses
  SysUtils, AdvUtils, AdvColorWheel;

function AsColorWheel(AColorWheel: TAdvCustomControl): TAdvColorWheel;
begin
  Result := (AColorWheel as TAdvColorWheel);
end;

{ TAdvCustomColorSelector }

procedure TAdvCustomColorSelector.AddColors;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  BeginUpdate;
  Items.Clear;
  case Mode of
    csmSimple:
    begin
      Columns := 4;
      Rows := 4;
      for I := 0 to AdvColorSelectorColorSetCount do
        TAdvColorSelectorExItem(Items.Add).Color := AdvColorSelectorColorSet[I];
    end;
    csmExtended:
    begin
      Columns := 8;
      Rows := 5;
      for I := 0 to AdvColorSelectorExtendedColorSetCount do
        TAdvColorSelectorExItem(Items.Add).Color := AdvColorSelectorExtendedColorSet[I];
    end;
    csmExtendedMore:
    begin
      Columns := 8;
      Rows := 7;
      for I := 0 to AdvColorSelectorExtendedColorSetCount do
        TAdvColorSelectorExItem(Items.Add).Color := AdvColorSelectorExtendedColorSet[I];

      it := Items.Insert(0);
      TAdvColorSelectorExItem(it).Color := gcBlack;
      it.ColumnSpan := 8;
      it.Text := 'Automatic';
      it.DataBoolean := True;

      it := Items.Add;
      it.ColumnSpan := 8;
      it.Text := 'More Colors...';
      it.CanSelect := False;
      it.DataBoolean := True;

      Items.Add.Visible := False;
    end;
  end;
  EndUpdate;
end;

procedure TAdvCustomColorSelector.BackButtonClicked(Sender: TObject);
begin
  if Assigned(FPanel) then
  begin
    FPanel.Visible := False;
    FPanel.Parent := nil;
    Width := Round(w);
    Height := Round(h);
    WrapperSizeChanged(w, h);
    Invalidate;
  end;
end;

procedure TAdvCustomColorSelector.WrapperSizeChanged(AWidth, AHeight: Single);
begin

end;

function TAdvCustomColorSelector.ColorWheelActive: Boolean;
begin
  Result := Assigned(FPanel) and Assigned(FPanel.Parent) and FPanel.Visible
    and Assigned(FColorWheel) and Assigned(FColorWheel.Parent) and FColorWheel.Visible
end;

procedure TAdvCustomColorSelector.ColorWheelChange(Sender: TObject; AColor: TAdvGraphicsColor);
begin
  if BlockChange then
    Exit;

  BlockChange := True;
  SelectedItemIndex := -1;
  Items[Items.Count - 1].Color := AsColorWheel(FColorWheel).SelectedColor;
  SelectedColor := AsColorWheel(FColorWheel).SelectedColor;
  DoItemSelected(SelectedItemIndex);
  Invalidate;
  BlockChange := False;
end;

constructor TAdvCustomColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  Width := 135;
  Height := 135;
  FMode := csmSimple;

  FPanel := TAdvCustomControl.Create(Self);
  FPanel.Visible := False;
  FPanel.Stored := False;
  FPanel.ControlAlignment := caClient;

  FBackButton := TButton.Create(FPanel);
  {$IFDEF CMNWEBLIB}
  {$IFNDEF LCLLIB}
  FBackButton.AlignWithMargins := True;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  FBackButton.BorderSpacing.Top := 3;
  FBackButton.BorderSpacing.Left := 3;
  FBackButton.BorderSpacing.Bottom := 3;
  FBackButton.BorderSpacing.Right := 3;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  FBackButton.Margins.Top := 3;
  FBackButton.Margins.Left := 3;
  FBackButton.Margins.Bottom := 3;
  FBackButton.Margins.Right := 3;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FBackButton.Text := 'Back';
  FBackButton.Align := TAlignLayout.Top;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  FBackButton.Caption := 'Back';
  FBackButton.Align := alTop;
  {$ENDIF}
  FBackButton.Parent := FPanel;
  FBackButton.OnClick := BackButtonClicked;

  FColorWheel := TAdvColorWheel.Create(FPanel);
  AsColorWheel(FColorWheel).ControlAlignment := caClient;
  AsColorWheel(FColorWheel).SetControlMargins(3, 0, 3, 3);
  AsColorWheel(FColorWheel).Parent := FPanel;
  AsColorWheel(FColorWheel).OnSelectedColorChanged := ColorWheelChange;

  if IsDesignTime then
    InitSample;
end;

function TAdvCustomColorSelector.CreateItemsCollection: TAdvCustomSelectorExItems;
begin
  Result := TAdvColorSelectorExItems.Create(Self);
end;

procedure TAdvCustomColorSelector.DoItemBeforeDrawContent(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean);
begin
  if Mode = csmExtendedMore then
  begin
    if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
      ADefaultDraw := not Items[AItemIndex].DataBoolean;
  end;

  inherited;
end;

procedure TAdvCustomColorSelector.DoItemClick(AItemIndex: Integer);
var
  l, t, r, b, nw, nh: Single;
begin
  if ClosedRemotely then
  begin
    ClosedRemotely := False;
    inherited;
    Exit;
  end;

  if (Mode = csmExtendedMore) and Assigned(FPanel) and (AItemIndex = Items.Count - 2) then
  begin
    w := Width;
    h := Height;
    l := 0;
    t := 0;
    r := 0;
    b := 0;
    FColorWheel.GetControlMargins(l, t, r, b);
    nh := ScalePaintValue(205) + FBackButton.Height + t + b;
    Height := Round(nh);
    nw := ScalePaintValue(350);
    Width := Round(nw);
    WrapperSizeChanged(nw, nh);
    BlockChange := True;
    AsColorWheel(FColorWheel).SelectedColor := FindColorByItem(AItemIndex + 1);
    BlockChange := False;
    FPanel.Visible := True;
    FPanel.Parent := Self;
  end;

  inherited;
end;

procedure TAdvCustomColorSelector.DoItemDeselected(AItemIndex: Integer);
begin
  inherited;
  if Assigned(OnColorDeselected) then
    OnColorDeselected(Self, FindColorByItem(AItemIndex));
end;

procedure TAdvCustomColorSelector.DoItemSelected(AItemIndex: Integer);
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := FindColorByItem(AItemIndex);
  if Assigned(FColorWheel) and not BlockChange then
  begin
    BlockChange := True;
    AsColorWheel(FColorWheel).SelectedColor := c;
    if (Mode = csmExtendedMore) and (Items.Count > 0) then
      Items[Items.Count - 1].Color := c;
    BlockChange := False;
  end;

  if Assigned(OnColorSelected) then
    OnColorSelected(Self, c);
end;

procedure TAdvCustomColorSelector.DrawItemContent(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  r: TRectF;
  it: TAdvCustomSelectorExItem;
  a: Boolean;
begin
  it := ADisplayItem.Item;
  if Assigned(it) and (it is TAdvColorSelectorExItem) then
  begin
    r := ADisplayItem.Rect;
    a := True;
    DoItemBeforeDrawContent(AGraphics, ADisplayItem.Rect, it.Index, a);
    if a then
    begin
      case Mode of
        csmSimple: InflateRectEx(r, -5, -5);
        csmExtended, csmExtendedMore: InflateRectEx(r, -3, -3);
      end;
      AGraphics.Fill.Kind := gfkSolid;
      AGraphics.Fill.Color := (it as TAdvColorSelectorExItem).Color;
      AGraphics.Stroke.Color := AGraphics.Fill.Color;
      AGraphics.DrawRectangle(r);
      DoItemAfterDrawContent(AGraphics, ADisplayItem.Rect, it.Index);
    end;
  end;
end;

function TAdvCustomColorSelector.FindColorByItem(
  AItem: Integer): TAdvGraphicsColor;
var
  I: Integer;
  it: TAdvColorSelectorExItem;
begin
  Result := gcNull;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I] as TAdvColorSelectorExItem;
    if it.Index = AItem then
    begin
      Result := it.Color;
      Break;
    end;
  end;
end;

function TAdvCustomColorSelector.FindItemByColor(
  AColor: TAdvGraphicsColor): Integer;
var
  I: Integer;
  it: TAdvColorSelectorExItem;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I] as TAdvColorSelectorExItem;
    if (it.Color = AColor) and it.CanSelect then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomColorSelector.GetItems: TAdvColorSelectorExItems;
begin
  Result := TAdvColorSelectorExItems(inherited Items);
end;

function TAdvCustomColorSelector.GetSelectedColor: TAdvGraphicsColor;
begin
  Result := FindColorByItem(SelectedItemIndex);
end;

function TAdvCustomColorSelector.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomColorSelector.InitializeDefault;
begin
  inherited;
  AddColors;
end;

procedure TAdvCustomColorSelector.InitSample;
begin
  ResetToDefaultStyle;
end;

procedure TAdvCustomColorSelector.ResetToDefaultStyle;
begin
  inherited;
  BeginUpdate;

  {$IFDEF FMXLIB}
  Appearance.Font.Color := $FF454545;
  Appearance.Fill.Color := $FFEEF2F9;
  Appearance.FillSelected.Color := $FFA8BCF0;
  Appearance.StrokeSelected.Color := $FF2D9BEF;
  Appearance.FillDown.Color := $FF5A81E6;
  Appearance.FillHover.Color := $FFFFFFFE;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Appearance.Font.Color := $454545;
  Appearance.Fill.Color := $F9F2EE;
  Appearance.FillSelected.Color := $F0BCA8;
  Appearance.StrokeSelected.Color := $EF9B2D;
  Appearance.FillDown.Color := $E6815A;
  {$ENDIF}

  Appearance.FillSelected.Kind := gfkSolid;
  Appearance.Fill.Kind := gfkSolid;
  Appearance.FillDown.Kind := gfkSolid;
  Appearance.FillHover.Kind := gfkSolid;
  Appearance.Stroke.Kind := gskSolid;
  Appearance.StrokeSelected.Kind := gskSolid;
  Appearance.StrokeHover.Kind := gskSolid;

  Appearance.Stroke.Color := gcDarkgray;
  Appearance.StrokeHover.Color := Appearance.FillDown.Color;
  Appearance.StrokeDown.Assign(Appearance.Stroke);

  EndUpdate;
end;

procedure TAdvCustomColorSelector.SetItems(
  const Value: TAdvColorSelectorExItems);
begin
  Items.Assign(Value);
end;

procedure TAdvCustomColorSelector.SetMode(const Value: TAdvCustomColorSelectorMode);
var
  s: Single;
begin
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF (COMPILERVERSION >= 33) and (COMPILERVERSION < 35)}
  s := PaintScaleFactor;
  {$ELSE}
  s := TAdvUtils.GetDPIScale(Self, 96);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFNDEF VCLLIB}
  s := 1;
  {$ENDIF}

  if FMode <> Value then
  begin
    FMode := Value;
    case Mode of
      csmSimple:
      begin
        Width := Round(s * 135);
        Height := Round(s * 135);
      end;
      csmExtended:
      begin
        Width := Round(s * 200);
        Height := Round(s * 135);
      end;
      csmExtendedMore:
      begin
        Width := Round(s * 200);
        Height := Round(s * 175);
      end;
    end;
    AddColors;
  end;
end;

procedure TAdvCustomColorSelector.SetSelectedColor(const Value: TAdvGraphicsColor);
begin
  case Mode of
    csmExtendedMore:
    begin
      if Items.Count > 0 then
        Items[Items.Count - 1].Color := Value;
    end;
  end;
  SelectedItemIndex := FindItemByColor(Value);
end;

{ TAdvColorSelectorExItems }

constructor TAdvColorSelectorExItems.Create(AOwner: TAdvCustomSelectorEx);
begin
  inherited;
  FOwner := AOwner as TAdvCustomColorSelector;
end;

function TAdvColorSelectorExItems.Add: TAdvColorSelectorExItem;
begin
  Result := TAdvColorSelectorExItem(inherited Add);
end;

function TAdvColorSelectorExItems.Insert(
  Index: integer): TAdvColorSelectorExItem;
begin
  Result := TAdvColorSelectorExItem(inherited Insert(Index));
end;

function TAdvColorSelectorExItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvColorSelectorExItem;
end;

function TAdvColorSelectorExItems.GetItem(
  Index: integer): TAdvColorSelectorExItem;
begin
  Result := TAdvColorSelectorExItem(inherited Items[Index]);
end;

procedure TAdvColorSelectorExItems.SetItem(Index: integer;
  const Value: TAdvColorSelectorExItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvColorSelectorExItem }

procedure TAdvColorSelectorExItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvColorSelectorExItem then
    FColor := (Source as TAdvColorSelectorExItem).Color;
end;

constructor TAdvColorSelectorExItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FOwner := (Collection as TAdvColorSelectorExItems).FOwner;
end;

procedure TAdvColorSelectorExItem.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FOwner) then
      FOwner.InvalidateItems;
  end;
end;

{ TAdvColorSelectorEx }

procedure TAdvColorSelectorEx.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClasses([TAdvColorSelectorEx, TAdvColorSelectorExItem]);
end;

end.
