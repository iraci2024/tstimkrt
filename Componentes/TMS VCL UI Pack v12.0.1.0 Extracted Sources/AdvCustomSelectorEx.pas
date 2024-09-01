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

unit AdvCustomSelectorEx;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvTypes, AdvGraphics, AdvGraphicsTypes,
  AdvCustomControl, Controls
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  ,UITypes, Generics.Collections, Types
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  //Version history
  //v1.0.0.0 : First Release
  //v1.0.1.0 : New : High DPI support for TAdvCustomSelectorEx

type
  TAdvCustomSelectorEx = class;

  TAdvCustomSelectorExItemState = (isNormal, isHover, isDown, isSelected, isDisabled);

  TAdvCustomSelectorExItem = class(TCollectionItem)
  private
    FOwner: TAdvCustomSelectorEx;
    FRowSpan: Integer;
    FColumnSpan: Integer;
    FVisible: Boolean;
    FText: String;
    FEnabled: Boolean;
    FSeparator: Boolean;
    FSeparatorHeight: Single;
    FMargins: TAdvMargins;
    FCanDeselect: Boolean;
    FCanSelect: Boolean;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FHorizontalTextAlign: TAdvGraphicsTextAlign;
    FHint: string;
    FDataBoolean: Boolean;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FDataPointer: Pointer;
    procedure SetColumnSpan(const Value: Integer);
    procedure SetRowSpan(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: String);
    procedure SetEnabled(const Value: Boolean);
    procedure SetSeparator(const Value: Boolean);
    procedure SetSeparatorHeight(const Value: Single);
    procedure SetMargins(const Value: TAdvMargins);
    procedure SetCanDeselect(const Value: Boolean);
    procedure SetCanSelect(const Value: Boolean);
    procedure SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    function IsSeparatorHeightStored: Boolean;
  protected
    procedure MarginsChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function State: TAdvCustomSelectorExItemState;
    destructor Destroy; override;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
  published
    property CanSelect: Boolean read FCanSelect write SetCanSelect default True;
    property CanDeselect: Boolean read FCanDeselect write SetCanDeselect default True;
    property ColumnSpan: Integer read FColumnSpan write SetColumnSpan default 1;
    property RowSpan: Integer read FRowSpan write SetRowSpan default 1;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Separator: Boolean read FSeparator write SetSeparator default False;
    property SeparatorHeight: Single read FSeparatorHeight write SetSeparatorHeight stored IsSeparatorHeightStored nodefault;
    property Margins: TAdvMargins read FMargins write SetMargins;
    property Text: String read FText write SetText;
    property HorizontalTextAlign: TAdvGraphicsTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default gtaCenter;
    property VerticalTextAlign: TAdvGraphicsTextAlign read FVerticalTextAlign write SetVerticalTextAlign default gtaCenter;
    property Hint: string read FHint write FHint;
  end;

  {$IFDEF WEBLIB}
  TAdvCustomSelectorExItems = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvCustomSelectorExItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvCustomSelectorExItem>)
  {$ENDIF}
  private
    FOwner: TAdvCustomSelectorEx;
    function GetItem(Index: Integer): TAdvCustomSelectorExItem;
    procedure SetItem(Index: Integer; const Value: TAdvCustomSelectorExItem);
  protected
    function CreateItemClass: TCollectionItemClass; virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvCustomSelectorEx); virtual;
    property Items[Index: Integer]: TAdvCustomSelectorExItem read GetItem write SetItem; default;
    function Add: TAdvCustomSelectorExItem;
    function Insert(Index: Integer): TAdvCustomSelectorExItem;
  end;

  TAdvCustomSelectorExAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomSelectorEx;
    FStrokeHover: TAdvGraphicsStroke;
    FFillDown: TAdvGraphicsFill;
    FVerticalSpacing: Single;
    FStrokeDown: TAdvGraphicsStroke;
    FFillSelected: TAdvGraphicsFill;
    FHorizontalSpacing: Single;
    FStrokeSelected: TAdvGraphicsStroke;
    FFill: TAdvGraphicsFill;
    FFillHover: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FFillDisabled: TAdvGraphicsFill;
    FStrokeDisabled: TAdvGraphicsStroke;
    FSeparatorStroke: TAdvGraphicsStroke;
    FFont: TAdvGraphicsFont;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetFillDown(const Value: TAdvGraphicsFill);
    procedure SetFillHover(const Value: TAdvGraphicsFill);
    procedure SetFillSelected(const Value: TAdvGraphicsFill);
    procedure SetHorizontalSpacing(const Value: Single);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetStrokeDown(const Value: TAdvGraphicsStroke);
    procedure SetStrokeHover(const Value: TAdvGraphicsStroke);
    procedure SetStrokeSelected(const Value: TAdvGraphicsStroke);
    procedure SetVerticalSpacing(const Value: Single);
    procedure SetFillDisabled(const Value: TAdvGraphicsFill);
    procedure SetStrokeDisabled(const Value: TAdvGraphicsStroke);
    procedure SetSeparatorStroke(const Value: TAdvGraphicsStroke);
    procedure SetFont(const Value: TAdvGraphicsFont);
    function IsHorizontalSpacingStored: Boolean;
    function IsVerticalSpacingStored: Boolean;
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvCustomSelectorEx);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property StrokeHover: TAdvGraphicsStroke read FStrokeHover write SetStrokeHover;
    property FillHover: TAdvGraphicsFill read FFillHover write SetFillHover;
    property StrokeDown: TAdvGraphicsStroke read FStrokeDown write SetStrokeDown;
    property FillDown: TAdvGraphicsFill read FFillDown write SetFillDown;
    property StrokeSelected: TAdvGraphicsStroke read FStrokeSelected write SetStrokeSelected;
    property FillSelected: TAdvGraphicsFill read FFillSelected write SetFillSelected;
    property StrokeDisabled: TAdvGraphicsStroke read FStrokeDisabled write SetStrokeDisabled;
    property FillDisabled: TAdvGraphicsFill read FFillDisabled write SetFillDisabled;
    property VerticalSpacing: Single read FVerticalSpacing write SetVerticalSpacing stored IsVerticalSpacingStored nodefault;
    property HorizontalSpacing: Single read FHorizontalSpacing write SetHorizontalSpacing stored IsHorizontalSpacingStored nodefault;
    property SeparatorStroke: TAdvGraphicsStroke read FSeparatorStroke write SetSeparatorStroke;
    property Font: TAdvGraphicsFont read FFont write SetFont;
  end;

  TAdvCustomSelectorExPositionItem = record
    TileSet: Boolean;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvCustomSelectorExPositionItem) b : Boolean;
    {$ENDIF}
  end;

  TAdvCustomSelectorExDisplayItem = record
    Rect: TRectF;
    Item: TAdvCustomSelectorExItem;
    PageIndex: Integer;
    Column, Row, ColumnSpan, RowSpan: Integer;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvCustomSelectorExDisplayItem) b : Boolean;
    {$ENDIF}
  end;

  TAdvCustomSelectorExItemPosArray = array of array of TAdvCustomSelectorExPositionItem;

  TAdvCustomSelectorExItemSelected = procedure(Sender: TObject; AItemIndex: Integer) of object;
  TAdvCustomSelectorExItemDeselected = procedure(Sender: TObject; AItemIndex: Integer) of object;
  TAdvCustomSelectorExItemClick = procedure(Sender: TObject; AItemIndex: Integer) of object;
  TAdvCustomSelectorExItemBeforeDrawBackground = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean) of object;
  TAdvCustomSelectorExItemAfterDrawBackground = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer) of object;
  TAdvCustomSelectorExItemBeforeDrawContent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean) of object;
  TAdvCustomSelectorExItemAfterDrawContent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer) of object;
  TAdvCustomSelectorExItemBeforeDrawText = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var AText: String; var ADefaultDraw: Boolean) of object;
  TAdvCustomSelectorExItemAfterDrawText = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; AText: String) of object;
  TAdvCustomSelectorExBeforeDraw = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; var ADefaultDraw: Boolean) of object;
  TAdvCustomSelectorExAfterDraw = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF) of object;

  {$IFDEF WEBLIB}
  TAdvCustomSelectorExDisplayList = class(TList)
  private
    function GetItem(Index: Integer): TAdvCustomSelectorExDisplayItem;
    procedure SetItem(Index: Integer; const Value: TAdvCustomSelectorExDisplayItem);
  public
    property Items[Index: Integer]: TAdvCustomSelectorExDisplayItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvCustomSelectorExDisplayList = class(TList<TAdvCustomSelectorExDisplayItem>);
  {$ENDIF}

  TAdvCustomSelectorEx = class(TAdvCustomControl)
  private
    FDisplayList: TAdvCustomSelectorExDisplayList;
    FUpdateCount: Integer;
    FItems: TAdvCustomSelectorExItems;
    FRows: Integer;
    FColumns: Integer;
    FPageCount: Integer;
    FAppearance: TAdvCustomSelectorExAppearance;
    FSelectedItemIndex, FFocusedItemIndex, FHoveredItemIndex, FDownItemIndex: Integer;
    FOnItemBeforeDrawText: TAdvCustomSelectorExItemBeforeDrawText;
    FOnAfterDraw: TAdvCustomSelectorExAfterDraw;
    FOnItemAfterDrawBackground: TAdvCustomSelectorExItemAfterDrawBackground;
    FOnItemSelected: TAdvCustomSelectorExItemSelected;
    FOnItemAfterDrawText: TAdvCustomSelectorExItemAfterDrawText;
    FOnBeforeDraw: TAdvCustomSelectorExBeforeDraw;
    FOnItemBeforeDrawBackground: TAdvCustomSelectorExItemBeforeDrawBackground;
    FOnItemDeselected: TAdvCustomSelectorExItemDeselected;
    FOnItemClick: TAdvCustomSelectorExItemClick;
    FOnItemBeforeDrawContent: TAdvCustomSelectorExItemBeforeDrawContent;
    FOnItemAfterDrawContent: TAdvCustomSelectorExItemAfterDrawContent;
    FBlockChange: Boolean;
    FClosedRemotely: Boolean;
    procedure SetItems(const Value: TAdvCustomSelectorExItems);
    procedure SetColumns(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetAppearance(const Value: TAdvCustomSelectorExAppearance);
    procedure SetSelectedItemIndex(const Value: Integer);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure RegisterRuntimeClasses; override;
    function GetDocURL: string; override;
    function GetHintString: string; override;
    function HasHint: Boolean; override;
    function GetVersion: String; override;
    function GetDisplayItem(AItemIndex: Integer): TAdvCustomSelectorExDisplayItem; virtual;
    function GetNextSelectableItem: Integer; virtual;
    function GetPreviousSelectableItem: Integer; virtual;
    function GetNextSelectableRowItem: Integer; virtual;
    function GetPreviousSelectableRowItem: Integer; virtual;
    function GetFirstSelectableItem: Integer; virtual;
    function GetLastSelectableItem: Integer; virtual;
    function CreateItemsCollection: TAdvCustomSelectorExItems; virtual;
    procedure CalculateItems; virtual;
    procedure UpdateCalculations; virtual;
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;
    function GetTopOffset: Single; virtual;
    function GetCalculationWidth: Single; virtual;
    function GetCalculationHeight: Single; virtual;
    function GetTotalSeparatorHeight: Single;
    function GetTotalSeparatorCount: Integer;
    procedure DoItemSelected(AItemIndex: Integer); virtual;
    procedure DoItemClick(AItemIndex: Integer); virtual;
    procedure DoItemDeselected(AItemIndex: Integer); virtual;
    procedure DoItemBeforeDrawBackground(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean); virtual;
    procedure DoItemAfterDrawBackground(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer); virtual;
    procedure DoItemBeforeDrawContent(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean); virtual;
    procedure DoItemAfterDrawContent(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer); virtual;
    procedure DoItemBeforeDrawText(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; var AText: String; var ADefaultDraw: Boolean); virtual;
    procedure DoItemAfterDrawText(AGraphics: TAdvGraphics; ARect: TRectF; AItemIndex: Integer; AText: String); virtual;
    procedure DoBeforeDraw(AGraphics: TAdvGraphics; ARect: TRectF; var ADefaultDraw: Boolean); reintroduce; virtual;
    procedure DoAfterDraw(AGraphics: TAdvGraphics; ARect: TRectF); reintroduce; virtual;
    procedure DrawItems(AGraphics: TAdvGraphics); virtual;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;

    procedure DrawItem(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); virtual;
    procedure DrawItemBackGround(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); virtual;
    procedure DrawItemContent(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); virtual;
    procedure DrawItemText(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem); virtual;

    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure HandleMouseLeave; override;
    procedure ProcessSelection(AItemIndex: Integer);
    property SelectedItemIndex: Integer read FSelectedItemIndex write SetSelectedItemIndex default -1;
    property Rows: Integer read FRows write SetRows default 4;
    property Columns: Integer read FColumns write SetColumns default 4;
    property Version: String read GetVersion;
    property Items: TAdvCustomSelectorExItems read FItems write SetItems;
    property Appearance: TAdvCustomSelectorExAppearance read FAppearance write SetAppearance;
    property OnItemSelected: TAdvCustomSelectorExItemSelected read FOnItemSelected write FOnItemSelected;
    property OnItemDeselected: TAdvCustomSelectorExItemDeselected read FOnItemDeselected write FOnItemDeselected;
    property OnItemClick: TAdvCustomSelectorExItemClick read FOnItemClick write FOnItemClick;
    property OnItemBeforeDrawBackground: TAdvCustomSelectorExItemBeforeDrawBackground read FOnItemBeforeDrawBackground write FOnItemBeforeDrawBackground;
    property OnItemAfterDrawBackground: TAdvCustomSelectorExItemAfterDrawBackground read FOnItemAfterDrawBackground write FOnItemAfterDrawBackground;
    property OnItemBeforeDrawContent: TAdvCustomSelectorExItemBeforeDrawContent read FOnItemBeforeDrawContent write FOnItemBeforeDrawContent;
    property OnItemAfterDrawContent: TAdvCustomSelectorExItemAfterDrawContent read FOnItemAfterDrawContent write FOnItemAfterDrawContent;
    property OnBeforeDraw: TAdvCustomSelectorExBeforeDraw read FOnBeforeDraw write FOnBeforeDraw;
    property OnAfterDraw: TAdvCustomSelectorExAfterDraw read FOnAfterDraw write FOnAfterDraw;
    property OnItemBeforeDrawText: TAdvCustomSelectorExItemBeforeDrawText read FOnItemBeforeDrawText write FOnItemBeforeDrawText;
    property OnItemAfterDrawText: TAdvCustomSelectorExItemAfterDrawText read FOnItemAfterDrawText write FOnItemAfterDrawText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure InvalidateItems;
    property BlockChange: Boolean read FBlockChange write FBlockChange;
    procedure UpdateControlAfterResize; override;
    procedure InitializeDefault; virtual;
    function XYToItem(X, Y: Single): Integer;
    property ClosedRemotely: Boolean read FClosedRemotely write FClosedRemotely;
  end;

  TAdvDefaultSelector = class(TAdvCustomSelectorEx)
  published
    property Fill;
    property Stroke;
    property Version;
  end;

implementation

uses
  Math, SysUtils, AdvUtils, AdvGraphicsStyles
  {$IFDEF VCLLIB}
  , VCL.Graphics
  {$ENDIF}
  ;

{ TAdvCustomSelectorEx }

procedure TAdvCustomSelectorEx.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
    Fill.Color := c;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundStrokeColor(c) then
    Stroke.Color := c;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleDefaultButtonFillColor(c) then
    Appearance.Fill.Color := c;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleSelectionFillColor(c) then
  begin
    Appearance.FillSelected.Color := c;
    Appearance.FillDown.Color := c;
    Appearance.FillHover.Color := Blend(c, Appearance.Fill.Color, 25);
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleDefaultButtonStrokeColor(c) then
  begin
    Appearance.Stroke.Color := c;
    Appearance.StrokeSelected.Color := c;
    Appearance.StrokeDown.Color := c;
    Appearance.StrokeHover.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleTextFontColor(c) then
    Appearance.Font.Color := c;
end;

procedure TAdvCustomSelectorEx.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomSelectorEx then
  begin
    FItems.Assign((Source as TAdvCustomSelectorEx).Items);
    FAppearance.Assign((Source as TAdvCustomSelectorEx).Appearance);
    FRows := (Source as TAdvCustomSelectorEx).Rows;
    FColumns := (Source as TAdvCustomSelectorEx).Columns;
  end;
end;

procedure TAdvCustomSelectorEx.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

procedure TAdvCustomSelectorEx.CalculateItems;
var
  itposarr: TAdvCustomSelectorExItemPosArray;
  r, newr, c, newc: Integer;
  AItemIndex: Integer;
  AItem: TAdvCustomSelectorExItem;
  cspan, rspan, newcspan, newrspan: Integer;
  I: Integer;
  K: Integer;
  sepc: Integer;
  f: Boolean;
  APageIndex: Integer;
  hs, vs: Single;
  iw, ih: Single;
  w, h: Single;
  exw, exh: Single;
  tx, ty: Single;
  pw, ph: Single;
  offs: Single;
  itd: TAdvCustomSelectorExDisplayItem;

  procedure FindNewPos(AItem: TAdvCustomSelectorExItem; var ANewR: Integer; var ANewC: Integer; var AFound: Boolean; ARows , {%H-}ACurRow, AColumns, {%H-}ACurCol: Integer; PosArr: TAdvCustomSelectorExItemPosArray);
  var
    i, k: integer;
    cspan, rspan: Integer;
    J, L: Integer;
  begin
    AFound := False;
    for I := ANewr to ARows - 1 do
    begin
      for K := ANewC to AColumns - 1 do
      begin
        cspan := AItem.ColumnSpan;
        cspan := Min(cspan, Columns - ANewC);
        rspan := AItem.RowSpan;
        rspan := Min(rspan, Rows - ANewR);

        AFound := true;
        for J := 0 to rspan - 1 do
        begin
          for L := 0 to cspan - 1 do
          begin
            if PosArr[I + J, K + L].TileSet then
              AFound := False;
          end;
        end;

        if AFound then
          Break;
        Inc(ANewC);
      end;
      if AFound then
        Break;

      ANewC := 0;
      Inc(ANewr);
    end;
  end;
begin
  if (csDestroying in ComponentState) or (FUpdateCount > 0) then
    Exit;

  FDisplayList.Clear;

  if Items.Count = 0 then
    Exit;

  pw := GetCalculationWidth;
  ph := GetCalculationHeight;
  hs := Appearance.HorizontalSpacing;
  vs := Appearance.VerticalSpacing;
  w := pw - vs;
  iw := w;
  if Columns > 0 then
    iw := (w - (Columns * hs)) / Columns;

  h := ph - vs - GetTotalSeparatorHeight;
  sepc := GetTotalSeparatorCount;
  ih := h;
  if (Rows - sepc) > 0 then
    ih := (h - ((Rows - sepc) * vs)) / (Rows - sepc);

  AItemIndex := 0;
  APageIndex := 0;
  offs := 0;
  while AItemIndex <= Items.Count - 1 do
  begin
    c := 0;
    r := 0;
    SetLength(itposarr, 0, 0);
    SetLength(itposarr, Rows, Columns);

    while r < Rows do
    begin
      while (c < Columns) do
      begin
        if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) and (APageIndex = 0) then
        begin
          AItem := Items[AItemIndex];
          if not AItem.Visible then
          begin
            Inc(AItemIndex);
            Continue;
          end;

          if AItem.Separator then
          begin
            cspan := Columns;
            rspan := 1;
          end
          else
          begin
            cspan := AItem.ColumnSpan;
            rspan := AItem.RowSpan;
          end;

          cspan := Min(cspan, Columns - c);
          rspan := Min(rspan, Rows - r);

          exw := iw * cspan + (hs * (cspan - 1));
          if AItem.Separator then
            exh := AItem.SeparatorHeight
          else
            exh := ih * rspan + (vs * (rspan - 1));

          tx := (pw * APageIndex) + hs + iw * c + (hs * c);
          ty := offs + vs + ih * r + (vs * r) + GetTopOffset;

          itd.Rect := RectF(tx + AItem.Margins.Left, ty + AItem.Margins.Top,
            tx + exw - AItem.Margins.Right, ty + exh - AItem.Margins.Bottom);
          itd.Item := AItem;
          itd.PageIndex := APageIndex;
          itd.Column := c;
          itd.Row := r;
          itd.ColumnSpan := cspan;
          itd.RowSpan := rspan;
          FDisplayList.Add(itd);

          newcspan := c;
          newrspan := r;
          newcspan := newcspan + cspan - 1;
          newrspan := newrspan + rspan - 1;

          for I := r to newrspan do
            for K := c to newcspan do
              itposarr[I, K].TileSet := True;

          if AItem.Separator then
            offs := offs + AItem.SeparatorHeight - ih * rspan + (vs * (rspan - 1));
        end;
        Inc(AItemIndex);
        Inc(c);
        newc := c;
        newr := r;
        f := False;
        if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
          FindNewPos(Items[AItemIndex], newr, newc, f, Rows, newr, Columns, newc, itposarr);
        c := newc;
        r := newr;

        if (c >= Columns) or (r >= Rows) then
          Break;
      end;
      c := 0;
      Inc(r);
      newc := c;
      newr := r;
      f := False;
      if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
        FindNewPos(Items[AItemIndex], newr, newc, f, Rows, newr, Columns, newc, itposarr);
      c := newc;
      r := newr;
      if r >= Rows then
        Break;
    end;
    Inc(APageIndex);
  end;

  FPageCount := APageIndex;
  InvalidateItems;
end;

procedure TAdvCustomSelectorEx.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  BeginUpdate;
  FAppearance.FVerticalSpacing := TAdvUtils.MulDivSingle(FAppearance.FVerticalSpacing, M, D);
  FAppearance.FHorizontalSpacing := TAdvUtils.MulDivSingle(FAppearance.FHorizontalSpacing, M, D);
  FAppearance.Font.Height := TAdvUtils.MulDivInt(FAppearance.Font.Height, M, D);
  EndUpdate;
end;

constructor TAdvCustomSelectorEx.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := 4;
  FRows := 4;
  FItems := CreateItemsCollection;
  FDisplayList := TAdvCustomSelectorExDisplayList.Create;
  FAppearance := TAdvCustomSelectorExAppearance.Create(Self);
  FSelectedItemIndex := -1;
  FFocusedItemIndex := -1;
  FHoveredItemIndex := -1;
  FDownItemIndex := -1;

  if IsDesignTime then
    InitializeDefault;
end;

procedure TAdvCustomSelectorEx.InitializeDefault;
begin

end;

function TAdvCustomSelectorEx.CreateItemsCollection: TAdvCustomSelectorExItems;
begin
  Result := TAdvCustomSelectorExItems.Create(Self);
end;

destructor TAdvCustomSelectorEx.Destroy;
begin
  FAppearance.Free;
  FDisplayList.Free;
  FItems.Free;
  inherited;
end;

procedure TAdvCustomSelectorEx.DoAfterDraw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  if Assigned(OnAfterDraw) then
    OnAfterDraw(Self, AGraphics, ARect);
end;

procedure TAdvCustomSelectorEx.DoBeforeDraw(AGraphics: TAdvGraphics; ARect: TRectF;
  var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDraw) then
    OnBeforeDraw(Self, AGraphics, ARect, ADefaultDraw);
end;

procedure TAdvCustomSelectorEx.DoItemAfterDrawBackground(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer);
begin
  if Assigned(OnItemAfterDrawBackground) then
    OnItemAfterDrawBackground(Self, AGraphics, ARect, AItemIndex);
end;

procedure TAdvCustomSelectorEx.DoItemAfterDrawContent(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer);
begin
  if Assigned(OnItemAfterDrawContent) then
    OnItemAfterDrawContent(Self, AGraphics, ARect, AItemIndex);
end;

procedure TAdvCustomSelectorEx.DoItemAfterDrawText(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer; AText: String);
begin
  if Assigned(OnItemAfterDrawText) then
    OnItemAfterDrawText(Self, AGraphics, ARect, AItemIndex, AText);
end;

procedure TAdvCustomSelectorEx.DoItemBeforeDrawBackground(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean);
begin
  if Assigned(OnItemBeforeDrawBackground) then
    OnItemBeforeDrawBackground(Self, AGraphics, ARect, AItemIndex, ADefaultDraw);
end;

procedure TAdvCustomSelectorEx.DoItemBeforeDrawContent(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer; var ADefaultDraw: Boolean);
begin
  if Assigned(OnItemBeforeDrawContent) then
    OnItemBeforeDrawContent(Self, AGraphics, ARect, AItemIndex, ADefaultDraw);
end;

procedure TAdvCustomSelectorEx.DoItemBeforeDrawText(AGraphics: TAdvGraphics;
  ARect: TRectF; AItemIndex: Integer; var AText: String;
  var ADefaultDraw: Boolean);
begin
  if Assigned(OnItemBeforeDrawText) then
    OnItemBeforeDrawText(Self, AGraphics, ARect, AItemIndex, AText, ADefaultDraw);
end;

procedure TAdvCustomSelectorEx.DoItemSelected(AItemIndex: Integer);
begin
  if Assigned(OnItemSelected) then
    OnItemSelected(Self, AItemIndex);
end;

procedure TAdvCustomSelectorEx.DoItemClick(AItemIndex: Integer);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, AItemIndex);
end;

procedure TAdvCustomSelectorEx.DoItemDeselected(AItemIndex: Integer);
begin
  if Assigned(OnItemDeselected) then
    OnItemDeselected(Self, AItemIndex);
end;

procedure TAdvCustomSelectorEx.HandleMouseLeave;
begin
  inherited;
  FHoveredItemIndex := -1;
  InvalidateItems;
end;

procedure TAdvCustomSelectorEx.DrawItem(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
begin
  DrawItemBackGround(AGraphics, ADisplayItem);
  DrawItemContent(AGraphics, ADisplayItem);
  DrawItemText(AGraphics, ADisplayItem);
end;

procedure TAdvCustomSelectorEx.DrawItemBackGround(AGraphics: TAdvGraphics; ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  r: TRectF;
  it: TAdvCustomSelectorExItem;
  a: Boolean;
  fr: TRectF;
begin
  it := ADisplayItem.Item;
  if Assigned(it) then
  begin
    if not it.Separator then
    begin
      AGraphics.Fill.Assign(Appearance.Fill);
      AGraphics.Stroke.Assign(Appearance.Stroke);
      if it.Enabled then
      begin
        if it.Index = FDownItemIndex then
        begin
          AGraphics.Fill.Assign(Appearance.FillDown);
          AGraphics.Stroke.Assign(Appearance.StrokeDown);
        end
        else if it.Index = FHoveredItemIndex then
        begin
          AGraphics.Fill.Assign(Appearance.FillHover);
          AGraphics.Stroke.Assign(Appearance.StrokeHover);
        end
        else if it.Index = FSelectedItemIndex then
        begin
          AGraphics.Fill.Assign(Appearance.FillSelected);
          AGraphics.Stroke.Assign(Appearance.StrokeSelected);
        end;
      end
      else
      begin
        AGraphics.Fill.Assign(Appearance.FillDisabled);
        AGraphics.Stroke.Assign(Appearance.StrokeDisabled);
      end;
    end
    else
      AGraphics.Stroke.Assign(Appearance.SeparatorStroke);

    r := ADisplayItem.Rect;
    a := True;
    DoItemBeforeDrawBackground(AGraphics, ADisplayItem.Rect, it.Index, a);
    if a then
    begin
      if it.Separator then
        AGraphics.DrawLine(PointF(r.Left, CenterPointEx(r).Y), PointF(r.Right, CenterPointEx(r).Y))
      else
      begin
        AGraphics.DrawRectangle(r);
        if Focused and (FFocusedItemIndex = it.Index) then
        begin
          fr := r;
          InflateRectEx(fr, ScalePaintValue(-2), ScalePaintValue(-2));
          AGraphics.DrawFocusRectangle(fr);
        end;
      end;

      DoItemAfterDrawBackground(AGraphics, ADisplayItem.Rect, it.Index);
    end;
  end;
end;

procedure TAdvCustomSelectorEx.DrawItemContent(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  it: TAdvCustomSelectorExItem;
  a: Boolean;
begin
  it := ADisplayItem.Item;
  if Assigned(it) then
  begin
    a := True;
    DoItemBeforeDrawContent(AGraphics, ADisplayItem.Rect, it.Index, a);
    if a then
      DoItemAfterDrawContent(AGraphics, ADisplayItem.Rect, it.Index);
  end;
end;

procedure TAdvCustomSelectorEx.DrawItems(AGraphics: TAdvGraphics);
var
  I: Integer;
begin
  for I := 0 to FDisplayList.Count - 1 do
    DrawItem(AGraphics, FDisplayList[I]);
end;

procedure TAdvCustomSelectorEx.DrawItemText(AGraphics: TAdvGraphics;
  ADisplayItem: TAdvCustomSelectorExDisplayItem);
var
  r: TRectF;
  it: TAdvCustomSelectorExItem;
  str: String;
  a: Boolean;
begin
  it := ADisplayItem.Item;
  if Assigned(it) and (it.Text <> '') then
  begin
    r := ADisplayItem.Rect;
    str := it.Text;
    a := True;
    InflateRectEx(r, -2, -2);
    AGraphics.Font.AssignSource(Appearance.Font);
    DoItemBeforeDrawText(AGraphics, ADisplayItem.Rect, it.Index, str, a);
    if a then
    begin
      AGraphics.DrawText(r, str, False, it.HorizontalTextAlign, it.VerticalTextAlign);
      DoItemAfterDrawText(AGraphics, ADisplayItem.Rect, it.Index, str);
    end;
  end;
end;

procedure TAdvCustomSelectorEx.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    CalculateItems;
end;

function TAdvCustomSelectorEx.GetFirstSelectableItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result := FFocusedItemIndex;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not it.Separator then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetCalculationHeight: Single;
begin
  Result := Height - GetTopOffset;
end;

function TAdvCustomSelectorEx.GetHintString: string;
var
  it: TAdvCustomSelectorExItem;
begin
  Result := inherited GetHintString;
  if (FHoveredItemIndex >= 0) and (FHoveredItemIndex <= FItems.Count - 1) then
  begin
    it := FItems[FHoveredItemIndex];
    Result := it.Hint;
  end;
end;

function TAdvCustomSelectorEx.GetLastSelectableItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result := FFocusedItemIndex;
  for I := FDisplayList.Count - 1 downto 0 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not it.Separator then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetNextSelectableItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result := FFocusedItemIndex;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not it.Separator and (it.Index > FFocusedItemIndex) then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetNextSelectableRowItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
  disp: TAdvCustomSelectorExDisplayItem;
begin
  Result := FFocusedItemIndex;
  if Result = -1 then
  begin
    Result := GetNextSelectableItem;
    Exit;
  end;

  disp := GetDisplayItem(FFocusedItemIndex);
  for I := 0 to FDisplayList.Count - 1 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not it.Separator and (it.Index > FFocusedItemIndex) and (disp.Column >= FDisplayList[I].Column) and
      (disp.Column <= FDisplayList[I].Column + (FDisplayList[I].ColumnSpan - 1))
      and (FDisplayList[I].Row > disp.Row) then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetPreviousSelectableItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result := FFocusedItemIndex;
  for I := FDisplayList.Count - 1 downto 0 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not it.Separator and (it.Index < FFocusedItemIndex) then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetPreviousSelectableRowItem: Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
  disp: TAdvCustomSelectorExDisplayItem;
begin
  Result := FFocusedItemIndex;
  if Result = -1 then
  begin
    Result := GetPreviousSelectableItem;
    Exit;
  end;

  disp := GetDisplayItem(FFocusedItemIndex);
  for I := FDisplayList.Count - 1 downto 0 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and it.Enabled and not (it.Separator) and (it.Index < FFocusedItemIndex) and (disp.Column >= FDisplayList[I].Column) and
      (disp.Column <= FDisplayList[I].Column + (FDisplayList[I].ColumnSpan - 1)) and (FDisplayList[I].Row < disp.Row) then
    begin
      Result := it.Index;
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetTopOffset: Single;
begin
  Result := 0;
end;

function TAdvCustomSelectorEx.GetTotalSeparatorCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].Separator then
      Inc(Result);
  end;
end;

function TAdvCustomSelectorEx.GetTotalSeparatorHeight: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].Separator then
      Result := Result + Items[I].SeparatorHeight + Appearance.VerticalSpacing;
  end;
end;

function TAdvCustomSelectorEx.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

function TAdvCustomSelectorEx.GetCalculationWidth: Single;
begin
  Result := Width;
end;

procedure TAdvCustomSelectorEx.HandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    KEY_UP:
    begin
      FFocusedItemIndex := GetPreviousSelectableRowItem;
      InvalidateItems;
    end;
    KEY_LEFT:
    begin
      FFocusedItemIndex := GetPreviousSelectableItem;
      InvalidateItems;
    end;
    KEY_DOWN:
    begin
      FFocusedItemIndex := GetNextSelectableRowItem;
      InvalidateItems;
    end;
    KEY_RIGHT:
    begin
      FFocusedItemIndex := GetNextSelectableItem;
      InvalidateItems;
    end;
    KEY_HOME:
    begin
      FFocusedItemIndex := GetFirstSelectableItem;
      InvalidateItems;
    end;
    KEY_END:
    begin
      FFocusedItemIndex := GetLastSelectableItem;
      InvalidateItems;
    end;
  end;

  if (Key = KEY_RETURN) then
  begin
    FDownItemIndex := FFocusedItemIndex;
    InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorEx.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = KEY_RETURN) then
  begin
    FDownItemIndex := -1;
    ProcessSelection(FFocusedItemIndex);
    InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorEx.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  CaptureEx;
  FDownItemIndex := XYToItem(X, Y);
  InvalidateItems;
end;

procedure TAdvCustomSelectorEx.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  h: Integer;
begin
  inherited;
  if FDownItemIndex > -1 then
    Exit;

  h := XYToItem(X, Y);
  if h <> FHoveredItemIndex then
  begin
    FHoveredItemIndex := h;
    CancelHint;
    InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorEx.HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  s: Integer;
begin
  inherited;
  ReleaseCaptureEx;
  s := XYToItem(X, Y);
  if (s = FDownItemIndex) and (FDownItemIndex <> -1) then
  begin
    ProcessSelection(s);
    if s <> -1 then
      DoItemClick(s);
  end;
  FDownItemIndex := -1;
  FHoveredItemIndex := -1;
  InvalidateItems;
end;

function TAdvCustomSelectorEx.HasHint: Boolean;
var
  it: TAdvCustomSelectorExItem;
begin
  Result := False;
  if (FHoveredItemIndex >= 0) and (FHoveredItemIndex <= FItems.Count - 1) then
  begin
    it := FItems[FHoveredItemIndex];
    Result := it.Hint <> '';
  end;
end;

procedure TAdvCustomSelectorEx.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  a: Boolean;
begin
  inherited;
  a := True;
  DoBeforeDraw(AGraphics, ARect, a);
  if a then
  begin
    DrawItems(AGraphics);
    DoAfterDraw(AGraphics, ARect);
  end;
end;

procedure TAdvCustomSelectorEx.ProcessSelection(AItemIndex: Integer);
var
  it: TAdvCustomSelectorExItem;
  prev: Integer;
begin
  if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
  begin
    it := FItems[AItemIndex];
    if it.CanSelect then
    begin
      prev := FSelectedItemIndex;
      if it.CanDeselect and (it.Index = FSelectedItemIndex) then
        FSelectedItemIndex := -1
      else
        FSelectedItemIndex := it.Index;

      if FSelectedItemIndex <> -1 then
        FFocusedItemIndex := FSelectedItemIndex;

      if it.CanDeselect and (prev <> -1) then
        DoItemDeSelected(prev);

      if FSelectedItemIndex <> -1 then
        DoItemSelected(FSelectedItemIndex);
    end;
  end;
end;

procedure TAdvCustomSelectorEx.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvCustomSelectorEx);
end;

procedure TAdvCustomSelectorEx.InvalidateItems;
begin
  Invalidate;
end;

procedure TAdvCustomSelectorEx.ResetToDefaultStyle;
begin
  inherited;
  Fill.Kind := gfkSolid;
  Stroke.Kind := gskSolid;
  Fill.Color := gcWhite;
  Stroke.Color := gcSilver;

  Appearance.Fill.Color := Lighter(gcLightgray, 50);
  Appearance.FillHover.Color := Lighter(gcLightslategray, 50);
  Appearance.FillDown.Color := Lighter(gcSlategray, 50);
  Appearance.FillSelected.Color := Lighter(gcSlategray, 50);
  Appearance.FillDisabled.Color := Lighter(gcGray, 50);

  Appearance.Stroke.Color := gcDarkgray;
  Appearance.StrokeHover.Color := gcLightslategray;
  Appearance.StrokeDown.Color := gcSlategray;
  Appearance.StrokeSelected.Color := gcDarkslategray;
  Appearance.StrokeDisabled.Color := gcDarkgray;

  Appearance.Font.Color := gcBlack;

  Appearance.Fill.Kind := gfkSolid;
  Appearance.FillHover.Kind := gfkSolid;
  Appearance.FillDown.Kind := gfkSolid;
  Appearance.FillSelected.Kind := gfkSolid;
  Appearance.FillDisabled.Kind := gfkSolid;

  Appearance.Stroke.Kind := gskSolid;
  Appearance.StrokeHover.Kind := gskSolid;
  Appearance.StrokeDown.Kind := gskSolid;
  Appearance.StrokeSelected.Kind := gskSolid;
  Appearance.StrokeDisabled.Kind := gskSolid;

  Appearance.SeparatorStroke.Kind := gskSolid;
end;

procedure TAdvCustomSelectorEx.UpdateCalculations;
begin

end;

procedure TAdvCustomSelectorEx.UpdateControlAfterResize;
begin
  inherited;
  CalculateItems;
end;

function TAdvCustomSelectorEx.GetDisplayItem(AItemIndex: Integer): TAdvCustomSelectorExDisplayItem;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result.Rect := RectF(0, 0, 0, 0);
  Result.Item := nil;
  Result.PageIndex := -1;
  Result.Row := -1;
  Result.Column := -1;
  Result.ColumnSpan := -1;
  Result.RowSpan := -1;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    it := FDisplayList[I].Item;
    if Assigned(it) and (it.Index = AItemIndex) then
    begin
      Result := FDisplayList[I];
      Break;
    end;
  end;
end;

function TAdvCustomSelectorEx.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/' + LowerCase(ClassName);
end;

procedure TAdvCustomSelectorEx.SetAppearance(
  const Value: TAdvCustomSelectorExAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvCustomSelectorEx.SetColumns(const Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    UpdateCalculations;
    CalculateItems;
  end;
end;

procedure TAdvCustomSelectorEx.SetItems(const Value: TAdvCustomSelectorExItems);
begin
  FItems.Assign(Value);
end;

procedure TAdvCustomSelectorEx.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Value;
    UpdateCalculations;
    CalculateItems;
  end;
end;

procedure TAdvCustomSelectorEx.SetSelectedItemIndex(const Value: Integer);
begin
  if FSelectedItemIndex <> Value then
  begin
    FSelectedItemIndex := Value;
    FFocusedItemIndex := Value;
    InvalidateItems;
  end;
end;

function TAdvCustomSelectorEx.XYToItem(X, Y: Single): Integer;
var
  I: Integer;
  it: TAdvCustomSelectorExItem;
begin
  Result := -1;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    if PtInRectEx(FDisplayList[I].Rect, PointF(X, Y)) then
    begin
      it := FDisplayList[I].Item;
      if Assigned(it) and it.Enabled and not it.Separator then
      begin
        Result := it.Index;
        Break;
      end;
    end;
  end;
end;

{ TAdvCustomSelectorExItem }

procedure TAdvCustomSelectorExItem.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomSelectorExItem then
  begin
    FRowSpan := (Source as TAdvCustomSelectorExItem).RowSpan;
    FColumnSpan := (Source as TAdvCustomSelectorExItem).ColumnSpan;
    FVisible := (Source as TAdvCustomSelectorExItem).Visible;
    FText := (Source as TAdvCustomSelectorExItem).Text;
    FEnabled := (Source as TAdvCustomSelectorExItem).Enabled;
    FSeparator := (Source as TAdvCustomSelectorExItem).Separator;
    FSeparatorHeight := (Source as TAdvCustomSelectorExItem).SeparatorHeight;
    FMargins.Assign((Source as TAdvCustomSelectorExItem).Margins);
    FCanDeselect := (Source as TAdvCustomSelectorExItem).CanDeselect;
    FCanSelect := (Source as TAdvCustomSelectorExItem).CanSelect;
    FHint := (Source as TAdvCustomSelectorExItem).Hint;
  end;
end;

constructor TAdvCustomSelectorExItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FOwner := (Collection as TAdvCustomSelectorExItems).FOwner;
  FSeparator := False;
  FColumnSpan := 1;
  FCanDeselect := True;
  FCanSelect := True;
  FRowSpan := 1;
  FMargins := TAdvMargins.Create;
  FMargins.OnChange := MarginsChanged;
  FEnabled := True;
  FSeparatorHeight := 5;
  FVisible := True;
  if Assigned(FOwner) then
    FOwner.CalculateItems;
end;

destructor TAdvCustomSelectorExItem.Destroy;
begin
  FMargins.Free;
  inherited;
  if Assigned(FOwner) then
    FOwner.CalculateItems;
end;

function TAdvCustomSelectorExItem.IsSeparatorHeightStored: Boolean;
begin
  Result := SeparatorHeight <> 5;
end;

procedure TAdvCustomSelectorExItem.MarginsChanged(Sender: TObject);
begin
  FOwner.CalculateItems;
end;

procedure TAdvCustomSelectorExItem.SetCanDeselect(const Value: Boolean);
begin
  FCanDeselect := Value;
end;

procedure TAdvCustomSelectorExItem.SetCanSelect(const Value: Boolean);
begin
  FCanSelect := Value;
end;

procedure TAdvCustomSelectorExItem.SetColumnSpan(const Value: Integer);
begin
  if FColumnSpan <> Value then
  begin
    FColumnSpan := Value;
    FOwner.CalculateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetMargins(const Value: TAdvMargins);
begin
  FMargins.Assign(Value);
end;

procedure TAdvCustomSelectorExItem.SetRowSpan(const Value: Integer);
begin
  if FRowSpan <> Value then
  begin
    FRowSpan := Value;
    FOwner.CalculateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetSeparator(const Value: Boolean);
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    FOwner.CalculateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetSeparatorHeight(const Value: Single);
begin
  if FSeparatorHeight <> Value then
  begin
    FSeparatorHeight := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    FOwner.InvalidateItems;
  end;
end;

procedure TAdvCustomSelectorExItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    FOwner.CalculateItems;
  end;
end;

function TAdvCustomSelectorExItem.State: TAdvCustomSelectorExItemState;
begin
  Result := isNormal;
  if not Separator then
  begin
    if Enabled then
    begin
      if Index = FOwner.FDownItemIndex then
        Result := isDown
      else if Index = FOwner.FHoveredItemIndex then
        Result := isHover
      else if Index = FOwner.FSelectedItemIndex then
        Result := isSelected
    end
    else
      Result := isDisabled;
  end;
end;

{ TAdvCustomSelectorExItems }

function TAdvCustomSelectorExItems.Add: TAdvCustomSelectorExItem;
begin
  Result := TAdvCustomSelectorExItem(inherited Add);
end;

constructor TAdvCustomSelectorExItems.Create(AOwner: TAdvCustomSelectorEx);
begin
  inherited Create(AOwner, CreateItemClass);
  FOwner := AOwner;
end;

function TAdvCustomSelectorExItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvCustomSelectorExItem;
end;

function TAdvCustomSelectorExItems.GetItem(
  Index: Integer): TAdvCustomSelectorExItem;
begin
  Result := TAdvCustomSelectorExItem(inherited Items[Index]);
end;

function TAdvCustomSelectorExItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvCustomSelectorExItems.Insert(
  Index: Integer): TAdvCustomSelectorExItem;
begin
  Result := TAdvCustomSelectorExItem(inherited Insert(Index));
end;

procedure TAdvCustomSelectorExItems.SetItem(Index: Integer;
  const Value: TAdvCustomSelectorExItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvCustomSelectorExAppearance }

procedure TAdvCustomSelectorExAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomSelectorExAppearance then
  begin
    FFill.Assign((Source as TAdvCustomSelectorExAppearance).Fill);
    FFillHover.Assign((Source as TAdvCustomSelectorExAppearance).FillHover);
    FFillSelected.Assign((Source as TAdvCustomSelectorExAppearance).FillSelected);
    FFillDisabled.Assign((Source as TAdvCustomSelectorExAppearance).FillDisabled);
    FFillDown.Assign((Source as TAdvCustomSelectorExAppearance).FillDown);
    FStroke.Assign((Source as TAdvCustomSelectorExAppearance).Stroke);
    FStrokeHover.Assign((Source as TAdvCustomSelectorExAppearance).StrokeHover);
    FStrokeSelected.Assign((Source as TAdvCustomSelectorExAppearance).StrokeSelected);
    FStrokeDown.Assign((Source as TAdvCustomSelectorExAppearance).StrokeDown);
    FStrokeDisabled.Assign((Source as TAdvCustomSelectorExAppearance).StrokeDisabled);
    FVerticalSpacing := (Source as TAdvCustomSelectorExAppearance).VerticalSpacing;
    FHorizontalSpacing := (Source as TAdvCustomSelectorExAppearance).HorizontalSpacing;
    FSeparatorStroke.Assign((Source as TAdvCustomSelectorExAppearance).SeparatorStroke);
    FFont.AssignSource((Source as TAdvCustomSelectorExAppearance).Font);
  end;
end;

procedure TAdvCustomSelectorExAppearance.Changed;
begin
  FOwner.CalculateItems;
end;

constructor TAdvCustomSelectorExAppearance.Create(AOwner: TAdvCustomSelectorEx);
begin
  FOwner := AOwner;
  FFill := TAdvGraphicsFill.Create;
  FFill.Color := Lighter(gcLightgray, 50);
  FFillHover := TAdvGraphicsFill.Create;
  FFillHover.Color := Lighter(gcLightslategray, 50);
  FFillDown := TAdvGraphicsFill.Create;
  FFillDown.Color := Lighter(gcSlategray, 50);
  FFillSelected := TAdvGraphicsFill.Create;
  FFillSelected.Color := Lighter(gcSlategray, 50);
  FFillDisabled := TAdvGraphicsFill.Create;
  FFillDisabled.Color := Lighter(gcGray, 50);

  FStroke := TAdvGraphicsStroke.Create;
  FStroke.Color := gcDarkgray;
  FStrokeHover := TAdvGraphicsStroke.Create;
  FStrokeHover.Color := gcLightslategray;
  FStrokeDown := TAdvGraphicsStroke.Create;
  FStrokeDown.Color := gcSlategray;
  FStrokeSelected := TAdvGraphicsStroke.Create;
  FStrokeSelected.Color := gcDarkslategray;
  FStrokeDisabled := TAdvGraphicsStroke.Create;
  FStrokeDisabled.Color := gcDarkgray;

  FSeparatorStroke := TAdvGraphicsStroke.Create;
  FSeparatorStroke.Color := gcDarkGray;

  FFont := TAdvGraphicsFont.Create;
  FFont.OnChanged := FontChanged;

  FSeparatorStroke.OnChanged := StrokeChanged;
  FFont.OnChanged := FontChanged;

  FFill.OnChanged := FillChanged;
  FFillDown.OnChanged := FillChanged;
  FFillHover.OnChanged := FillChanged;
  FFillSelected.OnChanged := FillChanged;
  FFillDisabled.OnChanged := FillChanged;

  FStroke.OnChanged := StrokeChanged;
  FStrokeHover.OnChanged := StrokeChanged;
  FStrokeDown.OnChanged := StrokeChanged;
  FStrokeDisabled.OnChanged := StrokeChanged;
  FStrokeSelected.OnChanged := StrokeChanged;

  FHorizontalSpacing := 4;
  FVerticalSpacing := 4;
end;

destructor TAdvCustomSelectorExAppearance.Destroy;
begin
  FFont.Free;
  FFill.Free;
  FFillDown.Free;
  FFillSelected.Free;
  FFillHover.Free;
  FFillDisabled.Free;
  FSeparatorStroke.Free;
  FStroke.Free;
  FStrokeDown.Free;
  FStrokeSelected.Free;
  FStrokeHover.Free;
  FStrokeDisabled.Free;
  inherited;
end;

procedure TAdvCustomSelectorExAppearance.FillChanged(Sender: TObject);
begin
  FOwner.InvalidateItems;
end;

procedure TAdvCustomSelectorExAppearance.FontChanged(Sender: TObject);
begin
  FOwner.InvalidateItems;
end;

function TAdvCustomSelectorExAppearance.IsHorizontalSpacingStored: Boolean;
begin
  Result := HorizontalSpacing <> 4;
end;

function TAdvCustomSelectorExAppearance.IsVerticalSpacingStored: Boolean;
begin
  Result := VerticalSpacing <> 4;
end;

procedure TAdvCustomSelectorExAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetFillDisabled(const Value: TAdvGraphicsFill);
begin
  FFillDisabled.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetFillDown(const Value: TAdvGraphicsFill);
begin
  FFillDown.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetFillHover(const Value: TAdvGraphicsFill);
begin
  FFillHover.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetFillSelected(const Value: TAdvGraphicsFill);
begin
  FFillSelected.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.AssignSource(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetHorizontalSpacing(
  const Value: Single);
begin
  if FHorizontalSpacing <> Value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvCustomSelectorExAppearance.SetSeparatorStroke(
  const Value: TAdvGraphicsStroke);
begin
  FSeparatorStroke.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetStrokeDisabled(
  const Value: TAdvGraphicsStroke);
begin
  FStrokeDisabled.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetStrokeDown(
  const Value: TAdvGraphicsStroke);
begin
  FStrokeDown.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetStrokeHover(
  const Value: TAdvGraphicsStroke);
begin
  FStrokeHover.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetStrokeSelected(
  const Value: TAdvGraphicsStroke);
begin
  FStrokeSelected.Assign(Value);
end;

procedure TAdvCustomSelectorExAppearance.SetVerticalSpacing(const Value: Single);
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvCustomSelectorExAppearance.StrokeChanged(Sender: TObject);
begin
  FOwner.InvalidateItems;
end;

{ TAdvGraphicsPathPoint }

{$IFDEF LCLLIB}
class operator TAdvCustomSelectorExDisplayItem.=(z1, z2: TAdvCustomSelectorExDisplayItem)b: boolean;
begin
  Result := z1 = z2;
end;

class operator TAdvCustomSelectorExPositionItem.=(z1, z2: TAdvCustomSelectorExPositionItem)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

{$IFDEF WEBLIB}
function TAdvCustomSelectorExDisplayList.GetItem(Index: Integer): TAdvCustomSelectorExDisplayItem;
begin
  Result := TAdvCustomSelectorExDisplayItem(inherited Items[Index]);
end;

procedure TAdvCustomSelectorExDisplayList.SetItem(Index: Integer; const Value: TAdvCustomSelectorExDisplayItem);
var
  v: TAdvCustomSelectorExDisplayItem;
begin
  v := Value;
  inherited Items[Index] := v;
end;
{$ENDIF}

initialization
  RegisterClass(TAdvCustomSelectorEx);

end.
