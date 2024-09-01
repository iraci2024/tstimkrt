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

unit AdvTreeViewBase;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

{$IFNDEF LCLLIB}
{$IFNDEF WEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 22}
{$DEFINE USEOWNEDCOLLECTION}
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE USEOWNEDCOLLECTION}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, AdvCustomControl, AdvScrollBar, StdCtrls,
  Controls, AdvTypes
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
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

const
  {$IFDEF LCLLIB}
  SWIPECOUNT = 300;
  DOWNCOUNT = 15;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 15;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ELSE}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}

type
  TAdvTreeViewBase = class;

  TAdvTreeViewDoubleListItem = class(TCollectionItem)
  private
    FOwner: TAdvTreeViewBase;
    FValue: Double;
    FCellVal: Integer;
    procedure SetCellData(const Value: Double);
  public
    constructor Create(Collection: TCollection); override;
  published
    property Value: Double read FValue write SeTCellData;
    property CellVal: Integer read FCellVal write FCellVal;
  end;

  {$IFNDEF USEOWNEDCOLLECTION}
  TAdvTreeViewDoubleList = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFDEF USEOWNEDCOLLECTION}
  TAdvTreeViewDoubleList = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvTreeViewDoubleListItem>)
  {$ENDIF}
  private
    FOwner: TAdvTreeViewBase;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvTreeViewDoubleListItem;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewDoubleListItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    function IndexOf(ACellVal: Integer): Integer;
    constructor Create(AOwner: TAdvTreeViewBase);
    property Items[Index: Integer]: TAdvTreeViewDoubleListItem read GetItem write SetItem; default;
    function Add: TAdvTreeViewDoubleListItem;
    function Insert(Index: Integer): TAdvTreeViewDoubleListItem;
  end;

  TAdvTreeViewScrollMode = (smPixelScrolling, smNodeScrolling);
  TAdvTreeViewScrollUpdate = (suContinuous, suOnce);

  TAdvTreeViewBase = class(TAdvCustomControl)
  private
    FSaveHScrollPos, FSaveVScrollPos: Single;
    FTotalRowHeight: Double;
    FNodeListBuild: Boolean;
    FTotalColumnWidth: Double;
    FStartOffset: Double;
    FStartCol, FStopCol, FStartRow, FStopRow: Integer;
    FStartX, FStopX, FStartY, FStopY: Double;
    FBlockScrollingUpdate: Boolean;
    FUpdateCount: Integer;
    FVerticalScrollBar, FHorizontalScrollBar: TScrollBar;
    FCustomVerticalScrollBar, FCustomHorizontalScrollBar: TAdvScrollBar;
    FRowCount: Integer;
    FColumnCount: Integer;
    FColumnW, FColumnP: TAdvTreeViewDoubleList;
    FDefaultRowHeight: Double;
    FDefaultColumnWidth: Double;
    FVerticalScrollBarVisible: Boolean;
    FHorizontalScrollBarVisible: Boolean;
    FScrollMode: TAdvTreeViewScrollMode;
    FIsMouseDown: Boolean;
    FDelayedLoading: Boolean;
    FStretchScrollBars: Boolean;
    FBlockUpdateNodeList: Boolean;
    FBlockUpdateNode: Boolean;
    FBlockRemoveNode: Integer;
    FBlockUserInput: Boolean;
    FBlockMouseClick: Boolean;
    FCustomScrollBars: Boolean;
    FScrollUpdate: TAdvTreeViewScrollUpdate;
    procedure SetColumnCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetDefaultColumnWidth(const Value: Double);
    procedure SetDefaultRowHeight(const Value: Double);
    procedure SetHorizontalScrollBarVisible(const Value: Boolean);
    procedure SetVerticalScrollBarVisible(const Value: Boolean);
    procedure SetScrollMode(const Value: TAdvTreeViewScrollMode);
    procedure SetColWidths(Col: Integer; const Value: Double);
    procedure SetColumnP(const Value: TAdvTreeViewDoubleList);
    procedure SetColumnW(const Value: TAdvTreeViewDoubleList);
    function GetColWidths(Col: integer): Double;
    function GetColPos(Col: integer): Double;
    procedure SetColPos(Col: Integer; const Value: Double);
    procedure SetStretchScrollBars(const Value: Boolean);
    procedure SetCustomScrollBars(const Value: Boolean);
    procedure SetScrollUpdate(const Value: TAdvTreeViewScrollUpdate);
  protected
    function ColumnStretchingActive: Boolean; virtual; abstract;
    procedure UpdateTreeView; virtual;
    procedure UpdateColumns; virtual; abstract;
    procedure AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False); virtual; abstract;
    procedure UpdateVisualRange; virtual;
    procedure StretchColumn(AStretchAll: Boolean = True; ACol: Integer = -1; ANewWidth: Double = -1); virtual;
    procedure StopAnimationTimer; virtual; abstract;
    procedure Scroll(AHorizontalPos, AVerticalPos: Double); virtual;
    procedure UpdateDisplay; virtual;
    procedure VerticalScrollPositionChanged; virtual; abstract;
    procedure HorizontalScrollPositionChanged; virtual; abstract;
    procedure VScrollChanged(Sender: TObject);
    procedure HScrollChanged(Sender: TObject);
    procedure HCustomScrollChanged(Sender: TObject; {%H-}AValue: Double);
    procedure VCustomScrollChanged(Sender: TObject; {%H-}AValue: Double);
    procedure UpdateAutoSizing; virtual; abstract;
    procedure UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True); virtual; abstract;
    procedure UpdateScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
    procedure UpdateCalculations; virtual; abstract;
    procedure ResetNodes(AUpdateAll: Boolean = True); virtual; abstract;
    procedure UpdateColumnsCache; virtual; abstract;
    procedure UpdateGroupsCache; virtual; abstract;
    procedure UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False); virtual; abstract;
    procedure UpdateTreeViewCache; virtual;
    procedure UpdateTreeViewDisplay; virtual;
    procedure Loaded; override;
    procedure SetHScrollValue(AValue: Single); virtual;
    procedure SetVScrollValue(AValue: Single); virtual;
    procedure UpdateControlAfterResize; override;
    function CanScrollDown: Boolean; virtual;
    function CanScrollUp: Boolean; virtual;
    function ScrollLimitation: Boolean; virtual;
    function IsColumnVisible({%H-}ACol: Integer): Boolean; virtual;
    function GetRowHeight({%H-}ARow: Integer): Double; virtual;
    function GetCalculationRect: TRectF; virtual;
    function GetContentRect: TRectF; override;
    function GetContentClipRect: TRectF; virtual;
    function GetHScrollValue: Single; virtual;
    function GetVScrollValue: Single; virtual;
    function GetVViewPortSize: Single; virtual;
    function GetHViewPortSize: Single; virtual;
    function GetColumnViewPortSize: Double; virtual;
    function GetRowViewPortSize: Double; virtual;
    property TotalColumnWidth: Double read FTotalColumnWidth write FTotalColumnWidth;
    property TotalRowHeight: Double read FTotalRowHeight write FTotalRowHeight;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property BlockUserInput: Boolean read FBlockUserInput write FBlockUserInput;
    property BlockMouseClick: Boolean read FBlockMouseClick write FBlockMouseClick;
    property DefaultRowHeight: Double read FDefaultRowHeight write SetDefaultRowHeight;
    property DefaultColumnWidth: Double read FDefaultColumnWidth write SetDefaultColumnWidth;
    property HorizontalScrollBarVisible: Boolean read FHorizontalScrollBarVisible write SetHorizontalScrollBarVisible default True;
    property VerticalScrollBarVisible: Boolean read FVerticalScrollBarVisible write SetVerticalScrollBarVisible default True;
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColumnCount: Integer read FColumnCount write SetColumnCount;
    property ScrollMode: TAdvTreeViewScrollMode read FScrollMode write SetScrollMode default smPixelScrolling;
    property ScrollUpdate: TAdvTreeViewScrollUpdate read FScrollUpdate write SetScrollUpdate default suContinuous;
    property CustomScrollBars: Boolean read FCustomScrollBars write SetCustomScrollBars default False;
    property ColumnPositions[Col: Integer]: Double read GetColPos write SetColPos;
    property ColumnWidths[Col: Integer]: Double read GetColWidths write SetColWidths;
    property ColumnW: TAdvTreeViewDoubleList read FColumnW write SetColumnW;
    property ColumnP: TAdvTreeViewDoubleList read FColumnP write SetColumnP;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property BlockScrollingUpdate: Boolean read FBlockScrollingUpdate write FBlockScrollingUpdate;
    property StretchScrollBars: Boolean read FStretchScrollBars write SetStretchScrollBars default True;
    property BlockUpdateNodeList: Boolean read FBlockUpdateNodeList write FBlockUpdateNodeList;
    property BlockUpdateNode: Boolean read FBlockUpdateNode write FBlockUpdateNode;
    property NodeListBuild: Boolean read FNodeListBuild write FNodeListBuild;
    property BlockRemoveNode: Integer read FBlockRemoveNode write FBlockRemoveNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Resize; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure SaveScrollPosition; virtual;
    procedure RestoreScrollPosition; virtual;
    procedure AutoSizeColumn(ACol: Integer); virtual;
    function GetVerticalScrollPosition: Double; virtual;
    function GetHorizontalScrollPosition: Double; virtual;
    function HorizontalScrollBar: TScrollBar;
    function VerticalScrollBar: TScrollBar;
    function CustomHorizontalScrollBar: TAdvScrollBar;
    function CustomVerticalScrollBar: TAdvScrollBar;
    function StartCol: Integer;
    function StopCol: Integer;
    function StartX: Double;
    function StopX: Double;
    function StartRow: Integer;
    function StartOffset: Double;
    function StopRow: Integer;
    function StartY: Double;
    function StopY: Double;
    function GetColumnWidth(AColumn: Integer): Double; virtual;
    function GetColumnPosition(AColumn: Integer): Double; virtual;
    function GetTotalColumnNodeWidth: Double; virtual;
    function GetTotalColumnWidth: Double; virtual;
    function GetTotalRowHeight: Double; virtual;
  end;

implementation

uses
  Math, AdvUtils, SysUtils, Forms;

{ TAdvTreeViewBase }

procedure TAdvTreeViewBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvTreeViewBase then
  begin
    FScrollMode := (Source as TAdvTreeViewBase).ScrollMode;
    FStretchScrollBars := (Source as TAdvTreeViewBase).StretchScrollBars;
  end;
end;

procedure TAdvTreeViewBase.RestoreScrollPosition;
begin
  Scroll(FSaveHScrollPos, FSaveVScrollPos);
end;

procedure TAdvTreeViewBase.SaveScrollPosition;
begin
  FSaveHScrollPos := GetHScrollValue;
  FSaveVScrollPos := GetVScrollValue;
end;

procedure TAdvTreeViewBase.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

function TAdvTreeViewBase.CanScrollDown: Boolean;
begin
  Result := GetVerticalScrollPosition < GetTotalRowHeight - GetVViewPortSize;
end;

function TAdvTreeViewBase.CanScrollUp: Boolean;
begin
  Result := GetVerticalScrollPosition > 0;
end;

constructor TAdvTreeViewBase.Create(AOwner: TComponent);
begin
  inherited;
  FDelayedLoading := False;

  FStretchScrollBars := True;

  FScrollUpdate := suContinuous;

  FCustomVerticalScrollBar := TAdvScrollBar.Create(Self);
  FCustomVerticalScrollBar.Parent := Self;
  FCustomHorizontalScrollBar := TAdvScrollBar.Create(Self);
  FCustomHorizontalScrollBar.Parent := Self;

  FVerticalScrollBar := TScrollBar.Create(Self);
  FVerticalScrollBar.Parent := Self;
  FHorizontalScrollBar := TScrollBar.Create(Self);
  FHorizontalScrollBar.Parent := Self;

  FCustomVerticalScrollBar.Appearance.ThumbButtonSize := 0;
  FCustomHorizontalScrollBar.Appearance.ThumbButtonSize := 0;

  {$IFDEF FMXLIB}
  FVerticalScrollBar.Stored := False;
  FHorizontalScrollBar.Stored := False;
  FCustomVerticalScrollBar.Stored := False;
  FCustomHorizontalScrollBar.Stored := False;
  FVerticalScrollBar.Orientation := TOrientation.Vertical;
  {$ENDIF}

  FCustomHorizontalScrollBar.Kind := sbkHorizontal;

  {$IFDEF CMNWEBLIB}
  FVerticalScrollBar.DoubleBuffered := False;
  FHorizontalScrollBar.DoubleBuffered := False;
  FVerticalScrollBar.Kind := sbVertical;
  {$ENDIF}

  FVerticalScrollBar.Visible := True;
  FHorizontalScrollBar.Visible := True;
  FCustomVerticalScrollBar.Visible := False;
  FCustomHorizontalScrollBar.Visible := False;
  FVerticalScrollBar.OnChange := VScrollChanged;
  FHorizontalScrollBar.OnChange := HScrollChanged;
  FCustomHorizontalScrollBar.OnValueChanged := HCustomScrollChanged;
  FCustomVerticalScrollBar.OnValueChanged := VCustomScrollChanged;

  FVerticalScrollBarVisible := True;
  FHorizontalScrollBarVisible := True;

  FScrollMode := smPixelScrolling;

  FColumnW := TAdvTreeViewDoubleList.Create(Self);
  FColumnP := TAdvTreeViewDoubleList.Create(Self);

  FDefaultRowHeight := 25;
  FDefaultColumnWidth := 75;
  FColumnCount := 0;
  FRowCount := 0;
  FVerticalScrollBarVisible := True;
  FHorizontalScrollBarVisible := True;
end;

function TAdvTreeViewBase.CustomHorizontalScrollBar: TAdvScrollBar;
begin
  Result := FCustomHorizontalScrollBar;
end;

function TAdvTreeViewBase.CustomVerticalScrollBar: TAdvScrollBar;
begin
  Result := FCustomVerticalScrollBar;
end;

destructor TAdvTreeViewBase.Destroy;
begin
  FColumnP.Free;
  FColumnW.Free;
  FCustomVerticalScrollBar.Free;
  FCustomHorizontalScrollBar.Free;
  FVerticalScrollBar.Free;
  FHorizontalScrollBar.Free;
  inherited;
end;

function TAdvTreeViewBase.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  if CustomScrollBars then
  begin
    if CustomHorizontalScrollBar.Visible then
      Result.Bottom := Result.Bottom - CustomHorizontalScrollBar.Height - 1;

    if CustomVerticalScrollBar.Visible then
      Result.Right := Result.Right - CustomVerticalScrollBar.Width - 1;
  end
  else
  begin
    if HorizontalScrollBar.Visible then
      Result.Bottom := Result.Bottom - HorizontalScrollBar.Height - 1;

    if VerticalScrollBar.Visible then
      Result.Right := Result.Right - VerticalScrollBar.Width - 1;
  end;
end;

function TAdvTreeViewBase.GetContentClipRect: TRectF;
begin
  Result := GetContentRect;
end;

function TAdvTreeViewBase.GetCalculationRect: TRectF;
begin
  Result := LocalRect;
end;

function TAdvTreeViewBase.GetHorizontalScrollPosition: Double;
var
  hVal, scrollh: Double;
begin
  hVal := GetHScrollValue;
  if ScrollMode = smNodeScrolling then
  begin
    scrollh := ColumnPositions[Round(hval)];
    hVal := scrollh;
  end;

  Result := hVal;
end;

function TAdvTreeViewBase.GetHScrollValue: Single;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.Value
  else
  begin
    {$IFDEF FMXLIB}
    Result := Min(HorizontalScrollBar.Max - HorizontalScrollBar.ViewportSize, Max(0, HorizontalScrollBar.Value));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFNDEF LCLLIB}
    Result := Round(Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, HorizontalScrollBar.Position)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    Result := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(HorizontalScrollBar.Position)));
    {$ELSE}
    case ScrollMode of
      smPixelScrolling: Result := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(HorizontalScrollBar.Position - HorizontalScrollBar.Position * (HorizontalScrollBar.PageSize / HorizontalScrollBar.Max))));
      smNodeScrolling: Result := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(HorizontalScrollBar.Position)));
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

function TAdvTreeViewBase.GetHViewPortSize: Single;
begin
  if CustomScrollBars then
    Result := CustomHorizontalScrollBar.PageSize
  else
  begin
    {$IFDEF FMXLIB}
    Result := HorizontalScrollBar.ViewportSize;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result := HorizontalScrollBar.PageSize;
    {$ENDIF}
  end;
end;

function TAdvTreeViewBase.GetRowHeight(ARow: Integer): Double;
begin
  Result := 0;
end;

function TAdvTreeViewBase.GetRowViewPortSize: Double;
var
  I,cnt: Integer;
  h: Double;
  ch: Double;
  r: TRectF;
begin
  r := GetContentRect;
  ch := r.Bottom - r.Top;
  Result := 0;
  h := 0;
  cnt := 0;
  for I := RowCount - 1 downto 0 do
  begin
    h := h + GetRowHeight(I);
    if h > ch then
    begin
      Result := Max(1, cnt);
      Break;
    end;
    Inc(cnt);
  end;
end;

function TAdvTreeViewBase.GetTotalColumnNodeWidth: Double;
begin
  Result := TotalColumnWidth;
end;

function TAdvTreeViewBase.GetTotalColumnWidth: Double;
begin
  Result := TotalColumnWidth;
end;

function TAdvTreeViewBase.GetTotalRowHeight: Double;
begin
  Result := TotalRowHeight;
end;

function TAdvTreeViewBase.GetVerticalScrollPosition: Double;
var
  vVal{, scrollv}: Double;
begin
  vVal := GetVScrollValue;
  if ScrollMode = smNodeScrolling then
  begin
//    scrollv := RowPositions[Round(vval)];
//    vVal := scrollv;
  end;

  Result := vVal;
end;

function TAdvTreeViewBase.GetVViewPortSize: Single;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.PageSize
  else
  begin
    {$IFDEF FMXLIB}
    Result := VerticalScrollBar.ViewportSize;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result := VerticalScrollBar.PageSize;
    {$ENDIF}
  end;
end;

function TAdvTreeViewBase.GetVScrollValue: Single;
begin
  if CustomScrollBars then
    Result := CustomVerticalScrollBar.Value
  else
  begin
    {$IFDEF FMXLIB}
    Result := Min(VerticalScrollBar.Max - VerticalScrollBar.ViewportSize, Max(0, VerticalScrollBar.Value));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFNDEF LCLLIB}
    Result := Round(Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, VerticalScrollBar.Position)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    Result := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(VerticalScrollBar.Position)));
    {$ELSE}
    case ScrollMode of
      smPixelScrolling: Result := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(VerticalScrollBar.Position - VerticalScrollBar.Position * (VerticalScrollBar.PageSize / VerticalScrollBar.Max))));
      smNodeScrolling: Result := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(VerticalScrollBar.Position)));
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure TAdvTreeViewBase.HCustomScrollChanged(Sender: TObject;
  AValue: Double);
begin
  if FBlockScrollingUpdate or not CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  HorizontalScrollPositionChanged;
  SetHScrollValue(GetHScrollValue);
end;

function TAdvTreeViewBase.HorizontalScrollBar: TScrollBar;
begin
  Result := FHorizontalScrollBar;
end;

procedure TAdvTreeViewBase.HScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  HorizontalScrollPositionChanged;
end;

function TAdvTreeViewBase.IsColumnVisible(ACol: Integer): Boolean;
begin
  Result := True;
end;

procedure TAdvTreeViewBase.Loaded;
begin
  inherited;
  UpdateTreeViewCache;
end;

procedure TAdvTreeViewBase.VCustomScrollChanged(Sender: TObject;
  AValue: Double);
begin
  if FBlockScrollingUpdate or not CustomScrollBars then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  VerticalScrollPositionChanged;
  SetVScrollValue(GetVScrollValue);
end;

function TAdvTreeViewBase.VerticalScrollBar: TScrollBar;
begin
  Result := FVerticalScrollBar;
end;

procedure TAdvTreeViewBase.VScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  VerticalScrollPositionChanged;
end;

procedure TAdvTreeViewBase.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateTreeViewCache;
end;

function TAdvTreeViewBase.GetColumnPosition(AColumn: Integer): Double;
begin
  Result := ColumnPositions[AColumn];
end;

function TAdvTreeViewBase.GetColumnViewPortSize: Double;
var
  I,cnt: Integer;
  w: Double;
  cw: Double;
  r: TRectF;
begin
  r := GetContentRect;
  cw := r.Right - r.Left;
  Result := 0;
  w := 0;
  cnt := 0;
  for I := ColumnCount - 1 downto 0 do
  begin
    w := w + ColumnWidths[I];
    if w > cw then
    begin
      Result := Max(1, cnt);
      Break;
    end;
    Inc(cnt);
  end;
end;

function TAdvTreeViewBase.GetColumnWidth(AColumn: Integer): Double;
begin
  Result := ColumnWidths[AColumn];
end;

function TAdvTreeViewBase.GetColWidths(Col: Integer): Double;
var
  idx: Integer;
begin
  idx := ColumnW.IndexOf(Col);
  if idx <> -1 then
    Result := ColumnW[idx].Value
  else
    Result := DefaultColumnWidth;
end;

function TAdvTreeViewBase.GetColPos(Col: Integer): Double;
var
  idx: Integer;
begin
  idx := ColumnP.IndexOf(Col);
  if idx <> -1 then
    Result := ColumnP[idx].Value
  else
    Result := -1;
end;

procedure TAdvTreeViewBase.Resize;
begin
  inherited;
  {$IFNDEF LCLLIB}
  UpdateControlAfterResize;
  {$ENDIF}
end;

procedure TAdvTreeViewBase.Scroll(AHorizontalPos, AVerticalPos: Double);
begin
  FBlockScrollingUpdate := True;
  SetHScrollValue(AHorizontalPos);
  SetVScrollValue(AVerticalPos);
  FBlockScrollingUpdate := False;
  UpdateDisplay;
end;

function TAdvTreeViewBase.ScrollLimitation: Boolean;
begin
  Result := False;
end;

procedure TAdvTreeViewBase.SetColumnCount(const Value: Integer);
begin
  if FColumnCount <> Value then
    FColumnCount := Value;
end;

procedure TAdvTreeViewBase.SetColumnW(const Value: TAdvTreeViewDoubleList);
begin
  FColumnW.Assign(Value);
end;

procedure TAdvTreeViewBase.SetColumnP(const Value: TAdvTreeViewDoubleList);
begin
  FColumnP.Assign(Value);
end;

procedure TAdvTreeViewBase.SetColWidths(Col: Integer; const Value: Double);
var
  idx: Integer;
  c: TAdvTreeViewDoubleListItem;
begin
  idx := ColumnW.IndexOf(Col);
  if (idx >= 0) and (idx <= ColumnW.Count - 1) then
  begin
    if Value = DefaultColumnWidth then
      ColumnW.Delete(idx)
    else
    begin
      ColumnW[idx].Value := Max(0, Value);
      ColumnW[idx].CellVal := Col;
    end;
  end
  else if Value <> DefaultColumnWidth then
  begin
    c := ColumnW.Add;
    c.Value := Max(0, Value);
    c.CellVal := Col;
  end;
end;

procedure TAdvTreeViewBase.SetCustomScrollBars(const Value: Boolean);
begin
  if FCustomScrollBars <> Value then
  begin
    FCustomScrollBars := Value;
    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetColPos(Col: Integer; const Value: Double);
var
  idx: Integer;
  c: TAdvTreeViewDoubleListItem;
begin
  idx := ColumnP.IndexOf(Col);
  if (idx >= 0) and (idx <= ColumnP.Count - 1) then
  begin
    if Value = -1 then
      ColumnP.Delete(idx)
    else
    begin
      ColumnP[idx].Value := Max(0, Value);
      ColumnP[idx].CellVal := Col;
    end;
  end
  else if Value <> -1 then
  begin
    c := ColumnP.Add;
    c.Value := Max(0, Value);
    c.CellVal := Col;
  end;
end;

procedure TAdvTreeViewBase.SetDefaultColumnWidth(const Value: Double);
begin
  if FDefaultColumnWidth <> Value then
    FDefaultColumnWidth := Value;
end;

procedure TAdvTreeViewBase.SetDefaultRowHeight(const Value: Double);
begin
  if FDefaultRowHeight <> Value then
    FDefaultRowHeight := Value;
end;

procedure TAdvTreeViewBase.SetHorizontalScrollBarVisible(
  const Value: Boolean);
begin
  if FHorizontalScrollBarVisible <> Value then
  begin
    FHorizontalScrollBarVisible := Value;
    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetHScrollValue(AValue: Single);
begin
  if CustomScrollBars then
    CustomHorizontalScrollBar.Value := AValue
  else
  begin
    {$IFDEF FMXLIB}
    HorizontalScrollBar.Value := Min(HorizontalScrollBar.Max - HorizontalScrollBar.ViewportSize, Max(0, AValue));
    {$ENDIF}
    {$IFDEF VCLLIB}
    HorizontalScrollBar.Position := Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ENDIF}
    {$IFDEF WEBLIB}
    HorizontalScrollBar.Position := Round(Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, AValue)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ELSE}
    case ScrollMode of
      smPixelScrolling: HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max, Max(0, Round(AValue + AValue * (HorizontalScrollBar.PageSize / (HorizontalScrollBar.Max - HorizontalScrollBar.PageSize)))));
      smNodeScrolling: HorizontalScrollBar.Position := {%H-}Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
    end;
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure TAdvTreeViewBase.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
    FRowCount := Value;
end;

procedure TAdvTreeViewBase.SetScrollMode(
  const Value: TAdvTreeViewScrollMode);
begin
  if FScrollMode <> Value then
  begin
    FScrollMode := Value;
    UpdateTreeViewDisplay;
  end;
end;

procedure TAdvTreeViewBase.SetScrollUpdate(
  const Value: TAdvTreeViewScrollUpdate);
begin
  if FScrollUpdate <> Value then
  begin
    FScrollUpdate := Value;
    if Assigned(FCustomVerticalScrollBar) then
      FCustomVerticalScrollBar.Tracking := ScrollUpdate = suContinuous;

    if Assigned(FCustomHorizontalScrollBar) then
      FCustomHorizontalScrollBar.Tracking := ScrollUpdate = suContinuous;

    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetStretchScrollBars(const Value: Boolean);
begin
  if FStretchScrollBars <> Value then
  begin
    FStretchScrollBars := Value;
    UpdateTreeViewDisplay;
  end;
end;

procedure TAdvTreeViewBase.SetVerticalScrollBarVisible(
  const Value: Boolean);
begin
  if FVerticalScrollBarVisible <> Value then
  begin
    FVerticalScrollBarVisible := Value;
    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetVScrollValue(AValue: Single);
begin
  if CustomScrollBars then
    CustomVerticalScrollBar.Value := AValue
  else
  begin
    {$IFDEF FMXLIB}
    VerticalScrollBar.Value := Min(VerticalScrollBar.Max - VerticalScrollBar.ViewportSize, Max(0, AValue));
    {$ENDIF}
    {$IFDEF VCLLIB}
    VerticalScrollBar.Position := Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ENDIF}
    {$IFDEF WEBLIB}
    VerticalScrollBar.Position := Round(Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, AValue)));
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    {$ELSE}
    case ScrollMode of
      smPixelScrolling: VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max, Max(0, Round(AValue + AValue * (VerticalScrollBar.PageSize / (VerticalScrollBar.Max - VerticalScrollBar.PageSize)))));
      smNodeScrolling: VerticalScrollBar.Position := {%H-}Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
    end;
    {$ENDIF}
    {$ENDIF}
  end;
end;

function TAdvTreeViewBase.StartCol: Integer;
begin
  Result := FStartCol;
end;

function TAdvTreeViewBase.StartOffset: Double;
begin
  Result := FStartOffset;
end;

function TAdvTreeViewBase.StartRow: Integer;
begin
  Result := FStartRow;
end;

function TAdvTreeViewBase.StartX: Double;
begin
  Result := FStartX;
end;

function TAdvTreeViewBase.StartY: Double;
begin
  Result := FStartY;
end;

function TAdvTreeViewBase.StopCol: Integer;
begin
  Result := FStopCol;
end;

function TAdvTreeViewBase.StopRow: Integer;
begin
  Result := FStopRow;
end;

function TAdvTreeViewBase.StopX: Double;
begin
  Result := FStopX;
end;

function TAdvTreeViewBase.StopY: Double;
begin
  Result := FStopY;
end;

procedure TAdvTreeViewBase.StretchColumn(AStretchAll: Boolean = True; ACol: Integer = -1; ANewWidth: Double = -1);
var
  i: Integer;
  w, nw, d: Double;
  cnt: Integer;
  horz, vert: TScrollBar;
  r: TRectF;
begin
  horz := HorizontalScrollBar;
  vert := VerticalScrollBar;
  if not Assigned(vert) or not Assigned(horz) then
    Exit;

  if ANewWidth = -1 then
  begin
    r := GetContentRect;
    nw := r.Right - r.Left;
  end
  else
    nw := ANewWidth;

  cnt := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    if IsColumnVisible(I) then
      cnt := cnt + 1;
  end;

  if ACol = - 1 then
    ACol := ColumnCount - 1;

  if (cnt = 0) then
    Exit;

  if (ACol >= ColumnCount) and not AStretchAll and not IsLoading then
  begin
    {$IFNDEF WEBLIB}
    raise Exception.Create('Stretch column index out of range');
    {$ENDIF}
    {$IFDEF WEBLIB}
    Exit;
    {$ENDIF}
  end;

  if ColumnCount = 1 then
  begin
    ColumnWidths[0] := nw;
    Exit;
  end;

  w := 0;

  if AStretchAll then
  begin
    if (cnt > 0) then
    begin
      d := nw;

      w := d / cnt;

      for i := 0 to ColumnCount - 1 do
      begin
        if IsColumnVisible(i) then
          ColumnWidths[i] := w;
      end;
    end;
  end
  else
  begin
    for i := 0 to ColumnCount - 1 do
    begin
      if (i <> ACol) and IsColumnVisible(i) then
        w := w + ColumnWidths[i];
    end;

    ColumnWidths[ACol] := nw - w {- 1};
  end;
end;

procedure TAdvTreeViewBase.AutoSizeColumn(ACol: Integer);
begin
  AutoSizeColumnInternal(ACol, True, True);
end;

procedure TAdvTreeViewBase.UpdateVisualRange;
var
  c, r: Integer;
  xval, yval: Double;
  cw, ch: Double;
  vpos, hpos: Double;
  cr: TRectF;
  reverse: Boolean;
  rh: Double;
begin
  hpos := GetHorizontalScrollPosition;
  vpos := GetVerticalScrollPosition;
  cr := GetContentRect;
  xval := -hpos;
  yval := -vpos;
  cw := cr.Right - cr.Left;
  ch := cr.Bottom - cr.Top;

  reverse := GetVScrollValue > VerticalScrollBar.Max / 2;

  if reverse then
    yval := yval + GetTotalRowHeight;

  FStartCol := -1;
  FStartRow := -1;
  FStopCol := -1;
  FStopRow := -1;
  FStartX := cr.Left;
  FStopX := cr.Left;
  FStartY := cr.Top;
  FStopY := cr.Top;
  FStartOffset := 0;

  for c := 0 to ColumnCount - 1 do
  begin
    xval := xval + ColumnWidths[c];
    if (xval > 0) and (FStartCol = -1) then
    begin
      FStartCol := c;
      FStartX := FStartX + int(xval - ColumnWidths[c]);
    end;

    if (xval >= cw) and (FStopCol = -1) then
    begin
      FStopCol := c;
      FStopX := FStopX + int(xval);
    end;

    if (FStartCol > -1) and (FStopCol > -1) then
      Break;
  end;

  if reverse then
  begin
    for r := RowCount - 1 downto 0 do
    begin
      rh := GetRowHeight(r);
      yval := yval - rh;
      if (yval <= 0) and (FStartRow = -1) then
      begin
        FStartRow := r;
        FStartOffset := yval;
        FStartY := FStartY + int(FStartOffset - rh);
      end;

      if (yval < ch) and (FStopRow = -1) then
      begin
        FStopRow := r;
        FStopY := FStopY + int(yval);
      end;

      if (FStartRow > -1) and (FStopRow > -1) then
        Break;
    end;
  end
  else
  begin
    for r := 0 to RowCount - 1 do
    begin
      rh := GetRowHeight(r);
      yval := yval + rh;
      if (yval > 0) and (FStartRow = -1) then
      begin
        FStartRow := r;
        FStartOffset := yval - rh;
        FStartY := FStartY + int(FStartOffset);
      end;

      if (yval >= ch) and (FStopRow = -1) then
      begin
        FStopRow := r;
        FStopY := FStopY + int(yval);
      end;

      if (FStartRow > -1) and (FStopRow > -1) then
        Break;
    end;
  end;

  if (FStartRow > -1) and (FStopRow = -1) then
    FStopRow := RowCount - 1;

  if (FStartCol > -1) and (FStopCol = -1) then
    FStopCol := ColumnCount - 1;
end;

procedure TAdvTreeViewBase.UpdateControlAfterResize;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ResetNodes;
  SaveScrollPosition;
  UpdateNodesCache(True, True);
  UpdateGroupsCache;
  UpdateColumnsCache;
  RestoreScrollPosition;
end;

procedure TAdvTreeViewBase.UpdateDisplay;
begin
  //ResetVisibleNodes;
end;

procedure TAdvTreeViewBase.UpdateTreeView;
begin
  SaveScrollPosition;
  UpdateNodesCache(True, True);
  RestoreScrollPosition;
  UpdateGroupsCache;
  UpdateColumnsCache;
end;

procedure TAdvTreeViewBase.UpdateTreeViewCache;
begin
  if (UpdateCount > 0) or BlockUpdateNode or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ResetNodes;
  UpdateCalculations;
  UpdateColumns;
  UpdateScrollBars;
  UpdateNodesCache(True, True);
  UpdateGroupsCache;
  UpdateColumnsCache;
end;

procedure TAdvTreeViewBase.UpdateTreeViewDisplay;
begin
  UpdateScrollBars;
  UpdateDisplay;
end;

procedure TAdvTreeViewBase.UpdateScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
var
  vs, hs: TScrollBar;
  vsc, hsc: TAdvScrollBar;
  w, h: Double;
  cw, ch: Double;
  cr: TRectF;
  vmgr: TAdvMargins;
  hmgr: TAdvMargins;
begin
  if {$IFDEF FMXLIB} FBlockScrollingUpdate or {$ENDIF} (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  FBlockScrollingUpdate := True;

  if ACalculate then
  begin
    UpdateAutoSizing;
    UpdateColumnRowCalculations;
  end;

  vs := VerticalScrollBar;
  hs := HorizontalScrollBar;
  vsc := CustomVerticalScrollBar;
  hsc := CustomHorizontalScrollBar;
  if Assigned(vs) and Assigned(hs) and Assigned(vsc) and Assigned(hsc) then
  begin
    if AUpdate then
      cr := GetCalculationRect
    else
      cr := GetContentRect;

    cw := cr.Right - cr.Left;
    ch := cr.Bottom - cr.Top;
    w := GetTotalColumnWidth;
    h := GetTotalRowHeight;

    hs.Visible := (w > 0) and (CompareValueEx(w, cw) = 1) and HorizontalScrollBarVisible and not ColumnStretchingActive and not CustomScrollBars;
    vs.Visible := (h > 0) and (CompareValueEx(h, ch) = 1) and VerticalScrollBarVisible and not CustomScrollBars;
    hsc.Visible := (w > 0) and (CompareValueEx(w, cw) = 1) and HorizontalScrollBarVisible and not ColumnStretchingActive and CustomScrollBars;
    vsc.Visible := (h > 0) and (CompareValueEx(h, ch) = 1) and VerticalScrollBarVisible and CustomScrollBars;

    {$IFDEF CMNWEBLIB}
    if vsc.Visible and CustomScrollBars then
      vsc.Parent := Self
    else
      vsc.Parent := nil;

    if hsc.Visible and CustomScrollBars then
      hsc.Parent := Self
    else
      hsc.Parent := nil;

    if vs.Visible and not CustomScrollBars then
      vs.Parent := Self
    else
      vs.Parent := nil;

    if hs.Visible and not CustomScrollBars then
      hs.Parent := Self
    else
      hs.Parent := nil;
    {$ENDIF}

    vmgr := TAdvMargins.Create;
    hmgr := TAdvMargins.Create;

    if CustomScrollBars then
    begin
      if vsc.Visible and StretchScrollBars then
        hmgr.Right := vsc.Width + 1
      else
        hmgr.Right := 1;
    end
    else
    begin
      if vs.Visible and StretchScrollBars then
        hmgr.Right := vs.Width + 1
      else
        hmgr.Right := 1;
    end;

    vmgr.Top := 1;
    vmgr.Right := 1;
    hmgr.Bottom := 1;
    hmgr.Left := 1;

    if CustomScrollBars then
    begin
      if hsc.Visible then
        vmgr.Bottom := hsc.Height + 1
      else
        vmgr.Bottom := 1;
    end
    else
    begin
      if hs.Visible then
        vmgr.Bottom := hs.Height + 1
      else
        vmgr.Bottom := 1;
    end;

    if not StretchScrollBars then
    begin
      cr := GetCalculationRect;
      hmgr.Left := hmgr.Left + cr.Left;
      hmgr.Right := hmgr.Right + (Width - cr.Right);
      vmgr.Top := vmgr.Top + cr.Top;
      vmgr.Bottom := vmgr.Bottom + (Height - cr.Bottom);
    end;

    {$IFDEF FMXLIB}
    hs.Position.X := hmgr.Left;
    hs.Position.Y := Height - hmgr.Bottom - hs.Height;
    vs.Position.X := Width - vmgr.Right - vs.Width;
    vs.Position.Y := vmgr.Top;
    hs.Width := Width - hmgr.Right - hmgr.Left;
    vs.Height := Height - vmgr.Bottom - vmgr.Top;
    hsc.Position.X := hmgr.Left;
    hsc.Position.Y := Height - hmgr.Bottom - hsc.Height;
    vsc.Position.X := Width - vmgr.Right - vsc.Width;
    vsc.Position.Y := vmgr.Top;
    hsc.Width := Width - hmgr.Right - hmgr.Left;
    vsc.Height := Height - vmgr.Bottom - vmgr.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    hs.Left := Round(hmgr.Left);
    hs.Top := Round(Height - hmgr.Bottom - hs.Height);
    vs.Left := Round(Width - vmgr.Right - vs.Width);
    vs.Top := Round(vmgr.Top);
    hs.Width := Round(Max(0, Width - hmgr.Right - hmgr.Left));
    vs.Height := Round(Max(0, Height - vmgr.Bottom - vmgr.Top));
    {$IFDEF MSWINDOWS}
    vs.Width := GetSystemMetrics(SM_CYVSCROLL);
    hs.Height := GetSystemMetrics(SM_CYHSCROLL);
    {$ENDIF}
    vs.Height := Round(Max(0, Height - vmgr.Bottom - vmgr.Top));
    hsc.Left := Round(hmgr.Left);
    hsc.Top := Round(Height - hmgr.Bottom - hsc.Height);
    vsc.Left := Round(Width - vmgr.Right - vsc.Width);
    vsc.Top := Round(vmgr.Top);
    hsc.Width := Round(Max(0, Width - hmgr.Right - hmgr.Left));
    vsc.Height := Round(Max(0, Height - vmgr.Bottom - vmgr.Top));
    {$ENDIF}

    hmgr.Free;
    vmgr.Free;

    cr := GetContentRect;
    cw := cr.Right - cr.Left;
    ch := cr.Bottom - cr.Top;

    {$IFDEF FMXLIB}
    if ScrollMode = smNodeScrolling then
    begin
      vs.ViewportSize := GetRowViewPortSize;
      vs.Max := RowCount;
    end
    else
    begin
      vs.ViewPortSize := Min(h, ch);
      vs.Max := h;
      vs.SmallChange := Round(DefaultRowHeight);
      vs.Value := Min(vs.Value, vs.Max - vs.ViewportSize);
    end;

    if ScrollMode = smNodeScrolling then
    begin
      hs.ViewportSize := GetColumnViewPortSize;
      hs.Max := ColumnCount;
    end
    else
    begin
      hs.ViewPortSize := Min(w, cw);
      hs.Max := w;
      hs.Value := Min(hs.Value, hs.Max - hs.ViewportSize);
    end;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    if ScrollMode = smNodeScrolling then
    begin
      vs.PageSize := Round(Max(0, GetRowViewPortSize));
      vs.Max := Max(vs.PageSize, RowCount);
    end
    else
    begin
      vs.PageSize := Round(Max(0, Min(h, ch)));
      vs.Max := Round(Max(vs.PageSize, h));
      vs.SmallChange := Round(DefaultRowHeight);
      vs.LargeChange := Max(1, vs.PageSize);
      vs.Position := Min(vs.Position, vs.Max);
    end;

    if ScrollMode = smNodeScrolling then
    begin
      hs.PageSize := Round(Max(0, GetColumnViewPortSize));
      hs.Max := Max(hs.PageSize, ColumnCount);
    end
    else
    begin
      hs.PageSize := Round(Max(0, Min(w, cw)));
      hs.Max := Round(Max(hs.PageSize, w));
      hs.Position := Min(hs.Position, hs.Max);
    end;
    {$ENDIF}

    if ScrollMode = smNodeScrolling then
    begin
      vsc.PageSize := GetRowViewPortSize;
      vsc.Max := RowCount - vsc.PageSize;
    end
    else
    begin
      vsc.PageSize := Min(h, ch);
      vsc.Max := h - vsc.PageSize;
      vsc.SmallChange := Round(DefaultRowHeight);
      vsc.Value := Min(vsc.Value, vsc.Max);
    end;

    if ScrollMode = smNodeScrolling then
    begin
      hsc.PageSize := GetColumnViewPortSize;
      hsc.Max := ColumnCount - hsc.PageSize;
    end
    else
    begin
      hsc.PageSize := Min(w, cw);
      hsc.Max := w - hsc.PageSize;
      hsc.Value := Min(hsc.Value, hsc.Max);
    end;
  end;

  FBlockScrollingUpdate := False;

  if AUpdate then
    UpdateScrollBars(False, False);
end;

{ TAdvTreeViewDoubleListItem }

constructor TAdvTreeViewDoubleListItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FOwner := (Collection as TAdvTreeViewDoubleList).FOwner;
end;

procedure TAdvTreeViewDoubleListItem.SetCellData(const Value: Double);
begin
  FValue := Value;
end;

{ TAdvTreeViewDoubleList }

function TAdvTreeViewDoubleList.Add: TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Add);
end;

constructor TAdvTreeViewDoubleList.Create(AOwner: TAdvTreeViewBase);
begin
  inherited Create(AOwner, TAdvTreeViewDoubleListItem);
  FOwner := AOwner;
end;

function TAdvTreeViewDoubleList.GetItem(
  Index: Integer): TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Items[Index]);
end;

function TAdvTreeViewDoubleList.IndexOf(ACellVal: Integer): Integer;
var
  r: Integer;
begin
  Result := -1;
  for r := 0 to Count - 1 do
  begin
    if Items[r].CellVal = ACellVal then
    begin
      Result := r;
      Exit;
    end;
  end;
end;

function TAdvTreeViewDoubleList.Insert(
  Index: Integer): TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Insert(Index));
end;

procedure TAdvTreeViewDoubleList.SetItem(Index: Integer;
  const Value: TAdvTreeViewDoubleListItem);
begin
  inherited Items[Index] := Value;
end;


end.
