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

unit AdvTreeView;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvCustomTreeView, AdvGraphics,
  AdvTreeViewData, AdvTypes, AdvGraphicsTypes
  {$IFDEF FNCLIB}
  , AdvControlPicker
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Types
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Layouts
  {$ENDIF}
  ;

type
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  {$IFDEF FNCLIB}
  TAdvTreeView = class(TAdvTreeViewPublished, IAdvControlPickerFull)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvTreeView = class(TAdvTreeViewPublished)
  {$ENDIF}
  protected
    procedure RegisterRuntimeClasses; override;
    {$IFDEF FMXLIB}
    procedure DoAbsoluteChanged; override;
    {$ENDIF}
    procedure DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode); override;
    procedure DrawBorders(AGraphics: TAdvGraphics); override;
    procedure DrawEmptySpaces(AGraphics: TAdvGraphics); override;
    procedure DrawNodeColumns(AGraphics: TAdvGraphics); override;

    procedure DrawNode(AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False; AOffsetX: Single = 0; AOffsetY: Single = 0); override;
    procedure DrawGroup(AGraphics: TAdvGraphics; ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind); override;
    procedure DrawColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind); override;
    procedure DrawSortIndicator(AGraphics: TAdvGraphics; ARect: TRectF; AColor: TAdvGraphicsColor; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind); virtual;
    {$IFDEF FNCLIB}
    function PickerGetContent: String; virtual;
    procedure PickerSelectItem(AItemIndex: Integer); virtual;
    function PickerGetSelectedItem: Integer; virtual;
    function PickerGetVisibleItemCount: Integer; virtual;
    function PickerGetItemCount: Integer; virtual;
    function PickerGetItemHeight: Single; virtual;
    procedure PickerSetItemHeight(AValue: Single); virtual;
    function PickerGetItemWidth: Single; virtual;
    procedure PickerSetItemWidth(AValue: Single); virtual;
    function PickerGetNextSelectableItem(AItemIndex: Integer): Integer; virtual;
    function PickerGetPreviousSelectableItem(AItemIndex: Integer): Integer; virtual;
    function PickerGetFirstSelectableItem: Integer; virtual;
    function PickerGetLastSelectableItem: Integer; virtual;
    procedure PickerResetFilter; virtual;
    procedure PickerApplyFilter(ACondition: string; ACaseSensitive: Boolean); virtual;
    function PickerLookupItem(ALookupString: String; ACaseSensitive: Boolean): TAdvControlPickerFilterItem; virtual;
    {$ENDIF}
  end;

implementation

uses
  Math, SysUtils, AdvUtils, Graphics
  {$IFDEF FNCLIB}
  , AdvCustomSelectorEx
  {$ENDIF};

type
  TAdvTreeViewColumnOpen = class(TAdvTreeViewColumn);

{ TAdvTreeView }

{$IFDEF FMXLIB}
procedure TAdvTreeView.DoAbsoluteChanged;
begin
  inherited;
  if Parent is TScaledLayout then
    UpdateTreeViewCache;
end;
{$ENDIF}

procedure TAdvTreeView.DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode);
begin
  inherited;
  {$IFDEF FNCLIB}
  if Assigned(Parent) and (Parent is TAdvCustomSelectorEx) and Assigned(Parent.Owner) and (Parent.Owner is TAdvControlPicker)then
  begin
//    Update ControlPicker if assigned as control
    (Parent.Owner as TAdvControlPicker).UpdateDropDown;
  end;
  {$ENDIF}
end;

procedure TAdvTreeView.DrawBorders(AGraphics: TAdvGraphics);
var
  rrt, rrb, trl, grt, grb: TRectF;
  nr: TRectF;
begin
  inherited;

  grt := GetGroupsTopRect;
  grb := GetGroupsBottomRect;
  rrt := GetColumnsTopRect;
  rrb := GetColumnsBottomRect;
  nr := GetContentClipRect;

  {$IFDEF CMNLIB}
  grt.Right := grt.Right - 1;
  grb.Right := grb.Right - 1;
  rrt.Right := rrt.Right - 1;
  rrb.Right := rrb.Right - 1;
  {$ENDIF}

  trl := RectF(nr.Right + 1, rrt.Bottom, nr.Right + 1, rrb.Top);

  AGraphics.Stroke.Assign(ColumnsAppearance.TopStroke);

  if (trl.Right - trl.Left > 0) and (trl.Bottom - trl.Top > 0) then
  begin
    if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
      AGraphics.DrawLine(PointF(trl.Right, trl.Top), PointF(trl.Right, trl.Bottom), gcpmLeftDown, gcpmLeftUp)
    else
      AGraphics.DrawLine(PointF(trl.Right, trl.Top), PointF(trl.Right, trl.Bottom), gcpmRightDown, gcpmRightUp);
  end;

  if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
  begin
    AGraphics.Stroke.Assign(ColumnsAppearance.TopStroke);
    if (tclTop in ColumnsAppearance.Layouts) and (ColumnsAppearance.TopSize > 0) then
    begin
      AGraphics.DrawLine(PointF(rrt.Left, rrt.Top), PointF(rrt.Left, rrt.Bottom), gcpmRightDown, gcpmRightUp);
      AGraphics.DrawLine(PointF(rrt.Right, rrt.Top), PointF(rrt.Right, rrt.Bottom), gcpmLeftDown, gcpmLeftUp);
    end
    else if not ((tglTop in GroupsAppearance.Layouts) or (GroupsAppearance.TopSize <= 0) or (DisplayGroups.Count = 0)) and (Stroke.Kind <> gskNone) then
      AGraphics.DrawLine(PointF(rrt.Left, rrt.Bottom), PointF(rrt.Right, rrt.Bottom), gcpmRightDown, gcpmLeftDown);

    AGraphics.Stroke.Assign(ColumnsAppearance.BottomStroke);
    if (tclBottom in ColumnsAppearance.Layouts) and (ColumnsAppearance.Bottomsize > 0) then
    begin
      AGraphics.DrawLine(PointF(rrb.Left, rrb.Top), PointF(rrb.Left, rrb.Bottom), gcpmRightDown, gcpmRightUp);
      AGraphics.DrawLine(PointF(rrb.Right, rrb.Top), PointF(rrb.Right, rrb.Bottom), gcpmLeftDown, gcpmLeftUp);
    end
    else if (not (tglBottom in GroupsAppearance.Layouts) or (GroupsAppearance.BottomSize <= 0) or (DisplayGroups.Count = 0)) and (Stroke.Kind <> gskNone) then
      AGraphics.DrawLine(PointF(rrb.Left, rrb.Top), PointF(rrb.Right, rrb.Top), gcpmRightUp, gcpmLeftUp);

    AGraphics.Stroke.Assign(GroupsAppearance.TopStroke);
    if (tglTop in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (GroupsAppearance.TopSize > 0) then
    begin
      AGraphics.DrawLine(PointF(grt.Left, grt.Top), PointF(grt.Left, grt.Bottom), gcpmRightDown, gcpmRightUp);
      AGraphics.DrawLine(PointF(grt.Right, grt.Top), PointF(grt.Right, grt.Bottom), gcpmLeftDown, gcpmLeftUp);
    end;

    if (not (tclTop in ColumnsAppearance.Layouts) or (ColumnsAppearance.TopSize <= 0)) and (Stroke.Kind <> gskNone) then
      AGraphics.DrawLine(PointF(grt.Left, grt.Bottom), PointF(grt.Right, grt.Bottom), gcpmRightDown, gcpmLeftDown);

    AGraphics.Stroke.Assign(GroupsAppearance.BottomStroke);
    if (tglBottom in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (GroupsAppearance.BottomSize > 0) then
    begin
      AGraphics.DrawLine(PointF(grb.Left, grb.Top), PointF(grb.Left, grb.Bottom), gcpmRightDown, gcpmRightUp);
      AGraphics.DrawLine(PointF(grb.Right, grb.Top), PointF(grb.Right, grb.Bottom), gcpmLeftDown, gcpmLeftUp);
    end;

    if (not (tclBottom in ColumnsAppearance.Layouts) or (ColumnsAppearance.BottomSize <= 0)) and (Stroke.Kind <> gskNone) then
      AGraphics.DrawLine(PointF(grb.Left, grb.Top), PointF(grb.Right, grb.Top), gcpmRightUp, gcpmLeftUp);
  end;
end;

procedure TAdvTreeView.DrawColumn(AGraphics: TAdvGraphics; ARect: TRectF;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
var
  b: Boolean;
  str: String;
  df: Boolean;
  txtr: TRectF;
  def: Boolean;
  col: TAdvTreeViewColumn;
  trim: TAdvGraphicsTextTrimming;
  ha, va: TAdvGraphicsTextAlign;
  ww: Boolean;
  r: TRectF;
  sr, dr: TRectF;
  szd: Single;
  szr: Single;
  szt: Single;
begin
  inherited;
  r := ARect;
  def := True;
  col := nil;
  if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
  begin
    col := Columns[AColumn];
    if not col.UseDefaultAppearance then
    begin
      def := False;
      case AKind of
        ikColumnTop:
        begin
          AGraphics.Stroke.Assign(col.TopStroke);
          AGraphics.Fill.Assign(col.TopFill);
        end;
        ikColumnBottom:
        begin
          AGraphics.Stroke.Assign(col.BottomStroke);
          AGraphics.Fill.Assign(col.BottomFill);
        end;
      end;
    end;
  end;

  if def then
  begin
    case AKind of
      ikColumnTop:
      begin
        AGraphics.Stroke.Assign(ColumnsAppearance.TopStroke);
        AGraphics.Fill.Assign(ColumnsAppearance.TopFill);
      end;
      ikColumnBottom:
      begin
        AGraphics.Stroke.Assign(ColumnsAppearance.BottomStroke);
        AGraphics.Fill.Assign(ColumnsAppearance.BottomFill);
      end;
    end;
  end;

  b := True;
  df := True;
  DoBeforeDrawColumnHeader(AGraphics, r, AColumn, AKind, b, df);

  if b then
  begin
    if df then
      AGraphics.DrawRectangle(r, gcrmNone);

    sr := r;
    szt := 0;

    if Assigned(col) then
    begin
      if col.Filtering.Enabled then
      begin
        szd := col.Filtering.ButtonSize;
        dr := RectF(Round(r.Right - szd - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2), Round(r.Right - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2 + szd));
        AGraphics.DrawDropDownButton(dr, False, False, True, True, AdaptToStyle, PaintScaleFactor);
        sr.Right := dr.Left;
        szt := szt + szd + ScalePaintValue(4);
      end;

      if col.Expandable then
      begin
        szr := col.ExpandingButtonSize;
        sr := RectF(Round(sr.Right - szr - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szr) / 2), Round(sr.Right - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szr) / 2 + szr));
        if col.Expanded then
          AGraphics.DrawCompactButton(sr, gcsExpanded, False, False, True, False, PaintScaleFactor)
        else
          AGraphics.DrawCompactButton(sr, gcsCollapsed, False, False, True, False, PaintScaleFactor);
        sr.Right := sr.Left;
        szt := szt + szr + ScalePaintValue(6);
      end;

      if (TAdvTreeViewColumnOpen(col).SortKind <> nskNone) and (SortColumn = AColumn) then
      begin
        szr := ScalePaintValue(8);
        sr := RectF(Round(sr.Right - szr - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szr) / 2), Round(sr.Right - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szr) / 2 + szr));
        DrawSortIndicator(AGraphics, sr, ColumnsAppearance.SortIndicatorColor, AColumn, TAdvTreeViewColumnOpen(col).SortIndex, TAdvTreeViewColumnOpen(col).SortKind);
        szt := szt + szr + ScalePaintValue(6);
      end;
    end;

    if def then
    begin
      case AKind of
        ikColumnTop: AGraphics.Font.Assign(ColumnsAppearance.TopFont);
        ikColumnBottom: AGraphics.Font.Assign(ColumnsAppearance.BottomFont);
      end;
    end
    else if Assigned(col) then
    begin
      case AKind of
        ikColumnTop: AGraphics.Font.Assign(col.TopFont);
        ikColumnBottom: AGraphics.Font.Assign(col.BottomFont);
      end;
    end;

    str := GetColumnText(AColumn);
    DoGetColumnText(AColumn, AKind, str);

    ha := gtaLeading;
    va := gtaCenter;
    ww := False;
    trim := gttNone;
    if Assigned(col) then
    begin
      ha := col.HorizontalTextAlign;
      va := col.VerticalTextAlign;
      ww := col.WordWrapping;
      trim := col.Trimming;
    end;

    DoGetColumnTrimming(AColumn, AKind, trim);
    DoGetColumnWordWrapping(AColumn, AKind, ww);
    DoGetColumnHorizontalTextAlign(AColumn, AKind, ha);
    DoGetColumnVerticalTextAlign(AColumn, AKind, va);

    b := True;
    txtr := r;
    InflateRectEx(txtr, ScalePaintValue(-3), ScalePaintValue(-3));
    txtr.Right := Max(txtr.Left, txtr.Right - szt);

    DoBeforeDrawColumnText(AGraphics, txtr, AColumn, AKind, str, b);
    if b then
    begin
      case AKind of
        ikColumnTop:
        begin
          if ColumnsAppearance.TopVerticalText then
            AGraphics.DrawText(txtr, str, ww, ha, va, trim, -90)
          else
            AGraphics.DrawText(txtr, str, ww, ha, va, trim)
        end;
        ikColumnBottom:
        begin
          if ColumnsAppearance.BottomVerticalText then
            AGraphics.DrawText(txtr, str, ww, ha, va, trim, 90)
          else
            AGraphics.DrawText(txtr, str, ww, ha, va, trim)
        end;
      end;
      DoAfterDrawColumnText(AGraphics, txtr, AColumn, AKind, str);
    end;
    DoAfterDrawColumnHeader(AGraphics, ARect, AColumn, AKind);
  end;
end;

procedure TAdvTreeView.DrawEmptySpaces(AGraphics: TAdvGraphics);
var
  r: TRectF;
  b, df: Boolean;
begin
  inherited;
  if ColumnsAppearance.FillEmptySpaces and not StretchScrollBars then
  begin
    if (tclTop in ColumnsAppearance.Layouts) and (ColumnsAppearance.TopSize > 0) then
    begin
      //Column top right
      r := GetColumnTopRightEmptyRect;
      {$IFDEF CMNLIB}
      r.Left := r.Left - 1;
      r.Right := r.Right + 1;
      r.Bottom := r.Bottom + 1;
      {$ENDIF}
      b := True;
      df := True;

      AGraphics.Fill.Assign(ColumnsAppearance.TopFill);
      AGraphics.Stroke.Assign(ColumnsAppearance.TopStroke);

      DoBeforeDrawColumnEmptySpace(AGraphics, r, tcesTopRight, b, df);
      if b then
      begin
        if df then
          AGraphics.DrawRectangle(r, gcrmShiftDownAndExpandWidth);

        DoAfterDrawColumnEmptySpace(AGraphics, r, tcesTopRight);
      end;
    end;

    if (tclBottom in ColumnsAppearance.Layouts) and (ColumnsAppearance.BottomSize > 0) then
    begin
      //Column bottom right
      r := GetColumnBottomRightEmptyRect;
      {$IFDEF CMNLIB}
      r.Left := r.Left - 1;
      r.Right := r.Right + 1;
      r.Top := r.Top - 1;
      {$ENDIF}
      b := True;
      df := True;

      AGraphics.Fill.Assign(ColumnsAppearance.BottomFill);
      AGraphics.Stroke.Assign(ColumnsAppearance.BottomStroke);

      DoBeforeDrawColumnEmptySpace(AGraphics, r, tcesBottomRight, b, df);
      if b then
      begin
        if df then
          AGraphics.DrawRectangle(r, gcrmShiftUpAndExpandWidth);

        DoAfterDrawColumnEmptySpace(AGraphics, r, tcesBottomRight);
      end;
    end;
  end;

  if GroupsAppearance.FillEmptySpaces and not StretchScrollBars then
  begin
    if (tglTop in GroupsAppearance.Layouts) and (GroupsAppearance.TopSize > 0) then
    begin
      //Group top right
      r := GetGroupTopRightEmptyRect;
      {$IFDEF CMNLIB}
      r.Left := r.Left - 1;
      r.Bottom := r.Bottom + 1;
      {$ENDIF}
      b := True;
      df := True;

      AGraphics.Fill.Assign(GroupsAppearance.TopFill);
      AGraphics.Stroke.Assign(GroupsAppearance.TopStroke);

      DoBeforeDrawGroupEmptySpace(AGraphics, r, tgesTopRight, b, df);
      if b then
      begin
        if df then
          AGraphics.DrawRectangle(r, gcrmShiftLeftDown);

        DoAfterDrawGroupEmptySpace(AGraphics, r, tgesTopRight);
      end;
    end;

    if (tglBottom in GroupsAppearance.Layouts) and (GroupsAppearance.BottomSize > 0) then
    begin
      //Group bottom right
      r := GetGroupBottomRightEmptyRect;
      {$IFDEF CMNLIB}
      r.Left := r.Left - 1;
      r.Right := r.Right + 1;
      r.Top := r.Top - 1;
      {$ENDIF}
      b := True;
      df := True;

      AGraphics.Fill.Assign(GroupsAppearance.BottomFill);
      AGraphics.Stroke.Assign(GroupsAppearance.BottomStroke);

      DoBeforeDrawGroupEmptySpace(AGraphics, r, tgesBottomRight, b, df);
      if b then
      begin
        if df then
          AGraphics.DrawRectangle(r, gcrmShiftLeftUp);

        DoAfterDrawGroupEmptySpace(AGraphics, r, tgesBottomRight);
      end;
    end;
  end;
end;

procedure TAdvTreeView.DrawGroup(AGraphics: TAdvGraphics; ARect: TRectF; AGroup,
  AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
var
  b, df: Boolean;
  str: String;
  txtr: TRectF;
  grp: TAdvTreeViewGroup;
  def: Boolean;
begin
  grp := nil;
  def := True;
  if (AGroup >= 0) and (AGroup <= Groups.Count - 1) then
  begin
    grp := Groups[AGroup];
    if not grp.UseDefaultAppearance then
    begin
      def := False;
      case AKind of
        ikGroupTop:
        begin
          AGraphics.Stroke.Assign(grp.TopStroke);
          AGraphics.Fill.Assign(grp.TopFill);
        end;
        ikGroupBottom:
        begin
          AGraphics.Stroke.Assign(grp.BottomStroke);
          AGraphics.Fill.Assign(grp.BottomFill);
        end;
      end;
    end;
  end;

  if def then
  begin
    case AKind of
      ikGroupTop:
      begin
        AGraphics.Stroke.Assign(GroupsAppearance.TopStroke);
        AGraphics.Fill.Assign(GroupsAppearance.TopFill);
      end;
      ikGroupBottom:
      begin
        AGraphics.Stroke.Assign(GroupsAppearance.BottomStroke);
        AGraphics.Fill.Assign(GroupsAppearance.BottomFill);
      end;
    end;
  end;

  b := True;
  df := True;
  DoBeforeDrawGroup(AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind, b, df);

  if b then
  begin
    if df then
      AGraphics.DrawRectangle(ARect, gcrmNone);

    if def then
    begin
      case AKind of
        ikGroupTop: AGraphics.Font.Assign(GroupsAppearance.TopFont);
        ikGroupBottom: AGraphics.Font.Assign(GroupsAppearance.BottomFont);
      end;
    end
    else if Assigned(grp) then
    begin
      case AKind of
        ikGroupTop: AGraphics.Font.Assign(grp.TopFont);
        ikGroupBottom: AGraphics.Font.Assign(grp.BottomFont);
      end;
    end;

    txtr := ARect;
    InflateRectEx(txtr, ScalePaintValue(-2), ScalePaintValue(-2));

    str := GetGroupText(AGroup);
    DoGetGroupText(AGroup, AKind, str);
    b := True;
    DoBeforeDrawGroupText(AGraphics, txtr, AGroup, AStartColumn, AEndColumn, AKind, str, b);
    if b then
    begin
      case AKind of
        ikGroupTop:
        begin
          if GroupsAppearance.TopVerticalText then
            AGraphics.DrawText(txtr, str, False, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, gttNone, -90)
          else
            AGraphics.DrawText(txtr, str, False, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, gttNone)
        end;
        ikGroupBottom:
        begin
          if GroupsAppearance.BottomVerticalText then
            AGraphics.DrawText(txtr, str, False, GroupsAppearance.BottomHorizontalTextAlign, GroupsAppearance.BottomVerticalTextAlign, gttNone, 90)
          else
            AGraphics.DrawText(txtr, str, False, GroupsAppearance.BottomHorizontalTextAlign, GroupsAppearance.BottomVerticalTextAlign, gttNone)
        end;
      end;
      DoAfterDrawGroupText(AGraphics, txtr, AGroup, AStartColumn, AEndColumn, AKind, str);
    end;
    DoAfterDrawGroup(AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind);
  end;
end;

procedure TAdvTreeView.DrawNode(AGraphics: TAdvGraphics; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False; AOffsetX: Single = 0; AOffsetY: Single = 0);
var
  str: String;
  b, df: Boolean;
  txtr, titr: TRectF;
  I: Integer;
  bmp: TAdvBitmap;
  bmpnr, extr: TRectF;
  AColor: TAdvGraphicsColor;
  AStrokeColor: TAdvGraphicsColor;
  sts, stp: Integer;
  bmpn: TAdvBitmap;
  ha, va: TAdvGraphicsTextAlign;
  c: TAdvTreeViewColumn;
  ww: Boolean;
  trim: TAdvGraphicsTextTrimming;
  en: Boolean;
  colw: Double;
  chk: TAdvTreeViewNodeCheckType;
  chkr: TRectF;
  chkbmp: TAdvBitmap;
  ck, ext: Boolean;
  lr, dr: TRectF;
  bmpexr, dexr: TRectF;
  sel: Boolean;
  cl: TAdvTreeViewColumn;
  rfoc: TRectF;
  p, lp, n: TAdvTreeViewVirtualNode;
  rsel, r: TRectF;
  ns: Double;
  dpi: Boolean;
  g: TAdvGraphics;
  bmpa: TBitmap;
  bt: TAdvBitmap;
  s: TAdvGraphicsSides;
  ro: Integer;
  corners: TAdvGraphicsCorners;
begin
  inherited;
  if not Assigned(ANode) or ((DragNode = ANode) and DragModeStarted) then
    Exit;

  dpi := ResourceScaleFactor > 1.0;

  en := True;
  DoIsNodeEnabled(ANode, en);
  ext := False;
  DoIsNodeExtended(ANode, ext);
  sel := IsVirtualNodeSelected(ANode);

  if en then
  begin
    if sel then
    begin
      if ext then
      begin
        AGraphics.Fill.Assign(NodesAppearance.ExtendedSelectedFill);
        AGraphics.Stroke.Assign(NodesAppearance.ExtendedSelectedStroke);
      end
      else
      begin
        AGraphics.Fill.Assign(NodesAppearance.SelectedFill);
        AGraphics.Stroke.Assign(NodesAppearance.SelectedStroke);
      end;

      AColor := AGraphics.Fill.Color;
      AStrokeColor := AGraphics.Stroke.Color;
      DoGetNodeSelectedColor(ANode, AColor);
      DoGetNodeSelectedStrokeColor(ANode, AStrokeColor);
      AGraphics.Fill.Color := AColor;
      AGraphics.Stroke.Color := AStrokeColor;
    end
    else
    begin
      if ext then
      begin
        AGraphics.Fill.Assign(NodesAppearance.ExtendedFill);
        AGraphics.Stroke.Assign(NodesAppearance.ExtendedStroke);
      end
      else
      begin
        AGraphics.Fill.Assign(NodesAppearance.Fill);
        AGraphics.Stroke.Assign(NodesAppearance.Stroke);
      end;

      AColor := AGraphics.Fill.Color;
      AStrokeColor := AGraphics.Stroke.Color;
      DoGetNodeColor(ANode, AColor);
      DoGetNodeStrokeColor(ANode, AStrokeColor);
      AGraphics.Fill.Color := AColor;
      AGraphics.Stroke.Color := AStrokeColor;
    end;
  end
  else
  begin
    if ext then
    begin
      AGraphics.Fill.Assign(NodesAppearance.ExtendedDisabledFill);
      AGraphics.Stroke.Assign(NodesAppearance.ExtendedDisabledStroke);
    end
    else
    begin
      AGraphics.Fill.Assign(NodesAppearance.DisabledFill);
      AGraphics.Stroke.Assign(NodesAppearance.DisabledStroke);
    end;

    AColor := AGraphics.Fill.Color;
    AStrokeColor := AGraphics.Stroke.Color;
    DoGetNodeDisabledColor(ANode, AColor);
    DoGetNodeDisabledStrokeColor(ANode, AStrokeColor);
    AGraphics.Fill.Color := AColor;
    AGraphics.Stroke.Color := AStrokeColor;
  end;

  df := True;
  b := True;
  DoBeforeDrawNode(AGraphics, ARect, ANode, b, df);

  if b then
  begin
    if df then
    begin
      rsel := ARect;
      if sel and not ext then
      begin
        case NodesAppearance.SelectionArea of
          tsaFull:
          begin
            if (Stroke.Kind <> gskNone) and (Stroke.Color <> gcNull) then
              rsel.Left := 1
            else
              rsel.Left := 0;
          end;
          tsaFromText:
          begin
            if Length(ANode.TextRects) > 0 then
              rsel.Left := ANode.TextRects[0].Left;
          end;
        end;
      end;

      s := AllSides;
      ro := 0;
      corners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
      DoGetNodeSides(ANode, s);
      DoGetNodeRounding(ANode, ro, corners);

      if ro > 0 then
      begin
        ro := Min( Trunc(Min(rsel.Right - rsel.Left, rsel.Bottom - rsel.Top)/2), ro);
        AGraphics.DrawRoundRectangle(rsel, ro, corners, gcrmNone)
      end
      else
      AGraphics.DrawRectangle(rsel, s, gcrmNone);

      if (ANode = FocusedVirtualNode) and Focused and NodesAppearance.ShowFocus then
      begin
        rfoc := rsel;
        InflateRectEx(rfoc, ScalePaintValue(-2), ScalePaintValue(-2));
        AGraphics.DrawFocusRectangle(rfoc);
      end;
    end;

    if not CompactMode then
    begin
      sts := GetFirstVisibleColumn;
      stp := GetLastVisibleColumn;
      if ext then
        stp := Min(sts, stp);

      for I := sts to stp do
      begin
        cl := nil;
        if (I >= 0) and (I <= Columns.Count - 1) then
          cl := Columns[I];

        if  (I - sts >= 0) and (I - sts <= Length(ANode.ExpandRects) - 1) and NodesAppearance.ShowLines and (I = NodesAppearance.ExpandColumn) and (NodesAppearance.ExpandWidth > 0) then
        begin
          r := ANode.ExpandRects[I - sts];
          OffsetRectEx(r, AOffsetX, AOffsetY);
          AGraphics.Stroke.Assign(NodesAppearance.LineStroke);

          if (NodesAppearance.LineStroke.Kind in [gskDash, gskDot, gskDashDot, gskDashDotDot]) then
            AGraphics.StartSpecialPen;

          ns := NodesAppearance.LevelIndent;
          lr := RectF(r.Left + NodesAppearance.ExpandWidth / 2, ARect.Top, r.Left + NodesAppearance.ExpandWidth / 2 + ns / 2, ARect.Bottom);

          AGraphics.DrawLine(PointF(lr.Left, lr.Top + (lr.Bottom - lr.Top) / 2), PointF(lr.Right, lr.Top + (lr.Bottom - lr.Top) / 2));

          if VisibleNodes.Count > 1 then
          begin
            n := ANode;
            p := n.GetParent;

            dr := lr;
            while Assigned(p) do
            begin
              if (((n = ANode) and (n.Index <= p.Children - 1)) or ((n <> ANode) and (n.Index < p.Children - 1))) then
              begin
                if n.Index < p.Children - 1 then
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Bottom), gcpmRightDown, gcpmRightUp)
                else
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Top + (dr.Bottom - dr.Top) / 2), gcpmRightDown, gcpmRightUp);
              end;

              n := p;
              p := n.GetParent;
              OffsetRectEx(dr, -ns, 0);
            end;

            if NodeStructure.Count - ANode.TotalChildren - 1 > 0 then
            begin
              p := ANode.GetParent;
              if not Assigned(p) then
              begin
                if (ANode.Row = NodeStructure.Count - ANode.TotalChildren - 1) then
                begin
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Top + (dr.Bottom - dr.Top) / 2), gcpmRightDown, gcpmRightUp)
                end
                else if ANode.Row = 0 then
                begin
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top + (dr.Bottom - dr.Top) / 2), PointF(dr.Left, Int(dr.Bottom) - 0.5), gcpmRightDown, gcpmRightUp)
                end
                else
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Bottom), gcpmRightDown, gcpmRightUp)
              end
              else if Assigned(p) then
              begin
                lp := p;
                p := p.GetParent;
                dr := lr;
                OffsetRectEx(dr, -ns, 0);
                while Assigned(p) do
                begin
                  lp := p;
                  p := p.GetParent;
                  OffsetRectEx(dr, -ns, 0);
                end;

                if Assigned(lp) and (lp.Row < NodeStructure.Count - lp.TotalChildren - 1)  then
                  AGraphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Bottom), gcpmRightDown, gcpmRightUp);
              end;
            end;
          end;

          if (NodesAppearance.LineStroke.Kind in [gskDash, gskDot, gskDashDot, gskDashDotDot]) then
            AGraphics.StopSpecialPen;
        end;

        if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
          AGraphics.Font.Assign(cl.Font)
        else
        begin
          if ext then
            AGraphics.Font.Assign(NodesAppearance.ExtendedFont)
          else
            AGraphics.Font.Assign(NodesAppearance.Font);
        end;

        colw := ColumnWidths[I];
        if (colw > 0) or ext then
        begin
          if en then
          begin
            if sel then
            begin
              if ext then
                AColor := NodesAppearance.ExtendedSelectedFontColor
              else
                AColor := NodesAppearance.SelectedFontColor;

              DoGetNodeSelectedTextColor(ANode, I, AColor);
              AGraphics.Font.Color := AColor;
            end
            else
            begin
              if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
                AColor := cl.Font.Color
              else
              begin
                if ext then
                  AColor := NodesAppearance.ExtendedFontColor
                else
                  AColor := NodesAppearance.Font.Color;
              end;

              DoGetNodeTextColor(ANode, I, AColor);
              AGraphics.Font.Color := AColor;
            end;
          end
          else
          begin
            if ext then
              AColor := NodesAppearance.ExtendedDisabledFontColor
            else
              AColor := NodesAppearance.DisabledFontColor;

            DoGetNodeDisabledTextColor(ANode, I, AColor);
            AGraphics.Font.Color := AColor;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.CheckRects) - 1) and (I - sts <= Length(ANode.CheckStates) - 1) then
          begin
            chk := tvntNone;
            DoGetNodeCheckType(ANode, I, chk);
            if chk <> tvntNone then
            begin
              chkbmp := nil;
              ck := ANode.CheckStates[I - sts];
              DoIsNodeChecked(ANode, I, ck);

              case chk of
                tvntCheckBox: chkbmp := TAdvBitmap(GetCheckBoxBitmap(ck, False));
                tvntRadioButton: chkbmp := TAdvBitmap(GetRadioButtonBitmap(ck, False));
              end;

              if Assigned(chkbmp) then
              begin
                chkr := ANode.CheckRects[I - sts];
                OffsetRectEx(chkr, AOffsetX, AOffsetY);

                b := True;
                DoBeforeDrawNodeCheck(AGraphics, chkr, I, ANode, chkbmp, b);
                if b then
                begin
                  AGraphics.DrawBitmap(chkr, chkbmp);
                  DoAfterDrawNodeCheck(AGraphics, chkr, I, ANode, chkbmp);
                end;
              end;
            end;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.BitmapRects) - 1) then
          begin
            bmpn := nil;
            DoGetNodeIcon(ANode, I, False, bmpn);
            if not Assigned(bmpn) and dpi then
              DoGetNodeIcon(ANode, I, True, bmpn);

            if Assigned(bmpn) and not IsBitmapEmpty(bmpn) then
            begin
              bmpnr := ANode.BitmapRects[I - sts];
              OffsetRectEx(bmpnr, AOffsetX, AOffsetY);

              b := True;
              DoBeforeDrawNodeIcon(AGraphics, bmpnr, I, ANode, bmpn, b);
              if b then
              begin
                AGraphics.DrawBitmap(bmpnr, bmpn);
                DoAfterDrawNodeIcon(AGraphics, bmpnr, I, ANode, bmpn);
              end;
            end;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.ExpandRects) - 1) and (ANode.Children > 0) and (I = NodesAppearance.ExpandColumn) and (NodesAppearance.ExpandWidth > 0) then
          begin
            if dpi then
            begin
              if ANode.Expanded then
                bmp := NodesAppearance.CollapseNodeIconLarge
              else
                bmp := NodesAppearance.ExpandNodeIconLarge;
            end
            else
            begin
              if ANode.Expanded then
                bmp := NodesAppearance.CollapseNodeIcon
              else
                bmp := NodesAppearance.ExpandNodeIcon;
            end;

            bmpa := nil;
            if AdaptToStyle then
            begin
              bmpa := TBitmap.Create;
              bmpa.SetSize(bmp.Width, bmp.Height);
              g := TAdvGraphics.Create(bmpa.Canvas);
              try
                g.BeginScene;
                g.Fill.Color := TAdvGraphics.DefaultSelectionFillColor;
                g.Fill.Kind := gfkSolid;
                g.Stroke.Kind := gskSolid;
                g.Stroke.Color := TAdvGraphics.DefaultTextFontColor;
                g.DrawRectangle(RectF(0, 0, bmp.Width, bmp.Height));
                g.Stroke.Width := ScalePaintValue(2);

                if not ANode.Expanded then
                  g.DrawLine(PointF(bmp.Width div 2, 0), PointF(bmp.Width div 2, bmp.Height), gcpmRightDown);

                g.DrawLine(PointF(0, bmp.Height div 2), PointF(bmp.Width, bmp.Height div 2), gcpmRightDown);
              finally
                g.EndScene;
                g.Free;
              end;
            end;

            bmpexr := ANode.ExpandRects[I - sts];
            OffsetRectEx(bmpexr, AOffsetX, AOffsetY);

            b := True;
            DoBeforeDrawNodeExpand(AGraphics, bmpexr, I, ANode, bmp, b);
            if b then
            begin
              if dpi then
              begin
                dexr := bmpexr;
                InflateRectEx(dexr, ScalePaintValue(-2), ScalePaintValue(-2));
              end
              else
              begin
                dexr := RectF(Int(bmpexr.Left + ((bmpexr.Right - bmpexr.Left) - bmp.Width) / 2), Int(bmpexr.Top + ((bmpexr.Bottom - bmpexr.Top) - bmp.Height) / 2),
                  Int(bmpexr.Left + ((bmpexr.Right - bmpexr.Left) - bmp.Width) / 2) + bmp.Width, Int(bmpexr.Top + ((bmpexr.Bottom - bmpexr.Top) - bmp.Height) / 2) + bmp.Height);
              end;

              if AdaptToStyle and Assigned(bmpa) then
              begin
                bt := TAdvBitmap.Create;
                try
                  bt.Assign(bmpa);
                  AGraphics.DrawBitmap(dexr, bt);
                finally
                  bt.Free;
                end;
                bmpa.Free;
                bmpa := nil;
              end
              else
              begin
                AGraphics.DrawBitmap(dexr, bmp);
              end;

              DoAfterDrawNodeExpand(AGraphics, bmpexr, I, ANode, bmp);
            end;

            if Assigned(bmpa) then
              bmpa.Free;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.TextRects) - 1) and ANode.TitleExpanded[I - sts] then
          begin
            txtr := ANode.TextRects[I - sts];
            OffsetRectEx(txtr, AOffsetX, AOffsetY);

            InflateRectEx(txtr, ScalePaintValue(-2), 0);

            c := nil;
            if (I >= 0) and (I <= Columns.Count - 1) then
              c := Columns[I];

            ha := gtaLeading;
            va := gtaCenter;
            ww := False;
            trim := gttNone;
            if Assigned(c) then
            begin
              ha := c.HorizontalTextAlign;
              va := c.VerticalTextAlign;
              ww := c.WordWrapping;
              trim := c.Trimming;
            end;

            DoGetNodeTrimming(ANode, I, trim);
            DoGetNodeWordWrapping(ANode, I, ww);
            DoGetNodeHorizontalTextAlign(ANode, I, ha);
            DoGetNodeVerticalTextAlign(ANode, I,va);

            str := '';
            DoGetNodeText(ANode, I, tntmDrawing, str);
            b := True;
            DoBeforeDrawNodeText(AGraphics, txtr, I, ANode, str, b);
            if b then
            begin
              if InplaceEditorActive and ((UpdateNodeColumn <> I) or (FocusedVirtualNode <> ANode)) or not InplaceEditorActive then
                AGraphics.DrawText(txtr, str, ww, ha, va, trim);

              DoAfterDrawNodeText(AGraphics, txtr, I, ANode, str);
            end;
          end;

          if Assigned(cl) and not cl.UseDefaultAppearance then
            AGraphics.Font.Assign(cl.TitleFont)
          else
            AGraphics.Font.Assign(NodesAppearance.TitleFont);

          if en then
          begin
            if sel then
            begin
              AColor := NodesAppearance.SelectedTitleFontColor;
              DoGetNodeSelectedTitleColor(ANode, I, AColor);
              AGraphics.Font.Color := AColor;
            end
            else
            begin
              if Assigned(cl) and not cl.UseDefaultAppearance then
                AColor := cl.TitleFont.Color
              else
                AColor := NodesAppearance.TitleFont.Color;

              DoGetNodeTitleColor(ANode, I, AColor);
              AGraphics.Font.Color := AColor;
            end;
          end
          else
          begin
            AColor := NodesAppearance.DisabledTitleFontColor;
            DoGetNodeDisabledTitleColor(ANode, I, AColor);
            AGraphics.Font.Color := AColor;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.TitleRects) - 1) then
          begin
            titr := ANode.TitleRects[I - sts];
            OffsetRectEx(titr, AOffsetX, AOffsetY);

            InflateRectEx(titr, ScalePaintValue(-2), 0);

            c := nil;
            if (I >= 0) and (I <= Columns.Count - 1) then
              c := Columns[I];

            ha := gtaLeading;
            va := gtaCenter;
            ww := False;
            trim := gttNone;
            if Assigned(c) then
            begin
              ha := c.TitleHorizontalTextAlign;
              va := c.TitleVerticalTextAlign;
              ww := c.TitleWordWrapping;
              trim := c.TitleTrimming;
            end;

            DoGetNodeTitleTrimming(ANode, I, trim);
            DoGetNodeTitleWordWrapping(ANode, I, ww);
            DoGetNodeTitleHorizontalTextAlign(ANode, I, ha);
            DoGetNodeTitleVerticalTextAlign(ANode, I,va);

            str := '';
            DoGetNodeTitle(ANode, I, tntmDrawing, str);
            b := True;
            DoBeforeDrawNodeTitle(AGraphics, titr, I, ANode, str, b);
            if b then
            begin
  //            if InplaceEditorActive and ((UpdateNodeColumn <> I) or (FocusedVirtualNode <> ANode)) or not InplaceEditorActive then
                AGraphics.DrawText(titr, str, ww, ha, va, trim);

              DoAfterDrawNodeTitle(AGraphics, titr, I, ANode, str);
            end;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.ExtraRects) - 1) then
          begin
            extr := ANode.ExtraRects[I - sts];
            OffsetRectEx(extr, AOffsetX, AOffsetY);
            b := True;
            DoBeforeDrawNodeExtra(AGraphics, extr, I, ANode, b);
            if b then
            begin
              DoDrawNodeExtra(AGraphics, extr, I, ANode);
              DoAfterDrawNodeExtra(AGraphics, extr, I, ANode);
            end;
          end;

          if (I - sts >= 0) and (I - sts <= Length(ANode.TitleExtraRects) - 1) then
          begin
            extr := ANode.TitleExtraRects[I - sts];
            OffsetRectEx(extr, AOffsetX, AOffsetY);
            b := True;
            DoBeforeDrawNodeTitleExtra(AGraphics, extr, I, ANode, b);
            if b then
            begin
              DoDrawNodeTitleExtra(AGraphics, extr, I, ANode);
              DoAfterDrawNodeTitleExtra(AGraphics, extr, I, ANode);
            end;
          end;
        end;
      end;
    end;

    DoAfterDrawNode(AGraphics, ARect, ANode);
  end;
end;

procedure TAdvTreeView.DrawNodeColumns(AGraphics: TAdvGraphics);
var
  b, df: Boolean;
  I: Integer;
  r: TRectF;
  x, w: Double;
  cr: TRectF;
  crcl: TRectF;
  hs: Double;
  c: TAdvTreeViewColumn;
begin
  inherited;
  cr := GetContentRect;
  crcl := GetContentClipRect;
  hs := GetHorizontalScrollPosition;
  for I := 1 to ColumnCount - 1 do
  begin
    if (I >= 0) and (I < Columns.Count - 1) then
    begin
      x := Int(ColumnPositions[I]) - Int(hs);
      w := Int(ColumnWidths[I]);

      if (ColumnStroke.Color <> gcNull) and (ColumnStroke.Kind <> gskNone) then
      begin
        AGraphics.Stroke.Assign(ColumnStroke);
        b := True;
        df := True;
        r := RectF(x, cr.Top, x + w, cr.Bottom);

        DoBeforeDrawColumn(AGraphics, r, I, b, df);

        if b then
        begin
          if df then
            AGraphics.DrawRectangle(r, gcrmShiftRightAndShrinkHeight);

          DoAfterDrawColumn(AGraphics, r, I);
        end;
      end;
    end;
  end;

  if NodeStructure.Count > 0 then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      if (I >= 0) and (I <= Columns.Count - 1) then
      begin
        c := Columns[I];
        x := Int(ColumnPositions[I]) - Int(hs);
        w := Int(ColumnWidths[I]);

        b := True;
        df := True;

        AGraphics.Fill.Kind := gfkNone;
        AGraphics.Stroke.Assign(NodesAppearance.ColumnStroke);
        if not c.UseDefaultAppearance then
        begin
          AGraphics.Stroke.Assign(c.Stroke);
          AGraphics.Fill.Assign(c.Fill);
        end;

        r := RectF(x, crcl.Top, x + w, crcl.Bottom);

        DoBeforeDrawNodeColumn(AGraphics, r, I, b, df);

        if b then
        begin
          if df then
            AGraphics.DrawRectangle(r, [gsTop, gsRight, gsBottom], gcrmShiftRightAndShrinkHeight);

          DoAfterDrawNodeColumn(AGraphics, r, I);
        end;
      end;
    end;
  end;
end;

procedure TAdvTreeView.DrawSortIndicator(AGraphics: TAdvGraphics; ARect: TRectF; AColor: TAdvGraphicsColor; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind);
var
  pth: TAdvGraphicsPath;
  vertt: TAdvGraphicsTextAlign;
  c: TAdvGraphicsColor;
  txtr: TRectF;
  b: Boolean;
begin
  inherited;
  c := AColor;
  AGraphics.Fill.Kind := gfkSolid;
  AGraphics.Fill.Color := c;
  AGraphics.Stroke.Kind := gskSolid;
  AGraphics.Stroke.Color := c;

  b := True;
  DoBeforeDrawSortIndicator(AGraphics, ARect, AColumn, ASortIndex, ASortKind, b);
  if b then
  begin
    vertt := gtaCenter;
    txtr := ARect;
    pth := TAdvGraphicsPath.Create;
    try
      case ASortKind of
        nskAscending:
        begin
          vertt := gtaTrailing;
          pth.MoveTo(PointF(ARect.Left + (ARect.Right - ARect.Left) / 2, ARect.Top));
          pth.LineTo(PointF(ARect.Right, ARect.Bottom));
          pth.LineTo(PointF(ARect.Left, ARect.Bottom));
          txtr := RectF(ARect.Left, ARect.Top + ScalePaintValue(2), ARect.Right, ARect.Bottom + ScalePaintValue(2));
        end;
        nskDescending:
        begin
          vertt := gtaLeading;
          pth.MoveTo(PointF(ARect.Left, ARect.Top));
          pth.LineTo(PointF(ARect.Right, ARect.Top));
          pth.LineTo(PointF(ARect.Left + (ARect.Right - ARect.Left) / 2, ARect.Bottom));
          txtr := RectF(ARect.Left, ARect.Top - ScalePaintValue(2), ARect.Right, ARect.Bottom - ScalePaintValue(2));
        end;
      end;
      pth.ClosePath;
      AGraphics.DrawPath(pth);

      if ASortIndex <> -1 then
      begin
        AGraphics.Font.Color := gcWhite;
        TAdvUtils.SetFontSize(AGraphics.Font, 9);
        AGraphics.DrawText(txtr, IntToStr(ASortIndex), False, gtaCenter, vertt);
      end;
    finally
      pth.Free;
    end;
    DoAfterDrawSortIndicator(AGraphics, ARect, AColumn, ASortIndex, ASortKind);
  end;
end;

{$IFDEF FNCLIB}
{ TAdvTreeView picker implementation }

procedure TAdvTreeView.PickerApplyFilter(ACondition: string; ACaseSensitive: Boolean);
var
  f: TAdvTreeViewFilterData;
begin
  f := Filter.Add;
  f.Condition := ACondition;
  f.CaseSensitive := ACaseSensitive;
  ApplyFilter;
end;

function TAdvTreeView.PickerGetContent: String;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := '';
  if SelectedVirtualNodeCount > 0 then
  begin
    n := SelectedVirtualNodes[0];
    Result := n.Text[0];
  end;
end;

function TAdvTreeView.PickerGetFirstSelectableItem: Integer;
begin
  Result := GetFirstVisibleVirtualNodeRow;
end;

function TAdvTreeView.PickerGetItemCount: Integer;
begin
  Result := GetTotalVirtualNodeCount;
end;

function TAdvTreeView.PickerGetItemHeight: Single;
begin
  Result := NodesAppearance.FixedHeight;
end;

function TAdvTreeView.PickerGetItemWidth: Single;
begin
  if Columns.Count > 0 then
    Result := Width / Columns.Count
  else
    Result := 0;
end;

function TAdvTreeView.PickerGetLastSelectableItem: Integer;
begin
  Result := GetLastVisibleVirtualNodeRow;
end;

function TAdvTreeView.PickerGetNextSelectableItem(AItemIndex: Integer): Integer;
var
  n, nf: TAdvTreeViewVirtualNode;
begin
  Result := -1;
  n := GetNodeFromNodeStructure(AItemIndex);
  if Assigned(n) then
  begin
    nf := GetNextFocusableNode(n);
    if Assigned(nf) then
      Result := nf.Row;
  end;
end;

function TAdvTreeView.PickerGetPreviousSelectableItem(AItemIndex: Integer): Integer;
var
  n, nf: TAdvTreeViewVirtualNode;
begin
  Result := -1;
  n := GetNodeFromNodeStructure(AItemIndex);
  if Assigned(n) then
  begin
    nf := GetPreviousFocusableNode(n);
    if Assigned(nf) then
      Result := nf.Row;
  end;
end;

function TAdvTreeView.PickerGetSelectedItem: Integer;
begin
  Result := SelectedVirtualNodeRow;
end;

function TAdvTreeView.PickerGetVisibleItemCount: Integer;
begin
  Result := GetVisibleNodeCount;
end;

function TAdvTreeView.PickerLookupItem(ALookupString: String; ACaseSensitive: Boolean): TAdvControlPickerFilterItem;
var
  vn: TAdvTreeViewVirtualNode;
begin
  Result.ItemIndex := -1;
  Result.ItemText := '';
  vn := LookupNode(ALookupString + '*', False, -1, ACaseSensitive, True, True);
  if Assigned(vn) then
  begin
    Result.ItemIndex := vn.Row;
    Result.ItemText := vn.Text[0];
  end;
end;

procedure TAdvTreeView.PickerResetFilter;
begin
  if Filter.Count > 0 then
    RemoveFilters;
end;

procedure TAdvTreeView.PickerSelectItem(AItemIndex: Integer);
var
  n: TAdvTreeViewVirtualNode;
begin
  n := GetNodeFromNodeStructure(AItemIndex);
  if Assigned(n) then
  begin
    SelectVirtualNode(n);
    ScrollToVirtualNode(n, True);
  end;
end;

procedure TAdvTreeView.PickerSetItemHeight(AValue: Single);
begin
  NodesAppearance.FixedHeight := AValue;
end;

procedure TAdvTreeView.PickerSetItemWidth(AValue: Single);
begin
  //
end;
{$ENDIF}

procedure TAdvTreeView.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClasses([TAdvTreeView, TAdvTreeViewColumn, TAdvTreeViewNode, TAdvTreeViewNodeValue]);
end;

end.
