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

unit AdvCustomTreeView;

{$I TMSDEFS.INC}

{$IFDEF FMXLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE FMXWEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvTreeViewData, AdvCustomComponent, AdvGraphics, AdvPopupEx, AdvImageEx,
  Types, AdvTypes, Controls, ExtCtrls, StdCtrls, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Generics.Collections
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,Contnrs, Web, WEBLIB.JSON
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.Edit, FMX.ListBox
  {$WARNINGS OFF}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 28}
  ,FMX.Memo.Types
  {$IFEND}
  {$HINTS ON}
  {$WARNINGS ON}
  {$ENDIF}
  {$IFDEF FNCLIB}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  ,JSON
  {$ELSE}
  ,fpjson
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

const
  TAdvTreeViewDocURL = 'https://download.tmssoftware.com/download/manuals/AdvTreeViewDevGuide.pdf';
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.
  CLP_FMT = '#TREEVIEW#';
  ENDOFLINE = #13#10;
  DRAGMARGIN = 10;

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed: Issue with reintroducing BeginUpdate with additional parameter,
  //            please use BeginUpdate in combination with ClearNodeList to clear all nodes
  // v1.1.0.0 : New: Clipboard support
  //          : New: Column sorting
  //          : New: Drag & Drop support
  //          : New: Reordering
  //          : New: Filtering
  //          : New: Keyboard Lookup
  //          : Fixed: Issue with font initialization
  // v1.1.0.1 : Fixed: Issue with copying and moving node data properties
  // v1.1.0.2 : Fixed: Issue with selecting all nodes when multi-select is false
  // v1.1.0.3 : Fixed: Access violation calling stopedit in OnExit of custom editor
  //          : Fixed: Issue loading complex tree files
  // v1.1.0.4 : Fixed: Issue assigning child node data
  // v1.1.0.5 : Fixed: Issue drag & drop interaction in combination with expand / collapse
  // v1.1.0.6 : Improved: Handling keyboard ALT, SHIFT and CTRL keys without navigating to selecting item
  // v1.1.0.7 : Fixed: Issue with displaying icons on high dpi systems
  // v1.1.0.8 : Fixed: Issue setting check state without rebuilding node list
  // v1.1.1.0 : New: FindNodeByRow and GetTotalNodeCount functions
  //          : Fixed: Issue with OnAfterSizeColumn not called
  // v1.1.1.1 : Fixed: Issue with HTML calculation and wordwrapping
  //          : Fixed: Issue with drawing node background color in VCL/LCL
  //          : Fixed: Issue with ScrollToNode in combination with variable node height
  // v1.1.1.2 : Improved: ShowFocus default true
  //          : Improved: SelectNode := nil clears selection
  //          : Improved: Double-click on node area to expand/collapse
  // v1.1.1.3 : Fixed: Issue with sizing invisible columns
  // v1.1.1.4 : Fixed: Issue with ScrollToNode hanging on collapsed nodes
  // v1.1.2.0 : New: FindNodeByTextAndColumn function
  // v1.1.3.0 : New: FindNodeByDBKey function
  // v1.1.3.1 : Fixed: Access violation when destroying editor
  // v1.1.3.2 : Fixed: Access violation during move node operation
  // v1.1.3.3 : Fixed: Issue panning and dblclick combination
  // v1.1.3.4 : Fixed: Issue sorting and sizing columns occurs simultaneously
  // v1.1.3.5 : Improved: Retrieve text of virtual node
  // v1.1.3.6 : Fixed: Issue with presetting combobox editor value
  // v1.1.3.7 : Fixed: Issue with missing assignment of DBKey property
  // v1.1.3.8 : Fixed: Issue with multi-select on mobile platforms
  // v1.1.3.9 : Fixed: Issue with node checking still active while Interaction.ReadOnly property is set.
  // v1.1.4.0 : New: OnNodeMouseEnter and OnNodeMouseLeave events
  // v1.1.4.1 : Fixed: Issue with filtering and expanding / collapsing nodes
  // v1.1.4.2 : Fixed: Issue with resizing treeview in combination with splitter in TMS WEB Core
  // v1.1.4.3 : Fixed: Issue with drawing transparent checkboxes
  // v1.1.4.4 : Fixed: Issue with closing inplace editor in TMS WEB Core
  // v1.1.4.5 : Fixed: Issue with editing under Android
  // v1.1.4.6 : Fixed: horizontal scrolling not working when there is no vertical scrolling
  // v1.1.4.7 : Improved: Name property of column reflecting at designtime
  // v1.1.4.8 : Fixed : Issue with state of expanded/collapse icons for nodes without children
  // v1.1.4.9 : Fixed : Issue with dblclicking and clicking on an item in Lazarus
  // v1.1.5.0 : Improved : OnColumnAnchorClick event added
  // v1.1.5.1 : Fixed : Issue with moving parent nodes inside children
  // v1.1.5.2 : Fixed : Issue with Anchor if font was changed
  // v1.1.5.3 : Fixed : Issue with default drag & drop events not being triggered
  // v1.1.5.4 : Fixed : Issue with OnIsNodeChecked missing AColumn parameter
  // v1.1.5.5 : Improved : Added AIncludeRows parameter to XYToColumn to include rows while detecting which column is located at X and Y coordinate
  // v1.1.5.6 : Fixed : Issue with Delete key triggering LookupNode when FLookupString is empty
  // v1.1.5.7 : Fixed : Issue with OnBeforeUnSelectNode & OnAfterUnSelectNode event not called
  // v1.1.5.8 : New : Interaction.MouseWheelDelta to configure mousewheel scrolling sensitivity
  //          : Fixed : Issue with OnMouseLeave & OnMouseEnter not triggered
  //          : Fixed : Issue with hanging scroll actions
  // v1.1.5.9 : Fixed : Issue in Delphi 11 with begin and end scene for CreateBitmapCanvas
  //          : Fixed : Issue in macOS destroying the inplace editor popup in certain conditions
  // v1.1.6.0 : New : OnFocusedNodeChanged event
  //          : New : + KEY expands one level, - KEY collapses one level, * KEY expands recursively, / KEY collapses recursively
  //          : New : OnGetNodeData event, Data* properties to link to virtual node structure
  // v1.1.6.1 : Improved : Interaction.Lookup.VisibleNodesOnly to lookup within visible nodes.
  // v1.1.6.2 : Improved : Less sensitive downward scrolling in combination with more options in TAdvTableView descendant
  // v1.1.6.3 : Fixed : Issue retrieving and inserting nodes based on row parameter
  //          : Fixed : Issue with dblclick in combination with modal dialogs consuming focus
  // v1.1.6.4 : Fixed : Issue with clearing/deleting nodes and focused node pointer
  // v1.1.7.0 : New : OnBeforeSelectAllNodes & OnAfterSelectAllNodes events
  // v1.1.7.1 : Fixed : Issue with LookupNode function not auto-expanding
  // v1.1.8.0 : New : OnBeforeSortNodes & OnAfterSortNodes events
  // v1.1.8.1 : Fixed : Issue with reordering nodes at root level
  // v1.1.8.2 : Improved : MoveNode no longer destroys the node
  //          : Fixed : Issue with special characters not rendering in HTML
  // v1.2.0.0 : New : GlobalFont interface implemented
  //          : New : Updated initial look
  //          : New : Load JSON Objects with ViewJSON
  //          : New : HTMLTemplate possible per column for node values
  // v1.2.0.1 : Fixed : Issue with keyboard selection when not multiselect
  // v1.2.0.2 : Fixed : Issue with inplace editors not being destroyed properly on iOS

  CACHEWIDTH = 1000;
  CACHEHEIGHT = 1000;
  {$IFDEF ANDROID}
  SCROLLINGDELAY = 40;
  {$ELSE}
  SCROLLINGDELAY = 0;
  {$ENDIF}

  {$IFDEF FMXLIB}
  TAdvTreeViewColorExtended = $FFF4F4F4;
  TAdvTreeViewColorSelection = $FF1BADF8;
  {$ELSE}
  TAdvTreeViewColorExtended = $F4F4F4;
  TAdvTreeViewColorSelection = $F8AD1B;
  {$ENDIF}

resourcestring
  sAdvTreeViewFilterAll = '(All)';

type
  TAdvCustomTreeView = class;

  {$IFDEF WEBLIB}
  TAdvTreeViewCache = class(TObjectList)
  private
    function GetItem(Index: Integer): TAdvTreeViewCacheItem;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewCacheItem);
  public
    property Items[Index: Integer]: TAdvTreeViewCacheItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTreeViewCache = class(TObjectList<TAdvTreeViewCacheItem>);
  {$ENDIF}
  TAdvTreeViewNodeCache = class(TAdvTreeViewCache);
  TAdvTreeViewColumnsCache = class(TAdvTreeViewCache);
  TAdvTreeViewColumnsTopCache = class(TAdvTreeViewColumnsCache);
  TAdvTreeViewColumnsBottomCache = class(TAdvTreeViewColumnsCache);
  TAdvTreeViewGroupsCache = class(TAdvTreeViewCache);
  TAdvTreeViewGroupsTopCache = class(TAdvTreeViewGroupsCache);
  TAdvTreeViewGroupsBottomCache = class(TAdvTreeViewGroupsCache);

  {$IFDEF WEBLIB}
  TAdvTreeViewDisplayList = class(TList)
  private
    function GetItem(Index: Integer): TAdvTreeViewCacheItem;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewCacheItem);
  public
    property Items[Index: Integer]: TAdvTreeViewCacheItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTreeViewDisplayList = class(TList<TAdvTreeViewCacheItem>);
  {$ENDIF}
  TAdvTreeViewNodeDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewColumnsDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewColumnsTopDisplayList = class(TAdvTreeViewColumnsDisplayList);
  TAdvTreeViewColumnsBottomDisplayList = class(TAdvTreeViewColumnsDisplayList);
  TAdvTreeViewGroupsDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewGroupsTopDisplayList = class(TAdvTreeViewGroupsDisplayList);
  TAdvTreeViewGroupsBottomDisplayList = class(TAdvTreeViewGroupsDisplayList);

  TAdvTreeViewColumnsLayout = (tclTop, tclBottom);
  TAdvTreeViewColumnsLayouts = set of TAdvTreeViewColumnsLayout;

  TAdvTreeViewGroupLayout = (tglTop, tglBottom);
  TAdvTreeViewGroupsLayouts = set of TAdvTreeViewGroupLayout;

  TAdvTreeViewNodeHeightMode = (tnhmFixed, tnhmVariable);

  TAdvTreeViewSelectionArea = (tsaDefault, tsaFull, tsaFromLevel, tsaFromText);

  TAdvTreeViewNodesAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FFont: TAdvGraphicsFont;
    FDisabledFill: TAdvGraphicsFill;
    FSelectedFill: TAdvGraphicsFill;
    FDisabledStroke: TAdvGraphicsStroke;
    FSelectedStroke: TAdvGraphicsStroke;
    FSelectionArea: TAdvTreeViewSelectionArea;
    FExpandColumn: Integer;
    FExpandWidth: Double;
    FExpandHeight: Double;
    FLevelIndent: Double;
    FHeightMode: TAdvTreeViewNodeHeightMode;
    FFixedHeight: Double;
    FVariableMinimumHeight: Double;
    FSelectedFontColor: TAdvGraphicsColor;
    FDisabledFontColor: TAdvGraphicsColor;
    FExtendedFontColor: TAdvGraphicsColor;
    FExtendedSelectedFontColor: TAdvGraphicsColor;
    FExtendedDisabledFontColor: TAdvGraphicsColor;
    FExtendedDisabledFill: TAdvGraphicsFill;
    FExtendedFont: TAdvGraphicsFont;
    FExtendedSelectedFill: TAdvGraphicsFill;
    FExtendedDisabledStroke: TAdvGraphicsStroke;
    FExtendedSelectedStroke: TAdvGraphicsStroke;
    FExtendedFill: TAdvGraphicsFill;
    FExtendedStroke: TAdvGraphicsStroke;
    FShowLines: Boolean;
    FExpandNodeIcon: TAdvBitmap;
    FCollapseNodeIcon: TAdvBitmap;
    FExpandNodeIconLarge: TAdvBitmap;
    FCollapseNodeIconLarge: TAdvBitmap;
    FColumnStroke: TAdvGraphicsStroke;
    FLineStroke: TAdvGraphicsStroke;
    FShowFocus: Boolean;
    FSpacing: Double;
    FDisabledTitleFontColor: TAdvGraphicsColor;
    FSelectedTitleFontColor: TAdvGraphicsColor;
    FTitleFont: TAdvGraphicsFont;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetDisabledStroke(const Value: TAdvGraphicsStroke);
    procedure SetSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetSelectionArea(const Value: TAdvTreeViewSelectionArea);
    procedure SetExtendedFill(const Value: TAdvGraphicsFill);
    procedure SetExtendedStroke(const Value: TAdvGraphicsStroke);
    procedure SetExtendedFont(const Value: TAdvGraphicsFont);
    procedure SetExtendedDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetExtendedDisabledStroke(const Value: TAdvGraphicsStroke);
    procedure SetExtendedSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetExtendedSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetExpandColumn(const Value: Integer);
    procedure SetExpandWidth(const Value: Double);
    procedure SetExpandHeight(const Value: Double);
    procedure SetLevelIndent(const Value: Double);
    procedure SetFixedHeight(const Value: Double);
    procedure SetVariableMinimumHeight(const Value: Double);
    procedure SetSelectedFontColor(const Value: TAdvGraphicsColor);
    procedure SetDisabledFontColor(const Value: TAdvGraphicsColor);
    procedure SetExtendedFontColor(const Value: TAdvGraphicsColor);
    procedure SetExtendedSelectedFontColor(const Value: TAdvGraphicsColor);
    procedure SetExtendedDisabledFontColor(const Value: TAdvGraphicsColor);
    procedure SetHeightMode(const Value: TAdvTreeViewNodeHeightMode);
    procedure SetShowLines(const Value: Boolean);
    procedure SetCollapseNodeIcon(const Value: TAdvBitmap);
    procedure SetExpandNodeIcon(const Value: TAdvBitmap);
    procedure SetCollapseNodeIconLarge(const Value: TAdvBitmap);
    procedure SetExpandNodeIconLarge(const Value: TAdvBitmap);
    procedure SetColumnStroke(const Value: TAdvGraphicsStroke);
    procedure SetLineStroke(const Value: TAdvGraphicsStroke);
    procedure SetShowFocus(const Value: Boolean);
    function IsSpacingStored: Boolean;
    procedure SetSpacing(const Value: Double);
    procedure SetDisabledTitleFontColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedTitleFontColor(const Value: TAdvGraphicsColor);
    procedure SetTitleFont(const Value: TAdvGraphicsFont);
  protected
    procedure Changed(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ExpandColumn: Integer read FExpandColumn write SetExpandColumn default 0;
    property ExpandWidth: Double read FExpandWidth write SetExpandWidth;
    property ExpandHeight: Double read FExpandHeight write SetExpandHeight;
    property LevelIndent: Double read FLevelIndent write SetLevelIndent;
    property Spacing: Double read FSpacing write SetSpacing stored IsSpacingStored nodefault;
    property FixedHeight: Double read FFixedHeight write SetFixedHeight;
    property VariableMinimumHeight: Double read FVariableMinimumHeight write SetVariableMinimumHeight;
    property HeightMode: TAdvTreeViewNodeHeightMode read FHeightMode write SetHeightMode default tnhmFixed;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;

    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property ColumnStroke: TAdvGraphicsStroke read FColumnStroke write SetColumnStroke;
    property LineStroke: TAdvGraphicsStroke read FLineStroke write SetLineStroke;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property TitleFont: TAdvGraphicsFont read FTitleFont write SetTitleFont;
    property SelectedFontColor: TAdvGraphicsColor read FSelectedFontColor write SetSelectedFontColor default gcWhite;
    property DisabledFontColor: TAdvGraphicsColor read FDisabledFontColor write SetDisabledFontColor default gcSilver;
    property SelectedTitleFontColor: TAdvGraphicsColor read FSelectedTitleFontColor write SetSelectedTitleFontColor default gcWhite;
    property DisabledTitleFontColor: TAdvGraphicsColor read FDisabledTitleFontColor write SetDisabledTitleFontColor default gcSilver;

    property ExtendedFontColor: TAdvGraphicsColor read FExtendedFontColor write SetExtendedFontColor default gcBlack;
    property ExtendedSelectedFontColor: TAdvGraphicsColor read FExtendedSelectedFontColor write SetExtendedSelectedFontColor default gcWhite;
    property ExtendedDisabledFontColor: TAdvGraphicsColor read FExtendedDisabledFontColor write SetExtendedDisabledFontColor default gcSilver;

    property SelectedFill: TAdvGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TAdvGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property SelectionArea: TAdvTreeViewSelectionArea read FSelectionArea write SetSelectionArea default tsaFromText;

    property DisabledFill: TAdvGraphicsFill read FDisabledFill write SetDisabledFill;
    property DisabledStroke: TAdvGraphicsStroke read FDisabledStroke write SetDisabledStroke;

    property ExtendedFill: TAdvGraphicsFill read FExtendedFill write SetExtendedFill;
    property ExtendedStroke: TAdvGraphicsStroke read FExtendedStroke write SetExtendedStroke;
    property ExtendedFont: TAdvGraphicsFont read FExtendedFont write SetExtendedFont;

    property ExtendedSelectedFill: TAdvGraphicsFill read FExtendedSelectedFill write SetExtendedSelectedFill;
    property ExtendedSelectedStroke: TAdvGraphicsStroke read FExtendedSelectedStroke write SetExtendedSelectedStroke;

    property ExtendedDisabledFill: TAdvGraphicsFill read FExtendedDisabledFill write SetExtendedDisabledFill;
    property ExtendedDisabledStroke: TAdvGraphicsStroke read FExtendedDisabledStroke write SetExtendedDisabledStroke;

    property ExpandNodeIcon: TAdvBitmap read FExpandNodeIcon write SetExpandNodeIcon;
    property CollapseNodeIcon: TAdvBitmap read FCollapseNodeIcon write SetCollapseNodeIcon;
    property ExpandNodeIconLarge: TAdvBitmap read FExpandNodeIconLarge write SetExpandNodeIconLarge;
    property CollapseNodeIconLarge: TAdvBitmap read FCollapseNodeIconLarge write SetCollapseNodeIconLarge;
  end;

  TAdvTreeViewColumnsAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FLayouts: TAdvTreeViewColumnsLayouts;
    FStretch: Boolean;
    FStretchColumn: Integer;
    FStretchAll: Boolean;
    FBottomSize: Double;
    FTopSize: Double;
    FBottomFill: TAdvGraphicsFill;
    FBottomStroke: TAdvGraphicsStroke;
    FTopFill: TAdvGraphicsFill;
    FTopStroke: TAdvGraphicsStroke;
    FTopFont: TAdvGraphicsFont;
    FBottomFont: TAdvGraphicsFont;
    FTopVerticalTextAlign: TAdvGraphicsTextAlign;
    FTopHorizontalTextAlign: TAdvGraphicsTextAlign;
    FBottomHorizontalTextAlign: TAdvGraphicsTextAlign;
    FBottomVerticalTextAlign: TAdvGraphicsTextAlign;
    FTopVerticalText: Boolean;
    FBottomVerticalText: Boolean;
    FFillEmptySpaces: Boolean;
    FSortIndicatorColor: TAdvGraphicsColor;
    FOptimizedColumnDisplay: Boolean;
    procedure SetLayouts(const Value: TAdvTreeViewColumnsLayouts);
    procedure SetStretch(const Value: Boolean);
    procedure SetStretchAll(const Value: Boolean);
    procedure SetStretchColumn(const Value: Integer);
    procedure SetBottomSize(const Value: Double);
    procedure SetTopSize(const Value: Double);
    procedure SetBottomFill(const Value: TAdvGraphicsFill);
    procedure SetBottomStroke(const Value: TAdvGraphicsStroke);
    procedure SetTopFill(const Value: TAdvGraphicsFill);
    procedure SetTopStroke(const Value: TAdvGraphicsStroke);
    procedure SetBottomFont(const Value: TAdvGraphicsFont);
    procedure SetTopFont(const Value: TAdvGraphicsFont);
    procedure SetTopVerticalText(const Value: Boolean);
    procedure SetBottomVerticalText(const Value: Boolean);
    procedure SetFillEmptySpaces(const Value: Boolean);
    procedure SetSortIndicatorColor(const Value: TAdvGraphicsColor);
    procedure SetOptimizedColumnDisplay(const Value: Boolean);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Layouts: TAdvTreeViewColumnsLayouts read FLayouts write SetLayouts default [tclTop];
    property Stretch: Boolean read FStretch write SetStretch default True;
    property StretchColumn: Integer read FStretchColumn write SetStretchColumn default -1;
    property StretchAll: Boolean read FStretchAll write SetStretchAll default True;
    property TopSize: Double read FTopSize write SetTopSize;
    property BottomSize: Double read FBottomSize write SetBottomSize;
    property TopFont: TAdvGraphicsFont read FTopFont write SetTopFont;
    property BottomFont: TAdvGraphicsFont read FBottomFont write SetBottomFont;
    property TopFill: TAdvGraphicsFill read FTopFill write SetTopFill;
    property BottomFill: TAdvGraphicsFill read FBottomFill write SetBottomFill;
    property TopStroke: TAdvGraphicsStroke read FTopStroke write SetTopStroke;
    property BottomStroke: TAdvGraphicsStroke read FBottomStroke write SetBottomStroke;
    property TopVerticalText: Boolean read FTopVerticalText write SetTopVerticalText default False;
    property BottomVerticalText: Boolean read FBottomVerticalText write SetBottomVerticalText default False;
    property FillEmptySpaces: Boolean read FFillEmptySpaces write SetFillEmptySpaces default True;
    property SortIndicatorColor: TAdvGraphicsColor read FSortIndicatorColor write SetSortIndicatorColor default gcSteelblue;
    property OptimizedColumnDisplay: Boolean read FOptimizedColumnDisplay write SetOptimizedColumnDisplay default True;
  end;

  TAdvTreeViewColumnEmptySpace = (tcesTopLeft, tcesTopRight, tcesBottomLeft, tcesBottomRight);
  TAdvTreeViewGroupEmptySpace = (tgesTopLeft, tgesTopRight, tgesBottomLeft, tgesBottomRight);

  TAdvTreeViewGroupsAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FLayouts: TAdvTreeViewGroupsLayouts;
    FBottomSize: Double;
    FTopSize: Double;
    FBottomFill: TAdvGraphicsFill;
    FBottomStroke: TAdvGraphicsStroke;
    FTopFill: TAdvGraphicsFill;
    FTopStroke: TAdvGraphicsStroke;
    FTopFont: TAdvGraphicsFont;
    FBottomFont: TAdvGraphicsFont;
    FBottomHorizontalTextAlign: TAdvGraphicsTextAlign;
    FTopVerticalTextAlign: TAdvGraphicsTextAlign;
    FTopHorizontalTextAlign: TAdvGraphicsTextAlign;
    FBottomVerticalTextAlign: TAdvGraphicsTextAlign;
    FTopVerticalText: Boolean;
    FBottomVerticalText: Boolean;
    FFillEmptySpaces: Boolean;
    procedure SetLayouts(const Value: TAdvTreeViewGroupsLayouts);
    procedure SetBottomSize(const Value: Double);
    procedure SetTopSize(const Value: Double);
    procedure SetBottomFill(const Value: TAdvGraphicsFill);
    procedure SetBottomStroke(const Value: TAdvGraphicsStroke);
    procedure SetTopFill(const Value: TAdvGraphicsFill);
    procedure SetTopStroke(const Value: TAdvGraphicsStroke);
    procedure SetBottomFont(const Value: TAdvGraphicsFont);
    procedure SetTopFont(const Value: TAdvGraphicsFont);
    procedure SetBottomHorizontalTextAlign(
      const Value: TAdvGraphicsTextAlign);
    procedure SetBottomVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTopHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTopVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTopVerticalText(const Value: Boolean);
    procedure SetBottomVerticalText(const Value: Boolean);
    procedure SetFillEmptySpaces(const Value: Boolean);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Layouts: TAdvTreeViewGroupsLayouts read FLayouts write SetLayouts default [tglTop];
    property TopSize: Double read FTopSize write SetTopSize;
    property BottomSize: Double read FBottomSize write SetBottomSize;
    property TopFill: TAdvGraphicsFill read FTopFill write SetTopFill;
    property BottomFill: TAdvGraphicsFill read FBottomFill write SetBottomFill;
    property TopFont: TAdvGraphicsFont read FTopFont write SetTopFont;
    property BottomFont: TAdvGraphicsFont read FBottomFont write SetBottomFont;
    property TopStroke: TAdvGraphicsStroke read FTopStroke write SetTopStroke;
    property BottomStroke: TAdvGraphicsStroke read FBottomStroke write SetBottomStroke;
    property TopHorizontalTextAlign: TAdvGraphicsTextAlign read FTopHorizontalTextAlign write SetTopHorizontalTextAlign default gtaCenter;
    property TopVerticalTextAlign: TAdvGraphicsTextAlign read FTopVerticalTextAlign write SetTopVerticalTextAlign default gtaCenter;
    property BottomHorizontalTextAlign: TAdvGraphicsTextAlign read FBottomHorizontalTextAlign write SetBottomHorizontalTextAlign default gtaCenter;
    property BottomVerticalTextAlign: TAdvGraphicsTextAlign read FBottomVerticalTextAlign write SetBottomVerticalTextAlign default gtaCenter;
    property TopVerticalText: Boolean read FTopVerticalText write SetTopVerticalText default False;
    property BottomVerticalText: Boolean read FBottomVerticalText write SetBottomVerticalText default False;
    property FillEmptySpaces: Boolean read FFillEmptySpaces write SetFillEmptySpaces default True;
  end;

  TAdvTreeViewBeforeDrawSortIndicatorEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawSortIndicatorEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind) of object;
  TAdvTreeViewNodeCompareEvent = procedure(Sender: TObject; Node1, Node2: TAdvTreeViewNode; AColumn: Integer; var ACompareResult: Integer) of object;
  TAdvTreeViewBeforeSizeColumnEvent = procedure(Sender: TObject; AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean) of object;
  TAdvTreeViewAfterSizeColumnEvent = procedure(Sender: TObject; AColumn: Integer; AColumnSize: Double) of object;
  TAdvTreeViewBeforeDrawColumnEmptySpaceEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnEmptySpaceEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace) of object;
  TAdvTreeViewBeforeDrawGroupEmptySpaceEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawGroupEmptySpaceEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace) of object;

  TAdvTreeViewNeedFilterDropDownDataEvent = procedure(Sender: TObject; AColumn: Integer; AValues: TStrings) of object;
  TAdvTreeViewBeforeDrawColumnEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer) of object;
  TAdvTreeViewBeforeDrawColumnHeaderEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnHeaderEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind) of object;
  TAdvTreeViewBeforeDrawGroupEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawGroupEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind) of object;

  TAdvTreeViewBeforeDrawNodeEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawNodeEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewGetNumberOfNodesEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer) of object;

  TAdvTreeViewGetColumnTextEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String) of object;
  TAdvTreeViewBeforeDrawColumnTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawColumnTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String) of object;
  TAdvTreeViewGetGroupTextEvent = procedure(Sender: TObject; AGroup: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String) of object;
  TAdvTreeViewBeforeDrawGroupTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawGroupTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String) of object;
  TAdvTreeViewNodeAnchorClickEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String) of object;
  TAdvTreeViewNodeTitleAnchorClickEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String) of object;
  TAdvTreeViewNodeChangedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewGetNodeTextEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String) of object;
  TAdvTreeViewGetNodeDataEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewGetNodeTitleEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var ATitle: String) of object;
  TAdvTreeViewGetNodeTitleExpandedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExpanded: Boolean) of object;
  TAdvTreeViewGetNodeSidesEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides) of object;
  TAdvTreeViewGetNodeRoundingEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ARounding: Integer; var ACorners: TAdvGraphicsCorners) of object;
  TAdvTreeViewGetNodeTrimmingEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming) of object;
  TAdvTreeViewGetNodeHorizontalTextAlignEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvGraphicsTextAlign) of object;
  TAdvTreeViewGetNodeVerticalTextAlignEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvGraphicsTextAlign) of object;
  TAdvTreeViewGetNodeWordWrappingEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean) of object;
  TAdvTreeViewGetNodeExtraSizeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single) of object;
  TAdvTreeViewGetNodeTitleExtraSizeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleExtraSize: Single) of object;
  TAdvTreeViewGetColumnTrimmingEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvGraphicsTextTrimming) of object;
  TAdvTreeViewGetColumnHorizontalTextAlignEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvGraphicsTextAlign) of object;
  TAdvTreeViewGetColumnVerticalTextAlignEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvGraphicsTextAlign) of object;
  TAdvTreeViewGetColumnWordWrappingEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean) of object;
  TAdvTreeViewGetNodeIconEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvBitmap) of object;
  TAdvTreeViewGetNodeIconSizeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; AIcon: TAdvBitmap; var AIconWidth: Double; var AIconHeight: Double) of object;
  TAdvTreeViewFocusedNodeChangedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewGetNodeHeightEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double) of object;
  TAdvTreeViewBeforeDrawNodeTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String) of object;
  TAdvTreeViewBeforeDrawNodeTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String) of object;
  TAdvTreeViewBeforeDrawNodeExpandEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand:TAdvBitmap; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeExpandEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand:TAdvBitmap) of object;
  TAdvTreeViewBeforeDrawNodeIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon:TAdvBitmap; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon:TAdvBitmap) of object;
  TAdvTreeViewBeforeDrawNodeExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewDrawNodeExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeDrawNodeTitleExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeTitleExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewDrawNodeTitleExtraEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeDrawNodeCheckEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeCheckEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap) of object;
  TAdvTreeViewGetNodeColorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor) of object;
  TAdvTreeViewGetNodeCheckTypeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType) of object;
  TAdvTreeViewGetNodeTextColorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor) of object;
  TAdvTreeViewGetNodeTitleColorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleColor: TAdvGraphicsColor) of object;
  TAdvTreeViewIsNodeExtendedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean) of object;
  TAdvTreeViewColumnAnchorClickEvent = procedure(Sender: TObject; AColumn: Integer; AAnchor: String) of object;

  TAdvTreeViewIsNodeDeletableEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ADeletable: Boolean) of object;
  TAdvTreeViewIsNodeCheckedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AChecked: Boolean) of object;
  TAdvTreeViewIsNodeExpandedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean) of object;
  TAdvTreeViewIsNodeVisibleEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AVisible: Boolean) of object;
  TAdvTreeViewIsNodeEnabledEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean) of object;

  TAdvTreeViewBeforeUpdateNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String; var ACanUpdate: Boolean) of object;
  TAdvTreeViewAfterUpdateNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;
  TAdvTreeViewBeforeCollapseNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean) of object;
  TAdvTreeViewAfterCollapseNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeExpandNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean) of object;
  TAdvTreeViewAfterExpandNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean) of object;
  TAdvTreeViewAfterCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;
  TAdvTreeViewBeforeUnCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean) of object;
  TAdvTreeViewAfterUnCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;

  TAdvTreeViewBeforeSelectAllNodesEvent = procedure(Sender: TObject; var ACanSelect: Boolean) of object;
  TAdvTreeViewAfterSelectAllNodesEvent = procedure(Sender: TObject) of object;

  TAdvTreeViewBeforeSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanSelect: Boolean) of object;
  TAdvTreeViewAfterSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeUnSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean) of object;
  TAdvTreeViewAfterUnSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewNodeClickEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewNodeMouseEnterEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewNodeMouseLeaveEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewScrollEvent = procedure(Sender: TObject; APosition: Single) of object;

  {$IFDEF FNCLIB}
  TAdvTreeViewBeforeAddJSONNodeEvent = procedure(Sender: TObject; AJSONValue: TJSONValue; var AAddNode: Boolean) of object;
  TAdvTreeViewAfterAddJSONNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewNode; AJSONValue: TJSONValue) of object;
  {$ENDIF}
  TAdvTreeViewGetHTMLTemplateValueEvent = procedure(Sender: TObject; ANodeValue: TAdvTreeViewNodeValue; AName: string; var AValue: string) of object;
  TAdvTreeViewGetHTMLTemplateEvent = procedure(Sender: TObject; ANodeValue: TAdvTreeViewNodeValue; AColumnIndex: Integer; var AHTMLTemplate: string) of object;

  {$IFDEF FMXLIB}
  TAdvTreeViewInplaceEditor = TControl;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvTreeViewInplaceEditor = TWinControl;
  {$ENDIF}
  TAdvTreeViewInplaceEditorClass = class of TAdvTreeViewInplaceEditor;

  TAdvTreeViewCustomizeInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor) of object;
  TAdvTreeViewGetInplaceEditorRectEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; var AInplaceEditorRect: TRectF) of object;
  TAdvTreeViewGetInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;{$IFDEF FMXLIB} var ATransparent: Boolean; {$ENDIF}var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass) of object;
  TAdvTreeViewBeforeOpenInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean) of object;
  TAdvTreeViewAfterOpenInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: TRectF) of object;
  TAdvTreeViewCloseInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean; var ACanClose: Boolean) of object;
  TAdvTreeViewBeforeReorderNodeEvent = procedure(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode; var ACanReorder: Boolean) of object;
  TAdvTreeViewAfterReorderNodeEvent = procedure(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeSortNodesEvent = procedure(Sender: TObject; ASortColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode; var ACanSort: Boolean) of object;
  TAdvTreeViewAfterSortNodesEvent = procedure(Sender: TObject; ASortColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode) of object;
  TAdvTreeViewBeforeDropNodeEvent = procedure(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode; var ACanDrop: Boolean) of object;
  TAdvTreeViewAfterDropNodeEvent = procedure(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeCopyToClipboardEvent = procedure(Sender: TObject; var ACanCopy: Boolean) of object;
  TAdvTreeViewBeforeCutToClipboardEvent = procedure(Sender: TObject; var ACanCut: Boolean) of object;
  TAdvTreeViewBeforePasteFromClipboardEvent = procedure(Sender: TObject; var ACanPaste: Boolean) of object;
  TAdvTreeViewAfterCopyToClipboardEvent = procedure(Sender: TObject) of object;
  TAdvTreeViewAfterCutToClipboardEvent = procedure(Sender: TObject) of object;
  TAdvTreeViewAfterPasteFromClipboardEvent = procedure(Sender: TObject) of object;

  TAdvTreeViewFilterSelectEvent = procedure(Sender: TObject; AColumn: Integer; var ACondition: string) of object;

  TAdvTreeViewMouseEditMode = (tmemDoubleClick, tmemSingleClick, tmemSingleClickOnSelectedNode);

  TAdvTreeViewClipboardMode = (tcmNone, tcmTextOnly, tcmFull);
  TAdvTreeViewDragDropMode = (tdmNone, tdmMove, tdmCopy);

  TMultiSelectStyles = (msControlSelect, msShiftSelect, msVisibleOnly, msSiblingOnly);
  TMultiSelectStyle = set of TMultiSelectStyles;

  TAdvTreeViewInteraction = class;

  TAdvTreeViewLookup = class(TPersistent)
  private
    FOwner: TAdvTreeViewInteraction;
    FEnabled: Boolean;
    FCaseSensitive: Boolean;
    FIncremental: Boolean;
    FColumn: Integer;
    FAutoSelect: Boolean;
    FRootNodesOnly: Boolean;
    FAutoExpand: Boolean;
    FVisibleNodesOnly: Boolean;
  public
    constructor Create(AOwner: TAdvTreeViewInteraction);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Incremental: Boolean read FIncremental write FIncremental default True;
    property Column: Integer read FColumn write FColumn default -1;
    property RootNodesOnly: Boolean read FRootNodesOnly write FRootNodesOnly default False;
    property VisibleNodesOnly: Boolean read FVisibleNodesOnly write FVisibleNodesOnly default False;
    property AutoExpand: Boolean read FAutoExpand write FAutoExpand default False;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
  end;

  TAdvTreeViewInteraction = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FMultiSelect: Boolean;
    FTouchScrolling: Boolean;
    FReadOnly: Boolean;
    FColumnSizing: Boolean;
    FColumnAutoSizeOnDblClick: Boolean;
    FExtendedSelectable: Boolean;
    FSelectionFollowsFocus: Boolean;
    FKeyboardEdit: Boolean;
    FMouseEditMode: TAdvTreeViewMouseEditMode;
    FExtendedEditable: Boolean;
    FClipboardMode: TAdvTreeViewClipboardMode;
    FReorder: Boolean;
    FDragDropMode: TAdvTreeViewDragDropMode;
    FLookup: TAdvTreeViewLookup;
    FExpandCollapseOnDblClick: Boolean;
    FAutoOpenURL: Boolean;
    FURLDetectionOnMouseMove: Boolean;
    FMouseWheelDelta: Single;
    FAnimationFactor: Single;
    FMultiSelectStyle: TMultiSelectStyle;
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetTouchScrolling(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetColumnSizing(const Value: Boolean);
    procedure SetColumnAutoSizeOnDblClick(const Value: Boolean);
    procedure SetExtendedSelectable(const Value: Boolean);
    procedure SetSelectionFollowsFocus(const Value: Boolean);
    procedure SetMouseEditMode(const Value: TAdvTreeViewMouseEditMode);
    procedure SetExtendedEditable(const Value: Boolean);
    procedure SetDragDropMode(const Value: TAdvTreeViewDragDropMode);
    procedure SetReorder(const Value: Boolean);
    procedure SetLookup(const Value: TAdvTreeViewLookup);
    procedure SetExpandCollapseOnDblClick(const Value: Boolean);
    procedure SetURLDetectionOnMouseMove(const Value: Boolean);
    function IsMouseWheelDeltaStored: Boolean;
    procedure SetMouseWheelDelta(const Value: Single);
    function IsAnimationFactorStored: Boolean;
    procedure SetMultiSelectStyle(const Value: TMultiSelectStyle);
  protected
    property SelectionFollowsFocus: Boolean read FSelectionFollowsFocus write SetSelectionFollowsFocus default True;
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AnimationFactor: Single read FAnimationFactor write FAnimationFactor stored IsAnimationFactorStored nodefault;
    property AutoOpenURL: Boolean read FAutoOpenURL write FAutoOpenURL default True;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property MultiSelectStyle: TMultiSelectStyle read FMultiSelectStyle write SetMultiSelectStyle default [msControlSelect, msShiftSelect];
    property ExtendedSelectable: Boolean read FExtendedSelectable write SetExtendedSelectable default False;
    property ExtendedEditable: Boolean read FExtendedEditable write SetExtendedEditable default False;
    property MouseEditMode: TAdvTreeViewMouseEditMode read FMouseEditMode write SetMouseEditMode default tmemSingleClickOnSelectedNode;
    property MouseWheelDelta: Single read FMouseWheelDelta write SetMouseWheelDelta stored IsMouseWheelDeltaStored nodefault;
    property TouchScrolling: Boolean read FTouchScrolling write SetTouchScrolling default True;
    property KeyboardEdit: Boolean read FKeyboardEdit write FKeyboardEdit default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ExpandCollapseOnDblClick: Boolean read FExpandCollapseOnDblClick write SetExpandCollapseOnDblClick default True;
    property ColumnSizing: Boolean read FColumnSizing write SetColumnSizing default False;
    property ColumnAutoSizeOnDblClick: Boolean read FColumnAutoSizeOnDblClick write SetColumnAutoSizeOnDblClick default False;
    property ClipboardMode: TAdvTreeViewClipboardMode read FClipboardMode write FClipboardMode default tcmNone;
    property Reorder: Boolean read FReorder write SetReorder default False;
    property DragDropMode: TAdvTreeViewDragDropMode read FDragDropMode write SetDragDropMode default tdmNone;
    property Lookup: TAdvTreeViewLookup read FLookup write SetLookup;
    property URLDetectionOnMouseMove: Boolean read FURLDetectionOnMouseMove write SetURLDetectionOnMouseMove default true;
  end;

  TAdvTreeViewSceneDrawingScale = record
    SceneScale: Double;
    DrawingScale: TPointF;
  end;

  {$IFDEF WEBLIB}
  TAdvTreeViewSelectedNodes = class(TList)
  private
    function GetItem(Index: Integer): TAdvTreeViewVirtualNode;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewVirtualNode);
  public
    property Items[Index: Integer]: TAdvTreeViewVirtualNode read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTreeViewSelectedNodes = class(TList<TAdvTreeViewVirtualNode>);
  {$ENDIF}
  TAdvTreeViewNodeArray = array of TAdvTreeViewNode;

  TAdvTreeViewNodeCheck = record
    AColumn: Integer;
    ANode: TAdvTreeViewVirtualNode;
  end;

  TAdvTreeViewNodeAnchor = record
    AAnchor: String;
    AColumn: Integer;
  end;

  TAdvTreeViewComboBox = class(TComboBox);
  {$IFDEF FMXLIB}
  TAdvTreeViewCaretPosition = TCaretPosition;
  TAdvTreeViewMemo = class(TMemo)
  end;
  TAdvTreeViewEdit = class(TEdit)
  protected
    function GetDefaultStyleLookupName: string; override;
  end;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvTreeViewCaretPosition = TPoint;
  TAdvTreeViewMemo = class(TMemo)
  private
    FTreeView: TAdvCustomTreeView;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  TAdvTreeViewEdit = class(TEdit)
  private
    FTreeView: TAdvCustomTreeView;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  {$ENDIF}

  TAdvTreeViewCopyNodes = class(TAdvTreeViewNodes);

  TAdvTreeViewDragOverEvent = procedure(Sender: TObject; Source: TObject; Point: TPointF; var Accept: Boolean) of object;
  TAdvTreeViewDragDropEvent = procedure(Sender: TObject; Source: TObject; Point: TPointF) of object;
  TAdvTreeViewColumnSortEvent = procedure(Sender: TObject; AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode) of object;
  TAdvTreeViewLookupEvent = procedure(Sender: TObject; ALookupString: String) of object;
  TAdvTreeViewReorderEvent = procedure(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewExportState = (tvesExportStart, tvesExportNewRow, tvesExportDone, tvesExportSelRow, tvesExportFail, tvesExportNextRow, tvesExportFindRow);

  TAdvTreeViewAdapter = class(TAdvCustomComponent)
  private
    FBlockAdd: Boolean;
    FTreeView: TAdvCustomTreeView;
    FActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetTreeView(const Value: TAdvCustomTreeView);
  protected
    function GetInstance: NativeUInt; override;
    procedure GetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateBounds; virtual;
    procedure Initialize; virtual;
    procedure ScrollTreeView({%H-}ADelta: Integer); virtual;
    procedure SelectNode({%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    procedure GetNodeText({%H-}ACol: Integer;{%H-}ANode: TAdvTreeViewVirtualNode; var {%H-}AText: String); virtual;
    procedure ExportNotification({%H-}AState: TAdvTreeViewExportState; {%H-}ARow: Integer); virtual;
    function GetColumnDisplayName({%H-}ACol: Integer): String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property TreeView: TAdvCustomTreeView read FTreeView write SetTreeView;
  end;

  {$IFDEF FNCLIB}
  TAdvTreeViewViewJSONOptions = class(TPersistent)
  private
    FValueHTMLTemplate: string;
    FExpandNodes: Boolean;
    FNameHTMLTemplate: string;
    FOnChange: TNotifyEvent;
    FStretchColumn: Integer;
    FArrayItemPrefix: string;
    FArrayItemSuffix: string;
  protected
    procedure Changed;
//    function GetOwner: TPersistent; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ExpandNodes: Boolean read FExpandNodes write FExpandNodes default True;
    property StretchColumn: Integer read FStretchColumn write FStretchColumn default 1;
    property NameHTMLTemplate: string read FNameHTMLTemplate write FNameHTMLTemplate; //Use <#NAME>
    property ValueHTMLTemplate: string read FValueHTMLTemplate write FValueHTMLTemplate; //Use <#VALUE>
    property ArrayItemPrefix: string read FArrayItemPrefix write FArrayItemPrefix;
    property ArrayItemSuffix: string read FArrayItemSuffix write FArrayItemSuffix;
  end;
  {$ENDIF}

  TAdvCustomTreeView = class(TAdvTreeViewData, IAdvAppearanceGlobalFont)
  private
    FPrevDSP: TAdvTreeViewCacheItem;
    FOldTopRow: Integer;
    FCompactMode: Boolean;
    FVerticalOffset, FVerticalOffsetTo: Double;
    FAccepted: Boolean;
    FLookupString: String;
    FY, FDragY, FSaveDragY: Single;
    FX: Single;
    FDragBitmap: TAdvImageEx;
    FReorderMode, FDragMode, FDragModeStarted: Boolean;
    FSortColumn: Integer;
    FClosing: Boolean;
    FInplaceEditorClosed: Boolean;
    FColumnSize: Double;
    FCloseWithDialogKey: Boolean;
    FFilterPopup: TAdvPopupEx;
    FFilterListBox: TListBox;
    FFilterTimer: TTimer;
    FInplaceEditorClass: TAdvTreeViewInplaceEditorClass;
    FInplaceEditor, FOldInplaceEditor: TAdvTreeViewInplaceEditor;
    FInplaceEditorActive: Boolean;
    FSizeColumn, FDownColumn, FDownColumnFilter, FDownColumnExpand: Integer;
    FUpdateNodeColumn: Integer;
    FUpdateNode: TAdvTreeViewVirtualNode;
    FCopyNodes: TAdvTreeViewCopyNodes;
    {$IFDEF FMXLIB}
    FScrollBarTimer: TTimer;
    FInplaceEditorTimer: TTimer;
    {$ENDIF}
    FDoubleSelection: Boolean;
    FDragTimer: TTimer;
    FDragRow: Integer;
    FDownNode, FFocusedNode, FDragNode: TAdvTreeViewVirtualNode;
    FDblClicked: Boolean;
    FDoNodeAnchor, FDoNodeTitleAnchor: TAdvTreeViewNodeAnchor;
    FDoNodeCheck: TAdvTreeViewNodeCheck;
    FDoNodeExpand: Boolean;
    FDoNodeExtra: Boolean;
    FDoNodeTitleExtra: Boolean;
    FScrolling: Boolean;
    FMouseUp, FAnimateVerticalPos, FAnimateHorizontalPos, FAnimateVerticalOffset: Boolean;
    FAnimating: Boolean;
    FSpX, FSpY: Double;
    FScrollX, FScrollY, FDownX, FDownY, FMouseX, FMouseY, FSizeX: Double;
    FScrollVTo, FScrollHTo: Double;
    FTimeStart, FTimeStop: Double;
    FAnimateTimer: TTimer;
    FNodeCache: TAdvTreeViewNodeCache;
    FColumnsTopCache: TAdvTreeViewColumnsTopCache;
    FColumnsBottomCache: TAdvTreeViewColumnsBottomCache;
    FGroupsTopCache: TAdvTreeViewGroupsTopCache;
    FGroupsBottomCache: TAdvTreeViewGroupsBottomCache;
    FNodeDisplay: TAdvTreeViewNodeDisplayList;
    FColumnsTopDisplay: TAdvTreeViewColumnsTopDisplayList;
    FColumnsBottomDisplay: TAdvTreeViewColumnsBottomDisplayList;
    FGroupsTopDisplay: TAdvTreeViewGroupsTopDisplayList;
    FGroupsBottomDisplay: TAdvTreeViewGroupsBottomDisplayList;
    FNodesAppearance: TAdvTreeViewNodesAppearance;
    FColumnsCaching: Boolean;
    FGroupsCaching: Boolean;
    FGroupsAppearance: TAdvTreeViewGroupsAppearance;
    FColumnsAppearance: TAdvTreeViewColumnsAppearance;
    FOnAfterDrawGroup: TAdvTreeViewAfterDrawGroupEvent;
    FOnNodeCompare: TAdvTreeViewNodeCompareEvent;
    FOnBeforeSizeColumn: TAdvTreeViewBeforeSizeColumnEvent;
    FOnAfterSizeColumn: TAdvTreeViewAfterSizeColumnEvent;
    FOnBeforeDrawColumn: TAdvTreeViewBeforeDrawColumnEvent;
    FOnBeforeDrawColumnHeader: TAdvTreeViewBeforeDrawColumnHeaderEvent;
    FOnAfterDrawColumnHeader: TAdvTreeViewAfterDrawColumnHeaderEvent;
    FOnBeforeDrawNode: TAdvTreeViewBeforeDrawNodeEvent;
    FOnAfterDrawColumn: TAdvTreeViewAfterDrawColumnEvent;
    FOnBeforeDrawGroup: TAdvTreeViewBeforeDrawGroupEvent;
    FOnAfterDrawNode: TAdvTreeViewAfterDrawNodeEvent;
    FOnBeforeDrawNodeText: TAdvTreeViewBeforeDrawNodeTextEvent;
    FOnBeforeDrawNodeTitle: TAdvTreeViewBeforeDrawNodeTitleEvent;
    FOnGetNodeHeight: TAdvTreeViewGetNodeHeightEvent;
    FOnAfterDrawColumnText: TAdvTreeViewAfterDrawColumnTextEvent;
    FOnBeforeDrawGroupText: TAdvTreeViewBeforeDrawGroupTextEvent;
    FOnAfterDrawNodeText: TAdvTreeViewAfterDrawNodeTextEvent;
    FOnAfterDrawNodeTitle: TAdvTreeViewAfterDrawNodeTitleEvent;
    FOnAfterDrawGroupText: TAdvTreeViewAfterDrawGroupTextEvent;
    FOnBeforeDrawColumnText: TAdvTreeViewBeforeDrawColumnTextEvent;
    FInteraction: TAdvTreeViewInteraction;
    FOnAfterUpdateNode: TAdvTreeViewAfterUpdateNodeEvent;
    FOnBeforeUpdateNode: TAdvTreeViewBeforeUpdateNodeEvent;
    FOnAfterUnCheckNode: TAdvTreeViewAfterUnCheckNodeEvent;
    FOnBeforeUnCheckNode: TAdvTreeViewBeforeUnCheckNodeEvent;
    FOnAfterCheckNode: TAdvTreeViewAfterCheckNodeEvent;
    FOnBeforeCheckNode: TAdvTreeViewBeforeCheckNodeEvent;
    FOnAfterExpandNode: TAdvTreeViewAfterExpandNodeEvent;
    FOnBeforeExpandNode: TAdvTreeViewBeforeExpandNodeEvent;
    FOnAfterCollapseNode: TAdvTreeViewAfterCollapseNodeEvent;
    FOnBeforeCollapseNode: TAdvTreeViewBeforeCollapseNodeEvent;
    FOnBeforeSelectNode: TAdvTreeViewBeforeSelectNodeEvent;
    FOnAfterSelectNode: TAdvTreeViewAfterSelectNodeEvent;
    FOnBeforeSelectAllNodes: TAdvTreeViewBeforeSelectAllNodesEvent;
    FOnAfterSelectAllNodes: TAdvTreeViewAfterSelectAllNodesEvent;
    FOnNodeClick: TAdvTreeViewNodeClickEvent;
    FOnNodeDblClick: TAdvTreeViewNodeClickEvent;
    FOnGetNodeText: TAdvTreeViewGetNodeTextEvent;
    FOnGetNodeTitle: TAdvTreeViewGetNodeTitleEvent;
    FOnGetColumnText: TAdvTreeViewGetColumnTextEvent;
    FOnGetGroupText: TAdvTreeViewGetGroupTextEvent;
    FOnNodeAnchorClick: TAdvTreeViewNodeAnchorClickEvent;
    FOnVScroll: TAdvTreeViewScrollEvent;
    FOnHScroll: TAdvTreeViewScrollEvent;
    FOnBeforeDrawColumnEmptySpace: TAdvTreeViewBeforeDrawColumnEmptySpaceEvent;
    FOnAfterDrawColumnEmptySpace: TAdvTreeViewAfterDrawColumnEmptySpaceEvent;
    FOnBeforeDrawGroupEmptySpace: TAdvTreeViewBeforeDrawGroupEmptySpaceEvent;
    FOnAfterDrawGroupEmptySpace: TAdvTreeViewAfterDrawGroupEmptySpaceEvent;
    FOnGetNumberOfNodes: TAdvTreeViewGetNumberOfNodesEvent;
    FOnIsNodeExpanded: TAdvTreeViewIsNodeExpandedEvent;
    FOnIsNodeEnabled: TAdvTreeViewIsNodeEnabledEvent;
    FOnIsNodeVisible: TAdvTreeViewIsNodeVisibleEvent;
    FSelectedNodes: TAdvTreeViewSelectedNodes;
    FOnGetNodeDisabledColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnGetNodeTitleColor: TAdvTreeViewGetNodeTitleColorEvent;
    FOnGetNodeSelectedColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeDisabledTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnGetNodeSelectedTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnGetNodeDisabledTitleColor: TAdvTreeViewGetNodeTitleColorEvent;
    FOnGetNodeSelectedTitleColor: TAdvTreeViewGetNodeTitleColorEvent;
    FOnIsNodeExtended: TAdvTreeViewIsNodeExtendedEvent;
    FOnGetNodeIcon: TAdvTreeViewGetNodeIconEvent;
    FOnGetColumnWordWrapping: TAdvTreeViewGetColumnWordWrappingEvent;
    FOnGetColumnVerticalTextAlign: TAdvTreeViewGetColumnVerticalTextAlignEvent;
    FOnGetColumnTrimming: TAdvTreeViewGetColumnTrimmingEvent;
    FOnGetColumnHorizontalTextAlign: TAdvTreeViewGetColumnHorizontalTextAlignEvent;
    FOnGetNodeWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent;
    FOnGetNodeVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent;
    FOnGetNodeTrimming: TAdvTreeViewGetNodeTrimmingEvent;
    FOnGetNodeHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent;
    FOnIsNodeChecked: TAdvTreeViewIsNodeCheckedEvent;
    FOnBeforeDrawNodeIcon: TAdvTreeViewBeforeDrawNodeIconEvent;
    FOnBeforeDrawNodeExpand: TAdvTreeViewBeforeDrawNodeExpandEvent;
    FOnAfterDrawNodeIcon: TAdvTreeViewAfterDrawNodeIconEvent;
    FOnAfterDrawNodeExpand: TAdvTreeViewAfterDrawNodeExpandEvent;
    FOnGetNodeCheckType: TAdvTreeViewGetNodeCheckTypeEvent;
    FOnAfterDrawNodeCheck: TAdvTreeViewAfterDrawNodeCheckEvent;
    FOnBeforeDrawNodeCheck: TAdvTreeViewBeforeDrawNodeCheckEvent;
    FColumnStroke: TAdvGraphicsStroke;
    FOnAfterDrawNodeColumn: TAdvTreeViewAfterDrawColumnEvent;
    FOnBeforeDrawNodeColumn: TAdvTreeViewBeforeDrawColumnEvent;
    FOnBeforeUnSelectNode: TAdvTreeViewBeforeUnSelectNodeEvent;
    FOnAfterUnSelectNode: TAdvTreeViewAfterUnSelectNodeEvent;
    FOnAfterOpenInplaceEditor: TAdvTreeViewAfterOpenInplaceEditorEvent;
    FOnGetInplaceEditor: TAdvTreeViewGetInplaceEditorEvent;
    FOnCloseInplaceEditor: TAdvTreeViewCloseInplaceEditorEvent;
    FOnBeforeOpenInplaceEditor: TAdvTreeViewBeforeOpenInplaceEditorEvent;
    FOnNodeChanged: TAdvTreeViewNodeChangedEvent;
    FOnCustomizeInplaceEditor: TAdvTreeViewCustomizeInplaceEditorEvent;
    FOnGetInplaceEditorRect: TAdvTreeViewGetInplaceEditorRectEvent;
    FOnBeforeReorderNode: TAdvTreeViewBeforeReorderNodeEvent;
    FOnAfterReorderNode: TAdvTreeViewAfterReorderNodeEvent;
    FOnAfterDropNode: TAdvTreeViewAfterDropNodeEvent;
    FOnBeforeDropNode: TAdvTreeViewBeforeDropNodeEvent;
    FOnAfterCutToClipboard: TAdvTreeViewAfterCutToClipboardEvent;
    FOnBeforePasteFromClipboard: TAdvTreeViewBeforePasteFromClipboardEvent;
    FOnBeforeCopyToClipboard: TAdvTreeViewBeforeCopyToClipboardEvent;
    FOnAfterPasteFromClipboard: TAdvTreeViewAfterPasteFromClipboardEvent;
    FOnBeforeCutToClipboard: TAdvTreeViewBeforeCutToClipboardEvent;
    FOnAfterCopyToClipboard: TAdvTreeViewAfterCopyToClipboardEvent;
    FOnNeedFilterDropDownData: TAdvTreeViewNeedFilterDropDownDataEvent;
    FOnFilterSelect: TAdvTreeViewFilterSelectEvent;
    FOnCustomReorder: TAdvTreeViewReorderEvent;
    FOnCustomDragDrop: TAdvTreeViewDragDropEvent;
    FOnCustomDragOver: TAdvTreeViewDragOverEvent;
    FOnCustomCopyToClipboard: TNotifyEvent;
    FOnCustomCutToClipboard: TNotifyEvent;
    FOnCustomPasteFromClipboard: TNotifyEvent;
    FOnCustomColumnSort: TAdvTreeViewColumnSortEvent;
    FOnCustomLookup: TAdvTreeViewLookupEvent;
    FOnBeforeDrawSortIndicator: TAdvTreeViewBeforeDrawSortIndicatorEvent;
    FOnAfterDrawSortIndicator: TAdvTreeViewAfterDrawSortIndicatorEvent;
    FAdapter: TAdvTreeViewAdapter;
    FOnGetNodeSides: TAdvTreeViewGetNodeSidesEvent;
    FOnGetNodeExtraSize: TAdvTreeViewGetNodeExtraSizeEvent;
    FOnGetNodeTitleExtraSize: TAdvTreeViewGetNodeTitleExtraSizeEvent;
    FOnAfterDrawNodeExtra: TAdvTreeViewAfterDrawNodeExtraEvent;
    FOnBeforeDrawNodeExtra: TAdvTreeViewBeforeDrawNodeExtraEvent;
    FOnDrawNodeExtra: TAdvTreeViewDrawNodeExtraEvent;
    FOnGetNodeTitleTrimming: TAdvTreeViewGetNodeTrimmingEvent;
    FOnGetNodeTitleHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent;
    FOnGetNodeTitleWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent;
    FOnGetNodeTitleVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent;
    FOnNodeTitleAnchorClick: TAdvTreeViewNodeTitleAnchorClickEvent;
    FOnGetNodeExpanded: TAdvTreeViewGetNodeTitleExpandedEvent;
    FOnBeforeDrawNodeTitleExtra: TAdvTreeViewBeforeDrawNodeTitleExtraEvent;
    FOnAfterDrawNodeTitleExtra: TAdvTreeViewAfterDrawNodeTitleExtraEvent;
    FOnDrawNodeTitleExtra: TAdvTreeViewDrawNodeTitleExtraEvent;
    FOnNodeMouseEnter: TAdvTreeViewNodeMouseEnterEvent;
    FOnNodeMouseLeave: TAdvTreeViewNodeMouseLeaveEvent;
    FOnGetNodeIconSize: TAdvTreeViewGetNodeIconSizeEvent;
    FOnColumnAnchorClick: TAdvTreeViewColumnAnchorClickEvent;
    FOnFocusedNodeChanged: TAdvTreeViewFocusedNodeChangedEvent;
    FOnGetNodeData: TAdvTreeViewGetNodeDataEvent;
    FOnAfterSortNodes: TAdvTreeViewAfterSortNodesEvent;
    FOnBeforeSortNodes: TAdvTreeViewBeforeSortNodesEvent;
    FGlobalFont: TAdvAppearanceGlobalFont;
    FOnGetNodeRounding: TAdvTreeViewGetNodeRoundingEvent;
    FOnGetNodeDisabledStrokeColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeSelectedStrokeColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeStrokeColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetHTMLTemplateValue: TAdvTreeViewGetHTMLTemplateValueEvent;
    FOnGetHTMLTemplate: TAdvTreeViewGetHTMLTemplateEvent;
    {$IFDEF FNCLIB}
    FDefaultViewJSONOptions: TAdvTreeViewViewJSONOptions;
    FOnAfterAddJSONNode: TAdvTreeViewAfterAddJSONNodeEvent;
    FOnBeforeAddJSONNode: TAdvTreeViewBeforeAddJSONNodeEvent;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FCheckStartSize: Integer;
    {$ENDIF}
    procedure SetNodesAppearance(const Value: TAdvTreeViewNodesAppearance);
    procedure SetGroupsAppearance(const Value: TAdvTreeViewGroupsAppearance);
    procedure SetColumnsAppearance(const Value: TAdvTreeViewColumnsAppearance);
    procedure SetInteraction(const Value: TAdvTreeViewInteraction);
    function GetFocusedNode: TAdvTreeViewNode;
    function GetFocusedVirtualNode: TAdvTreeViewVirtualNode;
    function GetSelNode(AIndex: Integer): TAdvTreeViewNode;
    function GetSelVirtualNode(AIndex: Integer): TAdvTreeViewVirtualNode;
    procedure SetColumnStroke(const Value: TAdvGraphicsStroke);
    procedure SetFocusedNode(const Value: TAdvTreeViewNode);
    procedure SetFocusedVirtualNode(const Value: TAdvTreeViewVirtualNode);
    procedure SetSortColumn(const Value: Integer);
    procedure HandleFilterTimer(Sender: TObject);
    procedure SetAdapter(const Value: TAdvTreeViewAdapter);
    procedure SetSelectedNode(const Value: TAdvTreeViewNode);
    procedure SetSelectedVirtualNode(const Value: TAdvTreeViewVirtualNode);
    procedure SetCompactMode(const Value: Boolean);
    function GetSelectedVirtualNodeRow: Integer;
    function GetTopRow: Integer;
    procedure SetTopRow(const Value: Integer);
    function GetBottomRow: Integer;
    procedure SetGlobalFont(const Value: TAdvAppearanceGlobalFont);
    {$IFDEF FNCLIB}
    procedure SetDefaultViewJSONOptions(const Value: TAdvTreeViewViewJSONOptions);
    {$ENDIF}
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure RegisterRuntimeClasses; override;
    procedure ApplyStyle; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure UpdateControlAfterResize; override;
    procedure ResetToDefaultStyle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitializeColumnSorting(AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode); virtual;
    procedure AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False); override;
    procedure DoNodeCompare(ANode1, ANode2: TAdvTreeViewNode; AColumn: Integer; var ACompareResult: Integer); override;
    procedure SetFonts(ASetType: TAdvAppearanceGlobalFontType); virtual;

    procedure ExportNotification(AState: TAdvTreeViewExportState; ARow: Integer); virtual;
    procedure DoBeforeSortNodes(ASortColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode; var ACanSort: Boolean); virtual;
    procedure DoAfterSortNodes(ASortColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode); virtual;
    procedure DoBeforeDrawSortIndicator(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawSortIndicator(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind); virtual;
    procedure DoBeforeCutToClipboard(var ACanCut: Boolean); virtual;
    procedure DoBeforeCopyToClipboard(var ACanCopy: Boolean); virtual;
    procedure DoBeforePasteFromClipboard(var ACanPaste: Boolean); virtual;
    procedure DoAfterCutToClipboard; virtual;
    procedure DoAfterCopyToClipboard; virtual;
    procedure DoAfterPasteFromClipboard; virtual;

    procedure DoBeforeReorderNode(AFromNode, AToNode: TAdvTreeViewVirtualNode; var ACanReorder: Boolean); virtual;
    procedure DoAfterReorderNode(AFromNode, AToNode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeDropNode(AFromNode, AToNode: TAdvTreeViewVirtualNode; var ACanDrop: Boolean); virtual;
    procedure DoAfterDropNode(AFromNode, AToNode: TAdvTreeViewVirtualNode); virtual;

    procedure DoBeforeDrawColumnEmptySpace(AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean);
    procedure DoAfterDrawColumnEmptySpace(AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace);
    procedure DoBeforeDrawGroupEmptySpace(AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean);
    procedure DoAfterDrawGroupEmptySpace(AGraphics: TAdvGraphics; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace);

    procedure DoBeforeDrawColumnHeader(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawColumnHeader(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DoBeforeDrawColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer); virtual;
    procedure DoBeforeDrawNodeColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawNodeColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer); virtual;
    procedure DoBeforeDrawGroup(AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawGroup(AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DoBeforeDrawNode(AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawNode(AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode); virtual;

    procedure DoGetColumnText(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String); virtual;
    procedure DoBeforeDrawColumnText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawColumnText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String); virtual;
    procedure DoGetGroupText(AGroup: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String); virtual;
    procedure DoBeforeDrawGroupText(AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawGroupText(AGraphics: TAdvGraphics; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String); virtual;
    procedure DoNodeClick(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeMouseLeave(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeMouseEnter(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeDblClick(ANode: TAdvTreeViewVirtualNode); virtual;

    procedure DoGetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer); override;
    procedure DoGetNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String); override;
    procedure DoGetNodeData(ANode: TAdvTreeViewVirtualNode); override;
    procedure DoGetNodeTitle(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var ATitle: String); override;
    procedure DoGetNodeTitleExpanded(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExpanded: Boolean); override;
    procedure DoGetNodeTrimming(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming); override;
    procedure DoGetNodeHorizontalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetNodeVerticalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetNodeWordWrapping(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean); override;
    procedure DoGetNodeTitleTrimming(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming); override;
    procedure DoGetNodeTitleHorizontalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetNodeTitleVerticalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetNodeTitleWordWrapping(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean); override;
    procedure DoGetNodeExtraSize(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single); override;
    procedure DoGetNodeTitleExtraSize(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleExtraSize: Single); override;
    procedure DoGetColumnTrimming(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvGraphicsTextTrimming); override;
    procedure DoGetColumnHorizontalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetColumnVerticalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvGraphicsTextAlign); override;
    procedure DoGetColumnWordWrapping(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean); override;
    procedure DoGetNodeCheckType(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType); override;
    procedure DoGetNodeHeight(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double); virtual;
    procedure DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvBitmap); override;
    procedure DoGetNodeIconSize(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; AIcon: TAdvBitmap; var AIconWidth: Double; var AIconHeight: Double); override;
    procedure DoGetNodeSelectedColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeSelectedStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeSides(ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides); override;
    procedure DoGetNodeRounding(ANode: TAdvTreeViewVirtualNode; var ARounding: Integer; var ACorners: TAdvGraphicsCorners); virtual;
    procedure DoGetNodeDisabledColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeDisabledStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeSelectedTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor); override;
    procedure DoGetNodeDisabledTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor); override;
    procedure DoGetNodeTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor); override;
    procedure DoGetNodeSelectedTitleColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleColor: TAdvGraphicsColor); override;
    procedure DoGetNodeDisabledTitleColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleColor: TAdvGraphicsColor); override;
    procedure DoGetNodeTitleColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleColor: TAdvGraphicsColor); override;
    procedure DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean); override;
    procedure DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean); override;
    procedure DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AChecked: Boolean); override;
    procedure DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode; var AVisible: Boolean); override;
    procedure DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean); override;

    procedure DoBeforeDrawNodeText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String); virtual;
    procedure DoBeforeDrawNodeTitle(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeTitle(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String); virtual;
    procedure DoBeforeDrawNodeIcon(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeIcon(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap); virtual;
    procedure DoBeforeDrawNodeExtra(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean); virtual;
    procedure DoDrawNodeExtra({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}AColumn: Integer; {%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterDrawNodeExtra(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeDrawNodeTitleExtra(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean); virtual;
    procedure DoDrawNodeTitleExtra({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}AColumn: Integer; {%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterDrawNodeTitleExtra(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeDrawNodeCheck(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeCheck(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap); virtual;
    procedure DoBeforeDrawNodeExpand(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeExpand(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand: TAdvBitmap); virtual;
    procedure DoBeforeSelectNode(ANode: TAdvTreeViewVirtualNode; var ACanSelect: Boolean); virtual;
    procedure DoBeforeSelectAllNodes(var ACanSelect: Boolean); virtual;
    procedure DoBeforeUnSelectNode(ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean); virtual;
    procedure DoCustomizeInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor); virtual;
    procedure DoBeforeSizeColumn(AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean); virtual;
    procedure DoAfterSizeColumn(AColumn: Integer; AColumnSize: Double); virtual;
    procedure DoBeforeUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String; var ACanUpdate: Boolean); virtual;
    procedure DoNodeChanged(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoUpdateNodeText({%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}AText: String); virtual;
    procedure DoUpdateNodeTitle({%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}ATitle: String); virtual;
    procedure DoBeforeExpandNode(ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean); virtual;
    procedure DoAfterExpandNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeCollapseNode(ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean); virtual;
    procedure DoAfterCollapseNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean); virtual;
    procedure DoAfterCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoBeforeUnCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean); virtual;
    procedure DoAfterUnCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoNodeAnchorClick(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String); virtual;
    procedure DoNodeTitleAnchorClick(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String); virtual;
    procedure DoColumnAnchorClick(AColumn: Integer; AAnchor: String); virtual;
    procedure DoGetHTMLTemplateValue(ANodeValue: TAdvTreeViewNodeValue; AName: string; var AValue: string); override;
    procedure DoGetHTMLTemplate(ANodeValue: TAdvTreeViewNodeValue; AColumnIndex: Integer; var AHTMLTemplate: string); override;
    {$IFDEF FMXLIB}
    procedure ApplyInplaceEditorStyleLookup(Sender: TObject);
    {$ENDIF}
    {$IFDEF FNCLIB}
    procedure DoBeforeAddJSONNode(AJSONValue: TJSONValue; var AAddNode: Boolean);
    procedure DoAfterAddJSONNode(ANode: TAdvTreeViewNode; AJSONValue: TJSONValue);
    {$ENDIF}

    procedure DoBeforeOpenInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean); virtual;
    procedure DoAfterOpenInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: TRectF); virtual;
    procedure DoCloseInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean; var ACanClose: Boolean); virtual;
    procedure UpdateInplaceEditorPosition; virtual;

    procedure DoGetInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;{$IFDEF FMXLIB} var ATransparent: Boolean;{$ENDIF} var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass); virtual;
    procedure CloseInplaceEditor(ACancel: Boolean; AFlagClose: Boolean = False); virtual;

    procedure DoFocusedNodeChanged(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterSelectAllNodes; virtual;
    procedure DoAfterUnSelectNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoHScroll(APosition: Single); virtual;
    procedure DoVScroll(APosition: Single); virtual;
    procedure ResetNodes(AUpdateAll: Boolean = True); override;
    procedure CreateDragBitmap; virtual;
    procedure DestroyDragBitmap; virtual;

    procedure HandleSelectNode(ANode: TAdvTreeViewVirtualNode; ATriggerEvents: Boolean; AKeyBoard: Boolean; AMultiSelect: Boolean);
    procedure Animate(Sender: TObject);
    {$IFDEF FMXLIB}
    procedure ScrollBarChanged(Sender: TObject);
    procedure DoInplaceEditorTimer(Sender: TObject);
    {$ENDIF}
    procedure ColumnStrokeChanged(Sender: TObject);
    procedure DragTime(Sender: TObject);
    procedure StopAnimationTimer; override;
    procedure HandleNodeToggle(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure HandleNodeExtra({%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    procedure HandleNodeTitleExtra({%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    procedure HandleNodeToggleCheck(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure HandleNodeExpand(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean); virtual;
    procedure HandleNodeCollapse(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean); virtual;
    procedure HandleCustomKeys({%H-}AKey: Word); virtual;
    procedure OffsetNodeRects(ANode: TAdvTreeViewVirtualNode; AX, AY: Single; var ARect: TRectF); virtual;
    procedure BuildDisplay(ACache: TAdvTreeViewCache; ADisplay: TAdvTreeViewDisplayList); virtual;
    procedure UpdateCalculations; override;
    procedure UpdateAutoSizing; override;
    procedure UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True); override;
    procedure UpdateColumnsCache; override;
    procedure UpdateColumnCache(ACache: TAdvTreeViewCache); overload; virtual;
    procedure UpdateGroupsCache; override;
    procedure UpdateGroupCache(ACache: TAdvTreeViewCache); overload; virtual;
    procedure UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False); override;
    procedure UpdateNodeCache; virtual;
    procedure CustomizeNodeCache({%H-}AGraphics: TAdvGraphics; {%H-}AStartY: Single); virtual;
    procedure UpdateDisplay; override;
    procedure UpdateTreeViewCache; override;
    procedure UpdateColumnsDisplay; virtual;
    procedure UpdateGroupsDisplay; virtual;
    procedure UpdateNodeDisplay; virtual;
    procedure VerticalScrollPositionChanged; override;
    procedure Scroll(AHorizontalPos, AVerticalPos: Double); override;
    procedure HorizontalScrollPositionChanged; override;
    procedure DrawNode({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}ANode: TAdvTreeViewVirtualNode; {%H-}ACaching: Boolean = False; {%H-}AOffsetX: Single = 0; {%H-}AOffsetY: Single = 0); virtual;
    procedure DrawGroup({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}AGroup: Integer; {%H-}AStartColumn, {%H-}AEndColumn: Integer; {%H-}AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DrawColumn({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}AColumn: Integer; {%H-}AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DrawNodes({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawColumns({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawNodeColumns({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawGroups({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawBorders({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawEmptySpaces({%H-}AGraphics: TAdvGraphics); virtual;
    procedure DrawDisplay(AGraphics: TAdvGraphics; ADisplay: TAdvTreeViewDisplayList); virtual;
    procedure HandleDragStart({%H-}X, {%H-}Y: Single); virtual;
    procedure HandleDragOver(const Source: TObject; const Point: TPointF; var Accept: Boolean); override;
    procedure HandleDragDrop(const Source: TObject; const Point: TPointF); override;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleDblClick(X, Y: Single); override;
    procedure DoNeedFilterDropDownData(AColumn: Integer; AValues: TStrings); virtual;
    procedure DoFilterSelect(AColumn: integer; var ACondition: string); virtual;
    procedure HandleFilter(AColumn: Integer); virtual;
    procedure HandleExpand({%H-}AColumn: Integer); virtual;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure StartVerticalOffsetAnimation; virtual;
    procedure ClearFocusedNode(ANode: TAdvTreeViewVirtualNode); override;
    procedure SetVerticalOffset(AVerticalOffset: Single); virtual;
    procedure HandleKeyPress(var Key: Char); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleDialogKey(var Key: Word; Shift: TShiftState); override;
    procedure HandleFilterListClick(Sender: TObject);
    {$IFDEF FMXLIB}
    procedure ApplyFilterListBoxStyleLookUp(Sender: TObject);
    {$ENDIF}
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure HandleNodeEditing(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure CustomizeInplaceEditor(AInplaceEditor: TAdvTreeViewInplaceEditor; ANode: TAdvTreeViewVirtualNode; AColumn: TAdvTreeViewColumn); virtual;
    procedure StartReload; virtual;
    procedure ConfigureNodeRect({%H-}AIndex: Integer; {%H-}ANode: TAdvTreeViewVirtualNode; var {%H-}ARect: TRectF); virtual;
    procedure GetNodeOffset({%H-}ANode: TAdvTreeViewVirtualNode; var {%H-}ALeft, {%H-}ATop, {%H-}ARight, {%H-}ABottom: Single); virtual;
    procedure GetNodeMargins(var {%H-}ALeft, {%H-}ATop, {%H-}ARight, {%H-}ABottom: Single); virtual;
    procedure HandleAlternativeDragOver({%H-}X, {%H-}Y: Single); virtual;
    procedure HandleAlternativeDragDrop({%H-}X, {%H-}Y: Single); virtual;
    procedure InternalSelectVirtualNode({%H-}ANode: TAdvTreeViewVirtualNode); virtual;
    function CanStartDragFromMouseDown: Boolean; virtual;
    function CanStartDragFromMouseMove: Boolean; virtual;
    function AlternativeDragDrop: Boolean; virtual;
    function GetNodesSpacing: Single; override;
    function IsAnimating: Boolean; virtual;
    function GetNodesRect: TRectF; virtual;
    function CanApplyVerticalOffset: Boolean; virtual;
    function GetReloadOffset: Single; virtual;
    function NeedsReload({%H-}AVerticalOffset: Single): Boolean; virtual;
    function IsVariableNodeHeight: Boolean; override;
    function GetDragObjectScreenShot: TAdvBitmap; override;
    function ColumnStretchingActive: Boolean; override;
    function GetBufStart(ABuffer: string; var ALevel: Integer): string;
    function GetFirstEditableColumn: Integer; virtual;
    function GetLastEditableColumn: Integer; virtual;
    function GetPreviousEditableColumn(AColumn: Integer): Integer; virtual;
    function GetNextEditableColumn(AColumn: Integer): Integer; virtual;
    function GetInplaceEditorRect(ANode: TAdvTreeViewVirtualNode; AColumn: Integer): TRectF;
    function GetNextFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetPreviousFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetVersion: string; override;
    function GetFirstVisibleColumn: Integer; virtual;
    function GetLastVisibleColumn: Integer; virtual;
    function ConfigureNode(AGraphics: TAdvGraphics; AIndex: Integer; ANode: TAdvTreeViewVirtualNode; var AStartY: Single): Boolean; virtual;
    function XYToColumnAnchorCache(AX, AY: Single; ACache: TAdvTreeViewColumnsCache; var AIndex: Integer): string;
    function GetDocURL: string; override;
    function GetRowHeight(ARow: Integer): Double; override;
    function GetColumnText(AColumn: Integer): String; virtual;
    function GetGroupText(AGroup: Integer): String;virtual;
    function GetColumnsTopSize: Double; virtual;
    function GetColumnsExtraSize: Double; virtual;
    function GetColumnsBottomSize: Double; virtual;
    function GetGroupsTopSize: Double; virtual;
    function GetGroupsBottomSize: Double; virtual;
    function GetContentClipRect: TRectF; override;
    function GetContentRect: TRectF; override;
    function GetCalculationRect: TRectF; override;
    function GetGroupsTopRect: TRectF; virtual;
    function GetGroupsBottomRect: TRectF; virtual;
    function GetColumnsTopRect: TRectF; virtual;
    function GetColumnTopLeftEmptyRect: TRectF; virtual;
    function GetColumnBottomLeftEmptyRect: TRectF; virtual;
    function GetColumnBottomRightEmptyRect: TRectF; virtual;
    function GetColumnTopRightEmptyRect: TRectF; virtual;
    function GetGroupTopLeftEmptyRect: TRectF; virtual;
    function GetGroupBottomLeftEmptyRect: TRectF; virtual;
    function GetGroupBottomRightEmptyRect: TRectF; virtual;
    function GetGroupTopRightEmptyRect: TRectF; virtual;
    function GetColumnsBottomRect: TRectF; virtual;
    function GetCacheWidth: Integer; virtual;
    function GetCacheHeight: Integer; virtual;
    function XYToCacheItem(X, Y: Double; OffsetX: Single = 0): TAdvTreeViewCacheItem; virtual;
    function GetFirstVisibleCacheItem: TAdvTreeViewCacheItem; virtual;
    function IsReorderActive: Boolean; virtual;
    function IsDragDropActive: Boolean; virtual;
    function GetHTMLTemplate(AColumnIndex: Integer): string; override;

    property SelectedVirtualNodeRow: Integer read GetSelectedVirtualNodeRow;

    property Adapter: TAdvTreeViewAdapter read FAdapter write SetAdapter;
    property NodesAppearance: TAdvTreeViewNodesAppearance read FNodesAppearance write SetNodesAppearance;
    property ColumnsAppearance: TAdvTreeViewColumnsAppearance read FColumnsAppearance write SetColumnsAppearance;
    property GroupsAppearance: TAdvTreeViewGroupsAppearance read FGroupsAppearance write SetGroupsAppearance;

    property GlobalFont: TAdvAppearanceGlobalFont read FGlobalFont write SetGlobalFont;
    {$IFDEF FNCLIB}
    property DefaultViewJSONOptions: TAdvTreeViewViewJSONOptions read FDefaultViewJSONOptions write SetDefaultViewJSONOptions;
    {$ENDIF}

    property OnBeforeCutToClipboard: TAdvTreeViewBeforeCutToClipboardEvent read FOnBeforeCutToClipboard write FOnBeforeCutToClipboard;
    property OnBeforeCopyToClipboard: TAdvTreeViewBeforeCopyToClipboardEvent read FOnBeforeCopyToClipboard write FOnBeforeCopyToClipboard;
    property OnBeforePasteFromClipboard: TAdvTreeViewBeforePasteFromClipboardEvent read FOnBeforePasteFromClipboard write FOnBeforePasteFromClipboard;
    property OnAfterCutToClipboard: TAdvTreeViewAfterCutToClipboardEvent read FOnAfterCutToClipboard write FOnAfterCutToClipboard;
    property OnAfterCopyToClipboard: TAdvTreeViewAfterCopyToClipboardEvent read FOnAfterCopyToClipboard write FOnAfterCopyToClipboard;
    property OnAfterPasteFromClipboard: TAdvTreeViewAfterPasteFromClipboardEvent read FOnAfterPasteFromClipboard write FOnAfterPasteFromClipboard;

    property OnBeforeSortNodes: TAdvTreeViewBeforeSortNodesEvent read FOnBeforeSortNodes write FOnBeforeSortNodes;
    property OnAfterSortNodes: TAdvTreeViewAfterSortNodesEvent read FOnAfterSortNodes write FOnAfterSortNodes;
    property OnBeforeReorderNode: TAdvTreeViewBeforeReorderNodeEvent read FOnBeforeReorderNode write FOnBeforeReorderNode;
    property OnAfterReorderNode: TAdvTreeViewAfterReorderNodeEvent read FOnAfterReorderNode write FOnAfterReorderNode;
    property OnBeforeDropNode: TAdvTreeViewBeforeDropNodeEvent read FOnBeforeDropNode write FOnBeforeDropNode;
    property OnAfterDropNode: TAdvTreeViewAfterDropNodeEvent read FOnAfterDropNode write FOnAfterDropNode;

    property OnBeforeOpenInplaceEditor: TAdvTreeViewBeforeOpenInplaceEditorEvent read FOnBeforeOpenInplaceEditor write FOnBeforeOpenInplaceEditor;
    property OnCloseInplaceEditor: TAdvTreeViewCloseInplaceEditorEvent read FOnCloseInplaceEditor write FOnCloseInplaceEditor;
    property OnAfterOpenInplaceEditor: TAdvTreeViewAfterOpenInplaceEditorEvent read FOnAfterOpenInplaceEditor write FOnAfterOpenInplaceEditor;
    property OnGetInplaceEditor: TAdvTreeViewGetInplaceEditorEvent read FOnGetInplaceEditor write FOnGetInplaceEditor;
    property OnCustomizeInplaceEditor: TAdvTreeViewCustomizeInplaceEditorEvent read FOnCustomizeInplaceEditor write FOnCustomizeInplaceEditor;
    property OnGetInplaceEditorRect: TAdvTreeViewGetInplaceEditorRectEvent read FOnGetInplaceEditorRect write FOnGetInplaceEditorRect;

    property OnNodeCompare: TAdvTreeViewNodeCompareEvent read FOnNodeCompare write FOnNodeCompare;
    property OnBeforeSizeColumn: TAdvTreeViewBeforeSizeColumnEvent read FOnBeforeSizeColumn write FOnBeforeSizeColumn;
    property OnAfterSizeColumn: TAdvTreeViewAfterSizeColumnEvent read FOnAfterSizeColumn write FOnAfterSizeColumn;
    property OnBeforeDrawColumnEmptySpace: TAdvTreeViewBeforeDrawColumnEmptySpaceEvent read FOnBeforeDrawColumnEmptySpace write FOnBeforeDrawColumnEmptySpace;
    property OnAfterDrawColumnEmptySpace: TAdvTreeViewAfterDrawColumnEmptySpaceEvent read FOnAfterDrawColumnEmptySpace write FOnAfterDrawColumnEmptySpace;
    property OnBeforeDrawGroupEmptySpace: TAdvTreeViewBeforeDrawGroupEmptySpaceEvent read FOnBeforeDrawGroupEmptySpace write FOnBeforeDrawGroupEmptySpace;
    property OnAfterDrawGroupEmptySpace: TAdvTreeViewAfterDrawGroupEmptySpaceEvent read FOnAfterDrawGroupEmptySpace write FOnAfterDrawGroupEmptySpace;

    property OnNeedFilterDropDownData: TAdvTreeViewNeedFilterDropDownDataEvent read FOnNeedFilterDropDownData write FOnNeedFilterDropDownData;
    property OnFilterSelect: TAdvTreeViewFilterSelectEvent read FOnFilterSelect write FOnFilterSelect;
    property OnBeforeDrawColumn: TAdvTreeViewBeforeDrawColumnEvent read FOnBeforeDrawColumn write FOnBeforeDrawColumn;
    property OnAfterDrawColumn: TAdvTreeViewAfterDrawColumnEvent read FOnAfterDrawColumn write FOnAfterDrawColumn;
    property OnBeforeDrawNodeColumn: TAdvTreeViewBeforeDrawColumnEvent read FOnBeforeDrawNodeColumn write FOnBeforeDrawNodeColumn;
    property OnAfterDrawNodeColumn: TAdvTreeViewAfterDrawColumnEvent read FOnAfterDrawNodeColumn write FOnAfterDrawNodeColumn;
    property OnBeforeDrawColumnHeader: TAdvTreeViewBeforeDrawColumnHeaderEvent read FOnBeforeDrawColumnHeader write FOnBeforeDrawColumnHeader;
    property OnAfterDrawColumnHeader: TAdvTreeViewAfterDrawColumnHeaderEvent read FOnAfterDrawColumnHeader write FOnAfterDrawColumnHeader;
    property OnBeforeDrawGroup: TAdvTreeViewBeforeDrawGroupEvent read FOnBeforeDrawGroup write FOnBeforeDrawGroup;
    property OnAfterDrawGroup: TAdvTreeViewAfterDrawGroupEvent read FOnAfterDrawGroup write FOnAfterDrawGroup;
    property OnBeforeDrawNode: TAdvTreeViewBeforeDrawNodeEvent read FOnBeforeDrawNode write FOnBeforeDrawNode;
    property OnAfterDrawNode: TAdvTreeViewAfterDrawNodeEvent read FOnAfterDrawNode write FOnAfterDrawNode;
    property OnBeforeDrawColumnText: TAdvTreeViewBeforeDrawColumnTextEvent read FOnBeforeDrawColumnText write FOnBeforeDrawColumnText;
    property OnGetColumnText: TAdvTreeViewGetColumnTextEvent read FOnGetColumnText write FOnGetColumnText;
    property OnAfterDrawColumnText: TAdvTreeViewAfterDrawColumnTextEvent read FOnAfterDrawColumnText write FOnAfterDrawColumnText;
    property OnBeforeDrawGroupText: TAdvTreeViewBeforeDrawGroupTextEvent read FOnBeforeDrawGroupText write FOnBeforeDrawGroupText;
    property OnGetGroupText: TAdvTreeViewGetGroupTextEvent read FOnGetGroupText write FOnGetGroupText;
    property OnAfterDrawGroupText: TAdvTreeViewAfterDrawGroupTextEvent read FOnAfterDrawGroupText write FOnAfterDrawGroupText;
    property OnBeforeDrawNodeText: TAdvTreeViewBeforeDrawNodeTextEvent read FOnBeforeDrawNodeText write FOnBeforeDrawNodeText;
    property OnBeforeDrawNodeTitle: TAdvTreeViewBeforeDrawNodeTitleEvent read FOnBeforeDrawNodeTitle write FOnBeforeDrawNodeTitle;
    property OnAfterDrawSortIndicator: TAdvTreeViewAfterDrawSortIndicatorEvent read FOnAfterDrawSortIndicator write FOnAfterDrawSortIndicator;
    property OnBeforeDrawSortIndicator: TAdvTreeViewBeforeDrawSortIndicatorEvent read FOnBeforeDrawSortIndicator write FOnBeforeDrawSortIndicator;
    property OnGetNodeHeight: TAdvTreeViewGetNodeHeightEvent read FOnGetNodeHeight write FOnGetNodeHeight;
    property OnGetNodeText: TAdvTreeViewGetNodeTextEvent read FOnGetNodeText write FOnGetNodeText;
    property OnGetNodeTitle: TAdvTreeViewGetNodeTitleEvent read FOnGetNodeTitle write FOnGetNodeTitle;
    property OnGetNodeTitleExpanded: TAdvTreeViewGetNodeTitleExpandedEvent read FOnGetNodeExpanded write FOnGetNodeExpanded;
    property OnGetNodeSides: TAdvTreeViewGetNodeSidesEvent read FOnGetNodeSides write FOnGetNodeSides;
    property OnGetNodeRounding: TAdvTreeViewGetNodeRoundingEvent read FOnGetNodeRounding write FOnGetNodeRounding;
    property OnGetNodeData: TAdvTreeViewGetNodeDataEvent read FOnGetNodeData write FOnGetNodeData;
    property OnGetNodeTrimming: TAdvTreeViewGetNodeTrimmingEvent read FOnGetNodeTrimming write FOnGetNodeTrimming;
    property OnGetNodeWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent read FOnGetNodeWordWrapping write FOnGetNodeWordWrapping;
    property OnGetNodeTitleTrimming: TAdvTreeViewGetNodeTrimmingEvent read FOnGetNodeTitleTrimming write FOnGetNodeTitleTrimming;
    property OnGetNodeTitleWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent read FOnGetNodeTitleWordWrapping write FOnGetNodeTitleWordWrapping;
    property OnGetNodeExtraSize: TAdvTreeViewGetNodeExtraSizeEvent read FOnGetNodeExtraSize write FOnGetNodeExtraSize;
    property OnGetNodeTitleExtraSize: TAdvTreeViewGetNodeTitleExtraSizeEvent read FOnGetNodeTitleExtraSize write FOnGetNodeTitleExtraSize;
    property OnGetNodeHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent read FOnGetNodeHorizontalTextAlign write FOnGetNodeHorizontalTextAlign;
    property OnGetNodeVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent read FOnGetNodeVerticalTextAlign write FOnGetNodeVerticalTextAlign;
    property OnGetNodeTitleHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent read FOnGetNodeTitleHorizontalTextAlign write FOnGetNodeTitleHorizontalTextAlign;
    property OnGetNodeTitleVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent read FOnGetNodeTitleVerticalTextAlign write FOnGetNodeTitleVerticalTextAlign;
    property OnGetColumnTrimming: TAdvTreeViewGetColumnTrimmingEvent read FOnGetColumnTrimming write FOnGetColumnTrimming;
    property OnGetColumnWordWrapping: TAdvTreeViewGetColumnWordWrappingEvent read FOnGetColumnWordWrapping write FOnGetColumnWordWrapping;
    property OnGetColumnHorizontalTextAlign: TAdvTreeViewGetColumnHorizontalTextAlignEvent read FOnGetColumnHorizontalTextAlign write FOnGetColumnHorizontalTextAlign;
    property OnGetColumnVerticalTextAlign: TAdvTreeViewGetColumnVerticalTextAlignEvent read FOnGetColumnVerticalTextAlign write FOnGetColumnVerticalTextAlign;
    property OnGetNodeIcon: TAdvTreeViewGetNodeIconEvent read FOnGetNodeIcon write FOnGetNodeIcon;
    property OnGetNodeIconSize: TAdvTreeViewGetNodeIconSizeEvent read FOnGetNodeIconSize write FOnGetNodeIconSize;
    property OnGetNumberOfNodes: TAdvTreeViewGetNumberOfNodesEvent read FOnGetNumberOfNodes write FOnGetNumberOfNodes;
    property OnAfterDrawNodeText: TAdvTreeViewAfterDrawNodeTextEvent read FOnAfterDrawNodeText write FOnAfterDrawNodeText;
    property OnAfterDrawNodeTitle: TAdvTreeViewAfterDrawNodeTitleEvent read FOnAfterDrawNodeTitle write FOnAfterDrawNodeTitle;
    property OnAfterDrawNodeIcon: TAdvTreeViewAfterDrawNodeIconEvent read FOnAfterDrawNodeIcon write FOnAfterDrawNodeIcon;
    property OnBeforeDrawNodeIcon: TAdvTreeViewBeforeDrawNodeIconEvent read FOnBeforeDrawNodeIcon write FOnBeforeDrawNodeIcon;
    property OnAfterDrawNodeExtra: TAdvTreeViewAfterDrawNodeExtraEvent read FOnAfterDrawNodeExtra write FOnAfterDrawNodeExtra;
    property OnDrawNodeExtra: TAdvTreeViewDrawNodeExtraEvent read FOnDrawNodeExtra write FOnDrawNodeExtra;
    property OnBeforeDrawNodeExtra: TAdvTreeViewBeforeDrawNodeExtraEvent read FOnBeforeDrawNodeExtra write FOnBeforeDrawNodeExtra;
    property OnAfterDrawNodeTitleExtra: TAdvTreeViewAfterDrawNodeTitleExtraEvent read FOnAfterDrawNodeTitleExtra write FOnAfterDrawNodeTitleExtra;
    property OnDrawNodeTitleExtra: TAdvTreeViewDrawNodeTitleExtraEvent read FOnDrawNodeTitleExtra write FOnDrawNodeTitleExtra;
    property OnBeforeDrawNodeTitleExtra: TAdvTreeViewBeforeDrawNodeTitleExtraEvent read FOnBeforeDrawNodeTitleExtra write FOnBeforeDrawNodeTitleExtra;
    property OnAfterDrawNodeExpand: TAdvTreeViewAfterDrawNodeExpandEvent read FOnAfterDrawNodeExpand write FOnAfterDrawNodeExpand;
    property OnBeforeDrawNodeExpand: TAdvTreeViewBeforeDrawNodeExpandEvent read FOnBeforeDrawNodeExpand write FOnBeforeDrawNodeExpand;
    property OnAfterDrawNodeCheck: TAdvTreeViewAfterDrawNodeCheckEvent read FOnAfterDrawNodeCheck write FOnAfterDrawNodeCheck;
    property OnBeforeDrawNodeCheck: TAdvTreeViewBeforeDrawNodeCheckEvent read FOnBeforeDrawNodeCheck write FOnBeforeDrawNodeCheck;
    property OnBeforeUpdateNode: TAdvTreeViewBeforeUpdateNodeEvent read FOnBeforeUpdateNode write FOnBeforeUpdateNode;
    property OnAfterUpdateNode: TAdvTreeViewAfterUpdateNodeEvent read FOnAfterUpdateNode write FOnAfterUpdateNode;
    property OnBeforeUnCheckNode: TAdvTreeViewBeforeUnCheckNodeEvent read FOnBeforeUnCheckNode write FOnBeforeUnCheckNode;
    property OnAfterUnCheckNode: TAdvTreeViewAfterUnCheckNodeEvent read FOnAfterUnCheckNode write FOnAfterUnCheckNode;
    property OnBeforeCheckNode: TAdvTreeViewBeforeCheckNodeEvent read FOnBeforeCheckNode write FOnBeforeCheckNode;
    property OnAfterCheckNode: TAdvTreeViewAfterCheckNodeEvent read FOnAfterCheckNode write FOnAfterCheckNode;
    property OnFocusedNodeChanged: TAdvTreeViewFocusedNodeChangedEvent read FOnFocusedNodeChanged write FOnFocusedNodeChanged;
    property OnBeforeCollapseNode: TAdvTreeViewBeforeCollapseNodeEvent read FOnBeforeCollapseNode write FOnBeforeCollapseNode;
    property OnAfterCollapseNode: TAdvTreeViewAfterCollapseNodeEvent read FOnAfterCollapseNode write FOnAfterCollapseNode;
    property OnBeforeExpandNode: TAdvTreeViewBeforeExpandNodeEvent read FOnBeforeExpandNode write FOnBeforeExpandNode;
    property OnAfterExpandNode: TAdvTreeViewAfterExpandNodeEvent read FOnAfterExpandNode write FOnAfterExpandNode;
    property OnBeforeSelectAllNodes: TAdvTreeViewBeforeSelectAllNodesEvent read FOnBeforeSelectAllNodes write FOnBeforeSelectAllNodes;
    property OnAfterSelectAllNodes: TAdvTreeViewAfterSelectAllNodesEvent read FOnAfterSelectAllNodes write FOnAfterSelectAllNodes;
    property OnBeforeSelectNode: TAdvTreeViewBeforeSelectNodeEvent read FOnBeforeSelectNode write FOnBeforeSelectNode;
    property OnAfterSelectNode: TAdvTreeViewAfterSelectNodeEvent read FOnAfterSelectNode write FOnAfterSelectNode;
    property OnNodeClick: TAdvTreeViewNodeClickEvent read FOnNodeClick write FOnNodeClick;
    property OnNodeMouseLeave: TAdvTreeViewNodeMouseLeaveEvent read FOnNodeMouseLeave write FOnNodeMouseLeave;
    property OnNodeMouseEnter: TAdvTreeViewNodeMouseEnterEvent read FOnNodeMouseEnter write FOnNodeMouseEnter;
    property OnNodeDblClick: TAdvTreeViewNodeClickEvent read FOnNodeDblClick write FOnNodeDblClick;
    property OnBeforeUnSelectNode: TAdvTreeViewBeforeUnSelectNodeEvent read FOnBeforeUnSelectNode write FOnBeforeUnSelectNode;
    property OnAfterUnSelectNode: TAdvTreeViewAfterUnSelectNodeEvent read FOnAfterUnSelectNode write FOnAfterUnSelectNode;
    property OnIsNodeChecked: TAdvTreeViewIsNodeCheckedEvent read FOnIsNodeChecked write FOnIsNodeChecked;
    property OnIsNodeExpanded: TAdvTreeViewIsNodeExpandedEvent read FOnIsNodeExpanded write FOnIsNodeExpanded;
    property OnIsNodeVisible: TAdvTreeViewIsNodeVisibleEvent read FOnIsNodeVisible write FOnIsNodeVisible;
    property OnIsNodeEnabled: TAdvTreeViewIsNodeEnabledEvent read FOnIsNodeEnabled write FOnIsNodeEnabled;
    property OnGetNodeColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeColor write FOnGetNodeColor;
    property OnGetNodeStrokeColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeStrokeColor write FOnGetNodeStrokeColor;
    property OnGetNodeCheckType: TAdvTreeViewGetNodeCheckTypeEvent read FOnGetNodeCheckType write FOnGetNodeCheckType;
    property OnGetNodeSelectedColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeSelectedStrokeColor write FOnGetNodeSelectedStrokeColor;
    property OnGetNodeSelectedStrokeColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeSelectedColor write FOnGetNodeSelectedColor;
    property OnGetNodeDisabledColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeDisabledColor write FOnGetNodeDisabledColor;
    property OnGetNodeDisabledStrokeColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeDisabledStrokeColor write FOnGetNodeDisabledStrokeColor;
    property OnGetNodeTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeTextColor write FOnGetNodeTextColor;
    property OnGetNodeSelectedTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeSelectedTextColor write FOnGetNodeSelectedTextColor;
    property OnGetNodeDisabledTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeDisabledTextColor write FOnGetNodeDisabledTextColor;
    property OnGetNodeTitleColor: TAdvTreeViewGetNodeTitleColorEvent read FOnGetNodeTitleColor write FOnGetNodeTitleColor;
    property OnGetNodeSelectedTitleColor: TAdvTreeViewGetNodeTitleColorEvent read FOnGetNodeSelectedTitleColor write FOnGetNodeSelectedTitleColor;
    property OnGetNodeDisabledTitleColor: TAdvTreeViewGetNodeTitleColorEvent read FOnGetNodeDisabledTitleColor write FOnGetNodeDisabledTitleColor;
    property OnIsNodeExtended: TAdvTreeViewIsNodeExtendedEvent read FOnIsNodeExtended write FOnIsNodeExtended;
    property OnNodeAnchorClick: TAdvTreeViewNodeAnchorClickEvent read FOnNodeAnchorClick write FOnNodeAnchorClick;
    property OnNodeTitleAnchorClick: TAdvTreeViewNodeTitleAnchorClickEvent read FOnNodeTitleAnchorClick write FOnNodeTitleAnchorClick;
    property OnNodeChanged: TAdvTreeViewNodeChangedEvent read FOnNodeChanged write FOnNodeChanged;
    property Interaction: TAdvTreeViewInteraction read FInteraction write SetInteraction;
    property OnVScroll: TAdvTreeViewScrollEvent read FOnVScroll write FOnVScroll;
    property OnHScroll: TAdvTreeViewScrollEvent read FOnHScroll write FOnHScroll;
    property Version: string read GetVersion;
    property ColumnStroke: TAdvGraphicsStroke read FColumnStroke write SetColumnStroke;
    property InplaceEditorActive: Boolean read FInplaceEditorActive write FInplaceEditorActive;
    property UpdateNodeColumn: Integer read FUpdateNodeColumn write FUpdateNodeColumn;
    property OnColumnAnchorClick: TAdvTreeViewColumnAnchorClickEvent read FOnColumnAnchorClick write FOnColumnAnchorClick;
    {$IFDEF FNCLIB}
    property OnBeforeAddJSONNode: TAdvTreeViewBeforeAddJSONNodeEvent read FOnBeforeAddJSONNode write FOnBeforeAddJSONNode;
    property OnAfterAddJSONNode: TAdvTreeViewAfterAddJSONNodeEvent read FOnAfterAddJSONNode write FOnAfterAddJSONNode;
    {$ENDIF}
    property OnGetHTMLTemplateValue: TAdvTreeViewGetHTMLTemplateValueEvent read FOnGetHTMLTemplateValue write FOnGetHTMLTemplateValue;
    property OnGetHTMLTemplate: TAdvTreeViewGetHTMLTemplateEvent read FOnGetHTMLTemplate write FOnGetHTMLTemplate;

    property DragBitmap: TAdvImageEx read FDragBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure InitSample; virtual;
    procedure CutToClipboard(ATextOnly: Boolean = False); virtual;
    procedure CopyToClipboard(ATextOnly: Boolean = False); virtual;
    procedure PasteFromClipboard; virtual;
    procedure Sort(AColumn: Integer = 0; ARecurse: Boolean = False; ACaseSensitive: Boolean = True; ASortingMode: TAdvTreeViewNodesSortMode = nsmAscending; AClearNodeList: Boolean = True); {$IFDEF FMXLIB}reintroduce;{$ENDIF} virtual;
    procedure ClearSorting;

    procedure LoadFromStream(const AStream: TStream); overload;
    {$IFNDEF WEBLIB}
    procedure LoadFromFile(const AFileName: string); overload;
    procedure SaveToFile(const AFileName: string; const ATextOnly: Boolean = True); overload;
    procedure SaveToStream(const AStream: TStream; const ATextOnly: Boolean = True); overload;
    {$ENDIF}
    {$IFDEF FNCLIB}
    procedure ViewJSON(AFileName: string; AViewOptions: TAdvTreeViewViewJSONOptions = nil); virtual;
    procedure ViewJSONFromStream(AStream: TStream; AViewOptions: TAdvTreeViewViewJSONOptions = nil); virtual;
    procedure ViewJSONFromText(AText: string; AViewOptions: TAdvTreeViewViewJSONOptions = nil); virtual;
    {$ENDIF}

    procedure RemoveSelectedVirtualNodes; virtual;
    procedure RemoveSelectedNodes; virtual;
    procedure UnSelectAllNodes; virtual;
    procedure SelectAllNodes; virtual;
    procedure UnSelectAllVirtualNodes; virtual;
    procedure SelectAllVirtualNodes; virtual;
    procedure SelectNode(ANode: TAdvTreeViewNode); virtual;
    procedure SelectVirtualNodeByRow(ARow: Integer); virtual;
    procedure SelectVirtualNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure SelectNodes(ANodes: TAdvTreeViewNodeArray); virtual;
    procedure SelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray); virtual;
    procedure UnSelectNode(ANode: TAdvTreeViewNode); virtual;
    procedure UnSelectVirtualNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure UnSelectNodes(ANodes: TAdvTreeViewNodeArray); virtual;
    procedure UnSelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray); virtual;
    procedure RemoveNodeFromSelection(ANode: TAdvTreeViewVirtualNode); override;
    procedure AddNodeToSelection(ANode: TAdvTreeViewVirtualNode); override;
    procedure EditNode(ANode: TAdvTreeViewNode; AColumn: Integer); virtual;
    procedure EditVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure StopEditing; virtual;
    procedure CancelEditing; virtual;
    procedure Clear; virtual;
    function GetVisibleNodeCount: Integer; virtual;
    function GetFirstVisibleVirtualNode: TAdvTreeViewVirtualNode; virtual;
    function GetLastVisibleVirtualNode: TAdvTreeViewVirtualNode; virtual;
    function GetFirstVisibleVirtualNodeRow: Integer; virtual;
    function GetLastVisibleVirtualNodeRow: Integer; virtual;
    function GetNodesFromClipboard: TAdvTreeViewCopyNodes; virtual;
    function IsEditing: Boolean; virtual;
    function LookupNode(ALookupString: String; ARootNodesOnly: Boolean = False; AColumn: Integer = -1; ACaseSensitive: Boolean = False; AAutoSelect: Boolean = False; AAutoExpand: Boolean = False): TAdvTreeViewVirtualNode; virtual;
    function GetInplaceEditor: TAdvTreeViewInplaceEditor; virtual;
    function IsNodeSelectable(ANode: TAdvTreeViewNode): Boolean; virtual;
    function IsVirtualNodeSelectable(ANode: TAdvTreeViewVirtualNode): Boolean; virtual;
    function XYToColumnAnchor(AX, AY: Single; var AIndex: Integer): string; virtual;
    function XYToNode(X, Y: Double; OffsetX: Single = 0): TAdvTreeViewVirtualNode; virtual;
    function XYToNodeAnchor(ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeAnchor; virtual;
    function XYToNodeTitleAnchor(ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeAnchor; virtual;
    function XYToNodeExpand(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Boolean; virtual;
    function XYToNodeExtra(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Boolean; virtual;
    function XYToNodeTitleExtra(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Boolean; virtual;
    function XYToNodeCheck(ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeCheck; virtual;
    function XYToNodeTextColumn(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Integer; virtual;
    function XYToColumnSize(X, Y: Single): Integer; virtual;
    function XYToColumn(X, Y: Single; AIncludeRows: Boolean = False): Integer; virtual;
    function XYToColumnFilter(X, Y: Single): Integer; virtual;
    function XYToColumnExpand(X, Y: Single): Integer; virtual;
    function SelectedNodeCount: Integer; virtual;
    function SelectedVirtualNodeCount: Integer; virtual;
    function IsNodeSelected(ANode: TAdvTreeViewNode): Boolean; virtual;
    function IsVirtualNodeSelected(ANode: TAdvTreeViewVirtualNode): Boolean; virtual;
    property FocusedVirtualNode: TAdvTreeViewVirtualNode read GetFocusedVirtualNode write SetFocusedVirtualNode;
    property FocusedNode: TAdvTreeViewNode read GetFocusedNode write SetFocusedNode;
    property SelectedNodes[AIndex: Integer]: TAdvTreeViewNode read GetSelNode;
    property SelectedVirtualNode: TAdvTreeViewVirtualNode read GetFocusedVirtualNode write SetSelectedVirtualNode;
    property SelectedNode: TAdvTreeViewNode read GetFocusedNode write SetSelectedNode;
    property SelectedVirtualNodes[AIndex: Integer]: TAdvTreeViewVirtualNode read GetSelVirtualNode;
    property SortColumn: Integer read FSortColumn write SetSortColumn default -1;
    property DragNode: TAdvTreeViewVirtualNode read FDragNode write FDragNode;
    property DragModeStarted: Boolean read FDragModeStarted;
    property CompactMode: Boolean read FCompactMode write SetCompactMode;
    property TopRow: Integer read GetTopRow write SetTopRow;
    property BottomRow: Integer read GetBottomRow;

    property OnCustomReorder: TAdvTreeViewReorderEvent read FOnCustomReorder write FOnCustomReorder;
    property OnCustomDragOver: TAdvTreeViewDragOverEvent read FOnCustomDragOver write FOnCustomDragOver;
    property OnCustomDragDrop: TAdvTreeViewDragDropEvent read FOnCustomDragDrop write FOnCustomDragDrop;
    property OnCustomCopyToClipboard: TNotifyEvent read FOnCustomCopyToClipboard write FOnCustomCopyToClipboard;
    property OnCustomCutToClipboard: TNotifyEvent read FOnCustomCutToClipboard write FOnCustomCutToClipboard;
    property OnCustomPasteFromClipboard: TNotifyEvent read FOnCustomPasteFromClipboard write FOnCustomPasteFromClipboard;
    property OnCustomColumnSort: TAdvTreeViewColumnSortEvent read FOnCustomColumnSort write FOnCustomColumnSort;
    property OnCustomLookup: TAdvTreeViewLookupEvent read FOnCustomLookup write FOnCustomLookup;
  end;

  TAdvTreeViewPublished = class(TAdvCustomTreeView)
  published
    property Fill;
    property Stroke;
    property PictureContainer;
    property HorizontalScrollBarVisible;
    property VerticalScrollBarVisible;
    property Groups;
    property GroupsAppearance;
    property ColumnStroke;
    property Columns;
    property Nodes;
    property ColumnsAppearance;
    property NodesAppearance;
    property Interaction;
    property StretchScrollBars;
    property Version;
    property PopupMenu;
    property GlobalFont;
    property OnNeedFilterDropDownData;
    property OnFilterSelect;
    property OnBeforeDrawColumn;
    property OnAfterDrawColumn;
    property OnBeforeDrawColumnHeader;
    property OnAfterDrawColumnHeader;
    property OnBeforeDrawNodeColumn;
    property OnAfterDrawNodeColumn;
    property OnBeforeSortNodes;
    property OnAfterSortNodes;
    property OnBeforeDrawGroup;
    property OnAfterDrawGroup;
    property OnBeforeDrawNode;
    property OnAfterDrawNode;
    property OnBeforeDrawColumnText;
    property OnGetColumnText;
    property OnNodeCompare;
    property OnAfterDrawColumnText;
    property OnBeforeDrawGroupText;
    property OnGetGroupText;
    property OnAfterDrawGroupText;
    property OnBeforeDrawNodeText;
    property OnAfterDrawNodeIcon;
    property OnAfterDrawNodeExpand;
    property OnBeforeDrawNodeIcon;
    property OnBeforeDrawNodeExpand;
    property OnAfterDrawNodeCheck;
    property OnBeforeDrawNodeCheck;
    property OnGetNodeCheckType;
    property OnGetNodeText;
    property OnGetNodeData;
    property OnBeforeSizeColumn;
    property OnAfterSizeColumn;
    property OnGetNodeTrimming;
    property OnGetNodeWordWrapping;
    property OnGetNodeHorizontalTextAlign;
    property OnGetNodeVerticalTextAlign;
    property OnGetColumnTrimming;
    property OnFocusedNodeChanged;
    property OnGetColumnWordWrapping;
    property OnGetColumnHorizontalTextAlign;
    property OnGetColumnVerticalTextAlign;
    property OnBeforeUnCheckNode;
    property OnAfterUnCheckNode;
    property OnBeforeCheckNode;
    property OnAfterCheckNode;
    property OnBeforeCollapseNode;
    property OnAfterCollapseNode;
    property OnBeforeExpandNode;
    property OnAfterExpandNode;
    property OnGetNodeIcon;
    property OnGetNodeIconSize;
    property OnGetNumberOfNodes;
    property OnIsNodeExtended;
    property OnIsNodeExpanded;
    property OnAfterDrawNodeText;
    property OnIsNodeChecked;
    property OnNodeAnchorClick;
    property OnBeforeUpdateNode;
    property OnAfterUpdateNode;
    property OnBeforeSelectNode;
    property OnAfterSelectNode;
    property OnBeforeSelectAllNodes;
    property OnAfterSelectAllNodes;
    property OnNodeClick;
    property OnNodeMouseLeave;
    property OnNodeMouseEnter;
    property OnNodeDblClick;
    property OnBeforeUnSelectNode;
    property OnAfterUnSelectNode;
    property OnGetNodeSides;
    property OnBeforeOpenInplaceEditor;
    property OnCloseInplaceEditor;
    property OnAfterOpenInplaceEditor;
    property OnGetInplaceEditor;
    property OnCustomizeInplaceEditor;
    property OnGetInplaceEditorRect;
    property OnNodeChanged;
    property OnHScroll;
    property OnVScroll;
    property OnBeforeDrawColumnEmptySpace;
    property OnAfterDrawColumnEmptySpace;
    property OnBeforeDrawGroupEmptySpace;
    property OnAfterDrawGroupEmptySpace;
//    property OnIsNodeVisible;
    property OnIsNodeEnabled;
    property OnGetNodeColor;
    property OnGetNodeSelectedColor;
    property OnGetNodeDisabledColor;
    property OnGetNodeTextColor;
    property OnGetNodeSelectedTextColor;
    property OnGetNodeDisabledTextColor;
    property OnGetNodeHeight;
    property OnBeforeReorderNode;
    property OnAfterReorderNode;
    property OnBeforeDropNode;
    property OnAfterDropNode;
    property OnBeforeCutToClipboard;
    property OnBeforeCopyToClipboard;
    property OnBeforePasteFromClipboard;
    property OnAfterCutToClipboard;
    property OnAfterCopyToClipboard;
    property OnAfterPasteFromClipboard;
    property OnBeforeDrawSortIndicator;
    property OnAfterDrawSortIndicator;
    property OnColumnAnchorClick;
    {$IFDEF FNCLIB}
    property OnBeforeAddJSONNode;
    property OnAfterAddJSONNode;
    property DefaultViewJSONOptions;
    {$ENDIF}
    property OnGetHTMLTemplateValue;
    property OnGetHTMLTemplate;
  end;


{$IFDEF WEBLIB}
const
  TAdvTREEVIEWEXPAND = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAIAAAAmzuBxAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAA'+
                          'AJcEhZcwAADsMAAA7DAcdvqGQAAAAYdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuNvyMY98AAAAySURBVChT'+
                          'Y6iurcaPQCr+4wZYVHj5ekFZYEBdFUAJTISiAg4gEnBAIxVoAKoCH6qtBgAs4hCpNRg4EQAAAABJRU5ErkJggg'+
                          '==';
  TAdvTREEVIEWCOLLAPSE = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAIAAAAmzuBxAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUA'+
                            'AAAJcEhZcwAADsMAAA7DAcdvqGQAAAAYdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuNvyMY98AAAAmSURB'+
                            'VChTY6iurcaPQCr+4wYDoMLL1wsZQQRJNAMroKIKfKi2GgCBNiGJV4wrvQAAAABJRU5ErkJggg==';
  TAdvTREEVIEWEXPANDLARGE = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAIAAABL1vtsAAAAAXNSR0IArs4'+
                               'c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAAYdEVYdFNvZnR3YXJlAHBhaW50'+
                               'Lm5ldCA0LjAuNvyMY98AAABDSURBVDhPY6iuraYQUc+I/2QBehnh5esFRFAOBhg1AgFoYwREAzEIop42R'+
                               'mACZA2YYNQIBKCXEfgBtY2gBFFsRG01AN+TQqHfgmnlAAAAAElFTkSuQmCC';
  TAdvTREEVIEWCOLLAPSELARGE = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAIAAABL1vtsAAAABGdBTUEAALGP'+
                                 'C/xhBQAAAAlwSFlzAAAOwgAADsIBFShKgAAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC42/Ixj3w'+
                                 'AAADRJREFUOE9jqK6tphBRz4j/ZIFRIxBg+Bvh5euFB0EVgQEtjSAejBqBAMPTCEoQxUbUVgMAPoCGIXvOpikAAAAASUVORK5CYII=';
{$ENDIF}

implementation

uses
  SysUtils, Math, AdvTreeViewBase, AdvUtils,
  AdvGraphicsStyles, Forms, Graphics
  {$IFDEF FMXLIB}
  ,FMX.Styles.Objects
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  ,Rtti
  {$ENDIF}
  {$ENDIF}
  ;

{$R AdvTreeView.res}

type
  TAdvTreeViewGroupOpen = class(TAdvTreeViewGroup);
  TAdvTreeViewNodeOpen = class(TAdvTreeViewNode);
  TAdvTreeViewColumnOpen = class(TAdvTreeViewColumn);
  TAdvTreeViewVirtualNodeOpen = class(TAdvTreeViewVirtualNode);

function GetTickCountX: DWORD;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := ms + s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000;
end;

function AnimateDouble(var Start, Stop: Double; Delta, Margin: Double): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

{ TAdvCustomTreeView }

{$IFDEF FNCLIB}
procedure TAdvCustomTreeView.ViewJSON(AFileName: string; AViewOptions: TAdvTreeViewViewJSONOptions = nil);
var
  ms: TMemoryStream;
  procedure InternalInputFromJSON;
  begin
    ms.Position := 0;
    ViewJSONFromStream(ms, AViewOptions);
  end;
begin
  ms := TMemoryStream.Create;
  try
    {$IFDEF WEBLIB}
    ms.LoadFromFile(AFileName,
    procedure
    begin
      InternalInputFromJSON;
      ms.Free;
    end
    );
    {$ENDIF}
    {$IFNDEF WEBLIB}
    ms.LoadFromFile(AFileName);
    InternalInputFromJSON;
    {$ENDIF}
  finally
    {$IFNDEF WEBLIB}
    ms.Free;
    {$ENDIF}
  end;
end;

procedure TAdvCustomTreeView.ViewJSONFromStream(AStream: TStream; AViewOptions: TAdvTreeViewViewJSONOptions = nil);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    AStream.Position := 0;
//    SL.LoadFromStream(AStream{$IFNDEF LCLWEBLIB}, TEncoding.UTF8{$ENDIF});    //GVH Error with Encoding
    SL.LoadFromStream(AStream);
    ViewJSONFromText(SL.Text, AViewOptions);
  finally
    SL.Free;
  end;
end;

type
  TAdvTreeViewJSONType = (jtValue, jtArraySize, jtObjectSize, jtArrayIndex);

procedure TAdvCustomTreeView.ViewJSONFromText(AText: string; AViewOptions: TAdvTreeViewViewJSONOptions = nil);
var
  j : TJSONValue;
  col: TAdvTreeViewColumn;
  NameIndex, ValueIndex: Integer;

  procedure SetJSONNodeText(ANode: TAdvTreeViewNode; AName: string; ATextType: TAdvTreeViewJSONType; AJSONValue: TJSONValue; AValue: string = '');
  begin
    if ANode.Values.Count = 0 then
      ANode.Values.Add;

    case ATextType of
      jtValue:
      begin
        ANode.Text[NameIndex] := AName;
        ANode.Values[NameIndex].HTMLTemplateItems.Add('NAME=' + AName);

        ANode.Text[ValueIndex] := AValue;
        ANode.Values[ValueIndex].HTMLTemplateItems.Add('VALUE=' + AValue);

        ANode.Tag := 1;
      end;
      jtArraySize:
      begin
        ANode.Text[NameIndex] := AName + ' [' + AValue + ']';
        ANode.Values[NameIndex].HTMLTemplateItems.Add('NAME=' + AName + ' [' + AValue + ']');
        ANode.Tag := 2;
      end;
      jtObjectSize:
      begin
        ANode.Text[NameIndex] := AName + ' {' + AValue + '}';
        ANode.Values[NameIndex].HTMLTemplateItems.Add('NAME=' + AName + ' {' + AValue + '}');
        ANode.Tag := 3;
      end;
      jtArrayIndex:
      begin
        ANode.Text[NameIndex] := AViewOptions.FArrayItemPrefix + AName + AViewOptions.FArrayItemSuffix;
        ANode.Values[NameIndex].HTMLTemplateItems.Add('NAME=' + AViewOptions.FArrayItemPrefix + AName + AViewOptions.FArrayItemSuffix);
        ANode.Tag := 4;
      end;
    end;

    DoAfterAddJSONNode(ANode, AJSONValue)
  end;

  procedure ParseNode(nj: TJSONValue; ANode: TAdvTreeViewNode);
  var
    I, K, c: Integer;
    jv, njav, jav: TJSONValue;
    n, sn: TAdvTreeViewNode;
    nja, ja: TJSONArray;
    name: string;
    add: Boolean;
  begin
    if nj is TJSONArray then
    begin
      add := True;
      DoBeforeAddJSONNode(nj, add);
      if add then
      begin
        if Assigned(ANode) then
          n := ANode.Nodes.Add
        else
          n := Nodes.Add;

        nja := nj as TJSONArray;
        c := TAdvUtils.GetJSONArraySize(nja);
        SetJSONNodeText(n, name, jtArraySize, nj, IntToStr(c));
        for K := 0 to c - 1 do
        begin
          njav := TAdvUtils.GetJSONArrayItem(nja, K);

          if (njav is TJSONObject) then
          begin
            sn := n.Nodes.Add;
            SetJSONNodeText(sn, IntToStr(K), jtArrayIndex, nj);
            sn.Text[0] := IntToStr(K);
            ParseNode(njav, sn)
          end
          else
          begin
            SetJSONNodeText(n.Nodes.Add, IntToStr(K), jtValue, njav, njav.Value);
          end;
        end;
      end;
    end
    else if nj is TJSONObject then
    begin
      for I := 0 to TAdvUtils.GetJSONObjectSize(nj as TJSONObject) - 1 do
      begin
        name := '';

        name := TAdvUtils.GetJSONObjectName(nj as TJSONObject, I);

        jv := TAdvUtils.GetJSONObjectItem(nj as TJSONObject, I);

        add := True;
        DoBeforeAddJSONNode(jv, add);

        if add then
        begin
          if Assigned(ANode) then
            n := ANode.Nodes.Add
          else
            n := Nodes.Add;


          if Assigned(jv) then
          begin
            if jv is TJSONArray then
            begin
              ja := jv as TJSONArray;
              c := TAdvUtils.GetJSONArraySize(ja);
              SetJSONNodeText(n, name, jtArraySize, jv, IntToStr(c));
              for K := 0 to c - 1 do
              begin
                jav := TAdvUtils.GetJSONArrayItem(ja, K);

                if (jav is TJSONObject) then
                begin
                  sn := n.Nodes.Add;
                  SetJSONNodeText(sn, IntToStr(K), jtArrayIndex, jv);
                  ParseNode(jav, sn)
                end
                else
                begin
                  SetJSONNodeText(n.Nodes.Add, IntToStr(K), jtValue, jav, jav.Value);
                end;
              end;
            end
            else if jv is TJSONObject then
            begin
              SetJSONNodeText(n, name, jtObjectSize, jv, IntToStr(TAdvUtils.GetJSONObjectSize((jv as TJSONObject))));
              ParseNode(jv, n);
            end
            else
              SetJSONNodeText(n, name, jtValue, jv, jv.Value);
          end;
        end;
      end;
    end
    else
    begin
      if Assigned(ANode) then
        SetJSONNodeText(ANode, name, jtValue, nj, nj.Value)
      else
        SetJSONNodeText(Nodes.Add, name, jtValue, nj, nj.Value);
    end;
  end;
begin
  NameIndex := 0;
  ValueIndex := 1;

  BeginUpdate;

  if AViewOptions = nil then
    AViewOptions := DefaultViewJSONOptions;

  Nodes.Clear;
  Columns.Clear;
  col := Columns.Add;
  col.Text := 'Object';
  col.Trimming := gttCharacter;
  col.Font.Style := [TFontStyle.fsBold];

  col := Columns.Add;
  col.Text := 'Value';

  Columns[NameIndex].HTMLTemplate := AViewOptions.NameHTMLTemplate;
  Columns[ValueIndex].HTMLTemplate := AViewOptions.ValueHTMLTemplate;

  j := TAdvUtils.ParseJSON(AText);
  if Assigned(j) then
  try
    ParseNode(j, nil);
  finally
    j.Free;
  end;

  EndUpdate;

  if AViewOptions.ExpandNodes then
    ExpandAll;

  if AViewOptions.StretchColumn > -1 then
  begin
    BeginUpdate;

    ColumnsAppearance.StretchAll := False;
    ColumnsAppearance.StretchColumn := AViewOptions.StretchColumn;
    ColumnsAppearance.Stretch := False;
    AutoSizeColumn(0);
    Columns[0].Width := ColumnWidths[0];
    ColumnsAppearance.Stretch := True;

    EndUpdate;
  end;
end;
{$ENDIF}

{$IFNDEF WEBLIB}
procedure TAdvCustomTreeView.LoadFromFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

procedure TAdvCustomTreeView.LoadFromStream(const AStream: TStream);
var
  List: TStringList;
  ANode, NextNode: TAdvTreeViewNode;
  ALevel, i: Integer;
  CurrStr: string;
begin
  List := TStringList.Create;
  BeginUpdate;
  ClearNodeList;
  try
    try
      ClearNodes;
      List.LoadFromStream(AStream);
      ANode := nil;
      for i := 0 to List.Count - 1 do
      begin
        ALevel := -1;
        CurrStr := GetBufStart(List[i], ALevel);

        if ANode = nil then
          ANode := AddNode
        else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel) then
          ANode := AddNode(ANode.GetParent)
        else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel - 1) then
          ANode := AddNode(ANode)
        else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level > ALevel) then
        begin
          NextNode := ANode.GetParent;
          while Assigned(NextNode) and Assigned(NextNode.VirtualNode) and (NextNode.VirtualNode.Level > ALevel) do
            NextNode := NextNode.GetParent;
          ANode := AddNode(NextNode.GetParent);
        end;

        if Assigned(ANode) then
          ANode.LoadFromString(CurrStr);

        BuildNodeList;
      end;
    finally
      EndUpdate;
      List.Free;
    end;
  except
  end;
end;

function TAdvCustomTreeView.LookupNode(ALookupString: String; ARootNodesOnly: Boolean = False; AColumn: Integer = -1; ACaseSensitive: Boolean = False; AAutoSelect: Boolean = False; AAutoExpand: Boolean = False): TAdvTreeViewVirtualNode;
var
  v: String;
  n, fvn, lvn: TAdvTreeViewVirtualNode;
  s: String;
  sr, er, m: Integer;
  C: Integer;
  fnd: Boolean;
begin
  Result := nil;
  v := ALookupString;
  if not ACaseSensitive then
    v := Uppercase(v);

  if ARootNodesOnly then
  begin
    n := GetFirstRootVirtualNode;
    while Assigned(n) do
    begin
      fvn := GetFirstVisibleVirtualNode;
      lvn := GetLastVisibleVirtualNode;

      if (not Interaction.Lookup.VisibleNodesOnly) or (Interaction.Lookup.VisibleNodesOnly and (Assigned(fvn) and Assigned(lvn) and (n.Row <= lvn.Row) and (n.Row >= fvn.Row))) then
      begin
        if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
        begin
          s := '';
          DoGetNodeText(n, AColumn, tntmDrawing, s);

          s := TAdvUtils.HTMLStrip(s);

          if not ACaseSensitive then
            s := UpperCase(s);

          if Pos(v, s) = 1  then
          begin
            Result := n;
            Break;
          end;
        end
        else
        begin
          fnd := False;
          for C := 0 to Columns.Count - 1 do
          begin
            s := '';
            DoGetNodeText(n, C, tntmDrawing, s);

            s := TAdvUtils.HTMLStrip(s);

            if not ACaseSensitive then
              s := UpperCase(s);

            if Pos(v, s) = 1 then
            begin
              Result := n;
              fnd := True;
              Break;
            end;
          end;
          if fnd then
            Break;
        end;
      end;
      n := n.GetNextSibling;
    end;
  end
  else
  begin
    sr := 0;
    er := NodeStructure.Count - 1;

    for m := sr to er do
    begin
      n := GetNodeFromNodeStructure(m);
      if Assigned(n) then
      begin
        fvn := GetFirstVisibleVirtualNode;
        lvn := GetLastVisibleVirtualNode;
        if (not Interaction.Lookup.VisibleNodesOnly) or (Interaction.Lookup.VisibleNodesOnly and (Assigned(fvn) and Assigned(lvn) and (n.Row <= lvn.Row) and (n.Row >= fvn.Row))) then
        begin
          if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
          begin
            s := '';
            DoGetNodeText(n, AColumn, tntmDrawing, s);

            s := TAdvUtils.HTMLStrip(s);

            if not ACaseSensitive then
              s := UpperCase(s);

            if Pos(v, s) = 1  then
            begin
              Result := n;
              Break;
            end;
          end
          else
          begin
            fnd := False;
            for C := 0 to Columns.Count - 1 do
            begin
              s := '';
              DoGetNodeText(n, C, tntmDrawing, s);

              s := TAdvUtils.HTMLStrip(s);

              if not ACaseSensitive then
                s := UpperCase(s);

              if Pos(v, s) = 1 then
              begin
                Result := n;
                fnd := True;
                Break;
              end;
            end;
            if fnd then
              Break;
          end;
        end;
      end;
    end;
  end;

  if Assigned(Result) then
  begin
    if AAutoExpand then
    begin
      n := Result.GetParent;
      while Assigned(n) and (n.Level > 0) do
      begin
        if not n.Expanded then
          n.Expand;

        n := n.GetParent;
      end;

      if Assigned(n) then
        n.Expand;
    end;

    if AAutoSelect then
    begin
      if IsVirtualNodeSelectable(Result) then
        SelectVirtualNode(Result);

      ScrollToVirtualNode(Result, True);
    end;
  end;
end;

function TAdvCustomTreeView.NeedsReload(AVerticalOffset: Single): Boolean;
begin
  Result := False;
end;

procedure TAdvCustomTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FAdapter) and (Operation = opRemove) then
    FAdapter := nil;
end;

procedure TAdvCustomTreeView.OffsetNodeRects(
  ANode: TAdvTreeViewVirtualNode; AX, AY: Single; var ARect: TRectF);
var
  C: Integer;
  nr: TRectF;
  r: TRectF;
begin
  r := GetNodesRect;
  OffsetRectEx(ARect, AX + r.Left, AY + r.Top);
  for C := 0 to ColumnCount - 1 do
  begin
    if (C >= 0) and (C <= Length(ANode.TitleRects) - 1) then
    begin
      nr := ANode.TitleRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.TitleRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.TextRects) - 1) then
    begin
      nr := ANode.TextRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.TextRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.ExtraRects) - 1) then
    begin
      nr := ANode.ExtraRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.ExtraRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.TitleExtraRects) - 1) then
    begin
      nr := ANode.TitleExtraRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.TitleExtraRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.BitmapRects) - 1) then
    begin
      nr := ANode.BitmapRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.BitmapRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.ExpandRects) - 1) then
    begin
      nr := ANode.ExpandRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.ExpandRects[C] := nr;
    end;
    if (C >= 0) and (C <= Length(ANode.CheckRects) - 1) then
    begin
      nr := ANode.CheckRects[C];
      OffsetRectEx(nr, AX + r.Left, AY + r.Top);
      ANode.CheckRects[C] := nr;
    end;
  end;
end;

procedure TAdvCustomTreeView.PasteFromClipboard;
var
  s: String;
  a: TStringList;
  ANode, ANextNode: TAdvTreeViewNode;
  ALevel: Integer;
  I, off: Integer;
  CurrStr: String;
begin
  if Assigned(OnCustomPasteFromClipboard) then
    OnCustomPasteFromClipboard(Self)
  else
  begin
    if TAdvClipBoard.HasFormat(TAdvClipBoardFormat.cfText) then
    begin
      s := TAdvClipBoard.GetText;
      if (Pos(CLP_FMT, s) > 0) then
      begin
        a := TStringList.Create;
        try
          s := StringReplace(s, CLP_FMT, '', [rfReplaceAll]);
          a.Text := s;
          try
            ANode := nil;
            off := 0;
            for I := 0 to a.Count - 1 do
            begin
              ALevel := -1;
              CurrStr := GetBufStart(a[i], ALevel);

              if ANode = nil then
              begin
                ANode := AddNode(FocusedNode);
                off := ALevel;
                if Assigned(FocusedNode) then
                  off := off - FocusedNode.VirtualNode.Level;
              end
              else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel - off) then
                ANode := AddNode(ANode.GetParent)
              else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel - 1 - off) then
                ANode := AddNode(ANode)
              else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level > ALevel - off) then
              begin
                ANextNode := ANode.GetParent;
                while Assigned(ANextNode) and Assigned(ANextNode.VirtualNode) and (ANextNode.VirtualNode.Level > ALevel - off) do
                  ANextNode := ANextNode.GetParent;
                ANode := AddNode(ANextNode.GetParent);
              end;

              if Assigned(ANode) then
                ANode.LoadFromString(CurrStr);
            end;
          finally
          end;
        finally
          a.Free;
        end;
      end;
    end;
  end;
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomTreeView.SaveToFile(const AFileName: string; const ATextOnly: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, ATextOnly);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

function TAdvCustomTreeView.ColumnStretchingActive: Boolean;
begin
  Result := ColumnsAppearance.Stretch and ColumnsAppearance.StretchAll;
end;

procedure TAdvCustomTreeView.ColumnStrokeChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvCustomTreeView.ConfigureNode(AGraphics: TAdvGraphics; AIndex: Integer; ANode: TAdvTreeViewVirtualNode; var AStartY: Single): Boolean;
var
  rt: TRectF;
  bw, bh: Double;
  x, y: Double;
  rc: TRectF;
  cache: TAdvTreeViewCacheItem;
  C: Integer;
  cr, urc: TRectF;
  vs: Double;
  v: TAdvTreeViewVirtualNode;
  lvlw: Double;
  rcalc, rcalctw, rcalct: TRectF;
  strc, strt: String;
  texp: Boolean;
  cx, cw: Double;
  maxh: Double;
  st, stp: Integer;
  bmpn: TAdvBitmap;
  bmpw, bmph, expw, exph: Double;
  ww, wwt: Boolean;
  colw: Double;
  chk: TAdvTreeViewNodeCheckType;
  ext: Boolean;
  chksz: Double;
  cl: TAdvTreeViewColumn;
  expr, bmpr, txtr, titr, chkr, extr, textr: TAdvArrayTRectF;
  vl: Double;
  r: TRectF;
  sz, szt, th: Single;
  loff, toff, roff, boff: Single;
  lm, tm, rm, bm: Single;
begin
  DoGetNodeData(ANode);

  loff := 0;
  toff := 0;
  roff := 0;
  boff := 0;
  GetNodeOffset(ANode, loff, toff, roff, boff);

  lm := 0;
  tm := 0;
  rm := 0;
  bm := 0;
  GetNodeMargins(lm, tm, rm, bm);

  vs := GetVerticalScrollPosition;
  cr := GetNodesRect;
  v := ANode;
  bw := GetTotalColumnNodeWidth - rm - lm;
  lvlw := NodesAppearance.LevelIndent * v.Level;

  if NodesAppearance.ExpandColumn = GetFirstVisibleColumn then
  begin
    x := lvlw;
    bw := bw - lvlw;
  end
  else
    x := 0;

  maxh := 0;
  y := AStartY;
  bh := 0;
  x := x + lm;
  y := y + tm;

  {$IFDEF FMXWEBLIB}
  rt := RectF(Int(x), Int(y), Int(x + bw), Int(y + bh));

  if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
  begin
    if AIndex = 0 then
      rc := RectF(rt.Left + 1.5, rt.Top + 1.5, rt.Right - 1.5, rt.Bottom + 0.5)
    else if AIndex = RowCount - 1 then
      rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 1.5, rt.Bottom + 0.5)
    else
      rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 1.5, rt.Bottom + 0.5);
  end
  else
  begin
    if AIndex = 0 then
      rc := RectF(rt.Left + 1.5, rt.Top + 1.5, rt.Right - 0.5, rt.Bottom + 0.5)
    else if AIndex = RowCount - 1 then
      rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 0.5, rt.Bottom + 0.5)
    else
      rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 0.5, rt.Bottom + 0.5);
  end;
  {$ENDIF}
  {$IFDEF CMNLIB}
  rt := RectF(x, y, x + bw, y + bh);

  if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
  begin
    if AIndex = 0 then
      rc := RectF(rt.Left + 1, rt.Top + 1, rt.Right - 1, rt.Bottom)
    else if AIndex = RowCount - 1 then
      rc := RectF(rt.Left + 1, rt.Top, rt.Right - 1, rt.Bottom)
    else
      rc := RectF(rt.Left + 1, rt.Top, rt.Right - 1, rt.Bottom);
  end
  else
  begin
    if AIndex = 0 then
      rc := RectF(rt.Left + 1, rt.Top + 1, rt.Right, rt.Bottom)
    else if AIndex = RowCount - 1 then
      rc := RectF(rt.Left + 1, rt.Top, rt.Right, rt.Bottom)
    else
      rc := RectF(rt.Left + 1, rt.Top, rt.Right, rt.Bottom);
  end;
  {$ENDIF}

  ConfigureNodeRect(AIndex, ANode, rc);

  if v.Calculated then
    bh := v.Height
  else
  begin
    UpdateNodeCalculated(v, True);

    if NodesAppearance.HeightMode = tnhmVariable then
    begin
      st := GetFirstVisibleColumn;
      stp := GetLastVisibleColumn;
      ext := False;
      DoIsNodeExtended(v, ext);

      if ext then
        stp := Min(st, stp);

      for C := st to stp do
      begin
        colw := ColumnWidths[C];
        if (colw > 0) or ext then
        begin
          strc := '';
          DoGetNodeText(v, C, tntmDrawing, strc);
          strt := '';
          DoGetNodeTitle(v, C, tntmDrawing, strt);
          texp := True;
          DoGetNodeTitleExpanded(v, C, texp);

          if CompactMode then
          begin
            strt := '';
            strc := '';
            texp := False;
          end;

          cx := ColumnPositions[C];
          if C = st then
            cx := cx + rc.Left;

          if ext then
            cw := cx + bw
          else
            cw := cx + ColumnWidths[C];

          if C = st then
            cw := cw - rc.Left;

          if (NodesAppearance.ExpandColumn = C) and (C > st) then
            cx := cx + lvlw;

          cw := cw - loff - roff - rm - lm;

          if C = NodesAppearance.ExpandColumn then
          begin
            expw := NodesAppearance.ExpandWidth + ScalePaintValue(4);
            exph := NodesAppearance.ExpandHeight + ScalePaintValue(4);
            cx := cx + expw;
            if exph > maxh then
              maxh := exph;
          end;

          chk := tvntNone;
          DoGetNodeCheckType(v, C, chk);
          if chk <> tvntNone then
          begin
            {$IFDEF FMXMOBILE}
            chksz := 29;
            {$ELSE}
            {$IFDEF CMNWEBLIB}
            chksz := ScalePaintValue(FCheckStartSize);
            {$ENDIF}
            {$IFDEF FMXLIB}
            chksz := 23;
            {$ENDIF}
            {$ENDIF}

            cx := cx + Max(0, chksz{$IFDEF CMNWEBLIB} + 4{$ENDIF});
            if chksz > maxh then
              maxh := chksz;
          end;

          bmpn := nil;
          DoGetNodeIcon(v, C, False, bmpn);
          if not Assigned(bmpn) and TAdvUtils.IsHighDPIScale then
            DoGetNodeIcon(v, C, True, bmpn);

          if Assigned(bmpn) and (bmpn.Width > 0) and (bmpn.Height > 0) then
          begin
            bmpw := bmpn.Width + ScalePaintValue(4);
            bmph := bmpn.Height + ScalePaintValue(4);

            DoGetNodeIconSize(v, C, TAdvUtils.IsHighDPIScale, bmpn, bmpw, bmph);

            cx := cx + bmpw;
            if bmph > maxh then
              maxh := bmph;
          end;

          ww := False;
          wwt := False;
          if (C >= 0) and (C <= Columns.Count - 1) then
          begin
            ww := Columns[C].WordWrapping;
            wwt := Columns[C].TitleWordWrapping;
          end;

          DoGetNodeWordWrapping(v, C, ww);
          DoGetNodeTitleWordWrapping(v, C, wwt);

          sz := 0;
          DoGetNodeExtraSize(v, C, sz);

          szt := 0;
          DoGetNodeTitleExtraSize(v, C, szt);

          rcalc := RectF(cx, 0, cw - sz, 10000);
          InflateRectEx(rcalc, ScalePaintValue(-2), 0);

          rcalctw := RectF(cx, 0, cw - szt, 10000);
          InflateRectEx(rcalctw, ScalePaintValue(-2), 0);

          cl := nil;
          if (C >= 0) and (C <= ColumnCount - 1) then
            cl := Columns[C];

          if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
            AGraphics.Font.Assign(cl.Font)
          else
          begin
            if ext then
              AGraphics.Font.Assign(NodesAppearance.ExtendedFont)
            else
              AGraphics.Font.Assign(NodesAppearance.Font);
          end;

          if texp then
          begin
            rcalc := AGraphics.CalculateText(strc, rcalc, ww);
            rcalc.Bottom := rcalc.Bottom + ScalePaintValue(4);

            vl := (rcalc.Bottom - rcalc.Top);
          end
          else
            vl := 0;

          if strt <> '' then
          begin
            if Assigned(cl) and not cl.UseDefaultAppearance then
              AGraphics.Font.Assign(cl.TitleFont)
            else
              AGraphics.Font.Assign(NodesAppearance.TitleFont);

            rcalct := AGraphics.CalculateText(strt, rcalctw, wwt);
            rcalct.Bottom := rcalct.Bottom + ScalePaintValue(4);
            th := rcalct.Bottom - rcalct.Top;
          end
          else
            th := 0;

          UpdateNodeTitleHeight(v, Max(szt, th));          
          vl := vl + v.TitleHeight;

          DoGetNodeHeight(v, C, vl);

          if vl > maxh then
            maxh := vl;
        end;
      end;

      bh := Max(DefaultRowHeight, maxh);
      bh := bh + GetNodesSpacing + toff + boff;
      UpdateNodeHeight(v, Int(bh));
      TotalRowHeight := (TotalRowHeight - DefaultRowHeight) + v.Height;
    end
    else
    begin
      bh := DefaultRowHeight;
      bh := bh + GetNodesSpacing + toff + boff;
      UpdateNodeHeight(v, bh);
    end;
  end;

  AStartY := AStartY + Int(bh);
  rc.Bottom := rc.Bottom + Int(bh);
  rc.Bottom := rc.Bottom - GetNodesSpacing;

  urc := rc;
  urc.Left := urc.Left + loff;
  urc.Top := urc.Top + toff;
  urc.Right := urc.Right + roff;
  urc.Bottom := urc.Bottom + boff;

  SetLength(extr, 0);
  SetLength(textr, 0);
  SetLength(txtr, 0);
  SetLength(titr, 0);
  SetLength(bmpr, 0);
  SetLength(expr, 0);
  SetLength(chkr, 0);

  st := GetFirstVisibleColumn;
  stp := GetLastVisibleColumn;
  ext := False;
  DoIsNodeExtended(v, ext);
  if ext then
    stp := Min(st, stp);

  for C := st to stp do
  begin
    colw := GetColumnWidth(C);
    cx := GetColumnPosition(C);
    if C = st then
      cx := cx + urc.Left;

    if ext then
      cw := cx + bw
    else
      cw := cx + colw;

    if C = st then
      cw := cw - urc.Left;

    if (NodesAppearance.ExpandColumn = C) and (C > st) then
      cx := cx + lvlw;

    if ext then
      cw := cw + lvlw;

    cw := cw - rm - roff;

    expw := 0;
    exph := 0;
    if C = NodesAppearance.ExpandColumn then
    begin
      expw := NodesAppearance.ExpandWidth + ScalePaintValue(4);
      exph := NodesAppearance.ExpandHeight + ScalePaintValue(4);
    end;

    SetLength(expr, Length(expr) + 1);
    expr[Length(expr) - 1] := RectF(Int(cx + ScalePaintValue(2)), Int(urc.Top + ((urc.Bottom - urc.Top) - exph) / 2 + ScalePaintValue(2)), Int(cx + expw - ScalePaintValue(2)),
      Int(urc.Top + ((urc.Bottom - urc.Top) - exph) / 2 + exph - ScalePaintValue(2)));

    cx := cx + expw;

    chk := tvntNone;
    chksz := 0;
    DoGetNodeCheckType(v, C, chk);
    if chk <> tvntNone then
    begin
      {$IFDEF FMXMOBILE}
      chksz := 29;
      {$ELSE}
      {$IFDEF CMNWEBLIB}
      chksz := ScalePaintValue(FCheckStartSize);
      {$ENDIF}
      {$IFDEF FMXLIB}
      chksz := 23;
      {$ENDIF}
      {$ENDIF}
    end;

    SetLength(chkr, Length(chkr) + 1);
    chkr[Length(chkr) - 1] := RectF(Int(cx + ScalePaintValue(4)), Int(urc.Top + ((urc.Bottom - urc.Top) - chksz) / 2 + ScalePaintValue(2)), Int(cx + chksz), Int(urc.Top + ((urc.Bottom - urc.Top) - chksz) / 2 + chksz - ScalePaintValue(2)));

    cx := cx + Max(0, chksz{$IFDEF CMNWEBLIB} + 4{$ENDIF});

    bmpn := nil;
    DoGetNodeIcon(v, C, False, bmpn);
    if not Assigned(bmpn) and TAdvUtils.IsHighDPIScale then
      DoGetNodeIcon(v, C, True, bmpn);

    bmpw := 0;
    bmph := 0;
    if Assigned(bmpn) and (bmpn.Width > 0) and (bmpn.Height > 0) then
    begin
      bmpw := bmpn.Width + ScalePaintValue(4);
      bmph := bmpn.Height + ScalePaintValue(4);

      DoGetNodeIconSize(v, C, TAdvUtils.IsHighDPIScale, bmpn, bmpw, bmph);
    end;

    SetLength(bmpr, Length(bmpr) + 1);
    bmpr[Length(bmpr) - 1] := RectF(Int(cx + ScalePaintValue(2)), Int(urc.Top + ((urc.Bottom - urc.Top) - bmph) / 2 + ScalePaintValue(2)), Int(cx + bmpw - ScalePaintValue(2)), Int(urc.Top + ((urc.Bottom - urc.Top) - bmph) / 2 + bmph - ScalePaintValue(2)));
    cx := cx + bmpw;

    sz := 0;
    DoGetNodeExtraSize(v, C, sz);

    szt := 0;
    DoGetNodeTitleExtraSize(v, C, szt);

    if ((v.Title[C] <> '') or (v.TitleHeight > 0)) and (v.Text[C] <> '') and v.TitleExpanded[C] then
    begin
      r := RectF(cx, urc.Top, cw - szt, urc.Top + v.TitleHeight);
      SetLength(titr, Length(titr) + 1);
      titr[Length(titr) - 1] := r;

      SetLength(textr, Length(textr) + 1);
      textr[Length(textr) - 1] := RectF(r.Right, urc.Top, r.Right + szt, urc.Top + szt);

      r := RectF(cx, r.Bottom, cw - sz, urc.Bottom);
      SetLength(txtr, Length(txtr) + 1);
      txtr[Length(txtr) - 1] := r;
    end
    else
    begin
      r := RectF(cx, urc.Top + ((urc.Bottom - urc.Top) - (rc.Bottom - rc.Top)) / 2, cw - szt, urc.Top + ((urc.Bottom - urc.Top) - (rc.Bottom - rc.Top)) / 2 + (rc.Bottom - rc.Top));
      SetLength(titr, Length(titr) + 1);
      titr[Length(titr) - 1] := r;

      SetLength(textr, Length(textr) + 1);
      textr[Length(textr) - 1] := RectF(r.Right, urc.Top, r.Right + szt, urc.Top + szt);

      r := RectF(cx, urc.Top + ((urc.Bottom - urc.Top) - (rc.Bottom - rc.Top)) / 2, cw - sz, urc.Top + ((urc.Bottom - urc.Top) - (rc.Bottom - rc.Top)) / 2 + (rc.Bottom - rc.Top));
      SetLength(txtr, Length(txtr) + 1);
      txtr[Length(txtr) - 1] := r;
    end;

    SetLength(extr, Length(extr) + 1);
    extr[Length(extr) - 1] := RectF(r.Right, r.Top, r.Right + sz, r.Bottom);
  end;

  UpdateNodeExtraRects(v, extr);
  UpdateNodeTitleExtraRects(v, textr);
  UpdateNodeTextRects(v, txtr);
  UpdateNodeTitleRects(v, titr);
  UpdateNodeBitmapRects(v, bmpr);
  UpdateNodeExpandRects(v, expr);
  UpdateNodeCheckRects(v, chkr);

  cache := TAdvTreeViewCacheItem.CreateNode(rc, v);
  FNodeCache.Add(cache);
  UpdateNodeCacheReference(v, cache);

  Result := y - vs > (cr.Bottom - cr.Top);
end;

procedure TAdvCustomTreeView.ConfigureNodeRect(AIndex: Integer; ANode: TAdvTreeViewVirtualNode; var ARect: TRectF);
begin
end;

procedure TAdvCustomTreeView.CopyToClipboard(ATextOnly: Boolean = False);
var
  s: String;
  I, K: Integer;
  n: TAdvTreeViewVirtualNode;
  v: TAdvTreeViewVirtualNode;
  cl: TAdvTreeViewIntegerList;
begin
  if Assigned(OnCustomCopyToClipboard) then
    OnCustomCopyToClipboard(Self)
  else
  begin
    cl := TAdvTreeViewIntegerList.Create;
    try
      s := '';
      for I := 0 to SelectedVirtualNodeCount - 1 do
      begin
        n := SelectedVirtualNodes[I];
        if Assigned(n) and (cl.IndexOf(n.Row) = -1) then
        begin
          if Assigned(n.Node) then
            s := s + n.Node.SaveToString(ATextOnly) + ENDOFLINE;

          cl.Add(n.Row);
        end;

        if Assigned(n) then
        begin
          for K := n.Row + 1 to n.Row + n.TotalChildren do
          begin
            v := GetNodeFromNodeStructure(K);
            if Assigned(v) and Assigned(v.Node) and (cl.IndexOf(v.Row) = -1) then
            begin
              s := s + v.Node.SaveToString(ATextOnly) + ENDOFLINE;
              cl.Add(v.Row);
            end;
          end;
        end;
      end;

      if s <> '' then
        TAdvClipBoard.SetText(CLP_FMT + s);
    finally
      cl.Free;
    end;
  end;
end;

procedure TAdvCustomTreeView.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvCustomTreeView);
end;

procedure TAdvCustomTreeView.ClearFocusedNode(ANode: TAdvTreeViewVirtualNode);
begin
  if (ANode = FFocusedNode) or (ANode = nil) then
    FFocusedNode := nil;
end;

procedure TAdvCustomTreeView.RemoveNodeFromSelection(ANode: TAdvTreeViewVirtualNode);
begin
  FSelectedNodes.Remove(ANode);
end;

procedure TAdvCustomTreeView.AddNodeToSelection(ANode: TAdvTreeViewVirtualNode);
begin
  FSelectedNodes.Add(ANode);
end;

procedure TAdvCustomTreeView.DoNodeCompare(ANode1: TAdvTreeViewNode; ANode2: TAdvTreeViewNode; AColumn: Integer; var ACompareResult: Integer);
begin
  if Assigned(OnNodeCompare) then
    OnNodeCompare(Self, ANode1, ANode2, AColumn, ACompareResult);
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomTreeView.SaveToStream(const AStream: TStream; const ATextOnly: Boolean = True);
var
  ANode: TAdvTreeViewNode;
  NodeStr: string;
  Buffer: TBytes;
  {$IFDEF LCLWEBLIB}
  I: Integer;
  {$ENDIF}
begin
  if Nodes.Count > 0 then
  begin
    ANode := Nodes[0];
    while ANode <> nil do
    begin
      NodeStr := ANode.SaveToString(ATextOnly);
      NodeStr := NodeStr + EndOfLine;
      {$IFNDEF LCLWEBLIB}
      Buffer := TEncoding.Default.GetBytes(NodeStr);
      {$ENDIF}
      {$IFDEF LCLWEBLIB}
      SetLength(Buffer, Length(NodeStr));
      for I := 1 to Length(NodeStr) do
        Buffer[I - 1] := Ord(NodeStr[I]);
      {$ENDIF}

      {$IFDEF CMNLIB}
      AStream.Write(Buffer[0], Length(Buffer));
      {$ELSE}
      AStream.Write(Buffer, Length(Buffer));
      {$ENDIF}

      ANode := ANode.GetNext;
    end;
  end;
end;
{$ENDIF}

procedure TAdvCustomTreeView.Scroll(AHorizontalPos, AVerticalPos: Double);
var
  h, v: Double;
begin
  h := GetHorizontalScrollPosition;
  v := GetVerticalScrollPosition;
  inherited;
  if h <> AHorizontalPos then
    DoHScroll(AHorizontalPos);

  if v <> AVerticalPos then
    DoVScroll(AVerticalPos);
end;

function TAdvCustomTreeView.GetPreviousEditableColumn(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AColumn > 0) and (AColumn <= Columns.Count - 1) then
  begin
    for I := AColumn - 1 downto 0 do
    begin
      if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.GetNextEditableColumn(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AColumn >= 0) and (AColumn < Columns.Count - 1) then
  begin
    for I := AColumn + 1 to Columns.Count - 1 do
    begin
      if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.GetBottomRow: Integer;
begin
  Result := GetLastVisibleVirtualNodeRow;
end;

function TAdvCustomTreeView.GetBufStart(ABuffer: string; var ALevel: Integer): string;
var
  Pos: Integer;
begin
  Pos := 1;
  ALevel := 0;
  while CharInArray(ABuffer[Pos{$IFDEF ZEROSTRINGINDEX}-1{$ENDIF}], [' ', #9]) do
  begin
    Inc(Pos);
    Inc(ALevel);
  end;
  Result := Copy(ABuffer, Pos, Length(ABuffer) - Pos + 1);
end;

function TAdvCustomTreeView.GetFirstEditableColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.GetFirstVisibleCacheItem: TAdvTreeViewCacheItem;
begin
  Result := nil;
  if FNodeDisplay.Count > 0 then
    Result := FNodeDisplay[0];
end;

function TAdvCustomTreeView.GetFirstVisibleColumn: Integer;
var
  r: TRectF;
  I: Integer;
begin
  Result := 0;
  if not ColumnsAppearance.OptimizedColumnDisplay then
    Exit;

  r := GetNodesRect;
  for I := 0 to Columns.Count - 1 do
  begin
    if ColumnPositions[I] + ColumnWidths[I] >= r.Left + GetHorizontalScrollPosition then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TAdvCustomTreeView.GetFirstVisibleVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if FNodeDisplay.Count > 0 then
    Result := FNodeDisplay[0].Node;
end;

function TAdvCustomTreeView.GetFirstVisibleVirtualNodeRow: Integer;
var
  n: TAdvTreeViewVirtualNode;
begin
  n := GetFirstVisibleVirtualNode;
  Result := 0;
  if Assigned(n) then
    Result := n.Row;  
end;

function TAdvCustomTreeView.GetLastEditableColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Columns.Count - 1 downto 0 do
  begin
    if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.GetLastVisibleColumn: Integer;
var
  r: TRectF;
  I: Integer;
begin
  Result := Columns.Count - 1;

  if not ColumnsAppearance.OptimizedColumnDisplay then
    Exit;

  r := GetNodesRect;
  for I := Result downto 0 do
  begin
    if ColumnPositions[I] <= r.Right + GetHorizontalScrollPosition then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TAdvCustomTreeView.GetLastVisibleVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if FNodeDisplay.Count > 0 then
    Result := FNodeDisplay[FNodeDisplay.Count - 1].Node;
end;

function TAdvCustomTreeView.GetLastVisibleVirtualNodeRow: Integer;
var
  n: TAdvTreeViewVirtualNode;
begin
  n := GetLastVisibleVirtualNode;
  Result := 0;
  if Assigned(n) then
    Result := n.Row;
end;

function TAdvCustomTreeView.GetFocusedNode: TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := FFocusedNode;
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvCustomTreeView.GetFocusedVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := FFocusedNode;
end;

function TAdvCustomTreeView.GetSelectedVirtualNodeRow: Integer;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := -1;
  n := SelectedVirtualNode;
  if Assigned(n) then
    Result := n.Row;  
end;

function TAdvCustomTreeView.GetSelNode(AIndex: Integer): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  v := SelectedVirtualNodes[AIndex];
  if Assigned(v) then
    Result := v.Node;
end;

function TAdvCustomTreeView.GetSelVirtualNode(
  AIndex: Integer): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= SelectedVirtualNodeCount - 1) then
    Result := FSelectedNodes[AIndex];
end;

function TAdvCustomTreeView.GetTopRow: Integer;
begin
  Result := GetFirstVisibleVirtualNodeRow;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomTreeView.DoInplaceEditorTimer(Sender: TObject);
begin
  if Assigned(FInplaceEditor) then
  begin
    FInplaceEditor.DisposeOf;
    FInplaceEditor := nil;
  end;

  FInplaceEditorTimer.Enabled := False;
end;

procedure TAdvCustomTreeView.ScrollBarChanged(Sender: TObject);
begin
  UpdateScrollBars(True, False);
  FScrollBarTimer.Enabled := False;
end;
{$ENDIF}

procedure TAdvCustomTreeView.DoAfterDrawGroup(AGraphics: TAdvGraphics; ARect: TRectF;
  AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
  if Assigned(OnAfterDrawGroup) then
    OnAfterDrawGroup(Self, AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind);
end;

procedure TAdvCustomTreeView.DoAfterDrawGroupEmptySpace(AGraphics: TAdvGraphics;
  ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace);
begin
  if Assigned(OnAfterDrawGroupEmptySpace) then
    OnAfterDrawGroupEmptySpace(Self, AGraphics, ARect, ASpace);
end;

procedure TAdvCustomTreeView.DoAfterDrawGroupText(AGraphics: TAdvGraphics;
  ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String);
begin
  if Assigned(OnAfterDrawGroupText) then
    OnAfterDrawGroupText(Self, AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoAfterDrawNode(AGraphics: TAdvGraphics; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDrawNode) then
    OnAfterDrawNode(Self, AGraphics, ARect, ANode);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeCheck(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode
  ;ACheck: TAdvBitmap);
begin
  if Assigned(OnAfterDrawNodeCheck) then
    OnAfterDrawNodeCheck(Self, AGraphics, ARect, AColumn, ANode, ACheck);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeExpand(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AExpand: TAdvBitmap);
begin
  if Assigned(OnAfterDrawNodeExpand) then
    OnAfterDrawNodeExpand(Self, AGraphics, ARect, AColumn, ANode, AExpand);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeExtra(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDrawNodeExtra) then
    OnAfterDrawNodeExtra(Self, AGraphics, ARect, AColumn, ANode);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeIcon(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AIcon: TAdvBitmap);
begin
  if Assigned(OnAfterDrawNodeIcon) then
    OnAfterDrawNodeIcon(Self, AGraphics, ARect, AColumn, ANode, AIcon);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String);
begin
  if Assigned(OnAfterDrawNodeText) then
    OnAfterDrawNodeText(Self, AGraphics, ARect, AColumn, ANode, AText);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeTitle(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  ATitle: String);
begin
  if Assigned(OnAfterDrawNodeTitle) then
    OnAfterDrawNodeTitle(Self, AGraphics, ARect, AColumn, ANode, ATitle);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeTitleExtra(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDrawNodeTitleExtra) then
    OnAfterDrawNodeTitleExtra(Self, AGraphics, ARect, AColumn, ANode);
end;

procedure TAdvCustomTreeView.DoAfterDrawSortIndicator(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer;
  ASortKind: TAdvTreeViewNodesSortKind);
begin
  if Assigned(OnAfterDrawSortIndicator) then
    OnAfterDrawSortIndicator(Self, AGraphics, ARect, AColumn, ASortIndex, ASortKind);
end;

procedure TAdvCustomTreeView.DoAfterDropNode(AFromNode,
  AToNode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDropNode) then
    OnAfterDropNode(Self, AFromNode, AToNode);
end;

procedure TAdvCustomTreeView.DoAfterExpandNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterExpandNode) then
    OnAfterExpandNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterOpenInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: TRectF);
begin
  if Assigned(OnAfterOpenInplaceEditor) then
    OnAfterOpenInplaceEditor(Self, ANode, AColumn, AInplaceEditor, AInplaceEditorRect);
end;

procedure TAdvCustomTreeView.DoAfterPasteFromClipboard;
begin
  if Assigned(OnAfterPasteFromClipboard) then
    OnAfterPasteFromClipboard(Self);
end;

procedure TAdvCustomTreeView.DoAfterReorderNode(AFromNode,
  AToNode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterReorderNode) then
    OnAfterReorderNode(Self, AFromNode, AToNode);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeColumn(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer);
begin
  if Assigned(OnAfterDrawNodeColumn) then
    OnAfterDrawNodeColumn(Self, AGraphics, ARect, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumn(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer);
begin
  if Assigned(OnAfterDrawColumn) then
    OnAfterDrawColumn(Self, AGraphics, ARect, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnHeader(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
  if Assigned(OnAfterDrawColumnHeader) then
    OnAfterDrawColumnHeader(Self, AGraphics, ARect, AColumn, AKind);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnEmptySpace(AGraphics: TAdvGraphics;
  ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace);
begin
  if Assigned(OnAfterDrawColumnEmptySpace) then
    OnAfterDrawColumnEmptySpace(Self, AGraphics, ARect, ASpace);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String);
begin
  if Assigned(OnAfterDrawColumnText) then
    OnAfterDrawColumnText(Self, AGraphics, ARect, AColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoAfterUnCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  if Assigned(OnAfterUnCheckNode) then
    OnAfterUnCheckNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterUnSelectNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterUnSelectNode) then
    OnAfterUnSelectNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  if Assigned(OnAfterUpdateNode) then
    OnAfterUpdateNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroup(AGraphics: TAdvGraphics; ARect: TRectF;
  AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawGroup) then
    OnBeforeDrawGroup(Self, AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroupEmptySpace(AGraphics: TAdvGraphics;
  ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow,
  ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawGroupEmptySpace) then
    OnBeforeDrawGroupEmptySpace(Self, AGraphics, ARect, ASpace, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroupText(AGraphics: TAdvGraphics;
  ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawGroupText) then
    OnBeforeDrawGroupText(Self, AGraphics, ARect, AGroup, AStartColumn, AEndColumn, AKind, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNode(AGraphics: TAdvGraphics; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawNode) then
    OnBeforeDrawNode(Self, AGraphics, ARect, ANode, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeCheck(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  ACheck: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeCheck) then
    OnBeforeDrawNodeCheck(Self, AGraphics, ARect, AColumn, ANode, ACheck, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeExpand(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AExpand: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeExpand) then
    OnBeforeDrawNodeExpand(Self, AGraphics, ARect, AColumn, ANode, AExpand, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeExtra(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeExtra) then
    OnBeforeDrawNodeExtra(Self, AGraphics, ARect, AColumn, ANode, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeTitleExtra(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeTitleExtra) then
    OnBeforeDrawNodeTitleExtra(Self, AGraphics, ARect, AColumn, ANode, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeIcon(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AIcon: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeIcon) then
    OnBeforeDrawNodeIcon(Self, AGraphics, ARect, AColumn, ANode, AIcon, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeText) then
    OnBeforeDrawNodeText(Self, AGraphics, ARect, AColumn, ANode, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeTitle(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; ATitle: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeTitle) then
    OnBeforeDrawNodeTitle(Self, AGraphics, ARect, AColumn, ANode, ATitle, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawSortIndicator(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; ASortIndex: Integer;
  ASortKind: TAdvTreeViewNodesSortKind; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawSortIndicator) then
    OnBeforeDrawSortIndicator(Self, AGraphics, ARect, AColumn, ASortIndex, ASortKind, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDropNode(AFromNode,
  AToNode: TAdvTreeViewVirtualNode; var ACanDrop: Boolean);
begin
  if Assigned(OnBeforeDropNode) then
    OnBeforeDropNode(Self, AFromNode, AToNode, ACanDrop);
end;

{$IFDEF FNCLIB}
procedure TAdvCustomTreeView.DoBeforeAddJSONNode(AJSONValue: TJSONValue; var AAddNode: Boolean);
begin
  if Assigned(OnBeforeAddJSONNode) then
    OnBeforeAddJSONNode(Self, AJSONValue, AAddNode);
end;
{$ENDIF}

procedure TAdvCustomTreeView.DoBeforeCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean);
begin
  if Assigned(OnBeforeCheckNode) then
    OnBeforeCheckNode(Self, ANode, AColumn, ACanCheck);
end;

procedure TAdvCustomTreeView.DoBeforeCollapseNode(
  ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean);
begin
  if Assigned(OnBeforeCollapseNode) then
    OnBeforeCollapseNode(Self, ANode, ACanCollapse);
end;

procedure TAdvCustomTreeView.DoBeforeCopyToClipboard(var ACanCopy: Boolean);
begin
  if Assigned(OnBeforeCopyToClipboard) then
    OnBeforeCopyToClipboard(Self, ACanCopy);
end;

procedure TAdvCustomTreeView.DoBeforeCutToClipboard(var ACanCut: Boolean);
begin
  if Assigned(OnBeforeCutToClipboard) then
    OnBeforeCutToClipboard(Self, ACanCut);
end;

procedure TAdvCustomTreeView.DoBeforeExpandNode(
  ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean);
begin
  if Assigned(OnBeforeExpandNode) then
    OnBeforeExpandNode(Self, ANode, ACanExpand);
end;

procedure TAdvCustomTreeView.DoBeforeOpenInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean);
begin
  if Assigned(OnBeforeOpenInplaceEditor) then
    OnBeforeOpenInplaceEditor(Self, ANode, AColumn, ACanOpen);
end;

procedure TAdvCustomTreeView.DoBeforePasteFromClipboard(
  var ACanPaste: Boolean);
begin
  if Assigned(OnBeforePasteFromClipboard) then
    OnBeforePasteFromClipboard(Self, ACanPaste);
end;

procedure TAdvCustomTreeView.DoBeforeReorderNode(AFromNode,
  AToNode: TAdvTreeViewVirtualNode; var ACanReorder: Boolean);
begin
  if Assigned(OnBeforeReorderNode) then
    OnBeforeReorderNode(Self, AFromNode, AToNode, ACanReorder);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumn(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumn) then
    OnBeforeDrawColumn(Self, AGraphics, ARect, AColumn, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeColumn(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawNodeColumn) then
    OnBeforeDrawNodeColumn(Self, AGraphics, ARect, AColumn, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnHeader(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumnHeader) then
    OnBeforeDrawColumnHeader(Self, AGraphics, ARect, AColumn, AKind, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeSizeColumn(AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean);
begin
  if Assigned(OnBeforeSizeColumn) then
    OnBeforeSizeColumn(Self, AColumn, AColumnSize, ANewColumnSize, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeSortNodes(ASortColumn: Integer;
  ASortMode: TAdvTreeViewNodesSortMode; var ACanSort: Boolean);
begin
  if Assigned(OnBeforeSortNodes) then
    OnBeforeSortNodes(Self, ASortColumn, ASortMode, ACanSort);
end;

procedure TAdvCustomTreeView.DoAfterSizeColumn(AColumn: Integer; AColumnSize: Double);
begin
  if Assigned(OnAfterSizeColumn) then
    OnAfterSizeColumn(Self, AColumn, AColumnSize);
end;

procedure TAdvCustomTreeView.DoAfterSortNodes(ASortColumn: Integer;
  ASortMode: TAdvTreeViewNodesSortMode);
begin
  if Assigned(OnAfterSortNodes) then
    OnAfterSortNodes(Self, ASortColumn, ASortMode);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnEmptySpace(AGraphics: TAdvGraphics;
  ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow,
  ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumnEmptySpace) then
    OnBeforeDrawColumnEmptySpace(Self, AGraphics, ARect, ASpace, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawColumnText) then
    OnBeforeDrawColumnText(Self, AGraphics, ARect, AColumn, AKind, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoNodeClick(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeClick) then
    OnNodeClick(Self, ANode);
end;

procedure TAdvCustomTreeView.DoNodeDblClick(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeDblClick) then
    OnNodeDblClick(Self, ANode);
end;

procedure TAdvCustomTreeView.DoNodeMouseLeave(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeMouseLeave) then
    OnNodeMouseLeave(Self, ANode);
end;

procedure TAdvCustomTreeView.DoNodeMouseEnter(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeMouseEnter) then
    OnNodeMouseEnter(Self, ANode);
end;

procedure TAdvCustomTreeView.DoNodeTitleAnchorClick(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String);
begin
  if Assigned(OnNodeTitleAnchorClick) then
    OnNodeTitleAnchorClick(Self, ANode, AColumn, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTreeView.DoUpdateNodeText(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AText: String);
begin

end;

procedure TAdvCustomTreeView.DoUpdateNodeTitle(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ATitle: String);
begin

end;

procedure TAdvCustomTreeView.DoBeforeSelectNode(ANode: TAdvTreeViewVirtualNode;
  var ACanSelect: Boolean);
begin
  if Assigned(OnBeforeSelectNode) then
    OnBeforeSelectNode(Self, ANode, ACanSelect);
end;

procedure TAdvCustomTreeView.DoBeforeSelectAllNodes(var ACanSelect: Boolean);
begin
  if Assigned(OnBeforeSelectAllNodes) then
    OnBeforeSelectAllNodes(Self, ACanSelect);
end;

procedure TAdvCustomTreeView.DoBeforeUnCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean);
begin
  if Assigned(OnBeforeUnCheckNode) then
    OnBeforeUnCheckNode(Self, ANode, AColumn, ACanUnCheck);
end;

procedure TAdvCustomTreeView.DoBeforeUnSelectNode(
  ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean);
begin
  if Assigned(OnBeforeUnSelectNode) then
    OnBeforeUnSelectNode(Self, ANode, ACanUnSelect);
end;

procedure TAdvCustomTreeView.DoBeforeUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String;
  var ACanUpdate: Boolean);
begin
  if Assigned(OnBeforeUpdateNode) then
    OnBeforeUpdateNode(Self, ANode, AColumn, AText, ACanUpdate);
end;

procedure TAdvCustomTreeView.DoCloseInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean;
  var ACanClose: Boolean);
begin
  if Assigned(OnCloseInplaceEditor) then
    OnCloseInplaceEditor(Self, ANode, AColumn, AInplaceEditor, ACancelled, ACanClose);
end;

procedure TAdvCustomTreeView.DoColumnAnchorClick(AColumn: Integer;
  AAnchor: String);
begin
  if Assigned(OnColumnAnchorClick) then
    OnColumnAnchorClick(Self, AColumn, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTreeView.DoCustomizeInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  AInplaceEditor: TAdvTreeViewInplaceEditor);
begin
  if Assigned(OnCustomizeInplaceEditor) then
    OnCustomizeInplaceEditor(Self, ANode, AColumn, AInplaceEditor);
end;

procedure TAdvCustomTreeView.DoDrawNodeExtra(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnDrawNodeExtra) then
    OnDrawNodeExtra(Self, AGraphics, ARect, AColumn, ANode);
end;

procedure TAdvCustomTreeView.DoDrawNodeTitleExtra(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnDrawNodeTitleExtra) then
    OnDrawNodeTitleExtra(Self, AGraphics, ARect, AColumn, ANode);
end;

procedure TAdvCustomTreeView.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvCustomTreeView.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TAdvCustomTreeView.DoFilterSelect(AColumn: integer;
  var ACondition: string);
begin
  if Assigned(OnFilterSelect) then
    OnFilterSelect(Self, AColumn, ACondition);
end;

procedure TAdvCustomTreeView.DoFocusedNodeChanged(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnFocusedNodeChanged) then
    OnFocusedNodeChanged(Self, ANode);
end;

{$IFDEF FNCLIB}
procedure TAdvCustomTreeView.DoAfterAddJSONNode(ANode: TAdvTreeViewNode; AJSONValue: TJSONValue);
begin
  if Assigned(OnAfterAddJSONNode) then
    OnAfterAddJSONNode(Self, ANode, AJSONValue);
end;
{$ENDIF}

procedure TAdvCustomTreeView.DoAfterCheckNode(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer);
begin
  if Assigned(OnAfterCheckNode) then
    OnAfterCheckNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterCollapseNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterCollapseNode) then
    OnAfterCollapseNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterCopyToClipboard;
begin
  if Assigned(OnAfterCopyToClipboard) then
    OnAfterCopyToClipboard(Self);
end;

procedure TAdvCustomTreeView.DoAfterCutToClipboard;
begin
  if Assigned(OnAfterCutToClipboard) then
    OnAfterCutToClipboard(Self);
end;

procedure TAdvCustomTreeView.DoGetGroupText(AGroup: Integer;
  AKind: TAdvTreeViewCacheItemKind; var AText: String);
begin
  if Assigned(OnGetGroupText) then
    OnGetGroupText(Self, AGroup, AKind, AText);
end;

procedure TAdvCustomTreeView.DoGetHTMLTemplate(ANodeValue: TAdvTreeViewNodeValue; AColumnIndex: Integer; var AHTMLTemplate: string);
begin
  if Assigned(OnGetHTMLTemplate) then
    OnGetHTMLTemplate(Self, ANodeValue, AColumnIndex, AHTMLTemplate);
end;

procedure TAdvCustomTreeView.DoGetHTMLTemplateValue(ANodeValue: TAdvTreeViewNodeValue; AName: string; var AValue: string);
begin
  if Assigned(OnGetHTMLTemplateValue) then
    OnGetHTMLTemplateValue(Self, ANodeValue, AName, AValue);
end;

procedure TAdvCustomTreeView.DoGetInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; {$IFDEF FMXLIB}var ATransparent: Boolean;{$ENDIF}
  var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass);
begin
  if Assigned(OnGetInplaceEditor) then
    OnGetInplaceEditor(Self, ANode, AColumn, {$IFDEF FMXLIB}ATransparent, {$ENDIF}AInplaceEditorClass);
end;

procedure TAdvCustomTreeView.DoGetNodeCheckType(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ACheckType: TAdvTreeViewNodeCheckType);
begin
  inherited;
  if Assigned(OnGetNodeCheckType) then
    OnGetNodeCheckType(Self, ANode, AColumn, ACheckType);
end;

procedure TAdvCustomTreeView.DoGetNodeHeight(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double);
begin
  inherited;
  if Assigned(OnGetNodeHeight) then
    OnGetNodeHeight(Self, ANode, AColumn, AHeight);
end;

procedure TAdvCustomTreeView.DoGetNodeColor(ANode: TAdvTreeViewVirtualNode;
  var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeColor) then
    OnGetNodeColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeStrokeColor(ANode: TAdvTreeViewVirtualNode;
  var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeStrokeColor) then
    OnGetNodeStrokeColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledColor) then
    OnGetNodeDisabledColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledStrokeColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledStrokeColor) then
    OnGetNodeDisabledStrokeColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledTextColor) then
    OnGetNodeDisabledTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledTitleColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATitleColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledTitleColor) then
    OnGetNodeDisabledTitleColor(Self, ANode, AColumn, ATitleColor);
end;

procedure TAdvCustomTreeView.DoGetNodeExtraSize(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single);
begin
  inherited;
  if Assigned(OnGetNodeExtraSize) then
    OnGetNodeExtraSize(Self, ANode, AColumn, AExtraSize);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleExtraSize(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleExtraSize: Single);
begin
  inherited;
  if Assigned(OnGetNodeTitleExtraSize) then
    OnGetNodeTitleExtraSize(Self, ANode, AColumn, ATitleExtraSize);
end;

procedure TAdvCustomTreeView.DoGetNodeHorizontalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
begin
  inherited;
  if Assigned(OnGetNodeHorizontalTextAlign) then
    OnGetNodeHorizontalTextAlign(Self, ANode, AColumn, AHorizontalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; ALarge: Boolean; var AIcon: TAdvBitmap);
begin
  inherited;
  if Assigned(OnGetNodeIcon) then
    OnGetNodeIcon(Self, ANode, AColumn, ALarge, AIcon);
end;

procedure TAdvCustomTreeView.DoGetNodeIconSize(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; ALarge: Boolean; AIcon: TAdvBitmap; var AIconWidth: Double; var AIconHeight: Double);
begin
  inherited;
  if Assigned(OnGetNodeIconSize) then
    OnGetNodeIconSize(Self, ANode, AColumn, ALarge, AIcon, AIconWidth, AIconHeight);
end;

procedure TAdvCustomTreeView.DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode;
  var AExtended: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeExtended) then
    OnIsNodeExtended(Self, ANode, AExtended);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedColor) then
    OnGetNodeSelectedColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedStrokeColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedStrokeColor) then
    OnGetNodeSelectedColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedTextColor) then
    OnGetNodeSelectedTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedTitleColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATitleColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedTitleColor) then
    OnGetNodeSelectedTitleColor(Self, ANode, AColumn, ATitleColor);
end;

procedure TAdvCustomTreeView.DoGetNodeSides(ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides);
begin
  inherited;
  if Assigned(OnGetNodeSides) then
    OnGetNodeSides(Self, ANode, ASides);
end;

procedure TAdvCustomTreeView.DoGetNodeRounding(ANode: TAdvTreeViewVirtualNode; var ARounding: Integer; var ACorners: TAdvGraphicsCorners);
begin
  inherited;
  if Assigned(OnGetNodeRounding) then
    OnGetNodeRounding(Self, ANode, ARounding, ACorners);
end;

procedure TAdvCustomTreeView.DoGetNodeData(ANode: TAdvTreeViewVirtualNode);
begin
  inherited;
  if Assigned(OnGetNodeData) then
    OnGetNodeData(Self, ANode);
end;

procedure TAdvCustomTreeView.DoGetNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String);
begin
  inherited;
  if Assigned(Adapter) then
    Adapter.GetNodeText(AColumn, ANode, AText);

  if Assigned(OnGetNodeText) then
    OnGetNodeText(Self, ANode, AColumn, AMode, AText);
end;

procedure TAdvCustomTreeView.DoGetNodeTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeTextColor) then
    OnGetNodeTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeTrimming(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATrimming: TAdvGraphicsTextTrimming);
begin
  inherited;
  if Assigned(OnGetNodeTrimming) then
    OnGetNodeTrimming(Self, ANode, AColumn, ATrimming);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleExpanded(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExpanded: Boolean);
begin
  inherited;
  if Assigned(OnGetNodeTitleExpanded) then
    OnGetNodeTitleExpanded(Self, ANode, AColumn, AExpanded);
end;

procedure TAdvCustomTreeView.DoGetNodeTitle(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  AMode: TAdvTreeViewNodeTextMode; var ATitle: String);
begin
  inherited;
  if Assigned(OnGetNodeTitle) then
    OnGetNodeTitle(Self, ANode, AColumn, AMode, ATitle);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATitleColor: TAdvGraphicsColor);
begin
  inherited;
  if Assigned(OnGetNodeTitleColor) then
    OnGetNodeTitleColor(Self, ANode, AColumn, ATitleColor);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleHorizontalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
begin
  inherited;
  if Assigned(OnGetNodeTitleHorizontalTextAlign) then
    OnGetNodeTitleHorizontalTextAlign(Self, ANode, AColumn, AHorizontalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleTrimming(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATrimming: TAdvGraphicsTextTrimming);
begin
  inherited;
  if Assigned(OnGetNodeTitleTrimming) then
    OnGetNodeTitleTrimming(Self, ANode, AColumn, ATrimming);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleVerticalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AVerticalTextAlign: TAdvGraphicsTextAlign);
begin
  inherited;
  if Assigned(OnGetNodeTitleVerticalTextAlign) then
    OnGetNodeTitleVerticalTextAlign(Self, ANode, AColumn, AVerticalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeTitleWordWrapping(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AWordWrapping: Boolean);
begin
  inherited;
  if Assigned(OnGetNodeTitleWordWrapping) then
    OnGetNodeTitleWordWrapping(Self, ANode, AColumn, AWordWrapping);
end;

procedure TAdvCustomTreeView.DoGetNodeVerticalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AVerticalTextAlign: TAdvGraphicsTextAlign);
begin
  if Assigned(OnGetNodeVerticalTextAlign) then
    OnGetNodeVerticalTextAlign(Self, ANode, AColumn, AVerticalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeWordWrapping(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean);
begin
  inherited;
  if Assigned(OnGetNodeWordWrapping) then
    OnGetNodeWordWrapping(Self, ANode, AColumn, AWordWrapping);
end;

procedure TAdvCustomTreeView.DoGetColumnTrimming(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var ATrimming: TAdvGraphicsTextTrimming);
begin
  inherited;
  if Assigned(OnGetColumnTrimming) then
    OnGetColumnTrimming(Self, AColumn, AKind, ATrimming);
end;

procedure TAdvCustomTreeView.DoGetColumnVerticalTextAlign(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AVerticalTextAlign: TAdvGraphicsTextAlign);
begin
  if Assigned(OnGetColumnVerticalTextAlign) then
    OnGetColumnVerticalTextAlign(Self, AColumn, AKind, AVerticalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetColumnHorizontalTextAlign(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
begin
  if Assigned(OnGetColumnHorizontalTextAlign) then
    OnGetColumnHorizontalTextAlign(Self, AColumn, AKind, AHorizontalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetColumnWordWrapping(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean);
begin
  inherited;
  if Assigned(OnGetColumnWordWrapping) then
    OnGetColumnWordWrapping(Self, AColumn, AKind, AWordWrapping);
end;

procedure TAdvCustomTreeView.DoGetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer);
begin
  inherited;
  if Assigned(Adapter) then
    Adapter.GetNumberOfNodes(ANode, ANumberOfNodes);

  if Assigned(OnGetNumberOfNodes) then
  begin
    UpdateCount := UpdateCount + 1;
    NodeListBuild := False;
    Nodes.Clear;
    UpdateCount := UpdateCount - 1;
    OnGetNumberOfNodes(Self, ANode, ANumberOfNodes);
  end;
end;

procedure TAdvCustomTreeView.DoGetColumnText(AColumn: Integer;
  AKind: TAdvTreeViewCacheItemKind; var AText: String);
begin
  if Assigned(OnGetColumnText) then
    OnGetColumnText(Self, AColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoHScroll(APosition: Single);
begin
  if Assigned(OnHScroll) then
    OnHScroll(Self, APosition);
end;

procedure TAdvCustomTreeView.DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AChecked: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeChecked) then
    OnIsNodeChecked(Self, ANode, AColumn, AChecked);
end;

procedure TAdvCustomTreeView.DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode;
  var AEnabled: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeEnabled) then
    OnIsNodeEnabled(Self, ANode, AEnabled);
end;

procedure TAdvCustomTreeView.DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeExpanded) then
    OnIsNodeExpanded(Self, ANode, AExpanded);
end;

procedure TAdvCustomTreeView.DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode;
  var AVisible: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeVisible) then
    OnIsNodeVisible(ANode, ANode, AVisible);
end;

procedure TAdvCustomTreeView.DoNeedFilterDropDownData(
  AColumn: Integer; AValues: TStrings);
begin
  if Assigned(OnNeedFilterDropDownData) then
    OnNeedFilterDropDownData(Self, AColumn, AValues);
end;

procedure TAdvCustomTreeView.DoNodeAnchorClick(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String);
begin
  if Assigned(OnNodeAnchorClick) then
    OnNodeAnchorClick(Self, ANode, AColumn, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTreeView.DoNodeChanged(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeChanged) then
    OnNodeChanged(Self, ANode);
end;

procedure TAdvCustomTreeView.DoVScroll(APosition: Single);
begin
  if Assigned(Adapter) then
    Adapter.ScrollTreeView(GetFirstVisibleVirtualNodeRow - FOldTopRow);
  if Assigned(OnVScroll) then
    OnVScroll(Self, APosition);
end;

procedure TAdvCustomTreeView.DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(Adapter) then
    Adapter.SelectNode(ANode);

  if Assigned(OnAfterSelectNode) then
    OnAfterSelectNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterSelectAllNodes;
begin
  if Assigned(OnAfterSelectAllNodes) then
    OnAfterSelectAllNodes(Self);
end;

procedure TAdvCustomTreeView.DragTime(Sender: TObject);
var
  contentr: TRectF;
begin
  if FReorderMode or ((FDragMode and FDragModeStarted) and AlternativeDragDrop) then
  begin
    contentr := GetNodesRect;

    if (FY >= contentr.Bottom) then
    begin
      FDragRow := FDragRow + 1;
      ScrollToVirtualNode(GetNodeForRow(FDragRow));
    end
    else if FY <= contentr.Top then
    begin
      FDragRow := FDragRow - 1;
      ScrollToVirtualNode(GetNodeForRow(FDragRow));
    end;
  end;
end;

function TAdvCustomTreeView.AlternativeDragDrop: Boolean;
begin
  Result := False;
end;

procedure TAdvCustomTreeView.Animate(Sender: TObject);
var
  dx, dy, posx, posy, dvo, posvo: Double;
  animh, animv, animvo: Boolean;
begin
  posy := GetVScrollValue;
  posx := GetHScrollValue;
  posvo := FVerticalOffset;

  animv := False;
  animh := False;
  animvo := False;

  if Interaction.AnimationFactor > 0 then
  begin
    dx := Abs(FScrollHTo - posx) / Max(1, Abs(FSpX) * Interaction.AnimationFactor);
    dy := Abs(FScrollVTo - posy) / Max(1, Abs(FSpY) * Interaction.AnimationFactor);
    dvo := Abs(FVerticalOffsetTo - posvo) / Interaction.AnimationFactor;

    if FAnimateVerticalPos then
      animv := AnimateDouble(posy, FScrollVTo, dy, 0.01);
    if FAnimateHorizontalPos then
      animh := AnimateDouble(posx, FScrollHTo, dx, 0.01);
    if FAnimateVerticalOffset then
      animvo := AnimateDouble(posvo, FVerticalOffsetTo, dvo, 0.01);
  end;

  FAnimating := animv or animh;
  if FAnimating then
    Scroll(posx, posy)
  else if animvo then
  begin
    FVerticalOffset := posvo;
    UpdateDisplay;
  end
  else
  begin
    FVerticalOffset := FVerticalOffsetTo;
    UpdateDisplay;
    FAnimateVerticalPos := False;
    FAnimateTimer.Enabled := False;
    FAnimateHorizontalPos := False;
  end;
end;

procedure TAdvCustomTreeView.CustomizeInplaceEditor(
  AInplaceEditor: TAdvTreeViewInplaceEditor;
  ANode: TAdvTreeViewVirtualNode; AColumn: TAdvTreeViewColumn);
{$IFDEF FMXLIB}
var
  ext, sel: Boolean;
  f: TAdvGraphicsFont;
  c: TAlphaColor;
{$ENDIF}
begin
  if not Assigned(ANode) or not Assigned(AColumn) or not Assigned(AInplaceEditor) then
    Exit;

  {$IFDEF FMXLIB}

  sel := IsVirtualNodeSelected(ANode);

  ext := False;
  DoIsNodeExtended(ANode, ext);

  if Assigned(AColumn) and not AColumn.UseDefaultAppearance and not ext then
  begin
    f := AColumn.Font;
    if sel then
      c := AColumn.SelectedFontColor
    else
      c := AColumn.Font.Color;
  end
  else
  begin
    if ext then
    begin
      f := NodesAppearance.ExtendedFont;
       if sel then
         c := NodesAppearance.ExtendedSelectedFontColor
       else
         c := NodesAppearance.ExtendedFontColor;
    end
    else
    begin
      f := NodesAppearance.Font;
      if sel then
        c := NodesAppearance.SelectedFontColor
      else
        c := NodesAppearance.Font.Color
    end;
  end;

  if (AInplaceEditor is TEdit) then
  begin
    (AInplaceEditor as TEdit).StyledSettings := [];
    (AInplaceEditor as TEdit).Font.Assign(f);
    (AInplaceEditor as TEdit).FontColor := c;
  end
  else if (AInplaceEditor is TMemo) then
  begin
    (AInplaceEditor as TMemo).StyledSettings := [];
    (AInplaceEditor as TMemo).Font.Assign(f);
    (AInplaceEditor as TMemo).FontColor := c;
  end;
  {$ENDIF}

  DoCustomizeInplaceEditor(ANode, AColumn.Index, FInplaceEditor);
end;

procedure TAdvCustomTreeView.CustomizeNodeCache(AGraphics: TAdvGraphics;
  AStartY: Single);
begin

end;

procedure TAdvCustomTreeView.CutToClipboard(ATextOnly: Boolean = False);
var
  I: Integer;
  n: TAdvTreeViewNode;
  nv: TAdvTreeViewVirtualNode;
begin
  if Assigned(OnCustomCutToClipboard) then
    OnCustomCutToClipboard(Self)
  else
  begin
    BeginUpdate;
    CopyToClipboard(ATextOnly);
    for I := SelectedNodeCount - 1 downto 0 do
    begin
      n := SelectedNodes[I];
      if Assigned(n) then
        RemoveNode(n)
      else
      begin
        nv := SelectedVirtualNodes[I];
        RemoveVirtualNode(nv);
      end;
    end;
    EndUpdate;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomTreeView.ApplyInplaceEditorStyleLookup(Sender: TObject);
var
  obj: TFmxObject;
  function FindSRes(AObject: TFmxObject; AResName: String): TFMXObject;
  var
    I: Integer;
  begin
    Result := nil;
    if Assigned(AObject) then
    begin
      Result := AObject.FindStyleResource(AResName);
      if not Assigned(Result) then
      begin
        for I := 0 to AObject.ChildrenCount - 1 do
        begin
          Result := FindSRes(AObject.Children[I], AResName);
          if Assigned(Result) then
            Break;
        end;
      end;
    end;
  end;
begin
  if (Sender = FInplaceEditor) and ((FInplaceEditor is TMemo) or (FInplaceEditor is TEdit)) then
  begin
    obj := FindSRes(FInplaceEditor, 'background');
    if Assigned(obj) and (obj is TCustomStyleObject) then
      TCustomStyleObject(obj).Source := nil
  end;
end;
{$ENDIF}

procedure TAdvCustomTreeView.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
    Fill.Color := c;

  if TAdvGraphicsStyles.GetStyleLineFillColor(c) then
  begin
    Stroke.Color := c;
    ColumnsAppearance.TopStroke.Color := c;
    ColumnsAppearance.BottomStroke.Color := c;
    GroupsAppearance.TopStroke.Color := c;
    GroupsAppearance.BottomStroke.Color := c;
    NodesAppearance.ExtendedFill.Color := c;
    NodesAppearance.ExtendedStroke.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleHeaderFillColor(c) then
  begin
    ColumnsAppearance.TopFill.Color := c;
    ColumnsAppearance.TopFill.Kind := gfkSolid;
    ColumnsAppearance.BottomFill.Color := c;
    ColumnsAppearance.BottomFill.Kind := gfkSolid;
    GroupsAppearance.TopFill.Color := c;
    GroupsAppearance.TopFill.Kind := gfkSolid;
    GroupsAppearance.BottomFill.Color := c;
    GroupsAppearance.BottomFill.Kind := gfkSolid;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleHeaderFillColorTo(c) then
  begin
    ColumnsAppearance.TopFill.ColorTo := c;
    ColumnsAppearance.TopFill.Kind := gfkGradient;
    ColumnsAppearance.BottomFill.ColorTo := c;
    ColumnsAppearance.BottomFill.Kind := gfkGradient;
    GroupsAppearance.TopFill.ColorTo := c;
    GroupsAppearance.TopFill.Kind := gfkGradient;
    GroupsAppearance.BottomFill.ColorTo := c;
    GroupsAppearance.BottomFill.Kind := gfkGradient;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleSelectionFillColor(c) then
  begin
    NodesAppearance.SelectedFill.Color := c;
    NodesAppearance.SelectedStroke.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleTextFontColor(c) then
  begin
    NodesAppearance.Font.Color := c;
    NodesAppearance.ExtendedFontColor := c;
    ColumnsAppearance.TopFont.Color := c;
    ColumnsAppearance.BottomFont.Color := c;
    GroupsAppearance.TopFont.Color := c;
    GroupsAppearance.BottomFont.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleAlternativeTextFontColor(c) then
  begin
    NodesAppearance.SelectedFontColor := c;
    NodesAppearance.SelectedTitleFontColor := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
    Fill.Color := c;
end;

procedure TAdvCustomTreeView.Assign(Source: TPersistent);
begin
  BeginUpdate;
  inherited;
  if Source is TAdvCustomTreeView then
  begin
    FNodesAppearance.Assign((Source as TAdvCustomTreeView).NodesAppearance);
    FColumnsAppearance.Assign((Source as TAdvCustomTreeView).ColumnsAppearance);
    FGroupsAppearance.Assign((Source as TAdvCustomTreeView).GroupsAppearance);
    FInteraction.Assign((Source as TAdvCustomTreeView).Interaction);
  end;
  EndUpdate;
end;

procedure TAdvCustomTreeView.BuildDisplay(ACache: TAdvTreeViewCache; ADisplay: TAdvTreeViewDisplayList);
var
  x, y: Double;
  I: Integer;
  cache: TAdvTreeViewCacheItem;
  r, rrt, rrb, rg, grt, grb: TRectF;
  n: TAdvTreeViewVirtualNode;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(ACache) or not Assigned(ADisplay) then
    Exit;

  ADisplay.Clear;

  x := -GetHorizontalScrollPosition;
  y := -GetVerticalScrollPosition;
  if CanApplyVerticalOffset then
    y := y + FVerticalOffset;

  r := GetNodesRect;
  grt := GetGroupsTopRect;
  grb := GetGroupsBottomRect;
  rrt := GetColumnsTopRect;
  rrb := GetColumnsBottomRect;
  for I := 0 to ACache.Count - 1 do
  begin
    cache := ACache[I];
    rg := cache.Rect;
    case cache.Kind of
      ikNode:
      begin
        n := cache.Node;
        if Assigned(n) then
          OffsetNodeRects(n, Int(x), Int(y), rg);
      end;
      ikGroupTop: OffsetRectEx(rg, int(x) + grt.Left, grt.Top);
      ikGroupBottom: OffsetRectEx(rg, int(x) + grb.Left, grb.Top - 1);
      ikColumnTop: OffsetRectEx(rg, int(x) + rrt.Left, rrt.Top);
      ikColumnBottom: OffsetRectEx(rg, int(x) + rrb.Left, rrb.Top - 1);
    end;

    cache.DrawRect := rg;
    case cache.Kind of
      ikNode:
      begin
        if RectIntersectsWithEx(rg, r) then
          ADisplay.Add(cache);
      end;
      ikColumnTop:
      begin
        if RectIntersectsWithEx(rg, rrt) then
          ADisplay.Add(cache);
      end;
      ikColumnBottom:
      begin
        if RectIntersectsWithEx(rg, rrb) then
          ADisplay.Add(cache);
      end;
      ikGroupTop:
      begin
        if RectIntersectsWithEx(rg, grt) then
          ADisplay.Add(cache);
      end;
      ikGroupBottom:
      begin
        if RectIntersectsWithEx(rg, grb) then
          ADisplay.Add(cache);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False);
var
  I: Integer;
  rcalc: TRectF;
  v: TAdvTreeViewVirtualNode;
  g: TAdvGraphics;
  strc, strt: String;
  ww, wwt: Boolean;
  maxw: Double;
  c: Boolean;
  colps: Double;
  def: Boolean;
  col: TAdvTreeViewColumn;
  hs: Double;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if (ACol >= 0) and (ACol <= ColumnCount - 1) and not ColumnsAppearance.Stretch and NodeListBuild then
  begin
    hs := GetHorizontalScrollPosition;
    maxw := 0;
    colps := ColumnPositions[ACol];
    g := TAdvGraphics.CreateBitmapCanvas;
    g.BeginScene;
    g.PictureContainer := PictureContainer;
    g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
    try
      def := True;
      col := nil;
      if (ACol >= 0) and (ACol <= Columns.Count - 1) then
      begin
        col := Columns[ACol];
        if not col.UseDefaultAppearance then
          def := False;
      end;

      if tclTop in ColumnsAppearance.Layouts then
      begin
        strc := GetColumnText(ACol);
        DoGetColumnText(ACol, ikColumnTop, strc);
        if def then
          g.Font.Assign(ColumnsAppearance.TopFont)
        else
          g.Font.Assign(col.TopFont);

        rcalc := RectF(0, 0, 10000, 10000);
        rcalc := g.CalculateText(strc, rcalc);
        rcalc.Right := rcalc.Right + ScalePaintValue(4
        );

        if rcalc.Right - rcalc.Left > maxw then
          maxw := rcalc.Right - rcalc.Left;
      end;

      if tclBottom in ColumnsAppearance.Layouts then
      begin
        strc := GetColumnText(ACol);
        DoGetColumnText(ACol, ikColumnBottom, strc);
        if def then
          g.Font.Assign(ColumnsAppearance.BottomFont)
        else
          g.Font.Assign(col.BottomFont);

        rcalc := RectF(0, 0, 10000, 10000);
        rcalc := g.CalculateText(strc, rcalc);
        rcalc.Right := rcalc.Right + ScalePaintValue(4);

        if rcalc.Right - rcalc.Left > maxw then
          maxw := rcalc.Right - rcalc.Left;
      end;

      for I := StartRow to StopRow do
      begin
        v := GetVisibleNodeForIndex(I);
        if Assigned(v) then
        begin
          strc := '';
          DoGetNodeText(v, ACol, tntmDrawing, strc);
          ww := False;
          if (ACol >= 0) and (ACol <= Columns.Count - 1) then
            ww := Columns[ACol].WordWrapping;

          DoGetNodeWordWrapping(v, ACol, ww);

          rcalc := RectF(0, 0, 10000, 10000);
          rcalc := g.CalculateText(strc, rcalc);
          rcalc.Right := rcalc.Right + ScalePaintValue(4
          );

          if ACol <= Length(v.BitmapRects) - 1 then
            rcalc.Right := rcalc.Right + v.BitmapRects[ACol].Right + hs - colps + ScalePaintValue(3);

          if rcalc.Right - rcalc.Left > maxw then
            maxw := rcalc.Right - rcalc.Left;

          strt := '';
          DoGetNodeTitle(v, ACol, tntmDrawing, strt);
          wwt := False;
          if (ACol >= 0) and (ACol <= Columns.Count - 1) then
            wwt := Columns[ACol].TitleWordWrapping;

          DoGetNodeTitleWordWrapping(v, ACol, wwt);

          rcalc := RectF(0, 0, 10000, 10000);
          rcalc := g.CalculateText(strt, rcalc);
          rcalc.Right := rcalc.Right + ScalePaintValue(4);

          if ACol <= Length(v.BitmapRects) - 1 then
            rcalc.Right := rcalc.Right + v.BitmapRects[ACol].Right + hs - colps + ScalePaintValue(3);

          if rcalc.Right - rcalc.Left > maxw then
            maxw := rcalc.Right - rcalc.Left;
        end;
      end;
    finally
      g.EndScene;
      g.Free;
    end;

    maxw := Int(maxw);

    c := True;
    if ACallEventHandlers then
      DoBeforeSizeColumn(ACol, ColumnWidths[ACol], maxw, c);

    if c then
    begin
      ColumnWidths[ACol] := maxw;
      if ACallEventHandlers then
        DoAfterSizeColumn(ACol, maxw);

      if AUpdate then
      begin
        UpdateTreeView;
        Invalidate;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.CloseInplaceEditor(ACancel: Boolean; AFlagClose: Boolean = False);
var
  {$IFNDEF LCLWEBLIB}
  AContext: TRttiContext;
  rt: TRttiType;
  propt: TRttiProperty;
  {$ENDIF}
  str: String;
  b, c: Boolean;
  n: String;
  {$IFDEF FMXLIB}
  propi: TRttiProperty;
  li: TListBoxItem;
  obj: TObject;
  {$ENDIF}
begin
  if FClosing then
  begin
    if AFlagClose then
      FInplaceEditorClosed := True;
    Exit;
  end;

  FClosing := True;
  if Assigned(FUpdateNode) and (FUpdateNodeColumn <> -1) and Assigned(FInplaceEditor) then
  begin
    if not ACancel then
    begin
      {$IFNDEF LCLWEBLIB}
      AContext := TRttiContext.Create;
      {$ENDIF}
      try
        n := '';
        {$IFNDEF LCLWEBLIB}
        rt := AContext.GetType(FInplaceEditor.ClassInfo);
        propt := rt.GetProperty('Text');
        if Assigned(Propt) then
          str := propt.GetValue(FInplaceEditor).AsString
        else
        {$ENDIF}
        begin
          {$IFDEF FMXLIB}
          propi := rt.GetProperty('Selected');
          if Assigned(propi) then
          begin
            obj := propi.GetValue(FInplaceEditor).AsObject;
            if Assigned(obj) and (obj is TListBoxItem) then
            begin
              li := obj as TListBoxItem;
              if Assigned(li) then
                str := li.Text;
            end;
          end;
          {$ENDIF}
        end;

        {$IFDEF LCLWEBLIB}
        if FInplaceEditor is TEdit then
          str := (FInplaceEditor as TEdit).Text
        else if FInplaceEditor is TComboBox then
          str := (FInplaceEditor as TComboBox).Text
        else if FInplaceEditor is TMemo then
          str := (FInplaceEditor as TMemo).Text;
        {$ENDIF}

        n := str;
      finally
        {$IFNDEF LCLWEBLIB}
        AContext.Free;
        {$ENDIF}
      end;
    end;

    c := True;
    DoCloseInplaceEditor(FUpdateNode, FUpdateNodeColumn, FInplaceEditor, ACancel, c);
    if c then
    begin
      b := True;
      DoBeforeUpdateNode(FUpdateNode, FUpdateNodeColumn, n, b);
      if b and not ACancel then
      begin
        UpdateCount := UpdateCount + 1;
        if Assigned(FUpdateNode.Node) then
          FUpdateNode.Node.Text[FUpdateNodeColumn] := n;
        DoUpdateNodeText(FUpdateNode, FUpdateNodeColumn, n);
        UpdateCount := UpdateCount - 1;

        DoAfterUpdateNode(FUpdateNode, FUpdateNodeColumn);
        DoNodeChanged(FUpdateNode);
      end;

      FInplaceEditor.Parent := nil;

      {$IFDEF FMXLIB}
      FInplaceEditorTimer.Enabled := True;
      {$IFNDEF FMXMOBILE}
      while FInplaceEditorTimer.Enabled do
        Application.ProcessMessages;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      if Assigned(FOldInplaceEditor) then
        FOldInplaceEditor.Free;

      FOldInplaceEditor := FInplaceEditor;
      {$ENDIF}
      FInplaceEditor := nil;
      FInplaceEditorActive := False;
    end;

    ResetNodes(False);
    UpdateTreeView;
    Invalidate;
    if CanFocus then
      SetFocus;
  end;
  FClosing := False;

  if AFlagClose then
    FInplaceEditorClosed := True;
end;

procedure TAdvCustomTreeView.SelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
    begin
      en := IsVirtualNodeSelectable(v);
      if en then
        FSelectedNodes.Add(v);
    end;
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.SelectNodes(ANodes: TAdvTreeViewNodeArray);
var
  I: Integer;
  v: TAdvTreeViewNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
    begin
      en := IsVirtualNodeSelectable(TAdvTreeViewNodeOpen(v).VirtualNode);

      if en then
        FSelectedNodes.Add(TAdvTreeViewNodeOpen(v).VirtualNode);
    end;
  end;

  Invalidate;
end;

procedure TAdvCustomTreeView.SelectVirtualNode(ANode: TAdvTreeViewVirtualNode);
var
  en: Boolean;
begin
  FSelectedNodes.Clear;
  if Assigned(ANode) then
  begin
    en := IsVirtualNodeSelectable(ANode);
    if en then
    begin
      FSelectedNodes.Add(ANode);
      FFocusedNode := ANode;
      InternalSelectVirtualNode(ANode);
    end;
  end;
  Invalidate;
end;

function TAdvCustomTreeView.IsAnimating: Boolean;
begin
  Result := (FAnimateTimer.Enabled = True) and FAnimating;
end;

function TAdvCustomTreeView.IsDragDropActive: Boolean;
begin
  Result := FDragMode and (Interaction.DragDropMode <> tdmNone);
end;

function TAdvCustomTreeView.IsEditing: Boolean;
begin
  Result := FInplaceEditorActive;
end;

function TAdvCustomTreeView.IsNodeSelectable(
  ANode: TAdvTreeViewNode): Boolean;
begin
  Result := True;
  if Assigned(ANode) then
    Result := IsVirtualNodeSelectable(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

function TAdvCustomTreeView.IsNodeSelected(ANode: TAdvTreeViewNode): Boolean;
begin
  Result := False;
  if Assigned(ANode) then
    Result := IsVirtualNodeSelected(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

function TAdvCustomTreeView.IsReorderActive: Boolean;
begin
  Result := FReorderMode and Interaction.Reorder;
end;

function TAdvCustomTreeView.IsVariableNodeHeight: Boolean;
begin
  Result := NodesAppearance.HeightMode = tnhmVariable;
end;

function TAdvCustomTreeView.IsVirtualNodeSelectable(
  ANode: TAdvTreeViewVirtualNode): Boolean;
var
  ext: Boolean;
begin
  Result := True;
  if Assigned(ANode) then
  begin
    DoIsNodeEnabled(ANode, Result);
    ext := False;
    DoIsNodeExtended(ANode, ext);
    if (ext and not Interaction.ExtendedSelectable) or not (VisibleNodes.IndexOf(ANode) > -1) then
      Result := False;
  end;
end;

function TAdvCustomTreeView.IsVirtualNodeSelected(ANode: TAdvTreeViewVirtualNode): Boolean;
begin
  Result := FSelectedNodes.IndexOf(ANode) > -1;
end;

procedure TAdvCustomTreeView.SelectAllNodes;
begin
  SelectAllVirtualNodes;
end;

procedure TAdvCustomTreeView.SelectAllVirtualNodes;
var
  I: Integer;
  n: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to VisibleNodes.Count - 1 do
  begin
    n := VisibleNodes[I];
    if Assigned(n) then
    begin
      en := IsVirtualNodeSelectable(n);

      if en then
        FSelectedNodes.Add(n);
    end;
  end;
  Invalidate;
end;

function TAdvCustomTreeView.SelectedNodeCount: Integer;
begin
  Result := SelectedVirtualNodeCount;
end;

function TAdvCustomTreeView.SelectedVirtualNodeCount: Integer;
begin
  Result := FSelectedNodes.Count;
end;

procedure TAdvCustomTreeView.SelectNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    SelectVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode)
  else
    SelectVirtualNode(nil);
end;

procedure TAdvCustomTreeView.SelectVirtualNodeByRow(ARow: Integer);
begin
  SelectVirtualNode(GetNodeForRow(ARow));
end;

procedure TAdvCustomTreeView.RemoveSelectedVirtualNodes;
var
  I: Integer;
  n: TAdvTreeViewVirtualNode;
begin
  BlockUpdate := True;
  for I := SelectedVirtualNodeCount - 1 downto 0 do
  begin
    n := SelectedVirtualNodes[I];
    FSelectedNodes.Remove(n);
    RemoveVirtualNode(n);
  end;
  BlockUpdate := False;

  ResetNodes(False);
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvCustomTreeView.RemoveSelectedNodes;
var
  I: Integer;
  n: TAdvTreeViewNode;
begin
  BlockUpdate := True;
  for I := SelectedNodeCount - 1 downto 0 do
  begin
    n := SelectedNodes[I];
    FSelectedNodes.Remove(TAdvTreeViewNodeOpen(n).VirtualNode);
    RemoveNode(n);
  end;
  BlockUpdate := False;

  ResetNodes(False);
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateInplaceEditorPosition;
var
  r: TRectF;
  {$IFDEF CMNWEBLIB}
  rt: TRect;
  {$ENDIF}
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  if Assigned(FUpdateNode) and (FUpdateNodeColumn <> -1) and Assigned(FInplaceEditor) and FInplaceEditorActive then
  begin
    if (FUpdateNode.Row >= StartRow) and (FUpdateNode.Row <= StopRow) then
    begin
      r := GetInplaceEditorRect(FUpdateNode, FUpdateNodeColumn);
      FInplaceEditor.Parent := Self;
      FInplaceEditor.Visible := False;
      if Assigned(FInplaceEditor) then
      begin
        {$IFDEF FMXLIB}
        FInplaceEditor.BoundsRect := r;
        FInplaceEditor.BoundsRect := RectF(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2 + FInplaceEditor.Height);
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        rt := Rect(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
        FInplaceEditor.BoundsRect := rt;
        FInplaceEditor.BoundsRect := Rect(rt.Left, rt.Top + (rt.Bottom - rt.Top - FInplaceEditor.Height) div 2, rt.Right, rt.Top + (rt.Bottom - rt.Top - FInplaceEditor.Height) div 2 + FInplaceEditor.Height);
        {$ENDIF}
        FInplaceEditor.Visible := True;
      end;
    end
    else
      FInplaceEditor.Parent := nil;
  end;
end;

procedure TAdvCustomTreeView.HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  dc: TAdvTreeViewCacheItem;
begin
  inherited;
  if (Button = {$IFNDEF WEBLIB}TAdvMouseButton.{$ENDIF}mbRight) or BlockUserInput or BlockMouseClick then
    Exit;

  if not FDblClicked then
    CaptureEx;

  FDragMode := False;
  FDragModeStarted := False;
  FReorderMode := False;
  FDragNode := nil;

  if not FDblClicked then
  begin
    if CanFocus then
      SetFocus;

    if FInplaceEditorActive then
      CloseInplaceEditor(False);
  end;

  dc := XYToCacheItem(X, Y);
  FDownNode := nil;
  if Assigned(dc) then
  begin
    FDownNode := dc.Node;
    if FSelectedNodes.IndexOf(FDownNode) > -1 then
    begin
      FDragNode := FDownNode;
      FDragMode := Assigned(FDownNode) and (Interaction.DragDropMode <> tdmNone) and not FDblClicked and not XYToNodeExpand(FDownNode, X, Y)
        and not XYToNodeExtra(FDownNode, X, Y) and not XYToNodeTitleExtra(FDownNode, X, Y) and not Assigned(XYToNodeCheck(FDownNode, X, Y).ANode);
      FReorderMode := Assigned(FDownNode) and Interaction.Reorder and not FDragMode and not FDblClicked and not XYToNodeExpand(FDownNode, X, Y)
        and not XYToNodeExtra(FDownNode, X, Y) and not XYToNodeTitleExtra(FDownNode, X, Y) and not Assigned(XYToNodeCheck(FDownNode, X, Y).ANode);
      if FReorderMode then
      begin
        CreateDragBitmap;
        FSaveDragY := FDragBitmap.Top;
      end;
    end;
  end;

  if FDragMode then
  begin
    if CanStartDragFromMouseDown then
    begin
      FDragModeStarted := True;
      HandleDragStart(X, Y);
      BeginDrag;
    end;
    IsMouseDown := not FDblClicked;
  end
  else
  begin
    if Assigned(FDownNode) then
    begin
      FDoNodeExpand := XYToNodeExpand(FDownNode, X, Y);
      FDoNodeCheck := XYToNodeCheck(FDownNode, X, Y);
      FDoNodeAnchor := XYToNodeAnchor(FDownNode, X, Y);
      FDoNodeTitleAnchor := XYToNodeTitleAnchor(FDownNode, X, Y);
      FDoNodeExtra := XYToNodeExtra(FDownNode, X, Y);
      FDoNodeTitleExtra := XYToNodeTitleExtra(FDownNode, X, Y);
    end;

    FSizeColumn := -1;
    if Interaction.ColumnSizing then
    begin
      FSizeColumn := XYToColumnSize(X, Y);
      if (FSizeColumn >= 0) and (FSizeColumn <= ColumnCount - 1) then
        FColumnSize := ColumnWidths[FSizeColumn];
    end;

    FDownColumn := XYToColumn(X, Y);
    FDownColumnFilter := XYToColumnFilter(X, Y);
    FDownColumnExpand := XYToColumnExpand(X, Y);

    IsMouseDown := not FDblClicked;
    FTimeStart := GetTickCountX;
    FTimeStop := FTimeStart;
    FScrollVTo := GetVScrollValue;
    FScrollHTo := GetHScrollValue;
    FScrollX := X;
    FScrollY := Y;
    FDownX := X;
    FDownY := Y;
    FMouseX := X;
    FMouseY := Y;
    FSizeX := X;
    FDragY := Y;
    FVerticalOffset := 0;
    FVerticalOffsetTo := 0;
    FMouseUp := False;
    FDoubleSelection := not FAnimateTimer.Enabled;
    FScrolling := False;
  end;
  FDblClicked := False;
end;

procedure TAdvCustomTreeView.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  f: Double;
  it: TAdvTreeViewVirtualNode;
  doscroll: Boolean;
  dsp: TAdvTreeViewCacheItem;
  sc: Integer;
  c: Boolean;
  colsz, newcolsz, newposy: Double;
  r: TRectF;
  n: TAdvTreeViewVirtualNode;
  su, sd: Boolean;
  a: string;
  crs: smallint;
  cl: Integer;
begin
  inherited;

  if BlockUserInput or (FAnimating and not IsMouseDown) then
    Exit;

  FY := Y;
  FX := X;

  if IsMouseDown and FDragMode and CanStartDragFromMouseMove and ((Abs(FY - FDownY) > DRAGMARGIN) or (Abs(FX - FDownX) > DRAGMARGIN)) then
  begin
    if not FDragModeStarted then
      HandleDragStart(X, Y);

    FDragModeStarted := True;
    if AlternativeDragDrop then
      HandleAlternativeDragOver(X, Y)
    else
      BeginDrag;
    Exit;
  end;

  if FReorderMode and Assigned(FDragBitmap) and Interaction.Reorder then
  begin
    r := GetContentClipRect;
    newposy := FSaveDragY + (FY - FDragY);
    if (newposy <= r.Top + (r.Bottom - r.Top) - FDragBitmap.Height) and (newposy >= r.Top) then
    begin
      FDragBitmap.Top := Round(Max(r.Top, Min(r.Top + (r.Bottom - r.Top) - FDragBitmap.Height, newposy)));
      FSaveDragY := FDragBitmap.Top;
      FDragTimer.Enabled := False;
      if (newposy > r.Top) and (newposy < r.Top + (r.Right - r.Left)) then
      begin
        n := XYToNode((r.Right - r.Left) / 2, newposy);
        if Assigned(n) then
          FDragRow := n.Row;
      end;
    end
    else if (newposy > r.Top + (r.Bottom - r.Top) - FDragBitmap.Height) then
    begin
      FDragBitmap.Top := Round(r.Top + (r.Bottom - r.Top) - FDragBitmap.Height);
      FSaveDragY := newposy;
      FDragTimer.Enabled := True;
    end
    else if (newposy < r.Top) then
    begin
      FDragBitmap.Top := Round(r.Top);
      FSaveDragY := newposy;
      FDragTimer.Enabled := True;
    end;

    FDragY := FY;
    Exit;
  end;

  if (FSizeColumn >= 0) and (FSizeColumn <= ColumnCount - 1) then
  begin
    c := True;
    colsz := FColumnSize;
    FColumnSize := colsz + (X - FSizeX);
    newcolsz := Max(0, FColumnSize);
    FSizeX := X;
    DoBeforeSizeColumn(FSizeColumn, colsz, newcolsz, c);
    if c then
    begin
      ColumnWidths[FSizeColumn] := newcolsz;
      if (FSizeColumn >= 0) and (FSizeColumn <= Columns.Count - 1) then
        Columns[FSizeColumn].UpdateWidth(newcolsz);

      DoAfterSizeColumn(FSizeColumn, newcolsz);
      ResetNodes(True);
      UpdateTreeView;
      Invalidate;
    end;
    Exit;
  end;

  if IsMouseDown and not (FDoNodeAnchor.AAnchor <> '') and not (FDoNodeTitleAnchor.AAnchor <> '') then
  begin
    doscroll := not FDoNodeExpand and not Assigned(FDoNodeCheck.ANode);
    if doscroll then
    begin
      f := 1;
      case ScrollMode of
        smNodeScrolling: f := 0.1;
      end;

      c := True;
      if ScrollLimitation and (Abs(Y - FMouseY) < 15) then
        c := Abs(X - FMouseX) < 5;

      if (FScrolling or (Abs(FMouseX - X) > 3) or (Abs(FMouseY - Y) > 3)) and Interaction.TouchScrolling and c then
      begin
        FDragTimer.Enabled := False;
        if (Abs(X - FDownX) > SCROLLINGDELAY) or (Abs(Y - FDownY) > SCROLLINGDELAY) then
        begin
          FScrolling := True;
          FDoubleSelection := False;
          if IsMouseDown and not FMouseUp then
          begin
            su := CanScrollUp;
            sd := CanScrollDown;
            if (su and (Y - FDownY > 0)) or (sd and (Y - FDownY < 0)) and (FVerticalOffset = 0) then
            begin
              FVerticalOffset := 0;
              Scroll(GetHScrollValue - (X - FDownX) * f, GetVScrollValue - (Y - FDownY) * f)
            end
            else if CanApplyVerticalOffset then
            begin
              FVerticalOffset := Min(75, Max(-75, FVerticalOffset + (Y - FDownY)));
              UpdateDisplay;
            end
            else
              Scroll(GetHScrollValue - (X - FDownX) * f, GetVScrollValue);

            FDownY := Y;
            FDownX := X;
          end;
        end;
      end
    end
  end
  else
  begin
    it := nil;
    dsp := XYToCacheItem(X, Y);
    if Assigned(dsp) then
      it := dsp.Node;

    crs := crDefault;

    if Assigned(it) and Assigned(dsp) then
    begin
      if Interaction.URLDetectionOnMouseMove and ((XYToNodeAnchor(it, X, Y).AAnchor <> '') or (XYToNodeTitleAnchor(it, X, Y).AAnchor <> '')) then
        crs := crHandPoint;
    end
    else
    begin
      if Interaction.ColumnSizing then
      begin
        sc := XYToColumnSize(X, Y);
        if (sc >= 0) and (sc <= ColumnCount - 1) then
          crs := crSizeWE;
      end;
    end;

    cl := -1;
    a := XYToColumnAnchor(X, Y, cl);
    if a <> '' then
      crs := crHandPoint;

    if crs <> Cursor then
      Cursor := crs;

    if (FPrevDSP <> dsp) then
    begin
      if Assigned(FPrevDSP) and Assigned(FPrevDSP.Node) then
        DoNodeMouseLeave(FPrevDSP.Node);
      if Assigned(dsp) and Assigned(dsp.Node) then
        DoNodeMouseEnter(dsp.Node);
    end;

    FPrevDSP := dsp;
  end;
end;

procedure TAdvCustomTreeView.HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  f: Double;
  a: TAdvTreeViewNodeAnchor;
  cl: Integer;
  ha: string;
  en, d, dosel, ext: Boolean;
  I: Integer;
  t: Integer;
  dn, prev: TAdvTreeViewVirtualNode;
  b: Boolean;
  sz: Integer;
  cs: Boolean;
  add: Integer;
begin
  inherited;

  if (Button = {$IFNDEF WEBLIB}TAdvMouseButton.{$ENDIF}mbRight) or BlockUserInput then
    Exit;

  {$IFDEF LCLLIB}
  if FDblClicked then
    FDblClicked := False;
  {$ENDIF}

  ReleaseCaptureEx;

  if FDragMode and FDragModeStarted and AlternativeDragDrop then
  begin
    HandleAlternativeDragDrop(X, Y);
    DestroyDragBitmap;
    Cursor := crDefault;
  end;

  sz := FSizeColumn;
  FSizeColumn := -1;
  FDragTimer.Enabled := False;

  if not IsMouseDown then
    Exit;

  if FReorderMode and Interaction.Reorder then
  begin
    dn := XYToNode(X, Y);
    if Assigned(OnCustomReorder) then
      OnCustomReorder(Self, FDragNode, dn)
    else
    begin
      if Assigned(dn) and (dn <> FDragNode) and Assigned(dn.Node) and Assigned(FDragNode.Node) then
      begin
        b := True;
        DoBeforeReorderNode(FDragNode, dn, b);
        if b then
        begin
          if FDragNode.Level = dn.Level then
          begin
            if dn.Node.GetParent = nil then
              FDragNode.Node.Index := dn.Node.Index
            else
              FDragNode.Node.MoveTo(dn.Node.GetParent, dn.Node.Index);
          end;
          DoAfterReorderNode(FDragNode, dn);
        end;
      end;
    end;

    DestroyDragBitmap;
    FDragNode := nil;
  end;

  f := 1;
  case ScrollMode of
    smNodeScrolling: f := 0.1;
  end;

  IsMouseDown := False;
  FMouseUp := True;
  FScrolling := False;

  if FDownColumnFilter <> -1 then
    HandleFilter(FDownColumnFilter)
  else if FDownColumnExpand <> -1 then
    HandleExpand(FDownColumnExpand)
  else if (FDownColumn <> -1) and (sz = -1) then
  begin
    if (FDownColumn >= 0) and (FDownColumn <= ColumnCount - 1) then
    begin
      if (Columns[FDownColumn].Sorting <> tcsNone) then
      begin
        case TAdvTreeViewColumnOpen(Columns[FDownColumn]).SortKind of
          nskNone, nskAscending:
          begin
            InitializeColumnSorting(FDownColumn, nsmDescending);
            if Assigned(OnCustomColumnSort) then
              OnCustomColumnSort(Self, FDownColumn, nsmDescending)
            else
            begin
              DoBeforeSortNodes(FDownColumn, nsmDescending, cs);
              if cs then
              begin
                Nodes.Sort(FDownColumn, Columns[FDownColumn].Sorting in [tcsRecursive, tcsRecursiveCaseSensitive], Columns[FDownColumn].Sorting in [tcsNormalCaseSensitive, tcsRecursiveCaseSensitive], nsmDescending);
                DoAfterSortNodes(FDownColumn, nsmDescending);
              end;
            end;
          end;
          nskDescending:
          begin
            InitializeColumnSorting(FDownColumn, nsmAscending);
            if Assigned(OnCustomColumnSort) then
              OnCustomColumnSort(Self, FDownColumn, nsmAscending)
            else
            begin
              DoBeforeSortNodes(FDownColumn, nsmAscending, cs);
              if cs then
              begin
                Nodes.Sort(FDownColumn, Columns[FDownColumn].Sorting in [tcsRecursive, tcsRecursiveCaseSensitive], Columns[FDownColumn].Sorting in [tcsNormalCaseSensitive, tcsRecursiveCaseSensitive], nsmAscending);
                DoAfterSortNodes(FDownColumn, nsmAscending);
              end;
            end;
          end;
        end;
      end;

      cl := -1;
      ha := XYToColumnAnchor(X, Y, cl);
      if ha <> '' then
        DoColumnAnchorClick(FDownColumn, ha);
    end;
  end
  else if not FReorderMode and not FDragModeStarted then
  begin
    if not FDoNodeExpand and not FDoNodeExtra and not FDoNodeTitleExtra and not Assigned(FDoNodeCheck.ANode) and not (FDoNodeAnchor.AAnchor <> '') and not (FDoNodeTitleAnchor.AAnchor <> '') then
    begin
      if not FDoubleSelection and Interaction.TouchScrolling then
      begin
        FTimeStop := GetTickCountX;
        if (FVerticalOffset = 0) and ((FTimeStop - FTimeStart) < SWIPECOUNT) and ((FTimeStop - FTimeStart) > 0) then
        begin
          FSpY := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
          if (FSpY > 0) then
          begin
            if (Y - FScrollY) > 0 then
              FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewportSize, FScrollVTo - Round(Abs(Y - FScrollY) * FSpY * f * 3)))
            else
              FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewportSize, FScrollVTo + Round(Abs(Y - FScrollY) * FSpY * f * 3)));

            FAnimateVerticalPos := True;
            FAnimateTimer.Enabled := True;
          end;

          FSpX := Abs(X - FScrollX) / (FTimeStop - FTimeStart);
          if (FSpX > 0) then
          begin
            if (X - FScrollX) > 0 then
              FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewportSize, FScrollHTo - Round(Abs(X - FScrollX) * FSpX * f * 3)))
            else
              FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewportSize, FScrollHTo + Round(Abs(X - FScrollX) * FSpX * f * 3)));

            FAnimateHorizontalPos := True;
            FAnimateTimer.Enabled := True;
          end;
        end
        else if CanApplyVerticalOffset then
        begin
          if NeedsReload(FVerticalOffset) then
            StartReload;

          StartVerticalOffsetAnimation;
        end;
      end
      else
      begin
        if Assigned(FDownNode) and not BlockMouseClick then
        begin
          en := IsVirtualNodeSelectable(FDownNode);
          prev := FFocusedNode;
          dosel := en;

          if en then
          begin
            if (ssShift in Shift) and (msShiftSelect in Interaction.MultiSelectStyle) then
            begin
              if Assigned(FFocusedNode) then
              begin
                if (msSiblingOnly in Interaction.MultiSelectStyle) and (FFocusedNode.ParentNode <> FDownNode.ParentNode) then
                 add := 0
                else
                  add := 1;
                if FFocusedNode.Row <= FDownNode.Row then
                begin
                  for I := FFocusedNode.Row + add to FDownNode.Row do
                    HandleSelectNode(GetNodeFromNodeStructure(I), True, False, {$IFNDEF FMXMOBILE}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
                end
                else
                begin
                  for I := FFocusedNode.Row - add downto FDownNode.Row do
                    HandleSelectNode(GetNodeFromNodeStructure(I), True, False, {$IFNDEF FMXMOBILE}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
                end;
              end;
            end
            else
            begin
              {$IFDEF FMXMOBILE}
              if Interaction.MultiSelect then
              begin
                if IsVirtualNodeSelected(FDownNode) and Interaction.MultiSelect then
                  UnSelectVirtualNode(FDownNode)
                else
                  HandleSelectNode(FDownNode, True, False, Interaction.MultiSelect)
              end
              else
              begin
              {$ENDIF}
                if (msSiblingOnly in Interaction.MultiSelectStyle) and Assigned(FFocusedNode) and (FDownNode.ParentNode <> FFocusedNode.ParentNode) and (FSelectedNodes.Count > 0) then
                begin
                  FSelectedNodes.Clear;
                end
                else if ((not (ssCtrl in Shift) and not (ssCommand in Shift)) or not(msControlSelect in Interaction.MultiSelectStyle)) and (FSelectedNodes.Count > 0) then
                  UnSelectVirtualNode(FDownNode);

                HandleSelectNode(FDownNode, True, False, (((ssCtrl in Shift) or (ssCommand in Shift)) and (msControlSelect in Interaction.MultiSelectStyle)) and Interaction.MultiSelect);
              {$IFDEF FMXMOBILE}
              end;
              {$ENDIF}
            end;
          end;

          if dosel then
          begin
            if FFocusedNode <> FDownNode then
            begin
              FFocusedNode := FDownNode;
              DoFocusedNodeChanged(FFocusedNode);
            end;
          end;

          if not FCompactMode and ((Interaction.MouseEditMode = tmemSingleClick) or ((Interaction.MouseEditMode = tmemSingleClickOnSelectedNode) and (FDownNode = prev))) and not Interaction.ReadOnly then
          begin
            if Assigned(FDownNode) then
            begin
              ext := False;
              DoIsNodeExtended(FDownNode, ext);
              d := True;
              DoIsNodeEnabled(FDownNode, d);

              a := XYToNodeTitleAnchor(FDownNode, X, Y);
              if a.AAnchor <> '' then
                DoNodeTitleAnchorClick(FDownNode, a.AColumn, a.AAnchor);

              a := XYToNodeAnchor(FDownNode, X, Y);
              if d and (a.AAnchor <> '') then
              begin
                DoNodeAnchorClick(FDownNode, a.AColumn, a.AAnchor);
              end
              else if ((ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext) and d then
              begin
                t := XYToNodeTextColumn(FDownNode, X, Y);
                if (t <> -1) and FDownNode.TitleExpanded[t] then
                begin
                  HandleNodeEditing(FDownNode, t);
                end;
              end;
            end;

          end;

          Invalidate;
        end
      end
    end
    else if not FCompactMode then         
    begin
      if FDoNodeExpand then
        HandleNodeToggle(FDownNode)
      else if FDoNodeExtra then
        HandleNodeExtra(FDownNode)
      else if FDoNodeTitleExtra then
        HandleNodeTitleExtra(FDownNode)
      else if Assigned(FDoNodeCheck.ANode) and not Interaction.ReadOnly then
        HandleNodeToggleCheck(FDownNode, FDoNodeCheck.AColumn)
      else
      begin
        a := XYToNodeAnchor(FDownNode, X, Y);
        if a.AAnchor <> '' then
          DoNodeAnchorClick(FDownNode, a.AColumn, a.AAnchor);

        a := XYToNodeTitleAnchor(FDownNode, X, Y);
        if a.AAnchor <> '' then
          DoNodeTitleAnchorClick(FDownNode, a.AColumn, a.AAnchor);
      end;
    end;
  end;

  FDoNodeExpand := False;
  FDoNodeExtra := False;
  FDoNodeTitleExtra := False;
  FDoNodeAnchor.AAnchor := '';
  FDoNodeAnchor.AColumn := -1;
  FDoNodeTitleAnchor.AAnchor := '';
  FDoNodeTitleAnchor.AColumn := -1;
  FDownColumn := -1;
  FDownColumnFilter := -1;
  FDownColumnExpand := -1;
  FReorderMode := False;
  FDragMode := False;
  FDragModeStarted := False;
end;

procedure TAdvCustomTreeView.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  vpos, hpos: Double;
  sz: Single;
  R: Integer;
  nd: TAdvTreeViewVirtualNode;
begin
  inherited;
  if BlockUserInput then
    Exit;

  vpos := GetVScrollValue;
  hpos := GetHScrollValue;

  if WheelDelta > 0 then
    R := StartRow - 1
  else
    R := StartRow;

  nd := GetVisibleNodeForIndex(R);

  if Assigned(nd) then
  begin
    if WheelDelta > 0 then
    begin
      sz := nd.Height * Interaction.MouseWheelDelta;
      Scroll(hpos, vpos - sz)
    end
    else
    begin
      sz := nd.Height * Interaction.MouseWheelDelta;
      Scroll(hpos, vpos + sz);
    end;
  end
  else
    Scroll(hpos, 0);

  Handled := True;
end;

procedure TAdvCustomTreeView.UpdateGroupCache(ACache: TAdvTreeViewCache);
var
  h: Double;
  I: Integer;
  grp: TAdvTreeViewDisplayGroup;
  x, y, bw, bh: Double;
  rt, rc: TRectF;
  cache: TAdvTreeViewCacheItem;
begin
  inherited;
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (ColumnCount = 0) or (DisplayGroups.Count = 0) or not Assigned(ACache) then
    Exit;

  ACache.Clear;

  if (ACache is TAdvTreeViewGroupsTopCache) and (not (tglTop in GroupsAppearance.Layouts) or (GroupsAppearance.TopSize <= 0)) then
    Exit;

  if (ACache is TAdvTreeViewGroupsBottomCache) and (not (tglBottom in GroupsAppearance.Layouts) or (GroupsAppearance.BottomSize <= 0)) then
    Exit;

  if ACache is TAdvTreeViewGroupsTopCache then
    h := GetGroupsTopSize + 1
  else
    h := GetGroupsBottomSize + 1;

  for I := 0 to DisplayGroups.Count - 1 do
  begin
    grp := DisplayGroups[I];
    {$IFDEF FMXWEBLIB}
    y := 0;
    x := Int(ColumnPositions[grp.StartColumn]);
    bh := h;
    bw := Int(ColumnPositions[grp.EndColumn + 1] - x);
    {$ENDIF}
    {$IFDEF CMNLIB}
    y := 0;
    x := ColumnPositions[grp.StartColumn];
    bh := h;
    bw := ColumnPositions[grp.EndColumn + 1] - x;
    {$ENDIF}

    if (bw <= 0) or (bh <= 0) then
      Continue;

    rt := RectF(0, 0, bw, bh);

    {$IFDEF FMXWEBLIB}
    rc.Top := int(rt.Top) + 0.5;
    rc.Bottom := int(rt.Bottom) - 0.5;
    rc.Left := int(rt.Left) + 0.5;
    rc.Right := int(rt.Right) + 0.5;
    {$ENDIF}
    {$IFDEF CMNLIB}
    rc.Top := rt.Top;
    rc.Bottom := rt.Bottom;
    rc.Left := rt.Left;
    rc.Right := rt.Right + 1;
    {$ENDIF}

    OffsetRectEx(rc, x, y);
    if ACache is TAdvTreeViewGroupsTopCache then
      cache := TAdvTreeViewCacheItem.CreateGroupTop(rc, I, grp.StartColumn, grp.EndColumn)
    else
      cache := TAdvTreeViewCacheItem.CreateGroupBottom(rc, I, grp.StartColumn, grp.EndColumn);

    ACache.Add(cache);
  end;
end;

procedure TAdvCustomTreeView.UpdateGroupsCache;
begin
  UpdateGroupCache(FGroupsTopCache);
  UpdateGroupCache(FGroupsBottomCache);
  UpdateGroupsDisplay;
end;

procedure TAdvCustomTreeView.UpdateGroupsDisplay;
begin
  BuildDisplay(FGroupsTopCache, FGroupsTopDisplay);
  BuildDisplay(FGroupsBottomCache, FGroupsBottomDisplay);
end;

function TAdvCustomTreeView.CanApplyVerticalOffset: Boolean;
begin
  Result := False;
end;

procedure TAdvCustomTreeView.CancelEditing;
begin
  if FInplaceEditorActive then
    CloseInplaceEditor(True);
end;

function TAdvCustomTreeView.CanStartDragFromMouseDown: Boolean;
begin
  Result := True;
end;

function TAdvCustomTreeView.CanStartDragFromMouseMove: Boolean;
begin
  Result := False;
end;

procedure TAdvCustomTreeView.ChangeDPIScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  BeginUpdate;
  CreateCheckBoxBitmaps;
  CreateRadioButtonBitmaps;
  NodesAppearance.LevelIndent := TAdvUtils.MulDivSingle(NodesAppearance.LevelIndent, M, D);
  NodesAppearance.ExpandWidth := TAdvUtils.MulDivSingle(NodesAppearance.ExpandWidth, M, D);
  NodesAppearance.ExpandHeight := TAdvUtils.MulDivSingle(NodesAppearance.ExpandHeight, M, D);
  ColumnsAppearance.TopSize := TAdvUtils.MulDivSingle(ColumnsAppearance.TopSize, M, D);
  ColumnsAppearance.BottomSize := TAdvUtils.MulDivSingle(ColumnsAppearance.BottomSize, M, D);
  GroupsAppearance.TopSize := TAdvUtils.MulDivSingle(GroupsAppearance.TopSize, M, D);
  GroupsAppearance.BottomSize := TAdvUtils.MulDivSingle(GroupsAppearance.BottomSize, M, D);
  ColumnsAppearance.BottomFont.Height := TAdvUtils.MulDivInt(ColumnsAppearance.BottomFont.Height, M, D);
  ColumnsAppearance.TopFont.Height := TAdvUtils.MulDivInt(ColumnsAppearance.TopFont.Height, M, D);
  GroupsAppearance.BottomFont.Height := TAdvUtils.MulDivInt(GroupsAppearance.BottomFont.Height, M, D);
  GroupsAppearance.TopFont.Height := TAdvUtils.MulDivInt(GroupsAppearance.TopFont.Height, M, D);
  NodesAppearance.ExtendedFont.Height := TAdvUtils.MulDivInt(NodesAppearance.ExtendedFont.Height, M, D);
  NodesAppearance.Font.Height := TAdvUtils.MulDivInt(NodesAppearance.Font.Height, M, D);
  NodesAppearance.FixedHeight := TAdvUtils.MulDivSingle(NodesAppearance.FixedHeight, M, D);
  NodesAppearance.VariableMinimumHeight := TAdvUtils.MulDivSingle(NodesAppearance.VariableMinimumHeight, M, D);
  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].Width := TAdvUtils.MulDivSingle(Columns[I].Width, M, D);
    Columns[I].Font.Height := TAdvUtils.MulDivInt(Columns[I].Font.Height, M, D);
    Columns[I].TitleFont.Height := TAdvUtils.MulDivInt(Columns[I].TitleFont.Height, M, D);
    Columns[I].TopFont.Height := TAdvUtils.MulDivInt(Columns[I].TopFont.Height, M, D);
    Columns[I].BottomFont.Height := TAdvUtils.MulDivInt(Columns[I].BottomFont.Height, M, D);
    Columns[I].ExpandingButtonSize := TAdvUtils.MulDivSingle(Columns[I].ExpandingButtonSize, M, D);
    Columns[I].Filtering.ButtonSize := TAdvUtils.MulDivSingle(Columns[I].Filtering.ButtonSize, M, D);
    Columns[I].Filtering.DropDownWidth := TAdvUtils.MulDivInt(Columns[I].Filtering.DropDownWidth, M, D);
    Columns[I].Filtering.DropDownHeight := TAdvUtils.MulDivInt(Columns[I].Filtering.DropDownHeight, M, D);
  end;
  EndUpdate;
end;

procedure TAdvCustomTreeView.UpdateNodeCache;
var
  g: TAdvGraphics;
  vs, stty: Single;
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(FNodeCache) then
    Exit;

  FNodeCache.Clear;

  if ColumnCount > 0 then
  begin
    g := TAdvGraphics.CreateBitmapCanvas;
    g.BeginScene;
    g.PictureContainer := PictureContainer;
    g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
    try
      vs := GetVerticalScrollPosition;
      stty := vs + StartOffset;

      for I := StartRow to StopRow do
      begin
        v := GetVisibleNodeForIndex(I);
        if Assigned(v) then
        begin
          if ConfigureNode(g, I, v, stty) then
            Break;
        end;
      end;

      CustomizeNodeCache(g, stty);
    finally
      g.EndScene;
      g.Free;
    end;
  end;
end;

procedure TAdvCustomTreeView.UpdateNodeDisplay;
begin
  BuildDisplay(FNodeCache, FNodeDisplay);
end;

constructor TAdvCustomTreeView.Create(AOwner: TComponent);
{$IFDEF CMNWEBLIB}
var
  s: Single;
{$ENDIF}
begin
  inherited;
  FColumnStroke := TAdvGraphicsStroke.Create(gskSolid, gcNull);
  FColumnStroke.OnChanged := ColumnStrokeChanged;

  FSizeColumn := -1;
  FGroupsCaching := False;
  FColumnsCaching := False;
  FSelectedNodes := TAdvTreeViewSelectedNodes.Create;
  FNodeCache := TAdvTreeViewNodeCache.Create;
  FColumnsTopCache := TAdvTreeViewColumnsTopCache.Create;
  FGroupsTopCache := TAdvTreeViewGroupsTopCache.Create;
  FColumnsBottomCache := TAdvTreeViewColumnsBottomCache.Create;
  FGroupsBottomCache := TAdvTreeViewGroupsBottomCache.Create;

  FNodeDisplay := TAdvTreeViewNodeDisplayList.Create;
  FColumnsTopDisplay := TAdvTreeViewColumnsTopDisplayList.Create;
  FGroupsTopDisplay := TAdvTreeViewGroupsTopDisplayList.Create;
  FColumnsBottomDisplay := TAdvTreeViewColumnsBottomDisplayList.Create;
  FGroupsBottomDisplay := TAdvTreeViewGroupsBottomDisplayList.Create;

  FNodesAppearance := TAdvTreeViewNodesAppearance.Create(Self);
  FColumnsAppearance := TAdvTreeViewColumnsAppearance.Create(Self);
  FGroupsAppearance := TAdvTreeViewGroupsAppearance.Create(Self);
  FInteraction := TAdvTreeViewInteraction.Create(Self);

  FDragTimer := TTimer.Create(Self);
  FDragTimer.Interval := 10;
  FDragTimer.Enabled := False;
  FDragTimer.OnTimer := DragTime;

  FAnimateTimer := TTimer.Create(Self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.Enabled := False;
  FAnimateTimer.OnTimer := Animate;

  {$IFDEF FMXLIB}
  FScrollBarTimer := TTimer.Create(Self);
  FScrollBarTimer.Interval := 1;
  FScrollBarTimer.Enabled := False;
  FScrollBarTimer.OnTimer := ScrollBarChanged;

  FInplaceEditorTimer := TTimer.Create(Self);
  FInplaceEditorTimer.Interval := 1;
  FInplaceEditorTimer.OnTimer := DoInplaceEditorTimer;
  FInplaceEditorTimer.Enabled := False;
  {$ENDIF}

  FCopyNodes := TAdvTreeViewCopyNodes.Create(nil, nil);

  FFilterListBox := TListBox.Create(Self);
  {$IFDEF LCLLIB}
  FFilterListBox.ClickOnSelChange := False;
  {$ENDIF}
  FFilterListBox.Tag := -1;
  FFilterListBox.OnClick := HandleFilterListClick;
  {$IFDEF FMXLIB}
  FFilterListBox.OnApplyStyleLookup := ApplyFilterListBoxStyleLookUp;
  {$ENDIF}

  FGlobalFont := TAdvAppearanceGlobalFont.Create(Self);

  FFilterTimer := TTimer.Create(Self);
  FFilterTimer.OnTimer := HandleFilterTimer;
  FFilterTimer.Interval := 100;
  FFilterTimer.Enabled := False;

  FFilterPopup := TAdvPopupEx.Create(Self);
  FFilterPopup.Placement := ppAbsolute;

  Width := 300;
  Height := 280;

  {$IFDEF FNCLIB}
  FDefaultViewJSONOptions := TAdvTreeViewViewJSONOptions.Create;
  {$ENDIF}

  {$IFDEF CMNWEBLIB}
  s := TAdvUtils.GetDPIScale(Self, 96);
  if (s = 1) then
    FCheckStartSize := 19
  else if (s = 1.25) or (s = 1.75) then
    FCheckStartSize := 16
  else
    FCheckStartSize := 17;
  {$ENDIF}

  if IsDesignTime then
    InitSample;
end;

procedure TAdvCustomTreeView.CreateDragBitmap;
var
  g: TAdvGraphics;
  dr: TRectF;
  {$IFDEF WEBLIB}
  r: String;
  {$ENDIF}
begin
  if not Assigned(FDragNode) then
    Exit;

  DestroyDragBitmap;

  dr := TAdvTreeViewVirtualNodeOpen(FDragNode).Cache.DrawRect;
  FDragBitmap := TAdvImageEx.Create(Self);
  {$IFDEF FMXLIB}
  FDragBitmap.HitTest := False;
  {$ENDIF}
  FDragBitmap.Width := Round((dr.Right - dr.Left) / ResourceScaleFactor);
  FDragBitmap.Height := Round((dr.Bottom - dr.Top) / ResourceScaleFactor);
  FDragBitmap.Left := Round(dr.Left / ResourceScaleFactor);
  FDragBitmap.Top := Round(dr.Top / ResourceScaleFactor);
  g := TAdvGraphics.CreateBitmapCanvas(Round(FDragBitmap.Width * ResourceScaleFactor), Round(FDragBitmap.Height * ResourceScaleFactor), False);
  g.PictureContainer := PictureContainer;
  g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
  try
    g.BeginScene;
    DrawNode(g, RectF(0, 0, dr.Right - dr.Left, dr.Bottom - dr.Top), FDragNode, False, -dr.Left, -dr.Top);
    g.EndScene;
    {$IFDEF WEBLIB}
    r := g.Bitmap.GetBase64Image;
    FDragBitmap.Bitmaps.AddBitmapFromResource(r);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FDragBitmap.Bitmaps.AddDrawBitmap(g.Bitmap);
    {$ENDIF}
  finally
    g.Free;
  end;

  FDragBitmap.Parent := Self;
end;

procedure TAdvCustomTreeView.HandleAlternativeDragDrop(X, Y: Single);
begin

end;

procedure TAdvCustomTreeView.HandleAlternativeDragOver(X, Y: Single);
begin

end;

procedure TAdvCustomTreeView.HandleCustomKeys(AKey: Word);
begin

end;

procedure TAdvCustomTreeView.HandleDblClick(X, Y: Single);
var
  pf: TPointF;
  c: TAdvTreeViewVirtualNode;
  col: Integer;
  en, ext: Boolean;
  t: Integer;
begin
  inherited;
  if BlockUserInput then
    Exit;

  FDblClicked := True;
  pf := PointF(X, Y);
  FDownNode := nil;
  c := XYToNode(pf.X, pf.Y);
  if Assigned(c) then
  begin
    if not XYToNodeExpand(c, pf.X, pf.Y) and not Assigned(XYToNodeCheck(c, pf.X, pf.Y).ANode) then
    begin
      FDownNode := c;
      if Assigned(FDownNode) then
      begin
        en := True;
        DoIsNodeEnabled(FDownNode, en);
        if en then
        begin
          ext := False;
          DoIsNodeExtended(FDownNode, ext);
          if not FCompactMode and ((ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext) and (Interaction.MouseEditMode = tmemDoubleClick) and not Interaction.ReadOnly then
          begin
            t := XYToNodeTextColumn(FDownNode, pf.X, pf.Y);
            if t <> -1 then
              HandleNodeEditing(FDownNode, t);
          end
          else if Interaction.ExpandCollapseOnDblClick then
            HandleNodeToggle(FDownNode);
        end;

        DoNodeDblClick(FDownNode);
      end;
    end;
  end
  else
  begin
    col := XYToColumnSize(pf.X, pf.Y);
    if Interaction.ColumnAutoSizeOnDblClick then
      AutoSizeColumn(col);
  end;

  IsMouseDown := False;
  FMouseUp := True;
  FScrolling := False;

  FDoNodeExpand := False;
  FDoNodeExtra := False;
  FDoNodeTitleExtra := False;
  FDoNodeAnchor.AAnchor := '';
  FDoNodeAnchor.AColumn := -1;
  FDoNodeTitleAnchor.AAnchor := '';
  FDoNodeTitleAnchor.AColumn := -1;
  FDownColumn := -1;
  FDownColumnFilter := -1;
  FDownColumnExpand := -1;
  FReorderMode := False;
  FDragMode := False;
  FDragModeStarted := False;
end;

destructor TAdvCustomTreeView.Destroy;
begin
  if Assigned(FOldInplaceEditor) then
    FOldInplaceEditor.Free;

  {$IFDEF FNCLIB}
  FDefaultViewJSONOptions.Free;
  {$ENDIF}

  FGlobalFont.Free;

  FCopyNodes.Free;
  FFilterTimer.Free;
  FFilterListBox.Free;
  FFilterPopup.Free;

  if Assigned(FDragBitmap) then
  begin
    FDragBitmap.Free;
    FDragBitmap := nil;
  end;
  {$IFDEF FMXLIB}
  FScrollBarTimer.Free;
  FInplaceEditorTimer.Free;
  {$ENDIF}
  FColumnStroke.Free;
  FAnimateTimer.Free;
  FNodesAppearance.Free;
  FGroupsAppearance.Free;
  FInteraction.Free;
  FColumnsAppearance.Free;
  FGroupsTopDisplay.Free;
  FColumnsTopDisplay.Free;
  FGroupsBottomDisplay.Free;
  FColumnsBottomDisplay.Free;
  FSelectedNodes.Free;
  FNodeDisplay.Free;
  FNodeCache.Free;
  FGroupsTopCache.Free;
  FColumnsTopCache.Free;
  FGroupsBottomCache.Free;
  FColumnsBottomCache.Free;
  inherited;
end;

procedure TAdvCustomTreeView.DestroyDragBitmap;
begin
  if Assigned(FDragBitmap) then
  begin
    FDragBitmap.Parent := nil;
    {$IFDEF FMXMOBILE}
    FDragBitmap.DisposeOf;
    {$ELSE}
    FDragBitmap.Free;
    {$ENDIF}
    FDragBitmap := nil;
  end;
end;

procedure TAdvCustomTreeView.HandleFilter(AColumn: Integer);
var
  sl: TStringList;
  pt: TPointF;
  n: TAdvTreeViewVirtualNode;
  s: String;
  topr, r, fr: TRectF;
  szd: Single;
  colw, colp: Single;
begin
  if (AColumn >= 0) and (AColumn <= Columns.Count - 1) and Columns[AColumn].Filtering.Enabled then
  begin
    colw := ColumnWidths[AColumn];
    if colw >= 0 then
    begin
      colp := ColumnPositions[AColumn];
      sl := TStringList.Create;
      try
        sl.Duplicates := dupIgnore;
        sl.Sorted := true;
        sl.Add(sAdvTreeViewFilterAll);

        n := GetFirstRootVirtualNode;
        while Assigned(n) do
        begin
          s := '';
          DoGetNodeText(n, AColumn, tntmDrawing, s);
          s := TAdvUtils.HTMLStrip(s);
          if s <> '' then
            sl.Add(s);
          n := n.GetNext;
        end;

        DoNeedFilterDropDownData(AColumn, sl);

        FFilterListBox.Parent := Self;
        {$IFDEF FMXLIB}
        FFilterListBox.BeginUpdate;
        {$ENDIF}
        FFilterListBox.Items.Assign(sl);
        FFilterListBox.ItemIndex := -1;
        {$IFDEF FMXLIB}
        FFilterListBox.EndUpdate;
        {$ENDIF}
        {$IFNDEF FMXLIB}
        TAdvUtils.SetFontSize(FFilterListBox.Font, 16);
        {$ENDIF}
        FFilterListBox.Parent := nil;

        FFilterListBox.Tag := AColumn;
      finally
        sl.Free;
      end;


      FFilterPopup.PlacementControl := Self;

      topr := GetColumnsTopRect;
      topr.Left := topr.Left - GetHorizontalScrollPosition;
      r := RectF(topr.Left + colp, topr.Top, topr.Left + colp + colw, topr.Bottom);
      szd := Columns[AColumn].Filtering.ButtonSize;
      fr := RectF(Round(r.Right - szd - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2), Round(r.Right - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2 + szd));

      pt := LocalToScreenEx(PointF(fr.Left + (fr.Right - fr.Left) - szd - ScalePaintValue(2), fr.Top + ((fr.Bottom - fr.Top) + szd) / 2));
      FFilterPopup.PlacementRectangle.Left := pt.X;
      FFilterPopup.PlacementRectangle.Top := pt.Y;

      FFilterListBox.Width := Columns[AColumn].Filtering.DropDownWidth;
      FFilterListBox.Height := Columns[AColumn].Filtering.DropDownHeight;
      FFilterPopup.DropDownHeight := Columns[AColumn].Filtering.DropDownHeight;
      FFilterPopup.DropDownWidth := Columns[AColumn].Filtering.DropDownWidth;

      FFilterPopup.PlacementRectangle.Right := pt.X + FFilterPopup.DropDownWidth;
      FFilterPopup.PlacementRectangle.Bottom := pt.Y + FFilterPopup.DropDownHeight;

      FFilterPopup.ContentControl := FFilterListBox;
      FFilterPopup.FocusedControl := FFilterListBox;

      FFilterPopup.Popup;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleFilterListClick(Sender: TObject);
begin
  FFilterTimer.Enabled := True;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomTreeView.ApplyFilterListBoxStyleLookUp(Sender: TObject);
begin
  (Sender as TListBox).ViewportPosition := PointF(0, 0);
end;
{$ENDIF}

procedure TAdvCustomTreeView.HandleFilterTimer(Sender: TObject);
var
  fd: TAdvTreeViewFilterData;
  cnd: string;
  i, filtercol: integer;
begin
  if Assigned(FFilterListBox) then
  begin
    filtercol := FFilterListBox.Tag;
    if (filtercol >= 0) and (filtercol <= Columns.Count - 1) then
    begin
      if Columns[filtercol].Filtering.MultiColumn then
      begin
        for i := Filter.Count - 1 downto 0 do
        begin
          if Filter.Items[i].Column = filtercol then
            Filter.Delete(i);
        end;
      end
      else
        Filter.Clear;

      fd := Filter.Add;
      fd.Column := filtercol;

      cnd := '';
      if (FFilterListBox.ItemIndex >= 0) and (FFilterListBox.ItemIndex <= FFilterListBox.Items.Count - 1) then
        cnd := FFilterListbox.Items[FFilterListbox.ItemIndex];

      if pos(' ',cnd) > 0 then
        cnd := '"' + cnd + '"';

      FFilterPopup.IsOpen := False;

      if cnd = sAdvTreeViewFilterAll then
      begin
        RemoveFilters;
      end
      else
      begin
        DoFilterSelect(fd.Column, cnd);
        fd.Condition := cnd;
        ApplyFilter;
      end;
    end;
  end;

  FFilterTimer.Enabled := False;
end;

procedure TAdvCustomTreeView.HandleDialogKey(var Key: Word; Shift: TShiftState);
var
  n: TAdvTreeViewVirtualNode;
  c: Integer;
  ext: Boolean;
begin
  if FInplaceEditorClosed or BlockUserInput then
  begin
    inherited;
    Exit;
  end;

  if Assigned(FInplaceEditor) and FInplaceEditorActive and ((Key = KEY_ESCAPE) or (Key = KEY_TAB) or (Key = KEY_F2)
    or ((FInplaceEditor is TEdit) and (Key = KEY_RETURN))) or ((FInplaceEditor is TMemo) and (Key = KEY_RETURN) and not (ssCtrl in Shift)) then
  begin
    FCloseWithDialogKey := True;
    CloseInplaceEditor(Key = KEY_ESCAPE);
    if Key = KEY_TAB then
    begin
      if Assigned(FFocusedNode) then
      begin
        if ssShift in Shift then
        begin
          ext := False;
          DoIsNodeExtended(FFocusedNode, ext);
          if (FUpdateNodeColumn > GetFirstEditableColumn) and not ext then
          begin
            HandleNodeEditing(FFocusedNode, GetPreviousEditableColumn(FUpdateNodeColumn));
            Invalidate;
            Key := 0;
          end
          else
          begin
            n := GetPreviousFocusableNode(FFocusedNode);
            if Assigned(n)then
            begin
              ext := False;
              DoIsNodeExtended(n, ext);
              if (ext and Interaction.ExtendedEditable) or not ext then
              begin
                if not ext then
                  c := GetLastEditableColumn
                else
                  c := 0;

                if FFocusedNode <> n then
                begin
                  FFocusedNode := n;
                  DoFocusedNodeChanged(FFocusedNode);
                end;
                SelectVirtualNode(n);
                ScrollToVirtualNode(n, True, tvnspTop);
                HandleNodeEditing(n, c);
                Invalidate;
                Key := 0;
              end;
            end;
          end;
        end
        else
        begin
          ext := False;
          DoIsNodeExtended(FFocusedNode, ext);
          if (FUpdateNodeColumn < GetLastEditableColumn) and not ext then
          begin
            EditVirtualNode(FFocusedNode, GetNextEditableColumn(FUpdateNodeColumn));
            Key := 0;
          end
          else
          begin
            n := GetNextFocusableNode(FFocusedNode);
            if Assigned(n) then
            begin
              ext := False;
              DoIsNodeExtended(n, ext);
              if (ext and Interaction.ExtendedEditable) or not ext then
              begin
                c := GetFirstEditableColumn;
                if FFocusedNode <> n then
                begin
                  FFocusedNode := n;
                  DoFocusedNodeChanged(FFocusedNode);
                end;
                SelectVirtualNode(n);
                ScrollToVirtualNode(n, True, tvnspBottom);
                HandleNodeEditing(n, c);
                Invalidate;
                Key := 0;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  inherited;
end;

procedure TAdvCustomTreeView.HandleDragDrop(const Source: TObject;
  const Point: TPointF);
var
  dn, dragn: TAdvTreeViewVirtualNode;
  b: Boolean;
begin
  inherited;

  if BlockUserInput or Assigned(OnDragDrop) then
    Exit;

  if Assigned(OnCustomDragDrop) then
    OnCustomDragDrop(Self, Source, Point)
  else
  begin
    if FAccepted and Assigned(Source) and (Source is TAdvCustomTreeView) and ((Source as TAdvCustomTreeView).Interaction.DragDropMode <> tdmNone) then
    begin
      dragn := (Source as TAdvCustomTreeView).DragNode;
      if Assigned(dragn) then
      begin
        dn := XYToNode(Point.X, Point.Y);
        if (dn <> dragn) and Assigned(dragn.Node) and PtInRectEx(GetNodesRect, Point) then
        begin
          b := True;
          DoBeforeDropNode(dragn, dn, b);
          if b then
          begin
            BeginUpdate;
            (Source as TAdvCustomTreeView).BeginUpdate;
            if Assigned(dn) and Assigned(dn.Node) then
            begin
              case (Source as TAdvCustomTreeView).Interaction.DragDropMode of
                tdmMove: dragn.Node.MoveTo(dn.Node, dn.Node.Index);
                tdmCopy: dragn.Node.CopyTo(dn.Node, dn.Node.Index);
              end;
            end
            else
            begin
              Nodes.Add.Assign(dragn.Node);
              case (Source as TAdvCustomTreeView).Interaction.DragDropMode of
                tdmMove: dragn.Node.TreeView.RemoveNode(dragn.Node);
              end;
            end;
            (Source as TAdvCustomTreeView).DragNode := nil;
            (Source as TAdvCustomTreeView).EndUpdate;
            EndUpdate;

            DoAfterDropNode(dragn, dn);
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleDragOver(const Source: TObject;
  const Point: TPointF; var Accept: Boolean);
begin
  inherited;
  if BlockUserInput or Assigned(OnDragOver) then
    Exit;

  if not AlternativeDragDrop then
  begin
    FDragMode := False;
    FDragModeStarted := False;
    IsMouseDown := False;
  end;

  if Assigned(OnCustomDragOver) then
    OnCustomDragOver(Self, Source, Point, Accept)
  else
  begin
    Accept := (Interaction.DragDropMode <> tdmNone) and (Source is TAdvCustomTreeView) and Assigned((Source as TAdvCustomTreeView).DragNode);
    Accept := Accept and PtInRectEx(GetNodesRect, Point);
  end;

  FAccepted := Accept;
end;

procedure TAdvCustomTreeView.HandleDragStart(X, Y: Single);
begin
end;

procedure TAdvCustomTreeView.HandleExpand(AColumn: Integer);
begin

end;

procedure TAdvCustomTreeView.DrawBorders(AGraphics: TAdvGraphics);
begin
end;

procedure TAdvCustomTreeView.DrawDisplay(AGraphics: TAdvGraphics; ADisplay: TAdvTreeViewDisplayList);
var
  I: Integer;
  cache: TAdvTreeViewCacheItem;
  st: TAdvGraphicsSaveState;
  r: TRectF;
begin
  if ADisplay.Count = 0 then
    Exit;

  st := AGraphics.SaveState;
  if (ADisplay is TAdvTreeViewNodeDisplayList) then
  begin
    r := GetContentClipRect;
    r.Right := r.Right + 1;
  end
  else if ADisplay is TAdvTreeViewGroupsTopDisplayList then
  begin
    r := GetGroupsTopRect;
    r.Bottom := r.Bottom + 1;
  end
  else if ADisplay is TAdvTreeViewGroupsBottomDisplayList then
  begin
    r := GetGroupsBottomRect;
    r.Top := r.Top - 1;
  end
  else if ADisplay is TAdvTreeViewColumnsTopDisplayList then
  begin
    r := GetColumnsTopRect;
    r.Bottom := r.Bottom + 1;
  end
  else if ADisplay is TAdvTreeViewColumnsBottomDisplayList then
  begin
    r := GetColumnsBottomRect;
    r.Top := r.Top - 1;
  end;

  AGraphics.ClipRect(r);

  for I := 0 to ADisplay.Count - 1 do
  begin
    cache := ADisplay[I];
    if ADisplay is TAdvTreeViewNodeDisplayList then
    begin
      if Assigned(cache.Node) then
        DrawNode(AGraphics, cache.DrawRect, cache.Node)
    end
    else if ADisplay is TAdvTreeViewColumnsDisplayList then
      DrawColumn(AGraphics, cache.DrawRect, cache.Column, cache.Kind)
    else if ADisplay is TAdvTreeViewGroupsDisplayList then
      DrawGroup(AGraphics, cache.DrawRect, cache.Group, cache.StartColumn, cache.EndColumn, cache.Kind)
  end;

  AGraphics.RestoreState(st);
end;

procedure TAdvCustomTreeView.DrawEmptySpaces(AGraphics: TAdvGraphics);
begin
end;

procedure TAdvCustomTreeView.DrawGroup(AGraphics: TAdvGraphics; ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
end;

procedure TAdvCustomTreeView.DrawGroups(AGraphics: TAdvGraphics);
begin
  DrawDisplay(AGraphics, FGroupsTopDisplay);
  DrawDisplay(AGraphics, FGroupsBottomDisplay);
end;

procedure TAdvCustomTreeView.DrawNode(AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False; AOffsetX: Single = 0; AOffsetY: Single = 0);
begin
end;

procedure TAdvCustomTreeView.DrawNodeColumns(AGraphics: TAdvGraphics);
begin

end;

procedure TAdvCustomTreeView.DrawNodes(AGraphics: TAdvGraphics);
begin
  DrawDisplay(AGraphics, FNodeDisplay);
end;

procedure TAdvCustomTreeView.DrawColumn(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
end;

procedure TAdvCustomTreeView.DrawColumns(AGraphics: TAdvGraphics);
begin
  DrawDisplay(AGraphics, FColumnsTopDisplay);
  DrawDisplay(AGraphics, FColumnsBottomDisplay);
end;

procedure TAdvCustomTreeView.EditNode(ANode: TAdvTreeViewNode; AColumn: Integer);
begin
  if Assigned(ANode) then
    EditVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode, AColumn);
end;

procedure TAdvCustomTreeView.EditVirtualNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  HandleNodeEditing(ANode, AColumn);
  Invalidate;
end;

procedure TAdvCustomTreeView.ExportNotification(
  AState: TAdvTreeViewExportState; ARow: Integer);
begin

end;

function TAdvCustomTreeView.GetColumnBottomLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsBottomRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetColumnBottomRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsBottomRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right - 1;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetCacheHeight: Integer;
begin
  Result := CACHEHEIGHT;
end;

function TAdvCustomTreeView.GetCacheWidth: Integer;
begin
  Result := CACHEWIDTH
end;

function TAdvCustomTreeView.GetCalculationRect: TRectF;
begin
  Result := inherited GetCalculationRect;
  Result.Top := Result.Top + GetColumnsTopSize + GetGroupsTopSize;
  Result.Bottom := Result.Bottom - GetColumnsBottomSize - GetGroupsBottomSize;
  Result.Right := Result.Left + Max(0, Result.Right - Result.Left);
  Result.Bottom := Result.Top + Max(0, Result.Bottom - Result.Top);
end;

function TAdvCustomTreeView.GetContentCliprect: TRectF;
begin
  Result := inherited GetContentClipRect;
  Result.Bottom := Result.Top + Min(GetTotalRowHeight, Result.Bottom - Result.Top);
  Result.Right := Result.Left + Min(GetTotalColumnWidth, Result.Right - Result.Left);
  Result.Left := Result.Left + 1;
  Result.Right := Result.Right - 1;
end;

function TAdvCustomTreeView.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  Result.Top := Result.Top + GetColumnsTopSize + GetGroupsTopSize;
  Result.Bottom := Result.Bottom - GetColumnsBottomSize - GetGroupsBottomSize;
end;

function TAdvCustomTreeView.GetDocURL: string;
begin
  Result := TAdvTreeViewDocURL;
end;

function TAdvCustomTreeView.GetDragObjectScreenShot: TAdvBitmap;
begin
  CreateDragBitmap;
  Result := TAdvBitmap.Create;
  if Assigned(FDragBitmap) then
  begin
    Result.Assign(FDragBitmap.GetBitmap);
    FDragBitmap.Free;
    FDragBitmap := nil;
  end;
end;

function TAdvCustomTreeView.GetGroupBottomLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsBottomRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := nr.Bottom;
end;

function TAdvCustomTreeView.GetGroupBottomRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsBottomRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetGroupsBottomRect: TRectF;
begin
  Result := GetColumnsBottomRect;
  Result.Top := Result.Bottom;
  Result.Bottom := Result.Top + GetGroupsBottomSize
end;

function TAdvCustomTreeView.GetGroupsBottomSize: Double;
begin
  Result := 0;
  if (tglBottom in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (ColumnCount > 0) then
    Result := GroupsAppearance.BottomSize;
end;

function TAdvCustomTreeView.GetGroupsTopRect: TRectF;
begin
  Result := GetColumnsTopRect;
  Result.Bottom := Result.Top;
  Result.Top := Result.Top - GetGroupsTopSize;
end;

function TAdvCustomTreeView.GetGroupsTopSize: Double;
begin
  Result := 0;
  if (tglTop in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (ColumnCount > 0) then
    Result := GroupsAppearance.TopSize;
end;

function TAdvCustomTreeView.GetGroupText(AGroup: Integer): String;
begin
  Result := TranslateTextEx(sAdvTreeViewGroup) + ' ' + inttostr(AGroup);
  if (AGroup >= 0) and (AGroup <= Groups.Count - 1) then
     Result := TAdvTreeViewGroupOpen(Groups[AGroup]).GetText;
end;

function TAdvCustomTreeView.GetGroupTopLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsTopRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetGroupTopRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsTopRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetHTMLTemplate(AColumnIndex: Integer): string;
begin
  if (AColumnIndex >= 0) and (AColumnIndex < Columns.Count) then
    Result := Columns[AColumnIndex].HTMLTemplate
  else
    Result := '';
end;

function TAdvCustomTreeView.GetInplaceEditor: TAdvTreeViewInplaceEditor;
begin
  Result := FInplaceEditor;
end;

function TAdvCustomTreeView.GetInplaceEditorRect(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer): TRectF;
var
  cr, r: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(ANode) then
    Exit;

  if (AColumn >= 0) and (AColumn <= Length(ANode.TextRects) - 1) then
  begin
    r := ANode.TextRects[AColumn];
    {$IFDEF CMNLIB}
    InflateRectEx(r, ScalePaintValue(-2), ScalePaintValue(-2));
    {$ENDIF}
    cr := GetContentClipRect;
    r.Top := Max(r.Top, cr.Top);
    r.Bottom := Min(r.Bottom, cr.Bottom);
    r.Left := Max(r.Left, cr.Left);
    r.Right := Min(r.Right, cr.Right);
    Result := RectF(Floor(r.Left), Floor(r.Top), Round(r.Right), Round(r.Bottom));

    if Assigned(OnGetInplaceEditorRect) then
      OnGetInplaceEditorRect(Self, ANode, AColumn, FInplaceEditor, Result);
  end;
end;

function TAdvCustomTreeView.GetColumnsBottomRect: TRectF;
var
  nr: TRectF;
begin
  nr := inherited GetContentRect;
  Result.Left := nr.Left;
  Result.Top := nr.Bottom - GetGroupsBottomSize - GetColumnsBottomSize;
  Result.Right := nr.Right + GetColumnsExtraSize;
  Result.Bottom := nr.Bottom - GetGroupsBottomSize;
end;

function TAdvCustomTreeView.GetColumnsBottomSize: Double;
begin
  Result := 0;
  if (tclBottom in ColumnsAppearance.Layouts) and (ColumnCount > 0) then
    Result := ColumnsAppearance.BottomSize;
end;

function TAdvCustomTreeView.GetColumnsExtraSize: Double;
begin
  Result := 0;
end;

function TAdvCustomTreeView.GetColumnTopLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsTopRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetColumnTopRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsTopRect;
  nr := LocalRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right - 1;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

function TAdvCustomTreeView.GetVisibleNodeCount: Integer;
var
  f, l: TAdvTreeViewVirtualNode;
begin
  Result := 0;
  f := GetFirstVisibleVirtualNode;
  l := GetLastVisibleVirtualNode;
  if Assigned(f) and Assigned(l) then
    Result := l.Row - f.Row;
end;

function TAdvCustomTreeView.GetColumnsTopRect: TRectF;
var
  nr: TRectF;
begin
  nr := inherited GetContentRect;
  Result.Left := nr.Left;
  Result.Top := nr.Top + GetGroupsTopSize;
  Result.Right := nr.Right + GetColumnsExtraSize;
  Result.Bottom := nr.Top + GetGroupsTopSize + GetColumnsTopSize;
end;

function TAdvCustomTreeView.GetColumnsTopSize: Double;
begin
  Result := 0;
  if (tclTop in ColumnsAppearance.Layouts) and (ColumnCount > 0) then
    Result := ColumnsAppearance.TopSize;
end;

function TAdvCustomTreeView.GetColumnText(AColumn: Integer): String;
begin
  Result := TranslateTextEx(sAdvTreeViewColumn) + ' ' + inttostr(AColumn);
  if (AColumn >= 0) and (AColumn <= ColumnCount - 1) then
  begin
    if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
    begin
      Result := Columns[AColumn].Text;
      if Result = '' then
        Result := Columns[AColumn].Name;
    end;

    if Result = '' then
      Result := TranslateTextEx(sAdvTreeViewColumn) + ' ' + inttostr(AColumn);
  end;

  if Assigned(Adapter) then
    Result := Adapter.GetColumnDisplayName(AColumn);
end;

function TAdvCustomTreeView.GetPreviousFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  en: Boolean;
begin
  if Assigned(ANode) then
    Result := GetPreviousVirtualNode(ANode)
  else
    Result := GetLastVirtualNode;

  en := IsVirtualNodeSelectable(Result);

  while Assigned(Result) and not en do
  begin
    Result := GetPreviousVirtualNode(Result);
    en := IsVirtualNodeSelectable(Result);
  end;
end;

procedure TAdvCustomTreeView.GetNodeMargins(var ALeft, ATop, ARight, ABottom: Single);
begin

end;

procedure TAdvCustomTreeView.GetNodeOffset(ANode: TAdvTreeViewVirtualNode;
  var ALeft, ATop, ARight, ABottom: Single);
begin

end;

function TAdvCustomTreeView.GetNodesRect: TRectF;
begin
  Result := GetContentRect;
end;

function TAdvCustomTreeView.GetNodesSpacing: Single;
begin
  Result := NodesAppearance.Spacing;
end;

function TAdvCustomTreeView.GetNodesFromClipboard: TAdvTreeViewCopyNodes;
var
  s: String;
  a: TStringList;
  ANode, ANextNode: TAdvTreeViewNode;
  ALevel: Integer;
  I, off: Integer;
  CurrStr: String;
  tempt: TAdvCustomTreeView;
begin
  if TAdvClipBoard.HasFormat(TAdvClipBoardFormat.cfText) then
  begin
    s := TAdvClipBoard.GetText;
    if (Pos(CLP_FMT, s) > 0) then
    begin
      tempt := TAdvCustomTreeView.Create(nil);
      a := TStringList.Create;
      try
        s := StringReplace(s, CLP_FMT, '', [rfReplaceAll]);
        a.Text := s;
        try
          ANode := nil;
          off := 0;
          for I := 0 to a.Count - 1 do
          begin
            ALevel := -1;
            CurrStr := GetBufStart(a[i], ALevel);

            if ANode = nil then
            begin
              ANode := tempt.AddNode;
              off := ALevel;
            end
            else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel - off) then
              ANode := tempt.AddNode(ANode.GetParent)
            else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level = ALevel - 1 - off) then
              ANode := tempt.AddNode(ANode)
            else if Assigned(ANode.VirtualNode) and (ANode.VirtualNode.Level > ALevel - off) then
            begin
              ANextNode := ANode.GetParent;
              while Assigned(ANextNode) and Assigned(ANextNode.VirtualNode) and (ANextNode.VirtualNode.Level > ALevel - off) do
                ANextNode := ANextNode.GetParent;
              ANode := tempt.AddNode(ANextNode.GetParent);
            end;

            if Assigned(ANode) then
              ANode.LoadFromString(CurrStr);
          end;
        finally
        end;
      finally
        FCopyNodes.Assign(tempt.Nodes);
        a.Free;
        tempt.Free;
      end;
    end;
  end;

  Result := FCopyNodes;
end;

function TAdvCustomTreeView.GetNextFocusableNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  en: Boolean;
begin
  if Assigned(ANode) then
    Result := GetNextVirtualNode(ANode)
  else
    Result := GetFirstRootVirtualNode;

  en := IsVirtualNodeSelectable(Result);

  while Assigned(Result) and not en do
  begin
    Result := GetNextVirtualNode(Result);
    en := IsVirtualNodeSelectable(Result);
  end;
end;

function TAdvCustomTreeView.GetReloadOffset: Single;
begin
  Result := 0;
end;

function TAdvCustomTreeView.GetRowHeight(ARow: Integer): Double;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := DefaultRowHeight;
  v := GetVisibleNodeForIndex(ARow);
  if Assigned(v) and (v.Calculated) then
    Result := v.Height;
end;

procedure TAdvCustomTreeView.HorizontalScrollPositionChanged;
begin
  BlockScrollingUpdate := True;
  UpdateDisplay;
  BlockScrollingUpdate := False;
  DoHScroll(GetHScrollValue);
end;

procedure TAdvCustomTreeView.Clear;
begin
  BeginUpdate;
  ClearNodes;
  ClearColumns;
  ClearSorting;
  EndUpdate;
end;

procedure TAdvCustomTreeView.ClearSorting;
var
  I: Integer;
begin
  BeginUpdate;
  FSortColumn := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    TAdvTreeViewColumnOpen(Columns[I]).SortKind := nskNone;
    TAdvTreeViewColumnOpen(Columns[I]).SortIndex := -1;
  end;
  EndUpdate;
end;

procedure TAdvCustomTreeView.Sort(AColumn: Integer = 0; ARecurse: Boolean = False; ACaseSensitive: Boolean = True; ASortingMode: TAdvTreeViewNodesSortMode = nsmAscending; AClearNodeList: Boolean = True);
begin
  Nodes.Sort(AColumn, ARecurse, ACaseSensitive, ASortingMode, AClearNodeList);
end;

procedure TAdvCustomTreeView.InitializeColumnSorting(AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode);
var
  I: Integer;
begin
  UpdateCount := UpdateCount + 1;
  FSortColumn := AColumn;
  for I := 0 to Columns.Count - 1 do
  begin
    case ASortMode of
      nsmAscending: Columns[I].SortKind := nskAscending;
      nsmDescending: Columns[I].SortKind := nskDescending;
    end;
  end;
  UpdateCount := UpdateCount - 1;
end;

procedure TAdvCustomTreeView.InitSample;
var
  pManagers, pSpecialists, pAssistants, pSub: TAdvTreeViewNode;
  c: TAdvTreeViewColumn;
  I: Integer;
  n: string;
begin
  BeginUpdate;

  Width := ScalePaintValue(350);

  ClearNodeList;
  Columns.Clear;
  Nodes.Clear;

  ResetToDefaultStyle;

  NodesAppearance.ExpandWidth := ScalePaintValue(18);
  NodesAppearance.ExpandHeight := ScalePaintValue(18);

//  {$IFNDEF WEBLIB}
//  NodesAppearance.CollapseNodeIcon.LoadFromResource('TAdvTREEVIEWCOLLAPSENEWSVG');
//  NodesAppearance.ExpandNodeIcon.LoadFromResource('TAdvTREEVIEWEXPANDNEWSVG');
//  NodesAppearance.CollapseNodeIconLarge.LoadFromResource('TAdvTREEVIEWCOLLAPSENEWSVG');
//  NodesAppearance.ExpandNodeIconLarge.LoadFromResource('TAdvTREEVIEWEXPANDNEWSVG');
//  {$ENDIF}

  ColumnsAppearance.TopSize := ScalePaintValue(36);
  ColumnsAppearance.TopStroke.Kind := gskNone;
  NodesAppearance.SelectedStroke.Width := Trunc(ScalePaintValue(2.0));

  c := Columns.Add;
  c.Text := 'Name';
  c.HorizontalTextAlign := gtaLeading;

  c := Columns.Add;
  c.Text := 'Unit';
  c.HorizontalTextAlign := gtaTrailing;
  c.Width := ScalePaintValue(90);

  c := Columns.Add;
  c.Text := 'Status';
  c.HorizontalTextAlign := gtaCenter;
  c.Width := ScalePaintValue(80);

  pManagers := AddNode;
  pManagers.Text[0] := 'Managers';

  pSpecialists := AddNode;
  pSpecialists.Text[0] := 'Specialists';

  pAssistants := AddNode;
  pAssistants.Text[0] := 'Assistants';

  for I := 0 to 7 do
  begin
    if I < 3 then
      pSub := AddNode(pManagers)
    else if I < 5  then
      pSub := AddNode(pSpecialists)
    else
      pSub := AddNode(pAssistants);

    case Random(125) mod 7 of
      0: n := 'Liam';
      1: n := 'Fatma';
      2: n := 'Yusuf';
      3: n := 'Marie';
      4: n := 'Isabella';
      5: n := 'Omar';
      else
        n := 'Arthur';
    end;

    case Random(125) mod 7 of
      0: n := n + ' Andersson';
      1: n := n + ' Wang';
      2: n := n + ' Smith';
      3: n := n + ' Peeters';
      4: n := n + ' Gonzales';
      5: n := n + ' Moyo';
      else
        n := n + ' Ali';
    end;

    pSub.Text[0] := n;

    case Random(120) mod 7 of
      1: pSub.Text[1] := 'Research';
      2: pSub.Text[1] := 'Finance';
      3: pSub.Text[1] := 'Manufactering';
      4: pSub.Text[1] := 'Sales';
      5: pSub.Text[1] := 'HR';
      6: pSub.Text[1] := 'Marketing';
      else
        pSub.Text[1] := 'Customers';
    end;

    case Random(120) mod 5 of
      0: pSub.Text[2] := 'Flight';
      1: pSub.Text[2] := 'On Leave';
      2: pSub.Text[2] := 'Abroad';
      else
        pSub.Text[2] := 'Office';
    end;
  end;

  pManagers.Expanded := True;
  pSpecialists.Expanded := True;

  GlobalFont.Name := 'Segoe UI';

  ColumnsAppearance.StretchAll := False;
  ColumnsAppearance.Stretch := True;
  ColumnsAppearance.StretchColumn := 0;

  EndUpdate;
end;

procedure TAdvCustomTreeView.InternalSelectVirtualNode(
  ANode: TAdvTreeViewVirtualNode);
begin

end;

procedure TAdvCustomTreeView.HandleKeyDown(var Key: Word; Shift: TShiftState);
var
  sel, prevsel, fsel: TAdvTreeViewVirtualNode;
  sa, spc, donext, doprev: Boolean;
  r: Integer;
  I: Integer;
  b: Boolean;
  a: Boolean;
  n: TAdvTreeViewVirtualNode;
begin
  inherited;
  if FInplaceEditorClosed or BlockUserInput then
    Exit;

  if Key in [KEY_ESCAPE,KEY_INSERT,KEY_DELETE,KEY_UP,KEY_DOWN,KEY_LEFT,KEY_RIGHT,KEY_HOME,KEY_END,
    KEY_PRIOR,KEY_NEXT,KEY_TAB,KEY_MULTIPLY,KEY_SUBTRACT,KEY_ADD,KEY_DIVIDE] then
      FLookupString := '';

  if (ssCtrl in Shift) or (ssCommand in Shift) then
  begin
    case Key of
      Ord('A'):
      begin
        if Interaction.MultiSelect then
        begin
          DoBeforeSelectAllNodes(sa);
          if sa then
          begin
            SelectAllNodes;
            DoAfterSelectAllNodes;
          end;
        end;
        Exit;
      end;
      Ord('X'):
      begin
        if Interaction.ClipboardMode <> tcmNone then
        begin
          b := True;
          DoBeforeCutToClipboard(b);
          if b then
          begin
            CutToClipboard(Interaction.ClipboardMode = tcmTextOnly);
            DoAfterCutToClipboard;
          end;
        end;
        Exit;
      end;
      Ord('C'):
      begin
        if Interaction.ClipboardMode <> tcmNone then
        begin
          b := True;
          DoBeforeCopyToClipboard(b);
          if b then
          begin
            CopyToClipboard(Interaction.ClipboardMode = tcmTextOnly);
            DoAfterCopyToClipboard;
          end;
        end;
        Exit;
      end;
      Ord('V'):
      begin
        if Interaction.ClipboardMode <> tcmNone then
        begin
          b := True;
          DoBeforePasteFromClipboard(b);
          if b then
          begin
            PasteFromClipboard;
            DoAfterPasteFromClipboard;
          end;
        end;
        Exit;
      end;
    end;
  end;

  sel := FocusedVirtualNode;
  donext := False;
  doprev := False;
  case Key of
    KEY_MENU, KEY_CONTROL, KEY_SHIFT: sel := nil;
    KEY_PRIOR:
    begin
      doprev := True;
      if Assigned(sel) then
      begin
        r := sel.Row - (StopRow - StartRow);
        sel := GetNodeFromNodeStructure(r);
        if Assigned(sel) then
        begin
          if not IsVirtualNodeSelectable(sel) then
            sel := GetPreviousFocusableNode(sel);
        end;

        if not Assigned(sel) then
          sel := GetNextFocusableNode(nil);
      end;
    end;
    KEY_NEXT:
    begin
      donext := True;
      if Assigned(sel) then
      begin
        r := sel.Row + (StopRow - StartRow);
        sel := GetNodeFromNodeStructure(r);
        if Assigned(sel) then
        begin
          if not IsVirtualNodeSelectable(sel) then
            sel := GetNextFocusableNode(sel);
        end;

        if not Assigned(sel) then
          sel := GetPreviousFocusableNode(nil);
      end;
    end;
    KEY_HOME:
    begin
      sel := GetNextFocusableNode(nil);
      doprev := True;
    end;
    KEY_END:
    begin
      sel := GetPreviousFocusableNode(nil);
      doprev := True
    end;
    KEY_DOWN:
    begin
      if not Interaction.MultiSelect or (Interaction.MultiSelect and not ((msSiblingOnly in Interaction.MultiSelectStyle) and (ssCtrl in Shift))) then
      begin
        sel := GetNextFocusableNode(sel);
        donext := True;
      end;
    end;
    KEY_UP:
    begin
      if not (msSiblingOnly in Interaction.MultiSelectStyle) or not Interaction.MultiSelect or not (ssCtrl in Shift) then
      begin
        sel := GetPreviousFocusableNode(sel);
        doprev := True;
      end;
    end;
    KEY_LEFT, KEY_SUBTRACT, KEY_DIVIDE: HandleNodeCollapse(sel, KEY = KEY_DIVIDE);
    KEY_RIGHT, KEY_ADD, KEY_MULTIPLY: HandleNodeExpand(sel, KEY = KEY_MULTIPLY);
    KEY_SPACE: DoNodeClick(sel);
  end;

  case Key of
    KEY_LEFT, KEY_RIGHT: HandleCustomKeys(Key);
  end;

  spc := False;
  if (Key = KEY_SPACE) then
    spc := True;

  if Assigned(sel) then
  begin
    if (Interaction.SelectionFollowsFocus and (donext or doprev)) or (spc and ((ssCtrl in Shift) or (ssCommand in Shift))) then
    begin
      if (ssShift in Shift) then
      begin
        if (FSelectedNodes.Count > 1) then
        begin
          prevsel := FSelectedNodes[FSelectedNodes.Count - 1];
          fsel := FSelectedNodes[0];
          if (doprev and (sel.Row < prevsel.Row) and (sel.Row >= fsel.Row)) or (donext and (sel.Row > prevsel.Row) and (sel.Row <= fsel.Row)) then
          begin
            if prevsel.Row <= sel.Row then
            begin
              for I := prevsel.Row to sel.Row do
              begin
                n := GetNodeFromNodeStructure(I);
                if Assigned(n) then
                begin
                  if n <> sel then
                  begin
                    a := True;
                    DoBeforeUnSelectNode(n, a);
                    if a then
                    begin
                      UnSelectVirtualNode(n);
                      DoAfterUnSelectNode(n);
                    end;
                  end
                  else
                    UnSelectVirtualNode(n);
                end;
              end;
            end
            else
            begin
              for I := prevsel.Row downto sel.Row do
              begin
                n := GetNodeFromNodeStructure(I);
                if Assigned(n) then
                begin
                  if n <> sel then
                  begin
                    a := True;
                    DoBeforeUnSelectNode(n, a);
                    if a then
                    begin
                      UnSelectVirtualNode(n);
                      DoAfterUnSelectNode(n);
                    end;
                  end
                  else
                    UnSelectVirtualNode(n);
                end;
              end;
            end;
          end;
        end;
      end
      else if not (ssCtrl in Shift) and not (ssCommand in Shift) and (donext or doprev) and (FSelectedNodes.Count > 0) then
      begin
        a := True;
        DoBeforeUnSelectNode(sel, a);
        if a then
        begin
          UnSelectVirtualNode(sel);
          DoAfterUnSelectNode(sel);
        end;
      end;

      if (ssShift in Shift) then
      begin
        if Assigned(FFocusedNode) then
        begin
          if FFocusedNode.Row <= sel.Row then
          begin
            for I := FFocusedNode.Row + 1 to sel.Row do
              HandleSelectNode(GetNodeFromNodeStructure(I), True, True, {$IFNDEF FMXMOBILE}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
          end
          else
          begin
            for I := FFocusedNode.Row - 1 downto sel.Row do
              HandleSelectNode(GetNodeFromNodeStructure(I), True, True, {$IFNDEF FMXMOBILE}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
          end;
        end;
      end
      else
         HandleSelectNode(sel, True, True, {$IFNDEF FMXMOBILE}((ssCtrl in Shift) or (ssCommand in Shift)) and {$ENDIF} Interaction.MultiSelect);
    end
    else if spc then
      HandleNodeToggleCheck(sel, 0);

    if FFocusedNode <> sel then
    begin
      FFocusedNode := sel;
      DoFocusedNodeChanged(FFocusedNode);
    end;

    if donext then
      ScrollToVirtualNode(FFocusedNode, True, tvnspBottom)
    else
      ScrollToVirtualNode(FFocusedNode, True, tvnspTop);
    Invalidate;
  end;
end;

procedure TAdvCustomTreeView.HandleKeyPress(var Key: Char);
var
  k: Integer;
begin
  inherited;
  if BlockUserInput then
    Exit;

  k := Ord(Key);

  if Interaction.Lookup.Enabled then
  begin
    if k in [KEY_ESCAPE,KEY_INSERT,KEY_DELETE,KEY_UP,KEY_DOWN,KEY_LEFT,KEY_RIGHT,KEY_HOME,KEY_END,KEY_PRIOR,KEY_NEXT,KEY_TAB, 0] then
      FLookupString := ''
    else
    begin
      if Interaction.Lookup.Incremental then
        FLookupString := FLookupString + Key
      else
        FLookupString := Key;

      if Assigned(OnCustomLookup) then
        OnCustomLookup(Self, FLookupString)
      else
      begin
        LookupNode(FLookupString, Interaction.Lookup.RootNodesOnly, Interaction.Lookup.Column,
          Interaction.Lookup.CaseSensitive, Interaction.Lookup.AutoSelect, Interaction.Lookup.AutoExpand);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleKeyUp(var Key: Word; Shift: TShiftState);
var
  en, ext: Boolean;
  c: Integer;
begin
  inherited;
  if BlockUserInput then
    Exit;

  if FInplaceEditorClosed then
  begin
    FInplaceEditorClosed := False;
    Exit;
  end;

  if FCloseWithDialogKey then
  begin
    FCloseWithDialogKey := False;
    Exit;
  end;

  if Assigned(FFocusedNode) then
  begin
    if not FCompactMode and Interaction.KeyboardEdit and not Interaction.ReadOnly and (Shift = []) then
    begin
      case Key of
        KEY_F2, KEY_RETURN:
        begin
          en := True;
          DoIsNodeEnabled(FFocusedNode, en);
          if en then
          begin
            ext := False;
            DoIsNodeExtended(FFocusedNode, ext);
            if (ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext then
            begin
              c := GetFirstEditableColumn;
              if (c <> -1) and FFocusedNode.TitleExpanded[c] then
                HandleNodeEditing(FFocusedNode, c);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  st: TAdvGraphicsSaveState;
begin
  inherited;
  {$IFDEF WEBLIB}
  if UpdateCount > 0 then
    Exit;
  {$ENDIF}

  AGraphics.PictureContainer := PictureContainer;
  DrawEmptySpaces(AGraphics);
  st := AGraphics.SaveState;
  AGraphics.ClipRect(GetContentClipRect);
  DrawNodeColumns(AGraphics);
  AGraphics.RestoreState(st);
  DrawNodes(AGraphics);
  DrawColumns(AGraphics);
  DrawGroups(AGraphics);
  DrawBorders(AGraphics);
end;

procedure TAdvCustomTreeView.ResetNodes(AUpdateAll: Boolean = True);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
  st, stp: Integer;
begin
  if Assigned(VisibleNodes) then
  begin
    if AUpdateAll then
    begin
      st := 0;
      stp := RowCount - 1;
    end
    else
    begin
      st := StartRow;
      stp := StopRow;
    end;

    for I := st to stp do
    begin
      v := GetVisibleNodeForIndex(I);
      if Assigned(v) then
      begin
        TotalRowHeight := TotalRowHeight - v.Height + DefaultRowHeight;
        UpdateNodeHeight(v, DefaultRowHeight);
        UpdateNodeCalculated(v, False);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.ResetToDefaultStyle;
begin
  BeginUpdate;

  TAdvUtils.SetFontSize(ColumnsAppearance.TopFont, 14);
  ColumnsAppearance.TopFont.Style := [TFontStyle.fsBold];
  {$IFDEF FMXLIB}
  ColumnsAppearance.TopFill.Color := $FFEEF2F9;
  ColumnsAppearance.TopFont.Color := $FF454545;
  NodesAppearance.SelectedFill.Color := $FFF6F8FC;
  NodesAppearance.SelectedStroke.Color := $FF2D9BEF;
  NodesAppearance.Font.Color := $FF7A7A7A;
  NodesAppearance.SelectedFontColor := $FF454545;
  NodesAppearance.SelectedStroke.Color := $FF2D9BEF;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  ColumnsAppearance.TopFill.Color := $F9F2EE;
  ColumnsAppearance.TopFont.Color := $454545;
  NodesAppearance.SelectedFill.Color := $FCF8F6;
  NodesAppearance.SelectedStroke.Color := $EF9B2D;
  NodesAppearance.Font.Color := $7A7A7A;
  NodesAppearance.SelectedFontColor := $454545;
  NodesAppearance.SelectedStroke.Color := $EF9B2D;
  {$ENDIF}
  NodesAppearance.ShowFocus := False;
  NodesAppearance.ExpandWidth := ScalePaintValue(18);
  NodesAppearance.ExpandHeight := ScalePaintValue(18);

  NodesAppearance.ShowFocus := False;
  NodesAppearance.SelectedStroke.Width := Trunc(ScalePaintValue(2.0));

  ColumnsAppearance.TopSize := ScalePaintValue(36);
  ColumnsAppearance.TopStroke.Kind := gskNone;

  Fill.Kind := gfkSolid;
  Stroke.Kind := gskSolid;
  Fill.Color := gcWhite;
  Stroke.Color := gcDarkGray;
  NodesAppearance.ExtendedFill.Color := TAdvTreeViewColorExtended;
  ColumnsAppearance.BottomFill.Color := ColumnsAppearance.TopFill.Color;
  ColumnsAppearance.TopStroke.Color := gcDarkGray;
  ColumnsAppearance.BottomStroke.Color := gcDarkgray;
  ColumnsAppearance.BottomFont.Color := ColumnsAppearance.TopFont.Color;
  GroupsAppearance.TopFill.Color := gcWhite;
  GroupsAppearance.BottomFill.Color := gcWhite;
  GroupsAppearance.TopStroke.Color := gcDarkGray;
  GroupsAppearance.BottomStroke.Color := gcDarkgray;
  GroupsAppearance.TopFont.Color := gcBlack;
  GroupsAppearance.BottomFont.Color := gcBlack;
  ColumnsAppearance.TopFill.Kind := gfkSolid;
  ColumnsAppearance.BottomFill.Kind := gfkNone;
  ColumnsAppearance.BottomStroke.Kind := gskSolid;
  GroupsAppearance.TopFill.Kind := gfkNone;
  GroupsAppearance.BottomFill.Kind := gfkNone;
  GroupsAppearance.TopStroke.Kind := gskSolid;
  GroupsAppearance.BottomStroke.Kind := gskSolid;
  NodesAppearance.ExtendedFontColor := ColumnsAppearance.TopFont.Color;
  ColumnsAppearance.BottomFont.Color := ColumnsAppearance.TopFont.Color;
  GroupsAppearance.TopFont.Color := ColumnsAppearance.TopFont.Color;
  GroupsAppearance.BottomFont.Color := ColumnsAppearance.TopFont.Color;

  EndUpdate;
end;

procedure TAdvCustomTreeView.SetInteraction(const Value: TAdvTreeViewInteraction);
begin
  if FInteraction <> Value then
    FInteraction.Assign(Value);
end;

procedure TAdvCustomTreeView.SetNodesAppearance(const Value: TAdvTreeViewNodesAppearance);
begin
  if FNodesAppearance <> Value then
    FNodesAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetSelectedNode(
  const Value: TAdvTreeViewNode);
begin
  if Assigned(Value) then
    SelectedVirtualNode := TAdvTreeViewNodeOpen(Value).VirtualNode
  else
    SelectedVirtualNode := nil;
end;

procedure TAdvCustomTreeView.SetSelectedVirtualNode(
  const Value: TAdvTreeViewVirtualNode);
begin
  FocusedVirtualNode := Value;
  SelectVirtualNode(Value);
end;

procedure TAdvCustomTreeView.SetSortColumn(const Value: Integer);
begin
  if FSortColumn <> Value then
  begin
    FSortColumn := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomTreeView.SetTopRow(const Value: Integer);
begin
  ScrollToVirtualNodeRow(Value, True, tvnspTop, True);
end;

procedure TAdvCustomTreeView.SetVerticalOffset(AVerticalOffset: Single);
begin
  FVerticalOffsetTo := AVerticalOffset;
end;

procedure TAdvCustomTreeView.SetGlobalFont( const Value: TAdvAppearanceGlobalFont);
begin
  FGlobalFont.Assign(Value);
end;

procedure TAdvCustomTreeView.SetGroupsAppearance(
  const Value: TAdvTreeViewGroupsAppearance);
begin
  if FGroupsAppearance <> Value then
    FGroupsAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetAdapter(const Value: TAdvTreeViewAdapter);
begin
  if Assigned(Value) then
    Value.TreeView := Self;
  FAdapter := Value;
end;

procedure TAdvCustomTreeView.SetColumnsAppearance(
  const Value: TAdvTreeViewColumnsAppearance);
begin
  if FColumnsAppearance <> Value then
    FColumnsAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetColumnStroke(const Value: TAdvGraphicsStroke);
begin
  if FColumnStroke <> Value then
    FColumnStroke.Assign(Value);
end;

procedure TAdvCustomTreeView.SetCompactMode(const Value: Boolean);
begin
  if FCompactMode <> Value then
  begin
    FCompactMode := Value;
    UpdateTreeView;
  end;
end;

{$IFDEF FNCLIB}
procedure TAdvCustomTreeView.SetDefaultViewJSONOptions(const Value: TAdvTreeViewViewJSONOptions);
begin
  FDefaultViewJSONOptions := Value;
end;
{$ENDIF}

procedure TAdvCustomTreeView.SetFocusedNode(
  const Value: TAdvTreeViewNode);
begin
  if Assigned(Value) then
    FocusedVirtualNode := TAdvTreeViewNodeOpen(Value).VirtualNode
  else
    FocusedVirtualNode := nil;
end;

procedure TAdvCustomTreeView.SetFocusedVirtualNode(
  const Value: TAdvTreeViewVirtualNode);
begin
  FFocusedNode := Value;
  Invalidate;
end;

procedure TAdvCustomTreeView.SetFonts(ASetType: TAdvAppearanceGlobalFontType);
begin
  BeginUpdate;

  GlobalFont.ApplyChange(NodesAppearance.Font, ASetType);
  GlobalFont.ApplyChange(NodesAppearance.ExtendedFont, ASetType);
  GlobalFont.ApplyChange(NodesAppearance.TitleFont, ASetType);

  GlobalFont.ApplyChange(ColumnsAppearance.TopFont, ASetType);
  GlobalFont.ApplyChange(ColumnsAppearance.BottomFont, ASetType);

  GlobalFont.ApplyChange(GroupsAppearance.TopFont, ASetType);
  GlobalFont.ApplyChange(GroupsAppearance.BottomFont, ASetType);

  if ASetType = aftColor then
  begin
    NodesAppearance.SelectedFontColor := GlobalFont.Color;
    NodesAppearance.ExtendedFontColor := GlobalFont.Color;
    NodesAppearance.SelectedTitleFontColor := GlobalFont.Color;
  end;

  EndUpdate;
end;

procedure TAdvCustomTreeView.StartReload;
begin
  FVerticalOffsetTo := GetReloadOffset;
  StartVerticalOffsetAnimation;
end;

procedure TAdvCustomTreeView.StartVerticalOffsetAnimation;
begin
  FAnimateVerticalOffset := True;
  FAnimateTimer.Enabled := True;
end;

procedure TAdvCustomTreeView.StopAnimationTimer;
begin
  FAnimateTimer.Enabled := False;
  FAnimating := False;
end;

procedure TAdvCustomTreeView.StopEditing;
begin
  if FInplaceEditorActive then
    CloseInplaceEditor(False);
end;

procedure TAdvCustomTreeView.UnSelectAllNodes;
begin
  UnSelectAllVirtualNodes;
end;

procedure TAdvCustomTreeView.UnSelectAllVirtualNodes;
begin
  FSelectedNodes.Clear;
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    UnSelectVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

procedure TAdvCustomTreeView.UnSelectNodes(ANodes: TAdvTreeViewNodeArray);
var
  I: Integer;
  v: TAdvTreeViewNode;
begin
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
      FSelectedNodes.Remove(TAdvTreeViewNodeOpen(v).VirtualNode);
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectVirtualNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  FSelectedNodes.Remove(ANode);
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectVirtualNodes(
  ANodes: TAdvTreeViewVirtualNodeArray);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    FSelectedNodes.Remove(v);
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateAutoSizing;
begin
  if ColumnsAppearance.Stretch then
    StretchColumn(ColumnsAppearance.StretchAll, ColumnsAppearance.StretchColumn);
end;

procedure TAdvCustomTreeView.UpdateCalculations;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  FOldTopRow := GetFirstVisibleVirtualNodeRow;

  ColumnCount := Columns.Count;

  if NodesAppearance.HeightMode = tnhmFixed then
    DefaultRowHeight := NodesAppearance.FixedHeight
  else
    DefaultRowHeight := NodesAppearance.VariableMinimumHeight;

  if (UpdateCount = 0) and not BlockUpdateNodeList and not ((csDestroying in ComponentState) or (csReading in ComponentState) or (csLoading in ComponentState)) then
    BuildNodeList;
end;

procedure TAdvCustomTreeView.UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True);
var
  I: Integer;
  r, c: Double;
  lm, tm, rm, bm: Single;
begin
  ColumnP.Clear;
  c := 0;
  ColumnPositions[0] := c;
  for I := 0 to ColumnCount - 1 do
  begin
    c := c + ColumnWidths[I];
    ColumnPositions[I + 1] := c;
  end;

  TotalColumnWidth := c;

  if AUpdateTotalRowHeight then
  begin
    lm := 0;
    tm := 0;
    rm := 0;
    bm := 0;
    GetNodeMargins(lm, tm, rm, bm);
    r := RowCount * DefaultRowHeight;
    TotalRowHeight := r + 1 + tm + bm;
  end;
end;

procedure TAdvCustomTreeView.UpdateDisplay;
begin
  inherited;
  UpdateNodesCache(False);
  UpdateGroupsDisplay;
  UpdateColumnsDisplay;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False);
begin
  if BlockUpdateNode then
    Exit;

  UpdateScrollBars(True, False);
  UpdateVisualRange;
  UpdateNodeCache;
  UpdateScrollBars(True, False);

  {$IFDEF FMXLIB}
  FScrollBarTimer.Enabled := True;
  {$ENDIF}

  if AUpdateNodes then
  begin
    if AResetNodes then
      ResetNodes(False);

    UpdateAutoSizing;
    UpdateColumnRowCalculations(False);
    UpdateNodesCache(False);
    UpdateVisualRange;
  end
  else
  begin
    UpdateNodeDisplay;
    UpdateInplaceEditorPosition;
  end;
end;

procedure TAdvCustomTreeView.UpdateTreeViewCache;
begin
  if (UpdateCount > 0) or IsDestroying then
    Exit;

  inherited;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateColumnsCache;
begin
  UpdateColumnCache(FColumnsTopCache);
  UpdateColumnCache(FColumnsBottomCache);
  UpdateColumnsDisplay;
end;

procedure TAdvCustomTreeView.UpdateColumnCache(ACache: TAdvTreeViewCache);
var
  w, h, bmpw, bmph, bw, bh: Double;
  rt: TRectF;
  c: Integer;
  dx: Double;
  offsetx, extrax: Double;
  ac: Integer;
  rc: TRectF;
  cnt: Integer;
begin
  inherited;
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(ACache) then
    Exit;

  ACache.Clear;

  if (ACache is TAdvTreeViewColumnsTopCache) and (not (tclTop in ColumnsAppearance.Layouts) or (ColumnsAppearance.TopSize <= 0)) then
    Exit;

  if (ACache is TAdvTreeViewColumnsBottomCache) and (not (tclBottom in ColumnsAppearance.Layouts) or (ColumnsAppearance.BottomSize <= 0)) then
    Exit;

  w := GetTotalColumnWidth;
  if ACache is TAdvTreeViewColumnsTopCache then
    h := GetColumnsTopSize + 1
  else
    h := GetColumnsBottomSize + 1;

  extrax := GetColumnsExtraSize;
  offsetx := 0;
  bmpw := 0;
  bmph := 0;
  ac := 0;
  c := 0;
  dx := 0;

  cnt := ColumnCount - 1;

  while bmpw < w do
  begin
    bw := w;

    while bmph < h do
    begin
      bh := h;

      rt := RectF(0, 0, bw, bh);

      dx := rt.Left + offsetx;

      for c := ac to cnt do
      begin
        {$IFDEF FMXWEBLIB}
        rc.Top := int(rt.Top) + 0.5;
        rc.Bottom := int(rt.Bottom) - 0.5;
        rc.Left := int(dx) + 0.5;
        dx := dx + ColumnWidths[c] + extrax;
        if c = cnt then
          rc.Right := int(dx) - 0.5
        else
          rc.Right := int(dx) + 0.5;
        {$ENDIF}
        {$IFDEF CMNLIB}
        rc.Top := rt.Top + 1;
        rc.Bottom := rt.Bottom;
        if c = ac then
          rc.Left := dx + 1
        else
          rc.Left := dx;

        dx := dx + ColumnWidths[c] + extrax;
        if c = cnt then
          rc.Right := dx
        else
          rc.Right := dx + 1;
        {$ENDIF}

        OffsetRectEx(rc, bmpw, bmph);
        if ACache is TAdvTreeViewColumnsTopCache then
          ACache.Add(TAdvTreeViewCacheItem.CreateColumnTop(rc, c))
        else
          ACache.Add(TAdvTreeViewCacheItem.CreateColumnBottom(rc, c));

        if dx > rt.Right - rt.Left then
          Break;
      end;

      bmph := bmph + bh;
    end;
    bmpw := bmpw + bw;
    bmph := 0;
    ac := c;
    offsetx := -ColumnWidths[c] - (bw - dx);
  end;
end;

procedure TAdvCustomTreeView.UpdateColumnsDisplay;
begin
  BuildDisplay(FColumnsTopCache, FColumnsTopDisplay);
  BuildDisplay(FColumnsBottomCache, FColumnsBottomDisplay);
end;

procedure TAdvCustomTreeView.UpdateControlAfterResize;
begin
  inherited;
  if Assigned(Adapter) then
    Adapter.UpdateBounds;
end;

procedure TAdvCustomTreeView.VerticalScrollPositionChanged;
begin
  BlockScrollingUpdate := True;
  UpdateDisplay;
  BlockScrollingUpdate := False;
  DoVScroll(GetVScrollValue);
end;

function TAdvCustomTreeView.XYToCacheItem(X,
  Y: Double; OffsetX: Single = 0): TAdvTreeViewCacheItem;
var
  I: Integer;
  dsp: TAdvTreeViewCacheItem;
  r: TRectF;
begin
  Result := nil;
  if not Assigned(FNodeDisplay) then
    Exit;

  if not PtInRectEx(GetContentClipRect, PointF(X, Y)) then
    Exit;

  for I := 0 to FNodeDisplay.Count - 1 do
  begin
    dsp := FNodeDisplay[I];
    r := dsp.DrawRect;
    r.Left := r.Left + OffsetX;
    r.Right := r.Right + OffsetX;
    if PtInRectEx(r, PointF(X, Y)) then
    begin
      Result := dsp;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.XYToColumn(X, Y: Single; AIncludeRows: Boolean = False): Integer;
var
  I: Integer;
  topr: TRectF;
  r: TRectF;
  colw, colp: Double;
begin
  Result := -1;
  if AIncludeRows then
    topr := inherited GetContentRect
  else
    topr := GetColumnsTopRect;

  topr.Left := topr.Left - GetHorizontalScrollPosition;
  for I := ColumnCount - 1 downto 0 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) and Columns[I].Visible then
    begin
      colw := ColumnWidths[I] + GetColumnsExtraSize;
      if colw >= 0 then
      begin
        colp := ColumnPositions[I];
        r := RectF(topr.Left + colp, topr.Top, topr.Left + colp + colw, topr.Bottom);
        if PtInRectEx(r, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToColumnAnchor(AX, AY: Single; var AIndex: Integer): string;
begin
  Result := XYToColumnAnchorCache(AX, AY, FColumnsTopCache, AIndex);
  if (Result = '') and (AIndex = -1) then
    Result := XYToColumnAnchorCache(AX, AY, FColumnsBottomCache, AIndex);
end;

function TAdvCustomTreeView.XYToColumnAnchorCache(AX, AY: Single;
  ACache: TAdvTreeViewColumnsCache; var AIndex: Integer): string;
var
  I: Integer;
  txtr: TRectF;
  a: String;
  r: TRectF;
  str: String;
  g: TAdvGraphics;
  c: TAdvTreeViewCacheItem;
  col: TAdvTreeViewColumn;
  def: Boolean;
  trim: TAdvGraphicsTextTrimming;
  ha, va: TAdvGraphicsTextAlign;
  ww: Boolean;
  szt, szd, szr: Single;
begin
  Result := '';
  AIndex := -1;
  g := TAdvGraphics.CreateBitmapCanvas;
  g.BeginScene;
  try
    g.PictureContainer := PictureContainer;
    for I := 0 to ACache.Count - 1 do
    begin
      c := ACache[I];

      col := nil;
      def := False;
      if (c.Column >= 0) and (c.Column <= Columns.Count - 1) then
        col := Columns[c.Column];

      r := c.DrawRect;
      szt := 0;

      if Assigned(col) then
      begin
        if col.Filtering.Enabled then
        begin
          szd := col.Filtering.ButtonSize;
          szt := szt + szd + ScalePaintValue(4);
        end;

        if col.Expandable then
        begin
          szr := col.ExpandingButtonSize;
          szt := szt + szr + ScalePaintValue(6);
        end;

        if (TAdvTreeViewColumnOpen(col).SortKind <> nskNone) and (SortColumn = c.Column) then
        begin
          szr := ScalePaintValue(8);
          szt := szt + szr + ScalePaintValue(6);
        end;
      end;

      if def then
      begin
        case c.Kind of
          ikColumnTop: g.Font.Assign(ColumnsAppearance.TopFont);
          ikColumnBottom: g.Font.Assign(ColumnsAppearance.BottomFont);
        end;
      end
      else if Assigned(col) then
      begin
        case c.Kind of
          ikColumnTop: g.Font.Assign(col.TopFont);
          ikColumnBottom: g.Font.Assign(col.BottomFont);
        end;
      end;

      str := GetColumnText(c.Column);
      DoGetColumnText(c.Column, c.Kind, str);

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

      DoGetColumnTrimming(c.Column, c.Kind, trim);
      DoGetColumnWordWrapping(c.Column, c.Kind, ww);
      DoGetColumnHorizontalTextAlign(c.Column, c.Kind, ha);
      DoGetColumnVerticalTextAlign(c.Column, c.Kind, va);

      txtr := r;
      InflateRectEx(txtr, ScalePaintValue(-2), ScalePaintValue(-2));
      txtr.Right := Max(txtr.Left, txtr.Right - szt);

      if PtInRectEx(txtr, PointF(AX, AY)) then
      begin
        case c.Kind of
          ikColumnTop:
          begin
            if Assigned(col) then
              g.Font.Assign(col.TopFont)
            else
              g.Font.Assign(ColumnsAppearance.TopFont);

            if ColumnsAppearance.TopVerticalText then
              a := g.DrawText(txtr, str, ww, ha, va, trim, -90, -1, -1, True, True, AX, AY)
            else
              a := g.DrawText(txtr, str, ww, ha, va, trim, 0, -1, -1, True, True, AX, AY)
          end;
          ikColumnBottom:
          begin
            if Assigned(col) then
              g.Font.Assign(col.BottomFont)
            else
              g.Font.Assign(ColumnsAppearance.BottomFont);

            if ColumnsAppearance.BottomVerticalText then
              a := g.DrawText(txtr, str, ww, ha, va, trim, 90, -1, -1, True, True, AX, AY)
            else
              a := g.DrawText(txtr, str, ww, ha, va, trim, 0, -1, -1, True, True, AX, AY)
          end;
        end;

        if a <> '' then
        begin
          Result := a;
          AIndex := c.Column;
          Break;
        end;
      end;
    end;
  finally
    g.EndScene;
    g.Free;
  end;
end;

function TAdvCustomTreeView.XYToColumnExpand(X, Y: Single): Integer;
var
  I: Integer;
  topr: TRectF;
  r, dr, sr: TRectF;
  szd: Single;
  colw, colp, f: Double;
begin
  Result := -1;
  topr := GetColumnsTopRect;
  topr.Left := topr.Left - GetHorizontalScrollPosition;
  for I := ColumnCount - 1 downto 0 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) and Columns[I].Expandable and Columns[I].Visible then
    begin
      colw := ColumnWidths[I] + GetColumnsExtraSize;
      if colw >= 0 then
      begin
        colp := ColumnPositions[I];
        r := RectF(topr.Left + colp, topr.Top, topr.Left + colp + colw, topr.Bottom);
        szd := Columns[I].ExpandingButtonSize;

        if Columns[I].Filtering.Enabled then
        begin
          f := Columns[I].Filtering.ButtonSize;
          dr := RectF(Round(r.Right - f - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - f) / 2), Round(r.Right - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - f) / 2 + f));
          sr := r;
          sr.Right := dr.Left;
        end
        else
          sr := r;

        dr := RectF(Round(sr.Right - szd - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szd) / 2), Round(sr.Right - ScalePaintValue(6)), Round(sr.Top + ((sr.Bottom - sr.Top) - szd) / 2 + szd));
        if PtInRectEx(dr, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToColumnFilter(X, Y: Single): Integer;
var
  I: Integer;
  topr: TRectF;
  r, dr: TRectF;
  szd: Single;
  colw, colp: Double;
begin
  Result := -1;
  topr := GetColumnsTopRect;
  topr.Left := topr.Left - GetHorizontalScrollPosition;
  for I := ColumnCount - 1 downto 0 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) and Columns[I].Filtering.Enabled and Columns[I].Visible then
    begin
      colw := ColumnWidths[I] + GetColumnsExtraSize;
      if colw >= 0 then
      begin
        colp := ColumnPositions[I];
        r := RectF(topr.Left + colp, topr.Top, topr.Left + colp + colw, topr.Bottom);
        szd := Columns[I].Filtering.ButtonSize;
        dr := RectF(Round(r.Right - szd - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2), Round(r.Right - ScalePaintValue(4)), Round(r.Top + ((r.Bottom - r.Top) - szd) / 2 + szd));
        if PtInRectEx(dr, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToColumnSize(X,
  Y: Single): Integer;
var
  I: Integer;
  topr: TRectF;
  r: TRectF;
  colw, colp: Double;
begin
  Result := -1;
  topr := GetColumnsTopRect;
  topr.Left := topr.Left - GetHorizontalScrollPosition;
  for I := ColumnCount - 1 downto 0 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) and Columns[I].Visible then
    begin
      colw := ColumnWidths[I];
      if (colw >= 0) and (I >= 0) and (I <= Columns.Count - 1) and (Columns[I].Visible) then
      begin
        colp := ColumnPositions[I];
        r := RectF(topr.Left + colp + colw - ScalePaintValue(4), topr.Top, topr.Left + colp + colw + ScalePaintValue(4), topr.Bottom);
        if PtInRectEx(r, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeAnchor(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): TAdvTreeViewNodeAnchor;
var
  I: Integer;
  txtr: TRectF;
  a: String;
  str: String;
  g: TAdvGraphics;
  c: TAdvTreeViewColumn;
  ha, va: TAdvGraphicsTextAlign;
  ww: Boolean;
  trim: TAdvGraphicsTextTrimming;
  ext: Boolean;
  st, stp: Integer;
begin
  Result.AAnchor := '';
  Result.AColumn := -1;
  if Assigned(ANode) then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.TextRects) - 1) then
      begin
        txtr := ANode.TextRects[I - st];
        InflateRectEx(txtr, ScalePaintValue(-2), ScalePaintValue(-2));
        if PtInRectEx(txtr, PointF(X, Y)) then
        begin
          c := nil;
          if (I <= Columns.Count - 1) then
            c := Columns[I];

          ext := False;
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

          DoIsNodeExtended(ANode, ext);
          DoGetNodeTrimming(ANode, I, trim);
          DoGetNodeWordWrapping(ANode, I, ww);
          DoGetNodeHorizontalTextAlign(ANode, I, ha);
          DoGetNodeVerticalTextAlign(ANode, I,va);

          str := '';
          DoGetNodeText(ANode, I, tntmDrawing, str);

          g := TAdvGraphics.CreateBitmapCanvas;
          g.BeginScene;

          if Assigned(c) and not c.UseDefaultAppearance and not ext then
            g.Font.Assign(c.Font)
          else
          begin
            if ext then
              g.Font.Assign(NodesAppearance.ExtendedFont)
            else
              g.Font.Assign(NodesAppearance.Font);
          end;

          g.PictureContainer := PictureContainer;
          g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
          try
            a := g.DrawText(txtr, str, ww, ha, va, trim, 0, -1, -1, True, True, X, Y);
          finally
            g.EndScene;
            g.Free;
          end;

          if a <> '' then
          begin
            Result.AAnchor := a;
            Result.AColumn := I;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeCheck(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): TAdvTreeViewNodeCheck;
var
  nrt: TRectF;
  en: Boolean;
  I: Integer;
  st, stp: Integer;
begin
  Result.AColumn := -1;
  Result.ANode := nil;
  en := True;
  DoIsNodeEnabled(ANode, en);
  if Assigned(ANode) and en then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.CheckRects) - 1) then
      begin
        nrt := ANode.CheckRects[I - st];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result.ANode := ANode;
          Result.AColumn := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeExpand(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): Boolean;
var
  nrt: TRectF;
  en: Boolean;
  c: Integer;
begin
  Result := False;
  if Assigned(ANode) and (ANode.Children > 0) then
  begin
    en := True;
    DoIsNodeEnabled(ANode, en);
    if en then
    begin
      c := GetFirstVisibleColumn;
      if (NodesAppearance.ExpandColumn - c >= 0) and (NodesAppearance.ExpandColumn - c <= Length(ANode.ExpandRects) - 1) then
      begin
        nrt := ANode.ExpandRects[NodesAppearance.ExpandColumn - c];
        Result := PtInRectEx(nrt, PointF(X, Y));
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeExtra(ANode: TAdvTreeViewVirtualNode;
  X, Y: Single): Boolean;
var
  nrt: TRectF;
  en: Boolean;
  I: Integer;
  st: Integer;
  stp: Integer;
begin
  Result := False;
  en := True;
  DoIsNodeEnabled(ANode, en);
  if Assigned(ANode) and en then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.ExtraRects) - 1) then
      begin
        nrt := ANode.ExtraRects[I - st];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeTextColumn(ANode: TAdvTreeViewVirtualNode;
  X, Y: Single): Integer;
var
  nrt: TRectF;
  I: Integer;
  st, stp: Integer;
begin
  Result := -1;
  if Assigned(ANode) then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.TextRects) - 1) then
      begin
        nrt := ANode.TextRects[I - st];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeTitleAnchor(
  ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeAnchor;
var
  I: Integer;
  titr: TRectF;
  a: String;
  str: String;
  g: TAdvGraphics;
  c: TAdvTreeViewColumn;
  ha, va: TAdvGraphicsTextAlign;
  ww: Boolean;
  trim: TAdvGraphicsTextTrimming;
  st, stp: Integer;
begin
  Result.AAnchor := '';
  Result.AColumn := -1;
  if Assigned(ANode) then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.TitleRects) - 1) then
      begin
        titr := ANode.TitleRects[I - st];
        InflateRectEx(titr, ScalePaintValue(-2), ScalePaintValue(-2));
        if PtInRectEx(titr, PointF(X, Y)) then
        begin
          c := nil;
          if (I <= Columns.Count - 1) then
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

          g := TAdvGraphics.CreateBitmapCanvas;
          g.BeginScene;

          if Assigned(c) and not c.UseDefaultAppearance then
            g.Font.Assign(c.TitleFont)
          else
            g.Font.Assign(NodesAppearance.TitleFont);

          g.PictureContainer := PictureContainer;
          g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
          try
            a := g.DrawText(titr, str, ww, ha, va, trim, 0, -1, -1, True, True, X, Y);
          finally
            g.EndScene;
            g.Free;
          end;

          if a <> '' then
          begin
            Result.AAnchor := a;
            Result.AColumn := I;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeTitleExtra(
  ANode: TAdvTreeViewVirtualNode; X, Y: Single): Boolean;
var
  nrt: TRectF;
  en: Boolean;
  I: Integer;
  st, stp: Integer;
begin
  Result := False;
  en := True;
  DoIsNodeEnabled(ANode, en);
  if Assigned(ANode) and en then
  begin
    st := GetFirstVisibleColumn;
    stp := GetLastVisibleColumn;
    for I := st to stp do
    begin
      if (I - st >= 0) and (I - st <= Length(ANode.TitleExtraRects) - 1) then
      begin
        nrt := ANode.TitleExtraRects[I - st];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNode(X, Y: Double; OffsetX: Single = 0): TAdvTreeViewVirtualNode;
var
  dsp: TAdvTreeViewCacheItem;
begin
  Result := nil;
  dsp := XYToCacheItem(X, Y, OffsetX);
  if Assigned(dsp) then
    Result := dsp.Node;
end;

procedure TAdvCustomTreeView.HandleNodeEditing(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  c: TAdvTreeViewColumn;
  ins: Boolean;
  {$IFNDEF LCLWEBLIB}
  AContext: TRttiContext;
  rt: TRttiType;
  propt: TRttiProperty;
  {$ENDIF}
  n: String;
  r: TRectF;
  {$IFDEF FMXLIB}
  trans: Boolean;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  rtr: TRect;
  {$ENDIF}

  function CreateCaretPos(ALine, APos: Integer): TAdvTreeViewCaretPosition;
  begin
    {$IFDEF FMXLIB}
    Result.Line := ALine;
    Result.Pos := APos;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result.X := ALine;
    Result.Y := APos;
    {$ENDIF}
  end;
begin
  if Assigned(ANode) and (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
  begin
    c := Columns[AColumn];
    FInplaceEditorClass := nil;
    if not c.CustomEditor then
    begin
      case c.EditorType of
        tcetEdit: FInplaceEditorClass := TAdvTreeViewEdit;
        tcetComboBox: FInplaceEditorClass := TAdvTreeViewComboBox;
        tcetMemo: FInplaceEditorClass := TAdvTreeViewMemo;
        tcetNone: Exit;
      end;
    end;

    ins := True;
    DoBeforeOpenInplaceEditor(ANode, AColumn, ins);
    if ins then
    begin
      {$IFDEF FMXLIB}
      trans := True;
      DoGetInplaceEditor(ANode, AColumn, trans, FInplaceEditorClass);
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      DoGetInplaceEditor(ANode, AColumn, FInplaceEditorClass);
      {$ENDIF}

      if Assigned(FInplaceEditor) then
      begin
        {$IFDEF FMXLIB}
        FInplaceEditor.DisposeOf;
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        if Assigned(FOldInplaceEditor) then
          FOldInplaceEditor.Free;

        FOldInplaceEditor := FInplaceEditor;
        {$ENDIF}
        FInplaceEditor := nil;
      end;

      if Assigned(FInplaceEditorClass) then
        FInplaceEditor := FInplaceEditorClass.Create(Self)
      else
        FInplaceEditor := TEdit.Create(Self);

      r := GetInplaceEditorRect(ANode, AColumn);
      if Assigned(FInplaceEditor) then
      begin
        FUpdateNode := ANode;
        FUpdateNodeColumn := AColumn;
        {$IFDEF FMXLIB}
        if trans then
        begin
          FInplaceEditor.DisableFocusEffect := True;
          FInplaceEditor.OnApplyStyleLookup := ApplyInplaceEditorStyleLookup;
        end;
        {$ENDIF}

        {$IFDEF WEBLIB}
        FInplaceEditor.ShowFocus := False;
        {$ENDIF}

        FInplaceEditor.Parent := Self;
        FInplaceEditor.Visible := False;
        {$IFDEF FMXLIB}
        FInplaceEditor.BoundsRect := r;
        FInplaceEditor.BoundsRect := RectF(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2 + FInplaceEditor.Height);
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        rtr := Rect(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
        FInplaceEditor.BoundsRect := rtr;
        FInplaceEditor.BoundsRect := Rect(rtr.Left, rtr.Top + (rtr.Bottom - rtr.Top - FInplaceEditor.Height) div 2, rtr.Right, rtr.Top + (rtr.Bottom - rtr.Top - FInplaceEditor.Height) div 2 + FInplaceEditor.Height);
        {$ENDIF}
        FInplaceEditor.Visible := True;

        if (c.EditorType = tcetComboBox) and not c.CustomEditor and (FInplaceEditor is TComboBox) then
        begin
          (FInplaceEditor as TComboBox).Items.Assign(c.EditorItems);
      	  {$IFDEF CMNWEBLIB}
          (FInplaceEditor as TComboBox).Style := csDropDownList;
      	  {$ENDIF}
      	end;

        CustomizeInplaceEditor(FInplaceEditor, ANode, c);

        {$IFNDEF LCLWEBLIB}
        AContext := TRttiContext.Create;
        {$ENDIF}
        try
          {$IFNDEF LCLWEBLIB}
          rt := AContext.GetType(FInplaceEditor.ClassInfo);
          {$ENDIF}
          n := '';
          DoGetNodeText(ANode, AColumn, tntmEditing, n);
          if (FInplaceEditor is TComboBox) then
            (FInplaceEditor as TComboBox).ItemIndex := (FInplaceEditor as TComboBox).Items.IndexOf(n)
          {$IFDEF LCLWEBLIB}
          ;
          {$ENDIF}
          {$IFNDEF LCLWEBLIB}
          else
          begin
            propt := rt.GetProperty('Text');
            if Assigned(propt) then
              propt.SetValue(FInplaceEditor, n)
          end;
          {$ENDIF}

          {$IFDEF LCLWEBLIB}
          if FInplaceEditor is TEdit then
            (FInplaceEditor as TEdit).Text := n;

          if FInplaceEditor is TMemo then
            (FInplaceEditor as TMemo).Text := n;
          {$ENDIF}
        finally
          {$IFNDEF LCLWEBLIB}
          AContext.Free;
          {$ENDIF}
        end;

        if FInplaceEditor.CanFocus then
          FInplaceEditor.SetFocus;

        if FInplaceEditor is TEdit then
          (FInplaceEditor as TEdit).SelStart := Length((FInplaceEditor as TEdit).Text);

        if FInplaceEditor is TMemo then
        begin
          (FInplaceEditor as TMemo).SelLength := 0;
          if (FInplaceEditor as TMemo).Lines.Count > 0 then
            (FInplaceEditor as TMemo).CaretPos := CreateCaretPos((FInplaceEditor as TMemo).Lines.Count - 1, Length((FInplaceEditor as TMemo).Lines[(FInplaceEditor as TMemo).Lines.Count - 1]))
          else
            (FInplaceEditor as TMemo).CaretPos := CreateCaretPos(0, 0);
        end;

        FInplaceEditorActive := True;
        Invalidate;
      end;

      DoAfterOpenInplaceEditor(ANode, AColumn, FInplaceEditor, r);
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleNodeExpand(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean);
var
  e: Boolean;
begin
  if Assigned(ANode) then
  begin
    if not ANode.Expanded and (ANode.Children > 0) then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeExpandNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode, ARecurse);
        DoAfterExpandNode(ANode);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleNodeExtra(
  ANode: TAdvTreeViewVirtualNode);
begin

end;

procedure TAdvCustomTreeView.HandleNodeCollapse(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean);
var
  e: Boolean;
begin
  if Assigned(ANode) then
  begin
    if ANode.Expanded and (ANode.Children > 0) then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeCollapseNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode, ARecurse);
        DoAfterCollapseNode(ANode);
      end;
    end
  end;
end;

procedure TAdvCustomTreeView.HandleNodeTitleExtra(
  ANode: TAdvTreeViewVirtualNode);
begin

end;

procedure TAdvCustomTreeView.HandleNodeToggle(ANode: TAdvTreeViewVirtualNode);
var
  e: Boolean;
begin
  if Assigned(ANode) and (ANode.Children > 0) then
  begin
    if ANode.Expanded then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeCollapseNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterCollapseNode(ANode);
      end;
    end
    else
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeExpandNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterExpandNode(ANode);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleNodeToggleCheck(ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  chk, e: Boolean;
  chkt: TAdvTreeViewNodeCheckType;
begin
  if Assigned(ANode) then
  begin
    if (AColumn >= 0) and (AColumn <= Length(ANode.CheckStates) - 1) then
    begin
      chk := ANode.CheckStates[AColumn];
      if chk then
      begin
        chkt := tvntNone;
        DoGetNodeCheckType(ANode, AColumn, chkt);
        if chkt = tvntCheckBox then
        begin
          e := True;
          DoBeforeUnCheckNode(ANode, AColumn, e);
          if e then
          begin
            ToggleCheckNodeInternal(ANode, AColumn);
            DoAfterUnCheckNode(ANode, AColumn);
          end;
        end;
      end
      else
      begin
        e := True;
        DoBeforeCheckNode(ANode, AColumn, e);
        if e then
        begin
          ToggleCheckNodeInternal(ANode, AColumn);
          DoAfterCheckNode(ANode, AColumn);
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleSelectNode(ANode: TAdvTreeViewVirtualNode;
  ATriggerEvents: Boolean; AKeyBoard: Boolean; AMultiSelect: Boolean);
var
  I: Integer;
  it: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  if not AMultiSelect then
  begin
    for I := FSelectedNodes.Count - 1 downto 0 do
    begin
      it := FSelectedNodes[I];
      if it <> ANode then
      begin
        en := True;
        if ATriggerEvents then
          DoBeforeUnSelectNode(it, en);

        if en then
        begin
          if FSelectedNodes.IndexOf(it) > -1 then
            FSelectedNodes.Remove(it);
          if ATriggerEvents then
            DoAfterUnSelectNode(it);
        end;
      end;
    end;
  end;

  if Assigned(ANode) then
  begin
    if (FSelectedNodes.IndexOf(ANode) > -1) or ((Assigned(FDownNode) and (FDownNode.ParentNode <> ANode.ParentNode)) and (msSiblingOnly in Interaction.MultiSelectStyle) and AMultiSelect)  then
    begin
      en := True;
      if ATriggerEvents then
        DoBeforeUnSelectNode(ANode, en);
      if en then
      begin
        if FSelectedNodes.IndexOf(ANode) > -1 then
          FSelectedNodes.Remove(ANode);
        if ATriggerEvents then
          DoAfterUnSelectNode(ANode);
      end;
    end
    else
    begin
      if (Assigned(FDownNode) and ((FDownNode.ParentNode = ANode.ParentNode) or not AMultiSelect)) or not (msSiblingOnly in Interaction.MultiSelectStyle) then
      begin
        if (VisibleNodes.IndexOf(ANode) > -1) or not (msVisibleOnly in Interaction.MultiSelectStyle) then
        begin
          en := True;
          if ATriggerEvents then
            DoBeforeSelectNode(ANode, en);

          if en then
          begin
            FSelectedNodes.Add(ANode);
            if ATriggerEvents then
              DoAfterSelectNode(ANode);
          end;
        end;
      end;
    end;

    if not AKeyBoard and ATriggerEvents then
      DoNodeClick(ANode);
  end;
end;

{ TAdvTreeViewNodesAppearance }

procedure TAdvTreeViewNodesAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewNodesAppearance then
  begin
    FFill.Assign((Source as TAdvTreeViewNodesAppearance).Fill);
    FStroke.Assign((Source as TAdvTreeViewNodesAppearance).Stroke);
    FFont.Assign((Source as TAdvTreeViewNodesAppearance).Font);
    FTitleFont.Assign((Source as TAdvTreeViewNodesAppearance).TitleFont);
    FSelectedFill.Assign((Source as TAdvTreeViewNodesAppearance).SelectedFill);
    FSelectedStroke.Assign((Source as TAdvTreeViewNodesAppearance).SelectedStroke);
    FDisabledFill.Assign((Source as TAdvTreeViewNodesAppearance).DisabledFill);
    FDisabledStroke.Assign((Source as TAdvTreeViewNodesAppearance).DisabledStroke);
    FExtendedFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedFill);
    FExtendedStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedStroke);
    FExtendedFont.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedFont);
    FExtendedSelectedFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedSelectedFill);
    FExtendedSelectedStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedSelectedStroke);
    FExtendedDisabledFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedDisabledFill);
    FExtendedDisabledStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedDisabledStroke);
    FExpandColumn := (Source as TAdvTreeViewNodesAppearance).ExpandColumn;
    FExpandWidth := (Source as TAdvTreeViewNodesAppearance).ExpandWidth;
    FExpandHeight := (Source as TAdvTreeViewNodesAppearance).ExpandHeight;
    FFixedHeight := (Source as TAdvTreeViewNodesAppearance).FixedHeight;
    FVariableMinimumHeight := (Source as TAdvTreeViewNodesAppearance).VariableMinimumHeight;
    FSelectedFontColor := (Source as TAdvTreeViewNodesAppearance).SelectedFontColor;
    FDisabledFontColor := (Source as TAdvTreeViewNodesAppearance).DisabledFontColor;
    FSelectedTitleFontColor := (Source as TAdvTreeViewNodesAppearance).SelectedTitleFontColor;
    FDisabledTitleFontColor := (Source as TAdvTreeViewNodesAppearance).DisabledTitleFontColor;
    FExtendedFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedFontColor;
    FExtendedSelectedFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedSelectedFontColor;
    FExtendedDisabledFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedDisabledFontColor;
    FLevelIndent := (Source as TAdvTreeViewNodesAppearance).LevelIndent;
    FSpacing := (Source as TAdvTreeViewNodesAppearance).Spacing;
    FHeightMode := (Source as TAdvTreeViewNodesAppearance).HeightMode;
    FShowLines := (Source as TAdvTreeViewNodesAppearance).ShowLines;
    FShowFocus := (Source as TAdvTreeViewNodesAppearance).ShowFocus;
    FColumnStroke.Assign((Source as TAdvTreeViewNodesAppearance).ColumnStroke);
    FLineStroke.Assign((Source as TAdvTreeViewNodesAppearance).LineStroke);
  end;
end;

procedure TAdvTreeViewNodesAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewNodesAppearance.BitmapChanged(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

constructor TAdvTreeViewNodesAppearance.Create(ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FShowFocus := True;
  FSelectionArea := tsaFromText;
  FExpandColumn := 0;
  FExpandWidth := 15;
  FExpandHeight := 15;
  FFixedHeight := 25;
  FVariableMinimumHeight := 25;
  FShowLines := True;
  FHeightMode := tnhmFixed;
  FLevelIndent := 20;
  FSpacing := 0;
  FSelectedFontColor := gcWhite;
  FDisabledFontColor := gcSilver;

  FSelectedTitleFontColor := gcWhite;
  FDisabledTitleFontColor := gcSilver;

  FExtendedFontColor := gcBlack;
  FExtendedSelectedFontColor := gcWhite;
  FExtendedDisabledFontColor := gcSilver;

  FColumnStroke := TAdvGraphicsStroke.Create(gskSolid, gcNull);
  FLineStroke := TAdvGraphicsStroke.Create(gskDot, gcDarkGray);
  FFill :=  TAdvGraphicsFill.Create(gfkSolid, gcNull);
  FStroke := TAdvGraphicsStroke.Create(gskNone, gcDarkGray);
  FSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvTreeViewColorSelection);
  FSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvTreeViewColorSelection);
  FDisabledFill := TAdvGraphicsFill.Create(gfkSolid, gcDarkGray);
  FDisabledStroke := TAdvGraphicsStroke.Create(gskNone, gcDarkGray);

  FExtendedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvTreeViewColorExtended);
  FExtendedStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FExtendedSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvTreeViewColorSelection);
  FExtendedSelectedStroke := TAdvGraphicsStroke.Create(gskNone, gcDarkGray);
  FExtendedDisabledFill := TAdvGraphicsFill.Create(gfkSolid, gcDarkGray);
  FExtendedDisabledStroke := TAdvGraphicsStroke.Create(gskNone, gcDarkGray);

  FExpandNodeIcon := TAdvBitmap(TAdvBitmap.CreateFromResource('TAdvTREEVIEWEXPAND', HInstance));
  FExpandNodeIcon.OnChange := BitmapChanged;
  FCollapseNodeIcon := TAdvBitmap(TAdvBitmap.CreateFromResource('TAdvTREEVIEWCOLLAPSE', HInstance));
  FExpandNodeIcon.OnChange := BitmapChanged;
  FExpandNodeIconLarge := TAdvBitmap(TAdvBitmap.CreateFromResource('TAdvTREEVIEWEXPANDLARGE', HInstance));
  FExpandNodeIconLarge.OnChange := BitmapChanged;
  FCollapseNodeIconLarge := TAdvBitmap(TAdvBitmap.CreateFromResource('TAdvTREEVIEWCOLLAPSELARGE', HInstance));
  FCollapseNodeIconLarge.OnChange := BitmapChanged;

  FFont := TAdvGraphicsFont.Create;
  if Assigned(FTreeView) and FTreeView.IsDesignTime then
    FFont.Color := gcBlack;

  FTitleFont := TAdvGraphicsFont.Create;
  if Assigned(FTreeView) and FTreeView.IsDesignTime then
    FTitleFont.Color := gcBlack;

  FFill.OnChanged := Changed;
  FStroke.OnChanged := Changed;

  FSelectedFill.OnChanged := Changed;
  FSelectedStroke.OnChanged := Changed;

  FDisabledFill.OnChanged := Changed;
  FDisabledStroke.OnChanged := Changed;

  FExtendedFont := TAdvGraphicsFont.Create;

  FExtendedFill.OnChanged := Changed;
  FExtendedStroke.OnChanged := Changed;

  FExtendedSelectedFill.OnChanged := Changed;
  FExtendedSelectedStroke.OnChanged := Changed;

  FExtendedDisabledFill.OnChanged := Changed;
  FExtendedDisabledStroke.OnChanged := Changed;

  FFont.OnChanged := Changed;
  FExtendedFont.OnChanged := Changed;
end;

destructor TAdvTreeViewNodesAppearance.Destroy;
begin
  FLineStroke.Free;
  FColumnStroke.Free;

  FExpandNodeIconLarge.Free;
  FCollapseNodeIconLarge.Free;
  FExpandNodeIcon.Free;
  FCollapseNodeIcon.Free;

  FFill.Free;
  FStroke.Free;
  FFont.Free;
  FTitleFont.Free;

  FSelectedFill.Free;
  FSelectedStroke.Free;

  FDisabledFill.Free;
  FDisabledStroke.Free;

  FExtendedFill.Free;
  FExtendedStroke.Free;
  FExtendedFont.Free;

  FExtendedSelectedFill.Free;
  FExtendedSelectedStroke.Free;

  FExtendedDisabledFill.Free;
  FExtendedDisabledStroke.Free;
  inherited;
end;

function TAdvTreeViewNodesAppearance.IsSpacingStored: Boolean;
begin
  Result := Spacing <> 0;
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedFill(const Value: TAdvGraphicsFill);
begin
  if FSelectedFill <> Value then
    FSelectedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  if FSelectedStroke <> Value then
    FSelectedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedTitleFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FSelectedTitleFontColor <> Value then
  begin
    FSelectedTitleFontColor := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetSelectionArea(const Value: TAdvTreeViewSelectionArea);
begin
  if FSelectionArea <> Value then
  begin
    FSelectionArea := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetShowLines(const Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetSpacing(const Value: Double);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  if FFill <> Value then
    FFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFontColor(const Value: TAdvGraphicsColor);
begin
  if FExtendedFontColor <> Value then
  begin
    FExtendedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedFontColor(const Value: TAdvGraphicsColor);
begin
  if FExtendedSelectedFontColor <> Value then
  begin
    FExtendedSelectedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledFontColor(const Value: TAdvGraphicsColor);
begin
  if FExtendedDisabledFontColor <> Value then
  begin
    FExtendedDisabledFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedFontColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledFontColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFixedHeight(const Value: Double);
begin
  if FFixedHeight <> Value then
  begin
    FFixedHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetVariableMinimumHeight(const Value: Double);
begin
  if FVariableMinimumHeight <> Value then
  begin
    FVariableMinimumHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  if FFont <> Value then
    FFont.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledFill(
  const Value: TAdvGraphicsFill);
begin
  if FExtendedDisabledFill <> Value then
    FExtendedDisabledFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FExtendedDisabledStroke <> Value then
    FExtendedDisabledStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedFill(
  const Value: TAdvGraphicsFill);
begin
  if FExtendedSelectedFill <> Value then
    FExtendedSelectedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FExtendedSelectedStroke <> Value then
    FExtendedSelectedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFill(
  const Value: TAdvGraphicsFill);
begin
  if FExtendedFill <> Value then
    FExtendedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFont(
  const Value: TAdvGraphicsFont);
begin
  if FExtendedFont <> Value then
    FExtendedFont.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FExtendedStroke <> Value then
    FExtendedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetHeightMode(
  const Value: TAdvTreeViewNodeHeightMode);
begin
  if FHeightMode <> Value then
  begin
    FHeightMode := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetLevelIndent(const Value: Double);
begin
  if FLevelIndent <> Value then
  begin
    FLevelIndent := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandNodeIcon(const Value: TAdvBitmap);
begin
  if FExpandNodeIcon <> Value then
  begin
    FExpandNodeIcon.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandNodeIconLarge(const Value: TAdvBitmap);
begin
  if FExpandNodeIconLarge <> Value then
  begin
    FExpandNodeIconLarge.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetCollapseNodeIconLarge(const Value: TAdvBitmap);
begin
  if FCollapseNodeIconLarge <> Value then
  begin
    FCollapseNodeIconLarge.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetColumnStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FColumnStroke <> Value then
    FColumnStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetLineStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FLineStroke <> Value then
    FLineStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExpandColumn(const Value: Integer);
begin
  if FExpandColumn <> Value then
  begin
    FExpandColumn := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandWidth(const Value: Double);
begin
  if FExpandWidth <> Value then
  begin
    FExpandWidth := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandHeight(const Value: Double);
begin
  if FExpandHeight <> Value then
  begin
    FExpandHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  if FStroke <> Value then
    FStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetTitleFont(
  const Value: TAdvGraphicsFont);
begin
  if FTitleFont <> Value then
    FTitleFont.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetCollapseNodeIcon(
  const Value: TAdvBitmap);
begin
  if FCollapseNodeIcon <> Value then
  begin
    FCollapseNodeIcon.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledFill(const Value: TAdvGraphicsFill);
begin
  if FDisabledFill <> Value then
    FDisabledFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledStroke(const Value: TAdvGraphicsStroke);
begin
  if FDisabledStroke <> Value then
    FDisabledStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledTitleFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FDisabledTitleFontColor <> Value then
  begin
    FDisabledTitleFontColor := Value;
    FTreeView.Invalidate;
  end;
end;

{ TAdvTreeViewColumnsAppearance }

procedure TAdvTreeViewColumnsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewColumnsAppearance then
  begin
    FSortIndicatorColor := (Source as TAdvTreeViewColumnsAppearance).SortIndicatorColor;
    FLayouts := (Source as TAdvTreeViewColumnsAppearance).Layouts;
    FTopSize := (Source as TAdvTreeViewColumnsAppearance).TopSize;
    FBottomSize := (Source as TAdvTreeViewColumnsAppearance).BottomSize;
    FBottomFill.Assign((Source as TAdvTreeViewColumnsAppearance).BottomFill);
    FBottomFont.Assign((Source as TAdvTreeViewColumnsAppearance).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewColumnsAppearance).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewColumnsAppearance).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewColumnsAppearance).TopStroke);
    FTopFont.Assign((Source as TAdvTreeViewColumnsAppearance).TopFont);
    FStretch := (Source as TAdvTreeViewColumnsAppearance).Stretch;
    FOptimizedColumnDisplay := (Source as TAdvTreeViewColumnsAppearance).OptimizedColumnDisplay;
    FStretchColumn  := (Source as TAdvTreeViewColumnsAppearance).StretchColumn;
    FStretchAll := (Source as TAdvTreeViewColumnsAppearance).StretchAll;
    FTopVerticalText := (Source as TAdvTreeViewColumnsAppearance).TopVerticalText;
    FBottomVerticalText := (Source as TAdvTreeViewColumnsAppearance).BottomVerticalText;
    FFillEmptySpaces := (Source as TAdvTreeViewColumnsAppearance).FillEmptySpaces;
  end;
end;

constructor TAdvTreeViewColumnsAppearance.Create(
  ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FSortIndicatorColor := gcSteelblue;
  FLayouts := [tclTop];
  FFillEmptySpaces := True;
  FOptimizedColumnDisplay := True;
  FStretch := True;
  FStretchAll := True;
  FStretchColumn := -1;
  FBottomSize := 25;
  FTopVerticalText := False;
  FStretchAll := True;
  FBottomVerticalText := False;
  FTopSize := 25;
  FTopHorizontalTextAlign := gtaCenter;
  FTopVerticalTextAlign := gtaCenter;
  FBottomHorizontalTextAlign := gtaCenter;
  FBottomVerticalTextAlign := gtaCenter;
  FBottomFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FBottomStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FTopFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FTopStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FTopFont := TAdvGraphicsFont.Create;
  FTopFont.Color := gcBlack;
  FBottomFont := TAdvGraphicsFont.Create;
  FBottomFont.Color := gcBlack;

  FBottomFill.OnChanged := Changed;
  FTopFill.OnChanged := Changed;
  FBottomStroke.OnChanged := Changed;
  FTopStroke.OnChanged := Changed;

  FTopFont.OnChanged := Changed;
  FBottomFont.OnChanged := Changed;
end;

destructor TAdvTreeViewColumnsAppearance.Destroy;
begin
  FTopFont.Free;
  FBottomFont.Free;
  FBottomFill.Free;
  FTopFill.Free;
  FBottomStroke.Free;
  FTopStroke.Free;
  inherited;
end;

procedure TAdvTreeViewColumnsAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomFill(const Value: TAdvGraphicsFill);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomFont(const Value: TAdvGraphicsFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomSize(const Value: Double);
begin
  if FBottomSize <> Value then
  begin
    FBottomSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomVerticalText(
  const Value: Boolean);
begin
  if FBottomVerticalText <> Value then
  begin
    FBottomVerticalText := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetFillEmptySpaces(
  const Value: Boolean);
begin
  if FFillEmptySpaces <> Value then
  begin
    FFillEmptySpaces := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetLayouts(
  const Value: TAdvTreeViewColumnsLayouts);
begin
  if FLayouts <> Value then
  begin
    FLayouts := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetOptimizedColumnDisplay(
  const Value: Boolean);
begin
  if FOptimizedColumnDisplay <> Value then
  begin
    FOptimizedColumnDisplay := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetSortIndicatorColor(
  const Value: TAdvGraphicsColor);
begin
  if FSortIndicatorColor <> Value then
  begin
    FSortIndicatorColor := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretchColumn(const Value: Integer);
begin
  if FStretchColumn <> Value then
  begin
    FStretchColumn := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretchAll(const Value: Boolean);
begin
  if FStretchAll <> Value then
  begin
    FStretchAll := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetTopFill(const Value: TAdvGraphicsFill);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopFont(const Value: TAdvGraphicsFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopSize(const Value: Double);
begin
  if FTopSize <> Value then
  begin
    FTopSize := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetTopStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopVerticalText(
  const Value: Boolean);
begin
  if FTopVerticalText <> Value then
  begin
    FTopVerticalText := Value;
    Changed(Self);
  end;
end;

{ TAdvTreeViewGroupsAppearance }

procedure TAdvTreeViewGroupsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewGroupsAppearance then
  begin
    FLayouts := (Source as TAdvTreeViewGroupsAppearance).Layouts;
    FTopSize := (Source as TAdvTreeViewGroupsAppearance).TopSize;
    FBottomSize := (Source as TAdvTreeViewGroupsAppearance).BottomSize;
    FBottomFill.Assign((Source as TAdvTreeViewGroupsAppearance).BottomFill);
    FBottomFont.Assign((Source as TAdvTreeViewGroupsAppearance).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewGroupsAppearance).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewGroupsAppearance).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewGroupsAppearance).TopStroke);
    FTopFont.Assign((Source as TAdvTreeViewGroupsAppearance).TopFont);
    FTopHorizontalTextAlign := (Source as TAdvTreeViewGroupsAppearance).TopHorizontalTextAlign;
    FTopVerticalTextAlign := (Source as TAdvTreeViewGroupsAppearance).TopVerticalTextAlign;
    FBottomHorizontalTextAlign := (Source as TAdvTreeViewGroupsAppearance).BottomHorizontalTextAlign;
    FBottomVerticalTextAlign := (Source as TAdvTreeViewGroupsAppearance).BottomVerticalTextAlign;
    FTopVerticalText := (Source as TAdvTreeViewGroupsAppearance).TopVerticalText;
    FBottomVerticalText := (Source as TAdvTreeViewGroupsAppearance).BottomVerticalText;
    FFillEmptySpaces := (Source as TAdvTreeViewGroupsAppearance).FillEmptySpaces;
  end;
end;

constructor TAdvTreeViewGroupsAppearance.Create(
  ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FLayouts := [tglTop];
  FFillEmptySpaces := True;
  FTopSize := 50;
  FBottomSize := 50;
  FTopVerticalText := False;
  FBottomVerticalText := False;
  FTopHorizontalTextAlign := gtaCenter;
  FTopVerticalTextAlign := gtaCenter;
  FBottomHorizontalTextAlign := gtaCenter;
  FBottomVerticalTextAlign := gtaCenter;
  FBottomFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FBottomStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FTopFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FTopStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FTopFont := TAdvGraphicsFont.Create;
  FTopFont.Color := gcBlack;
  FBottomFont := TAdvGraphicsFont.Create;
  FBottomFont.Color := gcBlack;

  FBottomFill.OnChanged := Changed;
  FTopFill.OnChanged := Changed;
  FBottomStroke.OnChanged := Changed;
  FTopStroke.OnChanged := Changed;

  FTopFont.OnChanged := Changed;
  FBottomFont.OnChanged := Changed;
end;

destructor TAdvTreeViewGroupsAppearance.Destroy;
begin
  FBottomFont.Free;
  FTopFont.Free;
  FBottomFill.Free;
  FBottomStroke.Free;
  FTopFill.Free;
  FTopStroke.Free;
  inherited;
end;

procedure TAdvTreeViewGroupsAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomFill(const Value: TAdvGraphicsFill);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FBottomHorizontalTextAlign <> Value then
  begin
    FBottomHorizontalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomFont(const Value: TAdvGraphicsFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomSize(const Value: Double);
begin
  if FBottomSize <> Value then
  begin
    FBottomSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FBottomVerticalTextAlign <> Value then
  begin
    FBottomVerticalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomVerticalText(
  const Value: Boolean);
begin
  if FBottomVerticalText <> Value then
  begin
    FBottomVerticalText := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetFillEmptySpaces(
  const Value: Boolean);
begin
  if FFillEmptySpaces <> Value then
  begin
    FFillEmptySpaces := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetLayouts(
  const Value: TAdvTreeViewGroupsLayouts);
begin
  if FLayouts <> Value then
  begin
    FLayouts := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopFill(const Value: TAdvGraphicsFill);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopFont(const Value: TAdvGraphicsFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FTopHorizontalTextAlign <> Value then
  begin
    FTopHorizontalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopSize(const Value: Double);
begin
  if FTopSize <> Value then
  begin
    FTopSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FTopVerticalTextAlign <> Value then
  begin
    FTopVerticalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopVerticalText(const Value: Boolean);
begin
  if FTopVerticalText <> Value then
  begin
    FTopVerticalText := Value;
    Changed(Self);
  end;
end;

{ TAdvTreeViewInteraction }

procedure TAdvTreeViewInteraction.Assign(Source: TPersistent);
begin
  if (Source is TAdvTreeViewInteraction) then
  begin
    FMultiSelect := (Source as TAdvTreeViewInteraction).MultiSelect;
    FTouchScrolling := (Source as TAdvTreeViewInteraction).TouchScrolling;
    FReadOnly := (Source as TAdvTreeViewInteraction).ReadOnly;
    FColumnSizing := (Source as TAdvTreeViewInteraction).ColumnSizing;
    FColumnAutoSizeOnDblClick := (Source as TAdvTreeViewInteraction).ColumnAutoSizeOnDblClick;
    FExtendedSelectable := (Source as TAdvTreeViewInteraction).ExtendedSelectable;
    FSelectionFollowsFocus := (Source as TAdvTreeViewInteraction).SelectionFollowsFocus;
    FKeyboardEdit := (Source as TAdvTreeViewInteraction).KeyboardEdit;
    FMouseEditMode := (Source as TAdvTreeViewInteraction).MouseEditMode;
    FExtendedEditable := (Source as TAdvTreeViewInteraction).ExtendedEditable;
    FClipboardMode := (Source as TAdvTreeViewInteraction).ClipboardMode;
    FReorder := (Source as TAdvTreeViewInteraction).Reorder;
    FDragDropMode := (Source as TAdvTreeViewInteraction).DragDropMode;
    FLookup.Assign((Source as TAdvTreeViewInteraction).Lookup);
    FExpandCollapseOnDblClick := (Source as TAdvTreeViewInteraction).ExpandCollapseOnDblClick;
    FAutoOpenURL := (Source as TAdvTreeViewInteraction).AutoOpenURL;
    FURLDetectionOnMouseMove := (Source as TAdvTreeViewInteraction).URLDetectionOnMouseMove;
    FMouseWheelDelta := (Source as TAdvTreeViewInteraction).MouseWheelDelta;
    FAnimationFactor := (Source as TAdvTreeViewInteraction).AnimationFactor;
  end;
end;

constructor TAdvTreeViewInteraction.Create(ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FAutoOpenURL := True;
  FAnimationFactor := 2.5;
  FMouseWheelDelta := 1;
  FClipboardMode := tcmNone;
  FExpandCollapseOnDblClick := True;
  FReorder := False;
  FDragDropMode := tdmNone;
  FReadOnly := False;
  FURLDetectionOnMouseMove := True;
  FSelectionFollowsFocus := True;
  FTouchScrolling := True;
  FKeyboardEdit := True;
  FExtendedSelectable := False;
  FMultiSelect := False;
  FColumnSizing := False;
  FExtendedEditable := False;
  FColumnAutoSizeOnDblClick := False;
  FMouseEditMode := tmemSingleClickOnSelectedNode;
  FLookup := TAdvTreeViewLookup.Create(Self);
  FMultiSelectStyle := [msControlSelect, msShiftSelect];
end;

destructor TAdvTreeViewInteraction.Destroy;
begin
  FLookup.Free;
  inherited;
end;

function TAdvTreeViewInteraction.IsAnimationFactorStored: Boolean;
begin
  Result := AnimationFactor <> 2.5;
end;

function TAdvTreeViewInteraction.IsMouseWheelDeltaStored: Boolean;
begin
  Result := MouseWheelDelta <> 1;
end;

procedure TAdvTreeViewInteraction.SetColumnAutoSizeOnDblClick(
  const Value: Boolean);
begin
  if FColumnAutoSizeOnDblClick <> Value then
    FColumnAutoSizeOnDblClick := Value;
end;

procedure TAdvTreeViewInteraction.SetColumnSizing(const Value: Boolean);
begin
  if FColumnSizing <> Value then
  begin
    FColumnSizing := Value;
    if Assigned(FTreeView) and FColumnSizing then
      FTreeView.ColumnsAppearance.StretchAll := False;
  end;
end;

procedure TAdvTreeViewInteraction.SetDragDropMode(const Value: TAdvTreeViewDragDropMode);
begin
  if FDragDropMode <> Value then
    FDragDropMode := Value;
end;

procedure TAdvTreeViewInteraction.SetExpandCollapseOnDblClick(
  const Value: Boolean);
begin
  if FExpandCollapseOnDblClick <> Value then
    FExpandCollapseOnDblClick := Value;
end;

procedure TAdvTreeViewInteraction.SetExtendedEditable(const Value: Boolean);
begin
  if FExtendedEditable <> Value then
    FExtendedEditable := Value;
end;

procedure TAdvTreeViewInteraction.SetExtendedSelectable(
  const Value: Boolean);
begin
  if FExtendedSelectable <> Value then
    FExtendedSelectable := Value;
end;

procedure TAdvTreeViewInteraction.SetLookup(
  const Value: TAdvTreeViewLookup);
begin
  if FLookup <> Value then
    FLookup.Assign(Value);
end;

procedure TAdvTreeViewInteraction.SetMouseEditMode(
  const Value: TAdvTreeViewMouseEditMode);
begin
  if FMouseEditMode <> Value then
    FMouseEditMode := Value;
end;

procedure TAdvTreeViewInteraction.SetMouseWheelDelta(const Value: Single);
begin
  FMouseWheelDelta := Value;
end;

procedure TAdvTreeViewInteraction.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
    FMultiSelect := Value;
end;

procedure TAdvTreeViewInteraction.SetMultiSelectStyle(const Value: TMultiSelectStyle);
begin
  if Value <> FMultiSelectStyle then
  begin
    FMultiSelectStyle := Value;
  end;
end;

procedure TAdvTreeViewInteraction.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
    FReadOnly := Value;
end;

procedure TAdvTreeViewInteraction.SetReorder(const Value: Boolean);
begin
  if FReorder <> Value then
    FReorder := Value;
end;

procedure TAdvTreeViewInteraction.SetSelectionFollowsFocus(
  const Value: Boolean);
begin
  if FSelectionFollowsFocus <> Value then
    FSelectionFollowsFocus := Value;
end;

procedure TAdvTreeViewInteraction.SetTouchScrolling(const Value: Boolean);
begin
  if FTouchScrolling <> Value then
    FTouchScrolling := Value;
end;

procedure TAdvTreeViewInteraction.SetURLDetectionOnMouseMove(
  const Value: Boolean);
begin
  if FURLDetectionOnMouseMove <> Value then
  begin
    FURLDetectionOnMouseMove := Value;
  end;
end;

{$IFDEF CMNWEBLIB}
constructor TAdvTreeViewEdit.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TAdvCustomTreeView then
    FTreeView := AOwner as TAdvCustomTreeView;
end;

procedure TAdvTreeViewEdit.DoExit;
begin
  inherited;
  if Assigned(FTreeView) and FTreeView.FInplaceEditorActive then
    FTreeView.CloseInplaceEditor(False);
end;

procedure TAdvTreeViewEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(FTreeView) and FTreeView.FInplaceEditorActive then
  begin
    case Key of
      KEY_ESCAPE:
      begin
        FTreeView.CloseInplaceEditor(True, True);
        Key := 0;
      end;
      KEY_F2, KEY_RETURN:
      begin
        FTreeView.CloseInplaceEditor(False, True);
        Key := 0;
      end;
    end;
  end;
end;

constructor TAdvTreeViewMemo.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TAdvCustomTreeView then
    FTreeView := AOwner as TAdvCustomTreeView;
end;

procedure TAdvTreeViewMemo.DoExit;
begin
  inherited;
  if Assigned(FTreeView) and FTreeView.FInplaceEditorActive then
    FTreeView.CloseInplaceEditor(False);
end;

procedure TAdvTreeViewMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(FTreeView) and FTreeView.FInplaceEditorActive then
  begin
    case Key of
      KEY_ESCAPE:
      begin
        FTreeView.CloseInplaceEditor(True, True);
        Key := 0;
      end;
      KEY_F2:
      begin
        FTreeView.CloseInplaceEditor(False, True);
        Key := 0;
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FMXLIB}
function TAdvTreeViewEdit.GetDefaultStyleLookupName: string;
begin
  Result := 'editstyle';
end;
{$ENDIF}

{ TAdvTreeViewLookup }

procedure TAdvTreeViewLookup.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewLookup then
  begin
    FEnabled := (Source as TAdvTreeViewLookup).Enabled;
    FCaseSensitive := (Source as TAdvTreeViewLookup).CaseSensitive;
    FIncremental := (Source as TAdvTreeViewLookup).Incremental;
    FColumn := (Source as TAdvTreeViewLookup).Column;
    FAutoSelect := (Source as TAdvTreeViewLookup).AutoSelect;
    FAutoExpand := (Source as TAdvTreeViewLookup).AutoExpand;
    FRootNodesOnly := (Source as TAdvTreeViewLookup).RootNodesOnly;
    FVisibleNodesOnly := (Source as TAdvTreeViewLookup).VisibleNodesOnly;
  end;
end;

constructor TAdvTreeViewLookup.Create(AOwner: TAdvTreeViewInteraction);
begin
  FOwner := AOwner;
  FEnabled := True;
  FCaseSensitive := False;
  FIncremental := True;
  FColumn := -1;
  FAutoSelect := True;
  FAutoExpand := False;
  FRootNodesOnly := False;
  FVisibleNodesOnly := False;
end;

destructor TAdvTreeViewLookup.Destroy;
begin

  inherited;
end;

{ TAdvTreeViewAdapter }

procedure TAdvTreeViewAdapter.ExportNotification(AState: TAdvTreeViewExportState; ARow: Integer);
begin

end;

constructor TAdvTreeViewAdapter.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FActive := False;
  if IsDesignTime and Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TAdvCustomTreeView) then
      begin
        TreeView := AOwner.Components[i] as TAdvCustomTreeView;
        Break;
      end;
    end;
  end;
end;

procedure TAdvTreeViewAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTreeView) then
    FTreeView := nil;
end;

procedure TAdvTreeViewAdapter.ScrollTreeView(ADelta: Integer);
begin
end;

procedure TAdvTreeViewAdapter.SelectNode(ANode: TAdvTreeViewVirtualNode);
begin

end;

procedure TAdvTreeViewAdapter.SetActive(const Value: boolean);
begin
  FActive := Value;
  if not Assigned(TreeView) then
    Exit;
  Initialize;
end;

procedure TAdvTreeViewAdapter.SetTreeView(const Value: TAdvCustomTreeView);
begin
  if FTreeView <> Value then
  begin
    FTreeView := Value;
    if FBlockAdd then
      Exit;

    FBlockAdd := True;
    if Assigned(FTreeView) then
    begin
      FTreeView.Adapter := Self;
      Initialize;
    end;
    FBlockAdd := False;
  end;
end;

procedure TAdvTreeViewAdapter.UpdateBounds;
begin

end;

procedure TAdvTreeViewAdapter.GetNodeText(ACol: Integer; ANode: TAdvTreeViewVirtualNode;
  var AText: String);
begin

end;

function TAdvTreeViewAdapter.GetColumnDisplayName(ACol: Integer): String;
begin
  Result := '';
end;

function TAdvTreeViewAdapter.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TAdvTreeViewAdapter.GetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer);
begin

end;

procedure TAdvTreeViewAdapter.Initialize;
begin
end;

{$IFDEF WEBLIB}
function TAdvTreeViewCache.GetItem(Index: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem(inherited Items[Index]);
end;

procedure TAdvTreeViewCache.SetItem(Index: Integer; const Value: TAdvTreeViewCacheItem);
begin
  inherited Items[Index] := Value;
end;

function TAdvTreeViewDisplayList.GetItem(Index: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem(inherited Items[Index]);
end;

procedure TAdvTreeViewDisplayList.SetItem(Index: Integer; const Value: TAdvTreeViewCacheItem);
begin
  inherited Items[Index] := Value;
end;

function TAdvTreeViewSelectedNodes.GetItem(Index: Integer): TAdvTreeViewVirtualNode;
begin
  Result := TAdvTreeViewVirtualNode(inherited Items[Index]);
end;

procedure TAdvTreeViewSelectedNodes.SetItem(Index: Integer; const Value: TAdvTreeViewVirtualNode);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{$IFDEF FNCLIB}
{ TAdvTreeViewViewJSONOptions }

procedure TAdvTreeViewViewJSONOptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvTreeViewViewJSONOptions) then
  begin
    FExpandNodes := (Source as TAdvTreeViewViewJSONOptions).ExpandNodes;
    FStretchColumn := (Source as TAdvTreeViewViewJSONOptions).StretchColumn;
    FNameHTMLTemplate := (Source as TAdvTreeViewViewJSONOptions).NameHTMLTemplate;
    FValueHTMLTemplate := (Source as TAdvTreeViewViewJSONOptions).ValueHTMLTemplate;
    FArrayItemPrefix := (Source as TAdvTreeViewViewJSONOptions).ArrayItemPrefix;
    FArrayItemSuffix := (Source as TAdvTreeViewViewJSONOptions).ArrayItemSuffix;
  end
  else
    inherited;
end;

procedure TAdvTreeViewViewJSONOptions.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvTreeViewViewJSONOptions.Create;
begin
  FExpandNodes := True;
  FStretchColumn := 1;
  FNameHTMLTemplate := '<#NAME>';
  FValueHTMLTemplate := '<#VALUE>';
  FArrayItemPrefix := '';
  FArrayItemSuffix := '';
end;

destructor TAdvTreeViewViewJSONOptions.Destroy;
begin

  inherited;
end;
{$ENDIF}

{$IFDEF WEBLIB}
initialization
begin
  TAdvBitmap.CreateFromResource(TAdvTREEVIEWEXPAND);
  TAdvBitmap.CreateFromResource(TAdvTREEVIEWEXPANDLARGE);
  TAdvBitmap.CreateFromResource(TAdvTREEVIEWCOLLAPSE);
  TAdvBitmap.CreateFromResource(TAdvTREEVIEWCOLLAPSELARGE);
end;
{$ENDIF}

end.
