{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2018 - 2022                               }
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

unit AdvKanbanBoard;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvCustomControl, AdvPersistence, AdvCustomScrollControl, PictureContainer, AdvCustomComponent,
  AdvTypes, Controls, AdvGraphics, AdvTableView, AdvTreeViewData, ExtCtrls, AdvToolBarEx
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,UITypes, Types, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,Contnrs
  {$ENDIF}
  ,AdvGraphicsTypes
  ;


resourcestring
  sAdvKanbanBoardHeader = 'Header';
  sAdvKanbanBoardFooter = 'Footer';

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 1; // Build nr.

  //version history
  //v1.0.0.0 : First Release
  //v1.0.0.1 : Fixed : Issue with not properly repainting after dropping item
  //v1.0.0.2 : Improved : Issue with persisting scroll location after expanding / collapsing
  //v1.0.0.3 : Fixed : Issue with recalculating items during startup
  //v1.0.1.0 : New :  OnColumnAnchorClick, OnItemTitleAnchorClick and OnItemTextAnchorClick events implemented
  //v1.0.1.1 : Fixed : Memory leak in ColumnsAppearance class
  //v1.0.1.1 : Fixed : Issue with selection event being triggered when programmatically selecting an item
  //v1.0.1.2 : Fixed : Issue with retrieving correct item when filtering
  //v1.0.2.0 : New : OnItemClick & OnItemDblClick events
  //v1.0.3.0 : Improved : GlobalFont interface implemented
  //         : Improved : Updated initial look
  //v1.0.3.1 : Fixed : Issue with drag & drop items
  
type
  TAdvCustomKanbanBoard = class;

  TAdvKanbanBoardColumn = class;

  TAdvKanbanBoardItemCheckType = (kbictNone, kbictCheckBox, kbictRadioButton);

  TAdvKanbanBoardMarkType = (kbmtLeft, kbmtTop, kbmtRight, kbmtBottom);

  TAdvKanbanBoardMarkTypes = set of TAdvKanbanBoardMarkType;

  TAdvKanbanBoardTableView = class(TAdvTableView)
  private
    FOriginalBoundsRect: TRectF;
    FKanbanBoard: TAdvCustomKanbanBoard;
    FColumn: TAdvKanbanBoardColumn;
  protected
    procedure DoAfterDropItem(AFromItem, AToItem: TAdvTableViewItem); override;
    procedure DragItemMove(AFromItem, AToItem: TAdvTableViewItem); override;
    procedure DragItemAdd(ANewItem, AAssignItem, AInsertItem: TAdvTableViewItem); override;
    procedure DragItemDelete(AItem: TAdvTableViewItem); override;
    procedure DoBeforeApplyFilter(AFilter: TAdvTableViewFilterData; AAllow: Boolean); override;
    procedure DoAfterApplyFilter(AFilter: TAdvTableViewFilterData); override;
    procedure DoDoneButtonClicked(Sender: TObject); override;
    procedure CustomizeButtons; override;
    procedure DoColumnSort(Sender: TObject; {%H-}AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode); override;
    function CreateTreeView: TAdvTreeViewTableView; override;
  end;

  TAdvKanbanBoardItem = class(TCollectionItem)
  private
    FTag: NativeInt;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FColumn: TAdvKanbanBoardColumn;
    FText: String;
    FDBKey: String;
    FChecked: Boolean;
    FDataBoolean: Boolean;
    FWordWrapping: Boolean;
    FTrimming: TAdvGraphicsTextTrimming;
    FHorizontalTextAlign: TAdvGraphicsTextAlign;
    FSelectedTextColor: TAdvGraphicsColor;
    FTextColor: TAdvGraphicsColor;
    FDisabledTextColor: TAdvGraphicsColor;
    FEnabled: Boolean;
    FBitmapName: string;
    FBitmap: TAdvBitmap;
    FHeight: Double;
    FDataPointer: Pointer;
    FHTMLTemplateItems: TStringList;
    FVisible: Boolean;
    FCheckType: TAdvKanbanBoardItemCheckType;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FTitle: String;
    FMarkType: TAdvKanbanBoardMarkTypes;
    FMarkColor: TAdvGraphicsColor;
    FMarkSizeLeft: Single;
    FMarkSizeTop: Single;
    FMarkSizeRight: Single;
    FMarkSizeBottom: Single;
    FTitleTrimming: TAdvGraphicsTextTrimming;
    FTitleHorizontalTextAlign: TAdvGraphicsTextAlign;
    FDisabledTitleColor: TAdvGraphicsColor;
    FTitleWordWrapping: Boolean;
    FSelectedTitleColor: TAdvGraphicsColor;
    FTitleColor: TAdvGraphicsColor;
    FTitleVerticalTextAlign: TAdvGraphicsTextAlign;
    FExpanded: Boolean;
    FUseDefaultAppearance: Boolean;
    FColor: TAdvGraphicsColor;
    FExpandable: Boolean;
    FMarkColorBottom: TAdvGraphicsColor;
    FMarkColorTop: TAdvGraphicsColor;
    FMarkColorLeft: TAdvGraphicsColor;
    FMarkColorRight: TAdvGraphicsColor;
    FMovable: Boolean;
    FRounding: Integer;
    FRoundingCorners: TAdvGraphicsCorners;
    FMarkCorners: TAdvGraphicsCorners;
    FMarkRounding: Integer;
    FDisabledStrokeColor: TAdvGraphicsColor;
    FSelectedStrokeColor: TAdvGraphicsColor;
    FStrokeColor: TAdvGraphicsColor;
    FDisabledColor: TAdvGraphicsColor;
    FSelectedColor: TAdvGraphicsColor;
    procedure SetText(const Value: String);
    procedure SetHeight(const Value: Double);
    procedure SetTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetSelectedTextColor(const Value: TAdvGraphicsColor);
    procedure SetTextColor(const Value: TAdvGraphicsColor);
    procedure SetDisabledTextColor(const Value: TAdvGraphicsColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetBitmapName(const Value: string);
    procedure SetBitmap(const Value: TAdvBitmap);
    procedure SetChecked(const Value: Boolean);
    function GetPictureContainer: TPictureContainer;
    function GetStrippedHTMLText: string;
    function IsHeightStored: Boolean;
    procedure SetHTMLTemplateItems(const Value: TStringList);
    procedure SetVisible(const Value: Boolean);
    procedure SetCheckType(const Value: TAdvKanbanBoardItemCheckType);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTitle(const Value: String);
    procedure SetMarkType(const Value: TAdvKanbanBoardMarkTypes);
    procedure SetMarkColor(const Value: TAdvGraphicsColor);
    function IsMarkSizeLeftStored: Boolean;
    procedure SetMarkSizeLeft(const Value: Single);
    procedure SetDisabledTitleColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedTitleColor(const Value: TAdvGraphicsColor);
    procedure SetTitleColor(const Value: TAdvGraphicsColor);
    procedure SetTitleHorizontalTextAlign(
      const Value: TAdvGraphicsTextAlign);
    procedure SetTitleTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetTitleVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTitleWordWrapping(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetUseDefaultAppearance(const Value: Boolean);
    procedure SetExpandable(const Value: Boolean);
    procedure SetMarkColorBottom(const Value: TAdvGraphicsColor);
    procedure SetMarkColorLeft(const Value: TAdvGraphicsColor);
    procedure SetMarkColorRight(const Value: TAdvGraphicsColor);
    procedure SetMarkColorTop(const Value: TAdvGraphicsColor);
    function IsMarkSizeBottomStored: Boolean;
    function IsMarkSizeRightStored: Boolean;
    function IsMarkSizeTopStored: Boolean;
    procedure SetMarkSizeBottom(const Value: Single);
    procedure SetMarkSizeRight(const Value: Single);
    procedure SetMarkSizeTop(const Value: Single);
    procedure SetRounding(const Value: Integer);
    procedure SetRoundingCorners(const Value: TAdvGraphicsCorners);
    procedure SetMarkCorners(const Value: TAdvGraphicsCorners);
    procedure SetMarkRounding(const Value: Integer);
    procedure SetDisabledStrokeColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedStrokeColor(const Value: TAdvGraphicsColor);
    procedure SetStrokeColor(const Value: TAdvGraphicsColor);
    procedure SetDisabledColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedColor(const Value: TAdvGraphicsColor);
  protected
    procedure UpdateItem;
    procedure Changed(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
    procedure TemplateItemsChanged(Sender: TObject);
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckType: TAdvKanbanBoardItemCheckType read FCheckType write SetCheckType default kbictNone;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetColumn: TAdvKanbanBoardColumn;
    function GetTableView: TAdvKanbanBoardTableView;
    function GetText: string;
    function GetTableViewItem: TAdvTableViewItem;
    function SaveToString(ATextOnly: Boolean = True): String; virtual;
    function IsSelected: Boolean; virtual;
    function MoveItem(AToColumn: TAdvKanbanBoardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem; virtual;
    function CopyItem(AToColumn: TAdvKanbanBoardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem; virtual;
    procedure SelectItem;
    procedure LoadFromString(AString: String); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignData(Source: TPersistent); virtual;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    property PictureContainer: TPictureContainer read GetPictureContainer;
    property StrippedHTMLText: String read GetStrippedHTMLText;
    property Column: TAdvKanbanBoardColumn read GetColumn;
    property TableView: TAdvKanbanBoardTableView read GetTableView;
  published
    property Movable: Boolean read FMovable write FMovable default True;
    property Text: String read FText write SetText;
    property Title: String read FTitle write SetTitle;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Expandable: Boolean read FExpandable write SetExpandable default False;
    property HTMLTemplateItems: TStringList read FHTMLTemplateItems write SetHTMLTemplateItems;
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapName: string read FBitmapName write SetBitmapName;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default True;
    property Trimming: TAdvGraphicsTextTrimming read FTrimming write SetTrimming default gttNone;
    property HorizontalTextAlign: TAdvGraphicsTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default gtaLeading;
    property VerticalTextAlign: TAdvGraphicsTextAlign read FVerticalTextAlign write SetVerticalTextAlign default gtaLeading;
    property TextColor: TAdvGraphicsColor read FTextColor write SetTextColor default gcBlack;
    property SelectedTextColor: TAdvGraphicsColor read FSelectedTextColor write SetSelectedTextColor default gcWhite;
    property DisabledTextColor: TAdvGraphicsColor read FDisabledTextColor write SetDisabledTextColor default gcSilver;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Height: Double read FHeight write SetHeight stored IsHeightStored nodefault;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: NativeInt read FTag write FTag default 0;
    property UseDefaultAppearance: Boolean read FUseDefaultAppearance write SetUseDefaultAppearance default True;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcWhite;
    property SelectedColor: TAdvGraphicsColor read FSelectedColor write SetSelectedColor;
    property DisabledColor: TAdvGraphicsColor read FDisabledColor write SetDisabledColor default gcDarkGray;
    property MarkType: TAdvKanbanBoardMarkTypes read FMarkType write SetMarkType default [];
    property MarkColor: TAdvGraphicsColor read FMarkColor write SetMarkColor default gcRed;
    property MarkColorLeft: TAdvGraphicsColor read FMarkColorLeft write SetMarkColorLeft default gcNull;
    property MarkColorRight: TAdvGraphicsColor read FMarkColorRight write SetMarkColorRight default gcNull;
    property MarkColorTop: TAdvGraphicsColor read FMarkColorTop write SetMarkColorTop default gcNull;
    property MarkColorBottom: TAdvGraphicsColor read FMarkColorBottom write SetMarkColorBottom default gcNull;
    property MarkSizeLeft: Single read FMarkSizeLeft write SetMarkSizeLeft stored IsMarkSizeLeftStored nodefault;
    property MarkSizeTop: Single read FMarkSizeTop write SetMarkSizeTop stored IsMarkSizeTopStored nodefault;
    property MarkSizeRight: Single read FMarkSizeRight write SetMarkSizeRight stored IsMarkSizeRightStored nodefault;
    property MarkSizeBottom: Single read FMarkSizeBottom write SetMarkSizeBottom stored IsMarkSizeBottomStored nodefault;
    property MarkRounding: Integer read FMarkRounding write SetMarkRounding default 0;
    property MarkCorners: TAdvGraphicsCorners read FMarkCorners write SetMarkCorners;
    property TitleWordWrapping: Boolean read FTitleWordWrapping write SetTitleWordWrapping default True;
    property TitleTrimming: TAdvGraphicsTextTrimming read FTitleTrimming write SetTitleTrimming default gttNone;
    property TitleHorizontalTextAlign: TAdvGraphicsTextAlign read FTitleHorizontalTextAlign write SetTitleHorizontalTextAlign default gtaLeading;
    property TitleVerticalTextAlign: TAdvGraphicsTextAlign read FTitleVerticalTextAlign write SetTitleVerticalTextAlign default gtaLeading;
    property TitleColor: TAdvGraphicsColor read FTitleColor write SetTitleColor default gcBlack;
    property SelectedTitleColor: TAdvGraphicsColor read FSelectedTitleColor write SetSelectedTitleColor default gcWhite;
    property DisabledTitleColor: TAdvGraphicsColor read FDisabledTitleColor write SetDisabledTitleColor default gcSilver;
    property Rounding: Integer read FRounding write SetRounding default 0;
    property RoundingCorners: TAdvGraphicsCorners read FRoundingCorners write SetRoundingCorners;
    property StrokeColor: TAdvGraphicsColor read FStrokeColor write SetStrokeColor default gcDarkGray;
    property DisabledStrokeColor: TAdvGraphicsColor read FDisabledStrokeColor write SetDisabledStrokeColor default gcDarkgray;
    property SelectedStrokeColor: TAdvGraphicsColor read FSelectedStrokeColor write SetSelectedStrokeColor;
  end;

  TAdvKanbanBoardItemsSortMode = (kbismAscending, kbismDescending);
  TAdvKanbanBoardItemsSortKind = (kbiskNone, kbiskAscending, kbiskDescending);
  TAdvKanbanBoardSorting = (kbsNone, kbsNormal, kbsNormalCaseSensitive);

  {$IFDEF WEBLIB}
  TAdvKanbanBoardItems = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvKanbanBoardItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvKanbanBoardItem>)
  {$ENDIF}
  private
    FColumn: TAdvKanbanBoardColumn;
    function GetItem(Index: Integer): TAdvKanbanBoardItem;
    procedure SetItem(Index: Integer; const Value: TAdvKanbanBoardItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    function Compare(AItem1, AItem2: TAdvKanbanBoardItem; ACaseSensitive: Boolean = True; ASortingMode: TAdvKanbanBoardItemsSortMode = kbismAscending): Integer; virtual;
    procedure QuickSort(L, R: Integer; ACaseSensitive: Boolean = True; ASortingMode: TAdvKanbanBoardItemsSortMode = kbismAscending); virtual;
  public
    function GetColumn: TAdvKanbanBoardColumn;
    function GetTableView: TAdvKanbanBoardTableView;
    constructor Create(AColumn: TAdvKanbanBoardColumn);
    function Add: TAdvKanbanBoardItem; virtual;
    function Insert(Index: Integer): TAdvKanbanBoardItem;
    procedure Clear; virtual;
    procedure Sort(ACaseSensitive: Boolean = True; ASortingMode: TAdvKanbanBoardItemsSortMode = kbismAscending);
    property Items[Index: Integer]: TAdvKanbanBoardItem read GetItem write SetItem; default;
    property Column: TAdvKanbanBoardColumn read GetColumn;
    property TableView: TAdvKanbanBoardTableView read GetTableView;
  end;

  TAdvKanbanBoardIntegerArray = array of Integer;
  TAdvKanbanBoardItemArray = array of TAdvKanbanBoardItem;
  TAdvKanbanBoardSelectedItems = TAdvKanbanBoardItemArray;

  TAdvKanbanBoardColumn = class(TCollectionItem)
  private
    FBlockUpdate: Boolean;
    FTag: NativeInt;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FTableView: TAdvKanbanBoardTableView;
    FKanbanBoard: TAdvCustomKanbanBoard;
    FHeaderText: String;
    FName: String;
    FDBKey: String;
    FDataBoolean: Boolean;
    FHeaderWordWrapping: Boolean;
    FHeaderVerticalTextAlign: TAdvGraphicsTextAlign;
    FHeaderTrimming: TAdvGraphicsTextTrimming;
    FHeaderHorizontalTextAlign: TAdvGraphicsTextAlign;
    FWidth: Double;
    FVisible: Boolean;
    FUseDefaultAppearance: Boolean;
    FFooterFill: TAdvGraphicsFill;
    FFooterStroke: TAdvGraphicsStroke;
    FHeaderFill: TAdvGraphicsFill;
    FHeaderStroke: TAdvGraphicsStroke;
    FHeaderFont: TAdvGraphicsFont;
    FFooterFont: TAdvGraphicsFont;
    FFont: TAdvGraphicsFont;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FDataPointer: Pointer;
    FItems: TAdvKanbanBoardItems;
    FFooterVerticalTextAlign: TAdvGraphicsTextAlign;
    FFooterTrimming: TAdvGraphicsTextTrimming;
    FFooterHorizontalTextAlign: TAdvGraphicsTextAlign;
    FFooterText: String;
    FFooterWordWrapping: Boolean;
    FFooterVisible: Boolean;
    FHeaderVisible: Boolean;
    FExpanded: Boolean;
    FExpandable: Boolean;
    FCollapsedWidth: Double;
    procedure SetHeaderText(const Value: String);
    procedure SetName(const Value: String);
    procedure SetHeaderHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetHeaderVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderWordWrapping(const Value: Boolean);
    procedure SetWidth(const Value: Double);
    procedure SetVisible(const Value: Boolean);
    procedure SetUseDefaultAppearance(const Value: Boolean);
    procedure SetFooterFill(const Value: TAdvGraphicsFill);
    procedure SetFooterStroke(const Value: TAdvGraphicsStroke);
    procedure SetHeaderFill(const Value: TAdvGraphicsFill);
    procedure SetHeaderStroke(const Value: TAdvGraphicsStroke);
    procedure SetFooterFont(const Value: TAdvGraphicsFont);
    procedure SetHeaderFont(const Value: TAdvGraphicsFont);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetItems(const Value: TAdvKanbanBoardItems);
    function GetPictureContainer: TPictureContainer;
    function GetSelectedItem: TAdvKanbanBoardItem;
    function GetSelItem(AIndex: Integer): TAdvKanbanBoardItem;
    procedure SetSelectedItem(const Value: TAdvKanbanBoardItem);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetFooterHorizontalTextAlign(
      const Value: TAdvGraphicsTextAlign);
    procedure SetFooterText(const Value: String);
    procedure SetFooterTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetFooterVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetFooterWordWrapping(const Value: Boolean);
    procedure SetFooterVisible(const Value: Boolean);
    procedure SetHeaderVisible(const Value: Boolean);
    procedure SetExpandable(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetCollapsedWidth(const Value: Double);
    function IsCollapsedWidthStored: Boolean;
    function GetFilter: TAdvTableViewFilter;
    function GetShowFilterButton: Boolean;
    procedure SetFilter(const Value: TAdvTableViewFilter);
    procedure SetShowFilterButton(const Value: Boolean);
    function GetSorting: TAdvKanbanBoardSorting;
    procedure SetSorting(const Value: TAdvKanbanBoardSorting);
    function GetDoneButton: TAdvToolBarExButton;
    function GetFilterButton: TAdvToolBarExButton;
  protected
    procedure UpdateColumn(AUpdate: Boolean = True; ARealign: Boolean = True);
    procedure Changed(Sender: TObject);
    function GetHeaderText: String; virtual;
    function GetHeaderColumnText: String; virtual;
    function GetFooterText: String; virtual;
    function GetFooterColumnText: String; virtual;
    function CreateTableView: TAdvKanbanBoardTableView; virtual;
    function CreateItems: TAdvKanbanBoardItems; virtual;
    procedure DoAfterDrawItem(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem); virtual;
    procedure DoBeforeDrawItem(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawItemText(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: string); virtual;
    procedure DoBeforeDrawItemText(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: string; var AAllow: Boolean); virtual;
    procedure DoAfterDrawItemIcon(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AIcon: TAdvBitmap); virtual;
    procedure DoBeforeDrawItemIcon(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AIcon: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawItemTitle(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: string); virtual;
    procedure DoBeforeDrawItemTitle(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: string; var AAllow: Boolean); virtual;
    procedure DoBeforeDropItem(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem; var ACanDrop: Boolean); virtual;
    procedure DoAfterDropItem(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem); virtual;
    procedure DoClick(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure DoHeaderAnchorClick(Sender: TObject; AAnchor: String); virtual;
    procedure DoItemTextAnchorClick(Sender: TObject; AItem: TAdvTableViewItem; AAnchor: String); virtual;
    procedure DoItemTitleAnchorClick(Sender: TObject; AItem: TAdvTableViewItem; AAnchor: String); virtual;
    procedure DoItemClick(Sender: TObject; AItem: TAdvTableViewItem);
    procedure DoItemDblClick(Sender: TObject; AItem: TAdvTableViewItem);
    {$IFDEF FMXLIB}
    procedure DoKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure DoKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    {$ENDIF}
  public
    function GetKanbanBoard: TAdvCustomKanbanBoard;
    function GetTableView: TAdvKanbanBoardTableView;
    function FindItem(AIndex: Integer): TAdvKanbanBoardItem;
    function FindItemByNode(ANode: TAdvTreeViewVirtualNode): TAdvKanbanBoardItem;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateWidth(AWidth: Double); virtual;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    property PictureContainer: TPictureContainer read GetPictureContainer;
    property KanbanBoard: TAdvCustomKanbanBoard read GetKanbanBoard;
    property TableView: TAdvKanbanBoardTableView read GetTableView;

    function AddItem(AText: string = ''): TAdvKanbanBoardItem; virtual;
    function XYToItem(X, Y: Single): TAdvKanbanBoardItem; virtual;
    function XYToItemIndex(X, Y: Single): Integer; virtual;
    function SelectedItemCount: Integer; virtual;
    function LookupItem(ALookupString: String; ACaseSensitive: Boolean = False; AAutoSelect: Boolean = False): TAdvKanbanBoardItem; virtual;
    function GetSelectedItems: TAdvKanbanBoardItemArray; virtual;
    function MoveItem(AItem: TAdvKanbanBoardItem; AToColumn: TAdvKanbanboardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem; virtual;
    function CopyItem(AItem: TAdvKanbanBoardItem; AToColumn: TAdvKanbanboardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem; virtual;
    procedure StartFiltering; virtual;
    procedure StopFiltering; virtual;
    procedure RemoveItem(AItem: TAdvKanbanBoardItem); virtual;
    procedure EnableInteraction; virtual;
    procedure DisableInteraction; virtual;
    procedure ScrollToItem(AItemIndex: Integer); virtual;
    procedure ClearSelection; virtual;
    procedure StartEditMode; virtual;
    procedure StopEditMode; virtual;
    procedure ToggleEditMode; virtual;
    procedure ApplyFilter; virtual;
    procedure RemoveFilter; virtual;
    procedure RemoveFilters; virtual;
    procedure SelectItem(AItemIndex: Integer); virtual;
    procedure SelectItems(AItemIndexes: TAdvKanbanBoardIntegerArray); virtual;
    property SelectedItem: TAdvKanbanBoardItem read GetSelectedItem write SetSelectedItem;
    property SelectedItems[AIndex: Integer]: TAdvKanbanBoardItem read GetSelItem;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property DoneButton: TAdvToolBarExButton read GetDoneButton;
    property FilterButton: TAdvToolBarExButton read GetFilterButton;
  published
    property Name: String read FName write SetName;
    property HeaderText: String read FHeaderText write SetHeaderText;
    property FooterText: String read FFooterText write SetFooterText;
    property Tag: NativeInt read FTag write FTag default 0;
    property HeaderHorizontalTextAlign: TAdvGraphicsTextAlign read FHeaderHorizontalTextAlign write SetHeaderHorizontalTextAlign default gtaCenter;
    property HeaderVerticalTextAlign: TAdvGraphicsTextAlign read FHeaderVerticalTextAlign write SetHeaderVerticalTextAlign default gtaCenter;
    property HeaderWordWrapping: Boolean read FHeaderWordWrapping write SetHeaderWordWrapping default False;
    property HeaderTrimming: TAdvGraphicsTextTrimming read FHeaderTrimming write SetHeaderTrimming default gttCharacter;
    property FooterHorizontalTextAlign: TAdvGraphicsTextAlign read FFooterHorizontalTextAlign write SetFooterHorizontalTextAlign default gtaCenter;
    property FooterVerticalTextAlign: TAdvGraphicsTextAlign read FFooterVerticalTextAlign write SetFooterVerticalTextAlign default gtaCenter;
    property FooterWordWrapping: Boolean read FFooterWordWrapping write SetFooterWordWrapping default False;
    property FooterTrimming: TAdvGraphicsTextTrimming read FFooterTrimming write SetFooterTrimming default gttCharacter;
    property Width: Double read FWidth write SetWidth;
    property CollapsedWidth: Double read FCollapsedWidth write SetCollapsedWidth stored IsCollapsedWidthStored nodefault;
    property Visible: Boolean read FVisible write SetVisible default True;
    property FooterVisible: Boolean read FFooterVisible write SetFooterVisible default False;
    property HeaderVisible: Boolean read FHeaderVisible write SetHeaderVisible default True;
    property UseDefaultAppearance: Boolean read FUseDefaultAppearance write SetUseDefaultAppearance default True;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property HeaderFill: TAdvGraphicsFill read FHeaderFill write SetHeaderFill;
    property HeaderStroke: TAdvGraphicsStroke read FHeaderStroke write SetHeaderStroke;
    property HeaderFont: TAdvGraphicsFont read FHeaderFont write SetHeaderFont;
    property FooterFill: TAdvGraphicsFill read FFooterFill write SetFooterFill;
    property FooterStroke: TAdvGraphicsStroke read FFooterStroke write SetFooterStroke;
    property FooterFont: TAdvGraphicsFont read FFooterFont write SetFooterFont;
    property Items: TAdvKanbanBoardItems read FItems write SetItems;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Expandable: Boolean read FExpandable write SetExpandable default False;
    property ShowFilterButton: Boolean read GetShowFilterButton write SetShowFilterButton default False;
    property Filter: TAdvTableViewFilter read GetFilter write SetFilter;
    property Sorting: TAdvKanbanBoardSorting read GetSorting write SetSorting default kbsNone;
  end;

  {$IFDEF WEBLIB}
  TAdvKanbanBoardColumns = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvKanbanBoardColumns = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvKanbanBoardColumn>)
  {$ENDIF}
  private
    FKanbanBoard: TAdvCustomKanbanBoard;
    function GetItem(Index: Integer): TAdvKanbanBoardColumn;
    procedure SetItem(Index: Integer; const Value: TAdvKanbanBoardColumn);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function GetKanbanBoard: TAdvCustomKanbanBoard;
    constructor Create(AKanbanBoard: TAdvCustomKanbanBoard);
    function Add: TAdvKanbanBoardColumn; virtual;
    function Insert(Index: Integer): TAdvKanbanBoardColumn;
    property Items[Index: Integer]: TAdvKanbanBoardColumn read GetItem write SetItem; default;
    property KanbanBoard: TAdvCustomKanbanBoard read GetKanbanBoard;
  end;

  TAdvKanbanBoardColumnsAppearance = class(TPersistent)
  private
    FKanbanBoard: TAdvCustomKanbanBoard;
    FMargins: TAdvMargins;
    FSpacing: Single;
    FFooterFill: TAdvGraphicsFill;
    FHeaderFont: TAdvGraphicsFont;
    FFooterStroke: TAdvGraphicsStroke;
    FHeaderFill: TAdvGraphicsFill;
    FHeaderStroke: TAdvGraphicsStroke;
    FFooterFont: TAdvGraphicsFont;
    FHeaderSize: Single;
    FFooterSize: Single;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    procedure SetMargins(const Value: TAdvMargins);
    function IsSpacingStored: Boolean;
    procedure SetSpacing(const Value: Single);
    procedure SetFooterFill(const Value: TAdvGraphicsFill);
    procedure SetFooterFont(const Value: TAdvGraphicsFont);
    procedure SetFooterStroke(const Value: TAdvGraphicsStroke);
    procedure SetHeaderFill(const Value: TAdvGraphicsFill);
    procedure SetHeaderFont(const Value: TAdvGraphicsFont);
    procedure SetHeaderStroke(const Value: TAdvGraphicsStroke);
    function IsFooterSizeStored: Boolean;
    function IsHeaderSizeStored: Boolean;
    procedure SetFooterSize(const Value: Single);
    procedure SetHeaderSize(const Value: Single);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(AKanbanBoard: TAdvCustomKanbanBoard);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Margins: TAdvMargins read FMargins write SetMargins;
    property Spacing: Single read FSpacing write SetSpacing stored IsSpacingStored nodefault;
    property HeaderSize: Single read FHeaderSize write SetHeaderSize stored IsHeaderSizeStored nodefault;
    property FooterSize: Single read FFooterSize write SetFooterSize stored IsFooterSizeStored nodefault;
    property HeaderFont: TAdvGraphicsFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TAdvGraphicsFont read FFooterFont write SetFooterFont;
    property HeaderFill: TAdvGraphicsFill read FHeaderFill write SetHeaderFill;
    property FooterFill: TAdvGraphicsFill read FFooterFill write SetFooterFill;
    property HeaderStroke: TAdvGraphicsStroke read FHeaderStroke write SetHeaderStroke;
    property FooterStroke: TAdvGraphicsStroke read FFooterStroke write SetFooterStroke;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
  end;

  TAdvKanbanBoardItemHeightMode = (kbhmFixed, kbhmVariable);

  TAdvKanbanBoardItemsAppearance = class(TPersistent)
  private
    FKanbanBoard: TAdvCustomKanbanBoard;
    FHTMLTemplate: string;
    FHeight: Double;
    FHeightMode: TAdvKanbanBoardItemHeightMode;
    FShowFocus: Boolean;
    FDisabledFill: TAdvGraphicsFill;
    FFont: TAdvGraphicsFont;
    FSelectedFill: TAdvGraphicsFill;
    FFixedHeight: Double;
    FDisabledStroke: TAdvGraphicsStroke;
    FSelectedStroke: TAdvGraphicsStroke;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FTitleFont: TAdvGraphicsFont;
    FSpacing: Double;
    FMargins: TAdvMargins;
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetDisabledStroke(const Value: TAdvGraphicsStroke);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetHeightMode(const Value: TAdvKanbanBoardItemHeightMode);
    procedure SetFixedHeight(const Value: Double);
    procedure SetHTMLTemplate(const Value: string);
    function IsHeightStored: Boolean;
    procedure SetHeight(const Value: Double);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetTitleFont(const Value: TAdvGraphicsFont);
    function IsSpacingStored: Boolean;
    procedure SetSpacing(const Value: Double);
    procedure SetMargins(const Value: TAdvMargins);
  protected
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AKanbanBoard: TAdvCustomKanbanBoard);
    destructor Destroy; override;
  published
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property TitleFont: TAdvGraphicsFont read FTitleFont write SetTitleFont;
    property Spacing: Double read FSpacing write SetSpacing stored IsSpacingStored nodefault;
    property Margins: TAdvMargins read FMargins write SetMargins;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property SelectedFill: TAdvGraphicsFill read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TAdvGraphicsStroke read FSelectedStroke write SetSelectedStroke;
    property DisabledFill: TAdvGraphicsFill read FDisabledFill write SetDisabledFill;
    property DisabledStroke: TAdvGraphicsStroke read FDisabledStroke write SetDisabledStroke;
    property HeightMode: TAdvKanbanBoardItemHeightMode read FHeightMode write SetHeightMode default kbhmVariable;
    property FixedHeight: Double read FFixedHeight write SetFixedHeight;
    property Height: Double read FHeight write SetHeight stored IsHeightStored nodefault;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
  end;

  TAdvKanbanBoardDragDropMode = (kbdmNone, kbdmMove, kbdmCopy);
  TAdvKanbanBoardMouseEditMode = (kbemDoubleClick, kbemSingleClick, kbemSingleClickOnSelectedItem);

  TAdvKanbanBoardInteraction = class(TPersistent)
  private
    FKanbanBoard: TAdvCustomKanbanBoard;
    FMultiSelect: Boolean;
    FSwipeBounceGesture: Boolean;
    FAutoOpenURL: Boolean;
    FTouchScrolling: Boolean;
    FDragDropMode: TAdvKanbanBoardDragDropMode;
    FMouseEditMode: TAdvKanbanBoardMouseEditMode;
    FKeyboardEdit: Boolean;
    FEditing: Boolean;
    FDirectDrag: Boolean;
    procedure SetAutoOpenURL(const Value: Boolean);
    procedure SetDragDropMode(const Value: TAdvKanbanBoardDragDropMode);
    procedure SetKeyboardEdit(const Value: Boolean);
    procedure SetMouseEditMode(const Value: TAdvKanbanBoardMouseEditMode);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetSwipeBounceGesture(const Value: Boolean);
    procedure SetTouchScrolling(const Value: Boolean);
    procedure SetEditing(const Value: Boolean);
    procedure SetDirectDrag(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AKanbanBoard: TAdvCustomKanbanBoard);
    procedure Assign(Source: TPersistent); override;
  published
    property AutoOpenURL: Boolean read FAutoOpenURL write SetAutoOpenURL default True;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property TouchScrolling: Boolean read FTouchScrolling write SetTouchScrolling default True;
    property DragDropMode: TAdvKanbanBoardDragDropMode read FDragDropMode write SetDragDropMode default kbdmNone;
    property SwipeBounceGesture: Boolean read FSwipeBounceGesture write SetSwipeBounceGesture default False;
    property MouseEditMode: TAdvKanbanBoardMouseEditMode read FMouseEditMode write SetMouseEditMode default kbemSingleClickOnSelectedItem;
    property KeyboardEdit: Boolean read FKeyboardEdit write SetKeyboardEdit default True;
    property Editing: Boolean read FEditing write SetEditing default False;
    property DirectDrag: Boolean read FDirectDrag write SetDirectDrag default False;
  end;

  TAdvKanbanBoardTreeViewTableView = class(TAdvTreeViewTableView)
  private
    FDragPoint: TPointF;
    FDragTableView: TAdvKanbanboardTreeViewTableView;
    FDragScrollLeft, FDragScrollRight: Boolean;
    FScrollTable: TAdvKanbanBoardTableView;
    FDragTimerAlternative: TTimer;
    FKanbanBoard: TAdvCustomKanbanBoard;
    FColumn: TAdvKanbanBoardColumn;
    FDragStarted: Boolean;
    FSaveLeft: Single;
    FSaveTop: Single;
  protected
    procedure DragTimeAlternative(Sender: TObject);
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleDragStart(X, Y: Single); override;
    procedure HandleDragOver(const Source: TObject; const Point: TPointF; var Accept: Boolean); override;
    procedure HandleDragDrop(const Source: TObject; const Point: TPointF); override;
    procedure GetNodeOffset(ANode: TAdvTreeViewVirtualNode; var ALeft, ATop, ARight, ABottom: Single); override;
    procedure GetNodeMargins(var ALeft, ATop, ARight, ABottom: Single); override;
    procedure DoGetNodeSides(ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides); override;
    procedure DoGetNodeRounding(ANode: TAdvTreeViewVirtualNode; var ARounding: Integer; var ACorners: TAdvGraphicsCorners); override;
    procedure DoGetNodeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeSelectedColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeSelectedStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeDisabledColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoGetNodeDisabledStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor); override;
    procedure DoUpdateNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AText: String); override;
    procedure DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode); override;
    procedure UpdateSelection(ANode: TAdvTreeViewVirtualNode; AFocus: Boolean; ATriggerEvents: Boolean); virtual;
    procedure DoGetNodeTitleExtraSize(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single); override;
    procedure DoDrawNodeTitleExtra({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; {%H-}AColumn: Integer; {%H-}ANode: TAdvTreeViewVirtualNode); override;
    procedure DrawBorders(AGraphics: TAdvGraphics); override;
    procedure HandleNodeTitleExtra(ANode: TAdvTreeViewVirtualNode); override;
    procedure HandleExpand(AColumn: Integer); override;
    procedure HandleCustomKeys(AKey: Word); override;
    procedure HandleAlternativeDragOver(X, Y: Single); override;
    procedure HandleAlternativeDragDrop(X, Y: Single); override;
    procedure InternalSelectVirtualNode(ANode: TAdvTreeViewVirtualNode); override;
    function GetColumnsExtraSize: Double; override;
    function CanStartDragFromMouseDown: Boolean; override;
    function CanStartDragFromMouseMove: Boolean; override;
    function AlternativeDragDrop: Boolean; override;
    function GetTotalLeft: Single;
    function GetTotalTop: Single;
    function GetContentRect: TRectF; override;
    property SaveLeft: Single read FSaveLeft write FSaveLeft;
    property SaveTop: Single read FSaveTop write FSaveTop;
    property DragStarted: Boolean read FDragStarted write FDragStarted;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TAdvKanbanBoardAdapter = class(TAdvCustomComponent)
  private
    FKanbanBoard: TAdvCustomKanbanBoard;
    FActive: boolean;
    procedure SetActive(const Value: boolean);
  protected
    function GetInstance: NativeUInt; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property KanbanBoard: TAdvCustomKanbanBoard read FKanbanBoard write FKanbanBoard;
    procedure LoadItems; virtual;
    procedure GetItems; virtual; abstract;
    procedure DeleteItem(AItem: TAdvKanbanBoardItem); virtual; abstract;
    procedure InsertItem(AItem: TAdvKanbanBoardItem); virtual; abstract;
    procedure UpdateItem(AItem: TAdvKanbanBoardItem); virtual; abstract;
    procedure AfterUpdateItem(AItem: TAdvKanbanBoardItem); virtual; abstract;
    procedure SelectItem(AItem: TAdvKanbanBoardItem); virtual; abstract;
    procedure UpdateItems; virtual;
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
  end;

  TAdvKanbanBoardDoneButtonClickedEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn) of object;
  TAdvKanbanBoardBeforeApplyFilterEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData; var AAllow: Boolean) of object;
  TAdvKanbanBoardAfterApplyFilterEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData) of object;
  TAdvKanbanBoardItemCompareEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; Item1, Item2: TAdvKanbanBoardItem; var ACompareResult: Integer) of object;
  TAdvKanbanBoardUpdateItemTextEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: String) of object;
  TAdvKanbanBoardItemCustomDrawMarkEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AMarkType: TAdvKanbanBoardMarkType; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardCustomizeColumnEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; ATableView: TAdvKanbanBoardTableView) of object;
  TAdvKanbanBoardBeforeDrawItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvKanbanBoardAfterDrawItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardBeforeDrawItemTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: string; var AAllow: Boolean) of object;
  TAdvKanbanBoardAfterDrawItemTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: string) of object;
  TAdvKanbanBoardBeforeDrawItemIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap; var AAllow: Boolean) of object;
  TAdvKanbanBoardAfterDrawItemIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap) of object;
  TAdvKanbanBoardBeforeDrawItemTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; ATitle: string; var AAllow: Boolean) of object;
  TAdvKanbanBoardAfterDrawItemTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; ATitle: string) of object;
  TAdvKanbanBoardBeforeDropItemEvent = procedure(Sender: TObject; AFromColumn, AToColumn: TAdvKanbanBoardColumn; AFromItem, AToItem: TAdvKanbanBoardItem; var ACanDrop: Boolean) of object;
  TAdvKanbanBoardAfterDropItemEvent = procedure(Sender: TObject; AFromColumn, AToColumn: TAdvKanbanBoardColumn; AFromItem, AToItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardSelectedItemEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardColumnEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn) of object;
  TAdvKanbanBoardItemEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardColumnAnchorClickEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AAnchor: string) of object;
  TAdvKanbanBoardColumnItemAnchorClickEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AAnchor: string) of object;
  TAdvKanbanBoardColumnItemClickEvent = procedure(Sender: TObject; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem) of object;

  {$IFDEF FNCLIB}
  TAdvCustomKanbanBoard = class(TAdvCustomScrollControl, IAdvPictureContainer, IAdvAppearanceGlobalFont)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvCustomKanbanBoard = class(TAdvCustomScrollControl, IAdvAppearanceGlobalFont)
  {$ENDIF}
  private
    FFirstLoad: Boolean;
    FPictureContainer: TPictureContainer;
    FColumns: TAdvKanbanBoardColumns;
    FItemsAppearance: TAdvKanbanBoardItemsAppearance;
    FColumnsAppearance: TAdvKanbanBoardColumnsAppearance;
    FOnItemCustomDrawMark: TAdvKanbanBoardItemCustomDrawMarkEvent;
    FInteraction: TAdvKanbanBoardInteraction;
    FOnCustomizeColumn: TAdvKanbanBoardCustomizeColumnEvent;
    FOnBeforeDrawItem: TAdvKanbanBoardBeforeDrawItemEvent;
    FOnAfterDrawItem: TAdvKanbanBoardAfterDrawItemEvent;
    FOnBeforeDrawItemIcon: TAdvKanbanBoardBeforeDrawItemIconEvent;
    FOnAfterDrawItemIcon: TAdvKanbanBoardAfterDrawItemIconEvent;
    FOnBeforeDropItem: TAdvKanbanBoardBeforeDropItemEvent;
    FOnAfterDropItem: TAdvKanbanBoardAfterDropItemEvent;
    FOnUpdateItemText: TAdvKanbanBoardUpdateItemTextEvent;
    FOnSelectItem: TAdvKanbanBoardSelectedItemEvent;
    FOnItemExpand: TAdvKanbanBoardItemEvent;
    FOnColumnExpand: TAdvKanbanBoardColumnEvent;
    FOnItemCollapse: TAdvKanbanBoardItemEvent;
    FOnColumnCollapse: TAdvKanbanBoardColumnEvent;
    FAdapter: TAdvKanbanBoardAdapter;
    FOnBeforeDrawItemText: TAdvKanbanBoardBeforeDrawItemTextEvent;
    FOnAfterDrawItemTitle: TAdvKanbanBoardAfterDrawItemTitleEvent;
    FOnAfterDrawItemText: TAdvKanbanBoardAfterDrawItemTextEvent;
    FOnBeforeDrawItemTitle: TAdvKanbanBoardBeforeDrawItemTitleEvent;
    FOnItemCompare: TAdvKanbanBoardItemCompareEvent;
    FOnDoneButtonClicked: TAdvKanbanBoardDoneButtonClickedEvent;
    FOnAfterApplyFilter: TAdvKanbanBoardAfterApplyFilterEvent;
    FOnBeforeApplyFilter: TAdvKanbanBoardBeforeApplyFilterEvent;
    FOnColumnAnchorClick: TAdvKanbanBoardColumnAnchorClickEvent;
    FOnItemTextAnchorClick: TAdvKanbanBoardColumnItemAnchorClickEvent;
    FOnItemTitleAnchorClick: TAdvKanbanBoardColumnItemAnchorClickEvent;
    FOnItemClick: TAdvKanbanBoardColumnItemClickEvent;
    FGlobalFont: TAdvAppearanceGlobalFont;
    FOnItemDblClick: TAdvKanbanBoardColumnItemClickEvent;
    function GetPictureContainer: TPictureContainer;
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetColumns(const Value: TAdvKanbanBoardColumns);
    procedure SetColumnsAppearance(
      const Value: TAdvKanbanBoardColumnsAppearance);
    procedure SetItemsAppearance(const Value: TAdvKanbanBoardItemsAppearance);
    procedure SetInteraction(const Value: TAdvKanbanBoardInteraction);
    procedure SetAdapter(const Value: TAdvKanbanBoardAdapter);
    function GetSelectedItem: TAdvKanbanBoardItem;
    procedure SetSelectedItem(const Value: TAdvKanbanBoardItem);
    procedure SetGlobalFont(const Value: TAdvAppearanceGlobalFont);
  protected
    function GetDocURL: string; override;
    function GetVersion: String; override;
    function GetTotalContentWidth: Double; override;
    function ColumnStretchingActive: Boolean; override;
    function CreateColumns: TAdvKanbanBoardColumns; virtual;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure ApplyStyle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HorizontalScrollPositionChanged; override;
    procedure UpdateColumns(AUpdate: Boolean = True; ARealign: Boolean = True); virtual;
    procedure UpdateControl(AUpdate: Boolean = True; ARealign: Boolean = True); override;
    procedure UpdateControlScroll({%H-}AHorizontalPos, {%H-}AVerticalPos, {%H-}ANewHorizontalPos, {%H-}ANewVerticalPos: Double); override;
    procedure DoItemCustomDrawMark(AGraphics: TAdvGraphics; ARect: TRectF; AMarkType: TAdvKanbanBoardMarkType; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoCustomizeColumn(AColumn: TAdvKanbanBoardColumn; ATableView: TAdvKanbanBoardTableView); virtual;
    procedure DoBeforeDrawItem(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawItem(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoBeforeDrawItemText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: string; var AAllow: Boolean); virtual;
    procedure DoAfterDrawItemText(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: string); virtual;
    procedure DoBeforeDrawItemIcon(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawItemIcon(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap); virtual;
    procedure DoBeforeDrawItemTitle(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; ATitle: string; var AAllow: Boolean); virtual;
    procedure DoAfterDrawItemTitle(AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; ATitle: string); virtual;
    procedure DoBeforeDropItem(AFromColumn, AToColumn: TAdvKanbanBoardColumn; AFromItem, AToItem: TAdvKanbanBoardItem; var ACanDrop: Boolean); virtual;
    procedure DoAfterDropItem(AFromColumn, AToColumn: TAdvKanbanBoardColumn; AFromItem, AToItem: TAdvKanbanBoardItem); virtual;
    procedure DoUpdateItemText(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AText: String); virtual;
    procedure DoSelectItem(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoColumnExpand(AColumn: TAdvKanbanBoardColumn); virtual;
    procedure DoColumnCollapse(AColumn: TAdvKanbanBoardColumn); virtual;
    procedure DoItemExpand(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemCollapse(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemCompare(AColumn: TAdvKanbanBoardColumn; AItem1, AItem2: TAdvKanbanBoardItem; var ACompareResult: Integer); virtual;
    procedure DoBeforeApplyFilter(AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData; AAllow: Boolean); virtual;
    procedure DoAfterApplyFilter(AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData); virtual;
    procedure DoDoneButtonClicked(AColumn: TAdvKanbanBoardColumn); virtual;
    procedure DoColumnAnchorClick(AColumn: TAdvKanbanBoardColumn; AAnchor: string); virtual;
    procedure DoItemTextAnchorClick(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AAnchor: string); virtual;
    procedure DoItemDblClick(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemClick(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemTitleAnchorClick(AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem; AAnchor: string); virtual;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure SetFonts(ASetType: TAdvAppearanceGlobalFontType); virtual;
    procedure ResetToDefaultStyle; override;
    property Version: String read GetVersion;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property Columns: TAdvKanbanBoardColumns read FColumns write SetColumns;
    property ColumnsAppearance: TAdvKanbanBoardColumnsAppearance read FColumnsAppearance write SetColumnsAppearance;
    property ItemsAppearance: TAdvKanbanBoardItemsAppearance read FItemsAppearance write SetItemsAppearance;
    property Interaction: TAdvKanbanBoardInteraction read FInteraction write SetInteraction;
    property GlobalFont: TAdvAppearanceGlobalFont read FGlobalFont write SetGlobalFont;
    property OnItemCustomDrawMark: TAdvKanbanBoardItemCustomDrawMarkEvent read FOnItemCustomDrawMark write FOnItemCustomDrawMark;
    property OnItemCompare: TAdvKanbanBoardItemCompareEvent read FOnItemCompare write FOnItemCompare;
    property OnBeforeApplyFilter: TAdvKanbanBoardBeforeApplyFilterEvent read FOnBeforeApplyFilter write FOnBeforeApplyFilter;
    property OnAfterApplyFilter: TAdvKanbanBoardAfterApplyFilterEvent read FOnAfterApplyFilter write FOnAfterApplyFilter;
    property OnDoneButtonClicked: TAdvKanbanBoardDoneButtonClickedEvent read FOnDoneButtonClicked write FOnDoneButtonClicked;
    property OnUpdateItemText: TAdvKanbanBoardUpdateItemTextEvent read FOnUpdateItemText write FOnUpdateItemText;
    property OnCustomizeColumn: TAdvKanbanBoardCustomizeColumnEvent read FOnCustomizeColumn write FOnCustomizeColumn;
    property OnBeforeDrawItem: TAdvKanbanBoardBeforeDrawItemEvent read FOnBeforeDrawItem write FOnBeforeDrawItem;
    property OnAfterDrawItem: TAdvKanbanBoardAfterDrawItemEvent read FOnAfterDrawItem write FOnAfterDrawItem;
    property OnBeforeDrawItemText: TAdvKanbanBoardBeforeDrawItemTextEvent read FOnBeforeDrawItemText write FOnBeforeDrawItemText;
    property OnAfterDrawItemText: TAdvKanbanBoardAfterDrawItemTextEvent read FOnAfterDrawItemText write FOnAfterDrawItemText;
    property OnBeforeDrawItemIcon: TAdvKanbanBoardBeforeDrawItemIconEvent read FOnBeforeDrawItemIcon write FOnBeforeDrawItemIcon;
    property OnAfterDrawItemIcon: TAdvKanbanBoardAfterDrawItemIconEvent read FOnAfterDrawItemIcon write FOnAfterDrawItemIcon;
    property OnBeforeDrawItemTitle: TAdvKanbanBoardBeforeDrawItemTitleEvent read FOnBeforeDrawItemTitle write FOnBeforeDrawItemTitle;
    property OnAfterDrawItemTitle: TAdvKanbanBoardAfterDrawItemTitleEvent read FOnAfterDrawItemTitle write FOnAfterDrawItemTitle;
    property OnBeforeDropItem: TAdvKanbanBoardBeforeDropItemEvent read FOnBeforeDropItem write FOnBeforeDropItem;
    property OnAfterDropItem: TAdvKanbanBoardAfterDropItemEvent read FOnAfterDropItem write FOnAfterDropItem;
    property OnSelectItem: TAdvKanbanBoardSelectedItemEvent read FOnSelectItem write FOnSelectItem;
    property OnColumnCollapse: TAdvKanbanBoardColumnEvent read FOnColumnCollapse write FOnColumnCollapse;
    property OnColumnExpand: TAdvKanbanBoardColumnEvent read FOnColumnExpand write FOnColumnExpand;
    property OnItemCollapse: TAdvKanbanBoardItemEvent read FOnItemCollapse write FOnItemCollapse;
    property OnItemExpand: TAdvKanbanBoardItemEvent read FOnItemExpand write FOnItemExpand;
    property Adapter: TAdvKanbanBoardAdapter read FAdapter write SetAdapter;
    property OnColumnAnchorClick: TAdvKanbanBoardColumnAnchorClickEvent read FOnColumnAnchorClick write FOnColumnAnchorClick;
    property OnItemTextAnchorClick: TAdvKanbanBoardColumnItemAnchorClickEvent read FOnItemTextAnchorClick write FOnItemTextAnchorClick;
    property OnItemTitleAnchorClick: TAdvKanbanBoardColumnItemAnchorClickEvent read FOnItemTitleAnchorClick write FOnItemTitleAnchorClick;
    property OnItemClick: TAdvKanbanBoardColumnItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TAdvKanbanBoardColumnItemClickEvent read FOnItemDblClick write FOnItemDblClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitSample; virtual;
    procedure ClearSelection;
    procedure ClearItems;
    procedure SelectItem(AItem: TAdvKanbanBoardItem); virtual;
    function XYToColumn(X, Y: Single): TAdvKanbanBoardColumn;
    function FindItemWithDBKey(ADBKey: String): TAdvKanbanBoardItem; virtual;
    property SelectedItem: TAdvKanbanBoardItem read GetSelectedItem write SetSelectedItem;
    procedure LoadSettingsFromFile(AFileName: string); override;
    procedure LoadSettingsFromStream(AStream: TStreamEx); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvKanbanBoard = class(TAdvCustomKanbanBoard)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property Adapter;
    property Version;
    property PictureContainer;
    property Stroke;
    property Fill;
    property Columns;
    property ColumnsAppearance;
    property Interaction;
    property ItemsAppearance;
    property GlobalFont;
    property OnItemCompare;
    property OnBeforeApplyFilter;
    property OnAfterApplyFilter;
    property OnDoneButtonClicked;
    property OnCustomizeColumn;
    property OnItemCustomDrawMark;
    property OnBeforeDrawItem;
    property OnAfterDrawItem;
    property OnBeforeDrawItemIcon;
    property OnAfterDrawItemIcon;
    property OnBeforeDrawItemText;
    property OnAfterDrawItemText;
    property OnBeforeDrawItemTitle;
    property OnAfterDrawItemTitle;
    property OnBeforeDropItem;
    property OnAfterDropItem;
    property OnSelectItem;
    property OnUpdateItemText;
    property OnColumnCollapse;
    property OnColumnExpand;
    property OnItemCollapse;
    property OnItemExpand;
    property OnColumnAnchorClick;
    property OnItemTextAnchorClick;
    property OnItemTitleAnchorClick;
    property OnItemClick;
    property OnItemDblClick;
  end;

implementation

uses
  Forms, Graphics, AdvGraphicsStyles, AdvImageEx, Math,
  SysUtils, AdvUtils, AdvCustomTreeView;

type
  TAdvCustomTreeViewOpen = class(TAdvCustomTreeView);
  TAdvTableViewInteractionOpen = class(TAdvTableViewInteraction);

{ TAdvCustomKanbanBoard }

procedure TAdvCustomKanbanBoard.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleAlternativeBackgroundFillColor(c) then
    ItemsAppearance.Fill.Color := c;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleLineFillColor(c) then
    ItemsAppearance.Stroke.Color := c;
end;

procedure TAdvCustomKanbanBoard.ChangeDPIScale(M, D: Integer);
var
  I, K: Integer;
  c: TAdvKanbanBoardColumn;
  it: TAdvKanbanBoardItem;
begin
  inherited;
  BeginUpdate;
  for I := 0 to Columns.Count - 1 do
  begin
    c := Columns[I];
    c.Width := TAdvUtils.MulDivSingle(c.Width, M, D);
    c.CollapsedWidth := TAdvUtils.MulDivSingle(c.CollapsedWidth, M, D);
    c.HeaderFont.Height := TAdvUtils.MulDivInt(c.HeaderFont.Height, M, D);
    c.FooterFont.Height := TAdvUtils.MulDivInt(c.FooterFont.Height, M, D);

    for K := 0 to c.Items.Count - 1 do
    begin
      it := c.Items[K];
      if it.Height > -1 then
        it.Height := TAdvUtils.MulDivSingle(it.Height, M, D);

      it.MarkSizeLeft := TAdvUtils.MulDivSingle(it.MarkSizeLeft, M, D);
      it.MarkSizeTop := TAdvUtils.MulDivSingle(it.MarkSizeTop, M, D);
      it.MarkSizeRight := TAdvUtils.MulDivSingle(it.MarkSizeRight, M, D);
      it.MarkSizeBottom := TAdvUtils.MulDivSingle(it.MarkSizeBottom, M, D);
    end;
  end;

  ColumnsAppearance.Margins.Left := TAdvUtils.MulDivSingle(ColumnsAppearance.Margins.Left, M, D);
  ColumnsAppearance.Margins.Top := TAdvUtils.MulDivSingle(ColumnsAppearance.Margins.Top, M, D);
  ColumnsAppearance.Margins.Right := TAdvUtils.MulDivSingle(ColumnsAppearance.Margins.Right, M, D);
  ColumnsAppearance.Margins.Bottom := TAdvUtils.MulDivSingle(ColumnsAppearance.Margins.Bottom, M, D);
  ColumnsAppearance.Spacing := TAdvUtils.MulDivSingle(ColumnsAppearance.Spacing, M, D);
  ColumnsAppearance.HeaderSize := TAdvUtils.MulDivSingle(ColumnsAppearance.HeaderSize, M, D);
  ColumnsAppearance.FooterSize := TAdvUtils.MulDivSingle(ColumnsAppearance.FooterSize, M, D);
  ColumnsAppearance.HeaderFont.Height := TAdvUtils.MulDivInt(ColumnsAppearance.HeaderFont.Height, M, D);
  ColumnsAppearance.FooterFont.Height := TAdvUtils.MulDivInt(ColumnsAppearance.FooterFont.Height, M, D);

  ItemsAppearance.Font.Height := TAdvUtils.MulDivInt(ItemsAppearance.Font.Height, M, D);
  ItemsAppearance.TitleFont.Height := TAdvUtils.MulDivInt(ItemsAppearance.TitleFont.Height, M, D);
  ItemsAppearance.Spacing := TAdvUtils.MulDivSingle(ItemsAppearance.Spacing, M, D);
  ItemsAppearance.Margins.Left := TAdvUtils.MulDivSingle(ItemsAppearance.Margins.Left, M, D);
  ItemsAppearance.Margins.Top := TAdvUtils.MulDivSingle(ItemsAppearance.Margins.Top, M, D);
  ItemsAppearance.Margins.Right := TAdvUtils.MulDivSingle(ItemsAppearance.Margins.Right, M, D);
  ItemsAppearance.Margins.Bottom := TAdvUtils.MulDivSingle(ItemsAppearance.Margins.Bottom, M, D);
  ItemsAppearance.FixedHeight := TAdvUtils.MulDivSingle(ItemsAppearance.FixedHeight, M, D);
  ItemsAppearance.Height := TAdvUtils.MulDivSingle(ItemsAppearance.Height, M, D);

  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    Columns[I].Items.Clear;
end;

procedure TAdvCustomKanbanBoard.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    if Assigned(Columns[I].FTableView) then
      Columns[I].FTableView.ClearSelection;
  end;
end;

function TAdvCustomKanbanBoard.ColumnStretchingActive: Boolean;
begin
  Result := False;
end;

constructor TAdvCustomKanbanBoard.Create(AOwner: TComponent);
begin
  inherited;
  FFirstLoad := True;
  FColumns := CreateColumns;
  FColumnsAppearance := TAdvKanbanBoardColumnsAppearance.Create(Self);
  FItemsAppearance := TAdvKanbanBoardItemsAppearance.Create(Self);
  FInteraction := TAdvKanbanBoardInteraction.Create(Self);
  FGlobalFont := TAdvAppearanceGlobalFont.Create(Self);
  Width := 810;
  Height := 450;
  if IsDesignTime then
    InitSample;
end;

function TAdvCustomKanbanBoard.CreateColumns: TAdvKanbanBoardColumns;
begin
  Result := TAdvKanbanBoardColumns.Create(Self);
end;

destructor TAdvCustomKanbanBoard.Destroy;
begin
  FGlobalFont.Free;
  FInteraction.Free;
  FItemsAppearance.Free;
  FColumnsAppearance.Free;
  FColumns.Free;
  inherited;
end;

procedure TAdvCustomKanbanBoard.DoAfterApplyFilter(
  AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData);
begin
  if Assigned(OnAfterApplyFilter) then
    OnAfterApplyFilter(Self, AColumn, AFilter);
end;

procedure TAdvCustomKanbanBoard.DoAfterDrawItem(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnAfterDrawItem) then
    OnAfterDrawItem(Self, AGraphics, ARect, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoAfterDrawItemIcon(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap);
begin
  if Assigned(OnAfterDrawItemIcon) then
    OnAfterDrawItemIcon(Self, AGraphics, ARect, AColumn, AItem, AIcon);
end;

procedure TAdvCustomKanbanBoard.DoAfterDrawItemText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; AText: string);
begin
  if Assigned(OnAfterDrawItemText) then
    OnAfterDrawItemText(Self, AGraphics, ARect, AColumn, AItem, AText);
end;

procedure TAdvCustomKanbanBoard.DoAfterDrawItemTitle(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; ATitle: string);
begin
  if Assigned(OnAfterDrawItemTitle) then
    OnAfterDrawItemTitle(Self, AGraphics, ARect, AColumn, AItem, ATitle);
end;

procedure TAdvCustomKanbanBoard.DoAfterDropItem(AFromColumn,
  AToColumn: TAdvKanbanBoardColumn; AFromItem,
  AToItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnAfterDropItem) then
    OnAfterDropItem(Self, AFromColumn, AToColumn, AFromItem, AToItem);

  if Assigned(Adapter) then
    Adapter.UpdateItem(AToItem);
end;

procedure TAdvCustomKanbanBoard.DoBeforeApplyFilter(
  AColumn: TAdvKanbanBoardColumn; AFilter: TAdvTableViewFilterData;
  AAllow: Boolean);
begin
  if Assigned(OnBeforeApplyFilter) then
    OnBeforeApplyFilter(Self, AColumn, AFilter, AAllow);
end;

procedure TAdvCustomKanbanBoard.DoBeforeDrawItem(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; var AAllow, ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawItem) then
    OnBeforeDrawItem(Self, AGraphics, ARect, AColumn, AItem, AAllow, ADefaultDraw);
end;

procedure TAdvCustomKanbanBoard.DoBeforeDrawItemIcon(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; AIcon: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemIcon) then
    OnBeforeDrawItemIcon(Self, AGraphics, ARect, AColumn, AItem, AIcon, AAllow);
end;

procedure TAdvCustomKanbanBoard.DoBeforeDrawItemText(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; AText: string; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemText) then
    OnBeforeDrawItemText(Self, AGraphics, ARect, AColumn, AItem, AText, AAllow);
end;

procedure TAdvCustomKanbanBoard.DoBeforeDrawItemTitle(AGraphics: TAdvGraphics;
  ARect: TRectF; AColumn: TAdvKanbanBoardColumn;
  AItem: TAdvKanbanBoardItem; ATitle: string; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemTitle) then
    OnBeforeDrawItemTitle(Self, AGraphics, ARect, AColumn, AItem, ATitle, AAllow);
end;

procedure TAdvCustomKanbanBoard.DoBeforeDropItem(AFromColumn, AToColumn: TAdvKanbanBoardColumn; AFromItem,
  AToItem: TAdvKanbanBoardItem; var ACanDrop: Boolean);
begin
  if Assigned(OnBeforeDropItem) then
    OnBeforeDropItem(Self, AFromColumn, AToColumn, AFromItem, AToItem, ACanDrop);
end;

procedure TAdvCustomKanbanBoard.DoColumnCollapse(
  AColumn: TAdvKanbanBoardColumn);
begin
  if Assigned(OnColumnCollapse) then
    OnColumnCollapse(Self, AColumn);
end;

procedure TAdvCustomKanbanBoard.DoColumnExpand(
  AColumn: TAdvKanbanBoardColumn);
begin
  if Assigned(OnColumnExpand) then
    OnColumnExpand(Self, AColumn);
end;

procedure TAdvCustomKanbanBoard.DoColumnAnchorClick(
  AColumn: TAdvKanbanBoardColumn; AAnchor: string);
begin
  if Assigned(OnColumnAnchorClick) then
    OnColumnAnchorClick(Self, AColumn, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomKanbanBoard.DoCustomizeColumn(
  AColumn: TAdvKanbanBoardColumn;
  ATableView: TAdvKanbanBoardTableView);
begin
  if Assigned(OnCustomizeColumn) then
    OnCustomizeColumn(Self, AColumn, ATableView);
end;

procedure TAdvCustomKanbanBoard.DoDoneButtonClicked(
  AColumn: TAdvKanbanBoardColumn);
begin
  if Assigned(OnDoneButtonClicked) then
    OnDoneButtonClicked(Self, AColumn);
end;

procedure TAdvCustomKanbanBoard.DoItemCollapse(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemCollapse) then
    OnItemCollapse(Self, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoItemCompare(
  AColumn: TAdvKanbanBoardColumn; AItem1, AItem2: TAdvKanbanBoardItem;
  var ACompareResult: Integer);
begin
  if Assigned(OnItemCompare) then
    OnItemCompare(Self, AColumn, AItem1, AItem2, ACompareResult);
end;

procedure TAdvCustomKanbanBoard.DoItemCustomDrawMark(
  AGraphics: TAdvGraphics; ARect: TRectF; AMarkType: TAdvKanbanBoardMarkType; AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemCustomDrawMark) then
    OnItemCustomDrawMark(Self, AGraphics, ARect, AMarkType, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoItemExpand(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemExpand) then
    OnItemExpand(Self, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoItemTextAnchorClick(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem;
  AAnchor: string);
begin
  if Assigned(OnItemTextAnchorClick) then
    OnItemTextAnchorClick(Self, AColumn, AItem, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenUrl(AAnchor);
end;

procedure TAdvCustomKanbanBoard.DoItemDblClick(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemDblClick) then
    OnItemDblClick(Self, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoItemClick(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, AColumn, AItem);
end;

procedure TAdvCustomKanbanBoard.DoItemTitleAnchorClick(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem;
  AAnchor: string);
begin
  if Assigned(OnItemTitleAnchorClick) then
    OnItemTitleAnchorClick(Self, AColumn, AItem, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenUrl(AAnchor);
end;

procedure TAdvCustomKanbanBoard.DoSelectItem(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnSelectItem) then
    OnSelectItem(Self, AColumn, AItem);

  if Assigned(Adapter) then
    Adapter.SelectItem(AItem);
end;

procedure TAdvCustomKanbanBoard.DoUpdateItemText(
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem;
  AText: String);
begin
  if Assigned(OnUpdateItemText) then
    OnUpdateItemText(Self, AColumn, AItem, AText);

  if Assigned(Adapter) then
    Adapter.UpdateItem(AItem);
end;

procedure TAdvCustomKanbanBoard.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  inherited;
  {$IFNDEF WEBLIB}
  if FFirstLoad then
  begin
    UpdateColumns(True, True);
    FFirstLoad := False;
  end;
  {$ENDIF}
end;

function TAdvCustomKanbanBoard.FindItemWithDBKey(
  ADBKey: String): TAdvKanbanBoardItem;
var
  I: Integer;
  k: Integer;
begin
  Result := nil;
  for I := 0 to Columns.Count - 1 do
  begin
    for k := 0 to Columns[I].Items.Count - 1 do
    begin
      if Columns[I].Items[K].DBKey = ADBKey then
      begin
        Result := Columns[I].Items[K];
        Break;
      end;
    end;
  end;
end;

function TAdvCustomKanbanBoard.GetPictureContainer: TPictureContainer;
begin
  Result := FPictureContainer;
end;

function TAdvCustomKanbanBoard.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/' + LowerCase(ClassName);
end;

function TAdvCustomKanbanBoard.GetSelectedItem: TAdvKanbanBoardItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FColumns.Count - 1 do
  begin
    Result := FColumns[I].SelectedItem;
    if Assigned(Result) then
      Break;
  end;
end;

function TAdvCustomKanbanBoard.GetTotalContentWidth: Double;
var
  I: Integer;
  tv: TAdvKanbanBoardTableView;
begin
  Result := inherited GetTotalContentWidth;

  for I := ControlCount - 1 downto 0 do
  begin
    if Controls[I] is TAdvKanbanBoardTableView then
    begin
      tv := Controls[I] as TAdvKanbanBoardTableView;
      Result := tv.Width + tv.Left + ColumnsAppearance.Margins.Right + GetHorizontalScrollPosition;
      Break;
    end;
  end;
end;

function TAdvCustomKanbanBoard.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomKanbanBoard.HorizontalScrollPositionChanged;
var
  I: Integer;
  x: Double;
  tv: TAdvKanbanBoardTableView;
begin
  inherited;
  x := 0;
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TAdvKanbanBoardTableView then
    begin
      tv := Controls[I] as TAdvKanbanBoardTableView;
      tv.Left := Round(x - GetHorizontalScrollPosition + ColumnsAppearance.Margins.Left);
      x := x + tv.Width + ColumnsAppearance.Spacing;
    end;
  end;
end;

procedure TAdvCustomKanbanBoard.InitSample;
var
  c: TAdvKanbanBoardColumn;
  it: TAdvKanbanBoardItem;
begin
  FColumns.Clear;
  BeginUpdate;

  ResetToDefaultStyle;

  Interaction.DragDropMode := kbdmMove;

  c := Columns.Add;
  c.HeaderText := 'Backlog';
  c.HeaderHorizontalTextAlign := gtaLeading;
  c.Expandable := True;

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'QA';
  it.Text := 'Test performance of the new feature.<br><i>And with remote connection.</i>';
  {$IFDEF FMXLIB}
  it.Color := $FFFFFFFE;
  it.SelectedColor := $7F5A81E6;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.MarkColor := $FF5A81E6;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $FEFFFF;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.MarkColor := $E6815A;
  it.SelectedColor := $F0BCA8;
  {$ENDIF}
  it.SelectedStrokeColor := it.MarkColor;
  it.MarkType := [kbmtTop];
  it.MarkSizeTop := ScalePaintValue(8);
  it.MarkRounding := ScalePaintValue(3);
  it.MarkCorners := [gcTopLeft, gcTopRight];
  it.Rounding := ScalePaintValue(3);

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'Documentation';
  it.Text := 'Write the developers guide and <a href="">FAQ</a>';

  {$IFDEF FMXLIB}
  it.Color := $FFFFFFFE;
  it.SelectedColor := $7FB0EDA8;
  it.SelectedStrokeColor := $FFB0EDA8;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.MarkColor := $FFB0EDA8;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $FEFFFF;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.MarkColor := $A8EDB0;
  it.SelectedColor := $D2F2D3;
  {$ENDIF}
  it.SelectedStrokeColor := it.MarkColor;
  it.MarkType := [kbmtTop];
  it.MarkSizeTop := ScalePaintValue(8);
  it.MarkRounding := ScalePaintValue(3);
  it.MarkCorners := [gcTopLeft, gcTopRight];
  it.Rounding := ScalePaintValue(3);

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'Meeting: Next Phase';
  it.Text := 'What will we implement in the next release';
  {$IFDEF FMXLIB}
  it.Color := $FFFFFFFE;
  it.SelectedColor := $7FB0EDA8;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.MarkColor := $FFB0EDA8;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $FEFFFF;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.MarkColor := $A8EDB0;
  it.SelectedColor := $D2F2D3;
  {$ENDIF}
  it.SelectedStrokeColor := it.MarkColor;
  it.Expandable := True;
  it.Expanded := False;
  it.MarkType := [kbmtTop];
  it.MarkSizeTop := ScalePaintValue(8);
  it.MarkRounding := ScalePaintValue(3);
  it.MarkCorners := [gcTopLeft, gcTopRight];
  it.Rounding := ScalePaintValue(3);

  c := Columns.Add;
  c.HeaderText := 'In Progress';
  c.HeaderHorizontalTextAlign := gtaLeading;
  c.Expandable := True;

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'Bugfix 376';
  it.Text := 'Fix <u>high priority</u> issues documented by customer.';
  {$IFDEF FMXLIB}
  it.Color := $FFFFFFFE;
  it.SelectedColor := $7FEE4353;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.MarkColor := $FFEE4353;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $FEFFFF;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.MarkColor := $5343EE;
  it.SelectedColor := $A79DF1;
  {$ENDIF}
  it.SelectedStrokeColor := it.MarkColor;
  it.MarkType := [kbmtTop];
  it.MarkSizeTop := ScalePaintValue(8);
  it.MarkRounding := ScalePaintValue(3);
  it.MarkCorners := [gcTopLeft, gcTopRight];
  it.Rounding := ScalePaintValue(3);

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'Update Customer';
  it.Text := 'Get in contact with the customer to let him know how far we are on developing the newly requested items.';
  {$IFDEF FMXLIB}
  it.Color := $FFFFFFFE;
  it.SelectedColor := $7FF0CA35;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.MarkColor := $FFF0CA35;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $FEFFFF;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.MarkColor := $35CAF0;
  it.SelectedColor := $98E1F2;
  {$ENDIF}
  it.SelectedStrokeColor := it.MarkColor;
  it.MarkType := [kbmtTop];
  it.MarkSizeTop := ScalePaintValue(8);
  it.MarkRounding := ScalePaintValue(3);
  it.MarkCorners := [gcTopLeft, gcTopRight];
  it.Rounding := ScalePaintValue(3);

  c := Columns.Add;
  c.HeaderText := 'Complete';
  c.HeaderHorizontalTextAlign := gtaLeading;

  it := c.Items.Add;
  it.UseDefaultAppearance := False;
  it.Title := 'Install New Server';
  it.Text := 'Name: <b>Serv2</b><br>IP: <b>192.168.124.12</b>';
  {$IFDEF FMXLIB}
  it.Color := $FFF0FAF2;
  it.TitleColor := $FF454545;
  it.TextColor := $FF7A7A7A;
  it.StrokeColor := $FFB0EDA8;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  it.Color := $F2FAF0;
  it.TitleColor := $454545;
  it.TextColor := $7A7A7A;
  it.StrokeColor := $A8EDB0;
  {$ENDIF}
  it.SelectedStrokeColor := it.StrokeColor;
  it.SelectedColor := it.StrokeColor;
  it.Rounding := ScalePaintValue(3);

  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.LoadSettingsFromFile(AFileName: string);
var
  I, K: Integer;
begin
  BeginUpdate;
  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].UseDefaultAppearance := True;
    for K := 0 to Columns[I].Items.Count - 1 do
      Columns[I].Items[K].UseDefaultAppearance := True;
  end;
  inherited;

  for I := 0 to Columns.Count - 1 do
  begin
    for k := 0 to Columns[I].Items.Count - 1 do
    begin
      Columns[i].Items[k].TextColor := ItemsAppearance.Font.Color;
      Columns[i].Items[k].TitleColor := ItemsAppearance.TitleFont.Color;
    end;
  end;

  UpdateColumns(True, True);
  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.LoadSettingsFromStream(AStream: TStreamEx);
var
  I, K: Integer;
begin
  BeginUpdate;
  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].UseDefaultAppearance := True;
    for K := 0 to Columns[I].Items.Count - 1 do
      Columns[I].Items[K].UseDefaultAppearance := True;
  end;
  inherited;

  for I := 0 to Columns.Count - 1 do
  begin
    for k := 0 to Columns[I].Items.Count - 1 do
    begin
      Columns[i].Items[k].TextColor := ItemsAppearance.Font.Color;
      Columns[i].Items[k].TitleColor := ItemsAppearance.TitleFont.Color;
    end;
  end;

  UpdateColumns(True, True);
  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AComponent = FAdapter) and (Operation = opRemove) then
    FAdapter := nil;
end;

procedure TAdvCustomKanbanBoard.ResetToDefaultStyle;
begin
  inherited;
  BeginUpdate;

  {$IFDEF FMXLIB}
  Color := $FFEEF2F9;
  ColumnsAppearance.Fill.Color := $FFF6F8FC;
  ColumnsAppearance.HeaderFill.Color := $FFEEF2F9;
  ColumnsAppearance.HeaderFill.ColorTo := $FFF0F3FA;
  ColumnsAppearance.HeaderStroke.Color := $FCF8F6;
  ColumnsAppearance.HeaderFont.Color := $FF454545;
  ColumnsAppearance.FooterFill.Color := $FFF6F8FC;
  ColumnsAppearance.FooterStroke.Color := $FCF8F6;
  ColumnsAppearance.Stroke.Color := $FFEEF2F9;

  ItemsAppearance.Fill.Color := $FFFFFFFE;
  ItemsAppearance.TitleFont.Color := $FF454545;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Color := $F9F2EE;
  ColumnsAppearance.Fill.Color := $FCF8F6;
  ColumnsAppearance.HeaderFill.Color := $F9F2EE;
  ColumnsAppearance.HeaderFill.ColorTo := $FAF3F0;
  ColumnsAppearance.HeaderStroke.Color := $FCF8F6;
  ColumnsAppearance.HeaderFont.Color := $454545;
  ColumnsAppearance.FooterFill.Color := $FCF8F6;
  ColumnsAppearance.FooterStroke.Color := $FCF8F6;
  ColumnsAppearance.Stroke.Color := $F9F2EE;

  ItemsAppearance.Fill.Color := $FEFFFF;
  ItemsAppearance.TitleFont.Color := $454545;
  {$ENDIF}

  ColumnsAppearance.Fill.Kind := gfkSolid;
  ColumnsAppearance.HeaderFill.Kind := gfkGradient;
  ColumnsAppearance.FooterFill.Kind := gfkSolid;
  ColumnsAppearance.Stroke.Kind := gskSolid;
  ColumnsAppearance.HeaderStroke.Kind := gskNone;
  ColumnsAppearance.FooterStroke.Kind := gskNone;

  ItemsAppearance.Fill.Kind := gfkSolid;

  TAdvUtils.SetFontSize(ColumnsAppearance.HeaderFont, 14, PaintScaleFactor);
  ColumnsAppearance.HeaderFont.Style := [TFontStyle.fsBold];
  TAdvUtils.SetFontSize(ItemsAppearance.TitleFont, 14, PaintScaleFactor);

  ItemsAppearance.ShowFocus := False;

  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.SelectItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(AItem) and Assigned(AItem.Column) then
    AItem.Column.SelectItem(AItem.Index);
end;

procedure TAdvCustomKanbanBoard.SetAdapter(
  const Value: TAdvKanbanBoardAdapter);
begin
  if Assigned(Value) then
    Value.KanbanBoard := Self;
  FAdapter := Value;
end;

procedure TAdvCustomKanbanBoard.SetPictureContainer(
  const Value: TPictureContainer);
begin
  if FPictureContainer <> Value then
  begin
    FPictureContainer := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomKanbanBoard.SetColumns(
  const Value: TAdvKanbanBoardColumns);
begin
  FColumns.Assign(Value);
end;

procedure TAdvCustomKanbanBoard.SetColumnsAppearance(
  const Value: TAdvKanbanBoardColumnsAppearance);
begin
  FColumnsAppearance.Assign(Value);
end;

procedure TAdvCustomKanbanBoard.SetFonts(ASetType: TAdvAppearanceGlobalFontType);
var
  I: Integer;
  J: Integer;
begin
  BeginUpdate;

  GlobalFont.ApplyChange(ColumnsAppearance.HeaderFont, ASettype);
  GlobalFont.ApplyChange(ColumnsAppearance.FooterFont, ASettype);

  GlobalFont.ApplyChange(ItemsAppearance.TitleFont, ASetType);
  GlobalFont.ApplyChange(ItemsAppearance.Font, ASetType);

  for I := 0 to Columns.Count - 1 do
  begin
    GlobalFont.ApplyChange(Columns[I].HeaderFont, ASetType);
    GlobalFont.ApplyChange(Columns[I].FooterFont, ASetType);
    GlobalFont.ApplyChange(Columns[I].FFont, ASetType);

    if ASetType = aftColor then
    begin
      for J := 0 to Columns[I].Items.Count -1 do
      begin
        Columns[I].Items[J].TextColor := GlobalFont.Color;
        Columns[I].Items[J].SelectedTextColor := GlobalFont.Color;
        Columns[I].Items[J].TitleColor := GlobalFont.Color;
        Columns[I].Items[J].SelectedTitleColor := GlobalFont.Color;
      end;
    end;
  end;

  EndUpdate;
end;

procedure TAdvCustomKanbanBoard.SetGlobalFont(
  const Value: TAdvAppearanceGlobalFont);
begin
  FGlobalFont.Assign(Value);
end;

procedure TAdvCustomKanbanBoard.SetInteraction(
  const Value: TAdvKanbanBoardInteraction);
begin
  FInteraction.Assign(Value);
end;

procedure TAdvCustomKanbanBoard.SetItemsAppearance(
  const Value: TAdvKanbanBoardItemsAppearance);
begin
  FItemsAppearance.Assign(Value);
end;

procedure TAdvCustomKanbanBoard.SetSelectedItem(
  const Value: TAdvKanbanBoardItem);
begin
  SelectItem(Value);
end;

procedure TAdvCustomKanbanBoard.UpdateColumns(AUpdate: Boolean = True; ARealign: Boolean = True);
var
  I: Integer;
  c: TAdvKanbanBoardColumn;
  tv: TAdvKanbanBoardTableView;
  tvi: TAdvTableViewItem;
  ci: TAdvKanbanBoardItem;
  J: Integer;
  x: Double;
  cr: TRectF;
  k, tr: Integer;
begin
  if (UpdateCount > 0) or IsDestroying then
    Exit;

  x := -GetHorizontalScrollPosition;
  cr := GetContentRect;
  for I := 0 to Columns.Count - 1 do
  begin
    c := Columns[I];
    tv := c.TableView;
    if Assigned(tv) then
    begin
      tr := tv.TreeView.TopRow;

      if c.Visible then
        tv.Parent := Self
      else
        tv.Parent := nil;

      tv.BeginUpdate;
      tv.PictureContainer := PictureContainer;
      tv.Left := Round(x + ColumnsAppearance.Margins.Left);
      tv.Top := Round(cr.Top + ColumnsAppearance.Margins.Top);
      tv.Height := Round(cr.Bottom - cr.Top - ColumnsAppearance.Margins.Bottom - ColumnsAppearance.Margins.Top);
      if c.Expanded then
        tv.Width := Round(c.Width)
      else
        tv.Width := Round(c.CollapsedWidth);

      tv.TreeView.CompactMode := not c.Expanded;

      if AUpdate then
      begin
        tv.Header.Text := c.HeaderText;
        tv.Footer.Text := c.FooterText;
        tv.Header.Visible := c.HeaderVisible;
        tv.Footer.Visible := c.FooterVisible;
        tv.Header.Height := ColumnsAppearance.HeaderSize;
        tv.Footer.Height := ColumnsAppearance.FooterSize;

        if c.UseDefaultAppearance then
        begin
          tv.Fill.Assign(ColumnsAppearance.Fill);
          tv.Stroke.Assign(ColumnsAppearance.Stroke);
          tv.Header.Fill.Assign(ColumnsAppearance.HeaderFill);
          tv.Header.Stroke.Assign(ColumnsAppearance.HeaderStroke);
          tv.Header.Font.Assign(ColumnsAppearance.HeaderFont);
          tv.Footer.Fill.Assign(ColumnsAppearance.FooterFill);
          tv.Footer.Stroke.Assign(ColumnsAppearance.FooterStroke);
          tv.Footer.Font.Assign(ColumnsAppearance.FooterFont);
        end
        else
        begin
          tv.Fill.Assign(c.Fill);
          tv.Stroke.Assign(c.Stroke);
          tv.Header.Fill.Assign(c.HeaderFill);
          tv.Header.Stroke.Assign(c.HeaderStroke);
          tv.Header.Font.Assign(c.HeaderFont);
          tv.Footer.Fill.Assign(c.FooterFill);
          tv.Footer.Stroke.Assign(c.FooterStroke);
          tv.Footer.Font.Assign(c.FooterFont);
        end;

        tv.Header.HorizontalTextAlign := c.HeaderHorizontalTextAlign;
        tv.Header.VerticalTextAlign := c.HeaderVerticalTextAlign;
        tv.Header.WordWrapping := c.HeaderWordWrapping;
        tv.Header.Trimming := c.HeaderTrimming;

        tv.Items.Clear;
        for J := 0 to c.Items.Count - 1 do
        begin
          ci := c.Items[J];
          tvi := tv.Items.Add;
          tvi.AccessoryWidth := 0;
          tvi.AccessoryHeight := 0;
          tvi.Title := ci.Title;
          tvi.TitleExpanded := ci.Expanded;
          tvi.Text := ci.Text;
          tvi.Checked := ci.Checked;
          tvi.HTMLTemplateItems.Assign(ci.HTMLTemplateItems);
          tvi.Bitmap.Assign(ci.Bitmap);
          tvi.BitmapName := ci.BitmapName;
          tvi.WordWrapping := ci.WordWrapping;
          tvi.Trimming := ci.Trimming;
          tvi.HorizontalTextAlign := ci.HorizontalTextAlign;
          tvi.VerticalTextAlign := ci.VerticalTextAlign;
          tvi.TextColor := ci.TextColor;
          tvi.SelectedTextColor := ci.SelectedTextColor;
          tvi.DisabledTextColor := ci.DisabledTextColor;
          tvi.TitleWordWrapping := ci.TitleWordWrapping;
          tvi.TitleTrimming := ci.TitleTrimming;
          tvi.TitleHorizontalTextAlign := ci.TitleHorizontalTextAlign;
          tvi.TitleVerticalTextAlign := ci.TitleVerticalTextAlign;
          tvi.TitleColor := ci.TitleColor;
          tvi.SelectedTitleColor := ci.SelectedTitleColor;
          tvi.DisabledTitleColor := ci.DisabledTitleColor;
          tvi.Enabled := ci.Enabled;
          tvi.Height := ci.Height;
          tvi.Visible := ci.Visible;
          tvi.Tag := ci.Tag;
          k := Integer(ci.CheckType);
          tvi.CheckType := TAdvTableViewItemCheckType(k);
        end;
      end;

      tv.ItemAppearance.Font.Assign(ItemsAppearance.Font);
      tv.ItemAppearance.Fill.Assign(ItemsAppearance.Fill);
      tv.ItemAppearance.Stroke.Assign(ItemsAppearance.Stroke);
      tv.ItemAppearance.SelectedStroke.Assign(ItemsAppearance.SelectedStroke);
      tv.ItemAppearance.SelectedFill.Assign(ItemsAppearance.SelectedFill);
      tv.ItemAppearance.DisabledStroke.Assign(ItemsAppearance.DisabledStroke);
      tv.ItemAppearance.DisabledFill.Assign(ItemsAppearance.DisabledFill);
      k := Integer(ItemsAppearance.HeightMode);
      tv.ItemAppearance.HeightMode := TAdvTableViewItemHeightMode(k);
      tv.ItemAppearance.FixedHeight := ItemsAppearance.FixedHeight;
      tv.ItemAppearance.Height := ItemsAppearance.Height;
      tv.ItemAppearance.HTMLTemplate := ItemsAppearance.HTMLTemplate;
      tv.ItemAppearance.ShowFocus := ItemsAppearance.ShowFocus;
      tv.TreeView.NodesAppearance.TitleFont.Assign(ItemsAppearance.TitleFont);
      tv.TreeView.NodesAppearance.Spacing := ItemsAppearance.Spacing;
      tv.TreeView.NodesAppearance.SelectionArea := tsaDefault;

      tv.Interaction.MultiSelect := Interaction.MultiSelect;
      tv.Interaction.AutoOpenURL := Interaction.AutoOpenURL;
      tv.Interaction.TouchScrolling := Interaction.TouchScrolling;
      k := Integer(Interaction.DragDropMode);
      tv.Interaction.DragDropMode := TAdvTableViewDragDropMode(k);
      tv.Interaction.SwipeBounceGesture := Interaction.SwipeBounceGesture;
      tv.Interaction.LongPressToEdit := False;
      k := Integer(Interaction.MouseEditMode);
      tv.TreeView.Interaction.MouseEditMode := TAdvTreeViewMouseEditMode(k);
      tv.TreeView.Interaction.KeyboardEdit := Interaction.KeyboardEdit;
      if Interaction.Editing then
        tv.TreeView.Columns[0].EditorType := tcetMemo
      else
        tv.TreeView.Columns[0].EditorType := tcetNone;

      tv.VerticalScrollBarVisible := True;
      tv.TreeView.StretchScrollBars := False;
      tv.TreeView.Columns[0].ExpandingButtonSize := ScalePaintValue(20);
      tv.TreeView.Columns[0].Expandable := c.Expandable;
      tv.TreeView.Columns[0].Expanded := c.Expanded;

      tv.EndUpdate;
      tv.AdaptToStyle := AdaptToStyle;

      tv.FOriginalBoundsRect := RectF(tv.Left + GetHorizontalScrollPosition, tv.Top, tv.Left + tv.Width + GetHorizontalScrollPosition, tv.Top + tv.Height);

      if c.Visible and Assigned(tv.Parent) then
        x := x + tv.Width + ColumnsAppearance.Spacing;

      tv.TreeView.TopRow := tr;

      DoCustomizeColumn(c, tv);
    end;
  end;

  if ARealign then
  begin
    UpdateControlScrollBars(False, False);
    UpdateControl(False, False);
  end
  else
    Invalidate;
end;

procedure TAdvCustomKanbanBoard.UpdateControl(AUpdate: Boolean = True; ARealign: Boolean = True);
begin
  UpdateColumns(AUpdate, ARealign);
  inherited;
end;

procedure TAdvCustomKanbanBoard.UpdateControlScroll(AHorizontalPos,
  AVerticalPos, ANewHorizontalPos, ANewVerticalPos: Double);
begin
  inherited;
  UpdateColumns(False);
end;

function TAdvCustomKanbanBoard.XYToColumn(X, Y: Single): TAdvKanbanBoardColumn;
var
  I: Integer;
  c: TAdvKanbanBoardColumn;
begin
  Result := nil;
  for I := 0 to Columns.Count - 1 do
  begin
    c := Columns[I];
    if c.Visible and Assigned(c.TableView) and Assigned(c.TableView.Parent) then
    begin
      if PtInRectEx(ConvertToRectF(c.TableView.BoundsRect), PointF(X, Y)) then
      begin
        Result := c;
        Break;
      end;
    end;
  end;
end;

{ TAdvKanbanBoard }

procedure TAdvKanbanBoard.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvKanbanBoard);
end;

{ TAdvKanbanBoardTableView }

function TAdvKanbanBoardTableView.CreateTreeView: TAdvTreeViewTableView;
begin
  Result := TAdvKanbanBoardTreeViewTableView.Create(Self);
end;

procedure TAdvKanbanBoardTableView.CustomizeButtons;
begin
  inherited;
  BackButton.Height := 20;
  DoneButton.Height := 20;
  EditButton.Height := 20;
  FilterButton.Height := 20;
end;

procedure TAdvKanbanBoardTableView.DoAfterApplyFilter(
  AFilter: TAdvTableViewFilterData);
begin
  inherited;
  if Assigned(FKanbanBoard) then
    FKanbanBoard.DoAfterApplyFilter(FColumn, AFilter);
end;

procedure TAdvKanbanBoardTableView.DoAfterDropItem(AFromItem,
  AToItem: TAdvTableViewItem);
begin
  inherited;
  if Assigned(FKanbanBoard) then
    FKanbanBoard.UpdateColumns;
end;

procedure TAdvKanbanBoardTableView.DoBeforeApplyFilter(
  AFilter: TAdvTableViewFilterData; AAllow: Boolean);
begin
  inherited;
  if Assigned(FKanbanBoard) then
    FKanbanBoard.DoBeforeApplyFilter(FColumn, AFilter, AAllow);
end;

procedure TAdvKanbanBoardTableView.DoColumnSort(Sender: TObject;
  AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode);
var
  i: Integer;
begin
  if Assigned(FColumn) then
  begin
    i := Integer(ASortMode);
    FColumn.Items.Sort(FColumn.Sorting in [kbsNormalCaseSensitive], TAdvKanbanBoardItemsSortMode(i));
  end;
end;

procedure TAdvKanbanBoardTableView.DoDoneButtonClicked(Sender: TObject);
begin
  inherited;
  if Assigned(FKanbanBoard) then
    FKanbanBoard.DoDoneButtonClicked(FColumn);
end;

procedure TAdvKanbanBoardTableView.DragItemAdd(ANewItem, AAssignItem, AInsertItem: TAdvTableViewItem);
var
  li, lii: TAdvKanbanBoardItem;
  tv: TAdvKanbanBoardTableView;
begin
  if Assigned(FColumn) and Assigned(ANewItem) and Assigned(ANewItem.TableView) then
  begin
    li := nil;
    if ANewItem.TableView is TAdvKanbanBoardTableView then
    begin
      tv := ANewItem.TableView as TAdvKanbanBoardTableView;
      if Assigned(tv.FColumn) then
      begin
        tv.FColumn.FBlockUpdate := True;
        li := tv.FColumn.Items.Add;
      end;
    end;

    if Assigned(AAssignItem) and Assigned(li) and Assigned(AAssignItem.TableView) then
    begin
      if AAssignItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AAssignItem.TableView as TAdvKanbanBoardTableView;
        if Assigned(tv.FColumn) then
        begin
          tv.FColumn.FBlockUpdate := True;
          li.Assign(tv.FColumn.FindItem(AAssignItem.Index));
          tv.FColumn.FBlockUpdate := False;
        end;
      end;
    end;

    if Assigned(AInsertItem) and Assigned(li) and Assigned(AInsertItem.TableView) then
    begin
      if AInsertItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AInsertItem.TableView as TAdvKanbanBoardTableView;
        if Assigned(tv.FColumn) then
        begin
          tv.FColumn.FBlockUpdate := True;
          lii := tv.FColumn.FindItem(AInsertItem.Index);
          if Assigned(lii) then
            li.Index := lii.Index;
          tv.FColumn.FBlockUpdate := False;
        end;
      end;
    end;

    if ANewItem.TableView is TAdvKanbanBoardTableView then
    begin
      tv := ANewItem.TableView as TAdvKanbanBoardTableView;
      tv.FColumn.FBlockUpdate := False;
    end;
  end;
end;

procedure TAdvKanbanBoardTableView.DragItemDelete(AItem: TAdvTableViewItem);
var
  li: TAdvKanbanBoardItem;
  tv: TAdvKanbanBoardTableView;
begin
  if Assigned(AItem) and Assigned(AItem.TableView) then
  begin
    if AItem.TableView is TAdvKanbanBoardTableView then
    begin
      tv := AItem.TableView as TAdvKanbanBoardTableView;
      if Assigned(tv.FColumn) then
      begin
        tv.FColumn.FBlockUpdate := True;
        li := tv.FColumn.FindItem(AItem.Index);
        if Assigned(li) then
          tv.FColumn.Items.Delete(li.Index);
        tv.FColumn.FBlockUpdate := False;
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardTableView.DragItemMove(AFromItem, AToItem: TAdvTableViewItem);
var
  tvf, tvt: TAdvKanbanBoardTableView;
  lif, lit: TAdvKanbanBoardItem;
begin
  if Assigned(AFromItem) and Assigned(AToItem) and Assigned(AFromItem.TableView) and Assigned(AToItem.TableView) then
  begin
    if AFromItem.TableView is TAdvKanbanBoardTableView then
    begin
      tvf := AFromItem.TableView as TAdvKanbanBoardTableView;
      tvt := AToItem.TableView as TAdvKanbanBoardTableView;
      if Assigned(tvf.FColumn) and Assigned(tvt.FColumn) then
      begin
        tvf.FColumn.FBlockUpdate := True;
        tvt.FColumn.FBlockUpdate := True;
        lif := tvf.FColumn.FindItem(AFromItem.Index);
        lit := tvt.FColumn.FindItem(AToItem.Index);
        if Assigned(lif) and Assigned(lit) then
          lif.Index := lit.Index;
        tvf.FColumn.FBlockUpdate := False;
        tvt.FColumn.FBlockUpdate := False;
      end;
    end;
  end;
end;

{ TAdvKanbanBoardTreeViewTableView }

function TAdvKanbanBoardTreeViewTableView.AlternativeDragDrop: Boolean;
begin
  Result := True;
end;

function TAdvKanbanBoardTreeViewTableView.CanStartDragFromMouseDown: Boolean;
begin
  Result := False;
end;

function TAdvKanbanBoardTreeViewTableView.CanStartDragFromMouseMove: Boolean;
var
  it: TAdvKanbanBoardItem;
begin
  Result := True;
  if Assigned(FColumn) and Assigned(DragNode) then
  begin
    it := FColumn.FindItemByNode(DragNode);
    if Assigned(it) then
      Result := it.Movable;
  end;
end;

constructor TAdvKanbanBoardTreeViewTableView.Create(AOwner: TComponent);
begin
  inherited;
  FDragTimerAlternative := TTimer.Create(Self);
  FDragTimerAlternative.Interval := 10;
  FDragTimerAlternative.Enabled := False;
  FDragTimerAlternative.OnTimer := DragTimeAlternative;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoAfterSelectNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  UpdateSelection(ANode, True, True);
end;

procedure TAdvKanbanBoardTreeViewTableView.DoDrawNodeTitleExtra(
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode);
var
  it: TAdvKanbanBoardItem;
  r: TRectF;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if it.Expandable then
      begin
        r := ARect;
        r := RectF(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
        InflateRectEx(r, -4, -4);
        if it.Expanded then
          AGraphics.DrawExpanderButton(r, gesExpanded, False, False, True, False)
        else
          AGraphics.DrawExpanderButton(r, gesCollapsed, False, False, True, False);
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.Color;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeDisabledColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.DisabledColor;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeDisabledStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.DisabledStrokeColor;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeTitleExtraSize(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if it.Expandable then
        AExtraSize := 28;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeSelectedColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.SelectedColor;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeSelectedStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.SelectedStrokeColor;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeSides(ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides);
begin
  inherited;
  if not IsVirtualNodeSelected(ANode) then
    ASides := AllSides;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeStrokeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvGraphicsColor);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if not it.UseDefaultAppearance then
        AColor := it.StrokeColor;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoGetNodeRounding(ANode: TAdvTreeViewVirtualNode; var ARounding: Integer; var ACorners: TAdvGraphicsCorners);
var
  it: TAdvKanbanBoardItem;
begin
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      ARounding := it.Rounding;
      ACorners := it.RoundingCorners;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DoUpdateNodeText(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AText: String);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      it.Text := AText;
      if Assigned(FKanbanBoard) then
        FKanbanBoard.DoUpdateItemText(FColumn, it, AText);
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DragTimeAlternative(
  Sender: TObject);
begin
  if not Assigned(FKanbanBoard) then
    Exit;

  if (FDragScrollLeft or FDragScrollRight) and Assigned(FScrollTable) then
  begin
    if FDragScrollLeft then
      FKanbanBoard.Scroll(FKanbanBoard.GetHorizontalScrollPosition - FScrollTable.Width - FKanbanBoard.ColumnsAppearance.Spacing, FKanbanBoard.GetVerticalScrollPosition)
    else if FDragScrollRight then
      FKanbanBoard.Scroll(FKanbanBoard.GetHorizontalScrollPosition + FScrollTable.Width + FKanbanBoard.ColumnsAppearance.Spacing, FKanbanBoard.GetVerticalScrollPosition);

    FDragTimerAlternative.Enabled := False;
  end
  else
  begin
    if Assigned(FDragTableView) then
    begin
      if (FDragPoint.Y >= FDragTableView.GetContentRect.Bottom) then
        FDragTableView.ScrollToVirtualNode(FDragTableView.GetNodeForRow(FDragTableView.StopRow + 1), True, tvnspBottom, True)
      else if FDragPoint.Y <= FDragTableView.GetContentRect.Top then
        FDragTableView.ScrollToVirtualNode(FDragTableView.GetNodeForRow(FDragTableView.StartRow - 1), True, tvnspTop, True);
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.DrawBorders(
  AGraphics: TAdvGraphics);
begin

end;

function TAdvKanbanBoardTreeViewTableView.GetColumnsExtraSize: Double;
begin
  Result := 0;
  if CustomScrollBars then
  begin
    if CustomHorizontalScrollBar.Visible then
      Result := CustomHorizontalScrollBar.Height + 1;

    if CustomVerticalScrollBar.Visible then
      Result := CustomVerticalScrollBar.Width + 1;
  end
  else
  begin
    if HorizontalScrollBar.Visible then
      Result := HorizontalScrollBar.Height + 1;

    if VerticalScrollBar.Visible then
      Result := VerticalScrollBar.Width + 1;
  end;
end;

function TAdvKanbanBoardTreeViewTableView.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  if Assigned(FKanbanBoard) then
    Result.Bottom := Result.Bottom - FKanbanBoard.ItemsAppearance.Margins.Bottom;
end;

procedure TAdvKanbanBoardTreeViewTableView.GetNodeMargins(var ALeft, ATop, ARight, ABottom: Single);
begin
  if Assigned(FKanbanBoard) then
  begin
    ALeft := FKanbanBoard.ItemsAppearance.Margins.Left;
    ATop := FKanbanBoard.ItemsAppearance.Margins.Top;
    ARight := FKanbanBoard.ItemsAppearance.Margins.Right;
    ABottom := FKanbanBoard.ItemsAppearance.Margins.Bottom;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.GetNodeOffset(
  ANode: TAdvTreeViewVirtualNode; var ALeft, ATop, ARight, ABottom: Single);
var
  it: TAdvKanbanBoardItem;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if kbmtLeft in it.MarkType then
        ALeft := it.MarkSizeLeft;
      if kbmtTop in it.MarkType then
        ATop := it.MarkSizeTop;
      if kbmtRight in it.MarkType then
        ARight := it.MarkSizeRight;
      if kbmtBottom in it.MarkType then
        ABottom := it.MarkSizeBottom;
    end;
  end;
end;

function TAdvKanbanBoardTreeViewTableView.GetTotalLeft: Single;
begin
  Result := Left;
  if Assigned(Parent) and (Parent is TAdvCustomControl) then
    Result := Result + (Parent as TAdvCustomControl).Left;
end;

function TAdvKanbanBoardTreeViewTableView.GetTotalTop: Single;
begin
  Result := Top;
  if Assigned(Parent) and (Parent is TAdvCustomControl) then
    Result := Result + (Parent as TAdvCustomControl).Top;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleAlternativeDragDrop(X,
  Y: Single);
var
  a: Boolean;
  p: TPointF;
  c: TAdvKanbanBoardColumn;
  tv: TAdvKanbanBoardTreeViewTableView;
begin
  inherited;
  if Assigned(FKanbanBoard) then
  begin
    FDragTableView := nil;
    FDragTimerAlternative.Enabled := False;
    p := PointF(X + GetTotalLeft, Y + GetTotalTop);
    c := FKanbanBoard.XYToColumn(p.X, p.Y);
    if Assigned(c) and Assigned(c.TableView) and Assigned(c.TableView.TreeView) then
    begin
      tv := (c.TableView.TreeView as TAdvKanbanBoardTreeViewTableView);
      p := PointF(p.X - tv.GetTotalLeft, p.Y - tv.GetTotalTop);
      a := True;
      tv.HandleDragOver(Self, p, a);
      if a then
        tv.HandleDragDrop(Self, p);
    end
    else
    begin
      a := True;
      HandleDragOver(Self, PointF(X, Y), a);
      if a then
        HandleDragDrop(Self, PointF(X, Y));
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleAlternativeDragOver(X,
  Y: Single);
var
  a, en: Boolean;
  p, pt: TPointF;
  I: Integer;
  c: TAdvKanbanBoardColumn;
  tv: TAdvKanbanBoardTreeViewTableView;
  tvx: TAdvKanbanBoardTableView;
begin
  inherited;
  if Assigned(FKanbanBoard) then
  begin
    en := False;
    p := PointF(X + GetTotalLeft, Y + GetTotalTop);
    c := FKanbanBoard.XYToColumn(p.X, p.Y);
    a := True;
    if Assigned(c) and Assigned(c.TableView) and Assigned(c.TableView.TreeView) then
    begin
      tv := (c.TableView.TreeView as TAdvKanbanBoardTreeViewTableView);
      pt := PointF(p.X - tv.GetTotalLeft, p.Y - tv.GetTotalTop);
      a := True;
      tv.HandleDragOver(Self, pt, a);
      FDragPoint := p;
      FDragTableView := tv;
      en := (p.Y >= tv.GetContentRect.Bottom) or (p.Y <= tv.GetContentRect.Top);
    end
    else
    begin
      a := True;
      HandleDragOver(Self, PointF(X, Y), a);
    end;

    FDragScrollLeft := (p.X < FKanbanBoard.LocalRect.Left);
    FDragScrollRight := (p.X > FKanbanBoard.LocalRect.Right);

    FScrollTable := nil;
    if FDragScrollRight then
    begin
      for I := 0 to FKanbanBoard.ControlCount - 1 do
      begin
        if FKanbanBoard.Controls[I] is TAdvKanbanBoardTableView then
        begin
          tvx := FKanbanBoard.Controls[I] as TAdvKanbanBoardTableView;
          if tvx.BoundsRect.Right > FKanbanBoard.LocalRect.Right then
          begin
            FScrollTable := tvx;
            Break;
          end;
        end;
      end;
    end
    else if FDragScrollLeft then
    begin
      for I := FKanbanBoard.ControlCount - 1 downto 0 do
      begin
        if FKanbanBoard.Controls[I] is TAdvKanbanBoardTableView then
        begin
          tvx := FKanbanBoard.Controls[I] as TAdvKanbanBoardTableView;
          if (tvx.BoundsRect.Left < FKanbanBoard.LocalRect.Left) then
          begin
            FScrollTable := tvx;
            Break;
          end;
        end;
      end;
    end;

    FDragTimerAlternative.Enabled := en or ((FDragScrollLeft or FDragScrollRight) and Assigned(FScrollTable));

    if a then
      Cursor := crDrag
    else
      Cursor := crNoDrop;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleCustomKeys(AKey: Word);
var
  I, K: Integer;
begin
  inherited;
  if not (AKey in [KEY_LEFT, KEY_RIGHT]) then
    Exit;

  if Assigned(FKanbanBoard) then
  begin
    for I := 0 to FKanbanBoard.Columns.Count - 1 do
    begin
      if (FKanbanBoard.Columns[I].TableView.ItemIndex <> -1) then
      begin
        K := I;
        case AKey of
          KEY_RIGHT:
          begin
            Inc(K);
            while (K < FKanbanBoard.Columns.Count - 1) and (not FKanbanBoard.Columns[K].Visible or not Assigned(FKanbanBoard.Columns[K].TableView.Parent)) do
              Inc(K);
          end;
          KEY_LEFT:
          begin
            Dec(K);
            while (K > 0) and (not FKanbanBoard.Columns[K].Visible or not Assigned(FKanbanBoard.Columns[K].TableView.Parent)) do
              Dec(K);
          end;
        end;

        if (K >= 0) and (K <= FKanbanBoard.Columns.Count - 1) and FKanbanBoard.Columns[K].Visible and Assigned(FKanbanBoard.Columns[K].TableView.Parent) then
        begin
          FKanbanBoard.ClearSelection;
          if not IntersectRectEx(ConvertToRectF(FKanbanBoard.Columns[K].TableView.BoundsRect), FKanbanBoard.LocalRect) then
          begin
            if AKey = KEY_RIGHT then
              FKanbanBoard.Scroll(FKanbanBoard.Columns[K].TableView.FOriginalBoundsRect.Right - FKanbanBoard.GetContentRect.Right - FKanbanBoard.GetContentRect.Left + FKanbanBoard.ColumnsAppearance.Margins.Right, FKanbanBoard.GetVerticalScrollPosition)
            else if AKey = KEY_LEFT then
              FKanbanBoard.Scroll(FKanbanBoard.Columns[K].TableView.FOriginalBoundsRect.Left - FKanbanBoard.ColumnsAppearance.Margins.Left, FKanbanBoard.GetVerticalScrollPosition);
          end;

          FKanbanBoard.Columns[K].TableView.ItemIndex := 0;

          if FKanbanBoard.Columns[K].TableView.TreeView.CanFocus then
          begin
            FKanbanBoard.Columns[K].TableView.TreeView.SetFocus;
          end;
        end;

        Break;
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleDragDrop(
  const Source: TObject; const Point: TPointF);
var
  tv: TAdvKanbanBoardTreeViewTableView;
begin
  inherited;
  if Source is TAdvKanbanBoardTreeViewTableView then
  begin
    tv := Source as TAdvKanbanBoardTreeViewTableView;
    if Assigned(tv.DragBitmap) and tv.DragStarted then
    begin
      tv.DestroyDragBitmap;
      tv.DragStarted := False;
      tv.Invalidate;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleDragOver(
  const Source: TObject; const Point: TPointF; var Accept: Boolean);
var
  img: TAdvImageEx;
  tv: TAdvKanbanBoardTreeViewTableView;
begin
  inherited;
  if Source is TAdvKanbanBoardTreeViewTableView then
  begin
    tv := Source as TAdvKanbanBoardTreeViewTableView;
    if Assigned(tv.DragBitmap) and tv.DragStarted then
    begin
      img := tv.DragBitmap;
      img.SetBounds(Round((Point.X - tv.SaveLeft) + GetTotalLeft), Round((Point.Y - tv.SaveTop) + GetTotalTop), img.Width, img.Height);
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleDragStart(X, Y: Single);
var
  img: TAdvImageEx;
  offx, offy: Single;
begin
  inherited;
  if IsDragDropActive and not IsReorderActive then
  begin
    CreateDragBitmap;
    if Assigned(DragBitmap) then
    begin
      img := DragBitmap;
      offx := img.Left;
      offy := img.Top;
      img.Parent := FKanbanBoard;
      img.SetBounds(Round(GetTotalLeft + offx), Round(GetTotalTop + offy), img.Width, img.Height);
      SaveLeft := (X - offx);
      SaveTop := (Y - offy);
      FDragStarted := True;
      Invalidate;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleExpand(AColumn: Integer);
var
  c: Boolean;
begin
  inherited;
  if Assigned(FColumn) then
  begin
    if FColumn.Expandable then
    begin
      c := not FColumn.Expanded;
      FColumn.Expanded := not FColumn.Expanded;
      if Assigned(FKanbanBoard) then
      begin
        if c then
          FKanbanBoard.DoColumnExpand(FColumn)
        else
          FKanbanBoard.DoColumnCollapse(FColumn);
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleMouseDown(
  Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single);
var
  it: TAdvTreeViewVirtualNode;
begin
  if Assigned(FKanbanBoard) then
  begin
    if FKanbanBoard.Interaction.DirectDrag then
    begin
      it := XYToNode(X, Y);
      if Assigned(it) then
        SelectVirtualNode(it);
    end;
  end;
  inherited;
end;

procedure TAdvKanbanBoardTreeViewTableView.HandleNodeTitleExtra(
  ANode: TAdvTreeViewVirtualNode);
var
  it: TAdvKanbanBoardItem;
  c: Boolean;
begin
  inherited;
  if Assigned(FColumn) and Assigned(ANode) then
  begin
    it := FColumn.FindItemByNode(ANode);
    if Assigned(it) then
    begin
      if it.Expandable then
      begin
        c := not it.Expanded;
        it.Expanded := not it.Expanded;
        if Assigned(FKanbanBoard) then
        begin
          if c then
            FKanbanBoard.DoItemExpand(FColumn, it)
          else
            FKanbanBoard.DoItemCollapse(FColumn, it);
        end;
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardTreeViewTableView.InternalSelectVirtualNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  inherited;
  UpdateSelection(ANode, False, False);
end;

procedure TAdvKanbanBoardTreeViewTableView.UpdateSelection(
  ANode: TAdvTreeViewVirtualNode; AFocus: Boolean; ATriggerEvents: Boolean);
var
  I: Integer;
  it: TAdvKanbanBoardItem;
begin
  if Assigned(FKanbanBoard) and Assigned(FColumn) then
  begin
    for I := 0 to FKanbanBoard.Columns.Count - 1 do
    begin
      if FKanbanBoard.Columns[I].FTableView <> FColumn.FTableView then
        FKanbanBoard.Columns[I].FTableView.ClearSelection;
    end;

    if Assigned(ANode) then
    begin
      it := FColumn.FindItemByNode(ANode);
      if Assigned(it) then
      begin
        if ATriggerEvents then
          FKanbanBoard.DoSelectItem(FColumn, it)
        else
        begin
          if Assigned(FKanbanBoard.Adapter) then
            FKanbanBoard.Adapter.SelectItem(it);
        end;

        if AFocus and Assigned(it.TableView) and Assigned(it.TableView.TreeView) and it.TableView.TreeView.CanFocus then
          it.TableView.TreeView.SetFocus;
      end;
    end;
  end;
end;

{ TAdvKanbanBoardColumn }

function TAdvKanbanBoardColumn.AddItem(AText: string): TAdvKanbanBoardItem;
begin
  Result := Items.Add;
  Result.Text := AText;
end;

procedure TAdvKanbanBoardColumn.ApplyFilter;
begin
  if Assigned(TableView) then
    TableView.ApplyFilter;
end;

procedure TAdvKanbanBoardColumn.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardColumn then
  begin
    FTag := (Source as TAdvKanbanBoardColumn).Tag;
    FName := (Source as TAdvKanbanBoardColumn).Name;
    FHeaderText := (Source as TAdvKanbanBoardColumn).HeaderText;
    FHeaderTrimming := (Source as TAdvKanbanBoardColumn).HeaderTrimming;
    FHeaderWordWrapping := (Source as TAdvKanbanBoardColumn).HeaderWordWrapping;
    FHeaderHorizontalTextAlign := (Source as TAdvKanbanBoardColumn).HeaderHorizontalTextAlign;
    FHeaderVerticalTextAlign := (Source as TAdvKanbanBoardColumn).HeaderVerticalTextAlign;
    FFooterText := (Source as TAdvKanbanBoardColumn).FooterText;
    FFooterTrimming := (Source as TAdvKanbanBoardColumn).FooterTrimming;
    FFooterWordWrapping := (Source as TAdvKanbanBoardColumn).FooterWordWrapping;
    FFooterHorizontalTextAlign := (Source as TAdvKanbanBoardColumn).FooterHorizontalTextAlign;
    FFooterVerticalTextAlign := (Source as TAdvKanbanBoardColumn).FooterVerticalTextAlign;
    FWidth := (Source as TAdvKanbanBoardColumn).Width;
    FVisible := (Source as TAdvKanbanBoardColumn).Visible;
    FHeaderVisible := (Source as TAdvKanbanBoardColumn).HeaderVisible;
    FFooterVisible := (Source as TAdvKanbanBoardColumn).FooterVisible;
    FUseDefaultAppearance := (Source as TAdvKanbanBoardColumn).UseDefaultAppearance;
    FFill.Assign((Source as TAdvKanbanBoardColumn).Fill);
    FStroke.Assign((Source as TAdvKanbanBoardColumn).Stroke);
    FFooterFill.Assign((Source as TAdvKanbanBoardColumn).FooterFill);
    FFooterFont.Assign((Source as TAdvKanbanBoardColumn).FooterFont);
    FFooterStroke.Assign((Source as TAdvKanbanBoardColumn).FooterStroke);
    FHeaderFill.Assign((Source as TAdvKanbanBoardColumn).HeaderFill);
    FHeaderStroke.Assign((Source as TAdvKanbanBoardColumn).HeaderStroke);
    FHeaderFont.Assign((Source as TAdvKanbanBoardColumn).HeaderFont);
    FExpanded := (Source as TAdvKanbanBoardColumn).Expanded;
    FExpandable := (Source as TAdvKanbanBoardColumn).Expandable;
    FCollapsedWidth := (Source as TAdvKanbanBoardColumn).CollapsedWidth;
    FItems.Assign((Source as TAdvKanbanBoardColumn).Items);
  end;
end;

procedure TAdvKanbanBoardColumn.Changed(Sender: TObject);
begin
  UpdateColumn;
end;

procedure TAdvKanbanBoardColumn.ClearSelection;
begin
  FTableView.ClearSelection;
end;

function TAdvKanbanBoardColumn.CopyItem(AItem: TAdvKanbanBoardItem;
  AToColumn: TAdvKanbanboardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem;
begin
  Result := nil;
  if Assigned(AItem) then
    Result := AItem.CopyItem(AToColumn, AIndex);
end;

constructor TAdvKanbanBoardColumn.Create(Collection: TCollection);
var
  s: Single;
begin
  inherited;

  s := 1;

  if Assigned(Collection) then
  begin
    FKanbanBoard := (Collection as TAdvKanbanBoardColumns).KanbanBoard;
    s := FKanbanBoard.PaintScaleFactor;
  end;

  FTableView := CreateTableView;
  (FTableView.TreeView as TAdvKanbanBoardTreeViewTableView).FColumn := Self;
  (FTableView.TreeView as TAdvKanbanBoardTreeViewTableView).FKanbanBoard := FKanbanBoard;
  FTableView.FKanbanBoard := FKanbanBoard;
  FTableView.FColumn := Self;
  FTableView.TabStop := True;
  FTableView.Stored := False;
  FTableView.OnAfterDrawItem := DoAfterDrawItem;
  FTableView.OnBeforeDrawItem := DoBeforeDrawItem;
  FTableView.OnAfterDrawItemText := DoAfterDrawItemText;
  FTableView.OnBeforeDrawItemText := DoBeforeDrawItemText;
  FTableView.OnAfterDrawItemTitle := DoAfterDrawItemTitle;
  FTableView.OnBeforeDrawItemTitle := DoBeforeDrawItemTitle;
  FTableView.OnAfterDrawItemIcon := DoAfterDrawItemIcon;
  FTableView.OnBeforeDrawItemIcon := DoBeforeDrawItemIcon;
  FTableView.OnAfterDropItem := DoAfterDropItem;
  FTableView.OnBeforeDropItem := DoBeforeDropItem;
  FTableView.OnHeaderAnchorClick := DoHeaderAnchorClick;
  FTableView.OnItemAnchorClick := DoItemTextAnchorClick;
  FTableView.OnItemTitleAnchorClick := DoItemTitleAnchorClick;
  FTableView.OnItemClick := DoItemClick;
  FTableView.OnItemDblClick := DoItemDblClick;
  FTableView.TreeView.OnKeyDown := DoKeyDown;
  FTableView.TreeView.OnKeyUp := DoKeyUp;
  FTableView.TreeView.OnClick := DoClick;
  FTableView.TreeView.OnDblClick := DoDblClick;

  FItems := CreateItems;

  FCollapsedWidth := s * 100;
  FHeaderVisible := True;
  FFooterVisible := False;
  FExpanded := True;
  FExpandable := False;

  FHeaderWordWrapping := False;
  FHeaderTrimming := gttCharacter;
  FHeaderHorizontalTextAlign := gtaCenter;
  FHeaderVerticalTextAlign := gtaCenter;
  if Assigned(Collection) then
    FHeaderText := TranslateTextEx(sAdvKanbanBoardHeader) + ' ' + inttostr(Collection.Count - 1);

  FFooterWordWrapping := False;
  FFooterTrimming := gttCharacter;
  FFooterHorizontalTextAlign := gtaCenter;
  FFooterVerticalTextAlign := gtaCenter;
  if Assigned(Collection) then
    FFooterText := TranslateTextEx(sAdvKanbanBoardFooter) + ' ' + inttostr(Collection.Count - 1);

  FName :=  StringReplace(FHeaderText, ' ', '', [rfReplaceAll]);
  FWidth := s * 250;
  FVisible := True;
  FUseDefaultAppearance := True;

  FFooterFill := TAdvGraphicsFill.Create(gfkNone, gcWhite);
  FFooterStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FHeaderFill := TAdvGraphicsFill.Create(gfkNone, gcWhite);
  FHeaderStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FFont := TAdvGraphicsFont.Create;
  FHeaderFont := TAdvGraphicsFont.Create;
  FFooterFont := TAdvGraphicsFont.Create;

  FFooterFill.OnChanged := Changed;
  FHeaderFill.OnChanged := Changed;
  FFooterStroke.OnChanged := Changed;
  FHeaderStroke.OnChanged := Changed;
  FStroke.OnChanged := Changed;
  FFill.OnChanged := Changed;

  FFont.OnChanged := Changed;
  FHeaderFont.OnChanged := Changed;
  FFooterFont.OnChanged := Changed;

  UpdateColumn;
end;

function TAdvKanbanBoardColumn.CreateItems: TAdvKanbanBoardItems;
begin
  Result := TAdvKanbanBoardItems.Create(Self);
end;

function TAdvKanbanBoardColumn.CreateTableView: TAdvKanbanBoardTableView;
begin
  Result := TAdvKanbanBoardTableView.Create(FKanbanBoard);
end;

destructor TAdvKanbanBoardColumn.Destroy;
begin
  FItems.Free;
  FHeaderFill.Free;
  FHeaderStroke.Free;
  FHeaderFont.Free;
  FFooterFill.Free;
  FFooterStroke.Free;
  FFooterFont.Free;
  FFill.Free;
  FStroke.Free;
  FFont.Free;
  if Assigned(FTableView) then
    FTableView.Free;
  inherited;
  UpdateColumn;
end;

procedure TAdvKanbanBoardColumn.DisableInteraction;
begin
  FTableView.DisableInteraction;
end;

procedure TAdvKanbanBoardColumn.DoAfterDrawItemTitle(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: string);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoAfterDrawItemTitle(AGraphics, ARect, Self, it, ATitle);
  end;
end;

procedure TAdvKanbanBoardColumn.DoAfterDrawItemText(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: string);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoAfterDrawItemText(AGraphics, ARect, Self, it, AText);
  end;
end;

procedure TAdvKanbanBoardColumn.DoAfterDrawItem(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
  c: TAdvGraphicsColor;
  r: TRectF;
  mr: Integer;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) and Assigned(it.Column) then
    begin
      c := it.MarkColor;
      AGraphics.Fill.Kind := gfkSolid;
      AGraphics.Stroke.Kind := gskSolid;
      AGraphics.Stroke.Color := c;
      AGraphics.Fill.Color :=  c;

      if kbmtLeft in it.MarkType then
      begin
        if it.MarkColorLeft <> gcNull then
        begin
          AGraphics.Stroke.Color := it.MarkColorLeft;
          AGraphics.Fill.Color :=  it.MarkColorLeft;
        end;

        r := RectF(ARect.Left, ARect.Top, ARect.Left + it.MarkSizeLeft, ARect.Bottom);
        if Assigned(k.OnItemCustomDrawMark) then
          k.DoItemCustomDrawMark(AGraphics, r, kbmtLeft, it.Column, it)
        else
        begin
          mr := Min(Trunc(it.MarkSizeLeft / 2), it.MarkRounding);
          AGraphics.DrawRoundRectangle(r, mr, it.FMarkCorners, gcrmNone);
        end;

        if it.MarkColorLeft <> gcNull then
        begin
          AGraphics.Stroke.Color := c;
          AGraphics.Fill.Color := c;
        end;
      end;

      if kbmtTop in it.MarkType then
      begin
        if it.MarkColorTop <> gcNull then
        begin
          AGraphics.Stroke.Color := it.MarkColorTop;
          AGraphics.Fill.Color :=  it.MarkColorTop;
        end;

        r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + it.MarkSizeTop);
        if Assigned(k.OnItemCustomDrawMark) then
          k.DoItemCustomDrawMark(AGraphics, r, kbmtTop, it.Column, it)
        else
        begin
          mr := Min(Trunc(it.MarkSizeTop / 2), it.MarkRounding);
          AGraphics.DrawRoundRectangle(r, mr, it.FMarkCorners, gcrmNone);
        end;

        if it.MarkColorTop <> gcNull then
        begin
          AGraphics.Stroke.Color := c;
          AGraphics.Fill.Color := c;
        end;
      end;

      if kbmtRight in it.MarkType then
      begin
        if it.MarkColorRight <> gcNull then
        begin
          AGraphics.Stroke.Color := it.MarkColorRight;
          AGraphics.Fill.Color :=  it.MarkColorRight;
        end;
        r := RectF(ARect.Right - it.MarkSizeRight, ARect.Top, ARect.Right, ARect.Bottom);
        if Assigned(k.OnItemCustomDrawMark) then
          k.DoItemCustomDrawMark(AGraphics, r, kbmtRight, it.Column, it)
        else
        begin
          mr := Min(Trunc(it.MarkSizeRight / 2), it.MarkRounding);
          AGraphics.DrawRoundRectangle(r, mr, it.FMarkCorners, gcrmNone);
        end;

        if it.MarkColorRight <> gcNull then
        begin
          AGraphics.Stroke.Color := c;
          AGraphics.Fill.Color := c;
        end;
      end;

      if kbmtBottom in it.MarkType then
      begin
        if it.MarkColorBottom <> gcNull then
        begin
          AGraphics.Stroke.Color := it.MarkColorBottom;
          AGraphics.Fill.Color :=  it.MarkColorBottom;
        end;

        r := RectF(ARect.Left, ARect.Bottom - it.MarkSizeBottom, ARect.Right, ARect.Bottom);
        if Assigned(k.OnItemCustomDrawMark) then
          k.DoItemCustomDrawMark(AGraphics, r, kbmtBottom, it.Column, it)
        else
        begin
          mr := Min(Trunc(it.MarkSizeBottom / 2), it.MarkRounding);
          AGraphics.DrawRoundRectangle(r, mr, it.FMarkCorners, gcrmNone);
        end;

        if it.MarkColorBottom <> gcNull then
        begin
          AGraphics.Stroke.Color := c;
          AGraphics.Fill.Color := c;
        end;
      end;

      k.DoAfterDrawItem(AGraphics, ARect, Self, it);
    end;
  end;
end;

procedure TAdvKanbanBoardColumn.DoAfterDrawItemIcon(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem;
  AIcon: TAdvBitmap);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoAfterDrawItemIcon(AGraphics, ARect, Self, it, AIcon);
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvKanbanBoardColumn.DoKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvKanbanBoardColumn.DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
  begin
    {$IFDEF FMXLIB}
    k.KeyUp(Key, KeyChar, Shift);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    k.KeyUp(Key, Shift);
    {$ENDIF}
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvKanbanBoardColumn.DoKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvKanbanBoardColumn.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
  begin
    {$IFDEF FMXLIB}
    k.KeyDown(Key, KeyChar, Shift);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    k.KeyDown(Key, Shift);
    {$ENDIF}
  end;
end;

procedure TAdvKanbanBoardColumn.DoAfterDropItem(Sender: TObject; AFromItem,
  AToItem: TAdvTableViewItem);
var
  itfrom, itto: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
  tv: TAdvKanbanBoardTableView;
  cfrom, cto: TAdvKanbanBoardColumn;
begin
  k := KanbanBoard;
  if Assigned(k) then
  begin
    cfrom := nil;
    cto := nil;
    itfrom := nil;
    itto := nil;

    if Assigned(AFromItem) and Assigned(AFromItem.TableView) then
    begin
      if AFromItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AFromItem.TableView as TAdvKanbanBoardTableView;
        cfrom := tv.FColumn;
        if Assigned(cfrom) then
          itfrom := cfrom.FindItem(AFromItem.Index);
      end;
    end;

    if Assigned(AToItem) and Assigned(AToItem.TableView) then
    begin
      if AToItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AToItem.TableView as TAdvKanbanBoardTableView;
        cto := tv.FColumn;
        if Assigned(cto) then
          itto := cto.FindItem(AToItem.Index);
      end;
    end;

     k.DoAfterDropItem(cfrom, cto, itfrom, itto);
  end;
end;

procedure TAdvKanbanBoardColumn.DoBeforeDrawItemTitle(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: string;
  var AAllow: Boolean);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoBeforeDrawItemTitle(AGraphics, ARect, Self, it, ATitle, AAllow);
  end;
end;

procedure TAdvKanbanBoardColumn.DoBeforeDrawItemText(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: string;
  var AAllow: Boolean);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoBeforeDrawItemText(AGraphics, ARect, Self, it, AText, AAllow);
  end;
end;

procedure TAdvKanbanBoardColumn.DoBeforeDrawItem(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem;
  var AAllow, ADefaultDraw: Boolean);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoBeforeDrawItem(AGraphics, ARect, Self, it, AAllow, ADefaultDraw);
  end;
end;

procedure TAdvKanbanBoardColumn.DoBeforeDrawItemIcon(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem;
  AIcon: TAdvBitmap; var AAllow: Boolean);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoBeforeDrawItemIcon(AGraphics, ARect, Self, it, AIcon, AAllow);
  end;
end;

procedure TAdvKanbanBoardColumn.DoBeforeDropItem(Sender: TObject; AFromItem,
  AToItem: TAdvTableViewItem; var ACanDrop: Boolean);
var
  itfrom, itto: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
  tv: TAdvKanbanBoardTableView;
  cfrom, cto: TAdvKanbanBoardColumn;
begin
  k := KanbanBoard;
  if Assigned(k) then
  begin
    cfrom := nil;
    cto := nil;
    itfrom := nil;
    itto := nil;

    if Assigned(AFromItem) and Assigned(AFromItem.TableView) then
    begin
      if AFromItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AFromItem.TableView as TAdvKanbanBoardTableView;
        cfrom := tv.FColumn;
        if Assigned(cfrom) then
          itfrom := cfrom.FindItem(AFromItem.Index);
      end;
    end;

    if Assigned(AToItem) and Assigned(AToItem.TableView) then
    begin
      if AToItem.TableView is TAdvKanbanBoardTableView then
      begin
        tv := AToItem.TableView as TAdvKanbanBoardTableView;
        cto := tv.FColumn;
        if Assigned(cto) then
          itto := cto.FindItem(AToItem.Index);
      end;
    end;

     k.DoBeforeDropItem(cfrom, cto, itfrom, itto, ACanDrop);
  end;
end;

procedure TAdvKanbanBoardColumn.DoClick(Sender: TObject);
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
    k.Click;
end;

procedure TAdvKanbanBoardColumn.DoDblClick(Sender: TObject);
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
    k.DblClick;
end;

procedure TAdvKanbanBoardColumn.DoHeaderAnchorClick(Sender: TObject; AAnchor: String);
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
  begin
    k.DoColumnAnchorClick(Self, AAnchor);
  end;
end;

procedure TAdvKanbanBoardColumn.DoItemTextAnchorClick(Sender: TObject; AItem: TAdvTableViewItem; AAnchor: String);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoItemTextAnchorClick(Self, it, AAnchor);
  end;
end;

procedure TAdvKanbanBoardColumn.DoItemDblClick(Sender: TObject; AItem: TAdvTableViewItem);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoItemDblClick(Self, it);
  end;
end;

procedure TAdvKanbanBoardColumn.DoItemClick(Sender: TObject; AItem: TAdvTableViewItem);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoItemClick(Self, it);
  end;
end;

procedure TAdvKanbanBoardColumn.DoItemTitleAnchorClick(Sender: TObject;
  AItem: TAdvTableViewItem; AAnchor: String);
var
  it: TAdvKanbanBoardItem;
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) and Assigned(AItem) then
  begin
    it := FindItem(AItem.Index);
    if Assigned(it) then
      k.DoItemTitleAnchorClick(Self, it, AAnchor);
  end;
end;

procedure TAdvKanbanBoardColumn.EnableInteraction;
begin
  FTableView.EnableInteraction;
end;

function TAdvKanbanBoardColumn.GetPictureContainer: TPictureContainer;
var
  k: TAdvKanbanBoardTableView;
begin
  Result := nil;
  k := TableView;
  if Assigned(k) then
    Result := k.PictureContainer;
end;

function TAdvKanbanBoardColumn.GetDoneButton: TAdvToolBarExButton;
begin
  Result := nil;
  if Assigned(FTableView) then
    Result := FTableView.DoneButton;
end;

function TAdvKanbanBoardColumn.GetFilter: TAdvTableViewFilter;
begin
  Result := nil;
  if Assigned(FTableView) then
    Result := FTableView.Filter;
end;

function TAdvKanbanBoardColumn.GetFilterButton: TAdvToolBarExButton;
begin
  Result := nil;
  if Assigned(FTableView) then
    Result := FTableView.FilterButton;
end;

function TAdvKanbanBoardColumn.GetFooterColumnText: String;
begin
  Result := FooterText;
  if (Result = '') or (Pos('</', Result) > 0) or (Pos('/>', Result) > 0) or (Pos('<BR>', UpperCase(Result)) > 0) then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sAdvKanbanBoardFooter) + ' ' + inttostr(Index);
  end;
end;

function TAdvKanbanBoardColumn.GetHeaderColumnText: String;
begin
  Result := HeaderText;
  if (Result = '') or (Pos('</', Result) > 0) or (Pos('/>', Result) > 0) or (Pos('<BR>', UpperCase(Result)) > 0) then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sAdvKanbanBoardHeader) + ' ' + inttostr(Index);
  end;
end;

function TAdvKanbanBoardColumn.GetItemIndex: Integer;
begin
  Result := -1;
  if Assigned(FTableView) then
    Result := FTableView.ItemIndex;
end;

function TAdvKanbanBoardColumn.FindItem(
  AIndex: Integer): TAdvKanbanBoardItem;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= Items.Count - 1) then
    Result := Items[AIndex];
end;

function TAdvKanbanBoardColumn.FindItemByNode(
  ANode: TAdvTreeViewVirtualNode): TAdvKanbanBoardItem;
var
  idx: Integer;
begin
  Result := nil;
  if Assigned(ANode) and Assigned(FTableView) then
  begin
    idx := FTableView.GetItemIndexForNode(ANode);
    if (idx >= 0) and (idx <= Items.Count - 1) then
      Result := Items[idx];
  end;
end;

function TAdvKanbanBoardColumn.GetHeaderText: String;
begin
  Result := HeaderText;
  if Result = '' then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sAdvKanbanBoardHeader) + ' ' + inttostr(Index);
  end;
end;

function TAdvKanbanBoardColumn.GetFooterText: String;
begin
  Result := FooterText;
  if Result = '' then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sAdvKanbanBoardFooter) + ' ' + inttostr(Index);
  end;
end;

function TAdvKanbanBoardColumn.LookupItem(ALookupString: String;
  ACaseSensitive, AAutoSelect: Boolean): TAdvKanbanBoardItem;
var
  it: TAdvTableViewItem;
begin
  Result := nil;
  if Assigned(FTableView) then
  begin
    it := FTableView.LookupItem(ALookupString, ACaseSensitive, AAutoSelect);
    if Assigned(it) then
      Result := FindItem(it.Index);
  end;
end;

function TAdvKanbanBoardColumn.MoveItem(AItem: TAdvKanbanBoardItem;
  AToColumn: TAdvKanbanboardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem;
begin
  Result := nil;
  if Assigned(AItem) then
    Result := AItem.MoveItem(AToColumn, AIndex);
end;

procedure TAdvKanbanBoardColumn.RemoveFilter;
begin
  if Assigned(FTableView) then
    FTableView.RemoveFilter;
end;

procedure TAdvKanbanBoardColumn.RemoveFilters;
begin
  if Assigned(FTableView) then
    FTableView.RemoveFilters;
end;

procedure TAdvKanbanBoardColumn.RemoveItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(AItem) then
    Items.Delete(AItem.Index);
end;

function TAdvKanbanBoardColumn.GetKanbanBoard: TAdvCustomKanbanBoard;
begin
  Result := FKanbanBoard;
end;

function TAdvKanbanBoardColumn.GetSelectedItem: TAdvKanbanBoardItem;
var
  it: TAdvTableViewItem;
begin
  Result := nil;
  if Assigned(FTableView) then
  begin
    it := FTableView.SelectedItem;
    if Assigned(it) then
      Result := FindItem(it.Index);
  end;
end;

function TAdvKanbanBoardColumn.GetSelectedItems: TAdvKanbanBoardSelectedItems;
var
  sel: TAdvTableViewSelectedItems;
  I: Integer;
begin
  if Assigned(FTableView) then
  begin
    sel := FTableView.GetSelectedItems;
    SetLength(Result, Length(sel));
    for I := 0 to Length(sel) - 1 do
    begin
      if Assigned(sel[I]) then
        Result[I] := FindItem(sel[I].Index);
    end;
  end;
end;

function TAdvKanbanBoardColumn.GetSelItem(
  AIndex: Integer): TAdvKanbanBoardItem;
var
  it: TAdvTableViewItem;
begin
  Result := nil;
  if Assigned(FTableView) then
  begin
    it := FTableView.SelectedItems[AIndex];
    if Assigned(it) then
      Result := FindItem(it.Index);
  end;
end;

function TAdvKanbanBoardColumn.GetShowFilterButton: Boolean;
begin
  Result := False;
  if Assigned(FTableView) then
    Result := FTableView.Interaction.ShowFilterButton;
end;

function TAdvKanbanBoardColumn.GetSorting: TAdvKanbanBoardSorting;
var
  k: Integer;
begin
  Result := kbsNone;
  if Assigned(FTableView) then
  begin
    k := Integer(TAdvTableViewInteractionOpen(FTableView.Interaction).Sorting);
    Result := TAdvKanbanBoardSorting(k);
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHeaderHorizontalTextAlign <> Value then
  begin
    FHeaderHorizontalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetItemIndex(const Value: Integer);
begin
  if Assigned(FTableView) then
    FTableView.ItemIndex := Value;
end;

procedure TAdvKanbanBoardColumn.SetItems(
  const Value: TAdvKanbanBoardItems);
begin
  FItems.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetFooterFont(const Value: TAdvGraphicsFont);
begin
  if FFooterFont <> Value then
    FFooterFont.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetFooterHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FFooterHorizontalTextAlign <> Value then
  begin
    FFooterHorizontalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FFooterStroke <> Value then
    FFooterStroke.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetFooterText(const Value: String);
begin
  if FFooterText <> Value then
  begin
    FFooterText := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterTrimming(
  const Value: TAdvGraphicsTextTrimming);
begin
  if FFooterTrimming <> Value then
  begin
    FFooterTrimming := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FFooterVerticalTextAlign <> Value then
  begin
    FFooterVerticalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterVisible(const Value: Boolean);
begin
  if FFooterVisible <> Value then
  begin
    FFooterVisible := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterWordWrapping(const Value: Boolean);
begin
  if FFooterWordWrapping <> Value then
  begin
    FFooterWordWrapping := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.ScrollToItem(AItemIndex: Integer);
begin
  if Assigned(FTableView) then
    FTableView.ScrollToItem(AItemIndex);
end;

function TAdvKanbanBoardColumn.SelectedItemCount: Integer;
begin
  Result := 0;
  if Assigned(FTableView) then
    Result := FTableView.SelectedItemCount;
end;

procedure TAdvKanbanBoardColumn.SelectItem(AItemIndex: Integer);
begin
  if Assigned(FTableView) then
    FTableView.SelectItem(AItemIndex);
end;

procedure TAdvKanbanBoardColumn.SelectItems(
  AItemIndexes: TAdvKanbanBoardIntegerArray);
var
  arr: TAdvTableViewIntegerArray;
  I: Integer;
begin
  if Assigned(FTableView) then
  begin
    SetLength(arr, Length(AItemIndexes));
    for I := 0 to Length(AItemIndexes) - 1 do
      arr[I] := AItemIndexes[I];

    FTableView.SelectItems(arr);
  end;
end;

procedure TAdvKanbanBoardColumn.SetFooterFill(const Value: TAdvGraphicsFill);
begin
  if FFooterFill <> Value then
    FFooterFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetSelectedItem(
  const Value: TAdvKanbanBoardItem);
begin
  if Assigned(Value) then
    ItemIndex := Value.Index;
end;

procedure TAdvKanbanBoardColumn.SetShowFilterButton(const Value: Boolean);
begin
  if Assigned(FTableView) then
    FTableView.Interaction.ShowFilterButton := Value;
end;

procedure TAdvKanbanBoardColumn.SetSorting(
  const Value: TAdvKanbanBoardSorting);
var
  k: Integer;
begin
  if Assigned(FTableView) then
  begin
    k := Integer(Value);
    TAdvTableViewInteractionOpen(FTableView.Interaction).Sorting := TAdvTableViewSorting(k);
  end;
end;

procedure TAdvKanbanBoardColumn.SetStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FStroke <> Value then
    FStroke.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetCollapsedWidth(const Value: Double);
begin
  if FCollapsedWidth <> Value then
  begin
    FCollapsedWidth := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetExpandable(const Value: Boolean);
begin
  if FExpandable <> Value then
  begin
    FExpandable := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetFill(const Value: TAdvGraphicsFill);
begin
  if FFill <> Value then
    FFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetFilter(
  const Value: TAdvTableViewFilter);
begin
  if Assigned(FTableView) then
    FTableView.Filter.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetHeaderFont(const Value: TAdvGraphicsFont);
begin
  if FHeaderFont <> Value then
    FHeaderFont.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetHeaderStroke(
  const Value: TAdvGraphicsStroke);
begin
  if FHeaderStroke <> Value then
    FHeaderStroke.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetHeaderFill(const Value: TAdvGraphicsFill);
begin
  if FHeaderFill <> Value then
    FHeaderFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumn.SetName(const Value: String);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderText(const Value: String);
begin
  if FHeaderText <> Value then
  begin
    FHeaderText := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderTrimming(const Value: TAdvGraphicsTextTrimming);
begin
  if FHeaderTrimming <> Value then
  begin
    FHeaderTrimming := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetUseDefaultAppearance(const Value: Boolean);
begin
  if FUseDefaultAppearance <> Value then
  begin
    FUseDefaultAppearance := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHeaderVerticalTextAlign <> Value then
  begin
    FHeaderVerticalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderVisible(const Value: Boolean);
begin
  if FHeaderVisible <> Value then
  begin
    FHeaderVisible := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetWidth(const Value: Double);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.SetHeaderWordWrapping(const Value: Boolean);
begin
  if FHeaderWordWrapping <> Value then
  begin
    FHeaderWordWrapping := Value;
    UpdateColumn;
  end;
end;

procedure TAdvKanbanBoardColumn.StartEditMode;
begin
  if Assigned(FTableView) then
    FTableView.StartEditMode;
end;

procedure TAdvKanbanBoardColumn.StartFiltering;
begin
  if Assigned(FTableView) then
    FTableView.StartFiltering;
end;

procedure TAdvKanbanBoardColumn.StopEditMode;
begin
  if Assigned(FTableView) then
    FTableView.StopEditMode;
end;

procedure TAdvKanbanBoardColumn.StopFiltering;
begin
  if Assigned(FTableView) then
    FTableView.StopFiltering;
end;

procedure TAdvKanbanBoardColumn.ToggleEditMode;
begin
  if Assigned(FTableView) then
    FTableView.ToggleEditMode;
end;

function TAdvKanbanBoardColumn.GetTableView: TAdvKanbanBoardTableView;
begin
  Result := FTableView;
end;

function TAdvKanbanBoardColumn.IsCollapsedWidthStored: Boolean;
begin
  Result := CollapsedWidth <> 100;
end;

procedure TAdvKanbanBoardColumn.UpdateColumn(AUpdate: Boolean = True; ARealign: Boolean = True);
var
  k: TAdvCustomKanbanBoard;
begin
  k := KanbanBoard;
  if Assigned(k) then
    k.UpdateColumns(AUpdate, ARealign);
end;

procedure TAdvKanbanBoardColumn.UpdateWidth(AWidth: Double);
begin
  FWidth := AWidth;
end;

function TAdvKanbanBoardColumn.XYToItem(X, Y: Single): TAdvKanbanBoardItem;
var
  it: TAdvTableViewItem;
begin
  Result := nil;
  if Assigned(FTableView) then
  begin
    it := FTableView.XYToItem(X, Y);
    if Assigned(it) then
      Result := FindItem(it.Index);
  end;
end;

function TAdvKanbanBoardColumn.XYToItemIndex(X, Y: Single): Integer;
begin
  Result := -1;
  if Assigned(FTableView) then
    Result := FTableView.XYToItemIndex(X, Y);
end;

{ TAdvKanbanBoardColumns }

function TAdvKanbanBoardColumns.Add: TAdvKanbanBoardColumn;
begin
  Result := TAdvKanbanBoardColumn(inherited Add);
end;

constructor TAdvKanbanBoardColumns.Create(AKanbanBoard: TAdvCustomKanbanBoard);
begin
  inherited Create(AKanbanBoard, GetItemClass);
  FKanbanBoard := AKanbanBoard;
end;

function TAdvKanbanBoardColumns.GetItem(Index: Integer): TAdvKanbanBoardColumn;
begin
  Result := TAdvKanbanBoardColumn(inherited Items[Index]);
end;

function TAdvKanbanBoardColumns.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvKanbanBoardColumn;
end;

function TAdvKanbanBoardColumns.Insert(Index: Integer): TAdvKanbanBoardColumn;
begin
  Result := TAdvKanbanBoardColumn(inherited Insert(Index));
end;

function TAdvKanbanBoardColumns.GetKanbanBoard: TAdvCustomKanbanBoard;
begin
  Result := FKanbanBoard;
end;

procedure TAdvKanbanBoardColumns.SetItem(Index: Integer;
  const Value: TAdvKanbanBoardColumn);
begin
  inherited Items[Index] := Value;
end;

{ TAdvKanbanBoardItem }

procedure TAdvKanbanBoardItem.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardItem then
  begin
    FTag := (Source as TAdvKanbanBoardItem).Tag;
    FText := (Source as TAdvKanbanBoardItem).Text;
    FTitle := (Source as TAdvKanbanBoardItem).Title;
    FColor := (Source as TAdvKanbanBoardItem).Color;
    FUseDefaultAppearance := (Source as TAdvKanbanBoardItem).UseDefaultAppearance;
    FExpanded := (Source as TAdvKanbanBoardItem).Expanded;
    FTitleTrimming := (Source as TAdvKanbanBoardItem).TitleTrimming;
    FTitleWordWrapping := (Source as TAdvKanbanBoardItem).TitleWordWrapping;
    FTitleHorizontalTextAlign := (Source as TAdvKanbanBoardItem).TitleHorizontalTextAlign;
    FTitleVerticalTextAlign := (Source as TAdvKanbanBoardItem).TitleVerticalTextAlign;
    FTitleColor := (Source as TAdvKanbanBoardItem).TitleColor;
    FSelectedTitleColor := (Source as TAdvKanbanBoardItem).SelectedTitleColor;
    FDisabledTitleColor := (Source as TAdvKanbanBoardItem).DisabledTitleColor;
    FMarkType := (Source as TAdvKanbanBoardItem).MarkType;
    FMarkColor := (Source as TAdvKanbanBoardItem).MarkColor;
    FMarkColorLeft := (Source as TAdvKanbanBoardItem).MarkColorLeft;
    FMarkColorBottom := (Source as TAdvKanbanBoardItem).MarkColorBottom;
    FMarkColorRight := (Source as TAdvKanbanBoardItem).MarkColorRight;
    FMarkColorTop := (Source as TAdvKanbanBoardItem).MarkColorTop;
    FMarkSizeLeft := (Source as TAdvKanbanBoardItem).MarkSizeLeft;
    FMarkSizeTop := (Source as TAdvKanbanBoardItem).MarkSizeTop;
    FMarkSizeRight := (Source as TAdvKanbanBoardItem).MarkSizeRight;
    FMarkSizeBottom := (Source as TAdvKanbanBoardItem).MarkSizeBottom;
    FHTMLTemplateItems.Assign((Source as TAdvKanbanBoardItem).HTMLTemplateItems);
    FTrimming := (Source as TAdvKanbanBoardItem).Trimming;
    FWordWrapping := (Source as TAdvKanbanBoardItem).WordWrapping;
    FHorizontalTextAlign := (Source as TAdvKanbanBoardItem).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvKanbanBoardItem).VerticalTextAlign;
    FTextColor := (Source as TAdvKanbanBoardItem).TextColor;
    FSelectedTextColor := (Source as TAdvKanbanBoardItem).SelectedTextColor;
    FDisabledTextColor := (Source as TAdvKanbanBoardItem).DisabledTextColor;
    FEnabled := (Source as TAdvKanbanBoardItem).Enabled;
    FVisible := (Source as TAdvKanbanBoardItem).Visible;
    FBitmapName := (Source as TAdvKanbanBoardItem).BitmapName;
    FBitmap.Assign((Source as TAdvKanbanBoardItem).Bitmap);
    FHeight := (Source as TAdvKanbanBoardItem).Height;
    FChecked := (Source as TAdvKanbanBoardItem).Checked;
    FCheckType := (Source as TAdvKanbanBoardItem).CheckType;
    FExpandable := (Source as TAdvKanbanBoardItem).Expandable;
    FMovable := (Source as TAdvKanbanBoardItem).Movable;
    FRounding := (Source as TAdvKanbanBoardItem).Rounding;
    FRoundingCorners := (Source as TAdvKanbanBoardItem).RoundingCorners;
    FMarkCorners := (Source as TAdvKanbanBoardItem).MarkCorners;
    FMarkRounding := (Source as TAdvKanbanBoardItem).MarkRounding;
    FDisabledStrokeColor := (Source as TAdvKanbanBoardItem).DisabledStrokeColor;
    FSelectedStrokeColor := (Source as TAdvKanbanBoardItem).SelectedStrokeColor;
    FStrokeColor := (Source as TAdvKanbanBoardItem).StrokeColor;
    FDisabledColor := (Source as TAdvKanbanBoardItem).DisabledColor;
    FSelectedColor := (Source as TAdvKanbanBoardItem).SelectedColor;
    AssignData(Source);
  end;
end;

procedure TAdvKanbanBoardItem.AssignData(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardItem then
  begin
    FDataString := (Source as TAdvKanbanBoardItem).DataString;
    FDataBoolean := (Source as TAdvKanbanBoardItem).DataBoolean;
    FDataObject := (Source as TAdvKanbanBoardItem).DataObject;
    FDataInteger := (Source as TAdvKanbanBoardItem).DataInteger;
    FDataPointer := (Source as TAdvKanbanBoardItem).DataPointer;
    FDBKey := (Source as TAdvKanbanBoardItem).DBKey;
  end;
end;

procedure TAdvKanbanBoardItem.BitmapChanged(Sender: TObject);
begin
  UpdateItem;
end;

procedure TAdvKanbanBoardItem.Changed(Sender: TObject);
begin
  UpdateItem;
end;

function TAdvKanbanBoardItem.CopyItem(AToColumn: TAdvKanbanBoardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem;
var
  it: TAdvKanbanBoardItem;
begin
  Result := nil;

  if not Assigned(AToColumn) then
    Exit;

  if (AIndex <> -1) then
    it := AToColumn.Items.Insert(AIndex)
  else
    it := AToColumn.Items.Add;

  it.Assign(Self);

  Result := it;
end;

constructor TAdvKanbanBoardItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FColumn := (Collection as TAdvKanbanBoardItems).Column;

  FMovable := True;
  FTextColor := gcBlack;
  FChecked := False;
  FCheckType := kbictNone;
  FColor := gcNull;
  FExpanded := True;
  FExpandable := False;
  FUseDefaultAppearance := True;
  FHeight := -1;
  FEnabled := True;
  FVisible := True;
  FHTMLTemplateItems := TStringList.Create;
  FHTMLTemplateItems.OnChange := TemplateItemsChanged;
  FSelectedTextColor := gcBlack;
  FDisabledTextColor := gcSilver;
  FWordWrapping := true;
  FHorizontalTextAlign := gtaLeading;
  FVerticalTextAlign := gtaCenter;
  FTrimming := gttNone;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FMarkType := [];
  FMarkColor := gcRed;
  FMarkColorLeft := gcNull;
  FMarkColorRight := gcNull;
  FMarkColorBottom := gcNull;
  FMarkColorTop := gcNull;
  FMarkSizeLeft := 4;
  FMarkSizeTop := 4;
  FMarkSizeRight := 4;
  FMarkSizeBottom := 4;
  FTitleColor := gcBlack;
  FSelectedTitleColor := gcBlack;
  FDisabledTitleColor := gcSilver;
  FTitleWordWrapping := true;
  FTitleHorizontalTextAlign := gtaLeading;
  FTitleVerticalTextAlign := gtaCenter;
  FTitleTrimming := gttNone;
  FRounding := 0;
  FMarkRounding := 0;
  FMarkCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FRoundingCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];

  FSelectedStrokeColor := TAdvTreeViewColorSelection;
  FSelectedColor := TAdvTreeViewColorSelection;
  FDisabledColor := gcDarkgray;
  FDisabledStrokeColor := gcDarkgray;
  FStrokeColor := gcDarkgray;

  UpdateItem;
end;

destructor TAdvKanbanBoardItem.Destroy;
begin
  FBitmap.Free;
  FHTMLTemplateItems.Free;
  inherited;
  UpdateItem;
end;

procedure TAdvKanbanBoardItem.SetHeight(const Value: Double);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetHTMLTemplateItems(const Value: TStringList);
begin
  if FHTMLTemplateItems <> Value then
  begin
    FHTMLTemplateItems.Assign(Value);
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkColorLeft(
  const Value: TAdvGraphicsColor);
begin
  if FMarkColorLeft <> Value then
  begin
    FMarkColorLeft := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkColor(
  const Value: TAdvGraphicsColor);
begin
  if FMarkColor <> Value then
  begin
    FMarkColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkSizeLeft(const Value: Single);
begin
  if FMarkSizeLeft <> Value then
  begin
    FMarkSizeLeft := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkSizeTop(const Value: Single);
begin
  if FMarkSizeTop <> Value then
  begin
    FMarkSizeTop := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkSizeRight(const Value: Single);
begin
  if FMarkSizeRight <> Value then
  begin
    FMarkSizeRight := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkSizeBottom(const Value: Single);
begin
  if FMarkSizeBottom <> Value then
  begin
    FMarkSizeBottom := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkType(
  const Value: TAdvKanbanBoardMarkTypes);
begin
  if FMarkType <> Value then
  begin
    FMarkType := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkColorRight(
  const Value: TAdvGraphicsColor);
begin
  if FMarkColorRight <> Value then
  begin
    FMarkColorRight := Value;
    Changed(Self);
  end;
end;

function TAdvKanbanBoardItem.GetPictureContainer: TPictureContainer;
var
  l: TAdvKanbanBoardColumn;
begin
  Result := nil;
  l := Column;
  if Assigned(l) then
    Result := l.PictureContainer;
end;

function TAdvKanbanBoardItem.GetColumn: TAdvKanbanBoardColumn;
begin
  Result := FColumn;
end;

function TAdvKanbanBoardItem.GetTableView: TAdvKanbanBoardTableView;
begin
  Result := nil;
  if Assigned(FColumn) then
    Result := FColumn.FTableView;
end;

function TAdvKanbanBoardItem.GetTableViewItem: TAdvTableViewItem;
var
  t: TAdvKanbanBoardTableView;
begin
  Result := nil;
  t := TableView;
  if Assigned(t) then
    if (Index >= 0) and (Index <= t.Items.Count - 1) then
      Result := t.Items[Index];
end;

procedure TAdvKanbanBoardItem.TemplateItemsChanged(Sender: TObject);
begin
  UpdateItem;
end;

procedure TAdvKanbanBoardItem.LoadFromString(AString: String);
var
  k: Integer;
  a: TStringList;
  s: TStringList;
  y: Integer;
  st: string;
  ps: Integer;
  subst: string;
begin
  a := TStringList.Create;
  try
    st := AString;
    while st <> '' do
    begin
      ps := Pos('{0}', st);
      if ps = 0 then
      begin
        a.Add(st);
        Break;
      end;
      subst := Copy(st, 1, ps - 1);
      a.Add(subst);
      Delete(st, 1, ps - 1 + Length('{0}'));
    end;

    for K := 0 to a.Count - 1 do
    begin
      if a[K] <> '' then
      begin
        if K = 0 then
          Text := a[K]
        else if K = 1 then
        begin
          s := TStringList.Create;
          try
            TAdvUtils.Split(';', a[K], s);
            for y := 0 to s.Count - 1 do
            begin
              if s[y] <> '' then
              begin
                if y = 0 then
                  Checked := StrToBool(s[y])
                else if y = 1 then
                  BitmapName := s[y]
                else if y = 2 then
                  TAdvUtils.LoadBitmapFromHexStr(s[y], Bitmap);
              end;
            end;
          finally
            s.Free;
          end;
        end
        else if K = 2 then
          Enabled := StrToBool(a[K]);
      end;
    end;
  finally
    a.Free;
  end;
end;

function TAdvKanbanBoardItem.MoveItem(AToColumn: TAdvKanbanBoardColumn; AIndex: Integer = -1): TAdvKanbanBoardItem;
var
  c: TAdvKanbanBoardColumn;
begin
  Result := CopyItem(AToColumn, AIndex);
  c := Column;
  if Assigned(c) then
    c.RemoveItem(Self);
end;

function TAdvKanbanBoardItem.GetStrippedHTMLText: String;
begin
  Result := TAdvUtils.HTMLStrip(GetText);
end;

function TAdvKanbanBoardItem.GetText: string;
var
  ht: string;
  s: string;
  tv: TAdvKanbanBoardTableView;
  it: TAdvTableViewItem;
begin
  s := '';
  tv := TableView;
  if Assigned(tv) then
  begin
    ht := tv.ItemAppearance.HTMLTemplate;
    if ht = '' then
      s := Text
    else
    begin
      it := GetTableViewItem;
      if Assigned(it) then
      begin
        tv.DoGetHTMLTemplate(it, ht);
        s := tv.HTMLReplace(ht, it);
      end;
    end;
  end;

  Result := s;
end;

function TAdvKanbanBoardItem.IsHeightStored: Boolean;
begin
  Result := Height <> -1;
end;

function TAdvKanbanBoardItem.IsMarkSizeLeftStored: Boolean;
begin
  Result := MarkSizeLeft <> 4;
end;

function TAdvKanbanBoardItem.IsMarkSizeTopStored: Boolean;
begin
  Result := MarkSizeTop <> 4;
end;

function TAdvKanbanBoardItem.IsMarkSizeRightStored: Boolean;
begin
  Result := MarkSizeRight <> 4;
end;

function TAdvKanbanBoardItem.IsMarkSizeBottomStored: Boolean;
begin
  Result := MarkSizeBottom <> 4;
end;

function TAdvKanbanBoardItem.IsSelected: Boolean;
var
  i: Integer;
  tv: TAdvKanbanBoardTableView;
begin
  Result := False;
  tv := TableView;
  if Assigned(tv) then
  begin
    for I := 0 to tv.SelectedItemCount - 1 do
    begin
      if tv.SelectedItems[I] = GetTableViewItem then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TAdvKanbanBoardItem.SaveToString(ATextOnly: Boolean): String;
var
  s: String;
  tv: TAdvKanbanBoardTableView;
begin
  tv := TableView;
  if not Assigned(tv) then
    Exit;

  Result := '';
  Result := Result + Text;

  if not ATextOnly then
    Result := Result + '{0}';

  if not ATextOnly then
  begin
    Result := Result + BoolToStr(Checked) + ';';
    Result := Result + BitmapName + ';';
    Result := Result + TAdvUtils.SaveBitmapToHexStr(Bitmap) + ';';
    Result := Result + s;
  end;

  if not ATextOnly then
  begin
    Result := Result + '{0}';
    Result := Result + BoolToStr(Enabled);
  end;
end;

procedure TAdvKanbanBoardItem.SetChecked(const Value: Boolean);
var
  c: TAdvKanbanBoardColumn;
  k: TAdvKanbanBoardTableView;
  n: TAdvTreeViewVirtualNode;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    c := FColumn;
    if Assigned(c) then
    begin
      k := c.FTableView;
      if Assigned(k) then
      begin
        n := k.GetNodeForItemIndex(Index);
        if Assigned(n) and (n.CheckStates[0] <> FChecked) then
        begin
          if FChecked then
            k.TreeView.CheckVirtualNode(n, 0)
          else
            k.TreeView.UnCheckVirtualNode(n, 0);
        end;
      end;
    end;
  end;
end;

procedure TAdvKanbanBoardItem.SetCheckType(
  const Value: TAdvKanbanBoardItemCheckType);
begin
  if FCheckType <> Value then
  begin
    FCheckType := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetExpandable(const Value: Boolean);
begin
  if FExpandable <> Value then
  begin
    FExpandable := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetRoundingCorners(const Value: TAdvGraphicsCorners);
begin
  if FRoundingCorners <> Value then
  begin
    FRoundingCorners := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SelectItem;
var
  c: TAdvKanbanBoardColumn;
begin
  c := Column;
  if Assigned(c) then
    c.SelectedItem := Self;
end;

procedure TAdvKanbanBoardItem.SetBitmap(const Value: TAdvBitmap);
begin
  if FBitmap <> Value then
  begin
    FBitmap.Assign(Value);
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetBitmapName(const Value: string);
begin
  if FBitmapName <> Value then
  begin
    FBitmapName := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkColorBottom(
  const Value: TAdvGraphicsColor);
begin
  if FMarkColorBottom <> Value then
  begin
    FMarkColorBottom := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetDisabledColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetDisabledStrokeColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledStrokeColor <> Value then
  begin
    FDisabledStrokeColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetDisabledTextColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetDisabledTitleColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledTitleColor <> Value then
  begin
    FDisabledTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetSelectedColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetSelectedStrokeColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedStrokeColor <> Value then
  begin
    FSelectedStrokeColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetSelectedTextColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetSelectedTitleColor(
  const Value: TAdvGraphicsColor);
begin
  if FSelectedTitleColor <> Value then
  begin
    FSelectedTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetStrokeColor(
  const Value: TAdvGraphicsColor);
begin
  if FStrokeColor <> Value then
  begin
    FStrokeColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTextColor(const Value: TAdvGraphicsColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitle(const Value: String);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitleColor(
  const Value: TAdvGraphicsColor);
begin
  if FTitleColor <> Value then
  begin
    FTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitleHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FTitleHorizontalTextAlign <> Value then
  begin
    FTitleHorizontalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitleTrimming(
  const Value: TAdvGraphicsTextTrimming);
begin
  if FTitleTrimming <> Value then
  begin
    FTitleTrimming := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitleVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FTitleVerticalTextAlign <> Value then
  begin
    FTitleVerticalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTitleWordWrapping(const Value: Boolean);
begin
  if FTitleWordWrapping <> Value then
  begin
    FTitleWordWrapping := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkColorTop(
  const Value: TAdvGraphicsColor);
begin
  if FMarkColorTop <> Value then
  begin
    FMarkColorTop := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkCorners(const Value: TAdvGraphicsCorners);
begin
  if FMarkCorners <> Value then
  begin
    FMarkCorners := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetMarkRounding(const Value: Integer);
begin
  if FMarkRounding <> Value then
  begin
    FMarkRounding := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetTrimming(const Value: TAdvGraphicsTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetUseDefaultAppearance(const Value: Boolean);
begin
  if FUseDefaultAppearance <> Value then
  begin
    FUseDefaultAppearance := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    UpdateItem;
  end;
end;

procedure TAdvKanbanBoardItem.UpdateItem;
var
  c: TAdvKanbanBoardColumn;
begin
  c := FColumn;
  if Assigned(c) and not c.FBlockUpdate then
    c.UpdateColumn;
end;

{ TAdvKanbanBoardItems }

function TAdvKanbanBoardItems.Add: TAdvKanbanBoardItem;
begin
  Result := TAdvKanbanBoardItem(inherited Add);
end;

procedure TAdvKanbanBoardItems.Clear;
var
  k: TAdvKanbanBoardTableView;
begin
  k := TableView;
  if Assigned(k) then
    k.BeginUpdate;
  inherited Clear;
  if Assigned(k) then
    k.EndUpdate;
end;

function TAdvKanbanBoardItems.GetColumn: TAdvKanbanBoardColumn;
begin
  Result := FColumn;
end;

function TAdvKanbanBoardItems.Compare(AItem1, AItem2: TAdvKanbanBoardItem;
  ACaseSensitive: Boolean; ASortingMode: TAdvKanbanBoardItemsSortMode): Integer;
var
  c: TAdvKanbanBoardColumn;
  it1, it2: TAdvKanbanBoardItem;
begin
  Result := 0;

  c := FColumn;
  if Assigned(c) and Assigned(c.FKanbanBoard) then
  begin
    if not ACaseSensitive then
      Result := AnsiCompareStr(UpperCase(AItem1.StrippedHTMLText), UpperCase(AItem2.StrippedHTMLText))
    else
      Result := AnsiCompareStr(AItem1.StrippedHTMLText, AItem2.StrippedHTMLText);

    case ASortingMode of
      kbismDescending: Result := Result * -1;
    end;

    it1 := AItem1;
    it2 := AItem2;

    c.FKanbanBoard.DoItemCompare(c, it1, it2, Result);
  end;
end;

constructor TAdvKanbanBoardItems.Create(AColumn: TAdvKanbanBoardColumn);
begin
  inherited Create(AColumn, GetItemClass);
  FColumn := AColumn;
end;

function TAdvKanbanBoardItems.GetItem(Index: Integer): TAdvKanbanBoardItem;
begin
  Result := TAdvKanbanBoardItem(inherited Items[Index]);
end;

function TAdvKanbanBoardItems.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvKanbanBoardItem;
end;

function TAdvKanbanBoardItems.Insert(Index: Integer): TAdvKanbanBoardItem;
begin
  Result := TAdvKanbanBoardItem(inherited Insert(Index));
end;

function TAdvKanbanBoardItems.GetTableView: TAdvKanbanBoardTableView;
begin
  Result := nil;
  if Assigned(FColumn) then
    Result := FColumn.FTableView;
end;

type
  {$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    {%H-}FItemClass: TCollectionItemClass;
    {$IFDEF LCLWEBLIB}
    FItems: TFPList;
    {$ENDIF}
    {$IFNDEF LCLWEBLIB}
    FItems: TList<TCollectionItem>;
    {$ENDIF}
  end;
  {$HINTS ON}

procedure TAdvKanbanBoardItems.QuickSort(L, R: Integer; ACaseSensitive: Boolean;
  ASortingMode: TAdvKanbanBoardItemsSortMode);
var
  I, J, p: Integer;
  {$IFDEF LCLWEBLIB}
  SortList: TFPList;

  procedure ExchangeItems(Index1, Index2: Integer);
  var
    {$IFDEF WEBLIB}
    Save: JSValue;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    Save: Pointer;
    {$ENDIF}
  begin
    Save := SortList.Items[Index1];
    SortList.Items[Index1] := SortList.Items[Index2];
    SortList.Items[Index2] := Save;
  end;
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  SortList: TList<TCollectionItem>;

  procedure ExchangeItems(Index1, Index2: Integer);
  var
    Save: TCollectionItem;
  begin
    Save := SortList[Index1];
    SortList[Index1] := SortList[Index2];
    SortList[Index2] := Save;
    Save.Index := Index2;
  end;
  {$ENDIF}
begin
  //This cast allows us to get at the private elements in the base class
  {$IFNDEF WEBLIB}
  SortList := {%H-}TShadowedCollection(Self).FItems;
  {$ENDIF}
  {$IFDEF WEBLIB}
  SortList := Self.FPList;
  {$ENDIF}

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Items[I], Items[P], ACaseSensitive, ASortingMode) < 0 do
        Inc(I);
      while Compare(Items[J], Items[P], ACaseSensitive, ASortingMode) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, ACaseSensitive, ASortingMode);
    L := I;
  until I >= R;
end;

procedure TAdvKanbanBoardItems.SetItem(Index: Integer;
  const Value: TAdvKanbanBoardItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvKanbanBoardItems.Sort(ACaseSensitive: Boolean;
  ASortingMode: TAdvKanbanBoardItemsSortMode);
var
  c: TAdvKanbanBoardColumn;
begin
  c := FColumn;
  if Assigned(c) and Assigned(c.FKanbanBoard) then
  begin
    c.FKanbanBoard.BeginUpdate;
    if Count > 1 then
      QuickSort(0, Pred(Count), ACaseSensitive, ASortingMode);
    c.FKanbanBoard.EndUpdate;
  end;
end;

{ TAdvKanbanBoardColumnsAppearance }

procedure TAdvKanbanBoardColumnsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardColumnsAppearance then
  begin
    FMargins.Assign((Source as TAdvKanbanBoardColumnsAppearance).Margins);
    FSpacing := (Source as TAdvKanbanBoardColumnsAppearance).Spacing;
    FHeaderFont.Assign((Source as TAdvKanbanBoardColumnsAppearance).HeaderFont);
    FHeaderFill.Assign((Source as TAdvKanbanBoardColumnsAppearance).HeaderFill);
    FHeaderStroke.Assign((Source as TAdvKanbanBoardColumnsAppearance).HeaderStroke);
    FHeaderSize := (Source as TAdvKanbanBoardColumnsAppearance).HeaderSize;
    FFooterFont.Assign((Source as TAdvKanbanBoardColumnsAppearance).FooterFont);
    FFooterFill.Assign((Source as TAdvKanbanBoardColumnsAppearance).FooterFill);
    FFooterStroke.Assign((Source as TAdvKanbanBoardColumnsAppearance).FooterStroke);
    FFooterSize := (Source as TAdvKanbanBoardColumnsAppearance).FooterSize;
  end;
end;

procedure TAdvKanbanBoardColumnsAppearance.Changed(Sender: TObject);
begin
  if Assigned(FKanbanBoard) then
    FKanbanBoard.UpdateColumns;
end;

constructor TAdvKanbanBoardColumnsAppearance.Create(
  AKanbanBoard: TAdvCustomKanbanBoard);
begin
  FKanbanBoard := AKanbanBoard;
  FMargins := TAdvMargins.Create(RectF(10, 10, 10, 10));
  FMargins.OnChange := Changed;
  FSpacing := 20;

  FFooterFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FFooterStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FHeaderFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FHeaderStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);

  FHeaderFont := TAdvGraphicsFont.Create;
  FHeaderFont.Color := gcBlack;
  FFooterFont := TAdvGraphicsFont.Create;
  FFooterFont.Color := gcBlack;

  FFooterFill.OnChanged := Changed;
  FFooterStroke.OnChanged := Changed;
  FHeaderFill.OnChanged := Changed;
  FHeaderStroke.OnChanged := Changed;

  FFill.OnChanged := Changed;
  FStroke.OnChanged := Changed;

  FHeaderFont.OnChanged := Changed;
  FFooterFont.OnChanged := Changed;

  FFooterSize := 35;
  FHeaderSize := 35;
end;

destructor TAdvKanbanBoardColumnsAppearance.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FMargins.Free;
  FHeaderFont.Free;
  FHeaderFill.Free;
  FHeaderStroke.Free;
  FFooterFont.Free;
  FFooterFill.Free;
  FFooterStroke.Free;
  inherited;
end;

function TAdvKanbanBoardColumnsAppearance.IsFooterSizeStored: Boolean;
begin
  Result := FooterSize <> 35;
end;

function TAdvKanbanBoardColumnsAppearance.IsHeaderSizeStored: Boolean;
begin
  Result := HeaderSize <> 35;
end;

function TAdvKanbanBoardColumnsAppearance.IsSpacingStored: Boolean;
begin
  Result := Spacing <> 20;
end;

procedure TAdvKanbanBoardColumnsAppearance.SetFill(
  const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetFooterFill(
  const Value: TAdvGraphicsFill);
begin
  FFooterFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetFooterFont(
  const Value: TAdvGraphicsFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetFooterSize(
  const Value: Single);
begin
  if FFooterSize <> Value then
  begin
    FFooterSize := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardColumnsAppearance.SetFooterStroke(
  const Value: TAdvGraphicsStroke);
begin
  FFooterStroke.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetMargins(
  const Value: TAdvMargins);
begin
  FMargins.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetSpacing(const Value: Single);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardColumnsAppearance.SetStroke(
  const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetHeaderFill(
  const Value: TAdvGraphicsFill);
begin
  FHeaderFill.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetHeaderFont(
  const Value: TAdvGraphicsFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TAdvKanbanBoardColumnsAppearance.SetHeaderSize(
  const Value: Single);
begin
  if FHeaderSize <> Value then
  begin
    FHeaderSize := Value;
    Changed(Self);
  end;
end;

procedure TAdvKanbanBoardColumnsAppearance.SetHeaderStroke(
  const Value: TAdvGraphicsStroke);
begin
  FHeaderStroke.Assign(Value);
end;

{ TAdvKanbanBoardItemsAppearance }

procedure TAdvKanbanBoardItemsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardItemsAppearance then
  begin
    Font.Assign((Source as TAdvKanbanBoardItemsAppearance).Font);
    Fill.Assign((Source as TAdvKanbanBoardItemsAppearance).Fill);
    Stroke.Assign((Source as TAdvKanbanBoardItemsAppearance).Stroke);
    SelectedFill.Assign((Source as TAdvKanbanBoardItemsAppearance).SelectedFill);
    SelectedStroke.Assign((Source as TAdvKanbanBoardItemsAppearance).SelectedStroke);
    DisabledFill.Assign((Source as TAdvKanbanBoardItemsAppearance).DisabledFill);
    DisabledStroke.Assign((Source as TAdvKanbanBoardItemsAppearance).DisabledStroke);
    HeightMode := (Source as TAdvKanbanBoardItemsAppearance).HeightMode;
    FixedHeight := (Source as TAdvKanbanBoardItemsAppearance).FixedHeight;
    FHTMLTemplate := (Source as TAdvKanbanBoardItemsAppearance).HTMLTemplate;
    FSpacing := (Source as TAdvKanbanBoardItemsAppearance).Spacing;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.Changed;
begin
  FKanbanBoard.UpdateColumns;
end;

constructor TAdvKanbanBoardItemsAppearance.Create(AKanbanBoard: TAdvCustomKanbanBoard);
begin
  FKanbanBoard := AKanbanBoard;
  FShowFocus := True;
  FFixedHeight := 25;
  FHeight := -1;
  FHeightMode := kbhmVariable;
  FSpacing := 10;

  FFill :=  TAdvGraphicsFill.Create(gfkSolid, gcWhitesmoke);
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkGray);
  FSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvTreeViewColorSelection);
  FSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvTreeViewColorSelection);
  FDisabledFill := TAdvGraphicsFill.Create(gfkSolid, gcDarkGray);
  FDisabledStroke := TAdvGraphicsStroke.Create(gskNone, gcDarkGray);

  FFont := TAdvGraphicsFont.Create;
  if Assigned(FKanbanBoard) and FKanbanBoard.IsDesignTime then
    FFont.Color := gcBlack;

  FTitleFont := TAdvGraphicsFont.Create;
  if Assigned(FKanbanBoard) and FKanbanBoard.IsDesignTime then
    FTitleFont.Color := gcBlack;

  FFill.OnChanged := FillChanged;
  FStroke.OnChanged := StrokeChanged;

  FSelectedFill.OnChanged := FillChanged;
  FSelectedStroke.OnChanged := StrokeChanged;

  FDisabledFill.OnChanged := FillChanged;
  FDisabledStroke.OnChanged := StrokeChanged;

  FFont.OnChanged := FontChanged;
  FTitleFont.OnChanged := FontChanged;

  FMargins := TAdvMargins.Create(RectF(10, 10, 10, 10));
  FMargins.OnChange := MarginsChanged;
end;

destructor TAdvKanbanBoardItemsAppearance.Destroy;
begin
  FMargins.Free;
  FFill.Free;
  FStroke.Free;
  FFont.Free;
  FTitleFont.Free;

  FSelectedFill.Free;
  FSelectedStroke.Free;

  FDisabledFill.Free;
  FDisabledStroke.Free;

  inherited;
end;

procedure TAdvKanbanBoardItemsAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvKanbanBoardItemsAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvKanbanBoardItemsAppearance.IsHeightStored: Boolean;
begin
  Result := Height <> -1;
end;

function TAdvKanbanBoardItemsAppearance.IsSpacingStored: Boolean;
begin
  Result := Spacing <> 10;
end;

procedure TAdvKanbanBoardItemsAppearance.MarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvKanbanBoardItemsAppearance.SetDisabledFill(
  const Value: TAdvGraphicsFill);
begin
  FDisabledFill.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetDisabledStroke(
  const Value: TAdvGraphicsStroke);
begin
  FDisabledStroke.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetFill(
  const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetFixedHeight(const Value: Double);
begin
  if FFixedHeight <> Value then
  begin
    FFixedHeight := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetFont(
  const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetHeight(const Value: Double);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetHeightMode(
  const Value: TAdvKanbanBoardItemHeightMode);
begin
  if FHeightMode <> Value then
  begin
    FHeightMode := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetHTMLTemplate(
  const Value: string);
begin
  if FHTMLTemplate <> Value then
  begin
    FHTMLTemplate := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetMargins(
  const Value: TAdvMargins);
begin
  FMargins.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetSelectedFill(
  const Value: TAdvGraphicsFill);
begin
  FSelectedFill.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetSelectedStroke(
  const Value: TAdvGraphicsStroke);
begin
  FSelectedStroke.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetSpacing(const Value: Double);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardItemsAppearance.SetStroke(
  const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.SetTitleFont(
  const Value: TAdvGraphicsFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TAdvKanbanBoardItemsAppearance.StrokeChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvKanbanBoardInteraction }

procedure TAdvKanbanBoardInteraction.Assign(Source: TPersistent);
begin
  if (Source is TAdvTableViewInteraction) then
  begin
    FMultiSelect := (Source as TAdvKanbanBoardInteraction).MultiSelect;
    FTouchScrolling := (Source as TAdvKanbanBoardInteraction).TouchScrolling;
    FSwipeBounceGesture := (Source as TAdvKanbanBoardInteraction).SwipeBounceGesture;
    FDragDropMode := (Source as TAdvKanbanBoardInteraction).DragDropMode;
    FMouseEditMode := (Source as TAdvKanbanBoardInteraction).MouseEditMode;
    FKeyboardEdit := (Source as TAdvKanbanBoardInteraction).KeyboardEdit;
    FEditing := (Source as TAdvKanbanBoardInteraction).Editing;
    FDirectDrag := (Source as TAdvKanbanBoardInteraction).DirectDrag;
  end;
end;

procedure TAdvKanbanBoardInteraction.Changed;
begin
  if Assigned(FKanbanBoard) then
    FKanbanBoard.UpdateColumns(False);
end;

constructor TAdvKanbanBoardInteraction.Create(AKanbanBoard: TAdvCustomKanbanBoard);
begin
  FAutoOpenURL := True;
  FKanbanBoard := AKanbanBoard;
  FSwipeBounceGesture := False;
  FTouchScrolling := True;
  FMultiSelect := False;
  FDragDropMode := kbdmNone;
  FMouseEditMode := kbemSingleClickOnSelectedItem;
  FKeyboardEdit := True;
  FEditing := False;
  FDirectDrag := False;
end;

procedure TAdvKanbanBoardInteraction.SetAutoOpenURL(const Value: Boolean);
begin
  if FAutoOpenURL <> Value then
  begin
    FAutoOpenURL := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetDirectDrag(const Value: Boolean);
begin
  if FDirectDrag <> Value then
  begin
    FDirectDrag := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetDragDropMode(
  const Value: TAdvKanbanBoardDragDropMode);
begin
  if FDragDropMode <> Value then
  begin
    FDragDropMode := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetEditing(const Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetKeyboardEdit(const Value: Boolean);
begin
  if FKeyboardEdit <> Value then
  begin
    FKeyboardEdit := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetMouseEditMode(
  const Value: TAdvKanbanBoardMouseEditMode);
begin
  if FMouseEditMode <> Value then
  begin
    FMouseEditMode := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetSwipeBounceGesture(
  const Value: Boolean);
begin
  if FSwipeBounceGesture <> Value then
  begin
    FSwipeBounceGesture := Value;
    Changed;
  end;
end;

procedure TAdvKanbanBoardInteraction.SetTouchScrolling(const Value: Boolean);
begin
  if FTouchScrolling <> Value then
  begin
    FTouchScrolling := Value;
    Changed;
  end;
end;

{ TAdvKanbanBoardAdapter }

constructor TAdvKanbanBoardAdapter.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FActive := False;
  if IsDesignTime and Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TAdvCustomKanbanBoard) then
      begin
        KanbanBoard := AOwner.Components[i] as TAdvCustomKanbanBoard;
        Break;
      end;
    end;
  end;
end;

function TAdvKanbanBoardAdapter.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TAdvKanbanBoardAdapter.LoadItems;
begin
  if not Assigned(FKanbanBoard) then
    Exit;

  FKanbanBoard.ClearItems;

  if Active then
    GetItems;
end;

procedure TAdvKanbanBoardAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FKanbanBoard) then
    FKanbanBoard := nil;
end;

procedure TAdvKanbanBoardAdapter.SetActive(const Value: boolean);
begin
  if (Value <> FActive) then
  begin
    FActive := Value;
    LoadItems;
  end;
end;

procedure TAdvKanbanBoardAdapter.UpdateItems;
begin
  Active := false;
  Active := true;
end;


end.
