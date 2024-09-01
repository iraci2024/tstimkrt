{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2022                               }
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

unit AdvTableView;

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
  Classes, AdvTreeViewBase, AdvTreeView, AdvPersistence, AdvGraphicsStyles, AdvTreeViewData, AdvCustomControl,
  AdvTypes, AdvGraphics, AdvCustomTreeView, PictureContainer,
  AdvGraphicsTypes, AdvEdit, AdvToolBarEx, Controls, ExtCtrls
  {$IFDEF FNCLIB}
  , AdvControlPicker
  {$ENDIF}
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
  ,TypInfo
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 2; // Build nr.
  CLP_FMT = '#TableView#';
  SWIPEOFFSET = 30;

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : SwipeBounceGesture property to toggle bounce effect when swiping up/down on the list
  //          : Fixed : Issue with lookup category
  // v1.0.2.0 : New : KeyboardEscapeMode property to clear or stop filtering
  //          : Improved : CategoryID can be a number that doesn't start from 0
  // v1.0.2.1 : Fixed : Issue with badge drawing
  // v1.0.2.2 : Fixed : Issue with showing filter button
  // v1.0.2.3 : Fixed : Issue with filtering condition
  // v1.0.2.4 : Fixed : Issue with scrolling to item with categories
  // v1.0.2.5 : Fixed : Issue with resizing
  // v1.0.3.0 : New : OnHeaderAnchorClick, OnFooterAnchorClick and OnItemTitleAnchorClick events implemented
  // v1.0.3.1 : Fixed : Issue with swiping to see more options in combination with LongPressToEdit property.
  //          : Fixed : Issue with anchor click when item was selected
  // v1.0.3.2 : Fixed : Issue with text centering in accessory due to word-wrapping.
  //          : Fixed : Issue with OnClick event not triggered
  // v1.0.3.3 : Fixed : Issue with Anchor if font was changed
  // v1.0.3.4 : Fixed : Issue with detail view event handler being called when view is not visible yet
  // v1.0.4.0 : New : Interface for AdvControlPicker implemented
  // v1.0.4.1 : Fixed : Issue with back button not visible in TMS WEB Core
  // v1.0.4.2 : Fixed : Issue in Delphi 11 with begin and end scene for CreateBitmapCanvas
  // v1.0.4.3 : Fixed : Issue with OnBeforeItemShowDetailControl event when calling ShowDetailControl programmatically
  // v1.0.4.4 : Fixed : Issue with returning existing category for category IDs not yet existing
  // v1.0.5.0 : New : GlobalFont interface implemented
  //          : New : Update initial look
  //          : Fixed : Issue with returning ItemIndex between BeginUpdate & EndUpdate
  // v1.0.5.1 : Fixed : Issue with index out of bounds when dragging item
  // v1.0.5.2 : Fixed : Issue with designtime Header visiblity in Lazarus

type
  TAdvTableViewItemAccessory = (tviaNone, tviaDetail, tviaProgress, tviaButton, tviaBadge, tviaCustom);
  TAdvTableViewItemAccessories = set of TAdvTableViewItemAccessory;

const
  ClickAccessories = [tviaButton, tviaCustom];

type
  TAdvCustomTableView = class;
  TAdvTableViewInteraction = class;

  TAdvTableViewCategory = class;

  TAdvTableViewItemCheckType = (tvictNone, tvictCheckBox, tvictRadioButton);

  TAdvTableViewItem = class(TCollectionItem)
  private
    FRow: Integer;
    FTag: NativeInt;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FTableView: TAdvCustomTableView;
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
    FCategoryID: integer;
    FDetailControl: TControl;
    FAccessory: TAdvTableViewItemAccessory;
    FAccessoryWidth: Single;
    FAccessoryProgress: Single;
    FAccessoryHeight: Single;
    FAccessoryText: String;
    FCheckType: TAdvTableViewItemCheckType;
    FAccessoryFontColor: TAdvGraphicsColor;
    FAccessoryBorderColor: TAdvGraphicsColor;
    FAccessoryColor: TAdvGraphicsColor;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FEnableMoreOptions: Boolean;
    FTitleTrimming: TAdvGraphicsTextTrimming;
    FTitleHorizontalTextAlign: TAdvGraphicsTextAlign;
    FTitle: String;
    FDisabledTitleColor: TAdvGraphicsColor;
    FTitleWordWrapping: Boolean;
    FSelectedTitleColor: TAdvGraphicsColor;
    FTitleColor: TAdvGraphicsColor;
    FTitleVerticalTextAlign: TAdvGraphicsTextAlign;
    FTitleExpanded: Boolean;
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
    procedure SetCategoryID(const Value: integer);
    procedure SetDetailControl(const Value: TControl);
    procedure SetAccessory(const Value: TAdvTableViewItemAccessory);
    function IsAccessoryWidthStored: Boolean;
    procedure SetAccessoryWidth(const Value: Single);
    function IsAccessoryProgressStored: Boolean;
    procedure SetAccessoryProgress(const Value: Single);
    function IsAccessoryHeightStored: Boolean;
    procedure SetAccessoryHeight(const Value: Single);
    procedure SetAccessoryText(const Value: String);
    procedure SetCheckType(const Value: TAdvTableViewItemCheckType);
    procedure SetAccessoryBorderColor(const Value: TAdvGraphicsColor);
    procedure SetAccessoryColor(const Value: TAdvGraphicsColor);
    procedure SetAccessoryFontColor(const Value: TAdvGraphicsColor);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetDisabledTitleColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedTitleColor(const Value: TAdvGraphicsColor);
    procedure SetTitle(const Value: String);
    procedure SetTitleColor(const Value: TAdvGraphicsColor);
    procedure SetTitleHorizontalTextAlign(
      const Value: TAdvGraphicsTextAlign);
    procedure SetTitleTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetTitleVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTitleWordWrapping(const Value: Boolean);
    procedure SetTitleExpanded(const Value: Boolean);
  protected
    procedure UpdateItem;
    procedure Changed(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
    procedure TemplateItemsChanged(Sender: TObject);
  public
    function GetTableView: TAdvCustomTableView;
    function GetText: string;
    function GetTitle: string;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function SaveToString(ATextOnly: Boolean = True): String; virtual;
    function IsSelected: Boolean; virtual;
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
    property TableView: TAdvCustomTableView read GetTableView;
    procedure ShowDetailControl; virtual;
    procedure HideDetailControl; virtual;
    property Title: String read FTitle write SetTitle;
    property TitleExpanded: Boolean read FTitleExpanded write SetTitleExpanded default True;
    property TitleWordWrapping: Boolean read FTitleWordWrapping write SetTitleWordWrapping default True;
    property TitleTrimming: TAdvGraphicsTextTrimming read FTitleTrimming write SetTitleTrimming default gttNone;
    property TitleHorizontalTextAlign: TAdvGraphicsTextAlign read FTitleHorizontalTextAlign write SetTitleHorizontalTextAlign default gtaLeading;
    property TitleVerticalTextAlign: TAdvGraphicsTextAlign read FTitleVerticalTextAlign write SetTitleVerticalTextAlign default gtaLeading;
    property TitleColor: TAdvGraphicsColor read FTitleColor write SetTitleColor default gcBlack;
    property SelectedTitleColor: TAdvGraphicsColor read FSelectedTitleColor write SetSelectedTitleColor default gcWhite;
    property DisabledTitleColor: TAdvGraphicsColor read FDisabledTitleColor write SetDisabledTitleColor default gcSilver;
  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property DetailControl: TControl read FDetailControl write SetDetailControl;
    property Text: String read FText write SetText;
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
    property CategoryID: integer read FCategoryID write SetCategoryID default -1;
    property Tag: NativeInt read FTag write FTag default 0;
    property AccessoryWidth: Single read FAccessoryWidth write SetAccessoryWidth stored IsAccessoryWidthStored nodefault;
    property AccessoryHeight: Single read FAccessoryHeight write SetAccessoryHeight stored IsAccessoryHeightStored nodefault;
    property Accessory: TAdvTableViewItemAccessory read FAccessory write SetAccessory default tviaNone;
    property AccessoryProgress: Single read FAccessoryProgress write SetAccessoryProgress stored IsAccessoryProgressStored nodefault;
    property AccessoryText: String read FAccessoryText write SetAccessoryText;
    property AccessoryColor: TAdvGraphicsColor read FAccessoryColor write SetAccessoryColor default gcNull;
    property AccessoryBorderColor: TAdvGraphicsColor read FAccessoryBorderColor write SetAccessoryBorderColor default gcNull;
    property AccessoryFontColor: TAdvGraphicsColor read FAccessoryFontColor write SetAccessoryFontColor default gcNull;
    property CheckType: TAdvTableViewItemCheckType read FCheckType write SetCheckType default tvictNone;
    property EnableMoreOptions: Boolean read FEnableMoreOptions write FEnableMoreOptions default True;
  end;

  TAdvTableViewItemsSortMode = (tvismAscending, tvismDescending);
  TAdvTableViewItemsSortKind = (tviskNone, tviskAscending, tviskDescending);

  {$IFDEF WEBLIB}
  TAdvTableViewItems = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvTableViewItem>)
  {$ENDIF}
  private
    FTableView: TAdvCustomTableView;
    function GetItem(Index: Integer): TAdvTableViewItem;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    function Compare(AItem1, AItem2: TAdvTableViewItem; ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewItemsSortMode = tvismAscending): Integer; virtual;
    procedure QuickSort(L, R: Integer; ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewItemsSortMode = tvismAscending); virtual;
  public
    function TableView: TAdvCustomTableView;
    constructor Create(ATableView: TAdvCustomTableView);
    function Add: TAdvTableViewItem; virtual;
    function Insert(Index: Integer): TAdvTableViewItem;
    procedure Clear; virtual;
    procedure Sort(ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewItemsSortMode = tvismAscending);
    property Items[Index: Integer]: TAdvTableViewItem read GetItem write SetItem; default;
  end;

  TAdvTableViewLookup = class(TPersistent)
  private
    FOwner: TAdvTableViewInteraction;
    function GetAutoSelect: Boolean;
    function GetCaseSensitive: Boolean;
    function GetEnabled: Boolean;
    function GetIncremental: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetIncremental(const Value: Boolean);
  public
    constructor Create(AOwner: TAdvTableViewInteraction);
    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive default False;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Incremental: Boolean read GetIncremental write SetIncremental default True;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
  end;

  TAdvTableViewFiltering = class(TPersistent)
  private
    FOwner: TAdvTableViewInteraction;
    function GetDropDownHeight: integer;
    function GetDropDownWidth: integer;
    function GetEnabled: Boolean;
    procedure SetDropDownHeight(const Value: integer);
    procedure SetDropDownWidth(const Value: integer);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(AOwner: TAdvTableViewInteraction);
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default False;
    property DropDownWidth: integer read GetDropDownWidth write SetDropDownWidth default 100;
    property DropDownHeight: integer read GetDropDownHeight write SetDropDownHeight default 120;
  end;

  TAdvTableViewMouseEditMode = (tvmemDoubleClick, tvmemSingleClick, tvmemSingleClickOnSelectedItem);
  TAdvTableViewClipboardMode = (tvcmNone, tvcmTextOnly, tvcmFull);
  TAdvTableViewDragDropMode = (tvdmNone, tvdmMove, tvdmCopy);
  TAdvTableViewSorting = (tvcsNone, tvcsNormal, tvcsNormalCaseSensitive);
  TAdvTableViewShowDetailMode = (tvdtNone, tvdtAfterSelectItem);
  TAdvTableViewKeyboardEscapeMode = (tvkeNone, tvkeClearEdit, tvkeStopFilter);

  TAdvTableViewInteraction = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FLookup: TAdvTableViewLookup;
    FFiltering: TAdvTableViewFiltering;
    FShowDetailMode: TAdvTableViewShowDetailMode;
    FShowEditButton: Boolean;
    FShowFilterButton: Boolean;
    FAccessoryClickDetection: TAdvTableViewItemAccessories;
    FLongPressToEdit: Boolean;
    FSwipeToShowMoreOptions: Boolean;
    FSwipeBounceGesture: Boolean;
    FKeyboardEscapeMode: TAdvTableViewKeyboardEscapeMode;
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    function GetClipboardMode: TAdvTableViewClipboardMode;
    function GetDragDropMode: TAdvTableViewDragDropMode;
    function GetReorder: Boolean;
    function GetTouchScrolling: Boolean;
    procedure SetDragDropMode(const Value: TAdvTableViewDragDropMode);
    procedure SetFiltering(const Value: TAdvTableViewFiltering);
    procedure SetLookup(const Value: TAdvTableViewLookup);
    procedure SetReorder(const Value: Boolean);
    procedure SetTouchScrolling(const Value: Boolean);
    procedure SetClipboardMode(const Value: TAdvTableViewClipboardMode);
    function GetSorting: TAdvTableViewSorting;
    procedure SetSorting(const Value: TAdvTableViewSorting);
    procedure SetShowDetailMode(const Value: TAdvTableViewShowDetailMode);
    procedure SetShowEditButton(const Value: Boolean);
    procedure SetShowFilterButton(const Value: Boolean);
    procedure SetAccessoryClickDetection(
      const Value: TAdvTableViewItemAccessories);
    function GetAutoOpenURL: Boolean;
    procedure SetAutoOpenURL(const Value: Boolean);
    function GetAnimationFactor: Single;
    procedure SetAnimationFactor(const Value: Single);
  protected
    property ClipboardMode: TAdvTableViewClipboardMode read GetClipboardMode write SetClipboardMode default tvcmNone;
    property Reorder: Boolean read GetReorder write SetReorder default False;
    property Sorting: TAdvTableViewSorting read GetSorting write SetSorting default tvcsNone;
    property Filtering: TAdvTableViewFiltering read FFiltering write SetFiltering;
    property Lookup: TAdvTableViewLookup read FLookup write SetLookup;
  public
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DragDropMode: TAdvTableViewDragDropMode read GetDragDropMode write SetDragDropMode default tvdmNone;
  published
    property AnimationFactor: Single read GetAnimationFactor write SetAnimationFactor;
    property AutoOpenURL: Boolean read GetAutoOpenURL write SetAutoOpenURL default True;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
    property TouchScrolling: Boolean read GetTouchScrolling write SetTouchScrolling default True;
    property ShowDetailMode: TAdvTableViewShowDetailMode read FShowDetailMode write SetShowDetailMode default tvdtAfterSelectItem;
    property ShowEditButton: Boolean read FShowEditButton write SetShowEditButton default False;
    property ShowFilterButton: Boolean read FShowFilterButton write SetShowFilterButton default False;
    property SwipeBounceGesture: Boolean read FSwipeBounceGesture write FSwipeBounceGesture default True;
    property AccessoryClickDetection: TAdvTableViewItemAccessories read FAccessoryClickDetection write SetAccessoryClickDetection default ClickAccessories;
    property SwipeToShowMoreOptions: Boolean read FSwipeToShowMoreOptions write FSwipeToShowMoreOptions default True;
    property LongPressToEdit: Boolean read FLongPressToEdit write FLongPressToEdit default true;
    property KeyboardEscapeMode: TAdvTableViewKeyboardEscapeMode read FKeyboardEscapeMode write FKeyboardEscapeMode default tvkeStopFilter;
  end;

  TAdvTableViewItemHeightMode = (tvhmFixed, tvhmVariable);

  TAdvTableViewItemAppearance = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FHTMLTemplate: string;
    FAccessoryDetailBitmaps: TAdvScaledBitmaps;
    FAccessoryFont: TAdvGraphicsFont;
    FAccessoryStroke: TAdvGraphicsStroke;
    FAccessoryFill: TAdvGraphicsFill;
    FHeight: Double;
    function GetFont: TAdvGraphicsFont;
    function GetSelectedFill: TAdvGraphicsFill;
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetSelectedFill(const Value: TAdvGraphicsFill);
    function GetSelectedStroke: TAdvGraphicsStroke;
    procedure SetSelectedStroke(const Value: TAdvGraphicsStroke);
    function GetDisabledFill: TAdvGraphicsFill;
    function GetDisabledStroke: TAdvGraphicsStroke;
    function GetFill: TAdvGraphicsFill;
    function GetStroke: TAdvGraphicsStroke;
    procedure SetDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetDisabledStroke(const Value: TAdvGraphicsStroke);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    function GetHeightMode: TAdvTableViewItemHeightMode;
    procedure SetHeightMode(const Value: TAdvTableViewItemHeightMode);
    function GetFixedHeight: Double;
    procedure SetFixedHeight(const Value: Double);
    procedure SetHTMLTemplate(const Value: string);
    procedure SetAccessoryDetailBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetAccessoryFont(const Value: TAdvGraphicsFont);
    procedure SetAccessoryFill(const Value: TAdvGraphicsFill);
    procedure SetAccessoryStroke(const Value: TAdvGraphicsStroke);
    function IsHeightStored: Boolean;
    procedure SetHeight(const Value: Double);
    function GetShowFocus: Boolean;
    procedure SetShowFocus(const Value: Boolean);
  protected
    procedure DoAccessoryDetailBitmapsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
  published
    property Font: TAdvGraphicsFont read GetFont write SetFont;
    property Fill: TAdvGraphicsFill read GetFill write SetFill;
    property Stroke: TAdvGraphicsStroke read GetStroke write SetStroke;
    property SelectedFill: TAdvGraphicsFill read GetSelectedFill write SetSelectedFill;
    property SelectedStroke: TAdvGraphicsStroke read GetSelectedStroke write SetSelectedStroke;
    property DisabledFill: TAdvGraphicsFill read GetDisabledFill write SetDisabledFill;
    property DisabledStroke: TAdvGraphicsStroke read GetDisabledStroke write SetDisabledStroke;
    property HeightMode: TAdvTableViewItemHeightMode read GetHeightMode write SetHeightMode default tvhmVariable;
    property FixedHeight: Double read GetFixedHeight write SetFixedHeight;
    property Height: Double read FHeight write SetHeight stored IsHeightStored nodefault;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
    property AccessoryDetailBitmaps: TAdvScaledBitmaps read FAccessoryDetailBitmaps write SetAccessoryDetailBitmaps;
    property AccessoryFont: TAdvGraphicsFont read FAccessoryFont write SetAccessoryFont;
    property AccessoryStroke: TAdvGraphicsStroke read FAccessoryStroke write SetAccessoryStroke;
    property AccessoryFill: TAdvGraphicsFill read FAccessoryFill write SetAccessoryFill;
    property ShowFocus: Boolean read GetShowFocus write SetShowFocus default True;
  end;

  TAdvTableViewCategoryAppearance = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FHeight: Single;
    function GetFont: TAdvGraphicsFont;
    procedure SetFont(const Value: TAdvGraphicsFont);
    function GetFill: TAdvGraphicsFill;
    function GetStroke: TAdvGraphicsStroke;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    function IsHeightStored: Boolean;
    procedure SetHeight(const Value: Single);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
  published
    property Font: TAdvGraphicsFont read GetFont write SetFont;
    property Fill: TAdvGraphicsFill read GetFill write SetFill;
    property Stroke: TAdvGraphicsStroke read GetStroke write SetStroke;
    property Height: Single read FHeight write SetHeight stored IsHeightStored nodefault;
  end;

  TAdvTableViewMoreOptionAppearance = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FFont: TAdvGraphicsFont;
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
  protected
    procedure FontChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
  published
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
  end;

  TAdvTableViewIntegerArray = array of Integer;

  TAdvTableViewHeaderFooter = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FText: String;
    FWordWrapping: Boolean;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FTrimming: TAdvGraphicsTextTrimming;
    FHorizontalTextAlign: TAdvGraphicsTextAlign;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: String);
    function GetFill: TAdvGraphicsFill;
    function GetHeight: Single;
    function GetStroke: TAdvGraphicsStroke;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetHeight(const Value: Single);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    function GetFont: TAdvGraphicsFont;
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetWordWrapping(const Value: Boolean);
    function GetSortIndicatorColor: TAdvGraphicsColor;
    procedure SetSortIndicatorColor(const Value: TAdvGraphicsColor);
  protected
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Text: String read FText write SetText;
    property Fill: TAdvGraphicsFill read GetFill write SetFill;
    property Font: TAdvGraphicsFont read GetFont write SetFont;
    property Stroke: TAdvGraphicsStroke read GetStroke write SetStroke;
    property HorizontalTextAlign: TAdvGraphicsTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default gtaCenter;
    property VerticalTextAlign: TAdvGraphicsTextAlign read FVerticalTextAlign write SetVerticalTextAlign default gtaCenter;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default False;
    property Trimming: TAdvGraphicsTextTrimming read FTrimming write SetTrimming default gttNone;
    property Height: Single read GetHeight write SetHeight;
    property SortIndicatorColor: TAdvGraphicsColor read GetSortIndicatorColor write SetSortIndicatorColor default TAdvTreeViewColorSelection;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ATableView: TAdvCustomTableView); virtual;
  end;

  TAdvTableViewHeader = class(TAdvTableViewHeaderFooter)
  published
    property Visible;
    property Text;
    property Fill;
    property Font;
    property Stroke;
    property HorizontalTextAlign;
    property VerticalTextAlign;
    property WordWrapping;
    property Trimming;
    property Height;
  end;

  TAdvTableViewFooter = class(TAdvTableViewHeaderFooter)
  published
    property Visible;
    property Text;
    property Fill;
    property Font;
    property Stroke;
    property HorizontalTextAlign;
    property VerticalTextAlign;
    property WordWrapping;
    property Trimming;
    property Height;
  end;

  TAdvTableViewCopyItems = class(TAdvTableViewItems);

  TAdvTableViewBeforeDrawSortIndicatorEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASortIndex: Integer; ASortKind: TAdvTableViewItemsSortKind; var ADefaultDraw: Boolean) of object;
  TAdvTableViewAfterDrawSortIndicatorEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ASortIndex: Integer; ASortKind: TAdvTableViewItemsSortKind) of object;
  TAdvTableViewNeedFilterDropDownDataEvent = procedure(Sender: TObject; AValues: TStrings) of object;
  TAdvTableViewItemAnchorClickEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem; AAnchor: String) of object;
  TAdvTableViewBeforeReorderItemEvent = procedure(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem; var ACanReorder: Boolean) of object;
  TAdvTableViewAfterReorderItemEvent = procedure(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem) of object;
  TAdvTableViewBeforeDropItemEvent = procedure(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem; var ACanDrop: Boolean) of object;
  TAdvTableViewAfterDropItemEvent = procedure(Sender: TObject; AFromItem, AToItem: TAdvTableViewItem) of object;
  TAdvTableViewBeforeCopyToClipboardEvent = procedure(Sender: TObject; var ACanCopy: Boolean) of object;
  TAdvTableViewBeforeCutToClipboardEvent = procedure(Sender: TObject; var ACanCut: Boolean) of object;
  TAdvTableViewBeforePasteFromClipboardEvent = procedure(Sender: TObject; var ACanPaste: Boolean) of object;
  TAdvTableViewAfterCopyToClipboardEvent = procedure(Sender: TObject) of object;
  TAdvTableViewAfterCutToClipboardEvent = procedure(Sender: TObject) of object;
  TAdvTableViewAfterPasteFromClipboardEvent = procedure(Sender: TObject) of object;
  TAdvTableViewBeforeDrawItemTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: String; var AAllow: Boolean) of object;
  TAdvTableViewAfterDrawItemTextEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AText: String) of object;
  TAdvTableViewBeforeDrawItemTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: String; var AAllow: Boolean) of object;
  TAdvTableViewAfterDrawItemTitleEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ATitle: String) of object;
  TAdvTableViewItemSelectedEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemUnSelectedEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemSelectionChangedEvent = TNotifyEvent;
  TAdvTableViewItemClickEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemMouseEnterEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemMouseLeaveEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemCheckChangedEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem) of object;
  TAdvTableViewScrollEvent = procedure(Sender: TObject; APosition: Single) of object;
  TAdvTableViewGetItemHeightEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem; var AHeight: Double) of object;
  TAdvTableViewBeforeDrawItemIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AIcon:TAdvBitmap; var AAllow: Boolean) of object;
  TAdvTableViewAfterDrawItemIconEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AIcon:TAdvBitmap) of object;
  TAdvTableViewBeforeDrawItemCheckEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ACheck: TAdvBitmap; var AAllow: Boolean) of object;
  TAdvTableViewAfterDrawItemCheckEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; ACheck: TAdvBitmap) of object;
  TAdvTableViewBeforeDrawItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTableViewAfterDrawItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemCompareEvent = procedure(Sender: TObject; Item1, Item2: TAdvTableViewItem; var ACompareResult: Integer) of object;
  TAdvTableViewCategoryCompareEvent = procedure(Sender: TObject; Category1, Category2: TAdvTableViewCategory; var ACompareResult: Integer) of object;
  TAdvTableViewFilterSelectEvent = procedure(Sender: TObject; var ACondition: string) of object;
  TAdvTableViewGetHTMLTemplateValueEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem; AName: string; var AValue: string) of object;
  TAdvTableViewGetHTMLTemplateEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem; var AHTMLTemplate: string) of object;
  TAdvTableViewAnchorHeaderFooterClick = procedure(Sender: TObject; AAnchor: string) of object;

  TAdvTableViewFilterOperation = (tvfoSHORT, tvfoNONE, tvfoAND, tvfoXOR, tvfoOR);

  TAdvTableViewFilterData = class(TCollectionItem)
  private
    FCondition: string;
    FCaseSensitive: Boolean;
    FSuffix: string;
    FPrefix: string;
    FOperation: TAdvTableViewFilterOperation;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Condition:string read FCondition write FCondition;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default True;
    property Prefix: string read FPrefix write FPrefix;
    property Suffix: string read FSuffix write FSuffix;
    property Operation: TAdvTableViewFilterOperation read FOperation write FOperation;
  end;

  {$IFDEF WEBLIB}
  TAdvTableViewFilter = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewFilter = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvTableViewFilterData>)
  {$ENDIF}
  private
    FOwner: TAdvCustomTableView;
    function GetItem(Index: Integer): TAdvTableViewFilterData;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewFilterData);
  public
    constructor Create(AOwner: TAdvCustomTableView);
    function Add: TAdvTableViewFilterData;
    function Insert(index: Integer): TAdvTableViewFilterData;
    property Items[Index: Integer]: TAdvTableViewFilterData read GetItem write SetItem; default;
  end;

  TAdvTableViewBeforeApplyFilterEvent = procedure(Sender: TObject; AFilter: TAdvTableViewFilterData; var AAllow: Boolean) of object;
  TAdvTableViewAfterApplyFilterEvent = procedure(Sender: TObject; AFilter: TAdvTableViewFilterData) of object;

  {$IFDEF WEBLIB}
  TAdvTreeViewTableViewFloatingNodes = class(TObjectList)
  private
    function GetItem(Index: Integer): TAdvTreeViewVirtualNode;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewVirtualNode);
  public
    property Items[Index: Integer]: TAdvTreeViewVirtualNode read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTreeViewTableViewFloatingNodes = class(TObjectList<TAdvTreeViewVirtualNode>);
  {$ENDIF}

  TAdvTableViewMoreOption = class;

  TAdvTreeViewTableView = class(TAdvTreeView)
  private
    FDownMoreOption: TAdvTableViewMoreOption;
    FMoreOffset, FMoreOffsetTo: Double;
    FMoreOffsetItem: TAdvTableViewItem;
    FDownAction: Boolean;
    FDownCount: Integer;
    FDownTimer: Boolean;
    FDownX, FDownY: Single;
    FAnimTimer: TTimer;
    FAnimateMoreOptions, FAnimateMoreOptionsClose: Boolean;
    FMouseDownOnAccessory: Boolean;
    FAnimMarqueeSwitch: Boolean;
    FAnimMarqueeStartAngle, FAnimMarqueeSweepAngle, FAnimShiftAngle: Single;
    FManualProgressSweepAngle, FManualProgressStartAngle: Single;
    FReloadActive: Boolean;
    FMouseDownOnLookupBar: Boolean;
    FTableView: TAdvCustomTableView;
    FFloatingNodes: TAdvTreeViewTableViewFloatingNodes;
    FOnFooterAnchorClick: TAdvTableViewAnchorHeaderFooterClick;
    FOnHeaderAnchorClick: TAdvTableViewAnchorHeaderFooterClick;
  protected
    procedure AnimateTimerChanged(Sender: TObject);
    procedure OffsetNodeRects(ANode: TAdvTreeViewVirtualNode; AX, AY: Single; var ARect: TRectF); override;
    procedure CustomizeNodeCache({%H-}AGraphics: TAdvGraphics; {%H-}AStartY: Single); override;
    procedure CustomizeScrollPosition({%H-}ANode: TAdvTreeViewVirtualNode; var {%H-}APosition, {%H-}ATopPosition: Double); override;
    procedure DrawLookupBar(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure DrawReload(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ProcessLookup(X, Y: Single; DoLookup: Boolean = True); virtual;
    procedure StartReload; override;
    procedure UpdateCheckState({%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}AChecked: Boolean); override;
    function ScrollLimitation: Boolean; override;
    function NeedsReload(AVerticalOffset: Single): Boolean; override;
    function GetReloadOffset: Single; override;
    function GetNodesRect: TRectF; override;
    function GetContentRect: TRectF; override;
    function GetCategoryAtXY(X, Y: Single): Integer; virtual;
    function GetLookupBarRect: TRectF; virtual;
    function GetContentClipRect: TRectF; override;
    function CanApplyVerticalOffset: Boolean; override;
    function XYToHeaderAnchor(X, Y: Single): string;
    function XYToFooterAnchor(X, Y: Single): string;
    procedure DoHeaderAnchorClick(AAnchor: String); virtual;
    procedure DoFooterAnchorClick(AAnchor: String); virtual;
    property OnFooterAnchorClick: TAdvTableViewAnchorHeaderFooterClick read FOnFooterAnchorClick write FOnFooterAnchorClick;
    property OnHeaderAnchorClick: TAdvTableViewAnchorHeaderFooterClick read FOnHeaderAnchorClick write FOnHeaderAnchorClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    function GetTotalColumnNodeWidth: Double; override;
    function GetColumnWidth(AColumn: Integer): Double; override;
    property CustomScrollBars;
  end;

  TAdvTableViewDisplayItemKind = (tvikNone, tvikItem, tvikCategory);

  TAdvTableViewDisplayItem = record
    Kind: TAdvTableViewDisplayItemKind;
    Item: TAdvTableViewItem;
    CategoryID: Integer;
    Row: Integer;
    Text: string;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvTableViewDisplayItem) b : boolean;
    {$ENDIF}
  end;

  {$IFDEF WEBLIB}
  TAdvTableViewDisplayList = class(TList)
  private
    function GetItem(Index: Integer): TAdvTableViewDisplayItem;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewDisplayItem);
  public
    property Items[Index: Integer]: TAdvTableViewDisplayItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewDisplayList = class(TList<TAdvTableViewDisplayItem>);
  {$ENDIF}

  TAdvTableViewLookupBarDisplayItem = record
    Rect: TRectF;
    Text: string;
    Active: Boolean;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvTableViewLookupBarDisplayItem) b : boolean;
    {$ENDIF}
  end;

  {$IFDEF WEBLIB}
  TAdvTableViewLookupBarDisplayList = class(TList)
  private
    function GetItem(Index: Integer): TAdvTableViewLookupBarDisplayItem;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewLookupBarDisplayItem);
  public
    property Items[Index: Integer]: TAdvTableViewLookupBarDisplayItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewLookupBarDisplayList = class(TList<TAdvTableViewLookupBarDisplayItem>);
  {$ENDIF}

  TAdvTableViewLookupBarPosition = (tvlbpLeft, tvlbpRight);

  TAdvTableViewLookupBar = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FVisible: Boolean;
    FFont: TAdvGraphicsFont;
    FAutoLookup: Boolean;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FWidth: Single;
    FPosition: TAdvTableViewLookupBarPosition;
    FDownFill: TAdvGraphicsFill;
    FDownStroke: TAdvGraphicsStroke;
    FInActiveFont: TAdvGraphicsFont;
    procedure SetVisible(const Value: Boolean);
    procedure SetFont(const Value: TAdvGraphicsFont);
    procedure SetAutoLookup(const Value: Boolean);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    function IsWidthStored: Boolean;
    procedure SetPosition(const Value: TAdvTableViewLookupBarPosition);
    procedure SetWidth(const Value: Single);
    procedure SetDownFill(const Value: TAdvGraphicsFill);
    procedure SetDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetInActiveFont(const Value: TAdvGraphicsFont);
  protected
    procedure FontChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
  public
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Single read FWidth write SetWidth stored IsWidthStored nodefault;
    property Position: TAdvTableViewLookupBarPosition read FPosition write SetPosition default tvlbpRight;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property InActiveFont: TAdvGraphicsFont read FInActiveFont write SetInActiveFont;
    property AutoLookup: Boolean read FAutoLookup write SetAutoLookup default True;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property DownFill: TAdvGraphicsFill read FDownFill write SetDownFill;
    property DownStroke: TAdvGraphicsStroke read FDownStroke write SetDownStroke;
  end;

  TAdvTableViewReloadProgressMode = (tvrpmMarquee, tvrpmMarqueeAlternate, tvrpmManual);

  TAdvTableViewReload = class(TPersistent)
  private
    FTableView: TAdvCustomTableView;
    FOffset: Single;
    FEnabled: Boolean;
    FStroke: TAdvGraphicsStroke;
    FSize: Single;
    FProgressMode: TAdvTableViewReloadProgressMode;
    function IsOffsetStored: Boolean;
    procedure SetOffset(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    function IsSizeStored: Boolean;
    procedure SetSize(const Value: Single);
    procedure SetProgressMode(const Value: TAdvTableViewReloadProgressMode);
  public
    constructor Create(ATableView: TAdvCustomTableView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Offset: Single read FOffset write SetOffset stored IsOffsetStored nodefault;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property ProgressMode: TAdvTableViewReloadProgressMode read FProgressMode write SetProgressMode default tvrpmMarquee;
  end;

  TAdvTableViewCategory = class(TCollectionItem)
  private
    FTableView: TAdvCustomTableView;
    FText: String;
    FID: integer;
    FLookupText: String;
    FDataPointer: Pointer;
    FDataBoolean: Boolean;
    FDBKey: String;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    procedure SetText(const Value: String);
    procedure SetId(const Value: integer);
    procedure SetLookupText(const Value: String);
    function GetStrippedHTMLText: String;
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    property StrippedHTMLText: String read GetStrippedHTMLText;
  published
    property Text: String read FText write SetText;
    property LookupText: String read FLookupText write SetLookupText;
    property Id: integer read FID write SetId;
  end;

  TAdvTableViewCategoriesSortMode = (tvcsmAscending, tvcsmDescending);

  {$IFDEF WEBLIB}
  TAdvTableViewCategories = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewCategories = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvTableViewCategory>)
  {$ENDIF}
  private
    FTableView: TAdvCustomTableView;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvTableViewCategory;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewCategory);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Compare(AItem1, AItem2: TAdvTableViewCategory; ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewCategoriesSortMode = tvcsmAscending): Integer; virtual;
    procedure QuickSort(L, R: Integer; ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewCategoriesSortMode = tvcsmAscending); virtual;
  public
    constructor Create(ATableView: TAdvCustomTableView);
    property Items[Index: Integer]: TAdvTableViewCategory read GetItem write SetItem; default;
    function ItemById(id: integer): TAdvTableViewCategory;
    function ItemIndexById(id: integer): integer;
    function Add: TAdvTableViewCategory;
    function Insert(Index: Integer): TAdvTableViewCategory;
    procedure Delete(Index: Integer);
    procedure Sort(ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewCategoriesSortMode = tvcsmAscending);
    procedure Clear;
  end;

  TAdvTableViewMoreOption = class(TCollectionItem)
  private
    FTableView: TAdvCustomTableView;
    FText: String;
    FDataPointer: Pointer;
    FDataBoolean: Boolean;
    FDBKey: String;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FWidth: Single;
    FColor: TAdvGraphicsColor;
    FFontColor: TAdvGraphicsColor;
    FBorderColor: TAdvGraphicsColor;
    procedure SetText(const Value: String);
    function GetStrippedHTMLText: String;
    function IsWidthStored: Boolean;
    procedure SetWidth(const Value: Single);
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetFontColor(const Value: TAdvGraphicsColor);
    procedure SetBorderColor(const Value: TAdvGraphicsColor);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    property StrippedHTMLText: String read GetStrippedHTMLText;
  published
    property Text: String read FText write SetText;
    property Width: Single read FWidth write SetWidth stored IsWidthStored nodefault;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcNull;
    property BorderColor: TAdvGraphicsColor read FBorderColor write SetBorderColor default gcNull;
    property FontColor: TAdvGraphicsColor read FFontColor write SetFontColor default gcNull;
  end;

  {$IFDEF WEBLIB}
  TAdvTableViewMoreOptions = class(TAdvOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewMoreOptions = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvTableViewMoreOption>)
  {$ENDIF}
  private
    FTableView: TAdvCustomTableView;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvTableViewMoreOption;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewMoreOption);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(ATableView: TAdvCustomTableView);
    property Items[Index: Integer]: TAdvTableViewMoreOption read GetItem write SetItem; default;
    function ItemById(id: integer): TAdvTableViewMoreOption;
    function ItemIndexById(id: integer): integer;
    function Add: TAdvTableViewMoreOption;
    function Insert(Index: Integer): TAdvTableViewMoreOption;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TAdvTableViewCategoryType = (tvctNone, tvctAlphaBetic, tvctAlphaNumericFirst, tvctAlphaNumericLast, tvctCustom);

  TAdvTableViewLookupCategoryEvent = procedure(Sender: TObject; ACharacter: String; ACharacterID: Integer) of object;
  TAdvTableViewDrawItemCustomAccessoryEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem) of object;
  TAdvTableViewItemMoreOptionClickEvent = procedure(Sender: TObject; AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption) of object;
  TAdvTableViewBeforeDrawItemMoreOptionEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption; var ADefaultDraw: Boolean) of object;
  TAdvTableViewAfterDrawItemMoreOptionEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption) of object;
  TAdvTableViewBeforeItemShowDetailControl = procedure(Sender: TObject; AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean) of object;
  TAdvTableViewBeforeItemHideDetailControl = procedure(Sender: TObject; AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean) of object;
  TAdvTableViewItemShowDetailControl = procedure(Sender: TObject; AItem: TAdvTableViewItem; ADetailControl: TControl) of object;
  TAdvTableViewItemHideDetailControl = procedure(Sender: TObject; AItem: TAdvTableViewItem; ADetailControl: TControl) of object;

  TAdvTableViewItemArray = array of TAdvTableViewItem;
  TAdvTableViewCheckedItems = TAdvTableViewItemArray;
  TAdvTableViewSelectedItems = TAdvTableViewItemArray;

  TAdvTableViewCustomChar = class
  private
    FID: Integer;
    FActive: Boolean;
  public
    constructor Create(AID: Integer; AActive: Boolean);
    property ID: Integer read FID write FID;
    property Active: Boolean read FActive write FActive;
  end;

  {$IFDEF WEBLIB}
  TAdvTableViewCustomCharList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAdvTableViewCustomChar;
    procedure SetItem(Index: Integer; const Value: TAdvTableViewCustomChar);
  public
    property Items[Index: Integer]: TAdvTableViewCustomChar read GetItem write SetItem; default;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvTableViewCustomCharList = class(TObjectList<TAdvTableViewCustomChar>)
  public
  {$ENDIF}
    procedure ActivateID(AID: Integer);
    function IsIDActive(AID: Integer): Boolean;
  end;

  TAdvTableViewEdit = class(TAdvEdit)
  private
    FTableView: TAdvCustomTableView;
  protected
    {$IFDEF FMXLIB}
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$ENDIF}
  end;

  {$IFDEF FNCLIB}
  TAdvCustomTableView = class(TAdvCustomControl, IAdvStylesManager, IAdvPictureContainer, IAdvControlPickerFull, IAdvAppearanceGlobalFont)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvCustomTableView = class(TAdvCustomControl, IAdvAppearanceGlobalFont)
  {$ENDIF}
  private
    FFirstLoad: Boolean;
    FFilterActive: Boolean;
    FEditMode: Boolean;
    FDoneButton, FEditButton, FBackButton, FFilterButton: TAdvToolBarExButton;
    FEdit: TAdvTableViewEdit;
    FActiveAccessoryItem: TAdvTableViewItem;
    FDetailControlTimer: TTimer;
    FDetailControlOffset: Double;
    FShowDetailControl: Boolean;
    FActiveDetailControl: TControl;
    FActiveDetailItem: TAdvTableViewItem;
    FCustomChar: TAdvTableViewCustomCharList;
    FLookupBarDisplayList: TAdvTableViewLookupBarDisplayList;
    FFilterApplied: Boolean;
    FUpdateCount: Integer;
    FDisplayList: TAdvTableViewDisplayList;
    FCategoryList: TAdvTableViewDisplayList;
    FSaveSelectedItem: Integer;
    FSaveSelectedItems: TAdvTableViewIntegerArray;
    FCopyItems: TAdvTableViewCopyItems;
    FAccepted: Boolean;
    FItems: TAdvTableViewItems;
    FTreeView: TAdvTreeViewTableView;
    FColumn: TAdvTreeViewColumn;
    FDefaultItem: TAdvTableViewItem;
    FItemAppearance: TAdvTableViewItemAppearance;
    FOnItemSelected: TAdvTableViewItemSelectedEvent;
    FInteraction: TAdvTableViewInteraction;
    FHeader: TAdvTableViewHeader;
    FFooter: TAdvTableViewFooter;
    FOnItemAnchorClick: TAdvTableViewItemAnchorClickEvent;
    FOnAfterCutToClipboard: TAdvTableViewAfterCutToClipboardEvent;
    FOnBeforePasteFromClipboard: TAdvTableViewBeforePasteFromClipboardEvent;
    FOnAfterReorderItem: TAdvTableViewAfterReorderItemEvent;
    FOnBeforeDropItem: TAdvTableViewBeforeDropItemEvent;
    FOnBeforeDrawItemText: TAdvTableViewBeforeDrawItemTextEvent;
    FOnVScroll: TAdvTableViewScrollEvent;
    FOnBeforeCopyToClipboard: TAdvTableViewBeforeCopyToClipboardEvent;
    FOnItemDblClick: TAdvTableViewItemClickEvent;
    FOnBeforeDrawItemCheck: TAdvTableViewBeforeDrawItemCheckEvent;
    FOnAfterDrawItemIcon: TAdvTableViewAfterDrawItemIconEvent;
    FOnBeforeDrawItem: TAdvTableViewBeforeDrawItemEvent;
    FOnAfterPasteFromClipboard: TAdvTableViewAfterPasteFromClipboardEvent;
    FOnItemClick: TAdvTableViewItemClickEvent;
    FOnAfterDropItem: TAdvTableViewAfterDropItemEvent;
    FOnAfterDrawItemText: TAdvTableViewAfterDrawItemTextEvent;
    FOnItemCompare: TAdvTableViewItemCompareEvent;
    FOnBeforeCutToClipboard: TAdvTableViewBeforeCutToClipboardEvent;
    FOnBeforeReorderItem: TAdvTableViewBeforeReorderItemEvent;
    FOnNeedFilterDropDownData: TAdvTableViewNeedFilterDropDownDataEvent;
    FOnAfterCopyToClipboard: TAdvTableViewAfterCopyToClipboardEvent;
    FOnFilterSelect: TAdvTableViewFilterSelectEvent;
    FOnAfterDrawItemCheck: TAdvTableViewAfterDrawItemCheckEvent;
    FOnAfterDrawItem: TAdvTableViewAfterDrawItemEvent;
    FOnBeforeDrawItemIcon: TAdvTableViewBeforeDrawItemIconEvent;
    FFilter: TAdvTableViewFilter;
    FOnBeforeDrawSortIndicator: TAdvTableViewBeforeDrawSortIndicatorEvent;
    FOnAfterDrawSortIndicator: TAdvTableViewAfterDrawSortIndicatorEvent;
    FOnGetHTMLTemplateValue: TAdvTableViewGetHTMLTemplateValueEvent;
    FOnGetHTMLTemplate: TAdvTableViewGetHTMLTemplateEvent;
    FCategoryType: TAdvTableViewCategoryType;
    FCategories: TAdvTableViewCategories;
    FOnCategoryCompare: TAdvTableViewCategoryCompareEvent;
    FLookupBar: TAdvTableViewLookupBar;
    FOnManualLookupCategory: TAdvTableViewLookupCategoryEvent;
    FOnCategoryClick: TAdvTableViewLookupCategoryEvent;
    FReload: TAdvTableViewReload;
    FOnStopReload: TNotifyEvent;
    FOnStartReload: TNotifyEvent;
    FOnDrawItemCustomAccessory: TAdvTableViewDrawItemCustomAccessoryEvent;
    FOnItemAccessoryClick: TAdvTableViewItemClickEvent;
    FCategoryAppearance: TAdvTableViewCategoryAppearance;
    FOnItemCheckChanged: TAdvTableViewItemCheckChangedEvent;
    FOnAfterApplyFilter: TAdvTableViewAfterApplyFilterEvent;
    FOnBeforeApplyFilter: TAdvTableViewBeforeApplyFilterEvent;
    FMoreOptions: TAdvTableViewMoreOptions;
    FMoreOptionAppearance: TAdvTableViewMoreOptionAppearance;
    FOnItemMoreOptionClick: TAdvTableViewItemMoreOptionClickEvent;
    FOnAfterDrawItemMoreOption: TAdvTableViewAfterDrawItemMoreOptionEvent;
    FOnBeforeDrawItemMoreOption: TAdvTableViewBeforeDrawItemMoreOptionEvent;
    FOnItemShowDetailControl: TAdvTableViewItemShowDetailControl;
    FOnItemHideDetailControl: TAdvTableViewItemHideDetailControl;
    FOnBeforeItemShowDetailControl: TAdvTableViewBeforeItemShowDetailControl;
    FOnBeforeItemHideDetailControl: TAdvTableViewBeforeItemHideDetailControl;
    FOnItemSelectionChanged: TAdvTableViewItemSelectionChangedEvent;
    FOnItemUnSelected: TAdvTableViewItemUnSelectedEvent;
    FOnAfterDrawItemTitle: TAdvTableViewAfterDrawItemTitleEvent;
    FOnBeforeDrawItemTitle: TAdvTableViewBeforeDrawItemTitleEvent;
    FOnItemMouseLeave: TAdvTableViewItemMouseLeaveEvent;
    FOnItemMouseEnter: TAdvTableViewItemMouseEnterEvent;
    FOnFooterAnchorClick: TAdvTableViewAnchorHeaderFooterClick;
    FOnHeaderAnchorClick: TAdvTableViewAnchorHeaderFooterClick;
    FOnItemTitleAnchorClick: TAdvTableViewItemAnchorClickEvent;
    FGlobalFont: TAdvAppearanceGlobalFont;
    procedure SetItems(const Value: TAdvTableViewItems);
    procedure SetDefaultItem(const Value: TAdvTableViewItem);
    procedure SetItemAppearance(const Value: TAdvTableViewItemAppearance);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetInteraction(const Value: TAdvTableViewInteraction);
    function GetPictureContainer: TPictureContainer;
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetHeader(const Value: TAdvTableViewHeader);
    procedure SetFooter(const Value: TAdvTableViewFooter);
    function GetSelectedItem: TAdvTableViewItem;
    procedure SetSelectedItem(const Value: TAdvTableViewItem);
    function GetSelItem(AIndex: Integer): TAdvTableViewItem;
    function GetVerticalScrollBarVisible: Boolean;
    procedure SetVerticalScrollBarVisible(const Value: Boolean);
    function GetFill: TAdvGraphicsFill;
    function GetStroke: TAdvGraphicsStroke;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetCategoryType(const Value: TAdvTableViewCategoryType);
    procedure SetCategories(const Value: TAdvTableViewCategories);
    procedure SetLookupBar(const Value: TAdvTableViewLookupBar);
    procedure SetReload(const Value: TAdvTableViewReload);
    procedure SetCategoryAppearance(
      const Value: TAdvTableViewCategoryAppearance);
    function GetChecked(AItemIndex: Integer): Boolean;
    function GetCheckedItem(AItem: TAdvTableViewItem): Boolean;
    procedure SetChecked(AItemIndex: Integer; const Value: Boolean);
    procedure SetCheckedItem(AItem: TAdvTableViewItem;
      const Value: Boolean);
    procedure SetMoreOptions(const Value: TAdvTableViewMoreOptions);
    procedure SetMoreOptionAppearance(const Value: TAdvTableViewMoreOptionAppearance);
    procedure SetGlobalFont(const Value: TAdvAppearanceGlobalFont);
  protected
    {$IFDEF FNCLIB}
    function GetSubComponentArray: TAdvGraphicsStylesManagerComponentArray;
    {$ENDIF}
    function GetDocURL: string; override;
    function GetVersion: string; override;
    function GetItemsRect: TRectF; virtual;
    function CreateTreeView: TAdvTreeViewTableView; virtual;
    function CreateItems: TAdvTableViewItems; virtual;
    function GetRowForItemIndex(AItemIndex: Integer): Integer; virtual;
    function GetDisplayCategoryIndex(AIndex: Integer): Integer; virtual;
    function GetDisplayItemIndex(AIndex: Integer): Integer; virtual;
    function GetNextDisplayCategoryIndex(AIndex: Integer): Integer; virtual;
    function GetItemIndexForNode(ANode: TAdvTreeViewVirtualNode): Integer; virtual;
    function GetItemForNode(ANode: TAdvTreeViewVirtualNode): TAdvTableViewItem; virtual;
    function GetNodeForItemIndex(AItemIndex: Integer): TAdvTreeViewVirtualNode; virtual;
    function HTMLReplace(AValue: string; AItem: TAdvTableViewItem): string;
    function ItemFromDifferentCategory(AItem1, AItem2: TAdvTableViewItem): Boolean; virtual;
    function MatchFilter(AItem: TAdvTableViewItem): Boolean; virtual;
    function GetCharacterForItem(AItem: TAdvTableViewItem): String; virtual;
    function GetCharacter(Idx: Integer): String; virtual;
    function GetCharacterCount: Integer; virtual;
    function FindCategoryWithCharacter(ACategory: String): TAdvTableViewDisplayItem;
    function FindCategoryWithCharacterID(ACategoryID: Integer): TAdvTableViewDisplayItem;
    function FindFirstItemWithCategory(ACategory: String): TAdvTableViewItem; virtual;
    function FindFirstItemWithCategoryID(ACategoryID: Integer): TAdvTableViewItem; virtual;
    function GetMoreOptionsSize: Single; virtual;
    function IsAppearanceProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean; override;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure DoEnter; override;
    procedure Loaded; override;
    procedure DoItemMoreOptionClick(AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption); virtual;
    procedure DoFilterButtonClicked(Sender: TObject); virtual;
    procedure DoEditButtonClicked(Sender: TObject); virtual;
    procedure DoEditChange(Sender: TObject); virtual;
    procedure DoBeforeApplyFilter(AFilter: TAdvTableViewFilterData; AAllow: Boolean); virtual;
    procedure DoAfterApplyFilter(AFilter: TAdvTableViewFilterData); virtual;
    procedure DoDoneButtonClicked(Sender: TObject); virtual;
    procedure DoBackButtonClicked(Sender: TObject); virtual;
    procedure DoItemAccessoryClick(AItem: TAdvTableViewItem); virtual;
    procedure DoDetailControlTimer(Sender: TObject);
    procedure DoItemShowDetailControl(AItem: TAdvTableViewItem; ADetailControl: TControl); virtual;
    procedure DoItemHideDetailControl(AItem: TAdvTableViewItem; ADetailControl: TControl); virtual;
    procedure DoBeforeItemShowDetailControl(AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean); virtual;
    procedure DoBeforeItemHideDetailControl(AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoStartReload; virtual;
    procedure DoStopReload; virtual;
    procedure ScrollToCategoryId(ACategoryID: Integer); virtual;
    procedure ScrollToCategoryCharacter(ACharacter: String); virtual;
    procedure RegisterRuntimeClasses; override;
    procedure ApplyStyle; override;
    procedure UpdateControlAfterResize; override;
    procedure ResetToDefaultStyle; override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure UpdateTableView(ADisplayOnly: Boolean = False); virtual;
    procedure CustomizeButtons; virtual;
    procedure SelectAllCheckedItems; virtual;
    procedure UnSelectAllItems; virtual;
    procedure CalculateItems; virtual;
    procedure CalculateLookupBar; virtual;
    procedure AddDisplayCategory(AItem: TAdvTableViewItem); virtual;
    procedure AddDisplayItem(AItem: TAdvTableViewItem); virtual;
    procedure DoClick(Sender: TObject);
    procedure DoGetNodeHeight(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AHeight: Double); virtual;
    procedure DoGetNodeIcon(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}ALarge: Boolean; var AIcon: TAdvBitmap); virtual;
    procedure DoGetNodeSides(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides); virtual;
    procedure DoGetColumnText(Sender: TObject; {%H-}AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String); virtual;
    procedure DoGetColumnTrimming(Sender: TObject; {%H-}AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvGraphicsTextTrimming); virtual;
    procedure DoGetColumnWordWrapping(Sender: TObject; {%H-}AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean); virtual;
    procedure DoGetColumnHorizontalTextAlign(Sender: TObject; {%H-}AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoGetColumnVerticalTextAlign(Sender: TObject; {%H-}AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoIsNodeChecked(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AIsChecked: Boolean); virtual;
    procedure DoGetNodeCheckType(Sender: TObject; {%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType); virtual;
    procedure DoAfterCheckNode(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer); virtual;
    procedure DoAfterUnCheckNode(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer); virtual;
    procedure DoItemCheckChanged(AItem: TAdvTableViewItem); virtual;
    procedure DoGetNumberOfNodes(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer); virtual;
    procedure DoGetHTMLTemplateValue(AItem: TAdvTableViewItem; AName: string; var AValue: string); virtual;
    procedure DoGetHTMLTemplate(AItem: TAdvTableViewItem; var AHTMLTemplate: string); virtual;
    procedure DoIsNodeExtended(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean); virtual;
    procedure DoGetNodeText(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}AMode: TAdvTreeViewNodeTextMode; var AText: String); virtual;
    procedure DoGetNodeTitle(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; {%H-}AMode: TAdvTreeViewNodeTextMode; var ATitle: String); virtual;
    procedure DoGetNodeTitleExpanded(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AExpanded: Boolean); virtual;
    procedure DoGetNodeTrimming(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming); virtual;
    procedure DoGetNodeTitleTrimming(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming); virtual;
    procedure DrawBadge(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem); virtual;
    procedure DoDrawNodeExtra(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoDrawItemCustomAccessory(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem); virtual;
    procedure DoBeforeDrawItemMoreOption(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawItemMoreOption(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption); virtual;
    procedure DoGetNodeWordWrapping(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AWordWrapping: Boolean); virtual;
    procedure DoGetNodeTitleWordWrapping(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AWordWrapping: Boolean); virtual;
    procedure DoGetNodeExtraSize(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AExtraSize: Single); virtual;
    procedure DoGetNodeTitleHorizontalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AHorizontalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoGetNodeTitleVerticalTextAlign(Sender: TObject; {%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var {%H-}AVerticalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoGetNodeHorizontalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var AHorizontalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoGetNodeVerticalTextAlign(Sender: TObject; {%H-}ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var {%H-}AVerticalTextAlign: TAdvGraphicsTextAlign); virtual;
    procedure DoGetNodeTextColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ATextColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeSelectedTextColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ASelectedTextColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeDisabledTextColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ADisabledTextColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ATitleColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeSelectedTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ASelectedTitleColor: TAdvGraphicsColor); virtual;
    procedure DoGetNodeDisabledTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; var ADisabledTitleColor: TAdvGraphicsColor); virtual;
    procedure DoIsNodeEnabled(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean); virtual;
    procedure DoAfterSelectNode(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterUnSelectNode(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNeedFilterDropDownData(Sender: TObject; {%H-}AColumn: Integer; AValues: TStrings); virtual;
    procedure DoFilterSelect(Sender: TObject; {%H-}AColumn: integer; var ACondition: string); virtual;
    procedure DoBeforeDrawSortIndicator(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawSortIndicator(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ASortIndex: Integer; ASortKind: TAdvTreeViewNodesSortKind); virtual;
    procedure DoBeforeDrawNode(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawNode(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoItemCompare(AItem1, AItem2: TAdvTableViewItem; var ACompareResult: Integer); virtual;
    procedure DoCategoryCompare(ACategory1, ACategory2: TAdvTableViewCategory; var ACompareResult: Integer); virtual;
    procedure DoBeforeDrawNodeText(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeText(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String); virtual;
    procedure DoBeforeDrawNodeTitle(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeTitle(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ATitle: String); virtual;
    procedure DoBeforeDrawNodeIcon(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeIcon(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap); virtual;
    procedure DoBeforeDrawNodeCheck(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeCheck(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRectF; {%H-}AColumn: Integer; ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap); virtual;
    procedure DoNodeAnchorClick(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; AAnchor: String); virtual;
    procedure DoNodeTitleAnchorClick(Sender: TObject; ANode: TAdvTreeViewVirtualNode; {%H-}AColumn: Integer; AAnchor: String); virtual;
    procedure DoNodeClick(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeMouseLeave(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeMouseEnter(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeDblClick(Sender: TObject; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoVScroll(Sender: TObject; APosition: Single); virtual;
    procedure DoBeforeCutToClipboard(Sender: TObject; var ACanCut: Boolean); virtual;
    procedure DoBeforeCopyToClipboard(Sender: TObject; var ACanCopy: Boolean); virtual;
    procedure DoBeforePasteFromClipboard(Sender: TObject; var ACanPaste: Boolean); virtual;
    procedure DoAfterCutToClipboard(Sender: TObject); virtual;
    procedure DoAfterCopyToClipboard(Sender: TObject); virtual;
    procedure DoAfterPasteFromClipboard(Sender: TObject); virtual;
    procedure DoBeforeReorderItem(AFromItem, AToItem: TAdvTableViewItem; var ACanReorder: Boolean); virtual;
    procedure DoAfterReorderItem(AFromItem, AToItem: TAdvTableViewItem); virtual;
    procedure DoBeforeDropItem(AFromItem, AToItem: TAdvTableViewItem; var ACanDrop: Boolean); virtual;
    procedure DoAfterDropItem(AFromItem, AToItem: TAdvTableViewItem); virtual;
    procedure CustomizeTreeView({%H-}ATreeView: TAdvTreeView); virtual;
    procedure DoCustomDragOver(Sender: TObject; Source: TObject; {%H-}Point: TPointF; var Accept: Boolean); virtual;
    procedure DoCustomDragDrop(Sender: TObject; Source: TObject; {%H-}Point: TPointF); virtual;
    procedure DragItemMove({%H-}AFromItem, {%H-}AToItem: TAdvTableViewItem); virtual;
    procedure DragItemAdd({%H-}ANewItem, {%H-}AAssignItem, {%H-}AInsertItem: TAdvTableViewItem); virtual;
    procedure DragItemDelete({%H-}AItem: TAdvTableViewItem); virtual;
    procedure DoCustomReorder(Sender: TObject; AFromNode, AToNode: TAdvTreeViewVirtualNode); virtual;
    procedure DoCopyToClipboard(Sender: TObject); virtual;
    procedure DoCutToClipboard(Sender: TObject); virtual;
    procedure DoPasteFromClipboard(Sender: TObject); virtual;
    procedure DoColumnSort(Sender: TObject; {%H-}AColumn: Integer; ASortMode: TAdvTreeViewNodesSortMode); virtual;
    procedure DoLookup(Sender: TObject; ALookupString: String); virtual;
    procedure UpdateActiveDetailControl; virtual;
    procedure DoFooterAnchorClick(Sender: TObject; AAnchor: String);
    procedure DoHeaderAnchorClick(Sender: TObject; AAnchor: String);
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
    procedure SetFonts(ASetType: TAdvAppearanceGlobalFontType); virtual;
    property Fill: TAdvGraphicsFill read GetFill write SetFill;
    property Stroke: TAdvGraphicsStroke read GetStroke write SetStroke;
    property Version: String read GetVersion;
    property TreeView: TAdvTreeViewTableView read FTreeView;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property Items: TAdvTableViewItems read FItems write SetItems;
    property DefaultItem: TAdvTableViewItem read FDefaultItem write SetDefaultItem;
    property ItemAppearance: TAdvTableViewItemAppearance read FItemAppearance write SetItemAppearance;
    property CategoryAppearance: TAdvTableViewCategoryAppearance read FCategoryAppearance write SetCategoryAppearance;
    property MoreOptionAppearance: TAdvTableViewMoreOptionAppearance read FMoreOptionAppearance write SetMoreOptionAppearance;
    property Interaction: TAdvTableViewInteraction read FInteraction write SetInteraction;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property LookupBar: TAdvTableViewLookupBar read FLookupBar write SetLookupBar;
    property Reload: TAdvTableViewReload read FReload write SetReload;
    property Categories: TAdvTableViewCategories read FCategories write SetCategories;
    property MoreOptions: TAdvTableViewMoreOptions read FMoreOptions write SetMoreOptions;
    property CategoryType: TAdvTableViewCategoryType read FCategoryType write SetCategoryType default tvctNone;
    property GlobalFont: TAdvAppearanceGlobalFont read FGlobalFont write SetGlobalFont;
    property OnItemSelected: TAdvTableViewItemSelectedEvent read FOnItemSelected write FOnItemSelected;
    property OnItemUnSelected: TAdvTableViewItemUnSelectedEvent read FOnItemUnSelected write FOnItemUnSelected;
    property OnItemSelectionChanged: TAdvTableViewItemSelectionChangedEvent read FOnItemSelectionChanged write FOnItemSelectionChanged;
    property Header: TAdvTableViewHeader read FHeader write SetHeader;
    property Footer: TAdvTableViewFooter read FFooter write SetFooter;
    property VerticalScrollBarVisible: Boolean read GetVerticalScrollBarVisible write SetVerticalScrollBarVisible default False;
    property OnNeedFilterDropDownData: TAdvTableViewNeedFilterDropDownDataEvent read FOnNeedFilterDropDownData write FOnNeedFilterDropDownData;
    property OnFilterSelect: TAdvTableViewFilterSelectEvent read FOnFilterSelect write FOnFilterSelect;
    property OnBeforeDrawItem: TAdvTableViewBeforeDrawItemEvent read FOnBeforeDrawItem write FOnBeforeDrawItem;
    property OnAfterDrawItem: TAdvTableViewAfterDrawItemEvent read FOnAfterDrawItem write FOnAfterDrawItem;
    property OnBeforeDrawItemMoreOption: TAdvTableViewBeforeDrawItemMoreOptionEvent read FOnBeforeDrawItemMoreOption write FOnBeforeDrawItemMoreOption;
    property OnAfterDrawItemMoreOption: TAdvTableViewAfterDrawItemMoreOptionEvent read FOnAfterDrawItemMoreOption write FOnAfterDrawItemMoreOption;
    property OnItemCompare: TAdvTableViewItemCompareEvent read FOnItemCompare write FOnItemCompare;
    property OnCategoryCompare: TAdvTableViewCategoryCompareEvent read FOnCategoryCompare write FOnCategoryCompare;
    property OnAfterDrawItemIcon: TAdvTableViewAfterDrawItemIconEvent read FOnAfterDrawItemIcon write FOnAfterDrawItemIcon;
    property OnBeforeDrawItemIcon: TAdvTableViewBeforeDrawItemIconEvent read FOnBeforeDrawItemIcon write FOnBeforeDrawItemIcon;
    property OnAfterDrawItemCheck: TAdvTableViewAfterDrawItemCheckEvent read FOnAfterDrawItemCheck write FOnAfterDrawItemCheck;
    property OnBeforeDrawItemCheck: TAdvTableViewBeforeDrawItemCheckEvent read FOnBeforeDrawItemCheck write FOnBeforeDrawItemCheck;
    property OnBeforeDrawItemText: TAdvTableViewBeforeDrawItemTextEvent read FOnBeforeDrawItemText write FOnBeforeDrawItemText;
    property OnAfterDrawItemText: TAdvTableViewAfterDrawItemTextEvent read FOnAfterDrawItemText write FOnAfterDrawItemText;
    property OnBeforeDrawItemTitle: TAdvTableViewBeforeDrawItemTitleEvent read FOnBeforeDrawItemTitle write FOnBeforeDrawItemTitle;
    property OnAfterDrawItemTitle: TAdvTableViewAfterDrawItemTitleEvent read FOnAfterDrawItemTitle write FOnAfterDrawItemTitle;
    property OnAfterDrawSortIndicator: TAdvTableViewAfterDrawSortIndicatorEvent read FOnAfterDrawSortIndicator write FOnAfterDrawSortIndicator;
    property OnBeforeDrawSortIndicator: TAdvTableViewBeforeDrawSortIndicatorEvent read FOnBeforeDrawSortIndicator write FOnBeforeDrawSortIndicator;
    property OnItemAnchorClick: TAdvTableViewItemAnchorClickEvent read FOnItemAnchorClick write FOnItemAnchorClick;
    property OnItemTitleAnchorClick: TAdvTableViewItemAnchorClickEvent read FOnItemTitleAnchorClick write FOnItemTitleAnchorClick;
    property OnItemClick: TAdvTableViewItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemMouseLeave: TAdvTableViewItemMouseLeaveEvent read FOnItemMouseLeave write FOnItemMouseLeave;
    property OnItemMouseEnter: TAdvTableViewItemMouseEnterEvent read FOnItemMouseEnter write FOnItemMouseEnter;
    property OnItemDblClick: TAdvTableViewItemClickEvent read FOnItemDblClick write FOnItemDblClick;
    property OnVScroll: TAdvTableViewScrollEvent read FOnVScroll write FOnVScroll;
    property OnBeforeReorderItem: TAdvTableViewBeforeReorderItemEvent read FOnBeforeReorderItem write FOnBeforeReorderItem;
    property OnAfterReorderItem: TAdvTableViewAfterReorderItemEvent read FOnAfterReorderItem write FOnAfterReorderItem;
    property OnBeforeDropItem: TAdvTableViewBeforeDropItemEvent read FOnBeforeDropItem write FOnBeforeDropItem;
    property OnAfterDropItem: TAdvTableViewAfterDropItemEvent read FOnAfterDropItem write FOnAfterDropItem;
    property OnBeforeCutToClipboard: TAdvTableViewBeforeCutToClipboardEvent read FOnBeforeCutToClipboard write FOnBeforeCutToClipboard;
    property OnBeforeCopyToClipboard: TAdvTableViewBeforeCopyToClipboardEvent read FOnBeforeCopyToClipboard write FOnBeforeCopyToClipboard;
    property OnBeforePasteFromClipboard: TAdvTableViewBeforePasteFromClipboardEvent read FOnBeforePasteFromClipboard write FOnBeforePasteFromClipboard;
    property OnAfterCutToClipboard: TAdvTableViewAfterCutToClipboardEvent read FOnAfterCutToClipboard write FOnAfterCutToClipboard;
    property OnAfterCopyToClipboard: TAdvTableViewAfterCopyToClipboardEvent read FOnAfterCopyToClipboard write FOnAfterCopyToClipboard;
    property OnAfterPasteFromClipboard: TAdvTableViewAfterPasteFromClipboardEvent read FOnAfterPasteFromClipboard write FOnAfterPasteFromClipboard;
    property OnGetHTMLTemplateValue: TAdvTableViewGetHTMLTemplateValueEvent read FOnGetHTMLTemplateValue write FOnGetHTMLTemplateValue;
    property OnGetHTMLTemplate: TAdvTableViewGetHTMLTemplateEvent read FOnGetHTMLTemplate write FOnGetHTMLTemplate;
    property OnManualLookupCategory: TAdvTableViewLookupCategoryEvent read FOnManualLookupCategory write FOnManualLookupCategory;
    property OnCategoryClick: TAdvTableViewLookupCategoryEvent read FOnCategoryClick write FOnCategoryClick;
    property OnStartReload: TNotifyEvent read FOnStartReload write FOnStartReload;
    property OnStopReload: TNotifyEvent read FOnStopReload write FOnStopReload;
    property OnDrawItemCustomAccessory: TAdvTableViewDrawItemCustomAccessoryEvent read FOnDrawItemCustomAccessory write FOnDrawItemCustomAccessory;
    property OnItemAccessoryClick: TAdvTableViewItemClickEvent read FOnItemAccessoryClick write FOnItemAccessoryClick;
    property OnItemCheckChanged: TAdvTableViewItemCheckChangedEvent read FOnItemCheckChanged write FOnItemCheckChanged;
    property OnBeforeApplyFilter: TAdvTableViewBeforeApplyFilterEvent read FOnBeforeApplyFilter write FOnBeforeApplyFilter;
    property OnAfterApplyFilter: TAdvTableViewAfterApplyFilterEvent read FOnAfterApplyFilter write FOnAfterApplyFilter;
    property OnItemMoreOptionClick: TAdvTableViewItemMoreOptionClickEvent read FOnItemMoreOptionClick write FOnItemMoreOptionClick;
    property OnBeforeItemShowDetailControl: TAdvTableViewBeforeItemShowDetailControl read FOnBeforeItemShowDetailControl write FOnBeforeItemShowDetailControl;
    property OnBeforeItemHideDetailControl: TAdvTableViewBeforeItemHideDetailControl read FOnBeforeItemHideDetailControl write FOnBeforeItemHideDetailControl;
    property OnItemShowDetailControl: TAdvTableViewItemShowDetailControl read FOnItemShowDetailControl write FOnItemShowDetailControl;
    property OnItemHideDetailControl: TAdvTableViewItemHideDetailControl read FOnItemHideDetailControl write FOnItemHideDetailControl;
    property OnHeaderAnchorClick: TAdvTableViewAnchorHeaderFooterClick read FOnHeaderAnchorClick write FOnHeaderAnchorClick;
    property OnFooterAnchorClick: TAdvTableViewAnchorHeaderFooterClick read FOnFooterAnchorClick write FOnFooterAnchorClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure Assign(Source: TPersistent); override;
    function AddItem(AText: string = ''): TAdvTableViewItem; virtual;
    function XYToAccessoryItem(X, Y: Single): TAdvTableViewItem; virtual;
    function XYToItem(X, Y: Single): TAdvTableViewItem; virtual;
    function XYToMoreOption(X, Y: Single): TAdvTableViewMoreOption; virtual;
    function XYToItemIndex(X, Y: Single): Integer; virtual;
    function SelectedItemCount: Integer; virtual;
    function GetItemsFromClipboard: TAdvTableViewCopyItems; virtual;
    function LookupItem(ALookupString: String; ACaseSensitive: Boolean = False; AAutoSelect: Boolean = False): TAdvTableViewItem; virtual;
    function IsItemSelectable(AItem: TAdvTableViewItem): Boolean; virtual;
    function ReloadProgress: Single; virtual;
    function DetailControlActive: Boolean; virtual;
    function GetCheckedItems: TAdvTableViewCheckedItems; virtual;
    function GetSelectedItems: TAdvTableViewSelectedItems; virtual;
    procedure LookupCustomCategory(ACategoryID: Integer); virtual;
    procedure LookupCategory(ACategory: String);virtual;
    procedure RemoveItem(AItem: TAdvTableViewItem); virtual;
    procedure EnableInteraction; virtual;
    procedure DisableInteraction; virtual;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure InitSample; virtual;
    procedure ScrollToItem(AItemIndex: Integer); virtual;
    procedure ClearSelection; virtual;
    procedure UnCheckAllItems; virtual;
    procedure StartFiltering; virtual;
    procedure StopFiltering; virtual;
    procedure StartEditMode; virtual;
    procedure ShowMoreOptions(AItem: TAdvTableViewItem); virtual;
    procedure HideMoreOptions; virtual;
    procedure StopEditMode; virtual;
    procedure ToggleEditMode; virtual;
    procedure CheckAllItems; virtual;
    procedure SelectItem(AItemIndex: Integer); virtual;
    procedure SelectItems(AItemIndexes: TAdvTableViewIntegerArray); virtual;
    procedure LoadFromStrings(AStrings: TStrings); virtual;
    {$IFNDEF WEBLIB}
    procedure LoadFromFile(AFileName: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToStrings(AStrings: TStrings); virtual;
    procedure SaveToFile(AFileName: string; ATextOnly: Boolean = True); virtual;
    procedure SaveToStream(AStream: TStream; ATextOnly: Boolean = True); virtual;
    {$ENDIF}
    procedure CutToClipboard(ATextOnly: Boolean = False); virtual;
    procedure CopyToClipboard(ATextOnly: Boolean = False); virtual;
    procedure PasteFromClipboard; virtual;
    procedure Sort(ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewItemsSortMode = tvismAscending); {$IFDEF FMXLIB}reintroduce;{$ENDIF} virtual;
    procedure ApplyFilter; virtual;
    procedure RemoveFilter; virtual;
    procedure RemoveFilters; virtual;
    procedure StopReload; virtual;
    procedure StartReload; virtual;
    procedure LoadSettingsFromFile(AFileName: string); override;
    procedure LoadSettingsFromStream(AStream: TStreamEx); override;
    procedure ShowDetailControl(AItemIndex: Integer = -1); virtual;
    procedure HideDetailControl; virtual;
    procedure UpdateReloadProgress(AProgress: Single; AAutoStopReload: Boolean = True); virtual;
    property Filter: TAdvTableViewFilter read FFilter;
    property SelectedItem: TAdvTableViewItem read GetSelectedItem write SetSelectedItem;
    property SelectedItems[AIndex: Integer]: TAdvTableViewItem read GetSelItem;
    property CheckedItem[AItem: TAdvTableViewItem]: Boolean read GetCheckedItem write SetCheckedItem;
    property Checked[AItemIndex: Integer]: Boolean read GetChecked write SetChecked;
    property DoneButton: TAdvToolBarExButton read FDoneButton;
    property EditButton: TAdvToolBarExButton read FEditButton;
    property FilterButton: TAdvToolBarExButton read FFilterButton;
    property BackButton: TAdvToolBarExButton read FBackButton;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvTableView = class(TAdvCustomTableView)
  protected
    procedure RegisterRuntimeClasses; override;
    property OnNeedFilterDropDownData;
    property OnFilterSelect;
    property OnBeforeReorderItem;
    property OnAfterReorderItem;
    property OnBeforeDropItem;
    property OnAfterDropItem;
    property OnBeforeCutToClipboard;
    property OnBeforeCopyToClipboard;
    property OnBeforePasteFromClipboard;
    property OnAfterCutToClipboard;
    property OnAfterCopyToClipboard;
    property OnAfterPasteFromClipboard;
  public
    property TreeView;
  published
    property Fill;
    property Stroke;
    property PictureContainer;
    property ItemAppearance;
    property DefaultItem;
    property Items;
    property Header;
    property Footer;
    property ItemIndex;
    property Interaction;
    property LookupBar;
    property Reload;
    property Categories;
    property MoreOptions;
    property MoreOptionAppearance;
    property CategoryAppearance;
    property CategoryType;
    property OnItemSelected;
    property OnItemUnSelected;
    property OnItemSelectionChanged;
    property Version;
    property VerticalScrollBarVisible;
    property GlobalFont;
    property OnBeforeDrawItem;
    property OnAfterDrawItem;
    property OnItemCompare;
    property OnCategoryCompare;
    property OnCategoryClick;
    property OnManualLookupCategory;
    property OnAfterDrawItemIcon;
    property OnBeforeDrawItemIcon;
    property OnBeforeDrawItemText;
    property OnAfterDrawItemText;
    property OnItemAnchorClick;
    property OnItemClick;
    property OnItemMouseLeave;
    property OnItemMouseEnter;
    property OnItemDblClick;
    property OnVScroll;
    property OnStartReload;
    property OnStopReload;
    property OnDrawItemCustomAccessory;
    property OnItemAccessoryClick;
    property OnItemCheckChanged;
    property OnAfterDrawItemCheck;
    property OnBeforeDrawItemCheck;
    property OnBeforeApplyFilter;
    property OnAfterApplyFilter;
    property OnItemMoreOptionClick;
    property OnBeforeItemShowDetailControl;
    property OnBeforeItemHideDetailControl;
    property OnItemShowDetailControl;
    property OnItemHideDetailControl;
    property OnHeaderAnchorClick;
    property OnFooterAnchorClick;
    property OnGetHTMLTemplateValue;
    property OnGetHTMLTemplate;
  end;

{$IFDEF WEBLIB}
const
  AdvTABLEVIEWSEARCH = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7DAAAOwwHHb6hkAAABXElEQVQ4jY2TP0'+
                           '/CUBTFf1VJGhmoEzCQIIkDG4y4gNGJ5cnoiKMTfACCpoubDk5OfAPoUpcm4sRYEkzcsEkHJNGIRgwJiwPUtKVFz3Tzzj3n3PdPYg3KDS0NlID0cq'+
                           'mrq6Lr7pFChArQBGoBtAVUHaMwAxPIReUIR/kUhWyS8fs3Rt9m8PzqtFV0VXRWDMoN7RxoZhIxLk/3icoRD2+YNldtE2AC7G76DfaKJ+2oHJFvzo'+
                           'orYoBMMoYEDKw3GRhv+NJLgFLIJgLFDg7zKacUG0ENcWU7VOznAw2+ZvO1BlMX7zFwrsYwbU+TH1pv6JQPQRNcT2dzbu8eA8XDlw86C4MJ0NoKCg'+
                           'BqhmkzHH0iChniO4s9955G7vS6rgrL8w7KDS0H3ANK6PyL5Lquiha4XqJPPAEOltSxS2wBHV0VE2dBChPrquivmeIXziG6x678V+w2uFgmV/3f9S'+
                           '/8AOOWdCNEGLMkAAAAAElFTkSuQmCC';
{$ENDIF}

implementation

uses
  Math, AdvUtils, SysUtils, Graphics
  {$IFDEF FNCLIB}
  , AdvCustomSelectorEx
  {$ENDIF}
  ;

{$R 'AdvTableView.res'}

type
  TAdvTreeViewOpen = class(TAdvTreeViewTableView);
  TAdvCustomTreeViewOpen = class(TAdvCustomTreeView);
  TAdvCustomTableViewOpen = class(TAdvCustomTableView);
  TAdvTreeViewColumnOpen = class(TAdvTreeViewColumn);

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

procedure TAdvCustomTableView.AddDisplayItem(AItem: TAdvTableViewItem);
var
  dp: TAdvTableViewDisplayItem;
begin
  dp.CategoryID := -1;
  dp.Item := AItem;
  dp.Kind := tvikItem;
  dp.Text := '';
  dp.Row := FDisplayList.Count;
  if Assigned(AItem) then
    AItem.FRow := dp.Row;
  FDisplayList.Add(dp);
end;

procedure TAdvCustomTableView.AddDisplayCategory(AItem: TAdvTableViewItem);
var
  dp: TAdvTableViewDisplayItem;
begin
  if AItem.Visible then
  begin
    dp.Item := nil;
    dp.Kind := tvikCategory;
    dp.Text := GetCharacterForItem(AItem);
    dp.CategoryID := AItem.CategoryID;
    dp.Row := FDisplayList.Count;
    if Assigned(AItem) then
      AItem.FRow := dp.Row;
    FDisplayList.Add(dp);
    FCategoryList.Add(dp);
  end;
end;

function TAdvCustomTableView.AddItem(AText: string = ''): TAdvTableViewItem;
begin
  Result := Items.Add;
  Result.Text := AText;
end;

procedure TAdvCustomTableView.ApplyFilter;
begin
  FFilterApplied := True;
  UpdateTableView;
end;

procedure TAdvCustomTableView.ApplyStyle;
var
  I: Integer;
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleTextFontColor(c) then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      Items[I].TextColor := c;
      Items[I].TitleColor := c;
    end;
  end;
end;

procedure TAdvCustomTableView.Assign(Source: TPersistent);
begin
  if (Source is TAdvCustomTableView) then
  begin
    FTreeView.Assign((Source as TAdvCustomTableView).TreeView);
    FItems.Assign((Source as TAdvCustomTableView).Items);
    FItemAppearance.Assign((Source as TAdvCustomTableView).ItemAppearance);
    FCategoryAppearance.Assign((Source as TAdvCustomTableView).CategoryAppearance);
    FCategoryType := (Source as TAdvCustomTableView).CategoryType;
    FFilter.Assign((Source as TAdvCustomTableView).Filter);
    FHeader.Assign((Source as TAdvCustomTableView).Header);
    FFooter.Assign((Source as TAdvCustomTableView).Footer);
    FDefaultItem.Assign((Source as TAdvCustomTableView).DefaultItem);
    FInteraction.Assign((Source as TAdvCustomTableView).Interaction);
    FReload.Assign((Source as TAdvCustomTableView).Reload);
    FLookupBar.Assign((Source as TAdvCustomTableView).LookupBar);
  end;
end;

procedure TAdvCustomTableView.BeginUpdate;
var
  a: TAdvTableViewSelectedItems;
  I: Integer;
begin
  inherited;
  Inc(FUpdateCount);
  if Assigned(FTreeView) then
  begin
    if Assigned(SelectedItem) then
      FSaveSelectedItem := SelectedItem.Index
    else
      FSaveSelectedItem := -1;

    a := GetSelectedItems;
    SetLength(FSaveSelectedItems, Length(a));
    for I := 0 to Length(a) - 1 do
    begin
      if Assigned(a[I]) then
        FSaveSelectedItems[I] := a[I].Index;
    end;

    FTreeView.BeginUpdate;
    FTreeView.ClearNodeList;
  end;
end;

procedure TAdvCustomTableView.CalculateItems;
var
  I: Integer;
  J: Integer;
  v: Boolean;
  it: TAdvTableViewItem;
begin
  if (FUpdateCount > 0) or IsDestroying then
    Exit;

  FDisplayList.Clear;
  FCategoryList.Clear;

  j := 0;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I];
    v := it.Visible;
    if FFilterApplied then
      v := v and MatchFilter(it);

    if CategoryType <> tvctNone then
    begin
      if FDisplayList.Count = 0 then
      begin
        if v then
        begin
          AddDisplayCategory(it);
          AddDisplayItem(it);
        end;
        Inc(J);
      end
      else
      begin
        if ItemFromDifferentCategory(it, Items[J - 1]) then
        begin
          if v then
          begin
            AddDisplayCategory(it);
            AddDisplayItem(it);
          end;
          Inc(J);
        end
        else
        begin
          if v then
            AddDisplayItem(it);
          Inc(J);
        end;
      end;
    end
    else if v then
      AddDisplayItem(it);
  end;
end;

procedure TAdvCustomTableView.CalculateLookupBar;
var
  s: Integer;
  I: Integer;
  str: string;
  r, cr: TRectF;
  ldp: TAdvTableViewLookupBarDisplayItem;
  ypos: Single;
  g: TAdvGraphics;
  j: Integer;
begin
  if (FUpdateCount > 0) or IsDestroying or not Assigned(FTreeView) then
    Exit;

  FLookupBarDisplayList.Clear;

  if LookupBar.Visible and (CategoryType <> tvctNone) then
  begin
    cr := FTreeView.GetLookupBarRect;

    case CategoryType of
      tvctAlphaBetic, tvctAlphaNumericFirst, tvctAlphaNumericLast:
      begin
        s := GetCharacterCount;
        for I := 1 to s do
        begin
          str := GetCharacter(I);
          r.Left := cr.Left;
          r.Right := cr.Right;
          r.Top := cr.Top + ((I - 1) * ((cr.Bottom - cr.Top) / s));
          r.Bottom := r.Top + ((cr.Bottom - cr.Top) / s);

          ldp.Rect := r;
          ldp.Text := str;
          ldp.Active := Assigned(FindFirstItemWithCategory(str));

          FLookupBarDisplayList.Add(ldp);
        end;
      end;
      tvctCustom:
      begin
        FCustomChar.Clear;
        for I := 0 to Categories.Count - 1 do
          FCustomChar.Add(TAdvTableViewCustomChar.Create(Categories[I].Id, False));

        for I := 0 to Items.Count - 1 do
        begin
          str := Items[I].StrippedHTMLText;
          if (str <> '') then
          begin
            j := Items[I].CategoryID;
            FCustomChar.ActivateID(j);
          end;
        end;

        g := TAdvGraphics.CreateBitmapCanvas;
        try
          g.Font.Assign(LookupBar.Font);
          s := Categories.Count - 1;
          ypos := cr.Top + 3;
          for I := 0 to s do
          begin
            str := Categories[I].LookupText;

            r.Left := cr.Left;
            r.Right := cr.Right;
            r.Top := ypos;
            r.Bottom := r.Top + g.CalculateTextHeight(str);
            ypos := ypos + (r.Bottom - r.Top) + 3;

            ldp.Rect := r;
            ldp.Text := str;
            ldp.Active := Assigned(FindFirstItemWithCategoryID(Categories[I].Id));

            FLookupBarDisplayList.Add(ldp);
          end;
        finally
          g.Free;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  FDoneButton.ScaleForPPI(CurrentPPI);
  FEditButton.ScaleForPPI(CurrentPPI);
  FBackButton.ScaleForPPI(CurrentPPI);
  FFilterButton.ScaleForPPI(CurrentPPI);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ItemAppearance.Height := TAdvUtils.MulDivSingle(ItemAppearance.Height, M, D);
  UpdateControlAfterResize;
end;

procedure TAdvCustomTableView.CheckAllItems;
var
  I: Integer;
begin
  BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Checked := True;
  EndUpdate;
end;

procedure TAdvCustomTableView.ClearSelection;
begin
  if Assigned(FTreeView) then
  begin
    if FEditMode then
      UnCheckAllItems;
    FTreeView.UnSelectAllNodes;
  end;
end;

function TAdvCustomTableView.GetCheckedItems: TAdvTableViewCheckedItems;
var
  I: Integer;
  n: TAdvTableViewItem;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    n := Items[I] as TAdvTableViewItem;
    if n.Checked then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := n;
    end;
  end;
end;

procedure TAdvCustomTableView.CopyToClipboard(ATextOnly: Boolean);
var
  s: String;
  I: Integer;
  it: TAdvTableViewItem;
  cl: TAdvTreeViewIntegerList;
begin
  cl := TAdvTreeViewIntegerList.Create;
  try
    s := '';
    for I := 0 to SelectedItemCount - 1 do
    begin
      it := SelectedItems[I];
      if Assigned(it) and (cl.IndexOf(it.Index) = -1) then
      begin
        s := s + it.SaveToString(ATextOnly) + ENDOFLINE;
        cl.Add(it.Index);
      end;
    end;

    if s <> '' then
      TAdvClipBoard.SetText(CLP_FMT + s);
  finally
    cl.Free;
  end;
end;

constructor TAdvCustomTableView.Create(AOwner: TComponent);
begin
  inherited;
  FFirstLoad := False;
  FEdit := TAdvtableviewEdit.Create(Self);
  FEdit.FTableView := Self;
  FEdit.OnChange := DoEditChange;
  {$IFDEF FMXLIB}
  FEdit.OnChangeTracking := DoEditChange;
  FEdit.Stored := False;
  {$ENDIF}

  FEditButton := TAdvToolBarExButton.Create(Self);
  FEditButton.OnClick := DoEditButtonClicked;
  FEditButton.Stored := False;
  FEditButton.Width := 50;
  FEditButton.Appearance.FlatStyle := True;

  FDoneButton := TAdvToolBarExButton.Create(Self);
  FDoneButton.OnClick := DoDoneButtonClicked;
  FDoneButton.Stored := False;
  FDoneButton.Width := 50;
  FDoneButton.Appearance.FlatStyle := True;

  FFilterButton := TAdvToolBarExButton.Create(Self);
  FFilterButton.OnClick := DoFilterButtonClicked;
  FFilterButton.Stored := False;
  FFilterButton.Width := 30;
  FFilterButton.Appearance.FlatStyle := True;
  FFilterButton.Bitmaps.AddBitmapFromResource('AdvTABLEVIEWSEARCH', HInstance, 1.0);
  FFilterButton.BitmapVisible := True;

  FBackButton := TAdvToolBarExButton.Create(Self);
  FBackButton.OnClick := DoBackButtonClicked;
  FBackButton.Stored := False;
  FBackButton.Width := 50;
  FBackButton.Appearance.FlatStyle := True;

  FEditButton.Text := 'Edit';
  FDoneButton.Text := 'Done';
  FBackButton.Text := 'Back';

  FDetailControlTimer := TTimer.Create(Self);
  FDetailControlTimer.OnTimer := DoDetailControlTimer;
  FDetailControlTimer.Enabled := False;
  FDetailControlTimer.Interval := 1;

  FDisplayList := TAdvTableViewDisplayList.Create;
  FCategoryList := TAdvTableViewDisplayList.Create;
  FLookupBarDisplayList := TAdvTableViewLookupBarDisplayList.Create;
  FItems := CreateItems;
  FLookupBar := TAdvTableViewLookupBar.Create(Self);
  FReload := TAdvTableViewReload.Create(Self);
  FCategories := TAdvTableViewCategories.Create(Self);
  FMoreOptions := TAdvTableViewMoreOptions.Create(Self);
  FCategoryType := tvctNone;

  FCustomChar := TAdvTableViewCustomCharList.Create;

  FTreeView := CreateTreeView;
  {$IFDEF WEBLIB}
  FTreeView.AllowTouch := False;
  {$ENDIF}
  FTreeView.OnClick := DoClick;
  FTreeView.OptimizedHTMLDrawing := True;
  FTreeView.FTableView := Self;
  FTreeView.Stored := False;
  FTreeView.Parent := Self;
  FTreeView.Columns.Clear;
  FColumn := FTreeView.Columns.Add;
  FTreeView.ClearNodes;
  FTreeView.ColumnsAppearance.Layouts := [tclTop, tclBottom];
  FTreeView.ColumnsAppearance.TopSize := 30;
  FTreeView.ColumnsAppearance.BottomSize := 30;
  FTreeView.StretchScrollBars := True;
  FTreeView.VerticalScrollBarVisible := False;
  FTreeview.NodesAppearance.Stroke.Kind := gskSolid;
  FTreeView.NodesAppearance.ExpandColumn := -1;
  FTreeView.NodesAppearance.LevelIndent := 0;
  FTreeView.NodesAppearance.ShowLines := False;
  FTreeView.NodesAppearance.ExpandWidth := 0;
  FTreeView.NodesAppearance.ExpandHeight := 0;
  FTreeView.NodesAppearance.SelectionArea := tsaDefault;
  FTreeView.NodesAppearance.ShowFocus := True;
  FTreeView.NodesAppearance.SelectedFill.Color := MakeGraphicsColor(217, 217, 217);
  FTreeView.NodesAppearance.SelectedStroke.Color := MakeGraphicsColor(217, 217, 217);
  FTreeView.NodesAppearance.Stroke.Color := MakeGraphicsColor(200, 199, 204);

  FFilter := TAdvTableViewFilter.Create(Self);
  FHeader := TAdvTableViewHeader.Create(Self);
  FFooter := TAdvTableViewFooter.Create(Self);
  FItemAppearance := TAdvTableViewItemAppearance.Create(Self);
  FCategoryAppearance := TAdvTableViewCategoryAppearance.Create(Self);
  FMoreOptionAppearance := TAdvTableViewMoreOptionAppearance.Create(Self);
  FDefaultItem := TAdvTableViewItem.Create(nil);
  FInteraction := TAdvTableViewInteraction.Create(Self);
  FCopyItems := TAdvTableViewCopyItems.Create(nil);

  FGlobalFont := TAdvAppearanceGlobalFont.Create(Self);

  FTreeView.OnGetNodeSides := DoGetNodeSides;
  FTreeView.OnGetColumnText := DoGetColumnText;
  FTreeView.OnGetColumnWordWrapping := DoGetColumnWordWrapping;
  FTreeView.OnGetColumnHorizontalTextAlign := DoGetColumnHorizontalTextAlign;
  FTreeView.OnGetColumnVerticalTextAlign := DoGetColumnVerticalTextAlign;
  FTreeView.OnGetColumnTrimming := DoGetColumnTrimming;
  FTreeView.OnGetNumberOfNodes := DoGetNumberOfNodes;
  FTreeView.OnIsNodeChecked := DoIsNodeChecked;
  FTreeView.OnGetNodeCheckType := DoGetNodeCheckType;
  FTreeView.OnAfterUnCheckNode := DoAfterUnCheckNode;
  FTreeView.OnAfterCheckNode := DoAfterCheckNode;
  FTreeView.OnGetNodeHeight := DoGetNodeHeight;
  FTreeView.OnGetNodeIcon := DoGetNodeIcon;
  FTreeView.OnGetNodeTrimming := DoGetNodeTrimming;
  FTreeView.OnGetNodeWordWrapping := DoGetNodeWordWrapping;
  FTreeView.OnGetNodeTitleTrimming := DoGetNodeTitleTrimming;
  FTreeView.OnGetNodeTitleWordWrapping := DoGetNodeTitleWordWrapping;
  FTreeView.OnDrawNodeExtra := DoDrawNodeExtra;
  FTreeView.OnGetNodeExtraSize := DoGetNodeExtraSize;
  FTreeView.OnGetNodeTextColor := DoGetNodeTextColor;
  FTreeView.OnGetNodeTitleColor := DoGetNodeTitleColor;
  FTreeView.OnIsNodeExtended := DoIsNodeExtended;
  FTreeView.OnGetNodeSelectedTextColor := DoGetNodeSelectedTextColor;
  FTreeView.OnGetNodeDisabledTextColor := DoGetNodeDisabledTextColor;
  FTreeView.OnGetNodeSelectedTitleColor := DoGetNodeSelectedTitleColor;
  FTreeView.OnGetNodeDisabledTitleColor := DoGetNodeDisabledTitleColor;
  FTreeView.OnGetNodeText := DoGetNodeText;
  FTreeView.OnGetNodeTitle := DoGetNodeTitle;
  FTreeView.OnGetNodeTitleExpanded := DoGetNodeTitleExpanded;
  FTreeView.OnGetNodeHorizontalTextAlign := DoGetNodeHorizontalTextAlign;
  FTreeView.OnGetNodeVerticalTextAlign := DoGetNodeVerticalTextAlign;
  FTreeView.OnGetNodeTitleHorizontalTextAlign := DoGetNodeTitleHorizontalTextAlign;
  FTreeView.OnGetNodeTitleVerticalTextAlign := DoGetNodeTitleVerticalTextAlign;
  FTreeView.OnIsNodeEnabled := DoIsNodeEnabled;
  FTreeView.OnAfterSelectNode := DoAfterSelectNode;
  FTreeView.OnAfterUnSelectNode := DoAfterUnSelectNode;
  FTreeView.OnNeedFilterDropDownData := DoNeedFilterDropDownData;
  FTreeView.OnFilterSelect := DoFilterSelect;
  FTreeView.OnBeforeDrawNode := DoBeforeDrawNode;
  FTreeView.OnAfterDrawNode := DoAfterDrawNode;
  FTreeView.OnAfterDrawNodeIcon := DoAfterDrawNodeIcon;
  FTreeView.OnBeforeDrawNodeIcon := DoBeforeDrawNodeIcon;
  FTreeView.OnAfterDrawNodeCheck := DoAfterDrawNodeCheck;
  FTreeView.OnBeforeDrawNodeCheck := DoBeforeDrawNodeCheck;
  FTreeView.OnBeforeDrawNodeText := DoBeforeDrawNodeText;
  FTreeView.OnAfterDrawNodeText := DoAfterDrawNodeText;
  FTreeView.OnBeforeDrawNodeTitle := DoBeforeDrawNodeTitle;
  FTreeView.OnAfterDrawNodeTitle := DoAfterDrawNodeTitle;
  FTreeView.OnNodeAnchorClick := DoNodeAnchorClick;
  FTreeView.OnNodeTitleAnchorClick := DoNodeTitleAnchorClick;
  FTreeView.OnBeforeDrawSortIndicator := DoBeforeDrawSortIndicator;
  FTreeView.OnAfterDrawSortIndicator := DoAfterDrawSortIndicator;
  FTreeView.OnNodeClick := DoNodeClick;
  FTreeView.OnNodeMouseLeave := DoNodeMouseLeave;
  FTreeView.OnNodeMouseEnter := DoNodeMouseEnter;
  FTreeView.OnNodeDblClick := DoNodeDblClick;
  FTreeView.OnVScroll := DoVScroll;
  FTreeView.OnBeforeCutToClipboard := DoBeforeCutToClipboard;
  FTreeView.OnBeforeCopyToClipboard := DoBeforeCopyToClipboard;
  FTreeView.OnBeforePasteFromClipboard := DoBeforePasteFromClipboard;
  FTreeView.OnAfterCutToClipboard := DoAfterCutToClipboard;
  FTreeView.OnAfterCopyToClipboard := DoAfterCopyToClipboard;
  FTreeView.OnAfterPasteFromClipboard := DoAfterPasteFromClipboard;
  FTreeView.OnHeaderAnchorClick := DoHeaderAnchorClick;
  FTreeView.OnFooterAnchorClick := DoFooterAnchorClick;

  TAdvTreeViewOpen(FTreeView).OnCustomReorder := DoCustomReorder;
  TAdvTreeViewOpen(FTreeView).OnCustomDragOver := DoCustomDragOver;
  TAdvTreeViewOpen(FTreeView).OnCustomDragDrop := DoCustomDragDrop;
  TAdvTreeViewOpen(FTreeView).OnCustomCopyToClipboard := DoCopyToClipboard;
  TAdvTreeViewOpen(FTreeView).OnCustomCutToClipboard := DoCutToClipboard;
  TAdvTreeViewOpen(FTreeView).OnCustomPasteFromClipboard := DoPasteFromClipboard;
  TAdvTreeViewOpen(FTreeView).OnCustomColumnSort := DoColumnSort;
  TAdvTreeViewOpen(FTreeView).OnCustomLookup := DoLookup;

  CustomizeTreeView(FTreeView);

  Width := 250;
  Height := 350;
  if IsDesignTime then
  begin
    Stroke.Color := MakeGraphicsColor(200, 199, 204);
    InitSample;
  end;
end;

function TAdvCustomTableView.CreateItems: TAdvTableViewItems;
begin
  Result := TAdvTableViewItems.Create(Self);
end;

function TAdvCustomTableView.CreateTreeView: TAdvTreeViewTableView;
begin
  Result := TAdvTreeViewTableView.Create(Self);
end;

procedure TAdvCustomTableView.CustomizeTreeView(
  ATreeView: TAdvTreeView);
begin

end;

procedure TAdvCustomTableView.CutToClipboard(ATextOnly: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  CopyToClipboard(ATextOnly);
  for I := SelectedItemCount - 1 downto 0 do
    RemoveItem(SelectedItems[I]);
  EndUpdate;
end;

destructor TAdvCustomTableView.Destroy;
begin
  FGlobalFont.Free;

  FEditButton.Free;
  FFilterButton.Free;
  FEdit.Free;
  FBackButton.Free;
  FDoneButton.Free;

  FDetailControlTimer.Free;
  FMoreOptions.Free;
  FCategories.Free;
  FReload.Free;
  FLookupBar.Free;
  FCustomChar.Free;
  FDisplayList.Free;
  FCategoryList.Free;
  FLookupBarDisplayList.Free;
  FFilter.Free;
  FCopyItems.Free;
  FHeader.Free;
  FFooter.Free;
  FDefaultItem.Free;
  FInteraction.Free;
  FItems.Free;
  FTreeView.Free;
  FItemAppearance.Free;
  FCategoryAppearance.Free;
  FMoreOptionAppearance.Free;
  inherited;
end;

function TAdvCustomTableView.DetailControlActive: Boolean;
begin
  Result := Assigned(FActiveDetailControl);
end;

procedure TAdvCustomTableView.StopEditMode;
begin
  if (Assigned(FTreeView) and FTreeView.FReloadActive) or not Assigned(FTreeView) then
    Exit;

  FEditMode := False;
  UpdateTableView;
  UnSelectAllItems;
  UnCheckAllItems;
end;

procedure TAdvCustomTableView.StopFiltering;
begin
  if (Assigned(FTreeView) and FTreeView.FReloadActive) or not Assigned(FTreeView) then
    Exit;

  FFilterActive := False;
  RemoveFilters;
  UpdateTableView;
end;

procedure TAdvCustomTableView.DisableInteraction;
begin
  if Assigned(FTreeView) then
    FTreeView.BlockUserInput := True;
end;

procedure TAdvCustomTableView.DoAfterApplyFilter(
  AFilter: TAdvTableViewFilterData);
begin
  if Assigned(OnAfterApplyFilter) then
    OnAfterApplyFilter(Self, AFilter);
end;

procedure TAdvCustomTableView.DoAfterCheckNode(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
        begin
          dp.Item.Checked := True;
          if Assigned(FTreeView) then
            FTreeView.DoAfterSelectNode(ANode);
          DoItemCheckChanged(dp.Item);
        end;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoAfterCopyToClipboard(Sender: TObject);
begin
  if Assigned(OnAfterCopyToClipboard) then
    OnAfterCopyToClipboard(Self);
end;

procedure TAdvCustomTableView.DoAfterCutToClipboard(Sender: TObject);
begin
  if Assigned(OnAfterCutToClipboard) then
    OnAfterCutToClipboard(Self);
end;

procedure TAdvCustomTableView.DoAfterDrawItemMoreOption(
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem;
  AMoreOption: TAdvTableViewMoreOption);
begin
  if Assigned(OnAfterDrawItemMoreOption) then
    OnAfterDrawItemMoreOption(Self, AGraphics, ARect, AItem, AMoreOption);
end;

procedure TAdvCustomTableView.DoAfterDrawNode(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDrawItem) then
    OnAfterDrawItem(Self, AGraphics, ARect, GetItemForNode(ANode));
end;

procedure TAdvCustomTableView.DoAfterDrawNodeCheck(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap);
begin
  if Assigned(OnAfterDrawItemCheck) then
    OnAfterDrawItemCheck(Self, AGraphics, ARect, GetItemForNode(ANode), ACheck);
end;

procedure TAdvCustomTableView.DoAfterDrawNodeIcon(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap);
begin
  if Assigned(OnAfterDrawItemIcon) then
    OnAfterDrawItemIcon(Self, AGraphics, ARect, GetItemForNode(ANode), AIcon);
end;

procedure TAdvCustomTableView.DoAfterDrawNodeText(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; AText: String);
begin
  if Assigned(OnAfterDrawItemText) then
    OnAfterDrawItemText(Self, AGraphics, ARect, GetItemForNode(ANode), AText);
end;

procedure TAdvCustomTableView.DoAfterDrawNodeTitle(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; ATitle: String);
begin
  if Assigned(OnAfterDrawItemTitle) then
    OnAfterDrawItemTitle(Self, AGraphics, ARect, GetItemForNode(ANode), ATitle);
end;

procedure TAdvCustomTableView.DoAfterDrawSortIndicator(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn, ASortIndex: Integer;
  ASortKind: TAdvTreeViewNodesSortKind);
var
  i: Integer;
  sk: TAdvTableViewItemsSortKind;
begin
  if Assigned(OnAfterDrawSortIndicator) then
  begin
    i := Integer(ASortKind);
    sk := TAdvTableViewItemsSortKind(i);
    OnAfterDrawSortIndicator(Self, AGraphics, ARect, ASortIndex, sk);
  end;
end;

procedure TAdvCustomTableView.DoAfterDropItem(AFromItem: TAdvTableViewItem; AToItem: TAdvTableViewItem);
begin
  if Assigned(OnAfterDropItem) then
    OnAfterDropItem(Self, AFromItem, AToItem);
end;

procedure TAdvCustomTableView.DoAfterPasteFromClipboard(Sender: TObject);
begin
  if Assigned(OnAfterPasteFromClipboard) then
    OnAfterPasteFromClipboard(Self);
end;

procedure TAdvCustomTableView.DoAfterReorderItem(AFromItem, AToItem: TAdvTableViewItem);
begin
  if Assigned(OnAfterReorderItem) then
    OnAfterReorderItem(Self, AFromItem, AToItem);
end;

procedure TAdvCustomTableView.DoAfterSelectNode(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnItemSelected) then
    OnItemSelected(Self, GetItemForNode(ANode));

  if Assigned(OnItemSelectionChanged) then
    OnItemSelectionChanged(Self);
end;

procedure TAdvCustomTableView.DoAfterUnCheckNode(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
        begin
          dp.Item.Checked := False;
          if Assigned(FTreeView) then
            FTreeView.DoAfterUnSelectNode(ANode);
          DoItemCheckChanged(dp.Item);
        end;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoAfterUnSelectNode(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnItemUnSelected) then
    OnItemUnSelected(Self, GetItemForNode(ANode));

  if Assigned(OnItemSelectionChanged) then
    OnItemSelectionChanged(Self);
end;

procedure TAdvCustomTableView.DoBackButtonClicked(Sender: TObject);
begin
  HideDetailControl;
end;

procedure TAdvCustomTableView.DoBeforeApplyFilter(
  AFilter: TAdvTableViewFilterData; AAllow: Boolean);
begin
  if Assigned(OnBeforeApplyFilter) then
    OnBeforeApplyFilter(Self, AFilter, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeCopyToClipboard(Sender: TObject;
  var ACanCopy: Boolean);
begin
  if Assigned(OnBeforeCopyToClipboard) then
    OnBeforeCopyToClipboard(Self, ACanCopy);
end;

procedure TAdvCustomTableView.DoBeforeCutToClipboard(Sender: TObject;
  var ACanCut: Boolean);
begin
  if Assigned(OnBeforeCutToClipboard) then
    OnBeforeCutToClipboard(Self, ACanCut);
end;

procedure TAdvCustomTableView.DoBeforeDrawItemMoreOption(
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem;
  AMoreOption: TAdvTableViewMoreOption; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawItemMoreOption) then
    OnBeforeDrawItemMoreOption(Self, AGraphics, ARect, AItem, AMoreOption, ADefaultDraw);
end;

procedure TAdvCustomTableView.DoBeforeDrawNode(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; ANode: TAdvTreeViewVirtualNode;
  var AAllow, ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawItem) then
    OnBeforeDrawItem(Self, AGraphics, ARect, GetItemForNode(ANode), AAllow, ADefaultDraw);
end;

procedure TAdvCustomTableView.DoBeforeDrawNodeCheck(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; ACheck: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemCheck) then
    OnBeforeDrawItemCheck(Self, AGraphics, ARect, GetItemForNode(ANode), ACheck, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeDrawNodeIcon(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; AIcon: TAdvBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemIcon) then
    OnBeforeDrawItemIcon(Self, AGraphics, ARect, GetItemForNode(ANode), AIcon, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeDrawNodeTitle(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; ATitle: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemTitle) then
    OnBeforeDrawItemTitle(Self, AGraphics, ARect, GetItemForNode(ANode), ATitle, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeDrawNodeText(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawItemText) then
    OnBeforeDrawItemText(Self, AGraphics, ARect, GetItemForNode(ANode), AText, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeDrawSortIndicator(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn, ASortIndex: Integer;
  ASortKind: TAdvTreeViewNodesSortKind; var ADefaultDraw: Boolean);
var
  i: Integer;
  sk: TAdvTableViewItemsSortKind;
begin
  if Assigned(OnBeforeDrawSortIndicator) then
  begin
    i := Integer(ASortKind);
    sk := TAdvTableViewItemsSortKind(i);
    OnBeforeDrawSortIndicator(Self, AGraphics, ARect, ASortIndex, sk, ADefaultDraw);
  end;
end;

procedure TAdvCustomTableView.DoBeforeDropItem(AFromItem: TAdvTableViewItem; AToItem: TAdvTableViewItem; var ACanDrop: Boolean);
begin
  if Assigned(OnBeforeDropItem) then
    OnBeforeDropItem(Self, AFromItem, AToItem, ACanDrop);
end;

procedure TAdvCustomTableView.DoBeforeItemHideDetailControl(
  AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean);
begin
  if Assigned(OnBeforeItemHideDetailControl) then
    OnBeforeItemHideDetailControl(Self, AItem, ADetailControl, AAllow);
end;

procedure TAdvCustomTableView.DoBeforeItemShowDetailControl(
  AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean);
begin
  if Assigned(OnBeforeItemShowDetailControl) then
    OnBeforeItemShowDetailControl(Self, AItem, ADetailControl, AAllow);
end;

procedure TAdvCustomTableView.DoBeforePasteFromClipboard(Sender: TObject;
  var ACanPaste: Boolean);
begin
  if Assigned(OnBeforePasteFromClipboard) then
    OnBeforePasteFromClipboard(Self, ACanPaste);
end;

procedure TAdvCustomTableView.DoBeforeReorderItem(AFromItem, AToItem: TAdvTableViewItem; var ACanReorder: Boolean);
begin
  if Assigned(OnBeforeReorderItem) then
    OnBeforeReorderItem(Self, AFromItem, AToItem, ACanReorder);
end;

procedure TAdvCustomTableView.DoDoneButtonClicked(Sender: TObject);
begin
  StopEditMode;
  StopFiltering;
end;

procedure TAdvCustomTableView.DoCategoryCompare(ACategory1,
  ACategory2: TAdvTableViewCategory; var ACompareResult: Integer);
begin
  if Assigned(OnCategoryCompare) then
    OnCategoryCompare(Self, ACategory1, ACategory2, ACompareResult);
end;

procedure TAdvCustomTableView.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TAdvCustomTableView.DoItemAccessoryClick(
  AItem: TAdvTableViewItem);
begin
  if Assigned(OnItemAccessoryClick) then
    OnItemAccessoryClick(Self, AItem);
end;

procedure TAdvCustomTableView.DoHeaderAnchorClick(Sender: TObject; AAnchor: String);
begin
  if Assigned(OnHeaderAnchorClick) then
    OnHeaderAnchorClick(Self, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTableView.DoColumnSort(Sender: TObject; AColumn: Integer;
  ASortMode: TAdvTreeViewNodesSortMode);
var
  i: Integer;
begin
  i := Integer(ASortMode);
  Items.Sort(Interaction.Sorting in [tvcsNormalCaseSensitive], TAdvTableViewItemsSortMode(i));
end;

procedure TAdvCustomTableView.DoCopyToClipboard(Sender: TObject);
begin
  CopyToClipboard(Interaction.ClipboardMode = tvcmTextOnly);
end;

procedure TAdvCustomTableView.DoCustomDragDrop(Sender, Source: TObject;
  Point: TPointF);
var
  di, dragi, li, dic, newi: TAdvTableViewItem;
  dragn: TAdvTreeViewVirtualNode;
  b: Boolean;
  tv: TAdvCustomTableViewOpen;
begin
  if FAccepted and Assigned(Source) and (Source is TAdvCustomTreeView) and ((Source as TAdvCustomTreeView).Parent is TAdvCustomTableView)
    and (TAdvCustomTreeViewOpen(Source).Interaction.DragDropMode  <> tdmNone) then
  begin
    dragn := (Source as TAdvCustomTreeView).DragNode;
    if Assigned(dragn) then
    begin
      dragi := TAdvCustomTableViewOpen((Source as TAdvCustomTreeView).Parent).GetItemForNode(dragn);
      di := XYToItem(Point.X, Point.Y);
      if (di <> dragi) and Assigned(dragi) and PtInRectEx(GetItemsRect, Point) then
      begin
        b := True;
        if Assigned(di) then
          DoBeforeDropItem(dragi, di, b)
        else
        begin
          Inc(FUpdateCount);
          newi := Items.Add;
          Dec(FUpdateCount);
          DoBeforeDropItem(dragi, newi, b);
          newi.Free;
        end;

        if b then
        begin
          dic := nil;
          BeginUpdate;
          tv := TAdvCustomTableViewOpen((Source as TAdvCustomTreeView).Parent);
          tv.BeginUpdate;
          if Assigned(di) then
          begin
            case tv.Interaction.DragDropMode of
              tvdmMove:
              begin
                if (Source as TAdvCustomTreeView).Parent = Self then
                begin
                  DragItemMove(dragi, di);
                  dragi.Index := di.Index;
                  dic := di;
                end
                else
                begin
                  li := Items.Add;
                  DragItemAdd(li, dragi, di);
                  tv.DragItemDelete(dragi);
                  li.Assign(dragi);
                  li.Index := di.Index;
                  tv.Items.Delete(dragi.Index);
                  tv.ItemIndex := tv.ItemIndex;
                  dic := li;
                  dragi := nil;
                end;
              end;
              tvdmCopy:
              begin
                li := Items.Add;
                DragItemAdd(li, dragi, di);
                li.Assign(dragi);
                li.Index := di.Index;
                dic := li;
              end;
            end;
          end
          else
          begin
            li := Items.Add;
            DragItemAdd(li, dragi, nil);
            li.Assign(dragi);
            case tv.Interaction.DragDropMode of
              tvdmMove:
              begin
                tv.DragItemDelete(dragi);
                tv.TreeView.RemoveNodeFromSelection(tv.GetNodeForItemIndex(dragi.Index));
                tv.Items.Delete(dragi.Index);
                tv.ItemIndex := tv.ItemIndex;
                dragi := nil;
              end;
            end;
            dic := li;
          end;
          (Source as TAdvCustomTreeView).DragNode := nil;
          if Assigned(dic) then
          begin
            FSaveSelectedItem := dic.Index;
            SetLength(FSaveSelectedItems, 1);
            FSaveSelectedItems[0] := dic.Index;
          end;
          tv.EndUpdate;
          EndUpdate;
          DoAfterDropItem(dragi, dic);
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoCustomDragOver(Sender, Source: TObject;
  Point: TPointF; var Accept: Boolean);
begin
  Accept := (Interaction.DragDropMode <> tvdmNone) and (Source is TAdvCustomTreeView) and Assigned((Source as TAdvCustomTreeView).DragNode);
  Accept := Accept and PtInRectEx(GetItemsRect, Point);
  FAccepted := Accept;
end;

procedure TAdvCustomTableView.CustomizeButtons;
begin

end;

procedure TAdvCustomTableView.DoCustomReorder(Sender: TObject; AFromNode,
  AToNode: TAdvTreeViewVirtualNode);
var
  it, itt: TAdvTableViewItem;
  b: Boolean;
begin
  if not Assigned(AFromNode) or not Assigned(AToNode) then
    Exit;

  b := True;
  it := GetItemForNode(AFromNode);
  itt := GetItemForNode(AToNode);
  DoBeforeReorderItem(it, itt, b);
  if b then
  begin
    BeginUpdate;
    it.Index := itt.Index;
    EndUpdate;
    DoAfterReorderItem(it, itt);
  end;
end;

procedure TAdvCustomTableView.DoCutToClipboard(Sender: TObject);
begin
  CutToClipboard(Interaction.ClipboardMode = tvcmTextOnly);
end;

procedure TAdvCustomTableView.DoDetailControlTimer(Sender: TObject);
var
  d: Double;
  p: Double;
  anim: Boolean;
  cr: TRectF;
begin
  if Assigned(FActiveDetailControl) and Assigned(FTreeView) then
  begin
    if not FShowDetailControl then
      p := 0
    else
    begin
      cr := FTreeView.GetContentRect;
      p := cr.Right - cr.Left;
    end;

    if FTreeView.Interaction.AnimationFactor > 0 then
    begin
      d := Abs(FDetailControlOffset - p) / FTreeView.Interaction.AnimationFactor;
      anim := AnimateDouble(FDetailControlOffset, p, d, 0.01);
    end
    else
      anim := False;

    if not anim then
    begin
      FDetailControlOffset := p;
      FShowDetailControl := not FShowDetailControl;
      if not FShowDetailControl then
      begin
        DoItemHideDetailControl(FActiveDetailItem, FActiveDetailControl);
        FActiveDetailControl.Visible := False;
        FActiveDetailControl.Parent := nil;
        FActiveDetailControl := nil;
        FActiveDetailItem := nil;
      end
      else
        DoItemShowDetailControl(FActiveDetailItem, FActiveDetailControl);

      FDetailControlTimer.Enabled := False;
    end;

    UpdateTableView(True);
    UpdateActiveDetailControl;
  end
  else
    FDetailControlTimer.Enabled := False;
end;

procedure TAdvCustomTableView.DoDrawItemCustomAccessory(
  AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem);
begin
  if Assigned(OnDrawItemCustomAccessory) then
    OnDrawItemCustomAccessory(Self, AGraphics, ARect, AItem);
end;

procedure TAdvCustomTableView.DoDrawNodeExtra(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode);
var
  dp: TAdvTableViewDisplayItem;
  bmp: TAdvBitmap;
  r: TRectF;
  h: Single;
  I: Integer;
  mo: TAdvTableViewMoreOption;
  sz: Single;
  w: Single;
  a: Boolean;
  c, fc: TAdvGraphicsColor;
begin
  if not Assigned(FTreeView) then
    Exit;

  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
        begin
          if (FTreeView.FMoreOffsetItem = dp.Item) and (MoreOptions.Count > 0) then
          begin
            AGraphics.Font.Assign(MoreOptionAppearance.Font);
            AGraphics.Fill.Assign(MoreOptionAppearance.Fill);
            AGraphics.Stroke.Assign(MoreOptionAppearance.Stroke);

            sz := FTreeView.FMoreOffset;
            w := sz / MoreOptions.Count;

            for I := 0 to MoreOptions.Count - 1 do
            begin
              mo := MoreOptions[I];
              if mo.Color <> gcNull then
                AGraphics.Fill.Color := mo.Color;
              if mo.BorderColor <> gcNull then
                AGraphics.Stroke.Color := mo.BorderColor;
              if mo.FontColor <> gcNull then
                AGraphics.Font.Color := mo.FontColor;
              r := RectF(ARect.Right - 1 + w * I, ARect.Top, ARect.Right - 1 + w * (I + 1), ARect.Bottom);
              a := True;
              DoBeforeDrawItemMoreOption(AGraphics, r, dp.Item, mo, a);
              if a then
              begin
                if FTreeview.FDownMoreOption = mo then
                begin
                  AGraphics.Fill.Color := Lighter(AGraphics.Fill.Color, 20);
                  AGraphics.Stroke.Color := Lighter(AGraphics.Stroke.Color, 20);
                end;

                AGraphics.DrawRectangle(r, gcrmNone);
                InflateRectEx(r, -2, -2);
                AGraphics.DrawText(r, mo.Text, False, gtaCenter);
                DoAfterDrawItemMoreOption(AGraphics, r, dp.Item, mo);
              end;
            end;
          end;

          if (dp.Item.AccessoryWidth > 0) and (dp.Item.AccessoryHeight > 0) and (dp.Item.Accessory <> tviaNone) then
          begin
            r := ARect;
            if dp.Item = FTreeView.FMoreOffsetItem then
              r.Left := r.Left + FTreeView.FMoreOffset;

            InflateRectEx(r, -4, -4);
            h := dp.Item.AccessoryHeight;
            r := RectF(r.Left, r.Top + ((r.Bottom - r.Top) - h) / 2, r.Right, r.Top + ((r.Bottom - r.Top) - h) / 2 + h);

            AGraphics.Font.Assign(ItemAppearance.AccessoryFont);

            if dp.Item.AccessoryFontColor <> gcNull then
              AGraphics.Font.Color := dp.Item.AccessoryFontColor;

            case dp.Item.Accessory of
              tviaDetail:
              begin
                bmp := TAdvGraphics.GetScaledBitmap(ItemAppearance.AccessoryDetailBitmaps, 0, PictureContainer);
                if Assigned(bmp) and not IsBitmapEmpty(bmp) then
                  AGraphics.DrawBitmap(r, bmp);
              end;
              tviaProgress:
              begin
                c := ItemAppearance.AccessoryFill.Color;
                if dp.Item.AccessoryColor <> gcNull then
                  c := dp.Item.AccessoryColor;
                fc := ItemAppearance.AccessoryFont.Color;
                if dp.Item.AccessoryFontColor <> gcNull then
                  fc := dp.Item.AccessoryFontColor;
                AGraphics.DrawProgressBar(r, dp.Item.AccessoryProgress, '%.0f%%', 100, c, fc);
              end;
              tviaBadge: DrawBadge(AGraphics, r, dp.Item);
              tviaButton:
              begin
                AGraphics.DrawButton(r, dp.Item = FActiveAccessoryItem);
                AGraphics.DrawText(r, dp.Item.AccessoryText, False, gtaCenter, gtaCenter);
              end;
              tviaCustom: DoDrawItemCustomAccessory(AGraphics, r, dp.Item);
            end;
          end;
        end;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoEditButtonClicked(Sender: TObject);
begin
  StartEditMode;
end;

procedure TAdvCustomTableView.DoEditChange(Sender: TObject);
var
  f: TAdvTableViewFilterData;
  a: Boolean;
begin
  RemoveFilters;
  f := Filter.Add;
  f.CaseSensitive := False;
  f.Condition := '*'+FEdit.Text+'*';

  a := True;
  DoBeforeApplyFilter(f, a);
  if a then
  begin
    ApplyFilter;
    DoAfterApplyFilter(f);
  end;
end;

procedure TAdvCustomTableView.DoEnter;
begin
  inherited;
  if Assigned(FTreeView) and FTreeView.AllowFocus then
    FTreeView.SetFocus;
end;

procedure TAdvCustomTableView.DoFilterSelect(Sender: TObject; AColumn: integer;
  var ACondition: string);
begin
  if Assigned(OnFilterSelect) then
    OnFilterSelect(Self, ACondition);
end;

procedure TAdvCustomTableView.DoFooterAnchorClick(Sender: TObject; AAnchor: String);
begin
  if Assigned(OnFooterAnchorClick) then
    OnFooterAnchorClick(Self, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTableView.DoGetColumnHorizontalTextAlign(Sender: TObject;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
begin
  case AKind of
    ikColumnTop: AHorizontalTextAlign := Header.HorizontalTextAlign;
    ikColumnBottom: AHorizontalTextAlign := Footer.HorizontalTextAlign;
  end;
end;

procedure TAdvCustomTableView.DoGetColumnText(Sender: TObject;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String);
begin
  case AKind of
    ikColumnTop: AText := Header.Text;
    ikColumnBottom: AText := Footer.Text;
  end;
end;

procedure TAdvCustomTableView.DoGetColumnTrimming(Sender: TObject;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var ATrimming: TAdvGraphicsTextTrimming);
begin
  case AKind of
    ikColumnTop: ATrimming := Header.Trimming;
    ikColumnBottom: ATrimming := Footer.Trimming;
  end;
end;

procedure TAdvCustomTableView.DoGetColumnVerticalTextAlign(Sender: TObject;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AVerticalTextAlign: TAdvGraphicsTextAlign);
begin
  case AKind of
    ikColumnTop: AVerticalTextAlign := Header.VerticalTextAlign;
    ikColumnBottom: AVerticalTextAlign := Footer.VerticalTextAlign;
  end;
end;

procedure TAdvCustomTableView.DoGetColumnWordWrapping(Sender: TObject;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AWordWrapping: Boolean);
begin
  case AKind of
    ikColumnTop: AWordWrapping := Header.WordWrapping;
    ikColumnBottom: AWordWrapping := Footer.WordWrapping;
  end;
end;

procedure TAdvCustomTableView.DoGetHTMLTemplate(AItem: TAdvTableViewItem;
  var AHTMLTemplate: string);
begin
  if Assigned(OnGetHTMLTemplate) then
    OnGetHTMLTemplate(Self, AItem, AHTMLTemplate);
end;

procedure TAdvCustomTableView.DoGetHTMLTemplateValue(
  AItem: TAdvTableViewItem; AName: string; var AValue: string);
begin
  if Assigned(OnGetHTMLTemplateValue) then
    OnGetHTMLTemplateValue(Self, AItem, AName, AValue);
end;

procedure TAdvCustomTableView.DoIsNodeExtended(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikCategory: AExtended := True;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeCheckType(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ACheckType: TAdvTreeViewNodeCheckType);
var
  dp: TAdvTableViewDisplayItem;
  i: Integer;
begin
  if FEditMode then
  begin
    if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
    begin
      dp := FDisplayList[ANode.Row];
      case dp.Kind of
        tvikItem:
        begin
          if Assigned(dp.Item) then
          begin
            i := Integer(dp.Item.CheckType);
            ACheckType := TAdvTreeViewNodeCheckType(i);
          end;
        end;
        tvikCategory: ;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeDisabledTextColor(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ADisabledTextColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ADisabledTextColor := dp.Item.DisabledTextColor;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeDisabledTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ADisabledTitleColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ADisabledTitleColor := dp.Item.DisabledTitleColor;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeHeight(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if ItemAppearance.Height > -1 then
          AHeight := ItemAppearance.Height;

        if Assigned(dp.Item) and (dp.Item.Height > -1) then
          AHeight := dp.Item.Height;
      end;
      tvikCategory:
      begin
        if CategoryAppearance.Height > -1 then
          AHeight := CategoryAppearance.Height;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeHorizontalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AHorizontalTextAlign := dp.Item.HorizontalTextAlign;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeVerticalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvGraphicsTextAlign);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AVerticalTextAlign := dp.Item.VerticalTextAlign;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeIcon(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvBitmap);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
        begin
          if Assigned(PictureContainer) and (dp.Item.BitmapName <> '') then
            AIcon := TAdvBitmap(PictureContainer.FindBitmap(dp.Item.BitmapName));

          if not Assigned(AIcon) then
            AIcon := dp.Item.Bitmap;
        end;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeSelectedTextColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ASelectedTextColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ASelectedTextColor := dp.Item.SelectedTextColor;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeSelectedTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ASelectedTitleColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ASelectedTitleColor := dp.Item.SelectedTitleColor;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeSides(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ASides: TAdvGraphicsSides);
var
  dp: TAdvTableViewDisplayItem;
  n: TAdvTreeViewVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
    begin
      dp := FDisplayList[ANode.Row];
      if not FTreeView.IsVirtualNodeSelected(ANode) and not (dp.Kind = tvikCategory) then
      begin
        if FTreeView.GetLastVirtualNode = ANode then
          ASides := []
        else
        begin
          n := FTreeView.GetNextVirtualNode(ANode);
          if Assigned(n) and (n.Row >= 0) and (n.Row <= FDisplayList.Count - 1) then
          begin
            dp := FDisplayList[n.Row];
            if dp.Kind = tvikItem then
              ASides := [gsBottom]
            else
              ASides := [];
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.HideDetailControl;
var
  a: Boolean;
begin
  if (Assigned(FTreeView) and FTreeView.FReloadActive) or not Assigned(FTreeView) or not FShowDetailControl then
    Exit;

  FTreeView.StopAnimationTimer;

  a := True;
  DoBeforeItemHideDetailControl(FActiveDetailItem, FActiveDetailControl, a);

  if Assigned(FActiveDetailControl) and Assigned(FTreeView) and a then
  begin
    FShowDetailControl := True;
    UpdateActiveDetailControl;
    FDetailControlTimer.Enabled := True;
  end;
end;

procedure TAdvCustomTableView.HideMoreOptions;
begin
  if (Assigned(FTreeView) and (FTreeView.FReloadActive or FFilterActive or Assigned(FActiveDetailControl))) or not Assigned(FTreeView) then
    Exit;

  FTreeView.FAnimateMoreOptions := False;
  FTreeView.FAnimateMoreOptionsClose := True;
  FTreeView.FMoreOffsetTo := 0;
  FTreeView.FAnimTimer.Enabled := True;
  FTreeView.BlockMouseClick := False;
end;

function TAdvCustomTableView.HTMLReplace(AValue: string; AItem: TAdvTableViewItem): string;
var
  beforetag, aftertag, nm, vl: string;
  i, j: integer;
begin
  Result := '';
  if not Assigned(AItem) then
    Exit;

  beforetag := '';

  while (Pos('<#', AValue) > 0) and (Pos('>', AValue) > 0) do
  begin
    i := pos('<#', AValue);
    beforetag := beforetag + copy(AValue, 1, i - 1); //part prior to the tag
    aftertag := copy(AValue, i, length(AValue)); //part after the tag
    j := pos('>', aftertag);
    nm := copy(aftertag, 1, j - 1);
    Delete(nm, 1, 2);
    Delete(AValue, 1, i + j - 1);
    vl := AItem.HTMLTemplateItems.Values[nm];
    DoGetHTMLTemplateValue(AItem, nm, vl);
    beforetag := beforetag + vl;
  end;

  Result := beforetag + AValue;
end;

procedure TAdvCustomTableView.DoGetNodeTitleExpanded(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExpanded: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AExpanded := dp.Item.TitleExpanded;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitle(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var ATitle: String);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ATitle := dp.Item.GetTitle;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitleColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATitleColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ATitleColor := dp.Item.TitleColor;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitleHorizontalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvGraphicsTextAlign);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AHorizontalTextAlign := dp.Item.TitleHorizontalTextAlign;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitleTrimming(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ATrimming := dp.Item.TitleTrimming;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitleVerticalTextAlign(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AVerticalTextAlign: TAdvGraphicsTextAlign);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AVerticalTextAlign := dp.Item.TitleVerticalTextAlign;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTitleWordWrapping(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AWordWrapping := dp.Item.TitleWordWrapping;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeText(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AText := dp.Item.GetText;
      end;
      tvikCategory: AText := dp.Text;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTextColor(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvGraphicsColor);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ATextColor := dp.Item.TextColor;
      end;
      tvikCategory: ATextColor := CategoryAppearance.Font.Color;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeTrimming(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvGraphicsTextTrimming);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          ATrimming := dp.Item.Trimming;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeWordWrapping(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AWordWrapping := dp.Item.WordWrapping;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNodeExtraSize(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AExtraSize: Single);
var
  dp: TAdvTableViewDisplayItem;
begin
  if not Assigned(FTreeView) then
    Exit;

  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) and (dp.Item.AccessoryWidth > 0) and (dp.Item.Accessory <> tviaNone) then
          AExtraSize := dp.Item.AccessoryWidth + 8;

        if dp.Item = FTreeView.FMoreOffsetItem then
          AExtraSize := AExtraSize + FTreeView.FMoreOffset;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoGetNumberOfNodes(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer);
begin
  if ANode.Level = -1 then
    ANumberOfNodes := FDisplayList.Count;
end;

procedure TAdvCustomTableView.DoIsNodeChecked(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AIsChecked: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AIsChecked := dp.Item.Checked;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoIsNodeEnabled(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  if (ANode.Row >= 0) and (ANode.Row <= FDisplayList.Count - 1) then
  begin
    dp := FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          AEnabled := dp.Item.Enabled;
      end;
      tvikCategory: ;
    end;
  end;
end;

procedure TAdvCustomTableView.DoLookup(Sender: TObject; ALookupString: String);
begin
  LookupItem(ALookupString, Interaction.Lookup.CaseSensitive, Interaction.Lookup.AutoSelect)
end;

procedure TAdvCustomTableView.DoItemMoreOptionClick(AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption);
begin
  if Assigned(OnItemMoreOptionClick) then
    OnItemMoreOptionClick(Self, AItem, AMoreOption);
end;

procedure TAdvCustomTableView.DoItemShowDetailControl(
  AItem: TAdvTableViewItem; ADetailControl: TControl);
begin
  if Assigned(OnItemShowDetailControl) then
    OnItemShowDetailControl(Self, AItem, ADetailControl);
end;

procedure TAdvCustomTableView.DoNeedFilterDropDownData(Sender: TObject;
  AColumn: Integer; AValues: TStrings);
begin
  if Assigned(OnNeedFilterDropDownData) then
    OnNeedFilterDropDownData(Self, AValues);
end;

procedure TAdvCustomTableView.DoNodeAnchorClick(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String);
begin
  if Assigned(OnItemAnchorClick) then
    OnItemAnchorClick(Self, GetItemForNode(ANode), AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTableView.DoNodeClick(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
var
  it: TAdvTableViewItem;
begin
  it := GetItemForNode(ANode);

  if Assigned(it) and (Interaction.ShowDetailMode = tvdtAfterSelectItem) and not FEditMode then
    it.ShowDetailControl;

  if Assigned(it) and FEditMode then
  begin
    if it.Checked then
    begin
      it.Checked := False;
      FTreeView.DoAfterUnSelectNode(ANode);
    end
    else
    begin
      it.Checked := True;
      FTreeView.DoAfterSelectNode(ANode);
    end;

    SelectAllCheckedItems;
  end;

  if Assigned(OnItemClick) then
    OnItemClick(Self, GetItemForNode(ANode));

  {$IFDEF FNCLIB}
  if Assigned(Parent) and (Parent is TAdvCustomSelectorEx) and Assigned(Parent.Owner) and (Parent.Owner is TAdvControlPicker)then
  begin
//    Update ControlPicker if assigned as control
    if Assigned(it) then
      (Parent.Owner as TAdvControlPicker).CallItemClicked(it.Index)
    else
      (Parent.Owner as TAdvControlPicker).UpdateDropDown;
  end;
  {$ENDIF}
end;

procedure TAdvCustomTableView.DoNodeMouseEnter(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnItemMouseEnter) then
    OnItemMouseEnter(Self, GetItemForNode(ANode));
end;

procedure TAdvCustomTableView.DoNodeMouseLeave(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnItemMouseLeave) then
    OnItemMouseLeave(Self, GetItemForNode(ANode));
end;

procedure TAdvCustomTableView.DoNodeTitleAnchorClick(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String);
begin
  if Assigned(OnItemTitleAnchorClick) then
    OnItemTitleAnchorClick(Self, GetItemForNode(ANode), AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomTableView.DoItemCheckChanged(AItem: TAdvTableViewItem);
begin
  if Assigned(OnItemCheckChanged) then
    OnItemCheckChanged(Self, AItem);
end;

procedure TAdvCustomTableView.DoItemCompare(AItem1: TAdvTableViewItem; AItem2: TAdvTableViewItem; var ACompareResult: Integer);
begin
  if Assigned(OnItemCompare) then
    OnItemCompare(Self, AItem1, AItem2, ACompareResult);
end;

procedure TAdvCustomTableView.DoItemHideDetailControl(
  AItem: TAdvTableViewItem; ADetailControl: TControl);
begin
  if Assigned(OnItemHideDetailControl) then
    OnItemHideDetailControl(Self, AItem, ADetailControl);
end;

procedure TAdvCustomTableView.DoNodeDblClick(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnItemDblClick) then
    OnItemDblClick(Self, GetItemForNode(ANode));
end;

procedure TAdvCustomTableView.DoPasteFromClipboard(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TAdvCustomTableView.DoFilterButtonClicked(Sender: TObject);
begin
  StartFiltering;
end;

procedure TAdvCustomTableView.DoStartReload;
begin
  if Assigned(OnStartReload) then
    OnStartReload(Self);
end;

procedure TAdvCustomTableView.DoStopReload;
begin
  if Assigned(OnStopReload) then
    OnStopReload(Self);
end;

procedure TAdvCustomTableView.DoVScroll(Sender: TObject; APosition: Single);
begin
  if Assigned(OnVScroll) then
    OnVScroll(Self, APosition);
end;

procedure TAdvCustomTableView.DragItemAdd(ANewItem, AAssignItem, AInsertItem: TAdvTableViewItem);
begin
end;

procedure TAdvCustomTableView.DragItemDelete(AItem: TAdvTableViewItem);
begin
end;

procedure TAdvCustomTableView.DragItemMove(AFromItem, AToItem: TAdvTableViewItem);
begin
end;

procedure TAdvCustomTableView.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  inherited;
  if not FFirstLoad then
  begin
    FFirstLoad := True;
    UpdateControlAfterResize;
  end;
end;

procedure TAdvCustomTableView.DrawBadge(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvTableViewItem);
var
  r: TRectF;
  sz: TSizeF;
  s: String;
  pth: TAdvGraphicsPath;
  rnd: Single;
begin
  if (AItem.AccessoryText = '') then
    Exit;

  AGraphics.Font.Assign(ItemAppearance.AccessoryFont);
  AGraphics.Stroke.Assign(ItemAppearance.AccessoryStroke);
  AGraphics.Fill.Assign(ItemAppearance.AccessoryFill);

  if AItem.AccessoryFontColor <> gcNull then
    AGraphics.Font.Color := AItem.AccessoryFontColor;

  if AItem.AccessoryBorderColor <> gcNull then
    AGraphics.Stroke.Color := AItem.AccessoryBorderColor;

  if AItem.AccessoryColor <> gcNull then
    AGraphics.Fill.Color := AItem.AccessoryColor;

  s := AItem.AccessoryText;
  sz := AGraphics.CalculateTextSize(s);
  sz.cy := sz.cy + 4;
  sz.cx := Max(sz.cy, sz.cx + 6);

  r := ARect;

  r := RectF(r.Left + ((r.Right - r.Left) - sz.cx) / 2, r.Top + ((r.Bottom - r.Top) - sz.cy) / 2,
    r.Left + ((r.Right - r.Left) - sz.cx) / 2 + sz.cx, r.Top + ((r.Bottom - r.Top) - sz.cy) / 2 + sz.cy);

  rnd := (r.Bottom - r.Top) / 2;

  pth := TAdvGraphicsPath.Create;
  try
    pth.AddArc(PointF(r.Left + rnd, r.Top + rnd), PointF(rnd, rnd), 180, 90);
    pth.AddArc(PointF(r.Right - rnd, r.Top + rnd), PointF(rnd, rnd), -90, 90);
    pth.AddArc(PointF(r.Right - rnd, r.Bottom - rnd), PointF(rnd, rnd), 0, 90);
    pth.AddArc(PointF(r.Left + rnd, r.Bottom - rnd), PointF(rnd, rnd), -270, 90);
    pth.ClosePath;
    AGraphics.DrawPath(pth);
  finally
    pth.Free;
  end;

  AGraphics.DrawText(r, s, False, gtaCenter, gtaCenter);
end;

procedure TAdvCustomTableView.StartEditMode;
begin
  if (Assigned(FTreeView) and FTreeView.FReloadActive) or not Assigned(FTreeView) then
    Exit;

  FEditMode := True;
  UpdateTableView;
  SelectAllCheckedItems ;
end;

procedure TAdvCustomTableView.StartFiltering;
begin
  if (Assigned(FTreeView) and (FTreeView.FReloadActive or FFilterActive)) or not Assigned(FTreeView) then
    Exit;

  FFilterActive := True;
  RemoveFilters;
  UpdateTableView;
  FEdit.Text := '';
  FEdit.SetFocus;
end;

procedure TAdvCustomTableView.StartReload;
begin
  if Assigned(FTreeView) then
    FTreeView.StartReload;
end;

procedure TAdvCustomTableView.EnableInteraction;
begin
  if Assigned(FTreeView) then
    FTreeView.BlockUserInput := False;
end;

procedure TAdvCustomTableView.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateTableView;

  if Assigned(FTreeView) then
    FTreeView.EndUpdate;

  SelectItem(FSaveSelectedItem);
  SelectItems(FSaveSelectedItems);
end;

function TAdvCustomTableView.FindCategoryWithCharacter(ACategory: String): TAdvTableViewDisplayItem;
var
  I: Integer;
  cat: TAdvTableViewDisplayItem;
begin
  Result.Kind := tvikNone;
  Result.Item := nil;
  Result.CategoryID := -1;
  Result.Text := '';
  for I := 0 to FCategoryList.Count - 1 do
  begin
    cat := FCategoryList[I];
    if cat.Text = ACategory then
    begin
      Result := cat;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.FindCategoryWithCharacterID(ACategoryID: Integer): TAdvTableViewDisplayItem;
var
  I: Integer;
  cat: TAdvTableViewDisplayItem;
begin
  Result.Kind := tvikNone;
  Result.Item := nil;
  Result.CategoryID := -1;
  Result.Text := '';
  for I := 0 to FCategoryList.Count - 1 do
  begin
    cat := FCategoryList[I];
    if cat.CategoryID = ACategoryID then
    begin
      Result := cat;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.FindFirstItemWithCategory(ACategory: String): TAdvTableViewItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if GetCharacterForItem(Items[I]) = ACategory then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TAdvCustomTableView.FindFirstItemWithCategoryID(ACategoryID: Integer): TAdvTableViewItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].CategoryID = ACategoryID then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TAdvCustomTableView.GetPictureContainer: TPictureContainer;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.PictureContainer;
end;

function TAdvCustomTableView.GetCharacter(Idx: Integer): String;
begin
  Result := '';
  if (CategoryType = tvctAlphaNumericLast) or (CategoryType = tvctAlphaBetic) then
  begin
    if Idx < 27 then
      Result := chr(ord('A') + (Idx - 1))
    else
      Result := chr(ord('0') + (Idx - 27));
  end
  else
  begin
    if Idx < 11 then
      Result := chr(ord('0') + (Idx - 1))
    else
      Result := chr(ord('A') + (Idx - 11));
  end;
end;

function TAdvCustomTableView.GetCharacterCount: Integer;
begin
  Result := 26;
  if (CategoryType = tvctAlphaNumericFirst) or (CategoryType = tvctAlphaNumericLast) then
    Result := 36;
end;

function TAdvCustomTableView.GetCharacterForItem(AItem: TAdvTableViewItem): String;
var
  cat: TAdvTableViewCategory;
  s: string;
  idx: Integer;
begin
  Result := '';
  case CategoryType of
    tvctAlphaNumericFirst, tvctAlphaNumericLast, tvctAlphaBetic:
    begin
      s := AItem.StrippedHTMLText;
      if Length(s) > 0 then
      begin
        {$IFDEF ZEROSTRINGINDEX}
        Result := s[0]
        {$ELSE}
        Result := s[1]
        {$ENDIF}
      end
      else
        Result := '';
    end;
    tvctCustom:
    begin
      idx := Categories.ItemIndexByID(AItem.CategoryID);
      if (idx >= 0) and (idx <= Categories.Count - 1) then
      begin
        cat := Categories[idx];
        Result := cat.Text;
      end;
    end;
  end;
end;

function TAdvCustomTableView.GetChecked(AItemIndex: Integer): Boolean;
begin
  Result := False;
  if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
    Result := Items[AItemIndex].Checked;
end;

function TAdvCustomTableView.GetCheckedItem(
  AItem: TAdvTableViewItem): Boolean;
begin
  Result := False;
  if Assigned(AItem) then
    Result := AItem.Checked;
end;

function TAdvCustomTableView.GetDisplayCategoryIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := FDisplayList.Count - 1 downto 0 do
  begin
    if (FDisplayList[I].Kind = tvikCategory) and (I <= AIndex) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.GetDisplayItemIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := FDisplayList.Count - 1 downto 0 do
  begin
    if (FDisplayList[I].Kind = tvikItem) and (I <= AIndex) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/' + LowerCase(ClassName);
end;

function TAdvCustomTableView.GetFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := TAdvTreeViewOpen(FTreeView).Fill;
end;

function TAdvCustomTableView.GetItemForNode(ANode: TAdvTreeViewVirtualNode): TAdvTableViewItem;
var
  i: Integer;
begin
  Result := nil;
  i := GetItemIndexForNode(ANode);
  if (i >= 0) and (i <= Items.Count - 1) then
    Result := Items[i];
end;

function TAdvCustomTableView.GetItemIndex: Integer;
var
  it: TAdvTableViewItem;
begin
  Result := -1;
  if Assigned(FTreeView) and (FTreeView.SelectedVirtualNodeCount > 0) and Assigned(FTreeView.SelectedVirtualNodes[0]) then
  begin
    it := GetItemForNode(FTreeView.SelectedVirtualNode);
    if Assigned(it) then
      Result := it.Index
    else if FSaveSelectedItem <> -1 then
      Result := FSaveSelectedItem;
  end;
end;

function TAdvCustomTableView.GetItemIndexForNode(ANode: TAdvTreeViewVirtualNode): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(ANode) then
  begin
    for I := 0 to FDisplayList.Count - 1 do
    begin
      if (FDisplayList[I].Row = ANode.Row) and Assigned(FDisplayList[I].Item) and Assigned(FDisplayList[I].Item.Collection) then
      begin
        Result := FDisplayList[I].Item.Index;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTableView.GetItemsFromClipboard: TAdvTableViewCopyItems;
var
  s: String;
  a: TStringList;
  AItem: TAdvTableViewItem;
  I: Integer;
  CurrStr: String;
  templ: TAdvCustomTableView;
begin
  if TAdvClipBoard.HasFormat(TAdvClipBoardFormat.cfText) then
  begin
    s := TAdvClipBoard.GetText;
    if (Pos(CLP_FMT, s) > 0) then
    begin
      templ := TAdvCustomTableView.Create(nil);
      a := TStringList.Create;
      try
        s := StringReplace(s, CLP_FMT, '', [rfReplaceAll]);
        a.Text := s;
        try
          for I := 0 to a.Count - 1 do
          begin
            CurrStr := a[i];
            AItem := templ.AddItem;
            AItem.LoadFromString(CurrStr);
          end;
        finally
        end;
      finally
        FCopyItems.Assign(templ.Items);
        a.Free;
        templ.Free;
      end;
    end;
  end;

  Result := FCopyItems;
end;

function TAdvCustomTableView.GetItemsRect: TRectF;
begin
  Result := EmptyRect;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNodesRect;
end;

function TAdvCustomTableView.GetMoreOptionsSize: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MoreOptions.Count - 1 do
    Result := Result + MoreOptions[I].Width;
end;

function TAdvCustomTableView.GetNextDisplayCategoryIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AIndex + 1 to FDisplayList.Count - 1 do
  begin
    if FDisplayList[I].Kind = tvikCategory then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.GetNodeForItemIndex(AItemIndex: Integer): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := TAdvTreeViewOpen(FTreeView).GetNodeForRow(GetRowForItemIndex(AItemIndex));
end;

function TAdvCustomTableView.GetRowForItemIndex(AItemIndex: Integer): Integer;
var
  I: Integer;
  dp: TAdvTableViewDisplayItem;
begin
  Result := -1;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    dp := FDisplayList[I];
    if Assigned(dp.Item) and Assigned(dp.Item.Collection) and (dp.Item.Index = AItemIndex) and (dp.Kind = tvikItem) then
    begin
      Result := dp.Item.FRow;
      Break;
    end;
  end;
end;

function TAdvCustomTableView.GetSelectedItem: TAdvTableViewItem;
begin
  Result := nil;
  if (ItemIndex >= 0) and (ItemIndex <= Items.Count - 1) then
    Result := Items[ItemIndex];
end;

function TAdvCustomTableView.GetSelectedItems: TAdvTableViewSelectedItems;
var
  I: Integer;
  n: TAdvTableViewItem;
begin
  Result := nil;
  for I := 0 to SelectedItemCount - 1 do
  begin
    n := SelectedItems[I] as TAdvTableViewItem;
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := n;
  end;
end;

function TAdvCustomTableView.GetSelItem(AIndex: Integer): TAdvTableViewItem;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := GetItemForNode(FTreeView.SelectedVirtualNodes[AIndex]);
end;

function TAdvCustomTableView.GetStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := TAdvTreeViewOpen(FTreeView).Stroke;
end;

{$IFDEF FNCLIB}
function TAdvCustomTableView.GetSubComponentArray: TAdvGraphicsStylesManagerComponentArray;
begin
  SetLength(Result, 1);
  Result[0] := FTreeView;
end;
{$ENDIF}

function TAdvCustomTableView.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

function TAdvCustomTableView.GetVerticalScrollBarVisible: Boolean;
begin
  Result := False;
  if Assigned(FTreeView) then
    Result := FTreeView.VerticalScrollBarVisible;
end;

procedure TAdvCustomTableView.InitSample;
begin
  BeginUpdate;
  Items.Clear;

  AddItem('Bugfix 376');
  AddItem('Bugfix 378');
  AddItem('Customer Update');
  AddItem('Install New server');
  AddItem('Meeting: Next Phase');
  AddItem('Release Planning');

  ResetToDefaultStyle;

  GlobalFont.Name := 'Segoe UI';

  EndUpdate;
end;

function TAdvCustomTableView.IsAppearanceProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  Result := inherited IsAppearanceProperty(AObject, APropertyName, APropertyType);
  Result := Result or (APropertyName = 'Header') or (APropertyName = 'Footer');
end;

function TAdvCustomTableView.IsItemSelectable(AItem: TAdvTableViewItem): Boolean;
begin
  Result := True;
  if Assigned(AItem) then
  begin
    Result := AItem.Enabled;
    if not (TAdvTreeViewOpen(FTreeView).VisibleNodes.IndexOf(GetNodeForItemIndex(AItem.Index)) > -1) then
      Result := False;
  end
end;

function TAdvCustomTableView.ItemFromDifferentCategory(AItem1, AItem2: TAdvTableViewItem): Boolean;
var
  s1, s2: string;
begin
  Result := False;
  if (AItem1 = nil) or (AItem2 = nil) then
  begin
    Result := true
  end
  else
  begin
    case CategoryType of
      tvctAlphaBetic, tvctAlphaNumericFirst, tvctAlphaNumericLast:
      begin
        s1 := AItem1.StrippedHTMLText;
        s2 := AItem2.StrippedHTMLText;
        if (Length(s1) > 0) and (Length(s2) > 0) then
        begin
          {$IFDEF ZEROSTRINGINDEX}
          Result := (s1[0] <> s2[0]);
          {$ELSE}
          Result := (s1[1] <> s2[1]);
          {$ENDIF}
        end
        else
        begin
          if (Length(s1) = 0) and (Length(s2) = 0) then
            Result := false
          else
            Result := true;
        end;
      end;
      tvctCustom:
      begin
        if (AItem1.CategoryID > -1) and (AItem2.CategoryID > -1) then
          Result := (AItem1.CategoryID <> AItem2.CategoryID)
        else
          Result := not ((AItem1.CategoryID = -1) and (AItem2.CategoryID = -1));
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.Loaded;
begin
  inherited;
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  TAdvTreeViewOpen(FTreeView).ChangeDPIScale(96, DesigntimeFormPixelsPerInch);
  FTreeView.ScaleForPPI(CurrentPPI);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomTableView.LoadFromFile(AFileName: string);
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

procedure TAdvCustomTableView.LoadFromStream(AStream: TStream);
var
  List: TStringList;
  AItem: TAdvTableViewItem;
  i: Integer;
  CurrStr: string;
begin
  List := TStringList.Create;
  BeginUpdate;
  try
    try
      Items.Clear;
      List.LoadFromStream(AStream);
      for i := 0 to List.Count - 1 do
      begin
        CurrStr := List[i];
        AItem := AddItem;
        if Assigned(AItem) then
          AItem.LoadFromString(CurrStr);
      end;
    finally
      EndUpdate;
      List.Free;
    end;
  except
  end;
end;
{$ENDIF}

procedure TAdvCustomTableView.LoadFromStrings(AStrings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  Items.Clear;
  for I := 0 to AStrings.Count - 1 do
    Items.Add.Text := AStrings[I];
  EndUpdate;
end;

procedure TAdvCustomTableView.LoadSettingsFromFile(AFileName: string);
var
  c: TAdvGraphicsColor;
  I: Integer;
begin
  c := DefaultItem.TextColor;
  inherited;
  if (c = DefaultItem.TextColor) and (c <> ItemAppearance.Font.Color) then
  begin
    DefaultItem.TextColor := ItemAppearance.Font.Color;
    for I := 0 to Items.Count - 1 do
      Items[I].TextColor := DefaultItem.TextColor;
  end;
end;

procedure TAdvCustomTableView.LoadSettingsFromStream(AStream: TStreamEx);
var
  c: TAdvGraphicsColor;
  I: Integer;
begin
  c := DefaultItem.TextColor;
  inherited;
  if (c = DefaultItem.TextColor) and (c <> ItemAppearance.Font.Color) then
  begin
    DefaultItem.TextColor := ItemAppearance.Font.Color;
    for I := 0 to Items.Count - 1 do
      Items[I].TextColor := DefaultItem.TextColor;
  end;
end;

procedure TAdvCustomTableView.LookupCategory(ACategory: String);
begin
  if LookupBar.AutoLookup then
    ScrollToCategoryCharacter(ACategory)
  else
  begin
    if Assigned(OnManualLookupCategory) then
      OnManualLookupCategory(Self, ACategory, -1);
  end;
end;

procedure TAdvCustomTableView.LookupCustomCategory(ACategoryID: Integer);
begin
  if LookupBar.AutoLookup then
    ScrollToCategoryID(ACategoryID)
  else
  begin
    if Assigned(OnManualLookupCategory) then
      OnManualLookupCategory(Self, '', ACategoryID);
  end;
end;

function TAdvCustomTableView.LookupItem(ALookupString: String; ACaseSensitive,
  AAutoSelect: Boolean): TAdvTableViewItem;
var
  v: String;
  it: TAdvTableViewItem;
  i: Integer;
  s: String;
begin
  Result := nil;
  v := ALookupString;
  if not ACaseSensitive then
    v := Uppercase(v);

  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I];
    s := it.StrippedHTMLText;

    if not ACaseSensitive then
      s := UpperCase(s);

    if Pos(v, s) = 1 then
    begin
      Result := it;
      Break;
    end;
  end;

  if Assigned(Result) and IsItemSelectable(Result) and AAutoSelect then
  begin
    SelectItem(Result.Index);
    ScrollToItem(Result.Index);
  end;
end;

function TAdvCustomTableView.MatchFilter(
  AItem: TAdvTableViewItem): Boolean;
var
  i: Integer;
  s:string;
  temp: Boolean;
begin
  Result := True;
  for i := 1 to FFilter.Count do
  begin
    with FFilter.Items[i - 1] do
    begin
      s := AItem.StrippedHTMLText;
      s := Trim(s);

      if Prefix <> '' then
        if Pos(Prefix,s) = 1 then
          Delete(s,1,Length(FFilter.Items[i - 1].Prefix));

      if Suffix <> '' then
        if Pos(Suffix,s) = 1 + Length(s) - Length(Suffix) then
          Delete(s,1 + Length(s) - Length(Suffix),Length(s));

      temp := true;
      try
        if FFilter.Items[i - 1].Condition <> '' then
          temp := TAdvUtils.MatchStrEx(Condition,s,CaseSensitive);
      except
        temp := false;
      end;

      case FFilter.Items[i - 1].Operation of
        tvfoSHORT:
        begin
          Result := temp;
          if not Result then
            Break;
        end;
        tvfoNONE: Result := temp;
        tvfoAND: Result := Result AND temp;
        tvfoOR:  Result := Result OR temp;
        tvfoXOR: Result := Result XOR temp;
      end;
    end;
  end;
end;

procedure TAdvCustomTableView.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if AComponent = Items[i].FDetailControl then
        Items[i].FDetailControl := nil;
    end;

    if Assigned(DefaultItem) and (AComponent = DefaultItem.FDetailControl) then
      DefaultItem.FDetailControl := nil;
  end;
end;

procedure TAdvCustomTableView.PasteFromClipboard;
var
  s: String;
  a: TStringList;
  AItem: TAdvTableViewItem;
  I: Integer;
  CurrStr: String;
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
          for I := 0 to a.Count - 1 do
          begin
            CurrStr := a[i];
            AItem := AddItem;
            AItem.LoadFromString(CurrStr);
          end;
        finally
        end;
      finally
        a.Free;
      end;
    end;
  end;
end;

function TAdvCustomTableView.ReloadProgress: Single;
begin
  Result := 0;
  if Assigned(FTreeView) and (Reload.ProgressMode = tvrpmManual) then
    Result := FTreeView.FManualProgressSweepAngle / 360 * 100;
end;

procedure TAdvCustomTableView.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvCustomTableView);
end;

procedure TAdvCustomTableView.RemoveFilter;
begin
  FFilterApplied := False;
  UpdateTableView;
end;

procedure TAdvCustomTableView.RemoveFilters;
begin
  FFilterApplied := False;
  Filter.Clear;
  UpdateTableView;
end;

procedure TAdvCustomTableView.RemoveItem(AItem: TAdvTableViewItem);
begin
  if Assigned(AItem) then
    Items.Delete(AItem.Index);
end;

procedure TAdvCustomTableView.ResetToDefaultStyle;
var
  I: Integer;
begin
  inherited;

  BeginUpdate;

  Stroke.Color := MakeGraphicsColor(178, 178, 178);
  Header.Fill.Kind := gfkSolid;
  Footer.Fill.Kind := gfkSolid;
  Header.Stroke.Color := Stroke.Color;
  Footer.Stroke.Color := Stroke.Color;

  TAdvUtils.SetFontSize(Header.Font, 16);
  TAdvUtils.SetFontSize(Footer.Font, 16);

  TAdvUtils.SetFontSize(Header.Font, 14);
  Header.Font.Style := [TFontStyle.fsBold];
  TAdvUtils.SetFontSize(Footer.Font, 14);
  ItemAppearance.Stroke.Kind := gskNone;
  {$IFDEF FMXLIB}
  Header.Fill.Color := $FFEEF2F9;
  Header.Font.Color := $FF454545;
  Footer.Fill.Color := $FFEEF2F9;
  Footer.Font.Color := $FF454545;
  ItemAppearance.SelectedFill.Color := $FFF6F8FC;
  ItemAppearance.SelectedStroke.Color := $FF2D9BEF;
  ItemAppearance.Font.Color := $FF7A7A7A;
  ItemAppearance.SelectedStroke.Color := $FF2D9BEF;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Header.Fill.Color := $F9F2EE;
  Header.Font.Color := $454545;
  Footer.Fill.Color := $F9F2EE;
  Footer.Font.Color := $454545;
  ItemAppearance.SelectedFill.Color := $FCF8F6;
  ItemAppearance.SelectedStroke.Color := $EF9B2D;
  ItemAppearance.Font.Color := $7A7A7A;
  ItemAppearance.SelectedStroke.Color := $EF9B2D;
  {$ENDIF}
  ItemAppearance.ShowFocus := False;

  Header.Height := ScalePaintValue(36);

  for I := 0 to Items.Count - 1 do
  begin
    Items[I].SelectedTitleColor := Header.Font.Color;
    Items[I].TitleColor := Header.Font.Color;
    Items[I].SelectedTextColor := Header.Font.Color;
    Items[I].TextColor := ItemAppearance.Font.Color;
  end;

  EndUpdate;
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomTableView.SaveToFile(AFileName: string; ATextOnly: Boolean = True);
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

procedure TAdvCustomTableView.SaveToStream(AStream: TStream; ATextOnly: Boolean = True);
var
  AItem: TAdvTableViewItem;
  ItemStr: string;
  Buffer: TBytes;
  K: Integer;
  {$IFDEF LCLLIB}
  I: Integer;
  {$ENDIF}
begin
  for K := 0 to Items.Count - 1 do
  begin
    AItem := Items[K];
    ItemStr := AItem.SaveToString(ATextOnly);
    ItemStr := ItemStr + EndOfLine;
    {$IFNDEF LCLLIB}
    Buffer := TEncoding.Default.GetBytes(ItemStr);
    {$ENDIF}
    {$IFDEF LCLLIB}
    SetLength(Buffer, Length(ItemStr));
    for I := 1 to Length(ItemStr) do
      Buffer[I - 1] := Ord(ItemStr[I]);
    {$ENDIF}

    {$IFDEF CMNLIB}
    AStream.Write(Buffer[0], Length(Buffer));
    {$ELSE}
    AStream.Write(Buffer, Length(Buffer));
    {$ENDIF}
  end;
end;

procedure TAdvCustomTableView.SaveToStrings(AStrings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    AStrings.Add(Items[I].Text);
end;
{$ENDIF}

procedure TAdvCustomTableView.ScrollToCategoryCharacter(ACharacter: String);
var
  cat: TAdvTableViewDisplayItem;
  t: TAdvTreeViewOpen;
begin
  if Assigned(FTreeView) then
  begin
    cat := FindCategoryWithCharacter(ACharacter);
    if (cat.Row >= 0) and (cat.Row <= FDisplayList.Count - 1) then
    begin
      t := TAdvTreeViewOpen(FTreeView);
      t.ScrollToVirtualNode(t.GetNodeForRow(cat.Row), True, tvnspTop, True);
    end;
  end;
end;

procedure TAdvCustomTableView.ScrollToCategoryId(ACategoryID: Integer);
var
  cat: TAdvTableViewDisplayItem;
  t: TAdvTreeViewOpen;
begin
  if Assigned(FTreeView) then
  begin
    cat := FindCategoryWithCharacterID(ACategoryID);
    if (cat.Row >= 0) and (cat.Row <= FDisplayList.Count - 1) then
    begin
      t := TAdvTreeViewOpen(FTreeView);
      t.ScrollToVirtualNode(t.GetNodeForRow(cat.Row), True, tvnspTop, True);
    end;
  end;
end;

procedure TAdvCustomTableView.ScrollToItem(AItemIndex: Integer);
var
  t: TAdvTreeViewOpen;
begin
  if Assigned(FTreeView) then
  begin
    if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
    begin
      t := TAdvTreeViewOpen(FTreeView);
      t.ScrollToVirtualNode(t.GetNodeForRow(GetRowForItemIndex(AItemIndex)), True, tvnspTop, True);
    end;
  end;
end;

procedure TAdvCustomTableView.SelectAllCheckedItems;
var
  I: Integer;
  arr: TAdvTableViewIntegerArray;
begin
  SetLength(arr, 0);
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].Checked then
    begin
      SetLength(arr, Length(arr) + 1);
      arr[Length(arr) - 1] := I;
    end;
  end;

  SelectItems(arr);
end;

function TAdvCustomTableView.SelectedItemCount: Integer;
begin
  Result := 0;
  if Assigned(FTreeView) then
    Result := FTreeView.SelectedVirtualNodeCount;
end;

procedure TAdvCustomTableView.SelectItem(AItemIndex: Integer);
var
  t: TAdvTreeViewOpen;
  i: Integer;
begin
  if Assigned(FTreeView) and (AItemIndex <> -1) then
  begin
    i := Max(0, Min(AItemIndex, Items.Count - 1));
    if (i >= 0) and (i <= Items.Count - 1) then
    begin
      t := TAdvTreeViewOpen(FTreeView);
      t.SelectVirtualNode(t.GetNodeForRow(GetRowForItemIndex(i)));
    end;
  end;
end;

procedure TAdvCustomTableView.SelectItems(
  AItemIndexes: TAdvTableViewIntegerArray);
var
  t: TAdvTreeViewOpen;
  I: Integer;
  na: TAdvTreeViewVirtualNodeArray;
begin
  if Assigned(FTreeView) then
  begin
    t := TAdvTreeViewOpen(FTreeView);
    SetLength(na, Length(AItemIndexes));
    for I := 0 to Length(AItemIndexes) - 1 do
      na[I] := t.GetNodeForRow(GetRowForItemIndex(AItemIndexes[I]));

    t.SelectVirtualNodes(na);
  end;
end;

procedure TAdvCustomTableView.SetItemAppearance(
  const Value: TAdvTableViewItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

procedure TAdvCustomTableView.SetLookupBar(const Value: TAdvTableViewLookupBar);
begin
  FLookupBar.Assign(Value);
end;

procedure TAdvCustomTableView.SetMoreOptionAppearance(
  const Value: TAdvTableViewMoreOptionAppearance);
begin
  FMoreOptionAppearance.Assign(Value);
end;

procedure TAdvCustomTableView.SetMoreOptions(
  const Value: TAdvTableViewMoreOptions);
begin
  FMoreOptions.Assign(Value);
end;

procedure TAdvCustomTableView.SetReload(
  const Value: TAdvTableViewReload);
begin
  FReload.Assign(Value);
end;

procedure TAdvCustomTableView.SetSelectedItem(const Value: TAdvTableViewItem);
begin
  if Assigned(Value) then
    ItemIndex := Value.Index;
end;

procedure TAdvCustomTableView.SetStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTreeView) then
    TAdvTreeViewOpen(FTreeView).Stroke.Assign(Value);
end;

procedure TAdvCustomTableView.SetVerticalScrollBarVisible(
  const Value: Boolean);
begin
  if Assigned(FTreeView) then
    FTreeView.VerticalScrollBarVisible := Value;
end;

procedure TAdvCustomTableView.ShowDetailControl(AItemIndex: Integer);
var
  a: Boolean;
  cr: TRectF;
begin
  if (Assigned(FTreeView) and FTreeView.FReloadActive) or not Assigned(FTreeView) then
    Exit;

  FTreeView.StopAnimationTimer;

  HideDetailControl;

  if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
  begin
    FActiveDetailControl := Items[AItemIndex].DetailControl;
    FActiveDetailItem := Items[AItemIndex];
  end;

  if not Assigned(FActiveDetailControl) then
    FActiveDetailControl := DefaultItem.DetailControl;

  a := True;
  DoBeforeItemShowDetailControl(FActiveDetailItem, FActiveDetailControl, a);

  if Assigned(FActiveDetailControl) and Assigned(FTreeView) and a then
  begin
    FShowDetailControl := False;
    FActiveDetailControl.Parent := Self;
    cr := FTreeView.GetContentRect;
    FDetailControlOffset := cr.Right - cr.Left;
    UpdateActiveDetailControl;
    FActiveDetailControl.Visible := True;
    FDetailControlTimer.Enabled := True;
  end
  else
  begin
    FActiveDetailControl := nil;
    FActiveDetailItem := nil;
  end;
end;

procedure TAdvCustomTableView.ShowMoreOptions(AItem: TAdvTableViewItem);
begin
  if (Assigned(FTreeView) and (FTreeView.FReloadActive or FFilterActive or Assigned(FActiveDetailControl))) or not Assigned(FTreeView) then
    Exit;

  FTreeView.FMoreOffsetItem := AItem;
  FTreeView.FMoreOffsetTo := GetMoreOptionsSize;
  FTreeView.BlockMouseClick := True;
  FTreeView.FAnimateMoreOptions := True;
  FTreeView.FAnimTimer.Enabled := True;
end;

procedure TAdvCustomTableView.Sort(ACaseSensitive: Boolean;
  ASortingMode: TAdvTableViewItemsSortMode);
begin
  Items.Sort(ACaseSensitive, ASortingMode);
end;

procedure TAdvCustomTableView.StopReload;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FManualProgressSweepAngle := 0;
    FTreeView.FManualProgressStartAngle := 0;
    FTreeView.FAnimTimer.Enabled := False;
    FTreeView.BlockUserInput := False;
    FTreeView.FReloadActive := False;
    FTreeView.SetVerticalOffset(0);
    FTreeView.StartVerticalOffsetAnimation;
    DoStopReload;
  end;
end;

procedure TAdvCustomTableView.ToggleEditMode;
begin
  if FEditMode then
    StopEditMode
  else
    StartEditMode;
end;

procedure TAdvCustomTableView.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  if Assigned(FTreeView) then
    FTreeView.AdaptToStyle := AdaptToStyle;
end;

procedure TAdvCustomTableView.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FTreeView.PictureContainer := Value;
end;

procedure TAdvCustomTableView.SetCategories(
  const Value: TAdvTableViewCategories);
begin
  FCategories.Assign(Value);
end;

procedure TAdvCustomTableView.SetCategoryAppearance(
  const Value: TAdvTableViewCategoryAppearance);
begin
  FCategoryAppearance.Assign(Value);
end;

procedure TAdvCustomTableView.SetCategoryType(
  const Value: TAdvTableViewCategoryType);
begin
  if FCategoryType <> Value then
  begin
    FCategoryType := Value;
    UpdateTableView;
  end;
end;

procedure TAdvCustomTableView.SetChecked(AItemIndex: Integer;
  const Value: Boolean);
begin
  if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
    Items[AItemIndex].Checked := Value;
end;

procedure TAdvCustomTableView.SetCheckedItem(
  AItem: TAdvTableViewItem; const Value: Boolean);
begin
  if Assigned(AItem) then
    AItem.Checked := Value;
end;

procedure TAdvCustomTableView.SetDefaultItem(const Value: TAdvTableViewItem);
begin
  FDefaultItem.Assign(Value);
end;

procedure TAdvCustomTableView.SetFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTreeView) then
    TAdvTreeViewOpen(FTreeView).Fill.Assign(Value);
end;

procedure TAdvCustomTableView.SetFooter(const Value: TAdvTableViewFooter);
begin
  FFooter.Assign(Value);
end;

procedure TAdvCustomTableView.SetFonts(ASetType: TAdvAppearanceGlobalFontType);
var
  I: Integer;
begin
  BeginUpdate;

  GlobalFont.ApplyChange(Footer.Font, ASetType);
  GlobalFont.ApplyChange(Header.Font, ASetType);

  GlobalFont.ApplyChange(ItemAppearance.Font, ASetType);
  GlobalFont.ApplyChange(ItemAppearance.AccessoryFont, ASetType);

  GlobalFont.ApplyChange(LookupBar.Font, ASetType);
  GlobalFont.ApplyChange(LookupBar.InActiveFont, ASetType);

  if ASetType = aftColor then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      Items[I].TextColor := GlobalFont.Color;
      Items[I].AccessoryFontColor := GlobalFont.Color;
      Items[I].SelectedTextColor := GlobalFont.Color;
      Items[I].FSelectedTitleColor := GlobalFont.Color;
    end
  end;

  EndUpdate;
end;

procedure TAdvCustomTableView.SetGlobalFont(const Value: TAdvAppearanceGlobalFont);
begin
  FGlobalFont.Assign(Value);
end;

procedure TAdvCustomTableView.SetHeader(const Value: TAdvTableViewHeader);
begin
  FHeader.Assign(Value);
end;

procedure TAdvCustomTableView.SetInteraction(
  const Value: TAdvTableViewInteraction);
begin
  FInteraction.Assign(Value);
end;

procedure TAdvCustomTableView.SetItemIndex(const Value: Integer);
begin
  SelectItem(Value);
end;

procedure TAdvCustomTableView.SetItems(const Value: TAdvTableViewItems);
begin
  FItems.Assign(Value);
end;

procedure TAdvCustomTableView.UnCheckAllItems;
var
  I: Integer;
begin
  BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Checked := False;
  EndUpdate;
end;

procedure TAdvCustomTableView.UnSelectAllItems;
begin
  if Assigned(FTreeView) then
    FTreeView.UnSelectAllNodes;
end;

procedure TAdvCustomTableView.UpdateActiveDetailControl;
var
  r: TRectF;
begin
  if Assigned(FActiveDetailControl) and Assigned(FTreeView) then
  begin
    r := FTreeView.GetContentRect;
    InflateRectEx(r, -1, -1);
    r := RectF(r.Left + FDetailControlOffset, r.Top, r.Right + FDetailControlOffset, r.Bottom);
    FActiveDetailControl.SetBounds(Round(r.Left), Round(R.Top), Round((r.Right - r.Left)), Round((r.Bottom - r.Top)));
  end;
end;

procedure TAdvCustomTableView.UpdateControlAfterResize;
begin
  inherited;
  if Assigned(FTreeView) then
    FTreeView.SetBounds(0, 0, Width, Height);

  UpdateTableView(False);
  UpdateActiveDetailControl;
end;

procedure TAdvCustomTableView.UpdateReloadProgress(AProgress: Single; AAutoStopReload: Boolean = True);
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FManualProgressStartAngle := 0;
    FTreeView.FManualProgressSweepAngle := AProgress / 100 * 360;

    if AAutoStopReload and (AProgress >= 100) then
      StopReload
    else
      FTreeView.Invalidate;
  end;
end;

procedure TAdvCustomTableView.UpdateTableView(ADisplayOnly: Boolean = False);
var
  sel: TAdvTableViewItem;
  cr, hr: TRectF;
  szd: Single;
  szr: Single;
  szt: Single;
  btnh: Single;
  b: Boolean;
  a: TAdvTableViewItemArray;
  sela: TAdvTableViewIntegerArray;
  I: Integer;
begin
  if (FUpdateCount > 0) or IsDestroying then
    Exit;

  if Assigned(FTreeView) then
  begin
    if not ADisplayOnly then
      CalculateItems;

    sel := SelectedItem;
    a := GetSelectedItems;
    SetLength(sela, Length(a));
    for I := 0 to Length(a) - 1 do
    begin
      if Assigned(a[I]) then
        sela[I] := a[I].Index;
    end;

    if not FDetailControlTimer.Enabled then
    begin
      if FShowDetailControl then
        FDetailControlOffset := 0
      else
      begin
        cr := FTreeView.GetContentRect;
        FDetailControlOffset := cr.Right - cr.Left;
      end;
    end;

    if ADisplayOnly then
      FTreeView.UpdateTreeViewDisplay
    else
    begin
      FTreeView.BeginUpdate;
      FTreeView.ClearNodeList;
      FTreeView.EndUpdate;
    end;

    CalculateLookupBar;
    SelectedItem := sel;
    SelectItems(sela);

    hr := FTreeView.GetColumnsTopRect;    

    b := False;
    if Assigned(FColumn) then
      b := FColumn.Expanded;

    if not IsDesignTime and FFilterActive and b then
      FEdit.Parent := Self
    else
      FEdit.Parent := nil;

    FDoneButton.Visible := FEditMode or FFilterActive and not Assigned(FActiveDetailControl) and b;
    if not IsDesignTime and ((Header.Height > 0) and Header.Visible) then
      FDoneButton.Parent := Self
    else
      FDoneButton.Parent := nil;

    FDoneButton.Invalidate;

    FBackButton.Visible := Assigned(FActiveDetailControl) and ((FShowDetailControl and not FDetailControlTimer.Enabled) or not FShowDetailControl) and b;
    if not IsDesignTime and ((Header.Height > 0) and Header.Visible) then
      FBackButton.Parent := Self
    else
      FBackButton.Parent := nil;

    FBackButton.Invalidate;

    FEditButton.Visible := not FEditMode and not FFilterActive and Interaction.ShowEditButton and not Assigned(FActiveDetailControl) and b;
    if not IsDesignTime and ((Header.Height > 0) and Header.Visible) then
      FEditButton.Parent := Self
    else
      FEditButton.Parent := nil;

    FEditButton.Invalidate;

    FFilterButton.Visible := Interaction.ShowFilterButton and not FFilterActive and not Assigned(FActiveDetailControl) and b;
    if not IsDesignTime and ((Header.Height > 0) and Header.Visible) then
      FFilterButton.Parent := Self
    else
      FFilterButton.Parent := nil;

    FFilterButton.Invalidate;

    btnh := hr.Bottom - hr.Top;
    {$IFDEF LCLLIB}
    if btnh < ScalePaintValue(4) then
      btnh := ScalePaintValue(4);
    {$ENDIF}

    FBackButton.Height := Round(btnh - ScalePaintValue(4));
    FDoneButton.Height := Round(btnh - ScalePaintValue(4));
    FEditButton.Height := Round(btnh - ScalePaintValue(4));
    FFilterButton.Height := Round(btnh - ScalePaintValue(4));

    CustomizeButtons;

    szt := 0;
    if Assigned(FColumn) then
    begin
      if FColumn.Filtering.Enabled then
      begin
        szd := FColumn.Filtering.ButtonSize;
        szt := szt + szd + ScalePaintValue(4);
      end;

      if FColumn.Expandable then
      begin
        szr := FColumn.ExpandingButtonSize;
        szt := szt + szr + ScalePaintValue(6);
      end;

      if (TAdvTreeViewColumnOpen(FColumn).SortKind <> nskNone) then
      begin
        szr := ScalePaintValue(8);
        szt := szt + szr + ScalePaintValue(6);
      end;
    end;

    FBackButton.SetBounds(Round(hr.Left + ScalePaintValue(3)), Round(hr.Top + ((hr.Bottom - hr.Top) - FBackButton.Height) / 2), FBackButton.Width, FBackButton.Height);
    if FFilterButton.Visible then
    begin
      FFilterButton.SetBounds(Round(hr.Right - ScalePaintValue(3) - FFilterButton.Width - szt), Round(hr.Top + ((hr.Bottom - hr.Top) - FFilterButton.Height) / 2), FFilterButton.Width, FFilterButton.Height);
      FDoneButton.SetBounds(Round(hr.Right - ScalePaintValue(6) - FDoneButton.Width - FFilterButton.Width - szt), Round(hr.Top + Round(((hr.Bottom - hr.Top) - FDoneButton.Height) / 2)), FDoneButton.Width, FDoneButton.Height);
      FEditButton.SetBounds(Round(hr.Right - ScalePaintValue(6) - FEditButton.Width - FFilterButton.Width - szt), Round(hr.Top + Round(((hr.Bottom - hr.Top) - FEditButton.Height) / 2)), FEditButton.Width, FEditButton.Height);
    end
    else
    begin
      FDoneButton.SetBounds(Round(hr.Right - ScalePaintValue(3) - FDoneButton.Width - szt), Round(hr.Top + ((hr.Bottom - hr.Top) - FDoneButton.Height) / 2), FDoneButton.Width, FDoneButton.Height);
      FEditButton.SetBounds(Round(hr.Right - ScalePaintValue(3) - FEditButton.Width - szt), Round(hr.Top + ((hr.Bottom - hr.Top) - FEditButton.Height) / 2), FEditButton.Width, FEditButton.Height);
    end;

    if LookupBar.Visible and (CategoryType <> tvctNone) then
    begin
      case LookupBar.Position of
        tvlbpLeft: FEdit.SetBounds(Round(hr.Left + 1 + LookupBar.Width), Round(hr.Bottom), Round((hr.Right - hr.Left) - LookupBar.Width - 2), FEdit.Height);
        tvlbpRight: FEdit.SetBounds(Round(hr.Left + 1), Round(hr.Bottom + 1), Round((hr.Right - hr.Left) - LookupBar.Width - 2), FEdit.Height);
      end;
    end
    else
      FEdit.SetBounds(Round(hr.Left + 1), Round(hr.Bottom), Round((hr.Right - hr.Left) - 2), FEdit.Height);

    FTreeView.Invalidate;
  end;
end;

function TAdvCustomTableView.XYToAccessoryItem(X,
  Y: Single): TAdvTableViewItem;
var
  n: TAdvTreeViewVirtualNode;
  r: TRectF;
  it: TAdvTableViewItem;
  h: Single;
begin
  Result := nil;
  if Assigned(FTreeView) then
  begin
    n := FTreeView.XYToNode(X, Y);
    if Assigned(n) and (n.Row >= 0) and (n.Row <= Items.Count - 1)  then
    begin
      it := GetItemForNode(n);
      if Assigned(it) and (it.Accessory in Interaction.AccessoryClickDetection) then
      begin
        r := n.ExtraRects[0];
        InflateRectEx(r, 0, -4);
        h := it.AccessoryHeight;
        r := RectF(r.Left, r.Top + ((r.Bottom - r.Top) - h) / 2, r.Right, r.Top + ((r.Bottom - r.Top) - h) / 2 + h);
        if PtInRectEx(r, PointF(X, Y)) then
          Result := it;
      end;
    end;
  end;
end;

function TAdvCustomTableView.XYToItem(X, Y: Single): TAdvTableViewItem;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
  begin
    n := FTreeView.XYToNode(X, Y);
    if Assigned(n) and (n.Row >= 0) and (n.Row <= FDisplayList.Count - 1) then
      Result := GetItemForNode(n);
  end;
end;

function TAdvCustomTableView.XYToItemIndex(X, Y: Single): Integer;
var
  it: TAdvTableViewItem;
begin
  Result := - 1;
  it := XYToItem(X, Y);
  if Assigned(it) then
    Result := it.Index;
end;

function TAdvCustomTableView.XYToMoreOption(X,
  Y: Single): TAdvTableViewMoreOption;
var
  sz: Single;
  w: Single;
  r, rt: TRectF;
  n: TAdvTreeViewVirtualNode;
  I: Integer;
  mo: TAdvTableViewMoreOption;
begin
  Result := nil;

  if not Assigned(FTreeView) then
    Exit;

  n := FTreeView.XYToNode(X, Y, FTreeView.FMoreOffset);

  if not Assigned(n) or (GetItemForNode(n) <> FTreeView.FMoreOffsetItem) then
    Exit;

  if MoreOptions.Count > 0 then
  begin
    rt := n.ExtraRects[0];
    sz := FTreeView.FMoreOffset;
    w := sz / MoreOptions.Count;

    for I := 0 to MoreOptions.Count - 1 do
    begin
      mo := MoreOptions[I];
      r := RectF(rt.Right + w * I, rt.Top, rt.Right + w * (I + 1), rt.Bottom);
      if PtInRectEx(r, PointF(X, Y)) then
      begin
        Result := mo;
        Break;
      end;
    end;
  end;
end;

{$IFDEF FNCLIB}
{ TAdvTreeView picker implementation }

procedure TAdvCustomTableView.PickerApplyFilter(ACondition: string; ACaseSensitive: Boolean);
begin
  ApplyFilter;
end;

function TAdvCustomTableView.PickerGetContent: String;
begin
  Result := '';

  if ItemIndex >= 0 then
    Result := Items[ItemIndex].Text;
end;

function TAdvCustomTableView.PickerGetFirstSelectableItem: Integer;
begin
  if Items.Count > 0 then
    Result := 0
  else
    Result := -1;
end;

function TAdvCustomTableView.PickerGetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TAdvCustomTableView.PickerGetItemHeight: Single;
begin
  Result := ItemAppearance.FixedHeight;
end;

function TAdvCustomTableView.PickerGetItemWidth: Single;
begin
  Result := Width;
end;

function TAdvCustomTableView.PickerGetLastSelectableItem: Integer;
begin
  if Items.Count > 0 then
    Result := Items.Count - 1
  else
    Result := -1;
end;

function TAdvCustomTableView.PickerGetNextSelectableItem(AItemIndex: Integer): Integer;
begin
  if AItemIndex < Items.Count - 1 then
    Result := AItemIndex + 1
  else
    Result := AItemIndex;
end;

function TAdvCustomTableView.PickerGetPreviousSelectableItem(AItemIndex: Integer): Integer;
begin
  if AItemIndex > 0 then
    Result := AItemIndex - 1
  else
    Result := AItemIndex;
end;

function TAdvCustomTableView.PickerGetSelectedItem: Integer;
begin
  Result := ItemIndex;
end;

function TAdvCustomTableView.PickerGetVisibleItemCount: Integer;
begin
  if Assigned(FTreeView) then
    Result := TAdvTreeViewOpen(FTreeView).VisibleNodes.Count
  else
    Result := 0;
end;

function TAdvCustomTableView.PickerLookupItem(ALookupString: String; ACaseSensitive: Boolean): TAdvControlPickerFilterItem;
var
  it: TAdvTableViewItem;
begin
  it := LookupItem(ALookupString, ACaseSensitive);

  Result.ItemIndex := it.Index;
  Result.ItemText := it.Text;
end;

procedure TAdvCustomTableView.PickerResetFilter;
begin
  RemoveFilters;
end;

procedure TAdvCustomTableView.PickerSelectItem(AItemIndex: Integer);
begin
  if (AItemIndex >= 0) and (AItemIndex <= Items.Count - 1) then
    ItemIndex := AItemIndex;
end;

procedure TAdvCustomTableView.PickerSetItemHeight(AValue: Single);
begin
  ItemAppearance.FixedHeight := AValue;
end;

procedure TAdvCustomTableView.PickerSetItemWidth(AValue: Single);
begin
  //
end;
{$ENDIF}

{ TAdvTableViewItems }

function TAdvTableViewItems.Add: TAdvTableViewItem;
begin
  Result := TAdvTableViewItem(inherited Add);
end;

procedure TAdvTableViewItems.Clear;
var
  l: TAdvCustomTableView;
begin
  l := TableView;
  if Assigned(l) then
    l.BeginUpdate;
  inherited Clear;
  if Assigned(l) then
    l.EndUpdate;
end;

function TAdvTableViewItems.Compare(AItem1, AItem2: TAdvTableViewItem; ACaseSensitive: Boolean; ASortingMode: TAdvTableViewItemsSortMode): Integer;
var
  cIdx1, cIdx2: integer;
  itemcap1, itemcap2: String;
  l: TAdvCustomTableView;
begin
  Result := 0;

  l := FTableView;
  if Assigned(l) then
  begin
    case l.CategoryType of
      tvctNone, tvctAlphaBetic, tvctAlphaNumericFirst, tvctAlphaNumericLast:
      begin
        if not ACaseSensitive then
          Result := AnsiCompareStr(UpperCase(AItem1.StrippedHTMLText), UpperCase(AItem2.StrippedHTMLText))
        else
          Result := AnsiCompareStr(AItem1.StrippedHTMLText, AItem2.StrippedHTMLText)
      end;
      tvctCustom:
      begin
        cIdx1 := l.Categories.ItemIndexById(AItem1.CategoryID);
        cIdx2 := l.Categories.ItemIndexById(AItem2.CategoryID);

        if cIdx1 < cIdx2 then
          result :=  -1
        else if cIdx1 > cIdx2 then
          result := 1
        else
        begin
          itemcap1 := AItem1.StrippedHTMLText;
          itemcap2 := AItem2.StrippedHTMLText;
          if not ACaseSensitive then
          begin
            itemcap1 := UpperCase(itemcap1);
            itemcap2 := UpperCase(itemcap2);
          end;

          if itemcap1 < itemcap2 then
            result :=  -1
          else if itemcap1 > itemcap2 then
            result := 1
          else
            result := 0
        end;
      end;
    end;

    case ASortingMode of
      tvismDescending: Result := Result * -1;
    end;

    l.DoItemCompare(AItem1, AItem2, Result);
  end;
end;

constructor TAdvTableViewItems.Create(ATableView: TAdvCustomTableView);
begin
  inherited Create(ATableView, GetItemClass);
  FTableView := ATableView;
end;

function TAdvTableViewItems.GetItem(Index: Integer): TAdvTableViewItem;
begin
  Result := TAdvTableViewItem(inherited Items[Index]);
end;

function TAdvTableViewItems.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvTableViewItem;
end;

function TAdvTableViewItems.Insert(Index: Integer): TAdvTableViewItem;
begin
  Result := TAdvTableViewItem(inherited Insert(Index));
end;

function TAdvTableViewItems.TableView: TAdvCustomTableView;
begin
  Result := FTableView;
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

procedure TAdvTableViewItems.QuickSort(L, R: Integer; ACaseSensitive: Boolean; ASortingMode: TAdvTableViewItemsSortMode);
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

procedure TAdvTableViewItems.SetItem(Index: Integer; const Value: TAdvTableViewItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvTableViewItems.Sort(ACaseSensitive: Boolean; ASortingMode: TAdvTableViewItemsSortMode);
var
  l: TAdvCustomTableView;
begin
  l := TableView;
  if Assigned(l) then
  begin
    l.BeginUpdate;
    BeginUpdate;
    if Count > 1 then
      QuickSort(0, Pred(Count), ACaseSensitive, ASortingMode);
    EndUpdate;
    l.EndUpdate;
  end;
end;

{ TAdvTableViewItem }

procedure TAdvTableViewItem.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewItem then
  begin
    FTag := (Source as TAdvTableViewItem).Tag;
    FHTMLTemplateItems.Assign((Source as TAdvTableViewItem).HTMLTemplateItems);
    FText := (Source as TAdvTableViewItem).Text;
    FTrimming := (Source as TAdvTableViewItem).Trimming;
    FWordWrapping := (Source as TAdvTableViewItem).WordWrapping;
    FHorizontalTextAlign := (Source as TAdvTableViewItem).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvTableViewItem).VerticalTextAlign;
    FTextColor := (Source as TAdvTableViewItem).TextColor;
    FSelectedTextColor := (Source as TAdvTableViewItem).SelectedTextColor;
    FDisabledTextColor := (Source as TAdvTableViewItem).DisabledTextColor;
    FTitle := (Source as TAdvTableViewItem).Title;
    FTitleTrimming := (Source as TAdvTableViewItem).TitleTrimming;
    FTitleWordWrapping := (Source as TAdvTableViewItem).TitleWordWrapping;
    FTitleHorizontalTextAlign := (Source as TAdvTableViewItem).TitleHorizontalTextAlign;
    FTitleVerticalTextAlign := (Source as TAdvTableViewItem).TitleVerticalTextAlign;
    FTitleColor := (Source as TAdvTableViewItem).TitleColor;
    FSelectedTitleColor := (Source as TAdvTableViewItem).SelectedTitleColor;
    FDisabledTitleColor := (Source as TAdvTableViewItem).DisabledTitleColor;
    FEnabled := (Source as TAdvTableViewItem).Enabled;
    FAccessory := (Source as TAdvTableViewItem).Accessory;
    FVisible := (Source as TAdvTableViewItem).Visible;
    FBitmapName := (Source as TAdvTableViewItem).BitmapName;
    FBitmap.Assign((Source as TAdvTableViewItem).Bitmap);
    FHeight := (Source as TAdvTableViewItem).Height;
    FCategoryID := (Source as TAdvTableViewItem).CategoryID;
    FAccessoryWidth := (Source as TAdvTableViewItem).AccessoryWidth;
    FAccessoryHeight := (Source as TAdvTableViewItem).AccessoryHeight;
    FAccessoryProgress := (Source as TAdvTableViewItem).AccessoryProgress;
    FAccessoryText := (Source as TAdvTableViewItem).AccessoryText;
    FChecked := (Source as TAdvTableViewItem).Checked;
    FCheckType := (Source as TAdvTableViewItem).CheckType;
    FAccessoryFontColor := (Source as TAdvTableViewItem).AccessoryFontColor;
    FAccessoryBorderColor := (Source as TAdvTableViewItem).AccessoryBorderColor;
    FAccessoryColor := (Source as TAdvTableViewItem).AccessoryColor;
    FEnableMoreOptions := (Source as TAdvTableViewItem).EnableMoreOptions;
    AssignData(Source);
  end;
end;

procedure TAdvTableViewItem.AssignData(Source: TPersistent);
begin
  if Source is TAdvTableViewItem then
  begin
    FDataString := (Source as TAdvTableViewItem).DataString;
    FDataBoolean := (Source as TAdvTableViewItem).DataBoolean;
    FDataObject := (Source as TAdvTableViewItem).DataObject;
    FDataInteger := (Source as TAdvTableViewItem).DataInteger;
    FDataPointer := (Source as TAdvTableViewItem).DataPointer;
    FDBKey := (Source as TAdvTableViewItem).DBKey;
  end;
end;

procedure TAdvTableViewItem.BitmapChanged(Sender: TObject);
begin
  UpdateItem;
end;

procedure TAdvTableViewItem.Changed(Sender: TObject);
begin
  UpdateItem;
end;

constructor TAdvTableViewItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
    FTableView := (Collection as TAdvTableViewItems).TableView;

  FTextColor := gcBlack;
  FEnableMoreOptions := True;
  FTitleExpanded := True;
  FChecked := False;
  FCheckType := tvictNone;
  FCategoryID := -1;
  FAccessoryFontColor := gcNull;
  FAccessoryColor := gcNull;
  FAccessoryBorderColor := gcNull;
  FHeight := -1;
  FEnabled := True;
  FAccessory := tviaNone;
  FAccessoryWidth := 24;
  FAccessoryHeight := 24;
  FAccessoryProgress := 0;
  if Assigned(FTableView) and FTableView.IsDesignTime then
    FAccessoryText := 'Button';
  FVisible := True;
  FHTMLTemplateItems := TStringList.Create;
  FHTMLTemplateItems.OnChange := TemplateItemsChanged;
  FTitleColor := gcBlack;
  FSelectedTextColor := gcBlack;
  FDisabledTextColor := gcSilver;
  FWordWrapping := true;
  FHorizontalTextAlign := gtaLeading;
  FVerticalTextAlign := gtaCenter;
  FTrimming := gttNone;
  FSelectedTitleColor := gcBlack;
  FDisabledTitleColor := gcSilver;
  FTitleWordWrapping := true;
  FTitleHorizontalTextAlign := gtaLeading;
  FTitleVerticalTextAlign := gtaCenter;
  FTitleTrimming := gttNone;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  if Assigned(FTableView) then
    Self.Assign(FTableView.DefaultItem);
  UpdateItem;
end;

destructor TAdvTableViewItem.Destroy;
begin
  FBitmap.Free;
  FHTMLTemplateItems.Free;
  inherited;
  UpdateItem;
end;

procedure TAdvTableViewItem.SetHeight(const Value: Double);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetHTMLTemplateItems(const Value: TStringList);
begin
  if FHTMLTemplateItems <> Value then
  begin
    FHTMLTemplateItems.Assign(Value);
    UpdateItem;
  end;
end;

function TAdvTableViewItem.GetPictureContainer: TPictureContainer;
var
  l: TAdvCustomTableView;
begin
  Result := nil;
  l := TableView;
  if Assigned(l) then
    Result := l.PictureContainer;
end;

function TAdvTableViewItem.GetTableView: TAdvCustomTableView;
begin
  Result := FTableView;
end;

procedure TAdvTableViewItem.TemplateItemsChanged(Sender: TObject);
begin
  UpdateItem;
end;

procedure TAdvTableViewItem.LoadFromString(AString: String);
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

function TAdvTableViewItem.GetStrippedHTMLText: String;
begin
  Result := TAdvUtils.HTMLStrip(GetText);
end;

function TAdvTableViewItem.GetText: string;
var
  ht: string;
  s: string;
begin
  s := '';
  if Assigned(FTableView) then
  begin
    ht := FTableView.ItemAppearance.HTMLTemplate;
    FTableView.DoGetHTMLTemplate(Self, ht);
    if ht = '' then
      s := Text
    else
      s := FTableView.HTMLReplace(ht, Self);
  end;

  Result := s;
end;

function TAdvTableViewItem.GetTitle: string;
begin
  Result := Title;
end;

procedure TAdvTableViewItem.HideDetailControl;
begin
  if Assigned(FTableView) then
    FTableView.HideDetailControl;
end;

function TAdvTableViewItem.IsAccessoryHeightStored: Boolean;
begin
  Result := AccessoryHeight <> 24;
end;

function TAdvTableViewItem.IsAccessoryProgressStored: Boolean;
begin
  Result := AccessoryProgress <> 0;
end;

function TAdvTableViewItem.IsAccessoryWidthStored: Boolean;
begin
  Result := AccessoryWidth <> 24;
end;

function TAdvTableViewItem.IsHeightStored: Boolean;
begin
  Result := Height <> -1;
end;

function TAdvTableViewItem.IsSelected: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(FTableView) then
  begin
    for I := 0 to FTableView.SelectedItemCount - 1 do
    begin
      if FTableView.SelectedItems[I] = Self then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TAdvTableViewItem.SaveToString(ATextOnly: Boolean): String;
var
  s: String;
begin
  if not Assigned(FTableView) then
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

procedure TAdvTableViewItem.SetCategoryID(const Value: integer);
begin
  if FCategoryID <> Value then
  begin
    FCategoryID := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetChecked(const Value: Boolean);
var
  l: TAdvCustomTableView;
  n: TAdvTreeViewVirtualNode;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    l := TableView;
    if Assigned(l) then
    begin
      n := TAdvCustomTableViewOpen(l).GetNodeForItemIndex(Index);
      if Assigned(n) and (n.CheckStates[0] <> FChecked) then
      begin
        if FChecked then
          TAdvCustomTableViewOpen(l).TreeView.CheckVirtualNode(n, 0)
        else
          TAdvCustomTableViewOpen(l).TreeView.UnCheckVirtualNode(n, 0);
      end;
    end;
  end;
end;

procedure TAdvTableViewItem.SetCheckType(
  const Value: TAdvTableViewItemCheckType);
begin
  if FCheckType <> Value then
  begin
    FCheckType := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleExpanded(const Value: Boolean);
begin
  if FTitleExpanded <> Value then
  begin
    FTitleExpanded := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessory(
  const Value: TAdvTableViewItemAccessory);
begin
  if FAccessory <> Value then
  begin
    FAccessory := Value;
    case FAccessory of
      tviaDetail:
      begin
        AccessoryWidth := 32;
        AccessoryHeight := 24;
      end;
      tviaProgress:
      begin
        AccessoryWidth := 75;
        AccessoryHeight := 20;
      end;
      tviaButton:
      begin
        AccessoryWidth := 75;
        AccessoryHeight := 20;
      end;
      tviaBadge:
      begin
        AccessoryWidth := 32;
        AccessoryHeight := 24;
      end;
      tviaCustom: ;
    end;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryBorderColor(const Value: TAdvGraphicsColor);
begin
  if FAccessoryBorderColor <> Value then
  begin
    FAccessoryBorderColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryColor(const Value: TAdvGraphicsColor);
begin
  if FAccessoryColor <> Value then
  begin
    FAccessoryColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryFontColor(const Value: TAdvGraphicsColor);
begin
  if FAccessoryFontColor <> Value then
  begin
    FAccessoryFontColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryHeight(const Value: Single);
begin
  if FAccessoryHeight <> Value then
  begin
    FAccessoryHeight := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryProgress(const Value: Single);
begin
  if FAccessoryProgress <> Value then
  begin
    FAccessoryProgress := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryText(const Value: String);
begin
  if FAccessoryText <> Value then
  begin
    FAccessoryText := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetAccessoryWidth(const Value: Single);
begin
  if FAccessoryWidth <> Value then
  begin
    FAccessoryWidth := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetBitmap(const Value: TAdvBitmap);
begin
  if FBitmap <> Value then
  begin
    FBitmap.Assign(Value);
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetBitmapName(const Value: string);
begin
  if FBitmapName <> Value then
  begin
    FBitmapName := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetDetailControl(const Value: TControl);
begin
  if Value = nil then
  begin
    if Assigned(FDetailControl) then
      FDetailControl.Visible := True;
  end;

  FDetailControl := Value;
  if Assigned(FDetailControl) then
    FDetailControl.Visible := False;
end;

procedure TAdvTableViewItem.SetDisabledTextColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetDisabledTitleColor(const Value: TAdvGraphicsColor);
begin
  if FDisabledTitleColor <> Value then
  begin
    FDisabledTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetSelectedTextColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetSelectedTitleColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedTitleColor <> Value then
  begin
    FSelectedTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTextColor(const Value: TAdvGraphicsColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitle(const Value: String);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleColor(const Value: TAdvGraphicsColor);
begin
  if FTitleColor <> Value then
  begin
    FTitleColor := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FTitleHorizontalTextAlign <> Value then
  begin
    FTitleHorizontalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleTrimming(const Value: TAdvGraphicsTextTrimming);
begin
  if FTitleTrimming <> Value then
  begin
    FTitleTrimming := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FTitleVerticalTextAlign <> Value then
  begin
    FTitleVerticalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTitleWordWrapping(const Value: Boolean);
begin
  if FTitleWordWrapping <> Value then
  begin
    FTitleWordWrapping := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetTrimming(const Value: TAdvGraphicsTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    UpdateItem;
  end;
end;

procedure TAdvTableViewItem.ShowDetailControl;
begin
  if Assigned(FTableView) then
    FTableView.ShowDetailControl(Index);
end;

procedure TAdvTableViewItem.UpdateItem;
var
  l: TAdvCustomTableView;
begin
  l := TableView;
  if Assigned(l) then
    l.UpdateTableView;
end;

{ TAdvTableViewItemAppearance }

procedure TAdvTableViewItemAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewItemAppearance then
  begin
    Font.Assign((Source as TAdvTableViewItemAppearance).Font);
    Fill.Assign((Source as TAdvTableViewItemAppearance).Fill);
    Stroke.Assign((Source as TAdvTableViewItemAppearance).Stroke);
    SelectedFill.Assign((Source as TAdvTableViewItemAppearance).SelectedFill);
    SelectedStroke.Assign((Source as TAdvTableViewItemAppearance).SelectedStroke);
    DisabledFill.Assign((Source as TAdvTableViewItemAppearance).DisabledFill);
    DisabledStroke.Assign((Source as TAdvTableViewItemAppearance).DisabledStroke);
    HeightMode := (Source as TAdvTableViewItemAppearance).HeightMode;
    FixedHeight := (Source as TAdvTableViewItemAppearance).FixedHeight;
    FHTMLTemplate := (Source as TAdvTableViewItemAppearance).HTMLTemplate;
    FAccessoryDetailBitmaps.Assign((Source as TAdvTableViewItemAppearance).AccessoryDetailBitmaps);
    FAccessoryFont.Assign((Source as TAdvTableViewItemAppearance).AccessoryFont);
    FAccessoryStroke.Assign((Source as TAdvTableViewItemAppearance).AccessoryStroke);
    FAccessoryFill.Assign((Source as TAdvTableViewItemAppearance).AccessoryFill);
  end;
end;

constructor TAdvTableViewItemAppearance.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;

  FAccessoryFill := TAdvGraphicsFill.Create(gfkSolid, gcYellowgreen);
  FAccessoryFill.OnChanged := FillChanged;
  FAccessoryStroke := TAdvGraphicsStroke.Create(gskSolid, gcBlack);
  FAccessoryStroke.OnChanged := StrokeChanged;
  FAccessoryFont := TAdvGraphicsFont.Create;
  if Assigned(FTableView) and FTableView.IsDesignTime then
  begin
    FAccessoryFont.Color := gcDarkred;
    FAccessoryFont.Style := [TFontStyle.fsBold];
  end;
  FAccessoryFont.OnChanged := FontChanged;

  FAccessoryDetailBitmaps := TAdvScaledBitmaps.Create(Self);
  FAccessoryDetailBitmaps.OnChange := DoAccessoryDetailBitmapsChanged;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) and FTableView.IsDesignTime then
  begin
    FTableView.FTreeView.NodesAppearance.DisabledFill.Kind := gfkNone;
    FTableView.FTreeView.NodesAppearance.DisabledStroke.Kind := gskNone;
    FTableView.FTreeView.NodesAppearance.SelectedStroke.Assign(FTableView.FTreeView.NodesAppearance.SelectedFill);
  end;
  HeightMode := tvhmVariable;
  FHeight := -1;
end;

destructor TAdvTableViewItemAppearance.Destroy;
begin
  FAccessoryFill.Free;
  FAccessoryStroke.Free;
  FAccessoryFont.Free;
  FAccessoryDetailBitmaps.Free;
  inherited;
end;

procedure TAdvTableViewItemAppearance.DoAccessoryDetailBitmapsChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

procedure TAdvTableViewItemAppearance.FontChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

procedure TAdvTableViewItemAppearance.StrokeChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

procedure TAdvTableViewItemAppearance.FillChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

function TAdvTableViewItemAppearance.GetDisabledFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.DisabledFill;
end;

function TAdvTableViewItemAppearance.GetDisabledStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.DisabledStroke;
end;

function TAdvTableViewItemAppearance.GetFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.Fill;
end;

function TAdvTableViewItemAppearance.GetFixedHeight: Double;
begin
  Result := 0;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.FixedHeight;
end;

function TAdvTableViewItemAppearance.GetFont: TAdvGraphicsFont;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.Font;
end;

function TAdvTableViewItemAppearance.GetHeightMode: TAdvTableViewItemHeightMode;
var
  i: Integer;
begin
  Result := tvhmVariable;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(FTableView.FTreeView.NodesAppearance.HeightMode);
    Result := TAdvTableViewItemHeightMode(i);
  end;
end;

function TAdvTableViewItemAppearance.GetOwner: TPersistent;
begin
  Result := FTableView;
end;

function TAdvTableViewItemAppearance.GetSelectedFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.SelectedFill;
end;

function TAdvTableViewItemAppearance.GetSelectedStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.SelectedStroke;
end;

function TAdvTableViewItemAppearance.GetShowFocus: Boolean;
begin
  Result := True;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.ShowFocus;
end;

function TAdvTableViewItemAppearance.GetStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.Stroke;
end;

function TAdvTableViewItemAppearance.IsHeightStored: Boolean;
begin
  Result := Height <> -1;
end;

procedure TAdvTableViewItemAppearance.SetAccessoryDetailBitmaps(const Value: TAdvScaledBitmaps);
begin
  FAccessoryDetailBitmaps.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetAccessoryFill(const Value: TAdvGraphicsFill);
begin
  FAccessoryFill.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetAccessoryFont(const Value: TAdvGraphicsFont);
begin
  FAccessoryFont.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetAccessoryStroke(const Value: TAdvGraphicsStroke);
begin
  FAccessoryStroke.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetDisabledFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.DisabledFill.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetDisabledStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.DisabledStroke.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.Fill.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetFixedHeight(const Value: Double);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.FixedHeight := Value;
end;

procedure TAdvTableViewItemAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.Font.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetHeight(const Value: Double);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewItemAppearance.SetHeightMode(const Value: TAdvTableViewItemHeightMode);
var
  i: Integer;
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(Value);
    FTableView.FTreeView.NodesAppearance.HeightMode := TAdvTreeViewNodeHeightMode(i);
  end;
end;

procedure TAdvTableViewItemAppearance.SetHTMLTemplate(const Value: string);
begin
  if FHTMLTemplate <> Value then
  begin
    FHTMLTemplate := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewItemAppearance.SetSelectedFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.SelectedFill.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.SelectedStroke.Assign(Value);
end;

procedure TAdvTableViewItemAppearance.SetShowFocus(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.ShowFocus := Value;
end;

procedure TAdvTableViewItemAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.Stroke.Assign(Value);
end;

{ TAdvTableViewCategoryAppearance }

procedure TAdvTableViewCategoryAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewCategoryAppearance then
  begin
    Font.Assign((Source as TAdvTableViewCategoryAppearance).Font);
    Fill.Assign((Source as TAdvTableViewCategoryAppearance).Fill);
    Stroke.Assign((Source as TAdvTableViewCategoryAppearance).Stroke);
    FHeight := (Source as TAdvTableViewCategoryAppearance).Height;
  end;
end;

constructor TAdvTableViewCategoryAppearance.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  FHeight := -1;
end;

destructor TAdvTableViewCategoryAppearance.Destroy;
begin
  inherited;
end;

function TAdvTableViewCategoryAppearance.GetFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.ExtendedFill;
end;

function TAdvTableViewCategoryAppearance.GetFont: TAdvGraphicsFont;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.ExtendedFont;
end;

function TAdvTableViewCategoryAppearance.GetStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.NodesAppearance.ExtendedStroke;
end;

function TAdvTableViewCategoryAppearance.IsHeightStored: Boolean;
begin
  Result := Height <> -1;
end;

procedure TAdvTableViewCategoryAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.ExtendedFill.Assign(Value);
end;

procedure TAdvTableViewCategoryAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.ExtendedFont.Assign(Value);
end;

procedure TAdvTableViewCategoryAppearance.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewCategoryAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.NodesAppearance.ExtendedStroke.Assign(Value);
end;

{ TAdvTableViewMoreOptionAppearance }

procedure TAdvTableViewMoreOptionAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewMoreOptionAppearance then
  begin
    FFont.Assign((Source as TAdvTableViewMoreOptionAppearance).Font);
    FFill.Assign((Source as TAdvTableViewMoreOptionAppearance).Fill);
    FStroke.Assign((Source as TAdvTableViewMoreOptionAppearance).Stroke);
  end;
end;

constructor TAdvTableViewMoreOptionAppearance.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  FFont := TAdvGraphicsFont.Create;
  if Assigned(FTableView) and FTableView.IsDesignTime then
    FFont.Color := gcWhite;
  FFont.OnChanged := FontChanged;
  FFill := TAdvGraphicsFill.Create(gfkSolid, gcSteelblue);
  FFill.OnChanged := FillChanged;
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcSteelblue);
  FStroke.OnChanged := StrokeChanged;
end;

destructor TAdvTableViewMoreOptionAppearance.Destroy;
begin
  FFont.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

procedure TAdvTableViewMoreOptionAppearance.FillChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.Invalidate;
end;

procedure TAdvTableViewMoreOptionAppearance.FontChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.Invalidate;
end;

procedure TAdvTableViewMoreOptionAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvTableViewMoreOptionAppearance.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvTableViewMoreOptionAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvTableViewMoreOptionAppearance.StrokeChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.Invalidate;
end;

{ TAdvTableViewInteraction }

procedure TAdvTableViewInteraction.Assign(Source: TPersistent);
begin
  if (Source is TAdvTableViewInteraction) then
  begin
    FLookup.Assign((Source as TAdvTableViewInteraction).Lookup);
    Filtering.Assign((Source as TAdvTableViewInteraction).Filtering);
    Sorting := (Source as TAdvTableViewInteraction).Sorting;
    FShowDetailMode := (Source as TAdvTableViewInteraction).ShowDetailMode;
    DragDropMode := (Source as TAdvTableViewInteraction).DragDropMode;
    MultiSelect := (Source as TAdvTableViewInteraction).MultiSelect;
    TouchScrolling := (Source as TAdvTableViewInteraction).TouchScrolling;
    ClipboardMode := (Source as TAdvTableViewInteraction).ClipboardMode;
    Reorder := (Source as TAdvTableViewInteraction).Reorder;
    FShowEditButton := (Source as TAdvTableViewInteraction).ShowEditButton;
    FShowFilterButton := (Source as TAdvTableViewInteraction).ShowFilterButton;
    FAccessoryClickDetection := (Source as TAdvTableViewInteraction).AccessoryClickDetection;
    FLongPressToEdit := (Source as TAdvTableViewInteraction).LongPressToEdit;
    FSwipeToShowMoreOptions := (Source as TAdvTableViewInteraction).SwipeToShowMoreOptions;
    FSwipeBounceGesture := (Source as TAdvTableViewInteraction).SwipeBounceGesture;
  end;
end;

constructor TAdvTableViewInteraction.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  FLongPressToEdit := True;
  FSwipeToShowMoreOptions := True;
  FAccessoryClickDetection := ClickAccessories;
  FLookup := TAdvTableViewLookup.Create(Self);
  FFiltering := TAdvTableViewFiltering.Create(Self);
  FShowDetailMode := tvdtAfterSelectItem;
  FShowEditButton := False;
  FShowFilterButton := False;
  FSwipeBounceGesture := True;
  FKeyboardEscapeMode := tvkeStopFilter;
end;

destructor TAdvTableViewInteraction.Destroy;
begin
  FLookup.Free;
  FFiltering.Free;
  inherited;
end;

function TAdvTableViewInteraction.GetAnimationFactor: Single;
begin
  Result := 2.5;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.Interaction.AnimationFactor;
end;

function TAdvTableViewInteraction.GetAutoOpenURL: Boolean;
begin
  Result := True;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.Interaction.AutoOpenURL;
end;

function TAdvTableViewInteraction.GetClipboardMode: TAdvTableViewClipboardMode;
var
  i: Integer;
begin
  Result := tvcmNone;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(FTableView.FTreeView.Interaction.ClipboardMode);
    Result := TAdvTableViewClipboardMode(i);
  end;
end;

function TAdvTableViewInteraction.GetDragDropMode: TAdvTableViewDragDropMode;
var
  i: Integer;
begin
  Result := tvdmNone;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(FTableView.FTreeView.Interaction.DragDropMode);
    Result := TAdvTableViewDragDropMode(i);
  end;
end;

function TAdvTableViewInteraction.GetMultiSelect: Boolean;
begin
  Result := False;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.Interaction.MultiSelect;
end;

function TAdvTableViewInteraction.GetReorder: Boolean;
begin
  Result := False;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.Interaction.Reorder;
end;

function TAdvTableViewInteraction.GetSorting: TAdvTableViewSorting;
begin
  Result := tvcsNone;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) and (FTableView.FTreeView.Columns.Count > 0) then
  begin
    case FTableView.FTreeView.Columns[0].Sorting of
      tcsNone: Result := tvcsNone;
      tcsNormal: Result := tvcsNormal;
      tcsNormalCaseSensitive: Result := tvcsNormalCaseSensitive;
    end;
  end;
end;

function TAdvTableViewInteraction.GetTouchScrolling: Boolean;
begin
  Result := True;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.Interaction.TouchScrolling;
end;

procedure TAdvTableViewInteraction.SetAccessoryClickDetection(const Value: TAdvTableViewItemAccessories);
begin
  FAccessoryClickDetection := Value;
end;

procedure TAdvTableViewInteraction.SetAnimationFactor(const Value: Single);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.Interaction.AnimationFactor := Value;
end;

procedure TAdvTableViewInteraction.SetAutoOpenURL(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.Interaction.AutoOpenURL := Value;
end;

procedure TAdvTableViewInteraction.SetClipboardMode(
  const Value: TAdvTableViewClipboardMode);
var
  i: Integer;
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(Value);
    FTableView.FTreeView.Interaction.ClipboardMode := TAdvTreeViewClipboardMode(i);
  end;
end;

procedure TAdvTableViewInteraction.SetShowDetailMode(const Value: TAdvTableViewShowDetailMode);
begin
  if FShowDetailMode <> Value then
    FShowDetailMode := Value;
end;

procedure TAdvTableViewInteraction.SetShowEditButton(const Value: Boolean);
begin
  if FShowEditButton <> Value then
  begin
    FShowEditButton := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewInteraction.SetShowFilterButton(const Value: Boolean);
begin
  if FShowFilterButton <> Value then
  begin
    FShowFilterButton := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewInteraction.SetDragDropMode(const Value: TAdvTableViewDragDropMode);
var
  i: Integer;
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    i := Integer(Value);
    FTableView.FTreeView.Interaction.DragDropMode := TAdvTreeViewDragDropMode(i);
  end;
end;

procedure TAdvTableViewInteraction.SetFiltering(const Value: TAdvTableViewFiltering);
begin
  FFiltering.Assign(Value);
end;

procedure TAdvTableViewInteraction.SetLookup(const Value: TAdvTableViewLookup);
begin
  FLookup.Assign(Value);
end;

procedure TAdvTableViewInteraction.SetMultiSelect(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.Interaction.MultiSelect := Value;
end;

procedure TAdvTableViewInteraction.SetReorder(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.Interaction.Reorder := Value;
end;

procedure TAdvTableViewInteraction.SetSorting(const Value: TAdvTableViewSorting);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) and (FTableView.FTreeView.Columns.Count > 0) then
  begin
    case Value of
      tvcsNone: FTableView.FTreeView.Columns[0].Sorting := tcsNone;
      tvcsNormal: FTableView.FTreeView.Columns[0].Sorting := tcsNormal;
      tvcsNormalCaseSensitive: FTableView.FTreeView.Columns[0].Sorting := tcsNormalCaseSensitive;
    end;
  end;
end;

procedure TAdvTableViewInteraction.SetTouchScrolling(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.Interaction.TouchScrolling := Value;
end;

{ TAdvTableViewLookup }

procedure TAdvTableViewLookup.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewLookup then
  begin
    CaseSensitive := (Source as TAdvTableViewLookup).CaseSensitive;
    Enabled := (Source as TAdvTableViewLookup).Enabled;
    Incremental := (Source as TAdvTableViewLookup).Incremental;
    AutoSelect := (Source as TAdvTableViewLookup).AutoSelect;
  end;
end;

constructor TAdvTableViewLookup.Create(AOwner: TAdvTableViewInteraction);
begin
  FOwner := AOwner;
end;

function TAdvTableViewLookup.GetAutoSelect: Boolean;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    Result := FOwner.FTableView.FTreeView.Interaction.Lookup.AutoSelect;
end;

function TAdvTableViewLookup.GetCaseSensitive: Boolean;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    Result := FOwner.FTableView.FTreeView.Interaction.Lookup.CaseSensitive;
end;

function TAdvTableViewLookup.GetEnabled: Boolean;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    Result := FOwner.FTableView.FTreeView.Interaction.Lookup.Enabled;
end;

function TAdvTableViewLookup.GetIncremental: Boolean;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    Result := FOwner.FTableView.FTreeView.Interaction.Lookup.Incremental;
end;

procedure TAdvTableViewLookup.SetAutoSelect(const Value: Boolean);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    FOwner.FTableView.FTreeView.Interaction.Lookup.AutoSelect := Value;
end;

procedure TAdvTableViewLookup.SetCaseSensitive(const Value: Boolean);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    FOwner.FTableView.FTreeView.Interaction.Lookup.CaseSensitive := Value;
end;

procedure TAdvTableViewLookup.SetEnabled(const Value: Boolean);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    FOwner.FTableView.FTreeView.Interaction.Lookup.Enabled := Value;
end;

procedure TAdvTableViewLookup.SetIncremental(const Value: Boolean);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) then
    FOwner.FTableView.FTreeView.Interaction.Lookup.Incremental := Value;
end;

{ TAdvTableViewFiltering }

procedure TAdvTableViewFiltering.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewFiltering then
  begin
    Enabled := (Source as TAdvTableViewFiltering).Enabled;
    DropDownWidth := (Source as TAdvTableViewFiltering).DropDownWidth;
    DropDownHeight := (Source as TAdvTableViewFiltering).DropDownHeight;
  end;
end;

constructor TAdvTableViewFiltering.Create(AOwner: TAdvTableViewInteraction);
begin
  FOwner := AOwner;
end;

function TAdvTableViewFiltering.GetDropDownHeight: integer;
begin
  Result := 120;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    Result := FOwner.FTableView.FTreeView.Columns[0].Filtering.DropDownHeight;
end;

function TAdvTableViewFiltering.GetDropDownWidth: integer;
begin
  Result := 100;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    Result := FOwner.FTableView.FTreeView.Columns[0].Filtering.DropDownWidth;
end;

function TAdvTableViewFiltering.GetEnabled: Boolean;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    Result := FOwner.FTableView.FTreeView.Columns[0].Filtering.Enabled;
end;

procedure TAdvTableViewFiltering.SetDropDownHeight(const Value: integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    FOwner.FTableView.FTreeView.Columns[0].Filtering.DropDownHeight := Value;
end;

procedure TAdvTableViewFiltering.SetDropDownWidth(const Value: integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    FOwner.FTableView.FTreeView.Columns[0].Filtering.DropDownWidth := Value;
end;

procedure TAdvTableViewFiltering.SetEnabled(const Value: Boolean);
begin
  if Assigned(FOwner) and Assigned(FOwner.FTableView) and Assigned(FOwner.FTableView.FTreeView) and (FOwner.FTableView.FTreeView.Columns.Count > 0) then
    FOwner.FTableView.FTreeView.Columns[0].Filtering.Enabled := Value;
end;

{ TAdvTableViewHeaderFooter }

procedure TAdvTableViewHeaderFooter.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewHeaderFooter then
  begin
    Visible := (Source as TAdvTableViewHeaderFooter).Visible;
    Text := (Source as TAdvTableViewHeaderFooter).Text;
    Fill.Assign((Source as TAdvTableViewHeaderFooter).Fill);
    Font.Assign((Source as TAdvTableViewHeaderFooter).Font);
    Stroke.Assign((Source as TAdvTableViewHeaderFooter).Stroke);
    HorizontalTextAlign := (Source as TAdvTableViewHeaderFooter).HorizontalTextAlign;
    VerticalTextAlign := (Source as TAdvTableViewHeaderFooter).VerticalTextAlign;
    WordWrapping := (Source as TAdvTableViewHeaderFooter).WordWrapping;
    Trimming := (Source as TAdvTableViewHeaderFooter).Trimming;
    Height := (Source as TAdvTableViewHeaderFooter).Height;
    SortIndicatorColor := (Source as TAdvTableViewHeaderFooter).SortIndicatorColor;
  end;
end;

constructor TAdvTableViewHeaderFooter.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  if Assigned(FTableView) and FTableView.IsDesignTime then
  begin
    if Self is TAdvTableViewHeader then
      FText := 'Header'
    else
      FText := 'Footer';

    Fill.Color := MakeGraphicsColor(249, 249, 249);
    Fill.Kind := gfkSolid;
    Stroke.Color := MakeGraphicsColor(178, 178, 178);
    Stroke.Kind := gskSolid;
  end;
end;

function TAdvTableViewHeaderFooter.GetFill: TAdvGraphicsFill;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      Result := FTableView.FTreeView.ColumnsAppearance.TopFill
    else
      Result := FTableView.FTreeView.ColumnsAppearance.BottomFill
  end;
end;

function TAdvTableViewHeaderFooter.GetFont: TAdvGraphicsFont;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      Result := FTableView.FTreeView.ColumnsAppearance.TopFont
    else
      Result := FTableView.FTreeView.ColumnsAppearance.BottomFont
  end;
end;

function TAdvTableViewHeaderFooter.GetHeight: Single;
begin
  Result := 30;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      Result := FTableView.FTreeView.ColumnsAppearance.TopSize
    else
      Result := FTableView.FTreeView.ColumnsAppearance.BottomSize
  end;
end;

function TAdvTableViewHeaderFooter.GetSortIndicatorColor: TAdvGraphicsColor;
begin
  Result := TAdvTreeViewColorSelection;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    Result := FTableView.FTreeView.ColumnsAppearance.SortIndicatorColor;
end;

function TAdvTableViewHeaderFooter.GetStroke: TAdvGraphicsStroke;
begin
  Result := nil;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      Result := FTableView.FTreeView.ColumnsAppearance.TopStroke
    else
      Result := FTableView.FTreeView.ColumnsAppearance.BottomStroke
  end;
end;

function TAdvTableViewHeaderFooter.GetVisible: Boolean;
begin
  Result := False;
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      Result := tclTop in FTableView.FTreeView.ColumnsAppearance.Layouts
    else
      Result := tclBottom in FTableView.FTreeView.ColumnsAppearance.Layouts;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetFill(const Value: TAdvGraphicsFill);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      FTableView.FTreeView.ColumnsAppearance.TopFill.Assign(Value)
    else
      FTableView.FTreeView.ColumnsAppearance.BottomFill.Assign(Value);
  end;
end;

procedure TAdvTableViewHeaderFooter.SetFont(const Value: TAdvGraphicsFont);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      FTableView.FTreeView.ColumnsAppearance.TopFill.Assign(Value)
    else
      FTableView.FTreeView.ColumnsAppearance.BottomFill.Assign(Value);
  end;
end;

procedure TAdvTableViewHeaderFooter.SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
      FTableView.FTreeView.Invalidate;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetHeight(const Value: Single);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
      FTableView.FTreeView.ColumnsAppearance.TopSize := Value
    else
      FTableView.FTreeView.ColumnsAppearance.BottomSize := Value;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetSortIndicatorColor(
  const Value: TAdvGraphicsColor);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.ColumnsAppearance.SortIndicatorColor := Value;
end;

procedure TAdvTableViewHeaderFooter.SetStroke(const Value: TAdvGraphicsStroke);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
    FTableView.FTreeView.ColumnsAppearance.TopStroke.Assign(Value);
end;

procedure TAdvTableViewHeaderFooter.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
      FTableView.FTreeView.Invalidate;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetTrimming(const Value: TAdvGraphicsTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
      FTableView.FTreeView.Invalidate;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
      FTableView.FTreeView.Invalidate;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetVisible(const Value: Boolean);
begin
  if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
  begin
    if Self is TAdvTableViewHeader then
    begin
      if Value then
        FTableView.FTreeView.ColumnsAppearance.Layouts := FTableView.FTreeView.ColumnsAppearance.Layouts + [tclTop]
      else
        FTableView.FTreeView.ColumnsAppearance.Layouts := FTableView.FTreeView.ColumnsAppearance.Layouts - [tclTop];
    end
    else
    begin
      if Value then
        FTableView.FTreeView.ColumnsAppearance.Layouts := FTableView.FTreeView.ColumnsAppearance.Layouts + [tclBottom]
      else
        FTableView.FTreeView.ColumnsAppearance.Layouts := FTableView.FTreeView.ColumnsAppearance.Layouts - [tclBottom];
    end;

    FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewHeaderFooter.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    if Assigned(FTableView) and Assigned(FTableView.FTreeView) then
      FTableView.FTreeView.Invalidate;
  end;
end;

{ TAdvTableViewFilterData }

procedure TAdvTableViewFilterData.Assign(Source: TPersistent);
var
  ASrcFilterData: TAdvTableViewFilterData;
begin
  ASrcFilterData := Source as TAdvTableViewFilterData;
  if Assigned(ASrcFilterData) then
  begin
    FCondition := ASrcFilterData.Condition;
    FCaseSensitive := ASrcFilterData.CaseSensitive;
    FPrefix := ASrcFilterData.Prefix;
    FSuffix := ASrcFilterData.Suffix;
    FOperation := ASrcFilterData.Operation;
  end;
end;

constructor TAdvTableViewFilterData.Create(ACollection: TCollection);
begin
  inherited;
  FCaseSensitive := True;
end;

{ TAdvTableViewFilter }

function TAdvTableViewFilter.Add: TAdvTableViewFilterData;
begin
  Result := TAdvTableViewFilterData(inherited Add);
  if Count = 1 then
    Result.Operation := tvfoNone
  else
    Result.Operation := tvfoAND;
end;

constructor TAdvTableViewFilter.Create(AOwner: TAdvCustomTableView);
begin
  inherited Create(AOwner, TAdvTableViewFilterData);
  FOwner := AOwner;
end;

function TAdvTableViewFilter.GetItem(Index: Integer): TAdvTableViewFilterData;
begin
  Result := TAdvTableViewFilterData(inherited GetItem(Index));
end;

function TAdvTableViewFilter.Insert(index: Integer): TAdvTableViewFilterData;
begin
  Result := TAdvTableViewFilterData(inherited Insert(Index));
end;

procedure TAdvTableViewFilter.SetItem(Index: Integer; const Value: TAdvTableViewFilterData);
begin
  inherited SetItem(Index, Value);
end;

{ TAdvTableView }

procedure TAdvTableView.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClasses([TAdvTableView, TAdvTableViewItem]);
end;

{ TAdvTreeViewTableView }

procedure TAdvTreeViewTableView.AnimateTimerChanged(Sender: TObject);
var
  d, pos: Double;
  anim: Boolean;
begin
  if FAnimateMoreOptions or FAnimateMoreOptionsClose then
  begin
    pos := FMoreOffset;
    if Interaction.AnimationFactor > 0 then
    begin
      d := Abs(FMoreOffsetTo - pos) / Interaction.AnimationFactor;
      anim := AnimateDouble(pos, FMoreOffsetTo, d, 0.01)
    end
    else
      anim := False;

    if anim then
      FMoreOffset := pos
    else
    begin
      FMoreOffset := FMoreOffsetTo;
      FAnimTimer.Enabled := False;
      if FAnimateMoreOptionsClose then
      begin
        FMoreOffsetItem := nil;
        FAnimateMoreOptionsClose := False;
      end;
    end;

    if Assigned(FTableView) then
      FTableView.UpdateTableView(True);
  end
  else
  begin
    if FDownTimer then
    begin
      Inc(FDownCount);
      if FDownCount = DOWNCOUNT + 50 then
      begin
        FDownAction := True;
        FAnimTimer.Enabled := False;
        if Assigned(FTableView) then
          FTableView.ToggleEditMode;
      end;
    end
    else
    begin
      FAnimShiftAngle := FAnimShiftAngle + 5;
      if FAnimShiftAngle = 360 then
        FAnimShiftAngle := 0;

      if not FAnimMarqueeSwitch then
        FAnimMarqueeSweepAngle := FAnimMarqueeSweepAngle + 10
      else
      begin
        FAnimMarqueeStartAngle := FAnimMarqueeStartAngle + 10;
        FAnimMarqueeSweepAngle := FAnimMarqueeSweepAngle - 10;
      end;

      if (FAnimMarqueeSweepAngle = 360) and (FAnimMarqueeStartAngle = 0) then
        FAnimMarqueeSwitch := True
      else if ((FAnimMarqueeStartAngle = 360) and (FAnimMarqueeSweepAngle = 0)) then
      begin
        FAnimMarqueeSwitch := False;
        FAnimMarqueeStartAngle := 0;
      end;

      Invalidate;
    end;
  end;
end;

function TAdvTreeViewTableView.CanApplyVerticalOffset: Boolean;
begin
  Result := False;
  if Assigned(FTableView) and Assigned(FTableView.Interaction) then
    Result := FTableView.Interaction.SwipeBounceGesture;
end;

constructor TAdvTreeViewTableView.Create(AOwner: TComponent);
begin
  inherited;
  FAnimMarqueeStartAngle := 0;
  FManualProgressSweepAngle := 0;
  FManualProgressStartAngle := 0;
  FAnimMarqueeSweepAngle := 0;
  FMoreOffset := 0;
  FMoreOffsetTo := 0;
  FAnimMarqueeSwitch := False;
  FFloatingNodes := TAdvTreeViewTableViewFloatingNodes.Create;
  FAnimTimer := TTimer.Create(Self);
  FAnimTimer.Enabled := False;
  FAnimTimer.Interval := 1;
  FAnimTimer.OnTimer := AnimateTimerChanged;
end;

destructor TAdvTreeViewTableView.Destroy;
begin
  FAnimTimer.Free;
  FFloatingNodes.Free;
  inherited;
end;

procedure TAdvTreeViewTableView.DoFooterAnchorClick(AAnchor: String);
begin
  if Assigned(OnFooterAnchorClick) then
    OnFooterAnchorClick(Self, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvTreeViewTableView.DoHeaderAnchorClick(AAnchor: String);
begin
  if Assigned(OnHeaderAnchorClick) then
    OnHeaderAnchorClick(Self, AAnchor)
  else if Interaction.AutoOpenURL then
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvTreeViewTableView.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  inherited;
  DrawLookupBar(AGraphics, ARect);
  DrawReload(AGraphics, ARect);
end;

procedure TAdvTreeViewTableView.DrawLookupBar(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  I: Integer;
  ldp: TAdvTableViewLookupBarDisplayItem;
  cr: TRectF;
begin
  if Assigned(FTableView) and (FTableView.LookupBar.Visible and (FTableView.CategoryType <> tvctNone)) then
  begin
    cr := GetLookupBarRect;
    if FMouseDownOnLookupBar then
    begin
      AGraphics.Fill.Assign(FTableView.LookupBar.DownFill);
      AGraphics.Stroke.Assign(FTableView.LookupBar.DownStroke);
    end
    else
    begin
      AGraphics.Fill.Assign(FTableView.LookupBar.Fill);
      AGraphics.Stroke.Assign(FTableView.LookupBar.Stroke);
    end;

    AGraphics.DrawRectangle(cr);

    for I := 0 to FTableView.FLookupBarDisplayList.Count - 1 do
    begin
      ldp := FTableView.FLookupBarDisplayList[I];
      if ldp.Active then
        AGraphics.Font.Assign(FTableView.LookupBar.Font)
      else
        AGraphics.Font.Assign(FTableView.LookupBar.InActiveFont);
      AGraphics.DrawText(ldp.Rect, ldp.Text, False, gtaCenter, gtaCenter);
    end;
  end;
end;

procedure TAdvTreeViewTableView.DrawReload(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  r: TRectF;
  sz: Single;
begin
  if Assigned(FTableView) and FTableView.Reload.Enabled and FReloadActive then
  begin
    r := GetNodesRect;
    sz := FTableView.Reload.Size;
    r := RectF(r.Left + ((r.Right - r.Left) - sz) / 2, r.Top + (FTableView.Reload.Offset - sz) / 2, r.Left + ((r.Right - r.Left) - sz) / 2 + sz, r.Top + (FTableView.Reload.Offset - sz) / 2 + sz);
    AGraphics.Stroke.Assign(FTableView.Reload.Stroke);
    case FTableView.Reload.ProgressMode of
      tvrpmMarquee: AGraphics.DrawArc(CenterPointEx(r), PointF((r.Right - r.Left) / 2, (r.Bottom - r.Top) / 2), FAnimMarqueeStartAngle, FAnimMarqueeSweepAngle);
      tvrpmMarqueeAlternate: AGraphics.DrawArc(CenterPointEx(r), PointF((r.Right - r.Left) / 2, (r.Bottom - r.Top) / 2), FAnimShiftAngle + FAnimMarqueeStartAngle, FAnimMarqueeSweepAngle);
      tvrpmManual: AGraphics.DrawArc(CenterPointEx(r), PointF((r.Right - r.Left) / 2, (r.Bottom - r.Top) / 2), FManualProgressStartAngle, FManualProgressSweepAngle);
    end;
  end;
end;

procedure TAdvTreeViewTableView.CustomizeNodeCache(
  AGraphics: TAdvGraphics; AStartY: Single);
var
  n: TAdvTreeViewVirtualNode;
  s: Single;
  dp: Integer;
begin
  inherited;
  if Assigned(FTableView) and (FTableView.FUpdateCount = 0) and (FTableView.CategoryType <> tvctNone) then
  begin
    dp := FTableView.GetDisplayCategoryIndex(StartRow);
    if (dp > -1) then
    begin
      FFloatingNodes.Clear;
      n := TAdvTreeViewVirtualNode.Create(Self);
      n.CopyFrom(GetNodeForRow(dp));
      s := 0;
      ConfigureNode(AGraphics, 0, n, s);
      FFloatingNodes.Add(n);
    end;
  end;
end;

procedure TAdvTreeViewTableView.CustomizeScrollPosition(
  ANode: TAdvTreeViewVirtualNode; var APosition, ATopPosition: Double);
begin
  inherited;
  if (FFloatingNodes.Count > 0) and not ANode.Extended {and (CompareValue(APosition, GetVerticalScrollPosition) = LessThanValue)} then
  begin
    APosition := APosition - FFloatingNodes[0].Height;
    ATopPosition := ATopPosition - FFloatingNodes[0].Height;
  end;
end;

function TAdvTreeViewTableView.GetCategoryAtXY(X, Y: Single): Integer;
var
  i, s: Integer;
  r: TRectF;
  str: String;
  ypos: Single;
  g: TAdvGraphics;
  cr: TRectF;
begin
  Result := -1;

  if not Assigned(FTableView) then
    Exit;

  g := TAdvGraphics.CreateBitmapCanvas;
  g.BeginScene;
  try
    g.Font.Assign(FTableView.LookupBar.Font);
    s := FTableView.Categories.Count - 1;
    cr := GetLookupBarRect;
    ypos := cr.Top + 3;
    for I := 0 to s do
    begin
      str := FTableView.Categories[I].LookupText;
      r.Left := cr.Left;
      r.Right := cr.Right;
      r.Top := ypos;
      r.Bottom := r.Top + g.CalculateTextHeight(str);
      ypos := ypos + (r.Bottom - r.Top) + 3;

      if PtInRectEx(r, PointF(X, Y)) then
      begin
        Result := I;
        Break;
      end;
    end;
  finally
    g.EndScene;
    g.Free;
  end;
end;

function TAdvTreeViewTableView.GetColumnWidth(AColumn: Integer): Double;
begin
  Result := inherited GetColumnWidth(AColumn);
  if Assigned(FTableView) and (FTableView.LookupBar.Visible and (FTableView.CategoryType <> tvctNone)) then
    Result := Result - FTableView.LookupBar.Width;
end;

function TAdvTreeViewTableView.GetContentClipRect: TRectF;
begin
  Result := inherited GetContentRect;
  Result.Left := Result.Left + 1;
  Result.Right := Result.Right - 1;
end;

function TAdvTreeViewTableView.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  if Assigned(FTableView) and FTableView.FFilterActive then
    Result.Top := Result.Top + FTableView.FEdit.Height;
end;

function TAdvTreeViewTableView.GetNodesRect: TRectF;
var
  d: Double;
begin
  Result := inherited GetNodesRect;
  if Assigned(FTableView) then
  begin
    d := (Result.Right - Result.Left) - FTableView.FDetailControlOffset;
    if (FTableView.LookupBar.Visible and (FTableView.CategoryType <> tvctNone)) then
    begin
      case FTableView.LookupBar.Position of
        tvlbpLeft:
        begin
          Result.Left := Result.Left + FTableView.LookupBar.Width - 1;
          Result.Right := Result.Right - FTableView.LookupBar.Width - 1;
        end;
        tvlbpRight: Result.Right := Result.Right - FTableView.LookupBar.Width;
      end;
    end;

    Result.Left := Result.Left - d;
    Result.Right := Result.Right - d;
  end;
end;

function TAdvTreeViewTableView.GetReloadOffset: Single;
begin
  Result := inherited GetReloadOffset;
  if Assigned(FTableView) then
  begin
    if FTableView.Reload.Enabled then
      Result := Result + FTableView.Reload.Offset;
  end;
end;

function TAdvTreeViewTableView.GetTotalColumnNodeWidth: Double;
begin
  Result := inherited GetTotalColumnNodeWidth;
  if Assigned(FTableView) and (FTableView.LookupBar.Visible and (FTableView.CategoryType <> tvctNone)) then
    Result := Result - FTableView.LookupBar.Width;
end;

function TAdvTreeViewTableView.GetLookupBarRect: TRectF;
var
  sz: Single;
  d: Single;
begin
  Result := EmptyRect;
  if Assigned(FTableView) and (FTableView.LookupBar.Visible and (FTableView.CategoryType <> tvctNone)) then
  begin
    sz := FTableView.LookupBar.Width;
    Result := GetContentRect;
    d := (Result.Right - Result.Left) - FTableView.FDetailControlOffset;
    case FTableView.LookupBar.Position of
      tvlbpLeft:
      begin
        Result.Right := Result.Left + sz;
        Result.Left := Result.Left + 1;
        Result.Top := Result.Top + 1;
        Result.Bottom := Result.Bottom - 1;
      end;
      tvlbpRight:
      begin
        Result.Left := Result.Right - sz;
        Result.Top := Result.Top + 1;
        Result.Bottom := Result.Bottom - 1;
        Result.Right := Result.Right - 1;
      end;
    end;

    Result.Left := Result.Left - d;
    Result.Right := Result.Right - d;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvTableViewEdit.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvTableViewEdit.KeyDown(var Key: Word; Shift: TShiftState);
{$ENDIF}
begin
  inherited;
  if not Assigned(FTableView) then
    Exit;

  if (Key in [KEY_ESCAPE]) and FTableView.FFilterActive then
  begin
    case FTableView.Interaction.KeyboardEscapeMode of
      tvkeNone: ;
      tvkeClearEdit:
      begin
        FTableView.FFilterActive := False;
        FTableView.StartFiltering;
      end;
      tvkeStopFilter: FTableView.StopFiltering;
    end;
  end;
end;

procedure TAdvTreeViewTableView.HandleKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if BlockUserInput or not Assigned(FTableView) then
    Exit;

  if (Key in [KEY_ESCAPE]) and FTableView.FFilterActive then
  begin
    case FTableView.Interaction.KeyboardEscapeMode of
      tvkeNone: ;
      tvkeClearEdit:
      begin
        FTableView.FFilterActive := False;
        FTableView.StartFiltering;
      end;
      tvkeStopFilter: FTableView.StopFiltering;
    end;
  end;
end;

procedure TAdvTreeViewTableView.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  it: TAdvTableViewItem;
begin
  if Assigned(FTableView) then
  begin
    FDownMoreOption := FTableView.XYToMoreOption(X, Y);
    if Assigned(FDownMoreOption) then
    begin
      BlockUserInput := True;
      inherited;
      BlockUserInput := False;
      Invalidate;
      Exit;
    end;
  end;

  if FAnimateMoreOptions then
  begin
    FAnimateMoreOptions := False;
    FAnimateMoreOptionsClose := True;
    FMoreOffsetTo := 0;
    FAnimTimer.Enabled := True;
    inherited;
    BlockMouseClick := False;
    Exit;
  end;

  if BlockUserInput then
  begin
    inherited;
    Exit;
  end;

  it := nil;
  FMouseDownOnLookupBar := PtInRectEx(GetLookupBarRect, PointF(X, Y));
  if not FMouseDownOnLookupBar and Assigned(FTableView) and not IsAnimating then
  begin
    it := FTableView.XYToAccessoryItem(X, Y);
    FMouseDownOnAccessory := Assigned(it);
  end;

  BlockUserInput := FMouseDownOnLookupBar or FMouseDownOnAccessory;
  inherited;

  if FMouseDownOnLookupBar then
  begin
    CaptureEx;
    StopAnimationTimer;
    ProcessLookup(X, Y);
    Invalidate;
  end;

  if FMouseDownOnAccessory then
  begin
    CaptureEx;
    StopAnimationTimer;
    if Assigned(FTableView) then
      FTableView.FActiveAccessoryItem := it;
    Invalidate;
  end;

  FDownX := X;
  FDownY := Y;

  if not FMouseDownOnAccessory and not FMouseDownOnLookupBar and (Assigned(FTableView) and FTableView.Interaction.LongPressToEdit) then
  begin
    FDownAction := False;
    FDownTimer := True;
    FDownCount := 0;
    FAnimTimer.Enabled := True;
  end;
end;

procedure TAdvTreeViewTableView.HandleMouseMove(Shift: TShiftState; X,
  Y: Single);
var
  a: string;
  crs: smallInt;
begin
  inherited;
  crs := Cursor;
  if FDownTimer and not (FAnimateMoreOptions or FAnimateMoreOptionsClose) then
  begin
    if (Abs(X - FDownX) > 5) or (Abs(Y - FDownY) > 5) then
    begin
      FDownAction := False;
      FDownCount := 0;
      FDownTimer := False;
      FAnimTimer.Enabled := False;
    end;
  end;

  if Assigned(FTableView) and (FTableView.MoreOptions.Count > 0) and FTableView.Interaction.SwipeToShowMoreOptions and not Assigned(FMoreOffsetItem) and IsMouseDown and
    not BlockUserInput and not BlockMouseClick and (Abs(X - FDownX) > SWIPEOFFSET) and (Abs(Y - FDownY) <= 5) then
  begin
    FMoreOffsetItem := FTableView.XYToItem(X, Y);
    if Assigned(FMoreOffsetItem) then
    begin
      if FMoreOffsetItem.EnableMoreOptions then
      begin
        FMoreOffsetTo := FTableView.GetMoreOptionsSize;
        BlockMouseClick := True;
        FAnimateMoreOptions := True;
        FAnimTimer.Enabled := True;
      end;
    end;
  end;

  a := XYToHeaderAnchor(X,Y);
  if a <> '' then
    crs := crHandPoint
  else
  begin
    a := XYToFooterAnchor(X,Y);
    if a <> '' then
      crs := crHandPoint;
  end;

  if crs <> Cursor then
    Cursor := crs;

  if FMouseDownOnLookupBar then
    ProcessLookup(X, Y);
end;

procedure TAdvTreeViewTableView.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  rp: Boolean;
  mo: TAdvTableViewMoreOption;
  a: string;
begin
  if Assigned(FDownMoreOption) then
  begin
    mo := FDownMoreOption;
    FDownMoreOption := nil;
    Invalidate;
    if FAnimateMoreOptions then
    begin
      FAnimateMoreOptions := False;
      FAnimateMoreOptionsClose := True;
      FMoreOffsetTo := 0;
      FAnimTimer.Enabled := True;
      inherited;
      BlockMouseClick := False;
      if Assigned(FTableView) and Assigned(FMoreOffsetItem) and Assigned(mo) then
        FTableView.DoItemMoreOptionClick(FMoreOffsetItem, mo);
      Exit;
    end;
  end;

  if FDownTimer and not (FAnimateMoreOptions or FAnimateMoreOptionsClose) then
  begin
    FDownCount := 0;
    FDownTimer := False;
    FAnimTimer.Enabled := False;
    if FDownAction then
    begin
      FDownAction := False;
      BlockUserInput := True;
      inherited;
      IsMouseDown := False;
      BlockUserInput := False;
      Exit;
    end;
  end;

  inherited;

  rp := False;
  if FMouseDownOnLookupBar then
  begin
    ReleaseCaptureEx;
    ProcessLookup(X, Y, False);
    rp := True;
  end;

  if FMouseDownOnAccessory then
  begin
    ReleaseCaptureEx;
    if Assigned(FTableView) then
    begin
      if Assigned(FTableView.FActiveAccessoryItem) then
        FTableView.DoItemAccessoryClick(FTableView.FActiveAccessoryItem);
      FTableView.FActiveAccessoryItem := nil;
    end;
    rp := True;
  end;

  if rp then
  begin
    FMouseDownOnAccessory := False;
    FMouseDownOnLookupBar := False;
    BlockUserInput := False;
  end;

  a := XYToFooterAnchor(X, Y);
  if a <> '' then
     DoFooterAnchorClick(a)
  else
  begin
    a := XYToHeaderAnchor(X, Y);
    if a <> '' then
       DoHeaderAnchorClick(a)
  end;

  Invalidate;
end;

function TAdvTreeViewTableView.NeedsReload(AVerticalOffset: Single): Boolean;
begin
  Result := inherited NeedsReload(AVerticalOffset);
  if Assigned(FTableView) and FTableView.Reload.Enabled and not Assigned(FTableView.FActiveDetailControl) and not FTableView.FEditMode and not FTableView.FFilterActive then
    Result := AVerticalOffset >= FTableView.Reload.Offset;
end;

procedure TAdvTreeViewTableView.OffsetNodeRects(
  ANode: TAdvTreeViewVirtualNode; AX, AY: Single; var ARect: TRectF);
var
  i: Integer;
begin
  if not Assigned(FFloatingNodes) then
  begin
    inherited;
    Exit;
  end;

  i := FFloatingNodes.IndexOf(ANode);
  if i > -1 then
    inherited OffsetNodeRects(ANode, AX, Max(0, AY), ARect)
  else
  begin
    if (FMoreOffsetItem = FTableView.GetItemForNode(ANode)) then
      inherited OffsetNodeRects(ANode, AX - FMoreOffset, AY, ARect)
    else
      inherited;
  end;
end;

procedure TAdvTreeViewTableView.ProcessLookup(X, Y: Single;
  DoLookup: Boolean);
var
  l: Integer;
  ch: String;
  catid: Integer;
  I: Integer;
begin
  if Assigned(FTableView) then
  begin
    case FTableView.CategoryType of
      tvctCustom:
      begin
        l := GetCategoryAtXY(X, Y);
        if (l > -1) and (l < FTableView.Categories.Count) then
        begin
          catid := FTableView.Categories[l].Id;
          if FTableView.FCustomChar.IsIDActive(catID) then
          begin
            if DoLookup then
              FTableView.LookupCustomCategory(catid)
            else if Assigned(FTableView.OnCategoryClick) then
              FTableView.OnCategoryClick(Self, '', catid);
          end;
        end;
      end;
      tvctAlphaBetic, tvctAlphaNumericFirst, tvctAlphaNumericLast:
      begin
        l := 0;
        for I := 0 to FTableView.FLookupBarDisplayList.Count - 1 do
        begin
          if PtInRectEx(FTableView.FLookupBarDisplayList[I].Rect, PointF(X, Y)) then
          begin
            l := I + 1;
            Break;
          end;
        end;

        if (l >= 1) and (l <= FTableView.GetCharacterCount) then
        begin
          if Assigned(FTableView) then
          begin
            ch := FTableView.GetCharacter(l);
            if (ch <> '') then
            begin
              if DoLookup then
                FTableView.LookupCategory(ch)
              else if Assigned(FTableView.OnCategoryClick) then
                FTableView.OnCategoryClick(Self, ch, -1);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvTreeViewTableView.ScrollLimitation: Boolean;
begin
  Result := True;
end;

procedure TAdvTreeViewTableView.StartReload;
begin
  inherited;
  FReloadActive := True;
  BlockUserInput := True;
  if Assigned(FTableView) then
  begin
    FManualProgressSweepAngle := 0;
    FManualProgressStartAngle := 0;
    FAnimTimer.Enabled := (FTableView.Reload.ProgressMode <> tvrpmManual);
    FTableView.DoStartReload;
  end;
end;

procedure TAdvTreeViewTableView.UpdateCheckState(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AChecked: Boolean);
var
  dp: TAdvTableViewDisplayItem;
begin
  inherited;
  if not Assigned(FTableView) then
    Exit;

  if (ANode.Row >= 0) and (ANode.Row <= FTableView.FDisplayList.Count - 1) then
  begin
    dp := FTableView.FDisplayList[ANode.Row];
    case dp.Kind of
      tvikItem:
      begin
        if Assigned(dp.Item) then
          dp.Item.FChecked := AChecked;

        FTableView.SelectAllCheckedItems;
      end;
      tvikCategory: ;
    end;
  end;
end;

function TAdvTreeViewTableView.XYToHeaderAnchor(X, Y: Single): string;
var
  r : TRectF;
  g: TAdvGraphics;
begin
  Result := '';

  r := RectF(0,0, Width, FTableView.Header.Height);

  g := TAdvGraphics.CreateBitmapCanvas;
  g.BeginScene;
  g.PictureContainer := PictureContainer;
  g.Font.Assign(FTableView.Header.Font);

  try
    Result := g.DrawText(r, FTableView.Header.Text, FTableView.Header.WordWrapping, FTableView.Header.HorizontalTextAlign , FTableView.Header.VerticalTextAlign, FTableView.Header.Trimming, 0, -1, -1, True, True, X, Y);
  finally
    g.EndScene;
    g.Free;
  end;
end;

function TAdvTreeViewTableView.XYToFooterAnchor(X, Y: Single): string;
var
  r : TRectF;
  g: TAdvGraphics;
begin
  Result := '';

  r := RectF(0, Height - FTableView.Footer.Height, Width, Height);

  g := TAdvGraphics.CreateBitmapCanvas;
  g.BeginScene;
  g.PictureContainer := PictureContainer;
  g.Font.Assign(FTableView.Footer.Font);

  try
    Result := g.DrawText(r, FTableView.Footer.Text, FTableView.Footer.WordWrapping, FTableView.Footer.HorizontalTextAlign , FTableView.Footer.VerticalTextAlign, FTableView.Footer.Trimming, 0, -1, -1, True, True, X, Y);
  finally
    g.EndScene;
    g.Free;
  end;
end;

{ TAdvTableViewCategory }

procedure TAdvTableViewCategory.Assign(Source: TPersistent);
begin
  if (Source is TAdvTableViewCategory) then
  begin
    FText := (Source as TAdvTableViewCategory).Text;
    FLookupText := (Source as TAdvTableViewCategory).LookupText;
    FId := (Source as TAdvTableViewCategory).Id;
    Changed;
  end;
end;

procedure TAdvTableViewCategory.Changed;
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

constructor TAdvTableViewCategory.Create(Collection: TCollection);
begin
  inherited;
  FTableView := (Collection as TAdvTableViewCategories).FTableView;
  FID := (Collection as TAdvTableViewCategories).Count - 1;
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

destructor TAdvTableViewCategory.Destroy;
begin
  inherited;
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

function TAdvTableViewCategory.GetStrippedHTMLText: String;
begin
  Result := TAdvUtils.HTMLStrip(Text);
end;

procedure TAdvTableViewCategory.SetId(const Value: integer);
begin
  if FId <> Value then
  begin
    FId := Value;
    Changed;
  end;
end;

procedure TAdvTableViewCategory.SetLookupText(const Value: String);
begin
  if FLookupText <> value then
  begin
    FLookupText := Value;
    Changed;
  end;
end;

procedure TAdvTableViewCategory.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TAdvTableViewCategories }

function TAdvTableViewCategories.Add: TAdvTableViewCategory;
begin
  Result := TAdvTableViewCategory(inherited Add);
end;

procedure TAdvTableViewCategories.Clear;
var
  it: TAdvTableViewCategory;
begin
  if Count > 0 then
  begin
    {$IFNDEF LCLWEBLIB}
    while Count > 0 do
    begin
      it := Items[Count - 1];
      it.DisposeOf;
    end;
    {$ENDIF}
    {$IFDEF LCLWEBLIB}
    while Count > 0 do
    begin
      it := Items[Count - 1];
      it.Free;
    end;
    {$ENDIF}
  end;
end;

function TAdvTableViewCategories.Compare(AItem1,
  AItem2: TAdvTableViewCategory; ACaseSensitive: Boolean;
  ASortingMode: TAdvTableViewCategoriesSortMode): Integer;
var
  l: TAdvCustomTableView;
begin
  Result := 0;

  l := FTableView;
  if not Assigned(l) then
    Exit;

  if not ACaseSensitive then
    Result := AnsiCompareStr(UpperCase(AItem1.StrippedHTMLText), UpperCase(AItem2.StrippedHTMLText))
  else
    Result := AnsiCompareStr(AItem1.StrippedHTMLText, AItem2.StrippedHTMLText);

  case ASortingMode of
    tvcsmDescending: Result := Result * -1;
  end;

  l.DoCategoryCompare(AItem1, AItem2, Result);
end;

constructor TAdvTableViewCategories.Create(ATableView: TAdvCustomTableView);
begin
  inherited Create(ATableView, TAdvTableViewCategory);
  FTableView := ATableView;
end;

procedure TAdvTableViewCategories.Delete(Index: Integer);
var
  it: TAdvTableViewCategory;
begin
  it := Items[Index];
  it.Free;
end;

function TAdvTableViewCategories.GetItem(
  Index: Integer): TAdvTableViewCategory;
begin
  Result := TAdvTableViewCategory(inherited Items[Index]);
end;

function TAdvTableViewCategories.Insert(
  Index: Integer): TAdvTableViewCategory;
begin
  Result := TAdvTableViewCategory(inherited Insert(Index));
end;

function TAdvTableViewCategories.ItemById(
  id: integer): TAdvTableViewCategory;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID = id then
    begin
      Result := Items[I];
      break;
    end;
  end;
end;

function TAdvTableViewCategories.ItemIndexByID(id: integer): integer;
var
  ci: TAdvTableViewCategory;
begin
  ci := ItemByID(id);
  if Assigned(ci) then
    result := ci.Index
  else
    result := -1;
end;

procedure TAdvTableViewCategories.QuickSort(L: Integer; R: Integer; ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewCategoriesSortMode = tvcsmAscending);
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

procedure TAdvTableViewCategories.SetItem(Index: Integer;
  const Value: TAdvTableViewCategory);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvTableViewCategories.Sort(ACaseSensitive: Boolean = True; ASortingMode: TAdvTableViewCategoriesSortMode = tvcsmAscending);
var
  l: TAdvCustomTableView;
begin
  l := FTableView;
  if Assigned(l) then
  begin
    l.BeginUpdate;
    BeginUpdate;
    if Count > 1 then
      QuickSort(0, Pred(Count), ACaseSensitive, ASortingMode);
    EndUpdate;
    l.EndUpdate;
  end;
end;

{ TAdvTableViewMoreOption }

procedure TAdvTableViewMoreOption.Assign(Source: TPersistent);
begin
  if (Source is TAdvTableViewMoreOption) then
  begin
    FText := (Source as TAdvTableViewMoreOption).Text;
    FWidth := (Source as TAdvTableViewMoreOption).Width;
    FColor := (Source as TAdvTableViewMoreOption).Color;
    FBorderColor := (Source as TAdvTableViewMoreOption).BorderColor;
    FFontColor := (Source as TAdvTableViewMoreOption).FontColor;
  end;
end;

procedure TAdvTableViewMoreOption.Changed;
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

constructor TAdvTableViewMoreOption.Create(Collection: TCollection);
begin
  inherited;
  FTableView := (Collection as TAdvTableViewMoreOptions).FTableView;
  FWidth := 60;
  FColor := gcNull;
  FBorderColor := gcNull;
  FFontColor := gcNull;
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

destructor TAdvTableViewMoreOption.Destroy;
begin
  inherited;
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

function TAdvTableViewMoreOption.GetStrippedHTMLText: String;
begin
  Result := TAdvUtils.HTMLStrip(Text);
end;

function TAdvTableViewMoreOption.IsWidthStored: Boolean;
begin
  Result := Width <> 60;
end;

procedure TAdvTableViewMoreOption.SetBorderColor(
  const Value: TAdvGraphicsColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvTableViewMoreOption.SetColor(
  const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvTableViewMoreOption.SetFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed;
  end;
end;

procedure TAdvTableViewMoreOption.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvTableViewMoreOption.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvTableViewMoreOptions }

function TAdvTableViewMoreOptions.Add: TAdvTableViewMoreOption;
begin
  Result := TAdvTableViewMoreOption(inherited Add);
end;

procedure TAdvTableViewMoreOptions.Clear;
var
  it: TAdvTableViewMoreOption;
begin
  if Count > 0 then
  begin
    {$IFNDEF LCLWEBLIB}
    while Count > 0 do
    begin
      it := Items[Count - 1];
      it.DisposeOf;
    end;
    {$ENDIF}
    {$IFDEF LCLWEBLIB}
    while Count > 0 do
    begin
      it := Items[Count - 1];
      it.Free;
    end;
    {$ENDIF}
  end;
end;

constructor TAdvTableViewMoreOptions.Create(ATableView: TAdvCustomTableView);
begin
  inherited Create(ATableView, TAdvTableViewMoreOption);
  FTableView := ATableView;
end;

procedure TAdvTableViewMoreOptions.Delete(Index: Integer);
var
  it: TAdvTableViewMoreOption;
begin
  it := Items[Index];
  it.Free;
end;

function TAdvTableViewMoreOptions.GetItem(
  Index: Integer): TAdvTableViewMoreOption;
begin
  Result := TAdvTableViewMoreOption(inherited Items[Index]);
end;

function TAdvTableViewMoreOptions.Insert(
  Index: Integer): TAdvTableViewMoreOption;
begin
  Result := TAdvTableViewMoreOption(inherited Insert(Index));
end;

function TAdvTableViewMoreOptions.ItemById(
  id: integer): TAdvTableViewMoreOption;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID = id then
    begin
      Result := Items[I];
      break;
    end;
  end;
end;

function TAdvTableViewMoreOptions.ItemIndexByID(id: integer): integer;
var
  ci: TAdvTableViewMoreOption;
begin
  ci := ItemByID(id);
  if Assigned(ci) then
    Result := ci.Index
  else
    Result := -1;
end;

procedure TAdvTableViewMoreOptions.SetItem(Index: Integer;
  const Value: TAdvTableViewMoreOption);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF LCLLIB}
class operator TAdvTableViewDisplayItem.=(z1, z2: TAdvTableViewDisplayItem)b: boolean;
begin
  Result := z1 = z2;
end;

class operator TAdvTableViewLookupBarDisplayItem.=(z1, z2: TAdvTableViewLookupBarDisplayItem)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

{ TAdvTableViewLookupBar }

procedure TAdvTableViewLookupBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvTableViewLookupBar) then
  begin
    FVisible := (Source as TAdvTableViewLookupBar).Visible;
    FFont.Assign((Source as TAdvTableViewLookupBar).Font);
    FInActiveFont.Assign((Source as TAdvTableViewLookupBar).InActiveFont);
    FStroke.Assign((Source as TAdvTableViewLookupBar).Stroke);
    FFill.Assign((Source as TAdvTableViewLookupBar).Fill);
    FAutoLookup := (Source as TAdvTableViewLookupBar).AutoLookup;
    FWidth := (Source as TAdvTableViewLookupBar).Width;
    FPosition := (Source as TAdvTableViewLookupBar).Position;
    FDownFill.Assign((Source as TAdvTableViewLookupBar).DownFill);
    FDownStroke.Assign((Source as TAdvTableViewLookupBar).DownStroke);
  end;
end;

constructor TAdvTableViewLookupBar.Create(
  ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  FWidth := 25;
  FVisible := True;
  FPosition := tvlbpRight;
  FAutoLookup := True;
  FFont := TAdvGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  FInActiveFont := TAdvGraphicsFont.Create;
  if Assigned(FTableView) and FTableView.IsDesignTime then
    FInActiveFont.Color := gcSilver;
  FInActiveFont.OnChanged := FontChanged;
  FFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FFill.OnChanged := FillChanged;
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcWhite);
  FStroke.OnChanged := StrokeChanged;

  FDownFill := TAdvGraphicsFill.Create(gfkSolid, MakeGraphicsColor(249, 249, 249));
  FDownFill.OnChanged := FillChanged;
  FDownStroke := TAdvGraphicsStroke.Create(gskSolid, MakeGraphicsColor(249, 249, 249));
  FDownStroke.OnChanged := StrokeChanged;
end;

destructor TAdvTableViewLookupBar.Destroy;
begin
  FFont.Free;
  FInActiveFont.Free;
  FFill.Free;
  FStroke.Free;
  FDownFill.Free;
  FDownStroke.Free;
  inherited;
end;

procedure TAdvTableViewLookupBar.FillChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

procedure TAdvTableViewLookupBar.FontChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

function TAdvTableViewLookupBar.IsWidthStored: Boolean;
begin
  Result := Width <> 25;
end;

procedure TAdvTableViewLookupBar.SetAutoLookup(const Value: Boolean);
begin
  FAutoLookup := Value;
end;

procedure TAdvTableViewLookupBar.SetDownFill(
  const Value: TAdvGraphicsFill);
begin
  FDownFill.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FDownStroke.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetInActiveFont(
  const Value: TAdvGraphicsFont);
begin
  FInActiveFont.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetPosition(
  const Value: TAdvTableViewLookupBarPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewLookupBar.SetStroke(
  const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvTableViewLookupBar.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewLookupBar.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if Assigned(FTableView) then
      FTableView.UpdateTableView;
  end;
end;

procedure TAdvTableViewLookupBar.StrokeChanged(Sender: TObject);
begin
  if Assigned(FTableView) then
    FTableView.UpdateTableView;
end;

{ TAdvTableViewReload }

procedure TAdvTableViewReload.Assign(Source: TPersistent);
begin
  if Source is TAdvTableViewReload then
  begin
    FOffset := (Source as TAdvTableViewReload).Offset;
    FSize := (Source as TAdvTableViewReload).Size;
    FEnabled := (Source as TAdvTableViewReload).Enabled;
    FStroke.Assign((Source as TAdvTableViewReload).Stroke);
    FProgressMode := (Source as TAdvTableViewReload).ProgressMode;
  end;
end;

constructor TAdvTableViewReload.Create(ATableView: TAdvCustomTableView);
begin
  FTableView := ATableView;
  FOffset := 40;
  FSize := 20;
  FEnabled := False;
  FProgressMode := tvrpmMarquee;
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcDarkgray);
  if Assigned(FTableView) and FTableView.IsDesignTime then
    FStroke.Width := 2;
end;

destructor TAdvTableViewReload.Destroy;
begin
  FStroke.Free;
  inherited;
end;

function TAdvTableViewReload.IsOffsetStored: Boolean;
begin
  Result := Offset <> 40;
end;

function TAdvTableViewReload.IsSizeStored: Boolean;
begin
  Result := Size <> 20;
end;

procedure TAdvTableViewReload.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
  end;
end;

procedure TAdvTableViewReload.SetOffset(const Value: Single);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
  end;
end;

procedure TAdvTableViewReload.SetProgressMode(
  const Value: TAdvTableViewReloadProgressMode);
begin
  if FProgressMode <> Value then
  begin
    FProgressMode := Value;
  end;
end;

procedure TAdvTableViewReload.SetSize(const Value: Single);
begin
  if FSize <> Value then
  begin
    FSize := Value;
  end;
end;

procedure TAdvTableViewReload.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

{$IFDEF WEBLIB}
function TAdvTreeViewTableViewFloatingNodes.GetItem(Index: Integer): TAdvTreeViewVirtualNode;
begin
  Result := TAdvTreeViewVirtualNode(inherited Items[Index]);
end;

procedure TAdvTreeViewTableViewFloatingNodes.SetItem(Index: Integer; const Value: TAdvTreeViewVirtualNode);
begin
  inherited Items[Index] := Value;
end;

function TAdvTableViewLookupBarDisplayList.GetItem(Index: Integer): TAdvTableViewLookupBarDisplayItem;
begin
  Result := TAdvTableViewLookupBarDisplayItem(inherited Items[Index]);
end;

procedure TAdvTableViewLookupBarDisplayList.SetItem(Index: Integer; const Value: TAdvTableViewLookupBarDisplayItem);
begin
  inherited Items[Index] := Value;
end;

function TAdvTableViewDisplayList.GetItem(Index: Integer): TAdvTableViewDisplayItem;
begin
  Result := TAdvTableViewDisplayItem(inherited Items[Index]);
end;

procedure TAdvTableViewDisplayList.SetItem(Index: Integer; const Value: TAdvTableViewDisplayItem);
begin
  inherited Items[Index] := Value;
end;

function TAdvTableViewCustomCharList.GetItem(Index: Integer): TAdvTableViewCustomChar;
begin
  Result := TAdvTableViewCustomChar(inherited Items[Index]);
end;

procedure TAdvTableViewCustomCharList.SetItem(Index: Integer; const Value: TAdvTableViewCustomChar);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TAdvTableViewCustomChar }

constructor TAdvTableViewCustomChar.Create(AID: Integer; AActive: Boolean);
begin
  FID := AID;
  FActive := AActive;
end;

{ TAdvTableViewCustomCharList }

procedure TAdvTableViewCustomCharList.ActivateID(AID: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID = AID then
    begin
      Items[I].Active := True;
      Break;
    end;
  end;
end;

function TAdvTableViewCustomCharList.IsIDActive(AID: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID = AID then
    begin
      Result := Items[I].Active;
      Break;
    end;
  end;
end;


end.
