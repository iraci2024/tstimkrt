{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }
unit dxListView;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  ImgList,
{$IFDEF DELPHI17}
  UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, ComCtrls,
  Messages, Types,
  Generics.Defaults, Generics.Collections, dxGenerics, cxDrawTextUtils,
  dxCore, dxCoreClasses, cxClasses, cxControls, dxGDIPlusClasses,
  cxLookAndFeels, cxLookAndFeelPainters,
  cxGraphics, cxGeometry, dxCoreGraphics, cxCustomCanvas, dxDirectX.D2D.Classes,
  cxMemo;

type
  TdxCustomListView = class;
  TdxListGroup = class;
  TdxListItem = class;
  TdxListViewPainter = class;
  TdxListViewPainterClass = class of TdxListViewPainter;
  TdxListViewOptions = class;
  TdxListViewController = class;
  TdxListItems = class;
  TdxListColumns = class;

  TdxListItemViewInfo = class;
  TdxListViewCustomGroupViewInfo = class;
  TdxListViewGroupViewInfo = class;
  TdxListViewViewInfo = class;
  TdxListViewColumnHeadersViewInfo = class;
  TdxListItemReportStyleViewInfo = class;

  { TdxListViewPersistent }

  TdxListViewPersistent = class(TcxOwnedPersistent)
  strict private
    function GetListView: TdxCustomListView;
  protected
    property ListView: TdxCustomListView read GetListView;
  public
    constructor Create(AOwner: TdxCustomListView); reintroduce; virtual;
  end;

  { TdxListItem }

  TdxListItem = class(TPersistent)
  strict protected
  const
  private
    FOwner: TdxListItems;
    FSubItems: TStrings;
    FCut: Boolean;
    FImageIndex: TImageIndex;
    FIndent: Integer;
    FIndex: Integer;
    FLoadingGroupID: Integer;
    FOverlayIndex: TImageIndex;
    FStateIndex: TImageIndex;
    FCaption: string;
    FDeleting: Boolean;
    FChecked: Boolean;
    FData: TCustomData;
    FGroup: TdxListGroup;
    FEnabled: Boolean;
    FHint: string;
    FTag: Int64;
    function GetChecked: Boolean;
    function GetFocused: Boolean;
    function GetIndex: Integer;
    function GetListView: TdxCustomListView; inline;
    function GetLeft: Integer;
    function GetGroupID: Integer;
    function GetSelected: Boolean;
    function GetTop: Integer;
    procedure SetChecked(AValue: Boolean);
    procedure SetCaption(const AValue: string);
    procedure SetCut(AValue: Boolean);
    procedure SetData(AValue: TCustomData);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFocused(AValue: Boolean);
    procedure SetImage(AIndex: Integer; AValue: TImageIndex);
    procedure SetIndent(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetSubItems(AValue: TStrings);
    procedure SetTop(AValue: Integer);
    function GetSubItemImage(AIndex: Integer): Integer;
    procedure SetSubItemImage(AIndex: Integer; const AValue: Integer);
    procedure SetGroup(AValue: TdxListGroup);
    procedure SetGroupID(AValue: Integer);
  protected
    procedure Changed(AType: TdxChangeType = ctHard); virtual;
    procedure FixupGroup;
    function IsEnabled: Boolean;
    function IsEqual(AItem: TdxListItem): Boolean;
  public
    constructor Create(AOwner: TdxListItems); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
    procedure CancelEdit;
    procedure Delete;
    function DisplayRect(Code: TDisplayCode): TRect;
    function EditCaption: Boolean;
    function GetPosition: TPoint;
    procedure MakeVisible(PartialOK: Boolean);
    procedure SetPosition(const Value: TPoint);

    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Cut: Boolean read FCut write SetCut;
    property Data: TCustomData read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    property Group: TdxListGroup read FGroup write SetGroup;
    property GroupID: Integer read GetGroupID write SetGroupID default -1;
    property ImageIndex: TImageIndex index 0 read FImageIndex write SetImage;
    property Indent: Integer read FIndent write SetIndent default 0;
    property Index: Integer read GetIndex;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TdxCustomListView read GetListView;
    property Owner: TdxListItems read FOwner;
    property OverlayIndex: TImageIndex index 1 read FOverlayIndex
      write SetImage;
    property Position: TPoint read GetPosition write SetPosition;
    property Selected: Boolean read GetSelected write SetSelected;
    property StateIndex: TImageIndex index 2 read FStateIndex write SetImage;
    property SubItems: TStrings read FSubItems write SetSubItems;
    property SubItemImages[Index: Integer]: Integer read GetSubItemImage
      write SetSubItemImage;
    property Top: Integer read GetTop write SetTop;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Hint: string read FHint write FHint;
    property Tag: Int64 read FTag write FTag default 0;
  end;

  TdxListItemClass = class of TdxListItem;

  { TdxListItemsEnumerator }

  TdxListItemsEnumerator = class
  private
    FIndex: Integer;
    FListItems: TdxListItems;
  public
    constructor Create(AListItems: TdxListItems);
    function GetCurrent: TdxListItem;
    function MoveNext: Boolean;
    property Current: TdxListItem read GetCurrent;
  end;

  { TdxListItems }

  TdxListItems = class(TPersistent)
  private
    FCount: Integer;
    FList: TdxFastObjectList;
    FListView: TdxCustomListView;
    procedure ReadItemData(AStream: TStream);
    procedure WriteItemData(AStream: TStream);
  protected
    procedure Changed; virtual;
    procedure DefineProperties(AFiler: TFiler); override;
    procedure FixupGroups;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxListItem;
    procedure SetCount(AValue: Integer);
    procedure SetItem(AIndex: Integer; AValue: TdxListItem);
    procedure SetUpdateState(AUpdating: Boolean);

    property List: TdxFastObjectList read FList;
  public
    constructor Create(AListView: TdxCustomListView); virtual;
    destructor Destroy; override;
    function Add: TdxListItem;
    function AddItem(AItem: TdxListItem; AIndex: Integer = -1): TdxListItem;
    procedure Assign(ASource: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure EndUpdate;
    function GetEnumerator: TdxListItemsEnumerator;
    function GetItemAtPos(const P: TPoint): TdxListItem;
    function IndexOf(AValue: TdxListItem): Integer;
    function Insert(AIndex: Integer): TdxListItem;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TdxListItem read GetItem
      write SetItem; default;
    property ListView: TdxCustomListView read FListView;
  end;

  { TdxListGroup }

  TdxListGroupOption = (Collapsible, Focusable, Hidden, NoHeader, SelectItems);
  TdxListGroupOptions = set of TdxListGroupOption;

  TdxListGroup = class(TcxComponentCollectionItem)
  protected const
    DefaultOptions = [TdxListGroupOption.Focusable,
      TdxListGroupOption.SelectItems];
  strict private
    FCollapsed: Boolean;
    FHeader: string;
    FFooter: string;
    FHeaderAlign: TAlignment;
    FFooterAlign: TAlignment;
    FItemIndices: TdxIntegerList;
    FOptions: TdxListGroupOptions;
    FSubtitle: string;
    FTitleImage: TImageIndex;
    function GetGroupID: Integer; inline;
    function GetItemCount: Integer; inline;
    function GetItem(AIndex: Integer): TdxListItem; inline;
    function GetListView: TdxCustomListView; inline;
    procedure SetCollapsed(const AValue: Boolean);
    procedure SetHeader(const AValue: string);
    procedure SetFooter(const AValue: string);
    procedure SetHeaderAlign(const AValue: TAlignment);
    procedure SetFooterAlign(const AValue: TAlignment);
    procedure SetOptions(const AValue: TdxListGroupOptions);
    procedure SetSubtitle(const AValue: string);
    procedure SetTitleImage(const AValue: TImageIndex);
  protected
    procedure AddItem(AItem: TdxListItem);
    procedure RemoveItem(AItem: TdxListItem);
    procedure Changed;
    procedure DettachItems;
    function GetDisplayName: string; override;
    function IsCollapsible: Boolean;
    function IsFocusable: Boolean;
    function IsVisible: Boolean;

    property ItemIndices: TdxIntegerList read FItemIndices;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetCollectionFromParent(AParent: TComponent)
      : TcxComponentCollection; override;
    procedure SelectAll;
    procedure SelectRange(AFirstIndex, ALastIndex: Integer);

    property GroupID: Integer read GetGroupID;
    property ListView: TdxCustomListView read GetListView;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TdxListItem read GetItem;
  published
    property Collapsed: Boolean read FCollapsed write SetCollapsed
      default False;
    property Header: string read FHeader write SetHeader;
    property Footer: string read FFooter write SetFooter;
    property HeaderAlign: TAlignment read FHeaderAlign write SetHeaderAlign
      default taLeftJustify;
    property FooterAlign: TAlignment read FFooterAlign write SetFooterAlign
      default taLeftJustify;
    property Options: TdxListGroupOptions read FOptions write SetOptions
      default DefaultOptions;
    property Subtitle: string read FSubtitle write SetSubtitle;
    property TitleImage: TImageIndex read FTitleImage write SetTitleImage
      default -1;
  end;

  TdxListGroupClass = class of TdxListGroup;

  { TdxListGroups }

  TdxListGroups = class(TcxInterfacedComponentCollection)
  strict private
    FListView: TdxCustomListView;
    function GetItem(AIndex: Integer): TdxListGroup;
    procedure SetItem(AIndex: Integer; AValue: TdxListGroup);
  protected
    function GetFirstVisibleGroup: TdxListGroup;
    procedure SetItemName(AItem: TcxComponentCollectionItem;
      ABaseIndex: Integer = -1); override;

    property ListView: TdxCustomListView read FListView;
  public
    constructor Create(AParentComponent: TComponent;
      AItemClass: TcxComponentCollectionItemClass); override;
    function Add: TdxListGroup;
    function FindByHeader(const AHeader: string;
      out AGroup: TdxListGroup): Boolean;
    function FindByID(AId: Integer): TdxListGroup;
    function GetGroupAtPos(const P: TPoint): TdxListGroup;
    function GetItemAtPos(const P: TPoint): TdxListItem;

    property Items[AIndex: Integer]: TdxListGroup read GetItem
      write SetItem; default;
  end;

  TdxListGroupsClass = class of TdxListGroups;

  { TdxListColumn }

  TdxListColumn = class(TcxComponentCollectionItem)
  protected const
    DefaultColumnWidth = 50;
    UndefinedCreatedOrderIndex = -2;
  strict private
    FAlignment: TAlignment;
    FCaption: string;
    FHeaderAlignment: TAlignment;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FImageIndex: TImageIndex;
    FSortOrder: TdxSortOrder;
    FWidth: Integer;
    FTag: Integer;
    function GetColumns: TdxListColumns; inline;
    function GetListView: TdxCustomListView; inline;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetHeaderAlignment(AValue: TAlignment);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetMaxWidth(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetSortOrder(AValue: TdxSortOrder);
    procedure SetWidth(AValue: Integer);
    //
    procedure ReadCreatedOrderIndex(AReader: TReader);
    procedure WriteCreatedOrderIndex(AWriter: TWriter);
  protected
    FSubItemIndex: Integer;
    FCreatedOrderIndex: Integer;
    procedure Changed;
    procedure DefineProperties(AFiler: TFiler); override;
    function GetCollectionFromParent(AParent: TComponent)
      : TcxComponentCollection; override;
    function GetDisplayName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyBestFit;

    property Columns: TdxListColumns read GetColumns;
    property ListView: TdxCustomListView read GetListView;
    property SortOrder: TdxSortOrder read FSortOrder write SetSortOrder
      default soNone;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property HeaderAlignment: TAlignment read FHeaderAlignment
      write SetHeaderAlignment default taLeftJustify;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      default -1;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property Tag: Integer read FTag write FTag default 0;
    property Width: Integer read FWidth write SetWidth
      default DefaultColumnWidth;
  end;

  TdxListColumnClass = class of TdxListColumn;

  { TdxListColumns }

  TdxListColumns = class(TcxInterfacedComponentCollection)
  strict private
    FListView: TdxCustomListView;
    function GetItem(AIndex: Integer): TdxListColumn;
    procedure SetItem(AIndex: Integer; AValue: TdxListColumn);
  protected
    procedure Notify(AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); override;
    procedure RebuildSubItemIndices;
    procedure SetItemName(AItem: TcxComponentCollectionItem;
      ABaseIndex: Integer = -1); override;
    procedure Update(AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); override;
    procedure ValidateCreateIndices;
  public
    constructor Create(AParentComponent: TComponent;
      AItemClass: TcxComponentCollectionItemClass); override;
    destructor Destroy; override;
    function Add: TdxListColumn;
    procedure BeginUpdate; override;
    procedure EndUpdate(AForceUpdate: Boolean = True); override;

    property ListView: TdxCustomListView read FListView;
    property Items[AIndex: Integer]: TdxListColumn read GetItem
      write SetItem; default;
  end;

  { TdxListViewCustomViewInfo }

  TdxListViewCustomViewInfo = class
  strict private
    FListView: TdxCustomListView;
    function GetController: TdxListViewController; inline;
    function GetExplorerStyle: Boolean; inline;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetOptions: TdxListViewOptions; inline;
    function GetPainter: TdxListViewPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected
    FBounds: TRect;

    procedure BeforeDraw; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); virtual; abstract;
    function GetHint: string; virtual;
    function GetHintBounds: TRect; virtual;
    function GetItemsGap: Integer; virtual;
    function IsGroupView: Boolean; inline;
    function IsReportView: Boolean; inline;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); virtual;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;

    property ExplorerStyle: Boolean read GetExplorerStyle;
    property ListView: TdxCustomListView read FListView;
  public
    constructor Create(AListView: TdxCustomListView);
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); virtual;
    procedure Draw(ACanvas: TcxCustomCanvas);
    procedure Invalidate; virtual;
    //
    property Bounds: TRect read FBounds;
    property Controller: TdxListViewController read GetController;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property Options: TdxListViewOptions read GetOptions;
    property Painter: TdxListViewPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxListViewCellViewParams }

  TdxListItemStateViewKind = (None, CheckBox, Glyph); // for internal use

  TdxListViewCellViewParams = class // for internal use
  public
    Padding: TdxPadding;
    Font: TcxCanvasBasedFont;
    GlyphIndent: Integer;
    GlyphSize: TSize;
    StateGlyphSize: TSize;
    GlyphsAreaSize: TSize;
    Images: TCustomImageList;
    StateImages: TCustomImageList;
    RightToLeftAlignmet: Boolean;
    TextConstraints: TSize;
    TextFlags: Integer;
    TextLineCount: Integer;
    TextPosition: TcxPosition;
    StateViewKind: TdxListItemStateViewKind;
    destructor Destroy; override;
    function GetNonTextWidth: Integer;
    function GetReportNonTextWidth(ACheckState, AGheckGlyph: Boolean): Integer;
  end;

  { TdxListViewCellViewInfo }

  TdxListViewCellViewInfo = class(TdxListViewCustomViewInfo)
  strict private
    FImageIndex: Integer;
    FIsDirty: Boolean;
    FTextLayout: TcxCanvasBasedTextLayout;
    FViewParams: TdxListViewCellViewParams;
    function GetOrigin: TPoint; inline;
    procedure SetOrigin(const P: TPoint);
  protected
    FContentBounds: TRect;
    FGlyphBounds: TRect;
    FGlyphsAreaBounds: TRect;
    FTextAreaBounds: TRect;
    FTextBounds: TRect;
    FTextSize: TSize;
    function CalculateBoundsCore(const ABounds, AFullBounds: TRect;
      const ASize: TSize; APosition: TcxPosition): TRect;
    procedure CalculateContentBounds; virtual;
    procedure CalculateGlyphsAreaBounds; virtual;
    procedure CalculateGlyphBounds(const ABounds: TRect); virtual;
    procedure CalculateGlyphsBounds(const ABounds: TRect); virtual;
    procedure CalculateTextAreaBounds(const ABounds: TRect); virtual;
    procedure CalculateTextAndGlyphLayout; virtual;
    procedure CalculateTextBounds; virtual;
    function HasGlyph: Boolean; virtual;
    procedure UpdateBounds; virtual;
    procedure DoCalculate; virtual;
    procedure DoRightToLeftAlignmet; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    procedure DrawGlyphCore(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
      AMode: TcxImageDrawMode);
    procedure DrawGlyph(ACanvas: TcxCustomCanvas); virtual;
    procedure DrawText(ACanvas: TcxCustomCanvas); virtual;
    function GetGlyphState: TcxImageDrawMode; virtual;
    function GetTextColor: TColor; virtual; abstract;
    function GetTextFlags: Integer; virtual;
    procedure Initialize(const AText: string; AImageIndex: Integer); overload;
    procedure MakeDirty; inline;
    procedure Offset(ADX, ADY: Integer); virtual;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); override;
    procedure MouseLeave; override;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); override;

    property ContentBounds: TRect read FContentBounds;
    property GlyphsAreaBounds: TRect read FGlyphsAreaBounds;
    property IsDirty: Boolean read FIsDirty;
  public
    constructor Create(AListView: TdxCustomListView;
      const AViewParams: TdxListViewCellViewParams);
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;

    property GlyphBounds: TRect read FGlyphBounds;
    property ImageIndex: Integer read FImageIndex;
    property Origin: TPoint read GetOrigin write SetOrigin;
    property TextLayout: TcxCanvasBasedTextLayout read FTextLayout;
    property TextAreaBounds: TRect read FTextAreaBounds;
    property TextBounds: TRect read FTextBounds;
    property ViewParams: TdxListViewCellViewParams read FViewParams;
  end;

  { TdxListViewItemCustomViewInfo }

  TdxListItemPart = (None, Content, Text, SubItemText, Glyph, StateGlyph);

  TdxListItemCustomViewInfo = class(TdxListViewCellViewInfo)
  strict private
    FCachedItem: TdxListItem;
    FItemIndex: Integer;
    FOwner: TdxListViewCustomGroupViewInfo;
    function GetItem: TdxListItem; inline;
    procedure SetHotTrackPart(AValue: TdxListItemPart);
  protected
    FHotTrackPart: TdxListItemPart;
    FStateAreaBounds: TRect;
    FStateGlyphBounds: TRect;
    FState: TdxListViewItemStates;
    procedure BeforeDraw; override;
    procedure CalculateCheckGlyphBounds; virtual;
    procedure CalculateGlyphsBounds(const ABounds: TRect); override;
    procedure DoRightToLeftAlignmet; override;
    procedure DrawBackground(ACanvas: TcxCustomCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    procedure DrawGlyph(ACanvas: TcxCustomCanvas); override;
    procedure DrawStateGlyph(ACanvas: TcxCustomCanvas); virtual;
    procedure DrawText(ACanvas: TcxCustomCanvas); override;
    function GetGlyphState: TcxImageDrawMode; override;
    function GetState: TdxListViewItemStates;
    function GetTextColor: TColor; override;
    procedure Initialize(AItem: TdxListItem); overload; virtual;
    function IsHovered: Boolean;
    procedure Offset(ADX, ADY: Integer); override;
    procedure InvalidatePart(APart: TdxListItemPart); virtual;
    procedure MouseLeave; override;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); override;
    function StartDrag(AShift: TShiftState; const AMousePos: TPoint)
      : Boolean; virtual;
    function StartEdit(AShift: TShiftState; const AMousePos: TPoint)
      : Boolean; virtual;
    function StartMultiSelection(AShift: TShiftState; const AMousePos: TPoint)
      : Boolean; virtual;

    property CachedItem: TdxListItem read FCachedItem;
    property CheckBounds: TRect read FStateAreaBounds;
    property HotTrackPart: TdxListItemPart read FHotTrackPart
      write SetHotTrackPart;
    property ItemIndex: Integer read FItemIndex;
    property Owner: TdxListViewCustomGroupViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxListViewCustomGroupViewInfo;
      const AItemIndex: Integer; const AViewParams: TdxListViewCellViewParams);
      reintroduce; virtual;
    function GetPart(const P: TPoint): TdxListItemPart; virtual;

    property Item: TdxListItem read GetItem;
    property StateGlyphBounds: TRect read FStateGlyphBounds;
    property State: TdxListViewItemStates read FState;
  end;

  { TdxListViewItemViewInfo }

  TdxListItemViewInfo = class(TdxListItemCustomViewInfo);

  { TdxListSubItemViewInfo }

  TdxListSubItemViewInfo = class(TdxListViewCellViewInfo)
  strict private
    FColumnIndex: Integer;
    FOwner: TdxListItemReportStyleViewInfo;
  protected
    procedure CalculateGlyphsAreaBounds; override;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetTextColor: TColor; override;
    function GetTextFlags: Integer; override;
    function HasGlyph: Boolean; override;
    procedure Initialize(AColumnIndex: Integer; const AText: string;
      AImageIndex: Integer); overload;

    property ColumnIndex: Integer read FColumnIndex;
    property Owner: TdxListItemReportStyleViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxListItemReportStyleViewInfo;
      const AViewParams: TdxListViewCellViewParams);
  end;

  { TdxListItemReportStyleViewInfo }

  TdxListItemReportStyleViewInfo = class(TdxListItemCustomViewInfo)
  strict private
    FColumnIndex: Integer;
    FHiddenGlyph: Boolean;
    FItemBounds: TRect;
    FSharedBackground: Boolean;
    FStateButtonInSubItem: Boolean;
    FSubItems: TdxFastObjectList;
    function GetSubItem(Index: Integer): TdxListSubItemViewInfo; inline;
    function GetSubItemCount: Integer;
  protected
    procedure AddSubItem(AViewInfo: TdxListSubItemViewInfo);
    procedure BeforeDraw; override;
    procedure CalculateCheckGlyphBounds; override;
    procedure CalculateGlyphsAreaBounds; override;
    procedure CalculateGlyphsBounds(const ABounds: TRect); override;
    procedure DoCalculate; override;
    procedure DrawBackground(ACanvas: TcxCustomCanvas); override;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetColumnTextFlags(AColumnIndex: Integer): Integer;
    function GetTextFlags: Integer; override;
    function HasGlyph: Boolean; override;
    procedure Initialize(AItem: TdxListItem); override;
    function StartEdit(AShift: TShiftState; const AMousePos: TPoint)
      : Boolean; override;
  public
    constructor Create(AOwner: TdxListViewCustomGroupViewInfo;
      const AItemIndex: Integer;
      const AViewParams: TdxListViewCellViewParams); override;
    destructor Destroy; override;

    property ItemBounds: TRect read FItemBounds;
    property SharedBackground: Boolean read FSharedBackground;
    property SubItemCount: Integer read GetSubItemCount;
    property SubItems[Index: Integer]: TdxListSubItemViewInfo
      read GetSubItem; default;
  end;

  { TdxListViewCustomGroupViewInfo }

  TdxListGroupPart = (None, Content, Item, Header, Footer, ExpandButton);

  TdxListViewCustomGroupViewInfo = class(TdxListViewCustomViewInfo)
  strict private
    FGroup: TdxListGroup;
    FHotTrackPart: TdxListGroupPart;
    FOwner: TdxListViewViewInfo;
    function GetItem(Index: Integer): TdxListItemCustomViewInfo; inline;
    function GetItemCount: Integer; inline;
    function GetItemSize: TSize; inline;
    function GetVisibleItem(Index: Integer): TdxListItemCustomViewInfo; inline;
    function GetVisibleItemCount: Integer; inline;
    procedure SetHotTrackPart(AValue: TdxListGroupPart);
  protected
    FColumnCount: Integer;
    FColumnWidths: TdxIntegerList;
    FContentBounds: TRect;
    FContentVisibleBounds: TRect;
    FItems: TdxFastObjectList;
    FItemsAreaBounds: TRect;
    FVisibleItems: TdxFastList;
    FRowCount: Integer;

    procedure AddVisibleItemViewInfo(AViewInfo: TdxListItemCustomViewInfo);
    procedure CalculateRowAndColumnCount; virtual;
    procedure CalculateContent(AType: TdxChangeType); virtual;
    procedure CalculateContentBounds; virtual;
    procedure CalculateContentVisibleBounds; virtual;
    procedure CalculateItems(AType: TdxChangeType); virtual;
    function CalculateItemViewInfo(AIndex: Integer; const ABounds: TRect)
      : TdxListItemCustomViewInfo; virtual;
    procedure CalculateHorizontallyArrangedIcons(AType: TdxChangeType); virtual;
    procedure CalculateVerticallyArrangedIcons(AType: TdxChangeType); virtual;
    procedure CalculateIconsViewStyle(AType: TdxChangeType); virtual;
    procedure CalculateListViewStyle(AType: TdxChangeType); virtual;
    procedure CalculateReportViewStyle(AType: TdxChangeType); virtual;
    procedure CalculateItemsArea; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetAvailableHeightForItems: Integer; virtual;
    function GetAvailableWidthForItems: Integer; virtual;
    function GetBoundsForItem(AItemIndex: Integer): TRect; overload;
    function GetBoundsForItem(AItem: TdxListItem): TRect; overload; virtual;
    function GetItemOrigin(AItemIndex: Integer): TPoint;
    function GetItemsHeight(ACount: Integer): Integer;
    function GetItemsWidth(ACount: Integer): Integer;
    procedure GetVisibleItemsRange(out AFirstOrderIndex, ALastOrderIndex
      : Integer);
    procedure InvalidatePart(APart: TdxListGroupPart); virtual;
    function IsHorizontalItemsArrangement: Boolean;
    procedure PopulateItems; virtual; abstract;
    procedure MouseLeave; override;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); override;
    function GetNextItem(var AItemIndex: Integer;
      ADirectionX, ADirectionY: Integer): Boolean;
    function ItemIndexToOrderIndex(AItemIndex: Integer): Integer;
    function OrderIndexToItemIndex(AVisualIndex: Integer): Integer;

    property HotTrackPart: TdxListGroupPart read FHotTrackPart
      write SetHotTrackPart;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxListItemCustomViewInfo read GetItem;
    property ItemSize: TSize read GetItemSize;
    property Owner: TdxListViewViewInfo read FOwner;
    property VisibleItemCount: Integer read GetVisibleItemCount;
    property VisibleItems[Index: Integer]: TdxListItemCustomViewInfo
      read GetVisibleItem;
  public
    constructor Create(AOwner: TdxListViewViewInfo;
      AGroup: TdxListGroup); virtual;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    function FindItemViewInfo(AItem: TdxListItem;
      out AViewInfo: TdxListItemCustomViewInfo): Boolean; overload;
    function GetItemAtPos(const P: TPoint;
      out AViewInfo: TdxListItemCustomViewInfo): Boolean; overload;
    function GetPart(const P: TPoint;
      out AItemViewInfo: TdxListItemCustomViewInfo): TdxListGroupPart; virtual;
    //
    property ColumnCount: Integer read FColumnCount;
    property ContentBounds: TRect read FContentBounds;
    property Group: TdxListGroup read FGroup;
    property ItemsAreaBounds: TRect read FItemsAreaBounds;
    property RowCount: Integer read FRowCount;
  end;

  { TdxListViewRootItemsLayoutViewInfo }

  TdxListViewRootItemsLayoutViewInfo = class(TdxListViewCustomGroupViewInfo)
  protected
    procedure PopulateItems; override;
  end;

  { TdxListViewGroupTextViewInfo }

  TdxListViewGroupTextViewInfo = class(TdxListViewCellViewInfo)
  strict private
    FKind: TdxListViewGroupTextKind;
    FOwner: TdxListViewGroupViewInfo;
  protected
    function GetTextColor: TColor; override;
    function GetTextFlags: Integer; override;
    property Owner: TdxListViewGroupViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxListViewGroupViewInfo;
      AKind: TdxListViewGroupTextKind;
      const AViewParams: TdxListViewCellViewParams);

    property Kind: TdxListViewGroupTextKind read FKind;
  end;

  { TdxListViewGroupViewInfo }

  TdxListViewGroupViewInfo = class(TdxListViewCustomGroupViewInfo)
  strict private
    FExpandButtonState: TcxButtonState;
    FHeaderState: TdxListViewGroupHeaderStates;
    function GetClosed: Boolean; inline;
    function GetContentPadding: TdxPadding; inline;
    function GetFocused: Boolean; inline;
    function GetHotTracked: Boolean; inline;
    function GetHeaderHeight: Integer; inline;
    function GetHeaderParams: TdxListViewCellViewParams; inline;
    function GetFooterHeight: Integer; inline;
    function GetFooterParams: TdxListViewCellViewParams; inline;
    function GetSubtitleHeight: Integer; inline;
    function GetSubtitleParams: TdxListViewCellViewParams; inline;
  protected
    FExpandButtonBounds: TRect;
    FHeader: TdxListViewGroupTextViewInfo;
    FHeaderBounds: TRect;
    FHeaderHottrackBounds: TRect;
    FHeaderLineBounds: TRect;
    FFooter: TdxListViewGroupTextViewInfo;
    FFooterBounds: TRect;
    FSubtitle: TdxListViewGroupTextViewInfo;
    FSubtitleBounds: TRect;
    procedure BeforeDraw; override;
    procedure CalculateContent(AType: TdxChangeType); override;
    function CalculateContentBottom: Integer;
    procedure CalculateContentBounds; override;
    function CalculateContentRight: Integer;
    procedure CalculateFooter(AType: TdxChangeType); virtual;
    procedure CalculateHeader(AType: TdxChangeType); virtual;
    function CalculateHeaderLeft: Integer;
    function CalculateHeaderWidth: Integer;
    procedure CalculateItemsArea; override;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function HasExpandButton: Boolean; inline;
    function HasFooter: Boolean; inline;
    function HasHeader: Boolean; inline;
    function HasSubtitle: Boolean; inline;
    function GetAvailableHeightForItems: Integer; override;
    function GetAvailableWidthForItems: Integer; override;
    function GetExpandButtonState: TcxButtonState; virtual;
    function GetFooterTextFlags: Integer; virtual;
    function GetHeaderState: TdxListViewGroupHeaderStates; virtual;
    function GetHeaderTextFlags: Integer; virtual;
    procedure InvalidatePart(APart: TdxListGroupPart); override;
    procedure PopulateItems; override;

    property ContentPadding: TdxPadding read GetContentPadding;
    property ExpandButtonState: TcxButtonState read FExpandButtonState;
    property HeaderHeight: Integer read GetHeaderHeight;
    property HeaderParams: TdxListViewCellViewParams read GetHeaderParams;
    property FooterHeight: Integer read GetFooterHeight;
    property FooterParams: TdxListViewCellViewParams read GetFooterParams;
    property SubtitleHeight: Integer read GetSubtitleHeight;
    property SubtitleParams: TdxListViewCellViewParams read GetSubtitleParams;
  public
    destructor Destroy; override;
    function GetPart(const P: TPoint;
      out AItemViewInfo: TdxListItemCustomViewInfo): TdxListGroupPart; override;
    procedure Invalidate; override;
    //
    property Closed: Boolean read GetClosed;
    property ExpandButtonBounds: TRect read FExpandButtonBounds;
    property Header: TdxListViewGroupTextViewInfo read FHeader;
    property HeaderBounds: TRect read FHeaderBounds;
    property HeaderHottrackBounds: TRect read FHeaderHottrackBounds;
    property HeaderLineBounds: TRect read FHeaderLineBounds;
    property HeaderState: TdxListViewGroupHeaderStates read FHeaderState;
    property HotTracked: Boolean read GetHotTracked;
    property Focused: Boolean read GetFocused;
    property Footer: TdxListViewGroupTextViewInfo read FFooter;
    property FooterBounds: TRect read FFooterBounds;
    property Subtitle: TdxListViewGroupTextViewInfo read FSubtitle;
    property SubtitleBounds: TRect read FSubtitleBounds;
  end;

  { TdxListViewDropTargetViewInfo }

  TdxListViewDropTargetViewInfo = class(TdxListViewCustomViewInfo)
  strict private
  const
    DropTargetSize = 2;
  strict private
    FSide: TcxBorder;
    FTargetBounds: TRect;
    FTargetObject: TObject;
    function GetSize: Integer;
  protected
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
  public
    procedure Calculate(ATargetObject: TObject; const ATargetBounds: TRect;
      ASide: TcxBorder); reintroduce; virtual;
    //
    property Side: TcxBorder read FSide;
    property Size: Integer read GetSize;
    property TargetBounds: TRect read FTargetBounds;
    property TargetObject: TObject read FTargetObject;
  end;

  { TdxListColumnHeaderViewInfo }

  TdxListColumnHeaderViewInfo = class(TdxListViewCellViewInfo)
  protected type
    THottrackPart = (None, Content, FilterButton);
  strict private
    FOwner: TdxListViewColumnHeadersViewInfo;
    FColumn: TdxListColumn;
    FHotTrackPart: THottrackPart;
    FSortArrowBounds: TRect;
    FState: TcxButtonState;
    procedure SetHotTrackPart(AValue: THottrackPart);
  protected
    procedure BeforeDraw; override;
    procedure CalculateTextAndGlyphLayout; override;
    procedure CheckShowHint;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetHint: string; override;
    function GetHintBounds: TRect; override;
    function GetHottrackPart(const AMousePos: TPoint): THottrackPart; virtual;
    function GetState: TcxButtonState; virtual;
    function GetTextColor: TColor; override;
    function GetTextFlags: Integer; override;
    function HasGlyph: Boolean; override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); override;
    procedure MouseLeave; override;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); override;

    property HotTrackPart: THottrackPart read FHotTrackPart
      write SetHotTrackPart;
    property Owner: TdxListViewColumnHeadersViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxListViewColumnHeadersViewInfo;
      AColumn: TdxListColumn; const AViewParams: TdxListViewCellViewParams);

    property Column: TdxListColumn read FColumn;
    property SortArrowBounds: TRect read FSortArrowBounds;
    property State: TcxButtonState read FState;
  end;

  { TdxListViewColumnHeadersViewInfo }

  TdxListViewColumnHeadersViewInfo = class(TdxListViewCustomViewInfo)
  strict private
    FHottrackItem: TdxListColumnHeaderViewInfo;
    FItems: TdxFastObjectList;
    FOwner: TdxListViewViewInfo;
    FSortArrowSize: TPoint;
    FViewParams: TdxListViewCellViewParams;
    function GetItem(Index: Integer): TdxListColumnHeaderViewInfo;
    function GetItemCount: Integer;
    procedure SetHottrackItem(AValue: TdxListColumnHeaderViewInfo);
  protected
    function CalculateHeight: Integer; virtual;
    procedure CalculateViewParams; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function FindItem(const AMousePos: TPoint): TdxListColumnHeaderViewInfo;
    function GetCursor(const AMousePos: TPoint): TCursor; virtual;
    function GetResizingColumn(const AMousePos: TPoint): TdxListColumn; virtual;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); override;
    procedure MouseLeave; override;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); override;

    property HottrackItem: TdxListColumnHeaderViewInfo read FHottrackItem
      write SetHottrackItem;
    property Owner: TdxListViewViewInfo read FOwner;
    property SortArrowSize: TPoint read FSortArrowSize;
  public
    constructor Create(AOwner: TdxListViewViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    procedure PopulateItems;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxListColumnHeaderViewInfo
      read GetItem; default;
    property ViewParams: TdxListViewCellViewParams read FViewParams;
  end;

  { TdxListViewHintHelper }

  TdxListViewHintHelper = class(TcxControlHintHelper)
  private
    FViewInfo: TdxListViewViewInfo;
  protected
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerControl: TcxControl; override;
    function PtInCaller(const P: TPoint): Boolean; override;
  public
    constructor Create(AViewInfo: TdxListViewViewInfo);
    procedure MouseDown; override;
  end;

  { TdxListViewViewInfo }

  TdxListViewViewInfo = class(TdxListViewCustomViewInfo)
  strict private
    FColumnHeadersViewInfo: TdxListViewColumnHeadersViewInfo;
    FHintHelper: TdxListViewHintHelper;
    FItemViewParams: TdxListViewCellViewParams;
    FGroupHeaderViewParams: TdxListViewCellViewParams;
    FGroupFooterViewParams: TdxListViewCellViewParams;
    FGroupSubtitleViewParams: TdxListViewCellViewParams;
    function GetColumnAutoWidth: Boolean;
    function GetContentOffset: TRect; inline;
    function GetItem(Index: Integer): TdxListItemCustomViewInfo; inline;
    function GetItemCount: Integer; inline;
    function GetGroup(Index: Integer): TdxListViewCustomGroupViewInfo; inline;
    function GetGroupCount: Integer; inline;
  protected
    FColumnHeadersBounds: TRect;
    FContentBounds: TRect;
    FDropTarget: TdxListViewDropTargetViewInfo;
    FGroupColumnCount: Integer;
    FGroupContentPadding: TdxPadding;
    FGroupExpandButtonSize: TSize;
    FGroupFooterHeight: Integer;
    FGroupHeaderHeight: Integer;
    FGroupRowCount: Integer;
    FGroups: TdxFastObjectList;
    FGroupSubtitleHeight: Integer;
    FItems: TdxFastList;
    FItemSize: TSize;
    FTextAreaSize: TSize;
    function AreGroupsVertical: Boolean;
    procedure CalculateContent(AType: TdxChangeType); virtual;
    procedure CalculateContentBounds; virtual;
    procedure CalculateCommonParameters; virtual;
    procedure CalculateItemSize; virtual;
    function CalculateImagesGlyphSize(AImages: TCustomImageList)
      : TSize; virtual;
    function CalculateItemStateGlyphSize: TSize; virtual;
    function CalculateItemTextSize: TSize; virtual;
    function CalculateItemGlyphsAreaSize: TSize; virtual;
    function GetDefaultItemGlyphIndent: Integer; virtual;
    function GetIconsGlyphSideGap: Integer; virtual;
    function GetItemStateViewKind: TdxListItemStateViewKind;
    function GetScrollArea: TRect;
    function GetGroupVisibleBounds: TRect;
    function IsOverlappedItemStateGlyph: Boolean;

    procedure CalculateCommonViewParams(AViewParams: TdxListViewCellViewParams;
      APadding: TdxPadding; AFont: TFont; AImages: TCustomImageList); virtual;
    procedure CalculateItemViewParams(AViewParams
      : TdxListViewCellViewParams); virtual;
    procedure CalculateGroupCommonViewParams(AViewParams
      : TdxListViewCellViewParams; AFont: TFont; AImages: TCustomImageList;
      AUsePainterPadding: Boolean); virtual;
    procedure CalculateGroupHeaderViewParams(AViewParams
      : TdxListViewCellViewParams); virtual;
    procedure CalculateGroupFooterViewParams(AViewParams
      : TdxListViewCellViewParams); virtual;
    procedure CalculateGroupSubtitleViewParams(AViewParams
      : TdxListViewCellViewParams); virtual;
    procedure CalculateGroupHeaderSizes; virtual;
    procedure CalculateGroupViewParams; virtual;
    procedure CalculateViewParams; virtual;

    function CreateDropTargetViewInfo: TdxListViewDropTargetViewInfo; virtual;
    function CreateGroupViewInfo(AGroup: TdxListGroup)
      : TdxListViewGroupViewInfo; virtual;
    function CreateItemViewInfo(AOwner: TdxListViewCustomGroupViewInfo;
      AItemIndex: Integer): TdxListItemCustomViewInfo; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function FindColumn(APosition: TPoint): TdxListColumn;
    function GetAvailableGroupsAreaHeight: Integer; virtual;
    function GetAvailableGroupsAreaWidth: Integer; virtual;
    function GetBorderWidths: TRect; virtual;
    function GetBoundsForItem(AItem: TdxListItem): TRect; overload;
    function GetBoundsForItem(AItemIndex: Integer): TRect; overload;
    function GetItemGlyphImages: TCustomImageList; virtual;
    function GetItemsOffset: TSize;
    function GetItemTextLineCount: Integer; virtual;
    function GetItemTextFlags: Integer; virtual;
    function GetItemTextPosition: TcxPosition; virtual;
    procedure RecreateSubItems; virtual;
    procedure ValidateColumnHeadersViewInfo;

    function CanStartMultiSelectionByMouse(const AMousePos: TPoint)
      : Boolean; virtual;
    procedure CheckHint(AElementViewInfo: TdxListViewCustomViewInfo);
    procedure MouseLeave; override;

    procedure CalculateColumnBestFitParams(AColumn: TdxListColumn;
      var ACheckState, ACheckGlyph: Boolean); virtual;
    function CalculateColumnValueWidth(AItem: TdxListItem; ASubIndex: Integer;
      ACheckState, ACheckGlyph: Boolean): Integer; virtual;
    function CalculateColumnHeaderBestFitWidth(AColumn: TdxListColumn)
      : Integer; virtual;
    function CalculateColumnBestFitWidth(AColumn: TdxListColumn)
      : Integer; virtual;

    property HintHelper: TdxListViewHintHelper read FHintHelper;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxListItemCustomViewInfo read GetItem;

    property ContentOffset: TRect read GetContentOffset;
    property ColumnHeadersViewInfo: TdxListViewColumnHeadersViewInfo
      read FColumnHeadersViewInfo;
    property GroupContentPadding: TdxPadding read FGroupContentPadding;
    property GroupExpandButtonSize: TSize read FGroupExpandButtonSize;
    property GroupHeaderHeight: Integer read FGroupHeaderHeight;
    property GroupHeaderViewParams: TdxListViewCellViewParams
      read FGroupHeaderViewParams;
    property GroupFooterHeight: Integer read FGroupFooterHeight;
    property GroupFooterViewParams: TdxListViewCellViewParams
      read FGroupFooterViewParams;
    property GroupSubtitleHeight: Integer read FGroupSubtitleHeight;
    property GroupSubtitleViewParams: TdxListViewCellViewParams
      read FGroupSubtitleViewParams;
    property ItemViewParams: TdxListViewCellViewParams read FItemViewParams;
  public
    constructor Create(AListView: TdxCustomListView); virtual;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    function FindGroupViewInfo(AGroup: TdxListGroup;
      out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean; overload;
    function FindGroupViewInfo(AItem: TdxListItem;
      out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean; overload;
    function FindGroupViewInfo(AItemIndex: Integer;
      out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean; overload;
    function FindItemViewInfo(AItemIndex: Integer;
      out AViewInfo: TdxListItemCustomViewInfo): Boolean; overload;
    function GetGroupAtPos(const P: TPoint;
      out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean; overload;
    function GetItemAtPos(const P: TPoint;
      out AViewInfo: TdxListItemCustomViewInfo): Boolean; overload;
    //
    property BorderWidths: TRect read GetBorderWidths;
    property ColumnAutoWidth: Boolean read GetColumnAutoWidth;
    property GroupColumnCount: Integer read FGroupColumnCount;
    property ContentBounds: TRect read FContentBounds;
    property DropTarget: TdxListViewDropTargetViewInfo read FDropTarget;
    property GroupCount: Integer read GetGroupCount;
    property Groups[Index: Integer]: TdxListViewCustomGroupViewInfo
      read GetGroup;
    property ItemSize: TSize read FItemSize;
    property GroupRowCount: Integer read FGroupRowCount;
    property TextAreaSize: TSize read FTextAreaSize;
  end;

  { TdxListViewController }

  TdxListViewController = class(TdxListViewPersistent)
  protected type
    TSelectAnchorInfo = record
    public
      GroupID: Integer;
      ItemIndex: Integer;
      constructor Create(AGroupID, AItemIndex: Integer);
      function IsNull: Boolean;
      procedure Reset;
    end;
  strict private
    FUpdateItemsSelectionOnMouseUp: Boolean;
    FEditingTimer: TcxTimer;
    FEditingItemIndex: Integer;
    FIsPressedItemSelected: Boolean;
    FPressedItemIndex: Integer;
    FSelectGroupItemsTimer: TcxTimer;
    FSelectionChangedFlag: Boolean;
    FSelectItemsGroup: TdxListGroup;
    FSelectedIndices: TdxIntegerList;
    FSelectAnchor: TSelectAnchorInfo;
    FDragCopy: Boolean;
    FDragItemIndex: Integer;
    FFocusedGroup: TdxListGroup;
    FFocusedItemIndex: Integer;
    FMouseHoveredGroup: TdxListGroup;
    FMouseHoveredItemIndex: Integer;
    FMousePressed: Boolean;
    FPressedColumn: TdxListColumn;
    FResizingColumn: TdxListColumn;
    FOriginalResizingColumnWidth: Integer;
    FEditWasClosed: Boolean;
    procedure FinishEditingTimer;
    procedure FinishSelectGroupItemsTimer;
    function GetDropTargetInfo: TdxListViewDropTargetViewInfo;
    function GetViewInfo: TdxListViewViewInfo; inline;
    procedure SelectGroupItems(Sender: TObject);
    procedure SetFocusedGroup(AValue: TdxListGroup);
    procedure SetFocusedItemIndex(AItemIndex: Integer);
    procedure SetMouseHoveredGroup(AValue: TdxListGroup);
    procedure SetMouseHoveredItemIndex(AItemIndex: Integer);
    procedure SetMousePressed(AValue: Boolean);
    procedure SetPressedColumn(AValue: TdxListColumn);
    procedure StartEditing(Sender: TObject);
    procedure StartEditingTimer;
    procedure StartSelectGroupItemsTimer;
  protected
    procedure CalculateDropTarget(X, Y: Integer); overload; virtual;
    procedure CalculateDropTarget(const AMousePos: TPoint); overload;
    procedure CancelEdit;
    function CheckStartEditingOnMouseUp(AItemViewInfo
      : TdxListItemCustomViewInfo; AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint): Boolean; virtual;
    procedure ResetDropTarget; virtual;

    procedure InvalidateItem(AItem: TdxListItem); overload;
    function IsGroupView: Boolean;
    procedure MakeItemVisible(AItemIndex: Integer;
      AVisibleType: TdxVisibilityType; ACheckHorizontalPosition,
      ACheckVerticalPosition: Boolean); overload; virtual;
    procedure MakeItemVisible(AItem: TdxListItem;
      AVisibleType: TdxVisibilityType); overload;
    procedure MakeItemVisible(AItemIndex: Integer;
      AVisibleType: TdxVisibilityType); overload;
    procedure RemoveContentHottrack; virtual;
    procedure ResetContent; virtual;
    procedure SetFocusedItemIndexCore(AItemIndex: Integer;
      AVisibleType: TdxVisibilityType; ACheckHorizontalPosition,
      ACheckVerticalPosition: Boolean); virtual;

    procedure AfterKeyDown(AKey: Word; AShift: TShiftState); virtual;
    procedure AfterMouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;
    procedure BeforeKeyDown(AKey: Word; AShift: TShiftState); virtual;
    procedure BeforeMouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;
    procedure ProcessItemMouseDown(AItemViewInfo: TdxListItemCustomViewInfo;
      AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;

    procedure UpdateGroupViewState(AGroup: TdxListGroup);
    procedure UpdateItemViewState(AItemIndex: Integer);
    procedure UpdateMouseHottrack(const AMousePos: TPoint); overload; inline;
    procedure UpdateMouseHottrack; overload;

    // ListView selection
    procedure AddSelection(ASelection: TdxIntegerList);
    procedure CheckSelectionChangedFlag;
    procedure ClearSelection(AItemIndexToExclude: Integer = -1);
    function GetFirstFocusibleItemInGroup(AGroup: TdxListGroup): Integer;
    function GetLastFocusibleItemInGroup(AGroup: TdxListGroup): Integer;
    procedure GetItems(const AAnchor1, AAnchor2: TSelectAnchorInfo;
      AItems: TdxIntegerList); overload;
    procedure GetItems(AGroup: TdxListGroup; AItemIndex: Integer;
      ADown: Boolean; AItems: TdxIntegerList); overload;
    function IsItemSelected(AItemIndex: Integer): Boolean;
    function MultiSelect: Boolean;
    procedure ReplaceSelection(ASelection: TdxIntegerList);
    procedure ResetSelection;
    procedure ResetSelectionChangedFlag;
    procedure SelectFirstAvailableItemInGroup(AGroup: TdxListGroup);
    procedure SelectItem(AItemIndex: Integer; ASelect: Boolean); virtual;
    procedure SelectItems(AStartIndex, AFinishIndex: Integer);
      overload; virtual;
    procedure SelectItems(const ANewAnchor: TSelectAnchorInfo); overload;
    procedure SelectSingleItem(AItemIndex: Integer); virtual;

    // Navigation
    procedure GotoItemIndex(AItemIndex: Integer; AShift: TShiftState);
    procedure GotoFirstFocusibleItem(AShift: TShiftState);
    procedure GotoLastFocusibleItem(AShift: TShiftState);
    function GetStartItemIndexForKeyboardNavigation: Integer; virtual;
    procedure SelectNextItem(AItemIndex: Integer;
      ADirectionX, ADirectionY: Integer; AShift: TShiftState);
      overload; virtual;
    procedure ShowPriorPage(AShift: TShiftState);
    procedure ShowNextPage(AShift: TShiftState);

    // drag and drop
    function CanDrag(const AMousePos: TPoint): Boolean; virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; virtual;

    property EditingItemIndex: Integer read FEditingItemIndex;
    property PressedColumn: TdxListColumn read FPressedColumn
      write SetPressedColumn;
    property SelectedIndices: TdxIntegerList read FSelectedIndices;
  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;

    // Keyboard
    procedure FocusEnter; virtual;
    procedure FocusLeave; virtual;
    procedure KeyDown(AKey: Word; AShift: TShiftState); virtual;
    procedure KeyUp(AKey: Word; AShift: TShiftState); virtual;
    // Mouse
    procedure CancelMode; virtual;
    procedure Click; virtual;
    procedure DblClick; virtual;
    function GetCursor(const AMousePos: TPoint): TCursor; virtual;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(AShift: TShiftState; const AMousePos: TPoint); virtual;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); virtual;
    // Drag-n-Drop
    procedure DragEnter; virtual;
    procedure DragLeave; virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure DragOver(ASource: TObject; const AMousePos: TPoint;
      var AAccept: Boolean); virtual;
    //
    property DragCopy: Boolean read FDragCopy write FDragCopy;
    property DropTargetInfo: TdxListViewDropTargetViewInfo
      read GetDropTargetInfo;
    property FocusedItemIndex: Integer read FFocusedItemIndex
      write SetFocusedItemIndex;
    property FocusedGroup: TdxListGroup read FFocusedGroup
      write SetFocusedGroup;
    property MouseHoveredGroup: TdxListGroup read FMouseHoveredGroup
      write SetMouseHoveredGroup;
    property MouseHoveredItemIndex: Integer read FMouseHoveredItemIndex
      write SetMouseHoveredItemIndex;
    property MousePressed: Boolean read FMousePressed write SetMousePressed;
    property ViewInfo: TdxListViewViewInfo read GetViewInfo;
  end;

  { TdxListViewCustomOptions }

  TdxListViewCustomOptions = class(TdxListViewPersistent)
  protected
    procedure Changed(AType: TdxChangeType = ctHard);
    procedure ChangeScale(M, D: Integer); virtual;
    function NeedNotifyControl: Boolean; virtual;
  end;

  { TdxListViewOptionsIcon }

  TdxListIconsArrangement = (Horizontal, Vertical);

  TdxListViewOptionsIcons = class(TdxListViewCustomOptions)
  public const
    MaxTextLineCount = 8;
    MaxItemsGap = 48;
    MinSmallIconsColumnWidth = 32;
    DefaultSmallIconsColumnWidth = 115;
  strict private
    FArrangement: TdxListIconsArrangement;
    FAutoArrange: Boolean;
    FSmallIconsColumnWidth: Integer;
    FItemsGap: Integer;
    FTextLineCount: Integer;
    procedure SetArrangement(AValue: TdxListIconsArrangement);
    procedure SetAutoArrange(AValue: Boolean);
    procedure SetSmallIconsColumnWidth(AValue: Integer);
    procedure SetTextLineCount(AValue: Integer);
    procedure SetItemsGap(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;

    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange
      default True;
  public
    constructor Create(AOwner: TdxCustomListView); override;
  published
    property Arrangement: TdxListIconsArrangement read FArrangement
      write SetArrangement default TdxListIconsArrangement.Horizontal;
    property SmallIconsColumnWidth: Integer read FSmallIconsColumnWidth
      write SetSmallIconsColumnWidth default DefaultSmallIconsColumnWidth;
    property ItemsGap: Integer read FItemsGap write SetItemsGap default 1;
    property TextLineCount: Integer read FTextLineCount write SetTextLineCount
      default 2;
  end;

  { TdxListViewOptionsList }

  TdxListViewOptionsList = class(TdxListViewCustomOptions)
  public const
    DefaultColumnWidth = 300;
    MinColumnWidth = 32;
    MaxItemsGap = 48;
  strict private
    FColumnWidth: Integer;
    FItemsGap: Integer;
    procedure SetColumnWidth(AValue: Integer);
    procedure SetItemsGap(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    function NeedNotifyControl: Boolean; override;
  public
    constructor Create(AOwner: TdxCustomListView); override;
  published
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth
      default DefaultColumnWidth;
    property ItemsGap: Integer read FItemsGap write SetItemsGap default 1;
  end;

  { TdxListViewOptionsReport }

  TdxListViewOptionsReport = class(TdxListViewCustomOptions)
  strict private
    FAlwaysShowItemImageInFirstColumn: Boolean;
    FRowSelect: Boolean;
    FShowColumsHeaders: Boolean;
    procedure SetAlwaysShowItemImageInFirstColumn(AValue: Boolean);
    procedure SetRowSelect(AValue: Boolean);
    procedure SetShowColumsHeaders(AValue: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
    function NeedNotifyControl: Boolean; override;
  public
    constructor Create(AOwner: TdxCustomListView); override;
  published
    property AlwaysShowItemImageInFirstColumn: Boolean
      read FAlwaysShowItemImageInFirstColumn
      write SetAlwaysShowItemImageInFirstColumn default False;
    property RowSelect: Boolean read FRowSelect write SetRowSelect
      default False;
    property ShowColumsHeaders: Boolean read FShowColumsHeaders
      write SetShowColumsHeaders default True;
  end;

  { TdxListViewPaddingOptions }

  TdxListViewPaddingOptions = class(TdxListViewCustomOptions)
  private
    FGroupContent: TcxMargin;
    FGroupHeader: TcxMargin;
    FItem: TcxMargin;
    FView: TcxMargin;
    procedure ChangeHandler(Sender: TObject);
    procedure SetGroupContent(const AValue: TcxMargin);
    procedure SetGroupHeader(const AValue: TcxMargin);
    procedure SetItem(const AValue: TcxMargin);
    procedure SetView(const AValue: TcxMargin);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function CreatePadding(ADefaultValue: Integer): TcxMargin;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;
  published
    property GroupContent: TcxMargin read FGroupContent write SetGroupContent;
    property GroupHeader: TcxMargin read FGroupHeader write SetGroupHeader;
    property Item: TcxMargin read FItem write SetItem;
    property View: TcxMargin read FView write SetView;
  end;

  { TdxListViewOptions }

  TdxListViewSortType = (None, Data, Text, Both);

  TdxListViewOptions = class(TdxListViewCustomOptions)
  strict private
    FColumnAutoWidth: Boolean;
    FExplorerStyle: Boolean;
    FGroupView: Boolean;
    FHotTrack: Boolean;
    FIcons: TdxListViewOptionsIcons;
    FItemShowHint: Boolean;
    FList: TdxListViewOptionsList;
    FMultiSelect: Boolean;
    FOwnerData: Boolean;
    FPaddingOptions: TdxListViewPaddingOptions;
    FReadOnly: Boolean;
    FReport: TdxListViewOptionsReport;
    FScaleImagesForDPI: Boolean;
    FShowCheckBoxes: Boolean;
    FSortType: TdxListViewSortType;
    procedure SetColumnAutoWidth(AValue: Boolean);
    procedure SetExplorerStyle(AValue: Boolean);
    procedure SetGroupView(AValue: Boolean);
    procedure SetIcons(const AValue: TdxListViewOptionsIcons);
    procedure SetList(const AValue: TdxListViewOptionsList);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetOwnerData(AValue: Boolean);
    procedure SetPaddingOptions(const AValue: TdxListViewPaddingOptions);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetReport(const AValue: TdxListViewOptionsReport);
    procedure SetScaleImagesForDPI(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetSortType(AValue: TdxListViewSortType);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateIcons: TdxListViewOptionsIcons; virtual;
    function CreateList: TdxListViewOptionsList; virtual;
    function CreateReport: TdxListViewOptionsReport; virtual;
    function CreatePadding: TdxListViewPaddingOptions; virtual;
    procedure DoAssign(Source: TPersistent); override;

    property ColumnAutoWidth: Boolean read FColumnAutoWidth
      write SetColumnAutoWidth default False;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;

    property ExplorerStyle: Boolean read FExplorerStyle write SetExplorerStyle
      default False;
  published
    property GroupView: Boolean read FGroupView write SetGroupView
      default False;
    property Icons: TdxListViewOptionsIcons read FIcons write SetIcons;
    property ItemShowHint: Boolean read FItemShowHint write FItemShowHint
      default False;
    property List: TdxListViewOptionsList read FList write SetList;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect
      default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData
      default False;
    property PaddingOptions: TdxListViewPaddingOptions read FPaddingOptions
      write SetPaddingOptions;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Report: TdxListViewOptionsReport read FReport write SetReport;
    property ScaleImagesForDPI: Boolean read FScaleImagesForDPI
      write SetScaleImagesForDPI default False;
    property ShowCheckBoxes: Boolean read FShowCheckBoxes
      write SetShowCheckBoxes default False;
    property SortType: TdxListViewSortType read FSortType write SetSortType
      default TdxListViewSortType.None;
  end;

  { TdxListViewDragSelectDragAndDropObject }

  TdxListViewDragSelectDragAndDropObject = class(TcxDragAndDropObject)
  // for internal use
  strict private
    FAnchor: TPoint;
    FExtendedMode: Boolean;
    FFinishPos: TPoint;
    FStartPos: TPoint;
    function GetListView: TdxCustomListView;
  protected
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState)
      : Boolean; override;
    function ProcessKeyUp(AKey: Word; AShiftState: TShiftState)
      : Boolean; override;
  public
    property FinishPos: TPoint read FFinishPos;
    property ListView: TdxCustomListView read GetListView;
    property StartPos: TPoint read FStartPos;
  end;

  { TdxListViewDragObject }

  TdxListViewDragObject = class(TcxDragControlObject) // for internal use
  strict private
    function GetListView: TdxCustomListView;
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    constructor Create(AControl: TControl); override;

    property ListView: TdxCustomListView read GetListView;
  end;

  { TdxListViewFonts }

  TdxListViewFonts = class(TdxListViewPersistent)
  strict private
    FColumnHeader: TFont;
    FGroupFooter: TFont;
    FGroupHeader: TFont;
    FGroupSubtitle: TFont;
    FItem: TFont;
    FSubItem: TFont;
    procedure FontChanged(Sender: TObject);
    procedure SetColumnHeader(const AValue: TFont);
    procedure SetGroupFooter(const AValue: TFont);
    procedure SetGroupHeader(const AValue: TFont);
    procedure SetGroupSubtitle(const AValue: TFont);
    procedure SetItem(const AValue: TFont);
    procedure SetSubItem(const AValue: TFont);
  protected
    FLockChanges: Boolean;
    procedure Changed; virtual;
    function CreateFont: TFont;
  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetFont(AFont: TFont; AKeepColors: Boolean);
  published
    property ColumnHeader: TFont read FColumnHeader write SetColumnHeader;
    property GroupHeader: TFont read FGroupHeader write SetGroupHeader;
    property GroupFooter: TFont read FGroupFooter write SetGroupFooter;
    property GroupSubtitle: TFont read FGroupSubtitle write SetGroupSubtitle;
    property Item: TFont read FItem write SetItem;
    property SubItem: TFont read FSubItem write SetSubItem;
  end;

  { TdxListViewImages }

  TdxListViewImages = class(TdxListViewPersistent)
  strict private
    FColumnHeaderImages: TCustomImageList;
    FGroupHeaderImages: TCustomImageList;
    FLargeImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FStateImages: TCustomImageList;
    FColumnHeaderImagesChangeLink: TChangeLink;
    FGroupHeaderImagesChangeLink: TChangeLink;
    FLargeImagesChangeLink: TChangeLink;
    FSmallImagesChangeLink: TChangeLink;
    FStateImagesChangeLink: TChangeLink;
    procedure SetColumnHeaderImages(const AValue: TCustomImageList);
    procedure SetGroupHeaderImages(const AValue: TCustomImageList);
    procedure SetLargeImages(const AValue: TCustomImageList);
    procedure SetSmallImages(const AValue: TCustomImageList);
    procedure SetStateImages(const AValue: TCustomImageList);
    procedure ImageListChanged(Sender: TObject);
  protected
    function AreImagesLinked(const AValue: TObject): Boolean; virtual;
    function CreateImagesChangeLink: TChangeLink;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property ColumnHeaderImages: TCustomImageList read FColumnHeaderImages
      write SetColumnHeaderImages;
    property GroupHeaderImages: TCustomImageList read FGroupHeaderImages
      write SetGroupHeaderImages;
    property LargeImages: TCustomImageList read FLargeImages
      write SetLargeImages;
    property SmallImages: TCustomImageList read FSmallImages
      write SetSmallImages;
    property StateImages: TCustomImageList read FStateImages
      write SetStateImages;
  end;

  { TdxListViewAutoScrollHelper }

  TdxListViewAutoScrollHelper = class(TdxListViewPersistent) // for internal use
  strict private
    FAfterScroll: TProc;
    FClientBounds: TRect;
    FCurrentDirection: TcxDirection;
    FScrollArea: array [TcxDirection] of TRect;
    FHasScrollArea: array [TScrollBarKind] of Boolean;
    FTimer: TcxTimer;
    procedure Calculate;
    function GetDirection(const APosition: TPoint): TcxDirection;
    function GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar; virtual;
    procedure GetScrollBarParams(AKind: TScrollBarKind;
      var AMin, AMax, APos: Integer); virtual;
    procedure Scroll(AKind: TScrollBarKind; ACode: TScrollCode);
    procedure OnTimer(Sender: TObject);
    procedure StartTimer(ADirection: TcxDirection);
    procedure StopTimer;
  public
    constructor Create(AOwner: TdxCustomListView; const AAfterScroll: TProc);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure CheckMousePosition(const APosition: TPoint);
  end;

  { IdxListViewInplaceEdit }

  IdxListViewInplaceEdit = interface // for internal use
    ['{526C0124-CA8D-43A7-B842-4262B36DBDB6}']
    function GetValue: string;
    procedure Hide;
    procedure Show(AListView: TdxCustomListView; const ABounds: TRect;
      const AText: string; AFont: TFont; ASelStart, ASelLength: Integer;
      AMaxLength: Integer);

    property Value: string read GetValue;
  end;

  { TdxCustomListView }

  TdxListViewStyle = (Icon, SmallIcon, List, Report);

  TdxListViewCompareProc = function(AItem1, AItem2: TdxListItem;
    AData: Integer): Integer;
  TdxListViewCompareEvent = procedure(Sender: TObject;
    AItem1, AItem2: TdxListItem; AData: Integer; var ACompare: Integer)
    of object;
  TdxListViewItemEvent = procedure(Sender: TObject; AItem: TdxListItem)
    of object;
  TdxListViewEditingEvent = procedure(Sender: TObject; AItem: TdxListItem;
    var AllowEdit: Boolean) of object;
  TdxListViewEditedEvent = procedure(Sender: TObject; AItem: TdxListItem;
    var S: string) of object;
  TdxListViewChangeEvent = procedure(Sender: TObject; AItem: TdxListItem;
    AChange: TItemChange) of object;
  TdxListViewChangingEvent = procedure(Sender: TObject; AItem: TdxListItem;
    AChange: TItemChange; var AllowChange: Boolean) of object;
  TdxListViewColumnClickEvent = procedure(Sender: TObject;
    AColumn: TdxListColumn) of object;
  TdxListViewColumnRightClickEvent = procedure(Sender: TObject;
    AColumn: TdxListColumn; APoint: TPoint) of object;
  TdxListViewCreateItemClassEvent = procedure(ASender: TdxCustomListView;
    var AItemClass: TdxListItemClass) of object;
  TdxListViewInfoTipEvent = procedure(ASender: TObject; AItem: TListItem;
    var AInfoTip: string) of object;
  TdxListViewOwnerDataEvent = procedure(ASender: TObject; AItem: TdxListItem)
    of object;
  TdxListViewOwnerDataFindEvent = procedure(ASender: TObject; AFind: TItemFind;
    const AFindString: string; const AFindPosition: TPoint;
    AFindData: TCustomData; AStartIndex: Integer; ADirection: TSearchDirection;
    AWrap: Boolean; var AIndex: Integer) of object;
  TdxListViewOwnerDataHintEvent = procedure(ASender: TObject;
    AStartIndex, AEndIndex: Integer) of object;
  TdxListViewOwnerDataStateChangeEvent = procedure(ASender: TObject;
    AStartIndex, AEndIndex: Integer; AOldState, ANewState: TItemStates)
    of object;
  TdxListViewSelectItemEvent = procedure(Sender: TObject; AItem: TdxListItem;
    ASelected: Boolean) of object;
  TdxListViewSubItemImageEvent = procedure(ASender: TObject; AItem: TdxListItem;
    ASubItem: Integer; var ImageIndex: Integer) of object;

  TdxCustomListView = class(TcxScrollingControl, IdxSkinSupport,
    IdxDirect2DSupport)
  strict private
    FAutoScrollHelper: TdxListViewAutoScrollHelper;
    FCanBeFocused: Boolean;
    FColumns: TdxListColumns;
    FController: TdxListViewController;
    FCurrentDragSelection: TdxHashSet<Integer>;
    FDragSelectRectangle: TRect;
    FOriginalSelection: TdxHashSet<Integer>;
    FEditingItemIndex: Integer;
    FFonts: TdxListViewFonts;
    FGroups: TdxListGroups;
    FItems: TdxListItems;
    FImages: TdxListViewImages;
    FInplaceEditImplementator: TCustomEdit;
    FOptions: TdxListViewOptions;
    FPainter: TdxListViewPainter;
    FTempItem: TdxListItem;
    FViewInfo: TdxListViewViewInfo;
    FViewStyle: TdxListViewStyle;

    FOnChange: TdxListViewChangeEvent;
    FOnChanging: TdxListViewChangingEvent;
    FOnColumnClick: TdxListViewColumnClickEvent;
    FOnColumnRightClick: TdxListViewColumnRightClickEvent;
    FOnCompare: TdxListViewCompareEvent;
    FOnCreateItemClass: TdxListViewCreateItemClassEvent;
    FOnData: TdxListViewOwnerDataEvent;
    FOnDataFind: TdxListViewOwnerDataFindEvent;
    FOnDataHint: TdxListViewOwnerDataHintEvent;
    FOnDataStateChange: TdxListViewOwnerDataStateChangeEvent;
    FOnDeletion: TdxListViewItemEvent;
    FOnEdited: TdxListViewEditedEvent;
    FOnEditing: TdxListViewEditingEvent;
    FOnInsert: TdxListViewItemEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSelectItem: TdxListViewSelectItemEvent;
    function AreItemsStored: Boolean;
    function GetEditingItem: TdxListItem;
    function GetFocusedItem: TdxListItem;
    function GetInplaceEdit: IdxListViewInplaceEdit;
    function GetSelectedItemCount: Integer;
    function GetSelectedItem(Index: Integer): TdxListItem;
    procedure SetColumns(const AValue: TdxListColumns);
    procedure SetFocusedItem(AItem: TdxListItem);
    procedure SetFonts(const AValue: TdxListViewFonts);
    procedure SetGroups(const AValue: TdxListGroups);
    procedure SetImages(const AValue: TdxListViewImages);
    procedure SetItems(const AValue: TdxListItems);
    procedure SetOptions(const AValue: TdxListViewOptions);
    procedure SetViewStyle(const AValue: TdxListViewStyle);

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged);
      message CM_PARENTFONTCHANGED;
  protected
    FLockCount: Integer;
    FIsDragSelectPaintMode: Boolean;
    function CreateColumns: TdxListColumns; virtual;
    function CreateController: TdxListViewController; virtual;
    function CreateInplaceEditImplementator: TCustomEdit;
    function CreateOptions: TdxListViewOptions; virtual;
    function CreatePainter: TdxListViewPainter; virtual;
    function CreateViewInfo: TdxListViewViewInfo; virtual;
    function CreateFonts: TdxListViewFonts; virtual;
    function CreateGroups: TdxListGroups; virtual;
    function CreateImages: TdxListViewImages; virtual;
    function CreateItems: TdxListItems; virtual;

    function CreateListItem: TdxListItem; virtual;
    procedure CreateViewSubClasses; virtual;
    procedure DestroyViewSubClasses; virtual;
    function GetColumnClass: TdxListColumnClass; virtual;
    function GetGroupClass: TdxListGroupClass; virtual;

    // Keyboard operations
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // Mouse operations
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    // TWinControl
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;

    // TcxScrollingControl
    procedure BoundsChanged; override;
    procedure Calculate(AType: TdxChangeType); override;
    function GetContentSize: TSize; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollStep: Integer; override;
    function IsScrollDataValid: Boolean; override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure ScrollPosChanged(const AOffset: TPoint); override;

    // TcxControl
    function AllowTouchScrollUIMode: Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateActualCanvas; override;
    procedure DoCancelMode; override;
    procedure DoChangeScaleEx(M, D: Integer; isDpiChange: Boolean); virtual;
    procedure DoPaint; override;
    procedure FreeActualCanvas; override;
    procedure FontChanged; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetScrollContentForegroundColor: TColor; override;
    function HasScrollBarArea: Boolean; override;
    function IsActive: Boolean;
    // drag and drop
    procedure CreateAutoScrollHelper(const AAfterScroll: TProc);
    procedure DestroyAutoScrollHelper;
    function IsDragging: Boolean;
    // delphi drag and drop
    function CanDrag(X, Y: Integer): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DrawDragImage(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawDragImageContent(ACanvas: TcxCanvas;
      const ABounds: TRect); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragImagesSize: TPoint; override;
    function GetDragObjectClass: TDragControlObjectClass; override;
    function HasDragImages: Boolean; override;
    procedure InitDragImages(ADragImages: TcxDragImageList); override;
    // internal drag and drop
    procedure BeginDragSelectOperation(AExtendedMode: Boolean);
    procedure EndDragSelectOperation;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure UpdateDragSelectState(const ABounds: TRect;
      AExtendedSelection: Boolean);

    // TdxCustomListView
    procedure Change(AItem: TdxListItem; AChange: Integer); virtual;
    function ColumnsShowing: Boolean;
    procedure DeleteItem(AItem: TdxListItem); virtual;
    procedure DoColumnClick(AColumn: TdxListColumn); virtual;
    procedure DoColumnRightClick(AColumn: TdxListColumn;
      APoint: TPoint); virtual;
    procedure DoEdited(AItem: TdxListItem; var ACaption: string); virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoSelectItem(AItemIndex: Integer; ASelected: Boolean);
    procedure DrawMultiSelection(const ABounds: TRect);
    function GetItemAtPos(const P: TPoint): TdxListItem;
    procedure InsertItem(AItem: TdxListItem); virtual;
    function IsItemSelected(AItem: TdxListItem): Boolean;
    function IsUpdateLocked: Boolean;
    function SupportsItemEnabledState: Boolean; virtual;
    function OwnerData: Boolean;
    procedure ResetContent;
    procedure UpdateGroups;
    procedure ViewStyleChanged; virtual;
    // OwnerData
    function GetItem(AIndex: Integer): TdxListItem;
    function OwnerDataFetch(AItem: TdxListItem; ARequest: TItemRequest)
      : Boolean; virtual;
    function OwnerDataFind(AFind: TItemFind; const AFindString: string;
      const AFindPosition: TPoint; AFindData: TCustomData; AStartIndex: Integer;
      ADirection: TSearchDirection; AWrap: Boolean): Integer; virtual;
    function OwnerDataHint(AStartIndex, AEndIndex: Integer): Boolean; virtual;
    function OwnerDataStateChange(AStartIndex, AEndIndex: Integer;
      AOldState, ANewState: TItemStates): Boolean; virtual;
    // Editing
    function CanEdit(AItem: TdxListItem): Boolean; virtual;
    procedure Edit(AItemIndex: Integer; const AText: string); virtual;
    function GetEditingText(AItem: TdxListItem): string; virtual;
    procedure InplaceEditKeyPress(Sender: TObject; var Key: Char); virtual;
    function IsEditingItem(AItemIndex: Integer): Boolean;
    function StartItemCaptionEditing(AItemIndex: Integer): Boolean; virtual;
    procedure FinishItemCaptionEditing(AAccept: Boolean = True); virtual;
    procedure ShowInplaceEdit(AItemIndex: Integer; ABounds: TRect;
      const AText: string); virtual;
    procedure ValidatePasteText(var AText: string); virtual;

    property AutoScrollHelper: TdxListViewAutoScrollHelper
      read FAutoScrollHelper;
    property EditingItem: TdxListItem read GetEditingItem;
    property InplaceEdit: IdxListViewInplaceEdit read GetInplaceEdit;
    property Painter: TdxListViewPainter read FPainter;
    property TempItem: TdxListItem read FTempItem;
    property ViewInfo: TdxListViewViewInfo read FViewInfo;

    property CanBeFocused: Boolean read FCanBeFocused write FCanBeFocused;
    property Options: TdxListViewOptions read FOptions write SetOptions;

    property OnChange: TdxListViewChangeEvent read FOnChange write FOnChange;
    property OnChanging: TdxListViewChangingEvent read FOnChanging
      write FOnChanging;
    property OnColumnClick: TdxListViewColumnClickEvent read FOnColumnClick
      write FOnColumnClick;
    property OnColumnRightClick: TdxListViewColumnRightClickEvent
      read FOnColumnRightClick write FOnColumnRightClick;
    property OnCompare: TdxListViewCompareEvent read FOnCompare
      write FOnCompare;
    property OnCreateItemClass: TdxListViewCreateItemClassEvent
      read FOnCreateItemClass write FOnCreateItemClass;
    property OnData: TdxListViewOwnerDataEvent read FOnData write FOnData;
    property OnDataFind: TdxListViewOwnerDataFindEvent read FOnDataFind
      write FOnDataFind;
    property OnDataHint: TdxListViewOwnerDataHintEvent read FOnDataHint
      write FOnDataHint;
    property OnDataStateChange: TdxListViewOwnerDataStateChangeEvent
      read FOnDataStateChange write FOnDataStateChange;
    property OnDeletion: TdxListViewItemEvent read FOnDeletion
      write FOnDeletion;
    property OnEdited: TdxListViewEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TdxListViewEditingEvent read FOnEditing
      write FOnEditing;
    property OnInsert: TdxListViewItemEvent read FOnInsert write FOnInsert;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
      write FOnSelectionChanged;
    property OnSelectItem: TdxListViewSelectItemEvent read FOnSelectItem
      write FOnSelectItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;

    function AlphaSort: Boolean;
    procedure ApplyBestFit(AColumn: TdxListColumn = nil);
    function CanFocus: Boolean; override;
    function CustomSort(ASortProc: TdxListViewCompareProc;
      AData: Integer): Boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsEditing: Boolean;
    procedure MakeItemVisible(AItem: TdxListItem;
      AVisibleType: TdxVisibilityType);
    procedure UpdateItems(AFirstIndex, ALastIndex: Integer);
    // selection
    procedure ClearSelection;

    // drag-n-drop
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function StartDrag(DragObject: TDragObject): Boolean; override;

    property Columns: TdxListColumns read FColumns write SetColumns;
    property Controller: TdxListViewController read FController;
    // for internal use
    property Fonts: TdxListViewFonts read FFonts write SetFonts;
    property Groups: TdxListGroups read FGroups write SetGroups;
    property Images: TdxListViewImages read FImages write SetImages;
    property FocusedItem: TdxListItem read GetFocusedItem write SetFocusedItem;
    property Items: TdxListItems read FItems write SetItems
      stored AreItemsStored;
    property ParentFont default False;
    property SelectedItemCount: Integer read GetSelectedItemCount;
    property SelectedItems[Index: Integer]: TdxListItem read GetSelectedItem;
    property ViewStyle: TdxListViewStyle read FViewStyle write SetViewStyle
      default TdxListViewStyle.Icon;
  end;

  { TdxListView }

  TdxListView = class(TdxCustomListView)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Fonts;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
    property Visible;

    property BorderStyle default cxcbsDefault;
    property Columns;
    property Images;
    property Groups;
    property Items;
    property LookAndFeel;
    property Options;
    property TabOrder;
    property TabStop;
    property Transparent;
    property ViewStyle;

    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCreateItemClass;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnStartDrag;
  end;

  { TdxListViewPainter }

  TdxListViewPainter = class(TdxListViewPersistent)
  strict private
    FImagesPaintCache: TObject;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected

    function CreateCanvasBasedFont(AFont: TFont): TcxCanvasBasedFont; virtual;
    function CreateCanvasBasedImage(ABitmap: TBitmap;
      AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage; virtual;
    function CreateCanvasBasedTextLayout: TcxCanvasBasedTextLayout; virtual;

    procedure DrawGlyphCore(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
      AMode: TcxImageDrawMode); virtual;
    function DrawItemSelectionFirst: Boolean; virtual;
    procedure PrepareGlyphBitmap(ABitmap: TcxAlphaBitmap;
      AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
      ADrawMode: TcxImageDrawMode; ASmoothImage: Boolean; ABrush: THandle;
      ATransparentColor: TColor; AUseLeftBottomPixelAsTransparent: Boolean;
      APalette: IdxColorPalette); virtual;

  public
    constructor Create(AOwner: TdxCustomListView); override;
    destructor Destroy; override;
    // Background
    procedure DrawBackground(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AExplorerStyle: Boolean); virtual;
    // Group
    procedure DrawGroupHeader(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListViewGroupViewInfo); virtual;
    procedure DrawGroupHeaderBackground(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListViewGroupViewInfo); virtual;
    procedure DrawGroupText(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListViewGroupTextViewInfo); virtual;
    // Items
    procedure DrawCheckButton(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean); virtual;
    procedure DrawItemBackground(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListItemCustomViewInfo); virtual;
    procedure DrawReportItemBackground(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListItemReportStyleViewInfo); virtual;

    procedure DrawMultiSelectionRect(ACanvas: TcxCustomCanvas;
      const ABounds: TRect); virtual;
    // DragDrop
    procedure DrawDropTargetSelection(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxListViewDropTargetViewInfo); virtual;
    //
    procedure InvalidateImageList(AImageList: TCustomImageList);

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

implementation

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Math, cxImageList, RTLConsts, dxHash, cxScrollBar, dxDPIAwareUtils,
  cxLibraryConsts,
  dxTypeHelpers, Clipbrd, cxContainer;

const
  OppositePositionMap: array [TcxPosition] of TcxPosition = (posNone, posRight,
    posLeft, posBottom, posTop);
  HorizontalAlignmentMap: array [TAlignment] of Integer = (CXTO_LEFT,
    CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY);

type
  TCustomEditHelper = class helper for TCustomEdit
  public
    function GetExtraHeight: Integer;
    function GetExtraWidth: Integer;
    procedure SetMargins;
    function GetSingleLineHeight: Integer;
    procedure SetKeyPressEvent(AEvent: TKeyPressEvent);
  end;

  TdxKeyboard = class
  strict private
    class function CheckIsKeyPressed(const Index: Integer): Boolean; static;
  public
    class property IsAltPressed: Boolean index VK_MENU read CheckIsKeyPressed;
    class property IsControlPressed: Boolean index VK_CONTROL
      read CheckIsKeyPressed;
    class property IsShiftPressed: Boolean index VK_SHIFT
      read CheckIsKeyPressed;
  end;

  { TdxListViewSingleLineInplaceEdit }

  TdxListViewSingleLineInplaceEdit = class(TEdit, IdxListViewInplaceEdit)
  // for internal use
  protected const
    Margin = 2;
  private
    FClickTime: Longint;
    FHiding: Boolean;
    FListView: TdxCustomListView;
    procedure CMShowingChanged(var Message: TMessage);
      message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
    function GetValue: string;
    procedure Hide;
    procedure Show(AListView: TdxCustomListView; const ABounds: TRect;
      const AText: string; AFont: TFont; ASelStart, ASelLength: Integer;
      AMaxLength: Integer);

    function CalculateWidth: Integer;
    procedure Change; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure WndProc(var Message: TMessage); override;

    property ListView: TdxCustomListView read FListView;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
  end;

  { TdxListViewMultilineInplaceEdit }

  TdxListViewMultilineInplaceEdit = class(TMemo, IdxListViewInplaceEdit)
  // for internal use
  private
    FAvailableWidth: Integer;
    FClickTime: Longint;
    FHiding: Boolean;
    FInitialBounds: TRect;
    FListView: TdxCustomListView;
    procedure CMShowingChanged(var Message: TMessage);
      message CM_SHOWINGCHANGED;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
    function GetValue: string;
    procedure Hide;
    procedure Show(AListView: TdxCustomListView; const ABounds: TRect;
      const AText: string; AFont: TFont; ASelStart, ASelLength: Integer;
      AMaxLength: Integer);

    procedure CalculateBounds;
    procedure Change; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure WndProc(var Message: TMessage); override;

    property ListView: TdxCustomListView read FListView;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TdxDrawImageCacheID }

  TdxDrawImageCacheID = packed record
    BrushHandle: HBRUSH;
    DrawMode: TcxImageDrawMode;
    ImageIndex: Integer;
    OverlayIndex: Integer;
    ImageList: TCustomImageList;
    PaletteID: TGUID;
    SmoothImage: Boolean;
    TransparentColor: TColor;
    UseLeftBottomPixelAsTransparent: Boolean;

    constructor Create(AImageList: TCustomImageList;
      AImageIndex, AOverlayIndex: Integer; ADrawMode: TcxImageDrawMode;
      AUseLeftBottomPixelAsTransparent, ASmoothImage: Boolean;
      ATransparentColor: TColor; const APalette: IdxColorPalette;
      ABrushHandle: HBRUSH = 0);
    function GetHashCode: Integer;
    function IsEqual(const ID: TdxDrawImageCacheID): Boolean;
    procedure Reset;
  end;

  { TImageListCacheHelper }

  TdxImageListPaintCache = class
{$REGION 'internal types'}
  protected type

    { TImageIdComparer }

    TImageIdComparer = class(TEqualityComparer<TdxDrawImageCacheID>)
    public
      function Equals(const Left, Right: TdxDrawImageCacheID): Boolean;
        override;
      function GetHashCode(const Value: TdxDrawImageCacheID): Integer; override;
    end;

    { TImageListCacheHelper }

    TImageListCacheHelper = class(TComponent)
    strict private
    type

      TImages = class(TObjectDictionary<TdxDrawImageCacheID,
        TcxCanvasBasedImage>)
      public
        constructor Create(ACapacity: Integer);
      end;

      TSizeImages = class(TObjectDictionary<TSize, TImages>);

    strict private
      FCache: TdxImageListPaintCache;
      FImageList: TCustomImageList;
      FImagesChangeLink: TChangeLink;
      FLastImages: TImages;
      FLastSize: TSize;
      FMaxSizeCount: Integer;
      FSizedImages: TSizeImages;
      FSizes: TQueue<TSize>;
    protected
      function GetImage(const ABounds: TRect; AIndex, AOverlayIndex: Integer;
        ADrawMode: TcxImageDrawMode; AUseLeftBottomPixelAsTransparent: Boolean;
        const APalette: IdxColorPalette; ASmoothImage: Boolean)
        : TcxCanvasBasedImage;
      procedure ImageListChanged(Sender: TObject);
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;
    public
      constructor Create(ACache: TdxImageListPaintCache;
        AImages: TCustomImageList; AMaxSizeCount: Integer = 0); reintroduce;
      destructor Destroy; override;
      procedure Draw(ACanvas: TcxCustomCanvas; const ABounds: TRect;
        AIndex, AOverlayIndex: Integer; ADrawMode: TcxImageDrawMode;
        AUseLeftBottomPixelAsTransparent: Boolean;
        const APalette: IdxColorPalette; ASmoothImage: Boolean);
      procedure Invalidate;

      property ImageList: TCustomImageList read FImageList;
    end;
{$ENDREGION}
  strict private
    FLastImages: TCustomImageList;
    FLastHelper: TImageListCacheHelper;
    FImages: TObjectDictionary<TCustomImageList, TImageListCacheHelper>;
    FTransferBitmap: TcxAlphaBitmap;
    FOwner: TdxListViewPainter;
    function GetCanvas: TcxCustomCanvas;
  protected
    procedure SetupHelper(AImages: TCustomImageList);
    procedure RemoveHelper(AHelper: TImageListCacheHelper);

    property Canvas: TcxCustomCanvas read GetCanvas;
    property TransferBitmap: TcxAlphaBitmap read FTransferBitmap;
    property Owner: TdxListViewPainter read FOwner;
  public
    constructor Create(AOwner: TdxListViewPainter);
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AImages: TCustomImageList; AIndex, AOverlayIndex: Integer;
      ADrawMode: TcxImageDrawMode; AUseLeftBottomPixelAsTransparent: Boolean;
      APalette: IdxColorPalette; ASmoothImage: Boolean);
    procedure InvalidateImageList(AImages: TCustomImageList);
    function PrepareImage(const ASize: TSize; const ID: TdxDrawImageCacheID;
      const AColorPalette: IdxColorPalette): TcxCanvasBasedImage;
  end;

  { TCustomEditHelper }

function TCustomEditHelper.GetExtraHeight: Integer;
var
  ADC: HDC;
  ASaveFont: HFont;
  ASysMetrics, AMetrics: TTextMetric;
begin
  if NewStyleControls then
    Result := GetSystemMetrics(SM_CYBORDER) * 6
  else
  begin
    ADC := GetDC(0);
    try
      GetTextMetrics(ADC, ASysMetrics);
      ASaveFont := SelectObject(ADC, Font.Handle);
      GetTextMetrics(ADC, AMetrics);
      SelectObject(ADC, ASaveFont);
    finally
      ReleaseDC(0, ADC);
    end;
    Result := ASysMetrics.tmHeight;
    if Result > AMetrics.tmHeight then
      Result := AMetrics.tmHeight;
    Result := Result div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
end;

function TCustomEditHelper.GetExtraWidth: Integer;
begin
  if NewStyleControls then
    Result := GetSystemMetrics(SM_CXBORDER) * 6
  else
    Result := 4 + GetSystemMetrics(SM_CXBORDER) * 4;
  Inc(Result, 2);
end;

function TCustomEditHelper.GetSingleLineHeight: Integer;
begin
  Result := TdxTextMeasurer.TextLineHeight(Font) + GetExtraHeight;
end;

procedure TCustomEditHelper.SetKeyPressEvent(AEvent: TKeyPressEvent);
begin
  OnKeyPress := AEvent;
end;

procedure TCustomEditHelper.SetMargins;
var
  AMargins: DWord;
begin
  AMargins := MakeLong(1, 1);
  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, AMargins);
end;

{ TdxListViewSingleLineInplaceEdit }

constructor TdxListViewSingleLineInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  Ctl3D := True;
  TabStop := False;
  BorderStyle := bsSingle;
  BorderWidth := 0;
  DoubleBuffered := False;
end;

function TdxListViewSingleLineInplaceEdit.CalculateWidth: Integer;
const
  MinimalWidth = 48;
  ExtraWidth = 20;
var
  ATextWidth: Integer;
  AText: string;
  AMargins: DWord;
begin
  if ListView = nil then
    Exit(MinimalWidth);
  AMargins := SendMessage(Handle, EM_GETMARGINS, 0, 0);
  AText := Text;
  ATextWidth := TdxTextMeasurer.TextSizeDT(Font, AText, DT_SINGLELINE or
    DT_CALCRECT or DT_NOPREFIX).cx;
  Result := Max(MinimalWidth, ATextWidth + LongRec(AMargins).Lo +
    LongRec(AMargins).Hi + ExtraWidth);
end;

procedure TdxListViewSingleLineInplaceEdit.Change;
begin
  Width := CalculateWidth;
  inherited Change;
end;

procedure TdxListViewSingleLineInplaceEdit.CMShowingChanged
  (var Message: TMessage);
begin
  // do nothing
end;

function TdxListViewSingleLineInplaceEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := ListView.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TdxListViewSingleLineInplaceEdit.GetValue: string;
begin
  Result := Text;
end;

procedure TdxListViewSingleLineInplaceEdit.Hide;
begin
  FHiding := True;
  try
    if HandleAllocated and IsWindowVisible(Handle) then
    begin
      Invalidate;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
        SWP_NOREDRAW);
      if (ListView <> nil) and Focused then
        Windows.SetFocus(ListView.Handle);
    end;
  finally
    FHiding := False;
  end;
end;

procedure TdxListViewSingleLineInplaceEdit.Show(AListView: TdxCustomListView;
  const ABounds: TRect; const AText: string; AFont: TFont;
  ASelStart, ASelLength: Integer; AMaxLength: Integer);
var
  AEditBounds: TRect;
begin
  if AFont <> nil then
    Font.Assign(AFont);
  MaxLength := AMaxLength;
  Text := AText;
  FListView := AListView;
  Parent := AListView;
  SetMargins;
  AEditBounds := ABounds;
  AEditBounds.Width := CalculateWidth;
  AEditBounds.Height := GetSingleLineHeight;
  AEditBounds.Offset(-4, Floor((ABounds.Height - AEditBounds.Height) / 2));
  BoundsRect := AEditBounds;
  ShowWindow(Handle, SW_SHOWNORMAL);
  SetFocus;
  case ASelLength of
    MaxInt:
      SelectAll;
    0:
      Exit;
  else
    SelStart := ASelStart;
    SelLength := ASelLength;
  end;
end;

procedure TdxListViewSingleLineInplaceEdit.SetFocus;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

procedure TdxListViewSingleLineInplaceEdit.KeyPress(var Key: Char);
begin
  case Key of
    #9, #13, #27:
      begin
        ListView.FinishItemCaptionEditing(Key <> #27);
        Key := #0;
      end;
  else
    inherited KeyPress(Key);
  end;
end;

procedure TdxListViewSingleLineInplaceEdit.WMGetDlgCode
  (var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure GetSel(AHandle: THandle; var ASelStart: Integer;
  var ASelStop: Integer);
begin
  SendMessage(AHandle, EM_GETSEL, WPARAM(@ASelStart), LPARAM(@ASelStop));
end;

procedure SetSel(AHandle: THandle; ASelStart: Integer; ASelStop: Integer);
begin
  SendMessage(AHandle, EM_SETSEL, ASelStart, ASelStop);
end;

procedure TdxListViewSingleLineInplaceEdit.WMPaste(var Message: TMessage);
var
  AClipboardText: string;
begin
  if ReadOnly then
    inherited
  else
  begin
    Clipboard.Open;
    AClipboardText := Clipboard.AsText;
    Clipboard.Close;
    ListView.ValidatePasteText(AClipboardText);
    if Length(AClipboardText) > 0 then
      SetSelText(AClipboardText);
  end;
end;

procedure TdxListViewSingleLineInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or
          ((ListView <> nil) and GetParentForm(Self).SetFocusedControl(ListView))
        then
          Dispatch(Message);
        Exit;
      end;
    WM_KILLFOCUS:
      if (ListView <> nil) and not FHiding then
        ListView.FinishItemCaptionEditing(not(csDestroying in ComponentState));
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

{ TdxListViewMultilineInplaceEdit }

constructor TdxListViewMultilineInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  Alignment := taCenter;
  Ctl3D := True;
  TabStop := False;
  BorderStyle := bsSingle;
  BorderWidth := 0;
  DoubleBuffered := False;
  WordWrap := True;
  ScrollBars := ssNone;
end;

procedure TdxListViewMultilineInplaceEdit.CMShowingChanged
  (var Message: TMessage);
begin
  // do nothing
end;

function TdxListViewMultilineInplaceEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := ListView.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TdxListViewMultilineInplaceEdit.GetValue: string;
begin
  Result := Text;
end;

procedure TdxListViewMultilineInplaceEdit.Hide;
begin
  FHiding := True;
  try
    if HandleAllocated and IsWindowVisible(Handle) then
    begin
      Invalidate;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
        SWP_NOREDRAW);
      if Focused then
        Windows.SetFocus(ListView.Handle);
    end;
  finally
    FHiding := False;
  end;
end;

procedure TdxListViewMultilineInplaceEdit.Show(AListView: TdxCustomListView;
  const ABounds: TRect; const AText: string; AFont: TFont;
  ASelStart, ASelLength: Integer; AMaxLength: Integer);
var
  AEditBounds: TRect;
begin
  FListView := AListView;
  if AFont <> nil then
    Font.Assign(AFont);
  Parent := AListView;
  MaxLength := AMaxLength;
  Text := AText;
  SetMargins;
  AEditBounds := ABounds;
  FAvailableWidth := ABounds.Width - GetExtraWidth;
  AEditBounds.Offset(0, -GetExtraHeight div 2);
  FInitialBounds := AEditBounds;
  CalculateBounds;
  ShowWindow(Handle, SW_SHOWNORMAL);
  SetFocus;
  case ASelLength of
    MaxInt:
      SelectAll;
    0:
      Exit;
  else
    SelStart := ASelStart;
    SelLength := ASelLength;
  end;
end;

procedure TdxListViewMultilineInplaceEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if (ListView <> nil) and (ATop + AHeight > ListView.ClientBounds.Bottom) then
    ATop := ListView.ClientBounds.Bottom - AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxListViewMultilineInplaceEdit.SetFocus;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

procedure TdxListViewMultilineInplaceEdit.KeyPress(var Key: Char);
begin
  case Key of
    #9, #13, #27:
      begin
        ListView.FinishItemCaptionEditing(Key <> #27);
        Key := #0;
      end;
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TdxListViewMultilineInplaceEdit.CalculateBounds;
var
  R: TRect;
  ALineHeight, ATextLength, AExtraHSize: Integer;
  AText: string;
  ASize: TSize;
begin
  AExtraHSize := ListView.ScaleFactor.Apply(32);
  ALineHeight := TdxTextMeasurer.TextLineHeight(Font);
  AText := Text;
  ATextLength := Length(AText);
  if (ListView = nil) or (ATextLength = 0) then
    ASize.Init(AExtraHSize, ALineHeight)
  else
  begin
    R.InitSize(0, 0, FAvailableWidth, $FFFF);
    TdxTextMeasurer.TextRectDT(R, AText, Font, DT_EDITCONTROL or DT_WORDBREAK or
      DT_CALCRECT or DT_NOPREFIX);
    ASize := R.Size;
    if (ATextLength >= 2) and (AText[ATextLength - 1] = #13) and
      (AText[ATextLength] = #10) then
      Inc(ASize.cy, ALineHeight);
    Inc(ASize.cx, AExtraHSize);
    if ASize.cx > FAvailableWidth then
      ASize.cx := FAvailableWidth;
  end;
  Inc(ASize.cx, GetExtraWidth);
  Inc(ASize.cy, GetExtraHeight);
  R.InitSize(FInitialBounds.Left + (FInitialBounds.Width - ASize.cx) div 2,
    FInitialBounds.Top, ASize);
  BoundsRect := R;
end;

procedure TdxListViewMultilineInplaceEdit.Change;
begin
  CalculateBounds;
  inherited Change;
end;

procedure TdxListViewMultilineInplaceEdit.WMPaste(var Message: TMessage);
var
  AClipboardText: string;
begin
  if ReadOnly then
    inherited
  else
  begin
    Clipboard.Open;
    AClipboardText := Clipboard.AsText;
    Clipboard.Close;
    ListView.ValidatePasteText(AClipboardText);
    if Length(AClipboardText) > 0 then
      SetSelText(AClipboardText);
  end;
end;

procedure TdxListViewMultilineInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or
          ((ListView <> nil) and GetParentForm(Self).SetFocusedControl(ListView))
        then
          Dispatch(Message);
        Exit;
      end;
    WM_KILLFOCUS:
      if (ListView <> nil) and not FHiding then
        ListView.FinishItemCaptionEditing(not(csDestroying in ComponentState));
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

{ TdxImageListPaintCache.TImageListCacheHelper.TImages }

constructor TdxImageListPaintCache.TImageListCacheHelper.TImages.Create
  (ACapacity: Integer);
begin
  inherited Create([doOwnsValues], ACapacity, TImageIdComparer.Create);
end;

{ TdxImageListPaintCache }

constructor TdxImageListPaintCache.TImageListCacheHelper.Create
  (ACache: TdxImageListPaintCache; AImages: TCustomImageList;
  AMaxSizeCount: Integer = 0);
begin
  inherited Create(nil);
  FCache := ACache;
  FImageList := AImages;
  if AMaxSizeCount = 0 then
    AMaxSizeCount := Screen.MonitorCount * 2;
  FMaxSizeCount := AMaxSizeCount;
  FLastSize := cxInvalidSize;
  FSizedImages := TSizeImages.Create([doOwnsValues], FMaxSizeCount);
  FSizes := TQueue<TSize>.Create;
{$IFDEF DELPHIXE}
  FSizes.Capacity := FMaxSizeCount;
{$ENDIF}
  AImages.FreeNotification(Self);
  if not(AImages is TcxCustomImageList) then
  begin
    FImagesChangeLink := TChangeLink.Create;
    FImagesChangeLink.OnChange := ImageListChanged;
  end;
end;

destructor TdxImageListPaintCache.TImageListCacheHelper.Destroy;
begin
  FreeAndNil(FImagesChangeLink);
  FreeAndNil(FSizedImages);
  FreeAndNil(FSizes);
  inherited Destroy;
end;

function TdxImageListPaintCache.TImageListCacheHelper.GetImage
  (const ABounds: TRect; AIndex, AOverlayIndex: Integer;
  ADrawMode: TcxImageDrawMode; AUseLeftBottomPixelAsTransparent: Boolean;
  const APalette: IdxColorPalette; ASmoothImage: Boolean): TcxCanvasBasedImage;
var
  AImageID: TdxDrawImageCacheID;
  AImages: TImages;
  ASize: TSize;
begin
  ASize := ABounds.Size;
  if ASize.IsEqual(FLastSize) then
    AImages := FLastImages
  else
  begin
    if not FSizedImages.TryGetValue(ASize, AImages) then
    begin
      if FSizes.Count = FMaxSizeCount then
        FSizedImages.Remove(FSizes.Dequeue);
      FSizes.Enqueue(ASize);
      AImages := TImages.Create(FImageList.Count);
      FSizedImages.Add(ASize, AImages);
    end;
    FLastSize := ASize;
    FLastImages := AImages;
  end;

  AImageID := TdxDrawImageCacheID.Create(ImageList, AIndex, AOverlayIndex,
    ADrawMode, AUseLeftBottomPixelAsTransparent, ASmoothImage, clNone,
    APalette);
  if not AImages.TryGetValue(AImageID, Result) then
  begin
    Result := FCache.PrepareImage(ASize, AImageID, APalette);
    AImages.Add(AImageID, Result);
  end;
end;

procedure TdxImageListPaintCache.TImageListCacheHelper.Draw
  (ACanvas: TcxCustomCanvas; const ABounds: TRect;
  AIndex, AOverlayIndex: Integer; ADrawMode: TcxImageDrawMode;
  AUseLeftBottomPixelAsTransparent: Boolean; const APalette: IdxColorPalette;
  ASmoothImage: Boolean);
begin
  GetImage(ABounds, AIndex, AOverlayIndex, ADrawMode,
    AUseLeftBottomPixelAsTransparent, APalette, ASmoothImage).Draw(ABounds);
end;

procedure TdxImageListPaintCache.TImageListCacheHelper.Invalidate;
begin
  FLastSize := cxInvalidSize;
  FSizes.Clear;
  FSizedImages.Clear;
end;

procedure TdxImageListPaintCache.TImageListCacheHelper.ImageListChanged
  (Sender: TObject);
begin
  Invalidate;
end;

procedure TdxImageListPaintCache.TImageListCacheHelper.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FImageList) then
  begin
    RemoveFreeNotification(AComponent);
    FCache.RemoveHelper(Self);
  end;
end;

{ TdxDrawImageCacheID }

constructor TdxDrawImageCacheID.Create(AImageList: TCustomImageList;
  AImageIndex, AOverlayIndex: Integer; ADrawMode: TcxImageDrawMode;
  AUseLeftBottomPixelAsTransparent, ASmoothImage: Boolean;
  ATransparentColor: TColor; const APalette: IdxColorPalette;
  ABrushHandle: HBRUSH);
begin
  ImageList := AImageList;
  ImageIndex := AImageIndex;
  OverlayIndex := AOverlayIndex;
  DrawMode := ADrawMode;
  UseLeftBottomPixelAsTransparent := AUseLeftBottomPixelAsTransparent;
  SmoothImage := ASmoothImage;
  TransparentColor := ATransparentColor;
  PaletteID := dxGetColorPaletteID(APalette);
  BrushHandle := ABrushHandle;
end;

function TdxDrawImageCacheID.GetHashCode: Integer;
begin
  Result := dxBobJenkinsHash(Self, SizeOf(Self), 0);
end;

function TdxDrawImageCacheID.IsEqual(const ID: TdxDrawImageCacheID): Boolean;
begin
  Result := CompareMem(@Self, @ID, SizeOf(Self));
end;

procedure TdxDrawImageCacheID.Reset;
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

{ TdxImageListPaintCache }

constructor TdxImageListPaintCache.Create(AOwner: TdxListViewPainter);
begin
  FOwner := AOwner;
  FImages := TObjectDictionary<TCustomImageList, TImageListCacheHelper>.Create
    ([doOwnsValues]);
  FLastImages := nil;
  FLastHelper := nil;
  FTransferBitmap := TcxAlphaBitmap.Create;
end;

destructor TdxImageListPaintCache.Destroy;
begin
  FreeAndNil(FTransferBitmap);
  FreeAndNil(FImages);
  inherited Destroy;
end;

function TdxImageListPaintCache.GetCanvas: TcxCustomCanvas;
begin
  Result := Owner.ListView.ActualCanvas;
end;

procedure TdxImageListPaintCache.RemoveHelper(AHelper: TImageListCacheHelper);
begin
  if FLastHelper = AHelper then
  begin
    FLastHelper := nil;
    FLastImages := nil;
  end;
  FImages.Remove(AHelper.ImageList);
end;

procedure TdxImageListPaintCache.SetupHelper(AImages: TCustomImageList);
var
  AHelper: TImageListCacheHelper;
begin
  Assert(AImages <> nil);
  if AImages <> FLastImages then
  begin
    if not FImages.TryGetValue(AImages, AHelper) then
    begin
      AHelper := TImageListCacheHelper.Create(Self, AImages);
      FImages.Add(AImages, AHelper);
    end;
    FLastHelper := AHelper;
    FLastImages := AImages;
  end;
end;

procedure TdxImageListPaintCache.Draw(ACanvas: TcxCustomCanvas;
  const ABounds: TRect; AImages: TCustomImageList;
  AIndex, AOverlayIndex: Integer; ADrawMode: TcxImageDrawMode;
  AUseLeftBottomPixelAsTransparent: Boolean; APalette: IdxColorPalette;
  ASmoothImage: Boolean);
begin
  SetupHelper(AImages);
  FLastHelper.Draw(ACanvas, ABounds, AIndex, AOverlayIndex, ADrawMode,
    AUseLeftBottomPixelAsTransparent, APalette, ASmoothImage)
end;

procedure TdxImageListPaintCache.InvalidateImageList(AImages: TCustomImageList);
var
  AHelper: TImageListCacheHelper;
begin
  Assert(AImages <> nil);
  if AImages = FLastImages then
    FLastHelper.Invalidate
  else if FImages.TryGetValue(AImages, AHelper) then
    AHelper.Invalidate;
end;

function TdxImageListPaintCache.PrepareImage(const ASize: TSize;
  const ID: TdxDrawImageCacheID; const AColorPalette: IdxColorPalette)
  : TcxCanvasBasedImage;
begin
  TransferBitmap.RefreshImage(ASize.cx, ASize.cy);
  Owner.PrepareGlyphBitmap(TransferBitmap, ID.ImageList, ID.ImageIndex,
    ID.OverlayIndex, ID.DrawMode, ID.SmoothImage, ID.BrushHandle,
    ID.TransparentColor, ID.UseLeftBottomPixelAsTransparent, AColorPalette);
  if ID.BrushHandle <> 0 then
    TransferBitmap.MakeOpaque;
  Result := Canvas.CreateImage(TransferBitmap, afDefined);
end;

{ TdxListViewPersistent }

constructor TdxListViewPersistent.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
end;

function TdxListViewPersistent.GetListView: TdxCustomListView;
begin
  Result := TdxCustomListView(Owner);
end;

{ TdxListGroup }

constructor TdxListGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaderAlign := taLeftJustify;
  FFooterAlign := taLeftJustify;
  FTitleImage := -1;
  FOptions := DefaultOptions;
  FItemIndices := TdxIntegerList.Create;
end;

destructor TdxListGroup.Destroy;
begin
  if not ListView.IsDestroying then
    DettachItems;
  FItemIndices.Free;
  inherited Destroy;
end;

function TdxListGroup.GetCollectionFromParent(AParent: TComponent)
  : TcxComponentCollection;
begin
  Result := (AParent as TdxCustomListView).Groups;
end;

function TdxListGroup.GetGroupID: Integer;
begin
  Result := ID;
end;

function TdxListGroup.GetItemCount: Integer;
begin
  Result := FItemIndices.Count;
end;

function TdxListGroup.GetItem(AIndex: Integer): TdxListItem;
begin
  Result := ListView.GetItem(AIndex);
end;

procedure TdxListGroup.AddItem(AItem: TdxListItem);
begin
  FItemIndices.Add(AItem.Index);
end;

procedure TdxListGroup.Assign(Source: TPersistent);
var
  AGroup: TdxListGroup;
begin
  AGroup := Safe<TdxListGroup>.Cast(Source);
  if AGroup <> nil then
  begin
    FCollapsed := AGroup.Collapsed;
    FHeader := AGroup.Header;
    FFooter := AGroup.Footer;
    FHeaderAlign := AGroup.HeaderAlign;
    FFooterAlign := AGroup.FooterAlign;
    FSubtitle := AGroup.Subtitle;
    FOptions := AGroup.Options;
  end
  else
    inherited Assign(Source);
end;

function TdxListGroup.GetListView: TdxCustomListView;
begin
  if Collection <> nil then
    Result := TdxListGroups(Collection).ListView
  else
    Result := nil;
end;

procedure TdxListGroup.SetCollapsed(const AValue: Boolean);
begin
  if FCollapsed <> AValue then
  begin
    FCollapsed := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetHeader(const AValue: string);
begin
  if FHeader <> AValue then
  begin
    FHeader := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetFooter(const AValue: string);
begin
  if FFooter <> AValue then
  begin
    FFooter := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetHeaderAlign(const AValue: TAlignment);
begin
  if FHeaderAlign <> AValue then
  begin
    FHeaderAlign := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetFooterAlign(const AValue: TAlignment);
begin
  if FFooterAlign <> AValue then
  begin
    FFooterAlign := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetOptions(const AValue: TdxListGroupOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetSubtitle(const AValue: string);
begin
  if FSubtitle <> AValue then
  begin
    FSubtitle := AValue;
    Changed;
  end;
end;

procedure TdxListGroup.SetTitleImage(const AValue: TImageIndex);
begin
  if FTitleImage <> AValue then
  begin
    FTitleImage := AValue;
    Changed;
  end;
end;

function TdxListGroup.IsCollapsible: Boolean;
begin
  Result := TdxListGroupOption.Collapsible in Options;
end;

function TdxListGroup.IsFocusable: Boolean;
begin
  Result := TdxListGroupOption.Focusable in Options;
end;

function TdxListGroup.IsVisible: Boolean;
begin
  Result := not(TdxListGroupOption.Hidden in Options) and (ItemCount > 0);
end;

procedure TdxListGroup.SelectAll;
begin
  SelectRange(0, ItemCount - 1);
end;

procedure TdxListGroup.SelectRange(AFirstIndex, ALastIndex: Integer);
var
  I: Integer;
  AMultiSelect: Boolean;
begin
  AMultiSelect := ListView.Options.MultiSelect;
  ListView.BeginUpdate;
  try
    ListView.ClearSelection;
    for I := AFirstIndex to ALastIndex do
    begin
      ListView.Controller.SelectItem(ItemIndices[I], True);
      if not AMultiSelect and (ListView.SelectedItemCount = 1) then
        Break;
    end;
  finally
    ListView.EndUpdate;
  end;
end;

procedure TdxListGroup.RemoveItem(AItem: TdxListItem);
begin
  FItemIndices.Remove(AItem.Index);
end;

procedure TdxListGroup.Changed;
var
  AListView: TdxCustomListView;
begin
  AListView := ListView;
  if (AListView <> nil) and not(csLoading in ListView.ComponentState) then
    ListView.UpdateGroups;
end;

procedure TdxListGroup.DettachItems;
var
  I: Integer;
  AItem: TdxListItem;
  AItems: TdxListItems;
begin
  AItems := ListView.Items;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[I];
    if AItem.GroupID = GroupID then
      AItem.GroupID := -1;
  end;
end;

function TdxListGroup.GetDisplayName: string;
begin
  Result := Header;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TdxListGroups }

constructor TdxListGroups.Create(AParentComponent: TComponent;
  AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);
  FListView := TdxCustomListView(AParentComponent);
end;

function TdxListGroups.Add: TdxListGroup;
begin
  Result := TdxListGroup(inherited Add);
end;

function TdxListGroups.FindByHeader(const AHeader: string;
  out AGroup: TdxListGroup): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := SameText(Items[I].Header, AHeader);
    if Result then
    begin
      AGroup := Items[I];
      Break;
    end;
  end;
end;

function TdxListGroups.FindByID(AId: Integer): TdxListGroup;
begin
  Result := TdxListGroup(FindItemByID(AId));
end;

function TdxListGroups.GetFirstVisibleGroup: TdxListGroup;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].IsVisible then
      Exit(Items[I]);
  Result := nil;
end;

function TdxListGroups.GetGroupAtPos(const P: TPoint): TdxListGroup;
var
  AViewInfo: TdxListViewCustomGroupViewInfo;
begin
  if ListView.Options.GroupView and ListView.ViewInfo.GetGroupAtPos(P, AViewInfo)
  then
    Result := AViewInfo.Group
  else
    Result := nil;
end;

function TdxListGroups.GetItemAtPos(const P: TPoint): TdxListItem;
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if ListView.ViewInfo.GetItemAtPos(P, AViewInfo) then
    Result := AViewInfo.Item
  else
    Result := nil;
end;

function TdxListGroups.GetItem(AIndex: Integer): TdxListGroup;
begin
  Result := TdxListGroup(inherited Items[AIndex]);
end;

procedure TdxListGroups.SetItem(AIndex: Integer; AValue: TdxListGroup);
begin
  Items[AIndex].Assign(AValue);
end;

procedure TdxListGroups.SetItemName(AItem: TcxComponentCollectionItem;
  ABaseIndex: Integer);
begin
  AItem.Name := CreateUniqueName(ParentComponent.Owner, ParentComponent, AItem,
    'TdxList', '', Count);
end;

{ TdxListColumn }

constructor TdxListColumn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreatedOrderIndex := UndefinedCreatedOrderIndex;
  FAlignment := taLeftJustify;
  FHeaderAlignment := taLeftJustify;
  FImageIndex := -1;
  FSortOrder := soNone;
  FWidth := 50;
end;

procedure TdxListColumn.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('CreatedOrderIndex', ReadCreatedOrderIndex,
    WriteCreatedOrderIndex, True);
end;

procedure TdxListColumn.Assign(Source: TPersistent);
var
  AColumn: TdxListColumn;
begin
  AColumn := Safe<TdxListColumn>.Cast(Source);
  if AColumn <> nil then
  begin
    Alignment := AColumn.Alignment;
    Caption := AColumn.Caption;
    HeaderAlignment := AColumn.HeaderAlignment;
    ImageIndex := AColumn.ImageIndex;
    MaxWidth := AColumn.MaxWidth;
    MinWidth := AColumn.MinWidth;
    Width := AColumn.Width;
    SortOrder := AColumn.SortOrder;
  end
  else
    inherited Assign(Source);
end;

procedure TdxListColumn.ApplyBestFit;
begin
  ListView.ApplyBestFit(Self);
end;

procedure TdxListColumn.Changed;
begin
  if (Columns.UpdateCount = 0) and ListView.ColumnsShowing then
    ListView.LayoutChanged;
end;

function TdxListColumn.GetCollectionFromParent(AParent: TComponent)
  : TcxComponentCollection;
begin
  Result := (AParent as TdxCustomListView).Columns;
end;

function TdxListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TdxListColumn.GetColumns: TdxListColumns;
begin
  Result := TdxListColumns(Collection);
end;

function TdxListColumn.GetListView: TdxCustomListView;
begin
  if Collection <> nil then
    Result := TdxListColumns(Collection).ListView
  else
    Result := nil;
end;

procedure TdxListColumn.ReadCreatedOrderIndex(AReader: TReader);
begin
  FCreatedOrderIndex := AReader.ReadInteger;
end;

procedure TdxListColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetHeaderAlignment(AValue: TAlignment);
begin
  if FHeaderAlignment <> AValue then
  begin
    FHeaderAlignment := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetMaxWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if MinWidth > 0 then
    AValue := Max(MinWidth, AValue);
  if FMaxWidth <> AValue then
  begin
    FMaxWidth := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetMinWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if MaxWidth > 0 then
    AValue := Min(MaxWidth, AValue);
  if FMinWidth <> AValue then
  begin
    FMinWidth := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetSortOrder(AValue: TdxSortOrder);
begin
  if FSortOrder <> AValue then
  begin
    FSortOrder := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.SetWidth(AValue: Integer);
begin
  if MinWidth <> MaxWidth then
  begin
    AValue := Max(MinWidth, AValue);
    AValue := Min(MaxWidth, AValue);
  end;
  AValue := Max(4, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

procedure TdxListColumn.WriteCreatedOrderIndex(AWriter: TWriter);
begin
  AWriter.WriteInteger(FCreatedOrderIndex);
end;

{ TdxListColumns }

constructor TdxListColumns.Create(AParentComponent: TComponent;
  AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);
  FListView := TdxCustomListView(AParentComponent);
end;

destructor TdxListColumns.Destroy;
begin

  inherited Destroy;
end;

procedure TdxListColumns.BeginUpdate;
begin
  if ListView <> nil then
    ListView.BeginUpdate;
  inherited BeginUpdate;
end;

procedure TdxListColumns.EndUpdate(AForceUpdate: Boolean);
begin
  inherited EndUpdate(AForceUpdate);
  if ListView <> nil then
    ListView.EndUpdate;
end;

function TdxListColumns.Add: TdxListColumn;
begin
  Result := TdxListColumn(inherited Add);
end;

procedure TdxListColumns.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  inherited Notify(AItem, AAction);
  if (AAction in [ccnAdded, ccnExtracted]) and (AItem <> nil) and
    (AItem.ComponentState * [csLoading, csReading, csDestroying] = []) then
  begin
    if AAction = ccnAdded then
      TdxListColumn(AItem).FCreatedOrderIndex := AItem.ID;
    RebuildSubItemIndices;
  end;
end;

procedure TdxListColumns.RebuildSubItemIndices;
var
  AList: TdxFastList;
  I: Integer;
begin
  if Count = 0 then
    Exit;
  AList := TdxFastList.Create(Count);
  try
    for I := 0 to Count - 1 do
      AList.Add(Items[I]);
    AList.SortList(function(Item1, Item2: Pointer): Integer
      begin
        Result := TdxListColumn(Item1).FCreatedOrderIndex - TdxListColumn(Item2)
          .FCreatedOrderIndex;
      end);
    for I := 0 to Count - 1 do
      TdxListColumn(AList[I]).FSubItemIndex := I - 1;
  finally
    AList.Free;
  end;
end;

function TdxListColumns.GetItem(AIndex: Integer): TdxListColumn;
begin
  Result := TdxListColumn(inherited Items[AIndex]);
end;

procedure TdxListColumns.SetItem(AIndex: Integer; AValue: TdxListColumn);
begin
  Items[AIndex].Assign(AValue);
end;

procedure TdxListColumns.SetItemName(AItem: TcxComponentCollectionItem;
ABaseIndex: Integer);
begin
  AItem.Name := CreateUniqueName(ParentComponent.Owner, ParentComponent, AItem,
    'TdxList', '', Count);
end;

procedure TdxListColumns.Update(AItem: TcxComponentCollectionItem;
AAction: TcxComponentCollectionNotification);
begin
  inherited Update(AItem, AAction);
  if (ListView.ComponentState * [csLoading, csReading, csDestroying] = []) then
    ListView.LayoutChanged;
end;

procedure TdxListColumns.ValidateCreateIndices;
var
  AList: TdxFastList;
  AColumn: TdxListColumn;
  I: Integer;
begin
  if Count = 0 then
    Exit;
  AList := TdxFastList.Create(Count);
  try
    for I := 0 to Count - 1 do
    begin
      AColumn := Items[I];
      if AColumn.FCreatedOrderIndex = TdxListColumn.UndefinedCreatedOrderIndex
      then
        AColumn.FCreatedOrderIndex := AColumn.ID;
      AList.Add(AColumn);
    end;
    AList.SortList(function(Item1, Item2: Pointer): Integer
      begin
        Result := TdxListColumn(Item1).FCreatedOrderIndex - TdxListColumn(Item2)
          .FCreatedOrderIndex;
      end);
    for I := 0 to Count - 1 do
      TdxListColumn(AList[I]).FCreatedOrderIndex := I;
  finally
    AList.Free;
  end;
  RebuildSubItemIndices;
end;

{ TdxListViewCustomViewInfo }

constructor TdxListViewCustomViewInfo.Create(AListView: TdxCustomListView);
begin
  FListView := AListView;
end;

procedure TdxListViewCustomViewInfo.Calculate(AType: TdxChangeType;
const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxListViewCustomViewInfo.Draw(ACanvas: TcxCustomCanvas);
begin
  if ACanvas.RectVisible(Bounds) then
  begin
    BeforeDraw;
    DrawContent(ACanvas);
  end;
end;

procedure TdxListViewCustomViewInfo.BeforeDraw;
begin
end;

function TdxListViewCustomViewInfo.GetItemsGap: Integer;
begin
  Result := Options.Icons.ItemsGap;
end;

function TdxListViewCustomViewInfo.IsGroupView: Boolean;
begin
  Result := Options.GroupView;
end;

function TdxListViewCustomViewInfo.IsReportView: Boolean;
begin
  Result := ListView.ViewStyle = TdxListViewStyle.Report;
end;

procedure TdxListViewCustomViewInfo.MouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
end;

procedure TdxListViewCustomViewInfo.MouseLeave;
begin
end;

procedure TdxListViewCustomViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
begin
end;

procedure TdxListViewCustomViewInfo.MouseUp(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
end;

procedure TdxListViewCustomViewInfo.Invalidate;
begin
  ListView.InvalidateRect(Bounds, True);
end;

function TdxListViewCustomViewInfo.GetController: TdxListViewController;
begin
  Result := ListView.Controller;
end;

function TdxListViewCustomViewInfo.GetExplorerStyle: Boolean;
begin
  Result := ListView.Options.ExplorerStyle;
end;

function TdxListViewCustomViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxListViewCustomViewInfo.GetHintBounds: TRect;
begin
  Result.Empty;
end;

function TdxListViewCustomViewInfo.GetLookAndFeelPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := ListView.LookAndFeelPainter;
end;

function TdxListViewCustomViewInfo.GetOptions: TdxListViewOptions;
begin
  Result := ListView.Options;
end;

function TdxListViewCustomViewInfo.GetPainter: TdxListViewPainter;
begin
  Result := ListView.Painter;
end;

function TdxListViewCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := ListView.ScaleFactor;
end;

function TdxListViewCustomViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := ListView.UseRightToLeftAlignment;
end;

{ TdxListItemsEnumerator }

constructor TdxListItemsEnumerator.Create(AListItems: TdxListItems);
begin
  inherited Create;
  FIndex := -1;
  FListItems := AListItems;
end;

function TdxListItemsEnumerator.GetCurrent: TdxListItem;
begin
  Result := FListItems[FIndex];
end;

function TdxListItemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FListItems.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TdxListItems }

type
  TItemDataInfo = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Tag: Int64;
    CaptionLength: Integer;
  end;

  TSubItemDataInfo = record
    ImageIndex: Integer;
    CaptionLength: Integer;
  end;

constructor TdxListItems.Create(AListView: TdxCustomListView);
begin
  inherited Create;
  FListView := AListView;
  FList := TdxFastObjectList.Create(True, 1024);
end;

destructor TdxListItems.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TdxListItems.Add: TdxListItem;
begin
  Result := AddItem(nil, -1);
end;

function TdxListItems.Insert(AIndex: Integer): TdxListItem;
begin
  Result := AddItem(nil, AIndex);
end;

function TdxListItems.AddItem(AItem: TdxListItem; AIndex: Integer): TdxListItem;
begin
  if AItem = nil then
    Result := ListView.CreateListItem
  else
    Result := AItem;
  if AIndex < 0 then
    AIndex := Count;
  FList.Insert(AIndex, Result);
  ListView.InsertItem(AItem);
  Changed;
end;

function TdxListItems.GetItemAtPos(const P: TPoint): TdxListItem;
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if ListView.ViewInfo.GetItemAtPos(P, AViewInfo) then
    Result := AViewInfo.Item
  else
    Result := nil;
end;

function TdxListItems.GetCount: Integer;
begin
  if ListView.OwnerData then
    Result := FCount
  else
    Result := FList.Count;
end;

function TdxListItems.GetEnumerator: TdxListItemsEnumerator;
begin
  Result := TdxListItemsEnumerator.Create(Self);
end;

function TdxListItems.GetItem(AIndex: Integer): TdxListItem;
begin
  if ListView.OwnerData then
    Result := ListView.GetItem(AIndex)
  else
    Result := TdxListItem(FList[AIndex]);
end;

function TdxListItems.IndexOf(AValue: TdxListItem): Integer;
begin
  Result := FList.IndexOf(AValue);
end;

procedure TdxListItems.SetCount(AValue: Integer);
begin
  if ListView.OwnerData then
  begin
    FCount := EnsureRange(AValue, 0, MaxInt);
    Clear;
  end
  else
    FCount := 0;
end;

procedure TdxListItems.SetItem(AIndex: Integer; AValue: TdxListItem);
begin
  Items[AIndex].Assign(AValue);
end;

procedure TdxListItems.Clear;
begin
  BeginUpdate;
  try
    FList.Clear;
    ListView.ResetContent;
  finally
    EndUpdate;
  end;
end;

procedure TdxListItems.BeginUpdate;
begin
  ListView.BeginUpdate;
end;

procedure TdxListItems.SetUpdateState(AUpdating: Boolean);
begin
end;

procedure TdxListItems.EndUpdate;
begin
  ListView.EndUpdate;
end;

procedure TdxListItems.Assign(ASource: TPersistent);
var
  Items: TdxListItems;
  I: Integer;
begin
  if ASource is TdxListItems then
  begin
    Clear;
    Items := TdxListItems(ASource);
    for I := 0 to Items.Count - 1 do
      Add.Assign(Items[I]);
  end
  else
    inherited Assign(ASource);
end;

procedure TdxListItems.Changed;
begin
  // dxAbstractError;
end;

procedure TdxListItems.DefineProperties(AFiler: TFiler);

  function WriteItems: Boolean;
  var
    I: Integer;
    AItems: TdxListItems;
  begin
    AItems := TdxListItems(AFiler.Ancestor);
    if AItems = nil then
      Result := Count > 0
    else if AItems.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Items[I].IsEqual(AItems[I]);
        if Result then
          Break;
      end
    end;
  end;

begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('ItemData', ReadItemData, WriteItemData,
    WriteItems);
end;

procedure TdxListItems.FixupGroups;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FixupGroup;
end;

const
  dxListItemStreamVersion = $1;

procedure TdxListItems.ReadItemData(AStream: TStream);
var
  AItemInfo: TItemDataInfo;
  ASubItemInfo: TSubItemDataInfo;
  I, J, AItemCount: Integer;
  AStreamVersion: Byte;
  ACaption: string;
begin
  BeginUpdate;
  try
    Clear;
    if AStream.Size = 0 then
      Exit;

    AStream.ReadBuffer(AStreamVersion, SizeOf(AStreamVersion));
    case AStreamVersion of
      dxListItemStreamVersion:
        begin
          AStream.ReadBuffer(AItemCount, SizeOf(Integer));
          for I := 0 to AItemCount - 1 do
          begin
            AStream.ReadBuffer(AItemInfo, SizeOf(TItemDataInfo));
            with Add do
            begin
              ImageIndex := AItemInfo.ImageIndex;
              OverlayIndex := AItemInfo.OverlayIndex;
              StateIndex := AItemInfo.StateIndex;
              Tag := AItemInfo.Tag;
              FLoadingGroupID := AItemInfo.GroupID;
              SetLength(ACaption, AItemInfo.CaptionLength);
              AStream.ReadBuffer(ACaption[1], AItemInfo.CaptionLength *
                SizeOf(Char));
              Caption := ACaption;
              for J := 0 to AItemInfo.SubItemCount - 1 do
              begin
                AStream.ReadBuffer(ASubItemInfo, SizeOf(TSubItemDataInfo));
                SetLength(ACaption, ASubItemInfo.CaptionLength);
                AStream.ReadBuffer(ACaption[1], ASubItemInfo.CaptionLength *
                  SizeOf(Char));
                SubItems.Add(ACaption);
                SubItemImages[J] := ASubItemInfo.ImageIndex;
              end;
            end;
          end;
        end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxListItems.WriteItemData(AStream: TStream);
var
  AItemInfo: TItemDataInfo;
  ASubItemInfo: TSubItemDataInfo;
  ACaption: string;
  AStreamVersion: Byte;
  I, J, AItemCount: Integer;
begin
  AItemCount := Count;
  AStreamVersion := dxListItemStreamVersion;
  AStream.WriteBuffer(AStreamVersion, SizeOf(Byte));
  AStream.WriteBuffer(AItemCount, SizeOf(Integer));
  for I := 0 to Count - 1 do
  begin
    with Items[I] do
    begin
      AItemInfo.ImageIndex := ImageIndex;
      AItemInfo.OverlayIndex := OverlayIndex;
      AItemInfo.StateIndex := StateIndex;
      AItemInfo.Tag := Tag;
      AItemInfo.GroupID := GroupID;
      AItemInfo.SubItemCount := SubItems.Count;
      ACaption := Caption;
      AItemInfo.CaptionLength := Length(ACaption);
      AStream.WriteBuffer(AItemInfo, SizeOf(TItemDataInfo));
      AStream.WriteBuffer(ACaption[1], AItemInfo.CaptionLength * SizeOf(Char));
      for J := 0 to SubItems.Count - 1 do
      begin
        ASubItemInfo.ImageIndex := SubItemImages[J];
        ACaption := SubItems[J];
        ASubItemInfo.CaptionLength := Length(ACaption);
        AStream.WriteBuffer(ASubItemInfo, SizeOf(TSubItemDataInfo));
        AStream.WriteBuffer(ACaption[1], ASubItemInfo.CaptionLength *
          SizeOf(Char));
      end;
    end;
  end;
end;

procedure TdxListItems.Delete(AIndex: Integer);
begin
  Items[AIndex].Delete;
end;

type
  TdxSubItems = class(TStringList)
  private
    FOwner: TdxListItem;
    FImageIndices: TList;
    procedure RefreshItem(Index: Integer);
    function GetImageIndex(Index: Integer): TImageIndex;
    procedure SetImageIndex(Index: Integer; const Value: TImageIndex);
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AOwner: TdxListItem);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    property Owner: TdxListItem read FOwner;
    property ImageIndex[Index: Integer]: TImageIndex read GetImageIndex
      write SetImageIndex;
  end;

constructor TdxSubItems.Create(AOwner: TdxListItem);
begin
  inherited Create;
  FOwner := AOwner;
  FImageIndices := TList.Create;
end;

destructor TdxSubItems.Destroy;
begin
  FImageIndices.Free;
  inherited Destroy;
end;

function TdxSubItems.Add(const S: string): Integer;
begin
  Result := inherited Add(S);
  FImageIndices.Add(Pointer(-1));
  RefreshItem(Result + 1);
end;

function TdxSubItems.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
  FImageIndices.Add(Pointer(-1));
  RefreshItem(Result + 1);
end;

procedure TdxSubItems.Clear;
begin
  inherited;
  FImageIndices.Clear;
end;

procedure TdxSubItems.Delete(Index: Integer);
begin
  inherited;
  FImageIndices.Delete(Index);
  Owner.Changed(ctMedium);
end;

procedure TdxSubItems.Insert(Index: Integer; const S: string);
var
  I: Integer;
begin
  inherited Insert(Index, S);
  FImageIndices.Insert(Index, Pointer(-1));
  for I := Index + 1 to Count do
    RefreshItem(I);
end;

procedure TdxSubItems.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  RefreshItem(Index + 1);
end;

procedure TdxSubItems.RefreshItem(Index: Integer);
begin
end;

procedure TdxSubItems.SetUpdateState(Updating: Boolean);
begin
end;

function TdxSubItems.GetImageIndex(Index: Integer): TImageIndex;
begin
  Result := TImageIndex(FImageIndices[Index]);
end;

procedure TdxSubItems.SetImageIndex(Index: Integer; const Value: TImageIndex);
begin
  FImageIndices[Index] := Pointer(Value);
end;

{ TdxListItem }

constructor TdxListItem.Create(AOwner: TdxListItems);
begin
  inherited Create;
  FOwner := AOwner;
  FSubItems := TdxSubItems.Create(Self);
  FEnabled := True;
  FImageIndex := -1;
  FOverlayIndex := -1;
  FStateIndex := -1;
end;

destructor TdxListItem.Destroy;
begin
  if not ListView.IsDestroying then
    Group := nil;
  FSubItems.Free;
  inherited Destroy;
end;

procedure TdxListItem.Assign(Source: TPersistent);
var
  AItem: TdxListItem;
begin
  AItem := Safe<TdxListItem>.Cast(Source);
  if AItem <> nil then
  begin
    Caption := AItem.Caption;
    Data := AItem.Data;
    ImageIndex := AItem.ImageIndex;
    Cut := AItem.Cut;
    Indent := AItem.Indent;
    OverlayIndex := AItem.OverlayIndex;
    StateIndex := AItem.StateIndex;
    SubItems := AItem.SubItems;
    Checked := AItem.Checked;
    GroupID := AItem.GroupID;
    Hint := AItem.Hint;
    Tag := AItem.Tag;
  end
  else
    inherited Assign(Source);
end;

procedure TdxListItem.BeforeDestruction;
begin
  FDeleting := True;
  ListView.DeleteItem(Self);
end;

function TdxListItem.GetListView: TdxCustomListView;
begin
  Result := Owner.ListView;
end;

procedure TdxListItem.Delete;
begin
  if not FDeleting and (Self <> ListView.TempItem) then
    Free;
end;

procedure TdxListItem.MakeVisible(PartialOK: Boolean);
const
  VisibleTypeMap: array [Boolean] of TdxVisibilityType = (vtFully, vtPartialy);
begin
  ListView.MakeItemVisible(Self, VisibleTypeMap[PartialOK]);
end;

function TdxListItem.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TdxListItem.GetFocused: Boolean;
begin
  Result := ListView.Controller.FocusedItemIndex = Index;
end;

function TdxListItem.GetLeft: Integer;
begin
  Result := GetPosition.X;
end;

function TdxListItem.GetGroupID: Integer;
begin
  if FGroup = nil then
    Result := -1
  else
    Result := FGroup.GroupID;
end;

function TdxListItem.GetSelected: Boolean;
begin
  Result := ListView.IsItemSelected(Self);
end;

function TdxListItem.GetTop: Integer;
begin
  Result := GetPosition.Y;
end;

procedure TdxListItem.SetChecked(AValue: Boolean);
begin
  if AValue <> Checked then
  begin
    FChecked := AValue;
    Changed(ctLight);
  end;
end;

procedure TdxListItem.SetGroup(AValue: TdxListGroup);
begin
  if AValue <> FGroup then
  begin
    if FGroup <> nil then
      FGroup.RemoveItem(Self);
    FGroup := AValue;
    if FGroup <> nil then
      FGroup.AddItem(Self);
    ListView.UpdateGroups;
  end;
end;

procedure TdxListItem.SetGroupID(AValue: Integer);
begin
  Group := ListView.Groups.FindByID(AValue);
end;

procedure TdxListItem.SetLeft(AValue: Integer);
begin
  SetPosition(Point(AValue, Top));
end;

procedure TdxListItem.SetSelected(AValue: Boolean);
begin
  ListView.Controller.SelectItem(Index, AValue);
end;

procedure TdxListItem.SetTop(AValue: Integer);
begin
  SetPosition(Point(Left, AValue));
end;

procedure TdxListItem.SetCaption(const AValue: string);
begin
  if AValue <> Caption then
  begin
    FCaption := AValue;
    if ListView.TempItem = Self then
      Exit;
    if ListView.Options.SortType in [TdxListViewSortType.Both,
      TdxListViewSortType.Text] then
      ListView.AlphaSort
    else
      ListView.LayoutChanged; // UpdateItems(Index, Index);
  end;
end;

procedure TdxListItem.SetCut(AValue: Boolean);
begin
  if FCut <> AValue then
  begin
    FCut := AValue;
    Changed(ctLight);
  end;
end;

procedure TdxListItem.SetData(AValue: TCustomData);
begin
  if AValue <> Data then
  begin
    FData := AValue;
    if ListView.TempItem = Self then
      Exit;
    if ListView.Options.SortType in [TdxListViewSortType.Both,
      TdxListViewSortType.Data] then
      ListView.AlphaSort;
  end;
end;

procedure TdxListItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if ListView.TempItem = Self then
      Exit;
    Changed(ctLight);
  end;
end;

procedure TdxListItem.SetFocused(AValue: Boolean);
begin
  if AValue then
    ListView.Controller.FocusedItemIndex := Index
  else if Focused then
    ListView.Controller.FocusedItemIndex := -1;
end;

function TdxListItem.EditCaption: Boolean;
begin
  Result := ListView.StartItemCaptionEditing(Index);
end;

procedure TdxListItem.CancelEdit;
begin
  ListView.FinishItemCaptionEditing;
end;

procedure TdxListItem.SetImage(AIndex: Integer; AValue: TImageIndex);
var
  AChanged: Boolean;
begin
  AChanged := False;
  case AIndex of
    0:
      if AValue <> FImageIndex then
      begin
        AChanged := True;
        FImageIndex := AValue;
      end;
    1:
      if AValue <> FOverlayIndex then
      begin
        AChanged := True;
        FOverlayIndex := AValue;
      end;
    2:
      if AValue <> FStateIndex then
      begin
        AChanged := True;
        FStateIndex := AValue;
      end;
  end;
  if AChanged and not Owner.ListView.OwnerData then
    Changed(ctLight);
end;

procedure TdxListItem.SetIndent(AValue: Integer);
begin
  if FIndent <> AValue then
  begin
    FIndent := AValue;
    if not Owner.ListView.OwnerData then
    begin
      Changed;
    end;
  end;
end;

procedure TdxListItem.Changed(AType: TdxChangeType = ctHard);
begin
  if Self <> ListView.TempItem then
    ListView.LayoutChanged(AType);
end;

procedure TdxListItem.FixupGroup;
begin
  GroupID := FLoadingGroupID;
end;

function TdxListItem.IsEnabled: Boolean;
begin
  Result := not ListView.SupportsItemEnabledState or Enabled;
end;

function TdxListItem.IsEqual(AItem: TdxListItem): Boolean;
begin
  Result := (Caption = AItem.Caption) and (Data = AItem.Data);
end;

procedure TdxListItem.SetSubItems(AValue: TStrings);
begin
  if AValue <> nil then
    FSubItems.Assign(AValue);
end;

function TdxListItem.GetIndex: Integer;
begin
  if Owner.ListView.OwnerData then
    Result := FIndex
  else
    Result := Owner.IndexOf(Self);
end;

function TdxListItem.GetPosition: TPoint;
begin
  Result := ListView.ViewInfo.GetBoundsForItem(Self).Location;
end;

procedure TdxListItem.SetPosition(const Value: TPoint);
var
  LAt: TPoint;
begin
  LAt := Position;
  if (LAt.X <> Value.X) or (LAt.Y <> Value.Y) then
    if ListView.ViewStyle in [TdxListViewStyle.SmallIcon, TdxListViewStyle.Icon]
    then
      dxAbstractError;
end;

function TdxListItem.DisplayRect(Code: TDisplayCode): TRect;
begin
  Result := ListView.ViewInfo.GetBoundsForItem(Self);
end;

function TdxListItem.GetSubItemImage(AIndex: Integer): Integer;
begin
  Result := TdxSubItems(FSubItems).ImageIndex[AIndex];
end;

procedure TdxListItem.SetSubItemImage(AIndex: Integer; const AValue: Integer);
begin
  if TdxSubItems(FSubItems).ImageIndex[AIndex] <> AValue then
  begin
    TdxSubItems(FSubItems).ImageIndex[AIndex] := AValue;
    Changed
  end;
end;

{ TdxListViewCellViewInfo }

constructor TdxListViewCellViewInfo.Create(AListView: TdxCustomListView;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AListView);
  FViewParams := AViewParams;
  FIsDirty := True;
end;

destructor TdxListViewCellViewInfo.Destroy;
begin
  FTextLayout.Free;
  inherited Destroy;
end;

procedure TdxListViewCellViewInfo.Calculate(AType: TdxChangeType;
const ABounds: TRect);
begin
  FBounds := ABounds;
  DoCalculate;
  if ViewParams.RightToLeftAlignmet then
    DoRightToLeftAlignmet;
  FIsDirty := False;
end;

procedure TdxListViewCellViewInfo.CalculateContentBounds;
begin
  FContentBounds := Bounds;
  FContentBounds.Deflate(FViewParams.Padding);
end;

procedure TdxListViewCellViewInfo.CalculateGlyphsAreaBounds;
begin
  FGlyphsAreaBounds := CalculateBoundsCore(ContentBounds, Bounds,
    ViewParams.GlyphsAreaSize, OppositePositionMap[ViewParams.TextPosition]);
end;

function TdxListViewCellViewInfo.CalculateBoundsCore(const ABounds,
  AFullBounds: TRect; const ASize: TSize; APosition: TcxPosition): TRect;
var
  AAnchor: TPoint;
begin
  if ABounds.IsZero then
  begin
    Result.Empty;
    Exit;
  end;
  AAnchor.Init(ABounds.Left + (ABounds.Width - ASize.cx) div 2,
    ABounds.Top + (ABounds.Height - ASize.cy) div 2);
  case APosition of
    posNone:
      Result := cxRectCenter(AFullBounds, ASize);
    posLeft:
      begin
        Result.Init(ABounds.Left, AAnchor.Y, ABounds.Left + ASize.cx,
          AAnchor.Y + ASize.cy);
      end;
    posRight:
      begin
        Result.Init(ABounds.Right - ASize.cx, AAnchor.Y, ABounds.Right,
          AAnchor.Y + ASize.cy);
      end;
    posTop:
      begin
        Result.Init(AAnchor.X, ABounds.Top, AAnchor.X + ASize.cx,
          ABounds.Top + ASize.cy);
      end;
    posBottom:
      begin
        Result.Init(AAnchor.X, ABounds.Bottom - ASize.cy, AAnchor.X + ASize.cx,
          ABounds.Bottom);
      end;
  end;
end;

procedure TdxListViewCellViewInfo.CalculateGlyphBounds(const ABounds: TRect);
begin
  if HasGlyph then
    FGlyphBounds := cxRectCenter(ABounds, ViewParams.GlyphSize)
  else
    FGlyphBounds.Empty;
end;

procedure TdxListViewCellViewInfo.CalculateGlyphsBounds(const ABounds: TRect);
begin
  CalculateGlyphBounds(ABounds);
end;

procedure TdxListViewCellViewInfo.CalculateTextAndGlyphLayout;
begin
  CalculateGlyphsBounds(FGlyphsAreaBounds);
  CalculateTextAreaBounds(FContentBounds);
end;

procedure TdxListViewCellViewInfo.CalculateTextBounds;
var
  ATextBox: TRect;
  ATextFlags: Integer;
begin
  if TextLayout <> nil then
  begin
    FTextLayout.SetLayoutConstraints(TextAreaBounds.Width, 0,
      ViewParams.TextLineCount);
    FTextSize := TextLayout.MeasureSize;
    ATextBox.InitSize(TextBounds.Left, TextBounds.Top, FTextSize);
    ATextFlags := GetTextFlags;
    if (CXTO_CENTER_HORIZONTALLY and ATextFlags) <> 0 then
      ATextBox.Offset((TextBounds.Width - FTextSize.cx) div 2, 0)
    else if (CXTO_RIGHT and ATextFlags) <> 0 then
      ATextBox.Offset(TextBounds.Width - FTextSize.cx, 0);
    if (CXTO_CENTER_VERTICALLY and ATextFlags) <> 0 then
      ATextBox.Offset(0, (TextBounds.Height - FTextSize.cy) div 2);
    ATextBox.Inflate(1, 0);
    FTextBounds := ATextBox;
  end;
end;

function TdxListViewCellViewInfo.HasGlyph: Boolean;
begin
  Result := not ViewParams.GlyphSize.IsZero;
end;

procedure TdxListViewCellViewInfo.UpdateBounds;
begin
  if (TextLayout <> nil) and (ViewParams.TextPosition = posBottom) then
  begin
    FContentBounds.Bottom := FTextBounds.Bottom;
    FBounds.Bottom := Min(FBounds.Bottom, FTextBounds.Bottom +
      ViewParams.Padding.Bottom);
    FTextAreaBounds.Intersect(FContentBounds);
  end;
end;

procedure TdxListViewCellViewInfo.CalculateTextAreaBounds(const ABounds: TRect);
begin
  if HasGlyph then
  begin
    case ViewParams.TextPosition of
      posNone:
        FTextAreaBounds.Empty;
      posLeft:
        begin
          FTextAreaBounds.Init(ABounds.Left, ABounds.Top,
            ABounds.Right - (FGlyphsAreaBounds.Width + ViewParams.GlyphIndent),
            ABounds.Bottom);
        end;
      posRight:
        begin
          FTextAreaBounds.Init(ABounds.Left + FGlyphsAreaBounds.Width +
            ViewParams.GlyphIndent, ABounds.Top, ABounds.Right, ABounds.Bottom);
        end;
      posTop:
        begin
          FTextAreaBounds.Init(ABounds.Left, ABounds.Top, ABounds.Right,
            ABounds.Bottom - (FGlyphsAreaBounds.Height +
            ViewParams.GlyphIndent));
        end;
      posBottom:
        begin
          FTextAreaBounds.Init(ABounds.Left,
            ABounds.Top + FGlyphsAreaBounds.Height + ViewParams.GlyphIndent,
            ABounds.Right, ABounds.Bottom);
        end;
    end;
  end
  else
    FTextAreaBounds := ABounds;
  FTextBounds := TextAreaBounds;
end;

procedure TdxListViewCellViewInfo.DoCalculate;
begin
  CalculateContentBounds;
  CalculateGlyphsAreaBounds;
  CalculateTextAndGlyphLayout;
  CalculateTextBounds;
  UpdateBounds;
end;

procedure TdxListViewCellViewInfo.DoRightToLeftAlignmet;
begin
  FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FContentBounds, Bounds);
  FGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FGlyphBounds, Bounds);
  FTextAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FTextAreaBounds, Bounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, Bounds);
  FGlyphsAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FGlyphsAreaBounds, Bounds);
end;

procedure TdxListViewCellViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  DrawGlyph(ACanvas);
  DrawText(ACanvas);
end;

procedure TdxListViewCellViewInfo.DrawGlyphCore(ACanvas: TcxCustomCanvas;
const ABounds: TRect; AImages: TCustomImageList;
AImageIndex, AOverlayIndex: Integer; AMode: TcxImageDrawMode);
begin
  if AImages <> nil then
    Painter.DrawGlyphCore(ACanvas, ABounds, AImages, AImageIndex,
      AOverlayIndex, AMode);
end;

procedure TdxListViewCellViewInfo.DrawGlyph(ACanvas: TcxCustomCanvas);
begin
  DrawGlyphCore(ACanvas, GlyphBounds, ViewParams.Images, FImageIndex, -1,
    GetGlyphState);
end;

procedure TdxListViewCellViewInfo.DrawText(ACanvas: TcxCustomCanvas);
begin
  if TextLayout <> nil then
  begin
    TextLayout.SetColor(GetTextColor);
    TextLayout.Draw(TextBounds);
  end;
end;

function TdxListViewCellViewInfo.GetGlyphState: TcxImageDrawMode;
begin
  Result := idmNormal;
end;

function TdxListViewCellViewInfo.GetTextFlags: Integer;
begin
  Result := ViewParams.TextFlags;
end;

function TdxListViewCellViewInfo.GetOrigin: TPoint;
begin
  Result := FBounds.TopLeft;
end;

procedure TdxListViewCellViewInfo.Initialize(const AText: string;
AImageIndex: Integer);
begin
  if AText = '' then
    FreeAndNil(FTextLayout)
  else
  begin
    if FTextLayout = nil then
      FTextLayout := Painter.CreateCanvasBasedTextLayout;
    FTextLayout.SetText(AText);
    FTextLayout.SetFlags(GetTextFlags);
    FTextLayout.SetFont(ViewParams.Font);
  end;
  if not IsImageAssigned(ViewParams.Images, AImageIndex) then
    AImageIndex := -1;
  FImageIndex := AImageIndex;
  MakeDirty;
end;

procedure TdxListViewCellViewInfo.MakeDirty;
begin
  FIsDirty := True;
end;

procedure TdxListViewCellViewInfo.Offset(ADX, ADY: Integer);
begin
  FBounds.Offset(ADX, ADY);
  FContentBounds.Offset(ADX, ADY);
  FGlyphsAreaBounds.Offset(ADX, ADY);
  FGlyphBounds.Offset(ADX, ADY);
  FTextAreaBounds.Offset(ADX, ADY);
  FTextBounds.Offset(ADX, ADY);
end;

procedure TdxListViewCellViewInfo.MouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
end;

procedure TdxListViewCellViewInfo.MouseLeave;
begin
  Invalidate;
end;

procedure TdxListViewCellViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
begin
end;

procedure TdxListViewCellViewInfo.MouseUp(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
end;

procedure TdxListViewCellViewInfo.SetOrigin(const P: TPoint);
begin
  Offset(P.X - FBounds.Left, P.Y - FBounds.Top);
end;

{ TdxListItemCustomViewInfo }

constructor TdxListItemCustomViewInfo.Create
  (AOwner: TdxListViewCustomGroupViewInfo; const AItemIndex: Integer;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AOwner.ListView, AViewParams);
  FOwner := AOwner;
  FItemIndex := AItemIndex;
end;

function TdxListItemCustomViewInfo.GetGlyphState: TcxImageDrawMode;
begin
  if CachedItem.Cut then
    Result := idmDingy
  else
    Result := idmNormal;
end;

function TdxListItemCustomViewInfo.GetItem: TdxListItem;
begin
  Result := ListView.GetItem(FItemIndex);
end;

procedure TdxListItemCustomViewInfo.BeforeDraw;
begin
  inherited BeforeDraw;
  FCachedItem := GetItem;
  FState := GetState;
end;

procedure TdxListItemCustomViewInfo.CalculateCheckGlyphBounds;
var
  R: TRect;
  ASize: TSize;
begin
  if ViewParams.StateViewKind = TdxListItemStateViewKind.None then
    FStateGlyphBounds.Empty
  else
  begin
    ASize.Init(ViewParams.GlyphSize.cx + ViewParams.StateGlyphSize.cx +
      ViewParams.GlyphIndent, Max(ViewParams.GlyphSize.cy,
      ViewParams.StateGlyphSize.cy));
    R := cxRectCenter(GlyphsAreaBounds, ASize);
    FStateGlyphBounds := CalculateBoundsCore(R, GlyphsAreaBounds,
      ViewParams.StateGlyphSize, posLeft);
  end;
end;

procedure TdxListItemCustomViewInfo.CalculateGlyphsBounds(const ABounds: TRect);
var
  R: TRect;
begin
  if ViewParams.StateViewKind = TdxListItemStateViewKind.None then
    inherited CalculateGlyphsBounds(ABounds)
  else
  begin
    CalculateCheckGlyphBounds;
    R := ABounds;
    R.Left := FStateGlyphBounds.Right + ViewParams.GlyphIndent;
    FGlyphBounds := CalculateBoundsCore(R, GlyphsAreaBounds,
      ViewParams.GlyphSize, posLeft);
  end;
end;

procedure TdxListItemCustomViewInfo.DoRightToLeftAlignmet;
begin
  inherited DoRightToLeftAlignmet;
  FStateAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FStateAreaBounds, Bounds);
  FStateGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FStateGlyphBounds, Bounds);
end;

procedure TdxListItemCustomViewInfo.DrawBackground(ACanvas: TcxCustomCanvas);
begin
  Painter.DrawItemBackground(ACanvas, Self);
end;

procedure TdxListItemCustomViewInfo.DrawStateGlyph(ACanvas: TcxCustomCanvas);
const
  CheckBoxMap: array [Boolean] of TcxButtonState = (cxbsDisabled, cxbsNormal);
var
  AGlyphState: TcxButtonState;
begin
  case ViewParams.StateViewKind of
    TdxListItemStateViewKind.CheckBox:
      begin
        if State * [dxlisHot, dxlisSelected] <> [] then
        begin
          if FHotTrackPart = TdxListItemPart.StateGlyph then
            AGlyphState := cxbsHot
          else
            AGlyphState := CheckBoxMap[CachedItem.IsEnabled];
          Painter.DrawCheckButton(ACanvas, StateGlyphBounds, AGlyphState,
            CachedItem.Selected);
        end;
      end;
    TdxListItemStateViewKind.Glyph:
      DrawGlyphCore(ACanvas, StateGlyphBounds, ViewParams.StateImages,
        CachedItem.StateIndex, -1, idmNormal);
  end;
end;

procedure TdxListItemCustomViewInfo.DrawText(ACanvas: TcxCustomCanvas);
begin
  if ListView.IsEditingItem(ItemIndex) then
    Exit;
  inherited DrawText(ACanvas);
end;

function TdxListItemCustomViewInfo.GetState: TdxListViewItemStates;
begin
  Result := [];
  if not ListView.IsActive then
    Include(Result, dxlisInactive);
  if CachedItem.Focused and not ListView.IsEditingItem(ItemIndex) then
    Include(Result, dxlisFocused);
  if CachedItem.Selected then
    Include(Result, dxlisSelected);
  if IsHovered then
    Include(Result, dxlisHot);
end;

function TdxListItemCustomViewInfo.GetTextColor: TColor;
begin
  Result := ListView.Fonts.Item.Color;
  if Result = clDefault then
    Result := ListView.LookAndFeel.Painter.GetListViewItemTextColor(State,
      ExplorerStyle);
end;

procedure TdxListItemCustomViewInfo.Initialize(AItem: TdxListItem);
begin
  Initialize(AItem.Caption, AItem.ImageIndex);
end;

function TdxListItemCustomViewInfo.IsHovered: Boolean;
begin
  Result := ListView.Controller.MouseHoveredItemIndex = FItemIndex;
end;

procedure TdxListItemCustomViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  DrawBackground(ACanvas);
  inherited DrawContent(ACanvas);
  DrawStateGlyph(ACanvas);
end;

procedure TdxListItemCustomViewInfo.DrawGlyph(ACanvas: TcxCustomCanvas);
begin
  DrawGlyphCore(ACanvas, GlyphBounds, ViewParams.Images, CachedItem.ImageIndex,
    CachedItem.OverlayIndex, GetGlyphState);
end;

function TdxListItemCustomViewInfo.GetPart(const P: TPoint): TdxListItemPart;
begin
  if ContentBounds.Contains(P) then
  begin
    if TextBounds.Contains(P) then
      Result := TdxListItemPart.Text
    else if GlyphBounds.Contains(P) then
      Result := TdxListItemPart.Glyph
    else if StateGlyphBounds.Contains(P) then
      Result := TdxListItemPart.StateGlyph
    else
      Result := TdxListItemPart.Content;
  end
  else
    Result := TdxListItemPart.None
end;

procedure TdxListItemCustomViewInfo.Offset(ADX, ADY: Integer);
begin
  inherited Offset(ADX, ADY);
  FStateAreaBounds.Offset(ADX, ADY);
  FStateGlyphBounds.Offset(ADX, ADY);
end;

procedure TdxListItemCustomViewInfo.InvalidatePart(APart: TdxListItemPart);
begin
  case APart of
    TdxListItemPart.StateGlyph:
      ListView.InvalidateRect(StateGlyphBounds, True);
  end;
end;

procedure TdxListItemCustomViewInfo.MouseLeave;
begin
  HotTrackPart := TdxListItemPart.None;
end;

procedure TdxListItemCustomViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
begin
  HotTrackPart := GetPart(AMousePos);
end;

function TdxListItemCustomViewInfo.StartDrag(AShift: TShiftState;
const AMousePos: TPoint): Boolean;
begin
  Result := GetPart(AMousePos) in [TdxListItemPart.Text, TdxListItemPart.Glyph];
end;

function TdxListItemCustomViewInfo.StartEdit(AShift: TShiftState;
const AMousePos: TPoint): Boolean;
begin
  Result := TextBounds.Contains(AMousePos);
end;

function TdxListItemCustomViewInfo.StartMultiSelection(AShift: TShiftState;
const AMousePos: TPoint): Boolean;
begin
  Result := not(TextBounds.Contains(AMousePos) or
    GlyphsAreaBounds.Contains(AMousePos) or Controller.IsItemSelected
    (ItemIndex));
end;

procedure TdxListItemCustomViewInfo.SetHotTrackPart(AValue: TdxListItemPart);
begin
  if FHotTrackPart <> AValue then
  begin
    InvalidatePart(FHotTrackPart);
    InvalidatePart(AValue);
    FHotTrackPart := AValue;
  end;
end;

{ TdxListSubItemViewInfo }

procedure TdxListSubItemViewInfo.CalculateGlyphsAreaBounds;
var
  ASize: TSize;
begin
  ASize := ViewParams.GlyphsAreaSize;
  ASize.cx := ViewParams.GlyphSize.cx;
  FGlyphsAreaBounds := CalculateBoundsCore(ContentBounds, Bounds, ASize,
    OppositePositionMap[ViewParams.TextPosition]);
end;

constructor TdxListSubItemViewInfo.Create
  (AOwner: TdxListItemReportStyleViewInfo;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AOwner.ListView, AViewParams);
  FOwner := AOwner;
end;

procedure TdxListSubItemViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  if not Owner.SharedBackground then
    LookAndFeelPainter.DrawListViewBackground(ACanvas, Bounds, ExplorerStyle);
  inherited DrawContent(ACanvas);
end;

function TdxListSubItemViewInfo.GetTextColor: TColor;
begin
  Result := ListView.Fonts.SubItem.Color;
  if Result = clDefault then
    if Options.Report.RowSelect then
      Result := Owner.GetTextColor
    else
      Result := LookAndFeelPainter.GetListViewItemTextColor([], ExplorerStyle);
end;

function TdxListSubItemViewInfo.GetTextFlags: Integer;
begin
  Result := Owner.GetColumnTextFlags(ColumnIndex);
end;

function TdxListSubItemViewInfo.HasGlyph: Boolean;
begin
  Result := inherited HasGlyph and (ImageIndex >= 0);
end;

procedure TdxListSubItemViewInfo.Initialize(AColumnIndex: Integer;
const AText: string; AImageIndex: Integer);
begin
  FColumnIndex := AColumnIndex;
  inherited Initialize(AText, AImageIndex);
end;

{ TdxListItemReportStyleViewInfo }

constructor TdxListItemReportStyleViewInfo.Create
  (AOwner: TdxListViewCustomGroupViewInfo; const AItemIndex: Integer;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AOwner, AItemIndex, AViewParams);
  FSubItems := TdxFastObjectList.Create(True, ListView.Columns.Count);
end;

destructor TdxListItemReportStyleViewInfo.Destroy;
begin
  FSubItems.Free;
  inherited Destroy;
end;

function TdxListItemReportStyleViewInfo.GetSubItem(Index: Integer)
  : TdxListSubItemViewInfo;
begin
  Result := TdxListSubItemViewInfo(FSubItems.List[Index]);
end;

function TdxListItemReportStyleViewInfo.GetSubItemCount: Integer;
begin
  Result := FSubItems.Count;
end;

function TdxListItemReportStyleViewInfo.GetTextFlags: Integer;
begin
  Result := GetColumnTextFlags(FColumnIndex);
end;

function TdxListItemReportStyleViewInfo.HasGlyph: Boolean;
begin
  Result := inherited HasGlyph and not FHiddenGlyph;
end;

procedure TdxListItemReportStyleViewInfo.AddSubItem
  (AViewInfo: TdxListSubItemViewInfo);
begin
  FSubItems.Add(AViewInfo);
end;

procedure TdxListItemReportStyleViewInfo.BeforeDraw;
begin
  inherited BeforeDraw;
  FSharedBackground := Options.Report.RowSelect;
end;

procedure TdxListItemReportStyleViewInfo.CalculateCheckGlyphBounds;
var
  AGlyphsAreaBounds, R: TRect;
  ASize: TSize;
begin
  if FStateButtonInSubItem and
    (ViewParams.StateViewKind <> TdxListItemStateViewKind.None) then
  begin
    ASize.Init(ViewParams.GlyphSize.cx + ViewParams.StateGlyphSize.cx +
      ViewParams.GlyphIndent, Max(ViewParams.GlyphSize.cy,
      ViewParams.StateGlyphSize.cy));

    R := Bounds;
    R.Deflate(ViewParams.Padding);
    AGlyphsAreaBounds := CalculateBoundsCore(R, Bounds,
      ViewParams.GlyphsAreaSize, OppositePositionMap[ViewParams.TextPosition]);
    R := cxRectCenter(AGlyphsAreaBounds, ASize);
    FStateGlyphBounds := CalculateBoundsCore(R, AGlyphsAreaBounds,
      ViewParams.StateGlyphSize, posLeft);
  end
  else
    inherited CalculateCheckGlyphBounds;
end;

procedure TdxListItemReportStyleViewInfo.CalculateGlyphsAreaBounds;
var
  AWidth: Integer;
begin
  inherited CalculateGlyphsAreaBounds;
  if FStateButtonInSubItem then
  begin
    AWidth := FGlyphsAreaBounds.Width;
    Dec(AWidth, ViewParams.GlyphIndent + ViewParams.StateGlyphSize.cx);
    FGlyphsAreaBounds.Width := AWidth;
  end;
end;

procedure TdxListItemReportStyleViewInfo.CalculateGlyphsBounds
  (const ABounds: TRect);
begin
  if FStateButtonInSubItem and
    (ViewParams.StateViewKind <> TdxListItemStateViewKind.None) then
  begin
    CalculateCheckGlyphBounds;
    FGlyphBounds := CalculateBoundsCore(ABounds, GlyphsAreaBounds,
      ViewParams.GlyphSize, posLeft);
  end
  else
    inherited CalculateGlyphsBounds(ABounds)
end;

procedure TdxListItemReportStyleViewInfo.DoCalculate;
var
  I, ASubItemIndex, ASubItemCount: Integer;
  ASubItemViewInfo: TdxListSubItemViewInfo;
  ABounds, ASubItemBounds: TRect;
  AColumn: TdxListColumn;
begin
  ABounds := Bounds;
  ASubItemCount := GetSubItemCount;
  for I := 0 to ListView.Columns.Count - 1 do
  begin
    AColumn := ListView.Columns[I];
    ABounds.Width := AColumn.Width;
    ASubItemIndex := AColumn.FSubItemIndex;
    if ASubItemIndex < 0 then
    begin
      FColumnIndex := I;
      FItemBounds := ABounds;
      FContentBounds := ABounds;
      FContentBounds.Deflate(ViewParams.Padding);
      CalculateGlyphsAreaBounds;
      CalculateTextAndGlyphLayout;
      CalculateTextBounds;
    end
    else if ASubItemIndex < ASubItemCount then
    begin
      ASubItemViewInfo := SubItems[ASubItemIndex];
      ASubItemBounds := ABounds;
      if FStateButtonInSubItem and (I = 0) then
        Inc(ASubItemBounds.Left, ViewParams.StateGlyphSize.cx +
          ViewParams.GlyphIndent);
      ASubItemViewInfo.Calculate(ctLight, ASubItemBounds);
    end;
    ABounds.Offset(AColumn.Width + GetItemsGap, 0);
  end;
  CalculateContentBounds;
end;

procedure TdxListItemReportStyleViewInfo.DrawBackground
  (ACanvas: TcxCustomCanvas);
begin
  Painter.DrawReportItemBackground(ACanvas, Self);
end;

procedure TdxListItemReportStyleViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  I: Integer;
begin
  DrawBackground(ACanvas);
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ItemBounds);
    DrawGlyph(ACanvas);
    DrawText(ACanvas);
    DrawStateGlyph(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
  for I := 0 to SubItemCount - 1 do
    SubItems[I].Draw(ACanvas);
end;

function TdxListItemReportStyleViewInfo.GetColumnTextFlags
  (AColumnIndex: Integer): Integer;
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or
    CXTO_END_ELLIPSIS or CXTO_SINGLELINE or CXTO_LEFT or CXTO_CENTER_VERTICALLY;
  Result := Result or HorizontalAlignmentMap
    [ListView.Columns[AColumnIndex].Alignment];
end;

procedure TdxListItemReportStyleViewInfo.Initialize(AItem: TdxListItem);
var
  I, ASubItemIndex, ASubItemCount, AImageIndex: Integer;
  ASubItemViewInfo: TdxListSubItemViewInfo;
  AColumn: TdxListColumn;
  AShowItemImageInFirstColumn: Boolean;
begin
  AShowItemImageInFirstColumn :=
    Options.Report.AlwaysShowItemImageInFirstColumn;
  ASubItemCount := AItem.SubItems.Count;
  FSubItems.Clear;
  for I := 0 to ListView.Columns.Count - 1 do
  begin
    AColumn := ListView.Columns[I];
    ASubItemIndex := AColumn.FSubItemIndex;
    if ASubItemIndex < 0 then
    begin
      FStateButtonInSubItem := (I > 0) and
        (ViewParams.StateViewKind <> TdxListItemStateViewKind.None);
      FHiddenGlyph := AShowItemImageInFirstColumn and (I > 0);
      if FHiddenGlyph then
        AImageIndex := -1
      else
        AImageIndex := AItem.ImageIndex;
      inherited Initialize(AItem.Caption, AImageIndex);
    end
    else
    begin
      if ASubItemIndex >= ASubItemCount then
        Continue;
      ASubItemViewInfo := TdxListSubItemViewInfo.Create(Self, ViewParams);
      if AShowItemImageInFirstColumn and (I = 0) then
        AImageIndex := AItem.ImageIndex
      else
        AImageIndex := AItem.SubItemImages[ASubItemIndex];
      ASubItemViewInfo.Initialize(I, AItem.SubItems[ASubItemIndex],
        AImageIndex);
      AddSubItem(ASubItemViewInfo);
    end;
  end;
end;

function TdxListItemReportStyleViewInfo.StartEdit(AShift: TShiftState;
const AMousePos: TPoint): Boolean;
begin
  Result := FTextAreaBounds.Contains(AMousePos);
end;

{ TdxListColumnHeaderViewInfo }

constructor TdxListColumnHeaderViewInfo.Create
  (AOwner: TdxListViewColumnHeadersViewInfo; AColumn: TdxListColumn;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AOwner.ListView, AViewParams);
  FColumn := AColumn;
  FOwner := AOwner;
  Initialize(AColumn.Caption, AColumn.ImageIndex);
end;

procedure TdxListColumnHeaderViewInfo.BeforeDraw;
begin
  inherited BeforeDraw;
  FState := GetState;
end;

procedure TdxListColumnHeaderViewInfo.CalculateTextAndGlyphLayout;
var
  ASortArrowSize: TPoint;
  ABounds: TRect;
begin
  CalculateGlyphsBounds(FGlyphsAreaBounds);
  ABounds := FContentBounds;
  if Column.SortOrder <> soNone then
  begin
    FSortArrowBounds := ABounds;
    ASortArrowSize := Owner.SortArrowSize;
    FSortArrowBounds.Left := FSortArrowBounds.Right - ASortArrowSize.X;
    FSortArrowBounds.Height := ASortArrowSize.Y;
    FSortArrowBounds.Offset(0,
      (FContentBounds.Height - ASortArrowSize.Y) div 2);
    ABounds.Right := FSortArrowBounds.Left - ViewParams.Padding.Left;
  end;
  CalculateTextAreaBounds(ABounds);
end;

procedure TdxListColumnHeaderViewInfo.CheckShowHint;
var
  ATextSize: TSize;
  R: TRect;
begin
  R := TextBounds;
  TdxTextMeasurer.TextRectDT(R, GetHint, ListView.Fonts.ColumnHeader,
    DT_WORDBREAK);
  ATextSize := R.Size;
  if (TextBounds.Width < ATextSize.cx) or (TextBounds.Height < ATextSize.cy)
  then
    ListView.ViewInfo.CheckHint(Self);
end;

procedure TdxListColumnHeaderViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  ANeighbors: TcxNeighbors;
begin
  ANeighbors := [nLeft, nRight];
  if Column.Index = 0 then
    Exclude(ANeighbors, nLeft);
  if Column.Index = ListView.Columns.Count - 1 then
    Exclude(ANeighbors, nRight);
  LookAndFeelPainter.DrawScaledHeader(ACanvas, Bounds, State, ANeighbors,
    LookAndFeelPainter.HeaderBorders(ANeighbors), ScaleFactor);
  inherited DrawContent(ACanvas);
  if Column.SortOrder <> soNone then
    LookAndFeelPainter.DrawListViewSortingMark(ACanvas, SortArrowBounds,
      Column.SortOrder = soAscending, ScaleFactor);
end;

function TdxListColumnHeaderViewInfo.GetHint: string;
begin
  Result := Column.Caption;
end;

function TdxListColumnHeaderViewInfo.GetHintBounds: TRect;
begin
  Result := TextBounds;
end;

function TdxListColumnHeaderViewInfo.GetHottrackPart(const AMousePos: TPoint)
  : THottrackPart;
begin
  if ListView.IsDragging then
    Exit(THottrackPart.None);

  if Bounds.Contains(AMousePos) then
    Result := THottrackPart.Content
  else
    Result := THottrackPart.None;
end;

function TdxListColumnHeaderViewInfo.GetState: TcxButtonState;
begin
  if Column = Controller.PressedColumn then
    Result := cxbsPressed
  else
    case HotTrackPart of
      TdxListColumnHeaderViewInfo.THottrackPart.Content:
        Result := cxbsHot;
    else
      Result := cxbsNormal;
    end;
end;

function TdxListColumnHeaderViewInfo.GetTextColor: TColor;
begin
  Result := ListView.Fonts.ColumnHeader.Color;
  if Result = clDefault then
    Result := LookAndFeelPainter.GetListViewColumnHeaderTextColor(State,
      ExplorerStyle);
end;

function TdxListColumnHeaderViewInfo.GetTextFlags: Integer;
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or
    CXTO_END_ELLIPSIS or CXTO_SINGLELINE or CXTO_LEFT or CXTO_CENTER_VERTICALLY;
  Result := Result or HorizontalAlignmentMap[Column.HeaderAlignment];
end;

function TdxListColumnHeaderViewInfo.HasGlyph: Boolean;
begin
  Result := inherited HasGlyph and (ImageIndex >= 0);
end;

procedure TdxListColumnHeaderViewInfo.MouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
  if AButton = mbLeft then
    Controller.PressedColumn := Column;
end;

procedure TdxListColumnHeaderViewInfo.MouseLeave;
begin
  HotTrackPart := THottrackPart.None;
end;

procedure TdxListColumnHeaderViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
begin
  HotTrackPart := GetHottrackPart(AMousePos);
end;

procedure TdxListColumnHeaderViewInfo.SetHotTrackPart(AValue: THottrackPart);
begin
  if FHotTrackPart <> AValue then
  begin
    FHotTrackPart := AValue;
    Invalidate;
  end;
end;

{ TdxListViewHeadersViewInfo }

constructor TdxListViewColumnHeadersViewInfo.Create
  (AOwner: TdxListViewViewInfo);
begin
  inherited Create(AOwner.ListView);
  FItems := TdxFastObjectList.Create(True, ListView.Columns.Count);
  FOwner := AOwner;
  FViewParams := TdxListViewCellViewParams.Create;
  CalculateViewParams;
end;

destructor TdxListViewColumnHeadersViewInfo.Destroy;
begin
  FViewParams.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TdxListViewColumnHeadersViewInfo.DrawContent
  (ACanvas: TcxCustomCanvas);
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxListViewColumnHeadersViewInfo.FindItem(const AMousePos: TPoint)
  : TdxListColumnHeaderViewInfo;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I];
    if Result.Bounds.Contains(AMousePos) then
      Exit;
  end;
  Result := nil;
end;

function TdxListViewColumnHeadersViewInfo.GetCursor(const AMousePos
  : TPoint): TCursor;
begin
  if GetResizingColumn(AMousePos) <> nil then
    Result := crcxGridHorzSize
  else
    Result := crDefault;
end;

function TdxListViewColumnHeadersViewInfo.GetResizingColumn(const AMousePos
  : TPoint): TdxListColumn;
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to ItemCount - 1 do
  begin
    R := Items[I].Bounds;
    R.Left := R.Right - 3;
    Inc(R.Right, 5);
    if R.Contains(AMousePos) then
      Exit(Items[I].Column);
  end;
  Result := nil;
end;

function TdxListViewColumnHeadersViewInfo.CalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledHeaderHeight
    (TdxTextMeasurer.TextLineHeight(ListView.Fonts.ColumnHeader), ScaleFactor) +
    ViewParams.Padding.Height;
end;

procedure TdxListViewColumnHeadersViewInfo.CalculateViewParams;
var
  APadding: TRect;
  AExtraPadding: TdxPadding;
begin
  AExtraPadding := Options.PaddingOptions.Item.Margin;
  APadding := LookAndFeelPainter.HeaderContentOffsets(ScaleFactor);
  AExtraPadding.InflatePadding(APadding);
  Owner.CalculateCommonViewParams(ViewParams, APadding,
    ListView.Fonts.ColumnHeader, ListView.Images.SmallImages);
  ViewParams.StateViewKind := Owner.GetItemStateViewKind;
  ViewParams.StateImages := ListView.Images.StateImages;
  ViewParams.StateGlyphSize := Owner.CalculateItemStateGlyphSize;
  ViewParams.GlyphsAreaSize := Owner.CalculateItemGlyphsAreaSize;
  ViewParams.TextPosition := posRight;
  ViewParams.TextLineCount := 0;
  FSortArrowSize := LookAndFeelPainter.GetListViewColumnHeaderSortingMarkSize
    (ScaleFactor);
end;

procedure TdxListViewColumnHeadersViewInfo.Calculate(AType: TdxChangeType;
const ABounds: TRect);
var
  AHeaderBounds: TRect;
  I: Integer;
  ARecalculate: Boolean;
  AItemViewInfo: TdxListColumnHeaderViewInfo;
begin
  FBounds := ABounds;
  FBounds.Height := CalculateHeight;
  ARecalculate := AType > TdxChangeType.ctLight;
  if ARecalculate then
    PopulateItems;
  AHeaderBounds := Bounds;
  for I := 0 to ItemCount - 1 do
  begin
    AHeaderBounds.Width := ListView.Columns[I].Width;
    AItemViewInfo := Items[I];
    if ARecalculate then
      AItemViewInfo.MakeDirty;
    AItemViewInfo.Calculate(AType, AHeaderBounds);
    AHeaderBounds.Offset(AHeaderBounds.Width, 0);
  end;
end;

function TdxListViewColumnHeadersViewInfo.GetItem(Index: Integer)
  : TdxListColumnHeaderViewInfo;
begin
  Result := TdxListColumnHeaderViewInfo(FItems.List[Index]);
end;

function TdxListViewColumnHeadersViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxListViewColumnHeadersViewInfo.MouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
var
  AItemViewInfo: TdxListColumnHeaderViewInfo;
begin
  AItemViewInfo := FindItem(AMousePos);
  if AItemViewInfo <> nil then
    AItemViewInfo.MouseDown(AButton, AShift, AMousePos);
end;

procedure TdxListViewColumnHeadersViewInfo.MouseLeave;
begin
  HottrackItem := nil;
end;

procedure TdxListViewColumnHeadersViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
begin
  HottrackItem := FindItem(AMousePos);
  if HottrackItem <> nil then
    HottrackItem.MouseMove(AShift, AMousePos);
end;

procedure TdxListViewColumnHeadersViewInfo.PopulateItems;
var
  I: Integer;
begin
  FItems.Clear;
  for I := 0 to ListView.Columns.Count - 1 do
    FItems.Add(TdxListColumnHeaderViewInfo.Create(Self, ListView.Columns[I],
      ViewParams));
end;

procedure TdxListViewColumnHeadersViewInfo.SetHottrackItem
  (AValue: TdxListColumnHeaderViewInfo);
begin
  if FHottrackItem <> AValue then
  begin
    ExchangePointers(FHottrackItem, AValue);
    if AValue <> nil then
      AValue.MouseLeave;
    if FHottrackItem <> nil then
      FHottrackItem.CheckShowHint;
  end;
end;

{ TdxListViewDropTargetViewInfo }

procedure TdxListViewDropTargetViewInfo.Calculate(ATargetObject: TObject;
const ATargetBounds: TRect; ASide: TcxBorder);
var
  ABounds: TRect;
begin
  FSide := ASide;
  FTargetObject := ATargetObject;
  FTargetBounds := ATargetBounds;

  if FTargetObject = nil then
    ABounds := cxNullRect
  else
    case Side of
      bLeft:
        ABounds := cxRectSetWidth(TargetBounds, Size);
      bRight:
        ABounds := cxRectSetRight(TargetBounds, TargetBounds.Right, Size);
      bTop:
        ABounds := cxRectSetHeight(TargetBounds, Size);
    else
      ABounds := cxRectSetBottom(TargetBounds, TargetBounds.Bottom, Size);
    end;

  if UseRightToLeftAlignment then
    ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds,
      ATargetBounds);

  if not cxRectIsEqual(ABounds, Bounds) then
  begin
    ListView.InvalidateRect(Bounds, False);
    FBounds := ABounds;
    ListView.InvalidateRect(Bounds, False);
  end;
end;

procedure TdxListViewDropTargetViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  Painter.DrawDropTargetSelection(ACanvas, Self);
end;

function TdxListViewDropTargetViewInfo.GetSize: Integer;
begin
  Result := ScaleFactor.Apply(DropTargetSize);
end;

{ TdxListViewPainter }

constructor TdxListViewPainter.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FImagesPaintCache := TdxImageListPaintCache.Create(Self);
end;

destructor TdxListViewPainter.Destroy;
begin
  FreeAndNil(FImagesPaintCache);
  inherited Destroy;
end;

procedure TdxListViewPainter.DrawBackground(ACanvas: TcxCustomCanvas;
const ABounds: TRect; AExplorerStyle: Boolean);
begin
  LookAndFeelPainter.DrawListViewBackground(ACanvas, ABounds, AExplorerStyle);
end;

procedure TdxListViewPainter.DrawGroupHeader(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListViewGroupViewInfo);
begin
  DrawGroupHeaderBackground(ACanvas, AViewInfo);
  DrawGroupText(ACanvas, AViewInfo.Header);

  if AViewInfo.Group.HeaderAlign <> taCenter then
    LookAndFeelPainter.DrawListViewGroupHeaderLine(ACanvas,
      AViewInfo.HeaderLineBounds);

  if AViewInfo.HasExpandButton then
    LookAndFeelPainter.DrawListViewGroupExpandButton(ACanvas,
      AViewInfo.ExpandButtonBounds, AViewInfo.ExpandButtonState,
      not AViewInfo.Group.Collapsed, AViewInfo.ExplorerStyle, ScaleFactor);

  DrawGroupText(ACanvas, AViewInfo.Subtitle);
  DrawGroupText(ACanvas, AViewInfo.Footer);
end;

procedure TdxListViewPainter.DrawGroupHeaderBackground(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListViewGroupViewInfo);
begin
  if AViewInfo.HeaderState * [dxlgsFocused, dxlgsHot] <> [] then
    LookAndFeelPainter.DrawListViewGroupHeaderBackground(ACanvas,
      AViewInfo.HeaderHottrackBounds, AViewInfo.HeaderState,
      AViewInfo.ExplorerStyle, ScaleFactor);
end;

procedure TdxListViewPainter.DrawGroupText(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListViewGroupTextViewInfo);
begin
  if AViewInfo <> nil then
    AViewInfo.Draw(ACanvas);
end;

procedure TdxListViewPainter.DrawCheckButton(ACanvas: TcxCustomCanvas;
const ABounds: TRect; AState: TcxButtonState; AChecked: Boolean);
begin
  LookAndFeelPainter.DrawListViewCheckButton(ACanvas, ABounds, AState, AChecked,
    ScaleFactor);
end;

procedure TdxListViewPainter.DrawItemBackground(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListItemCustomViewInfo);
begin
  if AViewInfo.State * [dxlisFocused, dxlisHot, dxlisSelected] <> [] then
    LookAndFeelPainter.DrawListViewItemBackground(ACanvas, AViewInfo.Bounds,
      AViewInfo.State, AViewInfo.ExplorerStyle);
end;

procedure TdxListViewPainter.DrawReportItemBackground(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListItemReportStyleViewInfo);
var
  ABounds: TRect;
begin
  if AViewInfo.State * [dxlisFocused, dxlisHot, dxlisSelected] <> [] then
  begin
    if AViewInfo.SharedBackground then
      ABounds := AViewInfo.Bounds
    else
      ABounds := AViewInfo.ItemBounds;
    LookAndFeelPainter.DrawListViewItemBackground(ACanvas, ABounds,
      AViewInfo.State, AViewInfo.ExplorerStyle);
  end;
end;

procedure TdxListViewPainter.DrawMultiSelectionRect(ACanvas: TcxCustomCanvas;
const ABounds: TRect);
var
  R: TRect;
begin
  R := ListView.ClientBounds;
  R.Inflate(2);
  R.Intersect(ABounds);
  ACanvas.Rectangle(R, clNone, clBlue, psSolid);
  ACanvas.FillRect(R, RGB($49, $A7, $FF), 96);
end;

procedure TdxListViewPainter.DrawDropTargetSelection(ACanvas: TcxCustomCanvas;
AViewInfo: TdxListViewDropTargetViewInfo);
begin
  ACanvas.FillRect(AViewInfo.Bounds,
    LookAndFeelPainter.GetGalleryDropTargetSelectionColor);
end;

procedure TdxListViewPainter.InvalidateImageList(AImageList: TCustomImageList);
begin
  TdxImageListPaintCache(FImagesPaintCache).InvalidateImageList(AImageList);
end;

procedure TdxListViewPainter.PrepareGlyphBitmap(ABitmap: TcxAlphaBitmap;
AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
ADrawMode: TcxImageDrawMode; ASmoothImage: Boolean; ABrush: THandle;
ATransparentColor: TColor; AUseLeftBottomPixelAsTransparent: Boolean;
APalette: IdxColorPalette);
var
  ABounds: TRect;
begin
  ABounds := ABitmap.ClientRect;
  cxDrawImage(ABitmap.cxCanvas.Handle, ABounds, ABounds, nil, AImages,
    AImageIndex, ADrawMode, ASmoothImage, ABrush, ATransparentColor,
    AUseLeftBottomPixelAsTransparent, APalette);
  if AOverlayIndex >= 0 then
    cxDrawImage(ABitmap.cxCanvas.Handle, ABounds, ABounds, nil, AImages,
      AOverlayIndex, ADrawMode, ASmoothImage, ABrush, ATransparentColor,
      AUseLeftBottomPixelAsTransparent, APalette);
end;

function TdxListViewPainter.CreateCanvasBasedFont(AFont: TFont)
  : TcxCanvasBasedFont;
begin
  Result := ListView.ActualCanvas.CreateFont(AFont);
end;

function TdxListViewPainter.CreateCanvasBasedImage(ABitmap: TBitmap;
AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage;
begin
  Result := ListView.ActualCanvas.CreateImage(ABitmap, AAlphaFormat);
end;

function TdxListViewPainter.CreateCanvasBasedTextLayout
  : TcxCanvasBasedTextLayout;
begin
  Result := ListView.ActualCanvas.CreateTextLayout;
end;

procedure TdxListViewPainter.DrawGlyphCore(ACanvas: TcxCustomCanvas;
const ABounds: TRect; AImages: TCustomImageList;
AImageIndex, AOverlayIndex: Integer; AMode: TcxImageDrawMode);
begin
  if (AImages <> nil) and (AImageIndex >= 0) then
    TdxImageListPaintCache(FImagesPaintCache).Draw(ACanvas, ABounds, AImages,
      AImageIndex, AOverlayIndex, AMode, False, nil, False);
end;

function TdxListViewPainter.DrawItemSelectionFirst: Boolean;
begin
  Result := LookAndFeelPainter.DrawGalleryItemSelectionFirst;
end;

function TdxListViewPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ListView.LookAndFeelPainter;
end;

function TdxListViewPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := ListView.ScaleFactor;
end;

function TdxListViewPainter.GetUseRightToLeftAlignment: Boolean;
begin
  Result := ListView.UseRightToLeftAlignment;
end;

{ TdxListViewCustomGroupViewInfo }

constructor TdxListViewCustomGroupViewInfo.Create(AOwner: TdxListViewViewInfo;
AGroup: TdxListGroup);
begin
  FOwner := AOwner;
  inherited Create(AOwner.ListView);
  FGroup := AGroup;
  FItems := TdxFastObjectList.Create(True, 512);
  FVisibleItems := TdxFastList.Create(512);
end;

destructor TdxListViewCustomGroupViewInfo.Destroy;
begin
  FVisibleItems.Free;
  FItems.Free;
  FColumnWidths.Free;
  inherited Destroy;
end;

function TdxListViewCustomGroupViewInfo.GetItem(Index: Integer)
  : TdxListItemCustomViewInfo;
begin
  Result := TdxListItemCustomViewInfo(FItems.List[Index]);
end;

function TdxListViewCustomGroupViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxListViewCustomGroupViewInfo.GetItemSize: TSize;
begin
  Result := Owner.ItemSize;
end;

function TdxListViewCustomGroupViewInfo.GetVisibleItem(Index: Integer)
  : TdxListItemCustomViewInfo;
begin
  Result := TdxListItemCustomViewInfo(FVisibleItems.List[Index]);
end;

function TdxListViewCustomGroupViewInfo.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

procedure TdxListViewCustomGroupViewInfo.SetHotTrackPart
  (AValue: TdxListGroupPart);
begin
  if FHotTrackPart <> AValue then
  begin
    InvalidatePart(FHotTrackPart);
    InvalidatePart(AValue);
    FHotTrackPart := AValue;
  end;
end;

procedure TdxListViewCustomGroupViewInfo.Calculate(AType: TdxChangeType;
const ABounds: TRect);
begin
  inherited Calculate(AType, ABounds);
  CalculateRowAndColumnCount;
  CalculateContentVisibleBounds;
  CalculateContent(AType);
  CalculateContentBounds;
end;

function TdxListViewCustomGroupViewInfo.FindItemViewInfo(AItem: TdxListItem;
out AViewInfo: TdxListItemCustomViewInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to VisibleItemCount - 1 do
  begin
    AViewInfo := VisibleItems[I];
    if AViewInfo.Item = AItem then
      Exit(True);
  end;
  AViewInfo := nil;
end;

function TdxListViewCustomGroupViewInfo.GetItemAtPos(const P: TPoint;
out AViewInfo: TdxListItemCustomViewInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to VisibleItemCount - 1 do
  begin
    AViewInfo := VisibleItems[I];
    if AViewInfo.Bounds.Contains(P) then
      Exit(True);
  end;
  AViewInfo := nil;
end;

function TdxListViewCustomGroupViewInfo.GetPart(const P: TPoint;
out AItemViewInfo: TdxListItemCustomViewInfo): TdxListGroupPart;
begin
  if ContentBounds.Contains(P) then
  begin
    if GetItemAtPos(P, AItemViewInfo) then
      Exit(TdxListGroupPart.Item)
    else
      Result := TdxListGroupPart.Content;
  end
  else
    Result := TdxListGroupPart.None;
  AItemViewInfo := nil;
end;

procedure TdxListViewCustomGroupViewInfo.AddVisibleItemViewInfo
  (AViewInfo: TdxListItemCustomViewInfo);
begin
  FVisibleItems.Add(AViewInfo);
  Owner.FItems.Add(AViewInfo);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateRowAndColumnCount;
var
  AItemsGap: Integer;
begin
  AItemsGap := GetItemsGap;
  if IsReportView then
  begin
    FColumnCount := 1;
    FRowCount := ItemCount;
  end
  else if IsHorizontalItemsArrangement then
  begin
    FColumnCount := Max(1, (GetAvailableWidthForItems + AItemsGap)
      div (ItemSize.cx + AItemsGap));
    FRowCount := ItemCount div FColumnCount;
    if ItemCount mod FColumnCount > 0 then
      Inc(FRowCount);
    FRowCount := Max(1, FRowCount);
  end
  else
  begin
    FRowCount := Max(1, (GetAvailableHeightForItems + AItemsGap)
      div (ItemSize.cy + AItemsGap));
    FColumnCount := ItemCount div FRowCount;
    if ItemCount mod FRowCount > 0 then
      Inc(FColumnCount);
    FColumnCount := Max(1, FColumnCount);
  end
end;

procedure TdxListViewCustomGroupViewInfo.CalculateContent(AType: TdxChangeType);
begin
  CalculateItemsArea;
  CalculateItems(AType);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateItems(AType: TdxChangeType);
begin
  case ListView.ViewStyle of
    TdxListViewStyle.Icon, TdxListViewStyle.SmallIcon:
      CalculateIconsViewStyle(AType);
    TdxListViewStyle.List:
      CalculateListViewStyle(AType);
    TdxListViewStyle.Report:
      CalculateReportViewStyle(AType);
  end;
end;

function TdxListViewCustomGroupViewInfo.CalculateItemViewInfo(AIndex: Integer;
const ABounds: TRect): TdxListItemCustomViewInfo;
var
  AItem: TdxListItem;
begin
  Result := Items[AIndex];
  AItem := ListView.GetItem(Result.ItemIndex);
  Result.Initialize(AItem);
  Result.Calculate(TdxChangeType.ctHard, ABounds);
  AddVisibleItemViewInfo(Result);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateHorizontallyArrangedIcons
  (AType: TdxChangeType);
var
  I, AColumnIndex, AFirstVisibleItemIndex, ALastVisibleItemIndex, AItemsGap,
    ACount: Integer;
  AItemBounds: TRect;
begin
  AItemsGap := GetItemsGap;
  GetVisibleItemsRange(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  if ListView.OwnerData then
    ListView.OwnerDataHint(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  AItemBounds.InitSize(GetItemOrigin(AFirstVisibleItemIndex), ItemSize);
  AColumnIndex := 0;
  FVisibleItems.Count := 0;
  for I := AFirstVisibleItemIndex to ALastVisibleItemIndex do
  begin
    CalculateItemViewInfo(I, AItemBounds);
    Inc(AColumnIndex);
    if AColumnIndex = ColumnCount then
    begin
      AColumnIndex := 0;
      AItemBounds.Offset(0, ItemSize.cy + AItemsGap);
      AItemBounds.X := ItemsAreaBounds.Left;
    end
    else
      AItemBounds.Offset(ItemSize.cx + AItemsGap, 0);
  end;
  FItemsAreaBounds.Bottom := ItemsAreaBounds.Top + GetItemsHeight(RowCount);
  ACount := IfThen(ColumnCount < ItemCount, ColumnCount, ItemCount);
  FItemsAreaBounds.Width := Max(GetItemsWidth(ACount), ItemSize.cx);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateVerticallyArrangedIcons
  (AType: TdxChangeType);
var
  I, ARowIndex, AFirstVisibleItemIndex, ALastVisibleItemIndex, AItemsGap,
    ACount: Integer;
  AItemBounds: TRect;
begin
  AItemsGap := GetItemsGap;
  GetVisibleItemsRange(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  if ListView.OwnerData then
    ListView.OwnerDataHint(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  AItemBounds.InitSize(GetItemOrigin(AFirstVisibleItemIndex), ItemSize);
  ARowIndex := 0;
  FVisibleItems.Count := 0;
  for I := AFirstVisibleItemIndex to ALastVisibleItemIndex do
  begin
    CalculateItemViewInfo(I, AItemBounds);
    Inc(ARowIndex);
    if ARowIndex = RowCount then
    begin
      ARowIndex := 0;
      AItemBounds.Offset(ItemSize.cx + AItemsGap, 0);
      AItemBounds.Y := ItemsAreaBounds.Top;
    end
    else
      AItemBounds.Offset(0, ItemSize.cy + AItemsGap);
  end;
  FItemsAreaBounds.Right := ItemsAreaBounds.Left + GetItemsWidth(ColumnCount);
  ACount := IfThen(RowCount < ItemCount, RowCount, ItemCount);
  FItemsAreaBounds.Height := Max(GetItemsHeight(ACount), ItemSize.cy);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateIconsViewStyle
  (AType: TdxChangeType);
begin
  if IsHorizontalItemsArrangement then
    CalculateHorizontallyArrangedIcons(AType)
  else
    CalculateVerticallyArrangedIcons(AType);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateListViewStyle
  (AType: TdxChangeType);
var
  I, ARowIndex, AFirstVisibleItemIndex, ALastVisibleItemIndex, AItemsGap,
    ACount: Integer;
  AItemBounds: TRect;
begin
  AItemsGap := GetItemsGap;
  GetVisibleItemsRange(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  if ListView.OwnerData then
    ListView.OwnerDataHint(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  AItemBounds.InitSize(GetItemOrigin(AFirstVisibleItemIndex), ItemSize);
  ARowIndex := 0;
  FVisibleItems.Count := 0;
  for I := AFirstVisibleItemIndex to ALastVisibleItemIndex do
  begin
    CalculateItemViewInfo(I, AItemBounds);
    Inc(ARowIndex);
    if ARowIndex = RowCount then
    begin
      ARowIndex := 0;
      AItemBounds.Offset(ItemSize.cx + AItemsGap, 0);
      AItemBounds.Y := ItemsAreaBounds.Top;
    end
    else
      AItemBounds.Offset(0, ItemSize.cy + AItemsGap);
  end;
  FItemsAreaBounds.Right := ItemsAreaBounds.Left + GetItemsWidth(ColumnCount);
  ACount := IfThen(RowCount < ItemCount, RowCount, ItemCount);
  FItemsAreaBounds.Height := Max(GetItemsHeight(ACount), ItemSize.cy);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateReportViewStyle
  (AType: TdxChangeType);
var
  I, ARowIndex, AFirstVisibleItemIndex, ALastVisibleItemIndex, AItemsGap,
    ACount: Integer;
  AItemBounds: TRect;
begin
  AItemsGap := GetItemsGap;
  GetVisibleItemsRange(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  if ListView.OwnerData then
    ListView.OwnerDataHint(AFirstVisibleItemIndex, ALastVisibleItemIndex);
  AItemBounds.InitSize(GetItemOrigin(AFirstVisibleItemIndex),
    Owner.FColumnHeadersBounds.Width, ItemSize.cy);
  ARowIndex := 0;
  FVisibleItems.Count := 0;
  for I := AFirstVisibleItemIndex to ALastVisibleItemIndex do
  begin
    CalculateItemViewInfo(I, AItemBounds);
    Inc(ARowIndex);
    if ARowIndex = RowCount then
    begin
      ARowIndex := 0;
      AItemBounds.Offset(ItemSize.cx + AItemsGap, 0);
      AItemBounds.Y := ItemsAreaBounds.Top;
    end
    else
      AItemBounds.Offset(0, ItemSize.cy + AItemsGap);
  end;
  FItemsAreaBounds.Right := ItemsAreaBounds.Left + GetItemsWidth(ColumnCount);
  ACount := IfThen(RowCount < ItemCount, RowCount, ItemCount);
  FItemsAreaBounds.Height := Max(GetItemsHeight(ACount), ItemSize.cy);
end;

procedure TdxListViewCustomGroupViewInfo.CalculateItemsArea;
begin
  FItemsAreaBounds := Bounds;
end;

procedure TdxListViewCustomGroupViewInfo.CalculateContentBounds;
begin
  FContentBounds := ItemsAreaBounds;
end;

procedure TdxListViewCustomGroupViewInfo.CalculateContentVisibleBounds;
begin
  FContentVisibleBounds := Owner.GetGroupVisibleBounds;
end;

procedure TdxListViewCustomGroupViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  I: Integer;
begin
  for I := 0 to VisibleItemCount - 1 do
    VisibleItems[I].Draw(ACanvas);
end;

function TdxListViewCustomGroupViewInfo.GetAvailableHeightForItems: Integer;
begin
  Result := Bounds.Height;
end;

function TdxListViewCustomGroupViewInfo.GetAvailableWidthForItems: Integer;
begin
  Result := Bounds.Width;
end;

function TdxListViewCustomGroupViewInfo.GetBoundsForItem
  (AItem: TdxListItem): TRect;
begin
  Result := GetBoundsForItem(AItem.Index);
end;

function TdxListViewCustomGroupViewInfo.GetBoundsForItem
  (AItemIndex: Integer): TRect;
var
  I, AIndex, AItemsGap: Integer;
begin
  AItemsGap := GetItemsGap;
  AIndex := -1;
  for I := 0 to FItems.Count - 1 do
    if Items[I].ItemIndex = AItemIndex then
    begin
      AIndex := I;
      Break;
    end;
  if AIndex < 0 then
    Exit(TRect.Null);

  if IsHorizontalItemsArrangement or IsReportView then
  begin
    Result.Top := ItemsAreaBounds.Top + (AIndex div ColumnCount) *
      (ItemSize.cy + AItemsGap);
    Result.Left := ItemsAreaBounds.Left + (AIndex mod ColumnCount) *
      (ItemSize.cx + AItemsGap);
  end
  else
  begin
    Result.Top := ItemsAreaBounds.Top + (AIndex mod RowCount) *
      (ItemSize.cy + AItemsGap);
    Result.Left := ItemsAreaBounds.Left + (AIndex div RowCount) *
      (ItemSize.cx + AItemsGap);
  end;
  Result.Size := ItemSize;
end;

function TdxListViewCustomGroupViewInfo.GetItemOrigin
  (AItemIndex: Integer): TPoint;
begin
  Result := ItemsAreaBounds.TopLeft;
  if IsHorizontalItemsArrangement or IsReportView then
    Inc(Result.Y, (AItemIndex div ColumnCount) * (ItemSize.cy + GetItemsGap))
  else
    Inc(Result.X, (AItemIndex div RowCount) * (ItemSize.cx + GetItemsGap));
end;

function TdxListViewCustomGroupViewInfo.GetItemsHeight(ACount: Integer)
  : Integer;
begin
  if ACount = 0 then
    Exit(0);
  Result := (ItemSize.cy * ACount) + ((ACount - 1) * GetItemsGap);
end;

function TdxListViewCustomGroupViewInfo.GetItemsWidth(ACount: Integer): Integer;
begin
  if ACount = 0 then
    Exit(0);
  Result := (ItemSize.cx * ACount) + ((ACount - 1) * GetItemsGap);
end;

procedure TdxListViewCustomGroupViewInfo.GetVisibleItemsRange
  (out AFirstOrderIndex, ALastOrderIndex: Integer);
var
  AOrigin, ASize, ACount, AItemsGap: Integer;
  AVisibleBounds: TRect;
begin
  AFirstOrderIndex := 0;
  AItemsGap := GetItemsGap;
  AVisibleBounds := FContentVisibleBounds;
  AVisibleBounds.Intersect(ItemsAreaBounds);
  if AVisibleBounds.IsEmpty then
  begin
    ALastOrderIndex := -1;
    Exit;
  end;
  if IsHorizontalItemsArrangement then
  begin
    AOrigin := ItemsAreaBounds.Top - AVisibleBounds.Top;
    if AOrigin < 0 then
    begin
      ASize := -AOrigin;
      ACount := (ASize + AItemsGap) div (ItemSize.cy + AItemsGap);
      AFirstOrderIndex := ACount * ColumnCount;
    end;
    ASize := AVisibleBounds.Bottom - ItemsAreaBounds.Top;
    ACount := (ASize + AItemsGap) div (ItemSize.cy + AItemsGap);
    ALastOrderIndex := ACount * ColumnCount + (ColumnCount - 1);
  end
  else if IsReportView then
  begin
    AOrigin := ItemsAreaBounds.Top - AVisibleBounds.Top;
    if AOrigin < 0 then
    begin
      ASize := -AOrigin;
      AFirstOrderIndex := (ASize + AItemsGap) div (ItemSize.cy + AItemsGap);
    end;
    ASize := AVisibleBounds.Bottom - ItemsAreaBounds.Top;
    ALastOrderIndex := Ceil((ASize + AItemsGap) div (ItemSize.cy + AItemsGap));
  end
  else
  begin
    AOrigin := ItemsAreaBounds.Left - AVisibleBounds.Left;
    if AOrigin < 0 then
    begin
      ASize := -AOrigin;
      ACount := (ASize + AItemsGap) div (ItemSize.cx + AItemsGap);
      AFirstOrderIndex := ACount * RowCount;
    end;
    ASize := AVisibleBounds.Right - ItemsAreaBounds.Left;
    ACount := (ASize + AItemsGap) div (ItemSize.cx + AItemsGap);
    ALastOrderIndex := ACount * RowCount + (RowCount - 1);
  end;
  ALastOrderIndex := Min(ALastOrderIndex, ItemCount - 1);
end;

procedure TdxListViewCustomGroupViewInfo.InvalidatePart
  (APart: TdxListGroupPart);
begin
end;

function TdxListViewCustomGroupViewInfo.IsHorizontalItemsArrangement: Boolean;
begin
  Result := not(ListView.ViewStyle in [TdxListViewStyle.List,
    TdxListViewStyle.Report]) and
    (Options.Icons.Arrangement = TdxListIconsArrangement.Horizontal);
end;

procedure TdxListViewCustomGroupViewInfo.MouseLeave;
begin
  Invalidate;
end;

procedure TdxListViewCustomGroupViewInfo.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
var
  AItemViewInfo: TdxListItemCustomViewInfo;
begin
  HotTrackPart := GetPart(AMousePos, AItemViewInfo);
end;

function TdxListViewCustomGroupViewInfo.GetNextItem(var AItemIndex: Integer;
ADirectionX, ADirectionY: Integer): Boolean;

  function GetVerticalIndexIncrement(AHorizontalArrangement: Boolean): Integer;
  begin
    if AHorizontalArrangement then
      Result := ColumnCount * ADirectionY
    else
      Result := ADirectionY;
  end;

  function GetHorizontalIndexIncrement(AHorizontalArrangement: Boolean)
    : Integer;
  begin
    if AHorizontalArrangement then
      Result := ADirectionX
    else
      Result := RowCount * ADirectionX
  end;

  function IsLastRow(AIndex: Integer): Boolean;
  begin
    Result := (AIndex div ColumnCount) = (RowCount - 1);
  end;

  function IsLastColumn(AIndex: Integer): Boolean;
  begin
    Result := (AIndex div RowCount) = (ColumnCount - 1);
  end;

var
  AIndex, ANextIndex: Integer;
  AHorizontalArrangement: Boolean;
begin
  AIndex := ItemIndexToOrderIndex(AItemIndex);
  if AIndex < 0 then
    Exit(False);
  ANextIndex := AItemIndex;
  AHorizontalArrangement := IsHorizontalItemsArrangement;
  if ADirectionY <> 0 then
  begin
    ANextIndex := AIndex + GetVerticalIndexIncrement(AHorizontalArrangement);
    if (ADirectionY < 0) and (ANextIndex < 0) then
      Exit(False);
    if (ADirectionY > 0) and (ANextIndex >= ItemCount) then
      if AHorizontalArrangement and not IsLastRow(AIndex) then
        ANextIndex := ItemCount - 1
      else
        Exit(False);
  end;
  if ADirectionX <> 0 then
  begin
    if not AHorizontalArrangement and (ColumnCount = 1) then
      Exit(False);
    ANextIndex := AIndex + GetHorizontalIndexIncrement(AHorizontalArrangement);
    if (ADirectionX < 0) and (ANextIndex < 0) then
      Exit(False);
    if (ADirectionX > 0) and (ANextIndex >= ItemCount) then
      if not AHorizontalArrangement and not IsLastColumn(AIndex) then
        ANextIndex := ItemCount - 1
      else
        Exit(False);
  end;
  AItemIndex := OrderIndexToItemIndex(ANextIndex);
  Result := True;
end;

function TdxListViewCustomGroupViewInfo.ItemIndexToOrderIndex
  (AItemIndex: Integer): Integer;
begin
  if IsGroupView then
    Result := Group.ItemIndices.IndexOf(AItemIndex)
  else
    Result := AItemIndex;
end;

function TdxListViewCustomGroupViewInfo.OrderIndexToItemIndex
  (AVisualIndex: Integer): Integer;
begin
  if IsGroupView then
    Result := Group.ItemIndices[AVisualIndex]
  else
    Result := AVisualIndex;
end;

{ TdxListViewRootItemsLayoutViewInfo }

procedure TdxListViewRootItemsLayoutViewInfo.PopulateItems;
var
  I: Integer;
begin
  for I := 0 to ListView.Items.Count - 1 do
    FItems.Add(Owner.CreateItemViewInfo(Self, I));
end;

{ TdxListViewGroupTextViewInfo }

constructor TdxListViewGroupTextViewInfo.Create
  (AOwner: TdxListViewGroupViewInfo; AKind: TdxListViewGroupTextKind;
const AViewParams: TdxListViewCellViewParams);
begin
  inherited Create(AOwner.ListView, AViewParams);
  FKind := AKind;
  FOwner := AOwner;
end;

function TdxListViewGroupTextViewInfo.GetTextColor: TColor;
begin
  case Kind of
    dxlgtHeader:
      Result := ListView.Fonts.GroupHeader.Color;
    dxlgtSubtitle:
      Result := ListView.Fonts.GroupSubtitle.Color;
  else
    Result := ListView.Fonts.GroupFooter.Color;
  end;
  if Result = clDefault then
    Result := LookAndFeelPainter.GetListViewGroupTextColor(FKind,
      Owner.HeaderState, ExplorerStyle);
end;

function TdxListViewGroupTextViewInfo.GetTextFlags: Integer;
begin
  if Kind in [dxlgtHeader, dxlgtSubtitle] then
    Result := Owner.GetHeaderTextFlags
  else
    Result := Owner.GetFooterTextFlags;
end;

{ TdxListViewGroupViewInfo }

destructor TdxListViewGroupViewInfo.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FSubtitle.Free;
  inherited Destroy;
end;

function TdxListViewGroupViewInfo.GetFocused: Boolean;
begin
  Result := ListView.Controller.MouseHoveredGroup = Group;
end;

function TdxListViewGroupViewInfo.GetHotTracked: Boolean;
begin
  Result := ListView.Controller.FocusedGroup = Group;
end;

function TdxListViewGroupViewInfo.HasExpandButton: Boolean;
begin
  Result := TdxListGroupOption.Collapsible in Group.Options;
end;

function TdxListViewGroupViewInfo.HasFooter: Boolean;
begin
  Result := HasHeader and (Group.Footer <> '');
end;

function TdxListViewGroupViewInfo.HasHeader: Boolean;
begin
  Result := not(TdxListGroupOption.NoHeader in Group.Options);
end;

function TdxListViewGroupViewInfo.HasSubtitle: Boolean;
begin
  Result := HasHeader and (Group.Subtitle <> '');
end;

function TdxListViewGroupViewInfo.GetClosed: Boolean;
begin
  Result := Group.Collapsed;
end;

function TdxListViewGroupViewInfo.GetPart(const P: TPoint;
out AItemViewInfo: TdxListItemCustomViewInfo): TdxListGroupPart;
begin
  Result := inherited GetPart(P, AItemViewInfo);
  if Result = TdxListGroupPart.Content then
  begin
    if ExpandButtonBounds.Contains(P) then
      Exit(TdxListGroupPart.ExpandButton);
    if HeaderHottrackBounds.Contains(P) then
      Exit(TdxListGroupPart.Header);
    if FooterBounds.Contains(P) then
      Exit(TdxListGroupPart.Footer);
  end;
end;

procedure TdxListViewGroupViewInfo.Invalidate;
begin
  ListView.InvalidateRect(HeaderHottrackBounds, True);
end;

procedure TdxListViewGroupViewInfo.BeforeDraw;
begin
  FHeaderState := GetHeaderState;
  FExpandButtonState := GetExpandButtonState;
end;

function TdxListViewGroupViewInfo.GetHeaderHeight: Integer;
begin
  Result := Owner.GroupHeaderHeight;
end;

function TdxListViewGroupViewInfo.GetHeaderParams: TdxListViewCellViewParams;
begin
  Result := Owner.GroupHeaderViewParams;
end;

function TdxListViewGroupViewInfo.GetFooterHeight: Integer;
begin
  Result := Owner.GroupFooterHeight;
end;

function TdxListViewGroupViewInfo.GetFooterParams: TdxListViewCellViewParams;
begin
  Result := Owner.GroupFooterViewParams;
end;

function TdxListViewGroupViewInfo.GetSubtitleHeight: Integer;
begin
  Result := Owner.GroupSubtitleHeight;
end;

function TdxListViewGroupViewInfo.GetSubtitleParams: TdxListViewCellViewParams;
begin
  Result := Owner.GroupSubtitleViewParams;
end;

procedure TdxListViewGroupViewInfo.CalculateContent(AType: TdxChangeType);
begin
  if HasHeader then
    CalculateHeader(AType);
  if not Closed then
    inherited CalculateContent(AType);
  if HasFooter then
    CalculateFooter(AType);
end;

function TdxListViewGroupViewInfo.CalculateContentBottom: Integer;
begin
  if HasFooter then
    Result := FooterBounds.Bottom
  else if Closed then
  begin
    if HasHeader then
      Result := HeaderHottrackBounds.Bottom
    else
      Result := FContentBounds.Top;
  end
  else
    Result := ItemsAreaBounds.Bottom + ContentPadding.Bottom;
end;

procedure TdxListViewGroupViewInfo.CalculateContentBounds;
begin
  inherited CalculateContentBounds;
  FContentBounds := Bounds;
  FContentBounds.Bottom := CalculateContentBottom;
  FContentBounds.Right := CalculateContentRight;
end;

function TdxListViewGroupViewInfo.CalculateContentRight: Integer;
begin
  if IsHorizontalItemsArrangement then
    Result := Bounds.Right
  else if Closed then
    Result := HeaderHottrackBounds.Right
  else
    Result := ItemsAreaBounds.Right + ContentPadding.Right;
end;

function TdxListViewGroupViewInfo.CalculateHeaderWidth: Integer;
begin
  if Owner.AreGroupsVertical then
    Result := Owner.GetAvailableGroupsAreaWidth
  else
  begin
    Result := ColumnCount * ItemSize.cx + (ColumnCount - 1) * GetItemsGap;
    Inc(Result, ContentPadding.Width);
  end;
  Result := Max(Result, ItemSize.cx);
end;

procedure TdxListViewGroupViewInfo.PopulateItems;
var
  I: Integer;
begin
  for I := 0 to Group.ItemCount - 1 do
    FItems.Add(Owner.CreateItemViewInfo(Self, Group.ItemIndices[I]));
end;

procedure TdxListViewGroupViewInfo.CalculateFooter(AType: TdxChangeType);
begin
  if Group.Footer <> '' then
  begin
    FFooterBounds := HeaderHottrackBounds;
    if Closed then
      FFooterBounds.Top := HeaderHottrackBounds.Bottom
    else
      FFooterBounds.Top := ItemsAreaBounds.Bottom;
    FFooterBounds.Height := Owner.GroupFooterHeight;
    if AType >= ctMedium then
    begin
      if Footer = nil then
        FFooter := TdxListViewGroupTextViewInfo.Create(Self, dxlgtFooter,
          FooterParams);
      Footer.Initialize(Group.Footer, -1);
    end;
    Footer.Calculate(AType, FooterBounds);
  end
  else
    FFooterBounds.Empty;
end;

procedure TdxListViewGroupViewInfo.CalculateHeader(AType: TdxChangeType);
var
  ASize: TSize;
  AOffset: Integer;
  ATextAreaBounds: TRect;
begin
  FHeaderHottrackBounds := Bounds;
  FHeaderHottrackBounds.Bottom := FHeaderHottrackBounds.Top +
    Owner.GroupHeaderHeight;
  FHeaderHottrackBounds.Width := CalculateHeaderWidth;

  ATextAreaBounds := HeaderHottrackBounds;
  if HasExpandButton then
  begin
    ASize := Owner.GroupExpandButtonSize;
    AOffset := (Owner.GroupHeaderHeight - ASize.cy) div 2;
    FExpandButtonBounds.InitSize(HeaderHottrackBounds.Left +
      HeaderParams.Padding.Left, HeaderHottrackBounds.Top + AOffset, ASize);
    ATextAreaBounds.Left := FExpandButtonBounds.Right +
      HeaderParams.GlyphIndent;
  end
  else
    FExpandButtonBounds.Empty;

  FHeaderLineBounds := ATextAreaBounds;
  Inc(FHeaderLineBounds.Top, ATextAreaBounds.Height div 2);
  FHeaderLineBounds.Bottom := FHeaderLineBounds.Top + 1;

  if Group.Header <> '' then
  begin
    FHeaderBounds := ATextAreaBounds;
    if AType >= ctMedium then
    begin
      if Header = nil then
        FHeader := TdxListViewGroupTextViewInfo.Create(Self, dxlgtHeader,
          HeaderParams);
      Header.Initialize(Group.Header, Group.TitleImage);
    end;
    Header.Calculate(AType, FHeaderBounds);
    FHeaderLineBounds.Deflate(HeaderParams.Padding.Left, 0,
      HeaderParams.Padding.Right, 0);
    case Group.HeaderAlign of
      taLeftJustify:
        FHeaderLineBounds.Left :=
          Min(Header.TextBounds.Left + Header.TextLayout.MeasureSize.cx +
          HeaderParams.Padding.Left, Bounds.Right);
      taRightJustify:
        FHeaderLineBounds.Right :=
          Max(Header.TextBounds.Right - (Header.TextLayout.MeasureSize.cx +
          HeaderParams.Padding.Right), Bounds.Left);
    end;
  end
  else
    FHeaderBounds.Empty;
  if Group.Subtitle <> '' then
  begin
    FSubtitleBounds := ATextAreaBounds;
    FSubtitleBounds.Offset(0, ATextAreaBounds.Height);
    FSubtitleBounds.Height := Owner.GroupSubtitleHeight;
    if AType >= ctMedium then
    begin
      if Subtitle = nil then
        FSubtitle := TdxListViewGroupTextViewInfo.Create(Self, dxlgtSubtitle,
          SubtitleParams);
      Subtitle.Initialize(Group.Subtitle, -1);
    end;
    Subtitle.Calculate(AType, FSubtitleBounds);
    Inc(FHeaderHottrackBounds.Bottom, Owner.GroupSubtitleHeight);
  end
  else
    FSubtitleBounds.Empty;
end;

function TdxListViewGroupViewInfo.CalculateHeaderLeft: Integer;
begin
  Result := Max(ListView.ClientBounds.Left, Bounds.Left);
end;

procedure TdxListViewGroupViewInfo.CalculateItemsArea;
begin
  FItemsAreaBounds := Bounds;
  if HasHeader then
    FItemsAreaBounds.Top := FHeaderHottrackBounds.Bottom;
  FItemsAreaBounds.Deflate(ContentPadding);
end;

procedure TdxListViewGroupViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  if HasHeader then
    Painter.DrawGroupHeader(ACanvas, Self);
  if not Closed then
    inherited DrawContent(ACanvas);
end;

function TdxListViewGroupViewInfo.GetAvailableHeightForItems: Integer;
begin
  Result := inherited GetAvailableHeightForItems;
  if HasHeader then
    Dec(Result, HeaderHeight + ContentPadding.Height);
  if HasSubtitle then
    Dec(Result, SubtitleHeight);
  if HasFooter then
    Dec(Result, FooterHeight);
end;

function TdxListViewGroupViewInfo.GetAvailableWidthForItems: Integer;
begin
  Result := inherited GetAvailableWidthForItems;
  Dec(Result, ContentPadding.Width);
end;

function TdxListViewGroupViewInfo.GetContentPadding: TdxPadding;
begin
  Result := Owner.GroupContentPadding;
end;

function TdxListViewGroupViewInfo.GetExpandButtonState: TcxButtonState;
begin
  if HotTrackPart = TdxListGroupPart.ExpandButton then
  begin
    if GetAsyncKeyState(VK_LBUTTON) < 0 then
      Result := cxbsPressed
    else
      Result := cxbsHot;
  end
  else
    Result := cxbsNormal;
end;

function TdxListViewGroupViewInfo.GetFooterTextFlags: Integer;
begin
  Result := HeaderParams.TextFlags or HorizontalAlignmentMap[Group.FooterAlign];
end;

function TdxListViewGroupViewInfo.GetHeaderState: TdxListViewGroupHeaderStates;
begin
  Result := [];
  if HotTracked then
    Include(Result, dxlgsHot);
  if Focused then
    Include(Result, dxlgsFocused);
  if Group.Collapsed then
    Include(Result, dxlgsCollapsed);
  if not ListView.Focused then
    Include(Result, dxlgsInactive);
end;

function TdxListViewGroupViewInfo.GetHeaderTextFlags: Integer;
begin
  Result := HeaderParams.TextFlags or HorizontalAlignmentMap[Group.HeaderAlign];
end;

procedure TdxListViewGroupViewInfo.InvalidatePart(APart: TdxListGroupPart);
begin
  case APart of
    TdxListGroupPart.ExpandButton:
      ListView.InvalidateRect(ExpandButtonBounds, True);
  end;
end;

{ TdxListViewHintHelper }

constructor TdxListViewHintHelper.Create(AViewInfo: TdxListViewViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

procedure TdxListViewHintHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited CorrectHintWindowRect(ARect);
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

function TdxListViewHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FViewInfo.ListView;
end;

procedure TdxListViewHintHelper.MouseDown;
begin
  CancelHint;
end;

function TdxListViewHintHelper.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(HintAreaBounds, P);
end;

{ TdxListViewViewInfo }

constructor TdxListViewViewInfo.Create(AListView: TdxCustomListView);
begin
  inherited Create(AListView);
  FHintHelper := TdxListViewHintHelper.Create(Self);
  FGroups := TdxFastObjectList.Create;
  FDropTarget := CreateDropTargetViewInfo;
  FItems := TdxFastList.Create(1024);
  FItemViewParams := TdxListViewCellViewParams.Create;
  FGroupHeaderViewParams := TdxListViewCellViewParams.Create;
  FGroupFooterViewParams := TdxListViewCellViewParams.Create;
  FGroupSubtitleViewParams := TdxListViewCellViewParams.Create;
end;

destructor TdxListViewViewInfo.Destroy;
begin
  FHintHelper.Free;
  FreeAndNil(FColumnHeadersViewInfo);
  FreeAndNil(FDropTarget);
  FreeAndNil(FGroups);
  FreeAndNil(FItemViewParams);
  FreeAndNil(FGroupHeaderViewParams);
  FreeAndNil(FGroupFooterViewParams);
  FreeAndNil(FGroupSubtitleViewParams);
  FItems.Free;
  inherited Destroy;
end;

function TdxListViewViewInfo.GetContentOffset: TRect;
begin
  Result := Options.PaddingOptions.View.Margin;
end;

function TdxListViewViewInfo.GetItem(Index: Integer): TdxListItemCustomViewInfo;
begin
  Result := TdxListItemCustomViewInfo(FItems.List[Index]);
end;

function TdxListViewViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxListViewViewInfo.Calculate(AType: TdxChangeType;
const ABounds: TRect);
begin
  inherited Calculate(AType, ABounds);
  FItems.Clear;
  if AType = ctHard then
  begin
    CalculateCommonParameters;
    RecreateSubItems;
  end;
  if AType <> ctLight then
    CalculateItemSize;
  CalculateContent(AType);
  CalculateContentBounds;
end;

function TdxListViewViewInfo.FindGroupViewInfo(AGroup: TdxListGroup;
out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
  begin
    AGroupViewInfo := Groups[I];
    if AGroupViewInfo.Group = AGroup then
    begin
      AViewInfo := AGroupViewInfo;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxListViewViewInfo.FindGroupViewInfo(AItemIndex: Integer;
out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  I: Integer;
begin
  if not Options.GroupView then
  begin
    AViewInfo := Groups[0];
    Exit(True);
  end
  else
  begin
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      if AGroupViewInfo.Group.ItemIndices.Contains(AItemIndex) then
      begin
        AViewInfo := AGroupViewInfo;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TdxListViewViewInfo.FindGroupViewInfo(AItem: TdxListItem;
out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  I: Integer;
begin
  if not Options.GroupView then
  begin
    AViewInfo := Groups[0];
    Exit(True);
  end
  else
  begin
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      if AGroupViewInfo.Group = AItem.Group then
      begin
        AViewInfo := AGroupViewInfo;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TdxListViewViewInfo.FindItemViewInfo(AItemIndex: Integer;
out AViewInfo: TdxListItemCustomViewInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
  begin
    AViewInfo := Items[I];
    if AViewInfo.ItemIndex = AItemIndex then
      Exit(True);
  end;
  AViewInfo := nil;
end;

function TdxListViewViewInfo.GetGroupAtPos(const P: TPoint;
out AViewInfo: TdxListViewCustomGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
  begin
    AGroupViewInfo := Groups[I];
    if cxRectPtIn(AGroupViewInfo.ContentBounds, P) then
    begin
      AViewInfo := AGroupViewInfo;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxListViewViewInfo.GetItemAtPos(const P: TPoint;
out AViewInfo: TdxListItemCustomViewInfo): Boolean;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
begin
  Result := GetGroupAtPos(P, AGroupViewInfo) and AGroupViewInfo.GetItemAtPos(P,
    AViewInfo);
end;

function TdxListViewViewInfo.AreGroupsVertical: Boolean;
begin
  case ListView.ViewStyle of
    TdxListViewStyle.Icon, TdxListViewStyle.SmallIcon:
      Result := Options.Icons.Arrangement = TdxListIconsArrangement.Horizontal;
    TdxListViewStyle.List:
      Result := False;
  else
    Result := True;
  end;
end;

procedure TdxListViewViewInfo.CalculateContent(AType: TdxChangeType);
var
  I, AHeaderHeight: Integer;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  AGroupAreaBounds, AHeadersBounds: TRect;
begin
  if ListView.HandleAllocated then
  begin
    if ColumnHeadersViewInfo <> nil then
      AHeaderHeight := ColumnHeadersViewInfo.CalculateHeight
    else
      AHeaderHeight := 0;
    FContentBounds.Init(Bounds.Left - ListView.LeftPos + ContentOffset.Left,
      Bounds.Top - ListView.TopPos + ContentOffset.Top + AHeaderHeight,
      Bounds.Right - ContentOffset.Right, Bounds.Bottom - ContentOffset.Bottom);
    if ColumnHeadersViewInfo <> nil then
    begin
      AHeadersBounds.Init(FContentBounds.Left, Bounds.Top + ContentOffset.Top,
        FContentBounds.Right, Bounds.Top + ContentOffset.Top);
      ColumnHeadersViewInfo.Calculate(AType, AHeadersBounds);
    end;
    if FContentBounds.IsEmpty then
      Exit;
    AGroupAreaBounds := ContentBounds;
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      if AGroupViewInfo.IsHorizontalItemsArrangement then
        AGroupAreaBounds.Bottom := MaxInt
      else if not IsReportView then
        AGroupAreaBounds.Right := MaxInt;
      AGroupViewInfo.Calculate(AType, AGroupAreaBounds);
      if AreGroupsVertical then
        AGroupAreaBounds.Offset(0, AGroupViewInfo.ContentBounds.Height)
      else
        AGroupAreaBounds.Offset(AGroupViewInfo.ContentBounds.Width, 0);
    end;
  end;
end;

procedure TdxListViewViewInfo.CalculateContentBounds;
var
  I: Integer;
  AGroupContentBounds: TRect;
  ABottomRight: TPoint;
begin
  if not ListView.HandleAllocated then
    FContentBounds.Empty
  else
  begin
    ABottomRight.Init(0, 0);
    for I := 0 to GroupCount - 1 do
    begin
      AGroupContentBounds := Groups[I].ContentBounds;
      ABottomRight.Y := Max(ABottomRight.Y, AGroupContentBounds.Bottom);
      ABottomRight.X := Max(ABottomRight.X, AGroupContentBounds.Right);
    end;
    FContentBounds.BottomRight := ABottomRight;
    if ColumnHeadersViewInfo <> nil then
      Inc(FContentBounds.Bottom, ColumnHeadersViewInfo.Bounds.Height);
  end;
end;

procedure TdxListViewViewInfo.CalculateCommonParameters;
var
  I, AWidth: Integer;
begin
  FGroupContentPadding := Options.PaddingOptions.GroupContent.Margin;
  CalculateViewParams;
  FColumnHeadersBounds.Empty;
  AWidth := 0;
  for I := 0 to ListView.Columns.Count - 1 do
    Inc(AWidth, ListView.Columns[I].Width);
  FColumnHeadersBounds.Width := AWidth;
end;

function TdxListViewViewInfo.CalculateImagesGlyphSize
  (AImages: TCustomImageList): TSize;
begin
  if AImages <> nil then
  begin
    Result.Init(AImages.Width, AImages.Height);
    if Options.ScaleImagesForDPI then
      Result := ScaleFactor.Apply(Result);
  end
  else
    Result.Init(0, 0);
end;

function TdxListViewViewInfo.CalculateItemStateGlyphSize: TSize;
begin
  case GetItemStateViewKind of
    TdxListItemStateViewKind.CheckBox:
      Result := Painter.LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor);
    TdxListItemStateViewKind.Glyph:
      Result := CalculateImagesGlyphSize(ItemViewParams.StateImages);
  else
    Result.Init(0, 0);
  end;
end;

function TdxListViewViewInfo.CalculateItemGlyphsAreaSize: TSize;
begin
  Result := ItemViewParams.GlyphSize;

  if ListView.ViewStyle = TdxListViewStyle.Icon then
    Inc(Result.cx, 2 * ScaleFactor.Apply(GetIconsGlyphSideGap));

  if ItemViewParams.StateGlyphSize.IsZero then
    Exit;
  if IsOverlappedItemStateGlyph then
  begin
  end
  else
  begin
    if (Result.cx > 0) and (ItemViewParams.StateGlyphSize.cx > 0) then
      Inc(Result.cx, ItemViewParams.GlyphIndent);
    Inc(Result.cx, ItemViewParams.StateGlyphSize.cx);
  end;
  Result.cy := Max(Result.cy, ItemViewParams.StateGlyphSize.cy);
end;

function TdxListViewViewInfo.CalculateItemTextSize: TSize;
begin
  case ListView.ViewStyle of
    TdxListViewStyle.SmallIcon:
      Result.cx := Options.Icons.SmallIconsColumnWidth -
        ItemViewParams.GetNonTextWidth;
    TdxListViewStyle.Icon:
      Result.cx := Max(1, FItemViewParams.Padding.Width);
    TdxListViewStyle.List:
      Result.cx := Options.List.ColumnWidth - ItemViewParams.GetNonTextWidth;
  end;
  Result.cy := TdxTextMeasurer.TextLineHeight(ListView.Fonts.Item) *
    Max(1, FItemViewParams.TextLineCount);
end;

function TdxListViewViewInfo.GetItemStateViewKind: TdxListItemStateViewKind;
begin
  if Options.ShowCheckBoxes then
    Result := TdxListItemStateViewKind.CheckBox
  else if ListView.Images.StateImages <> nil then
    Result := TdxListItemStateViewKind.Glyph
  else
    Result := TdxListItemStateViewKind.None;
end;

function TdxListViewViewInfo.GetScrollArea: TRect;
begin
  Result := GetGroupVisibleBounds;
end;

function TdxListViewViewInfo.GetGroupVisibleBounds: TRect;
begin
  Result := ListView.ClientBounds;
  if ColumnHeadersViewInfo <> nil then
    Result.Top := ColumnHeadersViewInfo.Bounds.Bottom;
end;

function TdxListViewViewInfo.IsOverlappedItemStateGlyph: Boolean;
begin
  Result := False;
end;

procedure TdxListViewViewInfo.CalculateItemSize;
var
  ATextAreaSize: TSize;
begin
  ATextAreaSize := CalculateItemTextSize;
  if ItemViewParams.TextPosition in [posTop, posBottom] then
  begin
    FItemSize.Init(Max(ATextAreaSize.cx, ItemViewParams.GlyphsAreaSize.cx),
      ATextAreaSize.cy + ItemViewParams.GlyphIndent +
      ItemViewParams.GlyphsAreaSize.cy);
  end
  else
  begin
    FItemSize.Init(ATextAreaSize.cx + ItemViewParams.GlyphIndent +
      ItemViewParams.GlyphsAreaSize.cx, Max(ATextAreaSize.cy,
      ItemViewParams.GlyphsAreaSize.cy));
  end;
  Inc(FItemSize.cx, ItemViewParams.Padding.Width);
  Inc(FItemSize.cy, ItemViewParams.Padding.Height);
  if IsReportView then
    if ListView.Columns.Count > 0 then
      FItemSize.cx := FColumnHeadersBounds.Width
    else
      FItemSize.cx := TdxListColumn.DefaultColumnWidth;
  Assert(FItemSize.cx > 0);
end;

procedure TdxListViewViewInfo.CalculateCommonViewParams
  (AViewParams: TdxListViewCellViewParams; APadding: TdxPadding; AFont: TFont;
AImages: TCustomImageList);
begin
  AViewParams.RightToLeftAlignmet := UseRightToLeftAlignment;

  AViewParams.Padding := APadding;
  AViewParams.Font.Free;
  AViewParams.Font := Painter.CreateCanvasBasedFont(AFont);
  AViewParams.Images := AImages;
  AViewParams.GlyphSize := CalculateImagesGlyphSize(AImages);

  AViewParams.GlyphIndent := ScaleFactor.Apply(GetDefaultItemGlyphIndent);
  AViewParams.StateViewKind := TdxListItemStateViewKind.None;
end;

procedure TdxListViewViewInfo.CalculateItemViewParams
  (AViewParams: TdxListViewCellViewParams);
var
  AItemPadding: TdxPadding;
  APadding: TRect;
begin
  APadding := LookAndFeelPainter.GetListViewItemContentPadding;
  AItemPadding := Options.PaddingOptions.Item.Margin;
  AItemPadding.InflatePadding(APadding, UseRightToLeftAlignment);
  CalculateCommonViewParams(AViewParams, APadding, ListView.Fonts.Item,
    GetItemGlyphImages);
  AViewParams.StateViewKind := GetItemStateViewKind;
  AViewParams.StateImages := ListView.Images.StateImages;
  AViewParams.StateGlyphSize := CalculateItemStateGlyphSize;
  AViewParams.GlyphsAreaSize := CalculateItemGlyphsAreaSize;
  AViewParams.TextFlags := GetItemTextFlags;
  AViewParams.TextPosition := GetItemTextPosition;
  AViewParams.TextLineCount := GetItemTextLineCount;
end;

procedure TdxListViewViewInfo.CalculateGroupCommonViewParams
  (AViewParams: TdxListViewCellViewParams; AFont: TFont;
AImages: TCustomImageList; AUsePainterPadding: Boolean);
var
  AGroupPadding: TdxPadding;
  APadding: TRect;
begin
  if AUsePainterPadding then
  begin
    APadding := LookAndFeelPainter.GetListViewItemContentPadding;
    AGroupPadding := Options.PaddingOptions.GroupHeader.Margin;
    AGroupPadding.InflatePadding(APadding, UseRightToLeftAlignment);
  end
  else
    APadding := Options.PaddingOptions.GroupHeader.Margin;
  CalculateCommonViewParams(AViewParams, APadding, AFont, AImages);
  AViewParams.TextFlags := CXTO_PREVENT_LEFT_EXCEED or
    CXTO_PREVENT_TOP_EXCEED or CXTO_END_ELLIPSIS or CXTO_SINGLELINE or
    CXTO_CENTER_VERTICALLY;
  AViewParams.TextPosition := posRight;
  AViewParams.TextLineCount := 1;
end;

procedure TdxListViewViewInfo.CalculateGroupHeaderViewParams
  (AViewParams: TdxListViewCellViewParams);
begin
  CalculateGroupCommonViewParams(AViewParams, ListView.Fonts.GroupHeader,
    ListView.Images.GroupHeaderImages, True);
  AViewParams.GlyphsAreaSize := AViewParams.GlyphSize;
end;

procedure TdxListViewViewInfo.CalculateGroupSubtitleViewParams
  (AViewParams: TdxListViewCellViewParams);
begin
  CalculateGroupCommonViewParams(AViewParams, ListView.Fonts.GroupSubtitle,
    nil, False);
end;

procedure TdxListViewViewInfo.CalculateGroupFooterViewParams
  (AViewParams: TdxListViewCellViewParams);
begin
  CalculateGroupCommonViewParams(AViewParams, ListView.Fonts.GroupFooter,
    nil, False);
end;

procedure TdxListViewViewInfo.CalculateGroupHeaderSizes;
var
  AExtraHeight: Integer;
begin
  AExtraHeight := GroupHeaderViewParams.Padding.Height;

  FGroupExpandButtonSize.Init(LookAndFeelPainter.GetListViewExpandButtonSize
    (ScaleFactor));
  FGroupHeaderHeight := TdxTextMeasurer.TextLineHeight
    (ListView.Fonts.GroupHeader) + AExtraHeight;
  FGroupHeaderHeight := Max(FGroupHeaderHeight, GroupExpandButtonSize.cy +
    AExtraHeight);

  AExtraHeight := Options.PaddingOptions.GroupHeader.Top +
    Options.PaddingOptions.GroupHeader.Bottom;

  FGroupSubtitleHeight := TdxTextMeasurer.TextLineHeight
    (ListView.Fonts.GroupSubtitle);
  Inc(FGroupSubtitleHeight, AExtraHeight);

  FGroupFooterHeight := TdxTextMeasurer.TextLineHeight
    (ListView.Fonts.GroupFooter);
  Inc(FGroupFooterHeight, AExtraHeight);
end;

procedure TdxListViewViewInfo.CalculateGroupViewParams;
begin
  CalculateGroupHeaderViewParams(FGroupHeaderViewParams);
  CalculateGroupSubtitleViewParams(FGroupSubtitleViewParams);
  CalculateGroupFooterViewParams(FGroupFooterViewParams);
  CalculateGroupHeaderSizes;
end;

procedure TdxListViewViewInfo.CalculateViewParams;
begin
  CalculateItemViewParams(FItemViewParams);
  if Options.GroupView then
    CalculateGroupViewParams;
  if ColumnHeadersViewInfo <> nil then
    ColumnHeadersViewInfo.CalculateViewParams;
end;

function TdxListViewViewInfo.CreateDropTargetViewInfo
  : TdxListViewDropTargetViewInfo;
begin
  Result := TdxListViewDropTargetViewInfo.Create(ListView);
end;

function TdxListViewViewInfo.CreateGroupViewInfo(AGroup: TdxListGroup)
  : TdxListViewGroupViewInfo;
begin
  Result := TdxListViewGroupViewInfo.Create(Self, AGroup);
end;

function TdxListViewViewInfo.CreateItemViewInfo
  (AOwner: TdxListViewCustomGroupViewInfo; AItemIndex: Integer)
  : TdxListItemCustomViewInfo;
begin
  if IsReportView then
    Result := TdxListItemReportStyleViewInfo.Create(AOwner, AItemIndex,
      FItemViewParams)
  else
    Result := TdxListItemViewInfo.Create(AOwner, AItemIndex, FItemViewParams);
end;

procedure TdxListViewViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  I: Integer;
  AContentDrawingBounds: TRect;
begin
  Painter.DrawBackground(ACanvas, Bounds, ExplorerStyle);
  if ColumnHeadersViewInfo <> nil then
  begin
    ColumnHeadersViewInfo.Draw(ACanvas);
    AContentDrawingBounds := ListView.ClientBounds;
    AContentDrawingBounds.Top := ColumnHeadersViewInfo.Bounds.Bottom;
    ACanvas.IntersectClipRect(AContentDrawingBounds);
  end;
  for I := 0 to GroupCount - 1 do
    Groups[I].Draw(ACanvas);
  DropTarget.Draw(ACanvas);
end;

function TdxListViewViewInfo.FindColumn(APosition: TPoint): TdxListColumn;
var
  AHeaderViewInfo: TdxListColumnHeaderViewInfo;
begin
  if ColumnHeadersViewInfo <> nil then
  begin
    AHeaderViewInfo := ColumnHeadersViewInfo.FindItem(APosition);
    if AHeaderViewInfo <> nil then
      Exit(AHeaderViewInfo.Column);
  end;
  Result := nil;
end;

function TdxListViewViewInfo.GetAvailableGroupsAreaHeight: Integer;
var
  AOffset: Integer;
begin
  AOffset := cxMarginsHeight(BorderWidths);
  if not ListView.IsTouchScrollUIMode then
  begin
    if ListView.IsScrollBarActive(sbHorizontal) then
      Inc(AOffset, ListView.GetHScrollBarDefaultAreaHeight);
  end;
  Result := ListView.Height - AOffset;
end;

function TdxListViewViewInfo.GetAvailableGroupsAreaWidth: Integer;
var
  AOffset: Integer;
begin
  AOffset := cxMarginsWidth(BorderWidths);
  if not ListView.IsTouchScrollUIMode then
  begin
    if ListView.IsScrollBarActive(sbVertical) then
      Inc(AOffset, ListView.GetVScrollBarDefaultAreaWidth);
  end;
  Result := ListView.Width - AOffset;
end;

function TdxListViewViewInfo.GetBorderWidths: TRect;
var
  ABorderSize: Integer;
begin
  Result := ContentOffset;
  ABorderSize := ListView.BorderSize;
  Inc(Result.Bottom, ABorderSize);
  Inc(Result.Left, ABorderSize);
  Inc(Result.Right, ABorderSize);
  Inc(Result.Top, ABorderSize);
end;

function TdxListViewViewInfo.GetBoundsForItem(AItem: TdxListItem): TRect;
begin
  Result := GetBoundsForItem(AItem.Index);
end;

function TdxListViewViewInfo.GetBoundsForItem(AItemIndex: Integer): TRect;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
begin
  if not FindGroupViewInfo(AItemIndex, AGroupViewInfo) then
    Result.Empty
  else
    Result := AGroupViewInfo.GetBoundsForItem(AItemIndex);
end;

function TdxListViewViewInfo.GetDefaultItemGlyphIndent: Integer;
begin
  Result := 3;
end;

function TdxListViewViewInfo.GetIconsGlyphSideGap: Integer;
begin
  Result := 20;
end;

function TdxListViewViewInfo.GetItemGlyphImages: TCustomImageList;
begin
  if ListView.ViewStyle = TdxListViewStyle.Icon then
    Result := ListView.Images.LargeImages
  else
    Result := ListView.Images.SmallImages;
end;

function TdxListViewViewInfo.GetItemsOffset: TSize;
var
  APadding: TdxListViewPaddingOptions;
  AGroupPadding: TRect;
begin
  Result.Init(0, 0);
  APadding := ListView.Options.PaddingOptions;
  if IsGroupView then
    AGroupPadding := GroupContentPadding
  else
    AGroupPadding.Empty;
  if UseRightToLeftAlignment then
  begin
    Inc(Result.cx, APadding.View.Margin.Right);
    Inc(Result.cx, AGroupPadding.Right);
  end
  else
  begin
    Inc(Result.cx, APadding.View.Margin.Left);
    Inc(Result.cx, AGroupPadding.Left);
  end;
  Inc(Result.cy, APadding.View.Margin.Top);
  Inc(Result.cy, AGroupPadding.Top);
end;

function TdxListViewViewInfo.GetItemTextLineCount: Integer;
begin
  if ListView.ViewStyle = TdxListViewStyle.Icon then
    Result := Options.Icons.TextLineCount
  else
    Result := 1;
end;

function TdxListViewViewInfo.GetItemTextFlags: Integer;
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or
    CXTO_END_ELLIPSIS;
  if ListView.ViewStyle = TdxListViewStyle.Icon then
    Result := Result or CXTO_CENTER_HORIZONTALLY or CXTO_WORDBREAK or
      CXTO_CHARBREAK
  else
    Result := Result or CXTO_SINGLELINE or CXTO_LEFT or CXTO_CENTER_VERTICALLY;
end;

function TdxListViewViewInfo.GetItemTextPosition: TcxPosition;
begin
  if ListView.ViewStyle = TdxListViewStyle.Icon then
    Result := posBottom
  else
    Result := posRight;
end;

procedure TdxListViewViewInfo.RecreateSubItems;
var
  AGroup: TdxListGroup;
  AGroups: TdxListGroups;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  I: Integer;
begin
  ValidateColumnHeadersViewInfo;
  FGroups.Clear;
  AGroups := ListView.Groups;
  if Options.GroupView then
  begin
    FGroups.Capacity := FGroups.Count;
    for I := 0 to AGroups.Count - 1 do
    begin
      AGroup := AGroups[I];
      if AGroup.IsVisible then
      begin
        AGroupViewInfo := CreateGroupViewInfo(AGroup);
        AGroupViewInfo.PopulateItems;
        FGroups.Add(AGroupViewInfo);
      end;
    end;
  end
  else
  begin
    AGroupViewInfo := TdxListViewRootItemsLayoutViewInfo.Create(Self, nil);
    AGroupViewInfo.PopulateItems;
    FGroups.Add(AGroupViewInfo);
  end;
end;

procedure TdxListViewViewInfo.ValidateColumnHeadersViewInfo;
begin
  if IsReportView and Options.Report.ShowColumsHeaders then
  begin
    if FColumnHeadersViewInfo = nil then
      FColumnHeadersViewInfo := TdxListViewColumnHeadersViewInfo.Create(Self);
  end
  else
    FreeAndNil(FColumnHeadersViewInfo);
end;

function TdxListViewViewInfo.CanStartMultiSelectionByMouse(const AMousePos
  : TPoint): Boolean;
var
  AItemViewInfo: TdxListItemCustomViewInfo;
begin
  if (ColumnHeadersViewInfo <> nil) and ColumnHeadersViewInfo.Bounds.Contains
    (AMousePos) then
    Exit(False);
  if GetItemAtPos(AMousePos, AItemViewInfo) then
    Exit(AItemViewInfo.StartMultiSelection([], AMousePos));
  Result := True;
end;

procedure TdxListViewViewInfo.CheckHint(AElementViewInfo
  : TdxListViewCustomViewInfo);
var
  AHint: string;
  AHintBounds: TRect;
begin
  AHint := AElementViewInfo.GetHint;
  AHintBounds := AElementViewInfo.GetHintBounds;
  if (AHint <> '') and ListView.ShowHint and cxCanShowHint(ListView) then
    HintHelper.ShowHint(AHintBounds, AHintBounds, AHint, False,
      AElementViewInfo);
end;

procedure TdxListViewViewInfo.MouseLeave;
begin
  if ColumnHeadersViewInfo <> nil then
    ColumnHeadersViewInfo.MouseLeave;
  HintHelper.MouseLeave;
end;

procedure TdxListViewViewInfo.CalculateColumnBestFitParams
  (AColumn: TdxListColumn; var ACheckState, ACheckGlyph: Boolean);
var
  ASubIndex: Integer;
begin
  ASubIndex := AColumn.FSubItemIndex;
  if ASubIndex < 0 then
  begin
    ACheckState := not((AColumn.Index > 0) and
      Options.Report.AlwaysShowItemImageInFirstColumn);
    ACheckGlyph := ACheckState;
  end
  else
  begin
    if (AColumn.Index = 0) and Options.Report.AlwaysShowItemImageInFirstColumn
    then
    begin
      ACheckState := True;
      ACheckGlyph := True;
    end
    else
    begin
      ACheckState := False;
      ACheckGlyph := True;
    end;
  end;
end;

function TdxListViewViewInfo.CalculateColumnHeaderBestFitWidth
  (AColumn: TdxListColumn): Integer;
var
  ANonTextWidth: Integer;
  AParams: TdxListViewCellViewParams;
begin
  AParams := ColumnHeadersViewInfo.ViewParams;
  ANonTextWidth := AParams.Padding.Width;
  if (AParams.GlyphSize.cx > 0) and (AColumn.ImageIndex >= 0) then
    Inc(ANonTextWidth, AParams.GlyphSize.cx + AParams.GlyphIndent);
  if AColumn.SortOrder <> soNone then
    Inc(ANonTextWidth, ColumnHeadersViewInfo.SortArrowSize.X +
      AParams.Padding.Left);
  Result := TdxTextMeasurer.TextWidthTO(ListView.Fonts.ColumnHeader,
    AColumn.Caption) + ANonTextWidth;
end;

function TdxListViewViewInfo.CalculateColumnValueWidth(AItem: TdxListItem;
ASubIndex: Integer; ACheckState, ACheckGlyph: Boolean): Integer;
var
  AValue: string;
  AImageIndex, ANonTextWidth: Integer;
begin
  if ASubIndex < 0 then
    AValue := AItem.Caption
  else
  begin
    if ASubIndex < AItem.SubItems.Count then
    begin
      AValue := AItem.SubItems[ASubIndex];
      AImageIndex := AItem.SubItemImages[ASubIndex];
    end
    else
    begin
      AValue := '';
      AImageIndex := -1;
    end;
    if ACheckGlyph and not ACheckState then
      ACheckGlyph := AImageIndex >= 0;
  end;
  ANonTextWidth := ItemViewParams.GetReportNonTextWidth(ACheckState,
    ACheckGlyph);
  Result := TdxTextMeasurer.TextWidthTO(ListView.Fonts.Item, AValue) +
    ANonTextWidth;
end;

function TdxListViewViewInfo.CalculateColumnBestFitWidth
  (AColumn: TdxListColumn): Integer;
var
  AItem: TdxListItem;
  ACheckState, ACheckGlyph: Boolean;
  I: Integer;
begin
  Result := CalculateColumnHeaderBestFitWidth(AColumn);
  CalculateColumnBestFitParams(AColumn, ACheckState, ACheckGlyph);
  for I := 0 to ListView.Items.Count - 1 do
  begin
    AItem := ListView.GetItem(I);
    Result := Max(Result, CalculateColumnValueWidth(AItem,
      AColumn.FSubItemIndex, ACheckState, ACheckGlyph));
  end;
end;

function TdxListViewViewInfo.GetColumnAutoWidth: Boolean;
begin
  Result := Options.ColumnAutoWidth;
end;

function TdxListViewViewInfo.GetGroup(Index: Integer)
  : TdxListViewCustomGroupViewInfo;
begin
  Result := TdxListViewCustomGroupViewInfo(FGroups.List[Index]);
end;

function TdxListViewViewInfo.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

{ TdxListViewController.TSelectAnchorInfo }

constructor TdxListViewController.TSelectAnchorInfo.Create(AGroupID,
  AItemIndex: Integer);
begin
  GroupID := AGroupID;
  ItemIndex := AItemIndex;
end;

function TdxListViewController.TSelectAnchorInfo.IsNull: Boolean;
begin
  Result := ItemIndex < 0;
end;

procedure TdxListViewController.TSelectAnchorInfo.Reset;
begin
  GroupID := -1;
  ItemIndex := -1;
end;

{ TdxListViewController }

constructor TdxListViewController.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FFocusedItemIndex := -1;
  FEditingItemIndex := -1;
  FMouseHoveredItemIndex := -1;
  FSelectedIndices := TdxIntegerList.Create;
end;

destructor TdxListViewController.Destroy;
begin
  FreeAndNil(FSelectedIndices);
  FinishEditingTimer;
  FinishSelectGroupItemsTimer;
  inherited Destroy;
end;

procedure TdxListViewController.FocusEnter;
begin
  ListView.Invalidate;
end;

procedure TdxListViewController.FocusLeave;
begin
  ListView.Invalidate;
end;

procedure TdxListViewController.KeyDown(AKey: Word; AShift: TShiftState);
var
  AItem: TdxListItem;
  AItemIndex: Integer;
begin
  case AKey of
    VK_SHIFT:
      if MultiSelect and (AShift = [ssShift]) and FSelectAnchor.IsNull then
      begin
        AItemIndex := GetStartItemIndexForKeyboardNavigation;
        if AItemIndex < 0 then
          Exit;
        AItem := ListView.GetItem(AItemIndex);
        FSelectAnchor := TSelectAnchorInfo.Create(AItem.GroupID, AItemIndex);
      end;
    VK_RIGHT:
      if FocusedGroup <> nil then
      begin
        if FocusedGroup.IsCollapsible and FocusedGroup.Collapsed then
          FocusedGroup.Collapsed := False;
      end
      else
        SelectNextItem(GetStartItemIndexForKeyboardNavigation,
          IfThen(ListView.UseRightToLeftAlignment, -1, 1), 0, AShift);
    VK_LEFT:
      if FocusedGroup <> nil then
      begin
        if FocusedGroup.IsCollapsible and not FocusedGroup.Collapsed then
          FocusedGroup.Collapsed := True;
      end
      else
        SelectNextItem(GetStartItemIndexForKeyboardNavigation,
          IfThen(ListView.UseRightToLeftAlignment, 1, -1), 0, AShift);
    VK_SPACE:
      if (AShift = [ssCtrl]) and (FocusedItemIndex >= 0) then
        if MultiSelect then
          SelectItem(FocusedItemIndex,
            not SelectedIndices.Contains(FocusedItemIndex))
        else
        begin
          ClearSelection(FocusedItemIndex);
          SelectItem(FocusedItemIndex, True);
        end;
    VK_UP:
      SelectNextItem(GetStartItemIndexForKeyboardNavigation, 0, -1, AShift);
    VK_DOWN:
      SelectNextItem(GetStartItemIndexForKeyboardNavigation, 0, 1, AShift);
    VK_PRIOR:
      ShowPriorPage(AShift);
    VK_NEXT:
      ShowNextPage(AShift);
    VK_HOME:
      GotoFirstFocusibleItem(AShift);
    VK_END:
      GotoLastFocusibleItem(AShift);
    VK_ESCAPE:
      if (FResizingColumn <> nil) and not ListView.Options.ExplorerStyle then
      begin
        FResizingColumn.Width := FOriginalResizingColumnWidth;
        FResizingColumn := nil;
        SetCapture(0);
      end;
    VK_F2:
      if AShift = [] then
        ListView.StartItemCaptionEditing(FocusedItemIndex);
  end;
end;

procedure TdxListViewController.KeyUp(AKey: Word; AShift: TShiftState);
begin
end;

procedure TdxListViewController.CancelMode;
begin
  ListView.FinishItemCaptionEditing;
  PressedColumn := nil;
  FResizingColumn := nil;
end;

procedure TdxListViewController.Click;
begin
  if FSelectItemsGroup <> nil then
    StartSelectGroupItemsTimer;
end;

procedure TdxListViewController.DblClick;
var
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  AGroupPart: TdxListGroupPart;
  AGroup: TdxListGroup;
  AItemViewInfo: TdxListItemCustomViewInfo;
  AMousePos: TPoint;
begin
  FinishSelectGroupItemsTimer;
  AMousePos := ListView.GetMouseCursorClientPos;
  if ViewInfo.GetGroupAtPos(AMousePos, AGroupViewInfo) then
  begin
    AGroup := AGroupViewInfo.Group;
    AGroupPart := AGroupViewInfo.GetPart(AMousePos, AItemViewInfo);
    if (AGroupPart = TdxListGroupPart.Header) and AGroup.IsFocusable then
    begin
      if AGroup.IsCollapsible then
        AGroup.Collapsed := not AGroup.Collapsed;
    end;
  end;
end;

function TdxListViewController.GetCursor(const AMousePos: TPoint): TCursor;
begin
  if (ViewInfo.ColumnHeadersViewInfo <> nil) and
    ViewInfo.ColumnHeadersViewInfo.Bounds.Contains(AMousePos) then
    Result := ViewInfo.ColumnHeadersViewInfo.GetCursor(AMousePos)
  else
    Result := crDefault;
end;

procedure TdxListViewController.MouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
var
  AItemViewInfo: TdxListItemCustomViewInfo;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  AGroupPart: TdxListGroupPart;
  AGroup, AFocusedGroup: TdxListGroup;
begin

  ViewInfo.HintHelper.MouseDown;
  if (AButton = mbLeft) and (ViewInfo.ColumnHeadersViewInfo <> nil) then
  begin
    FResizingColumn := ViewInfo.ColumnHeadersViewInfo.GetResizingColumn
      (AMousePos);
    if FResizingColumn <> nil then
    begin
      if ssDouble in AShift then
        FResizingColumn.ApplyBestFit
      else
      begin
        FOriginalResizingColumnWidth := FResizingColumn.Width;
        ListView.FinishItemCaptionEditing(True);
      end;
      Exit;
    end;
    ViewInfo.ColumnHeadersViewInfo.MouseDown(AButton, AShift, AMousePos);
  end;
  FResizingColumn := nil;
  FSelectItemsGroup := nil;
  AFocusedGroup := nil;
  if ViewInfo.GetGroupAtPos(AMousePos, AGroupViewInfo) then
  begin
    AGroup := AGroupViewInfo.Group;
    AGroupPart := AGroupViewInfo.GetPart(AMousePos, AItemViewInfo);
    if (AGroupPart = TdxListGroupPart.Header) and AGroup.IsFocusable then
    begin
      FSelectItemsGroup := AGroup;
      AFocusedGroup := AGroup;
    end;
    case AGroupPart of
      TdxListGroupPart.ExpandButton:
        begin
          AFocusedGroup := FocusedGroup;
          if AGroup.IsCollapsible then
            AGroup.Collapsed := not AGroup.Collapsed;
        end;
      TdxListGroupPart.Item:
        ProcessItemMouseDown(AItemViewInfo, AButton, AShift, AMousePos);
      TdxListGroupPart.Content, TdxListGroupPart.Footer:
        if not(ssCtrl in AShift) then
          ClearSelection;
    end;
  end
  else if not(ssCtrl in AShift) then
    ClearSelection;
  FocusedGroup := AFocusedGroup;
end;

procedure TdxListViewController.MouseLeave;
begin
  MouseHoveredItemIndex := -1;
  MouseHoveredGroup := nil;
  ViewInfo.MouseLeave;
end;

procedure TdxListViewController.MouseMove(AShift: TShiftState;
const AMousePos: TPoint);
var
  AItemViewInfo: TdxListItemCustomViewInfo;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
begin
  if (ssLeft in AShift) and (FResizingColumn <> nil) then
  begin
    FResizingColumn.Width := FOriginalResizingColumnWidth +
      (AMousePos.X - ListView.MouseDownPos.X);
    Exit;
  end;
  if not ListView.Dragging { IsInDragAndDropOperation } then
  begin
    if not ListView.ClientBounds.Contains(AMousePos) then
    begin
      MouseHoveredItemIndex := -1;
      MouseHoveredGroup := nil;
      if ViewInfo.ColumnHeadersViewInfo <> nil then
        ViewInfo.ColumnHeadersViewInfo.MouseLeave;
      Exit;
    end;
    if ViewInfo.ColumnHeadersViewInfo <> nil then
    begin
      if ViewInfo.ColumnHeadersViewInfo.Bounds.Contains(AMousePos) then
      begin
        ViewInfo.ColumnHeadersViewInfo.MouseMove([], AMousePos);
        MouseHoveredItemIndex := -1;
        MouseHoveredGroup := nil;
        Exit;
      end
      else
        ViewInfo.ColumnHeadersViewInfo.MouseLeave;
    end;
    if ViewInfo.GetGroupAtPos(AMousePos, AGroupViewInfo) then
    begin
      AGroupViewInfo.MouseMove([], AMousePos);
      case AGroupViewInfo.GetPart(AMousePos, AItemViewInfo) of
        TdxListGroupPart.Header, TdxListGroupPart.ExpandButton:
          begin
            MouseHoveredItemIndex := -1;
            MouseHoveredGroup := AGroupViewInfo.Group;
          end;
        TdxListGroupPart.Item:
          begin
            if not ListView.SupportsItemEnabledState or
              ListView.GetItem(AItemViewInfo.ItemIndex).Enabled then
            begin
              AItemViewInfo.MouseMove([], AMousePos);
              MouseHoveredItemIndex := AItemViewInfo.ItemIndex;
            end;
            MouseHoveredGroup := nil;
          end;
      else
        RemoveContentHottrack;
      end;
    end
    else
      RemoveContentHottrack;
  end;
end;

procedure TdxListViewController.MouseUp(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if PressedColumn <> nil then
    try
      if ViewInfo.FindColumn(AMousePos) = PressedColumn then
      begin
        case AButton of
          TMouseButton.mbLeft:
            ListView.DoColumnClick(PressedColumn);
          TMouseButton.mbRight:
            ListView.DoColumnRightClick(PressedColumn, AMousePos);
        end;
      end;
    finally
      PressedColumn := nil;
    end;
  if AButton = mbLeft then
  begin
    FResizingColumn := nil;
    if AShift = [] then
    begin
      if FUpdateItemsSelectionOnMouseUp and
        (MouseHoveredItemIndex = FPressedItemIndex) and
        (ListView.DragAndDropState = ddsNone) then
      begin
        FUpdateItemsSelectionOnMouseUp := False;
        SelectSingleItem(FPressedItemIndex);
        Exit;
      end;
      if (FEditingItemIndex >= 0) and (FEditingItemIndex = MouseHoveredItemIndex)
      then
      begin
        if ViewInfo.GetItemAtPos(AMousePos, AViewInfo) and
          AViewInfo.StartEdit(AShift, AMousePos) then
          StartEditingTimer;
      end;
    end;
  end;
end;

procedure TdxListViewController.DragEnter;
begin
  MouseHoveredItemIndex := -1;
  MousePressed := False;
  ListView.CreateAutoScrollHelper(nil);
end;

procedure TdxListViewController.DragLeave;
begin
  ListView.DestroyAutoScrollHelper;
  UpdateMouseHottrack;
end;

procedure TdxListViewController.DragDrop(Source: TObject; X, Y: Integer);
begin
end;

procedure TdxListViewController.DragOver(ASource: TObject;
const AMousePos: TPoint; var AAccept: Boolean);
var
  ADragObject: TdxListViewDragObject;
begin
  ADragObject := Safe<TdxListViewDragObject>.Cast(ASource);
  if ADragObject <> nil then
  begin
    AAccept := True;
    if ListView.AutoScrollHelper <> nil then
      ListView.AutoScrollHelper.CheckMousePosition(AMousePos);
  end
  else
    ResetDropTarget;
end;

procedure TdxListViewController.CalculateDropTarget(const AMousePos: TPoint);

  function FindNearest(AList: TList; ACheckVerticalRange: Boolean)
    : TdxListViewCustomViewInfo;
  var
    ADistance: Single;
    AItem: TdxListViewCustomViewInfo;
    AMinDistance: Single;
    I: Integer;
  begin
    Result := nil;
    AMinDistance := MaxInt;
    for I := 0 to AList.Count - 1 do
    begin
      AItem := TdxListViewCustomViewInfo(AList.List[I]);
      if PtInRect(AItem.Bounds, AMousePos) then
        Exit(AItem);
      if ACheckVerticalRange then
      begin
        if not InRange(AMousePos.Y, AItem.Bounds.Top, AItem.Bounds.Bottom) then
          Continue;
      end;
      ADistance := cxPointDistance(cxRectCenter(AItem.Bounds), AMousePos);
      if ADistance < AMinDistance then
      begin
        AMinDistance := ADistance;
        Result := AItem;
      end;
    end;
  end;

  function CalculateDropTargetCore: Boolean;
  begin
    Result := True;
  end;

begin
  if not CalculateDropTargetCore then
    ResetDropTarget;
end;

function TdxListViewController.CheckStartEditingOnMouseUp
  (AItemViewInfo: TdxListItemCustomViewInfo; AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint): Boolean;
begin
  if FEditWasClosed then
    Exit(False);
  Result := (FocusedItemIndex = AItemViewInfo.ItemIndex) and (AShift = [ssLeft])
    and not ListView.Options.ReadOnly and (SelectedIndices.Count = 1);
end;

procedure TdxListViewController.CalculateDropTarget(X, Y: Integer);
begin
  CalculateDropTarget(TPoint.Create(X, Y));
end;

procedure TdxListViewController.ResetDropTarget;
begin
  DropTargetInfo.Calculate(nil, cxNullRect, bLeft);
end;

procedure TdxListViewController.InvalidateItem(AItem: TdxListItem);
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if (AItem <> nil) and ViewInfo.FindItemViewInfo(AItem.Index, AViewInfo) then
    AViewInfo.Invalidate;
end;

function TdxListViewController.IsGroupView: Boolean;
begin
  Result := ListView.Options.GroupView;
end;

procedure TdxListViewController.MakeItemVisible(AItem: TdxListItem;
AVisibleType: TdxVisibilityType);
begin
  if AItem <> nil then
    MakeItemVisible(AItem.Index, AVisibleType, True, True);
end;

procedure TdxListViewController.MakeItemVisible(AItemIndex: Integer;
AVisibleType: TdxVisibilityType);
begin
  MakeItemVisible(AItemIndex, AVisibleType, True, True);
end;

procedure TdxListViewController.RemoveContentHottrack;
begin
  MouseHoveredItemIndex := -1;
  MouseHoveredGroup := nil;
  ViewInfo.HintHelper.CancelHint;
end;

procedure TdxListViewController.ResetContent;
begin
  ResetSelection;
  FFocusedItemIndex := -1;
  FFocusedGroup := nil;
end;

procedure TdxListViewController.SetFocusedItemIndexCore(AItemIndex: Integer;
AVisibleType: TdxVisibilityType; ACheckHorizontalPosition,
  ACheckVerticalPosition: Boolean);
begin
  if FFocusedItemIndex <> AItemIndex then
  begin
    FinishEditingTimer;
    ListView.FinishItemCaptionEditing(True);
    ExchangeIntegers(FFocusedItemIndex, AItemIndex);
    UpdateItemViewState(AItemIndex);
    UpdateItemViewState(FocusedItemIndex);
    if FocusedItemIndex >= 0 then
      FocusedGroup := nil;
    MakeItemVisible(FocusedItemIndex, vtFully, ACheckHorizontalPosition,
      ACheckVerticalPosition);
    if not TdxKeyboard.IsShiftPressed then
      FSelectAnchor.Reset;
  end;
end;

procedure TdxListViewController.AfterKeyDown(AKey: Word; AShift: TShiftState);
begin
  CheckSelectionChangedFlag;
end;

procedure TdxListViewController.AfterMouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
  CheckSelectionChangedFlag;
  if AButton = mbLeft then
    FEditWasClosed := False;
end;

procedure TdxListViewController.BeforeKeyDown(AKey: Word; AShift: TShiftState);
begin
  ResetSelectionChangedFlag;
end;

procedure TdxListViewController.BeforeMouseDown(AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
  ResetSelectionChangedFlag;
  FDragItemIndex := -1;
  FEditingItemIndex := -1;
  FPressedItemIndex := -1;
  FIsPressedItemSelected := False;
  FUpdateItemsSelectionOnMouseUp := False;
end;

procedure TdxListViewController.ProcessItemMouseDown(AItemViewInfo
  : TdxListItemCustomViewInfo; AButton: TMouseButton; AShift: TShiftState;
const AMousePos: TPoint);
var
  AItem: TdxListItem;
  AItemIndex: Integer;
  ASelectAnchor: TSelectAnchorInfo;
begin
  AItemIndex := AItemViewInfo.ItemIndex;
  AItem := ListView.GetItem(AItemIndex);
  if not AItem.IsEnabled then
    Exit;
  FPressedItemIndex := AItemIndex;
  FIsPressedItemSelected := SelectedIndices.Contains(FPressedItemIndex);
  if CheckStartEditingOnMouseUp(AItemViewInfo, AButton, AShift, AMousePos) then
  begin
    FEditingItemIndex := AItemIndex;
    Exit;
  end;
  SetFocusedItemIndexCore(AItemIndex, vtFully, not ViewInfo.IsReportView, True);
  if MultiSelect then
  begin
    ASelectAnchor := TSelectAnchorInfo.Create(AItem.GroupID, AItemIndex);
    if ssLeft in AShift then
    begin
      if ssCtrl in AShift then
      begin
        SelectItem(AItemIndex, not AItem.Selected);
        FSelectAnchor := ASelectAnchor;
      end
      else if ssShift in AShift then
        SelectItems(ASelectAnchor)
      else
      begin
        if AItemViewInfo.GetPart(AMousePos) = TdxListItemPart.StateGlyph then
          SelectItem(AItemIndex, not AItem.Selected)
        else
        begin
          if FIsPressedItemSelected then
            FUpdateItemsSelectionOnMouseUp := True
          else
            SelectSingleItem(AItemIndex);
        end;
        FSelectAnchor := ASelectAnchor;
      end;
    end
    else if (ssRight in AShift) and not(ssCtrl in AShift) and not FIsPressedItemSelected
    then
    begin
      if AItemViewInfo.GetPart(AMousePos) <> TdxListItemPart.StateGlyph then
      begin
        SelectSingleItem(AItemIndex);
        FSelectAnchor := ASelectAnchor;
      end;
    end;
  end
  else if (ssLeft in AShift) or (ssRight in AShift) then
    SelectSingleItem(AItemIndex);
end;

procedure TdxListViewController.MakeItemVisible(AItemIndex: Integer;
AVisibleType: TdxVisibilityType; ACheckHorizontalPosition,
  ACheckVerticalPosition: Boolean);
var
  ABounds: TRect;
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if AItemIndex >= 0 then
  begin
    if ViewInfo.FindItemViewInfo(AItemIndex, AViewInfo) then
      ABounds := AViewInfo.Bounds
    else
      ABounds := ViewInfo.GetBoundsForItem(ListView.GetItem(AItemIndex));
    if ListView.UseRightToLeftAlignment then
      ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds,
        ViewInfo.Bounds);
    ListView.MakeVisible(ABounds, ViewInfo.GetScrollArea, AVisibleType,
      ACheckHorizontalPosition, ACheckVerticalPosition);
  end;
end;

procedure TdxListViewController.UpdateGroupViewState(AGroup: TdxListGroup);
var
  AViewInfo: TdxListViewCustomGroupViewInfo;
begin
  if AGroup = nil then
    Exit;
  if ViewInfo.FindGroupViewInfo(AGroup, AViewInfo) then
    AViewInfo.Invalidate;
end;

procedure TdxListViewController.UpdateItemViewState(AItemIndex: Integer);
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if (AItemIndex >= 0) and ViewInfo.FindItemViewInfo(AItemIndex, AViewInfo) then
    AViewInfo.Invalidate;
end;

procedure TdxListViewController.UpdateMouseHottrack;
begin
  UpdateMouseHottrack(ListView.GetMouseCursorClientPos)
end;

procedure TdxListViewController.UpdateMouseHottrack(const AMousePos: TPoint);
begin
  MouseMove([], AMousePos);
end;

procedure TdxListViewController.AddSelection(ASelection: TdxIntegerList);
var
  I: Integer;
begin
  if not MultiSelect and (ASelection.Count > 1) then
    Exit;
  ListView.BeginUpdate;
  try
    for I := 0 to ASelection.Count - 1 do
      SelectItem(ASelection[I], True);
  finally
    ListView.EndUpdate;
  end;
end;

procedure TdxListViewController.CheckSelectionChangedFlag;
begin
  if FSelectionChangedFlag then
  begin
    FSelectionChangedFlag := False;
    ListView.DoSelectionChanged;
  end;
end;

procedure TdxListViewController.ClearSelection(AItemIndexToExclude
  : Integer = -1);
var
  I, AItemIndex: Integer;
begin
  for I := FSelectedIndices.Count - 1 downto 0 do
  begin
    AItemIndex := FSelectedIndices[I];
    if AItemIndex <> AItemIndexToExclude then
      SelectItem(AItemIndex, False);
  end;
end;

function TdxListViewController.GetFirstFocusibleItemInGroup
  (AGroup: TdxListGroup): Integer;
var
  I, AItemIndex: Integer;
  AItem: TdxListItem;
begin
  Result := -1;
  if not ListView.SupportsItemEnabledState then
  begin
    if AGroup.ItemIndices.Count > 0 then
      Result := 0;
    Exit;
  end;
  for I := 0 to AGroup.ItemIndices.Count - 1 do
  begin
    AItemIndex := AGroup.ItemIndices[I];
    AItem := ListView.GetItem(AItemIndex);
    if AItem.IsEnabled then
      Exit(AItemIndex);
  end;
end;

function TdxListViewController.GetLastFocusibleItemInGroup
  (AGroup: TdxListGroup): Integer;
var
  I, AItemIndex: Integer;
  AItem: TdxListItem;
begin
  if not ListView.SupportsItemEnabledState then
  begin
    Result := AGroup.ItemIndices.Count - 1;
    Exit;
  end;
  for I := AGroup.ItemIndices.Count - 1 downto 0 do
  begin
    AItemIndex := AGroup.ItemIndices[I];
    AItem := ListView.GetItem(AItemIndex);
    if AItem.Enabled then
      Exit(AItemIndex);
  end;
  Result := -1;
end;

procedure TdxListViewController.GetItems(const AAnchor1,
  AAnchor2: TSelectAnchorInfo; AItems: TdxIntegerList);
var
  AGroup1, AGroup2: TdxListGroup;
  I, AGroupIndex1, AGroupIndex2, AGroupItemIndex1, AGroupItemIndex2: Integer;
  ADown: Boolean;
begin
  AGroup1 := ListView.Groups.FindByID(AAnchor1.GroupID);
  AGroup2 := ListView.Groups.FindByID(AAnchor2.GroupID);
  if AGroup1 = AGroup2 then
  begin
    AGroupItemIndex1 := AGroup1.ItemIndices.IndexOf(AAnchor1.ItemIndex);
    AGroupItemIndex2 := AGroup1.ItemIndices.IndexOf(AAnchor2.ItemIndex);
    for I := Min(AGroupItemIndex1, AGroupItemIndex2) to Max(AGroupItemIndex1,
      AGroupItemIndex2) do
      AItems.Add(AGroup1.ItemIndices[I]);
  end
  else
  begin
    AGroupIndex1 := AGroup1.Index;
    AGroupIndex2 := AGroup2.Index;
    ADown := AGroupIndex1 <= AGroupIndex2;
    GetItems(AGroup1, AAnchor1.ItemIndex, ADown, AItems);
    GetItems(AGroup2, AAnchor2.ItemIndex, not ADown, AItems);
    for I := Min(AGroupIndex1, AGroupIndex2) + 1 to Max(AGroupIndex1,
      AGroupIndex2) - 1 do
      if ListView.Groups[I].IsVisible then
        AItems.AddRange(ListView.Groups[I].ItemIndices);
  end;
end;

procedure TdxListViewController.GetItems(AGroup: TdxListGroup;
AItemIndex: Integer; ADown: Boolean; AItems: TdxIntegerList);
var
  I, AGroupItemIndex1, AGroupItemIndex2: Integer;
begin
  AGroupItemIndex1 := AGroup.ItemIndices.IndexOf(AItemIndex);
  if ADown then
    AGroupItemIndex2 := AGroup.ItemIndices.Count - 1
  else
    AGroupItemIndex2 := 0;
  for I := Min(AGroupItemIndex1, AGroupItemIndex2) to Max(AGroupItemIndex1,
    AGroupItemIndex2) do
    AItems.Add(AGroup.ItemIndices[I]);
end;

function TdxListViewController.IsItemSelected(AItemIndex: Integer): Boolean;
begin
  Result := FSelectedIndices.IndexOf(AItemIndex) >= 0;
end;

function TdxListViewController.MultiSelect: Boolean;
begin
  Result := ListView.Options.MultiSelect;
end;

procedure TdxListViewController.SelectItem(AItemIndex: Integer;
ASelect: Boolean);
var
  AWasSelected: Boolean;
  AItemViewInfo: TdxListItemCustomViewInfo;
  ASelectionIndex: Integer;
begin
  ASelectionIndex := FSelectedIndices.IndexOf(AItemIndex);
  AWasSelected := ASelectionIndex >= 0;
  if AWasSelected <> ASelect then
  begin
    FSelectionChangedFlag := True;
    if ASelect then
      FSelectedIndices.Add(AItemIndex)
    else
      FSelectedIndices.Delete(ASelectionIndex);

    ListView.DoSelectItem(AItemIndex, ASelect);
    if ViewInfo.FindItemViewInfo(AItemIndex, AItemViewInfo) then
      AItemViewInfo.Invalidate;
  end;
end;

procedure TdxListViewController.SelectItems(AStartIndex, AFinishIndex: Integer);
var
  I: Integer;
begin
  ListView.BeginUpdate;
  try
    for I := Min(AStartIndex, AFinishIndex) to Max(AStartIndex, AFinishIndex) do
      SelectItem(I, True);
  finally
    ListView.EndUpdate;
  end;
end;

procedure TdxListViewController.SelectFirstAvailableItemInGroup
  (AGroup: TdxListGroup);
var
  AItemIndex: Integer;
begin
  AItemIndex := GetFirstFocusibleItemInGroup(AGroup);
  if AItemIndex >= 0 then
  begin
    ClearSelection(AItemIndex);
    SelectItem(AItemIndex, True);
  end;
end;

procedure TdxListViewController.ReplaceSelection(ASelection: TdxIntegerList);
var
  I, AItemIndex, ANewPosition: Integer;
  ACurrentSelection: TdxIntegerList;
begin
  if not MultiSelect and (ASelection.Count > 1) then
    Exit;
  ListView.BeginUpdate;
  try
    ACurrentSelection := TdxIntegerList.Create(ASelection);
    try
      for I := SelectedIndices.Count - 1 downto 0 do
      begin
        AItemIndex := SelectedIndices[I];
        ANewPosition := ACurrentSelection.IndexOf(AItemIndex);
        if ANewPosition < 0 then
          SelectItem(AItemIndex, False)
        else
          ACurrentSelection.Delete(ANewPosition);
      end;
      for I := 0 to ACurrentSelection.Count - 1 do
        SelectItem(ACurrentSelection[I], True);
    finally
      ACurrentSelection.Free;
    end;
  finally
    ListView.EndUpdate;
  end;
end;

procedure TdxListViewController.ResetSelection;
begin
  FSelectedIndices.Clear;
end;

procedure TdxListViewController.ResetSelectionChangedFlag;
begin
  FSelectionChangedFlag := False;
end;

procedure TdxListViewController.SelectItems(const ANewAnchor
  : TSelectAnchorInfo);
var
  ASelection: TdxIntegerList;
  I, AFirstItemIndex, ALastItemIndex: Integer;
  AGroup: TdxListGroup;
begin
  ASelection := TdxIntegerList.Create(256);
  try
    if IsGroupView then
    begin
      if FSelectAnchor.IsNull then
      begin
        AGroup := ListView.Groups.GetFirstVisibleGroup;
        if AGroup = nil then
          Exit;
        FSelectAnchor.GroupID := AGroup.GroupID;
        FSelectAnchor.ItemIndex := AGroup.ItemIndices[0];
      end;
      GetItems(FSelectAnchor, ANewAnchor, ASelection);
    end
    else
    begin
      if FSelectAnchor.IsNull then
        AFirstItemIndex := 0
      else
        AFirstItemIndex := FSelectAnchor.ItemIndex;
      ALastItemIndex := ANewAnchor.ItemIndex;
      for I := Min(AFirstItemIndex, ALastItemIndex) to Max(AFirstItemIndex,
        ALastItemIndex) do
        ASelection.Add(I);
    end;
    ReplaceSelection(ASelection);
  finally
    ASelection.Free;
  end;
end;

procedure TdxListViewController.SelectSingleItem(AItemIndex: Integer);
begin
  ClearSelection(AItemIndex);
  SelectItem(AItemIndex, True);
end;

procedure TdxListViewController.GotoItemIndex(AItemIndex: Integer;
AShift: TShiftState);
var
  AItem: TdxListItem;
  ASelectionEnd: TSelectAnchorInfo;
  APrevFocusedItemIndex: Integer;
begin
  if (AItemIndex < 0) or (FocusedItemIndex = AItemIndex) then
    Exit;
  APrevFocusedItemIndex := FocusedItemIndex;
  SetFocusedItemIndexCore(AItemIndex, vtFully, not ViewInfo.IsReportView, True);
  if AShift <> [ssCtrl] then // explorer-style for a single-selection mode
  begin
    if MultiSelect and (ssShift in AShift) then
    begin
      if ssCtrl in AShift then
        SelectItems(FocusedItemIndex, APrevFocusedItemIndex)
      else
      begin
        AItem := ListView.GetItem(AItemIndex);
        ASelectionEnd := TSelectAnchorInfo.Create(AItem.GroupID, AItemIndex);
        SelectItems(ASelectionEnd);
      end;
    end
    else
    begin
      ClearSelection(AItemIndex);
      SelectItem(FocusedItemIndex, True);
    end;
  end;
end;

procedure TdxListViewController.GotoFirstFocusibleItem(AShift: TShiftState);
var
  I, AItemIndex: Integer;
  AGroup: TdxListGroup;
  AItem: TdxListItem;
begin
  AItemIndex := -1;
  if IsGroupView then
  begin
    for I := 0 to ListView.Groups.Count - 1 do
    begin
      AGroup := ListView.Groups[I];
      if AGroup.IsVisible and not AGroup.Collapsed then
      begin
        AItemIndex := GetFirstFocusibleItemInGroup(AGroup);
        if AItemIndex >= 0 then
          Break;
      end;
    end
  end
  else
  begin
    if ListView.SupportsItemEnabledState then
    begin
      for I := 0 to ListView.Items.Count - 1 do
      begin
        AItem := ListView.Items[I];
        if AItem.Enabled then
        begin
          AItemIndex := I;
          Break;
        end;
      end;
    end
    else if ListView.Items.Count > 0 then
      AItemIndex := 0;
  end;
  GotoItemIndex(AItemIndex, AShift);
end;

procedure TdxListViewController.GotoLastFocusibleItem(AShift: TShiftState);
var
  I, AItemIndex: Integer;
  AGroup: TdxListGroup;
  AItem: TdxListItem;
begin
  AItemIndex := -1;
  if IsGroupView then
  begin
    for I := ListView.Groups.Count - 1 downto 0 do
    begin
      AGroup := ListView.Groups[I];
      if AGroup.IsVisible and not AGroup.Collapsed then
      begin
        AItemIndex := GetLastFocusibleItemInGroup(AGroup);
        if AItemIndex >= 0 then
          Break;
      end;
    end
  end
  else
  begin
    if ListView.SupportsItemEnabledState then
    begin
      for I := ListView.Items.Count - 1 downto 0 do
      begin
        AItem := ListView.Items[I];
        if AItem.Enabled then
        begin
          AItemIndex := I;
          Break;
        end;
      end;
    end
    else
      AItemIndex := ListView.Items.Count - 1;
  end;
  GotoItemIndex(AItemIndex, AShift);
end;

function TdxListViewController.GetStartItemIndexForKeyboardNavigation: Integer;
var
  I: Integer;
  AGroup: TdxListGroup;
  AItem: TdxListItem;
begin
  Result := FocusedItemIndex;
  if Result < 0 then
  begin
    if IsGroupView then
    begin
      for I := 0 to ListView.Groups.Count - 1 do
      begin
        AGroup := ListView.Groups[I];
        if AGroup.IsVisible and not AGroup.Collapsed then
        begin
          Result := GetFirstFocusibleItemInGroup(AGroup);
          if Result >= 0 then
            Break;
        end;
      end
    end
    else
      for I := 0 to ListView.Items.Count - 1 do
      begin
        AItem := ListView.GetItem(I);
        if AItem.IsEnabled then
          Exit(I);
      end;
  end;
end;

procedure TdxListViewController.SelectNextItem(AItemIndex: Integer;
ADirectionX, ADirectionY: Integer; AShift: TShiftState);
var
  AItem: TdxListItem;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  ANextItemIndex: Integer;
begin
  if AItemIndex < 0 then
    Exit;

  AItem := ListView.GetItem(AItemIndex);
  if not ViewInfo.FindGroupViewInfo(AItem, AGroupViewInfo) then
    Exit;

  ANextItemIndex := AItemIndex;
  repeat
    if AGroupViewInfo.GetNextItem(ANextItemIndex, ADirectionX, ADirectionY) then
      Break;
    if not IsGroupView then
      Break;
    Assert(False);
  until False;

  GotoItemIndex(ANextItemIndex, AShift);
end;

procedure TdxListViewController.ShowPriorPage(AShift: TShiftState);
var
  AItemIndex, AOrderIndex, ASize, ANextIndex, ATopFullVisibleRowIndex,
    ARowIndex, ALeftFullVisibleColumn, AGapSize, AItemSize,
    AColumnIndex: Integer;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  AItemViewInfo: TdxListItemCustomViewInfo;
  ABounds: TRect;
  AItemsOffset: TSize;
begin
  AItemIndex := GetStartItemIndexForKeyboardNavigation;
  if AItemIndex < 0 then
    Exit;

  if ViewInfo.FindItemViewInfo(AItemIndex, AItemViewInfo) then
  begin
    AGroupViewInfo := AItemViewInfo.Owner;
    AOrderIndex := AGroupViewInfo.ItemIndexToOrderIndex(AItemIndex);
    AGapSize := ViewInfo.GetItemsGap;
    ABounds := ViewInfo.GetGroupVisibleBounds;
    AItemsOffset := ViewInfo.GetItemsOffset;
    if ViewInfo.IsReportView then
    begin
      ASize := ListView.TopPos - AItemsOffset.cy;
      AItemSize := ViewInfo.ItemSize.cy;
      ANextIndex := Max(0, (ASize + AGapSize) div (AItemSize + AGapSize));
      if ANextIndex > 0 then
        ANextIndex := AOrderIndex - Max(1, (ABounds.Height + AGapSize)
          div (AItemSize + AGapSize));
    end
    else
    begin
      if AGroupViewInfo.IsHorizontalItemsArrangement then
      begin
        ASize := ListView.TopPos - AItemsOffset.cy;
        AItemSize := ViewInfo.ItemSize.cy;
        ARowIndex := AOrderIndex div AGroupViewInfo.ColumnCount;
        ATopFullVisibleRowIndex := (ASize + (AItemSize + AGapSize))
          div (AItemSize + AGapSize);
        if ARowIndex > ATopFullVisibleRowIndex then
          ANextIndex := AOrderIndex - (ARowIndex - ATopFullVisibleRowIndex) *
            AGroupViewInfo.ColumnCount
        else
          ANextIndex := AOrderIndex -
            ((ABounds.Height + AGapSize) div (AItemSize + AGapSize)) *
            AGroupViewInfo.ColumnCount;
      end
      else
      begin
        ASize := ListView.LeftPos - AItemsOffset.cx;
        AItemSize := ViewInfo.ItemSize.cx;
        AColumnIndex := AOrderIndex div AGroupViewInfo.RowCount;
        ALeftFullVisibleColumn := (ASize + (AItemSize + AGapSize))
          div (AItemSize + AGapSize);
        if AColumnIndex > ALeftFullVisibleColumn then
          ANextIndex := AOrderIndex - (AColumnIndex - ALeftFullVisibleColumn) *
            AGroupViewInfo.RowCount
        else
          ANextIndex := AOrderIndex -
            ((ABounds.Width + AGapSize) div (AItemSize + AGapSize)) *
            AGroupViewInfo.RowCount;
      end;
    end;
    if ANextIndex < 0 then
      ANextIndex := 0;
    GotoItemIndex(AGroupViewInfo.OrderIndexToItemIndex(ANextIndex), AShift);
  end;
end;

procedure TdxListViewController.ShowNextPage(AShift: TShiftState);
var
  AItemIndex, AOrderIndex, ASize, ANextIndex, ALastFullVisibleRowIndex,
    ARowIndex, ARightFullVisibleColumn, AGapSize, AItemSize,
    AColumnIndex: Integer;
  AGroupViewInfo: TdxListViewCustomGroupViewInfo;
  AItemViewInfo: TdxListItemCustomViewInfo;
  ABounds: TRect;
  AItemsOffset: TSize;
begin
  AItemIndex := GetStartItemIndexForKeyboardNavigation;
  if AItemIndex < 0 then
    Exit;

  if ViewInfo.FindItemViewInfo(AItemIndex, AItemViewInfo) then
  begin
    AGroupViewInfo := AItemViewInfo.Owner;
    AOrderIndex := AGroupViewInfo.ItemIndexToOrderIndex(AItemIndex);
    AItemsOffset := ViewInfo.GetItemsOffset;
    AGapSize := ViewInfo.GetItemsGap;
    ABounds := ViewInfo.GetGroupVisibleBounds;
    if ViewInfo.IsReportView then
    begin
      ASize := ABounds.Height + ListView.TopPos - AItemsOffset.cy;
      AItemSize := ViewInfo.ItemSize.cy;
      ANextIndex := Max(0, (ASize + AGapSize) div (AItemSize + AGapSize) - 1);
      if ANextIndex <= AOrderIndex then
        ANextIndex := AOrderIndex + Max(1, (ABounds.Height + AGapSize)
          div (AItemSize + AGapSize));
    end
    else
    begin
      if AGroupViewInfo.IsHorizontalItemsArrangement then
      begin
        ASize := ABounds.Height + ListView.TopPos - AItemsOffset.cy;
        AItemSize := ViewInfo.ItemSize.cy;
        ARowIndex := AOrderIndex div AGroupViewInfo.ColumnCount;
        ALastFullVisibleRowIndex :=
          Max(0, (ASize + AGapSize) div (AItemSize + AGapSize) - 1);
        if ARowIndex < ALastFullVisibleRowIndex then
          ANextIndex := AOrderIndex + (ALastFullVisibleRowIndex - ARowIndex) *
            AGroupViewInfo.ColumnCount
        else
          ANextIndex := AOrderIndex +
            ((ABounds.Height + AGapSize) div (AItemSize + AGapSize)) *
            AGroupViewInfo.ColumnCount;
      end
      else
      begin
        ASize := ABounds.Width + ListView.LeftPos - AItemsOffset.cx;
        AItemSize := ViewInfo.ItemSize.cx;
        AColumnIndex := AOrderIndex div AGroupViewInfo.RowCount;
        ARightFullVisibleColumn :=
          Max(0, (ASize + AGapSize) div (AItemSize + AGapSize) - 1);
        if AColumnIndex < ARightFullVisibleColumn then
          ANextIndex := AOrderIndex + (ARightFullVisibleColumn - AColumnIndex) *
            AGroupViewInfo.RowCount
        else
          ANextIndex := AOrderIndex +
            ((ABounds.Width + AGapSize) div (AItemSize + AGapSize)) *
            AGroupViewInfo.RowCount;
      end;
    end;
    if ANextIndex >= AGroupViewInfo.ItemCount then
      ANextIndex := AGroupViewInfo.ItemCount - 1;
    GotoItemIndex(AGroupViewInfo.OrderIndexToItemIndex(ANextIndex), AShift);
  end;
end;

function TdxListViewController.CanDrag(const AMousePos: TPoint): Boolean;
var
  AItemViewInfo: TdxListItemCustomViewInfo;
begin
  if FPressedItemIndex < 0 then
    Exit(False);

  if FIsPressedItemSelected then
    FDragItemIndex := FPressedItemIndex
  else
  begin
    if ViewInfo.FindItemViewInfo(FPressedItemIndex, AItemViewInfo) and
      AItemViewInfo.StartDrag([], AMousePos) then
      FDragItemIndex := FPressedItemIndex;
  end;
  Result := FDragItemIndex >= 0;
end;

procedure TdxListViewController.EndDragAndDrop(Accepted: Boolean);
begin
  PressedColumn := nil;
  FDragItemIndex := -1;
end;

function TdxListViewController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := MultiSelect and ViewInfo.CanStartMultiSelectionByMouse(P);
end;

procedure TdxListViewController.CancelEdit;
begin
  FEditWasClosed := True;
end;

procedure TdxListViewController.FinishEditingTimer;
begin
  FEditingItemIndex := -1;
  FreeAndNil(FEditingTimer);
end;

procedure TdxListViewController.FinishSelectGroupItemsTimer;
begin
  FreeAndNil(FSelectGroupItemsTimer);
end;

function TdxListViewController.GetDropTargetInfo: TdxListViewDropTargetViewInfo;
begin
  Result := ViewInfo.DropTarget;
end;

function TdxListViewController.GetViewInfo: TdxListViewViewInfo;
begin
  Result := ListView.ViewInfo;
end;

procedure TdxListViewController.SelectGroupItems(Sender: TObject);
begin
  FinishSelectGroupItemsTimer;
  if FSelectItemsGroup <> nil then
  begin
    ResetSelectionChangedFlag;
    if MultiSelect then
    begin
      if IsCtrlPressed then
        AddSelection(FSelectItemsGroup.ItemIndices)
      else
        ReplaceSelection(FSelectItemsGroup.ItemIndices);
    end
    else
      SelectFirstAvailableItemInGroup(FSelectItemsGroup);
    FSelectItemsGroup := nil;
    CheckSelectionChangedFlag;
  end;
end;

procedure TdxListViewController.SetFocusedGroup(AValue: TdxListGroup);
begin
  if FFocusedGroup <> AValue then
  begin
    FinishEditingTimer;
    ExchangePointers(FFocusedGroup, AValue);
    UpdateGroupViewState(FFocusedGroup);
    UpdateGroupViewState(AValue);
    if FocusedGroup <> nil then
      FocusedItemIndex := -1;
  end;
end;

procedure TdxListViewController.SetFocusedItemIndex(AItemIndex: Integer);
begin
  SetFocusedItemIndexCore(AItemIndex, vtFully, True, True);
end;

procedure TdxListViewController.SetMouseHoveredGroup(AValue: TdxListGroup);
var
  AOldGroup: TdxListGroup;
begin
  if FMouseHoveredGroup <> AValue then
  begin
    Application.CancelHint;
    AOldGroup := FMouseHoveredGroup;
    FMouseHoveredGroup := AValue;
    UpdateGroupViewState(AValue);
    UpdateGroupViewState(AOldGroup);
  end;
end;

procedure TdxListViewController.SetMouseHoveredItemIndex(AItemIndex: Integer);
var
  AOldIndex: Integer;
begin
  if MouseHoveredItemIndex <> AItemIndex then
  begin
    Application.CancelHint;
    AOldIndex := FMouseHoveredItemIndex;
    FMouseHoveredItemIndex := AItemIndex;
    UpdateItemViewState(AItemIndex);
    UpdateItemViewState(AOldIndex);
  end;
end;

procedure TdxListViewController.SetMousePressed(AValue: Boolean);
begin
  if MousePressed <> AValue then
  begin
    FMousePressed := AValue;
    UpdateItemViewState(MouseHoveredItemIndex);
  end;
end;

procedure TdxListViewController.SetPressedColumn(AValue: TdxListColumn);
begin
  if FPressedColumn <> AValue then
  begin
    FPressedColumn := AValue;
    if ViewInfo.ColumnHeadersViewInfo <> nil then
      ViewInfo.ColumnHeadersViewInfo.Invalidate;
  end;
end;

procedure TdxListViewController.StartEditing(Sender: TObject);
begin
  FreeAndNil(FEditingTimer);
  ListView.StartItemCaptionEditing(FEditingItemIndex);
end;

procedure TdxListViewController.StartEditingTimer;
begin
  FreeAndNil(FEditingTimer);
  FEditingTimer := TcxTimer.Create(nil);
  FEditingTimer.Interval := GetDoubleClickTime;
  FEditingTimer.OnTimer := StartEditing;
end;

procedure TdxListViewController.StartSelectGroupItemsTimer;
begin
  FinishSelectGroupItemsTimer;
  FSelectGroupItemsTimer := TcxTimer.Create(nil);
  FSelectGroupItemsTimer.Interval := GetDoubleClickTime;
  FSelectGroupItemsTimer.OnTimer := SelectGroupItems;
end;

{ TdxListViewCustomOptions }

procedure TdxListViewCustomOptions.Changed(AType: TdxChangeType);
begin
  if NeedNotifyControl then
    ListView.LayoutChanged(AType);
end;

procedure TdxListViewCustomOptions.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TdxListViewCustomOptions.NeedNotifyControl: Boolean;
begin
  Result := True;
end;

{ TdxListViewOptionsIcon }

constructor TdxListViewOptionsIcons.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FAutoArrange := True;
  FItemsGap := 1;
  FTextLineCount := 2;
  FSmallIconsColumnWidth := DefaultSmallIconsColumnWidth;
end;

procedure TdxListViewOptionsIcons.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  ItemsGap := MulDiv(ItemsGap, M, D);
  SmallIconsColumnWidth := MulDiv(SmallIconsColumnWidth, M, D);
end;

procedure TdxListViewOptionsIcons.DoAssign(Source: TPersistent);
var
  AOptions: TdxListViewOptionsIcons;
begin
  inherited DoAssign(Source);
  AOptions := Safe<TdxListViewOptionsIcons>.Cast(Source);
  if AOptions <> nil then
  begin
    Arrangement := AOptions.Arrangement;
    AutoArrange := AOptions.AutoArrange;
    SmallIconsColumnWidth := AOptions.SmallIconsColumnWidth;
    ItemsGap := AOptions.ItemsGap;
    TextLineCount := AOptions.TextLineCount;
  end;
end;

procedure TdxListViewOptionsIcons.SetArrangement
  (AValue: TdxListIconsArrangement);
begin
  if FArrangement <> AValue then
  begin
    FArrangement := AValue;
    if AutoArrange then
      try
        ListView.BeginUpdate;
        ListView.SetLeftTop(TPoint.Null, False);
      finally
        ListView.EndUpdate;
      end;
  end;
end;

procedure TdxListViewOptionsIcons.SetAutoArrange(AValue: Boolean);
begin
  if FAutoArrange <> AValue then
  begin
    FAutoArrange := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsIcons.SetSmallIconsColumnWidth(AValue: Integer);
begin
  AValue := EnsureRange(AValue, MinSmallIconsColumnWidth, AValue);
  if FSmallIconsColumnWidth <> AValue then
  begin
    FSmallIconsColumnWidth := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsIcons.SetItemsGap(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxItemsGap);
  if FItemsGap <> AValue then
  begin
    FItemsGap := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsIcons.SetTextLineCount(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, MaxTextLineCount);
  if FTextLineCount <> AValue then
  begin
    FTextLineCount := AValue;
    Changed;
  end;
end;

{ TdxListViewOptionsList }

constructor TdxListViewOptionsList.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FItemsGap := 1;
  FColumnWidth := DefaultColumnWidth;
end;

procedure TdxListViewOptionsList.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  ColumnWidth := MulDiv(ColumnWidth, M, D);
  ItemsGap := MulDiv(ItemsGap, M, D);
end;

procedure TdxListViewOptionsList.DoAssign(Source: TPersistent);
var
  AOptions: TdxListViewOptionsList;
begin
  inherited DoAssign(Source);
  AOptions := Safe<TdxListViewOptionsList>.Cast(Source);
  if AOptions <> nil then
  begin
    ColumnWidth := AOptions.ColumnWidth;
    ItemsGap := AOptions.ItemsGap;
  end;
end;

function TdxListViewOptionsList.NeedNotifyControl: Boolean;
begin
  Result := ListView.ViewStyle = TdxListViewStyle.List;
end;

procedure TdxListViewOptionsList.SetColumnWidth(AValue: Integer);
begin
  AValue := EnsureRange(AValue, MinColumnWidth, AValue);
  if FColumnWidth <> AValue then
  begin
    FColumnWidth := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsList.SetItemsGap(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxItemsGap);
  if FItemsGap <> AValue then
  begin
    FItemsGap := AValue;
    Changed;
  end;
end;

{ TdxListViewOptionsReport }

constructor TdxListViewOptionsReport.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FShowColumsHeaders := True;
end;

procedure TdxListViewOptionsReport.DoAssign(Source: TPersistent);
var
  AOptions: TdxListViewOptionsReport;
begin
  inherited DoAssign(Source);
  AOptions := Safe<TdxListViewOptionsReport>.Cast(Source);
  if AOptions <> nil then
  begin
    AlwaysShowItemImageInFirstColumn :=
      AOptions.AlwaysShowItemImageInFirstColumn;
    RowSelect := AOptions.RowSelect;
    ShowColumsHeaders := AOptions.ShowColumsHeaders;
  end;
end;

function TdxListViewOptionsReport.NeedNotifyControl: Boolean;
begin
  Result := ListView.ViewStyle = TdxListViewStyle.Report;
end;

procedure TdxListViewOptionsReport.SetAlwaysShowItemImageInFirstColumn
  (AValue: Boolean);
begin
  if FAlwaysShowItemImageInFirstColumn <> AValue then
  begin
    FAlwaysShowItemImageInFirstColumn := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsReport.SetRowSelect(AValue: Boolean);
begin
  if FRowSelect <> AValue then
  begin
    FRowSelect := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptionsReport.SetShowColumsHeaders(AValue: Boolean);
begin
  if FShowColumsHeaders <> AValue then
  begin
    FShowColumsHeaders := AValue;
    Changed;
  end;
end;

{ TdxListViewPaddingOptions }

procedure TdxListViewPaddingOptions.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  GroupContent.Margin := cxRectScale(GroupContent.Margin, M, D);
  GroupHeader.Margin := cxRectScale(GroupHeader.Margin, M, D);
  Item.Margin := cxRectScale(Item.Margin, M, D);
  View.Margin := cxRectScale(View.Margin, M, D);
end;

constructor TdxListViewPaddingOptions.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FGroupContent := CreatePadding(1);
  FGroupHeader := CreatePadding(2);
  FItem := CreatePadding(2);
  FView := CreatePadding(1);
end;

function TdxListViewPaddingOptions.CreatePadding(ADefaultValue: Integer)
  : TcxMargin;
begin
  Result := TcxMargin.Create(Self, ADefaultValue);
  Result.OnChange := ChangeHandler;
end;

destructor TdxListViewPaddingOptions.Destroy;
begin
  FreeAndNil(FGroupContent);
  FreeAndNil(FGroupHeader);
  FreeAndNil(FItem);
  FreeAndNil(FView);
  inherited Destroy;
end;

procedure TdxListViewPaddingOptions.DoAssign(Source: TPersistent);
var
  AOptions: TdxListViewPaddingOptions;
begin
  inherited DoAssign(Source);
  AOptions := Safe<TdxListViewPaddingOptions>.Cast(Source);
  if AOptions <> nil then
  begin
    GroupContent := AOptions.GroupContent;
    GroupHeader := AOptions.GroupHeader;
    Item := AOptions.Item;
    View := AOptions.View;
  end;
end;

procedure TdxListViewPaddingOptions.SetGroupContent(const AValue: TcxMargin);
begin
  FGroupContent.Assign(AValue);
end;

procedure TdxListViewPaddingOptions.SetGroupHeader(const AValue: TcxMargin);
begin
  FGroupHeader.Assign(AValue);
end;

procedure TdxListViewPaddingOptions.SetItem(const AValue: TcxMargin);
begin
  FItem.Assign(AValue);
end;

procedure TdxListViewPaddingOptions.SetView(const AValue: TcxMargin);
begin
  FView.Assign(AValue);
end;

procedure TdxListViewPaddingOptions.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

{ TdxListViewOptions }

constructor TdxListViewOptions.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FIcons := CreateIcons;
  FList := CreateList;
  FPaddingOptions := CreatePadding;
  FReport := CreateReport;
  FScaleImagesForDPI := False;
  FHotTrack := False;
  FSortType := TdxListViewSortType.None;
end;

destructor TdxListViewOptions.Destroy;
begin
  FreeAndNil(FPaddingOptions);
  FreeAndNil(FReport);
  FreeAndNil(FList);
  FreeAndNil(FIcons);
  inherited Destroy;
end;

procedure TdxListViewOptions.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  PaddingOptions.ChangeScale(M, D);
  Icons.ChangeScale(M, D);
  List.ChangeScale(M, D);
  Report.ChangeScale(M, D);
end;

function TdxListViewOptions.CreateIcons: TdxListViewOptionsIcons;
begin
  Result := TdxListViewOptionsIcons.Create(ListView);
end;

function TdxListViewOptions.CreateList: TdxListViewOptionsList;
begin
  Result := TdxListViewOptionsList.Create(ListView);
end;

function TdxListViewOptions.CreatePadding: TdxListViewPaddingOptions;
begin
  Result := TdxListViewPaddingOptions.Create(ListView);
end;

function TdxListViewOptions.CreateReport: TdxListViewOptionsReport;
begin
  Result := TdxListViewOptionsReport.Create(ListView);
end;

procedure TdxListViewOptions.DoAssign(Source: TPersistent);
var
  AOptions: TdxListViewOptions;
begin
  inherited DoAssign(Source);
  AOptions := Safe<TdxListViewOptions>.Cast(Source);
  if AOptions <> nil then
  begin
    ColumnAutoWidth := AOptions.ColumnAutoWidth;
    ExplorerStyle := AOptions.ExplorerStyle;
    GroupView := AOptions.GroupView;
    HotTrack := AOptions.HotTrack;
    Icons := AOptions.Icons;
    ItemShowHint := AOptions.ItemShowHint;
    List := AOptions.List;
    MultiSelect := AOptions.MultiSelect;
    PaddingOptions := AOptions.PaddingOptions;
    ReadOnly := AOptions.ReadOnly;
    Report := AOptions.Report;
    ShowCheckBoxes := AOptions.ShowCheckBoxes;
    SortType := AOptions.SortType;
  end;
end;

procedure TdxListViewOptions.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect <> AValue then
  begin
    FMultiSelect := AValue;
    if not AValue then
      ListView.Controller.ClearSelection;
  end;
end;

procedure TdxListViewOptions.SetOwnerData(AValue: Boolean);
begin
  if FOwnerData <> AValue then
  begin
    FOwnerData := AValue;
    if AValue then
      ListView.Items.Clear;
  end;
end;

procedure TdxListViewOptions.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    if AValue then
      ListView.FinishItemCaptionEditing(False);
  end;
end;

procedure TdxListViewOptions.SetSortType(AValue: TdxListViewSortType);
begin
  if FSortType <> AValue then
  begin
    FSortType := AValue;
    if ((AValue in [TdxListViewSortType.Data, TdxListViewSortType.Both]) and
      Assigned(ListView.OnCompare)) or
      (AValue in [TdxListViewSortType.Text, TdxListViewSortType.Both]) then
      ListView.AlphaSort;
  end;
end;

procedure TdxListViewOptions.SetColumnAutoWidth(AValue: Boolean);
begin
  if FColumnAutoWidth <> AValue then
  begin
    FColumnAutoWidth := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptions.SetExplorerStyle(AValue: Boolean);
begin
  if FExplorerStyle <> AValue then
  begin
    FExplorerStyle := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptions.SetPaddingOptions(const AValue
  : TdxListViewPaddingOptions);
begin
  FPaddingOptions.Assign(AValue);
end;

procedure TdxListViewOptions.SetGroupView(AValue: Boolean);
begin
  if FGroupView <> AValue then
  begin
    FGroupView := AValue;
    ListView.BeginUpdate;
    try
      ListView.SetLeftTop(TPoint.Null, False);
    finally
      ListView.EndUpdate;
    end;
  end;
end;

procedure TdxListViewOptions.SetIcons(const AValue: TdxListViewOptionsIcons);
begin
  FIcons.Assign(AValue);
end;

procedure TdxListViewOptions.SetList(const AValue: TdxListViewOptionsList);
begin
  FList.Assign(AValue);
end;

procedure TdxListViewOptions.SetReport(const AValue: TdxListViewOptionsReport);
begin
  FReport.Assign(AValue);
end;

procedure TdxListViewOptions.SetScaleImagesForDPI(AValue: Boolean);
begin
  if FScaleImagesForDPI <> AValue then
  begin
    FScaleImagesForDPI := AValue;
    Changed;
  end;
end;

procedure TdxListViewOptions.SetShowCheckBoxes(AValue: Boolean);
begin
  if FShowCheckBoxes <> AValue then
  begin
    FShowCheckBoxes := AValue;
    if ShowCheckBoxes then
      ListView.Images.StateImages := nil;
    Changed;
  end;
end;

{ TdxListViewDragSelectDragAndDropObject }

procedure TdxListViewDragSelectDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FAnchor.Init(ListView.LeftPos, ListView.TopPos);
  FStartPos := GetClientCursorPos;
  FFinishPos := FStartPos;
  FExtendedMode := TdxKeyboard.IsControlPressed;
  ListView.BeginDragSelectOperation(FExtendedMode);
end;

procedure TdxListViewDragSelectDragAndDropObject.DragAndDrop(const P: TPoint;
var Accepted: Boolean);
var
  ABounds: TRect;
begin
  inherited DragAndDrop(P, Accepted);
  ListView.AutoScrollHelper.CheckMousePosition(P);
  FFinishPos := P;
  FStartPos.Offset(FAnchor.X - ListView.LeftPos, FAnchor.Y - ListView.TopPos);
  ABounds.Init(FStartPos, FFinishPos, True);
  ListView.UpdateDragSelectState(ABounds, FExtendedMode);
end;

procedure TdxListViewDragSelectDragAndDropObject.EndDragAndDrop
  (Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  ListView.EndDragSelectOperation;
end;

function TdxListViewDragSelectDragAndDropObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  Result := crDefault;
end;

function TdxListViewDragSelectDragAndDropObject.GetListView: TdxCustomListView;
begin
  Result := TdxCustomListView(Control);
end;

function TdxListViewDragSelectDragAndDropObject.ProcessKeyDown(AKey: Word;
AShiftState: TShiftState): Boolean;
begin
  Result := True;
end;

function TdxListViewDragSelectDragAndDropObject.ProcessKeyUp(AKey: Word;
AShiftState: TShiftState): Boolean;
begin
  if AKey = VK_CONTROL then
    FExtendedMode := False;
  Result := True;
end;

{ TdxListViewDragObject }

constructor TdxListViewDragObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  AlwaysShowDragImages := True;
end;

function TdxListViewDragObject.GetDragCursor(Accepted: Boolean;
X, Y: Integer): TCursor;
begin
  Result := inherited GetDragCursor(Accepted, X, Y);
end;

function TdxListViewDragObject.GetListView: TdxCustomListView;
begin
  Result := TdxCustomListView(inherited Control);
end;

{ TdxListViewFonts }

constructor TdxListViewFonts.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FColumnHeader := CreateFont;;
  FGroupHeader := CreateFont;
  FGroupFooter := CreateFont;
  FGroupSubtitle := CreateFont;
  FItem := CreateFont;
  FSubItem := CreateFont;
end;

destructor TdxListViewFonts.Destroy;
begin
  FSubItem.Free;
  FItem.Free;
  FGroupHeader.Free;
  FGroupFooter.Free;
  FGroupSubtitle.Free;
  FColumnHeader.Free;
  inherited Destroy;
end;

procedure TdxListViewFonts.Assign(ASource: TPersistent);
var
  AFonts: TdxListViewFonts;
begin
  AFonts := Safe<TdxListViewFonts>.Cast(ASource);
  if AFonts <> nil then
  begin
    ColumnHeader := AFonts.ColumnHeader;
    GroupHeader := AFonts.GroupHeader;
    GroupFooter := AFonts.GroupFooter;
    GroupSubtitle := AFonts.GroupSubtitle;
    Item := AFonts.Item;
    SubItem := AFonts.SubItem;
  end;
end;

procedure TdxListViewFonts.Changed;
begin
  ListView.ParentFont := False;
  ListView.LayoutChanged;
end;

function TdxListViewFonts.CreateFont: TFont;
begin
  Result := TFont.Create;
  Result.Color := clDefault;
  Result.OnChange := FontChanged;
end;

procedure TdxListViewFonts.FontChanged(Sender: TObject);
begin
  if FLockChanges then
    Exit;
  Changed;
end;

procedure TdxListViewFonts.SetColumnHeader(const AValue: TFont);
begin
  FColumnHeader.Assign(AValue);
end;

procedure TdxListViewFonts.SetFont(AFont: TFont; AKeepColors: Boolean);

  procedure AssignFont(ATargetFont: TFont);
  var
    ASaveColor: TColor;
  begin
    ASaveColor := ATargetFont.Color;
    ATargetFont.Assign(AFont);
    if AKeepColors then
      ATargetFont.Color := ASaveColor;
  end;

begin
  if AFont = nil then
    Exit;
  ListView.BeginUpdate;
  try
    AssignFont(ColumnHeader);
    AssignFont(GroupHeader);
    AssignFont(GroupFooter);
    AssignFont(GroupSubtitle);
    AssignFont(Item);
    AssignFont(SubItem);
  finally
    ListView.EndUpdate;
  end;
end;

procedure TdxListViewFonts.SetGroupFooter(const AValue: TFont);
begin
  FGroupFooter.Assign(AValue);
end;

procedure TdxListViewFonts.SetGroupHeader(const AValue: TFont);
begin
  FGroupHeader.Assign(AValue);
end;

procedure TdxListViewFonts.SetGroupSubtitle(const AValue: TFont);
begin
  FGroupSubtitle.Assign(AValue);
end;

procedure TdxListViewFonts.SetItem(const AValue: TFont);
begin
  FItem.Assign(AValue);
end;

procedure TdxListViewFonts.SetSubItem(const AValue: TFont);
begin
  FItem.Assign(AValue);
end;

{ TdxListViewImages }

constructor TdxListViewImages.Create(AOwner: TdxCustomListView);
begin
  inherited Create(AOwner);
  FColumnHeaderImagesChangeLink := CreateImagesChangeLink;
  FGroupHeaderImagesChangeLink := CreateImagesChangeLink;
  FLargeImagesChangeLink := CreateImagesChangeLink;
  FSmallImagesChangeLink := CreateImagesChangeLink;
  FStateImagesChangeLink := CreateImagesChangeLink;
end;

destructor TdxListViewImages.Destroy;
begin
  FColumnHeaderImagesChangeLink.Free;
  FGroupHeaderImagesChangeLink.Free;
  FLargeImagesChangeLink.Free;
  FSmallImagesChangeLink.Free;
  FStateImagesChangeLink.Free;
  inherited Destroy;
end;

procedure TdxListViewImages.Assign(ASource: TPersistent);
var
  AImages: TdxListViewImages;
begin
  AImages := Safe<TdxListViewImages>.Cast(ASource);
  if AImages <> nil then
  begin
    ColumnHeaderImages := AImages.ColumnHeaderImages;
    GroupHeaderImages := AImages.GroupHeaderImages;
    LargeImages := AImages.LargeImages;
    SmallImages := AImages.SmallImages;
    StateImages := AImages.StateImages;
  end;
end;

function TdxListViewImages.AreImagesLinked(const AValue: TObject): Boolean;
begin
  Result := (AValue = FColumnHeaderImages) or (AValue = FGroupHeaderImages) or
    (AValue = FLargeImages) or (AValue = FSmallImages) or
    (AValue = FStateImages);
end;

function TdxListViewImages.CreateImagesChangeLink: TChangeLink;
begin
  Result := TChangeLink.Create;
  Result.OnChange := ImageListChanged;
end;

procedure TdxListViewImages.ImageListChanged(Sender: TObject);
begin
  if AreImagesLinked(Sender) then
    ListView.LayoutChanged;
end;

procedure TdxListViewImages.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = ColumnHeaderImages then
      ColumnHeaderImages := nil;
    if AComponent = GroupHeaderImages then
      GroupHeaderImages := nil;
    if AComponent = LargeImages then
      LargeImages := nil;
    if AComponent = SmallImages then
      SmallImages := nil;
    if AComponent = StateImages then
      StateImages := nil;
  end;
end;

procedure TdxListViewImages.SetColumnHeaderImages(const AValue
  : TCustomImageList);
begin
  cxSetImageList(AValue, FColumnHeaderImages, FColumnHeaderImagesChangeLink,
    ListView);
end;

procedure TdxListViewImages.SetGroupHeaderImages(const AValue
  : TCustomImageList);
begin
  cxSetImageList(AValue, FGroupHeaderImages, FGroupHeaderImagesChangeLink,
    ListView);
end;

procedure TdxListViewImages.SetLargeImages(const AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FLargeImages, FLargeImagesChangeLink, ListView);
end;

procedure TdxListViewImages.SetSmallImages(const AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FSmallImages, FSmallImagesChangeLink, ListView);
end;

procedure TdxListViewImages.SetStateImages(const AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FStateImages, FStateImagesChangeLink, ListView);
  if AValue <> nil then
    ListView.Options.ShowCheckBoxes := False;
end;

{ TdxListViewAutoScrollHelper }

constructor TdxListViewAutoScrollHelper.Create(AOwner: TdxCustomListView;
const AAfterScroll: TProc);
begin
  inherited Create(AOwner);
  FCurrentDirection := dirNone;
  FAfterScroll := AAfterScroll;
  Calculate;
end;

destructor TdxListViewAutoScrollHelper.Destroy;
begin
  StopTimer;
  FAfterScroll := nil;
  inherited Destroy;
end;

procedure TdxListViewAutoScrollHelper.Calculate;
const
  ScrollAreaSize = 30;
var
  ASize: Integer;
begin
  FClientBounds := ListView.ClientBounds;
  ASize := ListView.ScaleFactor.Apply(ScrollAreaSize);
  if ListView.IsScrollBarActive(sbHorizontal) then
  begin
    FHasScrollArea[sbHorizontal] := True;
    FScrollArea[dirLeft] := TRect.Create(FClientBounds.Left, FClientBounds.Top,
      FClientBounds.Left + ASize, FClientBounds.Bottom);
    FScrollArea[dirRight] := TRect.Create(FClientBounds.Right - ASize,
      FClientBounds.Top, FClientBounds.Right, FClientBounds.Bottom);
  end
  else
    FHasScrollArea[sbHorizontal] := False;
  if ListView.IsScrollBarActive(sbVertical) then
  begin
    FHasScrollArea[sbVertical] := True;
    FScrollArea[dirUp] := TRect.Create(FClientBounds.Left, FClientBounds.Top,
      FClientBounds.Right, FClientBounds.Top + ASize);
    FScrollArea[dirDown] := TRect.Create(FClientBounds.Left,
      FClientBounds.Bottom - ASize, FClientBounds.Right, FClientBounds.Bottom);
  end
  else
    FHasScrollArea[sbVertical] := False;
end;

procedure TdxListViewAutoScrollHelper.CheckMousePosition
  (const APosition: TPoint);
var
  ADirection: TcxDirection;
begin
  ADirection := GetDirection(APosition);
  if ADirection <> FCurrentDirection then
    StopTimer;
  if ADirection <> dirNone then
    StartTimer(ADirection);
end;

function TdxListViewAutoScrollHelper.GetDirection(const APosition: TPoint)
  : TcxDirection;
begin
  if FHasScrollArea[sbHorizontal] then
  begin
    if FScrollArea[dirLeft].Contains(APosition) then
      Exit(dirLeft);
    if FScrollArea[dirRight].Contains(APosition) then
      Exit(dirRight);
  end;
  if FHasScrollArea[sbVertical] then
  begin
    if FScrollArea[dirUp].Contains(APosition) then
      Exit(dirUp);
    if FScrollArea[dirDown].Contains(APosition) then
      Exit(dirDown);
  end;
  Result := dirNone;
end;

function TdxListViewAutoScrollHelper.GetScrollBar(AKind: TScrollBarKind)
  : IcxControlScrollBar;
begin
  if AKind = sbHorizontal then
    Result := ListView.HScrollBar
  else
    Result := ListView.VScrollBar;
end;

procedure TdxListViewAutoScrollHelper.GetScrollBarParams(AKind: TScrollBarKind;
var AMin, AMax, APos: Integer);
begin
  with GetScrollBar(AKind) do
  begin
    AMin := Min;
    AMax := Max - PageSize + 1;
    APos := Position;
  end;
end;

procedure TdxListViewAutoScrollHelper.OnTimer(Sender: TObject);
var
  ADirection: TcxDirection;
begin
  ADirection := GetDirection(ListView.GetMouseCursorClientPos);
  if ADirection = FCurrentDirection then
    case ADirection of
      dirLeft:
        Scroll(sbHorizontal, scLineUp);
      dirRight:
        Scroll(sbHorizontal, scLineDown);
      dirUp:
        Scroll(sbVertical, scLineUp);
      dirDown:
        Scroll(sbVertical, scLineDown);
    end;
end;

procedure TdxListViewAutoScrollHelper.Scroll(AKind: TScrollBarKind;
ACode: TScrollCode);
var
  AMin, AMax, APos, ANewPos: Integer;
begin
  GetScrollBarParams(AKind, AMin, AMax, APos);
  ANewPos := APos + ListView.ScrollStep;
  if ANewPos < AMin then
    ANewPos := AMin
  else if ANewPos > AMax then
    ANewPos := AMax;
  ListView.Scroll(AKind, ACode, ANewPos);
  if Assigned(FAfterScroll) then
    FAfterScroll;
end;

procedure TdxListViewAutoScrollHelper.StartTimer(ADirection: TcxDirection);
begin
  StopTimer;
  FTimer := TcxTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FTimer.Interval := 333;
  FCurrentDirection := ADirection;
end;

procedure TdxListViewAutoScrollHelper.StopTimer;
begin
  FCurrentDirection := dirNone;
  FreeAndNil(FTimer);
end;

{ TdxCustomListView }

function DefaultListViewSort(AItem1, AItem2: TdxListItem;
AData: Integer): Integer;
begin
  with AItem1 do
    if Assigned(ListView.OnCompare) then
      ListView.OnCompare(ListView, AItem1, AItem2, AData, Result)
    else
      Result := CompareStr(AItem1.Caption, AItem2.Caption)
end;

constructor TdxCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := False;
  ControlStyle := ControlStyle - [csParentBackground];

  CanBeFocused := True;
  ShowHint := True;
  Keys := [kArrows];
  BorderStyle := cxcbsDefault;
  Width := 150;
  Height := 100;

  FFonts := CreateFonts;
  FOptions := CreateOptions;
  FImages := CreateImages;
  FColumns := CreateColumns;
  FGroups := CreateGroups;
  FItems := CreateItems;
  FTempItem := CreateListItem;
  FEditingItemIndex := -1;
end;

destructor TdxCustomListView.Destroy;
begin
  FTempItem.Free;
  FreeAndNil(FInplaceEditImplementator);
  FreeAndNil(FItems);
  FreeAndNil(FGroups);
  FreeAndNil(FColumns);
  FreeAndNil(FOptions);
  FreeAndNil(FImages);
  FreeAndNil(FFonts);
  inherited Destroy;
end;

procedure TdxCustomListView.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomListView.CreateAutoScrollHelper(const AAfterScroll: TProc);
begin
  DestroyAutoScrollHelper;
  FAutoScrollHelper := TdxListViewAutoScrollHelper.Create(Self, AAfterScroll);
end;

procedure TdxCustomListView.DestroyAutoScrollHelper;
begin
  FreeAndNil(FAutoScrollHelper);
end;

function TdxCustomListView.IsDragging: Boolean;
begin
  Result := Dragging or (DragAndDropState <> ddsNone);
end;

function TdxCustomListView.CanDrag(X, Y: Integer): Boolean;
begin
  Result := inherited CanDrag(X, Y) and Controller.CanDrag(TPoint.Create(X, Y));
end;

procedure TdxCustomListView.DrawDragImage(ACanvas: TcxCanvas; const R: TRect);
const
  FrameColor = $FFD199;
  ShadowSize = 4;
var
  AColor: TColor;
  ABounds, AShadowHLineBounds, AShadowVLineBounds: TRect;
  I: Integer;
begin
  ABounds := R;
  Dec(ABounds.Right, ShadowSize);
  Dec(ABounds.Bottom, ShadowSize);
  ACanvas.Rectangle(ABounds, $FFE8CC, FrameColor, psSolid);
  AShadowVLineBounds.Init(ABounds.Right, ABounds.Top + 1, ABounds.Right + 1,
    ABounds.Bottom + 1);
  AShadowHLineBounds.Init(ABounds.Left + 1, ABounds.Bottom, ABounds.Right + 1,
    ABounds.Bottom + 1);
  for I := 1 to ShadowSize do
  begin
    AColor := TdxColorHelper.ChangeLightness(clBlack, 0.25 + I / 6);
    ACanvas.FillRect(AShadowHLineBounds, AColor);
    ACanvas.FillRect(AShadowVLineBounds, AColor);
    AShadowHLineBounds.Offset(1, 1);
    AShadowVLineBounds.Offset(1, 1);
  end;
  ABounds.Deflate(2);
  ACanvas.IntersectClipRect(ABounds);
  DrawDragImageContent(ACanvas, ABounds);
end;

procedure TdxCustomListView.DrawDragImageContent(ACanvas: TcxCanvas;
const ABounds: TRect);
var
  ACount: Integer;
  ASize: TSize;
  R: TRect;
  S: string;
begin
  ACount := SelectedItemCount;
  // if ACount > 1 then
  begin
    S := IntToStr(ACount);
    ACanvas.Font := Font;
    ACanvas.Font.Style := [fsBold];
    ASize := ACanvas.TextExtent(S);
    Inc(ASize.cx, 8);
    Inc(ASize.cy, 4);
    R := cxRectCenter(ABounds, ASize);
    ACanvas.Rectangle(R, RGB(20, 0, 80), clWhite, psSolid);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clWhite;
    ACanvas.TextOut(R.Left + 4, R.Top + 2, S);
  end;
end;

procedure TdxCustomListView.EndDragAndDrop(Accepted: Boolean);
begin
  Controller.EndDragAndDrop(Accepted);
  inherited EndDragAndDrop(Accepted);
end;

function TdxCustomListView.GetDragImagesSize: TPoint;
begin
  Result.Init(48, 48);
end;

function TdxCustomListView.HasDragImages: Boolean;
begin
  Result := True;
end;

procedure TdxCustomListView.BeginDragSelectOperation(AExtendedMode: Boolean);
begin
  FIsDragSelectPaintMode := True;
  FDragSelectRectangle.Empty;
  FOriginalSelection := TdxHashSet<Integer>.Create;
  if AExtendedMode then
    FOriginalSelection.Include(Controller.SelectedIndices.ToArray)
  else
    Controller.ClearSelection;
  FCurrentDragSelection := TdxHashSet<Integer>.Create;
  CreateAutoScrollHelper(procedure()
    var
      AAccepted: Boolean;
      ADragAndDropObject: TdxListViewDragSelectDragAndDropObject;
    begin
      if DragAndDropState = ddsInProcess then
      begin
        ADragAndDropObject := Safe<TdxListViewDragSelectDragAndDropObject>.Cast
          (DragAndDropObject);
        if ADragAndDropObject <> nil then
        begin
          AAccepted := True;
          ADragAndDropObject.DragAndDrop(GetMouseCursorClientPos, AAccepted);
        end;
      end;
    end);
end;

procedure TdxCustomListView.EndDragSelectOperation;
begin
  FIsDragSelectPaintMode := False;
  FreeAndNil(FCurrentDragSelection);
  FreeAndNil(FOriginalSelection);
  DestroyAutoScrollHelper;
  Invalidate;
end;

function TdxCustomListView.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxListViewDragSelectDragAndDropObject;
end;

function TdxCustomListView.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := Controller.StartDragAndDrop(P);
end;

procedure TdxCustomListView.UpdateDragSelectState(const ABounds: TRect;
AExtendedSelection: Boolean);

  function GetItemBounds(AItemIndex: Integer): TRect;
  var
    AItemViewInfo: TdxListItemCustomViewInfo;
  begin
    if not ViewInfo.FindItemViewInfo(AItemIndex, AItemViewInfo) then
      Result := ViewInfo.GetBoundsForItem(AItemIndex)
    else
      Result := AItemViewInfo.Bounds;
  end;

var
  I: Integer;
  R: TRect;
  ANewSelection: TdxHashSet<Integer>;
  ASelect, AControlPressed: Boolean;
begin
  R.InitSize(ABounds.TopLeft, Max(1, ABounds.Width), Max(1, ABounds.Height));
  if FDragSelectRectangle.IsEqual(R) then
    Exit;
  FDragSelectRectangle := R;
  Controller.ResetSelectionChangedFlag;
  ANewSelection := TdxHashSet<Integer>.Create;
  try
    AControlPressed := TdxKeyboard.IsControlPressed;
    for I := 0 to Items.Count - 1 do
    begin
      if AExtendedSelection and AControlPressed then
        ASelect := not FOriginalSelection.Contains(I)
      else
        ASelect := True;
      R := GetItemBounds(I);
      if R.IntersectsWith(FDragSelectRectangle) then
      begin
        ANewSelection.Include(I);
        Controller.SelectItem(I, ASelect);
      end
      else if FCurrentDragSelection.Contains(I) then
        Controller.SelectItem(I, not ASelect);
    end;
  finally
    FCurrentDragSelection.Free;
    FCurrentDragSelection := ANewSelection;
    Controller.CheckSelectionChangedFlag;
  end;
  Repaint;
end;

procedure TdxCustomListView.InitDragImages(ADragImages: TcxDragImageList);
var
  B: TcxBitmap32;
  ASize: TPoint;
begin
  ASize := GetDragImagesSize;
  ADragImages.DragHotspot := TPoint.Create(ASize.X div 2, ASize.Y - 8);
  ADragImages.Masked := False;
  ADragImages.Width := ASize.X;
  ADragImages.Height := ASize.Y;
  B := TcxBitmap32.Create;
  try
    B.SetSize(ASize.X, ASize.Y);
    DrawDragImage(B.cxCanvas, Rect(0, 0, ASize.X, ASize.Y));
    ADragImages.Add(B, nil);
  finally
    B.Free;
  end;
end;

procedure TdxCustomListView.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    LayoutChanged;
end;

function TdxCustomListView.AlphaSort: Boolean;
begin
  if HandleAllocated then
    Result := CustomSort(nil, 0)
  else
    Result := False;
end;

procedure TdxCustomListView.ApplyBestFit(AColumn: TdxListColumn = nil);
var
  I: Integer;
begin
  BeginUpdate;
  try
    if AColumn = nil then
    begin
      for I := 0 to Columns.Count - 1 do
      begin
        AColumn := Columns[I];
        AColumn.Width := ViewInfo.CalculateColumnBestFitWidth(AColumn);
      end;
      Exit;
    end;
    AColumn.Width := ViewInfo.CalculateColumnBestFitWidth(AColumn);
  finally
    EndUpdate;
  end;
end;

function TdxCustomListView.CanFocus: Boolean;
begin
  Result := inherited CanFocus and CanBeFocused;
end;

function TdxCustomListView.CustomSort(ASortProc: TdxListViewCompareProc;
AData: Integer): Boolean;
begin
  Result := False;
  if HandleAllocated and (FItems.Count > 1) then
  begin
    if not Assigned(ASortProc) then
      ASortProc := @DefaultListViewSort;
    BeginUpdate;
    try
      FItems.List.SortList(function(AItem1, AItem2: Pointer): Integer
        begin
          Result := ASortProc(TdxListItem(AItem1), TdxListItem(AItem2), AData);
        end, True);
      Result := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxCustomListView.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Owner = Root then
      Proc(Groups[I]);
  for I := 0 to Columns.Count - 1 do
    if Columns[I].Owner = Root then
      Proc(Columns[I]);
end;

procedure TdxCustomListView.MakeItemVisible(AItem: TdxListItem;
AVisibleType: TdxVisibilityType);
begin
  Controller.MakeItemVisible(AItem, AVisibleType);
end;

function TdxCustomListView.CreateColumns: TdxListColumns;
begin
  Result := TdxListColumns.Create(Self, GetColumnClass);
end;

function TdxCustomListView.CreateController: TdxListViewController;
begin
  Result := TdxListViewController.Create(Self);
end;

function TdxCustomListView.CreateInplaceEditImplementator: TCustomEdit;
begin
  if ViewStyle = TdxListViewStyle.Icon then
    Result := TdxListViewMultilineInplaceEdit.Create(nil)
  else
    Result := TdxListViewSingleLineInplaceEdit.Create(nil);
  Result.SetKeyPressEvent(InplaceEditKeyPress);
  Result.ReadOnly := Options.ReadOnly;
end;

function TdxCustomListView.CreateOptions: TdxListViewOptions;
begin
  Result := TdxListViewOptions.Create(Self);
end;

function TdxCustomListView.CreatePainter: TdxListViewPainter;
begin
  Result := TdxListViewPainter.Create(Self);
end;

function TdxCustomListView.CreateViewInfo: TdxListViewViewInfo;
begin
  Result := TdxListViewViewInfo.Create(Self);
end;

function TdxCustomListView.CreateListItem: TdxListItem;
var
  AClass: TdxListItemClass;
begin
  AClass := TdxListItem;
  if Assigned(FOnCreateItemClass) then
    FOnCreateItemClass(Self, AClass);
  Result := AClass.Create(Items);
end;

procedure TdxCustomListView.CreateViewSubClasses;
begin
  FViewInfo := CreateViewInfo;
  FController := CreateController;
  FPainter := CreatePainter;
end;

procedure TdxCustomListView.DestroyViewSubClasses;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FPainter);
end;

function TdxCustomListView.GetColumnClass: TdxListColumnClass;
begin
  Result := TdxListColumn;
end;

function TdxCustomListView.GetGroupClass: TdxListGroupClass;
begin
  Result := TdxListGroup;
end;

procedure TdxCustomListView.FocusEnter;
begin
  inherited FocusEnter;
  Controller.FocusEnter;
end;

procedure TdxCustomListView.FocusLeave;
begin
  inherited FocusLeave;
  Controller.FocusLeave;
end;

procedure TdxCustomListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.BeforeKeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
  Controller.AfterKeyDown(Key, Shift);
end;

procedure TdxCustomListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  Controller.KeyUp(Key, Shift);
end;

procedure TdxCustomListView.MouseUp(Button: TMouseButton; Shift: TShiftState;
X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, TPoint.Create(X, Y));
end;

procedure TdxCustomListView.Click;
begin
  inherited Click;
  Controller.Click;
end;

procedure TdxCustomListView.DblClick;
begin
  inherited DblClick;
  Controller.DblClick;
end;

procedure TdxCustomListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.BeforeMouseDown(Button, Shift, TPoint.Create(X, Y));
  Controller.MouseDown(Button, Shift, TPoint.Create(X, Y));
  Controller.AfterMouseDown(Button, Shift, TPoint.Create(X, Y));
end;

procedure TdxCustomListView.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

procedure TdxCustomListView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, TPoint.Create(X, Y));
end;

procedure TdxCustomListView.DragOver(Source: TObject; X, Y: Integer;
State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  case State of
    dsDragEnter:
      Controller.DragEnter;
    dsDragLeave:
      Controller.DragLeave;
    dsDragMove:
      Controller.DragOver(Source, TPoint.Create(X, Y), Accept);
  end;
end;

function TdxCustomListView.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TdxListViewDragObject;
end;

procedure TdxCustomListView.DragDrop(Source: TObject; X, Y: Integer);
begin
  Controller.DragDrop(Source, X, Y);
  Controller.ResetDropTarget;
  inherited DragDrop(Source, X, Y);
end;

function TdxCustomListView.StartDrag(DragObject: TDragObject): Boolean;
// var
// AHoveredItem: TdxListItem;
// AItemDragObject: TdxListViewDragObject;
begin
  if DragObject is TdxListViewDragObject then
  begin

    Result := True; // AItemDragObject.SelectedItems.Count > 0;
  end
  else
    Result := False;
end;

procedure TdxCustomListView.ShowInplaceEdit(AItemIndex: Integer; ABounds: TRect;
const AText: string);
begin
  InplaceEdit.Show(Self, ABounds, AText, Fonts.Item, 0, MaxInt, 0);
end;

procedure TdxCustomListView.ValidatePasteText(var AText: string);
begin
end;

function TdxCustomListView.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  Result := False;
end;

procedure TdxCustomListView.Loaded;
begin
  Columns.ValidateCreateIndices;
  Items.FixupGroups;
  inherited Loaded;
end;

procedure TdxCustomListView.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Images <> nil then
    Images.Notification(AComponent, Operation);
end;

procedure TdxCustomListView.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_PAINT) and (FLockCount > 0) then
  begin
    Message.Result := 0;
    Exit;
  end;
  inherited WndProc(Message);
end;

function TdxCustomListView.GetContentSize: TSize;
var
  ABounds: TRect;
begin
  if ViewInfo = nil then
    Exit(ClientBounds.Size);
  ABounds := ViewInfo.ContentBounds;
  ABounds.Inflate(Options.PaddingOptions.View.Margin);
  Result := ABounds.Size;
  if GetScrollbarMode = sbmHybrid then
  begin
    if IsScrollBarActive(sbHorizontal) then
      Inc(Result.cy, GetHScrollBarDefaultAreaHeight);
    if IsScrollBarActive(sbVertical) then
      Inc(Result.cx, GetVScrollBarDefaultAreaWidth);
  end;
end;

function TdxCustomListView.GetMouseWheelScrollingKind
  : TcxMouseWheelScrollingKind;
begin
  if ViewInfo = nil then
    Result := inherited GetMouseWheelScrollingKind
  else if ViewInfo.AreGroupsVertical then
    Result := mwskVertical
  else
    Result := mwskHorizontal;
end;

function TdxCustomListView.GetScrollStep: Integer;
begin
  if HandleAllocated then
    Result := ViewInfo.ItemSize.cy + Options.Icons.ItemsGap
  else
    Result := inherited GetScrollStep;
end;

function TdxCustomListView.IsScrollDataValid: Boolean;
begin
  Result := inherited IsScrollDataValid and not IsDestroying;
end;

procedure TdxCustomListView.BoundsChanged;
begin
  FinishItemCaptionEditing;
  LayoutChanged(ctMedium);
end;

procedure TdxCustomListView.Calculate(AType: TdxChangeType);
begin
  ViewInfo.Calculate(AType, ClientBounds);
end;

procedure TdxCustomListView.LayoutChanged(AType: TdxChangeType);
begin
  if not IsUpdateLocked then
    inherited LayoutChanged(AType);
end;

procedure TdxCustomListView.Scroll(AScrollBarKind: TScrollBarKind;
AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  FinishItemCaptionEditing(True);
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TdxCustomListView.ScrollPosChanged(const AOffset: TPoint);

  function HasScrollBarCapture: Boolean;
  var
    AScrollBar: TcxScrollBar;
  begin
    AScrollBar := VScrollBar.Control;
    Result := (AScrollBar <> nil) and AScrollBar.HandleAllocated and
      (AScrollBar.Handle = GetCapture);
    if Result then
      Exit;
    AScrollBar := HScrollBar.Control;
    Result := (AScrollBar <> nil) and AScrollBar.HandleAllocated and
      (AScrollBar.Handle = GetCapture);
  end;

begin
  LayoutChanged(ctLight);
  if not HasScrollBarCapture then
    Controller.UpdateMouseHottrack;
  Repaint;
end;

function TdxCustomListView.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomListView.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  BeginUpdate;
  try
    FinishItemCaptionEditing;
    inherited ChangeScaleEx(M, D, isDpiChange);
    DoChangeScaleEx(M, D, isDpiChange);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomListView.CreateActualCanvas;
begin
  inherited CreateActualCanvas;
  CreateViewSubClasses;
  if HandleAllocated then
  begin
    LayoutChanged;
    Invalidate;
  end;
end;

procedure TdxCustomListView.FreeActualCanvas;
begin
  DestroyViewSubClasses;
  inherited FreeActualCanvas;
end;

procedure TdxCustomListView.DoCancelMode;
begin
  Controller.CancelMode;
  inherited DoCancelMode;
end;

procedure TdxCustomListView.DoChangeScaleEx(M, D: Integer;
isDpiChange: Boolean);
begin
  Options.ChangeScale(M, D);
end;

procedure TdxCustomListView.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Draw(ActualCanvas);
  if FIsDragSelectPaintMode and not FDragSelectRectangle.IsEmpty then
    Painter.DrawMultiSelectionRect(ActualCanvas, FDragSelectRectangle);
end;

procedure TdxCustomListView.FontChanged;
begin
  inherited FontChanged;
  LayoutChanged;
end;

function TdxCustomListView.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := Controller.GetCursor(TPoint.Create(X, Y));
  if Result = crDefault then
    Result := inherited GetCurrentCursor(X, Y);
end;

function TdxCustomListView.GetScrollContentForegroundColor: TColor;
var
  AState: TdxGalleryItemViewState;
begin
  ZeroMemory(@AState, SizeOf(AState));
  Result := LookAndFeelPainter.GetGalleryItemCaptionTextColor(AState);
end;

function TdxCustomListView.HasScrollBarArea: Boolean;
begin
  Result := inherited HasScrollBarArea or (GetScrollbarMode = sbmHybrid);
end;

function TdxCustomListView.IsActive: Boolean;
begin
  Result := Focused or IsEditing;
end;

function TdxCustomListView.ColumnsShowing: Boolean;
begin
  Result := ViewStyle = TdxListViewStyle.Report;
end;

function TdxCustomListView.CreateFonts: TdxListViewFonts;
begin
  Result := TdxListViewFonts.Create(Self);
end;

function TdxCustomListView.CreateGroups: TdxListGroups;
begin
  Result := TdxListGroups.Create(Self, GetGroupClass);
end;

function TdxCustomListView.CreateImages: TdxListViewImages;
begin
  Result := TdxListViewImages.Create(Self);
end;

function TdxCustomListView.CreateItems: TdxListItems;
begin
  Result := TdxListItems.Create(Self);
end;

procedure TdxCustomListView.DeleteItem(AItem: TdxListItem);
begin
  if (AItem <> nil) and (AItem <> TempItem) then
  begin
    if Assigned(FOnDeletion) then
      FOnDeletion(Self, AItem);
    LayoutChanged;
  end;
end;

procedure TdxCustomListView.DoColumnClick(AColumn: TdxListColumn);
begin
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, AColumn);
end;

procedure TdxCustomListView.DoColumnRightClick(AColumn: TdxListColumn;
APoint: TPoint);
begin
  if Assigned(FOnColumnRightClick) then
    FOnColumnRightClick(Self, AColumn, APoint);
end;

procedure TdxCustomListView.DoEdited(AItem: TdxListItem; var ACaption: string);
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self, AItem, ACaption);
end;

procedure TdxCustomListView.DoSelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TdxCustomListView.DoSelectItem(AItemIndex: Integer;
ASelected: Boolean);
var
  AItem: TdxListItem;
begin
  if Assigned(FOnSelectItem) then
  begin
    AItem := GetItem(AItemIndex);
    FOnSelectItem(Self, AItem, ASelected);
  end;
end;

procedure TdxCustomListView.DrawMultiSelection(const ABounds: TRect);
begin
  if ABounds.IsEqual(FDragSelectRectangle) then
    Exit;
  FDragSelectRectangle := ABounds;
  Repaint;
end;

function TdxCustomListView.GetItemAtPos(const P: TPoint): TdxListItem;
var
  AViewInfo: TdxListItemCustomViewInfo;
begin
  if ViewInfo.GetItemAtPos(P, AViewInfo) then
    Result := AViewInfo.Item
  else
    Result := nil;
end;

procedure TdxCustomListView.InsertItem(AItem: TdxListItem);
begin
  if Assigned(FOnInsert) then
    FOnInsert(Self, AItem);
end;

function TdxCustomListView.IsItemSelected(AItem: TdxListItem): Boolean;
begin
  Result := Controller.IsItemSelected(AItem.Index);
end;

function TdxCustomListView.IsUpdateLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading or IsDestroying or CreatingWindow;
end;

function TdxCustomListView.SupportsItemEnabledState: Boolean;
begin
  Result := True;
end;

function TdxCustomListView.OwnerData: Boolean;
begin
  Result := Options.OwnerData;
end;

procedure TdxCustomListView.ResetContent;
begin
  if Controller <> nil then
    Controller.ResetContent;
  SetLeftTop(TPoint.Null);
end;

procedure TdxCustomListView.UpdateItems(AFirstIndex, ALastIndex: Integer);
begin
end;

procedure TdxCustomListView.ClearSelection;
begin
  if Controller <> nil then
    Controller.ClearSelection;
end;

procedure TdxCustomListView.UpdateGroups;
begin
  LayoutChanged;
end;

procedure TdxCustomListView.ViewStyleChanged;
begin
end;

function TdxCustomListView.GetItem(AIndex: Integer): TdxListItem;
var
  Request: TItemRequest;
begin
  if OwnerData then
  begin
    if AIndex < 0 then
      Result := nil
    else if True { iSubItem = 0 } then
    begin
      Request := [irText, irImage, irParam, irState, irIndent];
      FTempItem.FIndex := AIndex;
      FTempItem.FData := nil; // Pointer(lParam);
      FTempItem.FSubItems.Clear;
      OwnerDataFetch(FTempItem, Request);
      Result := FTempItem;
    end
    else
      Result := FTempItem;
  end
  else
    Result := Items[AIndex];
end;

function TdxCustomListView.OwnerDataFetch(AItem: TdxListItem;
ARequest: TItemRequest): Boolean;
begin
  Result := Assigned(FOnData);
  if Result then
    FOnData(Self, AItem);
end;

function TdxCustomListView.OwnerDataFind(AFind: TItemFind;
const AFindString: string; const AFindPosition: TPoint; AFindData: TCustomData;
AStartIndex: Integer; ADirection: TSearchDirection; AWrap: Boolean): Integer;
begin
  Result := -1;
  if Assigned(FOnDataFind) then
    FOnDataFind(Self, AFind, AFindString, AFindPosition, AFindData, AStartIndex,
      ADirection, AWrap, Result);
end;

function TdxCustomListView.OwnerDataHint(AStartIndex,
  AEndIndex: Integer): Boolean;
begin
  Result := Assigned(FOnDataHint);
  if Result then
    FOnDataHint(Self, AStartIndex, AEndIndex);
end;

function TdxCustomListView.OwnerDataStateChange(AStartIndex, AEndIndex: Integer;
AOldState, ANewState: TItemStates): Boolean;
begin
  Result := Assigned(FOnDataStateChange);
  if Result then
    FOnDataStateChange(Self, AStartIndex, AEndIndex, AOldState, ANewState);
end;

function TdxCustomListView.CanEdit(AItem: TdxListItem): Boolean;
begin
  Result := not SupportsItemEnabledState or AItem.Enabled;
  if Assigned(FOnEditing) then
    FOnEditing(Self, AItem, Result);
end;

procedure TdxCustomListView.Edit(AItemIndex: Integer; const AText: string);
var
  ABounds: TRect;
  AItemViewInfo: TdxListItemCustomViewInfo;
begin
  Controller.FocusedItemIndex := AItemIndex;
  if ViewInfo.FindItemViewInfo(AItemIndex, AItemViewInfo) then
  begin
    ABounds := AItemViewInfo.TextAreaBounds;
    if ViewStyle = TdxListViewStyle.Icon then
    begin
      ABounds.Left := AItemViewInfo.Bounds.Left;
      ABounds.Right := AItemViewInfo.Bounds.Right;
    end;
    ShowInplaceEdit(AItemIndex, ABounds, AText);
    FEditingItemIndex := AItemIndex;
  end;
end;

function TdxCustomListView.GetEditingText(AItem: TdxListItem): string;
begin
  Result := AItem.Caption;
end;

procedure TdxCustomListView.InplaceEditKeyPress(Sender: TObject; var Key: Char);
begin
end;

function TdxCustomListView.IsEditingItem(AItemIndex: Integer): Boolean;
begin
  Result := IsEditing and (FEditingItemIndex = AItemIndex);
end;

function TdxCustomListView.IsEditing: Boolean;
begin
  Result := HandleAllocated and (FEditingItemIndex >= 0);
end;

function TdxCustomListView.StartItemCaptionEditing(AItemIndex: Integer)
  : Boolean;
var
  AItem: TdxListItem;
begin
  if AItemIndex < 0 then
    Exit(False);
  AItem := GetItem(AItemIndex);
  Result := CanEdit(AItem);
  if Result then
    Edit(AItemIndex, GetEditingText(AItem));
end;

procedure TdxCustomListView.FinishItemCaptionEditing(AAccept: Boolean = True);
var
  ACaption: string;
  AItem: TdxListItem;
begin
  if IsEditing then
    try
      if AAccept then
      begin
        ACaption := InplaceEdit.Value;
        AItem := EditingItem;
        DoEdited(AItem, ACaption);
        AItem.Caption := ACaption;
      end;
    finally
      FEditingItemIndex := -1;
      InplaceEdit.Hide;
      Controller.CancelEdit;
      Invalidate;
    end;
end;

function TdxCustomListView.AreItemsStored: Boolean;
begin
  Result := not OwnerData;
end;

function TdxCustomListView.GetEditingItem: TdxListItem;
begin
  if FEditingItemIndex < 0 then
    Result := nil
  else
    Result := GetItem(FEditingItemIndex);
end;

function TdxCustomListView.GetFocusedItem: TdxListItem;
begin
  if Controller.FocusedItemIndex < 0 then
    Result := nil
  else
    Result := GetItem(Controller.FocusedItemIndex);
end;

function TdxCustomListView.GetInplaceEdit: IdxListViewInplaceEdit;
begin
  if FInplaceEditImplementator = nil then
    FInplaceEditImplementator := CreateInplaceEditImplementator;
  Supports(FInplaceEditImplementator, IdxListViewInplaceEdit, Result);
end;

function TdxCustomListView.GetSelectedItemCount: Integer;
begin
  Result := Controller.SelectedIndices.Count;
end;

function TdxCustomListView.GetSelectedItem(Index: Integer): TdxListItem;
begin
  Result := GetItem(Controller.SelectedIndices[Index]);
end;

procedure TdxCustomListView.SetColumns(const AValue: TdxListColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TdxCustomListView.SetFocusedItem(AItem: TdxListItem);
begin
  if AItem <> nil then
    AItem.Focused := True
  else
    Controller.FocusedItemIndex := -1;
end;

procedure TdxCustomListView.SetFonts(const AValue: TdxListViewFonts);
begin
  FFonts.Assign(AValue);
end;

procedure TdxCustomListView.SetGroups(const AValue: TdxListGroups);
begin
  FGroups.Assign(AValue);
end;

procedure TdxCustomListView.SetImages(const AValue: TdxListViewImages);
begin
  FImages.Assign(AValue);
end;

procedure TdxCustomListView.SetItems(const AValue: TdxListItems);
begin
  FItems.Assign(AValue);
end;

procedure TdxCustomListView.Change(AItem: TdxListItem; AChange: Integer);
begin
end;

procedure TdxCustomListView.SetOptions(const AValue: TdxListViewOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TdxCustomListView.SetViewStyle(const AValue: TdxListViewStyle);
begin
  if FViewStyle <> AValue then
  begin
    BeginUpdate;
    try
      FViewStyle := AValue;
      SetLeftTop(TPoint.Null);
      FinishItemCaptionEditing(True);
      FreeAndNil(FInplaceEditImplementator);
      ViewStyleChanged;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxCustomListView.CMHintShow(var Message: TCMHintShow);
var
  AItemViewInfo: TdxListItemCustomViewInfo;
  AItem: TdxListItem;
begin
  if Options.ItemShowHint and not IsDragging then
  begin
    if ViewInfo.GetItemAtPos(Message.HintInfo.CursorPos, AItemViewInfo) then
    begin
      AItem := AItemViewInfo.Item;
      if AItem.Enabled and (AItem.Hint <> '') then
      begin
        Message.HintInfo.CursorRect := AItemViewInfo.Bounds;
        Message.HintInfo.HintStr := AItem.Hint;
        Message.Result := 0;
      end;
    end;
  end;
end;

procedure TdxCustomListView.CMParentFontChanged(var Message
  : TCMParentFontChanged);
begin
  inherited;
  if ParentFont then
  begin
    Fonts.FLockChanges := True;
    try
      Fonts.SetFont(Font, True);
    finally
      Fonts.FLockChanges := True;
    end;
    LayoutChanged;
  end;
end;

{ TdxImageListPaintCache.TImageListCacheHelper.TImages.TImageComparer }

function TdxImageListPaintCache.TImageIdComparer.Equals(const Left,
  Right: TdxDrawImageCacheID): Boolean;
begin
  Result := Left.IsEqual(Right);
end;

function TdxImageListPaintCache.TImageIdComparer.GetHashCode
  (const Value: TdxDrawImageCacheID): Integer;
begin
  Result := Value.GetHashCode;
end;

{ TdxListViewCellViewParams }

destructor TdxListViewCellViewParams.Destroy;
begin
  FreeAndNil(Font);
  inherited Destroy;
end;

function TdxListViewCellViewParams.GetNonTextWidth: Integer;
begin
  Result := Padding.Width;
  if GlyphsAreaSize.cx > 0 then
    Inc(Result, GlyphsAreaSize.cx + GlyphIndent);
end;

function TdxListViewCellViewParams.GetReportNonTextWidth(ACheckState,
  AGheckGlyph: Boolean): Integer;
begin
  Result := Padding.Width;
  if ACheckState and (StateGlyphSize.cx > 0) then
    Inc(Result, StateGlyphSize.cx + GlyphIndent);
  if AGheckGlyph and (GlyphSize.cx > 0) then
    Inc(Result, GlyphSize.cx + GlyphIndent);
end;

{ TdxKeyboard }

class function TdxKeyboard.CheckIsKeyPressed(const Index: Integer): Boolean;
begin
  Result := GetAsyncKeyState(Index) < 0;
end;

initialization

RegisterClasses([TdxListItem, TdxListGroup, TdxListColumn]);

end.
