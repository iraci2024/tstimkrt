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

unit dxGalleryControl;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Types, Graphics, Forms, Controls, StdCtrls,
  Messages, ImgList, cxDrawTextUtils,
  dxCore, dxCoreClasses, cxClasses, cxControls, dxGDIPlusClasses,
  cxLookAndFeels, cxLookAndFeelPainters,
  cxGraphics, cxGeometry, dxGallery, dxCoreGraphics, cxCustomCanvas,
  dxDirectX.D2D.Classes;

type
  TdxCustomGalleryControl = class;
  TdxGalleryControlGroup = class;
  TdxGalleryControlItem = class;
  TdxGalleryControlPainter = class;
  TdxGalleryControlPainterClass = class of TdxGalleryControlPainter;
  TdxGalleryControlOptionsItemImage = class;
  TdxGalleryControlOptionsItemText = class;
  TdxGalleryControlOptionsView = class;

  TdxGalleryControlItemEvent = procedure(Sender: TObject;
    AItem: TdxGalleryControlItem) of object;

  TdxGalleryControlItemMatrix = array of array of TdxGalleryControlItem;

  TdxGalleryItemMultiSelectKind = (imskGallery, imskListView);

  { TdxGalleryPersistent }

  TdxGalleryPersistent = class(TcxOwnedPersistent)
  strict private
    function GetOwnerControl: TdxCustomGalleryControl;
  protected
    property Owner: TdxCustomGalleryControl read GetOwnerControl;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); reintroduce; virtual;
  end;

  { TdxGalleryCustomViewInfo }

  TdxGalleryCustomViewInfo = class
  strict private
    FGalleryControl: TdxCustomGalleryControl;

    function GetContentOffset: TRect; inline;
    function GetContentOffsetGroups: TRect; inline;
    function GetContentOffsetItems: TRect; inline;
    function GetOptionsItemImage: TdxGalleryControlOptionsItemImage; inline;
    function GetOptionsItemText: TdxGalleryControlOptionsItemText; inline;
    function GetOptionsView: TdxGalleryControlOptionsView; inline;
    function GetPainter: TdxGalleryControlPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected
    FBounds: TRect;

    procedure DrawContent(ACanvas: TcxCustomCanvas); virtual; abstract;
  public
    constructor Create(AGalleryControl: TdxCustomGalleryControl);
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); virtual;
    procedure Draw(ACanvas: TcxCustomCanvas);
    //
    property Bounds: TRect read FBounds;
    property ContentOffset: TRect read GetContentOffset;
    property ContentOffsetGroups: TRect read GetContentOffsetGroups;
    property ContentOffsetItems: TRect read GetContentOffsetItems;
    property GalleryControl: TdxCustomGalleryControl read FGalleryControl;
    property OptionsItemImage: TdxGalleryControlOptionsItemImage
      read GetOptionsItemImage;
    property OptionsItemText: TdxGalleryControlOptionsItemText
      read GetOptionsItemText;
    property OptionsView: TdxGalleryControlOptionsView read GetOptionsView;
    property Painter: TdxGalleryControlPainter read GetPainter;
    // for internal use
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxGalleryItemViewInfo }

  TdxGalleryItemViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
  const
    WordWrapMap: array [Boolean] of Integer = (0, CXTO_WORDBREAK);
  strict private
    FCacheGlyph: TcxCanvasBasedImage;
    FCacheGlyphColorPalette: Pointer;
    FItem: TdxGalleryControlItem;

    function GetCacheGlyph: TcxCanvasBasedImage;
    function GetCaption: string;
    function GetDescription: string;
    function GetGlyphSize: TSize;
  protected
    FCaptionRect: TRect;
    FCaptionSize: TSize;
    FCaptionTextLayout: TcxCanvasBasedTextLayout;
    FCellPositionInGroup: TPoint;
    FContentBounds: TRect;
    FDescriptionRect: TRect;
    FDescriptionSize: TSize;
    FDescriptionTextLayout: TcxCanvasBasedTextLayout;
    FGlyphFrameRect: TRect;
    FGlyphRect: TRect;
    FState: TdxGalleryItemViewState;
    FTextArea: TRect;

    procedure CalculateGlyphArea(const AGlyphSize: TSize); virtual;
    procedure CalculateTextArea(const ATextAreaSize: TSize); virtual;
    procedure CalculateTextAreaContent(const ABounds: TRect); virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetDescriptionOffset: Integer; virtual;
    function GetTextAreaSize: TSize; virtual;
    procedure ResetCache; virtual;
    procedure UpdateFonts; virtual;
  public
    constructor Create(AGalleryControl: TdxCustomGalleryControl;
      AItem: TdxGalleryControlItem); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    procedure CalculateTextAreaSizeLimitedByRowCount
      (ARowCount: Integer); virtual;
    procedure CalculateTextAreaSizeLimitedByWidth(AMaxWidth,
      AMaxRowCount: Integer); virtual;

    property CacheGlyph: TcxCanvasBasedImage read GetCacheGlyph;
    // for internal use
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionSize: TSize read FCaptionSize;
    property CaptionTextLayout: TcxCanvasBasedTextLayout
      read FCaptionTextLayout; // for internal use
    property CellPositionInGroup: TPoint read FCellPositionInGroup;
    property ContentBounds: TRect read FContentBounds;
    property Description: string read GetDescription;
    property DescriptionOffset: Integer read GetDescriptionOffset;
    property DescriptionRect: TRect read FDescriptionRect;
    property DescriptionSize: TSize read FDescriptionSize;
    property DescriptionTextLayout: TcxCanvasBasedTextLayout
      read FDescriptionTextLayout; // for internal use
    property GlyphFrameRect: TRect read FGlyphFrameRect;
    property GlyphRect: TRect read FGlyphRect;
    property GlyphSize: TSize read GetGlyphSize;
    property Item: TdxGalleryControlItem read FItem;
    property State: TdxGalleryItemViewState read FState; // for internal use
    property TextArea: TRect read FTextArea;
    property TextAreaSize: TSize read GetTextAreaSize;
  end;

  { TdxGalleryControlItem }

  TdxGalleryControlItem = class(TdxGalleryItem)
  strict private
    function GetGroup: TdxGalleryControlGroup; inline;
    function GetGalleryControl: TdxCustomGalleryControl; inline;
    function GetImages: TCustomImageList; inline;
    procedure SetGroup(AGroup: TdxGalleryControlGroup);
  protected
    property GalleryControl: TdxCustomGalleryControl read GetGalleryControl;
    property Images: TCustomImageList read GetImages;
  public
    property Group: TdxGalleryControlGroup read GetGroup write SetGroup;
  published
    property Caption;
    property Checked;
    property Description;
    property Enabled;
    property Glyph;
    property Hint;
    property ImageIndex;
  end;

  { TdxGalleryControlItems }

  TdxGalleryControlItems = class(TdxGalleryItems)
  strict private
    function GetGalleryControl: TdxCustomGalleryControl;
    function GetItem(AIndex: Integer): TdxGalleryControlItem;
    procedure SetItem(AIndex: Integer; AValue: TdxGalleryControlItem);
  protected
    procedure SetItemName(AItem: TcxComponentCollectionItem;
      ABaseIndex: Integer = -1); override;
  public
    function Add: TdxGalleryControlItem;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;

    property Items[AIndex: Integer]: TdxGalleryControlItem read GetItem
      write SetItem; default;
  end;

  { TdxGalleryGroupViewInfo }

  TdxGalleryGroupViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
    FGroup: TdxGalleryControlGroup;

    function GetCaption: string;
    function GetItem(Index: Integer): TdxGalleryItemViewInfo; inline;
    function GetItemCount: Integer; inline;
    function GetSize: TSize;
  protected
    FCaptionHeight: Integer;
    FCaptionRect: TRect;
    FCaptionTextLayout: TcxCanvasBasedTextLayout;
    FCaptionTextRect: TRect;
    FColumnCount: Integer;
    FItems: TcxObjectList;
    FItemsRect: TRect;
    FRowCount: Integer;

    procedure CalculateCaption(AType: TdxChangeType); virtual;
    procedure CalculateItems(AType: TdxChangeType); virtual;
    procedure CreateCaptionTextLayout; virtual;
    procedure CreateSubItems; virtual;
    function CreateItemViewInfo(AItem: TdxGalleryControlItem)
      : TdxGalleryItemViewInfo; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetCaptionTextOffsets: TRect; virtual;
    procedure PlaceItem(AItem: TdxGalleryItemViewInfo;
      AChangeType: TdxChangeType; const AItemsArea: TRect; ACellIndex: Integer);
    procedure UpdateFonts; virtual;
  public
    constructor Create(AGalleryControl: TdxCustomGalleryControl;
      AGroup: TdxGalleryControlGroup); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    function FindItemViewInfo(AItem: TdxGalleryControlItem;
      out AViewInfo: TdxGalleryItemViewInfo): Boolean; virtual;
    function GetItemAtPos(const P: TPoint;
      out AViewInfo: TdxGalleryItemViewInfo): Boolean; virtual;
    function GetMaxColumnCount: Integer; virtual;
    function HasCaption: Boolean; virtual;
    //
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionTextLayout: TcxCanvasBasedTextLayout
      read FCaptionTextLayout; // for internal use
    property CaptionTextOffsets: TRect read GetCaptionTextOffsets;
    property CaptionTextRect: TRect read FCaptionTextRect;
    property ColumnCount: Integer read FColumnCount;
    property Group: TdxGalleryControlGroup read FGroup;
    property ItemSize: TSize read GetSize;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxGalleryItemViewInfo read GetItem;
    // for internal use
    property ItemsRect: TRect read FItemsRect;
    property RowCount: Integer read FRowCount;
  end;

  { TdxGalleryControlGroup }

  TdxGalleryControlGroup = class(TdxGalleryGroup)
  strict private
    function GetGalleryControl: TdxCustomGalleryControl;
    function GetItems: TdxGalleryControlItems;
  protected
    function GetGalleryItemClass: TdxGalleryItemClass; override;
    function GetGalleryItemsClass: TdxGalleryItemsClass; override;

    property GalleryControl: TdxCustomGalleryControl read GetGalleryControl;
  public
    property Items: TdxGalleryControlItems read GetItems;
  published
    property Caption;
    property ShowCaption default True;
    property Visible default True;
  end;

  { TdxGalleryControlGroups }

  TdxGalleryControlGroups = class(TdxGalleryGroups)
  strict private
    function GetGalleryControl: TdxCustomGalleryControl;
    function GetGroup(AIndex: Integer): TdxGalleryControlGroup;
    procedure SetGroup(AIndex: Integer; AValue: TdxGalleryControlGroup);
  public
    function Add: TdxGalleryControlGroup;
    function FindByCaption(const ACaption: string;
      out AGroup: TdxGalleryControlGroup): Boolean;
    function GetGroupAtPos(const P: TPoint): TdxGalleryControlGroup;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;

    property Groups[AIndex: Integer]: TdxGalleryControlGroup read GetGroup
      write SetGroup; default;
  end;

  { TdxGalleryControlStructure }

  TdxGalleryControlStructure = class(TdxGallery)
  strict private
    function GetGroups: TdxGalleryControlGroups;
  protected
    function GetGroupClass: TdxGalleryGroupClass; override;
    function GetGroupsClass: TdxGalleryGroupsClass; override;
  public
    function GetCheckedItem: TdxGalleryControlItem;
    function GetFirstItem: TdxGalleryControlItem;
    function GetFirstVisibleItem: TdxGalleryControlItem;

    property Groups: TdxGalleryControlGroups read GetGroups;
  end;

  { TdxGalleryControlDropTargetViewInfo }

  TdxGalleryControlDropTargetViewInfo = class(TdxGalleryCustomViewInfo)
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
    //
    property Size: Integer read GetSize;
  public
    procedure Calculate(ATargetObject: TObject; const ATargetBounds: TRect;
      ASide: TcxBorder); reintroduce; virtual; // for internal use
    //
    property Side: TcxBorder read FSide;
    property TargetBounds: TRect read FTargetBounds;
    property TargetObject: TObject read FTargetObject;
  end;

  { TdxGalleryControlPainter }

  TdxGalleryControlPainter = class(TdxGalleryPersistent)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetUseRightToLeftAlignment: Boolean; inline;
  protected
    FGroupFont: TcxCanvasBasedFont;
    FItemFont: TcxCanvasBasedFont;

    function CreateCanvasBasedFont(AFont: TFont): TcxCanvasBasedFont; virtual;
    function CreateCanvasBasedImage(ABitmap: TBitmap;
      AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage; virtual;
    function CreateCanvasBasedTextLayout: TcxCanvasBasedTextLayout; virtual;

    function DrawItemSelectionFirst: Boolean; virtual;
    function GetDrawTextFlags(AAlignment: TAlignment;
      AWordWrap: Boolean): Integer;
    function GetGroupCaptionTextColor: TColor; virtual;
    function GetItemCaptionTextColor(AViewInfo: TdxGalleryItemViewInfo)
      : TColor; virtual;
    function GetItemDescriptionTextColor(AViewInfo: TdxGalleryItemViewInfo)
      : TColor; virtual;
    procedure UpdateFonts; virtual;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // General
    procedure FlushCache; virtual;
    // Background
    procedure DrawBackground(ACanvas: TcxCustomCanvas;
      const ABounds: TRect); virtual;
    // Group
    procedure DrawGroupHeader(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryGroupViewInfo); virtual;
    procedure DrawGroupHeaderBackground(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryGroupViewInfo); virtual;
    procedure DrawGroupHeaderText(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryGroupViewInfo); virtual;
    function GetGroupHeaderContentOffsets: TRect; virtual;
    // Items
    procedure DrawItem(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemImage(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemSelection(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryItemViewInfo); virtual;
    procedure DrawItemText(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryItemViewInfo); virtual;
    // DragDrop
    procedure DrawDropTargetSelection(ACanvas: TcxCustomCanvas;
      AViewInfo: TdxGalleryControlDropTargetViewInfo); virtual;
    //
    property GroupFont: TcxCanvasBasedFont read FGroupFont; // for internal use
    property ItemFont: TcxCanvasBasedFont read FItemFont; // for internal use
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxGalleryControlViewInfo }

  TdxGalleryControlViewInfo = class(TdxGalleryCustomViewInfo)
  strict private
    function GetAutoHeight: Boolean;
    function GetAutoWidth: Boolean;
    function GetColumnAutoWidth: Boolean;
    function GetFont: TFont; inline;
    function GetGroup(Index: Integer): TdxGalleryGroupViewInfo; inline;
    function GetGroupCount: Integer; inline;
    function GetMaxColumnCount: Integer;
    function GetRowCount: Integer;
  protected
    FAutoScrollBottomArea: TRect;
    FAutoScrollTopArea: TRect;
    FColumnCount: Integer;
    FContentBounds: TRect;
    FDropTarget: TdxGalleryControlDropTargetViewInfo;
    FGroups: TcxObjectList;
    FImageSize: TSize;
    FItemSize: TSize;
    FTextAreaSize: TSize;

    procedure CalculateAutoScrollAreas; virtual;
    procedure CalculateColumnCount; virtual;
    procedure CalculateContentBounds(AType: TdxChangeType); virtual;
    procedure CalculateItemSize; virtual;
    function CalculateMaxItemImageSize: TSize; virtual;
    function CalculateMaxItemTextAreaSizeLimitedByRowCount(ARowCount: Integer)
      : TSize; virtual;
    function CalculateMaxItemTextAreaSizeLimitedByWidth(AMaxWidth,
      AMaxRowCount: Integer): TSize; virtual;
    function CreateDropTargetViewInfo
      : TdxGalleryControlDropTargetViewInfo; virtual;
    function CreateGroupViewInfo(AGroup: TdxGalleryControlGroup)
      : TdxGalleryGroupViewInfo; virtual;
    function DoCalculateItemSize: TSize; virtual;
    procedure DrawContent(ACanvas: TcxCustomCanvas); override;
    function GetAvailableGroupsAreaWidth: Integer; virtual;
    function GetBorderWidths: TRect; virtual;
    function GetTextAreaMaxRowCount(const AImageSize: TSize): Integer; virtual;
    function GetTextAreaMaxWidth(const AImageSize: TSize): Integer; virtual;
    procedure RecreateSubItems; virtual;
    procedure UpdateFonts; virtual;
  public
    constructor Create(AGalleryControl: TdxCustomGalleryControl); virtual;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType; const ABounds: TRect); override;
    function FindGroupViewInfo(AGroup: TdxGalleryControlGroup;
      out AViewInfo: TdxGalleryGroupViewInfo): Boolean; overload;
    function FindItemViewInfo(AItem: TdxGalleryControlItem;
      out AViewInfo: TdxGalleryItemViewInfo): Boolean; overload;
    function GetGroupAtPos(const P: TPoint;
      out AViewInfo: TdxGalleryGroupViewInfo): Boolean; overload;
    function GetItemAtPos(const P: TPoint;
      out AViewInfo: TdxGalleryItemViewInfo): Boolean; overload;
    //
    property AutoHeight: Boolean read GetAutoHeight;
    property AutoScrollBottomArea: TRect read FAutoScrollBottomArea;
    property AutoScrollTopArea: TRect read FAutoScrollTopArea;
    property AutoWidth: Boolean read GetAutoWidth;
    property BorderWidths: TRect read GetBorderWidths;
    property ColumnAutoWidth: Boolean read GetColumnAutoWidth;
    property ColumnCount: Integer read FColumnCount;
    property ContentBounds: TRect read FContentBounds;
    property DropTarget: TdxGalleryControlDropTargetViewInfo read FDropTarget;
    // for internal use
    property Font: TFont read GetFont;
    property GroupCount: Integer read GetGroupCount;
    property Groups[Index: Integer]: TdxGalleryGroupViewInfo read GetGroup;
    // for internal use
    property ImageSize: TSize read FImageSize;
    property ItemSize: TSize read FItemSize;
    property MaxColumnCount: Integer read GetMaxColumnCount;
    property RowCount: Integer read GetRowCount;
    property TextAreaSize: TSize read FTextAreaSize;
  end;

  { TdxGalleryControlNavigationMatrix }

  TdxGalleryControlNavigationMatrix = class(TObject)
  private
    FColumnCount: Integer;
    FRowCount: Integer;
    FValues: TdxGalleryControlItemMatrix;
    function GetValue(ACol, ARow: Integer): TdxGalleryControlItem;
    procedure SetValue(ACol, ARow: Integer;
      const AValue: TdxGalleryControlItem);
  protected
    procedure Populate(AViewInfo: TdxGalleryControlViewInfo); virtual;
  public
    constructor Create(AViewInfo: TdxGalleryControlViewInfo);
    destructor Destroy; override;
    function GetRightMostItemIndex(ARow: Integer): Integer;
    //
    property ColumnCount: Integer read FColumnCount;
    property RowCount: Integer read FRowCount;
    property Values[ACol, ARow: Integer]: TdxGalleryControlItem read GetValue
      write SetValue;
  end;

  { TdxGalleryControlController }

  TdxGalleryControlController = class(TdxGalleryPersistent)
  strict private
    FAutoScrollTimer: TcxTimer;
    FDragCopy: Boolean;
    FIsInDragAndDropOperation: Boolean;
    FKeyPressed: Boolean;
    FKeySelectedItem: TdxGalleryControlItem;
    FMouseHoveredItem: TdxGalleryControlItem;
    FMousePressed: Boolean;
    FMousePressedItem: TdxGalleryControlItem;
    FNavigationMatrix: TdxGalleryControlNavigationMatrix;
    FStartSelectionItem: TdxGalleryControlItem;

    procedure AutoScrollTimerHandler(Sender: TObject);
    function CanChangeSelection(AButton: TMouseButton;
      AShift: TShiftState): Boolean;
    function GetDropTargetInfo: TdxGalleryControlDropTargetViewInfo;
    function GetGallery: TdxGalleryControlStructure; inline;
    function GetItemCheckMode: TdxGalleryItemCheckMode;
    function GetNavigationMatrix: TdxGalleryControlNavigationMatrix;
    function GetViewInfo: TdxGalleryControlViewInfo; inline;
    procedure SetKeyPressed(AValue: Boolean);
    procedure SetKeySelectedItem(AItem: TdxGalleryControlItem);
    procedure SetMouseHoveredItem(AItem: TdxGalleryControlItem);
    procedure SetMousePressed(AValue: Boolean);
  protected
    procedure CalculateDropTarget(const P: TPoint); overload; virtual;
    procedure CalculateDropTarget(const X, Y: Integer); overload;
    procedure ResetDropTarget; virtual;

    function GetItemPosition(AItem: TdxGalleryControlItem): TPoint; virtual;
    function GetItemViewState(AItem: TdxGalleryControlItem)
      : TdxGalleryItemViewState;
    procedure InvalidateItem(AItem: TdxGalleryControlItem); overload;
    procedure InvalidateItem(AItemViewInfo: TdxGalleryItemViewInfo); overload;
    function IsGalleryStyleSelection: Boolean;
    function IsListViewStyleSelection: Boolean;
    procedure MakeItemVisible(AItem: TdxGalleryControlItem); virtual;
    procedure UpdateAutoScrollTimerState(X, Y: Integer);
    procedure UpdateItemViewState(AItem: TdxGalleryControlItem);
    procedure UpdateMouseHoveredItem(const P: TPoint); overload; virtual;
    procedure UpdateMouseHoveredItem(X, Y: Integer); overload; inline;
    procedure UpdateMouseHoveredItem; overload;

    // ListView selection mode
    procedure SelectItems(AStartFromItem, AFinishAtItem
      : TdxGalleryControlItem); virtual;

    // Navigation
    function CreateNavigationMatrix: TdxGalleryControlNavigationMatrix; virtual;
    procedure GetNextItem(var AItemPos: TPoint;
      ADirectionX, ADirectionY: Integer); virtual;
    function GetStartItemForKeyboardNavigation: TdxGalleryControlItem; virtual;
    procedure SelectNextItem(AItem: TdxGalleryControlItem;
      ADirectionX, ADirectionY: Integer; AShift: TShiftState); virtual;

    procedure ProcessItemClick(AItem: TdxGalleryControlItem;
      X, Y: Integer); virtual;

    property AutoScrollTimer: TcxTimer read FAutoScrollTimer;
  public
    destructor Destroy; override;
    procedure CheckSelectedItem; virtual; // for internal use
    procedure LayoutChanged; virtual; // for internal use
    // Keyboard
    procedure FocusEnter; virtual; // for internal use
    procedure FocusLeave; virtual; // for internal use
    procedure KeyDown(AKey: Word; AShift: TShiftState); virtual;
    // for internal use
    procedure KeyUp(AKey: Word; AShift: TShiftState); virtual;
    // for internal use
    // Mouse
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; // for internal use
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; // for internal use
    procedure MouseLeave; virtual; // for internal use
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    // for internal use
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      virtual; // for internal use
    // drag & drop
    procedure DragEnter; virtual; // for internal use
    procedure DragLeave; virtual; // for internal use
    procedure DragDrop(Source: TObject; X, Y: Integer); virtual;
    // for internal use
    procedure DragOver(Source: TObject; X, Y: Integer; var Accept: Boolean);
      virtual; // for internal use
    //
    property DragCopy: Boolean read FDragCopy write FDragCopy;
    property DropTargetInfo: TdxGalleryControlDropTargetViewInfo
      read GetDropTargetInfo; // for internal use
    property Gallery: TdxGalleryControlStructure read GetGallery;
    property IsInDragAndDropOperation: Boolean read FIsInDragAndDropOperation;
    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode;
    property KeyPressed: Boolean read FKeyPressed write SetKeyPressed;
    // for internal use
    property KeySelectedItem: TdxGalleryControlItem read FKeySelectedItem
      write SetKeySelectedItem; // for internal use
    property MouseHoveredItem: TdxGalleryControlItem read FMouseHoveredItem
      write SetMouseHoveredItem; // for internal use
    property MousePressed: Boolean read FMousePressed write SetMousePressed;
    // for internal use
    property MousePressedItem: TdxGalleryControlItem read FMousePressedItem
      write FMousePressedItem; // for internal use
    property NavigationMatrix: TdxGalleryControlNavigationMatrix
      read GetNavigationMatrix; // for internal use
    property StartSelectionItem: TdxGalleryControlItem read FStartSelectionItem
      write FStartSelectionItem; // for internal use
    property ViewInfo: TdxGalleryControlViewInfo read GetViewInfo;
    // for internal use
  end;

  { TdxGalleryControlCustomOptions }

  TdxGalleryControlCustomOptions = class(TdxGalleryPersistent)
  protected
    procedure Changed(AType: TdxChangeType = ctHard);
    procedure ChangeScale(M, D: Integer); virtual;
  end;

  { TdxGalleryControlOptionsBehavior }

  TdxGalleryControlOptionsBehavior = class(TdxGalleryControlCustomOptions)
  strict private
    FItemHotTrack: Boolean;
    FItemMultiSelectKind: TdxGalleryItemMultiSelectKind;
    FItemShowHint: Boolean;
    FSelectOnRightClick: Boolean;

    function GetItemCheckMode: TdxGalleryItemCheckMode;
    procedure SetItemCheckMode(const Value: TdxGalleryItemCheckMode);
  protected
    procedure DoAssign(Source: TPersistent); override;

    property ItemHotTrack: Boolean read FItemHotTrack write FItemHotTrack
      default True;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
  published
    property ItemMultiSelectKind: TdxGalleryItemMultiSelectKind
      read FItemMultiSelectKind write FItemMultiSelectKind default imskGallery;
    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode
      write SetItemCheckMode default icmNone;
    property ItemShowHint: Boolean read FItemShowHint write FItemShowHint
      default False;
    property SelectOnRightClick: Boolean read FSelectOnRightClick
      write FSelectOnRightClick default False;
  end;

  { TdxGalleryControlOptionsItemImage }

  TdxGalleryControlOptionsItemImage = class(TdxGalleryControlCustomOptions)
  strict private
    FShowFrame: Boolean;
    FSize: TcxSize;

    procedure ChangeHandler(Sender: TObject);
    procedure SetShowFrame(const Value: Boolean);
    procedure SetSize(const Value: TcxSize);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default True;
    property Size: TcxSize read FSize write SetSize;
  end;

  { TdxGalleryControlOptionsItemText }

  TdxGalleryControlOptionsItemText = class(TdxGalleryControlCustomOptions)
  strict private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FPosition: TcxPosition;
    FWordWrap: Boolean;

    procedure SetAlignHorz(const Value: TAlignment);
    procedure SetAlignVert(const Value: TcxAlignmentVert);
    procedure SetPosition(const Value: TcxPosition);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
  published
    property AlignHorz: TAlignment read FAlignHorz write SetAlignHorz
      default taCenter;
    property AlignVert: TcxAlignmentVert read FAlignVert write SetAlignVert
      default vaTop;
    property Position: TcxPosition read FPosition write SetPosition
      default posNone;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

  { TdxGalleryControlOptionsItem }

  TdxGalleryControlOptionsItem = class(TdxGalleryControlCustomOptions)
  strict private
    FImage: TdxGalleryControlOptionsItemImage;
    FText: TdxGalleryControlOptionsItemText;

    procedure SetImage(AValue: TdxGalleryControlOptionsItemImage);
    procedure SetText(AValue: TdxGalleryControlOptionsItemText);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateImage: TdxGalleryControlOptionsItemImage; virtual;
    function CreateText: TdxGalleryControlOptionsItemText; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property Image: TdxGalleryControlOptionsItemImage read FImage
      write SetImage;
    property Text: TdxGalleryControlOptionsItemText read FText write SetText;
  end;

  { TdxGalleryControlOptionsView }

  TdxGalleryControlOptionsView = class(TdxGalleryControlCustomOptions)
  strict private
    FColumnAutoWidth: Boolean;
    FColumnCount: Integer;
    FContentOffset: TcxMargin;
    FContentOffsetGroups: TcxMargin;
    FContentOffsetItems: TcxMargin;
    FItem: TdxGalleryControlOptionsItem;

    procedure ChangeHandler(Sender: TObject);
    procedure SetColumnAutoWidth(AValue: Boolean);
    procedure SetColumnCount(AValue: Integer);
    procedure SetContentOffset(const Value: TcxMargin);
    procedure SetContentOffsetGroups(const Value: TcxMargin);
    procedure SetContentOffsetItems(const Value: TcxMargin);
    procedure SetItem(const Value: TdxGalleryControlOptionsItem);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateItem: TdxGalleryControlOptionsItem; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TdxCustomGalleryControl); override;
    destructor Destroy; override;
  published
    property ColumnAutoWidth: Boolean read FColumnAutoWidth
      write SetColumnAutoWidth default False;
    property ColumnCount: Integer read FColumnCount write SetColumnCount
      default 0;
    property ContentOffset: TcxMargin read FContentOffset
      write SetContentOffset;
    property ContentOffsetGroups: TcxMargin read FContentOffsetGroups
      write SetContentOffsetGroups;
    property ContentOffsetItems: TcxMargin read FContentOffsetItems
      write SetContentOffsetItems;
    property Item: TdxGalleryControlOptionsItem read FItem write SetItem;
  end;

  { TdxGalleryControlDragObject }

  TdxGalleryControlDragObject = class(TcxDragControlObject)
  strict private
    FSelectedItems: TList;

    function GetControl: TdxCustomGalleryControl;
  protected
    function CheckTarget(ATarget: TdxGalleryControlDropTargetViewInfo): Boolean;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    property Control: TdxCustomGalleryControl read GetControl;
    property SelectedItems: TList read FSelectedItems;
  end;

  { TdxCustomGalleryControl }

  TdxCustomGalleryControl = class(TcxScrollingControl, IdxSkinSupport,
    IdxGalleryOwner)
  strict private
    FCanBeFocused: Boolean;
    FController: TdxGalleryControlController;
    FGallery: TdxGalleryControlStructure;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FLockCount: Integer;
    FOptionsBehavior: TdxGalleryControlOptionsBehavior;
    FOptionsView: TdxGalleryControlOptionsView;
    FPainter: TdxGalleryControlPainter;
    FViewInfo: TdxGalleryControlViewInfo;

    FOnItemClick: TdxGalleryControlItemEvent;

    function GetColumnCount: Integer;
    function GetContentOffset: TcxMargin;
    function GetContentOffsetGroups: TcxMargin;
    function GetContentOffsetItems: TcxMargin;
    function GetItemCheckMode: TdxGalleryItemCheckMode;
    function GetItemCount: Integer;
    function GetItemImageSize: TcxSize;
    function GetItemShowHint: Boolean;
    function GetItemShowImageFrame: Boolean;
    function GetItemTextPosition: TcxPosition;
    procedure SetColumnCount(AValue: Integer);
    procedure SetContentOffset(AValue: TcxMargin);
    procedure SetContentOffsetGroups(AValue: TcxMargin);
    procedure SetContentOffsetItems(AValue: TcxMargin);
    procedure SetGallery(AValue: TdxGalleryControlStructure);
    procedure SetImages(Value: TCustomImageList);
    procedure SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
    procedure SetItemImageSize(AValue: TcxSize);
    procedure SetItemShowHint(const Value: Boolean);
    procedure SetItemShowImageFrame(AValue: Boolean);
    procedure SetItemTextPosition(AValue: TcxPosition);
    procedure SetOptionsBehavior(const Value: TdxGalleryControlOptionsBehavior);
    procedure SetOptionsView(const Value: TdxGalleryControlOptionsView);

    procedure ImageListChanged(Sender: TObject);
    procedure GalleryChangeHandler(ASender: TObject;
      AChangeType: TdxGalleryChangeType);
    procedure GalleryItemClickHandler(ASender: TObject; AItem: TdxGalleryItem);

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    function CreateController: TdxGalleryControlController; virtual;
    function CreateGallery: TdxGalleryControlStructure; virtual;
    function CreateOptionsBehavior: TdxGalleryControlOptionsBehavior; virtual;
    function CreateOptionsView: TdxGalleryControlOptionsView; virtual;
    function CreatePainter: TdxGalleryControlPainter; virtual;
    function CreateViewInfo: TdxGalleryControlViewInfo; virtual;

    procedure CreateViewSubClasses; virtual;
    procedure DestroyViewSubClasses; virtual;

    // Keyboard operations
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // Mouse operations
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    // Drag-n-drop
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    function GetDragObjectClass: TDragControlObjectClass; override;

    // TWinControl
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    // TcxScrollingControl
    procedure BoundsChanged; override;
    procedure Calculate(AType: TdxChangeType); override;
    procedure CreateHandle; override;
    function GetContentSize: TSize; override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); override;
    procedure ScrollPosChanged(const AOffset: TPoint); override;

    // TcxControl
    function AllowTouchScrollUIMode: Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateActualCanvas; override;
    procedure FreeActualCanvas; override;
    procedure DoPaint; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    function GetScrollContentForegroundColor: TColor; override;
    function HasScrollBarArea: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure SetAutoSizeMode(AValue: TdxAutoSizeMode); override;

    // TdxCustomGalleryControl
    procedure DoClickItem(AItem: TdxGalleryItem); virtual;
    function GetItemAtPos(const P: TPoint): TdxGalleryControlItem;
    function IsUpdateLocked: Boolean;

    // IdxGalleryOwner
    function GetGallery: IdxGallery;
    function GetGallery2: IdxGallery2;

    property Painter: TdxGalleryControlPainter read FPainter;
    property ViewInfo: TdxGalleryControlViewInfo read FViewInfo;

    property CanBeFocused: Boolean read FCanBeFocused write FCanBeFocused;
    property Gallery: TdxGalleryControlStructure read FGallery write SetGallery;
    property Images: TCustomImageList read FImages write SetImages;
    property OptionsBehavior: TdxGalleryControlOptionsBehavior
      read FOptionsBehavior write SetOptionsBehavior;
    property OptionsView: TdxGalleryControlOptionsView read FOptionsView
      write SetOptionsView;

    // Obsolete
    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
    property ContentOffset: TcxMargin read GetContentOffset
      write SetContentOffset;
    property ContentOffsetGroups: TcxMargin read GetContentOffsetGroups
      write SetContentOffsetGroups;
    property ContentOffsetItems: TcxMargin read GetContentOffsetItems
      write SetContentOffsetItems;
    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode
      write SetItemCheckMode;
    property ItemImageSize: TcxSize read GetItemImageSize
      write SetItemImageSize;
    property ItemShowHint: Boolean read GetItemShowHint write SetItemShowHint;
    property ItemShowImageFrame: Boolean read GetItemShowImageFrame
      write SetItemShowImageFrame;
    property ItemTextPosition: TcxPosition read GetItemTextPosition
      write SetItemTextPosition;

    property OnItemClick: TdxGalleryControlItemEvent read FOnItemClick
      write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;

    function CanFocus: Boolean; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MakeItemVisible(AItem: TdxGalleryControlItem);

    // drag-n-drop
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function StartDrag(DragObject: TDragObject): Boolean; override;

    property Controller: TdxGalleryControlController read FController;
    // for internal use
  end;

  { TdxGalleryControl }

  TdxGalleryControl = class(TdxCustomGalleryControl, IdxDirect2DSupport)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property PopupMenu;
    property Visible;

    property AutoSizeMode default asNone;
    property BorderStyle default cxcbsDefault;
    property Gallery;
    property Images;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsView;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property Transparent;

    // Obsolete
    property ColumnCount stored False;
    property ContentOffset stored False;
    property ContentOffsetGroups stored False;
    property ContentOffsetItems stored False;
    property ItemCheckMode stored False;
    property ItemImageSize stored False;
    property ItemShowHint stored False;
    property ItemShowImageFrame stored False;
    property ItemTextPosition stored False;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  Math, cxScrollBar, dxDPIAwareUtils, cxLibraryConsts;

const
  dxcItemGlyphFrameOffset = 2;
  dxcItemIndentBetweenGlyphAndText = 4;

type
  TdxGalleryAccess = class(TdxGallery);

function GetCombinedSize(const AMasterSize, ASlaveSize: TSize): TSize;
begin
  if AMasterSize.cx <> 0 then
    Result.cx := AMasterSize.cx
  else
    Result.cx := ASlaveSize.cx;

  if AMasterSize.cy <> 0 then
    Result.cy := AMasterSize.cy
  else
    Result.cy := ASlaveSize.cy;
end;

{ TdxGalleryPersistent }

constructor TdxGalleryPersistent.Create(AOwner: TdxCustomGalleryControl);
begin
  inherited Create(AOwner);
end;

function TdxGalleryPersistent.GetOwnerControl: TdxCustomGalleryControl;
begin
  Result := inherited Owner as TdxCustomGalleryControl;
end;

{ TdxGalleryCustomViewInfo }

constructor TdxGalleryCustomViewInfo.Create(AGalleryControl
  : TdxCustomGalleryControl);
begin
  FGalleryControl := AGalleryControl;
end;

procedure TdxGalleryCustomViewInfo.Calculate(AType: TdxChangeType;
  const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxGalleryCustomViewInfo.Draw(ACanvas: TcxCustomCanvas);
begin
  if ACanvas.RectVisible(Bounds) then
    DrawContent(ACanvas);
end;

function TdxGalleryCustomViewInfo.GetContentOffset: TRect;
begin
  Result := OptionsView.ContentOffset.Margin;
end;

function TdxGalleryCustomViewInfo.GetContentOffsetGroups: TRect;
begin
  Result := OptionsView.ContentOffsetGroups.Margin;
end;

function TdxGalleryCustomViewInfo.GetContentOffsetItems: TRect;
begin
  Result := OptionsView.ContentOffsetItems.Margin;
end;

function TdxGalleryCustomViewInfo.GetOptionsItemImage
  : TdxGalleryControlOptionsItemImage;
begin
  Result := OptionsView.Item.Image;
end;

function TdxGalleryCustomViewInfo.GetOptionsItemText
  : TdxGalleryControlOptionsItemText;
begin
  Result := OptionsView.Item.Text;
end;

function TdxGalleryCustomViewInfo.GetOptionsView: TdxGalleryControlOptionsView;
begin
  Result := GalleryControl.OptionsView;
end;

function TdxGalleryCustomViewInfo.GetPainter: TdxGalleryControlPainter;
begin
  Result := GalleryControl.Painter;
end;

function TdxGalleryCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GalleryControl.ScaleFactor;
end;

function TdxGalleryCustomViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := GalleryControl.UseRightToLeftAlignment;
end;

{ TdxGalleryItemViewInfo }

constructor TdxGalleryItemViewInfo.Create(AGalleryControl
  : TdxCustomGalleryControl; AItem: TdxGalleryControlItem);
begin
  inherited Create(AGalleryControl);
  FItem := AItem;

  FCaptionTextLayout := Painter.CreateCanvasBasedTextLayout;
  FCaptionTextLayout.SetText(AItem.Caption);

  FDescriptionTextLayout := Painter.CreateCanvasBasedTextLayout;
  FDescriptionTextLayout.SetText(AItem.Description);
end;

destructor TdxGalleryItemViewInfo.Destroy;
begin
  FreeAndNil(FCacheGlyph);
  FreeAndNil(FCaptionTextLayout);
  FreeAndNil(FDescriptionTextLayout);
  inherited Destroy;
end;

procedure TdxGalleryItemViewInfo.AfterConstruction;
begin
  inherited;
  UpdateFonts;
end;

procedure TdxGalleryItemViewInfo.Calculate(AType: TdxChangeType;
  const ABounds: TRect);
begin
  if AType = ctHard then
    ResetCache;

  FBounds := ABounds;
  FState := GalleryControl.Controller.GetItemViewState(Item);
  FContentBounds := cxRectContent(Bounds, ContentOffsetItems);
  CalculateTextArea(GalleryControl.ViewInfo.TextAreaSize);
  CalculateGlyphArea(GlyphSize);

  if UseRightToLeftAlignment then
  begin
    FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect
      (FContentBounds, Bounds);
    FGlyphFrameRect := TdxRightToLeftLayoutConverter.ConvertRect
      (FGlyphFrameRect, Bounds);
    FGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphRect, Bounds);
    FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect
      (FCaptionRect, Bounds);
    FDescriptionRect := TdxRightToLeftLayoutConverter.ConvertRect
      (FDescriptionRect, Bounds);
    FTextArea := TdxRightToLeftLayoutConverter.ConvertRect(FTextArea, Bounds);
  end;
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaSizeLimitedByRowCount
  (ARowCount: Integer);
begin
  FCaptionTextLayout.SetFlags
    (Painter.GetDrawTextFlags(OptionsItemText.AlignHorz,
    OptionsItemText.WordWrap));
  FCaptionTextLayout.SetLayoutConstraints(0, 0, ARowCount);
  FCaptionSize := FCaptionTextLayout.MeasureSize;

  FDescriptionTextLayout.SetFlags
    (Painter.GetDrawTextFlags(OptionsItemText.AlignHorz, True));
  if Caption <> '' then
  begin
    DescriptionTextLayout.SetLayoutConstraints(FCaptionSize.cx, 0, 0);
    FDescriptionSize := DescriptionTextLayout.MeasureSize;
  end
  else
  begin
    DescriptionTextLayout.SetLayoutConstraints(0, 0, ARowCount);
    FDescriptionSize := DescriptionTextLayout.MeasureSize;
  end;
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaSizeLimitedByWidth(AMaxWidth,
  AMaxRowCount: Integer);
begin
  FCaptionTextLayout.SetFlags
    (Painter.GetDrawTextFlags(OptionsItemText.AlignHorz,
    OptionsItemText.WordWrap));
  FCaptionTextLayout.SetLayoutConstraints(AMaxWidth, 0, AMaxRowCount);
  FCaptionSize := FCaptionTextLayout.MeasureSize;

  FDescriptionTextLayout.SetFlags
    (Painter.GetDrawTextFlags(OptionsItemText.AlignHorz, True));
  FDescriptionTextLayout.SetLayoutConstraints(AMaxWidth, 0, 0);
  FDescriptionSize := FDescriptionTextLayout.MeasureSize;
end;

procedure TdxGalleryItemViewInfo.CalculateGlyphArea(const AGlyphSize: TSize);

  function MinOffset(const ARect: TRect): Integer;
  begin
    Result := Min(ARect.Left, ARect.Right);
    Result := Min(Result, ARect.Top);
    Result := Min(Result, ARect.Bottom);
  end;

var
  AGlyphFrameOffset: Integer;
begin
  FGlyphRect := cxNullRect;
  FGlyphFrameRect := cxNullRect;
  if not cxSizeIsEmpty(AGlyphSize) then
  begin
    AGlyphFrameOffset := Min(ScaleFactor.Apply(dxcItemGlyphFrameOffset),
      MinOffset(ContentOffsetItems));

    FGlyphRect := ContentBounds;
    case OptionsItemText.Position of
      posLeft:
        FGlyphRect.Left := TextArea.Right + AGlyphFrameOffset +
          ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posRight:
        FGlyphRect.Right := TextArea.Left - AGlyphFrameOffset -
          ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posBottom:
        FGlyphRect.Bottom := TextArea.Top - AGlyphFrameOffset -
          ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
      posTop:
        FGlyphRect.Top := TextArea.Bottom + AGlyphFrameOffset +
          ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText);
    end;
    FGlyphRect := cxGetImageRect(GlyphRect, GlyphSize, ifmFit);

    if OptionsItemImage.ShowFrame then
      FGlyphFrameRect := cxRectInflate(GlyphRect, AGlyphFrameOffset,
        AGlyphFrameOffset);
  end;
end;

procedure TdxGalleryItemViewInfo.CalculateTextArea(const ATextAreaSize: TSize);
var
  R: TRect;
begin
  case OptionsItemText.Position of
    posLeft:
      FTextArea := cxRectSetWidth(ContentBounds, ATextAreaSize.cx);
    posRight:
      FTextArea := cxRectSetRight(ContentBounds, ContentBounds.Right,
        ATextAreaSize.cx);
    posBottom:
      FTextArea := cxRectSetBottom(ContentBounds, ContentBounds.Bottom,
        ATextAreaSize.cy);
    posTop:
      FTextArea := cxRectSetHeight(ContentBounds, ATextAreaSize.cy);
  else
    FTextArea := cxNullRect;
  end;

  case OptionsItemText.AlignVert of
    vaBottom:
      R := cxRectSetBottom(TextArea, TextArea.Bottom, TextAreaSize.cy);
    vaCenter:
      R := cxRectCenterVertically(TextArea, TextAreaSize.cy);
  else
    R := cxRectSetHeight(TextArea, TextAreaSize.cy);
  end;

  CalculateTextAreaContent(R);
end;

procedure TdxGalleryItemViewInfo.CalculateTextAreaContent(const ABounds: TRect);
begin
  FCaptionRect := cxRectSetSize(ABounds, cxRectWidth(ABounds), CaptionSize.cy);
  FDescriptionRect := cxRectSetTop(ABounds, CaptionRect.Bottom +
    DescriptionOffset, DescriptionSize.cy);
  FDescriptionRect := cxRectSetWidth(DescriptionRect, cxRectWidth(ABounds));
end;

procedure TdxGalleryItemViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
begin
  Painter.DrawItem(ACanvas, Self);
end;

function TdxGalleryItemViewInfo.GetDescription: string;
begin
  Result := Item.Description;
end;

function TdxGalleryItemViewInfo.GetTextAreaSize: TSize;
begin
  Result.cx := Max(CaptionSize.cx, DescriptionSize.cx);
  Result.cy := CaptionSize.cy + DescriptionSize.cy + DescriptionOffset;
end;

procedure TdxGalleryItemViewInfo.ResetCache;
begin
  FreeAndNil(FCacheGlyph);
end;

procedure TdxGalleryItemViewInfo.UpdateFonts;
begin
  FCaptionTextLayout.SetFont(Painter.ItemFont);
  FDescriptionTextLayout.SetFont(Painter.ItemFont);
end;

function TdxGalleryItemViewInfo.GetCacheGlyph: TcxCanvasBasedImage;
var
  ABitmap: TcxBitmap32;
  AColorPalette: IdxColorPalette;
begin
  AColorPalette := Painter.LookAndFeelPainter.GetGalleryItemColorPalette(State);
  if (FCacheGlyph = nil) or (FCacheGlyphColorPalette <> Pointer(AColorPalette))
  then
  begin
    ABitmap := TcxBitmap32.CreateSize(GlyphRect, True);
    try
      ABitmap.Canvas.Lock;
      try
        cxDrawImage(ABitmap.cxCanvas, ABitmap.ClientRect, Item.Glyph,
          Item.Images, Item.ImageIndex, ifmFit,
          EnabledImageDrawModeMap[State.Enabled], False, AColorPalette,
          ScaleFactor);
      finally
        ABitmap.Canvas.Unlock;
      end;
      FreeAndNil(FCacheGlyph);
      FCacheGlyph := Painter.CreateCanvasBasedImage(ABitmap, afPremultiplied);
      FCacheGlyphColorPalette := Pointer(AColorPalette);
    finally
      ABitmap.Free;
    end;
  end;
  Result := FCacheGlyph;
end;

function TdxGalleryItemViewInfo.GetCaption: string;
begin
  Result := Item.Caption;
end;

function TdxGalleryItemViewInfo.GetDescriptionOffset: Integer;
begin
  if (CaptionSize.cy > 0) and (DescriptionSize.cy > 0) then
    Result := ScaleFactor.Apply(cxTextOffset)
  else
    Result := 0;
end;

function TdxGalleryItemViewInfo.GetGlyphSize: TSize;
begin
  Result := dxGetImageSize(Item.Glyph, GalleryControl.Images, Item.ImageIndex,
    ScaleFactor);
end;

{ TdxGalleryControlItem }

function TdxGalleryControlItem.GetGroup: TdxGalleryControlGroup;
begin
  Result := inherited Group as TdxGalleryControlGroup;
end;

function TdxGalleryControlItem.GetImages: TCustomImageList;
begin
  Result := GalleryControl.Images;
end;

function TdxGalleryControlItem.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Gallery.GetParentComponent as TdxCustomGalleryControl
end;

procedure TdxGalleryControlItem.SetGroup(AGroup: TdxGalleryControlGroup);
begin
  inherited SetGroup(AGroup);
end;

{ TdxGalleryControlItems }

function TdxGalleryControlItems.Add: TdxGalleryControlItem;
begin
  Result := inherited Add as TdxGalleryControlItem;
end;

function TdxGalleryControlItems.GetItemAtPos(const P: TPoint)
  : TdxGalleryControlItem;
var
  AViewInfo: TdxGalleryItemViewInfo;
begin
  if GetGalleryControl.ViewInfo.GetItemAtPos(P, AViewInfo) then
    Result := AViewInfo.Item
  else
    Result := nil;
end;

procedure TdxGalleryControlItems.SetItemName(AItem: TcxComponentCollectionItem;
  ABaseIndex: Integer = -1);
begin
  inherited SetItemName(AItem, Count);
end;

function TdxGalleryControlItems.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := (ParentComponent as TdxGalleryControlGroup).GalleryControl;
end;

function TdxGalleryControlItems.GetItem(AIndex: Integer): TdxGalleryControlItem;
begin
  Result := inherited Items[AIndex] as TdxGalleryControlItem;
end;

procedure TdxGalleryControlItems.SetItem(AIndex: Integer;
  AValue: TdxGalleryControlItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxGalleryGroupViewInfo }

constructor TdxGalleryGroupViewInfo.Create(AGalleryControl
  : TdxCustomGalleryControl; AGroup: TdxGalleryControlGroup);
begin
  inherited Create(AGalleryControl);
  FGroup := AGroup;
  FItems := TcxObjectList.Create;
  CreateCaptionTextLayout;
end;

destructor TdxGalleryGroupViewInfo.Destroy;
begin
  FreeAndNil(FCaptionTextLayout);
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxGalleryGroupViewInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateFonts;
end;

procedure TdxGalleryGroupViewInfo.Calculate(AType: TdxChangeType;
  const ABounds: TRect);
begin
  FBounds := ABounds;
  CalculateCaption(AType);
  FColumnCount := GalleryControl.ViewInfo.ColumnCount;
  FRowCount := Group.ItemCount div ColumnCount +
    Ord(Group.ItemCount mod ColumnCount <> 0);
  FItemsRect := cxRect(Bounds.Left, CaptionRect.Bottom, 0, 0);
  FItemsRect := cxRectContent(ItemsRect, ContentOffsetGroups);
  FItemsRect := cxRectSetSize(ItemsRect, ColumnCount * ItemSize.cx,
    RowCount * ItemSize.cy);
  CalculateItems(AType);
  FBounds.Bottom := ItemsRect.Bottom + ContentOffsetGroups.Bottom;
end;

function TdxGalleryGroupViewInfo.FindItemViewInfo(AItem: TdxGalleryControlItem;
  out AViewInfo: TdxGalleryItemViewInfo): Boolean;
var
  AItemViewInfo: TdxGalleryItemViewInfo;
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    AItemViewInfo := Items[I];
    if AItemViewInfo.Item = AItem then
    begin
      AViewInfo := AItemViewInfo;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxGalleryGroupViewInfo.GetItemAtPos(const P: TPoint;
  out AViewInfo: TdxGalleryItemViewInfo): Boolean;
var
  AItemViewInfo: TdxGalleryItemViewInfo;
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
  begin
    AItemViewInfo := Items[I];
    if cxRectPtIn(AItemViewInfo.Bounds, P) then
    begin
      AViewInfo := AItemViewInfo;
      Exit(True);
    end;
  end;
end;

function TdxGalleryGroupViewInfo.GetMaxColumnCount: Integer;
begin
  Result := Group.ItemCount;
end;

function TdxGalleryGroupViewInfo.HasCaption: Boolean;
begin
  Result := Group.ShowCaption and (Group.Caption <> '');
end;

procedure TdxGalleryGroupViewInfo.CalculateCaption(AType: TdxChangeType);
const
  TextOutFlags = CXTO_END_ELLIPSIS or CXTO_SINGLELINE;
begin
  if AType >= ctMedium then
  begin
    if CaptionTextLayout <> nil then
    begin
      CaptionTextLayout.SetFlags(TextOutFlags or IfThen(UseRightToLeftAlignment,
        CXTO_RIGHT or CXTO_RTLREADING, CXTO_LEFT));
      CaptionTextLayout.SetColor(Painter.GetGroupCaptionTextColor);
      CaptionTextLayout.SetLayoutConstraints(cxRectWidth(Bounds), 0, 0);
      FCaptionHeight := CaptionTextLayout.MeasureSize.cy +
        cxMarginsHeight(CaptionTextOffsets);
    end
    else
      FCaptionHeight := 0;
  end;

  FCaptionRect := cxRectSetHeight(Bounds, FCaptionHeight);
  FCaptionTextRect := cxRectContent(CaptionRect, CaptionTextOffsets);
  if CaptionTextLayout <> nil then
    CaptionTextLayout.SetLayoutConstraints(FCaptionTextRect);

  if UseRightToLeftAlignment then
  begin
    FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect
      (FCaptionRect, Bounds);
    FCaptionTextRect := TdxRightToLeftLayoutConverter.ConvertRect
      (FCaptionTextRect, Bounds);
  end;
end;

procedure TdxGalleryGroupViewInfo.CalculateItems(AType: TdxChangeType);
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    PlaceItem(Items[I], AType, ItemsRect, I);
end;

procedure TdxGalleryGroupViewInfo.CreateCaptionTextLayout;
begin
  if HasCaption then
  begin
    FCaptionTextLayout := Painter.CreateCanvasBasedTextLayout;
    FCaptionTextLayout.SetText(Group.Caption);
  end;
end;

procedure TdxGalleryGroupViewInfo.CreateSubItems;
var
  I: Integer;
begin
  FItems.Capacity := Group.ItemCount;
  for I := 0 to Group.ItemCount - 1 do
    FItems.Add(CreateItemViewInfo(Group.Items[I]));
end;

function TdxGalleryGroupViewInfo.CreateItemViewInfo
  (AItem: TdxGalleryControlItem): TdxGalleryItemViewInfo;
begin
  Result := TdxGalleryItemViewInfo.Create(GalleryControl, AItem);
end;

procedure TdxGalleryGroupViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  I: Integer;
begin
  if not cxRectIsEmpty(CaptionRect) then
    Painter.DrawGroupHeader(ACanvas, Self);
  for I := 0 to ItemCount - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxGalleryGroupViewInfo.GetCaptionTextOffsets: TRect;
begin
  Result := Painter.GetGroupHeaderContentOffsets;
end;

procedure TdxGalleryGroupViewInfo.PlaceItem(AItem: TdxGalleryItemViewInfo;
  AChangeType: TdxChangeType; const AItemsArea: TRect; ACellIndex: Integer);
var
  X, Y, ARow, AColumn: Integer;
begin
  ARow := ACellIndex div ColumnCount;
  AColumn := ACellIndex - ARow * ColumnCount;

  Y := AItemsArea.Top + ItemSize.cy * ARow;
  if UseRightToLeftAlignment then
    X := AItemsArea.Right - ItemSize.cx * (AColumn + 1)
  else
    X := AItemsArea.Left + ItemSize.cx * AColumn;

  AItem.FCellPositionInGroup := cxPoint(AColumn, ARow);
  AItem.Calculate(AChangeType, cxRectBounds(X, Y, ItemSize));
end;

procedure TdxGalleryGroupViewInfo.UpdateFonts;
var
  I: Integer;
begin
  if CaptionTextLayout <> nil then
    CaptionTextLayout.SetFont(Painter.GroupFont);
  for I := 0 to ItemCount - 1 do
    Items[I].UpdateFonts;
end;

function TdxGalleryGroupViewInfo.GetCaption: string;
begin
  Result := Group.Caption;
end;

function TdxGalleryGroupViewInfo.GetItem(Index: Integer)
  : TdxGalleryItemViewInfo;
begin
  Result := TdxGalleryItemViewInfo(FItems.List[Index]);
end;

function TdxGalleryGroupViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxGalleryGroupViewInfo.GetSize: TSize;
begin
  Result := GalleryControl.ViewInfo.ItemSize;
end;

{ TdxGalleryGroup }

function TdxGalleryControlGroup.GetGalleryItemClass: TdxGalleryItemClass;
begin
  Result := TdxGalleryControlItem;
end;

function TdxGalleryControlGroup.GetGalleryItemsClass: TdxGalleryItemsClass;
begin
  Result := TdxGalleryControlItems;
end;

function TdxGalleryControlGroup.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := Gallery.GetParentComponent as TdxCustomGalleryControl
end;

function TdxGalleryControlGroup.GetItems: TdxGalleryControlItems;
begin
  Result := inherited Items as TdxGalleryControlItems;
end;

{ TdxGalleryControlGroups }

function TdxGalleryControlGroups.Add: TdxGalleryControlGroup;
begin
  Result := inherited Add as TdxGalleryControlGroup;
end;

function TdxGalleryControlGroups.FindByCaption(const ACaption: string;
  out AGroup: TdxGalleryControlGroup): Boolean;
begin
  Result := inherited FindByCaption(ACaption, TdxGalleryGroup(AGroup));
end;

function TdxGalleryControlGroups.GetGroupAtPos(const P: TPoint)
  : TdxGalleryControlGroup;
var
  AViewInfo: TdxGalleryGroupViewInfo;
begin
  if GetGalleryControl.ViewInfo.GetGroupAtPos(P, AViewInfo) then
    Result := AViewInfo.Group
  else
    Result := nil;
end;

function TdxGalleryControlGroups.GetItemAtPos(const P: TPoint)
  : TdxGalleryControlItem;
var
  AViewInfo: TdxGalleryItemViewInfo;
begin
  if GetGalleryControl.ViewInfo.GetItemAtPos(P, AViewInfo) then
    Result := AViewInfo.Item
  else
    Result := nil;
end;

function TdxGalleryControlGroups.GetGalleryControl: TdxCustomGalleryControl;
begin
  Result := ParentComponent as TdxCustomGalleryControl;
end;

function TdxGalleryControlGroups.GetGroup(AIndex: Integer)
  : TdxGalleryControlGroup;
begin
  Result := inherited Groups[AIndex] as TdxGalleryControlGroup;
end;

procedure TdxGalleryControlGroups.SetGroup(AIndex: Integer;
  AValue: TdxGalleryControlGroup);
begin
  Groups[AIndex] := AValue;
end;

{ TdxGalleryControlStructure }

function TdxGalleryControlStructure.GetCheckedItem: TdxGalleryControlItem;
begin
  Result := inherited GetCheckedItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetFirstItem: TdxGalleryControlItem;
begin
  Result := inherited GetFirstItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetFirstVisibleItem: TdxGalleryControlItem;
begin
  Result := inherited GetFirstVisibleItem as TdxGalleryControlItem;
end;

function TdxGalleryControlStructure.GetGroupClass: TdxGalleryGroupClass;
begin
  Result := TdxGalleryControlGroup;
end;

function TdxGalleryControlStructure.GetGroupsClass: TdxGalleryGroupsClass;
begin
  Result := TdxGalleryControlGroups;
end;

function TdxGalleryControlStructure.GetGroups: TdxGalleryControlGroups;
begin
  Result := inherited Groups as TdxGalleryControlGroups;
end;

{ TdxGalleryControlDropTargetViewInfo }

procedure TdxGalleryControlDropTargetViewInfo.Calculate(ATargetObject: TObject;
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
    GalleryControl.InvalidateRect(Bounds, False);
    FBounds := ABounds;
    GalleryControl.InvalidateRect(Bounds, False);
  end;
end;

procedure TdxGalleryControlDropTargetViewInfo.DrawContent
  (ACanvas: TcxCustomCanvas);
begin
  Painter.DrawDropTargetSelection(ACanvas, Self);
end;

function TdxGalleryControlDropTargetViewInfo.GetSize: Integer;
begin
  Result := ScaleFactor.Apply(DropTargetSize);
end;

{ TdxGalleryControlPainter }

destructor TdxGalleryControlPainter.Destroy;
begin
  FreeAndNil(FGroupFont);
  FreeAndNil(FItemFont);
  inherited;
end;

procedure TdxGalleryControlPainter.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateFonts;
end;

procedure TdxGalleryControlPainter.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FlushCache;
end;

procedure TdxGalleryControlPainter.FlushCache;
begin
  // do nothing
end;

procedure TdxGalleryControlPainter.DrawBackground(ACanvas: TcxCustomCanvas;
  const ABounds: TRect);
begin
  if Owner.GetBackgroundStyle <> bgTransparent then
    LookAndFeelPainter.DrawGalleryBackground(ACanvas, ABounds);
end;

procedure TdxGalleryControlPainter.DrawGroupHeader(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryGroupViewInfo);
begin
  DrawGroupHeaderBackground(ACanvas, AViewInfo);
  DrawGroupHeaderText(ACanvas, AViewInfo);
end;

procedure TdxGalleryControlPainter.DrawGroupHeaderBackground
  (ACanvas: TcxCustomCanvas; AViewInfo: TdxGalleryGroupViewInfo);
begin
  LookAndFeelPainter.DrawGalleryGroupHeader(ACanvas, AViewInfo.CaptionRect);
end;

procedure TdxGalleryControlPainter.DrawGroupHeaderText(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryGroupViewInfo);
begin
  if AViewInfo.CaptionTextLayout <> nil then
    AViewInfo.CaptionTextLayout.Draw(AViewInfo.CaptionTextRect);
end;

function TdxGalleryControlPainter.GetGroupHeaderContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.GetGalleryScaledGroupHeaderContentOffsets
    (ScaleFactor);
end;

procedure TdxGalleryControlPainter.DrawItem(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryItemViewInfo);
begin
  if DrawItemSelectionFirst then
    DrawItemSelection(ACanvas, AViewInfo);
  DrawItemImage(ACanvas, AViewInfo);
  if not DrawItemSelectionFirst then
    DrawItemSelection(ACanvas, AViewInfo);
  DrawItemText(ACanvas, AViewInfo);
end;

procedure TdxGalleryControlPainter.DrawItemImage(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryItemViewInfo);
begin
  if ACanvas.RectVisible(AViewInfo.GlyphFrameRect) then
    LookAndFeelPainter.DrawGalleryItemImageFrame(ACanvas,
      AViewInfo.GlyphFrameRect);
  if ACanvas.RectVisible(AViewInfo.GlyphRect) then
    AViewInfo.CacheGlyph.Draw(AViewInfo.GlyphRect);
end;

procedure TdxGalleryControlPainter.DrawItemSelection(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryItemViewInfo);
begin
  LookAndFeelPainter.DrawGalleryItemSelection(ACanvas, AViewInfo.Bounds,
    AViewInfo.State);
end;

procedure TdxGalleryControlPainter.DrawItemText(ACanvas: TcxCustomCanvas;
  AViewInfo: TdxGalleryItemViewInfo);

  procedure DrawLine(const R: TRect; const AText: TcxCanvasBasedTextLayout;
    AColor: TColor);
  begin
    if ACanvas.RectVisible(R) then
    begin
      AText.SetColor(AColor);
      AText.Draw(R);
    end;
  end;

begin
  DrawLine(AViewInfo.CaptionRect, AViewInfo.CaptionTextLayout,
    GetItemCaptionTextColor(AViewInfo));
  DrawLine(AViewInfo.DescriptionRect, AViewInfo.DescriptionTextLayout,
    GetItemDescriptionTextColor(AViewInfo));
end;

procedure TdxGalleryControlPainter.DrawDropTargetSelection
  (ACanvas: TcxCustomCanvas; AViewInfo: TdxGalleryControlDropTargetViewInfo);
begin
  ACanvas.FillRect(AViewInfo.Bounds,
    LookAndFeelPainter.GetGalleryDropTargetSelectionColor);
end;

function TdxGalleryControlPainter.CreateCanvasBasedFont(AFont: TFont)
  : TcxCanvasBasedFont;
begin
  Result := Owner.ActualCanvas.CreateFonT(AFont);
end;

function TdxGalleryControlPainter.CreateCanvasBasedImage(ABitmap: TBitmap;
  AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage;
begin
  Result := Owner.ActualCanvas.CreateImage(ABitmap, AAlphaFormat);
end;

function TdxGalleryControlPainter.CreateCanvasBasedTextLayout
  : TcxCanvasBasedTextLayout;
begin
  Result := Owner.ActualCanvas.CreateTextLayout;
end;

function TdxGalleryControlPainter.DrawItemSelectionFirst: Boolean;
begin
  Result := LookAndFeelPainter.DrawGalleryItemSelectionFirst;
end;

function TdxGalleryControlPainter.GetGroupCaptionTextColor: TColor;
begin
  Result := LookAndFeelPainter.GetGalleryGroupTextColor;
  if Result = clDefault then
    Result := Owner.Font.Color;
end;

function TdxGalleryControlPainter.GetDrawTextFlags(AAlignment: TAlignment;
  AWordWrap: Boolean): Integer;
const
  TextAlignHorzMap: array [TAlignment] of Integer = (CXTO_LEFT, CXTO_RIGHT,
    CXTO_CENTER_HORIZONTALLY);
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or
    CXTO_CENTER_VERTICALLY;
  if AWordWrap then
    Result := Result or CXTO_WORDBREAK;
  if UseRightToLeftAlignment then
  begin
    ChangeBiDiModeAlignment(AAlignment);
    Result := Result or CXTO_RTLREADING;
  end;
  Result := Result or TextAlignHorzMap[AAlignment];
end;

function TdxGalleryControlPainter.GetItemCaptionTextColor
  (AViewInfo: TdxGalleryItemViewInfo): TColor;
begin
  Result := LookAndFeelPainter.GetGalleryItemCaptionTextColor(AViewInfo.State);
end;

function TdxGalleryControlPainter.GetItemDescriptionTextColor
  (AViewInfo: TdxGalleryItemViewInfo): TColor;
begin
  Result := LookAndFeelPainter.GetGalleryItemDescriptionTextColor
    (AViewInfo.State);
end;

function TdxGalleryControlPainter.GetLookAndFeelPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := Owner.LookAndFeelPainter;
end;

function TdxGalleryControlPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxGalleryControlPainter.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Owner.UseRightToLeftAlignment;
end;

procedure TdxGalleryControlPainter.UpdateFonts;
var
  AGroupFont: TcxCanvasBasedFont;
  AItemFont: TcxCanvasBasedFont;
  ATempFont: TFont;
begin
  AItemFont := FItemFont;
  AGroupFont := FGroupFont;

  ATempFont := TFont.Create;
  try
    ATempFont.Assign(Owner.Font);
    FItemFont := CreateCanvasBasedFont(ATempFont);
    ATempFont.Style := ATempFont.Style + [fsBold];
    FGroupFont := CreateCanvasBasedFont(ATempFont);
  finally
    ATempFont.Free;
  end;

  Owner.ViewInfo.UpdateFonts;

  FreeAndNil(AGroupFont);
  FreeAndNil(AItemFont);
end;

{ TdxGalleryControlViewInfo }

constructor TdxGalleryControlViewInfo.Create(AGalleryControl
  : TdxCustomGalleryControl);
begin
  inherited Create(AGalleryControl);
  FGroups := TcxObjectList.Create;
  FDropTarget := CreateDropTargetViewInfo;
end;

destructor TdxGalleryControlViewInfo.Destroy;
begin
  FreeAndNil(FDropTarget);
  FreeAndNil(FGroups);
  inherited;
end;

procedure TdxGalleryControlViewInfo.Calculate(AType: TdxChangeType;
  const ABounds: TRect);
begin
  inherited Calculate(AType, ABounds);
  if AType = ctHard then
    RecreateSubItems;
  if AType <> ctLight then
  begin
    CalculateItemSize;
    CalculateColumnCount;
  end;
  CalculateContentBounds(AType);
  CalculateAutoScrollAreas;
end;

function TdxGalleryControlViewInfo.FindGroupViewInfo
  (AGroup: TdxGalleryControlGroup;
  out AViewInfo: TdxGalleryGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
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

function TdxGalleryControlViewInfo.FindItemViewInfo
  (AItem: TdxGalleryControlItem; out AViewInfo: TdxGalleryItemViewInfo)
  : Boolean;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
begin
  Result := FindGroupViewInfo(AItem.Group, AGroupViewInfo) and
    AGroupViewInfo.FindItemViewInfo(AItem, AViewInfo);
end;

function TdxGalleryControlViewInfo.GetGroupAtPos(const P: TPoint;
  out AViewInfo: TdxGalleryGroupViewInfo): Boolean;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
  begin
    AGroupViewInfo := Groups[I];
    if cxRectPtIn(AGroupViewInfo.Bounds, P) then
    begin
      AViewInfo := AGroupViewInfo;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxGalleryControlViewInfo.GetItemAtPos(const P: TPoint;
  out AViewInfo: TdxGalleryItemViewInfo): Boolean;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
begin
  Result := GetGroupAtPos(P, AGroupViewInfo) and AGroupViewInfo.GetItemAtPos(P,
    AViewInfo);
end;

procedure TdxGalleryControlViewInfo.CalculateAutoScrollAreas;
const
  AreaSize = 15;
begin
  FAutoScrollTopArea := cxRectSetHeight(Bounds, ScaleFactor.Apply(AreaSize));
  FAutoScrollBottomArea := cxRectSetBottom(Bounds, Bounds.Bottom,
    ScaleFactor.Apply(AreaSize));
end;

procedure TdxGalleryControlViewInfo.CalculateColumnCount;
begin
  FColumnCount := GalleryControl.ColumnCount;
  if ColumnCount = 0 then
  begin
    if AutoWidth then
      FColumnCount := 5
    else
      FColumnCount := GetAvailableGroupsAreaWidth div ItemSize.cx;

    if ColumnAutoWidth then
      FColumnCount := Min(FColumnCount, MaxColumnCount);
    FColumnCount := Max(1, FColumnCount);
  end;
end;

procedure TdxGalleryControlViewInfo.CalculateContentBounds
  (AType: TdxChangeType);
var
  AContentWidth: Integer;
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  ARect: TRect;
  I: Integer;
begin
  if GalleryControl.HandleAllocated and (GroupCount > 0) then
  begin
    if ColumnAutoWidth then
    begin
      AContentWidth := cxRectWidth(Bounds) - cxMarginsWidth(ContentOffset);
      if (GalleryControl.GetScrollbarMode = sbmHybrid) and
        GalleryControl.IsScrollBarActive(sbVertical) then
        Dec(AContentWidth, GalleryControl.GetVScrollBarAreaWidth);
    end
    else
      AContentWidth := ColumnCount * ItemSize.cx +
        cxMarginsWidth(ContentOffsetGroups);

    FContentBounds.Top := Bounds.Top - GalleryControl.TopPos +
      ContentOffset.Top;
    FContentBounds.Bottom := cxMaxRectSize;
    if UseRightToLeftAlignment then
    begin
      FContentBounds.Right := Bounds.Right + GalleryControl.LeftPos -
        ContentOffset.Right;
      FContentBounds.Left := ContentBounds.Right - AContentWidth;
    end
    else
    begin
      FContentBounds.Left := Bounds.Left - GalleryControl.LeftPos +
        ContentOffset.Left;
      FContentBounds.Right := ContentBounds.Left + AContentWidth;
    end;

    ARect := ContentBounds;
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      AGroupViewInfo.Calculate(AType, ARect);
      ARect.Top := AGroupViewInfo.Bounds.Bottom;
    end;

    FContentBounds.Bottom := ARect.Top;
  end
  else
    FContentBounds := cxEmptyRect;
end;

procedure TdxGalleryControlViewInfo.CalculateItemSize;
var
  ADeltaSize: Integer;
  AMaxRowCount: Integer;
begin
  AMaxRowCount := 0;
  FImageSize := CalculateMaxItemImageSize;
  case OptionsItemText.Position of
    posTop, posBottom:
      FTextAreaSize := CalculateMaxItemTextAreaSizeLimitedByWidth
        (GetTextAreaMaxWidth(FImageSize), 0);
    posLeft, posRight:
      begin
        AMaxRowCount := GetTextAreaMaxRowCount(FImageSize);
        FTextAreaSize := CalculateMaxItemTextAreaSizeLimitedByRowCount
          (AMaxRowCount);
      end;
  else
    FTextAreaSize := cxNullSize;
  end;

  if (ImageSize.cx = 0) and (TextAreaSize.cx = 0) then
    FImageSize.cx := ScaleFactor.Apply(16);
  if (ImageSize.cy = 0) and (TextAreaSize.cy = 0) then
    FImageSize.cy := ScaleFactor.Apply(16);

  if ColumnAutoWidth then
  begin
    FItemSize := DoCalculateItemSize;
    CalculateColumnCount;
    ADeltaSize := (GetAvailableGroupsAreaWidth - ItemSize.cx * ColumnCount)
      div ColumnCount;
    if OptionsItemText.Position = posNone then
      FImageSize.cx := Max(0, ImageSize.cx + ADeltaSize)
    else
      FTextAreaSize.cx := Max(0, TextAreaSize.cx + ADeltaSize);
  end;

  if OptionsItemText.Position <> posNone then
    FTextAreaSize.cy := CalculateMaxItemTextAreaSizeLimitedByWidth
      (TextAreaSize.cx, AMaxRowCount).cy;
  FItemSize := DoCalculateItemSize;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemImageSize: TSize;
var
  AGroup: TdxGalleryGroupViewInfo;
  I, J: Integer;
begin
  Result := OptionsItemImage.Size.Size;
  if cxSizeIsEmpty(Result) then
    for I := 0 to GroupCount - 1 do
    begin
      AGroup := Groups[I];
      for J := 0 to AGroup.ItemCount - 1 do
        Result := cxSizeMax(Result, AGroup.Items[J].GlyphSize);
    end;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemTextAreaSizeLimitedByRowCount
  (ARowCount: Integer): TSize;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  AItemViewInfo: TdxGalleryItemViewInfo;
  I, J: Integer;
begin
  Result := cxNullSize;
  if OptionsItemText.Position <> posNone then
  begin
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      for J := 0 to AGroupViewInfo.ItemCount - 1 do
      begin
        AItemViewInfo := AGroupViewInfo.Items[J];
        AItemViewInfo.CalculateTextAreaSizeLimitedByRowCount(ARowCount);
        Result := cxSizeMax(Result, AItemViewInfo.TextAreaSize);
      end;
    end;
  end;
end;

function TdxGalleryControlViewInfo.CalculateMaxItemTextAreaSizeLimitedByWidth
  (AMaxWidth, AMaxRowCount: Integer): TSize;
var
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  AItemViewInfo: TdxGalleryItemViewInfo;
  I, J: Integer;
begin
  Result := cxNullSize;
  if OptionsItemText.Position <> posNone then
  begin
    AMaxWidth := Max(1, AMaxWidth);
    for I := 0 to GroupCount - 1 do
    begin
      AGroupViewInfo := Groups[I];
      for J := 0 to AGroupViewInfo.ItemCount - 1 do
      begin
        AItemViewInfo := AGroupViewInfo.Items[J];
        AItemViewInfo.CalculateTextAreaSizeLimitedByWidth(AMaxWidth,
          AMaxRowCount);
        Result := cxSizeMax(Result, AItemViewInfo.TextAreaSize);
      end;
    end;
  end;
end;

function TdxGalleryControlViewInfo.CreateDropTargetViewInfo
  : TdxGalleryControlDropTargetViewInfo;
begin
  Result := TdxGalleryControlDropTargetViewInfo.Create(GalleryControl);
end;

function TdxGalleryControlViewInfo.CreateGroupViewInfo
  (AGroup: TdxGalleryControlGroup): TdxGalleryGroupViewInfo;
begin
  Result := TdxGalleryGroupViewInfo.Create(GalleryControl, AGroup);
end;

function TdxGalleryControlViewInfo.DoCalculateItemSize: TSize;

  function CalculateItemHeight(AMarginTop, AMarginBottom, ATextHeight,
    AImageHeight: Integer): Integer;
  begin
    Result := AMarginTop + AMarginBottom + Max(ATextHeight, AImageHeight);
  end;

  function CalculateItemWidth(AMarginLeft, AMarginRight, ATextWidth,
    AImageWidth: Integer): Integer;
  begin
    Result := AMarginLeft + AMarginRight + ATextWidth + AImageWidth +
      ScaleFactor.Apply(dxcItemIndentBetweenGlyphAndText) +
      Min(Min(AMarginLeft, AMarginRight),
      ScaleFactor.Apply(dxcItemGlyphFrameOffset));
  end;

begin
  case OptionsItemText.Position of
    posLeft, posRight:
      begin
        Result.cx := CalculateItemWidth(ContentOffsetItems.Left,
          ContentOffsetItems.Right, TextAreaSize.cx, ImageSize.cx);
        Result.cy := CalculateItemHeight(ContentOffsetItems.Top,
          ContentOffsetItems.Bottom, TextAreaSize.cy, ImageSize.cy);
      end;

    posTop, posBottom:
      begin
        Result.cx := CalculateItemHeight(ContentOffsetItems.Left,
          ContentOffsetItems.Right, TextAreaSize.cx, ImageSize.cx);
        Result.cy := CalculateItemWidth(ContentOffsetItems.Top,
          ContentOffsetItems.Bottom, TextAreaSize.cy, ImageSize.cy);
      end;

  else // posNone
    begin
      Result.cx := ImageSize.cx + cxMarginsWidth(ContentOffsetItems);
      Result.cy := ImageSize.cy + cxMarginsHeight(ContentOffsetItems);
    end;
  end;
end;

procedure TdxGalleryControlViewInfo.DrawContent(ACanvas: TcxCustomCanvas);
var
  I: Integer;
begin
  Painter.DrawBackground(ACanvas, Bounds);
  for I := 0 to GroupCount - 1 do
    Groups[I].Draw(ACanvas);
  DropTarget.Draw(ACanvas);
end;

function TdxGalleryControlViewInfo.GetAvailableGroupsAreaWidth: Integer;
var
  AOffset: Integer;
begin
  AOffset := cxMarginsWidth(BorderWidths) + cxMarginsWidth(ContentOffsetGroups);
  if not GalleryControl.IsTouchScrollUIMode and not AutoHeight then
  begin
    if not ColumnAutoWidth or GalleryControl.IsScrollBarActive(sbVertical) then
      Inc(AOffset, GalleryControl.GetVScrollBarDefaultAreaWidth);
  end;
  Result := GalleryControl.Width - AOffset;
end;

function TdxGalleryControlViewInfo.GetBorderWidths: TRect;
var
  ABorderSize: Integer;
begin
  Result := ContentOffset;
  ABorderSize := GalleryControl.BorderSize;
  Inc(Result.Bottom, ABorderSize);
  Inc(Result.Left, ABorderSize);
  Inc(Result.Right, ABorderSize);
  Inc(Result.Top, ABorderSize);
end;

function TdxGalleryControlViewInfo.GetTextAreaMaxRowCount(const AImageSize
  : TSize): Integer;
begin
  Result := Max(1, AImageSize.cy div TdxTextMeasurer.TextLineHeight(Font));
end;

function TdxGalleryControlViewInfo.GetTextAreaMaxWidth(const AImageSize
  : TSize): Integer;
begin
  Result := Max(AImageSize.cx, cxTextWidth(Font, 'W') * 3);
end;

procedure TdxGalleryControlViewInfo.RecreateSubItems;
var
  AGroup: TdxGalleryControlGroup;
  AGroups: TdxGalleryControlGroups;
  I: Integer;
begin
  AGroups := GalleryControl.Gallery.Groups;
  FGroups.Clear;
  FGroups.Capacity := AGroups.Count;
  for I := 0 to AGroups.Count - 1 do
  begin
    AGroup := AGroups[I];
    if AGroup.Visible then
      FGroups.Add(CreateGroupViewInfo(AGroup));
  end;
  for I := 0 to GroupCount - 1 do
    Groups[I].CreateSubItems;
end;

procedure TdxGalleryControlViewInfo.UpdateFonts;
var
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
    Groups[I].UpdateFonts;
end;

function TdxGalleryControlViewInfo.GetColumnAutoWidth: Boolean;
begin
  Result := OptionsView.ColumnAutoWidth and not AutoWidth;
end;

function TdxGalleryControlViewInfo.GetFont: TFont;
begin
  Result := GalleryControl.Font;
end;

function TdxGalleryControlViewInfo.GetGroup(Index: Integer)
  : TdxGalleryGroupViewInfo;
begin
  Result := TdxGalleryGroupViewInfo(FGroups.List[Index]);
end;

function TdxGalleryControlViewInfo.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

function TdxGalleryControlViewInfo.GetAutoHeight: Boolean;
begin
  Result := GalleryControl.AutoSizeMode in [asAutoHeight, asAutoSize];
end;

function TdxGalleryControlViewInfo.GetAutoWidth: Boolean;
begin
  Result := GalleryControl.AutoSizeMode in [asAutoWidth, asAutoSize];
end;

function TdxGalleryControlViewInfo.GetMaxColumnCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GroupCount - 1 do
    Result := Max(Result, Groups[I].GetMaxColumnCount);
end;

function TdxGalleryControlViewInfo.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GroupCount - 1 do
    Inc(Result, Groups[I].RowCount);
end;

{ TdxGalleryControlNavigationMatrix }

constructor TdxGalleryControlNavigationMatrix.Create
  (AViewInfo: TdxGalleryControlViewInfo);
begin
  inherited Create;
  FRowCount := AViewInfo.RowCount;
  FColumnCount := AViewInfo.ColumnCount;
  SetLength(FValues, ColumnCount, RowCount);
  Populate(AViewInfo);
end;

destructor TdxGalleryControlNavigationMatrix.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

function TdxGalleryControlNavigationMatrix.GetRightMostItemIndex
  (ARow: Integer): Integer;
var
  AColumn: Integer;
begin
  Result := 0;
  for AColumn := ColumnCount - 1 downto 0 do
    if Values[AColumn, ARow] <> nil then
    begin
      Result := AColumn;
      Break;
    end;
end;

procedure TdxGalleryControlNavigationMatrix.Populate
  (AViewInfo: TdxGalleryControlViewInfo);

  procedure ProcessGroup(AGroup: TdxGalleryGroupViewInfo; var AIndex: Integer);
  var
    AItemViewInfo: TdxGalleryItemViewInfo;
    I: Integer;
    P: TPoint;
  begin
    for I := 0 to AGroup.ItemCount - 1 do
    begin
      AItemViewInfo := AGroup.Items[I];
      P := AItemViewInfo.CellPositionInGroup;
      Values[P.X, P.Y + AIndex] := AItemViewInfo.Item;
    end;
    Inc(AIndex, AGroup.RowCount);
  end;

var
  AIndex, I: Integer;
begin
  AIndex := 0;
  for I := 0 to AViewInfo.GroupCount - 1 do
    ProcessGroup(AViewInfo.Groups[I], AIndex);
end;

function TdxGalleryControlNavigationMatrix.GetValue(ACol, ARow: Integer)
  : TdxGalleryControlItem;
begin
  Result := FValues[ACol, ARow];
end;

procedure TdxGalleryControlNavigationMatrix.SetValue(ACol, ARow: Integer;
  const AValue: TdxGalleryControlItem);
begin
  FValues[ACol, ARow] := AValue;
end;

{ TdxGalleryControlController }

destructor TdxGalleryControlController.Destroy;
begin
  FreeAndNil(FNavigationMatrix);
  FreeAndNil(FAutoScrollTimer);
  inherited;
end;

procedure TdxGalleryControlController.CheckSelectedItem;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    Gallery.GetAllItems(AList);
    if (StartSelectionItem <> nil) and (AList.IndexOf(StartSelectionItem) < 0)
    then
      FStartSelectionItem := nil;
    if (KeySelectedItem <> nil) and (AList.IndexOf(KeySelectedItem) < 0) then
      FKeySelectedItem := nil;
    if AList.IndexOf(MouseHoveredItem) < 0 then
      FMouseHoveredItem := nil;
    if AList.IndexOf(MousePressedItem) < 0 then
      FMousePressedItem := nil;
  finally
    AList.Free;
  end;
end;

procedure TdxGalleryControlController.LayoutChanged;
begin
  FreeAndNil(FNavigationMatrix);
end;

function TdxGalleryControlController.CreateNavigationMatrix
  : TdxGalleryControlNavigationMatrix;
begin
  Result := TdxGalleryControlNavigationMatrix.Create(ViewInfo);
end;

procedure TdxGalleryControlController.FocusEnter;
begin
  UpdateItemViewState(KeySelectedItem)
end;

procedure TdxGalleryControlController.FocusLeave;
begin
  UpdateItemViewState(KeySelectedItem);
end;

procedure TdxGalleryControlController.KeyDown(AKey: Word; AShift: TShiftState);
begin
  case AKey of
    VK_RIGHT:
      SelectNextItem(GetStartItemForKeyboardNavigation,
        IfThen(Owner.UseRightToLeftAlignment, -1, 1), 0, AShift);
    VK_LEFT:
      SelectNextItem(GetStartItemForKeyboardNavigation,
        IfThen(Owner.UseRightToLeftAlignment, 1, -1), 0, AShift);
    VK_UP:
      SelectNextItem(GetStartItemForKeyboardNavigation, 0, -1, AShift);
    VK_DOWN:
      SelectNextItem(GetStartItemForKeyboardNavigation, 0, 1, AShift);
    VK_SPACE:
      KeyPressed := True;
  end;
end;

procedure TdxGalleryControlController.KeyUp(AKey: Word; AShift: TShiftState);
begin
  case AKey of
    VK_SPACE:
      begin
        KeyPressed := False;
        Gallery.ClickItem(KeySelectedItem);
      end;
  end;
end;

procedure TdxGalleryControlController.MouseClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function CanSelectHoveredItem: Boolean;
  begin
    Result := (Button = mbLeft) or
      ((Button = mbRight) and ((MouseHoveredItem = nil) or
      not MouseHoveredItem.Checked));
  end;

begin
  if IsListViewStyleSelection then
  begin
    if not(ssShift in Shift) and CanSelectHoveredItem then
      StartSelectionItem := MouseHoveredItem;

    if ssCtrl in Shift then
      ProcessItemClick(MouseHoveredItem, X, Y)
    else if CanSelectHoveredItem then
      SelectItems(StartSelectionItem, MouseHoveredItem);

    KeySelectedItem := MouseHoveredItem;
  end
  else if (Button = mbLeft) or (ItemCheckMode = icmSingleRadio) or
    ((ItemCheckMode = icmSingleCheck) or IsGalleryStyleSelection) and CanSelectHoveredItem
  then
  begin
    ProcessItemClick(MouseHoveredItem, X, Y);
    KeySelectedItem := nil;
  end;
end;

procedure TdxGalleryControlController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if CanChangeSelection(Button, Shift) then
  begin
    UpdateMouseHoveredItem(X, Y);
    if Button = mbRight then
      MouseClick(Button, Shift, X, Y)
    else
    begin
      MousePressed := True;
      MousePressedItem := MouseHoveredItem;
    end;
  end;
end;

procedure TdxGalleryControlController.MouseLeave;
begin
  MouseHoveredItem := nil;
end;

procedure TdxGalleryControlController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  if not IsInDragAndDropOperation then
    UpdateMouseHoveredItem(X, Y);
end;

procedure TdxGalleryControlController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if CanChangeSelection(Button, Shift) then
  begin
    UpdateMouseHoveredItem(X, Y);
    if FMousePressed then
    begin
      MousePressed := False;
      if MouseHoveredItem = MousePressedItem then
        MouseClick(Button, Shift, X, Y);
      MousePressedItem := nil;
    end;
  end;
end;

procedure TdxGalleryControlController.DragEnter;
begin
  if FAutoScrollTimer = nil then
    FAutoScrollTimer := cxCreateTimer(AutoScrollTimerHandler, 100, False);
  FIsInDragAndDropOperation := True;
  MouseHoveredItem := nil;
  MousePressedItem := nil;
  MousePressed := False;
end;

procedure TdxGalleryControlController.DragLeave;
begin
  ResetDropTarget;
  FIsInDragAndDropOperation := False;
  FreeAndNil(FAutoScrollTimer);
  UpdateMouseHoveredItem;
end;

procedure TdxGalleryControlController.DragDrop(Source: TObject; X, Y: Integer);
var
  AGroup: TdxGalleryControlGroup;
  AInsertIndex: Integer;
  AItem: TdxGalleryControlItem;
  ASource: TdxGalleryControlDragObject;
  ASourceItem: TdxGalleryControlItem;
  I: Integer;
begin
  if Source is TdxGalleryControlDragObject then
  begin
    ASource := TdxGalleryControlDragObject(Source);
    CalculateDropTarget(X, Y);
    if ASource.CheckTarget(DropTargetInfo) then
    begin
      ASource.Control.BeginUpdate;
      Owner.BeginUpdate;
      try
        if (ASource.Control = Owner) and not DragCopy then
        begin
          for I := 0 to ASource.SelectedItems.Count - 1 do
            TdxGalleryControlItem(ASource.SelectedItems.List[I]).Group := nil;
        end;

        if DropTargetInfo.TargetObject is TdxGalleryControlItem then
        begin
          AGroup := TdxGalleryControlItem(DropTargetInfo.TargetObject).Group;
          AInsertIndex := TdxGalleryControlItem
            (DropTargetInfo.TargetObject).Index;
          if DropTargetInfo.Side = bRight then
            Inc(AInsertIndex);
        end
        else
        begin
          AGroup := DropTargetInfo.TargetObject as TdxGalleryControlGroup;
          if DropTargetInfo.Side = bBottom then
            AInsertIndex := AGroup.ItemCount
          else
            AInsertIndex := 0;
        end;

        AGroup.Items.BeginUpdate;
        try
          if (ASource.Control <> Owner) or DragCopy then
          begin
            Owner.Gallery.UncheckAll;
            for I := ASource.SelectedItems.Count - 1 downto 0 do
            begin
              ASourceItem := TdxGalleryControlItem
                (ASource.SelectedItems.List[I]);
              AItem := AGroup.Items.Add;
              AItem.Assign(ASourceItem);
              AItem.Tag := ASourceItem.Tag;
              AItem.Index := AInsertIndex;
              AItem.Checked := True;
            end;
            if not DragCopy then
            begin
              for I := ASource.SelectedItems.Count - 1 downto 0 do
                TdxGalleryControlItem(ASource.SelectedItems.List[I]).Free;
              ASource.Control.Controller.CheckSelectedItem;
            end;
          end
          else
            for I := 0 to ASource.SelectedItems.Count - 1 do
            begin
              AItem := TdxGalleryControlItem(ASource.SelectedItems.List[I]);
              AItem.Group := AGroup;
              AItem.Index := AInsertIndex + I;
            end;
        finally
          AGroup.Items.EndUpdate;
        end;
      finally
        ASource.Control.EndUpdate;
        Owner.EndUpdate;
      end;
    end;
  end;
end;

procedure TdxGalleryControlController.DragOver(Source: TObject; X, Y: Integer;
  var Accept: Boolean);
begin
  if Source is TdxGalleryControlDragObject then
  begin
    CalculateDropTarget(X, Y);
    Accept := Accept and TdxGalleryControlDragObject(Source)
      .CheckTarget(DropTargetInfo);
    UpdateAutoScrollTimerState(X, Y);
  end
  else
    ResetDropTarget;
end;

procedure TdxGalleryControlController.CalculateDropTarget(const P: TPoint);

  function FindNearest(AList: TList; ACheckVerticalRange: Boolean)
    : TdxGalleryCustomViewInfo;
  var
    ADistance: Single;
    AItem: TdxGalleryCustomViewInfo;
    AMinDistance: Single;
    I: Integer;
  begin
    Result := nil;
    AMinDistance := MaxInt;
    for I := 0 to AList.Count - 1 do
    begin
      AItem := TdxGalleryCustomViewInfo(AList.List[I]);
      if PtInRect(AItem.Bounds, P) then
        Exit(AItem);
      if ACheckVerticalRange then
      begin
        if not InRange(P.Y, AItem.Bounds.Top, AItem.Bounds.Bottom) then
          Continue;
      end;
      ADistance := cxPointDistance(cxRectCenter(AItem.Bounds), P);
      if ADistance < AMinDistance then
      begin
        AMinDistance := ADistance;
        Result := AItem;
      end;
    end;
  end;

  function CalculateDropTargetCore: Boolean;
  const
    SideMap: array [Boolean, Boolean] of TcxBorder = ((bRight, bLeft),
      (bLeft, bRight));
  var
    AGroupViewInfo: TdxGalleryGroupViewInfo;
    AItemViewInfo: TdxGalleryItemViewInfo;
  begin
    AGroupViewInfo := FindNearest(ViewInfo.FGroups, True)
      as TdxGalleryGroupViewInfo;
    if AGroupViewInfo = nil then
      AGroupViewInfo := FindNearest(ViewInfo.FGroups, False)
        as TdxGalleryGroupViewInfo;
    if AGroupViewInfo = nil then
      Exit(False);

    AItemViewInfo := FindNearest(AGroupViewInfo.FItems, True)
      as TdxGalleryItemViewInfo;
    if (AItemViewInfo = nil) and (AGroupViewInfo.ItemCount > 0) then
    begin
      Result := PtInRect(AGroupViewInfo.Bounds, P);
      if Result then
      begin
        AItemViewInfo := AGroupViewInfo.Items[AGroupViewInfo.ItemCount - 1];
        DropTargetInfo.Calculate(AItemViewInfo.Item, AItemViewInfo.Bounds,
          SideMap[ViewInfo.UseRightToLeftAlignment, False]);
      end;
    end
    else
    begin
      if AItemViewInfo <> nil then
        DropTargetInfo.Calculate(AItemViewInfo.Item, AItemViewInfo.Bounds,
          SideMap[ViewInfo.UseRightToLeftAlignment,
          P.X < cxRectCenter(AItemViewInfo.Bounds).X])
      else
      begin
        if AGroupViewInfo.HasCaption and not PtInRect(AGroupViewInfo.Bounds, P)
        then
          Exit(False);
        DropTargetInfo.Calculate(AGroupViewInfo.Group,
          cxRectSetHeight(AGroupViewInfo.ItemsRect,
          DropTargetInfo.Size), bBottom);
      end;
      Result := True;
    end;
  end;

begin
  if not CalculateDropTargetCore then
    ResetDropTarget;
end;

procedure TdxGalleryControlController.CalculateDropTarget(const X, Y: Integer);
begin
  CalculateDropTarget(Point(X, Y));
end;

procedure TdxGalleryControlController.ResetDropTarget;
begin
  DropTargetInfo.Calculate(nil, cxNullRect, bLeft);
end;

function TdxGalleryControlController.GetItemCheckMode: TdxGalleryItemCheckMode;
begin
  Result := Owner.OptionsBehavior.ItemCheckMode;
end;

function TdxGalleryControlController.GetItemPosition
  (AItem: TdxGalleryControlItem): TPoint;
var
  AGroup: TdxGalleryGroup;
  AGroupViewInfo: TdxGalleryGroupViewInfo;
  AItemViewInfo: TdxGalleryItemViewInfo;
  AOffset: Integer;
  I: Integer;
begin
  AOffset := 0;
  AGroup := AItem.Group;
  for I := 0 to ViewInfo.GroupCount - 1 do
  begin
    AGroupViewInfo := ViewInfo.Groups[I];
    if AGroupViewInfo.Group = AGroup then
    begin
      if AGroupViewInfo.FindItemViewInfo(AItem, AItemViewInfo) then
        Exit(cxPointOffset(AItemViewInfo.CellPositionInGroup, 0, AOffset));
      Break;
    end;
    Inc(AOffset, AGroupViewInfo.RowCount);
  end;
  Result := cxNullPoint;
end;

function TdxGalleryControlController.GetItemViewState
  (AItem: TdxGalleryControlItem): TdxGalleryItemViewState;
begin
  Result.Enabled := AItem.Enabled and Owner.Enabled;
  Result.Checked := AItem.Checked;
  Result.Hover := (AItem = MouseHoveredItem) and
    Owner.OptionsBehavior.ItemHotTrack;
  Result.Focused := (AItem = KeySelectedItem) and Owner.Focused;
  Result.Pressed := Result.Hover and MousePressed or Result.Focused and
    KeyPressed;
end;

procedure TdxGalleryControlController.InvalidateItem
  (AItem: TdxGalleryControlItem);
var
  AViewInfo: TdxGalleryItemViewInfo;
begin
  if (AItem <> nil) and ViewInfo.FindItemViewInfo(AItem, AViewInfo) then
    InvalidateItem(AViewInfo);
end;

procedure TdxGalleryControlController.InvalidateItem(AItemViewInfo
  : TdxGalleryItemViewInfo);
begin
  Owner.InvalidateRect(AItemViewInfo.Bounds, False);
end;

function TdxGalleryControlController.IsGalleryStyleSelection: Boolean;
begin
  Result := (ItemCheckMode = icmMultiple) and
    (Owner.OptionsBehavior.ItemMultiSelectKind = imskGallery);
end;

function TdxGalleryControlController.IsListViewStyleSelection: Boolean;
begin
  Result := (ItemCheckMode = icmMultiple) and
    (Owner.OptionsBehavior.ItemMultiSelectKind = imskListView);
end;

procedure TdxGalleryControlController.MakeItemVisible
  (AItem: TdxGalleryControlItem);
var
  ABounds: TRect;
  AViewInfo: TdxGalleryItemViewInfo;
begin
  if (AItem <> nil) and ViewInfo.FindItemViewInfo(AItem, AViewInfo) then
  begin
    ABounds := AViewInfo.Bounds;
    if Owner.UseRightToLeftAlignment then
      ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds,
        ViewInfo.Bounds);
    Owner.MakeVisible(ABounds, vtFully);
  end;
end;

procedure TdxGalleryControlController.UpdateAutoScrollTimerState(X, Y: Integer);
begin
  if AutoScrollTimer <> nil then
  begin
    if cxRectPtIn(ViewInfo.AutoScrollTopArea, X, Y) then
      AutoScrollTimer.Tag := -1
    else if cxRectPtIn(ViewInfo.AutoScrollBottomArea, X, Y) then
      AutoScrollTimer.Tag := 1
    else
      AutoScrollTimer.Tag := 0;

    AutoScrollTimer.Enabled := AutoScrollTimer.Tag <> 0;
  end;
end;

procedure TdxGalleryControlController.UpdateItemViewState
  (AItem: TdxGalleryControlItem);

  function IsItemValid(AItem: TdxGalleryControlItem): Boolean;
  begin
    Result := (AItem <> nil) and not(csDestroying in AItem.ComponentState);
  end;

  function IsViewStateEqual(const AViewState1, AViewState2
    : TdxGalleryItemViewState): Boolean;
  begin
    Result := CompareMem(@AViewState1, @AViewState2,
      SizeOf(TdxGalleryItemViewState));
  end;

var
  AViewInfo: TdxGalleryItemViewInfo;
begin
  if IsItemValid(AItem) and ViewInfo.FindItemViewInfo(AItem, AViewInfo) then
  begin
    if not IsViewStateEqual(AViewInfo.State, GetItemViewState(AItem)) then
    begin
      AViewInfo.FState := GetItemViewState(AItem);
      InvalidateItem(AViewInfo);
    end;
  end;
end;

procedure TdxGalleryControlController.UpdateMouseHoveredItem;
begin
  UpdateMouseHoveredItem(Owner.GetMouseCursorClientPos)
end;

procedure TdxGalleryControlController.UpdateMouseHoveredItem(X, Y: Integer);
begin
  UpdateMouseHoveredItem(Point(X, Y));
end;

procedure TdxGalleryControlController.UpdateMouseHoveredItem(const P: TPoint);
var
  AItemViewInfo: TdxGalleryItemViewInfo;
begin
  if Owner.ViewInfo.GetItemAtPos(P, AItemViewInfo) and AItemViewInfo.Item.Enabled
  then
    MouseHoveredItem := AItemViewInfo.Item
  else
    MouseHoveredItem := nil;
end;

procedure TdxGalleryControlController.SelectItems(AStartFromItem,
  AFinishAtItem: TdxGalleryControlItem);
var
  AFinishIndex: Integer;
  AList: TList;
  AStartIndex: Integer;
  I: Integer;
begin
  Owner.BeginUpdate;
  try
    AList := TList.Create;
    try
      Gallery.GetAllItems(AList);
      AStartIndex := Min(AList.IndexOf(AStartFromItem),
        AList.IndexOf(AFinishAtItem));
      AFinishIndex := Max(AList.IndexOf(AStartFromItem),
        AList.IndexOf(AFinishAtItem));
      for I := 0 to AList.Count - 1 do
        TdxGalleryItem(AList[I]).Checked := (I >= AStartIndex) and
          (I <= AFinishIndex);
    finally
      AList.Free;
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TdxGalleryControlController.GetNextItem(var AItemPos: TPoint;
  ADirectionX, ADirectionY: Integer);
var
  ANewItemPosX: Integer;
begin
  AItemPos.X := Max(0, Min(AItemPos.X + ADirectionX,
    NavigationMatrix.ColumnCount - 1));
  if ADirectionY <> 0 then
  begin
    repeat
      AItemPos.Y := Max(0, Min(AItemPos.Y + ADirectionY,
        NavigationMatrix.RowCount - 1));
      if NavigationMatrix.Values[AItemPos.X, AItemPos.Y] = nil then
        ANewItemPosX := NavigationMatrix.GetRightMostItemIndex(AItemPos.Y)
      else
        ANewItemPosX := AItemPos.X until (NavigationMatrix.Values[ANewItemPosX,
          AItemPos.Y] <> nil) or (AItemPos.Y = NavigationMatrix.RowCount - 1) or
          (AItemPos.Y = 0);
      AItemPos.X := ANewItemPosX;
    end;
  end;

  function TdxGalleryControlController.GetStartItemForKeyboardNavigation
    : TdxGalleryControlItem;
  begin
    Result := KeySelectedItem;
    if Result = nil then
      Result := Gallery.GetCheckedItem;
    if Result = nil then
      Result := Gallery.GetFirstVisibleItem;
  end;

  procedure TdxGalleryControlController.SelectNextItem
    (AItem: TdxGalleryControlItem; ADirectionX, ADirectionY: Integer;
    AShift: TShiftState);

    function ValidateItem(AItemPos: TPoint): Boolean;
    begin
      Result := (High(NavigationMatrix.FValues) >= AItemPos.X) and
        (High(NavigationMatrix.FValues[AItemPos.X]) >= AItemPos.Y) and
        (NavigationMatrix.Values[AItemPos.X, AItemPos.Y] <> nil);
    end;

  var
    AItemPos: TPoint;
  begin
    if AItem <> nil then
    begin
      AItemPos := GetItemPosition(AItem);
      GetNextItem(AItemPos, ADirectionX, ADirectionY);
      if ValidateItem(AItemPos) then
        KeySelectedItem := NavigationMatrix.Values[AItemPos.X, AItemPos.Y];
    end
    else
      KeySelectedItem := Gallery.GetFirstVisibleItem;

    if IsListViewStyleSelection then
    begin
      if not(ssShift in AShift) then
        StartSelectionItem := KeySelectedItem;
      if not(ssCtrl in AShift) then
        SelectItems(StartSelectionItem, KeySelectedItem);
    end
    else if ItemCheckMode = icmSingleRadio then
      SelectItems(KeySelectedItem, KeySelectedItem);
  end;

  procedure TdxGalleryControlController.ProcessItemClick
    (AItem: TdxGalleryControlItem; X, Y: Integer);
  begin
    Gallery.ClickItem(AItem);
  end;

  procedure TdxGalleryControlController.AutoScrollTimerHandler(Sender: TObject);
  const
    Map: array [Boolean] of TScrollCode = (scLineUp, scLineDown);
  var
    AScrollPos: Integer;
  begin
    AScrollPos := 0;
    Owner.Scroll(sbVertical, Map[AutoScrollTimer.Tag > 0], AScrollPos);
    CalculateDropTarget(Owner.CalcCursorPos);
  end;

  function TdxGalleryControlController.CanChangeSelection(AButton: TMouseButton;
    AShift: TShiftState): Boolean;
  begin
    Result := (AButton = mbLeft) or (AButton = mbRight) and
      Owner.OptionsBehavior.SelectOnRightClick and
      (AShift * [ssShift, ssCtrl] = []);
  end;

  function TdxGalleryControlController.GetDropTargetInfo
    : TdxGalleryControlDropTargetViewInfo;
  begin
    Result := ViewInfo.DropTarget;
  end;

  function TdxGalleryControlController.GetGallery: TdxGalleryControlStructure;
  begin
    Result := Owner.Gallery;
  end;

  function TdxGalleryControlController.GetNavigationMatrix
    : TdxGalleryControlNavigationMatrix;
  begin
    if FNavigationMatrix = nil then
      FNavigationMatrix := CreateNavigationMatrix;
    Result := FNavigationMatrix;
  end;

  function TdxGalleryControlController.GetViewInfo: TdxGalleryControlViewInfo;
  begin
    Result := Owner.ViewInfo;
  end;

  procedure TdxGalleryControlController.SetKeyPressed(AValue: Boolean);
  begin
    if FKeyPressed <> AValue then
    begin
      FKeyPressed := AValue;
      UpdateItemViewState(KeySelectedItem);
    end;
  end;

  procedure TdxGalleryControlController.SetKeySelectedItem
    (AItem: TdxGalleryControlItem);
  begin
    if FKeySelectedItem <> AItem then
    begin
      ExchangePointers(FKeySelectedItem, AItem);
      UpdateItemViewState(AItem);
      UpdateItemViewState(KeySelectedItem);
      MakeItemVisible(KeySelectedItem);
    end;
  end;

  procedure TdxGalleryControlController.SetMouseHoveredItem
    (AItem: TdxGalleryControlItem);
  begin
    if MouseHoveredItem <> AItem then
    begin
      Application.CancelHint;
      ExchangePointers(FMouseHoveredItem, AItem);
      UpdateItemViewState(AItem);
      UpdateItemViewState(MouseHoveredItem);
    end;
  end;

  procedure TdxGalleryControlController.SetMousePressed(AValue: Boolean);
  begin
    if MousePressed <> AValue then
    begin
      FMousePressed := AValue;
      UpdateItemViewState(MouseHoveredItem);
    end;
  end;

  { TdxGalleryControlCustomOptions }

  procedure TdxGalleryControlCustomOptions.Changed(AType: TdxChangeType);
  begin
    Owner.LayoutChanged(AType);
  end;

  procedure TdxGalleryControlCustomOptions.ChangeScale(M, D: Integer);
  begin
    // do nothing
  end;

  { TdxGalleryControlOptionsBehavior }

  constructor TdxGalleryControlOptionsBehavior.Create
    (AOwner: TdxCustomGalleryControl);
  begin
    inherited Create(AOwner);
    FItemHotTrack := True;
  end;

  procedure TdxGalleryControlOptionsBehavior.DoAssign(Source: TPersistent);
  begin
    inherited DoAssign(Source);
    if Source is TdxGalleryControlOptionsBehavior then
    begin
      ItemMultiSelectKind := TdxGalleryControlOptionsBehavior(Source)
        .ItemMultiSelectKind;
      ItemCheckMode := TdxGalleryControlOptionsBehavior(Source).ItemCheckMode;
      ItemShowHint := TdxGalleryControlOptionsBehavior(Source).ItemShowHint;
      ItemHotTrack := TdxGalleryControlOptionsBehavior(Source).ItemHotTrack;
    end;
  end;

  function TdxGalleryControlOptionsBehavior.GetItemCheckMode
    : TdxGalleryItemCheckMode;
  begin
    Result := Owner.Gallery.ItemCheckMode;
  end;

  procedure TdxGalleryControlOptionsBehavior.SetItemCheckMode
    (const Value: TdxGalleryItemCheckMode);
  begin
    Owner.Gallery.ItemCheckMode := Value;
  end;

  { TdxGalleryControlOptionsItemImage }

  constructor TdxGalleryControlOptionsItemImage.Create
    (AOwner: TdxCustomGalleryControl);
  begin
    inherited Create(AOwner);
    FSize := TcxSize.Create(Self);
    FSize.OnChange := ChangeHandler;
    FShowFrame := True;
  end;

  destructor TdxGalleryControlOptionsItemImage.Destroy;
  begin
    FreeAndNil(FSize);
    inherited Destroy;
  end;

  procedure TdxGalleryControlOptionsItemImage.ChangeScale(M, D: Integer);
  begin
    inherited ChangeScale(M, D);
    Size.Size := cxSizeScale(Size.Size, M, D);
  end;

  procedure TdxGalleryControlOptionsItemImage.DoAssign(Source: TPersistent);
  begin
    inherited DoAssign(Source);
    if Source is TdxGalleryControlOptionsItemImage then
    begin
      Size := TdxGalleryControlOptionsItemImage(Source).Size;
      ShowFrame := TdxGalleryControlOptionsItemImage(Source).ShowFrame;
    end;
  end;

  procedure TdxGalleryControlOptionsItemImage.ChangeHandler(Sender: TObject);
  begin
    Changed;
  end;

  procedure TdxGalleryControlOptionsItemImage.SetShowFrame
    (const Value: Boolean);
  begin
    if Value <> FShowFrame then
    begin
      FShowFrame := Value;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsItemImage.SetSize(const Value: TcxSize);
  begin
    FSize.Assign(Value);
  end;

  { TdxGalleryControlOptionsItemText }

  constructor TdxGalleryControlOptionsItemText.Create
    (AOwner: TdxCustomGalleryControl);
  begin
    inherited Create(AOwner);
    FAlignHorz := taCenter;
    FWordWrap := True;
  end;

  procedure TdxGalleryControlOptionsItemText.DoAssign(Source: TPersistent);
  begin
    inherited DoAssign(Source);
    if Source is TdxGalleryControlOptionsItemText then
    begin
      AlignHorz := TdxGalleryControlOptionsItemText(Source).AlignHorz;
      AlignVert := TdxGalleryControlOptionsItemText(Source).AlignVert;
      Position := TdxGalleryControlOptionsItemText(Source).Position;
      WordWrap := TdxGalleryControlOptionsItemText(Source).WordWrap;
    end;
  end;

  procedure TdxGalleryControlOptionsItemText.SetAlignHorz
    (const Value: TAlignment);
  begin
    if FAlignHorz <> Value then
    begin
      FAlignHorz := Value;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsItemText.SetAlignVert
    (const Value: TcxAlignmentVert);
  begin
    if FAlignVert <> Value then
    begin
      FAlignVert := Value;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsItemText.SetPosition
    (const Value: TcxPosition);
  begin
    if FPosition <> Value then
    begin
      FPosition := Value;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsItemText.SetWordWrap(const Value: Boolean);
  begin
    if FWordWrap <> Value then
    begin
      FWordWrap := Value;
      Changed
    end;
  end;

  { TdxGalleryControlOptionsItem }

  constructor TdxGalleryControlOptionsItem.Create
    (AOwner: TdxCustomGalleryControl);
  begin
    inherited Create(AOwner);
    FImage := CreateImage;
    FText := CreateText;
  end;

  destructor TdxGalleryControlOptionsItem.Destroy;
  begin
    FreeAndNil(FText);
    FreeAndNil(FImage);
    inherited Destroy;
  end;

  procedure TdxGalleryControlOptionsItem.ChangeScale(M, D: Integer);
  begin
    inherited ChangeScale(M, D);
    Image.ChangeScale(M, D);
    Text.ChangeScale(M, D);
  end;

  function TdxGalleryControlOptionsItem.CreateImage
    : TdxGalleryControlOptionsItemImage;
  begin
    Result := TdxGalleryControlOptionsItemImage.Create(Owner);
  end;

  function TdxGalleryControlOptionsItem.CreateText
    : TdxGalleryControlOptionsItemText;
  begin
    Result := TdxGalleryControlOptionsItemText.Create(Owner);
  end;

  procedure TdxGalleryControlOptionsItem.DoAssign(Source: TPersistent);
  begin
    inherited DoAssign(Source);
    if Source is TdxGalleryControlOptionsItem then
      Text := TdxGalleryControlOptionsItem(Source).Text;
  end;

  procedure TdxGalleryControlOptionsItem.SetImage
    (AValue: TdxGalleryControlOptionsItemImage);
  begin
    FImage.Assign(AValue);
  end;

  procedure TdxGalleryControlOptionsItem.SetText
    (AValue: TdxGalleryControlOptionsItemText);
  begin
    FText.Assign(AValue);
  end;

  { TdxGalleryControlOptionsView }

  constructor TdxGalleryControlOptionsView.Create
    (AOwner: TdxCustomGalleryControl);
  begin
    inherited Create(AOwner);
    FItem := CreateItem;
    FContentOffset := TcxMargin.Create(Self, 1);
    FContentOffset.OnChange := ChangeHandler;
    FContentOffsetGroups := TcxMargin.Create(Self, 0);
    FContentOffsetGroups.OnChange := ChangeHandler;
    FContentOffsetItems := TcxMargin.Create(Self, 6);
    FContentOffsetItems.OnChange := ChangeHandler;
  end;

  destructor TdxGalleryControlOptionsView.Destroy;
  begin
    FreeAndNil(FContentOffsetItems);
    FreeAndNil(FContentOffsetGroups);
    FreeAndNil(FContentOffset);
    FreeAndNil(FItem);
    inherited Destroy;
  end;

  procedure TdxGalleryControlOptionsView.ChangeScale(M, D: Integer);
  begin
    inherited ChangeScale(M, D);
    ContentOffset.Margin := cxRectScale(ContentOffset.Margin, M, D);
    ContentOffsetGroups.Margin := cxRectScale(ContentOffsetGroups.Margin, M, D);
    ContentOffsetItems.Margin := cxRectScale(ContentOffsetItems.Margin, M, D);
    Item.ChangeScale(M, D);
  end;

  function TdxGalleryControlOptionsView.CreateItem
    : TdxGalleryControlOptionsItem;
  begin
    Result := TdxGalleryControlOptionsItem.Create(Owner);
  end;

  procedure TdxGalleryControlOptionsView.DoAssign(Source: TPersistent);
  begin
    inherited DoAssign(Source);
    if Source is TdxGalleryControlOptionsView then
    begin
      ColumnCount := TdxGalleryControlOptionsView(Source).ColumnCount;
      ColumnAutoWidth := TdxGalleryControlOptionsView(Source).ColumnAutoWidth;
      ContentOffset := TdxGalleryControlOptionsView(Source).ContentOffset;
      ContentOffsetGroups := TdxGalleryControlOptionsView(Source)
        .ContentOffsetGroups;
      ContentOffsetItems := TdxGalleryControlOptionsView(Source)
        .ContentOffsetItems;
      Item := TdxGalleryControlOptionsView(Source).Item;
    end;
  end;

  procedure TdxGalleryControlOptionsView.ChangeHandler(Sender: TObject);
  begin
    Changed;
  end;

  procedure TdxGalleryControlOptionsView.SetColumnAutoWidth(AValue: Boolean);
  begin
    if FColumnAutoWidth <> AValue then
    begin
      FColumnAutoWidth := AValue;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsView.SetColumnCount(AValue: Integer);
  begin
    AValue := Max(0, AValue);
    if FColumnCount <> AValue then
    begin
      FColumnCount := AValue;
      Changed;
    end;
  end;

  procedure TdxGalleryControlOptionsView.SetContentOffset
    (const Value: TcxMargin);
  begin
    FContentOffset.Assign(Value);
  end;

  procedure TdxGalleryControlOptionsView.SetContentOffsetGroups
    (const Value: TcxMargin);
  begin
    FContentOffsetGroups.Assign(Value);
  end;

  procedure TdxGalleryControlOptionsView.SetContentOffsetItems
    (const Value: TcxMargin);
  begin
    FContentOffsetItems.Assign(Value);
  end;

  procedure TdxGalleryControlOptionsView.SetItem(const Value
    : TdxGalleryControlOptionsItem);
  begin
    FItem.Assign(Value);
  end;

  { TdxGalleryControlDragObject }

  constructor TdxGalleryControlDragObject.Create(AControl: TControl);
  begin
    inherited Create(AControl);
    FSelectedItems := TList.Create;
  end;

  destructor TdxGalleryControlDragObject.Destroy;
  begin
    FreeAndNil(FSelectedItems);
    inherited Destroy;
  end;

  function TdxGalleryControlDragObject.CheckTarget
    (ATarget: TdxGalleryControlDropTargetViewInfo): Boolean;
  begin
    Result := (ATarget.TargetObject <> nil) and
      (SelectedItems.IndexOf(ATarget.TargetObject) < 0);
  end;

  function TdxGalleryControlDragObject.GetDragCursor(Accepted: Boolean;
    X, Y: Integer): TCursor;
  begin
    if Accepted and (TObject(DragTarget) is TdxCustomGalleryControl) then
    begin
      if TdxCustomGalleryControl(DragTarget).Controller.DragCopy then
        Exit(crDragCopy);
    end;
    Result := inherited GetDragCursor(Accepted, X, Y);
  end;

  function TdxGalleryControlDragObject.GetControl: TdxCustomGalleryControl;
  begin
    Result := TdxCustomGalleryControl(inherited Control);
  end;

  { TdxCustomGalleryControl }

  constructor TdxCustomGalleryControl.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);

    ControlStyle := ControlStyle - [csParentBackground];

    CanBeFocused := True;
    BorderStyle := cxcbsDefault;
    Width := 150;
    Height := 100;

    FGallery := CreateGallery;
    TdxGalleryAccess(FGallery).OnChange := GalleryChangeHandler;
    TdxGalleryAccess(FGallery).OnItemClick := GalleryItemClickHandler;

    FOptionsBehavior := CreateOptionsBehavior;
    FOptionsView := CreateOptionsView;

    FImageChangeLink := TChangeLink.Create;
    FImageChangeLink.OnChange := ImageListChanged;

    ShowHint := True;
    Keys := [kArrows];
  end;

  destructor TdxCustomGalleryControl.Destroy;
  begin
    FreeAndNil(FImageChangeLink);
    FreeAndNil(FOptionsBehavior);
    FreeAndNil(FOptionsView);
    FreeAndNil(FGallery);
    inherited Destroy;
  end;

  procedure TdxCustomGalleryControl.BeginUpdate;
  begin
    Inc(FLockCount);
  end;

  procedure TdxCustomGalleryControl.EndUpdate;
  begin
    Dec(FLockCount);
    if FLockCount = 0 then
    begin
      if Controller <> nil then
        Controller.CheckSelectedItem;
      LayoutChanged;
    end;
  end;

  function TdxCustomGalleryControl.CanFocus: Boolean;
  begin
    Result := inherited CanFocus and CanBeFocused;
  end;

  procedure TdxCustomGalleryControl.GetChildren(Proc: TGetChildProc;
    Root: TComponent);
  begin
    Gallery.GetChildren(Proc, Root);
  end;

  procedure TdxCustomGalleryControl.MakeItemVisible
    (AItem: TdxGalleryControlItem);
  begin
    Controller.MakeItemVisible(AItem);
  end;

  function TdxCustomGalleryControl.CreateController
    : TdxGalleryControlController;
  begin
    Result := TdxGalleryControlController.Create(Self);
  end;

  function TdxCustomGalleryControl.CreateGallery: TdxGalleryControlStructure;
  begin
    Result := TdxGalleryControlStructure.Create(Self);
  end;

  function TdxCustomGalleryControl.CreateOptionsBehavior
    : TdxGalleryControlOptionsBehavior;
  begin
    Result := TdxGalleryControlOptionsBehavior.Create(Self);
  end;

  function TdxCustomGalleryControl.CreateOptionsView
    : TdxGalleryControlOptionsView;
  begin
    Result := TdxGalleryControlOptionsView.Create(Self);
  end;

  function TdxCustomGalleryControl.CreatePainter: TdxGalleryControlPainter;
  begin
    Result := TdxGalleryControlPainter.Create(Self);
  end;

  function TdxCustomGalleryControl.CreateViewInfo: TdxGalleryControlViewInfo;
  begin
    Result := TdxGalleryControlViewInfo.Create(Self);
  end;

  procedure TdxCustomGalleryControl.CreateViewSubClasses;
  begin
    FViewInfo := CreateViewInfo;
    FController := CreateController;
    FPainter := CreatePainter;
  end;

  procedure TdxCustomGalleryControl.DestroyViewSubClasses;
  begin
    FreeAndNil(FViewInfo);
    FreeAndNil(FController);
    FreeAndNil(FPainter);
  end;

  procedure TdxCustomGalleryControl.FocusEnter;
  begin
    inherited FocusEnter;
    Controller.FocusEnter;
  end;

  procedure TdxCustomGalleryControl.FocusLeave;
  begin
    inherited FocusLeave;
    Controller.FocusLeave;
  end;

  procedure TdxCustomGalleryControl.KeyDown(var Key: Word; Shift: TShiftState);
  begin
    inherited KeyDown(Key, Shift);
    Controller.KeyDown(Key, Shift);
  end;

  procedure TdxCustomGalleryControl.KeyUp(var Key: Word; Shift: TShiftState);
  begin
    inherited KeyUp(Key, Shift);
    Controller.KeyUp(Key, Shift);
  end;

  procedure TdxCustomGalleryControl.MouseUp(Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    inherited MouseUp(Button, Shift, X, Y);
    Controller.MouseUp(Button, Shift, X, Y);
  end;

  procedure TdxCustomGalleryControl.MouseDown(Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Controller.MouseDown(Button, Shift, X, Y);
  end;

  procedure TdxCustomGalleryControl.MouseLeave(AControl: TControl);
  begin
    inherited MouseLeave(AControl);
    Controller.MouseLeave;
  end;

  procedure TdxCustomGalleryControl.MouseMove(Shift: TShiftState;
    X, Y: Integer);
  begin
    inherited MouseMove(Shift, X, Y);
    Controller.MouseMove(Shift, X, Y);
  end;

  procedure TdxCustomGalleryControl.DragOver(Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
  begin
    inherited DragOver(Source, X, Y, State, Accept);

    case State of
      dsDragEnter:
        Controller.DragEnter;
      dsDragLeave:
        Controller.DragLeave;
      dsDragMove:
        Controller.DragOver(Source, X, Y, Accept);
    end;
  end;

  function TdxCustomGalleryControl.GetDragObjectClass: TDragControlObjectClass;
  begin
    Result := TdxGalleryControlDragObject;
  end;

  procedure TdxCustomGalleryControl.DragDrop(Source: TObject; X, Y: Integer);
  begin
    Controller.DragDrop(Source, X, Y);
    Controller.ResetDropTarget;
    inherited;
  end;

  function TdxCustomGalleryControl.StartDrag(DragObject: TDragObject): Boolean;
  var
    AHoveredItem: TdxGalleryControlItem;
    AItemDragObject: TdxGalleryControlDragObject;
  begin
    if DragObject is TdxGalleryControlDragObject then
    begin
      AItemDragObject := TdxGalleryControlDragObject(DragObject);

      Controller.KeySelectedItem := nil;
      Controller.UpdateMouseHoveredItem;

      AHoveredItem := Controller.MouseHoveredItem;
      if AHoveredItem <> nil then
      begin
        if AHoveredItem.Checked then
          Gallery.GetCheckedItems(AItemDragObject.SelectedItems)
        else
        begin
          Gallery.UncheckAll;
          AHoveredItem.Checked := True;
          AItemDragObject.SelectedItems.Add(AHoveredItem);
        end;
      end;
      Result := AItemDragObject.SelectedItems.Count > 0;
    end
    else
      Result := False;
  end;

  function TdxCustomGalleryControl.CanAutoSize(var NewWidth,
    NewHeight: Integer): Boolean;
  begin
    Result := GetItemCount > 0;
    if Result then
    begin
      Calculate(ctMedium);
      case AutoSizeMode of
        asAutoSize:
          begin
            NewWidth := cxRectWidth(ViewInfo.ContentBounds) +
              cxMarginsWidth(ViewInfo.BorderWidths);
            NewHeight := cxRectHeight(ViewInfo.ContentBounds) +
              cxMarginsHeight(ViewInfo.BorderWidths);
          end;

        asAutoWidth:
          begin
            NewWidth := cxRectWidth(ViewInfo.ContentBounds) +
              cxMarginsWidth(ViewInfo.BorderWidths);
            if not IsTouchScrollUIMode and IsScrollBarActive(sbVertical) then
              Inc(NewWidth, GetVScrollBarAreaWidth);
          end;

        asAutoHeight:
          begin
            NewHeight := cxRectHeight(ViewInfo.ContentBounds) +
              cxMarginsHeight(ViewInfo.BorderWidths);
            if not IsTouchScrollUIMode and IsScrollBarActive(sbHorizontal) then
              Inc(NewHeight, GetHScrollBarAreaHeight);
          end;
      end;
    end;
  end;

  procedure TdxCustomGalleryControl.Notification(AComponent: TComponent;
    Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if (Operation = opRemove) and (AComponent = Images) then
      Images := nil;
  end;

  function TdxCustomGalleryControl.GetContentSize: TSize;
  begin
    Result := cxSize(cxRectInflate(ViewInfo.ContentBounds,
      ContentOffset.Margin));
    if GetScrollbarMode = sbmHybrid then
    begin
      if IsScrollBarActive(sbHorizontal) then
        Inc(Result.cy, GetHScrollBarDefaultAreaHeight);
      if IsScrollBarActive(sbVertical) then
        Inc(Result.cx, GetVScrollBarDefaultAreaWidth);
    end;
  end;

  procedure TdxCustomGalleryControl.BoundsChanged;
  begin
    LayoutChanged(ctMedium);
  end;

  procedure TdxCustomGalleryControl.Calculate(AType: TdxChangeType);
  begin
    ViewInfo.Calculate(AType, ClientBounds);
    if AType <> ctLight then
      Controller.LayoutChanged;
  end;

  procedure TdxCustomGalleryControl.CreateHandle;
  begin
    BeginUpdate;
    try
      inherited CreateHandle;
    finally
      EndUpdate;
    end;
  end;

  procedure TdxCustomGalleryControl.LayoutChanged(AType: TdxChangeType);
  begin
    if not IsUpdateLocked then
      inherited LayoutChanged(AType);
  end;

  procedure TdxCustomGalleryControl.ScrollPosChanged(const AOffset: TPoint);
  begin
    inherited;
    Controller.UpdateMouseHoveredItem;
  end;

  function TdxCustomGalleryControl.AllowTouchScrollUIMode: Boolean;
  begin
    Result := not IsDesigning;
  end;

  procedure TdxCustomGalleryControl.ChangeScaleEx(M, D: Integer;
    isDpiChange: Boolean);
  begin
    BeginUpdate;
    try
      inherited;
      OptionsBehavior.ChangeScale(M, D);
      OptionsView.ChangeScale(M, D);
      Gallery.ChangeScale(M, D);
    finally
      EndUpdate;
    end;
  end;

  procedure TdxCustomGalleryControl.CreateActualCanvas;
  begin
    inherited CreateActualCanvas;
    CreateViewSubClasses;
    if HandleAllocated then
    begin
      LayoutChanged;
      Invalidate;
    end;
  end;

  procedure TdxCustomGalleryControl.FreeActualCanvas;
  begin
    DestroyViewSubClasses;
    inherited FreeActualCanvas;
  end;

  procedure TdxCustomGalleryControl.DoPaint;
  begin
    inherited DoPaint;
    ViewInfo.Draw(ActualCanvas);
  end;

  procedure TdxCustomGalleryControl.EnabledChanged;
  begin
    inherited;
    LayoutChanged;
  end;

  procedure TdxCustomGalleryControl.FontChanged;
  begin
    Painter.UpdateFonts;
    inherited FontChanged;
    LayoutChanged;
  end;

  function TdxCustomGalleryControl.GetScrollContentForegroundColor: TColor;
  var
    AState: TdxGalleryItemViewState;
  begin
    ZeroMemory(@AState, SizeOf(AState));
    AState.Enabled := True;
    Result := LookAndFeelPainter.GetGalleryItemCaptionTextColor(AState);
  end;

  function TdxCustomGalleryControl.HasScrollBarArea: Boolean;
  begin
    Result := inherited HasScrollBarArea or (GetScrollbarMode = sbmHybrid);
  end;

  procedure TdxCustomGalleryControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
    AChangedValues: TcxLookAndFeelValues);
  begin
    Painter.FlushCache;
    inherited;
  end;

  procedure TdxCustomGalleryControl.SetAutoSizeMode(AValue: TdxAutoSizeMode);
  begin
    if AutoSizeMode <> AValue then
    begin
      inherited SetAutoSizeMode(AValue);
      LayoutChanged;
    end;
  end;

  procedure TdxCustomGalleryControl.DoClickItem(AItem: TdxGalleryItem);
  begin
    if Assigned(OnItemClick) then
      OnItemClick(Self, AItem as TdxGalleryControlItem);
  end;

  function TdxCustomGalleryControl.GetItemAtPos(const P: TPoint)
    : TdxGalleryControlItem;
  var
    AViewInfo: TdxGalleryItemViewInfo;
  begin
    if ViewInfo.GetItemAtPos(P, AViewInfo) then
      Result := AViewInfo.Item
    else
      Result := nil;
  end;

  function TdxCustomGalleryControl.IsUpdateLocked: Boolean;
  begin
    Result := (FLockCount > 0) or IsLoading;
  end;

  function TdxCustomGalleryControl.GetGallery: IdxGallery;
  begin
    Result := Gallery;
  end;

  function TdxCustomGalleryControl.GetGallery2: IdxGallery2;
  begin
    Result := Gallery;
  end;

  function TdxCustomGalleryControl.GetColumnCount: Integer;
  begin
    Result := OptionsView.ColumnCount;
  end;

  function TdxCustomGalleryControl.GetContentOffset: TcxMargin;
  begin
    Result := OptionsView.ContentOffset;
  end;

  function TdxCustomGalleryControl.GetContentOffsetGroups: TcxMargin;
  begin
    Result := OptionsView.ContentOffsetGroups;
  end;

  function TdxCustomGalleryControl.GetContentOffsetItems: TcxMargin;
  begin
    Result := OptionsView.ContentOffsetItems;
  end;

  function TdxCustomGalleryControl.GetItemCheckMode: TdxGalleryItemCheckMode;
  begin
    Result := OptionsBehavior.ItemCheckMode;
  end;

  function TdxCustomGalleryControl.GetItemCount: Integer;
  var
    AList: TList;
  begin
    AList := TList.Create;
    try
      Gallery.GetAllItems(AList);
      Result := AList.Count;
    finally
      AList.Free;
    end;
  end;

  function TdxCustomGalleryControl.GetItemImageSize: TcxSize;
  begin
    Result := OptionsView.Item.Image.Size;
  end;

  function TdxCustomGalleryControl.GetItemShowHint: Boolean;
  begin
    Result := OptionsBehavior.ItemShowHint;
  end;

  function TdxCustomGalleryControl.GetItemShowImageFrame: Boolean;
  begin
    Result := OptionsView.Item.Image.ShowFrame;
  end;

  function TdxCustomGalleryControl.GetItemTextPosition: TcxPosition;
  begin
    Result := OptionsView.Item.Text.Position;
  end;

  procedure TdxCustomGalleryControl.SetGallery
    (AValue: TdxGalleryControlStructure);
  begin
    FGallery.Assign(AValue);
  end;

  procedure TdxCustomGalleryControl.SetColumnCount(AValue: Integer);
  begin
    OptionsView.ColumnCount := AValue;
  end;

  procedure TdxCustomGalleryControl.SetContentOffset(AValue: TcxMargin);
  begin
    OptionsView.ContentOffset := AValue;
  end;

  procedure TdxCustomGalleryControl.SetContentOffsetGroups(AValue: TcxMargin);
  begin
    OptionsView.ContentOffsetGroups := AValue;
  end;

  procedure TdxCustomGalleryControl.SetContentOffsetItems(AValue: TcxMargin);
  begin
    OptionsView.ContentOffsetItems := AValue;
  end;

  procedure TdxCustomGalleryControl.SetImages(Value: TCustomImageList);
  begin
    cxSetImageList(Value, FImages, FImageChangeLink, Self);
  end;

  procedure TdxCustomGalleryControl.SetItemCheckMode
    (AValue: TdxGalleryItemCheckMode);
  begin
    OptionsBehavior.ItemCheckMode := AValue;
  end;

  procedure TdxCustomGalleryControl.SetItemImageSize(AValue: TcxSize);
  begin
    OptionsView.Item.Image.Size := AValue;
  end;

  procedure TdxCustomGalleryControl.SetItemTextPosition(AValue: TcxPosition);
  begin
    OptionsView.Item.Text.Position := AValue;
  end;

  procedure TdxCustomGalleryControl.SetItemShowHint(const Value: Boolean);
  begin
    OptionsBehavior.ItemShowHint := Value;
  end;

  procedure TdxCustomGalleryControl.SetItemShowImageFrame(AValue: Boolean);
  begin
    OptionsView.Item.Image.ShowFrame := AValue;
  end;

  procedure TdxCustomGalleryControl.GalleryChangeHandler(ASender: TObject;
    AChangeType: TdxGalleryChangeType);
  begin
    if HandleAllocated and not IsUpdateLocked then
    begin
      Controller.CheckSelectedItem;
      LayoutChanged;
      Controller.UpdateMouseHoveredItem;
    end;
  end;

  procedure TdxCustomGalleryControl.GalleryItemClickHandler(ASender: TObject;
    AItem: TdxGalleryItem);
  begin
    DoClickItem(AItem);
  end;

  procedure TdxCustomGalleryControl.SetOptionsBehavior
    (const Value: TdxGalleryControlOptionsBehavior);
  begin
    FOptionsBehavior.Assign(Value);
  end;

  procedure TdxCustomGalleryControl.SetOptionsView
    (const Value: TdxGalleryControlOptionsView);
  begin
    FOptionsView.Assign(Value);
  end;

  procedure TdxCustomGalleryControl.ImageListChanged(Sender: TObject);
  begin
    if Sender = Images then
      LayoutChanged;
  end;

  procedure TdxCustomGalleryControl.CMHintShow(var Message: TCMHintShow);
  var
    AItemViewInfo: TdxGalleryItemViewInfo;
  begin
    if ItemShowHint and not Controller.IsInDragAndDropOperation then
    begin
      if ViewInfo.GetItemAtPos(Message.HintInfo.CursorPos, AItemViewInfo) then
        if AItemViewInfo.Item.Enabled and (AItemViewInfo.Item.Hint <> '') then
        begin
          Message.HintInfo.CursorRect := AItemViewInfo.Bounds;
          Message.HintInfo.HintStr := AItemViewInfo.Item.Hint;
          Message.Result := 0;
        end;
    end;
  end;

initialization

RegisterClasses([TdxGalleryControlItem, TdxGalleryControlGroup]);

end.
