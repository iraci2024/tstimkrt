{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressGanttControl }
{ }
{ Copyright (c) 2020 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSGANTTCONTROL AND ALL }
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

unit dxGanttControlCustomSheet;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, ImgList, Windows, Controls,
  Generics.Defaults, Generics.Collections, Forms, StdCtrls, Variants,
  dxCore, dxCoreClasses, cxGraphics, cxCustomCanvas, cxGeometry, cxClasses,
  cxListBox,
  dxGDIPlusClasses, cxControls, cxVariants, cxEdit, cxDropDownEdit,
  dxUIElementPopupWindow,
  cxDrawTextUtils, cxLookAndFeels, cxLookAndFeelPainters, dxTouch,
  dxGanttControlCustomClasses,
  dxGanttControlCommands;

type
  TdxGanttControlSheetCustomViewInfo = class;
  TdxGanttControlSheetHeaderViewInfo = class;
  TdxGanttControlSheetColumnHeaderViewInfo = class;
  TdxGanttControlSheetRowHeaderViewInfo = class;
  TdxGanttControlSheetDataRowViewInfo = class;
  TdxGanttControlSheetColumns = class;
  TdxGanttControlSheetOptions = class;
  TdxGanttControlSheetColumn = class;
  TdxGanttControlSheetDragHelper = class;
  TdxGanttSheetColumnQuickCustomizationPopup = class;
  TdxGanttControlSheetController = class;

  TdxGanttControlSheetColumnClass = class of TdxGanttControlSheetColumn;

  { TdxGanttControlSheetControllerHistoryItem }

  TdxGanttControlSheetControllerHistoryItem = class(TdxGanttControlHistoryItem)
  strict private
    FController: TdxGanttControlSheetController;
  public
    constructor Create(AController: TdxGanttControlSheetController);
      reintroduce; virtual;
    property Controller: TdxGanttControlSheetController read FController;
  end;

  { TdxGanttControlSheetResizeHistoryItem }

  TdxGanttControlSheetResizeHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  strict private
    FOldWidth: Integer;
  protected
    FColumn: TdxGanttControlSheetColumn;
    FNewWidth: Integer;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetChangeRowHeaderWidthHistoryItem }

  TdxGanttControlSheetChangeRowHeaderWidthHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  strict private
    FOldWidth: Integer;
  protected
    FNewWidth: Integer;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetToggleItemExpandStateHistoryItem }

  TdxGanttControlSheetToggleItemExpandStateHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FData: TObject;
    FIsExpanded: Boolean;

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetChangeValueHistoryItem }

  TdxGanttControlSheetChangeValueHistoryItem = class abstract
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FData: TObject;
    FNewValue: Variant;
    FOldValue: Variant;

    function GetEditValue: Variant; virtual; abstract;
    procedure SetEditValue(const Value: Variant); virtual; abstract;

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetChangeCellValueHistoryItem }

  TdxGanttControlSheetChangeCellValueHistoryItem = class
    (TdxGanttControlSheetChangeValueHistoryItem)
  protected
    FColumn: TdxGanttControlSheetColumn;

    function GetEditValue: Variant; override;
    procedure SetEditValue(const Value: Variant); override;
  end;

  { TdxGanttControlSheetFocusedCellChangeHistoryItem }

  TdxGanttControlSheetFocusedCellChangeHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FFirstVisibleCell: TPoint;
    FFocusedCell: TPoint;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlCreateDataItemHistoryItem }

  TdxGanttControlCreateDataItemHistoryItem = class abstract
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FDataItem: TObject;
    FIndex: Integer;
  public
    destructor Destroy; override;
  end;

  { TdxGanttControlAppendDataItemHistoryItem }

  TdxGanttControlAppendDataItemHistoryItem = class
    (TdxGanttControlCreateDataItemHistoryItem)
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlInsertDataItemHistoryItem }

  TdxGanttControlInsertDataItemHistoryItem = class
    (TdxGanttControlCreateDataItemHistoryItem)
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetMoveColumnHistoryItem }

  TdxGanttControlSheetMoveColumnHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FCurrentIndex: Integer;
    FNewIndex: Integer;
    FFirstVisibleColumnIndex: Integer;
    FNewFirstVisibleColumnIndex: Integer;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetHideColumnHistoryItem }

  TdxGanttControlSheetHideColumnHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FColumnIndex: Integer;
    FFirstVisibleColumnIndex: Integer;
    FNewFirstVisibleColumnIndex: Integer;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetShowColumnHistoryItem }

  TdxGanttControlSheetShowColumnHistoryItem = class
    (TdxGanttControlSheetControllerHistoryItem)
  protected
    FColumnIndex: Integer;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlSheetCommand }

  TdxGanttControlSheetCommand = class(TdxGanttControlCommand)
  strict private
    FController: TdxGanttControlSheetController;
  protected
    property Controller: TdxGanttControlSheetController read FController;
  public
    constructor Create(AController: TdxGanttControlSheetController);
      reintroduce; virtual;
  end;

  { TdxGanttControlSheetResizeColumnCommand }

  TdxGanttControlSheetResizeColumnCommand = class(TdxGanttControlSheetCommand)
  strict private
    FColumn: TdxGanttControlSheetColumn;
    FNewWidth: Integer;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      AColumn: TdxGanttControlSheetColumn; ANewWidth: Integer);
      reintroduce; virtual;
  end;

  { TdxGanttControlSheetChangeRowHeaderWidthCommand }

  TdxGanttControlSheetChangeRowHeaderWidthCommand = class
    (TdxGanttControlSheetCommand)
  strict private
    FNewWidth: Integer;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      ANewWidth: Integer); reintroduce; virtual;
  end;

  { TdxGanttControlSheetChangeCellCommand }

  TdxGanttControlSheetChangeCellCommand = class(TdxGanttControlSheetCommand)
  strict private
    function GetController: TdxGanttControlSheetController; inline;
  protected
    FColumn: TdxGanttControlSheetColumn;
    FData: TObject;
    procedure SaveCellPosition;
    property Controller: TdxGanttControlSheetController read GetController;
  public
    constructor Create(AController: TdxGanttControlSheetController);
      reintroduce;
  end;

  { TdxGanttControlSheetDeleteFocusedItemCommand }

  TdxGanttControlSheetDeleteFocusedItemCommand = class abstract
    (TdxGanttControlSheetChangeCellCommand)
  protected
    procedure BeforeExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetInsertNewItemCommand }

  TdxGanttControlSheetInsertNewItemCommand = class abstract
    (TdxGanttControlSheetChangeCellCommand)
  protected
    procedure BeforeExecute; override;
    procedure DoExecute; override;
  end;

  { TdxGanttControlSheetChangeCellValueCommand }

  TdxGanttControlSheetChangeCellValueCommand = class
    (TdxGanttControlSheetChangeCellCommand)
  strict private
    function CreateDataItem: TObject;
  protected
    FNewValue: Variant;
    function CreateChangeValueCommand: TdxGanttControlCommand; virtual;
      abstract;
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      const ANewValue: Variant); reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetToggleItemExpandStateCommand }

  TdxGanttControlSheetToggleItemExpandStateCommand = class
    (TdxGanttControlSheetChangeCellCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetExpandItemCommand }

  TdxGanttControlSheetExpandItemCommand = class
    (TdxGanttControlSheetToggleItemExpandStateCommand)
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetCollapseItemCommand }

  TdxGanttControlSheetCollapseItemCommand = class
    (TdxGanttControlSheetToggleItemExpandStateCommand)
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetMoveColumnCommand }

  TdxGanttControlSheetMoveColumnCommand = class(TdxGanttControlSheetCommand)
  strict private
    FCurrentIndex: Integer;
    FNewIndex: Integer;
    FFirstVisibleColumnIndex: Integer;
    FNewFirstVisibleColumnIndex: Integer;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      ACurrentIndex, ANewIndex: Integer;
      AFirstVisibleColumnIndex: Integer = -1); reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetHideColumnCommand }

  TdxGanttControlSheetHideColumnCommand = class(TdxGanttControlSheetCommand)
  strict private
    FColumnIndex: Integer;
    FFirstVisibleColumnIndex: Integer;
    FNewFirstVisibleColumnIndex: Integer;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      AColumnIndex: Integer; AFirstVisibleColumnIndex: Integer = -1);
      reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetShowColumnCommand }

  TdxGanttControlSheetShowColumnCommand = class(TdxGanttControlSheetCommand)
  strict private
    FColumnIndex: Integer;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      AColumnIndex: Integer); reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetAddColumnCommand }

  TdxGanttControlSheetAddColumnCommand = class(TdxGanttControlSheetCommand)
  strict private
    FColumnClass: TdxGanttControlSheetColumnClass;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlSheetController;
      AColumnClass: TdxGanttControlSheetColumnClass); reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSheetCellCustomViewInfo }

  TdxGanttControlSheetCellCustomViewInfo = class abstract
    (TdxGanttControlCustomParentViewInfo)
  strict private
    FColumn: TdxGanttControlSheetColumn;
    FColumnViewInfo: TdxGanttControlSheetHeaderViewInfo;
    function GetOwner: TdxGanttControlSheetDataRowViewInfo; inline;
  protected
    function CalculateBestFit: Integer; virtual;
    function IsFocused: Boolean;
    procedure DoDraw; override;
    function GetFont: TFont; virtual;

    procedure UpdateColumnViewInfo(AColumnViewInfo
      : TdxGanttControlSheetHeaderViewInfo);

    property Column: TdxGanttControlSheetColumn read FColumn;
    property ColumnViewInfo: TdxGanttControlSheetHeaderViewInfo
      read FColumnViewInfo;
  public
    constructor Create(AOwner: TdxGanttControlSheetDataRowViewInfo;
      AColumnViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
      AColumn: TdxGanttControlSheetColumn); reintroduce; virtual;

    function MeasureHeight(AWidth: Integer): Integer; virtual;

    property Owner: TdxGanttControlSheetDataRowViewInfo read GetOwner;
  end;

  { TdxGanttControlSheetCellViewInfo }

  TdxGanttControlSheetCellViewInfo = class
    (TdxGanttControlSheetCellCustomViewInfo)
  strict private
    function GetEditValue: Variant;
  protected
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    property EditValue: Variant read GetEditValue;
  end;

  TdxGanttControlSheetCellViewInfoClass = class of
    TdxGanttControlSheetCellViewInfo;

  { TdxGanttControlSheetEmptyCellViewInfo }

  TdxGanttControlSheetEmptyCellViewInfo = class
    (TdxGanttControlSheetCellCustomViewInfo);

  { TdxGanttControlSheetDataRowViewInfo }

  TdxGanttControlSheetDataRowViewInfo = class
    (TdxGanttControlCustomParentViewInfo)
  strict private
    FCells: TObjectList<TdxGanttControlSheetCellCustomViewInfo>;
    FData: TObject;
    FHeaderViewInfo: TdxGanttControlSheetRowHeaderViewInfo;
    FIndex: Integer;
    FLineBounds: TRect;
    function GetOwner: TdxGanttControlSheetCustomViewInfo; inline;
  protected
    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    procedure DoDraw; override;
    procedure DoScroll(const DX: Integer; const DY: Integer); override;
    function IsFocused: Boolean;
    function IsFullyVisible: Boolean;
    function IsOdd: Boolean;
    function IsSelected: Boolean;

    procedure UpdateIndex(const Value: Integer);

    property Cells: TObjectList<TdxGanttControlSheetCellCustomViewInfo>
      read FCells;
    property Index: Integer read FIndex;
  public
    constructor Create(AOwner: TdxGanttControlSheetCustomViewInfo;
      AIndex: Integer; AData: TObject); reintroduce;
    destructor Destroy; override;

    procedure Calculate(const R: TRect); override;
    procedure CalculateLayout; override;
    procedure ViewChanged; override;

    function MeasureHeight: Integer;

    property Data: TObject read FData;
    property HeaderViewInfo: TdxGanttControlSheetRowHeaderViewInfo
      read FHeaderViewInfo;
    property Owner: TdxGanttControlSheetCustomViewInfo read GetOwner;
  end;

  { TdxGanttControlSheetCellStringValueViewInfo }

  TdxGanttControlSheetCellStringValueViewInfo = class
    (TdxGanttControlSheetCellViewInfo)
  strict private
    FDisplayText: string;
    FTextBounds: TRect;
    FTextLayout: TcxCanvasBasedTextLayout;
  protected
    function CalculateBestFit: Integer; override;
    function CalculateTextBounds: TRect; virtual;
    function CalculateDisplayText: string;
    function DoCalculateDisplayText: string; virtual;
    procedure DoDraw; override;
    procedure DoScroll(const DX: Integer; const DY: Integer); override;
    function GetDrawTextFlags: Integer; virtual;
    function HasDisplayText: Boolean; virtual;
    function MultilineSupports: Boolean; virtual;
  public
    constructor Create(AOwner: TdxGanttControlSheetDataRowViewInfo;
      AColumnViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
      AColumn: TdxGanttControlSheetColumn); override;
    destructor Destroy; override;
    procedure Calculate(const R: TRect); override;
    procedure CalculateLayout; override;
    function MeasureHeight(AWidth: Integer): Integer; override;

    property TextBounds: TRect read FTextBounds;
  end;

  { TdxGanttControlSheetHeaderViewInfo }

  TdxGanttControlSheetHeaderViewInfo = class
    (TdxGanttControlCustomParentViewInfo)
  strict private
    function GetOwner: TdxGanttControlSheetCustomViewInfo; inline;
  protected
    FCaption: string;
    FTextBounds: TRect;
    FTextLayout: TcxCanvasBasedTextLayout;
    FSelectedRect: TRect;

    procedure DrawBackground(AViewInfo: TdxGanttControlCustomOwnedItemViewInfo;
      ABorders: TcxBorders; ANeighbors: TcxNeighbors);
    function IsHotState(AViewInfo
      : TdxGanttControlCustomOwnedItemViewInfo): Boolean;

    procedure DoDraw; override;
    procedure DoScroll(const DX: Integer; const DY: Integer); override;

    function CalculateSelectedRect: TRect; virtual;
    procedure CalculateTextBounds(const R: TRect); virtual;
    function GetBorders: TcxBorders; virtual;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function GetDrawTextFlags: Integer; virtual;
    function GetMinWidth: Integer; virtual;
    function GetNeighbors: TcxNeighbors; virtual;
    function GetTextColor: TColor; overload;
    function GetTextColor(AState: TcxButtonState): TColor; overload;
    function GetTextHorizontalAlignment: TcxTextAlignX; virtual;
    function GetTextVerticalAlignment: TcxTextAlignY; virtual;
    function HasHotTrackState: Boolean; override;
    function HasPressedState: Boolean; override;
    function IsMovingZone(const P: TPoint): Boolean; virtual;
    function IsSizingZone(const P: TPoint): Boolean; virtual;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(const R: TRect); override;

    property Owner: TdxGanttControlSheetCustomViewInfo read GetOwner;
  end;

  { TdxGanttControlSheetColumnHeaderViewInfo }

  TdxGanttControlSheetColumnHeaderViewInfo = class
    (TdxGanttControlSheetHeaderViewInfo)
  strict private
    FColumn: TdxGanttControlSheetColumn;
  protected
    FButtonBounds: TRect;
    function CalculateBestFit: Integer; virtual;
    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; override;
    function CalculateSelectedRect: TRect; override;
    procedure CalculateTextBounds(const R: TRect); override;
    function GetBorders: TcxBorders; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function GetHintText: string; override; final;
    function GetMinWidth: Integer; override;
    function GetNeighbors: TcxNeighbors; override;
    function GetTextHorizontalAlignment: TcxTextAlignX; override;
    function GetTextVerticalAlignment: TcxTextAlignY; override;
    function HasHint: Boolean; override;
    function HasHotTrackState: Boolean; override;
    function HasPressedState: Boolean; override;
    function IsMovingZone(const P: TPoint): Boolean; override;
    function IsPressed: Boolean; override;
    function IsSizingZone(const P: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TdxGanttControlSheetCustomViewInfo;
      AColumn: TdxGanttControlSheetColumn); reintroduce;
    procedure Calculate(const R: TRect); override;

    property Column: TdxGanttControlSheetColumn read FColumn;
  end;

  { TdxGanttControlSheetColumnHeaderImageViewInfo }

  TdxGanttControlSheetColumnHeaderImageViewInfo = class abstract
    (TdxGanttControlSheetColumnHeaderViewInfo)
  strict private
    FImage: TdxGPImage;
    FImageRect: TRect;
  protected
    function CalculateBestFit: Integer; override;
    function CalculateImage: TdxGPImage; virtual; abstract;
    function CalculateImageBounds: TRect; virtual;
    procedure CalculateTextBounds(const R: TRect); override;
    procedure DoDraw; override;
  public
    procedure Calculate(const R: TRect); override;
  end;

  { TdxGanttControlSheetColumnHeaderFilterButtonViewInfo }

  TdxGanttControlSheetColumnHeaderFilterButtonViewInfo = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    function GetOwner: TdxGanttControlSheetColumnHeaderViewInfo; inline;
  protected
    procedure DoDraw; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function HasHotTrackState: Boolean; override;
  public
    property Owner: TdxGanttControlSheetColumnHeaderViewInfo read GetOwner;
  end;

  { TdxGanttControlSheetColumnEmptyHeaderViewInfo }

  TdxGanttControlSheetColumnEmptyHeaderViewInfo = class
    (TdxGanttControlSheetHeaderViewInfo)
  protected
    function GetBorders: TcxBorders; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function GetNeighbors: TcxNeighbors; override;
    function HasHotTrackState: Boolean; override;
  end;

  { TdxGanttControlSheetRowHeaderViewInfo }

  TdxGanttControlSheetRowHeaderViewInfo = class
    (TdxGanttControlSheetHeaderViewInfo)
  strict private
    FDataRow: TdxGanttControlSheetDataRowViewInfo;
    function GetData: TObject; inline;
  protected
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function GetBorders: TcxBorders; override;
    function GetDrawTextFlags: Integer; override;
    function GetMinWidth: Integer; override;
    function GetNeighbors: TcxNeighbors; override;
  public
    constructor Create(AOwner: TdxGanttControlSheetCustomViewInfo;
      ADataRow: TdxGanttControlSheetDataRowViewInfo); reintroduce;

    property Data: TObject read GetData;
    property DataRow: TdxGanttControlSheetDataRowViewInfo read FDataRow;
  end;

  { TdxGanttControlSheetHeaderGripViewInfo }

  TdxGanttControlSheetHeaderGripViewInfo = class
    (TdxGanttControlSheetHeaderViewInfo)
  protected
    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; override;
    function GetBorders: TcxBorders; override;
    function GetMinWidth: Integer; override;
    function GetNeighbors: TcxNeighbors; override;
    function HasHotTrackState: Boolean; override;
  public
    procedure Calculate(const R: TRect); override;
  end;

  { TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo }

  TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    function GetOwner: TdxGanttControlSheetHeaderGripViewInfo; inline;
  protected
    procedure DoDraw; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
    function HasHotTrackState: Boolean; override;
    function HasPressedState: Boolean; override;
    function IsPressed: Boolean; override;
  public
    property Owner: TdxGanttControlSheetHeaderGripViewInfo read GetOwner;
  end;

  { TdxGanttControlSheetCustomViewInfo }

  TdxGanttControlSheetCustomViewInfo = class abstract
    (TdxGanttControlCustomParentViewInfo)
  strict private
    FCachedImageHeight: Integer;
    FCachedDataRowHeight: TDictionary<TObject, Integer>;
    FCachedSingleLineHeight: Integer;
    FClientRect: TRect;
    FDataRows: TObjectList<TdxGanttControlSheetDataRowViewInfo>;
    FHeaders: TObjectList<TdxGanttControlSheetHeaderViewInfo>;
    FFocusedCellViewInfo: TdxGanttControlSheetCellCustomViewInfo;
    FGridlines: TList<TRect>;
    FOptions: TdxGanttControlSheetOptions;
    FVisibleColumnCount: Integer;
    procedure AppendDataRow(ARowIndex: Integer);
    procedure AppendDataRows;
    procedure CalculateDataRows;
    function CalculateFocusedCellViewInfo
      : TdxGanttControlSheetCellCustomViewInfo;
    procedure CalculateGridlines;
    procedure CalculateHeaders;
    procedure CalculateClientRect;
    function GetController: TdxGanttControlSheetController; inline;
    function GetImageHeight: Integer;
    function GetFocusedCellViewInfo: TdxGanttControlSheetCellCustomViewInfo;
    function GetSingleLineHeight: Integer;
    function GetVisibleRowCount: Integer;
  protected
    FFirstVisibleColumnIndex: Integer;
    FFirstVisibleRowIndex: Integer;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    procedure Clear; override;
    procedure DoDraw; override;
    function GetColumnHeaderHeight: Integer; virtual;
    procedure ResetFocusedCellViewInfo;

    procedure Reset; override;

    function GetQuickCustomizationPopupOwnerBounds: TRect;

    property CachedDataRowHeight: TDictionary<TObject, Integer>
      read FCachedDataRowHeight;
    property Controller: TdxGanttControlSheetController read GetController;
    property DataRows: TObjectList<TdxGanttControlSheetDataRowViewInfo>
      read FDataRows;
    property FocusedCellViewInfo: TdxGanttControlSheetCellCustomViewInfo
      read GetFocusedCellViewInfo;
    property ImageHeight: Integer read GetImageHeight;
    property SingleLineHeight: Integer read GetSingleLineHeight;
    property Headers: TObjectList<TdxGanttControlSheetHeaderViewInfo>
      read FHeaders;
    property VisibleColumnCount: Integer read FVisibleColumnCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo;
      AOptions: TdxGanttControlSheetOptions); reintroduce;
    destructor Destroy; override;

    procedure Calculate(const R: TRect); override;
    procedure ViewChanged; override;

    property ClientRect: TRect read FClientRect;
    property ColumnHeaderHeight: Integer read GetColumnHeaderHeight;
    property Options: TdxGanttControlSheetOptions read FOptions;
  end;

  { TdxGanttControlSheetColumn }

  TdxGanttControlSheetColumn = class(TcxInterfacedPersistent)
  public const
    DefaultWidth = 100;
    MinWidth = 25;
  strict private
    FAllowHide: TdxDefaultBoolean;
    FAllowMove: TdxDefaultBoolean;
    FAllowSize: TdxDefaultBoolean;
    FCaption: string;
    FIsCaptionAssigned: Boolean;
    FProperties: TcxCustomEditProperties;
    FShowFilterButton: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    function IsCaptionStored: Boolean;
    function IsShowFilterButtonStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsWidthStored: Boolean;
    function GetCaption: string; inline;
    function GetCollection: TdxGanttControlSheetColumns; inline;
    function GetIndex: Integer;
    function GetScaleFactor: TdxScaleFactor; inline;
    procedure SetCaption(const Value: string);
    procedure SetIndex(const Value: Integer);
    procedure SetShowFilterButton(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure DoReset; virtual;
    procedure Changed; virtual;

    procedure WriteProperties(Writer: TWriter); virtual;

    function CanShowFilterButton: Boolean; virtual;
    function CreateProperties: TcxCustomEditProperties; virtual;
    function CreateViewInfo(ASheetViewInfo: TdxGanttControlSheetCustomViewInfo)
      : TdxGanttControlSheetColumnHeaderViewInfo; virtual;
    function IsEditable: Boolean;
    function GetDataCellViewInfoClass
      : TdxGanttControlSheetCellViewInfoClass; virtual;
    function GetDefaultCaption: string; virtual;
    function GetDefaultShowFilterButton: Boolean; virtual;
    function GetDefaultImageIndex: Integer; virtual;
    function GetDefaultVisible: Boolean; virtual;
    function GetDefaultWidth: Integer; virtual;
    class function GetDesignCaption: string; virtual;
    function GetHintText: string; virtual;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; virtual;
    procedure PrepareEditProperties(AProperties: TcxCustomEditProperties;
      AData: TObject); virtual;

    function GetEditValue(AData: TObject): Variant; virtual;
    procedure SetEditValue(AData: TObject; const Value: Variant); virtual;

    function RealAllowHide: Boolean;
    function RealAllowMove: Boolean;
    function RealAllowSize: Boolean;

    property Properties: TcxCustomEditProperties read FProperties;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ShowFilterButton: Boolean read FShowFilterButton
      write SetShowFilterButton stored IsShowFilterButtonStored;
  public
    constructor Create(AOwner: TdxGanttControlSheetColumns);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Reset;

    property Index: Integer read GetIndex write SetIndex;
    property Owner: TdxGanttControlSheetColumns read GetCollection;
  published
    property AllowHide: TdxDefaultBoolean read FAllowHide write FAllowHide
      default bDefault;
    property AllowMove: TdxDefaultBoolean read FAllowMove write FAllowMove
      default bDefault;
    property AllowSize: TdxDefaultBoolean read FAllowSize write FAllowSize
      default bDefault;
    property Caption: string read GetCaption write SetCaption
      stored IsCaptionStored;
    property Visible: Boolean read FVisible write SetVisible
      stored IsVisibleStored;
    property Width: Integer read FWidth write SetWidth stored IsWidthStored;
  end;

  { TdxGanttControlSheetColumns }

  TdxGanttControlSheetColumns = class(TcxInterfacedPersistent)
  strict private
    FList: TObjectList<TdxGanttControlSheetColumn>;
    FLockCount: Integer;
    FRegisteredColumnClasses: TList<TdxGanttControlSheetColumnClass>;

    procedure DataReaderHandler(Reader: TReader);
    procedure DataWriterHandler(Writer: TWriter);

    function GetCount: Integer;
    function GetItem(Index: Integer): TdxGanttControlSheetColumn;
    function GetScaleFactor: TdxScaleFactor; inline;
    function InternalGetOwner: TdxGanttControlSheetOptions;
    function GetVisibleCount: Integer;
    function GetVisibleItem(Index: Integer): TdxGanttControlSheetColumn;
    procedure SetItem(Index: Integer; const Value: TdxGanttControlSheetColumn);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Changed;
    procedure Clear;
    procedure DoReset;
    function GetVisibleIndex(AItem: TdxGanttControlSheetColumn): Integer;
    procedure Extract(AItem: TdxGanttControlSheetColumn);
    function IndexOf(AItem: TdxGanttControlSheetColumn): Integer;
    procedure Move(ACurrentIndex, ANewIndex: Integer);

    procedure RegisterColumnClass(AClass: TdxGanttControlSheetColumnClass);
    procedure RegisterColumnClasses; virtual;

    property RegisteredColumnClasses: TList<TdxGanttControlSheetColumnClass>
      read FRegisteredColumnClasses;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TdxGanttControlSheetColumn
      read GetVisibleItem;
  public
    constructor Create(AOwner: TdxGanttControlSheetOptions);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function Add(AClass: TdxGanttControlSheetColumnClass)
      : TdxGanttControlSheetColumn;
    procedure Delete(AIndex: Integer);
    function Insert(AIndex: Integer; AClass: TdxGanttControlSheetColumnClass)
      : TdxGanttControlSheetColumn;
    procedure Remove(AItem: TdxGanttControlSheetColumn);
    procedure Reset;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxGanttControlSheetColumn read GetItem
      write SetItem; default;
    property Owner: TdxGanttControlSheetOptions read InternalGetOwner;
  end;

  { TdxGanttControlSheetCustomDataProvider }

  TdxGanttControlSheetCustomDataProvider = class abstract
    (TdxGanttControlCustomDataProvider)
  protected
    procedure InternalAppendItem; virtual; abstract;
    procedure InternalInsertNewItem(AIndex: Integer); virtual; abstract;
    procedure InternalExtractLastItem; virtual; abstract;
    procedure InternalExtractItem(AIndex: Integer); virtual; abstract;
    procedure InternalInsertItem(AIndex: Integer; ADataItem: TObject);
      virtual; abstract;
    function InternalIndexOf(AItem: TObject): Integer; override;

    function GetDataItemIndex(ADataItem: TObject): Integer; virtual; abstract;

    function GetRowHeaderCaption(AData: TObject): string; virtual;
  end;

  { TdxGanttControlSheetEditingController }

  TdxGanttControlSheetEditingController = class(TcxCustomEditingController)
  // for internal use
  strict private
  type
    TActivateEditProc = reference to procedure;
  strict private
    FController: TdxGanttControlSheetController;
    FEditData: TcxCustomEditData;
    procedure AssignEditStyle;
    function GetFocusedCellViewInfo: TdxGanttControlSheetCellViewInfo;
  protected
    function CanInitEditing: Boolean; override;
    procedure ClearEditingItem; override;
    procedure DoHideEdit(Accept: Boolean); override;
    procedure DoUpdateEdit; override;
    function GetCancelEditingOnExit: Boolean; override;
    function GetEditParent: TWinControl; override;
    function GetHideEditOnExit: Boolean; override;
    function GetHideEditOnFocusedRecordChange: Boolean; override;
    function GetFocusedCellBounds: TRect; override;
    function GetIsEditing: Boolean; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure StartEditingByTimer; override;
    procedure UpdateInplaceParamsPosition; override;

    procedure EditAfterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); override;
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); override;
    procedure EditKeyPress(Sender: TObject; var Key: Char); override;
    procedure EditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); override;

    function PrepareEdit(AIsMouseEvent: Boolean): Boolean;
    procedure UpdateEditPosition;

    procedure DoShowEdit(AProc: TActivateEditProc);

    property FocusedCellViewInfo: TdxGanttControlSheetCellViewInfo
      read GetFocusedCellViewInfo;
    property Controller: TdxGanttControlSheetController read FController;
  public
    constructor Create(AController: TdxGanttControlSheetController);
      reintroduce;
    destructor Destroy; override;

    procedure ShowEdit; override;
    procedure ShowEditByKey(const AChar: Char);
    procedure ShowEditByMouse;
  end;

  { TdxGanttControlSheetResizingObject }

  TdxGanttControlSheetResizingObject = class abstract
    (TdxGanttControlResizingObject) // for internal use
  public const
    Width = 1;
  strict private
    FViewInfo: TdxGanttControlSheetHeaderViewInfo;
    function GetController: TdxGanttControlSheetController; inline;
    function GetHelper: TdxGanttControlSheetDragHelper; inline;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    function CanDrop(const P: TPoint): Boolean; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragImageHeight: Integer; override;
    function GetDragImageWidth: Integer; override;
    procedure SetWidth(const Value: Integer); virtual; abstract;
    property ViewInfo: TdxGanttControlSheetHeaderViewInfo read FViewInfo;
  public
    constructor Create(AController: TdxGanttControlCustomController;
      AViewInfo: TdxGanttControlSheetHeaderViewInfo); reintroduce;

    property Controller: TdxGanttControlSheetController read GetController;
    property Helper: TdxGanttControlSheetDragHelper read GetHelper;
  end;

  { TdxGanttControlSheetColumnResizingObject }

  TdxGanttControlSheetColumnResizingObject = class
    (TdxGanttControlSheetResizingObject) // for internal use
  strict private
    function GetViewInfo: TdxGanttControlSheetColumnHeaderViewInfo; inline;
  protected
    procedure SetWidth(const Value: Integer); override;
    property ViewInfo: TdxGanttControlSheetColumnHeaderViewInfo
      read GetViewInfo;
  end;

  { TdxGanttControlSheetRowHeaderWidthResizingObject }

  TdxGanttControlSheetRowHeaderWidthResizingObject = class
    (TdxGanttControlSheetResizingObject) // for internal use
  protected
    procedure SetWidth(const Value: Integer); override;
  end;

  { TdxGanttControlSheetMovingObject }

  TdxGanttControlSheetMovingObject = class(TdxGanttControlMovingObject)
  // for internal use
  strict private
    FColumn: TdxGanttControlSheetColumn;
    FFirstVisibleColumnIndex: Integer;
    FTopArrowLocation: TPoint;
    function GetController: TdxGanttControlSheetController; inline;
    function GetHitViewInfo(const P: TPoint)
      : TdxGanttControlSheetColumnHeaderViewInfo;
    function GetNewIndex(AHitViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
      const P: TPoint): Integer;
  protected
    procedure AfterDragAndDrop(Accepted: Boolean); override;
    procedure ApplyChanges(const P: TPoint); override;
    function CanDrop(const P: TPoint): Boolean; override;
    function CreateDragImage: TcxDragImage; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  public
    constructor Create(AController: TdxGanttControlCustomController;
      AColumn: TdxGanttControlSheetColumn); reintroduce;

    property Column: TdxGanttControlSheetColumn read FColumn;
    property Controller: TdxGanttControlSheetController read GetController;
  end;

  { TdxGanttControlSheetDragHelper }

  TdxGanttControlSheetDragHelper = class(TdxGanttControlDragHelper)
  // for internal use
  strict private
    FHitPoint: TPoint;
    function GetController: TdxGanttControlSheetController; inline;
    function CreateMovingObject(AViewInfo
      : TdxGanttControlSheetColumnHeaderViewInfo)
      : TdxGanttControlSheetMovingObject;
    function CreateColumnResizingObject(AViewInfo
      : TdxGanttControlSheetColumnHeaderViewInfo)
      : TdxGanttControlSheetColumnResizingObject;
    function CreateRowHeaderWidthResizingObject
      (AViewInfo: TdxGanttControlSheetHeaderViewInfo)
      : TdxGanttControlSheetRowHeaderWidthResizingObject;
  protected
    function CreateDragAndDropObject: TdxGanttControlDragAndDropObject;
      override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    function CalculateResizePoint(AViewInfo: TdxGanttControlSheetHeaderViewInfo;
      const P: TPoint): TPoint;
    function CreateDragAndDropObjectByPoint(const P: TPoint)
      : TdxGanttControlDragAndDropObject; virtual;
    procedure DoScroll; override;
    function GetScrollableArea: TRect; override;
  public
    property Controller: TdxGanttControlSheetController read GetController;
  end;

  { TdxGanttSheetScrollBars }

  TdxGanttSheetScrollBars = class(TdxGanttControlCustomScrollBars)
  // for internal use
  strict private
    function GetController: TdxGanttControlSheetController; inline;
  protected
    procedure DoHScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    procedure DoVScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    procedure DoInitHScrollBarParameters; override;
    procedure DoInitVScrollBarParameters; override;
    function IsUnlimitedScrolling(AScrollKind: TScrollBarKind;
      ADeltaX, ADeltaY: Integer): Boolean; override;
  public
    property Controller: TdxGanttControlSheetController read GetController;
  end;

  { TdxGanttSheetQuickCustomizationControl }

  TdxGanttSheetQuickCustomizationControl = class
    (TdxQuickCustomizationCustomControl) // for internal use
  strict private
    procedure CheckSortItems;

    procedure ShowAllClickHandler(Sender: TObject);
    procedure SortItemsClickHandler(Sender: TObject);

    function GetCheckingAllState: TcxCheckBoxState;
    function GetPopup: TdxGanttSheetColumnQuickCustomizationPopup; inline;
  protected
    procedure CheckShowAll;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PopulateCheckListBox; override;
    procedure PopulateCommandListBox; override;
  public
    property Popup: TdxGanttSheetColumnQuickCustomizationPopup read GetPopup;
  end;

  { TdxGanttSheetColumnQuickCustomizationPopup }

  TdxGanttSheetColumnQuickCustomizationPopup = class(TdxUIElementPopupWindow)
  // for internal use
  private
    FController: TdxGanttControlSheetController;
    FCustomizationControl: TdxGanttSheetQuickCustomizationControl;
    procedure ChangeColumnVisible(AColumn: TdxGanttControlSheetColumn;
      AValue: Boolean);
    procedure CheckListBoxActionHandler(Sender: TdxCustomListBox;
      AItemIndex: Integer);
    procedure CheckListBoxDragDropHandler(Sender, Source: TObject;
      X, Y: Integer);
    procedure CheckListBoxItemDragOverHandler(AItem: Pointer;
      var AAccept: Boolean);
    procedure CheckListSelectedItemCheckedStateChangedHandler(Sender: TObject);
  protected
    procedure InitPopup; override;
    procedure Paint; override;
  public
    constructor Create(AController: TdxGanttControlSheetController);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure CloseUp; override;

    property Controller: TdxGanttControlSheetController read FController;
    property CustomizationControl: TdxGanttSheetQuickCustomizationControl
      read FCustomizationControl;
  end;

  { TdxGanttControlSheetController }

  TdxGanttControlSheetController = class(TdxGanttControlCustomController,
    IdxUIElementPopupWindowOwner)
  strict private
    FColumnQuickCustomizationPopup: TdxGanttSheetColumnQuickCustomizationPopup;
    FEditingController: TdxGanttControlSheetEditingController;
    FFirstVisibleColumnIndex: Integer;
    FFirstVisibleRowIndex: Integer;
    FFocusedCell: TPoint;
    FIsColumnQuickCustomizationPopupVisible: Boolean;
    FOptions: TdxGanttControlSheetOptions;
    FScrollBars: TdxGanttSheetScrollBars;

    procedure CalculateBestFit(AColumnHeaderViewInfo
      : TdxGanttControlSheetColumnHeaderViewInfo);

    function GetColumnQuickCustomizationPopup
      : TdxGanttSheetColumnQuickCustomizationPopup; inline;
    function GetDataProvider: TdxGanttControlSheetCustomDataProvider; inline;
    function GetFocusedCellViewInfo: TdxGanttControlSheetCellCustomViewInfo;
    function GetFocusedColumnIndex: Integer;
    function GetFocusedDataItem: TObject;
    function GetFocusedRowIndex: Integer;
    function InternalGetViewInfo: TdxGanttControlSheetCustomViewInfo; inline;
    procedure SetFirstVisibleColumnIndex(const Value: Integer);
    procedure SetFirstVisibleRowIndex(const Value: Integer);
    procedure InternalSetFocusedCell(const Value: TPoint);
    procedure SetFocusedColumnIndex(const Value: Integer);
    procedure SetFocusedRowIndex(const Value: Integer);
  protected
    function CreateScrollBars: TdxGanttSheetScrollBars; virtual;
    procedure DeleteFocusedItem; virtual; abstract;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;
    function GetDesignHitTest(X: Integer; Y: Integer; Shift: TShiftState)
      : Boolean; override;
    function ProcessNCSizeChanged: Boolean; override;

    procedure InitScrollbars; override;
    procedure InsertNewDataItem; virtual;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    procedure UnInitScrollbars; override;

    procedure ResetFirstVisibleColumnIndex;

    function CanAutoScroll(ADirection: TcxDirection): Boolean; override;
    function CreateDragHelper: TdxGanttControlDragHelper; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;

    function CreateEditingController
      : TdxGanttControlSheetEditingController; virtual;

    function IsActive: Boolean;
    function IsEditing: Boolean;

    procedure DoClick; override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; override;

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyPress(var Key: Char); override;

    function CreateChangeCellValueCommand(const AValue: Variant)
      : TdxGanttControlSheetChangeCellValueCommand; virtual; abstract;

    procedure HideEditing; override;
    procedure MakeFocusedCellVisible;
    procedure SetFocusedCell(ACell: TdxGanttControlSheetCellViewInfo);
    procedure ValidateFocusedCell;

    procedure CollapseItem;
    procedure ExpandItem;
    function HasItemExpandState(AItem: TObject): Boolean; virtual;
    procedure ToggleExpandState(AItem: TObject);

    procedure ShowColumnQuickCustomizationPopup;

    function GetVisibleRowCount: Integer; virtual;

{$REGION 'IdxUIElementPopupWindowOwner'}
    function ClosePopupWhenSetNil: Boolean;
    procedure InitPopup(APopup: TdxUIElementPopupWindow);
    procedure PopupClosed;
{$ENDREGION 'IdxUIElementPopupWindowOwner'}
    property ColumnQuickCustomizationPopup
      : TdxGanttSheetColumnQuickCustomizationPopup
      read GetColumnQuickCustomizationPopup;
    property DataProvider: TdxGanttControlSheetCustomDataProvider
      read GetDataProvider;
    property EditingController: TdxGanttControlSheetEditingController
      read FEditingController;
    property FocusedCell: TPoint read FFocusedCell write InternalSetFocusedCell;
    property FocusedCellViewInfo: TdxGanttControlSheetCellCustomViewInfo
      read GetFocusedCellViewInfo;
    property FocusedDataItem: TObject read GetFocusedDataItem;
    property Options: TdxGanttControlSheetOptions read FOptions;
    property ScrollBars: TdxGanttSheetScrollBars read FScrollBars;
    property ViewInfo: TdxGanttControlSheetCustomViewInfo
      read InternalGetViewInfo;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AOptions: TdxGanttControlSheetOptions); reintroduce;
    destructor Destroy; override;

    property FirstVisibleColumnIndex: Integer read FFirstVisibleColumnIndex
      write SetFirstVisibleColumnIndex;
    property FirstVisibleRowIndex: Integer read FFirstVisibleRowIndex
      write SetFirstVisibleRowIndex;
    property FocusedColumnIndex: Integer read GetFocusedColumnIndex
      write SetFocusedColumnIndex;
    property FocusedRowIndex: Integer read GetFocusedRowIndex
      write SetFocusedRowIndex;
  end;

  { TdxGanttControlSheetOptions }

  TdxGanttControlSheetInitEditEvent = procedure(Sender: TObject;
    AColumn: TdxGanttControlSheetColumn; AEdit: TcxCustomEdit) of object;
  TdxGanttControlSheetEditingEvent = procedure(Sender: TObject;
    AColumn: TdxGanttControlSheetColumn; var AAllow: Boolean) of object;
  TdxGanttControlSheetColumnEvent = procedure(Sender: TObject;
    AColumn: TdxGanttControlSheetColumn) of object;

  TdxGanttControlSheetOptions = class abstract(TdxGanttControlCustomOptions)
  protected const
    DefaultRowHeaderWidth = 35;
    DefaultRowHeight = 21;
    DefaultFilterButtonWidth = 19;
    ColumnHeaderMinHeight = 36;
    RowHeaderMinWidth = 25;
  strict private
    FAlwaysShowEditor: TdxDefaultBoolean;
    FCellAutoHeight: Boolean;
    FAllowColumnHide: Boolean;
    FAllowColumnMove: Boolean;
    FAllowColumnSize: Boolean;
    FColumnQuickCustomization: Boolean;
    FColumnQuickCustomizationSorted: Boolean;
    FColumns: TdxGanttControlSheetColumns;
    FRowHeaderWidth: Integer;
    FRowHeight: Integer;

    FOnBeforeEdit: TdxGanttControlSheetEditingEvent;
    FOnEditValueChanged: TdxGanttControlSheetColumnEvent;
    FOnInitEdit: TdxGanttControlSheetInitEditEvent;

    FOnFirstVisibleColumnIndexChanged: TNotifyEvent;
    FOnFirstVisibleRowIndexChanged: TNotifyEvent;
    FOnFocusedColumnIndexChanged: TNotifyEvent;
    FOnFocusedRowIndexChanged: TNotifyEvent;

    FOnColumnPositionChanged: TdxGanttControlSheetColumnEvent;
    FOnColumnSizeChanged: TdxGanttControlSheetColumnEvent;

    function GetControl: TdxGanttControlBase; inline;
    function GetRealVisibleColumnCount: Integer; inline;
    procedure SetCellAutoHeight(const Value: Boolean);
    procedure SetColumnQuickCustomization(const Value: Boolean);
    procedure SetColumns(const Value: TdxGanttControlSheetColumns);
    procedure SetRowHeaderWidth(const Value: Integer);
    procedure SetRowHeight(const Value: Integer);
  protected
    procedure DoChanged(AChanges: TdxGanttControlOptionsChangedTypes); override;
    procedure DoReset; override;

    function CreateColumns: TdxGanttControlSheetColumns; virtual;
    function GetDataProvider: TdxGanttControlSheetCustomDataProvider;
      virtual; abstract;
    function GetOwnerComponent: TComponent; virtual; abstract;
    function GetController: TdxGanttControlSheetController; virtual; abstract;
    function GetRealAlwaysShowEditor: Boolean; virtual;

    function DoBeforeEdit(AColumn: TdxGanttControlSheetColumn)
      : Boolean; virtual;
    procedure DoEditValueChanged(AColumn: TdxGanttControlSheetColumn); virtual;
    procedure DoInitEdit(AColumn: TdxGanttControlSheetColumn;
      AEdit: TcxCustomEdit); virtual;
    procedure DoFirstVisibleColumnIndexChanged; virtual;
    procedure DoFirstVisibleRowIndexChanged; virtual;
    procedure DoFocusedColumnIndexChanged; virtual;
    procedure DoFocusedRowIndexChanged; virtual;

    procedure DoColumnPositionChanged(AColumn: TdxGanttControlSheetColumn);
    procedure DoColumnSizeChanged(AColumn: TdxGanttControlSheetColumn);

    property ColumnQuickCustomizationSorted: Boolean
      read FColumnQuickCustomizationSorted
      write FColumnQuickCustomizationSorted;
    property RealAlwaysShowEditor: Boolean read GetRealAlwaysShowEditor;
    property RealVisibleColumnCount: Integer read GetRealVisibleColumnCount;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Columns: TdxGanttControlSheetColumns read FColumns
      write SetColumns;
    property Control: TdxGanttControlBase read GetControl; // for internal use
    property Controller: TdxGanttControlSheetController read GetController;
    // for internal use
    property DataProvider: TdxGanttControlSheetCustomDataProvider
      read GetDataProvider; // for internal use
    property RowHeight: Integer read FRowHeight write SetRowHeight
      default DefaultRowHeight;
  published
    property AllowColumnHide: Boolean read FAllowColumnHide
      write FAllowColumnHide default False;
    property AllowColumnMove: Boolean read FAllowColumnMove
      write FAllowColumnMove default True;
    property AllowColumnSize: Boolean read FAllowColumnSize
      write FAllowColumnSize default True;
    property AlwaysShowEditor: TdxDefaultBoolean read FAlwaysShowEditor
      write FAlwaysShowEditor default bDefault;
    property CellAutoHeight: Boolean read FCellAutoHeight
      write SetCellAutoHeight default True;
    property ColumnQuickCustomization: Boolean read FColumnQuickCustomization
      write SetColumnQuickCustomization default False;
    property RowHeaderWidth: Integer read FRowHeaderWidth
      write SetRowHeaderWidth default DefaultRowHeaderWidth;

    property OnBeforeEdit: TdxGanttControlSheetEditingEvent read FOnBeforeEdit
      write FOnBeforeEdit;
    property OnEditValueChanged: TdxGanttControlSheetColumnEvent
      read FOnEditValueChanged write FOnEditValueChanged;
    property OnInitEdit: TdxGanttControlSheetInitEditEvent read FOnInitEdit
      write FOnInitEdit;
    property OnFirstVisibleColumnIndexChanged: TNotifyEvent
      read FOnFirstVisibleColumnIndexChanged
      write FOnFirstVisibleColumnIndexChanged;
    property OnFirstVisibleRowIndexChanged: TNotifyEvent
      read FOnFirstVisibleRowIndexChanged write FOnFirstVisibleRowIndexChanged;
    property OnFocusedColumnIndexChanged: TNotifyEvent
      read FOnFocusedColumnIndexChanged write FOnFocusedColumnIndexChanged;
    property OnFocusedRowIndexChanged: TNotifyEvent
      read FOnFocusedRowIndexChanged write FOnFocusedRowIndexChanged;
    property OnColumnPositionChanged: TdxGanttControlSheetColumnEvent
      read FOnColumnPositionChanged write FOnColumnPositionChanged;
    property OnColumnSizeChanged: TdxGanttControlSheetColumnEvent
      read FOnColumnSizeChanged write FOnColumnSizeChanged;
  end;

implementation

uses
  Math, TypInfo, RTLConsts,
  dxTypeHelpers, dxCoreGraphics, cxTextEdit, cxLibraryStrs,
  dxGanttControlUtils,
  dxGanttControlStrs,
  dxGanttControlCursors;

type
  TcxCustomDragImageAccess = class(TcxCustomDragImage);
  TWriterAccess = class(TWriter);
  TReaderAccess = class(TReader);
  TcxCustomLookAndFeelPainterAccess = class(TcxCustomLookAndFeelPainter);
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomEditStyleAccess = class(TcxCustomEditStyle);
  TdxGanttControlCustomOwnedItemViewInfoAccess = class
    (TdxGanttControlCustomOwnedItemViewInfo);
  TdxCustomCheckListBoxAccess = class(TdxCustomCheckListBox);
  TdxGanttControlCustomItemViewInfoAccess = class
    (TdxGanttControlCustomItemViewInfo);

  { TdxGanttControlSheetControllerHistoryItem }

constructor TdxGanttControlSheetControllerHistoryItem.Create
  (AController: TdxGanttControlSheetController);
begin
  inherited Create(AController.Control.History);
  FController := AController;
end;

{ TdxGanttControlSheetResizeHistoryItem }

procedure TdxGanttControlSheetResizeHistoryItem.DoRedo;
begin
  FOldWidth := FColumn.Width;
  FColumn.Width := FNewWidth;
end;

procedure TdxGanttControlSheetResizeHistoryItem.DoUndo;
begin
  FColumn.Width := FOldWidth;
end;

{ TdxGanttControlSheetChangeRowHeaderWidthHistoryItem }

procedure TdxGanttControlSheetChangeRowHeaderWidthHistoryItem.DoRedo;
begin
  FOldWidth := Controller.Options.RowHeaderWidth;
  Controller.Options.RowHeaderWidth := FNewWidth;
end;

procedure TdxGanttControlSheetChangeRowHeaderWidthHistoryItem.DoUndo;
begin
  Controller.Options.RowHeaderWidth := FOldWidth;
end;

{ TdxGanttControlSheetToggleItemExpandStateHistoryItem }

procedure TdxGanttControlSheetToggleItemExpandStateHistoryItem.DoRedo;
begin
  inherited DoRedo;
  if FIsExpanded then
    Controller.DataProvider.Collapse(FData)
  else
    Controller.DataProvider.Expand(FData);
end;

procedure TdxGanttControlSheetToggleItemExpandStateHistoryItem.DoUndo;
begin
  if not FIsExpanded then
    Controller.DataProvider.Collapse(FData)
  else
    Controller.DataProvider.Expand(FData);
  inherited DoUndo;
end;

{ TdxGanttControlSheetChangeValueHistoryItem }

procedure TdxGanttControlSheetChangeValueHistoryItem.DoRedo;
begin
  inherited DoRedo;
  FOldValue := GetEditValue;
  SetEditValue(FNewValue);
end;

procedure TdxGanttControlSheetChangeValueHistoryItem.DoUndo;
begin
  SetEditValue(FOldValue);
  inherited DoUndo;
end;

{ TdxGanttControlSheetChangeCellValueHistoryItem }

function TdxGanttControlSheetChangeCellValueHistoryItem.GetEditValue: Variant;
begin
  Result := FColumn.GetEditValue(FData);
end;

procedure TdxGanttControlSheetChangeCellValueHistoryItem.SetEditValue
  (const Value: Variant);
begin
  FColumn.SetEditValue(FData, Value);
end;

{ TdxGanttControlSheetFocusedCellChangeHistoryItem }

procedure TdxGanttControlSheetFocusedCellChangeHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Controller.FocusedCell := FFocusedCell;
  Controller.FirstVisibleColumnIndex := FFirstVisibleCell.X;
  Controller.FirstVisibleRowIndex := FFirstVisibleCell.Y;
end;

procedure TdxGanttControlSheetFocusedCellChangeHistoryItem.DoUndo;
begin
  Controller.FocusedCell := FFocusedCell;
  Controller.FirstVisibleColumnIndex := FFirstVisibleCell.X;
  Controller.FirstVisibleRowIndex := FFirstVisibleCell.Y;
  inherited DoUndo;
end;

{ TdxGanttControlCreateDataItemHistoryItem }

destructor TdxGanttControlCreateDataItemHistoryItem.Destroy;
begin
  FreeAndNil(FDataItem);
  inherited Destroy;
end;

{ TdxGanttControlAppendDataItemHistoryItem }

procedure TdxGanttControlAppendDataItemHistoryItem.DoRedo;
begin
  inherited DoRedo;
  if FDataItem = nil then
    Controller.DataProvider.InternalAppendItem
  else
    Controller.DataProvider.InternalInsertItem(FIndex, FDataItem);
  FDataItem := nil;
end;

procedure TdxGanttControlAppendDataItemHistoryItem.DoUndo;
begin
  FDataItem := Controller.DataProvider.LastDataItem;
  FIndex := Controller.DataProvider.DataItemCount - 1;
  Controller.DataProvider.InternalExtractLastItem;
  inherited DoUndo;
end;

{ TdxGanttControlInsertDataItemHistoryItem }

procedure TdxGanttControlInsertDataItemHistoryItem.DoRedo;
begin
  inherited DoRedo;
  if FDataItem = nil then
    Controller.DataProvider.InternalInsertNewItem(FIndex)
  else
    Controller.DataProvider.InternalInsertItem(FIndex, FDataItem);
  FDataItem := nil;
end;

procedure TdxGanttControlInsertDataItemHistoryItem.DoUndo;
begin
  inherited DoUndo;
  FDataItem := Controller.DataProvider.DataItems[FIndex];
  Controller.DataProvider.InternalExtractItem(FIndex);
end;

{ TdxGanttControlSheetMoveColumnHistoryItem }

procedure TdxGanttControlSheetMoveColumnHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Controller.Options.Columns.Move(FCurrentIndex, FNewIndex);
  Controller.FirstVisibleColumnIndex := FNewFirstVisibleColumnIndex;
end;

procedure TdxGanttControlSheetMoveColumnHistoryItem.DoUndo;
begin
  inherited DoUndo;
  Controller.Options.Columns.Move(FNewIndex, FCurrentIndex);
  Controller.FirstVisibleColumnIndex := FFirstVisibleColumnIndex;
end;

{ TdxGanttControlSheetHideColumnHistoryItem }

procedure TdxGanttControlSheetHideColumnHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Controller.Options.Columns[FColumnIndex].Visible := False;
  Controller.FirstVisibleColumnIndex := FNewFirstVisibleColumnIndex;
end;

procedure TdxGanttControlSheetHideColumnHistoryItem.DoUndo;
begin
  inherited DoUndo;
  Controller.Options.Columns[FColumnIndex].Visible := True;
  Controller.FirstVisibleColumnIndex := FFirstVisibleColumnIndex;
end;

{ TdxGanttControlSheetShowColumnHistoryItem }

procedure TdxGanttControlSheetShowColumnHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Controller.Options.Columns[FColumnIndex].Visible := True;
end;

procedure TdxGanttControlSheetShowColumnHistoryItem.DoUndo;
begin
  inherited DoUndo;
  Controller.Options.Columns[FColumnIndex].Visible := False;
end;

{ TdxGanttControlSheetCommand }

constructor TdxGanttControlSheetCommand.Create(AController
  : TdxGanttControlSheetController);
begin
  inherited Create(AController.Control);
  FController := AController;
end;

{ TdxGanttControlSheetResizeColumnCommand }

constructor TdxGanttControlSheetResizeColumnCommand.Create
  (AController: TdxGanttControlSheetController;
  AColumn: TdxGanttControlSheetColumn; ANewWidth: Integer);
begin
  inherited Create(AController);
  FColumn := AColumn;
  FNewWidth := ANewWidth;
end;

procedure TdxGanttControlSheetResizeColumnCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetResizeHistoryItem;
begin
  AHistoryItem := TdxGanttControlSheetResizeHistoryItem.Create(Controller);
  AHistoryItem.FColumn := FColumn;
  AHistoryItem.FNewWidth := FNewWidth;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlSheetChangeRowHeaderWidthCommand }

constructor TdxGanttControlSheetChangeRowHeaderWidthCommand.Create
  (AController: TdxGanttControlSheetController; ANewWidth: Integer);
begin
  inherited Create(AController);
  FNewWidth := ANewWidth;
end;

procedure TdxGanttControlSheetChangeRowHeaderWidthCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetChangeRowHeaderWidthHistoryItem;
begin
  AHistoryItem := TdxGanttControlSheetChangeRowHeaderWidthHistoryItem.Create
    (Controller);
  AHistoryItem.FNewWidth := FNewWidth;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlSheetChangeCellCommand }

constructor TdxGanttControlSheetChangeCellCommand.Create
  (AController: TdxGanttControlSheetController);
begin
  inherited Create(AController);
  FData := Controller.FocusedDataItem;
  if Controller.EditingController.FocusedCellViewInfo <> nil then
    FColumn := Controller.EditingController.FocusedCellViewInfo.Column;
end;

function TdxGanttControlSheetChangeCellCommand.GetController
  : TdxGanttControlSheetController;
begin
  Result := TdxGanttControlSheetController(inherited Controller);
end;

procedure TdxGanttControlSheetChangeCellCommand.SaveCellPosition;
var
  AHistoryItem: TdxGanttControlSheetFocusedCellChangeHistoryItem;
begin
  AHistoryItem := TdxGanttControlSheetFocusedCellChangeHistoryItem.Create
    (Controller);
  AHistoryItem.FFocusedCell := Controller.FocusedCell;
  AHistoryItem.FFirstVisibleCell :=
    TPoint.Create(Controller.FirstVisibleColumnIndex,
    Controller.FirstVisibleRowIndex);
  Control.History.AddItem(AHistoryItem);
end;

{ TdxGanttControlSheetDeleteFocusedItemCommand }

procedure TdxGanttControlSheetDeleteFocusedItemCommand.BeforeExecute;
begin
  SaveCellPosition;
  inherited BeforeExecute;
end;

function TdxGanttControlSheetDeleteFocusedItemCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Controller.FocusedDataItem <> nil);
end;

{ TdxGanttControlSheetInsertNewItemCommand }

procedure TdxGanttControlSheetInsertNewItemCommand.BeforeExecute;
begin
  SaveCellPosition;
  inherited BeforeExecute;
end;

procedure TdxGanttControlSheetInsertNewItemCommand.DoExecute;
var
  I: Integer;
  AHistoryItem: TdxGanttControlCreateDataItemHistoryItem;
begin
  inherited DoExecute;
  if Controller.DataProvider.Count <= Controller.FocusedRowIndex then
    for I := Controller.DataProvider.Count to Controller.FocusedRowIndex do
    begin
      AHistoryItem := TdxGanttControlAppendDataItemHistoryItem.Create
        (Controller);
      Control.History.AddItem(AHistoryItem);
      AHistoryItem.Execute;
    end
  else
  begin
    AHistoryItem := TdxGanttControlInsertDataItemHistoryItem.Create(Controller);
    AHistoryItem.FIndex := Controller.DataProvider.GetDataItemIndex
      (Controller.FocusedDataItem);
    Control.History.AddItem(AHistoryItem);
    AHistoryItem.Execute;
  end;
end;

{ TdxGanttControlSheetToggleItemExpandStateCommand }

procedure TdxGanttControlSheetToggleItemExpandStateCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetToggleItemExpandStateHistoryItem;
begin
  SaveCellPosition;
  AHistoryItem := TdxGanttControlSheetToggleItemExpandStateHistoryItem.Create
    (Controller);
  AHistoryItem.FData := FData;
  AHistoryItem.FIsExpanded := Controller.DataProvider.IsExpanded(FData);
  Controller.Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSheetToggleItemExpandStateCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Controller.HasItemExpandState(FData);
end;

{ TdxGanttControlSheetExpandItemCommand }

function TdxGanttControlSheetExpandItemCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and not Controller.DataProvider.IsExpanded(FData);
end;

{ TdxGanttControlSheetCollapseItemCommand }

function TdxGanttControlSheetCollapseItemCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Controller.DataProvider.IsExpanded(FData);
end;

{ TdxGanttControlSheetMoveColumnCommand }

constructor TdxGanttControlSheetMoveColumnCommand.Create
  (AController: TdxGanttControlSheetController;
  ACurrentIndex, ANewIndex: Integer; AFirstVisibleColumnIndex: Integer = -1);
begin
  inherited Create(AController);
  FCurrentIndex := ACurrentIndex;
  FNewIndex := ANewIndex;
  if AFirstVisibleColumnIndex = -1 then
    FFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex
  else
    FFirstVisibleColumnIndex := AFirstVisibleColumnIndex;
  FNewFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex;
end;

procedure TdxGanttControlSheetMoveColumnCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetMoveColumnHistoryItem;
begin
  inherited DoExecute;
  AHistoryItem := TdxGanttControlSheetMoveColumnHistoryItem.Create(Controller);
  AHistoryItem.FCurrentIndex := FCurrentIndex;
  AHistoryItem.FNewIndex := FNewIndex;
  AHistoryItem.FFirstVisibleColumnIndex := FFirstVisibleColumnIndex;
  AHistoryItem.FNewFirstVisibleColumnIndex := FNewFirstVisibleColumnIndex;
  Controller.Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSheetMoveColumnCommand.Enabled: Boolean;
begin
  Result := FCurrentIndex <> FNewIndex;
end;

{ TdxGanttControlSheetHideColumnCommand }

constructor TdxGanttControlSheetHideColumnCommand.Create
  (AController: TdxGanttControlSheetController; AColumnIndex: Integer;
  AFirstVisibleColumnIndex: Integer = -1);
begin
  inherited Create(AController);
  FColumnIndex := AColumnIndex;
  if AFirstVisibleColumnIndex = -1 then
    FFirstVisibleColumnIndex := AController.FirstVisibleColumnIndex
  else
    FFirstVisibleColumnIndex := AFirstVisibleColumnIndex;
  FNewFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex;
end;

procedure TdxGanttControlSheetHideColumnCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetHideColumnHistoryItem;
begin
  inherited DoExecute;
  AHistoryItem := TdxGanttControlSheetHideColumnHistoryItem.Create(Controller);
  AHistoryItem.FColumnIndex := FColumnIndex;
  AHistoryItem.FFirstVisibleColumnIndex := FFirstVisibleColumnIndex;
  AHistoryItem.FNewFirstVisibleColumnIndex := FNewFirstVisibleColumnIndex;
  Controller.Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSheetHideColumnCommand.Enabled: Boolean;
begin
  Result := Controller.Options.Columns[FColumnIndex].Visible;
end;

{ TdxGanttControlSheetShowColumnCommand }

constructor TdxGanttControlSheetShowColumnCommand.Create
  (AController: TdxGanttControlSheetController; AColumnIndex: Integer);
begin
  inherited Create(AController);
  FColumnIndex := AColumnIndex
end;

procedure TdxGanttControlSheetShowColumnCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlSheetShowColumnHistoryItem;
begin
  inherited DoExecute;
  AHistoryItem := TdxGanttControlSheetShowColumnHistoryItem.Create(Controller);
  AHistoryItem.FColumnIndex := FColumnIndex;
  Controller.Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSheetShowColumnCommand.Enabled: Boolean;
begin
  Result := not Controller.Options.Columns[FColumnIndex].Visible;
end;

{ TdxGanttControlSheetAddColumnCommand }

constructor TdxGanttControlSheetAddColumnCommand.Create
  (AController: TdxGanttControlSheetController;
  AColumnClass: TdxGanttControlSheetColumnClass);
begin
  inherited Create(AController);
  FColumnClass := AColumnClass;
end;

procedure TdxGanttControlSheetAddColumnCommand.DoExecute;

  function GetInvisibleColumn: TdxGanttControlSheetColumn;
  var
    I: Integer;
  begin
    for I := 0 to Controller.Options.Columns.Count - 1 do
      if not Controller.Options.Columns[I].Visible and
        (Controller.Options.Columns[I] is FColumnClass) then
        Exit(Controller.Options.Columns[I]);
    Result := nil;
  end;

var
  AColumn: TdxGanttControlSheetColumn;
begin
  inherited DoExecute;
  AColumn := GetInvisibleColumn;
  if AColumn = nil then
  begin
    AColumn := Controller.Options.Columns.Add(FColumnClass);
    AColumn.Visible := False;
  end
  else
  begin
  end;
  with TdxGanttControlSheetShowColumnCommand.Create(Controller,
    AColumn.Index) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlSheetAddColumnCommand.Enabled: Boolean;
begin
  Result := FColumnClass <> nil;
end;

{ TdxGanttControlSheetChangeCellValueCommand }

constructor TdxGanttControlSheetChangeCellValueCommand.Create
  (AController: TdxGanttControlSheetController; const ANewValue: Variant);
begin
  inherited Create(AController);
  FNewValue := ANewValue;
end;

procedure TdxGanttControlSheetChangeCellValueCommand.DoExecute;
begin
  if FData = nil then
    FData := CreateDataItem;
  SaveCellPosition;
  with CreateChangeValueCommand do
    try
      Execute;
    finally
      Free;
    end;
  SaveCellPosition;
end;

function TdxGanttControlSheetChangeCellValueCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and FColumn.IsEditable;
end;

function TdxGanttControlSheetChangeCellValueCommand.CreateDataItem: TObject;
var
  AHistoryItem: TdxGanttControlAppendDataItemHistoryItem;
  I: Integer;
begin
  for I := Controller.DataProvider.Count to Controller.FocusedRowIndex do
  begin
    AHistoryItem := TdxGanttControlAppendDataItemHistoryItem.Create(Controller);
    Control.History.AddItem(AHistoryItem);
    AHistoryItem.Execute;
  end;
  Result := Controller.DataProvider.LastDataItem;
end;

{ TdxGanttControlSheetCellCustomViewInfo }

constructor TdxGanttControlSheetCellCustomViewInfo.Create
  (AOwner: TdxGanttControlSheetDataRowViewInfo;
  AColumnViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
  AColumn: TdxGanttControlSheetColumn);
begin
  inherited Create(AOwner);
  FColumnViewInfo := AColumnViewInfo;
  FColumn := AColumn;
end;

procedure TdxGanttControlSheetCellCustomViewInfo.DoDraw;
var
  R: TRect;
begin
  inherited DoDraw;
  if IsFocused then
  begin
    R := Bounds;
    if UseRightToLeftAlignment then
      Inc(R.Left)
    else
      Dec(R.Right);
    Dec(R.Bottom);
    Canvas.FrameRect(R, LookAndFeelPainter.DefaultSelectionColor,
      ScaleFactor.Apply(1));
  end;
end;

function TdxGanttControlSheetCellCustomViewInfo.GetFont: TFont;
begin
  Result := CanvasCache.GetBaseFont;
end;

procedure TdxGanttControlSheetCellCustomViewInfo.UpdateColumnViewInfo
  (AColumnViewInfo: TdxGanttControlSheetHeaderViewInfo);
begin
  FColumnViewInfo := AColumnViewInfo;
end;

function TdxGanttControlSheetCellCustomViewInfo.GetOwner
  : TdxGanttControlSheetDataRowViewInfo;
begin
  Result := TdxGanttControlSheetDataRowViewInfo(inherited Owner);
end;

function TdxGanttControlSheetCellCustomViewInfo.CalculateBestFit: Integer;
begin
  Result := 0;
end;

function TdxGanttControlSheetCellCustomViewInfo.IsFocused: Boolean;
begin
  Result := Owner.IsFocused and
    (Owner.Cells.IndexOf(Self) = Owner.Owner.Controller.FocusedCell.X -
    Owner.Owner.FFirstVisibleColumnIndex);
end;

function TdxGanttControlSheetCellCustomViewInfo.MeasureHeight
  (AWidth: Integer): Integer;
begin
  Result := 0;
end;

{ TdxGanttControlSheetCellViewInfo }

function TdxGanttControlSheetCellViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  if IsFocused and Column.IsEditable and not Owner.Owner.Controller.Control.IsDesigning
  then
    Result := crIBeam
  else
    Result := ADefaultCursor;
end;

function TdxGanttControlSheetCellViewInfo.GetEditValue: Variant;
begin
  if Owner.Data = nil then
    Result := Null
  else
    Result := Column.GetEditValue(Owner.Data);
end;

{ TdxGanttControlSheetDataRowViewInfo }

constructor TdxGanttControlSheetDataRowViewInfo.Create
  (AOwner: TdxGanttControlSheetCustomViewInfo; AIndex: Integer; AData: TObject);
begin
  inherited Create(AOwner);
  FCells := TObjectList<TdxGanttControlSheetCellCustomViewInfo>.Create;
  FData := AData;
  FIndex := AIndex;
  FHeaderViewInfo := TdxGanttControlSheetRowHeaderViewInfo.Create(Owner, Self);
end;

destructor TdxGanttControlSheetDataRowViewInfo.Destroy;
begin
  FreeAndNil(FHeaderViewInfo);
  FreeAndNil(FCells);
  inherited Destroy;
end;

function TdxGanttControlSheetDataRowViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    if FHeaderViewInfo.CalculateHitTest(AHitTest) then
      Exit(True);
    for I := 0 to FCells.Count - 1 do
      if FCells[I].CalculateHitTest(AHitTest) then
        Exit(True);
  end;
end;

procedure TdxGanttControlSheetDataRowViewInfo.DoDraw;

  function GetBackgroundColor: TColor;
  begin
    if IsOdd then
      Result := LookAndFeelPainter.GridLikeControlContentOddColor
    else
      Result := LookAndFeelPainter.GridLikeControlContentEvenColor;
  end;

var
  I: Integer;
begin
  if IsFocused then
    LookAndFeelPainter.DrawGanttFocusedRow(Canvas, cxRectInflate(Bounds, 2, 2),
      not IsSelected)
  else
    Canvas.FillRect(Bounds, GetBackgroundColor);
  inherited DoDraw;
  FHeaderViewInfo.Draw;
  for I := 0 to Cells.Count - 1 do
    Cells[I].Draw;
  I := Index + 1;
  Canvas.FillRect(FLineBounds,
    IfThen(IsFocused or ((I < Owner.DataRows.Count) and
    Owner.DataRows[I].IsFocused), LookAndFeelPainter.DefaultSelectionColor,
    LookAndFeelPainter.DefaultGridlineColor));
end;

procedure TdxGanttControlSheetDataRowViewInfo.DoScroll(const DX, DY: Integer);
var
  I: Integer;
begin
  inherited DoScroll(DX, DY);
  HeaderViewInfo.DoScroll(0, DY);
  FLineBounds.Offset(0, DY);
  for I := 0 to Cells.Count - 1 do
    Cells[I].DoScroll(DX, DY);
end;

function TdxGanttControlSheetDataRowViewInfo.GetOwner
  : TdxGanttControlSheetCustomViewInfo;
begin
  Result := TdxGanttControlSheetCustomViewInfo(inherited Owner);
end;

function TdxGanttControlSheetDataRowViewInfo.IsFocused: Boolean;
begin
  Result := Index = Owner.Controller.FocusedCell.Y -
    Owner.FFirstVisibleRowIndex;
end;

function TdxGanttControlSheetDataRowViewInfo.IsFullyVisible: Boolean;
begin
  Result := Bounds.Bottom <= Owner.ClientRect.Bottom;
end;

function TdxGanttControlSheetDataRowViewInfo.IsOdd: Boolean;
begin
  Result := (Owner.Controller.FirstVisibleRowIndex + Index) mod 2 = 1;
end;

function TdxGanttControlSheetDataRowViewInfo.IsSelected: Boolean;
begin
  Result := IsFocused and (Owner.Controller.FocusedCell.X = -1);
end;

function TdxGanttControlSheetDataRowViewInfo.MeasureHeight: Integer;
var
  I: Integer;
  ACellViewInfo: TdxGanttControlSheetCellCustomViewInfo;
  AResult: Integer;
begin
  Result := dxGetTouchableSize(MulDiv(Owner.Options.RowHeight,
    Abs(CanvasCache.GetBaseFont.Height), 11), ScaleFactor);
  if (Data <> nil) and Owner.Options.CellAutoHeight then
  begin
    if not Owner.CachedDataRowHeight.TryGetValue(Data, AResult) then
    begin
      for I := 0 to Owner.Options.Columns.Count - 1 do
        if Owner.Options.Columns[I].Visible then
        begin
          ACellViewInfo := Owner.Options.Columns[I].GetDataCellViewInfoClass.
            Create(Self, nil, Owner.Options.Columns[I]);
          try
            ACellViewInfo.CalculateLayout;
            Result := Max(Result,
              ACellViewInfo.MeasureHeight
              (ScaleFactor.Apply(Owner.Options.Columns[I].Width)));
          finally
            ACellViewInfo.Free;
          end;
        end;
      Owner.CachedDataRowHeight.Add(Data, Result);
    end
    else
      Result := AResult;
  end;
end;

procedure TdxGanttControlSheetDataRowViewInfo.UpdateIndex(const Value: Integer);
begin
  FIndex := Value;
end;

procedure TdxGanttControlSheetDataRowViewInfo.ViewChanged;
var
  I: Integer;
  AColumnViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
  AIndex: Integer;
begin
  inherited ViewChanged;
  for I := 1 to Owner.Headers.Count - 1 do
    if Owner.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo then
    begin
      AIndex := I - 1;
      AColumnViewInfo := TdxGanttControlSheetColumnHeaderViewInfo
        (Owner.Headers[I]);
      if (FCells.Count > AIndex) and
        (FCells[AIndex].Column.Index < AColumnViewInfo.Column.Index) then
      begin
        while (FCells.Count > AIndex) and
          (FCells[AIndex].Column <> AColumnViewInfo.Column) do
          FCells.Delete(AIndex);
      end;
      if FCells.Count <= AIndex then
      begin
        FCells.Add(AColumnViewInfo.Column.GetDataCellViewInfoClass.Create(Self,
          AColumnViewInfo, AColumnViewInfo.Column));
        FCells.Last.CalculateLayout;
      end;
      if FCells[AIndex].Column.Index > AColumnViewInfo.Column.Index then
      begin
        FCells.Insert(AIndex, AColumnViewInfo.Column.GetDataCellViewInfoClass.
          Create(Self, AColumnViewInfo, AColumnViewInfo.Column));
        FCells[AIndex].CalculateLayout;
      end;
      FCells[AIndex].UpdateColumnViewInfo(AColumnViewInfo);
    end;
  while FCells.Count > Owner.Headers.Count - 1 do
    FCells.Delete(FCells.Count - 1);
  Recalculate;
end;

procedure TdxGanttControlSheetDataRowViewInfo.CalculateLayout;
var
  I: Integer;
  AColumnHeaderViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
begin
  inherited CalculateLayout;
  FCells.Clear;
  for I := 1 to Owner.Headers.Count - 1 do
  begin
    if Owner.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo then
    begin
      AColumnHeaderViewInfo := TdxGanttControlSheetColumnHeaderViewInfo
        (Owner.Headers[I]);
      FCells.Add(AColumnHeaderViewInfo.Column.GetDataCellViewInfoClass.Create
        (Self, AColumnHeaderViewInfo, AColumnHeaderViewInfo.Column));
      FCells.Last.CalculateLayout;
    end;
  end;
end;

procedure TdxGanttControlSheetDataRowViewInfo.Calculate(const R: TRect);
var
  AHeaderBounds: TRect;
  ACellBounds: TRect;
  I: Integer;
begin
  AHeaderBounds := R;
  AHeaderBounds.Left := Owner.Headers[0].Bounds.Left;
  AHeaderBounds.Right := Owner.Headers[0].Bounds.Right;
  for I := 0 to FCells.Count - 1 do
  begin
    ACellBounds := TRect.Create(FCells[I].ColumnViewInfo.Bounds.Left,
      AHeaderBounds.Top, FCells[I].ColumnViewInfo.Bounds.Right,
      AHeaderBounds.Bottom);
    FCells[I].Calculate(ACellBounds);
  end;
  FHeaderViewInfo.Calculate(AHeaderBounds);
  inherited Calculate(R);
  if UseRightToLeftAlignment then
    FLineBounds := TRect.Create(Bounds.Left, AHeaderBounds.Bottom - 1,
      AHeaderBounds.Left, AHeaderBounds.Bottom)
  else
    FLineBounds := TRect.Create(AHeaderBounds.Right, AHeaderBounds.Bottom - 1,
      Bounds.Right, AHeaderBounds.Bottom);
end;

{ TdxGanttControlSheetCellStringValueViewInfo }

constructor TdxGanttControlSheetCellStringValueViewInfo.Create
  (AOwner: TdxGanttControlSheetDataRowViewInfo;
  AColumnViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
  AColumn: TdxGanttControlSheetColumn);
begin
  inherited Create(AOwner, AColumnViewInfo, AColumn);
  FTextLayout := Canvas.CreateTextLayout;
end;

destructor TdxGanttControlSheetCellStringValueViewInfo.Destroy;
begin
  FreeAndNil(FTextLayout);
  inherited Destroy;
end;

procedure TdxGanttControlSheetCellStringValueViewInfo.Calculate(const R: TRect);
begin
  inherited Calculate(R);
  FTextBounds := CalculateTextBounds;
  if not Bounds.IsEmpty and (FDisplayText <> '') then
  begin
    FTextLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
    FTextLayout.SetFlags(GetDrawTextFlags);
    FTextLayout.SetFont(CanvasCache.GetFont(GetFont));
    FTextLayout.SetText(FDisplayText);
    FTextLayout.SetLayoutConstraints(FTextBounds.Width, FTextBounds.Height, 0);
    FTextLayout.MeasureSize;
  end;
end;

procedure TdxGanttControlSheetCellStringValueViewInfo.CalculateLayout;
begin
  inherited CalculateLayout;
  FDisplayText := CalculateDisplayText;
end;

function TdxGanttControlSheetCellStringValueViewInfo.HasDisplayText: Boolean;
begin
  Result := Owner.Data <> nil;
end;

function TdxGanttControlSheetCellStringValueViewInfo.CalculateTextBounds: TRect;
begin
  Result := Bounds;
  Result.Inflate(-ScaleFactor.Apply(cxTextOffset),
    -ScaleFactor.Apply(cxTextOffset));
end;

function TdxGanttControlSheetCellStringValueViewInfo.CalculateBestFit: Integer;
begin
  Result := TdxGanttControlUtils.MeasureTextWidth(FTextLayout, FDisplayText,
    CanvasCache.GetFont(GetFont));
  Result := Result + ScaleFactor.Apply(cxTextOffset * 2);
end;

function TdxGanttControlSheetCellStringValueViewInfo.
  CalculateDisplayText: string;
begin
  if HasDisplayText then
    Result := DoCalculateDisplayText
  else
    Result := '';
end;

function TdxGanttControlSheetCellStringValueViewInfo.
  DoCalculateDisplayText: string;
var
  AProperties: TcxCustomEditProperties;
  ADisplayValue: Variant;
begin
  AProperties := Column.Properties;
  AProperties.PrepareDisplayValue(Column.GetEditValue(Owner.Data),
    ADisplayValue, False);
  Result := VarToStr(ADisplayValue);
end;

procedure TdxGanttControlSheetCellStringValueViewInfo.DoDraw;
begin
  inherited DoDraw;
  if not Bounds.IsEmpty and (FDisplayText <> '') then
    FTextLayout.Draw(FTextBounds);
end;

procedure TdxGanttControlSheetCellStringValueViewInfo.DoScroll(const DX,
  DY: Integer);
begin
  inherited DoScroll(DX, DY);
  FTextBounds.Offset(DX, DY);
end;

function TdxGanttControlSheetCellStringValueViewInfo.GetDrawTextFlags: Integer;
const
  ASingleLineFlagsMap: array [Boolean] of Integer = (CXTO_WORDBREAK,
    CXTO_SINGLELINE);
  AAlignmentFlagsMap: array [Boolean] of Integer = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := ASingleLineFlagsMap[not(Column.Owner.Owner.CellAutoHeight and
    MultilineSupports)] or CXTO_END_ELLIPSIS or AAlignmentFlagsMap
    [UseRightToLeftAlignment];
end;

function TdxGanttControlSheetCellStringValueViewInfo.MeasureHeight
  (AWidth: Integer): Integer;
begin
  if Trim(FDisplayText) = '' then
    Result := 0
  else
  begin
    Result := TdxGanttControlUtils.MeasureTextHeight(FTextLayout,
      AWidth - ScaleFactor.Apply(cxTextOffset) * 2, FDisplayText,
      CanvasCache.GetFont(GetFont), not(Column.Owner.Owner.CellAutoHeight and
      MultilineSupports));
  end;
  Result := Result + ScaleFactor.Apply(cxTextOffset) * 2;
end;

function TdxGanttControlSheetCellStringValueViewInfo.MultilineSupports: Boolean;
begin
  Result := False;
end;

{ TdxGanttControlSheetHeaderViewInfo }

constructor TdxGanttControlSheetHeaderViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FTextLayout := Canvas.CreateTextLayout;
end;

destructor TdxGanttControlSheetHeaderViewInfo.Destroy;
begin
  FreeAndNil(FTextLayout);
  inherited Destroy;
end;

procedure TdxGanttControlSheetHeaderViewInfo.Calculate(const R: TRect);
begin
  inherited Calculate(R);
  FSelectedRect := CalculateSelectedRect;
  if FCaption = '' then
    FTextBounds := cxNullRect
  else
    CalculateTextBounds(R);
end;

procedure TdxGanttControlSheetHeaderViewInfo.DoDraw;
begin
  UpdateState;
  DrawBackground(Self, GetBorders, GetNeighbors);
  if not FTextBounds.IsEmpty and (FCaption <> '') then
    FTextLayout.Draw(FTextBounds);
  inherited DoDraw;
end;

procedure TdxGanttControlSheetHeaderViewInfo.DoScroll(const DX, DY: Integer);
begin
  inherited DoScroll(DX, DY);
  FTextBounds.Offset(DX, DY);
  FSelectedRect.Offset(DX, DY);
end;

procedure TdxGanttControlSheetHeaderViewInfo.DrawBackground
  (AViewInfo: TdxGanttControlCustomOwnedItemViewInfo; ABorders: TcxBorders;
  ANeighbors: TcxNeighbors);
begin
  LookAndFeelPainter.DrawGanttSheetHeader(Canvas, AViewInfo.Bounds,
    AViewInfo.State, ANeighbors, ABorders, ScaleFactor);
end;

function TdxGanttControlSheetHeaderViewInfo.CalculateSelectedRect: TRect;
begin
  Result := cxNullRect;
end;

procedure TdxGanttControlSheetHeaderViewInfo.CalculateTextBounds
  (const R: TRect);
var
  ATextOffset: Integer;
begin
  FTextBounds := R;
  ATextOffset := ScaleFactor.Apply(cxTextOffset) +
    LookAndFeelPainter.BorderSize;
  FTextBounds.Inflate(-ATextOffset, -ATextOffset);
  if FCaption <> '' then
  begin
    FTextLayout.SetFlags(GetDrawTextFlags);
    FTextLayout.SetFont(CanvasCache.GetControlFont);
    FTextLayout.SetColor(GetTextColor);
    FTextLayout.SetText(FCaption);
    FTextLayout.SetLayoutConstraints(FTextBounds.Width, FTextBounds.Height);
    FTextLayout.MeasureSize;
  end;
end;

function TdxGanttControlSheetHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := [];
end;

function TdxGanttControlSheetHeaderViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  if IsSizingZone(P) then
    Result := TdxGanttControlCursors.ColumnResize
  else
    Result := inherited GetCurrentCursor(P, ADefaultCursor);
end;

function TdxGanttControlSheetHeaderViewInfo.GetMinWidth: Integer;
begin
  Result := 0;
end;

function TdxGanttControlSheetHeaderViewInfo.GetNeighbors: TcxNeighbors;
begin
  Result := [];
end;

function TdxGanttControlSheetHeaderViewInfo.GetOwner
  : TdxGanttControlSheetCustomViewInfo;
begin
  Result := TdxGanttControlSheetCustomViewInfo(inherited Owner);
end;

function TdxGanttControlSheetHeaderViewInfo.GetDrawTextFlags: Integer;
begin
  Result := CXTO_WORDBREAK or CXTO_END_ELLIPSIS or
    cxMakeFormat(GetTextHorizontalAlignment, GetTextVerticalAlignment);
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RTLREADING;
end;

function TdxGanttControlSheetHeaderViewInfo.GetTextColor: TColor;
begin
  Result := GetTextColor(State);
end;

function TdxGanttControlSheetHeaderViewInfo.GetTextColor
  (AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot, cxbsPressed:
      Result := LookAndFeelPainter.ButtonSymbolColor(AState);
  else
    Result := LookAndFeelPainter.DefaultHeaderTextColor;
  end;
end;

function TdxGanttControlSheetHeaderViewInfo.GetTextHorizontalAlignment
  : TcxTextAlignX;
begin
  Result := taCenterX;
end;

function TdxGanttControlSheetHeaderViewInfo.GetTextVerticalAlignment
  : TcxTextAlignY;
begin
  Result := taCenterY;
end;

function TdxGanttControlSheetHeaderViewInfo.HasHotTrackState: Boolean;
begin
  Result := not Owner.Controller.Control.IsDesigning;
end;

function TdxGanttControlSheetHeaderViewInfo.HasPressedState: Boolean;
begin
  Result := not Owner.Controller.Control.IsDesigning;
end;

function TdxGanttControlSheetHeaderViewInfo.IsHotState
  (AViewInfo: TdxGanttControlCustomOwnedItemViewInfo): Boolean;
begin
  Result := not Owner.Controller.DragHelper.IsDragging;
  if Result then
  begin
    Result := TdxGanttControlCustomOwnedItemViewInfoAccess(AViewInfo)
      .HasHotTrackState and (AViewInfo.State = cxbsHot);
    Result := Result or TdxGanttControlCustomOwnedItemViewInfoAccess(AViewInfo)
      .HasPressedState and (AViewInfo.State = cxbsPressed);
  end;
end;

function TdxGanttControlSheetHeaderViewInfo.IsMovingZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlSheetHeaderViewInfo.IsSizingZone
  (const P: TPoint): Boolean;
var
  X: Integer;
begin
  if UseRightToLeftAlignment then
    X := Bounds.Left
  else
    X := Bounds.Right;
  Result := (P.X >= X - GetResizeHitZoneWidth div 2) and
    (P.X <= X + GetResizeHitZoneWidth div 2);
end;

{ TdxGanttControlSheetRowHeaderViewInfo }

constructor TdxGanttControlSheetRowHeaderViewInfo.Create
  (AOwner: TdxGanttControlSheetCustomViewInfo;
  ADataRow: TdxGanttControlSheetDataRowViewInfo);
begin
  inherited Create(AOwner);
  FDataRow := ADataRow;
  FCaption := Owner.Options.DataProvider.GetRowHeaderCaption(Data);
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := [bBottom, bLeft, bRight];
  if UseRightToLeftAlignment then
    Exclude(Result, bRight)
  else
    Exclude(Result, bLeft);
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  if IsSizingZone(P) then
    Result := TdxGanttControlCursors.ColumnResize
  else if not Owner.Controller.Control.IsDesigning then
  begin
    if UseRightToLeftAlignment then
      Result := TdxGanttControlCursors.ArrowRight
    else
      Result := TdxGanttControlCursors.ArrowLeft;
  end
  else
    Result := inherited GetCurrentCursor(P, ADefaultCursor);
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetNeighbors: TcxNeighbors;
begin
  Result := [nTop, nBottom];
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetData: TObject;
begin
  Result := DataRow.Data;
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetDrawTextFlags: Integer;
begin
  Result := inherited GetDrawTextFlags and not CXTO_END_ELLIPSIS;
end;

function TdxGanttControlSheetRowHeaderViewInfo.GetMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(TdxGanttControlSheetOptions.RowHeaderMinWidth);
end;

{ TdxGanttControlSheetHeaderGripViewInfo }

procedure TdxGanttControlSheetHeaderGripViewInfo.Calculate(const R: TRect);
begin
  Clear;
  if Owner.Options.ColumnQuickCustomization then
    AddChild(TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.
      Create(Self));
  inherited Calculate(R);
end;

function TdxGanttControlSheetHeaderGripViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
var
  AOffset: Integer;
begin
  Result := Bounds;
  AOffset := LookAndFeelPainter.BorderSize;
  Result.Height := Result.Height - AOffset;
  Result.Width := Result.Width - AOffset;
  if UseRightToLeftAlignment then
    Result.MoveToRight(Bounds.Right);
end;

function TdxGanttControlSheetHeaderGripViewInfo.GetBorders: TcxBorders;
begin
  Result := [bBottom, bLeft, bRight];
  if Owner.UseRightToLeftAlignment then
    Exclude(Result, bRight)
  else
    Exclude(Result, bLeft);
end;

function TdxGanttControlSheetHeaderGripViewInfo.GetMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(TdxGanttControlSheetOptions.RowHeaderMinWidth);
end;

function TdxGanttControlSheetHeaderGripViewInfo.GetNeighbors: TcxNeighbors;
begin
  Result := [nBottom, nLeft, nRight];
  if Owner.UseRightToLeftAlignment then
    Exclude(Result, nRight)
  else
    Exclude(Result, nLeft)
end;

function TdxGanttControlSheetHeaderGripViewInfo.HasHotTrackState: Boolean;
begin
  Result := False;
end;

{ TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo }

procedure TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.DoDraw;
const
  ASize = 12;
var
  R: TRect;
begin
  UpdateState;
  if Owner.IsHotState(Self) or cxIsTouchModeEnabled then
    Owner.DrawBackground(Self, [], []);
  R := cxRectCenter(Bounds, ScaleFactor.Apply(TSize.Create(ASize, ASize)));
  R.MoveToBottom(Bounds.Bottom - ScaleFactor.Apply(cxTextOffset));
  LookAndFeelPainter.DrawScaledIndicatorCustomizationMark(Canvas, R,
    Owner.GetTextColor(State), ScaleFactor);
end;

function TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.
  GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor): TCursor;
begin
  if Owner.IsSizingZone(P) then
    Result := Owner.GetCurrentCursor(P, ADefaultCursor)
  else
    Result := ADefaultCursor;
end;

function TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.GetOwner
  : TdxGanttControlSheetHeaderGripViewInfo;
begin
  Result := TdxGanttControlSheetHeaderGripViewInfo(inherited Owner);
end;

function TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.
  HasHotTrackState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.
  HasPressedState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo.
  IsPressed: Boolean;
begin
  Result := Owner.Owner.Controller.ColumnQuickCustomizationPopup.Visible;
end;

{ TdxGanttControlSheetColumnHeaderViewInfo }

constructor TdxGanttControlSheetColumnHeaderViewInfo.Create
  (AOwner: TdxGanttControlSheetCustomViewInfo;
  AColumn: TdxGanttControlSheetColumn);
begin
  inherited Create(AOwner);
  FColumn := AColumn;
  FCaption := AColumn.Caption;
end;

procedure TdxGanttControlSheetColumnHeaderViewInfo.CalculateTextBounds
  (const R: TRect);
var
  ARect: TRect;
begin
  ARect := R;
  if FColumn.ShowFilterButton then
    if UseRightToLeftAlignment then
      ARect.Left := ARect.Left + ScaleFactor.Apply
        (Owner.Options.DefaultFilterButtonWidth)
    else
      ARect.Right := ARect.Right - ScaleFactor.Apply
        (Owner.Options.DefaultFilterButtonWidth);
  inherited CalculateTextBounds(ARect);
end;

procedure TdxGanttControlSheetColumnHeaderViewInfo.Calculate(const R: TRect);
begin
  Clear;
  if FColumn.ShowFilterButton then
    AddChild(TdxGanttControlSheetColumnHeaderFilterButtonViewInfo.Create(Self));
  inherited Calculate(R);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := [bBottom];
  if Owner.UseRightToLeftAlignment then
    Include(Result, bLeft)
  else
    Include(Result, bRight)
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetCurrentCursor
  (const P: TPoint; const ADefaultCursor: TCursor): TCursor;
var
  AIndex: Integer;
begin
  if IsSizingZone(P) then
    Result := TdxGanttControlCursors.ColumnResize
  else
  begin
    AIndex := Owner.Headers.IndexOf(Self);
    if (AIndex >= 1) and Owner.Headers[AIndex - 1].IsSizingZone(P) then
      Result := TdxGanttControlCursors.ColumnResize
    else
      Result := inherited GetCurrentCursor(P, ADefaultCursor);
  end;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetHintText: string;
begin
  Result := Format('[B]%s[/B]'#13#10#13#10'%s',
    [Column.Caption, Column.GetHintText]);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(TdxGanttControlSheetColumn.MinWidth);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.CalculateBestFit: Integer;
begin
  Result := TdxGanttControlUtils.MeasureTextWidth(FTextLayout, FCaption,
    CanvasCache.GetControlFont);
  Result := Bounds.Width - FTextBounds.Width + Result;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
begin
  Result := Bounds;
  Result.Bottom := Result.Bottom - LookAndFeelPainter.BorderSize;
  if UseRightToLeftAlignment then
    Result.Right := Result.Left + ScaleFactor.Apply
      (Owner.Options.DefaultFilterButtonWidth)
  else
    Result.Left := Result.Right - ScaleFactor.Apply
      (Owner.Options.DefaultFilterButtonWidth);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.CalculateSelectedRect: TRect;
begin
  Result := Bounds;
  Result.Height := ScaleFactor.Apply(4);
  Result.MoveToBottom(Bounds.Bottom);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetNeighbors: TcxNeighbors;
begin
  Result := [nLeft, nRight];
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetTextHorizontalAlignment
  : TcxTextAlignX;
begin
  if Owner.UseRightToLeftAlignment then
    Result := taRight
  else
    Result := taLeft;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.GetTextVerticalAlignment
  : TcxTextAlignY;
begin
  Result := taBottom;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.HasHint: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.HasHotTrackState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.HasPressedState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnHeaderViewInfo.IsMovingZone
  (const P: TPoint): Boolean;
var
  R: TRect;
  I: Integer;
begin
  if not Column.RealAllowMove then
    Exit(False);
  R := Bounds;
  Result := R.Contains(P);
  if Result then
    for I := 0 to ViewInfoCount - 1 do
      if ViewInfos[I].Bounds.Contains(P) then
        Exit(False);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.IsSizingZone
  (const P: TPoint): Boolean;
begin
  Result := Column.RealAllowSize and inherited IsSizingZone(P) and
    (P.Y >= Bounds.Top) and (P.Y < Bounds.Bottom);
end;

function TdxGanttControlSheetColumnHeaderViewInfo.IsPressed: Boolean;
var
  ADragAndDropObject: TcxDragAndDropObject;
begin
  ADragAndDropObject := Owner.Controller.DragHelper.DragAndDropObject;
  Result := (ADragAndDropObject is TdxGanttControlSheetMovingObject) and
    (TdxGanttControlSheetMovingObject(ADragAndDropObject).Column = Column);
end;

{ TdxGanttControlSheetColumnHeaderImageViewInfo }

procedure TdxGanttControlSheetColumnHeaderImageViewInfo.Calculate
  (const R: TRect);
begin
  inherited Calculate(R);
  FImage := CalculateImage;
  FImageRect := CalculateImageBounds;
end;

function TdxGanttControlSheetColumnHeaderImageViewInfo.CalculateBestFit
  : Integer;
begin
  Result := FImageRect.Width;
end;

function TdxGanttControlSheetColumnHeaderImageViewInfo.
  CalculateImageBounds: TRect;
begin
  if FImage = nil then
    Exit(TRect.Null);
  Result.Height := Bounds.Height div 2;
  Result.Width := Ceil(FImage.Width * Result.Height / FImage.Height);
  Result := cxRectCenter(Bounds, Result.Size);
  Result.MoveToBottom(Bounds.Bottom - ScaleFactor.Apply(cxTextOffset) -
    LookAndFeelPainter.BorderSize);
end;

procedure TdxGanttControlSheetColumnHeaderImageViewInfo.CalculateTextBounds
  (const R: TRect);
begin
  // do nothing
end;

procedure TdxGanttControlSheetColumnHeaderImageViewInfo.DoDraw;
begin
  inherited DoDraw;
  if FImage <> nil then
    CanvasCache.GetImage(FImage).Draw(FImageRect);
end;

{ TdxGanttControlSheetColumnHeaderFilterButtonViewInfo }

procedure TdxGanttControlSheetColumnHeaderFilterButtonViewInfo.DoDraw;
var
  ABorders: TcxBorders;
begin
  if Owner.IsHotState(Self) then
  begin
    ABorders := [bLeft, bRight];
    Owner.DrawBackground(Self, ABorders, []);
  end;
end;

function TdxGanttControlSheetColumnHeaderFilterButtonViewInfo.
  HasHotTrackState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumnHeaderFilterButtonViewInfo.GetCurrentCursor
  (const P: TPoint; const ADefaultCursor: TCursor): TCursor;
begin
  if Owner.IsSizingZone(P) then
    Result := Owner.GetCurrentCursor(P, ADefaultCursor)
  else
    Result := ADefaultCursor;
end;

function TdxGanttControlSheetColumnHeaderFilterButtonViewInfo.GetOwner
  : TdxGanttControlSheetColumnHeaderViewInfo;
begin
  Result := TdxGanttControlSheetColumnHeaderViewInfo(inherited Owner);
end;

{ TdxGanttControlSheetColumnEmptyHeaderViewInfo }

function TdxGanttControlSheetColumnEmptyHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := [bBottom];
end;

function TdxGanttControlSheetColumnEmptyHeaderViewInfo.GetCurrentCursor
  (const P: TPoint; const ADefaultCursor: TCursor): TCursor;
begin
  if Owner.Options.AllowColumnSize and
    ((not UseRightToLeftAlignment and (P.X >= Bounds.Left) and
    (P.X <= Bounds.Left + GetResizeHitZoneWidth)) or
    (UseRightToLeftAlignment and (P.X >= Bounds.Right - GetResizeHitZoneWidth)
    and (P.X <= Bounds.Right))) then
    Result := TdxGanttControlCursors.ColumnResize
  else
    Result := inherited GetCurrentCursor(P, ADefaultCursor);
end;

function TdxGanttControlSheetColumnEmptyHeaderViewInfo.GetNeighbors
  : TcxNeighbors;
begin
  if Owner.UseRightToLeftAlignment then
    Result := [nRight]
  else
    Result := [nLeft];
end;

function TdxGanttControlSheetColumnEmptyHeaderViewInfo.HasHotTrackState
  : Boolean;
begin
  Result := False;
end;

{ TdxGanttControlSheetCustomViewInfo }

constructor TdxGanttControlSheetCustomViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo;
  AOptions: TdxGanttControlSheetOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
  FHeaders := TObjectList<TdxGanttControlSheetHeaderViewInfo>.Create;
  FGridlines := TList<TRect>.Create;
  FDataRows := TObjectList<TdxGanttControlSheetDataRowViewInfo>.Create;
  FCachedDataRowHeight := TDictionary<TObject, Integer>.Create;
end;

destructor TdxGanttControlSheetCustomViewInfo.Destroy;
begin
  FreeAndNil(FCachedDataRowHeight);
  FreeAndNil(FDataRows);
  FreeAndNil(FGridlines);
  FreeAndNil(FHeaders);
  inherited Destroy;
end;

procedure TdxGanttControlSheetCustomViewInfo.Reset;
begin
  inherited Reset;
  FCachedDataRowHeight.Clear;
  FCachedSingleLineHeight := 0;
  FCachedImageHeight := 0;
end;

procedure TdxGanttControlSheetCustomViewInfo.AppendDataRow(ARowIndex: Integer);
var
  AHeight: Integer;
  AData: TObject;
  R: TRect;
begin
  if FDataRows.Count = 0 then
    AHeight := FHeaders[0].Bounds.Bottom
  else
    AHeight := FDataRows.Last.Bounds.Bottom;

  if ARowIndex < Options.DataProvider.Count then
    AData := Options.DataProvider.Items[ARowIndex]
  else
    AData := nil;
  FDataRows.Add(TdxGanttControlSheetDataRowViewInfo.Create(Self,
    FDataRows.Count, AData));
  FDataRows.Last.CalculateLayout;
  R := Bounds;
  R.Top := AHeight;
  R.Bottom := R.Top + FDataRows.Last.MeasureHeight;
  FDataRows.Last.Calculate(R);
end;

procedure TdxGanttControlSheetCustomViewInfo.AppendDataRows;
var
  I: Integer;
  ABottom: Integer;
begin
  I := FFirstVisibleRowIndex + DataRows.Count;
  ABottom := ClientRect.Bottom;
  repeat
    AppendDataRow(I);
    Inc(I);
  until FDataRows.Last.Bounds.Bottom > ABottom;
end;

procedure TdxGanttControlSheetCustomViewInfo.CalculateDataRows;
begin
  FDataRows.Clear;
  AppendDataRows;
end;

function TdxGanttControlSheetCustomViewInfo.CalculateFocusedCellViewInfo
  : TdxGanttControlSheetCellCustomViewInfo;
var
  X, Y: Integer;
begin
  Result := nil;
  Y := Controller.FocusedCell.Y - Controller.FirstVisibleRowIndex;
  if (Y >= 0) and (Y < DataRows.Count) then
  begin
    X := Controller.FocusedCell.X - Controller.FirstVisibleColumnIndex;
    if (X >= 0) and (X < DataRows[Y].Cells.Count) then
      Result := DataRows[Y].Cells[X];
  end;
end;

procedure TdxGanttControlSheetCustomViewInfo.CalculateGridlines;
var
  I: Integer;
begin
  FGridlines.Clear;
  for I := 0 to FHeaders.Count - 1 do
  begin
    if FHeaders[I] is TdxGanttControlSheetColumnHeaderViewInfo then
    begin
      if UseRightToLeftAlignment then
        FGridlines.Add(TRect.Create(FHeaders[I].Bounds.Left,
          FHeaders[I].Bounds.Bottom, FHeaders[I].Bounds.Left + 1,
          ClientRect.Bottom))
      else
        FGridlines.Add(TRect.Create(FHeaders[I].Bounds.Right - 1,
          FHeaders[I].Bounds.Bottom, FHeaders[I].Bounds.Right,
          ClientRect.Bottom));
    end;
  end;
end;

procedure TdxGanttControlSheetCustomViewInfo.CalculateHeaders;
var
  I: Integer;
  AWidth, AMaxWidth: Integer;
  AHeaderRowWidth: Integer;
  R: TRect;
  AColumnBounds: TRect;
  AColumnHeight: Integer;
begin
  FHeaders.Clear;
  AHeaderRowWidth := dxGetTouchableSize
    (ScaleFactor.Apply(Options.RowHeaderWidth), ScaleFactor);
  R.Top := Bounds.Top;
  R.Bottom := Bounds.Top + GetColumnHeaderHeight;
  if UseRightToLeftAlignment then
  begin
    R.Left := Bounds.Right - AHeaderRowWidth;
    R.Right := Bounds.Right;
  end
  else
  begin
    R.Left := 0;
    R.Right := AHeaderRowWidth;
  end;
  FHeaders.Add(TdxGanttControlSheetHeaderGripViewInfo.Create(Self));
  FHeaders.Last.Calculate(R);
  AMaxWidth := Bounds.Width - R.Width;
  FVisibleColumnCount := 0;
  AColumnHeight := R.Height;
  AWidth := 0;
  if (Options.Columns.VisibleCount > 0) and
    (Options.Columns.VisibleCount > FFirstVisibleColumnIndex) then
  begin
    I := Options.Columns.VisibleItems[FFirstVisibleColumnIndex].Index;
    if I < Options.Columns.Count then
      repeat
        if not Options.Columns[I].Visible then
        begin
          Inc(I);
          Continue;
        end;
        FHeaders.Add(Options.Columns[I].CreateViewInfo(Self));
        AColumnBounds := TRect.Create(0, 0,
          ScaleFactor.Apply(Options.Columns[I].Width), AColumnHeight);
        if UseRightToLeftAlignment then
          AColumnBounds.Offset(R.Left - AWidth - AColumnBounds.Width, R.Top)
        else
          AColumnBounds.Offset(R.Right + AWidth, R.Top);
        FHeaders.Last.Calculate(AColumnBounds);
        AWidth := AWidth + FHeaders.Last.Bounds.Width;
        Inc(FVisibleColumnCount);
        Inc(I);
      until (AWidth > AMaxWidth) or (I >= Options.Columns.Count);
  end;

  if AWidth < AMaxWidth then
  begin
    FHeaders.Add(TdxGanttControlSheetColumnEmptyHeaderViewInfo.Create(Self));
    AColumnBounds := TRect.Create(0, 0, AMaxWidth - AWidth + 1, AColumnHeight);
    if UseRightToLeftAlignment then
      AColumnBounds.Offset(Bounds.Left - 1, R.Top)
    else
      AColumnBounds.Offset(R.Right + AWidth, R.Top);
    FHeaders.Last.Calculate(AColumnBounds);
  end
  else
    Dec(FVisibleColumnCount);
end;

procedure TdxGanttControlSheetCustomViewInfo.CalculateClientRect;
begin
  FClientRect := Controller.ScrollBars.GetClientRect(Bounds);
end;

procedure TdxGanttControlSheetCustomViewInfo.Clear;
begin
  inherited Clear;
  FCachedImageHeight := 0;
end;

function TdxGanttControlSheetCustomViewInfo.CalculateHitTest(const AHitTest
  : TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := ClientRect.Contains(AHitTest.HitPoint) and
    inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    for I := 0 to FHeaders.Count - 1 do
      if FHeaders[I].CalculateHitTest(AHitTest) then
        Exit(True);
    for I := 0 to DataRows.Count - 1 do
      if DataRows[I].CalculateHitTest(AHitTest) then
        Exit(True);
  end;
end;

procedure TdxGanttControlSheetCustomViewInfo.Calculate(const R: TRect);
begin
  ResetFocusedCellViewInfo;
  FFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex;
  FFirstVisibleRowIndex := Controller.FirstVisibleRowIndex;
  inherited Calculate(R);
  CalculateHeaders;
  CalculateClientRect;
  CalculateDataRows;
  CalculateGridlines;
  Controller.ScrollBars.CalculateScrollBars;
end;

procedure TdxGanttControlSheetCustomViewInfo.ViewChanged;

  procedure MoveDown;
  var
    ATop: Integer;
    I: Integer;
  begin
    for I := FFirstVisibleRowIndex - 1 downto Controller.FirstVisibleRowIndex do
    begin
      AppendDataRow(I);
      DataRows.Move(DataRows.Count - 1, 0);
    end;
    FFirstVisibleRowIndex := Controller.FirstVisibleRowIndex;
    ATop := Headers[0].Bounds.Bottom;
    for I := 0 to DataRows.Count - 1 do
    begin
      DataRows[I].DoScroll(0, ATop - DataRows[I].Bounds.Top);
      ATop := DataRows[I].Bounds.Bottom;
    end;
    for I := DataRows.Count - 1 downto 0 do
      if DataRows[I].Bounds.Top > ClientRect.Bottom then
        DataRows.Delete(I)
      else
        DataRows[I].UpdateIndex(I);
  end;

  procedure MoveUp;
  var
    AOffset: Integer;
    I: Integer;
  begin
    AOffset := 0;
    for I := FFirstVisibleRowIndex to Controller.FirstVisibleRowIndex - 1 do
    begin
      AOffset := AOffset + DataRows.First.Bounds.Height;
      DataRows.Delete(0);
    end;
    FFirstVisibleRowIndex := Controller.FirstVisibleRowIndex;
    for I := 0 to DataRows.Count - 1 do
    begin
      DataRows[I].UpdateIndex(I);
      DataRows[I].DoScroll(0, -AOffset);
    end;
    AppendDataRows;
    for I := DataRows.Count - 1 downto 0 do
      if DataRows[I].Bounds.Top > ClientRect.Bottom then
        DataRows.Delete(I)
      else
        Break;
  end;

var
  ARecalculateNeeded: Boolean;
  I: Integer;
begin
  if (FFirstVisibleColumnIndex <> Controller.FirstVisibleColumnIndex) or
    (FFirstVisibleRowIndex <> Controller.FirstVisibleRowIndex) then
  begin
    ARecalculateNeeded := True;
    if (FFirstVisibleColumnIndex = Controller.FirstVisibleColumnIndex) and
      (DataRows.Count > Abs(FFirstVisibleRowIndex -
      Controller.FirstVisibleRowIndex)) then
    begin
      if FFirstVisibleRowIndex < Controller.FirstVisibleRowIndex then
        MoveUp
      else
        MoveDown;
      ARecalculateNeeded := False;
    end;
    if FFirstVisibleRowIndex = Controller.FirstVisibleRowIndex then
    begin
      FFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex;
      CalculateHeaders;
      CalculateGridlines;
      for I := 0 to DataRows.Count - 1 do
        DataRows[I].ViewChanged;
      Controller.ScrollBars.CalculateScrollBars;
      ARecalculateNeeded := False;
    end;
    if ARecalculateNeeded then
      Recalculate
    else
      ResetFocusedCellViewInfo;
  end;
end;

procedure TdxGanttControlSheetCustomViewInfo.DoDraw;
var
  I: Integer;
begin
  Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  Controller.ScrollBars.DrawSizeGrip(Canvas);
  for I := 0 to FHeaders.Count - 1 do
    FHeaders[I].Draw;
  for I := 0 to FDataRows.Count - 1 do
    FDataRows[I].Draw;
  for I := 0 to FGridlines.Count - 1 do
    Canvas.FillRect(FGridlines[I], LookAndFeelPainter.DefaultGridlineColor);
end;

function TdxGanttControlSheetCustomViewInfo.GetColumnHeaderHeight: Integer;
begin
  Result := dxGetTouchableSize
    (MulDiv(TdxGanttControlSheetOptions.ColumnHeaderMinHeight,
    Abs(CanvasCache.GetBaseFont.Height), 11), ScaleFactor);
end;

procedure TdxGanttControlSheetCustomViewInfo.ResetFocusedCellViewInfo;
begin
  FFocusedCellViewInfo := nil;
end;

function TdxGanttControlSheetCustomViewInfo.GetFocusedCellViewInfo
  : TdxGanttControlSheetCellCustomViewInfo;
begin
  if FFocusedCellViewInfo = nil then
    FFocusedCellViewInfo := CalculateFocusedCellViewInfo;
  Result := FFocusedCellViewInfo;
end;

function TdxGanttControlSheetCustomViewInfo.GetSingleLineHeight: Integer;
var
  ATextLayout: TcxCanvasBasedTextLayout;
begin
  if FCachedSingleLineHeight = 0 then
  begin
    ATextLayout := Canvas.CreateTextLayout;
    try
      FCachedSingleLineHeight := TdxGanttControlUtils.MeasureTextHeight
        (ATextLayout, MaxInt, 'Qq', CanvasCache.GetControlFont, True);
    finally
      ATextLayout.Free;
    end;
  end;
  Result := FCachedSingleLineHeight;
end;

function TdxGanttControlSheetCustomViewInfo.GetVisibleRowCount: Integer;
begin
  Result := DataRows.Count;
  if (Result > 0) and (ClientRect.Bottom < DataRows.Last.Bounds.Bottom) then
    Dec(Result);
end;

function TdxGanttControlSheetCustomViewInfo.GetController
  : TdxGanttControlSheetController;
begin
  Result := Options.Controller;
end;

function TdxGanttControlSheetCustomViewInfo.GetImageHeight: Integer;
begin
  if FCachedImageHeight = 0 then
    FCachedImageHeight := ScaleFactor.Apply(Options.RowHeight - 2 *
      cxTextOffset) - 1;
  Result := FCachedImageHeight;
end;

function TdxGanttControlSheetCustomViewInfo.
  GetQuickCustomizationPopupOwnerBounds: TRect;
begin
  if (Headers.Count > 0) and (Headers[0].ViewInfoCount > 0) then
    Result := Headers[0].ViewInfos[0].Bounds
  else
    Result := TRect.Null;
end;

{ TdxGanttControlSheetColumn }

procedure TdxGanttControlSheetColumn.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlSheetColumn;
begin
  if Source is TdxGanttControlSheetColumn then
  begin
    ASource := TdxGanttControlSheetColumn(Source);
    Caption := ASource.Caption;
    ShowFilterButton := ASource.ShowFilterButton;
    Visible := ASource.Visible;
    AllowMove := ASource.AllowMove;
    AllowSize := ASource.AllowSize;
    Width := ASource.Width;
  end
  else
    inherited Assign(Source);
end;

function TdxGanttControlSheetColumn.RealAllowHide: Boolean;
begin
  Result := (FAllowHide = bTrue) or
    ((FAllowHide = bDefault) and (Owner.Owner.AllowColumnHide));
end;

function TdxGanttControlSheetColumn.RealAllowMove: Boolean;
begin
  Result := (FAllowMove = bTrue) or
    ((FAllowMove = bDefault) and (Owner.Owner.AllowColumnMove));
end;

function TdxGanttControlSheetColumn.RealAllowSize: Boolean;
begin
  Result := (FAllowSize = bTrue) or
    ((FAllowSize = bDefault) and (Owner.Owner.AllowColumnSize));
end;

procedure TdxGanttControlSheetColumn.Changed;
begin
  Owner.Changed;
end;

procedure TdxGanttControlSheetColumn.WriteProperties(Writer: TWriter);
var
  APropInfo: PPropInfo;
begin
  Writer.WriteUTF8Str('ItemClass');
  Writer.WriteString(ClassName);

  APropInfo := GetPropInfo(ClassInfo, 'AllowHide');

  if AllowHide <> bDefault then
  begin
    Writer.WriteUTF8Str('AllowHide');
    TWriterAccess(Writer).WriteIdent(GetEnumName(APropInfo^.PropType^,
      Ord(AllowHide)));
  end;

  if AllowMove <> bDefault then
  begin
    Writer.WriteUTF8Str('AllowMove');
    TWriterAccess(Writer).WriteIdent(GetEnumName(APropInfo^.PropType^,
      Ord(AllowMove)));
  end;

  if AllowSize <> bDefault then
  begin
    Writer.WriteUTF8Str('AllowSize');
    TWriterAccess(Writer).WriteIdent(GetEnumName(APropInfo^.PropType^,
      Ord(AllowSize)));
  end;

  if IsCaptionStored then
  begin
    Writer.WriteUTF8Str('Caption');
    Writer.WriteString(Caption);
  end;

  if IsShowFilterButtonStored then
  begin
    Writer.WriteUTF8Str('ShowFilterButton');
    Writer.WriteBoolean(ShowFilterButton);
  end;

  if IsVisibleStored then
  begin
    Writer.WriteUTF8Str('Visible');
    Writer.WriteBoolean(Visible);
  end;

  if IsWidthStored then
  begin
    Writer.WriteUTF8Str('Width');
    Writer.WriteInteger(Width);
  end;
end;

constructor TdxGanttControlSheetColumn.Create
  (AOwner: TdxGanttControlSheetColumns);
begin
  inherited Create(AOwner);
  FProperties := CreateProperties;
  DoReset;
end;

destructor TdxGanttControlSheetColumn.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TdxGanttControlSheetColumn.DoReset;
begin
  FWidth := GetDefaultWidth;
  FVisible := GetDefaultVisible;
  FCaption := GetDefaultCaption;
  FIsCaptionAssigned := False;
  FAllowHide := bDefault;
  FAllowMove := bDefault;
  FAllowSize := bDefault;
  FShowFilterButton := GetDefaultShowFilterButton;
end;

function TdxGanttControlSheetColumn.GetCaption: string;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := GetDefaultCaption;
end;

function TdxGanttControlSheetColumn.GetCollection: TdxGanttControlSheetColumns;
begin
  Result := TdxGanttControlSheetColumns(inherited Owner);
end;

function TdxGanttControlSheetColumn.CanShowFilterButton: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumn.CreateProperties: TcxCustomEditProperties;
begin
  if GetPropertiesClass <> nil then
    Result := GetPropertiesClass.Create(Self)
  else
    Result := nil;
end;

function TdxGanttControlSheetColumn.CreateViewInfo(ASheetViewInfo
  : TdxGanttControlSheetCustomViewInfo)
  : TdxGanttControlSheetColumnHeaderViewInfo;
begin
  Result := TdxGanttControlSheetColumnHeaderViewInfo.Create
    (ASheetViewInfo, Self);
end;

function TdxGanttControlSheetColumn.IsEditable: Boolean;
begin
  Result := GetPropertiesClass <> nil;
end;

function TdxGanttControlSheetColumn.GetDataCellViewInfoClass
  : TdxGanttControlSheetCellViewInfoClass;
begin
  Result := TdxGanttControlSheetCellViewInfo;
end;

function TdxGanttControlSheetColumn.GetDefaultCaption: string;
begin
  Result := '';
end;

function TdxGanttControlSheetColumn.GetDefaultShowFilterButton: Boolean;
begin
  Result := False;
end;

function TdxGanttControlSheetColumn.GetDefaultImageIndex: Integer;
begin
  Result := -1;
end;

function TdxGanttControlSheetColumn.GetDefaultVisible: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetColumn.GetDefaultWidth: Integer;
begin
  Result := DefaultWidth;
end;

class function TdxGanttControlSheetColumn.GetDesignCaption: string;
begin
  Result := '';
end;

function TdxGanttControlSheetColumn.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := nil;
end;

procedure TdxGanttControlSheetColumn.PrepareEditProperties
  (AProperties: TcxCustomEditProperties; AData: TObject);
begin
  // do nothing
end;

function TdxGanttControlSheetColumn.GetEditValue(AData: TObject): Variant;
begin
  Result := Null;
end;

function TdxGanttControlSheetColumn.GetHintText: string;
begin
  Result := '';
end;

procedure TdxGanttControlSheetColumn.SetEditValue(AData: TObject;
  const Value: Variant);
begin
  // do nothing
end;

function TdxGanttControlSheetColumn.GetIndex: Integer;
begin
  Result := Owner.IndexOf(Self);
end;

function TdxGanttControlSheetColumn.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxGanttControlSheetColumn.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned;
end;

function TdxGanttControlSheetColumn.IsShowFilterButtonStored: Boolean;
begin
  Result := ShowFilterButton <> GetDefaultShowFilterButton;
end;

function TdxGanttControlSheetColumn.IsVisibleStored: Boolean;
begin
  Result := Visible <> GetDefaultVisible;
end;

function TdxGanttControlSheetColumn.IsWidthStored: Boolean;
begin
  Result := Width <> GetDefaultWidth;
end;

procedure TdxGanttControlSheetColumn.Reset;
begin
  Owner.BeginUpdate;
  try
    DoReset;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TdxGanttControlSheetColumn.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    FIsCaptionAssigned := FCaption <> GetDefaultCaption;
    Changed;
  end;
end;

procedure TdxGanttControlSheetColumn.SetShowFilterButton(const Value: Boolean);
begin
  if (ShowFilterButton = Value) and (not Value or CanShowFilterButton) then
  begin
    FShowFilterButton := Value;
    Changed;
  end;
end;

procedure TdxGanttControlSheetColumn.SetIndex(const Value: Integer);
begin
  Owner.Move(Index, Value);
end;

procedure TdxGanttControlSheetColumn.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TdxGanttControlSheetColumn.SetWidth(const Value: Integer);
begin
  if (Width <> Value) and (Value >= MinWidth) then
  begin
    FWidth := Value;
    Owner.Owner.DoColumnSizeChanged(Self);
    Changed;
    SetDesignerModified(Owner.Owner.Control);
  end;
end;

{ TdxGanttControlSheetColumns }

constructor TdxGanttControlSheetColumns.Create
  (AOwner: TdxGanttControlSheetOptions);
begin
  inherited Create(AOwner);
  FRegisteredColumnClasses := TList<TdxGanttControlSheetColumnClass>.Create;
  RegisterColumnClasses;
  FList := TObjectList<TdxGanttControlSheetColumn>.Create;
end;

destructor TdxGanttControlSheetColumns.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FRegisteredColumnClasses);
  inherited Destroy;
end;

procedure TdxGanttControlSheetColumns.AfterConstruction;
begin
  inherited AfterConstruction;
  DoReset;
end;

procedure TdxGanttControlSheetColumns.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxGanttControlSheetColumns.Changed;
begin
  if FLockCount = 0 then
    Owner.Changed([TdxGanttControlOptionsChangedType.Size]);
end;

procedure TdxGanttControlSheetColumns.Clear;
begin
  FList.Clear;
  Changed;
end;

procedure TdxGanttControlSheetColumns.DoReset;
var
  I: Integer;
begin
  Clear;
  for I := 0 to RegisteredColumnClasses.Count - 1 do
    Add(RegisteredColumnClasses[I]);
end;

procedure TdxGanttControlSheetColumns.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

procedure TdxGanttControlSheetColumns.Extract
  (AItem: TdxGanttControlSheetColumn);
begin
  FList.Extract(AItem);
end;

procedure TdxGanttControlSheetColumns.DataReaderHandler(Reader: TReader);
var
  AClass: string;
  AItem: TdxGanttControlSheetColumn;
  AItemClass: TClass;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
    begin
      if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
        Reader.ReadInteger;
      Reader.ReadListBegin;
      AItem := nil;
      if Reader.ReadStr = 'ItemClass' then
      begin
        AClass := Reader.ReadString;
        AItemClass := FindClass(AClass);
        if AItemClass <> nil then
          AItem := Add(TdxGanttControlSheetColumnClass(AItemClass));
      end;
      while not Reader.EndOfList do
        if AItem <> nil then
          TReaderAccess(Reader).ReadProperty(AItem)
        else
        begin
          Reader.ReadStr;
          Reader.ReadValue;
        end;
      Reader.ReadListEnd;
    end;
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlSheetColumns.DataWriterHandler(Writer: TWriter);
var
  I: Integer;
begin
  TWriterAccess(Writer).WriteValue(vaCollection);
  for I := 0 to Count - 1 do
  begin
    Writer.WriteListBegin;
    Items[I].WriteProperties(Writer);
    Writer.WriteListEnd;
  end;
  Writer.WriteListEnd;
end;

function TdxGanttControlSheetColumns.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxGanttControlSheetColumns.GetItem(Index: Integer)
  : TdxGanttControlSheetColumn;
begin
  Result := FList[Index];
end;

function TdxGanttControlSheetColumns.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxGanttControlSheetColumns.IndexOf
  (AItem: TdxGanttControlSheetColumn): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TdxGanttControlSheetColumns.Move(ACurrentIndex, ANewIndex: Integer);
begin
  FList.Move(ACurrentIndex, ANewIndex);
  Owner.DoColumnPositionChanged(Items[ANewIndex]);
  Changed;
  SetDesignerModified(Owner.Control);
end;

procedure TdxGanttControlSheetColumns.RegisterColumnClass
  (AClass: TdxGanttControlSheetColumnClass);
begin
  FRegisteredColumnClasses.Add(AClass);
end;

procedure TdxGanttControlSheetColumns.RegisterColumnClasses;
begin
  // do nothing
end;

function TdxGanttControlSheetColumns.InternalGetOwner
  : TdxGanttControlSheetOptions;
begin
  Result := TdxGanttControlSheetOptions(inherited Owner);
end;

function TdxGanttControlSheetColumns.GetVisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Visible then
      Inc(Result);
end;

function TdxGanttControlSheetColumns.GetVisibleIndex
  (AItem: TdxGanttControlSheetColumn): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not AItem.Visible then
    Exit;
  for I := 0 to Count - 1 do
  begin
    if not Items[I].Visible then
      Continue;
    Inc(Result);
    if Items[I] = AItem then
      Exit;
  end;
  Result := -1;
end;

function TdxGanttControlSheetColumns.GetVisibleItem(Index: Integer)
  : TdxGanttControlSheetColumn;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Visible then
    begin
      if Index = 0 then
        Exit(Items[I]);
      Dec(Index);
    end;
  Result := nil;
end;

procedure TdxGanttControlSheetColumns.Reset;
begin
  BeginUpdate;
  try
    DoReset;
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlSheetColumns.SetItem(Index: Integer;
  const Value: TdxGanttControlSheetColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TdxGanttControlSheetColumns.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', DataReaderHandler, DataWriterHandler, True);
end;

function TdxGanttControlSheetColumns.Add
  (AClass: TdxGanttControlSheetColumnClass): TdxGanttControlSheetColumn;
begin
  Result := AClass.Create(Self);
  FList.Add(Result);
  Changed;
end;

procedure TdxGanttControlSheetColumns.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
  Changed;
end;

function TdxGanttControlSheetColumns.Insert(AIndex: Integer;
  AClass: TdxGanttControlSheetColumnClass): TdxGanttControlSheetColumn;
begin
  Result := AClass.Create(Self);
  FList.Insert(AIndex, Result);
  Changed;
end;

procedure TdxGanttControlSheetColumns.Remove(AItem: TdxGanttControlSheetColumn);
begin
  FList.Remove(AItem);
  Changed;
end;

{ TdxGanttControlSheetCustomDataProvider }

function TdxGanttControlSheetCustomDataProvider.GetRowHeaderCaption
  (AData: TObject): string;
begin
  Result := '';
end;

function TdxGanttControlSheetCustomDataProvider.InternalIndexOf
  (AItem: TObject): Integer;
var
  L, H: Integer;
  mid, cmp: Integer;
  AResult: Boolean;
begin
  if Count = 0 then
    Exit(-1);
  L := 0;
  H := Count - 1;
  AResult := False;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := GetDataItemIndex(Items[mid]) - GetDataItemIndex(AItem);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
      begin
        L := mid;
        AResult := True;
        Break;
      end;
    end;
  end;
  if AResult then
    Result := L
  else
    Result := -1;
end;

{ TdxGanttControlSheetEditingController }

constructor TdxGanttControlSheetEditingController.Create
  (AController: TdxGanttControlSheetController);
begin
  inherited Create(AController.Control);
  FController := AController;
end;

destructor TdxGanttControlSheetEditingController.Destroy;
begin
  FreeAndNil(FEditData);
  inherited Destroy;
end;

procedure TdxGanttControlSheetEditingController.DoUpdateEdit;
begin
  if not IsEditing or (Edit = nil) then
    Exit;

  if EditPreparing then
  begin
    AssignEditStyle;
    UpdateEditPosition;
    Edit.Visible := True;
    Controller.Options.DoInitEdit(FocusedCellViewInfo.Column, Edit);
  end;
end;

function TdxGanttControlSheetEditingController.CanInitEditing: Boolean;
begin
  Result := (FocusedCellViewInfo <> nil) and
    FocusedCellViewInfo.Column.IsEditable;
  if Result then
  begin
    Result := Controller.Options.DoBeforeEdit(FocusedCellViewInfo.Column);
    Exit;
  end;
end;

procedure TdxGanttControlSheetEditingController.ClearEditingItem;
begin
  // do nothing
end;

procedure TdxGanttControlSheetEditingController.DoHideEdit(Accept: Boolean);
var
  AEditValue, APreviousValue: Variant;
begin
  if Edit = nil then
    Exit;

  if Accept then
    Edit.Deactivate;

  if Accept and Edit.EditModified then
  begin
    AEditValue := Edit.EditValue;
    APreviousValue := GetValue;
    if not VarEquals(APreviousValue, AEditValue) then
      SetValue(AEditValue);
  end;
  UninitEdit;
  Edit.EditModified := False;
  if Controller.Control.CanFocusEx and (Edit <> nil) and Edit.IsFocused then
    Controller.Control.SetFocus;
  HideInplaceEditor;
end;

procedure TdxGanttControlSheetEditingController.DoShowEdit
  (AProc: TActivateEditProc);
begin
  try
    if Controller.Control.DragAndDropState = ddsNone then
      Controller.MakeFocusedCellVisible;
    if PrepareEdit(False) then
    begin
      Controller.ScrollBars.HideTouchScrollUI(True);
      AProc;
    end;
  except
    HideEdit(False);
    raise;
  end;
end;

function TdxGanttControlSheetEditingController.GetCancelEditingOnExit: Boolean;
begin
  Result := False;
end;

function TdxGanttControlSheetEditingController.GetEditParent: TWinControl;
begin
  Result := Controller.Control;
end;

function TdxGanttControlSheetEditingController.GetHideEditOnExit: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetEditingController.
  GetHideEditOnFocusedRecordChange: Boolean;
begin
  Result := True;
end;

function TdxGanttControlSheetEditingController.GetFocusedCellBounds: TRect;
begin
  Result := FocusedCellViewInfo.Bounds;
end;

function TdxGanttControlSheetEditingController.GetIsEditing: Boolean;
begin
  Result := (FocusedCellViewInfo <> nil) and
    FocusedCellViewInfo.Column.IsEditable and (Edit <> nil);
end;

function TdxGanttControlSheetEditingController.GetValue: Variant;
begin
  Result := FocusedCellViewInfo.EditValue;
end;

procedure TdxGanttControlSheetEditingController.SetValue(const AValue: Variant);
var
  AIsEnabled: Boolean;
begin
  with Controller.CreateChangeCellValueCommand(AValue) do
    try
      AIsEnabled := Enabled;
      Execute;
    finally
      Free;
    end;
  if AIsEnabled then
    Controller.Options.DoEditValueChanged(FocusedCellViewInfo.Column);
end;

procedure TdxGanttControlSheetEditingController.StartEditingByTimer;
begin
  // do nothing
end;

procedure TdxGanttControlSheetEditingController.UpdateInplaceParamsPosition;
begin
  // do nothing
end;

procedure TdxGanttControlSheetEditingController.EditAfterKeyDown
  (Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AIsReplacementMode: Boolean;
  AEdit: TcxCustomTextEdit;
begin
  inherited EditAfterKeyDown(Sender, Key, Shift);
  AIsReplacementMode := VarIsSoftNull(Edit.EditValue) or
    VarIsSoftEmpty(Edit.EditValue);
  AEdit := TcxCustomTextEdit(Edit);
  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME,
    VK_END] then
  begin
    if AIsReplacementMode or (Controller.Options.RealAlwaysShowEditor and
      ((Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR]) or
      ((AEdit.SelLength = 0) and ((Key in [VK_HOME, VK_LEFT]) and
      (AEdit.SelStart = 0)) or ((Key in [VK_END, VK_RIGHT]) and
      (AEdit.SelStart = Length(AEdit.Text)))))) then
    begin
      Controller.KeyDown(Key, Shift);
    end;
  end;
end;

procedure TdxGanttControlSheetEditingController.EditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited EditKeyDown(Sender, Key, Shift);
  case Key of
    VK_ESCAPE:
      HideEdit(False);
    VK_TAB, VK_RETURN:
      begin
        HideEdit(True);
        Controller.KeyDown(Key, Shift);
      end;
  end;
  if not IsEditing then
    Key := 0;
end;

procedure TdxGanttControlSheetEditingController.EditKeyPress(Sender: TObject;
  var Key: Char);
begin
  Controller.MakeFocusedCellVisible;
  inherited EditKeyPress(Sender, Key);
end;

procedure TdxGanttControlSheetEditingController.EditKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited EditKeyUp(Sender, Key, Shift);
end;

function TdxGanttControlSheetEditingController.PrepareEdit
  (AIsMouseEvent: Boolean): Boolean;
begin
  Result := IsEditing;
  if EditPreparing or EditHiding or not CanInitEditing then
    Exit;

  FEditPreparing := True;
  try
    Result := FocusedCellViewInfo <> nil;
    if Result then
    begin
      FreeAndNil(FEditData);
      FEdit := EditList.GetEdit(FocusedCellViewInfo.Column.Properties);
      FocusedCellViewInfo.Column.PrepareEditProperties
        (TcxCustomEditAccess(FEdit).Properties, FocusedCellViewInfo.Owner.Data);
      Edit.Parent := nil;
      Edit.Visible := False;
    end;
    Result := Edit <> nil;

    if Result then
    begin
      InitEdit;
      UpdateEditPosition;
    end;
  finally
    FEditPreparing := False;
  end;
end;

procedure TdxGanttControlSheetEditingController.UpdateEditPosition;
var
  R: TRect;
begin
  if Edit <> nil then
  begin
    R := cxRectInflate(GetFocusedCellBounds, -1, -1);
    R.Intersect(Controller.ViewInfo.Bounds);
    Dec(R.Bottom);
    Dec(R.Right);
    if Controller.ViewInfo.UseRightToLeftAlignment then
      Inc(R.Left);
    Edit.BoundsRect := R;
  end;
end;

procedure TdxGanttControlSheetEditingController.ShowEdit;
begin
  DoShowEdit(procedure
    begin
      FEdit.Activate(FEditData, Controller.Control.Focused);
    end);
end;

procedure TdxGanttControlSheetEditingController.ShowEditByKey
  (const AChar: Char);
begin
  DoShowEdit(procedure
    begin
      Edit.ActivateByKey(AChar, FEditData);
    end);
end;

procedure TdxGanttControlSheetEditingController.ShowEditByMouse;
begin
  DoShowEdit(procedure
    begin
      Edit.ActivateByMouse(KeyboardStateToShiftState,
        Controller.HitTest.HitPoint.X, Controller.HitTest.HitPoint.Y,
        FEditData);
    end);
end;

procedure TdxGanttControlSheetEditingController.AssignEditStyle;
var
  AStyle: TcxCustomEditStyleAccess;
begin
  AStyle := TcxCustomEditStyleAccess(Edit.Style);
  AStyle.Font := FocusedCellViewInfo.GetFont;
  AStyle.Color := FocusedCellViewInfo.LookAndFeelPainter.DefaultContentColor;
  AStyle.TextColor := FocusedCellViewInfo.LookAndFeelPainter.
    DefaultContentTextColor;
  AStyle.ButtonTransparency := ebtHideInactive;
  AStyle.Changed;
  AStyle.BorderStyle := ebsNone;
  AStyle.TransparentBorder := False;
end;

function TdxGanttControlSheetEditingController.GetFocusedCellViewInfo
  : TdxGanttControlSheetCellViewInfo;
begin
  if Controller.FocusedCellViewInfo is TdxGanttControlSheetCellViewInfo then
    Result := TdxGanttControlSheetCellViewInfo(Controller.FocusedCellViewInfo)
  else
    Result := nil;
end;

{ TdxGanttControlSheetResizingObject }

constructor TdxGanttControlSheetResizingObject.Create
  (AController: TdxGanttControlCustomController;
AViewInfo: TdxGanttControlSheetHeaderViewInfo);
begin
  inherited Create(AController);
  FViewInfo := AViewInfo;
end;

procedure TdxGanttControlSheetResizingObject.ApplyChanges(const P: TPoint);
var
  AWidth: Integer;
begin
  AWidth := Helper.CalculateResizePoint(FViewInfo, P).X;
  if FViewInfo.UseRightToLeftAlignment then
    AWidth := FViewInfo.Bounds.Right - AWidth
  else
    AWidth := AWidth - FViewInfo.Bounds.Left;
  SetWidth(AWidth);
end;

function TdxGanttControlSheetResizingObject.CanDrop(const P: TPoint): Boolean;
begin
  DragAndDrop(P, Result);
end;

procedure TdxGanttControlSheetResizingObject.DragAndDrop(const P: TPoint;
var Accepted: Boolean);
var
  APos: TPoint;
begin
  Accepted := True;
  APos := Helper.CalculateResizePoint(FViewInfo, P);
  ShowDragImage(APos);
end;

function TdxGanttControlSheetResizingObject.GetController
  : TdxGanttControlSheetController;
begin
  Result := TdxGanttControlSheetController(inherited Controller);
end;

function TdxGanttControlSheetResizingObject.GetDragImageHeight: Integer;
begin
  Result := Controller.ViewInfo.ClientRect.Height;
end;

function TdxGanttControlSheetResizingObject.GetDragImageWidth: Integer;
begin
  Result := Controller.ViewInfo.ScaleFactor.Apply(Width);
end;

function TdxGanttControlSheetResizingObject.GetHelper
  : TdxGanttControlSheetDragHelper;
begin
  Result := TdxGanttControlSheetDragHelper(Controller.DragHelper);
end;

{ TdxGanttControlSheetColumnResizingObject }

function TdxGanttControlSheetColumnResizingObject.GetViewInfo
  : TdxGanttControlSheetColumnHeaderViewInfo;
begin
  Result := TdxGanttControlSheetColumnHeaderViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlSheetColumnResizingObject.SetWidth
  (const Value: Integer);
begin
  with TdxGanttControlSheetResizeColumnCommand.Create(Controller,
    ViewInfo.Column, Controller.ScaleFactor.Revert(Max(1, Value))) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlSheetRowHeaderWidthResizingObject }

procedure TdxGanttControlSheetRowHeaderWidthResizingObject.SetWidth
  (const Value: Integer);
begin
  with TdxGanttControlSheetChangeRowHeaderWidthCommand.Create(Controller,
    Controller.ScaleFactor.Revert(Max(1, Value))) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlSheetMovingObject }

constructor TdxGanttControlSheetMovingObject.Create
  (AController: TdxGanttControlCustomController;
AColumn: TdxGanttControlSheetColumn);
begin
  inherited Create(AController);
  FColumn := AColumn;
  FFirstVisibleColumnIndex := Controller.FirstVisibleColumnIndex;
end;

function TdxGanttControlSheetMovingObject.CanDrop(const P: TPoint): Boolean;
var
  AIndex: Integer;
  AViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
begin
  AViewInfo := GetHitViewInfo(P);
  Result := (AViewInfo <> nil) and (AViewInfo.Column <> Column);
  if Result then
  begin
    AIndex := GetNewIndex(AViewInfo, P);
    Result := AIndex <> Column.Index;
  end;
  if not Result and Column.RealAllowHide and
    Controller.ViewInfo.Bounds.Contains(P) then
    Result := True;
end;

procedure TdxGanttControlSheetMovingObject.AfterDragAndDrop(Accepted: Boolean);
begin
  inherited AfterDragAndDrop(Accepted);
  Controller.ViewInfo.Invalidate;
end;

procedure TdxGanttControlSheetMovingObject.ApplyChanges(const P: TPoint);
var
  AViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
  AIndex: Integer;
begin
  AViewInfo := GetHitViewInfo(P);
  if AViewInfo = nil then
  begin
    with TdxGanttControlSheetHideColumnCommand.Create(Controller, Column.Index,
      FFirstVisibleColumnIndex) do
      try
        Execute;
      finally
        Free;
      end;
  end
  else
  begin
    AIndex := GetNewIndex(AViewInfo, P);
    with TdxGanttControlSheetMoveColumnCommand.Create(Controller, Column.Index,
      AIndex, FFirstVisibleColumnIndex) do
      try
        Execute;
      finally
        Free;
      end;
  end;
end;

function TdxGanttControlSheetMovingObject.CreateDragImage: TcxDragImage;
var
  AViewInfo: TdxGanttControlSheetCustomViewInfo;
  ATopArrow, ABottomArrow: TRect;
  R: TRect;
begin
  AViewInfo := Controller.ViewInfo;
  Result := TcxDragImage.Create;
  ATopArrow := TcxDragAndDropArrow.CalculateBounds(AViewInfo.Headers[0].Bounds,
    AViewInfo.Headers[0].Bounds, apTop, AViewInfo.ScaleFactor, False);
  ABottomArrow := TcxDragAndDropArrow.CalculateBounds
    (AViewInfo.Headers[0].Bounds, AViewInfo.Headers[0].Bounds, apBottom,
    AViewInfo.ScaleFactor, False);
  Result.SetBounds(0, 0, Max(ATopArrow.Width, ABottomArrow.Width),
    ABottomArrow.Bottom - ATopArrow.Top);
  FTopArrowLocation := cxPointOffset(ATopArrow.Location,
    AViewInfo.Headers[0].Bounds.Location, False);
  Result.Canvas.Lock;
  try
    TcxCustomDragImageAccess(Result).TransparentColor := True;
    TcxCustomDragImageAccess(Result).TransparentColorValue := clWhite;
    Result.Canvas.FillRect(Result.ClientRect, clWhite);
    R := cxRectCenter(Result.BoundsRect, ATopArrow.Size);
    R.MoveToTop(Result.BoundsRect.Top);
    TcxDragAndDropArrow.Draw(Result.Canvas, R, apTop, AViewInfo.ScaleFactor);
    R := cxRectCenter(Result.BoundsRect, ABottomArrow.Size);
    R.MoveToBottom(Result.BoundsRect.Bottom);
    TcxDragAndDropArrow.Draw(Result.Canvas, R, apBottom, AViewInfo.ScaleFactor);
  finally
    Result.Canvas.Unlock;
  end;
end;

procedure TdxGanttControlSheetMovingObject.DragAndDrop(const P: TPoint;
var Accepted: Boolean);
var
  APos: TPoint;
  AViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
begin
  CheckScrolling(P);
  Accepted := CanDrop(P);
  AViewInfo := GetHitViewInfo(P);
  if Accepted and (AViewInfo <> nil) then
  begin
    APos.Y := AViewInfo.Bounds.Top;
    if P.X < (AViewInfo.Bounds.Left + AViewInfo.Bounds.Right) div 2 then
      APos.X := AViewInfo.Bounds.Left
    else
      APos.X := AViewInfo.Bounds.Right;
    if Controller.ViewInfo.Bounds.Contains(APos) then
      ShowDragImage(cxPointOffset(APos, FTopArrowLocation))
    else
      HideDragImage;
  end
  else
    HideDragImage;
  if Accepted and (AViewInfo = nil) and Column.RealAllowHide then
    Screen.Cursor := TdxGanttControlCursors.HideColumn
  else
    inherited DragAndDrop(P, Accepted)
end;

function TdxGanttControlSheetMovingObject.GetController
  : TdxGanttControlSheetController;
begin
  Result := TdxGanttControlSheetController(inherited Controller);
end;

function TdxGanttControlSheetMovingObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxGanttControlSheetMovingObject.GetHitViewInfo(const P: TPoint)
  : TdxGanttControlSheetColumnHeaderViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if HitTest.HitObject is TdxGanttControlSheetColumnHeaderViewInfo then
    Result := TdxGanttControlSheetColumnHeaderViewInfo(HitTest.HitObject)
  else if HitTest.HitObject is TdxGanttControlSheetColumnHeaderFilterButtonViewInfo
  then
    Result := TdxGanttControlSheetColumnHeaderFilterButtonViewInfo
      (HitTest.HitObject).Owner
  else
  begin
    if not Column.RealAllowHide and Controller.ViewInfo.Bounds.Contains(P) then
    begin
      for I := 0 to Controller.ViewInfo.Headers.Count - 1 do
      begin
        if Controller.ViewInfo.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo
        then
        begin
          if (P.X >= Controller.ViewInfo.Headers[I].Bounds.Left) and
            (P.X < Controller.ViewInfo.Headers[I].Bounds.Right) then
            Exit(TdxGanttControlSheetColumnHeaderViewInfo
              (Controller.ViewInfo.Headers[I]));
        end;
      end;
    end;
  end;
end;

function TdxGanttControlSheetMovingObject.GetNewIndex
  (AHitViewInfo: TdxGanttControlSheetColumnHeaderViewInfo;
const P: TPoint): Integer;
begin
  Result := AHitViewInfo.Column.Index;
  if AHitViewInfo.UseRightToLeftAlignment xor
    (P.X >= (AHitViewInfo.Bounds.Left + AHitViewInfo.Bounds.Right) div 2) then
  begin
    if Result < Column.Index then
      Inc(Result)
  end
  else if Result > Column.Index then
    Dec(Result);
end;

{ TdxGanttControlSheetDragHelper }

function TdxGanttControlSheetDragHelper.CalculateResizePoint
  (AViewInfo: TdxGanttControlSheetHeaderViewInfo; const P: TPoint): TPoint;
begin
  Result := P;
  Result.Y := Controller.ViewInfo.Bounds.Top;
  if AViewInfo.UseRightToLeftAlignment then
  begin
    Result.X := Min(Result.X, AViewInfo.Bounds.Right - AViewInfo.GetMinWidth);
    Result.X := Max(Result.X, Controller.ViewInfo.Bounds.Left);
  end
  else
  begin
    Result.X := Max(Result.X, AViewInfo.Bounds.Left + AViewInfo.GetMinWidth);
    Result.X := Min(Result.X, Controller.ViewInfo.Bounds.Right);
  end;
end;

function TdxGanttControlSheetDragHelper.CreateDragAndDropObject
  : TdxGanttControlDragAndDropObject;
begin
  Result := CreateDragAndDropObjectByPoint(FHitPoint);
end;

procedure TdxGanttControlSheetDragHelper.EndDragAndDrop(Accepted: Boolean);
begin
  FHitPoint := cxInvisiblePoint;
end;

function TdxGanttControlSheetDragHelper.StartDragAndDrop
  (const P: TPoint): Boolean;
var
  ADragDropObject: TdxGanttControlDragAndDropObject;
begin
  ADragDropObject := CreateDragAndDropObjectByPoint(P);
  try
    Result := ADragDropObject <> nil;
  finally
    ADragDropObject.Free;
  end;
  if Result then
    FHitPoint := P;
end;

function TdxGanttControlSheetDragHelper.CreateDragAndDropObjectByPoint
  (const P: TPoint): TdxGanttControlDragAndDropObject;
var
  I: Integer;
  AViewInfo: TdxGanttControlSheetHeaderViewInfo;
begin
  for I := 0 to Controller.ViewInfo.Headers.Count - 1 do
  begin
    AViewInfo := Controller.ViewInfo.Headers[I];
    if AViewInfo.IsSizingZone(P) then
    begin
      if Controller.ViewInfo.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo
      then
        Exit(CreateColumnResizingObject(TdxGanttControlSheetColumnHeaderViewInfo
          (AViewInfo)))
      else
        Exit(CreateRowHeaderWidthResizingObject(AViewInfo));
    end
    else if AViewInfo.IsMovingZone(P) then
    begin
      if Controller.ViewInfo.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo
      then
        Exit(CreateMovingObject(TdxGanttControlSheetColumnHeaderViewInfo
          (AViewInfo)));
    end;
  end;
  Result := nil;
end;

procedure TdxGanttControlSheetDragHelper.DoScroll;
var
  AIndex: Integer;
begin
  AIndex := Controller.FirstVisibleColumnIndex;
  if ScrollDirection = dirLeft then
    Dec(AIndex)
  else
    Inc(AIndex);
  Controller.FirstVisibleColumnIndex := AIndex;
end;

function TdxGanttControlSheetDragHelper.GetController
  : TdxGanttControlSheetController;
begin
  Result := TdxGanttControlSheetController(inherited Controller);
end;

function TdxGanttControlSheetDragHelper.GetScrollableArea: TRect;
var
  AHeaderViewInfo: TdxGanttControlSheetHeaderViewInfo;
begin
  Result := Controller.ViewInfo.Bounds;
  AHeaderViewInfo := Controller.ViewInfo.Headers[0];
  if Controller.ViewInfo.UseRightToLeftAlignment then
    Result.Right := AHeaderViewInfo.Bounds.Left
  else
    Result.Left := AHeaderViewInfo.Bounds.Right;
  Result.Top := AHeaderViewInfo.Bounds.Top;
  Result.Bottom := AHeaderViewInfo.Bounds.Bottom;
end;

function TdxGanttControlSheetDragHelper.CreateMovingObject
  (AViewInfo: TdxGanttControlSheetColumnHeaderViewInfo)
  : TdxGanttControlSheetMovingObject;
begin
  Result := TdxGanttControlSheetMovingObject.Create(Controller,
    AViewInfo.Column);
  AViewInfo.Invalidate;
end;

function TdxGanttControlSheetDragHelper.CreateColumnResizingObject
  (AViewInfo: TdxGanttControlSheetColumnHeaderViewInfo)
  : TdxGanttControlSheetColumnResizingObject;
begin
  Result := TdxGanttControlSheetColumnResizingObject.Create(Controller,
    AViewInfo);
end;

function TdxGanttControlSheetDragHelper.CreateRowHeaderWidthResizingObject
  (AViewInfo: TdxGanttControlSheetHeaderViewInfo)
  : TdxGanttControlSheetRowHeaderWidthResizingObject;
begin
  Result := TdxGanttControlSheetRowHeaderWidthResizingObject.Create(Controller,
    AViewInfo);
end;

{ TdxGanttSheetScrollBars }

procedure TdxGanttSheetScrollBars.DoHScroll(ScrollCode: TScrollCode;
var ScrollPos: Integer);
var
  APosition: Integer;
begin
  if ScrollCode = TScrollCode.scEndScroll then
    Exit;
  APosition := Controller.FirstVisibleColumnIndex;
  case ScrollCode of
    TScrollCode.scTrack:
      APosition := ScrollPos;
    TScrollCode.scLineUp:
      Dec(APosition);
    TScrollCode.scLineDown:
      Inc(APosition);
    TScrollCode.scPageUp:
      Dec(APosition, HScrollBar.PageSize);
    TScrollCode.scPageDown:
      Inc(APosition, HScrollBar.PageSize);
  end;
  APosition := Max(0, APosition);
  APosition := Min(HScrollBar.Max - HScrollBar.PageSize + 1, APosition);
  Controller.FirstVisibleColumnIndex := APosition;
  ScrollPos := Controller.FirstVisibleColumnIndex;
end;

function TdxGanttSheetScrollBars.GetController: TdxGanttControlSheetController;
begin
  Result := TdxGanttControlSheetController(inherited Controller);
end;

procedure TdxGanttSheetScrollBars.DoInitHScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
  AVisible: Boolean;
begin
  AVisible := (Controller.ViewInfo <> nil) and
    (Controller.ViewInfo.VisibleColumnCount <
    Controller.Options.RealVisibleColumnCount);

  if AVisible then
  begin
    AMin := 0;
    AMax := Controller.Options.RealVisibleColumnCount - 1;
    APageSize := Max(1, Controller.ViewInfo.VisibleColumnCount);
    APosition := Min(AMax - APageSize + 1, Controller.FirstVisibleColumnIndex);
    SetScrollInfo(sbHorizontal, AMin, AMax, 1, APageSize, APosition,
      True, True);
  end
  else
    Controller.ResetFirstVisibleColumnIndex;
  HScrollBar.Data.Visible := AVisible;
  HScrollBar.UnlimitedTracking := True;
end;

procedure TdxGanttSheetScrollBars.DoInitVScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
  AVisible: Boolean;
begin
  AVisible := (Controller.ViewInfo <> nil) and
    (Controller.ViewInfo.VisibleRowCount > 0);
  if AVisible then
  begin
    AMin := 0;
    AMax := Controller.ViewInfo.DataRows.Count +
      Controller.FirstVisibleRowIndex;
    AMax := Max(AMax, Controller.DataProvider.Count);
    APageSize := Max(1, Controller.ViewInfo.VisibleRowCount);
    APosition := Controller.FirstVisibleRowIndex;
    SetScrollInfo(sbVertical, AMin, AMax, 1, APageSize, APosition, True, True);
  end;
  VScrollBar.Data.Visible := AVisible;
  VScrollBar.UnlimitedTracking := True;
end;

procedure TdxGanttSheetScrollBars.DoVScroll(ScrollCode: TScrollCode;
var ScrollPos: Integer);
var
  APosition: Integer;
begin
  APosition := Controller.FirstVisibleRowIndex;
  case ScrollCode of
    scTrack:
      APosition := ScrollPos;
    scPageUp:
      Dec(APosition, Controller.GetVisibleRowCount);
    scPageDown:
      Inc(APosition, Controller.GetVisibleRowCount);
    scLineUp, scLineDown:
      begin
        if ScrollCode = scLineUp then
          Dec(APosition)
        else
          Inc(APosition, 1);
      end;
  end;
  Controller.FirstVisibleRowIndex := Max(APosition, 0);
  ScrollPos := APosition;
end;

function TdxGanttSheetScrollBars.IsUnlimitedScrolling
  (AScrollKind: TScrollBarKind; ADeltaX, ADeltaY: Integer): Boolean;
begin
  Result := (AScrollKind = sbVertical) and (ADeltaY < 0);
end;

{ TdxGanttSheetQuickCustomizationControl }

procedure TdxGanttSheetQuickCustomizationControl.ShowAllClickHandler
  (Sender: TObject);
var
  ACursor: TCursor;
  I: Integer;
  AChecked: Boolean;
begin
  AChecked := CommandListBox.States[0] = cbsChecked;
  ACursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Popup.Controller.Control.BeginUpdate;
    CheckListBox.BeginUpdate;
    try
      for I := 0 to CheckListBox.Count - 1 do
      begin
        CheckListBox.Checked[I] := AChecked;
        Popup.ChangeColumnVisible
          (TdxGanttControlSheetColumn(CheckListBox.Items[I].Data), AChecked);
      end;
    finally
      CheckListBox.EndUpdate;
      Popup.Controller.Control.EndUpdate;
    end;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TdxGanttSheetQuickCustomizationControl.CheckShowAll;
begin
  if HasCommands then
    CommandListBox.States[0] := GetCheckingAllState;
end;

procedure TdxGanttSheetQuickCustomizationControl.CheckSortItems;
const
  AState: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  if HasCommands then
    CommandListBox.States[1] :=
      AState[Popup.Controller.Options.ColumnQuickCustomizationSorted];
end;

function TdxGanttSheetQuickCustomizationControl.GetCheckingAllState
  : TcxCheckBoxState;
const
  AState: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  I, ACount: Integer;
begin
  Result := cbsUnchecked;
  ACount := 0;
  for I := 0 to CheckListBox.Count - 1 do
  begin
    if CheckListBox.Checked[I] then
      Inc(ACount);
    if (ACount > 0) and (ACount < I + 1) then
    begin
      Result := cbsGrayed;
      Break;
    end;
  end;
  if Result <> cbsGrayed then
    Result := AState[ACount = CheckListBox.Count];
end;

function TdxGanttSheetQuickCustomizationControl.GetPopup
  : TdxGanttSheetColumnQuickCustomizationPopup;
begin
  Result := TdxGanttSheetColumnQuickCustomizationPopup(Owner);
end;

procedure TdxGanttSheetQuickCustomizationControl.KeyDown(var Key: Word;
Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Popup.CloseUp;
  end;
end;

procedure TdxGanttSheetQuickCustomizationControl.PopulateCheckListBox;
var
  I: Integer;
  AColumn: TdxGanttControlSheetColumn;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    CheckListBox.Sorted := False;
    for I := 0 to Popup.Controller.Options.Columns.Count - 1 do
    begin
      AColumn := Popup.Controller.Options.Columns[I];
      CheckListBox.Items.AddObject(AColumn.Caption, AColumn).Checked :=
        AColumn.Visible;
    end;
    CheckListBox.Sorted := Popup.Controller.Options.
      ColumnQuickCustomizationSorted;
    CheckListBox.ItemMoving := not CheckListBox.Sorted;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

procedure TdxGanttSheetQuickCustomizationControl.PopulateCommandListBox;
begin
  AddCommand(cxGetResourceString(@scxQuickCustomizationAllCommandCaption), True,
    ShowAllClickHandler);
  AddCommand(cxGetResourceString(@scxQuickCustomizationSortedCommandCaption),
    True, SortItemsClickHandler);
  CheckSortItems;
  CheckShowAll;
end;

procedure TdxGanttSheetQuickCustomizationControl.SortItemsClickHandler
  (Sender: TObject);
begin
  Popup.Controller.Options.ColumnQuickCustomizationSorted :=
    not Popup.Controller.Options.ColumnQuickCustomizationSorted;
  PopulateCheckListBox;
end;

{ TdxGanttSheetColumnQuickCustomizationPopup }

constructor TdxGanttSheetColumnQuickCustomizationPopup.Create
  (AController: TdxGanttControlSheetController);
begin
  inherited Create(AController.Control);
  FController := AController;
  Owner := AController;
  OwnerParent := AController.Control;
  FCustomizationControl := TdxGanttSheetQuickCustomizationControl.Create(Self);
  CustomizationControl.Style.BorderStyle := cbsNone;
  CustomizationControl.Style.Edges := [];
  CustomizationControl.Style.HotTrack := False;
  CustomizationControl.Style.TransparentBorder := False;
  CustomizationControl.Style.LookAndFeel.MasterLookAndFeel :=
    Controller.Control.LookAndFeel;
  CustomizationControl.CheckListBox.OnAction := CheckListBoxActionHandler;
  CustomizationControl.CheckListBox.OnDragDrop := CheckListBoxDragDropHandler;
  CustomizationControl.CheckListBox.OnItemDragOver :=
    CheckListBoxItemDragOverHandler;
  CustomizationControl.CheckListBox.OnSelectedItemCheckedStateChanged :=
    CheckListSelectedItemCheckedStateChangedHandler;
end;

destructor TdxGanttSheetColumnQuickCustomizationPopup.Destroy;
begin
  FreeAndNil(FCustomizationControl);
  inherited;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.ChangeColumnVisible
  (AColumn: TdxGanttControlSheetColumn; AValue: Boolean);
var
  ACommand: TdxGanttControlSheetCommand;
begin
  if AValue then
    ACommand := TdxGanttControlSheetShowColumnCommand.Create(Controller,
      AColumn.Index)
  else
    ACommand := TdxGanttControlSheetHideColumnCommand.Create(Controller,
      AColumn.Index);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  CustomizationControl.CheckShowAll;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.CheckListBoxActionHandler
  (Sender: TdxCustomListBox; AItemIndex: Integer);
var
  AColumn: TdxGanttControlSheetColumn;
begin
  AColumn := TdxGanttControlSheetColumn
    (TdxQuickCustomizationCheckListBox(Sender).Items[AItemIndex].Data);
  ChangeColumnVisible(AColumn, TdxQuickCustomizationCheckListBox(Sender)
    .Items[AItemIndex].Checked);
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.CheckListBoxDragDropHandler
  (Sender, Source: TObject; X, Y: Integer);

  procedure MoveItems(AItems: TList; AIndex: Integer);
  var
    I: Integer;
  begin
    Controller.Control.BeginUpdate;
    try
      for I := 0 to AItems.Count - 1 do
      begin
        if TdxGanttControlSheetColumn(AItems[I]).Index < AIndex then
          Dec(AIndex);
        with TdxGanttControlSheetMoveColumnCommand.Create(Controller,
          TdxGanttControlSheetColumn(AItems[I]).Index, AIndex) do
          try
            Execute;
          finally
            Free;
          end;
        CustomizationControl.CheckListBox.Items.Move
          (CustomizationControl.CheckListBox.Items.IndexOfObject(AItems[I]
          ), AIndex);
        CustomizationControl.CheckListBox.Selected[AIndex] := True;
        CustomizationControl.CheckListBox.ItemIndex := AIndex;
        Inc(AIndex);
      end;
    finally
      Controller.Control.EndUpdate;
    end;
  end;

var
  AIndex: Integer;
  AItems: TList;
begin
  AIndex := TdxCustomCheckListBoxAccess(CustomizationControl.CheckListBox)
    .GetDragItemInsertionIndex(X, Y);
  if AIndex = -1 then
    Exit;
  if AIndex = CustomizationControl.CheckListBox.Count then
    AIndex := AIndex - 1;

  AItems := TdxCustomCheckListBoxAccess(CustomizationControl.CheckListBox)
    .GetSelectedItems(True);
  CustomizationControl.CheckListBox.BeginUpdate;
  try
    TdxCustomCheckListBoxAccess(CustomizationControl.CheckListBox)
      .Selection.Clear;
    MoveItems(AItems, AIndex);
  finally
    CustomizationControl.CheckListBox.EndUpdate;
    AItems.Free;
  end;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.
  CheckListBoxItemDragOverHandler(AItem: Pointer; var AAccept: Boolean);
begin
  AAccept := not Controller.Options.ColumnQuickCustomizationSorted;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.
  CheckListSelectedItemCheckedStateChangedHandler(Sender: TObject);
var
  I: Integer;
  AItems: TList;
  AShow: Boolean;
begin
  AItems := TdxCustomCheckListBoxAccess(CustomizationControl.CheckListBox)
    .GetSelectedItems(True);
  CustomizationControl.CheckListBox.BeginUpdate;
  try
    with CustomizationControl.CheckListBox do
      AShow := Items[ItemIndex].Checked;
    for I := 0 to AItems.Count - 1 do
    begin
      ChangeColumnVisible(TdxGanttControlSheetColumn(AItems[I]), AShow);
    end;
  finally
    CustomizationControl.CheckListBox.EndUpdate;
    AItems.Free;
  end;
  CustomizationControl.CheckShowAll;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.CloseUp;
begin
  inherited CloseUp;
  CustomizationControl.Clear;
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.InitPopup;
begin
  inherited InitPopup;
  CustomizationControl.Canvas.Font := Font;
  CustomizationControl.CheckListBox.Columns := 0;
  AlignHorz := GetDefaultAlignHorz;
  AlignVert := pavBottom;
  Direction := pdVertical;
  CustomizationControl.Initialize(Self);
end;

procedure TdxGanttSheetColumnQuickCustomizationPopup.Paint;
begin
  DrawFrame;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientBounds);
end;

{ TdxGanttControlSheetController }

constructor TdxGanttControlSheetController.Create(AControl: TdxGanttControlBase;
AOptions: TdxGanttControlSheetOptions);
begin
  inherited Create(AControl);
  FOptions := AOptions;
  FEditingController := CreateEditingController;
  FScrollBars := CreateScrollBars;
end;

destructor TdxGanttControlSheetController.Destroy;
begin
  FreeAndNil(FColumnQuickCustomizationPopup);
  FreeAndNil(FEditingController);
  FreeAndNil(FScrollBars);
  inherited Destroy;
end;

procedure TdxGanttControlSheetController.CalculateBestFit(AColumnHeaderViewInfo
  : TdxGanttControlSheetColumnHeaderViewInfo);
var
  AResult: Integer;
  I: Integer;
  ARow: TdxGanttControlSheetDataRowViewInfo;
  ACell: TdxGanttControlSheetCellCustomViewInfo;
begin
  AResult := AColumnHeaderViewInfo.CalculateBestFit;
  for I := 0 to DataProvider.Count - 1 do
  begin
    ARow := TdxGanttControlSheetDataRowViewInfo.Create(ViewInfo, I,
      DataProvider[I]);
    try
      ACell := AColumnHeaderViewInfo.Column.GetDataCellViewInfoClass.Create
        (ARow, AColumnHeaderViewInfo, AColumnHeaderViewInfo.Column);
      try
        ACell.CalculateLayout;
        ACell.Calculate(TRect.Create(0, 0, MaxInt, MaxInt));
        AResult := Max(AResult, ACell.CalculateBestFit);
      finally
        ACell.Free;
      end;
    finally
      ARow.Free;
    end;
  end;
  AResult := ScaleFactor.Revert(AResult);
  AResult := Max(TdxGanttControlSheetColumn.MinWidth, AResult);
  with TdxGanttControlSheetResizeColumnCommand.Create(Self,
    AColumnHeaderViewInfo.Column, AResult) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlSheetController.CanAutoScroll
  (ADirection: TcxDirection): Boolean;
begin
  Result := ((ADirection = dirLeft) and (FirstVisibleColumnIndex > 0)) or
    ((ADirection = dirRight) and (FirstVisibleColumnIndex +
    ViewInfo.VisibleColumnCount < Options.RealVisibleColumnCount));
end;

function TdxGanttControlSheetController.CreateDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := TdxGanttControlSheetDragHelper.Create(Self);
end;

function TdxGanttControlSheetController.GetGestureClient(const APoint: TPoint)
  : IdxGestureClient;
begin
  Result := FScrollBars;
end;

function TdxGanttControlSheetController.HasItemExpandState
  (AItem: TObject): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlSheetController.HideEditing;
begin
  inherited HideEditing;
  EditingController.HideEdit(False);
end;

procedure TdxGanttControlSheetController.ResetFirstVisibleColumnIndex;
begin
  FFirstVisibleColumnIndex := 0;
end;

function TdxGanttControlSheetController.CreateEditingController
  : TdxGanttControlSheetEditingController;
begin
  Result := TdxGanttControlSheetEditingController.Create(Self);
end;

function TdxGanttControlSheetController.CreateScrollBars
  : TdxGanttSheetScrollBars;
begin
  Result := TdxGanttSheetScrollBars.Create(Self);
end;

function TdxGanttControlSheetController.IsActive: Boolean;
begin
  Result := ViewInfo <> nil;
end;

function TdxGanttControlSheetController.IsEditing: Boolean;
begin
  Result := EditingController.IsEditing;
end;

procedure TdxGanttControlSheetController.DoClick;
begin
  inherited DoClick;
end;

procedure TdxGanttControlSheetController.DoMouseDown(Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
var
  ARowHeaderViewInfo: TdxGanttControlSheetRowHeaderViewInfo;
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if Button = mbMiddle then
    ScrollBars.ProcessControlScrollingOnMiddleButton;
  if HitTest.HitObject is TdxGanttControlSheetCellViewInfo then
  begin
    if not EditingController.IsErrorOnPost then
    begin
      if TdxGanttControlSheetCellViewInfo(HitTest.HitObject).IsFocused and
        not Options.RealAlwaysShowEditor and (Button = mbLeft) and
        not(ssDouble in Shift) and not DragHelper.IsDragging then
        EditingController.ShowEditByMouse
      else
        SetFocusedCell(TdxGanttControlSheetCellViewInfo(HitTest.HitObject));
    end;
  end;
  if not DragHelper.IsDragging then
  begin
    if HitTest.HitObject is TdxGanttControlSheetRowHeaderViewInfo then
    begin
      ARowHeaderViewInfo := TdxGanttControlSheetRowHeaderViewInfo
        (HitTest.HitObject);
      FocusedCell := TPoint.Create(-1, ARowHeaderViewInfo.DataRow.
        Index + FirstVisibleRowIndex);
    end;
    if HitTest.HitObject is TdxGanttControlSheetColumnQuickCustomizationButtonViewInfo
    then
      ShowColumnQuickCustomizationPopup;
  end;
end;

procedure TdxGanttControlSheetController.DoMouseUp(Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseUp(Button, Shift, X, Y);
  if (HitTest.HitObject is TdxGanttControlCustomItemViewInfo) and
    TdxGanttControlCustomItemViewInfoAccess(HitTest.HitObject).HasPressedState
  then
    TdxGanttControlCustomItemViewInfoAccess(HitTest.HitObject).MouseEnter
end;

function TdxGanttControlSheetController.DoMouseWheel(Shift: TShiftState;
AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
begin
  Result := ScrollBars.DoMouseWheel(Shift, AIsIncrement);
end;

procedure TdxGanttControlSheetController.DoKeyDown(var Key: Word;
Shift: TShiftState);
begin
  inherited DoKeyDown(Key, Shift);
  if Key = 0 then
    Exit;
  case Key of
    VK_LEFT, VK_RIGHT:
      if Shift = [] then
        FocusedColumnIndex :=
          Max(0, FocusedCell.X + IfThen((Key = VK_RIGHT)
          xor ViewInfo.UseRightToLeftAlignment, 1, -1));
    VK_HOME:
      begin
        if [ssCtrl] * Shift = [ssCtrl] then
          FocusedRowIndex := 0;
        FocusedColumnIndex := 0;
      end;
    VK_END:
      begin
        if [ssCtrl] * Shift = [ssCtrl] then
          FocusedRowIndex := Max(0, DataProvider.Count - 1);
        FocusedColumnIndex := MaxInt;
      end;
    VK_UP, VK_DOWN, VK_RETURN:
      FocusedRowIndex := Max(0, FocusedCell.Y + IfThen((Key = VK_DOWN) or
        ((Key = VK_RETURN) and not(ssShift in Shift)), 1, -1));
    VK_PRIOR, VK_NEXT:
      FocusedRowIndex := Max(0, FocusedCell.Y + IfThen(Key = VK_NEXT, 1, -1) *
        (GetVisibleRowCount - 1));
    VK_F2:
      if not EditingController.IsEditing then
        EditingController.ShowEdit;
    VK_F4:
      if not EditingController.IsEditing and (FocusedCellViewInfo <> nil) and
        (FocusedCellViewInfo is TdxGanttControlSheetCellViewInfo) and
        (TdxGanttControlSheetCellViewInfo(FocusedCellViewInfo)
        .Column.Properties is TcxCustomDropDownEditProperties) then
        EditingController.ShowEdit;
    VK_ADD, VK_OEM_PLUS:
      if [ssAlt, ssShift] * Shift = [ssAlt, ssShift] then
        ExpandItem;
    VK_SUBTRACT, VK_OEM_MINUS:
      if [ssAlt, ssShift] * Shift = [ssAlt, ssShift] then
        CollapseItem;
    VK_DELETE:
      if not IsEditing then
      begin
        MakeFocusedCellVisible;
        if (Shift = []) and (FocusedColumnIndex >= 0) then
          EditingController.SetValue(Null)
        else if (Shift = [ssCtrl]) or (FocusedColumnIndex = -1) then
          DeleteFocusedItem;
      end;
    VK_INSERT:
      InsertNewDataItem;
  end;
end;

procedure TdxGanttControlSheetController.DoKeyPress(var Key: Char);
begin
  inherited DoKeyPress(Key);
  if not EditingController.IsEditing and (Ord(Key) >= 32) then
    EditingController.ShowEditByKey(Key);
end;

procedure TdxGanttControlSheetController.MakeFocusedCellVisible;
var
  AFirstVisibleColumnIndex, AFirstVisibleRowIndex: Integer;
  ACount: Integer;
  AOffset: Integer;
begin
  AFirstVisibleColumnIndex := FirstVisibleColumnIndex;
  ACount := ViewInfo.VisibleColumnCount;
  if FocusedCell.X < AFirstVisibleColumnIndex then
    AFirstVisibleColumnIndex := Max(0, FocusedCell.X)
  else if FocusedCell.X >= AFirstVisibleColumnIndex + ACount then
    AFirstVisibleColumnIndex := Min(FOptions.RealVisibleColumnCount - 1,
      FocusedCell.X - ACount + 1);

  if FocusedCellViewInfo <> nil then
  begin
    AOffset := 0;
    ACount := 0;
    while (AFirstVisibleColumnIndex < FocusedCell.X) and
      ((not ViewInfo.UseRightToLeftAlignment and
      (FocusedCellViewInfo.Bounds.Right - AOffset > ViewInfo.ClientRect.Right))
      or (ViewInfo.UseRightToLeftAlignment and (FocusedCellViewInfo.Bounds.Left
      + AOffset < ViewInfo.ClientRect.Left))) do
    begin
      AOffset := AOffset + ViewInfo.DataRows
        [FocusedCell.Y - FirstVisibleRowIndex].Cells[ACount].Bounds.Width;
      Inc(ACount);
      Inc(AFirstVisibleColumnIndex);
    end;
  end;

  AFirstVisibleRowIndex := FirstVisibleRowIndex;
  ACount := ViewInfo.DataRows.Count;
  if not ViewInfo.DataRows.Last.IsFullyVisible then
    Dec(ACount);
  if FocusedCell.Y < AFirstVisibleRowIndex then
    AFirstVisibleRowIndex := Max(0, FocusedCell.Y)
  else if FocusedCell.Y >= AFirstVisibleRowIndex + ACount then
    AFirstVisibleRowIndex := FocusedCell.Y - ACount + 1;

  if (AFirstVisibleRowIndex <> FirstVisibleRowIndex) or
    (AFirstVisibleColumnIndex <> FirstVisibleColumnIndex) then
  begin
    Control.BeginUpdate;
    try
      FirstVisibleColumnIndex := Min(FocusedCell.X, AFirstVisibleColumnIndex);
      FirstVisibleRowIndex := AFirstVisibleRowIndex;
    finally
      Control.EndUpdate;
    end;
    if EditingController.IsEditing then
      EditingController.UpdateEditPosition;
  end;
end;

procedure TdxGanttControlSheetController.SetFocusedCell
  (ACell: TdxGanttControlSheetCellViewInfo);
var
  P: TPoint;
begin
  P.X := ACell.Owner.Cells.IndexOf(ACell) + FirstVisibleColumnIndex;
  P.Y := ACell.Owner.Owner.DataRows.IndexOf(ACell.Owner) + FirstVisibleRowIndex;
  FocusedCell := P;
end;

procedure TdxGanttControlSheetController.ValidateFocusedCell;
begin
  FFirstVisibleColumnIndex := Min(FirstVisibleColumnIndex,
    Max(0, Options.RealVisibleColumnCount - 1));
  FFocusedCell.X := Min(FocusedColumnIndex, Options.RealVisibleColumnCount - 1);
end;

procedure TdxGanttControlSheetController.SetFocusedColumnIndex
  (const Value: Integer);
var
  P: TPoint;
begin
  P := FocusedCell;
  P.X := Value;
  FocusedCell := P;
end;

procedure TdxGanttControlSheetController.SetFocusedRowIndex
  (const Value: Integer);
var
  P: TPoint;
begin
  P := FocusedCell;
  P.Y := Value;
  FocusedCell := P;
end;

procedure TdxGanttControlSheetController.CollapseItem;
begin
  with TdxGanttControlSheetCollapseItemCommand.Create(Self) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlSheetController.ExpandItem;
begin
  with TdxGanttControlSheetExpandItemCommand.Create(Self) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlSheetController.ToggleExpandState(AItem: TObject);
begin
  with TdxGanttControlSheetToggleItemExpandStateCommand.Create(Self) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlSheetController.ShowColumnQuickCustomizationPopup;
begin
  EditingController.HideEdit(False);
  if ColumnQuickCustomizationPopup.Visible or FIsColumnQuickCustomizationPopupVisible
  then
    Exit;
  ColumnQuickCustomizationPopup.BiDiMode := Control.BiDiMode;
  if ViewInfo.UseRightToLeftAlignment then
    ColumnQuickCustomizationPopup.AlignHorz :=
      TdxRightToLeftLayoutConverter.ConvertPopupAlignHorz(pahRight)
  else
    ColumnQuickCustomizationPopup.AlignHorz := pahRight;
  FIsColumnQuickCustomizationPopupVisible := True;
  try
    ColumnQuickCustomizationPopup.Popup;
  finally
    FIsColumnQuickCustomizationPopupVisible := False;
  end;
end;

function TdxGanttControlSheetController.GetVisibleRowCount: Integer;
begin
  Result := ViewInfo.VisibleRowCount;
end;

function TdxGanttControlSheetController.ClosePopupWhenSetNil: Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlSheetController.InitPopup
  (APopup: TdxUIElementPopupWindow);
begin
  APopup.BiDiMode := Control.BiDiMode;
  APopup.OwnerParent := Control;
  APopup.Font := Control.Font;
  APopup.LookAndFeel := Control.LookAndFeel;
  APopup.BorderStyle := ViewInfo.LookAndFeelPainter.PopupBorderStyle;
  APopup.OwnerBounds := ViewInfo.GetQuickCustomizationPopupOwnerBounds;
end;

procedure TdxGanttControlSheetController.PopupClosed;
begin
  // do nothing
end;

function TdxGanttControlSheetController.ProcessNCSizeChanged: Boolean;
begin
  Result := ScrollBars.NCSizeChanged;
end;

function TdxGanttControlSheetController.GetColumnQuickCustomizationPopup
  : TdxGanttSheetColumnQuickCustomizationPopup;
begin
  if FColumnQuickCustomizationPopup = nil then
    FColumnQuickCustomizationPopup :=
      TdxGanttSheetColumnQuickCustomizationPopup.Create(Self);
  Result := FColumnQuickCustomizationPopup;
end;

function TdxGanttControlSheetController.GetDataProvider
  : TdxGanttControlSheetCustomDataProvider;
begin
  Result := FOptions.DataProvider;
end;

function TdxGanttControlSheetController.GetDesignHitTest(X, Y: Integer;
Shift: TShiftState): Boolean;
begin
  Result := CanDrag(X, Y);
end;

function TdxGanttControlSheetController.GetFocusedCellViewInfo
  : TdxGanttControlSheetCellCustomViewInfo;
begin
  if ViewInfo = nil then
    Result := nil
  else
    Result := ViewInfo.FocusedCellViewInfo;
end;

function TdxGanttControlSheetController.GetFocusedColumnIndex: Integer;
begin
  Result := FocusedCell.X;
end;

function TdxGanttControlSheetController.GetFocusedDataItem: TObject;
begin
  if (FocusedRowIndex < 0) or (FocusedRowIndex >= DataProvider.Count) then
    Result := nil
  else
    Result := DataProvider.Items[FocusedRowIndex];
end;

function TdxGanttControlSheetController.GetFocusedRowIndex: Integer;
begin
  Result := FocusedCell.Y;
end;

procedure TdxGanttControlSheetController.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  ScrollBars.DoCreateScrollBars;
end;

procedure TdxGanttControlSheetController.DoDblClick;
var
  I: Integer;
begin
  inherited DoDblClick;
  if (HitTest.HitObject is TdxGanttControlSheetColumnHeaderViewInfo) or
    (HitTest.HitObject is TdxGanttControlSheetColumnEmptyHeaderViewInfo) then
  begin
    for I := 0 to ViewInfo.Headers.Count - 1 do
      if ViewInfo.Headers[I] is TdxGanttControlSheetColumnHeaderViewInfo then
      begin
        if TdxGanttControlSheetColumnHeaderViewInfo(ViewInfo.Headers[I])
          .IsSizingZone(HitTest.HitPoint) then
        begin
          CalculateBestFit(TdxGanttControlSheetColumnHeaderViewInfo
            (ViewInfo.Headers[I]));
          Exit;
        end;
      end;
  end;
end;

procedure TdxGanttControlSheetController.DoDestroyScrollBars;
begin
  ScrollBars.DoDestroyScrollBars;
  inherited DoDestroyScrollBars;
end;

function TdxGanttControlSheetController.GetTouchScrollUIOwner
  (const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := ScrollBars;
end;

procedure TdxGanttControlSheetController.InitScrollbars;
begin
  inherited InitScrollbars;
  ScrollBars.InitScrollbars;
end;

procedure TdxGanttControlSheetController.InsertNewDataItem;
begin
  with TdxGanttControlSheetInsertNewItemCommand.Create(Self) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlSheetController.IsPanArea(const APoint: TPoint)
  : Boolean;
var
  R: TRect;
begin
  R := ViewInfo.ClientRect;
  R.Top := ViewInfo.Headers[0].Bounds.Bottom;
  Result := PtInRect(R, APoint);
end;

procedure TdxGanttControlSheetController.UnInitScrollbars;
begin
  inherited UnInitScrollbars;
  ScrollBars.UnInitScrollbars;
end;

function TdxGanttControlSheetController.InternalGetViewInfo
  : TdxGanttControlSheetCustomViewInfo;
begin
  Result := TdxGanttControlSheetCustomViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlSheetController.SetFirstVisibleColumnIndex
  (const Value: Integer);
begin
  if (FirstVisibleColumnIndex <> Value) and (Value >= 0) then
  begin
    ScrollBars.ShowTouchScrollUI;
    FFirstVisibleColumnIndex := Value;
    FOptions.Changed([TdxGanttControlOptionsChangedType.View]);
    if EditingController.IsEditing then
      EditingController.UpdateEditPosition;
    Options.DoFirstVisibleColumnIndexChanged;
  end;
end;

procedure TdxGanttControlSheetController.SetFirstVisibleRowIndex
  (const Value: Integer);
begin
  if (FirstVisibleRowIndex <> Value) and (Value >= 0) then
  begin
    FFirstVisibleRowIndex := Value;
    FOptions.Changed([TdxGanttControlOptionsChangedType.View]);
    if EditingController.IsEditing then
      EditingController.UpdateEditPosition;
    Options.DoFirstVisibleRowIndexChanged;
  end;
end;

procedure TdxGanttControlSheetController.InternalSetFocusedCell
  (const Value: TPoint);
var
  P, AOldValue: TPoint;
  AMakeFocusedCellViewInfoVisible: Boolean;
begin
  P := Value;
  P.X := Max(-1, Min(P.X, Options.RealVisibleColumnCount - 1));
  P.Y := Max(-1, P.Y);
  if not FocusedCell.IsEqual(P) then
  begin
    if EditingController.IsEditing then
      EditingController.HideEdit(True);
    AOldValue := FFocusedCell;
    FFocusedCell := P;
    AMakeFocusedCellViewInfoVisible := (ViewInfo <> nil) and
      (FocusedCellViewInfo = nil);
    if ViewInfo <> nil then
      ViewInfo.ResetFocusedCellViewInfo;
    if AMakeFocusedCellViewInfoVisible then
      ViewInfo.Recalculate;
    if ViewInfo <> nil then
    begin
      MakeFocusedCellVisible;
      HitTest.Recalculate;
    end;
    if Options.RealAlwaysShowEditor then
      EditingController.ShowEdit;
    Control.Invalidate;
    if AOldValue.X <> FFocusedCell.X then
      Options.DoFocusedColumnIndexChanged;
    if AOldValue.Y <> FFocusedCell.Y then
      Options.DoFocusedRowIndexChanged;
  end;
end;

{ TdxGanttControlSheetOptions }

constructor TdxGanttControlSheetOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FColumns := CreateColumns;
end;

destructor TdxGanttControlSheetOptions.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TdxGanttControlSheetOptions.DoChanged
  (AChanges: TdxGanttControlOptionsChangedTypes);
begin
  if Controller <> nil then
    Controller.ValidateFocusedCell;
  inherited DoChanged(AChanges);
end;

procedure TdxGanttControlSheetOptions.DoReset;
begin
  FCellAutoHeight := True;
  FRowHeight := ScaleFactor.Apply(DefaultRowHeight);
  FRowHeaderWidth := ScaleFactor.Apply(DefaultRowHeaderWidth);
  FColumnQuickCustomization := False;
  FAllowColumnHide := False;
  FAllowColumnMove := True;
  FAllowColumnSize := True;
  FAlwaysShowEditor := bDefault;
  Columns.Reset;
  FColumnQuickCustomizationSorted := False;
end;

procedure TdxGanttControlSheetOptions.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlSheetOptions;
begin
  if Source is TdxGanttControlSheetOptions then
  begin
    ASource := TdxGanttControlSheetOptions(Source);
    AlwaysShowEditor := ASource.AlwaysShowEditor;
    RowHeight := ASource.RowHeight;
    RowHeaderWidth := ASource.RowHeaderWidth;
    Columns := ASource.Columns;
    CellAutoHeight := ASource.CellAutoHeight;
    AllowColumnHide := ASource.AllowColumnHide;
    AllowColumnMove := ASource.AllowColumnMove;
    AllowColumnSize := ASource.AllowColumnSize;
    ColumnQuickCustomization := ASource.ColumnQuickCustomization;
  end;
  inherited Assign(Source);
end;

function TdxGanttControlSheetOptions.CreateColumns: TdxGanttControlSheetColumns;
begin
  Result := TdxGanttControlSheetColumns.Create(Self);
end;

function TdxGanttControlSheetOptions.GetControl: TdxGanttControlBase;
begin
  Result := Controller.Control;
end;

function TdxGanttControlSheetOptions.GetRealVisibleColumnCount: Integer;
begin
  Result := Columns.VisibleCount;
end;

function TdxGanttControlSheetOptions.GetRealAlwaysShowEditor: Boolean;
begin
  if FAlwaysShowEditor = bDefault then
    Result := Control.OptionsBehavior.AlwaysShowEditor
  else
    Result := FAlwaysShowEditor = bTrue;
end;

function TdxGanttControlSheetOptions.DoBeforeEdit
  (AColumn: TdxGanttControlSheetColumn): Boolean;
begin
  Result := True;
  if Assigned(OnBeforeEdit) then
    OnBeforeEdit(Self, AColumn, Result);
end;

procedure TdxGanttControlSheetOptions.DoEditValueChanged
  (AColumn: TdxGanttControlSheetColumn);
begin
  if Assigned(OnEditValueChanged) then
    OnEditValueChanged(Self, AColumn)
end;

procedure TdxGanttControlSheetOptions.DoInitEdit
  (AColumn: TdxGanttControlSheetColumn; AEdit: TcxCustomEdit);
begin
  if Assigned(OnInitEdit) then
    OnInitEdit(Self, AColumn, AEdit)
end;

procedure TdxGanttControlSheetOptions.DoFirstVisibleColumnIndexChanged;
begin
  if Assigned(OnFirstVisibleColumnIndexChanged) then
    OnFirstVisibleColumnIndexChanged(Self);
end;

procedure TdxGanttControlSheetOptions.DoFirstVisibleRowIndexChanged;
begin
  if Assigned(OnFirstVisibleRowIndexChanged) then
    OnFirstVisibleRowIndexChanged(Self);
end;

procedure TdxGanttControlSheetOptions.DoFocusedColumnIndexChanged;
begin
  if Assigned(OnFocusedColumnIndexChanged) then
    OnFocusedColumnIndexChanged(Self);
end;

procedure TdxGanttControlSheetOptions.DoFocusedRowIndexChanged;
begin
  if Assigned(OnFocusedRowIndexChanged) then
    OnFocusedRowIndexChanged(Self);
end;

procedure TdxGanttControlSheetOptions.DoColumnPositionChanged
  (AColumn: TdxGanttControlSheetColumn);
begin
  if Assigned(OnColumnPositionChanged) then
    OnColumnPositionChanged(Self, AColumn);
end;

procedure TdxGanttControlSheetOptions.DoColumnSizeChanged
  (AColumn: TdxGanttControlSheetColumn);
begin
  if Controller.ViewInfo <> nil then
    Controller.ViewInfo.CachedDataRowHeight.Clear;
  if Assigned(OnColumnSizeChanged) then
    OnColumnSizeChanged(Self, AColumn);
end;

procedure TdxGanttControlSheetOptions.SetCellAutoHeight(const Value: Boolean);
begin
  if CellAutoHeight <> Value then
  begin
    FCellAutoHeight := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlSheetOptions.SetColumnQuickCustomization
  (const Value: Boolean);
begin
  if ColumnQuickCustomization <> Value then
  begin
    FColumnQuickCustomization := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlSheetOptions.SetColumns(const Value
  : TdxGanttControlSheetColumns);
begin
  FColumns.Assign(Value);
end;

procedure TdxGanttControlSheetOptions.SetRowHeaderWidth(const Value: Integer);
begin
  if (FRowHeaderWidth <> Value) and (Value >= RowHeaderMinWidth) then
  begin
    FRowHeaderWidth := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlSheetOptions.SetRowHeight(const Value: Integer);
begin
  if (FRowHeight <> Value) and (Value > 0) then
  begin
    FRowHeight := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

end.
