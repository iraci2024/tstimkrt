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

unit dxFilterBox;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Controls, Windows, Graphics, cxClasses, dxCore, dxCoreClasses,
  cxGeometry, cxGraphics,
  cxLookAndFeelPainters, cxFilter, cxCustomData, dxUIElementPopupWindow,
  cxListBox, cxControls,
  Generics.Defaults, Generics.Collections;

type
  TdxFilterBoxMRUItemsPopup = class;
  TdxFilterBoxTokenCriteriaCustomItemViewInfo = class;
  TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo = class;
  TdxFilterBoxTokenCriteriaItemViewInfo = class;
  TdxFilterBoxTokenCriteriaListViewInfo = class;
  TdxFilterBoxTokenCriteriaViewInfo = class;
  TdxFilterBoxTokenCriteriaItemSubOperators = class;
  TdxFilterBoxTokenCriteriaItem = class;
  TdxFilterBoxTokenCriteria = class;

  TcxFilterBoxPosition = (fpTop, fpBottom);
  TcxFilterBoxButtonAlignment = (fbaLeft, fbaRight);
  TcxFilterBoxVisible = (fvNever, fvNonEmpty, fvAlways);
  TdxFilterBoxTokenCriteriaExtOperatorKind = (eokNone, eokDateList, eokFromTo);
  // for internal use only

  { IdxFilterBoxTokenCriteriaOptions }

  IdxFilterBoxTokenCriteriaOptions = interface // for internal use only
    ['{B9930D9B-B4B4-4713-BAFC-487E59D71E61}']
    function GetItemRemoval: Boolean;

    property ItemRemoval: Boolean read GetItemRemoval;
  end;

  { IdxFilterBoxTokenCriteriaViewInfoOwner }

  IdxFilterBoxTokenCriteriaViewInfoOwner = interface // for internal use only
    ['{DD64CF3F-2CBF-4EFA-8C12-5507EE5B29C2}']
    function GetControl: TcxControl;
    function GetCriteria: TdxFilterBoxTokenCriteria;
    function GetOptions: IdxFilterBoxTokenCriteriaOptions;
    procedure GetViewParams(var AParams: TcxViewParams);

    property Control: TcxControl read GetControl;
    property Criteria: TdxFilterBoxTokenCriteria read GetCriteria;
    property Options: IdxFilterBoxTokenCriteriaOptions read GetOptions;
  end;

  { TdxFilterBoxMRUItem }

  TdxFilterBoxMRUItem = class(TcxMRUItem)
  private
    FFilter: TcxDataFilterCriteria;

    function GetCaption: string;
    function GetFilterStream(AFilter: TcxDataFilterCriteria): TMemoryStream;
  protected
    function StreamEquals(AStream: TMemoryStream): Boolean;
  public
    constructor Create(AFilter: TcxDataFilterCriteria);
    destructor Destroy; override;

    procedure AssignTo(AFilter: TcxDataFilterCriteria);

    function Equals(AItem: TcxMRUItem): Boolean; override;
    function FilterEquals(AFilter: TcxDataFilterCriteria): Boolean;
    function GetStream: TMemoryStream;

    property Caption: string read GetCaption;
    property Filter: TcxDataFilterCriteria read FFilter;
  end;

  TdxFilterBoxMRUItemClass = class of TdxFilterBoxMRUItem;

  { TdxFilterBoxMRUItems }

  TdxFilterBoxMRUItems = class(TcxMRUItems)
  private
    FVisibleItems: TdxFastList;

    function GetItem(Index: Integer): TdxFilterBoxMRUItem; inline;
    function GetVisibleCount: Integer; inline;
    function GetVisibleItem(Index: Integer): TdxFilterBoxMRUItem; inline;
  protected
    procedure AddCurrentFilter; virtual;
    function GetCurrentFilter: TcxDataFilterCriteria; virtual;
    function GetItemClass: TdxFilterBoxMRUItemClass; virtual;
    procedure DeleteEmptyItems; virtual;
    procedure FilterChanged; virtual;
    procedure RefreshVisibleItemsList; virtual;
    procedure SetMaxCount(AMaxCount: Integer); virtual;
    procedure VisibleCountChanged(APrevVisibleCount: Integer); virtual;

    property CurrentFilter: TcxDataFilterCriteria read GetCurrentFilter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(AFilter: TcxDataFilterCriteria);

    property Items[Index: Integer]: TdxFilterBoxMRUItem read GetItem; default;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TdxFilterBoxMRUItem
      read GetVisibleItem;
  end;

  TdxFilterBoxMRUItemsClass = class of TdxFilterBoxMRUItems;

  { TdxFilterBoxMRUItemsPopupListBox }

  TdxFilterBoxMRUItemsPopupListBox = class(TdxCustomCheckListBox)
  strict private
    function GetPopup: TdxFilterBoxMRUItemsPopup;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function NeedHotTrack: Boolean; override;
  public
    constructor Create(APopup: TdxFilterBoxMRUItemsPopup); reintroduce; virtual;

    property Popup: TdxFilterBoxMRUItemsPopup read GetPopup;
  end;

  TdxFilterBoxMRUItemsPopupListBoxClass = class of
    TdxFilterBoxMRUItemsPopupListBox;

  { TdxFilterBoxMRUItemsPopup }

  TdxFilterBoxMRUItemsPopup = class(TdxUIElementPopupWindow)
  private
    FListBox: TdxFilterBoxMRUItemsPopupListBox;

    procedure ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
  protected
    procedure ApplyFilter(AItemIndex: Integer); virtual;
    procedure AddFilterMRUItems; virtual;
    procedure ApplyFilterMRUItem(AItemIndex: Integer); virtual;
    function GetListBoxClass: TdxFilterBoxMRUItemsPopupListBoxClass; virtual;
    function GetMRUItemCount: Integer; virtual;
    function GetMRUItems: TdxFilterBoxMRUItems; virtual;
    function GetTextOffsetHorz: Integer; virtual;
    procedure InitListBox; virtual;
    procedure InitPopup; override;
    procedure UpdateInnerControlsHeight(var AClientHeight: Integer); override;

    property ListBox: TdxFilterBoxMRUItemsPopupListBox read FListBox;
    property MRUItems: TdxFilterBoxMRUItems read GetMRUItems;
  public
    constructor Create(AOwnerControl: TWinControl); override;

    property TextOffsetHorz: Integer read GetTextOffsetHorz;
  end;

  TdxFilterBoxMRUItemsPopupClass = class of TdxFilterBoxMRUItemsPopup;

  { TdxFilterBoxTokenCriteriaOptions }

  TdxFilterBoxTokenCriteriaOptions = class // for internal use only
  strict private
    FItemRemoval: Boolean;
  public
    constructor Create(AOptions: IdxFilterBoxTokenCriteriaOptions); virtual;

    property ItemRemoval: Boolean read FItemRemoval;
  end;

  { TdxFilterBoxTokenCriteriaCustomElementViewInfo }

  TdxFilterBoxTokenCriteriaCustomElementViewInfo = class(TInterfacedPersistent)
  // for internal use only
  strict private
    FBounds: TRect;
    FHeight: Integer;
    FIsHeightCalculated: Boolean;
    FIsRightToLeftConverted: Boolean;
    FIsViewParamsCalculated: Boolean;
    FIsWidthCalculated: Boolean;
    FViewParams: TcxViewParams;
    FWidth: Integer;

    function GetViewParams: TcxViewParams;
  protected
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); overload; virtual;
    function CalculateHeight: Integer; virtual; abstract;
    function CalculateViewParams: TcxViewParams; virtual; abstract;
    function CalculateWidth: Integer; virtual; abstract;
    procedure DoPaint(ACanvas: TcxCanvas); virtual; abstract;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetControl: TcxControl; virtual; abstract;
    function GetCriteria: TdxFilterBoxTokenCriteria; virtual; abstract;
    function GetHeight: Integer; virtual;
    function GetPainter: TcxCustomLookAndFeelPainter; virtual; abstract;
    function GetScaleFactor: TdxScaleFactor; virtual; abstract;
    function GetWidth: Integer; virtual;
    function HasPoint(const APoint: TPoint): Boolean;
    procedure Invalidate; overload;
    procedure Invalidate(const ARect: TRect); overload;
    function IsVisibleForPainting: Boolean; virtual;

    property Bounds: TRect read FBounds;
    property Control: TcxControl read GetControl;
    property Height: Integer read GetHeight;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewParams: TcxViewParams read GetViewParams;
    property Width: Integer read GetWidth;
  public
    procedure Calculate(const ABounds: TRect); overload;
    procedure Paint(ACanvas: TcxCanvas);
    procedure RightToLeftConversion(const ABounds: TRect);

    property Criteria: TdxFilterBoxTokenCriteria read GetCriteria;
  end;

  { TdxFilterBoxTokenCriteriaElementViewInfo }

  TdxFilterBoxTokenCriteriaElementViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomElementViewInfo, IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2) // for internal use only
  strict private
    FBackgroundBounds: TRect;
    FCriteriaViewInfo: TdxFilterBoxTokenCriteriaViewInfo;
    FIsTextAssigned: Boolean;
    FIsTextSizeCalculated: Boolean;
    FState: TcxButtonState;
    FText: string;
    FTextBounds: TRect;
    FTextSize: TSize;

    function CalculateTextSize: TSize;
    function GetTextFormat: Cardinal;
    function GetTextSize: TSize;
    function GetTextValue: string;

    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
  protected
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CalculateBackgroundBounds: TRect; virtual;
    function CalculateHeight: Integer; override;
    function CalculateTextBounds: TRect; virtual;
    function CalculateViewParams: TcxViewParams; override;
    function CalculateWidth: Integer; override;
    procedure Click; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    function GetControl: TcxControl; override;
    function GetCriteria: TdxFilterBoxTokenCriteria; override;
    function GetHitInfo(const APoint: TPoint)
      : TdxFilterBoxTokenCriteriaElementViewInfo; virtual;
    function GetPainter: TcxCustomLookAndFeelPainter; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetState: TcxButtonState; virtual;
    function GetText: string; virtual;
    function GetTextMargins: TRect; virtual;
    function HasHitInfo: Boolean; virtual;
    procedure InvalidateOnStateChanged; virtual;
    procedure MouseLeave; virtual;
    function NeedDrawBackground: Boolean; virtual;
    function NeedInvalidateOnStateChanged: Boolean; virtual;
    procedure StateChanged; virtual;
    procedure UpdateState; virtual;

    property BackgroundBounds: TRect read FBackgroundBounds;
    property CriteriaViewInfo: TdxFilterBoxTokenCriteriaViewInfo
      read FCriteriaViewInfo;
    property State: TcxButtonState read FState;
    property Text: string read GetTextValue;
    property TextBounds: TRect read FTextBounds;
    property TextFormat: Cardinal read GetTextFormat;
    property TextMargins: TRect read GetTextMargins;
    property TextSize: TSize read GetTextSize;
  public
    constructor Create(ACriteriaViewInfo
      : TdxFilterBoxTokenCriteriaViewInfo); virtual;
    destructor Destroy; override;
  end;

  { TdxFilterBoxTokenCriteriaIndentViewInfo }

  TdxFilterBoxTokenCriteriaIndentViewInfo = class
    (TdxFilterBoxTokenCriteriaElementViewInfo) // for internal use only
  strict private
    FWidth: Integer;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
  public
    constructor Create(ACriteriaViewInfo: TdxFilterBoxTokenCriteriaViewInfo;
      AWidth: Integer); reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaCustomItemElementViewInfo }

  TdxFilterBoxTokenCriteriaCustomItemElementViewInfo = class
    (TdxFilterBoxTokenCriteriaElementViewInfo) // for internal use only
  strict private
    FCriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo;
  protected
    property CriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo
      read FCriteriaItemViewInfo;
  public
    constructor Create(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaCustomItemViewInfo); reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaRemoveButtonViewInfo }

  TdxFilterBoxTokenCriteriaRemoveButtonViewInfo = class
    (TdxFilterBoxTokenCriteriaElementViewInfo) // for internal use only
  strict private
    FAreaViewInfo: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo;
    FImageBounds: TRect;

    function GetDrawState: TcxButtonState;
    function GetImageMargin: TRect;
    function GetImageSize: TSize;
  protected
    procedure Calculate(ALeftBound: Integer; ATopBound: Integer;
      AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure Click; override;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function HasHitInfo: Boolean; override;
    function IsVisibleForPainting: Boolean; override;
    function NeedInvalidateOnStateChanged: Boolean; override;
    procedure StateChanged; override;

    property AreaViewInfo: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo
      read FAreaViewInfo;
    property DrawState: TcxButtonState read GetDrawState;
    property ImageBounds: TRect read FImageBounds;
    property ImageMargin: TRect read GetImageMargin;
    property ImageSize: TSize read GetImageSize;
  public
    constructor Create(AAreaViewInfo
      : TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo);
      reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo }

  TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomItemElementViewInfo)
  // for internal use only
  strict private
    FButton: TdxFilterBoxTokenCriteriaRemoveButtonViewInfo;
  protected
    procedure Calculate(ALeftBound: Integer; ATopBound: Integer;
      AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure CalculateButton; virtual;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CreateButton
      : TdxFilterBoxTokenCriteriaRemoveButtonViewInfo; virtual;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitInfo(const APoint: TPoint)
      : TdxFilterBoxTokenCriteriaElementViewInfo; override;
    function HasHitInfo: Boolean; override;
    function NeedInvalidateOnStateChanged: Boolean; override;

    property Button: TdxFilterBoxTokenCriteriaRemoveButtonViewInfo read FButton;
  public
    constructor Create(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaCustomItemViewInfo;
      ANeedBindWithItem: Boolean); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TdxFilterBoxTokenCriteriaItemElementViewInfo }

  TdxFilterBoxTokenCriteriaItemElementViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomItemElementViewInfo)
  // for internal use only
  strict private
    function GetCriteriaItemViewInfo: TdxFilterBoxTokenCriteriaItemViewInfo;
  protected
    property CriteriaItemViewInfo: TdxFilterBoxTokenCriteriaItemViewInfo
      read GetCriteriaItemViewInfo;
  end;

  { TdxFilterBoxTokenCriteriaItemCaptionViewInfo }

  TdxFilterBoxTokenCriteriaItemCaptionViewInfo = class
    (TdxFilterBoxTokenCriteriaItemElementViewInfo) // for internal use only
  protected
    function CalculateViewParams: TcxViewParams; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    function GetText: string; override;
    function GetTextMargins: TRect; override;
    function NeedDrawBackground: Boolean; override;
  end;

  { TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo }

  TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaItemElementViewInfo) // for internal use only
  protected
    function CalculateViewParams: TcxViewParams; override;
    function GetTextMargins: TRect; override;
  end;

  { TdxFilterBoxTokenCriteriaItemOperatorViewInfo }

  TdxFilterBoxTokenCriteriaItemOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo)
  // for internal use only
  strict private
    FImageSize: TSize;
    FIsImageSizeCalculated: Boolean;
    FSubIndex: Integer;

    function GetImageSize: TSize;
    function GetKind: TcxFilterOperatorKind;
    function GetOperator: TcxFilterOperator;
  protected
    function CalculateHeight: Integer; override;
    function CalculateImageSize: TSize; virtual;
    function CalculateWidth: Integer; override;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawImage(ACanvas: TcxCanvas); virtual;
    function GetText: string; override;
    function NeedDrawAsImage: Boolean; virtual;
    function NeedDrawBackground: Boolean; override;

    property ImageSize: TSize read GetImageSize;
    property Kind: TcxFilterOperatorKind read GetKind;
    property &Operator: TcxFilterOperator read GetOperator;
    property SubIndex: Integer read FSubIndex write FSubIndex;
  public
    constructor Create(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaCustomItemViewInfo); override;
  end;

  { TdxFilterBoxTokenCriteriaItemAndOperatorViewInfo }

  TdxFilterBoxTokenCriteriaItemAndOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo)
  // for internal use only
  protected
    function GetText: string; override;
  end;

  { TdxFilterBoxTokenCriteriaItemToOperatorViewInfo }

  TdxFilterBoxTokenCriteriaItemToOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo)
  protected
    function GetText: string; override;
  end;

  { TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo }

  TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo = class
    (TdxFilterBoxTokenCriteriaItemElementViewInfo) // for internal use only
  strict private
    FDisplayValue: string;
  protected
    function CalculateViewParams: TcxViewParams; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    function GetText: string; override;
    function GetTextMargins: TRect; override;
    function NeedDrawBackground: Boolean; override;
  public
    constructor Create(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaItemViewInfo; const ADisplayValue: string);
      reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaListElementViewInfo }

  TdxFilterBoxTokenCriteriaListElementViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomItemElementViewInfo)
  // for internal use only
  strict private
    function GetCriteriaItemViewInfo: TdxFilterBoxTokenCriteriaListViewInfo;
  protected
    property CriteriaItemViewInfo: TdxFilterBoxTokenCriteriaListViewInfo
      read GetCriteriaItemViewInfo;
  end;

  { TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo }

  TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaListElementViewInfo)
  protected
    function CalculateViewParams: TcxViewParams; override;
  end;

  { TdxFilterBoxTokenCriteriaListNotOperatorViewInfo }

  TdxFilterBoxTokenCriteriaListNotOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo)
  // for internal use only
  protected
    function GetText: string; override;
  end;

  { TdxFilterBoxTokenCriteriaListBoolOperatorViewInfo }

  TdxFilterBoxTokenCriteriaListBoolOperatorViewInfo = class
    (TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo)
  // for internal use only
  protected
    function GetText: string; override;
  end;

  { TdxFilterBoxTokenCriteriaCollectionViewInfo }

  TdxFilterBoxTokenCriteriaCollectionViewInfo = class
    (TdxFilterBoxTokenCriteriaElementViewInfo) // for internal use only
  strict private
    FContentBounds: TRect;
    FElements: TObjectList<TdxFilterBoxTokenCriteriaElementViewInfo>;

    function GetContentMargins: TRect;
  protected
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CalculateContentBounds: TRect; virtual;
    procedure CalculateElements; virtual;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function DoGetContentMargins: TRect; virtual;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitInfo(const APoint: TPoint)
      : TdxFilterBoxTokenCriteriaElementViewInfo; override;
    procedure Populate; virtual;

    property ContentBounds: TRect read FContentBounds;
    property ContentMargins: TRect read GetContentMargins;
    property Elements: TObjectList<TdxFilterBoxTokenCriteriaElementViewInfo>
      read FElements;
  public
    constructor Create(ACriteriaViewInfo
      : TdxFilterBoxTokenCriteriaViewInfo); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
  end;

  { TdxFilterBoxTokenCriteriaCustomItemViewInfo }

  TdxFilterBoxTokenCriteriaCustomItemViewInfo = class
    (TdxFilterBoxTokenCriteriaCollectionViewInfo) // for internal use only
  strict private
  type
    TBordersType = (None, Brackets, RemovingArea);
  strict private
    FCriteriaItem: TcxCustomFilterCriteriaItem;
    FRemoveButton: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo;

    function GetBordersType: TBordersType;
    function GetElementsIndent: Integer;
  protected
    procedure AddCloseBracketIndent; virtual;
    procedure AddIndent(AWidth: Integer); virtual;
    procedure AddOpenBracketIndent; virtual;
    procedure BindRemoveButton(AButton
      : TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo); virtual;
    function DoGetContentMargins: TRect; override;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DrawBorders(ACanvas: TcxCanvas); virtual;
    procedure DrawBrackets(ACanvas: TcxCanvas); virtual;
    procedure DrawRemovingArea(ACanvas: TcxCanvas); virtual;
    function HasBrackets: Boolean; virtual;
    function HasHitInfo: Boolean; override;
    function HasRemoveButton: Boolean; virtual;
    procedure InvalidateOnStateChanged; override;
    function NeedRemoveButton: Boolean; virtual;
    function NeedInvalidateOnStateChanged: Boolean; override;
    procedure Populate; override;
    procedure PopulateContent; virtual;

    property BordersType: TBordersType read GetBordersType;
    property CriteriaItem: TcxCustomFilterCriteriaItem read FCriteriaItem;
    property ElementsIndent: Integer read GetElementsIndent;
  public
    constructor Create(ACriteriaViewInfo: TdxFilterBoxTokenCriteriaViewInfo;
      ACriteriaItem: TcxCustomFilterCriteriaItem); reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaItemViewInfo }

  TdxFilterBoxTokenCriteriaItemViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomItemViewInfo) // for internal use only
  strict private
    function GetCaption: string;
    function GetCriteriaItem: TdxFilterBoxTokenCriteriaItem;
  protected
    procedure AddAndOperator; virtual;
    procedure AddDisplayValue(const ADisplayValue: string); virtual;
    procedure AddDisplayValues; virtual;
    procedure AddDisplayValueSeparator; virtual;
    procedure AddItemCaption; virtual;
    function AddOperator(ASubIndex: Integer = -1)
      : TdxFilterBoxTokenCriteriaItemOperatorViewInfo; virtual;
    procedure AddToOperator; virtual;
    procedure PopulateContent; override;

    property Caption: string read GetCaption;
    property CriteriaItem: TdxFilterBoxTokenCriteriaItem read GetCriteriaItem;
  end;

  { TdxFilterBoxTokenCriteriaListViewInfo }

  TdxFilterBoxTokenCriteriaListViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomItemViewInfo) // for internal use only
  strict private
    FAuxiliary: Boolean;
    FRemoveButtonWidth: Integer;
    FRemoveButtonWidthCalculated: Boolean;

    function GetCriteriaItem: TcxFilterCriteriaItemList;
    function GetNotOperatorLeftIndent: Integer;
    function GetNotOperatorRightIndent: Integer;
    function GetRemoveButtonWidth: Integer;
  protected
    procedure AddBoolOperator; virtual;
    procedure AddCloseBracketIndent; override;
    procedure AddRemoveButton(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaCustomItemViewInfo); virtual;
    function AddItem(ACriteriaItem: TdxFilterBoxTokenCriteriaItem)
      : TdxFilterBoxTokenCriteriaItemViewInfo; virtual;
    function AddItemList(ACriteriaItem: TcxFilterCriteriaItemList;
      AAuxiliary: Boolean = False)
      : TdxFilterBoxTokenCriteriaListViewInfo; virtual;
    procedure AddNotOperator; virtual;
    procedure AddOpenBracketIndent; override;
    function ContainsNotOperator: Boolean; virtual;
    function CreateRemoveButton(ACriteriaItemViewInfo
      : TdxFilterBoxTokenCriteriaCustomItemViewInfo;
      ANeedBindWithItem: Boolean = True)
      : TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo; virtual;
    function GetText: string; override;
    function GetTextMargins: TRect; override;
    function HasBrackets: Boolean; override;
    procedure PopulateAuxiliaryItemList; virtual;
    procedure PopulateContent; override;

    property Auxiliary: Boolean read FAuxiliary;
    property CriteriaItem: TcxFilterCriteriaItemList read GetCriteriaItem;
    property NotOperatorLeftIndent: Integer read GetNotOperatorLeftIndent;
    property NotOperatorRightIndent: Integer read GetNotOperatorRightIndent;
    property RemoveButtonWidth: Integer read GetRemoveButtonWidth;
  public
    constructor Create(ACriteriaViewInfo: TdxFilterBoxTokenCriteriaViewInfo;
      AItemList: TcxFilterCriteriaItemList; AAuxiliary: Boolean = False);
      reintroduce; virtual;
  end;

  { TdxFilterBoxTokenCriteriaViewInfo }

  TdxFilterBoxTokenCriteriaViewInfo = class
    (TdxFilterBoxTokenCriteriaCustomElementViewInfo) // for internal use only
  strict private
    FControl: TcxControl;
    FCriteria: TdxFilterBoxTokenCriteria;
    FHotElement: TdxFilterBoxTokenCriteriaElementViewInfo;
    FOptions: TdxFilterBoxTokenCriteriaOptions;
    FOwner: IdxFilterBoxTokenCriteriaViewInfoOwner;
    FPainter: TcxCustomLookAndFeelPainter;
    FPainterParams: TdxFilterTokenParams;
    FPainterParamsCalculated: Boolean;
    FPressedElement: TdxFilterBoxTokenCriteriaElementViewInfo;
    FRootViewInfo: TdxFilterBoxTokenCriteriaListViewInfo;
    FScaleFactor: TdxScaleFactor;

    procedure AdjustTokenPadding(var APadding: TRect);
    function GetContentHeight: Integer;
    function GetContentMargins: TRect;
    function GetContentWidth: Integer;
    function GetPainterParams: TdxFilterTokenParams;
    procedure SetHotElement(AValue: TdxFilterBoxTokenCriteriaElementViewInfo);
    procedure SetPressedElement
      (AValue: TdxFilterBoxTokenCriteriaElementViewInfo);
  protected
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CalculateHeight: Integer; override;
    procedure CalculateRootViewInfo; virtual;
    function CalculateViewParams: TcxViewParams; override;
    function CalculateWidth: Integer; override;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetControl: TcxControl; override;
    function GetCriteria: TdxFilterBoxTokenCriteria; override;
    function GetHeight: Integer; override;
    function GetPainter: TcxCustomLookAndFeelPainter; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetWidth: Integer; override;
    procedure MouseLeave; virtual;

    property ContentMargins: TRect read GetContentMargins;
    property HotElement: TdxFilterBoxTokenCriteriaElementViewInfo
      read FHotElement write SetHotElement;
    property Options: TdxFilterBoxTokenCriteriaOptions read FOptions;
    property Owner: IdxFilterBoxTokenCriteriaViewInfoOwner read FOwner;
    property PainterParams: TdxFilterTokenParams read GetPainterParams;
    property PressedElement: TdxFilterBoxTokenCriteriaElementViewInfo
      read FPressedElement write SetPressedElement;
    property RootViewInfo: TdxFilterBoxTokenCriteriaListViewInfo
      read FRootViewInfo;
  public
    constructor Create(AOwner: IdxFilterBoxTokenCriteriaViewInfoOwner); virtual;
    destructor Destroy; override;

    function HasMouse(const APoint: TPoint): Boolean;

    procedure MouseDown(const APoint: TPoint); virtual;
    procedure MouseMove(const APoint: TPoint); virtual;
    procedure MouseUp(const APoint: TPoint); virtual;

    property ContentHeight: Integer read GetContentHeight;
    property ContentWidth: Integer read GetContentWidth;
  end;

  { TdxFilterBoxTokenCriteriaFromToOperator }

  TdxFilterBoxTokenCriteriaFromToOperator = class(TcxFilterOperator)
  // for internal use only
  public
    function DisplayText: string; override;
  end;

  { TdxFilterBoxTokenCriteriaDateListOperator }

  TdxFilterBoxTokenCriteriaDateListOperator = class(TcxFilterOperator)
  // for internal use only
  public
    function DisplayText: string; override;
    function IsUnary: Boolean; override;
  end;

  { TdxFilterBoxTokenCriteriaItemSubOperator }

  TdxFilterBoxTokenCriteriaItemSubOperator = class
  strict private
    FKind: TcxFilterOperatorKind;
    FOperator: TcxFilterOperator;
    FOwner: TdxFilterBoxTokenCriteriaItemSubOperators;
  protected
    function CreateOperator: TcxFilterOperator; virtual;

    property Owner: TdxFilterBoxTokenCriteriaItemSubOperators read FOwner;
  public
    constructor Create(AOwner: TdxFilterBoxTokenCriteriaItemSubOperators;
      AKind: TcxFilterOperatorKind);
    destructor Destroy; override;

    function Kind: TcxFilterOperatorKind;
    function &Operator: TcxFilterOperator;
  end;

  { TdxFilterBoxTokenCriteriaItemSubOperators }

  TdxFilterBoxTokenCriteriaItemSubOperators = class
    (TObjectList<TdxFilterBoxTokenCriteriaItemSubOperator>)
  strict private
    FItem: TdxFilterBoxTokenCriteriaItem;
  protected
    function CreateSubOperator(AKind: TcxFilterOperatorKind)
      : TdxFilterBoxTokenCriteriaItemSubOperator; virtual;

    property Item: TdxFilterBoxTokenCriteriaItem read FItem;
  public
    constructor Create(AItem: TdxFilterBoxTokenCriteriaItem); virtual;

    procedure Add(AKind: TcxFilterOperatorKind);
    function Contains(AKind: TcxFilterOperatorKind): Boolean;
  end;

  { TdxFilterBoxTokenCriteriaItem }

  TdxFilterBoxTokenCriteriaItem = class(TcxDataFilterCriteriaItem)
  // for internal use only
  strict private
    FExtOperatorKind: TdxFilterBoxTokenCriteriaExtOperatorKind;
    FSubOperators: TdxFilterBoxTokenCriteriaItemSubOperators;

    procedure SetExtOperatorKind
      (AValue: TdxFilterBoxTokenCriteriaExtOperatorKind);
  protected
    procedure AddSubOperator(AKind: TcxFilterOperatorKind);
    procedure AssignSubOperators(ASource
      : TdxFilterBoxTokenCriteriaItemSubOperators); virtual;
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;

    property ExtOperatorKind: TdxFilterBoxTokenCriteriaExtOperatorKind
      read FExtOperatorKind write SetExtOperatorKind;
    property SubOperators: TdxFilterBoxTokenCriteriaItemSubOperators
      read FSubOperators;
  public
    constructor Create(AOwner: TcxFilterCriteriaItemList; AItemLink: TObject;
      AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
      const ADisplayValue: string); override;
    destructor Destroy; override;
  end;

  { TdxFilterBoxTokenCriteriaCustomMerger }

  TdxFilterBoxTokenCriteriaCustomMerger = class // for inernal use only
  strict private
    procedure LoadCriteria(ACriteria, ASourceCriteria
      : TdxFilterBoxTokenCriteria);
    procedure LoadList(AList, ASourceList: TcxFilterCriteriaItemList);
  protected
    function CloneExpressionItem(AItem: TcxFilterCriteriaItem;
      AParent: TcxFilterCriteriaItemList): TcxFilterCriteriaItem;
    function CloneItem(AItem: TdxFilterBoxTokenCriteriaItem;
      AParent: TcxFilterCriteriaItemList): TdxFilterBoxTokenCriteriaItem;
    function CloneList(AList: TcxFilterCriteriaItemList;
      AParent: TcxFilterCriteriaItemList): TcxFilterCriteriaItemList;
    procedure PopulateMergedCriteria(ACriteria, ASourceCriteria
      : TdxFilterBoxTokenCriteria; out AHasMerge: Boolean); virtual;
    procedure PopulateMergedList(AList, ASourceList: TcxFilterCriteriaItemList;
      out AHasMerge: Boolean); virtual; abstract;
  public
    function Merge(ACriteria: TdxFilterBoxTokenCriteria): Boolean;
  end;

  TdxFilterBoxTokenCriteriaCustomMergerClass = class of
    TdxFilterBoxTokenCriteriaCustomMerger;

  { TdxFilterBoxTokenCriteriaListMerger }

  TdxFilterBoxTokenCriteriaListMerger = class
    (TdxFilterBoxTokenCriteriaCustomMerger) // for inernal use only
  strict private
    function NeedMerge(AList, AParentList: TcxFilterCriteriaItemList): Boolean;
  protected
    procedure PopulateMergedCriteria(ACriteria, ASourceCriteria
      : TdxFilterBoxTokenCriteria; out AHasMerge: Boolean); override;
    procedure PopulateMergedList(AList, ASourceList: TcxFilterCriteriaItemList;
      out AHasMerge: Boolean); override;
  end;

  { TdxFilterBoxTokenCriteriaItemMergeInfo }

  TdxFilterBoxTokenCriteriaItemMergeProc = procedure(AItem,
    AMergeItem: TdxFilterBoxTokenCriteriaItem) of object;
  TdxFilterBoxTokenCriteriaItemSupportsMergeFunc = function(AItem,
    AMergeItem: TdxFilterBoxTokenCriteriaItem): Boolean of object;

  TdxFilterBoxTokenCriteriaItemMergeInfo = class // for inernal use only
  strict private
    FMergedItems: TDictionary<TObject, TdxFilterBoxTokenCriteriaItem>;
    FMergeProc: TdxFilterBoxTokenCriteriaItemMergeProc;
    FSupportsMergeFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc;
  strict protected
    property MergedItems: TDictionary<TObject, TdxFilterBoxTokenCriteriaItem>
      read FMergedItems;
    property MergeProc: TdxFilterBoxTokenCriteriaItemMergeProc read FMergeProc;
    property SupportsMergeFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc
      read FSupportsMergeFunc;
  public
    constructor Create(AMergeProc: TdxFilterBoxTokenCriteriaItemMergeProc;
      ASupportsMergeFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc);
    destructor Destroy; override;

    function Merge(AItem: TdxFilterBoxTokenCriteriaItem): Boolean;
    procedure RegisterMergedItem(AItem: TdxFilterBoxTokenCriteriaItem);
    function SupportsMerge(AItem: TdxFilterBoxTokenCriteriaItem): Boolean;
  end;

  { TdxFilterBoxTokenCriteriaItemMergeInfos }

  TdxFilterBoxTokenCriteriaItemMergeInfos = class(TdxFastObjectList)
  // for inernal use only
  strict private
    function GetItem(AIndex: Integer)
      : TdxFilterBoxTokenCriteriaItemMergeInfo; inline;
  public
    procedure Add(AMergeProc: TdxFilterBoxTokenCriteriaItemMergeProc;
      AIsItemSupportedFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc);
    function Find(AItem: TdxFilterBoxTokenCriteriaItem)
      : TdxFilterBoxTokenCriteriaItemMergeInfo;

    property Items[Index: Integer]: TdxFilterBoxTokenCriteriaItemMergeInfo
      read GetItem; default;
  end;

  { TdxFilterBoxTokenCriteriaItemMerger }

  TdxFilterBoxTokenCriteriaItemMerger = class
    (TdxFilterBoxTokenCriteriaCustomMerger) // for inernal use only
  protected
    function CreateInfos: TdxFilterBoxTokenCriteriaItemMergeInfos; virtual;
    procedure PopulateInfos(AInfos
      : TdxFilterBoxTokenCriteriaItemMergeInfos); virtual;
    procedure PopulateMergedList(AList, ASourceList: TcxFilterCriteriaItemList;
      out AHasMerge: Boolean); override;
    procedure MergeDateProc(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem); virtual;
    procedure MergeFromToProc(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem); virtual;
    procedure MergeInListProc(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem); virtual;
    procedure MergeNotInListProc(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem); virtual;
    function SupportsMergeDate(AItem, AMergeItem: TdxFilterBoxTokenCriteriaItem)
      : Boolean; virtual;
    function SupportsMergeFromTo(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem): Boolean; virtual;
    function SupportsMergeInList(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem): Boolean; virtual;
    function SupportsMergeNotInList(AItem, AMergeItem
      : TdxFilterBoxTokenCriteriaItem): Boolean; virtual;
  end;

  { TcxFilterCriteriaSimplificator }

  TdxFilterBoxTokenCriteriaSimplificator = class // for inernal use only
  strict private
    FCriteria: TdxFilterBoxTokenCriteria;

    procedure Merge;
    function MergeItems: Boolean;
    function MergeLists: Boolean;
    procedure RemoveEmptyItems(AList: TcxFilterCriteriaItemList = nil);
  strict protected
    function DoMerge(AMergerClass: TdxFilterBoxTokenCriteriaCustomMergerClass)
      : Boolean; virtual;

    property Criteria: TdxFilterBoxTokenCriteria read FCriteria;
  public
    constructor Create(ACriteria: TdxFilterBoxTokenCriteria); virtual;

    procedure Simplify; virtual;
  end;

  { TdxFilterBoxTokenCriteria }

  TdxFilterBoxTokenCriteria = class(TcxDataFilterCriteria)
  // for internal use only
  strict private
    FSourceCriteria: TcxDataFilterCriteria;
  protected
    procedure CloneItemToSourceCriteria(AItem: TdxFilterBoxTokenCriteriaItem;
      AParent: TcxFilterCriteriaItemList); virtual;
    function CreateSimplificator
      : TdxFilterBoxTokenCriteriaSimplificator; virtual;
    function GetFilterCaption: string; override;
    function GetItemClass: TcxFilterCriteriaItemClass; override;
    function IsInternal: Boolean; override;
    procedure PopulateSourceCriteria; virtual;
    procedure PopulateSourceCriteriaList(AList, AOriginalList
      : TcxFilterCriteriaItemList); virtual;

    property SourceCriteria: TcxDataFilterCriteria read FSourceCriteria;
  public
    constructor Create(ASourceCriteria: TcxDataFilterCriteria);
      reintroduce; virtual;

    procedure Simplify;
    procedure UpdateBySourceCriteria;
    procedure UpdateSourceCriteria;
  end;

implementation

uses
  SysUtils, Variants, RTLConsts, Math, cxDrawTextUtils, dxCoreGraphics,
  cxFilterConsts, cxFilterControl, cxFilterControlUtils, cxLookAndFeels,
  dxDPIAwareUtils, cxVariants;

const
  dxIsEmptyStr = '<Filter is Empty>';

type
  { TdxCheckListBoxHelper }

  TdxCheckListBoxHelper = class helper for TdxCustomCheckListBox
  public
    function GetTextOffsets: TRect;
  end;

  { TcxFilterCriteriaHelper }

  TcxFilterCriteriaHelper = class helper for TcxFilterCriteria
  public
    function GetItemCaption(ACriteriaItem: TcxFilterCriteriaItem): string;
    function GetItemDisplayValue(ACriteriaItem: TcxFilterCriteriaItem): string;
  end;

  { TdxCheckListBoxHelper }

function TdxCheckListBoxHelper.GetTextOffsets: TRect;
begin
  Result := TextOffsets;
end;

{ TcxFilterCriteriaHelper }

function TcxFilterCriteriaHelper.GetItemCaption(ACriteriaItem
  : TcxFilterCriteriaItem): string;
begin
  Result := GetItemExpressionFieldName(ACriteriaItem, True);
end;

function TcxFilterCriteriaHelper.GetItemDisplayValue(ACriteriaItem
  : TcxFilterCriteriaItem): string;
begin
  Result := GetItemExpressionValue(ACriteriaItem, True);
end;

{ TdxFilterBoxMRUItem }

constructor TdxFilterBoxMRUItem.Create(AFilter: TcxDataFilterCriteria);
begin
  inherited Create;
  FFilter := AFilter.DataController.CreateFilter;
  Filter.Assign(AFilter);
end;

destructor TdxFilterBoxMRUItem.Destroy;
begin
  FreeAndNil(FFilter);
  inherited Destroy;
end;

procedure TdxFilterBoxMRUItem.AssignTo(AFilter: TcxDataFilterCriteria);
begin
  AFilter.AssignItems(Filter);
end;

function TdxFilterBoxMRUItem.Equals(AItem: TcxMRUItem): Boolean;
begin
  Result := StreamEquals(TdxFilterBoxMRUItem(AItem).GetStream);
end;

function TdxFilterBoxMRUItem.FilterEquals
  (AFilter: TcxDataFilterCriteria): Boolean;
begin
  Result := StreamEquals(GetFilterStream(AFilter));
end;

function TdxFilterBoxMRUItem.GetStream: TMemoryStream;
begin
  Result := GetFilterStream(Filter);
end;

function TdxFilterBoxMRUItem.StreamEquals(AStream: TMemoryStream): Boolean;
var
  AOwnStream: TMemoryStream;
begin
  AOwnStream := GetStream;
  try
    Result := StreamsEqual(AOwnStream, AStream);
  finally
    AStream.Free;
    AOwnStream.Free;
  end;
end;

function TdxFilterBoxMRUItem.GetCaption: string;
begin
  Result := Filter.FilterCaption;
end;

function TdxFilterBoxMRUItem.GetFilterStream(AFilter: TcxDataFilterCriteria)
  : TMemoryStream;
begin
  Result := TMemoryStream.Create;
  AFilter.WriteData(Result);
end;

{ TdxFilterBoxMRUItems }

constructor TdxFilterBoxMRUItems.Create;
begin
  inherited Create;
  FVisibleItems := TdxFastList.Create;
end;

destructor TdxFilterBoxMRUItems.Destroy;
begin
  FVisibleItems.Free;
  inherited Destroy;
end;

function TdxFilterBoxMRUItems.GetItem(Index: Integer): TdxFilterBoxMRUItem;
begin
  Result := TdxFilterBoxMRUItem(inherited Items[Index]);
end;

function TdxFilterBoxMRUItems.GetVisibleCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TdxFilterBoxMRUItems.GetVisibleItem(Index: Integer)
  : TdxFilterBoxMRUItem;
begin
  Result := TdxFilterBoxMRUItem(FVisibleItems[Index]);
end;

procedure TdxFilterBoxMRUItems.AddCurrentFilter;
begin
  Add(CurrentFilter);
end;

function TdxFilterBoxMRUItems.GetCurrentFilter: TcxDataFilterCriteria;
begin
  Result := nil;
end;

function TdxFilterBoxMRUItems.GetItemClass: TdxFilterBoxMRUItemClass;
begin
  Result := TdxFilterBoxMRUItem;
end;

procedure TdxFilterBoxMRUItems.DeleteEmptyItems;
var
  APrevCount, I: Integer;
begin
  APrevCount := Count;
  for I := Count - 1 downto 0 do
    if Items[I].Filter.IsEmpty then
      Delete(I);
  if Count <> APrevCount then
    RefreshVisibleItemsList;
end;

procedure TdxFilterBoxMRUItems.FilterChanged;
begin
  RefreshVisibleItemsList;
end;

procedure TdxFilterBoxMRUItems.RefreshVisibleItemsList;
var
  APrevVisibleCount: Integer;
  I: Integer;
  AItem: TdxFilterBoxMRUItem;
begin
  APrevVisibleCount := VisibleCount;
  FVisibleItems.Clear;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if not((CurrentFilter <> nil) and AItem.FilterEquals(CurrentFilter)) then
      FVisibleItems.Add(AItem);
  end;
  if VisibleCount <> APrevVisibleCount then
    VisibleCountChanged(APrevVisibleCount);
end;

procedure TdxFilterBoxMRUItems.SetMaxCount(AMaxCount: Integer);
begin
  MaxCount := AMaxCount;
  FVisibleItems.Count := Min(VisibleCount, AMaxCount);
end;

procedure TdxFilterBoxMRUItems.VisibleCountChanged(APrevVisibleCount: Integer);
begin
  // do nothing
end;

procedure TdxFilterBoxMRUItems.Add(AFilter: TcxDataFilterCriteria);
begin
  if not AFilter.IsEmpty then
  begin
    inherited Add(GetItemClass.Create(AFilter));
    RefreshVisibleItemsList;
  end;
end;

{ TdxFilterBoxMRUItemsPopupListBox }

constructor TdxFilterBoxMRUItemsPopupListBox.Create
  (APopup: TdxFilterBoxMRUItemsPopup);
begin
  inherited Create(APopup);
end;

procedure TdxFilterBoxMRUItemsPopupListBox.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      Popup.CloseUp;
  end;
end;

function TdxFilterBoxMRUItemsPopupListBox.NeedHotTrack: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxMRUItemsPopupListBox.GetPopup: TdxFilterBoxMRUItemsPopup;
begin
  Result := TdxFilterBoxMRUItemsPopup(Owner);
end;

{ TdxFilterBoxMRUItemsPopup }

constructor TdxFilterBoxMRUItemsPopup.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  FListBox := GetListBoxClass.Create(Self);
  InitListBox;
end;

procedure TdxFilterBoxMRUItemsPopup.ListBoxAction(Sender: TdxCustomListBox;
  AItemIndex: Integer);
begin
  ApplyFilterMRUItem(AItemIndex);
  CloseUp;
end;

procedure TdxFilterBoxMRUItemsPopup.ApplyFilter(AItemIndex: Integer);
begin
  TdxFilterBoxMRUItem(ListBox.Items[AItemIndex].Data)
    .AssignTo(MRUItems.CurrentFilter);
end;

procedure TdxFilterBoxMRUItemsPopup.AddFilterMRUItems;
var
  I: Integer;
  AItem: TdxFilterBoxMRUItem;
begin
  if MRUItems = nil then
    Exit;
  ListBox.BeginUpdate;
  try
    ListBox.Clear;
    for I := 0 to MRUItems.VisibleCount - 1 do
    begin
      AItem := MRUItems.VisibleItems[I];
      ListBox.Items.AddObject(AItem.Caption, AItem);
    end;
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TdxFilterBoxMRUItemsPopup.ApplyFilterMRUItem(AItemIndex: Integer);
begin
  if MRUItems = nil then
    Exit;
  ApplyFilter(AItemIndex);
  MRUItems.AddCurrentFilter;
end;

function TdxFilterBoxMRUItemsPopup.GetListBoxClass
  : TdxFilterBoxMRUItemsPopupListBoxClass;
begin
  Result := TdxFilterBoxMRUItemsPopupListBox;
end;

function TdxFilterBoxMRUItemsPopup.GetMRUItemCount: Integer;
begin
  Result := 0;
end;

function TdxFilterBoxMRUItemsPopup.GetMRUItems: TdxFilterBoxMRUItems;
begin
  Result := nil;
end;

function TdxFilterBoxMRUItemsPopup.GetTextOffsetHorz: Integer;
begin
  Result := ListBox.GetTextOffsets.Left;
end;

procedure TdxFilterBoxMRUItemsPopup.InitListBox;
begin
  ListBox.BorderStyle := cxcbsNone;
  ListBox.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  ListBox.ShowCheckBoxes := False;
  ListBox.Parent := Self;
  ListBox.OnAction := ListBoxAction;
end;

procedure TdxFilterBoxMRUItemsPopup.InitPopup;
begin
  inherited;
  ListBox.BeginUpdate;
  try
    AddFilterMRUItems;
    ListBox.VisibleItemCount := GetMRUItemCount;
    ListBox.AutoSize := True;
    ListBox.Constraints.MinWidth := ClientMinWidth;
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TdxFilterBoxMRUItemsPopup.UpdateInnerControlsHeight(var AClientHeight
  : Integer);
begin
  ListBox.Height := AClientHeight;
  AClientHeight := ListBox.Height;
end;

{ TdxFilterBoxTokenCriteriaOptions }

constructor TdxFilterBoxTokenCriteriaOptions.Create
  (AOptions: IdxFilterBoxTokenCriteriaOptions);
begin
  FItemRemoval := AOptions.ItemRemoval;
end;

{ TdxFilterBoxTokenCriteriaCustomElementViewInfo }

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.Calculate
  (const ABounds: TRect);
begin
  Calculate(ABounds.Left, ABounds.Top, cxRectWidth(ABounds),
    cxRectHeight(ABounds));
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.Paint
  (ACanvas: TcxCanvas);
begin
  if not IsVisibleForPainting or not ACanvas.RectVisible(Bounds) then
    Exit;
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    DoPaint(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.RightToLeftConversion
  (const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.Calculate(ALeftBound,
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  FIsRightToLeftConverted := False;
  if AWidth = -1 then
    AWidth := Width;
  if AHeight = -1 then
    AHeight := Height;
  FBounds := Rect(ALeftBound, ATopBound, ALeftBound + AWidth,
    ATopBound + AHeight);
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.DoRightToLeftConversion
  (const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
end;

function TdxFilterBoxTokenCriteriaCustomElementViewInfo.GetHeight: Integer;
begin
  if not FIsHeightCalculated then
  begin
    FHeight := CalculateHeight;
    FIsHeightCalculated := True;
  end;
  Result := FHeight;
end;

function TdxFilterBoxTokenCriteriaCustomElementViewInfo.GetWidth: Integer;
begin
  if not FIsWidthCalculated then
  begin
    FWidth := CalculateWidth;
    FIsWidthCalculated := True;
  end;
  Result := FWidth;
end;

function TdxFilterBoxTokenCriteriaCustomElementViewInfo.HasPoint
  (const APoint: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, APoint);
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.Invalidate;
begin
  Invalidate(Bounds);
end;

procedure TdxFilterBoxTokenCriteriaCustomElementViewInfo.Invalidate
  (const ARect: TRect);
begin
  cxInvalidateRect(Control, ARect);
end;

function TdxFilterBoxTokenCriteriaCustomElementViewInfo.
  IsVisibleForPainting: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxTokenCriteriaCustomElementViewInfo.GetViewParams
  : TcxViewParams;
begin
  if not FIsViewParamsCalculated then
  begin
    FViewParams := CalculateViewParams;
    FIsViewParamsCalculated := True;
  end;
  Result := FViewParams;
end;

{ TdxFilterBoxTokenCriteriaElementViewInfo }

constructor TdxFilterBoxTokenCriteriaElementViewInfo.Create(ACriteriaViewInfo
  : TdxFilterBoxTokenCriteriaViewInfo);
begin
  inherited Create;
  FCriteriaViewInfo := ACriteriaViewInfo;
  FState := cxbsNormal;
end;

destructor TdxFilterBoxTokenCriteriaElementViewInfo.Destroy;
begin
  EndMouseTracking(Self);
  inherited Destroy;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.Calculate(ALeftBound,
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  FBackgroundBounds := CalculateBackgroundBounds;
  FTextBounds := CalculateTextBounds;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.
  CalculateBackgroundBounds: TRect;
begin
  Result := Bounds;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.CalculateHeight: Integer;
begin
  if Text <> '' then
    Result := TextMargins.Top + TextSize.cy + TextMargins.Bottom
  else
    Result := 0;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.CalculateTextBounds: TRect;
begin
  Result := cxRectCenter(Bounds, TextSize);
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.CalculateViewParams
  : TcxViewParams;
begin
  Result := CriteriaViewInfo.ViewParams;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.CalculateWidth: Integer;
begin
  if Text <> '' then
    Result := TextMargins.Left + TextSize.cx + TextMargins.Right
  else
    Result := 0;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.Click;
begin
  // do nothing
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.DrawBackground
  (ACanvas: TcxCanvas);
begin
  // do nothing
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.DoPaint(ACanvas: TcxCanvas);
begin
  if NeedDrawBackground then
    DrawBackground(ACanvas);
  if Text <> '' then
    DrawText(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.DoRightToLeftConversion
  (const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FBackgroundBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FBackgroundBounds, ABounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds,
    ABounds);
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.DrawText(ACanvas: TcxCanvas);
var
  ARect: TRect;
begin
  ARect := TextBounds;
  cxTextOut(ACanvas.Handle, Text, ARect, TextFormat, ViewParams.Font, 0, 0, 0,
    ViewParams.TextColor);
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetControl: TcxControl;
begin
  Result := CriteriaViewInfo.Control;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetCriteria
  : TdxFilterBoxTokenCriteria;
begin
  Result := CriteriaViewInfo.Criteria;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetHitInfo
  (const APoint: TPoint): TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  if HasHitInfo and HasPoint(APoint) then
    Result := Self
  else
    Result := nil;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := CriteriaViewInfo.Painter;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetScaleFactor
  : TdxScaleFactor;
begin
  Result := CriteriaViewInfo.ScaleFactor;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetState: TcxButtonState;
begin
  if Self = CriteriaViewInfo.PressedElement then
    Result := cxbsPressed
  else if Self = CriteriaViewInfo.HotElement then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetText: string;
begin
  Result := '';
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetTextMargins: TRect;
begin
  Result := cxEmptyRect;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.HasHitInfo: Boolean;
begin
  Result := False;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.InvalidateOnStateChanged;
begin
  Invalidate;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.MouseLeave;
begin
  if CriteriaViewInfo.HotElement = Self then
    CriteriaViewInfo.HotElement := nil;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.NeedDrawBackground: Boolean;
begin
  Result := False;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.
  NeedInvalidateOnStateChanged: Boolean;
begin
  Result := False;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.StateChanged;
begin
  if NeedInvalidateOnStateChanged then
    InvalidateOnStateChanged;
  case State of
    cxbsHot:
      BeginMouseTracking(Control, Bounds, Self);
    cxbsNormal:
      EndMouseTracking(Self);
  end;
end;

procedure TdxFilterBoxTokenCriteriaElementViewInfo.UpdateState;
var
  AState: TcxButtonState;
begin
  AState := GetState;
  if State <> AState then
  begin
    FState := AState;
    StateChanged;
  end;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.PtInCaller
  (const P: TPoint): Boolean;
begin
  Result := HasPoint(P);
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.CalculateTextSize: TSize;
begin
  if Text <> '' then
    Result := TdxTextMeasurer.TextSizeTO(ViewParams.Font, Text)
  else
    Result := Size(0, 0);
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetTextFormat: Cardinal;
begin
  Result := CXTO_LEFT or CXTO_TOP or CXTO_SINGLELINE;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetTextSize: TSize;
begin
  if not FIsTextSizeCalculated then
  begin
    FTextSize := CalculateTextSize;
    FIsTextSizeCalculated := True;
  end;
  Result := FTextSize;
end;

function TdxFilterBoxTokenCriteriaElementViewInfo.GetTextValue: string;
begin
  if not FIsTextAssigned then
  begin
    FText := GetText;
    FIsTextAssigned := True;
  end;
  Result := FText;
end;

{ TdxFilterBoxTokenCriteriaIndentViewInfo }

function TdxFilterBoxTokenCriteriaIndentViewInfo.CalculateHeight: Integer;
begin
  Result := 1;
end;

function TdxFilterBoxTokenCriteriaIndentViewInfo.CalculateWidth: Integer;
begin
  Result := FWidth;
end;

constructor TdxFilterBoxTokenCriteriaIndentViewInfo.Create(ACriteriaViewInfo
  : TdxFilterBoxTokenCriteriaViewInfo; AWidth: Integer);
begin
  inherited Create(ACriteriaViewInfo);
  FWidth := AWidth;
end;

{ TdxFilterBoxTokenCriteriaCustomItemElementViewInfo }

constructor TdxFilterBoxTokenCriteriaCustomItemElementViewInfo.Create
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo);
begin
  inherited Create(ACriteriaItemViewInfo.CriteriaViewInfo);
  FCriteriaItemViewInfo := ACriteriaItemViewInfo;
end;

{ TdxFilterBoxTokenCriteriaRemoveButtonViewInfo }

constructor TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.Create
  (AAreaViewInfo: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo);
begin
  inherited Create(AAreaViewInfo.CriteriaViewInfo);
  FAreaViewInfo := AAreaViewInfo;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.Calculate
  (ALeftBound: Integer; ATopBound: Integer; AWidth: Integer = -1;
  AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  FImageBounds := cxRectCenter(Bounds, ImageSize);
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.Click;
begin
  AreaViewInfo.CriteriaItemViewInfo.CriteriaItem.Free;
  Criteria.Simplify;
  Criteria.UpdateSourceCriteria;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.DoPaint
  (ACanvas: TcxCanvas);
begin
  inherited DoPaint(ACanvas);
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ImageBounds);
    Painter.DrawScaledFilterPanelRemoveButtonGlyph(ACanvas, ImageBounds,
      DrawState, ScaleFactor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.DoRightToLeftConversion
  (const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FImageBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FImageBounds, ABounds);
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.GetHeight: Integer;
begin
  Result := ImageMargin.Top + ImageSize.cy + ImageMargin.Bottom;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.GetWidth: Integer;
begin
  Result := ImageMargin.Left + ImageSize.cx + ImageMargin.Right;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.HasHitInfo: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.
  IsVisibleForPainting: Boolean;
begin
  Result := [DrawState, AreaViewInfo.State,
    AreaViewInfo.CriteriaItemViewInfo.State] * [cxbsPressed, cxbsHot] <> [];
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.
  NeedInvalidateOnStateChanged: Boolean;
begin
  Result := True;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.StateChanged;
begin
  AreaViewInfo.CriteriaItemViewInfo.Invalidate;
  inherited StateChanged;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.GetDrawState
  : TcxButtonState;
begin
  if cxIsTouchModeEnabled then
    Result := cxbsHot
  else
    Result := State
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.GetImageMargin: TRect;
begin
  Result := ScaleFactor.Apply(Rect(1, 1, 1, 1));
end;

function TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.GetImageSize: TSize;
begin
  Result := ScaleFactor.Apply(Size(8, 8));
end;

{ TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo }

constructor TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.Create
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo;
  ANeedBindWithItem: Boolean);
begin
  inherited Create(ACriteriaItemViewInfo);
  FButton := CreateButton;
  if ANeedBindWithItem then
    CriteriaItemViewInfo.BindRemoveButton(Self);
end;

destructor TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.Calculate
  (ALeftBound: Integer; ATopBound: Integer; AWidth: Integer = -1;
  AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  CalculateButton;
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.CalculateButton;
var
  ALeft: Integer;
begin
  ALeft := cxRectCenterHorizontally(Bounds, Button.Width).Left;
  Button.Calculate(ALeft, Bounds.Top);
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.
  CalculateHeight: Integer;
begin
  Result := CriteriaItemViewInfo.Height;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.
  CalculateWidth: Integer;
begin
  Result := Button.Width;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.CreateButton
  : TdxFilterBoxTokenCriteriaRemoveButtonViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaRemoveButtonViewInfo.Create(Self);
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.DoPaint
  (ACanvas: TcxCanvas);
begin
  inherited DoPaint(ACanvas);
  Button.Paint(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.
  DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  Button.RightToLeftConversion(ABounds);
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.GetHitInfo
  (const APoint: TPoint): TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  Result := Button.GetHitInfo(APoint);
  if Result = nil then
    Result := inherited GetHitInfo(APoint)
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.HasHitInfo: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.
  NeedInvalidateOnStateChanged: Boolean;
begin
  Result := True;
end;

{ TdxFilterBoxTokenCriteriaItemElementViewInfo }

function TdxFilterBoxTokenCriteriaItemElementViewInfo.GetCriteriaItemViewInfo
  : TdxFilterBoxTokenCriteriaItemViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaItemViewInfo
    (inherited CriteriaItemViewInfo);
end;

{ TdxFilterBoxTokenCriteriaItemCaptionViewInfo }

function TdxFilterBoxTokenCriteriaItemCaptionViewInfo.CalculateViewParams
  : TcxViewParams;
begin
  Result := inherited CalculateViewParams;
  Result.TextColor := CriteriaViewInfo.PainterParams.ItemCaptionTextColor;
end;

procedure TdxFilterBoxTokenCriteriaItemCaptionViewInfo.DrawBackground
  (ACanvas: TcxCanvas);
begin
  Painter.DrawScaledFilterItemCaptionBackground(ACanvas, BackgroundBounds,
    State, ScaleFactor);
end;

function TdxFilterBoxTokenCriteriaItemCaptionViewInfo.GetText: string;
begin
  Result := CriteriaItemViewInfo.Caption;
end;

function TdxFilterBoxTokenCriteriaItemCaptionViewInfo.GetTextMargins: TRect;
begin
  Result := CriteriaViewInfo.PainterParams.ItemCaptionTextMargins;
end;

function TdxFilterBoxTokenCriteriaItemCaptionViewInfo.
  NeedDrawBackground: Boolean;
begin
  Result := True;
end;

{ TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo }

function TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo.CalculateViewParams
  : TcxViewParams;
begin
  Result := inherited CalculateViewParams;
  Result.TextColor := CriteriaViewInfo.PainterParams.OperatorTextColor;
end;

function TdxFilterBoxTokenCriteriaItemCustomOperatorViewInfo.
  GetTextMargins: TRect;
begin
  Result := CriteriaViewInfo.PainterParams.OperatorTextMargins;
end;

{ TdxFilterBoxTokenCriteriaItemOperatorViewInfo }

constructor TdxFilterBoxTokenCriteriaItemOperatorViewInfo.Create
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo);
begin
  inherited Create(ACriteriaItemViewInfo);
  FSubIndex := -1;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.CalculateHeight: Integer;
begin
  if NeedDrawAsImage then
    Result := ImageSize.cy
  else
    Result := inherited CalculateHeight;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.
  CalculateImageSize: TSize;
begin
  Result := TcxFilterControlImagesHelper.GetScaledSize(ScaleFactor);
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.CalculateWidth: Integer;
begin
  if NeedDrawAsImage then
    Result := ImageSize.cx
  else
    Result := inherited CalculateWidth;
end;

procedure TdxFilterBoxTokenCriteriaItemOperatorViewInfo.DoPaint
  (ACanvas: TcxCanvas);
begin
  if NeedDrawAsImage then
    DrawImage(ACanvas)
  else
    inherited DoPaint(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaItemOperatorViewInfo.DrawBackground
  (ACanvas: TcxCanvas);
begin
  Painter.DrawScaledFilterOperatorBackground(ACanvas, BackgroundBounds, State,
    ScaleFactor);
end;

procedure TdxFilterBoxTokenCriteriaItemOperatorViewInfo.DrawImage
  (ACanvas: TcxCanvas);
begin
  TcxFilterControlImagesHelper.DrawOperatorImage(ACanvas.Canvas,
    GetFilterControlOperator(Kind, False), Bounds, Painter,
    TdxAlphaColors.FromColor(Painter.DefaultContentTextColor));
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.GetText: string;
begin
  Result := &Operator.DisplayText;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.GetImageSize: TSize;
begin
  if not FIsImageSizeCalculated then
  begin
    FImageSize := CalculateImageSize;
    FIsImageSizeCalculated := True;
  end;
  Result := FImageSize;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.GetKind
  : TcxFilterOperatorKind;
begin
  if SubIndex <> -1 then
    Result := CriteriaItemViewInfo.CriteriaItem.SubOperators[SubIndex].Kind
  else
    Result := CriteriaItemViewInfo.CriteriaItem.OperatorKind;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.GetOperator
  : TcxFilterOperator;
begin
  if SubIndex <> -1 then
    Result := CriteriaItemViewInfo.CriteriaItem.SubOperators[SubIndex].Operator
  else
    Result := CriteriaItemViewInfo.CriteriaItem.Operator;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.NeedDrawAsImage: Boolean;
begin
  Result := (CriteriaItemViewInfo.CriteriaItem.ExtOperatorKind = eokNone) and
    (Kind in [foEqual .. foGreaterEqual]) and
    not(fcoShowOperatorDescription in Criteria.Options) and
    not CriteriaItemViewInfo.CriteriaItem.IsOperatorNull;
end;

function TdxFilterBoxTokenCriteriaItemOperatorViewInfo.
  NeedDrawBackground: Boolean;
begin
  Result := not NeedDrawAsImage and
    (CriteriaItemViewInfo.CriteriaItem.ExtOperatorKind <> eokFromTo);
end;

{ TdxFilterBoxTokenCriteriaItemAndOperatorViewInfo }

function TdxFilterBoxTokenCriteriaItemAndOperatorViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@cxSFilterAndCaption);
  Result := AnsiLowerCase(Result);
end;

{ TdxFilterBoxTokenCriteriaItemToOperatorViewInfo }

function TdxFilterBoxTokenCriteriaItemToOperatorViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@cxSFilterToCaption);
end;

{ TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo }

constructor TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.Create
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaItemViewInfo;
  const ADisplayValue: string);
begin
  inherited Create(ACriteriaItemViewInfo);
  FDisplayValue := ADisplayValue;
end;

function TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.CalculateViewParams
  : TcxViewParams;
begin
  Result := inherited CalculateViewParams;
  Result.TextColor := CriteriaViewInfo.PainterParams.ValueTextColor;
end;

procedure TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.DrawBackground
  (ACanvas: TcxCanvas);
begin
  Painter.DrawScaledFilterValueBackground(ACanvas, BackgroundBounds, State,
    ScaleFactor);
end;

function TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.GetText: string;
begin
  Result := FDisplayValue;
end;

function TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.
  GetTextMargins: TRect;
begin
  Result := CriteriaViewInfo.PainterParams.ValueTextMargins;
end;

function TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.
  NeedDrawBackground: Boolean;
begin
  Result := True;
end;

{ TdxFilterBoxTokenCriteriaListElementViewInfo }

function TdxFilterBoxTokenCriteriaListElementViewInfo.GetCriteriaItemViewInfo
  : TdxFilterBoxTokenCriteriaListViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaListViewInfo
    (inherited CriteriaItemViewInfo);
end;

{ TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo }

function TdxFilterBoxTokenCriteriaListCustomBoolOperatorViewInfo.
  CalculateViewParams: TcxViewParams;
begin
  Result := inherited CalculateViewParams;
  Result.TextColor := CriteriaViewInfo.PainterParams.BoolOperatorTextColor;
end;

{ TdxFilterBoxTokenCriteriaListNotOperatorViewInfo }

function TdxFilterBoxTokenCriteriaListNotOperatorViewInfo.GetText: string;
begin
  Result := cxSFilterString(@cxSFilterNotCaption);
end;

{ TdxFilterBoxTokenCriteriaListBoolOperatorViewInfo }

function TdxFilterBoxTokenCriteriaListBoolOperatorViewInfo.GetText: string;
begin
  case CriteriaItemViewInfo.CriteriaItem.BoolOperatorKind of
    fboAnd, fboNotAnd:
      Result := cxGetResourceString(@cxSFilterAndCaption);
    fboOr, fboNotOr:
      Result := cxGetResourceString(@cxSFilterOrCaption);
  end;
end;

{ TdxFilterBoxTokenCriteriaCollectionViewInfo }

constructor TdxFilterBoxTokenCriteriaCollectionViewInfo.Create(ACriteriaViewInfo
  : TdxFilterBoxTokenCriteriaViewInfo);
begin
  inherited Create(ACriteriaViewInfo);
  FElements := TObjectList<TdxFilterBoxTokenCriteriaElementViewInfo>.Create;
end;

destructor TdxFilterBoxTokenCriteriaCollectionViewInfo.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.AfterConstruction;
begin
  Populate;
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.Calculate(ALeftBound,
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  FContentBounds := CalculateContentBounds;
  CalculateElements;
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.
  CalculateContentBounds: TRect;
begin
  Result := cxRectContent(Bounds, ContentMargins);
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.CalculateElements;
var
  I, ALeft, ATop: Integer;
  AElement: TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  ALeft := ContentBounds.Left;
  for I := 0 to Elements.Count - 1 do
  begin
    AElement := Elements[I];
    ATop := cxRectCenterVertically(Bounds, AElement.Height).Top;
    AElement.Calculate(ALeft, ATop);
    Inc(ALeft, AElement.Width);
  end;
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.CalculateHeight: Integer;
var
  I: Integer;
begin
  Result := inherited CalculateHeight;
  for I := 0 to Elements.Count - 1 do
    Result := Max(Result, Elements[I].Height);
  Inc(Result, ContentMargins.Top + ContentMargins.Bottom);
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := inherited CalculateWidth;
  for I := 0 to Elements.Count - 1 do
    Inc(Result, Elements[I].Width);
  Inc(Result, ContentMargins.Left + ContentMargins.Right);
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.DoGetContentMargins: TRect;
begin
  Result := cxEmptyRect;
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.DoPaint
  (ACanvas: TcxCanvas);
var
  I: Integer;
begin
  inherited DoPaint(ACanvas);
  for I := 0 to Elements.Count - 1 do
    Elements[I].Paint(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.DoRightToLeftConversion
  (const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Elements.Count - 1 do
    Elements[I].RightToLeftConversion(ABounds);
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.GetHitInfo
  (const APoint: TPoint): TdxFilterBoxTokenCriteriaElementViewInfo;
var
  I: Integer;
  AElement: TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  Result := inherited GetHitInfo(APoint);
  if (Result = Self) or (Result = nil) and HasPoint(APoint) then
    for I := 0 to Elements.Count - 1 do
    begin
      AElement := Elements[I].GetHitInfo(APoint);
      if AElement <> nil then
        Exit(AElement);
    end;
end;

procedure TdxFilterBoxTokenCriteriaCollectionViewInfo.Populate;
begin
  // do nothing
end;

function TdxFilterBoxTokenCriteriaCollectionViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(DoGetContentMargins);
end;

{ TdxFilterBoxTokenCriteriaCustomItemViewInfo }

constructor TdxFilterBoxTokenCriteriaCustomItemViewInfo.Create(ACriteriaViewInfo
  : TdxFilterBoxTokenCriteriaViewInfo;
  ACriteriaItem: TcxCustomFilterCriteriaItem);
begin
  inherited Create(ACriteriaViewInfo);
  FCriteriaItem := ACriteriaItem;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.AddCloseBracketIndent;
begin
  // do nothing
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.AddIndent
  (AWidth: Integer);
begin
  Elements.Add(TdxFilterBoxTokenCriteriaIndentViewInfo.Create
    (CriteriaViewInfo, AWidth));
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.AddOpenBracketIndent;
begin
  // do nothing
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.BindRemoveButton
  (AButton: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo);
begin
  FRemoveButton := AButton;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.DoGetContentMargins: TRect;
begin
  if HasBrackets then
    Result := CriteriaViewInfo.PainterParams.FilterPanelItemMargins
  else
    Result := inherited DoGetContentMargins;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.DoPaint
  (ACanvas: TcxCanvas);
begin
  DrawBorders(ACanvas);
  inherited DoPaint(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.DrawBorders
  (ACanvas: TcxCanvas);
begin
  case BordersType of
    RemovingArea:
      DrawRemovingArea(ACanvas);
    Brackets:
      DrawBrackets(ACanvas);
  end;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.DrawBrackets
  (ACanvas: TcxCanvas);
begin
  Painter.DrawScaledFilterPanelBrackets(ACanvas, Bounds, ScaleFactor);
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.DrawRemovingArea
  (ACanvas: TcxCanvas);
begin
  Painter.DrawScaledFilterPanelRemovingArea(ACanvas, Bounds, ScaleFactor);
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.HasBrackets: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.HasHitInfo: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.HasRemoveButton: Boolean;
begin
  Result := FRemoveButton <> nil;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.InvalidateOnStateChanged;
begin
  if HasRemoveButton then
    FRemoveButton.Button.Invalidate;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.NeedRemoveButton: Boolean;
begin
  Result := CriteriaViewInfo.Options.ItemRemoval and HasBrackets;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.
  NeedInvalidateOnStateChanged: Boolean;
begin
  Result := True;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.Populate;
begin
  if HasBrackets then
    AddOpenBracketIndent;
  PopulateContent;
  if HasBrackets then
    AddCloseBracketIndent;
end;

procedure TdxFilterBoxTokenCriteriaCustomItemViewInfo.PopulateContent;
begin
  // do nothing
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.GetBordersType
  : TBordersType;
begin
  if HasRemoveButton and (FRemoveButton.Button.State in [cxbsPressed, cxbsHot])
  then
    Result := RemovingArea
  else if HasBrackets then
    Result := Brackets
  else
    Result := None;
end;

function TdxFilterBoxTokenCriteriaCustomItemViewInfo.GetElementsIndent: Integer;
begin
  Result := CriteriaViewInfo.PainterParams.ElementsIndent;
end;

{ TdxFilterBoxTokenCriteriaItemViewInfo }

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddAndOperator;
begin
  Elements.Add(TdxFilterBoxTokenCriteriaItemAndOperatorViewInfo.Create(Self));
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddDisplayValue
  (const ADisplayValue: string);
begin
  Elements.Add(TdxFilterBoxTokenCriteriaItemDisplayValueViewInfo.Create(Self,
    ADisplayValue));
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddDisplayValues;
var
  I, APos: Integer;
begin
  if (CriteriaItem.OperatorKind in [foBetween .. foNotInList]) or
    (CriteriaItem.ExtOperatorKind = eokFromTo) then
  begin
    APos := 1;
    for I := VarArrayLowBound(CriteriaItem.Value, 1) to VarArrayHighBound
      (CriteriaItem.Value, 1) do
    begin
      if I <> 0 then
        AddDisplayValueSeparator;
      AddDisplayValue(ExtractFilterDisplayValue
        (CriteriaItem.DisplayValue, APos));
    end;
  end
  else
    AddDisplayValue(Criteria.GetItemDisplayValue(CriteriaItem));
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddDisplayValueSeparator;
begin
  if CriteriaItem.ExtOperatorKind = eokFromTo then
    AddToOperator
  else if CriteriaItem.OperatorKind in [foBetween, foNotBetween] then
    AddAndOperator
  else
    AddIndent(ElementsIndent);
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddItemCaption;
begin
  Elements.Add(TdxFilterBoxTokenCriteriaItemCaptionViewInfo.Create(Self));
end;

function TdxFilterBoxTokenCriteriaItemViewInfo.AddOperator
  (ASubIndex: Integer = -1): TdxFilterBoxTokenCriteriaItemOperatorViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaItemOperatorViewInfo.Create(Self);
  Result.SubIndex := ASubIndex;
  if Result.NeedDrawBackground then
    AddIndent(ElementsIndent);
  Elements.Add(Result);
  if not Result.Operator.IsUnary and Result.NeedDrawBackground then
    AddIndent(ElementsIndent);
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.AddToOperator;
begin
  Elements.Add(TdxFilterBoxTokenCriteriaItemToOperatorViewInfo.Create(Self));
end;

procedure TdxFilterBoxTokenCriteriaItemViewInfo.PopulateContent;
var
  I: Integer;
  AOperator: TdxFilterBoxTokenCriteriaItemOperatorViewInfo;
begin
  if Caption <> '' then
    AddItemCaption;
  if CriteriaItem.SubOperators.Count > 0 then
  begin
    AOperator := nil;
    for I := 0 to CriteriaItem.SubOperators.Count - 1 do
      AOperator := AddOperator(I);
  end
  else
    AOperator := AddOperator;
  if not AOperator.Operator.IsUnary and (CriteriaItem.DisplayValue <> '') then
    AddDisplayValues;
end;

function TdxFilterBoxTokenCriteriaItemViewInfo.GetCaption: string;
begin
  Result := Criteria.GetItemCaption(CriteriaItem);
end;

function TdxFilterBoxTokenCriteriaItemViewInfo.GetCriteriaItem
  : TdxFilterBoxTokenCriteriaItem;
begin
  Result := TdxFilterBoxTokenCriteriaItem(inherited CriteriaItem);
end;

{ TdxFilterBoxTokenCriteriaListViewInfo }

constructor TdxFilterBoxTokenCriteriaListViewInfo.Create(ACriteriaViewInfo
  : TdxFilterBoxTokenCriteriaViewInfo; AItemList: TcxFilterCriteriaItemList;
  AAuxiliary: Boolean = False);
begin
  inherited Create(ACriteriaViewInfo, AItemList);
  FAuxiliary := AAuxiliary;
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.AddBoolOperator;
begin
  Elements.Add(TdxFilterBoxTokenCriteriaListBoolOperatorViewInfo.Create(Self));
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.AddCloseBracketIndent;
begin
  if ContainsNotOperator or not CriteriaViewInfo.Options.ItemRemoval then
    AddIndent(ElementsIndent);
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.AddRemoveButton
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo);
begin
  Elements.Add(CreateRemoveButton(ACriteriaItemViewInfo));
end;

function TdxFilterBoxTokenCriteriaListViewInfo.AddItem(ACriteriaItem
  : TdxFilterBoxTokenCriteriaItem): TdxFilterBoxTokenCriteriaItemViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaItemViewInfo.Create(CriteriaViewInfo,
    ACriteriaItem);
  Elements.Add(Result);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.AddItemList
  (ACriteriaItem: TcxFilterCriteriaItemList; AAuxiliary: Boolean = False)
  : TdxFilterBoxTokenCriteriaListViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaListViewInfo.Create(CriteriaViewInfo,
    ACriteriaItem, AAuxiliary);
  Elements.Add(Result);
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.AddNotOperator;
begin
  Elements.Add(TdxFilterBoxTokenCriteriaListNotOperatorViewInfo.Create(Self));
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.AddOpenBracketIndent;
var
  AIndent: Integer;
begin
  if ContainsNotOperator then
    Exit;
  if CriteriaViewInfo.Options.ItemRemoval and (CriteriaItem.Count > 1) then
    AIndent := RemoveButtonWidth
  else
    AIndent := ElementsIndent;
  AddIndent(AIndent);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.ContainsNotOperator: Boolean;
begin
  Result := (CriteriaItem.BoolOperatorKind in [fboNotAnd, fboNotOr]) and
    not Auxiliary;
end;

function TdxFilterBoxTokenCriteriaListViewInfo.CreateRemoveButton
  (ACriteriaItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo;
  ANeedBindWithItem: Boolean = True)
  : TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo;
begin
  Result := TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo.Create
    (ACriteriaItemViewInfo, ANeedBindWithItem);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.GetText: string;
begin
  if (CriteriaItem = Criteria.Root) and CriteriaItem.IsEmpty then
    Result := dxIsEmptyStr
  else
    Result := inherited GetText;
end;

function TdxFilterBoxTokenCriteriaListViewInfo.GetTextMargins: TRect;
begin
  if (CriteriaItem = Criteria.Root) and CriteriaItem.IsEmpty then
    Result := CriteriaViewInfo.PainterParams.ItemCaptionTextMargins
  else
    Result := inherited GetTextMargins;
end;

function TdxFilterBoxTokenCriteriaListViewInfo.HasBrackets: Boolean;
begin
  Result := (CriteriaItem = Criteria.Root) and CriteriaItem.IsEmpty or
    (CriteriaItem.BoolOperatorKind in [fboAnd, fboOr]) and
    (CriteriaItem <> Criteria.Root) or
    (CriteriaItem.BoolOperatorKind in [fboNotAnd, fboNotOr]) and
    (ContainsNotOperator and (CriteriaItem <> Criteria.Root) or
    not ContainsNotOperator and (CriteriaItem.Count > 1));
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.PopulateAuxiliaryItemList;
begin
  if HasBrackets then
    AddIndent(NotOperatorLeftIndent);
  AddNotOperator;
  AddIndent(NotOperatorRightIndent);
  AddItemList(CriteriaItem, True);
end;

procedure TdxFilterBoxTokenCriteriaListViewInfo.PopulateContent;
var
  I: Integer;
  AItemViewInfo: TdxFilterBoxTokenCriteriaCustomItemViewInfo;
begin
  if ContainsNotOperator then
    PopulateAuxiliaryItemList
  else
    for I := 0 to CriteriaItem.Count - 1 do
    begin
      if I <> 0 then
      begin
        if not CriteriaViewInfo.Options.ItemRemoval then
          AddIndent(RemoveButtonWidth);
        AddBoolOperator;
        AddIndent(RemoveButtonWidth);
      end;
      if CriteriaItem[I].IsItemList then
        AItemViewInfo := AddItemList(TcxFilterCriteriaItemList(CriteriaItem[I]))
      else
        AItemViewInfo :=
          AddItem(TdxFilterBoxTokenCriteriaItem(CriteriaItem[I]));
      if (not Auxiliary or (CriteriaItem.Count > 1)) and AItemViewInfo.NeedRemoveButton
      then
        AddRemoveButton(AItemViewInfo);
    end;
end;

function TdxFilterBoxTokenCriteriaListViewInfo.GetCriteriaItem
  : TcxFilterCriteriaItemList;
begin
  Result := TcxFilterCriteriaItemList(inherited CriteriaItem);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.GetNotOperatorLeftIndent
  : Integer;
begin
  Result := ScaleFactor.Apply(1);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.
  GetNotOperatorRightIndent: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxFilterBoxTokenCriteriaListViewInfo.GetRemoveButtonWidth: Integer;
var
  ARemoveButton: TdxFilterBoxTokenCriteriaRemoveButtonAreaViewInfo;
begin
  if not FRemoveButtonWidthCalculated then
  begin
    ARemoveButton := CreateRemoveButton(Self, False);
    try
      FRemoveButtonWidth := ARemoveButton.Width;
      FRemoveButtonWidthCalculated := True;
    finally
      ARemoveButton.Free;
    end;
  end;
  Result := FRemoveButtonWidth;
end;

{ TdxFilterBoxTokenCriteriaViewInfo }

constructor TdxFilterBoxTokenCriteriaViewInfo.Create
  (AOwner: IdxFilterBoxTokenCriteriaViewInfoOwner);
var
  AScaleFactor: IdxScaleFactor;
  ALookAndFeelContainer: IcxLookAndFeelContainer;
begin
  inherited Create;
  FOwner := AOwner;
  FControl := Owner.Control;
  if Control = nil then
    Exit;
  FCriteria := Owner.Criteria;
  if Supports(Control, IcxLookAndFeelContainer, ALookAndFeelContainer) then
    FPainter := ALookAndFeelContainer.GetLookAndFeel.Painter;
  if Supports(Control, IdxScaleFactor, AScaleFactor) then
    FScaleFactor := AScaleFactor.Value;
  FOptions := TdxFilterBoxTokenCriteriaOptions.Create(Owner.Options);
  FRootViewInfo := TdxFilterBoxTokenCriteriaListViewInfo.Create(Self,
    Criteria.Root);
end;

destructor TdxFilterBoxTokenCriteriaViewInfo.Destroy;
begin
  FreeAndNil(FRootViewInfo);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TdxFilterBoxTokenCriteriaViewInfo.HasMouse(const APoint
  : TPoint): Boolean;
begin
  Result := HasPoint(APoint) and RootViewInfo.HasPoint(APoint);
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.MouseDown(const APoint: TPoint);
begin
  PressedElement := RootViewInfo.GetHitInfo(APoint);
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.MouseLeave;
begin
  HotElement := nil;
  PressedElement := nil;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.MouseMove(const APoint: TPoint);
begin
  HotElement := RootViewInfo.GetHitInfo(APoint);
  if PressedElement <> HotElement then
    PressedElement := nil;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.MouseUp(const APoint: TPoint);
var
  AElement: TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  AElement := PressedElement;
  PressedElement := nil;
  if AElement <> nil then
    AElement.Click;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.Calculate(ALeftBound,
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  CalculateRootViewInfo;
end;

function TdxFilterBoxTokenCriteriaViewInfo.CalculateHeight: Integer;
begin
  Result := ContentHeight;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.CalculateRootViewInfo;
begin
  RootViewInfo.Calculate(Bounds.Left, cxRectCenterVertically(Bounds,
    RootViewInfo.Height).Top);
end;

function TdxFilterBoxTokenCriteriaViewInfo.CalculateViewParams: TcxViewParams;
begin
  Owner.GetViewParams(Result);
end;

function TdxFilterBoxTokenCriteriaViewInfo.CalculateWidth: Integer;
begin
  Result := ContentWidth;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.DoPaint(ACanvas: TcxCanvas);
begin
  RootViewInfo.Paint(ACanvas);
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.DoRightToLeftConversion
  (const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  RootViewInfo.RightToLeftConversion(ABounds);
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetControl: TcxControl;
begin
  Result := FControl;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetCriteria
  : TdxFilterBoxTokenCriteria;
begin
  Result := FCriteria;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetHeight: Integer;
begin
  Result := cxRectHeight(Bounds);
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := FPainter;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetWidth: Integer;
begin
  Result := cxRectWidth(Bounds);
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.AdjustTokenPadding
  (var APadding: TRect);
var
  AMinPadding: TRect;
begin
  AMinPadding := cxGetMinTokenPadding(ScaleFactor,
    Control.UseRightToLeftAlignment);
  APadding.Left := Max(APadding.Left, AMinPadding.Left);
  APadding.Right := Max(APadding.Right, AMinPadding.Right);
  APadding.Top := Max(APadding.Top, AMinPadding.Top);
  APadding.Bottom := Max(APadding.Bottom, AMinPadding.Bottom);
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetContentHeight: Integer;
begin
  Result := ContentMargins.Top + RootViewInfo.Height + ContentMargins.Bottom;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(0, 2, 0, 2));
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetContentWidth: Integer;
begin
  Result := ContentMargins.Left + RootViewInfo.Width + ContentMargins.Right;
end;

function TdxFilterBoxTokenCriteriaViewInfo.GetPainterParams
  : TdxFilterTokenParams;
begin
  if not FPainterParamsCalculated then
  begin
    Painter.GetScaledFilterTokenParams(FPainterParams, ScaleFactor);
    AdjustTokenPadding(FPainterParams.BoolOperatorTextMargins);
    AdjustTokenPadding(FPainterParams.ItemCaptionTextMargins);
    AdjustTokenPadding(FPainterParams.OperatorTextMargins);
    AdjustTokenPadding(FPainterParams.ValueTextMargins);
    FPainterParamsCalculated := True;
  end;
  Result := FPainterParams;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.SetHotElement
  (AValue: TdxFilterBoxTokenCriteriaElementViewInfo);
var
  APrevElement: TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  if AValue <> HotElement then
  begin
    APrevElement := HotElement;
    FHotElement := AValue;
    if APrevElement <> nil then
      APrevElement.UpdateState;
    if HotElement <> nil then
      HotElement.UpdateState;
  end;
end;

procedure TdxFilterBoxTokenCriteriaViewInfo.SetPressedElement
  (AValue: TdxFilterBoxTokenCriteriaElementViewInfo);
var
  APrevElement: TdxFilterBoxTokenCriteriaElementViewInfo;
begin
  if AValue <> PressedElement then
  begin
    APrevElement := PressedElement;
    FPressedElement := AValue;
    if APrevElement <> nil then
      APrevElement.UpdateState;
    if PressedElement <> nil then
      PressedElement.UpdateState;
  end;
end;

{ TdxFilterBoxTokenCriteriaFromToOperator }

function TdxFilterBoxTokenCriteriaFromToOperator.DisplayText: string;
begin
  Result := cxGetResourceString(@cxSFilterFromCaption);
end;

{ TdxFilterBoxTokenCriteriaDateListOperator }

function TdxFilterBoxTokenCriteriaDateListOperator.DisplayText: string;
begin
  Result := '';
end;

function TdxFilterBoxTokenCriteriaDateListOperator.IsUnary: Boolean;
begin
  Result := True;
end;

{ TdxFilterBoxTokenCriteriaItemSubOperator }

constructor TdxFilterBoxTokenCriteriaItemSubOperator.Create
  (AOwner: TdxFilterBoxTokenCriteriaItemSubOperators;
  AKind: TcxFilterOperatorKind);
begin
  inherited Create;
  FOwner := AOwner;
  FKind := AKind;
  FOperator := CreateOperator;
end;

destructor TdxFilterBoxTokenCriteriaItemSubOperator.Destroy;
begin
  FreeAndNil(FOperator);
  inherited Destroy;
end;

function TdxFilterBoxTokenCriteriaItemSubOperator.CreateOperator
  : TcxFilterOperator;
begin
  Result := Owner.Item.GetFilterOperatorClass(Kind).Create(Owner.Item);
end;

function TdxFilterBoxTokenCriteriaItemSubOperator.Kind: TcxFilterOperatorKind;
begin
  Result := FKind;
end;

function TdxFilterBoxTokenCriteriaItemSubOperator.&Operator: TcxFilterOperator;
begin
  Result := FOperator;
end;

{ TdxFilterBoxTokenCriteriaItemSubOperators }

constructor TdxFilterBoxTokenCriteriaItemSubOperators.Create
  (AItem: TdxFilterBoxTokenCriteriaItem);
begin
  inherited Create;
  FItem := AItem;
end;

function TdxFilterBoxTokenCriteriaItemSubOperators.Contains
  (AKind: TcxFilterOperatorKind): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].Kind = AKind then
      Exit(True);
end;

procedure TdxFilterBoxTokenCriteriaItemSubOperators.Add
  (AKind: TcxFilterOperatorKind);
begin
  if not Contains(AKind) then
    inherited Add(CreateSubOperator(AKind));
end;

function TdxFilterBoxTokenCriteriaItemSubOperators.CreateSubOperator
  (AKind: TcxFilterOperatorKind): TdxFilterBoxTokenCriteriaItemSubOperator;
begin
  Result := TdxFilterBoxTokenCriteriaItemSubOperator.Create(Self, AKind);
end;

{ TdxFilterBoxTokenCriteriaItem }

constructor TdxFilterBoxTokenCriteriaItem.Create
  (AOwner: TcxFilterCriteriaItemList; AItemLink: TObject;
  AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
  const ADisplayValue: string);
begin
  inherited Create(AOwner, AItemLink, AOperatorKind, AValue, ADisplayValue);
  FSubOperators := TdxFilterBoxTokenCriteriaItemSubOperators.Create(Self);
end;

destructor TdxFilterBoxTokenCriteriaItem.Destroy;
begin
  FreeAndNil(FSubOperators);
  inherited Destroy;
end;

procedure TdxFilterBoxTokenCriteriaItem.AssignSubOperators
  (ASource: TdxFilterBoxTokenCriteriaItemSubOperators);
var
  I: Integer;
begin
  for I := 0 to ASource.Count - 1 do
    AddSubOperator(ASource[I].Kind);
end;

procedure TdxFilterBoxTokenCriteriaItem.AddSubOperator
  (AKind: TcxFilterOperatorKind);
begin
  SubOperators.Add(AKind);
end;

function TdxFilterBoxTokenCriteriaItem.GetFilterOperatorClass
  : TcxFilterOperatorClass;
begin
  case ExtOperatorKind of
    eokDateList:
      Result := TdxFilterBoxTokenCriteriaDateListOperator;
    eokFromTo:
      Result := TdxFilterBoxTokenCriteriaFromToOperator;
  else
    Result := inherited GetFilterOperatorClass;
  end;
end;

procedure TdxFilterBoxTokenCriteriaItem.SetExtOperatorKind
  (AValue: TdxFilterBoxTokenCriteriaExtOperatorKind);
begin
  if ExtOperatorKind <> AValue then
  begin
    FExtOperatorKind := AValue;
    OperatorKind := foEqual;
    RecreateOperator;
  end;
end;

{ TdxFilterBoxTokenCriteriaCustomMerger }

function TdxFilterBoxTokenCriteriaCustomMerger.Merge
  (ACriteria: TdxFilterBoxTokenCriteria): Boolean;
var
  AMergedCriteria: TdxFilterBoxTokenCriteria;
begin
  AMergedCriteria := TdxFilterBoxTokenCriteria.Create(ACriteria.SourceCriteria);
  try
    PopulateMergedCriteria(AMergedCriteria, ACriteria, Result);
    if Result then
      LoadCriteria(ACriteria, AMergedCriteria);
  finally
    AMergedCriteria.Free;
  end;
end;

function TdxFilterBoxTokenCriteriaCustomMerger.CloneExpressionItem
  (AItem: TcxFilterCriteriaItem; AParent: TcxFilterCriteriaItemList)
  : TcxFilterCriteriaItem;
begin
  Result := AParent.AddExpressionItem(AItem.ItemLink, AItem.OperatorKind,
    AItem.Expression, AItem.DisplayValue);
end;

function TdxFilterBoxTokenCriteriaCustomMerger.CloneItem
  (AItem: TdxFilterBoxTokenCriteriaItem; AParent: TcxFilterCriteriaItemList)
  : TdxFilterBoxTokenCriteriaItem;
var
  ACloneItem: TcxFilterCriteriaItem;
begin
  ACloneItem := AParent.AddItem(AItem.ItemLink, AItem.OperatorKind, AItem.Value,
    AItem.DisplayValue);
  Result := TdxFilterBoxTokenCriteriaItem(ACloneItem);
  Result.ExtOperatorKind := AItem.ExtOperatorKind;
  Result.AssignSubOperators(AItem.SubOperators);
end;

function TdxFilterBoxTokenCriteriaCustomMerger.CloneList(AList,
  AParent: TcxFilterCriteriaItemList): TcxFilterCriteriaItemList;
begin
  Result := AParent.AddItemList(AList.BoolOperatorKind);
end;

procedure TdxFilterBoxTokenCriteriaCustomMerger.PopulateMergedCriteria
  (ACriteria, ASourceCriteria: TdxFilterBoxTokenCriteria;
  out AHasMerge: Boolean);
begin
  ACriteria.BeginUpdate;
  try
    ACriteria.Root.BoolOperatorKind := ASourceCriteria.Root.BoolOperatorKind;
    PopulateMergedList(ACriteria.Root, ASourceCriteria.Root, AHasMerge);
  finally
    ACriteria.EndUpdate;
  end;
end;

procedure TdxFilterBoxTokenCriteriaCustomMerger.LoadCriteria(ACriteria,
  ASourceCriteria: TdxFilterBoxTokenCriteria);
begin
  ACriteria.BeginUpdate;
  try
    ACriteria.Clear;
    ACriteria.Root.BoolOperatorKind := ASourceCriteria.Root.BoolOperatorKind;
    LoadList(ACriteria.Root, ASourceCriteria.Root);
  finally
    ACriteria.EndUpdate;
  end;
end;

procedure TdxFilterBoxTokenCriteriaCustomMerger.LoadList(AList,
  ASourceList: TcxFilterCriteriaItemList);
var
  I: Integer;
  ASourceChild: TcxCustomFilterCriteriaItem;
  ASourceChildItem: TdxFilterBoxTokenCriteriaItem absolute ASourceChild;
  ASourceChildList: TcxFilterCriteriaItemList absolute ASourceChild;
begin
  for I := 0 to ASourceList.Count - 1 do
  begin
    ASourceChild := ASourceList[I];
    if ASourceChild.IsItemList then
      LoadList(CloneList(ASourceChildList, AList), ASourceChildList)
    else if ASourceChildItem.Expression <> '' then
      CloneExpressionItem(ASourceChildItem, AList)
    else
      CloneItem(ASourceChildItem, AList);
  end;
end;

{ TdxFilterBoxTokenCriteriaListMerger }

procedure TdxFilterBoxTokenCriteriaListMerger.PopulateMergedCriteria(ACriteria,
  ASourceCriteria: TdxFilterBoxTokenCriteria; out AHasMerge: Boolean);
var
  AOutParam: Boolean;
  ASourceRoot: TcxFilterCriteriaItemList;
begin
  AHasMerge := False;
  ASourceRoot := ASourceCriteria.Root;
  while (ASourceRoot.Count = 1) and ASourceRoot[0].IsItemList and
    (TcxFilterCriteriaItemList(ASourceRoot[0]).BoolOperatorKind
    in [fboAnd, fboOr]) do
  begin
    ASourceRoot := TcxFilterCriteriaItemList(ASourceRoot[0]);
    AHasMerge := True;
  end;
  ACriteria.Root.BoolOperatorKind := ASourceRoot.BoolOperatorKind;
  PopulateMergedList(ACriteria.Root, ASourceRoot, AOutParam);
  AHasMerge := AHasMerge or AOutParam;
end;

procedure TdxFilterBoxTokenCriteriaListMerger.PopulateMergedList(AList,
  ASourceList: TcxFilterCriteriaItemList; out AHasMerge: Boolean);
var
  I: Integer;
  AOutParam: Boolean;
  ASourceChild: TcxCustomFilterCriteriaItem;
  ASourceChildItem: TdxFilterBoxTokenCriteriaItem absolute ASourceChild;
  ASourceChildList: TcxFilterCriteriaItemList absolute ASourceChild;
  AChildItemList: TcxFilterCriteriaItemList;
begin
  AHasMerge := False;
  for I := 0 to ASourceList.Count - 1 do
  begin
    ASourceChild := ASourceList[I];
    if ASourceChild.IsItemList then
    begin
      if NeedMerge(ASourceChildList, AList) then
      begin
        AChildItemList := AList;
        AHasMerge := True;
      end
      else
        AChildItemList := CloneList(ASourceChildList, AList);
      PopulateMergedList(AChildItemList, ASourceChildList, AOutParam);
      AHasMerge := AHasMerge or AOutParam;
    end
    else if ASourceChildItem.Expression <> '' then
      CloneExpressionItem(ASourceChildItem, AList)
    else
      CloneItem(ASourceChildItem, AList);
  end;
end;

function TdxFilterBoxTokenCriteriaListMerger.NeedMerge(AList,
  AParentList: TcxFilterCriteriaItemList): Boolean;
begin
  Result := (AList.BoolOperatorKind in [fboAnd, fboOr]) and
    ((AList.Count = 1) or
    (AList.BoolOperatorKind = AParentList.BoolOperatorKind));
end;

{ TdxFilterBoxTokenCriteriaItemMerger }

function TdxFilterBoxTokenCriteriaItemMerger.CreateInfos
  : TdxFilterBoxTokenCriteriaItemMergeInfos;
begin
  Result := TdxFilterBoxTokenCriteriaItemMergeInfos.Create;
  PopulateInfos(Result);
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.PopulateInfos
  (AInfos: TdxFilterBoxTokenCriteriaItemMergeInfos);
begin
  AInfos.Add(MergeInListProc, SupportsMergeInList);
  AInfos.Add(MergeNotInListProc, SupportsMergeNotInList);
  AInfos.Add(MergeDateProc, SupportsMergeDate);
  AInfos.Add(MergeFromToProc, SupportsMergeFromTo);
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.PopulateMergedList(AList,
  ASourceList: TcxFilterCriteriaItemList; out AHasMerge: Boolean);
var
  I: Integer;
  AOutParam: Boolean;
  AInfo: TdxFilterBoxTokenCriteriaItemMergeInfo;
  AInfos: TdxFilterBoxTokenCriteriaItemMergeInfos;
  ASourceChild: TcxCustomFilterCriteriaItem;
  ASourceChildItem: TdxFilterBoxTokenCriteriaItem absolute ASourceChild;
  ASourceChildList: TcxFilterCriteriaItemList absolute ASourceChild;
begin
  AHasMerge := False;
  AInfos := CreateInfos;
  try
    for I := 0 to ASourceList.Count - 1 do
    begin
      ASourceChild := ASourceList[I];
      if ASourceChild.IsItemList then
      begin
        PopulateMergedList(CloneList(ASourceChildList, AList), ASourceChildList,
          AOutParam);
        AHasMerge := AHasMerge or AOutParam;
      end
      else if ASourceChildItem.Expression <> '' then
        CloneExpressionItem(ASourceChildItem, AList)
      else
      begin
        AInfo := AInfos.Find(ASourceChildItem);
        if AInfo <> nil then
          if AInfo.Merge(ASourceChildItem) then
            AHasMerge := True
          else
            AInfo.RegisterMergedItem(CloneItem(ASourceChildItem, AList))
        else
          CloneItem(ASourceChildItem, AList);
      end;
    end;
  finally
    AInfos.Free;
  end;
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.MergeDateProc(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem);
begin
  if AMergeItem.ExtOperatorKind <> eokDateList then
    if AItem.OperatorKind = AMergeItem.OperatorKind then
      Exit
    else
    begin
      AMergeItem.AddSubOperator(AMergeItem.OperatorKind);
      AMergeItem.Value := Null;
      AMergeItem.DisplayValue := '';
      AMergeItem.ExtOperatorKind := eokDateList;
    end;
  AMergeItem.AddSubOperator(AItem.OperatorKind);
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.MergeFromToProc(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem);
var
  AValue: Variant;
  ADisplayValue: string;
begin
  if AItem.OperatorKind = foGreaterEqual then
  begin
    AValue := VarBetweenArrayCreate(AItem.Value, AMergeItem.Value);
    ADisplayValue := AItem.DisplayValue + ';' + AMergeItem.DisplayValue;
  end
  else
  begin
    AValue := VarBetweenArrayCreate(AMergeItem.Value, AItem.Value);
    ADisplayValue := AMergeItem.DisplayValue + ';' + AItem.DisplayValue;
  end;
  AMergeItem.ExtOperatorKind := eokFromTo;
  AMergeItem.Value := AValue;
  AMergeItem.DisplayValue := ADisplayValue;
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.MergeInListProc(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem);
var
  I: Integer;
  AValue: Variant;
begin
  if AMergeItem.OperatorKind = foEqual then
  begin
    AMergeItem.OperatorKind := foInList;
    AValue := VarListArrayCreate(AMergeItem.Value);
  end
  else
    AValue := AMergeItem.Value;
  if AItem.OperatorKind = foEqual then
    VarListArrayAddValue(AValue, AItem.Value)
  else
    for I := VarArrayLowBound(AItem.Value, 1) to VarArrayHighBound
      (AItem.Value, 1) do
      VarListArrayAddValue(AValue, AItem.Value[I]);
  AMergeItem.Value := AValue;
  AMergeItem.DisplayValue := AMergeItem.DisplayValue + ';' + AItem.DisplayValue;
end;

procedure TdxFilterBoxTokenCriteriaItemMerger.MergeNotInListProc(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem);
var
  I: Integer;
  AValue: Variant;
begin
  if AMergeItem.OperatorKind = foNotEqual then
  begin
    AMergeItem.OperatorKind := foNotInList;
    AValue := VarListArrayCreate(AMergeItem.Value);
  end
  else
    AValue := AMergeItem.Value;
  if AItem.OperatorKind = foNotEqual then
    VarListArrayAddValue(AValue, AItem.Value)
  else
    for I := VarArrayLowBound(AItem.Value, 1) to VarArrayHighBound
      (AItem.Value, 1) do
      VarListArrayAddValue(AValue, AItem.Value[I]);
  AMergeItem.Value := AValue;
  AMergeItem.DisplayValue := AMergeItem.DisplayValue + ';' + AItem.DisplayValue;
end;

function TdxFilterBoxTokenCriteriaItemMerger.SupportsMergeDate(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem): Boolean;
begin
  Result := (AItem.Parent.BoolOperatorKind = fboOr) and
    (AItem.OperatorKind in [foYesterday .. foInFuture]);
end;

function TdxFilterBoxTokenCriteriaItemMerger.SupportsMergeFromTo(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem): Boolean;
begin
  Result := (AItem.Parent.BoolOperatorKind = fboAnd) and
    (AItem.OperatorKind in [foLessEqual, foGreaterEqual]) and
    ((AMergeItem = nil) or (AMergeItem.ExtOperatorKind <> eokFromTo) and
    (AItem.OperatorKind = foLessEqual) and
    (AMergeItem.OperatorKind = foGreaterEqual) and
    (VarCompare(AItem.Value, AMergeItem.Value) >= 0) or
    (AItem.OperatorKind = foGreaterEqual) and
    (AMergeItem.OperatorKind = foLessEqual) and
    (VarCompare(AItem.Value, AMergeItem.Value) <= 0));
end;

function TdxFilterBoxTokenCriteriaItemMerger.SupportsMergeInList(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem): Boolean;
begin
  Result := (AItem.Parent.BoolOperatorKind = fboOr) and
    (AItem.ExtOperatorKind = eokNone) and
    (AItem.OperatorKind in [foEqual, foInList]) and not AItem.Operator.IsUnary;
end;

function TdxFilterBoxTokenCriteriaItemMerger.SupportsMergeNotInList(AItem,
  AMergeItem: TdxFilterBoxTokenCriteriaItem): Boolean;
begin
  Result := (AItem.Parent.BoolOperatorKind = fboAnd) and
    (AItem.OperatorKind in [foNotEqual, foNotInList]) and
    not AItem.Operator.IsUnary;
end;

{ TdxFilterBoxTokenCriteriaItemMergeInfo }

constructor TdxFilterBoxTokenCriteriaItemMergeInfo.Create
  (AMergeProc: TdxFilterBoxTokenCriteriaItemMergeProc;
  ASupportsMergeFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc);
begin
  inherited Create;
  FMergeProc := AMergeProc;
  FSupportsMergeFunc := ASupportsMergeFunc;
  FMergedItems := TDictionary<TObject, TdxFilterBoxTokenCriteriaItem>.Create;
end;

destructor TdxFilterBoxTokenCriteriaItemMergeInfo.Destroy;
begin
  FreeAndNil(FMergedItems);
  inherited Destroy;
end;

function TdxFilterBoxTokenCriteriaItemMergeInfo.Merge
  (AItem: TdxFilterBoxTokenCriteriaItem): Boolean;
var
  AMergedItem: TdxFilterBoxTokenCriteriaItem;
begin
  Result := MergedItems.TryGetValue(AItem.ItemLink, AMergedItem);
  if Result then
    MergeProc(AItem, AMergedItem);
end;

procedure TdxFilterBoxTokenCriteriaItemMergeInfo.RegisterMergedItem
  (AItem: TdxFilterBoxTokenCriteriaItem);
begin
  if not MergedItems.ContainsKey(AItem.ItemLink) then
    MergedItems.Add(AItem.ItemLink, AItem);
end;

function TdxFilterBoxTokenCriteriaItemMergeInfo.SupportsMerge
  (AItem: TdxFilterBoxTokenCriteriaItem): Boolean;
var
  AMergedItem: TdxFilterBoxTokenCriteriaItem;
begin
  MergedItems.TryGetValue(AItem.ItemLink, AMergedItem);
  Result := SupportsMergeFunc(AItem, AMergedItem);
end;

{ TdxFilterBoxTokenCriteriaItemMergeInfos }

procedure TdxFilterBoxTokenCriteriaItemMergeInfos.Add
  (AMergeProc: TdxFilterBoxTokenCriteriaItemMergeProc;
  AIsItemSupportedFunc: TdxFilterBoxTokenCriteriaItemSupportsMergeFunc);
begin
  inherited Add(TdxFilterBoxTokenCriteriaItemMergeInfo.Create(AMergeProc,
    AIsItemSupportedFunc));
end;

function TdxFilterBoxTokenCriteriaItemMergeInfos.Find
  (AItem: TdxFilterBoxTokenCriteriaItem)
  : TdxFilterBoxTokenCriteriaItemMergeInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.SupportsMerge(AItem) then
      Exit;
  end;
  Result := nil;
end;

function TdxFilterBoxTokenCriteriaItemMergeInfos.GetItem(AIndex: Integer)
  : TdxFilterBoxTokenCriteriaItemMergeInfo;
begin
  Result := TdxFilterBoxTokenCriteriaItemMergeInfo(inherited Items[AIndex]);
end;

{ TcxFilterCriteriaSimplificationHelper }

constructor TdxFilterBoxTokenCriteriaSimplificator.Create
  (ACriteria: TdxFilterBoxTokenCriteria);
begin
  inherited Create;
  FCriteria := ACriteria;
end;

procedure TdxFilterBoxTokenCriteriaSimplificator.Simplify;
begin
  Criteria.BeginUpdate;
  try
    RemoveEmptyItems;
    Merge;
  finally
    Criteria.EndUpdate;
  end;
end;

function TdxFilterBoxTokenCriteriaSimplificator.DoMerge
  (AMergerClass: TdxFilterBoxTokenCriteriaCustomMergerClass): Boolean;
var
  AMerger: TdxFilterBoxTokenCriteriaCustomMerger;
begin
  AMerger := AMergerClass.Create;
  try
    Result := AMerger.Merge(Criteria);
  finally
    AMerger.Free;
  end;
end;

procedure TdxFilterBoxTokenCriteriaSimplificator.Merge;
begin
  repeat
    MergeLists;
  until not MergeItems;
end;

function TdxFilterBoxTokenCriteriaSimplificator.MergeItems: Boolean;
begin
  Result := DoMerge(TdxFilterBoxTokenCriteriaItemMerger);
end;

function TdxFilterBoxTokenCriteriaSimplificator.MergeLists: Boolean;
begin
  Result := DoMerge(TdxFilterBoxTokenCriteriaListMerger);
end;

procedure TdxFilterBoxTokenCriteriaSimplificator.RemoveEmptyItems
  (AList: TcxFilterCriteriaItemList = nil);
var
  I: Integer;
begin
  Criteria.BeginUpdate;
  try
    if AList = nil then
      AList := Criteria.Root;
    for I := AList.Count - 1 downto 0 do
    begin
      if AList[I].IsEmpty then
        AList[I].Free
      else if AList[I].IsItemList then
        RemoveEmptyItems(TcxFilterCriteriaItemList(AList[I]));
    end;
    if (AList <> Criteria.Root) and AList.IsEmpty then
      AList.Free;
  finally
    Criteria.EndUpdate;
  end;
end;

{ TdxFilterBoxTokenCriteria }

constructor TdxFilterBoxTokenCriteria.Create(ASourceCriteria
  : TcxDataFilterCriteria);
begin
  FSourceCriteria := ASourceCriteria;
  inherited Create(SourceCriteria.DataController);
end;

procedure TdxFilterBoxTokenCriteria.Simplify;
var
  ASimplificator: TdxFilterBoxTokenCriteriaSimplificator;
begin
  ASimplificator := CreateSimplificator;
  try
    ASimplificator.Simplify;
  finally
    ASimplificator.Free;
  end;
end;

procedure TdxFilterBoxTokenCriteria.UpdateBySourceCriteria;
begin
  BeginUpdate;
  try
    Clear;
    Assign(SourceCriteria);
    Simplify;
  finally
    EndUpdate;
  end;
end;

procedure TdxFilterBoxTokenCriteria.UpdateSourceCriteria;
begin
  SourceCriteria.BeginUpdate;
  try
    SourceCriteria.Clear;
    PopulateSourceCriteria;
  finally
    SourceCriteria.EndUpdate;
  end;
end;

procedure TdxFilterBoxTokenCriteria.CloneItemToSourceCriteria
  (AItem: TdxFilterBoxTokenCriteriaItem; AParent: TcxFilterCriteriaItemList);
var
  I: Integer;
  APos: Integer;
begin
  case AItem.ExtOperatorKind of
    eokDateList:
      for I := 0 to AItem.SubOperators.Count - 1 do
        AParent.AddItem(AItem.ItemLink, AItem.SubOperators[I].Kind, Null, '');
    eokFromTo:
      begin
        APos := 1;
        AParent.AddItem(AItem.ItemLink, foGreaterEqual, AItem.Value[0],
          ExtractFilterDisplayValue(AItem.DisplayValue, APos));
        AParent.AddItem(AItem.ItemLink, foLessEqual, AItem.Value[1],
          ExtractFilterDisplayValue(AItem.DisplayValue, APos));
      end
  else
    AParent.AddItem(AItem.ItemLink, AItem.OperatorKind, AItem.Value,
      AItem.DisplayValue);
  end;
end;

function TdxFilterBoxTokenCriteria.CreateSimplificator
  : TdxFilterBoxTokenCriteriaSimplificator;
begin
  Result := TdxFilterBoxTokenCriteriaSimplificator.Create(Self);
end;

function TdxFilterBoxTokenCriteria.GetFilterCaption: string;
begin
  Result := '';
end;

function TdxFilterBoxTokenCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TdxFilterBoxTokenCriteriaItem;
end;

function TdxFilterBoxTokenCriteria.IsInternal: Boolean;
begin
  Result := True;
end;

procedure TdxFilterBoxTokenCriteria.PopulateSourceCriteria;
begin
  SourceCriteria.BeginUpdate;
  try
    SourceCriteria.Root.BoolOperatorKind := Root.BoolOperatorKind;
    PopulateSourceCriteriaList(SourceCriteria.Root, Root);
  finally
    SourceCriteria.EndUpdate;
  end;
end;

procedure TdxFilterBoxTokenCriteria.PopulateSourceCriteriaList(AList,
  AOriginalList: TcxFilterCriteriaItemList);
var
  I: Integer;
  AOriginalChild: TcxCustomFilterCriteriaItem;
  AOriginalChildItem: TdxFilterBoxTokenCriteriaItem absolute AOriginalChild;
  AOriginalChildList: TcxFilterCriteriaItemList absolute AOriginalChild;
begin
  for I := 0 to AOriginalList.Count - 1 do
  begin
    AOriginalChild := AOriginalList[I];
    if AOriginalChild.IsItemList then
      PopulateSourceCriteriaList
        (AList.AddItemList(AOriginalChildList.BoolOperatorKind),
        AOriginalChildList)
    else if AOriginalChildItem.Expression <> '' then
      AList.AddExpressionItem(AOriginalChildItem.ItemLink,
        AOriginalChildItem.OperatorKind, AOriginalChildItem.Expression,
        AOriginalChildItem.DisplayValue)
    else
      CloneItemToSourceCriteria(AOriginalChildItem, AList);
  end;
end;

end.
