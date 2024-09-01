unit sComboBoxes;
{$I sDefs.inc}
//{$DEFINE LOGGED}
//{$DEFINE NOTSKINNEDCONTENT}

interface

uses
  ExtCtrls, Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, ImgList, Dialogs, Forms,
  {$IFDEF DELPHI7UP} ListActns, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5} types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFDEF DELPHI_10RIO} System.Generics.Collections, {$ENDIF}
  sCommonData, sDefaults, sConst, acntUtils, sGraphUtils, acSBUtils, acAlphaImageList, sSpeedButton, sSkinProvider,
  sScrollBox, acntTypes, sComboBox, sLabel;


type
{$IFNDEF NOTFORHELP}

{$IFNDEF D2010}
  TCustomData = Pointer;
{$ENDIF}

{$IFNDEF DELPHI7UP}
  TListControlItems = class;
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);
  TComboBoxExStyle = (csExDropDown, csExSimple, csExDropDownList);
  TAutoCompleteOption = (acoAutoSuggest, acoAutoAppend, acoSearch,
    acoFilterPrefixes, acoUseTab, acoUpDownKeyDropsList, acoRtlReading);
  TAutoCompleteOptions = set of TAutoCompleteOption;
  TComboBoxExStyleEx = (csExCaseSensitive, csExNoEditImage, csExNoEditImageIndent,
                        csExNoSizeLimit, csExPathWordBreak);
  TComboBoxExStyles = set of TComboBoxExStyleEx;

  TListControlItem = class(TCollectionItem)
  private
    FListControlItems: TListControlItems;
  protected
    FCaption: String;
    FData: Pointer;
    FImageIndex: TImageIndex;
    procedure Changed;
    function GetDisplayName: String; override;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: Pointer); virtual;
    procedure SetImageIndex(const Value: TImageIndex); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write SetData;
  published
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  TListItemsSortType = (stNone, stData, stText, stBoth);
  TListCompareEvent = function(List: TListControlItems;
    Item1, Item2: TListControlItem): Integer of object;
  TListItemsCompare = function(List: TListControlItems;
    Index1, Index2: Integer): Integer;

  TListControlItems = class(TOwnedCollection)
  private
    FCaseSensitive: Boolean;
    FSortType: TListItemsSortType;
    FOnCompare: TListCompareEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    function GetListItem(const Index: Integer): TListControlItem;
    procedure QuickSort(L, R: Integer; SCompare: TListItemsCompare);
    procedure SetSortType(const Value: TListItemsSortType);
  protected
    function CompareItems(I1, I2: TListControlItem): Integer; virtual;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);  
    function Add: TListControlItem;
    procedure Sort;
    procedure CustomSort(Compare: TListItemsCompare);
    property Items[const Index: Integer]: TListControlItem read GetListItem; default;
  published
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property SortType: TListItemsSortType read FSortType write SetSortType default stNone;
    property OnCompare: TListCompareEvent read FOnCompare write FOnCompare;
  end;
{$ENDIF} // IFNDEF DELPHI7UP

  TacComboExItem = class(TListControlItem)
  private
    FSelectedImageIndex: TImageIndex;
    FOverlayImageIndex: TImageIndex;
    FIndent: Integer;
  protected
    procedure SetOverlayImageIndex(const Value: TImageIndex); virtual;
    procedure SetSelectedImageIndex(const Value: TImageIndex); virtual;
    procedure SetCaption(const Value: String); override;
    procedure SetData(const Value: TCustomData); override;
    procedure SetDisplayName(const Value: String); override;
    procedure SetImageIndex(const Value: TImageIndex); override;
    procedure SetIndex(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Indent: Integer read FIndent write FIndent default -1;
    property OverlayImageIndex: TImageIndex read FOverlayImageIndex write SetOverlayImageIndex default -1;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex write SetSelectedImageIndex default -1;
  end;


  TacComboExItems = class(TListControlItems)
  private
    function GetComboItem(const Index: Integer): TacComboExItem;
  protected
{$IFDEF DELPHI7UP}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
{$ENDIF}
    procedure SetItem(const Index: Integer); virtual;
  public
    function Add: TacComboExItem;
    function AddItem(const Caption: string; const ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent: Integer; Data: TCustomData): TacComboExItem;
    function Insert(Index: Integer): TacComboExItem;
    property ComboItems[const Index: Integer]: TacComboExItem read GetComboItem;
  end;


  TacCustomComboBoxStrings = class(TStrings)
  private
    FComboBox: TacCustomCombo;
  protected
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: boolean); override;
    property ComboBox: TacCustomCombo read FComboBox write FComboBox;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
  end;

  TsCustomComboBoxEx = class;

  TacComboBoxExStrings = class(TacCustomComboBoxStrings)
  private
    FItems: TacComboExItems;
    function GetSortType: TListItemsSortType;
    procedure SetItems(const Value: TacComboExItems);
    procedure SetSortType(const Value: TListItemsSortType);
  protected
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: boolean); override;
  public
    constructor Create(Owner: TsCustomComboBoxEx); reintroduce;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddItem(const Caption: string; const ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent: Integer; Data: TCustomData): TacComboExItem;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1: Integer; Index2: Integer); override;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function IndexOf(const S: String): Integer; override;
{$IFDEF DELPHI7UP}
    function IndexOfName(const Name: String): Integer; override;
{$ENDIF}    
    procedure Insert(Index: Integer; const S: String); override;
    procedure Move(CurIndex: Integer; NewIndex: Integer); override;
    property SortType: TListItemsSortType read GetSortType write SetSortType;
    property ItemsEx: TacComboExItems read FItems write SetItems;
  end;


  TsCustomComboBoxEx = class(TacCustomCombo)
  private
    FItemsEx: TacComboExItems;
    FStyle: TComboBoxExStyle;
    FAutoCompleteOptions: TAutoCompleteOptions;
    FStyleEx: TComboBoxExStyles;
{$IFDEF DELPHI7UP}
    function IsItemsExStored: boolean;
{$ENDIF}
    function GetSelectedItem: TacComboExItem;
    procedure SetStyle(Value: TComboBoxExStyle);
    procedure SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
    procedure UpdateAutoComplete;
    procedure SetStyleEx(const Value: TComboBoxExStyles);
    procedure UpdateStyleEx;
  protected
    FGlyphIndex: integer;
    procedure PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
{$IFDEF DELPHI7UP}
    function GetActionLinkClass: TControlActionLinkClass; override;
{$ENDIF}
    function ItemTextDefined(AIndex: integer): boolean; override;
    function AllowBtnStyle: boolean; override;
    procedure PaintNCArea(DC: hdc); override;
    function GetEditText(AIndex: integer): acString; override;
    function GetItemCount: integer; override;
    procedure SetItemsEx(const Value: TacComboExItems);
    function IsOwnerDraw: boolean; override;
    function ImageExists: boolean; override;
    function IsCaseSensitive: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function IsDropDownList: boolean; override;
    property SelectedItem: TacComboExItem read GetSelectedItem;
    property AutoCompleteOptions: TAutoCompleteOptions read FAutoCompleteOptions write SetAutoCompleteOptions default [acoAutoAppend];
    property ItemsEx: TacComboExItems read FItemsEx write SetItemsEx {$IFDEF DELPHI7UP}stored IsItemsExStored {$ENDIF};
    property Style: TComboBoxExStyle read FStyle write SetStyle default csExDropDown;
    property StyleEx: TComboBoxExStyles read FStyleEx write SetStyleEx default [];
  end;
  {$IFDEF DELPHI7UP}
  TacComboBoxExActionLink = class(TListActionLink)
  protected
    procedure AddItem(AnItem: TListControlItem); override;
    procedure AddItem(ACaption: String; AImageIndex: Integer; DataPtr: TCustomData); override;
  end;
  {$ENDIF}
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsComboBoxEx = class(TsCustomComboBoxEx)
  published
{$IFNDEF NOTFORHELP}
  {$IFDEF DELPHI7UP}
    property AutoCompleteOptions;
  {$ENDIF}
    property ItemsEx;
    property Style;
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
  {$IFDEF DELPHI7UP}
    property DirectInput;
  {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
{$IFDEF DELPHI7UP}
    property DropInplace;
{$ENDIF}    
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;

    property ItemIndex default -1;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text stored TextStored;
  {$IFDEF DELPHI7UP}
    property TextHint;
  {$ENDIF}
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Images;
    property DropDownCount;
    property StyleEx;
{$ENDIF} // NOTFORHELP
    property BoundLabel;
    property DisabledKind;
    property ShowButton;
    property SkinData;
    property ReadOnly;
  end;


{$IFNDEF NOTFORHELP}
    {$IFNDEF ALITE}
  TsColorBoxStyles = (cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors, cbSavedColors, cbSkinColors);
  TsColorBoxStyle = set of TsColorBoxStyles;

  TsCustomColorBox = class;
  TGetColorsEvent = procedure(Sender: TsCustomColorBox; Items: TStrings) of object;
  TOnColorName    = procedure(Sender: TsCustomColorBox; Value: TColor; var ColorName: string) of object;

  TacColorPopupMode = (pmColorList, pmPickColor);

  TsCustomColorBox = class(TsCustomComboBoxEx)
  private
    FSelectedColor,
    FNoneColorColor,
    FDefaultColorColor: TColor;

    FUseAlpha,
    FInPopulating,
    FShowColorName,
    FNeedToPopulate: boolean;

    FMargin,
    FColorRectHeight,
    FColorRectWidth: integer;

    FStyle: TsColorBoxStyle;
    FOnColorName: TOnColorName;
    FOnGetColors: TGetColorsEvent;
    FPopupMode: TacColorPopupMode;
    FOnColorPreview: TacColorPreview;
    function GetSelected: TColor;
    function GetColor(Index: Integer): TColor;
    procedure SetDefaultColorColor(const Value: TColor);
    procedure SetNoneColorColor   (const Value: TColor);
    procedure SetShowColorName    (const Value: boolean);
    procedure SetSelected         (const AColor: TColor);
    procedure SetPopupMode        (const Value: TacColorPopupMode);
    procedure SetInteger          (const Index, Value: integer);
  protected
    function AllowDropDownList: boolean; override;
    procedure PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function PickCustomColor: boolean;
    procedure PopulateList;
    procedure SetStyle(AStyle: TsColorBoxStyle); reintroduce;
    function UpdateAlpha(aColor: TColor): TColor;
    function GetEditText(AIndex: integer): acString; override;
    function HandleKeyDown(var ACharCode: word): boolean; override;
  public
    procedure Select; override;
    procedure PopupWindowShow; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWnd; override;
    property ColorNames[Index: Integer]: acString read GetText;
    property Colors[Index: Integer]: TColor read GetColor;
  published
    property Style: TsColorBoxStyle read FStyle write SetStyle default [cbStandardColors, cbExtendedColors, cbSystemColors];
    property ShowColorName: boolean read FShowColorName write SetShowColorName default True;
    property UseAlpha: boolean read FUseAlpha write FUseAlpha default False;

    property Margin:          integer index 0 read FMargin          write SetInteger default 0;
    property ColorRectHeight: integer index 1 read FColorRectHeight write SetInteger default 14;
    property ColorRectWidth:  integer index 2 read FColorRectWidth  write SetInteger default 21;

    property Selected:          TColor read GetSelected        write SetSelected          default clBlack;
    property DefaultColorColor: TColor read FDefaultColorColor write SetDefaultColorColor default clBlack;
    property NoneColorColor:    TColor read FNoneColorColor    write SetNoneColorColor    default clBlack;

    property PopupMode: TacColorPopupMode read FPopupMode write SetPopupMode default pmColorList;

    property OnColorName: TOnColorName    read FOnColorName write FOnColorName;
    property OnGetColors: TGetColorsEvent read FOnGetColors write FOnGetColors;
    property OnColorPreview: TacColorPreview read FOnColorPreview write FOnColorPreview;
  end;
    {$ENDIF} // ALITE
{$ENDIF} // NOTFORHELP


{$IFNDEF ALITE}
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsColorBox = class(TsCustomColorBox)
  {$IFNDEF NOTFORHELP}
  published
    property Align;
    property DisabledKind;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DropDownCount;
{$IFDEF DELPHI7UP}
    property DropInplace;
{$ENDIF}
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
  {$IFDEF DELPHI7UP}
    property TextHint;
  {$ENDIF}
    property Visible;
    property OnChange;
    property OnCloseUp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  {$ENDIF} // NOTFORHELP
    property Style;
    property Margin;
    property Selected;
    property ShowColorName;
    property DefaultColorColor;
    property NoneColorColor;
    property SkinData;
    property ReadOnly;
  end;
{$ENDIF} // ALITE


{$IFNDEF NOTFORHELP}
  TsSkinSelector = class;


  TacSkinSelectBtn = class(TsSpeedButton)
  public
    FDoClick: boolean;
    procedure Click; override;
    function PrepareCache: boolean; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
  end;


  TacSkinSelectForm = class(TForm)
  protected
    BtnIndex,
    ItemCount,
    ItemWidth,
    ItemHeight: integer;

    sp: TsSkinProvider;
    ScrollBox: TsScrollBox;
    Selector: TsSkinSelector;
    ImgList: TsVirtualImageList;
    UpdateFlags: set of TacUpdateFlags;

    procedure InitFormData;
    procedure UpdateBoxSize;
    procedure UpdateControls;
    procedure UpdateHotControl;
    procedure SetBtnIndex(NewIndex: integer);
    procedure ClickBtn(AButton: TacSkinSelectBtn);
  public
    destructor Destroy; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;


  TacThumbSize = (tsSmall, tsMedium, tsBig);
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsSkinSelector = class(TsCustomComboBoxEx)
{$IFNDEF NOTFORHELP}
  private
    FColCount,
    FRowCount,
    FItemMargin,
    FMinItemWidth: byte;
    FShowNoSkin,
    FDontChangeManager: boolean;
    Form: TacSkinSelectForm;
    FThumbSize: TacThumbSize;
    procedure SetThumbSize(const Value: TacThumbSize);
    procedure SetShowNoSkin(const Value: boolean);
    procedure SetColCount(const Value: byte);
    procedure SetRowCount(const Value: byte);
  protected
    procedure UpdateItemIndex;
    procedure UpdateList(Immediate: boolean);
    function AllowDropDownList: boolean; override;
    function GetEditText(AIndex: integer): acString; override;
    function ItemTextDefined(AIndex: integer): boolean; override;
    function HandleKeyDown(var ACharCode: word): boolean; override;
    procedure PopulateList(Immediate: boolean);
    procedure SetIndex(BtnIndex: integer; Step: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    procedure UpdateForm;
    procedure PopupWindowShow; override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI7UP}
    property TextHint;
{$ENDIF}
    property Visible;

    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP

    property ItemMargin: byte read FItemMargin write FItemMargin default 4;
    property ColCount: byte read FColCount write SetColCount default 3;
    property RowCount: byte read FRowCount write SetRowCount default 5;
    property MinItemWidth: byte read FMinItemWidth write FMinItemWidth default 0;
    property ShowNoSkin: boolean read FShowNoSkin write SetShowNoSkin default True;
    property ThumbSize: TacThumbSize read FThumbSize write SetThumbSize default tsBig;
    property ReadOnly;
  end;


{$IFNDEF NOTFORHELP}
  TsPopupInfoBox = class;

  TacPopupInfoForm = class(TForm)
  protected
    ItemMargin,
    ItemWidth,
    ItemHeight: integer;

    FOwner: TsPopupInfoBox;
    sp: TsSkinProvider;
    ScrollBox: TsScrollBox;
    Lbl: TsHtmlLabel;
    LabelSize: TSize;
    UpdateFlags: set of TacUpdateFlags;

    procedure CalcLabelSize;
    procedure InitFormData;
    procedure UpdateControls;
    procedure UpdateBoxSize;
    procedure ac_CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    destructor Destroy; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPopupInfoBox = class(TsCustomComboBoxEx)
{$IFNDEF NOTFORHELP}
  private
    FAutoHide,
    FAutoShow: boolean;
    FItemMargin: byte;
    Form: TacPopupInfoForm;
    FMinPopupHeight: integer;
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
  protected
    function AllowDropDownList: boolean; override;
    function GetEditText(AIndex: integer): acString; override;
    function ItemTextDefined(AIndex: integer): boolean; override;
    function HandleKeyDown(var ACharCode: word): boolean; override;
    function AllowBtnStyle: boolean; override;
    procedure TryAutoShow;
    procedure TryAutoHide;
  public
    procedure UpdateForm;
    procedure CreateWnd; override;
    procedure PopupWindowShow; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Align;
    property ButtonSkinSection;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property AutoHide: boolean read FAutoHide write FAutoHide default False;
    property AutoShow: boolean read FAutoShow write FAutoShow default False;
    property Lines: TStrings read GetLines write SetLines;
    property ItemMargin: byte read FItemMargin write FItemMargin default 4;
    property MinPopupHeight: integer read FMinPopupHeight write FMinPopupHeight default 0;
  end;


implementation

uses
  math, {$IFDEF DELPHI_10}Vcl.Consts{$ELSE}Consts{$ENDIF}, Buttons, Menus, StdCtrls,
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  sAlphaGraph, sSkinProps, sMessages, sVCLUtils, acGlow, sSkinManager, acAnimation,
{$IFNDEF ALITE}
  sDialogs, sColorDialog,
{$ENDIF}
  CommCtrl, sStyleSimply, sThirdParty, acPopupController, sPanel, sCustomComboEdit;


const
  StandardColorsCount = 16;
  ExtendedColorsCount = 4;
  NoColorSelected = clNone;
  ThumbSizes: array [boolean, TacThumbSize] of integer = ((17, 50, 100), (24, 70, 140));


procedure TacComboExItem.Assign(Source: TPersistent);
begin
  if Source is TacComboExItem then begin
    FSelectedImageIndex := TacComboExItem(Source).SelectedImageIndex;
    FIndent := TacComboExItem(Source).Indent;
    FOverlayImageIndex := TacComboExItem(Source).OverlayImageIndex;
    FImageIndex := TacComboExItem(Source).ImageIndex;
    FCaption := TacComboExItem(Source).Caption;
    FData := TacComboExItem(Source).Data;
  end
  else
    inherited Assign(Source);
end;


procedure TacComboExItem.SetCaption(const Value: string);
begin
  inherited SetCaption(Value);
  TacComboExItems(Collection).SetItem(Index);
  if (TacComboExItems(Collection).SortType = {$IFDEF DELPHI_10}Vcl.ListActns.{$ENDIF}stText) or
  (TacComboExItems(Collection).SortType = {$IFDEF DELPHI_10}Vcl.ListActns.{$ENDIF}stBoth) then
    TacComboExItems(Collection).Sort;
end;


procedure TacComboExItem.SetData(const Value: TCustomData);
begin
  inherited SetData(Value);
  TacComboExItems(Collection).SetItem(Index);
  if (TacComboExItems(Collection).SortType = {$IFDEF DELPHI_10}Vcl.ListActns.{$ENDIF}stData) or
  (TacComboExItems(Collection).SortType = {$IFDEF DELPHI_10}Vcl.ListActns.{$ENDIF}stBoth) then
    TacComboExItems(Collection).Sort;
end;


procedure TacComboExItem.SetDisplayName(const Value: string);
begin
  inherited SetDisplayName(Value);
  TacComboExItems(Collection).SetItem(Index);
end;


procedure TacComboExItem.SetImageIndex(const Value: TImageIndex);
begin
  if (FSelectedImageIndex = -1) or (FSelectedImageIndex = ImageIndex) then
    FSelectedImageIndex := Value;
  inherited SetImageIndex(Value);
  TacComboExItems(Collection).SetItem(Index);
end;


procedure TacComboExItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  TacComboExItems(Collection).SetItem(Value);
end;


procedure TacComboExItem.SetOverlayImageIndex(const Value: TImageIndex);
begin
  FOverlayImageIndex := Value;
  TacComboExItems(Collection).SetItem(Index);
end;


procedure TacComboExItem.SetSelectedImageIndex(const Value: TImageIndex);
begin
  FSelectedImageIndex := Value;
  TacComboExItems(Collection).SetItem(Index);
end;


function TacCustomComboBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0);
end;


function TacCustomComboBoxStrings.GetObject(Index: Integer): TObject;
var
  LItemData: LPARAM;
begin
  LItemData := SendMessage(ComboBox.Handle, CB_GETITEMDATA, Index, 0);
  // Do additional checking on Count and Index here is so in the event
  // the object being retrieved is the integer -1 the call will succeed
  if (LItemData = CB_ERR) and ((Count = 0) or (Index < 0) or (Index > Count)) then
    Error('SListIndexError', Index);

  Result := TObject(LItemData);
end;


procedure TacCustomComboBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, LPARAM(AObject));
end;


function TacCustomComboBoxStrings.Get(Index: Integer): string;
var
  Len: Integer;
begin
  Len := SendMessage(ComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
  // When style = csSimple we need to also check the Len > 0
  if (Len <> CB_ERR) and (Len > 0) then begin
    SetLength(Result, Len);
    SendMessage(ComboBox.Handle, CB_GETLBTEXT, Index, LPARAM(PChar(Result)));
  end
  else
    SetLength(Result, 0);
end;


procedure TacCustomComboBoxStrings.Clear;
var
  S: string;
begin
  S := ComboBox.Text;
  SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
  ComboBox.Text := S;
  ComboBox.Update;
end;


procedure TacCustomComboBoxStrings.Delete(Index: Integer);
begin
  SendMessage(ComboBox.Handle, CB_DELETESTRING, Index, 0);
end;


function TacCustomComboBoxStrings.IndexOf(const S: string): Integer;
var
  Stop: boolean;
begin
  if S <> '' then
    Result := SendMessage(ComboBox.Handle, CB_FINDSTRINGEXACT, WPARAM(-1), LPARAM(PacChar(s)))
//    Result := SendTextMessage(ComboBox.Handle, CB_FINDSTRINGEXACT, WPARAM(-1), S)
  else begin
    // the ComboBox messages do not search for empty strings so we have
    // to do the search manually
    Stop := False;
    Result := 0;
    while (not Stop) and (Result < Count) do
      if SendMessage(ComboBox.Handle, CB_GETLBTEXTLEN, Result, 0) = 0 then
        Stop := True
      else
        Inc(Result);

    if not Stop then
      Result := -1;
  end;
end;


procedure TacCustomComboBoxStrings.SetUpdateState(Updating: boolean);
begin
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
    ComboBox.Refresh;
end;


function TacComboExItems.Add: TacComboExItem;
begin
  Result := TacComboExItem(inherited Add);
end;


function TacComboExItems.AddItem(const Caption: string; const ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent: Integer; Data: TCustomData): TacComboExItem;
begin
  Result := Add;
  Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;
  Result.SelectedImageIndex := SelectedImageIndex;
  Result.OverlayImageIndex := OverlayImageIndex;
  Result.Indent := Indent;
  Result.Data := Data;
  SetItem(Result.Index);
end;


function TacComboExItems.GetComboItem(const Index: Integer): TacComboExItem;
begin
  Result := TacComboExItem(Items[Index]);
end;


function TacComboExItems.Insert(Index: Integer): TacComboExItem;
var
  I : integer;
begin
  Result := TacComboExItem(inherited Insert(Index));
  for I := Index to Count - 1 do
    SetItem(I);
end;


{$IFDEF DELPHI7UP}
procedure TacComboExItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      with TacComboExItem(Item) do begin
        FImageIndex := -1;
        FSelectedImageIndex := -1;
        FIndent := -1;
        FOverlayImageIndex := -1;
        FData := nil;
        FCaption := '';
      end;

    cnExtracting:
      if not (csDestroying in TWinControl(Owner).ComponentState) then
        SendMessage(TWinControl(Owner).Handle, CBEM_DELETEITEM, Item.Index, 0);
  end;
end;
{$ENDIF}


procedure TacComboExItems.SetItem(const Index: Integer);
var
  AnItem: TComboBoxExItem;
  Insert: BOOL;
  Msg: Cardinal;
begin
  FillChar(AnItem, SizeOf(TComboBoxExItem), 0);
  AnItem.iItem := Index;
  AnItem.cchTextMax := 0;
//  Insert := SendGetStructMessage(TWinControl(Owner).Handle, WM_USER + 13{_CBEM_GETITEM}, 0, AnItem, True) = 0;
  Insert := SendMessage(TWinControl(GetOwner).Handle, WM_USER + 13{_CBEM_GETITEM}, 0, LPARAM(@AnItem)) = 0;

  with AnItem, ComboItems[Index] do begin
    mask := CBEIF_TEXT or CBEIF_IMAGE or CBEIF_SELECTEDIMAGE or CBEIF_INDENT or CBEIF_LPARAM;
    pszText := PChar(Caption);
    iItem := Index;
    cchTextMax := Length(Caption);
    iImage := ImageIndex;
    iSelectedImage := SelectedImageIndex;
    iOverlay := OverlayImageIndex;
    iIndent := Indent;
    lParam := Windows.LPARAM(Data);
  end;
  if Insert then
    Msg := WM_USER + 11{_CBEM_INSERTITEM}
  else
    Msg := WM_USER + 12{_CBEM_SETITEM};

  SendMessage(TWinControl(GetOwner).Handle, Msg, Index, LParam(@AnItem));
end;


function TacComboBoxExStrings.Add(const S: string): Integer;
begin
  with FItems.Add do begin
    Caption := S;
    Result := Index;
  end;
end;


function TacComboBoxExStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  with FItems.Add do begin
    Caption := S;
    Data := AObject;
    Result := Index;
  end;
end;


procedure TacComboBoxExStrings.Clear;
begin
  FItems.BeginUpdate;
  try
    FItems.Clear;
    if ComboBox.HandleAllocated then
      ComboBox.Perform(CM_RECREATEWND, 0, 0);
  finally
    FItems.EndUpdate;
  end;
end;


constructor TacComboBoxExStrings.Create(Owner: TsCustomComboboxEx);
begin
  inherited Create;
  FItems := TacComboExItems.Create(Owner, TacComboExItem);
end;


destructor TacComboBoxExStrings.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;


procedure TacComboBoxExStrings.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;


procedure TacComboBoxExStrings.Exchange(Index1, Index2: Integer);
var
  Text: string;
  Image: Integer;
begin
  Text := ItemsEx[Index2].Caption;
  ItemsEx[Index2].Caption := ItemsEx[Index1].Caption;
  ItemsEx[Index1].Caption := Text;

  Image := ItemsEx[Index2].ImageIndex;
  ItemsEx[Index2].ImageIndex := ItemsEx[Index1].ImageIndex;
  ItemsEx[Index1].ImageIndex := Image;

  Image := TacComboExItem(ItemsEx[Index2]).SelectedImageIndex;
  TacComboExItem(ItemsEx[Index2]).SelectedImageIndex := TacComboExItem(ItemsEx[Index1]).SelectedImageIndex;
  TacComboExItem(ItemsEx[Index1]).SelectedImageIndex := Image;

  Image := TacComboExItem(ItemsEx[Index2]).OverlayImageIndex;
  TacComboExItem(ItemsEx[Index2]).OverlayImageIndex := TacComboExItem(ItemsEx[Index1]).OverlayImageIndex;
  TacComboExItem(ItemsEx[Index1]).OverlayImageIndex := Image;
end;


function TacComboBoxExStrings.Get(Index: Integer): string;
begin
  Result := FItems[Index].Caption;
end;


function TacComboBoxExStrings.GetCapacity: Integer;
begin
  Result := FItems.Count;
end;


function TacComboBoxExStrings.GetCount: Integer;
begin
  Result := FItems.Count;
end;


function TacComboBoxExStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(FItems[Index].Data);
end;


function TacComboBoxExStrings.IndexOf(const S: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if AnsiCompareText(FItems[I].Caption, S) = 0 then begin
      Result := I;
      break;
    end;
end;


{$IFDEF DELPHI7UP}
function TacComboBoxExStrings.IndexOfName(const Name: string): Integer;
begin
  Result := IndexOf(Name); // Simply forward this on to IndexOf
end;
{$ENDIF}

procedure TacComboBoxExStrings.Insert(Index: Integer; const S: string);
var
  AnItem: TacComboExItem;
begin
  AnItem := FItems.Insert(Index);
  AnItem.Caption := S;
end;


procedure TacComboBoxExStrings.Move(CurIndex, NewIndex: Integer);
begin
  FItems[CurIndex].Index := NewIndex;
end;


procedure TacComboBoxExStrings.PutObject(Index: Integer; AObject: TObject);
begin
  FItems[Index].Data := AObject;
end;


procedure TacComboBoxExStrings.SetItems(const Value: TacComboExItems);
begin
  FItems.Assign(Value);
end;


procedure TacComboBoxExStrings.SetUpdateState(Updating: boolean);
begin
  if ComboBox.HandleAllocated then begin
    if IsWindowVisible(ComboBox.Handle) then
      SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
      
    if not Updating then
      ComboBox.Refresh;
  end;
end;


function TacComboBoxExStrings.AddItem(const Caption: string; const ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent: Integer; Data: TCustomData): TacComboExItem;
begin
  Result := FItems.AddItem(Caption, ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent, Data);
end;


function TacComboBoxExStrings.GetSortType: TListItemsSortType;
begin
  Result := FItems.SortType;
end;


procedure TacComboBoxExStrings.SetSortType(const Value: TListItemsSortType);
begin
  FItems.SortType := Value;
end;


(*
procedure TsCustomComboBoxEx.WndProc(var Message: TMessage);
begin
    WM_MOUSEWHEEL: if not FAllowMouseWheel then
      Exit;

    WM_SYSCHAR, WM_SYSKEYDOWN, CN_SYSCHAR, CN_SYSKEYDOWN, WM_KEYDOWN, CN_KEYDOWN:
      case TWMKey(Message).CharCode of
        VK_PRIOR..VK_DOWN, $30..$39:
          if (ReadOnly or not AllowDropDown) then
            Exit;

        $41..$5A:
          if ReadOnly or not AllowDropDown then begin
            GetKeyboardState(kbState);
            if (kbState[VK_CONTROL] and 128 = 0) and (kbState[VK_SHIFT] and 128 = 0) and (kbState[VK_MENU] and 128 = 0) then
              Exit;
          end;
      end;

    WM_CHAR:
      if ReadOnly or not AllowDropDown then
        Exit;

    WM_COMMAND, CN_COMMAND:
      if (Message.WParamHi in [CBN_DROPDOWN, CBN_SELCHANGE, CBN_EDITCHANGE]) and (FReadOnly or not AllowDropDown) then begin
        Message.Result := 1;
        Exit;
      end;

    WM_DRAWITEM: begin
      WMDrawItem(TWMDrawItem(Message));
      if Message.Result = 1 then
        Exit
    end;

    WM_CTLCOLORLISTBOX:
      if not (csLoading in ComponentState) and (PopupWidth <> 0) then begin
        GetWindowrect(hwnd(Message.LParam), R);
        if WidthOf(R) <> PopupWidth then
          SetWindowPos(hwnd(Message.LParam), 0, 0, 0, PopupWidth, HeightOf(R), SWPA_FRAMECHANGED and not SWP_NOSIZE);
      end;
  end;

      CN_COMMAND:
        case TWMCommand(Message).NotifyCode of
          CBN_CLOSEUP: begin
            FDropDown := False;
            FCommonData.BGChanged := True;
            if SkinData.AnimTimer <> nil then
              SkinData.AnimTimer.Glow := 0;

            if not acMouseInControl(Self) then begin
              State := 0;
              SkinData.FMouseAbove := False;
            end;
            RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
            if FCommonData.AnimTimer <> nil then
              FCommonData.AnimTimer.CopyFrom(FCommonData.AnimTimer.BmpOut, FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp));

            if AllowBtnStyle then
              AnimateCtrl(integer(SkinData.FMouseAbove))
          end;

          CBN_DROPDOWN: begin
            FDropDown := True;
            FCommonData.BGChanged := True;
            State := 2;
            AnimateCtrl(2);
            inherited;
            Exit;
          end;
        end;

      WM_COMMAND:
        case Message.WParamHi of
          CBN_DROPDOWN:
            if ReadOnly or not AllowDropDown then
              Exit;

          CBN_CLOSEUP: begin
            FDropDown := False;
            Repaint;
          end;
        end;

      SM_ALPHACMD:
        case Message.WParamHi of
          AC_DROPPEDDOWN:
            Message.Result := LRESULT(DroppedDown);
        end;

      CM_VISIBLECHANGED:
        if SkinData.CtrlSkinState and ACS_CHANGING = 0 then
          Repaint;

      WM_SIZE, CM_CHANGED, CM_TEXTCHANGED, CB_SETCURSEL: begin
        FCommonData.BGChanged := True;
        if ExHandle <> 0 then
          RedrawWindow(ExHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
      end;

      CM_ENABLEDCHANGED: begin
        FCommonData.BGChanged := True;
        if ExHandle <> 0 then
          RedrawWindow(ExHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
      end;
    end;
end;
*)

procedure TsCustomComboBoxEx.ActionChange(Sender: TObject; CheckDefaults: boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
{$IFDEF DELPHI7UP}
  if Sender is TStaticListAction then
    with TStaticListAction(Sender) do begin
      if not CheckDefaults or (Self.Images = nil) then
        Self.Images := Images;

      if not CheckDefaults or (Self.ItemIndex <> -1) then
        Self.ItemIndex := ItemIndex;
    end;
{$ENDIF}    
end;


function TsCustomComboBoxEx.AllowBtnStyle: boolean;
begin
  Result := IsDropDownList and (SkinData.SkinSection = '');
end;


procedure TsCustomComboBoxEx.PaintNCArea(DC: hdc);

  function ImageRect: TRect;
  var
    i, h, iPPI: integer;
  begin
    Result := TextRect;
    iPPI := GetPPI(SkinData);
    if BiDiMode <> bdLeftToRight then
      Result.Left := Result.Right - GetImageWidth(Images, -1, iPPI) - SkinData.CommonSkinData.Spacing
    else
      Result.Right := Result.Left + GetImageWidth(Images, -1, iPPI) + SkinData.CommonSkinData.Spacing;

    h := GetImageHeight(Images, -1, iPPI);
    i := (HeightOf(Result) - h) div 2;
    Result.Top := Result.Top + i;
    Result.Bottom := Result.Top + h;
  end;

begin
  inherited PaintNCArea(DC);
  if ImageExists then
    with ImageRect do
      BitBlt(DC, Left, Top, Right - Left, Bottom - Top, SkinData.FCacheBmp.Canvas.Handle, Left, Top, SRCCOPY);
end;


constructor TsCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FreeAndNil(FItems);
  FItems := TacComboBoxExStrings.Create(Self);
  TacComboBoxExStrings(FItems).ComboBox := Self;
  FItemsEx := TacComboBoxExStrings(FItems).FItems;
  FStyle := csExDropDown;
  FAutoCompleteOptions := [acoAutoAppend];
end;


destructor TsCustomComboBoxEx.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;


function TsCustomComboBoxEx.ImageExists: boolean;
begin
  Result := (Images <> nil) and ([csExNoEditImage{, csExNoEditImageIndent}] * StyleEx = []);
end;


function TsCustomComboBoxEx.IsCaseSensitive: boolean;
begin
  Result := csExCaseSensitive in StyleEx;
end;


function TsCustomComboBoxEx.IsDropDownList: boolean;
begin
  Result := Style = csExDropDownList;
end;


{$IFDEF DELPHI7UP}
function TsCustomComboBoxEx.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TacComboBoxExActionLink;
end;
{$ENDIF}


function TsCustomComboBoxEx.GetItemCount: integer;
begin
  Result := ItemsEx.Count;
end;


{$IFDEF DELPHI7UP}
function TsCustomComboBoxEx.IsItemsExStored: boolean;
begin
  Result := (Action = nil) or Assigned(Action) and not (Action is TCustomListAction);
end;
{$ENDIF}


procedure TsCustomComboBoxEx.UpdateAutoComplete;
//var
//  Auto2: IAutoComplete2;
//  ActualOptions: DWORD;
begin
//  if HandleAllocated and (FAutoCompleteIntf <> nil) then
//  begin
//    if Supports(FAutoCompleteIntf, IAutoComplete2, Auto2) then
//    begin
//      if FAutoCompleteOptions = [] then
//        Auto2.SetOptions(ACO_NONE)
//      else
//      begin
//        ActualOptions := 0;
//        if acoAutoSuggest in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_AUTOSUGGEST;
//        if acoAutoAppend in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_AUTOAPPEND;
//        if acoSearch in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_SEARCH;
//        if acoFilterPrefixes in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_FILTERPREFIXES;
//        if acoUseTab in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_USETAB;
//        if acoUpDownKeyDropsList in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_UPDOWNKEYDROPSLIST;
//        if acoRtlReading in FAutoCompleteOptions then
//          ActualOptions := ActualOptions or ACO_RTLREADING;
//
//        Auto2.SetOptions(ActualOptions);
//      end;
//    end
//    else
//      FAutoCompleteIntf.Enable(acoAutoSuggest in FAutoCompleteOptions);
//  end;
end;


procedure TsCustomComboBoxEx.UpdateStyleEx;
//const
//  ComboExStyles: array[TComboBoxExStyleEx] of DWORD = (CBES_EX_CASESENSITIVE, CBES_EX_NOEDITIMAGE,
//    CBES_EX_NOEDITIMAGEINDENT, CBES_EX_NOSIZELIMIT, CBES_EX_PATHWORDBREAKPROC);
begin
  UpdateButtonMode;
  if HandleAllocated and not (csLoading in ComponentState) then begin
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
    SkinData.Invalidate;
  end;
end;


procedure TsCustomComboBoxEx.SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
begin
  if FAutoCompleteOptions <> Value then begin
    FAutoCompleteOptions := Value;
    UpdateAutoComplete;
  end;
end;


function TsCustomComboBoxEx.IsOwnerDraw: boolean;
begin
  Result := False;
end;


function TsCustomComboBoxEx.ItemTextDefined(AIndex: integer): boolean;
begin
  if not IsDropDownList then
    Result := True
  else
    Result := inherited ItemTextDefined(AIndex);
end;


procedure TsCustomComboBoxEx.Loaded;
begin
  inherited;
  if ItemIndex >= 0 then begin
    SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_INIT;
    UpdateText;
    SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_INIT;
  end;
end;


procedure TsCustomComboBoxEx.PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer);
var
  gRect: TRect;
  bEdit: boolean;
  i, iPPI, imgNdx: integer;
begin
  bEdit := odComboBoxEdit in DrawState;
  if IsValidIndex(ItIndex, Items.Count) then begin
    // Calc indent
    if not bEdit then begin
      if ([csExNoEditImageIndent] * StyleEx = []) and (ItemsEx.ComboItems[ItIndex].Indent >= 0) then begin
        i := ItemsEx.ComboItems[ItIndex].Indent * SkinData.CommonSkinData.Spacing * 2;
        R.Left := R.Left + i;
      end;
      if R.Right - R.Left <= 0 then
        Exit;
    end;
    if ImageExists then begin
      iPPI := GetPPI(SkinData);
      if bEdit and (ItemsEx.ComboItems[ItIndex].SelectedImageIndex >= 0) then
        imgNdx := ItemsEx.ComboItems[ItIndex].SelectedImageIndex
      else
        imgNdx := ItemsEx.ComboItems[ItIndex].ImageIndex;

      if BiDiMode <> bdLeftToRight then begin
        dec(R.Right, SkinData.CommonSkinData.Spacing div 2);
        gRect := R;
        gRect.Left := gRect.Right - GetImageWidth(Images, -1, iPPI);
        R.Right := gRect.Left - SkinData.CommonSkinData.Spacing;
      end
      else begin
        inc(R.Left, SkinData.CommonSkinData.Spacing div 2);
        gRect := R;
        gRect.Right := gRect.Left + GetImageWidth(Images, -1, iPPI);
        R.Left := gRect.Right + SkinData.CommonSkinData.Spacing;
      end;
      // Draw image
      if IsValidIndex(imgNdx, GetImageCount(Images)) then begin
        i := (HeightOf(gRect) - GetImageHeight(Images, -1, iPPI)) div 2;
        SetImagesPPI(Images, iPPI);
        if State > 0 then
          SetImagesState(Images, State);

        Images.Draw(Bmp.Canvas, gRect.Left, max(0, gRect.Top + i), imgNdx, True);
        if State > 0 then
          SetImagesState(Images, 0); // Reset
      end;
    end;
  end
  else
    if (Images <> nil) and not IsDefaultValue then // If text is shifted
      if BiDiMode <> bdLeftToRight then
        R.Right := R.Right - GetImageWidth(Images, -1, GetPPI(SkinData)) - SkinData.CommonSkinData.Spacing
      else
        R.Left := R.Left + GetImageWidth(Images, -1, GetPPI(SkinData)) + SkinData.CommonSkinData.Spacing;

  inherited PaintItemContent(ItIndex, SkinIndex, Bmp, R, DrawState, State);
end;


procedure TsCustomComboBoxEx.SetItemsEx(const Value: TacComboExItems);
begin
  FItemsEx.Assign(Value);
end;


function TsCustomComboBoxEx.GetEditText(AIndex: integer): acString;
begin
  Result := Text;
end;


procedure TsCustomComboBoxEx.SetStyle(Value: TComboBoxExStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    UpdateStyleEx;
  end;
end;


procedure TsCustomComboBoxEx.SetStyleEx(const Value: TComboBoxExStyles);
begin
  FStyleEx := Value;
  UpdateStyleEx;
end;


function TsCustomComboBoxEx.GetSelectedItem: TacComboExItem;
begin
  if ItemIndex >= 0 then
    Result := ItemsEx.ComboItems[ItemIndex]
  else
    Result := nil; ///////////////////
end;


{$IFNDEF ALITE}

procedure TacPopupInfoForm.CalcLabelSize;
begin
end;


constructor TacPopupInfoForm.CreateNew(AOwner: TComponent; Dummy: Integer);
var
  Panel: TsPanel;
begin
  inherited;
  FOwner := TsPopupInfoBox(AOwner);
  ItemMargin := ScaleInt(FOwner.ItemMargin, FOwner.SkinData);

  sp := TsSkinProvider.Create(Self);
  AutoScroll := False;
  Position := poDesigned;
  BiDiMode := FOwner.BiDiMode;
  BorderStyle := bsNone;
  Scaled := False;

  Panel := TsPanel.Create(Self);
  with Panel do begin
    Align := alClient;
    BorderWidth := ItemMargin;
    BevelOuter := bvNone;
    Ctl3D := True;
    SkinData.SkinSection := s_Edit;
  end;

  ScrollBox := TsScrollBox.Create(Self);
  with ScrollBox do begin
    BorderStyle := bsNone;
    HorzScrollBar.Visible := False;
    AutoScroll := False;
    AutoMouseWheel := True;
    VertScrollBar.Tracking := True;
    SkinData.VertScrollData.ButtonsSize := 0;
    SkinData.SkinSection := s_Transparent;
    Align := alClient;
    Ctl3D := False;
    ScrollBox.ParentFont := True;
    Visible := True;
    Parent := Panel;
  end;

  Lbl := TsHtmlLabel.Create(Self);
  with Lbl do begin
    Caption := FOwner.Items.Text;
    Top := 0;
    Left := 0;
    Parent := ScrollBox;
  end;

  Panel.Parent := Self;
end;


destructor TacPopupInfoForm.Destroy;
begin
  if FOwner <> nil then
    FOwner.Form := nil;

  inherited;
end;


type
  TAccessControl = class(TWinControl);

procedure TacPopupInfoForm.InitFormData;
begin
  UpdateFlags := UpdateFlags + [ufFormInitializing];
{$IFDEF DELPHI_10BERLIN}
  if (FOwner.SkinData.SkinManager <> nil) and (FOwner.SkinData.SkinManager.Options.ScaleMode <> smVCL) then
    TAccessControl(Self).FCurrentPPI := 96; // Do not scale automatically
{$ENDIF}
  Font.Assign(FOwner.Font);
//  CalcLabelSize;

  ItemHeight := Lbl.Height;
  ItemWidth  := max(ScaleInt(FOwner.MinPopupWidth, FOwner.SkinData), Lbl.Width);

  Height := ItemMargin * 2 + ItemHeight;
  Width  := ItemMargin * 2 + ItemWidth;

  UpdateFlags := UpdateFlags - [ufUpdateScale];
  UpdateFlags := UpdateFlags - [ufFormInitializing];
end;


procedure TacPopupInfoForm.ac_CMMouseLeave(var Message: TMessage);
begin
//  if not OwnerFocused then // If was hovered by mouse
    if not acMouseInControl(FOwner) and not acMouseInControl(Self) and (GetCapture = 0) then
      Close;
end;


procedure TacPopupInfoForm.UpdateBoxSize;
begin
  if FOwner.MinPopupWidth <= 0 then
    Self.Width := max(Self.Width, FOwner.Width)
  else
    Self.Width := max(Self.Width, FOwner.MinPopupWidth);
end;


procedure TacPopupInfoForm.UpdateControls;
begin
  InitFormData;
  UpdateBoxSize;
end;



function TsPopupInfoBox.AllowDropDownList: boolean;
begin
  Result := False; // Own custom form used
end;


function TsPopupInfoBox.AllowBtnStyle: boolean;
begin
  Result := True;
end;


constructor TsPopupInfoBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoHide := False;
  FAutoShow := False;
  FItemMargin := 4;
end;


procedure TsPopupInfoBox.TryAutoShow;
begin
  if not (csDesigning in ComponentState) and AutoShow and not DroppedDown and Application.Active then
    AdjustDropDown;
end;


procedure TsPopupInfoBox.TryAutoHide;
begin
  if not (csDesigning in ComponentState) and AutoHide and (Form <> nil) and DroppedDown and not acMouseInControl(Self) and not acMouseInControl(Form) then
    Form.Close;
end;


procedure TsPopupInfoBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or CBS_DROPDOWNLIST;
end;


procedure TsPopupInfoBox.CreateWnd;
begin
  inherited Style := csExDropDownList;
  inherited CreateWnd;
end;


function TsPopupInfoBox.GetEditText(AIndex: integer): acString;
begin
  Result := Text;
end;


function TsPopupInfoBox.GetLines: TStrings;
begin
  Result := FItems;
end;


function TsPopupInfoBox.HandleKeyDown(var ACharCode: word): boolean;
begin
  case ACharCode of
    VK_RETURN, VK_ESCAPE:
      if (Form <> nil) and FDroppedDown then begin
        Result := True;
        Form.Close;
      end
      else
        Result := False;

    VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_HOME, VK_END, VK_LEFT, VK_RIGHT: begin
      ACharCode := 0;
      Result := True;
    end

    else
      Result := False;
  end;
  if not Result then
    Result := inherited HandleKeyDown(ACharCode);
end;


function TsPopupInfoBox.ItemTextDefined(AIndex: integer): boolean;
begin
  Result := True;
end;


procedure TsPopupInfoBox.PopupWindowShow;
begin
  if not FDroppedDown then begin
    if (SkinData.SkinManager <> nil) and
         ((Form = nil) or (Form.sp = nil) or ((Form.sp.FormState and FS_ANIMCLOSING = 0) or (acHideTimer = nil) or not acHideTimer.Enabled)) then begin

      FDroppedDown := True;
      BtnState := ACPRESSED;
      StopTimer(SkinData);
      SkinData.FDoLighting := False;
      FreeAndNil(SkinData.LightMask);
      FreeAndNil(SkinData.HotCache);
      SkinData.Invalidate(True);
      if Form = nil then begin
        Form := TacPopupInfoForm.CreateNew(Self);
        Form.sp.AllowScale := False;
        Form.sp.AllowAnimation := True;
        Form.Name := Name + 'Form';
        Form.UpdateFlags := Form.UpdateFlags + [ufFormCreating];
      end;
      Form.UpdateFlags := Form.UpdateFlags + [ufUpdateScale];
      UpdateForm;
      Form.UpdateFlags := Form.UpdateFlags - [ufFormCreating];
      ShowPopupForm(Form, Self, -1, -1, True, PopupAnimProc);
    end;
  end
  else
    if (Form <> nil) and IsWindowVisible(Form.Handle) then begin
      FDroppedDown := False;
      StopTimer(SkinData);
      SkinData.Invalidate(True);
      Form.Close;
    end;
end;


procedure TsPopupInfoBox.SetLines(const Value: TStrings);
begin
  FItems.Assign(Value);
end;


procedure TsPopupInfoBox.UpdateForm;
begin
  if Form <> nil then begin
    Form.UpdateControls;
    Form.UpdateBoxSize;
  end;
end;


procedure TsPopupInfoBox.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN: begin
          inherited;
          if Form <> nil then
            AlphaBroadCast(Form, Message);

          Exit;
        end;

        AC_SETSCALE: begin
          FreeAndNil(Form);
          SkinData.CommonSkinData := nil;
        end;
      end;

    CN_COMMAND:
      case TWMCommand(Message).NotifyCode of
        CBN_DROPDOWN:
          DoDropDown;
      end;

    CB_SHOWDROPDOWN:
      if Message.WParam <> 0 then begin
        DoDropDown;
        Exit;
      end;
  end;
  inherited;
  case Message.Msg of
    CM_MOUSEENTER:
      TryAutoShow;

    CM_MOUSELEAVE:
      TryAutoHide;
  end;
end;


constructor TsCustomColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := [cbStandardColors, cbExtendedColors, cbSystemColors];
  FSelectedColor := clBlack;
  FDefaultColorColor := clBlack;
  FPopupMode := pmColorList;
  FShowColorName := True;
  FNoneColorColor := clBlack;
  FColorRectHeight := 14;
  FColorRectWidth := 21;
  FUseAlpha := False;
  FInPopulating := False;
  SkinData.COC := COC_TsColorBox;
  PopulateList;
end;


procedure TsCustomColorBox.CreateWnd;
begin
  if not (csDesigning in ComponentState) then
    inherited Style := csExDropDownList;

  inherited CreateWnd;
  if FNeedToPopulate then
    PopulateList;
end;


function TsCustomColorBox.GetColor(Index: Integer): TColor;
begin
  if (Index < 0) or (Index >= Items.Count) then
    Result := NoColorSelected
  else
    Result := TColor(Items.Objects[Index]);
end;


function TsCustomColorBox.GetSelected: TColor;
begin
  if HandleAllocated then
    if ItemIndex >= 0 then
      Result := GetColor(ItemIndex)
    else
      Result := NoColorSelected
  else
    Result := FSelectedColor;
end;


function TsCustomColorBox.HandleKeyDown(var ACharCode: word): boolean;
begin
  if (PopupMode = pmPickColor) and (ACharCode in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_HOME, VK_END]) then begin
    ACharCode := 0;
    Result := False;
  end
  else
    Result := inherited HandleKeyDown(ACharCode)
end;


function TsCustomColorBox.GetEditText(AIndex: integer): acString;
begin
  if IsValidIndex(AIndex, ItemsEx.Count) then
    Result := ItemsEx[AIndex].Caption
  else
    Result := Text;
end;


procedure TsCustomColorBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN]) then
    Key := 0;

  inherited KeyDown(Key, Shift);
end;


procedure TsCustomColorBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (cbCustomColor in Style) and (Key = #13) and (ItemIndex = 0) then begin
    PickCustomColor;
    Key := #0;
  end;
end;


procedure TsCustomColorBox.PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer);
var
  C: TColor;
  cR: TRect;
  i, imgW, imgH: integer;
  bEdit, Skinned: boolean;

  function ColorToBorderColor(AColor: TColor): TColor;
  begin
    if (TsColor(AColor).R > 128) or (TsColor(AColor).G > 128) or (TsColor(AColor).B > 128) then
      Result := clGray
    else
      if odSelected in DrawState then
        Result := clWhite
      else
        Result := AColor;
  end;

  procedure PaintColorRect(gRect: TRect; AColor: TColor; aFocus: boolean);
  var
    cRect: TRect;
  begin
    if IsRightToLeft then
      dec(gRect.Right, 2);

    cRect := gRect;
    InflateRect(cRect, -1, -1);
    if not UseAlpha then
      if AColor = clNone then
        PaintTransBG(Bmp, cRect, clWhite, clSilver, SkinData.CommonSkinData.ArrowSize)
      else
        FillDC(Bmp.Canvas.Handle, cRect, acColorToRGB(aColor))
    else begin
      PaintTransBG(Bmp, cRect, clWhite, clSilver, SkinData.CommonSkinData.ArrowSize);
      BlendColorRect(Bmp, cRect, MaxByte - TsColor(aColor).A, Cardinal(aColor) and clWhite);
    end;

    if Skinned then
      FillDCBorder(Bmp.Canvas.Handle, gRect, 1, 1, 1, 1, SkinData.SkinManager.Palette[pcBorder])
    else
      FillDCBorder(Bmp.Canvas.Handle, gRect, 1, 1, 1, 1, clBlack);

    if aFocus then
      DrawFocusRect(Bmp.Canvas.Handle, gRect);

    if bEdit then // May be semitransparent
      FillAlphaRect(Bmp, gRect, MaxByte);
  end;

begin
  if IsValidIndex(ItIndex, Items.Count) then begin
    Skinned := SkinData.Skinned;
    bEdit := odComboBoxEdit in DrawState;
    C := GetColor(ItIndex);
    if C = clDefault then
      C := DefaultColorColor
    else
      if C = clNone then
        C := NoneColorColor;

    if ShowColorName or not bEdit then begin
      imgW := ScaleInt(FColorRectWidth, SkinData);
      imgH := ScaleInt(FColorRectHeight, SkinData);
      i := (HeightOf(R) - imgH) div 2;
      cR.Top := R.Top + i;
      cR.Bottom := cR.Top + imgH;
      if IsRightToLeft then begin
        cR.Right := R.Right - max(0, i);
        cR.Left := cR.Right - imgW;
        R.Right := cR.Left - SkinData.CommonSkinData.Spacing;
      end
      else begin
        cR.Left := R.Left + max(0, i);
        cR.Right := cR.Left + imgW;
        R.Left := cR.Right + SkinData.CommonSkinData.Spacing;
      end;

      PaintColorRect(cR, C, False);
      inherited;
    end
    else
      PaintColorRect(R, C, [odSelected, odFocused] * DrawState = [odSelected, odFocused]);
  end;
end;


function TsCustomColorBox.PickCustomColor: boolean;
begin
  if ColDlg = nil then begin
    ColDlg := TsColorDialog.Create(nil);
    TsColorDialog(ColDlg).OnColorPreview := OnColorPreview;
    TsColorDialog(ColDlg).PreviewCmp := Self;
  end;

  TsColorDialog(ColDlg).UseAlpha := UseALpha;
  if UseAlpha then
    ColDlg.Color := TColor(Items.Objects[0])
  else
    ColDlg.Color := acColorToRGB(TColor(Items.Objects[0]));

  Result := ColDlg.Execute;
  if Result then begin
    if cbSavedColors in Style then
      PopulateList;

    TacComboBoxExStrings(Items).ItemsEx[0].Data := TObject(TsColorDialog(ColDlg).Color);
    Selected := ColDlg.Color;
    ItemIndex := 0;
    Invalidate;
  end;
end;


function TsCustomColorBox.UpdateAlpha(aColor: TColor): TColor;
begin
  if not UseAlpha then
    Result := aColor
  else
    Result := TColor(Cardinal(aColor) or $FF000000);
end;


type
  TacColorsHolder = class(TObject)
  protected
    acColors: TStringList;
    acPrettyNames: TStringList;
    procedure acColorCallBack(const AName: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  ColorsHolder: TacColorsHolder = nil;


constructor TacColorsHolder.Create;
begin
  acColors := TStringList.Create;
  acPrettyNames := TStringList.Create;
end;


destructor TacColorsHolder.Destroy;
begin
  inherited;
  acColors.Free;
  acPrettyNames.Free;
end;


procedure TacColorsHolder.acColorCallBack(const AName: string);
var
  I, LStart: Integer;
  LColor: TColor;
  LName: string;
begin
  LColor := StringToColor(AName);
  LStart := iff(Copy(AName, 1, 2) = 'cl', 3, 1);
  LName := '';
  for I := LStart to Length(AName) do begin
    case AName[I] of
      'A'..'Z': if (LName <> '') and (LName <> '3') then
        LName := LName + s_Space;
    end;
    LName := LName + AName[I];
  end;
  acColors.AddObject(AName, TObject(LColor));
  acPrettyNames.AddObject(LName, TObject(LColor));
end;


procedure PrepareColors;
begin
  if ColorsHolder = nil then begin
    ColorsHolder := TacColorsHolder.Create;
    GetColorValues(ColorsHolder.acColorCallBack);
  end;
end;


procedure TsCustomColorBox.PopulateList;

  procedure DeleteRange(const AMin, AMax: Integer);
  var
    I: Integer;
  begin
    for I := AMax downto AMin do
      Items.Delete(I);
  end;

  procedure DeleteColor(const AColor: TColor);
  var
    I: Integer;
  begin
    I := Items.IndexOfObject(TObject(UpdateAlpha(AColor)));
    if I >= 0 then
      Items.Delete(I);
  end;

var
  aColor, lSelectedColor, lCustomColor, C: TColor;
  i: integer;
  s: string;
begin
  if HandleAllocated and not (csDestroying in ComponentState) then begin
    FInPopulating := True;
    Items.BeginUpdate;
    try
      if (cbCustomColor in Style) and (Items.Count > 0) then
        LCustomColor := TColor(Items.Objects[0])
      else
        lCustomColor := UpdateAlpha(clBlack);

      LSelectedColor := FSelectedColor;
      while Items.Count > 0 do
        Items.Delete(0);

      PrepareColors;
      if cbPrettyNames in Style then
        for i := 0 to ColorsHolder.acPrettyNames.Count - 1 do begin
          aColor := TColor(ColorsHolder.acPrettyNames.Objects[i]);
          s := ColorsHolder.acPrettyNames[i];
          if Assigned(FOnColorName) then
            FOnColorName(Self, aColor, s);

          Items.AddObject(ColorsHolder.acPrettyNames[i], TObject(UpdateAlpha(AColor)));
        end
      else
        for i := 0 to ColorsHolder.acColors.Count - 1 do begin
          aColor := TColor(ColorsHolder.acColors.Objects[i]);
          s := ColorsHolder.acColors[i];
          if Assigned(FOnColorName) then
            FOnColorName(Self, aColor, s);

          Items.AddObject(ColorsHolder.acColors[i], TObject(UpdateAlpha(AColor)));
        end;

      if not (cbIncludeNone in Style) then
        DeleteColor(clNone);

      if not (cbIncludeDefault in Style) then
        DeleteColor(clDefault);

      if not (cbSystemColors in Style) then begin
        DeleteRange(StandardColorsCount + ExtendedColorsCount, Items.Count - 1);
        if cbIncludeNone in Style then
          Items.AddObject('clNone', TObject(clNone));
      end;

      if not (cbExtendedColors in Style) then
        DeleteRange(StandardColorsCount, StandardColorsCount + ExtendedColorsCount - 1);

      if not (cbStandardColors in Style) then
        DeleteRange(0, StandardColorsCount - 1);

      if (cbSkinColors in Style) then
        for i := 0 to Length(acSkinColorNames) - 1 do
          Items.AddObject(acSkinColorNames[i].Name, TObject(acSkinColorNames[i].Value));

      if (cbSavedColors in Style) and (ColDlg <> nil) then
        for i := 0 to acCustomColors.Count - 1 do
          if acCustomColors[i] <> 'FFFFFF' then begin
            Items.Insert(0, acCustomColors[i]);
            C := HexToInt(acCustomColors[i]);
            C := SwapRedBlue(C);
            TacComboBoxExStrings(Items).ItemsEx[0].Data := TObject(C);
          end;

      if cbCustomColor in Style then begin
        Items.Insert(0, {$IFDEF DELPHI7UP}SColorBoxCustomCaption{$ELSE}'Custom...'{$ENDIF});
        TacComboBoxExStrings(Items).ItemsEx[0].Data := TObject(UpdateAlpha(LCustomColor));
      end;
      if (cbCustomColors in Style) and Assigned(OnGetColors) then begin
        OnGetColors(Self, Items);
        for i := 0 to Items.Count - 1 do
          TacComboBoxExStrings(Items).ItemsEx[i].Data := Items.Objects[i];
      end;
      SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_INIT;
      Selected := LSelectedColor;
      SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_INIT;
    finally
      Items.EndUpdate;
    end;
    FNeedToPopulate := False;
    FInPopulating := False;
  end
  else
    FNeedToPopulate := True;
end;


procedure TsCustomColorBox.PopupWindowShow;
begin
  if AllowDropDownList then
    inherited
  else
    if not FDroppedDown then begin
      FDroppedDown := True;
      if sColorDialogForm <> nil then
        FreeAndNil(sColorDialogForm);

      sColorDialogForm := TsColorDialogForm.Create(Application);

      sColorDialogForm.AddPal.UseAlpha := FUseAlpha;
      sColorDialogForm.MainPal.UseAlpha := FUseAlpha;
      sColorDialogForm.OnColorPreview := OnColorPreview;
      sColorDialogForm.PreviewCmp := Self;

      sColorDialogForm.UseAlpha := UseAlpha;
      sColorDialogForm.InitControls(True, False, Selected, bsNone);
      sColorDialogForm.SetCurrentColor(Selected, False, True);

      StopTimer(SkinData);
      SkinData.FDoLighting := False;
      FreeAndNil(SkinData.LightMask);
      FreeAndNil(SkinData.HotCache);
      if CanFocus then
        SetFocus;

      SkinData.Invalidate(True);
      ShowPopupForm(sColorDialogForm, Self, -1, -1, True, PopupAnimProc);
      // Allow showing of Dlg when screen is captured
      SetWindowLong(sColorDialogForm.Handle, GWL_EXSTYLE, GetWindowLong(sColorDialogForm.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
    end
    else
      if (sColorDialogForm <> nil) and IsWindowVisible(sColorDialogForm.Handle) then begin
        FDroppedDown := False;
        StopTimer(SkinData);
        SkinData.Invalidate(True);
        sColorDialogForm.Close;
      end;
end;


procedure TsCustomColorBox.Select;
begin
  if not (cbCustomColor in Style) or (ItemIndex <> 0) or PickCustomColor then
    inherited Select;
end;


procedure TsCustomColorBox.SetDefaultColorColor(const Value: TColor);
begin
  if Value <> FDefaultColorColor then begin
    FDefaultColorColor := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomColorBox.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      SkinData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FMargin, min(Value, Height div 2));
    1: ChangeProp(FColorRectHeight, Value);
    2: ChangeProp(FColorRectWidth, Value);
  end;
end;


procedure TsCustomColorBox.SetNoneColorColor(const Value: TColor);
begin
  if Value <> FNoneColorColor then begin
    FNoneColorColor := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomColorBox.SetPopupMode(const Value: TacColorPopupMode);
begin
  if FPopupMode <> Value then begin
    FPopupMode := Value;
    if Value = pmPickColor then
      Style := Style + [cbCustomColor]
    else
      PopulateList;
  end;
end;


procedure TsCustomColorBox.SetSelected(const AColor: TColor);
var
  I, J, NewItemIndex: Integer;
begin
  if HandleAllocated then begin
    I := Items.IndexOfObject(TObject(AColor));
    if {AColor} I <> 0 then begin
      if (cbCustomColor in Style) or (PopupMode = pmPickColor) then
        TacComboBoxExStrings(Items).ItemsEx[0].Data := TObject(AColor);

      if I = -1 then
        NewItemIndex := 0
      else
        NewItemIndex := I;
    end
    else
      if cbCustomColor in Style then begin
        if (I = -1) and (AColor <> NoColorSelected) or UseAlpha then begin
          TacComboBoxExStrings(Items).ItemsEx[0].Data := TObject(AColor);
          I := 0;
        end;
        if (I = 0) and (Items.Objects[0] = TObject(AColor)) then begin
          NewItemIndex := 0;
          for J := 1 to Items.Count - 1 do
            if Items.Objects[J] = TObject(AColor) then begin
              NewItemIndex := J;
              Break;
            end;
        end
        else
          NewItemIndex := I;
      end
      else
        NewItemIndex := I;

    ItemIndex := NewItemIndex;
    SendAMessage(Handle, AC_INVALIDATE);
  end;
  FSelectedColor := AColor;
end;


procedure TsCustomColorBox.SetShowColorName(const Value: boolean);
begin
  if FShowColorName <> Value then begin
    FShowColorName := Value;
    SkinData.Invalidate;
    if not (csLoading in ComponentState) and not SkinData.Skinned then
      RedrawWindow(Handle, nil, 0, RDWA_ALLCHILDREN);
  end;
end;


procedure TsCustomColorBox.SetStyle(AStyle: TsColorBoxStyle);
begin
  if AStyle <> Style then begin
    FStyle := AStyle;
    PopulateList;

    if csDesigning in componentState then
      recreateWnd
    else
      SkinData.Invalidate;
  end;
end;


function TsCustomColorBox.AllowDropDownList: boolean;
begin
  Result := PopupMode <> pmPickColor;
end;

(*
function TsCustomColorBox.CanDoChange: boolean;
begin
  if FInPopulating {or FNeedToPopulate} then
    Result := False
  else
    Result := {not AllowDropDownList or }inherited CanDoChange;
end;
*)

procedure TsCustomColorBox.WndProc(var Message: TMessage);
var
  SavedColor: TColor;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETSCALE:
          if sColorDialogForm <> nil then
            FreeAndNil(sColorDialogForm);
      end;

    CN_COMMAND:
      case TWMCommand(Message).NotifyCode of
        CBN_DROPDOWN:
          if cbSavedColors in Style then begin
            SavedColor := Selected;
            PopulateList;
            Selected := SavedColor;
          end;
      end;

    CN_SYSKEYDOWN:
      if PopupMode = pmPickColor then
        case TWMKey(Message).CharCode of
          VK_DOWN:
            if not ReadOnly and (ssAlt in KeyDataToShiftState(TWMKey(Message).KeyData)) then begin
              AdjustDropDown;
              TWMKey(Message).Result := 1;
              Exit;
            end;
        end;
  end;
  inherited;
end;
{$ENDIF} // ALITE


procedure TacSkinSelectBtn.Click;
begin
  FDoClick := True;
end;


constructor TacSkinSelectBtn.Create(AOwner: TComponent);
begin
  inherited;
  FDoClick := False;
end;


function TacSkinSelectBtn.PrepareCache: boolean;
var
  R: TRect;
begin
  R := Parent.ClientRect;
  if RectInRect(R, BoundsRect, False) then
    Result := inherited PrepareCache
  else
    Result := False;
end;


procedure TacSkinSelectBtn.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_LBUTTONUP:
      if FDoClick then begin
        FDoClick := False;
        TacSkinSelectForm(Parent.Parent.Parent).ClickBtn(Self);
      end;
  end;
end;


constructor TacSkinSelectForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
var
  Panel: TsPanel;
begin
  inherited;
  sp := TsSkinProvider.Create(Self);
  AutoScroll := False;
  Position := poDesigned;
  UpdateFlags := UpdateFlags + [ufUpdateThumbs];
  Selector := TsSkinSelector(AOwner);
  BiDiMode := Selector.BiDiMode;
  BorderStyle := bsNone;
  Scaled := False;
  ImgList := TsVirtualImageList.Create(Self);
  ImgList.Name := 'SkinPreviews';

  Panel := TsPanel.Create(Self);
  Panel.Align := alClient;
  Panel.BorderWidth := Panel.SkinData.CommonSkinData.Spacing;
  Panel.BevelOuter := bvNone;
  Panel.Ctl3D := True;
  Panel.SkinData.SkinSection := s_Edit;
  ScrollBox := TsScrollBox.Create(Self);
  with ScrollBox do begin
    BorderStyle := bsNone;
    HorzScrollBar.Visible := False;
    AutoScroll := True;
    AutoMouseWheel := True;
    VertScrollBar.Tracking := True;
    SkinData.VertScrollData.ButtonsSize := 0;
    SkinData.SkinSection := s_Transparent;
    Align := alClient;
    Ctl3D := False;
    ScrollBox.ParentFont := True;
    Visible := True;
    Parent := Panel;
  end;
  Panel.Parent := Self;
end;


destructor TacSkinSelectForm.Destroy;
begin
  if Selector <> nil then
    Selector.Form := nil;

  inherited;
end;


procedure TacSkinSelectForm.InitFormData;
var
  iActualRowCount, iActualColCount: integer;
begin
  UpdateFlags := UpdateFlags + [ufFormInitializing];
{$IFDEF DELPHI_10BERLIN}
  if (Selector.SkinData.SkinManager <> nil) and (Selector.SkinData.SkinManager.Options.ScaleMode <> smVCL) then
    TAccessControl(Self).FCurrentPPI := 96; // Do not scale automatically
{$ENDIF}
  Font.Assign(Selector.Font);
  ItemWidth  := Selector.ItemMargin * 2 + max(Selector.MinItemWidth, ThumbSizes[True, Selector.ThumbSize]);
  ItemHeight := Selector.ItemMargin * 2 + ThumbSizes[False, Selector.ThumbSize];

  if Selector.ThumbSize <> tsSmall then
    inc(ItemHeight, GetFontHeight(Selector.Font.Handle) + sp.SkinData.CommonSkinData.Spacing);

  if ufUpdateScale in UpdateFlags then begin
    ItemWidth  := ScaleInt(ItemWidth,  Selector.SkinData);
    ItemHeight := ScaleInt(ItemHeight, Selector.SkinData);
  end;
  ImgList.StdHandleUsed := False;
  if ufUpdateThumbs in UpdateFlags then begin
    ImgList.AcBeginUpdate;
    ImgList.AlphaImageList := nil;
    Selector.SkinData.SkinManager.SkinListController.UpdateIfNeeded;
    ImgList.AlphaImageList := TsAlphaImageList(Selector.SkinData.SkinManager.SkinListController.ImgList);
    UpdateFlags := UpdateFlags - [ufUpdateThumbs];
    Selector.PopulateList(True);
    Selector.UpdateItemIndex;

    ImgList.Width  := ThumbSizes[True,  Selector.ThumbSize];
    ImgList.Height := ThumbSizes[False, Selector.ThumbSize];
    ImgList.AcEndUpdate(False);
  end;
  ItemCount := Selector.GetItemCount;
  if Selector.ShowNoSkin then
    inc(ItemCount);

  iActualRowCount := min(Selector.RowCount, ItemCount div Selector.ColCount + integer((ItemCount mod Selector.ColCount) <> 0));
  Height := sp.SkinData.CommonSkinData.Spacing * 2 + iActualRowCount * ItemHeight;
  iActualColCount := min(Selector.ColCount, ItemCount);
  Width  := sp.SkinData.CommonSkinData.Spacing * 2 + iActualColCount * ItemWidth;

  if ufUpdateScale in UpdateFlags then
    SetImagesPPI(ImgList, GetPPI(Selector.SkinData));

  UpdateFlags := UpdateFlags - [ufUpdateScale];
  ImgList.Loaded;
  UpdateFlags := UpdateFlags - [ufFormInitializing];
  ScrollBox.VertScrollBar.Increment := ItemHeight;
end;


type
  TAccessScrollBox = class(TsScrollBox);

procedure TacSkinSelectForm.UpdateBoxSize;
var
  sbWidth: integer;
begin
  if Selector.ThumbSize = tsSmall then begin
    AutoScroll := True;
    if Selector.MinItemWidth <= 0 then
      Self.Width := Selector.Width
    else
      Self.Width := max(Self.Width, Selector.MinItemWidth);
  end
  else
    with sp.SkinData.CommonSkinData do begin
      Width := min(Selector.ColCount, ItemCount) * ItemWidth + Spacing * 2;
      with TAccessScrollBox(ScrollBox) do
        if ItemCount > Selector.RowCount * Selector.ColCount then begin
          AutoScroll := True;
          if (ListSW = nil) and Selector.SkinData.Skinned(True) then begin
            HandleNeeded;
            RefreshScrolls(SkinData, ListSW);
          end;
          if ListSW <> nil then
            sbWidth := GetScrollMetric(ListSW.sBarVert, SM_SCROLLWIDTH) + Spacing
          else
            sbWidth := GetSystemMetrics(SM_CXVSCROLL) + Spacing;

          Self.Width := Self.Width + sbWidth;
        end
        else
          AutoScroll := False;

        if Selector.MinItemWidth <= 0 then
          Self.Width := max(Self.Width, Selector.Width)
        else
          Self.Width := max(Self.Width, Selector.MinItemWidth)
    end;
end;


procedure TacSkinSelectForm.UpdateControls;
var
  X, Y, i, wScroll, w, BtnWidth: integer;
  sb: TAccessScrollBox;

  function AddBtn(const ACaption: string; ImgIndex: integer): TacSkinSelectBtn;
  begin
    Result := TacSkinSelectBtn.Create(ScrollBox);
    Result.AnimatEvents := [];
    Result.Width := BtnWidth;
    Result.Height := ItemHeight;
    if BiDiMode = bdRightToLeft then
      Result.Left := w - (X + 1) * ItemWidth
    else
      Result.Left := X * ItemWidth;

    Result.Top := Y * ItemHeight;
    Result.Parent := ScrollBox;
    Result.Font.Assign(Font);
    Result.Visible := True;
    Result.Caption := ACaption;
    Result.ImageIndex := ImgIndex;
    Result.SkinData.SkinSection := {$IFDEF NOTSKINNEDCONTENT}s_NA{$ELSE}s_MenuItem{$ENDIF};
    Result.Images := ImgList;
    if Selector.ThumbSize = tsSmall then begin
      Result.Margin := Selector.ItemMargin;
      Result.TextAlignment := taLeftJustify;
      Result.Layout := blGlyphLeft;
      Result.Alignment := taLeftJustify;
    end
    else
      Result.Layout := blGlyphTop;

    Result.WordWrap := False;
    Result.Flat := True;
    Result.Tag := i;
    if Selector.SkinData.SkinManager.Active and (Selector.SkinData.SkinManager.SkinName = Result.Caption) then begin
      BtnIndex := i + integer(Selector.ShowNoSkin);
      Result.FHotState := True;
    end
    else
      Result.FHotState := False;

    inc(X);
    if X >= Selector.ColCount then begin
      X := 0;
      inc(Y);
    end;
  end;

begin
  InitFormData;
  UpdateBoxSize;

  sb := TAccessScrollBox(Scrollbox);
  sb.DisableAlign;

  X := 0;
  Y := 0;
  SendScrollMessage(ScrollBox.Handle, WM_VSCROLL, SB_THUMBPOSITION, 0);
  with sp.SkinData.CommonSkinData do begin
    if Selector.GetItemCount > Selector.RowCount then
      if ScrollBox.ListSW <> nil then
        wScroll := GetScrollMetric(ScrollBox.ListSW.sBarVert, SM_SCROLLWIDTH) + Spacing
      else
        wScroll := GetSystemMetrics(SM_CXVSCROLL) + Spacing
    else
      wScroll := 0;

    for i := ScrollBox.ControlCount - 1 downto 0 do
      ScrollBox.Controls[i].Free;

    i := -1;
    BtnIndex := -1;

    if Selector.ThumbSize = tsSmall then begin
      BtnWidth := Width - 2 * Spacing;
      if wScroll <> 0 then
        BtnWidth := BtnWidth - wScroll;
    end
    else
      BtnWidth := ItemWidth;
  end;
  w := Selector.ColCount * BtnWidth;
  if Selector.ShowNoSkin then
    with AddBtn('Standard theme', -1) do
      if not Selector.SkinData.SkinManager.IsActive then
        Enabled := False;

  for i := 0 to Selector.GetItemCount - 1 do
    AddBtn(Selector.ItemsEx[i].Caption, Selector.ItemsEx[i].ImageIndex);

  ScrollBox.EnableAlign;
end;


procedure TacSkinSelectForm.UpdateHotControl;
var
  i: integer;
  b: boolean;
begin
  for i := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[i] is TacSkinSelectBtn then
      with TacSkinSelectBtn(ScrollBox.Controls[i]) do
        if Selector.SkinData.SkinManager.Active then begin
          b := TacSkinSelectBtn(ScrollBox.Controls[i]).Caption = Selector.SkinData.SkinManager.SkinName;
          if FHotState <> b then begin
            FHotState := b;
            SkinData.Invalidate(True);
          end;
          if b then
            BtnIndex := i;
        end
        else
          FHotState := False;

  if Selector.ShowNoSkin and (ScrollBox.ControlCount > 0) then begin
    TacSkinSelectBtn(ScrollBox.Controls[0]).Enabled := Selector.SkinData.SkinManager.Active;
    if not Selector.SkinData.SkinManager.Active then
      TacSkinSelectBtn(ScrollBox.Controls[0]).FHotState := True;
  end;

  if BtnIndex >= 0 then
    TsScrollBox(ScrollBox).ScrollInView(ScrollBox.Controls[BtnIndex]);
end;


procedure TacSkinSelectForm.ClickBtn(AButton: TacSkinSelectBtn);
var
  sName: string;
begin
  if Selector.ShowNoSkin and (AButton.Tag = -1) then
    sName := ''
  else
    sName := AButton.Caption;

  Close;
  if Selector.Items.Count > 0 then
    Selector.ItemIndex := Selector.Items.IndexOf(sName)
  else // If list is not generated yet
    if Selector.SkinData.SkinManager <> nil then begin
      Selector.SkinData.SkinManager.SkinName := sName;
      Selector.SkinData.SkinManager.Active := True;
    end;

  BtnIndex := Selector.ItemIndex;
  if Selector.CanFocus then
    Selector.SetFocus;
end;


procedure TacSkinSelectForm.SetBtnIndex(NewIndex: integer);
var
  bIndex: integer;
  M: TMessage;
begin
  if IsValidIndex(BtnIndex, ScrollBox.ControlCount) then
    bIndex := BtnIndex
  else
    bIndex := 0;

  if IsValidIndex(bIndex, ScrollBox.ControlCount) and (bIndex <> NewIndex) and IsValidIndex(NewIndex, ScrollBox.ControlCount) then begin
    with TacSkinSelectBtn(ScrollBox.Controls[bIndex]) do begin
      FHotState := False;
      SkinData.Invalidate(True);
    end;
    BtnIndex := NewIndex;
    with TacSkinSelectBtn(ScrollBox.Controls[BtnIndex]) do begin
      FHotState := True;
      SkinData.Invalidate(True);
    end;
    if (TsScrollBox(ScrollBox).ListSW <> nil) and ScrollBox.ListSW.sBarVert.fScrollVisible then begin
      TsScrollBox(ScrollBox).ScrollInView(ScrollBox.Controls[BtnIndex]);
      M := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
      AlphaBroadCast(ScrollBox, M);
      RedrawWindow(ScrollBox.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN or RDW_FRAME);
    end;
  end;
end;


function TsSkinSelector.AllowDropDownList: boolean;
begin
  Result := False; // Own custom form used
end;


constructor TsSkinSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowNoSkin := True;
  FThumbSize := tsBig;
  FDontChangeManager := False;
  FColCount := 3;
  FRowCount := 5;
  FItemMargin := 4;
  FMinItemWidth := 0;
end;


procedure TsSkinSelector.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or CBS_DROPDOWNLIST;
end;


procedure TsSkinSelector.CreateWnd;
begin
//  if not (csDesigning in ComponentState) then
  inherited Style := csExDropDownList;
  inherited CreateWnd;
  if SkinData.SkinManager <> nil then begin
    SkinData.SkinManager.SkinListController.AddControl(Self);
    SkinData.SkinManager.SkinListController.UpdateIfNeeded;
  end;
  UpdateItemIndex;
end;


destructor TsSkinSelector.Destroy;
begin
  if SkinData.SkinManager <> nil then
    SkinData.SkinManager.SkinListController.DelControl(Self);

  inherited;
end;


procedure TsSkinSelector.KeyDown(var Key: Word; Shift: TShiftState);
//var
//  sc: TShortCut;
begin
  inherited KeyDown(Key, Shift);
{
//Exit;
  sc := ShortCut(Key, Shift);
  if sc = ClickKey then begin
    ReleaseCapture;
    DoDropDown;
    Key := 0;
  end
  else
    case Key of
      VK_ESCAPE:
        if Form <> nil then begin
          Key := 0;
          Form.Close;
        end;

      VK_HOME:
        if ReadOnly then
          MessageBeep(0)
        else
          SetIndex(0, 0);

      VK_END:
        if ReadOnly then
          MessageBeep(0)
        else
          SetIndex(Items.Count + Integer(ShowNoSkin) - 1, 0);

      VK_LEFT:
        if ReadOnly then
          MessageBeep(0)
        else
          SetIndex(0, -1);

      VK_RIGHT:
        if ReadOnly then
          MessageBeep(0)
        else
          SetIndex(0, 1);

      VK_UP:
        if ReadOnly then
          MessageBeep(0)
        else
          if (Form <> nil) and FDroppedDown then
            SetIndex(0, -Self.ColCount)
          else
            SetIndex(0, -1);

      VK_DOWN:
        if ReadOnly then
          MessageBeep(0)
        else
          if (Form <> nil) and FDroppedDown then
            SetIndex(0, Self.ColCount)
          else
            SetIndex(0, 1);

      VK_PRIOR:
        if ReadOnly then
          MessageBeep(0)
        else
          if (Form <> nil) and FDroppedDown then
            SetIndex(0, -Self.ColCount * RowCount)
          else
            SetIndex(0, -1);

      VK_NEXT:
        if ReadOnly then
          MessageBeep(0)
        else
          if (Form <> nil) and FDroppedDown then
            SetIndex(0, Self.ColCount * RowCount)
          else
            SetIndex(0, 1);

      VK_RETURN:
        if (Form <> nil) and IsValidIndex(Form.BtnIndex, Form.ScrollBox.ControlCount) then
          Form.ClickBtn(TacSkinSelectBtn(Form.ScrollBox.Controls[Form.BtnIndex]));

      else
        inherited KeyDown(Key, Shift);
    end;
}
end;

{
procedure TsSkinSelector.DoDropDown;
begin
  if AllowDropDown then
    inherited
  else
    if not FDropDown then begin
      if (SkinData.SkinManager <> nil) and
           ((Form = nil) or (Form.sp = nil) or ((Form.sp.FormState and FS_ANIMCLOSING = 0) or (acHideTimer = nil) or not acHideTimer.Enabled)) then begin
        if CanFocus then
          SetFocus;

        FDropDown := True;
        StopTimer(SkinData);
        SkinData.Invalidate(True);
        if Form = nil then begin
          Form := TacSkinSelectForm.CreateNew(Self);
          Form.sp.AllowScale := False;
          Form.sp.AllowAnimation := True;
          Form.Name := Name + 'Form';
          Form.UpdateFlags := Form.UpdateFlags + [ufFormCreating];
        end;
        Form.UpdateFlags := Form.UpdateFlags + [ufUpdateScale];
        UpdateForm;
        Form.UpdateHotControl;
        Form.UpdateFlags := Form.UpdateFlags - [ufFormCreating];
        ShowPopupForm(Form, Self);
      end;
    end
    else
      if (Form <> nil) and IsWindowVisible(Form.Handle) then begin
        FDropDown := False;
        StopTimer(SkinData);
        SkinData.Invalidate(True);
        Form.Close;
      end;
end;
}
{
function TsSkinSelector.DroppedDown: boolean;
begin
  Result := FDroppedDown;
end;
}

function TsSkinSelector.GetEditText(AIndex: integer): acString;
begin
  if AIndex < 0 then
    if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
      Result := SkinData.SkinManager.SkinName
    else
      Result := ''
  else
    Result := Items[AIndex];
end;


function TsSkinSelector.HandleKeyDown(var ACharCode: word): boolean;
begin
  Result := True;
  if ReadOnly then
    case ACharCode of
      VK_HOME, VK_END, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_RETURN:
        MessageBeep(0);

      else
        Result := False;
    end
  else
    if (Form <> nil) and FDroppedDown then
      case ACharCode of
        VK_RETURN:
          if IsValidIndex(Form.BtnIndex, Form.ScrollBox.ControlCount) then
            Form.ClickBtn(TacSkinSelectBtn(Form.ScrollBox.Controls[Form.BtnIndex]));

        VK_ESCAPE: Form.Close;
        VK_HOME:  SetIndex(0, 0);
        VK_END:   SetIndex(Items.Count + Integer(ShowNoSkin) - 1, 0);
        VK_LEFT:  SetIndex(0, -1);
        VK_RIGHT: SetIndex(0, 1);
        VK_UP:    SetIndex(0, -Self.ColCount);
        VK_DOWN:  SetIndex(0, Self.ColCount);
        VK_PRIOR: SetIndex(0, -Self.ColCount * RowCount);
        VK_NEXT:  SetIndex(0, Self.ColCount * RowCount)
        else
          Result := False;
      end
    else
      case ACharCode of
        VK_HOME: SetIndex(0, 0);
        VK_END: SetIndex(Items.Count + Integer(ShowNoSkin) - 1, 0);
        VK_PRIOR, VK_UP, VK_LEFT:  SetIndex(0, -1);
        VK_NEXT, VK_DOWN, VK_RIGHT: SetIndex(0, 1)
        else
          Result := False;
      end;

  if not Result then
    Result := inherited HandleKeyDown(ACharCode);
end;


function TsSkinSelector.ItemTextDefined(AIndex: integer): boolean;
begin
  if (AIndex < 0) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
    Result := True
  else
    Result := inherited ItemTextDefined(AIndex);
end;


procedure TsSkinSelector.SetColCount(const Value: byte);
begin
  if FColCount <> Value then begin
    FColCount := Value;
    UpdateForm;
  end;
end;


procedure TsSkinSelector.SetIndex(BtnIndex: integer; Step: integer);
begin
  if (Form <> nil) and DroppedDown and (Form.ScrollBox <> nil) and (Form.ScrollBox.ControlCount > 0) then
    if Step <> 0 then
      Form.SetBtnIndex(max(0, min(Form.ItemCount - 1, Form.BtnIndex + Step)))
    else
      Form.SetBtnIndex(BtnIndex)
  else begin
    if Items.Count = 0 then
      PopulateList(True);

    if Step <> 0 then
      ItemIndex := max(0, min(Items.Count - 1, ItemIndex + Step))
    else
      if BtnIndex >= Items.Count then
        ItemIndex := Items.Count - 1
      else
        ItemIndex := BtnIndex;
  end;
end;


procedure TsSkinSelector.SetItemIndex(const Value: Integer);
begin
  if (ItemIndex <> Value) and (SkinData.SkinManager <> nil) then
    if (Value >= 0) or ShowNoSkin then begin
      inherited;
      if not FDontChangeManager then
        if IsValidIndex(ItemIndex, Items.Count) then begin
          if SkinData.SkinManager.SkinName <> ItemsEx[ItemIndex].Caption then
            SkinData.SkinManager.SkinName := ItemsEx[ItemIndex].Caption;

          SkinData.SkinManager.Active := True;
{$IFDEF DELPHI7UP}
          UpdateCaret;
{$ENDIF} // DELPHI7UP
        end
        else
          SkinData.SkinManager.Active := False;
    end;
end;


procedure TsSkinSelector.SetRowCount(const Value: byte);
begin
  if FRowCount <> Value then begin
    FRowCount := Value;
    UpdateForm;
  end;
end;


procedure TsSkinSelector.SetShowNoSkin(const Value: boolean);
begin
  if FShowNoSkin <> Value then begin
    FShowNoSkin := Value;
    UpdateForm;
    if Form <> nil then
      Form.UpdateFlags := Form.UpdateFlags + [ufUpdateThumbs];
  end;
end;


procedure TsSkinSelector.SetThumbSize(const Value: TacThumbSize);
begin
  if FThumbSize <> Value then begin
    FThumbSize := Value;
    UpdateForm;
    if Form <> nil then
      Form.UpdateFlags := Form.UpdateFlags + [ufUpdateThumbs];
  end;
end;


procedure TsSkinSelector.PopulateList(Immediate: boolean);
var
  i: integer;
begin
  ItemsEx.Clear;
  ItemsEx.BeginUpdate;
  if Assigned(SkinData.SkinManager) then begin
    if Immediate then begin
      SkinData.SkinManager.SkinListController.CheckThread;
      SkinData.SkinManager.SkinListController.UpdateIfNeeded(True);
      for i := 0 to Length(SkinData.SkinManager.SkinListController.SkinList) - 1 do
        with SkinData.SkinManager.SkinListController.SkinList[i], ItemsEx.Add do begin
          Caption := skName;
          ImageIndex := skImageIndex;
        end;
    end;
  end;
  ItemsEx.Sort;
  ItemsEx.EndUpdate;
end;


procedure TsSkinSelector.PopupWindowShow;
begin
  if AllowDropDownList then
    inherited
  else
    if not DroppedDown then begin
      if (SkinData.SkinManager <> nil) and
           ((Form = nil) or (Form.sp = nil) or ((Form.sp.FormState and FS_ANIMCLOSING = 0) or (acHideTimer = nil) or not acHideTimer.Enabled)) then begin
        if CanFocus then
          SetFocus;

        DroppedDown := True;
        StopTimer(SkinData);
        BtnState := ACPRESSED;
        SkinData.FDoLighting := False;
        FreeAndNil(SkinData.LightMask);
        FreeAndNil(SkinData.HotCache);
        SkinData.Invalidate(True);
        if Form = nil then begin
          Form := TacSkinSelectForm.CreateNew(Self);
          Form.sp.AllowScale := False;
          Form.sp.AllowAnimation := True;
          Form.Name := Name + 'Form';
          Form.UpdateFlags := Form.UpdateFlags + [ufFormCreating];
        end;
        Form.UpdateFlags := Form.UpdateFlags + [ufUpdateScale];
        UpdateForm;
        Form.UpdateHotControl;
        Form.UpdateFlags := Form.UpdateFlags - [ufFormCreating];
        ShowPopupForm(Form, Self, -1, -1, True, PopupAnimProc);
      end;
    end
    else
      if (Form <> nil) and IsWindowVisible(Form.Handle) then begin
        DroppedDown := False;
        StopTimer(SkinData);
        SkinData.Invalidate(True);
        Form.Close;
      end;
end;


procedure TsSkinSelector.UpdateForm;
begin
  if Form <> nil then begin
    Form.UpdateControls;
    Form.UpdateBoxSize;
  end;
end;


procedure TsSkinSelector.UpdateItemIndex;
var
  i: integer;
begin
  if (SkinData.SkinManager <> nil) and SkinData.SkinManager.Active and (SkinData.SkinManager.SkinName <> '') then
    i := Items.IndexOf(SkinData.SkinManager.SkinName)
  else
    i := -1;

  FDontChangeManager := True;
  ItemIndex := i;
  FDontChangeManager := False;
end;


procedure TsSkinSelector.UpdateList(Immediate: boolean);
begin
  if not (csLoading in ComponentState) then begin
    PopulateList(False);
    UpdateItemIndex;
  end;
end;

(*
procedure TsSkinSelector.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
var
  m: TCMMouseWheel;
begin
  case Message.Msg of
    WM_DESTROY:
      ItemsEx.Clear;

    WM_MOUSEWHEEL: begin
//{$IFDEF DELPHI7UP}
      if FAllowMouseWheel then
//{$ENDIF}
        if (Form <> nil) and (Form.ScrollBox <> nil) and FDropDown then begin
          m := TCMMouseWheel(Message);
          TsScrollBox(Form.ScrollBox).DoMouseWheel(m.ShiftState, m.WheelDelta, Point(m.XPos, m.YPos))
        end
        else
          if Items.Count > 0 then
            if TCMMouseWheel(Message).WheelDelta < 0 then
              ItemIndex := min(ItemIndex + 1, Items.Count - 1)
            else
              ItemIndex := max(ItemIndex - 1, -1);

      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_KEYDOWN:
      KeyDown(TWMKey(Message).CharCode, KeyDataToShiftState(TWMKey(Message).KeyData));

    WM_SETCURSOR:
      SetCursor(Screen.Cursors[Cursor]);
  end;
end;
*)

procedure TsSkinSelector.WndProc(var Message: TMessage);
var
  m: TCMMouseWheel;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN: begin
          inherited;
          if Form <> nil then
            AlphaBroadCast(Form, Message);

          if ParamStr(1) <> s_PreviewKey then
            UpdateItemIndex;

          Exit;
        end;
{
        AC_POPUPCLOSED: begin
          StopTimer(SkinData);
          FDroppedDown := False;
          SkinData.Invalidate(True);
        end;
}
        AC_SKINLISTCHANGED: begin
          UpdateList(False);
          if Form <> nil then
            Form.UpdateFlags := Form.UpdateFlags + [ufUpdateThumbs];
        end;

        AC_SKINCHANGED:
          if ParamStr(1) <> s_PreviewKey then
            UpdateItemIndex;

        AC_SETSCALE: begin
          FreeAndNil(Form);
          SkinData.CommonSkinData := nil;
        end;

        AC_FONTSCHANGED:
          SkinData.SkinManager.UpdateFontName(Self);
      end;

//  CM_RECREATEWND: FreeAndNil(Form); // Error occurs if message is sent in the Screen.Forms.Count loop

    WM_MOUSEWHEEL: begin
      if AllowMouseWheel then
        if (Form <> nil) and (Form.ScrollBox <> nil) and DroppedDown then begin
          m := TCMMouseWheel(Message);
          TsScrollBox(Form.ScrollBox).DoMouseWheel(m.ShiftState, m.WheelDelta, Point(m.XPos, m.YPos))
        end
        else
          if Items.Count > 0 then
            if TCMMouseWheel(Message).WheelDelta < 0 then
              ItemIndex := min(ItemIndex + 1, Items.Count - 1)
            else
              ItemIndex := max(ItemIndex - 1, -1);

      Exit;
    end;

    CN_COMMAND:
      case TWMCommand(Message).NotifyCode of
        CBN_DROPDOWN:
          DoDropDown;
      end;

    CB_SHOWDROPDOWN:
      if Message.WParam <> 0 then begin
        DoDropDown;
        Exit;
      end;
  end;
  inherited;
end;


{$IFDEF DELPHI7UP}
procedure TacComboBoxExActionLink.AddItem(AnItem: TListControlItem);
begin
  with FClient as TsCustomComboBoxEx do
    ItemsEx.AddItem(AnItem.Caption, AnItem.ImageIndex, AnItem.ImageIndex, -1, -1, AnItem.Data);
end;


procedure TacComboBoxExActionLink.AddItem(ACaption: String; AImageIndex: Integer; DataPtr: TCustomData);
begin
  with FClient as TsCustomComboBoxEx do
    ItemsEx.AddItem(ACaption, AImageIndex, AImageIndex, -1, -1, DataPtr);
end;
{$ENDIF} // DELPHI7UP

{$IFNDEF DELPHI7UP}
procedure TListControlItem.Assign(Source: TPersistent);
begin
  if Source is TListControlItem then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      Caption := TListControlItem(Source).Caption;
      ImageIndex := TListControlItem(Source).ImageIndex;
      Data := TListControlItem(Source).Data;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end;
end;

procedure TListControlItem.Changed;
begin
  if Assigned(FListControlItems) then
    FListControlItems.Update(Self);
end;

constructor TListControlItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
end;

function TListControlItem.GetDisplayName: String;
begin
  if Length(Caption) > 0 then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TListControlItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed;
end;

procedure TListControlItem.SetData(const Value: Pointer);
begin
  FData := Value;
  Changed;
end;

procedure TListControlItem.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  Changed;
end;

{ TListControlItems }

function ListItemsCompare(List: TListControlItems; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareItems(List.Items[Index1], List.Items[Index2]);
end;

function TListControlItems.Add: TListControlItem;
begin
  Result := TListControlItem(inherited Add);
  Result.FListControlItems := Self;
end;

procedure TListControlItems.CustomSort(Compare: TListItemsCompare);
begin
  if (SortType <> stNone) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

function TListControlItems.CompareItems(I1, I2: TListControlItem): Integer;
begin
  if Assigned(OnCompare) then
    Result := OnCompare(Self, I1, I2)
  else
    if CaseSensitive then
      Result := AnsiCompareStr(I1.Caption, I2.Caption)
    else
      Result := AnsiCompareText(I1.Caption, I2.Caption);
end;

procedure TListControlItems.ExchangeItems(Index1, Index2: Integer);
var
  Item1, Item2: TListControlItem;
  I1, I2: Integer;
begin
  Item1 := Items[Index1];
  Item2 := Items[Index2];
  I1 := Items[Index1].Index;
  I2 := Items[Index2].Index;
  Item1.Index := I2;
  Item2.Index := I1;
end;

procedure TListControlItems.QuickSort(L, R: Integer; SCompare: TListItemsCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
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
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

function TListControlItems.GetListItem(
  const Index: Integer): TListControlItem;
begin
  Result := TListControlItem(GetItem(Index));
end;

procedure TListControlItems.SetSortType(const Value: TListItemsSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    if Value <> stNone then
      CustomSort(ListItemsCompare);
  end;
end;

procedure TListControlItems.Sort;
begin
  CustomSort(ListItemsCompare);
end;

constructor TListControlItems.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FCaseSensitive := False;
  FSortType := stNone;
end;
{$ENDIF}


{$IFNDEF ALITE}

initialization

finalization
  {IFDEF DELPHI7UP}
  if Assigned(ColorsHolder) then
    ColorsHolder.Free;
  {ENDIF} // DELPHI7UP

  if ColDlg <> nil then
    FreeAndNil(ColDlg);
{$ENDIF} // ALITE

end.
