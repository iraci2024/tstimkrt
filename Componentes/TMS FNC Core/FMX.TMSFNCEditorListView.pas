{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021 - 2023                               }
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

unit FMX.TMSFNCEditorListView;

{$I FMX.TMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF} 
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types , FMX.TMSFNCTypes, FMX.Controls, FMX.StdCtrls, FMX.TMSFNCCustomControl
  , FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes
  {$IFDEF FNCLIB}
  ,FMX.TMSFNCBitmapEditor
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Edit
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  , ShellApi, Windows
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLib.Forms
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Winapi.Messages, Vcl.ActnList
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : Added Item Text color

type
  TTMSFNCListEditorItemRectType = (eirtNone, eirtItem, eirtImage, eirtName, eirtDataString);
  TTMSFNCEditorListView = class;

  TTMSFNCEditorListViewAppearance = class(TPersistent)
  private
    FOwner: TTMSFNCEditorListView;
    FItemDownStroke: TTMSFNCGraphicsStroke;
    FItemFont: TTMSFNCGraphicsFont;
    FItemSelectedFill: TTMSFNCGraphicsFill;
    FItemHoverFont: TTMSFNCGraphicsFont;
    FEditFont: TTMSFNCGraphicsFont;
    FItemFill: TTMSFNCGraphicsFill;
    FItemSelectedStroke: TTMSFNCGraphicsStroke;
    FItemHoverFill: TTMSFNCGraphicsFill;
    FItemStroke: TTMSFNCGraphicsStroke;
    FItemDownFont: TTMSFNCGraphicsFont;
    FItemHoverStroke: TTMSFNCGraphicsStroke;
    FOnChanged: TNotifyEvent;
    FFill: TTMSFNCGraphicsFill;
    FItemDownFill: TTMSFNCGraphicsFill;
    FItemSelectedFont: TTMSFNCGraphicsFont;
    FStroke: TTMSFNCGraphicsStroke;
    FItemVerticalSpacing: Single;
    FItemRounding: Integer;
    FItemImageHoverStroke: TTMSFNCGraphicsStroke;
    FItemImageDownFill: TTMSFNCGraphicsFill;
    FItemImageDownStroke: TTMSFNCGraphicsStroke;
    FItemImageSelectedFill: TTMSFNCGraphicsFill;
    FItemImageFill: TTMSFNCGraphicsFill;
    FItemImageSelectedStroke: TTMSFNCGraphicsStroke;
    FItemImageHoverFill: TTMSFNCGraphicsFill;
    FItemImageStroke: TTMSFNCGraphicsStroke;
    FItemImageRounding: Integer;
    FUseImageAppearance: Boolean;
    FItemHorizontalSpacing: Single;
    FStrokeSides: TTMSFNCGraphicsSides;
    procedure SetEditFont(const Value: TTMSFNCGraphicsFont);
    procedure SetItemDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemDownFont(const Value: TTMSFNCGraphicsFont);
    procedure SetItemDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemFont(const Value: TTMSFNCGraphicsFont);
    procedure SetItemHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemHoverFont(const Value: TTMSFNCGraphicsFont);
    procedure SetItemHoverStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemSelectedFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemSelectedFont(const Value: TTMSFNCGraphicsFont);
    procedure SetItemSelectedStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemVerticalSpacing(const Value: Single);
    procedure SetItemRounding(const Value: Integer);
    procedure SetItemImageDownFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemImageDownStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemImageFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemImageHoverFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemImageHoverStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemImageSelectedFill(const Value: TTMSFNCGraphicsFill);
    procedure SetItemImageSelectedStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemImageStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetItemImageRounding(const Value: Integer);
    procedure SetUseImageAppearance(const Value: Boolean);
    procedure SetItemHorizontalSpacing(const Value: Single);
    procedure SetStrokeSides(const Value: TTMSFNCGraphicsSides);
  protected
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoImageChanged(Sender: TObject); virtual;
    procedure DoFillChanged(Sender: TObject); virtual;
    procedure DoEditFontChanged(Sender: TObject); virtual;
    procedure DoStrokeChanged(Sender: TObject); virtual;
    procedure DoItemFillChanged(Sender: TObject); virtual;
    procedure DoItemFontChanged(Sender: TObject); virtual;
    procedure DoItemStrokeChanged(Sender: TObject); virtual;
    procedure DoItemImageFillChanged(Sender: TObject); virtual;
    procedure DoItemImageStrokeChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TTMSFNCEditorListView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TTMSFNCGraphicsFill read FFill write SetItemFill;
    property EditFont: TTMSFNCGraphicsFont read FEditFont write SetEditFont;
    property Stroke: TTMSFNCGraphicsStroke read FStroke write SetStroke;
    property ItemHoverFill: TTMSFNCGraphicsFill read FItemHoverFill write SetItemHoverFill;
    property ItemHoverFont: TTMSFNCGraphicsFont read FItemHoverFont write SetItemHoverFont;
    property ItemHoverStroke: TTMSFNCGraphicsStroke read FItemHoverStroke write SetItemHoverStroke;
    property ItemDownFill: TTMSFNCGraphicsFill read FItemDownFill write SetItemDownFill;
    property ItemDownFont: TTMSFNCGraphicsFont read FItemDownFont write SetItemDownFont;
    property ItemDownStroke: TTMSFNCGraphicsStroke read FItemDownStroke write SetItemDownStroke;
    property ItemSelectedFill: TTMSFNCGraphicsFill read FItemSelectedFill write SetItemSelectedFill;
    property ItemSelectedFont: TTMSFNCGraphicsFont read FItemSelectedFont write SetItemSelectedFont;
    property ItemSelectedStroke: TTMSFNCGraphicsStroke read FItemSelectedStroke write SetItemSelectedStroke;
    property ItemFill: TTMSFNCGraphicsFill read FItemFill write SetItemFill;
    property ItemFont: TTMSFNCGraphicsFont read FItemFont write SetItemFont;
    property ItemStroke: TTMSFNCGraphicsStroke read FItemStroke write SetItemStroke;
    property ItemImageHoverFill: TTMSFNCGraphicsFill read FItemImageHoverFill write SetItemImageHoverFill;
    property ItemImageHoverStroke: TTMSFNCGraphicsStroke read FItemImageHoverStroke write SetItemImageHoverStroke;
    property ItemImageDownFill: TTMSFNCGraphicsFill read FItemImageDownFill write SetItemImageDownFill;
    property ItemImageDownStroke: TTMSFNCGraphicsStroke read FItemImageDownStroke write SetItemImageDownStroke;
    property ItemImageSelectedFill: TTMSFNCGraphicsFill read FItemImageSelectedFill write SetItemImageSelectedFill;
    property ItemImageSelectedStroke: TTMSFNCGraphicsStroke read FItemImageSelectedStroke write SetItemImageSelectedStroke;
    property ItemImageFill: TTMSFNCGraphicsFill read FItemImageFill write SetItemImageFill;
    property ItemImageStroke: TTMSFNCGraphicsStroke read FItemImageStroke write SetItemImageStroke;
    property ItemVerticalSpacing: Single read FItemVerticalSpacing write SetItemVerticalSpacing;
    property ItemHorizontalSpacing: Single read FItemHorizontalSpacing write SetItemHorizontalSpacing;
    property ItemRounding: Integer read FItemRounding write SetItemRounding;
    property ItemImageRounding: Integer read FItemImageRounding write SetItemImageRounding;
    property UseImageAppearance: Boolean read FUseImageAppearance write SetUseImageAppearance;
    property StrokeSides: TTMSFNCGraphicsSides read FStrokeSides write SetStrokeSides;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTMSFNCEditorListItem = class(TCollectionItem)
  private
    FDrawRect: TRectF;
    FImgRect: TRectF;
    FNameRect: TRectF;
    FDataStringRect: TRectF;
    FBitmap: TTMSFNCBitmap;
    FTag: NativeInt;
    FName: string;
    FDataObject: TObject;
    FDataString: string;
    FOnChanged: TNotifyEvent;
    FItemHeight: single;
    FSelected: Boolean;
    FFontColor: TTMSFNCGraphicsColor;
    FSelectedFontColor: TTMSFNCGraphicsColor;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
    procedure SetDataObject(const Value: TObject);
    procedure SetDataString(const Value: string);
    procedure SetName(const Value: string);
    procedure DoChanged;
    procedure DoSelectItemChanged(AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
    procedure SetItemHeight(const Value: single);
    procedure SetSelected(const Value: Boolean);
    procedure SetFontColor(const Value: TTMSFNCGraphicsColor);
    procedure SetSelectedFontColor(const Value: TTMSFNCGraphicsColor);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure Assign(Source: TPersistent); override;
    property DrawRect: TRectF read FDrawRect;
  published
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property Name: string read FName write SetName;
    property DataObject: TObject read FDataObject write SetDataObject;
    property DataString: string read FDataString write SetDataString;
    property Tag: NativeInt read FTag write FTag;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property FontColor: TTMSFNCGraphicsColor read FFontColor write SetFontColor;
    property SelectedFontColor: TTMSFNCGraphicsColor read FSelectedFontColor write SetSelectedFontColor;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  TTMSFNCEditorListItemArray = Array of TTMSFNCEditorListItem;

  TTMSFNCEditorListCollection = class(TOwnedCollection)
  private
    FOwner: TTMSFNCEditorListView;
    FOnChanged: TNotifyEvent;
    function GetItemEx(Index: Integer): TTMSFNCEditorListItem;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCEditorListItem);
  protected
      function GetEditorListItemClass: TCollectionItemClass; virtual;
      procedure DoChanged; virtual;
  public
    constructor Create(AOwner: TTMSFNCEditorListView);
    procedure Assign(Source: TPersistent); override;
    function Add: TTMSFNCEditorListItem;
    function Insert(index: Integer): TObject;
    property Items[Index: Integer]: TTMSFNCEditorListItem read GetItemEx write SetItemEx; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTMSFNCOnDoubleClickEditorListItem = procedure(Sender: TObject; AIndex: integer; AItem: TTMSFNCEditorListItem; X, Y: Single) of object;
  TTMSFNCOnSelectItemChanged = procedure(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean) of object;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCEditorListView = class(TTMSFNCCustomControl)
  private
    FItemsChanged: Boolean;
    FLastSelectedItemIndex: Integer;
    FKeyIndex: integer;
    {$IFDEF FNCLIB}
    FBitmapEditor: TTMSFNCBitmapEditor;
    {$ENDIF}
    FEdit: TEdit;
    FEditMode: Boolean;
    FEditType: TTMSFNCListEditorItemRectType;
    FEditIndex: Integer;
    FPrevText: string;
    FIntUpdateBlock: Boolean;
    FUpdateBlock: Boolean;
    FItems: TTMSFNCEditorListCollection;
    FDefaultItemHeight: Single;
    FImgMargin: Single;
    FHoverIndex: Integer;
    FDragStart: TPointF;
    FMouseDown: Boolean;
    FDownIndex: Integer;
    FAddItemIndex: Integer;
    FAddItemRect: TRectF;
    FOnItemsChanged: TNotifyEvent;
    FOnDoubleClickItem: TTMSFNCOnDoubleClickEditorListItem;
    FOnAddNewItem: TNotifyEvent;
    FOnItemSelectedChanged: TTMSFNCOnSelectItemChanged;
    FAppearance: TTMSFNCEditorListViewAppearance;
    FDrawDataString: Boolean;
    FItemsReadOnly: Boolean;
    FMultiSelect: Boolean;
    FCanUnselectItems: Boolean;
    function GetItems: TTMSFNCEditorListCollection;
    procedure SetItems(const Value: TTMSFNCEditorListCollection);
    procedure SetDefaultItemHeight(const Value: Single);
    function GetItemIndex(X, Y: Single): Integer;
    function GetItemRectType(X, Y: Single; AIndex: integer = -1): TTMSFNCListEditorItemRectType;
    procedure AssignBitmap(AItem: TTMSFNCEditorListItem);
    procedure SetAppearance(const Value: TTMSFNCEditorListViewAppearance);
    procedure SetDrawDataString(const Value: Boolean);
    procedure SetItemsReadOnly(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    {$IFNDEF WEBLIB}
    function IsFileSupported(AFileName: string): Boolean; virtual;
    {$ENDIF}
    {$IFDEF WEBLIB}
    procedure AssignBitmapCallBack(AValue: TModalResult);
    {$ENDIF}
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    function MultiSelected: Boolean;
    procedure MoveItems(X, Y: Single); virtual;
    procedure DeleteSelectedItems; virtual;
    {$IFDEF FMXLIB}
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    {$ENDIF}
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyPress(var {%H-}Key: Char); override;
    procedure HandleMouseDown({%H-}Button: TTMSFNCMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseUp({%H-}Button: TTMSFNCMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    {$IFDEF FMXLIB}
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    {$ENDIF}
    procedure HandleDblClick(X, Y: Single); override;
    function CreateItems: TTMSFNCEditorListCollection; virtual;
    Procedure SetItemRect(AIndex: integer; X, Y: Single); virtual;
    Procedure SetItemImageRect(AIndex: integer); virtual;
    Procedure SetItemNameRect(AIndex: integer); virtual;
    Procedure SetItemDataStringRect(AIndex: integer); virtual;
    procedure InitializeItems; virtual;
    procedure UpdateControlAfterResize; override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure DoAddNewItem; virtual;
    procedure DoDblClickItem(AIndex: integer; AItem: TTMSFNCEditorListItem; X, Y: Single); virtual;
    procedure DoItemsChanged(Sender: TObject); virtual;
    procedure DoSelectItemChanged(AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean); virtual;
    procedure Draw({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBackground(AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBitmap({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF; AItem: TTMSFNCEditorListItem); virtual;
    procedure DrawItems({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF; AItemCollection: TTMSFNCEditorListCollection); virtual;
    procedure DrawListItem({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF; AIndex:Integer; AItem: TTMSFNCEditorListItem); virtual;
    procedure SelectItem(AIndex:Integer); virtual;
    procedure SelectItemInt(AIndex:Integer); virtual;
    function GetSelectedItems: TTMSFNCEditorListItemArray; virtual;
    property ItemsReadOnly: Boolean read FItemsReadOnly write SetItemsReadOnly default false;
    property Appearance: TTMSFNCEditorListViewAppearance read FAppearance write SetAppearance;
    property Items: TTMSFNCEditorListCollection read GetItems write SetItems;
    property DefaultItemHeight: Single read FDefaultItemHeight write SetDefaultItemHeight;
    property LastSelectedItemIndex: Integer read FLastSelectedItemIndex;
    property DrawDataString: Boolean read FDrawDataString write SetDrawDataString;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default True;
    property CanUnselectItems: Boolean read FCanUnselectItems write FCanUnselectItems default True;
    property OnAddNewItem: TNotifyEvent read FOnAddNewItem write FOnAddNewItem;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnDoubleClickItem: TTMSFNCOnDoubleClickEditorListItem read FOnDoubleClickItem write FOnDoubleClickItem;
    property OnItemSelectedChanged: TTMSFNCOnSelectItemChanged read FOnItemSelectedChanged write FOnItemSelectedChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function GetListHeight: Integer; virtual;
    procedure UpdateList; virtual;
    procedure UnselectAllItems;
    procedure SelectAllItems;
    {$IFDEF VCLLIB}
    procedure SetAcceptDrag(Value: Boolean); virtual;
    {$ENDIF}
    procedure StartEditMode;
    procedure EndEditMode(const Execute: Boolean);
    property EditMode: Boolean read FEditMode;
  end;

  TTMSFNCBitmapEditorListView = class(TTMSFNCEditorListView)
  {$IFDEF CMNLIB}
  private
    {$IFDEF VCLLIB}
    procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
    {$ENDIF}
 {$ENDIF}
  protected
    {$IFNDEF WEBLIB}
    function IsFileSupported(AFileName: string): Boolean; override;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    {$ENDIF}
  public
    procedure DeleteSelectedItems; override;
    property LastSelectedItemIndex;
    procedure SelectItem(AIndex:Integer); override;
    function GetSelectedItems: TTMSFNCEditorListItemArray; override;
    {$IFDEF LCLLIB}
    procedure AddDroppedFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}
  published
    property DrawDataString;
    property Appearance;
    property Items;
    property DefaultItemHeight;
    property ItemsReadOnly;
    property OnAddNewItem;
    property OnItemsChanged;
    property OnDoubleClickItem;
    property OnItemSelectedChanged;
  end;

  TTMSFNCEditorList = class(TTMSFNCEditorListView)
  protected
      Procedure SetItemImageRect(AIndex: integer); override;
  public
      function GetSelectedItems: TTMSFNCEditorListItemArray; override;
      procedure SelectItem(AIndex:Integer); override;
  published
    property DrawDataString;
    property Appearance;
    property Items;
    property DefaultItemHeight;
    property ItemsReadOnly;
    property LastSelectedItemIndex;
    property MultiSelect;
    property CanUnselectItems;
    property OnAddNewItem;
    property OnItemsChanged;
    property OnDoubleClickItem;
    property OnItemSelectedChanged;
  end;

implementation

uses
  TypInfo, SysUtils, FMX.TMSFNCUtils, Math, FMX.Graphics
  {$IFNDEF LIMITEDGRAPHICSMODE}
  , FMX.TMSFNCBitmapContainer
  {$ENDIF}
  {$IFDEF FNCLIB}
  , FMX.TMSFNCURLBitmapContainer
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Winapi.Windows, Winapi.ShellAPI
  {$ENDIF}
  ;

{ TTMSFNCEditorListView }

procedure TTMSFNCEditorListView.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TTMSFNCEditorListView.AssignBitmap(AItem: TTMSFNCEditorListItem);
begin
  {$IFDEF FNCLIB}
  FBitmapEditor.Bitmap := AItem.Bitmap;
  {$ENDIF}

  {$IFDEF WEBLIB}
  FEditIndex := AItem.Index;
  FBitmapEditor.Execute(@AssignBitmapCallBack);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$IFDEF FNCLIB}
  FBitmapEditor.Execute;
  {$ENDIF}
  Repaint;
  {$ENDIF}
end;

{$IFDEF WEBLIB}
procedure TTMSFNCEditorListView.AssignBitmapCallback(AValue: TModalResult);
begin
  if (AValue = mrOk) and (FEditIndex >= 0) then
  begin
    FItems[FEditIndex].Bitmap.Assign(FBitmapEditor.Bitmap);
  end;

  FEditIndex := -1;

  Repaint;
end;
{$ENDIF}

procedure TTMSFNCEditorListView.BeginUpdate;
begin
  inherited;
  FUpdateBlock := True;
end;

procedure TTMSFNCEditorListView.ChangeDPIScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;

  BeginUpdate;

  FAppearance.FItemFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FItemFont.Height, M, D);
  FAppearance.FItemHoverFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FItemHoverFont.Height, M, D);
  FAppearance.FItemDownFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FItemDownFont.Height, M, D);
  FAppearance.FItemSelectedFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FItemSelectedFont.Height, M, D);
  FAppearance.FItemImageRounding := TTMSFNCUtils.MulDivInt(FAppearance.FItemImageRounding, M, D);
  FAppearance.FItemRounding := TTMSFNCUtils.MulDivInt(FAppearance.FItemRounding, M, D);
  FAppearance.FEditFont.Height := TTMSFNCUtils.MulDivInt(FAppearance.FEditFont.Height, M, D);
  FAppearance.FItemVerticalSpacing := TTMSFNCUtils.MulDivSingle(FAppearance.FItemVerticalSpacing, M, D);
  FAppearance.FItemHorizontalSpacing := TTMSFNCUtils.MulDivSingle(FAppearance.FItemHorizontalSpacing, M, D);

  FDefaultItemHeight := TTMSFNCUtils.MulDivSingle(FDefaultItemHeight, M, D);

  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].FItemHeight := TTMSFNCUtils.MulDivSingle(FItems[I].FItemHeight, M, D);
  end;

  EndUpdate;
end;

constructor TTMSFNCEditorListView.Create(AOwner: TComponent);
begin
  inherited;

  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  NativeCanvas := True;
  TextQuality := gtqClearType;
  {$ENDIF}
  {$ENDIF}

  Fill.Kind := gfkNone;
  Stroke.Kind := gskNone;

  FMultiSelect := True;
  FCanUnselectItems := True;

  FItems := CreateItems;
  FItems.OnChanged := DoItemsChanged;
  FItemsChanged := False;
  FDefaultItemHeight := 60;
  FImgMargin := 5;

  FHoverIndex := -1;
  FDownIndex := -1;
  FLastSelectedItemIndex := - 1;

  FAddItemIndex := -10;
  FAddItemRect := RectF(-1,-1,-1,-1);

  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Visible := False;
  FEdit.OnKeyDown := DoEditKeyDown;
  {$IFDEF FNCLIB}
  FBitmapEditor := TTMSFNCBitmapEditor.Create(Self);
  {$ENDIF}

  FAppearance := TTMSFNCEditorListViewAppearance.Create(Self);
  FAppearance.OnChanged := DoAppearanceChanged;

  Width := 300;
  Height := 400;
end;

function TTMSFNCEditorListView.CreateItems: TTMSFNCEditorListCollection;
begin
  Result := TTMSFNCEditorListCollection.Create(Self);
end;

procedure TTMSFNCEditorListView.DeleteSelectedItems;
var
  I: Integer;
begin
  if FEditMode then
    EndEditMode(False);
     
  FIntUpdateBlock := True;
  for I := FItems.Count - 1 downto 0 do
  begin
    if FItems[I].Selected then
      FItems.Delete(I);
  end;
  FIntUpdateBlock := False;
  UpdateList;
end;

destructor TTMSFNCEditorListView.Destroy;
begin
  FAppearance.Free;
  FItems.Free;
  FEdit.Free;
  {$IFDEF FNCLIB}
  FBitmapEditor.Free;
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCEditorListView.DoAddNewItem;
begin
  if Assigned(OnAddNewItem) then
    OnAddNewItem(Self);
end;

procedure TTMSFNCEditorListView.DoAppearanceChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TTMSFNCEditorListView.DoDblClickItem(AIndex: integer; AItem: TTMSFNCEditorListItem; X, Y: Single);
begin
  if Assigned(OnDoubleClickItem) then
    OnDoubleClickItem(Self, AIndex, AItem, X, Y);
end;

{$IFDEF FMXLIB}
procedure TTMSFNCEditorListView.DoEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

{$ENDIF}
{$IFNDEF FMXLIB}
procedure TTMSFNCEditorListView.DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
begin
  if FEditMode then
  begin
    if (Key = KEY_RETURN) then
      EndEditMode(True)
    else if (Key = KEY_ESCAPE) then
    begin
      EndEditMode(False);
    end;
  end;
end;

procedure TTMSFNCEditorListView.DoItemsChanged(Sender: TObject);
begin
  if FIntUpdateBlock or FUpdateBlock then
  begin
    FItemsChanged := True;
    Exit;
  end;

  if Assigned(OnItemsChanged) then
    OnItemsChanged(Self);

  UpdateList;
end;

procedure TTMSFNCEditorListView.DoSelectItemChanged(AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
begin
  if Assigned(OnItemSelectedChanged) then
    OnItemSelectedChanged(Self, AIndex, AItem, ASelected);
end;

{$IFDEF FMXLIB}
procedure TTMSFNCEditorListView.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  inherited;
end;

procedure TTMSFNCEditorListView.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;
  if Length(Data.Files) > 0 then
  begin
    Operation := TDragOperation.Copy;
  end;
end;
{$ENDIF}

procedure TTMSFNCEditorListView.Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  DrawItems(AGraphics, ARect, FItems);
end;

procedure TTMSFNCEditorListView.DrawBackground(AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  AGraphics.Fill.Assign(FAppearance.FFill);

  
  AGraphics.Stroke.Assign(FAppearance.FStroke);
  AGraphics.DrawRectangle(ARect);

//  AGraphics.Stroke.Assign(FAppearance.FStroke);
//  sw := FAppearance.Stroke.Width;
//
//  if gsLeft in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Left, ARect.Bottom));
//  if gsRight in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Right - sw, ARect.Top), PointF(ARect.Right - sw, ARect.Bottom));
//  if gsTop in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Right, ARect.Top));
//  if gsBottom in FAppearance.FStrokeSides then
//    AGraphics.DrawLine(PointF(ARect.Left, ARect.Bottom), PointF(ARect.Right, ARect.Bottom));
end;

procedure TTMSFNCEditorListView.DrawBitmap(AGraphics: TTMSFNCGraphics; ARect: TRectF; AItem: TTMSFNCEditorListItem);
var
  r: TRectF;
begin
  if ARect.Right > 0 then
  begin
    if FAppearance.UseImageAppearance then
    begin
      if AItem.Selected then
      begin
        AGraphics.Fill.Assign(FAppearance.ItemImageSelectedFill);
        AGraphics.Stroke.Assign(FAppearance.ItemImageSelectedStroke);
      end
      else if AItem.Index = FDownIndex then
      begin
        AGraphics.Fill.Assign(FAppearance.ItemImageDownFill);
        AGraphics.Stroke.Assign(FAppearance.ItemImageDownStroke);
      end
      else if AItem.Index = FHoverIndex then
      begin
        AGraphics.Fill.Assign(FAppearance.ItemImageHoverFill);
        AGraphics.Stroke.Assign(FAppearance.ItemImageHoverStroke);
      end
      else
      begin
        AGraphics.Fill.Assign(FAppearance.ItemImageFill);
        AGraphics.Stroke.Assign(FAppearance.ItemImageStroke);
      end;
    end;

    r := ARect;
    AGraphics.DrawRoundRectangle(r, Round(Min(FAppearance.ItemImageRounding, (r.Bottom - r.Top)/2)));

    InflateRectEx(r, -2, -2);

    AGraphics.DrawBitmap(r, AItem.Bitmap);
  end;
end;

procedure TTMSFNCEditorListView.DrawItems(AGraphics: TTMSFNCGraphics; ARect: TRectF; AItemCollection: TTMSFNCEditorListCollection);
var
  I: Integer;
begin
  for I := 0 to AItemCollection.Count - 1 do
  begin
    DrawListItem(AGraphics, AItemCollection.Items[I].FDrawRect, I, AItemCollection.Items[I]);
  end;
end;

procedure TTMSFNCEditorListView.DrawListItem(AGraphics: TTMSFNCGraphics; ARect: TRectF; AIndex: Integer; AItem: TTMSFNCEditorListItem);
var
  ro: integer;
begin
  if AItem.Selected then
  begin
    AGraphics.Fill.Assign(FAppearance.ItemSelectedFill);
    AGraphics.Font.Assign(FAppearance.ItemSelectedFont);
    AGraphics.Stroke.Assign(FAppearance.ItemSelectedStroke);
  end
  else if AIndex = FDownIndex then
  begin
    AGraphics.Fill.Assign(FAppearance.ItemDownFill);
    AGraphics.Font.Assign(FAppearance.ItemDownFont);
    AGraphics.Stroke.Assign(FAppearance.ItemDownStroke);
  end
  else if AIndex = FHoverIndex then
  begin
    AGraphics.Fill.Assign(FAppearance.ItemHoverFill);
    AGraphics.Font.Assign(FAppearance.ItemHoverFont);
    AGraphics.Stroke.Assign(FAppearance.ItemHoverStroke);
  end
  else
  begin
    AGraphics.Fill.Assign(FAppearance.ItemFill);
    AGraphics.Font.Assign(FAppearance.ItemFont);
    AGraphics.Stroke.Assign(FAppearance.ItemStroke);
  end;

  if AItem.Selected and (AItem.SelectedFontColor <> gcNull) then
    AGraphics.Font.Color := AItem.FSelectedFontColor
  else if (AItem.FontColor <> gcNull) then
    AGraphics.Font.Color := AItem.FFontColor;

  ro := Round(Min(FAppearance.ItemRounding, (AItem.FDrawRect.Bottom - AItem.FDrawRect.Top)/2));

  AGraphics.DrawRoundRectangle(AItem.FDrawRect, ro);

  DrawBitmap(AGraphics, AItem.FImgRect, AItem);

  AGraphics.DrawText(AItem.FNameRect, AItem.Name);
  if DrawDataString then
    AGraphics.DrawText(AItem.FDataStringRect, AItem.DataString);
end;

procedure TTMSFNCEditorListView.EndUpdate;
begin
  inherited;

  FUpdateBlock := False;
  UpdateList;
end;

function TTMSFNCEditorListView.GetItemIndex(X, Y: Single): Integer;
var
  I: integer;
begin
  Result := -1;

  if (FAddItemRect.Top < Y) and (FAddItemRect.Bottom > Y) then
    Result := FAddItemIndex
  else
  begin
    for I := 0 to FItems.Count - 1 do
    begin
      if (FItems[I].FDrawRect.Top < Y) and (FItems[I].FDrawRect.Bottom > Y) then
//      if TTMSFNCGraphics.PointInRect(PointF(X,Y), FItems[I].FDrawRect) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TTMSFNCEditorListView.GetItemRectType(X, Y: Single; AIndex: integer): TTMSFNCListEditorItemRectType;
begin
  if AIndex < 0 then
    AIndex := GetItemIndex(X, Y);

  if AIndex >= 0 then
  begin
    if TTMSFNCGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FImgRect) then
    begin
      Result := eirtImage;
    end
    else if TTMSFNCGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FNameRect) then
    begin
      Result := eirtName;
    end
    else if TTMSFNCGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FDataStringRect) then
    begin
      Result := eirtDataString;
    end
    else
      Result := eirtItem;
  end
  else
    Result := eirtNone;
end;

function TTMSFNCEditorListView.GetItems: TTMSFNCEditorListCollection;
begin
  Result := FItems;
end;

function TTMSFNCEditorListView.GetListHeight: Integer;
var
  I: Integer;
  s: single;
begin
  s := 0;
  for I := 0 to FItems.Count - 1 do
  begin
    s := s + FItems[I].ItemHeight;
  end;

  Result := Round(s);
end;

function TTMSFNCEditorListView.GetSelectedItems: TTMSFNCEditorListItemArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I].Selected then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1] := FItems[I];
    end;
  end;
end;

procedure TTMSFNCEditorListView.HandleDblClick(X, Y: Single);
var
  ci: Integer;
begin
  inherited;

  ci := GetItemIndex(X, Y);

  if (ci >= 0) and Assigned(FItems[ci]) then
    DoDblClickItem(ci, FItems[ci], X, Y);
end;

procedure TTMSFNCEditorListView.HandleKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
end;

procedure TTMSFNCEditorListView.HandleKeyPress(var Key: Char);
begin
  inherited;

end;

procedure TTMSFNCEditorListView.HandleKeyUp(var Key: Word; Shift: TShiftState);
var
  rp: Boolean;
begin
  inherited;

  rp := False;

  if Key = KEY_INSERT then
  begin
    DoAddNewItem;
    rp := True;
  end
  else if Key = KEY_DELETE then
  begin
    DeleteSelectedItems;
    Height := GetListHeight;
    rp := True;
  end
  else if Key = KEY_UP then
  begin
    if (ssShift in Shift) and MultiSelect then
    begin
      if FKeyIndex = -1 then
      begin
        if FLastSelectedItemIndex = -1 then
          SelectItemInt(0)
        else if FLastSelectedItemIndex > FItems.Count - 1 then
          SelectItemInt(FItems.Count - 1)
        else if FItems[FLastSelectedItemIndex].Selected then
          FKeyIndex := FLastSelectedItemIndex - 1
        else
         FKeyIndex := FLastSelectedItemIndex;
      end;


      if (FKeyIndex >= 0) and (FKeyIndex < FItems.Count) then
      begin
        if FKeyIndex > FLastSelectedItemIndex then
          FItems[FKeyIndex].Selected := False
        else
          FItems[FKeyIndex].Selected := True;
      end;
      FKeyIndex := Max(0, FKeyIndex - 1);
    end
    else
      SelectItemInt(FLastSelectedItemIndex - 1);

    rp := True;
  end
  else if Key = KEY_DOWN then
  begin
    if (ssShift in Shift) and MultiSelect then
    begin
      if FKeyIndex = -1 then
      begin
        if FLastSelectedItemIndex = -1 then
          SelectItemInt(0)
        else if FLastSelectedItemIndex > FItems.Count - 1 then
          SelectItemInt(FItems.Count - 1)
        else if FItems[FLastSelectedItemIndex].Selected then
          FKeyIndex := FLastSelectedItemIndex + 1
        else
         FKeyIndex := FLastSelectedItemIndex;
      end;

      if (FKeyIndex >= 0) and (FKeyIndex < FItems.Count) then
      begin
        if FKeyIndex < FLastSelectedItemIndex then
          FItems[FKeyIndex].Selected := False
        else
          FItems[FKeyIndex].Selected := True;
      end;
      FKeyIndex := Min(FItems.Count - 1, FKeyIndex + 1);
    end
    else
      SelectItemInt(FLastSelectedItemIndex + 1);

    rp := True;
  end
  else if (ssCtrl in Shift) then
  begin
    begin
      case Key of
        Ord('A'):
        begin
          SelectAllItems;
          rp := True;
        end;
        Ord('D'):
        begin
          UnselectAllItems;
          rp := True;
        end;
      end;
    end;
  end;

  if rp then
    Repaint;
end;

procedure TTMSFNCEditorListView.HandleMouseDown(Button: TTMSFNCMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  di: integer;
begin
  inherited;

  FKeyIndex := -1;

  if not FEditMode then
  begin
    FMouseDown := True;
    FDragStart := PointF(X,Y);

    di := GetItemIndex(X,Y);

    if di <> FDownIndex then
    begin
      FDownIndex := di;
      Repaint;
    end;
  end;
end;

procedure TTMSFNCEditorListView.HandleMouseEnter;
begin
  inherited;

end;

procedure TTMSFNCEditorListView.HandleMouseLeave;
begin
  inherited;
  FHoverIndex := -1;
  FDownIndex := -1;
  Repaint;
end;

procedure TTMSFNCEditorListView.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  hi: integer;
begin
  inherited;

  hi := GetItemIndex(X,Y);

  if hi <> FHoverIndex then
  begin
    FHoverIndex := hi;
    Repaint;
  end;
end;

procedure TTMSFNCEditorListView.HandleMouseUp(Button: TTMSFNCMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  si: integer;
  irt: TTMSFNCListEditorItemRectType;
  I: Integer;
  sel: boolean;
begin
  inherited;

  if FEditMode then
    EndEditMode(True)
  else if FMouseDown and (FDownIndex <> -1) then
  begin
    if ((Y < FDragStart.Y + 5) and (Y > FDragStart.Y - 5)) then
    begin
      si := GetItemIndex(X,Y);

      if si = FAddItemIndex then
        DoAddNewItem
      else if si >=0 then
      begin
        if (ssShift in Shift) and MultiSelect then
        begin
          if FLastSelectedItemIndex < 0 then
            FLastSelectedItemIndex := 0
          else if FLastSelectedItemIndex > FItems.Count - 1 then
            FLastSelectedItemIndex := FItems.Count - 1;

          for I := Min(si, FLastSelectedItemIndex) to Max(si, FLastSelectedItemIndex) do
          begin
            FItems[I].Selected := True;
          end;

          FLastSelectedItemIndex := si;
        end
        else
        begin
          irt := GetItemRectType(X, Y, si);

          if (Button = TTMSFNCMouseButton.mbRight) then
            irt := eirtName;
          if FItemsReadOnly and (irt in [eirtName, eirtDataString]) then
            irt := eirtItem;

          case irt of
            eirtItem:
            begin
              sel := FItems[si].Selected;
              if not (ssCtrl in Shift) or not MultiSelect then
                UnselectAllItems;

              FLastSelectedItemIndex := si;

              if not FCanUnselectItems then
                FItems[si].Selected := True
              else
                FItems[si].Selected := not sel;
            end;
            eirtImage: AssignBitmap(FItems[si]);
            eirtName,eirtDataString:
            begin
              FEditIndex := si;
              FEditType := irt;
              if not (ssCtrl in Shift) then
                UnselectAllItems;
              FItems[si].Selected := True;
              StartEditMode;
            end;
          end;
        end;
      end;
      Repaint;
    end
    else
    begin
      MoveItems(X, Y);
    end;
  end;

  FDownIndex := -1;
  FDragStart := PointF(-1,-1);
  FMouseDown := False;
end;

procedure TTMSFNCEditorListView.InitializeItems;
var
  i: Integer;
  y: single;
begin
  y := FAppearance.FItemVerticalSpacing;
  for I := 0 to FItems.Count - 1 do
  begin
    SetItemRect(I, 0, y);
    SetItemImageRect(I);
    SetItemNameRect(I);
    SetItemDataStringRect(I);

    y := FItems[I].FDrawRect.Bottom + FAppearance.ItemVerticalSpacing;
  end;

  Height := Round(Max(Height, y));
end;

{$IFNDEF WEBLIB}
function TTMSFNCEditorListView.IsFileSupported(AFileName: string): Boolean;
begin
  Result := True;
end;
{$ENDIF}

procedure TTMSFNCEditorListView.MoveItems(X, Y: Single);
var
  mi, di: Integer;
  I: Integer;
  c: Boolean;
begin
  c := False;
  di := FDownIndex;
  mi := GetItemIndex(X,Y);

  if mi = -1 then
  begin
    mi := FItems.Count - 1;
  end;

  if di <> -1 then
  begin
    FItems[di].SetIndex(mi);
    di := mi;
    c := true;
  end;

  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I].Selected and (di <> I) then
    begin
      if mi >= FItems.Count - 1 then
        mi := FItems.Count - 1
      else
        Inc(mi);

      FItems[I].SetIndex(mi);
      c := true;
    end;
  end;

  if c then
    DoItemsChanged(Self);
end;

function TTMSFNCEditorListView.MultiSelected: Boolean;
var
  I: Integer;
  sec: Boolean;
begin
  Result := False;
  sec := False;
  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I].Selected then
    begin
      if not sec then
        sec := True
      else
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TTMSFNCEditorListView.SelectAllItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].Selected := True;
  end;

  FKeyIndex := -1;
end;

procedure TTMSFNCEditorListView.SelectItem(AIndex: Integer);
begin
  UnselectAllItems;
  FLastSelectedItemIndex := AIndex;
  FKeyIndex := -1;
  if (AIndex >= 0) and (AIndex < FItems.Count) then
  begin        
    FItems[AIndex].Selected := True;
  end;
  
  Repaint;
end;

procedure TTMSFNCEditorListView.SelectItemInt(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FItems.Count) then
  begin
    UnselectAllItems;

    FLastSelectedItemIndex := AIndex;
    FKeyIndex := -1;
    FItems[AIndex].Selected := True;
  end;
end;

{$IFDEF VCLLIB}
procedure TTMSFNCEditorListView.SetAcceptDrag(Value: Boolean);
begin
  if HandleAllocated then
    DragAcceptFiles(Self.Handle, Value);
end;
{$ENDIF}

procedure TTMSFNCEditorListView.SetDrawDataString(const Value: Boolean);
begin
  if FDrawDataString <> Value then
  begin
    FDrawDataString := Value;
    InitializeItems;
    Repaint;
  end;
end;

procedure TTMSFNCEditorListView.SetAppearance(const Value: TTMSFNCEditorListViewAppearance);
begin
  FAppearance.Assign(Value);
  Repaint;
end;

procedure TTMSFNCEditorListView.StartEditMode;
var
  r: TRectF;
begin
  if not FItemsReadOnly then
  begin
    FEditMode := true;
    FEdit.Font.Assign(FAppearance.FEditFont);

    if FEditType = eirtName then
    begin
      FEdit.Text := FItems[FEditIndex].Name;
      r := FItems[FEditIndex].FNameRect;
    end
    else if FEditType = eirtDataString then
    begin
      FEdit.Text := FItems[FEditIndex].DataString;
      r := FItems[FEditIndex].FDataStringRect;
    end;

    FPrevText := FEdit.Text;

    FEdit.Visible := True;
    FEdit.SetFocus;

    {$IFDEF FMXLIB}
    FEdit.Position.X := r.Left;
    FEdit.Position.Y := r.Top + ((r.Bottom - r.Top) - FEdit.Height) / 2;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FEdit.Left := Round(r.Left);
    FEdit.Top := Round(r.Top + ((r.Bottom - r.Top) - FEdit.Height) / 2);
    {$ENDIF}
    FEdit.Width := Round(r.Right - r.Left);
  end;
end;

procedure TTMSFNCEditorListView.EndEditMode(const Execute: Boolean);
var
  I,c: Integer;
  s: string;
begin
  if not Execute then
    s := FPrevText
  else
    s := FEdit.Text;

  if FEditType = eirtName then
  begin
    if Execute and (FPrevText <> FEdit.Text) and MultiSelected then
    begin
      c := 1;
      FItems[FEditIndex].Name := s + IntToStr(c);

      for I := 0 to FItems.Count - 1 do
      begin
        if FItems[I].Selected and (I <> FEditIndex) and (FEditType = eirtName) then
        begin
          Inc(c);
          FItems[I].Name := s + IntToStr(c);
        end;
      end;
    end
    else
      FItems[FEditIndex].Name := s;
  end
  else if FEditType = eirtDataString then
    FItems[FEditIndex].DataString := s;

  FEditMode := False;
  FEdit.Visible := False;
  FEditIndex := -1;
end;

procedure TTMSFNCEditorListView.SetItemDataStringRect(AIndex: integer);
var
  r, imgR: TRectF;
  g: TTMSFNCGraphics;
  th, hs, h, subtract: single;
begin
  hs := Max(5, FAppearance.ItemHorizontalSpacing);
  r := FItems[AIndex].FDrawRect;

  imgR := FItems[AIndex].FImgRect;
  if imgR.Right <= 0 then
    imgR := RectF(r.Left, r.Top, r.Left, r.Bottom);

  g := TTMSFNCGraphics.Create(Self.Canvas);
  try
    g.Font.Assign(FAppearance.FItemFont);
    th := g.CalculateTextHeight('Hg');
  finally
    g.Free;
  end;

  if FItemsReadOnly then
    subtract := hs
  else
    subtract := 6 * hs;

  if DrawDataString then
  begin
    h := (imgR.Bottom - imgR.Top) / 2;
    FItems[AIndex].FDataStringRect := RectF(imgR.Right + hs, imgR.Top + h + (h - th)/2 , r.Right - subtract, imgR.Top + h +(h - th)/2 + th);
  end
  else
    FItems[AIndex].FDataStringRect := RectF(-1, -1, -1, -1);
end;

procedure TTMSFNCEditorListView.SetDefaultItemHeight(const Value: Single);
begin
  if FDefaultItemHeight <> Value then
  begin
    FDefaultItemHeight := Value;
    UpdateList;
  end;
end;

procedure TTMSFNCEditorListView.SetItemImageRect(AIndex: integer);
var
  r: TRectF;
begin
  r := FItems[AIndex].FDrawRect;
  FItems[AIndex].FImgRect := RectF(r.Left + FImgMargin, r.Top + FImgMargin, r.Left + ((r.Bottom - FImgMargin) - (r.Top + FImgMargin)), r.Bottom - FImgMargin);
end;

procedure TTMSFNCEditorListView.SetItemNameRect(AIndex: integer);
var
  r, imgR: TRectF;
  g: TTMSFNCGraphics;
  th, h, hs, subtract: single;
begin
  hs := Max(5, FAppearance.ItemHorizontalSpacing);
  r := FItems[AIndex].FDrawRect;

  imgR := FItems[AIndex].FImgRect;
  if imgR.Right < 0 then
  begin
    imgR := RectF(r.Left,r.Top,r.Left,r.Bottom);
  end;

  g := TTMSFNCGraphics.Create(Self.Canvas);
  try
    g.Font.Assign(FAppearance.FItemFont);
    th := g.CalculateTextHeight('Hg');
  finally
    g.Free;
  end;

  if DrawDataString then
    h := (imgR.Bottom - imgR.Top) / 2
  else
    h := (imgR.Bottom - imgR.Top);

  if FItemsReadOnly then
    subtract := hs
  else
    subtract := 6 * hs;

  FItems[AIndex].FNameRect := RectF(imgR.Right + hs, imgR.Top + (h - th)/2 , r.Right - subtract, imgR.Top + (h - th)/2 + th);
end;

procedure TTMSFNCEditorListView.SetItemRect(AIndex: integer; X, Y: Single);
begin
  FItems[AIndex].FDrawRect := RectF(FAppearance.ItemHorizontalSpacing, Y, Width - FAppearance.ItemHorizontalSpacing, Y + FItems[AIndex].ItemHeight);
end;

procedure TTMSFNCEditorListView.SetItems(const Value: TTMSFNCEditorListCollection);
begin
  FItems.Assign(Value);
  UpdateList;
end;

procedure TTMSFNCEditorListView.SetItemsReadOnly(const Value: Boolean);
begin
  if FItemsReadOnly <> Value then
  begin
    FItemsReadOnly := Value;
    UpdateList;
  end;
end;

procedure TTMSFNCEditorListView.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not FMultiSelect then
      UnselectAllItems;
    Repaint;
  end;
end;

procedure TTMSFNCEditorListView.UnselectAllItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].FSelected := False;
  end;
end;

procedure TTMSFNCEditorListView.UpdateControlAfterResize;
begin
  inherited;
  UpdateList;
end;

procedure TTMSFNCEditorListView.UpdateList;
begin
  if not FUpdateBlock and not FIntUpdateBlock then
  begin
    InitializeItems;

    if FItemsChanged then
    begin
      FItemsChanged := False;
      DoItemsChanged(Self);
    end;
    Repaint;
  end;
end;

{ TTMSFNCEditorListCollection }

function TTMSFNCEditorListCollection.Add: TTMSFNCEditorListItem;
begin
  Result := TTMSFNCEditorListItem(inherited Add);
end;

procedure TTMSFNCEditorListCollection.Assign(Source: TPersistent);
begin
  inherited;
  DoChanged;
end;

constructor TTMSFNCEditorListCollection.Create(AOwner: TTMSFNCEditorListView);
begin
  inherited Create(AOwner, GetEditorListItemClass);
  FOwner := AOwner;
end;

procedure TTMSFNCEditorListCollection.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TTMSFNCEditorListCollection.GetEditorListItemClass: TCollectionItemClass;
begin
  Result := TTMSFNCEditorListItem;
end;

function TTMSFNCEditorListCollection.GetItemEx(Index: Integer): TTMSFNCEditorListItem;
begin
  Result := TTMSFNCEditorListItem(inherited Items[Index]);
end;

function TTMSFNCEditorListCollection.Insert(index: Integer): TObject;
begin
  Result := TTMSFNCEditorListItem(inherited Insert(Index));
end;

procedure TTMSFNCEditorListCollection.SetItemEx(Index: Integer;
  const Value: TTMSFNCEditorListItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTMSFNCEditorListItem }

procedure TTMSFNCEditorListItem.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCEditorListItem) then
  begin
    FName := (Source as TTMSFNCEditorListItem).Name;
    FTag := (Source as TTMSFNCEditorListItem).Tag;
    FBitmap.Assign((Source as TTMSFNCEditorListItem).Bitmap);
    FDataObject := (Source as TTMSFNCEditorListItem).DataObject;
    FItemHeight := (Source as TTMSFNCEditorListItem).ItemHeight;
  end
  {$IFDEF FNCLIB}
  else if (Source is TTMSFNCURLBitmapItem) then
  begin
    FName := (Source as TTMSFNCURLBitmapItem).Name;
    FTag := (Source as TTMSFNCURLBitmapItem).Tag;
    FBitmap.Assign((Source as TTMSFNCURLBitmapItem).Bitmap);
    FDataString := (Source as TTMSFNCURLBitmapItem).URL;
  end
  else if (Source is TTMSFNCBitmapItem) then
  begin
    FName := (Source as TTMSFNCBitmapItem).Name;
    FTag := (Source as TTMSFNCBitmapItem).Tag;
    FBitmap.Assign((Source as TTMSFNCBitmapItem).Bitmap);
  end
  {$ENDIF}
  else
    inherited;
end;

procedure TTMSFNCEditorListItem.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);

  if (Collection is TTMSFNCEditorListCollection) then
  begin
    if Assigned((Collection as TTMSFNCEditorListCollection)) then
      (Collection as TTMSFNCEditorListCollection).DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.DoSelectItemChanged(AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
begin
  if {$IFNDEF LCLWEBLIB}Assigned(Collection) and {$ENDIF} (Collection is TTMSFNCEditorListCollection) and Assigned((Collection as TTMSFNCEditorListCollection).FOwner)
        and ((Collection as TTMSFNCEditorListCollection).FOwner is TTMSFNCEditorListView) then
  begin
    ((Collection as TTMSFNCEditorListCollection).FOwner as TTMSFNCEditorListView).DoSelectItemChanged(AIndex, AItem, ASelected);
  end
end;

constructor TTMSFNCEditorListItem.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TTMSFNCBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FName := '';
  FFontColor := gcNull;
  FSelectedFontColor := gcNull;

  if Assigned(Collection) and (Collection is TTMSFNCEditorListCollection) and Assigned((Collection as TTMSFNCEditorListCollection).FOwner)
        and ((Collection as TTMSFNCEditorListCollection).FOwner is TTMSFNCEditorListView) then
  begin
    FItemHeight := ((Collection as TTMSFNCEditorListCollection).FOwner as TTMSFNCEditorListView).DefaultItemHeight;
  end
  else
    FItemHeight := 50;
end;

destructor TTMSFNCEditorListItem.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TTMSFNCEditorListItem.DoBitmapChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TTMSFNCEditorListItem.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TTMSFNCEditorListItem.SetDataObject(const Value: TObject);
begin
  if FDataObject <> Value then
  begin
    FDataObject := Value;
    DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.SetDataString(const Value: string);
begin
  if FDataString <> Value then
  begin
    FDataString := Value;
    DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.SetFontColor(const Value: TTMSFNCGraphicsColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    DoChanged;
  end;
end;

procedure TTMSFNCEditorListItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    DoSelectItemChanged(Index, Self, FSelected);
  end;
end;

procedure TTMSFNCEditorListItem.SetSelectedFontColor(const Value: TTMSFNCGraphicsColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    DoChanged;
  end;
end;

{ TTMSFNCBitmapEditorListView }

procedure TTMSFNCBitmapEditorListView.DeleteSelectedItems;
begin
  inherited;
end;

{$IFDEF FMXLIB}
procedure TTMSFNCBitmapEditorListView.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;
end;

procedure TTMSFNCBitmapEditorListView.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  I: Integer;
  str: string;
  bi: TTMSFNCEditorListItem;
begin
  inherited;

  FIntUpdateBlock := True;

  for I := 0 to Length(Data.Files) - 1 do
  begin
    if IsFileSupported(Data.Files[I]) then
    begin
      bi := FItems.Add;
      str := Data.Files[I];
      bi.Name := ExtractFileName(str);
      bi.Bitmap.LoadFromFile(Data.Files[I]);
    end;
  end;

  FIntUpdateBlock := False;
  UpdateList;
end;
{$ENDIF}

function TTMSFNCBitmapEditorListView.GetSelectedItems: TTMSFNCEditorListItemArray;
begin
  Result := inherited;
end;

{$IFNDEF WEBLIB}
function TTMSFNCBitmapEditorListView.IsFileSupported(AFileName: string): Boolean;
var
  ext: string;
begin
  ext := ExtractFileExt(AFileName);
  if (ext = '.png') or (ext = '.gif') or (ext = '.jpg') or (ext = '.jpeg') or (ext = '.bmp') or (ext = '.svg')
      or (ext = '.tif') or (ext = '.tiff') or (ext = '.ico') or (ext = '.emf') or (ext = '.wmf') then
    Result := True
  else
    Result := False;
end;
{$ENDIF}

procedure TTMSFNCBitmapEditorListView.SelectItem(AIndex: Integer);
begin
  inherited;
end;

{$IFDEF CMNLIB}
{$IFDEF VCLLIB}
procedure TTMSFNCBitmapEditorListView.WMDropFiles(var Message: TMessage);
var
  FileCount,Len,i: Integer;
  FileName: array[0..255] of Char;
  str: string;
  bi: TTMSFNCEditorListItem;
begin
  FIntUpdateBlock := True;

  FileCount := DragQueryFile(Message.wParam, UINT(-1), nil, 0);
  for i := 0 to FileCount - 1 do
  begin
    Len := DragQueryFile(Message.wParam, I, FileName, 255);
    if Len > 0 then
    begin
      str := StrPas(FileName);
      if IsFileSupported(str) then
      begin
        bi := FItems.Add;
        bi.Name := ExtractFileName(str);
        bi.Bitmap.LoadFromFile(str);
      end;
    end;
  end;

  FIntUpdateBlock := False;
  UpdateList;
end;
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCBitmapEditorListView.AddDroppedFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
  str: string;
  bi: TTMSFNCEditorListItem;
begin
  for i := Low(FileNames) to High(FileNames) do
  begin
    str := FileNames[I];
    if IsFileSupported(str) then
    begin
      bi := FItems.Add;
      bi.Name := ExtractFileName(str);
      bi.Bitmap.LoadFromFile(str);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{ TTMSFNCEditorListViewAppearance }

procedure TTMSFNCEditorListViewAppearance.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCEditorListViewAppearance then
  begin
    FUseImageAppearance := (Source as TTMSFNCEditorListViewAppearance).UseImageAppearance;

    FItemRounding := (Source as TTMSFNCEditorListViewAppearance).ItemRounding;
    FItemImageRounding := (Source as TTMSFNCEditorListViewAppearance).ItemImageRounding;

    FItemVerticalSpacing := (Source as TTMSFNCEditorListViewAppearance).ItemVerticalSpacing;
    FItemHorizontalSpacing := (Source as TTMSFNCEditorListViewAppearance).ItemHorizontalSpacing;

    FStrokeSides := (Source as TTMSFNCEditorListViewAppearance).StrokeSides;

    FFill.Assign((Source as TTMSFNCEditorListViewAppearance).Fill);
    FStroke.Assign((Source as TTMSFNCEditorListViewAppearance).Stroke);
    FEditFont.Assign((Source as TTMSFNCEditorListViewAppearance).EditFont);
    FItemFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemFill);
    FItemFont.Assign((Source as TTMSFNCEditorListViewAppearance).ItemFont);
    FItemStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemStroke);
    FItemHoverFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemHoverFill);
    FItemHoverFont.Assign((Source as TTMSFNCEditorListViewAppearance).ItemHoverFont);
    FItemHoverStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemHoverStroke);
    FItemDownFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemDownFill);
    FItemDownFont.Assign((Source as TTMSFNCEditorListViewAppearance).ItemDownFont);
    FItemDownStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemDownStroke);
    FItemSelectedFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemSelectedFill);
    FItemSelectedFont.Assign((Source as TTMSFNCEditorListViewAppearance).ItemSelectedFont);
    FItemSelectedStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemSelectedStroke);

    FItemImageFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageFill);
    FItemImageStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageStroke);
    FItemImageHoverFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageHoverFill);
    FItemImageHoverStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageHoverStroke);
    FItemImageDownFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageDownFill);
    FItemImageDownStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageDownStroke);
    FItemImageSelectedFill.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageSelectedFill);
    FItemImageSelectedStroke.Assign((Source as TTMSFNCEditorListViewAppearance).ItemImageSelectedStroke);
  end
  else
    inherited;
end;

constructor TTMSFNCEditorListViewAppearance.Create(AOwner: TTMSFNCEditorListView);
begin
  FOwner := AOwner;

  FItemVerticalSpacing := 0;
  FItemHorizontalSpacing := 0;

  FItemRounding := 0;
  FItemImageRounding := 0;

  FStrokeSides := [gsLeft, gsTop, gsRight, gsBottom];

  FFill := TTMSFNCGraphicsFill.Create;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FEditFont := TTMSFNCGraphicsFont.Create;
  FItemFill := TTMSFNCGraphicsFill.Create;
  FItemFont := TTMSFNCGraphicsFont.Create;
  FItemStroke := TTMSFNCGraphicsStroke.Create;
  FItemHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FItemHoverFont := TTMSFNCGraphicsFont.Create;
  FItemHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemDownFont := TTMSFNCGraphicsFont.Create;
  FItemDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FItemSelectedFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemSelectedFont := TTMSFNCGraphicsFont.Create;
  FItemSelectedStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));

  FItemImageFill := TTMSFNCGraphicsFill.Create;
  FItemImageStroke := TTMSFNCGraphicsStroke.Create;
  FItemImageHoverFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FItemImageHoverStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemImageDownFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemImageDownStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#4edbfa'));
  FItemImageSelectedFill := TTMSFNCGraphicsFill.Create(gfkSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));
  FItemImageSelectedStroke := TTMSFNCGraphicsStroke.Create(gskSolid, TTMSFNCGraphics.HTMLToColor('#1BADF8'));

  FFill.OnChanged := DoFillChanged;
  FStroke.OnChanged := DoStrokeChanged;
  FEditFont.OnChanged := DoEditFontChanged;
  FItemFill.OnChanged := DoItemFillChanged;
  FItemFont.OnChanged := DoItemFontChanged;
  FItemStroke.OnChanged := DoItemStrokeChanged;
  FItemHoverFill.OnChanged := DoItemFillChanged;
  FItemHoverFont.OnChanged := DoItemFontChanged;
  FItemHoverStroke.OnChanged := DoItemStrokeChanged;
  FItemDownFill.OnChanged := DoItemFillChanged;
  FItemDownFont.OnChanged := DoItemFontChanged;
  FItemDownStroke.OnChanged := DoItemStrokeChanged;
  FItemSelectedFill.OnChanged := DoItemFillChanged;
  FItemSelectedFont.OnChanged := DoItemFontChanged;
  FItemSelectedStroke.OnChanged := DoItemStrokeChanged;
  FItemImageFill.OnChanged := DoItemImageFillChanged;
  FItemImageStroke.OnChanged := DoItemImageStrokeChanged;
  FItemImageHoverFill.OnChanged := DoItemImageFillChanged;
  FItemImageHoverStroke.OnChanged := DoItemImageStrokeChanged;
  FItemImageDownFill.OnChanged := DoItemImageFillChanged;
  FItemImageDownStroke.OnChanged := DoItemImageStrokeChanged;
  FItemImageSelectedFill.OnChanged := DoItemImageFillChanged;
  FItemImageSelectedStroke.OnChanged := DoItemImageStrokeChanged;
end;

destructor TTMSFNCEditorListViewAppearance.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FEditFont.Free;
  FItemFill.Free;
  FItemFont.Free;
  FItemStroke.Free;
  FItemHoverFill.Free;
  FItemHoverFont.Free;
  FItemHoverStroke.Free;
  FItemDownFill.Free;
  FItemDownFont.Free;
  FItemDownStroke.Free;
  FItemSelectedFill.Free;
  FItemSelectedFont.Free;
  FItemSelectedStroke.Free;

  FItemImageFill.Free;
  FItemImageStroke.Free;
  FItemImageHoverFill.Free;
  FItemImageHoverStroke.Free;
  FItemImageDownFill.Free;
  FItemImageDownStroke.Free;
  FItemImageSelectedFill.Free;
  FItemImageSelectedStroke.Free;
  inherited;
end;

procedure TTMSFNCEditorListViewAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.DoImageChanged(Sender: TObject);
begin
  DoChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.DoEditFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoItemFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoItemFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoItemImageFillChanged(Sender: TObject);
begin
  DoImageChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoItemImageStrokeChanged(Sender: TObject);
begin
  DoImageChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoItemStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTMSFNCEditorListViewAppearance.SetEditFont(const Value: TTMSFNCGraphicsFont);
begin
  FEditFont.Assign(Value);
  DoEditFontChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemDownFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemDownFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemDownFont(const Value: TTMSFNCGraphicsFont);
begin
  FItemDownFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemDownStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemDownStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemFont(const Value: TTMSFNCGraphicsFont);
begin
  FItemFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemHorizontalSpacing(const Value: Single);
begin
  if FItemHorizontalSpacing <> Value then
  begin
    FItemHorizontalSpacing := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorListViewAppearance.SetItemHoverFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemHoverFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemHoverFont(const Value: TTMSFNCGraphicsFont);
begin
  FItemHoverFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemHoverStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemHoverStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageDownFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemImageDownFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageDownStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemImageDownStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemImageFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageHoverFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemImageHoverFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageHoverStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemImageHoverStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageRounding(
  const Value: Integer);
begin
 if FItemImageRounding <> Value then
  begin
    FItemImageRounding := Value;
    DoImageChanged(Self);
  end;
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageSelectedFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemImageSelectedFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageSelectedStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemImageSelectedStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemImageStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemImageStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemRounding(const Value: Integer);
begin
  if FItemRounding <> Value then
  begin
    FItemRounding := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorListViewAppearance.SetItemSelectedFill(const Value: TTMSFNCGraphicsFill);
begin
  FItemSelectedFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemSelectedFont(const Value: TTMSFNCGraphicsFont);
begin
  FItemSelectedFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemSelectedStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemSelectedStroke.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FItemStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetItemVerticalSpacing(const Value: Single);
begin
  if FItemVerticalSpacing <> Value then
  begin
    FItemVerticalSpacing := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorListViewAppearance.SetStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TTMSFNCEditorListViewAppearance.SetStrokeSides(
  const Value: TTMSFNCGraphicsSides);
begin
  if FStrokeSides <> Value then
  begin
    FStrokeSides := Value;
    DoChanged(Self);
  end;
end;

procedure TTMSFNCEditorListViewAppearance.SetUseImageAppearance(const Value: Boolean);
begin
  if FUseImageAppearance <> Value then
  begin
    FUseImageAppearance := Value;
    DoChanged(Self);
  end;
end;


{ TTMSFNCEditorList }

function TTMSFNCEditorList.GetSelectedItems: TTMSFNCEditorListItemArray;
begin
  Result := inherited;
end;

procedure TTMSFNCEditorList.SelectItem(AIndex: Integer);
begin
  inherited;
end;

procedure TTMSFNCEditorList.SetItemImageRect(AIndex: integer);
begin
  FItems[AIndex].FImgRect := RectF(-1,-1,-1,-1);
end;

end.
