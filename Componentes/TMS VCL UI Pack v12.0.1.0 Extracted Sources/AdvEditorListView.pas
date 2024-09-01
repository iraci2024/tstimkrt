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

unit AdvEditorListView;

{$I TMSDEFS.INC}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF} 
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types , AdvTypes, Controls, StdCtrls, AdvCustomControl
  , AdvGraphics, AdvGraphicsTypes
  {$IFDEF FNCLIB}
  ,AdvBitmapEditor
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
  TAdvListEditorItemRectType = (eirtNone, eirtItem, eirtImage, eirtName, eirtDataString);
  TAdvEditorListView = class;

  TAdvEditorListViewAppearance = class(TPersistent)
  private
    FOwner: TAdvEditorListView;
    FItemDownStroke: TAdvGraphicsStroke;
    FItemFont: TAdvGraphicsFont;
    FItemSelectedFill: TAdvGraphicsFill;
    FItemHoverFont: TAdvGraphicsFont;
    FEditFont: TAdvGraphicsFont;
    FItemFill: TAdvGraphicsFill;
    FItemSelectedStroke: TAdvGraphicsStroke;
    FItemHoverFill: TAdvGraphicsFill;
    FItemStroke: TAdvGraphicsStroke;
    FItemDownFont: TAdvGraphicsFont;
    FItemHoverStroke: TAdvGraphicsStroke;
    FOnChanged: TNotifyEvent;
    FFill: TAdvGraphicsFill;
    FItemDownFill: TAdvGraphicsFill;
    FItemSelectedFont: TAdvGraphicsFont;
    FStroke: TAdvGraphicsStroke;
    FItemVerticalSpacing: Single;
    FItemRounding: Integer;
    FItemImageHoverStroke: TAdvGraphicsStroke;
    FItemImageDownFill: TAdvGraphicsFill;
    FItemImageDownStroke: TAdvGraphicsStroke;
    FItemImageSelectedFill: TAdvGraphicsFill;
    FItemImageFill: TAdvGraphicsFill;
    FItemImageSelectedStroke: TAdvGraphicsStroke;
    FItemImageHoverFill: TAdvGraphicsFill;
    FItemImageStroke: TAdvGraphicsStroke;
    FItemImageRounding: Integer;
    FUseImageAppearance: Boolean;
    FItemHorizontalSpacing: Single;
    FStrokeSides: TAdvGraphicsSides;
    procedure SetEditFont(const Value: TAdvGraphicsFont);
    procedure SetItemDownFill(const Value: TAdvGraphicsFill);
    procedure SetItemDownFont(const Value: TAdvGraphicsFont);
    procedure SetItemDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemFill(const Value: TAdvGraphicsFill);
    procedure SetItemFont(const Value: TAdvGraphicsFont);
    procedure SetItemHoverFill(const Value: TAdvGraphicsFill);
    procedure SetItemHoverFont(const Value: TAdvGraphicsFont);
    procedure SetItemHoverStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetItemSelectedFont(const Value: TAdvGraphicsFont);
    procedure SetItemSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemStroke(const Value: TAdvGraphicsStroke);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemVerticalSpacing(const Value: Single);
    procedure SetItemRounding(const Value: Integer);
    procedure SetItemImageDownFill(const Value: TAdvGraphicsFill);
    procedure SetItemImageDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemImageFill(const Value: TAdvGraphicsFill);
    procedure SetItemImageHoverFill(const Value: TAdvGraphicsFill);
    procedure SetItemImageHoverStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemImageSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetItemImageSelectedStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemImageStroke(const Value: TAdvGraphicsStroke);
    procedure SetItemImageRounding(const Value: Integer);
    procedure SetUseImageAppearance(const Value: Boolean);
    procedure SetItemHorizontalSpacing(const Value: Single);
    procedure SetStrokeSides(const Value: TAdvGraphicsSides);
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
    constructor Create(AOwner: TAdvEditorListView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TAdvGraphicsFill read FFill write SetItemFill;
    property EditFont: TAdvGraphicsFont read FEditFont write SetEditFont;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property ItemHoverFill: TAdvGraphicsFill read FItemHoverFill write SetItemHoverFill;
    property ItemHoverFont: TAdvGraphicsFont read FItemHoverFont write SetItemHoverFont;
    property ItemHoverStroke: TAdvGraphicsStroke read FItemHoverStroke write SetItemHoverStroke;
    property ItemDownFill: TAdvGraphicsFill read FItemDownFill write SetItemDownFill;
    property ItemDownFont: TAdvGraphicsFont read FItemDownFont write SetItemDownFont;
    property ItemDownStroke: TAdvGraphicsStroke read FItemDownStroke write SetItemDownStroke;
    property ItemSelectedFill: TAdvGraphicsFill read FItemSelectedFill write SetItemSelectedFill;
    property ItemSelectedFont: TAdvGraphicsFont read FItemSelectedFont write SetItemSelectedFont;
    property ItemSelectedStroke: TAdvGraphicsStroke read FItemSelectedStroke write SetItemSelectedStroke;
    property ItemFill: TAdvGraphicsFill read FItemFill write SetItemFill;
    property ItemFont: TAdvGraphicsFont read FItemFont write SetItemFont;
    property ItemStroke: TAdvGraphicsStroke read FItemStroke write SetItemStroke;
    property ItemImageHoverFill: TAdvGraphicsFill read FItemImageHoverFill write SetItemImageHoverFill;
    property ItemImageHoverStroke: TAdvGraphicsStroke read FItemImageHoverStroke write SetItemImageHoverStroke;
    property ItemImageDownFill: TAdvGraphicsFill read FItemImageDownFill write SetItemImageDownFill;
    property ItemImageDownStroke: TAdvGraphicsStroke read FItemImageDownStroke write SetItemImageDownStroke;
    property ItemImageSelectedFill: TAdvGraphicsFill read FItemImageSelectedFill write SetItemImageSelectedFill;
    property ItemImageSelectedStroke: TAdvGraphicsStroke read FItemImageSelectedStroke write SetItemImageSelectedStroke;
    property ItemImageFill: TAdvGraphicsFill read FItemImageFill write SetItemImageFill;
    property ItemImageStroke: TAdvGraphicsStroke read FItemImageStroke write SetItemImageStroke;
    property ItemVerticalSpacing: Single read FItemVerticalSpacing write SetItemVerticalSpacing;
    property ItemHorizontalSpacing: Single read FItemHorizontalSpacing write SetItemHorizontalSpacing;
    property ItemRounding: Integer read FItemRounding write SetItemRounding;
    property ItemImageRounding: Integer read FItemImageRounding write SetItemImageRounding;
    property UseImageAppearance: Boolean read FUseImageAppearance write SetUseImageAppearance;
    property StrokeSides: TAdvGraphicsSides read FStrokeSides write SetStrokeSides;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAdvEditorListItem = class(TCollectionItem)
  private
    FDrawRect: TRectF;
    FImgRect: TRectF;
    FNameRect: TRectF;
    FDataStringRect: TRectF;
    FBitmap: TAdvBitmap;
    FTag: NativeInt;
    FName: string;
    FDataObject: TObject;
    FDataString: string;
    FOnChanged: TNotifyEvent;
    FItemHeight: single;
    FSelected: Boolean;
    FFontColor: TAdvGraphicsColor;
    FSelectedFontColor: TAdvGraphicsColor;
    procedure SetBitmap(const Value: TAdvBitmap);
    procedure SetDataObject(const Value: TObject);
    procedure SetDataString(const Value: string);
    procedure SetName(const Value: string);
    procedure DoChanged;
    procedure DoSelectItemChanged(AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean);
    procedure SetItemHeight(const Value: single);
    procedure SetSelected(const Value: Boolean);
    procedure SetFontColor(const Value: TAdvGraphicsColor);
    procedure SetSelectedFontColor(const Value: TAdvGraphicsColor);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure Assign(Source: TPersistent); override;
    property DrawRect: TRectF read FDrawRect;
  published
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property Name: string read FName write SetName;
    property DataObject: TObject read FDataObject write SetDataObject;
    property DataString: string read FDataString write SetDataString;
    property Tag: NativeInt read FTag write FTag;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property FontColor: TAdvGraphicsColor read FFontColor write SetFontColor;
    property SelectedFontColor: TAdvGraphicsColor read FSelectedFontColor write SetSelectedFontColor;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  TAdvEditorListItemArray = Array of TAdvEditorListItem;

  TAdvEditorListCollection = class(TOwnedCollection)
  private
    FOwner: TAdvEditorListView;
    FOnChanged: TNotifyEvent;
    function GetItemEx(Index: Integer): TAdvEditorListItem;
    procedure SetItemEx(Index: Integer; const Value: TAdvEditorListItem);
  protected
      function GetEditorListItemClass: TCollectionItemClass; virtual;
      procedure DoChanged; virtual;
  public
    constructor Create(AOwner: TAdvEditorListView);
    procedure Assign(Source: TPersistent); override;
    function Add: TAdvEditorListItem;
    function Insert(index: Integer): TObject;
    property Items[Index: Integer]: TAdvEditorListItem read GetItemEx write SetItemEx; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAdvOnDoubleClickEditorListItem = procedure(Sender: TObject; AIndex: integer; AItem: TAdvEditorListItem; X, Y: Single) of object;
  TAdvOnSelectItemChanged = procedure(Sender: TObject; AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean) of object;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvEditorListView = class(TAdvCustomControl)
  private
    FItemsChanged: Boolean;
    FLastSelectedItemIndex: Integer;
    FKeyIndex: integer;
    {$IFDEF FNCLIB}
    FBitmapEditor: TAdvBitmapEditor;
    {$ENDIF}
    FEdit: TEdit;
    FEditMode: Boolean;
    FEditType: TAdvListEditorItemRectType;
    FEditIndex: Integer;
    FPrevText: string;
    FIntUpdateBlock: Boolean;
    FUpdateBlock: Boolean;
    FItems: TAdvEditorListCollection;
    FDefaultItemHeight: Single;
    FImgMargin: Single;
    FHoverIndex: Integer;
    FDragStart: TPointF;
    FMouseDown: Boolean;
    FDownIndex: Integer;
    FAddItemIndex: Integer;
    FAddItemRect: TRectF;
    FOnItemsChanged: TNotifyEvent;
    FOnDoubleClickItem: TAdvOnDoubleClickEditorListItem;
    FOnAddNewItem: TNotifyEvent;
    FOnItemSelectedChanged: TAdvOnSelectItemChanged;
    FAppearance: TAdvEditorListViewAppearance;
    FDrawDataString: Boolean;
    FItemsReadOnly: Boolean;
    FMultiSelect: Boolean;
    FCanUnselectItems: Boolean;
    function GetItems: TAdvEditorListCollection;
    procedure SetItems(const Value: TAdvEditorListCollection);
    procedure SetDefaultItemHeight(const Value: Single);
    function GetItemIndex(X, Y: Single): Integer;
    function GetItemRectType(X, Y: Single; AIndex: integer = -1): TAdvListEditorItemRectType;
    procedure AssignBitmap(AItem: TAdvEditorListItem);
    procedure SetAppearance(const Value: TAdvEditorListViewAppearance);
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
    procedure HandleMouseDown({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    procedure HandleMouseUp({%H-}Button: TAdvMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Single); override;
    {$IFDEF FMXLIB}
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    {$ENDIF}
    procedure HandleDblClick(X, Y: Single); override;
    function CreateItems: TAdvEditorListCollection; virtual;
    Procedure SetItemRect(AIndex: integer; X, Y: Single); virtual;
    Procedure SetItemImageRect(AIndex: integer); virtual;
    Procedure SetItemNameRect(AIndex: integer); virtual;
    Procedure SetItemDataStringRect(AIndex: integer); virtual;
    procedure InitializeItems; virtual;
    procedure UpdateControlAfterResize; override;
    procedure DoAppearanceChanged(Sender: TObject); virtual;
    procedure DoAddNewItem; virtual;
    procedure DoDblClickItem(AIndex: integer; AItem: TAdvEditorListItem; X, Y: Single); virtual;
    procedure DoItemsChanged(Sender: TObject); virtual;
    procedure DoSelectItemChanged(AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean); virtual;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBackground(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure DrawBitmap({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; AItem: TAdvEditorListItem); virtual;
    procedure DrawItems({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; AItemCollection: TAdvEditorListCollection); virtual;
    procedure DrawListItem({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF; AIndex:Integer; AItem: TAdvEditorListItem); virtual;
    procedure SelectItem(AIndex:Integer); virtual;
    procedure SelectItemInt(AIndex:Integer); virtual;
    function GetSelectedItems: TAdvEditorListItemArray; virtual;
    property ItemsReadOnly: Boolean read FItemsReadOnly write SetItemsReadOnly default false;
    property Appearance: TAdvEditorListViewAppearance read FAppearance write SetAppearance;
    property Items: TAdvEditorListCollection read GetItems write SetItems;
    property DefaultItemHeight: Single read FDefaultItemHeight write SetDefaultItemHeight;
    property LastSelectedItemIndex: Integer read FLastSelectedItemIndex;
    property DrawDataString: Boolean read FDrawDataString write SetDrawDataString;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default True;
    property CanUnselectItems: Boolean read FCanUnselectItems write FCanUnselectItems default True;
    property OnAddNewItem: TNotifyEvent read FOnAddNewItem write FOnAddNewItem;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnDoubleClickItem: TAdvOnDoubleClickEditorListItem read FOnDoubleClickItem write FOnDoubleClickItem;
    property OnItemSelectedChanged: TAdvOnSelectItemChanged read FOnItemSelectedChanged write FOnItemSelectedChanged;
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

  TAdvBitmapEditorListView = class(TAdvEditorListView)
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
    function GetSelectedItems: TAdvEditorListItemArray; override;
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

  TAdvEditorList = class(TAdvEditorListView)
  protected
      Procedure SetItemImageRect(AIndex: integer); override;
  public
      function GetSelectedItems: TAdvEditorListItemArray; override;
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
  TypInfo, SysUtils, AdvUtils, Math, Graphics
  {$IFNDEF LIMITEDGRAPHICSMODE}
  , PictureContainer
  {$ENDIF}
  {$IFDEF FNCLIB}
  , AdvURLPictureContainer
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Winapi.Windows, Winapi.ShellAPI
  {$ENDIF}
  ;

{ TAdvEditorListView }

procedure TAdvEditorListView.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TAdvEditorListView.AssignBitmap(AItem: TAdvEditorListItem);
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
  Invalidate;
  {$ENDIF}
end;

{$IFDEF WEBLIB}
procedure TAdvEditorListView.AssignBitmapCallback(AValue: TModalResult);
begin
  if (AValue = mrOk) and (FEditIndex >= 0) then
  begin
    FItems[FEditIndex].Bitmap.Assign(FBitmapEditor.Bitmap);
  end;

  FEditIndex := -1;

  Invalidate;
end;
{$ENDIF}

procedure TAdvEditorListView.BeginUpdate;
begin
  inherited;
  FUpdateBlock := True;
end;

procedure TAdvEditorListView.ChangeDPIScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;

  BeginUpdate;

  FAppearance.FItemFont.Height := TAdvUtils.MulDivInt(FAppearance.FItemFont.Height, M, D);
  FAppearance.FItemHoverFont.Height := TAdvUtils.MulDivInt(FAppearance.FItemHoverFont.Height, M, D);
  FAppearance.FItemDownFont.Height := TAdvUtils.MulDivInt(FAppearance.FItemDownFont.Height, M, D);
  FAppearance.FItemSelectedFont.Height := TAdvUtils.MulDivInt(FAppearance.FItemSelectedFont.Height, M, D);
  FAppearance.FItemImageRounding := TAdvUtils.MulDivInt(FAppearance.FItemImageRounding, M, D);
  FAppearance.FItemRounding := TAdvUtils.MulDivInt(FAppearance.FItemRounding, M, D);
  FAppearance.FEditFont.Height := TAdvUtils.MulDivInt(FAppearance.FEditFont.Height, M, D);
  FAppearance.FItemVerticalSpacing := TAdvUtils.MulDivSingle(FAppearance.FItemVerticalSpacing, M, D);
  FAppearance.FItemHorizontalSpacing := TAdvUtils.MulDivSingle(FAppearance.FItemHorizontalSpacing, M, D);

  FDefaultItemHeight := TAdvUtils.MulDivSingle(FDefaultItemHeight, M, D);

  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].FItemHeight := TAdvUtils.MulDivSingle(FItems[I].FItemHeight, M, D);
  end;

  EndUpdate;
end;

constructor TAdvEditorListView.Create(AOwner: TComponent);
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
  FBitmapEditor := TAdvBitmapEditor.Create(Self);
  {$ENDIF}

  FAppearance := TAdvEditorListViewAppearance.Create(Self);
  FAppearance.OnChanged := DoAppearanceChanged;

  Width := 300;
  Height := 400;
end;

function TAdvEditorListView.CreateItems: TAdvEditorListCollection;
begin
  Result := TAdvEditorListCollection.Create(Self);
end;

procedure TAdvEditorListView.DeleteSelectedItems;
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

destructor TAdvEditorListView.Destroy;
begin
  FAppearance.Free;
  FItems.Free;
  FEdit.Free;
  {$IFDEF FNCLIB}
  FBitmapEditor.Free;
  {$ENDIF}
  inherited;
end;

procedure TAdvEditorListView.DoAddNewItem;
begin
  if Assigned(OnAddNewItem) then
    OnAddNewItem(Self);
end;

procedure TAdvEditorListView.DoAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvEditorListView.DoDblClickItem(AIndex: integer; AItem: TAdvEditorListItem; X, Y: Single);
begin
  if Assigned(OnDoubleClickItem) then
    OnDoubleClickItem(Self, AIndex, AItem, X, Y);
end;

{$IFDEF FMXLIB}
procedure TAdvEditorListView.DoEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

{$ENDIF}
{$IFNDEF FMXLIB}
procedure TAdvEditorListView.DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TAdvEditorListView.DoItemsChanged(Sender: TObject);
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

procedure TAdvEditorListView.DoSelectItemChanged(AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean);
begin
  if Assigned(OnItemSelectedChanged) then
    OnItemSelectedChanged(Self, AIndex, AItem, ASelected);
end;

{$IFDEF FMXLIB}
procedure TAdvEditorListView.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  inherited;
end;

procedure TAdvEditorListView.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;
  if Length(Data.Files) > 0 then
  begin
    Operation := TDragOperation.Copy;
  end;
end;
{$ENDIF}

procedure TAdvEditorListView.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  DrawItems(AGraphics, ARect, FItems);
end;

procedure TAdvEditorListView.DrawBackground(AGraphics: TAdvGraphics; ARect: TRectF);
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

procedure TAdvEditorListView.DrawBitmap(AGraphics: TAdvGraphics; ARect: TRectF; AItem: TAdvEditorListItem);
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

procedure TAdvEditorListView.DrawItems(AGraphics: TAdvGraphics; ARect: TRectF; AItemCollection: TAdvEditorListCollection);
var
  I: Integer;
begin
  for I := 0 to AItemCollection.Count - 1 do
  begin
    DrawListItem(AGraphics, AItemCollection.Items[I].FDrawRect, I, AItemCollection.Items[I]);
  end;
end;

procedure TAdvEditorListView.DrawListItem(AGraphics: TAdvGraphics; ARect: TRectF; AIndex: Integer; AItem: TAdvEditorListItem);
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

procedure TAdvEditorListView.EndUpdate;
begin
  inherited;

  FUpdateBlock := False;
  UpdateList;
end;

function TAdvEditorListView.GetItemIndex(X, Y: Single): Integer;
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
//      if TAdvGraphics.PointInRect(PointF(X,Y), FItems[I].FDrawRect) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvEditorListView.GetItemRectType(X, Y: Single; AIndex: integer): TAdvListEditorItemRectType;
begin
  if AIndex < 0 then
    AIndex := GetItemIndex(X, Y);

  if AIndex >= 0 then
  begin
    if TAdvGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FImgRect) then
    begin
      Result := eirtImage;
    end
    else if TAdvGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FNameRect) then
    begin
      Result := eirtName;
    end
    else if TAdvGraphics.PointInRect(PointF(X,Y), FItems[AIndex].FDataStringRect) then
    begin
      Result := eirtDataString;
    end
    else
      Result := eirtItem;
  end
  else
    Result := eirtNone;
end;

function TAdvEditorListView.GetItems: TAdvEditorListCollection;
begin
  Result := FItems;
end;

function TAdvEditorListView.GetListHeight: Integer;
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

function TAdvEditorListView.GetSelectedItems: TAdvEditorListItemArray;
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

procedure TAdvEditorListView.HandleDblClick(X, Y: Single);
var
  ci: Integer;
begin
  inherited;

  ci := GetItemIndex(X, Y);

  if (ci >= 0) and Assigned(FItems[ci]) then
    DoDblClickItem(ci, FItems[ci], X, Y);
end;

procedure TAdvEditorListView.HandleKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
end;

procedure TAdvEditorListView.HandleKeyPress(var Key: Char);
begin
  inherited;

end;

procedure TAdvEditorListView.HandleKeyUp(var Key: Word; Shift: TShiftState);
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
    Invalidate;
end;

procedure TAdvEditorListView.HandleMouseDown(Button: TAdvMouseButton;
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
      Invalidate;
    end;
  end;
end;

procedure TAdvEditorListView.HandleMouseEnter;
begin
  inherited;

end;

procedure TAdvEditorListView.HandleMouseLeave;
begin
  inherited;
  FHoverIndex := -1;
  FDownIndex := -1;
  Invalidate;
end;

procedure TAdvEditorListView.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  hi: integer;
begin
  inherited;

  hi := GetItemIndex(X,Y);

  if hi <> FHoverIndex then
  begin
    FHoverIndex := hi;
    Invalidate;
  end;
end;

procedure TAdvEditorListView.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  si: integer;
  irt: TAdvListEditorItemRectType;
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

          if (Button = TAdvMouseButton.mbRight) then
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
      Invalidate;
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

procedure TAdvEditorListView.InitializeItems;
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
function TAdvEditorListView.IsFileSupported(AFileName: string): Boolean;
begin
  Result := True;
end;
{$ENDIF}

procedure TAdvEditorListView.MoveItems(X, Y: Single);
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

function TAdvEditorListView.MultiSelected: Boolean;
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

procedure TAdvEditorListView.SelectAllItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].Selected := True;
  end;

  FKeyIndex := -1;
end;

procedure TAdvEditorListView.SelectItem(AIndex: Integer);
begin
  UnselectAllItems;
  FLastSelectedItemIndex := AIndex;
  FKeyIndex := -1;
  if (AIndex >= 0) and (AIndex < FItems.Count) then
  begin        
    FItems[AIndex].Selected := True;
  end;
  
  Invalidate;
end;

procedure TAdvEditorListView.SelectItemInt(AIndex: Integer);
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
procedure TAdvEditorListView.SetAcceptDrag(Value: Boolean);
begin
  if HandleAllocated then
    DragAcceptFiles(Self.Handle, Value);
end;
{$ENDIF}

procedure TAdvEditorListView.SetDrawDataString(const Value: Boolean);
begin
  if FDrawDataString <> Value then
  begin
    FDrawDataString := Value;
    InitializeItems;
    Invalidate;
  end;
end;

procedure TAdvEditorListView.SetAppearance(const Value: TAdvEditorListViewAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TAdvEditorListView.StartEditMode;
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

procedure TAdvEditorListView.EndEditMode(const Execute: Boolean);
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

procedure TAdvEditorListView.SetItemDataStringRect(AIndex: integer);
var
  r, imgR: TRectF;
  g: TAdvGraphics;
  th, hs, h, subtract: single;
begin
  hs := Max(5, FAppearance.ItemHorizontalSpacing);
  r := FItems[AIndex].FDrawRect;

  imgR := FItems[AIndex].FImgRect;
  if imgR.Right <= 0 then
    imgR := RectF(r.Left, r.Top, r.Left, r.Bottom);

  g := TAdvGraphics.Create(Self.Canvas);
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

procedure TAdvEditorListView.SetDefaultItemHeight(const Value: Single);
begin
  if FDefaultItemHeight <> Value then
  begin
    FDefaultItemHeight := Value;
    UpdateList;
  end;
end;

procedure TAdvEditorListView.SetItemImageRect(AIndex: integer);
var
  r: TRectF;
begin
  r := FItems[AIndex].FDrawRect;
  FItems[AIndex].FImgRect := RectF(r.Left + FImgMargin, r.Top + FImgMargin, r.Left + ((r.Bottom - FImgMargin) - (r.Top + FImgMargin)), r.Bottom - FImgMargin);
end;

procedure TAdvEditorListView.SetItemNameRect(AIndex: integer);
var
  r, imgR: TRectF;
  g: TAdvGraphics;
  th, h, hs, subtract: single;
begin
  hs := Max(5, FAppearance.ItemHorizontalSpacing);
  r := FItems[AIndex].FDrawRect;

  imgR := FItems[AIndex].FImgRect;
  if imgR.Right < 0 then
  begin
    imgR := RectF(r.Left,r.Top,r.Left,r.Bottom);
  end;

  g := TAdvGraphics.Create(Self.Canvas);
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

procedure TAdvEditorListView.SetItemRect(AIndex: integer; X, Y: Single);
begin
  FItems[AIndex].FDrawRect := RectF(FAppearance.ItemHorizontalSpacing, Y, Width - FAppearance.ItemHorizontalSpacing, Y + FItems[AIndex].ItemHeight);
end;

procedure TAdvEditorListView.SetItems(const Value: TAdvEditorListCollection);
begin
  FItems.Assign(Value);
  UpdateList;
end;

procedure TAdvEditorListView.SetItemsReadOnly(const Value: Boolean);
begin
  if FItemsReadOnly <> Value then
  begin
    FItemsReadOnly := Value;
    UpdateList;
  end;
end;

procedure TAdvEditorListView.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not FMultiSelect then
      UnselectAllItems;
    Invalidate;
  end;
end;

procedure TAdvEditorListView.UnselectAllItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].FSelected := False;
  end;
end;

procedure TAdvEditorListView.UpdateControlAfterResize;
begin
  inherited;
  UpdateList;
end;

procedure TAdvEditorListView.UpdateList;
begin
  if not FUpdateBlock and not FIntUpdateBlock then
  begin
    InitializeItems;

    if FItemsChanged then
    begin
      FItemsChanged := False;
      DoItemsChanged(Self);
    end;
    Invalidate;
  end;
end;

{ TAdvEditorListCollection }

function TAdvEditorListCollection.Add: TAdvEditorListItem;
begin
  Result := TAdvEditorListItem(inherited Add);
end;

procedure TAdvEditorListCollection.Assign(Source: TPersistent);
begin
  inherited;
  DoChanged;
end;

constructor TAdvEditorListCollection.Create(AOwner: TAdvEditorListView);
begin
  inherited Create(AOwner, GetEditorListItemClass);
  FOwner := AOwner;
end;

procedure TAdvEditorListCollection.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TAdvEditorListCollection.GetEditorListItemClass: TCollectionItemClass;
begin
  Result := TAdvEditorListItem;
end;

function TAdvEditorListCollection.GetItemEx(Index: Integer): TAdvEditorListItem;
begin
  Result := TAdvEditorListItem(inherited Items[Index]);
end;

function TAdvEditorListCollection.Insert(index: Integer): TObject;
begin
  Result := TAdvEditorListItem(inherited Insert(Index));
end;

procedure TAdvEditorListCollection.SetItemEx(Index: Integer;
  const Value: TAdvEditorListItem);
begin
  inherited SetItem(Index, Value);
end;

{ TAdvEditorListItem }

procedure TAdvEditorListItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvEditorListItem) then
  begin
    FName := (Source as TAdvEditorListItem).Name;
    FTag := (Source as TAdvEditorListItem).Tag;
    FBitmap.Assign((Source as TAdvEditorListItem).Bitmap);
    FDataObject := (Source as TAdvEditorListItem).DataObject;
    FItemHeight := (Source as TAdvEditorListItem).ItemHeight;
  end
  {$IFDEF FNCLIB}
  else if (Source is TAdvURLBitmapItem) then
  begin
    FName := (Source as TAdvURLBitmapItem).Name;
    FTag := (Source as TAdvURLBitmapItem).Tag;
    FBitmap.Assign((Source as TAdvURLBitmapItem).Bitmap);
    FDataString := (Source as TAdvURLBitmapItem).URL;
  end
  else if (Source is TAdvBitmapItem) then
  begin
    FName := (Source as TAdvBitmapItem).Name;
    FTag := (Source as TAdvBitmapItem).Tag;
    FBitmap.Assign((Source as TAdvBitmapItem).Bitmap);
  end
  {$ENDIF}
  else
    inherited;
end;

procedure TAdvEditorListItem.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);

  if (Collection is TAdvEditorListCollection) then
  begin
    if Assigned((Collection as TAdvEditorListCollection)) then
      (Collection as TAdvEditorListCollection).DoChanged;
  end;
end;

procedure TAdvEditorListItem.DoSelectItemChanged(AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean);
begin
  if {$IFNDEF LCLWEBLIB}Assigned(Collection) and {$ENDIF} (Collection is TAdvEditorListCollection) and Assigned((Collection as TAdvEditorListCollection).FOwner)
        and ((Collection as TAdvEditorListCollection).FOwner is TAdvEditorListView) then
  begin
    ((Collection as TAdvEditorListCollection).FOwner as TAdvEditorListView).DoSelectItemChanged(AIndex, AItem, ASelected);
  end
end;

constructor TAdvEditorListItem.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FName := '';
  FFontColor := gcNull;
  FSelectedFontColor := gcNull;

  if Assigned(Collection) and (Collection is TAdvEditorListCollection) and Assigned((Collection as TAdvEditorListCollection).FOwner)
        and ((Collection as TAdvEditorListCollection).FOwner is TAdvEditorListView) then
  begin
    FItemHeight := ((Collection as TAdvEditorListCollection).FOwner as TAdvEditorListView).DefaultItemHeight;
  end
  else
    FItemHeight := 50;
end;

destructor TAdvEditorListItem.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TAdvEditorListItem.DoBitmapChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TAdvEditorListItem.SetBitmap(const Value: TAdvBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TAdvEditorListItem.SetDataObject(const Value: TObject);
begin
  if FDataObject <> Value then
  begin
    FDataObject := Value;
    DoChanged;
  end;
end;

procedure TAdvEditorListItem.SetDataString(const Value: string);
begin
  if FDataString <> Value then
  begin
    FDataString := Value;
    DoChanged;
  end;
end;

procedure TAdvEditorListItem.SetFontColor(const Value: TAdvGraphicsColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    DoChanged;
  end;
end;

procedure TAdvEditorListItem.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    DoChanged;
  end;
end;

procedure TAdvEditorListItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    DoChanged;
  end;
end;

procedure TAdvEditorListItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    DoSelectItemChanged(Index, Self, FSelected);
  end;
end;

procedure TAdvEditorListItem.SetSelectedFontColor(const Value: TAdvGraphicsColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    DoChanged;
  end;
end;

{ TAdvBitmapEditorListView }

procedure TAdvBitmapEditorListView.DeleteSelectedItems;
begin
  inherited;
end;

{$IFDEF FMXLIB}
procedure TAdvBitmapEditorListView.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;
end;

procedure TAdvBitmapEditorListView.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  I: Integer;
  str: string;
  bi: TAdvEditorListItem;
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

function TAdvBitmapEditorListView.GetSelectedItems: TAdvEditorListItemArray;
begin
  Result := inherited;
end;

{$IFNDEF WEBLIB}
function TAdvBitmapEditorListView.IsFileSupported(AFileName: string): Boolean;
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

procedure TAdvBitmapEditorListView.SelectItem(AIndex: Integer);
begin
  inherited;
end;

{$IFDEF CMNLIB}
{$IFDEF VCLLIB}
procedure TAdvBitmapEditorListView.WMDropFiles(var Message: TMessage);
var
  FileCount,Len,i: Integer;
  FileName: array[0..255] of Char;
  str: string;
  bi: TAdvEditorListItem;
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
procedure TAdvBitmapEditorListView.AddDroppedFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
  str: string;
  bi: TAdvEditorListItem;
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

{ TAdvEditorListViewAppearance }

procedure TAdvEditorListViewAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvEditorListViewAppearance then
  begin
    FUseImageAppearance := (Source as TAdvEditorListViewAppearance).UseImageAppearance;

    FItemRounding := (Source as TAdvEditorListViewAppearance).ItemRounding;
    FItemImageRounding := (Source as TAdvEditorListViewAppearance).ItemImageRounding;

    FItemVerticalSpacing := (Source as TAdvEditorListViewAppearance).ItemVerticalSpacing;
    FItemHorizontalSpacing := (Source as TAdvEditorListViewAppearance).ItemHorizontalSpacing;

    FStrokeSides := (Source as TAdvEditorListViewAppearance).StrokeSides;

    FFill.Assign((Source as TAdvEditorListViewAppearance).Fill);
    FStroke.Assign((Source as TAdvEditorListViewAppearance).Stroke);
    FEditFont.Assign((Source as TAdvEditorListViewAppearance).EditFont);
    FItemFill.Assign((Source as TAdvEditorListViewAppearance).ItemFill);
    FItemFont.Assign((Source as TAdvEditorListViewAppearance).ItemFont);
    FItemStroke.Assign((Source as TAdvEditorListViewAppearance).ItemStroke);
    FItemHoverFill.Assign((Source as TAdvEditorListViewAppearance).ItemHoverFill);
    FItemHoverFont.Assign((Source as TAdvEditorListViewAppearance).ItemHoverFont);
    FItemHoverStroke.Assign((Source as TAdvEditorListViewAppearance).ItemHoverStroke);
    FItemDownFill.Assign((Source as TAdvEditorListViewAppearance).ItemDownFill);
    FItemDownFont.Assign((Source as TAdvEditorListViewAppearance).ItemDownFont);
    FItemDownStroke.Assign((Source as TAdvEditorListViewAppearance).ItemDownStroke);
    FItemSelectedFill.Assign((Source as TAdvEditorListViewAppearance).ItemSelectedFill);
    FItemSelectedFont.Assign((Source as TAdvEditorListViewAppearance).ItemSelectedFont);
    FItemSelectedStroke.Assign((Source as TAdvEditorListViewAppearance).ItemSelectedStroke);

    FItemImageFill.Assign((Source as TAdvEditorListViewAppearance).ItemImageFill);
    FItemImageStroke.Assign((Source as TAdvEditorListViewAppearance).ItemImageStroke);
    FItemImageHoverFill.Assign((Source as TAdvEditorListViewAppearance).ItemImageHoverFill);
    FItemImageHoverStroke.Assign((Source as TAdvEditorListViewAppearance).ItemImageHoverStroke);
    FItemImageDownFill.Assign((Source as TAdvEditorListViewAppearance).ItemImageDownFill);
    FItemImageDownStroke.Assign((Source as TAdvEditorListViewAppearance).ItemImageDownStroke);
    FItemImageSelectedFill.Assign((Source as TAdvEditorListViewAppearance).ItemImageSelectedFill);
    FItemImageSelectedStroke.Assign((Source as TAdvEditorListViewAppearance).ItemImageSelectedStroke);
  end
  else
    inherited;
end;

constructor TAdvEditorListViewAppearance.Create(AOwner: TAdvEditorListView);
begin
  FOwner := AOwner;

  FItemVerticalSpacing := 0;
  FItemHorizontalSpacing := 0;

  FItemRounding := 0;
  FItemImageRounding := 0;

  FStrokeSides := [gsLeft, gsTop, gsRight, gsBottom];

  FFill := TAdvGraphicsFill.Create;
  FStroke := TAdvGraphicsStroke.Create;
  FEditFont := TAdvGraphicsFont.Create;
  FItemFill := TAdvGraphicsFill.Create;
  FItemFont := TAdvGraphicsFont.Create;
  FItemStroke := TAdvGraphicsStroke.Create;
  FItemHoverFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FItemHoverFont := TAdvGraphicsFont.Create;
  FItemHoverStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemDownFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemDownFont := TAdvGraphicsFont.Create;
  FItemDownStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FItemSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemSelectedFont := TAdvGraphicsFont.Create;
  FItemSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));

  FItemImageFill := TAdvGraphicsFill.Create;
  FItemImageStroke := TAdvGraphicsStroke.Create;
  FItemImageHoverFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FItemImageHoverStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemImageDownFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemImageDownStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#4edbfa'));
  FItemImageSelectedFill := TAdvGraphicsFill.Create(gfkSolid, TAdvGraphics.HTMLToColor('#1BADF8'));
  FItemImageSelectedStroke := TAdvGraphicsStroke.Create(gskSolid, TAdvGraphics.HTMLToColor('#1BADF8'));

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

destructor TAdvEditorListViewAppearance.Destroy;
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

procedure TAdvEditorListViewAppearance.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvEditorListViewAppearance.DoImageChanged(Sender: TObject);
begin
  DoChanged(Self);
end;

procedure TAdvEditorListViewAppearance.DoEditFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoItemFillChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoItemFontChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoItemImageFillChanged(Sender: TObject);
begin
  DoImageChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoItemImageStrokeChanged(Sender: TObject);
begin
  DoImageChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoItemStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.DoStrokeChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TAdvEditorListViewAppearance.SetEditFont(const Value: TAdvGraphicsFont);
begin
  FEditFont.Assign(Value);
  DoEditFontChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemDownFill(const Value: TAdvGraphicsFill);
begin
  FItemDownFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemDownFont(const Value: TAdvGraphicsFont);
begin
  FItemDownFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemDownStroke(const Value: TAdvGraphicsStroke);
begin
  FItemDownStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemFill(const Value: TAdvGraphicsFill);
begin
  FItemFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemFont(const Value: TAdvGraphicsFont);
begin
  FItemFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemHorizontalSpacing(const Value: Single);
begin
  if FItemHorizontalSpacing <> Value then
  begin
    FItemHorizontalSpacing := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorListViewAppearance.SetItemHoverFill(const Value: TAdvGraphicsFill);
begin
  FItemHoverFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemHoverFont(const Value: TAdvGraphicsFont);
begin
  FItemHoverFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemHoverStroke(const Value: TAdvGraphicsStroke);
begin
  FItemHoverStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageDownFill(const Value: TAdvGraphicsFill);
begin
  FItemImageDownFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageDownStroke(const Value: TAdvGraphicsStroke);
begin
  FItemImageDownStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageFill(const Value: TAdvGraphicsFill);
begin
  FItemImageFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageHoverFill(const Value: TAdvGraphicsFill);
begin
  FItemImageHoverFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageHoverStroke(const Value: TAdvGraphicsStroke);
begin
  FItemImageHoverStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageRounding(
  const Value: Integer);
begin
 if FItemImageRounding <> Value then
  begin
    FItemImageRounding := Value;
    DoImageChanged(Self);
  end;
end;

procedure TAdvEditorListViewAppearance.SetItemImageSelectedFill(const Value: TAdvGraphicsFill);
begin
  FItemImageSelectedFill.Assign(Value);
  DoItemImageFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  FItemImageSelectedStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemImageStroke(const Value: TAdvGraphicsStroke);
begin
  FItemImageStroke.Assign(Value);
  DoItemImageStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemRounding(const Value: Integer);
begin
  if FItemRounding <> Value then
  begin
    FItemRounding := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorListViewAppearance.SetItemSelectedFill(const Value: TAdvGraphicsFill);
begin
  FItemSelectedFill.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemSelectedFont(const Value: TAdvGraphicsFont);
begin
  FItemSelectedFont.Assign(Value);
  DoItemFontChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemSelectedStroke(const Value: TAdvGraphicsStroke);
begin
  FItemSelectedStroke.Assign(Value);
  DoItemFillChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemStroke(const Value: TAdvGraphicsStroke);
begin
  FItemStroke.Assign(Value);
  DoItemStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetItemVerticalSpacing(const Value: Single);
begin
  if FItemVerticalSpacing <> Value then
  begin
    FItemVerticalSpacing := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorListViewAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
  DoStrokeChanged(Self);
end;

procedure TAdvEditorListViewAppearance.SetStrokeSides(
  const Value: TAdvGraphicsSides);
begin
  if FStrokeSides <> Value then
  begin
    FStrokeSides := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvEditorListViewAppearance.SetUseImageAppearance(const Value: Boolean);
begin
  if FUseImageAppearance <> Value then
  begin
    FUseImageAppearance := Value;
    DoChanged(Self);
  end;
end;


{ TAdvEditorList }

function TAdvEditorList.GetSelectedItems: TAdvEditorListItemArray;
begin
  Result := inherited;
end;

procedure TAdvEditorList.SelectItem(AIndex: Integer);
begin
  inherited;
end;

procedure TAdvEditorList.SetItemImageRect(AIndex: integer);
begin
  FItems[AIndex].FImgRect := RectF(-1,-1,-1,-1);
end;

end.
