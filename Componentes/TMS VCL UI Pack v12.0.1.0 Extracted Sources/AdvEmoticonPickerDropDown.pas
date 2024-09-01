{***************************************************************************}
{ TAdvEmoticonPickerDropDown components                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2022                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvEmoticonPickerDropDown;

interface

uses
  Classes, Windows, Messages, AdvDropDown, Controls, Graphics, ExtCtrls,
  AdvStyleIF, Direct2D, WinAPI.D2D1;

type
  TEmoticonItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FImage: TPicture;
    FRect: TRect;
    FHint: string;
    procedure SetCaption(const Value: TCaption);
    procedure SetImage(const Value: TPicture);
    procedure SetImageIndex(const Value: Integer);
  protected
    procedure Changed;
    property Rect: TRect read FRect write FRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Image: TPicture read FImage write SetImage;
    property Hint: string read FHint write FHint;
  end;

  TEmoticonItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TEmoticonItem;
    procedure SetItem(Index: Integer; const Value: TEmoticonItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TEmoticonItem read GetItem write SetItem; default;
    function Add: TEmoticonItem;
    function Insert(Index: Integer): TEmoticonItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEmoticonPickerDropDown = class(TAdvCustomDropDown)
  private
    FItemSelector: TAdvCustomItemSelector;
    FColumns: Integer;
    FLayout: TItemLayout;
    FItems: TEmoticonItems;
    FItemIndex: Integer;
    FOnSelect: TNotifyEvent;
    FOnDrawSelectedImage: TOnDrawSelectedItem;
    FItemAppearance: TItemAppearance;
    FInternalCall: Boolean;
    FKeyTimer: TTimer;
    FCurSearch: string;
    FOldItemIndex: Integer;
    FOnDrawItem: TDrawItemEvent;
    FDirect2DCanvas: TDirect2DCanvas;
    FWriteTextFormat: IDWriteTextFormat;
    FSelItem: widestring;
    FSelIndex: integer;
    FSelLeft: integer;
    FSelTop: integer;
    FEmoticonSize: integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure DrawSelectedImage;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnSelectorItemSelect(Sender: TObject);
    procedure OnKeyTimerTime(Sender: TObject);
    procedure OnItemSelectorDrawItem(Sender: TObject; Canvas: TCanvas; R: TRect; Index: Integer);
    procedure OnItemSize(Sender: TObject; var ASize: TSize);
    procedure OnBeforeDrawItems(Sender: TObject; Canvas: TCanvas);
    procedure OnAfterDrawItems(Sender: TObject; Canvas: TCanvas);
    procedure SetColumns(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetLayout(const Value: TItemLayout);
    procedure SetItems(const Value: TEmoticonItems);
    procedure AssignedItemsToItemSelector;
    procedure SetSelectorProperties;
    procedure SetItemAppearance(const Value: TItemAppearance);
    function GetImageIndex: integer;
    procedure SetImageIndex(const Value: integer);
    function GetEmoticonHTML: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure SetEditRect; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;
    procedure DrawBackGround; override;
    property Items: TEmoticonItems read FItems write SetItems;
    property Images;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    property EmoticonIndex: integer read GetImageIndex write SetImageIndex;
    property EmoticonHTML: string read GetEmoticonHTML;
  published
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Layout: TItemLayout read FLayout write SetLayout default ilCaptionRight;
    property ItemAppearance: TItemAppearance read FItemAppearance write SetItemAppearance;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property Cursor default crArrow;
    property BorderColor;
    property DisabledBorder;
    property EmoticonSize: integer read FEmoticonSize write FEmoticonSize default 12;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownButtonGlyph;
    property DropDownSizeable;
    property Enabled;
    property Font;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnDrawSelectedImage: TOnDrawSelectedItem read FOnDrawSelectedImage write FOnDrawSelectedImage;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
    property UIStyle;
  end;


implementation

uses
  StdCtrls, Math, AdvHTML, Character, SysUtils;

type
  TInternalItemSelector = class(TAdvCustomItemSelector);
  TInternalItemAppearance = class(TItemAppearance);


function DrawEmoticonDirect(LCanvas: TDirect2DCanvas; TextFormat: IDWriteTextFormat; X,Y: integer; FontSize: integer; AText: string): integer;
const
  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT = $00000004;
var
  r: TD2DRectF;
begin
  r.left := x;
  r.right := x + 100;
  r.top := y;
  r.bottom := y + 100;

  LCanvas.RenderTarget.DrawText(PWideChar(AText),Length(AText), TextFormat, r, LCanvas.CreateBrush(clBlack),D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT);

  Result := 0;
end;

//------------------------------------------------------------------------------

{ TEmoticonItem }

procedure TEmoticonItem.Assign(Source: TPersistent);
begin
  if (Source is TEmoticonItem) then
  begin
    Caption := (Source as TEmoticonItem).Caption;
    ImageIndex := (Source as TEmoticonItem).ImageIndex;
    Image.Assign((Source as TEmoticonItem).Image);
    Enabled := (Source as TEmoticonItem).Enabled;
    Hint := (Source as TEmoticonItem).Hint;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TEmoticonItem.Changed;
begin
  TEmoticonItems(Collection).Change;
end;

//------------------------------------------------------------------------------

constructor TEmoticonItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FImage := TPicture.Create;
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TEmoticonItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TEmoticonItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TEmoticonItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TEmoticonItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TEmoticonItems }

function TEmoticonItems.Add: TEmoticonItem;
begin
  Result := TEmoticonItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TEmoticonItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TEmoticonItems.Create(AOwner: TPersistent);
begin
  inherited Create(TEmoticonItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TEmoticonItems.GetItem(Index: Integer): TEmoticonItem;
begin
  Result := TEmoticonItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TEmoticonItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TEmoticonItems.Insert(Index: Integer): TEmoticonItem;
begin
  Result := TEmoticonItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TEmoticonItems.SetItem(Index: Integer;
  const Value: TEmoticonItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvEmoticonPickerDropDown }

constructor TAdvEmoticonPickerDropDown.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  FItems := TEmoticonItems.Create(Self);
  FItems.OnChange := OnItemsChanged;
  FItemAppearance := TItemAppearance.Create(Self);
  FColumns := 8;
  FItemIndex := -1;
  FOldItemIndex := FItemIndex;
  FLayout := ilCaptionRight;
  AutoSize := False;
  EditorEnabled := False;
  DropDownEnabled := True;
  FCurSearch := '';
  FKeyTimer := TTimer.Create(Self);
  FKeyTimer.Enabled := False;
  FKeyTimer.Interval := 500;
  FKeyTimer.OnTimer := OnKeyTimerTime;
  FSelIndex := -1;
  Cursor := crArrow;
  DropDownWidth := 220;
  Width := 60;
  FEmoticonSize := 12;

  for i := 128512 to 128580 do
    FItems.Add.ImageIndex := i;

  for i := 129296 to 129326 do
    FItems.Add.ImageIndex := i;

  for i := 128064 to 128079 do
    FItems.Add.ImageIndex := i;

end;

//------------------------------------------------------------------------------

destructor TAdvEmoticonPickerDropDown.Destroy;
begin
  FKeyTimer.Enabled := False;
  FKeyTimer.Free;
  FItems.Free;
  FItemAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.CreateDropDownForm;
begin
  inherited;
  if not Assigned(FItemSelector) then
  begin
    FItemSelector := TAdvCustomItemSelector.Create(Self);
    FItemSelector.Parent := FDropDownForm;
    //FItemSelector.AdvDropDown := Self;
    FItemSelector.SelectorType := stImage;
    FItemSelector.Left := 0;
    FItemSelector.Top := 0;
    FItemSelector.Height := 150;
    FItemSelector.OnItemSize := OnItemSize;
    //FItemSelector.Initialize;
  end;
  FItemSelector.Color := DropDownColor;
  TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
  Control := FItemSelector;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetSelectorProperties;
begin
  if Assigned(FItemSelector) then
  begin
    FItemSelector.OnItemSelect := nil;
    FItemSelector.Columns := FColumns;
    FItemSelector.Images := Images;
    FItemSelector.ItemLayout := FLayout;
    FItemSelector.Color := DropDownColor;
    //FItemSelector.ItemAppearance.ColorStyle := SelectionColorStyle;
    FItemSelector.ItemAppearance.Assign(ItemAppearance);
    AssignedItemsToItemSelector;
    TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
    FItemSelector.ItemIndex := FItemIndex;
    FItemSelector.OnItemSelect := OnSelectorItemSelect;
    FItemSelector.ShowHint := ShowHint;
    FItemSelector.OnDrawItem := OnItemSelectorDrawItem;
    FItemSelector.OnItemSize := OnItemSize;
    FItemSelector.OnBeforeDrawItems := OnBeforeDrawItems;
    FItemSelector.OnAfterDrawItems := OnAfterDrawItems;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.BeforeDropDown;
begin
  inherited;
  SetSelectorProperties;
//  if Assigned(OnDrawItem) then

//  else
//    FItemSelector.OnDrawItem := nil;
  FOldItemIndex := ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.AssignedItemsToItemSelector;
var
  i: Integer;
begin
  if not Assigned(FItemSelector) then
    Exit;

  FItemSelector.Items.Clear;

  for i := 0 to Items.Count - 1 do
  begin
    with FItemSelector.Items.Add do
    begin
      Caption := Items[i].Caption;
      Image.Assign(Items[i].Image);
      ImageIndex := Items[i].ImageIndex;
      Hint := Items[i].Hint;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.UpdateDropDownSize;
var
  sz: TSize;
begin
  if not Assigned(FItemSelector) then
  begin
    inherited;
    Exit;
  end;

  FItemSelector.Align := alNone;
  sz := TInternalItemSelector(FItemSelector).GetItemPanelSize;
  if (DropDownWidth <= 0) then
    FItemSelector.Width := sz.cx + 4;

  if (DropDownHeight <= 0) then
    FItemSelector.Height := sz.cy + 4;

  inherited;

  if FItemSelector.VertScrollBar.IsScrollBarVisible then
  begin
    FDropDownForm.Width := FDropDownForm.Width + GetSystemMetrics(SM_CXVSCROLL);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;
  if Canceled then
  begin
    if Assigned(FItemSelector) then
      ItemIndex := FOldItemIndex;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.HandleMouseWheelDown;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if DroppedDown then
      FItemSelector.HotNext
    else
    begin
      FInternalCall := True;
      FItemSelector.SelectNext;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.HandleMouseWheelUp;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if DroppedDown then
      FItemSelector.HotPrevious
    else
    begin
      FInternalCall := True;
      FItemSelector.SelectPrevious;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.OnAfterDrawItems(Sender: TObject;
  Canvas: TCanvas);
begin
  FDirect2DCanvas.EndDraw;
  FDirect2DCanvas.Free;

  if FSelIndex >= 0 then
  begin
    FItemSelector.ItemSelectorPanel.DrawItemBackground(FSelIndex,Canvas);
    DrawEmoticon(Canvas, FSelLeft, FSelTop + 2, EmoticonSize, FSelItem);
  end;
end;

procedure TAdvEmoticonPickerDropDown.OnBeforeDrawItems(Sender: TObject;
  Canvas: TCanvas);
begin
  FDirect2DCanvas := TDirect2DCanvas.Create(Canvas, Rect(0,0,1000,1000));
  FDirect2DCanvas.BeginDraw;

  DWriteFactory.CreateTextFormat(PWideChar('Segoe UI Emoji'), nil, DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_STRETCH_NORMAL, MulDiv(EmoticonSize, 96, 72), 'en-us', FWriteTextFormat);
end;

procedure TAdvEmoticonPickerDropDown.OnHideDropDown;
begin
  inherited;
  //if Assigned(FItemSelector) then
    //FItemSelector.OnItemSelect := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.WMKeyDown(var Msg: TWMKeyDown);
var
  IsAlt, IsCtrl, NewSel: Boolean;
  i: Integer;
begin
  NewSel := False;
  if Enabled and DroppedDown then
  begin
    if (Msg.CharCode = VK_RETURN) then
    begin
      if Assigned(FItemSelector) then
      begin
        if (FItemSelector.ItemHot >= 0) then
        begin
          FItemSelector.ItemIndex := FItemSelector.ItemHot;
          NewSel := True;
        end;
      end;
    end;
  end;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);
  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  if not ((Msg.CharCode in [VK_DOWN, VK_UP]) and DroppedDown and not IsAlt) then
    inherited;

  if Enabled and not IsAlt then
  begin
    case Msg.CharCode of
      VK_UP:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          if (FItemSelector.ItemIndex < 0) then
            i := 0
          else
            i := FItemSelector.ItemIndex - Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
        end
        else
          SelectPrevious;
      end;
      VK_DOWN:
      begin
        if (DroppedDown) and Assigned(FItemSelector) and not isAlt then
        begin
          if (FItemSelector.ItemIndex < 0) then
            i := 0
          else
            i := FItemSelector.ItemIndex + Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
        end
        else
          SelectNext;
      end;
      VK_LEFT:
      begin
        if not EditorEnabled and DroppedDown then
        begin
          FInternalCall := True;
          FItemSelector.SelectPrevious;
          FInternalCall := False;
        end;
      end;
      VK_RIGHT:
      begin
        if not EditorEnabled and DroppedDown then
        begin
          FInternalCall := True;
          FItemSelector.SelectNext;
          FInternalCall := False;
        end;
      end;
      VK_HOME:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          FInternalCall := True;
          FItemSelector.SelectFirst;
          FInternalCall := False;
        end
        else
          SelectFirst;
      end;
      VK_END:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          FInternalCall := True;
          FItemSelector.SelectLast;
          FInternalCall := False;
        end
        else
          SelectLast;
      end;
      VK_PRIOR:
      begin
        if Assigned(FItemSelector) then
        begin
          if (FItemSelector.Items.Count <> Items.Count) then
            SetSelectorProperties;
          i := FItemSelector.GetVisibleItemCount;
          if (i > 0) then
          begin
            i := Max(FItemSelector.ItemIndex - i, 0);
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
          end;
        end;
      end;
      VK_NEXT:
      begin
        if Assigned(FItemSelector) then
        begin
          if (FItemSelector.Items.Count <> Items.Count) then
            SetSelectorProperties;
          i := FItemSelector.GetVisibleItemCount;
          if (i > 0) then
          begin
            i := Min(FItemSelector.ItemIndex + i, Items.Count - 1);
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
          end;
        end;
      end;
      VK_RETURN:
      begin
        if not NewSel and Assigned(FOnSelect) then
          FOnSelect(Self);
      end;
      else
      begin
        if not IsAlt and not IsCtrl and Assigned(FItemSelector) then
        begin
          FCurSearch := FCurSearch + Char(Msg.CharCode);
          FInternalCall := True;
          FItemSelector.LookupItem(FCurSearch);
          FInternalCall := False;
          FKeyTimer.Enabled := True;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.WMPaint(var Message: TWMPaint);
begin
  inherited;
  DrawSelectedImage;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.DrawBackGround;
begin
  inherited;
  DrawSelectedImage;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.DrawSelectedImage;
var
  DC: HDC;
  Canvas: TCanvas;
  R: TRect;
  ws: widestring;
  dy,th: integer;
begin
  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    R := GetEditRect;
    if Assigned(FOnDrawSelectedImage) then
      FOnDrawSelectedImage(Self, Canvas, R)
    else if (ItemIndex >= 0) then
    begin
      ws := Char.ConvertFromUtf32(Items[ItemIndex].ImageIndex);

      Canvas.Font.Size := EmoticonSize;
      th := Canvas.TextHeight('gh');

      dy := (Height - Abs(th)) div 2;
      DrawEmoticon(Canvas, R.Left + 2, R.Top + dy, EmoticonSize, ws);
    end;

    Canvas.Free;
  finally
    ReleaseDC(Handle,DC);
  end;
end;

function TAdvEmoticonPickerDropDown.GetEmoticonHTML: string;
begin
  Result := '';

  if EmoticonIndex >= 0 then
  begin
    Result := '&#'+IntToStr(EmoticonIndex)+ ';';
  end;

end;

function TAdvEmoticonPickerDropDown.GetImageIndex: integer;
begin
  if ItemIndex >= 0 then
    Result := Items[ItemIndex].ImageIndex
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetEditRect;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetSelectionColorStyle(
  const Value: TSelectionColorStyle);
begin
  inherited;

  if Assigned(FItemSelector) then
    TInternalItemAppearance(FItemSelector.ItemAppearance).ColorStyle := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SelectFirst;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SelectLast;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := Items.Count - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SelectNext;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 1) < Items.Count) then
      ItemIndex := ItemIndex + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SelectPrevious;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 1) >= 0) then
      ItemIndex := ItemIndex - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetColumns(const Value: Integer);
begin
  FColumns := Value;
end;

procedure TAdvEmoticonPickerDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  SetAppearanceStyle(ItemAppearance, AStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetItemIndex(const Value: Integer);
begin
  if (FItemIndex <> Value) and (Value < FItems.Count) then
  begin
    FItemIndex := Value;
    Invalidate;

    if Assigned(OnChange) then
      OnChange(Self);

    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetLayout(const Value: TItemLayout);
begin
  FLayout := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetItems(const Value: TEmoticonItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.OnItemsChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.OnItemSelectorDrawItem(Sender: TObject;
  Canvas: TCanvas; R: TRect; Index: Integer);
var
  ws: widestring;
begin
  ws := Char.ConvertFromUtf32(Items[Index].ImageIndex);

  if (Index = FItemSelector.ItemIndex) or (Index = FItemSelector.ItemHot) then
  begin
    DrawEmoticon(Canvas, r.Left, r.Top + 2, EmoticonSize, ws);

    FSelIndex := Index;
    FSelItem := ws;
    FSelLeft := r.Left;
    FSelTop := r.Top;
  end
  else
  begin
    DrawEmoticonDirect(FDirect2DCanvas, FWriteTextFormat, R.Left, R.Top, EmoticonSize, ws);
  end;
end;

procedure TAdvEmoticonPickerDropDown.OnItemSize(Sender: TObject;
  var ASize: TSize);
begin
  ASize.cx := EmoticonSize * 2;
  ASize.cy := EmoticonSize * 2;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.OnKeyTimerTime(Sender: TObject);
begin
  FKeyTimer.Enabled := False;
  FCurSearch := '';
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.OnSelectorItemSelect(Sender: TObject);
begin
  if Assigned(FItemSelector) then
  begin
    ItemIndex := FItemSelector.ItemIndex;
    if not FInternalCall then
      DoHideDropDown(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvEmoticonPickerDropDown.SetImageIndex(const Value: integer);
var
  i: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to Items.Count - 1 do
  begin
    if  Items[i].ImageIndex = Value then
    begin
      ItemIndex := i;
      found := true;
      break;
    end;
  end;
  if not found then
    ItemIndex := -1;
end;

procedure TAdvEmoticonPickerDropDown.SetItemAppearance(
  const Value: TItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------


end.
