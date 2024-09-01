{***************************************************************************}
{ TAdvDualListBox component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2020                                        }
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

unit AdvDualListBox;

{$R ADVDUALLISTBOX.RES}

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, AdvGroupBox;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with read-back of left & right list after moving items
  // v1.0.1.0 : New: Event OnCustomizeControls added
  //          : New: Exposed buttons & listbox controls as public properties
  // v1.0.2.0 : New: Event OnListsChanged added
  // v1.0.2.1 : Fixed : Issue with reopening projects containing TAdvDualListBox
  // v1.0.2.2 : Fixed : Issue with drag & drop between lists
  // v1.0.2.3 : Fixed : Issue with using bitmaps for button glyphs that have no disabled part
  // v1.0.2.4 : Fixed : Issue with focus handling
  // v1.1.0.0 : New : ListLeft.Caption, ListRight.Caption properties added
  // v1.1.0.1 : Fixed : Issue with MoveWithKey = true
  // v1.1.1.0 : Improved : Support for high DPI
  // v1.1.1.1 : Fixed : Issue with different initializations for high DPI
  // v1.2.0.0 : New : OnMovedLeftToRight, OnMovedRightToLeft events added
  //          : New : OnMovedLeftToRightAll, OnMovedRightToLeftAll events added
  // v2.0.0.0 : New : Top, up, down and bottom buttons added to change the order of the items in the right listbox


type
  TListBoxItemMoveEvent = procedure (Sender: TObject; Index: Integer; var Allow: Boolean) of object;
  TListBoxItemMovedEvent = procedure (Sender: TObject; Index: Integer) of object;
  TListBoxItemMoveAllEvent = procedure (Sender: TObject; var Allow: Boolean) of object;

  TDualListBox = class(TListBox)
  private
    { Private declarations }
    FBrother: TDualListBox;
    FAcceptBrotherItems, FAcceptOwnItems: Boolean;
    FMoveWithKey: Boolean;
    FMoveKey: Word;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DraggingOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDropped(Sender, Source: TObject; X, Y: Integer);
    property MoveWithKey: Boolean read FMoveWithKey write FMoveWithKey default False;
    property MoveKey: Word read FMoveKey write FMoveKey;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function NextItem(ItemIndex: Integer): Integer;
    procedure SelectNextItem;
  published
    { Published declarations }
    property Brother: TDualListBox read FBrother write FBrother;
    property AcceptBrotherItems: Boolean read FAcceptBrotherItems write FAcceptBrotherItems;
    property AcceptOwnItems: Boolean read FAcceptOwnItems write FAcceptOwnItems;
  end;

  TListBoxProp = class(TPersistent)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    FItems: TStringList;
    FColor: TColor;
    FFont: TFont;
    FSorted: Boolean;
    FMultiSelect: Boolean;
    FCaption: string;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetItems(const Value: TStringList);
    procedure SetSorted(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetCaption(const Value: string);
  protected
    { Protected declarations }
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Caption: string read FCaption write SetCaption;
    property Items: TStringList read FItems write SetItems;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWhite;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
  end;

  TVisibleButton = (vbRight, vbLeft, vbRightAll, vbLeftAll, vbTop, vbBottom, vbUp, vbDown);
  TVisibleButtons = set of TVisibleButton;

  TButtonsProp = class(TPersistent)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    FRightGlyph: TBitmap;
    FLeftAllGlyph: TBitmap;
    FVisibleButtons: TVisibleButtons;
    FButtonWidth: Integer;
    FRightAllGlyph: TBitmap;
    FLeftGlyph: TBitmap;
    FHintRight: String;
    FHintLeftAll: String;
    FHintRightAll: String;
    FHintLeft: String;
    FBottomGlyph: TBitmap;
    FDownGlyph: TBitmap;
    FUpGlyph: TBitmap;
    FTopGlyph: TBitmap;
    FHintBottom: String;
    FHintDown: String;
    FHintUp: String;
    FHintTop: String;
    procedure OnGlyphChanged(Sender: TObject);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetLeftAllGlyph(const Value: TBitmap);
    procedure SetLeftGlyph(const Value: TBitmap);
    procedure SetRightAllGlyph(const Value: TBitmap);
    procedure SetRightGlyph(const Value: TBitmap);
    procedure SetVisibleButtons(const Value: TVisibleButtons);
    procedure SetHintLeft(const Value: String);
    procedure SetHintLeftAll(const Value: String);
    procedure SetHintRight(const Value: String);
    procedure SetHintRightAll(const Value: String);
    procedure SetBottomGlyph(const Value: TBitmap);
    procedure SetDownGlyph(const Value: TBitmap);
    procedure SetTopGlyph(const Value: TBitmap);
    procedure SetUpGlyph(const Value: TBitmap);
    procedure SetHintBottom(const Value: String);
    procedure SetHintDown(const Value: String);
    procedure SetHintTop(const Value: String);
    procedure SetHintUp(const Value: String);
  protected
    { Protected declarations }
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 22;
    property VisibleButtons: TVisibleButtons read FVisibleButtons write SetVisibleButtons;
    property LeftGlyph: TBitmap read FLeftGlyph write SetLeftGlyph;
    property RightGlyph: TBitmap read FRightGlyph write SetRightGlyph;
    property LeftAllGlyph: TBitmap read FLeftAllGlyph write SetLeftAllGlyph;
    property RightAllGlyph: TBitmap read FRightAllGlyph write SetRightAllGlyph;
    property TopGlyph: TBitmap read FTopGlyph write SetTopGlyph;
    property UpGlyph: TBitmap read FUpGlyph write SetUpGlyph;
    property DownGlyph: TBitmap read FDownGlyph write SetDownGlyph;
    property BottomGlyph: TBitmap read FBottomGlyph write SetBottomGlyph;
    property HintLeft: String read FHintLeft write SetHintLeft;
    property HintLeftAll: String read FHintLeftAll write SetHintLeftAll;
    property HintRight: String read FHintRight write SetHintRight;
    property HintRightAll: String read FHintRightAll write SetHintRightAll;
    property HintUp: String read FHintUp write SetHintUp;
    property HintDown: String read FHintDown write SetHintDown;
    property HintTop: String read FHintTop write SetHintTop;
    property HintBottom: String read FHintBottom write SetHintBottom;
  end;

  //moMoveLeftRight: enables moving from left to right
  //moMoveRightLeft: enables moving from right to left
  //moMoveWithButton: enables moving through buttons
  //moMoveWithKey:  when true and pressing VK_Right key on left list, move from
  // left to right, when pressing VK_LEFT on right  list, move from right to left
  TMoveOption = (moMoveWithDblClick, moMoveWithDragDrop, moMoveWithButton, moMoveWithKey, moMoveLeftRight, moMoveRightLeft, moMoveUpDown);
  TMoveOptions = set of TMoveOption;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDualListBox = class(TAdvGroupBox)
  private
    { Private declarations }
    FArrangeButtonsVisible: Boolean;
    FBtnRightOne, FBtnLeftOne, FBtnRightAll, FBtnLeftAll: TSpeedButton;
    FLeftLabel, FRightLabel: TLabel;
    FLeftBox, FRightBox: TDualListBox;
    FMoveOptions: TMoveOptions;
    FOnLeftListClick: TNotifyEvent;
    FOnRightListClick: TNotifyEvent;
    FListLeft: TListBoxProp;
    FListRight: TListBoxProp;
    FButtons: TButtonsProp;
    FOnMoveRightLeft: TListBoxItemMoveEvent;
    FOnMoveLeftRight: TListBoxItemMoveEvent;
    FOnMoveRightLeftAll: TListBoxItemMoveAllEvent;
    FOnMoveLeftRightAll: TListBoxItemMoveAllEvent;
    FInternalLeftListChange: Boolean;
    FInternalRightListChange: Boolean;
    FOnCustomizeControls: TNotifyEvent;
    FOnListsChanged: TNotifyEvent;
    FDesignTime: boolean;
    FOnMovedRightLeft: TListBoxItemMovedEvent;
    FOnMovedLeftRight: TListBoxItemMovedEvent;
    FOnMovedRightLeftAll: TNotifyEvent;
    FOnMovedLeftRightAll: TNotifyEvent;
    FBtnTop: TSpeedButton;
    FBtnBottom: TSpeedButton;
    FBtnDown: TSpeedButton;
    FBtnUp: TSpeedButton;
    FOnMoveDown: TListBoxItemMoveEvent;
    FOnMoveUp: TListBoxItemMoveEvent;
    FOnMovedDown: TListBoxItemMovedEvent;
    FOnMovedUp: TListBoxItemMovedEvent;
    FOnMovedTop: TListBoxItemMovedEvent;
    FOnMoveBottom: TListBoxItemMoveEvent;
    FOnMoveTop: TListBoxItemMoveEvent;
    FOnMovedBottom: TListBoxItemMovedEvent;
    procedure OnListLeftChanged(Sender: TObject);
    procedure OnListRightChanged(Sender: TObject);
    procedure OnButtonsChanged(Sender: TObject);
    procedure SetListLeft(const Value: TListBoxProp);
    procedure SetListRight(const Value: TListBoxProp);
    procedure SetButtons(const Value: TButtonsProp);
    function GetItemIndexLeft: Integer;
    function GetItemIndexRight: Integer;
    procedure SetItemIndexLeft(const Value: Integer);
    procedure SetItemIndexRight(const Value: Integer);
    procedure CheckArrangeButtons;
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Resized(Sender: TObject);
    procedure ClickLeft(Sender: TObject);
    procedure ClickRight(Sender: TObject);
    procedure DblClickLeft(Sender: TObject);
    procedure DblClickRight(Sender: TObject);
    procedure LeftOneClick(Sender: TObject);
    procedure RightOneClick(Sender: TObject);
    procedure LeftAllClick(Sender: TObject);
    procedure RightAllClick(Sender: TObject);
    procedure TopClick(Sender: TObject);
    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    procedure BottomClick(Sender: TObject);

    procedure SetMoveOptions(Value: TMoveOptions);
    procedure LoadGlyphs;
    procedure UpdateEnableLeftBtn;
    procedure UpdateEnableRightBtn;
    procedure UpdateEnableButtons;
    procedure UpdateEnableUpBtn;
    procedure UpdateEnableDownBtn;
    procedure MoveSelectedItems(FromList, ToList: TDualListBox; AtIndex: Integer);  // AtIndex = -1 means Add()
    procedure DoCustomizeControls; virtual;
    procedure DoListsChanged; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MoveLeftRightAll;
    procedure MoveRightLeftAll;
    procedure MoveLeftToRight; overload; // Move Selected
    procedure MoveLeftToRight(Index: Integer); overload;
    procedure MoveRightToLeft; overload; // Move Selected
    procedure MoveRightToLeft(Index: Integer); overload;
    procedure MoveUp; overload;
    procedure MoveUp(Index: Integer); overload;
    procedure MoveTop; overload;
    procedure MoveTop(Index: Integer); overload;
    procedure MoveDown; overload;
    procedure MoveDown(Index: Integer); overload;
    procedure MoveBottom; overload;
    procedure MoveBottom(Index: Integer); overload;

    property BtnRightOne: TSpeedButton read FBtnRightOne;
    property BtnLeftOne: TSpeedButton read FBtnLeftOne;
    property BtnRightAll: TSpeedButton read FBtnRightAll;
    property BtnLeftAll: TSpeedButton read FBtnLeftAll;
    property BtnTop: TSpeedButton read FBtnTop;
    property BtnUp: TSpeedButton read FBtnUp;
    property BtnDown: TSpeedButton read FBtnDown;
    property BtnBottom: TSpeedButton read FBtnBottom;
    property LeftList: TDualListBox read FLeftBox;
    property RightList: TDualListBox read FRightBox;
    property LeftLabel: Tlabel read FLeftLabel;
    property RightLabel: TLabel read FRightLabel;

    function GetVersionNr: Integer; override;
    property ItemIndexLeft: Integer read GetItemIndexLeft write SetItemIndexLeft;
    property ItemIndexRight: Integer read GetItemIndexRight write SetItemIndexRight;
  published
    { Published declarations }
    property Buttons: TButtonsProp read FButtons write SetButtons;
    property ListLeft: TListBoxProp read FListLeft write SetListLeft;
    property ListRight: TListBoxProp read FListRight write SetListRight;
    property MoveOptions: TMoveOptions read FMoveOptions write SetMoveOptions;
    property Padding;
    //property OnLeftListSelect: TNotifyEvent read FOnLeftListSelect write FOnLeftListSelect;
    //property OnRightListSelect: TNotifyEvent read FOnRightListSelect write FOnRightListSelect;
    property OnListsChanged: TNotifyEvent read FOnListsChanged write FOnListsChanged;
    property OnLeftListClick: TNotifyEvent read FOnLeftListClick write FOnLeftListClick;
    property OnRightListClick: TNotifyEvent read FOnRightListClick write FOnRightListClick;
    property OnMoveLeftRight: TListBoxItemMoveEvent read FOnMoveLeftRight write FOnMoveLeftRight;
    property OnMoveRightLeft: TListBoxItemMoveEvent read FOnMoveRightLeft write FOnMoveRightLeft;
    property OnMovedLeftRight: TListBoxItemMovedEvent read FOnMovedLeftRight write FOnMovedLeftRight;
    property OnMovedRightLeft: TListBoxItemMovedEvent read FOnMovedRightLeft write FOnMovedRightLeft;
    property OnMoveLeftRightAll: TListBoxItemMoveAllEvent read FOnMoveLeftRightAll write FOnMoveLeftRightAll;
    property OnMoveRightLeftAll: TListBoxItemMoveAllEvent read FOnMoveRightLeftAll write FOnMoveRightLeftAll;
    property OnMovedLeftRightAll: TNotifyEvent read FOnMovedLeftRightAll write FOnMovedLeftRightAll;
    property OnMovedRightLeftAll: TNotifyEvent read FOnMovedRightLeftAll write FOnMovedRightLeftAll;
    property OnCustomizeControls: TNotifyEvent read FOnCustomizeControls write FOnCustomizeControls;
    property OnMoveUp: TListBoxItemMoveEvent read FOnMoveUp write FOnMoveUp;
    property OnMoveDown: TListBoxItemMoveEvent read FOnMoveDown write FOnMoveDown;
    property OnMovedUp: TListBoxItemMovedEvent read FOnMovedUp write FOnMovedUp;
    property OnMovedDown: TListBoxItemMovedEvent read FOnMovedDown write FOnMovedDown;
    property OnMoveTop: TListBoxItemMoveEvent read FOnMoveTop write FOnMoveTop;
    property OnMoveBottom: TListBoxItemMoveEvent read FOnMoveBottom write FOnMoveBottom;
    property OnMovedTop: TListBoxItemMovedEvent read FOnMovedTop write FOnMovedTop;
    property OnMovedBottom: TListBoxItemMovedEvent read FOnMovedBottom write FOnMovedBottom;
  end;

implementation

uses
  AdvStyleIF;

//------------------------------------------------------------------------------

{ TListBoxProp }

constructor TListBoxProp.Create;
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := OnItemsChanged;
  FFont := TFont.Create;
  FFont.OnChange := OnFontChanged;
  FColor := clWhite;
  FMultiSelect := False;
  FSorted := False;
end;

//------------------------------------------------------------------------------

destructor TListBoxProp.Destroy;
begin
  inherited;
  FItems.Free;
  FFont.Free;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.Assign(Source: TPersistent);
begin
  if (Source is TListBoxProp) then
  begin
    FItems.Assign((Source as TListBoxProp).Items);
    FFont.Assign((Source as TListBoxProp).Font);
    FSorted := (Source as TListBoxProp).Sorted;
    FMultiSelect := (Source as TListBoxProp).MultiSelect;
    Color := (Source as TListBoxProp).Color;
    Caption := (Source as TListBoxProp).Caption;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TListBoxProp.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.SetMultiSelect(const Value: Boolean);
begin
  if (FMultiSelect <> Value) then
  begin
    FMultiSelect := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.SetSorted(const Value: Boolean);
begin
  if (FSorted <> Value) then
  begin
    FSorted := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.OnFontChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.OnItemsChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TListBoxProp.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

{ TDualListBox }

constructor TDualListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DragMode := dmAutomatic;

  OnDragOver := DraggingOver;
  OnDragDrop := DragDropped;

  FMoveWithKey := False;
end;

//------------------------------------------------------------------------------

procedure TDualListBox.DraggingOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;

  if not (Source is TDualListBox) then
    Exit;

  if (Source = FBrother) and AcceptBrotherItems then
    Accept := True;

  if (Source = Self) and AcceptOwnItems then
    Accept := True;
end;

//------------------------------------------------------------------------------

procedure TDualListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDualListBox.KeyPress(var Key: Char);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDualListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDualListBox.WMKeyDown(var Message: TWMKeyDown);
var
  I, idx: Integer;
begin
  if Assigned(FBrother) and (MoveWithKey) and (Message.CharCode = MoveKey) then
  begin
    if (Parent is TAdvDualListBox) then
      TAdvDualListBox(Parent).MoveSelectedItems(Self, FBrother, -1)
    else
    begin
      if MultiSelect then
      begin
        I := 0;
        idx := ItemIndex;
        while (I <= Items.Count - 1) do
        begin
          if Selected[I] then
          begin
            FBrother.Items.AddObject(Items[I], Items.Objects[I]);
            Items.Delete(I);
          end
          else
            Inc(I);
        end;

        if (idx >= 0) then
        begin
          idx := NextItem(idx);
          if (idx >= 0) then
            ItemIndex := idx;
        end;
      end
      else  // not MultiSelect
      begin
        I := ItemIndex;
        if I < 0 then
          Exit;

        FBrother.Items.Add(Items[I]);
        SelectNextItem;
        Items.Delete(I);
      end;
    end;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

function TDualListBox.NextItem(ItemIndex: Integer): Integer;
begin
  if (ItemIndex >= Items.Count - 1) then
    Result := 0
  else
    Result := ItemIndex + 1;
  if (Result >= Items.Count) then
    Result := Items.Count - 1;

  if (Items.Count <= 0) then
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TDualListBox.SelectNextItem;
var
  i: Integer;
begin
  if MultiSelect then
    Exit;

  i := NextItem(ItemIndex);
  if (i >= 0) then
    ItemIndex := i;
end;

//------------------------------------------------------------------------------

procedure TDualListBox.DragDropped(Sender, Source: TObject; X, Y: Integer);
var
  I, AtIndex: Integer;
begin
  if not (Source is TListBox) then
    Exit;

  AtIndex := (Sender as TListBox).TopIndex + Y div (Sender as TListBox).ItemHeight;
  I := (Source as TListBox).ItemIndex;
  if I < 0 then
    Exit;

  if (Parent is TAdvDualListBox) and (Sender is TDualListBox) and (Source is TDualListBox) then
  begin
    if AtIndex >= (Sender as TListBox).Items.Count then
      TAdvDualListBox(Parent).MoveSelectedItems((Source as TDualListBox), (Sender as TDualListBox), -1)
    else
      TAdvDualListBox(Parent).MoveSelectedItems((Source as TDualListBox), (Sender as TDualListBox), AtIndex);
  end
  else
  begin
    if AtIndex >= (Sender as TListBox).Items.Count then
      (Sender as TListBox).Items.Add((Source as TListBox).Items[I])
    else
      (Sender as TListBox).Items.Insert(AtIndex, (Source as TListBox).Items[I]);

    if (Source is TDualListBox) then
      (Source as TDualListBox).SelectNextItem;
    (Source as TListBox).Items.Delete(I);
  end;
end;

//------------------------------------------------------------------------------

{ TAdvDualListBox }

constructor TAdvDualListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable, csPannable];

  Padding.Left := 4;
  Padding.Top := 4;
  Padding.Right := 4;
  Padding.Bottom := 4;

  FListLeft := TListBoxProp.Create;
  FListLeft.OnChange := OnListLeftChanged;
  FListRight := TListBoxProp.Create;
  FListRight.OnChange := OnListRightChanged;

  FButtons := TButtonsProp.Create;
  FButtons.OnChange := OnButtonsChanged;

  Width := 300;
  Height := 180;

  FMoveOptions := [moMoveWithDblClick, moMoveWithDragDrop, moMoveWithButton, moMoveLeftRight, moMoveRightLeft, moMoveWithKey, moMoveUpDown];

  FBtnRightOne := TSpeedButton.Create(Self);
  FBtnRightOne.Parent := Self;
  with FBtnRightOne do
  begin
    Caption := ''; //'>';
    Glyph.Assign(FButtons.RightGlyph);
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Flat := True;
    Enabled := False;
    Hint := Buttons.HintRight;
    OnClick := RightOneClick;
  end;

  FBtnLeftOne := TSpeedButton.Create(Self);
  FBtnLeftOne.Parent := Self;
  with FBtnLeftOne do
  begin
    Parent := Self;
    Caption := ''; //'<';
    Glyph.Assign(FButtons.LeftGlyph);
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Flat := True;
    Enabled := False;
    Hint := Buttons.HintLeft;
    OnClick := LeftOneClick;
  end;

  FBtnRightAll := TSpeedButton.Create(Self);
  FBtnRightAll.Parent := Self;
  with FBtnRightAll do
  begin
    Caption := ''; //'>>';
    Glyph.Assign(FButtons.RightAllGlyph);
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Flat := True;
    Enabled := False;
    Hint := Buttons.HintRightAll;
    OnClick := RightAllClick;
    //Enabled := (moMoveWithButton in MoveOptions) and (moMoveLeftRight in MoveOptions);
  end;

  FBtnLeftAll := TSpeedButton.Create(Self);
  FBtnLeftAll.Parent := Self;
  with FBtnLeftAll do
  begin
    Caption := ''; //'<<';
    Glyph.Assign(FButtons.LeftAllGlyph);
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Flat := True;
    Enabled := False;
    Hint := Buttons.HintLeftAll;
    OnClick := LeftAllClick;
    //Enabled := (moMoveWithButton in MoveOptions) and (moMoveRightLeft in MoveOptions);
  end;

  FLeftBox := TDualListBox.Create(Self);
  FLeftBox.Parent := Self;
  with FLeftBox do
  begin
    Align := alLeft;
    Ctl3D := True;
    AcceptBrotherItems := True;
    MoveWithKey := (moMoveWithKey in MoveOptions);
    MoveKey := VK_RIGHT;
    OnClick := ClickLeft;
    OnDblClick := DblClickLeft;
    TabStop := true;
    TabOrder := 0;
  end;

  FRightBox := TDualListBox.Create(Self);
  FRightBox.Parent := Self;
  with FRightBox do
  begin
    Align := alRight;
    Ctl3D := True;
    AcceptBrotherItems := True;
    MoveWithKey := (moMoveWithKey in MoveOptions);
    MoveKey := VK_LEFT;
    OnClick := ClickRight;
    OnDblClick := DblClickRight;
    TabStop := true;
    TabOrder := 1;
  end;

  FArrangeButtonsVisible := False;

  FLeftBox.Brother := FRightBox;
  FRightBox.Brother := FLeftBox;

  FLeftLabel := TLabel.Create(Self);
  FLeftLabel.Parent := Self;
  FLeftLabel.Transparent := true;
  FLeftLabel.Caption := '';
  FLeftLabel.AutoSize := true;

  FRightLabel := TLabel.Create(Self);
  FRightLabel.Parent := Self;
  FRightLabel.Transparent := true;
  FRightLabel.Caption := '';
  FRightLabel.AutoSize := true;

  FRightBox.Brother := FLeftBox;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

//------------------------------------------------------------------------------

destructor TAdvDualListBox.Destroy;
begin
  FBtnRightOne.Free;
  FBtnRightAll.Free;
  FBtnLeftOne.Free;
  FBtnLeftAll.Free;
  if Assigned(FBtnTop) then
    FBtnTop.Free;
  if Assigned(FBtnUp) then
    FBtnUp.Free;
  if Assigned(FBtnDown) then
    FBtnDown.Free;
  if Assigned(FBtnBottom) then
    FBtnBottom.Free;

  FListLeft.Free;
  FListRight.Free;
  FButtons.Free;
  inherited;
end;

procedure TAdvDualListBox.DoCustomizeControls;
begin
  if Assigned(OnCustomizeControls) then
    OnCustomizeControls(Self);
end;

procedure TAdvDualListBox.DoEnter;
begin
  inherited;
  FLeftBox.SetFocus;
end;

procedure TAdvDualListBox.DoListsChanged;
begin
  if Assigned(OnListsChanged) then
    OnListsChanged(self);
end;

//------------------------------------------------------------------------------

function TAdvDualListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.KeyPress(var Key: Char);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetListLeft(const Value: TListBoxProp);
begin
  FListLeft.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetListRight(const Value: TListBoxProp);
begin
  FListRight.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.OnListLeftChanged(Sender: TObject);
var
  b,res: boolean;
  FDPIScale: single;
begin
  if FInternalLeftListChange then
    Exit;

  res := FLeftLabel.Caption <> FListLeft.Caption;

  FDPIScale := GetDPIScale(Self, Canvas);

  FLeftLabel.Caption := FListLeft.Caption;
  FLeftBox.Color := FListLeft.Color;
  FLeftBox.Items.Assign(FListLeft.Items);

  FLeftBox.Font.Assign(FListLeft.Font);

  FLeftBox.Font.Height := Round(FDPIScale * FListLeft.Font.Height);

  FLeftBox.ItemHeight :=  FLeftBox.ItemHeight;

  b := FLeftBox.Sorted <> FListLeft.Sorted;
  FLeftBox.Sorted := FListLeft.Sorted;
  FLeftBox.MultiSelect := FListLeft.MultiSelect;

  UpdateEnableRightBtn;

  if res then
    Resized(Self);

  if b then
    FListLeft.Items.Assign(FLeftBox.Items);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.OnListRightChanged(Sender: TObject);
var
  res: boolean;
  FDPIScale: single;
begin
  if FInternalRightListChange then
    Exit;

  res := FRightLabel.Caption <> FListRight.Caption;

  FDPIScale := GetDPIScale(Self, Canvas);

  FRightLabel.Caption := FListRight.Caption;
  FRightBox.Color := FListRight.Color;
  FRightBox.Items.Assign(FListRight.Items);
  FRightBox.Font.Assign(FListRight.Font);

  FRightBox.Font.Height := Round(FDPIScale * FListRight.Font.Height);

  FRightBox.ItemHeight := FRightBox.ItemHeight;

  FRightBox.Repaint;

  FRightBox.Sorted := FListRight.Sorted;
  FRightBox.MultiSelect := FListRight.MultiSelect;

  UpdateEnableLeftBtn;
  UpdateEnableUpBtn;
  UpdateEnableDownBtn;

  if res then
    Resized(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetMoveOptions(Value: TMoveOptions);
begin
  if (FMoveOptions <> Value) then
  begin
    FMoveOptions := Value;

    FBtnLeftOne.Enabled := (moMoveWithButton in MoveOptions) and FBtnLeftOne.Enabled and (moMoveRightLeft in MoveOptions) and (FRightBox.ItemIndex >= 0);
    FBtnLeftAll.Enabled := (moMoveWithButton in MoveOptions) and (moMoveRightLeft in MoveOptions) and (FRightBox.Count > 0);
    FBtnRightOne.Enabled := (moMoveWithButton in MoveOptions) and FBtnRightOne.Enabled and (moMoveLeftRight in MoveOptions) and (FLeftBox.ItemIndex >= 0);
    FBtnRightAll.Enabled := (moMoveWithButton in MoveOptions) and (moMoveLeftRight in MoveOptions) and (FLeftBox.Count > 0);

    if FArrangeButtonsVisible then
    begin
      FBtnTop.Enabled := (moMoveWithButton in MoveOptions) and FBtnTop.Enabled and (moMoveUpDown in MoveOptions) and (FRightBox.ItemIndex > 0) and not FRightBox.Sorted;
      FBtnUp.Enabled := (moMoveWithButton in MoveOptions) and FBtnUp.Enabled and (moMoveUpDown in MoveOptions) and (FRightBox.ItemIndex > 0) and not FRightBox.Sorted;
      FBtnDown.Enabled := (moMoveWithButton in MoveOptions) and FBtnDown.Enabled and (moMoveUpDown in MoveOptions) and (FRightBox.ItemIndex >= 0) and ( FRightBox.ItemIndex < FRightBox.Items.Count - 1) and not FRightBox.Sorted;
      FBtnBottom.Enabled := (moMoveWithButton in MoveOptions) and FBtnBottom.Enabled and (moMoveUpDown in MoveOptions) and (FRightBox.ItemIndex >= 0) and ( FRightBox.ItemIndex < FRightBox.Items.Count - 1) and not FRightBox.Sorted;
    end;

    FLeftBox.MoveWithKey := (moMoveWithKey in MoveOptions);
    FRightBox.MoveWithKey := (moMoveWithKey in MoveOptions);

    if moMoveWithDragDrop in MoveOptions then
    begin
      if (moMoveLeftRight in MoveOptions) then
        FLeftBox.DragMode := dmAutomatic
      else
        FLeftBox.DragMode := dmManual;
      if (moMoveRightLeft in MoveOptions) then
        FRightBox.DragMode := dmAutomatic
      else
        FRightBox.DragMode := dmManual;
    end else
    begin
      FLeftBox.DragMode := dmManual;
      FRightBox.DragMode := dmManual;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if FDesignTime then
    LoadGlyphs;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.TopClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  MoveTop;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  MoveUp;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.DownClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  MoveDown;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.BottomClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  MoveBottom;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.OnButtonsChanged(Sender: TObject);
begin
  CheckArrangeButtons;
  Resized(Self);
  LoadGlyphs;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetButtons(const Value: TButtonsProp);
begin
  FButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvDualListBox.GetItemIndexLeft: Integer;
begin
  Result := FLeftBox.ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetItemIndexLeft(const Value: Integer);
begin
  FLeftBox.ItemIndex := Value;
  UpdateEnableRightBtn;
end;

//------------------------------------------------------------------------------

function TAdvDualListBox.GetItemIndexRight: Integer;
begin
  Result := FRightBox.ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.SetItemIndexRight(const Value: Integer);
begin
  FRightBox.ItemIndex := Value;
  UpdateEnableLeftBtn;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;
  Resized(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.Resize;
begin
  inherited;
  Resized(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.Resized(Sender: TObject);
var
  ListBoxWidth, SpaceWidth,lh: Integer;
  CR: TRect;
begin
  SpaceWidth := 5;

  if FArrangeButtonsVisible then
    ListBoxWidth := (ClientWidth - Padding.Left - Padding.Right - FButtons.ButtonWidth * 2 - SpaceWidth * 4) div 2
  else
    ListBoxWidth := (ClientWidth - Padding.Left - Padding.Right - FButtons.ButtonWidth - SpaceWidth * 2) div 2;

  lh := FLeftLabel.Height;
  if FLeftLabel.Caption = '' then
    lh := 0;

  CR := ClientRect;
  AdjustClientRect(CR);
  FLeftBox.Width := ListBoxWidth;
  FLeftBox.Left := Padding.Left;
  FLeftBox.Top := CR.Top + Padding.Top + lh;
  FLeftBox.Height := (CR.Bottom - CR.Top) - Padding.Top - Padding.Bottom - lh;

  lh := FRightLabel.Height;
  if FRightLabel.Caption = '' then
    lh := 0;

  FRightBox.Width := ListBoxWidth;
  if FArrangeButtonsVisible then
    FRightBox.Left := Width - FButtons.ButtonWidth - SpaceWidth * 2 - Padding.Right - FRightBox.Width
  else
    FRightBox.Left := Width - Padding.Right - FRightBox.Width;
  FRightBox.Top := CR.Top + Padding.Top + lh;
  FRightBox.Height := (CR.Bottom - CR.Top) - Padding.Top - Padding.Bottom - lh;

  FLeftLabel.Top := CR.Top + Padding.Top;
  FLeftLabel.Left := FLeftBox.Left;

  FRightLabel.Top := CR.Top + Padding.Top;
  FRightLabel.Left := FRightBox.Left;

  with FBtnRightOne do
  begin
    Left := ListBoxWidth + SpaceWidth + Padding.Left;
    Top := SpaceWidth * 3 + Padding.Top + lh;
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Hint := Buttons.HintRight;
    Visible := vbRight in Buttons.VisibleButtons;
  end;

  with FBtnLeftOne do
  begin
    Left := ListBoxWidth + SpaceWidth + Padding.Left;
    Top := FBtnRightOne.Top + FBtnRightOne.Height + SpaceWidth;
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Hint := Buttons.HintLeft;
    Visible := vbLeft in Buttons.VisibleButtons;
  end;

  with FBtnRightAll do
  begin
    Left := ListBoxWidth + SpaceWidth + Padding.Left;
    Top := FBtnLeftOne.Top + FBtnLeftOne.Height + SpaceWidth * 2;
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Hint := Buttons.HintRightAll;
    Visible := vbRightAll in Buttons.VisibleButtons;
  end;

  with FBtnLeftAll do
  begin
    Left := ListBoxWidth + SpaceWidth + Padding.Left;
    Top := FBtnRightAll.Top + FBtnRightAll.Height + SpaceWidth;
    Width := FButtons.ButtonWidth;
    Height := FButtons.ButtonWidth;
    Hint := Buttons.HintLeftAll;
    Visible := vbLeftAll in Buttons.VisibleButtons;
  end;

  if FArrangeButtonsVisible then
  begin
    FBtnTop.Left := Width - Padding.Right - SpaceWidth - FButtons.ButtonWidth;
    FBtnTop.Top := FBtnRightOne.Top;
    FBtnTop.Width := FButtons.ButtonWidth;
    FBtnTop.Height := FButtons.ButtonWidth;
    FBtnTop.Hint := Buttons.HintTop;
    FBtnTop.Visible := vbTop in Buttons.VisibleButtons;

    FBtnUp.Left := FBtnTop.Left;
    FBtnUp.Top := FBtnLeftOne.Top; //.Top + FBtnTop.Height + SpaceWidth * 2;
    FBtnUp.Width := FButtons.ButtonWidth;
    FBtnUp.Height := FButtons.ButtonWidth;
    FBtnUp.Hint := Buttons.HintUp;
    FBtnUp.Visible := vbUp in Buttons.VisibleButtons;

    FBtnDown.Left := FBtnTop.Left;
    FBtnDown.Top := FBtnRightAll.Top;
    FBtnDown.Width := FButtons.ButtonWidth;
    FBtnDown.Height := FButtons.ButtonWidth;
    FBtnDown.Hint := Buttons.HintDown;
    FBtnDown.Visible := vbDown in Buttons.VisibleButtons;

    FBtnBottom.Left := FBtnTop.Left;
    FBtnBottom.Top := FBtnLeftAll.Top;
    FBtnBottom.Width := FButtons.ButtonWidth;
    FBtnBottom.Height := FButtons.ButtonWidth;
    FBtnBottom.Hint := Buttons.HintBottom;
    FBtnBottom.Visible := vbBottom in Buttons.VisibleButtons;
  end;

  DoCustomizeControls;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.CheckArrangeButtons;
begin
  if (vbTop in Buttons.VisibleButtons) or (vbUp in Buttons.VisibleButtons) or (vbDown in Buttons.VisibleButtons) or (vbBottom in Buttons.VisibleButtons) then
  begin
    if not Assigned(FBtnTop) then
    begin
      FBtnTop := TSpeedButton.Create(Self);
      FBtnTop.Parent := Self;
      FBtnTop.Caption := ''; //'>';
      FBtnTop.Glyph.Assign(FButtons.TopGlyph);
      FBtnTop.Width := FButtons.ButtonWidth;
      FBtnTop.Height := FButtons.ButtonWidth;
      FBtnTop.Flat := True;
      FBtnTop.Enabled := False;
      FBtnTop.Hint := Buttons.HintTop;
      FBtnTop.OnClick := TopClick;
    end;

    if not Assigned(FBtnUp) then
    begin
      FBtnUp := TSpeedButton.Create(Self);
      FBtnUp.Parent := Self;
      FBtnUp.Caption := ''; //'>';
      FBtnUp.Glyph.Assign(FButtons.UpGlyph);
      FBtnUp.Width := FButtons.ButtonWidth;
      FBtnUp.Height := FButtons.ButtonWidth;
      FBtnUp.Flat := True;
      FBtnUp.Enabled := False;
      FBtnUp.Hint := Buttons.HintUp;
      FBtnUp.OnClick := UpClick;
    end;

    if not Assigned(FBtnDown) then
    begin
      FBtnDown := TSpeedButton.Create(Self);
      FBtnDown.Parent := Self;
      FBtnDown.Caption := ''; //'>';
      FBtnDown.Glyph.Assign(FButtons.DownGlyph);
      FBtnDown.Width := FButtons.ButtonWidth;
      FBtnDown.Height := FButtons.ButtonWidth;
      FBtnDown.Flat := True;
      FBtnDown.Enabled := False;
      FBtnDown.Hint := Buttons.HintTop;
      FBtnDown.OnClick := DownClick;
    end;

    if not Assigned(FBtnBottom) then
    begin
      FBtnBottom := TSpeedButton.Create(Self);
      FBtnBottom.Parent := Self;
      FBtnBottom.Caption := ''; //'>';
      FBtnBottom.Glyph.Assign(FButtons.BottomGlyph);
      FBtnBottom.Width := FButtons.ButtonWidth;
      FBtnBottom.Height := FButtons.ButtonWidth;
      FBtnBottom.Flat := True;
      FBtnBottom.Enabled := False;
      FBtnBottom.Hint := Buttons.HintTop;
      FBtnBottom.OnClick := BottomClick;
    end;

    FArrangeButtonsVisible := True;
  end
  else
    FArrangeButtonsVisible := False;
end;

procedure TAdvDualListBox.ClickLeft(Sender: TObject);
begin
  UpdateEnableRightBtn;
  if Assigned(FOnLeftListClick) then
    FOnLeftListClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpdateEnableRightBtn;
var
  I: Integer;
begin
  I := FLeftBox.ItemIndex;
  FBtnRightOne.Enabled := (I >= 0) and (moMoveLeftRight in MoveOptions) and (moMoveWithButton in MoveOptions);

  FBtnRightAll.Enabled := (moMoveWithButton in MoveOptions) and (moMoveLeftRight in MoveOptions) and (FLeftBox.Count > 0);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpdateEnableUpBtn;
var
  I: Integer;
begin
  I := FRightBox.ItemIndex;
  if Assigned(FBtnTop) then
    FBtnTop.Enabled := (I > 0) and (moMoveUpDown in MoveOptions) and (moMoveWithButton in MoveOptions) and not FRightBox.Sorted;
  if Assigned(FBtnUp) then
    FBtnUp.Enabled := (I > 0) and (moMoveUpDown in MoveOptions) and (moMoveWithButton in MoveOptions) and not FRightBox.Sorted;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpdateEnableDownBtn;
var
  I: Integer;
begin
  I := FRightBox.ItemIndex;
  if Assigned(FBtnDown) then
    FBtnDown.Enabled := (I < FRightBox.Count - 1) and (moMoveUpDown in MoveOptions) and (moMoveWithButton in MoveOptions) and not FRightBox.Sorted;
  if Assigned(FBtnBottom) then
    FBtnBottom.Enabled := (I < FRightBox.Count - 1) and (moMoveUpDown in MoveOptions) and (moMoveWithButton in MoveOptions) and not FRightBox.Sorted;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.ClickRight(Sender: TObject);
begin
  UpdateEnableLeftBtn;
  UpdateEnableUpBtn;
  UpdateEnableDownBtn;
  if Assigned(FOnRightListClick) then
    FOnRightListClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpdateEnableLeftBtn;
var
  I: Integer;
begin
  I := FRightBox.ItemIndex;
  FBtnLeftOne.Enabled := (I >= 0) and (moMoveRightLeft in MoveOptions) and (moMoveWithButton in MoveOptions);

  FBtnLeftAll.Enabled := (moMoveWithButton in MoveOptions) and (moMoveRightLeft in MoveOptions) and (FRightBox.Count > 0);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.UpdateEnableButtons;
begin
  UpdateEnableLeftBtn;
  UpdateEnableRightBtn;
  UpdateEnableUpBtn;
  UpdateEnableDownBtn;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.DblClickLeft(Sender: TObject);
begin
  if not (moMoveWithDblClick in MoveOptions) then
    Exit;
  RightOneClick(nil);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.DblClickRight(Sender: TObject);
begin
  if not (moMoveWithDblClick in MoveOptions) then
    Exit;
  LeftOneClick(nil);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.LeftOneClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveRightLeft in MoveOptions) then
    Exit;

  MoveRightToLeft
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveRightToLeft;
var
  I, idx: Integer;
  allow: Boolean;
begin
  if not (moMoveRightLeft in MoveOptions) then
    Exit;

  if FRightBox.MultiSelect then
  begin
    I := 0;
    idx := FRightBox.ItemIndex;
    while (I <= FRightBox.Items.Count - 1) do
    begin
      if FRightBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveRightLeft) then
          FOnMoveRightLeft(Self, I, allow);

        if not allow then
        begin
          Inc(I);
          Continue;
        end;

        FLeftBox.Items.AddObject(FRightBox.Items[I], FRightBox.Items.Objects[I]);
        FRightBox.Items.Delete(I);

        if Assigned(FOnMovedRightLeft) then
          FOnMovedRightLeft(Self, I);
      end
      else
        Inc(I);
    end;

    if (idx >= 0) then
    begin
      if (idx >= FRightBox.Items.Count) then
        idx := FRightBox.Items.Count - 1;
      FRightBox.ItemIndex := idx;
      FRightBox.Selected[idx] := True;
    end;

    FInternalRightListChange := True;
    FListRight.Items.Assign(FRightBox.Items);
    FInternalRightListChange := False;

    FInternalLeftListChange := True;
    FListLeft.Items.Assign(FLeftBox.Items);
    FInternalLeftListChange := False;

    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveRightToLeft(FRightBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveRightToLeft(Index: Integer);
var
  allow: Boolean;
  idx: Integer;
begin
  if not (moMoveRightLeft in MoveOptions) then
    Exit;

  if (Index >= 0) and (Index < FRightBox.Items.Count) then
  begin
    allow := True;
    if Assigned(FOnMoveRightLeft) then
      FOnMoveRightLeft(Self, Index, allow);

    if not allow then
      Exit;

    idx := FRightBox.ItemIndex;

    FLeftBox.Items.AddObject(FRightBox.Items[Index], FRightBox.Items.Objects[Index]);
    FRightBox.Items.Delete(Index);
    if (Index >= 0) and (Index < FListRight.Items.Count) then
    begin
      FInternalRightListChange := True;
      FListRight.Items.Assign(FRightBox.Items);
      FInternalRightListChange := False;
    end;

    FInternalLeftListChange := True;
    FListLeft.Items.Assign(FLeftBox.Items);
    FInternalLeftListChange := False;

    if (idx >= 0) then
    begin
      if (idx >= FRightBox.Items.Count) then
        idx := FRightBox.Items.Count - 1;
      FRightBox.ItemIndex := idx;
    end;

    if Assigned(FOnMovedRightLeft) then
      FOnMovedRightLeft(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.RightOneClick(Sender: TObject);
begin
  if (not (moMoveWithButton in MoveOptions)) and (Sender <> nil) or not (moMoveLeftRight in MoveOptions) then
    Exit;

  MoveLeftToRight;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveLeftToRight;
var
  I, idx: Integer;
  allow: Boolean;
begin
  if not (moMoveLeftRight in MoveOptions) then
    Exit;

  if FLeftBox.MultiSelect then
  begin
    I := 0;
    idx := FLeftBox.ItemIndex;
    while (I <= FLeftBox.Items.Count - 1) do
    begin
      if FLeftBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveLeftRight) then
          FOnMoveLeftRight(Self, I, allow);

        if not allow then
        begin
          Inc(I);
          Continue;
        end;

        FRightBox.Items.AddObject(FLeftBox.Items[I], FLeftBox.Items.Objects[I]);
        FLeftBox.Items.Delete(I);

        if Assigned(FOnMovedLeftRight) then
          FOnMovedLeftRight(Self, I);
      end
      else
        Inc(I);
    end;

    if (idx >= 0) then
    begin
      if (idx >= FLeftBox.Items.Count) then
        idx := FLeftBox.Items.Count - 1;
      FLeftBox.ItemIndex := idx;
      FLeftBox.Selected[idx] := True;
    end;

    FInternalLeftListChange := True;
    FListLeft.Items.Assign(FLeftBox.Items);
    FInternalLeftListChange := False;

    FInternalRightListChange := True;
    FListRight.Items.Assign(FRightBox.Items);
    FInternalRightListChange := False;

    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveLeftToRight(FLeftBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveLeftToRight(Index: Integer);
var
  allow: Boolean;
  idx: Integer;
begin
  if not (moMoveLeftRight in MoveOptions) then
    Exit;

  if (Index >= 0) and (Index < FLeftBox.Items.Count) then
  begin
    allow := True;
    if Assigned(FOnMoveLeftRight) then
      FOnMoveLeftRight(Self, Index, allow);

    if not allow then
      Exit;

    idx := FLeftBox.ItemIndex;
    FRightBox.Items.AddObject(FLeftBox.Items[Index], FLeftBox.Items.Objects[Index]);
    FLeftBox.Items.Delete(Index);

    if (Index >= 0) and (Index < FListLeft.Items.Count) then
    begin
      FInternalLeftListChange := True;
      FListLeft.Items.Assign(FLeftBox.Items);
      FInternalLeftListChange := False;
    end;

    FInternalRightListChange := True;
    FListRight.Items.Assign(FRightBox.Items);
    FInternalRightListChange := False;

    if (idx >= 0) then
    begin
      if (idx >= FLeftBox.Items.Count) then
        idx := FLeftBox.Items.Count - 1;
      FLeftBox.ItemIndex := idx;
    end;

    if Assigned(FOnMovedLeftRight) then
      FOnMovedLeftRight(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.LeftAllClick(Sender: TObject);
begin
  if not (moMoveWithButton in MoveOptions) or not (moMoveRightLeft in MoveOptions) then
    Exit;
  MoveRightLeftAll;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveRightLeftAll;
var
  allow: Boolean;
begin
  if not (moMoveRightLeft in MoveOptions) then
    Exit;

  allow := True;
  if Assigned(OnMoveRightLeftAll) then
    FOnMoveRightLeftAll(Self, allow);

  if not allow then
    Exit;

  FLeftBox.Items.AddStrings(FRightBox.Items);
  FInternalLeftListChange := True;
  FListLeft.Items.Assign(FLeftBox.Items);
  FInternalLeftListChange := False;

  FRightBox.Items.Clear;
  FInternalRightListChange := True;
  FListRight.Items.Clear;
  FInternalRightListChange := False;

  if Assigned(OnMovedRightLeftAll) then
    FOnMovedRightLeftAll(Self);

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.RightAllClick(Sender: TObject);
begin
  if not (moMoveWithButton in MoveOptions) or not (moMoveLeftRight in MoveOptions) then
    Exit;

  MoveLeftRightAll;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveBottom;
var
  I, idx: Integer;
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if FRightBox.MultiSelect then
  begin
    I := FRightBox.Items.Count - 1;
    idx := I;
    while (I >= 0) do
    begin
      if FRightBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveBottom) then
          FOnMoveBottom(Self, I, allow);

        if allow then
        begin
          FRightBox.Items.Move(I, idx);
          FRightBox.Selected[idx] := true;
          Dec(idx);

          if Assigned(FOnMovedBottom) then
            FOnMovedBottom(Self, I);
        end;
        Dec(I);
      end
      else
        Dec(I);
    end;

    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveBottom(FRightBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveBottom(Index: Integer);
var
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if (Index >= 0) and (Index < FRightBox.Items.Count - 1) then
  begin
    allow := True;
    if Assigned(FOnMoveBottom) then
      FOnMoveBottom(Self, Index, allow);

    if not allow then
      Exit;

    FRightBox.Items.Move(Index, FRightBox.Items.Count - 1);
    FRightBox.ItemIndex := FRightBox.Items.Count - 1;

    if Assigned(FOnMovedBottom) then
      FOnMovedBottom(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveDown;
var
  I: Integer;
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if FRightBox.MultiSelect then
  begin
    I := FRightBox.Items.Count - 1;
    while (I >= 0) do
    begin
      if FRightBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveDown) then
          FOnMoveDown(Self, I, allow);

        if allow then
        begin
          FRightBox.Items.Move(I, I + 1);
          FRightBox.Selected[I + 1] := true;

          if Assigned(FOnMovedDown) then
            FOnMovedDown(Self, I);
        end;
        Dec(I);
      end
      else
        Dec(I);
    end;
    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveDown(FRightBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveDown(Index: Integer);
var
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if (Index >= 0) and (Index < FRightBox.Items.Count - 1) then
  begin
    allow := True;
    if Assigned(FOnMoveDown) then
      FOnMoveDown(Self, Index, allow);

    if not allow then
      Exit;

    FRightBox.Items.Move(Index, Index + 1);
    FRightBox.ItemIndex := Index + 1;

    if Assigned(FOnMovedDown) then
      FOnMovedDown(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveLeftRightAll;
var
  allow: Boolean;
begin
  if not (moMoveLeftRight in MoveOptions) then
    Exit;

  allow := True;
  if Assigned(OnMoveLeftRightAll) then
    FOnMoveLeftRightAll(Self, allow);

  if not allow then
    Exit;

  FRightBox.Items.AddStrings(FLeftBox.Items);
  FInternalRightListChange := True;
  FListRight.Items.Assign(FRightBox.Items);
  FInternalRightListChange := False;

  FLeftBox.Items.Clear;
  FInternalLeftListChange := True;
  FListLeft.Items.Clear;
  FInternalLeftListChange := False;

  if Assigned(OnMovedLeftRightAll) then
    FOnMovedLeftRightAll(Self);

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveSelectedItems(FromList, ToList: TDualListBox; AtIndex: Integer);
begin
  if (FromList = FRightBox) and (ToList = FLeftBox) then
    MoveRightToLeft
  else if (FromList = FLeftBox) and (ToList = FRightBox) then
    MoveLeftToRight;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveTop;
var
  I, idx: Integer;
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if FRightBox.MultiSelect then
  begin
    I := 0;
    idx := 0;
    while (I <= FRightBox.Items.Count - 1) do
    begin
      if FRightBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveTop) then
          FOnMoveTop(Self, I, allow);

        if allow then
        begin
          FRightBox.Items.Move(I, idx);
          FRightBox.Selected[idx] := true;
          Inc(idx);

          if Assigned(FOnMovedTop) then
            FOnMovedTop(Self, I);
        end;
        Inc(I);
      end
      else
        Inc(I);
    end;

    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveTop(FRightBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveTop(Index: Integer);
var
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if (Index > 0) and (Index < FRightBox.Items.Count) then
  begin
    allow := True;
    if Assigned(FOnMoveTop) then
      FOnMoveTop(Self, Index, allow);

    if not allow then
      Exit;

    FRightBox.Items.Move(Index, 0);
    FRightBox.ItemIndex := 0;

    if Assigned(FOnMovedTop) then
      FOnMovedTop(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveUp;
var
  I: Integer;
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if FRightBox.MultiSelect then
  begin
    I := 0;

    while (I <= FRightBox.Items.Count - 1) do
    begin
      if FRightBox.Selected[I] then
      begin
        allow := True;
        if Assigned(FOnMoveUp) then
          FOnMoveUp(Self, I, allow);

        if allow then
        begin
          FRightBox.Items.Move(I, I - 1);
          FRightBox.Selected[I - 1] := True;

          if Assigned(FOnMovedUp) then
            FOnMovedUp(Self, I);
        end;
        Inc(I);
      end
      else
        Inc(I);
    end;

    UpdateEnableButtons;

    DoListsChanged;
  end
  else
    MoveUp(FRightBox.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.MoveUp(Index: Integer);
var
  allow: Boolean;
begin
  if not (moMoveUpDown in MoveOptions) and not FRightBox.Sorted then
    Exit;

  if (Index > 0) and (Index < FRightBox.Items.Count) then
  begin
    allow := True;
    if Assigned(FOnMoveUp) then
      FOnMoveUp(Self, Index, allow);

    if not allow then
      Exit;

    FRightBox.Items.Move(Index, Index - 1);
    FRightBox.ItemIndex := Index - 1;

    if Assigned(FOnMovedUp) then
      FOnMovedUp(Self, Index);
  end;

  UpdateEnableButtons;

  DoListsChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.Loaded;
begin
  inherited;

  LoadGlyphs;
  OnListLeftChanged(Self);
  OnListRightChanged(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDualListBox.LoadGlyphs;
begin
  if (csLoading in ComponentState) or not Assigned(FButtons) then
    Exit;

  if Assigned(FBtnLeftOne) then
  begin
    if not FButtons.LeftGlyph.Empty then
      FBtnLeftOne.Glyph.Assign(FButtons.LeftGlyph)
    else
    begin
      FBtnLeftOne.Glyph.LoadFromResourceName(HInstance, 'DLB_LEFTGLYPH');
      FBtnLeftAll.NumGlyphs := 2;
    end;

    if FBtnLeftOne.Glyph.Width > 16 then
      FBtnLeftOne.NumGlyphs := 2
    else
      FBtnLeftOne.NumGlyphs := 1;
  end;

  if Assigned(FBtnLeftAll) then
  begin
    if not FButtons.LeftAllGlyph.Empty then
      FBtnLeftAll.Glyph.Assign(FButtons.LeftAllGlyph)
    else
    begin
      FBtnLeftAll.Glyph.LoadFromResourceName(HInstance, 'DLB_LEFTALLGLYPH');
      FBtnLeftAll.NumGlyphs := 2;
    end;

    if FBtnLeftAll.Glyph.Width > 16 then
      FBtnLeftAll.NumGlyphs := 2
    else
      FBtnLeftAll.NumGlyphs := 1;
  end;

  if Assigned(FBtnRightOne) then
  begin
    if not FButtons.RightGlyph.Empty then
      FBtnRightOne.Glyph.Assign(FButtons.RightGlyph)
    else
    begin
      FBtnRightOne.Glyph.LoadFromResourceName(HInstance, 'DLB_RIGHTGLYPH');
      FBtnRightOne.NumGlyphs := 2;
    end;

    if FBtnRightOne.Glyph.Width > 16 then
      FBtnRightOne.NumGlyphs := 2
    else
      FBtnRightOne.NumGlyphs := 1;
  end;

  if Assigned(FBtnRightAll) then
  begin
    if not FButtons.RightAllGlyph.Empty then
      FBtnRightAll.Glyph.Assign(FButtons.RightAllGlyph)
    else
    begin
      FBtnRightAll.Glyph.LoadFromResourceName(HInstance, 'DLB_RIGHTALLGLYPH');
      FBtnRightAll.NumGlyphs := 2;
    end;

    if FBtnRightAll.Glyph.Width > 16 then
      FBtnRightAll.NumGlyphs := 2
    else
      FBtnRightAll.NumGlyphs := 1;

  end;

  if Assigned(FBtnTop) then
  begin
    if not FButtons.TopGlyph.Empty then
      FBtnTop.Glyph.Assign(FButtons.TopGlyph)
    else
    begin
      FBtnTop.Glyph.LoadFromResourceName(HInstance, 'DLB_TOPGLYPH');
      FBtnTop.NumGlyphs := 2;
    end;

    if FBtnTop.Glyph.Width > 16 then
      FBtnTop.NumGlyphs := 2
    else
      FBtnTop.NumGlyphs := 1;
  end;

  if Assigned(FBtnUp) then
  begin
    if not FButtons.UpGlyph.Empty then
      FBtnUp.Glyph.Assign(FButtons.UpGlyph)
    else
    begin
      FBtnUp.Glyph.LoadFromResourceName(HInstance, 'DLB_UPGLYPH');
      FBtnUp.NumGlyphs := 2;
    end;

    if FBtnUp.Glyph.Width > 16 then
      FBtnUp.NumGlyphs := 2
    else
      FBtnUp.NumGlyphs := 1;
  end;

  if Assigned(FBtnDown) then
  begin
    if not FButtons.DownGlyph.Empty then
      FBtnDown.Glyph.Assign(FButtons.DownGlyph)
    else
    begin
      FBtnDown.Glyph.LoadFromResourceName(HInstance, 'DLB_DOWNGLYPH');
      FBtnDown.NumGlyphs := 2;
    end;

    if FBtnDown.Glyph.Width > 16 then
      FBtnDown.NumGlyphs := 2
    else
      FBtnDown.NumGlyphs := 1;
  end;

  if Assigned(FBtnBottom) then
  begin
    if not FButtons.DownGlyph.Empty then
      FBtnBottom.Glyph.Assign(FButtons.BottomGlyph)
    else
    begin
      FBtnBottom.Glyph.LoadFromResourceName(HInstance, 'DLB_BottomGLYPH');
      FBtnBottom.NumGlyphs := 2;
    end;

    if FBtnBottom.Glyph.Width > 16 then
      FBtnBottom.NumGlyphs := 2
    else
      FBtnBottom.NumGlyphs := 1;
  end;
end;

//------------------------------------------------------------------------------

{ TButtonsProp }

procedure TButtonsProp.Assign(Source: TPersistent);
begin
  if (Source is TButtonsProp) then
  begin
    FLeftGlyph.Assign((Source as TButtonsProp).LeftGlyph);
    FLeftAllGlyph.Assign((Source as TButtonsProp).LeftAllGlyph);
    FRightGlyph.Assign((Source as TButtonsProp).RightGlyph);
    FRightAllGlyph.Assign((Source as TButtonsProp).RightAllGlyph);
    FTopGlyph.Assign((Source as TButtonsProp).TopGlyph);
    FUpGlyph.Assign((Source as TButtonsProp).UpGlyph);
    FDownGlyph.Assign((Source as TButtonsProp).DownGlyph);
    FBottomGlyph.Assign((Source as TButtonsProp).BottomGlyph);
    FButtonWidth := (Source as TButtonsProp).ButtonWidth;
    FHintRight := (Source as TButtonsProp).HintRight;
    FHintLeftAll := (Source as TButtonsProp).HintLeftAll;
    FHintRightAll := (Source as TButtonsProp).HintRightAll;
    FHintLeft := (Source as TButtonsProp).HintLeft;
    FHintTop := (Source as TButtonsProp).HintTop;
    FHintUp := (Source as TButtonsProp).HintUp;
    FHintDown := (Source as TButtonsProp).HintDown;
    FHintBottom := (Source as TButtonsProp).HintBottom;
    VisibleButtons := (Source as TButtonsProp).VisibleButtons;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TButtonsProp.Create;
begin
  inherited;

  FButtonWidth := 22;
  FVisibleButtons := [vbLeft, vbRight, vbLeftAll, vbRightAll];
  FLeftGlyph := TBitmap.Create;
  FLeftGlyph.OnChange := OnGlyphChanged;
  FLeftAllGlyph := TBitmap.Create;
  FLeftAllGlyph.OnChange := OnGlyphChanged;

  FRightGlyph := TBitmap.Create;
  FRightGlyph.OnChange := OnGlyphChanged;
  FRightAllGlyph := TBitmap.Create;
  FRightAllGlyph.OnChange := OnGlyphChanged;

  FTopGlyph := TBitmap.Create;
  FTopGlyph.OnChange := OnGlyphChanged;
  FUpGlyph := TBitmap.Create;
  FUpGlyph.OnChange := OnGlyphChanged;
  FDownGlyph := TBitmap.Create;
  FDownGlyph.OnChange := OnGlyphChanged;
  FBottomGlyph := TBitmap.Create;
  FBottomGlyph.OnChange := OnGlyphChanged;

  FHintRight := 'Move Right';
  FHintLeftAll := 'Move Left All';
  FHintRightAll := 'Move Right All';
  FHintLeft := 'Move Left';
  FHintTop := 'Move item to the top of the list';
  FHintUp := 'Move item one place up';
  FHintDown := 'Move item one place down';
  FHintBottom := 'Move item to the bottom of the list';
end;

//------------------------------------------------------------------------------

destructor TButtonsProp.Destroy;
begin
  FLeftGlyph.Free;
  FLeftAllGlyph.Free;
  FRightGlyph.Free;
  FRightAllGlyph.Free;
  FTopGlyph.Free;
  FUpGlyph.Free;
  FDownGlyph.Free;
  FBottomGlyph.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.OnGlyphChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetBottomGlyph(const Value: TBitmap);
begin
  FBottomGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetButtonWidth(const Value: Integer);
begin
  if (FButtonWidth <> Value) then
  begin
    FButtonWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetDownGlyph(const Value: TBitmap);
begin
  FDownGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintBottom(const Value: String);
begin
  FHintBottom := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintDown(const Value: String);
begin
  FHintDown := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintLeft(const Value: String);
begin
  FHintLeft := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintLeftAll(const Value: String);
begin
  FHintLeftAll := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintRight(const Value: String);
begin
  FHintRight := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintRightAll(const Value: String);
begin
  FHintRightAll := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintTop(const Value: String);
begin
  FHintTop := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetHintUp(const Value: String);
begin
  FHintUp := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetLeftAllGlyph(const Value: TBitmap);
begin
  FLeftAllGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetLeftGlyph(const Value: TBitmap);
begin
  FLeftGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetRightAllGlyph(const Value: TBitmap);
begin
  FRightAllGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetRightGlyph(const Value: TBitmap);
begin
  FRightGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetTopGlyph(const Value: TBitmap);
begin
  FTopGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetUpGlyph(const Value: TBitmap);
begin
  FUpGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonsProp.SetVisibleButtons(const Value: TVisibleButtons);
begin
  if (FVisibleButtons <> Value) then
  begin
    FVisibleButtons := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

end.


