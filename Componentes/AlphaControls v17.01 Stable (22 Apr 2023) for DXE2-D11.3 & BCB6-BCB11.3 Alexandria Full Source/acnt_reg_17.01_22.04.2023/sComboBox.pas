unit sComboBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ImgList,
{$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
{$IFNDEF DELPHI5} Types, {$ENDIF}
{$IFDEF TNTUNICODE} TntControls, TntClasses, TntActnList, TntStdCtrls, TntGraphics, {$ENDIF}
  sConst, sDefaults, acSBUtils, sCommonData, acntTypes, sCustomComboEdit, sSkinProvider, sPanel,
  sSpeedButton, sScrollBox, sListBox;


type
{$IFNDEF NOTFORHELP}
  TacGetItemDrawData = procedure(Sender: TObject; var ItemDrawData: TacItemDrawData) of object;
  TacCustomCombo = class;
  TacItemsPopupForm = class;


  TacPopupBtn = class(TsSpeedButton)
  protected
    FPressed: boolean;
    FOwnerForm: TacItemsPopupForm;
    function PrepareCache: boolean; override;
    function CanShiftContent: boolean; override;
    procedure StdPaint(PaintButton: boolean = True); override;
    procedure DrawCaption; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure SelectItem;
  end;


  TacPopupScrollBox = class(TsScrollBox)
  protected
    FOwner: TacItemsPopupForm;
  public
    function CanFocus: Boolean; override;
  end;


  TacPopupPanel = class(TsPanel)
  public
    function PrepareCache: Boolean; override;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  end;


  TacItemsPopupForm = class(TForm)
  protected
    FItems: TList;
    ItemCount, ItemWidth, ItemHeight: integer;

    sp: TsSkinProvider;
    Panel: TacPopupPanel;
    sb: TacPopupScrollBox;
    FOwner: TacCustomCombo;
    UpdateFlags: set of TacUpdateFlags;
    FItemIndex: integer;

    procedure SetItemIndex(const Value: integer);
    procedure LimitScroll;
    procedure InitFormData;
    procedure UpdateBoxSize;
    procedure UpdateControls;
    procedure UpdateHotControl;
    procedure DoClose(var Action: TCloseAction); override;
    procedure HandleOnClose(Sender: TObject; var Action: TCloseAction);
    procedure CreateParams(var Params: TCreateParams); override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Items: TList read FItems write FItems;
  public
    destructor Destroy; override;
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    procedure WndProc(var Message: TMessage); override;
  end;


  TacSimpleBox = class(TsListBox)
  public
    DsgnPressed: boolean;
    Owner: TacCustomCombo;
    procedure AlignBox;
    procedure Change; override;
    procedure HandleOwnerMsg(const Message: TMessage; const OwnerControl: TControl);
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    function CanFocus: boolean; override;
  end;


  TacCustomCombo = class(TsCustomComboEdit)
  private
    FCanvas: TCanvas;
    FImageChangeLink: TChangeLink;
    FAllowMouseWheel: Boolean;
    FOnGetItemDrawData: TacGetItemDrawData;
    FOnDropDown: TNotifyEvent;
    FDroppedDownCount: integer;
    FOnDrawItem: TDrawItemEvent;
    FItemIndex: integer;
    FMinPopupWidth: integer;
    FOnCloseUp: TNotifyEvent;
    FItemHeight: integer;
    FOnSelect: TNotifyEvent;
    FImages: TCustomImageList;
    FDropInplace: boolean;
    FTextBuffer: string;
    FTextTimer: TObject;
    FMakeTextPlain: boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(const Value: TCustomImageList);
    function GetDroppedDown: boolean;
    procedure SetDroppedDown(const Value: boolean);

{$IFDEF DELPHI7UP}
    procedure CreateTextTimer;
    procedure TextTimerEvent(Owner: TObject);
{$ENDIF}
  protected
    FItems: TStrings;
    State,
    FGlyphIndex: integer;
    FItemChanging,
    FCloseAllowed,
    FRequestExtData: Boolean;
    FSimpleBox: TacSimpleBox;
    FPopupForm: TacItemsPopupForm;

    function GetEditText(AItemIndex: integer): acString; virtual; abstract;
    function GetItemCount: integer; virtual; abstract;
    function IsOwnerDraw: boolean; virtual; abstract;
    function IsDropDownList: boolean; virtual; abstract;
    function ImageExists: boolean; virtual;

    function GetText(AItemIndex: integer): acString;
    function BodyRect: TRect; override;
    procedure Change; override;
    function ItemTextDefined(AIndex: integer): boolean; virtual;
    function ItemDrawState: TOwnerDrawState; virtual;
    function ButtonRect: TRect; virtual;
    procedure SetItems(const Value: TStrings);
    function AllowDropDownList: boolean; virtual;
    function SetNewIndex(AValue: integer): boolean;
    procedure UpdateButtonMode;
    procedure SetItemIndex(const Value: integer); virtual;
    procedure UpdateText;
    function HandleAlphaMsg(var Message: TMessage): Boolean;
    function DoMouseDown: boolean;
    procedure WMLButtonDblClk(var Message: TMessage); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); Message WM_LBUTTONDOWN;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure AdjustDropDown;
    procedure CloseUp; virtual;
    function CanMoveTextHint(InFocusing: boolean): boolean; override;
    function CanDropDown: boolean; virtual;
    function CanCreateListBox: boolean; virtual;
    function GetAutoHeight: integer;
    procedure CreateListBox; virtual;
    procedure ItemsChanged;

    function HandleKeyDown(var ACharCode: word): boolean; virtual;
    function HandleChar(var AChar: word): boolean; virtual;

    function GetTextDrawStyle: cardinal;
    procedure GetItemDrawData(var DrawData: TacItemDrawData); virtual;
    function GetItemFontColor(SkinIndex, State: integer; DrawState: TOwnerDrawState): TColor;
    procedure PaintItemContentEx(SkinIndex: integer; Bmp: TBitmap; R: TRect; DrawState: TOwnerDrawState; DrawData: TacItemDrawData);
    procedure PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer); virtual;
    procedure DrawItem(Bmp: TBitmap; Index: Integer; R: TRect; DrawState: TOwnerDrawState); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CheckAutoComplete: boolean; virtual;
    function IsDefaultValue: boolean; override;
    procedure PaintText; override;
    function IsCaseSensitive: boolean; virtual;
    procedure UpdateIndexes;
    procedure DoCloseUp;
    function ContentHeight(DrawData: TacItemDrawData; CheckImages: boolean = True): integer;
    function GetTextHeight(aDrawData: TacItemDrawData; aMaxWidth: integer): integer;

    function TextStored: boolean;
  public
    procedure UpdateForm;
    procedure DoDropDown; virtual;
    procedure Select; dynamic;
    function IsFullPaint: boolean; override;
    procedure WndProc(var Message: TMessage); override;
    procedure PopupWindowShow; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Clear; override;
    procedure CreateWnd; override;
    procedure AddItem(Item: String; AObject: TObject); virtual;
    procedure ClearSelection; reintroduce;
    function GetCount: Integer; virtual;
    function CurrentState: integer; override;

    property DroppedDown: boolean read GetDroppedDown write SetDroppedDown;

    property Canvas: TCanvas read FCanvas;
    property ItemHeight: integer read FItemHeight write FItemHeight default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property DropInplace: boolean read FDropInplace write FDropInplace default False;
    property MakeTextPlain: boolean read FMakeTextPlain write FMakeTextPlain default False;

    property OnGetItemDrawData: TacGetItemDrawData read FOnGetItemDrawData write FOnGetItemDrawData;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  published
    property AllowMouseWheel: Boolean read FAllowMouseWheel write FAllowMouseWheel default True;
    property DropDownCount: integer read FDroppedDownCount write FDroppedDownCount default 16;
    property MinPopupWidth: integer read FMinPopupWidth write FMinPopupWidth default 0;
{$ENDIF} // NOTFORHELP

    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TacComboBoxStrings = class(TStringList)
  public
    OnCleared: TNotifyEvent;
    procedure Clear; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    constructor Create; overload;
  end;

  TsCustomComboBox = class(TacCustomCombo)
  private
    FStyle: TComboBoxStyle;
    FSorted: boolean;
    FAutoComplete: boolean;
    FAutoCompleteDelay: Cardinal;
    FAutoCloseUp: boolean;
    FAutoDropDown: boolean;
    procedure ItemsCleared(Sender: TObject);
    procedure SetSorted(const Value: Boolean);
  protected
    function AllowBtnStyle: Boolean; override;
    function CanShowButton: boolean; override;
    procedure SetStyle(const Value: TComboBoxStyle);
    function GetEditText(AItemIndex: integer): acString; override;
    function GetItemCount: integer; override;
    function IsOwnerDraw: boolean; override;
    function HandleKeyDown(var ACharCode: word): boolean; override;
    function HandleChar(var AChar: word): boolean; override;
    function CheckAutoComplete: boolean; override;
    function CanDropDown: boolean; override;
    function CanCreateListBox: boolean; override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure Change; override;
    procedure DoItemsChanged(Sender: TObject);
  public
    function IsDropDownList: boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOf(const s: acString): integer;
    procedure WndProc(var Message: TMessage); override;
    property AutoCloseUp: boolean read FAutoCloseUp write FAutoCloseUp default False;
    property AutoDropDown: boolean read FAutoDropDown write FAutoDropDown default False;
    property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
  published
{$IFDEF DELPHI7UP}
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default True;
    property AutoCompleteDelay: Cardinal read FAutoCompleteDelay write FAutoCompleteDelay default 500;
{$ENDIF}
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnGetItemDrawData;
    property ReadOnly;
    property TextHint;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TsComboBox = class(TsCustomComboBox)
  published
{$IFNDEF NOTFORHELP}
    property AutoCloseUp;
    property AutoDropDown;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropInplace;
    property Enabled;
    property Font;
    property Images;
    property ImeMode;
    property ImeName;
    property ItemIndex default -1;
    property ItemHeight;
    property Items;
    property MaxLength;
    property MakeTextPlain;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
{$IFDEF TNTUNICODE}
    property SelText;
    property SelStart;
    property SelLength;
{$ENDIF}
    property ShowHint;
    property Style default csDropDown;
    property TabOrder;
    property TabStop;
    property Text stored TextStored;
    property Visible;
{$IFDEF D2006}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
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
    property BoundLabel;
    property DisabledKind;
    property SkinData;
  end;


implementation

uses
  math, ExtCtrls, Buttons,
{$IFDEF DELPHI7UP}Themes, {$ENDIF}
  sStyleSimply, sSkinProps, sVCLUtils, sMessages, sAlphaGraph, acntUtils, sGraphUtils, sSkinManager, acGlow,
  acGPUtils, acAnimation, sGlyphUtils, acPopupController, sThirdParty, acAlphaImageList, sHtmlParse;


procedure TacCustomCombo.AddItem(Item: String; AObject: TObject);
begin
  Items.AddObject(Item, AObject);
end;


procedure TacCustomCombo.AdjustDropDown;
begin
  if SysPopupCount <= 0 then
    GetIntController.KillAllForms(Self);

  PopupWindowShow;
end;


function TacCustomCombo.CanCreateListBox: boolean;
begin
  Result := False;
end;


function TacCustomCombo.CanDropDown: boolean;
begin
  Result := True;
end;


function TacCustomCombo.CanMoveTextHint(InFocusing: boolean): boolean;
begin
  Result := (BtnState <> ACPRESSED) and not DroppedDown and (not IsDropDownList or not InFocusing);
end;


procedure TacCustomCombo.Change;
begin
  if Assigned(OnChange) and not (csLoading in ComponentState) and (SkinData.CtrlSkinState and ACS_INIT = 0) then
    OnChange(Self);

  if (Items.Count = 0) and (FItemIndex >= 0) then
    FItemIndex := -1;

  if FSimpleBox <> nil then begin
    FSimpleBox.ItemIndex := ItemIndex;
  end;
end;


function TacCustomCombo.CheckAutoComplete: boolean;
var
  i, l: integer;

  function ItemIsFound(s: string): boolean;
  begin
    if IsCaseSensitive then
      Result := Pos(FTextBuffer, Items[i]) = 1
    else
      Result := Pos(UpperCase(FTextBuffer), UpperCase(Items[i])) = 1;
  end;

begin
  if (SkinData.CtrlSkinState and ACS_CHANGING = 0) then begin
    Result := True;
    for i := 0 to Items.Count - 1 do
      if ItemIsFound(Items[i]) then begin
        SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_CHANGING;
        ItemIndex := i;
        if not AllowBtnStyle then begin
          l := Length(FTextBuffer);
          Self.SetSelStart(l);
          Self.SetSelLength(Length(Text) - l);
        end;
        if Assigned(OnChange) then
          OnChange(Self);

        SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_CHANGING;
{$IFDEF DELPHI7UP}
        CreateTextTimer;
{$ENDIF}
        Exit;
      end;

{$IFDEF DELPHI7UP}
    CreateTextTimer;
{$ENDIF}
  end
  else
    Result := False;
end;


procedure TacCustomCombo.Clear;
begin
  SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_INIT;
  inherited;
  ItemIndex := -1;
  FItems.Clear;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_INIT;
end;


procedure TacCustomCombo.ClearSelection;
begin
  ItemIndex := -1;
end;


procedure TacCustomCombo.CloseUp;
begin
  FDroppedDown := False;
  FArrowAngle := 0;
end;


function TacCustomCombo.ContentHeight(DrawData: TacItemDrawData; CheckImages: boolean = True): integer;
var
  ImgSize: TSize;
begin
  if CheckImages and (Images <> nil) then begin
    ImgSize.cx := GetImageWidth(Images, -1, SkinData.CurrentPPI) + SkinData.CommonSkinData.Spacing;
    ImgSize.cy := GetImageHeight(Images, -1, SkinData.CurrentPPI);
  end
  else
    ImgSize := MkSize;

  Result := GetStringSize(SkinData.FCacheBmp.Canvas.Font.Handle, DrawData.Text, 0, False, Width - ImgSize.cx).cy +
            SkinData.CommonSkinData.Spacing + GetStringSize(SkinData.FCacheBmp.Canvas.Font.Handle, DrawData.HtmlText).cy;

  Result := max(Result, ImgSize.cy);
end;


function TacCustomCombo.AllowDropDownList: boolean;
begin
  Result := True;
end;


function TacCustomCombo.BodyRect: TRect;
begin
  Result := MkRect(Self);
  if CanCreateListBox then
    Result.Bottom := Result.Top + GetAutoHeight;
end;


function TacCustomCombo.ButtonRect: TRect;
var
  w: integer;
begin
  if CanShowButton then
    w := GetComboBtnSize(SkinData.SkinManager, SkinData.CommonSkinData.PPI) - 1
  else
    w := 0;

  Result.Top := SkinData.CommonSkinData.ComboBoxMargin;
  if UseRightToLeftAlignment then
    Result.Left := Result.Top
  else
    Result.Left := Width - w - Result.Top;

  Result.Right := Result.Left + w;
  Result.Bottom := Height - Result.Top;
end;


function TacCustomCombo.GetAutoHeight: integer;
var
  DC: HDC;
  I: Integer;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  if NewStyleControls then
    I := GetSystemMetrics(SM_CYBORDER) * iff(Ctl3D, 8, 6)
  else begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;

    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Result := Metrics.tmHeight + I;
end;


function TacCustomCombo.GetCount: Integer;
begin
  Result := GetItemCount;
end;


function TacCustomCombo.GetDroppedDown: boolean;
begin
  Result := FDroppedDown;
end;


procedure TacCustomCombo.GetItemDrawData(var DrawData: TacItemDrawData);
begin
  FOnGetItemDrawData(Self, DrawData);
end;


function TacCustomCombo.GetItemFontColor(SkinIndex, State: integer; DrawState: TOwnerDrawState): TColor;
begin
  if (odSelected in DrawState) and not (AllowBtnStyle and (odComboBoxEdit in DrawState)) then
    Result := SkinData.SkinManager.GetHighLightFontColor(True)
  else
    if SkinData.CustomFont then
       Result := Font.Color
    else
      if odComboBoxEdit in DrawState then
        Result := SkinData.CommonSkinData.gd[SkinIndex].Props[State].FontColor.Color
      else
        Result := SkinData.SkinManager.Palette[pcEditText];
end;


function TacCustomCombo.GetText(AItemIndex: integer): acString;
begin
  if IsValidIndex(AItemIndex, Items.Count) then
    Result := Items[AItemIndex]
  else
    Result := '';
end;


function TacCustomCombo.GetTextDrawStyle: cardinal;
begin
  Result := DT_NOPREFIX or DT_EXPANDTABS or DT_SINGLELINE;
  if UseRightToLeftAlignment then
    Result := Result or DT_RIGHT;

  if UseRightToLeftReading then
    Result := Result or DT_RTLREADING;

  case Alignment of
    taLeftJustify:;
    taCenter:       Result := Result or DT_CENTER;
    taRightJustify: Result := Result or DT_RIGHT;
  end;
end;


function TacCustomCombo.GetTextHeight(aDrawData: TacItemDrawData; aMaxWidth: integer): integer;
var
  s: acString;
begin
  if aDrawData.Text <> '' then
    Result := GetStringSize(SkinData.FCacheBmp.Canvas.Font.Handle, aDrawData.Text, 0, False, aMaxWidth).cy
  else
    Result := 0;

  if aDrawData.HtmlText <> '' then begin
    s := GetPlainText(aDrawData.HtmlText, True); // Quick removing of tags (except <br>)
    Result := Result + GetStringSize(aDrawData.Font.Handle, s, 0, True, aMaxWidth).cy;
    if aDrawData.Text <> '' then
      Result := Result + SkinData.CommonSkinData.Spacing;
  end;
end;


constructor TacCustomCombo.Create(AOwner: TComponent);
begin
  FSimpleBox := nil;
  inherited Create(AOwner);
  FTextTimer := nil;
  SkinData.COC := COC_TsComboBox;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FDefBmpID := iBTN_ARROW;
  FCloseAllowed := False;
  DropDownCount := 16;
  FMinPopupWidth := 0;
  FItemIndex := -1;
  FItemHeight := 0;
  FRequestExtData := False;
  FDroppedDown := False;
  FMakeTextPlain := False;
  FSkipDrop := False;
  FItemChanging := False;
  FDropInplace := False;
  FAllowMouseWheel := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;


procedure TacCustomCombo.CreateListBox;
begin
  if CanCreateListBox and (FSimpleBox = nil) then begin
    FSimpleBox := TacSimpleBox.Create(Self);
    FSimpleBox.Name := Name + '_sb';
    FSimpleBox.Parent := Parent;
    if Parent <> nil then begin
      FSimpleBox.AlignBox;
      ItemsChanged;
    end;
  end;
end;

{$IFDEF DELPHI7UP}

procedure TacCustomCombo.CreateTextTimer;
var
  iDelay: integer;
begin
  if Self is TsCustomComboBox then
    iDelay := TsCustomComboBox(Self).AutoCompleteDelay
  else
    iDelay := 500;

  if FTextTimer = nil then begin
    FTextTimer := TTimer.Create(Self);
    TTimer(FTextTimer).Interval := iDelay;
    TTimer(FTextTimer).OnTimer := TextTimerEvent;
  end
  else // Reset interval
    TTimer(FTextTimer).Enabled := False;

  TTimer(FTextTimer).Enabled := True;
end;

{$ENDIF}

procedure TacCustomCombo.CreateWnd;
begin
  inherited;
  UpdateIndexes;
  UpdateButtonMode;
  CreateListBox;
end;


function TacCustomCombo.CurrentState: integer;
begin
  if (FSimpleBox <> nil) and FSimpleBox.Focused then
    Result := 1
  else
    Result := inherited CurrentState;
end;


destructor TacCustomCombo.Destroy;
begin
  FreeAndNil(FTextTimer);
  FreeAndNil(FCanvas);
  FreeAndNil(FImageChangeLink);
  FSimpleBox := nil; // Destroyed automatically
  inherited Destroy;
end;


procedure TacCustomCombo.DoCloseUp;
begin
  CloseUp;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;


procedure TacCustomCombo.DoDropDown;
begin
  if not Focused and CanFocus then
    SetFocus;

  if Assigned(OnDropDown) then
    OnDropDown(Self);
end;


function TacCustomCombo.DoMouseDown: boolean;
begin
  Result := True;
  if not(csDesigning in ComponentState) then
    if ReadOnly then begin
      if CanFocus then
        SetFocus;
    end
    else
      if not FSkipDrop then
        if not AllowDropDownList then
          AdjustDropDown
        else
          if IsDropDownList then begin
            BtnState := ACPRESSED;
            if ComboBtn then begin
              if SysPopupCount <= 0 then
                GetIntController.KillAllForms(Self);

              ButtonClick;
            end;
          end
          else
            Result := False
      else
        FSkipDrop := False
  else
    Result := False;
end;


procedure TacCustomCombo.PaintItemContent(ItIndex, SkinIndex: integer; Bmp: TBitmap; var R: TRect; DrawState: TOwnerDrawState; State: integer);
var
  s: acString;
  TextStyle: Cardinal;
begin
  TextStyle := GetTextDrawStyle;
  if not (odComboBoxEdit in DrawState) then begin
    InflateRect(R, -2, -1);
    TextStyle := TextStyle or DT_VCENTER;
    s := GetText(ItIndex);
  end
  else
    s := GetEditText(ItIndex);

  Bmp.Canvas.Brush.Style := bsClear;
  if SkinIndex >= 0 then // Selection skin is specified
    acWriteTextEx(Bmp.Canvas, PACChar(s), True, R, TextStyle, SkinIndex, (odSelected in DrawState) or (State > 0), SkinData.SkinManager)
  else
    if not (odComboBoxEdit in DrawState) or not IsDefaultValue or (TextHint = '') then
      AcDrawText(Bmp.Canvas.Handle, s, R, TextStyle)
    else
      if (TextHint <> '') and (odComboBoxEdit in DrawState) and (not (odFocused in DrawState) or IsDropDownList) then begin // Draw TextHint
        if SkinData.CtrlSkinState and ACS_TEXTANIMATING = 0 then begin
          Bmp.Canvas.Brush.Style := bsClear;
          Bmp.Canvas.Font.Color := TextHintColor(SkinData, State, GetTransMode(SkinData) = TM_FULL);
          AcDrawText(Bmp.Canvas.Handle, TextHint, R, GetTextDrawStyle);
        end;
      end
end;


procedure TacCustomCombo.PaintItemContentEx(SkinIndex: integer; Bmp: TBitmap; R: TRect; DrawState: TOwnerDrawState; DrawData: TacItemDrawData);
var
  gRect, rText, sText: TRect;
  TextStyle: Cardinal;
  imgW, imgH, tH, iHeight, wMax, iSpacing: integer;
begin
  iSpacing := SkinData.CommonSkinData.Spacing;
  // Calc rects
  if Images <> nil then begin
    imgW := GetImageWidth(Images, -1, SkinData.CurrentPPI);
    imgH := GetImageHeight(Images, -1, SkinData.CurrentPPI);
  end
  else begin
    imgW := 0;
    imgH := 0;
  end;
  if odComboBoxEdit in DrawState then
    iHeight := Height
  else
    if FPopupForm <> nil then
      iHeight := FPopupForm.ItemHeight
    else
      iHeight := ItemHeight;

  wMax := Width - imgW - iSpacing;
  tH := GetTextHeight(DrawData, wMax);

  case VerticalAlignment of
    taAlignTop:       R.Top := iSpacing div 2;
    taVerticalCenter: R.Top := (iHeight - tH) div 2;
    taAlignBottom:    R.Top := iHeight - tH - iSpacing div 2;
  end;
  R.Bottom := R.Top + tH;
  rText := R;
  if not (odComboBoxEdit in DrawState) then
    InflateRect(rText, -4, -1);

  gRect := R;
  if Images <> nil then begin
    gRect.Left := rText.Left;
    gRect.Right := gRect.Left + imgW;
    rText.Left := gRect.Right + iSpacing;
    // Paint glyph
    if IsValidIndex(DrawData.ImageIndex, Images.Count) then begin
      case VerticalAlignment of
        taAlignTop:       gRect.Top := rText.Top;
        taVerticalCenter: gRect.Top := rText.Top + (tH - imgH) div 2;
        taAlignBottom:    gRect.Top := rText.Bottom - imgH;
      end;
      gRect.Bottom := gRect.Top + imgH;
      SetImagesPPI(Images, SkinData.CurrentPPI);
      Images.Draw(Bmp.Canvas, gRect.Left, gRect.Top, DrawData.ImageIndex);
      SetImagesPPI(Images, 96);
    end;
  end
  else begin
    gRect.Left := rText.Left;
    gRect.Right := gRect.Left;
  end;
  // Paint text
  sText := rText;
  Bmp.Canvas.Brush.Style := bsClear;
  if DrawData.Text <> '' then begin
    TextStyle := GetTextDrawStyle;
    if SkinIndex >= 0 then // Selection skin is specified
      acWriteTextEx(Bmp.Canvas, PACChar(DrawData.Text), True, rText, TextStyle, SkinIndex, (odSelected in DrawState) or (State > 0), SkinData.SkinManager)
    else
      AcDrawText(Bmp.Canvas.Handle, DrawData.Text, rText, TextStyle);

    sText.Top := rText.Top + GetFontHeight(Bmp.Canvas.Font.Handle, True) + SkinData.CommonSkinData.Spacing;
  end;
  // Paint subtext
  if DrawData.HtmlText <> '' then begin
    TextStyle := DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_NOCLIP;
    Bmp.Canvas.Font.Assign(DrawData.Font);
    Bmp.Canvas.Font.Style := [];
    DrawHtmlText(Bmp, DrawData.HtmlText, sText, TextStyle, SkinData.SkinManager);
  end;
end;


procedure TacCustomCombo.PaintText;
var
  R: TRect;
begin
  R := TextRect;
  DrawItem(SkinData.FCacheBmp, ItemIndex, R, ItemDrawState);
  if ButtonMode and ShowFocus and Focused and CanShowFocus(SkinData) and not DroppedDown then begin
    R := MkRect(Self);
    InflateRect(R, -2, -2);
    if SkinData.SkinIndex >= 0 then
      FocusRect(SkinData.FCacheBmp.Canvas, R)
    else
      acDrawFocusRect(SkinData.FCacheBmp.Canvas, R);
  end;
end;


procedure TacCustomCombo.PopupWindowShow;
var
  R: TRect;

  function CreatePopupForm: TacItemsPopupForm;
  begin
    if FPopupForm = nil then begin
      FPopupForm := TacItemsPopupForm.CreateNew(Self);
      FPopupForm.sp.AllowScale := False;
      FPopupForm.sp.AllowAnimation := True;
      FPopupForm.Name := name + 'Form';
      FPopupForm.UpdateFlags := FPopupForm.UpdateFlags + [ufFormCreating];
      FPopupForm.UpdateFlags := FPopupForm.UpdateFlags + [ufUpdateScale];
      UpdateForm;
      FPopupForm.UpdateHotControl;
      FPopupForm.UpdateFlags := FPopupForm.UpdateFlags - [ufFormCreating];
    end;
    Result := FPopupForm;
  end;

  procedure CalcPopupBounds;
  var
    i: integer;
    CurButton: TsSpeedButton;
  begin
    if IsValidIndex(FItemIndex, Items.Count) then begin
      OffsetRect(R, 0, -2);
      CurButton := TsSpeedButton(FPopupForm.Items[FItemIndex]);
      i := (CurButton.Top + CurButton.Height div 2) - FPopupForm.sb.ClientHeight div 2;
      if i < 0 then
        OffsetRect(R, 0, -i)
      else
        if i > FPopupForm.sb.VertScrollBar.Range - FPopupForm.sb.ClientHeight then
          OffsetRect(R, 0, (FPopupForm.sb.VertScrollBar.Range - FPopupForm.sb.ClientHeight) - i);

      FPopupForm.sb.VertScrollBar.Position := FPopupForm.sb.VertScrollBar.Position + i;
      UpdateScrolls(FPopupForm.sb.ListSW, True);
    end;
  end;

begin
  if CanDropDown then begin
    SkinData.FFocused := True;
    SkinData.BGChanged := True;
    if FPopupForm = nil then begin
      DoDropDown; // SetFocus and call event
      FDroppedDown := True;
      BtnState := ACPRESSED;
      SkinData.FDoLighting := False;
      FreeAndNil(SkinData.LightMask);
      FreeAndNil(SkinData.HotCache);
      PrepareCache;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);

      FPopupWindow := CreatePopupForm;
      if FPopupForm.BiDiMode <> BiDiMode then
        FPopupForm.BiDiMode := BiDiMode;

      GetWindowRect(Handle, R);
      if DropInplace then begin // Zoom
        CalcPopupBounds;
        ShowPopupForm(FPopupForm, Self, R.Left, R.Top - (FPopupForm.Height - HeightOf(R)) div 2, True, PopupAnimProc, True)
      end
      else begin // Drop down
        FPopupForm.LimitScroll;
        ShowPopupForm(FPopupForm, Self, -1, -1, True, PopupAnimProc);
      end;
    end
    else begin
      BtnState := ACNORMAL;
      FCloseAllowed := True;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
      FPopupForm.Close;
      FPopupForm := nil;
      FPopupWindow := nil;
      FCloseAllowed := False;
    end;
  end;
end;


procedure TacCustomCombo.Loaded;
begin
  inherited;
  UpdateIndexes;
  UpdateButtonMode;
  UpdateCaret;
  CreateListBox;
end;


procedure TacCustomCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;


procedure TacCustomCombo.Select;
begin
  FItemChanging := True;
  Click;
  if not (csLoading in ComponentState) {and CanDoChange} then
    if Assigned(FOnSelect) then
      FOnSelect(Self)
    else
      Change;

  FItemChanging := False;
end;


procedure TacCustomCombo.SetDroppedDown(const Value: boolean);
begin
  FDroppedDown := Value;
  SendMessage(Handle, CB_SHOWDROPDOWN, WPARAM(Value), 0);
end;


procedure TacCustomCombo.SetImages(const Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);

  FImages := Value;
  if Images <> nil then begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    Perform($1300{TCM_SETIMAGELIST}, 0, Images.Handle);
  end
  else
    Perform($1300{TCM_SETIMAGELIST}, 0, 0);
end;


procedure TacCustomCombo.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then begin
    FItemIndex := Value;
    if SkinData.CtrlSkinState and ACS_CHANGING = 0 then
      FTextBuffer := '';

    if not (csLoading in ComponentState) then begin
      UpdateText;
      SkinData.Invalidate(True);
      if FSimpleBox <> nil then
        FSimpleBox.ItemIndex := ItemIndex;
    end;
  end;
end;


function TacCustomCombo.SetNewIndex(AValue: integer): boolean;
begin
  if AValue > GetItemCount - 1 then
    AValue := GetItemCount - 1
  else
    if AValue < 0 then
      AValue := 0;

  if ItemIndex <> AValue then begin
    ItemIndex := AValue;
    if DroppedDown and (FPopupForm <> nil) then
      FPopupForm.ItemIndex := ItemIndex;

    SelectAll;
    Select;
  end;
  Result := True;
end;


function TacCustomCombo.TextStored: boolean;
begin
  Result := Text <> '';
end;


{$IFDEF DELPHI7UP}

procedure TacCustomCombo.TextTimerEvent(Owner: TObject);
begin
  FTextBuffer := '';
  TTimer(FTextTimer).Enabled := False;
end;

{$ENDIF}

procedure TacCustomCombo.WMLButtonDblClk(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or not DoMouseDown then
    inherited;
end;


procedure TacCustomCombo.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if (csDesigning in ComponentState) or not DoMouseDown then
    inherited;
end;


procedure TacCustomCombo.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if ImageExists then
    with Message.CalcSize_Params.rgrc[0] do
      if IsRightToLeft then
        dec(Right, GetImageWidth(Images, -1, SkinData.CurrentPPI) + SkinData.CommonSkinData.Spacing)
      else
        inc(Left, GetImageWidth(Images, -1, SkinData.CurrentPPI) + SkinData.CommonSkinData.Spacing);
end;


procedure TacCustomCombo.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETDEFSECTION: begin
          if ButtonMode and (SkinData.SkinManager <> nil) then
            if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.SkinCommonInfo.Sections[ssComboNoEdit] >= 0) then
              Message.Result := 18 + 1 // COMBONOEDIT
            else
              Message.Result := 1 + 1 // BUTTON
          else
            Message.Result := 5 + 1;

          Exit;
        end;

        AC_GETDEFINDEX: begin
          if SkinData.SkinManager <> nil then
            with SkinData.SkinManager.SkinCommonInfo do
              Message.Result := iff(AllowBtnStyle, iff(Sections[ssComboNoEdit] >= 0, Sections[ssComboNoEdit], Sections[ssButton]), Sections[ssComboBox]) + 1;

          Exit;
        end;
      end;

    CB_GETDROPPEDSTATE: begin
      Message.Result := integer(FDroppedDown);
      Exit;
    end;

    CB_SHOWDROPDOWN: begin
      if Message.WParam = 0 then begin
        if DroppedDown and (FPopupForm <> nil) then
          FreeAndNil(FPopupForm);
      end
      else begin
        DoDropDown;
        AdjustDropDown;
      end;
    end;

    CB_SETDROPPEDWIDTH: begin
      MinPopupWidth := max(0, Message.WParam);
    end;

    WM_LBUTTONDOWN: if (FSimpleBox <> nil) and (csDesigning in ComponentState) then begin
      FSimpleBox.DsgnPressed := True;
      FSimpleBox.Parent := nil;
    end;

    WM_WINDOWPOSCHANGED: if (FSimpleBox <> nil) and (csDesigning in ComponentState) and FSimpleBox.DsgnPressed then
      if TWindowPos(Pointer(Message.LParam)^).flags and SWP_SHOWWINDOW = SWP_SHOWWINDOW then begin
        FSimpleBox.Parent := Parent;
        FSimpleBox.DsgnPressed := False;
        FSimpleBox.AlignBox;
      end;

    WM_LBUTTONUP: if (FSimpleBox <> nil) and (csDesigning in ComponentState) and FSimpleBox.DsgnPressed then begin
      FSimpleBox.Parent := Parent;
      FSimpleBox.DsgnPressed := False;
      FSimpleBox.AlignBox;
    end;

    WM_SYSCHAR, WM_SYSKEYDOWN, CN_SYSCHAR, CN_SYSKEYDOWN, WM_KEYDOWN, CN_KEYDOWN: begin
      case TWMKey(Message).CharCode of
        VK_F4, VK_SPACE .. VK_DOWN, $39 .. $39, $41 .. $5A:
          if ReadOnly then
            Exit;
      end;
      case Message.Msg of
        WM_KEYDOWN:
          if HandleKeyDown(TWMKey(Message).CharCode) then
            Exit;
      end;
    end;

    CM_MOUSEWHEEL:
      if AllowMouseWheel then begin
        SetNewIndex(ItemIndex - TCMMouseWheel(Message).WheelDelta div 120);
        Message.Result := 1;
        Exit;
      end;

    WM_CHAR:
      if ReadOnly or not AllowDropDownList or HandleChar(TWMKey(Message).CharCode) then
        Exit
      else begin
        inherited;
        case TWMKey(Message).CharCode of
          VK_BACK, 0:;
          VK_RETURN, VK_ESCAPE: TWMKey(Message).CharCode := 0;
          else begin
            if IsDropDownList then
              FTextBuffer := FTextBuffer + acChar(TWMKey(Message).CharCode)
            else
              FTextBuffer := Text;

            CheckAutoComplete;
          end;
        end;
        Exit;
      end;

    WM_COMMAND, CN_COMMAND:
      if TWMCommand(Message).NotifyCode = CBN_DROPDOWN then begin
        if ReadOnly or not AllowDropDownList then
          Exit;

        if (SkinData.SkinIndex < 0) and (TWMCommand(Message).NotifyCode = CBN_DROPDOWN) then
          FDroppedDown := True; // Blinking of label avoiding
      end;
  end;
  if not ControlIsReady(Self) or not SkinData.Skinned then begin
    inherited;
  end
  else begin
    case Message.Msg of
      CM_COLORCHANGED, CM_MOUSEWHEEL:
        SkinData.BGChanged := True;

      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT:
        if SkinData.CtrlSkinState and ACS_CHANGING = 0 then
          SkinData.BGChanged := True
        else
          Exit;
    end;
    inherited;
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_DROPPEDDOWN:
            Message.Result := LRESULT(DroppedDown);
        end;

      CB_SETCURSEL: begin
        StopTimer(SkinData);
        SkinData.BGChanged := True;
        Repaint;
      end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);

  if Assigned(FSimpleBox) then
    FSimpleBox.HandleOwnerMsg(Message, Self);
end;


procedure TacCustomCombo.DrawItem(Bmp: TBitmap; Index: integer; R: TRect; DrawState: TOwnerDrawState);
var
  CI: TCacheInfo;
  TranspMode: byte;
  BtnStyle: boolean;
  C: TColor;
  SelRect: TRect;
  OldDC, SavedDC: hdc;
  DrawData: TacItemDrawData;
  sNdx, AState: integer;
begin
  Bmp.Canvas.Font.Assign(Font);
  sNdx := -1; // Selection skin index
  BtnStyle := AllowBtnStyle and (odComboBoxEdit in DrawState);

  TranspMode := GetTransMode(SkinData);
  // State of control
  if (odComboBoxEdit in DrawState) and not (csDesigning in ComponentState) then
    if IsDropDownList then
      if FDroppedDown then
        AState := ACPRESSED
      else
        AState := integer(ControlIsActive(SkinData) or Focused or SkinData.FMouseAbove)
    else
      AState := integer(ControlIsActive(SkinData) or Focused)
  else
    AState := ACNORMAL;

  SelRect := R;
  // Get default colors
  if SkinData.Skinned then begin
    CI := MakeCacheInfo(Bmp, R.Left, R.Top);
    if (odSelected in DrawState) and not BtnStyle then begin
      if odComboBoxEdit in DrawState then begin
        SelRect.Top := 3;
        SelRect.Bottom := Bmp.Height - 3;
        SelRect.Left := max(3, SelRect.Left - 2);
        SelRect.Right := SelRect.Right + 2;
      end;
      sNdx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection];
      if sNdx >= 0 then begin // Paint selection
        C := SkinData.CommonSkinData.gd[sNdx].Props[2].Color;
        PaintItem(sNdx, CI, True, 1, SelRect, MkPoint, Bmp, SkinData.CommonSkinData);
        Bmp.Canvas.Font.Color := SkinData.CommonSkinData.gd[sNdx].Props[2].FontColor.Color;
      end
      else begin
        C := SkinData.SkinManager.GetHighLightColor(True);
        Bmp.Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(True);
      end;
    end
    else begin
      if SkinData.CustomColor then
        C := Color
      else
        if odComboBoxEdit in DrawState then
          C := SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[AState].Color
        else
          C := SkinData.SkinManager.Palette[pcEditBG];

      Bmp.Canvas.Font.Color := GetItemFontColor(SkinData.SkinIndex, AState, DrawState);
    end;
  end
  else // Standard colors
    if (odSelected in DrawState) and not BtnStyle then begin
      C := ColorToRGB(clHighLight);
      Bmp.Canvas.Font.Color := ColorToRGB(clHighLightText);
    end
    else begin
      C := ColorToRGB(Color);
      Bmp.Canvas.Font.Color := ColorToRGB(Font.Color);
    end;

  Bmp.Canvas.Brush.Color := C;
  // Fill if is not selection and not in edit box
  if (sNdx < 0) and not (odComboBoxEdit in DrawState) then
    FillDC(Bmp.Canvas.Handle, SelRect, C);

  // Paint content
  if ItemTextDefined(Index) then begin
    if Assigned(OnDrawItem) and IsOwnerDraw then begin
      OldDC := Canvas.Handle;
      Canvas.Handle := Bmp.Canvas.Handle;
      Bmp.Canvas.Lock;
      SavedDC := SaveDC(Canvas.Handle);
      try
        if not (odComboBoxEdit in DrawState) then
          MovewindowOrg(Canvas.Handle, -R.Left, -R.Top);

        OnDrawItem(Self, Index, R, DrawState);
      finally
        RestoreDC(Canvas.Handle, SavedDC);
        Bmp.Canvas.Unlock;
      end;
      Canvas.Handle := OldDC;
    end
    else
      if Assigned(FOnGetItemDrawData) and (not (odComboBoxEdit in DrawState) or IsDropDownList) or FRequestExtData then begin
        DrawData.Font := Bmp.Canvas.Font;
        DrawData.ItemIndex := Index;
        DrawData.State := DrawState;
        if odComboBoxEdit in DrawState then
          DrawData.Text := GetEditText(DrawData.ItemIndex)
        else
          DrawData.Text := GetText(DrawData.ItemIndex);

        GetItemDrawData(DrawData);
        PaintItemContentEx(sNdx, Bmp, R, DrawState, DrawData);
      end
      else
        PaintItemContent(Index, sNdx, Bmp, R, DrawState, AState);

    if (TranspMode = TM_PARTIAL) and (odComboBoxEdit in DrawState) then
      FillAlphaRect(Bmp, R, MaxByte);
  end
  else // Just a text
    if IsDefaultValue and (TextHint <> '') and (odComboBoxEdit in DrawState) and (not (odFocused in DrawState) or IsDropDownList) then begin // Draw TextHint
      if SkinData.CtrlSkinState and ACS_TEXTANIMATING = 0 then begin
        Bmp.Canvas.Brush.Style := bsClear;
        Bmp.Canvas.Font.Color := TextHintColor(SkinData, AState, TranspMode = TM_FULL);
        AcDrawText(Bmp.Canvas.Handle, TextHint, R, GetTextDrawStyle);
      end;
    end
    else
      if not IsDropDownList then
        inherited PaintText;
end;


function TacCustomCombo.HandleAlphaMsg(var Message: TMessage): Boolean;
begin
  Result := False;
end;


function TacCustomCombo.HandleChar(var AChar: word): boolean;
begin
  Result := False;
  if DroppedDown and (FPopupForm <> nil) then
    case AChar of
      VK_ESCAPE: begin
        Result := True;
        AChar := 0;
        FPopupForm.Close;
      end;
    end;
end;


function TacCustomCombo.HandleKeyDown(var ACharCode: word): Boolean;
begin
  Result := False;
  case ACharCode of
    VK_DOWN:  SetNewIndex(ItemIndex + 1);
    VK_UP:    SetNewIndex(ItemIndex - 1);
    VK_NEXT:  SetNewIndex(max(0, ItemIndex) + DropDownCount - 1);
    VK_PRIOR: SetNewIndex(max(0, ItemIndex) - DropDownCount + 1);
    VK_HOME: if IsDropDownList then begin
      SetNewIndex(0);
      ACharCode := 0;
    end;
    VK_END: if IsDropDownList then begin
      SetNewIndex(Items.Count - 1);
      ACharCode := 0;
    end;
  end;
  if DroppedDown and (FPopupForm <> nil) or (FSimpleBox <> nil) then
    case ACharCode of
      VK_RETURN: begin
        Result := True;
        ACharCode := 0;
        if FPopupForm <> nil then
          FPopupForm.Close;
      end;
      VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR: ACharCode := 0;
    end;
end;


function TacCustomCombo.ImageExists: boolean;
begin
  Result := (Images <> nil) and IsDropDownList;
end;


procedure TacCustomCombo.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    Perform(WM_USER + 2{CBEM_SETIMAGELIST}, 0, TCustomImageList(Sender).Handle);
end;


function TacCustomCombo.IsCaseSensitive: boolean;
begin
  Result := False;
end;


function TacCustomCombo.IsDefaultValue: boolean;
begin
  if IsDropDownList or (Text = '') then
    Result := ItemIndex < 0
  else
    Result := False;
end;


function TacCustomCombo.IsFullPaint: boolean;
begin
  Result := IsDropDownList;
end;


function TacCustomCombo.ItemDrawState: TOwnerDrawState;
begin
  Result := [odComboBoxEdit];
  if Focused then begin
    Result := Result + [odFocused];
    if IsDropDownList and (SkinData.SkinSection <> '') then
      Result := Result + [odSelected];
  end;
end;


procedure TacCustomCombo.ItemsChanged;
begin
  if FSimpleBox <> nil then
    FSimpleBox.Items.Assign(Items)
  else
    if DroppedDown and (FPopupForm <> nil) then begin
      FPopupForm.UpdateControls;
      FPopupForm.UpdateBoxSize;
    end;
end;


function TacCustomCombo.ItemTextDefined(AIndex: integer): boolean;
begin
  Result := IsValidIndex(AIndex, GetItemCount);
end;


procedure TacCustomCombo.UpdateButtonMode;
begin
  ButtonMode := IsDropDownList;
end;


procedure TacCustomCombo.UpdateForm;
begin
  if FPopupForm <> nil then begin
    FPopupForm.UpdateControls;
    FPopupForm.UpdateBoxSize;
  end;
end;


procedure TacCustomCombo.UpdateIndexes;
begin
  with SkinData do
    if Skinned then
      FGlyphIndex := SkinManager.GetMaskIndex(SkinIndex, s_ItemGlyph, SkinData.CommonSkinData)
    else
      FGlyphIndex := -1;
end;


procedure TacCustomCombo.UpdateText;
var
  FSavedIndex: integer;
begin
  FItemChanging := True;
  if IsValidIndex(FItemIndex, GetItemCount) then begin
    FSavedIndex := FItemIndex;
    if MakeTextPlain then
      Text := GetPlainText(Items[FItemIndex])
    else
      Text := Items[FItemIndex];

    FItemIndex := FSavedIndex;
    if Focused then
      SelectAll;
  end
  else
    Text := '';

  FItemChanging := False;
end;


procedure TacItemsPopupForm.LimitScroll;
var
  i: integer;
begin
  if IsValidIndex(FItemIndex, Items.Count) then begin
    TsSpeedButton(Items[FItemIndex]).SkinData.Invalidate(True);
    if FItemIndex >= 0 then begin // If index is correct still
      i := MaxInt;
      if TsSpeedButton(Items[FItemIndex]).Top > sb.Height - TsSpeedButton(Items[FItemIndex]).Height then
        i := TsSpeedButton(Items[FItemIndex]).Top - (sb.Height - TsSpeedButton(Items[FItemIndex]).Height)
      else
        if TsSpeedButton(Items[FItemIndex]).Top < 0 then
          i := TsSpeedButton(Items[FItemIndex]).Top;

      if i <> MaxInt then begin
        sb.VertScrollBar.Position := sb.VertScrollBar.Position + i;
        UpdateScrolls(sb.ListSW, True);
      end;
    end;
  end;
end;


constructor TacItemsPopupForm.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  FItemIndex := -1;
  FItems := TList.Create;
  sp := TsSkinProvider.Create(Self);
  sp.AllowAnimation := False;
  sp.Name := 'PopupForm' + IntToStr(GetTickCount);


  AutoScroll := False;
  Position := poDesigned;
  UpdateFlags := UpdateFlags + [ufUpdateThumbs];
  FOwner := TacCustomCombo(AOwner);
  BiDiMode := FOwner.BiDiMode;
  BorderStyle := bsNone;
  BorderWidth := 0;
  Scaled := False;

  Panel := TacPopupPanel.Create(Self);
  Panel.SetBounds(0, 0, Width, Height);
  Panel.BorderWidth := 1;
  Panel.BevelOuter := bvNone;
  Panel.Ctl3D := True;
  Panel.SkinData.SkinSection := s_Edit;

  sb := TacPopupScrollBox.Create(Self);
  with sb do begin
    Color := ColorToRGB(clWindow);
    FOwner := Self;
    BorderStyle := bsNone;
    AutoScroll := False;
    AutoMouseWheel := True;
    SkinData.VertScrollData.ButtonsSize := 0;
    Align := alClient;
    Ctl3D := False;
    sb.VertScrollBar.Tracking := True;
    SkinData.SkinSection := s_Transparent;
    Parent := Panel;
  end;
  Panel.Parent := Self;
  OnClose := HandleOnClose;
end;


procedure TacItemsPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
end;


destructor TacItemsPopupForm.Destroy;
begin
  inherited;
  FItems.Free;
  if FOwner <> nil then begin
    FOwner.FPopupForm := nil;
    FOwner.FDroppedDown := False;
  end;
end;


procedure TacItemsPopupForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  FOwner.DoCloseUp;
end;


procedure TacItemsPopupForm.HandleOnClose(Sender: TObject; var Action: TCloseAction);
begin
  sp.AllowAnimation := False;
  Action := caFree;
  FOwner.FPopupWindow := nil;
  FOwner.FPopupForm := nil;
  FOwner.FDroppedDown := False;
  FOwner.BtnState := integer(FOwner.MouseInBtn);
end;


type
  TAccessControl = class(TWinControl);


procedure TacItemsPopupForm.InitFormData;
var
  iActualRowCount: integer;
begin
  UpdateFlags := UpdateFlags + [ufFormInitializing];
{$IFDEF DELPHI_10BERLIN}
  if (FOwner.SkinData.SkinManager <> nil) and (FOwner.SkinData.SkinManager.Options.ScaleMode <> smVCL) then
    TAccessControl(Self).FCurrentPPI := 96; // Do not scale automatically
{$ENDIF}
  Font.Assign(FOwner.Font);
  ItemWidth := FOwner.MinPopupWidth;
  if FOwner.ItemHeight <= 0 then begin
    ItemHeight := GetFontHeight(FOwner.Font.Handle, True) + ScaleInt(5, FOwner.SkinData);
    if FOwner.Images <> nil then
      ItemHeight := max(ItemHeight, GetImageHeight(FOwner.Images, -1, FOwner.SkinData.CurrentPPI) + FOwner.SkinData.CommonSkinData.Spacing);
  end
  else begin
    ItemHeight := FOwner.ItemHeight;
    if ufUpdateScale in UpdateFlags then
      ItemHeight := ScaleInt(ItemHeight, FOwner.SkinData);
  end;

  ItemCount := FOwner.GetItemCount;
  iActualRowCount := min(FOwner.DropDownCount, ItemCount);
  sb.WheelDelta := 120 * ItemHeight div 2;

  Height := 2 + max(1, iActualRowCount) * ItemHeight;
  UpdateFlags := UpdateFlags - [ufUpdateScale];
  UpdateFlags := UpdateFlags - [ufFormInitializing];
end;


procedure TacItemsPopupForm.SetItemIndex(const Value: integer);
var
  OldIndex: integer;
begin
  if FItemIndex <> Value then begin
    OldIndex := FItemIndex;
    FItemIndex := Value;
    if IsValidIndex(OldIndex, Items.Count) then
      TsSpeedButton(Items[OldIndex]).SkinData.Invalidate(True);

    LimitScroll;
  end;
end;


procedure TacItemsPopupForm.UpdateBoxSize;
begin
  if FOwner.MinPopupWidth <= 0 then
    Width := FOwner.Width
  else
    Width := max(FOwner.Width, ScaleInt(FOwner.MinPopupWidth, FOwner.SkinData));

  Panel.SetBounds(0, 0, Width, Height);
  sb.Width := Panel.ClientWidth;
end;


procedure TacItemsPopupForm.UpdateControls;
var
  i, y: integer;

  procedure AddItem(const ACaption: string);
  var
    n: integer;
  begin
    n := Items.Add(TacPopupBtn.Create(Self));
    with TacPopupBtn(Items[n]) do begin
      Transparent := False;
      FOwnerForm := Self;
      AnimatEvents := [];
      Flat := True;
      Alignment := taLeftJustify;
      Align := alTop;
      Caption := ACaption;
      SetBounds(0, y, sb.Width, ItemHeight);
      Tag := i;
      SkinData.SkinSection := s_Transparent;
      TextOffset := SkinData.CommonSkinData.Spacing;
      Parent := sb;
      inc(y, ItemHeight);
    end;
  end;

begin
  y := 0;
  InitFormData;
  UpdateBoxSize;
  // Update items
  for i := 0 to Items.Count - 1 do
    TacPopupBtn(Items[i]).Free;

  Items.Clear;
  for i := 0 to FOwner.GetItemCount - 1 do
    AddItem(FOwner.Items[i]);

  // Update scroll
  sb.VertScrollBar.Range := FOwner.GetItemCount * ItemHeight;
end;


procedure TacItemsPopupForm.UpdateHotControl;
begin
  if FOwner.ItemIndex >= 0 then
    ItemIndex := FOwner.ItemIndex
  else
    ItemIndex := FOwner.Items.IndexOf(FOwner.Text);
end;


procedure TacItemsPopupForm.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    CM_RELEASE:
      Close;
  end;
  inherited;
end;


function TacPopupPanel.PrepareCache: Boolean;
begin
  Result := inherited PrepareCache;
  if Result then
    FillDCBorder(SkinData.FCacheBmp.Canvas.Handle, MkRect(SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height), 1, 1, 1, 1, acColorToRGB(clWindowFrame));
end;


function TacPopupBtn.CanShiftContent: boolean;
begin
  Result := False;
end;


constructor TacPopupBtn.Create(AOwner: TComponent);
begin
  inherited;
  FPressed := False;
end;


procedure TacPopupBtn.DrawCaption;
var
  AState: TOwnerDrawState;
begin
  if (Tag = FOwnerForm.FItemIndex) or (CurrentState > 0) then
    AState := [odSelected]
  else
    AState := [];

  FOwnerForm.FOwner.DrawItem(SkinData.FCacheBmp, Tag, MkRect(SkinData.FCacheBmp), AState);
end;


function TacPopupBtn.PrepareCache: boolean;
begin
  if (inherited CurrentState > 0) and (FOwnerForm.ItemIndex >= 0) then
    FOwnerForm.ItemIndex := -1;

  if SkinData.Skinned then
    if CurrentState > 0 then begin
      if SkinData.SkinIndex <> SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection] then begin
        SkinData.FUpdating := True;
        SkinData.SkinSection := s_Selection;
        SkinData.FUpdating := False;
      end;
    end
    else begin
      if SkinData.SkinIndex <> SkinData.SkinManager.SkinCommonInfo.Sections[ssTransparent] then begin
        SkinData.FUpdating := True;
        SkinData.SkinSection := s_Transparent;
        SkinData.FUpdating := False;
      end;
    end
  else
    SkinData.SkinIndex := -1;

  Result := inherited PrepareCache;
end;


procedure TacPopupBtn.SelectItem;
var
  bIsDefValue: boolean;
  cBox: TacCustomCombo;
begin
  cBox := FOwnerForm.FOwner;
  if cBox <> nil then begin
    bIsDefValue := (cBox.ItemIndex < 0) and cBox.IsDropDownList or cBox.IsDefaultValue;

    GetIntController.ClosingForbide(FOwnerForm); // Do not free in the TacFormHandler.CloseForm
    ShowWindow(FOwnerForm.Handle, SW_HIDE); // Hide immediately (DroppedDown will be changed to False)

    StopTimer(cBox.SkinData);
    FreeAndNil(cBox.SkinData.AnimTimer);
    cBox.SkinData.CtrlSkinState := cBox.SkinData.CtrlSkinState or ACS_INIT; // Do not call OnChange
    cBox.ItemIndex := Tag;
    cBox.SkinData.CtrlSkinState := cBox.SkinData.CtrlSkinState and not ACS_INIT;
    cBox.Select;

    if not (csDestroying in FOwnerForm.ComponentState) then
      PostMessage(FOwnerForm.Handle, CM_RELEASE, 0, 0);

    GetIntController.ClosingAllow(FOwnerForm); // Call it after Select (OnChange), prevent closing by user code
    if cBox.LabelFromTextHint and bIsDefValue then begin
      cBox.FArrowAngle := 0;
      MakeLabelTextHint(cBox.SkinData, cBox.BoundLabel, cBox.TextHint);
    end;
  end;
end;


procedure TacPopupBtn.StdPaint(PaintButton: boolean);
var
  PaintRect: TRect;
  TmpCanvas: TCanvas;
begin
  if (inherited CurrentState > 0) and (FOwnerForm.ItemIndex >= 0) then
    FOwnerForm.ItemIndex := -1;

  InitCacheBmp(SkinData);
  TmpCanvas := SkinData.FCacheBmp.Canvas;
  FState := bsUp;
  TmpCanvas.Font.Assign(Font);
  InitParams;
  PaintRect := ClientRect;
  if inherited CurrentState > 0 then
    FillDC(TmpCanvas.Handle, PaintRect, ColorToRGB(clHighlight))
  else
    FillDC(TmpCanvas.Handle, PaintRect, ColorToRGB(clWindow));

  DrawCaption;
  DrawGlyph;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, TmpCanvas.Handle, 0, 0, SRCCOPY);
end;


procedure TacPopupBtn.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN: begin
      FPressed := True;
      inherited;
    end;

    WM_LBUTTONUP:
      if FPressed and acMouseInControl(Self) then begin
        FPressed := False;
        Message.Result := -1;
        SelectItem;
        Parent.Perform(Message.Msg, Message.WParam, Message.LParam); // Solving of issue with non-shown hints after select
      end
      else
        inherited;

    else
      inherited;
  end;
end;


procedure TacPopupPanel.WMNCPaint(var Message: TWMNCPaint);
var
  DC: hdc;
begin
  DC := GetWindowDC(Handle);
  try
    FillDCBorder(DC, MkRect(Self), 1, 1, 1, 1, acColorToRGB(clWindowFrame))
  finally
    ReleaseDC(Handle, DC);
  end;
end;


function TacPopupScrollBox.CanFocus: Boolean;
begin
  Result := False;
end;


function TsCustomComboBox.AllowBtnStyle: Boolean;
begin
  Result := (Style in [csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable]) and ((SkinData.SkinSection = '') or not SkinData.Skinned);
end;


function TsCustomComboBox.CanCreateListBox: boolean;
begin
  Result := HandleAllocated and (Style = csSimple);
end;


function TsCustomComboBox.CanDropDown: boolean;
begin
  Result := Style <> csSimple;
end;


function TsCustomComboBox.CanShowButton: boolean;
begin
  if Style = csSimple then
    Result := False
  else
    Result := inherited CanShowButton;
end;


procedure TsCustomComboBox.Change;
begin
  if not (csLoading in ComponentState) then
    inherited;
end;


function TsCustomComboBox.CheckAutoComplete: boolean;
begin
{$IFDEF DELPHI7UP}
  if AutoComplete then begin
    Result := inherited CheckAutoComplete;
    if Result and AutoDropDown then
      if not DroppedDown or (FPopupForm = nil) then
        AdjustDropDown
      else
        FPopupForm.ItemIndex := Items.IndexOf(Text);
  end
  else
{$ENDIF} // DELPHI7UP
    Result := False;
end;


procedure TsCustomComboBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_DBLCLK:
      DblClick;

    EN_CHANGE: begin
      if not (csLoading in ComponentState) and (FItemIndex >= 0) and (Text <> GetText(FItemIndex)) then
        FItemIndex := -1;

      if FItemChanging then // Do not call the OnChange event
        Exit;
    end
  end;
  inherited;
end;


constructor TsCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoComplete := True;
  FAutoCompleteDelay := 500;
  FSorted := False;
  FAutoCloseUp := False;
  FAutoDropDown := False;
  FItems := TacComboBoxStrings.Create;
  TacComboBoxStrings(FItems).OnChange := DoItemsChanged;
  TacComboBoxStrings(FItems).OnCleared := ItemsCleared;
end;


destructor TsCustomComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;


procedure TsCustomComboBox.DoItemsChanged(Sender: TObject);
begin
  ItemsChanged;
end;


function TsCustomComboBox.GetEditText(AItemIndex: integer): acString;
begin
  if IsValidIndex(AItemIndex, Items.Count) then
    if MakeTextPlain then
      Result := GetPlainText(Items[AItemIndex])
    else
      Result := Items[AItemIndex]
  else
    Result := Text;
end;


function TsCustomComboBox.HandleChar(var AChar: word): boolean;
begin
  Result := inherited HandleChar(AChar);
  if DroppedDown and (FPopupForm <> nil) then
    case AChar of
      VK_TAB:
        if not AutoCloseUp and DroppedDown then begin
          Result := True;
          AChar := 0;
        end;
    end;
end;


function TsCustomComboBox.HandleKeyDown(var ACharCode: word): boolean;
begin
  Result := inherited HandleKeyDown(ACharCode);
  if DroppedDown and (FPopupForm <> nil) then
    case ACharCode of
      VK_TAB:
        if not AutoCloseUp and DroppedDown then begin
          Result := True;
          ACharCode := 0;
        end;
    end;
end;


function TsCustomComboBox.IndexOf(const s: acString): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items[i] = s then begin
      Result := i;
      Exit;
    end;
end;


function TsCustomComboBox.IsDropDownList: boolean;
begin
  Result := Style in [csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable];
end;


function TsCustomComboBox.IsOwnerDraw: boolean;
begin
  Result := Style in [csOwnerDrawFixed, csOwnerDrawVariable];
end;


procedure TsCustomComboBox.ItemsCleared(Sender: TObject);
begin
  if [csLoading, csReading, csDestroying] * ComponentState = [] then begin
    ItemIndex := -1; // Reset with repainting
  end;
end;


function TsCustomComboBox.GetItemCount: integer;
begin
  Result := Items.Count;
end;


procedure TacCustomCombo.SetItems(const Value: TStrings);
begin
  if Assigned(FItems) then
    FItems.Assign(Value)
  else
    FItems := Value;
end;


procedure TsCustomComboBox.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then begin
    FSorted := Value;
    TStringList(Items).Sorted := Value;
  end;
end;


procedure TsCustomComboBox.SetStyle(const Value: TComboBoxStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    if FStyle = csSimple then begin
      AutoSize := False;
	    CreateListBox;
  	end
    else begin
      if FSimpleBox <> nil then begin
        FreeAndNil(FSimpleBox);
        AutoSize := True;
      end;
    end;

    UpdateButtonMode;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomComboBox.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    CM_TEXTCHANGED:
      if not (csLoading in ComponentState) and (FItemIndex >= 0) and (Text <> GetText(FItemIndex)) then begin
        FItemIndex := -1;
      end;
  end;
  inherited;
end;


function TacComboBoxStrings.Add(const S: string): Integer;
begin
  Result := inherited Add(StrPas(PChar(S)));
end;


function TacComboBoxStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := inherited AddObject(StrPas(PChar(S)), AObject);
end;


procedure TacComboBoxStrings.Clear;
begin
  inherited;
  if (Count = 0) and Assigned(OnCleared) then
    OnCleared(Self);
end;


constructor TacComboBoxStrings.Create;
begin
  inherited Create;
  OnCleared := nil;
end;


procedure TacSimpleBox.AlignBox;
begin
  if ([csLoading] * ComponentState = []) and not DsgnPressed then begin
    Parent := Owner.Parent;
    if Parent <> nil then begin
      Parent.DisableAlign;
      Width := Owner.Width;
      Height := Owner.Height - Owner.GetAutoHeight;
      Left := Owner.Left;
      Top := Owner.Top + Owner.GetAutoHeight;
      Parent.EnableAlign;
      BringToFront;
    end;
  end;
end;


function TacSimpleBox.CanFocus: boolean;
begin
  Result := False;
end;


procedure TacSimpleBox.Change;
begin
  Owner.ItemIndex := ItemIndex;
  if Owner.CanFocus then
    Owner.SetFocus;
end;


constructor TacSimpleBox.Create(AOwner: TComponent);
begin
  inherited;
  DsgnPressed := False;
  Owner := TacCustomCombo(AOwner);
  TabStop := False;
end;


procedure TacSimpleBox.HandleOwnerMsg(const Message: TMessage; const OwnerControl: TControl);
begin
  case Message.Msg of
    WM_SIZE, WM_WINDOWPOSCHANGED, WM_MOVE:
      AlignBox;

    CM_ENABLEDCHANGED: begin
      Enabled := Owner.Enabled;
      AlignBox;
    end;

    CM_BIDIMODECHANGED: begin
      BiDiMode := Owner.BiDiMode;
      AlignBox;
    end;

    CM_VISIBLECHANGED, CM_SHOWINGCHANGED: begin
      Visible := Owner.Visible or (csDesigning in ComponentState);
      AlignBox;
    end;

    CM_PARENTFONTCHANGED: begin
      if ParentFont then begin
        if Parent <> nil then
          Font.Assign(TAccessControl(Parent).Font);

        ParentFont := True;
      end;
      Visible := Owner.Visible or (csDesigning in ComponentState);
      AlignBox;
    end;
  end;
end;


procedure TacSimpleBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN:
      if csDesigning in ComponentState then
        Exit;

    CM_HITTEST:
      if csDesigning in ComponentState then begin
        Message.Result := HTNOWHERE;
        Exit;
      end;
  end;
  inherited;
end;

end.
