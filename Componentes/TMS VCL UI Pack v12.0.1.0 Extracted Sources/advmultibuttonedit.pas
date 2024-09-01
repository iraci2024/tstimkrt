{**************************************************************************}
{ TAdvMultiButtonEdit component                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2013 - 2023                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                      }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvMultiButtonEdit;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, StdCtrls, AdvEdit, Buttons, ImgList, Dialogs,
  PngImage, Forms, Messages, Graphics, AdvStyleIF
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with setting Enabled at runtime
  // v1.0.0.2 : Fixed : Issue with OnEnter event
  // v1.0.0.3 : Fixed : Issue with displaying disabled buttons at design-time
  // v1.0.0.4 : Fixed : Issue with semi transparent button images
  // v1.1.0.0 : New : Label added
  //          : New : TAdvTouchSpinEdit control added
  // v1.1.1.0 : New : ButtonWidth property added
  //          : Fixed : Issue with use with VCL styles
  // v1.1.2.0 : Improved : Label positioning in high DPI
  // v1.2.0.0 : Improved : VCL Styles support
  // v1.2.0.1 : Improved : VCL Styles handling
  // v1.2.0.2 : Fixed : Issue with label position when used on container controls
  // v1.2.0.3 : Improved : High DPI handling for buttons
  // v1.2.0.4 : Fixed : Issue with signed input for TAdvUpDownEdit
  // v1.2.1.0 : New : StyleElements property added
  // v1.2.1.1 : Fixed : Issue with OnChange handling
  // v1.2.1.2 : Improved : Additional button painting when StyleElements is set to []
  // v1.2.2.0 : New : UIStyle property for Office 2019 look and button colors can be adjusted
  // v1.3.0.0 : New : EditAlign and EditType properties added
  // v1.3.0.1 : Fixed : Images change between black and white according to button text color.
  //          : Fixed : VCL styles check updated to work in 10.4 Sydney
  // v1.3.1.0 : New : BorderColor property added
  // v1.3.1.1 : Fixed : Issue with OnClickSub not exposed
  // v1.3.1.2 : Fixed : Issue with border appearance when BorderStyle = bsNone
  // v1.3.1.3 : Improved : Label font size initialization at design-time for high DPI
  // v1.3.1.4 : Improved : LabelFont handling in connection with ParentFont
  // v1.3.1.5 : Fixed : Label visibility when control visibility changes
  // v1.3.2.0 : New : Exposed properties EmptyTextStyle, Precision, PrecisionDisplay, Prefix, Suffix, ReturnIsTab, AllowNumericNullValue, FloatValue, MaxFloatValue, MinFloatValue
  //          : Fixed : Issue with setting Button.Style back to bsCustom

type
  TPNGSpeedButton = class(TSpeedButton)
  private
    FHot: Boolean;
    FPNGName: string;
    FImages: TCustomImageList;
    FImageIndexInt: TImageIndex;
    FCaption: string;
    procedure SetPNGName(const Value: string);
    procedure SetCaption(const Value: string);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
  protected
    procedure Paint; override;
  public
    property Images: TCustomImageList read FImages write FImages;
    property ImageIndexInt: TImageIndex read FImageIndexInt write FImageIndexInt;
  published
    property Caption: string read FCaption write SetCaption;
    property PNGName: string read FPNGName write SetPNGName;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
  end;

  TAdvMultiButtonEdit = class;

  TButtonPosition = (bpLeft, bpRight);

  TButtonStyle = (bsClear,bsFind,bsOK,bsTrash,bsAccept,bsDeny,bsClose,bsCopy,bsPrevious,bsNext,bsUndo,bsAdd,bsSub,bsCustom);

  TEditButton = class(TCollectionItem)
  private
    FButtonPosition: TButtonPosition;
    FButton: TPNGSpeedButton;
    FEnabled: boolean;
    FFlat: boolean;
    FStyle: TButtonStyle;
    FImageIndex: TImageIndex;
    FHint: string;
    FCaption: string;
    procedure SetButtonPosition(const Value: TButtonPosition);
    procedure SetEnabled(const Value: boolean);
    procedure SetFlat(const Value: boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetHint(const Value: string);
    procedure SetCaption(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Button: TPNGSpeedButton read FButton;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Flat: boolean read FFlat write SetFlat default false;
    property Hint: string read FHint write SetHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Position: TButtonPosition read FButtonPosition write SetButtonPosition default bpRight;
    property Style: TButtonStyle read FStyle write SetStyle default bsCustom;
  end;

  TEditButtons = class(TOwnedCollection)
  private
    FEdit: TAdvMultiButtonEdit;
    function GetItem(Index: Integer): TEditButton;
    procedure SetItem(Index: Integer; const Value: TEditButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TEditButton read GetItem write SetItem; default;
    function Add: TEditButton;
    function Insert(Index: Integer): TEditButton;
    function FindButton(AStyle: TButtonStyle): TEditButton;
  end;

  TButtonClickEvent = procedure(Sender: TObject; ButtonIndex: integer) of object;

  TAdvCustomMultiButtonEdit = class(TCustomControl, ITMSStyle)
  private
    FDesignTime: Boolean;
    FEdit: TAdvEdit;
    FEditButtons: TEditButtons;
    FImages: TCustomImageList;
    FOnClickOK: TNotifyEvent;
    FOnClickCustom: TButtonClickEvent;
    FOnClickClear: TNotifyEvent;
    FOnClickFind: TNotifyEvent;
    FOnClickDeny: TNotifyEvent;
    FOnClickClose: TNotifyEvent;
    FOnClickTrash: TNotifyEvent;
    FOnClickAccept: TNotifyEvent;
    FOnClickCopy: TNotifyEvent;
    FOnClickPrevious: TNotifyEvent;
    FOnClickNext: TNotifyEvent;
    FOnClickUndo: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnClickAdd: TNotifyEvent;
    FOnClickSub: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FOnDblClick: TNotifyEvent;
    FEditColor: TColor;
    FCharCase: TEditCharCase;
    FHideSelection: Boolean;
    FMaxLength: integer;
    FReadOnly: boolean;
    FEmptyText: string;
    FEmptyTextFocused: boolean;
    FLabelMargin: Integer;
    FLabelPosition: TLabelPosition;
    FLabelAlwaysEnabled: Boolean;
    FLabelTransparent: Boolean;
    FLabelFont: TFont;
    FLabel: TCustomAdvEditLabel;
    FParentFnt: boolean;
    FFocusLabel: boolean;
    FButtonWidth: integer;
    FTMSStyle: TTMSStyle;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonBorderColor: TColor;
    FButtonColorDown: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonTextColorDown: TColor;
    FButtonColorDisabled: TColor;
    FButtonTextColorDisabled: TColor;
    FEditAlign: TEditAlign;
    FEditType: TAdvEditType;
    FBorderColor: TColor;
    FDesignCreate: boolean;
    FMinValue: Longint;
    FReturnIsTab: boolean;
    FPrecision: smallint;
    FPrecisionDisplay: TPrecisionDisplay;
    FLeadingZeros: boolean;
    FEmptyTextStyle: TFontStyles;
    FMaxFloatValue: double;
    FMaxValue: Longint;
    FPrefix: string;
    FMinFloatValue: double;
    FSuffix: string;
    FAllowNumericNullValue: boolean;
    procedure SetText(const Value: string);
    procedure SetButtons(const Value: TEditButtons);
    function GetText: string;
    function GetVersion: string;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure SetEditColor(const Value: TColor);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: integer);
    procedure SetReadOnly(const Value: boolean);
    procedure SetEmptyText(const Value: string);
    procedure SetEmptyTextFocused(const Value: boolean);
    function GetLabelCaption: string;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelMargin(const Value: Integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: Boolean);
    procedure SetButtonWidth(const Value: integer);
    procedure SetUIStyle(const Value: TTMSStyle);
    procedure SetEditAlign(const Value: TEditAlign);
    procedure SetEditType(const Value: TAdvEditType);
    procedure SetBorderColor(const Value: TColor);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
    procedure SetLeadingZeros(const Value: boolean);
    procedure SetMaxFloatValue(const Value: double);
    procedure SetMaxValue(const Value: Longint);
    procedure SetMinFloatValue(const Value: double);
    procedure SetMinValue(const Value: Longint);
    procedure SetPrecision(const Value: smallint);
    procedure SetPrecisionDisplay(const Value: TPrecisionDisplay);
    procedure SetPrefix(const Value: string);
    procedure SetReturnIsTab(const Value: boolean);
    procedure SetSuffix(const Value: string);
    procedure SetAllowNumericNullValue(const Value: boolean);
    function GetFloatValue: double;
    procedure SetFloatValue(const Value: double);
  protected
    {$IFDEF DELPHIXE11_LVL}
    procedure SetStyleElements(const Value: TStyleElements); override;
    {$ENDIF}
    procedure ButtonClick(Sender: TObject); virtual;
    procedure DoClickButton(ButtonIndex: integer); virtual;
    procedure DoClickSub; virtual;
    procedure DoClickAdd; virtual;
    procedure DoClickFind; virtual;
    procedure DoClickClear; virtual;
    procedure DoClickOK; virtual;
    procedure DoClickClose; virtual;
    procedure DoClickCopy; virtual;
    procedure DoClickAccept; virtual;
    procedure DoClickDeny; virtual;
    procedure DoClickTrash; virtual;
    procedure DoClickNext; virtual;
    procedure DoClickPrevious; virtual;
    procedure DoClickUndo; virtual;
    procedure DoEditChange(Sender: TObject); virtual;
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeypress(Sender: TObject; var Key: Char); virtual;
    procedure DoEditEnter(Sender: TObject); virtual;
    procedure DoEditExit(Sender: TObject); virtual;
    procedure DoEditDblClick(Sender: TObject); virtual;
    procedure DoEditClick(Sender: TObject); virtual;
    procedure CreateWnd; override;
    function CreateLabel: TCustomAdvEditLabel;
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure LabelFontChanged(Sender: TObject);
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    function CreateButtons: TEditButtons; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure UpdateButtons;
    {$IFNDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    property Buttons: TEditButtons read FEditButtons write SetButtons;
    property OnClickAdd: TNotifyEvent read FOnClickAdd write FOnClickAdd;
    property OnClickSub: TNotifyEvent read FOnClickSub write FOnClickSub;
    property OnClickFind: TNotifyEvent read FOnClickFind write FOnClickFind;
    property OnClickClear: TNotifyEvent read FOnClickClear write FOnClickClear;
    property OnClickOK: TNotifyEvent read FOnClickOK write FOnClickOK;
    property OnClickTrash: TNotifyEvent read FOnClickTrash write FOnClickTrash;
    property OnClickAccept: TNotifyEvent read FOnClickAccept write FOnClickAccept;
    property OnClickDeny: TNotifyEvent read FOnClickDeny write FOnClickDeny;
    property OnClickClose: TNotifyEvent read FOnClickClose write FOnClickClose;
    property OnClickCopy: TNotifyEvent read FOnClickCopy write FOnClickCopy;
    property OnClickNext: TNotifyEvent read FOnClickNext write FOnClickNext;
    property OnClickPrevious: TNotifyEvent read FOnClickPrevious write FOnClickPrevious;
    property OnClickUndo: TNotifyEvent read FOnClickUndo write FOnClickUndo;
    property OnClickCustom: TButtonClickEvent read FOnClickCustom write FOnClickCustom;
    property UIStyle: TTMSStyle read FTMSStyle write SetUIStyle default tsCustom;
    property AllowNumericNullValue: boolean read FAllowNumericNullValue write SetAllowNumericNullValue default false;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonColorDisabled: TColor read FButtonColorDisabled write FButtonColorDisabled default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonTextColorDisabled: TColor read FButtonTextColorDisabled write FButtonTextColorDisabled default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;
    property EditAlign: TEditAlign read FEditAlign write SetEditAlign default eaLeft;
    property EditType: TAdvEditType read FEditType write SetEditType default etString;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 0;
    property MinFloatValue: double read FMinFloatValue write SetMinFloatValue;
    property MaxFloatValue: double read FMaxFloatValue write SetMaxFloatValue;
    property EmptyTextStyle: TFontStyles read FEmptyTextStyle write SetEmptyTextStyle;
    property LeadingZeros: boolean read FLeadingZeros write SetLeadingZeros default true;
    property Precision: smallint read FPrecision write SetPrecision default 0;
    property PrecisionDisplay: TPrecisionDisplay read FPrecisionDisplay write SetPrecisionDisplay default pdNormal;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property ReturnIsTab: boolean read FReturnIsTab write SetReturnIsTab default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetVersionNr: integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property FloatValue: double read GetFloatValue write SetFloatValue;
    property Edit: TAdvEdit read FEdit;
    property Text: string read GetText write SetText;
    property MaxLength: integer read FMaxLength write SetMaxLength default 0;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
  published
    property Align;
    property Anchors;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 22;
    property Color;
    property Constraints;
    property BiDiMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DragCursor;
    property DragMode;
    property DragKind;
    property EditColor: TColor read FEditColor write SetEditColor default clWindow;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write SetEmptyTextFocused default false;
    property Enabled;
    property FocusLabel: boolean read FFocusLabel write FFocusLabel default false;
    property Font;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default true;
    property Hint;
    property Images: TCustomImageList read FImages write FImages;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ParentCtl3D;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default false;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE11_LVL}
    property StyleElements;
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Version: string read GetVersion;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

  TAdvMultiButtonEdit = class(TAdvCustomMultiButtonEdit)
  published
    property AllowNumericNullValue;
    property Buttons;
    property CharCase;
    property EmptyTextStyle;
    property LeadingZeros;
    property MaxLength;
    property MinValue;
    property MaxValue;
    property MinFloatValue;
    property MaxFloatValue;
    property Precision;
    property PrecisionDisplay;
    property Prefix;
    property Suffix;
    property ReturnIsTab;
    property Text;
    property OnClickAdd;
    property OnClickFind;
    property OnClickClear;
    property OnClickOK;
    property OnClickTrash;
    property OnClickAccept;
    property OnClickDeny;
    property OnClickClose;
    property OnClickCopy;
    property OnClickNext;
    property OnClickPrevious;
    property OnClickUndo;
    property OnClickCustom;
    property OnClickSub;
    property UIStyle;
    property ButtonColor;
    property ButtonColorHot;
    property ButtonColorDown;
    property ButtonColorDisabled;
    property ButtonTextColor;
    property ButtonTextColorHot;
    property ButtonTextColorDown;
    property ButtonBorderColor;
    property ButtonTextColorDisabled;
    property EditAlign;
    property EditType;
  end;

  TSpinButtonSettings = class(TPersistent)
  private
    FHint: string;
    FCaption: string;
    FImageIndex: integer;
    FOnChanged: TNotifyEvent;
    procedure SetCaption(const Value: string);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: integer);
  protected
    procedure Changed;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
  published
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
  end;

  TAdvTouchSpinEdit = class(TAdvCustomMultiButtonEdit)
  private
    FValue: integer;
    FEditAlign: TEditAlign;
    FMax: integer;
    FMin: integer;
    FStep: integer;
    FSpinLeft: TSpinButtonSettings;
    FSpinRight: TSpinButtonSettings;
    procedure SetValue(const AValue: integer);
    procedure SetEditAlign(const AValue: TEditAlign);
    procedure SetMax(const AValue: integer);
    procedure SetMin(const AValue: integer);
    procedure SetSpinLeft(const Value: TSpinButtonSettings);
    procedure SetSpinRight(const Value: TSpinButtonSettings);
    procedure UpdateEnabledState; virtual;
  protected
    procedure DoEditChange(Sender: TObject); override;
    procedure DoEditExit(Sender: TObject); override;
    procedure DoClickButton(ButtonIndex: integer); override;
    procedure DoSpinChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EditAlign: TEditAlign read FEditAlign write SetEditAlign default eaCenter;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property SpinLeft: TSpinButtonSettings read FSpinLeft write SetSpinLeft;
    property SpinRight: TSpinButtonSettings read FSpinRight write SetSpinRight;
    property Step: integer read FStep write FStep default 1;
    property Value: integer read FValue write SetValue;
  end;


implementation

uses
  SysUtils, Clipbrd
{$IFDEF DELPHIXE2_LVL}
  , VCL.Themes
{$ENDIF}
  ;

type
  TSpeedButtonCrack = class(TPNGSpeedButton);

{ TAdvMultiButtonEdit }


{$IFDEF DELPHIXE11_LVL}
procedure TAdvCustomMultiButtonEdit.SetStyleElements(const Value: TStyleElements);
var
  i: integer;
begin
  inherited;
  FEdit.StyleElements := StyleElements;
  for i := 0 to Buttons.Count - 1 do
    Buttons[i].Button.StyleElements := StyleElements;
end;
{$ENDIF}


procedure TAdvCustomMultiButtonEdit.ButtonClick(Sender: TObject);
var
  sp: TPNGSpeedButton;
begin
  if (Sender is TPNGSpeedButton) then
  begin
    sp := (Sender as TPNGSpeedButton);

    case Buttons[sp.Tag].Style of
    bsClear: DoClickClear;
    bsFind: DoClickFind;
    bsOK: DoClickOK;
    bsCopy: DoClickCopy;
    bsNext: DoClickNext;
    bsPrevious: DoClickPrevious;
    bsTrash: DoClickTrash;
    bsClose: DoClickClose;
    bsAccept: DoClickAccept;
    bsDeny: DoClickDeny;
    bsUndo: DoClickUndo;
    bsAdd: DoClickAdd;
    bsSub: DoClickSub;
    bsCustom: DoClickButton(sp.Tag);
    end;
  end;
end;


{$IFDEF DELPHIXE10_LVL}
procedure TAdvCustomMultiButtonEdit.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
{$IFNDEF DELPHIXE10_LVL}
procedure TAdvCustomMultiButtonEdit.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  inherited;

  LabelFont.Height := MulDiv(LabelFont.Height,M,D);
end;

procedure TAdvCustomMultiButtonEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.BiDiMode := BidiMode;
end;

procedure TAdvCustomMultiButtonEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvCustomMultiButtonEdit.CMEnabledChanged(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  FEdit.Enabled := Enabled;

  if not (csDesigning in ComponentState) then
  begin
    for i := 0 to Buttons.Count - 1 do
      Buttons[i].FButton.Enabled := Enabled;
  end;
end;

procedure TAdvCustomMultiButtonEdit.CMFontChanged(var Message: TMessage);
begin
  if ((csDesigning in ComponentState) and not (csLoading in ComponentState)) and ParentFont then
  begin
    FLabelFont.Assign(Font);
    if Assigned(FLabel) then
      FLabel.Font.Assign(Font);
  end;

  inherited;

  FEdit.Font.Assign(Font);
  UpdateButtons;
end;

procedure TAdvCustomMultiButtonEdit.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FLabel) then
    FLabel.Visible := Visible;
end;

constructor TAdvCustomMultiButtonEdit.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := TAdvEdit.Create(Self);
  FEdit.OnChange := DoEditChange;
  FEdit.OnKeyDown:= DoEditKeyDown;
  FEdit.OnKeyUp := DoEditKeyUp;
  FEdit.OnKeyPress := DoEditKeypress;
  FEdit.OnEnter := DoEditEnter;
  FEdit.OnExit := DoEditExit;
  FEdit.OnDblClick := DoEditDblClick;
  FEdit.OnClick := DoEditClick;
  FEditButtons := CreateButtons;
  FEdit.TabStop := true;
  FEdit.TabOrder := 0;
  FEditAlign := eaLeft;
  FEdit.EditAlign := eaLeft;
  FEditType := etString;
  FEdit.EditType := etString;
  FBorderStyle := bsSingle;
  FBorderColor := clNone;
  FEditColor := clWindow;
  FHideSelection := true;
  FMaxLength := 0;
  FButtonWidth := 22;
  FLeadingZeros := true;
  FEmptyTextFocused := false;
  Height := 22;
  Width := 200;
  FParentFnt := false;
  FLabelFont := TFont.Create;
  FLabelFont.Assign(Font);
  FLabelFont.OnChange := LabelFontChanged;
  FLabelMargin := 4;

  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonColorDisabled := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonTextColorDisabled := clNone;
  FButtonBorderColor := clNone;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  FDesignCreate := FDesignTime;

  if FDesignTime then
  begin
    SetComponentStyle(GetDefaultStyle(Owner));
    if SetParentFontForStyle(FTMSStyle) then
      ParentFont := true;
  end;
  FTMSStyle := tsCustom;

end;

function TAdvCustomMultiButtonEdit.CreateButtons: TEditButtons;
begin
  Result := TEditButtons.Create(Self);
end;

function TAdvCustomMultiButtonEdit.CreateLabel: TCustomAdvEditLabel;
begin
  Result := TCustomAdvEditLabel.Create(Self);
  Result.Parent := Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
//  Result.OnClick := LabelClick;
//  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := ParentFont;
end;

procedure TAdvCustomMultiButtonEdit.CreateWnd;
begin
  inherited;
  UpdateButtons;

  if Assigned(FLabel) then
    UpdateLabelPos;
end;

destructor TAdvCustomMultiButtonEdit.Destroy;
begin
  FEditButtons.Free;
  FLabelFont.Free;
  inherited;
end;

procedure TAdvCustomMultiButtonEdit.DoClickAccept;
begin
  if Assigned(OnClickAccept) then
    OnClickAccept(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickAdd;
begin
  if Assigned(OnClickAdd) then
    OnClickAdd(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickSub;
begin
  if Assigned(OnClickSub) then
    OnClickSub(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickButton(ButtonIndex: integer);
begin
  if Assigned(OnClickCustom) then
    OnClickCustom(Self, ButtonIndex);
end;

procedure TAdvCustomMultiButtonEdit.DoClickClear;
begin
  FEdit.Text := '';
  if Assigned(OnClickClear) then
    OnClickClear(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickClose;
begin
  Visible := false;
  if Assigned(OnClickClose) then
    OnClickClose(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickDeny;
begin
  if Assigned(OnClickDeny) then
    OnClickDeny(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickCopy;
begin
  if Assigned(OnClickCopy) then
    OnClickCopy(Self);
  Clipboard.Open;
  Clipboard.AsText := FEdit.Text;
  Clipboard.Close;
end;

procedure TAdvCustomMultiButtonEdit.DoClickFind;
begin
  if Assigned(OnClickFind) then
    OnClickFind(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickNext;
begin
  if Assigned(OnClickNext) then
    OnClickNext(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickOK;
begin
  if Assigned(OnClickOK) then
    OnClickOK(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickPrevious;
begin
  if Assigned(OnClickPrevious) then
    OnClickPrevious(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickTrash;
begin
  if Assigned(OnClickTrash) then
    OnClickTrash(Self);
  FEdit.Text := '';
end;

procedure TAdvCustomMultiButtonEdit.DoClickUndo;
begin
  if Assigned(OnClickUndo) then
    OnClickUndo(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditChange(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    Exit;

  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditEnter(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  if FFocusLabel and (FLabel <> nil) then
  begin
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
    UpdateLabelPos;
  end;

  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    OnExit(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if csLoading in ComponentState then
    Exit;

  if FFocusLabel and (FLabel <> nil) then
  begin
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    UpdateLabelPos;
  end;

  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeypress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvCustomMultiButtonEdit.DoEnter;
begin
  FEdit.SetFocus;
end;

function TAdvCustomMultiButtonEdit.GetFloatValue: double;
begin
  Result := 0;
  if Assigned(FEdit) then
    Result := FEdit.FloatValue;
end;

function TAdvCustomMultiButtonEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

function TAdvCustomMultiButtonEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

function TAdvCustomMultiButtonEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomMultiButtonEdit.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomMultiButtonEdit.LabelFontChanged(Sender: TObject);
begin
  if Assigned(FLabel) then
    UpdateLabel;

  if not FDesignCreate then
    ParentFont := false;
end;

procedure TAdvCustomMultiButtonEdit.Loaded;
begin
  inherited;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
  begin
    FLabel.Enabled := Enabled;
  end;

  if (FLabel <> nil) then
    UpdateLabel;

  if ParentFont and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end;

  FParentFnt := ParentFont;

  if (FLabel <> nil) then
    UpdateLabel;

{$IFDEF DELPHIXE2_LVL}
  if CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    LabelTransparent := true;
  end;
{$ENDIF}
end;

procedure TAdvCustomMultiButtonEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TAdvCustomMultiButtonEdit.Paint;
{$IFDEF DELPHIXE2_LVL}
var
  LStyle: TCustomStyleServices;
  R: TRect;
  l: TThemedElementDetails;
  c: TColor;
  se: boolean;
{$ENDIF}

begin
  inherited;

  {$IFDEF DELPHIXE2_LVL}

  {$IFDEF DELPHIXE6_LVL}
  se := seClient in StyleElements;
  {$ELSE}
  se := true;
  {$ENDIF}

  LStyle := StyleServices;
  if CheckVCLStylesEnabled(LStyle,(csDesigning in ComponentState)) and se then
  begin
    R := ClientRect;
    InflateRect(R,1,1);
    l := StyleServices.GetElementDetails(tpPanelBackground);
    c := clNone;
    StyleServices.GetElementColor(l, ecFillColor, c);
    if c <> clNone then
    begin
      Canvas.Brush.Color := c;
      Canvas.Brush.Style := bsSolid;
      Canvas.Rectangle(R);
    end;
  end;
  {$ENDIF}
end;

procedure TAdvCustomMultiButtonEdit.SetAllowNumericNullValue(
  const Value: boolean);
begin
  FAllowNumericNullValue := Value;
  if Assigned(FEdit) then
    FEdit.AllowNumericNullValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  if Assigned(FEdit) then
    FEdit.BorderColor := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  if Assigned(FEdit) then
  begin
    FEdit.BorderStyle := Value;
  end;
end;

procedure TAdvCustomMultiButtonEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  lblmargin: integer;
begin
  if Assigned(FLabel) then
  begin
    lblmargin := Round(FLabelMargin *  GetDPIScale(Self, nil));

    case LabelPosition of
      lpLeftTop, lpLeftCenter, lpLeftBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
          begin
            AWidth := AWidth - (FLabel.Width + lblmargin);
            ALeft := ALeft + (FLabel.Width + lblmargin);
          end;
        end;
      lpRightTop, lpRightCenter, lpRighBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
            AWidth := AWidth - (FLabel.Width + lblmargin);
        end;
      lpTopLeft, lpTopCenter, lpTopRight:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (FLabel <> nil) then
  begin
    if (FLabel.Parent <> nil) then
      UpdateLabel;
  end;
end;

procedure TAdvCustomMultiButtonEdit.SetButtons(const Value: TEditButtons);
begin
  FEditButtons.Assign(Value);
end;

procedure TAdvCustomMultiButtonEdit.SetButtonWidth(const Value: integer);
begin
  if (FButtonWidth <> Value) and (Value > 0) and (Value < 256) then
  begin
    FButtonWidth := Value;
    UpdateButtons;
  end;
end;

procedure TAdvCustomMultiButtonEdit.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;

  if Assigned(FEdit) then
    FEdit.CharCase := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;

  FEdit.UIStyle := FTMSStyle;

  if FTMSStyle <> tsCustom then
  begin
    case AStyle of
      tsOffice2016Black:
      begin
        Font.Color := clSilver;
        LabelFont.Color := clSilver;
        ButtonColor := clNone;
        ButtonColorHot := clNone;
        ButtonColorDown := clNone;
        ButtonTextColor := clNone;
        ButtonTextColorHot := clNone;
        ButtonTextColorDown := clNone;
        ButtonBorderColor := clNone;
        ButtonColorDisabled := clNone;
        ButtonTextColorDisabled := clNone;
      end;
      tsOffice2019White:
      begin
        Font.Color := $00444648;
        LabelFont.Color := $00444648;
        ButtonColor := clWhite;
        ButtonColorHot := $00F2E1D5;
        ButtonColorDown := $00E3BDA3;
        ButtonTextColor := $00444648;
        ButtonTextColorHot := $00232425;
        ButtonTextColorDown := $00232425;
        ButtonTextColorDisabled := clGray;
        ButtonBorderColor := $00ABABAB;
        ButtonColorDisabled := $00D4D4D4;
      end;
      tsOffice2019Gray:
      begin
        Font.Color := $00232425;
        LabelFont.Color := clWhite;
        ButtonColor := $00B8BBBE;
        ButtonColorHot := $00969696;
        ButtonColorDown := $00666666;
        ButtonTextColor := $00232425;
        ButtonTextColorHot := $00232425;
        ButtonTextColorDown := $00232425;
        ButtonTextColorDisabled := clGray;
        ButtonBorderColor := $00808080;
        ButtonColorDisabled := $00D4D4D4;
      end;
      tsOffice2019Black:
      begin
        Font.Color := clWhite;
        LabelFont.Color := clWhite;
        ButtonColor := $00444444;
        ButtonColorHot := $00686868;
        ButtonColorDown := $00828282;
        ButtonTextColor := clWhite;
        ButtonTextColorHot := clWhite;
        ButtonTextColorDown := clWhite;
        ButtonTextColorDisabled := clGray;
        ButtonBorderColor := $00686868;
        ButtonColorDisabled := $00D4D4D4;
      end;
      else
      begin
        Font.Color := clWindowText;
        LabelFont.Color := clWindowText;
        ButtonColor := clNone;
        ButtonColorHot := clNone;
        ButtonColorDown := clNone;
        ButtonTextColor := clNone;
        ButtonTextColorHot := clNone;
        ButtonTextColorDown := clNone;
        ButtonBorderColor := clNone;
        ButtonColorDisabled := clNone;
        ButtonTextColorDisabled := clNone;
      end;
    end;
  end;

  Invalidate;
end;

procedure TAdvCustomMultiButtonEdit.SetEditAlign(const Value: TEditAlign);
begin
  FEditAlign := Value;
  if Assigned(FEdit) then
    FEdit.EditAlign := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEditColor(const Value: TColor);
begin
  FEditColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEditType(const Value: TAdvEditType);
begin
  FEditType := Value;
  if Assigned(FEdit) then
    FEdit.EditType := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEmptyText(const Value: string);
begin
  FEmptyText := Value;
  if Assigned(FEdit) then
    FEdit.EmptyText := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEmptyTextFocused(const Value: boolean);
begin
  FEmptyTextFocused := Value;
  if Assigned(FEdit) then
    FEdit.EmptyTextFocused := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEmptyTextStyle(const Value: TFontStyles);
begin
  FEmptyTextStyle := Value;
  if Assigned(FEdit) then
    FEdit.EmptyTextStyle := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetFloatValue(const Value: double);
begin
  if Assigned(FEdit) then
    FEdit.FloatValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  if Assigned(FEdit) then
    FEdit.HideSelection := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if Assigned(FLabel) then
  begin
    if Value then
      FLabel.Enabled := True;
    FLabel.AlwaysEnable := Value;
  end;
  Invalidate;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelPosition(
  const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomMultiButtonEdit.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomMultiButtonEdit.SetLeadingZeros(const Value: boolean);
begin
  FLeadingZeros := Value;
  if Assigned(FEdit) then
    FEdit.LeadingZeros := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMaxFloatValue(const Value: double);
begin
  FMaxFloatValue := Value;
  if Assigned(FEdit) then
    FEdit.MaxFloatValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
  if Assigned(FEdit) then
    FEdit.MaxLength := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMaxValue(const Value: Longint);
begin
  FMaxValue := Value;
  if Assigned(FEdit) then
    FEdit.MaxValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMinFloatValue(const Value: double);
begin
  FMinFloatValue := Value;
  if Assigned(FEdit) then
    FEdit.MinFloatValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMinValue(const Value: Longint);
begin
  FMinValue := Value;
  if Assigned(FEdit) then
    FEdit.MinValue := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetParent(AParent: TWinControl);
begin
  inherited;

  if not (csDestroying in ComponentState) then
  begin
    if FLabel <> nil then
      FLabel.Parent := AParent;
  end;

end;

procedure TAdvCustomMultiButtonEdit.SetPrecision(const Value: smallint);
begin
  FPrecision := Value;
  if Assigned(FEdit) then
    FEdit.Precision := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetPrecisionDisplay(
  const Value: TPrecisionDisplay);
begin
  FPrecisionDisplay := Value;
  if Assigned(FEdit) then
    FEdit.PrecisionDisplay := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  if Assigned(FEdit) then
    FEdit.Prefix := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if Assigned(FEdit) then
    FEdit.ReadOnly := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetReturnIsTab(const Value: boolean);
begin
  FReturnIsTab := Value;
  if Assigned(FEdit) then
    FEdit.ReturnIsTab := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  if Assigned(FEdit) then
    FEdit.Suffix := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetText(const Value: string);
begin
  if Assigned(FEdit) then
    FEdit.Text := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetUIStyle(const Value: TTMSStyle);
begin
  SetComponentStyle(Value);
end;

procedure TAdvCustomMultiButtonEdit.UpdateButtons;
var
  i: integer;
  FromLeft,FromRight: integer;
  FDPIScale: single;
begin
  if (csReading in ComponentState) then
    Exit;

  FromLeft := 0;
  FromRight := Width;

  FDPIScale := GetDPIScale(Self, nil);

  for i := 0 to Buttons.Count - 1 do
  begin
    Buttons[i].FButton.Parent := Self;
    Buttons[i].FButton.Width := Round(FButtonWidth * FDPIScale);

    if Buttons[i].Position = bpLeft then
    begin
      Buttons[i].FButton.Left := FromLeft;
      Buttons[i].FButton.Align := alLeft;
      FromLeft := FromLeft + Buttons[i].FButton.Width;
    end;

    if Buttons[i].Position = bpRight then
    begin
      Buttons[i].FButton.Left := FromRight - Buttons[i].FButton.Width;
      Buttons[i].FButton.Align := alRight;
      FromRight := FromRight + Buttons[i].FButton.Width;
    end;

    if (Buttons[i].Style = bsCustom) and Assigned(FImages) and (Buttons[i].ImageIndex >= 0) then
    begin
      Buttons[i].FButton.Images := FImages;
      Buttons[i].FButton.ImageIndexInt := Buttons[i].ImageIndex;
    end;

    case Buttons[i].Style of
    bsCustom: Buttons[i].FButton.PNGName := '';
    bsClear: Buttons[i].FButton.PNGName := 'tms_gl_cancel';
    bsFind: Buttons[i].FButton.PNGName := 'tms_gl_search';
    bsOK: Buttons[i].FButton.PNGName := 'tms_gl_ok';
    bsTrash: Buttons[i].FButton.PNGName := 'tms_gl_trash';
    bsCopy: Buttons[i].FButton.PNGName := 'tms_gl_copy';
    bsClose: Buttons[i].FButton.PNGName := 'tms_gl_close';
    bsDeny: Buttons[i].FButton.PNGName := 'tms_gl_deny';
    bsAccept: Buttons[i].FButton.PNGName := 'tms_gl_accept';
    bsPrevious: Buttons[i].FButton.PNGName := 'tms_gl_prev';
    bsNext: Buttons[i].FButton.PNGName := 'tms_gl_next';
    bsUndo: Buttons[i].FButton.PNGName := 'tms_gl_undo';
    bsAdd: Buttons[i].FButton.PNGName := 'tms_gl_add';
    bsSub: Buttons[i].FButton.PNGName := 'tms_gl_sub';
    end;

    Buttons[i].FButton.Hint := Buttons[i].Hint;
    Buttons[i].FButton.ShowHint := Buttons[i].Hint <> '';
    Buttons[i].FButton.Caption := Buttons[i].Caption;
    Buttons[i].FButton.Tag := i;
    Buttons[i].FButton.Top := 0;
    Buttons[i].FButton.Enabled := Buttons[i].Enabled;
    Buttons[i].FButton.Flat := Buttons[i].Flat;
    Buttons[i].FButton.OnClick := ButtonClick;
    Buttons[i].FButton.Font.Assign(Font);
    {$IFDEF DELPHIXE6_LVL}
    Buttons[i].FButton.StyleElements := StyleElements;
    {$ENDIF}
  end;

  FEdit.Parent := Self;
  FEdit.Left := FromLeft;
  FEdit.Align := alClient;
end;


procedure TAdvCustomMultiButtonEdit.UpdateLabel;
begin
  if Assigned(FLabel.Parent) then
  begin
    FLabel.Transparent := FLabeltransparent;

    if not FParentFnt then
    begin
      FLabel.Font.Assign(FLabelFont);
    end
    else
      FLabel.Font.Assign(Font);

    if FocusLabel then
    begin
      if Focused then
        FLabel.Font.Style := FLabel.Font.Style + [fsBold]
      else
        FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    end;

    if FLabel.Parent.HandleAllocated then
      UpdateLabelPos;
  end;
end;

procedure TAdvCustomMultiButtonEdit.UpdateLabelPos;
var
  tw,brdr,lblmargin: Integer;
  r: TRect;
begin
  r := Rect(0,0,1000,255);
  DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;

  lblmargin := Round(FLabelMargin * GetDPIScale(Self, nil));

  brdr := 0;
  if BorderStyle = bsSingle then
    brdr := 2;


  case FLabelPosition of
    lpLeftTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left - tw - lblmargin;
      end;
    lpLeftCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := Left - tw - lblmargin;
      end;
    lpLeftBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left - tw - lblmargin;
      end;
    lpTopLeft:
      begin
        FLabel.Top := self.Top - FLabel.Height - lblmargin;
        FLabel.Left := self.Left;
      end;
    lpTopRight:
      begin
        FLabel.Top := self.Top - FLabel.Height - lblmargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpTopCenter:
      begin
        FLabel.Top := self.Top - FLabel.height - lblmargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.Width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.Width) div 2)
      end;
    lpBottomLeft:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        FLabel.left := self.left;
      end;
    lpBottomCenter:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.width) div 2)
      end;
    lpBottomRight:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpLeftTopLeft:
      begin
        FLabel.top := self.top;
        FLabel.left := self.left - lblmargin;
      end;
    lpLeftCenterLeft:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.top + ((Height - brdr - FLabel.height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - Height + brdr) div 2);
        FLabel.left := self.left - lblmargin;
      end;
    lpLeftBottomLeft:
      begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - lblmargin;
      end;
    lpRightTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
    lpRightCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
    lpRighBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
  end;

  FLabel.Visible := Visible;
end;

{ TEditButtons }

function TEditButtons.Add: TEditButton;
begin
  Result := TEditButton(inherited Add);
end;

constructor TEditButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TEditButton);
  if AOwner is TAdvMultiButtonEdit then
    FEdit := AOwner as TAdvMultiButtonEdit
  else
    FEdit := nil;
end;

function TEditButtons.FindButton(AStyle: TButtonStyle): TEditButton;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Style = AStyle then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TEditButtons.GetItem(Index: Integer): TEditButton;
begin
  Result := TEditButton(inherited Items[Index]);
end;

function TEditButtons.Insert(Index: Integer): TEditButton;
begin
  Result := TEditButton(inherited Insert(Index));
end;

procedure TEditButtons.SetItem(Index: Integer; const Value: TEditButton);
begin
  inherited Items[Index] := Value;
end;

procedure TEditButtons.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.UpdateButtons;
end;

{ TEditButton }

procedure TEditButton.Assign(Source: TPersistent);
begin
  if (Source is TEditButton) then
  begin
    FButtonPosition := (Source as TEditButton).Position;
    FFlat := (Source as TEditButton).Flat;
    FEnabled := (Source as TEditButton).Enabled;
    FHint := (Source as TEditButton).Hint;
    FStyle := (Source as TEditButton).Style;
    FImageIndex := (Source as TEditButton).ImageIndex;
  end;
end;

constructor TEditButton.Create(Collection: TCollection);
begin
  Collection.BeginUpdate;
  inherited Create(Collection);
  FButton := TPNGSpeedButton.Create((Collection as TEditButtons).FEdit);
  FButtonPosition := bpRight;
  FButton.ImageIndexInt := -1;
  FButton.Images := nil;
  FFlat := false;
  FEnabled := true;
  FImageIndex := -1;
  FStyle := bsCustom;
  Collection.EndUpdate;
end;

destructor TEditButton.Destroy;
begin
  FButton.Free;
  inherited;
end;

procedure TEditButton.SetButtonPosition(const Value: TButtonPosition);
begin
  if (FButtonPosition <> Value) then
  begin
    FButtonPosition := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetFlat(const Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetHint(const Value: string);
begin
  if (FHint <> Value) then
  begin
    FHint := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetImageIndex(const Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetStyle(const Value: TButtonStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;


procedure PNGInvertWB(Image: TPngImage; AWhite: Boolean);

  procedure WBInvertRGB(var R, G, B: Byte);
  var
    Color: LongInt;
  begin
    if AWhite then
    begin
      if RGB(R, G, B) = uint(clWhite) then
      begin
        Color := ColorToRGB(clBlack);
        R := GetRValue(Color);
        G := GetGValue(Color);
        B := GetBValue(Color);
      end;
    end
    else
    begin
      if RGB(R, G, B) = uint(clBlack) then
      begin
        Color := ColorToRGB(clWhite);
        R := GetRValue(Color);
        G := GetGValue(Color);
        B := GetBValue(Color);
      end;

      R := 255;
      G := 255;
      B := 255;
    end;
  end;

var
  X, Y, PalCount: Integer;
  Line: PRGBLine;
  PaletteHandle: HPalette;
  Palette: array[Byte] of TPaletteEntry;
begin
  if not (Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA]) then
  begin
    if Image.Header.ColorType = COLOR_PALETTE then
    begin
      PaletteHandle := Image.Palette;
      PalCount := GetPaletteEntries(PaletteHandle, 0, 256, Palette);
      for X := 0 to PalCount - 1 do
        WBInvertRGB(Palette[X].peRed, Palette[X].peGreen, Palette[X].peBlue);
      SetPaletteEntries(PaletteHandle, 0, PalCount, Palette);
      Image.Palette := PaletteHandle;
    end
    else
    begin
      for Y := 0 to Image.Height - 1 do begin
        Line := Image.Scanline[Y];
        for X := 0 to Image.Width - 1 do
        begin
          WBInvertRGB(Line[X].rgbtRed, Line[X].rgbtGreen, Line[X].rgbtBlue);
        end;
      end;
    end;
  end;
end;

function IsDark(AColor: TColor): boolean;
var
  r,g,b: byte;
begin
  ColorToRGB(AColor);
  r := (AColor and $FF0000) shr 16;
  g := (AColor and $00FF00) shr 8;
  b := AColor and $0000FF;
  Result := (R * 0.299 + G * 0.587 + B * 0.114) < 186
end;

{ TPNGSpeedButton }

procedure TPNGSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
end;

procedure TPNGSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
end;

procedure TPNGSpeedButton.Paint;
var
  png: TPngImage;
  t,l: integer;
  r: TRect;
  drwCaption: boolean;
  Clr, FontColor, FontColorDisabled: TColor;
  VCLStyleEnabled: boolean;
begin
  inherited;

  drwCaption := true;
  Clr := clWindowText;

  VCLStyleEnabled := False;
  FontColor := clBlack;
  FontColorDisabled := clGray;

  {$IFDEF DELPHIXE2_LVL}
  if CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    Clr := StyleServices.GetStyleFontColor(sfButtonTextNormal);
    VCLStyleEnabled := true;
  end;
  {$ENDIF}

  if not VCLStyleEnabled and (Parent is TAdvCustomMultiButtonEdit) and (TAdvCustomMultiButtonEdit(Parent).ButtonColor <> clNone) then
  begin
    Canvas.Brush.Color := TAdvCustomMultiButtonEdit(Parent).ButtonColor;
    if (TAdvCustomMultiButtonEdit(Parent).ButtonTextColor <> clNone) then
      FontColor := TAdvCustomMultiButtonEdit(Parent).ButtonTextColor;

    if (TAdvCustomMultiButtonEdit(Parent).ButtonColorHot <> clNone) and FHot then
      Canvas.Brush.Color := TAdvCustomMultiButtonEdit(Parent).ButtonColorHot;
    if (TAdvCustomMultiButtonEdit(Parent).ButtonTextColorHot <> clNone) and FHot then
      FontColor := TAdvCustomMultiButtonEdit(Parent).ButtonTextColorHot;

    if (TAdvCustomMultiButtonEdit(Parent).ButtonColorDown <> clNone) and (FState in [bsDown, bsExclusive]) then
      Canvas.Brush.Color := TAdvCustomMultiButtonEdit(Parent).ButtonColorDown;
    if (TAdvCustomMultiButtonEdit(Parent).ButtonTextColorDown <> clNone) and (FState in [bsDown, bsExclusive]) then
      FontColor := TAdvCustomMultiButtonEdit(Parent).ButtonTextColorDown;

    if (TAdvCustomMultiButtonEdit(Parent).ButtonColorDisabled <> clNone) and not Enabled then
      Canvas.Brush.Color := TAdvCustomMultiButtonEdit(Parent).ButtonColorDisabled;

    if (TAdvCustomMultiButtonEdit(Parent).ButtonTextColorDisabled <> clNone) and not Enabled then
      FontColorDisabled := TAdvCustomMultiButtonEdit(Parent).ButtonTextColorDisabled;

    if (TAdvCustomMultiButtonEdit(Parent).ButtonBorderColor <> clNone) then
      Canvas.Pen.Color := TAdvCustomMultiButtonEdit(Parent).ButtonBorderColor
    else
      Canvas.Pen.Color := TAdvCustomMultiButtonEdit(Parent).FEdit.BorderColor;

    clr := FontColor;

    r := ClientRect;
    InflateRect(r, -1, -1);
    Canvas.FillRect(r);

    Canvas.Brush.Style := TBrushStyle.bsClear;
    Canvas.Rectangle(r);
  end;

  if Assigned(FImages) and (FImageIndexInt >= 0) then
  begin
    t := (Height - FImages.Height) div 2;
    l := (Width - FImages.Width) div 2;
    FImages.Draw(Canvas, l, t, FImageIndexInt);
    drwCaption := false;
  end;

  if PNGName <> '' then
  begin
    png := TPngImage.Create;
    try
      png.LoadFromResourceName(Hinstance, PNGName);
      t := (Height - png.Height) div 2;
      l := (Width - png.Width) div 2;

      if not IsDark(clr) then
        PNGInvertWB(png, false);

      png.Draw(Canvas, Rect(l,t,l + png.Width,t + png.Height));
      drwCaption := false;
    finally
      png.Free;
    end;
  end;

  if (FCaption <> '') and drwCaption then
  begin
    if not Enabled then
      Canvas.Font.Color := FontColorDisabled
    else
      Canvas.Font.Color := FontColor;

    r := ClientRect;

    DrawText(Canvas.Handle, PChar(FCaption), Length(FCaption), r, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;
    {

procedure TPNGSpeedButton.PaintButton;
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  r, ARect: TRect;
  BtnFaceBrush: HBRUSH;
  HTheme: THandle;
  FFlat: boolean;
begin
  FFlat := false;


//  if (TEditButton(Owner).ButtonColor <> clNone) then
  begin
    FFlat := true;
    Canvas.Brush.Color := clREd;// TEditButton(Owner).ButtonColor;

    if FHot then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorDown;

    Canvas.Pen.Color := Canvas.Brush.Color;
    ARect := ClientRect;
    ARect.Left := ARect.Left + 2;
    Canvas.FillRect(ClientRect);
    ARect.Left := ARect.Left - 2;

    Canvas.Pen.Color := TCustomAdvEditBtn(Owner.Owner).BorderColor;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom);

    Canvas.Brush.Color := TEditButton(Owner).ButtonTextColor;

    if FHot then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorHot;
    end;

    if ((FState in [bsDown, bsExclusive]) and not FUp) then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorDown;
    end;

    Canvas.Pen.Color := Canvas.Brush.Color;
  end
  else
  begin
    if DoVisualStyles then
    begin
      r := ClientRect;
      FillRect(Canvas.Handle,r,Canvas.Brush.Handle);
      InflateRect(r,1,1);

      HTheme := OpenThemeData(Parent.Handle,'button');

      if not TCustomAdvEditBtn(Owner.Owner).Enabled or TCustomAdvEditBtn(Owner.Owner).ReadOnly then
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_DISABLED,@r,nil)
      else
      begin
        if (FState in [bsDown, bsExclusive]) and not FUp then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@r,nil)
        else
          if FHot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@r,nil);
      end;

      CloseThemeData(HTheme);
    end
    else
    begin
      Enabled := TCustomAdvEditBtn(Owner.Owner).Enabled and not TCustomAdvEditBtn(Owner.Owner).ReadOnly;

      if not Flat then
        inherited Paint else
      begin
        r := BoundsRect;

        FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

        BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
        try
          FillRect(Canvas.Handle, r, BtnFaceBrush);
        finally
          DeleteObject(BtnFaceBrush);
        end;

        r.Bottom := r.Bottom + 1;
        r.Right := r.Right + 1;
        DrawEdge(Canvas.Handle, r, Edge[fEtched], BF_RECT or flags[fState=bsDown]);
      end;
    end;
  end;

  r := ClientRect;

  if Assigned(Glyph) then
  begin
    if not Glyph.Empty and (Glyph.Height > 0) and (Glyph.Width > 0) then
    begin
      OffsetRect(r,1,1);

      if (Caption = '') then
      begin
        if Glyph.Width < r.Right - r.Left then
          r.Left := r.Left + (r.Right - r.Left - Glyph.Width) shr 1;
      end
      else
        r.Left := r.Left + 2;

      if Glyph.Height < r.Bottom - r.Top then
        r.Top := r.Top + (r.Bottom - r.Top - Glyph.Height) shr 1;

      if (fState = bsdown) then
        Offsetrect(r,1,1);

      Glyph.Transparent := true;
      Glyph.TransparentMode := tmAuto;
      Canvas.Draw(r.Left, r.Top, Glyph);
    end;
  end;

  if (GlyphIndex <> -1) and Assigned(Images) then
  begin
    OffsetRect(r,1,1);

    if (Caption = '') then
    begin
      if Images.Width < r.Right - r.Left then
        r.Left := r.Left + (r.Right - r.Left - Images.Width) shr 1;
    end
    else
      r.Left := r.Left + 2;

    if Images.Height < r.Bottom - r.Top then
      r.Top := r.Top + (r.Bottom - r.Top - Images.Height) shr 1;

    if (fState = bsdown) then
      Offsetrect(r,1,1);

    Images.Draw(Canvas, r.Left, r.Top, GlyphIndex);
  end;

  pic := Picture;

  if not Enabled and not PictureDisabled.Empty then
    pic := PictureDisabled
  else
    if (FState in [bsDown, bsExclusive]) and not FUp and not PictureDown.Empty then
      pic := PictureDown
    else
      if FHot and not PictureHot.Empty then
        pic := PictureHot;


  if not pic.Empty then
  begin
    pic.GetImageSizes;

    OffsetRect(r,1,1);

    if (Caption = '') then
    begin
      if pic.Width < r.Right - r.Left then
        r.Left := r.Left + (r.Right - r.Left - pic.Width) shr 1;
    end
    else
      r.Left := r.Left + 2;

    if pic.Height < r.Bottom - r.Top then
      r.Top := r.Top + (r.Bottom - r.Top - pic.Height) shr 1;

    if (fState = bsdown) then
      Offsetrect(r,1,1);

    Canvas.Draw(r.Left, r.Top, pic);
  end;

  if (Caption <> '') then
  begin
    Canvas.Font.Assign(TAdvEdit(Owner.Owner).Font);

    if TEditButton(Owner).ButtonTextColor <> clNone then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColor;

    if not TCustomAdvEditBtn(Owner.Owner).Enabled or TCustomAdvEditBtn(Owner.Owner).ReadOnly then
      Canvas.Font.Color := clGray;

    if FHot and (TEditButton(Owner).ButtonTextColorHot <> clNone) then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp and (TEditButton(Owner).ButtonTextColorDown <> clNone) then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColorDown;

    r := ClientRect;

    if not FFlat then
    begin
      if not Assigned(Glyph) then
        InflateRect(r,-3,-1)
    end
    else
      r.Left := r.Left + 2;

    Windows.SetBKMode(Canvas.Handle,Windows.TRANSPARENT);

    if not Glyph.Empty then
    begin
      r.Left := r.Left + Glyph.Width + 2;
      r.Top := r.Top -1;
    end
    else
    begin
      if not FFlat then
        Inflaterect(r,-3,-1);
      if FState = bsdown then Offsetrect(r,1,1);
    end;

    DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r,DT_CENTER);
  end;
end;
}

procedure TPNGSpeedButton.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TPNGSpeedButton.SetPNGName(const Value: string);
begin
  if (FPNGName <> Value) then
  begin
    FPNGName := Value;
    Invalidate;
  end;
end;

{ TAdvTouchSpinEdit }

constructor TAdvTouchSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Buttons.Add.Position := bpLeft;
  Buttons.Add.Position := bpRight;
  Buttons[0].Caption := '-';
  Buttons[1].Caption := '+';
  Edit.EditType := etNumeric;
  Edit.EditAlign := eaCenter;
  FEditAlign := eaCenter;
  FValue := 0;
  FMin := 0;
  FMax := 100;
  FStep := 1;
  FSpinLeft := TSpinButtonSettings.Create;
  FSpinLeft.Caption := '-';
  FSpinLeft.OnChanged := DoSpinChanged;
  FSpinRight := TSpinButtonSettings.Create;
  FSpinRight.Caption := '+';
  FSpinRight.OnChanged := DoSpinChanged;
end;

destructor TAdvTouchSpinEdit.Destroy;
begin
  FSpinLeft.Free;
  FSpinRight.Free;
  inherited;
end;

procedure TAdvTouchSpinEdit.DoClickButton(ButtonIndex: integer);
begin
  if (ButtonIndex = 0) then
  begin
    Value := Value - Step;
  end;

  if ButtonIndex = 1 then
  begin
    Value := Value + Step;
  end;
end;

procedure TAdvTouchSpinEdit.DoEditChange(Sender: TObject);
var
  e: integer;
begin
  inherited;

  val(Text, FValue, e);

  if not (csLoading in ComponentState) then
    UpdateEnabledState;
end;

procedure TAdvTouchSpinEdit.DoEditExit(Sender: TObject);
var
  v,e: integer;
begin
  inherited;

  Val(Text, v, e);
  SetValue(v);
end;

procedure TAdvTouchSpinEdit.SetEditAlign(const AValue: TEditAlign);
begin
  FEditAlign := AValue;
  Edit.EditAlign := AValue;
end;

procedure TAdvTouchSpinEdit.SetMax(const AValue: integer);
begin
  FMax := AValue;
  if Value > FMax then
    Value := FMax;
end;

procedure TAdvTouchSpinEdit.SetMin(const AValue: integer);
begin
  FMin := AValue;
  if Value < FMin then
    Value := FMin;
end;

procedure TAdvTouchSpinEdit.SetSpinLeft(const Value: TSpinButtonSettings);
begin
  FSpinLeft.Assign(Value);
end;

procedure TAdvTouchSpinEdit.SetSpinRight(const Value: TSpinButtonSettings);
begin
  FSpinRight.Assign(Value);
end;

procedure TAdvTouchSpinEdit.SetValue(const AValue: integer);
begin
  if AValue > Max then
    FValue := Max
  else
    if AValue < Min then
      FValue := Min
    else
      FValue := AValue;

  Text := IntToStr(FValue);
  UpdateEnabledState;
end;

procedure TAdvTouchSpinEdit.UpdateEnabledState;
begin
  if not ((Max = 0) and (Min = 0)) then
  begin
    Buttons[1].Enabled := Value <> Max;
    Buttons[0].Enabled := Value <> Min;
    UpdateButtons;
  end;
end;

procedure TAdvTouchSpinEdit.DoSpinChanged(Sender: TObject);
begin
  Buttons[0].Caption := FSpinLeft.Caption;
  Buttons[0].Hint := FSpinLeft.Hint;
  Buttons[0].ImageIndex := FSpinLeft.ImageIndex;

  Buttons[1].Caption := FSpinRight.Caption;
  Buttons[1].Hint := FSpinRight.Hint;
  Buttons[1].ImageIndex := FSpinRight.ImageIndex;

  UpdateButtons;
end;

{ TSpinButtonSettings }

procedure TSpinButtonSettings.Assign(Source: TPersistent);
begin
  if (Source is TSpinButtonSettings) then
  begin
    FCaption := (Source as TSpinButtonSettings).Caption;
    FHint := (Source as TSpinButtonSettings).Hint;
    FImageIndex := (Source as TSpinButtonSettings).ImageIndex;
  end;
end;

procedure TSpinButtonSettings.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

constructor TSpinButtonSettings.Create;
begin
  FImageIndex := -1;
end;

procedure TSpinButtonSettings.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TSpinButtonSettings.SetHint(const Value: string);
begin
  if (FHint <> Value) then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TSpinButtonSettings.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

end.
