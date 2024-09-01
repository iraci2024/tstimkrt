unit FlatCtrls;

interface

{$I FlatStyle.inc}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics, SysUtils, MMSystem,
  StdCtrls, ExtCtrls, MaskUtils, Themes, Dialogs, ShellApi, ActnList, Grids,
  ComCtrls, Menus,Vcl.ImgList, CommCtrl, FlatUtils, FlatSkins;

type
  { TDefineListBox }
  TDefineListBox = class(TVersionControl)
  private
    scrollType: TScrollType;
    FirstItem: Integer;
    FSorted: Boolean;
    FItems: TStringList;
    FRects: TList;
    FChecks: TList;
    FItemIndex: Integer;
    FMultiSelect: Boolean;
    FOnChange: TNotifyChange;
    FOnClick: TNotifyClick;
    FStyle: TListStyle;
    FCaption: TCaption;
    FMouseIn: boolean;
    procedure SetSorted(Value: Boolean);
    procedure SetItems(Value: TStringList);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetItemIndex(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetListStyle(const Value: TListStyle);
    procedure SetCaption(const Value: TCaption);
    function  GetItemCount: Integer;
    function GetMouseIn: Boolean;
  protected
    procedure SetItemsRect;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetName(const Value: TComponentName); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure StyleChange(Sender: TObject);
    procedure SelectNotifyEvent;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DeleteChecked(index: integer);
    procedure AddCheck(index: integer);
    function FindChecked(Value: Integer;var Index:Integer): boolean;
    function GetMaxItems: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetSelCount: Integer;
    function GetItemIndex: Integer;
    function GetItemText: TCaption;
    property Skin: TListStyle read FStyle write SetListStyle;
    property MaxItems: Integer read GetMaxItems;
    property Items: TStringList read FItems write SetItems;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property Caption:TCaption read FCaption write SetCaption;
    property Sorted: Boolean read FSorted write SetSorted default false;
    property OnClick: TNotifyClick read FOnClick write FOnClick;
    property OnChange: TNotifyChange read FOnChange write FOnChange;
    property TabStop default True;
    property ParentColor default True;
    property ParentFont default True;
    property Enabled default True;
    property Visible default True;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Clear;
    function  Find(Value:String; var Index : Integer):boolean;
    property  ItemText:TCaption read GetItemText;
    property  Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property  SelCount: Integer read GetSelCount;
    property  ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property  ItemCount: Integer read GetItemCount;
  end;

  

  { TDefineListChecks }
  TDefineListChecks = class(TVersionControl)
  private
    FSelected: Integer;
    FCurSelected :integer;
    scrollType: TScrollType;
    FirstItem: Integer;
    FSorted: Boolean;
    FItems: TStringList;
    FRects: TList;
    FChecks: TList;
    FOnChange: TNotifyChange;
    FOnClick: TNotifyClick;
    FOnClickCheck: TNotifyEvent;
    FCaption: TCaption;
    FStyle: TCheckStyle;
    FMouseIn: boolean;
    procedure SetSorted(Value: Boolean);
    procedure SetItems(Value: TStringList);
    procedure SetChecked(Index: Integer; Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetCheckStyle(const Value: TCheckStyle);
    procedure SetItemIndex(Value: Integer);
    function GetItemCount: Integer;
    function GetMouseIn: Boolean;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure SetItemsRect;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure DrawCheckRect(Canvas: TCanvas; StartRect: TRect; checked: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure StyleChange( Sender: TObject);
    procedure SetName(const Value: TComponentName); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SelectNotifyEvent;
    procedure DeleteChecked(index: integer);
    procedure AddCheck(index: integer);
    
    function FindChecked(Value: Integer;var Index:Integer): boolean;
    function GetChecked(Index: Integer): Boolean;  
    function GetSelCount: Integer;
    function GetItemIndex: Integer;
    function GetItemText: TCaption;
    function GetMaxItems: Integer;

    property Skin: TCheckStyle read FStyle write SetCheckStyle;
    property Sorted: Boolean read FSorted write SetSorted default false;
    property Items: TStringList read FItems write SetItems;
    property MaxItems: Integer read GetMaxItems;
    property Caption: TCaption read FCaption write SetCaption;
    property OnClick: TNotifyClick read FOnClick write FOnClick;
    property OnChange: TNotifyChange read FOnChange write FOnChange;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property TabStop default True;
    property ParentColor default True;
    property ParentFont default True;
    property Enabled default True;
    property Visible default True;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Click; override;
    procedure CheckAll;
    procedure CheckCancel;
    procedure Delete(Index:Integer);
    function  Find(Value: String; var Index: Integer): boolean;
    property  Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property  SelCount: Integer read GetSelCount;
    property  ItemText: TCaption read GetItemText;
    property  ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property  ItemCount: Integer read GetItemCount;
  end;   
 
  { TDefineCheckBox }
  TDefineCheckBox = class(TVersionControl)
  private
    FMouseIn: Boolean;
    FMouseDown: Boolean;
    Focused: Boolean;
    FLayout: TLayoutPosition;
    FFocusedColor: TColor;
    FDownColor: TColor;
    FCheckedColor: TColor;
    FBorderColor: TColor;
    FTransparent: Boolean;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetLayout(Value: TLayoutPosition);
    procedure SetChecked(Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function GetMouseIn: Boolean;
    procedure SetState(const Value: TCheckBoxState);
    function GetChecked: Boolean;
  protected
    procedure Toggle; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Click; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    //procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUP(var Message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure Paint; override;
    //procedure CreateParams(var Params: TCreateParams); override;
    //procedure CreateWnd; override;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default DefaultBackdropColor;
    property ColorDown: TColor index 1 read FDownColor write SetColors default DefaultBarColor;
    property ColorChecked: TColor index 2 read FCheckedColor write SetColors default DefaultCheckColor;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors default DefaultBorderColor;
    property Layout: TLayoutPosition read FLayout write SetLayout default lpLeft;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Color default DefaultFlatColor;
    property ParentColor default false;
    property TabStop default True;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  { TDefineGroupBox }
  TDefineGroupBox = class(TVersionControl)
  private
    FTransparent: Boolean;
    FBorderColor: TColor;
    FBorder: TGroupBoxBorder;
    FBackgropStopColor: TColor;
    FBackgropStartColor: TColor;
    FStyleFace: TStyleFace;
    FBackgropOrien: TFillDirection;
    FAlignment: TAlignmentText;
    procedure SetColors(const Index: Integer; const Value: TColor);
    procedure SetBorder(const Value: TGroupBoxBorder);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFillDirect(const Value: TFillDirection);
    procedure SetStyleFace(const Value: TStyleFace); virtual;
    procedure SetAlignment(const Value: TAlignmentText);
  protected
    procedure Paint; override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure AdjustClientRect(var Rect: TRect); override;
    property ColorBorder:        TColor index 0 read FBorderColor        write SetColors default DefaultBorderColor;
    property BackgropStartColor: TColor index 1 read FBackgropStartColor write SetColors default DefaultColorStart;
    property BackgropStopColor:  TColor index 2 read FBackgropStopColor  write SetColors default DefaultColorStop;
    property BackgropOrien: TFillDirection read FBackgropOrien write SetFillDirect default fdLeftToRight;
    property StyleFace: TStyleFace read FStyleFace write SetStyleFace default fsDefault;
    property Border: TGroupBoxBorder read FBorder write SetBorder default brFull;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Alignment: TAlignmentText read FAlignment write SetAlignment default stLeft;
  public
    constructor Create(AOwner: TComponent); override;
  end;    

  { TDefineRadioButton }
  TDefineRadioButton = class(TVersionControl)
  private
    FMouseIn: Boolean;
    FMouseDown: Boolean;
    FFocused: Boolean;
    FGroupIndex: Integer;
    FLayout: TLayoutPosition;
    FChecked: Boolean;
    FFocusedColor: TColor;
    FDownColor: TColor;
    FCheckedColor: TColor;
    FBorderColor: TColor;
    FTransparent: Boolean;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetLayout(Value: TLayoutPosition);
    procedure SetChecked(Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function GetMouseIn: Boolean;    
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Click; override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    //procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    //procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    //procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUP(var Message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure Paint; override;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property Checked: Boolean read FChecked write SetChecked default false;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default DefaultBackdropColor;
    property ColorDown: TColor index 1 read FDownColor write SetColors default DefaultBarColor;
    property ColorChecked: TColor index 2 read FCheckedColor write SetColors default DefaultCheckColor;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors default DefaultBorderColor;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property Layout: TLayoutPosition read FLayout write SetLayout default lpLeft;
    property Color default DefaultFlatColor;
    property ParentColor default false;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TDefineRadioGroup }
  TDefineRadioGroup = class(TDefineGroupBox)
  private
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FTopOffset: Integer;
    function  GetButtons(Index: Integer):TDefineRadioButton;// TFlatRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetStyleFace(const Value: TStyleFace); override;
    procedure UpdateButtons;
    procedure SetOffset(const Value: Integer);
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property TopOffset: Integer read FTopOffset write SetOffset default 5;
    property Items: TStrings read FItems write SetItems;
    function CanModify: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Buttons[Index: Integer]: TDefineRadioButton read GetButtons;
  end;
  
  { TDefineListBoxExt }
  TDefineListBoxExt = class(TVersionListBoxExt)
  private
    FParentColor: Boolean;
    FFocusColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    function GetMouseIn: Boolean;
  protected
    procedure RedrawBorder (const Clip: HRGN = 0);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    property ColorFocused: TColor index 0 read FFocusColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property ParentFont default True;
    property AutoSize default False;
    property Ctl3D default False;
    property BorderStyle default bsNone;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
  end;         
  { TDefineCheckListExt }
  TDefineCheckListExt = class(TDefineListBoxExt)
  private
    FAllowGrayed: Boolean;
    FFlat: Boolean;
    FStandardItemHeight: Integer;
    FOnClickCheck: TNotifyEvent;
    FSaveStates: TList;
    FHeaderColor: TColor;
    FHeaderBkColor: TColor;
    procedure ResetItemHeight;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
    procedure SetChecked(Index: Integer; AChecked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    function CreateWrapper(Index: Integer): TObject;
    function ExtractWrapper(Index: Integer): TObject;
    function GetWrapper(Index: Integer): TObject;
    function HaveWrapper(Index: Integer): Boolean;
    procedure SetFlat(Value: Boolean);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMDestroy(var Msg : TWMDestroy);message WM_DESTROY;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetHeader(Index: Integer): Boolean;
    procedure SetHeader(Index: Integer; const Value: Boolean);
    procedure SetHeaderBkColor(const Value: TColor);
    procedure SetHeaderColor(const Value: TColor);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    function InternalGetItemData(Index: Integer): Longint; overLOAD;
    procedure InternalSetItemData(Index: Integer; AData: Longint); overLOAD;
    procedure SetItemData(Index: Integer; AData: LongInt); overLOAD;
    function GetItemData(Index: Integer): LongInt; overLOAD;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ResetContent; override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetCheckWidth: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckAll;
    procedure CheckCancel;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property Header[Index: Integer]: Boolean read GetHeader write SetHeader;
  published
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clInfoText;
    property HeaderBkColor: TColor read FHeaderBkColor write SetHeaderBkColor default clInfoBk;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Flat: Boolean read FFlat write SetFlat default True;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Align;
    property Anchors;
    property AutoComplete;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TDefineSpeed }
  TDefineSpeed = class(TVersionGraphic)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTransparent: TTransparentMode;
    TextBounds: TRect;
    GlyphPos: TPoint;
    FNumGlyphs: TNumGlyphs;
    fColorDown: TColor;
    FColorBorder: TColor;
    FColorShadow: TColor;
    fColorFocused: TColor;
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseIn: Boolean;
    FModalResult: TModalResult;
    fColorFlat: TColor;
    FFoisChange: Boolean;
    FTransBorder: Boolean;
    FAutoColor: TColor;
    FAutoStyle: TFontStyles;
    procedure UpdateExclusive;
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure SetTransparent (const Value: TTransparentMode);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetFoisChange(const Value: Boolean);
    procedure SetAutoStyle(const Value: TFontStyles);
    procedure SetTransBorder(const Value: Boolean);
    function GetMouseIn: Boolean;
  protected
    FState: TButtonState;
    function  GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property Transparent: TTransparentMode read FTransparent write SetTransparent default tmNone;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Color default DefaultFlatColor;
    property ColorFocused: TColor index 0 read fColorFocused write SetColors default DefaultFocusedColor;
    property ColorDown: TColor index 1 read fColorDown write SetColors default DefaultDownColor;
    property ColorBorder: TColor index 2 read FColorBorder write SetColors default DefaultBorderColor;
    property ColorShadow: TColor index 3 read FColorShadow write SetColors default DefaultShadowColor;
    property ColorFlat: TColor index 4 read fColorFlat write SetColors default DefaultFlatColor;
    property FoisColor: TColor index 5 read FAutoColor write SetColors default DefaultFoisColor;
    property TransBorder: Boolean read FTransBorder write SetTransBorder default false;
    property FoisChange: Boolean read FFoisChange write SetFoisChange default true;
    property FoisStyle: TFontStyles read FAutoStyle write SetAutoStyle default [fsBold];
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property MouseIn: Boolean read GetMouseIn;
   {$IFDEF DFS_DELPHI_4_UP}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure MouseEnter;
    procedure MouseLeave;
  end;
  
  { TTimeBtnState }
  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);
  { TDefineSpins }
  TDefineSpins = class(TDefineSpeed)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    procedure TimerExpired( Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property  Cursor default crHandPoint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;
  TDefineSpin = class(TWinControl)
  private
    FUpButton: TDefineSpins;
    FDownButton: TDefineSpins;
    FFocusedButton: TDefineSpins;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton: TDefineSpins;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TDefineSpins);
    procedure AdjustSize(var W, H: Integer); reintroduce;// {$IFDEF DFS_COMPILER_4_UP} reintroduce; {$ENDIF}
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds (ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Enabled;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;
  { TDefineTicket }
  TDefineTicket = class(TCustomLabel)
  private
    function  GetTop: Integer;
    function  GetLeft: Integer;
    function  GetWidth: Integer;
    function  GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure AdjustBounds; override;
    property AutoSize default True; 
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Font;
    property Height: Integer read GetHeight write SetHeight;
    property ParentFont;
    property Left: Integer read GetLeft;
    property Top: Integer read GetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Visible;
  end;   
  
  { TDefineEdit }
  TDefineEdit = class(TVersionEdit)
  private
    FParentColor: Boolean;
    FFocusColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FAlignment: TAlignment;
    FTicketSpace: Integer;
    FMouseIn: Boolean;
    FTicket: TDefineTicket;
    FTicketPosition: TTicketPosition;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor (Value: Boolean);
    function GetMouseIn: Boolean;
  protected
    fHintLabel: TLabel;
    procedure RedrawBorder(const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure SetAlignment(const Value: TAlignment);

    procedure LabelMouseEnter(Sender: TObject);
    procedure SetTicketPosition(const Value: TTicketPosition);
    procedure SetTicketSpace(const Value: Integer);
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure SetupInternalLabel;
    property Ticket: TDefineTicket read FTicket;
    property TicketPosition: TTicketPosition read FTicketPosition write SetTicketPosition default poLeft;
    property TicketSpace: Integer read FTicketSpace write SetTicketSpace default 3;

    property ColorFocused: TColor index 0 read FFocusColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  end;
  { TDefineInteger }
  TDefineInteger = class(TDefineEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FButton: TDefineSpin;
    FEditorEnabled: Boolean;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
  protected
    function  IsValidChar (Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;    
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxValue: LongInt read FMaxValue write FMaxValue default 0;
    property MinValue: LongInt read FMinValue write FMinValue default 0;
    property Value: LongInt read GetValue write SetValue default 0;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TDefineSpin read FButton;
  end;
  { TDefineFloat }
  TDefineFloat = class(TDefineEdit)
  private
    FPrecision, FDigits: Integer;
    FFloatFormat: TFloatFormat;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FButton: TDefineSpin;
    FEditorEnabled: Boolean;
    function GetValue: Extended;
    function CheckValue (Value: Extended): Extended;
    procedure SetValue (Value: Extended);
    procedure SetPrecision (Value: Integer);
    procedure SetDigits (Value: Integer);
    procedure SetFloatFormat (Value: TFloatFormat);
  protected
    function IsValidChar (Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    property Digits: Integer read FDigits write SetDigits;
    property Precision: Integer read FPrecision write SetPrecision;
    property FloatFormat: TFloatFormat read FFloatFormat write SetFloatFormat;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property Value: Extended read GetValue write SetValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TDefineSpin read FButton;
  end;
  { TDefineMemo }
  TDefineMemo = class(TVersionMemo)
  private
    FParentColor: Boolean;
    FFocusColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    function GetMouseIn: Boolean;
  protected
    procedure RedrawBorder (const Clip: HRGN);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    property ColorFocused: TColor index 0 read FFocusColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
  end;     
  {TDefineMask}
  TDefineError = class(Exception);

  TDefineState = set of (msMasked, msReEnter, msDBSetText);

  TDefineMask = class(TDefineEdit)
  private
    FEditMask: TEditMask;
    FMaskBlank: Char;
    FMaxChars: Integer;
    FMaskSave: Boolean;
    FMaskState: TDefineState;
    FCaretPos: Integer;
    FBtnDownX: Integer;
    FOldValue: string;
    FSettingCursor: Boolean;
    FOnValidate: TValidateEvent;
    function DoInputChar(var NewChar: Char; MaskOffset: Integer): Boolean;
    function InputChar(var NewChar: Char; Offset: Integer): Boolean;
    function DeleteSelection(var Value: string; Offset: Integer; Len: Integer): Boolean;
    function InputString(var Value: string; const NewValue: string; Offset: Integer): Integer;
    function AddEditFormat(const Value: string; Active: Boolean): string;
    function RemoveEditFormat(const Value: string): string;
    function FindLiteralChar (MaskOffset: Integer; InChar: Char): Integer;
    function GetEditText: string;
    function GetMasked: Boolean;
    function GetText: TMaskedText;
    function GetMaxLength: Integer;
    function CharKeys(var CharCode: Char): Boolean;
    procedure SetEditText(const Value: string);
    procedure SetEditMask(const Value: TEditMask);
    procedure SetMaxLength(Value: Integer);
    procedure SetText(const Value: TMaskedText);
    procedure DeleteKeys(CharCode: Word);
    procedure HomeEndKeys(CharCode: Word; Shift: TShiftState);
    procedure CursorInc(CursorPos: Integer; Incr: Integer);
    procedure CursorDec(CursorPos: Integer);
    procedure ArrowKeys(CharCode: Word; Shift: TShiftState);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure ReformatText(const NewMask: string);
    procedure GetSel(var SelStart: Integer; var SelStop: Integer);
    procedure SetSel(SelStart: Integer; SelStop: Integer);
    procedure SetCursor(Pos: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function EditCanModify: Boolean; virtual;
    procedure Reset; virtual;
    function GetFirstEditChar: Integer;
    function GetLastEditChar: Integer;
    function GetNextEditChar(Offset: Integer): Integer;
    function GetPriorEditChar(Offset: Integer): Integer;
    function GetMaxChars: Integer;
    function Validate(const Value: string; var Pos: Integer): Boolean; virtual;
    procedure ValidateError; virtual;
    procedure CheckCursor;
    property MaskState: TDefineState read FMaskState write FMaskState;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property OnValidate : TValidateEvent read FOnValidate write FOnValidate;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; virtual;
    procedure Clear; override;
    function GetTextLen: Integer;
    property IsMasked: Boolean read GetMasked;
    property EditText: string read GetEditText write SetEditText;
    property Text: TMaskedText read GetText write SetText;
    property EditMask: TEditMask read FEditMask write SetEditMask;
  end;

  { TDefineIPEdit }
  TDefineIPEdit = class(TDefineMask)
  protected
    { Protected declarations }
    IPText:TIP;
    fIPAddress : String;
    function  GetInx: integer;
    function  GetIPText: String;
    procedure SetIPText(const Value: String);
    function  Replace(Start, Len: Integer):integer;
    procedure KeyPress(var Key: Char); override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMExit(var Message: TCMExit);  message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    property IPAddress: String read GetIPText write SetIPText;
  public
    property Index:integer read GetInx;
    constructor Create(AOwner: TComponent); override;
  end;       
  { TDefineComboBox }
  TDefineComboBox = class(TVersionComboBox)
  private
    FArrowColor: TColor;
    FArrowBackgroundColor: TColor;
    FBorderColor: TColor;
    FButtonWidth: Integer;
    FChildHandle: HWND;
    FDefListProc: Pointer;
    FListHandle: HWND;
    FListInstance: Pointer;
    FSysBtnWidth: Integer;
    FSolidBorder: Boolean;
    FTicketSpace: Integer;
    FTicket: TDefineTicket;
    FMouseIn: Boolean;
    FTicketPosition: TTicketPosition;
    FFocusedColor: TColor;
    FFlatColor: TColor;
    fParentColor: Boolean;
    FReadOnly: boolean;
    procedure SetColors(Index: Integer; Value: TColor);
    function  GetButtonRect: TRect;
    procedure PaintButton;
    procedure PaintBorder;
    procedure RedrawBorders;
    procedure InvalidateSelection;
    function  GetSolidBorder: Boolean;
    procedure SetSolidBorder;
    procedure SetParentColor(const Value: Boolean);
    procedure SetReadOnly(const Value: boolean);
  protected
    procedure ListWndProc(var Message: TMessage);
    procedure KeyPress(var Key: Char); override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure WndProc(var Message: TMessage); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure SetTicketPosition(const Value: TTicketPosition);
    procedure SetTicketSpace(const Value: Integer);
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupInternalLabel;
    procedure CreateWnd; override;
    procedure SetItemIndex(const Value: Integer); override;
    function  GetMouseIn: boolean;
    function  GetItemsCount: integer;
    property SolidBorder: Boolean read FSolidBorder;
    property Ticket: TDefineTicket read FTicket;
    property TicketPosition: TTicketPosition read FTicketPosition write SetTicketPosition default poLeft;
    property TicketSpace: Integer read FTicketSpace write SetTicketSpace;
    property ParentColor: Boolean read fParentColor write SetParentColor default true;
    property ColorArrow: TColor index 0 read FArrowColor write SetColors default clBlack;
    property ColorArrowBackground: TColor index 1 read FArrowBackgroundColor write SetColors default $00C5D6D9;
    property ColorBorder: TColor index 2 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 3 read FFlatColor write SetColors default DefaultFlatColor;
    property ColorFocued: TColor index 4 read FFocusedColor write SetColors default clWhite;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default false;
    property MouseIn: boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    property Count:integer read GetItemsCount;
  end;
  { TFlatComboBox }
  { TDefineColorBox }
  TDefineColorBox = class(TVersionComboBox)
  private
    FArrowColor: TColor;
    FArrowBackgroundColor: TColor;
    FBorderColor: TColor;
    FHighlightColor: TColor;
    FButtonWidth: Integer;
    FChildHandle: HWND;
    FDefListProc: Pointer;
    FListHandle: HWND;
    FListInstance: Pointer;
    FSysBtnWidth: Integer;
    FSolidBorder: Boolean;
    FShowNames: Boolean;
    FValue: TColor;
    FColorBoxWidth: Integer;
    FColorDlg: TColorDialog;
    FTicketSpace: Integer;
    FTicket: TDefineTicket;
    FTicketPosition: TTicketPosition;
    fLanguage: TLanguage;
    procedure SetColors(Index: Integer; Value: TColor);
    function  GetButtonRect: TRect;
    procedure PaintButton;
    procedure PaintBorder;
    procedure RedrawBorders;
    procedure InvalidateSelection;
    function  GetSolidBorder: Boolean;
    procedure SetSolidBorder;
    procedure SetShowNames(Value: Boolean);
    procedure SetColorValue(Value: TColor);
    procedure SetColorBoxWidth(Value: Integer);
    procedure SetTicketPosition(const Value: TTicketPosition);
    procedure SetTicketSpace(const Value: Integer);
    procedure SetLanguage(const Value: TLanguage);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure ListWndProc(var Message: TMessage);
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    property  SolidBorder: Boolean read FSolidBorder;
    procedure Click; override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupInternalLabel;
    property Color default DefaultFlatColor;
    property ColorArrow: TColor index 0 read FArrowColor write SetColors default clBlack;
    property ColorArrowBackground: TColor index 1 read FArrowBackgroundColor write SetColors default $00C5D6D9;
    property ColorBorder: TColor index 2 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorHighlight: TColor index 3 read FHighlightColor write SetColors default clHighlight;
    property ColorBoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth default 30;
    property ShowNames: Boolean read FShowNames write SetShowNames;
    property Value: TColor read FValue write SetColorValue;
    property Language:TLanguage read fLanguage write SetLanguage default lgChinese;
    property Ticket: TDefineTicket read FTicket;
    property TicketPosition: TTicketPosition read FTicketPosition write SetTicketPosition default poLeft;
    property TicketSpace: Integer read FTicketSpace write SetTicketSpace default 3;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddColor(ColorName: String; Color: TColor): Boolean;
    function DeleteColorByName(ColorName: String): Boolean;
    function DeleteColorByColor(Color: TColor): Boolean;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  end;

  
  { TDefineSplitter }
  TDefineHack = class(TWinControl);

  TDefineSplitter = class(TVersionGraphic)
  private
    FBorderColor: TColor;
    FFocusedColor: TColor;
    FLineDC: HDC;
    FDownPos: TPoint;
    FSplit: Integer;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FControl: TControl;
    FNewSize: Integer;
    FActiveControl: TWinControl;
    FOldKeyDown: TKeyEvent;
    FLineVisible: Boolean;
    FOnMoved: TNotifyEvent;
    FStatus: TSplitterStatus;
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure ReleaseLineDC;
    procedure UpdateSize(X, Y: Integer);
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetColors (Index: Integer; Value: TColor);
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged (var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMExit(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure StopSizing;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color default $00E0E9EF;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default $0053D2FF;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default $00555E66;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property Align default alLeft;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;
  end;     
  
   { TDefinePucker }
  TDefinePucker = class;
  //Event types
  TAfterSizeChanged = procedure(Sender : TDefinePucker; ASizeRestored : Boolean) of object;
  TDefinePucker = class(TVersionControl)
  private
    FCloseBtnRect : TRect;
    FMaxBtnRect   : TRect;
    FMinBtnRect   : TRect;
    FOldBounds    : TRect;
    FOldAlign     : TAlign;
    FMinimizing   : Boolean;
    FGradientFill : Boolean;
    FFillDirection: TFillDirection;
    FShadow       : Boolean;
    FShadowDist   : Integer;
    FHeight       : Integer;
    FDefaultHeight : Integer;
    FShowHeader : Boolean;
    FCaption    : String;
    FTitleFont  : TFont;
    FTitleHeight: Integer;
    FTitleAlignment : TAlignment;
    FTitleShadowOnMouseEnter : Boolean;
    FTitleGradient : Boolean;
    FStartColor   : TColor;
    FEndColor     : TColor;
    FTitleStartColor : TColor;
    FTitleEndColor : TColor;
    FTitleColor : TColor;
    FBorderColor: TColor;
    FTitleBtnBorderColor: TColor;
    FTitleBtnBGColor: TColor;
    FTitleFillDirect : TFillDirection;
    FTitleImage : TBitmap;
    FTitleImageAlign : TTitleImageAlign;
    FTitleImageTransparent : Boolean;
    FTitleCursor : TCursor;
    FTitleButtons : TTitleButtons;
    FAnimation : Boolean;
    FMovable : Boolean;
    FSizable : Boolean;
    FMinimized : Boolean;
    FMaximized : Boolean;
    FBorderSize : Integer;
    FShowBorder : Boolean;
    FPanelCorner : TPanelCorners;
    FBGImage : TBitmap;
    FBGImageAlign : TBGImageAlign;
    FBGImageTransparent : Boolean;
    FMouseOnHeader : Boolean;
    FOnTitleClick : TNotifyEvent;
    FOnTitleDblClick : TNotifyEvent;
    FOnTitleMouseDown : TMouseEvent;
    FOnTitleMouseUp : TMouseEvent;
    FOnTitleMouseEnter: TNotifyEvent;
    FOnTitleMouseExit : TNotifyEvent;
    FOnMouseEnter     : TNotifyEvent;
    FOnMouseExit      : TNotifyEvent;
    FAfterMinimized   : TAfterSizeChanged;
    FAfterMaximized   : TAfterSizeChanged;
    FBeforeMoving     : TNotifyEvent;
    FAfterMoving      : TNotifyEvent;
    FAfterClose       : TNotifyEvent;
    FFullRepaint: Boolean;
    FTitleButtonsStyle: TTitleButtonsStyle;
    FTitleBtnBorderSize: Integer;
    procedure SetFillDirection(AFillDirection : TFillDirection);
    procedure SetCaption(AValue : String);
    procedure SetTitleFont(AFont : TFont);
    procedure OnTitleFontChange(Sender : TObject);
    procedure SetDefaultHeight(AValue : Integer);
    procedure SetTitleHeight(AHeight : Integer);
    procedure SetTitleAlignment(AValue : TAlignment);
    procedure SetTitleFillDirect(AValue : TFillDirection);
    procedure SetTitleImage(AValue : TBitmap);
    procedure SetTitleImageAlign(AValue : TTitleImageAlign);
    procedure SetTitleButtons(AValue : TTitleButtons);
    procedure SetPanelCorner(AValue : TPanelCorners);
    procedure SetMinimized(AValue : Boolean);
    procedure SetMaximized(AValue : Boolean);
    procedure SetBGImage(AImage : TBitmap);
    procedure SetBGImageAlign(AImageAlign : TBGImageAlign);
    procedure SetTitleButtonsStyle(AValue: TTitleButtonsStyle);
    procedure SetTitleBtnBorderSize(AValue: Integer);
    procedure SetColors(Index:Integer; Value:TColor);
    procedure SetBools(Index:Integer; Value:Boolean);
  protected
    procedure DrawTitle(ACanvas : TCanvas; ATitleRect : TRect);
    procedure DrawAllTitleButtons(ACanvas : TCanvas; ATitleRect : TRect);
    procedure DrawTitleButton(ACanvas : TCanvas; AButtonRect : TRect; ABtnType : TTitleButton);
    procedure DrawBorder(ACanvas : TCanvas; ARect : TRect; AClient : Boolean); //AClient = true - draw client area border only
    procedure DrawBGImage(ACanvas : TCanvas);
    procedure ForceReDraw;
    procedure Loaded; override;
    procedure SetShape(ARounded : TPanelCorners);
    procedure WMSize(var Message : TMessage); message WM_SIZE;
    procedure MouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure NCHitTest(var Message : TWMNCHitTest); message WM_NCHITTEST;
    procedure NCMouseDown(var Message : TWMNCLBUTTONDOWN); message WM_NCLBUTTONDOWN;
    procedure NCMouseUp(var Message : TWMNCLBUTTONUP); message WM_NCLBUTTONUP;
    procedure NCMouseDblClick(var Message : TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure WMNCPaint(var Message : TWMNCPaint); message WM_NCPAINT;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCACTIVATE(var Message : TWMNCActivate); message WM_NCACTIVATE;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
    property FillGradient : Boolean index 0 read FGradientFill write SetBools default True;
    property FullRepaint: Boolean index 1 read FFullRepaint write SetBools default True;
    property TitleShow : Boolean index 2 read FShowHeader write SetBools default True;
    property Minimized : Boolean index 3 read FMinimized write SetBools default False;
    property Maximized : Boolean index 4 read FMaximized write SetBools default False;
    property TitleShadowOnMoseEnter : Boolean index 5 read FTitleShadowOnMouseEnter write SetBools default True;
    property TitleFillGradient : Boolean index 6 read FTitleGradient write SetBools default True;
    property Movable : Boolean index 7 read FMovable write SetBools default False;
    property Sizable : Boolean index 8 read FSizable write SetBools default False;
    property ShowBorder  : Boolean index 9 read FShowBorder write SetBools default True;
    property Animation : Boolean index 10 read FAnimation write SetBools default True;
    property BGImageTransparent : Boolean index 11 read FBGImageTransparent write SetBools default True;
    property TitleImageTransparent : Boolean index 12 read FTitleImageTransparent write SetBools default True;
    
    property FillDirection : TFillDirection read FFillDirection write SetFillDirection;
    property Caption : String read FCaption write SetCaption;
    property TitleFont : TFont read FTitleFont write SetTitleFont;
    property TitleHeight : Integer read FTitleHeight write SetTitleHeight default  30;
    property TitleAlignment : TAlignment read FTitleAlignment write SetTitleAlignment;
    property ColorStart : TColor index 0 read FStartColor write SetColors default DefaultColorStart;
    property ColorEnd : TColor index 1 read FEndColor write SetColors default DefaultColorStop;
    property TitleColorStart : TColor index 2 read FTitleStartColor write SetColors default DefaultTitleColorStart;
    property TitleColorEnd : TColor index 3 read FTitleEndColor write SetColors default DefaultTitleColorEnd;
    property TitleColor : TColor index 4 read FTitleColor write SetColors default clWhite;
    property TitleBtnBorderColor: TColor index 5 read FTitleBtnBorderColor write SetColors default DefaultBorderColor;
    property TitleBtnBGColor: TColor index 6 read FTitleBtnBGColor write SetColors default DefaultBackdropColor;
    property ColorBorder : TColor index 7 read FBorderColor write SetColors default DefaultBorderColor;

    property TitleImage : TBitmap read FTitleImage write SetTitleImage;
    property TitleFillDirect : TFillDirection read FTitleFillDirect write SetTitleFillDirect;
    property TitleImageAlign : TTitleImageAlign read FTitleImageAlign write SetTitleImageAlign;
    property TitleButtons : TTitleButtons read FTitleButtons write SetTitleButtons;
    property TitleBtnStyle: TTitleButtonsStyle read FTitleButtonsStyle write SetTitleButtonsStyle default tbsRectangle;
    property TitleBtnBorderSize: Integer read FTitleBtnBorderSize write SetTitleBtnBorderSize default 1;
    property DefaultHeight : Integer read FDefaultHeight write SetDefaultHeight default 100;
    property PanelCorner : TPanelCorners read FPanelCorner write SetPanelCorner default [];
    property BGImage : TBitmap read FBGImage write SetBGImage;
    property BGImageAlign : TBGImageAlign read FBGImageAlign write SetBGImageAlign;
    property AfterMinimized : TAfterSizeChanged read FAfterMinimized write FAfterMinimized;
    property AfterMaximized : TAfterSizeChanged read FAfterMaximized write FAfterMaximized;
    property BeforeMove     : TNotifyEvent read FBeforeMoving write FBeforeMoving;
    property AfterMove      : TNotifyEvent read FAfterMoving write FAfterMoving;
    property AfterClose : TNotifyEvent read FAfterClose write FAfterClose;
    property OnTitleClick : TNotifyEvent read FOnTitleClick write FOnTitleClick;
    property OnTitleDblClick : TNotifyEvent read FOnTitleDblClick write FOnTitleDblClick;
    property OnTitleMouseDown : TMouseEvent read FOnTitleMouseDown write FOnTitleMouseDown;
    property OnTitleMouseUp : TMouseEvent read FOnTitleMouseUp write FOnTitleMouseUp;
    property OnTitleMouseEnter: TNotifyEvent read FOnTitleMouseEnter write FOnTitleMouseEnter;
    property OnTitleMouseExit : TNotifyEvent read FOnTitleMouseExit write FOnTitleMouseExit;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit : TNotifyEvent read FOnMouseExit write FOnMouseExit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;
  
   { TDefineButton }
  TDefineButton = class(TVersionControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTransparent: TTransparentMode;
    FModalResult: TModalResult;
    TextBounds: TRect;
    GlyphPos: TPoint;
    FNumGlyphs: TNumGlyphs;
    fColorDown: TColor;
    FColorBorder: TColor;
    FColorShadow: TColor;
    fColorFocused: TColor;
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseIn: Boolean;
    FDefault: Boolean;
    fHasFocusFrame: boolean;
    fColorFlat: TColor;
    FTransBorder: Boolean;
    FFoisChange: Boolean;
    FAutoColor: TColor;
    FAutoStyle: TFontStyles;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure UpdateExclusive;
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure SetDefault(const Value: Boolean);
    procedure SetTransparent (const Value: TTransparentMode);
    procedure SetTransBorder(const Value: Boolean);
    procedure SetFoisChange(const Value: Boolean);
    procedure SetAutoStyle(const Value: TFontStyles);
    function GetMouseIn: Boolean;
  protected
    FState: TButtonState;
    function  GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
    procedure MouseEnter;
    procedure MouseLeave;
    property Transparent: TTransparentMode read FTransparent write SetTransparent default tmNone;
    property HasFocusFrame:boolean read fHasFocusFrame write fHasFocusFrame default true;
    property Default: Boolean read FDefault write SetDefault default False;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property ColorFocused: TColor index 0 read fColorFocused write SetColors default DefaultFocusedColor;
    property ColorDown: TColor index 1 read fColorDown write SetColors default DefaultDownColor;
    property ColorBorder: TColor index 2 read FColorBorder write SetColors default DefaultBorderColor;
    property ColorShadow: TColor index 3 read FColorShadow write SetColors default DefaultShadowColor;
    property ColorFlat: TColor index 4 read fColorFlat write SetColors default DefaultFlatColor;
    property FoisColor: TColor index 5 read FAutoColor write SetColors default DefaultFoisColor;
    property TransBorder: Boolean read FTransBorder write SetTransBorder default false;
    property FoisChange: Boolean read FFoisChange write SetFoisChange default true;
    property FoisStyle: TFontStyles read FAutoStyle write SetAutoStyle default [fsBold];
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property TabStop default true;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0; 
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property MouseIn: Boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;
  
  { TDefinePanel }
  TDefinePanel = class(TVersionCtrlExt)
  private
    FAutoSizeDocking: Boolean;
    FTransparent: Boolean;
    FColorBorder: TColor;
    FBackgropStartColor: TColor;
    FBackgropStopColor: TColor;
    FBackgropOrien: TFillDirection;
    FStyleFace: TStyleFace;
    FAlignment: TAlignment;
    FLocked: Boolean;
    FFullRepaint: Boolean;
    FParentBackgroundSet: Boolean;
    FTransBorder: boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetFillDirect(Value: TFillDirection);
    procedure SetStyleFace(Value: TStyleFace);
    procedure SetAlignment(Value: TAlignment);
    procedure SetTransBorder(Value: boolean);
  protected
    procedure Paint; override;
    procedure SetColors(Index: Integer; Value: TColor); virtual;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure SetParentBackground(Value: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property TransBorder: boolean read FTransBorder write SetTransBorder default false;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Locked: Boolean read FLocked write FLocked default False;
    property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
    property ColorBorder: TColor index 0 read FColorBorder write SetColors default DefaultBorderColor;
    property BackgropStartColor: TColor index 1 read FBackgropStartColor write SetColors default DefaultColorStart;
    property BackgropStopColor: TColor index 2 read FBackgropStopColor write SetColors default DefaultColorStop;
    property BackgropOrien: TFillDirection read FBackgropOrien write SetFillDirect default fdLeftToRight;
    property StyleFace: TStyleFace read FStyleFace write SetStyleFace default fsDefault;
    property Color default clBtnFace;
  public
    constructor Create(AOwner: TComponent); override;
    function GetControlsAlignment: TAlignment; override;
    property ParentBackground stored FParentBackgroundSet;
  end;
  { TDefineLabel }
  TDefineLabel = class(TDefinePanel)
  private
    FTicketSpace: Integer;
    FTicket: TDefineTicket;
    FTicketPosition: TTicketPosition;
  protected
    procedure Loaded; override;
    procedure NewAdjustHeight;
    procedure SetTicketPosition(const Value: TTicketPosition);
    procedure SetLabelSpacing(const Value: Integer);
    procedure SetName(const Value: TComponentName); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetupInternalLabel;
    property Ticket: TDefineTicket read FTicket;
    property TicketPosition: TTicketPosition read FTicketPosition write SetTicketPosition default poLeft;
    property TicketSpace: Integer read FTicketSpace write SetLabelSpacing default 3;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;  
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
  end;
  
  { TDefineProgressBar }
  TDefineProgressBar = class(TVersionGraphic)
  private
    FTransparent: Boolean;
    FSmooth: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FOrientation: TProgressBarOrientation;
    FElementWidth: Integer;
    FElementColor: TColor;
    FBorderColor: TColor;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FStep: Integer;
    procedure SetMin (Value: Integer);
    procedure SetMax (Value: Integer);
    procedure SetPosition (Value: Integer);
    procedure SetStep (Value: Integer);
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetOrientation (Value: TProgressBarOrientation);
    procedure SetSmooth (Value: Boolean);
    procedure CheckBounds;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged (var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure SetTransparent (const Value: Boolean);
  protected
    procedure CalcAdvColors;
    procedure DrawElements;
    procedure Paint; override;
   {$IFDEF DFS_COMPILER_4_UP}
    procedure SetBiDiMode(Value: TBiDiMode); override;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Color default DefaultFlatColor;
    property ColorElement: TColor index 0 read FElementColor write SetColors default $00996633;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors default 50;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default false;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition default 0;
    property Step: Integer read FStep write SetStep default 10;
    property Smooth: Boolean read FSmooth write SetSmooth default false;
  public
    constructor Create (AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy (Delta: Integer);
  end;

  TDefineTitlebar = class(TVersionControl)
  private
   FForm: TCustomForm;
   FWndProcInstance: Pointer;
   FDefProc: LongInt;
   FActive: Boolean;
   FDown: Boolean;
   FOldX, FOldY: Integer;
   FActiveTextColor: TColor;
   FInactiveTextColor: TColor;
   FTitlebarColor: TColor;
   FOnActivate: TNotifyEvent;
   FOnDeactivate: TNotifyEvent;
   procedure FormWndProc(var Message: TMessage);
   procedure DoActivateMessage(var Message: TWMActivate);
   procedure DoActivation;
   procedure DoDeactivation;
   procedure SetActiveTextColor(Value: TColor);
   procedure SetInactiveTextColor(Value: TColor);
   procedure SetTitlebarColor(Value: TColor);
   procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
   procedure CMTextChanged (var Message: TMessage); message CM_TEXTCHANGED;
  protected
   procedure Loaded; override;
   procedure Paint; override;
   procedure SetParent(AParent: TWinControl); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   property ActiveTextColor: TColor read FActiveTextColor write SetActiveTextColor;
   property InactiveTextColor: TColor read FInactiveTextColor write SetInactiveTextColor;
   property TitlebarColor: TColor read FTitlebarColor write SetTitlebarColor;
   property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
   property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  public
    { Public declarations }
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  end;
   { TDefineScrollbarThumb }
  TDefineScrollbarThumb = class(TDefineButton)
  private
    FDown: Boolean;
    FOldX, FOldY: Integer;
    FTopLimit: Integer;
    FBottomLimit: Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color;
  end;

{ TDefineScrollbarTrack }

  TDefineScrollbarTrack = class (TVersionControl)
  private
    FThumb: TDefineScrollbarThumb;

    FKind: TScrollBarKind;

    FSmallChange: Integer;
    FLargeChange: Integer;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;

    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetKind(Value: TScrollBarKind);

    procedure WMSize(var Message: TMessage); message WM_SIZE;

    function ThumbFromPosition: Integer;
    function PositionFromThumb: Integer;

    procedure DoPositionChange;

    procedure DoThumbHighlightColor(Value: TColor);
    procedure DoThumbShadowColor(Value: TColor);
    procedure DoThumbBorderColor(Value: TColor);
    procedure DoThumbFocusedColor(Value: TColor);
    procedure DoThumbDownColor(Value: TColor);
    procedure DoThumbColor(Value: TColor);

    procedure DoHScroll(var Message: TWMScroll);
    procedure DoVScroll(var Message: TWMScroll);
    procedure DoEnableArrows(var Message: TMessage);
    procedure DoGetPos(var Message: TMessage);
    procedure DoGetRange(var Message: TMessage);
    procedure DoSetPos(var Message: TMessage);
    procedure DoSetRange(var Message: TMessage);
    procedure DoKeyDown(var Message: TWMKeyDown);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align;
    property Color;
    property ParentColor;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property SmallChange: Integer read FSmallChange write SetSmallChange;
    property LargeChange: Integer read FLargeChange write SetLargeChange;
    property Position: Integer read FPosition write SetPosition;
    property Kind: TScrollBarKind read FKind write SetKind;
    property Version;
  end;

{ TDefineScrollbarButton }

  TDefineScrollbarButton = class (TDefineButton)
  private
    FNewDown: Boolean;
    FTimer: TTimer;
    FOnDown: TNotifyEvent;
    procedure DoTimer(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property OnDown: TNotifyEvent read FOnDown write FOnDown;
    property Version;
  end;

  { TDefineScrollbar }    
  TFlatOnScroll = procedure (Sender: TObject; ScrollPos: Integer) of object;
  TDefineScrollbar = class(TVersionControl)
  private
    FTrack: TDefineScrollbarTrack;
    FBtnOne: TDefineScrollbarButton;
    FBtnTwo: TDefineScrollbarButton;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FPosition: Integer;
    FKind: TScrollBarKind;
    FButtonHighlightColor: TColor;
    FButtonShadowColor: TColor;
    FButtonBorderColor: TColor;
    FButtonFocusedColor: TColor;
    FButtonDownColor: TColor;
    FButtonColor: TColor;
    FThumbHighlightColor: TColor;
    FThumbShadowColor: TColor;
    FThumbBorderColor: TColor;
    FThumbFocusedColor: TColor;
    FThumbDownColor: TColor;
    FThumbColor: TColor;
    FOnScroll: TFlatOnScroll;
    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetButtonHighlightColor(Value: TColor);
    procedure SetButtonShadowColor(Value: TColor);
    procedure SetButtonBorderColor(Value: TColor);
    procedure SetButtonFocusedColor(Value: TColor);
    procedure SetButtonDownColor(Value: TColor);
    procedure SetButtonColor(Value: TColor);
    procedure SetThumbHighlightColor(Value: TColor);
    procedure SetThumbShadowColor(Value: TColor);
    procedure SetThumbBorderColor(Value: TColor);
    procedure SetThumbFocusedColor(Value: TColor);
    procedure SetThumbDownColor(Value: TColor);
    procedure SetThumbColor(Value: TColor);
    procedure BtnOneClick(Sender: TObject);
    procedure BtnTwoClick(Sender: TObject);
    procedure EnableBtnOne(Value: Boolean);
    procedure EnableBtnTwo(Value: Boolean);
  protected
    procedure DoScroll;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CNHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure CNVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure SBMEnableArrows(var Message: TMessage); message SBM_ENABLE_ARROWS;
    procedure SBMGetPos(var Message: TMessage); message SBM_GETPOS;
    procedure SBMGetRange(var Message: TMessage); message SBM_GETRANGE;
    procedure SBMSetPos(var Message: TMessage); message SBM_SETPOS;
    procedure SBMSetRange(var Message: TMessage); message SBM_SETRANGE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 1;
    property Position: Integer read FPosition write SetPosition default 0;
    property Kind: TScrollBarKind read FKind write SetKind default sbVertical;
    property OnScroll: TFlatOnScroll read FOnScroll write FOnScroll;
    property ButtonHighlightColor: TColor read FButtonHighlightColor write SetButtonHighlightColor;
    property ButtonShadowColor: TColor read FButtonShadowColor write SetButtonShadowColor;
    property ButtonBorderColor: TColor read FButtonBorderColor write SetButtonBorderColor;
    property ButtonFocusedColor: TColor read FButtonFocusedColor write SetButtonFocusedColor;
    property ButtonDownColor: TColor read FButtonDownColor write SetButtonDownColor;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property ThumbHighlightColor: TColor read FThumbHighlightColor write SetThumbHighlightColor;
    property ThumbShadowColor: TColor read FThumbShadowColor write SetThumbShadowColor;
    property ThumbBorderColor: TColor read FThumbBorderColor write SetThumbBorderColor;
    property ThumbFocusedColor: TColor read FThumbFocusedColor write SetThumbFocusedColor;
    property ThumbDownColor: TColor read FThumbDownColor write SetThumbDownColor;
    property ThumbColor: TColor read FThumbColor write SetThumbColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  { TDefineGauge }
  TDefineGauge = class(TVersionGraphic)
  private
    FTransparent: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FBarColor, FBorderColor: TColor;
    FMinValue, FMaxValue, FProgress: LongInt;
    FShowText: Boolean;
    fTextFront: TCaption;
    fTextAfter: TCaption;
    fColorStop: TColor;
    fColorStart: TColor;
    fStyleBars: TStyleOrien;
    fStyleFace: TStyleFace;
    procedure SetShowText(Value: Boolean);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    procedure SetProgress(Value: Longint);
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged (var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure SetTransparent (const Value: Boolean);
    procedure SetTextFront(const Value: TCaption);
    procedure SetTextAfter(const Value: TCaption);
    procedure SetStyleOrien(const Value: TStyleOrien);
    procedure SetStyleFace(const Value: TStyleFace);
  protected
    procedure CalcAdvColors;
    procedure Paint; override;
   {$IFDEF DFS_COMPILER_4_UP}
    procedure SetBiDiMode(Value: TBiDiMode); override;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors default 50;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default False;

    property StyleFace: TStyleFace read fStyleFace write SetStyleFace default DefaultStyleFace;
    property StyleOrien: TStyleOrien read fStyleBars write SetStyleOrien default DefaultStyleHorizontal;
    property StyleColorStart: TColor index 2 read fColorStart write SetColors default DefaultColorStart;
    property StyleColorStop: TColor index 3 read fColorStop write SetColors default DefaultColorStop;
    property Version;
    property Color default $00E0E9EF;
    property ColorBorder: TColor index 0 read FBorderColor write SetColors default DefaultBorderColor;
    property BarColor: TColor index 1 read FBarColor write SetColors default $00996633;
    property Min: Longint read FMinValue write SetMinValue default 0;
    property Max: Longint read FMaxValue write SetMaxValue default 100;
    property Progress: Longint read FProgress write SetProgress;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property TextFront: TCaption read fTextFront write SetTextFront;
    property TextAfter: TCaption read fTextAfter write SetTextAfter;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TDefineGUIScrollBar }
  TDefineGUIScrollBar = class(TVersionGraphic)
  private
    FOnDrawControl: TScrollDrawEvent;
    FX,
    FY,
    FTrackPos: integer;
    FIsStartChange: Boolean;
    FOnChange: TNotifyEvent;
    FLeftBtn,
    FRightBtn,
    FTrackBtn,
    FSpaceLeft,
    FSpaceRight: TRect;
    FTimer: TTimer;
    FDownPos: TScrollBarPos;
    FCurPos: TScrollBarPos;
    FLargeChange,
    FSmallChange: TScrollBarInc;
    FPageSize: integer;
    FPosition,
    FMin: Integer;
    FMax: Integer;
    FAutoHide: boolean;
    FScrollcode: TIScrollCode;
    FScrollMode: TScrollMode;
    FScrollBarKind: TScrollBarKind;
    FOnScroll: TScrollEvent;
    FWaitInterval: Cardinal;           //点击对象之后等待的时间间隔
    fOnEnabledChange: TNotifyEvent;
    FOwnerDraw: Boolean;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageSize(const Value: integer);
    procedure SetLargeChange(const Value: TScrollBarInc);
    procedure SetSmallChange(const Value: TScrollBarInc);
    procedure SetScrollBarKind(const Value: TScrollBarKind);
    procedure OnTimer(Sender: TObject);
    procedure SetPosition(Value: integer);
    procedure SetAutoHide(const Value: boolean);
  protected
    procedure AdjustTrack(Value: Integer);//相同于命名 SetTrackPos
    procedure UpdateHideState; //尝试隐藏自身
    procedure UpdateEnabledState;//更新可用状态
    Function GetSliderRect: TRect;
    Function GetDrawStateBy(const Typ: TDrawScrollBar): TButtonState;
    function CanShowTrack: Boolean;//当 ScrollBar 可视范围太小的时候,必须屏蔽 Track 的显示
    Function GetMinTrackSize: integer;//返回 Track 许可的最小大小
    procedure Changed;
    Function GetValidSize: integer;//获取用于计算的参数 FMax - FMin - FPageSize ... - 1;
    Function GetTrackPos: integer;//根据参数计算 Track 按钮的起始位置:
    Function GetTrackSize: integer;//根据参数计算 Track 按钮的大小:
    Function GetCurTrackSize: Integer;//速度更快的获取 Track 按钮的大小,依赖于 TRect 的计算:
    Function GetSliderSize: integer;//简单计算滑动长度
    procedure FreeTimer;  //释放 TImer
    procedure StartTimer(const Interval: Cardinal);//启动 Timer
    procedure SetDownPos(const Value: TScrollBarPos); //设置鼠标左击对象
    procedure SetCurPos(const value: TScrollBarPos); //设置当前鼠标指向对象
    procedure DoMouseLeavePos(const Value: TScrollBarPos); //鼠标离开对象
    procedure DoMouseEnterPos(const Value: TScrollBarPos);//鼠标进入对象
    procedure DoMouseDownPos(const Value: TScrollBarPos);//鼠标左击对象
    procedure DoMouseUpPos(const Value: TScrollBarPos); //鼠标释放左击对象
    procedure Paint; override;//继承控件不要继承 Paint 事件
    procedure DrawControl(const Typ: TDrawScrollBar; const R: TRect; const State: TButtonState); virtual; //继承这个 DrawControl 去画控件
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
    procedure UpdateScrollBarGUI; //更新控件各个状态的大小和位置
    Function GetMousePos(const X, Y: integer): TScrollBarPos;//返回鼠标所在位置
    procedure Scroll(Const Code:TIScrollCode;const Mode: TScrollMode);// 标准 Scroll
    procedure DoAutoScroll(Const aCode:TIScrollCode; aScrollMode: TScrollMode); //自动 Scroll
    property OnDrawControl: TScrollDrawEvent read FOnDrawControl write FOnDrawControl;
    property OwnerDraw: Boolean Read FOwnerDraw write FOwnerDraw;
    property OnEnabledChange: TNotifyEvent read fOnEnabledChange write FOnEnabledChange;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: integer read FPosition write SetPosition;
    property ScrollBarKind: TScrollBarKind read FScrollBarKind write SetScrollBarKind default sbHorizontal;
    property LargeChange: TScrollBarInc read FLargeChange write SetLargeChange ;
    property SmallChange: TScrollBarInc read FSmallChange write SetSmallChange;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property PageSize: integer read FPageSize write SetPageSize;
  public
    property WaitInterval: Cardinal read FWaitInterval write FWaitInterval;
    property AutoHide: boolean read FAutoHide write SetAutoHide ;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoScroll(Const aMode: TScrollMode; const StartChange: boolean; const ScrollSize: integer);
    procedure DrawArrows(Cav: TCanvas; const v: TDrawArrow;const R: TRect);
    Function IsVertical: Boolean; //为了编码的方便 
  end;

  TDefineGUICtrlList = class;

  TDefineGUICtrlString = Class(TStringList)
  private
    FMoving: Boolean;
    FControl: TDefineGUICtrlList;
  protected
    procedure SetListControl(const aListControl:TDefineGUICtrlList);
  public
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); override;
    function  AddObject(const S: string; AObject: TObject): Integer; override;
    procedure SetTextStr(const Value: string); override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;
  { TDefineGUISelectList }
  TDefineGUISelectList = class(TBits)
  public
    procedure ChangeSelect(const Value: integer);
    procedure ChangeSelectSome(V1, V2: integer);
    procedure Select(const Value: integer);
    procedure UnSelect(const Value: integer);
    procedure SelectAll;
    procedure UnSelectAll;
    procedure SelectSome(V1, V2: integer);
    procedure UnSelectSome(V1, V2: integer);
  end;
  { TDefineGUICtrlSave }
  TDefineGUICtrlSave = class(TVersionCtrlExt)
  private
    FBmp: TBitMap;
    FKeyPage:TKeyFirst;           //键盘改变页面枚举
    FMousePage: TMouseChangePage; //鼠标改变页面枚举
    FWheel:TListControlWheel;
    FActiveItem: integer;
    FDownItem,          //鼠标点击项目
    FMoveItem: integer;
    FCtrlIsClear: Boolean;
    FDownShift: TShiftState;
    FBakList,
    FSelectList: TDefineGUISelectList;
    FMouseDown: boolean;
    FMouseItem: integer; //鼠标指向的项目
    FVBar: TDefineGUIScrollBar;
    FCount,
    FItemIndex,
    FFocusItem,         //获得焦点的项目
    FTopIndex: integer; //顶部项目
    FMultiSelect: boolean;
    FRefreshing: boolean;  //更新项目中.... ?
    FItemHeight: integer; //项目高度
    FWorkRect: TRect;
    FOnItemClick: TListItemEvent;
    FOwnerDraw: Boolean;
    FOnItemDraw: TListItemDrawEvent;
    FOnItemDlbClick: TListItemEvent;
    //为了让 KeyDown 事件支持系统按键:
    procedure CMFONTCHANGED(var msg: TMessage);message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSIZE(var msg: TWMSIZE); message WM_SIZE;
    procedure OnTimer(var Msg: TWMTimer); Message WM_TIMER;
    procedure WMKILLFOCUS(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSETFOCUS(var message: TMessage); message WM_SETFOCUS;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetMultiSelect(const Value: boolean);
    procedure SetSelected(const index: integer; const Value: Boolean);
    procedure SetItemHeight(const Value: integer); virtual;
    procedure SetItemIndex(Value: integer);
    procedure SetTopIndex(Value: integer);
    procedure SetMouseItem(const Index: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetActiveItem(const Value: integer);
    procedure OnVbarScroll(Sender: TObject; const StartChange:boolean;
                           Code:TIScrollCode; Mode:TScrollMode;
                           const ChangeValue: integer);
    procedure SetMouseChangePage(const Value: TMouseChangePage);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetOnDrawScrollBar(const Value: TScrollDrawEvent);
    function GetItemRect(const Index: integer): TRect;
    function GetPageSize: integer;
    function GetTopIndex: integer;
    function  GetOnDrawScrollBar: TScrollDrawEvent;
    function GetSelected(const index: integer): Boolean;
  protected

    //以下两个函数为了处理"多行选择" 和 Ctrl 状态的:
    procedure LoadBakSelectState;
    procedure SaveBakSelectState;
    //画内存,为了滚动显示效果:
    procedure DrawBitMap(bmp: TBitmap; BeginItem, EndItem: integer);
    procedure AdjustSee(value: integer);
    procedure StartTimer(const ID, interval: integer);
    procedure CloseTimer(const ID: integer);
    procedure DrawItem(Cav: TCanvas; const Index: Integer;
                        const R: TRect; const State: TListItemStates);virtual;
    procedure SetFocusItem(const Value: integer; const DoRePaint:boolean);
    procedure SetMouseDownItem(const Value: Integer);
    procedure SimpleSetItemIndex(Value: integer);
    procedure OnVbarEnabledChange(Sender: TObject);
    procedure ItemClick(const Index: integer); dynamic;
    procedure Paint; Override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams);Override;
    procedure KeyDown(var Key: word; Shift: TShiftState); Override;
    procedure KeyUp(var Key: Word; shift: TShiftState); override;
    procedure DblClick; override ;
    procedure Clear;
    procedure UpdatePageSizeOfVbar;
    procedure UpdateMax;
    procedure UpdateTopIndex;
    procedure UpdateWorkRect; 
    procedure BeginUpdate;
    procedure EndUpdate;
    //复制,滚动动画 DC 
    procedure CopyBit(const EndY, startY: Integer;const Source: HDC; forward: boolean);
    procedure MouseEnterItem(const Index: integer); virtual;
    procedure MouseLeaveItem(const Index: integer); virtual;
    procedure MouseDownItem(const Index: integer); virtual;
    procedure MouseUpItem(const Index: integer); virtual;
    procedure CalcSizeOfWoekRect(var R: TRect); virtual;
    procedure Add; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Insert(Index: integer); virtual;
    procedure Move(const CurIndex, NewIndex: Integer); virtual;
    procedure Put(const Index: integer); virtual;
    function VbarCanSee: boolean;
    Function ItemCanSee(const Index: integer): boolean;
    Function IsNoStandardSize: Boolean; //result := WorkRect:客户区 Mod ItemHeight = 0 ;
    Function GetItemRectEx(const VirtualTopIndex, index: integer): TRect;
    property VBar: TDefineGUIScrollBar read FVBar;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect ;
    property OnDrawScrollBar: TScrollDrawEvent read GetOnDrawScrollBar write SetOnDrawScrollBar;
    property OwnerDraw: Boolean Read FOwnerDraw write SetOwnerDraw;
    property OnItemClick: TListItemEvent read FOnItemClick write FOnItemClick;
    property OnItemDlbClick:TListItemEvent read FOnItemDlbClick write FOnItemDlbClick;
    property TopIndex: integer read GetTopIndex write SetTopIndex ;
    property ItemIndex: integer read FItemIndex write SetItemIndex ;
    property WorkRect: TRect read FWorkRect;
    property ItemHeight: integer read FItemHeight write SetItemHeight ;
    property ItemRect[const Index: integer]: TRect read GetItemRect;
    // GetPageSize 返回包含仅一半可视项目的项目
    property PageSize:integer read GetPageSize;
    property Selected[const Index: Integer]: boolean read GetSelected write SetSelected;
    property ActiveItem: integer read FActiveItem write SetActiveItem;
    property Count: Integer read FCount write SetCount;
    property Refreshing : boolean read FRefreshing ;
    property OnItemDraw: TListItemDrawEvent read FOnItemDraw write FOnItemDraw;
  public
    //该函数的计算忽略 X 座标
    Function ItemAtY(const y: integer): integer;
    //该函数的计算包含 X 座标
    Function ItemAtPoint(const X, Y: integer): integer; virtual;
    Function IsItem(const Index: Integer): boolean;
    procedure ToSeeItem(Index: integer);  //如果真的需要执行,并且执行成功那么返回 true
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  { TDefineGUICtrlList }
  TDefineGUICtrlList = class(TDefineGUICtrlSave)
  private
    FGUIStyle: TListControlGUI;
    FItemBorderColor: TColor;
    FItemSelectColor: TColor;
    FItemBrightColor: TColor;
    FItemColor: TColor;
    FItemSpaceColor: TColor;
    FMouseIn: Boolean;
    FFocusColor: TColor;
    FFlatColor: TColor;
    procedure SetGUIStyle(const Value: TListControlGUI);
    procedure SetColors(const Index:Integer;const Value: TColor);
    function GetMouseIn: boolean;
  protected      
    procedure CalcSizeOfWoekRect(var R: TRect); override;
    procedure OnVBarDrawControl(Cav: TCanvas; const Typ: TDrawScrollBar;
                                const R: TRect; const State: TButtonState);
    procedure Paint; override;
    procedure DrawItem(Cav: TCanvas; const Index: Integer;
                       const R: TRect; const State: TListItemStates);override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    property MouseIn         : boolean read GetMouseIn;
    property GUIStyle        : TListControlGUI read FGUIStyle write SetGUIStyle default lcgFlat;
    property GUISelectColor  : TColor index 0 read FItemSelectColor write SetColors default DefaultItemSelectColor;
    property GUIBorderColor  : TColor index 1 read FItemBorderColor write SetColors default DefaultBorderColor;
    property GUIBrightColor  : TColor index 2 read FItemBrightColor write SetColors default DefaultItemBrightColor;
    property GUIColor        : TColor index 3 read FItemColor       write SetColors default DefaultItemColor;
    property GUISpaceColor   : TColor index 4 read FItemSpaceColor  write SetColors default DefaultItemSpaceColor;
    property GUIFocusedColor : TColor index 5 read FFocusColor      write SetColors default clWhite;
    property GUIFlatColor    : TColor index 6 read FFlatColor       write SetColors default DefaultFlatColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  { TDefineGUIListBox }
  TDefineGUIListBox = class(TDefineGUICtrlList)
  private
    FAutoItemHeight: boolean;
    FItems: TDefineGUICtrlString;
    procedure SetItems(const Value: TStrings);
    function  GetItems: TStrings;
    procedure CMSHOWINGCHANGED(var msg: TMessage); message CM_SHOWINGCHANGED;
    procedure CMFONTCHANGED(var msg: TMessage);message CM_FONTCHANGED;
    procedure SetAutoItemHeight(const Value: Boolean);
  protected
    procedure DrawItem(Cav: TCanvas; const Index: Integer;
                       const R: TRect; const State: TListItemStates);override;
    procedure UpdateItemheight;
    property  AutoItemHeight: boolean read FAutoItemHeight write SetAutoItemHeight;
    property  Items: TStrings read GetItems write SetItems;
    property  TabStop default True;
  public
    property    Selected;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    Function    GetCount: integer;
  end;
  { TDefineTreeView }
  TDefineTreeView = class(TVersionTreeView)
  private
    FParentColor: Boolean;
    FFocusedColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    FInterDrawing: boolean;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    function  GetItemsCount: Integer;
  protected
    procedure RedrawBorder(const Clip: HRGN = 0);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure Loaded; override;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property ParentFont default True;
    property AutoSize default False;
    property Ctl3D default False;
    property BorderStyle default bsNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemsCount: Integer read GetItemsCount;
  end;

  { TDefineListView }
  TDrawTitleEvent = procedure (Cnvs: TCanvas; Column: TListColumn;
                               Pressed: Boolean; R: TRect) of object;

  TDefineListView = class(TVersionListView)
  private
    FHeaderHandle: HWND;
    FHeaderInstance: Pointer;
    FDefHeaderProc: Pointer;
    FActiveSection: Integer;
    FHeaderDown: Boolean;
    FParentColor: Boolean;
    FFocusedColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    FOnDrawTitle: TDrawTitleEvent;
    FTitleFaceColor: TColor;
    FTitleCheckColor: TColor;
    FGroundPic: TPicture;
    FGroundHas: Boolean;
    FOnDrawBackground: TLVCustomDrawEvent;
    FGroundStretch: Boolean;
    FAllCheck: Boolean;
    FTransparent: Boolean;
    FTransBit: TBitmap;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    function  GetColumnCount: Integer;
    function  GetItemsCount: Integer;
    procedure SetGroundPic(const Value: TPicture);
    procedure SetGroundHas(const Value: Boolean);
    function  GetHeaderHeight: Integer;
    procedure SetGroundStretch(const Value: Boolean);
    procedure SetAllCheck(const Value: Boolean);
    function GetListCount: integer;
    function GetCheckCount: integer;
    procedure SetTransparent(const Value: Boolean);
  protected
    FCheckInBox: Boolean;
    procedure RedrawBorder(const Clip: HRGN = 0);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure HeaderWndProc(var Message: TMessage);
    procedure DrawTitle(Cnvs: TCanvas; Column: TListColumn; Active, Pressed: Boolean; R: TRect);
    procedure DrawHeader(DC: HDC);
    procedure WndProc(var Message: TMessage); override;
    procedure DrawBackground(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure DrawTransparent(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure Loaded; override;
    function  GetHeaderSectionRect(Index: Integer): TRect;
    property HeaderHeight: Integer read GetHeaderHeight;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ColorTitleFace: TColor index 3 read FTitleFaceColor write SetColors default DefaultTitleFaceColor;
    property ColorTitleCheck: TColor index 4 read FTitleCheckColor write SetColors default DefaultTitleCheckColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property GroundHas: Boolean read FGroundHas write SetGroundHas default false;
    property GroundPic: TPicture read FGroundPic write SetGroundPic;
    property GroundStretch: Boolean read FGroundStretch write SetGroundStretch default false;
    property OnDrawBackground: TLVCustomDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawTitle: TDrawTitleEvent read FOnDrawTitle write FOnDrawTitle;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property ParentFont default True;
    property AutoSize default False;
    property Ctl3D default False;
    property BorderStyle default bsNone;
    property FlatScrollBars default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AllCheck: Boolean read FAllCheck write SetAllCheck default false;
    property ColCount: Integer read GetColumnCount;
    property Count: integer read GetListCount;
    property CheckCount: integer read GetCheckCount;
    property ItemCount: Integer read GetItemsCount;
  end;
  { TDefineGridDraw }
  TDefineGridDraw = class(TVersionDrawGrid)
  private
    FParentColor: Boolean;
    FFocusColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    FLinesColor: TColor;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    function GetMouseIn: boolean;
  protected
    procedure RedrawBorder (const Clip: HRGN = 0);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;AState: TGridDrawState); override;
    property  MouseIn:boolean read GetMouseIn;
    property ColorFocused: TColor index 0 read FFocusColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ColorLines: TColor index 3 read FLinesColor write SetColors default DefaultBorderColor;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  { TDefineGridString}
  TDefineGridString= class;

  TDefineGridStrings = class(TStrings)
  private
    FGrid: TDefineGridString;
    FIndex: Integer;
    procedure CalcXY(Index: Integer; var X, Y: Integer);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AGrid: TDefineGridString; AIndex: Longint);
    function Add(const S: string): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TDefineGridString= class(TDefineGridDraw)
  private
    FData: Pointer;
    FRows: Pointer;
    FCols: Pointer;
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCols(Index: Integer; Value: TStrings);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetRows(Index: Integer; Value: TStrings);
    function EnsureColRow(Index: Integer; IsCol: Boolean): TDefineGridStrings;
    function EnsureDataRow(ARow: Integer): Pointer;
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
  end;
  { TDeinePages }
  TDefinePages = class (TVersionPages)
  private
    FCanvas : TControlCanvas;      // canvas for drawing on with tabOwnerDraw
    FImageList : TImageList;          // link to a TImageList component
    FOnDrawItem : TPageDrawItemEvent;  // Owner draw event
    FOnGlyphMap : TGlyphMapEvent;  // glyph mapping event
    FBorderColor : TColor;
    FHotTrackTab : Integer;
    FBorderRect  : TRect;
    FTabPosition : TPagesPosition;
    FOwnerDraw : Boolean;
    FStyle : TPagesStyle;
    FTabTextAlignment : TAlignment;
//    function  PageIndexToWin (AIndex : Integer) : Integer;
    function  WinIndexToPage (AIndex : Integer) : Integer;
    procedure SetGlyphs (Value : TImageList);
    function  GetMultiline : boolean;
    procedure CNDrawItem (var Msg : TWMDrawItem); message CN_DRAWITEM;
    procedure WMAdjasment (var Msg : TMessage); message TCM_ADJUSTRECT;
//    procedure WMNCPaint (var Message : TWMNCPaint); message WM_NCPAINT;
    procedure WMNCCalcSize (var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMPaint (var Message : TWMPaint); message WM_PAINT;
    procedure WMMouseMove (var Message : TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMSIZE (var Message : TWMSIZE); message WM_SIZE;
    procedure MouseLeave (var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMSysColorChange (var Message: TMessage); message WM_SYSCOLORCHANGE;
    procedure GlyphsChanged (Sender : TObject);
    procedure SetTabPosition (Value : TPagesPosition);
    procedure SetTabTextAlignment (Value : TAlignment);
    procedure SetBorderColor (Value : TColor);
    procedure SetStyle (Value : TPagesStyle);
    procedure SetOwnerDraw (AValue : Boolean);
  protected
    procedure CreateParams (var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure DrawItemInside (AIndex : Integer; ACanvas : TCanvas; ARect : TRect); virtual;
    procedure DrawBorder (ACanvas : TCanvas); virtual;

    procedure DrawTopTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
    procedure DrawBottomTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
    procedure DrawLeftTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
    procedure DrawRightTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
    procedure DrawHotTrackTab (ATabIndex : Integer; AHotTrack : Boolean);
    procedure Loaded; override;
    // for owner draw
    property Canvas : TControlCanvas read FCanvas write FCanvas;
    // republish Multiline as read only
    property MultiLine : boolean read GetMultiline;
    // link to TImageList
    property ImageList : TImageList Read FImageList write SetGlyphs;
    // owner draw event
    property OnDrawItem : TPageDrawItemEvent read FOnDrawItem write FOnDrawItem;
    // glyph map event
    property OnGlyphMap : TGlyphMapEvent read FOnGlyphMap write FOnGlyphMap;

    property OwnerDraw : Boolean read FOwnerDraw write SetOwnerDraw default False;
    property ColorBorder : TColor read FBorderColor write SetBorderColor default DefaultBorderColor;
    property TabPosition : TPagesPosition read FTabPosition write SetTabPosition;
    property TabTextAlignment : TAlignment read FTabTextAlignment write SetTabTextAlignment;
    property Style : TPagesStyle read FStyle write SetStyle;
  public
    procedure UpdateGlyphs; virtual;
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TDefineSheetBGStyle = (bgsNone, bgsGradient, bgsTileImage, bgsStrechImage);

  TDefineSheet = class (TVersionSheet)
  private
    FCanvas : TControlCanvas;
    FColor : TColor;
    FGradientStartColor : TColor;
    FGradientEndColor : TColor;
    FGradientFillDir : TFillDirection;
    FImageIndex : Integer;
    FShowTabHint : Boolean;
    FTabHint : String;
    FBGImage : TBitmap;
    FBGStyle : TDefineSheetBGStyle;

    procedure SetColor (AValue : TColor);
    procedure WMNCPaint (var Message : TWMNCPaint); message WM_NCPAINT;
    procedure WMPaint (var Message : TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetImageIndex (AIndex : Integer);

    procedure SetBGImage (AValue : TBitmap);
    procedure SetBGStyle (AValue : TDefineSheetBGStyle);
    procedure SetGradientStartColor (AValue : TColor);
    procedure SetGradientEndColor (AValue : TColor);
    procedure SetGradientFillDir (AValue : TFillDirection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
  published
    property Color : TColor read FColor write SetColor;
    property ImageIndex : Integer read FImageIndex write SetImageIndex default -1;
    property ShowTabHint : Boolean read FShowTabHint write FShowTabHint default False;
    property TabHint : String read FTabHint write FTabHint;
    property BGImage : TBitmap read FBGImage write SetBGImage;
    property BGStyle : TDefineSheetBGStyle read FBGStyle write SetBGStyle;
    property GradientStartColor : TColor read FGradientStartColor write SetGradientStartColor;
    property GradientEndColor : TColor read FGradientEndColor write SetGradientEndColor;
    property GradientFillDir : TFillDirection read FGradientFillDir write SetGradientFillDir;
  end;

  { TDefineBarcode }
  TDefineBarcode = class(TVersionControl)
   private
    fText           : string;
    FModul          : integer;
    FRatio          : double;
    FCodeType       : TDefineBarcodeType;
    FRotateType     : TDefineBarcodeRotation;
    fBarHeight      : Integer;
    fBorderWidth    : Byte;
    fBarColor       : TColor;
    fBarTop         : Byte;

    fTypName        : String;
    fColor          : TColor;
    FShowText       : boolean;
    FCheckSum       : boolean;
    fCheckOdd       : Boolean;
    fTransparent    : boolean;
    fAutoSize       : Boolean;
    procedure SetModul(const Value:Integer);
    procedure SetRotateType(const Value: TDefineBarcodeRotation);
    procedure SetRatio(const Value: double);
    procedure SetCodeType(const Value: TDefineBarcodeType);
    procedure SetText(const Value: string);
    procedure SetBarHeight(const Value: Integer);
    procedure SetBorderWidth(const Value: Byte);
    procedure SetBarColor(const Value: TColor);
    procedure SetBarTop(const Value: Byte);
    procedure SetColor(const Value: TColor);
    procedure FontChange(sender : TObject);
    procedure SetBools(Index: Integer; Value: Boolean);
   protected
    fBitmap: TBitmap;
    function Code_25ILeaved  : string;
    function Code_25ITrial   : string;
    function Code_25Matrix   : string;
    function Code_39         : string;
    function Code_128        : string;
    function Code_93         : string;
    function Code_MSI        : string;
    function Code_PostNet    : string;
    function Code_CodaBar    : string;
    function Code_EAN8       : string;
    function Code_EAN13      : string;
    function Code_UPC_A      : string;
    function Code_UPC_EODD   : string;
    function Code_UPC_EVEN   : string;
    function Code_Supp5      : string;
    function Code_Supp2      : string;
    Function MakeData        : string;
    function MakeBarText     : String;
    function GetTypName      : String;
    function GetProLine      : Integer;
    function DoCheckSumming(const Data: string;OddCheck:Boolean=True): string;
    function GetCheckLen(CodeType: TDefineBarcodeType; Data: String): String;
    function SetLen(pI:byte): string;
    function ClearNotText(Value: String): String;
    function MakeModules : TDefineBarcodeModules;

    procedure DrawBarcode;
    procedure OneBarProps(Data: Char; var Width: Integer; var lt: TDefineBarcodeLines);
    procedure GetABCED(var a, b, c, d, orgin: TPoint; xadd, Width, Height: Integer);
    procedure DrawEAN13Text(Canvas: TCanvas; width,wBorder: Integer);
    procedure DrawEAN8Text(Canvas: TCanvas; width, wBorder: Integer);
    procedure DrawUPC_AText(Canvas: TCanvas; width, wBorder: Integer);
    procedure DrawUPC_EText(Canvas: TCanvas; width, wBorder: Integer);
    procedure Paint; override;
    procedure WMSize (var Message: TWMSize); message WM_SIZE;
    property Data      : String read MakeData;
    property BarText   : String read MakeBarText;
    property Modules   : TDefineBarcodeModules read MakeModules;
    property ProLine   : Integer read GetProLine;
    property BarCode: String  read GetTypName write fTypName;
    property Rotate: TDefineBarcodeRotation read FRotateType write SetRotateType;
    property Modul: Integer read fModul write SetModul;
    property Ratio: double read fRatio write SetRatio;
    property CodeType: TDefineBarcodeType read FCodeType write SetCodeType default EAN13;
    property Text: string  read fText write SetText;
    property LineHeight: Integer read fBarHeight write SetBarHeight;
    property BorderWidth: Byte read fBorderWidth write SetBorderWidth;
    property LineTop: Byte read fBarTop write SetBarTop;
    property Color: TColor read FColor write SetColor default clWhite;
    property LineColor: TColor read fBarColor write SetBarColor default clBlack;
    property AutoSize: Boolean index 0 read fAutoSize write SetBools default True;
    property Checksum: boolean index 1 read FCheckSum write SetBools default FALSE;
    property CheckOdd: Boolean index 2 read fCheckOdd write SetBools default true;
    property ShowText: boolean index 3 read FShowText write SetBools default True;
    property Transparent: boolean index 4 read fTransparent write SetBools default false;
   public
    constructor Create(Owner:TComponent); override;
    destructor destroy;override;
    property Bitmap: TBitmap read fBitmap;
  end;

  { TFlatButton }
  TFlatButton = class(TDefineButton)
  published
    property Transparent;
    property TransBorder;
    property HasFocusFrame;
    property Default;
    property AllowAllUp;
    property ColorFocused;
    property ColorDown;
    property ColorBorder;
    property ColorShadow;
    property ColorFlat;
    property GroupIndex;
    property Action;
    property Down;
    property Caption;
    property Enabled;
    property Font;
    property FoisChange;
    property FoisColor;
    property FoisStyle;
    property Glyph;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Spacing;
    property ModalResult; 
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
   {$IFDEF DFS_DELPHI_4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
  end;
  
  { TFlatColorBox }
  TFlatColorBox = class(TDefineColorBox)
  published
    property Color;
    property ColorArrow;
    property ColorArrowBackground;
    property ColorBorder;
    property ColorHighlight;
    property ColorBoxWidth;
    property ShowNames;
    property Value;
    property Language;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ImeMode;
    property ImeName;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
  end;
  { TFlatComboBox }
  TFlatComboBox = class(TDefineComboBox)
  published
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property CharCase;
    property Style;
    property ParentColor;
    property ColorArrow;
    property ColorArrowBackground;
    property ColorBorder;
    property ColorFlat;
    property ColorFocued;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property ReadOnly;
    property Font;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property ImeMode;
    property ImeName;
    property Text;
    property Visible;
    property ItemIndex;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;
  { TFlatEdit }
  TFlatEdit = class(TDefineEdit)
  published
    property Alignment;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property CharCase;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property ImeMode;
    property ImeName;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
  
  { TFlatMemo }
  TFlatMemo = class(TDefineMemo)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Version;
    property Align;
    property Alignment;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ScrollBars;
    property TabOrder;
    property TabStop;
    property Visible;
    property Lines;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property ImeMode;
    property ImeName;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  { TFlatPanel }
  TFlatPanel = class(TDefinePanel)
  published
    property Constraints;
    property Action;
    property Transparent;
    property TransBorder;
    property Alignment;
    property Locked;
    property FullRepaint;
    property ColorBorder;
    property BackgropStartColor;
    property BackgropStopColor;
    property BackgropOrien;
    property StyleFace;
    property Color;
    property Caption;
    property Font;
    property ParentColor;
    property UseDockManager;
    property Enabled;
    property Visible;
    property DockSite;
    property Align;
    property AutoSize;
    property Cursor;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
   {$IFDEF DFS_DELPHI_4_UP}
    //property AutoSize;
    //property UseDockManager;
    property Anchors;
    property BiDiMode;
    //property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property ParentBiDiMode;
    //property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
   {$ENDIF}
   {$IFDEF DFS_DELPHI_5_UP}
    property OnContextPopup;
   {$ENDIF}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
  { TFlatMaskEdit }
  TFlatMaskEdit = class(TDefineMask)
  published
    property Ticket;
    property TicketPosition;
    property TicketSpace;

    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Alignment;
    property CharCase;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ImeMode;
    property ImeName;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnValidate;
  end;    

  { TFlatSplitter }
  TFlatSplitter = class(TDefineSplitter)
  published
    property Color;
    property ColorFocused;
    property ColorBorder;
    property MinSize;
    property OnMoved;
    property Align;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;
  end;
  { TFlatSpeedButton }
  TFlatSpeedButton = class(TDefineSpeed)
  published
    property Transparent;
    property TransBorder;
    property Version;
    property AllowAllUp;
    property ColorFocused;
    property ColorDown;
    property ColorBorder;
    property ColorShadow;
    property ColorFlat;
    property GroupIndex;
    property Down;
    property Caption;
    property Enabled;
    property Font;     
    property FoisChange;
    property FoisColor;
    property FoisStyle;
    property Glyph;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ModalResult;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
   {$IFDEF DFS_DELPHI_4_UP}
    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
  end;
  TFlatPucker = class(TDefinePucker)
  published
    property Action;
    property FillGradient;
    property ColorStart;
    property ColorEnd;
    property Enabled;
    property FillDirection;
    property TitleShow;
    property Minimized;
    property Maximized;
    property Caption;
    property TitleFont;
    property TitleHeight;
    property TitleAlignment;
    property TitleShadowOnMoseEnter;
    property TitleFillGradient;
    property TitleColorStart;
    property TitleColorEnd;
    property TitleColor;
    property TitleImage;
    property TitleFillDirect;
    property TitleImageAlign;
    property TitleImageTransparent;
    property TitleButtons;
    property TitleBtnStyle;
    property TitleBtnBorderColor;
    property TitleBtnBGColor;
    property TitleBtnBorderSize;
    property Animation;
    property DefaultHeight;
    property Movable;
    property Sizable;
    property ShowBorder;
    property ColorBorder;
    property PanelCorner;
    property BGImage;
    property BGImageAlign;
    property BGImageTransparent;
    property Color;
    property Align;
    property Visible;
    property TabOrder;
    property TabStop;
    property DragMode;
    property OnResize;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property AfterMinimized;
    property AfterMaximized;
    property BeforeMove;
    property AfterMove;
    property AfterClose;
    property OnTitleClick;
    property OnTitleDblClick;
    property OnTitleMouseDown;
    property OnTitleMouseUp;
    property OnTitleMouseEnter;
    property OnTitleMouseExit;
    property OnMouseEnter;
    property OnMouseExit;
  end;
  { TFlatCheckBox }
  TFlatCheckBox = class(TDefineCheckBox)
  published
    property Transparent;
    property AllowGrayed;
    property Caption;
    property Checked;
    property ColorFocused;
    property ColorDown;
    property ColorChecked;
    property Color;
    property ColorBorder;
    property Action;
    property Enabled;
    property Font;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;
   { TFlatCheckListBox }
  TFlatCheckListBox = class(TDefineListChecks)
  published
    property Skin;
    property Caption;
    property Sorted;
    property Items;
    property Align;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ParentColor;
    property OnClick;
    property OnChange;
    property OnClickCheck;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  { TDefineGroupBox }
  TFlatGroupBox = class(TDefineGroupBox)
  published
    property Action;
    property Transparent;
    property Alignment;
    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property TabOrder;
    property TabStop;
    property Hint;
    property HelpContext;
    property ColorBorder;
    property BackgropStartColor;
    property BackgropStopColor;
    property BackgropOrien;
    property StyleFace;
    property Border;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property ParentBiDiMode;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
  { TFlatRadioButton }
  TFlatRadioButton = class(TDefineRadioButton)
  published
    property Action;
    property Transparent;
    property Version;
    property Caption;
    property Checked;
    property ColorFocused;
    property ColorDown;
    property ColorChecked;
    property ColorBorder;
    property Color;
    property Enabled;
    property Font;
    property GroupIndex;
    property Layout;
    property ParentColor;
    property ParentFont;
    property Anchors;
    property Constraints;
    property DragKind;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEndDock;
    property OnStartDock;
  end;
  { TFlatRadioGroup }
  TFlatRadioGroup = class(TDefineRadioGroup)
  published
    property Transparent;
    property Alignment;
    property Items;
    property ItemIndex;
    property Columns;
    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property TabOrder;
    property TabStop;
    property Hint;
    property TopOffset;
    property ColorBorder;
    property BackgropStartColor;
    property BackgropStopColor;
    property BackgropOrien;
    property StyleFace;
    property Border;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
  { TFlatListBox }

  TFlatListBox = class(TDefineListBox)
  published
    property Caption;
    property Skin;
    property Align;
    property Items;
    property MultiSelect;
    property Sorted;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnChange;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  { TFlatListBoxExt }
  TFlatListBoxExt = class(TDefineListBoxExt)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Style;
    property AutoComplete;
    property Align;
    property Anchors;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  { TFlatCheckListExt }
  TFlatCheckListExt = class(TDefineCheckListExt)
  published
    property OnClickCheck;
    property HeaderColor;
    property HeaderBkColor;
    property AllowGrayed;
    property Flat;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Align;
    property Anchors;
    property AutoComplete;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TFlatGauge = class(TDefineGauge)
  published
    property AdvColorBorder;
    property Transparent;
    property UseAdvColors;
    property StyleFace;
    property StyleOrien;
    property StyleColorStart;
    property StyleColorStop;
    property Version;
    property Color;
    property ColorBorder;
    property BarColor;
    property Min;
    property Max;
    property Progress;
    property ShowText;
    property TextFront;
    property TextAfter;
    property Align;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
   {$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
  end;
  TFlatProgressBar = class(TDefineProgressBar)
  published
    property Transparent;
    property Align;
    property Cursor;
    property Color;
    property ColorElement;
    property ColorBorder;
    property AdvColorBorder;
    property UseAdvColors;
    property Orientation;
    property Enabled;
    property ParentColor;
    property Visible;
    property Hint;
    property ShowHint;
    property PopupMenu;
    property ParentShowHint;
    property Min;
    property Max;
    property Position;
    property Step;
    property Smooth;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
   {$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
  end;
  TFlatScrollbar = class(TDefineScrollbar)
  published
    property Min;
    property Max;
    property SmallChange;
    property LargeChange;
    property Position;
    property Kind;
    property OnScroll;
    property ButtonHighlightColor;
    property ButtonShadowColor;
    property ButtonBorderColor;
    property ButtonFocusedColor;
    property ButtonDownColor;
    property ButtonColor;
    property ThumbHighlightColor;
    property ThumbShadowColor;
    property ThumbBorderColor;
    property ThumbFocusedColor;
    property ThumbDownColor;
    property ThumbColor;
    property Version;
    property Align;
    property Color;
    property ParentColor;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnStartDrag;
  end;
  TFlatTitlebar = class(TDefineTitlebar)
  published
   property ActiveTextColor;
   property InactiveTextColor;
   property TitlebarColor;
   property Align;
   property Font;
   property Caption;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnActivate;
   property OnDeactivate;
  end;
  
  TFlatFloat = class(TDefineFloat)
  published
    property Digits;
    property Precision;
    property FloatFormat;
    property EditorEnabled;
    property Increment;
    property MaxValue;
    property MinValue;
    property Value;
    property Alignment;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property AutoSelect;
    property AutoSize;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ImeMode;
    property ImeName;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
  
  TFlatInteger = class(TDefineInteger)
  published
    property Increment;
    property MaxValue;
    property MinValue;
    property Value;
    property EditorEnabled;
    property Alignment;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property AutoSelect;
    property AutoSize;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property Font;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
  
  TFlatIPEdit = class(TDefineIPEdit)
  published
    property IPAddress;
    property Text;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Alignment;
    property AutoSelect;
    property CharCase;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnValidate;
  end;
  
  TFlatLabel = class(TDefineLabel)
  published
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property Transparent;
    property TransBorder;
    property Alignment;
    property Locked;
    property FullRepaint;
    property ColorBorder;
    property BackgropStartColor;
    property BackgropStopColor;
    property BackgropOrien;
    property StyleFace;
    property Color;
    property Caption;
    property Font;
    property ParentColor;
    property UseDockManager;
    property Enabled;
    property Visible;
    property Align;
    property AutoSize;
    property Cursor;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
   {$IFDEF DFS_DELPHI_4_UP}
    //property AutoSize;
    //property UseDockManager;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property ParentBiDiMode;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
   {$ENDIF}
   {$IFDEF DFS_DELPHI_5_UP}
    property OnContextPopup;
   {$ENDIF}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
  { TFlatPages }
  TFlatPages = class (TDefinePages)
  published
    property ImageList;
    property OnDrawItem;
    property OnGlyphMap;
    property OwnerDraw;
    property ColorBorder;
    property TabPosition;
    property TabTextAlignment;
    property Style;
  end;
  { TFlatSheet }
  TFlatSheet = class (TDefineSheet)
  published
    property Color;
    property ImageIndex;
    property ShowTabHint;
    property TabHint;
    property BGImage;
    property BGStyle;
    property GradientStartColor;
    property GradientEndColor;
    property GradientFillDir;
  end;

  { TFlatTreeView }
  TFlatTreeView = class(TDefineTreeView)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property ChangeDelay;
    property Constraints;
    property DragKind;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property ToolTips;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChanging;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnStartDock;
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
    property Items;
  end;
  { TFlatListView }
  TFlatListView = class(TDefineListView)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ColorTitleFace;
    property ColorTitleCheck;
    property GroundHas;
    property GroundPic;
    property GroundStretch;
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property Checkboxes;
    property Columns;
    property ColumnClick;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property Transparent;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnDrawTitle;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    //property OnCustomDraw;
    property OnDrawBackground;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;
  TFlatGUIScrollBar = class(TDefineGUIScrollBar)
  published
    property OnDrawControl;
    property OwnerDraw;
    property OnEnabledChange;
    property OnScroll;
    property OnChange;
    property Position;
    property ScrollBarKind;
    property LargeChange;
    property SmallChange;
    property Max;
    property Min;
    property PageSize;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Align;
    property Color;
    property Caption;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property ParentColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  { TFlatGUIListBox }
  TFlatGUIListBox = class(TDefineGUIListBox)
  published
    property AutoItemHeight;
    property Items;
    property ItemHeight;
    property TopIndex;
    property Hint;
    property TabOrder;
    property TabStop;
    property OwnerDraw;
    property ActiveItem;
    property ItemIndex;
    property GUISelectColor;
    property GUIBorderColor;
    property GUIBrightColor;
    property GUIColor;
    property GUISpaceColor;
    property GUIStyle;
    property GUIFlatColor;
    property GUIFocusedColor;
    property MultiSelect;
    property Count;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Align;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property ParentColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;     
    property OnDrawScrollBar;
    property OnItemClick;
    property OnItemDlbClick;
    property OnItemDraw;
  end;
  { TDefineDrawGrid }
  TFlatDrawGrid = class(TDefineGridDraw)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ColorLines;
    property ParentColor;
    property Align;
    property Anchors;
    property BiDiMode;
    property ColCount;
    property Constraints;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property Options;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;
  { TFlatStringGrid }
  TFlatStringGrid = class(TDefineGridString)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ColorLines;
    property ParentColor;
    property Align;
    property Anchors;
    property BiDiMode;
    property ColCount;
    property Constraints;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property Options;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;
  TFlatBarcode = class(TDefineBarcode)
  published
    property BarCode;
    property Rotate;
    property Modul;
    property Ratio;
    property CodeType;
    property Text;
    property LineHeight;
    property BorderWidth;
    property LineTop;
    property Color;
    property LineColor;
    property AutoSize;
    property Checksum;
    property CheckOdd;
    property ShowText;
    property Transparent;
    property ShowHint;
    property ParentFont;
    property Font;
    property Height;
    property Width;
    property Top;
    property Left;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R FlatCtrls.res}

uses Clipbrd, FlatCnsts;


{ TDefineTicket }
constructor TDefineTicket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name     := 'Ticket';  { do not localize }
  SetSubComponent(True);
  if Assigned(AOwner) then Caption := '';//AOwner.Name;
  AutoSize := True;
end;

procedure TDefineTicket.AdjustBounds;
begin
  inherited AdjustBounds;
  if Owner is TDefineEdit then begin
    with Owner as TDefineEdit do begin
         SetTicketPosition(TicketPosition);
    end;
  end;
  if Owner is TDefineComboBox then begin
    with Owner as TDefineComboBox do
      SetTicketPosition(TicketPosition);
  end;
  if Owner is TDefineColorBox then begin
    with Owner as TDefineColorBox do
      SetTicketPosition(TicketPosition);
  end;
  if Owner is TDefineLabel then begin
    with Owner as TDefineLabel do
      SetTicketPosition(TicketPosition);
  end;
end;

function TDefineTicket.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TDefineTicket.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TDefineTicket.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TDefineTicket.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TDefineTicket.SetHeight(const Value: Integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TDefineTicket.SetWidth(const Value: Integer);
begin
  SetBounds(Left, Top, Value, Height);
end;

{ TDefineEdit }

procedure TDefineEdit.SetupInternalLabel;
begin
  if not(csDesigning in ComponentState) then begin
     fHintLabel := TLabel.Create(Self);
     with fHintLabel do begin
      Parent       := self;
      OnClick      := LabelMouseEnter;
      AutoSize     := false;
      Visible      := false;
      Transparent  := True;
      FocusControl := self;
      Font.Assign(self.Font);
     end;
  end;
  if (DefaultHasTicket)and(not Assigned(FTicket)) then
  begin
   FTicket := TDefineTicket.Create(self);
   FTicket.FreeNotification(Self);
   FTicket.AutoSize     := True;
   FTicket.Transparent  := True;
   FTicket.FocusControl := Self;
  end;
end;

constructor TDefineEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle     := ControlStyle - [csFramed];
  ParentFont       := True;
  AutoSize         := False;
  Ctl3D            := False;
  BorderStyle      := bsNone;
  FFocusColor      := clWhite;
  FBorderColor     := DefaultBorderColor;
  FFlatColor       := DefaultFlatColor;
  FParentColor     := True; 
  FAlignment       := taLeftJustify;
  FTicketPosition  := poLeft;
  FTicketSpace     := 3;
  SetupInternalLabel;
end;

destructor TDefineEdit.Destroy;
begin
  if Assigned(fHintLabel) then fHintLabel.Free;
  if Assigned(FTicket) then FTicket.Free;
  inherited destroy;
end;

procedure TDefineEdit.RedrawBorder(const Clip: HRGN);
var
  Attrib:TBorderAttrib;
begin
  with Attrib do
  begin
   Ctrl        := self;
   FocusColor  := ColorFocused;
   BorderColor := ColorBorder;
   FlatColor   := ColorFlat;
   MouseState  := MouseIn;
   FocusState  := Focused;
   DesignState := ComponentState;
   HasBars     := false;
   BoldState   := false;
  end;
  Color := DrawEditBorder(Attrib,Clip);
  if (not(csDesigning in ComponentState))and(Assigned(fHintLabel)) then
  begin
      if not Focused then
         fHintLabel.Visible := self.Text = ''
      else
         fHintLabel.Visible := False;
      if fHintLabel.Visible then
      begin
         fHintLabel.Font.Assign(self.Font);
         fHintLabel.Width   := self.Width;
         fHintLabel.Top     := (self.Height-fHintLabel.Height-2) div 2;
         fHintLabel.Left    := 0;
         fHintLabel.Caption := self.Hint;
      end;
  end;
end;

procedure TDefineEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Params.Style := Params.Style or ES_MULTILINE or Aligns[FAlignment];
  end;
end;

procedure TDefineEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = Char(VK_RETURN)) then
      Key := #0;
end;

procedure TDefineEdit.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
         FFlatColor := TForm(Parent).Color;
      RedrawBorder(0);
    end;
  end;
end;     

procedure TDefineEdit.LabelMouseEnter(Sender: TObject);
begin
  if (not(csDesigning in ComponentState))and(Assigned(fHintLabel)) then begin
     fHintLabel.Visible := false;
     self.SetFocus;
  end;
end;

procedure TDefineEdit.SetTicketPosition(const Value: TTicketPosition);
begin
  if Assigned(FTicket) then
  begin
    FTicketPosition := Value;
    SetTicketPoint(Value,Self,Ticket,FTicketSpace);
  end;
end;

procedure TDefineEdit.SetTicketSpace(const Value: Integer);
begin
  if Assigned(FTicket) then
  begin
     FTicketSpace := Value;
     SetTicketPosition(FTicketPosition);
  end;
end;

procedure TDefineEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineEdit.SetParent(AParent: TWinControl);
begin
  if Assigned(FTicket) then
  begin
     FTicket.Parent  := AParent;
     FTicket.Visible := Visible;
  end;
  inherited SetParent(AParent); 
end;

procedure TDefineEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.BiDiMode := BiDiMode;
end;

procedure TDefineEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.Visible := Visible;
end;

procedure TDefineEdit.SetName(const Value: TComponentName);
var ChangeText:boolean;
begin
  if Assigned(FTicket) then
  begin         //csDesigning
   ChangeText := (csDesigning in ComponentState) and ((FTicket.GetTextLen = 0) or
                 (CompareText(FTicket.Caption, Name) = 0));
   if ChangeText then FTicket.Caption := Value;
  end;
  inherited SetName(Value);
  if (csDesigning in ComponentState)and(Assigned(FTicket))and
    ((GetTextLen = 0)or(CompareText(Text, Name) = 0)) then
      Text := '';
end;

procedure TDefineEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTicket) and (Operation = opRemove) then
     FTicket := nil;
end;

procedure TDefineEdit.CMSysColorChange(var Message: TMessage);
begin
  if (Parent <> nil)and(FParentColor) then
      FFlatColor := TForm(Parent).Color;
  RedrawBorder(0);
end;

procedure TDefineEdit.CMParentColorChanged(var Message: TWMNoParams);
begin
  if (Parent <> nil)and(FParentColor) then
      FFlatColor := TForm(Parent).Color;
  RedrawBorder(0);
end;

procedure TDefineEdit.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusColor  := Value;
    1: FBorderColor := Value;
    2: begin
        FFlatColor   := Value;
        FParentColor := False;
       end;
  end;
  RedrawBorder(0);
end;

procedure TDefineEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder(0);
  end;
end;

procedure TDefineEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := False;
     RedrawBorder(0);
  end;
end;

procedure TDefineEdit.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + 6;
end;

procedure TDefineEdit.Loaded;
begin
  inherited Loaded;
  //if not(csDesigning in ComponentState) then
  //begin
     NewAdjustHeight;
  //end;
end;

procedure TDefineEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) and Assigned(fHintLabel) then
  begin
     if fHintLabel.Visible then
        fHintLabel.Visible := false;
     if (not fHintLabel.Visible) and (Text = '') then
        fHintLabel.Visible := True;
  end;
end;

procedure TDefineEdit.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor= (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  if assigned(FTicket) then FTicket.Enabled := Enabled;
  if (not(csDesigning in ComponentState))and(assigned(fHintLabel)) then
     fHintLabel.Enabled  := Enabled;
  RedrawBorder(0);
end;

procedure TDefineEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
  if (not(csDesigning in ComponentState))and(assigned(fHintLabel)) then
    fHintLabel.Font.Assign(Font);
end;

procedure TDefineEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    RedrawBorder(0);
    SelectAll;
  end;
end;

procedure TDefineEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TDefineEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TDefineEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TDefineEdit.SetAlignment(const Value: TAlignment);
begin
  If FAlignment <> Value Then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TDefineEdit.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

procedure TDefineEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  NewAdjustHeight;
end;

{ TDefineInteger }

procedure ResetBounds(Self:TWinControl; Spin:TDefineSpin);
begin
 with Self do begin
  SetEditRect(Handle, Clientwidth, ClientHeight, Spin.Width);
  Spin.SetBounds(Width - Spin.Width - 5, 0, Spin.Width, Height - 6);
 end;
end;

constructor TDefineInteger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle         := ControlStyle - [csSetCaption];
  FButton              := TDefineSpin.Create(Self);
  FButton.Parent       := Self;
  FButton.Width        := 32;
  FButton.Height       := 10;
  FButton.Visible      := True;
  FButton.FocusControl := Self;
  FButton.OnUpClick    := UpClick;
  FButton.OnDownClick  := DownClick;
  Text                 := '0';
  FIncrement           := 1;
  FMaxValue            := 0;
  FMinValue            := 0;
  FEditorEnabled       := True;
end;

destructor TDefineInteger.Destroy;
begin
  FButton.Free;
  FButton := nil;
  inherited Destroy;
end;

procedure TDefineInteger.UpClick(Sender: TObject);
begin
  if ReadOnly then
     MessageBeep(0)
  else
     Value := Value + FIncrement;
end;

procedure TDefineInteger.DownClick (Sender: TObject);
begin
  if ReadOnly then
     MessageBeep(0)
  else
     Value := Value - FIncrement;
end;

procedure TDefineInteger.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:   UpClick(Self);
    VK_DOWN: DownClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

function TDefineInteger.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in ['0'..'9',#8,#13]);
  if not FEditorEnabled and Result then
    Result := False;
end;

procedure TDefineInteger.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then
  inherited KeyPress(Key);
end;

procedure TDefineInteger.WMSize(var Message: TWMSize);
begin
  inherited;
  if Button <> nil then begin
     ResetBounds(Self,Button);
  end;
end;

function TDefineInteger.CheckValue(NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
       Result := FMinValue
    else
      if NewValue > FMaxValue then
         Result := FMaxValue;
  end;
end;

procedure TDefineInteger.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TDefineInteger.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TDefineInteger.CMExit(var Message: TCMExit);
begin
  inherited;
  if Text = '' then
     Value := 0;
  if CheckValue(Value) <> Value then
     SetValue(Value)
  else
     SetValue(Value);
end;

function TDefineInteger.GetValue: LongInt;
begin
  if Text = '' then
     Text := '0';
  try
    result := StrToInt(Text);
  except
    result := FMinValue;
  end;
end;

procedure TDefineInteger.SetValue(NewValue: LongInt);
begin
  Text := IntToStr(CheckValue(NewValue));
end;

procedure TDefineInteger.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
     SelectAll;
  inherited;
end;

procedure TDefineInteger.Loaded;
begin
  ResetBounds(Self,Button);
  inherited Loaded;
end;

procedure TDefineInteger.CreateWnd;
begin
  inherited CreateWnd;
  ResetBounds(Self,Button);
end;

procedure TDefineInteger.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Text = '' then begin
     Text := '0';
  end;
  Value := CheckValue(StrToInt(Text));
end;

{ TDefineFloat }

constructor TDefineFloat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle         := ControlStyle - [csSetCaption];
  FButton              := TDefineSpin.Create(Self);
  FButton.Parent       := Self;
  FButton.Width        := 32;
  FButton.Height       := 10;
  FButton.Visible      := True;
  FButton.FocusControl := Self;
  FButton.OnUpClick    := UpClick;
  FButton.OnDownClick  := DownClick;
  Text := '0' + FormatSettings.DecimalSeparator + '00';
  FDigits              := 2;
  FPrecision           := 9;
  FIncrement           := 0.5;
  FEditorEnabled       := True;
end;

destructor TDefineFloat.Destroy;
begin
  FButton.Free;
  FButton := nil;
  inherited Destroy;
end;

procedure TDefineFloat.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
      VK_UP: UpClick(Self);
    VK_DOWN: DownClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TDefineFloat.KeyPress(var Key: Char);
begin
  if (not IsValidChar(Key))or((key='.') and (pos('.',Text)>0)) then begin
    Key := #0;
    MessageBeep(0)
  end;

  if Key <> #0 then
  inherited KeyPress(Key);
end;

function TDefineFloat.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [FormatSettings.DecimalSeparator, '0'..'9',#8,#13,#46]);
  if not FEditorEnabled and Result then
    Result := False;
end;

procedure TDefineFloat.WMSize(var Message: TWMSize);
begin
  inherited;
  if Button <> nil then begin
     ResetBounds(Self,Button);
  end;
end;

function TDefineFloat.CheckValue(Value: Extended): Extended;
begin
  Result := Value;
  if (FMaxValue <> FMinValue) then begin
    if Value < FMinValue then
       Result := FMinValue
    else
      if Value > FMaxValue then
        Result := FMaxValue;
  end;
end;

procedure TDefineFloat.UpClick(Sender: TObject);
begin
  if ReadOnly then
    MessageBeep(0)
  else
    Value := Value + FIncrement;
end;

procedure TDefineFloat.DownClick(Sender: TObject);
begin
  if ReadOnly then
    MessageBeep(0)
  else
    Value := Value - FIncrement;
end;

procedure TDefineFloat.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TDefineFloat.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TDefineFloat.CMExit(var Message: TCMExit);
begin
  inherited;
  if (Text = '')or(Text = '￥')or(Text = '.') then
     Value := 0;
  if CheckValue(Value) <> Value then
     SetValue(Value)
  else
     SetValue(Value);
end;

function TDefineFloat.GetValue: Extended;
var
  s: string;
begin
  try
    s := Text;
    while Pos(FormatSettings.CurrencyString, S) > 0 do
      Delete(S, Pos(FormatSettings.CurrencyString, S), Length(FormatSettings.CurrencyString));
    while Pos(#32, S) > 0 do
      Delete(S, Pos(#32, S), 1);
    while Pos(FormatSettings.ThousandSeparator, S) > 0 do
      Delete(S, Pos(FormatSettings.ThousandSeparator, S), Length(FormatSettings.ThousandSeparator));

    //Delete negative numbers in format Currency
    if Pos('(', S) > 0 then
    begin
      Delete(S, Pos('(', S), 1);
      if Pos(')', S) > 0 then
        Delete(S, Pos(')', S), 1);
      Result := StrToFloat(S)*-1;
    end
    else
      Result := StrToFloat(S);
  except
    Result := FMinValue;
  end;
end;

procedure TDefineFloat.SetFloatFormat(Value: TFloatFormat);
begin
  FFloatFormat := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TDefineFloat.SetDigits(Value: Integer);
begin
  FDigits := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TDefineFloat.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TDefineFloat.SetValue(Value: Extended);
begin
  Text := FloatToStrF(CheckValue(Value), FloatFormat, Precision, Digits);
end;

procedure TDefineFloat.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TDefineFloat.Loaded;
begin
  ResetBounds(Self,Button);
  inherited Loaded;
end;

procedure TDefineFloat.CreateWnd;
begin
  inherited CreateWnd;
  ResetBounds(Self,Button);
end;

procedure TDefineFloat.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Text = '' then begin
     Text := '0';
  end;
  Value := GetValue;
end;

{ TDefineMemo }
constructor TDefineMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle  := ControlStyle - [csFramed];
  ParentFont    := True;
  AutoSize      := False;
  Ctl3D         := False;
  BorderStyle   := bsNone;
  FFocusColor   := clWhite;
  FBorderColor  := DefaultBorderColor;
  FFlatColor    := DefaultFlatColor;
  FParentColor  := True;
  FMouseIn      := False;
end;

procedure TDefineMemo.RedrawBorder(const Clip: HRGN);
var
  Attrib:TBorderAttrib;
begin
  with Attrib do
  begin
   Ctrl        := self;
   FocusColor  := ColorFocused;
   BorderColor := ColorBorder;
   FlatColor   := ColorFlat;
   MouseState  := MouseIn;
   FocusState  := Focused;
   DesignState := ComponentState;
   HasBars     := ScrollBars = ssBoth;
   BoldState   := false;
  end;
  Color := DrawEditBorder(Attrib,Clip);
end;

procedure TDefineMemo.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
      RedrawBorder(0);
    end;
  end;
end;       

procedure TDefineMemo.CMSysColorChange(var Message: TMessage);
begin
    if (Parent <> nil)and(FParentColor) then
      FFlatColor := TForm(Parent).Color;
    RedrawBorder(0);
end;

procedure TDefineMemo.CMParentColorChanged(var Message: TWMNoParams);
begin
    if (Parent <> nil)and(FParentColor) then
       FFlatColor := TForm(Parent).Color;
    RedrawBorder(0);
end;

procedure TDefineMemo.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusColor    := Value;
    1: FBorderColor   := Value;
    2: begin
         FFlatColor   := Value;
         FParentColor := False;
       end;
  end;
  RedrawBorder(0);
end;

procedure TDefineMemo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder(0);
  end;
end;

procedure TDefineMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := False;
     RedrawBorder(0);
  end;
end;

procedure TDefineMemo.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder(0);
end;

procedure TDefineMemo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TDefineMemo.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TDefineMemo.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TDefineMemo.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

function TDefineMemo.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefineIPEdit }

function TDefineIPEdit.Replace(Start, Len: Integer): integer;
var t,s:String;
begin
  s := Text;
  t := trim(copy(Text,Start-Len,Len));
  if t <> '' then begin
   if StrToInt(t)>255 then begin
     delete(s,Start-Len,Len);
     insert('255',s,Start-Len);
     Text      := s;
     SelStart  := Start-4;
     SelLength := Len;
   end;
  end;
  result := SelStart;
end;

procedure TDefineIPEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  SetIPText(Text);
end;

constructor TDefineIPEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EditMask := IPMaskStr;
  Text     := IPStart;
end;

function TDefineIPEdit.GetIPText: String;
begin
  result := self.Text;
  while Pos(#32,Result) > 0 do
        delete(Result,Pos(#32,Result),1);
end;

procedure TDefineIPEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
end;

procedure TDefineIPEdit.SetIPText(const Value: String);
var i:integer;
    t:TIPChar;
    s:string;
begin
if fIPAddress <> Value then begin
 if Value <> '' then begin
    s := '';
    for i:=1 to Length(Value) do begin
        if Value[i] in ['0'..'9','.'] then
           s := s + Value[i];
    end;
    if Length(s)>0 then
    begin
    if s[Length(s)]<>'.' then
       s:=s+'.';
    IPEmpty(IPText);
    i:=1;
    while (pos('.',s)>0)and(i<=4) do begin
     t:=Trim(Copy(s,1,Pos('.',s)-1));
     if t <> '' then begin
        if StrToInt(t) > 255 then
           IPValue(IPText,I,'255')
        else begin
          case Length(t) of
           1:t := #32+t+#32;
           2:t := #32+t;
          end;
        IPValue(IPText,I,t);
        end;
     end;
     s:=copy(s,Pos('.',s)+1,Length(s));
     Inc(I);
    end;
    end;
    fIPAddress := format('%s.%s.%s.%s',[IPText.NO1,IPText.NO2,IPText.NO3,IPText.NO4]);
 end else begin
    fIPAddress := IPStart;
 end;
end;
Text := fIPAddress;
end;

procedure TDefineIPEdit.CMExit(var Message: TCMExit);
begin
 if IsMasked and not (csDesigning in ComponentState) then
    SetIPText(Text);
 inherited;
end;

function TDefineIPEdit.GetInx: integer;
var inx:integer;
begin
 GetSel(Result,inx);
end;

procedure TDefineIPEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TDefineIPEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
 if IsMasked then begin
  if SelStart <= 4 then
     Replace( 4,3)
  else if SelStart <=  8 then
     Replace( 8,3)
  else if SelStart <= 12 then
     Replace(12,3)
  else
     Replace(16,3);
  end;
 inherited KeyUp(Key,Shift);
end;

{ TDefineComboBox }

procedure TDefineComboBox.SetupInternalLabel;
begin
  if DefaultHasTicket then begin
   if Assigned(FTicket) then exit;

   FTicket := TDefineTicket.Create(Self);
   FTicket.FreeNotification(Self);
   FTicket.Transparent  := True;
   FTicket.FocusControl := Self;
  end;
end;

constructor TDefineComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csFixedHeight] + [csOpaque];
  TControlCanvas(Canvas).Control := self;
  FButtonWidth    := 16;
  FSysBtnWidth    := GetSystemMetrics(SM_CXVSCROLL);
  FListInstance   := MakeObjectInstance(ListWndProc);
  FDefListProc    := nil;
  ItemHeight      := 13;
  FArrowColor     := clBlack;
  FArrowBackgroundColor := $00C5D6D9;
  FFocusedColor   := clWhite;
  FFlatColor      := DefaultFlatColor;
  FParentColor    := True;
  FBorderColor    := DefaultBorderColor;
  FReadOnly       := false;
  FTicketPosition := poLeft;
  FTicketSpace    := 3;
  SetBounds(0,0,120,20);
  SetupInternalLabel;
end;

destructor TDefineComboBox.Destroy;
begin
  FreeObjectInstance(FListInstance);
  inherited Destroy;
end;

procedure TDefineComboBox.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FArrowColor   := Value;
    1: FArrowBackgroundColor := Value;
    2: FBorderColor  := Value;
    3: FFlatColor    := Value;
    4: FFocusedColor := Value;
  end;
  if index = 3 then
     FParentColor := False;
  Invalidate;
end;

procedure TDefineComboBox.CMSysColorChange(var Message: TMessage);
begin
  if FParentColor then begin
     if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineComboBox.InvalidateSelection;
var
  R: TRect;
begin
  R := ClientRect;
  InflateRect(R, -2, -3);
  R.Left := R.Right - FButtonWidth - 8;
  Dec(R.Right, FButtonWidth + 3);
  if(GetFocus = Handle) and not DroppedDown then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  if(GetFocus = Handle) and not DroppedDown then
  begin
    R := ClientRect;
    InflateRect(R, -3, -3);
    Dec(R.Right, FButtonWidth + 2);
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWindow;
  end;
  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
end;

procedure TDefineComboBox.CMParentColorChanged(var Message: TWMNoParams);
begin
  if FParentColor then begin
     if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineComboBox.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_PARENTNOTIFY) then
    case LoWord(Message.wParam) of
      WM_CREATE:
        if FDefListProc <> nil then
        begin
          SetWindowLong(FListHandle, GWL_WNDPROC, Longint(FDefListProc));
          FDefListProc := nil;
          FChildHandle := Message.lParam;
        end
        else
          if FChildHandle = 0 then
            FChildHandle := Message.lParam
          else
            FListHandle := Message.lParam;
      end
  else
    if (Message.Msg = WM_WINDOWPOSCHANGING) then
      if Style in [csDropDown, csSimple] then
        SetWindowPos( EditHandle, 0,
          0, 0, ClientWidth - FButtonWidth - 2 * 2 - 4, Height - 2 * 2 - 2,
          SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOREDRAW);
  inherited;
  if Message.Msg = WM_CTLCOLORLISTBOX then
  begin
    SetBkColor(Message.wParam, ColorToRGB(Color));
    Message.Result := CreateSolidBrush(ColorToRGB(Color));
  end;
end;

procedure TDefineComboBox.ListWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_WINDOWPOSCHANGING:
      with TWMWindowPosMsg(Message).WindowPos^ do
      begin
        // size of the drop down list
        if Style in [csDropDown, csDropDownList] then
          cy := (GetFontHeight(Font)-2) * Min(DropDownCount, Items.Count) + 4
        else
          cy := (ItemHeight) * Min(DropDownCount, Items.Count) + 4;
        if cy <= 4  then
          cy := 10;
      end;
    else
      with Message do
        Result := CallWindowProc(FDefListProc, FListHandle, Msg, WParam, LParam);
  end;
end;

procedure TDefineComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  inherited;
  if (ComboWnd = EditHandle) then
    case Message.Msg of
      WM_SETFOCUS, WM_KILLFOCUS:
        SetSolidBorder;
    end;
end;

procedure TDefineComboBox.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    Color := FFocusedColor;
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TDefineComboBox.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    Color := FFlatColor;
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TDefineComboBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
  if Assigned(FTicket) then FTicket.Enabled := Enabled;
end;

procedure TDefineComboBox.CNCommand(var Message: TWMCommand);
var
  R: TRect;
begin
  inherited;
  if Message.NotifyCode in [1, 9, CBN_DROPDOWN, CBN_SELCHANGE] then
  begin
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
  if (Message.NotifyCode in [CBN_CLOSEUP]) then
  begin
    R := GetButtonRect;
    Dec(R.Left, 2);
    InvalidateRect(Handle, @R, FALSE);
  end;
end;

procedure TDefineComboBox.WMKeyDown(var Message: TMessage);
var
  S: String;
begin
  S := Text;
  inherited;
  if not (Style in [csSimple, csDropDown]) and(Text <> S) then
    InvalidateSelection;
end;

procedure TDefineComboBox.WMPaint(var Message: TWMPaint);
var
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := BeginPaint(Handle, PS);
  try
    R := PS.rcPaint;
    if R.Right > Width - FButtonWidth - 4 then
      R.Right := Width - FButtonWidth - 4;
    FillRect(DC, R, Brush.Handle);
    if RectInRect(GetButtonRect, PS.rcPaint) then
      PaintButton;
    ExcludeClipRect(DC, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
    PaintWindow(DC);
    if(Style = csDropDown) and DroppedDown then
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Right := Width - FButtonWidth - 3;
      Canvas.Brush.Color := clWindow;
      Canvas.FrameRect(R);
    end
    else
      if Style <> csDropDown then
        InvalidateSelection;
  finally
    EndPaint(Handle, PS);
  end;
  RedrawBorders;
  Message.Result := 0;
end;

procedure TDefineComboBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorders;
end;

procedure TDefineComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ItemHeight := 13;
  RecreateWnd;
end;

function TDefineComboBox.GetButtonRect: TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);
  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;

procedure TDefineComboBox.PaintButton;
var
  R: TRect;
  x, y: Integer;
begin
  R := GetButtonRect;
  InflateRect(R, 1, 0);

  Canvas.Brush.Color := FArrowBackgroundColor;
  Canvas.FillRect(R);
  Canvas.Brush.Color := FBorderColor;
  Canvas.FrameRect(R);

  x :=(R.Right - R.Left) div 2 - 6 + R.Left;
  if DroppedDown then
    y :=(R.Bottom - R.Top) div 2 - 1 + R.Top
  else
    y :=(R.Bottom - R.Top) div 2 - 1 + R.Top;

  if Enabled then
  begin
    Canvas.Brush.Color := FArrowColor;
    Canvas.Pen.Color := FArrowColor;
    if DroppedDown then
      Canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      Canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end
  else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    if DroppedDown then
      Canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      Canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
    Dec(x); Dec(y);
    Canvas.Brush.Color := clGray;
    Canvas.Pen.Color := clGray;
    if DroppedDown then
      Canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      Canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end;
  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth, 0, ClientWidth, ClientHeight);
end;

procedure TDefineComboBox.PaintBorder;
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  DC := GetWindowDC(Handle);

  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  Dec(R.Right, FButtonWidth + 1);
  try
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
    WindowBrush  := CreateSolidBrush(ColorToRGB(Color));
    if(not(csDesigning in ComponentState) and
     (Focused or(MouseIn and not(Screen.ActiveControl is TDefineComboBox)))) then
       Color := FFocusedColor
    else
       Color := FFlatColor;
    FrameRect(DC, R, BtnFaceBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
  finally
    ReleaseDC(Handle, DC);
  end;
  DeleteObject(WindowBrush);
  DeleteObject(BtnFaceBrush);
end;

function TDefineComboBox.GetSolidBorder: Boolean;
begin
  Result :=((csDesigning in ComponentState) and Enabled) or
   (not(csDesigning in ComponentState) and
   (DroppedDown or(GetFocus = Handle) or(GetFocus = EditHandle)) );
end;

procedure TDefineComboBox.SetSolidBorder;
var
  sb: Boolean;
begin
  sb := GetSolidBorder;
  if sb <> FSolidBorder then begin
    FSolidBorder := sb;
    RedrawBorders;
  end;
end;

procedure TDefineComboBox.RedrawBorders;
begin
  PaintBorder;
  if Style <> csSimple then
     PaintButton;
end;

procedure TDefineComboBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.BiDiMode := BiDiMode;
end;

procedure TDefineComboBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.Visible := Visible;
end;

procedure TDefineComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(AComponent = FTicket) and(Operation = opRemove) then
     FTicket := nil;
end;

procedure TDefineComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineComboBox.SetTicketPosition(const Value: TTicketPosition);
begin
  if FTicket = nil then exit;
  FTicketPosition := Value;
  SetTicketPoint(Value,Self,Ticket,FTicketSpace);
end;

procedure TDefineComboBox.SetTicketSpace(const Value: Integer);
begin
  if assigned(FTicket) then FTicketSpace := Value;
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineComboBox.SetName(const Value: TComponentName);
begin
  if assigned(FTicket) then begin
   if(csDesigning in ComponentState) and((FTicket.GetTextLen = 0) or
     (CompareText(FTicket.Caption, Name) = 0)) then
       FTicket.Caption := Value;
  end;
  inherited SetName(Value);
  if csDesigning in ComponentState then
     Text := '';
end;

procedure TDefineComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FTicket = nil then exit;
  FTicket.Parent := AParent;
  FTicket.Visible := True;
end;   

procedure TDefineComboBox.SetParentColor(const Value: Boolean);
begin
  if Value <> FParentColor then begin
    FParentColor := Value;
    if FParentColor then begin
      if Parent <> nil then
         FFlatColor := TForm(Parent).Color;
      RedrawBorders;
    end;
  end;
end;

procedure TDefineComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if(GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorders;
  end;
end;

procedure TDefineComboBox.CMMouseLeave(var Message: TMessage);
begin
 inherited;
 if MouseIn then begin
    FMouseIn := False;
    RedrawBorders;
 end;
end;

procedure TDefineComboBox.KeyPress(var Key: Char);
begin
  if FReadOnly then begin
     MessageBeep(0);
     Key := #0;
  end else inherited KeyPress(Key);
end;

procedure TDefineComboBox.SetReadOnly(const Value: boolean);
begin
  if FReadOnly <> Value then begin
     FReadOnly := Value;
     if FEditHandle > 0 then
        SendMessage(FEditHandle, EM_SETREADONLY, Ord(Value), 0);
  end;
end;

procedure TDefineComboBox.CreateWnd;
begin
  inherited CreateWnd;
  if FEditHandle > 0 then
     SendMessage(FEditHandle, EM_SETREADONLY, Ord(FReadOnly), 0);  
end;

function TDefineComboBox.GetMouseIn: boolean;
begin
  result := FMouseIn;
end;

procedure TDefineComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  if Assigned(OnChange) then OnChange(self);
end;

function TDefineComboBox.GetItemsCount: integer;
begin
  result := inherited Items.Count;
end;

{ TDefineColorBox }

procedure TDefineColorBox.SetupInternalLabel;
begin
  if DefaultHasTicket then begin
   if Assigned(FTicket) then exit;
   FTicket := TDefineTicket.Create(Self);
   FTicket.FreeNotification(Self);
   FTicket.Transparent  := True;
   FTicket.FocusControl := Self;
  end;
end;

constructor TDefineColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csFixedHeight] + [csOpaque];
  TControlCanvas(Canvas).Control := Self;
  FColorDlg        := TColorDialog.Create(Self);
  Style            := csOwnerDrawFixed;
  FButtonWidth     := 16;
  FSysBtnWidth     := GetSystemMetrics(SM_CXVSCROLL);
  FListInstance    := MakeObjectInstance(ListWndProc);
  FDefListProc     := nil;
  FArrowColor      := clBlack;
  FArrowBackgroundColor := $00C5D6D9;
  FBorderColor     := DefaultBorderColor;
  FHighlightColor  := clHighlight;
  FShowNames       := True;
  FColorBoxWidth   := 30;
  FValue           := clBlack;
  FTicketPosition  := poLeft;
  FTicketSpace     := 3;
  fLanguage        := lgChinese;
  SetBounds(0,0,120,20);
  SetupInternalLabel;  
end;

destructor TDefineColorBox.Destroy;
begin
  FColorDlg.Free;
  FreeObjectInstance(FListInstance);
  inherited;
end;

procedure TDefineColorBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineColorBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.BiDiMode := BiDiMode;
end;

procedure TDefineColorBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.Visible := Visible;
end;

procedure TDefineColorBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(AComponent = FTicket) and(Operation = opRemove) then
     FTicket := nil;
end;

procedure TDefineColorBox.SetName(const Value: TComponentName);
begin
  if Assigned(FTicket) then begin
   if(csDesigning in ComponentState) and((FTicket.GetTextLen = 0) or
    (CompareText(FTicket.Caption, Name) = 0)) then begin
    FTicket.Caption := Value;
    case fLanguage of
      lgChinese:FTicket.Caption := StdColorCN;
      lgEnglish:FTicket.Caption := StdColorEN;
    end;
   end;
  end;
  inherited SetName(Value);
end;

procedure TDefineColorBox.SetParent(AParent: TWinControl);
begin
  inherited  SetParent(AParent);
  if FTicket = nil then exit;
  FTicket.Parent := AParent;
  FTicket.Visible := True;
end;

procedure TDefineColorBox.SetLanguage(const Value: TLanguage);
var Item:Integer;
begin
  if(fLanguage <> Value)and(Items.Count>=StdColorCount) then begin
    fLanguage := Value;
    for Item := Low(StdColors) to High(StdColors) do begin
        case Value of
         lgChinese : Items[Item] := StdColors[Item].cnName;
         lgEnglish : Items[Item] := StdColors[Item].enName;
        end;
    end;
    if Assigned(FTicket) then begin
     case fLanguage of
      lgChinese : FTicket.Caption := StdColorCN;
      lgEnglish : FTicket.Caption := StdColorEN;
     end;
    end;
    for Item := 0 to Pred(Items.Count) do
    begin
      if TColor(Items.Objects[Item]) = FValue then
       begin
        ItemIndex := Item;
        Change;
        Break;
       end;
    end;
  end;
end;

procedure TDefineColorBox.SetTicketSpace(const Value: Integer);
begin
  FTicketSpace := Value;
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineColorBox.SetTicketPosition(const Value: TTicketPosition);
begin
  if FTicket = nil then exit;
  FTicketPosition := Value;
  SetTicketPoint(Value,Self,Ticket,FTicketSpace);;
end;

procedure TDefineColorBox.CreateWnd;
var
  I: Integer;
  ColorName: string;
begin
  inherited CreateWnd;
  Clear;
  for I := Low(StdColors) to High(StdColors) do begin
    case fLanguage of
      lgChinese : ColorName := StdColors[I].cnName;
      lgEnglish : ColorName := StdColors[I].enName;
    end;
    Items.AddObject(ColorName, TObject(StdColors[I].Value));
  end;
  ItemIndex := 0;
  Change;
end;

procedure TDefineColorBox.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FArrowColor           := Value;
    1: FArrowBackgroundColor := Value;
    2: FBorderColor          := Value;
    3: FHighlightColor       := Value;
  end;
  Invalidate;
end;

procedure TDefineColorBox.WndProc(var Message: TMessage);
begin
  if(Message.Msg = WM_PARENTNOTIFY) then
    case LoWord(Message.wParam) of
      WM_CREATE:
        if FDefListProc <> nil then
        begin
          SetWindowLong(FListHandle, GWL_WNDPROC, Longint(FDefListProc));
          FDefListProc := nil;
          FChildHandle := Message.lParam;
        end
        else
          if FChildHandle = 0 then
            FChildHandle := Message.lParam
          else
            FListHandle := Message.lParam;
      end
  else
    if(Message.Msg = WM_WINDOWPOSCHANGING) then
      if Style in [csDropDown, csSimple] then
        SetWindowPos( EditHandle, 0,
          0, 0, ClientWidth - FButtonWidth - 2 * 2 - 4, Height - 2 * 2 - 2,
          SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOREDRAW);
  inherited;
  if Message.Msg = WM_CTLCOLORLISTBOX then
  begin
    SetBkColor(Message.wParam, ColorToRGB(Color)); 
    Message.Result := CreateSolidBrush(ColorToRGB(Color));
  end;
end;

procedure TDefineColorBox.ListWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_WINDOWPOSCHANGING:
      with TWMWindowPosMsg(Message).WindowPos^ do
      begin
        // size of the drop down list
        if Style in [csDropDown, csDropDownList] then
           cy :=(GetFontHeight(Font)-2) * Min(DropDownCount, Items.Count) + 4
        else
           cy :=(ItemHeight) * Min(DropDownCount, Items.Count) + 4;
        if cy <= 4  then
           cy := 12;
      end;
    else
      with Message do
        Result := CallWindowProc(FDefListProc, FListHandle, Msg, WParam, LParam);
  end;
end;

procedure TDefineColorBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  inherited;
  if(ComboWnd = EditHandle) then
    case Message.Msg of
      WM_SETFOCUS, WM_KILLFOCUS:
        SetSolidBorder;
    end;
end;

procedure TDefineColorBox.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    if not(Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TDefineColorBox.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    if not(Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TDefineColorBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FTicket.Enabled := Enabled;
  Invalidate;
end;

procedure TDefineColorBox.CNCommand(var Message: TWMCommand);
var
  R: TRect;
begin
  inherited;
  if Message.NotifyCode in [1, 9, CBN_DROPDOWN, CBN_SELCHANGE] then
  begin
    if not(Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
  if(Message.NotifyCode in [CBN_CLOSEUP]) then
  begin
    R := GetButtonRect;
    Dec(R.Left, 2);
    InvalidateRect(Handle, @R, FALSE);
  end;
end;

procedure TDefineColorBox.WMKeyDown(var Message: TMessage);
var
  S: String;
begin
  S := Text;
  inherited;
  if not(Style in [csSimple, csDropDown]) and(Text <> S) then
    InvalidateSelection;
end;

procedure TDefineColorBox.WMPaint(var Message: TWMPaint);
var
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := BeginPaint(Handle, PS);
  try
    R := PS.rcPaint;
    if R.Right  > Width - FButtonWidth - 4 then
       R.Right := Width - FButtonWidth - 4;
    FillRect(DC, R, Brush.Handle);
    if RectInRect(GetButtonRect, PS.rcPaint) then
       PaintButton;
    ExcludeClipRect(DC, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
    PaintWindow(DC);
    if(Style = csDropDown) and DroppedDown then
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Right := Width - FButtonWidth - 3;
      Canvas.Brush.Color := clWindow;
      Canvas.FrameRect(R);
    end
    else
      if Style <> csDropDown then
        InvalidateSelection;
  finally
    EndPaint(Handle, PS);
  end;
  RedrawBorders;
  Message.Result := 0;
end;

procedure TDefineColorBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorders;
end;

procedure TDefineColorBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ItemHeight := 13;
  RecreateWnd;
end;

procedure TDefineColorBox.InvalidateSelection;
var
  R: TRect;
begin
  R := ClientRect;
  InflateRect(R, -2, -3);
  R.Left := R.Right - FButtonWidth - 8;
  Dec(R.Right, FButtonWidth + 3);
  if(GetFocus = Handle) and not DroppedDown then
    Canvas.Brush.Color := FHighLightcolor
  else
    Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  if(GetFocus = Handle) and not DroppedDown then
  begin
    R := ClientRect;
    InflateRect(R, -3, -3);
    Dec(R.Right, FButtonWidth + 2);
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWindow;
  end;
  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
end;

function TDefineColorBox.GetButtonRect: TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);
  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;

procedure TDefineColorBox.PaintButton;
var
  R: TRect;
  x, y: Integer;
begin
  R := GetButtonRect;
  InflateRect(R, 1, 0);

  Canvas.Brush.Color := FArrowBackgroundColor;
  Canvas.FillRect(R);
  Canvas.Brush.Color := FBorderColor;
  Canvas.FrameRect(R);

  x :=(R.Right - R.Left) div 2 - 6 + R.Left;
  if DroppedDown then
    y :=(R.Bottom - R.Top) div 2 - 1 + R.Top
  else
    y :=(R.Bottom - R.Top) div 2 - 1 + R.Top;

  if Enabled then begin
    canvas.Brush.Color := FArrowColor;
    canvas.Pen.Color := FArrowColor;
    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end else begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color := clGray;
    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end;
  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth, 0, ClientWidth, ClientHeight);
end;

procedure TDefineColorBox.PaintBorder;
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  DC := GetWindowDC(Handle);

  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  Dec(R.Right, FButtonWidth + 1);
  try
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
    WindowBrush := CreateSolidBrush(ColorToRGB(Color));

    FrameRect(DC, R, BtnFaceBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
  finally
    ReleaseDC(Handle, DC);
  end;
  DeleteObject(WindowBrush);
  DeleteObject(BtnFaceBrush);
end;

function TDefineColorBox.GetSolidBorder: Boolean;
begin
 Result :=((csDesigning in ComponentState) and Enabled) or
   (not(csDesigning in ComponentState) and
   (DroppedDown or(GetFocus = Handle) or(GetFocus = EditHandle)) );
end;

procedure TDefineColorBox.SetSolidBorder;
var
 sb: Boolean;
begin
 sb := GetSolidBorder;
  if sb <> FSolidBorder then
   begin
    FSolidBorder := sb;
    RedrawBorders;
   end;
end;

procedure TDefineColorBox.RedrawBorders;
begin
  PaintBorder;
  if Style <> csSimple then PaintButton;
end;

procedure TDefineColorBox.SetShowNames(Value: Boolean);
begin
 if Value <> FShowNames then
  begin
   FShowNames := Value;
   Invalidate;
  end;
end;

procedure TDefineColorBox.SetColorValue(Value: TColor);
var
 Item: Integer;
 CurrentColor: TColor;
begin
 if(ItemIndex < 0) or(Value <> FValue) then
  begin
   for Item := 0 to Pred(Items.Count) do
    begin
     CurrentColor := TColor(Items.Objects[Item]);
      if CurrentColor = Value then
       begin
        FValue := Value;
         if ItemIndex <> Item then ItemIndex := Item;
        Change;
        Break;
       end;
    end;
  end;
end;

procedure TDefineColorBox.SetColorBoxWidth(Value: Integer);
begin
 if Value <> FColorBoxWidth then
  begin
   FColorBoxWidth := Value;
  end;
 Invalidate;
end;

procedure TDefineColorBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  with ARect do begin
   Inc(Top,    1);
   Inc(Left,   1);
   Dec(Right,  1);
   Dec(Bottom, 1);
   if FShowNames then begin
      Right := Left + FColorBoxWidth;
   end else begin
      Dec(Right, 5);
   end;
  end;
  with Canvas do begin
    Safer := Brush.Color;
    if(odSelected in State) then begin
      Brush.Color := FHighlightColor;
    end else begin
      Brush.Color := Color;
    end;
    FillRect(Rect);
    Pen.Color   := clBlack;
    Rectangle(ARect);
    Brush.Color := ColorToRgb(TColor(Items.Objects[Index]));
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect)
    finally
      Brush.Color := Safer;
    end;
    if FShowNames then begin
      StrPCopy(Text, Items[Index]);
      Rect.Left   := ARect.Right + 5;
      Brush.Style := bsClear;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      Brush.Style := bsSolid;
    end;
  end;
end;

procedure TDefineColorBox.Click;
begin
  if ItemIndex >= 0 then
  begin
    if(Items[ItemIndex] = StdCustomCN)or(Items[ItemIndex] = StdCustomEN) then
    begin
      if not FColorDlg.Execute then
         Exit;
      Items.Objects[ItemIndex] := TObject(FColorDlg.Color);
    end;
    Value := TColor(Items.Objects[ItemIndex]);
  end;
  inherited Click;
end;

function TDefineColorBox.AddColor(ColorName: String; Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do begin
    if UpperCase(ColorName) = UpperCase(Items[I]) then begin
       Result := False;
       Exit;
    end;
  end;
  Items.InsertObject(Items.Count - 1, ColorName, TObject(Color));
  Result := True;
end;

function TDefineColorBox.DeleteColorByName(ColorName: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do begin
    if UpperCase(ColorName) = UpperCase(Items[I]) then begin
      Items.Delete(I);
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TDefineColorBox.DeleteColorByColor(Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do begin
    if Color = TColor(Items.Objects[I]) then begin
      Items.Delete(I);
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{ TDefineSplitter }

constructor TDefineSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alLeft;
  Width := 5;
  Cursor := crHSplit;
  FMinSize := 30;
  FStatus := ssOut;
  ParentColor := true;
  ColorFocused := $0053D2FF;
  ColorBorder := $00555E66;
end;

procedure TDefineSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
end;

procedure TDefineSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit
  else
    P.Y := Top + FSplit;
  with P do
    PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TDefineSplitter.ReleaseLineDC;
begin
  ReleaseDC(Parent.Handle, FLineDC);
end;

procedure TDefineSplitter.Paint;
var
  memBitmap: TBitmap;
begin
  memBitmap := TBitmap.Create; // create memory-bitmap to draw flicker-free
  try
    memBitmap.Height := ClientRect.Bottom;
    memBitmap.Width := ClientRect.Right;

    if FStatus = ssIn then
    begin
      memBitmap.Canvas.Brush.Color := FFocusedColor;
      memBitmap.Canvas.FillRect(ClientRect);
      DrawButtonBorder(memBitmap.Canvas, ClientRect, FBorderColor, 1);
    end;
    if FStatus = ssOut then
    begin
      memBitmap.Canvas.Brush.Color := Color;
      memBitmap.Canvas.FillRect(ClientRect);
      DrawButtonBorder(memBitmap.Canvas, ClientRect, FBorderColor, 1);
    end;

    canvas.CopyRect(ClientRect, memBitmap.canvas, ClientRect); // Copy bitmap to screen
  finally
    memBitmap.free; // delete the bitmap
  end;
end;

procedure TDefineSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

function FindControl: TControl;
var
  P: TPoint;
  I: Integer;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if PtInRect(Result.BoundsRect, P) then
      Exit;
  end;
  Result := nil;
end;

var
  I: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then
              Dec(FMaxSize, Width);
            Inc(FMaxSize, FControl.Width);
        end
else
begin
FMaxSize := Parent.ClientHeight - FMinSize;
for I := 0 to Parent.ControlCount - 1 do
with Parent.Controls[I] do
if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
Inc(FMaxSize, FControl.Height);
end;
UpdateSize(X, Y);
AllocateLineDC;
with ValidParentForm(Self) do
if ActiveControl <> nil then
begin
FActiveControl := ActiveControl;
FOldKeyDown := TDefineHack(FActiveControl).OnKeyDown;
TDefineHack(FActiveControl).OnKeyDown := FocusKeyDown;
end;
DrawLine;
end;
end;
end;

procedure TDefineSplitter.UpdateSize(X, Y: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    FSplit := X - FDownPos.X
  else
    FSplit := Y - FDownPos.Y;
  S := 0;
  case Align of
      alLeft: S := FControl.Width + FSplit;
     alRight: S := FControl.Width - FSplit;
       alTop: S := FControl.Height + FSplit;
    alBottom: S := FControl.Height - FSplit;
  end;
  FNewSize := S;
  if S < FMinSize then
    FNewSize := FMinSize
  else
    if S > FMaxSize then
      FNewSize := FMaxSize;
  if S <> FNewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - FNewSize
    else
      S := FNewSize - S;
    Inc(FSplit, S);
  end;
end;

procedure TDefineSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    UpdateSize(X, Y);
    DrawLine;
  end;
end;

procedure TDefineSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    StopSizing;
  end;
end;

procedure TDefineSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
     StopSizing
  else
    if Assigned(FOldKeyDown) then
       FOldKeyDown(Sender, Key, Shift);
end;

procedure TDefineSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TDefineHack(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TDefineSplitter.CMEnter(var Message: TMessage);
begin
  if FStatus <> ssIn then
  begin
     FStatus := ssIn;
     Invalidate;
  end;
end;

procedure TDefineSplitter.CMExit(var Message: TMessage);
begin
  if FStatus <> ssOut then
  begin
    FStatus := ssOut;
    Invalidate;
  end;
end;

procedure TDefineSplitter.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TDefineSplitter.CMSysColorChange (var Message: TMessage);
begin
  inherited;
  if (ParentColor) and (Parent <> nil) then
     Color := TForm(Parent).Color;
  Invalidate;
end;

procedure TDefineSplitter.CMParentColorChanged (var Message: TWMNoParams);
begin
  inherited;
  if (ParentColor) and (Parent <> nil) then
     Color := TForm(Parent).Color;
  Invalidate;
end;

{ TDefineMask }

constructor TDefineMask.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaskState := [];
  FMaskBlank := DefaultBlank;
end;

procedure TDefineMask.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FSettingCursor then inherited KeyDown(Key, Shift);
  if IsMasked and (Key <> 0) and not (ssAlt in Shift) then
  begin
    if (Key = VK_LEFT) or(Key = VK_RIGHT) then
    begin
      ArrowKeys(Key, Shift);
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then
        Key := 0;
      Exit;
    end
    else if (Key = VK_UP) or(Key = VK_DOWN) then
    begin
      Key := 0;
      Exit;
    end
    else if (Key = VK_HOME) or(Key = VK_END) then
    begin
      HomeEndKeys(Key, Shift);
      Key := 0;
      Exit;
    end
    else if ((Key = VK_DELETE) and not (ssShift in Shift)) or
      (Key = VK_BACK) then
    begin
      if EditCanModify then
        DeleteKeys(Key);
      Key := 0;
      Exit;
    end;
    CheckCursor;
  end;
end;

procedure TDefineMask.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FSettingCursor then inherited KeyUp(Key, Shift);
  if IsMasked and (Key <> 0) then
  begin
    if ((Key = VK_LEFT) or(Key = VK_RIGHT)) and (ssCtrl in Shift) then
      CheckCursor;
  end;
end;

procedure TDefineMask.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if IsMasked and (Key <> #0) and not (Char(Key) in [^V, ^X, ^C]) then
  begin
    CharKeys(Key);
    Key := #0;
  end;
end;

procedure TDefineMask.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  FBtnDownX := Message.XPos;
end;

procedure TDefineMask.WMLButtonUp(var Message: TWMLButtonUp);
var
  SelStart, SelStop : Integer;
begin
  inherited;
  if (IsMasked) then
  begin
    GetSel(SelStart, SelStop);
    FCaretPos := SelStart;
    if (SelStart <> SelStop) and (Message.XPos > FBtnDownX) then
      FCaretPos := SelStop;
    CheckCursor;
  end;
end;

procedure TDefineMask.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (IsMasked) then
    CheckCursor;
end;

procedure TDefineMask.SetEditText(const Value: string);
begin
  if GetEditText <> Value then
  begin
    SetTextBuf(PChar(Value));
    CheckCursor;
  end;
end;

function TDefineMask.GetEditText: string;
begin
  Result := inherited Text;
end;

function TDefineMask.GetTextLen: Integer;
begin
  Result := Length(Text);
end;

function TDefineMask.GetText: TMaskedText;
begin
  if not IsMasked then
    Result := inherited Text
  else
  begin
    Result := RemoveEditFormat(EditText);
    if FMaskSave then
      Result := AddEditFormat(Result, False);
  end;
end;

procedure TDefineMask.SetText(const Value: TMaskedText);
var
  OldText: string;
  Pos: Integer;
begin
  if not IsMasked then
    inherited Text := Value
  else
  begin
    OldText := Value;
    if FMaskSave then
      OldText := PadInputLiterals(EditMask, OldText, FMaskBlank)
    else
      OldText := AddEditFormat(OldText, True);
    if not (msDBSetText in FMaskState) and
      (csDesigning in ComponentState) and
      not (csLoading in ComponentState) and
      not Validate(OldText, Pos) then
          raise TDefineError.Create(SMaskErr);
    EditText := OldText;
  end;
end;

procedure TDefineMask.WMCut(var Message: TMessage);
begin
  if not (IsMasked) then
    inherited
  else
  begin
    CopyToClipboard;
    DeleteKeys(VK_DELETE);
  end;
end;

procedure TDefineMask.WMPaste(var Message: TMessage);
var
  Value: string;
  Str: string;
  SelStart, SelStop : Integer;
begin
  if not (IsMasked) or ReadOnly then
    inherited
  else
  begin
    Clipboard.Open;
    Value := Clipboard.AsText;
    Clipboard.Close;

    GetSel(SelStart, SelStop);
    Str := EditText;
    DeleteSelection(Str, SelStart, SelStop - SelStart);
    EditText := Str;
    SelStart := InputString(Str, Value, SelStart);
    EditText := Str;
    SetCursor(SelStart);
  end;
end;

function TDefineMask.GetMasked: Boolean;
begin
  Result := EditMask <> '';
end;

function TDefineMask.GetMaxChars: Integer;
begin
  if IsMasked then
    Result := FMaxChars
  else
    Result := inherited GetTextLen;
end;

procedure TDefineMask.ReformatText(const NewMask: string);
var
  OldText: string;
begin
  OldText := RemoveEditFormat(EditText);
  FEditMask := NewMask;
  FMaxChars  := MaskOffsetToOffset(EditMask, Length(NewMask));
  FMaskSave  := MaskGetMaskSave(NewMask);
  FMaskBlank := MaskGetMaskBlank(NewMask);
  OldText := AddEditFormat(OldText, True);
  EditText := OldText;
end;

procedure TDefineMask.SetEditMask(const Value: TEditMask);
var
  SelStart, SelStop: Integer;
begin
  if Value <> EditMask then
  begin
    if (csDesigning in ComponentState) and (Value <> '') and
      not (csLoading in ComponentState) then
      EditText := '';
    if HandleAllocated then GetSel(SelStart, SelStop);
    ReformatText(Value);
    Exclude(FMaskState, msMasked);
    if EditMask <> '' then Include(FMaskState, msMasked);
    inherited MaxLength := 0;
    if IsMasked and (FMaxChars > 0) then
      inherited MaxLength := FMaxChars;
    if HandleAllocated and (GetFocus = Handle) and
       not (csDesigning in ComponentState) then
      SetCursor(SelStart);
  end;
end;

function TDefineMask.GetMaxLength: Integer;
begin
  Result := inherited MaxLength;
end;

procedure TDefineMask.SetMaxLength(Value: Integer);
begin
  if not IsMasked then
    inherited MaxLength := Value
  else
    inherited MaxLength := FMaxChars;
end;

procedure TDefineMask.GetSel(var SelStart: Integer; var SelStop: Integer);
begin
  SendMessage(Handle, EM_GETSEL, Integer(@SelStart), Integer(@SelStop));
end;

procedure TDefineMask.SetSel(SelStart: Integer; SelStop: Integer);
begin
  SendMessage(Handle, EM_SETSEL, SelStart, SelStop);
end;

procedure TDefineMask.SetCursor(Pos: Integer);
const
  ArrowKey: array[Boolean] of Word = (VK_LEFT, VK_RIGHT);
var
  SelStart, SelStop: Integer;
  KeyState: TKeyboardState;
  NewKeyState: TKeyboardState;
  I: Integer;
begin
  if (Pos >= 1) and (ByteType(EditText, Pos) = mbLeadByte) then Dec(Pos);
  SelStart := Pos;
  if (IsMasked) then
  begin
    if SelStart < 0 then
      SelStart := 0;
    SelStop  := SelStart + 1;
    if (Length(EditText) > SelStop) and (EditText[SelStop] in LeadBytes) then
      Inc(SelStop);
    if SelStart >= FMaxChars then
    begin
      SelStart := FMaxChars;
      SelStop  := SelStart;
    end;

    SetSel(SelStop, SelStop);
    
    if SelStart <> SelStop then
    begin
      GetKeyboardState(KeyState);
      for I := Low(NewKeyState) to High(NewKeyState) do
        NewKeyState[I] := 0;
      NewKeyState [VK_SHIFT] := $81;
      NewKeyState [ArrowKey[UseRightToLeftAlignment]] := $81;
      SetKeyboardState(NewKeyState);
      FSettingCursor := True;
      try
        SendMessage(Handle, WM_KEYDOWN, ArrowKey[UseRightToLeftAlignment], 1);
        SendMessage(Handle, WM_KEYUP, ArrowKey[UseRightToLeftAlignment], 1);
      finally
        FSettingCursor := False;
      end;
      SetKeyboardState(KeyState);
    end;
    FCaretPos := SelStart;
  end
  else
  begin
    if SelStart < 0 then
      SelStart := 0;
    if SelStart >= Length(EditText) then
      SelStart := Length(EditText);
    SetSel(SelStart, SelStart);
  end;
end;

procedure TDefineMask.CheckCursor;
var
  SelStart, SelStop: Integer;
begin
  if not HandleAllocated then  Exit;
  if (IsMasked) then
  begin
    GetSel(SelStart, SelStop);
    if SelStart = SelStop then
      SetCursor(SelStart);
  end;
end;

procedure TDefineMask.Clear;
begin
  Text := '';
end;

function TDefineMask.EditCanModify: Boolean;
begin
  Result := True;
end;

procedure TDefineMask.Reset;
begin
  if Modified then
  begin
    EditText := FOldValue;
    Modified := False;
  end;
end;

function TDefineMask.CharKeys(var CharCode: Char): Boolean;
var
  SelStart, SelStop : Integer;
  Txt: string;
  CharMsg: TMsg;
begin
  Result := False;
  if Word(CharCode) = VK_ESCAPE then
  begin
    Reset;
    Exit;
  end;
  if not EditCanModify or ReadOnly then Exit;
  if (Word(CharCode) = VK_BACK) then Exit;
  if (Word(CharCode) = VK_RETURN) then
  begin
    ValidateEdit;
    Exit;
  end;

  GetSel(SelStart, SelStop);
  if (SelStop - SelStart) > 1 then
  begin
    DeleteKeys(VK_DELETE);
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end;

  if (CharCode in LeadBytes) then
    if PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
      if CharMsg.Message = WM_Quit then
        PostQuitMessage(CharMsg.wparam);
  Result := InputChar(CharCode, SelStart);
  if Result then
  begin
    if (CharCode in LeadBytes) then
    begin
      Txt := CharCode + Char(CharMsg.wParam);
      SetSel(SelStart, SelStart + 2);
    end
    else
      Txt := CharCode;
    SendMessage(Handle, EM_REPLACESEL, 0, LongInt(PChar(Txt)));
    GetSel(SelStart, SelStop);
    CursorInc(SelStart, 0);
  end;
end;

procedure TDefineMask.ArrowKeys(CharCode: Word; Shift: TShiftState);
var
  SelStart, SelStop : Integer;
begin
  if (ssCtrl in Shift) then Exit;
  GetSel(SelStart, SelStop);
  if (ssShift in Shift) then
  begin
    if (CharCode = VK_RIGHT) then
    begin
      Inc(FCaretPos);
      if (SelStop = SelStart + 1) then
      begin
        SetSel(SelStart, SelStop);  {reset caret to end of string}
        Inc(FCaretPos);
      end;
      if FCaretPos > FMaxChars then FCaretPos := FMaxChars;
    end
    else  {if (CharCode = VK_LEFT) then}
    begin
      Dec(FCaretPos);
      if (SelStop = SelStart + 2) and
        (FCaretPos > SelStart) then
      begin
        SetSel(SelStart + 1, SelStart + 1);  {reset caret to show up at start}
        Dec(FCaretPos);
      end;
      if FCaretPos < 0 then FCaretPos := 0;
    end;
  end
  else
  begin
    if (SelStop - SelStart) > 1 then
    begin
      if ((SelStop - SelStart) = 2) and (EditText[SelStart+1] in LeadBytes) then
      begin
        if (CharCode = VK_LEFT) then
          CursorDec(SelStart)
        else
          CursorInc(SelStart, 2);
        Exit;
      end;
      if SelStop = FCaretPos then
        Dec(FCaretPos);
      SetCursor(FCaretPos);
    end
    else if (CharCode = VK_LEFT) then
      CursorDec(SelStart)
    else   { if (CharCode = VK_RIGHT) then  }
    begin
      if SelStop = SelStart then
        SetCursor(SelStart)
      else
        if EditText[SelStart+1] in LeadBytes then
          CursorInc(SelStart, 2)
        else
          CursorInc(SelStart, 1);
    end;
  end;
end;

procedure TDefineMask.CursorInc(CursorPos: Integer; Incr: Integer);
var
  NuPos: Integer;
begin
  NuPos := CursorPos + Incr;
  NuPos := GetNextEditChar(NuPos);
  if IsLiteralChar(EditMask, nuPos) then
    NuPos := CursorPos;
  SetCursor(NuPos);
end;


procedure TDefineMask.CursorDec(CursorPos: Integer);
var
  nuPos: Integer;
begin
  nuPos := CursorPos;
  Dec(nuPos);
  nuPos := GetPriorEditChar(nuPos);
  SetCursor(NuPos);
end;

function TDefineMask.GetFirstEditChar: Integer;
begin
  Result := 0;
  if IsMasked then
    Result := GetNextEditChar(0);
end;

function TDefineMask.GetLastEditChar: Integer;
begin
  Result := GetMaxChars;
  if IsMasked then
    Result := GetPriorEditChar(Result - 1);
end;

function TDefineMask.GetNextEditChar(Offset: Integer): Integer;
begin
  Result := Offset;
  while(Result < FMaxChars) and (IsLiteralChar(EditMask, Result)) do
    Inc(Result);
end;

function TDefineMask.GetPriorEditChar(Offset: Integer): Integer;
begin
  Result := Offset;
  while(Result >= 0) and (IsLiteralChar(EditMask, Result)) do
    Dec(Result);
  if Result < 0 then
    Result := GetNextEditChar(Result);
end;

procedure TDefineMask.HomeEndKeys(CharCode: Word; Shift: TShiftState);
var
  SelStart, SelStop : Integer;
begin
  GetSel(SelStart, SelStop);
  if (CharCode = VK_HOME) then
  begin
    if (ssShift in Shift) then
    begin
      if (SelStart <> FCaretPos) and (SelStop <> (SelStart + 1)) then
        SelStop := SelStart + 1;
      SetSel(0, SelStop);
      CheckCursor;
    end
    else
      SetCursor(0);
    FCaretPos := 0;
  end
  else
  begin
    if (ssShift in Shift) then
    begin
      if (SelStop <> FCaretPos) and (SelStop <> (SelStart + 1)) then
        SelStart := SelStop - 1;
      SetSel(SelStart, FMaxChars);
      CheckCursor;
    end
    else
      SetCursor(FMaxChars);
    FCaretPos := FMaxChars;
  end;
end;

procedure TDefineMask.DeleteKeys(CharCode: Word);
var
  SelStart, SelStop : Integer;
  NuSelStart: Integer;
  Str: string;
begin
  if ReadOnly then Exit;
  GetSel(SelStart, SelStop);
  if ((SelStop - SelStart) <= 1) and (CharCode = VK_BACK) then
  begin
    NuSelStart := SelStart;
    CursorDec(SelStart);
    GetSel(SelStart, SelStop);
    if SelStart = NuSelStart then Exit;
  end;

  if (SelStop - SelStart) < 1 then Exit;

  Str := EditText;
  DeleteSelection(Str, SelStart, SelStop - SelStart);
  Str := Copy(Str, SelStart+1, SelStop - SelStart);
  SendMessage(Handle, EM_REPLACESEL, 0, LongInt(PChar(Str)));
  if (SelStop - SelStart) <> 1 then
  begin
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end
  else begin
    GetSel(SelStart, SelStop);
    SetCursor(SelStart - 1);
  end;
end;

procedure TDefineMask.CMEnter(var Message: TCMEnter);
begin
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    if not (msReEnter in FMaskState) then
    begin
      FOldValue := EditText;
      inherited;
    end;
    Exclude(FMaskState, msReEnter);
    CheckCursor;
  end
  else
    inherited;
end;

procedure TDefineMask.CMTextChanged(var Message: TMessage);
var
  SelStart, SelStop : Integer;
  Temp: Integer;
begin
  inherited;
  FOldValue := EditText;
  if HandleAllocated then
  begin
    GetSel(SelStart, SelStop);
    Temp := GetNextEditChar(SelStart);
    if Temp <> SelStart then
      SetCursor(Temp);
  end;
end;

procedure TDefineMask.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if (Message.CharCode = VK_ESCAPE) and IsMasked and Modified then
    Message.Result := 1;
end;

procedure TDefineMask.CMExit(var Message: TCMExit);
begin
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    ValidateEdit;
    CheckCursor;
  end;
  inherited;
end;

procedure TDefineMask.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified then
  begin
    if not Validate(Str, Pos) then
    begin
      if not (csDesigning in ComponentState) then
      begin
        Include(FMaskState, msReEnter);
        SetFocus;
      end;
      SetCursor(Pos);
      ValidateError;
    end;
  end;
end;

procedure TDefineMask.ValidateError;
begin
  MessageBeep(0);
  raise TDefineError.Create(SMaskEditErr);
end;

function TDefineMask.AddEditFormat(const Value: string; Active: Boolean): string;
begin
  if not Active then
    Result := MaskDoFormatText(EditMask, Value, ' ')
  else
    Result := MaskDoFormatText(EditMask, Value, FMaskBlank);
end;

function TDefineMask.RemoveEditFormat(const Value: string): string;
var
  I: Integer;
  OldLen: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  Dir: TMaskDirectives;
begin
  Offset := 1;
  Result := Value;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral] then
      Result := Copy(Result, 1, Offset - 1) +
        Copy(Result, Offset + 1, Length(Result) - Offset);
    if CType in [mcMask, mcMaskOpt] then Inc(Offset);
  end;

  Dir := MaskGetCurrentDirectives(EditMask, 1);
  if mdReverseDir in Dir then
  begin
    Offset := 1;
    for I := 1 to Length(Result) do
    begin
      if Result[I] = FMaskBlank then
        Inc(Offset)
      else
        break;
    end;
    if Offset <> 1 then
      Result := Copy(Result, Offset, Length(Result) - Offset + 1);
  end
  else begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[OldLen - I + 1] = FMaskBlank then
        SetLength(Result, Length(Result) - 1)
      else Break;
    end;
  end;
  if FMaskBlank <> ' ' then
  begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[I] = FMaskBlank then
        Result[I] := ' ';
      if I > OldLen then Break;
    end;
  end;
end;

function TDefineMask.InputChar(var NewChar: Char; Offset: Integer): Boolean;
var
  MaskOffset: Integer;
  CType: TMaskCharType;
  InChar: Char;
begin
  Result := True;
  if EditMask <> '' then
  begin
    Result := False;
    MaskOffset := OffsetToMaskOffset(EditMask, Offset);
    if MaskOffset >= 0 then
    begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      InChar := NewChar;
      Result := DoInputChar(NewChar, MaskOffset);
      if not Result and (CType in [mcMask, mcMaskOpt]) then
      begin
        MaskOffset := FindLiteralChar (MaskOffset, InChar);
        if MaskOffset > 0 then
        begin
          MaskOffset := MaskOffsetToOffset(EditMask, MaskOffset);
          SetCursor (MaskOffset);
          Exit;
        end;
      end;
    end;
  end;
  if not Result then
    MessageBeep(0)
end;

function TDefineMask.DoInputChar(var NewChar: Char; MaskOffset: Integer): Boolean;
var
  Dir: TMaskDirectives;
  Str: string;
  CType: TMaskCharType;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

  function TestChar(NewChar: Char): Boolean;
  var
    Offset: Integer;
  begin
    Offset := MaskOffsetToOffset(EditMask, MaskOffset);
    Result := not ((MaskOffset < Length(EditMask)) and
               (UpCase(EditMask[MaskOffset]) = UpCase(EditMask[MaskOffset+1]))) or
               (ByteType(EditText, Offset) = mbTrailByte) or
               (ByteType(EditText, Offset+1) = mbLeadByte);
  end;

begin
  Result := True;
  CType := MaskGetCharType(EditMask, MaskOffset);
  if CType in [mcLiteral, mcIntlLiteral] then
    NewChar := MaskIntlLiteralToChar(EditMask[MaskOffset])
  else
  begin
    Dir := MaskGetCurrentDirectives(EditMask, MaskOffset);
    case EditMask[MaskOffset] of
      mMskNumeric, mMskNumericOpt:
        begin
          if not ((NewChar >= '0') and (NewChar <= '9')) then
            Result := False;
        end;
      mMskNumSymOpt:
        begin
          if not (((NewChar >= '0') and (NewChar <= '9')) or
                 (NewChar = ' ') or(NewChar = '+') or(NewChar = '-')) then
            Result := False;
        end;
      mMskAscii, mMskAsciiOpt:
        begin
          if (NewChar in LeadBytes) and TestChar(NewChar) then
          begin
            Result := False;
            Exit;
          end;
          if IsCharAlpha(NewChar) then
          begin
            Str := ' ';
            Str[1] := NewChar;
            if (mdUpperCase in Dir)  then
              Str := AnsiUpperCase(Str)
            else if mdLowerCase in Dir then
              Str := AnsiLowerCase(Str);
            NewChar := Str[1];
          end;
        end;
      mMskAlpha, mMskAlphaOpt, mMskAlphaNum, mMskAlphaNumOpt:
        begin
          if (NewChar in LeadBytes) then
          begin
            if TestChar(NewChar) then
              Result := False;
            Exit;
          end;
          Str := ' ';
          Str[1] := NewChar;
          if IsKatakana(Byte(NewChar)) then
          begin
              NewChar := Str[1];
              Exit;
          end;
          if not IsCharAlpha(NewChar) then
          begin
            Result := False;
            if ((EditMask[MaskOffset] = mMskAlphaNum) or
                (EditMask[MaskOffset] = mMskAlphaNumOpt)) and
                (IsCharAlphaNumeric(NewChar)) then
              Result := True;
          end
          else if mdUpperCase in Dir then
            Str := AnsiUpperCase(Str)
          else if mdLowerCase in Dir then
            Str := AnsiLowerCase(Str);
          NewChar := Str[1];
        end;
    end;
  end;
end;

function TDefineMask.Validate(const Value: string; var Pos: Integer): Boolean;
var
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  Offset := 1;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral, mcMaskOpt] then
      Inc(Offset)
    else if (CType = mcMask) and (Value <> '') then
    begin
      if (Value [Offset] = FMaskBlank) or
        ((Value [Offset] = ' ') and (EditMask[MaskOffset] <> mMskAscii)) then
      begin
        Result := False;
        Pos := Offset - 1;
        Exit;
      end;
      Inc(Offset);
    end;
  end;
end;

function TDefineMask.DeleteSelection(var Value: string; Offset: Integer;
  Len: Integer): Boolean;
var
  EndDel: Integer;
  StrOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  if Len = 0 then Exit;

  StrOffset := Offset + 1;
  EndDel := StrOffset + Len;
  Temp := OffsetToMaskOffset(EditMask, Offset);
  if Temp < 0 then  Exit;
  for MaskOffset := Temp to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
      Inc(StrOffset)
    else if CType in [mcMask, mcMaskOpt] then
    begin
      Value[StrOffset] := FMaskBlank;
      Inc(StrOffset);
    end;
    if StrOffset >= EndDel then Break;
  end;
end;

function TDefineMask.InputString(var Value: string; const NewValue: string;
  Offset: Integer): Integer;
var
  NewOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
  NewVal: string;
  NewChar: Char;
begin
  Result := Offset;
  if NewValue = '' then Exit;
  { replace chars with new chars, except literals }
  NewOffset := 1;
  NewVal := NewValue;
  Temp := OffsetToMaskOffset(EditMask, Offset);
  if Temp < 0 then  Exit;
  MaskOffset := Temp;
  While MaskOffset <= Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral, mcMask, mcMaskOpt] then
    begin
      NewChar := NewVal[NewOffset];
      if not (DoInputChar(NewChar, MaskOffset)) then
      begin
        if (NewChar in LeadBytes) then
          NewVal[NewOffset + 1] := FMaskBlank;
        NewChar := FMaskBlank;
      end;
        { if pasted text does not contain a literal in the right place,
          insert one }
      if not ((CType in [mcLiteral, mcIntlLiteral]) and
        (NewChar <> NewVal[NewOffset])) then
      begin
        NewVal[NewOffset] := NewChar;
        if (NewChar in LeadBytes) then
        begin
          Inc(NewOffset);
          Inc(MaskOffset);
        end;
      end
      else
        NewVal := Copy(NewVal, 1, NewOffset-1) + NewChar +
          Copy(NewVal, NewOffset, Length (NewVal));
      Inc(NewOffset);
    end;
    if (NewOffset + Offset) > FMaxChars then Break;
    if (NewOffset) > Length(NewVal) then Break;
    Inc(MaskOffset);
  end;

  if (Offset + Length(NewVal)) < FMaxChars then
  begin
    if ByteType(Value, OffSet + Length(NewVal) + 1) = mbTrailByte then
    begin
      NewVal := NewVal + FMaskBlank;
      Inc(NewOffset);
    end;
    Value := Copy(Value, 1, Offset) + NewVal +
      Copy(Value, OffSet + Length(NewVal) + 1,
        FMaxChars -(Offset + Length(NewVal)));
  end
  else
  begin
    Temp := Offset;
    if (ByteType(NewVal, FMaxChars - Offset) = mbLeadByte) then
      Inc(Temp);
    Value := Copy(Value, 1, Offset) +
             Copy(NewVal, 1, FMaxChars - Temp);
  end;
  Result := NewOffset + Offset - 1;
end;

function TDefineMask.FindLiteralChar(MaskOffset: Integer; InChar: Char): Integer;
var
  CType: TMaskCharType;
  LitChar: Char;
begin
  Result := -1;
  while MaskOffset < Length(EditMask) do
  begin
    Inc(MaskOffset);
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
    begin
      LitChar := EditMask[MaskOffset];
      if CType = mcIntlLiteral then
        LitChar := MaskIntlLiteralToChar(LitChar);
      if LitChar = InChar then
        Result := MaskOffset;
      Exit;
    end;
  end;
end;   

{ TDefinePucker}
constructor TDefinePucker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  { When themes are on in an application default to making
    TDefinePanel's paint with their ParentBackground }
  if ThemeServices.ThemesEnabled then
     ControlStyle  := ControlStyle + [csParentBackground] - [csOpaque];
  FGradientFill    := true;
  FFullRepaint     := True;
  FStartColor      := DefaultColorStart;
  FEndColor        := DefaultColorStop;
  FFillDirection   := fdLeftToRight;
  FShadow          := true;
  FShadowDist      := 5;
 // Width          := 180;
//  Height         := 100;
  FShowHeader      := True;
  FDefaultHeight   := 100;
  FTitleHeight     := 30;
  FTitleAlignment  := taCenter;
  FTitleShadowOnMouseEnter := true;
  FTitleGradient   := true;
  FTitleStartColor := DefaultTitleColorStart;
  FTitleEndColor   := DefaultTitleColorEnd;
  FTitleColor      := clWhite;
  FTitleFillDirect := fdLeftToRight;

  FTitleImage         := TBitmap.Create;
  FTitleCursor        := crSystemHand;
  FTitleImageTransparent := true;
  FTitleImageAlign    := tiaLeft;
  FTitleFont          := TFont.Create;
  FTitleFont.Style    := [fsBold];
  FTitleFont.Color    := clNavy;
  FTitleFont.OnChange := OnTitleFontChange;
  FTitleButtons       := [tbMinimize];
  FTitleButtonsStyle  := tbsRectangle;
  FTitleBtnBorderColor:= DefaultBorderColor;
  FTitleBtnBGColor    := DefaultBackdropColor;
  FTitleBtnBorderSize := 1;
  FMouseOnHeader      := False;
  FBorderSize         := 1;
  FShowBorder         := True;
  FBorderColor        := DefaultBorderColor;
  FPanelCorner        := [];

  FBGImage            := TBitmap.Create;
  FBGImageAlign       := iaStretch;
  FBGImageTransparent := true;

  FOnTitleClick       := nil;
  FOnTitleDblClick    := nil;
  FOnTitleMouseDown   := nil;
  FOnTitleMouseUp     := nil;
  FOnTitleMouseEnter  := nil;
  FOnTitleMouseExit   := nil;
  FOnMouseEnter       := nil;
  FOnMouseExit        := nil;

  FAfterMinimized     := nil;
  FAfterMaximized     := nil;
  FBeforeMoving       := nil;
  FAfterMoving        := nil;
  FAfterClose         := nil;
  FMovable            := False;
  FSizable            := False;
  FMinimized          := False;
  FAnimation          := True;
  FMinimizing         := False;
  SetBounds(0,0,180,100);
end;

destructor  TDefinePucker.Destroy;
begin
  try FTitleFont.Free; except end;
  try FBGImage.Free; except end;
  try FTitleImage.Free; except end;
  inherited;
end;

procedure TDefinePucker.DrawTitle(ACanvas : TCanvas; ATitleRect : TRect);
var
  X, Y : Integer;
  AGrayImage : TBitmap;
  ATextFormat : Integer;
  ATextRect : TRect;
  ABtnOffset : Integer;
begin
  if FTitleGradient then
    GradientFillRect(ACanvas, ATitleRect, FTitleStartColor, FTitleEndColor, FTitleFillDirect, 50)
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := FTitleColor;
    ACanvas.FillRect(ATitleRect);
  end;

  ATextRect := ATitleRect;

  InflateRect(ATextRect, -2, -2);

  ABtnOffset := ATextRect.Right;

  if tbMinimize in FTitleButtons then ABtnOffset := FMinBtnRect.Left - 4 else
    if tbMaximize in FTitleButtons then ABtnOffset := FMaxBtnRect.Left - 4 else
      if tbClose in FTitleButtons then ABtnOffset := FCloseBtnRect.Left - 4;

  if not FTitleImage.Empty then
  begin
    FTitleImage.TransparentMode := tmAuto;
    FTitleImage.Transparent := False;

    if(FTitleImageAlign in [tiaLeft, tiaRight, tiaCenter]) then
    begin
      case FTitleImageAlign of
        tiaLeft:
        begin
          X := 2;
          Y :=(ATitleRect.Bottom + ATitleRect.Top - FTitleImage.Height) div 2;
          ATextRect.Left := ATextRect.Left + FTitleImage.Width + 8;
        end;
        tiaRight:
        begin
          X := ABtnOffset - FTitleImage.Width;
          Y :=(ATitleRect.Bottom + ATitleRect.Top - FTitleImage.Height) div 2;
          ABtnOffset := X - 4;
        end;
      else
       // tiaCenter:
        begin
          X :=(ATitleRect.Right + ATitleRect.Left - FTitleImage.Width) div 2;
          Y :=(ATitleRect.Bottom + ATitleRect.Top - FTitleImage.Height) div 2;
        end;
      end;
      //Image Shadow
      if FMouseOnHeader then
      begin
        AGrayImage := TBitmap.Create;
        try
          CopyBitmap(FTitleImage, AGrayImage);
          AGrayImage.TransparentMode := tmAuto;
          AGrayImage.Transparent := true;
          ConvertBitmapToGrayscale(AGrayImage);
          if FTitleImageTransparent then
            DrawBitmapTransparent(ACanvas, X, Y, AGrayImage, AGrayImage.Canvas.Pixels [0,0])
          else
            ACanvas.Draw(X, Y, AGrayImage);
        finally
          AGrayImage.Free;
        end;
      end;
      //Image
      if FTitleImageTransparent then
        DrawBitmapTransparent(ACanvas, X - Integer(FMouseOnHeader), Y - Integer(FMouseOnHeader),
          FTitleImage, FTitleImage.Canvas.Pixels [0,0])
      else
        ACanvas.Draw(X - Integer(FMouseOnHeader),  Y - Integer(FMouseOnHeader), FTitleImage);
    end
    else
    begin
      FTitleImage.TransparentMode := tmAuto;
      FTitleImage.Transparent := FTitleImageTransparent;
      case FTitleImageAlign of
        tiaStretch:
          ACanvas.StretchDraw(ATitleRect, FTitleImage);
        tiaTile:
          TileImage(ACanvas, ATitleRect, FTitleImage);
      end;
    end;

  end;

  if FCaption <> '' then
  begin
    ATextRect.Right := ABtnOffset;

    ATextFormat := DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
    ACanvas.Font.Assign(FTitleFont);
    case FTitleAlignment of
      taLeftJustify: ATextFormat := ATextFormat or DT_LEFT;
      taRightJustify: ATextFormat := ATextFormat or DT_RIGHT;
      taCenter: ATextFormat := ATextFormat or DT_CENTER;
    end;
    ACanvas.Brush.Style := bsClear;

    //Shadow
    ACanvas.Font.Color := clLtGray;
    DrawText(ACanvas.Handle, PChar(FCaption), Length(FCaption), ATextRect, ATextFormat);

    //Text
    ACanvas.Font.Assign(FTitleFont);
    OffsetRect(ATextRect, -1, -1);
    if FMouseOnHeader then OffsetRect(ATextRect, -1, -1);
    DrawText(ACanvas.Handle, PChar(FCaption), Length(FCaption), ATextRect, ATextFormat);
  end;
end;


procedure TDefinePucker.DrawAllTitleButtons(ACanvas : TCanvas; ATitleRect : TRect);
const
  XOffset : Integer = 22;
var
  AButtonRect : TRect;
begin
  if FTitleButtons = [] then Exit;

  AButtonRect.Left   := ATitleRect.Right - cTitleButtonSize - 2 + XOffset;
  AButtonRect.Right  := ATitleRect.Right - 2 + XOffset;
  AButtonRect.Top    :=(ATitleRect.Bottom + ATitleRect.Top) div 2 -(cTitleButtonSize div 2)+1;
  AButtonRect.Bottom :=(ATitleRect.Bottom + ATitleRect.Top) div 2 +(cTitleButtonSize div 2);

  if tbClose in FTitleButtons then
  begin
    AButtonRect.Left  := AButtonRect.Left - XOffset;
    AButtonRect.Right := AButtonRect.Right- XOffset;
    FCloseBtnRect     := AButtonRect;
    DrawTitleButton(ACanvas, AButtonRect, tbClose);
  end;

  if tbMaximize in FTitleButtons then
  begin
    AButtonRect.Left  := AButtonRect.Left - XOffset;
    AButtonRect.Right := AButtonRect.Right- XOffset;
    FMaxBtnRect       := AButtonRect;
    DrawTitleButton(ACanvas, AButtonRect, tbMaximize);
  end;

  if tbMinimize in FTitleButtons then
  begin
    AButtonRect.Left  := AButtonRect.Left - XOffset;
    AButtonRect.Right := AButtonRect.Right- XOffset;
    FMinBtnRect       := AButtonRect;
    DrawTitleButton(ACanvas, AButtonRect, tbMinimize);
  end;
end;

procedure TDefinePucker.DrawTitleButton(ACanvas : TCanvas; AButtonRect : TRect; ABtnType : TTitleButton);
var
  XCenter, YCenter, Radius : Integer;
  procedure DrawStyle(Canvas:TCanvas;Rect:TRect;Style:TTitleButtonsStyle);
  begin
   case Style of
      tbsEllipse   : Canvas.Ellipse(Rect);
      tbsRectangle : Canvas.Rectangle(Rect);
   end;
  end;
begin
  ACanvas.Pen.Color   := MakeDarkColor(FTitleBtnBorderColor, 30);
  ACanvas.Pen.Width   := FTitleBtnBorderSize;
  ACanvas.Brush.Color := MakeDarkColor(FTitleBtnBGColor, 30);
  DrawStyle(ACanvas,AButtonRect,FTitleButtonsStyle);

  XCenter :=(AButtonRect.Right + AButtonRect.Left) div 2;
  YCenter :=(AButtonRect.Bottom + AButtonRect.Top) div 2;

  if XCenter < YCenter then
    Radius :=(XCenter - AButtonRect.Left)-4
  else
    Radius :=(YCenter - AButtonRect.Top)-4;

  ACanvas.Pen.Width := 2;
  if FMouseOnHeader and FShowHeader then
    ACanvas.Pen.Color := $FF5C33
  else
    ACanvas.Pen.Color := $A53C00;

  case ABtnType of
    tbClose:
      begin
          ACanvas.Polyline([Point(XCenter - Radius + 2, YCenter - Radius + 2),
                       Point(XCenter + Radius - 2, YCenter + Radius - 2)    ]);

          ACanvas.Polyline([Point(XCenter + Radius - 2, YCenter - Radius + 2),
                       Point(XCenter - Radius + 2, YCenter + Radius - 2)    ]);
      end;
    tbMaximize:
      begin
        ACanvas.Pen.Width := 1;
        if FMaximized then
        begin
          ACanvas.Rectangle(XCenter - Radius + 1, YCenter - Radius + 1,
                             XCenter + Radius-1, YCenter + Radius-2);
          ACanvas.Rectangle(XCenter - Radius + 3, YCenter - Radius + 3,
                             XCenter + Radius+1, YCenter + Radius);
        end
        else
        begin
          ACanvas.Rectangle(XCenter - Radius + 1, YCenter - Radius + 1,
                             XCenter + Radius, YCenter + Radius);
          ACanvas.Rectangle(XCenter - Radius + 1, YCenter - Radius + 2,
                             XCenter + Radius, YCenter + Radius);
        end;
      end;
    tbMinimize:
      begin
        if FMinimized then
        begin
          //Drawing down arrows
          ACanvas.Polyline([Point(XCenter - Radius + 2, YCenter - Radius + 1),
                       Point(XCenter, YCenter-1),
                       Point(XCenter + Radius - 2, YCenter - Radius + 1)    ]);

          ACanvas.Polyline([Point(XCenter - Radius + 2, YCenter+1),
                       Point(XCenter, YCenter + Radius - 1),
                       Point(XCenter + Radius - 2, YCenter+1)    ]);
        end
        else
        begin
          //Drawing up arrows
          ACanvas.Polyline([Point(XCenter - Radius + 2, YCenter - 1),
                       Point(XCenter, YCenter - Radius + 1),
                       Point(XCenter + Radius - 2, YCenter - 1)    ]);

          ACanvas.Polyline([Point(XCenter - Radius + 2, YCenter + Radius - 1),
                       Point(XCenter, YCenter+1),
                       Point(XCenter + Radius - 2, YCenter + Radius - 1)    ]);
        end;
    end;     
  end;
end;

procedure TDefinePucker.DrawBorder(ACanvas : TCanvas; ARect : TRect; AClient : Boolean);
var
  APanelCorner : TPanelCorners;
begin
  ACanvas.Brush.Style := BSCLEAR;
  ACanvas.Pen.Color   := FBorderColor;
  ACanvas.Pen.Width   := FBorderSize;

  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  if FPanelCorner = [] then Exit;

  APanelCorner := FPanelCorner;

  if AClient then
    APanelCorner := APanelCorner - [rcTopLeft, rcTopRight];

  if(rcTopLeft in APanelCorner) and(rcTopRight in APanelCorner) and
     (rcBottomLeft in APanelCorner) and(rcBottomRight in APanelCorner) then
  begin
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
    APanelCorner := [];
  end
  else
  if(rcTopLeft in APanelCorner) and(rcTopRight in APanelCorner) then
  begin
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom + DefaultCornerRadius*2, DefaultCornerRadius, DefaultCornerRadius);
    APanelCorner := APanelCorner - [rcTopLeft, rcTopRight];
  end
  else
  if(rcBottomLeft in APanelCorner) and(rcBottomRight in APanelCorner) then
  begin
    ACanvas.RoundRect(ARect.Left, ARect.Top - DefaultCornerRadius*2, ARect.Right, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
    APanelCorner := APanelCorner - [rcBottomLeft, rcBottomRight];
  end
  else
  if(rcTopLeft in APanelCorner) and(rcBottomLeft in APanelCorner) then
  begin
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right + DefaultCornerRadius*2, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
    APanelCorner := APanelCorner - [rcTopLeft, rcBottomLeft];
  end
  else
  if(rcTopRight in APanelCorner) and(rcBottomRight in APanelCorner) then
  begin
    ACanvas.RoundRect(ARect.Left - DefaultCornerRadius*2, ARect.Top, ARect.Right, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
    APanelCorner := APanelCorner - [rcTopRight, rcBottomRight];
  end;

  if APanelCorner = [] then Exit;

  if(rcTopLeft in APanelCorner) then
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right + DefaultCornerRadius*2, ARect.Bottom + DefaultCornerRadius*2, DefaultCornerRadius, DefaultCornerRadius);
  if(rcTopRight in APanelCorner) then
    ACanvas.RoundRect(ARect.Left - DefaultCornerRadius*2, ARect.Top, ARect.Right, ARect.Bottom + DefaultCornerRadius*2, DefaultCornerRadius, DefaultCornerRadius);
  if(rcBottomLeft in APanelCorner) then
    ACanvas.RoundRect(ARect.Left, ARect.Top - DefaultCornerRadius*2, ARect.Right + DefaultCornerRadius*2, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
  if(rcBottomRight in APanelCorner) then
    ACanvas.RoundRect(ARect.Left - DefaultCornerRadius*2, ARect.Top - DefaultCornerRadius*2, ARect.Right, ARect.Bottom, DefaultCornerRadius, DefaultCornerRadius);
end;

procedure TDefinePucker.DrawBGImage(ACanvas : TCanvas);
begin
  FBGImage.TransparentMode := tmAuto;
  FBGImage.Transparent := FBGImageTransparent;
  case FBGImageAlign of
    iaStretch:
      begin
        ACanvas.StretchDraw(ClientRect, FBGImage);
      end;
    iaCenter:
      begin
        ACanvas.Draw(
         (ClientWidth - FBGImage.Width) div 2,
         (ClientHeight - FBGImage.Height) div 2,
          FBGImage);
      end;
    iaTile:
      begin
        TileImage(ACanvas, ClientRect, FBGImage);
      end;
  end;
end;
//Draw client area

procedure TDefinePucker.Paint;
var
  TempCanvas : TBitmap;
begin
  TempCanvas := TBitmap.Create;
  try
    TempCanvas.Width  := ClientWidth;
    TempCanvas.Height := ClientHeight;
    if FGradientFill then begin
       GradientFillRect(TempCanvas.Canvas, ClientRect, FStartColor, FEndColor, FFillDirection, 60);
    end else Begin
      TempCanvas.Canvas.Brush.Style := bsSolid;
      TempCanvas.Canvas.Brush.Color := Color;
      TempCanvas.Canvas.FillRect(ClientRect);
    end;

    if not FBGImage.Empty then DrawBGImage(TempCanvas.Canvas);

    BitBlt(Canvas.Handle, 0, 0, TempCanvas.Width, TempCanvas.Height,TempCanvas.Canvas.Handle, 0, 0, SRCCOPY);

    if FShowBorder then begin
       SendMessage(Handle, WM_NCPAINT, wmNCPaintOnlyBorder, 0);
      //SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;

  finally
    TempCanvas.Free;
  end;
end;

//Calculate nonclient area
procedure TDefinePucker.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  if FShowBorder then
  begin
    InflateRect(Message.CalcSize_Params^.rgrc[0], -FBorderSize, -FBorderSize);
    if FShowHeader then
      Inc(Message.CalcSize_Params^.rgrc[0].Top, FTitleHeight);
  end
  else
  begin
    if FShowHeader then
      Inc(Message.CalcSize_Params^.rgrc[0].Top, FTitleHeight+1);
  end;

  inherited;
end;

procedure TDefinePucker.WMNCACTIVATE(var Message : TWMNCActivate);
begin
  inherited;
end;

procedure TDefinePucker.NCHitTest(var Message : TWMNCHitTest);
var
  WinRect : TRect;
  ClientPoint : TPoint;
  PanelPoint : TPoint;
  ABottom : Integer;
  ATitleHeight : Integer;
  ABorderSize : Integer;
begin
  inherited;
  Message.Result := HTCLIENT;

  GetWindowRect(Handle, WinRect);
  ABottom := WinRect.Bottom;

  if FShowHeader then ATitleHeight := FTitleHeight else ATitleHeight := 0;

  if FShowBorder then
  begin
    ABorderSize := FBorderSize;
    if ABorderSize < 5 then ABorderSize := 5;
  end
  else
    ABorderSize := 0;


  WinRect.Bottom := WinRect.Top + ATitleHeight;

  ClientPoint := Point(Message.XPos, Message.YPos);

  PanelPoint := ScreenToClient(ClientPoint);

  if PtInRect(WinRect, Point(Message.XPos, Message.YPos)) then
    Message.Result := HTOBJECT;

  if FTitleShadowOnMouseEnter then
  begin
    if(not FMouseOnHeader) and((PtInRect(WinRect, Point(Message.XPos, Message.YPos)))) then
    begin
      FMouseOnHeader := true;
      SendMessage(Handle, WM_NCPAINT, 0, 0);

      if Assigned(FOnTitleMouseEnter) then FOnTitleMouseEnter(self);
    end
    else
    if(not((PtInRect(WinRect, Point(Message.XPos, Message.YPos))))) and(FMouseOnHeader) then
    begin
      FMouseOnHeader := False;
      SendMessage(Handle, WM_NCPAINT, 0, 0);
      if Assigned(FOnTitleMouseExit) then FOnTitleMouseExit(self);
    end;
  end;

  Inc(PanelPoint.y, FTitleHeight);

  if tbClose in FTitleButtons then
  begin
    if PtInRect(FCloseBtnRect, PanelPoint) then
      Message.Result := HTCLOSE;
  end;

  if tbMaximize in FTitleButtons then
  begin
    if PtInRect(FMaxBtnRect, PanelPoint) then
      Message.Result := HTMAXBUTTON;
  end;

  if tbMinimize in FTitleButtons then
  begin
    if PtInRect(FMinBtnRect, PanelPoint) then
      Message.Result := HTMINBUTTON;
  end;

  if(csDesigning in ComponentState) then Exit;

  WinRect.Bottom := ABottom;
  if FSizable and not FMinimized and not Maximized then
  begin
    if PtInRect(Rect(WinRect.Left, WinRect.Top, WinRect.Left + ABorderSize+5, WinRect.Top + ABorderSize + 5), ClientPoint) then
      Message.Result := HTTOPLEFT
    else
    //Check mouse on TopRight border
    if PtInRect(Rect(WinRect.Right - 5, WinRect.Top, WinRect.Right+1, WinRect.Top + 5), ClientPoint) then
      Message.Result := HTTOPRIGHT
    //Check mouse on BottomLeft border
    else
    if PtInRect(Rect(WinRect.Left, WinRect.Bottom - ABorderSize-5, WinRect.Left+5, WinRect.Bottom), ClientPoint) then
      Message.Result := HTBOTTOMLEFT
    //Check mouse on BottomRight border
    else
    if PtInRect(Rect(WinRect.Right-5, WinRect.Bottom - ABorderSize-5, WinRect.Right, WinRect.Bottom), ClientPoint) then
      Message.Result := HTBOTTOMRIGHT
    else
    //Check mouse on Left border
    if PtInRect(Rect(WinRect.Left, WinRect.Top + 5, WinRect.Left + ABorderSize, WinRect.Right - ABorderSize), ClientPoint) then
      Message.Result := HTLEFT
    else
    //Check mouse on Right border
    if PtInRect(Rect(WinRect.Right - ABorderSize, WinRect.Top + 5, WinRect.Right+1, WinRect.Bottom - 5), ClientPoint) then
      Message.Result := HTRIGHT
    else
    //Check mouse on Top border
    if PtInRect(Rect(WinRect.Left+5, WinRect.Top, WinRect.Right-5, WinRect.Top + ABorderSize), ClientPoint) then
      Message.Result := HTTOP
    //Check mouse on Bottom border
    else
    if PtInRect(Rect(WinRect.Left+5, WinRect.Bottom - ABorderSize, WinRect.Right-5, WinRect.Bottom), ClientPoint) then
      Message.Result := HTBOTTOM;
  end;


  if FMovable and PtInRect(WinRect, ClientPoint) and
     not(Message.Result in [HTCLOSE, HTMINBUTTON, HTMAXBUTTON]) then
  begin
    WinRect.Bottom := WinRect.Top + ATitleHeight;
    InflateRect(WinRect, -ABorderSize, -ABorderSize);
    if PtInRect(WinRect, ClientPoint) then Message.Result := HTCAPTION;
  end;

end;


//Draw nonclient area
procedure TDefinePucker.WMNCPaint(var Message : TWMNCPaint);
var
  UpdateRect : TRect;
  HeaderRect : TRect;
  DC : hDC;
  NCCanvas : TCanvas;
  TempCanvas : TBitmap;
begin
  DC := GetWindowDC(Handle);
  NCCanvas := TCanvas.Create;
  try
    NCCanvas.Handle := DC;
    GetWindowRect(Handle, UpdateRect);

    OffsetRect(UpdateRect, - UpdateRect.Left, - UpdateRect.Top);

    HeaderRect := UpdateRect;
    HeaderRect.Left   := HeaderRect.Left - FBorderSize;
    HeaderRect.Bottom := FTitleHeight    + FBorderSize;

    if FShowBorder then
    begin
      HeaderRect.Bottom := FTitleHeight + FBorderSize;
      InflateRect(HeaderRect, -FBorderSize, 0);
    end;

    if(FShowHeader) and(Message.Unused<> wmNCPaintOnlyBorder) then
    begin
      TempCanvas := TBitmap.Create;
      try
        //Title Drawing
        TempCanvas.Width := HeaderRect.Right - HeaderRect.Left;
        TempCanvas.Height := HeaderRect.Bottom - HeaderRect.Top;
        DrawTitle(TempCanvas.Canvas, HeaderRect);

        //Title Butons Drawing
        DrawAllTitleButtons(TempCanvas.Canvas, HeaderRect);

        BitBlt(DC, HeaderRect.Left, HeaderRect.Top, TempCanvas.Width, TempCanvas.Height,
          TempCanvas.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        TempCanvas.Free;
      end;
    end;

    if FShowBorder then
    begin
      //DrawBorder(NCCanvas, UpdateRect,(Message.Unused[0] = wmNCPaintOnlyBorder));
      DrawBorder(NCCanvas, UpdateRect, False);
    end;


  finally
    NCCanvas.Free;
    ReleaseDC(Handle, DC);
  end;
  Message.Result := 0;

  inherited;
end;

procedure TDefinePucker.WMSize(var Message : TMessage);
begin
  FullRepaint :=(FGradientFill and FBGImage.Empty) or
   ((not FBGImage.Empty) and(FBGImageAlign <> iaTile )) or
   (FGradientFill and(not FBGImage.Empty) and(FBGImageAlign <> iaTile)) ;
  SetShape(FPanelCorner);
  inherited;
end;


procedure TDefinePucker.SetShape(ARounded : TPanelCorners);
var
  WinRgn : hRgn;
  WinRgn1 : hRgn;
  WinRgn2 : hRgn;
  Rectn : TRect;
  RTop, RBottom : Integer;
  AWidth, AHeight : Integer;
begin
  WinRgn := 0;
  GetWindowRect(Handle, Rectn);
  OffsetRect(Rectn, -Rectn.Left, -Rectn.Top);

  //Delete old window region
  GetWindowRgn(Handle, WinRgn);
  DeleteObject(WinRgn);

  AWidth := Width;
  AHeight := Height;
  if ARounded <> [] then
  begin
    RTop := 0;
    RBottom := AHeight;
    if(rcTopLeft in ARounded) or(rcTopRight in ARounded) then RTop := DefaultCornerRadius div 2;
    if(rcBottomLeft in ARounded) or(rcBottomRight in ARounded) then RBottom := AHeight - DefaultCornerRadius div 2;

    WinRgn := CreateRectRgn(0, RTop, AWidth, RBottom);

    //Create topleft rounded corner
    if  rcTopLeft in ARounded then
    begin
      WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, DefaultCornerRadius div 2, DefaultCornerRadius, DefaultCornerRadius);
      WinRgn2 := CreateEllipticRgn(0,0,DefaultCornerRadius+1,DefaultCornerRadius+1);
      CombineRgn(WinRgn1, WinRgn1, WinRgn2, RGN_OR);
      CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      DeleteObject(WinRgn1);
      DeleteObject(WinRgn2);

      //Create result region
      if rcTopRight in ARounded then
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, 0, AWidth - DefaultCornerRadius div 2, DefaultCornerRadius);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end
      else
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, 0, AWidth, DefaultCornerRadius);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end;
      DeleteObject(WinRgn1);
    end;

    //Create topright rounded corner
    if  rcTopRight in ARounded then
    begin
      WinRgn1 := CreateRectRgn(AWidth - DefaultCornerRadius, 0, AWidth - DefaultCornerRadius div 2, DefaultCornerRadius);
      WinRgn2 := CreateEllipticRgn(AWidth - DefaultCornerRadius + 1, 0, AWidth+1, DefaultCornerRadius);
      CombineRgn(WinRgn1, WinRgn1, WinRgn2, RGN_OR);
      CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      DeleteObject(WinRgn1);
      DeleteObject(WinRgn2);

      //Create result region
      if rcTopLeft in ARounded then
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, 0, AWidth - DefaultCornerRadius div 2, DefaultCornerRadius);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end
      else
      begin
        WinRgn1 := CreateRectRgn(0, 0, AWidth - DefaultCornerRadius, DefaultCornerRadius);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end;
      DeleteObject(WinRgn1);
    end;

    //Create bottomleft rounded corner
    if  rcBottomLeft in ARounded then
    begin
      WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, AHeight - DefaultCornerRadius, DefaultCornerRadius, AHeight - DefaultCornerRadius div 2);
      WinRgn2 := CreateEllipticRgn(0, AHeight - DefaultCornerRadius, DefaultCornerRadius,AHeight+1);
      CombineRgn(WinRgn1, WinRgn1, WinRgn2, RGN_OR);
      CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      DeleteObject(WinRgn1);
      DeleteObject(WinRgn2);

      //Create result region
      if rcBottomRight in ARounded then
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, AHeight - DefaultCornerRadius div 2, AWidth - DefaultCornerRadius div 2, AHeight);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end
      else
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, AHeight - DefaultCornerRadius div 2, AWidth, AHeight);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end;
      DeleteObject(WinRgn1);
    end;

    //Create bottomright rounded corner
    if  rcBottomRight in ARounded then
    begin
      WinRgn1 := CreateRectRgn(AWidth - DefaultCornerRadius, AHeight - DefaultCornerRadius,
        AWidth - DefaultCornerRadius div 2, AHeight);
      WinRgn2 := CreateEllipticRgn(AWidth - DefaultCornerRadius + 1, AHeight-DefaultCornerRadius+1, AWidth+1, AHeight+1);
      CombineRgn(WinRgn1, WinRgn1, WinRgn2, RGN_OR);
      CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      DeleteObject(WinRgn1);
      DeleteObject(WinRgn2);

      //Create result region
      if rcBottomLeft in ARounded then
      begin
        WinRgn1 := CreateRectRgn(DefaultCornerRadius div 2, AHeight - DefaultCornerRadius div 2, AWidth - DefaultCornerRadius div 2+1, AHeight);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR)
      end
      else
      begin
        WinRgn1 := CreateRectRgn(0, AHeight - DefaultCornerRadius div 2, AWidth - DefaultCornerRadius div 2+1, AHeight);
        CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);
      end;
      DeleteObject(WinRgn1);
    end;

  end
  else
    WinRgn := CreateRectRgn(0, 0, AWidth, AHeight);


  //////////////////////////////////////////////////////////////////////////////
  ////////////////  Creating  top region for title bitmap //////////////////////
  //////////////////////////////////////////////////////////////////////////////
  {
  if(not FTitleImage.Empty) and(FTitleImageAlign in [tiaLeft, tiaCenter, tiaRight]) and
    (FTitleImage.Height > FTitleHeight) then
  begin
    if FTitleImageTransparent then
      WinRgn1 := CreateRegionFromBitmap(FTitleImage,
                 FTitleImage.Canvas.Pixels [FTitleImage.Canvas.ClipRect.Left, FTitleImage.Canvas.ClipRect.Top],
                 0)
    else
      WinRgn1 := CreateRegionFromBitmap(FTitleImage, clNone,  30);

    //OffsetRgn(WinRgn1, 5, FTitleImage.Height - FTitleHeight + 5);
    OffsetRgn(WinRgn, 0, FTitleImage.Height - FTitleHeight + 5);
    CombineRgn(WinRgn, WinRgn, WinRgn1, RGN_OR);

    DeleteObject(WinRgn1);
  end;        }
  //////////////////////////////////////////////////////////////////////////////
  
  SetWindowRgn(Handle, WinRgn, true);
end;

procedure TDefinePucker.ForceReDraw;
begin
  SendMessage(Handle, WM_NCPAINT, 0, 0);
  Invalidate;
end;

procedure TDefinePucker.Loaded;
begin
  inherited;
  if FPanelCorner <> [] then SetShape(FPanelCorner);
  SendMessage(Handle, WM_NCPAINT, 0, 0);

  if Minimized then
    FHeight := DefaultHeight
  else
    FHeight := Height;
  FOldBounds := BoundsRect;
  if Align = alClient then
  begin
    FOldAlign := alNone;
    FMaximized := true;
  end
  else
    FMaximized := false;
end;

procedure TDefinePucker.MouseEnter(var Message : TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(self);
end;

procedure TDefinePucker.MouseLeave(var Message : TMessage);
begin
  inherited;
  if FMouseOnHeader then
  begin
    FMouseOnHeader := False;
    FullRepaint := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);

    if Assigned(FOnTitleMouseExit) then FOnTitleMouseExit(self);
  end;

  if Assigned(FOnMouseExit) then FOnMouseExit(self);
end;

procedure TDefinePucker.NCMouseDown(var Message : TWMNCLBUTTONDOWN);
var
  ATitleHeight : Integer;
begin
  if not(Message.HitTest in [HTCLOSE, HTMINBUTTON, HTMAXBUTTON]) then
  begin
    if Message.HitTest = HTCAPTION then
    begin
      if Assigned(FBeforeMoving) then FBeforeMoving(self);
    end;

    inherited;

    Invalidate;
    if Message.HitTest in [HTTOP, HTLEFT, HTRIGHT, HTBOTTOM,
          HTTOPLEFT, HTTOPRIGHT, HTBOTTOMLEFT, HTBOTTOMRIGHT] then
    begin
      Invalidate;
    end;

    if Message.HitTest = HTCAPTION then
    begin
      if Assigned(FAfterMoving) then FAfterMoving(self);
    end;

    try Parent.Realign; except end;
  end;

  ATitleHeight := 0;
  if FShowHeader then ATitleHeight := FTitleHeight;
  if FShowBorder then ATitleHeight := ATitleHeight + 1;

  if Assigned(FOnTitleMouseDown) then
    FOnTitleMouseDown(Self, mbLeft, [],
      ScreenToClient(Point(Message.XCursor, Message.YCursor)).x,
      ScreenToClient(Point(Message.XCursor, Message.YCursor)).y + ATitleHeight);

end;

procedure TDefinePucker.NCMouseUp(var Message : TWMNCLBUTTONUP);
var
  ATitleHeight : Integer;
begin
  inherited;
  Parent.Realign;
  if Assigned(FOnTitleClick) and
     not(Message.HitTest in [HTCLOSE, HTMINBUTTON, HTMAXBUTTON]) then FOnTitleClick(Self);

  ATitleHeight := 0;
  if FShowHeader then ATitleHeight := FTitleHeight;
  if FShowBorder then ATitleHeight := ATitleHeight + 1;

  if Assigned(FOnTitleMouseUp) then
    FOnTitleMouseUp(Self, mbLeft, [],
      ScreenToClient(Point(Message.XCursor, Message.YCursor)).x,
      ScreenToClient(Point(Message.XCursor, Message.YCursor)).y + ATitleHeight);

  case Message.HitTest of
    HTCLOSE:
    begin
      Visible := False;
      if Assigned(FAfterClose) then FAfterClose(Self);
    end;
    HTMAXBUTTON:
    begin
      Maximized := not Maximized;
    end;
    HTMINBUTTON:
    begin
      Minimized := not Minimized;
    end;
  end;
end;

procedure TDefinePucker.NCMouseDblClick(var Message : TWMNCLButtonDblClk);
begin
  if Assigned(FOnTitleDblClick) then FOnTitleDblClick(self);
  if tbMinimize in FTitleButtons then Minimized := not Minimized else
    if tbMaximize in FTitleButtons then Maximized := not Maximized;
end;

procedure TDefinePucker.SetFillDirection(AFillDirection : TFillDirection);
begin
  if FFillDirection <> AFillDirection then begin
     FFillDirection := AFillDirection;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetCaption(AValue : String);
begin
  if FCaption <> AValue then begin
     FCaption := AValue;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetTitleAlignment(AValue : TAlignment);
begin
  if FTitleAlignment <> AValue then begin
     FTitleAlignment := AValue;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetTitleFillDirect(AValue : TFillDirection);
begin
  if FTitleFillDirect <> AValue then begin
     FTitleFillDirect := AValue;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetTitleImage(AValue : TBitmap);
begin
  if not FTitleImage.Empty then FTitleImage.FreeImage;
  FTitleImage.Assign(AValue);
  ForceReDraw;
end;

procedure TDefinePucker.SetTitleFont(AFont : TFont);
begin
  FTitleFont.Assign(AFont);
  ForceReDraw;
end;


procedure TDefinePucker.OnTitleFontChange(Sender : TObject);
begin
  ForceReDraw;
end;


procedure TDefinePucker.SetTitleHeight(AHeight : Integer);
begin
  if FTitleHeight <> AHeight then begin
     FTitleHeight := AHeight;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetBGImage(AImage : TBitmap);
begin
  FBGImage.Assign(AImage);
  ForceReDraw;
end;

procedure TDefinePucker.SetBGImageAlign(AImageAlign : TBGImageAlign);
begin
  if FBGImageAlign <> AImageAlign then begin
     FBGImageAlign := AImageAlign;
     if(FBGImageAlign = iaTile) or(FBGImageAlign = iaStretch) then FGradientFill := False;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetTitleImageAlign(AValue : TTitleImageAlign);
begin
  if FTitleImageAlign <> AValue then begin
     FTitleImageAlign := AValue;
     ForceReDraw;
  end;
end;

procedure TDefinePucker.SetPanelCorner(AValue : TPanelCorners);
begin
  if FPanelCorner <> AValue then begin
     FPanelCorner := AValue;
     FullRepaint := true;
     SetShape(FPanelCorner);
     FullRepaint := False;
  end;
end;

procedure TDefinePucker.SetMinimized(AValue : Boolean);
{/*****************************/*}
  procedure Anime(NewSize : Integer);
  var
    I, Step, Iteration : Integer;
    YStart, YEnd : Integer;
    OldFRepaint : Boolean;
  begin
      //Animation
      if FAnimation then
      begin
        Step := 0;
        if Height > NewSize then
        begin
          YStart := newSize;
          YEnd := Height;
        end
        else
        begin
          YStart := Height;
          YEnd := newSize;
        end;
        Iteration :=(YEnd - YStart) div 10;
        if Iteration = 0 then Iteration := 1;
        OldFRepaint := FullRepaint;
        FullRepaint := False;
        For I := YStart to YEnd do
        begin
          if Step = Iteration then
          begin
            if Height < NewSize then Height := Height + Step
            else Height := Height - Step;
            Application.ProcessMessages;
            Step := 0;
          end;
          Inc(Step);
        end;
        FullRepaint := OldFRepaint;
      end;
  end;
{/*****************************/*}

begin
  if(FMinimized <> AValue) and(not FMinimizing ) then
  begin
    Maximized := False;
    FMinimized := AValue;

    if AValue then
    begin
      try
        FMinimizing := True;
        FHeight := Height;
        if FAnimation then Anime(FTitleHeight + FBorderSize);
        Height := FTitleHeight + FBorderSize;
      finally
        FMinimizing := False;
      end;
    end
    else
    begin
      try
        FMinimizing := true;
        if Height = FHeight then FHeight := FDefaultHeight;
        if FAnimation then Anime(FHeight);
        Height := FHeight;
      finally
        FMinimizing := false;
      end;
    end;

    Invalidate;
    if Assigned(FAfterMinimized) then
      FAfterMinimized(Self, FMinimized);
  end;
end;

procedure TDefinePucker.SetMaximized(AValue : Boolean);
begin
  if FMaximized <> AValue then
  begin
    FMaximized := AValue;

    if FMaximized then
    begin
      FOldBounds := BoundsRect;
      FOldAlign := Align;
      Align := alClient;
    end
    else
    begin
      Align := FOldAlign;
      BoundsRect := FOldBounds;
    end;

    Invalidate;
    if Assigned(FAfterMaximized) then
      FAfterMaximized(Self, FMaximized);
  end;
end;


procedure TDefinePucker.SetTitleButtons(AValue : TTitleButtons);
begin
  if FTitleButtons <> AValue then
  begin
    FTitleButtons := AValue;
    if Parent <> nil then
    begin
      SendMessage(Handle, WM_NCPAINT, 0, 0);
      SendMessage(Handle, WM_SIZE, 0, 0);
    end;  
  end;
end;

procedure TDefinePucker.SetDefaultHeight(AValue : Integer);
begin
  if AValue <> FDefaultHeight then
  begin
    FDefaultHeight := AValue;
    if Minimized then FHeight := FDefaultHeight;
  end;
end;

procedure TDefinePucker.CMIsToolControl(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TDefinePucker.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TDefinePucker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TDefinePucker.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  Rect: TRect;
begin
  if FullRepaint then
     Invalidate
  else
  begin
    Rect.Right := Width;
    Rect.Bottom := Height;
    if Message.WindowPos^.cx <> Rect.Right then
    begin
       Rect.Top := 0;
       Rect.Left := Rect.Right - 2;
       InvalidateRect(Handle, @Rect, True);
    end;
    if Message.WindowPos^.cy <> Rect.Bottom then
    begin
       Rect.Left := 0;
       Rect.Top := Rect.Bottom - 2;
       InvalidateRect(Handle, @Rect, True);
    end;
  end;
  inherited;
end;

procedure TDefinePucker.SetTitleButtonsStyle(AValue: TTitleButtonsStyle);
begin
 if FTitleButtonsStyle <> AValue then
 begin
    FTitleButtonsStyle := AValue;
    Invalidate;
 end;
end;

procedure TDefinePucker.SetTitleBtnBorderSize(AValue: Integer);
begin
 if FTitleBtnBorderSize <> AValue then
 begin
  FTitleBtnBorderSize := AValue;
  Invalidate;
 end;
end;

procedure TDefinePucker.SetName(const Value: TComponentName);
begin              
  if (csDesigning in ComponentState)and((GetTextLen = 0)or
     (CompareText(FCaption, Name) = 0)) then
      FCaption := Value;
  inherited SetName(Value);
end;

procedure TDefinePucker.SetColors(Index: Integer; Value: TColor);
begin
 case Index of
   0 : FStartColor          := Value;
   1 : FEndColor            := Value;
   2 : FTitleStartColor     := Value;
   3 : FTitleEndColor       := Value;
   4 : FTitleColor          := Value;
   5 : FTitleBtnBorderColor := Value;
   6 : FTitleBtnBGColor     := Value;
   7 : FBorderColor         := Value;
 end;
 Invalidate;
end;

procedure TDefinePucker.SetBools(Index: Integer; Value: Boolean);
begin
 case Index of
   0:if FGradientFill <> Value then begin
        FGradientFill := Value;
        ForceReDraw;
     end;
   1:if FFullRepaint <> Value then begin
        FFullRepaint := Value;
        ForceReDraw;
     end;
   2:if FShowHeader <> Value then begin
        FShowHeader := Value;
        SendMessage(Handle, WM_SIZE, 0, 0);
     end;
   3:SetMinimized(Value);
   4:SetMaximized(Value);
   5:if FTitleShadowOnMouseEnter <> Value then begin
        FTitleShadowOnMouseEnter := Value;
     end;
   6:if FTitleGradient <> Value then begin
        FTitleGradient := Value;
        ForceReDraw;
     end;
   7:if FMovable <> Value then begin
        FMovable := Value;
     end;
   8:if FSizable <> Value then begin
        FSizable := Value;
     end;
   9:if FShowBorder <> Value then begin
        FShowBorder := Value;
        SetShape(FPanelCorner);
     end;
  10:if FAnimation <> Value then begin
        FAnimation := Value;
     end;
  11:if FBGImageTransparent <> Value then begin
        FBGImageTransparent := Value;
        ForceReDraw;
     end;
  12:if FTitleImageTransparent <> Value then begin
        FTitleImageTransparent := Value;
        ForceReDraw;
     end;
 end;
end;

{ TDefineSpin }

constructor TDefineSpin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle   := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];
  FUpButton      := CreateButton;
  FDownButton    := CreateButton;
  UpGlyph        := nil;
  DownGlyph      := nil;
  FFocusedButton := FUpButton;
  SetBounds(0,0,20,10);
end;

function TDefineSpin.CreateButton: TDefineSpins;
begin
  Result := TDefineSpins.Create(Self);
  Result.FoisChange   := false;
  Result.OnClick      := BtnClick;
  Result.OnMouseDown  := BtnMouseDown;
  Result.Visible      := True;
  Result.Enabled      := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent       := Self;
end;

procedure TDefineSpin.Notification (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TDefineSpin.AdjustSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  FUpButton.SetBounds(0, 0, 15, H);
  FDownButton.SetBounds(16, 0, 15, H);
end;

procedure TDefineSpin.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TDefineSpin.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

  // check for minimum size
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TDefineSpin.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TDefineSpin.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TDefineSpin.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn(FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn(FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TDefineSpin.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TDefineSpins(Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TDefineSpin.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
    if Assigned(FOnUpClick) then
      FOnUpClick(Self);
  if Sender = FDownButton then
    if Assigned(FOnDownClick) then
      FOnDownClick(Self);
end;

procedure TDefineSpin.SetFocusBtn (Btn: TDefineSpins);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then 
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TDefineSpin.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDefineSpin.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, Width, Height);
end;

function TDefineSpin.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TDefineSpin.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'Flat_Up');
    FUpButton.NumGlyphs := 1;
    FUpButton.Margin := 2;
    FUpButton.Invalidate;
    FUpButton.Layout := blGlyphTop;
  end;
end;

function TDefineSpin.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TDefineSpin.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TDefineSpin.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TDefineSpin.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'Flat_Down');
    FDownButton.NumGlyphs := 1;
    FDownButton.Margin := 2;
    FDownButton.Invalidate;
    FDownButton.Layout := blGlyphBottom;
  end;
end;

function TDefineSpin.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TDefineSpin.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

{ TDefineSpins }

constructor TDefineSpins.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crHandPoint;
end;

destructor TDefineSpins.Destroy;
begin
  if FRepeatTimer <> nil then
     FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TDefineSpins.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
       FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer  := TimerExpired;
    FRepeatTimer.Interval := DefaultInitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TDefineSpins.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
     FRepeatTimer.Enabled  := False;
end;

procedure TDefineSpins.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := DefaultRepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;
{ TDefineSpeed }
constructor TDefineSpeed.Create(AOwner: TComponent);
begin
  FGlyph        := TBitmap.Create;
  inherited Create(AOwner);
  ControlStyle  := [csCaptureMouse, csDoubleClicks];
  ParentFont    := True;
  ParentColor   := True;
  fColorFocused := DefaultFocusedColor;
  fColorDown    := DefaultDownColor;
  FColorBorder  := DefaultBorderColor;
  FColorShadow  := DefaultShadowColor;
  FState        := bsUp;
  fColorFlat    := DefaultFlatColor;
  FAutoColor    := DefaultFoisColor;
  FTransBorder  := false;
  FFoisChange   := True;
  FAutoStyle    := [fsBold];
  FSpacing      := 4;
  FMargin       := -1;
  FNumGlyphs    := 1;
  FLayout       := blGlyphTop;
  FModalResult  := mrNone;
  FTransparent  := tmNone;
  SetBounds(0, 0, 25, 25);
end;

destructor TDefineSpeed.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TDefineSpeed.Paint;
var
  FTransColor: TColor;
  FImageList: TImageList;
  sourceRect, destRect: TRect;
  tempGlyph: TBitmap;
  Offset: TPoint;
begin
  // get the transparent color
  FTransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  Canvas.Font := Self.Font;

  if FState in [bsDown, bsExclusive] then
     Offset := Point(1, 1)
  else
     Offset := Point(0, 0);

  if MouseIn and FFoisChange then begin
     canvas.Font.Color := FAutoColor;
     canvas.Font.Style := FAutoStyle;
  end;
  
  CalcButtonLayout(Canvas, ClientRect, Offset, FLayout, FSpacing,
                   FMargin, FGlyph, FNumGlyphs, Caption, TextBounds, GlyphPos);

  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else
    if FState = bsDisabled then
      if FDown and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;

  // DrawBackground
  case FTransparent of
    tmAlways:;
    tmNone:
      begin
        case FState of
          bsUp:
            if MouseIn then
              Canvas.Brush.Color := fColorFocused
            else
              Canvas.Brush.Color := fColorFlat;
          bsDown:
            Canvas.Brush.Color := fColorDown;
          bsExclusive:
            if MouseIn then
              Canvas.Brush.Color := fColorFocused
            else
              Canvas.Brush.Color := fColorDown;
          bsDisabled:
            Canvas.Brush.Color := fColorFlat;
        end;
        Canvas.FillRect(ClientRect);
      end;
    tmNotFocused:
      if MouseIn then
      begin
        case FState of
          bsUp:
            if MouseIn then
              Canvas.Brush.Color := fColorFocused
            else
              Canvas.Brush.Color := Self.Color;
          bsDown:
            Canvas.Brush.Color := fColorDown;
          bsExclusive:
            if MouseIn then
              Canvas.Brush.Color := fColorFocused
            else
              Canvas.Brush.Color := fColorDown;
          bsDisabled:
            Canvas.Brush.Color := Self.Color;
        end;
        Canvas.FillRect(ClientRect);
      end;
  end;

  if not FTransBorder then begin // DrawBorder
   case FState of
    bsUp: if MouseIn then
             DrawButtonBorder(canvas, ClientRect, FColorShadow, 1)
          else
             DrawButtonBorder(canvas, ClientRect, FColorBorder, 1);
    bsDown, bsExclusive:
             DrawButtonBorder(canvas, ClientRect, FColorShadow, 1);
    bsDisabled:
             DrawButtonBorder(canvas, ClientRect, FColorBorder, 1);
   end;
  end;
  // DrawGlyph
  if not FGlyph.Empty then
  begin
    tempGlyph := TBitmap.Create;
    case FNumGlyphs of
      1: case FState of
           bsUp:        sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsDisabled:  sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsDown:      sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
         end;
      2: case FState of
           bsUp:        sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  sourceRect := Rect(FGlyph.Width div FNumGlyphs, 0, FGlyph.Width, FGlyph.Height);
           bsDown:      sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
         end;
      3: case FState of
           bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
           bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
           bsExclusive: SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
         end;
      4: case FState of
           bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
           bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, (FGlyph.Width div FNumGlyphs) * 3, FGlyph.Height);
           bsExclusive: SourceRect := Rect((FGlyph.width div FNumGlyphs) * 3, 0, FGlyph.Width, FGlyph.Height);
         end;
    end;

    destRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
    tempGlyph.Width := FGlyph.Width div FNumGlyphs;
    tempGlyph.Height := FGlyph.Height;
    tempGlyph.canvas.copyRect(destRect, FGlyph.canvas, sourcerect);

    if (FNumGlyphs = 1) and (FState = bsDisabled) then
    begin
      tempGlyph := CreateDisabledBitmap(tempGlyph, clBlack, clBtnFace, clBtnHighlight, clBtnShadow, True);
      FTransColor := tempGlyph.Canvas.Pixels[0, tempGlyph.Height - 1];
    end;

    FImageList := TImageList.CreateSize(FGlyph.Width div FNumGlyphs, FGlyph.Height);
    try
      FImageList.AddMasked(tempGlyph, FTransColor);
      if MouseIn and FFoisChange then
           FImageList.Draw(canvas, pred(glyphpos.x), pred(glyphpos.y), 0)
        else
           FImageList.Draw(canvas, glyphpos.x, glyphpos.y, 0);
      //FImageList.Draw(canvas, glyphpos.x, glyphpos.y, 0);
    finally
      FImageList.Free;
    end;
    tempGlyph.free;
  end;

  // DrawText
  Canvas.Brush.Style := bsClear;
  if FState = bsDisabled then
  begin
    OffsetRect(TextBounds, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    OffsetRect(TextBounds, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end
  else
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TDefineSpeed.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseIn := not (FindDragTarget(P, True) = Self);
    if FMouseIn then
      MouseLeave
    else
      MouseEnter;
  end;
end;

procedure TDefineSpeed.Loaded;
begin
  inherited Loaded;
  Invalidate;
end;

procedure TDefineSpeed.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if(Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TDefineSpeed.MouseMove (Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited;
  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end;
end;

procedure TDefineSpeed.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := bsUp;
      FMouseIn := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click else MouseLeave;
    UpdateTracking;
  end;
end;

procedure TDefineSpeed.Click;
begin
  if Parent <> nil then
     GetParentForm(self).ModalResult := FModalResult;
  if Assigned(PopupMenu) then
     PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X,
                     ClientToScreen(Point(0, Height)).Y);
  inherited Click;
end;

function TDefineSpeed.GetPalette: HPALETTE;
begin
  Result := FGlyph.Palette;
end;

procedure TDefineSpeed.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: fColorFocused := Value;
    1: fColorDown    := Value;
    2: FColorBorder  := Value;
    3: FColorShadow  := Value;
    4: FColorFlat    := Value;
    5: FAutoColor     := Value;
  end;
  Invalidate;
end;

procedure TDefineSpeed.SetGlyph(Value: TBitmap);
begin
  if value <> FGlyph then
  begin
    FGlyph.Assign(value);
    if not FGlyph.Empty then
    begin
      if FGlyph.Width mod FGlyph.Height = 0 then
      begin
        FNumGlyphs := FGlyph.Width div FGlyph.Height;
        if FNumGlyphs > 4 then FNumGlyphs := 1;
      end;
    end;
    Invalidate;
  end;
end;

procedure TDefineSpeed.SetNumGlyphs(Value: TNumGlyphs);
begin
  if value <> FNumGlyphs then
  begin
    FNumGlyphs := value;
    Invalidate;
  end;
end;

procedure TDefineSpeed.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TDefineSpeed.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TDefineSpeed.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TDefineSpeed.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TDefineSpeed.SetMargin(Value: Integer);
begin
  if(Value <> FMargin) and(Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TDefineSpeed.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TDefineSpeed.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TDefineSpeed.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TDefineSpeed.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseIn := False;
    FState := bsDisabled;
    //RemoveMouseTimer;
  end;
  UpdateTracking;
  Invalidate;
end;

procedure TDefineSpeed.CMButtonPressed(var Message: TMessage);
var
  Sender: TDefineSpeed;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TDefineSpeed(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TDefineSpeed.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TDefineSpeed.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TDefineSpeed.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TDefineSpeed.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
      Color := TDefineSpeed(Parent).Color;
  Invalidate;
end;

procedure TDefineSpeed.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  if (Parent <> nil)and(not ParentColor) then
      Color := TDefineSpeed(Parent).Color;
  Invalidate;
end;

procedure TDefineSpeed.MouseEnter;
begin
  if Enabled and not MouseIn  then
  begin
    FMouseIn := True;
    Invalidate;
  end;
end;

procedure TDefineSpeed.MouseLeave;
begin
  if Enabled and MouseIn and not FDragging then
  begin
    FMouseIn := False;
    Invalidate;
  end;
end;

{$IFDEF DFS_DELPHI_4_UP}
procedure TDefineSpeed.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;
{$ENDIF}

procedure TDefineSpeed.SetTransparent(const Value: TTransparentMode);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     Invalidate;
  end;
end;

procedure TDefineSpeed.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self)
  else if not(csDesigning in ComponentState) then
     MouseEnter;
end;

procedure TDefineSpeed.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self)
  else if not(csDesigning in ComponentState) then
     MouseLeave;
end;

procedure TDefineSpeed.SetFoisChange(const Value: Boolean);
begin
  if FFoisChange <> Value then begin
     FFoisChange := Value;
     Invalidate;
  end;
end;

procedure TDefineSpeed.SetAutoStyle(const Value: TFontStyles);
begin
  if FAutoStyle <> Value then begin
     FAutoStyle := Value;
     Invalidate;
  end;
end;

procedure TDefineSpeed.SetTransBorder(const Value: Boolean);
begin
  if FTransBorder <> Value then begin
     FTransBorder := Value;
     Invalidate;
  end;
end;

function TDefineSpeed.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefineButton }

constructor TDefineButton.Create(AOwner: TComponent);
begin
  FGlyph        := TBitmap.Create;
  inherited Create(AOwner);
  ControlStyle  := [csCaptureMouse, csOpaque, csDoubleClicks, csSetCaption];
  TabStop       := true;
  ParentFont    := True;
  ParentColor   := True;
  fColorFocused := DefaultFocusedColor;
  fColorDown    := DefaultDownColor;
  FColorBorder  := DefaultBorderColor;
  FColorShadow  := DefaultShadowColor;
  FState        := bsUp;
  fColorFlat    := DefaultFlatColor;
  FAutoColor    := DefaultFoisColor;
  FTransBorder  := false;
  FFoisChange   := True;
  FAutoStyle    := [fsBold];
  FSpacing      := 4;
  FMargin       := -1;
  FNumGlyphs    := 1;
  FLayout       := blGlyphTop;
  FModalResult  := mrNone;
  FTransparent  := tmNone;
  fHasFocusFrame:= true;
  SetBounds(0, 0, 100, 25);
end;

destructor TDefineButton.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TDefineButton.Paint;
var
  FTransColor: TColor;
  FImageList: TImageList;
  sourceRect, destRect, FocusRect: TRect;
  tempGlyph, memBitmap: TBitmap;
  Offset: TPoint;
begin
  // get the transparent color
  FTransColor  := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];

  memBitmap := TBitmap.Create; // create memory-bitmap to draw flicker-free
  try
    memBitmap.Height := ClientRect.Bottom;
    memBitmap.Width  := ClientRect.Right;
    memBitmap.Canvas.Font := Self.Font;

    if FState in [bsDown, bsExclusive] then
      Offset := Point(1, 1)
    else
      Offset := Point(0, 0);
    if MouseIn and FFoisChange then begin
       memBitmap.canvas.Font.Color := FAutoColor;
       memBitmap.canvas.Font.Style := FAutoStyle;
    end;
    CalcButtonLayout(memBitmap.Canvas, ClientRect, Offset, FLayout, FSpacing,
                     FMargin, FGlyph, FNumGlyphs, Caption, TextBounds, GlyphPos);

    if not Enabled then
    begin
      FState    := bsDisabled;
      FDragging := False;
    end
    else
    begin
      if FState = bsDisabled then
      begin
        if FDown and (GroupIndex <> 0) then
           FState := bsExclusive
        else
           FState := bsUp;
      end;
    end;

    // DrawBackground
    case FTransparent of
      tmAlways:
        DrawParentImage(Self, memBitmap.Canvas);
      tmNone:
        begin
          case FState of
            bsUp:
              if MouseIn then
                 memBitmap.Canvas.Brush.Color := fColorFocused
              else
                 memBitmap.Canvas.Brush.Color := fColorFlat;
            bsDown:
                 memBitmap.Canvas.Brush.Color := fColorDown;
            bsExclusive:
              if MouseIn then
                 memBitmap.Canvas.Brush.Color := fColorFocused
              else
                 memBitmap.Canvas.Brush.Color := fColorDown;
            bsDisabled:
                 memBitmap.Canvas.Brush.Color := fColorFlat;
          end;
          memBitmap.Canvas.FillRect(ClientRect);
          //memBitmap.Canvas.Polygon();
        end;
      tmNotFocused:
        if MouseIn then
        begin
          case FState of
            bsUp:
              if MouseIn then
                memBitmap.Canvas.Brush.Color := fColorFocused
              else
                memBitmap.Canvas.Brush.Color := fColorFlat;
            bsDown:
              memBitmap.Canvas.Brush.Color := fColorDown;
            bsExclusive:
              if MouseIn then
                memBitmap.Canvas.Brush.Color := fColorFocused
              else
                memBitmap.Canvas.Brush.Color := fColorDown;
            bsDisabled:
              memBitmap.Canvas.Brush.Color := fColorFlat;
          end;
          memBitmap.Canvas.FillRect(ClientRect);
        end
        else
          DrawParentImage(Self, memBitmap.Canvas);
    end;
    if not FTransBorder then begin // DrawBorder
     case FState of
      bsUp: if MouseIn then
               DrawButtonBorder(memBitmap.canvas, ClientRect, FColorShadow, 1)
            else if FDefault then
               DrawButtonBorder(memBitmap.canvas, ClientRect, FColorBorder, 2)
            else
               DrawButtonBorder(memBitmap.canvas, ClientRect, FColorBorder, 1);
      bsDown, bsExclusive:
               DrawButtonBorder(memBitmap.canvas, ClientRect, FColorShadow, 1);
      bsDisabled:
               DrawButtonBorder(memBitmap.canvas, ClientRect, FColorBorder, 1);
     end;
    end;
    if (MouseIn)and(fHasFocusFrame)and(Enabled) then begin
       with ClientRect do begin
        FocusRect := Rect(Left+2,Top+2,Right-2,Bottom-2);
       end;
       if not FTransBorder then
          memBitmap.Canvas.DrawFocusRect(FocusRect);
    end;
    // DrawGlyph
    if not FGlyph.Empty then
    begin
      tempGlyph := TBitmap.Create;
      case FNumGlyphs of
        1: case FState of
             bsUp:        sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
             bsDisabled:  sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
             bsDown:      sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
             bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           end;
        2: case FState of
             bsUp:        sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
             bsDisabled:  sourceRect := Rect(FGlyph.Width div FNumGlyphs, 0, FGlyph.Width, FGlyph.Height);
             bsDown:      sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
             bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           end;
        3: case FState of
             bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
             bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
             bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
             bsExclusive: SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
           end;
        4: case FState of
             bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
             bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
             bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, (FGlyph.Width div FNumGlyphs) * 3, FGlyph.Height);
             bsExclusive: SourceRect := Rect((FGlyph.width div FNumGlyphs) * 3, 0, FGlyph.Width, FGlyph.Height);
           end;
      end;

      destRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
      tempGlyph.Width  := FGlyph.Width div FNumGlyphs;
      tempGlyph.Height := FGlyph.Height;
      tempGlyph.canvas.copyRect(destRect, FGlyph.canvas, sourcerect);

      if (FNumGlyphs = 1) and (FState = bsDisabled) then
      begin
        tempGlyph := CreateDisabledBitmap(tempGlyph, clBlack, clBtnFace, clBtnHighlight, clBtnShadow, True);
        FTransColor := tempGlyph.Canvas.Pixels[0, tempGlyph.Height - 1];
      end;

      FImageList := TImageList.CreateSize(FGlyph.Width div FNumGlyphs, FGlyph.Height);
      try
        FImageList.AddMasked(tempGlyph, FTransColor);
        if MouseIn and FFoisChange then
           FImageList.Draw(memBitmap.canvas, pred(glyphpos.x), pred(glyphpos.y), 0)
        else
           FImageList.Draw(memBitmap.canvas, glyphpos.x, glyphpos.y, 0);
      finally
        FImageList.Free;
      end;
      tempGlyph.free;
    end;

    // DrawText
    memBitmap.Canvas.Brush.Style := bsClear;
    if FState = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      memBitmap.Canvas.Font.Color := clBtnHighlight;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      OffsetRect(TextBounds, -1, -1);
      memBitmap.Canvas.Font.Color := clBtnShadow;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end
    else
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

    // Copy memBitmap to screen
    canvas.CopyRect(ClientRect, memBitmap.canvas, ClientRect);
  finally
    memBitmap.free; // delete the bitmap
  end;
end;

procedure TDefineButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseIn := not (FindDragTarget(P, True) = Self);
    if FMouseIn then
       MouseLeave
    else
       MouseEnter;
  end;
end;

procedure TDefineButton.Loaded;
begin
  inherited Loaded;
  Invalidate;
end;

procedure TDefineButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if(Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
    SetFocus;
  end;
end;

procedure TDefineButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited;
  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end;
end;

procedure TDefineButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := bsUp;
      FMouseIn := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click else
      MouseLeave;
    UpdateTracking;
  end;
end;

procedure TDefineButton.Click;
begin
  if Parent <> nil then begin
     GetParentForm(self).ModalResult := FModalResult;
     SetDown(False);
  end;
  if Assigned(PopupMenu) then
     PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X,
                     ClientToScreen(Point(0, Height)).Y);
  inherited Click;
end;   

function TDefineButton.GetPalette: HPALETTE;
begin
  Result := FGlyph.Palette;
end;

procedure TDefineButton.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: fColorFocused  := Value;
    1: fColorDown     := Value;
    2: FColorBorder   := Value;
    3: FColorShadow   := Value;
    4: FColorFlat     := Value;
    5: FAutoColor     := Value;
  end;
  Invalidate;
end;

procedure TDefineButton.SetGlyph(Value: TBitmap);
begin
  if value <> FGlyph then
  begin
    FGlyph.Assign(value);
    if not FGlyph.Empty then
    begin
      if FGlyph.Width mod FGlyph.Height = 0 then
      begin
        FNumGlyphs := FGlyph.Width div FGlyph.Height;
        if FNumGlyphs > 4 then FNumGlyphs := 1;
      end;
    end;
    Invalidate;
  end;
end;

procedure TDefineButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if value <> FNumGlyphs then
  begin
    FNumGlyphs := value;
    Invalidate;
  end;
end;

procedure TDefineButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TDefineButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TDefineButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TDefineButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TDefineButton.SetMargin(Value: Integer);
begin
  if(Value <> FMargin) and(Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TDefineButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TDefineButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TDefineButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TDefineButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then begin
    FMouseIn := False;
    FState := bsDisabled;
   // RemoveMouseTimer;
  end;
  UpdateTracking;
  Invalidate;
end;

procedure TDefineButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TDefineButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TDefineButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TDefineButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if ((CharCode = VK_RETURN) and MouseIn) and
       (KeyDataToShiftState(Message.KeyData) = []) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TDefineButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then begin
      if GroupIndex <> 0 then
         SetDown(true);
      Click;
      Result := 1;
    end;
end;

procedure TDefineButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TDefineButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TDefineButton.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
      Color := TDefineButton(Parent).Color;
  Invalidate;
end;

procedure TDefineButton.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  if (Parent <> nil)and(not ParentColor) then
      Color := TDefineButton(Parent).Color;
  Invalidate;
end;

procedure TDefineButton.MouseEnter;
begin
  if Enabled and not MouseIn then
  begin
    FMouseIn := True;
    Invalidate;
  end;
end;

procedure TDefineButton.MouseLeave;
begin
  if Enabled and MouseIn and not FDragging then
  begin
    FMouseIn := False;
    Invalidate;
  end;
end;

procedure TDefineButton.SetDefault(const Value: Boolean);
var
 {$IFDEF DFS_COMPILER_2}
  Form: TForm;
 {$ELSE}
  Form: TCustomForm;
 {$ENDIF}
begin
  FDefault := Value;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
  Invalidate;
end;

procedure TDefineButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  MouseLeave;
end;

procedure TDefineButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Enabled then
  begin
     FMouseIn := True;
     Invalidate;
  end;
end;

procedure TDefineButton.WMKeyDown(var Message: TWMKeyDown);
var CharCode:Word;
begin
  CharCode := Message.CharCode;
  if CharCode = VK_SPACE then
  begin
    if GroupIndex = 0 then
       FState := bsDown
    else
       SetDown(true);
    Invalidate;
  end;
end;

procedure TDefineButton.WMKeyUp(var Message: TWMKeyUp);
var CharCode:Word;
begin
  CharCode := Message.CharCode;
  if  CharCode = VK_SPACE then  begin
    if GroupIndex = 0 then
       FState := bsUp
    else
       SetDown(false);
    Click;
    Invalidate;
  end;
end;

procedure TDefineButton.SetTransparent(const Value: TTransparentMode);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     Invalidate;
  end;
end;

procedure TDefineButton.WMMove(var Message: TWMMove);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TDefineButton.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TDefineButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self)
  else if not(csDesigning in ComponentState) then
     MouseEnter;
end;

procedure TDefineButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self)
  else if not(csDesigning in ComponentState) then
     MouseLeave;
end;

procedure TDefineButton.SetName(const Value: TComponentName);
var ChangeText:boolean;
begin//GetTextLen  csDesigning
  ChangeText := (csLoading in ComponentState)and((GetTextLen = 0)or
                (CompareText(Caption, Name) = 0));
  inherited SetName(Value);
  if (not ChangeText)and(CompareText(Caption, Name) = 0) then Caption := Value;
end;

procedure TDefineButton.SetTransBorder(const Value: Boolean);
begin
  if FTransBorder <> Value then begin
     FTransBorder := Value;
     Invalidate;
  end;
end;

procedure TDefineButton.SetFoisChange(const Value: Boolean);
begin
  if FFoisChange <> Value then begin
     FFoisChange := Value;
     Invalidate;
  end;
end;

procedure TDefineButton.SetAutoStyle(const Value: TFontStyles);
begin
  if FAutoStyle <> Value then begin
     FAutoStyle := Value;
     Invalidate;
  end;
end;

function TDefineButton.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefinePanel }
constructor TDefinePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  { When themes are on in an application default to making
    TDefinePanel's paint with their ParentBackground }
  if ThemeServices.ThemesEnabled then
     ControlStyle := ControlStyle + [csParentBackground] - [csOpaque];
  ParentColor     := True;
  UseDockManager  := True;
  ParentFont      := True;
  Color           := clBtnFace;
  FColorBorder    := DefaultBorderColor;
  FFullRepaint    := True;
  FAlignment      := taCenter;
  FTransBorder    := false;
  FTransparent    := false;
  FStyleFace      := fsDefault;
  FBackgropStartColor := DefaultColorStart;
  FBackgropStopColor  := DefaultColorStop;
  FBackgropOrien      := fdLeftToRight;
  SetBounds(0, 0, 185, 41);
end;

procedure TDefinePanel.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FColorBorder := Value;
    1: FBackgropStartColor := Value;
    2: FBackgropStopColor := Value;
  end;
  Invalidate;
end;

procedure TDefinePanel.Paint;
var
  memBitmap: TBitmap;
  TextBounds: TRect;
  Format: UINT;
begin
  TextBounds := ClientRect;
  TextBounds.Left  := TextBounds.Left + 3;
  TextBounds.Right := TextBounds.Right - 3;
  Format := DT_SINGLELINE or DT_VCENTER;
  case Alignment of
    taLeftJustify: Format := Format or DT_LEFT;
    taCenter:      Format := Format or DT_CENTER;
    taRightJustify:Format := Format or DT_RIGHT;
  end;

  memBitmap := TBitmap.Create; // create memory-bitmap to draw flicker-free
  try
    memBitmap.Height := ClientRect.Bottom;
    memBitmap.Width  := ClientRect.Right;
    if not ThemeServices.ThemesEnabled or not ParentBackground then
    begin
      memBitmap.Canvas.Brush.Color := Color;
      memBitmap.Canvas.FillRect(TextBounds);
    end;
    // Draw Background
    if FTransparent then
       DrawParentImage(Self, memBitmap.Canvas)
    else begin
      if FStyleFace=fsDefault then begin
         memBitmap.Canvas.Brush.Color := Self.Color;
         memBitmap.Canvas.FillRect(ClientRect);
      end else
         //DrawBackdrop(memBitmap.Canvas,FBackgropStartColor,FBackgropStopColor,ClientRect,FBackgropOrien);
         GradientFillRect(memBitmap.Canvas,ClientRect,FBackgropStartColor,FBackgropStopColor,FBackgropOrien,60);
    end;
    // Draw Border
    if not FTransBorder then DrawButtonBorder(memBitmap.Canvas, ClientRect, FColorBorder, 1);
    // Draw Text
    memBitmap.Canvas.Font := Self.Font;
    memBitmap.Canvas.Brush.Style := bsClear;
    if not Enabled then begin
      OffsetRect(TextBounds, 1, 1);
      memBitmap.Canvas.Font.Color := clBtnHighlight;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Format);
      OffsetRect(TextBounds, -1, -1);
      memBitmap.Canvas.Font.Color := clBtnShadow;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    end else
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Format);

    // Copy memBitmap to screen
    canvas.CopyRect(ClientRect, memBitmap.canvas, ClientRect);
  finally
    memBitmap.free; // delete the bitmap
  end;
end;

procedure TDefinePanel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TDefinePanel.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TDefinePanel.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then begin
    FTransparent := Value;
    Invalidate;
 end;
end;

procedure TDefinePanel.SetFillDirect(Value: TFillDirection);
begin
  if FBackgropOrien <> Value then begin
     FBackgropOrien := Value;
     Invalidate;
  end;
end;

procedure TDefinePanel.SetStyleFace(Value: TStyleFace);
begin
  if FStyleFace <> Value then begin
     FStyleFace := Value;
     Invalidate;
  end;
end;

procedure TDefinePanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
     FAlignment := Value;
     Invalidate;
  end;
end;

procedure TDefinePanel.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then Message.Result := 1;
end;

procedure TDefinePanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  Rect: TRect;
begin
  if FullRepaint or(Caption <> '') then
     Invalidate
  else
  begin
    Rect.Right := Width;
    Rect.Bottom := Height;
    if Message.WindowPos^.cx <> Rect.Right then
    begin
       Rect.Top := 0;
       Rect.Left := Rect.Right - 2;
       InvalidateRect(Handle, @Rect, True);
    end;
    if Message.WindowPos^.cy <> Rect.Bottom then
    begin
       Rect.Left := 0;
       Rect.Top := Rect.Bottom - 2;
       InvalidateRect(Handle, @Rect, True);
    end;
  end;
  inherited;
end;

procedure TDefinePanel.CMDockClient(var Message: TCMDockClient);
var
  R: TRect;
  Dim: Integer;
begin
  if AutoSize then
  begin
    FAutoSizeDocking := True;
    try
      R := Message.DockSource.DockRect;
      case Align of
        alLeft: if Width = 0 then Width := R.Right - R.Left;
        alRight: if Width = 0 then
          begin
            Dim := R.Right - R.Left;
            SetBounds(Left - Dim, Top, Dim, Height);
          end;
        alTop: if Height = 0 then Height := R.Bottom - R.Top;
        alBottom: if Height = 0 then
          begin
            Dim := R.Bottom - R.Top;
            SetBounds(Left, Top - Dim, Width, Dim);
          end;
      end;
      inherited;
      Exit;
    finally
      FAutoSizeDocking := False;
    end;
  end;
  inherited;
end;

function TDefinePanel.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result :=(not FAutoSizeDocking) and inherited CanAutoSize(NewWidth, NewHeight);
end;

function TDefinePanel.GetControlsAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TDefinePanel.SetParentBackground(Value: Boolean);
begin
  { TCustomPanel needs to not have csOpaque when painting
    with the ParentBackground in Themed applications }
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TDefinePanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.style := WindowClass.style and not(CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TDefinePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Inc(Rect.Top);
  Inc(Rect.Left);
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -1, -1);
end;

procedure TDefinePanel.SetTransBorder(Value: boolean);
begin
  if FTransBorder <> Value then begin
     FTransBorder := Value;
     Invalidate;
  end;
end;

{ TDefineLabel }
procedure TDefineLabel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.Enabled := Enabled;
end;

procedure TDefineLabel.SetTicketPosition(const Value: TTicketPosition);
begin
  if FTicket = nil then exit;
  FTicketPosition := Value;
  SetTicketPoint(Value,Self,Ticket,FTicketSpace);
end;

procedure TDefineLabel.SetLabelSpacing(const Value: Integer);
begin
  if Assigned(FTicket) then FTicketSpace := Value;
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineLabel.SetupInternalLabel;
begin
  if DefaultHasTicket then begin
   if Assigned(FTicket) then exit;
   FTicket := TDefineTicket.Create(Self);
   FTicket.FreeNotification(Self);
   FTicket.AutoSize     := True;
   FTicket.Transparent  := True;
   FTicket.FocusControl := Self;
  end;
end;

procedure TDefineLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetTicketPosition(FTicketPosition);
end;

procedure TDefineLabel.SetParent(AParent: TWinControl);
begin
  if Assigned(FTicket) then
  begin
     FTicket.Parent  := AParent;
     FTicket.Visible := Visible;
  end;
  inherited SetParent(AParent);
end;

procedure TDefineLabel.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.BiDiMode := BiDiMode;
end;

procedure TDefineLabel.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTicket) then FTicket.Visible := Visible;
end;

procedure TDefineLabel.SetName(const Value: TComponentName);
begin
  if Assigned(FTicket) then begin
   if(csDesigning in ComponentState) and((FTicket.GetTextLen = 0) or
     (CompareText(FTicket.Caption, Name) = 0)) then
       FTicket.Caption := Value;
  end;
  inherited SetName(Value);
  if(csDesigning in ComponentState)and(Assigned(FTicket)) then
     Caption := '';
end;

procedure TDefineLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(AComponent = FTicket) and(Operation = opRemove) then
     FTicket := nil;
end;

procedure TDefineLabel.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + 6;
end;

procedure TDefineLabel.Loaded;
begin
  inherited Loaded;
  //if not(csDesigning in ComponentState) then
  //begin
     NewAdjustHeight;
  //end;
end;

constructor TDefineLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTicketPosition   := poLeft;
  FTicketSpace      := 3;
  SetBounds(0,0,121,20);
  SetupInternalLabel;
end;

procedure TDefineLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TDefineLabel.WMSize(var Message: TWMSize);
begin
  inherited;
  NewAdjustHeight;
end;

destructor TDefineLabel.Destroy;
begin
  if Assigned(FTicket) then FTicket.Free;
  inherited Destroy;
end;

{ TDefineCheckBox }

constructor TDefineCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle  := [csSetCaption, csDoubleClicks];
  ParentColor   := False;
  ParentFont    := True;
  TabStop       := True;
  Enabled       := True;
  Visible       := True;
  FTransparent  := True;
  Color         := DefaultFlatColor;
  FFocusedColor := DefaultBackdropColor;
  FDownColor    := DefaultBarColor;
  FCheckedColor := DefaultCheckColor;
  FBorderColor  := DefaultBorderColor;
  FLayout       := lpLeft;
  SetBounds(0, 0, 121, 15);
end;

procedure TDefineCheckBox.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FDownColor    := Value;
    2: FCheckedColor   := Value;
    3: FBorderColor  := Value;
  end;
  Invalidate;
end;

procedure TDefineCheckBox.SetLayout(Value: TLayoutPosition);
begin
  FLayout := Value;
  Invalidate;
end;

function TDefineCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TDefineCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;


procedure TDefineCheckBox.SetState(const Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    if Enabled then Click;
    invalidate;
  end;
end;

procedure TDefineCheckBox.Toggle;
begin
  case State of
    cbUnchecked:
      if AllowGrayed then State := cbGrayed else State := cbChecked;
    cbChecked: State := cbUnchecked;
    cbGrayed:  State := cbChecked;
  end;
end;

procedure TDefineCheckBox.Click;
begin
  inherited Changed;
  inherited Click;
end;

procedure TDefineCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseIn    := False;
    FMouseDown := False;
  end;
  Invalidate;
end;

procedure TDefineCheckBox.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TDefineCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
    end else if(CharCode = VK_SPACE) and Focused then begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
   end else inherited; 
end;

procedure TDefineCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) and Enabled then
  begin
    Focused := True;
    invalidate;
  end;
end;

procedure TDefineCheckBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) and Enabled then
  begin
    FMouseIn := False;
    Focused  := False;
    invalidate;
  end;
end;

procedure TDefineCheckBox.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
  begin
      Color := TDefineCheckBox(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineCheckBox.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
  begin
      Color   := TDefineCheckBox(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineCheckBox.DoEnter;
begin
  inherited DoEnter;
  Focused := True;
  invalidate;
end;

procedure TDefineCheckBox.DoExit;
begin
  inherited DoExit;
  Focused := False;
  invalidate;
end;

procedure TDefineCheckBox.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     ParentColor  := not Value;
     Invalidate;
  end;
end;

procedure TDefineCheckBox.WMMove(var Message: TWMMove);
begin
  inherited;
  if FTransparent then
     Invalidate;
end;

procedure TDefineCheckBox.WMSize(var Message: TWMSize);
begin
  inherited;
  if FTransparent then
     Invalidate;
end;

procedure TDefineCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) and
        (GetActiveWindow <> 0) and (not MouseIn) then
  begin
    FMouseIn := True;
    Invalidate;
  end;
end;

procedure TDefineCheckBox.CMMouseLeave(var Message: TMessage);
begin
 inherited;
 if MouseIn then begin
    FMouseIn := false;
    Invalidate;
 end;
end;

procedure TDefineCheckBox.CMFontChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

function TDefineCheckBox.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

procedure TDefineCheckBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Enabled then begin
     SetFocus;
     FMouseDown := true;
     Toggle;
     invalidate;
  end;
end;

procedure TDefineCheckBox.WMLButtonUP(var Message: TWMLButtonDown);
begin
  if Enabled then
  begin
    FMouseDown := false;
    invalidate;
  end;
end;

procedure TDefineCheckBox.Paint;
var
  TextBounds, CheckRect: TRect;
  Format: UINT;
  TextAs:Integer;
begin
  with Canvas do
  begin
    Lock;
    Font.Assign(self.Font);
    if Layout = lpLeft then
       Width := TextWidth(DelCapLink(Caption))+TextHeight('H')+5;
    Height := TextHeight('H')+2;
    if FTransparent then
       DrawParentImage(Self, Canvas)
    else
    begin
       Brush.Color := self.Color;
       FillRect(ClientRect);
    end;
    //draw Background
    with ClientRect do
    begin
     case FLayout of
      lpLeft: CheckRect := Rect(1, HeightOf(ClientRect) div 2 - 7, 15, HeightOf(ClientRect) div 2 + 7);
     lpRight: CheckRect := Rect(Width-15, HeightOf(ClientRect) div 2 - 7, Width-1, HeightOf(ClientRect) div 2 + 7);
     end;
    end;
    Pen.style := psSolid;
    Pen.width := 1;
    if (Focused or MouseIn)and(not(csDesigning in ComponentState)) then
    begin
     if (not FMouseDown) then
     begin
        Brush.color := FFocusedColor;
        Pen.color   := FBorderColor;
     end else begin
        Brush.color := FDownColor;
        Pen.color   := FBorderColor;
     end;
    end else begin
      Brush.color := self.Color;
      Pen.color   := FBorderColor;
    end;
    FillRect(CheckRect);
    if Checked then
    begin
     if Enabled then
        DrawInCheck(Canvas,CheckRect,FCheckedColor)
     else
        DrawInCheck(Canvas,CheckRect,clBtnShadow);
    end;
    //draw Border
    Brush.color := FBorderColor;
    FrameRect(CheckRect);
    //draw text
    Brush.Style := bsClear;
    Format      := DT_WORDBREAK;
    with ClientRect do
    begin
     TextAs:=(RectHeight(ClientRect)+ CheckRect.top - TextHeight('W')) div 2;
     case FLayout of
      lpLeft: begin
          TextBounds := Rect(Left + WidthOf(CheckRect)+2, Top + TextAs, Right + WidthOf(CheckRect), Bottom - TextAs);
          Format     := Format or DT_LEFT;
      end;
      lpRight: begin
          TextBounds := Rect(Left + 1,  Top + TextAs, Right - WidthOf(CheckRect)-2, Bottom - TextAs);
          Format     := Format or DT_RIGHT;
      end;
     end;
    end;
    if Enabled and Focused then begin
       DrawFocusRect(ClientRect);
    end;
    if not Enabled then begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    end
    else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    unLock;
  end;
end;

{ TDefineGroupBox }

constructor TDefineGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  FBackgropStartColor := DefaultColorStart;
  FBackgropStopColor  := DefaultColorStop;
  FBorderColor        := DefaultBorderColor;
  FBackgropOrien      := fdLeftToRight;
  FTransparent        := false;
  FStyleFace          := fsDefault;
  FBorder             := brFull;
  FAlignment          := stLeft;
  SetBounds(0, 0, 185, 105);
end;

procedure GetStyleGroupBox(Value:TAlignmentText; var Result:UINT);
begin
  case Value of
   stLeft   : result := DT_TOP or DT_LEFT   or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
   stRight  : result := DT_TOP or DT_RIGHT  or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
   stCenter : result := DT_TOP or DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  end;
end;

procedure TDefineGroupBox.Paint;
var
  memBitmap: TBitmap;
  borderRect, TextRect: TRect;
  textHeight, textWidth, TextLeft, TextRight: integer;
  Format: UINT;
begin
  borderRect := ClientRect;
  GetStyleGroupBox(FAlignment,Format);
  memBitmap := TBitmap.Create; // create memory-bitmap to draw flicker-free
  try
    memBitmap.Height := ClientRect.Bottom;
    memBitmap.Width  := ClientRect.Right;
    memBitmap.Canvas.Font := Self.Font;

    textHeight := memBitmap.Canvas.TextHeight(caption);
    textWidth  := memBitmap.Canvas.TextWidth(caption);

    TextRect := Rect(ClientRect.Left + 10, ClientRect.Top, ClientRect.Right - 10, ClientRect.Top + textHeight);
    // Draw Background
    if FTransparent then
       DrawParentImage(Self, memBitmap.Canvas)
    else begin
      if FStyleFace=fsDefault then begin
         memBitmap.Canvas.Brush.Color := Self.Color;
         memBitmap.Canvas.FillRect(ClientRect);
      end else
         //DrawBackdrop(memBitmap.Canvas,FBackgropStartColor,FBackgropStopColor,ClientRect,FBackgropOrien);
         GradientFillRect(memBitmap.Canvas,ClientRect,FBackgropStartColor,FBackgropStopColor,FBackgropOrien,60);
    end;

    case FAlignment of
     stLeft:
       begin
         TextLeft := ClientRect.left + 5;
         TextRight:= ClientRect.left + 12 + textWidth;
       end;
     stRight:begin
         TextLeft := ClientRect.Right - TextWidth - 15;
         TextRight:= ClientRect.Right - 8;
       end;
    else//stCenter:
         TextRight:= (RectWidth(ClientRect) + textWidth + 5) div 2;
         TextLeft := (RectWidth(ClientRect) - textWidth - 12) div 2;
    end;
    // Draw Border
    memBitmap.Canvas.Pen.Color := FBorderColor;
    case FBorder of
      brFull:
        begin
          memBitmap.Canvas.Polyline([Point(TextLeft, ClientRect.top +(textHeight div 2)),
            Point(ClientRect.left, ClientRect.top +(textHeight div 2)),
            Point(ClientRect.left, ClientRect.bottom-1), Point(ClientRect.right-1, ClientRect.bottom-1),
            Point(ClientRect.right-1, ClientRect.top +(textHeight div 2)),
            Point(TextRight, ClientRect.top +(textHeight div 2))]);
        end;
      brOnlyTopLine:
        begin
            memBitmap.Canvas.Polyline([Point(ClientRect.left + 5, ClientRect.top +(textHeight div 2)), Point(ClientRect.left, ClientRect.top +(Canvas.textHeight(caption) div 2))]);
            memBitmap.Canvas.Polyline([Point(ClientRect.right-1, ClientRect.top +(textHeight div 2)), Point(ClientRect.left + 12 + textWidth, ClientRect.top +(textHeight div 2))]);
        end;
    end;

    // Draw Text
    memBitmap.Canvas.Brush.Style := bsClear;
    if not Enabled then
    begin
      OffsetRect(TextRect, 1, 1);
      memBitmap.Canvas.Font.Color := clBtnHighlight;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextRect, Format);
      OffsetRect(TextRect, -1, -1);
      memBitmap.Canvas.Font.Color := clBtnShadow;
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextRect, Format);
    end
    else
      DrawText(memBitmap.Canvas.Handle, PChar(Caption), Length(Caption), TextRect, Format);

    // Copy memBitmap to screen
    Canvas.CopyRect(ClientRect, memBitmap.Canvas, ClientRect);
  finally
    memBitmap.free; // delete the bitmap
  end;
end;

procedure TDefineGroupBox.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TDefineGroupBox.SetColors(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0: FBorderColor        := Value;
    1: FBackgropStartColor := Value;
    2: FBackgropStopColor  := Value;
  end;
  Invalidate;
end;

procedure TDefineGroupBox.SetBorder(const Value: TGroupBoxBorder);
begin
  if FBorder <> Value then
  begin
     FBorder := Value;
     Invalidate;
  end;
end;

procedure TDefineGroupBox.SetFillDirect(const Value: TFillDirection);
begin
  if FBackgropOrien <> Value then begin
     FBackgropOrien := Value;
     Invalidate;
  end;
end;

procedure TDefineGroupBox.SetStyleFace(const Value: TStyleFace);
begin
  if FStyleFace <> Value then begin
     FStyleFace := Value;
     Invalidate;
  end;
end;

procedure TDefineGroupBox.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  //FTransParent := not ParentColor;
  if (Parent <> nil)and(ParentColor) then
  begin
      Color := TForm(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineGroupBox.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
      Color := TForm(Parent).Color;
  Invalidate;
end;

procedure TDefineGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus; 
      Result := 1;
    end;
end;

procedure TDefineGroupBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TDefineGroupBox.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     //ParentColor  := not Value;
     Invalidate;
  end;
end;

procedure TDefineGroupBox.WMMove(var Message: TWMMove);
begin
  inherited;
  if FTransparent then Invalidate;
end;

procedure TDefineGroupBox.WMSize(var Message: TWMSize);
begin
  inherited;
  if FTransparent then  Invalidate;
end;

procedure TDefineGroupBox.SetAlignment(const Value: TAlignmentText);
begin
 if FAlignment <> Value then
 begin
    FAlignment := Value;
    Invalidate;
 end;
end;

procedure TDefineGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  Inc(Rect.Top, Canvas.TextHeight('0'));
  InflateRect(Rect, -1, -1);
  if Ctl3d then InflateRect(Rect, -1, -1);
end;

{ TDefineListBox }

var
  ScrollTimer: TTimer = nil;

const
  FTimerInterval = 600;
  FScrollSpeed   = 100;

procedure DrawScrollBar(control:TControl; Focused:boolean; canvas: TCanvas; BarsRect: TBarsRect; Style: TFlatSkin;
                        FirstItem, MaxItems, ItemsCount: Integer; Enabled: Boolean);
var
 x, y: Integer;
 procedure DrawImage;
 begin
   with Style, BarsRect do begin
    if not BarUseBitmap then
    begin
     if UserFace = fsDefault then
     begin
        canvas.Brush.Color := BarColor;
        canvas.FillRect(prevRect);
        canvas.FillRect(downRect);
     end else begin
     DrawBackdrop(Canvas,BarStartColor,BarStopColor,prevRect,BarOrien);
     case Style.BarOrien of
         bsHorizontal:DrawBackdrop(Canvas,BarStartColor,BarStopColor,downRect,BarOrien);  //水平
         bsVertical  :DrawBackdrop(Canvas,BarStopColor,BarStartColor,downRect,BarOrien);  //垂直
     end;
     end;
    end else begin
     DrawBitmap(Canvas,prevRect,BarTopBitmap);
     DrawBitmap(Canvas,downRect,BarDownBitmap);
    end;
   end;
 end;
begin
  // 画滚动条背景
  with Style,BarsRect do begin
  case Transparent of
          tmAlways: DrawParentImage(control, Canvas);
            tmNone: DrawImage;
      tmNotFocused: if Focused then
                       DrawImage
                    else
                       DrawParentImage(control, Canvas);
  end;
  // 画滚动条边框
  canvas.Brush.Color := BorderColor;
  canvas.FrameRect(prevRect);
  canvas.FrameRect(downRect);

  // Draw the up arrow
  x := (prevRect.Right - prevRect.Left) div 2 - 6;
  y :=  prevRect.Top + 4;

  if (firstItem <> 0) and Enabled then
  begin
    canvas.Brush.Color := BarArrowColor;
    canvas.Pen.Color   := BarArrowColor;
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end
  else
  begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color   := clWhite;
    Inc(x); Inc(y);
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color   := clGray;
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end;

  // Draw the down arrow
  x := (downRect.Right - downRect.Left) div 2 - 6;
  y :=  downRect.Bottom - 7;
  if (firstItem + maxItems + 1 <= ItemsCount) and Enabled then
  begin
    canvas.Brush.Color := BarArrowColor;
    canvas.Pen.Color   := BarArrowColor;
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end
  else
  begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color   := clWhite;
    Inc(x); Inc(y);
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color   := clGray;
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end;
  end;
end;

function CurItemRect(CurPos:TPoint;CurRect:TRect;ItemHeight:integer):TRect;
begin
 result := Rect(CurPos.x, CurPos.y, CurRect.Right - 3, CurPos.y + ItemHeight);
end;

procedure CreateRects(List:TList;MaxItems,ItemHeight:integer;CurPos:TPoint;CurRect:TRect);
var
  ItemRect: ^TRect;
  inx:integer;
begin
  RemoveList(List);
  for inx := 0 to MaxItems - 1 do
  begin
    New(ItemRect);
    ItemRect^ := CurItemRect(CurPos,CurRect,ItemHeight);
    List.Add(ItemRect);
    CurPos    := Point(CurPos.x, CurPos.y + ItemHeight + 2);
  end;
end;

constructor TDefineListBox.Create(AOwner: TComponent);
begin
  if ScrollTimer = nil then begin
     ScrollTimer := TTimer.Create(nil);
     ScrollTimer.Enabled  := False;
     ScrollTimer.Interval := FTimerInterval;
  end;
  inherited Create(AOwner);
  ControlStyle    := ControlStyle + [csOpaque];
  SetBounds(0, 0, 140, 158);
  ParentColor     := True;
  ParentFont      := True;
  Enabled         := true;
  Visible         := true;
  TabStop         := True;
  FStyle          := TListStyle.Create;
  FStyle.Parent   := self;
  FStyle.OnChange := StyleChange;
  FItems          := TStringList.Create;
  //FItems          := TListBoxStrings.Create;
  //TListBoxStrings(FItems).ListBox := Self;
  FItems.OnChange := StyleChange;
  FRects          := TList.Create;
  FChecks         := TList.Create;
  FMultiSelect    := false;
  FSorted         := false;
  FirstItem       := 0;
  FItemIndex      := -1;
  FCaption        := '';
end;

destructor TDefineListBox.Destroy;
begin
  ScrollTimer.Free;
  ScrollTimer := nil;
  //释放 FRect
  RemoveList(FRects, lsFree);
  //释放 FChecks
  RemoveList(FChecks, lsFree);
  FItems.Free;
  FStyle.Free;
  inherited Destroy;
end;

procedure TDefineListBox.WMMouseWheel(var Message: TMessage);
var
  fScrollLines: Integer;
begin
  if not(csDesigning in ComponentState) then
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @fScrollLines, 0);

    if(fScrollLines = 0) then
       fScrollLines := MaxItems;

    if ShortInt(Message.WParamHi) = -WHEEL_DELTA then
      if FirstItem + MaxItems + fScrollLines <= FItems.Count then
         Inc(FirstItem, fScrollLines)
      else
        if FItems.Count - MaxItems < 0 then
           FirstItem := 0
        else
           FirstItem := FItems.Count - MaxItems
    else
      if ShortInt(Message.WParamHi) = WHEEL_DELTA then
        if FirstItem - fScrollLines < 0 then
           FirstItem := 0
        else
           dec(FirstItem, fScrollLines);
    Invalidate;
  end;
end;

function TDefineListBox.GetItemText: TCaption;
begin
  if IndexInCount(FItemIndex,FItems.Count) then
     result := FItems.Strings[FItemIndex]
  else
     result := '';
end;

function TDefineListBox.Find(Value: String; var Index: Integer): boolean;
begin
  result := false;
  index  := -1;
  while(index < Items.Count) and(not result) do begin
     inc(Index);
     if IndexInCount(Index,Items.Count) then
        result := Items.Strings[index]=Value;
  end;
end;

function TDefineListBox.FindChecked(Value:Integer; var index:integer):boolean;
var inx:integer;
    tmp:^Integer;
begin
  inx    := 0;
  result := false;
  while (inx < FChecks.Count)and(not result) do
  begin
    tmp := FChecks.Items[inx];
    result := Tmp^ = Value;
    if result then index := inx else index := -1;
    inc(inx);
  end;
end;

procedure TDefineListBox.AddCheck(Index:integer);
var inx:^Integer;
    x:integer;
begin
 if not FindChecked(index,x) then begin
    new(inx);
    inx^:=Index;
    FChecks.Add(inx);
 end;
end;

procedure TDefineListBox.DeleteChecked(Index:Integer);
begin
  Dispose(FChecks.Items[index]);
  FChecks.Delete(index);
end;

procedure TDefineListBox.Click;
begin
  inherited Click;
  if not Focused then SetFocus;
  if assigned(FOnClick) and IndexInCount(FItemIndex,FItems.Count) then begin
     FOnClick(self,FItems.Strings[FItemIndex]);
  end;
end;

procedure TDefineListBox.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted       := Value;
    FItems.Sorted := Value;
    Invalidate;
  end;
end;

procedure TDefineListBox.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TDefineListBox.SetItemsRect;
var
  CurPos: TPoint;
  curRect: TRect;
begin
  CurRect := ClientRect;
  with FStyle do begin
  if TitleHas then begin
    case TitlePosition of
      tsTop   : CurRect.Top    := CurRect.Top + TitleHeight;
      tsBottom: CurRect.Bottom := CurRect.Bottom - TitleHeight;
    end;
  end;
  // set left/top PosR for the the first item
  if ScrollBars then
     CurPos := Point(CurRect.left + 3, CurRect.top + 3 + BarsHeight)
  else
     CurPos := Point(CurRect.left + 3, CurRect.top + 3);

  // recreate all items-rect
  CreateRects(FRects,MaxItems,ItemHeight,CurPos,CurRect);
  end;
  Invalidate;
end;

function TDefineListBox.GetSelected(Index: Integer): Boolean;
begin
  Result := FindChecked(index, FItemIndex);
end;

procedure TDefineListBox.SetSelected(Index: Integer; Value: Boolean);
var inx:Integer;
begin
  if MultiSelect then
  begin
   if FindChecked(Index , inx) and Value then
      DeleteChecked(inx)
   else
      AddCheck(index);
  end else begin
      RemoveList(FChecks);
      FChecks.Clear;
  end;
  Invalidate;
end;

function TDefineListBox.GetSelCount: Integer;
begin
  if MultiSelect then
     Result := FChecks.Count
  else
     Result := -1;
end;

procedure TDefineListBox.Paint;
var
 memBitmap: TBitmap;
 inxRect, inxItem, CurIndex: Integer;
 itemRect: ^TRect;
 Format, TitleFormat: UINT;
 WorkRect, TitleRect:TRect;
 BarsRect: TBarsRect;
 curState: Boolean;
 procedure DrawImage(Canvas:TCanvas;Skin:TListStyle;WorkRect,TitleRect:TRect;TitleHas:Boolean);
 begin
  with Skin do begin
   //draw backgroud
   if not BackUseBitmap then
   begin
      if (Enabled)and(Focused or MouseIn) then
         BoxDrawBackDrop(Canvas,BackStartColor,BackStopColor,BackdropOrien,WorkRect,BackdropColor,UserFace)
      else
         BoxDrawBackDrop(Canvas,BackStartColor,BackStopColor,BackdropOrien,WorkRect,BackFocusColor,UserFace);
   end
   else
      DrawBitmap(Canvas,WorkRect,BackBitmap);
   //draw title backgroud
   if TitleHas then
   begin
     if not TitleUseBitmap then
        BoxDrawBackDrop(Canvas,TitleStartColor,TitleStopColor,TitleOrien,TitleRect,TitleColor,UserFace)
     else
        DrawBitmap(Canvas,TitleRect,TitleBitmap);
   end;
  end;
 end;
begin
  // create memory-bitmap to draw flicker-free
  memBitmap := TBitmap.Create;
  try
   memBitmap.Height := ClientRect.Bottom;
   memBitmap.Width  := ClientRect.Right;
   //控制区域
   WorkRect   := ClientRect;
   TitleRect  := ClientRect;
   with FStyle do begin
    if TitleHas then begin
      case TitlePosition of
          tsTop : begin
           WorkRect.Top     := WorkRect.Top  + TitleHeight;
           TitleRect.Bottom := TitleRect.Top + TitleHeight;
         end;
       tsBottom : begin
           WorkRect.Bottom  := WorkRect.Bottom  - TitleHeight;
           TitleRect.Top    := TitleRect.Bottom - TitleHeight;
         end;
      end;
    end;
    with BarsRect do begin
    if ScrollBars then begin
      prevRect := Rect(WorkRect.Left, WorkRect.Top, WorkRect.Right, WorkRect.Top + BarsHeight);
      downRect := Rect(WorkRect.Left, WorkRect.Bottom - BarsHeight, WorkRect.Right, WorkRect.Bottom);
      workRect := Rect(workRect.Left, workRect.Top + BarsHeight, workRect.Right, workRect.Bottom - BarsHeight);
    end;
    end;
    GetStyleText(ItemAlignment, Format);
    GetStyleText(TitleAlignment,TitleFormat);
    // Clear Background
    case Transparent of
          tmAlways: DrawParentImage(Self, memBitmap.Canvas);
            tmNone: DrawImage(memBitmap.Canvas,FStyle,WorkRect,TitleRect,TitleHas);
      tmNotFocused: if Focused then
                       DrawImage(memBitmap.Canvas,FStyle,WorkRect,TitleRect,TitleHas)
                    else
                       DrawParentImage(Self, memBitmap.Canvas);
    end;
    //Draw ScrollBars
    if ScrollBars then begin
       DrawScrollBar(self, Focused, memBitmap.Canvas, BarsRect, FStyle, FirstItem, MaxItems, FItems.Count, Enabled);
    end;
    // Draw Border
    memBitmap.Canvas.Brush.Color := BorderColor;
    memBitmap.Canvas.FrameRect(ClientRect);
    // Draw Focused Frame
    if(fItems.Count <=0)and(Focused) then
       DrawFocusRect(memBitmap.Canvas,WorkRect,ItemHeight);
    // draw titletext
    if TitleHas then begin
       MemBitmap.Canvas.Font.Assign(FStyle.TitleFont);
       FlatDrawText(memBitmap.Canvas, Enabled, FCaption, TitleRect, TitleFormat);
    end;
   end;
   // Initialize the counter for the Items
   memBitmap.Canvas.Font.Assign(Self.Font);
   inxItem := FirstItem;
   // Draw Items
   for inxRect := 0 to MaxItems - 1 do
    begin
      itemRect := FRects.Items[inxRect];
      if(inxItem <= FItems.Count - 1) then
      begin
        // Item is selected
        CurState := FindChecked(inxItem, CurIndex);
        with FStyle do begin
         // Draw ItemBorder
         if ItemLineHas then
         begin
            memBitmap.Canvas.Brush.color := ItemLineColor;
            memBitmap.Canvas.FrameRect(itemRect^);
         end;
         if inxItem = FItemIndex then
         begin
          // Fill ItemRect
          BoxDrawBackDrop(memBitmap.Canvas,ItemStartColor,ItemStopColor,ItemOrien, itemRect^, ItemSelectColor,UserFace);
          if Focused and (not MultiSelect) then
             DrawFocusRect(memBitmap.Canvas,itemRect^,ItemHeight);
          memBitmap.Canvas.Brush.color := ItemFrameColor;
          memBitmap.Canvas.FrameRect(itemRect^);
         end else if CurState then begin
          BoxDrawBackDrop(memBitmap.Canvas,ItemStartColor,ItemStopColor,bsVertical, itemRect^, ItemSelectColor,UserFace);
         end; 
        end;
        // Draw ItemText
        FlatDrawText(memBitmap.Canvas, Enabled, FItems[inxItem], itemRect^, Format);
        // draw next Item
        Inc(inxItem);
      end;
    end;
    // Copy bitmap to screen
    Canvas.CopyRect(ClientRect, memBitmap.Canvas, ClientRect);
  finally
    // delete the memory bitmap
    memBitmap.free;
  end;
end;

procedure TDefineListBox.SelectNotifyEvent;
begin
  if assigned(FOnChange) and IndexInCount(FItemIndex,FItems.Count) then FOnChange(self,FItems.Strings[FItemIndex]);
  if assigned(FOnClick) and IndexInCount(FItemIndex,FItems.Count) then FOnClick(self,FItems.Strings[FItemIndex]);
end;

procedure TDefineListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  curPos: TPoint;
  inxRect: Integer;
  curRect: ^TRect;
  BarsRect: TBarsRect;
begin
  GetCursorPos(curPos);
  curPos := ScreenToClient(curPos);
  with FStyle do
  begin
  if(FItems.Count > 0) and(Button = mbLeft) then
  begin
    for inxRect := 0 to FRects.Count - 1 do
    begin
      curRect := FRects.Items[inxRect];
      if PtInRect(curRect^, curPos) then
      begin
       FItemIndex := FirstItem + inxRect;
       SetSelected(FItemIndex,True);
       SetFocus;
       Invalidate;
       Exit;
      end;
    end;
  end;

  if ScrollBars then
  begin
    GetBarPosition(ClientRect,TitleHas,TitlePosition,BarsRect,TitleHeight,BarsHeight);
    if PtInRect(BarsRect.prevRect, curPos) then
    begin
      if (FirstItem - 1) < 0 then
        FirstItem := 0
      else
        Dec(FirstItem);
      SetFocus;
      Invalidate;
      scrollType := stUp;
      if ScrollTimer.Enabled then
         ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
    if PtInRect(BarsRect.downRect, curPos) then
    begin
      if FirstItem + MaxItems + 1 <= FItems.Count then
         Inc(FirstItem);
      SetFocus;
      Invalidate;
      scrollType := stDown;
      if ScrollTimer.Enabled then
         ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
  end;
  end;
  Inherited;
end;

procedure TDefineListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ScrollTimer.Enabled  := False;
  ScrollTimer.Interval := FTimerInterval;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TDefineListBox.ScrollTimerHandler(Sender: TObject);
begin
  ScrollTimer.Interval := FScrollSpeed;
  if scrollType = stUp then
    if(FirstItem - 1) < 0 then
    begin
      FirstItem := 0;
      ScrollTimer.Enabled := False;
    end
    else
      Dec(FirstItem)
  else
    if FirstItem + MaxItems + 1 <= FItems.Count then
      Inc(FirstItem)
    else
      ScrollTimer.Enabled := False;
  Invalidate;
end;

procedure TDefineListBox.Loaded;
begin
  inherited;
  SetItemsRect;
end;

procedure TDefineListBox.WMSize(var Message: TWMSize);
var y,inx:integer;
begin
  inherited;
  with FStyle do begin
  y := 2;
  for inx := 1 to MaxItems do
      y := y +(ItemHeight + 2);
  y := y + 2;
  if ScrollBars then
     y := y + BarsHeight * 2;
  if TitleHas then
     y := y + TitleHeight;
  if not(csLoading in ComponentState) then
     SetBounds(Left,Top,Width,y);
  end;
  // Recalculate the itemRects
  SetItemsRect;
end;

procedure TDefineListBox.WMMove(var Message: TWMMove);
begin
  inherited;
  if not(FStyle.Transparent = tmNone) then
    Invalidate;
end;

procedure TDefineListBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FMouseIn := False;
  if IndexInCount(FItemIndex, FItems.Count) then
     SetSelected(FItemIndex,False);
  Invalidate;
end;

procedure TDefineListBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if FItemIndex >= 0 then
     SetSelected(FItemIndex,True)
  else if FItems.Count > 0 then begin
     FItemIndex := 0;
     SetSelected(FItemIndex,True);
  end;
  Invalidate;
end;

procedure TDefineListBox.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_UP: begin
       if(FirstItem - 1) < 0 then
          FirstItem := 0
       else
          Dec(FirstItem);
       if FItems.Count > 0 then begin
        if FItemIndex > 0 then
           Dec(FItemIndex)
        else
           FItemIndex := 0;
        //SetSelected(FItemIndex,True);
        SelectNotifyEvent;
       end;
      end;
    VK_DOWN:begin
      if FirstItem + MaxItems + 1 <= FItems.Count then
         Inc(FirstItem);
         
      if FItems.Count > 0 then begin
       if FItemIndex < FItems.Count-1 then
          Inc(FItemIndex)
       else
          FItemIndex := FItems.Count-1;
       //SetSelected(FItemIndex,True);
       SelectNotifyEvent;
      end;
      end;
    VK_PRIOR:
      if(FirstItem - MaxItems) < 0 then
        FirstItem := 0
      else
        Dec(FirstItem, MaxItems);
    VK_NEXT:
      if FirstItem +(MaxItems * 2) <= FItems.Count then
        Inc(FirstItem, MaxItems)
      else
        FirstItem := FItems.Count - MaxItems;
    VK_SPACE: begin
      SetSelected(FItemIndex,True);
      SelectNotifyEvent;
      end;
  else
    inherited;
  end;
  Invalidate;
end;

function TDefineListBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TDefineListBox.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
  begin
    FItemIndex := Value;
    Invalidate;
  end;
end;

procedure TDefineListBox.SetMultiSelect(Value: Boolean);
begin
  FMultiSelect := Value;
  if Value then
     FItemIndex := 0;
end;

procedure TDefineListBox.SetName(const Value: TComponentName);
begin
  if(csDesigning in ComponentState) and((Length(FCaption) = 0) or
    (CompareText(FCaption, Name) = 0)) then
    FCaption   := Value;
  inherited SetName(Value);
end;

procedure TDefineListBox.SetListStyle(const Value: TListStyle);
begin
 FStyle.Assign(Value);
end;

procedure TDefineListBox.StyleChange(Sender: TObject);
begin
 SetItemsRect;
 Invalidate;
end;

function TDefineListBox.GetMaxItems: Integer;
begin
  result := ClientRect.Bottom - ClientRect.Top;
  with FStyle do begin
  if TitleHas then
     result := result - TitleHeight;
  if ScrollBars then
     result := result - BarsHeight * 2;
  result :=(result - 4) div(ItemHeight + 2);
  end;
end;

procedure TDefineListBox.SetCaption(const Value: TCaption);
begin
 if FCaption <> Value then
 begin
    FCaption := Value;
    Invalidate;
 end;
end;

procedure TDefineListBox.WMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TDefineListBox.Clear;
begin
  RemoveList(FChecks);
  RemoveList(FRects);
  FItems.Clear;
end;

procedure TDefineListBox.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont and Assigned(FStyle) then
  begin
     if FStyle.ParentFont then
        FStyle.TitleFont.Assign(Font);
  end;
end;

procedure TDefineListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FStyle) then
  begin
     if FStyle.ParentFont then
        FStyle.TitleFont.Assign(Font);
  end;
end;

function TDefineListBox.GetItemCount: Integer;
begin
  result := Items.Count;
end;

procedure TDefineListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) and
        (GetActiveWindow <> 0) and (not MouseIn) then
  begin
    FMouseIn := True;
    Invalidate;
  end;  
end;

procedure TDefineListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := false;
     Invalidate;
  end;
end;

function TDefineListBox.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefineListChecks }

constructor TDefineListChecks.Create(AOwner: TComponent);
begin
  if ScrollTimer = nil then begin
     ScrollTimer := TTimer.Create(nil);
     ScrollTimer.Enabled  := False;
     ScrollTimer.Interval := FTimerInterval;
  end;
  inherited Create(AOwner);
  ControlStyle    := ControlStyle + [csOpaque];
  SetBounds(0, 0, 140, 158);
  ParentColor     := True;
  ParentFont      := True;
  Enabled         := true;
  TabStop         := True;
  Visible         := true;
  FStyle          := TCheckStyle.Create;
  FStyle.Parent   := self;
  FStyle.OnChange := StyleChange;
  FItems          := TStringList.Create;
  FItems.OnChange := StyleChange;
  FRects          := TList.Create;
  FChecks         := TList.Create;
  FSorted         := false;
  FSelected       := -1;
  FirstItem       := 0;
  FCaption        := '';
end;

destructor TDefineListChecks.Destroy;
begin
  ScrollTimer.Free;
  ScrollTimer := nil;
  //释放 FRect
  RemoveList(FRects, lsFree);
  //释放 FChecks
  RemoveList(FChecks, lsFree);
  FItems.Free;
  FStyle.Free;
  inherited Destroy;
end;

procedure TDefineListChecks.WMMouseWheel(var Message: TMessage);
var
  fScrollLines: Integer;
begin
  if not(csDesigning in ComponentState) then
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @fScrollLines, 0);

    if(fScrollLines = 0) then
      fScrollLines := MaxItems;

    if ShortInt(Message.WParamHi) = -WHEEL_DELTA then
      if FirstItem + MaxItems + fScrollLines <= FItems.Count then
        Inc(FirstItem, fScrollLines)
      else
        if FItems.Count - MaxItems < 0 then
          FirstItem := 0
        else
          FirstItem := FItems.Count - MaxItems
    else
      if ShortInt(Message.WParamHi) = WHEEL_DELTA then
        if FirstItem - fScrollLines < 0 then
          FirstItem := 0
        else
          dec(FirstItem, fScrollLines);
    Invalidate;
  end;
end;

procedure TDefineListChecks.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted       := Value;
    FItems.Sorted := Value;
    Invalidate;
  end;
end;

procedure TDefineListChecks.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TDefineListChecks.SetItemsRect;
var
  CurPos: TPoint;
  CurRect:TRect;
begin
  CurRect := ClientRect;
  with FStyle do begin
  if TitleHas then begin
    case TitlePosition of
      tsTop   : CurRect.Top    := CurRect.Top + TitleHeight;
      tsBottom: CurRect.Bottom := CurRect.Bottom - TitleHeight;
    end;
  end;
  // set left/top PosR for the the first item
  if ScrollBars then
     CurPos := Point(CurRect.left + 3, CurRect.top + 3 + BarsHeight)
  else
     CurPos := Point(CurRect.left + 3, CurRect.top + 3);
  // Recreate all Item - Rects
  CreateRects(FRects,MaxItems,ItemHeight,CurPos,CurRect);
  end;
  Invalidate;
end;

function TDefineListChecks.GetChecked(Index: Integer): Boolean;
begin
  Result := FindChecked(index, FSelected);
end;

procedure TDefineListChecks.SetChecked(Index: Integer; Value: Boolean);
var inx:integer;
begin
  if FindChecked(Index,inx) and Value then
     DeleteChecked(inx)
  else begin
     AddCheck(index);
  end;
  Invalidate;
end;

function TDefineListChecks.GetSelCount: Integer;
begin
  result := FChecks.Count;
end;

procedure TDefineListChecks.DrawCheckRect(Canvas: TCanvas; StartRect: TRect; checked: Boolean);
var
  CheckBox: TRect;
begin
  DrawCheckBox(StartRect,FStyle.SelectPosition,FStyle.SelectSize,CheckBox);
  with Canvas do begin
   Pen.style := psSolid;
   Pen.width := 1;
   // 画背景
   Brush.color := FStyle.BackdropColor;

   FillRect(Checkbox);
   // 画选定
   if Checked then
   begin
    DrawInCheck(Canvas, CheckBox, FStyle.BorderColor);
   end;
   // 画边框
   Brush.color := FStyle.BorderColor;
   FrameRect(Checkbox);
  end;
end;

procedure TDefineListChecks.Paint;
var
  memBitmap: TBitmap;
  inxRect, inxItem: Integer;
  itemRect: ^TRect;
  Format, TitleFormat: UINT;
  WorkRect, TitleRect:TRect;
  BarsRect: TBarsRect;
  curIndex: integer;
  curState: boolean;
 procedure DrawImage(Canvas:TCanvas;Skin:TCheckStyle;WorkRect,TitleRect:TRect;TitleHas:Boolean);
 begin
  with Skin do begin
   //draw backgroud
   if not BackUseBitmap then
   begin
      if (Enabled)and(Focused or MouseIn) then
         BoxDrawBackDrop(Canvas,BackStartColor,BackStopColor,BackdropOrien,WorkRect,BackdropColor,UserFace)
      else
         BoxDrawBackDrop(Canvas,BackStartColor,BackStopColor,BackdropOrien,WorkRect,BackFocusColor,UserFace);
   end
   else
      DrawBitmap(Canvas,WorkRect,BackBitmap);
   //draw title backgroud
   if TitleHas then
   begin
     if not TitleUseBitmap then
        BoxDrawBackDrop(Canvas,TitleStartColor,TitleStopColor,TitleOrien,TitleRect,TitleColor,UserFace)
     else
        DrawBitmap(Canvas,TitleRect,TitleBitmap);
   end;
  end;
 end;
begin
  // create memory-bitmap to draw flicker-free
  memBitmap := TBitmap.Create;
  try
   memBitmap.Height := ClientRect.Bottom;
   memBitmap.Width  := ClientRect.Right;
   //控制区域
   WorkRect   := ClientRect;
   TitleRect  := ClientRect;
   with FStyle do begin
    if TitleHas then begin
      case TitlePosition of
          tsTop : begin
           WorkRect.Top     := WorkRect.Top  + TitleHeight;
           TitleRect.Bottom := TitleRect.Top + TitleHeight;
         end;
       tsBottom : begin
           WorkRect.Bottom  := WorkRect.Bottom  - TitleHeight;
           TitleRect.Top    := TitleRect.Bottom - TitleHeight;
         end;
      end;
    end;
    with BarsRect do begin
    if ScrollBars then begin
      prevRect := Rect(WorkRect.Left, WorkRect.Top, WorkRect.Right, WorkRect.Top + BarsHeight);
      downRect := Rect(WorkRect.Left, WorkRect.Bottom - BarsHeight, WorkRect.Right, WorkRect.Bottom);
      workRect := Rect(workRect.Left, workRect.Top + BarsHeight, workRect.Right, workRect.Bottom - BarsHeight);
    end;
    end;
    //设置样式
    GetStyleText(TitleAlignment, TitleFormat);
    GetCheckBoxPosition(SelectPosition, Format);
    // Clear Background
    case Transparent of
          tmAlways: DrawParentImage(Self, memBitmap.Canvas);
            tmNone: DrawImage(memBitmap.Canvas,FStyle,WorkRect,TitleRect,TitleHas);
      tmNotFocused: if Focused then
                       DrawImage(memBitmap.Canvas,FStyle,WorkRect,TitleRect,TitleHas)
                    else
                       DrawParentImage(Self, memBitmap.Canvas);
    end;
    // Draw ScrollBars
    if ScrollBars then begin
       DrawScrollBar(self, Focused, memBitmap.Canvas, BarsRect, FStyle, FirstItem, MaxItems, FItems.Count, Enabled);
    end;
    // Draw Border
    memBitmap.Canvas.Brush.Color := BorderColor;
    memBitmap.Canvas.FrameRect(ClientRect);
    // Draw Focused Frame
    if(fItems.Count <=0)and(Focused) then
       DrawFocusRect(memBitmap.Canvas,WorkRect,ItemHeight);
    // draw titletext
    if TitleHas then begin
       MemBitmap.Canvas.Font.Assign(FStyle.TitleFont);
       FlatDrawText(memBitmap.Canvas, Enabled, FCaption, TitleRect, TitleFormat);
    end;
   end;
   // Initialize the counter for the Items
   memBitmap.Canvas.Font.Assign(Self.Font);
   inxItem := FirstItem;
   // Draw Items
   for inxRect := 0 to MaxItems - 1 do
    begin
      itemRect := FRects.Items[inxRect];
      if(inxItem <= FItems.Count - 1) then
      begin
        CurState := FindChecked(inxItem, CurIndex);
        // Item is selected
        with FStyle do begin
          // Draw ItemBorder
          if ItemLineHas then begin
             memBitmap.Canvas.Brush.color := ItemLineColor;
             memBitmap.Canvas.FrameRect(itemRect^);
          end;
          if inxItem = FSelected then begin
             // Fill ItemRect
             BoxDrawBackDrop(memBitmap.Canvas,ItemStartColor,ItemStopColor,ItemOrien,itemRect^, ItemSelectColor,UserFace);
             // draw focused rect
             if Focused then DrawFocusRect(memBitmap.Canvas,itemRect^,ItemHeight);
             // Draw selected ItemBorder
             memBitmap.Canvas.Brush.color := ItemFrameColor;
             memBitmap.Canvas.FrameRect(itemRect^);
          end else if CurState then begin
             BoxDrawBackDrop(memBitmap.Canvas,SelectStartColor, SelectStopColor,SelectOrien, itemRect^, SelectCheckColor,UserFace);
          end;
        // Draw select box
        DrawCheckRect(memBitmap.Canvas, itemRect^, CurState);
        // Draw ItemText
        case SelectPosition of
          bpLeft : begin
             itemRect^.Left  := itemRect^.Left + SelectSize + 3;//16;
             FlatDrawText(memBitmap.Canvas, Enabled, FItems[inxItem], itemRect^, Format);
             itemRect^.Left  := itemRect^.Left - SelectSize - 3;//16;
           end;
         bpRight : begin
             itemRect^.Right := itemRect^.Right - SelectSize - 1;// 14;
             FlatDrawText(memBitmap.Canvas, Enabled, FItems[inxItem], itemRect^, Format);
             itemRect^.Right := itemRect^.Right + SelectSize + 1;//14;
           end;
        end;
        end;
        //end draw itemtext
        Inc(inxItem);
      end;
     end;
    // Copy bitmap to screen
    Canvas.CopyRect(ClientRect, memBitmap.Canvas, ClientRect);
  finally
    // delete the memory bitmap
    memBitmap.free;
  end;
end;

function TDefineListChecks.FindChecked(Value:Integer; var index:integer):boolean;
var inx:integer;
    tmp:^Integer;
begin
  inx    := 0;
  result := false;
  while (inx < FChecks.Count)and(not result) do
  begin
    tmp := FChecks.Items[inx];
    result := Tmp^ = Value;
    if result then index := inx else index := -1;
    inc(inx);
  end;
end;

procedure TDefineListChecks.AddCheck(Index:integer);
var inx:^Integer;
    x:integer;
begin
  if not FindChecked(index,x) then begin
     new(inx);
     inx^:=Index;
     FChecks.Add(inx);
  end;
end;

procedure TDefineListChecks.DeleteChecked(Index:Integer);
begin
  dispose(FChecks.Items[index]);
  FChecks.Delete(index);
end;

procedure TDefineListChecks.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  curPos: TPoint;
  inxRect,index: Integer;
  curRect: ^TRect;
  checkRect: TRect;
  BarsRect: TBarsRect;
begin
  GetCursorPos(curPos);
  curPos := ScreenToClient(curPos);
  with FStyle do begin
  if(FItems.Count > 0) and(Button = mbLeft) then
  begin
    for inxRect := 0 to FRects.Count - 1 do
    begin
      curRect := FRects.Items[inxRect];
      //获取点击区域
      DrawCheckBox(curRect^, SelectPosition, SelectSize, checkRect);
      //选中状态
      if PtInRect(checkRect, curPos) then
      begin
        if FindChecked(FirstItem + inxRect, index) then
           DeleteChecked(index)
        else
           AddCheck(FirstItem + inxRect);
        SetFocus;
        if Assigned(FOnClickCheck) then
           FOnClickCheck(Self);
        Invalidate;
        Exit;
      end else if PtInRect(curRect^, curPos) then begin
        FSelected := FirstItem + inxRect;
        SetFocus;
        Invalidate;
        Exit;
      end;
    end;
  end;

  if ScrollBars then
  begin
    GetBarPosition(ClientRect,TitleHas,TitlePosition,BarsRect,TitleHeight,BarsHeight);
    if PtInRect(BarsRect.prevRect, curPos) then
    begin
      if(FirstItem - 1) < 0 then
         FirstItem := 0
      else
         Dec(FirstItem);
      SetFocus;
      Invalidate;
      scrollType := stUp;
      if ScrollTimer.Enabled then
        ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
    if PtInRect(BarsRect.downRect, curPos) then
    begin
      if FirstItem + MaxItems + 1 <= FItems.Count then
        Inc(FirstItem);
      SetFocus;
      Invalidate;
      scrollType := stDown;
      if ScrollTimer.Enabled then
        ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
  end;
  end;
  Inherited;
end;

procedure TDefineListChecks.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ScrollTimer.Enabled := False;
  ScrollTimer.Interval := FTimerInterval;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TDefineListChecks.ScrollTimerHandler(Sender: TObject);
begin
  ScrollTimer.Interval := FScrollSpeed;
  if scrollType = stUp then
    if(FirstItem - 1) < 0 then
    begin
      FirstItem := 0;
      ScrollTimer.Enabled := False;
    end
    else
      Dec(FirstItem)
  else
    if FirstItem + MaxItems + 1 <= FItems.Count then
      Inc(FirstItem)
    else
      ScrollTimer.Enabled := False;
  Invalidate;
end;

procedure TDefineListChecks.Loaded;
begin
  inherited;
  SetItemsRect;
end;

procedure TDefineListChecks.WMSize(var Message: TWMSize);
var y,inx:integer;
begin
  inherited;
  with FStyle do begin
  //reset clientrect size
  y := 2;
  for inx := 1 to MaxItems do
      y := y +(ItemHeight + 2);
  y := y + 2;
  if ScrollBars then
     y := y + BarsHeight * 2;
  if TitleHas then
     y := y + TitleHeight;
  if not(csLoading in ComponentState) then
     SetBounds(Left,Top,Width,y);
  end;
  // Recalculate the itemRects
  SetItemsRect;
end;

procedure TDefineListChecks.WMMove(var Message: TWMMove);
begin
  inherited;
  if not(FStyle.Transparent = tmNone) then
     Invalidate;
end;

procedure TDefineListChecks.Clear;
begin
  RemoveList(FChecks);
  RemoveList(FRects);
  FItems.Clear;
  FSelected := -1;
  Invalidate;
end;

procedure TDefineListChecks.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FCurSelected := FSelected;
  FSelected    := -1;
  FMouseIn     := False;
  Invalidate;
end;

procedure TDefineListChecks.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FSelected := FCurSelected;
  Invalidate;
end;

procedure TDefineListChecks.SelectNotifyEvent;
begin
  if assigned(FOnChange) and IndexInCount(FSelected,FItems.Count) then FOnChange(self,FItems.Strings[FSelected]);
  if assigned(FOnClick) and IndexInCount(FSelected,FItems.Count) then FOnClick(self,FItems.Strings[FSelected]);
end;

procedure TDefineListChecks.WMKeyDown(var Message: TWMKeyDown);
var index:Integer;
begin
  case Message.CharCode of
    VK_UP: begin
       if (FirstItem - 1) < 0 then
           FirstItem := 0
       else
           Dec(FirstItem);
       if FItems.Count > 0 then begin
        if FSelected > 0 then
           Dec(FSelected)
        else
           FSelected := 0;
        SelectNotifyEvent;
       end;
      end;
    VK_DOWN:begin
      if FirstItem + MaxItems + 1 <= FItems.Count then
         Inc(FirstItem);
      if FItems.Count > 0 then begin
       if FSelected  < FItems.Count - 1 then
          Inc(FSelected)
       else
          FSelected := FItems.Count - 1;
       SelectNotifyEvent;
      end;
      end;
    VK_PRIOR:
      if (FirstItem - MaxItems) < 0 then
          FirstItem := 0
      else
          Dec(FirstItem, MaxItems);
    VK_NEXT:
      if FirstItem +(MaxItems * 2) <= FItems.Count then
         Inc(FirstItem, MaxItems)
      else
         FirstItem := FItems.Count - MaxItems;
    VK_SPACE: begin
      if FindChecked(FSelected, Index) then
         DeleteChecked(Index)
      else
         AddCheck(FSelected);
      SelectNotifyEvent;
      end;
  else
    inherited;
  end;
  Invalidate;
end;

function TDefineListChecks.GetItemIndex: Integer;
begin
  Result := FSelected;
end;

procedure TDefineListChecks.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

procedure TDefineListChecks.SetName(const Value: TComponentName);
begin
  if(csDesigning in ComponentState) and((Length(FCaption) = 0) or
    (CompareText(FCaption, Name) = 0)) then
    FCaption   := Value;
  inherited SetName(Value);
end;

function TDefineListChecks.GetItemText: TCaption;
begin
  if IndexInCount(FSelected,FItems.Count) then
     result := FItems.Strings[FSelected]
  else
     result := '';
end;

function TDefineListChecks.Find(Value: String;  var Index: Integer): boolean;
begin
  result := false;
  index  := -1;
  while(index < Items.Count) and(not result) do begin
     inc(Index);
     if IndexInCount(Index,Items.Count) then
        result := UpperCase(Items.Strings[index])=UpperCase(Value);
  end;
end;

procedure TDefineListChecks.Click;
begin
  inherited Click;
  if not Focused then SetFocus;
  if assigned(FOnClick) and IndexInCount(FSelected,FItems.Count) then begin
     FOnClick(self,FItems.Strings[FSelected]);
  end;
end;

procedure TDefineListChecks.CheckAll;
var inx:Integer;
begin
  if FItems.Count > 0 then begin
     RemoveList(FChecks);
     for inx := 0 to FItems.Count - 1 do
         AddCheck(inx);
  end;
  SelectNotifyEvent;
end;

procedure TDefineListChecks.CheckCancel;
begin
  RemoveList(FChecks);
  SelectNotifyEvent;
end;

procedure TDefineListChecks.SetCheckStyle(const Value: TCheckStyle);
begin
  FStyle.Assign(Value);
end;

procedure TDefineListChecks.StyleChange(Sender: TObject);
begin
  SetItemsRect;
  Invalidate;
end;

function TDefineListChecks.GetMaxItems: Integer;
begin
  result:=ClientRect.Bottom - ClientRect.Top;
  with FStyle do begin
  if TitleHas then
     result := result - TitleHeight;
  if ScrollBars then
     result := result - BarsHeight * 2;
  result :=(result - 4) div(ItemHeight + 2);
  end;
end;

procedure TDefineListChecks.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if(ssCtrl in Shift)and Focused then begin
    case key of
     vk_selall   :CheckAll;
     vk_selcancel:CheckCancel;
    end;
  end;
end;

procedure TDefineListChecks.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then begin
     FCaption := Value;
     Invalidate;
  end;
end;

procedure TDefineListChecks.WMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TDefineListChecks.Delete(Index:Integer);
var inx:integer;
begin
  if IndexInCount(index,FItems.Count) then
  begin
     if FindChecked(index,inx) then
        DeleteChecked(inx);
     FItems.Delete(index);
  end;
end;

procedure TDefineListChecks.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FStyle) then
  begin
     if FStyle.ParentFont then
        FStyle.TitleFont.Assign(Font);
  end;
end;

procedure TDefineListChecks.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont and Assigned(FStyle) then
  begin
     if FStyle.ParentFont then
        FStyle.TitleFont.Assign(Font);
  end;
end;

function TDefineListChecks.GetItemCount: Integer;
begin
  result := Items.Count;
end;

{ TDefineGroupButton }

type
  TDefineGroupButton = class(TDefineRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TDefineRadioGroup);
    destructor Destroy; override;
  end;

constructor TDefineGroupButton.InternalCreate(RadioGroup: TDefineRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
end;

destructor TDefineGroupButton.Destroy;
begin
  TDefineRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TDefineGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TDefineRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TDefineGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TDefineRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TDefineRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TDefineGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TDefineRadioGroup(Parent).KeyDown(Key, Shift);
end;

procedure TDefineListChecks.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) and
        (GetActiveWindow <> 0) and (not MouseIn) then
  begin
    FMouseIn := True;
    Invalidate;
  end;  
end;

procedure TDefineListChecks.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := false;
     Invalidate;
  end;
end;

function TDefineListChecks.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefineRadioGroup }

constructor TDefineRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks, csParentBackground];
  FButtons     := TList.Create;
  FItems       := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex   := -1;
  FColumns     := 1;
  FTopOffset   := 5;
end;

destructor TDefineRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TDefineRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC); 
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth   := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin    := Metrics.tmHeight + FTopOffset + (I mod ButtonsPerCol) div 2;
    DeferHandle  := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TDefineGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
             ALeft := Self.ClientWidth - ALeft - Width;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            Width, Height,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TDefineRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TDefineRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TDefineRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TDefineRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TDefineRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do TDefineGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do TDefineGroupButton(FButtons.Last).Free;
end;

procedure TDefineRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TDefineRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
         TDefineGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
         TDefineGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TDefineRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TDefineRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TDefineGroupButton(FButtons[I]).Caption := FItems[I];
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TDefineGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TDefineRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TDefineGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TDefineRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TDefineRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TDefineRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

function TDefineRadioGroup.GetButtons(Index: Integer): TDefineRadioButton;
begin
  Result := TDefineRadioButton(FButtons[Index]);
end;

procedure TDefineRadioGroup.SetStyleFace(const Value: TStyleFace);
begin
  inherited;
  FTransparent := (FStyleFace <> fsCustom) and (not ParentColor);
end;

procedure TDefineRadioGroup.SetOffset(const Value: Integer);
begin
  if FTopOffset <> Value then begin
   FTopOffset := Value;
   ArrangeButtons;
   Invalidate;
  end;
end;

{ TDefineRadioButton }

constructor TDefineRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle  := [csSetCaption, csDoubleClicks];
  ParentColor   := False;
  ParentFont    := True;
  Enabled       := True;
  Visible       := True;
  Color         := DefaultFlatColor;
  FFocusedColor := DefaultBackdropColor;
  FDownColor    := DefaultBarColor;
  FCheckedColor := DefaultCheckColor;
  FBorderColor  := DefaultBorderColor;
  FLayout       := lpLeft;
  FChecked      := false;
  FGroupIndex   := 0;
  FTransparent  := True;
  SetBounds(0, 0, 121, 15);
end;

procedure TDefineRadioButton.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FDownColor    := Value;
    2: FCheckedColor   := Value;
    3: FBorderColor  := Value;
  end;
  Invalidate;
end;

procedure TDefineRadioButton.SetLayout(Value: TLayoutPosition);
begin
 if FLayout <> Value then
 begin
    FLayout := Value;
    //AdjustBounds;
    Invalidate;
 end;
end;

procedure TDefineRadioButton.SetChecked(Value: Boolean);
{var
  I: Integer;
  Sibling: TDefineRadioButton;
begin
  if FChecked <> Value then
  begin
    TabStop  := Value;
    FChecked := Value;
    if Value then
    begin
      if Parent <> nil then
        for i := 0 to Parent.ControlCount-1 do
          if Parent.Controls[i] is TDefineRadioButton then
          begin
            Sibling := TDefineRadioButton(Parent.Controls[i]);
            if (Sibling <> Self) and (Sibling.GroupIndex = GroupIndex) then
            with TDefineRadioButton(Sibling) do
            begin
              if Assigned(Action) and (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                 TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
          end;
      Click;
      if csDesigning in ComponentState then
        if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
            GetParentForm(self).Designer.Modified;
    end;
    invalidate;
  end;   }
  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TDefineRadioButton;
  begin
   if Parent <> nil then
        for i := 0 to Parent.ControlCount-1 do
          if Parent.Controls[i] is TDefineRadioButton then
          begin
            Sibling := TDefineRadioButton(Parent.Controls[i]);
            if (Sibling <> Self) and (Sibling.GroupIndex = GroupIndex) then
            with TDefineRadioButton(Sibling) do
            begin
              if Assigned(Action) and (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                 TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
          end;
      Click;
      if csDesigning in ComponentState then
        if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
            GetParentForm(self).Designer.Modified;
  end;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    if Value then
    begin
      TurnSiblingsOff;
      inherited Changed;
      if Enabled then Click;
    end;
    invalidate;
  end;         
end;

procedure TDefineRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseIn    := False;
    FMouseDown := False;
  end;
  Invalidate;
end;

procedure TDefineRadioButton.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TDefineRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end
    else
      inherited;
end;
{
procedure TDefineRadioButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Click;
  showbox('kasfj');
end;
}
procedure TDefineRadioButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Enabled then
  begin
    FFocused := True;
    FMouseIn := True;
    if not FChecked then
       SetChecked(True);
  end;
  invalidate;
end;

procedure TDefineRadioButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Enabled then
  begin
    FMouseIn := False;
    FFocused := False;
  end;
  invalidate;
end;
        
procedure TDefineRadioButton.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
  begin
      Color := TDefineRadioButton(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineRadioButton.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  if (Parent <> nil)and(ParentColor) then
  begin
      Color := TDefineRadioButton(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineRadioButton.DoEnter;
begin
  inherited DoEnter;
  if FMouseDown and MouseIn then
     FChecked := True;
  FFocused := True;
  invalidate;
end;

procedure TDefineRadioButton.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  FMouseIn := False;
  invalidate;
end;
{
procedure TDefineRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft) and Enabled then
  begin
    SetFocus;
    FMouseDown := true;
    invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDefineRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft) and Enabled then
  begin
    FMouseDown := false;
    if (X>=0) and (X<=Width) and (Y>=0) and (Y<=Height) and not Checked then
        Checked := True;
    invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;
}
procedure TDefineRadioButton.Paint;
var
  TextBounds, RadioRect, SelectRect: TRect;
  Format: UINT;
  TextAs:Integer;
begin
  with Canvas do
  begin
    Lock;
    Font.Assign(self.Font);
    if Layout = lpLeft then
       Width := TextWidth(DelCapLink(Caption))+TextHeight('H')+5;
    Height := TextHeight('H')+2;
    if FTransparent then
       DrawParentImage(Self, Canvas)
    else
    begin
       Brush.Color := self.Color;
       FillRect(ClientRect);
    end;
    //draw Background + Border
    with ClientRect do
    begin
    case FLayout of
      lpLeft:RadioRect := Rect(1, HeightOf(ClientRect) div 2 - 7, 15, HeightOf(ClientRect) div 2 + 7);
     lpRight:RadioRect := Rect(Width-15, HeightOf(ClientRect) div 2 - 7, Width-1, HeightOf(ClientRect) div 2 + 7);
     end;
    end;
    Pen.style   := psSolid;
    Brush.Style := bsClear;
    Pen.width := 1;
    if (Focused or MouseIn)and(not(csDesigning in ComponentState)) then
    begin
     if (not FMouseDown) then
     begin
        Brush.color := FFocusedColor;
        Pen.color   := FBorderColor;
     end else begin
        Brush.color := FDownColor;
        Pen.color   := FBorderColor;
     end;
    end else begin
      Brush.color := self.Color;
      Pen.color   := FBorderColor;
    end;
    DrawEllipse(Handle, RadioRect);
    if Checked then
    begin
     if Enabled then
     begin
        Brush.color := FCheckedColor;
        Pen.color   := FCheckedColor;
     end else begin
        Brush.color := clBtnShadow;
        Pen.color   := clBtnShadow;
     end;
     with RadioRect do
     begin
      SelectRect := Rect(Left + 3, Top + 3, Right - 3, Bottom - 3);
     end;
     DrawEllipse(Handle, SelectRect);
    end;
    //draw text
    Format := DT_WORDBREAK;
    Brush.Style := bsClear;
    with ClientRect do
    begin
     TextAs:=(RectHeight(ClientRect) - TextHeight('H')) div 2;
     case FLayout of
      lpLeft: begin
       TextBounds := Rect(Left + WidthOf(RadioRect)+2, Top + TextAs, Right + WidthOf(RadioRect), Bottom - TextAs);
       Format     := Format or DT_LEFT;
      end;
      lpRight: begin
       TextBounds := Rect(Left + 1, Top + TextAs, Right - WidthOf(RadioRect)-2, Bottom - TextAs);
       Format     := Format or DT_RIGHT;
      end;
     end;
    end;
    if not Enabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    end
    else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    UnLock;
  end;
end;

procedure TDefineRadioButton.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TDefineRadioButton.WMMove(var Message: TWMMove);
begin
  inherited;
  Invalidate;
end;

procedure TDefineRadioButton.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     ParentColor  := not Value;
     Invalidate;
  end;
end;

procedure TDefineRadioButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) and
        (GetActiveWindow <> 0) and (not MouseIn) then
  begin
    FMouseIn := True;
    Invalidate;
  end;
end;

procedure TDefineRadioButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := False;      
  end;
  invalidate;
end;

procedure TDefineRadioButton.CMFontChanged(var Message: TMessage);
begin
 inherited;
 invalidate;
end;

function TDefineRadioButton.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

procedure TDefineRadioButton.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if Enabled then
  begin
    SetFocus;
    //SetChecked(True);
    Click;
    //FChecked := True;
    FMouseDown := true;
    invalidate;
  end;
end;

procedure TDefineRadioButton.WMLButtonUP(var Message: TWMLButtonDown);
begin
  if Enabled then
  begin
    FMouseDown := false;
    Invalidate;
  end;
end;

procedure TDefineRadioButton.Click;
begin
  SetChecked(True);
  inherited Changed;
  inherited Click;  
end;

{ TDefineListBoxExt }

constructor TDefineListBoxExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle  := ControlStyle - [csOpaque];
  ParentFont    := True;
  AutoSize      := False;
  Ctl3D         := False;
  BorderStyle   := bsNone;
  FFocusColor   := clWhite;
  FBorderColor  := DefaultBorderColor;
  FFlatColor    := DefaultFlatColor;
  FParentColor  := True;
  FMouseIn      := False;
end;

procedure TDefineListBoxExt.RedrawBorder(const Clip: HRGN);
var
  Attrib:TBorderAttrib;
begin
  with Attrib do
  begin
   Ctrl           := self;
   BorderColor    := ColorBorder;
   if Enabled then begin
      FocusColor  := ColorFocused;
      FlatColor   := ColorFlat;
   end else begin
      FocusColor  := clBtnFace;
      FlatColor   := clBtnFace;
   end;
   MouseState     := MouseIn;
   FocusState     := Focused;
   DesignState    := ComponentState;
   HasBars        := false;
   BoldState      := false;
  end;
  Color := DrawEditBorder(Attrib,Clip);
end;

procedure TDefineListBoxExt.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
      RedrawBorder;
    end;
  end;
end;       

procedure TDefineListBoxExt.CMSysColorChange(var Message: TMessage);
begin
    if (Parent <> nil)and(FParentColor) then
       FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TDefineListBoxExt.CMParentColorChanged(var Message: TWMNoParams);
begin
    if (Parent <> nil)and(FParentColor) then
       FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TDefineListBoxExt.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusColor    := Value;
    1: FBorderColor   := Value;
    2: begin
         FFlatColor   := Value;
         FParentColor := False;
       end;
  end;
  RedrawBorder;
end;

procedure TDefineListBoxExt.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder;
  end;           
end;

procedure TDefineListBoxExt.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := False;
     RedrawBorder;
  end;
end;

procedure TDefineListBoxExt.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  RedrawBorder;
end;

procedure TDefineListBoxExt.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited; 
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TDefineListBoxExt.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TDefineListBoxExt.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TDefineListBoxExt.WMNCPaint(var Message: TMessage);
begin
  inherited;    
  RedrawBorder(HRGN(Message.WParam));
end;

{ TDefineCheckWrapper }
type
  TDefineCheckWrapper = class
  private
    FData: LongInt;
    FState: TCheckBoxState;
    FDisabled: Boolean;
    FHeader: Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    class function GetDefaultState: TCheckBoxState;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Header: Boolean read FHeader write FHeader;
  end;

var
  FCheckWidth, FCheckHeight: Integer;

procedure GetCheckSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckWidth  := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

function MakeSaveState(State: TCheckBoxState; Disabled: Boolean): TObject;
begin
  Result := TObject((Byte(State) shl 16) or Byte(Disabled));
end;

function GetSaveState(AObject: TObject): TCheckBoxState;
begin
  Result := TCheckBoxState(Integer(AObject) shr 16);
end;

function GetSaveDisabled(AObject: TObject): Boolean;
begin
  Result := Boolean(Integer(AObject) and $FF);
end;

function TDefineListBoxExt.GetMouseIn: Boolean;
begin
  Result := FMouseIn;
end;

{ TDefineCheckWrapper }

procedure TDefineCheckWrapper .SetChecked(Check: Boolean);
begin
  if Check then FState := cbChecked else FState := cbUnchecked;
end;

function TDefineCheckWrapper .GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

class function TDefineCheckWrapper .GetDefaultState: TCheckBoxState;
begin
  Result := cbUnchecked;
end;

{ TDefineCheckListExt }

constructor TDefineCheckListExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlat        := True;
  FHeaderColor := clInfoText;
  FHeaderBkColor := clInfoBk;
end;

destructor TDefineCheckListExt.Destroy;
begin
  FSaveStates.Free;
  inherited;
end;

procedure TDefineCheckListExt.CreateWnd;
var
  I: Integer;
  Wrapper: TDefineCheckWrapper ;
  SaveState: TObject;
begin
  inherited CreateWnd;
  if FSaveStates <> nil then
  begin
    for I := 0 to FSaveStates.Count - 1 do
    begin
      Wrapper := TDefineCheckWrapper (GetWrapper(I));
      SaveState := FSaveStates[I];
      Wrapper.FState := GetSaveState(SaveState);
      Wrapper.FDisabled := GetSaveDisabled(SaveState);
    end;
    FreeAndNil(FSaveStates);
  end;
  ResetItemHeight;
end;

procedure TDefineCheckListExt.DestroyWnd;
var
  I: Integer;
begin
  if Items.Count > 0 then
  begin
    FSaveStates := TList.Create;
    for I := 0 to Items.Count - 1 do
      FSaveStates.Add(MakeSaveState(State[I], not ItemEnabled[I]));
  end;
  inherited DestroyWnd;
end;

procedure TDefineCheckListExt.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;
    
function TDefineCheckListExt.GetCheckWidth: Integer;
begin
  Result := FCheckWidth + 2;
end;

procedure TDefineCheckListExt.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure TDefineCheckListExt.ResetItemHeight;
begin
  if HandleAllocated and (Style = lbStandard) then
  begin
    Canvas.Font := Font;
    FStandardItemHeight := Canvas.TextHeight('Wg');
    Perform(LB_SETITEMHEIGHT, 0, FStandardItemHeight);
  end;
end;

procedure TDefineCheckListExt.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  SaveEvent: TDrawItemEvent;
  ACheckWidth: Integer;
  Enable: Boolean;
begin
  ACheckWidth := GetCheckWidth;
  if Index < Items.Count then
  begin
    R := Rect;
    Enable := Self.Enabled and GetItemEnabled(Index);
    if not Header[Index] then
    begin
      if not UseRightToLeftAlignment then
      begin
        R.Right := Rect.Left;
        R.Left := R.Right - ACheckWidth;
      end
      else
      begin
        R.Left := Rect.Right;
        R.Right := R.Left + ACheckWidth;
      end;
      DrawCheck(R, GetState(Index), Enable);
    end
    else
    begin
      Canvas.Font.Color  := FHeaderColor;
      Canvas.Brush.Color := FHeaderBkColor;
    end;
    if not Enable then
      Canvas.Font.Color := clGrayText;
  end;

  if (Style = lbStandard) and Assigned(OnDrawItem) then
  begin
    { Force lbStandard list to ignore OnDrawItem event. }
    SaveEvent := OnDrawItem;
    OnDrawItem := nil;
    try
      inherited;
    finally
      OnDrawItem := SaveEvent;
    end;
  end
  else
    inherited;
end;

procedure TDefineCheckListExt.CNDrawItem(var Message: TWMDrawItem);
begin
  if Items.Count = 0 then exit;
  with Message.DrawItemStruct^ do
    if not Header[itemID] then
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left + GetCheckWidth
      else
        rcItem.Right := rcItem.Right - GetCheckWidth;
  inherited;
end;

procedure TDefineCheckListExt.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;
  ElementDetails: TThemedElementDetails;
begin
  SaveRgn := 0;
  DrawRect.Left   := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top    := R.Top + (R.Bottom - R.Top - FCheckHeight) div 2;
  DrawRect.Right  := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
  with Canvas do
  begin
    if Flat then
    begin
      { Remember current clipping region }
      SaveRgn := CreateRectRgn(0,0,0,0);
      GetClipRgn(Handle, SaveRgn);
      { Clip 3d-style checkbox to prevent flicker }
      with DrawRect do
        Rgn := CreateRectRgn(Left + 2, Top + 2, Right - 2, Bottom - 2);
      SelectClipRgn(Handle, Rgn);
      DeleteObject(Rgn);
    end;

   if ThemeServices.ThemesEnabled then
   begin
      case AState of
        cbChecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedDisabled);
        cbUnchecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled)
        else // cbGrayed
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedDisabled);
      end;
      ThemeServices.DrawElement(Handle, ElementDetails, R);
    end
    else
    begin
      case AState of
        cbChecked:
          DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
        cbUnchecked:
          DrawState := DFCS_BUTTONCHECK;
        else // cbGrayed
          DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
      end;
      if not AEnabled then
        DrawState := DrawState or DFCS_INACTIVE;
      DrawFrameControl(Handle, DrawRect, DFC_BUTTON, DrawState);
    end;

    if Flat then
    begin
      SelectClipRgn(Handle, SaveRgn);
      DeleteObject(SaveRgn);
      { Draw flat rectangle in-place of clipped 3d checkbox above }
      OldBrushStyle := Brush.Style;
      OldBrushColor := Brush.Color;
      OldPenColor := Pen.Color;
      Brush.Style := bsClear;
      Pen.Color := clBtnShadow;
      SetBkMode(Canvas.Handle,TRANSPARENT);
      with DrawRect do
        Rectangle(Left + 1, Top + 1, Right - 1, Bottom - 1);
      Brush.Style := OldBrushStyle;
      Brush.Color := OldBrushColor;
      Pen.Color := OldPenColor;
    end;
  end;
end;

procedure TDefineCheckListExt.SetChecked(Index: Integer; AChecked: Boolean);
begin
  if AChecked <> GetChecked(Index) then
  begin
    TDefineCheckWrapper (GetWrapper(Index)).SetChecked(AChecked);
    InvalidateCheck(Index);
  end;
end;

procedure TDefineCheckListExt.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then
  begin
    TDefineCheckWrapper (GetWrapper(Index)).Disabled := not Value;
    InvalidateCheck(Index);
  end;
end;

procedure TDefineCheckListExt.SetState(Index: Integer; AState: TCheckBoxState);
begin
  if AState <> GetState(Index) then
  begin
    TDefineCheckWrapper (GetWrapper(Index)).State := AState;
    InvalidateCheck(Index);
  end;
end;

procedure TDefineCheckListExt.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  if not Header[Index] then
  begin
    R := ItemRect(Index);
    if not UseRightToLeftAlignment then
      R.Right := R.Left + GetCheckWidth
    else
      R.Left := R.Right - GetCheckWidth;
    InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
    UpdateWindow(Handle);
  end;
end;
    
function TDefineCheckListExt.GetChecked(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := TDefineCheckWrapper (GetWrapper(Index)).GetChecked
  else
    Result := False;
end;

function TDefineCheckListExt.GetItemEnabled(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := not TDefineCheckWrapper (GetWrapper(Index)).Disabled
  else
    Result := True;
end;

function TDefineCheckListExt.GetState(Index: Integer): TCheckBoxState;
begin
  if HaveWrapper(Index) then
    Result := TDefineCheckWrapper (GetWrapper(Index)).State
  else
    Result := TDefineCheckWrapper .GetDefaultState;
end;

procedure TDefineCheckListExt.KeyPress(var Key: Char);
begin
  if (Key = ' ') then
    ToggleClickCheck(ItemIndex);
  inherited KeyPress(Key);
end;

procedure TDefineCheckListExt.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    Index := ItemAtPos(Point(X,Y),True);
    if (Index <> -1) and GetItemEnabled(Index) then
      if not UseRightToLeftAlignment then
      begin
        if X - ItemRect(Index).Left < GetCheckWidth then
          ToggleClickCheck(Index)
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          ToggleClickCheck(Index)
      end;
  end;
end;

procedure TDefineCheckListExt.ToggleClickCheck;
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and GetItemEnabled(Index) then
  begin
    State := Self.State[Index];
    case State of
      cbUnchecked:
        if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
    Self.State[Index] := State;
    ClickCheck;
  end;
end;

procedure TDefineCheckListExt.ClickCheck;
begin
  if Assigned(FOnClickCheck) then FOnClickCheck(Self);
end;

function TDefineCheckListExt.GetItemData(Index: Integer): LongInt;
begin
  Result := 0;
  if HaveWrapper(Index) then
    Result := TDefineCheckWrapper (GetWrapper(Index)).FData;
end;

function TDefineCheckListExt.GetWrapper(Index: Integer): TObject;
begin
  Result := ExtractWrapper(Index);
  if Result = nil then
     Result := CreateWrapper(Index);
end;

function TDefineCheckListExt.ExtractWrapper(Index: Integer): TObject;
begin
  Result := TDefineCheckWrapper (inherited GetItemData(Index));
  if LB_ERR = Integer(Result) then
    raise EListError.CreateFmt(SListIndexError,[index]);
  if (Result <> nil) and (not (Result is TDefineCheckWrapper )) then
    Result := nil;
end;

function TDefineCheckListExt.InternalGetItemData(Index: Integer): LongInt;
begin
  Result := inherited GetItemData(Index);
end;

procedure TDefineCheckListExt.InternalSetItemData(Index: Integer; AData: LongInt);
begin
  inherited SetItemData(Index, AData);
end;

function TDefineCheckListExt.CreateWrapper(Index: Integer): TObject;
begin
  Result := TDefineCheckWrapper .Create;
  inherited SetItemData(Index, LongInt(Result));
end;

function TDefineCheckListExt.HaveWrapper(Index: Integer): Boolean;
begin
  Result := ExtractWrapper(Index) <> nil;
end;

procedure TDefineCheckListExt.SetItemData(Index: Integer; AData: LongInt);
var
  Wrapper: TDefineCheckWrapper ;
begin
  if HaveWrapper(Index) or (AData <> 0) then
  begin
    Wrapper := TDefineCheckWrapper (GetWrapper(Index));
    Wrapper.FData := AData;
  end;
end;

procedure TDefineCheckListExt.ResetContent;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if HaveWrapper(I) then
      GetWrapper(I).Free;
  inherited;
end;

procedure TDefineCheckListExt.DeleteString(Index: Integer);
begin
  if HaveWrapper(Index) then
    GetWrapper(Index).Free;
  inherited;
end;

procedure TDefineCheckListExt.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TDefineCheckListExt.WMDestroy(var Msg: TWMDestroy);
var
  i: Integer;
begin
  for i := 0 to Items.Count -1 do
    ExtractWrapper(i).Free;
  inherited;
end;

function TDefineCheckListExt.GetHeader(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := TDefineCheckWrapper (GetWrapper(Index)).Header
  else
    Result := False;
end;

procedure TDefineCheckListExt.SetHeader(Index: Integer; const Value: Boolean);
begin
  if Value <> GetHeader(Index) then
  begin
    TDefineCheckWrapper(GetWrapper(Index)).Header := Value;
    InvalidateCheck(Index);
  end;
end;

procedure TDefineCheckListExt.SetHeaderBkColor(const Value: TColor);
begin
  if Value <> FHeaderBkColor then
  begin
    FHeaderBkColor := Value;
    Invalidate;
  end;
end;

procedure TDefineCheckListExt.SetHeaderColor(const Value: TColor);
begin
  if Value <> HeaderColor then
  begin
    FHeaderColor := Value;
    Invalidate;
  end;
end;

procedure TDefineCheckListExt.CheckAll;
var inx:integer;
begin
 for inx := 0 to Items.Count - 1 do
     Checked[inx] := true;
end;

procedure TDefineCheckListExt.CheckCancel;
var inx:integer;
begin
 for inx := 0 to Items.Count - 1 do
     Checked[inx] := False;
end;

{ TDefineProgressBar }
constructor TDefineProgressBar.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height          := 16;
  Width           := 147;
  FElementWidth   := 8;
  FElementColor   := $00996633;
  FBorderColor    := DefaultBorderColor;
  ParentColor     := True;
  Orientation     := pbHorizontal;
  FStep           := 10;
  FMin            := 0;
  FMax            := 100;
  FUseAdvColors   := false;
  FAdvColorBorder := 50;
  Transparent     := false;
end;

procedure TDefineProgressBar.SetOrientation (Value: TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if (csLoading in ComponentState) then
    begin
      Repaint;
      Exit;
    end;
    SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TDefineProgressBar.SetMin (Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.SetMax (Value: Integer);
begin
  if FMax <> Value then
  begin
    if Value < FPosition then FPosition := Value;
    FMax := Value;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.SetPosition (Value: Integer);
begin
  if Value > FMax then Value := FMax;
  if Value < FMin then Value := FMin;
  
  if Value > FPosition then
  begin
    FPosition := Value;
    DrawElements;
  end;
  if Value < FPosition then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.SetStep (Value: Integer);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.StepIt;
begin
  if (FPosition + FStep) > FMax then
    FPosition := FMax
  else
    FPosition := FPosition + FStep;
  DrawElements;
end;

procedure TDefineProgressBar.StepBy (Delta: Integer);
begin
  if (FPosition + Delta) > FMax then
    FPosition := FMax
  else
    FPosition := FPosition + Delta;
  DrawElements;
end;

procedure TDefineProgressBar.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FElementColor := Value;
    1: FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TDefineProgressBar.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TDefineProgressBar.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TDefineProgressBar.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TDefineProgressBar.CMParentColorChanged (var Message: TWMNoParams);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TDefineProgressBar.SetSmooth(Value: Boolean);
begin
  if Value <> FSmooth then
  begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TDefineProgressBar.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     Invalidate;
  end;
end;

{$IFDEF DFS_COMPILER_4_UP}
procedure TDefineProgressBar.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

procedure TDefineProgressBar.CheckBounds;
var
  maxboxes: Word;
begin
  if FOrientation = pbHorizontal then
  begin
    maxboxes := (Width - 3) div (FElementWidth + 1);
    if Width < 12 then
      Width := 12
    else
      Width := maxboxes * (FElementWidth + 1) + 3;
  end
  else
  begin
    maxboxes := (Height - 3) div (FElementWidth + 1);
    if Height < 12 then
      Height := 12
    else
      Height := maxboxes * (FElementWidth + 1) + 3;
  end;
end;

procedure TDefineProgressBar.Paint;
var
  PaintRect: TRect;
begin
  if not Smooth then
    CheckBounds;
  PaintRect := ClientRect;
  
  // Background
  if not FTransparent then begin
    canvas.Brush.Color := Self.Color;
    canvas.Brush.Style := bsSolid;
    canvas.FillRect(PaintRect);
  end;

  // Border
  canvas.Brush.Color := FBorderColor;
  Canvas.FrameRect(PaintRect);

  // Elements
  DrawElements;
end;

procedure TDefineProgressBar.DrawElements;
var
  NumElements, NumToPaint: LongInt;
  Painted: Byte;
  ElementRect: TRect;
begin
  with canvas do
  begin
    if not Smooth then begin
      if FOrientation = pbHorizontal then
      begin
        NumElements := Trunc((ClientWidth - 3) div (FElementWidth + 1));
        NumToPaint := Trunc((FPosition - FMin) / ((FMax - FMin) / NumElements) + 0.00000001);

        if NumToPaint > NumElements then
          NumToPaint := NumElements;

        {$IFDEF DFS_COMPILER_4_UP}
        if BidiMode = bdRightToLeft then
          ElementRect := Rect(ClientRect.Right - 2 - FElementWidth, ClientRect.Top + 2, ClientRect.Right - 2, ClientRect.Bottom - 2)
        else
          ElementRect := Rect(ClientRect.Left + 2, ClientRect.Top + 2, ClientRect.Left + 2 + FElementWidth, ClientRect.Bottom - 2);
        {$ELSE}
        ElementRect := Rect(ClientRect.Left + 2, ClientRect.Top + 2, ClientRect.Left + 2 + FElementWidth, ClientRect.Bottom - 2);
        {$ENDIF}

        if NumToPaint > 0 then
        begin
          Brush.Color := FElementColor;
          Brush.Style := bsSolid;
          for Painted := 1 to NumToPaint do
          begin
            Canvas.FillRect(ElementRect);
            {$IFDEF DFS_COMPILER_4_UP}
            if BidiMode = bdRightToLeft then
            begin
              ElementRect.Left := ElementRect.Left - FElementWidth - 1;
              ElementRect.Right := ElementRect.Right - FElementWidth - 1;
            end
            else
            begin
             ElementRect.Left := ElementRect.Left + FElementWidth + 1;
             ElementRect.Right := ElementRect.Right + FElementWidth + 1;
            end;
            {$ELSE}
            ElementRect.Left := ElementRect.Left + FElementWidth + 1;
            ElementRect.Right := ElementRect.Right + FElementWidth + 1;
            {$ENDIF}
          end;
        end;
      end
      else
      begin
        NumElements := Trunc((ClientHeight - 3) div (FElementWidth + 1));
        NumToPaint := Trunc((FPosition - FMin) / ((FMax - FMin) / NumElements) + 0.00000001);

        if NumToPaint > NumElements then
          NumToPaint := NumElements;
        ElementRect := Rect(ClientRect.Left + 2, ClientRect.Bottom - FElementWidth - 2, ClientRect.Right - 2, ClientRect.Bottom - 2);


        if NumToPaint > 0 then
        begin
          Brush.Color := FElementColor;
          Brush.Style := bsSolid;
          for Painted := 1 to NumToPaint do
          begin
            Canvas.FillRect(ElementRect);
            ElementRect.Top := ElementRect.Top - (FElementWidth + 1);
            ElementRect.Bottom := ElementRect.Bottom - (FElementWidth + 1);
          end;
        end;
      end;
    end
    else
    begin
      if (FOrientation = pbHorizontal) and (FPosition > 0) then
      begin
        Brush.Color := FElementColor;
        Canvas.FillRect(Rect(2, 2, ClientRect.Left + 2 + ((FPosition * (ClientWidth - 4)) div (FMax - FMin)), ClientRect.Bottom - 2));
      end
      else
      begin
        Brush.Color := FElementColor;
        Canvas.FillRect(Rect(2, ClientRect.Bottom - 2 - ((FPosition * (ClientHeight - 4)) div (FMax - FMin)), ClientRect.Right - 2, ClientRect.Bottom - 2));
      end;
    end;
  end;
end;

{ TDefineTitlebar }
constructor TDefineTitlebar.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Width := 100;
 Height := 19;
 ControlStyle := ControlStyle + [csAcceptsControls];
 TitlebarColor := ecCaptionBackground;
 ActiveTextColor := ecActiveCaption;
 InactiveTextColor := ecInactiveCaption;
  if csDesigning in ComponentState then
   begin
    FActive := True;
   end;
end;

destructor TDefineTitlebar.Destroy;
begin
 inherited Destroy;
end;

procedure TDefineTitlebar.Loaded;
var
 Wnd: HWND;
begin
 inherited Loaded;
  if not (csDesigning in ComponentState) and (FForm <> nil) then
   begin
    if FForm <> nil then
     begin
      Wnd := FForm.Handle;
      FWndProcInstance := MakeObjectInstance(FormWndProc);
      FDefProc := SetWindowLong(Wnd,GWL_WNDPROC,LongInt(FWndProcInstance));
     end;
  end;
end;

procedure TDefineTitlebar.FormWndProc(var Message: TMessage);
begin
 case Message.Msg of
  WM_ACTIVATE: DoActivateMessage(TWMActivate(Message));
 end;
 Message.Result := CallWindowProc(Pointer(FDefProc),FForm.Handle,Message.Msg,Message.WParam, Message.LParam);
end;

procedure TDefineTitlebar.DoActivateMessage(var Message: TWMActivate);
begin
 case Message.Active of
  WA_ACTIVE: DoActivation;
  WA_CLICKACTIVE: DoActivation;
  WA_INACTIVE: DoDeactivation;
 end;
end;

procedure TDefineTitlebar.DoActivation;
begin
 FActive := True;
 Invalidate;
 if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TDefineTitlebar.DoDeactivation;
begin
 FActive := False;
 Invalidate;
 if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TDefineTitlebar.Paint;
var
 iCaptionWidth, iCaptionHeight, iX, iY: Integer;
begin
 with Canvas do
  begin
   with ClientRect do
    begin
     Canvas.Font.Assign(Self.Font);
      case FActive of
       True: Canvas.Font.Color := FActiveTextColor;
       False: Canvas.Font.Color := FInactiveTextColor;
      end;
     iCaptionWidth := TextWidth(Caption);
     iCaptionHeight := TextHeight(Caption);
     Brush.Color := TitlebarColor;
     FillRect(ClientRect);
     iX := Width div 2 - iCaptionWidth div 2;
     iY := Height div 2 - iCaptionHeight div 2;
     TextOut(iX,iY,Caption);
    end;
  end;
end;

procedure TDefineTitlebar.MouseMove;
begin
 if FDown then
  begin
   TCustomForm(Owner).Left := TCustomForm(Owner).Left + X - FOldX;
   TCustomForm(Owner).Top := TCustomForm(Owner).Top + Y - FOldY;
  end;
end;

procedure TDefineTitlebar.MouseUp;
begin
 FDown := False;
end;

procedure TDefineTitlebar.MouseDown;
begin
 if (Button = mbleft) and not FDown then FDown := True;
 FOldX := X;
 FOldy := Y;
end;

procedure TDefineTitlebar.SetActiveTextColor(Value: TColor);
begin
 if Value <> FActiveTextColor then
  begin
   FActiveTextColor := Value;
   Invalidate;
  end;
end;

procedure TDefineTitlebar.SetInactiveTextColor(Value: TColor);
begin
 if Value <> FInactiveTextColor then
  begin
   FInactiveTextColor := Value;
   Invalidate;
  end;
end;

procedure TDefineTitlebar.SetTitlebarColor(Value: TColor);
begin
 if Value <> FTitlebarColor then
  begin
   FTitlebarColor := Value;
   Invalidate;
  end;
end;

procedure TDefineTitlebar.SetParent(AParent: TWinControl);
begin
 if (AParent <> nil) and not(AParent is TCustomForm) then
  raise EInvalidOperation.Create(SParentForm);
 FForm := TCustomForm(AParent);
 inherited;
end;

procedure TDefineTitlebar.CMFontChanged (var Message: TMessage);
begin
 Invalidate;
end;

procedure TDefineTitlebar.CMTextChanged (var Message: TMessage);
begin
 Invalidate;
end;

{ TDefineScrollbarTrackThumb }

constructor TDefineScrollbarThumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDefineScrollbarThumb.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iTop: Integer;
begin
  if TDefineScrollbarTrack(Parent).Kind = sbVertical then
  begin
    FTopLimit := 0;
    FBottomLimit := TDefineScrollbarTrack(Parent).Height;
    if FDown = True then
    begin
      iTop := Top + Y - FOldY;
      if iTop < FTopLimit then
      begin
        iTop := FTopLimit;
      end;
      if (iTop > FBottomLimit) or ((iTop + Height) > FBottomLimit) then
      begin
        iTop := FBottomLimit - Height;
      end;
      Top := iTop;
    end;
  end
  else
  begin
    FTopLimit := 0;
    FBottomLimit := TDefineScrollbarTrack(Parent).Width;
    if FDown = True then
    begin
      iTop := Left + X - FOldX;
      if iTop < FTopLimit then
      begin
        iTop := FTopLimit;
      end;
      if (iTop > FBottomLimit) or ((iTop + Width) > FBottomLimit) then
      begin
        iTop := FBottomLimit - Width;
      end;
      Left := iTop;
    end;
  end;
  TDefineScrollbarTrack(Parent).FPosition := TDefineScrollbarTrack(Parent).PositionFromThumb;
  TDefineScrollbarTrack(Parent).DoPositionChange;
  inherited MouseMove(Shift,X,Y);
end;

procedure TDefineScrollbarThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TDefineScrollbarThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbleft) and not FDown then FDown := True;
  FOldX := X;
  FOldy := Y;
  inherited MouseDown(Button,Shift,X,Y);
end;

{ TDefineScrollbarTrack }

constructor TDefineScrollbarTrack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := ecLightKaki;

  FThumb := TDefineScrollbarThumb.Create(Self);
  FThumb.Color := ecLightBrown;
  FThumb.ColorFocused := ecLightBrown;
  FThumb.ColorDown := ecLightBrown;
  FThumb.ColorBorder := ecLightBrown;
  //FThumb.ColorHighLight := ecLightBrown;
  FThumb.ColorShadow := ecLightBrown;
  FThumb.Height := 17;
  InsertControl(FThumb);

  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 1;
  FPosition := 0;

  FThumb.Top := ThumbFromPosition;
end;

destructor TDefineScrollbarTrack.Destroy;
begin
  FThumb.Free;
  inherited Destroy;
end;

procedure TDefineScrollbarTrack.Paint;
begin
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(ClientRect);
  end;
end;

procedure TDefineScrollbarTrack.SetSmallChange(Value: Integer);
begin
  if Value <> FSmallChange then
  begin
    FSmallChange := Value;
  end;
end;

procedure TDefineScrollbarTrack.SetLargeChange(Value: Integer);
begin
  if Value <> FLargeChange then
  begin
    FLargeChange := Value;
  end;
end;

procedure TDefineScrollbarTrack.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FThumb.Top := ThumbFromPosition;
  end;
end;

procedure TDefineScrollbarTrack.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FThumb.Top := ThumbFromPosition;
  end;
end;

procedure TDefineScrollbarTrack.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if Position > Max then
  begin
    Position := Max;
  end;
  if Position < Min then
  begin
    Position := Min;
  end;
  case FKind of
    sbVertical: FThumb.Top := ThumbFromPosition;
    sbHorizontal: FThumb.Left := ThumbFromPosition;
  end;
end;

procedure TDefineScrollbarTrack.SetKind(Value: TScrollBarKind);
begin
  if Value <> FKind then
  begin
    FKind:= Value;
    case FKind of
      sbVertical: FThumb.Height := 17;
      sbHorizontal: FThumb.Width := 17;
    end;
  end;
  Position := FPosition;
end;

procedure TDefineScrollbarTrack.WMSize(var Message: TMessage);
begin
  if FKind = sbVertical then
  begin
    FThumb.Width := Width;
  end
  else
  begin
    FThumb.Height := Height;
   end;
end;

function TDefineScrollbarTrack.ThumbFromPosition: Integer;
var
  iHW, iMin, iMax, iPosition, iResult: Integer;
begin
  iHW := 0;
  case FKind of
    sbVertical: iHW := Height - FThumb.Height;
    sbHorizontal: iHW := Width - FThumb.Width;
  end;
  iMin := FMin;
  iMax := FMax;
  iPosition := FPosition;
  iResult := Round((iHW / (iMax - iMin)) * iPosition);
  Result := iResult;
end;

function TDefineScrollbarTrack.PositionFromThumb: Integer;
var
  iHW, iMin, iMax, iPosition, iResult: Integer;
begin
  iHW := 0;
  case FKind of
    sbVertical: iHW :=  Height - FThumb.Height;
    sbHorizontal: iHW := Width - FThumb.Width;
  end;
  iMin := FMin;
  iMax := FMax;
  iPosition := 0;
  case FKind of
    sbVertical: iPosition := FThumb.Top;
    sbHorizontal: iPosition := FThumb.Left;
  end;
  iResult := Round(iPosition / iHW * (iMax - iMin));
  Result := iResult;
end;

procedure TDefineScrollbarTrack.DoPositionChange;
begin
  TDefineScrollbar(Parent).FPosition := Position;
  TDefineScrollbar(Parent).DoScroll;
end;

procedure TDefineScrollbarTrack.DoThumbHighlightColor(Value: TColor);
begin
  //FThumb.ColorHighlight := Value;
end;

procedure TDefineScrollbarTrack.DoThumbShadowColor(Value: TColor);
begin
  FThumb.ColorShadow := Value;
end;

procedure TDefineScrollbarTrack.DoThumbBorderColor(Value: TColor);
begin
  FThumb.ColorBorder := Value;
end;

procedure TDefineScrollbarTrack.DoThumbFocusedColor(Value: TColor);
begin
  FThumb.ColorFocused := Value;
end;

procedure TDefineScrollbarTrack.DoThumbDownColor(Value: TColor);
begin
  FThumb.ColorDown := Value;
end;

procedure TDefineScrollbarTrack.DoThumbColor(Value: TColor);
begin
 FThumb.Color := Value;
end;

procedure TDefineScrollbarTrack.DoHScroll(var Message: TWMScroll);
var
  iPosition: Integer;
begin
  case Message.ScrollCode of
    SB_BOTTOM: Position := Max;
    SB_LINELEFT: begin
                iPosition := Position;
                Dec(iPosition,SmallChange);
                Position := iPosition;
               end;
    SB_LINERIGHT: begin
                 iPosition := Position;
                 Inc(iPosition,SmallChange);
                 Position := iPosition;
                end;
    SB_PAGELEFT: begin
                iPosition := Position;
                Dec(iPosition,LargeChange);
                Position := iPosition;
               end;
    SB_PAGERIGHT: begin
                 iPosition := Position;
                 Inc(iPosition,LargeChange);
                 Position := iPosition;
                end;
    SB_THUMBPOSITION, SB_THUMBTRACK: Position := Message.Pos;
    SB_TOP: Position := Min;
  end;
  Message.Result := 0;
end;

procedure TDefineScrollbarTrack.DoVScroll(var Message: TWMScroll);
var
  iPosition: Integer;
begin
  case Message.ScrollCode of
    SB_BOTTOM: Position := Max;
    SB_LINEUP: begin
                iPosition := Position;
                Dec(iPosition,SmallChange);
                Position := iPosition;
               end;
    SB_LINEDOWN: begin
                 iPosition := Position;
                 Inc(iPosition,SmallChange);
                 Position := iPosition;
                end;
    SB_PAGEUP: begin
                iPosition := Position;
                Dec(iPosition,LargeChange);
                Position := iPosition;
               end;
    SB_PAGEDOWN: begin
                 iPosition := Position;
                 Inc(iPosition,LargeChange);
                 Position := iPosition;
                end;
    SB_THUMBPOSITION, SB_THUMBTRACK: Position := Message.Pos;
    SB_TOP: Position := Min;
  end;
  Message.Result := 0;
end;

procedure TDefineScrollbarTrack.DoEnableArrows(var Message: TMessage);
begin
  if Message.WParam = ESB_DISABLE_BOTH then
  begin
    TDefineScrollbar(Parent).EnableBtnOne(False);
    TDefineScrollbar(Parent).EnableBtnTwo(False);
  end;
  if Message.WParam = ESB_DISABLE_DOWN then
  begin
    if FKind = sbVertical then TDefineScrollbar(Parent).EnableBtnTwo(False);
  end;
  if Message.WParam = ESB_DISABLE_LTUP then
  begin
    TDefineScrollbar(Parent).EnableBtnOne(False);
  end;
  if Message.WParam = ESB_DISABLE_LEFT then
  begin
    if FKind = sbHorizontal then TDefineScrollbar(Parent).EnableBtnOne(False);
  end;
  if Message.WParam = ESB_DISABLE_RTDN then
  begin
    TDefineScrollbar(Parent).EnableBtnTwo(False);
  end;
  if Message.WParam = ESB_DISABLE_UP then
  begin
    if FKind = sbVertical then TDefineScrollbar(Parent).EnableBtnOne(False);
  end;
  if Message.WParam = ESB_ENABLE_BOTH then
  begin
    TDefineScrollbar(Parent).EnableBtnOne(True);
    TDefineScrollbar(Parent).EnableBtnTwo(True);
  end;
  Message.Result := 1;
end;

procedure TDefineScrollbarTrack.DoGetPos(var Message: TMessage);
begin
  Message.Result := Position;
end;

procedure TDefineScrollbarTrack.DoGetRange(var Message: TMessage);
begin
  Message.WParam := Min;
  Message.LParam := Max;
end;

procedure TDefineScrollbarTrack.DoSetPos(var Message: TMessage);
begin
  Position := Message.WParam;
end;

procedure TDefineScrollbarTrack.DoSetRange(var Message: TMessage);
begin
  Min := Message.WParam;
  Max := Message.LParam;
end;

procedure TDefineScrollbarTrack.DoKeyDown(var Message: TWMKeyDown);
var
  iPosition: Integer;
begin
  iPosition := Position;
  case Message.CharCode of
    VK_PRIOR: Dec(iPosition,LargeChange);
    VK_NEXT: Inc(iPosition,LargeChange);
    VK_UP: if FKind = sbVertical then Dec(iPosition,SmallChange);
    VK_DOWN: if FKind = sbVertical then Inc(iPosition,SmallChange);
    VK_LEFT: if FKind = sbHorizontal then Dec(iPosition,SmallChange);
    VK_RIGHT: if FKind = sbHorizontal then Inc(iPosition,SmallChange);
  end;
  Position := iPosition;
end;

{ TDefineScrollbarButton }

constructor TDefineScrollbarButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDefineScrollbarButton.Destroy;
begin
  inherited Destroy;
end;

procedure TDefineScrollbarButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  FNewDown := True;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := True;
end;

procedure TDefineScrollbarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

procedure TDefineScrollbarButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  FNewDown := False;
  FTimer.Enabled := False;
  FTimer.Free;
end;

procedure TDefineScrollbarButton.DoTimer(Sender: TObject);
begin
  if FNewDown = True then
  begin
    if Assigned(FOnDown) then FOnDown(Self);
    TDefineScrollbar(Parent).DoScroll;
  end;
end;

{ TDefineScrollbar }

constructor TDefineScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 200;
  Height := 17;

  Color := ecLightKaki;

  FBtnOne := TDefineScrollbarButton.Create(Self);
  FBtnOne.Color := ecLightKaki;
  FBtnOne.ColorFocused := ecLightKaki;
  FBtnOne.ColorDown := ecLightKaki;
  FBtnOne.ColorBorder := ecLightKaki;
  FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
  FBtnOne.OnDown := BtnOneClick;
  InsertControl(FBtnOne);

  FBtnTwo := TDefineScrollbarButton.Create(Self);
  FBtnTwo.Color := ecLightKaki;
  FBtnTwo.ColorFocused := ecLightKaki;
  FBtnTwo.ColorDown := ecLightKaki;
  FBtnTwo.ColorBorder := ecLightKaki;
  FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
  FBtnTwo.OnDown := BtnTwoClick;
  InsertControl(FBtnTwo);

  FTrack := TDefineScrollbarTrack.Create(Self);
  FTrack.Color := ecLightKaki;
  FTrack.SetBounds(0,0,Width,Height);
  InsertControl(FTrack);

  Kind := sbVertical;

  Min := 0;
  Max := 100;
  Position := 0;
  SmallChange := 1;
  LargeChange := 1;

  ButtonColor := ecScrollbar;
  ButtonFocusedColor := ecScrollbar;
  ButtonDownColor := ecScrollbar;
  ButtonBorderColor := ecScrollbar;
  ButtonHighlightColor := clWhite;
  ButtonShadowColor := clBlack;

  ThumbColor := ecScrollbarThumb;
  ThumbFocusedColor := ecScrollbarThumb;
  ThumbDownColor := ecScrollbarThumb;
  ThumbBorderColor := ecScrollbarThumb;
  ThumbHighlightColor := ecScrollbarThumb;
  ThumbShadowColor := ecScrollbarThumb;
end;

destructor TDefineScrollbar.Destroy;
begin
  FTrack.Free;
  FBtnOne.Free;
  FBtnTwo.Free;
  inherited Destroy;
end;

procedure TDefineScrollbar.SetSmallChange(Value: Integer);
begin
  if Value <> FSmallChange then
  begin
    FSmallChange := Value;
    FTrack.SmallChange := FSmallChange;
  end;
end;

procedure TDefineScrollbar.SetLargeChange(Value: Integer);
begin
  if Value <> FLargeChange then
  begin
    FLargeChange := Value;
    FTrack.LargeChange := FLargeChange;
  end;
end;

procedure TDefineScrollbar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FTrack.Min := FMin;
  end;
end;

procedure TDefineScrollbar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FTrack.Max := FMax;
  end;
end;

procedure TDefineScrollbar.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if Position < Min then
  begin
    Position := Min;
  end;
  if Position > Max then
  begin
    Position := Max;
  end;
  FTrack.Position := FPosition;
end;

procedure TDefineScrollbar.SetKind(Value: TScrollBarKind);
var
  i: Integer;
begin
  if FKind <> Value then
  begin
    FKind := Value;
    FTrack.Kind := FKind;
    if FKind = sbVertical then
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
      FBtnOne.Refresh;
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
      FBtnTwo.Refresh;
    end
    else
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
      FBtnOne.Refresh;
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
      FBtnTwo.Refresh;
    end;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      i := Width;
      Width := Height;
      Height := i;
    end;
  end;
end;

procedure TDefineScrollbar.SetButtonHighlightColor(Value: TColor);
begin
  if Value <> FButtonHighlightColor then
  begin
    FButtonHighlightColor := Value;
    //FBtnOne.ColorHighlight := ButtonHighlightColor;
    //FBtnTwo.ColorHighlight := ButtonHighlightColor;
  end;
end;

procedure TDefineScrollbar.SetButtonShadowColor(Value: TColor);
begin
  if Value <> FButtonShadowColor then
  begin
    FButtonShadowColor := Value;
    FBtnOne.ColorShadow := ButtonShadowColor;
    FBtnTwo.ColorShadow := ButtonShadowColor;
  end;
end;

procedure TDefineScrollbar.SetButtonBorderColor(Value: TColor);
begin
  if Value <> FButtonBorderColor then
  begin
    FButtonBorderColor := Value;
    FBtnOne.ColorBorder := ButtonBorderColor;
    FBtnTwo.ColorBorder := ButtonBorderColor;
  end;
end;

procedure TDefineScrollbar.SetButtonFocusedColor(Value: TColor);
begin
  if Value <> FButtonFocusedColor then
  begin
    FButtonFocusedColor := Value;
    FBtnOne.ColorFocused := ButtonFocusedColor;
    FBtnTwo.ColorFocused := ButtonFocusedColor;
  end;
end;

procedure TDefineScrollbar.SetButtonDownColor(Value: TColor);
begin
  if Value <> FButtonDownColor then
  begin
    FButtonDownColor := Value;
    FBtnOne.ColorDown := ButtonDownColor;
    FBtnTwo.ColorDown := ButtonDownColor;
  end;
end;

procedure TDefineScrollbar.SetButtonColor(Value: TColor);
begin
  if Value <> FButtonColor then
  begin
    FButtonColor := Value;
    FBtnOne.Color := ButtonColor;
    FBtnTwo.Color := ButtonColor;
  end;
end;

procedure TDefineScrollbar.SetThumbHighlightColor(Value: TColor);
begin
  if Value <> FThumbHighlightColor then
  begin
    FThumbHighlightColor := Value;
    FTrack.DoThumbHighlightColor(Value);
  end;
end;

procedure TDefineScrollbar.SetThumbShadowColor(Value: TColor);
begin
  if Value <> FThumbShadowColor then
  begin
    FThumbShadowColor := Value;
    FTrack.DoThumbShadowColor(Value);
  end;
end;

procedure TDefineScrollbar.SetThumbBorderColor(Value: TColor);
begin
  if Value <> FThumbBorderColor then
  begin
    FThumbBorderColor := Value;
    FTrack.DoThumbBorderColor(Value);
  end;
end;

procedure TDefineScrollbar.SetThumbFocusedColor(Value: TColor);
begin
  if Value <> FThumbFocusedColor then
  begin
    FThumbFocusedColor := Value;
    FTrack.DoThumbFocusedColor(Value);
  end;
end;

procedure TDefineScrollbar.SetThumbDownColor(Value: TColor);
begin
  if Value <> FThumbDownColor then
  begin
    FThumbDownColor := Value;
    FTrack.DoThumbDownColor(Value);
  end;
end;

procedure TDefineScrollbar.SetThumbColor(Value: TColor);
begin
  if Value <> FThumbColor then
  begin
    FThumbColor := Value;
    FTrack.DoThumbColor(Value);
  end;
end;

procedure TDefineScrollbar.BtnOneClick(Sender: TObject);
var
  iPosition: Integer;
begin
  iPosition := Position;
  Dec(iPosition,SmallChange);
  Position := iPosition;
end;

procedure TDefineScrollbar.BtnTwoClick(Sender: TObject);
var
  iPosition: Integer;
begin
  iPosition := Position;
  Inc(iPosition,SmallChange);
  Position := iPosition;
end;

procedure TDefineScrollbar.EnableBtnOne(Value: Boolean);
begin
  if Value = True then
  begin
    FBtnOne.Enabled := True;
    case FKind of
      sbVertical: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
      sbHorizontal: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
    end;
  end
  else
  begin
    case FKind of
      sbVertical: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_DISABLED');
      sbHorizontal: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_DISABLED');
    end;
    FBtnOne.Enabled := False;
  end;
end;

procedure TDefineScrollbar.EnableBtnTwo(Value: Boolean);
begin
  if Value = True then
  begin
    FBtnTwo.Enabled := True;
    case FKind of
      sbVertical: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
      sbHorizontal: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
    end;
  end
  else
  begin
    case FKind of
      sbVertical: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_DISABLED');
      sbHorizontal: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_DISABLED');
    end;
    FBtnTwo.Enabled := False;
  end;
end;

procedure TDefineScrollbar.WMSize(var Message: TWMSize);
begin
  if FKind = sbVertical then
  begin
    SetBounds(Left, Top, Width, Height);
    FBtnOne.SetBounds(0,0,Width,17);
    FBtnTwo.SetBounds(0,Height - 17,Width,17);
    FTrack.SetBounds(0,17,Width,Height - 34);
  end
  else
  begin
    SetBounds(Left, Top, Width, Height);
    FBtnOne.SetBounds(0,0,17,Height);
    FBtnTwo.SetBounds(Width - 17,0,17,Height);
    FTrack.SetBounds(17,0,Width - 34,Height);
  end;
  Position := FPosition;
end;

procedure TDefineScrollbar.DoScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self,Position);
end;

{ These scrollbar messages are just passed onto the TDefineScrollbarTrack for handling }

procedure TDefineScrollbar.CNHScroll(var Message: TWMScroll);
begin
  FTrack.DoHScroll(Message);
end;

procedure TDefineScrollbar.CNVScroll(var Message: TWMScroll);
begin
  FTrack.DoVScroll(Message);
end;

procedure TDefineScrollbar.SBMEnableArrows(var Message: TMessage);
begin
  FTrack.DoEnableArrows(Message);
end;

procedure TDefineScrollbar.SBMGetPos(var Message: TMessage);
begin
  FTrack.DoGetPos(Message);
end;

procedure TDefineScrollbar.SBMGetRange(var Message: TMessage);
begin
  FTrack.DoGetRange(Message);
end;

procedure TDefineScrollbar.SBMSetPos(var Message: TMessage);
begin
  FTrack.DoSetPos(Message);
end;

procedure TDefineScrollbar.SBMSetRange(var Message: TMessage);
begin
  FTrack.DoSetRange(Message);
end;

{ This message handler handles keyboard events }

procedure TDefineScrollbar.WMKeyDown(var Message: TWMKeyDown);
begin
  FTrack.DoKeyDown(Message); { Problems? }
end;

{ TDefineGauge }

constructor TDefineGauge.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  SetBounds(0,0,145,25);
  FMinValue       := 0;
  FMaxValue       := 100;
  FProgress       := 25;
  FShowText       := True;
  FBarColor       := $00996633;
  FBorderColor    := DefaultBorderColor;
  fStyleFace      := DefaultStyleFace;
  fStyleBars      := DefaultStyleHorizontal;

  fColorStart     := DefaultColorStart;
  fColorStop      := DefaultColorStop;
  ParentColor     := true;
  fTextAfter      := '';
  fTextFront      := '';
end;

procedure TDefineGauge.Paint;
var
  barRect, solvedRect: TRect;
  PercentText: String;
  PerInt,iDrawLen:Integer;
  memBitmap: TBitmap;
begin
  barRect   := ClientRect;
  memBitmap := TBitmap.Create;
  try;
   memBitmap.Width := ClientRect.Right;
   memBitmap.Height:= ClientRect.Bottom;
   // Clear Background
   if not FTransparent then begin
      memBitmap.Canvas.Brush.Color := Color;
      memBitmap.Canvas.FillRect(barRect);
   end;

   // Draw Border
   DrawButtonBorder(memBitmap.Canvas, ClientRect, FBorderColor, 1);

   // Calculate the Rect
   InflateRect(barRect, -1, -1);
   iDrawLen := SolvePercent(barRect.right-barRect.left,FMaxValue-FMinValue,FProgress);
   {$IFDEF DFS_COMPILER_4_UP}
   if BidiMode = bdRightToLeft then
      solvedRect := Rect(barRect.right - iDrawLen, barRect.top, barRect.right, barRect.bottom)
   else
      solvedRect := Rect(barRect.left, barRect.top, barRect.left + iDrawLen, barRect.bottom);
   {$ELSE}
      solvedRect := Rect(barRect.left, barRect.top, barRect.left + iDrawLen, barRect.bottom);
   {$ENDIF}

   // Fill the Rect
   if fStyleFace = fsDefault then begin
      memBitmap.Canvas.Brush.Color := FBarColor;
      memBitmap.Canvas.FillRect(solvedRect);
   end else begin
      DrawBackdrop(memBitmap.Canvas,fColorStart,fColorStop,solvedRect,fStyleBars);
   end;
   // Draw Text
   if FShowText then begin
    PerInt := SolvePercent(Fprogress-FMinValue,FMaxValue-FMinValue,100);
    PercentText := format('%s%3d%%%s',[fTextFront,PerInt,fTextAfter]);
    memBitmap.Canvas.Font.Assign(Self.Font);
    memBitmap.Canvas.Brush.Style := bsClear;
    DrawText(memBitmap.Canvas.Handle, PChar(PercentText), Length(PercentText), barRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    // bar is under caption
    IntersectClipRect(memBitmap.canvas.handle, solvedrect.left, solvedrect.top, solvedrect.right, solvedrect.bottom);
    memBitmap.Canvas.Font.Color := color;
    DrawText(memBitmap.Canvas.Handle, PChar(PercentText), Length(PercentText), barRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
   end;
   canvas.Lock;
   Canvas.CopyMode := cmSrcCopy;
   canvas.CopyRect(ClientRect, memBitmap.canvas, ClientRect);
   canvas.Unlock;
  finally
   memBitmap.Free;
  end;
end;

procedure TDefineGauge.SetShowText(Value: Boolean);
begin
  if FShowText <> Value then begin
    FShowText := Value;
    Repaint;
  end;
end;

procedure TDefineGauge.SetMinValue(Value: Longint);
begin
  if Value <> FMinValue then begin
    if Value > FMaxValue then
      FMinValue := FMaxValue
    else
      FMinValue := Value;
    if FProgress < Value then FProgress := Value;
      Repaint;
  end;
end;

procedure TDefineGauge.SetMaxValue(Value: Longint);
begin
  if Value <> FMaxValue then begin
    if Value < FMinValue then
      FMaxValue := FMinValue
    else
      FMaxValue := Value;
    if FProgress > Value then FProgress := Value;
      Repaint;
  end;
end;

procedure TDefineGauge.SetProgress(Value: Longint);
begin
  if Value < FMinValue then
     Value := FMinValue
  else
    if Value > FMaxValue then
       Value := FMaxValue;
  if FProgress <> Value then begin
    FProgress := Value;
    Repaint;
  end;
end;

procedure TDefineGauge.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FBorderColor := Value;
    1: FBarColor    := Value;
    2: fColorStart  := Value;
    3: fColorStop   := Value;
  end;
  Invalidate;
end;

procedure TDefineGauge.CalcAdvColors;
begin
  if FUseAdvColors then begin
     FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TDefineGauge.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TDefineGauge.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TDefineGauge.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TDefineGauge.CMParentColorChanged (var Message: TWMNoParams);
begin
  inherited;
  if FUseAdvColors then begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TDefineGauge.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
     FTransparent := Value;
     Invalidate;
  end;
end;

{$IFDEF DFS_COMPILER_4_UP}
procedure TDefineGauge.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

procedure TDefineGauge.SetTextAfter(const Value: TCaption);
begin
   if fTextAfter <> Value then begin
      fTextAfter := Value;
      Invalidate;
   end;
end;

procedure TDefineGauge.SetTextFront(const Value: TCaption);
begin
   if fTextFront <> Value then begin
      fTextFront := Value;
      Invalidate;
   end;
end;

procedure TDefineGauge.SetStyleOrien(const Value: TStyleOrien);
begin
  if fStyleBars <> Value then begin
     fStyleBars := Value;
     Invalidate;
  end;
end;

procedure TDefineGauge.SetStyleFace(const Value: TStyleFace);
begin
  if fStyleFace <> Value then begin
     fStyleFace := Value;
     Invalidate;
  end;
end;   

{ TDefineGUIScrollBar }

procedure TDefineGUIScrollBar.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    SetDownPos(spNone);
    SetCurPos(spNone);
    FreeTimer;
  end;

  UpdateHideState;

  //注意 UpdateHideState 必须写在 FOnEnabledChange 前面:

  if Assigned(FOnEnabledChange) then FOnEnabledChange(Self);
end;

procedure TDefineGUIScrollBar.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
//  FreeTimer;
  //为了配合 GetMousePos(FX,FY),设置这两个值:

  SetCurPos(spNone);
end;


constructor TDefineGUIScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoHide := false;
  FScrollcode := scSmall;
  FScrollMode := smAdd;
  FIsStartChange := true;
  FPosition := 0;
  FMin := 0;
  FPageSize := 0;
  FMax := 100;
  width := 121;
  FX := 0;
  FY := FX;
  WaitInterval := C_IntervalOfWait;
  height := C_Win2000ScrllBarBtnSize;  //  = 16
  FSmallChange := 1;
  FLargeChange := 8;
//  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TDefineGUIScrollBar.Destroy;
begin
  //保证 FTimer 的释放
  freeTimer;
  FOnDrawControl := nil;
  FOnChange := nil;
  fOnEnabledChange := nil;
  FOnScroll := nil;
  inherited;
end;

procedure TDefineGUIScrollBar.DoAutoScroll(Const aCode:TIScrollCode;
  aScrollMode: TScrollMode);
begin
  FScrollMode := aScrollMode;
  FScrollCode := aCode;
  FIsStartChange := true;  //设置 StartChange 为 真
  FreeTimer;    //FreeTimer 里面假如 Assigned(FTimer) 那么 设置 StartChange 为假
  if FIsStartChange then
  begin
    Scroll(aCode,aScrollMode);
    FIsStartChange := false;
    StartTimer(WaitInterval);
  end
  else
  StartTimer(C_Interval);
end;


procedure TDefineGUIScrollBar.DoMouseDownPos(const Value: TScrollBarPos);
begin
  paint; // invalidate 通过消息执行 paint 函数,速度慢于直接调用 paint
         // 虽然直接调用 Paint 可能出现设备错误,但该事件触发于鼠标点击
         // 中,所以 Canvas.handle 可以确定是可用的.
  case Value of
    spLeftBtn:
    begin
      DoAutoScroll(scSmall,smDec);
    end;
    spRightBtn:
    begin
      DoAutoScroll(scSmall,smAdd);
    end;
    spleftSpace:
    begin
      DoAutoScroll(scLarge,smDec);
    end;
    spRightSpace:
    begin
      DoAutoScroll(scLarge,smAdd);
    end;
    spTrack:
    begin

    end;
  end;
end;

procedure TDefineGUIScrollBar.DoMouseEnterPos(const Value: TScrollBarPos);
begin
  //如果鼠标点击对象然后离开对象,又再次回到对象:
  case FDownPos of
    spTrack,spNone:;
  else
    if FDownPos = Value then
      StartTimer(C_Interval);
  end;
  paint;
end;

procedure TDefineGUIScrollBar.DoMouseLeavePos(const Value: TScrollBarPos);
begin
  //如果鼠标点击对象,然后离开对象:
  case FDownPos of
    spTrack,spNone:;
  else
    if FDownPos = Value then
      FreeTimer;
  end;
  paint;
end;

procedure TDefineGUIScrollBar.DoMouseUpPos(const Value: TScrollBarPos);
begin
  //该 Invalidate 能让 Track 在移动之后回到正确位置,相当不错的代码:
  invalidate;
end;

procedure TDefineGUIScrollBar.FreeTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := false;
    FreeAndNil(FTimer);
    FIsStartChange := false;
  end;
end;

function TDefineGUIScrollBar.GetMousePos(const X, Y: integer): TScrollBarPos;
var
  p: TPoint;
begin
  p := Point(x,y);
  if PtInRect(FLeftBtn,p) then
    result := spLeftBtn
  else
  if PtInRect(FSpaceLeft,p) then
    result := spLeftSpace
  else
  if PtInRect(FTrackBtn,p) then
    result := spTrack
  else
  if PtInRect(FSpaceRight,p) then
    result := spRightSpace
  else
  if PtInRect(FRightBtn,p) then
    result := spRightBtn
  else result := spNone;
end;

function TDefineGUIScrollBar.GetSliderSize: integer;
begin
  if isVertical then
    result := FRightBtn.Top - FLeftBtn.Bottom
  else result := FRightBtn.Left - FLeftBtn.Right;
end;

function TDefineGUIScrollBar.GetTrackPos: integer;
var
  i: double;
  p: double;
  ValidSize: integer;
begin
  p := FPosition - FMin;
  ValidSize := GetValidSize;
  if p > ValidSize then
    p := ValidSize;
  if ValidSize > 0 then
    i := p / ValidSize
  else i := 0;
  result := Round((GetSliderSize - GetTrackSize) * i) ;
  if IsVertical then
    result := result + FLeftBtn.Bottom
  else result := result + FLeftBtn.Right;
end;

                                                
function TDefineGUIScrollBar.GetTrackSize: integer;
var
  i: integer;
  p: Double;
begin
  if FPageSize = 0 then
    result := C_Win2000ScrllBarBtnSize
  else         //判断为了防止 TrackSize 超越了最大范围
  if not Enabled or ((FMax - FMin + 1) <= FPageSize) then
    result := 0
  else
  begin  // FMin 永远小于或等于 FMax, 因此不怕发生除零错误.
         // 加入判断只为了安全起见.毕竟 Fmax-FMin+1 这样的计算不会占用 486 CPU :)
    i := GetSliderSize;
    if (FMax - FMin + 1) > 0 then
      p := FPageSize / (FMax - FMin + 1)
    else p := 0;
    result := Round(i * p);
    if result < GetMinTrackSize then
      result := GetMinTrackSize;
  end;
end;

function TDefineGUIScrollBar.IsVertical: Boolean;
begin
  result := FScrollBarKind = sbVertical;
end;

procedure TDefineGUIScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: integer);
begin
  if Button = mbleft then
  begin
    FreeTimer;
    FIsStartChange := true; //少许多余的代码,但是为了安全期间,不得不写.作用:确认状态
    SetDownPos(FCurPos);
    if FDownPos = spTrack then
    begin
      FX := x;
      FY := y;
      //保存 TrackPos
      if IsVertical then
        FTrackpos := FTrackBtn.Top
      else FTrackPos := FTrackBtn.Left;
    end;
  end;
  inherited MouseDown(button,Shift,x,y);
end;

procedure TDefineGUIScrollBar.MouseMove(Shift: TShiftState; x, y: integer);
begin
  SetCurPos(GetMousePos(x,y));

  if FDownPos = spTrack then
  begin
    if IsVertical then
      AdjustTrack(FTrackPos + y - Fy)
    else
      AdjustTrack(FTrackPos + x - Fx);
  end
  else
  begin
    FX := x;
    FY := y;
  end;
  inherited MouseMove(shift,x,y);
end;

procedure TDefineGUIScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: integer);
begin
  if Button = mbleft then
  begin
    FreeTimer;
    FIsStartChange := false; //少许多余的代码,但是为了安全期间,不得不写.作用:确认状态
    SetDownPos(spNone);
    SetCurPos(GetMousePos(x,y));
  end;
  inherited MouseUp(button,shift,x,y);     
end;

procedure TDefineGUIScrollBar.OnTimer(Sender: TObject);
begin
  //防止 Track 按钮经过移动之后进入鼠标位置:
  if (FDownPos = spLeftSpace) or (FDownPos = spRightSpace) then
    SetCurPos(GetMousePos(FX, FY));

  if FDownPos = FCurPos then
     Scroll(FScrollCode,FScrollMode);

  if Assigned(FTimer) then
  begin
    if FTimer.Interval = WaitInterval then //等待间隔状态
    begin
      FTimer.Interval := C_Interval;
    end;
  end;
end;

procedure TDefineGUIScrollBar.Paint;
var
  b: boolean;
begin
  //此处不可修改,它将计算 ScrollBar 的整个界面的 Rect:
  if FDownPos <> spTrack then
    UpdateScrollBarGUI;

  b := FOwnerDraw and Assigned(FOnDrawControl);

  //画滑轮:
  if b then
    FOnDrawControl(Canvas,dsTrack,FTrackBtn,GetDrawStateBy(dsTrack))
  else
    DrawControl(dsTrack,FTrackBtn,GetDrawStateBy(dsTrack));

  //画左边按钮:
  if b then
    FOnDrawControl(Canvas,dsLeftBtn,FLeftBtn,GetDrawStateBy(dsLeftBtn))
  else
    DrawControl(dsLeftBtn,FLeftBtn,GetDrawStateBy(dsLeftBtn));
  //画右边按钮:
  if b then
    FOnDrawControl(canvas,dsRightBtn,FRightBtn,GetDrawStateBy(dsRightBtn))
  else
    DrawControl(dsRightBtn,FRightBtn,GetDrawStateBy(dsRightBtn));

  //右边的空白地方:
  if b then
    FOnDrawControl(canvas,dsSpaceLeft, FSpaceLeft, GetDrawStateBy(dsSpaceLeft))
  else
    DrawControl(dsSpaceLeft, FSpaceLeft, GetDrawStateBy(dsSpaceLeft));
  if b then
    FOnDrawControl(canvas,dsSpaceRight, FSpaceRight, GetDrawStateBy(dsSpaceRight))
  else
    DrawControl(dsSpaceRight, FSpaceRight, GetDrawStateBy(dsSpaceRight));
end;

procedure TDefineGUIScrollBar.SetCurPos(const value: TScrollBarPos);
var
  b: TScrollBarPos;
begin
  if value <> FCurPos then
  begin
    b := FCurPos;
    FCurPos := value;
    DoMouseLeavePos(b);
    DoMouseEnterPos(value);
  end;
end;

procedure TDefineGUIScrollBar.SetDownPos(const Value: TScrollBarPos);
var
  b: TScrollBarPos;
begin
  if not CanShowTrack and (value = spRightSpace) then
  begin  //处理为了在 Track 不可见的时候,点击空白区域
    FDownPos := spNone;
  end
  else
  if value <> FDownPos then
  begin
    b := FDownPos;
    FDownPos := Value;
    DoMouseUpPos(b);
    DoMouseDownPos(Value);
  end;
end;

procedure TDefineGUIScrollBar.SetLargeChange(const Value: TScrollBarInc);
begin
  FLargeChange := Value;
end;

procedure TDefineGUIScrollBar.SetMax(Value: Integer);
begin
  if value < FMin then value := FMin;
  if FMax <> Value then
  begin
    FMax := Value;
    if PageSize > 0 then 
    begin
      if FMax - FPageSize + 1 < FPosition then
        SetPosition(FMax - FPageSize + 1);
    end
    else if FPosition > FMax then SetPosition(FMax);
    UpdateEnabledState;
    invalidate;
  end;
end;

procedure TDefineGUIScrollBar.SetMin(Value: Integer);
begin
  if Value > FMax then Value := FMax;
  if Value <> FMin then
  begin
    FMin := Value;
    if FPosition < FMin then SetPosition(FMin);
    UpdateEnabledState;
    Invalidate;
  end;
end;

procedure TDefineGUIScrollBar.SetPageSize(const Value: integer);
begin
  if (Value > -1) and (Value <> FPageSize) then
  begin
    FPageSize := Value;
    UpdateEnabledState;
    Invalidate;
  end;
end;

procedure TDefineGUIScrollBar.SetScrollBarKind(const Value: TScrollBarKind);
begin
  if FScrollBarKind <> Value then
  begin
    FScrollBarKind := Value;
    //注意载入控件的时候可能发生重复设置:
    if not (csloading in componentstate) then
      SetBounds(left,top,height,width);
    UpdateScrollBarGUI;
  end;
end;

procedure TDefineGUIScrollBar.SetSmallChange(const Value: TScrollBarInc);
begin
  FSmallChange := Value;
end;

procedure TDefineGUIScrollBar.StartTimer(const Interval: Cardinal);
begin
  if FTimer = nil then FTimer := TTimer.Create(self)
  else FTimer.OnTimer := nil;
  FTimer.Interval := Interval;
  FTimer.Enabled := true;
  FTimer.OnTimer := OnTimer;
end;

procedure TDefineGUIScrollBar.UpdateScrollBarGUI;
var
  i: integer;
begin
  if FScrollBarKind = sbHorizontal then
  begin
    if Width > C_Win2000ScrllBarBtnSize * 2 then
    begin
      FLeftBtn := Rect(0, 0, C_Win2000ScrllBarBtnSize, Height);
      FRightBtn := Rect(width - C_Win2000ScrllBarBtnSize, 0, width, Height);
    end
    else
    begin
      FLeftBtn := Rect(0, 0, width div 2, Height);
      FRightBtn := Rect(width div 2, 0, width, Height);
    end;
    if CanShowTrack then
    begin
    i := GetTrackPos;
    FTrackBtn.Left := i;
    FTrackBtn.Right := i + GetTrackSize;
    FTrackBtn.Top := 0;
    FTrackBtn.Bottom :=  height;

    FSpaceLeft := Rect(Fleftbtn.Right , 0, FTrackBtn.Left ,  Height);
    FSpaceRight := Rect(FTrackBtn.Right , 0, FRightBtn.Left ,  Height);
    end
    else
    begin
      FTrackBtn := Rect(-1,-1,-1,-1);
      FSpaceLeft := FTrackBtn;
      if Width > C_Win2000ScrllBarBtnSize * 2 then
        FSpaceRight := Rect(FLeftBtn.Right, 0, FRightBtn.Left , height)
      else FSpaceRight := FTrackBtn;
    end;
  end
  else
  begin
    if height > C_Win2000ScrllBarBtnSize * 2 then
    begin
      FLeftBtn := Rect(0, 0,  width, C_Win2000ScrllBarBtnSize);
      FRightBtn := Rect(0,  height - C_Win2000ScrllBarBtnSize,  width,  height);
    end
    else
    begin
      FLeftBtn := Rect(0, 0,  width, Height div 2);
      FRightBtn := Rect(0,  height div 2,  width,  height);
    end;
    if CanShowTrack then
    begin
      i := GetTrackPos;
      FTrackBtn.Left := 0;
      FTrackBtn.Top := i;
      FTrackBtn.Right :=  width;
      FTrackBtn.Bottom := i + GetTrackSize;

      FSpaceLeft := Rect(0,FLeftBtn.Bottom,  width ,FTrackBtn.Top);
      FSpaceRight := Rect(0,FTrackBtn.Bottom ,  width , FRightBtn.Top);
    end
    else
    begin
      FTrackBtn := Rect(-1,-1,-1,-1);
      FSpaceLeft := FTrackBtn;
      if height > C_Win2000ScrllBarBtnSize * 2 then
        FSpaceRight := Rect(0, FLeftBtn.Bottom, width , FRightBtn.top)
      else FSpaceRight := FTrackBtn;
    end;
  end;
end;


procedure TDefineGUIScrollBar.SetPosition(Value: integer);
begin
  if Value > FMax then value := FMax;
  if Value < FMin then Value := FMin;
  if FPosition <> value then
  begin
    FPosition := Value;
    if FDownPos <> spTrack then
      if parent <> nil then
      begin
        if Parent.Showing then paint
      end
      else Invalidate; 
    Changed;
  end;
end;

procedure TDefineGUIScrollBar.Scroll(const Code: TIScrollCode;
  const Mode: TScrollMode);
var
  t, j: integer;
begin
  case Code of
    scSmall:

      if mode = smAdd then
        t := FPosition + FSmallChange
      else t := FPosition - FSmallChange;

    scLarge:

      if mode = smAdd then
        t := FPosition + FLargeChange
      else t := FPosition - FLargeChange;
  else
    Exit;
  end;
  if t < FMin then t := FMin;
  if t > FMax - FPageSize + 1 then t := FMax - FPageSize + 1;
  if t > FMax then t := FMax;
  if t <> FPosition then
  begin
    if t > FPosition then
      j := t - FPosition
    else j := FPosition - t;
    SetPosition(t);
    if assigned(FOnScroll) then FOnScroll(self, FIsStartChange, code, mode, j);
  end;
end;

procedure TDefineGUIScrollBar.Changed;
begin
  if assigned(FOnChange) then FOnChange(self);
end;

procedure TDefineGUIScrollBar.DoScroll(const aMode: TScrollMode; const StartChange: boolean;
  const ScrollSize: integer);
var
  i: integer;
  j: integer;
begin
  if aMode = smAdd then
    i := FPosition + ScrollSize
  else
    i := Fposition - ScrollSize;

  if i > FMax - FpageSize + 1 then i := FMax - FPageSize + 1;
  if i > FMax then i := FMax;
  if i < FMin then i := FMin;
  
  if i <> FPosition then
  begin
    if i > FPosition then
      j := i - FPosition
    else j := FPosition - i;
    SetPosition(i);
    if Assigned(FOnScroll) then
      FOnScroll(self, StartChange, scCustom, amode, j);
  end;
end;

function TDefineGUIScrollBar.GetSliderRect: TRect;
begin
  if IsVertical then
    result := rect(0,FLeftBtn.Bottom,  width, FRightBtn.Top)
  else
    result := Rect(FLeftBtn.Right, 0, FRightBtn.Left,  height);
end;


procedure TDefineGUIScrollBar.AdjustTrack(Value: Integer);
    procedure UpdateScrollbarSpace;
    begin
      if FScrollBarKind = sbHorizontal then
      begin
        FSpaceLeft := Rect(Fleftbtn.Right , 0, FTrackBtn.Left ,  Height);
        FSpaceRight := Rect(FTrackBtn.Right , 0, FRightBtn.Left ,  Height);
      end
      else
      begin
        FSpaceLeft := Rect(0,FLeftBtn.Bottom,  width ,FTrackBtn.Top);
        FSpaceRight := Rect(0,FTrackBtn.Bottom ,  width , FRightBtn.Top);
      end;
    end;         
var
  size: integer;
  percent:Double;
  t: integer;  //TempInteger
  m: TScrollMode;
begin
  size := GetCurTrackSize;
  if IsVertical then
  begin
    if value <= FLeftBtn.Bottom then value := FLeftBtn.Bottom
    else if value + Size >= FRightBtn.Top then value := FRightBtn.Top - Size;
    FTrackBtn.Top := value;
    FTrackBtn.Bottom := FTrackBtn.Top + Size;
    //计算, 并且防止 除 零 错误:
    if GetSliderSize - GetCurTrackSize <> 0 then
    begin
      percent := (FTrackBtn.Top - (FLeftBtn.Bottom)) / (GetSliderSize - GetCurTrackSize);
      size := FMin + round(Percent * GetValidSize);
    end
    else size := 0;  //注意的地方
  end
  else
  begin
    if value <= FLeftBtn.Right then value := FLeftBtn.Right
    else if value + Size >= FRightBtn.Left then value := FRightBtn.Left - Size;
    FTrackBtn.Left := value;
    FTrackBtn.Right := FTrackBtn.left + Size;

    //计算, 并且防止 除 零 错误:
    if GetSliderSize - GetCurTrackSize <> 0 then
    begin
      percent := (FTrackBtn.left - (FLeftBtn.right)) / (GetSliderSize - GetCurTrackSize);
      size := FMin + round(Percent * GetValidSize );
    end
    else size := 0;  //注意的地方
  end;
  
  //注意这儿,必须更新和刷新空白区域
  UpdateScrollbarSpace;

  
  // Size Is New Position
  if size <> Fposition then
  begin
    if size > FPosition then
    begin
      m := smAdd;
      t := Size - FPosition;
    end
    else
    begin
      m := smDec;
      t := FPosition - size;
    end;
    SetPosition(size);
    if Assigned(FOnScroll) then
      FOnScroll(self, FIsStartChange, scTrackMove, m, t);
    FIsStartChange := false;
  end;
  invalidate;
end;

function TDefineGUIScrollBar.GetCurTrackSize: Integer;
begin
  if IsVertical then
    result := FTrackbtn.Bottom - FTrackBtn.Top
  else result := FTrackBtn.Right - FTrackBtn.Left;
end;

function TDefineGUIScrollBar.GetDrawStateBy(const Typ: TDrawScrollBar): TButtonState;
begin
  if not Enabled then result := bsDisabled
  else
  begin
    case Typ of
      dsLeftBtn:
      begin
        if FDownPos <> spNone then
        begin
          if (FDownPos = spLeftBtn) and (FCurPos = spLeftBtn) then
            result := bsDown
          else result := bsExclusive;
        end
        else
        begin
          if FCurPos = spLeftBtn then
            result := bsUp
          else result := bsExclusive;
        end;
      end;
      dsRightBtn:
      begin
        if FDownPos <> spNone then
        begin
          if (FDownPos = spRightBtn) and (FCurPos = spRightBtn) then
            result := bsDown
          else result := bsExclusive;
        end
        else
        begin
          if FCurPos = spRightBtn then
            result := bsUp
          else result := bsExclusive;
        end;
      end;
      dsTrack:
      begin
        if FDownPos <> spNone then
        begin
          if FDownPos = spTrack then
            result := bsDown
          else result := bsExclusive;
        end
        else
        begin
          if FCurPos = spTrack then
            result := bsUp
          else Result := bsExclusive;
        end;
      end;
      dsSpaceLeft:
      begin
        if FDownPos <> spNone then
        begin
          if FDownPos = spLeftSpace then
            result := bsDown
          else result := bsExclusive;
        end
        else Result := bsExclusive;
      end;
      dsSpaceRight:
      begin
        if FDownPos <> spNone then
        begin
          if FDownPos = spRightSpace then
            result := bsDown
          else result := bsExclusive;
        end
        else Result := bsExclusive;
      end;
    else result := bsDisabled;
    end;
  end;
end;

procedure TDefineGUIScrollBar.DrawControl(const Typ: TDrawScrollBar;
  const R: TRect; const State: TButtonState);
var
  re: TREct;
  i: integer;
begin
  canvas.Brush.Color := color;
  canvas.Brush.Style := bsSolid;

  if (Typ = dsspaceright) or (Typ = dsspaceleft)  then
  begin
    if State = bsdown then
      canvas.brush.Color := clBlack;
    canvas.FillRect(r) ;
  end
  else
  begin
    re := r;
    if State = bsdown then i := BDR_SUNKENOUTER else
      i := BDR_RAISEDINNER;
    canvas.FillRect(r);
    DrawEdge(Canvas.Handle,re, i, BF_RECT);
    if State = bsdown then
      InflateRect(re,-3,-3);
    if Typ = dsLeftBtn then
    begin
      if IsVertical then
        DrawArrows(canvas,daTop,re)
      else DrawArrows(canvas,daLeft,re);
    end
    else
    if Typ = dsRightBtn then
    begin
      if IsVertical then
        DrawArrows(canvas,daBottom,re)
      else DrawArrows(canvas,daRight,re);
    end;
  end;
end;

procedure TDefineGUIScrollBar.SetAutoHide(const Value: boolean);
begin
  FAutoHide := Value;
  UpdateHideState;
end;

procedure TDefineGUIScrollBar.UpdateHideState;
begin
  Visible := not (FAutoHide and not Enabled);
end;

procedure TDefineGUIScrollBar.UpdateEnabledState;
begin
  Enabled := (FMax - FMin >= FPageSize) ;
end;

function TDefineGUIScrollBar.GetValidSize: integer;
begin
  result := FMax - FMin - FPageSize + 1;
  if result > FMax then Result := FMax;
end;

Function TDefineGUIScrollBar.GetMinTrackSize: integer;
begin
  result := C_Win2000ScrllBarBtnSize div 2 + 1;
end;

function TDefineGUIScrollBar.CanShowTrack: Boolean;
begin
  if IsVertical then
    result := height > C_Win2000ScrllBarBtnSize * 2 + GetTrackSize
  else
  result := Width > C_Win2000ScrllBarBtnSize * 2 + GetTrackSize;
end;

procedure TDefineGUIScrollBar.DrawArrows(Cav: TCanvas; const v: TDrawArrow;const  R: TRect);
var
  x, y: integer;
  i: integer;
begin
  x := r.Left + (r.Right - r.Left - 1) div 2;
  y := r.Top + (r.Bottom - r.Top - 1) div 2;
  i := 0;
  case v of
    daleft, daRight:
    begin
      if (r.Right - r.Left >= 11) and (r.Bottom - r.Top >= 8) then
        i := 0
      else
      if (r.Right - r.Left >= 9) and (r.Bottom - r.Top >= 7) then
        i := 1
      else
      if (r.Right - r.Left >= 7) and (r.Bottom - r.Top >= 6) then
        i := 2
      else i := -1;
    end;
    datop,dabottom:
    begin
      if (r.Right - r.Left >= 8) and (r.Bottom - r.Top >= 11) then
        i := 0
      else
      if (r.Right - r.Left >= 7) and (r.Bottom - r.Top >= 9) then
        i := 1
      else
      if (r.Right - r.Left >= 6) and (r.Bottom - r.Top >= 7) then
        i := 2
      else i := -1;
    end;
  end;

  with Cav do
  begin
    Case i of
      0:           // 画最大的:
      begin
        case v of
          daleft:
          begin
            MoveTo(x-2,y);
            LineTo(x+2,y);
            MoveTo(x-1,y-1);
            LineTo(x+1,y-1);
            MoveTo(x-1,y+1);
            LineTo(x+1,y+1);
            MoveTo(x,y-2);
            LineTo(x,y+3);
            MoveTo(x+1,y-3);
            LineTo(x+1,y+4);
          end;
          datop:
          begin
            MoveTo(x,y-2);
            LineTo(x,y+2);
            MoveTo(x-1,y-1);
            LineTo(x+2,y-1);
            Moveto(x-2,y);
            LineTo(x+3,y);
            Moveto(x-3,y+1);
            LineTo(x+4,y+1);
          end;
          daRight:
          begin
            MoveTo(x-1,y);
            LineTo(x+3,y);
            MoveTo(x-1,y-3);
            LineTo(x-1,y+4);
            MoveTo(x,y-2);
            LineTo(x,y+3);
            MoveTo(x+1,y-1);
            LineTo(x+1,y+2);
          end;
          dabottom:
          begin
            MoveTo(x,y-1);
            LineTo(x,y+3);
            MoveTo(x-1,y+1);
            LineTo(x+2,y+1);
            Moveto(x-2,y);
            LineTo(x+3,y);
            Moveto(x-3,y-1);
            LineTo(x+4,y-1);
          end;
        end;
      end;       //画中等的
      1:
      begin
        case v of
          daleft:
          begin
            MoveTo(x-1,y);
            LineTo(x+2,y);
            MoveTo(x,y-1);
            LineTo(x,y+2);
            MoveTo(x+1,y-2);
            LineTo(x+1,y+3);
          end;
          datop:
          begin
            MoveTo(x,y-1);
            LineTo(x,y+2);
            MoveTo(x-1,y);
            LineTo(x+2,y);
            MoveTo(x-2,y+1);
            LineTo(x+3,y+1);
          end;
          daRight:
          begin
            MoveTo(x-1,y);
            LineTo(x+2,y);
            MoveTo(x,y-1);
            LineTo(x,y+2);
            MoveTo(x-1,y-2);
            LineTo(x-1,y+3);
          end;
          dabottom:
          begin
            MoveTo(x,y-1);
            LineTo(x,y+2);
            MoveTo(x-1,y);
            LineTo(x+2,y);
            MoveTo(x-2,y-1);
            LineTo(x+3,y-1);
          end;
        end;
      end;
      2:         //画最小的:
      begin
        case v of
          daleft:
          begin
            MoveTo(x-1,y);
            LineTo(x+1,y);
            MoveTo(x,y-1);
            LineTo(x,y+2);
          end;
          datop:
          begin
            MoveTo(x,y-1);
            LineTo(x,y+1);
            MoveTo(x-1,y);
            LineTo(x+2,y);
          end;
          daRight:
          begin
            MoveTo(x,y);
            LineTo(x+2,y);
            MoveTo(x,y-1);
            LineTo(x,y+2);
          end;
          dabottom:
          begin
            MoveTo(x-1,y);
            LineTo(x+2,y);
            MoveTo(x,y);
            LineTo(x,y+2);
          end;
        end;
      end;
    end;
  end;
end;

{ TDefineGUISelectList }

procedure TDefineGUISelectList.ChangeSelect(const Value: integer);
begin
  if (value > -1) and (value < size ) then
    Bits[Value] := not Bits[Value];
end;

procedure TDefineGUISelectList.Select(const Value: integer);
begin
  if (value > -1) and (Value < Size) then
    bits[Value] := true;
end;

procedure TDefineGUISelectList.SelectAll;
var
  i: integer;                         
begin
  for i := 0 to Size -1 do
    Bits[i] := true;
end;

procedure TDefineGUISelectList.UnSelect(const Value: integer);
begin
  if (value > -1) and (Value < Size) then
    bits[Value] := false;
end;

procedure TDefineGUISelectList.UnSelectAll;
var
  i: integer;
begin
  for i := 0 to Size -1 do
    Bits[i] := false;
end;

procedure TDefineGUISelectList.ChangeSelectSome(V1, V2: integer);
begin
  if v1 > size -1 then v1 := size -1
  else if v1 < 0 then V1 := 0;
  if v2 > size -1 then v2 := size -1
  else if v2 < 0 then V2 := 0;
  for v1 := v2 to v1 do
    bits[V1] := not Bits[V1]; 
end;

procedure TDefineGUISelectList.UnSelectSome(V1, V2: integer);
begin
  if v1 > size -1 then v1 := size -1
  else if v1 < 0 then V1 := 0;
  if v2 > size -1 then v2 := size -1
  else if v2 < 0 then V2 := 0;
  if V1 > v2 then
  begin
    for v1 := v2 to v1 do
      bits[V1] := false;
  end
  else
  begin
    for v1 := v1 to v2 do
      bits[V1] := false;
  end;
end;

procedure TDefineGUISelectList.SelectSome(V1, V2: integer);
begin
  if v1 > size -1 then v1 := size -1
  else if v1 < 0 then V1 := 0;
  if v2 > size -1 then v2 := size -1
  else if v2 < 0 then V2 := 0;
  if V1 > v2 then
  begin
    for v1 := v2 to v1 do
      bits[V1] := true;
  end
  else
  begin
    for v1 := v1 to v2 do
      bits[V1] := true;
  end;
end;


{ TDefineGUICtrlSave } // =======================================================

procedure TDefineGUICtrlSave.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

constructor TDefineGUICtrlSave.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FSelectList := TDefineGUISelectList.Create;
  FBakList := TDefineGUISelectList.Create;
  FVbar := TDefineGUIScrollBar.Create(self);
  with FVBar do
  begin
    FVBar.ParentColor := false;
    Parent := self;
    color := clBtnFace;
    ScrollBarKind := sbVertical;
    Min := 0;
    Max := 0;
    AutoHide := true;
    OnScroll := OnVbarScroll;
    WaitInterval := 150;
    OnEnabledChange := OnVbarEnabledChange;
  end;
  FOwnerDraw   := false;
  FBmp         := TBitmap.Create;
  FActiveItem  := -1;
  width        := 180;
  height       := 180;

  FMousePage   := cpNone;
  FTopIndex    := 0;
  FWheel.WheelCount := 0;
  FWheel.Wheeling   := false;

  TabStop      := true; //不能忽略
  FFocusItem   := -1;
  FItemHeight  := 14;
  FItemIndex   := -1;
  FRefreshing    := false;
  FMultiSelect := false;
  FCount       := 0;
  UpdateWorkRect;

  ControlStyle := ControlStyle + [csOpaque] ;
end;

destructor TDefineGUICtrlSave.Destroy;
begin
  FOnItemClick    := nil;
  FOnItemDlbClick := nil;
  FOnItemDraw     := nil;
//******************************************************
  if FBakList <> nil then
    FreeAndNil(FBakList);
  if FSelectList <> nil then
    FreeAndNil(FSelectList);
  if FVbar <> nil then
    FreeAndNil(FVBar);
  if FBmp <> nil then
    FreeAndNil(FBmp);
  inherited;
end;
           
procedure TDefineGUICtrlSave.UpdateTopIndex;
begin
  if (Count + 1 - Topindex) < GetPageSize then
  begin
    Topindex := Topindex - 1;
  end;
end;

procedure TDefineGUICtrlSave.Put(const Index: Integer);
begin
  if not FRefreshing and ItemCanSee(index) then invalidate;
end;

procedure TDefineGUICtrlSave.Insert(Index: integer);
var
  i: integer;
begin
  if IsItem(index) then
  begin
    Count := FCount + 1;
    for i := FCount -1 downto Index  do
      if Selected[i] then
      begin
        FSelectlist.UnSelect(i);
        Fselectlist.Select(i + 1);
      end;
    if Index <= FDownItem then
      inc(FDownItem);
    if Index <= FItemIndex then
      inc(FItemIndex);
    if FFocusItem <= FItemIndex then inc(FFocusItem);
    invalidate;
  end;
end;

procedure TDefineGUICtrlSave.Move(const CurIndex, NewIndex: Integer);
     procedure MoveFlagItem(var i: integer);
     begin
       if CurIndex < NewIndex then
       begin
         if i = CurIndex then
           i := NewIndex
         else
         if (i > CurIndex) and (i <= NewIndex) then
           Dec(i);
       end
       else
       begin
         if i = CurIndex then
           i := NewIndex
         else
         if (i >= NewIndex) and (i < CurIndex) then
           inc(i);
       end;
     end;
var
  i: integer;
  Cb: boolean;
begin
  if isItem(CurIndex) and IsItem(NewIndex) and (CurIndex <> NewIndex) then
  begin
    Cb := GetSelected(CurIndex);

    //必须增加这个, 在改变同时被移动的项目的 Select 状态的时候, CurIndex 被特殊处理:
    FSelectList.UnSelect(CurIndex);

    if CurIndex < NewIndex then
    begin
      for i := CurIndex + 1 to NewIndex do
        if Selected[i] then
        begin
          FSelectList.select(i - 1);
          FSelectList.UnSelect(i);
        end;
    end
    else
    begin
      for i := CurIndex - 1 downto NewIndex do
        if Selected[i] then
        begin
          FSelectlist.UnSelect(i);
          Fselectlist.Select(i + 1);
        end;
    end;
    FSelectList.Bits[NewIndex] := Cb;
    MoveFlagItem(FitemIndex);
    MoveFlagItem(FDownItem);
    MoveFlagItem(FFocusItem);
    invalidate;
  end;
end;

procedure TDefineGUICtrlSave.Add;
var
  b: boolean;
begin
  b := Refreshing;
  if not b then
  begin                   
    BeginUpdate;
    try
      count := count + 1;
      if ItemCanSee(Count - 1) then
        Invalidate;
    finally EndUpdate; end;
  end;
end;

procedure TDefineGUICtrlSave.Delete(Index: Integer);
var
  i: integer;
begin
  if IsItem(index) then
  begin
    if count > 0 then
    begin
      if FMultiSelect then
      begin
        if Selected[index] then
          FSelectlist.UnSelect(index);
        for i := index + 1  to FCount do
          if Selected[i] then
          begin
            FSelectList.select(i - 1);
            FSelectList.UnSelect(i);
          end;
      end
      else FSelectlist.UnSelectAll;
      Count := FCount -1;
      UpdateTopIndex ;//  重要的代码

      if index > 0 then
      begin
        if FDownItem >= index then
          dec(FDOwnItem);

        if FItemIndex >= index then
          Dec(FItemIndex);

        if FFocusItem >= index then
          Dec(FFocusItem);
      end;
      if not FMultiSelect then
        FSelectList.Select(FItemindex);

      invalidate;
    end;
  end;
end;

function TDefineGUICtrlSave.GetSelected(const index: integer): Boolean;
begin
  result := IsItem(index);
  if result then
    result := FSelectList.Bits[index];
end;

procedure TDefineGUICtrlSave.SetCount(const Value: Integer);
begin
  if FCount <> value then
  begin
    FCount := Value;
    FSelectList.Size := Value;
    UpdateMax;
    UpdatePageSizeOfVbar;
    if not Refreshing then invalidate;
  end;
end;

procedure TDefineGUICtrlSave.SetMultiSelect(const Value: boolean);
begin
  FMultiSelect := Value;
end;

procedure TDefineGUICtrlSave.SetSelected(const index: integer;
  const Value: Boolean);
begin
  if IsItem(index) then
  begin
    if FMultiSelect then
    begin
      FSelectList.Bits[index] := value;
    end
    else
    begin
      if Value then
        SetItemIndex(Index)
      else FSelectList.UnSelect(index); 
    end;
  end;
end;

procedure TDefineGUICtrlSave.UpdateMax;
begin
  FVbar.Max := FCount;
end;

procedure TDefineGUICtrlSave.SetItemHeight(const Value: integer);
begin
  if value < 1 then raise Exception.Create('Can not Set ItemHeight < 1.'); 
  if FItemHeight <> value then
  begin
    FItemHeight := Value;
    UpdatePageSizeOfVbar;
    UpdateTopIndex;
    if not Refreshing then Invalidate;
  end;
end;

function TDefineGUICtrlSave.IsItem(const Index: Integer): boolean;
begin
  result := (Index > -1) and (Count > Index); 
end;

procedure TDefineGUICtrlSave.MouseEnterItem(const Index: integer);
begin

end;

procedure TDefineGUICtrlSave.MouseLeaveItem(const Index: integer);
begin

end;

procedure TDefineGUICtrlSave.Paint;
var
  i: integer;
  j: integer;
  State: TListItemStates;
  R: TRect;
begin
  if not Refreshing then
  begin
    if Fcount > 0 then
    begin
      Fbmp.Width := FWorkRect.Right + 1;
      FBmp.Height := FWorkRect.Bottom;
      FBmp.Canvas.Brush.Color := Color;
      FBmp.Canvas.FillRect(FWorkRect);       
      //want for Draw Item:
      if Count - FTopIndex >= PageSize then
        j := TopIndex + GetPageSize
      else j := TopIndex + (Count - TopIndex);
      if j >= FCount then j := FCount - 1;
      //===================================================
      for i := FTopIndex to j do
      begin
        //Rect:
        r := GetItemRect(i);
        //Item tate:
        state := [];
        if Selected[i] then State := state + [isSelected];
        if i = FActiveItem then state := state + [isActive];
        if (i = FFocusItem) and Focused then state := state + [isFocused];
        if not Enabled then
        begin
          state := state + [isDisabled];
        end
        else if FMouseDown then
        begin
          if (FDownItem = i) and (i = FMouseItem) then
            state := state + [isDown];
        end
        else if FMouseItem = i then State := state + [isUp];

        // Run
        if FOwnerDraw and Assigned(FOnItemDraw) then
          FOnItemDraw(FBmp.canvas, i, r, State)
        else
          DrawItem(FBmp.canvas, i, r, State); 
      end;
      BitBlt(canvas.Handle,FWorkRect.Left,FWorkRect.Top,  FWorkRect.Right - FWorkRect.Left, FWorkRect.Bottom - FWorkRect.top,
        FBmp.Canvas.Handle, FWorkRect.Left, FWorkRect.top,SRCCOPY);
    end
    else
    begin
      canvas.Brush.Color := Color;
      canvas.FillRect(FWorkRect); 
    end;
  end;
end;

function TDefineGUICtrlSave.ItemAtPoint(const X, Y: integer): integer;
begin
  result := -1;
  if (x >= FWorkRect.Left) and (x < FWorkRect.Right) then
  begin
    result := ItemAtY(y);
  end;
end;

procedure TDefineGUICtrlSave.WMSIZE(var msg: TWMSIZE);
begin
  inherited;
  UpdateWorkRect;
  UpdatePageSizeOfVbar;
  UpdateTopIndex;
  FVbar.LargeChange := PageSize;
  invalidate;
end;

//这个子程序特别要注意;
//当双击一个控件之后,双击事件事先出发,然后又再触发 MouseDown 事件.
//这将导致 FMouseDown 不能正确释放.
//因此必须加入 ssDouble in Shift 的判断
procedure TDefineGUICtrlSave.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: integer;
begin
  if (Button = mbLeft) and not(ssDouble in Shift) then
  begin
    BeginUpdate;
    i := ItemAtPoint(x, y);
    try
      if i > -1 then
      begin
        FDownShift := shift;
        FMouseDown := true;
        SetItemIndex(i);
        //=================================
        if FMultiSelect then
        begin
          //备份状态:
          SaveBakSelectState;
          if (ssShift in Shift) and (FDownItem > -1) then
          begin
            FSelectList.UnSelectAll;
            FSelectList.SelectSome(i,FDownItem);
          end
          else
          if ssCtrl in shift then
          begin
            //设置鼠标拖动动作类型:
            FCtrlIsClear := FSelectlist.Bits[i];
            FSelectList.ChangeSelect(i);
            SetMouseDownItem(i);
          end
          else
          begin
            FSelectList.UnSelectAll;
            FSelectList.Select(i);
            SetMouseDownItem(i);
          end;
        end
        else
          SetMouseDownItem(i);
        //===============================
        //Link:
        MouseDownItem(i);
      end;
    finally EndUpdate; if i > -1 then invalidate; end;
    if not Focused then SetFocus;  // SetFocus
  end;
  inherited MouseDown(Button, shift, x, y);;
end;

procedure TDefineGUICtrlSave.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if FMouseDown then SetMouseItem(ItemAtY(Y))
  else SetMouseItem(ItemAtPoint(x, y));
  //======================================
  if FMouseDown then
  begin
    i := ItemAtY( y);
    if (y < FWorkRect.Top) and (i < TopIndex) then
      i := TopIndex
    else
    if (y > FWorkRect.Bottom) then
    begin
      i := TopIndex + GetPageSize - 1;
      if (i >= count) and (count > 0) then i := count -1;
    end;

    if (i > -1) and (FMoveItem <> i) then
    begin
      SetItemIndex(i);
      if FMultiSelect then
      begin
        if ssCtrl in FDownShift then
        begin
          LoadBakSelectState;
          if FCtrlIsClear then
            FSelectList.UnSelectSome(FDownItem, i)
          else FSelectList.SelectSome(FDownItem, i);
        end
        else
        begin
          FSelectList.UnSelectAll;
          FSelectList.SelectSome(FDownItem, i);
        end;
      end;
    end;
    FMoveItem := i;

    if Y - FWorkRect.Top < 0 then   //鼠标在上面位置
    begin
      if Y - FWorkRect.Top < -30 then
        SetMouseChangePage(cpDecMax)
      else
      if Y - FWorkRect.Top < -15 then
        SetMouseChangePage(cpDecNormal)
      else
        SetMouseChangePage(cpDecMin);
    end
    else
    if Y  > FWorkRect.Bottom then
    begin
      if Y - FWorkRect.Bottom > 30 then
        SetMouseChangePage(cpAddMax)
      else
      if Y - FWorkRect.Bottom > 15 then
        SetMouseChangePage(cpAddNormal)
      else
        SetMouseChangePage(cpAddMin);
    end
    else   SetMouseChangePage(cpNone);  //关闭

  end;
  inherited MouseMove(shift, x, y);
end;

procedure TDefineGUICtrlSave.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    //注意关闭 Timer:
    SetMouseChangePage(cpNone) ;
    
    if FMultiSelect then
      FBakList.Size := 0;
    FDownShift := [];
    FMouseDown := false;
    //link:
    if FMouseItem <> -1 then
      MouseUpItem(FMouseItem);
    if FMouseItem = FDownItem then
      ItemClick(FDownItem);
  end;
  inherited MouseUp(button, shift, x, y);
end;

procedure TDefineGUICtrlSave.ItemClick(const Index: integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(self, Index);
end;

procedure TDefineGUICtrlSave.MouseDownItem(const Index: integer);
begin

end;

procedure TDefineGUICtrlSave.MouseUpItem(const Index: integer);
begin

end;

procedure TDefineGUICtrlSave.SetItemIndex(Value: integer);
begin
  if value < -1 then value := -1
  else if value >= FCount then value := FCount - 1;
  if FItemIndex <> value then
  begin
    if not FMultiSelect then
    begin
      FSelectList.UnSelectALl;
      FSelectList.Select(Value);
    end;
    FItemIndex := Value;
    SetFocusItem(Value, false);
    if not Refreshing then
    begin
      if not ItemCanSee(FItemIndex) then
        ToSeeItem(FItemIndex) 
      else invalidate;
    end;
  end;
end;

procedure TDefineGUICtrlSave.ToSeeItem(Index: integer);
begin
  if FCount > 0 then
  begin
    if index < 0 then index := 0
    else if index >= FCount then index := FCount - 1;
    if not ItemCanSee(index) then
    begin
      if Index < FTopIndex then
        FVBar.DoScroll(smDec, True, TopIndex - index)
      else
        FVBar.DoScroll(smAdd, true, index - PageSize - FTopIndex + 1);
    end;
  end;
end;

function TDefineGUICtrlSave.ItemCanSee(const Index: integer): boolean;
begin
  result := false;
  if count > -1 then
  begin
    if IsNoStandardSize then
    begin
      result := (Index >= TopIndex) and
       ((Index - GetTopIndex) * FItemHeight <
          (FWorkRect.Bottom - FWorkRect.Top));
    end
    else
    begin
      result := (Index >= TopIndex) and
       ((Index - GetTopIndex) * FItemHeight <
          (FWorkRect.Bottom - FWorkRect.Top));
    end;
  end;
end;

function TDefineGUICtrlSave.ItemAtY(const y: integer): integer;
begin
  result := -1;
  if (y > FWorkRect.top) and (Y < FWorkRect.Bottom) then
    result := FTopIndex + (y - FWorkRect.Top) div FItemHeight;
  if result >= FCount then result := -1;
end;



function TDefineGUICtrlSave.GetTopIndex: integer;
begin
  result := FTopIndex;
end;

procedure TDefineGUICtrlSave.SetTopIndex(Value: integer);
begin
  if not VbarCanSee then value := 0
  else
  if (Count + 1 - value) < GetPageSize then
    value := count - GetPageSize + 1;
    
  if Value < 0 then value := 0;
  if value <> FTopIndex then
  begin
    FTopIndex := value;
    if FVbar.Position <> value then FVbar.Position := value;
    if not Refreshing then Invalidate;
  end;
end;

function TDefineGUICtrlSave.IsNoStandardSize: Boolean;
begin
  result := (FWorkRect.Bottom - FWorkRect.Top) mod ItemHeight > 0;
end;

procedure TDefineGUICtrlSave.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseDown := false;
    FDownItem := -1;
    FMouseItem := -1;
  end;
end;

procedure TDefineGUICtrlSave.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
end;

procedure TDefineGUICtrlSave.SetMouseItem(const Index: Integer);
var
  b: integer;
begin
  if FMouseItem <> Index then
  begin
    b := FMouseItem;
    FMouseItem := Index;
    if (b > -1) and (b < Count) then
      MouseLeaveItem(b);
    if (FMouseItem > -1) and (FMouseItem < Count) then
      MouseEnterItem(FMouseItem);
  end;
end;

procedure TDefineGUICtrlSave.SetFocusItem(const Value: integer; const DoRePaint:boolean);
begin
  if Value <> FFocusItem then
  begin
    FFocusItem := Value;
    if not Refreshing and DoRePaint then invalidate;
  end;
end;

procedure TDefineGUICtrlSave.BeginUpdate;
begin
  FRefreshing := true;
end;

procedure TDefineGUICtrlSave.EndUpdate;
begin
  FRefreshing := false;
end;

procedure TDefineGUICtrlSave.SimpleSetItemIndex(Value: integer);
begin
  if value < -1 then value := -1
  else if value >= FCount then value := FCount - 1;
  if FItemIndex <> value then
    FItemIndex := Value;
end;

procedure TDefineGUICtrlSave.SetMouseDownItem(const Value: Integer);
begin
  if Value <> FDownItem then
  begin
    FDownItem := Value;
    if FDownItem < -1 then FDownItem := -1
    else if FDownItem > FCount - 1 then FDownItem := FCount - 1;
  end
end;

//减少闪烁:
procedure TDefineGUICtrlSave.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  inherited;
//  Message.Result := 1;
end;

procedure TDefineGUICtrlSave.LoadBakSelectState;
var
  i: integer;
begin
  FSelectList.Size :=  FBakList.Size;
  if FBakList.Size > 0 then
    for i := 0 to FBakList.size -1 do
      FSelectList.Bits[i] := FBakList.Bits[i];
end;

procedure TDefineGUICtrlSave.SaveBakSelectState;
var
  i: integer;
begin
  FBakList.Size := FSelectList.Size;
  if FBakList.Size > 0 then
    for i := 0 to FSelectList.size -1 do
      FBakList.Bits[i] := FSelectList.Bits[i];
end;

procedure TDefineGUICtrlSave.DrawItem(Cav: TCanvas; const Index: Integer;
  const R: TRect; const State: TListItemStates);
begin

end;

function TDefineGUICtrlSave.GetItemRectEx(const virtualTopIndex, index: integer): TRect;
var
  i: integer;
begin
  result := Rect(0,0,0,0);
  if (Index >= virtualTopIndex) and (index < FCount) and (virtualTopIndex > -1) and
    (virtualTopIndex < FCount) then
  begin
    i := FWorkRect.Top + FItemHeight * (Index - virtualTopIndex);
    result := Rect(FWorkRect.Left, i,FWorkRect.Right, i + FItemHeight);
  end;
end;

function TDefineGUICtrlSave.GetPageSize: integer;
begin
  result := (FWorkRect.Bottom - FWorkRect.Top) div FItemHeight;
  if IsNoStandardSize then
    Result := result + 1;
end;

procedure TDefineGUICtrlSave.SetActiveItem(const Value: integer);
begin
  FActiveItem := Value;
end;

function TDefineGUICtrlSave.GetItemRect(const Index: integer): TRect;
var
  i: integer;
begin
  result := Rect(0,0,0,0);
  if IsItem(index) then
  begin
    i := FWorkRect.Top + FItemHeight * (Index - TopIndex);
    result := Rect(FWorkRect.Left, i,FWorkRect.Right, i + FItemHeight);
  end;
end;

procedure TDefineGUICtrlSave.OnVbarScroll(Sender: TObject;
  const StartChange: boolean; Code: TIScrollCode; Mode: TScrollMode;
  const ChangeValue: integer);
var
  i: integer;
begin
  if Mode = smAdd then
    i := ChangeValue
  else i := - ChangeValue;
                 //Code = scTrackMove
  if StartChange then
  begin
    if code = scTrackMove then
    begin
      TopIndex := FTopIndex + i;
    end
    else AdjustSee(i);
  end
  else TopIndex := FTopIndex + i;

  //正在使用鼠标改变页面:
  case FMousePage of
    cpAddMin, cpAddNormal, cpAddMax:
    begin
      if FMultiSelect then
      begin
        if ssCtrl in FDownShift then
        begin
          LoadBakSelectState;
          if FCtrlIsClear then
            FSelectList.UnSelectSome(FDownItem, FTopIndex + PageSize - 1)
          else FSelectList.SelectSome(FDownItem, FTopIndex + PageSize - 1);
        end
        else
        begin
          FSelectList.UnSelectAll;
          FSelectList.SelectSome(FDownItem, FTopIndex + PageSize - 1);
        end;
      end;
      SetItemIndex(FTopIndex + PageSize - 1);
    end;
    cpDecMin, cpDecNormal, cpDecMax:
    begin
      if FMultiSelect then
      begin
        if ssCtrl in FDownShift then
        begin
          LoadBakSelectState;
          if FCtrlIsClear then
            FSelectList.UnSelectSome(FDownItem, FTopIndex)
          else FSelectList.SelectSome(FDownItem, FTopIndex);
        end
        else
        begin
          FSelectList.UnSelectAll;
          FSelectList.SelectSome(FDownItem, FTopIndex);
        end;
      end;
      SetItemIndex(FTopIndex);
    end;
  end;
end;

procedure TDefineGUICtrlSave.WMKILLFOCUS(var Message: TMessage);
begin
  if FMultiSelect then
    FBakList.Size := 0;
  FDownShift := [];
  FMouseDown := false;
  if not Refreshing then invalidate;
  inherited;
end;

procedure TDefineGUICtrlSave.WMSETFOCUS(var message: TMessage);
begin
  inherited;
  if not Refreshing then invalidate;
end;

procedure TDefineGUICtrlSave.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;      // DLGC_WANTARROWS 让 KeyDown 事件支持系统按键
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TDefineGUICtrlSave.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if FMousePage = cpNone then
  begin
    FWheel.WheelCount := FWheel.WheelCount + 1;

    FWheel.IsAdd := message.WheelDelta < 0;
    
    if FWheel.IsAdd then
      FVBar.DoScroll(smadd,true, FWheel.WheelCount * 3)
    else FVBar.DoScroll(smdec, true,FWheel.WheelCount * 3);
    
    if not FWheel.Wheeling then
    begin
      FWheel.Wheeling := true;
      StartTimer(C_WheelWaitTimerID, C_WheelWait);
    end;
  end;
  inherited;
end;

procedure TDefineGUICtrlSave.CloseTimer(const ID: integer);
begin
  KillTimer(handle, ID);
end;

procedure TDefineGUICtrlSave.StartTimer(const ID, interval: integer);
begin
  SetTimer(handle, ID, interval,nil);
end;

procedure TDefineGUICtrlSave.OnTimer(var Msg: TWMTimer);
begin
  //鼠标滑轮改变页面事件:
  if msg.TimerID = C_WheelWaitTimerID then
  begin
    CloseTimer(C_WheelWaitTimerID);
    FWheel.Wheeling := false;
    FWheel.WheelCount := 0;
  End
  else   //鼠标改变页面事件:
  if msg.TimerID = C_MouseChangePageTimerID then
  begin
    case FMousePage of
      cpNone: CloseTimer(C_MouseChangePageTimerID) ;
      cpAddMin, cpAddNormal, cpAddMax:
        FVbar.DoScroll(smAdd,false,1)
    else
      FVbar.DoScroll(smDec,false,1)
    end;
  end;
end;

procedure TDefineGUICtrlSave.SetMouseChangePage(const Value: TMouseChangePage);
  function GetInterval(const value: TMouseChangePage): Integer;
  begin
    result := -1;
    case value of
      cpAddMin, cpDecMin: result := 100;
      cpAddNormal , cpDecNormal: result := 50;
      cpAddMax, cpDecMax: result := 10;
    end;
  end;
begin
  if value <> FMousePage then
  begin
    if FMousePage = cpNone then
    begin
      case value of    //这儿需要修改:
        cpAddMin, cpAddNormal, cpAddMax:
        begin
          FVBar.DoScroll(smAdd, true, 1);
        end;

        cpDecMin, cpDecNormal, cpDecMax:
        begin
          FVBar.DoScroll(smDec, true, 1);
        end;
      end;
    end;
    FMousePage := Value;
    if value = cpNone then
      CloseTimer(C_MouseChangePageTimerID)
    else SetTimer(handle, C_MouseChangePageTimerID, GetInterval(value), nil);
  end;
end;

procedure TDefineGUICtrlSave.DrawBitMap(bmp: TBitmap; BeginItem,
  EndItem: integer);
var
  i: integer;
  r: TRect;
  state:TListItemStates;
begin
  if BeginItem < 0 then BeginItem := 0;
  if EndItem >= FCount then EndItem := FCount - 1;
  if BeginItem < EndItem then
  begin
    bmp.Width := FWorkRect.Right + 1;
    bmp.Height := (EndItem - BeginItem + 2) * ItemHeight;
    FBmp.Canvas.Brush.Color := Color;
    FBmp.Canvas.FillRect(Rect(0, 0, FBmp.Width, Fbmp.Height));
    for i := BeginItem to EndItem do
    begin
      R := GetItemRectEx(BeginItem,i);
      //Item tate:
      state := [];
      if Selected[i] then State := state + [isSelected];
      if i = FActiveItem then state := state + [isActive];
      if (i = FFocusItem) and Focused then state := state + [isFocused];
      if not Enabled then
      begin
        state := state + [isDisabled];
      end
      else if FMouseDown then
      begin
        if (FDownItem = i) and (i = FMouseItem) then
          state := state + [isDown];
      end
      else if FMouseItem = i then State := state + [isUp];

      // Run
      if FOwnerDraw and Assigned(FOnItemDraw) then
        FOnItemDraw(bmp.canvas, i, r, State)
      else
        DrawItem(bmp.canvas, i, r, State);
    end;

  end;
end;

procedure TDefineGUICtrlSave.CMFONTCHANGED(var msg: TMessage);
begin
  inherited;
  FBmp.Canvas.Font.Assign(Font);
  canvas.Font.Assign(font); 
end;


procedure TDefineGUICtrlSave.AdjustSee(value: integer);
var
  i: integer;
begin
  if (FCount > 0) and showing then
  begin
    value := TopIndex + value;
    if value < 0 then value := 0
    else if value >= FCount then value := FCount - 1;

    //如果页面可视范围小于 Itemheight 那么直接转移到该项目,不作动画:
    if ItemHeight <= (GetPageSize * ItemHeight) then
    begin

      //注意此处, While 语句为了截去过长的 Item 数量.
      i := value;
      if i > TopIndex then
      begin
        while ItemHeight * (i - TopIndex) > (FWorkRect.Bottom - FWorkRect.Top) do
          Dec(i);
      end
      else
      begin
        while ItemHeight * (TopIndex - i) > (FWorkRect.Bottom - FWorkRect.Top) do
          inc(i);
      end;

      if value > TopIndex then
      begin
        DrawBitMap(FBMP, TopIndex, i + PageSize);
        CopyBit((i - TopIndex) * ItemHeight,0 ,FBmp.Canvas.Handle, true );
      end
      else
      if value < topindex then
      begin
       DrawBitMap(FBmp, i, TopIndex + PageSize);
        CopyBit((TopIndex - i) * ItemHeight ,
          0,FBmp.Canvas.Handle , false );
      end;
    end;
    TopIndex := value;
  end;
end;

function TDefineGUICtrlSave.VbarCanSee: boolean;
begin
  result := FVbar.Visible;
end;


//复制,滚动动画 DC 
procedure TDefineGUICtrlSave.CopyBit(const EndY, startY: Integer;const Source: HDC; forward: boolean);
var
  i: integer;                 
  int: integer;
  j: integer;
  k: double;
begin
  // Sleep 时间间隔:
  int := C_MaxInterval div (EndY - StartY);

  //如果时间间隔小于 整体动画时间 除以 最大动画贞数量,那么调整它
  if int < (C_MaxInterval div C_SleepMaxCount) then
    int := (C_MaxInterval div C_SleepMaxCount);

  // 设置 k 为 每一贞滚动的象素:
  k := (EndY - startY) / C_SleepMaxCount;
  if k < 1 then k := 1;

  //设置 j 为滚动贞数量,如果 j 大于 最大贞数量,那么设置为最大贞数量:
  if Endy - startY > C_SleepMaxCount then
    j := C_SleepMaxCount
   else j := EndY - startY ;
   
  if forward then
  begin
    for i := startY to startY + j do
    begin
      BitBlt(canvas.Handle,FWorkRect.Left,FWorkRect.Top,  FWorkRect.Right - FWorkRect.Left, FWorkRect.Bottom - FWorkRect.top,
        Source, FWorkRect.Left, FWorkRect.top + trunc(i * k),SRCCOPY);
      Sleep(int);
    end;
  end
  else
  begin
    for i := (starty + j) downto Starty do
    begin
      BitBlt(canvas.Handle, FWorkRect.Left,FWorkRect.Top, FWorkRect.Right - FWorkRect.Left, FWorkRect.Bottom - FWorkRect.Top,
        Source, FWorkRect.Left ,FWorkRect.top + trunc(i * k) ,SRCCOPY);
      Sleep(int);
    end;
  end;
end;


//请注意,这儿描述了 KeyDown 事件中需要注意的地方:

  //KeyDown 事件中附带的 Temp 子程序是为了处理"键盘改变页面"的时候,
  //决定是否显示动画的.

  //最重要的地方是这儿:

  //因为 Temp 是计算参数是否小于 Count 来决定是否执行自身代码的,
  //所以,如果在非标准项目可视状态下,必须自己增加一个 FVbar 的 Position,
  //以让最后一个项目,也就是只显示了一半的项目显示出来
  //在 KeyDown 子程序中的很多地方都标注了 "补丁" 字样,那里就是需要注意的地方.

procedure TDefineGUICtrlSave.KeyDown(var Key: word; Shift: TShiftState);
  procedure Temp(index: integer);
  var old: integer;
  begin
    if index < 0 then index := 0
    else if index >= FCount then index := FCount - 1;
    if index <> ItemIndex then
    begin
      if not MultiSelect then
      begin
        FSelectList.UnSelectAll;
        FSelectList.Select(index); 
      end;
      old := FItemIndex;
      FItemIndex := index;
      SetFocusItem(index, false);
      if index > old then
        FVBar.DoScroll(smAdd, false , index - PageSize - TopIndex + 1)
      else
        FVbar.DoScroll(smDec, false, FTopIndex - index);
    end;
  end;
var
  OldIndex: integer;
begin
  OldIndex := FItemIndex;
  case Key of
    VK_UP, VK_LEFT:
    begin
      if (FItemIndex > 0) and (Count > 0) then
      begin
        if ItemCanSee(FItemIndex - 1) then
          SetItemIndex(FItemIndex - 1)
        else
        begin
          if FKeyPage <> kfup then
          begin
            Fkeypage := kfup;
            SetItemIndex(FItemIndex - 1);
          end
          else
          begin
            temp(FItemIndex - 1);
          end;
        end;
        if FMultiSelect then
        begin
          if (ssShift in Shift) then
          begin
            if FDownItem > -1 then
              FSelectlist.SelectSome(FItemIndex,FDownItem)
            else
              FDownItem := FItemIndex;
          end
          else
          begin
            FDownItem := FItemIndex;
            FSelectList.UnSelectAll;
            FSelectList.Select(FItemIndex);
          end;
          Invalidate;
        end;
      end;
    end;
    VK_DOWN, VK_RIGHT:
    begin
      if (Count > 0) then
      begin
        if ItemCanSee(FItemIndex + 1) then
          SetItemIndex(FItemIndex + 1)
        else
        begin
          if FKeyPage <> kfDown then
          begin
            Fkeypage := kfDown;
            if (FItemIndex + 1 = Count) and IsNoStandardSize then
              FVBar.DoScroll(smAdd, true, 1)
            else
            SetItemIndex(FItemIndex + 1);
          end
          else
          begin
            temp(FItemIndex + 1);
          end;
        end;
        if FMultiSelect then
        begin
          if (ssShift in Shift) then
          begin
            if FDownItem > -1 then
              FSelectlist.SelectSome(FItemIndex,FDownItem)
            else
              FDownItem := FItemIndex;
          end
          else
          begin
            FDownItem := FItemIndex;
            FSelectList.UnSelectAll;
            FSelectList.Select(FItemIndex);
          end;
          Invalidate;
        end;
        //补丁:
        if (FItemIndex = FCount - 1) then
           FVBar.DoScroll(smAdd, true, 1); 
      end;
    end;
    VK_PRIOR:
      if FVBar.Enabled then
      begin
        if ItemCanSee(FItemIndex - (PageSize - 1)) then
          SetItemIndex(FItemIndex - (PageSize - 1))
        else
        begin
          if FKeyPage <> kfPRIOR then
          begin
            Fkeypage := kfPRIOR;
            SetItemIndex(FItemIndex - (PageSize - 1));
          end
          else
          begin
            temp(FItemIndex - (PageSize - 1));
          end;
        end;
        if (ssShift in Shift) and FMultiSelect then
        begin
          if FDownItem > -1 then
            FSelectList.SelectSome(FItemIndex,FDownItem)
          else FDownItem := FItemIndex;
        end
        else
        begin
          FDownItem := FItemIndex;
          FSelectList.UnSelectAll;
          FSelectList.Select(FItemIndex);
        end;
        invalidate;
      end;
    VK_NEXT:
      if FVBar.Enabled then
      begin
        if ItemCanSee(FItemIndex + PageSize) then
          SetItemIndex(FItemIndex + PageSize)
        else
        begin
          if FKeyPage <> kfNext then
          begin
            Fkeypage := kfNext;
            SetItemIndex(FItemIndex + PageSize);
          end
          else
          begin
            temp(FItemIndex + PageSize);
          end;
        end;
        if (ssShift in Shift) and FMultiSelect then
        begin
          if FDownItem > -1 then
            FSelectList.SelectSome(FItemIndex,FDownItem)
          else FDownItem := FItemIndex;
        end
        else
        begin
          FDownItem := FItemIndex;
          FSelectList.UnSelectAll;
          FSelectList.Select(FItemIndex);
        end;
        //补丁:
        if (FItemIndex = FCount - 1) then
           FVBar.DoScroll(smAdd, true, 1); 
        invalidate;
      end;
    VK_END:
      if FCount > 0 then
      begin
        SetItemIndex(FCount -1);
        if FMultiSelect and (ssShift in Shift) then
        begin
          if FDownItem > -1 then
            FSelectList.SelectSome(FItemIndex,FDownItem)
          else FDownItem := FItemIndex;
        end
        else
        begin
          FDownItem := FCount - 1;
          FSelectList.UnSelectAll;
          FSelectList.Select(FItemIndex);
          invalidate;
        end;
        //补丁:
        FVBar.DoScroll(smAdd, true, 1); 
      end;
    VK_HOME:
      if FCount > 0 then
      begin
        SetItemIndex(0);
        if FMultiSelect and (ssShift in Shift) then
        begin
          if FDownItem > -1 then
            FSelectList.SelectSome(FItemIndex,FDownItem)
          else FDownItem := FItemIndex;
        end
        else
        begin
          FDownItem := 0;
          FSelectList.UnSelectAll;
          FSelectList.Select(FItemIndex);
          invalidate;
        end;
      end;
  end;
  if OldIndex <> FItemIndex then
    Click;
  inherited;
end;


procedure TDefineGUICtrlSave.KeyUp(var Key: Word; shift: TShiftState);
begin
  //复原 FKeyChangePage State
  case Key of
    VK_UP,
    VK_LEFT,
    VK_DOWN,
    VK_RIGHT,
    VK_PRIOR,
    VK_NEXT: FKeyPage := kfNone;
  end;
  inherited;
end;

procedure TDefineGUICtrlSave.CalcSizeOfWoekRect(var R: TRect);
begin
end;

procedure TDefineGUICtrlSave.UpdateWorkRect;
begin
  FVBar.Left := width - FVbar.Width;
  FWorkRect := Rect(0, 0, FVBar.Left , height);
  if not FVBar.Enabled then
    FWorkRect.Right := width;
    
  CalcSizeOfWoekRect(Fworkrect);

  if FWorkRect.Bottom < FWorkRect.Top then
    FWorkRect.Bottom := FWorkRect.Top;

  if FWorkRect.Right < FWorkRect.Left then
    FWorkRect.Right := FWorkRect.Left;  

  FVBar.Left := FWorkRect.Right;
  FVBar.Top := FWorkRect.Top;
  FVBar.Height := FWorkRect.Bottom - FWorkRect.Top;

end;

procedure TDefineGUICtrlSave.DblClick;
begin
  inherited;
  if FMouseItem = FDownItem then
    If Assigned(FOnItemDlbClick) then
      FOnItemDlbClick(self,FDownItem); 
end;

procedure TDefineGUICtrlSave.OnVbarEnabledChange(Sender: TObject);
begin
  UpdateWorkRect;
end;

procedure TDefineGUICtrlSave.Clear;
begin
  FDownItem := -1;
  FMouseItem := -1;
  FItemIndex := -1;
  FFocusItem := -1;
  Count := 0;
  invalidate;
end;

procedure TDefineGUICtrlSave.UpdatePageSizeOfVbar;
var
  i: integer;
begin
  i := PageSize ;
  if i > 0 then FVbar.PageSize := i
  else FVBar.pageSize := 0;
end;

procedure TDefineGUICtrlSave.SetOwnerDraw(const Value: Boolean);
begin
  if Value <> FOwnerDraw then
  begin
    FOwnerDraw := Value;
    FVbar.OwnerDraw := value;
    invalidate;
  end;
end;

function TDefineGUICtrlSave.GetOnDrawScrollBar: TScrollDrawEvent;
begin
  result := FVbar.OnDrawControl;
end;

procedure TDefineGUICtrlSave.SetOnDrawScrollBar(const Value: TScrollDrawEvent);
begin
  FVBar.OnDrawControl := value;
end;



{ TDefineGUIListBoxString }  //*********************************************


function TDefineGUICtrlString.AddObject(const S: string; AObject: TObject): Integer;
begin
  inherited AddObject(s, AObject);
  FControl.Add;
  result := FControl.Count;
end;

procedure TDefineGUICtrlString.Clear;
begin
  inherited Clear;
  FControl.clear;
end;

procedure TDefineGUICtrlString.Delete(Index: Integer);
begin
  inherited Delete(index);
  if not FMoving then FControl.Delete(Index);
end;

procedure TDefineGUICtrlString.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  inherited InsertObject(index, s, AObject);
  if not FMoving then FControl.Insert(index);
end;

procedure TDefineGUICtrlString.Move(CurIndex, NewIndex: Integer);
begin
  FMoving := true;
  try
    inherited MOVE(CurIndex, NewIndex);
    //这儿首先调用 Inherited Move ;
    //当 CurIndex 等参数发生错误的时候,FControl.Move 就不会继续执行:
    FControl.Move(CurIndex, NewIndex);
  finally FMoving := false; end;
end;

procedure TDefineGUICtrlString.Put(Index: Integer; const S: string);
begin
  inherited Put(index,s);
  FControl.Put(index);
end;

procedure TDefineGUICtrlString.SetListControl(const aListControl: TDefineGUICtrlList);
begin
  FControl := aListControl;
end;


procedure TDefineGUICtrlString.SetTextStr(const Value: string);
begin
  inherited;
  FControl.TopIndex := 0;
end;


{ TDefineGUIListBox } //*****************************************************

procedure TDefineGUIListBox.CMFONTCHANGED(var msg: TMessage);
begin
  inherited;
  UpdateItemheight;
end;


constructor TDefineGUIListBox.Create(AOwner: TComponent);
begin          
  inherited Create(AOwner);
  FAutoItemHeight := true;
  Color := clWhite;
  FItems := TDefineGUICtrlString.Create ;
  FItems.SetListControl(Self);
end;

destructor TDefineGUIListBox.Destroy;
begin
  if FItems <> nil then
    FreeAndNil(FItems);
  inherited;
end;

procedure TDefineGUIListBox.DrawItem(Cav: TCanvas; const Index: Integer;
  const R: TRect; const State: TListItemStates);
    Function GetChanged(Clr:TColor):TColor;
    var
      r,g,b:integer;
    begin
      clr := ColorToRGB(clr);
      r := Clr and $000000FF;
      g := (Clr and $0000FF00) shr 8;
      b := (Clr and $00FF0000) shr 16;
      r := 255 - r;
      g := 255 - g;
      b := 255 - b;
      Result := RGB(r, g, b);
    end;
var
  flags: Cardinal;
  nr: TRect;
begin
  inherited ;
  if GUIStyle <> lcgNone then
    nr := rect(r.Left+ 1,r.Top + 1,r.Right -1,r.Bottom -1)
  else
    nr := r;
  if isDisabled in state then
    Cav.Font.Color := clGradientInactiveCaption
  else
  if (isSelected in State) and (GUIStyle = lcgNone) then
    Cav.Font.Color := GetChanged(Cav.Font.Color)
  else
  if GUIStyle = lcglowered then begin
    if isfocused in State then
      cav.Font.Color := $0000C8FF
    else
    if isactive in State then
      cav.Font.Color := $003C9DFF
    else
      Cav.Font.Color := $00B5BBC4
  end
  else
  Cav.Font.Color := font.Color;
  Flags := DT_SINGLELINE or DT_VCENTER or DT_Left or DT_END_ELLIPSIS;
  DrawText(Cav.Handle,PChar(FItems[index]),length(FItems[index]),nr,flags);
  if (isFocused in state) and (GUIStyle = lcgNone) then Cav.DrawFocusRect(r);
end;

function TDefineGUIListBox.GetItems: TStrings;
begin
  result := FItems;
end;

procedure TDefineGUIListBox.SetAutoItemHeight(const Value: Boolean);
begin
  if FAutoItemHeight <> Value then
  begin
    FAutoItemHeight := Value;
    UpdateItemheight;
  end;
end;

procedure TDefineGUIListBox.SetItems(const Value: TStrings);
begin
  if FItems <> value then
  begin
    FItems.Assign(Value);
    topindex := 0;        
  end;
end;

procedure TDefineGUIListBox.UpdateItemheight;
var
  i: integer;
begin
  // 增加 showing 判断,用于避免控件在没有 Parent 的时候执行 TextHeight ,而
  //导致的错误
  if FAutoItemHeight and showing then
  begin
    Canvas.Font.Assign(Font);
    i := canvas.TextHeight('H');
    if GUIStyle = lcgFlat then
       inc(i,4)
    else
    if GUIStyle = lcglowered then
       inc(i, 4);
    SetItemHeight(i);
    if not Refreshing then invalidate;
  end;
end;

procedure TDefineGUIListBox.CMSHOWINGCHANGED(var msg: TMessage);
begin
  inherited;
  UpdateItemheight;
end;

function TDefineGUIListBox.GetCount: integer;
begin
  result := count;
end;

{ TDefineGUICtrlList }  //*****************************************


procedure TDefineGUICtrlList.CalcSizeOfWoekRect(var R: TRect);
begin
  case FGUIStyle of
    lcgFlat:
          r := Rect(r.Left + 2,r.Top + 2, r.Right - 2,r.Bottom -2);
    lcgLowered,
    lcgNone:
          r := Rect(r.Left + 1,r.Top + 1, r.Right - 1,r.Bottom -1);
  end;
end;

procedure TDefineGUICtrlList.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    Invalidate;
  end;
end;

procedure TDefineGUICtrlList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseIn then begin
     FMouseIn := False;
     Invalidate;
  end;
end;

constructor TDefineGUICtrlList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemSelectColor   := DefaultItemSelectColor;
  FItemBorderColor   := DefaultBorderColor;
  FItemBrightColor   := DefaultItemBrightColor;
  FItemColor         := DefaultItemColor;
  FItemSpaceColor    := DefaultItemSpaceColor;
  FFocusColor        := clWhite;
  FFlatColor         := DefaultFlatColor;
  FGUIStyle          := lcgFlat;
  VBar.OnDrawControl := OnVBarDrawControl;
  VBar.OwnerDraw     := true;
end;

destructor TDefineGUICtrlList.Destroy;
begin
  VBar.OnDrawControl := nil;
  inherited Destroy;
end;

procedure TDefineGUICtrlList.DrawItem(Cav: TCanvas; const Index: Integer;
  const R: TRect; const State: TListItemStates);
var
  re: Trect;
begin
  case FGUIStyle of
    lcgLowered:
    begin
      re := R;
      cav.Pen.Style := psSolid;
      if isselected in state then begin
        cav.Brush.Color := fItemSelectColor;
        cav.FillRect(R);
        Frame3D(Cav,re,fItemBrightColor, FItemBorderColor,1);
      end else if isactive in state then begin
        cav.Brush.Color := fItemSelectColor;
        cav.FillRect(R);
        Frame3D(Cav,re,cav.Brush.Color, FItemBorderColor,1);
      end else begin
        cav.Brush.Color := fItemColor;
        cav.FillRect(R);
        Frame3D(Cav,re,fItemBrightColor,FItemBorderColor,1);
      end;
    end;
    lcgFlat:
    begin
      if isselected in State then begin
        Cav.Pen.Style   := psSolid;
        cav.Brush.Color := fItemSelectColor;
        cav.Pen.Color   := fItemBorderColor;
        cav.Rectangle(R);
      end else if isActive in state then begin
        Cav.Pen.Style := psSolid;
        cav.Brush.Color := $009CDEF7;
        cav.Pen.Color := $008396A0;
        cav.Rectangle(R);
      end else begin
        Cav.Pen.Style := psclear;
        cav.Brush.Color := color;
        cav.FillRect(R);
      end;
    end;
    lcgNone:
    begin
      if isSelected in State then
         cav.Brush.Color := clActiveCaption
      else Cav.Brush.Color := color;
      Cav.FillRect(R);
    end;
  end;
end;

function TDefineGUICtrlList.GetMouseIn: boolean;
begin
  result := FMouseIn;
end;

procedure TDefineGUICtrlList.OnVBarDrawControl(Cav: TCanvas;
  const Typ: TDrawScrollBar; const R: TRect; const State: TButtonState);
var
  i: integer;
  re: Trect;
begin
  re := R;
  case FGUIStyle of
  lcgLowered: begin
   Cav.Brush.Style := bsSolid;
   if (Typ = dsspaceright) or (Typ = dsspaceleft) then begin
     if State = bsdown then
        Cav.Brush.Color := $006E6E6E
     else
        cav.Brush.Color := $00B5BBC4 ;
     Cav.FillRect(R)
   end else begin
     if (state = bsup) or (state = bsDown) then
        cav.Brush.Color := fItemSelectColor
     else
        cav.Brush.Color := fItemColor;
     cav.FillRect(R);
     if state = bsdown then
       Frame3D(cav,re,FItemBorderColor,FItemBorderColor,1)
     else
       Frame3D(cav,re,fItemBrightColor,FItemBorderColor,1);

     Cav.Pen.Style := psSolid;
     cav.Pen.Color := $00B5BBC4;

     if Typ = dsLeftBtn then begin
       if FVBar.IsVertical then
          FVBar.DrawArrows(cav,daTop,re)
       else
          FVBar.DrawArrows(cav,daLeft,re);
     end else if Typ = dsRightBtn then begin
       if FVBar.IsVertical then
          FVBar.DrawArrows(cav,daBottom,re)
       else
          FVBar.DrawArrows(cav,daRight,re);
     end else begin
       Cav.Pixels[R.Right div 2-3,R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
       Cav.Pixels[R.Right div 2,  R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
       Cav.Pixels[R.Right div 2+3,R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
     end;
   end;
  end;

  lcgFlat: begin
   cav.Brush.Color := FVBar.color;
   cav.Brush.Style := bsSolid;
   cav.Pen.Style   := psSolid;
   if (Typ = dsSpaceRight) or (Typ = dsSpaceLeft)  then begin
     if State = bsdown then
        Cav.Brush.Color := clBackground;
     Cav.FillRect(R);
     cav.Pen.Color :=  $00C9C2C2;
     if Vbar.IsVertical then begin
        cav.MoveTo(R.Left,R.Top);
        cav.LineTo(R.Left,R.Bottom);
        cav.MoveTo(R.Right-1,R.Top);
        cav.LineTo(R.Right-1,R.Bottom);
     end;
   end else begin
     if state = bsdown then begin
        Cav.Pen.Color   := fItemBorderColor ;
        cav.Brush.Color := fItemSpaceColor;
     end else if State = bsup then begin
        cav.Pen.Color   := clMoneyGreen ;
        cav.Brush.Color := fItemSpaceColor;
     end else
        cav.Pen.Color := fItemBorderColor;
     cav.Rectangle(R);
   end;
   if state <> bsExclusive then
      Cav.Pen.Color := clInfoBk;
   cav.Pen.Style := psSolid;

   if Typ = dsLeftBtn then begin
     if FVBar.IsVertical then
        FVBar.DrawArrows(cav,daTop,re)
     else
        FVBar.DrawArrows(cav,daLeft,re);
   end else if Typ = dsRightBtn then begin
     if FVBar.IsVertical then
        FVBar.DrawArrows(cav,daBottom,re)
     else
        FVBar.DrawArrows(cav,daRight,re);
   end else if (Typ = dsTrack) then begin
     Cav.Pixels[R.Right div 2-3,R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
     Cav.Pixels[R.Right div 2,  R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
     Cav.Pixels[R.Right div 2+3,R.Top + ((R.Bottom - R.Top) div 2)] := cav.Pen.Color;
   end;
  end;

  lcgNone: begin
   cav.Brush.Color := FVBar.color;
   cav.Brush.Style := bsSolid;
   if (Typ = dsspaceright) or (Typ = dsspaceleft)  then begin
     if State = bsdown then
        cav.brush.Color := clBlack;
     cav.FillRect(R) ;
   end else begin
     if State = bsdown then
        i := BDR_SUNKENOUTER
     else
        i := BDR_RAISEDINNER;
     cav.FillRect(R);
     DrawEdge(cav.Handle,re, i, BF_RECT);
     if State = bsdown then
        InflateRect(re,-3,-3);
     cav.Pen.Color := clblack;
     if Typ = dsLeftBtn then begin
       if FVBar.IsVertical then
          FVBar.DrawArrows(cav,daTop,re)
       else
          FVBar.DrawArrows(cav,daLeft,re);
     end else if Typ = dsrightbtn then begin
       if FVBar.IsVertical then
          FVBar.DrawArrows(cav,daBottom,re)
       else
          FVBar.DrawArrows(cav,daRight,re);
     end;
   end;
  end;
  end;
end;

procedure TDefineGUICtrlList.Paint;
var re: TRect;
begin
  inherited Paint;   //继承
  re := ClientRect;
  if (not(csDesigning in ComponentState) and
     (Focused or(MouseIn and not(Screen.ActiveControl is TDefineGUICtrlList)))) then
     Color := GUIFocusedColor
  else
     Color := GUIFlatColor;

  case FGUIStyle of
    lcgFlat:
    begin
      re := clientrect;
      canvas.Brush.Color := fItemBorderColor;
      FrameRect(canvas.Handle,ClientRect,canvas.brush.Handle);
      canvas.Brush.Color := color;
      re := Rect(re.Left + 1, re.Top + 1, re.Right - 1,re.Bottom - 1);
      FrameRect(canvas.Handle,re,canvas.brush.Handle);
    end;
    lcgLowered:
    begin
      canvas.Brush.color := FItemBorderColor;
      FrameRect(canvas.Handle,ClientRect,canvas.brush.Handle);
    end;
    lcgNone:
    begin
      DrawEdge(canvas.Handle,re, BDR_SUNKENOUTER, BF_RECT);
    end;
  end;
end;

procedure TDefineGUICtrlList.SetColors(const Index: Integer; const Value: TColor);
begin
 case Index of
   0:FItemSelectColor := Value;
   1:FItemBorderColor := Value;
   2:FItemBrightColor := Value;
   3:FItemColor       := Value;
   4:FItemSpaceColor  := Value;
   5:FFocusColor      := Value;
   6:FFlatColor       := Value;
 end;
 Invalidate;
end;

procedure TDefineGUICtrlList.SetGUIStyle(const Value: TListControlGUI);
begin
  if FGUIStyle <> Value then begin
     FGUIStyle := value;
     UpdateWorkRect;
     Perform(CM_SHOWINGCHANGED,0,0);  // 触发事件 ListBox UpdateItemHeight
     invalidate;
  end;
end;

{ TDefineTreeView }
constructor TDefineTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle  := ControlStyle - [csOpaque];
  ParentFont     := True;
  AutoSize       := False;
  Ctl3D          := False;
  BorderStyle    := bsNone;
  Width          := 185;
  Height         := 89;
  FFocusedColor  := clWhite;
  FBorderColor   := DefaultBorderColor;
  FFlatColor     := DefaultFlatColor;
  FParentColor   := True;
  FInterDrawing  := False;
end;

destructor TDefineTreeView.Destroy;
begin
  inherited Destroy;
end;

procedure TDefineTreeView.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
      RedrawBorder;
    end;
  end;
end;

procedure TDefineTreeView.CMSysColorChange(var Message: TMessage);
begin
  if FParentColor then
  begin
      if Parent <> nil then
         FFlatColor := TForm(Parent).Color;
  end;
  RedrawBorder;
end;

procedure TDefineTreeView.CMParentColorChanged(var Message: TWMNoParams);
begin
  if FParentColor then
  begin
      if Parent <> nil then
         FFlatColor := TForm(Parent).Color;
  end;
  RedrawBorder;
end;

procedure TDefineTreeView.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FBorderColor := Value;
    2: begin
       FFlatColor := Value;
       FParentColor := False;
       end;
  end;
  RedrawBorder;
end;

procedure TDefineTreeView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder;
  end;
end;

procedure TDefineTreeView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseIn := False;
  RedrawBorder;
end;

procedure TDefineTreeView.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  RedrawBorder;
end;

procedure TDefineTreeView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder;
end;

procedure TDefineTreeView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder;
end;

procedure TDefineTreeView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;

procedure TDefineTreeView.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TDefineTreeView.RedrawBorder(const Clip: HRGN = 0);
var ViewBorder:TBorderAttrib;
begin
  with ViewBorder do
  begin
    Ctrl := Self;
    BorderColor := ColorBorder;
    if Enabled then
    begin
       FlatColor   := ColorFlat;
       FocusColor  := ColorFocused;
    end
    else
    begin
       FlatColor   := clSilver;
       FocusColor  := clSilver;
    end;
    MouseState  := FMouseIn;
    DesignState := ComponentState;
    FocusState  := Focused;
    HasBars     := False;
  end;
  Color := DrawViewBorder(ViewBorder);
end;

function TDefineTreeView.GetItemsCount: Integer;
begin
  result := inherited Items.Count;
end;

procedure TDefineTreeView.Loaded;
begin
  inherited;
end;

{ TDefineListView }

constructor TDefineListView.Create(AOwner: TComponent);
begin
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
  FGroundPic      := TPicture.Create;
  FTransBit       := TBitmap.Create;
  inherited Create(AOwner);
  ParentFont         := True;
  AutoSize           := False;
  Ctl3D              := False;
  BorderStyle        := bsNone;
  FlatScrollBars     := True;
  Width              := 185;
  Height             := 89;
  FFocusedColor      := clWhite;
  FBorderColor       := DefaultBorderColor;
  FFlatColor         := DefaultFlatColor;
  FTitleFaceColor    := DefaultTitleFaceColor;
  FTitleCheckColor   := DefaultTitleCheckColor;
  FParentColor       := True;
  FGroundHas         := False;
  FGroundStretch     := False;
  FAllCheck          := False;
  FTransparent       := False;
  FHeaderHandle      := 0;
  FDefHeaderProc     := nil;
end;

destructor TDefineListView.Destroy;
begin
  if FHeaderHandle <> 0 then
     SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FDefHeaderProc));
  FreeObjectInstance(FHeaderInstance);
  FHeaderHandle  := 0;
  FDefHeaderProc := nil;
  FGroundPic.Free;
  FGroundPic     := nil;
  FTransBit.Free;
  FTransBit      := nil;
  OnCustomDraw   := nil;
  inherited Destroy;
end;

procedure TDefineListView.DrawTitle(Cnvs: TCanvas; Column: TListColumn; Active, Pressed: Boolean; R: TRect);
var
  BR, RA, CR: TRect;
  S: String;
  B: TBitMap;
  TX, TY, GX, GY: Integer;
begin
 if (RectWidth(R) <= 0) or (RectHeight(R) <= 0) then Exit;
 S := Column.Caption;
 B := TBitMap.Create;
 try
  B.Width  := RectWidth(R)+1;
  B.Height := RectHeight(R);
  BR := Rect(0, 0, B.Width, B.Height);
  with B.Canvas do
  begin
    if Pressed then begin
      if (not FCheckInBox)and(ColumnClick) then
         Brush.Color := BS_XP_BTNDOWNCOLOR
      else
         Brush.Color := FTitleFaceColor;
      if not(Column.Index = 0) then
         Inc(Br.Left);
      Dec(Br.Right);
    end else if Active then begin
       if (not FCheckInBox)and(ColumnClick) then
         Brush.Color := BS_XP_BTNACTIVECOLOR
       else
         Brush.Color := FTitleFaceColor;
    end else begin
       DrawFrame(B.Canvas, BR, FTitleFaceColor, FTitleFaceColor, 1);
       Brush.Color := FTitleFaceColor;// clBtnFace;
    end;
    FillRect(BR);
    if (Column.Index = 0)and(CheckBoxes) then
    begin
     RA := RECT(0,0,HeaderHeight,HeaderHeight);
     FillRect(RA);
     CR := RECT(RA.Left+1,RA.Top+1,RA.Right-1,RA.Bottom-1);
     // 画选定
     if AllCheck then
     begin
      DrawInCheck(B.Canvas,CR,FTitleCheckColor);
     end;
     BR := RECT(RA.Right+2,BR.Top,BR.Right,BR.Bottom);
    end;
    Frame3d(B.Canvas, CR, FTitleCheckColor, FTitleCheckColor, 2);
    Brush.Style := bsClear;
    Font.Assign(Self.Font);
    Font.Color  := clBtnText;
  end;
  if Assigned(FOnDrawTitle) then
     FOnDrawTitle(B.Canvas, Column, Pressed, Rect(0, 0, B.Width, B.Height))
  else with B.Canvas do begin
    Brush.Style := bsClear;
    Inc(BR.Left, 2); Dec(BR.Right, 2);
    if (SmallImages <> nil) and (Column.ImageIndex >= 0) and
       (Column.ImageIndex < SmallImages.Count) then
    begin
        CorrectTextbyWidth(B.Canvas, S, RectWidth(BR) - 4 - SmallImages.Width);
        GX := BR.Left;
        if S = Column.Caption then
         case Column.Alignment of
           taRightJustify: GX := BR.Right - TextWidth(S) - SmallImages.Width - 4;
                 taCenter: GX := BR.Left + RectWidth(BR) div 2 - (TextWidth(S) + SmallImages.Width + 4) div 2;
         end;
        TX := GX + SmallImages.Width + 4;
        TY := BR.Top + (RectHeight(BR) - TextHeight(S)) div 2;
        GY := BR.Top + (RectHeight(BR) - SmallImages.Height) div 2;
        SmallImages.Draw(B.Canvas, GX, GY, Column.ImageIndex, True);
    end else begin
        CorrectTextbyWidth(B.Canvas, S, RectWidth(BR));
        TX := BR.Left;
        TY := BR.Top + (RectHeight(BR) - TextHeight(S)) div 2;
        case Column.Alignment of
            taRightJustify: TX := BR.Right - TextWidth(S);
                  taCenter: TX := (RectWidth(BR) - TextWidth(S) + 4) div 2;
        end;
    end;
    TextRect(BR, TX, TY, S);
  end;
  Cnvs.Draw(R.Left, R.Top, B);
 finally
  B.Free;
 end;

end;

function TDefineListView.GetHeaderSectionRect(Index: Integer): TRect;
var
  SectionOrder: array of Integer;
  R: TRect;
begin
  if Self.FullDrag then
  begin
      SetLength(SectionOrder, Columns.Count);
      Header_GetOrderArray(FHeaderHandle, Columns.Count, PInteger(SectionOrder));
      Header_GETITEMRECT(FHeaderHandle, SectionOrder[Index] , @R);
  end else
      Header_GETITEMRECT(FHeaderHandle, Index, @R);
  Result := R;
end;

procedure TDefineListView.DrawHeader(DC: HDC);
var
  Cnvs: TControlCanvas;
  i, RightOffset, HeaderCount: Integer;
  R, BGR, HR: TRect;
  PS: TPaintStruct;
begin
  Cnvs := TControlCanvas.Create;
  try
    Cnvs.Handle := BeginPaint(FHeaderHandle, PS);
    HeaderCount := Header_GetItemCount(FHeaderHandle);
    RightOffset := 0;
    for i := 0 to HeaderCount - 1 do begin
        R := GetHeaderSectionRect(i);
        DrawTitle(Cnvs, Columns[i], False, (FActiveSection = I) and FHeaderDown, R);
        if RightOffset < R.Right then RightOffset := R.Right;
    end;
    GetWindowRect(FHeaderHandle, HR);
    BGR := Rect(RightOffset+1, 0, RectWidth(HR), RectHeight(HR));
    if BGR.Left < BGR.Right then begin
       Cnvs.Brush.Color := FTitleFaceColor;//clBtnFace;
       Cnvs.FillRect(BGR);
       DrawFrame(Cnvs, BGR, FTitleFaceColor, FTitleFaceColor, 1);
    end;;
  finally
    Cnvs.Free;
    EndPaint(FHeaderHandle, PS)
  end;
end;

procedure TDefineListView.HeaderWndProc(var Message: TMessage);
var
  X, Y: Integer;

 procedure GetSectionFromPoint(P: TPoint);
 var
  i: Integer;
  R,RA,BR: TRect;
 begin
  FActiveSection := -1;
  RA   := RECT(0,0,HeaderHeight,HeaderHeight);
  for i := 0 to Columns.Count - 1 do
  begin
    R := GetHeaderSectionRect(i);
    FCheckInBox := False;
    if i = 0 then
    begin
       BR := Rect(RA.Right,R.Top,R.Right,R.Bottom);
       if PtInRect(RA, Point(X, Y)) then
       begin
          FActiveSection := i;
          FCheckInBox    := True;
          Break;
       end
       else if PtInRect(BR, Point(X, Y)) then
       begin
          FActiveSection := i;
          Break;
       end;
    end else begin
     if PtInRect(R, Point(X, Y)) then
     begin
       FActiveSection := i;
       Break;
     end;
    end;
  end;
 end;

var
  Info: THDHitTestInfo;
begin
  with Message do begin
   case Msg of
      WM_WINDOWPOSCHANGING :
      begin
       with TWMWINDOWPOSCHANGING(Message) do
            WindowPos.cx := WindowPos.cx + 4;
      end;
      WM_PAINT:DrawHeader(TWMPAINT(Message).DC);
      WM_ERASEBKGND : result := 1;
   else
      Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg, WParam, LParam);
   end;
   case Msg of
      WM_LBUTTONDOWN:
      begin
        X := TWMLBUTTONDOWN(Message).XPos;
        Y := TWMLBUTTONDOWN(Message).YPos;
        GetSectionFromPoint(Point(X, Y));
        Info.Point.X := X;
        Info.Point.Y := Y;
        SendMessage(FHeaderHandle, HDM_HITTEST, 0, Integer(@Info));
        FHeaderDown := not (Info.Flags = HHT_ONDIVIDER);
        if FCheckInBox then SetAllCheck(not FAllCheck);
        RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
      end;
    WM_LBUTTONUP:
      begin
        FHeaderDown := False;
        FActiveSection := -1;
        RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
      end;
   end;
  end;
end;

procedure TDefineListView.WndProc(var Message: TMessage);
var WndClass: String;
begin
  case Message.Msg of
    WM_PARENTNOTIFY:
       with TWMPARENTNOTIFY(Message) do
       begin
         SetLength(WndClass, 80);
         SetLength(WndClass, GetClassName(ChildWnd, PChar(WndClass), Length(WndClass)));
         if (Event = WM_CREATE) and (FHeaderHandle <> 0) and ShowColumnHeaders and
            (WndClass = 'SysHeader32') then
         begin
             SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FDefHeaderProc));
             FHeaderHandle := 0;
         end;

         if (Event = WM_CREATE) and (FHeaderHandle = 0) and ShowColumnHeaders and
            (WndClass = 'SysHeader32') then
         begin
             FHeaderHandle := ChildWnd;
             FDefHeaderProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
             SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FHeaderInstance));
         end;
        end;
    WM_MOUSEWHEEL,
    WM_HSCROLL,
    WM_VSCROLL: if (GroundHas)or(Transparent) then InvalidateRect(Handle, nil, False);
    WM_KEYDOWN:
    Case Message.WParam of
     VK_Left,
     VK_Right,
     VK_UP,
     VK_Down : if (GroundHas)or(Transparent) then InvalidateRect(Handle, nil, False);
    end;
  end;
  inherited;
end;

procedure TDefineListView.RedrawBorder(const Clip: HRGN = 0);
var ViewBorder:TBorderAttrib;
    clColor:TColor;
begin
  with ViewBorder do
  begin
    Ctrl := Self;
    BorderColor := ColorBorder;
    if Enabled then
    begin
       FlatColor   := ColorFlat;
       FocusColor  := ColorFocused;
    end
    else
    begin
       FlatColor   := clSilver;
       FocusColor  := clSilver;
    end;
    MouseState  := FMouseIn;
    DesignState := ComponentState;
    FocusState  := Focused;
    HasBars     := False;
  end;
  clColor := DrawViewBorder(ViewBorder);
  if ((GroundPic.Graphic <> nil) and GroundHas)or
     (Transparent)or
     (Assigned(OnCustomDraw)) then
     Color := clNone
  else   
     Color := clColor;
end;

procedure TDefineListView.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if (FParentColor)and(Parent <> nil) then
        FFlatColor := TForm(Parent).Color;
    RedrawBorder;
  end;
end;

procedure TDefineListView.CMSysColorChange(var Message: TMessage);
begin
  if (FParentColor)and(Parent <> nil) then
      FFlatColor := TForm(Parent).Color;
  RedrawBorder;
end;

procedure TDefineListView.CMParentColorChanged(var Message: TWMNoParams);
begin
  if (FParentColor)and(Parent <> nil) then
      FFlatColor := TForm(Parent).Color;
  RedrawBorder;
end;

procedure TDefineListView.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FBorderColor  := Value;
    2: begin
          FFlatColor    := Value;
          FParentColor  := False;
       end;
    3: if FTitleFaceColor <> Value then
       begin
          FTitleFaceColor   := Value;
          RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
       end;
    4: if FTitleCheckColor <> Value then
       begin
          FTitleCheckColor   := Value;
          RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
       end;
  end;
  RedrawBorder;
end;

procedure TDefineListView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder;
  end;
end;

procedure TDefineListView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseIn := False;
  RedrawBorder;
end;

procedure TDefineListView.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  RedrawBorder;
end;

procedure TDefineListView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder;
end;

procedure TDefineListView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder;
end;

procedure TDefineListView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;

procedure TDefineListView.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

function TDefineListView.GetColumnCount: Integer;
begin
  result := inherited Columns.Count;
end;

function TDefineListView.GetItemsCount: Integer;
begin
  result := inherited Items.Count;
end;

procedure TDefineListView.SetGroundPic(const Value: TPicture);
begin
  FGroundPic.Assign(Value);
  if FGroundPic.Graphic = nil then
     FGroundHas := false;
  RedrawBorder;
  Invalidate;
end;

procedure TDefineListView.DrawBackground(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
var
  x,y:integer;
  R:TRect;
begin
  if GroundPic.Graphic <> nil then
  begin
   with Canvas, ClientRect do
   begin
    Lock;
    R := Rect(Left, Top + HeaderHeight, Right, Bottom);
    if not GroundStretch then
    begin
      x:=0; y:=HeaderHeight;
      while x < Width do
      begin
       while y < Height do
       begin
        Draw(x, y, GroundPic.Graphic);
        y := y + GroundPic.Height;
       end;
       x := x + GroundPic.Width;
       y := HeaderHeight;
      end;
    end else begin
      StretchDraw(R, GroundPic.Graphic);
    end;
    SetBkMode(Handle, bkModeTRANSPARENT);
    Unlock;
   end;
   Perform(LVM_SETTEXTBKCOLOR, 0, LongInt(CLR_NONE));
   ListView_SetBKColor(Handle, CLR_NONE);
  end;
end;

procedure TDefineListView.SetGroundHas(const Value: Boolean);
begin
  FGroundHas := Value;
  if FGroundHas and (FGroundPic.Graphic <> nil) then begin
     FTransparent := false;
     OnCustomDraw := DrawBackground;
  end else if not(csDesigning in ComponentState) then
     OnCustomDraw := FOnDrawBackground
  else begin
     OnCustomDraw := Nil;
  end;
  RedrawBorder;
  Invalidate;
end;

procedure TDefineListView.Loaded;
begin
  inherited;
  if (GroundHas)and(GroundPic.Graphic <> nil) then
      OnCustomDraw := DrawBackground
  else if Transparent then
      OnCustomDraw := DrawTransparent
  else
      OnCustomDraw := OnDrawBackground;
end;

function TDefineListView.GetHeaderHeight: Integer;
begin
  result := RectHeight(GetHeaderSectionRect(0));
  if not (ShowColumnHeaders and (ViewStyle = vsReport)) then
     result := 0;
end;

procedure TDefineListView.SetGroundStretch(const Value: Boolean);
begin
  if FGroundStretch <> value then
  begin
     FGroundStretch := Value;
     RedrawBorder;
     Invalidate;
  end;
end;

procedure TDefineListView.WMPaint(var Message: TWMPaint);
begin
  inherited;
  RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
end;

procedure TDefineListView.SetAllCheck(const Value: Boolean);
var
  inx : integer;
begin
  if FAllCheck <> Value then
  begin
     FAllCheck := Value;
     for inx:=0 to Items.Count - 1 do
         Items.Item[inx].Checked := FAllCheck;
  end;
end;

function TDefineListView.GetListCount: integer;
begin
  result := Items.Count;
end;

function TDefineListView.GetCheckCount: integer;
var inx:integer;
begin
  result := 0;
  for inx := 0 to Items.Count - 1 do
  begin
    if Items.Item[inx].Checked then
       result := result + 1;
  end;
end;

procedure TDefineListView.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  if FTransparent then begin
     FGroundHas   := False;
     OnCustomDraw := DrawTransparent;
  end else if not(csDesigning in ComponentState) then
     OnCustomDraw := FOnDrawBackground
  else begin
     OnCustomDraw := Nil;
  end;
  RedrawBorder;
  Invalidate;
end;

procedure TDefineListView.DrawTransparent(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
begin
    FTransBit.Height := ClientRect.Bottom;
    FTransBit.Width  := ClientRect.Right;
    DrawParentImage(Self, FTransBit.Canvas);
    with Canvas do
    begin
     Lock;
     Draw(0, 0, FTransBit);
     SetBkMode(Handle, bkModeTRANSPARENT);
     Unlock;
    end;
    Perform(LVM_SETTEXTBKCOLOR, 0, LongInt(CLR_NONE));
    ListView_SetBKColor(Handle, CLR_NONE);
end;

procedure TDefineListView.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  case Message.Msg of
   WM_SIZE,WM_PARENTNOTIFY:
   begin
    RedrawBorder;
    Invalidate;
   end;
  end;
end;

{ TDefineGridDraw }

function TDefineGridDraw.GetMouseIn: boolean;
begin
  result := FMouseIn;
end;

constructor TDefineGridDraw.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle   := bsNone;
  FFocusColor   := clWhite;
  FBorderColor  := DefaultBorderColor;
  FLinesColor   := DefaultBorderColor;
  FFlatColor    := DefaultFlatColor;
  FParentColor  := True;
  FMouseIn      := False;
end;

procedure TDefineGridDraw.RedrawBorder(const Clip: HRGN);
var
  Attrib:TBorderAttrib;
begin
  with Attrib do
  begin
   Ctrl        := self;
   FocusColor  := ColorFocused;
   BorderColor := ColorBorder;
   FlatColor   := ColorFlat;
   FocusState  := Focused;
   MouseState  := FMouseIn;
   DesignState := ComponentState;
   HasBars     := ScrollBars = ssBoth;
   BoldState   := True;
  end;
  Color := DrawEditBorder(Attrib,Clip);
end;

procedure TDefineGridDraw.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
      RedrawBorder;
    end;
  end;
end;

procedure TDefineGridDraw.CMSysColorChange(var Message: TMessage);
begin
    if (Parent <> nil)and(FParentColor) then
      FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TDefineGridDraw.CMParentColorChanged(var Message: TWMNoParams);
begin
    if (Parent <> nil)and(FParentColor) then
       FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TDefineGridDraw.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusColor    := Value;
    1: FBorderColor   := Value;
    2: begin
         FFlatColor   := Value;
         FParentColor := False;
       end;
    3: FLinesColor    := Value;
  end;
  Repaint;
  RedrawBorder;
end;

procedure TDefineGridDraw.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder;
  end;
end;

procedure TDefineGridDraw.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseIn := False;
  RedrawBorder;
end;

procedure TDefineGridDraw.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder;
end;

procedure TDefineGridDraw.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TDefineGridDraw.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TDefineGridDraw.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TDefineGridDraw.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TDefineGridDraw.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var FRect:TRect;
begin
  inherited;
  {//绘制数据区的表格边框
  with ARect, Canvas do
  begin
   if (ACol = 0)or(ARow = 0) then
   begin
     if ARow > 0 then begin
        FRect := Rect(Left-1,Top-1,Right,Bottom+2);
        DrawFrame(Canvas, FRect, FLinesColor, FLinesColor, 1)
     end else if ACol > 0 then begin
        FRect := Rect(Left-2,Top,Right+1,Bottom+1);
        DrawFrame(Canvas, FRect, FLinesColor, FLinesColor, 1)
     end else begin
        FRect := Rect(Left,Top,Right+1,Bottom+1);
        DrawButtonBorder(Canvas,FRect,FLinesColor,1)
     end;
   end else begin
      //FRect := Rect(Left-1,Top-1,Right+1,Bottom+1);
      //DrawButtonBorder(Canvas,FRect,FLinesColor,1);
      InflateRect(FRect, -1, -1);
    FRect := Rect(Left-2,Top-2,Right+2,Bottom+2);
    //选择线型颜色。。。
    Brush.Color:=FLinesColor;
    //对表格进行绘制
    InflateRect(FRect, -1, -1);
    FrameRect(FRect);
   end;
  end; }
  //绘制数据区的表格边框
  with ARect, Canvas do
  begin
    FRect := Rect(Left-2,Top-2,Right+2,Bottom+2);
    //选择线型颜色。。。
    Brush.Color:=FLinesColor;
    //对表格进行绘制
    InflateRect(FRect, -1, -1);
    FrameRect(FRect);  
  end;
end;

{ TDefineGridString}

{ StrItem management for TStringSparseList }

type
  PStrItem = ^TStrItem;
  TStrItem = record
    FObject: TObject;
    FString: string;
  end;

function NewStrItem(const AString: string; AObject: TObject): PStrItem;
begin
  New(Result);
  Result^.FObject := AObject;
  Result^.FString := AString;
end;

procedure DisposeStrItem(P: PStrItem);
begin
  Dispose(P);
end;

type
{ TDefineGridSparseArray class}

{ Used by TSparseList.  Based on Sparse1Array, but has Pointer elements
  and Integer index, just like TPointerList/TList, and less indirection }

  { Apply function for the applicator:
        TheIndex        Index of item in array
        TheItem         Value of item (i.e pointer element) in section
        Returns: 0 if success, else error code. }
  TSPAApply = function(TheIndex: Integer; TheItem: Pointer): Integer;

  TSecDir = array[0..4095] of Pointer;  { Enough for up to 12 bits of sec }
  PSecDir = ^TSecDir;
  TSPAQuantum = (SPASmall, SPALarge);   { Section size }

  TDefineGridSparseArray = class(TObject)
  private
    secDir: PSecDir;
    slotsInDir: Word;
    indexMask, secShift: Word;
    FHighBound: Integer;
    FSectionSize: Word;
    cachedIndex: Integer;
    cachedPointer: Pointer;
    { Return item[i], nil if slot outside defined section. }
    function  GetAt(Index: Integer): Pointer;
    { Return address of item[i], creating slot if necessary. }
    function  MakeAt(Index: Integer): PPointer;
    { Store item at item[i], creating slot if necessary. }
    procedure PutAt(Index: Integer; Item: Pointer);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;

    { Traverse SPA, calling apply function for each defined non-nil
      item.  The traversal terminates if the apply function returns
      a value other than 0. }
    { NOTE: must be static method so that we can take its address in
      TSparseList.ForAll }
    function  ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;

    { Ratchet down HighBound after a deletion }
    procedure ResetHighBound;

    property HighBound: Integer read FHighBound;
    property SectionSize: Word read FSectionSize;
    property Items[Index: Integer]: Pointer read GetAt write PutAt; default;
  end;

{ TDefineGridSparseList class }

  TDefineGridSparseList = class(TObject)
  private
    FList: TDefineGridSparseArray;
    FCount: Integer;    { 1 + HighBound, adjusted for Insert/Delete }
    FQuantum: TSPAQuantum;
    procedure NewList(Quantum: TSPAQuantum);
  protected
    function  Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    procedure Move(CurIndex, NewIndex: Integer);
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

{ TDefineGridSparseLists class }

  TDefineGridSparseLists = class(TStrings)
  private
    FList: TDefineGridSparseList;                 { of StrItems }
    FOnChange: TNotifyEvent;
  protected
    function  Get(Index: Integer): String; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure Changed;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Clear; override;
    property List: TDefineGridSparseList read FList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TDefineGridSparseArray }

const
  SPAIndexMask: array[TSPAQuantum] of Byte = (15, 255);
  SPASecShift: array[TSPAQuantum] of Byte = (4, 8);

{ Expand Section Directory to cover at least `newSlots' slots. Returns: Possibly
  updated pointer to the Section Directory. }
function  ExpandDir(secDir: PSecDir; var slotsInDir: Word;
  newSlots: Word): PSecDir;
begin
  Result := secDir;
  ReallocMem(Result, newSlots * SizeOf(Pointer));
  FillChar(Result^[slotsInDir], (newSlots - slotsInDir) * SizeOf(Pointer), 0);
  slotsInDir := newSlots;
end;

{ Allocate a section and set all its items to nil. Returns: Pointer to start of
  section. }
function  MakeSec(SecIndex: Integer; SectionSize: Word): Pointer;
var
  SecP: Pointer;
  Size: Word;
begin
  Size := SectionSize * SizeOf(Pointer);
  GetMem(secP, size);
  FillChar(secP^, size, 0);
  MakeSec := SecP
end;

constructor TDefineGridSparseArray.Create(Quantum: TSPAQuantum);
begin
  SecDir := nil;
  SlotsInDir   := 0;
  FHighBound   := -1;
  FSectionSize := Word(SPAIndexMask[Quantum]) + 1;
  IndexMask    := Word(SPAIndexMask[Quantum]);
  SecShift     := Word(SPASecShift[Quantum]);
  CachedIndex  := -1;
end;

destructor TDefineGridSparseArray.Destroy;
var
  i:  Integer;
  size: Word;
begin
  { Scan section directory and free each section that exists. }
  i := 0;
  size := FSectionSize * SizeOf(Pointer);
  while i < slotsInDir do begin
    if secDir^[i] <> nil then
      FreeMem(secDir^[i], size);
    Inc(i)
  end;

  { Free section directory. }
  if secDir <> nil then
    FreeMem(secDir, slotsInDir * SizeOf(Pointer));
end;

function  TDefineGridSparseArray.GetAt(Index: Integer): Pointer;
var
  byteP: PChar;
  secIndex: Cardinal;
begin
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If not null, index into
    Section using low order part of index. }
  if Index = cachedIndex then
     Result := cachedPointer
  else begin
    secIndex := Index shr secShift;
    if secIndex >= slotsInDir then
      byteP := nil
    else begin
      byteP := secDir^[secIndex];
      if byteP <> nil then begin
        Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
      end
    end;
    if byteP = nil then Result := nil else Result := PPointer(byteP)^;
    cachedIndex  := Index;
    cachedPointer := Result;
  end
end;

function  TDefineGridSparseArray.MakeAt(Index: Integer): PPointer;
var
  dirP: PSecDir;
  p: Pointer;
  byteP: PChar;
  secIndex: Word;
begin
  { Expand Section Directory if necessary. }
  secIndex := Index shr secShift;       { Unsigned shift }
  if secIndex >= slotsInDir then
    dirP := expandDir(secDir, slotsInDir, secIndex + 1)
  else
    dirP := secDir;

  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If null, create new
    Section.  Index into Section using low order part of index. }
  secDir := dirP;
  p := dirP^[secIndex];
  if p = nil then begin
    p := makeSec(secIndex, FSectionSize);
    dirP^[secIndex] := p
  end;
  byteP := p;
  Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
  if Index > FHighBound then
    FHighBound := Index;
  Result := PPointer(byteP);
  cachedIndex := -1
end;

procedure TDefineGridSparseArray.PutAt(Index: Integer; Item: Pointer);
begin
  if (Item <> nil) or (GetAt(Index) <> nil) then
  begin
    MakeAt(Index)^ := Item;
    if Item = nil then
      ResetHighBound
  end
end;

function  TDefineGridSparseArray.ForAll(ApplyFunction: Pointer {TSPAApply}):
  Integer;
var
  itemP: PChar;                         { Pointer to item in section }
  item: Pointer;
  i, callerBP: Cardinal;
  j, index: Integer;
begin
  { Scan section directory and scan each section that exists,
    calling the apply function for each non-nil item.
    The apply function must be a far local function in the scope of
    the procedure P calling ForAll.  The trick of setting up the stack
    frame (taken from TurboVision's TCollection.ForEach) allows the
    apply function access to P's arguments and local variables and,
    if P is a method, the instance variables and methods of P's class }
  Result := 0;
  i := 0;
  asm
    mov   eax,[ebp]                     { Set up stack frame for local }
    mov   callerBP,eax
  end;
  while (i < slotsInDir) and (Result = 0) do begin
    itemP := secDir^[i];
    if itemP <> nil then begin
      j := 0;
      index := i shl SecShift;
      while (j < FSectionSize) and (Result = 0) do begin
        item := PPointer(itemP)^;
        if item <> nil then
          { ret := ApplyFunction(index, item.Ptr); }
          asm
            mov   eax,index
            mov   edx,item
            push  callerBP
            call  ApplyFunction
            pop   ecx
            mov   @Result,eax
          end;
        Inc(itemP, SizeOf(Pointer));
        Inc(j);
        Inc(index)
      end
    end;
    Inc(i)
  end;
end;

procedure TDefineGridSparseArray.ResetHighBound;
var
  NewHighBound: Integer;

  function  Detector(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    if TheIndex > FHighBound then
      Result := 1
    else
    begin
      Result := 0;
      if TheItem <> nil then NewHighBound := TheIndex
    end
  end;

begin
  NewHighBound := -1;
  ForAll(@Detector);
  FHighBound := NewHighBound
end;

{ TDefineGridSparseList }

constructor TDefineGridSparseList.Create(Quantum: TSPAQuantum);
begin
  NewList(Quantum)
end;

destructor TDefineGridSparseList.Destroy;
begin
  if FList <> nil then FList.Destroy
end;

procedure TDefineGridSparseList.Clear;
begin
  FList.Destroy;
  NewList(FQuantum);
  FCount := 0
end;

procedure TDefineGridSparseList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then Exit;
  for I := Index to FCount - 1 do
    FList[I] := FList[I + 1];
  FList[FCount] := nil;
  Dec(FCount);
end;

procedure TDefineGridSparseList.Exchange(Index1, Index2: Integer);
var
  temp: Pointer;
begin
  temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, temp);
end;

{ Jump to TDefineGridSparseArray.ForAll so that it looks like it was called
  from our caller, so that the BP trick works. }

function TDefineGridSparseList.ForAll(ApplyFunction: Pointer {TSPAApply}): Integer; assembler;
asm
        MOV     EAX,[EAX].TDefineGridSparseList.FList
        JMP     TDefineGridSparseArray.ForAll
end;

function  TDefineGridSparseList.Get(Index: Integer): Pointer;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  Result := FList[Index]
end;

procedure TDefineGridSparseList.Insert(Index: Integer; Item: Pointer);
var
  i: Integer;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  I := FCount;
  while I > Index do
  begin
    FList[i] := FList[i - 1];
    Dec(i)
  end;
  FList[Index] := Item;
  if Index > FCount then FCount := Index;
  Inc(FCount)
end;

procedure TDefineGridSparseList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TDefineGridSparseList.NewList(Quantum: TSPAQuantum);
begin
  FQuantum := Quantum;
  FList := TDefineGridSparseArray.Create(Quantum)
end;

procedure TDefineGridSparseList.Put(Index: Integer; Item: Pointer);
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  FList[Index] := Item;
  FCount := FList.HighBound + 1
end;

{ TDefineGridSparseLists }

constructor TDefineGridSparseLists.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  FList := TDefineGridSparseList.Create(Quantum)
end;

destructor  TDefineGridSparseLists.Destroy;
begin
  if FList <> nil then begin
    Clear;
    FList.Destroy
  end
end;

procedure TDefineGridSparseLists.ReadData(Reader: TReader);
var
  i: Integer;
begin
  with Reader do begin
    i := Integer(ReadInteger);
    while i > 0 do begin
      InsertObject(Integer(ReadInteger), ReadString, nil);
      Dec(i)
    end
  end
end;

procedure TDefineGridSparseLists.WriteData(Writer: TWriter);
var
  itemCount: Integer;

  function  CountItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Inc(itemCount);
    Result := 0
  end;

  function  StoreItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    with Writer do
    begin
      WriteInteger(TheIndex);           { Item index }
      WriteString(PStrItem(TheItem)^.FString);
    end;
    Result := 0
  end;

begin
  with Writer do
  begin
    itemCount := 0;
    FList.ForAll(@CountItem);
    WriteInteger(itemCount);
    FList.ForAll(@StoreItem);
  end
end;

procedure TDefineGridSparseLists.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('List', ReadData, WriteData, True);
end;

function  TDefineGridSparseLists.Get(Index: Integer): String;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then Result := '' else Result := p^.FString
end;

function  TDefineGridSparseLists.GetCount: Integer;
begin
  Result := FList.Count
end;

function  TDefineGridSparseLists.GetObject(Index: Integer): TObject;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then Result := nil else Result := p^.FObject
end;

procedure TDefineGridSparseLists.Put(Index: Integer; const S: String);
var
  p: PStrItem;
  obj: TObject;
begin
  p := PStrItem(FList[Index]);
  if p = nil then obj := nil else obj := p^.FObject;
  if (S = '') and (obj = nil) then   { Nothing left to store }
    FList[Index] := nil
  else
    FList[Index] := NewStrItem(S, obj);
  if p <> nil then DisposeStrItem(p);
  Changed
end;

procedure TDefineGridSparseLists.PutObject(Index: Integer; AObject: TObject);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then
    p^.FObject := AObject
  else if AObject <> nil then
    FList[Index] := NewStrItem('',AObject);
  Changed
end;

procedure TDefineGridSparseLists.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self)
end;

procedure TDefineGridSparseLists.Delete(Index: Integer);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then DisposeStrItem(p);
  FList.Delete(Index);
  Changed
end;

procedure TDefineGridSparseLists.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TDefineGridSparseLists.Insert(Index: Integer; const S: String);
begin
  FList.Insert(Index, NewStrItem(S, nil));
  Changed
end;

procedure TDefineGridSparseLists.Clear;

  function  ClearItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    DisposeStrItem(PStrItem(TheItem));    { Item guaranteed non-nil }
    Result := 0
  end;

begin
  FList.ForAll(@ClearItem);
  FList.Clear;
  Changed
end;

{ TDefineGridStrings }

{ AIndex < 0 is a column (for column -AIndex - 1)
  AIndex > 0 is a row (for row AIndex - 1)
  AIndex = 0 denotes an empty row or column }
  
constructor TDefineGridStrings.Create(AGrid: TDefineGridString; AIndex: Longint);
begin
  inherited Create;
  FGrid := AGrid;
  FIndex := AIndex;
end;

procedure TDefineGridStrings.Assign(Source: TPersistent);
var
  I, Max: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    Max := TStrings(Source).Count - 1;
    if Max >= Count then Max := Count - 1;
    try
      for I := 0 to Max do
      begin
        Put(I, TStrings(Source).Strings[I]);
        PutObject(I, TStrings(Source).Objects[I]);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TDefineGridStrings.CalcXY(Index: Integer; var X, Y: Integer);
begin
  if FIndex = 0 then
  begin
    X := -1; Y := -1;
  end else if FIndex > 0 then
  begin
    X := Index;
    Y := FIndex - 1;
  end else
  begin
    X := -FIndex - 1;
    Y := Index;
  end;
end;

{ Changes the meaning of Add to mean copy to the first empty string }
function TDefineGridStrings.Add(const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Strings[I] = '' then
    begin
      if S = '' then
        Strings[I] := ' '
      else
        Strings[I] := S;
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TDefineGridStrings.Clear;
var
  SSList: TDefineGridSparseLists;
  I: Integer;

  function BlankStr(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Objects[TheIndex] := nil;
    Strings[TheIndex] := '';
    Result := 0;
  end;

begin
  if FIndex > 0 then
  begin
    SSList := TDefineGridSparseLists(TDefineGridSparseList(FGrid.FData)[FIndex - 1]);
    if SSList <> nil then SSList.List.ForAll(@BlankStr);
  end
  else if FIndex < 0 then
    for I := Count - 1 downto 0 do
    begin
      Objects[I] := nil;
      Strings[I] := '';
    end;
end;

procedure InvalidOp(const id: string);
begin
  raise EInvalidGridOperation.Create(id);
end;

procedure TDefineGridStrings.Delete(Index: Integer);
begin
  InvalidOp(sInvalidStringGridOp);
end;

function TDefineGridStrings.Get(Index: Integer): string;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := '' else Result := FGrid.Cells[X, Y];
end;

function TDefineGridStrings.GetCount: Integer;
begin
  { Count of a row is the column count, and vice versa }
  if FIndex = 0 then Result := 0
  else if FIndex > 0 then Result := Integer(FGrid.ColCount)
  else Result := Integer(FGrid.RowCount);
end;

function TDefineGridStrings.GetObject(Index: Integer): TObject;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := nil else Result := FGrid.Objects[X, Y];
end;

procedure TDefineGridStrings.Insert(Index: Integer; const S: string);
begin
  InvalidOp(sInvalidStringGridOp);
end;

procedure TDefineGridStrings.Put(Index: Integer; const S: string);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Cells[X, Y] := S;
end;

procedure TDefineGridStrings.PutObject(Index: Integer; AObject: TObject);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Objects[X, Y] := AObject;
end;

procedure TDefineGridStrings.SetUpdateState(Updating: Boolean);
begin
  FGrid.SetUpdateState(Updating);
end;

{ TStringGrid }

constructor TDefineGridString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialize;
end;

destructor TDefineGridString.Destroy;
  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;

begin
  if FRows <> nil then
  begin
    TDefineGridSparseList(FRows).ForAll(@FreeItem);
    TDefineGridSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TDefineGridSparseList(FCols).ForAll(@FreeItem);
    TDefineGridSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TDefineGridSparseList(FData).ForAll(@FreeItem);
    TDefineGridSparseList(FData).Free;
  end;
  inherited Destroy;
end;

procedure TDefineGridString.ColumnMoved(FromIndex, ToIndex: Longint);

  function MoveColData(Index: Integer; ARow: TDefineGridSparseLists): Integer; far;
  begin
    ARow.Move(FromIndex, ToIndex);
    Result := 0;
  end;

begin
  TDefineGridSparseList(FData).ForAll(@MoveColData);
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TDefineGridString.RowMoved(FromIndex, ToIndex: Longint);
begin
  TDefineGridSparseList(FData).Move(FromIndex, ToIndex);
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

function TDefineGridString.GetEditText(ACol, ARow: Longint): string;
begin
  Result := Cells[ACol, ARow];
  if Assigned(OnGetEditText) then OnGetEditText(Self, ACol, ARow, Result);
end;

procedure TDefineGridString.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  DisableEditUpdate;
  try
    if Value <> Cells[ACol, ARow] then Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TDefineGridString.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  if DefaultDrawing then
     Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TDefineGridString.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TDefineGridString.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

procedure TDefineGridString.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TDefineGridSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TDefineGridSparseList.Create(quantum);
  if FData = nil then FData := TDefineGridSparseList.Create(quantum);
end;

procedure TDefineGridString.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating; 
  if not Updating and FNeedsUpdating then
  begin
    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TDefineGridString.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

function  TDefineGridString.EnsureColRow(Index: Integer; IsCol: Boolean): TDefineGridStrings;
var
  RCIndex: Integer;
  PList: ^TDefineGridSparseList;
begin
  if IsCol then PList := @FCols else PList := @FRows;
  Result := TDefineGridStrings(PList^[Index]);
  if Result = nil then
  begin
    if IsCol then RCIndex := -Index - 1 else RCIndex := Index + 1;
    Result := TDefineGridStrings.Create(Self, RCIndex);
    PList^[Index] := Result;
  end;
end;

function  TDefineGridString.EnsureDataRow(ARow: Integer): Pointer;
var
  quantum: TSPAQuantum;
begin
  Result := TDefineGridSparseLists(TDefineGridSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TDefineGridSparseLists.Create(quantum);
    TDefineGridSparseList(FData)[ARow] := Result;
  end;
end;

function TDefineGridString.GetCells(ACol, ARow: Integer): string;
var
  ssl: TDefineGridSparseLists;
begin
  ssl := TDefineGridSparseLists(TDefineGridSparseList(FData)[ARow]);
  if ssl = nil then Result := '' else Result := ssl[ACol];
end;

function TDefineGridString.GetCols(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, True);
end;

function TDefineGridString.GetObjects(ACol, ARow: Integer): TObject;
var
  ssl: TDefineGridSparseLists;
begin
  ssl := TDefineGridSparseLists(TDefineGridSparseList(FData)[ARow]);
  if ssl = nil then Result := nil else Result := ssl.Objects[ACol];
end;

function TDefineGridString.GetRows(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, False);
end;

procedure TDefineGridString.SetCells(ACol, ARow: Integer; const Value: string);
begin
  TDefineGridStrings(EnsureDataRow(ARow))[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  Update(ACol, ARow);
end;

procedure TDefineGridString.SetCols(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, True).Assign(Value);
end;

procedure TDefineGridString.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  TDefineGridStrings(EnsureDataRow(ARow)).Objects[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  Update(ACol, ARow);
end;

procedure TDefineGridString.SetRows(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, False).Assign(Value);
end;

const
  DefaultTabWidth = 100;

function Max (Value1, Value2 : Integer) : Integer;
begin
  If Value1 > Value2 then Result := Value1 else Result := Value2;
end;

function Min (Value1, Value2 : Integer) : Integer;
begin
  If Value1 < Value2 then Result := Value1 else Result := Value2;
end;

function MakeDarkColor (AColor : TColor; ADarkRate : Integer) : TColor;
var
  R, G, B : Integer;
begin
  R := GetRValue   (ColorToRGB (AColor)) - ADarkRate;
  G := GetGValue (ColorToRGB (AColor)) - ADarkRate;
  B := GetBValue (ColorToRGB (AColor)) - ADarkRate;
  if R < 0 then R := 0;
  if G < 0 then G := 0;
  if B < 0 then B := 0;
  if R > 255 then R := 255;
  if G > 255 then G := 255;
  if B > 255 then B := 255;
  Result := TColor (RGB (R, G, B));
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
var
  X, Y: Integer;
  SaveIndex: Integer;
begin
  if (Image.Width = 0) or (Image.Height = 0) then Exit;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    for X := 0 to (WidthOf(Rect) div Image.Width) do
      for Y := 0 to (HeightOf(Rect) div Image.Height) do
        Canvas.Draw(Rect.Left + X * Image.Width,
          Rect.Top + Y * Image.Height, Image);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure GradientSimpleFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array[0..2] of Byte;    { Start RGB values }
  RGBDelta: array[0..2] of Integer; { Difference between start and end RGB values }
  ColorBand: TRect;                 { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBrush;
begin
  if IsRectEmpty(ARect) then Exit;
  if Colors < 2 then begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(Canvas.Handle, ARect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    fdTopToBottom, fdLeftToRight: begin
      { Set the Red, Green and Blue colors }
      StartRGB[0] := GetRValue(StartColor);
      StartRGB[1] := GetGValue(StartColor);
      StartRGB[2] := GetBValue(StartColor);
      { Calculate the difference between begin and end RGB values }
      RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
    end;
    fdBottomToTop, fdRightToLeft: begin
      { Set the Red, Green and Blue colors }
      { Reverse of TopToBottom and LeftToRight directions }
      StartRGB[0] := GetRValue(EndColor);
      StartRGB[1] := GetGValue(EndColor);
      StartRGB[2] := GetBValue(EndColor);
      { Calculate the difference between begin and end RGB values }
      { Reverse of TopToBottom and LeftToRight directions }
      RGBDelta[0] := GetRValue(StartColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(StartColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(StartColor) - StartRGB[2];
    end;
  end; {case}
  { Calculate the color band's coordinates }
  ColorBand := ARect;
  if Direction in [fdTopToBottom, fdBottomToTop] then begin
    Colors := Max(2, Min(Colors, HeightOf(ARect)));
    Delta := HeightOf(ARect) div Colors;
  end
  else begin
    Colors := Max(2, Min(Colors, WidthOf(ARect)));
    Delta := WidthOf(ARect) div Colors;
  end;
  with Canvas.Pen do begin { Set the pen style and mode }
    Style := psSolid;
    Mode := pmCopy;
  end;
  { Perform the fill }
  if Delta > 0 then begin
    for I := 0 to Colors do begin
      case Direction of
        { Calculate the color band's top and bottom coordinates }
        fdTopToBottom, fdBottomToTop: begin
          ColorBand.Top := ARect.Top + I * Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
        { Calculate the color band's left and right coordinates }
        fdLeftToRight, fdRightToLeft: begin
          ColorBand.Left := ARect.Left + I * Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
      end; {case}
      { Calculate the color band's color }
      Brush := CreateSolidBrush(RGB(
        StartRGB[0] + MulDiv(I, RGBDelta[0], Colors - 1),
        StartRGB[1] + MulDiv(I, RGBDelta[1], Colors - 1),
        StartRGB[2] + MulDiv(I, RGBDelta[2], Colors - 1)));
      FillRect(Canvas.Handle, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  end;
  if Direction in [fdTopToBottom, fdBottomToTop] then
    Delta := HeightOf(ARect) mod Colors
  else Delta := WidthOf(ARect) mod Colors;
  if Delta > 0 then begin
    case Direction of
      { Calculate the color band's top and bottom coordinates }
      fdTopToBottom, fdBottomToTop: begin
        ColorBand.Top := ARect.Bottom - Delta;
        ColorBand.Bottom := ColorBand.Top + Delta;
      end;
      { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft: begin
        ColorBand.Left := ARect.Right - Delta;
        ColorBand.Right := ColorBand.Left + Delta;
      end;
    end; {case}
    case Direction of
      fdTopToBottom, fdLeftToRight:
        Brush := CreateSolidBrush(EndColor);
      else {fdBottomToTop, fdRightToLeft }
        Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(Canvas.Handle, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

procedure GradientXPFillRect (ACanvas : TCanvas; ARect : TRect; LightColor : TColor; DarkColor : TColor; Colors : Byte);
const
  cLightColorOffset : Integer = 30;
  cMainBarOffset : Integer = 6;
var
  DRect : TRect;
  I : Integer;
begin
  if IsRectEmpty(ARect) then Exit;

  ACanvas.Brush.Color := DarkColor;
  ACanvas.FrameRect (ARect);
  //InflateRect (ARect, -1, -1);

  //Main center rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := DRect.Top + cMainBarOffset;
  DRect.Bottom := DRect.Bottom - cMainBarOffset;
  GradientSimpleFillRect (ACanvas, DRect, DarkColor, LightColor, fdTopToBottom, Colors);

  //Bottom rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := ARect.Bottom - cMainBarOffset;
  GradientSimpleFillRect (ACanvas, DRect, LightColor, DarkColor, fdTopToBottom, Colors);

  //Second left rect
  DRect := ARect;
  DRect := Rect (ARect.Left + cMainBarOffset div 4, 0, ARect.Left + cMainBarOffset, 1);
  For I := ARect.Top + cMainBarOffset to ARect.Bottom do
  begin
    DRect.Top := I;
    DRect.Bottom := I+1;
    GradientSimpleFillRect (ACanvas, DRect, ACanvas.Pixels [DRect.Left-1, DRect.Top],
      ACanvas.Pixels [DRect.Right + 1, DRect.Top], fdLeftToRight, 8);
  end;

  //Top light rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Bottom := DRect.Top + cMainBarOffset div 4;
  GradientSimpleFillRect (ACanvas, DRect, MakeDarkColor (LightColor, -cLightColorOffset), LightColor, fdTopToBottom, 8);

  //Second top rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := DRect.Top + cMainBarOffset div 4;
  DRect.Bottom := ARect.Top + cMainBarOffset;
  GradientSimpleFillRect (ACanvas, DRect, LightColor, DarkColor, fdTopToBottom, 8);

  //Left light rect
  DRect := ARect;
  DRect.Top := DRect.Top + cMainBarOffset;
  DRect.Right := DRect.Left + cMainBarOffset div 4;
  GradientSimpleFillRect (ACanvas, DRect, MakeDarkColor (LightColor, -cLightColorOffset), LightColor, fdLeftToRight, 8);

  //Second left rect
  DRect := ARect;
  DRect := Rect (ARect.Left + cMainBarOffset div 4, 0, ARect.Left + cMainBarOffset, 1);
  For I := ARect.Top + cMainBarOffset to ARect.Bottom do
  begin
    DRect.Top := I;
    DRect.Bottom := I+1;
    GradientSimpleFillRect (ACanvas, DRect, ACanvas.Pixels [DRect.Left-1, DRect.Top],
      ACanvas.Pixels [DRect.Right + 1, DRect.Top], fdLeftToRight, 8);
  end;

  For I := 0 to cMainBarOffset do
  begin
    ACanvas.Pen.Color := ACanvas.Pixels [ARect.Left + I, ARect.Top + cMainBarOffset+1];
    ACanvas.MoveTo (ARect.Left + I, ARect.Top + cMainBarOffset);
    ACanvas.LineTo (ARect.Left + I, ARect.Top + I);
    ACanvas.LineTo (ARect.Left + cMainBarOffset, ARect.Top + I);
  end;
end;

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  BRect : TRect;
begin
  case Direction of
    fdCenterToVerti:
      begin
        BRect := ARect;
        BRect.Bottom := BRect.Top +  HeightOf (ARect) div 2;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdTopToBottom, Colors);
        BRect.Top := (BRect.Top + HeightOf (ARect) div 2);
        BRect.Bottom := ARect.Bottom;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdBottomToTop, Colors);
      end;
    fdCenterToHoriz:
      begin
        BRect := ARect;
        BRect.Right := BRect.Left +  WidthOf (ARect) div 2;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdLeftToRight, Colors);
        BRect.Left := (BRect.Left + WidthOf (ARect) div 2);
        BRect.Right := ARect.Right;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdRightToLeft, Colors);
      end;
    fdXPFace:
      begin
        GradientXPFillRect (Canvas, ARect, StartColor, EndColor, Colors);
      end
    else
      GradientSimpleFillRect(Canvas, ARect, StartColor, EndColor, Direction, Colors);
  end;
end;
// constructor must create a TControlCanvas for the owner draw style

constructor TDefinePages.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FCanvas      := TControlCanvas.Create; 
  FBorderColor := DefaultBorderColor;
  FTabPosition := tpTop;
  FHotTrackTab := -1;
  ShowHint     := true;
  FStyle       := pcsFlatStyle;
  FTabTextAlignment := taCenter;
  FOwnerDraw := False;
end;

// remove link with glyphs and free the canvas

destructor TDefinePages.Destroy;
begin
  try
    FCanvas.Free;
  except
  end;
  if Assigned (FImageList) then
  try
    FImageList.OnChange := nil;
  except
  end;
  inherited Destroy;
end;

// CreateParams called to set the additional style bits

procedure TDefinePages.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams (Params);

  with Params do
  begin
    case FStyle of
      pcsTabs: Style:= Style or TCS_TABS;
      pcsButtons: Style:= Style or TCS_BUTTONS;
      pcsFlatButtons: Style := Style or TCS_BUTTONS or TCS_FLATBUTTONS;
      pcsFlatStyle: begin end;
    end;

    if FOwnerDraw then Style := Style or TCS_OWNERDRAWFIXED;

    case FTabPosition of
      tpTop:
      begin
        //Style := Style and (not TCS_VERTICAL) and (not TCS_BOTTOM);
      end;
      tpBottom:
      begin
        Style := Style or TCS_BOTTOM;
      end;
      tpLeft:
      begin
        Style := Style or TCS_VERTICAL;
      end;
      tpRight:
      begin
        Style := Style or TCS_VERTICAL or TCS_RIGHT;
      end;
    end;
  end;
end;

// CreateWnd also must set links to the glyphs

procedure TDefinePages.CreateWnd;
begin
  inherited CreateWnd;
  if Assigned (FImageList) then SetGlyphs (FImageList);
end;

// if the glyphs should change then update the tabs

procedure TDefinePages.GlyphsChanged (Sender : TObject);
begin
  if Assigned (FImageList) then UpdateGlyphs;
end;

// multiline property redefined as readonly, this makes it
// disappear from the object inspector

function TDefinePages.GetMultiline : boolean;
begin
  Result := inherited Multiline
end;

// link the tabs to the glyph list
// nil parameter removes link

procedure TDefinePages.SetGlyphs (Value : TImageList);
var
  I : Integer;
begin
  FImageList := Value;
  if Assigned(FImageList) then
    begin
      SendMessage (Handle, TCM_SETIMAGELIST, 0, FImageList.Handle);
      For I := 0 to PageCount - 1 do begin
        if Pages[i]<>Nil then
        (Pages[I] as TDefineSheet).ImageIndex := I;
      end;
      FImageList.OnChange := GlyphsChanged
    end
  else
  begin
    SendMessage (Handle, TCM_SETIMAGELIST, 0, 0);
    For I := 0 to PageCount - 1 do begin
        if Pages[i]<>Nil then
        (Pages[I] as TDefineSheet).ImageIndex := -1;
    end;
  end;
  UpdateGlyphs;
  SendMessage (Handle, WM_SIZE, 0, 0);
end;


// determine properties whenever the tab styles are changed

procedure TDefinePages.SetOwnerDraw (AValue : Boolean);
begin
  if FOwnerDraw <> AValue then
  begin
    FOwnerDraw := AValue;
    ReCreateWnd;
    SendMessage (Handle, WM_SIZE, 0, 0);
    if (Self.PageCount > 0) and (ActivePage <> nil) then
      ActivePage.Invalidate;
  end
end;

// update the glyphs linked to the tab

procedure TDefinePages.UpdateGlyphs;
var
  TCItem : TTCItem;
  Control,
  Loop : integer;
begin
  if FImageList <> nil then
  begin
    for Loop := 0 to pred(PageCount) do
    begin
      TCItem.Mask := TCIF_IMAGE;
      TCItem.iImage := Loop;
      Control := Loop;
      // OnGlyphMap allows the user to reselect the glyph linked to a
      // particular tab
      if Assigned (FOnGlyphMap) then
        FOnGlyphMap (Self, Control, TCItem.iImage);

      if SendMessage (Handle, TCM_SETITEM, Control, longint(@TCItem)) = 0 then;
        //raise EListError.Create ('TDefinePages error in setting tab glyph')
    end
  end
end;

// called when Owner Draw style is selected:
// retrieve the component style, set up the canvas and
// call the DrawItem method

procedure TDefinePages.CNDrawItem (var Msg : TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    //State := TOwnerDrawState (WordRec (LongRec (itemState).Lo).Lo);
    //!!
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if integer (itemID) >= 0 then
      DrawItem (itemID, rcItem, State)
    else
      FCanvas.FillRect (rcItem);
    FCanvas.Handle := 0
  end;
end;

// default DrawItem method

procedure TDefinePages.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then
     FOnDrawItem (Self, Index, FCanvas, Rect, State)
  else begin
    //FCanvas.FillRect (Rect);
    GradientFillRect (FCanvas, Rect, clWhite, RGB (220,220,220), fdCenterToVerti, (Rect.Bottom - Rect.Top) div 2);
    FCanvas.Brush.Style := BSCLEAR;
    if odSelected in State then
      FCanvas.TextOut (Rect.Left + 16, Rect.Top + (Rect.Bottom - Rect.Top - FCanvas.TextHeight ('A')) div 2, Tabs[Index])
    else
      FCanvas.TextOut (Rect.Left + 12, Rect.Top + (Rect.Bottom - Rect.Top - FCanvas.TextHeight ('A')) div 2, Tabs[Index])
  end
end;

procedure TDefinePages.WMAdjasment (var Msg : TMessage);
begin
  inherited;
  if Msg.WParam = 0 then
  begin
    InflateRect(PRect(Msg.LParam)^, 3, 3);
    Dec(PRect(Msg.LParam)^.Top, 1);
  end;
end;

{procedure TDefinePages.WMNCPaint (var Message : TWMNCPaint);
var
  NCCanvas : TCanvas;
begin
  inherited;
  NCCanvas := TCanvas.Create;
  try
    NCCanvas.Handle := GetWindowDC (Handle);
    NCCanvas.Brush.Color := clRed;
    NCCanvas.Brush.Style := bsClear;
    NCCanvas.Pen.Color := clSilver;
    NCCanvas.Rectangle (0, 30, Width-1, Height-1);
  finally
    NCCanvas.Free;
  end;
end;}

procedure TDefinePages.DrawHotTrackTab (ATabIndex : Integer; AHotTrack : Boolean);
var
  ItemRect    : TRect;
  DrawRect    : TRect;
  StartColor  : TColor;
  EndColor    : TColor;
begin
  if SendMessage (Handle, TCM_GETITEMRECT, ATabIndex, LongInt (@ItemRect)) <> 0 then
  begin
    DrawRect := ItemRect;
    StartColor := $2C8BE6;
    EndColor := $3CC7FF;
    case TabPosition of
      tpTop:    begin
                  DrawRect.Left := ItemRect.Left + 2;
                  DrawRect.Right := ItemRect.Right - 3;
                  DrawRect.Bottom := ItemRect.Top + 1;
                  if AHotTrack then
                  begin
                    StartColor := $2C8BE6;
                    EndColor := $3CC7FF;
                  end
                  else
                  begin
                    StartColor := FBorderColor;
                    EndColor := MakeDarkColor((Pages[ATabIndex] as TDefineSheet).Color, 5);
                  end;
                end;
      tpBottom: begin
                  DrawRect.Top := ItemRect.Bottom - 3;
                  DrawRect.Bottom := ItemRect.Bottom - 2;
                  DrawRect.Left := ItemRect.Left + 2;
                  DrawRect.Right := ItemRect.Right - 3;
                  if AHotTrack then
                  begin
                    StartColor := $3CC7FF;
                    EndColor := $2C8BE6;
                  end
                  else
                  begin
                    StartColor := MakeDarkColor ((Pages[ATabIndex] as TDefineSheet).Color, 20);
                    EndColor := FBorderColor;
                  end;
                end;
      tpLeft:   begin
                  DrawRect.Left := ItemRect.Left;
                  DrawRect.Top := ItemRect.Top+2;
                  DrawRect.Bottom := ItemRect.Bottom - 3;
                  DrawRect.Right := ItemRect.Left+1;
                  if AHotTrack then
                  begin
                    StartColor := $3CC7FF;
                    EndColor := $2C8BE6;
                  end
                  else
                  begin
                    StartColor := FBorderColor;
                    EndColor := MakeDarkColor ((Pages[ATabIndex] as TDefineSheet).Color, 20);
                  end;
                end;
      tpRight:  begin
                  DrawRect.Left := ItemRect.Right-1;
                  DrawRect.Top := ItemRect.Top+2;
                  DrawRect.Bottom := ItemRect.Bottom - 3;
                  DrawRect.Right := ItemRect.Right;
                  if AHotTrack then
                  begin
                    StartColor := $3CC7FF;
                    EndColor := $2C8BE6;
                  end
                  else
                  begin
                    StartColor := FBorderColor;
                    EndColor := MakeDarkColor ((Pages[ATabIndex] as TDefineSheet).Color, 20);
                  end;
                end;
    end;
    FCanvas.Handle := GetWindowDC (Handle);

    case TabPosition of
     tpTop, tpBottom:
       begin
         FCanvas.Pen.Color := StartColor;
         FCanvas.MoveTo (DrawRect.Left, DrawRect.Top );
         FCanvas.LineTo (DrawRect.Right, DrawRect.Top );
         FCanvas.Pen.Color := EndColor;
         FCanvas.MoveTo (DrawRect.Left, DrawRect.Bottom);
         FCanvas.LineTo (DrawRect.Right, DrawRect.Bottom);
       end;
     tpLeft,tpRight:
       begin
         FCanvas.Pen.Color := StartColor;
         FCanvas.MoveTo (DrawRect.Left, DrawRect.Top );
         FCanvas.LineTo (DrawRect.Left, DrawRect.Bottom);
         FCanvas.Pen.Color := EndColor;
         FCanvas.MoveTo (DrawRect.Right, DrawRect.Top);
         FCanvas.LineTo (DrawRect.Right, DrawRect.Bottom);
       end;
    end;   
  end;
end;


procedure TDefinePages.DrawItemInside(AIndex : Integer; ACanvas : TCanvas; ARect : TRect);
var
  dX       : Integer;
  ACaption : String;
  AFormat  : Integer;
  DrawRect : TRect;
begin
  ACanvas.Brush.Style := BSCLEAR;
  ACanvas.Font.Assign (Self.Pages[AIndex].Font);
  If Assigned (FImageList) then dX := FImageList.Width + 6 else dX := 0;

  DrawRect := ARect;
  InflateRect (DrawRect, -2, -2);
  DrawRect.Left := DrawRect.Left + dX;

  ACaption := Self.Pages[AIndex].Caption;

  AFormat := DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;

  case FTabTextAlignment of
    taLeftJustify: AFormat := AFormat or DT_LEFT;
    taRightJustify: AFormat := AFormat or DT_RIGHT;
    taCenter: AFormat := AFormat or DT_CENTER;
  end;

  ACanvas.Font.Color := MakeDarkColor((TDefineSheet(Self.Pages[AIndex]).Color), 30);
  OffsetRect (DrawRect, 1, 1);
  DrawText (ACanvas.Handle, PChar (ACaption), Length(ACaption), DrawRect, AFormat);

  ACanvas.Font.Color := Self.Pages[AIndex].Font.Color;
  OffsetRect (DrawRect, -1,-1);
  DrawText (ACanvas.Handle, PChar (ACaption), Length(ACaption), DrawRect, AFormat);

  if Assigned (FImageList) then
  begin
    FImageList.Draw (ACanvas, ARect.Left + 3,
                              (ARect.Top + ARect.Bottom - FImageList.Height) div 2,
                              (Self.Pages[AIndex] as TDefineSheet).ImageIndex);
  end;

end;


//============================================================================//
//===================== Tabs drawing procedures  =============================//
//============================================================================//


  //====================== Draw top tabs =============================//
procedure TDefinePages.DrawTopTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
var
  AActiveTab : Boolean;
  ATabColor  : TColor;
begin
  Dec (TabRect.Bottom,2);
  AActiveTab := (SendMessage (Handle, TCM_GETCURSEL, 0, 0) = AVisibleIndex);
  ATabColor := (Self.Pages [AIndex] as TDefineSheet).Color;
  if AActiveTab then
  begin
    Dec (TabRect.Top, 2);
    Dec (TabRect.Left, 2);
    Inc (TabRect.Right, 1);
  end
  else
  begin
    Dec (TabRect.Right);
    Dec (TabRect.Bottom);
    ATabColor := MakeDarkColor (ATabColor, 5);
  end;
  Inc (TabRect.Bottom, 1);

  ACanvas.Brush.Color := ATabColor;
  ACanvas.Pen.Color := FBorderColor;
  ACanvas.Rectangle (TabRect.Left, TabRect.Top + 6, TabRect.Right, TabRect.Bottom);
  ACanvas.RoundRect (TabRect.Left, TabRect.Top, TabRect.Right, TabRect.Bottom - 7, 6, 6);
  ACanvas.FillRect (Rect (TabRect.Left+1, TabRect.Top + 5, TabRect.Right-1, TabRect.Bottom));

  if AActiveTab then
  begin
    ACanvas.Brush.Color := ATabColor;
    ACanvas.Pen.Color := ATabColor;
    ACanvas.Rectangle (TabRect.Left+1, TabRect.Bottom-1, TabRect.Right-1, TabRect.Bottom+2);

    if HotTrack then
    begin
      FCanvas.Pen.Color := $2C8BE6;
      FCanvas.MoveTo (TabRect.Left + 2, TabRect.Top );
      FCanvas.LineTo (TabRect.Right - 2, TabRect.Top );
      FCanvas.Pen.Color := $3CC7FF;
      FCanvas.MoveTo (TabRect.Left + 2, TabRect.Top + 1);
      FCanvas.LineTo (TabRect.Right - 2, TabRect.Top + 1);
    end;
  end
  else
  begin
    //Draw tab vertical right shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Right-2, TabRect.Top+2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-1);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Right-3, TabRect.Top+4);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-2);

    //Draw tab horizontal bottom shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Left+2, TabRect.Bottom-1);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-1);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Left + 3, TabRect.Bottom - 2);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-2);

  end;


  //Draw text and image
  DrawItemInside (AIndex, ACanvas, TabRect);

end;

  //====================== Draw bottom tabs =============================//
procedure TDefinePages.DrawBottomTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
var
  AActiveTab : Boolean;
  ATabColor  : TColor;
begin
  Dec (TabRect.Bottom,2);
  AActiveTab := (SendMessage (Handle, TCM_GETCURSEL, 0, 0) = AVisibleIndex);
  ATabColor := (Self.Pages [AIndex] as TDefineSheet).Color;
  if AActiveTab then
  begin
    Inc (TabRect.Bottom, 1);
    Dec (TabRect.Left, 2);
    Inc (TabRect.Right, 1);
  end
  else
  begin
    Dec (TabRect.Right);
    Inc (TabRect.Top);
    ATabColor := MakeDarkColor (ATabColor, 5);
  end;
  Inc (TabRect.Bottom, 1);

  ACanvas.Brush.Color := ATabColor;
  ACanvas.Pen.Color := FBorderColor;
  ACanvas.Rectangle (TabRect.Left, TabRect.Top, TabRect.Right, TabRect.Bottom - 6);
  ACanvas.RoundRect (TabRect.Left, TabRect.Top+6, TabRect.Right, TabRect.Bottom, 6, 6);
  ACanvas.FillRect (Rect (TabRect.Left+1, TabRect.Top+6, TabRect.Right-1, TabRect.Bottom-3));

  if AActiveTab then
  begin
    ACanvas.Brush.Color := ATabColor;
    ACanvas.Pen.Color := ATabColor;
    ACanvas.Rectangle (TabRect.Left+1, TabRect.Top-1, TabRect.Right-1, TabRect.Top+2);

    if HotTrack then
    begin
      FCanvas.Pen.Color := $2C8BE6;
      FCanvas.MoveTo (TabRect.Left + 2, TabRect.Bottom -1);
      FCanvas.LineTo (TabRect.Right - 2, TabRect.Bottom -1);
      FCanvas.Pen.Color := $3CC7FF;
      FCanvas.MoveTo (TabRect.Left + 2, TabRect.Bottom);
      FCanvas.LineTo (TabRect.Right - 2, TabRect.Bottom);
    end;
  end
  else
  begin
    //Draw tab vertical right shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Right-2, TabRect.Top+2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-2);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Right-3, TabRect.Top+4);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-3);

    //Draw tab horizontal bottom shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Left+2, TabRect.Bottom-2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-2);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Left + 3, TabRect.Bottom - 3);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-3);
  end;


  //Draw text and image
  DrawItemInside (AIndex, ACanvas, TabRect);

end;


  //====================== Draw left tabs =============================//
procedure TDefinePages.DrawLeftTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
var
  AActiveTab : Boolean;
  ATabColor  : TColor;
begin
  Dec (TabRect.Bottom,2);
  AActiveTab := (SendMessage (Handle, TCM_GETCURSEL, 0, 0) = AVisibleIndex);
  ATabColor := (Self.Pages [AIndex] as TDefineSheet).Color;
  if AActiveTab then
  begin
    Dec (TabRect.Left, 2);
    Dec (TabRect.Top, 1);
    Inc (TabRect.Bottom, 1);
  end
  else
  begin
    Dec (TabRect.Right);
    ATabColor := MakeDarkColor (ATabColor, 5);
  end;
  Inc (TabRect.Bottom, 1);

  ACanvas.Brush.Color := ATabColor;
  ACanvas.Pen.Color := FBorderColor;
  ACanvas.Rectangle (TabRect.Left+6, TabRect.Top, TabRect.Right, TabRect.Bottom);
  ACanvas.RoundRect (TabRect.Left, TabRect.Top, TabRect.Left+8, TabRect.Bottom, 6, 6);
  ACanvas.FillRect (Rect (TabRect.Left+5, TabRect.Top + 1, TabRect.Right-1, TabRect.Bottom-1));

  if AActiveTab then
  begin

    if HotTrack then
    begin
      FCanvas.Pen.Color := $2C8BE6;
      FCanvas.MoveTo (TabRect.Left, TabRect.Top + 2);
      FCanvas.LineTo (TabRect.Left, TabRect.Bottom -2);
      FCanvas.Pen.Color := $3CC7FF;
      FCanvas.MoveTo (TabRect.Left + 1, TabRect.Top + 1);
      FCanvas.LineTo (TabRect.Left + 1, TabRect.Bottom - 1);
    end;
  end
  else
  begin
    //Draw tab vertical right shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Right-2, TabRect.Top+2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-1);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Right-3, TabRect.Top+4);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-2);

    //Draw tab horizontal bottom shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Left+2, TabRect.Bottom-2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-2);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Left + 3, TabRect.Bottom - 3);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-4);
  end;


  //Draw text and image
  DrawItemInside (AIndex, ACanvas, TabRect);

end;


  //====================== Draw right tabs =============================//
procedure TDefinePages.DrawRightTab (TabRect : TRect; ACanvas : TCanvas; AIndex, AVisibleIndex : Integer);
var
  AActiveTab : Boolean;
  ATabColor  : TColor;
begin
  Dec (TabRect.Bottom,2);
  AActiveTab := (SendMessage (Handle, TCM_GETCURSEL, 0, 0) = AVisibleIndex);
  ATabColor := (Self.Pages [AIndex] as TDefineSheet).Color;
  if AActiveTab then
  begin
    Inc (TabRect.Right, 2);
    Dec (TabRect.Top, 1);
    Inc (TabRect.Bottom, 1);
  end
  else
  begin
    Inc (TabRect.Left);
    ATabColor := MakeDarkColor (ATabColor, 5);
  end;
  Inc (TabRect.Bottom, 1);

  ACanvas.Brush.Color := ATabColor;
  ACanvas.Pen.Color := FBorderColor;
  ACanvas.Rectangle (TabRect.Left, TabRect.Top, TabRect.Right-6, TabRect.Bottom);
  ACanvas.RoundRect (TabRect.Right-8, TabRect.Top, TabRect.Right, TabRect.Bottom, 6, 6);
  ACanvas.FillRect (Rect (TabRect.Right-8, TabRect.Top + 1, TabRect.Right-3, TabRect.Bottom-1));

  if AActiveTab then
  begin

    if HotTrack then
    begin
      FCanvas.Pen.Color := $2C8BE6;
      FCanvas.MoveTo (TabRect.Right-2, TabRect.Top + 2);
      FCanvas.LineTo (TabRect.Right-2, TabRect.Bottom -2);
      FCanvas.Pen.Color := $3CC7FF;
      FCanvas.MoveTo (TabRect.Right-1, TabRect.Top + 1);
      FCanvas.LineTo (TabRect.Right-1, TabRect.Bottom - 1);
    end;
  end
  else
  begin
    //Draw tab vertical right shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Right-2, TabRect.Top+2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-1);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Right-3, TabRect.Top+4);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-2);

    //Draw tab horizontal bottom shadow line
    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 20);
    ACanvas.Brush.Color := ATabColor;
    ACanvas.MoveTo (TabRect.Left+2, TabRect.Bottom-2);
    ACanvas.LineTo (TabRect.Right-2, TabRect.Bottom-2);

    ACanvas.Pen.Color := MakeDarkColor (ATabColor, 10);
    ACanvas.MoveTo (TabRect.Left + 3, TabRect.Bottom - 3);
    ACanvas.LineTo (TabRect.Right-3, TabRect.Bottom-4);
  end;


  //Draw text and image
  DrawItemInside (AIndex, ACanvas, TabRect);

end;
//============================================================================//
//=================== End tabs drawing procedures  ===========================//
//============================================================================//



procedure TDefinePages.DrawBorder (ACanvas : TCanvas);
begin
   FCanvas.Brush.Style := BSCLEAR;
   FCanvas.Pen.Color := FBorderColor;

   FCanvas.Rectangle (FBorderRect.Left, FBorderRect.Top, FBorderRect.Right, FBorderRect.Bottom);
end;

procedure TDefinePages.WMPaint (var Message : TWMPaint);
var
  DC : hDC;
  PS : TPaintStruct;
  ItemRect : TRect;
  I : Integer;
  Index : Integer;
begin
 if FStyle <> pcsFlatStyle then
  begin
    inherited;
    Exit;
  end;  

  if Message.DC = 0 then DC := BeginPaint(Handle, PS) else DC := Message.DC;
  try
   FCanvas.Handle := DC;

   DrawBorder (FCanvas);

   if Self.PageCount > 0 then
   begin
     Index := 0;
     For I := 0 to Self.PageCount - 1 do
     begin
       if Pages [I].TabVisible then
       begin
         SendMessage (Handle, TCM_GETITEMRECT, Index, LongInt (@ItemRect));
         if (FOwnerDraw) and (Assigned (OnDrawItem)) then
         begin
           OnDrawItem (Self, I, FCanvas, ItemRect, []);
         end
         else
         begin
           Case TabPosition of
             tpTop: DrawTopTab (ItemRect, FCanvas, I, Index);
             tpBottom: DrawBottomTab (ItemRect, FCanvas, I, Index);
             tpLeft: DrawLeftTab (ItemRect, FCanvas, I, Index);
             tpRight: DrawRightTab (ItemRect, FCanvas, I, Index);
           end;
         end;
         Inc (Index);
       end;
     end;
   end;
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TDefinePages.WMSIZE (var Message : TWMSIZE);
begin
  inherited;
  FBorderRect := Self.BoundsRect;
  OffsetRect (FBorderRect, -FBorderRect.Left, -FBorderRect.Top);
  SendMessage (Handle, TCM_ADJUSTRECT, 0, LongInt (@FBorderRect));
  InflateRect (FBorderRect, 1, 1);
  Inc (FBorderRect.Top);
end;

procedure TDefinePages.WMMouseMove (var Message : TWMMouseMove);
var
  HitTest : TTCHitTestInfo;
  AActiveTab : Integer;
begin
  if FStyle <> pcsFlatStyle then
  begin
    inherited;
    Exit;
  end;   
  If not HotTrack then exit;

  HitTest.pt := Point (Message.XPos, Message.YPos);
  AActiveTab := SendMessage (Handle, TCM_HITTEST, 0, LongInt (@HitTest));
  if AActiveTab <> FHotTrackTab then
  begin
    if (FHotTrackTab <> SendMessage (Handle, TCM_GETCURSEL, 0, 0)) then
      DrawHotTrackTab (FHotTrackTab, False);

    FHotTrackTab := AActiveTab;

    if (FHotTrackTab <> -1) and (FHotTrackTab <> SendMessage (Handle, TCM_GETCURSEL, 0, 0)) then
      DrawHotTrackTab (FHotTrackTab, True);
  end;
end;


procedure TDefinePages.MouseLeave (var Message : TMessage);
begin
  If HotTrack and (FHotTrackTab <> -1) and (FHotTrackTab <> SendMessage (Handle, TCM_GETCURSEL, 0, 0)) then
  begin
    DrawHotTrackTab (FHotTrackTab, False);
    FHotTrackTab := -1;
  end;
end;


procedure TDefinePages.WMNCCalcSize (var Message : TWMNCCalcSize);
begin
  inherited;
end;

procedure TDefinePages.CMHintShow(var Message: TMessage);
var
  Tab   : TDefineSheet;
  ItemRect : TRect;
  HitTest : TTCHitTestInfo;
  AActiveTab : Integer;
  AWinActiveTab : Integer;
begin
  inherited;
  if TCMHintShow (Message).Result=1 then exit; // CanShow = false?

  with TCMHintShow(Message).HintInfo^ do
  begin
    if TControl(Self) <> HintControl then exit;

    HitTest.pt := Point (CursorPos.X, CursorPos.Y);
    AWinActiveTab := SendMessage (Handle, TCM_HITTEST, 0, LongInt (@HitTest));
    AActiveTab := WinIndexToPage (AWinActiveTab);

    if (AActiveTab >= 0) and (AActiveTab < Self.PageCount) then
    begin
       Tab := (Self.Pages [AActiveTab] as TDefineSheet);
       if not (Assigned(Tab) and (Tab.ShowTabHint) and (Tab.TabHint <> '')) then Exit;
    end
    else
      Exit;

     HintStr := GetShortHint(Tab.TabHint);
     SendMessage (Handle, TCM_GETITEMRECT, AWinActiveTab, LongInt (@ItemRect));
     CursorRect := ItemRect;
  end; //with
end;


{function TDefinePages.PageIndexToWin (AIndex : Integer) : Integer;
var
  I : Integer;
begin
  Result := -1;
  if (Self.PageCount <= 0) or (AIndex >= Self.PageCount) then Exit;
  if not Self.Pages[AIndex].TabVisible then Exit;
  For I := 0 to AIndex do
    if Self.Pages[I].TabVisible then Inc (Result);
end;     }


function TDefinePages.WinIndexToPage (AIndex : Integer) : Integer;
var
  I : Integer;
begin
  Result := -1;
  if (Self.PageCount <= 0) or (AIndex >= Self.PageCount) then Exit;
  I := 0;
  Result := 0;
  While (I <= AIndex) and (Result < Self.PageCount) do
  begin
    if Self.Pages[Result].TabVisible then Inc (I);
    Inc (Result);
  end;
  Dec (Result);
end;


procedure TDefinePages.WMSysColorChange (var Message: TMessage);
begin
  invalidate;
  inherited;
end;


procedure TDefinePages.Loaded;
begin
  inherited;
  SendMessage (Handle, WM_SIZE, 0, 0);
end;

procedure TDefinePages.SetBorderColor (Value : TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TDefinePages.SetTabPosition (Value : TPagesPosition);
begin
  if FTabPosition <> Value then
  begin
    if (FStyle in [pcsButtons, pcsFlatButtons]) and (Value <> tpTop) then
      raise Exception.Create ('Tab position incompatible with current tab style');
    FTabPosition := Value;
    RecreateWnd;
    SendMessage (Handle, WM_SIZE, 0, 0);
    if (Self.PageCount > 0) and (ActivePage <> nil) then
      ActivePage.Invalidate;
  end;
end;

procedure TDefinePages.SetTabTextAlignment (Value : TAlignment);
begin
  if Value <> FTabTextAlignment then
  begin
    FTabTextAlignment := Value;
    Invalidate;
  end;
end;


procedure TDefinePages.SetStyle (Value : TPagesStyle);
begin
  if FStyle <> Value then
  begin
    if (Value in [pcsButtons, pcsFlatButtons]) then TabPosition := tpTop;
    FStyle := Value;
    RecreateWnd;
    SendMessage (Handle, WM_SIZE, 0, 0);
    if (Self.PageCount > 0) and (ActivePage <> nil) then
      ActivePage.Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////


constructor TDefineSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBtnFace;
  FImageIndex := -1;
  FShowTabHint := False;
  FTabHint := '';
  FCanvas := TControlCanvas.Create;
  FBGImage := TBitmap.Create;
  FBGStyle := bgsNone;
  FGradientStartColor := clWhite;
  FGradientEndColor := clSilver;
  FGradientFillDir := fdTopToBottom;
end;

destructor TDefineSheet.Destroy;
begin
  try FCanvas.Free;
  except
  end;

  try FBGImage.Free;
  except
  end;

  inherited Destroy;
end;

procedure TDefineSheet.SetBGImage (AValue : TBitmap);
begin
  FBGImage.Assign (AValue);
  Invalidate;
  if (FBGImage.Empty) and (FBGStyle in [bgsTileImage, bgsStrechImage]) then
    FBGStyle := bgsNone;
end;

procedure TDefineSheet.SetBGStyle (AValue : TDefineSheetBGStyle);
begin
  if FBGStyle <> AValue then
  begin
    FBGStyle := AValue;
    Invalidate;
  end;
end;

procedure TDefineSheet.SetColor (AValue : TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    Invalidate;
    if Assigned (PageControl) then
    try
      PageControl.Invalidate;
    except
    end;
  end;
end;

procedure TDefineSheet.SetGradientStartColor (AValue : TColor);
begin
  if FGradientStartColor <> AValue then
  begin
    FGradientStartColor := AValue;
    Invalidate;
  end;
end;

procedure TDefineSheet.SetGradientEndColor (AValue : TColor);
begin
  if FGradientEndColor <> AValue then
  begin
    FGradientEndColor := AValue;
    Invalidate;
  end;
end;

procedure TDefineSheet.SetGradientFillDir (AValue : TFillDirection);
begin
  if FGradientFillDir <> AValue then
  begin
    FGradientFillDir := AValue;
    Invalidate;
  end;
end;

procedure TDefineSheet.WMPaint (var Message : TWMPaint);
begin
  Brush.Color := FColor;
  inherited;
end;

procedure TDefineSheet.WMEraseBkgnd (var Message : TWMEraseBkgnd);
var
  DC : hDC;
  PS : TPaintStruct;
begin
  if Message.DC = 0 then DC := BeginPaint(Handle, PS) else DC := Message.DC;
  try
    FCanvas.Handle := DC;
    Brush.Color := FColor;
    case FBGStyle of
      bgsNone: begin
                 FCanvas.Brush.Color := FColor;
                 FCanvas.FillRect (ClientRect);
               end;
      bgsGradient:
               begin
                 GradientFillRect (FCanvas, ClientRect, FGradientStartColor, FGradientEndColor, FGradientFillDir, 60);
               end;
      bgsTileImage:
               if not FBGImage.Empty then
               begin
                 TileImage(FCanvas, ClientRect, FBGImage);
               end
               else
               begin
                 FCanvas.Brush.Color := FColor;
                 FCanvas.FillRect (ClientRect);
               end;
      bgsStrechImage:
               if not FBGImage.Empty then
               begin
                 FCanvas.StretchDraw (ClientRect, FBGImage);
               end
               else
               begin
                 FCanvas.Brush.Color := FColor;
                 FCanvas.FillRect (ClientRect);
               end;
    end;
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end; 

procedure TDefineSheet.WMNCPaint (var Message : TWMNCPaint);
begin
  Brush.Color := FColor;
  inherited;
end;

procedure TDefineSheet.SetImageIndex (AIndex : Integer);
var
  Item : TTCItem;
begin
  if AIndex < -1 then AIndex := -1;
  if (FImageIndex <> AIndex) and Assigned (PageControl) then
  begin
    FImageIndex := AIndex;
    Item.iImage := FImageIndex;
    Item.mask := TCIF_IMAGE;
    SendMessage (PageControl.Handle, TCM_SETITEM, PageIndex, LongInt (@Item));
  end;
end;
{ TDefineBarcode }
const
  StartA       = '211412';
  StartB       = '211214';
  StartC       = '211232';
  Stop         = '2331112';
  {Pattern for Barcode EAN Charset A} {L1   S1   L2   S2}
  BARCode_EAN_A:array['0'..'9'] of string =
              (('2605'), { 0 } ('1615'), { 1 } ('1516'), { 2 } ('0805'), { 3 }
               ('0526'), { 4 } ('0625'), { 5 } ('0508'), { 6 } ('0706'), { 7 }
               ('0607'), { 8 } ('2506'));{ 9 }
  BARCode_EAN_B:array['0'..'9'] of string =
              (('0517'), { 0 } ('0616'), { 1 } ('1606'), { 2 } ('0535'), { 3 }
               ('1705'), { 4 } ('0715'), { 5 } ('3505'), { 6 } ('1525'), { 7 }
               ('2515'), { 8 } ('1507'));{ 9 }
{Pattern for Barcode EAN Charset C}		 {S1   L1   S2   L2}
  BARCode_EAN_C:array['0'..'9'] of string =
              (('7150' ), { 0 }('6160' ), { 1 } ('6061' ), { 2 }('5350' ), { 3 }
               ('5071' ), { 4 }('5170' ), { 5 } ('5053' ), { 6 }('5251' ), { 7 }
               ('5152' ), { 8 }('7051' ));{ 9 }
  BARCode_ParityEAN13:array[0..9, 1..6] of char =
              (('A', 'A', 'A', 'A', 'A', 'A'), { 0 } ('A', 'A', 'B', 'A', 'B', 'B'), { 1 }
               ('A', 'A', 'B', 'B', 'A', 'B'), { 2 } ('A', 'A', 'B', 'B', 'B', 'A'), { 3 }
               ('A', 'B', 'A', 'A', 'B', 'B'), { 4 } ('A', 'B', 'B', 'A', 'A', 'B'), { 5 }
               ('A', 'B', 'B', 'B', 'A', 'A'), { 6 } ('A', 'B', 'A', 'B', 'A', 'B'), { 7 }
               ('A', 'B', 'A', 'B', 'B', 'A'), { 8 } ('A', 'B', 'B', 'A', 'B', 'A'));{ 9 }
  BARCode_UPC_E:array['0'..'9', 1..6] of char =
              (('E', 'E', 'E', 'O', 'O', 'O' ), { 0 } ('E', 'E', 'O', 'E', 'O', 'O' ), { 1 }
               ('E', 'E', 'O', 'O', 'E', 'O' ), { 2 } ('E', 'E', 'O', 'O', 'O', 'E' ), { 3 }
               ('E', 'O', 'E', 'E', 'O', 'O' ), { 4 } ('E', 'O', 'O', 'E', 'E', 'O' ), { 5 }
               ('E', 'O', 'O', 'O', 'E', 'E' ), { 6 } ('E', 'O', 'E', 'O', 'E', 'O' ), { 7 }
               ('E', 'O', 'E', 'O', 'O', 'E' ), { 8 } ('E', 'O', 'O', 'E', 'O', 'E' ));  { 9 }
  BARCode_PostNet:array['0'..'9'] of string[10] =
              (('5151A1A1A1'),{0} ('A1A1A15151'),{1} ('A1A151A151'),{2}
               ('A1A15151A1'),{3} ('A151A1A151'),{4} ('A151A151A1'),{5}
               ('A15151A1A1'),{6} ('51A1A1A151'),{7} ('51A1A151A1'),{8}
               ('51A151A1A1'));{9}
  BARCode_MSI:array['0'..'9'] of string[8] =
              (('51515151'),{0} ('51515160'),{1} ('51516051'),{2}
               ('51516060'),{3} ('51605151'),{4} ('51605160'),{5}
               ('51606051'),{6} ('51606060'),{7} ('60515151'),{8}
               ('60515160'));{9}
  BARCode_25:array['0'..'9', 1..5] of char =
              (('0', '0', '1', '1', '0'),{0} ('1', '0', '0', '0', '1'),{1}
               ('0', '1', '0', '0', '1'),{2} ('1', '1', '0', '0', '0'),{3}
               ('0', '0', '1', '0', '1'),{4} ('1', '0', '1', '0', '0'),{5}
               ('0', '1', '1', '0', '0'),{6} ('0', '0', '0', '1', '1'),{7}
               ('1', '0', '0', '1', '0'),{8} ('0', '1', '0', '1', '0'));{9}
  BARCode_Codabar: array[0..19] of TCodabar =
              ((c:'1'; data:'5050615'), (c:'2'; data:'5051506'), (c:'3'; data:'6150505'),
               (c:'4'; data:'5060515'), (c:'5'; data:'6050515'), (c:'6'; data:'5150506'),
               (c:'7'; data:'5150605'), (c:'8'; data:'5160505'), (c:'9'; data:'6051505'),
               (c:'0'; data:'5050516'), (c:'-'; data:'5051605'), (c:'$'; data:'5061505'),
               (c:':'; data:'6050606'), (c:'/'; data:'6060506'), (c:'.'; data:'6060605'),
               (c:'+'; data:'5060606'), (c:'A'; data:'5061515'), (c:'B'; data:'5151506'), //'5151506'  '5061515'
               (c:'C'; data:'5051516'), (c:'D'; data:'5051615'));
  BARCode_39x : array[0..127] of string[2] =
              (('%U'), ('$A'), ('$B'), ('$C'), ('$D'), ('$E'), ('$F'), ('$G'),
               ('$H'), ('$I'), ('$J'), ('$K'), ('$L'), ('$M'), ('$N'), ('$O'),
               ('$P'), ('$Q'), ('$R'), ('$S'), ('$T'), ('$U'), ('$V'), ('$W'),
               ('$X'), ('$Y'), ('$Z'), ('%A'), ('%B'), ('%C'), ('%D'), ('%E'),
               (' ' ), ('/A'), ('/B'), ('/C'), ('/D'), ('/E'), ('/F'), ('/G'),
               ('/H'), ('/I'), ('/J'), ('/K'), ('/L'), ('/M'), ('/N'), ('/O'),
               ('0' ), ('1' ), ('2' ), ('3' ), ('4' ), ('5' ), ('6' ), ('7' ),
               ('8' ), ('9' ), ('/Z'), ('%F'), ('%G'), ('%H'), ('%I'), ('%J'),
               ('%V'), ('A' ), ('B' ), ('C' ), ('D' ), ('E' ), ('F' ), ('G' ),
               ('H' ), ('I' ), ('J' ), ('K' ), ('L' ), ('M' ), ('N' ), ('O' ),
               ('P' ), ('Q' ), ('R' ), ('S' ), ('T' ), ('U' ), ('V' ), ('W' ),
               ('X' ), ('Y' ), ('Z' ), ('%K'), ('%L'), ('%M'), ('%N'), ('%O'),
               ('%W'), ('+A'), ('+B'), ('+C'), ('+D'), ('+E'), ('+F'), ('+G'),
               ('+H'), ('+I'), ('+J'), ('+K'), ('+L'), ('+M'), ('+N'), ('+O'),
               ('+P'), ('+Q'), ('+R'), ('+S'), ('+T'), ('+U'), ('+V'), ('+W'),
               ('+X'), ('+Y'), ('+Z'), ('%P'), ('%Q'), ('%R'), ('%S'), ('%T'));
  BARCode_93x : array[0..127] of string[2] =
	            ((']U'), ('[A'), ('[B'), ('[C'), ('[D'), ('[E'), ('[F'), ('[G'),
               ('[H'), ('[I'), ('[J'), ('[K'), ('[L'), ('[M'), ('[N'), ('[O'),
               ('[P'), ('[Q'), ('[R'), ('[S'), ('[T'), ('[U'), ('[V'), ('[W'),
               ('[X'), ('[Y'), ('[Z'), (']A'), (']B'), (']C'), (']D'), (']E'),
               (' ' ), ('{A'), ('{B'), ('{C'), ('{D'), ('{E'), ('{F'), ('{G'),
               ('{H'), ('{I'), ('{J'), ('{K'), ('{L'), ('{M'), ('{N'), ('{O'),
               ('0' ), ('1' ), ('2' ), ('3' ), ('4' ), ('5' ), ('6' ), ('7' ),
               ('8' ), ('9' ), ('{Z'), (']F'), (']G'), (']H'), (']I'), (']J'),
               (']V'), ('A' ), ('B' ), ('C' ), ('D' ), ('E' ), ('F' ), ('G' ),
               ('H' ), ('I' ), ('J' ), ('K' ), ('L' ), ('M' ), ('N' ), ('O' ),
               ('P' ), ('Q' ), ('R' ), ('S' ), ('T' ), ('U' ), ('V' ), ('W' ),
               ('X' ), ('Y' ), ('Z' ), (']K'), (']L'), (']M'), (']N'), (']O'),
               (']W'), ('}A'), ('}B'), ('}C'), ('}D'), ('}E'), ('}F'), ('}G'),
               ('}H'), ('}I'), ('}J'), ('}K'), ('}L'), ('}M'), ('}N'), ('}O'),
               ('}P'), ('}Q'), ('}R'), ('}S'), ('}T'), ('}U'), ('}V'), ('}W'),
               ('}X'), ('}Y'), ('}Z'), (']P'), (']Q'), (']R'), (']S'), (']T'));
  BARCode_93: array[0..46] of TCode93 =
              ((c:'0'; data:'131112'), (c:'1'; data:'111213'), (c:'2'; data:'111312'),
               (c:'3'; data:'111411'), (c:'4'; data:'121113'), (c:'5'; data:'121212'),
               (c:'6'; data:'121311'), (c:'7'; data:'111114'), (c:'8'; data:'131211'),
               (c:'9'; data:'141111'), (c:'A'; data:'211113'), (c:'B'; data:'211212'),
               (c:'C'; data:'211311'), (c:'D'; data:'221112'), (c:'E'; data:'221211'),
               (c:'F'; data:'231111'), (c:'G'; data:'112113'), (c:'H'; data:'112212'),
               (c:'I'; data:'112311'), (c:'J'; data:'122112'), (c:'K'; data:'132111'),
               (c:'L'; data:'111123'), (c:'M'; data:'111222'), (c:'N'; data:'111321'),
               (c:'O'; data:'121122'), (c:'P'; data:'131121'), (c:'Q'; data:'212112'),
               (c:'R'; data:'212211'), (c:'S'; data:'211122'), (c:'T'; data:'211221'),
               (c:'U'; data:'221121'), (c:'V'; data:'222111'), (c:'W'; data:'112122'),
               (c:'X'; data:'112221'), (c:'Y'; data:'122121'), (c:'Z'; data:'123111'),
               (c:'-'; data:'121131'), (c:'.'; data:'311112'), (c:' '; data:'311211'),
               (c:'$'; data:'321111'), (c:'/'; data:'112131'), (c:'+'; data:'113121'),
               (c:'%'; data:'211131'),
               (c:'['; data:'121221'),   // only used for Extended Code 93
               (c:']'; data:'312111'),   // only used for Extended Code 93
               (c:'{'; data:'311121'),   // only used for Extended Code 93
               (c:'}'; data:'122211'));  // only used for Extended Code 93
  BARCode_39: array[0..43] of TCode39 =
              ((c:'0'; data:'505160605'; chk:0 ), (c:'1'; data:'605150506'; chk:1 ),
               (c:'2'; data:'506150506'; chk:2 ), (c:'3'; data:'606150505'; chk:3 ),
               (c:'4'; data:'505160506'; chk:4 ), (c:'5'; data:'605160505'; chk:5 ),
               (c:'6'; data:'506160505'; chk:6 ), (c:'7'; data:'505150606'; chk:7 ),
               (c:'8'; data:'605150605'; chk:8 ), (c:'9'; data:'506150605'; chk:9 ),
               (c:'A'; data:'605051506'; chk:10), (c:'B'; data:'506051506'; chk:11),
               (c:'C'; data:'606051505'; chk:12), (c:'D'; data:'505061506'; chk:13),
               (c:'E'; data:'605061505'; chk:14), (c:'F'; data:'506061505'; chk:15),
               (c:'G'; data:'505051606'; chk:16), (c:'H'; data:'605051605'; chk:17),
               (c:'I'; data:'506051600'; chk:18), (c:'J'; data:'505061605'; chk:19),
               (c:'K'; data:'605050516'; chk:20), (c:'L'; data:'506050516'; chk:21),
               (c:'M'; data:'606050515'; chk:22), (c:'N'; data:'505060516'; chk:23),
               (c:'O'; data:'605060515'; chk:24), (c:'P'; data:'506060515'; chk:25),
               (c:'Q'; data:'505050616'; chk:26), (c:'R'; data:'605050615'; chk:27),
               (c:'S'; data:'506050615'; chk:28), (c:'T'; data:'505060615'; chk:29),
               (c:'U'; data:'615050506'; chk:30), (c:'V'; data:'516050506'; chk:31),
               (c:'W'; data:'616050505'; chk:32), (c:'X'; data:'515060506'; chk:33),
               (c:'Y'; data:'615060505'; chk:34), (c:'Z'; data:'516060505'; chk:35),
               (c:'-'; data:'515050606'; chk:36), (c:'.'; data:'615050605'; chk:37),
               (c:' '; data:'516050605'; chk:38), (c:'*'; data:'515060605'; chk:0 ),
               (c:'$'; data:'515151505'; chk:39), (c:'/'; data:'515150515'; chk:40),
               (c:'+'; data:'515051515'; chk:41), (c:'%'; data:'505151515'; chk:42));
  BARCode_128: array[0..102] of TCode128 =
              ((a:' '; b:' '; c:'00'; data:'212222'; ),
               (a:'!'; b:'!'; c:'01'; data:'222122'; ),
               (a:'"'; b:'"'; c:'02'; data:'222221'; ),
               (a:'#'; b:'#'; c:'03'; data:'121223'; ),
               (a:'$'; b:'$'; c:'04'; data:'121322'; ),
               (a:'%'; b:'%'; c:'05'; data:'131222'; ),
               (a:'&'; b:'&'; c:'06'; data:'122213'; ),
               (a:'''';b:'''';c:'07'; data:'122312'; ),
               (a:'('; b:'('; c:'08'; data:'132212'; ),
               (a:')'; b:')'; c:'09'; data:'221213'; ),
               (a:'*'; b:'*'; c:'10'; data:'221312'; ),
               (a:'+'; b:'+'; c:'11'; data:'231212'; ),
               (a:'?'; b:'?'; c:'12'; data:'112232'; ),
               (a:'-'; b:'-'; c:'13'; data:'122132'; ),
               (a:'.'; b:'.'; c:'14'; data:'122231'; ),
               (a:'/'; b:'/'; c:'15'; data:'113222'; ),
               (a:'0'; b:'0'; c:'16'; data:'123122'; ),
               (a:'1'; b:'1'; c:'17'; data:'123221'; ),
               (a:'2'; b:'2'; c:'18'; data:'223211'; ),
               (a:'3'; b:'3'; c:'19'; data:'221132'; ),
               (a:'4'; b:'4'; c:'20'; data:'221231'; ),
               (a:'5'; b:'5'; c:'21'; data:'213212'; ),
               (a:'6'; b:'6'; c:'22'; data:'223112'; ),
               (a:'7'; b:'7'; c:'23'; data:'312131'; ),
               (a:'8'; b:'8'; c:'24'; data:'311222'; ),
               (a:'9'; b:'9'; c:'25'; data:'321122'; ),
               (a:':'; b:':'; c:'26'; data:'321221'; ),
               (a:';'; b:';'; c:'27'; data:'312212'; ),
               (a:'<'; b:'<'; c:'28'; data:'322112'; ),
               (a:'='; b:'='; c:'29'; data:'322211'; ),
               (a:'>'; b:'>'; c:'30'; data:'212123'; ),
               (a:'?'; b:'?'; c:'31'; data:'212321'; ),
               (a:'@'; b:'@'; c:'32'; data:'232121'; ),
               (a:'A'; b:'A'; c:'33'; data:'111323'; ),
               (a:'B'; b:'B'; c:'34'; data:'131123'; ),
               (a:'C'; b:'C'; c:'35'; data:'131321'; ),
               (a:'D'; b:'D'; c:'36'; data:'112313'; ),
               (a:'E'; b:'E'; c:'37'; data:'132113'; ),
               (a:'F'; b:'F'; c:'38'; data:'132311'; ),
               (a:'G'; b:'G'; c:'39'; data:'211313'; ),
               (a:'H'; b:'H'; c:'40'; data:'231113'; ),
               (a:'I'; b:'I'; c:'41'; data:'231311'; ),
               (a:'J'; b:'J'; c:'42'; data:'112133'; ),
               (a:'K'; b:'K'; c:'43'; data:'112331'; ),
               (a:'L'; b:'L'; c:'44'; data:'132131'; ),
               (a:'M'; b:'M'; c:'45'; data:'113123'; ),
               (a:'N'; b:'N'; c:'46'; data:'113321'; ),
               (a:'O'; b:'O'; c:'47'; data:'133121'; ),
               (a:'P'; b:'P'; c:'48'; data:'313121'; ),
               (a:'Q'; b:'Q'; c:'49'; data:'211331'; ),
               (a:'R'; b:'R'; c:'50'; data:'231131'; ),
               (a:'S'; b:'S'; c:'51'; data:'213113'; ),
               (a:'T'; b:'T'; c:'52'; data:'213311'; ),
               (a:'U'; b:'U'; c:'53'; data:'213131'; ),
               (a:'V'; b:'V'; c:'54'; data:'311123'; ),
               (a:'W'; b:'W'; c:'55'; data:'311321'; ),
               (a:'X'; b:'X'; c:'56'; data:'331121'; ),
               (a:'Y'; b:'Y'; c:'57'; data:'312113'; ),
               (a:'Z'; b:'Z'; c:'58'; data:'312311'; ),
               (a:'['; b:'['; c:'59'; data:'332111'; ),
               (a:'\'; b:'\'; c:'60'; data:'314111'; ),
               (a:']'; b:']'; c:'61'; data:'221411'; ),
               (a:'^'; b:'^'; c:'62'; data:'431111'; ),
               (a:'_'; b:'_'; c:'63'; data:'111224'; ),
               (a:' '; b:'`'; c:'64'; data:'111422'; ),
               (a:' '; b:'a'; c:'65'; data:'121124'; ),
               (a:' '; b:'b'; c:'66'; data:'121421'; ),
               (a:' '; b:'c'; c:'67'; data:'141122'; ),
               (a:' '; b:'d'; c:'68'; data:'141221'; ),
               (a:' '; b:'e'; c:'69'; data:'112214'; ),
               (a:' '; b:'f'; c:'70'; data:'112412'; ),
               (a:' '; b:'g'; c:'71'; data:'122114'; ),
               (a:' '; b:'h'; c:'72'; data:'122411'; ),
               (a:' '; b:'i'; c:'73'; data:'142112'; ),
               (a:' '; b:'j'; c:'74'; data:'142211'; ),
               (a:' '; b:'k'; c:'75'; data:'241211'; ),
               (a:' '; b:'l'; c:'76'; data:'221114'; ),
               (a:' '; b:'m'; c:'77'; data:'413111'; ),
               (a:' '; b:'n'; c:'78'; data:'241112'; ),
               (a:' '; b:'o'; c:'79'; data:'134111'; ),
               (a:' '; b:'p'; c:'80'; data:'111242'; ),
               (a:' '; b:'q'; c:'81'; data:'121142'; ),
               (a:' '; b:'r'; c:'82'; data:'121241'; ),
               (a:' '; b:'s'; c:'83'; data:'114212'; ),
               (a:' '; b:'t'; c:'84'; data:'124112'; ),
               (a:' '; b:'u'; c:'85'; data:'124211'; ),
               (a:' '; b:'v'; c:'86'; data:'411212'; ),
               (a:' '; b:'w'; c:'87'; data:'421112'; ),
               (a:' '; b:'x'; c:'88'; data:'421211'; ),
               (a:' '; b:'y'; c:'89'; data:'212141'; ),
               (a:' '; b:'z'; c:'90'; data:'214121'; ),
               (a:' '; b:'{'; c:'91'; data:'412121'; ),
               (a:' '; b:'|'; c:'92'; data:'111143'; ),
               (a:' '; b:'}'; c:'93'; data:'111341'; ),
               (a:' '; b:'~'; c:'94'; data:'131141'; ),
               (a:' '; b:' '; c:'95'; data:'114113'; ),
               (a:' '; b:' '; c:'96'; data:'114311'; ),
               (a:' '; b:' '; c:'97'; data:'411113'; ),
               (a:' '; b:' '; c:'98'; data:'411311'; ),
               (a:' '; b:' '; c:'99'; data:'113141'; ),
               (a:' '; b:' '; c:'  '; data:'114131'; ),
               (a:' '; b:' '; c:'  '; data:'311141'; ),
               (a:' '; b:' '; c:'  '; data:'411131'; ));

  BCData:array[Code25IL..UPC_S5] of TBCData =
        ((Name:'Code InterLeaved 2.5'; num:True),
	       (Name:'Code Industrial 2.5';  num:True),
	       (Name:'Code Matrix 2.5';      num:True),
	       (Name:'Code 39';              num:False),
	       (Name:'Code 39 Extended';     num:False),
	       (Name:'Code 128A';            num:False),
	       (Name:'Code 128B';            num:False),
	       (Name:'Code 128C';            num:True),
	       (Name:'Code 93';              num:False),
         (Name:'Code 93 Extended';     num:False),
	       (Name:'Code MSI';             num:True),
	       (Name:'Code PostNet';         num:True),
	       (Name:'Codabar';              num:False),
	       (Name:'EAN-8';                num:True),
	       (Name:'EAN-13';               num:True),
         (Name:'EAN-128A';             num:False),
	       (Name:'EAN-128B';             num:False),
	       (Name:'EAN-128C';             num:True),
	       (Name:'UPC-A';                num:True),
	       (Name:'UPC-EODD';             num:True),
	       (Name:'UPC-EVEN';             num:True),
	       (Name:'UPC-Supp2';            num:True),
	       (Name:'UPC-Supp5';            num:True));

{assist function}
function getSupp(Nr : String) : String;
var i,fak,sum : Integer;
		  tmp   : String;
begin
	sum := 0;
	tmp := copy(nr,1,Length(Nr)-1);
	fak := Length(tmp);
	for i:=1 to length(tmp) do
	begin
		if (fak mod 2) = 0 then
			sum := sum + (StrToInt(tmp[i])*9)
		else
			sum := sum + (StrToInt(tmp[i])*3);
		dec(fak);
	end;
	sum:=((sum mod 10) mod 10) mod 10;
	result := tmp+IntToStr(sum);
end;

{$ifndef WIN32}
function Trim(const S: string): string; export;
{ Removes leading and trailing whitespace from s}
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
	 while S[L] <= ' ' do Dec(L);
	 Result := Copy(S, I, L - I + 1);
  end;
end;
{$endif}

function Convert(s:string): string;
var i, v : integer;
    t : string;
begin
 t := '';
 for i:=1 to Length(s) do
 begin
  v := ord(s[i]) - 1;
  if odd(i) then
     Inc(v, 5);
  t := t + Chr(v);
 end;
 Convert := t;
end;

function Quersumme(x:integer):integer;
var sum:integer;
begin
 sum := 0;
 while x > 0 do
  begin
   sum := sum + (x mod 10);
   x := x div 10;
  end;
 result := sum;
end;

constructor TDefineBarcode.Create(Owner:TComponent);
begin
  fBitmap       := TBitmap.Create;
  inherited Create(owner);
  Font.OnChange := FontChange;
  Height       := 50;
  Width        := 100;
  fBarColor    := clBlack;
  fColor       := clWhite;
  fRotateType  := raNone;
  fAutoSize    := true;
  fRatio       := 2.0;
  fModul       := 1;
  fCodeType    := EAN13;
  fBarHeight   := 35;
  fBorderWidth := 5;
  fBarTop      := 5;
  fCheckSum    := FALSE;
  fShowText    := True;
  fTransparent := false;
  fCheckOdd    := true;
  fText        := '0123456789';
end;

destructor TDefineBarcode.destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

function TDefineBarcode.SetLen(pI: byte): string;
begin
 Result := fText;
 while Length(Result) < pI do
       Result:=Result+'0';
end;

function TDefineBarcode.DoCheckSumming(const Data: string;OddCheck:Boolean=True): string;
var i,sum,s : Integer;
begin
  sum := 0;
  for i:=1 to Length(data) do
  begin
    s := StrToInt(Data[i]);
    if OddCheck then
     begin
      if odd(i) then
         sum := sum + s
      else
         sum := sum + s*3;
      end
    else
     begin
      if odd(i) then
         sum := sum + s*3
      else
         sum := sum + s;
     end;
  end;
  if (sum mod 10) = 0 then
      result := data+'0'
  else
      result := data+IntToStr(10-(sum mod 10));
end;

function  TDefineBarcode.GetCheckLen(CodeType:TDefineBarcodeType;Data:String): string;
begin
 result := Data;
 case CodeType of
     EAN13:Begin
           if Length(Result)>12 then
              result := Copy(Result,1,12)
           else
              result := SetLen(12);
           result := DoCheckSumming(Result,fCheckOdd);
           end;
     EAN8:begin
           if Length(Result)>7 then
              result := Copy(Result,1,7)
           else
              result := SetLen(7);
           result := DoCheckSumming(result,fCheckOdd);
          end;
     UPC_A:begin
           if Length(Result)>11 then
              result := Copy(Result,1,11)
           else
              result := SetLen(11);
           result := DoCheckSumming(result,fCheckOdd);
           end;
     UPC_EODD,UPC_EVEN:
           begin
            if Length(Result)>6 then
               result := Copy(Result,1,6)
            else
               result := SetLen(6);
            result := DoCheckSumming(result,fCheckOdd);
           end;
     UPC_S2:
           begin
            if Length(Result)>2 then
               result := Copy(Result,1,2)
            else
               result := SetLen(2);
            result := getSupp(copy(Result,1,2)+'0');
           end;
     UPC_S5:
           begin
            if Length(Result)>5 then
               result := Copy(Result,1,5)
            else
               result := SetLen(5);
            result := getSupp(copy(Result,1,5)+'0');
           end;
    end;
end;

function  TDefineBarcode.ClearNotText(Value:String): string;
var inx:Integer;TempValue: string;
begin
    result := '';
    case CodeType of
      Code25IL, Code25IT, Code25Mx,
      CodeMSI, PostNet, EAN13, EAN8,
      UPC_A, UPC_EODD, UPC_EVEN, UPC_S2,
      Code128C,EAN128A,EAN128B,EAN128C,
      UPC_S5: begin
                TempValue := UpperCase(Value);
                for inx:=1 to Length(TempValue) do
                  if TempValue[Inx] in ['0'..'9'] then
                     result := result + TempValue[Inx];
                result := GetCheckLen(CodeType,result);
              end;
      Codabar:begin
                TempValue := UpperCase(Value);
                for inx:=1 to Length(TempValue) do
                  if TempValue[Inx] in ['0'..'9','A'..'B','-','$',':','/','.','+'] then
                     Result := result + TempValue[Inx];
              end;
      Code39, Code93:
              Begin
                result := UpperCase(Value);
              end;
      Code93Ext:
              Begin
                for inx:=0 to Length(Value) do
	         begin
	           if ord(Value[inx]) <= 127 then
		      result := result + BARCode_93x[ord(Value[inx])];
	         end;
              end;
      Code39Ext:
              begin
                for inx:=0 to Length(Value) do
                 begin
                   if ord(value[inx]) <= 127 then
                      result := result + BARCode_39x[ord(value[inx])];
                 end;
              end;
    else
      result := Value;
    end;
end;

function TDefineBarcode.MakeBarText: String;
begin
 result := ClearNotText(fText);
end;

function TDefineBarcode.Code_25ILeaved: string;
var i, j: integer;
    c : char;
begin
 result := result + '5050';   // Startcode
 for i:=1 to Length(BarText) div 2 do
  begin
   for j:= 1 to 5 do
    begin
     if BARCode_25[BarText[i*2-1], j] = '1' then
        c := '6'
     else
        c := '5';
     result := result + c;
     if BARCode_25[BarText[i*2], j] = '1' then
        c := '1'
     else
        c := '0';
     result := result + c;
    end;
  end;
 result := result + '605';    // Stopcode
end;

function TDefineBarcode.Code_25ITrial: string;
var i, j: integer;
begin
 result := result + '606050';   // Startcode
 for i:=1 to Length(BarText) do
  begin
   for j:= 1 to 5 do
    begin
    if BARCode_25[BarText[i], j] = '1' then
       result := result + '60'
    else
       result := result + '50';
    end;
  end;
 result := result + '605060';   // Stopcode
end;

function TDefineBarcode.Code_25Matrix: string;
var i, j: integer;c :char;
begin
 result := result + '705050';   // Startcode
 for i:=1 to Length(BarText) do
 begin
  for j:= 1 to 5 do
   begin
    if BARCode_25[BarText[i], j] = '1' then
       c := '1'
    else
       c := '0';
  if odd(j) then
     c := chr(ord(c)+5);
  result := result + c;
 end;
result := result + '0';   // L點ke zwischen den Zeichen
end;
result := result + '70505';   // Stopcode
end;

function TDefineBarcode.Code_39: string;
 function FindIdx(z:char):integer;
 var i:integer;
 begin
  for i:=0 to High(BARCode_39) do
   begin
    if z = BARCode_39[i].c then
     begin
       result := i;
       exit;
     end;
   end;
  result := -1;
 end;
var i, idx , checksum:integer; S:STRING;
begin
 checksum := 0;// Startcode
 S := BARCode_39[FindIdx('*')].data;
 result := S + '0';
 for i:=1 to Length(BarText) do
  begin
   idx := FindIdx(BarText[i]);
   if idx < 0 then
      continue;
   result := result + BARCode_39[idx].data + '0';
   Inc(checksum, BARCode_39[idx].chk);
  end;// Calculate Checksum Data
 if FCheckSum then
 begin
  checksum := checksum mod 43;
  for i:=0 to High(BARCode_39) do
   if checksum = BARCode_39[i].chk then
    begin
     result := result + BARCode_39[i].data + '0';
     break;
    end;
 end;// Stopcode
 result := result + BARCode_39[FindIdx('*')].data;
end;

{Code 128}
function TDefineBarcode.Code_128: string;
 function Find_Code128AB(c:char):integer;  // find Code 128 Codeset A or B
 var i:integer; v:char;
 begin
  for i:=0 to High(BARCode_128) do
   begin
    if FCodeType = Code128A then
       v := BARCode_128[i].a
    else
       v := BARCode_128[i].b;
    if c = v then
     begin
       result := i;
       exit;
     end;
   end;
  result := -1;
 end;
 function Find_Code128C(c:String):integer;  // find Code 128 Codeset C
 var i:integer;
 begin
  for i:=0 to High(BARCode_128) do
   begin
    if c = BARCode_128[i].C then
     begin
       result := i;
       exit;
     end;
   end;
  result := -1;
 end;
var i, idx , j: integer;
    startcode,Tmp: string;
    checksum : integer;
    codeword_pos : integer;
begin
 checksum := 103;
 case CodeType of
  Code128A,EAN128A: begin checksum := 103; startcode:= StartA; end;
  Code128B,EAN128B: begin checksum := 104; startcode:= StartB; end;
  Code128C,EAN128C: begin checksum := 105; startcode:= StartC; end;
 end;
 result := Convert(startcode);    // Startcode
 codeword_pos := 1;
 Tmp := BarText;
 case CodeType of
    EAN128A,
    EAN128B,
    EAN128C:
      begin
	result := result + Convert(BARCode_128[102].data);
	inc(checksum, 102*codeword_pos);
	Inc(codeword_pos);
	if FCheckSum then Tmp:=DoCheckSumming(Tmp);
      end;
 end;
 if (CodeType = Code128C) or (CodeType = EAN128C) then
  begin
    if ODD(Length(Tmp)) then //check Length(Tmp) for ODD or EVEN;//
       Tmp:='0'+Tmp;
    for i:=1 to (Length(Tmp) div 2) do
        begin
	  j:=(i-1)*2+1;
	  idx:=Find_Code128C(copy(Tmp,j,2));
	  if idx < 0 then
             idx := Find_Code128C('00');
	  result := result + Convert(BARCode_128[idx].data);
	  Inc(checksum, idx*codeword_pos);
	  Inc(codeword_pos);
        end;
   end
 else
  for i:=1 to Length(Tmp) do
   begin
    idx := Find_Code128AB(Tmp[i]);
    if idx < 0 then
       idx := Find_Code128AB(' ');
    result := result + Convert(BARCode_128[idx].data);
    Inc(checksum, idx*i);
   end;
 checksum := checksum mod 103;
 result := result + Convert(BARCode_128[checksum].data);
 result := result + Convert(Stop);      {Stopcode}
end;

function TDefineBarcode.Code_93: string;
 function Find_Code93(c:char):integer;// find Code 93
 var i:integer;
 begin
  for i:=0 to High(BARCode_93) do
   begin
    if c = BARCode_93[i].c then
     begin
      result := i;
      exit;
     end;
   end;
  result := -1;
 end;
var i, idx : integer;
    checkC, checkK,   // Checksums
    weightC, weightK : integer;
begin
 result := Convert('111141');
 for i:=1 to Length(BarText) do
  begin
   idx := Find_Code93(BarText[i]);
   if idx < 0 then
    raise Exception.CreateFmt('%s:Code93 bad Data <%s>', [self.ClassName,BarText]);
   result := result + Convert(BARCode_93[idx].data);
  end;
 checkC := 0;
 checkK := 0;
 weightC := 1;
 weightK := 2;
 for i:=Length(BarText) downto 1 do
  begin
   idx := Find_Code93(BarText[i]);
   Inc(checkC, idx*weightC);
   Inc(checkK, idx*weightK);
   Inc(weightC);
   if weightC > 20 then weightC := 1;
      Inc(weightK);
   if weightK > 15 then weightC := 1;
  end;
 Inc(checkK, checkC);
 checkC := checkC mod 47;
 checkK := checkK mod 47;
 result := result + Convert(BARCode_93[checkC].data) +
 Convert(BARCode_93[checkK].data);
 result := result + Convert('1111411');   // Stopcode
end;

function TDefineBarcode.Code_MSI: string;
var i,check_even, check_odd, checksum:integer;
begin
 result := '60';    // Startcode
 check_even := 0;
 check_odd  := 0;
 for i:=1 to Length(BarText) do
  begin
   if odd(i-1) then
      check_odd := check_odd*10+ord(BarText[i])
   else
      check_even := check_even+ord(BarText[i]);
   result := result + BARCode_MSI[BarText[i]];
  end;
 checksum := quersumme(check_odd*2) + check_even;
 checksum := checksum mod 10;
 if checksum > 0 then
    checksum := 10-checksum;
 result := result + BARCode_MSI[chr(ord('0')+checksum)];
 result := result + '515'; // Stopcode
end;

function TDefineBarcode.Code_PostNet: string;
var i:integer;
begin
 result := '51';
 for i:=1 to Length(BarText) do
  begin
   result := result + BARCode_PostNet[BarText[i]];
  end;
 result := result + '5';
end;

function TDefineBarcode.Code_CodaBar: string;
 function Find_Codabar(c:char):integer;
 var i:integer;
 begin
  for i:=0 to High(BARCode_Codabar) do
   begin
    if c = BARCode_Codabar[i].c then
     begin
      result := i;
      exit;
     end;
   end;
  result := -1;
 end;
var i, idx : integer; S:STRING;
begin
 S := BARCode_Codabar[Find_Codabar('A')].data;
 result := S + '0';
 for i:=1 to Length(BarText) do
  begin
   idx := Find_Codabar(BarText[i]);
   result := result + BARCode_Codabar[idx].data + '0';
  end;
 result := result + BARCode_Codabar[Find_Codabar('B')].data;
// result := result + BARCode_Codabar[Find_Codabar('A')].data;
end;

function TDefineBarcode.Code_EAN13: string;
var I, LK: integer;
    tmp : String;
begin
 LK := StrToInt(BarText[1]);
 tmp := copy(BarText,2,12);
 result := '505';{Startcode}
 for i:=1 to 6 do
  begin
   case BARCode_ParityEAN13[LK,i] of
    'A' : result := result + BARCode_EAN_A[tmp[i]];
    'B' : result := result + BARCode_EAN_B[tmp[i]] ;
    'C' : result := result + BARCode_EAN_C[tmp[i]] ;
   end;
 end;
 result := result + '05050';{Center Guard Pattern}
 for i:=7 to 12 do
     result := result + BARCode_EAN_C[tmp[i]] ;
 result := result + '505';{Stopcode}
end;

function TDefineBarcode.Code_EAN8: string;
var i : integer;
begin
 result := '505';{Startcode}
 for i:=1 to 4 do
     result := result + BARCode_EAN_A[BarText[i]] ;
 result := result + '05050';{Center Guard Pattern}
 for i:=5 to 8 do
     result := result + BARCode_EAN_C[BarText[i]] ;
 result := result + '505';{Stopcode}
end;

function TDefineBarcode.Code_Supp2: string;
var     i,j : integer;
        mS : String;
begin
	i:=StrToInt(Copy(BarText,1,2));
	case i mod 4 of
		3: mS:='EE';
		2: mS:='EO';
		1: mS:='OE';
		0: mS:='OO';
	end;
	result := '506';{Startcode}
	for i:=1 to 2 do
	begin
	  if mS[i]='E' then
	   begin
	     for j:= 1 to 4 do
                 result := result + BARCode_EAN_C[BarText[i],5-j];
	   end
	  else
	   begin
	     result := result + BARCode_EAN_A[BarText[i]];
	   end;
	  if i<2 then
             result:=result+'05'; // character delineator
	end;
end;

function TDefineBarcode.Code_Supp5: string;
var i,j : integer;
    c   : char;
begin
	c:=BarText[6];
	result := '506';{Startcode}
	for i:=1 to 5 do
	begin
	 if BARCode_UPC_E[c,(6-5)+i]='E' then
	  begin
	    for j:= 1 to 4 do result := result + BARCode_EAN_C[BarText[i],5-j];
	  end
	 else
	  begin
	    result := result + BARCode_EAN_A[BarText[i]];
	  end;
	 if i<5 then result:=result+'05'; // character delineator
	end;
end;

function TDefineBarcode.Code_UPC_A: string;
var	i : integer;
begin
	result := '505';{Startcode}
	for i:=1 to 6 do
	    result := result + BARCode_EAN_A[BarText[i]];
	result := result + '05050';{Trennzeichen}
	for i:=7 to 12 do
	    result := result + BARCode_EAN_C[BarText[i]];
	result := result + '505';{Stopcode}
end;

function TDefineBarcode.Code_UPC_EODD: string;
var     i,j : integer;
	c   : char;
begin
	c:=BarText[7];
	result := '505';{Startcode}
	for i:=1 to 6 do
	begin
	   if BARCode_UPC_E[c,i]='E' then
	   begin
	     for j:= 1 to 4 do
                 result := result + BARCode_EAN_C[BarText[i],5-j];
	   end
	   else
	   begin
	         result := result + BARCode_EAN_A[BarText[i]];
	   end;
	end;
	result := result + '0505';{Stopcode}
end;

function TDefineBarcode.Code_UPC_EVEN: string;
var     i,j : integer;
	c   : char;
begin
	c:=BarText[7];
	result := '505';{Startcode}
	for i:=1 to 6 do
	begin
	  if BARCode_UPC_E[c,i]='E' then
	   begin
	     result := result + BARCode_EAN_A[BarText[i]];
	   end
	 else
	   begin
	     for j:= 1 to 4 do
               result := result + BARCode_EAN_C[BarText[i],5-j];
	   end;
	end;
	result := result + '0505';{Stopcode}
end;

procedure TDefineBarcode.GetABCED(Var a,b,c,d,orgin:TPoint;xadd,Width,Height:Integer);
begin
 a.x := xadd;
 a.y := Orgin.y;//0

 b.x := xadd;
 b.y := Orgin.y+height;

 c.x := xadd+width-1;
 c.y := Orgin.y+height;

 d.x := xadd+width-1;
 d.y := Orgin.y;//0
end;

function TDefineBarcode.MakeData;
begin
 case CodeType of
  Code25IL      : result := Code_25ILeaved;
  Code25IT      : result := Code_25ITrial;
  Code25Mx      : result := Code_25Matrix;
  Code39,
  Code39Ext     : result := Code_39;
  Code93,
  Code93Ext     : result := Code_93;
  CodeMSI       : result := Code_MSI;
  PostNet       : result := Code_PostNet;
  CodaBar       : result := Code_CodaBar;
  EAN8          : Result := Code_EAN8;
  EAN13         : Result := Code_EAN13;
  UPC_A         : Result := Code_UPC_A;
  UPC_EODD      : Result := Code_UPC_EODD;
  UPC_EVEN      : Result := Code_UPC_EVEN;
  UPC_S2        : Result := Code_Supp2;
  UPC_S5        : Result := Code_Supp5;
 else
  result        := Code_128; //for Code128A,Code128B,Code128C;EAN128A,EAN128B,EAN128C
 end;
end;

function TDefineBarcode.MakeModules:TDefineBarcodeModules;
begin
 case CodeType of
  Code25IL, Code25IT, Code39,
  Code39Ext, Codabar, EAN8, EAN13,
  UPC_A, UPC_EODD, UPC_EVEN, UPC_S2,
  UPC_S5:begin
                 if fRatio <> 2.0 then
                    fRatio := 2.0;
                end;
  Code25Mx :begin
                 if fRatio < 2.25 then
                    fRatio := 2.25;
                 if fRatio > 3.0  then
                    fRatio := 3.0;
           end;
  Code128A, Code128B, Code128C,
  EAN128A, EAN128B, EAN128C,
  Code93,Code93Ext, CodeMSI,
  PostNet:;
 end;
 Result[0] := fModul;
 Result[1] := Round(fModul*fRatio);
 Result[2] := Result[1] * 3 div 2;
 Result[3] := Result[1] * 2;
end;

{Print the Barcode data :0-3 white Line;5-8 black Line;A-D black Line (2/5 in Height)}
procedure TDefineBarcode.OneBarProps(Data:Char;Var Width:Integer;var lt:TDefineBarcodeLines);
begin
 case data of
   '0': begin width := Modules[0]; lt := ltWhite; end;
   '1': begin width := Modules[1]; lt := ltWhite; end;
   '2': begin width := Modules[2]; lt := ltWhite; end;
   '3': begin width := Modules[3]; lt := ltWhite; end;

   '5': begin width := Modules[0]; lt := ltBlack; end;
   '6': begin width := Modules[1]; lt := ltBlack; end;
   '7': begin width := Modules[2]; lt := ltBlack; end;
   '8': begin width := Modules[3]; lt := ltBlack; end;

   'A': begin width := Modules[0]; lt := ltBlack_half; end;
   'B': begin width := Modules[1]; lt := ltBlack_half; end;
   'C': begin width := Modules[2]; lt := ltBlack_half; end;
   'D': begin width := Modules[3]; lt := ltBlack_half; end;
 end;
end;

procedure TDefineBarcode.DrawUPC_AText(Canvas:TCanvas;width,wBorder:Integer);
var x,y,tCenter:Integer;
    Rect:TRect;
    str:String;
begin
 with Canvas do
  begin
        x           := wBorder - TextWidth('1')-2;
        y           := fBarHeight+fBarTop-(TextHeight('A') div 2);
        str         := BarText[1];
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := x+TextWidth(Str);
        Rect.Bottom := y+TextHeight(Str);
        TextRect(Rect,x,y,Str);
        Str         := Copy(BarText,2,5);
        x           := wBorder + ProLine;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := (width-ProLine) div 2;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
        str         := Copy(BarText,7,5);
        x           := (Width + ProLine)div 2;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := width - wBorder - ProLine;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
        str         := BarText[12];
        x           := Width - wBorder;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := width;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
  end;
end;

procedure TDefineBarcode.DrawEAN8Text(Canvas:TCanvas;width,wBorder:Integer);
var x,y,tCenter:Integer;
    Rect:TRect;
    str:String;
begin
 with Canvas do
  begin
        y           := fBarHeight+fBarTop-(TextHeight('A') div 2);
        str         := copy(BarText,1,4);
        x           := wBorder + ProLine;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := (width-ProLine) div 2;
        Rect.Bottom := y+TextHeight(Str);
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
        str         := copy(BarText,5,4);
        x           := (Width + ProLine)div 2;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := width - wBorder - ProLine;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
  end;
end;

procedure TDefineBarcode.DrawUPC_EText(Canvas:TCanvas;width,wBorder:Integer);
var x,y,tCenter:Integer;
    Rect:TRect;
    str:String;
begin
 with Canvas do
  begin
        y           := fBarHeight+fBarTop-(TextHeight('A') div 2);
        str         := copy(BarText,1,6);
        x           := wBorder + ProLine;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := width - wBorder - ProLine;
        Rect.Bottom := y+TextHeight(Str);
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
  end;
end;

procedure TDefineBarcode.DrawEAN13Text(Canvas:TCanvas;width,wBorder:Integer);
var x,y,tCenter:Integer;
    Rect:TRect;
    str:String;
begin
 with Canvas do
  begin
        x           := wBorder - TextWidth('1')-2;
        y           := fBarHeight+fBarTop-(TextHeight('A') div 2);
        str         := BarText[1];
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := x+TextWidth(Str);
        Rect.Bottom := y+TextHeight(Str);
        TextRect(Rect,x,y,Str);
        Str         := Copy(BarText,2,6);
        x           := wBorder + ProLine;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := (width-ProLine) div 2;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
        str         := Copy(BarText,8,6);
        x           := (Width + ProLine)div 2;
        Rect.Left   := x;
        Rect.Top    := y;
        Rect.Right  := width - wBorder - ProLine;
        tCenter     := (Rect.Right + x - TextWidth(str)) div 2;
        TextRect(Rect,tCenter,y,Str);
  end;
end;

procedure TDefineBarcode.DrawBarcode;
var tCenter,i,xadd, x, y:Integer;
    lt : TDefineBarcodeLines;
    fwidth, fheight,wBorder:integer;
    a,b,c,d, orgin : TPoint;
    bmpMem:TBitmap;
    Rect:TRect;
    str:String;
begin
 bmpMem:= TBitmap.Create;
 try
 with bmpMem.Canvas do
 begin
  Font.Assign(self.Font);
  wBorder    := TextWidth('1')*2 + fBorderWidth div 2;
  case CodeType of
   EAN13,EAN8,UPC_A,UPC_EODD,UPC_EVEN:
     xadd    := wBorder
  else
     xadd    := fBorderWidth;
  end;
  orgin.x := xadd;//Left;
  orgin.y := fBarTop;//Top 0;
  bmpMem.Width   := xadd;
  bmpMem.Height  := fBarHeight+fBarTop;
  brush.Style := bsClear;
  Brush.Color := Color;
  FillRect(ClipRect);
  Pen.Width   := 1;
  for i:=1 to Length(data) do
   begin
    OneBarProps(Data[i],fWidth,lt);
    Pen.Color   := fBarColor;//clWhite;
    brush.Style := bsClear;
    Brush.Color := Color;
    if (lt = ltBlack) or (lt = ltBlack_half) then
        Brush.Color := fBarColor;//clBlack
    if lt = ltBlack_half then
       fheight := bmpMem.Height * 2 div 5
    else
       fheight := bmpMem.Height;
    GetABCED(a,b,c,d,orgin,xadd,fWidth,fHeight);
    Polygon([a,b,c,d]);
    xadd        := xadd + fwidth;
    bmpMem.Width   := xadd;
   end;//结束画直线
  Brush.Color := Color;
  Rect        := ClipRect;
  Rect.Bottom := fBarTop;
  FillRect(Rect);
  Rect        := ClipRect;
  Rect.Right  := fBorderWidth;
  FillRect(Rect);
  if fShowText then
   begin
     if (CodeType = EAN13)or(CodeType = EAN8)or
        (CodeType = UPC_A)or(CodeType = UPC_EODD)or
        (CodeType = UPC_EVEN) then
      begin
        bmpMem.Height     := bmpMem.Height + TextHeight('A') div 2;
        bmpMem.Width      := xadd + wBorder;
        case CodeType of
          EAN13  : DrawEAN13Text(bmpMem.Canvas,bmpMem.Width,wBorder);
          EAN8   : DrawEAN8Text(bmpMem.Canvas,bmpMem.Width,wBorder);
          UPC_A  : DrawUPC_AText(bmpMem.Canvas,bmpMem.Width,wBorder);
        else  //UPC_EODD,UPC_EVEN;
          DrawUPC_EText(bmpMem.Canvas,bmpMem.Width,wBorder);
        end;
      end
     else
      begin
       bmpMem.Height     := bmpMem.Height + TextHeight('A');
       bmpMem.Width      := xadd + fBorderWidth;
       if bmpMem.Width > TextWidth(BarText) then
          tCenter:=(bmpMem.width-TextWidth(BarText))div 2
       else
          tCenter:=0;
       case CodeType of
         Code93Ext,
         Code39Ext:Str := Copy(BarText,3,Length(BarText)-2);
       else
         Str := BarText;
       end;
       TextOut(tCenter, fBarHeight+fBarTop, Str);
      end;
   end
  else
   begin
     bmpMem.Width   := xadd + fBorderWidth;
     Rect        := ClipRect;
     Rect.Top    := Rect.Bottom - fBarTop;
     FillRect(Rect);
   end;  
   case fRotateType of
    raNone:fBitmap.Assign(bmpMem);
    ra270:begin
            fBitmap.width  := bmpMem.Height;
            fBitmap.Height := bmpMem.Width;
            for x:=0 to bmpMem.Height-1 do
               for y:=0 to bmpMem.Width-1 do
                   fBitmap.canvas.Pixels[(-x+bmpMem.Height),y]:=Pixels[y,x];
          end;
    ra180:begin
            fBitmap.width  := bmpMem.Width;
            fBitmap.Height := bmpMem.Height;
            for x:=0 to bmpMem.Height-1 do
               for y:=0 to bmpMem.Width-1 do
                   fBitmap.canvas.Pixels[(bmpMem.Width-y),(bmpMem.Height-x)]:=Pixels[y,x];
          end;
    ra090:begin
            fBitmap.width  := bmpMem.Height;
            fBitmap.Height := bmpMem.Width;
            for x:=0 to bmpMem.Height-1 do
               for y:=0 to bmpMem.Width-1 do
                   fBitmap.canvas.Pixels[x,(bmpMem.Width-y)]:=Pixels[y,x];

          end;
   end;
 end;
 finally
 bmpMem.free;
 end;
end;

{Print the Barcode data :0-3 white Line;5-8 black Line;A-D black Line (2/5 in Height)}
procedure TDefineBarcode.Paint;
begin
 DrawBarcode;
 inherited Paint;
 if AutoSize then
 begin
    Width  := fBitmap.Width;
    Height := fBitmap.Height;
 end;
 fBitmap.Transparent := fTransparent;
 if FTransparent then
 begin
    DrawparentImage(self, Canvas);
 end;
 Canvas.StretchDraw(ClientRect,fBitmap);
end;   

procedure TDefineBarcode.SetRotateType(const Value: TDefineBarcodeRotation);
begin
 if FRotateType <> value then
  begin
    FRotateType := Value;
    Invalidate;
  end;
end;

function TDefineBarcode.GetTypName: String;
begin
 result := BCData[CodeType].Name;
end;

function TDefineBarcode.GetProLine: Integer;
var Inx,w:Integer;
    TempStr:String;
    lt : TDefineBarcodeLines;
begin
 Result  := 0;
 TempStr := '505';
 for Inx := 1 to Length(TempStr) do
   begin
     OneBarProps(TempStr[Inx],w,lt);
     Inc(Result,W);
   end;
end;

procedure TDefineBarcode.SetText(const Value: string);
begin
 if fText <> Value then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetBarHeight(const Value: Integer);
begin
 if fBarHeight <> Value then
  begin
    fBarHeight := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetBorderWidth(const Value: Byte);
begin
 if fBorderWidth <> Value then
 begin
  fBorderWidth := Value;
  Invalidate;
 end;
end;

procedure TDefineBarcode.SetBarColor(const Value: TColor);
begin
 if fBarColor <> Value then
  begin
   fBarColor := Value;
   Invalidate;
  end;
end;

procedure TDefineBarcode.SetRatio(const Value: double);
begin
 if FRatio <> Value then
  begin
    FRatio := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetCodeType(const Value: TDefineBarcodeType);
begin
 if FCodeType <> Value then
  begin
    FCodeType := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetModul(const Value:Integer);
begin
 if (Value >= 1) and (Value  < 50) then
  begin
    fModul  := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetBarTop(const Value: Byte);
begin
 if fBarTop <> Value then
  begin
    fBarTop := Value;
    Invalidate;
  end;
end;

procedure TDefineBarcode.SetColor(const Value: TColor);
begin
 if FColor <> Value then
 begin
    FColor := Value;
    Invalidate;
 end;
end;

procedure TDefineBarcode.FontChange(sender: TObject);
begin
  Invalidate;
end;

procedure TDefineBarcode.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TDefineBarcode.SetBools(Index: Integer; Value: Boolean);
begin
  case index of
   0: fAutoSize       := Value;
   1: FCheckSum       := Value;
   2: fCheckOdd       := Value;
   3: FShowText       := Value;
   4: fTransparent    := Value;
  end;
  invalidate;
end;

initialization
  GetCheckSize;

end.
