unit FlatCtrdb;
//download by http://www.codefans.net
interface

{$I FlatStyle.inc}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics, SysUtils,
  StdCtrls, ExtCtrls,  DB, DBCtrls, Grids, DBGrids, FlatUtils,
  FlatCtrls;

type
  { TDefineDBButton }
  TDefineDBButton = class;
  { TDefineDBBDataLink }
  TDefineDBBDataLink = class(TDataLink)
  private
    FDBBitBtn: TDefineDBButton;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDefineDBButton);
    destructor Destroy; override;
  end;
  { TDefineDBButton }
  TDefineDBButton = class(TDefineButton)
  private
    FDBButton: TFlatDBBName;
    FBeforeAction: EFlatBroClick;
    FShowDialog: Boolean;
    FOnNavClick: EFlatBroClick;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDBButton(const Value: TFlatDBBName);
  protected
    FDataLink : TDefineDBBDataLink;
    procedure ActiveChanged;
    procedure DataChanged;
    procedure EditingChanged;
    procedure BtnClick(Index: TFlatDBBName);
    procedure Loaded; override;
    procedure LoadResourceData(Value: TFlatDBBName);
    procedure ClickHandler(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetName(const Value: TComponentName); override;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property DBButton : TFlatDBBName read FDBButton write SetDBButton default vbNew;
    property BeforeAction: EFlatBroClick read FBeforeAction write FBeforeAction;
    property ShowDialog : Boolean read FShowDialog write FShowDialog default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  { TDefineDBNavigator }
  TDefineDBNavigator = class;
  TDefineDBNButton = class(TDefineButton)
  private
    FIndex: TFlatDBBName;
    FBroStyle: TFlatDBBStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property BroStyle: TFlatDBBStyle read FBroStyle write FBroStyle;
    property Index : TFlatDBBName read FIndex write FIndex;
  end;
  { TDefineDBNDataLink }
  TDefineDBNDataLink = class(TDataLink)
  private
    FBrowser: TDefineDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDefineDBNavigator);
    destructor Destroy; override;
  end;
  { TDefineDBNavigator }
  TDefineDBNavigator = class(TDefinePanel)
  private
    FDataLink: TDefineDBNDataLink;
    FVisibleBtns: TFlatDBBTSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: EFlatBroClick;
    FBeforeAction: EFlatBroClick;
    FocusedButton: TFlatDBBName;
    FConfirmDelete: Boolean;
    procedure SetDataSource(Value: TDataSource);
    procedure SetVisible(Value: TFlatDBBTSet);
    procedure SetHints(Value: TStrings);
    function  GetDataSource: TDataSource;
    function  GetHints: TStrings;
  protected
    Buttons: array[TFlatDBBName] of TDefineDBNButton;
    procedure InitButtons;
    procedure InitHints;
    procedure SetSize(var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    procedure HintsChanged(Sender: TObject);
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure SetColors(Index: Integer; Value: TColor); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure CalcMinSize(var W, H: Integer);
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VISButtons: TFlatDBBTSet read FVisibleBtns write SetVisible
    default [vbFirst,vbPrior,vbNext,vbLast,vbNew,vbDelete,vbEdit,vbSave,vbCancel,vbRefresh];
    property DeleteDialog: Boolean read FConfirmDelete write FConfirmDelete default True;
    property Hints: TStrings read GetHints write SetHints;
    property BeforeAction: EFlatBroClick read FBeforeAction write FBeforeAction;
    property OnClick: EFlatBroClick read FOnNavClick write FOnNavClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TFlatDBBName); virtual;
  end;
  { TDefineDBComboBox }
  TDefineDBComboBox = class(TDefineComboBox)
  private
    FDataLink: TFieldDataLink;
    FPaintControl: TPaintControl;  
    function GetComboText: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetComboText(const Value: string);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetReadOnly(Value: Boolean);
  protected
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure ResetMaxLength;
    procedure ActiveChange(Sender: TObject);
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetItems(const Value: TStrings); override;
    procedure SetStyle(Value: TComboboxStyle); override;
    procedure WndProc(var Message: TMessage); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Items write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Text;
  end;

  { TDefineDBListBox }
  TDefineDBListBox = class(TDefineListBox)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetItems(Value: TStringList);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Items write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  end;

  { TDefineDBEdit }
  TDefineDBEdit = class(TDefineEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetTextMargins: TPoint;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
  protected 
    procedure ResetMaxLength;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  end;

  { TDefineDBFloat }
  TDefineDBFloat = class(TDefineFloat)
  private
    FDataLink : TFieldDataLink;
    function  GetField      : TField;
    function  GetDataField  : string;
    procedure SetDataField(const Value: string);
    function  GetDataSource : TDataSource ;
    procedure SetDataSource(Value : TDataSource);
    procedure EditingChange(Sender: TObject);
    procedure DataChange(sender : TObject);
    procedure UpdateData(sender : TObject);
    procedure ActiveChange(sender : TObject);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DownClick (Sender: TObject); override;
    procedure UpClick (Sender: TObject); override;
    procedure CMExit(var Message:TCMExit);message CM_Exit;
    procedure Change; override;  
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public                      
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

 { TDefineDBInteger }
 TDefineDBInteger = class (TDefineInteger)
  private
    FDataLink : TFieldDataLink;
    function GetDataField : String;
    function GetDataSource : TDataSource;
    function GetReadOnly : Boolean;
    procedure SetReadOnly (aValue : Boolean);
    procedure SetDataSource (aValue : TDataSource);
    procedure SetDataField (const aValue : String);
    procedure DataChange (Sender : TObject);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetField: TField;
  protected
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DownClick (Sender: TObject); override;
    procedure UpClick (Sender: TObject); override;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  { TDefineDBMask }
  TDefineDBMask = Class(TDefineMask)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure ResetMaxLength;
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    function GetTextMargins: TPoint;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  end;

  { TDefineDBMemo }
  TDefineDBMemo = class(TDefineMemo)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FPaintControl: TPaintControl;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);  
  protected
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadMemo; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  end;

  { TDefineDBCheckBox }

  TDefineDBCheckBox = class(TDefineCheckBox)
  private
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function ValueMatch(const ValueList, Value: string): Boolean;
    function GetFieldState: TCheckBoxState;
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
  protected
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
//    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure Toggle; override;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
    property DataLink: TFieldDataLink read FDataLink;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ValueChecked: string read FValueCheck write SetValueCheck;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck;  
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked;
    property Field: TField read GetField;
    property State;     
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
  end;

  { TFlatDBCheckBox }

  TFlatDBCheckBox = class(TDefineDBCheckBox)
  published
    property DataField;
    property DataSource;
    property ReadOnly;
    property ValueChecked;
    property ValueUnchecked;
    property Transparent;
    property Action;
    property Caption;
    property ColorFocused;
    property ColorDown;
    property ColorChecked;
    property Color;
    property ColorBorder;
    property Enabled;
    property Font;
    property Layout;
    property ParentColor;
    property ParentFont;
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
  end;

  TFlatDBGrid = class(TVersionDBGrid)
  private
    FSingleColor: TColor;
    FDoubleColor: TColor;
    FDbBgColor: boolean;
    OldGridWnd : TWndMethod;
    FParentColor: Boolean;
    FFocusColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FMouseIn: Boolean;
    FLinesColor: TColor;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetParentColor(Value: Boolean);
    procedure NewGridWnd (var Message : TMessage);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetDbBgColor(const Value: boolean);
    function GetMouseIn: boolean;
  protected
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); override;
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
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    property MouseIn:boolean read GetMouseIn;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property SelectedRows;
  published
    property DbBgColor :Boolean read FDbBgColor Write SetDbBgColor default true;
    property ColorFocused: TColor index 0 read FFocusColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors default DefaultFlatColor;
    property ColorLines: TColor index 3 read FLinesColor write SetColors default DefaultBorderColor;
    property ColorRowSingle :TColor index 4 read FSingleColor Write SetColors default clWhite;
    property ColorRowDouble :TColor index 5 read FDoubleColor Write SetColors default clWhite;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property Align;
    property Anchors;
    property BiDiMode;
    property Columns stored False; //StoreColumns;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property ImeMode;
    property ImeName;
    property Options;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;  { obsolete }
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

  { TDefineDBRadioGroup }

  TDefineDBRadioGroup = class(TDefineRadioGroup)
  private
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStrings;
    FInSetValue: Boolean;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetButtonValue(Index: Integer): string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: string);
    procedure SetItems(Value: TStrings);
    procedure SetValues(Value: TStrings);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; dynamic;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
    function CanModify: Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateData(Sender: TObject);
    property DataLink: TFieldDataLink read FDataLink;
    //----------------------------
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Items write SetItems;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property ItemIndex;
    property Value: string read FValue write SetValue;
  end;

  TFlatDBButton = class(TDefineDBButton)
  published
    property DataSource;
    property DBButton;
    property BeforeAction;
    property ShowDialog;
    property Transparent;
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
    property Font;
    property Layout;
    property Margin;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Spacing;
    property ModalResult;
    property Visible;
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
  TFlatDBComboBox = class(TDefineDBComboBox)
  published
    property Style;
    property Ticket;
    property TicketPosition;
    property TicketSpace;
    property DataField;
    property DataSource;
    property ReadOnly;
    property Items;
    property Version;
    property ColorArrow;
    property ColorArrowBackground;
    property ColorBorder;
    property ColorFlat;
    property ColorFocued;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property CharCase;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
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
  TFlatDBListBox = class(TDefineDBListBox)
  published
    property ReadOnly;
    property DataField;
    property DataSource;
    property Items;
    property Version;
    property Caption;
    property Skin;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  TFlatDBEdit = class(TDefineDBEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField;
    property DataSource;
    property ReadOnly;
    property Ticket;
    property TicketPosition;
    property TicketSpace;

    property Version;
    property Alignment;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property MaxLength;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  TFlatDBFloat = class(TDefineDBFloat)
  published
    property DataField;
    property DataSource;
    property ReadOnly;
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
  TFlatDBInteger = class (TDefineDBInteger)
  published
    property DataSource;
    property DataField;
    property ReadOnly;
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
  TFlatDBMaskEdit = Class(TDefineDBMask)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property Ticket;
    property TicketPosition;
    property TicketSpace;

    property Version;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Alignment;
    property Align;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
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
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
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
    {$IFDEF DFS_DELPHI_4_UP}
    property ImeMode;
    property ImeName;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF}
  end;
  TFlatDBMemo = class(TDefineDBMemo)
  published
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property ParentColor;
    property Version;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataField;
    property DataSource;
    property ReadOnly;
    property AutoDisplay;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  TFlatDBNavigator = class(TDefineDBNavigator)
  published
    property DataSource;
    property VISButtons;
    property DeleteDialog;
    property Hints;
    property BeforeAction;
    property OnClick;
    property ColorBorder;
    property Constraints;
    property Transparent;
    property Align;
    property Anchors;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Locked;
    property FullRepaint;
    property Color;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TFlatDBRadioGroup = class(TDefineDBRadioGroup)
  published
    property Transparent;
    property Alignment;
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
    property TopOffset;
    property Hint;
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
    property DataField;
    property DataSource;
    property Items;
    property ReadOnly;
    property Values;
    property OnChange;
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
  
implementation

uses Clipbrd, FlatCnsts;  

{ TDefineDBBDataLink }

constructor TDefineDBBDataLink.Create(ANav: TDefineDBButton);
begin
  inherited Create;
  FDBBitBtn     := ANav;
  VisualControl := True;
end;

destructor TDefineDBBDataLink.Destroy;
begin
  FDBBitBtn := nil;
  inherited Destroy;
end;

procedure TDefineDBBDataLink.EditingChanged;
begin
  if FDBBitBtn <> nil then FDBBitBtn.EditingChanged;
end;

procedure TDefineDBBDataLink.DataSetChanged;
begin
  if FDBBitBtn <> nil then FDBBitBtn.DataChanged;
end;

procedure TDefineDBBDataLink.ActiveChanged;
begin
  if FDBBitBtn <> nil then FDBBitBtn.ActiveChanged;
end;

{ TDefineDBButton }
var
  myResIDName   : array[TFlatDBBName] of PChar = ('FIRST', 'PRIOR', 'NEXT',
                       'LAST', 'NEW', 'DELETE', 'EDIT', 'SAVE', 'CANCEL', 'REFRESH');
  myBtnHintId   : array[TFlatDBBName] of Pointer = (@myFirstHint, @myPriorHint,
                       @myNextHint, @myLastHint, @myNewHint, @myDeleteHint,
                       @myEditHint, @myPostHint, @myCancelHint, @myRefreshHint);
  myBtnCapId    : array[TFlatDBBName] of Pointer = (@myFirstCap, @myPriorCap,
                       @myNextCap, @myLastCap, @myNewCap, @myDeleteCap, @myEditCap,
                       @myPostCap, @myCancelCap, @myRefreshCap);

constructor TDefineDBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink   := TDefineDBBDataLink.Create(self);
  FDBButton   := vbNew;
  FShowDialog := True;
  Layout      := blGlyphLeft;
  OnClick     := ClickHandler;
  Enabled     := False;
  ShowHint    := True;
  LoadResourceData(DBButton);
end;

destructor TDefineDBButton.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TDefineDBButton.GetDataSource: TDataSource;
begin
 result := FDataLink.DataSource;
end;

procedure TDefineDBButton.SetDataSource(const Value: TDataSource);
begin
 FDataLink.DataSource := Value;
 if not (csLoading in ComponentState) then
    ActiveChanged;
 if Value <> nil then
   begin
    LoadResourceData(DBButton);
    Value.FreeNotification(Self);
   end;
end;

procedure TDefineDBButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
     (AComponent = DataSource) then DataSource := nil;
end;

procedure TDefineDBButton.LoadResourceData(Value:TFlatDBBName);
var ResName : String;
begin
 FmtStr(ResName, 'FLAT_%s', [myResIDName[Value]]);
 Glyph.LoadFromResourceName(HInstance, ResName);
 NumGlyphs := 2;
 Caption := LoadResString(myBtnCapId[Value]);
 Hint    := LoadResString(myBtnHintId[Value]);
end;

procedure TDefineDBButton.SetDBButton(const Value: TFlatDBBName);
begin
 If FDBButton <> Value then
  begin
    FDBButton := Value;
    LoadResourceData(Value);
    ActiveChanged;
  end;
end;

procedure TDefineDBButton.BtnClick(Index: TFlatDBBName);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        vbFirst   : First;
        vbPrior   : Prior;
        vbNext    : Next;
        vbLast    : Last;
        vbEdit    : Edit;
        vbCancel  : Cancel;
        vbRefresh : Refresh;
        vbNew     : if not FShowDialog or(ShowBox(myNewRecordQuestion, mbIYn)<>idNo) then Insert;
        vbSave    : if not FShowDialog or(ShowBox(mySaveRecordQuestion, mbIYn)<>idNo) then Post;
        vbDelete  : if not FShowDialog or(ShowBox(myDeleteRecordQuestion, mbIYn)<>idNo) then Delete;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
     FOnNavClick(Self, Index);
end;

procedure TDefineDBButton.ClickHandler(Sender: TObject);
begin
  BtnClick(FDBButton);
end;

procedure TDefineDBButton.DataChanged;
var
  UpEnable, DnEnable, EnDelete: Boolean;
begin
  UpEnable := FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := FDataLink.Active and not FDataLink.DataSet.EOF;
  EnDelete := FDataLink.Active and FDataLink.DataSet.CanModify and
              not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  case FDBButton of
    vbFirst,vbPrior : Enabled := UpEnable;
    vbNext,vbLast   : Enabled := DnEnable;
    vbDelete        : Enabled := EnDelete;
  end;
end;

procedure TDefineDBButton.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := FDataLink.Active and FDataLink.DataSet.CanModify;
  case FDBButton of
   vbNew,vbRefresh  : Enabled := CanModify;
   vbEdit           : Enabled := CanModify and not FDataLink.Editing;
   vbSave           : Enabled := CanModify and FDataLink.Editing;
   vbCancel         : Enabled := CanModify and FDataLink.Editing;
  end;
end;

procedure TDefineDBButton.ActiveChanged;
begin
  if not FDataLink.Active then
     Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDefineDBButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
     ActiveChanged;
end;

procedure TDefineDBButton.Loaded;
begin
  inherited Loaded;
  ActiveChanged;
end;

procedure TDefineDBButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDefineDBButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TDefineDBButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TDefineDBButton.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if (csDesigning in ComponentState)and((GetTextLen = 0)or
     (CompareText(Caption, Name) = 0)) then
      LoadResourceData(FDBButton);
end;

{ TDefineDBNavigator }

constructor TDefineDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FDataLink := TDefineDBNDataLink.Create(Self);
  FHints    := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  FVisibleBtns := [vbFirst,vbPrior,vbNext,vbLast,vbNew,vbDelete,vbEdit,vbSave,vbCancel,vbRefresh];
  InitButtons;
  InitHints;
  ShowHint       := True;
  Cursor         := crHandPoint;
  Width          := 241;
  Height         := 25;
  ButtonWidth    := 0;
  FocusedButton  := vbFirst;
  FConfirmDelete := True;
  FullRepaint    := False;
end;

destructor TDefineDBNavigator.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBNavigator.InitButtons;
var
  I: TFlatDBBName;
  Btn: TDefineDBNButton;
  X: Integer;
  ResName: string;
begin
  MinBtnSize := Point(20, 18);
  X := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TDefineDBNButton.Create (Self);
    Btn.Index     := I;
    Btn.Visible   := I in FVisibleBtns;
    Btn.Enabled   := True;
    Btn.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);
    FmtStr(ResName, 'FLAT_%s', [myResIDName[I]]);
    Btn.Glyph.LoadFromResourceName(HInstance, ResName);
    Btn.NumGlyphs := 2;
    Btn.Enabled   := False;
    Btn.Parent    := Self;
    Btn.Transparent := tmAlways;
    Btn.OnClick     := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.ColorBorder := ColorBorder;
    Buttons[I]    := Btn;
    X := X + MinBtnSize.X;
  end;
  Buttons[vbPrior].BroStyle := Buttons[vbPrior].BroStyle + [myAllowTimer];
  Buttons[vbNext].BroStyle  := Buttons[vbNext].BroStyle + [myAllowTimer];
end;

procedure TDefineDBNavigator.InitHints;
var
  I: Integer;
  J: TFlatDBBName;
begin
  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;
    for J := Low(Buttons) to High(Buttons) do
        FDefHints.Add(LoadResString(myBtnHintId[J]));
  end;
  for J := Low(Buttons) to High(Buttons) do
    Buttons[J].Hint := FDefHints[Ord(J)];
  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then
       Buttons[J].Hint := FHints.Strings[I];
    if J = High(Buttons) then
       Exit;
    Inc(J);
  end;
end;

procedure TDefineDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TDefineDBNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
     FHints.Clear
  else
     FHints.Assign(Value);
end;

function TDefineDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

procedure TDefineDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TDefineDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDefineDBNavigator.SetVisible(Value: TFlatDBBTSet);
var
  I: TFlatDBBName;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleBtns := Value;
  for I := Low(Buttons) to High(Buttons) do Buttons[I].Visible := I in FVisibleBtns;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then  inherited SetBounds(Left, Top, W, H);
  Invalidate;
end;

procedure TDefineDBNavigator.CalcMinSize(var W, H: Integer);
var
  Count: Integer;
  I: TFlatDBBName;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[vbFirst] = nil then Exit;

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do if Buttons[I].Visible then Inc(Count);
  if Count = 0 then Inc(Count);
  W := Max(W, Count * MinBtnSize.X);
  H := Max(H, MinBtnSize.Y);
  if Align = alNone then W := (W div Count) * Count;
end;

procedure TDefineDBNavigator.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  I: TFlatDBBName;
  Space, Temp, Remain: Integer;
  X: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[vbFirst] = nil then Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do if Buttons[I].Visible then Inc(Count);
  if Count = 0 then Inc(Count);

  ButtonWidth := W div Count;
  Temp := Count * ButtonWidth;
  if Align = alNone then W := Temp;
  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      Buttons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
      Inc(X, ButtonWidth + Space);
    end
    else
      Buttons[I].SetBounds(Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TDefineDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TDefineDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TDefineDBNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
     CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TDefineDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TDefineDBNButton(Sender).Index);
end;

procedure TDefineDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TFlatDBBName;
begin
  OldFocus := FocusedButton;
  FocusedButton := TDefineDBNButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
end;

procedure TDefineDBNavigator.BtnClick(Index: TFlatDBBName);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        vbPrior : Prior;
        vbNext  : Next;
        vbFirst : First;
        vbLast  : Last;
        vbNew   : Insert;
        vbEdit  : Edit;
        vbCancel: Cancel;
        vbSave  : Post;
        vbRefresh: Refresh;
        vbDelete:
        if(not FConfirmDelete)or(ShowBox(myDeleteRecordQuestion, mbIYN)<>idCancel) then Delete;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then FOnNavClick(Self, Index);
end;

procedure TDefineDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDefineDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDefineDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TFlatDBBName;
  OldFocus: TFlatDBBName;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus < High(Buttons) then
             NewFocus := Succ(NewFocus);
        until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
             NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
           Buttons[FocusedButton].Click;
      end;
  end;
end;

procedure TDefineDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDefineDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[vbFirst].Enabled  := UpEnable;
  Buttons[vbPrior].Enabled  := UpEnable;
  Buttons[vbNext].Enabled   := DnEnable;
  Buttons[vbLast].Enabled   := DnEnable;
  Buttons[vbDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TDefineDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[vbNew].Enabled := CanModify;
  Buttons[vbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[vbSave].Enabled := CanModify and FDataLink.Editing;
  Buttons[vbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[vbRefresh].Enabled := CanModify;
end;

procedure TDefineDBNavigator.ActiveChanged;
var
  I: TFlatDBBName;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(Buttons) to High(Buttons) do
        Buttons[I].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDefineDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
     ActiveChanged;
end;

procedure TDefineDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
     ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;

procedure TDefineDBNavigator.SetColors(Index: Integer; Value: TColor);
var I: TFlatDBBName;
begin
  inherited;
  for I := low(Buttons) to high(Buttons) do
  begin
      Buttons[i].ColorBorder := Value;
  end;
end;

{ TDefineDBNDataLink }

constructor TDefineDBNDataLink.Create(ANav: TDefineDBNavigator);
begin
  inherited Create;
  FBrowser      := ANav;
  VisualControl := True;
end;

destructor TDefineDBNDataLink.Destroy;
begin
  FBrowser := nil;
  inherited Destroy;
end;

procedure TDefineDBNDataLink.EditingChanged;
begin
  if FBrowser <> nil then FBrowser.EditingChanged;
end;

procedure TDefineDBNDataLink.DataSetChanged;
begin
  if FBrowser <> nil then FBrowser.DataChanged;
end;

procedure TDefineDBNDataLink.ActiveChanged;
begin
  if FBrowser <> nil then FBrowser.ActiveChanged;
end;

{TDefineDBNButton}

destructor TDefineDBNButton.Destroy;
begin
  if FRepeatTimer <> nil then
     FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TDefineDBNButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if myAllowTimer in FBroStyle then
  begin
    if FRepeatTimer = nil then
       FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := FlatInitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TDefineDBNButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
     FRepeatTimer.Enabled  := False;
end;

procedure TDefineDBNButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval :=FlatRepeatPause;
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

{ TDefineDBComboBox }

constructor TDefineDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange  := ActiveChange;
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');
end;

destructor TDefineDBComboBox.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBComboBox.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if(csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDefineDBComboBox.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TDefineDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(Operation = opRemove) and(FDataLink <> nil) and
   (AComponent = DataSource) then DataSource := nil;
end;

procedure TDefineDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TDefineDBComboBox.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TDefineDBComboBox.DataChange(Sender: TObject);
begin
  if not(Style = csSimple) and DroppedDown then Exit;
  if not (csDesigning in ComponentState) then
  begin
     if FDataLink.Field <> nil then begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
     end;
  end;
  if FDataLink.Field <> nil then
    SetComboText(FDataLink.Field.Text)
  else
    if csDesigning in ComponentState then
    begin
      if DataField <> '' then
         SetComBoText(DataField)
      else
         SetComboText(Name);
    end else
      SetComboText('');
end;

procedure TDefineDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TDefineDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw :=(Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

function TDefineDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

procedure TDefineDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TDefineDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

procedure TDefineDBComboBox.DropDown;
begin
  inherited DropDown;
end;

function TDefineDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not(FDataLink.DataSourceFixed and(csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBComboBox.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
     ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TDefineDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDefineDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and(Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TDefineDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if(Key in [#32..#255]) and(FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TDefineDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TDefineDBComboBox.SetEditReadOnly;
begin
  if(Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TDefineDBComboBox.WndProc(var Message: TMessage);
begin
  if not(csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then FDataLink.Edit else
          if not FDataLink.Editing then DataChange(Self); {Restore text}
      WM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED:
        FPaintControl.DestroyHandle;
    end;
  inherited WndProc(Message);
end;

procedure TDefineDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not(csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if(Style = csSimple) and(ComboWnd <> EditHandle) then
          if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TDefineDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(False), 0);
end;

procedure TDefineDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDefineDBComboBox.WMPaint(var Message: TWMPaint);
var
  S: string;
  R: TRect;
  P: TPoint;
  Child: HWND;
begin
  if csPaintCopy in ControlState then
  begin
    if FDataLink.Field <> nil then S := FDataLink.Field.Text else S := '';
    if Style = csDropDown then
    begin
      SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Longint(PChar(S)));
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      Child := GetWindow(FPaintControl.Handle, GW_CHILD);
      if Child <> 0 then
      begin
        Windows.GetClientRect(Child, R);
        Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
        GetWindowOrgEx(Message.DC, P);
        SetWindowOrgEx(Message.DC, P.X - R.Left, P.Y - R.Top, nil);
        IntersectClipRect(Message.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
        SendMessage(Child, WM_PAINT, Message.DC, 0);
      end;
    end else
    begin
      SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
      if Items.IndexOf(S) <> -1 then
      begin
        SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
        SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
      end;
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
    end;
  end else
    inherited;
end;

procedure TDefineDBComboBox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  DataChange(Self);
end;

procedure TDefineDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if(Value = csSimple) and Assigned(FDatalink) and FDatalink.DatasourceFixed then
    DatabaseError(SNotReplicatable);
  inherited SetStyle(Value);
end;

function TDefineDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDefineDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TDefineDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or(FDataLink <> nil) and
  FDataLink.ExecuteAction(Action);
end;

function TDefineDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or(FDataLink <> nil) and
  FDataLink.UpdateAction(Action);
end;

{ TDefineDBListBox }

constructor TDefineDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDefineDBListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if(Operation = opRemove) and(FDataLink <> nil) and
   (AComponent = DataSource) then DataSource := nil;
end;

function TDefineDBListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDefineDBListBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
     ItemIndex := Items.IndexOf(FDataLink.Field.Text)
  else
     ItemIndex := -1;
end;

procedure TDefineDBListBox.UpdateData(Sender: TObject);
begin
  if ItemIndex >= 0 then
     FDataLink.Field.Text := Items[ItemIndex]
  else
     FDataLink.Field.Text := '';
end;

procedure TDefineDBListBox.Click;
begin
  if FDataLink.Edit then
  begin
    inherited Click;
    FDataLink.Modified;
  end;
end;

function TDefineDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBListBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDefineDBListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDefineDBListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN] then
    if not FDataLink.Edit then Key := 0;
end;

procedure TDefineDBListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TDefineDBListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if FDataLink.Edit then inherited
  else
  begin
    Self.SetFocus;
    with Message do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TDefineDBListBox.CMExit(var Message: TCMExit);
begin
  try
    if IndexInCount(ItemIndex, Items.Count) then
       FDataLink.UpdateRecord;
  except
    Self.SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDefineDBListBox.SetItems(Value: TStringList);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

function TDefineDBListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or(FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or(FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

{ TDefineDBEdit }

procedure TDefineDBEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
       MaxLength := 0;
  end;
end;

constructor TDefineDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange    := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData    := UpdateData;
  FDataLink.OnActiveChange  := ActiveChange;
end;

destructor TDefineDBEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDefineDBEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDefineDBEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDefineDBEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDefineDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDefineDBEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDefineDBEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TDefineDBEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TDefineDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TDefineDBEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDefineDBEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TDefineDBEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      Text := '';  {forces update}
      FAlignment := FDataLink.Field.Alignment;
    end;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      Text := FDataLink.Field.DisplayText;
      if FDataLink.Editing  then
        Modified := True;
    end;
  end else
  begin
    FAlignment := taLeftJustify;
    if csDesigning in ComponentState then
    begin
      if DataField <> '' then
         Text := DataField
      else
         Text := Name;
    end else
      Text := '';
  end;
end;

procedure TDefineDBEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TDefineDBEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TDefineDBEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDefineDBEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

procedure TDefineDBEdit.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  AAlignment := FAlignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TDefineDBEdit.CMGetDataLink(var Message: TMessage);
begin
 // Message.Result := Integer(FDataLink);
  Message.Result := SizeOf(FDataLink);
end;

function TDefineDBEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function TDefineDBEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

{ TDefineDBFloat }

procedure TDefineDBFloat.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

function TDefineDBFloat.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBFloat.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  Enabled := FDataLink.Active and (FDataLink.Field <> nil) and
          not FDataLink.Field.ReadOnly ;
end;

function TDefineDBFloat.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDefineDBFloat.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBFloat.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

constructor TDefineDBFloat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TDefineDBFloat.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBFloat.DataChange(sender : TObject);
begin
  if (FDataLink.Field <> nil) and
       ((FDataLink.Field is TFloatField) or (FDataLink.Field is TCurrencyField))  then
    value := FDataLink.Field.AsFloat
  else
    value := 0.00;
end;

procedure TDefineDBFloat.UpdateData(sender : TObject);
begin
  if (FDataLink.Field <> nil)  and
       ((FDataLink.Field is TFloatField) or (FDataLink.Field is TCurrencyField))  then
    FDataLink.Field.AsFloat :=  value  ;
end;

procedure TDefineDBFloat.ActiveChange(sender : TObject);
begin
  Enabled := FDataLink.Active and (FDataLink.Field <> nil);
end;

procedure TDefineDBFloat.CMExit(var Message:TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
   inherited;
end;

procedure TDefineDBFloat.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDefineDBFloat.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    {^H, ^V, ^X,} #48..#57:
      FDataLink.Edit;
    #27:                              //esc取消
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDefineDBFloat.Change;
begin
  if FDataLink <> nil then
    FDataLink.Modified;
  inherited Change;
end;

procedure TDefineDBFloat.DownClick (Sender: TObject);
begin
  inherited DownClick (Sender);
  FDataLink.Edit;
end;

procedure TDefineDBFloat.UpClick (Sender: TObject);
begin
  inherited UpClick (Sender);
  FDataLink.Edit;
end;     

function TDefineDBFloat.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
  inherited ReadOnly := Result;
end;

procedure TDefineDBFloat.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  inherited ReadOnly := Value;
end;

{ TDefineDBInteger }

function TDefineDBInteger.GetDataField : String;
begin
  Result := FDataLink.FieldName;
end;

function TDefineDBInteger.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDefineDBInteger.GetReadOnly : Boolean;
begin
  Result := FDataLink.ReadOnly;
  inherited ReadOnly := Result;
end;


procedure TDefineDBInteger.SetReadOnly (aValue : Boolean);
begin
  FDataLink.ReadOnly := aValue;
  inherited ReadOnly := aValue;
end;


procedure TDefineDBInteger.SetDataSource (aValue : TDataSource);
begin
  FDataLink.DataSource := aValue;
  if aValue <> nil then aValue.FreeNotification(Self);
end;


procedure TDefineDBInteger.SetDataField (const aValue : String);
begin
  FDataLink.FieldName := aValue;
end;


procedure TDefineDBInteger.DataChange (Sender : TObject);
begin
  if FDataLink.Field <> nil then
    begin
      if not (csDesigning in ComponentState) then
        begin
          if (FDataLink.Field.DataType = ftInteger) and (MaxLength = 0) then
            MaxLength := FDataLink.Field.Size;
        end;
      if {FFocused and} FDataLink.CanModify then
        begin
          Value := FDataLink.Field.AsInteger;
        end
      else
        begin
          Value := FDataLink.Field.AsInteger;
        end;
    end
end;

procedure TDefineDBInteger.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsInteger := Value; { Value, Text }
end;

 
procedure TDefineDBInteger.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

 
procedure TDefineDBInteger.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;
 
procedure TDefineDBInteger.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBInteger.Change;
begin
  if FDataLink <> nil then
    FDataLink.Modified;
  inherited Change;
end;


procedure TDefineDBInteger.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;


procedure TDefineDBInteger.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TDefineDBInteger.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
end;

procedure TDefineDBInteger.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDefineDBInteger.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    {^H, ^V, ^X,} #48..#57:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDefineDBInteger.DownClick (Sender: TObject);
begin
  inherited DownClick (Sender);
  FDataLink.Edit;
end;

procedure TDefineDBInteger.UpClick (Sender: TObject);
begin
  inherited UpClick (Sender);
  FDataLink.Edit;
end;

constructor TDefineDBInteger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle:=ControlStyle-[csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDefineDBInteger.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TDefineDBInteger.GetField: TField;
begin
  result := FDataLink.Field;
end;

{ TDefineDBMask }

procedure TDefineDBMask.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TDefineDBMask.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDefineDBMask.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDefineDBMask.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

procedure TDefineDBMask.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := SizeOf(FDataLink);
end;

constructor TDefineDBMask.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

procedure TDefineDBMask.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      Text := '';  {forces update}
      FAlignment := FDataLink.Field.Alignment;
    end;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      Text := FDataLink.Field.DisplayText;
      if FDataLink.Editing  then
        Modified := True;
    end;
  end else
  begin
    FAlignment := taLeftJustify;
    if csDesigning in ComponentState then
      Text := Name else
      Text := '';
  end;
end;

destructor TDefineDBMask.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDefineDBMask.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

function TDefineDBMask.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBMask.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDefineDBMask.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDefineDBMask.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDefineDBMask.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TDefineDBMask.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TDefineDBMask.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDefineDBMask.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDefineDBMask.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDefineDBMask.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDefineDBMask.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TDefineDBMask.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDefineDBMask.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
     FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TDefineDBMask.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TDefineDBMask.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBMask.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TDefineDBMask.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

function TDefineDBMask.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDefineDBMask.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBMask.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  AAlignment := FAlignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TDefineDBMask.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBMask.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

{TDefineDBMemo}
constructor TDefineDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  AutoSize           := False;
  FAutoDisplay       := True;
  FDataLink          := TFieldDataLink.Create;
  FDataLink.Control  := Self;
  FDataLink.OnDataChange    := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData    := UpdateData;
  FPaintControl             := TPaintControl.Create(Self, 'EDIT');
end;

destructor TDefineDBMemo.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBMemo.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDefineDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDefineDBMemo.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDefineDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      FDataLink.Edit;
  end;
end;

procedure TDefineDBMemo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        FDataLink.Edit;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TDefineDBMemo.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TDefineDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBMemo.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDefineDBMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDefineDBMemo.LoadMemo;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    try
      Lines.Text := FDataLink.Field.AsString;
      FMemoLoaded := True;
    except
     // Memo too large
     on E:EInvalidOperation do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TDefineDBMemo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TDefineDBMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TDefineDBMemo.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := Text;
end;

procedure TDefineDBMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TDefineDBMemo.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg = WM_CREATE) or (Msg = WM_WINDOWPOSCHANGED) or
      (Msg = CM_FONTCHANGED) then FPaintControl.DestroyHandle;
  inherited;
end;

procedure TDefineDBMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDefineDBMemo.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TDefineDBMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TDefineDBMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TDefineDBMemo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBMemo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBMemo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDefineDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TDefineDBMemo.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if not (csPaintCopy in ControlState) then inherited else
  begin
    if FDataLink.Field <> nil then
      if FDataLink.Field.IsBlob then
      begin
        if FAutoDisplay then
          S := AdjustLineBreaks(FDataLink.Field.AsString)
        else
          S := Format('(%s)', [FDataLink.Field.DisplayLabel]);
      end else
        S := FDataLink.Field.DisplayText;
    SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Integer(PChar(S)));
    SendMessage(FPaintControl.Handle, WM_ERASEBKGND, Message.DC, 0);
    SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
  end;
end;

function TDefineDBMemo.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBMemo.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

{ TFlatDBGrid }

constructor TFlatDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSingleColor    := clWhite;
  FDoubleColor    := clWhite;//$00FFF0E1;
  OldGridWnd      := self.WindowProc ;
  self.WindowProc := NewGridWnd;
  fDBBGColor      := True;
  BorderStyle   := bsNone;
  FFocusColor   := clWhite;
  FBorderColor  := DefaultBorderColor;
  FLinesColor   := DefaultBorderColor;
  FFlatColor    := DefaultFlatColor;
  FParentColor  := True;
  FMouseIn      := False;
end;

procedure TFlatDBGrid.NewGridWnd(var Message: TMessage);
var
 IsNeg : Boolean;
begin
 if Message.Msg = WM_MOUSEWHEEL then
 begin
   IsNeg := Short(Message.WParamHi) < 0;
   if IsNeg then
     Self.DataSource.DataSet.MoveBy(1)
   else
     Self.DataSource.DataSet.MoveBy(-1)
 end
 else
   OldGridWnd(Message);
end;

procedure TFlatDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  inherited;
  if GdSelected in State then  exit;
  if DbBgColor then
  begin
   if DataSource.DataSet.RecNo mod 2<>0 then
      Canvas.Brush.Color := FSingleColor   //读取单横颜色值。。。
   else
      Canvas.Brush.Color := FDoubleColor; // 读取双横颜色值。$00F7E7E7。。
  end;
  DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;


procedure TFlatDBGrid.SetDbBgColor(const Value: boolean);
begin
  FDbBgColor := Value;
  repaint;
end;

procedure TFlatDBGrid.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
inherited;
  if Datalink.Active then
    with Message, DataLink.DataSet do
      case ScrollCode of
        SB_THUMBPOSITION:
          begin
            if IsSequenced then
            begin
              SI.cbSize := sizeof(SI);
              SI.fMask := SIF_ALL;
              SetScrollPos(self.Handle,SB_VERT,Pos,True);    //强行设定滚动条的位置
              GetScrollInfo(Self.Handle, SB_VERT, SI);
              if SI.nTrackPos <= 1 then First
              else if SI.nTrackPos >= RecordCount then Last
              else RecNo := SI.nTrackPos;
            end
            else
              case Pos of
                0: First;
                1: MoveBy(-VisibleRowCount);
                2: Exit;
                3: MoveBy(VisibleRowCount);
                4: Last;
              end;
          end;
      end;
end;

procedure TFlatDBGrid.RedrawBorder(const Clip: HRGN);
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
   BoldState   := False;
  end;
  Color := DrawEditBorder(Attrib,Clip);
end;

procedure TFlatDBGrid.SetParentColor(Value: Boolean);
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

procedure TFlatDBGrid.CMSysColorChange(var Message: TMessage);
begin
    if (Parent <> nil)and(FParentColor) then
      FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TFlatDBGrid.CMParentColorChanged(var Message: TWMNoParams);
begin
    if (Parent <> nil)and(FParentColor) then
       FFlatColor := TForm(Parent).Color;
    RedrawBorder;
end;

procedure TFlatDBGrid.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusColor    := Value;
    1: FBorderColor   := Value;
    2: begin
         FFlatColor   := Value;
         FParentColor := False;
       end;
    3: FLinesColor    := Value;
    4: FSingleColor   := Value;
    5: FDoubleColor   := Value;
  end;
  Repaint;
  RedrawBorder;
end;

procedure TFlatDBGrid.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseIn := True;
    RedrawBorder;
  end;
end;

procedure TFlatDBGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseIn := False;
  RedrawBorder;
end;

procedure TFlatDBGrid.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder;
end;

procedure TFlatDBGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TFlatDBGrid.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
     RedrawBorder;
end;

procedure TFlatDBGrid.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TFlatDBGrid.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TFlatDBGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var FRect:TRect;
begin
  inherited;
  //绘制数据区的表格边框
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
      FRect := Rect(Left-1,Top-1,Right+1,Bottom+1);
      DrawButtonBorder(Canvas,FRect,FLinesColor,1);
   end;
  end;
end;

function TFlatDBGrid.GetMouseIn: boolean;
begin
  result := FMouseIn;
end;

{ TDefineDBCheckBox }

constructor TDefineDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle           := ControlStyle + [csReplicatable];
  FValueCheck            := STextTrue;
  FValueUncheck          := STextFalse;
  State                  := cbUnchecked;
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDefineDBCheckBox.Destroy;
begin    
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDefineDBCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDefineDBCheckBox.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := cbGrayed
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := cbChecked
      else
        Result := cbUnchecked
    else
    begin
      Result := cbGrayed;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := cbChecked else
        if ValueMatch(FValueUncheck, Text) then Result := cbUnchecked;
    end
  else
    Result := cbUnchecked;
end;

function TDefineDBCheckBox.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TDefineDBCheckBox.DataChange(Sender: TObject);
begin
  State := GetFieldState;
end;

procedure TDefineDBCheckBox.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if State = cbGrayed then
    FDataLink.Field.Clear
  else
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := Checked
    else
    begin
      if Checked then S := FValueCheck else S := FValueUncheck;
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName(S, Pos);
    end;
end;

function TDefineDBCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBCheckBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName
end;

procedure TDefineDBCheckBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDefineDBCheckBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBCheckBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBCheckBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDefineDBCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
     #8: FDataLink.Edit;
    #27: FDataLink.Reset;
  end;
end;

procedure TDefineDBCheckBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDefineDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TDefineDBCheckBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
    Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBCheckBox.UpdateAction(Action: TBasicAction): Boolean;
begin
    Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

function TDefineDBCheckBox.UseRightToLeftAlignment: Boolean;
begin
   Result := inherited UseRightToLeftAlignment;
end;

procedure TDefineDBCheckBox.Toggle;
begin
  if FDataLink.Edit then
  begin
    inherited Toggle;
    FDataLink.Modified;
  end;
end;

procedure TDefineDBCheckBox.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TDefineDBCheckBox.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

{ TDefineDBRadioGroup }

constructor TDefineDBRadioGroup.Create(AOwner: TComponent);
begin
  FValues   := TStringList.Create;
  FDataLink := TFieldDataLink.Create;
  inherited Create(AOwner);
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDefineDBRadioGroup.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TDefineDBRadioGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDefineDBRadioGroup.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

procedure TDefineDBRadioGroup.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
     Value := FDataLink.Field.Text else
     Value := '';
end;

procedure TDefineDBRadioGroup.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then FDataLink.Field.Text := Value;
end;

function TDefineDBRadioGroup.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineDBRadioGroup.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDefineDBRadioGroup.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDefineDBRadioGroup.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDefineDBRadioGroup.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDefineDBRadioGroup.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDefineDBRadioGroup.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDefineDBRadioGroup.GetButtonValue(Index: Integer): string;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else if Index < Items.Count then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TDefineDBRadioGroup.SetValue(const Value: string);
var
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    FInSetValue := True;
    try
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue(I) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;

procedure TDefineDBRadioGroup.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0 then
      TDefineRadioButton(Controls[ItemIndex]).SetFocus else
      TDefineRadioButton(Controls[0]).SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDefineDBRadioGroup.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TDefineDBRadioGroup.Click;
begin
  if not FInSetValue then
  begin
    FDataLink.Edit;
    inherited Click;    
    if ItemIndex >= 0 then Value := GetButtonValue(ItemIndex);
    if FDataLink.Editing then FDataLink.Modified;
  end;   
end;

procedure TDefineDBRadioGroup.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TDefineDBRadioGroup.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

procedure TDefineDBRadioGroup.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDefineDBRadioGroup.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8  : FDataLink.Edit;//UpdateRecord;
    #27 : FDataLink.Reset;
  end;
end;

function TDefineDBRadioGroup.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TDefineDBRadioGroup.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDefineDBRadioGroup.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.
