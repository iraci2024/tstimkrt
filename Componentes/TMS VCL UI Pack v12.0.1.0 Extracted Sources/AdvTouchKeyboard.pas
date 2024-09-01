{***************************************************************************}
{ TAdvTouchKeyboard component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright � 2007 - 2022                                        }
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

unit AdvTouchKeyboard;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Classes, Controls, ExtCtrls, Graphics, StdCtrls, Dialogs,
  IniFiles, Messages, Forms, Math, Variants, Types
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 4; // Build nr.

  // version history
  // 1.0.0.0 : first release
  // 1.0.1.0 : Fixed : issue with detection of combobox
  //         : Fixed : issues in QWERTY layout
  // 1.1.0.0 : New : method Zoom() added
  // 1.2.0.0 : Improved : key drawing
  //         : Improved : Tab key handling
  //         : Improved : zoom capability
  //         : New : QWERTZ keyboard layout
  //         : New : public property PostWMCharOnly
  // 1.2.0.1 : Fixed : issue with changing keyboard size in TAdvPopupTouchKeyBoard
  // 1.2.1.0 : Improved : tab key & arrow key handling
  // 1.2.1.1 : Fixed : issue with AutoHide
  // 1.2.1.2 : Fixed : issue with comboboxes unable to accept a key
  // 1.2.1.3 : Improved : handling of sending characters to comboboxes
  // 1.2.1.4 : Fixed : issue with sending key and make keyboard invisible
  //         : Fixed : issue with CAPS lock
  // 1.2.1.5 : Fixed : issue with key posted via WM_KEYDOWN, WM_KEYUP
  // 1.2.2.0 : New : added support for special keys : Next, Prior, Home, End
  // 1.2.2.1 : Fixed : issue with posting shifted keys and cursor in controls
  // 1.2.2.2 : Fixed : issue with CAPS key and AutoCapsDisplay
  // 1.2.2.3 : Fixed : auto positioning of keyboard close to right edge of the screen
  // 1.2.2.4 : Improved : call to TAdvPopupTouchKeyboard.ShowAtXY()
  // 1.2.2.5 : Fixed : issue with OnKeyClick and triggering modal dialogs
  // 1.2.3.0 : New : OnClose event for TAdvPopupTouchKeyboard
  // 1.2.4.0 : Fixed : Correctly gets the state of the CAPS Lock key
  // 1.2.4.1 : Fixed : size of popup keyboard when ShowCaption = false
  // 1.2.5.0 : New : OnShow, OnKeyboardCreated events added in TAdvPopupTouchKeyboard
  // 1.2.6.0 : New : multimonitor support added
  // 1.2.6.1 : Improved : uppercase/lowercase handling for international character sets
  // 1.2.7.0 : New : overload function of ShowAtXY() added
  // 1.2.8.0 : Improved : Behaviour with auto key repeat
  // 1.2.9.0 : New : property KeyboardRect: TRect added in TAdvPopupTouchKeyboard
  // 1.2.9.1 : Improved : Positioning of keyboard at border of screen
  // 1.2.10.0: New : AutoPost property added per key
  // 1.2.11.0: New : Added support for F1..F12 keys
  // 1.2.12.0: New : skDelete special key support added
  // 1.2.13.0: New : skEscape special key support added
  // 1.2.13.1: Fixed : Issue with saving keyboard layout
  // 1.2.13.2: Fixed : Issue with persisting keyboard size
  //         : Fixed : Background painting issue
  // 1.2.13.3: Fixed : Issue with backspace handling
  // 1.3.0.0 : New : Hint property per key added
  // 1.3.0.1 : Fixed : Issue with using key images when no imagelist is assigned
  // 1.3.1.0 : Improved : Replace Numlock on numeric keyboard by Backspace
  // 1.3.1.1 : Improved : Handling of multiply/add/substract/divide keys on numeric keyboard
  // 1.3.1.2 : Fixed : Issue with sending Ctrl-key combinations
  // 1.3.1.3 : Improved : Handling of special key combinations Shift/Ctrl & Alt
  // 1.4.0.0 : New: OnFocusControlChange event added for TAdvPopupTouchKeyboard
  // 1.4.1.0 : New : Wordwrapped key caption drawing support added for single caption keys
  // 1.4.2.0 : New : skCustom key type added for keys that are only handled via the OnKeyClick event
  // 1.4.3.0 : New : Per monitor support for high DPI
  // 1.4.3.1 : Fixed : Issue with TAdvTouchKeyboard used on TAdvPanel
  // 1.4.3.2 : Fixed : Issue with design-time key size changes
  // 2.0.0.0 : New : KeyString property on TAdvTouchKeyItem set custom text
  //         : New : Support for multiple characters when SpecialKey is skNone and KeyValue -1
  //         : New : OnlyShowStateCaption property makes it possible to only see the caption of the current state
  // 2.0.0.1 : Fixed : Issue with keyboard size after runtime creation
  // 2.0.1.0 : New : Event OnKeyboardPosition added
  // 2.0.2.0 : New : Support to persist keyboards with unicode characters
  // 2.0.2.1 : Fixed : Issue with saving item.KeyString value
  // 2.0.2.2 : Fixed : Issue with SaveKbdLayout on newer Delphi versions
  // 2.0.2.3 : Fixed : Issue with high DPI in Delphi 11.x
  // 2.0.2.4 : Fixed : Issue with setting custom keyboard at design-time
  //         : Fixed : Issue with highDPI in combination with form.Scaled = false

type
  TAdvTouchKeyItem = class;
  TAdvTouchKeyboard = class;

  TSpecialKey = (skNone, skAlt, skAltGr, skShift, skCaps, skCtrl, skNum, skScroll,
                 skReturn, skAdd, skDivide, skDecimal, skSubstract, skMultiply,
                 skTab, skWin, skApp, skBackSpace, skSpaceBar, skLeft, skRight, skUp, skDown, skNext, skPrior, skHome, skEnd,
                 skF1, skF2, skF3, skF4, skF5, skF6, skF7, skF8, skF9, skF10, skF11, skF12, skDelete, skEscape, skCustom);

  TTouchKeyEvent = procedure(Sender: TObject; Index: integer) of object;

  TTouchFocusChangeEvent = procedure(Sender: TObject; NewFocusControl: TWinControl; var ShowKeyboard: boolean) of object;


  TKeyboardPositionEvent = procedure(Sender: TObject; AKeyboard: TAdvTouchKeyboard; var x,y: integer) of object;

  {$IFNDEF DELPHI9_LVL}
  TSelection = record
    StartPos, EndPos: Integer;
  end;
  {$ENDIF}

 {TAdvTouchKey}

  TAdvTouchKey = class(TGraphicControl)
  private
    FCaption: string;
    FCurrentBitmap : TBitmap;
    FItem: TAdvTouchKeyItem;
    FPictureNormalState: TBitmap;
    FPictureDownState: TBitmap;
    FKeyValue: integer;
    FOnKeyDown: TKeyEvent;
    FBmp : TBitmap;
    FSpecialKey: TSpecialKey;
    FBorderColor: TColor;
    FBorderColorDown: TColor;
    FColor: TColor;
    FTextColorDown: TColor;
    FColorDown: TColor;
    FImageIndex: Integer;
    FTextColor: TColor;
    FAltGrCaption: string;
    FShiftCaption: string;
    FDownState: Byte;
    FKeyNormalPosition : Boolean;
    FSmallFont : TFont;
    FFont      : TFont;
    FAltGrKeyValue: Integer;
    FShiftKeyValue: Integer;
    FOldH, FOldW : Integer;
    FOldX, FOldY : Integer;
    FShortCut: string;
    FAutoPost: boolean;
    FDPIScale: single;
    procedure SetCaption(const Value: string);
    procedure SetPictureDownState(const Value: TBitmap);
    procedure SetPictureNormalState(const Value: TBitmap);
    procedure SetAltGrCaption(const Value: string);
    procedure SetAltrCaption(const Value: string);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    function GetDownState: boolean;
    procedure SetDownState(const Value: boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetKeyboard: TAdvTouchKeyboard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TouchKeyItem: TAdvTouchKeyItem read FItem write FItem;
    property OldH : Integer read FOldH Write fOldH;
    property OldW : Integer read FOldW Write fOldW;
    property OldX : Integer read FOldX Write fOldX;
    property OldY : Integer read FOldY Write fOldY;
    property Down: boolean read GetDownState write SetDownState;
  published
    property Left;
    property Top;
    property AutoPost: boolean read FAutoPost write FAutoPost default true;
    property ShiftCaption: string read FShiftCaption write SetAltrCaption;
    property AltGrCaption: string read FAltGrCaption write SetAltGrCaption;
    property Caption: string read FCaption write SetCaption;
    property BorderColor : TColor read FBorderColor write SetBorderColor default clGray;
    property BorderColorDown : TColor read FBorderColorDown write FBorderColorDown default clBlack;
    property Color : TColor read FColor write SetColor default clSilver;
    property ColorDown : TColor read FColorDown write FColorDown default clGray;
    property TextColor : TColor read FTextColor write SetTextColor default clBlack;
    property TextColorDown : TColor read FTextColorDown write FTextColorDown default clBlack;
    property ImageIndex : Integer read FImageIndex write FImageIndex default -1;
    property Height default 40;
    property Width default 40;
    property SpecialKey : TSpecialKey read FSpecialKey write FSpecialKey;
    property KeyValue: Integer read FKeyValue write FKeyvalue default -1;
    property ShiftKeyValue: Integer read FShiftKeyValue write FShiftKeyValue default -1;
    property AltGrKeyValue: Integer read FAltGrKeyValue write FAltGrKeyValue default -1;
    property PictureDownState : TBitmap read FPictureDownState write SetPictureDownState;
    property PictureNormalState : TBitmap read FPictureNormalState write SetPictureNormalState;
    property ShortCut: string read FShortCut write FShortCut;
    property OnKeyDown : TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

{TAdvTouchKeyItem}

  TAdvTouchKeyItem = class(TCollectionItem)
  private
    FTouchKey: TAdvTouchKey;
    FKeyString: string;
    function GetCaption: string;
    function GetX: integer;
    function GetY: integer;
    procedure SetCaption(const Value: string);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetPictureDownState(const Value: TBitmap);
    procedure SetPictureNormalState(const Value: TBitmap);
    function GetPictureDownState: TBitmap;
    function GetPictureNormalState: TBitmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetOldH: Integer;
    function GetOldW: Integer;
    procedure SetOldH(const Value: Integer);
    procedure SetOldW(const Value: Integer);
    function GetOldX: Integer;
    function GetOldY: Integer;
    procedure SetOldX(const Value: Integer);
    procedure SetOldY(const Value: Integer);
    function GetKeyValue: Integer;
    procedure SetKeyvalue(const Value: Integer);
    function GetBorderColor: TColor;
    function GetBorderColorDown: TColor;
    function GetColor: TColor;
    function GetColorDown: TColor;
    function GetImageIndex: Integer;
    function GetSpecialKey: TSpecialKey;
    function GetTextColor: TColor;
    function GetTextColorDown: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetImageIndex(const Value: Integer);
    procedure SetSpecialKey(const Value: TSpecialKey);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextColorDown(const Value: TColor);
    function GetShiftCaption: string;
    function GetAltGrCaption: string;
    procedure SetAltGrCaption(const Value: string);
    procedure SetShiftCaption(const Value: string);
    function GetAltGrKeyValue: Integer;
    function GetShiftKeyValue: Integer;
    procedure SetAltGrKeyValue(const Value: Integer);
    procedure SetShiftKeyValue(const Value: Integer);
    function GetShortCut: string;
    procedure SetShortCut(const Value: string);
    function GetAutoPost: boolean;
    procedure SetAutoPost(const Value: boolean);
    function GetHint: string;
    procedure SetHint(const Value: string);
    procedure SetKeyString(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OldH : Integer read GetOldH write SetOldH;
    property OldW : Integer read GetOldW write SetOldW;
    property OldX : Integer read GetOldX write SetOldX;
    property OldY : Integer read GetOldY write SetOldY;
  published
    property AutoPost: boolean read GetAutoPost write SetAutoPost default true;
    property Caption: string read GetCaption write SetCaption;
    property ShiftCaption: string read GetShiftCaption write SetShiftCaption;
    property AltGrCaption: string read GetAltGrCaption write SetAltGrCaption;
    property KeyValue: Integer read GetKeyValue write SetKeyvalue;
    property ShiftKeyValue: Integer read GetShiftKeyValue write SetShiftKeyValue;
    property AltGrKeyValue: Integer read GetAltGrKeyValue write SetAltGrKeyValue;
    property PictureDownState : TBitmap read GetPictureDownState write SetPictureDownState;
    property PictureNormalState : TBitmap read GetPictureNormalState write SetPictureNormalState;
    property Height : Integer read GetHeight write SetHeight;
    property Width : Integer read GetWidth write SetWidth;
    property SpecialKey : TSpecialKey read GetSpecialKey write SetSpecialKey;
    property BorderColor : TColor read GetBorderColor write SetBorderColor;
    property BorderColorDown : TColor read GetBorderColorDown write SetBorderColorDown;
    property Color : TColor read GetColor write SetColor;
    property ColorDown : TColor read GetColorDown write SetColorDown;
    property TextColor : TColor read GetTextColor write SetTextColor;
    property TextColorDown : TColor read GetTextColorDown write SetTextColorDown;
    property ImageIndex : Integer read GetImageIndex write SetImageIndex;
    property ShortCut: string read GetShortCut write SetShortCut;
    property Hint: string read GetHint write SetHint;
    property X: integer read GetX write SetX;
    property Y: integer read GetY write SetY;
    property KeyString: string read FKeyString write SetKeyString;
  end;

{TAdvTouchKeyCollection}

  TAdvTouchKeyCollection = class(TCollection)
  private
    FOwner : TComponent;
    function GetItem(Index: Integer): TAdvTouchKeyItem;
    procedure SetItem(Index: Integer; const Value: TAdvTouchKeyItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TComponent);
    function Add: TAdvTouchKeyItem;
    function Insert(index:integer): TAdvTouchKeyItem;
    property Items[Index: Integer]: TAdvTouchKeyItem read GetItem write SetItem; default;
  end;

  TKeyboardType = (ktQWERTY, ktAZERTY, ktDVORAK, ktNUMERIC, ktCELLPHONE, ktQWERTZ, ktCustom);

  TDrawKeyEvent = procedure(Sender: TObject; Key: TAdvTouchKeyItem; Canvas: TCanvas;
                   Down: Boolean; Rect: TRect; var DefaultDraw: boolean) of Object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTouchKeyboard = class(TCustomPanel)
  private
    FCapsTimer: TTimer;
    FKeys: TAdvTouchKeyCollection;
    FOnKeyDown: TKeyEvent;
    FPictureNormalState: TBitmap;
    FPictureDownState: TBitmap;
    FSmallFont: TFont;
    FKeyboardType: TKeyboardType;
    FOnDrawKey: TDrawKeyEvent;
    FImages: TImageList;
    FShift: TShiftState;
    FAutoPostKey : Boolean;
    FHighlightCaps: TColor;
    FAutoCapsDisplay: Boolean;
    FHighlightAltGr: TColor;
    FDisableErase: boolean;
    FRepeatTimer: TTimer;
    FRepeatTimerCount: integer;
    FRepeatItemIndex: integer;
    FOnKeyClick: TTouchKeyEvent;
    FKeyDistance: Integer;
    FOldW,FOldH: integer;
    FPostWMCharOnly: boolean;
    FCapsDown: boolean;
    FCtrlDown: boolean;
    FAltDown: boolean;
    FRepeatHandle: THandle;
    FOnlyShowStateCaption: Boolean;
    FDPIScale: single;
    FDesignTime: boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure SetKeys(const Value: TAdvTouchKeyCollection);
    procedure SetPictureDownState(const Value: TBitmap);
    procedure SetPictureNormalState(const Value: TBitmap);
    procedure SetSmallFont(const Value: TFont);
    procedure SetKeyboardType(const Value: TKeyboardType);
    procedure AddKey(Caption, ShiftCaption, AltGrCaption : ShortString;
        KeyValue, ShiftKeyValue, AltGrKeyValue, ImageIndex, Width, Height: Integer; var X : Integer; Y : Integer;
        SpecialKey : TSpecialKey; Color: TColor = clSilver);
    procedure NewRow(var X, Y : Integer; Size : Integer);
    procedure SetImages(const Value: TImageList);
    procedure PostNormalKeys(Index: Integer);
    procedure PostKeyString(Index: Integer);
    procedure PostSpecialKeys(Key: Word; const pShift: TShiftState; SpecialKey: Boolean);
    function GetKeybdInputHandle: HWND;
    procedure PictureChanged(Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetAutoCapsDisplay(const Value: Boolean);
    procedure SetHighlightAltGr(const Value: TColor);
    procedure SetHighlightCaps(const Value: TColor);
    procedure SetKeyDistance(Value : Integer);
    function IsCapsDown: boolean;
    function IsCtrlDown: boolean;
    function IsAltDown: boolean;
    procedure SetOnlyShowStateCaption(const Value: Boolean);
    procedure SetShift(const Value: TShiftState);
  protected
    procedure ItemKeyDown(Index: Integer);
    procedure ItemKeyUp(Index: Integer);
    procedure SyncEqualKeys(Index: Integer);
    procedure TurnOffShifts;
    procedure DrawKey(Sender: TObject; Key: TAdvTouchKeyItem; Canvas: TCanvas;
                   Down: Boolean; Rect: TRect; var DefaultDraw: boolean);
    procedure BuildQWERTYKeyBoard;
    procedure BuildQWERTZKeyBoard;
    procedure BuildAZERTYKeyBoard;
    procedure BuildDVORAKKeyBoard;
    procedure BuildNumericKeyBoard;
    procedure BuildCellPhoneKeyboard;
    property Shift : TShiftState read FShift write SetShift;
    property KeybdInputHandle : HWND read GetKeybdInputHandle;
    procedure CreateWnd; override;
    procedure PaintAllKeys;
    procedure Paint; override;
    procedure StartTimer(index: integer);
    procedure StopTimer;
    procedure RepeatTimerProc(Sender: TObject);
    procedure CapsTimerProc(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Zoom(fhorz,fvert: double; keysonly: boolean = false; absoluteZoom : boolean = false);
    procedure SaveKeybdLayout(FileName: string);
    procedure LoadKeybdLayout(FileName: string);
    procedure PostKey(Key: Word; const pShift: TShiftState; ExtendedKeyBD: Boolean; Index: Integer);
    property PostWMCharOnly: Boolean read FPostWMCharOnly write FPostWMCharOnly;
  published
    property Align;
    property Font;
    property Color;
    property AutoPostKey : Boolean read FAutoPostKey write FAutoPostKey default true;
    property AutoCapsDisplay: Boolean read FAutoCapsDisplay write SetAutoCapsDisplay default false;
    property HighlightCaps: TColor read FHighlightCaps write SetHighlightCaps default clNone;
    property HighlightAltGr: TColor read FHighlightAltGr write SetHighlightAltGr default clNone;
    property Images : TImageList read FImages write SetImages;
    property KeyboardType : TKeyboardType read FKeyboardType write SetKeyboardType;
    property KeyDistance : Integer Read FKeyDistance Write SetKeyDistance default 0;
    property Keys : TAdvTouchKeyCollection read FKeys write SetKeys;
    property OnlyShowStateCaption: Boolean read FOnlyShowStateCaption write SetOnlyShowStateCaption default False;
    property PictureDownState : TBitmap read FPictureDownState write SetPictureDownState;
    property PictureNormalState : TBitmap read FPictureNormalState write SetPictureNormalState;
    property SmallFont : TFont read FSmallFont write SetSmallFont;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    // Events
    property OnKeyClick: TTouchKeyEvent read FOnKeyClick write FOnKeyClick;
    property OnKeyDown : TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnDrawKey : TDrawKeyEvent read FOnDrawKey write FOnDrawKey;
  end;

  TKeyboardToolForm = class(TCustomForm)
  private
    FShowCaption: boolean;
    FShowClose: boolean;
    FPrevPPI: integer;
    FFirst: Boolean;
    FHeight: integer;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF DELPHIXE10_LVL}
    procedure WMDpiChanged(var Message: TWMDpi); message WM_DPICHANGED;
    {$ENDIF}
  published
    property ShowClose: boolean read FShowClose write FShowClose;
    property ShowCaption: boolean read FShowCaption write FShowCaption;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPopupTouchKeyBoard = class(TComponent)
  private
    FTimer: TTimer;
    FFrm : TKeyboardToolForm;
    FKbd : TAdvTouchKeyboard;
    FOwnerform: TCustomForm;
    FAutoCapsDisplay: Boolean;
    FAutoPostKey: Boolean;
    FHighlightCaps: TColor;
    FKeyboardType: TKeyboardType;
    FHighlightAltGr: TColor;
    FAutoFollowFocus: Boolean;
    FAutoHide: Boolean;
    FShowCaption: boolean;
    FShowClose: boolean;
    FDisableSizing: boolean;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnKeyboardCreated: TNotifyEvent;
    FLastX,FLastY: integer;
    FOldWnd: THandle;
    FOldShow: boolean;
    FOnFocusControlChange: TTouchFocusChangeEvent;
    FOnKeyboardPosition: TKeyboardPositionEvent;
    procedure SetAutoCapsDisplay(const Value: Boolean);
    procedure SetAutoPostKey(const Value: Boolean);
    procedure SetHighlightAltGr(const Value: TColor);
    procedure SetHighlightCaps(const Value: TColor);
    procedure SetKeyboardType(const Value: TKeyboardType);
    function GetKeyboardRect: TRect;
  protected
    procedure TimerProc(Sender: TObject);
    procedure DoFocusControlChange(AControl: TWinControl; var ShowKeyboard: boolean); virtual;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateForm; virtual;
    procedure Show;
    procedure ShowAtXY(x,y: integer); overload;
    procedure ShowAtXY(AParent: TCustomForm; x,y: integer); overload;
    procedure Hide;
    property KeyboardRect: TRect read GetKeyboardRect;
    property Keyboard: TAdvTouchKeyboard read FKbd;
  published
    property ShowCaption: boolean read FShowCaption write FShowCaption default true;
    property ShowClose: boolean read FShowClose write FShowClose default true;
    property AutoFollowFocus: Boolean read FAutoFollowFocus write FAutoFollowFocus default false;
    property AutoHide: Boolean read FAutoHide write FAutoHide default false;
    property AutoPostKey : Boolean read FAutoPostKey write SetAutoPostKey default true;
    property AutoCapsDisplay: Boolean read FAutoCapsDisplay write SetAutoCapsDisplay default false;
    property HighlightCaps: TColor read FHighlightCaps write SetHighlightCaps default clNone;
    property HighlightAltGr: TColor read FHighlightAltGr write SetHighlightAltGr default clNone;
    property KeyboardType : TKeyboardType read FKeyboardType write SetKeyboardType;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnKeyboardCreated: TNotifyEvent read FOnKeyboardCreated write FOnKeyboardCreated;
    property OnKeyboardPosition: TKeyboardPositionEvent read FOnKeyboardPosition write FOnKeyboardPosition;
    property OnFocusControlChange: TTouchFocusChangeEvent read FOnFocusControlChange write FOnFocusControlChange;
  end;


implementation
uses
  AdvStyleIF;

function IsComboBox(hwnd: THandle): boolean;
var
  ClassName: array[0..255] of char;
begin
  GetClassName(hwnd, ClassName, SizeOf(ClassName));

  if Pos('EDIT',Uppercase(StrPas(ClassName))) >  0 then
  begin
    GetClassName(GetParent(hwnd), ClassName, SizeOf(ClassName));
  end;

  Result := Pos('COMBO',Uppercase(StrPas(ClassName))) >  0;
end;


procedure StretchTransparentBitmap(hdc: THandle; hBitmap: THandle;
xStart, yStart: Integer; width, height, offsx, offsy, bmpw, bmph: Integer;
cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  StretchBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  StretchBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcback, 0, 0, ptsize.x, ptsize.y, SRCAND);
  StretchBlt(hdcMem, 0, 0, ptSize.X, ptSize.Y, hdctemp, offsx, offsy, bmpw, bmph, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcSave, 0, 0, ptsize.x, ptsize.y, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure DrawTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  BitBlt(hdcTemp, offsx, offsy, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);


  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure BitmapStretch(bmp: TBitmap; Canvas: TCanvas; x, y, height: integer);
var
  mid: integer;
  fillh: integer;
  c: TColor;
begin
  mid := bmp.height div 2;
  fillh := height - bmp.height;
  c := bmp.Canvas.Pixels[0, 0]; //bmp.Height - 1];

  DrawTransparentBitmap(Canvas.Handle, bmp.handle, x, y, bmp.Width, bmp.Height div 2, 0, 0, c);
  StretchTransparentBitmap(Canvas.Handle, bmp.Handle, x, y + mid, bmp.width, fillh, 0, mid - 1, bmp.Width, 2, c);
  DrawTransparentBitmap(Canvas.Handle, bmp.handle, x, y + mid + fillh, bmp.width, bmp.Height div 2, 0, mid, c);
end;

procedure BitmapStretchInWidth(bmp: TBitmap; Canvas: TCanvas; X, Y, Width: Integer);
var
  mid: integer;
  fillw: integer;
  c: TColor;
  ofs: Integer;
begin
  // bitmap center value
  mid := bmp.Width div 2;
  // filling width
  fillw := width - bmp.Width- 1;
  ofs := 0;
  // transparent color
  c := bmp.Canvas.Pixels[0, 0];
  
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, mid, bmp.Height, 0, 0, c);
  StretchTransparentBitmap(canvas.Handle, bmp.Handle, x + mid, y, fillw, bmp.Height, mid - 1, 0, 2, bmp.Height, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x + mid + fillw, y, mid, bmp.Height, mid + ofs, 0, c);
end;

{ TAdvTouchKeyboard }

procedure TAdvTouchKeyboard.AddKey(Caption, ShiftCaption,
  AltGrCaption: ShortString; KeyValue, ShiftKeyValue, AltGrKeyValue,
  ImageIndex, Width, Height: Integer; var X :Integer; Y: Integer;
  SpecialKey: TSpecialKey; Color: TColor = clSilver);
begin
  with FKeys do
  begin
    Add;
    Items[FKeys.Count - 1].AutoPost := true;
    Items[FKeys.Count - 1].Caption := string(Caption);
    Items[FKeys.Count - 1].Color := Color;
    Items[FKeys.Count - 1].ShiftCaption := string(ShiftCaption);
    Items[FKeys.Count - 1].AltGrCaption := string(AltGrCaption);
    Items[FKeys.Count - 1].KeyValue := KeyValue;
    Items[FKeys.Count - 1].ShiftKeyValue := ShiftKeyValue;
    Items[FKeys.Count - 1].AltGrKeyValue := AltGrKeyValue;
    if not (csDesigning in Self.ComponentState) then
    begin
      if Assigned((Owner as TAdvTouchKeyboard).Images) then
        Items[FKeys.Count - 1].ImageIndex := ImageIndex
    end
    else
      Items[FKeys.Count - 1].ImageIndex := -1;

    Items[FKeys.Count - 1].SpecialKey := SpecialKey;
    Items[FKeys.Count - 1].Width := Width;
    Items[FKeys.Count - 1].Height := Height;
    Items[FKeys.Count - 1].X := X;
    Items[FKeys.Count - 1].Y := Y;
    X := X + Items[FKeys.Count - 1].Width;
  end;
end;

procedure TAdvTouchKeyboard.CapsTimerProc(Sender: TObject);
var
  State: shortint;
  FNewCapsDown: boolean;
begin
  State := GetKeyState(VK_CAPITAL);
  FNewCapsDown := (State and $1) <> 0;
  if FNewCapsDown <> FCapsDown then
  begin
    Invalidate;
    FCapsDown := FNewCapsDown;
  end;
end;

constructor TAdvTouchKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FPictureNormalState := TBitmap.Create;
  FPictureDownState := TBitmap.Create;

  FPictureDownState.OnChange := PictureChanged;
  FPictureNormalState.OnChange := PictureChanged;

  if (GetKeyState(VK_CAPITAL) = 1) then
  begin
    FShift := [ssShift];
    FCapsDown := true;
  end
  else
  begin
    FShift := [];
    FCapsDown := false;
  end;

  FCtrlDown := false;
  FAltDown := false;

  FOnlyShowStateCaption := False;

  FKeys := TAdvTouchKeyCollection.Create(self);
  FSmallFont := TFont.Create;
  FSmallFont.OnChange := FontChanged;
  BevelOuter := bvNone;
  Color := clBtnFace;
  FDPIScale := 1;
  Height := 200;
  Width  := 160;
  FAutoPostKey := True;
  FHighlightCaps := clNone;
  FHighlightAltGr := clNone;
  Font.Height := -11;
  SmallFont.Height := -9;
  FKeyDistance := 0;
  KeyboardType := ktQWERTY;
  FRepeatTimer := TTimer.Create(self);
  FRepeatTimer.Interval := 200;
  FRepeatTimer.Enabled := false;
  FRepeatTimer.OnTimer := RepeatTimerProc;
  FOldW := -1;
  FOldH := -1;
  FCapsTimer := TTimer.Create(self);
  FCapsTimer.Interval := 250;
  FCapsTimer.Enabled := true;
  FCapsTimer.OnTimer := CapsTimerProc;
end;

procedure TAdvTouchKeyboard.CreateWnd;
{$IFDEF DELPHIXE10_LVL}
var
  frm: TCustomForm;
{$ENDIF}
begin
  inherited;

  {$IFDEF DELPHIXE10_LVL}
  frm := GetParentForm(Self);
  if Assigned(frm) then
  begin
    if (frm is TForm) and ((frm as TForm).Scaled or (csDesigning in ComponentState)) then
    begin
      {$IFDEF DELPHIXE14_LVL}
      FDPIScale := frm.CurrentPPI / 96;
      {$ELSE}
      FDPIScale := frm.Monitor.PixelsPerInch / 96;
      {$ENDIF}
      if KeyboardType <> ktCustom then
        SetKeyboardType(KeyboardType);
    end;
  end;
  {$ENDIF}
end;

destructor TAdvTouchKeyboard.Destroy;
begin
  FKeys.Free;
  FPictureNormalState.Free;
  FPictureDownState.Free;
  FSmallFont.Free;
  FRepeatTimer.Free;
  FCapsTimer.Free;
  inherited;
end;

procedure TAdvTouchKeyboard.DrawKey(Sender: TObject; Key: TAdvTouchKeyItem;
  Canvas: TCanvas; Down: Boolean; Rect: TRect; var DefaultDraw: boolean);
begin
  if Assigned(FOnDrawKey) then
    FOnDrawKey(Sender, Key, Canvas, Down, Rect, DefaultDraw);
end;

procedure TAdvTouchKeyboard.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvTouchKeyboard.GetKeybdInputHandle: HWND;
begin
  Result := GetFocus;
end;

function TAdvTouchKeyboard.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvTouchKeyboard.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvTouchKeyboard.BuildAZERTYKeyBoard;
var
  CurrentX,
  CurrentY,
  Size      : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;

  Size := Round(40 * FDPIScale);
  if not (csLoading in ComponentState) then
  begin
    Height := Round(200 * FDPIScale);
    Width  := Round(600 * FDPIScale);
  end;

  with Keys do
  begin
  AddKey('�', '�','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('&', '1','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '2','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('"', '3','#', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('''', '4','',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('(', '5','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '6','^',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '7','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('!', '8','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '9','{', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '0','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(')', '�','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','�', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('^', '�','[',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('$', '*',']', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  CurrentX := CurrentX + (Size div 4);
  AddKey('Enter', '','', VK_RETURN, -1, -1, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, $A0A0A0);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, -1, -1, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, $A0A0A0);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '%','�', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('�', '�','`', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 5) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  AddKey('<', '>','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(';', '.','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(';', ',','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(':', '/','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, $A0A0A0);
  AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  end;
end;

procedure TAdvTouchKeyboard.BuildDVORAKKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;
  Size := Round(40 * FDPIScale);
  if not (csLoading in ComponentState) then
  begin
    Height := Round(200 * FDPIScale);
    Width  := Round(600 * FDPIScale);
  end;

  with Keys do
  begin
  AddKey('`', '~','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('1', '!','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '@','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '#','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '^','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('7', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '*','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '(','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', ')','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('[', '{','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(']', '}','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, $A0A0A0);
  AddKey('''', '"','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '<','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '>','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('/', '?','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('\', '|','', -1, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skNone);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, -1, -1, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Enter', '','', VK_RETURN, -1, -1, -1, (Size * 9) div 4, Size, CurrentX, CurrentY, skReturn, $A0A0A0);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 9) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  AddKey(';', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', VK_NONCONVERT, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, $A0A0A0);
  AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  end;
end;

procedure TAdvTouchKeyboard.BuildCellPhoneKeyBoard;
var
  CurrentX,
  CurrentY,
  Size : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;

  Size := Round(40 * FDPIScale);
  if not (csLoading in ComponentState) then
  begin
    Height := Round(160 * FDPIScale);
    Width  := Round(120 * FDPIScale);
  end;

  with Keys do
  begin
  AddKey('7', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('4', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('1', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('*', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Back', '', '', VK_BACK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skBackspace, $A0A0A0);
  end;
end;


procedure TAdvTouchKeyboard.BuildNumericKeyBoard;
var
  CurrentX,
  CurrentY,
  Size : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;

  Size := Round(40 * FDPIScale);
  if not (csLoading in ComponentState) then
  begin
    Height := Round(200 * FDPIScale);
    Width  := Round(160 * FDPIScale);
  end;

  with Keys do
  begin
  AddKey('Back', '','', VK_BACK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skBackSpace, $A0A0A0);
  AddKey('/', '', '', VK_DIVIDE, -1, -1, -1, Size, Size, CurrentX, CurrentY, skDivide);
  AddKey('*', '', '', VK_MULTIPLY, -1, -1, -1, Size, Size, CurrentX, CurrentY, skMultiply);
  AddKey('-', '','', VK_SUBTRACT, -1, -1, -1, Size, Size, CurrentX, CurrentY, skSubstract);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('7', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('+', '', '', VK_ADD, -1, -1, -1, Size, Size * 2, CurrentX, CurrentY, skAdd);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('4', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('1', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Enter', '', '', VK_RETURN, -1, -1, -1, Size, Size * 2, CurrentX, CurrentY, skReturn, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('0', '', '', -1, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '', '', VK_DECIMAL, -1, -1, -1, Size, Size, CurrentX, CurrentY, skDecimal);
  end;
end;

procedure TAdvTouchKeyboard.BuildQWERTYKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;

  Size := Round(40 * FDPIScale);

  if not (csLoading in ComponentState) then
  begin
    Height := Round(200 * FDPIScale);
    Width  := Round(600 * FDPIScale);
  end;

  with Keys do
  begin
  //AddKey('�', '�','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('`', '~','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('1', '!','|', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  //AddKey('2', '"','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '@','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '#','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '^','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('7', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '*','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '(','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', ')','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('\', '|','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Back', '','', VK_BACK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skBackSpace, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, $A0A0A0);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('[', '{','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(']', '}','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  CurrentX := CurrentX + (Size div 4);
  AddKey('Enter', '','', VK_RETURN, 0, 0, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, $A0A0A0);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, 0, 0, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(';', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '"','', -1, -1,-1, -1, Size, Size, CurrentX, CurrentY, skNone);
//  AddKey('�', '','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  //AddKey('<', '>','', -1, -1 , -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '<','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '>','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('/', '?','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, $A0A0A0);

  //AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  AddKey('->', '','', VK_RIGHT, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skRight, $A0A0A0);

  end;
end;
procedure TAdvTouchKeyboard.BuildQWERTZKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;

begin
  CurrentY := 0;
  CurrentX := CurrentY;

  Size := Round(40 * FDPIScale);
  if not (csLoading in ComponentState) then
  begin
    Height := Round(200 * FDPIScale);
    Width  := Round(600 * FDPIScale);
  end;

  with Keys do
  begin
    AddKey('^', '�','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('1', '!','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('2', '"','�', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('3', '�','�', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('6', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('7', '/','{', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('8', '(','[', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('9', ')',']', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('0', '=','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('�', '?','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('�', '`','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, $A0A0A0);
    // End Row #1
    NewRow(CurrentX, CurrentY, Size);
    AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab);
    AddKey('Q', '','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('E', '','�', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('�', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('+', '*','~', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    CurrentX := CurrentX + (Size div 4);
    AddKey('Enter', '','', VK_RETURN, 0, 0, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, $A0A0A0);
    // End Row #2
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Caps Lock', '','', VK_CAPITAL, 0, 0, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, $A0A0A0);
    AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('�', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('�', '','', -1, -1,-1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('#', '''','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    // End Row #3
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 5) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
    AddKey('<', '>','', -1, -1 , -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey(',', ';','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('.', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, $A0A0A0);
    // End Row #4
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
    AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
    AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, $A0A0A0);
    AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
    AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, $A0A0A0);
    AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, $A0A0A0);
    AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, $A0A0A0);
    AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  end;
end;

procedure TAdvTouchKeyboard.TurnOffShifts;
var
  i: integer;
  invalid: boolean;
begin
  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skCaps) then
    begin
      if Keys[i].FTouchKey.FDownState > 0 then
        Exit;
    end;
  end;

  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skShift) then
    begin
      Keys[i].FTouchKey.FDownState := 0;
      Keys[i].FTouchKey.Invalidate;
      Keys[i].FTouchKey.FKeyNormalPosition := true;
    end;
  end;

  invalid := ssShift in Shift;
  Shift  := Shift - [ssShift];

  if invalid then
    Invalidate;
end;

procedure TAdvTouchKeyboard.SyncEqualKeys(Index: Integer);
var
  i: integer;
  ds: integer;
  kn: boolean;
  sk: TSpecialKey;

begin
  with Keys.Items[Index] do
  begin
    ds := FTouchKey.FDownState;
    kn := FTouchKey.FKeyNormalPosition;
    sk := SpecialKey;

    for i := 0 to Keys.Count - 1 do
    begin
      if (Keys[i].SpecialKey = sk) then
      begin
        Keys[i].FTouchKey.FDownState := ds;
        Keys[i].FTouchKey.FKeyNormalPosition := kn;
        Keys[i].FTouchKey.Invalidate;
      end;
    end;
  end;
end;

function TAdvTouchKeyboard.IsCapsDown: boolean;
begin
  Result := FCapsDown;
end;

function TAdvTouchKeyboard.IsCtrlDown: boolean;
begin
  Result := FCtrlDown;
end;

function TAdvTouchKeyboard.IsAltDown: boolean;
begin
  Result := FAltDown;
end;

procedure TAdvTouchKeyboard.ItemKeyUp(Index: Integer);
begin
  if Assigned(FOnKeyClick) then
    FOnKeyClick(Self, Index);
end;

procedure TAdvTouchKeyboard.ItemKeyDown(Index: Integer);
var
  Key : Word;
  i: integer;
  ch: char;
  c: string;
begin
  if Keys.Items[Index].FKeyString <> '' then
  begin
    PostKeyString(Index);
    Exit;
  end;

  with Keys.Items[Index] do
  begin
    Key := KeyValue;

    if FAutoPostKey then
    begin
      if ShortCut <> '' then
      begin
        for i := 1 to length(ShortCut) do
        begin
          if not FPostWMCharOnly then
            SendMessage(KeybdInputHandle, WM_KEYDOWN, Ord(ShortCut[i]),0);

          SendMessage(KeybdInputHandle, WM_CHAR, Ord(ShortCut[i]),0);

          if not FPostWMCharOnly then
            SendMessage(KeybdInputHandle, WM_KEYUP, Ord(ShortCut[i]),0);
        end;
      end
      else
      begin
        if not (SpecialKey = skNone)  then
        begin
          if KeyValue = -1 then
            case SpecialKey of
            skUp: Key := VK_UP;
            skDown: Key := VK_DOWN;
            skRight: Key := VK_RIGHT;
            skLeft: Key := VK_LEFT;
            skNext: Key := VK_NEXT;
            skPrior: Key := VK_PRIOR;
            skHome: Key := VK_HOME;
            skEnd: Key := VK_END;
            skSpaceBar: Key := VK_SPACE;
            skReturn: Key := VK_RETURN;
            skBackSpace: Key := VK_BACK;
            skSubstract: Key := VK_SUBTRACT;
            skMultiply: Key := VK_MULTIPLY;
            skDivide: Key := VK_DIVIDE;
            skAdd: Key := VK_ADD;
            skF1: Key := VK_F1;
            skF2: Key := VK_F2;
            skF3: Key := VK_F3;
            skF4: Key := VK_F4;
            skF5: Key := VK_F5;
            skF6: Key := VK_F6;
            skF7: Key := VK_F7;
            skF8: Key := VK_F8;
            skF9: Key := VK_F9;
            skF10: Key := VK_F10;
            skF11: Key := VK_F11;
            skF12: Key := VK_F12;
            skDelete: Key := VK_DELETE;
            skEscape: Key := VK_ESCAPE;
            skCustom: Key := 0;
            end;
          if Key <> 0 then
            PostSpecialKeys(Key, Shift, False)
        end
        else
        begin
          c := Keys.Items[Index].Caption;

          if IsAltDown then
          begin
            if length(c) > 0 then
            begin
              ch := c[1];
              Key := ord(ch);

              if (IsCtrlDown) then
                PostSpecialKeys(Key, [ssCtrl, ssAlt], False)
              else
                PostSpecialKeys(Key, [ssAlt], False);
            end;
          end
          else
          if IsCtrlDown then
          begin
            if length(c) > 0 then
            begin
              ch := c[1];
              Key := ord(ch);
              if (ssShift in Shift ) then
                PostSpecialKeys(Key, [ssCtrl, ssShift], False)
              else
                PostSpecialKeys(Key, [ssCtrl], False)
            end;
          end
          else
            PostNormalKeys(Index);
        end;
      end;
    end;

    if SpecialKey = skCtrl then
      FCtrlDown := not FCtrlDown;

    if SpecialKey = skAlt then
      FAltDown := not FAltDown;

    if (SpecialKey in [skShift, skAlt, skCtrl, skAltGr]) then
      SyncEqualKeys(Index)
    else
    begin
      TurnOffShifts;
    end;

    if Assigned(FOnKeyDown) then
      FOnKeyDown(Self.Keys.Items[Index], Key, Shift);

    if (SpecialKey = skApp) then
      Keys.Items[Index].FTouchKey.FKeyNormalPosition := True;
  end;
end;

procedure TAdvTouchKeyboard.LoadKeybdLayout(FileName: string);
var
  IniFile: TMemIniFile;
  i, TotalKeys: Integer;

begin
  {$IFDEF DELPHIXE_LVL}
  try
    IniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
  except
    IniFile := TMemIniFile.Create(FileName);
  end;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  IniFile := TMemIniFile.Create(FileName);
  {$ENDIF}

  with Keys do
  begin
    TotalKeys := IniFile.ReadInteger('Keys','Count', 0);
    Clear;
    try
      for i := 0 to TotalKeys - 1 do
        begin
        Add;
        Items[i].Caption        := IniFile.ReadString  ('Key' + IntToStr(i), 'Caption', '');
        Items[i].ShiftCaption   := IniFile.ReadString  ('Key' + IntToStr(i), 'ShiftCaption', '');
        Items[i].AltGrCaption   := IniFile.ReadString  ('Key' + IntToStr(i), 'AltGrCaption', '');
        Items[i].KeyValue       := IniFile.ReadInteger ('Key' + IntToStr(i), 'KeyValue', -1);
        Items[i].ShiftKeyValue  := IniFile.ReadInteger ('Key' + IntToStr(i), 'ShiftKeyValue', -1);
        Items[i].AltGrKeyValue  := IniFile.ReadInteger ('Key' + IntToStr(i), 'AltGrKeyValue', -1);

        Items[i].KeyString      := IniFile.ReadString('Key' + IntToStr(i), 'KeyString', '');

        if Assigned((Owner as TAdvTouchKeyboard).Images) then
          Items[i].ImageIndex   := IniFile.ReadInteger ('Key' + IntToStr(i), 'ImageIndex', -1)
        else
          Items[i].ImageIndex   := -1;

        Items[i].SpecialKey     := TSpecialKey(IniFile.ReadInteger ('Key' + IntToStr(i), 'SpecialKey', 0));

        Items[i].Color          := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'Color', ''));
        Items[i].ColorDown      := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'ColorDown', ''));
        Items[i].BorderColor    := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'BorderColor', ''));
        Items[i].BorderColorDown:= StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'BorderColorDown', ''));
        Items[i].TextColor      := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'TextColor', ''));
        Items[i].TextColorDown  := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'TextColorDown', ''));

        Items[i].Width          := IniFile.ReadInteger ('Key' + IntToStr(i), 'Width', 40);
        Items[i].Height         := IniFile.ReadInteger ('Key' + IntToStr(i), 'Height', 40);
        Items[i].X              := IniFile.ReadInteger ('Key' + IntToStr(i), 'X', 0);
        Items[i].Y              := IniFile.ReadInteger ('Key' + IntToStr(i), 'Y', 0);
        Items[i].AutoPost       := IniFile.ReadInteger ('Key' + IntToStr(i), 'AutoPost', 1) = 1;
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TAdvTouchKeyboard.NewRow(var X, Y: Integer; Size: Integer);
begin
  X := 0;
  Y := Y + Size;
end;

procedure TAdvTouchKeyboard.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.FillRect(ClientRect);
end;

procedure TAdvTouchKeyboard.PaintAllKeys;
var
  i: integer;
begin
  for i := 0 to Keys.Count - 1 do
    Keys[i].FTouchKey.Repaint;
end;

procedure TAdvTouchKeyboard.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvTouchKeyboard.PostKey(Key: Word; const pShift: TShiftState;
  ExtendedKeyBD: Boolean; Index: Integer);
begin
  if Self.Keys.Items[Index].FKeyString <> '' then
    PostKeyString(Index)
  else if not (Self.Keys.Items[Index].SpecialKey = skNone)  then
    PostSpecialKeys(Key, Shift, ExtendedKeyBD)
  else
    PostNormalKeys(Index);
end;

procedure TAdvTouchKeyboard.PostKeyString(Index: Integer);
var
  j: integer;
  isCombo: Boolean;
  s: string;
begin
  s := Keys[Index].FKeyString;
  if (s <> '') then
    begin
      isCombo := IsComboBox(KeybdInputHandle);

      for j := 1 to Length(s) do
      begin
        if not FPostWMCharOnly and not isCombo then
        begin
          if (Upcase(s[j]) >= '0') and
             (Upcase(s[j]) <= 'Z') then
             SendMessage(KeybdInputHandle, WM_KEYDOWN, Ord(Upcase(s[j])),0);
        end;

        SendMessage(KeybdInputHandle, WM_CHAR, Ord(s[j]),0);

        if not FPostWMCharOnly and not isCombo then
        begin
          if (Upcase(s[j]) >= '0') and
             (Upcase(s[j]) <= 'Z') then
          SendMessage(KeybdInputHandle, WM_KEYUP, Ord(Upcase(s[j])),0);
        end;
      end;
    end;
end;

procedure TAdvTouchKeyboard.PostNormalKeys(Index: Integer);
type
  TKeyVal = (kvNormal, kvShifted, kvAtlGr, kvCapital);
var
  KeyCaption: string;
  KeyVal    : TKeyVal;
  KVal, i, j: integer;
  isCombo: boolean;
  isCaps: boolean;

begin
  KeyCaption := '';
  KVal := -1;
  isCaps := false;

  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skCaps) then
    begin
      if Keys[i].FTouchKey.FDownState > 0 then
        isCaps := true;
    end;
  end;

  with Keys.Items[Index] do
  begin
    if ((ssShift in Shift) or (IsCaps)) and not (SpecialKey = skShift) then
    begin
      KeyVal := kvShifted;
    end
    else if IsCapsDown then
      KeyVal := kvCapital
    else if ((ssCtrl in Shift) and (ssAlt in Shift)) and not (SpecialKey = skAltGr) then
      KeyVal := kvAtlGr
    else
      KeyVal := kvNormal;

    case KeyVal of
      kvNormal  :
        begin
          KeyCaption := SysUtils.AnsiLowerCase(Caption);
          KVal := KeyValue;
        end;
      kvShifted :
        begin
          if ShiftCaption = '' then
            KeyCaption := Caption
          else
            KeyCaption := ShiftCaption;

          KVal := ShiftKeyValue;
        end;

      kvAtlGr   : KeyCaption := AltGrCaption;
      kvCapital : KeyCaption := Caption
    end;
  end;

  if (KVal <> -1) and (Keys.Items[Index].AutoPost) then
  begin
    isCombo := IsComboBox(KeybdInputHandle);

    if not FPostWMCharOnly and not isCombo then
      SendMessage(KeybdInputHandle, WM_KEYDOWN, KVal,0);

    SendMessage(KeybdInputHandle, WM_CHAR, KVal,0);

    if not FPostWMCharOnly and not isCombo then
      SendMessage(KeybdInputHandle, WM_KEYUP, KVal,0);
  end
  else
  begin
    if (KeyCaption <> '') and (Keys.Items[Index].AutoPost) then
    begin
      isCombo := IsComboBox(KeybdInputHandle);

      for j := 1 to Length(KeyCaption) do
      begin
        if not FPostWMCharOnly and not isCombo then
        begin
          if (Upcase(KeyCaption[j]) >= '0') and
             (Upcase(KeyCaption[J]) <= 'Z') then
             SendMessage(KeybdInputHandle, WM_KEYDOWN, Ord(Upcase(KeyCaption[j])),0);
        end;

        SendMessage(KeybdInputHandle, WM_CHAR, Ord(KeyCaption[j]),0);

        if not FPostWMCharOnly and not isCombo then
        begin
          if (Upcase(KeyCaption[j]) >= '0') and
             (Upcase(KeyCaption[j]) <= 'Z') then
          SendMessage(KeybdInputHandle, WM_KEYUP, Ord(Upcase(KeyCaption[j])),0);
        end;
      end;
    end;
  end;
end;

procedure TAdvTouchKeyboard.PostSpecialKeys(Key: Word; const pShift: TShiftState; SpecialKey: Boolean);
type
  TShiftKeyInfo = record
    Shift: Byte;
    VKey: Byte;
  end;

  ByteSet = set of 0..7;

const
  ShiftKeys: array [1..3] of TShiftKeyInfo =
    ((Shift: Ord(ssCtrl ) ; VKey : VK_CONTROL),
    ( Shift: Ord(ssShift) ; VKey : VK_SHIFT  ),
    ( Shift: Ord(ssAlt  ) ; VKey : VK_MENU   )) ;
var
  Flag: DWORD;
  bShift: ByteSet absolute pShift;
  i,res: Integer;

begin

   if Key = VK_CAPITAL then
     FCapsDown := not FCapsDown;

   if Key = VK_SHIFT then
     FCapsDown := false;

   //pressing shift keys
   for i := 1 to 3 do
   begin
     if ShiftKeys[i].Shift in bShift then
     begin
       keybd_event(ShiftKeys[i].VKey, MapVirtualKey(ShiftKeys[i].VKey, 0), 0, 0) ;
     end;
   end;

   // apply KEYEVENTF_EXTENDEDKEY if specialkey selected
   if SpecialKey then
     Flag := KEYEVENTF_EXTENDEDKEY
   else
     Flag := 0;

   if (Key <> VK_TAB) Then
   begin
     if (Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]) then
     begin
       SendMessage(KeybdInputHandle, WM_KEYDOWN, Key,0);
       SendMessage(KeybdInputHandle, WM_KEYUP, Key,0);
     end
     else
     begin
       // pressing the actual key
       keybd_event(Key, MapvirtualKey(Key, 0), Flag, 0) ;
       Flag := Flag or KEYEVENTF_KEYUP;
       // releasing the actual key
       keybd_event(Key, MapvirtualKey(Key, 0), Flag, 0) ;
     end;
   end
   else
   begin
     res := SendMessage(KeybdInputHandle, WM_GETDLGCODE, VK_TAB, 0);

     if res and DLGC_WANTTAB = DLGC_WANTTAB then
     begin
       SendMessage(KeybdInputHandle, WM_KEYDOWN, VK_TAB,0);
       SendMessage(KeybdInputHandle, WM_CHAR, VK_TAB,0);
       SendMessage(KeybdInputHandle, WM_KEYUP, VK_TAB,0);
     end
     else
     begin
       PostMessage(KeybdInputHandle, WM_KEYDOWN, VK_TAB,0);
     end;
   end;

   // Releasing shift keys
   for i := 3 downto 1 do
   begin
     if ShiftKeys[i].Shift in bShift then
     begin
       keybd_event(ShiftKeys[i].VKey, MapVirtualKey(ShiftKeys[i].VKey, 0), KEYEVENTF_KEYUP, 0) ;
     end;
   end;
end;

procedure TAdvTouchKeyboard.RepeatTimerProc(Sender: TObject);
begin
  Inc(FRepeatTimerCount);
  if (FRepeatTimerCount > 2) then
  begin
    if FRepeatHandle = GetFocus then
      ItemKeyDown(FRepeatItemIndex)
    else
      StopTimer;
  end;
end;

procedure TAdvTouchKeyboard.SaveKeybdLayout(FileName: string);
var
  IniFile : TMemIniFile;
  i       : Integer;
begin
  {$IFDEF DELPHIXE_LVL}
  try
    IniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
  except
    IniFile := TMemIniFile.Create(FileName);
  end;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  IniFile := TMemIniFile.Create(FileName);
  {$ENDIF}

  {$IFDEF DELPHIXE11_LVL}
  IniFile.AutoSave := true;
  {$ENDIF}

  With Keys do
  begin
    IniFile.WriteInteger('Keys','Count', Count);
    try
      for i := 0 to Count - 1 do
        begin
        IniFile.WriteString('Key' + IntToStr(i), 'Caption', Items[i].Caption);
        IniFile.WriteString('Key' + IntToStr(i), 'ShiftCaption', Items[i].ShiftCaption);
        IniFile.WriteString('Key' + IntToStr(i), 'AltGrCaption', Items[i].AltGrCaption);

        IniFile.WriteString('Key' + IntToStr(i), 'KeyString', Items[i].KeyString);

        IniFile.WriteInteger('Key' + IntToStr(i), 'KeyValue', Items[i].KeyValue);
        IniFile.WriteInteger('Key' + IntToStr(i), 'ShiftKeyValue', Items[i].ShiftKeyValue);
        IniFile.WriteInteger('Key' + IntToStr(i), 'AltGrKeyValue', Items[i].AltGrKeyValue);

        IniFile.WriteInteger('Key' + IntToStr(i), 'ImageIndex', Items[i].ImageIndex);
        IniFile.WriteInteger('Key' + IntToStr(i), 'SpecialKey', Integer(Items[i].SpecialKey));
        IniFile.WriteInteger('Key' + IntToStr(i), 'Color', Items[i].Color);
        IniFile.WriteInteger('Key' + IntToStr(i), 'ColorDown', Items[i].ColorDown);
        IniFile.WriteInteger('Key' + IntToStr(i), 'BorderColor', Items[i].BorderColor);
        IniFile.WriteInteger('Key' + IntToStr(i), 'BorderColorDown', Items[i].BorderColorDown);
        IniFile.WriteInteger('Key' + IntToStr(i), 'TextColor', Items[i].TextColor);
        IniFile.WriteInteger('Key' + IntToStr(i), 'TextColorDown', Items[i].TextColorDown);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Width', Items[i].Width);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Height', Items[i].Height);
        IniFile.WriteInteger('Key' + IntToStr(i), 'X', Items[i].X);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Y', Items[i].Y);

        if Items[i].AutoPost then
          IniFile.WriteInteger('Key' + IntToStr(i), 'AutoPost', 1)
        else
          IniFile.WriteInteger('Key' + IntToStr(i), 'AutoPost', 0);
        end;

        {$IFDEF DELPHIXE11_LVL}
        IniFile.UpdateFile;
        {$ENDIF}

    finally
      IniFile.Free;
    end;
  end;
end;

procedure TAdvTouchKeyboard.SetAutoCapsDisplay(const Value: Boolean);
begin
  FAutoCapsDisplay := Value;
  Invalidate;
end;

procedure TAdvTouchKeyboard.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TAdvTouchKeyboard.SetHighlightAltGr(const Value: TColor);
begin
  FHighlightAltGr := Value;
  Invalidate;
end;

procedure TAdvTouchKeyboard.SetHighlightCaps(const Value: TColor);
begin
  FHighlightCaps := Value;
  Invalidate;
end;

procedure TAdvTouchKeyboard.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TAdvTouchKeyboard.SetKeyboardType(const Value: TKeyboardType);
begin
  FKeyboardType := Value;

  if (csLoading in ComponentState) then
  begin
    if (Value <> ktCustom) then
      Keys.Clear;
  end
  else
    Keys.Clear;

  Shift := [];
  case Value of
    ktQWERTY    : BuildQWERTYKeyBoard;
    ktQWERTZ    : BuildQWERTZKeyBoard;
    ktAZERTY    : BuildAZERTYKeyBoard;
    ktDVORAK    : BuildDVORAKKeyBoard;
    ktNUMERIC   : BuildNumericKeyBoard;
    ktCELLPHONE : BuildCellPhoneKeyBoard;
    ktCustom    : ;
  end;
end;

procedure TAdvTouchKeyboard.SetKeys(const Value: TAdvTouchKeyCollection);
begin
  FKeys.Assign(Value);
end;

procedure TAdvTouchKeyboard.SetPictureDownState(const Value: TBitmap);
begin
    FPictureDownState.Assign(Value);
end;

procedure TAdvTouchKeyboard.SetPictureNormalState(const Value: TBitmap);
begin
  FPictureNormalState.Assign(Value);
end;

procedure TAdvTouchKeyboard.SetShift(const Value: TShiftState);
begin
  if FShift <> Value then
  begin
    FShift := Value;
    Invalidate;
  end;
end;

procedure TAdvTouchKeyboard.SetOnlyShowStateCaption(const Value: Boolean);
begin
  if FOnlyShowStateCaption <> Value then
  begin
    FOnlyShowStateCaption := Value;

    Invalidate;
  end;
end;

procedure TAdvTouchKeyboard.SetSmallFont(const Value: TFont);
begin
  FSmallFont.Assign(Value);
end;

procedure TAdvTouchKeyboard.SetVersion(const Value: string);
begin

end;

procedure TAdvTouchKeyboard.SetKeyDistance(Value : Integer);
begin
   if (Value <> FKeyDistance) Then
   begin
     FKeyDistance := Value;
     Invalidate;
   end;
end;

procedure TAdvTouchKeyboard.StartTimer(index: integer);
begin
  FRepeatItemIndex := index;
  FRepeatTimerCount := 0;
  FRepeatTimer.Enabled := true;
  FRepeatHandle := GetFocus;
end;

procedure TAdvTouchKeyboard.StopTimer;
begin
  FRepeatTimer.Enabled := false;
  FRepeatTimerCount := 0;
end;

procedure TAdvTouchKeyboard.WMEraseBkGnd(var Msg: TMessage);
begin
  if FDisableErase then
    Msg.Result := 1
  else
    inherited;
end;

procedure TAdvTouchKeyboard.WMSize(var Message: TWMSize);
begin
  inherited;
end;

procedure TAdvTouchKeyboard.Zoom(fhorz,fvert: double; keysonly : boolean = false; absoluteZoom : boolean = false);
var
  i: integer;
begin
  if (FOldH < 0) Then
  begin
    FOldW := Width;
    FOldH := Height;
  end;

  for i := 0 to Keys.Count - 1 do
  begin
    with Keys[i] do
    begin

      if (OldW < 0) Then
      begin
         OldW := width;
         OldH := height;
         OldX := X;
         OldY := Y;
      end;

      if absoluteZoom Then
      begin
         width := round(width * fhorz);
         height := round(height * fvert);
         x := round(x * fhorz);
         y := round(y  * fvert);
      end
      else 
      begin
         width := round(OldW * fhorz);
         height := round(OldH * fvert);
         x := round(OldX * fhorz);
         y := round(OldY  * fvert);
      end;
    end;
  end;
  if not keysonly then
  begin
    iF absoluteZoom then
    begin
      Width := round(Width * fhorz) + FKeyDistance;
      Height := round(Height * fvert) + FKeyDistance;
    end
    else
    begin
      Width := round(FOldW * fhorz) + FKeyDistance;
      Height := round(FOldH * fvert) + FKeyDistance;
    end;
  end;
  
end;

{ TAdvTouchKeyItem }


procedure TAdvTouchKeyItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvTouchKeyItem) then
  begin
    X := (Source as TAdvTouchKeyItem).X;
    Y := (Source as TAdvTouchKeyItem).Y;
    AutoPost := (Source as TAdvTouchKeyItem).AutoPost;
    Caption := (Source as TAdvTouchKeyItem).Caption;
    ShiftCaption := (Source as TAdvTouchKeyItem).ShiftCaption;
    AltGrCaption := (Source as TAdvTouchKeyItem).AltGrCaption;
    KeyValue := (Source as TAdvTouchKeyItem).Keyvalue;
    ShiftKeyValue := (Source as TAdvTouchKeyItem).ShiftKeyValue;
    AltGrKeyValue := (Source as TAdvTouchKeyItem).AltGrKeyValue;
    PictureDownState.Assign((Source as TAdvTouchKeyItem).PictureDownState);
    PictureNormalState.Assign((Source as TAdvTouchKeyItem).PictureNormalState);
    Height := (Source as TAdvTouchKeyItem).Height;
    Width := (Source as TAdvTouchKeyItem).Width;
    SpecialKey := (Source as TAdvTouchKeyItem).SpecialKey;
    BorderColor := (Source as TAdvTouchKeyItem).BorderColor;
    BorderColorDown := (Source as TAdvTouchKeyItem).BorderColorDown;
    Color := (Source as TAdvTouchKeyItem).Color;
    ColorDown := (Source as TAdvTouchKeyItem).ColorDown;
    TextColor := (Source as TAdvTouchKeyItem).TextColor;
    TextColorDown := (Source as TAdvTouchKeyItem).TextColorDown;
    ImageIndex := (Source as TAdvTouchKeyItem).ImageIndex;
    ShortCut:= (Source as TAdvTouchKeyItem).ShortCut;
  end;
end;

constructor TAdvTouchKeyItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FTouchKey := TAdvTouchKey.Create((Collection as TAdvTouchKeyCollection).FOwner);
  FTouchKey.Parent := (Collection as TAdvTouchKeyCollection).FOwner as TWinControl;
  FTouchKey.TouchKeyItem:= Self;

  FKeyString := '';

  // setting initial values
  FTouchKey.Left := 20;
  FTouchKey.Top := 20;
  FTouchKey.Width := 40;
  FTouchKey.Height := 40;
end;

destructor TAdvTouchKeyItem.Destroy;
begin
  FTouchKey.Free;
  inherited;
end;

function TAdvTouchKeyItem.GetShiftCaption: string;
begin
  Result := FTouchKey.ShiftCaption;
end;

function TAdvTouchKeyItem.GetAltGrCaption: string;
begin
  Result := FTouchKey.AltGrCaption;
end;

function TAdvTouchKeyItem.GetAltGrKeyValue: Integer;
begin
  Result := FTouchKey.AltGrKeyValue;
end;

function TAdvTouchKeyItem.GetAutoPost: boolean;
begin
  Result := FTouchKey.AutoPost;
end;

function TAdvTouchKeyItem.GetBorderColor: TColor;
begin
  Result := FTouchKey.BorderColor;
end;

function TAdvTouchKeyItem.GetBorderColorDown: TColor;
begin
  Result := FTouchKey.BorderColorDown;
end;

function TAdvTouchKeyItem.GetCaption: string;
begin
  Result := FTouchKey.Caption;
end;

function TAdvTouchKeyItem.GetColor: TColor;
begin
  Result := FTouchKey.Color;
end;

function TAdvTouchKeyItem.GetColorDown: TColor;
begin
  Result := FTouchKey.ColorDown;
end;

function TAdvTouchKeyItem.GetDisplayName: string;
begin
  Result := 'Key ' + Caption;
end;

function TAdvTouchKeyItem.GetHeight: Integer;
begin
  Result := FTouchKey.Height;
end;

function TAdvTouchKeyItem.GetHint: string;
begin
  Result := FTouchKey.Hint;
end;

function TAdvTouchKeyItem.GetImageIndex: Integer;
begin
  Result := FTouchKey.ImageIndex;
end;

function TAdvTouchKeyItem.GetKeyValue: Integer;
begin
  Result := FTouchKey.KeyValue;
end;

function TAdvTouchKeyItem.GetPictureDownState: TBitmap;
begin
  Result := FTouchKey.PictureDownState;
end;

function TAdvTouchKeyItem.GetPictureNormalState: TBitmap;
begin
  Result := FTouchKey.PictureNormalState;
end;

function TAdvTouchKeyItem.GetShiftKeyValue: Integer;
begin
  Result := FTouchKey.ShiftKeyValue;
end;

function TAdvTouchKeyItem.GetShortCut: string;
begin
  Result := FTouchKey.ShortCut;
end;

function TAdvTouchKeyItem.GetSpecialKey: TSpecialKey;
begin
  Result := FTouchKey.SpecialKey;
end;

function TAdvTouchKeyItem.GetTextColor: TColor;
begin
  Result := FTouchKey.TextColor;
end;

function TAdvTouchKeyItem.GetTextColorDown: TColor;
begin
  Result := FTouchKey.TextColorDown;
end;

function TAdvTouchKeyItem.GetWidth: Integer;
begin
  Result := FTouchKey.Width;
end;

function TAdvTouchKeyItem.GetX: integer;
begin
  Result := FTouchKey.Left;
end;

function TAdvTouchKeyItem.GetY: integer;
begin
  Result := FTouchKey.Top;
end;

procedure TAdvTouchKeyItem.SetAltGrCaption(const Value: string);
begin
  FTouchKey.AltGrCaption := Value;
end;

procedure TAdvTouchKeyItem.SetAltGrKeyValue(const Value: Integer);
begin
  FTouchKey.AltGrKeyValue := Value;
end;

procedure TAdvTouchKeyItem.SetAutoPost(const Value: boolean);
begin
  FTouchKey.AutoPost := Value;
end;

procedure TAdvTouchKeyItem.SetShiftCaption(const Value: string);
begin
  FTouchKey.ShiftCaption := Value;
end;

procedure TAdvTouchKeyItem.SetBorderColor(const Value: TColor);
begin
  FTouchKey.BorderColor := Value;
end;

procedure TAdvTouchKeyItem.SetBorderColorDown(const Value: TColor);
begin
  FTouchKey.BorderColorDown := Value;
end;

procedure TAdvTouchKeyItem.SetCaption(const Value: string);
begin
  FTouchKey.Caption := Value;
end;

procedure TAdvTouchKeyItem.SetColor(const Value: TColor);
begin
  FTouchKey.Color := Value;
end;

procedure TAdvTouchKeyItem.SetColorDown(const Value: TColor);
begin
  FTouchKey.ColorDown := Value;
end;

procedure TAdvTouchKeyItem.SetHeight(const Value: Integer);
begin
  FTouchKey.Height := Value;
end;

procedure TAdvTouchKeyItem.SetHint(const Value: string);
begin
  FTouchKey.Hint := Value;
  FTouchKey.ShowHint := (Value <> '');
end;

procedure TAdvTouchKeyItem.SetImageIndex(const Value: Integer);
begin
  FTouchKey.ImageIndex := Value;
end;

procedure TAdvTouchKeyItem.SetKeyString(const Value: string);
begin
  FKeyString := Value;
end;

procedure TAdvTouchKeyItem.SetKeyvalue(const Value: Integer);
begin
  FTouchKey.KeyValue := Value;
end;

procedure TAdvTouchKeyItem.SetSpecialKey(const Value: TSpecialKey);
begin
  FTouchKey.SpecialKey := Value;
end;

procedure TAdvTouchKeyItem.SetPictureDownState(const Value: TBitmap);
begin
  FTouchKey.PictureDownState.Assign(Value);
end;

procedure TAdvTouchKeyItem.SetPictureNormalState(const Value: TBitmap);
begin
  FTouchKey.PictureNormalState.Assign(Value);
end;

procedure TAdvTouchKeyItem.SetShiftKeyValue(const Value: Integer);
begin
  FTouchKey.ShiftKeyValue := Value;
end;

procedure TAdvTouchKeyItem.SetShortCut(const Value: string);
begin
  FTouchKey.ShortCut := Value;
end;

procedure TAdvTouchKeyItem.SetTextColor(const Value: TColor);
begin
  FTouchKey.TextColor := Value;
end;

procedure TAdvTouchKeyItem.SetTextColorDown(const Value: TColor);
begin
  FTouchKey.TextColorDown := Value;
end;

procedure TAdvTouchKeyItem.SetWidth(const Value: Integer);
begin
  FTouchKey.Width := Value;
end;

procedure TAdvTouchKeyItem.SetX(const Value: integer);
begin
  FTouchKey.Left := Value;
end;

procedure TAdvTouchKeyItem.SetY(const Value: integer);
begin
  FTouchKey.Top := Value;
end;

function TAdvTouchKeyItem.GetOldH: Integer;
begin
   Result := FTouchKey.OldH;
end;

function TAdvTouchKeyItem.GetOldW: Integer;
begin
   Result := FTouchKey.OldW;
end;

procedure TAdvTouchKeyItem.SetOldH(const Value: Integer);
begin
   FTouchKey.OldH := Value;
end;

procedure TAdvTouchKeyItem.SetOldW(const Value: Integer);
begin
   FTouchKey.OldW := Value;
end;

function TAdvTouchKeyItem.GetOldX: Integer;
begin
   Result := FTouchKey.OldX;
end;

function TAdvTouchKeyItem.GetOldY: Integer;
begin
   Result := FTouchKey.OldY;
end;

procedure TAdvTouchKeyItem.SetOldX(const Value: Integer);
begin
   FTouchKey.OldX := Value;
end;

procedure TAdvTouchKeyItem.SetOldY(const Value: Integer);
begin
   FTouchKey.OldY := Value;
end;
{ TAdvTouchKey }


constructor TAdvTouchKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FAutoPost := true;
  FBmp := TBitmap.Create;
  FPictureNormalState := TBitmap.Create;
  FPictureDownState := TBitmap.Create;
  if not FPictureDownState.Empty then
    FCurrentBitmap := FPictureNormalState
  else
    FCurrentBitmap := TAdvTouchKeyboard(Owner).PictureNormalState;

  FKeyValue := -1;
  FShiftKeyValue := -1;
  FAltGrKeyValue := -1;
  FFont := TAdvTouchKeyboard(Owner).Font;
  FSmallFont := TAdvTouchKeyboard(Owner).SmallFont;

  // defoult colors for none picture keys
  FBorderColor := clGray;
  FBorderColorDown := clBlack;
  FColor := clSilver;
  FColorDown := clGray;
  FTextColor := clBlack;
  FTextColorDown := clBlack;
  FImageIndex := -1;
  // initial counter state
  FDownState := 0;
  FKeyNormalPosition := True;
  FDPIScale := 1;
  // default brush and pen like Mouseup
  // default size of keys
  Height := 40;
  Width := 40;
  FOldH := -1;
  FOldW := -1;
  FOldX := -1;
  FOldY := -1;
end;

destructor TAdvTouchKey.Destroy;
begin
  FPictureNormalState.Free;
  FPictureDownState.Free;
  FBmp.Free;
  inherited;
end;

function TAdvTouchKey.GetDownState: boolean;
begin
  Result := FDownState > 0;
end;

function TAdvTouchKey.GetKeyboard: TAdvTouchKeyboard;
begin
  Result := nil;
  if Assigned(TouchKeyItem) then
  begin
    Result := (TouchKeyItem.Collection as TAdvTouchKeyCollection).Owner as TAdvTouchKeyboard;
  end;
end;

procedure TAdvTouchKey.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FSpecialKey in [skShift, skAlt, skAltGr, skCaps, skCtrl, skNum, skScroll] then
  begin
    FDownState := FDownState + 1;
    
    case FSpecialKey of
      skShift, skCaps : TAdvTouchKeyboard(Owner).Shift :=
           TAdvTouchKeyboard(Owner).Shift + [ssShift];
      skAlt   : TAdvTouchKeyboard(Owner).Shift :=
           TAdvTouchKeyboard(Owner).Shift + [ssAlt];
      skCtrl  : TAdvTouchKeyboard(Owner).Shift :=
           TAdvTouchKeyboard(Owner).Shift + [ssCtrl];
      skAltGr : TAdvTouchKeyboard(Owner).Shift :=
           TAdvTouchKeyboard(Owner).Shift + [ssCtrl, ssAlt];
      end;

    // if FDownState greater then zero then key change to DownState
    if (FDownState > 0) then
      FKeyNormalPosition := False;

    if (TAdvTouchKeyboard(Owner).AutoCapsDisplay) or
       (TAdvTouchKeyboard(Owner).HighlightCaps <> clNone) or
       (TAdvTouchKeyboard(Owner).HighlightAltGr <> clNone) then
    begin
      TAdvTouchKeyboard(Owner).FDisableErase := true;

      if Assigned(TAdvTouchKeyboard(Owner).PictureNormalState) and not
        TAdvTouchKeyboard(Owner).PictureNormalState.Empty  then
        begin
          TAdvTouchKeyboard(Owner).Repaint;
        end;

      TAdvTouchKeyboard(Owner).PaintAllKeys;

      TAdvTouchKeyboard(Owner).FDisableErase := false;
    end;
  end
  else
  begin
    if (FSpecialKey <> skApp) Then
      TAdvTouchKeyboard(Owner).StartTimer(TouchKeyItem.Index);
  end;

  FKeyNormalPosition := False;

  Invalidate;

  inherited MouseDown(Button, Shift, X, Y);

  if (FSpecialKey in [skShift, skCaps, skAlt, skCtrl, skAltGr]) then
    TAdvTouchKeyboard(Owner).ItemKeyDown(TouchKeyItem.Index);
end;

procedure TAdvTouchKey.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  // is this specialkey?
  if FSpecialKey in [skShift, skAlt, skAltGr, skCaps, skCtrl, skNum, skScroll] then
  begin
    if (FDownState > 1) then       // is this its second MouseUp?
    begin
      FDownState := 0;
      case FSpecialKey of
        skShift, skCaps:
            TAdvTouchKeyboard(Owner).Shift :=
                TAdvTouchKeyboard(Owner).Shift - [ssShift];
        skAlt   : TAdvTouchKeyboard(Owner).Shift :=
            TAdvTouchKeyboard(Owner).Shift - [ssAlt];
        skCtrl  : TAdvTouchKeyboard(Owner).Shift :=
            TAdvTouchKeyboard(Owner).Shift - [ssCtrl];
        skAltGr : TAdvTouchKeyboard(Owner).Shift :=
            TAdvTouchKeyboard(Owner).Shift - [ssCtrl, ssAlt];
        end;

      if (TAdvTouchKeyboard(Owner).AutoCapsDisplay) or
         (TAdvTouchKeyboard(Owner).HighlightCaps <> clNone) or
         (TAdvTouchKeyboard(Owner).HighlightAltGr <> clNone) then
      begin
        TAdvTouchKeyboard(Owner).FDisableErase := true;

        if Assigned(TAdvTouchKeyboard(Owner).PictureNormalState) and not
          TAdvTouchKeyboard(Owner).PictureNormalState.Empty  then
          TAdvTouchKeyboard(Owner).Repaint;

        TAdvTouchKeyboard(Owner).PaintAllKeys;

        TAdvTouchKeyboard(Owner).FDisableErase := false;
      end;
    end
    else
      Exit;
  end
  else
  begin
    TAdvTouchKeyboard(Owner).ItemKeyDown(TouchKeyItem.Index);
    TAdvTouchKeyboard(Owner).StopTimer;
  end;

  // if FDownState equal to zero then key can change
  if FDownState = 0 then
    FKeyNormalPosition := True;

  Invalidate;

  if (TouchKeyItem.SpecialKey in [skShift, skAlt, skCtrl, skAltGr]) then
    TAdvTouchKeyboard(Owner).SyncEqualKeys(TouchKeyItem.Index);

  TAdvTouchKeyboard(Owner).ItemKeyUp(TouchKeyItem.Index);
end;

function DarkenColor(Color: TColor): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := ((l AND $FF0000) shr 1) and $FF0000;
  g := ((l AND $FF00) shr 1) and $FF00;
  b := ((l AND $FF) shr 1) and $FF;
  Result := r or g or b;
end;

function LightenColor(Color: TColor): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);

  r := (l AND $FF0000) shr 16;
  g := (l AND $FF00) shr 8;
  b := (l AND $FF);

  r := min($FF, r * 2);
  g := min($FF, g * 2);
  b := min($FF, b * 2);

  r := (r AND $FF) shl 16;
  g := (g AND $FF) shl 8;
  b := (b AND $FF);
  Result := r or g or b;
end;



procedure TAdvTouchKey.Paint;

  function TextWidthMid(AText: String): Integer;
  var
    cvs : TCanvas;
  begin
    cvs := TCanvas.Create;
    Result := cvs.TextWidth(AText);
    cvs.Free;
  end;

var
  xw, yh, VMargin, HMargin: Integer;
  DefaultDraw : boolean;
  c           : TColor;
  TempB,TempC : TBitmap;
  paintshift, paintaltgr: boolean;
  cap: string;
  offset, th: integer;
  curDistance : integer;
  darkcolor, lightcolor: TColor;
  dr: TRect;
  {$IFDEF DELPHIXE10_LVL}
  kbd: TAdvTouchKeyboard;
  frm: TCustomForm;
  {$ENDIF}
  OnlyShowStateCaption: Boolean;
begin
  FDPIScale := 1;
  VMargin := 0;

  {$IFDEF DELPHIXE10_LVL}
  kbd := GetKeyboard;
  if Assigned(kbd) then
  begin
    frm := GetParentForm(kbd);

    if Assigned(frm) {$IFDEF DELPHIXE14_LVL} and (frm.Scaled) {$ENDIF} then
    begin
      {$IFDEF DELPHIXE14_LVL}
      FDPIScale := frm.CurrentPPI / 96;
      {$ELSE}
      FDPIScale := frm.Monitor.PixelsPerInch / 96;
      {$ENDIF}
    end;
  end
  else
  {$ENDIF}

  VMargin := Round(FDPIScale * 5);
  HMargin := Round(FDPIScale * 10);
  offset := 0;
  TempB   := nil;
  curDistance := Round(FDPIScale * TAdvTouchKeyboard(Owner).KeyDistance);

  DefaultDraw := True;

  if not FKeyNormalPosition then
    offset := 1;

  if (SpecialKey = skCaps) then
  begin
    FKeyNormalPosition := not TAdvTouchKeyboard(Owner).IsCapsDown;

    if not FKeyNormalPosition then
      FDownState := 2;
  end;

  Canvas.Brush.Color := (Owner as TAdvTouchKeyboard).Color;
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(ClientRect);

  (Owner as TAdvTouchKeyboard).DrawKey(Owner, FItem, Canvas, FDownState > 1,
       ClientRect, DefaultDraw);

  if not DefaultDraw then
    Exit;

  // Checking for Key state
  if FKeyNormalPosition then
    begin
    // checking if there's a bitmap in key or Keyboard
    if (FPictureDownState.Empty) and
        (TAdvTouchKeyboard(Owner).PictureDownState.Empty) then
      begin
      Canvas.Brush.Style := bsClear;
      Canvas.Brush.Color := FColor;
      Canvas.Pen.Color := FBorderColor;
      end
    else
      begin
      if not FPictureDownState.Empty then
        FCurrentBitmap := FPictureNormalState
      else
        FCurrentBitmap := TAdvTouchKeyboard(Owner).PictureNormalState;
      end;
    end
  else
    begin
    if (FPictureDownState.Empty) and
        (TAdvTouchKeyboard(Owner).PictureDownState.Empty) then
      begin
      Canvas.Brush.Style := bsClear;
      Canvas.Brush.Color := FColorDown;
      Canvas.Pen.Color := FBorderColorDown;
      end
    else
      begin
      if not FPictureDownState.Empty then
        FCurrentBitmap := FPictureDownState
      else
        FCurrentBitmap := TAdvTouchKeyboard(Owner).PictureDownState;
      end;
    end;

  with inherited Canvas do
  begin
    if not FCurrentBitmap.Empty then
    begin
      // drawing current state bitmap
      FBmp.Assign(FCurrentBitmap);
      FBmp.TransparentMode := tmAuto;
      FBmp.Transparent := true;

      // if clientrect bigger than bitmap then stretch

      if (ClientHeight > FCurrentBitmap.Height) then
      begin
        try
          TempB := TBitmap.Create;
          TempB.Height := FCurrentBitmap.Height;
          TempB.Width  := Self.ClientWidth;
          TempB.Canvas.Brush.Color := clFuchsia;
          TempB.Canvas.FillRect(TempB.Canvas.ClipRect);

          if ClientWidth > FCurrentBitmap.Width then
          begin
            TempC := TBitmap.Create;
            TempC.Height := FCurrentBitmap.Height;
            TempC.Width := Self.ClientWidth;
            TempC.Canvas.Brush.Color := clFuchsia;
            TempC.Canvas.FillRect(TempB.Canvas.ClipRect);

            BitmapStretchInWidth(FBmp, TempC.Canvas, ClientRect.Left,
              ClientRect.Top, ClientRect.Right - ClientRect.Left + 1);

            TempB.Canvas.CopyRect(rect(0,0,TempB.Width,TempB.Height - 1),
               TempC.Canvas, rect(0,0,TempB.Width,TempB.Height));
            TempC.Free;

            FBmp.Assign(TempB);

            FBmp.TransparentMode := tmAuto;
            FBmp.Transparent := true;
          end
          else
          begin
            TempB.Canvas.CopyRect(rect(0,0,TempB.Width,TempB.Height - 1),
              FBmp.Canvas, rect(0,0,TempB.Width,TempB.Height));
            FBmp.Assign(TempB);
          end;
        finally
          TempB.Free;
        end;

        FBmp.Transparent := true;
        FBmp.TransparentMode := tmAuto;
        BitmapStretch(FBmp, Self.Canvas, ClientRect.Left,
          ClientRect.Top, ClientRect.Bottom - ClientRect.Top + 1);

      end
      else
      begin
        FBmp.Transparent := true;
        FBmp.TransparentMode := tmAuto;

        if ClientWidth > FCurrentBitmap.Width then
        begin
          BitmapStretchInWidth(FBmp, Self.Canvas, ClientRect.Left,
            ClientRect.Top, ClientRect.Right - ClientRect.Left + 1)
        end
        else
          Draw(0, 0, FBmp);
      end;
    end
    else
    begin
      // drawing current state roundedrectangle
      RoundRect(curDistance, curDistance, ClientWidth, ClientHeight, 10, 10);

      if FKeyNormalPosition then
      begin
        darkcolor := DarkenColor(Canvas.Brush.Color);
        lightcolor := LightenColor(Canvas.Brush.Color);

        Pen.Color := lightcolor;
        MoveTo(curdistance + 1, curDistance + 4);
        LineTo(curdistance + 1, ClientHeight - 4);
        MoveTo(curdistance + 4, curDistance + 1);
        LineTo(ClientWidth - 4, curDistance + 1);

        Pixels[curdistance + 2, curdistance + 2] := lightcolor;
        Pixels[curdistance + 3, curdistance + 2] := lightcolor;
        Pixels[curdistance + 2, curdistance + 3] := lightcolor;

        Pen.Color := darkcolor;
        MoveTo(curdistance + 4, ClientHeight - 2);
        LineTo(ClientWidth - 4, ClientHeight - 2);
        MoveTo(ClientWidth - 2, curDistance + 4);
        LineTo(ClientWidth - 2, ClientHeight - 4);

        Pixels[ClientWidth - 3, ClientHeight - 3] := darkcolor;
        Pixels[ClientWidth - 3, ClientHeight - 4] := darkcolor;
        Pixels[ClientWidth - 4, ClientHeight - 3] := darkcolor;
      end;

    end;

    // drawing transparent text
    // default text pos at center
    xw   := (ClientWidth div 2) + (curDistance DIV 2);
    yh   := (ClientHeight div 2) + (curDistance DIV 2);

    OnlyShowStateCaption := TAdvTouchKeyboard(Owner).OnlyShowStateCaption;

    if (ImageIndex >= 0) and Assigned((Owner as TAdvTouchKeyboard).Images) then
    begin
      (Owner as TAdvTouchKeyboard).Images.GetBitmap(ImageIndex, FBmp);
      c := FBmp.Canvas.Pixels[0, FBmp.Height - 1];
      DrawTransparentBitmap(Canvas.Handle, FBmp.Handle,  xw - (FBmp.Width div 2),
      yh - (FBmp.Height div 2), FBmp.Width, FBmp.Height, 0, 0, c);
    end
    else
    begin
      SetBkMode(Handle, TRANSPARENT);
      // Long caption or ShiftCaption or AltGrCaption = SmallFont else Font
      if ((Length(Caption) > 1) or (Length(ShiftCaption) > 0)
         or (Length(AltGrCaption) > 0)) and not OnlyShowStateCaption then
      begin
        Font.Assign(FSmallFont);
        Font.Height := Round(FDPIScale * FSmallFont.Height);
      end
      else
      begin
        Font.Assign(FFont);
      end;

      if FKeyNormalPosition then
      begin
        Font.Color := TextColor
      end
      else
        Font.Color := TextColorDown;

      // FShiftCaption // FAltGrCaption

      paintshift := (ssShift in TAdvTouchKeyboard(Owner).Shift) or TAdvTouchKeyboard(Owner).IsCapsDown;
      paintaltgr := (ssCtrl in TAdvTouchKeyboard(Owner).Shift) and (ssAlt in TAdvTouchKeyboard(Owner).Shift);

      if not (OnlyShowStateCaption and ((SpecialKey = skNone) or ((SpecialKey = skTab) and (ShiftCaption <> '')))) then
      begin
        if paintaltgr then
          paintshift := false;

        cap := FCaption;

        if TAdvTouchKeyboard(Owner).AutoCapsDisplay and (length(FCaption) = 1) then
        begin
          if paintshift then
            cap := AnsiUpperCase(FCaption)
          else
            cap := AnsiLowerCase(FCaption);
        end;
      end
      else
      begin
        if paintaltgr then
          cap := AltGrCaption
        else if paintshift and (ShiftCaption <> '') then
        begin
          cap := ShiftCaption;
        end
        else if paintshift then
          cap := AnsiUpperCase(FCaption)
        else
          cap := AnsiLowerCase(FCaption);
      end;

      if (FShiftCaption = '') or OnlyShowStateCaption then
      begin
        dr := ClientRect;

        DrawText(inherited Canvas.Handle, PChar(Cap), Length(Cap), dr, DT_WORDBREAK or DT_CALCRECT or DT_CENTER);

        th := (ClientHeight - (dr.Bottom - dr.Top))  div 2;

        dr := ClientRect;

        OffsetRect(dr, 0, th);

        DrawText(inherited Canvas.Handle, PChar(Cap), Length(Cap), dr, DT_WORDBREAK or DT_CENTER or DT_VCENTER);
      end
      else
      begin
        TextOut(HMargin + offset, ClientHeight + offset - VMargin - TextHeight('gh'), cap);

        if paintshift and (TAdvTouchKeyboard(Owner).HighlightCaps <> clNone) then
        begin
          Font.Color := TAdvTouchKeyboard(Owner).HighlightCaps;
        end;

        TextOut(HMargin + offset, VMargin + offset, FShiftCaption);
      end;

      if FKeyNormalPosition then
      begin
        Font.Color := TextColor
      end
      else
        Font.Color := TextColorDown;

      if paintaltgr and (TAdvTouchKeyboard(Owner).HighlightAltGr <> clNone) then
      begin
        Font.Color := TAdvTouchKeyboard(Owner).HighlightAltGr
      end;

      if (Length(AltGrCaption) > 0) and not OnlyShowStateCaption then
        TextOut(ClientWidth + offset - HMargin - TextWidth('Z'), ClientHeight + offset - VMargin - TextHeight('Z'), FAltGrCaption);
    end;
  end;
end;

procedure TAdvTouchKey.SetAltGrCaption(const Value: string);
begin
  FAltGrCaption := Value;
  Invalidate;  
end;

procedure TAdvTouchKey.SetAltrCaption(const Value: string);
begin
  FShiftCaption := Value;
  Invalidate;
end;

procedure TAdvTouchKey.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TAdvTouchKey.SetCaption(const Value: string);
begin
  FCaption := Value;
  Invalidate;
end;

procedure TAdvTouchKey.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TAdvTouchKey.SetDownState(const Value: boolean);
begin
  FDownState := FDownState + 1;
end;

procedure TAdvTouchKey.SetPictureDownState(const Value: TBitmap);
begin
  FPictureDownState.Assign(Value);
end;

procedure TAdvTouchKey.SetPictureNormalState(const Value: TBitmap);
begin
  FPictureNormalState.Assign(Value);
  FCurrentBitmap := FPictureNormalState;
  Invalidate;
end;

procedure TAdvTouchKey.SetTextColor(const Value: TColor);
begin
  FTextColor := Value;
  Invalidate;  
end;

{ TAdvTouchKeyCollection }

function TAdvTouchKeyCollection.Add: TAdvTouchKeyItem;
begin
  Result := TAdvTouchKeyItem(inherited Add);
end;

constructor TAdvTouchKeyCollection.Create(AOwner: TComponent);
begin
  inherited Create(TAdvTouchKeyItem);
  FOwner := AOwner;
end;

function TAdvTouchKeyCollection.GetItem(Index: Integer): TAdvTouchKeyItem;
begin
  Result := TAdvTouchKeyItem(inherited Items[Index]);
end;

function TAdvTouchKeyCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvTouchKeyCollection.Insert(index: integer): TAdvTouchKeyItem;
begin
  Result := TAdvTouchKeyItem(inherited Insert(Index));
end;

procedure TAdvTouchKeyCollection.SetItem(Index: Integer;
  const Value: TAdvTouchKeyItem);
begin
  inherited Items[index] := Value;
end;

procedure TKeyboardToolForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    WndParent := (Owner as TCustomForm).Handle;
    Style := WS_POPUP;

    if ShowClose then
      Style := Style or WS_SYSMENU;
    if ShowCaption then
      Style := Style or WS_CAPTION;

    ExStyle := WS_EX_PALETTEWINDOW;
  end;
end;

procedure TKeyboardToolForm.WMActivate(var Message: TMessage);
begin
  inherited;
  message.Result := 1;
end;

procedure TKeyboardToolForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ TAdvPopupTouchKeyBoard }

constructor TAdvPopupTouchKeyBoard.Create(AOwner: TComponent);
begin
  inherited;
  FFrm := nil;
  FKbd := nil;
  FOwnerForm := nil;

  if (AOwner is TWinControl) then
    FOwnerform := GetParentForm(AOwner as TWinControl);

  FTimer := TTimer.Create(self);
  FTimer.OnTimer := TimerProc;
  FTimer.Interval := 250;
  FTimer.Enabled := false;

  FAutoPostKey := true;
  FAutoCapsDisplay := false;
  FHighlightCaps := clNone;
  FHighlightAltGr := clNone;
  FKeyboardType := ktQWERTY;
  FShowClose := true;
  FShowCaption := true;
  FDisableSizing := false;
  FLastX := -1;
  FLastY := -1;
  FOldWnd := 0;
end;

destructor TAdvPopupTouchKeyBoard.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TAdvPopupTouchKeyBoard.DoFocusControlChange(AControl: TWinControl; var ShowKeyboard: boolean);
begin
  if Assigned(OnFocusControlChange) then
    OnFocusControlChange(Self, AControl, ShowKeyboard);
end;

procedure TAdvPopupTouchKeyBoard.Hide;
begin
  FLastX := -1;
  FLastY := -1;
  FTimer.Enabled := false;
  if Assigned(FFrm) then
    FFrm.Free;
  FFrm := nil;
  FKbd := nil;
end;

procedure TAdvPopupTouchKeyBoard.SetAutoCapsDisplay(const Value: Boolean);
begin
  FAutoCapsDisplay := Value;
  if Assigned(FKbd) then
    FKbd.AutoCapsDisplay := Value;
end;

procedure TAdvPopupTouchKeyBoard.SetAutoPostKey(const Value: Boolean);
begin
  FAutoPostKey := Value;
  if Assigned(FKbd) then
    FKbd.AutoPostKey := Value;
end;

procedure TAdvPopupTouchKeyBoard.SetHighlightAltGr(const Value: TColor);
begin
  FHighlightAltGr := Value;
  if Assigned(FKbd) then
    FKbd.HighlightAltGr := Value;
end;

procedure TAdvPopupTouchKeyBoard.SetHighlightCaps(const Value: TColor);
begin
  FHighlightCaps := Value;
  if Assigned(FKbd) then
    FKbd.HighlightCaps := Value;
end;

procedure TAdvPopupTouchKeyBoard.SetKeyboardType(const Value: TKeyboardType);
begin
  FKeyboardType := Value;
  if Assigned(FKbd) then
    FKbd.KeyboardType := Value;
end;

procedure TAdvPopupTouchKeyBoard.CreateForm;
begin
  FFrm := TKeyboardToolForm.CreateNew(FOwnerForm);

  FFrm.FPrevPPI := 96;
  FFrm.FFirst := True;
  FFrm.ShowCaption := FShowCaption;
  FFrm.ShowClose := FShowClose;
  FFrm.OnCloseQuery := FormCloseQuery;
  FFrm.BorderStyle := bsToolWindow;
  FFrm.Scaled := true;

  FKbd := TAdvTouchKeyboard.Create(FFrm);
  FKbd.Parent := FFrm;
  FKbd.AutoPostKey := FAutoPostKey;
  FKbd.AutoCapsDisplay := FAutoCapsDisplay;
  FKbd.HighlightCaps := FHighlightCaps;
  FKbd.HighlightAltGr := FHighlightAltGr;
  FKbd.KeyboardType := FKeyboardType;

  FFrm.ScaleBy(FFrm.PixelsPerInch, FFrm.FPrevPPI);
  FFrm.FPrevPPI := FFrm.PixelsPerInch;

  if Assigned(OnKeyboardCreated) then
    OnKeyboardCreated(Self);

  if ShowCaption then
  begin
    FFrm.Width := (FKbd.Width + Round(FKbd.FDPIScale * (GetSystemMetrics(SM_CXDLGFRAME) * 2))) + FKbd.KeyDistance;
    FFrm.Height := (FKbd.Height + Round(FKbd.FDPIScale * (GetSystemMetrics(SM_CYDLGFRAME) * 2 + GetSystemMetrics(SM_CYSMCAPTION)))) + FKbd.KeyDistance;
  end
  else
  begin
    FFrm.Width := FKbd.Width;
    FFrm.Height := FKbd.Height;
  end;

  FFrm.FHeight := FFrm.Height;
end;

procedure TAdvPopupTouchKeyBoard.Show;
var
  wnd: THandle;
begin
  wnd := GetActiveWindow;
  if not Assigned(FFrm) then
    CreateForm;

  FFrm.Visible := true;
  FTimer.Enabled := true;
  SetActiveWindow(wnd);

  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TAdvPopupTouchKeyBoard.ShowAtXY(AParent: TCustomForm; x, y: integer);
begin
  FOwnerForm := AParent;
  ShowAtXY(x,y);
end;

procedure TAdvPopupTouchKeyBoard.ShowAtXY(x, y: integer);
var
  wnd: THandle;
  w,h: integer;
begin
  if (FLastX = x) and (FLastY = y) then
    Exit;

  FLastX := x;
  FLastY := y;

  wnd := GetActiveWindow;
  if not Assigned(FFrm) then
  begin
    CreateForm;
  end;

  w := FFrm.Width;
  h := FFrm.Height;

  FDisableSizing := true;

  FFrm.Width := 0;
  FFrm.Height  := 0;
  FFrm.Left := X;
  FFrm.Top := Y;

  FFrm.Visible := true;

  FDisableSizing := false;

  if Assigned(OnKeyboardPosition) then
    OnKeyboardPosition(Self, Keyboard, x, y);

  MoveWindow(FFrm.Handle, X,Y, W, H, true);

  FTimer.Enabled := true;
  SetActiveWindow(wnd);
end;


procedure TAdvPopupTouchKeyBoard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   FTimer.Enabled := False;
   if Assigned(FOnClose) then
     FOnClose(self);
end;

function TAdvPopupTouchKeyBoard.GetKeyboardRect: TRect;
begin
  Result := Rect(-1,-1,-1,-1);

  if Assigned(FFrm) then
    Result := Rect(FFrm.Left, FFrm.Top, FFrm.Left + FFrm.Width, FFrm.Top + FFrm.Height);
end;

{$IFDEF DELPHIXE10_LVL}
procedure TKeyboardToolForm.WMDpiChanged(var Message: TWMDpi);
begin
  inherited;

  if FFirst then
  begin
    FFirst := False;
    ScaleBy(Message.YDpi, FPrevPPI);

    Height := Round(FHeight * Message.YDpi / FPrevPPI);
    FPrevPPI := Message.YDpi;
  end;

end;
{$ENDIF}

procedure TAdvPopupTouchKeyBoard.TimerProc(Sender: TObject);
var
  wnd, awnd: THandle;
  r, wa: TRect;
  Selection: TSelection;
  pt: TPoint;
  isEdit, isCombo: boolean;
  mon: TMonitor;
  showkeyboard: boolean;
begin
  awnd := GetActiveWindow;
  if awnd = FFrm.Handle then
    Exit;

  if FAutoFollowFocus then
  begin
    isCombo := false;

    wnd := Windows.GetFocus;

    ShowKeyboard := true;

    if FOldWnd <> Wnd then
    begin
      DoFocusControlChange(Screen.ActiveControl, ShowKeyboard);
      FOldShow := ShowKeyboard;
    end
    else
      ShowKeyboard := FOldShow;

    FOldWnd := Wnd;

    Selection.StartPos := -1;
    Selection.EndPos := -1;
    SendMessage(wnd, EM_GETSEL, WParam(@Selection.StartPos), LParam(@Selection.EndPos));

    // tests for edit, memo, spin, rich edit & descending components
    isEdit := Selection.StartPos <> -1;

    if not ShowKeyboard then
    begin
      isEdit := false;
      isCombo := false;
    end;

    // tests for combo, datepicker
    if not isEdit then
    begin
      isCombo := SendMessage(wnd,CB_GETTOPINDEX,0,0) > 0;
      //isCombo := SendMessage(wnd,CB_GETCOUNT,0,0) > 0;
    end;

    if isEdit or isCombo then
    begin
      GetWindowRect(wnd,r);

      Mon := Screen.MonitorFromPoint(Point(r.Left, r.Top));

      wa := Mon.WorkareaRect;

      // default X,Y pos
      pt.X := r.Left + 50;
      pt.Y := r.Bottom;

      if r.Left + 50 + FFrm.Width > wa.Right then
         pt.X := r.Right - FFrm.Width;

      if r.Bottom + FFrm.Height > wa.Bottom then
         pt.Y := r.Top - FFrm.Height;

      if pt.X < 0 then
        pt.X := 0;

      if pt.Y < 0 then
        pt.Y := 0;

      ShowAtXY(Mon.Left + pt.X, Mon.Top + pt.Y);
      //FFrm.Width := FKbd.Width;
    end
    else
    begin
      if FAutoHide then
      begin
        //FFrm.Width := 0;
        FFrm.Visible := false;
        FLastX := -1;
        FLastY := -1;
      end;
    end;
  end;
end;

end.

