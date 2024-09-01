{**********************************************************************}
{ TAdvComboBox component                                               }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by                                                           }
{  TMS Software                                                        }
{  copyright © 1996 - 2022                                             }
{  Email : info@tmssoftware.com                                        }
{  Web : https://www.tmssoftware.com                                   }
{                                                                      }
{ The source code is given as is. The author is not responsible        }
{ for any possible damage done due to the use of this code.            }
{ The component can be freely used in any application. The source      }
{ code remains property of the author and may not be distributed       }
{ freely as such.                                                      }
{**********************************************************************}

unit AdvCombo;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Forms, Controls, Graphics, StdCtrls, CommCtrl,
  SysUtils, ACXPVS, AdvStyleIF, AdvToolTip
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes, VCL.Themes
  {$ENDIF};

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 6; // Build nr.

  // version history
  // 1.1.0.1 : Fixed issue with changing visibility at runtime
  // 1.2.0.0 : New FocusColor, FocusBorderColor properties added
  //         : Improved DFM property storage
  // 1.2.1.0 : Exposed ComboLabel public property
  // 1.2.2.0 : Improved : behaviour with ParentFont = true for LabelFont
  // 1.2.3.0 : New : property DisabledBorder added
  // 1.2.4.0 : New : method SelectItem added
  // 1.2.4.1 : Fixed : issue with label margin
  // 1.2.5.0 : New : exposed Align property
  // 1.2.5.1 : Fixed : possible issue with label positioning for large label font
  // 1.2.5.2 : Improved : painting of dropdown button in flat mode
  // 1.2.5.3 : Fixed : focus border paint issue on Windows Vista
  // 1.2.6.0 : New : FocusLabel property added
  // 1.2.6.1 : Improved : painting in flat mode
  // 1.3.0.0 : New : label RightTop, RightCenter, RightBottom positions added
  // 1.3.0.1 : Fixed : issue with LabelFont and Form ScaleBy
  // 1.3.1.0 : New : Exposed CharCase property
  // 1.3.2.0 : New : Events OnMouseEnter / OnMouseLeave exposed
  // 1.3.2.1 : Fixed : Issue with ParentFont persistence
  // 1.3.2.2 : Fixed : Issue with painting in non themed apps
  // 1.3.2.3 : Fixed : Issue with label positioning with accelerators
  // 1.4.0.0 : New : BorderColor property added
  // 1.4.1.0 : Improved : OnChange event triggered when SelectItem is called
  // 1.4.2.0 : New : Property FocusFontColor added
  // 1.4.3.0 : Improved : Label font handling with ChangeScale()
  // 1.4.3.1 : Fixed : Issue with label position & reparenting
  // 1.5.0.0 : New : EmptyText, EmptyTextFocused, EmptyTextStyle added
  // 1.5.0.1 : Fixed : Issue with EmptyText and BackgroundColor <> clWindow
  // 1.5.1.0 : New : Exposed : OnCloseUp event
  // 1.5.1.1 : Fixed : Issue with alignment & use of labels
  // 1.6.0.0 : New : Property ReturnIsTab added
  // 1.6.0.1 : Improved : Behavior of EmptyText / EmptyTextWhenFocused
  // 1.6.1.0 : Improved : Tab key handling when dropdown is visible
  // 1.6.1.1 : Fixed : Issue with setting LabelFont color at design time
  // 1.6.1.2 : Fixed : Rare issue with use via RDP & Windows XP
  // 1.6.1.3 : Fixed : Rare issue with TAB when style = csSimple
  // 1.6.1.4 : Fixed : Rare issue with ReturnIsTab and starting form with TAdvComboBox focused
  // 1.6.2.0 : Improved : HighDPI support
  // 1.6.2.1 : Fixed : Case issue with TAdvComboBox.BiDiMode
  // 1.6.3.0 : New : StyleElements exposed
  // 1.7.0.0 : New : OnMouseDown, OnMouseUp, OnMouseMove events added
  // 1.8.0.0 : New : lpTopRight, lpBottomRight label positions added
  // 1.8.0.1 : Improved : Behavior when Form.Scaled = false
  // 1.8.1.0 : New : Property EnableWheel added
  // 1.8.2.0 : New : Support for multimonitor High DPI handling
  // 1.9.0.0 : New : OnLabelClick, OnLabelDblClick events added
  // 1.9.0.1 : Fixed : Issue with ReturnIsTab and pressing Enter/Tab
  // 1.9.0.2 : Fixed : Issue with runtime parent changing
  // 1.9.0.3 : Fixed : Regression with label caption setting
  // 1.9.1.0 : Improved : Label positioning in high DPI
  // 1.9.2.0 : Improved : High DPI behavior
  // 1.9.2.1 : Fixed : Label position update for runtime setting
  // 1.9.2.2 : Fixed : Dropdown size for High DPI
  // 1.9.3.0 : Improved : VCL Styles support
  // 1.9.4.0 : New: Per monitor support for high DPI
  // 1.9.5.0 : Improved : Office black style handling
  // 1.9.5.1 : Improved : Label uses StyleElements
  // 1.9.5.2 : Fixed : Issue with label font size handling in high DPI scenarios
  // 1.9.5.3 : Fixed : Regression with label font in specific scenario
  // 1.9.6.0 : New : AutoDropWidth property added
  // 1.9.7.0 : New : UIStyle property for office look
  //         : Improved : On creation check for enabled AdvFormStyler
  // 1.9.7.1 : Fixed : Issue with styler & font color setting
  // 1.9.7.2 : Improved : Label positioning when ParentFont = true
  // 1.9.7.3 : Fixed : Color consistency for edit components and forcing color in dropdown
  // 1.9.7.4 : Fixed : Issue with FocusFont = true
  // 1.9.7.5 : Fixed : Issue with runtime label font initialization
  // 1.9.7.6 : Fixed : VCL styles check updated to work in 10.4 Sydney
  // 1.9.7.7 : Fixed : Issue with combination of custom color and custom focus color
  // 1.9.7.8 : Fixed : Issue with AutoDropWidth = true and scrollbar in dropdown
  // 2.0.0.0 : New : Balloon functionality added
  // 2.0.0.1 : Fixed : DPI calculation when parent form is not Scaled
  // 2.0.0.2 : Fixed : Rare issue with anchored combobox and doublebuffered parent
  // 2.0.0.3 : Improved : Label font size initialization at design-time for high DPI
  // 2.0.0.4 : Fixed : LabelFont color initialization for Office 2019 white mode
  // 2.0.0.5 : Improved : LabelFont handling in connection with ParentFont
  // 2.0.0.6 : Fixed : ParentFont property handling


type
  TWinCtrl = class(TWinControl);

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
                    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter,
                    lpBottomCenter, lpRightTop, lpRightCenter, lpRighBottom,
                    lpTopRight, lpBottomRight);

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


  TAdvCustomCombo = class(TCustomComboBox, ITMSStyle)
  private
    FAutoFocus: boolean;
    FAutoDropWidth: boolean;
    FFlat: Boolean;
    FEtched: Boolean;
    FOldColor: TColor;
    FLoadedColor: TColor;
    FOldParentColor: Boolean;
    FButtonWidth: Integer;
    FFocusBorder: Boolean;
    FMouseInControl: Boolean;
    FDropWidth: integer;
    FIsWinXP: Boolean;
    FIsThemed: Boolean;
    FButtonHover: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FLabelTransparent: Boolean;
    FLabelMargin: Integer;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabel: TLabel;
    FFocusLabel: boolean;
    FFlatLineColor: TColor;
    FFlatParentColor: Boolean;
    FOnDropUp: TNotifyEvent;
    FFocusColor: TColor;
    FFocusBorderColor: TColor;
    FBorderColor: TColor;
    FDisabledBorder: boolean;
    FNormalColor: TColor;
    FHasFocus: Boolean;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    FFocusFontColor: TColor;
    FParentFnt: boolean;
    FFontColor: TColor;
    FWinProc: LInteger;
    FEmptyTextStyle: TFontStyles;
    FEmptyText: string;
    FEmptyTextFocused: boolean;
    FReturnIsTab: boolean;
    FGotReturn: boolean;
    FLblUpdate: boolean;
    FLblFntHeight: integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FEnableWheel: boolean;
    FOnLabelDblClick: TNotifyEvent;
    FOnLabelClick: TNotifyEvent;
    FDPIScale: single;
    FLoaded: boolean;
    FTMSStyle: TTMSStyle;
    FToolTip: TAdvToolTip;
    FToolTipWindow: TAdvToolTipWindow;
    FDesignCreate: boolean;
    FLabelFontEqual: boolean;
    procedure SetEtched(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetButtonWidth(const Value: Integer);
    procedure DrawButtonBorder(DC:HDC);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    {$IFDEF DELPHIXE_LVL}
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$ENDIF}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CNCommand (var Message: TWMCommand); message CN_COMMAND;
    procedure SetDropWidth(const Value: integer);
    function GetLabelCaption: string;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelMargin(const Value: Integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: Boolean);
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure LabelFontChange(Sender: TObject);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetFlatParentColor(const Value: Boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    function GetVersionEx: string;
    procedure SetVersion(const Value: string);
    function GetVisibleEx: boolean;
    procedure SetVisibleEx(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetEmptyText(const Value: string);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
    procedure SetUIStyle(const Value: TTMSStyle);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure AdjustDropDown; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    property WinProc: LInteger read FWinProc write FWinProc;
    function GetVersionNr: Integer; virtual;
    function DoVisualStyles: Boolean;
    function CreateLabel: TLabel;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 19;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor default clBlack;
    property FlatParentColor: Boolean read FFlatParentColor write SetFlatParentColor default True;
    property Etched: Boolean read FEtched write SetEtched default false;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder default false;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property AutoDropWidth: Boolean read FAutoDropWidth write FAutoDropWidth default false;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default false;
    property DropWidth: integer read FDropWidth write SetDropWidth;
    property ReturnIsTab: boolean read FReturnIsTab write FReturnIsTab default false;
    {$IFDEF DELPHIXE_LVL}
    procedure EditWndProc(var Message: TMessage); override;
    {$ENDIF}
    procedure DoExit; override;
    {$IFDEF DELPHI_UNICODE}
    {$IFNDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    {$ENDIF}
    procedure LabelClick(Sender: TObject); virtual;
    procedure LabelDblClick(Sender: TObject); virtual;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Init;
    {$IFDEF DELPHI2007_LVL}
    procedure ShowBalloon(const ATitle: string; const AText: string; AType: TBalloonType);
    procedure HideBalloon;
    {$ENDIF}
    procedure ShowValidation(const AText: string);
    procedure HideValidation;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ComboLabel: TLabel read FLabel;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;

    property EmptyText: string read FEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write FEmptyTextFocused default false;
    property EmptyTextStyle: TFontStyles read FEmptyTextStyle write SetEmptyTextStyle;
    property EnableWheel: boolean read FEnableWheel write FEnableWheel default true;

    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clNone;
    property DisabledBorder: Boolean read FDisabledBorder write FDisabledBorder default true;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    procedure SelectItem(AString: string); virtual;
    property UIStyle: TTMSStyle read FTMSStyle write SetUIStyle default tsCustom;
  published
    property AutoComplete;
    property Color: TColor read GetColorEx write SetColorEx;
    property ToolTip: TAdvToolTip read FToolTip write FToolTip;
    property Version: string read GetVersionEx write SetVersion;
    property Visible: boolean read GetVisibleEx write SetVisibleEx;
    property OnDropUp: TNotifyEvent read FOnDropUp write FOnDropUp;
    property OnLabelClick: TNotifyEvent read FOnLabelClick write FOnLabelClick;
    property OnLabelDblClick: TNotifyEvent read FOnLabelDblClick write FOnLabelDblClick;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvComboBox = class(TAdvCustomCombo)
  published
    property Align;
    property Anchors;
    {$IFDEF DELPHIXE_LVL}
    property AutoCloseUp;
    property AutoDropDown;
    {$ENDIF}
    property AutoDropWidth;
    property AutoFocus;
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BevelEdges;
    property BiDiMode;
    property BorderColor;
    property ButtonWidth;
    property Constraints;
    property DisabledBorder;
    property DragKind;
    property Style;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property EmptyText;
    property EmptyTextFocused;
    property EmptyTextStyle;
    property Etched;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property FocusFontColor;
    property FocusLabel;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property DropWidth;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemIndex;
    property ItemHeight;
    property Items;
    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReturnIsTab;
    property ShowHint;
    property Sorted;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnDropUp;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLabelClick;
    property OnLabelDblClick;
    property OnMeasureItem;
    {$IFDEF DELPHIXE_LVL}
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    {$ENDIF}
    {$IFDEF DELPHI2007_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDrag;
    property OnSelect;
    property OnEndDock;
    property OnStartDock;
    property UIStyle;
  end;


implementation

var
  wpOrigEditProc: Integer;

function CalculateDPIScale(AControl: TControl): single;
var
  FHDC: HDC;
  FDPI: integer;
  {$IFDEF DELPHI10_LVL}
  frm: TCustomForm;
  {$ENDIF}

  function FontHeightAtDpi(iDPI, iFontSize: Integer): Integer;
  var
    FTmpCanvas: TCanvas;
    FHDC: HDC;
  begin
    FTmpCanvas := TCanvas.Create;
    try
      FHDC := GetDC(0);
      FTmpCanvas.Handle := FHDC;
      FTmpCanvas.Font.PixelsPerInch := iDPI; //must be set BEFORE size
      FTmpCanvas.Font.Size := iFontSize;
      Result := FTmpCanvas.TextHeight('0');
      ReleaseDC(0,FHDC)
    finally
      FTmpCanvas.Free;
    end;
  end;

begin
  Result := 1.0;

  {$IFDEF DELPHI10_LVL}
  frm := GetParentForm(AControl);
  if Assigned(frm) and (frm is TForm) then
  begin
    if (frm as TForm).Scaled then
      FDPI := TForm(frm).Monitor.PixelsPerInch
    else
      FDPI := 96;
  end
  else
  {$ENDIF}
  begin
    FHDC := GetDC(0);
    try
      FDPI := GetDeviceCaps(FHDC, LOGPIXELSX);
    finally
      ReleaseDC(0, FHDC);
    end;
  end;

  try
    if FDPI <> 96 then
      Result := FontHeightAtDpi(FDPI, 9) / FontHeightAtDpi(96, 9);
  finally
  end;
end;



function EditSubclassProc(hwnd: THandle; uMsg: Integer; wParam: word; lparam: longint): Integer; stdcall;
begin
  Result := CallWindowProc(tfnwndproc(wpOrigEditProc), hwnd, uMsg, wParam, lParam);
end;

{ TAdvCustomCombo }
constructor TAdvCustomCombo.Create(AOwner: TComponent);
var
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;
  i: Integer;
  FDesignTime: boolean;

begin
  inherited;

  FLabelFontEqual := true;
  FLoaded := false;
  FDPIScale := 1;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FFlat := False;
  FMouseInControl := False;
  FDisabledBorder := True;
  FReturnIsTab := False;
  FGotReturn := false;

  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));

  FIsWinXP := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  FEnableWheel := true;
  FParentFnt := false;
  FButtonHover := False;
  FLabel := nil;
  FLabelFont := TFont.Create;
  FLabelFont.Assign(Font);
  FLabelFont.OnChange := LabelFontChange;
  FLabelMargin := 4;
  FFlatLineColor := clBlack;
  FFlatParentColor := True;
  FLoadedColor := clWindow;
  FFocusColor := clNone;
  FFocusBorderColor := clNone;
  FBorderColor := clNone;

  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;
  FFocusFontColor := clNone;
  FLblUpdate := false;

  FTMSStyle := tsCustom;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  FDesignCreate := FDesignTime;

  if FDesignTime then
    SetComponentStyle(GetDefaultStyle(AOwner, tsCustom));
end;

procedure TAdvCustomCombo.SetButtonWidth(const Value: integer);
begin
  if (value<14) or (value>32) then
    Exit;

  FButtonWidth := Value;
  Invalidate;
end;

procedure TAdvCustomCombo.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Ctl3D := not Value;
    if FFlatParentColor and FFlat then
    begin
      if (Parent is TWinControl) then
        inherited Color := (Parent as TWinControl).Brush.Color;
    end
    else
      inherited Color := FLoadedColor;

    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := true;
    DrawBorders;
    if (FFocusColor <> clNone) then
      inherited Color := FFocusColor;

    if FFocusLabel and (FLabel <> nil) then
    begin
      FLabel.Font.Style := FLabel.Font.Style + [fsBold];
      UpdateLabelPos;
    end;
  end;
end;

procedure TAdvCustomCombo.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := false;
    DrawBorders;
    if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
      Color := FNormalColor;

    if FFocusLabel and (FLabel <> nil) then
    begin
      FLabel.Font.Style := FLabel.Font.Style - [fsBold];
      UpdateLabelPos;
    end;

//    if not FIsThemed then
//    begin
//      Width := Width + 1;
//      Width := Width - 1;
//    end;
  end;
end;

procedure TAdvCustomCombo.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) and FLabelFontEqual then
  begin
    FLabelFont.Assign(Font);
    if Assigned(FLabel) then
    begin
      FLabel.Font.Assign(Font);
      UpdateLabel;
    end;
  end;

  inherited;
end;

procedure TAdvCustomCombo.CMMouseEnter(var Message: TMessage);
var
  pf: TCustomForm;
begin
  inherited;

  if not FMouseInControl and Enabled then
    begin
     FMouseInControl := True;
     DrawBorders;
    end;

  pf := GetParentForm(self);

  if FAutoFocus and not (csDesigning in ComponentState) then
  begin
    if Assigned(pf) then
    begin
      if (GetActiveWindow = pf.Handle) then
        SetFocus;
    end
    else
      SetFocus;
  end;

  if FIsWinXP then
    Invalidate;
end;

procedure TAdvCustomCombo.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if FMouseInControl and Enabled then
    begin
     FMouseInControl := False;
     DrawBorders;
    end;

  FButtonHover := False;

  if FIsWinXP then
    Invalidate;
end;

procedure TAdvCustomCombo.AdjustDropDown;
var
  i,tw,mtw,sw: integer;
begin
  inherited;

  if AutoDropWidth then
  begin
    mtw := 0;
    sw := 0;

    if Items.Count > DropDownCount then
      sw := GetSystemMetrics(SM_CXVSCROLL);

    Canvas.Font.Assign(Font);
    for i := 0 to Items.Count - 1 do
    begin
      tw := sw + Canvas.TextWidth(Items[i]) + 4 * 2;
      if tw > mtw then
        mtw := tw;
    end;

    if mtw > Width then
      SendMessage(Handle, CB_SETDROPPEDWIDTH, mtw, 0);
  end;
end;

procedure TAdvCustomCombo.CMEnabledChanged(var Msg: TMessage);
begin

  if FFlat then
  begin
    if not (csLoading in ComponentState) then
    begin
      if Enabled then
      begin
        inherited ParentColor := FOldParentColor;
        inherited Color := FOldColor;
      end
      else
      begin
        FOldParentColor := inherited Parentcolor;
        FOldColor := inherited Color;
        inherited ParentColor := True;
      end;
    end;
  end;

  inherited;
end;

procedure TAdvCustomCombo.WMNCPaint(var Message: TMessage);
begin
  inherited;

  if FFlat or (not Enabled and DoVisualStyles and not FDisabledBorder) then
    DrawBorders;

  if ((FFocusBorderColor <> clNone) and (GetFocus = Handle)) or (FBorderColor <> clNone) then
    DrawBorders;
end;

function IsMouseButtonDown: Boolean;
// Returns a "True" if a Mouse button happens to be down.
begin
  {Note: Key state is read twice because the first time you read it,
   you learn if the bittpm has been pressed ever.
   The second time you read it you learn if
   the button is currently pressed.}
  Result := not(((GetAsyncKeyState(VK_RBUTTON)and $8000)=0) and
     ((GetAsyncKeyState(VK_LBUTTON)and $8000)=0));
end;


procedure TAdvCustomCombo.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;

  procedure DrawButton;
  var
    ARect: TRect;
    htheme: THandle;
  begin
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Inc(ARect.Left, ClientWidth - FButtonWidth);
//    InflateRect(ARect, -1, -1);

    if FButtonColor <> clNone then
    begin
      Canvas.Brush.Color := FButtonColor;

      if FButtonHover then
        Canvas.Brush.Color := FButtonColorHot;

      if IsMouseButtonDown and FButtonHover then
        Canvas.Brush.Color := FButtonColorDown;

      Canvas.Pen.Color := Canvas.Brush.Color;

      ARect.Left := ARect.Left - 1;
      ARect.Top := ARect.Top + 1;
      ARect.Bottom := ARect.Bottom - 1;
      ARect.Right := ARect.Right - 1;
      Canvas.FillRect(ARect);

      Canvas.Pen.Color := FButtonBorderColor;
      Canvas.MoveTo(ARect.Left, ARect.Top);
      Canvas.LineTo(ARect.Left, ARect.Bottom);

      Canvas.Brush.Color := FButtonTextColor;

      if IsMouseButtonDown then
       Canvas.Brush.Color := FButtonTextColorDown;

      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Pen.Width := Trunc(FDPIScale);

      if (Round(FDPIScale) mod 2 = 0) then
      begin
        Canvas.MoveTo(ARect.Left + (ARect.Right - ARect.Left) div 2 - Round(FDPIScale * 4), ARect.Top + (ARect.Bottom - ARect.Top) div 2 - Round(FDPIScale * 2));
        Canvas.LineTo(ARect.Left + (ARect.Right - ARect.Left) div 2, ARect.Top + (ARect.Bottom - ARect.Top) div 2 + Round(FDPIScale * 1) + 1);
        Canvas.LineTo(ARect.Left + (ARect.Right - ARect.Left) div 2 + Round(FDPIScale * 4) + 1, ARect.Top + (ARect.Bottom - ARect.Top) div 2 - Round(FDPIScale * 2) - 1);
      end
      else
      begin
        Canvas.MoveTo(ARect.Left + (ARect.Right - ARect.Left) div 2 - Round(FDPIScale * 3), ARect.Top + (ARect.Bottom - ARect.Top) div 2 - Round(FDPIScale * 1));
        Canvas.LineTo(ARect.Left + (ARect.Right - ARect.Left) div 2, ARect.Top + (ARect.Bottom - ARect.Top) div 2 + Round(FDPIScale * 2));
        Canvas.LineTo(ARect.Left + (ARect.Right - ARect.Left) div 2 + Round(FDPIScale * 3) + 1, ARect.Top + (ARect.Bottom - ARect.Top) div 2 - Round(FDPIScale * 1) - 1 );
      end;
    end
    else
    begin
      if DoVisualStyles then
      begin
        htheme := OpenThemeData(Handle,'combobox');

        if not Enabled then
        begin
          DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
        end
        else
        begin
          if IsMouseButtonDown and DroppedDown then
          begin
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
          end
          else
          begin
            if not IsMouseButtonDown and FButtonHover and not DroppedDown then
              DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
            else
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
          end;
        end;

        CloseThemeData(htheme);
      end
      else
      begin
        if Enabled then
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
        else
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
      end;
    end;

    ExcludeClipRect(DC, ClientWidth - FButtonWidth - 4 , 0, ClientWidth +2, ClientHeight);
  end;


begin
  inherited;

  if not (FFlat or (FButtonColor <> clNone) or (FBorderColor <> clNone) or ((FFocusBorderColor <> clNone) and FHasFocus)) and not (not Enabled and DoVisualStyles and not DisabledBorder) then
  begin
    Exit;
  end;

  if Message.DC = 0 then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;
  try
    if (Style <> csSimple) then
    begin
      FillRect(DC, ClientRect, Brush.Handle);
      DrawButton;
    end;

    PaintWindow(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;

  DrawBorders;
end;

function TAdvCustomCombo.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

function TAdvCustomCombo.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TAdvCustomCombo.DrawButtonBorder(DC: HDC);
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  ARect: TRect;
  BtnFaceBrush: HBRUSH;

begin
  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4, ClientWidth - 4, ClientHeight - 4);

  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  if Is3DBorderButton then
    DrawEdge(DC, ARect, Edge[Etched], BF_RECT or Flags[DroppedDown])
  else
  begin
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    InflateRect(ARect, 0, -1);
    ARect.Right := ARect.Right - 1;
    FillRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
  end;

  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TAdvCustomCombo.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  OldPen: HPen;
begin
  if not Enabled and FIsThemed and not DisabledBorder then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB($B99D7F));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
    Exit;
  end;

  if (FBorderColor <> clNone) then
  begin
    if not FHasFocus or (FFocusBorderColor = clNone) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
      Exit;
    end;
  end;

  if (FFocusBorderColor <> clNone) then
  begin
    if FHasFocus then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((Parent as TWinControl).Brush.Color));

  WindowBrush := CreateSolidBrush(ColorToRGB(self.Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);

      ARect.Right := ARect.Right - ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      ARect.Right := ARect.Right + ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
    end;

    if FFlat and (FFlatLineColor <> clNone) then
    begin
      OldPen := SelectObject(DC,CreatePen( PS_SOLID,1,ColorToRGB(FFlatLineColor)));
      MovetoEx(DC,ARect.Left - 2,Height - 1,nil);
      LineTo(DC,ARect.Right - ButtonWidth - 1 ,Height - 1);
      DeleteObject(SelectObject(DC,OldPen));
    end;

  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

{$IFDEF DELPHIXE_LVL}
procedure TAdvCustomCombo.EditWndProc(var Message: TMessage);
var
  Shift: TShiftState;
begin
  if (Message.Msg = WM_LBUTTONDOWN) then
  begin
    Shift := [];
    if Message.WParam and MK_CONTROL = MK_CONTROL then
      Shift := Shift + [ssCtrl];
    if Message.WParam and MK_SHIFT = MK_SHIFT then
      Shift := Shift + [ssShift];

    if Assigned(OnMouseDown) then
      OnMouseDown(Self, mbLeft, Shift, Loword(Message.lparam), Hiword(Message.lparam));
  end;

  if (Message.Msg = WM_LBUTTONUP) then
  begin
    Shift := [];
    if Message.WParam and MK_CONTROL = MK_CONTROL then
      Shift := Shift + [ssCtrl];
    if Message.WParam and MK_SHIFT = MK_SHIFT then
      Shift := Shift + [ssShift];

    if Assigned(OnMouseUp) then
      OnMouseUp(Self, mbLeft, Shift, Loword(Message.lparam), Hiword(Message.lparam));
  end;
  inherited;
end;
{$ENDIF}

procedure TAdvCustomCombo.DrawBorders;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FBorderColor <> clNone) or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    if (Style <> csSimple) and not
      (FIsWinXP and DoVisualStyles) then
        DrawButtonBorder(DC);
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TAdvCustomCombo.CNCommand(var Message: TWMCommand);
var
  r: TRect;
begin
  inherited;

  if (Message.NotifyCode in [CBN_CLOSEUP,CBN_DROPDOWN]) then
  begin
    r := GetClientRect;
    r.Left := r.Right -  Round(FDPIScale * FButtonWidth);
    InvalidateRect(Handle,@r,FALSE);
    if (Message.NotifyCode = CBN_CLOSEUP) and Assigned(FOnDropUp) then
      FOnDropUp(Self);
  end;
end;


procedure TAdvCustomCombo.SetDropWidth(const Value: integer);
var
  dw: integer;
begin
  FDropWidth := Value;
  if Value > 0 then
  begin
    dw := Round(FDropWidth * FDPIScale);
    SendMessage(Handle, CB_SETDROPPEDWIDTH, dw, 0);
  end;
end;

function TAdvCustomCombo.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

procedure TAdvCustomCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (X > Width - Round(FDPIScale * FButtonWidth)) and (X < Width) then
  begin
    if not FButtonHover then
    begin
      FButtonHover := True;
      Invalidate;
    end;
  end
  else
  begin
    if FButtonHover then
    begin
      FButtonHover := False;
      Invalidate;
    end;
  end;

end;

procedure TAdvCustomCombo.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FToolTip) then
    FToolTip := nil;
end;

function TAdvCustomCombo.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAdvCustomCombo.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then UpdateLabel;  
end;

procedure TAdvCustomCombo.SetLabelCaption(const Value: string);
begin
  if (FLabel = nil) and (Value <> '') then
  begin
    FLabel := CreateLabel;
    FLabel.Caption := Value;
    UpdateLabel;
  end;

  if Assigned(FLabel) and (Value <> FLabel.Caption) then
  begin
    FLabel.Caption := Value;
    UpdateLabel;
  end;

end;

procedure TAdvCustomCombo.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomCombo.SetParent(AParent: TWinControl);
begin
  inherited;

//  if not (csDestroying in ComponentState) then
  begin
    if FLabel <> nil then
      FLabel.Parent := AParent;
  end;
end;

procedure TAdvCustomCombo.SetUIStyle(const Value: TTMSStyle);
begin
  SetComponentStyle(Value);
end;

destructor TAdvCustomCombo.Destroy;
begin
  FlabelFont.Destroy;
  if FLabel <> nil then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  inherited;
end;

function TAdvCustomCombo.CreateLabel: TLabel;
begin
  Result := TLabel.Create(self);
  Result.Parent:= Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.OnClick := LabelClick;
  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := ParentFont;
end;

function EditWindowProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
var
  OldWndProc: LInteger;
  DC: THandle;
  ACanvas: TCanvas;
  CC: TAdvCustomCombo;
  i: integer;
begin
  {$IFDEF DELPHI_UNICODE}
  CC := TAdvCustomCombo(GetWindowLongPtr(hWnd, GWL_USERDATA));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  CC := TAdvCustomCombo(GetWindowLong(hWnd, GWL_USERDATA));
  {$ENDIF}


  if (uMsg = WM_LBUTTONDOWN) and (CC.Text = '') and (CC.EmptyText <> '') and CC.EmptyTextFocused then
  begin
    SetFocus(hwnd);
    InvalidateRect(Hwnd,nil,true);
    Result := 1;
    Exit;
  end;

  if (uMsg = WM_KEYDOWN) then
  begin
    if (wParam = VK_RETURN) and CC.ReturnIsTab then
    begin
      PostMessage(CC.Handle, CB_SHOWDROPDOWN, Longint(False), 0);
      PostMessage(CC.Handle, WM_KEYDOWN, VK_TAB, 0);
      Result := 1;
      Exit;
    end;
  end;


  if (uMsg = WM_CHAR) then
  begin
    if (wParam = VK_RETURN) and CC.ReturnIsTab  then
    begin
      Result := 1;
      Exit;
    end;

    if (wParam = ord(#9)) and CC.DroppedDown then
    begin
      Result := 1;
      Exit;
    end;
  end;

  OldWndProc := CC.WinProc;
  Result := CallWindowProc(IntPtr(OldWndProc), hWnd, uMsg, WParam, lParam);

  if (uMsg = WM_KEYUP) and (lParam and $40000000 = $40000000) and CC.DroppedDown and not (CC.Style in [csSimple]) then
  begin
    if wParam = VK_TAB then
    begin
      i := cc.ItemIndex;
      PostMessage(CC.Handle, CB_SHOWDROPDOWN, Longint(False), 0);
      if (CC.Style = csDropDownList) then
       PostMessage(CC.Handle, CB_SETCURSEL, i, 0);
      PostMessage(CC.Handle, WM_KEYDOWN, VK_TAB, 0);
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_KEYDOWN) then
  begin
    CC.FGotReturn := wParam = VK_RETURN;
  end;

  if (uMsg = WM_KEYDOWN) and CC.DroppedDown then
  begin
    if wParam = VK_TAB then
    begin
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_KEYUP) and CC.ReturnIsTab and CC.FGotReturn then
  begin
    CC.FGotReturn := false;
    if wParam = VK_RETURN then
    begin
      PostMessage(CC.Handle, WM_KEYDOWN, VK_TAB, 0);
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_PAINT) and (CC.Text = '') and (CC.EmptyText <> '') then
  begin
    if not CC.EmptyTextFocused and (GetFocus = hwnd) then
      Exit;

    DC := GetDC(hwnd);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ACanvas.Font.Assign(CC.Font);
      ACanvas.Font.Color := clGray;
      ACanvas.Font.Style := CC.EmptyTextStyle;
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextOut(3,1, CC.EmptyText);
    finally
      ACanvas.Free;
      ReleaseDC(hwnd, DC);
    end;
  end;
end;

procedure TAdvCustomCombo.CreateWnd;
var
  ci: TComboBoxInfo;
  isScaled: boolean;
  frm: TCustomForm;
begin
  inherited;

  isScaled := true;

  frm := GetParentForm(Self);

  if Assigned(frm) and (frm is TForm) then
    isScaled := (frm as TForm).Scaled;

  FDPIScale := GetDPIScale(Self, Canvas);

  if not ParentFont and not FLoaded and (LabelCaption <> '') and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and isScaled then
  begin
    FLblUpdate := true;
    LabelFont.Height := Round(FLblFntHeight * FDPIScale);
    FLblUpdate := false;
  end;

  if Assigned(FLabel) then
    UpdateLabelPos;

  ci.cbSize := Sizeof(ci);
  GetComboBoxInfo(Handle, ci);

  {$IFDEF DELPHI_UNICODE}
  if (GetWindowLongPtr(ci.hwndItem, GWL_WNDPROC) <> LInteger(@EditWindowProc)) then
  begin
    WinProc := GetWindowLongPtr(ci.hwndItem, GWL_WNDPROC);
    SetWindowLongPtr(ci.hwndItem, GWL_USERDATA, LInteger(Self));
    SetWindowLongPtr(ci.hwndItem, GWL_WNDPROC, LInteger(@EditWindowProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if (GetWindowLong(ci.hwndItem, GWL_WNDPROC) <> LInteger(@EditWindowProc)) then
  begin
    WinProc := GetWindowLong(ci.hwndItem, GWL_WNDPROC);
    SetWindowLong(ci.hwndItem, GWL_USERDATA, LInteger(Self));
    SetWindowLong(ci.hwndItem, GWL_WNDPROC, LInteger(@EditWindowProc));
  {$ENDIF}
  end;
end;

procedure TAdvCustomCombo.UpdateLabel;
begin
  if Assigned(FLabel.Parent) then
  begin
    FLabel.Transparent := FLabeltransparent;
    {$IFDEF DELPHIXE6_LVL}
    FLabel.StyleElements := StyleElements;
    {$ENDIF}

    if not FParentFnt then
    begin
      FLabel.Font.Assign(FLabelFont)
    end
    else
    begin
      FLabel.Font.Assign(Font);
    end;

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

procedure TAdvCustomCombo.UpdateLabelPos;
var
  tw,th,lblmargin: integer;
  r: TRect;
  dpiscale: single;
begin
  r := Rect(0,0,1000,255);
  th := DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;
  flabel.Height := th;

  dpiscale := CalculateDPIScale(Self);
  lblmargin := Trunc(FLabelMargin * dpiscale);

  case FLabelPosition of
    lpLeftTop:
    begin
      FLabel.top := Top;
      FLabel.left := Left- tw - lblmargin;
    end;
    lpLeftCenter:
    begin
      if self.Height < FLabel.Height then
        FLabel.Top := Top - ((FLabel.Height - Height) div 2)
      else
        FLabel.top := Top + ((Height - FLabel.Height) div 2);
      FLabel.left := Left - tw - lblmargin;
    end;
    lpLeftBottom:
    begin
      FLabel.top := Top + Height - FLabel.Height;
      FLabel.left := Left - tw - lblmargin;
    end;
    lpTopLeft:
    begin
      FLabel.top := Top - FLabel.Height - lblmargin;
      FLabel.left := Left;
    end;
    lpTopCenter:
    begin
      FLabel.Top := Top - FLabel.Height - lblmargin;
      if (self.Width > FLabel.width) then
        FLabeL.Left := Left + ((Width - FLabel.width) div 2)
      else
        FLabeL.Left := Left - ((FLabel.Width - Width) div 2)
    end;
    lpTopRight:
    begin
      FLabel.Top := Top - FLabel.Height - lblmargin;
      FLabel.Left := Left + Width - FLabel.Width;
    end;
    lpBottomLeft:
    begin
      FLabel.top := Top + Height + lblmargin;
      FLabel.left := Left;
    end;
    lpBottomCenter:
    begin
      FLabel.top := self.Top + self.Height + lblmargin;

      if (self.Width > FLabel.width) then
        FLabeL.Left := Left + ((Width-FLabel.Width) div 2)
      else
        FLabeL.Left := Left - ((FLabel.Width - Width) div 2)
    end;
    lpBottomRight:
    begin
      FLabel.top := Top + Height + lblmargin;
      FLabel.Left := Left + Width - FLabel.Width;
    end;
    lpLeftTopLeft:
    begin
      FLabel.top := Top;
      FLabel.left := Left - lblmargin;
    end;
    lpLeftCenterLeft:
    begin
      if self.Height < FLabel.Height then
        FLabel.Top := Top - ((FLabel.Height - self.Height) div 2)
      else
        FLabel.top := Top + ((self.height-FLabel.height) div 2);

      FLabel.left := Left - lblmargin;
    end;
    lpLeftBottomLeft:
    begin
      FLabel.top := Top + Height - FLabel.height;
      FLabel.left := Left - lblmargin;
    end;
    lpRightTop:
    begin
      FLabel.Top := Top;
      FLabel.Left := Left + Width + lblmargin;
    end;
    lpRightCenter:
    begin
      if Self.Height > FLabel.Height then
        FLabel.Top := Top + ((self.Height - FLabel.Height) div 2)
      else
        FLabel.Top := Top - ((FLabel.Height - Height) div 2);

      FLabel.Left := Left + Width + lblmargin;
    end;
    lpRighBottom:
    begin
      FLabel.Top := Top + Height - FLabel.Height;
      FLabel.Left := Left + Width + lblmargin;
    end;
  end;

  FLabel.Visible := Visible;
end;

procedure TAdvCustomCombo.LabelClick(Sender: TObject);
begin
  if Assigned(OnLabelClick) then
    OnLabelClick(Self);
end;

procedure TAdvCustomCombo.LabelDblClick(Sender: TObject);
begin
  if Assigned(OnLabelDblClick) then
    OnLabelDblClick(Self);
end;

procedure TAdvCustomCombo.LabelFontChange(Sender: TObject);
begin
  if FLblUpdate then
    Exit;

  if Assigned(FLabel) then
    UpdateLabel;

  if not FDesignCreate and not (csLoading in ComponentState) then
    ParentFont := false;

  if not FLblUpdate and not (csLoading in ComponentState) then
  begin
    FLblFntHeight := LabelFont.Height;
  end;

  FLabelFontEqual := FontIsEqual(Font, LabelFont);
end;

procedure TAdvCustomCombo.Loaded;
{$IFNDEF DELPHIXE10_LVL}
var
  dpiscale: single;
{$ENDIF}
begin
  FLoaded := true;

  inherited Loaded;

  FLblFntHeight := LabelFont.Height;

  if FDropWidth > 0 then
    DropWidth := FDropWidth;

  FOldColor := FLoadedColor;

  if not FlatParentColor or Flat then
    Color := FLoadedColor;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
  begin
    FLabel.Enabled := Enabled;
  end;

  if Assigned(FLabel) then
    UpdateLabel;

  if ParentFont and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end
  else
  begin
    // is done in ChangeScale that is called always now
    {$IFNDEF DELPHIXE10_LVL}
    dpiscale := CalculateDPIScale(Self);
    FLabelFont.Height := Round(FLblFntHeight * dpiscale);
    {$ENDIF}
  end;

  FParentFnt := ParentFont;

  if Assigned(FLabel) then
    UpdateLabel;

  Init;

{$IFDEF DELPHIXE2_LVL}
  if not (csDesigning in ComponentState) and CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    LabelTransparent := true;
  end;
{$ENDIF}

end;

procedure TAdvCustomCombo.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;


procedure TAdvCustomCombo.Init;
begin
  FNormalColor := Color;
end;

procedure TAdvCustomCombo.SelectItem(AString: string);
var
  i: integer;
begin
  i := Items.IndexOf(Astring);
  if (i <> -1) then
  begin
    ItemIndex := i;
    Change;
  end;
end;

procedure TAdvCustomCombo.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0,0);
  end;
end;

procedure TAdvCustomCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  lblmargin: integer;
begin
  if Assigned(FLabel) then
  begin
    lblmargin := Trunc(FLabelMargin * FDPIScale);

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
      lpTopLeft, lpTopCenter:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited;

  if (csDestroying in ComponentState) then
    Exit;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomCombo.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvCustomCombo.SetFlatParentColor(const Value: Boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC);
var
  LastOrigin: TPoint;
begin
  GetWindowOrgEx(DC, LastOrigin);
  SetWindowOrgEx(DC, LastOrigin.X + Control.Left, LastOrigin.Y + Control.Top, nil);
  Control.Parent.Perform(WM_ERASEBKGND, DC, DC);
  SetWindowOrgEx(DC, LastOrigin.X, LastOrigin.Y, nil);
end;


procedure TAdvCustomCombo.SetItemIndex(const Value: Integer);
var
  w: integer;
begin
  inherited;

  if HandleAllocated then
    Invalidate;

  if Assigned(Parent) and Parent.HandleAllocated and Parent.DoubleBuffered and not (akRight in Anchors) then
  begin
    w := Parent.Width;
    MoveWindow(parent.Handle, parent.left, parent.Top, w + 1, parent.Height, false);
    MoveWindow(parent.Handle, parent.left, parent.Top, w, parent.Height, true);
  end;
end;

function TAdvCustomCombo.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvCustomCombo.SetColorEx(const Value: TColor);
begin
  if (csLoading in ComponentState) then
    FLoadedColor := Value;

  inherited Color := Value;

  FNormalColor := Value;
end;

procedure TAdvCustomCombo.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  if AStyle = tsCustom then
    Exit;

  case FTMSStyle of
    tsOffice2013LightGray:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := clWhite;
      FlatLineColor := clWindowText;
      //FocusFontColor := clWindowText;
      //Font.Color := clWindowText;
      FFontColor := clWindowText;
      FLabelFont.Color := clWindowText;
      LabelFont.Color := clWindowText;
    end;
    tsOffice2013Gray:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := clWhite;
      FlatLineColor := clWindowText;
      //FocusFontColor := clWindowText;
      //Font.Color := clWindowText;
      LabelFont.Color := clWindowText;
      FFontColor := clWindowText;
      FLabelFont.Color := clWindowText;
    end;
    tsOffice2016White:
    begin
      if Flat and Assigned(Parent) then
      begin
        Color := (Parent as TWinControl).Brush.Color;
      end
      else
        Color := clWhite;
      FlatLineColor := clWindowText;
      //FocusFontColor := clWindowText;
      //Font.Color := clWindowText;
      LabelFont.Color := clWindowText;
      FFontColor := clWindowText;
      FLabelFont.Color := clWindowText;
    end;
    tsOffice2016Gray:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := $00B2B2B2;
      FlatLineColor := $00444444;
      //FocusFontColor := clWindowText;
      //Font.Color := clWindowText;
      LabelFont.Color := clWindowText;
      FFontColor := clWindowText;
      FLabelFont.Color := clWindowText;
    end;
    tsOffice2016Black:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := $00363636;
      FlatLineColor := $00444444;
      //FocusFontColor := clSilver;
      //Font.Color := clSilver;
      LabelFont.Color := clSilver;
      FFontColor := clSilver;
      FLabelFont.Color := clSilver;
    end;
    tsOffice2019White:
    begin
      if Flat and Assigned(Parent) then
      begin
        Color := (Parent as TWinControl).Brush.Color;
      end
      else
        Color := clWhite;
      FlatLineColor := $00ABABAB;
      //FocusFontColor := $003B3B3B;
      //Font.Color := $00444648;
      LabelFont.Color := $00444648;
      FFontColor := $00444648;
      FLabelFont.Color := clWindowText;
      ButtonColor := clWhite;
      ButtonColorHot := $00F2E1D5;
      ButtonColorDown := $00E3BDA3;
      ButtonTextColor := $00444648;
      ButtonTextColorHot := $00232425;
      ButtonTextColorDown := $00232425;
      ButtonBorderColor := $00ABABAB
    end;
    tsOffice2019Gray:
    begin
      if Flat and Assigned(Parent) then
      begin
        Color := (Parent as TWinControl).Brush.Color;
        //FocusFontColor := clWhite;
      end
      else
      begin
        Color := $00B8BBBE;
        //FocusFontColor := $001E1F20;
      end;
      FlatLineColor := $00808080;
      //Font.Color := $00232425;
      LabelFont.Color := clWhite;;
      FFontColor := $00232425;
      FLabelFont.Color := clWhite;
      ButtonColor := $00B8BBBE;
      ButtonColorHot := $00969696;
      ButtonColorDown := $00666666;
      ButtonTextColor := $00232425;
      ButtonTextColorHot := $00232425;
      ButtonTextColorDown := $00232425;
      ButtonBorderColor := $00808080;
    end;
    tsOffice2019Black:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := $00444444;
      FlatLineColor := $00686868;
      //FocusFontColor := clWhite;
      //Font.Color := clWhite;
      LabelFont.Color := clWhite;
      FFontColor := clWhite;
      FLabelFont.Color := clWhite;
      ButtonColor := $00444444;
      ButtonColorHot := $00686868;
      ButtonColorDown := $00828282;
      ButtonTextColor := clWhite;
      ButtonTextColorHot := clWhite;
      ButtonTextColorDown := clWhite;
      ButtonBorderColor := $00686868;
    end
    else
    begin
      if Flat and FlatParentColor and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := clWhite;
      FlatLineColor := clWindowText;
      //FocusFontColor := clWindowText;
      //Font.Color := clWindowText;
      LabelFont.Color := clWindowText;
      FFontColor := clWindowText;
      FLabelFont.Color := clWindowText;
      ButtonColor := clNone;
      ButtonColorHot := clNone;
      ButtonColorDown := clNone;
      ButtonTextColor := clNone;
      ButtonTextColorHot := clNone;
      ButtonTextColorDown := clNone;
      ButtonBorderColor := clNone;
    end;
  end;

  Font.Color := FFontColor;
  FocusFontColor := FFontColor;
  Invalidate;
end;

function TAdvCustomCombo.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvCustomCombo.SetEmptyText(const Value: string);
begin
  if (FEmptyText <> Value) then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEmptyTextStyle(const Value: TFontStyles);
begin
  if (FEmptyTextStyle <> Value) then
  begin
    FEmptyTextStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEnabledEx(const Value: Boolean);
var
  OldValue: Boolean;

begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
     (csDesigning in ComponentState) then
    Exit;

  if OldValue <> Value then
  begin
    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
      begin
        FLabel.Enabled := Value;
        UpdateLabel;
      end;
  end;
end;

function TAdvCustomCombo.GetVersionEx: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomCombo.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomCombo.SetVersion(const Value: string);
begin

end;

function TAdvCustomCombo.GetVisibleEx: boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvCustomCombo.HideValidation;
begin
  if Assigned(FToolTipWindow) then
  begin
    FToolTipWindow.Free;
    FToolTipWindow := nil;
  end;
end;

{$IFDEF DELPHI2007_LVL}
procedure TAdvCustomCombo.HideBalloon;
var
  cbi: TComboBoxInfo;
begin
  cbi.cbSize := sizeof(TComboBoxInfo);
  GetComboBoxInfo(Handle, cbi);
  SendMessage(cbi.hwndItem, EM_HIDEBALLOONTIP, 0, 0);
end;
{$ENDIF}

procedure TAdvCustomCombo.SetVisibleEx(const Value: boolean);
begin
  inherited Visible := Value;
  if Assigned(FLabel) then
    FLabel.Visible := Value;
end;

procedure TAdvCustomCombo.ShowValidation(const AText: string);
begin
  if not Assigned(FToolTipWindow) then
    FToolTipWindow := TAdvToolTipWindow.Create(Self);

  FToolTipWindow.Text := AText;
  FToolTipWindow.Show(Self, FToolTip);
end;

{$IFDEF DELPHI2007_LVL}
procedure TAdvCustomCombo.ShowBalloon(const ATitle, AText: string;
  AType: TBalloonType);
var
  ebt: TEditBalloonTip;
  title, Text: Widestring;
  icon: Integer;
  cbi: TComboBoxInfo;

begin
  Title := ATitle;
  Text := AText;
  Icon := integer(AType);
  with ebt do
  begin
    cbStruct := SizeOf(ebt);
    pszTitle := PWideChar(title);
    pszText := PWideChar(Text);
    ttiIcon := icon;
  end;

  cbi.cbSize := sizeof(TComboBoxInfo);
  GetComboBoxInfo(Handle, cbi);
  SendMessage(cbi.hwndItem, EM_SHOWBALLOONTIP, 0, LParam(@ebt));
end;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
{$IFNDEF DELPHIXE10_LVL}
procedure TAdvCustomCombo.ChangeScale(M, D: Integer);
{$ENDIF}
{$IFDEF DELPHIXE10_LVL}
procedure TAdvCustomCombo.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
begin
  inherited;

//  if not (csLoading in ComponentState) then
  begin
    FLblUpdate := true;
    FLabelFont.Height := MulDiv(FLabelFont.Height, M, D);
    FLblUpdate := false;
  end;

  FDPIScale := GetDPIScale(Self, Canvas);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);

  if HandleAllocated then
    DropWidth := FDropWidth;
end;
{$ENDIF}

procedure TAdvCustomCombo.DoExit;
begin
  inherited;
  if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
    Color := FNormalColor;

  if (FFocusFontColor <> clNone) then
  begin
    Font.Color := FFontColor;
    if Assigned(FLabel) then
      UpdateLabel;
  end;

  if FFocusLabel and Assigned(FLabel) then
  begin
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    UpdateLabelPos;
  end;

  Invalidate;
end;


procedure TAdvCustomCombo.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
end;

procedure TAdvCustomCombo.WMMouseWheel(var Message: TMessage);
begin
  if not FEnableWheel then
    Message.Result := 1
  else
    inherited;
end;

{$IFDEF DELPHIXE_LVL}
procedure TAdvCustomCombo.WMContextMenu(var Message: TWMContextMenu);
begin
  inherited;
end;

procedure TAdvCustomCombo.WMMouseMove(var Message: TWMMouseMove);
var
  Shift: TShiftState;
begin
  inherited;

  Shift := [];
  if Message.Keys and MK_CONTROL = MK_CONTROL then
    Shift := Shift + [ssCtrl];
  if Message.Keys and MK_SHIFT = MK_SHIFT then
    Shift := Shift + [ssShift];

  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, Message.XPos, Message.YPos);
end;

procedure TAdvCustomCombo.WMLButtonDown(var Message: TWMLButtonDown);
var
  Shift: TShiftState;
begin
  inherited;

  Shift := [];
  if Message.Keys and MK_CONTROL = MK_CONTROL then
    Shift := Shift + [ssCtrl];
  if Message.Keys and MK_SHIFT = MK_SHIFT then
    Shift := Shift + [ssShift];

  if Assigned(OnMouseDown) then
    OnMouseDown(Self, mbLeft, Shift, Message.XPos, Message.YPos);
end;

procedure TAdvCustomCombo.WMLButtonUp(var Message: TWMLButtonUp);
var
  Shift: TShiftState;
begin
  inherited;
  Shift := [];
  if Message.Keys and MK_CONTROL = MK_CONTROL then
    Shift := Shift + [ssCtrl];
  if Message.Keys and MK_SHIFT = MK_SHIFT then
    Shift := Shift + [ssShift];

  if Assigned(OnMouseUp) then
    OnMouseUp(Self, mbLeft, Shift, Message.XPos, Message.YPos);
end;
{$ENDIF}

procedure TAdvCustomCombo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  if (FFocusBorderColor <> clNone) then
   Invalidate;

  if (FFocusColor <> clNone) then
  begin
    inherited Color := FFocusColor;
  end;

  if (FFocusFontColor <> clNone) and (Font.Color <> FFocusFontColor) then
  begin
    FFontColor := Font.Color;
    Font.Color := FFocusFontColor;
    if Assigned(FLabel) then
      UpdateLabel;
  end;

  if FFocusLabel and Assigned(FLabel) then
  begin
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
    UpdateLabelPos;
  end;
end;

end.

