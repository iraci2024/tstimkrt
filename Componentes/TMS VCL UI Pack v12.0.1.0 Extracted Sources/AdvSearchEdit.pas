{*************************************************************************}
{ TMS TAdvSearchEdit                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                        }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2022                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSearchEdit;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Types, Forms, Controls, StdCtrls, Messages, Graphics,
  AdvGlowButton, AdvSearchList, AdvDropDown, SysUtils, Dialogs, Menus, GDIPicture,
  AdvStyleIF, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : Control border drawing
  // v1.0.2.0 : New : Exposed TabOrder property
  //          : New : TAdvSearchEdit.SearchList exposed
  //          : New : FocusColor, FocusFontColor, FocusBorder, FocusBorderColor properties added
  //          : New : DisabledColor property added
  // v1.0.3.0 : New : Property ItemIndex added at TAdvSearchEdit level
  // v1.0.3.1 : Fixed : Issue with Appearance.SelectionColor / Appearance.SelectionTextColor when no VCL styles are used
  //          : Fixed : Issue with handling Shift,Ctrl key when dropdown is displayed
  // v1.0.4.0 : New : Exposed Images property to set an imagelist
  // v1.0.4.1 : Fixed : Initialization issue with EmptyText
  // v1.0.4.2 : Improved : Lookup handling & ItemIndex handling
  // v1.0.4.3 : Fixed : Issue with footer & header button images
  // v1.0.4.4 : Fixed : Issue with focus when setting Form.ActiveControl
  // v1.0.5.0 : New : Exposed Font property
  // v1.0.6.0 : New : Exposed public property Edit to access embedded search edit control directly
  // v1.0.6.1 : Improved : Changed sequence to update ItemIndex to get the index correct from the OnChange event
  // v1.0.7.0 : New : Event OnSelect added
  // v1.0.8.0 : New : DropDownHeight property exposed for TAdvSearchEdit
  // v1.0.9.0 : New : OnFiltered event added
  // v1.0.9.1 : Fixed : Issue with setting ItemIndex programmatically
  // v1.0.9.2 : Fixed : Issue in combination with FastMM4
  // v1.0.9.3 : Improved : Tab key can be used to close the TAdvSearchEdit
  // v1.1.0.0 : New : Creation style is now Office 2019 White
  // v1.1.0.1 : Fixed : Creation style is changed to default style defined in AdvStyleIF
  // v1.1.0.2 : New : UIStyle property for office look
  //          : Improved : On creation check for enabled AdvFormStyler
  //          : Fixed : Category button lines are now drawn in the selected font color
  // v1.1.2.0 : New : StyleElements property exposed
  //          : Fixed : color persistence
  // v1.1.3.0 : Improved : Office styles support
  // v1.1.3.1 : Fixed : Issue with ItemIndex retrieval from OnSelect event
  // v1.1.3.2 : Fixed : Issue with incremental search lookup reset
  // v1.1.3.3 : Fixed : Issue with retrieving ItemIndex after setting at runtime
  // v1.1.3.4 : Fixed : Issue with retrieving ItemIndex from the OnSelect event
  // v1.1.4.0 : Fixed : Issue with accented chars in search
  // v1.1.4.1 : Fixed : Issue with ESC char in dropdown triggering Cancel property
  // v1.1.5.0 : New : ShowDropDown method added to TAdvSearchEdit
  // v1.1.5.1 : Fixed : Issue with category filtering
  // v1.1.5.2 : Fixed : Issue with font initialization in high DPI
  // v1.1.5.3 : Fixed : High DPI handling in Delphi 11
  // v1.1.5.4 : Improved : BorderColor updating during editing
  // v1.1.5.5 : Fixed : Rare issue with font size on high DPI during runtime creation

type

  TDropDownSelectEvent = procedure(Sender: TObject; var NewValue: string) of object;

  TAdvSearchDropDown = class(TAdvCustomDropDown)
  private
    FOldText: string;
    FItemIndex: integer;
    FSearchList: TAdvSearchList;
    FOnSelect: TDropDownSelectEvent;
    FKeyDropDown: boolean;
    FOnFiltered: TNotifyEvent;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
  protected
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure UpdateIndex;
  protected
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: Char); override;
    procedure OnDropDownControlClick(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function CreateSearchList: TAdvSearchList; virtual;
    procedure DoShowDropDown; override;
    procedure ShowDropDown;
    property ItemIndex: integer read FItemIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SearchList: TAdvSearchList read FSearchList;
    property OnSelect: TDropDownSelectEvent read FOnSelect write FOnSelect;
    property OnFiltered: TNotifyEvent read FOnFiltered write FOnFiltered;
    property DropDownHeight;
  end;

  TGetButtonAppearance = procedure(Sender: TObject; var Appearance: TGlowButtonAppearance) of object;
  TSetButtonAppearance = procedure(Sender: TObject; Appearance: TGlowButtonAppearance) of object;

  TSearchEditButton = class(TPersistent)
  private
    FOwner: TPersistent;
    FWidth: integer;
    FVisible: boolean;
    FCaption: string;
    FOnChange: TNotifyEvent;
    FPicture: TGDIPPicture;
    FOnGetAppearance: TGetButtonAppearance;
    FOnSetAppearance: TSetButtonAppearance;
    FBorderStyle: TBorderStyle;
    FAppearance: TGlowButtonAppearance;
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: boolean);
    procedure SetWidth(const Value: integer);
    procedure SetPicture(const Value: TGDIPPicture);
    function GetAppearance: TGlowButtonAppearance;
    procedure SetAppearance(const Value: TGlowButtonAppearance);
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAppearance: TGetButtonAppearance read FOnGetAppearance write FOnGetAppearance;
    property OnSetAppearance: TSetButtonAppearance read FOnSetAppearance write FOnSetAppearance;
  published
    property Appearance: TGlowButtonAppearance read GetAppearance write SetAppearance;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Caption: string read FCaption write SetCaption;
    property Picture: TGDIPPicture read FPicture write SetPicture;
    property Width: integer read FWidth write SetWidth default 24;
    property Visible: boolean read FVisible write SetVisible default true;
  end;

  TPopupMenuType = (pmCheck, pmRadio);

  TSearchEditPopupButton = class(TSearchEditButton)
  private
    FPopupType: TPopupMenuType;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property PopupType: TPopupMenuType read FPopupType write FPopupType default pmCheck;
  end;

  TCategoryItemClick = procedure(Sender: TObject; CategoryIndex: integer; isChecked: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCustomSearchEdit = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FEdit: TAdvSearchDropDown;
    FCatBtn: TAdvGlowButton;
    FSearchBtn: TAdvGlowButton;
    FCatPopup: TPopupMenu;
    FCategoryButton: TSearchEditPopupButton;
    FSearchButton: TSearchEditButton;
    FOnDropDownHeaderButtonClick: TDropDownButtonItemClick;
    FOnBeforeDropDown: TNotifyEvent;
    FOnGetHeaderText: TGetTextEvent;
    FOnDrawFooter: TDrawBackGroundEvent;
    FOnBeforeDropUp: TNotifyEvent;
    FOnDropDownFooterButtonClick: TDropDownButtonItemClick;
    FOnGetFooterText: TGetTextEvent;
    FOnDropDown: TDropDown;
    FOnDrawHeader: TDrawBackGroundEvent;
    FOnDropUP: TDropUP;
    FOnCategoryPopupClick: TCategoryItemClick;
    FOnSearchButtonClick: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FUseVCLStyles: boolean;
    FOnChange: TNotifyEvent;
    FFocusBorder: boolean;
    FFocusBorderColor: TColor;
    FDisabledColor: TColor;
    FNormalColor: TColor;
    FNormalFontColor: TColor;
    FFocusFontColor: TColor;
    FFocusColor: TColor;
    FItemIndex: integer;
    FOnSelect: TNotifyEvent;
    FOnFiltered: TNotifyEvent;
    FDropDownHeight: integer;
    FTMSStyle: TTMSStyle;
    FEditColor: TColor;
    FListSelectionTextColor: TColor;
    FListColor: TColor;
    FListItemFontColor: TColor;
    FListSelectionColor: TColor;
    FDropDownColor: TColor;
    FDropDownButtonsColorDown: TColor;
    FDropDownButtonsColorDownTo: TColor;
    FDropDownButtonsBorderColor: TColor;
    FDropDownButtonsBorderColorHot: TColor;
    FDropDownButtonsColorDisabled: TColor;
    FDropDownButtonsColorDisabledTo: TColor;
    FDropDownButtonsColor: TColor;
    FDropDownButtonsColorTo: TColor;
    FDropDownButtonsColorHot: TColor;
    FDropDownButtonsColorHotTo: TColor;
    FDropDownButtonsBorderColorDown: TColor;
    FDropDownButtonsBorderColorDisabled: TColor;
    FDropDownButtonsFontColor: TColor;
    FDropDownArrowColor: TColor;
    FDropDownArrowColorHot: TColor;
    FDropDownArrowColorDown: TColor;
    FDropDownButtonColor: TColor;
    FDropDownButtonColorHot: TColor;
    FDropDownButtonColorDown: TColor;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    function GetCategories: TCategoryList;
    procedure SetCategories(const Value: TCategoryList);
    function GetAppearance: TAdvSearchListAppearance;
    procedure SetAppearance(const Value: TAdvSearchListAppearance);
    function GetColumns: TColumnItems;
    procedure SetColumns(const Value: TColumnItems);
    function GetItems: TSearchList;
    procedure SetItems(const Value: TSearchList);
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    function GetFilterCondition: TFilterCondition;
    procedure SetFilterCondition(const Value: TFilterCondition);
    function GetDropDownFooter: TFooterAppearance;
    function GetDropDownHeader: THeaderAppearance;
    procedure SetDropDownFooter(const Value: TFooterAppearance);
    procedure SetDropDownHeader(const Value: THeaderAppearance);
    function GetDropDownShadow: boolean;
    function GetDropDownSizeable: boolean;
    procedure SetDropDownShadow(const Value: boolean);
    procedure SetDropDownSizeable(const Value: boolean);
    function GetDropDownWidth: integer;
    procedure SetDropDownWidth(const Value: integer);
    procedure SetCategoryButton(const Value: TSearchEditPopupButton);
    procedure SetSearchButton(const Value: TSearchEditButton);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBorderColor(const Value: TColor);
    procedure SetOnDrawFooter(const Value: TDrawBackGroundEvent);
    procedure SetOnDrawHeader(const Value: TDrawBackGroundEvent);
    function GetEmptyText: string;
    function GetEmptyTextFocused: boolean;
    function GetEmptyTextStyle: TFontStyles;
    procedure SetEmptyText(const {%H-}Value{%H+}: string);
    procedure SetEmptyTextFocused(const Value: boolean);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    function GetAutoSelect: boolean;
    procedure SetAutoSelect(const Value: boolean);
    function GetSearchList: TAdvSearchList;
    procedure SetDisabledColor(const Value: TColor);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    function GetDropDownHeight: integer;
    procedure SetDropDownHeight(const Value: integer);
    procedure SetEditColor(const Value: TColor);
    procedure SetListColor(const Value: TColor);
    procedure SetListItemFontColor(const Value: TColor);
    procedure SetListSelectionColor(const Value: TColor);
    procedure SetListSelectionTextColor(const Value: TColor);
    procedure SetColors;
    procedure StoreColors;
    procedure SetDropDownColor(const Value: TColor);
    procedure SetDropDownButtonsBorderColor(const Value: TColor);
    procedure SetDropDownButtonsBorderColorDisabled(const Value: TColor);
    procedure SetDropDownButtonsBorderColorDown(const Value: TColor);
    procedure SetDropDownButtonsBorderColorHot(const Value: TColor);
    procedure SetDropDownButtonsColor(const Value: TColor);
    procedure SetDropDownButtonsColorDisabled(const Value: TColor);
    procedure SetDropDownButtonsColorDisabledTo(const Value: TColor);
    procedure SetDropDownButtonsColorDown(const Value: TColor);
    procedure SetDropDownButtonsColorDownTo(const Value: TColor);
    procedure SetDropDownButtonsColorHot(const Value: TColor);
    procedure SetDropDownButtonsColorHotTo(const Value: TColor);
    procedure SetDropDownButtonsColorTo(const Value: TColor);
    procedure SetDropDownButtonsFontColor(const Value: TColor);
    procedure SetDropDownArrowColor(const Value: TColor);
    procedure SetDropDownArrowColorDown(const Value: TColor);
    procedure SetDropDownArrowColorHot(const Value: TColor);
    procedure SetDropDownButtonColor(const Value: TColor);
    procedure SetDropDownButtonColorDown(const Value: TColor);
    procedure SetDropDownButtonColorHot(const Value: TColor);
    function GetDroppedDown: boolean;
  protected
    function GetDocURL: string;
    function CreateSearchDropDown: TAdvSearchDropDown; virtual;
    procedure DrawBorders; virtual;
    procedure DrawCategoryButton(Sender: TObject; Canvas: TCanvas; Rect: TRect; State: TGlowButtonState);
    procedure CatBtnClick(Sender: TObject);
    procedure CatBtnGetAppearance(Sender: TObject; var Appearance: TGlowButtonAppearance);
    procedure CatBtnSetAppearance(Sender: TObject; Appearance: TGlowButtonAppearance);
    procedure SearchBtnGetAppearance(Sender: TObject; var Appearance: TGlowButtonAppearance);
    procedure SearchBtnSetAppearance(Sender: TObject; Appearance: TGlowButtonAppearance);
    procedure DoCatMenuItemClick(Sender: TObject);
    procedure DoCategoryButtonChanged(Sender: TObject);
    procedure DoSearchButtonChanged(Sender: TObject);
    procedure DoCategoryPopupClick(CategoryIndex: integer; isChecked: boolean); virtual;
    procedure DoSearchButtonClick(Sender: TObject); virtual;
    procedure DoDropDownHeaderButtonClick(Sender: TObject; ButtonIndex: Integer);
    procedure DoDropDownFooterButtonClick( Sender: TObject; ButtonIndex: Integer);
    procedure DoDrawHeader(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure DoDrawFooter(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure DoGetHeaderText(Sender: TObject; var Text: string);
    procedure DoGetFooterText(Sender: TObject; var Text: string);
    procedure DoBeforeDropDown(Sender: TObject); virtual;
    procedure DoEditChanged(Sender: TObject); virtual;
    procedure DoSelectValue(Sender: TObject; var Value: string); virtual;
    procedure DoFiltered(Sender: TObject); virtual;
    procedure DoDropDown(Sender: TObject; var AcceptDrop: Boolean); virtual;
    procedure DoDropUp(Sender: TObject; Cancelled: Boolean); virtual;
    procedure DoBeforeDropUp(Sender: TObject); virtual;

    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyPress(Sender: TObject; var key:char); virtual;
    procedure HandleEnter(Sender: TObject); virtual;
    procedure HandleExit(Sender: TObject); virtual;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    property UseVCLStyles: boolean read FUseVCLStyles write FUseVCLStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    procedure SetColorTones(ATones: TColorTones); virtual;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    procedure Init;
    function GetComponentStyle: TTMSStyle;
    property SearchList: TAdvSearchList read GetSearchList;
    procedure UpdateFilter; virtual;
    procedure LoadStrings(Value: TStrings);
    procedure SelectAll;
    procedure ShowDropDown;
    procedure Repaint; override;
    property Edit: TAdvSearchDropDown read FEdit;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;

    property Align;
    property Anchors;
    property Appearance: TAdvSearchListAppearance read GetAppearance write SetAppearance;
    property AutoSelect: boolean read GetAutoSelect write SetAutoSelect default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Categories: TCategoryList read GetCategories write SetCategories;
    property CategoryButton: TSearchEditPopupButton read FCategoryButton write SetCategoryButton;
    property Columns: TColumnItems read GetColumns write SetColumns;
    property Constraints;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownHeader: THeaderAppearance read GetDropDownHeader write SetDropDownHeader;
    property DropDownHeight: integer read GetDropDownHeight write SetDropDownHeight default 0;
    property DropDownFooter: TFooterAppearance read GetDropDownFooter write SetDropDownFooter;
    property DropDownShadow: boolean read GetDropDownShadow write SetDropDownShadow default false;
    property DropDownSizable: boolean read GetDropDownSizeable write SetDropDownSizeable default true;
    property DropDownWidth: integer read GetDropDownWidth write SetDropDownWidth default 0;
    property DroppedDown: boolean read GetDroppedDown;

    property EmptyText: string read GetEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read GetEmptyTextFocused write SetEmptyTextFocused default false;
    property EmptyTextStyle: TFontStyles read GetEmptyTextStyle write SetEmptyTextStyle default [];

    property FilterCondition: TFilterCondition read GetFilterCondition write SetFilterCondition;
    property FocusBorder: boolean read FFocusBorder write FFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clNone;
    property Font;
    property Images: TCustomImageList read GetImages write SetImages;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property Items: TSearchList read GetItems write SetItems;
    property PopupMenu;
    property SearchButton: TSearchEditButton read FSearchButton write SetSearchButton;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}

    property ShowHint;
    property TabOrder;
    property Text: string read GetText write SetText;
    property Version: string read GetVersion write SetVersion;
    property Visible;

    property OnCategoryPopupClick: TCategoryItemClick read FOnCategoryPopupClick write FOnCategoryPopupClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDownHeaderButtonClick: TDropDownButtonItemClick read FOnDropDownHeaderButtonClick write FOnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick: TDropDownButtonItemClick read FOnDropDownFooterButtonClick write FOnDropDownFooterButtonClick;
    property OnDrawHeader: TDrawBackGroundEvent read FOnDrawHeader write SetOnDrawHeader;
    property OnDrawFooter: TDrawBackGroundEvent read FOnDrawFooter write SetOnDrawFooter;
    property OnGetHeaderText: TGetTextEvent read FOnGetHeaderText write FOnGetHeaderText;
    property OnGetFooterText: TGetTextEvent read FOnGetFooterText write FOnGetFooterText;

    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnDropDown: TDropDown read FOnDropDown write FOnDropDown;
    property OnBeforeDropUp: TNotifyEvent read FOnBeforeDropUp write FOnBeforeDropUp;
    property OnDropUp: TDropUP read FOnDropUP write FOnDropUp;
    property OnFiltered: TNotifyEvent read FOnFiltered write FOnFiltered;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnSearchButtonClick: TNotifyEvent read FOnSearchButtonClick write FOnSearchButtonClick;

    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnExit;
    property OnEnter;
    property UIStyle: TTMSStyle read FTMSStyle write SetComponentStyle default tsCustom;
    property EditColor: TColor read FEditColor write SetEditColor default clWindow;
    property ListColor: TColor read FListColor write SetListColor default clWindow;
    property ListItemFontColor: TColor read FListItemFontColor write SetListItemFontColor default clNone;
    property ListSelectionColor: TColor read FListSelectionColor write SetListSelectionColor default clHighlight;
    property ListSelectionTextColor: TColor read FListSelectionTextColor write SetListSelectionTextColor default clHighlightText;
    property DropDownColor: TColor read FDropDownColor write SetDropDownColor default clWhite;
    property DropDownButtonsBorderColor: TColor read FDropDownButtonsBorderColor write SetDropDownButtonsBorderColor default clNone;
    property DropDownButtonsBorderColorHot: TColor read FDropDownButtonsBorderColorHot write SetDropDownButtonsBorderColorHot default clGray;
    property DropDownButtonsBorderColorDown: TColor read FDropDownButtonsBorderColorDown write SetDropDownButtonsBorderColorDown default clNavy;
    property DropDownButtonsBorderColorDisabled: TColor read FDropDownButtonsBorderColorDisabled write SetDropDownButtonsBorderColorDisabled default clGray;
    property DropDownButtonsColor: TColor read FDropDownButtonsColor write SetDropDownButtonsColor default clNone;
    property DropDownButtonsColorTo: TColor read FDropDownButtonsColorTo write SetDropDownButtonsColorTo default clNone;
    property DropDownButtonsColorDisabled: TColor read FDropDownButtonsColorDisabled write SetDropDownButtonsColorDisabled default $00F2F2F2;
    property DropDownButtonsColorDisabledTo: TColor read FDropDownButtonsColorDisabledTo write SetDropDownButtonsColorDisabledTo default clNone;
    property DropDownButtonsColorDown: TColor read FDropDownButtonsColorDown write SetDropDownButtonsColorDown default $00F5D8CA;
    property DropDownButtonsColorDownTo: TColor read FDropDownButtonsColorDownTo write SetDropDownButtonsColorDownTo default $00F9BDA0;
    property DropDownButtonsColorHot: TColor read FDropDownButtonsColorHot write SetDropDownButtonsColorHot default $F5F0E1;
    property DropDownButtonsColorHotTo: TColor read FDropDownButtonsColorHotTo write SetDropDownButtonsColorHotTo default $F9D2B2;
    property DropDownButtonsFontColor: TColor read FDropDownButtonsFontColor write SetDropDownButtonsFontColor default clWindowText;
    property DropDownArrowColor: TColor read FDropDownArrowColor write SetDropDownArrowColor default clWindowText;
    property DropDownArrowColorHot: TColor read FDropDownArrowColorHot write SetDropDownArrowColorHot default clWindowText;
    property DropDownArrowColorDown: TColor read FDropDownArrowColorDown write SetDropDownArrowColorDown default clWindowText;
    property DropDownButtonColor: TColor read FDropDownButtonColor write SetDropDownButtonColor default clNone;
    property DropDownButtonColorHot: TColor read FDropDownButtonColorHot write SetDropDownButtonColorHot default clNone;
    property DropDownButtonColorDown: TColor read FDropDownButtonColorDown write SetDropDownButtonColorDown default clNone;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSearchEdit = class(TAdvCustomSearchEdit)
  published
    property Align;
    property Anchors;
    property Appearance;
    property AutoSelect;
    property BorderColor;
    property BorderStyle;
    property Categories;
    property CategoryButton;
    property Columns;
    property Constraints;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownHeader;
    property DropDownHeight;
    property DropDownFooter;
    property DropDownShadow;
    property DropDownSizable;
    property DropDownWidth;

    property EmptyText;
    property EmptyTextFocused;
    property EmptyTextStyle;

    property FilterCondition;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property FocusFontColor;
    property Font;
    property Images;
    property ItemHeight;
    property Items;
    property PopupMenu;
    property SearchButton;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}

    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Version;
    property Visible;

    property OnCategoryPopupClick;
    property OnChange;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;

    property OnBeforeDropDown;
    property OnDropDown;
    property OnBeforeDropUp;
    property OnDropUp;
    property OnFiltered;
    property OnSelect;
    property OnSearchButtonClick;

    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnExit;
    property OnEnter;
    property UIStyle;
    property EditColor;
    property ListColor;
    property ListItemFontColor;
    property ListSelectionColor;
    property ListSelectionTextColor;
    property DropDownColor;
    property DropDownButtonsBorderColor;
    property DropDownButtonsBorderColorHot;
    property DropDownButtonsBorderColorDown;
    property DropDownButtonsBorderColorDisabled;
    property DropDownButtonsColor;
    property DropDownButtonsColorTo;
    property DropDownButtonsColorDisabled;
    property DropDownButtonsColorDisabledTo;
    property DropDownButtonsColorDown;
    property DropDownButtonsColorDownTo;
    property DropDownButtonsColorHot;
    property DropDownButtonsColorHotTo;
    property DropDownButtonsFontColor;
    property DropDownArrowColor;
    property DropDownArrowColorHot;
    property DropDownArrowColorDown;
    property DropDownButtonColor;
    property DropDownButtonColorHot;
    property DropDownButtonColorDown;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSearchComboBox = class(TAdvCustomSearchEdit)
  private
    FItems: TStrings;
    FSorted: boolean;
    FOnCloseUp: TNotifyEvent;
    procedure SetSorted(const Value: boolean);
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
  protected
    procedure ItemsChanged(Sender: TObject);
    procedure SetItems(AItems: TStrings); reintroduce;
    procedure Resize; override;
    procedure DoDropUp(Sender: TObject; Cancelled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property BorderColor;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items: TStrings read FItems write SetItems;

    property ParentColor;
    property ParentCtl3D;
    property ParentCustomHint;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted: boolean read FSorted write SetSorted default false;
    property TabOrder;
    property Text;
    property TextHint: string read GetTextHint write SetTextHint;
    property Version;
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;

    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnExit;
    property OnEnter;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
  uxTheme;

const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

type
  // cracker classes
  TSearchListEx = class(TAdvSearchList)
  end;

  TAdvGlowButtonCracker = class(TAdvGlowButton)
  end;

{ TAdvCustomSearchEdit }

procedure TAdvCustomSearchEdit.CatBtnClick(Sender: TObject);
var
  mnu: TMenuItem;
  pt: TPoint;
  i: integer;
begin
  FCatPopup.Items.Clear;

  pt := Point(FCatBtn.Left, FCatBtn.Top + FCatBtn.Height);

  pt := FCatBtn.ClientToScreen(pt);

  for i := 0 to Categories.Count - 1 do
  begin
    mnu := TMenuItem.Create(FCatPopup);
    mnu.Caption := Categories[i].Caption;

    if FCategoryButton.PopupType = pmCheck then
    begin
      mnu.Checked := not Categories[i].Filter;
      mnu.RadioItem := false;
    end
    else
    begin
      mnu.Checked := Categories[i].Filter;
      mnu.RadioItem := true;
    end;

    mnu.AutoCheck := true;
    mnu.GroupIndex := 1;
    mnu.Tag := i;
    mnu.OnClick := DoCatMenuItemClick;

    FCatPopup.Items.Add(mnu);
  end;

  FCatPopup.Popup(pt.X, pt.Y);
end;

procedure TAdvCustomSearchEdit.CatBtnGetAppearance(Sender: TObject;
  var Appearance: TGlowButtonAppearance);
begin
  Appearance := FCatBtn.Appearance;
end;

procedure TAdvCustomSearchEdit.CatBtnSetAppearance(Sender: TObject;
  Appearance: TGlowButtonAppearance);
begin
  FCatBtn.Appearance.Assign(Appearance);
end;

procedure TAdvCustomSearchEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;

  if not Enabled then
  begin
    Color := FDisabledColor;
    FEdit.Color := FDisabledColor;
  end
  else
  begin
    Color := FNormalColor;
    FEdit.Color := FNormalColor;
  end;
end;

procedure TAdvCustomSearchEdit.CMFontChanged(var Message: TMessage);
begin
  if Assigned(FEdit) and  FEdit.HandleAllocated then
  begin
    FEdit.Font.Name := Font.Name;
    FEdit.Font.Height := Font.Height;
  end;
end;

constructor TAdvCustomSearchEdit.Create(AOwner: TComponent);
var
  FDesignTime: boolean;

begin
  inherited;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FCategoryButton := TSearchEditPopupButton.Create(Self);
  FCategoryButton.OnGetAppearance := CatBtnGetAppearance;
  FCategoryButton.OnSetAppearance := CatBtnSetAppearance;

  FSearchButton := TSearchEditButton.Create(Self);
  FSearchButton.OnGetAppearance := SearchBtnGetAppearance;
  FSearchButton.OnSetAppearance := SearchBtnSetAppearance;

  FEdit := CreateSearchDropDown;
  FEdit.Font.Height := -11;
  FEdit.Parent := Self;
  FEdit.BorderStyle := bsNone;
  FEdit.OnDropDown := DoDropDown;
  FEdit.OnDropUp := DoDropUp;
  FEdit.OnDropDownHeaderButtonClick := DoDropDownHeaderButtonClick;
  FEdit.OnDropDownFooterButtonClick := DoDropDownFooterButtonClick;
  FEdit.OnBeforeDropDown := DoBeforeDropDown;
  FEdit.OnBeforeDropUp := DoBeforeDropUp;
  FEdit.OnGetHeaderText := DoGetHeaderText;
  FEdit.OnGetFooterText := DoGetFooterText;
  FEdit.OnSelect := DoSelectValue;
  FEdit.OnFiltered := DoFiltered;
  FEdit.OnChange := DoEditChanged;
  FEdit.OnKeyDown := DoKeyDown;
  FEdit.OnKeyUp := DoKeyUp;
  FEdit.OnKeyPress := DoKeyPress;
  FEdit.OnEnter := HandleEnter;
  FEdit.OnExit := HandleExit;

  FCatBtn := TAdvGlowButton.Create(Self);
  TAdvGlowButtonCracker(FCatBtn).IsDesignTime := FDesignTime;
  FCatBtn.Parent := Self;
  FCatBtn.Width := 24;
  FCatBtn.BorderStyle := bsNone;
  FCatBtn.OnClick := CatBtnClick;
  FCatBtn.OnDrawButton := DrawCategoryButton;

  FSearchBtn := TAdvGlowButton.Create(Self);
  TAdvGlowButtonCracker(FSearchBtn).IsDesignTime := FDesignTime;
  FSearchBtn.Parent := Self;
  FSearchBtn.Width := 24;
  FSearchBtn.BorderStyle := bsNone;
  FSearchBtn.OnClick := DoSearchButtonClick;

  FCatPopup := TPopupMenu.Create(Self);

  FUseVCLStyles := false;
  FBorderColor := clNone;
  FBorderStyle := bsSingle;
  Width := 300;
  Height := 21;

  FItemIndex := -1;

  FCatBtn.Align := alLeft;

  FSearchBtn.Align := alRight;

  FSearchBtn.Caption := '...';

  FEdit.Margins.Top := 0;
  FEdit.Margins.Bottom := 0;
  FEdit.Margins.Left := 1;
  FEdit.Margins.Right := 1;
  FEdit.AlignWithMargins := true;
  FEdit.Align := alClient;

  if FDesignTime then
  begin
    FEdit.EmptyText := 'Search ...';
    SetComponentStyle(GetDefaultStyle(AOwner));
    Font.Height := -11;
  end;

  FTMSStyle := tsCustom;

  FFocusBorder := false;
  FFocusBorderColor := clNone;
  FDisabledColor := clSilver;
  FFocusColor := clNone;
  FFocusFontColor := clBlack;

  FNormalColor := clWindow;
  FNormalFontColor := clWindowText;

  FEditColor := clWindow;
  FListColor := clWindow;
  FListItemFontColor := clNone;
  FListSelectionColor := clHighlight;
  FListSelectionTextColor := clHighlightText;

  FDropDownButtonsBorderColor := clNone;
  FDropDownButtonsBorderColorHot := clGray;
  FDropDownButtonsBorderColorDown := clNavy;
  FDropDownButtonsBorderColorDisabled := clGray;
  FDropDownButtonsColor := clNone;
  FDropDownButtonsColorTo := clNone;
  FDropDownButtonsColorDisabled := $00F2F2F2;
  FDropDownButtonsColorDisabledTo := clNone;
  FDropDownButtonsColorDown := $00F5D8CA;
  FDropDownButtonsColorDownTo := $00F9BDA0;
  FDropDownButtonsColorHot := $F5F0E1;
  FDropDownButtonsColorHotTo := $F9D2B2;
  FDropDownButtonsFontColor := clWindowText;
  FDropDownArrowColor := clWindowText;
  FDropDownArrowColorHot := clWindowText;
  FDropDownArrowColorDown := clWindowText;
  DropDownButtonColor := clNone;
  DropDownButtonColorHot := clNone;
  DropDownButtonColorDown := clNone;

  FCategoryButton.OnChange := DoCategoryButtonChanged;
  FSearchButton.OnChange := DoSearchButtonChanged;
end;

procedure TAdvCustomSearchEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BorderStyles[FBorderStyle];
end;

function TAdvCustomSearchEdit.CreateSearchDropDown: TAdvSearchDropDown;
begin
  Result := TAdvSearchDropDown.Create(Self);
end;

destructor TAdvCustomSearchEdit.Destroy;
begin
  FCategoryButton.Free;
  FSearchButton.Free;
  FCatPopup.Free;
  FEdit.Free;
  FCatBtn.Free;
  FSearchBtn.Free;
  inherited;
end;

procedure TAdvCustomSearchEdit.DoBeforeDropDown(Sender: TObject);
begin
  if Assigned(OnBeforeDropDown) then
    OnBeforeDropDown(Self);
end;

procedure TAdvCustomSearchEdit.DoBeforeDropUp(Sender: TObject);
begin
  if Assigned(OnBeforeDropUp) then
    OnBeforeDropUp(Self);
end;

procedure TAdvCustomSearchEdit.DoCategoryButtonChanged(Sender: TObject);
begin
  FCatBtn.Visible := FCategoryButton.Visible;

  if not FCategoryButton.Visible then
    FCatBtn.Width := 0
  else
    FCatBtn.Width := FCategoryButton.Width;

  FCatBtn.Caption := FCategoryButton.Caption;
  FCatBtn.Picture.Assign(FCategoryButton.Picture);
  FCatBtn.BorderStyle := FCategoryButton.BorderStyle;
end;

procedure TAdvCustomSearchEdit.DoCategoryPopupClick(CategoryIndex: integer; isChecked: boolean);
begin
  if Assigned(OnCategoryPopupClick) then
    OnCategoryPopupClick(Self, CategoryIndex, isChecked);
end;

procedure TAdvCustomSearchEdit.DoCatMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  with (Sender as TMenuItem) do
  begin
    DoCategoryPopupClick((Sender as TMenuItem).Tag, not Checked);

    if FCategoryButton.PopupType = pmCheck then
      Categories[Tag].Filter := not Checked
    else
    begin
      for i := 0 to Categories.Count - 1 do
        Categories[i].Filter := False;

      Categories[Tag].Filter := True;
    end;
  end;
end;

procedure TAdvCustomSearchEdit.DoDrawFooter(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect);
begin
  if Assigned(OnDrawFooter) then
    OnDrawFooter(Self, ACanvas, ARect);
end;

procedure TAdvCustomSearchEdit.DoDrawHeader(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect);
begin
  if Assigned(OnDrawHeader) then
    OnDrawHeader(Self, ACanvas, ARect);
end;

procedure TAdvCustomSearchEdit.DoDropDown(Sender: TObject; var AcceptDrop: Boolean);
begin
  if Assigned(OnDropDown) then
    OnDropDown(Self, AcceptDrop);
end;

procedure TAdvCustomSearchEdit.DoDropDownFooterButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  if Assigned(OnDropDownFooterButtonClick) then
    OnDropDownFooterButtonClick(Self, ButtonIndex);
end;

procedure TAdvCustomSearchEdit.DoDropDownHeaderButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  if Assigned(OnDropDownHeaderButtonClick) then
    OnDropDownHeaderButtonClick(Self, ButtonIndex);
end;

procedure TAdvCustomSearchEdit.DoDropUp(Sender: TObject; Cancelled: Boolean);
begin
  if Assigned(OnDropUp) then
    OnDropUp(Self, Cancelled);
end;

procedure TAdvCustomSearchEdit.DoEditChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvCustomSearchEdit.DoFiltered(Sender: TObject);
begin
  if Assigned(OnFiltered) then
    OnFiltered(Self);
end;

procedure TAdvCustomSearchEdit.HandleEnter(Sender: TObject);
begin
  DrawBorders;
  if FFocusColor <> clNone then
  begin
    // prepare for restore
    FNormalColor := FEdit.Color;
    FEdit.Color := FFocusColor;
    Color := FFocusColor;
  end;

  if FFocusFontColor <> clNone then
  begin
    FNormalFontColor := FEdit.Font.Color;
    FEdit.Font.Color := FFocusFontColor;
    Font.Color := FFocusFontColor;
  end;
end;

procedure TAdvCustomSearchEdit.HandleExit(Sender: TObject);
begin
  DrawBorders;

  if FFocusColor <> clNone then
  begin
    FEdit.Color := FNormalColor;
    Color := FNormalColor;
  end;

  if FFocusFontColor <> clNone then
  begin
    FEdit.Font.Color := FNormalFontColor;
    Font.Color := FNormalFontColor;
  end;
end;

procedure TAdvCustomSearchEdit.Init;
var
  OldColor: TColor;
begin
  FNormalColor := Color;
  FNormalFontColor := Font.Color;

  if not Enabled then
  begin
    OldColor := Color;
    Color := FDisabledColor;
    FNormalColor := OldColor;
  end;
end;

procedure TAdvCustomSearchEdit.DoGetFooterText(Sender: TObject; var Text: string);
begin
  Text := DropDownFooter.Caption;
  if Assigned(OnGetFooterText) then
    OnGetFooterText(Self, Text);
end;

procedure TAdvCustomSearchEdit.DoGetHeaderText(Sender: TObject; var Text: string);
begin
  Text := DropDownHeader.Caption;
  if Assigned(OnGetHeaderText) then
    OnGetHeaderText(Self, Text);
end;

procedure TAdvCustomSearchEdit.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvCustomSearchEdit.DoKeyPress(Sender: TObject; var key: char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvCustomSearchEdit.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvCustomSearchEdit.DoSearchButtonChanged(Sender: TObject);
begin
  FSearchBtn.Visible := FSearchButton.Visible;

  if not FSearchButton.Visible then
    FSearchBtn.Width := 0
  else
    FSearchBtn.Width := FSearchButton.Width;

  FSearchBtn.Caption := FSearchButton.Caption;
  FSearchBtn.Picture.Assign(FSearchButton.Picture);
  FSearchBtn.BorderStyle := FSearchButton.BorderStyle;
end;

procedure TAdvCustomSearchEdit.DoSearchButtonClick(Sender: TObject);
begin
  if Assigned(OnSearchButtonClick) then
    OnSearchButtonClick(Self);
end;

procedure TAdvCustomSearchEdit.DoSelectValue(Sender: TObject; var Value: string);
begin
  FItemIndex := FEdit.SearchList.SelectedItem.Index;

  FEdit.FItemIndex := FItemIndex;
  // reset search lookup
  FEdit.FOldText := '';

  if Assigned(OnSelect) then
    OnSelect(Self);
end;

procedure TAdvCustomSearchEdit.DrawBorders;
{$IFDEF VCLLIB}
var
  DC: HDC;
  OldPen: HPen;
  ARect: TRect;
  //hTheme: THandle;
  clr: TColor;
{$ENDIF}
begin
  {$IFDEF VCLLIB}
  DC := GetWindowDC(Handle);

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    if IsThemeActive and not UseVCLStyles and (BorderColor = clNone) then
    begin
      //hTheme := OpenThemeData(Handle,'COMBOBOX');

      if GetFocus = FEdit.Handle then
        clr := $D77800
      else
        clr := $7A7A7A;

      if FocusBorder and (GetFocus = FEdit.Handle) and (FocusBorderColor <> clNone) then
        clr := FocusBorderColor;

      OldPen := SelectObject(DC,CreatePen(PS_SOLID, 1, ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));

//      if GetFocus = FEdit.Handle then
//        DrawThemeBackground(hTheme, DC, CP_BACKGROUND, CBS_HOT, ARect, 0)
//      else
//        DrawThemeBackground(hTheme, DC, CP_BACKGROUND, CBS_NORMAL, ARect, 0);
//
//      CloseThemeData(hTheme);
    end
    else
    begin
      if BorderColor = clNone then
        clr := clBlack
      else
        clr := BorderColor;

      if FocusBorder and (GetFocus = FEdit.Handle) and (FocusBorderColor <> clNone) then
        clr := FocusBorderColor;

      OldPen := SelectObject(DC,CreatePen(PS_SOLID, 1, ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));
    end;
  finally
    ReleaseDC(Handle,DC);
  end;
  {$ENDIF}
end;

procedure TAdvCustomSearchEdit.DrawCategoryButton(Sender: TObject; Canvas: TCanvas;
  Rect: TRect; State: TGlowButtonState);
var
  d,l: integer;
begin
  if (FCatBtn.Caption = '') and (FCatBtn.Picture.Empty) then
  begin
    Canvas.Pen.Color := FCatBtn.Font.Color;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;

    d := (FCatBtn.Height - 10) div 2;
    l := (FCatBtn.Width - 14) div 2;

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d);

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d + 5);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d + 5);

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d + 10);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d + 10);
  end;

end;

function TAdvCustomSearchEdit.Focused: Boolean;
begin
  Result := false;
  if Assigned(FEdit) then
    Result := FEdit.Focused;
end;

function TAdvCustomSearchEdit.GetAppearance: TAdvSearchListAppearance;
begin
  Result := FEdit.SearchList.Appearance;
end;

function TAdvCustomSearchEdit.GetAutoSelect: boolean;
begin
  Result := FEdit.AutoSelect;
end;

function TAdvCustomSearchEdit.GetCategories: TCategoryList;
begin
  Result := FEdit.SearchList.Categories;
end;

function TAdvCustomSearchEdit.GetColumns: TColumnItems;
begin
  Result := FEdit.SearchList.Columns;
end;

function TAdvCustomSearchEdit.GetComponentStyle: TTMSStyle;
begin
  Result := FEdit.GetComponentStyle;
end;

function TAdvCustomSearchEdit.GetDocURL: string;
begin
  Result := TTMSFNCSearchListDocURL;
end;

function TAdvCustomSearchEdit.GetDropDownFooter: TFooterAppearance;
begin
  Result := FEdit.DropDownFooter;
end;

function TAdvCustomSearchEdit.GetDropDownHeader: THeaderAppearance;
begin
  Result := FEdit.DropDownHeader;
end;

function TAdvCustomSearchEdit.GetDropDownHeight: integer;
begin
  Result := FDropDownHeight;
end;

function TAdvCustomSearchEdit.GetDropDownShadow: boolean;
begin
  Result := FEdit.DropDownShadow;
end;

function TAdvCustomSearchEdit.GetDropDownSizeable: boolean;
begin
  Result := FEdit.DropDownSizeable;
end;

function TAdvCustomSearchEdit.GetDropDownWidth: integer;
begin
  Result := FEdit.DropDownWidth;
end;

function TAdvCustomSearchEdit.GetDroppedDown: boolean;
begin
  Result := false;
  if Assigned(FEdit) then
    Result := FEdit.DroppedDown;
end;

function TAdvCustomSearchEdit.GetEmptyText: string;
begin
  Result := FEdit.EmptyText;
end;

function TAdvCustomSearchEdit.GetEmptyTextFocused: boolean;
begin
  Result := FEdit.EmptyTextFocused;
end;

function TAdvCustomSearchEdit.GetEmptyTextStyle: TFontStyles;
begin
  Result := FEdit.EmptyTextStyle;
end;

function TAdvCustomSearchEdit.GetFilterCondition: TFilterCondition;
begin
  Result := FEdit.SearchList.FilterCondition;
end;

function TAdvCustomSearchEdit.GetImages: TCustomImageList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.Images;
end;

function TAdvCustomSearchEdit.GetItemHeight: Integer;
begin
  Result := 20;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.ItemHeight;
end;

function TAdvCustomSearchEdit.GetItemIndex: Integer;
begin
  Result := -1;
  if not (csDestroying in ComponentState) then
    Result := FEdit.FItemIndex;
end;

function TAdvCustomSearchEdit.GetItems: TSearchList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.Items;
end;

function TAdvCustomSearchEdit.GetSearchList: TAdvSearchList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList;
end;

function TAdvCustomSearchEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

function TAdvCustomSearchEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

function TAdvCustomSearchEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

function TAdvCustomSearchEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomSearchEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomSearchEdit.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    Init;

  FCatBtn.Picture.Assign(FCategoryButton.Picture);
  FSearchBtn.Picture.Assign(FSearchButton.Picture);
  {$IFDEF DELPHIXE6_LVL}
  FEdit.StyleElements := StyleElements;
  {$ENDIF}
end;

procedure TAdvCustomSearchEdit.LoadStrings(Value: TStrings);
begin
  if Columns.Count = 0 then
  begin
    Columns.Add;
    FilterCondition.Column := 0;
  end;
  Items.LoadStrings(Value);
end;

procedure TAdvCustomSearchEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TAdvCustomSearchEdit.Repaint;
begin
  inherited;
  if Assigned(FEdit) and (FEdit.Focused or FEdit.DroppedDown) then
  begin
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TAdvCustomSearchEdit.SearchBtnGetAppearance(Sender: TObject;
  var Appearance: TGlowButtonAppearance);
begin
  Appearance := FSearchBtn.Appearance;
end;

procedure TAdvCustomSearchEdit.SearchBtnSetAppearance(Sender: TObject;
  Appearance: TGlowButtonAppearance);
begin
  FSearchBtn.Appearance.Assign(Appearance);
end;

procedure TAdvCustomSearchEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TAdvCustomSearchEdit.SetAppearance(const Value: TAdvSearchListAppearance);
begin
  FEdit.SearchList.Appearance.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetAutoSelect(const Value: boolean);
begin
  FEdit.AutoSelect := Value;
end;

procedure TAdvCustomSearchEdit.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if Assigned(FEdit) then
    begin
      if Assigned(FEdit.FSearchList) then
        FEdit.FSearchList.BorderColor := Value;
    end;
    Invalidate;
  end;
end;

procedure TAdvCustomSearchEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TAdvCustomSearchEdit.SetDropDownArrowColor(const Value: TColor);
begin
  FDropDownArrowColor := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonTextColor := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownArrowColorDown(const Value: TColor);
begin
  FDropDownArrowColorHot := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonTextColorDown := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownArrowColorHot(const Value: TColor);
begin
  FDropDownArrowColorHot := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonTextColorHot := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonColor(const Value: TColor);
begin
  FDropDownButtonColor := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonColor := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonColorDown(const Value: TColor);
begin
  FDropDownButtonColorDown := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonColorDown := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonColorHot(const Value: TColor);
begin
  FDropDownButtonColorHot := Value;
  if Assigned(FEdit) and Assigned(FEdit.Button) then
    FEdit.Button.ButtonColorHot := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsBorderColor(const Value: TColor);
begin
  FDropDownButtonsBorderColor := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.BorderColor := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsBorderColorDisabled(const Value: TColor);
begin
  FDropDownButtonsBorderColorDisabled := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.BorderColorDisabled := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsBorderColorDown(const Value: TColor);
begin
  FDropDownButtonsBorderColorDown := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.BorderColorDown := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsBorderColorHot(const Value: TColor);
begin
  FDropDownButtonsBorderColorHot := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.BorderColorHot := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColor(const Value: TColor);
begin
  FDropDownButtonsColor := Value;
  if Assigned(FEdit) then
  begin
    FEdit.ButtonAppearance.Color := Value;
    if Assigned(FEdit.Button) then
      FEdit.Button.ButtonColor := Value;
  end;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorDisabled(const Value: TColor);
begin
  FDropDownButtonsColorDisabled := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.ColorDisabled := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorDisabledTo(const Value: TColor);
begin
  FDropDownButtonsColorDisabledTo := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.ColorDisabledTo := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorDown(const Value: TColor);
begin
  FDropDownButtonsColorDown := Value;
  if Assigned(FEdit) then
  begin
    FEdit.ButtonAppearance.ColorDown := Value;
    if Assigned(FEdit.Button) then
      FEdit.Button.ButtonColorDown := Value;
  end;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorDownTo(const Value: TColor);
begin
  FDropDownButtonsColorDownTo := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.ColorDownTo := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorHot(const Value: TColor);
begin
  FDropDownButtonsColorHot := Value;
  if Assigned(FEdit) then
  begin
    FEdit.ButtonAppearance.ColorHot := Value;
    if Assigned(FEdit.Button) then
      FEdit.Button.ButtonColorHot := Value;
  end;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorHotTo(const Value: TColor);
begin
  FDropDownButtonsColorHotTo := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.ColorHotTo := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsColorTo(const Value: TColor);
begin
  FDropDownButtonsColorTo := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.ColorTo := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownButtonsFontColor(const Value: TColor);
begin
  FDropDownButtonsFontColor := Value;
  if Assigned(FEdit) then
    FEdit.ButtonAppearance.Font.Color := Value;
end;

procedure TAdvCustomSearchEdit.SetCategories(const Value: TCategoryList);
begin
  FEdit.SearchList.Categories.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetCategoryButton(const Value: TSearchEditPopupButton);
begin
  FCategoryButton.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetColors;
begin
  if (FTMSStyle = tsCustom) and Assigned(FEdit) then
  begin
    FEdit.Color := FEditColor;
    FEdit.Font.Color := Font.Color;

    FEdit.ButtonAppearance.Color := FDropDownButtonsColor;
    FEdit.ButtonAppearance.ColorTo := FDropDownButtonsColorTo;
    FEdit.ButtonAppearance.BorderColor := FDropDownButtonsBorderColor;

    FEdit.ButtonAppearance.ColorHot := FDropDownButtonsColorHot;
    FEdit.ButtonAppearance.ColorHotTo := FDropDownButtonsColorHotTo;
    FEdit.ButtonAppearance.BorderColorHot := FDropDownButtonsBorderColorHot;

    FEdit.ButtonAppearance.ColorDown := FDropDownButtonsColorDown;
    FEdit.ButtonAppearance.ColorDownTo := FDropDownButtonsColorDownTo;
    FEdit.ButtonAppearance.BorderColorDown := FDropDownButtonsBorderColorDown;

    FEdit.ButtonAppearance.ColorDisabled := FDropDownButtonsColorDisabled;
    FEdit.ButtonAppearance.ColorDisabledTo := FDropDownButtonsColorDisabledTo;
    FEdit.ButtonAppearance.BorderColorDisabled := FDropDownButtonsBorderColorDisabled;

    FEdit.ButtonAppearance.Font.Color := FDropDownButtonsFontColor;

    if Assigned(FEdit.Button) then
    begin
      FEdit.Button.ButtonTextColor := FDropDownArrowColor;
      FEdit.Button.ButtonTextColorHot := FDropDownArrowColorHot;
      FEdit.Button.ButtonTextColorDown := FDropDownArrowColorDown;

      FEdit.Button.ButtonColor := FDropDownButtonColor;
      FEdit.Button.ButtonColor := FDropDownButtonColorHot;
      FEdit.Button.ButtonColorDown := FDropDownButtonColorDown;
    end;
  end;
end;

procedure TAdvCustomSearchEdit.SetColorTones(ATones: TColorTones);
begin
  FEdit.SetColorTones(ATones);
  FCatBtn.SetColorTones(ATones);
  FSearchBtn.SetColorTones(ATones);
end;

procedure TAdvCustomSearchEdit.SetColumns(const Value: TColumnItems);
begin
  FEdit.SearchList.Columns.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;

  FEdit.SetComponentStyle(AStyle);

  if Assigned(FEdit.FSearchList) then
    FEdit.FSearchList.SetComponentStyle(AStyle);

  FCatBtn.SetComponentStyle(AStyle);
  FSearchBtn.SetComponentStyle(AStyle);

  FCategoryButton.SetAppearance(FCatBtn.Appearance);
  SearchButton.SetAppearance(FSearchBtn.Appearance);

  if AStyle <> tsCustom then
  begin
    StoreColors;

    case AStyle of
      tsOffice2019White:
      begin
        Font.Color := $00444648;
        FocusFontColor := $003B3B3B;
        DropDownHeader.BorderWidth := 0;
        DropDownFooter.BorderWidth := 0;
        ListColor := clWhite;
        ListItemFontColor := $00444648;
        ListSelectionColor := $00F2D5C2;
        ListSelectionTextColor := $003B3B3B;
        BorderColor := $00A3A3A3;
        DropDownShadow := False;
      end;
      tsOffice2019Gray:
      begin
        Font.Color := $00232425;
        FocusFontColor := $003B3B3B;
        DropDownHeader.BorderWidth := 0;
        DropDownFooter.BorderWidth := 0;
        ListColor := $00B8BBBE;
        ListItemFontColor := $00232425;
        ListSelectionColor := $00969696;
        ListSelectionTextColor := $003B3B3B;
        BorderColor := $00808080;
        DropDownShadow := False;
      end;
      tsOffice2019Black:
      begin
        Font.Color := clWhite;
        FocusFontColor := clWhite;
        DropDownHeader.BorderWidth := 0;
        DropDownFooter.BorderWidth := 0;
        ListColor := clBlack;
        ListItemFontColor := clWhite;
        ListSelectionColor := $00505050;
        ListSelectionTextColor := clWhite;
        BorderColor := $00686868;
        DropDownShadow := False;
      end;
      else
      begin
        Font.Color := clWindowText;
        FocusFontColor := clBlack;
        DropDownHeader.BorderWidth := 1;
        DropDownFooter.BorderWidth := 1;
        DropDownShadow := False;
        ListColor := clWindow;
        ListItemFontColor := clNone;
        ListSelectionColor :=clHighlight;
        ListSelectionTextColor := clHighlightText;
      end;
    end;
  end;
end;

procedure TAdvCustomSearchEdit.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomSearchEdit.SetDropDownColor(const Value: TColor);
begin
  FDropDownColor := Value;
  if Assigned(FEdit) then
    FEdit.DropDownColor := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownFooter(const Value: TFooterAppearance);
begin
  FEdit.DropDownFooter.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetDropDownHeader(const Value: THeaderAppearance);
begin
  FEdit.DropDownHeader.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetDropDownHeight(const Value: integer);
begin
  FDropDownHeight := Value;
  FEdit.DropDownHeight := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownShadow(const Value: boolean);
begin
  FEdit.DropDownShadow := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownSizeable(const Value: boolean);
begin
  FEdit.DropDownSizeable := Value;
end;

procedure TAdvCustomSearchEdit.SetDropDownWidth(const Value: integer);
begin
  FEdit.DropDownWidth := Value;
end;

procedure TAdvCustomSearchEdit.SetEditColor(const Value: TColor);
begin
  FEditColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := FEditColor;
end;

procedure TAdvCustomSearchEdit.SetEmptyText(const Value: string);
begin
  FEdit.EmptyText := Value;
end;

procedure TAdvCustomSearchEdit.SetEmptyTextFocused(const Value: boolean);
begin
  FEdit.EmptyTextFocused := Value;
end;

procedure TAdvCustomSearchEdit.SetEmptyTextStyle(const Value: TFontStyles);
begin
  FEdit.EmptyTextStyle := Value;
end;

procedure TAdvCustomSearchEdit.SetFilterCondition(const Value: TFilterCondition);
begin
  FEdit.SearchList.FilterCondition.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetFocus;
begin
  FEdit.SetFocus;
end;

procedure TAdvCustomSearchEdit.SetImages(const Value: TCustomImageList);
begin
  FEdit.SearchList.Images := Value;
  FEdit.Images := Value;
end;

procedure TAdvCustomSearchEdit.SetItemHeight(const Value: Integer);
begin
  FEdit.SearchList.ItemHeight := Value;
end;

procedure TAdvCustomSearchEdit.SetItemIndex(const Value: Integer);
var
  sli: TSearchListItem;
begin
  FEdit.SearchList.ItemIndex := Value;
  FEdit.FItemIndex := Value;

  if (Value >= 0) and (Value < FEdit.FSearchList.Items.Count) then
  begin
    sli := FEdit.FSearchList.Items[Value];

    if Assigned(sli) then
    begin
      if FEdit.FSearchList.FilterCondition.Column < sli.Columns.Count  then
        Text := sli.Columns[FEdit.FSearchList.FilterCondition.Column].Caption;
      FEdit.SelectAll;
    end;
  end
  else
    Text := '';
end;

procedure TAdvCustomSearchEdit.SetItems(const Value: TSearchList);
begin
  FEdit.SearchList.Items.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetListColor(const Value: TColor);
begin
  FListColor := Value;
  if Assigned(FEdit.FSearchList) then
    FEdit.FSearchList.Color := Value;
end;

procedure TAdvCustomSearchEdit.SetListItemFontColor(const Value: TColor);
var
  i,j: integer;
begin
  FListItemFontColor := Value;
  if Assigned(FEdit.FSearchList) then
  begin
    for I := 0 to Items.Count - 1  do
    begin
      for J := 0 to Items[i].Columns.Count - 1 do
      begin
        Items[i].Columns[j].TextColor := Value;
      end;
    end;
  end;
end;

procedure TAdvCustomSearchEdit.SetListSelectionColor(const Value: TColor);
begin
  FListSelectionColor := Value;
  if Assigned(FEdit.FSearchList) then
    FEdit.FSearchList.Appearance.SelectionColor := Value;
end;

procedure TAdvCustomSearchEdit.SetListSelectionTextColor(const Value: TColor);
begin
  FListSelectionTextColor := Value;
  if Assigned(FEdit.FSearchList) then
    FEdit.FSearchList.Appearance.SelectionTextColor := Value;
end;

procedure TAdvCustomSearchEdit.SetOnDrawFooter(const Value: TDrawBackGroundEvent);
begin
  FOnDrawFooter := Value;

  if Assigned(Value) then
    FEdit.OnDrawFooter := DoDrawFooter
  else
    FEdit.OnDrawFooter := nil;
end;

procedure TAdvCustomSearchEdit.SetOnDrawHeader(const Value: TDrawBackGroundEvent);
begin
  FOnDrawHeader := Value;

  if Assigned(Value) then
    FEdit.OnDrawHeader := DoDrawHeader
  else
    FEdit.OnDrawHeader := nil;
end;

procedure TAdvCustomSearchEdit.SetSearchButton(const Value: TSearchEditButton);
begin
  FSearchButton.Assign(Value);
end;

procedure TAdvCustomSearchEdit.SetSelLength(const Value: Integer);
begin
  FEdit.SelLength := Value;
end;

procedure TAdvCustomSearchEdit.SetSelStart(const Value: Integer);
begin
  FEdit.SelStart := Value;
end;

procedure TAdvCustomSearchEdit.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

procedure TAdvCustomSearchEdit.SetVersion(const Value: string);
begin
//
end;

procedure TAdvCustomSearchEdit.ShowDropDown;
begin
  FEdit.ShowDropDown;
end;

procedure TAdvCustomSearchEdit.StoreColors;
begin
  if Assigned(FEdit) then
  begin
    FEditColor := FEdit.Color;
    Font.Color := FEdit.Font.Color;

    FDropDownButtonsColor := FEdit.ButtonAppearance.Color;
    FDropDownButtonsColorTo := FEdit.ButtonAppearance.ColorTo;
    FDropDownButtonsBorderColor := FEdit.ButtonAppearance.BorderColor;

    FDropDownButtonsColorHot := FEdit.ButtonAppearance.ColorHot;
    FDropDownButtonsColorHotTo := FEdit.ButtonAppearance.ColorHotTo;
    FDropDownButtonsBorderColorHot := FEdit.ButtonAppearance.BorderColorHot;

    FDropDownButtonsColorDown := FEdit.ButtonAppearance.ColorDown;
    FDropDownButtonsColorDownTo := FEdit.ButtonAppearance.ColorDownTo;
    FDropDownButtonsBorderColorDown := FEdit.ButtonAppearance.BorderColorDown;

    FDropDownButtonsColorDisabled := FEdit.ButtonAppearance.ColorDisabled;
    FDropDownButtonsColorDisabledTo := FEdit.ButtonAppearance.ColorDisabledTo;
    FDropDownButtonsBorderColorDisabled := FEdit.ButtonAppearance.BorderColorDisabled;

    FDropDownButtonsFontColor := FEdit.ButtonAppearance.Font.Color;

    if Assigned(FEdit.Button) then
    begin
      FDropDownArrowColor := FEdit.Button.ButtonTextColor;
      FDropDownArrowColorHot := FEdit.Button.ButtonTextColorHot;
      FDropDownArrowColorDown := FEdit.Button.ButtonTextColorDown;

      FDropDownButtonColor := FEdit.Button.ButtonColor;
      FDropDownButtonColorHot := FEdit.Button.ButtonColor;
      FDropDownButtonColorDown := FEdit.Button.ButtonColorDown;
    end;
  end;
end;

procedure TAdvCustomSearchEdit.UpdateFilter;
begin
  FEdit.SearchList.UpdateFilter;
end;

procedure TAdvCustomSearchEdit.WMChar(var Msg: TWMChar);
begin
  if Assigned(FEdit) then
    FEdit.DefaultHandler(Msg);
end;

procedure TAdvCustomSearchEdit.WMNCPaint(var Message: TMessage);
begin
  SetColors;
  inherited;
  if BorderStyle = bsSingle then
    DrawBorders;
end;

procedure TAdvCustomSearchEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if Visible then
    FEdit.SetFocus;
end;

{ TAdvSearchDropDown }

procedure TAdvSearchDropDown.BeforeDropDown;
begin
  inherited;
  FSearchList.Width := Width;
  FSearchList.UpdateFilter;
end;

constructor TAdvSearchDropDown.Create(AOwner: TComponent);
begin
  FSearchList := CreateSearchList;
  FSearchList.BorderStyle := bsNone;
  FSearchList.FilterCondition.AutoSelect := true;
  inherited;
  FSearchList.OnClick := OnDropDownControlClick;
  FItemIndex := -1;
end;

procedure TAdvSearchDropDown.CreateDropDownForm;
begin
  inherited;

  if Assigned(FSearchList) then
    FSearchList.Parent := FDropDownForm;

  Control := FSearchList;

  FDropDownForm.CancelOnDeActivate := False;
end;

function TAdvSearchDropDown.CreateSearchList: TAdvSearchList;
begin
  Result := TAdvSearchList.Create(Self);
end;

destructor TAdvSearchDropDown.Destroy;
begin
  FSearchList.Free;
  inherited;
end;

procedure TAdvSearchDropDown.DoShowDropDown;
begin
  inherited;
  if not FKeyDropDown then
  begin
    {$IFDEF DELPHIXE6_LVL}
    FSearchList.StyleElements := StyleElements;
    TSearchListEx(FSearchList).InitVCLStyle(true);
    {$ENDIF}
    FSearchList.FilterCondition.Text := '';
    FSearchList.UpdateFilter;
  end;
end;

procedure TAdvSearchDropDown.KeyUp(var Key: Word; Shift: TShiftState);
var
  doshow: boolean;
begin
  inherited;

  if (Key = VK_DOWN) then // <--- Keyboard down should open the popup list
  begin
    if not DroppedDown then
      ShowDropDown;
    Exit;
  end;

  if (Key < Ord('0')) and (Key <> VK_BACK) then
    Exit;

  doshow := (FOldText <> Text) and not ((Text = '') and (FOldText <> ''));

  FOldText := Text;

  FItemIndex := -1;
  FSearchList.FilterCondition.Text := Text;
  FSearchList.UpdateFilter;

  if Assigned(OnFiltered) then
    OnFiltered(Self);

  if doshow and (FSearchList.ItemCount > 0) and not DroppedDown and not (Key in [VK_RETURN, VK_ESCAPE]) then
  begin
    FKeyDropDown := true;
    ShowDropDown;
    FKeyDropDown := false;
  end;

end;

procedure TAdvSearchDropDown.OnDropDownControlClick(Sender: TObject);
var
  sli: TSearchListItem;
  NewValue: string;
begin
  sli := FSearchList.SelectedItem;
  if Assigned(sli) then
  begin
    if sli.Columns.Count > FSearchList.FilterCondition.Column then
    begin
      NewValue := sli.Columns[FSearchList.FilterCondition.Column].Caption;

      if Assigned(OnSelect) then
        OnSelect(Self, NewValue);

      //if NewValue <> Text then
      begin
        UpdateIndex;
        Text := NewValue;
        SelStart := 0;
        SelLength := Length(Text);
        HideDropDown(false);
      end;
    end;
  end;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyDown(var Key: Word;
  Shift: TShiftState);
var
  sli: TSearchListItem;
  s: string;
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_RETURN, VK_ESCAPE]) or (ssAlt in Shift) then
    inherited;

  if (Key = VK_RETURN) or (Key = VK_TAB) then
  begin
    sli := FSearchList.SelectedItem;

    if Assigned(sli) then
    begin
      if FSearchList.FilterCondition.Column < sli.Columns.Count  then
      begin
        s := sli.Columns[FSearchList.FilterCondition.Column].Caption;

        if Assigned(OnSelect) then
          OnSelect(Self, S);

        UpdateIndex;

        Text := s;
      end;
      SelectAll;
      inherited;
    end;
  end;

  if (Key = VK_TAB) then
  begin
    Key := 0;
    HideDropDown(false);
  end;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyPress(var Key: Char);
begin
  if Key = #9 then
    Key := #0;

  inherited;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_ESCAPE]) or (ssAlt in Shift) then
    inherited;

  if Key = VK_ESCAPE then
  begin
    HideDropDown(True);
  end;
end;

procedure TAdvSearchDropDown.ShowDropDown;
begin
  inherited DoShowDropDown;
end;

procedure TAdvSearchDropDown.UpdateIndex;
var
  sli: TSearchListItem;
begin
  sli := FSearchList.SelectedItem;
  if Assigned(sli) then
    FItemIndex := sli.Index
  else
    FItemIndex := -1;
end;

procedure TAdvSearchDropDown.WMKeyDown(var Msg: TWMKeydown);
var
  IsAlt: Boolean;
begin
  inherited;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);

  if Enabled and not IsAlt then
  begin
    case Msg.CharCode of
    VK_DOWN:
      begin
        FSearchList.SelectNextItem(FSearchList.ItemIndex);
        UpdateIndex;
      end;
    VK_UP:
      begin
        FSearchList.SelectPreviousItem(FSearchList.ItemIndex);
        UpdateIndex;
      end;
    end;
  end;
end;

{ TCategoryButton }

procedure TSearchEditButton.Assign(Source: TPersistent);
begin
  if (Source is TSearchEditButton) then
  begin
    FWidth := (Source as TSearchEditButton).Width;
    FVisible := (Source as TSearchEditButton).Visible;
    FCaption := (Source as TSearchEditButton).Caption;
    FPicture.Assign((Source as TSearchEditButton).Picture);
  end;
end;

procedure TSearchEditButton.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TSearchEditButton.Create(Aowner: TPersistent);
begin
  inherited Create;
  FWidth := 24;
  FVisible := true;
  FPicture := TGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FAppearance := TGlowButtonAppearance.Create;
  FOwner := AOwner;
end;

destructor TSearchEditButton.Destroy;
begin
  FAppearance.Free;
  FPicture.Free;
  inherited;
end;

function TSearchEditButton.GetAppearance: TGlowButtonAppearance;
begin
  if Assigned(OnGetAppearance) then
    OnGetAppearance(Self, Result)
  else
   Result := FAppearance;
end;

function TSearchEditButton.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSearchEditButton.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TSearchEditButton.SetAppearance(const Value: TGlowButtonAppearance);
begin
  if Assigned(OnSetAppearance) then
    OnSetAppearance(Self, Value)
  else
    FAppearance.Assign(Value);
end;

procedure TSearchEditButton.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetPicture(const Value: TGDIPPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

procedure TSearchEditButton.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetWidth(const Value: integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TSearchEditPopupButton }

procedure TSearchEditPopupButton.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TSearchEditPopupButton) then
  begin
    FPopupType := (Source as TSearchEditPopupButton).PopupType;
  end;

end;

constructor TSearchEditPopupButton.Create(AOwner: TPersistent);
begin
  inherited;
  FPopupType := pmCheck;
end;

{ TAdvSearchComboBox }

constructor TAdvSearchComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  CategoryButton.Visible := false;
  SearchButton.Visible := false;
  DropDownHeader.Visible := false;
  DropDownFooter.Visible := false;
  DropDownSizable := false;
  Width := 145;
  DropDownWidth := Width;
end;

destructor TAdvSearchComboBox.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TAdvSearchComboBox.DoDropUp(Sender: TObject; Cancelled: Boolean);
begin
  inherited;
  if Assigned(OnCloseUp) then
    OnCloseUp(Self);
end;

function TAdvSearchComboBox.GetTextHint: string;
begin
  Result := EmptyText;
end;

procedure TAdvSearchComboBox.ItemsChanged(Sender: TObject);
var
  i: integer;
  si: TSearchListItem;
begin
  inherited items.Clear;

  for i := 0 to FItems.Count - 1 do
  begin
    si := inherited Items.Add;
    si.Captions[0] := FItems[i];
    si.&Object := FItems.Objects[i];
  end;
end;

procedure TAdvSearchComboBox.Resize;
begin
  inherited;
  DropDownWidth := Width;
end;

procedure TAdvSearchComboBox.SetItems(AItems: TStrings);
begin
  FItems.Assign(AItems);
end;

procedure TAdvSearchComboBox.SetSorted(const Value: boolean);
begin
  FSorted := Value;
  if FSorted then
  begin
    (FItems as TStringList).Sort;
  end;
  (FItems as TStringList).Sorted := FSorted;
  ItemsChanged(Self);
end;

procedure TAdvSearchComboBox.SetTextHint(const Value: string);
begin
  EmptyText := Value;
end;

end.
