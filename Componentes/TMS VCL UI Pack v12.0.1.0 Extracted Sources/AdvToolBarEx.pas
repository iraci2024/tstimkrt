{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2022                               }
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

unit AdvToolBarEx;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF VCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvGraphics, AdvPersistence, AdvGraphicsStyles, AdvCustomControl, Controls, Types, AdvBitmapSelectorEx,
  AdvPopupEx, AdvTypes, PictureContainer, StdCtrls, AdvColorSelectorEx,
  {%H-}AdvToolBarResEx, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  ,UITypes, Generics.Collections, Generics.Defaults
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.ListBox, VCL.Menus, FMX.Types, FMX.Edit
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFNDEF WEBLIB}
  ,Menus
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,Contnrs, WEBLib.Menus
  {$ENDIF}
  ,ExtCtrls
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : DisabledBitmaps property
  // v1.0.1.1 : Fixed : Issue with DropDownButton on VCL
  // v1.0.1.2 : Fixed : Issue with bounds loop on LCL
  // v1.0.1.3 : Fixed : Issue with Tag property being overriden
  // v1.0.1.4 : Fixed : Issue with PictureContainer not being given to internal TAdvBitmapSelectorEx in TAdvToolBarExBitmapPicker
  // v1.0.1.5 : Improved: Added onAnchorClick for HTML text in the TAdvToolBar
  // v1.0.1.6 : Fixed : Issue updating Visible property triggering multiple unnecessary UpdateToolBar calls
  //          : Improved : IsLoading check expanded checking for loading subcontrols
  // v1.0.1.7 : Fixed : Issue in Delphi 11 with begin and end scene for CreateBitmapCanvas
  //          : Fixed : Issue with compact toolbars not getting hidden in combination with TAdvRibbon
  // v1.0.2.0 : Improved : Support for Delphi 11 high dpi handling added
  // v1.0.2.1 : Fixed : Issue with realignment causing extreme slowness at designtime in FMX.
  // v1.0.2.2 : Fixed : Issue with aligning buttons when parenting

  {$IFDEF WEBLIB}
  AdvTOOLBAREXPANDSVG = 'data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdo'
                +'dD0iMTIiIHdpZHRoPSIxMCI+DQogIDxwYXRoIGQ9Ik0gMiA0IEwgNSA3IEwgOCA0IFoiIHN0eWxlPSJzdHJva2U6YmxhY2s7'
                +'c3Ryb2tlLXdpZHRoOjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgLz4NCjwvc3ZnPg==';
  {$ENDIF}

type
  TAdvCustomToolBarEx = class;
  TAdvCustomDockPanelEx = class;
  TAdvCustomToolBarExSeparator = class;
  TAdvDefaultToolBarExButton = class;
  TAdvToolBarExButton = class;
  TAdvToolBarExDropDownButton = class;

  TAdvToolBarExCustomButtonAppearance = class(TPersistent)
  private
    FOwner: TAdvDefaultToolBarExButton;
    FHoverStroke: TAdvGraphicsStroke;
    FDownFill: TAdvGraphicsFill;
    FNormalFill: TAdvGraphicsFill;
    FDownStroke: TAdvGraphicsStroke;
    FDisabledFill: TAdvGraphicsFill;
    FNormalStroke: TAdvGraphicsStroke;
    FDisabledStroke: TAdvGraphicsStroke;
    FHoverFill: TAdvGraphicsFill;
    FCorners: TAdvGraphicsCorners;
    FRounding: Single;
    FTransparent: Boolean;
    FShowInnerStroke: Boolean;
    FInnerStroke: TAdvGraphicsStroke;
    FFlatStyle: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetDisabledFill(const Value: TAdvGraphicsFill);
    procedure SetDisabledStroke(const Value: TAdvGraphicsStroke);
    procedure SetDownFill(const Value: TAdvGraphicsFill);
    procedure SetDownStroke(const Value: TAdvGraphicsStroke);
    procedure SetHoverFill(const Value: TAdvGraphicsFill);
    procedure SetHoverStroke(const Value: TAdvGraphicsStroke);
    procedure SetNormalFill(const Value: TAdvGraphicsFill);
    procedure SetNormalStroke(const Value: TAdvGraphicsStroke);
    procedure SetCorners(const Value: TAdvGraphicsCorners); virtual;
    procedure SetRounding(const Value: Single); virtual;
    procedure SetInnerStroke(const Value: TAdvGraphicsStroke);
    procedure SetShowInnerStroke(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function IsRoundingStored: Boolean;
    procedure SetFlatStyle(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure Changed; virtual;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property ShowInnerStroke: Boolean read FShowInnerStroke write SetShowInnerStroke default True;
    property Rounding: Single read FRounding write SetRounding stored IsRoundingStored nodefault;
    property Corners: TAdvGraphicsCorners read FCorners write SetCorners default [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
    property InnerStroke: TAdvGraphicsStroke read FInnerStroke write SetInnerStroke;
    property NormalFill: TAdvGraphicsFill read FNormalFill write SetNormalFill;
    property NormalStroke: TAdvGraphicsStroke read FNormalStroke write SetNormalStroke;
    property HoverFill: TAdvGraphicsFill read FHoverFill write SetHoverFill;
    property HoverStroke: TAdvGraphicsStroke read FHoverStroke write SetHoverStroke;
    property DownFill: TAdvGraphicsFill read FDownFill write SetDownFill;
    property DownStroke: TAdvGraphicsStroke read FDownStroke write SetDownStroke;
    property DisabledFill: TAdvGraphicsFill read FDisabledFill write SetDisabledFill;
    property DisabledStroke: TAdvGraphicsStroke read FDisabledStroke write SetDisabledStroke;
    property FlatStyle: Boolean read FFlatStyle write SetFlatStyle default False;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvDefaultToolBarExButton);
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvToolBarExButtonAppearance = class(TAdvToolBarExCustomButtonAppearance)
  published
    property Transparent;
    property ShowInnerStroke;
    property Rounding;
    property Corners;
    property InnerStroke;
    property NormalFill;
    property NormalStroke;
    property HoverFill;
    property HoverStroke;
    property DownFill;
    property DownStroke;
    property DisabledFill;
    property DisabledStroke;
    property FlatStyle;
  end;

  TAdvToolBarExCompactAppearance = class(TAdvToolBarExCustomButtonAppearance)
  published
    property NormalFill;
    property NormalStroke;
    property HoverFill;
    property HoverStroke;
    property DownFill;
    property DownStroke;
    property DisabledFill;
    property DisabledStroke;
    property FlatStyle;
  end;

  TAdvToolBarExQuickMenuButtonAppearance = class(TAdvToolBarExCustomButtonAppearance)
  published
    property NormalFill;
    property NormalStroke;
    property HoverFill;
    property HoverStroke;
    property DownFill;
    property DownStroke;
    property DisabledFill;
    property DisabledStroke;
    property FlatStyle;
  end;

  TAdvToolBarExElementState = (esNormal, esLarge);

  TAdvCustomToolBarExElement = class(TAdvCustomControl)
  private
    FBlockUpdate: Boolean;
    FOnUpdateToolBar: TNotifyEvent;
    FOnUpdateToolBarControl: TNotifyEvent;
    FCanCopy: Boolean;
    FState: TAdvToolBarExElementState;
    FLastElement: Boolean;
    FControlIndex: Integer;
    procedure SetState(const Value: TAdvToolBarExElementState);
    procedure SetLastElement(const Value: Boolean);
  protected
    function GetDocURL: string; override;
    procedure UpdateState; virtual;
    procedure UpdateToolBar; virtual;
    procedure UpdateToolBarControl; virtual;
    {$IFDEF FMXLIB}
    procedure SetVisible(const Value: Boolean); override;
    procedure DoMatrixChanged(Sender: TObject); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure VisibleChanging; override;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    procedure DefineProperties(Filer : TFiler); override;
    procedure ReadControlIndex(Reader: TReader);
    procedure WriteControlIndex(Writer: TWriter);
    {$ENDIF}
    procedure UpdateControlAfterResize; override;
    property OnUpdateToolBar: TNotifyEvent read FOnUpdateToolBar write FOnUpdateToolBar;
    property OnUpdateToolBarControl: TNotifyEvent read FOnUpdateToolBarControl write FOnUpdateToolBarControl;
    property State: TAdvToolBarExElementState read FState write SetState default esNormal;
    property LastElement: Boolean read FLastElement write SetLastElement default False;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CanCopy: Boolean read FCanCopy write FCanCopy default True;
    property ControlIndex: Integer read FControlIndex write FControlIndex default -1;
  end;

  TAdvToolBarExButtonBitmapPosition = (bbpLeft, bbpTop);

  TAdvToolBarExButtonLayout = (bblNone, bblBitmap, bblLabel, bblLarge);

  {$IFDEF FNCLIB}
  TAdvDefaultToolBarExButton = class(TAdvCustomToolBarExElement, IAdvPictureContainer)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvDefaultToolBarExButton = class(TAdvCustomToolBarExElement)
  {$ENDIF}
  private
    FOldHeight: Single;
    FBitmapVisible: Boolean;
    FText: String;
    FPopup: TAdvPopupEx;
    FPopupPlacement: TAdvPopupExPlacement;
    FDropDownHeight: Single;
    FDropDownWidth: Single;
    FDown, FHover, FPrevHover: Boolean;
    FAppearance: TAdvToolBarExButtonAppearance;
    FPictureContainer: TPictureContainer;
    FBitmaps: TAdvScaledBitmaps;
    FApplyName: Boolean;
    FDropDownControl: TControl;
    FFont: TAdvGraphicsFont;
    FOnBeforeDropDown: TNotifyEvent;
    FCloseOnSelection: Boolean;
    FDownState: Boolean;
    FTextVisible: Boolean;
    FOnCloseDropDown: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FHorizontalTextAlign: TAdvGraphicsTextAlign;
    FWordWrapping: Boolean;
    FTrimming: TAdvGraphicsTextTrimming;
    FBitmapSize: Single;
    FStretchText: Boolean;
    FDisabledBitmaps: TAdvScaledBitmaps;
    FHoverBitmaps: TAdvScaledBitmaps;
    FShowFocus: Boolean;
    FDropDownAutoWidth: Boolean;
    FHoverFontColor: TAdvGraphicsColor;
    FDownFontColor: TAdvGraphicsColor;
    FDisabledFontColor: TAdvGraphicsColor;
    FStretchBitmapIfNoText: Boolean;
    FBitmapPosition: TAdvToolBarExButtonBitmapPosition;
    FMaximumLayout: TAdvToolBarExButtonLayout;
    FMinimumLayout: TAdvToolBarExButtonLayout;
    FLayout: TAdvToolBarExButtonLayout;
    FCompactLayout: TAdvToolBarExButtonLayout;
    FLargeLayoutBitmapSize: Single;
    FLargeLayoutBitmaps: TAdvScaledBitmaps;
    FLargeLayoutHoverBitmaps: TAdvScaledBitmaps;
    FLargeLayoutDisabledBitmaps: TAdvScaledBitmaps;
    FAutoBitmapSize: Boolean;
    FLargeLayoutAutoBitmapSize: Boolean;
    procedure SetAppearance(const Value: TAdvToolBarExButtonAppearance);
    procedure SetPictureContainer(const Value: TPictureContainer); virtual;
    procedure SetBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetText(const Value: String);
    procedure SetDropDownHeight(const Value: Single);
    procedure SetDropDownWidth(const Value: Single);
    procedure SetBitmapVisible(const Value: Boolean);
    procedure SetTextVisible(const Value: Boolean);
    procedure SetFont(const Value: TAdvGraphicsFont); reintroduce;
    procedure SetDropDownControl(const Value: TControl);
    procedure SetCloseOnSelection(const Value: Boolean);
    procedure SetDownState(const Value: Boolean);
    function IsDropDownHeightStored: Boolean;
    function IsDropDownWidthStored: Boolean;
    procedure SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetTrimming(const Value: TAdvGraphicsTextTrimming);
    function IsBitmapSizeStored: Boolean;
    procedure SetBitmapSize(const Value: Single);
    procedure SetStretchText(const Value: Boolean);
    procedure SetDisabledBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetHoverBitmaps(const Value: TAdvScaledBitmaps);
    function GetPictureContainer: TPictureContainer;
    procedure SetShowFocus(const Value: Boolean);
    procedure SetDropDownAutoWidth(const Value: Boolean);
    procedure SetDisabledFontColor(const Value: TAdvGraphicsColor);
    procedure SetDownFontColor(const Value: TAdvGraphicsColor);
    procedure SetHoverFontColor(const Value: TAdvGraphicsColor);
    procedure SetStretchBitmapIfNoText(const Value: Boolean);
    procedure SetBitmapPosition(const Value: TAdvToolBarExButtonBitmapPosition);
    procedure SetMaximumLayout(const Value: TAdvToolBarExButtonLayout);
    procedure SetMinimumLayout(const Value: TAdvToolBarExButtonLayout);
    procedure SetLayout(const Value: TAdvToolBarExButtonLayout);
    procedure SetCompactLayout(const Value: TAdvToolBarExButtonLayout);
    function IsLargeLayoutBitmapSizeStored: Boolean;
    procedure SetLargeLayoutBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetLargeLayoutBitmapSize(const Value: Single);
    procedure SetLargeLayoutDisabledBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetLargeLayoutHoverBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetAutoBitmapSize(const Value: Boolean);
    procedure SetLargeLayoutAutoBitmapSize(const Value: Boolean);
  protected
    function CanDrawDesignTime: Boolean; virtual;
    function HasHint: Boolean; override;
    function GetHintString: String; override;
    function GetText: String; virtual;
    function GetTextSize: TSizeF; virtual;
    {$IFNDEF WEBLIB}
    {$IFDEF FNCLIB}
    {$IFNDEF LCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 30}
    procedure ActionChange(Sender: {$IFNDEF FMXLIB}TObject{$ELSE}TBasicAction{$ENDIF}; CheckDefaults: Boolean); override;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
    procedure UpdateControlAfterResize; override;
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure BitmapsChanged(Sender: TObject);
    procedure DoFontChanged(Sender: TObject);
    procedure UpdateLayout; virtual;
    procedure DoClosePopup(Sender: TObject);
    procedure DoPopup(Sender: TObject);
    procedure InitializePopup; virtual;
    procedure UpdateState; override;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure AppearanceChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyPress(var Key: Char); override;
    procedure HandleMouseEnter; override;
    procedure HandleMouseLeave; override;
    procedure SelectFirstValue; virtual;
    procedure SelectValueWithCharacter({%H-}ACharacter: Char); virtual;
    procedure SelectLastValue; virtual;
    procedure SelectNextValue; virtual;
    procedure SelectPreviousValue; virtual;
    procedure DrawText(AGraphics: TAdvGraphics); virtual;
    procedure DrawBitmap(AGraphics: TAdvGraphics; ARect: TRectF); virtual;
    procedure DrawButton({%H-}AGraphics: TAdvGraphics); virtual;
    function CanDropDown: Boolean; virtual;
    function CanChangeText: Boolean; virtual;
    function GetTextRect: TRectF; virtual;
    function GetDropDownRect: TRectF; virtual;
    function GetBitmapRect: TRectF; overload; virtual;
    function GetBitmapRect(ARect: TRectF): TRectF; overload; virtual;
    function GetBitmapSize: Single; virtual;
    function GetLargeLayoutBitmapSize: Single; virtual;
    property Appearance: TAdvToolBarExButtonAppearance read FAppearance write SetAppearance;
    property BitmapVisible: Boolean read FBitmapVisible write SetBitmapVisible default False;
    property Bitmaps: TAdvScaledBitmaps read FBitmaps write SetBitmaps;
    property DisabledBitmaps: TAdvScaledBitmaps read FDisabledBitmaps write SetDisabledBitmaps;
    property HoverBitmaps: TAdvScaledBitmaps read FHoverBitmaps write SetHoverBitmaps;
    property LargeLayoutBitmaps: TAdvScaledBitmaps read FLargeLayoutBitmaps write SetLargeLayoutBitmaps;
    property LargeLayoutDisabledBitmaps: TAdvScaledBitmaps read FLargeLayoutDisabledBitmaps write SetLargeLayoutDisabledBitmaps;
    property LargeLayoutHoverBitmaps: TAdvScaledBitmaps read FLargeLayoutHoverBitmaps write SetLargeLayoutHoverBitmaps;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property BitmapSize: Single read FBitmapSize write SetBitmapSize stored IsBitmapSizeStored nodefault;
    property AutoBitmapSize: Boolean read FAutoBitmapSize write SetAutoBitmapSize default false;
    property LargeLayoutBitmapSize: Single read FLargeLayoutBitmapSize write SetLargeLayoutBitmapSize stored IsLargeLayoutBitmapSizeStored nodefault;
    property LargeLayoutAutoBitmapSize: Boolean read FLargeLayoutAutoBitmapSize write SetLargeLayoutAutoBitmapSize default False;
    property StretchBitmapIfNoText: Boolean read FStretchBitmapIfNoText write SetStretchBitmapIfNoText default True;
    property Text: String read GetText write SetText;
    property Trimming: TAdvGraphicsTextTrimming read FTrimming write SetTrimming default gttCharacter;
    property HorizontalTextAlign: TAdvGraphicsTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default gtaCenter;
    property VerticalTextAlign: TAdvGraphicsTextAlign read FVerticalTextAlign write SetVerticalTextAlign default gtaCenter;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property HoverFontColor: TAdvGraphicsColor read FHoverFontColor write SetHoverFontColor default gcNull;
    property DownFontColor: TAdvGraphicsColor read FDownFontColor write SetDownFontColor default gcNull;
    property DisabledFontColor: TAdvGraphicsColor read FDisabledFontColor write SetDisabledFontColor default gcNull;
    property TextVisible: Boolean read FTextVisible write SetTextVisible default True;
    property StretchText: Boolean read FStretchText write SetStretchText default False;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default False;
    property DropDownAutoWidth: Boolean read FDropDownAutoWidth write SetDropDownAutoWidth default False;
    property DropDownHeight: Single read FDropDownHeight write SetDropDownHeight stored IsDropDownHeightStored nodefault;
    property DropDownWidth: Single read FDropDownWidth write SetDropDownWidth stored IsDropDownWidthStored nodefault;
    property DropDownControl: TControl read FDropDownControl write SetDropDownControl;
    property CloseOnSelection: Boolean read FCloseOnSelection write SetCloseOnSelection default True;
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseDropDown: TNotifyEvent read FOnCloseDropDown write FOnCloseDropDown;
    property ApplyName: Boolean read FApplyName write FApplyName default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property BitmapPosition: TAdvToolBarExButtonBitmapPosition read FBitmapPosition write SetBitmapPosition default bbpLeft;
    property CompactLayout: TAdvToolBarExButtonLayout read FCompactLayout write SetCompactLayout default bblNone;
    property Layout: TAdvToolBarExButtonLayout read FLayout write SetLayout default bblNone;
    property MinimumLayout: TAdvToolBarExButtonLayout read FMinimumLayout write SetMinimumLayout default bblBitmap;
    property MaximumLayout: TAdvToolBarExButtonLayout read FMaximumLayout write SetMaximumLayout default bblLarge;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure DropDown; virtual;
    procedure CloseDropDown; virtual;
    function GetPopupControl: TAdvPopupEx;
    function GetBitmap: TAdvBitmap; virtual;
    property DownState: Boolean read FDownState write SetDownState;
    property PopupPlacement: TAdvPopupExPlacement read FPopupPlacement write FPopupPlacement default ppBottom;
  end;

  TAdvToolBarExButtonDropDownKind = (ddkNormal, ddkDropDown, ddkDropDownButton);
  TAdvToolBarExButtonDropDownPosition = (ddpRight, ddpBottom);

  TAdvToolBarExDropDownButton = class(TAdvDefaultToolBarExButton)
  private
    FDefaultStyle: Boolean;
    procedure SetDefaultStyle(const Value: Boolean);
  protected
    procedure RegisterRuntimeClasses; override;
    procedure DrawBitmap(AGraphics: TAdvGraphics; ARect: TRectF); override;
    function GetBitmapRect(ARect: TRectF): TRectF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadSettingsFromFile(AFileName: string); override;
    procedure LoadSettingsFromStream(AStream: TStreamEx); override;
  published
    property DefaultStyle: Boolean read FDefaultStyle write SetDefaultStyle default True;
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 30}
    property Action;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$ENDIF}
    property Font;
    property Text;
    property ShowFocus;
    property TextVisible;
    property StretchText;
    property WordWrapping;
    property HorizontalTextAlign;
    property VerticalTextAlign;
    property Trimming;
    property Bitmaps;
    property DisabledBitmaps;
    property HoverBitmaps;
    property BitmapVisible;
    property PictureContainer;
    property State;
    property LastElement;
    property Appearance;
  end;

  TAdvToolBarExDropDownButtonClass = class of TAdvToolBarExDropDownButton;


  TAdvCustomToolBarExButton = class(TAdvDefaultToolBarExButton)
  private
    FDropDownButton: TAdvToolBarExDropDownButton;
    FDropDownKind: TAdvToolBarExButtonDropDownKind;
    FDropDownPosition: TAdvToolBarExButtonDropDownPosition;
    FHidden: Boolean;
    FAutoOptionsMenuText: String;
    procedure SetDropDownKind(const Value: TAdvToolBarExButtonDropDownKind);
    procedure SetDropDownPosition(
      const Value: TAdvToolBarExButtonDropDownPosition);
    procedure SetHidden(const Value: Boolean);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure DoFontChanged(Sender: TObject);
    procedure AppearanceChanged; override;
    procedure UpdateDropDownButton; virtual;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DropDownButtonClick(Sender: TObject);
    procedure DrawButton(AGraphics: TAdvGraphics); override;
    {$IFDEF FMXLIB}
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure SetEnabled(Value: Boolean); override;
    {$ENDIF}
    function GetDropDownButtonClass: TAdvToolBarExDropDownButtonClass; virtual;
    function GetDropDownRect: TRectF; override;
    function GetTextRect: TRectF; override;
    function GetBitmapRect(ARect: TRectF): TRectF; overload; override;
    function GetBitmapRect: TRectF; overload; override;
    function CanDropDown: Boolean; override;
    property DropDownKind: TAdvToolBarExButtonDropDownKind read FDropDownKind write SetDropDownKind default ddkNormal;
    property DropDownPosition: TAdvToolBarExButtonDropDownPosition read FDropDownPosition write SetDropDownPosition default ddpRight;
    property AutoOptionsMenuText: String read FAutoOptionsMenuText write FAutoOptionsMenuText;
    property Hidden: Boolean read FHidden write SetHidden default False;
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDropDownButtonControl: TAdvToolBarExDropDownButton;
    procedure LoadSettingsFromFile(AFileName: string); override;
    procedure LoadSettingsFromStream(AStream: TStreamEx); override;
  end;

  TAdvCustomToolBarExSeparatorAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomToolBarExSeparator;
    FStroke: TAdvGraphicsStroke;
    FInnerStroke: TAdvGraphicsStroke;
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetInnerStroke(const Value: TAdvGraphicsStroke);
  protected
    procedure StrokeChanged(Sender: TObject);
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property InnerStroke: TAdvGraphicsStroke read FInnerStroke write SetInnerStroke;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvCustomToolBarExSeparator);
    destructor Destroy; override;
  end;

  TAdvToolBarExSeparatorAppearance = class(TAdvCustomToolBarExSeparatorAppearance)
  published
    property Stroke;
    property InnerStroke;
  end;

  TAdvCustomToolBarExSeparator = class(TAdvCustomToolBarExElement)
  private
    FAppearance: TAdvToolBarExSeparatorAppearance;
    procedure SetAppearance(const Value: TAdvToolBarExSeparatorAppearance);
  protected
    procedure UpdateState; override;
    property Appearance: TAdvToolBarExSeparatorAppearance read FAppearance write SetAppearance;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExButton = class(TAdvCustomToolBarExButton)
  protected
    procedure RegisterRuntimeClasses; override;
  public
    property Hidden;
  published
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 30}
    property Action;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$ENDIF}
    property Font;
    property AutoOptionsMenuText;
    property DropDownAutoWidth;
    property DropDownKind;
    property DropDownPosition;
    property DropDownHeight;
    property DropDownWidth;
    property DropDownControl;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnCloseDropDown;
    property Text;
    property ShowFocus;
    property TextVisible;
    property StretchText;
    property StretchBitmapIfNoText;
    property HorizontalTextAlign;
    property VerticalTextAlign;
    property Trimming;
    property Bitmaps;
    property LargeLayoutBitmaps;
    property Layout;
    property CompactLayout;
    property MinimumLayout;
    property MaximumLayout;
    property BitmapPosition;
    property DisabledBitmaps;
    property HoverBitmaps;
    property LargeLayoutDisabledBitmaps;
    property LargeLayoutHoverBitmaps;
    property BitmapVisible;
    property PictureContainer;
    property BitmapSize;
    property AutoBitmapSize;
    property LargeLayoutBitmapSize;
    property LargeLayoutAutoBitmapSize;
    property State;
    property LastElement;
    property Appearance;
    property WordWrapping;
  end;

  TAdvToolBarExButtonClass = class of TAdvToolBarExButton;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExSeparator = class(TAdvCustomToolBarExSeparator)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property State;
    property LastElement;
    property Appearance;
  end;

  TAdvToolBarExSeparatorClass = class of TAdvToolBarExSeparator;

  TAdvToolBarExItemPickerItemSelected = procedure(Sender: TObject; AItemIndex: Integer) of object;

  TAdvToolBarExCustomItemPicker = class(TAdvToolBarExButton)
  private
    FEdit: TEdit;
    FTimer: TTimer;
    FItemIndex: Integer;
    FItems: TStringList;
    FKeyboardUsed: Boolean;
    FItemSelector: TListBox;
    FOnItemSelected: TAdvToolBarExItemPickerItemSelected;
    FEditable: Boolean;
    FOnEditChange: TNotifyEvent;
    function GetSelectedItemIndex: Integer;
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetSelectedItem: String;
    procedure SetSelectedItem(const Value: String);
    procedure SetItems(const Value: TStringList);
    procedure SetEditable(const Value: Boolean);
  protected
    procedure ChangeDPIScale(M, D: Integer); override;
    function CanChangeText: Boolean; override;
    function GetText: String; override;
    procedure EnterTimerChanged(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure HandleDialogKey(var Key: Word; Shift: TShiftState); override;
    procedure InitializePopup; override;
    procedure SelectValueWithString(AValue: string); virtual;
    procedure SelectValueWithCharacter(ACharacter: Char); override;
    procedure SelectFirstValue; override;
    procedure SelectLastValue; override;
    procedure SelectNextValue; override;
    procedure SelectPreviousValue; override;
    {$IFDEF FMXLIB}
    procedure ItemKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure ItemKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure ItemSelected(const Sender: TCustomListBox; const Item: TListBoxItem);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure ItemKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure ItemKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure ItemSelected(Sender: TObject);
    {$ENDIF}
    procedure DoItemSelected; virtual;
    procedure DoEnter; override;
    property CloseOnSelection;
    property Items: TStringList read FItems write SetItems;
    property OnItemSelected: TAdvToolBarExItemPickerItemSelected read FOnItemSelected write FOnItemSelected;
    property ListBox: TListBox read FItemSelector;
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex default -1;
    property SelectedItem: String read GetSelectedItem write SetSelectedItem;
    property Editable: Boolean read FEditable write SetEditable default False;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    destructor Destroy; override;
    procedure DropDown; override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExItemPicker = class(TAdvToolBarExCustomItemPicker)
  published
    property OnItemSelected;
    property SelectedItemIndex;
    property Items;
    property Editable;
    property OnEditChange;
  end;

  TAdvToolBarExFontNamePickerFontNameSelected = procedure(Sender: TObject; AFontName: String) of object;

  TAdvToolBarExItemPickerClass = class of TAdvToolBarExItemPicker;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExFontNamePicker = class(TAdvToolBarExCustomItemPicker)
  private
    FOnFontNameSelected: TAdvToolBarExFontNamePickerFontNameSelected;
    function GetSelectedFontName: String;
    procedure SetSelectedFontName(const Value: String);
  protected
    procedure RegisterRuntimeClasses; override;
    procedure DoItemSelected; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedFontName: String read GetSelectedFontName write SetSelectedFontName;
    property SelectedItemIndex;
    property Items;
  published
    property OnFontNameSelected: TAdvToolBarExFontNamePickerFontNameSelected read FOnFontNameSelected write FOnFontNameSelected;
    property CloseOnSelection;
    property Editable;
    property OnEditChange;
  end;

  TAdvToolBarExFontSizePickerFontSizeSelected = procedure(Sender: TObject; AFontSize: Single) of object;

  TAdvToolBarExFontNamePickerClass = class of TAdvToolBarExFontNamePicker;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExFontSizePicker = class(TAdvToolBarExCustomItemPicker)
  private
    FOnFontSizeSelected: TAdvToolBarExFontSizePickerFontSizeSelected;
    function GetSelectedFontSize: Single;
    procedure SetSelectedFontSize(const Value: Single);
  protected
    procedure RegisterRuntimeClasses; override;
    procedure DoItemSelected; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedFontSize: Single read GetSelectedFontSize write SetSelectedFontSize;
    property SelectedItemIndex;
    property Items;
  published
    property OnFontSizeSelected: TAdvToolBarExFontSizePickerFontSizeSelected read FOnFontSizeSelected write FOnFontSizeSelected;
    property CloseOnSelection;
    property Editable;
    property OnEditChange;
  end;

  TAdvToolBarExColorPickerColorSelected = procedure(Sender: TObject; AColor: TAdvGraphicsColor) of object;

  TAdvToolBarExFontSizePickerClass = class of TAdvToolBarExFontSizePicker;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExColorPicker = class(TAdvToolBarExButton)
  private
    FColorSelector: TAdvColorSelectorEx;
    FOnColorSelected: TAdvToolBarExColorPickerColorSelected;
    function GetSelectedColor: TAdvGraphicsColor;
    procedure SetSelectedColor(const Value: TAdvGraphicsColor);
    function GetSelectedItemIndex: Integer;
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetItems: TAdvColorSelectorExItems;
    procedure SetItems(const Value: TAdvColorSelectorExItems);
  protected
    procedure RegisterRuntimeClasses; override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure DrawColor(AColor: TAdvGraphicsColor; ARect: TRectF; AGraphics: TAdvGraphics);
    procedure ColorSelected(Sender: TObject; AColor: TAdvGraphicsColor);
    procedure DoColorSelected(AColor: TAdvGraphicsColor); virtual;
    function CanChangeText: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function BlockChange: Boolean;
    function ColorSelector: TAdvColorSelectorEx;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure DrawSelectedColor(AGraphics: TAdvGraphics; {%H-}ARect: TRectF); virtual;
  published
    property OnColorSelected: TAdvToolBarExColorPickerColorSelected read FOnColorSelected write FOnColorSelected;
    property SelectedColor: TAdvGraphicsColor read GetSelectedColor write SetSelectedColor default gcNull;
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex;
    property Items: TAdvColorSelectorExItems read GetItems write SetItems;
    property CloseOnSelection;
  end;

  TAdvToolBarExBitmapPickerBitmapSelected = procedure(Sender: TObject; ABitmap: TAdvBitmap) of object;

  TAdvToolBarExColorPickerClass = class of TAdvToolBarExColorPicker;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarExBitmapPicker = class(TAdvToolBarExButton)
  private
    FBitmapSelector: TAdvBitmapSelectorEx;
    FOnBitmapSelected: TAdvToolBarExBitmapPickerBitmapSelected;
    function GetSelectedBitmap: TAdvBitmap;
    function GetSelectedItemIndex: Integer;
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetItems: TAdvBitmapSelectorExItems;
    procedure SetItems(const Value: TAdvBitmapSelectorExItems);
    procedure SetPictureContainer(const Value: TPictureContainer); override;
  protected
    function GetBitmapRect(ARect: TRectF): TRectF; override;
    function CanChangeText: Boolean; override;
    procedure RegisterRuntimeClasses; override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure BitmapSelected(Sender: TObject; ABitmap: TAdvBitmap);
    procedure DoBitmapSelected(ABitmap: TAdvBitmap); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function BlockChange: Boolean;
    function BitmapSelector: TAdvBitmapSelectorEx;
    property SelectedBitmap: TAdvBitmap read GetSelectedBitmap;
  published
    property OnBitmapSelected: TAdvToolBarExBitmapPickerBitmapSelected read FOnBitmapSelected write FOnBitmapSelected;
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex;
    property Items: TAdvBitmapSelectorExItems read GetItems write SetItems;
    property CloseOnSelection;
  end;

  TAdvToolBarExBitmapPickerClass = class of TAdvToolBarExBitmapPicker;

  TAdvCustomToolBarExAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomToolBarEx;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FVerticalSpacing: Single;
    FHorizontalSpacing: Single;
    FDragGripColor: TAdvGraphicsColor;
    FDragGrip: Boolean;
    FFlatStyle: Boolean;
    FFont: TAdvGraphicsFont;
    FSeparatorStroke: TAdvGraphicsStroke;
    FSeparator: Boolean;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetHorizontalSpacing(const Value: Single);
    procedure SetVerticalSpacing(const Value: Single);
    procedure SetDragGrip(const Value: Boolean);
    procedure SetDragGripColor(const Value: TAdvGraphicsColor);
    function IsHorizontalSpacingStored: Boolean;
    function IsVerticalSpacingStored: Boolean;
    procedure SetFlatStyle(const Value: Boolean);
    procedure SetSeparator(const Value: Boolean);
    procedure SetSeparatorStroke(const Value: TAdvGraphicsStroke);
  protected
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property HorizontalSpacing: Single read FHorizontalSpacing write SetHorizontalSpacing stored IsHorizontalSpacingStored nodefault;
    property VerticalSpacing: Single read FVerticalSpacing write SetVerticalSpacing stored IsVerticalSpacingStored nodefault;
    property DragGrip: Boolean read FDragGrip write SetDragGrip default True;
    property DragGripColor: TAdvGraphicsColor read FDragGripColor write SetDragGripColor default gcLightgray;
    property FlatStyle: Boolean read FFlatStyle write SetFlatStyle default False;
    property Separator: Boolean read FSeparator write SetSeparator default False;
    property SeparatorStroke: TAdvGraphicsStroke read FSeparatorStroke write SetSeparatorStroke;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvCustomToolBarEx);
    destructor Destroy; override;
  end;

  TAdvToolBarExAppearance = class(TAdvCustomToolBarExAppearance)
  published
    property FlatStyle;
    property Fill;
    property Stroke;
    property Separator;
    property SeparatorStroke;
    property HorizontalSpacing;
    property VerticalSpacing;
    property DragGrip;
    property DragGripColor;
  end;

  TAdvToolBarExOptionMenuItem = class(TMenuItem)
  private
    FControl: TControl;
    FGlyphWidth: Single;
    FCalculate: Boolean;
  protected
    property Calculate: Boolean read FCalculate write FCalculate;
    property GlyphWidth: Single read FGlyphWidth write FGlyphWidth;
  public
    property Control: TControl read FControl write FControl;
  end;

  TAdvToolBarExControl = class(TPersistent)
  private
    FControlIndex: Integer;
    FControl: TControl;
    FLayout: TAdvToolBarExButtonLayout;
    FBitmap: TAdvBitmap;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TControlClass = class of TControl;

  TAdvToolBarExCustomizeOptionsMenu = procedure(Sender: TObject; APopupMenu: TPopupMenu) of object;
  TAdvToolBarExCustomizeOptionsMenuItem = procedure(Sender: TObject; AControl: TControl; AMenuItem: TMenuItem) of object;
  TAdvToolBarExCanShowOptionsMenuItem = procedure(Sender: TObject; AControl: TControl; var ACanShowItem: Boolean) of object;
  TAdvToolBarExOptionsMenuItemClick = procedure(Sender: TObject; AControl: TControl; AMenuItem: TMenuItem; var AExecuteDefaultAction: Boolean) of object;
  TAdvToolBarExIsLastElement = procedure(Sender: TObject; AControl: TControl; var AIsLastElement: Boolean) of object;
  TAdvToolBarExAnchorClickEvent = procedure(Sender: TObject; AAnchor: string) of object;

  TAdvCustomToolBarExOptionsMenu = class(TPersistent)
  private
    FOwner: TAdvCustomToolBarEx;
    FShowItemText: Boolean;
    FShowItemBitmap: Boolean;
    FShowButton: Boolean;
    FItemBitmapWidth: Single;
    FAutoItemBitmapWidth: Boolean;
    procedure SetShowButton(const Value: Boolean);
    procedure SetShowItemBitmap(const Value: Boolean);
    procedure SetShowItemText(const Value: Boolean);
    procedure SetAutoItemBitmapWidth(const Value: Boolean);
    procedure SetItemBitmapWidth(const Value: Single);
    function IsItemBitmapWidthStored: Boolean;
  protected
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property ShowItemBitmap: Boolean read FShowItemBitmap write SetShowItemBitmap default True;
    property ShowItemText: Boolean read FShowItemText write SetShowItemText default True;
    property AutoItemBitmapWidth: Boolean read FAutoItemBitmapWidth write SetAutoItemBitmapWidth default True;
    property ItemBitmapWidth: Single read FItemBitmapWidth write SetItemBitmapWidth stored IsItemBitmapWidthStored nodefault;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvCustomToolBarEx);
    destructor Destroy; override;
  end;

  TAdvToolBarExOptionsMenu = class(TAdvCustomToolBarExOptionsMenu)
  published
    property ShowButton;
    property ShowItemBitmap;
    property ShowItemText;
    property AutoItemBitmapWidth;
    property ItemBitmapWidth;
  end;

  {$IFDEF WEBLIB}
  TAdvToolBarExControlObjectList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAdvToolBarExControl;
    procedure SetItem(Index: Integer; const Value: TAdvToolBarExControl);
  public
    property Items[Index: Integer]: TAdvToolBarExControl read GetItem write SetItem; default;
  end;
  TAdvToolBarExControlList = class(TList)
  private
    function GetItem(Index: Integer): TAdvToolBarExControl;
    procedure SetItem(Index: Integer; const Value: TAdvToolBarExControl);
  public
    property Items[Index: Integer]: TAdvToolBarExControl read GetItem write SetItem; default;
  end;
  TAdvControlList = class(TList)
  private
    function GetItem(Index: Integer): TControl;
    procedure SetItem(Index: Integer; const Value: TControl);
  public
    property Items[Index: Integer]: TControl read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvToolBarExControlObjectList = TObjectList<TAdvToolBarExControl>;
  TAdvToolBarExControlList = TList<TAdvToolBarExControl>;
  TAdvControlList = TList<TControl>;
  {$ENDIF}

  TAdvToolBarExDragGripMovingEvent = procedure(Sender: TObject; DeltaX: Double; DeltaY: Double) of object;

  TAdvToolBarExPopup = class(TAdvPopupEx)
  public
    property OnClosePopup;
  end;

  {$IFDEF FNCLIB}
  TAdvCustomToolBarEx = class(TAdvCustomControl, IAdvStylesManager, IAdvPictureContainer)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvCustomToolBarEx = class(TAdvCustomControl)
  {$ENDIF}
  private
    FCompactToolbar: TAdvCustomToolBarEx;
    FCompactPopup: TAdvToolBarExPopup;
    FHover, FHoverP, FQuickMenuButtonHover, FQuickMenuButtonHoverP: Boolean;
    FDown, FQuickMenuButtonDown: Boolean;
    FOldWidth: Single;
    FOldMenuButtonState: Boolean;
    FPictureContainer: TPictureContainer;
    FInsideDrag, FDragGripMoving, FDragGripDown: Boolean;
    FDragGripDownPt: TPointF;
    FTotalOptionsMenuWidth: Single;
    {$IFNDEF LCLLIB}
    {$IFNDEF WEBLIB}
    FCompareControls: IComparer<TControl>;
    FCompareHiddenControls: IComparer<TAdvToolBarExControl>;
    {$ENDIF}
    {$ENDIF}
    FHiddenControls: TAdvToolBarExControlObjectList;
    FCompactControls: TAdvToolBarExControlObjectList;
    FBlockUpdate, FBlockResize: Boolean;
    FAutoSize: Boolean;
    FAppearance: TAdvToolBarExAppearance;
    FAutoAlign: Boolean;
    FOptionsMenuButton: TAdvToolBarExDropDownButton;
    FAutoOptionsMenu: TPopupMenu;
    FOnOptionsMenuCustomize: TAdvToolBarExCustomizeOptionsMenu;
    FOnOptionsMenuItemCustomize: TAdvToolBarExCustomizeOptionsMenuItem;
    FOnOptionsMenuButtonClick: TNotifyEvent;
    FOptionsMenu: TAdvToolBarExOptionsMenu;
    FCustomOptionsMenu: TPopupMenu;
    FOnOptionsMenuItemCanShow: TAdvToolBarExCanShowOptionsMenuItem;
    FState: TAdvToolBarExElementState;
    FOnOptionsMenuItemClick: TAdvToolBarExOptionsMenuItemClick;
    FOnOptionsMenuItemApplyStyle: TAdvToolBarExCustomizeOptionsMenuItem;
    FOnUpdateDockPanel: TNotifyEvent;
    FOnUpdateControls: TNotifyEvent;
    FOnIsLastElement: TAdvToolBarExIsLastElement;
    FOnDragGripMoving: TAdvToolBarExDragGripMovingEvent;
    FAutoMoveToolBar: Boolean;
    FOnInternalDblClick: TNotifyEvent;
    FOnInternalMouseDown: TNotifyEvent;
    FOnInternalMouseMove: TNotifyEvent;
    FOnInternalMouseUp: TNotifyEvent;
    FAutoHeight: Boolean;
    FAutoWidth: Boolean;
    FOnInternalInsertControl: TNotifyEvent;
    FAutoStretchHeight: Boolean;
    FText: string;
    FFirstLoad: Boolean;
    FFont: TAdvGraphicsFont;
    FVerticalTextAlign: TAdvGraphicsTextAlign;
    FTrimming: TAdvGraphicsTextTrimming;
    FHorizontalTextAlign: TAdvGraphicsTextAlign;
    FWordWrapping: Boolean;
    FTextVisible: Boolean;
    FMinimumWidth: Single;
    FCompact: Boolean;
    FCanCompact: Boolean;
    FCompactWidth: Single;
    FCompactAppearance: TAdvToolBarExCompactAppearance;
    FCompactBitmaps: TAdvScaledBitmaps;
    FOnCompactClick: TNotifyEvent;
    FCompactBitmapSize: Single;
    FCompactBitmapVisible: Boolean;
    FCompactExpanderBitmaps: TAdvScaledBitmaps;
    FQuickMenuButton: Boolean;
    FQuickMenuButtonAppearance: TAdvToolBarExQuickMenuButtonAppearance;
    FOnQuickMenuButtonClick: TNotifyEvent;
    FQuickMenuButtonBitmaps: TAdvScaledBitmaps;
    FQuickMenuButtonHint: string;
    FCompactAutoBitmapSize: Boolean;
    FOnAnchorClick: TAdvToolBarExAnchorClickEvent;
    procedure SetAppearance(const Value: TAdvToolBarExAppearance);
    procedure SetAS(const Value: Boolean);
    procedure SetAutoAlign(const Value: Boolean);
    procedure SetOptionsMenu(const Value: TAdvToolBarExOptionsMenu);
    procedure SetState(const Value: TAdvToolBarExElementState);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetAutoStretchHeight(const Value: Boolean);
    procedure SetText(const Value: string);
    function GetPictureContainer: TPictureContainer;
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetFont(const Value: TAdvGraphicsFont); reintroduce;
    procedure SetHorizontalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetTrimming(const Value: TAdvGraphicsTextTrimming);
    procedure SetVerticalTextAlign(const Value: TAdvGraphicsTextAlign);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetTextVisible(const Value: Boolean);
    function IsMinimumWidthStored: Boolean;
    procedure SetMinimumWidth(const Value: Single);
    procedure SetCompact(const Value: Boolean);
    procedure SetCanCompact(const Value: Boolean);
    function IsCompactWidthStored: Boolean;
    procedure SetCompactWidth(const Value: Single);
    procedure SetCompactAppearance(const Value: TAdvToolBarExCompactAppearance);
    procedure SetCompactBitmaps(const Value: TAdvScaledBitmaps);
    function IsCompactBitmapSizeStored: Boolean;
    procedure SetCompactBitmapSize(const Value: Single);
    procedure SetCompactBitmapVisible(const Value: Boolean);
    procedure SetCompactExpanderBitmaps(const Value: TAdvScaledBitmaps);
    procedure SetQuickMenuButton(const Value: Boolean);
    procedure SetQuickMenuButtonAppearance(
      const Value: TAdvToolBarExQuickMenuButtonAppearance);
    procedure SetQuickMenuButtonBitmaps(
      const Value: TAdvScaledBitmaps);
    procedure SetCompactAutoBitmapSize(const Value: Boolean);
    function GetTextRect: TRectF;
  protected
    {$IFDEF FNCLIB}
    function GetSubComponentArray: TAdvGraphicsStylesManagerComponentArray;
    {$ENDIF}
    function GetDocURL: string; override;
    function GetVersion: String; override;
    function CanDrawDesignTime: Boolean; virtual;
    function CanBuildControls: Boolean; virtual;
    function GetOptionsMenuButtonClass: TAdvToolBarExDropDownButtonClass; virtual;
    function GetBitmapPickerClass: TAdvToolBarExBitmapPickerClass; virtual;
    function GetToolBarButtonClass: TAdvToolBarExButtonClass; virtual;
    function GetColorPickerClass: TAdvToolBarExColorPickerClass; virtual;
    function GetItemPickerClass: TAdvToolBarExItemPickerClass; virtual;
    function GetFontNamePickerClass: TAdvToolBarExFontNamePickerClass; virtual;
    function GetFontSizePickerClass: TAdvToolBarExFontSizePickerClass; virtual;
    function GetToolBarSeparatorClass: TAdvToolBarExSeparatorClass; virtual;
    function GetTextSize: TSizeF; virtual;
    function GetHiddenControl(AControl: TControl): TAdvToolBarExControl; virtual;
    function GetCompactControl(AControl: TControl): TAdvToolBarExControl; virtual;
    function GetHiddenControlCount(AControl: TControl): Integer; virtual;
    function GetCompactControlCount: Integer; virtual;
    function GetCompactTextRect: TRectF; virtual;
    function GetCompactBitmapRect(ARect: TRectF): TRectF; virtual;
    function GetCompactBitmapSize: Single; virtual;
    function GetQuickMenuButtonRect: TRectF; virtual;
    function HasHint: Boolean; override;
    function GetHintString: String; override;
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure HandleQuickMenuButton; virtual;
    procedure CompactAppearanceChanged(Sender: TObject);
    procedure QuickMenuButtonAppearanceChanged(Sender: TObject);
    procedure CompactBitmapsChanged(Sender: TObject);
    procedure CompactExpanderBitmapsChanged(Sender: TObject);
    procedure QuickMenuButtonBitmapsChanged(Sender: TObject);    
    procedure ApplyFlatStyle; virtual;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseLeave; override;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleCompact; virtual;
    procedure DeactivateAllPopups; virtual;
    procedure DoCompactClick; virtual;
    procedure HandleDblClick({%H-}X, {%H-}Y: Single); override;
    procedure UpdateDockPanel; virtual;
    procedure InsertToolBarControl(AControl: TControl; {%H-}AIndex: Integer);
    procedure DoDragGripMoving(ADeltaX, ADeltaY: Double); virtual;
    {$IFDEF FMXLIB}
    procedure SetVisible(const Value: Boolean); override;
    procedure DoMatrixChanged(Sender: TObject); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure VisibleChanging; override;
    {$ENDIF}
    function GetDragGripRect(AInteraction: Boolean = False): TRectF;
    procedure CompactControls; virtual;
    procedure UncompactControls; virtual;
    procedure DoCustomizeOptionsMenu; virtual;
    procedure DoCustomizeOptionsMenuItem(AControl: TControl; AMenuItem: TMenuItem); virtual;
    procedure DoCanShowOptionsMenuItem(AControl: TControl; var ACanShowItem: Boolean); virtual;
    procedure UpdateToolBar(Sender: TObject);
    procedure DoIsLastElement(AControl: TControl; var ALastElement: Boolean);
    procedure UpdateToolBarControl(Sender: TObject);
    procedure OptionMenuItemClick(Sender: TObject);
    {$IFDEF FMXLIB}
    procedure OptionMenuItemApplyStyleLookup(Sender: TObject);
    {$ENDIF}
    procedure OptionsButtonClick(Sender: TObject);
    procedure DoCloseCompactPopup(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    {$IFDEF FMXLIB}
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    {$ENDIF}

    {$IFDEF CMNWEBLIB}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    {$ENDIF}
    procedure UpdateControlAfterResize; override;
    procedure UpdateControls; virtual;
    procedure InitializeOptionsMenu; virtual;
    procedure DrawDragGrip(AGraphics: TAdvGraphics); virtual;
    procedure DrawText(AGraphics: TAdvGraphics); virtual;
    procedure DrawCompactBitmap(AGraphics: TAdvGraphics; ARect: TRectF); virtual;
    procedure DrawCompactText(AGraphics: TAdvGraphics); virtual;
    procedure DrawCompactExpander(AGraphics: TAdvGraphics); virtual;
    procedure ShowOptionsMenu(X, Y: Single); virtual;
    function XYToAnchor(AX, AY: Single): string;
    procedure DoAnchorClick(AAnchor: string); virtual;
    procedure Loaded; override;
    property BlockUpdate: Boolean read FBlockUpdate write FBlockUpdate;
    property OnCompactClick: TNotifyEvent read FOnCompactClick write FOnCompactClick;
    property OnUpdateControls: TNotifyEvent read FOnUpdateControls write FOnUpdateControls;
    property OnUpdateDockPanel: TNotifyEvent read FOnUpdateDockPanel write FOnUpdateDockPanel;
    property OnInternalMouseDown: TNotifyEvent read FOnInternalMouseDown write FOnInternalMouseDown;
    property OnInternalInsertControl: TNotifyEvent read FOnInternalInsertControl write FOnInternalInsertControl;
    property OnInternalMouseUp: TNotifyEvent read FOnInternalMouseUp write FOnInternalMouseUp;
    property OnInternalMouseMove: TNotifyEvent read FOnInternalMouseMove write FOnInternalMouseMove;
    property OnInternalDblClick: TNotifyEvent read FOnInternalDblClick write FOnInternalDblClick;
    property Version: String read GetVersion;
    property AutoSize: Boolean read FAutoSize write SetAS default True;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property AutoAlign: Boolean read FAutoAlign write SetAutoAlign default True;
    property AutoStretchHeight: Boolean read FAutoStretchHeight write SetAutoStretchHeight default False;
    property AutoMoveToolBar: Boolean read FAutoMoveToolBar write FAutoMoveToolBar default True;
    property Appearance: TAdvToolBarExAppearance read FAppearance write SetAppearance;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default False;
    property Text: string read FText write SetText;
    property TextVisible: Boolean read FTextVisible write SetTextVisible default True;
    property Trimming: TAdvGraphicsTextTrimming read FTrimming write SetTrimming default gttCharacter;
    property HorizontalTextAlign: TAdvGraphicsTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default gtaCenter;
    property VerticalTextAlign: TAdvGraphicsTextAlign read FVerticalTextAlign write SetVerticalTextAlign default gtaCenter;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property OptionsMenu: TAdvToolBarExOptionsMenu read FOptionsMenu write SetOptionsMenu;
    property OnOptionsMenuItemCanShow: TAdvToolBarExCanShowOptionsMenuItem read FOnOptionsMenuItemCanShow write FOnOptionsMenuItemCanShow;
    property OnOptionsMenuCustomize: TAdvToolBarExCustomizeOptionsMenu read FOnOptionsMenuCustomize write FOnOptionsMenuCustomize;
    property OnOptionsMenuItemCustomize: TAdvToolBarExCustomizeOptionsMenuItem read FOnOptionsMenuItemCustomize write FOnOptionsMenuItemCustomize;
    property OnOptionsMenuItemApplyStyle: TAdvToolBarExCustomizeOptionsMenuItem read FOnOptionsMenuItemApplyStyle write FOnOptionsMenuItemApplyStyle;
    property OnOptionsMenuButtonClick: TNotifyEvent read FOnOptionsMenuButtonClick write FOnOptionsMenuButtonClick;
    property OnOptionsMenuItemClick: TAdvToolBarExOptionsMenuItemClick read FOnOptionsMenuItemClick write FOnOptionsMenuItemClick;
    property CustomOptionsMenu: TPopupMenu read FCustomOptionsMenu write FCustomOptionsMenu;
    property State: TAdvToolBarExElementState read FState write SetState default esNormal;
    property OnIsLastElement: TAdvToolBarExIsLastElement read FOnIsLastElement write FOnIsLastElement;
    property OnDragGripMoving: TAdvToolBarExDragGripMovingEvent read FOnDragGripMoving write FOnDragGripMoving;
    property AutoOptionsMenu: TPopupMenu read FAutoOptionsMenu;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property MinimumWidth: Single read FMinimumWidth write SetMinimumWidth stored IsMinimumWidthStored nodefault;
    property CompactBitmapVisible: Boolean read FCompactBitmapVisible write SetCompactBitmapVisible default False;
    property CompactAutoBitmapSize: Boolean read FCompactAutoBitmapSize write SetCompactAutoBitmapSize default False;
    property CompactBitmaps: TAdvScaledBitmaps read FCompactBitmaps write SetCompactBitmaps;
    property Compact: Boolean read FCompact write SetCompact default False;
    property QuickMenuButton: Boolean read FQuickMenuButton write SetQuickMenuButton default False;
    property QuickMenuButtonHint: string read FQuickMenuButtonHint write FQuickMenuButtonHint;
    property CompactWidth: Single read FCompactWidth write SetCompactWidth stored IsCompactWidthStored nodefault;
    property CanCompact: Boolean read FCanCompact write SetCanCompact default True;
    property CompactAppearance: TAdvToolBarExCompactAppearance read FCompactAppearance write SetCompactAppearance;
    property QuickMenuButtonAppearance: TAdvToolBarExQuickMenuButtonAppearance read FQuickMenuButtonAppearance write SetQuickMenuButtonAppearance;
    property CompactBitmapSize: Single read FCompactBitmapSize write SetCompactBitmapSize stored IsCompactBitmapSizeStored nodefault;
    property CompactExpanderBitmaps: TAdvScaledBitmaps read FCompactExpanderBitmaps write SetCompactExpanderBitmaps;
    property QuickMenuButtonBitmaps: TAdvScaledBitmaps read FQuickMenuButtonBitmaps write SetQuickMenuButtonBitmaps;    
    property OnQuickMenuButtonClick: TNotifyEvent read FOnQuickMenuButtonClick write FOnQuickMenuButtonClick;
    property OnAnchorClick: TAdvToolBarExAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build; virtual;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
    function CanExpand: Boolean; virtual;
    function CanShrink: Boolean; virtual;
    function DropDownActive: Boolean; virtual;
    procedure CloseCompactPopup; virtual;
    procedure AddCustomControl(AControl: TControl; AIndex: Integer = -1); virtual;
    function AddCustomControlClass(AControlClass: TControlClass; AIndex: Integer = -1): TControl; virtual;
    function AddButton(AWidth: Single = -1; AHeight: Single = -1; AResource: String = ''; AResourceLarge: String = ''; AText: String = '';
      AIndex: Integer = -1): TAdvToolBarExButton; overload; virtual;
    function AddSeparator(AIndex: Integer = -1): TAdvToolBarExSeparator; virtual;
    function AddFontNamePicker(AIndex: Integer = -1): TAdvToolBarExFontNamePicker; virtual;
    function AddFontSizePicker(AIndex: Integer = -1): TAdvToolBarExFontSizePicker; virtual;
    function AddColorPicker(AIndex: Integer = -1): TAdvToolBarExColorPicker; virtual;
    function AddItemPicker(AIndex: Integer = -1): TAdvToolBarExItemPicker; virtual;
    function AddBitmapPicker(AIndex: Integer = -1): TAdvToolBarExBitmapPicker; virtual;
    function GetOptionsMenuButtonControl: TAdvToolBarExDropDownButton; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvToolBarEx = class(TAdvCustomToolBarEx)
  protected
    procedure RegisterRuntimeClasses; override;
  published
    property OnOptionsMenuButtonClick;
    property OnOptionsMenuItemCanShow;
    property OnOptionsMenuCustomize;
    property OnOptionsMenuItemCustomize;
    property OnOptionsMenuItemApplyStyle;
    property OnOptionsMenuItemClick;
    property OnIsLastElement;
    property OnDragGripMoving;
    property OnQuickMenuButtonClick;
    property OnAnchorClick;
    //
    property Version;
    property AutoHeight;
    property AutoWidth;
    property AutoSize;
    property AutoAlign;
    property AutoStretchHeight;
    property AutoMoveToolBar;
    property Appearance;
    property CanCompact;
    property OnCompactClick;
    property CompactBitmapVisible;
    property CompactBitmaps;
    property Compact;
    property CompactWidth;
    property CompactAppearance;
    property CompactBitmapSize;
    property CompactAutoBitmapSize;
    property QuickMenuButtonHint;
    property QuickMenuButton;
    property QuickMenuButtonAppearance;
    property MinimumWidth;
    property WordWrapping;
    property Text;
    property Trimming;
    property HorizontalTextAlign;
    property VerticalTextAlign;
    property Font;
    property OptionsMenu;
    property CustomOptionsMenu;
    property State;
  end;

  TAdvCustomDockPanelExAppearance = class(TPersistent)
  private
    FOwner: TAdvCustomDockPanelEx;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FMargins: TAdvMargins;
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetMargins(const Value: TAdvMargins);
  protected
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property Margins: TAdvMargins read FMargins write SetMargins;
  public
    constructor Create(AOwner: TAdvCustomDockPanelEx);
    destructor Destroy; override;
  end;

  TAdvDockPanelExAppearance = class(TAdvCustomDockPanelExAppearance)
  published
    property Fill;
    property Stroke;
  end;

  TAdvCustomDockPanelEx = class(TAdvCustomControl)
  private
    FBlockUpdate: Boolean;
    FAppearance: TAdvDockPanelExAppearance;
    FAutoSize: Boolean;
    FAutoAlign: Boolean;
    FState: TAdvToolBarExElementState;
    procedure SetAppearance(const Value: TAdvDockPanelExAppearance);
    procedure SetAutoAlign(const Value: Boolean);
    procedure SetAS(const Value: Boolean);
    procedure SetState(const Value: TAdvToolBarExElementState);
  protected
    function GetDocURL: string; override;
    function GetVersion: String; override;
    procedure SetAdaptToStyle(const Value: Boolean); override;
    procedure UpdateControls; virtual;
    procedure InitializeControls; virtual;
    procedure RearrangeControls; virtual;
    procedure UpdateDockPanel(Sender: TObject);
    procedure ApplyStyle; override;
    procedure ResetToDefaultStyle; override;

    {$IFDEF FMXLIB}
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    {$ENDIF}

    property Version: String read GetVersion;
    property Appearance: TAdvDockPanelExAppearance read FAppearance write SetAppearance;
    property AutoSize: Boolean read FAutoSize write SetAS default True;
    property AutoAlign: Boolean read FAutoAlign write SetAutoAlign default True;
    property State: TAdvToolBarExElementState read FState write SetState default esNormal;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    function AddToolBar({%H-}AIndex: Integer = -1): TAdvToolBarEx;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvDockPanelEx = class(TAdvCustomDockPanelEx)
  published
    property Version;
    property Appearance;
    property AutoSize;
    property AutoAlign;
    property State;
  end;

  {$IFDEF CMNWEBLIB}
  TAdvToolBarExParent = TWinControl;
  {$ENDIF}
  {$IFDEF FMXLIB}
  TAdvToolBarExParent = TControl;
  {$ENDIF}

implementation

uses
  Math, SysUtils, AdvUtils
  {$IFNDEF WEBLIB}
  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 30}
  ,ActnList, ImgList
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ,Graphics, {%H-}StrUtils;

type
  TControlOpen = class(TControl);

function TAdvCustomToolBarEx.AddBitmapPicker(
  AIndex: Integer): TAdvToolBarExBitmapPicker;
begin
  FBlockUpdate := True;
  Result := GetBitmapPickerClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddButton(AWidth: Single = -1; AHeight: Single = -1;
  AResource: String = ''; AResourceLarge: String = ''; AText: String = ''; AIndex: Integer = -1): TAdvToolBarExButton;
begin
  FBlockUpdate := True;
  Result := GetToolBarButtonClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  Result.Text := AText;
  if AWidth <> -1 then
  begin
    {$IFDEF FMXLIB}
    Result.Width := AWidth;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result.Width := Round(AWidth);
    {$ENDIF}
  end;

  if AHeight <> -1 then
  begin
    {$IFDEF FMXLIB}
    Result.Height := AHeight;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    Result.Height := Round(AHeight);
    {$ENDIF}
  end;

  if AResource <> '' then
  begin
    Result.Bitmaps.AddBitmapFromResource(AResource, HInstance, 1.0);
    Result.DisabledBitmaps.AddBitmapFromResource(AResource, HInstance, 1.0);
    Result.HoverBitmaps.AddBitmapFromResource(AResource, HInstance, 1.0);
  end;

  if AResourceLarge <> '' then
  begin
    Result.Bitmaps.AddBitmapFromResource(AResourceLarge, HInstance, 1.5);
    Result.DisabledBitmaps.AddBitmapFromResource(AResourceLarge, HInstance, 1.5);
    Result.HoverBitmaps.AddBitmapFromResource(AResourceLarge, HInstance, 1.5);
  end;

  InsertToolBarControl(Result, AIndex);

  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddColorPicker(
  AIndex: Integer): TAdvToolBarExColorPicker;
begin
  FBlockUpdate := True;
  Result := GetColorPickerClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.AddCustomControl(AControl: TControl; AIndex: Integer = -1);
begin
  FBlockUpdate := True;
  InsertToolBarControl(AControl, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddCustomControlClass(AControlClass: TControlClass;
  AIndex: Integer = -1): TControl;
begin
  FBlockUpdate := True;
  Result := AControlClass.Create(Self);
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddFontNamePicker(
  AIndex: Integer): TAdvToolBarExFontNamePicker;
begin
  FBlockUpdate := True;
  Result := GetFontNamePickerClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddFontSizePicker(
  AIndex: Integer): TAdvToolBarExFontSizePicker;
begin
  FBlockUpdate := True;
  Result := GetFontSizePickerClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddItemPicker(
  AIndex: Integer): TAdvToolBarExItemPicker;
begin
  FBlockUpdate := True;
  Result := GetItemPickerClass.Create(Self);
  Result.Appearance.FlatStyle := Appearance.FlatStyle;
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.AddSeparator(AIndex: Integer = -1): TAdvToolBarExSeparator;
begin
  FBlockUpdate := True;
  Result := GetToolBarSeparatorClass.Create(Self);
  InsertToolBarControl(Result, AIndex);
  FBlockUpdate := False;
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.ApplyFlatStyle;
var
  I: Integer;
  c: TControl;
begin
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if c is TAdvDefaultToolBarExButton then
      (c as TAdvDefaultToolBarExButton).Appearance.FlatStyle := Appearance.FlatStyle;

    if c is TAdvCustomToolBarEx then
      (c as TAdvCustomToolBarEx).Appearance.FlatStyle := Appearance.FlatStyle;
  end;

  Invalidate;
end;

procedure TAdvCustomToolBarEx.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleHeaderFillColor(c) then
  begin
    Appearance.Fill.Kind := gfkSolid;
    Appearance.Fill.Color := c;
  end;

  if TAdvGraphicsStyles.GetStyleHeaderFillColorTo(c) then
  begin
    Appearance.Fill.Kind := gfkGradient;
    Appearance.Fill.ColorTo := c;
  end;
end;

procedure TAdvCustomToolBarEx.Assign(Source: TPersistent);
var
  c, cc: TControl;
  I: Integer;
begin
  inherited;
  if Source is TAdvCustomToolBarEx then
  begin
    FCompactAppearance.Assign((Source as TAdvCustomToolBarEx).CompactAppearance);
    FQuickMenuButtonAppearance.Assign((Source as TAdvCustomToolBarEx).QuickMenuButtonAppearance);
    FQuickMenuButton := (Source as TAdvCustomToolBarEx).QuickMenuButton;
    FCompactBitmaps.Assign((Source as TAdvCustomToolBarEx).CompactBitmaps);
    FCompact := (Source as TAdvCustomToolBarEx).Compact;
    FCanCompact := (Source as TAdvCustomToolBarEx).CanCompact;
    FMinimumWidth := (Source as TAdvCustomToolBarEx).MinimumWidth;
    FCompactWidth := (Source as TAdvCustomToolBarEx).CompactWidth;
    FCompactBitmapSize := (Source as TAdvCustomToolBarEx).CompactBitmapSize;
    FCompactAutoBitmapSize := (Source as TAdvCustomToolBarEx).CompactAutoBitmapSize;
    FCompactExpanderBitmaps.Assign((Source as TAdvCustomToolBarEx).CompactExpanderBitmaps);
    FQuickMenuButtonBitmaps.Assign((Source as TAdvCustomToolBarEx).QuickMenuButtonBitmaps);    
    FCompactBitmapVisible := (Source as TAdvCustomToolBarEx).CompactBitmapVisible;
    FText := (Source as TAdvCustomToolBarEx).Text;
    FTextVisible := (Source as TAdvCustomToolBarEx).TextVisible;
    FWordWrapping := (Source as TAdvCustomToolBarEx).WordWrapping;
    FHorizontalTextAlign := (Source as TAdvCustomToolBarEx).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvCustomToolBarEx).VerticalTextAlign;
    FTrimming := (Source as TAdvCustomToolBarEx).Trimming;
    FFont.Assign((Source as TAdvCustomToolBarEx).Font);
    FAutoSize := (Source as TAdvCustomToolBarEx).AutoSize;
    FAutoHeight := (Source as TAdvCustomToolBarEx).AutoHeight;
    FAutoWidth := (Source as TAdvCustomToolBarEx).AutoWidth;
    FAutoAlign := (Source as TAdvCustomToolBarEx).AutoAlign;
    FAutoStretchHeight := (Source as TAdvCustomToolBarEx).AutoStretchHeight;
    FAutoMoveToolBar := (Source as TAdvCustomToolBarEx).AutoMoveToolBar;
    FAppearance.Assign((Source as TAdvCustomToolBarEx).Appearance);
    for I := 0 to (Source as TAdvCustomToolBarEx).ControlCount - 1 do
    begin
      cc := (Source as TAdvCustomToolBarEx).Controls[I];
      if ((cc is TAdvDefaultToolBarExButton) and (cc as TAdvDefaultToolBarExButton).CanCopy) or not ((cc is TAdvDefaultToolBarExButton) and (cc is TControl)) then
      begin
        if cc is TAdvDefaultToolBarExButton then
          (cc as TAdvDefaultToolBarExButton).ApplyName := False;

        c := (TAdvUtils.Clone(cc) as TControl);
        if cc is TAdvDefaultToolBarExButton then
          (cc as TAdvDefaultToolBarExButton).ApplyName := True;

        AddCustomControl(c);
      end;
    end;
    Invalidate;
  end
  else
    inherited;
end;

procedure TAdvCustomToolBarEx.Build;
begin
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  Font.Height := TAdvUtils.MulDivInt(Font.Height, M, D);
  Appearance.HorizontalSpacing := TAdvUtils.MulDivSingle(Appearance.HorizontalSpacing, M, D);
  Appearance.VerticalSpacing := TAdvUtils.MulDivSingle(Appearance.VerticalSpacing, M, D);
  CompactWidth := TAdvUtils.MulDivSingle(CompactWidth, M, D);
  CompactBitmapSize := TAdvUtils.MulDivSingle(CompactBitmapSize, M, D);
end;

function TAdvCustomToolBarEx.CanShrink: Boolean;
var
  I: Integer;
  c: TControl;
  btn: TAdvDefaultToolBarExButton;
begin
  Result := False;

  if not Compact and CanCompact then
  begin
    Result := True;
    Exit;
  end;

  for I := Self.ControlCount - 1 downto 0 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if c <> FOptionsMenuButton then
    begin
      if c is TAdvDefaultToolBarExButton then
      begin
        btn := c as TAdvDefaultToolBarExButton;
        if (Integer(btn.Layout) > Integer(btn.MinimumLayout)) and (btn.Layout <> bblNone) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomToolBarEx.CloseCompactPopup;
begin
  if Assigned(FCompactPopup) then
    FCompactPopup.IsOpen := False;
end;

function TAdvCustomToolBarEx.CanBuildControls: Boolean;
begin
  Result := True;
end;

function TAdvCustomToolBarEx.CanDrawDesignTime: Boolean;
begin
  Result := False;
end;

function TAdvCustomToolBarEx.CanExpand: Boolean;
var
  I: Integer;
  c: TControl;
  btn: TAdvDefaultToolBarExButton;
begin
  Result := False;

  if Compact then
  begin
    Result := True;
    Exit;
  end;

  for I := Self.ControlCount - 1 downto 0 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if c <> FOptionsMenuButton then
    begin
      if c is TAdvDefaultToolBarExButton then
      begin
        btn := c as TAdvDefaultToolBarExButton;
        if (Integer(btn.Layout) < Integer(btn.MaximumLayout)) and (btn.Layout <> bblNone) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomToolBarEx.CompactAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomToolBarEx.CompactBitmapsChanged(Sender: TObject);
var
  bmp: TAdvBitmap;
begin
  bmp := TAdvGraphics.GetScaledBitmap(CompactBitmaps, 0, PictureContainer);
  CompactBitmapVisible := Assigned(bmp) and not IsBitmapEmpty(bmp);
  Invalidate;
end;

procedure TAdvCustomToolBarEx.CompactControls;
var
  I: Integer;
  cc: TAdvToolBarExControl;
  c: TControl;
begin
  FBlockUpdate := True;
  FCompactControls.Clear;
  for I := Self.ControlCount - 1 downto 0 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if c <> FOptionsMenuButton then
    begin
      cc := TAdvToolBarExControl.Create;
      cc.FControl := c;
      if c is TAdvDefaultToolBarExButton then
      begin
        cc.FLayout := (c as TAdvDefaultToolBarExButton).Layout;
        if ((c as TAdvDefaultToolBarExButton).CompactLayout = bblNone) and ((c as TAdvDefaultToolBarExButton).Layout <> bblNone) then
          (c as TAdvDefaultToolBarExButton).CompactLayout := (c as TAdvDefaultToolBarExButton).MaximumLayout;
      end;

      if c is TAdvCustomToolBarExElement then
        cc.FControlIndex := (c as TAdvCustomToolBarExElement).ControlIndex
      else
        cc.FControlIndex := c.Tag;

      c.Parent := nil;
      c.Visible := False;
      FCompactControls.Add(cc);
    end;
  end;
  FBlockUpdate := True;
  UpdateControls;
  Invalidate;
end;

procedure TAdvCustomToolBarEx.CompactExpanderBitmapsChanged(Sender: TObject);
begin
  Invalidate;
end;

constructor TAdvCustomToolBarEx.Create(AOwner: TComponent);
var
  c: TAdvGraphicsColor;
begin
  inherited;
  FFirstLoad := True;
  FHiddenControls := TAdvToolBarExControlObjectList.Create;
  FCompactControls := TAdvToolBarExControlObjectList.Create;
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  FCompareControls := TDelegatedComparer<TControl>.Create(
    function(const Item1, Item2: TControl): Integer
    var
      c1, c2: Integer;
    begin
      if Item1 is TAdvCustomToolBarExElement then
        c1 := (Item1 as TAdvCustomToolBarExElement).ControlIndex
      else
        c1 := Item1.Tag;

      if Item2 is TAdvCustomToolBarExElement then
        c2 := (Item2 as TAdvCustomToolBarExElement).ControlIndex
      else
        c2 := Item2.Tag;

      Result := CompareValue(c1, c2);
    end);
  FCompareHiddenControls := TDelegatedComparer<TAdvToolBarExControl>.Create(
    function(const Item1, Item2: TAdvToolBarExControl): Integer
    begin
      Result := CompareValue(Item1.FControlIndex, Item2.FControlIndex);
    end);
  {$ENDIF}
  {$ENDIF}
  FState := esNormal;
  FAppearance := TAdvToolBarExAppearance.Create(Self);
  FCompactAppearance := TAdvToolBarExCompactAppearance.Create(nil);
  FCompactAppearance.OnChange := CompactAppearanceChanged;
  FQuickMenuButtonAppearance := TAdvToolBarExQuickMenuButtonAppearance.Create(nil);
  FQuickMenuButtonAppearance.OnChange := QuickMenuButtonAppearanceChanged;
  FCompactBitmaps := TAdvScaledBitmaps.Create(Self);
  FCompactBitmaps.OnChange := CompactBitmapsChanged;
  FOptionsMenu := TAdvToolBarExOptionsMenu.Create(Self);
  FMinimumWidth := 30;
  FCompactWidth := 50;
  FCompactBitmapSize := 24;
  FCompactAutoBitmapSize := False;
  FCompactBitmapVisible := False;
  FCompactExpanderBitmaps := TAdvScaledBitmaps.Create(Self);
  FCompactExpanderBitmaps.OnChange := CompactExpanderBitmapsChanged;
  FQuickMenuButtonBitmaps := TAdvScaledBitmaps.Create(Self);
  FQuickMenuButtonBitmaps.OnChange := QuickMenuButtonBitmapsChanged;  
  FCanCompact := True;
  FQuickMenuButton := False;
  FCompact := False;
  FAutoSize := True;
  FAutoWidth := True;
  FAutoHeight := True;
  FAutoAlign := True;
  FAutoStretchHeight := False;
  FAutoMoveToolBar := True;
  FOptionsMenuButton := GetOptionsMenuButtonClass.Create(Self);
  FOptionsMenuButton.Appearance.NormalFill.Kind := gfkSolid;
  c := gcLightgray;
  FOptionsMenuButton.Appearance.NormalFill.Color := c;
  FOptionsMenuButton.Appearance.ShowInnerStroke := False;
  FOptionsMenuButton.Appearance.Rounding := 0;

  FOptionsMenuButton.Appearance.HoverFill.Assign(FOptionsMenuButton.Appearance.NormalFill);
  FOptionsMenuButton.Appearance.DisabledFill.Assign(FOptionsMenuButton.Appearance.NormalFill);
  FOptionsMenuButton.Appearance.DownFill.Assign(FOptionsMenuButton.Appearance.NormalFill);

  FOptionsMenuButton.Appearance.HoverFill.Color := gcLightblue;
  FOptionsMenuButton.Appearance.DownFill.Color := gcLightsteelblue;
  FOptionsMenuButton.Appearance.DisabledFill.Color := gcLightgray;

  {$IFDEF VCLWEBLIB}
  FOptionsMenuButton.AlignWithMargins := True;
  {$ENDIF}
  
  {$IFNDEF LCLLIB}
  FOptionsMenuButton.Margins.Top := 3;
  FOptionsMenuButton.Margins.Bottom := 3;
  FOptionsMenuButton.Margins.Right := 3;
  {$ENDIF}
  {$IFDEF LCLLIB}
  FOptionsMenuButton.BorderSpacing.Top := 3;
  FOptionsMenuButton.BorderSpacing.Bottom := 3;
  FOptionsMenuButton.BorderSpacing.Right := 3;
  {$ENDIF}

  FOptionsMenuButton.CanCopy := False;
  FAutoOptionsMenu := TPopupMenu.Create(Self);
  {$IFDEF FMXLIB}
  FAutoOptionsMenu.Parent := Self;
  FAutoOptionsMenu.Stored := False;
  {$ENDIF}
  FOptionsMenuButton.OnClick := OptionsButtonClick;
  FOptionsMenuButton.Width := 17;
  FOptionsMenuButton.Text := '';
  FOptionsMenuButton.Stored := False;

  FOptionsMenuButton.Bitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENU', HInstance, 1.0);
  FOptionsMenuButton.Bitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENULARGE', HInstance, 1.5);
  FOptionsMenuButton.DisabledBitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENU', HInstance, 1.0);
  FOptionsMenuButton.DisabledBitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENULARGE', HInstance,1.5);
  FOptionsMenuButton.HoverBitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENU', HInstance, 1.0);
  FOptionsMenuButton.HoverBitmaps.AddBitmapFromResource('AdvTOOLBAROPTIONSMENULARGE', HInstance,1.5);

  FHorizontalTextAlign := gtaCenter;
  FVerticalTextAlign := gtaCenter;
  FTrimming := gttCharacter;
  FWordWrapping := False;
  FTextVisible := True;

  FFont := TAdvGraphicsFont.Create;

  FCompactPopup := TAdvToolBarExPopup.Create(Self);
  FCompactPopup.OnClosePopup := DoCloseCompactPopup;

  FCompactExpanderBitmaps.AddBitmapFromResource('AdvGRAPHICSDOWN', HInstance, 1.0);
  FQuickMenuButtonBitmaps.AddBitmapFromResource('AdvTOOLBARQUICKMENU', HInstance, 1.0);  
end;

procedure TAdvCustomToolBarEx.DeactivateAllPopups;
var
  I: Integer;
begin
  if Assigned(Parent) and (Parent is TAdvToolBarExParent) then
    for I := 0 to (Parent as TAdvToolBarExParent).ControlCount - 1 do
      if (Parent as TAdvToolBarExParent).Controls[I] is TAdvCustomToolBarEx then
        ((Parent as TAdvToolBarExParent).Controls[I] as TAdvCustomToolBarEx).CloseCompactPopup;
end;

destructor TAdvCustomToolBarEx.Destroy;
begin
  FQuickMenuButtonBitmaps.Free;
  FCompactExpanderBitmaps.Free;
  FCompactPopup.Free;
  FQuickMenuButtonAppearance.Free;
  FCompactAppearance.Free;
  FCompactBitmaps.Free;
  FFont.Free;
  FOptionsMenu.Free;
  FAppearance.Free;
  FHiddenControls.Free;
  FCompactControls.Free;
  inherited;
end;

procedure TAdvCustomToolBarEx.DoAnchorClick(AAnchor: string);
begin
  if Assigned(OnAnchorClick) then
    OnAnchorClick(Self, AAnchor)
  else
    TAdvUtils.OpenURL(AAnchor);
end;

procedure TAdvCustomToolBarEx.DoCanShowOptionsMenuItem(AControl: TControl;
  var ACanShowItem: Boolean);
begin
  if Assigned(OnOptionsMenuItemCanShow) then
    OnOptionsMenuItemCanShow(Self, AControl, ACanShowItem);
end;

procedure TAdvCustomToolBarEx.DoCloseCompactPopup(Sender: TObject);
var
  I: Integer;
  cc: TAdvToolBarExControl;
begin
  FCompactPopup.ContentControl := nil;

  for I := 0 to FCompactControls.Count - 1 do
  begin
    cc := TAdvToolBarExControl(FCompactControls[I]);
    if Assigned(cc.FControl) then
    begin
      if cc.FControl is TAdvDefaultToolBarExButton then
        (cc.FControl as TAdvDefaultToolBarExButton).Layout := cc.FLayout;

      cc.FControl.Parent := nil;
      cc.FControl.Visible := False;
    end;
  end;

  if Assigned(FCompactToolbar) then
  begin
    FCompactToolbar.Free;
    FCompactToolbar := nil;
  end;
end;

procedure TAdvCustomToolBarEx.DoCompactClick;
begin
  if Assigned(OnCompactClick) then
    OnCompactClick(Self);
end;

procedure TAdvCustomToolBarEx.DoCustomizeOptionsMenu;
begin
  if Assigned(OnOptionsMenuCustomize) then
    OnOptionsMenuCustomize(Self, FAutoOptionsMenu);
end;

procedure TAdvCustomToolBarEx.DoCustomizeOptionsMenuItem(AControl: TControl; AMenuItem: TMenuItem);
begin
  if Assigned(OnOptionsMenuItemCustomize) then
    OnOptionsMenuItemCustomize(Self, AControl, AMenuItem);
end;

procedure TAdvCustomToolBarEx.DoDragGripMoving(ADeltaX, ADeltaY: Double);
begin
  if Assigned(OnDragGripMoving) then
    OnDragGripMoving(Self, ADeltaX, ADeltaY);
end;

procedure TAdvCustomToolBarEx.DoIsLastElement(AControl: TControl; var ALastElement: Boolean);
begin
  if Assigned(OnIsLastElement) then
    OnIsLastElement(Self, AControl, ALastElement);
end;

procedure TAdvCustomToolBarEx.HandleQuickMenuButton;
begin
  if Assigned(OnQuickMenuButtonClick) then
    OnQuickMenuButtonClick(Self);
end;

function TAdvCustomToolBarEx.HasHint: Boolean;
begin
  Result := inherited HasHint;
  if FQuickMenuButtonHover then
    Result := QuickMenuButtonHint <> '';
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarEx.DoMatrixChanged(Sender: TObject);
begin
  inherited;
  UpdateDockPanel;
end;
{$ENDIF}

procedure TAdvCustomToolBarEx.HandleMouseLeave;
begin
  inherited;
  FHoverP := False;
  FQuickMenuButtonHover := False;
  Cursor := crDefault;
  FInsideDrag := False;
  if FHoverP <> FHover then
  begin
    FHover := False;
    Invalidate;
  end;
end;

{$IFDEF CMNWEBLIB}
procedure TAdvCustomToolBarEx.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;
  UpdateControls;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarEx.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.DoInsertObject(Index: Integer;
  const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.SetBounds(X, Y, AWidth, AHeight: Single);
var
  r: TRectF;
begin
  r := BoundsRect;
  inherited;
  if (csDesigning in ComponentState) and not ((csLoading in ComponentState) or (csReading in ComponentState)) then
  begin
    if ((r.Width <> AWidth) or (r.Height <> AHeight)) then
      UpdateControls;
  end
  else
    UpdateControls;
end;

procedure TAdvCustomToolBarEx.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;
{$ENDIF}

procedure TAdvCustomToolBarEx.DrawDragGrip(AGraphics: TAdvGraphics);
var
  cnt: Integer;

  procedure DrawDots(ARect: TRectF);
  var
    i: integer;
    rdg: TRectF;
    sz: Integer;
  begin
    if State = esLarge then
      sz := ScalePaintValue(3)
    else
      sz := ScalePaintValue(2);

    ARect.Left := ARect.Left;
    ARect.Top := ARect.Top;
    cnt := Round(ARect.Bottom) div (Round(sz) * 2);
    for i := 1 to cnt do
    begin
      rdg := RectF(ARect.Left + ((ARect.Right - ARect.Left) - sz) / 2, ARect.Top + 1, ARect.Left + sz + ((ARect.Right - ARect.Left) - sz) / 2, ARect.Top + 1 + sz);
      rdg := RectF(int(rdg.Left), int(rdg.Top), int(rdg.Right), int(rdg.Bottom));
      AGraphics.DrawRectangle(rdg);
      ARect.Top := ARect.Top + sz * 2;
    end;
  end;
var
  r: TRectF;
begin
  AGraphics.Fill.Color := Appearance.DragGripColor;
  AGraphics.Fill.Kind := gfkSolid;
  r := GetDragGripRect;
  DrawDots(r);
end;

procedure TAdvCustomToolBarEx.DrawText(AGraphics: TAdvGraphics);
var
  r: TRectF;
begin
  if TextVisible and (Text <> '') then
  begin
    AGraphics.Font.Assign(Font);

    r := GetTextRect;

    AGraphics.DrawText(r, Text, WordWrapping, HorizontalTextAlign, VerticalTextAlign, Trimming);
  end;
end;

function TAdvCustomToolBarEx.DropDownActive: Boolean;
var
  I: Integer;
  c: TControl;
begin
  Result := False;
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];
    if c is TAdvDefaultToolBarExButton then
    begin
      if Assigned((c as TAdvDefaultToolBarExButton).FPopup) then
      begin
        Result := (c as TAdvDefaultToolBarExButton).FPopup.IsOpen;
        if Result then
          Break;
      end;
    end;
  end;
end;

function TAdvCustomToolBarEx.GetPictureContainer: TPictureContainer;
begin
  Result := FPictureContainer;
end;

function TAdvCustomToolBarEx.GetBitmapPickerClass: TAdvToolBarExBitmapPickerClass;
begin
  Result := TAdvToolBarExBitmapPicker;
end;

function TAdvCustomToolBarEx.GetCompactBitmapRect(ARect: TRectF): TRectF;
var
  r: TRectF;
  bs: Single;
begin
  r := ARect;
  Result := RectF(r.Left, r.Top, r.Right, r.Top);
  if CompactBitmapVisible then
  begin
    bs := GetCompactBitmapSize;
    if not TextVisible or (Text = '') then
    begin
      Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);
      if (bs <> -1) then
      begin
        Result.Left := Round(r.Left + ((r.Right - r.Left) - bs) / 2);
        Result.Top := Round(r.Top + ((r.Bottom - r.Top) - bs) / 2);
        Result.Right := Result.Left + bs;
        Result.Bottom := Result.Top + bs;
      end;
    end
    else if TextVisible and not (Text = '') then
      Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, r.Top + bs);
  end;
end;

function TAdvCustomToolBarEx.GetCompactBitmapSize: Single;
var
  bmp: TAdvBitmap;
begin
  Result := CompactBitmapSize;
  if CompactAutoBitmapSize then
  begin
    bmp := TAdvGraphics.GetScaledBitmap(CompactBitmaps, 0, PictureContainer);
    if Assigned(bmp) and not IsBitmapEmpty(bmp) then
      Result := Max(bmp.Height, bmp.Width);
  end;
end;

function TAdvCustomToolBarEx.GetColorPickerClass: TAdvToolBarExColorPickerClass;
begin
  Result := TAdvToolBarExColorPicker;
end;

function TAdvCustomToolBarEx.GetCompactControl(
  AControl: TControl): TAdvToolBarExControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCompactControls.Count - 1 do
  begin
    if TAdvToolBarExControl(FCompactControls[I]).FControl = AControl then
    begin
      Result := TAdvToolBarExControl(FCompactControls[I]);
      Break;
    end;
  end;
end;

function TAdvCustomToolBarEx.GetCompactControlCount: Integer;
begin
  Result := FCompactControls.Count;
end;

function TAdvCustomToolBarEx.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/' + LowerCase(ClassName);
end;

function TAdvCustomToolBarEx.GetDragGripRect(AInteraction: Boolean = False): TRectF;
var
  r: TRectF;
  f: Single;
begin
  r := LocalRect;
  f := 1;
  if State = esLarge then
    f := 1.75;

  if AInteraction then
    Result := RectF(r.Left, r.Top, r.Left + ScalePaintValue(8 * f), r.Bottom)
  else
    Result := RectF(r.Left + ScalePaintValue(3), r.Top + ScalePaintValue(3), r.Left + ScalePaintValue(6 * f), r.Bottom - ScalePaintValue(3));
end;

function TAdvCustomToolBarEx.GetFontNamePickerClass: TAdvToolBarExFontNamePickerClass;
begin
  Result := TAdvToolBarExFontNamePicker;
end;

function TAdvCustomToolBarEx.GetFontSizePickerClass: TAdvToolBarExFontSizePickerClass;
begin
  Result := TAdvToolBarExFontSizePicker;
end;

function TAdvCustomToolBarEx.GetHiddenControl(
  AControl: TControl): TAdvToolBarExControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FHiddenControls.Count - 1 do
  begin
    if TAdvToolBarExControl(FHiddenControls[I]).FControl = AControl then
    begin
      Result := TAdvToolBarExControl(FHiddenControls[I]);
      Break;
    end;
  end;              
end;

function TAdvCustomToolBarEx.GetHiddenControlCount(
  AControl: TControl): Integer;
var
  I: Integer;
  lst: TAdvToolBarExControlList;
begin
  lst := TAdvToolBarExControlList.Create;
  for I := 0 to FHiddenControls.Count - 1 do
    lst.Add(FHiddenControls[I]);

  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  lst.Sort(FCompareHiddenControls);
  {$ENDIF}
  {$ENDIF}
  Result := 0;
  for I := 0 to lst.Count - 1 do
  begin
    if TAdvToolBarExControl(lst[I]).FControl = AControl then
      Break
    else
      Inc(Result);
  end;
  lst.Free;
end;

function TAdvCustomToolBarEx.GetHintString: String;
begin
  Result := inherited GetHintString;
  if FQuickMenuButtonHover then
    Result := QuickMenuButtonHint
end;

function TAdvCustomToolBarEx.GetItemPickerClass: TAdvToolBarExItemPickerClass;
begin
  Result := TAdvToolBarExItemPicker;
end;

function TAdvCustomToolBarEx.GetOptionsMenuButtonClass: TAdvToolBarExDropDownButtonClass;
begin
  Result := TAdvToolBarExDropDownButton;
end;

function TAdvCustomToolBarEx.GetOptionsMenuButtonControl: TAdvToolBarExDropDownButton;
begin
  Result := FOptionsMenuButton;
end;

function TAdvCustomToolBarEx.GetQuickMenuButtonRect: TRectF;
begin
  Result := RectF(Width - 16, Height - 16, Width, Height);
  if Appearance.Separator then
  begin
    Result.Left := Result.Left - Appearance.SeparatorStroke.Width;
    Result.Right := Result.Right - Appearance.SeparatorStroke.Width;
  end;
end;

{$IFDEF FNCLIB}
function TAdvCustomToolBarEx.GetSubComponentArray: TAdvGraphicsStylesManagerComponentArray;
var
  I: Integer;
begin
  SetLength(Result, ControlCount + FHiddenControls.Count + 1);
  for I := 0 to ControlCount - 1 do
    Result[I] := Controls[I];

  for I := 0 to FHiddenControls.Count - 1 do
    Result[I + ControlCount] := FHiddenControls[I].FControl;

  Result[Length(Result) - 1] := FOptionsMenuButton;
end;
{$ENDIF}

function TAdvCustomToolBarEx.GetCompactTextRect: TRectF;
var
  bmpr: TRectF;
  r: TRectF;
begin
  r := LocalRect;
  bmpr := GetCompactBitmapRect(r);
  Result := RectF(r.Left + 3, bmpr.Bottom + 3, r.Right - 3, r.Bottom - 23)
end;

function TAdvCustomToolBarEx.GetTextRect: TRectF;
var
  r: TRectF;
  h, sz: Single;
begin
  sz := 0;
  if Appearance.DragGrip then
  begin
    if State = esLarge then
      sz := 3
    else
      sz := 2;
  end;
  r := LocalRect;
  r := RectF(r.Left + ScalePaintValue(sz), r.Top, r.Right, r.Bottom);
  InflateRectEx(r, ScalePaintValue(-2), ScalePaintValue(-2));

  h := GetTextSize.cy;

  Result := RectF(r.Left, r.Bottom - h, r.Right, r.Bottom);
end;

function TAdvCustomToolBarEx.GetTextSize: TSizeF;
var
  g: TAdvGraphics;
  sz: Single;
  r: TRectF;
begin
  Result.cx := 0;
  Result.cy := 0;
  if TextVisible and (Text <> '') then
  begin
    g := TAdvGraphics.CreateBitmapCanvas(1, 1, NativeCanvas);
    if not NativeCanvas then
      g.BeginScene;
    g.PictureContainer := PictureContainer;
    g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
    try
      sz := 0;
      if Appearance.DragGrip then
      begin
        if State = esLarge then
          sz := 3
        else
          sz := 2;
      end;

      g.Font.Assign(Font);

      r := LocalRect;
      r := RectF(r.Left + sz, r.Top, r.Right, r.Bottom);
      InflateRectEx(r, -2, -2);
      Result := g.CalculateTextSize(Text, r, WordWrapping);
    finally
      if not NativeCanvas then
        g.EndScene;
      g.Free;
    end;
  end;
end;

function TAdvCustomToolBarEx.GetToolBarButtonClass: TAdvToolBarExButtonClass;
begin
  Result := TAdvToolBarExButton;
end;

function TAdvCustomToolBarEx.GetToolBarSeparatorClass: TAdvToolBarExSeparatorClass;
begin
  Result := TAdvToolBarExSeparator;
end;

function TAdvCustomToolBarEx.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomToolBarEx.UpdateToolBar(Sender: TObject);
begin
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.UpdateToolBarControl(Sender: TObject);
var
  tb: TAdvToolBarExControl;
  bmp: TAdvBitmapHelperClass;
  c: TControl;
  tc: TControl;
  tci: Integer;
begin
  if not Assigned(Sender) then
    Exit;

  FBlockUpdate := True;

  c := Sender as TControl;

  tb := GetHiddenControl(c);
  if Assigned(tb) then
  begin
    tc := tb.FControl;
    tci := Max(0, Min(ControlCount, tb.FControlIndex - GetHiddenControlCount(tc)));
    FHiddenControls.Remove(tb);
    InsertToolBarControl(tc, tci);
    tc.Visible := True;
  end
  else
  begin
    tb := TAdvToolBarExControl.Create;

    if c is TAdvCustomToolBarExElement then
      tb.FControlIndex := (c as TAdvCustomToolBarExElement).ControlIndex
    else
      tb.FControlIndex := c.Tag;

    tb.FControl := c;
    FBlockUpdate := True;
    bmp := c.MakeScreenshot;
    FBlockUpdate := False;
    tb.FBitmap.Assign(bmp);
    bmp.Free;
    FHiddenControls.Add(tb);
    c.Parent := nil;
    c.Visible := False;
  end;

  FBlockUpdate := False;
  UpdateControls;
end;

function TAdvCustomToolBarEx.XYToAnchor(AX, AY: Single): string;
var
  r: TRectF;
  g: TAdvGraphics;
begin
  Result := '';
  r := GetTextRect;

  g := TAdvGraphics.CreateBitmapCanvas;
  g.BeginScene;
  g.PictureContainer := PictureContainer;
  try
    g.Font.Assign(Font);
    Result := g.DrawText(r, FText, FWordWrapping, FHorizontalTextAlign, FVerticalTextAlign, FTrimming, 0, -1, -1, True, True, AX, AY);
  finally
    g.EndScene;
    g.Free;
  end;
end;

procedure TAdvCustomToolBarEx.InitializeOptionsMenu;
{$IFDEF WEBLIB}
begin
{$ENDIF}
{$IFNDEF WEBLIB}
var
  mnu: TAdvToolBarExOptionMenuItem;
  bmp: TAdvBitmapHelperClass;
  I: Integer;
  c: TControl;
  lst: TAdvControlList;
  tb: TAdvToolBarExControl;
  sh: Boolean;
  bmpw, bw, bh: Single;
  {$IFDEF FMXLIB}
  o: TFmxObject;
  {$ENDIF}
  maxh: Single;
begin
  lst := nil;
  try
    lst := TAdvControlList.Create;
    for I := 0 to Self.ControlCount - 1 do
      lst.Add(Controls[I]);

    for I := 0 to FHiddenControls.Count - 1 do
      lst.Add(TAdvToolBarExControl(FHiddenControls[I]).FControl);

    {$IFNDEF LCLLIB}
    {$IFNDEF WEBLIB}
    lst.Sort(FCompareControls);
    {$ENDIF}
    {$ENDIF}

    {$IFDEF FMXLIB}
    FAutoOptionsMenu.Clear;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FAutoOptionsMenu.Items.Clear;
    {$ENDIF}

    FTotalOptionsMenuWidth := 0;

    for I := 0 to lst.Count - 1 do
    begin
      c := TControl(lst[I]);

      {$IFDEF FMXLIB}
      if Supports(c, IDesignerControl) then
        Continue;
      {$ENDIF}

      if not (c = FOptionsMenuButton) and not (c is TAdvCustomToolBarExSeparator) then
      begin
        sh := True;
        DoCanShowOptionsMenuItem(c, sh);
        if sh then
        begin
          mnu := TAdvToolBarExOptionMenuItem.Create(FAutoOptionsMenu);
          mnu.OnClick := OptionMenuItemClick;
          {$IFDEF FMXLIB}
          mnu.OnApplyStyleLookup := OptionMenuItemApplyStyleLookup;
          mnu.Calculate := True;
          mnu.ApplyStyleLookup;
          {$ENDIF}

          bmpw := 0;
          bw := 0;
          bh := 0;
          {$IFDEF FMXLIB}
          o := mnu.FindStyleResource('checkmark');
          if Assigned(o) and (o is TControl) then
            bmpw := (o as TControl).Width;
          {$ENDIF}

          maxh := 20;
          if OptionsMenu.ShowItemBitmap then
          begin
            tb := GetHiddenControl(c);
            if Assigned(tb) then
            begin
              bmp := tb.FBitmap;
              mnu.Bitmap.Assign(bmp);
              bw := bmp.Width;
              bh := bmp.Height;
            end
            else
            begin
              FBlockUpdate := True;
              bmp := c.MakeScreenshot;
              FBlockUpdate := False;
              mnu.Bitmap.Assign(bmp);
              bw := bmp.Width;
              bh := bmp.Height;
              bmp.Free;
            end;
          end;

          TAdvGraphics.GetAspectSize(bw, bh, bw, bh, bw, maxh, True, False, False);
          bmpw := bmpw + int(bw);

          if OptionsMenu.ShowItemText then
          begin
            if c is TAdvCustomToolBarExButton then
              mnu.Caption := (c as TAdvCustomToolBarExButton).AutoOptionsMenuText;

            if mnu.Caption = '' then
              mnu.Caption := c.Name;
          end;

          mnu.Control := c;
          mnu.Checked := Assigned(c.Parent);

          DoCustomizeOptionsMenuItem(c, mnu);

          {$IFDEF FMXLIB}
          FAutoOptionsMenu.AddObject(mnu);
          {$ENDIF}
          {$IFDEF CMNLIB}
          FAutoOptionsMenu.Items.Add(mnu);
          {$ENDIF}
          FTotalOptionsMenuWidth := Max(FTotalOptionsMenuWidth, bmpw);
        end;
      end;
    end;

    {$IFDEF FMXLIB}
    for I := 0 to FAutoOptionsMenu.ItemsCount - 1 do
    {$ENDIF}
    {$IFDEF CMNLIB}
    for I := 0 to FAutoOptionsMenu.Items.Count - 1 do
    {$ENDIF}
    begin
      if FAutoOptionsMenu.Items[I] is TAdvToolBarExOptionMenuItem then
      begin
        mnu := (FAutoOptionsMenu.Items[I] as TAdvToolBarExOptionMenuItem);
        {$IFDEF FMXLIB}
        mnu.NeedStyleLookup;
        mnu.Calculate := False;
        {$ENDIF}
        if OptionsMenu.AutoItemBitmapWidth then
          mnu.GlyphWidth := FTotalOptionsMenuWidth
        else
          mnu.GlyphWidth := OptionsMenu.ItemBitmapWidth;
      end;
    end;

  finally
    if Assigned(lst) then
      lst.Free;
  end;
{$ENDIF}
end;

procedure TAdvCustomToolBarEx.InsertToolBarControl(AControl: TControl;
  AIndex: Integer);
begin
  {$IFDEF FMXLIB}
  if (AIndex >= 0) and (AIndex <= Self.ControlCount - 1) then
    InsertObject(AIndex, AControl)
  else
    AddObject(AControl);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AControl.Parent := Self;
  if AIndex > -1 then
    SetChildOrder(AControl, AIndex);
  {$ENDIF}

  if Assigned(OnInternalInsertControl) then
    OnInternalInsertControl(AControl);
end;

function TAdvCustomToolBarEx.IsCompactBitmapSizeStored: Boolean;
begin
  Result := CompactBitmapSize <> 24;
end;

function TAdvCustomToolBarEx.IsCompactWidthStored: Boolean;
begin
  Result := CompactWidth <> 50;
end;

function TAdvCustomToolBarEx.IsMinimumWidthStored: Boolean;
begin
  Result := MinimumWidth <> 30;
end;

procedure TAdvCustomToolBarEx.Loaded;
begin
  inherited;
  {$IFDEF FMXLIB}
  if (csDesigning in ComponentState) or (FFirstLoad) then
    UpdateControls;
  {$ENDIF}
end;

procedure TAdvCustomToolBarEx.HandleCompact;
var
  I: Integer;
  cc: TAdvToolBarExControl;
begin
  inherited;

  DeactivateAllPopups;

  if Assigned(FCompactToolbar) then
  begin
    FCompactToolbar.Free;
    FCompactToolbar := nil;
  end;

  if Assigned(FCompactPopup) then
  begin
    FCompactPopup.Free;
    FCompactPopup := nil;
  end;

  FCompactPopup := TAdvToolBarExPopup.Create(Self);
  FCompactPopup.OnClosePopup := DoCloseCompactPopup;

  FCompactToolbar := TAdvCustomToolBarEx.Create(FCompactPopup);

  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  FCompactToolbar.ScaleForPPI(CurrentPPI);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}

  FCompactToolbar.Assign(Self);
  FCompactToolbar.Compact := False;
  FCompactToolbar.AutoSize := True;
  FCompactToolbar.AutoHeight := False;
  FCompactToolbar.AutoWidth := True;
  FCompactToolbar.AutoStretchHeight := False;
  FCompactToolbar.Appearance.Separator := False;
  FCompactToolbar.SetControlMargins(1, 1, 1, 1);
  FCompactToolbar.Height := Height + 2;

  for I := FCompactControls.Count - 1 downto 0 do
  begin
    cc := TAdvToolBarExControl(FCompactControls[I]);
    if Assigned(cc.FControl) then
    begin
      if cc.FControl is TAdvDefaultToolBarExButton then
        (cc.FControl as TAdvDefaultToolBarExButton).Layout := (cc.FControl as TAdvDefaultToolBarExButton).CompactLayout;
      cc.FControl.Visible := True;
      cc.FControl.Parent := FCompactToolbar;
    end;
  end;

  FCompactToolbar.UpdateControls;
  FCompactPopup.PlacementControl := Self;
  FCompactPopup.ContentControl := FCompactToolbar;
  FCompactPopup.IsOpen := True;
  DoCompactClick;
end;

procedure TAdvCustomToolBarEx.HandleDblClick(X, Y: Single);
begin
  inherited;
  if Assigned(OnInternalDblClick) then
    OnInternalDblClick(Self);
end;

procedure TAdvCustomToolBarEx.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Appearance.DragGrip and not FDragGripMoving then
  begin
    FDragGripDown := PtInRectEx(GetDragGripRect(True), PointF(X, Y));
    FDragGripDownPt := PointF(X, Y);
  end;

  if not FDragGripDown then
  begin
    if Compact then
    begin
      FDown := True;
      Invalidate;
      CaptureEx;
    end
    else if QuickMenuButton then
    begin
      FQuickMenuButtonDown := PtInRectEx(GetQuickMenuButtonRect, PointF(X, Y));
      if FQuickMenuButtonDown then
      begin
        Invalidate;
        CaptureEx;
      end;
    end;
  end;

  if Assigned(OnInternalMouseDown) and not (Button = TAdvMouseButton.mbRight) then
    OnInternalMouseDown(Self);
end;

procedure TAdvCustomToolBarEx.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  r: TRectF;
  absx, absy: Single;
  pt: TPointF;
  d: TAdvCustomDockPanelEx;
  a: string;
begin
  inherited;

  if Compact then
  begin
    FHoverP := FHover;
    FHover := True;
    if FHover <> FHoverP then
      Invalidate;
  end
  else if QuickMenuButton then
  begin
    FQuickMenuButtonHoverP := FQuickMenuButtonHover;
    FQuickMenuButtonHover := PtInRectEx(GetQuickMenuButtonRect, PointF(X, Y));
    if FQuickMenuButtonHover <> FQuickMenuButtonHoverP then
      Invalidate;
  end;

  if Appearance.DragGrip then
  begin
    if FDragGripDown then
    begin
      if not FDragGripMoving then
      begin
        CaptureEx;
        FDragGripMoving := True;
      end;

      if FDragGripMoving then
      begin
        absx := (FDragGripDownPt.X - X);
        absy := (FDragGripDownPt.Y - Y);
        if AutoMoveToolBar then
        begin
          {$IFDEF FMXLIB}
          pt := PointF(Position.Point.X - absx, Position.Point.Y - absy);
          {$ENDIF}
          {$IFDEF CMNWEBLIB}
          pt := PointF(Left - absx, Top - absy);
          {$ENDIF}

          if Parent is TAdvCustomDockPanelEx then
          begin
            d := Parent as TAdvCustomDockPanelEx;
            if d.AutoAlign then
            begin
              if pt.Y < d.Appearance.Margins.Top then
                pt.Y := Round(d.Appearance.Margins.Top);
              if pt.X < d.Appearance.Margins.Left then
                pt.X := Round(d.Appearance.Margins.Left)
              else if pt.X + Width > d.Width - d.Appearance.Margins.Right then
                pt.X := Round(d.Width - d.Appearance.Margins.Right - Width);

              SetBounds(Round(pt.X), Round(pt.Y), Width, Height);
            end;
          end;
        end;
        FDragGripDownPt := PointF(X + absx, Y + absy);
        DoDragGripMoving(absx, absy);
      end;
    end
    else
    begin
      r := GetDragGripRect(True);
      if PtInRectEx(r, PointF(X, Y)) then
      begin
        if not FInsideDrag then
        begin
          FInsideDrag := True;
          Cursor := crSize;
        end;
      end
      else if FInsideDrag then
      begin
        FInsideDrag := False;
        Cursor := crDefault;
      end;
    end;
  end;

  a := XYToAnchor(X, Y);

  if a <> '' then
    Cursor := crHandPoint
  else if (Cursor = crHandPoint) then
    Cursor := crDefault;

  if Assigned(OnInternalMouseMove) then
    OnInternalMouseMove(Self);
end;

procedure TAdvCustomToolBarEx.HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  a: string;
begin
  inherited;
  if FDragGripDown then
    ReleaseCaptureEx
  else if FDown and Compact then
  begin
    Invalidate;
    ReleaseCaptureEx;
    HandleCompact;
  end
  else if FQuickMenuButtonDown and QuickMenuButton and not Compact then
  begin
    Invalidate;
    ReleaseCaptureEx;
    HandleQuickMenuButton;
  end;

  FDragGripDown := False;
  FDragGripMoving := False;
  FDown := False;
  FQuickMenuButtonDown := False;

  a := XYToAnchor(X, Y);
  if a <> '' then
    DoAnchorClick(a);

  if Assigned(OnInternalMouseUp) and not (Button = TAdvMouseButton.mbRight) then
    OnInternalMouseUp(Self);
end;

procedure TAdvCustomToolBarEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (Operation = opRemove) and (AComponent = FCustomOptionsMenu) then
    FCustomOptionsMenu := nil;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarEx.OptionMenuItemApplyStyleLookup(Sender: TObject);
var
  c: TControl;
begin
  c := nil;
  if (Sender is TAdvToolBarExOptionMenuItem) then
    c := (Sender as TAdvToolBarExOptionMenuItem).FControl;

  if Assigned(OnOptionsMenuItemApplyStyle) and (Sender is TMenuItem) then
    OnOptionsMenuItemApplyStyle(Self, c, Sender as TMenuItem);
end;
{$ENDIF}

procedure TAdvCustomToolBarEx.OptionMenuItemClick(Sender: TObject);
var
  a: Boolean;
  c: TControl;
begin
  a := True;
  c := nil;
  if (Sender is TAdvToolBarExOptionMenuItem) then
    c := (Sender as TAdvToolBarExOptionMenuItem).FControl;

  if Assigned(OnOptionsMenuItemClick) and (Sender is TMenuItem) then
    OnOptionsMenuItemClick(Self, c, Sender as TMenuItem, a);

  if a then
    UpdateToolBarControl(c);
end;

procedure TAdvCustomToolBarEx.OptionsButtonClick(Sender: TObject);
var
  pt: TPointF;
begin
  if Assigned(OnOptionsMenuButtonClick) then
    OnOptionsMenuButtonClick(Self)
  else
  begin
    {$IFDEF FMXLIB}
    pt := FOptionsMenuButton.Position.Point;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    pt := PointF(FOptionsMenuButton.Left, FOptionsMenuButton.Top);
    {$ENDIF}
    pt := LocalToScreenEx(PointF(pt.X, pt.Y + FOptionsMenuButton.Height));
    ShowOptionsMenu(pt.X, pt.Y);
  end;
end;

procedure TAdvCustomToolBarEx.QuickMenuButtonAppearanceChanged(
  Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomToolBarEx.QuickMenuButtonBitmapsChanged(
  Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomToolBarEx.ResetToDefaultStyle;
begin
  inherited;
  Appearance.DragGripColor := gcLightgray;
  Appearance.Fill.Color := gcWhite;
  Appearance.Stroke.Color := gcGray;
  Appearance.Fill.Kind := gfkGradient;
  Appearance.Stroke.Kind := gskSolid;
  Appearance.Fill.Color := gcWhite;
  Appearance.Fill.ColorTo := MakeGraphicsColor(230, 230, 230);
end;

procedure TAdvCustomToolBarEx.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  qr, r: TRectF;
  w: Single;  
begin
  inherited;

  r := LocalRect;

  if Compact then
  begin
    if FDown then
    begin
      AGraphics.Fill.Assign(CompactAppearance.DownFill);
      AGraphics.Stroke.Assign(CompactAppearance.DownStroke);
    end
    else if FHover then
    begin
      AGraphics.Fill.Assign(CompactAppearance.HoverFill);
      AGraphics.Stroke.Assign(CompactAppearance.HoverStroke);
    end
    else
    begin
      AGraphics.Fill.Assign(CompactAppearance.NormalFill);
      AGraphics.Stroke.Assign(CompactAppearance.NormalStroke);
    end;

    if CompactAppearance.FlatStyle then
      AGraphics.Fill.Kind := gfkSolid;

    AGraphics.DrawRectangle(RectF(r.Left, r.Top + 2, r.Right - 3, r.Bottom - 2));
  end
  else
  begin
    AGraphics.Fill.Assign(Appearance.Fill);
    AGraphics.Stroke.Assign(Appearance.Stroke);

    if Appearance.FlatStyle then
      AGraphics.Fill.Kind := gfkSolid;

    AGraphics.DrawRectangle(r);
  end;

  if Compact then
  begin
    DrawCompactBitmap(AGraphics, ARect);
    DrawCompactText(AGraphics);
    DrawCompactExpander(AGraphics);
  end
  else
  begin
    if Appearance.DragGrip then
      DrawDragGrip(AGraphics);

    DrawText(AGraphics);

    if QuickMenuButton then
    begin
      if FQuickMenuButtonDown then
      begin
        AGraphics.Fill.Assign(QuickMenuButtonAppearance.DownFill);
        AGraphics.Stroke.Assign(QuickMenuButtonAppearance.DownStroke);
      end
      else if FQuickMenuButtonHover then
      begin
        AGraphics.Fill.Assign(QuickMenuButtonAppearance.HoverFill);
        AGraphics.Stroke.Assign(QuickMenuButtonAppearance.HoverStroke);
      end
      else
      begin
        AGraphics.Fill.Assign(QuickMenuButtonAppearance.NormalFill);
        AGraphics.Stroke.Assign(QuickMenuButtonAppearance.NormalStroke);
      end;

      if QuickMenuButtonAppearance.FlatStyle then
        AGraphics.Fill.Kind := gfkSolid;
      
      qr := GetQuickMenuButtonRect;
      AGraphics.DrawRectangle(qr);

      InflateRectEx(qr, -2, -2);
      AGraphics.DrawScaledBitmap(qr, QuickMenuButtonBitmaps);      
    end;
  end;

  if Appearance.Separator then
  begin
    AGraphics.Stroke.Assign(Appearance.SeparatorStroke);
    w := Appearance.SeparatorStroke.Width;
    AGraphics.DrawLine(PointF(r.Right - w, r.Top + 4), PointF(r.Right - w, r.Bottom - 4));
  end;

  if IsDesignTime and CanDrawDesignTime then
    AGraphics.DrawFocusRectangle(ARect, gcBlack);
end;

procedure TAdvCustomToolBarEx.DrawCompactBitmap(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  r: TRectF;
begin
  r := GetCompactBitmapRect(ARect);
  AGraphics.PictureContainer := PictureContainer;
  AGraphics.DrawScaledBitmap(r, CompactBitmaps)
end;

procedure TAdvCustomToolBarEx.DrawCompactExpander(AGraphics: TAdvGraphics);
var
  r: TRectF;
begin
  r := LocalRect;
  r := RectF(r.Left, r.Bottom - 20, r.Right, r.Bottom);
  InflateRectEx(r, -2, -2);
  AGraphics.DrawScaledBitmap(r, CompactExpanderBitmaps);
end;

procedure TAdvCustomToolBarEx.DrawCompactText(AGraphics: TAdvGraphics);
var
  tr: TRectF;
  st: TAdvGraphicsSaveState;
begin
  if not TextVisible or (Text = '') then
    Exit;

  tr := GetCompactTextRect;
  st := AGraphics.SaveState;
  try
    AGraphics.ClipRect(tr);
    AGraphics.Font.Assign(Font);
    AGraphics.DrawText(tr, Text, WordWrapping, HorizontalTextAlign, VerticalTextAlign, Trimming);
  finally
    AGraphics.RestoreState(st);
  end;
end;

procedure TAdvCustomToolBarEx.UncompactControls;
var
  I: Integer;
  cc: TAdvToolBarExControl;
  c: TControl;
begin
  FBlockUpdate := True;
  for I := FCompactControls.Count - 1 downto 0 do
  begin
    cc := TAdvToolBarExControl(FCompactControls[I]);
    c := cc.FControl;
    if Assigned(c) then
    begin
      if c is TAdvCustomToolBarExElement then
        (c as TAdvCustomToolBarExElement).FBlockUpdate := True;
      c.Parent := Self;
      c.Visible := True;
      if c is TAdvCustomToolBarExElement then
        (c as TAdvCustomToolBarExElement).FBlockUpdate := False;
      {$IFDEF VCLLIB}
      {$HINTS OFF}
      {$IF COMPILERVERSION >= 33}
      c.ScaleForPPI(CurrentPPI);
      {$IFEND}
      {$HINTS ON}
      {$ENDIF}
    end;
  end;

  FBlockUpdate := False;
  UpdateControls;

  Invalidate;
end;

procedure TAdvCustomToolBarEx.UpdateControlAfterResize;
begin
  inherited;
  UpdateDockPanel;
end;

procedure TAdvCustomToolBarEx.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomToolBarEx.SetAppearance(
  const Value: TAdvToolBarExAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetOptionsMenu(
  const Value: TAdvToolBarExOptionsMenu);
begin
  FOptionsMenu.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetQuickMenuButton(const Value: Boolean);
begin
  if FQuickMenuButton <> Value then
  begin
    FQuickMenuButton := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarEx.SetQuickMenuButtonAppearance(
  const Value: TAdvToolBarExQuickMenuButtonAppearance);
begin
  FQuickMenuButtonAppearance.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetQuickMenuButtonBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FQuickMenuButtonBitmaps.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetState(const Value: TAdvToolBarExElementState);
var
  I: Integer;
  c: TControl;
begin
  if FState <> Value then
  begin
    FState := Value;
    for I := 0 to Self.ControlCount - 1 do
    begin
      c := Self.Controls[I];
      if c is TAdvCustomToolBarExElement then
        (c as TAdvCustomToolBarExElement).State := FState;
    end;
  end;
end;

procedure TAdvCustomToolBarEx.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetTextVisible(const Value: Boolean);
begin
  if FTextVisible <> Value then
  begin
    FTextVisible := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetTrimming(
  const Value: TAdvGraphicsTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetAutoAlign(const Value: Boolean);
begin
  if FAutoAlign <> Value then
  begin
    FAutoAlign := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetAutoStretchHeight(const Value: Boolean);
begin
  if FAutoStretchHeight <> Value then
  begin
    FAutoStretchHeight := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetPictureContainer(
  const Value: TPictureContainer);
var
  I: Integer;
  {$IFDEF FNCLIB}
  c: TControl;
  b: IAdvPictureContainer;
  {$ENDIF}
begin
  FPictureContainer := Value;
  for I := 0 to Self.ControlCount - 1 do
  begin
    {$IFDEF FNCLIB}
    {$IFNDEF WEBLIB}
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}

    {$IFDEF FNCLIB}
    if Supports(c, IAdvPictureContainer, b) then
      b.PictureContainer := Value;
    {$ENDIF}
  end;
  Invalidate;
end;

procedure TAdvCustomToolBarEx.SetCanCompact(const Value: Boolean);
begin
  if FCanCompact <> Value then
    FCanCompact := Value;
end;

procedure TAdvCustomToolBarEx.SetCompact(const Value: Boolean);
begin
  if FCompact <> Value then
  begin
    if CanCompact then
    begin
      FCompact := Value;
      if Compact then
      begin
        FOldMenuButtonState := OptionsMenu.ShowButton;
        OptionsMenu.ShowButton := False;
        FOldWidth := Width;
        CompactControls;
        Width := Round(CompactWidth);
      end
      else
      begin
        Width := Round(FOldWidth);
        UncompactControls;
        OptionsMenu.ShowButton := FOldMenuButtonState;
      end;

      UpdateControls;
    end;
  end;
end;

procedure TAdvCustomToolBarEx.SetCompactAppearance(
  const Value: TAdvToolBarExCompactAppearance);
begin
  FCompactAppearance.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetCompactAutoBitmapSize(const Value: Boolean);
begin
  if FCompactAutoBitmapSize <> Value then
  begin
    FCompactAutoBitmapSize := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarEx.SetCompactBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FCompactBitmaps.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetCompactBitmapSize(const Value: Single);
begin
  if FCompactBitmapSize <> Value then
  begin
    FCompactBitmapSize := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarEx.SetCompactBitmapVisible(const Value: Boolean);
begin
  if FCompactBitmapVisible <> Value then
  begin
    FCompactBitmapVisible := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarEx.SetCompactExpanderBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FCompactExpanderBitmaps.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetCompactWidth(const Value: Single);
begin
  if FCompactWidth <> Value then
  begin
    FCompactWidth := Value;
    if Compact then
      Width := Round(CompactWidth);
  end;
end;

procedure TAdvCustomToolBarEx.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvCustomToolBarEx.SetHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarEx.SetMinimumWidth(const Value: Single);
begin
  if FMinimumWidth <> Value then
  begin
    FMinimumWidth := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomToolBarEx.SetAS(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    UpdateControls;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarEx.SetVisible(const Value: Boolean);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomToolBarEx.VisibleChanging;
{$ENDIF}
begin
  inherited;
  UpdateDockPanel;
end;

procedure TAdvCustomToolBarEx.ShowOptionsMenu(X, Y: Single);
begin
  {$IFDEF FMXLIB}
  if not Assigned(CustomOptionsMenu) then
  begin
    InitializeOptionsMenu;
    DoCustomizeOptionsMenu;
    if FAutoOptionsMenu.ItemsCount > 0 then
      FAutoOptionsMenu.Popup(X, Y);
  end
  else
    CustomOptionsMenu.Popup(X, Y);
 {$ENDIF}
 {$IFDEF CMNLIB}
  if not Assigned(CustomOptionsMenu) then
  begin
    InitializeOptionsMenu;
    DoCustomizeOptionsMenu;
    if FAutoOptionsMenu.Items.Count > 0 then
      FAutoOptionsMenu.Popup(Round(X), Round(Y));
  end
  else
    CustomOptionsMenu.Popup(Round(X), Round(Y));
  {$ENDIF}
end;

procedure TAdvCustomToolBarEx.UpdateControls;
var
  I: Integer;
  c: TControl;
  idx: Integer;
  fu: Boolean;
  hc: TAdvToolBarExControl;
  {$IFDEF FMXLIB}
  maxx, x, w, y, h, hs, hsm, vs, xs, ys, txth, txtw: Single;
  mgr: TBounds;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  maxx, x, w, y, h, hs, hsm, vs, xs, ys, txth, txtw: Integer;
  {$ENDIF}
  {$IFDEF LCLLIB}
  mgr: TControlBorderSpacing;
  {$ENDIF}
  {$IFDEF VCLWEBLIB}
  mgr: TMargins;
  {$ENDIF}
  l: Boolean;
  {$IFDEF FNCLIB}
  ia: IAdvAdaptToStyle;
  {$ENDIF}
  sz: TSizeF;
  cn: TControl;
  lm, tm, rm, bm: Single;

  function GetControl(AIndex: Integer): TControl;
  begin
    Result := nil;

    if (AIndex >= 0) and (AIndex <= Self.ControlCount - 1) then
    begin
      if Self.Controls[AIndex].Visible then
      begin
        Result := Self.Controls[AIndex];
        Exit;
      end;
    end;

    while (AIndex >= 0) and (AIndex <= Self.ControlCount - 1) do
    begin
      Inc(AIndex);
      if (AIndex >= 0) and (AIndex <= Self.ControlCount - 1) then
      begin
        if Self.Controls[AIndex].Visible then
        begin
          Result := Self.Controls[AIndex];
          Exit;
        end;
      end;
    end;
  end;

  function IsLoadingEx: Boolean;
  var
    ii: Integer;
    ctl: TControl;
  begin
    Result := IsLoading;
    if not Result then
    begin
      for ii := ControlCount - 1 downto 0 do
      begin
        ctl := Controls[ii];
        if (csLoading in ctl.ComponentState) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

begin
  if FBlockUpdate or FBlockResize or IsLoadingEx or IsDestroying or not CanBuildControls then
    Exit;

  FFirstLoad := False;

  FBlockUpdate := True;
  {$IFDEF FMXLIB}
  BeginUpdate;
  hs := Appearance.HorizontalSpacing;
  vs := Appearance.VerticalSpacing;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  hs := Round(Appearance.HorizontalSpacing);
  vs := Round(Appearance.VerticalSpacing);
  {$ENDIF}

  x := hs;
  if Appearance.DragGrip then
  begin
    {$IFDEF FMXLIB}
    x := x + GetDragGripRect.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    x := x + Round(GetDragGripRect.Right);
    {$ENDIF}
    if hs = 0 then
      x := x + ScalePaintValue(3);
  end;

  y := vs;

  xs := x;
  ys := y;

  hsm := ScalePaintValue(30);
  txth := 0;
  txtw := 0;
  sz := GetTextSize;
  if sz.cy > 0 then
    txth := Round(sz.cy + ScalePaintValue(4));

  if sz.cx > 0 then
    txtw := Round(sz.cx + ScalePaintValue(8));

  w := txtw;
  maxx := 0;
  h := 0;
  idx := 0;

  fu := False;
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    hc := GetHiddenControl(c);
    fu := not Assigned(hc);
    if fu then
      Break;
  end;

  l := False;
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    {$IFDEF FNCLIB}
    if Supports(c, IAdvAdaptToStyle, ia) then
      ia.AdaptToStyle := AdaptToStyle;
    {$ENDIF}

    if l then
    begin
      x := xs;
      y := ys + h - vs;
      ys := y;
    end;

    l := False;
    if c is TAdvCustomToolBarExElement then
    begin
      l := (c as TAdvCustomToolBarExElement).LastElement;
      (c as TAdvCustomToolBarExElement).OnUpdateToolBar := UpdateToolBar;
      (c as TAdvCustomToolBarExElement).OnUpdateToolBarControl := UpdateToolBarControl;
    end;

    DoIsLastElement(c, l);

    if l then
      l := true;

    if not (c = FOptionsMenuButton) then
    begin
      if fu then
      begin
        if c is TAdvCustomToolBarExElement then
          (c as TAdvCustomToolBarExElement).ControlIndex := idx
        else
          c.Tag := idx;
      end;

      if c.Visible then
      begin
        {$IFDEF FMXLIB}
        if AutoAlign then
        begin
          c.Position.X := x + c.Margins.Left;
          c.Position.Y := y + c.Margins.Top;
        end;

        if AutoStretchHeight or ((c is TAdvDefaultToolBarExButton) and ((c as TAdvDefaultToolBarExButton).Layout = bblLarge)) then
        begin
          h := Height - c.Margins.Bottom - txth - y;
          c.Height := h;
        end
        else
          h := Max(h, c.Position.y + c.Height + vs + c.Margins.Bottom);

        cn := GetControl(I + 1);
        if Assigned(cn) and (cn is TAdvDefaultToolBarExButton) then
        begin
          case (cn as TAdvDefaultToolBarExButton).Layout of
            bblBitmap, bblLabel:
            begin
              y := Max(y, c.Position.Y + c.Height + vs + c.Margins.Bottom);
              if y + cn.Margins.Top + cn.Height > Height - cn.Margins.Bottom - txth then
              begin
                x := Max(x, Max(maxx, c.Position.x + c.Width + hs + c.Margins.Right));
                y := ys;
              end
              else
              begin
                c.Position.X := x + c.Margins.Left;
                maxx := Max(x, Max(maxx, c.Position.x + c.Width + hs + c.Margins.Right));
              end;
            end;
            bblNone, bblLarge:
            begin
              y := ys;
              x := Max(x, c.Position.x + c.Width + hs + c.Margins.Right);
            end;
          end;
        end
        else
        begin
          y := ys;
          x := Max(x, Max(maxx, c.Position.x + c.Width + hs + c.Margins.Right));
        end;

        {$ENDIF}
        {$IFDEF VCLWEBLIB}
        if c.AlignWithMargins then
        begin
          if AutoAlign then
          begin
            c.Left := x + c.Margins.Left;
            c.Top := y + c.Margins.Top;
          end;

          if AutoStretchHeight or ((c is TAdvDefaultToolBarExButton) and ((c as TAdvDefaultToolBarExButton).Layout = bblLarge)) then
          begin
            h := Height - c.Margins.Bottom - txth - y;
            c.Height := h;
          end
          else
            h := Max(h, c.Top + c.Height + vs + c.Margins.Bottom);

          cn := GetControl(I + 1);
          if Assigned(cn) and (cn is TAdvDefaultToolBarExButton) then
          begin
            case (cn as TAdvDefaultToolBarExButton).Layout of
              bblBitmap, bblLabel:
              begin
                y := Max(y, c.Top + c.Height + vs + c.Margins.Bottom);
                if y + cn.Margins.Top + cn.Height > Height - cn.Margins.Bottom - txth then
                begin
                  x := Max(x, Max(maxx, c.Left + c.Width + hs + c.Margins.Right));
                  y := ys;
                end
                else
                begin
                  c.Left := x + c.Margins.Left;
                  maxx := Max(x, Max(maxx, c.Left + c.Width + hs + c.Margins.Right));
                end;
              end;
              bblNone, bblLarge:
              begin
                x := Max(x, c.Left + c.Width + hs + c.Margins.Right);
                y := ys;
              end;
            end;
          end
          else
          begin
            x := Max(x, Max(maxx, c.Left + c.Width + hs + c.Margins.Right));
            y := ys;
          end;
        end
        else
        begin
          if AutoAlign then
          begin
            c.Left := x;
            c.Top := y;
          end;

          if AutoStretchHeight or ((c is TAdvDefaultToolBarExButton) and ((c as TAdvDefaultToolBarExButton).Layout = bblLarge)) then
          begin
            h := Height - txth - y;
            c.Height := h
          end
          else
            h := Max(h, c.Top + c.Height + vs);

          cn := GetControl(I + 1);
          if Assigned(cn) and (cn is TAdvDefaultToolBarExButton) then
          begin
            case (cn as TAdvDefaultToolBarExButton).Layout of
              bblBitmap, bblLabel:
              begin
                y := Max(y, c.Top + c.Height + vs);
                if y + cn.Height > Height - txth then
                begin
                  x := Max(x, Max(maxx, c.Left + c.Width + hs));
                  y := ys;
                end
                else
                begin
                  c.Left := x;
                  maxx := Max(x, Max(maxx, c.Left + c.Width + hs));
                end;
              end;
              bblNone, bblLarge:
              begin
                x := Max(x, c.Left + c.Width + hs);
                y := ys;
              end;
            end;
          end
          else
          begin
            x := Max(x, Max(maxx, c.Left + c.Width + hs));
            y := ys;
          end;
        end;
        {$ENDIF}
        {$IFDEF LCLLIB}
        if AutoAlign then
        begin
          c.Left := x + c.BorderSpacing.Left;
          c.Top := y + c.BorderSpacing.Top;
        end;

        if AutoStretchHeight or ((c is TAdvDefaultToolBarExButton) and ((c as TAdvDefaultToolBarExButton).Layout = bblLarge)) then
        begin
          h := Height - c.BorderSpacing.Bottom - txth - y;
          c.Height := h;
        end
        else
          h := Max(h, c.Top + c.Height + vs + c.BorderSpacing.Bottom);

        cn := GetControl(I + 1);
        if Assigned(cn) and (cn is TAdvDefaultToolBarExButton) then
        begin
          case (cn as TAdvDefaultToolBarExButton).Layout of
            bblBitmap, bblLabel:
            begin
              y := Max(y, c.Top + c.Height + vs + c.BorderSpacing.Bottom);
              if y + cn.BorderSpacing.Top + cn.Height > Height - cn.BorderSpacing.Bottom - txth then
              begin
                x := Max(x, Max(maxx, c.Left + c.Width + hs + c.BorderSpacing.Right));
                y := ys;
              end
              else
              begin
                c.Left := x + c.BorderSpacing.Left;
                maxx := Max(x, Max(maxx, c.Left + c.Width + hs + c.BorderSpacing.Right));
              end;
            end;
            bblNone, bblLarge:
            begin
              x := Max(x, c.Left + c.Width + hs + c.BorderSpacing.Right);
              y := ys;
            end;
          end;
        end
        else
        begin
          x := Max(x, Max(maxx, c.Left + c.Width + hs + c.BorderSpacing.Right));
          y := ys;
        end;

        {$ENDIF}
        w := Max(w, x);
      end;
      inc(idx);
    end;
  end;

  if h = 0 then
    h := hsm;

  FOptionsMenuButton.Visible := OptionsMenu.ShowButton;
  if OptionsMenu.ShowButton then
    FOptionsMenuButton.Parent := Self
  else
    FOptionsMenuButton.Parent := nil;

  {$IFDEF FMXLIB}
  if FOptionsMenuButton.Visible then
  begin
    FOptionsMenuButton.Index := Self.ControlCount;
    FOptionsMenuButton.ControlIndex := Self.ControlCount - 1;
    FOptionsMenuButton.Position.Y := FOptionsMenuButton.Margins.Top;
    mgr := FOptionsMenuButton.Margins;

    if AutoSize then
    begin
      FOptionsMenuButton.Position.X := w + mgr.Left;
      if AutoHeight then
        FOptionsMenuButton.Height := h - mgr.Bottom - mgr.Top - txth
      else if AutoStretchHeight then
        FOptionsMenuButton.Height := Height - mgr.Bottom - mgr.Top - txth;
    end
    else
    begin
      FOptionsMenuButton.Position.X := Width - FOptionsMenuButton.Width - mgr.Right;
      FOptionsMenuButton.Height := Height - mgr.Bottom - mgr.Top - txth;
    end;
    w := w + FOptionsMenuButton.Width + mgr.Right + mgr.Left;
  end;

  if AutoSize then
  begin
    if Appearance.Separator then
      w := w + Appearance.SeparatorStroke.Width;

    w := Round(Max(MinimumWidth, w));

    if ControlAlignment = caNone then
    begin
      lm := 0;
      rm := 0;
      tm := 0;
      bm := 0;
      GetControlMargins(lm, tm, rm, bm);
      w := w + lm + rm;
    end;

    h := h + txth;

    if AutoHeight and AutoWidth and (ControlAlignment = caNone) then
      SetBounds(Position.X, Position.Y, w, h)
    else if AutoHeight and not (ControlAlignment in [caLeft, caRight, caClient]) then
      SetBounds(Position.X, Position.Y, Width, h)
    else if AutoWidth and not (ControlAlignment in [caTop, caBottom, caClient]) then
      SetBounds(Position.X, Position.Y, w, Height);
  end;

  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  if FOptionsMenuButton.Visible then
  begin
    FOptionsMenuButton.ControlIndex := Self.ControlCount - 1;
    {$IFDEF LCLLIB}
    FOptionsMenuButton.Top := FOptionsMenuButton.BorderSpacing.Top;
    mgr := FOptionsMenuButton.BorderSpacing;
    {$ENDIF}
    {$IFDEF VCLWEBLIB}
    FOptionsMenuButton.Top := FOptionsMenuButton.Margins.Top;
    mgr := FOptionsMenuButton.Margins;
    if not FOptionsMenuButton.AlignWithMargins then
    begin
      mgr.Left := 0;
      mgr.Top := 0;
      mgr.Right := 0;
      mgr.Bottom := 0;
    end;
    {$ENDIF}

    if AutoSize then
    begin
      FOptionsMenuButton.Left := w + mgr.Left;
      if AutoHeight then
        FOptionsMenuButton.Height := h - mgr.Bottom - mgr.Top
      else if AutoStretchHeight then
        FOptionsMenuButton.Height := Height - mgr.Bottom - txth - mgr.Top;
    end
    else
    begin
      FOptionsMenuButton.Left := Width - FOptionsMenuButton.Width - mgr.Right;
      FOptionsMenuButton.Height := Height - mgr.Bottom - mgr.Top - txth;
    end;
    w := w + FOptionsMenuButton.Width + mgr.Right + mgr.Left;
  end;

  if AutoSize then
  begin
    if Appearance.Separator then
      w := w + Round(Appearance.SeparatorStroke.Width);

    w := Round(Max(MinimumWidth, w));

    if ControlAlignment = caNone then
    begin
      lm := 0;
      rm := 0;
      tm := 0;
      bm := 0;
      GetControlMargins(lm, tm, rm, bm);
      w := w + Round(lm + rm);
    end;

    h := h + txth;

    if AutoHeight and AutoWidth and (ControlAlignment = caNone) then
      SetBounds(Left, Top, w, h)
    else if AutoHeight and not (ControlAlignment in [caLeft, caRight, caClient]) then
      SetBounds(Left, Top, Width, h)
    else if AutoWidth and not (ControlAlignment in [caTop, caBottom, caClient]) then
      SetBounds(Left, Top, w, Height);
  end;
  {$ENDIF}

  {$IFDEF FMXLIB}
  EndUpdate;
  {$ENDIF}

  if Assigned(OnUpdateControls) then
    OnUpdateControls(Self);

  FBlockUpdate := False;
end;

procedure TAdvCustomToolBarEx.UpdateDockPanel;
begin
  if Assigned(OnUpdateDockPanel) then
    OnUpdateDockPanel(Self);
end;

{ TAdvDefaultToolBarExButton }

{$IFNDEF WEBLIB}
{$IFDEF FNCLIB}
{$IFNDEF LCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 30}
procedure TAdvDefaultToolBarExButton.ActionChange(Sender: {$IFNDEF FMXLIB}TObject{$ELSE}TBasicAction{$ENDIF};
  CheckDefaults: Boolean);
var
  l: TCustomImageList;
  idx: Integer;
  b: TAdvBitmap;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    if (not CheckDefaults) or (Text = '') or (Text = Name) then
     Text := TCustomAction(Sender).Text;
  end;

  if (Sender is TCustomAction) and not CheckDefaults then
  begin
    if (TCustomAction(Sender).ActionList <> nil) and (TCustomAction(Sender).ActionList.Images is TCustomImageList) then
    begin
      l := TCustomImageList(TCustomAction(Sender).ActionList.Images);
      idx := TCustomAction(Sender).ImageIndex;
      b := TAdvBitmap.Create;
      try
        TAdvUtils.LoadBitmapFromImageList(b, l, idx);
        if not IsBitmapEmpty(b) then
        begin
          Bitmaps.Clear;
          Bitmaps.Add.Bitmap.Assign(b);
        end;
      finally
        b.free;
      end;
    end;
  end;
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TAdvDefaultToolBarExButton.AppearanceChanged;
begin
  Invalidate;
end;

procedure TAdvDefaultToolBarExButton.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundFillColor(c) then
  begin
    Appearance.NormalFill.Color := c;
    Appearance.NormalFill.Kind := gfkSolid;
    Appearance.DisabledFill.Color := Blend(c, gcDarkgray, 20);
    Appearance.DisabledFill.Kind := gfkSolid;
    Appearance.InnerStroke.Color := Blend(Appearance.NormalFill.Color, Appearance.NormalStroke.Color, 75);
    Appearance.InnerStroke.Kind := gskSolid;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleTextFontColor(c) then
    Font.Color := c;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleBackgroundStrokeColor(c) then
  begin
    Appearance.NormalStroke.Color := c;
    Appearance.DownStroke.Color := c;
    Appearance.DisabledStroke.Color := c;
    Appearance.NormalStroke.Color := c;
    Appearance.DownStroke.Color := c;
    Appearance.DisabledStroke.Color := c;
    Appearance.HoverStroke.Color := c;
  end;

  c := gcNull;
  if TAdvGraphicsStyles.GetStyleSelectionFillColor(c) then
  begin
    Appearance.HoverFill.Color := c;
    Appearance.HoverFill.Kind := gfkSolid;
    Appearance.DownFill.Color := Blend(c, Fill.Color, 20);
    Appearance.DownFill.Kind := gfkSolid;
  end;
end;

procedure TAdvDefaultToolBarExButton.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvDefaultToolBarExButton then
  begin
    FLayout := (Source as TAdvDefaultToolBarExButton).Layout;
    FCompactLayout := (Source as TAdvDefaultToolBarExButton).CompactLayout;
    FMinimumLayout := (Source as TAdvDefaultToolBarExButton).MinimumLayout;
    FMaximumLayout := (Source as TAdvDefaultToolBarExButton).MaximumLayout;
    FBitmapPosition := (Source as TAdvDefaultToolBarExButton).BitmapPosition;
    FBitmapSize := (Source as TAdvDefaultToolBarExButton).BitmapSize;
    FLargeLayoutBitmapSize := (Source as TAdvDefaultToolBarExButton).LargeLayoutBitmapSize;
    FAutoBitmapSize := (Source as TAdvDefaultToolBarExButton).AutoBitmapSize;
    FLargeLayoutAutoBitmapSize := (Source as TAdvDefaultToolBarExButton).LargeLayoutAutoBitmapSize;
    FStretchBitmapIfNoText := (Source as TAdvDefaultToolBarExButton).StretchBitmapIfNoText;
    FAppearance.Assign((Source as TAdvDefaultToolBarExButton).Appearance);
    FPictureContainer := (Source as TAdvDefaultToolBarExButton).PictureContainer;
    FBitmaps.Assign((Source as TAdvDefaultToolBarExButton).Bitmaps);
    FDisabledBitmaps.Assign((Source as TAdvDefaultToolBarExButton).DisabledBitmaps);
    FLargeLayoutHoverBitmaps.Assign((Source as TAdvDefaultToolBarExButton).LargeLayoutHoverBitmaps);
    FLargeLayoutBitmaps.Assign((Source as TAdvDefaultToolBarExButton).LargeLayoutBitmaps);
    FLargeLayoutDisabledBitmaps.Assign((Source as TAdvDefaultToolBarExButton).LargeLayoutDisabledBitmaps);
    FHoverBitmaps.Assign((Source as TAdvDefaultToolBarExButton).HoverBitmaps);
    FBitmapVisible := (Source as TAdvDefaultToolBarExButton).BitmapVisible;
    FText := (Source as TAdvDefaultToolBarExButton).Text;
    FTextVisible := (Source as TAdvDefaultToolBarExButton).TextVisible;
    FDropDownHeight := (Source as TAdvDefaultToolBarExButton).DropDownHeight;
    FDropDownWidth := (Source as TAdvDefaultToolBarExButton).DropDownWidth;
    FDropDownAutoWidth := (Source as TAdvDefaultToolBarExButton).DropDownAutoWidth;
    FWordWrapping := (Source as TAdvDefaultToolBarExButton).WordWrapping;
    FHorizontalTextAlign := (Source as TAdvDefaultToolBarExButton).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvDefaultToolBarExButton).VerticalTextAlign;
    FTrimming := (Source as TAdvDefaultToolBarExButton).Trimming;
    FFont.Assign((Source as TAdvDefaultToolBarExButton).Font);
    FHoverFontColor := (Source as TAdvDefaultToolBarExButton).HoverFontColor;
    FDownFontColor := (Source as TAdvDefaultToolBarExButton).DownFontColor;
    FDisabledFontColor := (Source as TAdvDefaultToolBarExButton).DisabledFontColor;
    FStretchText := (Source as TAdvDefaultToolBarExButton).StretchText;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.BitmapsChanged(Sender: TObject);
var
  bmp: TAdvBitmap;
begin
  bmp := TAdvGraphics.GetScaledBitmap(Bitmaps, 0, PictureContainer);
  BitmapVisible := Assigned(bmp) and not IsBitmapEmpty(bmp);

  if not BitmapVisible and (Layout = bblLarge) then
  begin
    bmp := TAdvGraphics.GetScaledBitmap(LargeLayoutBitmaps, 0, PictureContainer);
    BitmapVisible := Assigned(bmp) and not IsBitmapEmpty(bmp);
  end;

  Invalidate;
end;

function TAdvDefaultToolBarExButton.CanChangeText: Boolean;
begin
  Result := True;
end;

function TAdvDefaultToolBarExButton.CanDrawDesignTime: Boolean;
begin
  Result := False;
end;

function TAdvDefaultToolBarExButton.CanDropDown: Boolean;
begin
  Result := False;
end;

procedure TAdvDefaultToolBarExButton.ChangeDPIScale(M, D: Integer);
begin
  inherited;

  BeginUpdate;
  Font.Height := TAdvUtils.MulDivInt(Font.Height, M, D);
  BitmapSize := TAdvUtils.MulDivSingle(BitmapSize, M, D);
  LargeLayoutBitmapSize := TAdvUtils.MulDivSingle(LargeLayoutBitmapSize, M, D);

  if FOldHeight <> -1 then
    FOldHeight := TAdvUtils.MulDivSingle(FOldHeight, M, D);
  FDropDownHeight := TAdvUtils.MulDivSingle(FDropDownHeight, M, D);
  FDropDownWidth := TAdvUtils.MulDivSingle(FDropDownWidth, M, D);

  FPopup.DropDownWidth := TAdvUtils.MulDivSingle(FPopup.DropDownWidth, M, D);
  FPopup.DropDownHeight := TAdvUtils.MulDivSingle(FPopup.DropDownHeight, M, D);

//  FAppearance.Rounding := TAdvUtils.MulDivSingle(FAppearance.Rounding, M, D);
  EndUpdate;
end;

procedure TAdvDefaultToolBarExButton.CloseDropDown;
begin
  if not Assigned(FPopup) then
    Exit;

  if FPopup.IsOpen then
    FPopup.IsOpen := False;
end;

constructor TAdvDefaultToolBarExButton.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF FMXLIB}
  EnableExecuteAction := True;
  {$ENDIF}
  FOldHeight := -1;
  FLayout := bblNone;
  FCompactLayout := bblNone;
  FMinimumLayout := bblBitmap;
  FMaximumLayout := bblLarge;

  FFont := TAdvGraphicsFont.Create;
  FFont.OnChanged := DoFontChanged;

  FHoverFontColor := gcNull;
  FDownFontColor := gcNull;
  FDisabledFontColor := gcNull;

  FDownState := False;
  FApplyName := True;
  FCloseOnSelection := True;
  FBitmapSize := 24;
  FLargeLayoutBitmapSize := 32;
  FAutoBitmapSize := False;
  FLargeLayoutAutoBitmapSize := False;
  FStretchBitmapIfNoText := True;
  FStretchText := False;

  FDropDownWidth := 135;
  FDropDownHeight := 135;
  FDropDownAutoWidth := False;
  FPopup := TAdvPopupEx.Create(Self);
  FPopup.DragWithParent := True;
  FPopup.DropDownWidth := FDropDownWidth;
  FPopup.DropDownHeight := FDropDownHeight;
  FPopup.OnPopup := DoPopup;
  FPopup.OnClosePopup := DoClosePopup;
  FPopupPlacement := ppBottom;

  FBitmaps := TAdvScaledBitmaps.Create(Self);
  FBitmaps.OnChange := BitmapsChanged;
  FDisabledBitmaps := TAdvScaledBitmaps.Create(Self);
  FDisabledBitmaps.OnChange := BitmapsChanged;
  FHoverBitmaps := TAdvScaledBitmaps.Create(Self);
  FHoverBitmaps.OnChange := BitmapsChanged;

  FLargeLayoutBitmaps := TAdvScaledBitmaps.Create(Self);
  FLargeLayoutBitmaps.OnChange := BitmapsChanged;
  FLargeLayoutDisabledBitmaps := TAdvScaledBitmaps.Create(Self);
  FLargeLayoutDisabledBitmaps.OnChange := BitmapsChanged;
  FLargeLayoutHoverBitmaps := TAdvScaledBitmaps.Create(Self);
  FLargeLayoutHoverBitmaps.OnChange := BitmapsChanged;

  FAppearance := TAdvToolBarExButtonAppearance.Create(Self);

  FTextVisible := True;
  FBitmapVisible := False;
  FWordWrapping := False;
  FHorizontalTextAlign := gtaCenter;
  FVerticalTextAlign := gtaCenter;
  FTrimming := gttCharacter;
  FBitmapPosition := bbpLeft;

  Width := 100;
  Height := 24;
end;

procedure TAdvDefaultToolBarExButton.SetDisabledBitmaps(const Value: TAdvScaledBitmaps);
begin
  FDisabledBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetDisabledFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetHoverBitmaps(const Value: TAdvScaledBitmaps);
begin
  FHoverBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetDownFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FDownFontColor <> Value then
  begin
    FDownFontColor := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetHoverFontColor(
  const Value: TAdvGraphicsColor);
begin
  if FHoverFontColor <> Value then
  begin
    FHoverFontColor := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetMaximumLayout(
  const Value: TAdvToolBarExButtonLayout);
begin
  if FMaximumLayout <> Value then
  begin
    FMaximumLayout := Value;
    UpdateToolBar;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetMinimumLayout(
  const Value: TAdvToolBarExButtonLayout);
begin
  if FMinimumLayout <> Value then
  begin
    FMinimumLayout := Value;
    UpdateToolBar;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetDownState(const Value: Boolean);
begin
  FDownState := Value;
  Invalidate;
end;

procedure TAdvDefaultToolBarExButton.SetDropDownControl(const Value: TControl);
begin
  FDropDownControl := Value;
  if Assigned(FDropDownControl) then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(FPopup) then
      begin
        FPopup.ContentControl := FDropDownControl;
        FPopup.FocusedControl := FDropDownControl;
      end;
    end
    else if (csDesigning in ComponentState) then
    begin
      DropDownHeight := FDropDownControl.Height;
      DropDownWidth := FDropDownControl.Width;
    end;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetDropDownAutoWidth(
  const Value: Boolean);
begin
  if FDropDownAutoWidth <> Value then
  begin
    FDropDownAutoWidth := Value;
    if Assigned(FPopup) then
    begin
      if FDropDownAutoWidth then
        FPopup.DropDownWidth := Width
      else
        FPopup.DropDownWidth := FDropDownWidth;
    end;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetDropDownHeight(const Value: Single);
begin
  FDropDownHeight := Value;
  if Assigned(FPopup) and not DropDownAutoWidth then
    FPopup.DropDownHeight := FDropDownHeight;
end;

procedure TAdvDefaultToolBarExButton.SetDropDownWidth(const Value: Single);
begin
  FDropDownWidth := Value;
  if Assigned(FPopup) then
    FPopup.DropDownWidth := FDropDownWidth;
end;

procedure TAdvDefaultToolBarExButton.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetHorizontalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarExButton.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  if Assigned(FDropDownButton) then
    FDropDownButton.AdaptToStyle := AdaptToStyle;
end;

procedure TAdvCustomToolBarExButton.SetDropDownKind(
  const Value: TAdvToolBarExButtonDropDownKind);
begin
  if FDropDownKind <> Value then
  begin
    FDropDownKind := Value;
    if Assigned(FDropDownButton) then
    begin
      case DropDownKind of
        ddkNormal:
        begin
          FDropDownButton.Visible := False;
          FDropDownButton.Parent := nil;
          {$IFDEF FMXLIB}
          FDropDownButton.HitTest := True;
          {$ENDIF}
          FDropDownButton.Appearance.Transparent := False;
        end;
        ddkDropDown:
        begin
          FDropDownButton.Visible := False;
          FDropDownButton.Parent := Self;
          {$IFDEF FMXLIB}
          FDropDownButton.HitTest := False;
          {$ENDIF}
          FDropDownButton.Appearance.Transparent := True;
        end;
        ddkDropDownButton:
        begin
          FDropDownButton.Visible := True;
          FDropDownButton.Parent := Self;
          {$IFDEF FMXLIB}
          FDropDownButton.HitTest := True;
          {$ENDIF}
          FDropDownButton.Appearance.Transparent := False;
        end;
      end;
    end;
    Invalidate;
  end;
end;

procedure TAdvCustomToolBarExButton.SetDropDownPosition(const Value: TAdvToolBarExButtonDropDownPosition);
var
  sc: Single;
begin
  if FDropDownPosition <> Value then
  begin
    sc := TAdvUtils.GetDPIScale(Self, 96);

    FDropDownPosition := Value;
    if Assigned(FDropDownButton) then
    begin
      case Value of
        ddpRight:
        begin
          {$IFDEF FMXLIB}
          FDropDownButton.Align := TAlignLayout.Right;
          {$ENDIF}
          {$IFDEF CMNWEBLIB}
          FDropDownButton.Align := alRight;
          {$ENDIF}
          FDropDownButton.Width := Round(sc *17);
          FDropDownButton.Appearance.Corners := [gcTopRight, gcBottomRight];
        end;
        ddpBottom:
        begin
          {$IFDEF FMXLIB}
          FDropDownButton.Align := TAlignLayout.Bottom;
          {$ENDIF}
          {$IFDEF CMNWEBLIB}
          FDropDownButton.Align := alBottom;
          {$ENDIF}
          FDropDownButton.Height := Round(sc * 10);
          FDropDownButton.Appearance.Corners := [gcBottomLeft, gcBottomRight];
        end;
      end;
    end;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarExButton.SetEnabled(const Value: Boolean);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomToolBarExButton.SetEnabled(Value: Boolean);
{$ENDIF}
var
  d: TAdvToolBarExDropDownButton;
begin
  inherited;
  d := GetDropDownButtonControl;
  if Assigned(d) then
    d.Enabled := Value;
end;

procedure TAdvCustomToolBarExButton.SetHidden(const Value: Boolean);
begin
  if FHidden <> Value then
  begin
    FHidden := Value;
    UpdateToolBarControl;
  end;
end;

procedure TAdvCustomToolBarExButton.UpdateDropDownButton;
begin
  if Assigned(FDropDownButton) then
    FDropDownButton.Appearance.Rounding := Appearance.Rounding;
end;

destructor TAdvDefaultToolBarExButton.Destroy;
begin
  FFont.Free;
  FPopup.Free;
  FBitmaps.Free;
  FDisabledBitmaps.Free;
  FHoverBitmaps.Free;
  FLargeLayoutBitmaps.Free;
  FLargeLayoutDisabledBitmaps.Free;
  FLargeLayoutHoverBitmaps.Free;
  FAppearance.Free;
  inherited;
end;

procedure TAdvDefaultToolBarExButton.DoPopup(Sender: TObject);
begin
  InitializePopup;
end;

procedure TAdvDefaultToolBarExButton.HandleMouseEnter;
begin
  inherited;
end;

procedure TAdvDefaultToolBarExButton.HandleMouseLeave;
begin
  inherited;
  FDown := False;
  FHover := False;
  FPrevHover := False;
  Invalidate;
end;

procedure TAdvCustomToolBarExButton.DoFontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomToolBarExButton.AppearanceChanged;
begin
  inherited;
  UpdateDropDownButton;
end;

procedure TAdvCustomToolBarExButton.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomToolBarExButton then
  begin
    FDropDownKind := (Source as TAdvCustomToolBarExButton).DropDownKind;
    FDropDownPosition := (Source as TAdvCustomToolBarExButton).DropDownPosition;
    FAutoOptionsMenuText := (Source as TAdvCustomToolBarExButton).AutoOptionsMenuText;
    FHidden := (Source as TAdvCustomToolBarExButton).Hidden;
    Invalidate;
  end;
end;

function TAdvCustomToolBarExButton.CanDropDown: Boolean;
begin
  Result := DropDownKind <> ddkNormal;
end;

procedure TAdvCustomToolBarExButton.ChangeDPIScale(M, D: Integer);
begin
  inherited;

end;

constructor TAdvCustomToolBarExButton.Create(AOwner: TComponent);
var
  res: string;
begin
  inherited;
  FHidden := False;
  FDropDownPosition := ddpRight;
  FDropDownButton := GetDropDownButtonClass.Create(Self);
  FDropDownButton.TabStop := False;
  FDropDownButton.Text := '';
  FDropDownButton.OnClick := DropDownButtonClick;
  FDropDownButton.Visible := False;
  FDropDownButton.Width := 17;
  FDropDownButton.Appearance.Corners := [gcTopRight, gcBottomRight];
  {$IFDEF FMXLIB}
  FDropDownButton.Align := TAlignLayout.Right;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FDropDownButton.Align := alRight;
  {$ENDIF}
  FDropDownButton.Stored := False;

  {$IFNDEF WEBLIB}
  res := 'AdvTOOLBAREXPANDSVG';
  {$ELSE}
  res := AdvTOOLBAREXPANDSVG;
  {$ENDIF}

  FDropDownButton.Bitmaps.AddBitmapFromResource(res, HInstance);
  FDropDownButton.DisabledBitmaps.AddBitmapFromResource(res, HInstance);
  FDropDownButton.HoverBitmaps.AddBitmapFromResource(res, HInstance);
end;

destructor TAdvCustomToolBarExButton.Destroy;
begin
  FDropDownButton.Free;
  inherited;
end;

procedure TAdvCustomToolBarExButton.DrawButton(AGraphics: TAdvGraphics);
var
  r: TRectF;
begin
  inherited;
  if Assigned(FDropDownButton) and (DropDownKind = ddkDropDown) then
  begin
    case DropDownPosition of
      ddpRight: r := RectF(Width - FDropDownButton.Width, 0, Width, Height);
      ddpBottom: r := RectF(0, Height - FDropDownButton.Height, Width, Height);
    end;
    FDropDownButton.DrawBitmap(AGraphics, r);
  end;
end;

procedure TAdvCustomToolBarExButton.DropDownButtonClick(Sender: TObject);
begin
  DropDown;
end;

procedure TAdvDefaultToolBarExButton.DrawBitmap(AGraphics: TAdvGraphics; ARect: TRectF);
var
  r: TRectF;
  bmpa: TBitmap;
  bmp: TAdvBitmap;
  g: TAdvGraphics;
  x, y: Integer;
begin
  if not BitmapVisible then
    Exit;

  r := GetBitmapRect(ARect);

  if (Self is TAdvToolBarExDropDownButton) and AdaptToStyle then
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(ScalePaintValue(7), ScalePaintValue(7));
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TAdvGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := TAdvGraphics.DefaultSelectionFillColor;
//      g.DrawLine(PointF(0, 1), PointF(6, 1), gcpmRightDown);
//      g.DrawLine(PointF(0, 2), PointF(6, 2), gcpmRightDown);
//      g.DrawLine(PointF(1, 3), PointF(5, 3), gcpmRightDown);
//      g.DrawLine(PointF(2, 4), PointF(4, 4), gcpmRightDown);
//      g.DrawLine(PointF(3, 5), PointF(3, 5), gcpmRightDown);

      for y := ScalePaintValue(1) to ScalePaintValue(5) do
      begin
        x := Max(0, y - ScalePaintValue(2));
        g.DrawLine(PointF(x, y), PointF((bmpa.Width - 1) - x, y), gcpmRightDown);
        if x >= ((bmpa.Width - 1) - x) then
          Break;
      end;

    finally
      g.EndScene;
      g.Free;
    end;

    try
      bmp := TAdvBitmap.Create;
      try
        bmp.Assign(bmpa);
        AGraphics.DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    finally
      bmpa.Free;
    end;
  end
  else
  begin
    AGraphics.PictureContainer := PictureContainer;

    case Layout of
      bblLarge:
      begin
        if Enabled then
        begin
          if FHover and (LargeLayoutHoverBitmaps.Count > 0) then
            AGraphics.DrawScaledBitmap(r, LargeLayoutHoverBitmaps, ResourceScaleFactor)
          else if FHover and (HoverBitmaps.Count > 0) then
            AGraphics.DrawScaledBitmap(r, HoverBitmaps, ResourceScaleFactor)
          else if LargeLayoutBitmaps.Count > 0 then
            AGraphics.DrawScaledBitmap(r, LargeLayoutBitmaps, ResourceScaleFactor)
          else
            AGraphics.DrawScaledBitmap(r, Bitmaps, ResourceScaleFactor)
        end
        else
        begin
          if LargeLayoutDisabledBitmaps.Count > 0 then
            AGraphics.DrawScaledBitmap(r, LargeLayoutDisabledBitmaps, ResourceScaleFactor)
          else if DisabledBitmaps.Count > 0 then
            AGraphics.DrawScaledBitmap(r, DisabledBitmaps, ResourceScaleFactor)
          else
            AGraphics.DrawScaledBitmap(r, Bitmaps, ResourceScaleFactor)
        end;
      end
      else
      begin
        if Enabled then
        begin
          if FHover and (HoverBitmaps.Count > 0) then
            AGraphics.DrawScaledBitmap(r, HoverBitmaps, ResourceScaleFactor)
          else
            AGraphics.DrawScaledBitmap(r, Bitmaps, ResourceScaleFactor)
        end
        else if DisabledBitmaps.Count > 0 then
          AGraphics.DrawScaledBitmap(r, DisabledBitmaps, ResourceScaleFactor)
        else
          AGraphics.DrawScaledBitmap(r, Bitmaps, ResourceScaleFactor)
      end;
    end;
  end;
end;

procedure TAdvDefaultToolBarExButton.DrawButton(AGraphics: TAdvGraphics);
begin

end;

procedure TAdvDefaultToolBarExButton.DrawText(AGraphics: TAdvGraphics);
var
  tr: TRectF;
  st: TAdvGraphicsSaveState;
begin
  if not TextVisible or (Text = '') then
    Exit;

  tr := GetTextRect;
  st := AGraphics.SaveState;
  try
    AGraphics.ClipRect(tr);
    AGraphics.Font.Assign(Font);

    if Enabled then
    begin
      if (FDown or FDownState) and (DownFontColor <> gcNull) then
        AGraphics.Font.Color := DownFontColor
      else if FHover and (HoverFontColor <> gcNull) then
        AGraphics.Font.Color := HoverFontColor
    end
    else if DisabledFontColor <> gcNull then
      AGraphics.Font.Color := DisabledFontColor;

    AGraphics.DrawText(tr, Text, WordWrapping, HorizontalTextAlign, VerticalTextAlign, Trimming);
  finally
    AGraphics.RestoreState(st);
  end;
end;

procedure TAdvDefaultToolBarExButton.DropDown;
begin
  if not Assigned(FPopup) then
    Exit;

  FPopup.PlacementControl := Self;

  if not FPopup.IsOpen then
  begin
    if Assigned(OnBeforeDropDown) then
      OnBeforeDropDown(Self);

    if DropDownAutoWidth then
      FPopup.DropDownWidth := Width;

    FPopup.Placement := FPopupPlacement;
    FPopup.IsOpen := True;
    if Assigned(OnDropDown) then
      OnDropDown(Self);
  end
  else
    FPopup.IsOpen := False;
end;

procedure TAdvDefaultToolBarExButton.DoFontChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvCustomToolBarExButton.GetBitmapRect: TRectF;
begin
  Result := inherited GetBitmapRect;
end;

function TAdvCustomToolBarExButton.GetBitmapRect(ARect: TRectF): TRectF;
var
  r: TRectF;
  dr: TRectf;
  bs: Single;
begin
  bs := GetBitmapSize;
  case Layout of
    bblLarge: bs := GetLargeLayoutBitmapSize;
  end;

  r := ARect;
  Result := RectF(r.Left, r.Top, r.Left, r.Bottom);
  if BitmapVisible then
  begin
    if DropDownKind <> ddkNormal then
    begin
      dr := GetDropDownRect;
      case BitmapPosition of
        bbpLeft:
        begin
          case DropDownPosition of
            ddpRight: Result := RectF(Result.Left + 3, Result.Top + 3, Result.Left + 3 + bs, Result.Bottom - 3);
            ddpBottom: Result := RectF(Result.Left + 3, Result.Top + 3, Result.Left + 3 + bs, dr.Top - 3);
          end;
        end;
        bbpTop:
        begin
          case DropDownPosition of
            ddpRight: Result := RectF(Result.Left + 3, Result.Top + 3, dr.Left - 3, Result.Top + 3 + bs);
            ddpBottom: Result := RectF(Result.Left + 3, Result.Top + 3, r.Right - 3, Result.Top + 3 + bs);
          end;
        end;
      end;
    end
    else if not TextVisible or (Text = '') then
    begin
      Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);
      if (bs <> -1) and not StretchBitmapIfNoText then
      begin
        Result.Left := Round(r.Left + ((r.Right - r.Left) - bs) / 2);
        Result.Top := Round(r.Top + ((r.Bottom - r.Top) - bs) / 2);
        Result.Right := Result.Left + bs;
        Result.Bottom := Result.Top + bs;
      end;      
    end
    else if TextVisible and not (Text = '') then
    begin
      case BitmapPosition of
        bbpLeft: Result := RectF(r.Left + 3, r.Top + 3, r.Left + 3 + bs, r.Bottom - 3);
        bbpTop: Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, r.Top + 3 + bs);
      end;
    end;
  end;
end;

function TAdvCustomToolBarExButton.GetDropDownButtonClass: TAdvToolBarExDropDownButtonClass;
begin
  Result := TAdvToolBarExDropDownButton;
end;

function TAdvCustomToolBarExButton.GetDropDownButtonControl: TAdvToolBarExDropDownButton;
begin
  Result := FDropDownButton;
end;

function TAdvCustomToolBarExButton.GetDropDownRect: TRectF;
begin
  Result := inherited GetDropDownRect;
  if Assigned(FDropDownButton) and (DropDownKind <> ddkNormal) then
  begin
    if DropDownKind = ddkDropDown then
    begin
      case DropDownPosition of
        ddpRight: Result := RectF(Width - FDropDownButton.Width, 0, Width, Height);
        ddpBottom: Result := RectF(0, Height - FDropDownButton.Height, Width, Height);
      end;
    end
    else
    begin
      {$IFDEF FMXLIB}
      Result := RectF(FDropDownButton.Position.X, FDropDownButton.Position.Y, FDropDownButton.Position.X + FDropDownButton.Width, FDropDownButton.Position.Y + FDropDownButton.Height);
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      Result := RectF(FDropDownButton.Left, FDropDownButton.Top, FDropDownButton.Left + FDropDownButton.Width, FDropDownButton.Top + FDropDownButton.Height);
      {$ENDIF}
    end;
  end;
end;

function TAdvCustomToolBarExButton.GetTextRect: TRectF;
var
  bmpr: TRectF;
  dr: TRectF;
  r: TRectF;
begin
  r := LocalRect;
  bmpr := GetBitmapRect(r);
  dr := GetDropDownRect;
  if DropDownKind <> ddkNormal then
  begin
    if not StretchText then
    begin
      case BitmapPosition of
        bbpLeft:
        begin
          case DropDownPosition of
            ddpRight: Result := RectF(bmpr.Right + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
            ddpBottom: Result := RectF(bmpr.Right + 3, r.Top + 3, r.Right - 3, dr.Top - 3);
          end;
        end;
        bbpTop:
        begin
          case DropDownPosition of
            ddpRight: Result := RectF(r.Left + 3, bmpr.Bottom + 3, dr.Left - 3, r.Bottom - 3);
            ddpBottom: Result := RectF(r.Left + 3, bmpr.Bottom + 3, r.Right - 3, dr.Top - 3);
          end;
        end;
      end;
    end
    else
    begin
      case DropDownPosition of
        ddpRight: Result := RectF(r.Left + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
        ddpBottom: Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, dr.Top - 3);
      end;
    end;
  end
  else
  begin
    if not StretchText then
    begin
      case BitmapPosition of
        bbpLeft: Result := RectF(bmpr.Right + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
        bbpTop: Result := RectF(r.Left + 3, bmpr.Bottom + 3, dr.Left - 3, r.Bottom - 3)
      end;
    end
    else
      Result := RectF(r.Left + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
  end;
end;

procedure TAdvCustomToolBarExButton.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if DropDownKind = ddkDropDown then
    DropDown;
end;

procedure TAdvCustomToolBarExButton.Loaded;
begin
  inherited;
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  FDropDownButton.ScaleForPPI(CurrentPPI);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
end;

procedure TAdvCustomToolBarExButton.LoadSettingsFromFile(AFileName: string);
begin
  inherited;
  if Assigned(FDropDownButton) then
  begin
    FDropDownButton.Appearance.ShowInnerStroke := Appearance.ShowInnerStroke;
    FDropDownButton.Appearance.InnerStroke.Assign(Appearance.InnerStroke);
    FDropDownButton.Appearance.NormalFill.Assign(Appearance.NormalFill);
    FDropDownButton.Appearance.NormalStroke.Assign(Appearance.NormalStroke);
    FDropDownButton.Appearance.HoverFill.Assign(Appearance.HoverFill);
    FDropDownButton.Appearance.HoverStroke.Assign(Appearance.HoverStroke);
    FDropDownButton.Appearance.DownFill.Assign(Appearance.DownFill);
    FDropDownButton.Appearance.DownStroke.Assign(Appearance.DownStroke);
    FDropDownButton.Appearance.DisabledFill.Assign(Appearance.DisabledFill);
    FDropDownButton.Appearance.DisabledStroke.Assign(Appearance.DisabledStroke);
    FDropDownButton.Appearance.FlatStyle := Appearance.FlatStyle;
    FDropDownButton.Fill.Assign(Fill);
    FDropDownButton.Stroke.Assign(Stroke);
  end;
end;

procedure TAdvCustomToolBarExButton.LoadSettingsFromStream(AStream: TStreamEx);
begin
  inherited;
  if Assigned(FDropDownButton) then
  begin
    FDropDownButton.Appearance.ShowInnerStroke := Appearance.ShowInnerStroke;
    FDropDownButton.Appearance.InnerStroke.Assign(Appearance.InnerStroke);
    FDropDownButton.Appearance.NormalFill.Assign(Appearance.NormalFill);
    FDropDownButton.Appearance.NormalStroke.Assign(Appearance.NormalStroke);
    FDropDownButton.Appearance.HoverFill.Assign(Appearance.HoverFill);
    FDropDownButton.Appearance.HoverStroke.Assign(Appearance.HoverStroke);
    FDropDownButton.Appearance.DownFill.Assign(Appearance.DownFill);
    FDropDownButton.Appearance.DownStroke.Assign(Appearance.DownStroke);
    FDropDownButton.Appearance.DisabledFill.Assign(Appearance.DisabledFill);
    FDropDownButton.Appearance.DisabledStroke.Assign(Appearance.DisabledStroke);
    FDropDownButton.Appearance.FlatStyle := Appearance.FlatStyle;
    FDropDownButton.Fill.Assign(Fill);
    FDropDownButton.Stroke.Assign(Stroke);
  end;
end;

function TAdvDefaultToolBarExButton.GetBitmap: TAdvBitmap;
begin
  Result := TAdvGraphics.GetScaledBitmap(Bitmaps, 0, FPictureContainer);
end;

function TAdvDefaultToolBarExButton.GetPictureContainer: TPictureContainer;
begin
  Result := FPictureContainer;
end;

function TAdvDefaultToolBarExButton.GetBitmapRect(ARect: TRectF): TRectF;
var
  r: TRectF;
  bs: Single;
begin
  bs := GetBitmapSize;
  case Layout of
    bblLarge: bs := GetLargeLayoutBitmapSize;
  end;

  r := ARect;
  Result := RectF(r.Left, r.Top, r.Left, r.Bottom);
  if BitmapVisible then
  begin
    case BitmapPosition of
      bbpLeft: Result := RectF(Result.Left + 3, Result.Top + 3, Result.Left + 3 + bs, Result.Bottom - 3);
      bbpTop: Result := RectF(Result.Left + 3, Result.Top + 3, Result.Right - 3, Result.Top + 3 + bs);
    end;
  end;
end;

function TAdvDefaultToolBarExButton.GetBitmapSize: Single;
var
  bmp: TAdvBitmap;
begin
  Result := BitmapSize;
  if AutoBitmapSize then
  begin
    bmp := TAdvGraphics.GetScaledBitmap(Bitmaps, 0, PictureContainer);
    if Assigned(bmp) and not IsBitmapEmpty(bmp) then
      Result := Max(bmp.Height, bmp.Width);
  end;
end;

function TAdvDefaultToolBarExButton.GetBitmapRect: TRectF;
begin
  Result := GetBitmapRect(LocalRect);
end;

function TAdvDefaultToolBarExButton.GetDropDownRect: TRectF;
var
  r: TRectF;
begin
  r := LocalRect;
  Result := RectF(r.Right, r.Top, r.Right, r.Bottom);
end;

function TAdvDefaultToolBarExButton.GetHintString: String;
begin
  Result := Hint;
end;

function TAdvDefaultToolBarExButton.GetLargeLayoutBitmapSize: Single;
var
  bmp: TAdvBitmap;
begin
  Result := LargeLayoutBitmapSize;
  if LargeLayoutAutoBitmapSize then
  begin
    bmp := TAdvGraphics.GetScaledBitmap(LargeLayoutBitmaps, 0, PictureContainer);
    if Assigned(bmp) and not IsBitmapEmpty(bmp) then
      Result := Max(bmp.Height, bmp.Width);
  end;
end;

function TAdvDefaultToolBarExButton.GetPopupControl: TAdvPopupEx;
begin
  Result := FPopup;
end;

function TAdvDefaultToolBarExButton.GetText: String;
begin
  Result := FText;
end;

function TAdvDefaultToolBarExButton.GetTextRect: TRectF;
var
  bmpr: TRectF;
  dr: TRectF;
  r: TRectF;
begin
  r := LocalRect;
  bmpr := GetBitmapRect;
  dr := GetDropDownRect;
  if not StretchText then
  begin
    case BitmapPosition of
      bbpLeft: Result := RectF(bmpr.Right + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
      bbpTop: Result := RectF(r.Left + 3, bmpr.Bottom + 3, dr.Left - 3, r.Bottom - 3);
    end;
  end
  else
    Result := RectF(r.Left + 3, r.Top + 3, dr.Left - 3, r.Bottom - 3);
end;

function TAdvDefaultToolBarExButton.GetTextSize: TSizeF;
var
  g: TAdvGraphics;
  r: TRectF;
begin
  Result.cx := 0;
  Result.cy := 0;
  if TextVisible and (Text <> '') then
  begin
    g := TAdvGraphics.CreateBitmapCanvas(1, 1, NativeCanvas);
    if not NativeCanvas then
      g.BeginScene;
    g.PictureContainer := PictureContainer;
    g.OptimizedHTMLDrawing := OptimizedHTMLDrawing;
    try
      g.Font.Assign(Font);
      r := LocalRect;
      InflateRectEx(r, ScalePaintValue(-2), ScalePaintValue(-2));
      Result := g.CalculateTextSize(Text, r, WordWrapping);
    finally
      if not NativeCanvas then
        g.EndScene;
      g.Free;
    end;
  end;
end;

procedure TAdvDefaultToolBarExButton.InitializePopup;
begin

end;

function TAdvDefaultToolBarExButton.IsBitmapSizeStored: Boolean;
begin
  Result := BitmapSize <> 24;
end;

function TAdvDefaultToolBarExButton.IsDropDownHeightStored: Boolean;
begin
  Result := DropDownHeight <> 135;
end;

function TAdvDefaultToolBarExButton.IsDropDownWidthStored: Boolean;
begin
  Result := DropDownWidth <> 135;
end;

function TAdvDefaultToolBarExButton.IsLargeLayoutBitmapSizeStored: Boolean;
begin
  Result := LargeLayoutBitmapSize <> 32;
end;

procedure TAdvDefaultToolBarExButton.HandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    KEY_HOME: SelectFirstValue;
    KEY_END: SelectLastValue;
    KEY_UP, KEY_LEFT:  SelectPreviousValue;
    KEY_DOWN, KEY_RIGHT: SelectNextValue;
    KEY_SPACE: Click;
  end;
end;

procedure TAdvDefaultToolBarExButton.HandleKeyPress(var Key: Char);
begin
  inherited;
  if (Key >= #32) and not (Ord(Key) in [KEY_F2, KEY_RETURN, KEY_SPACE, KEY_ESCAPE, KEY_HOME, KEY_END, KEY_UP, KEY_LEFT, KEY_DOWN, KEY_RIGHT]) then
    SelectValueWithCharacter(Key);
end;

procedure TAdvDefaultToolBarExButton.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift <> [] then
    Exit;

  case Key of
    KEY_RETURN, KEY_F4:
    begin
      if CanDropDown then
        DropDown;
    end;
  end;
end;

procedure TAdvDefaultToolBarExButton.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if CanFocus then
    SetFocus;

  FDown := True;
  Invalidate;
end;

procedure TAdvDefaultToolBarExButton.HandleMouseMove(Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FHover := True;
  if FPrevHover <> FHover then
    Invalidate;

  FPrevHover := FHover;
end;

procedure TAdvDefaultToolBarExButton.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FDown := False;
  Invalidate;
end;

function TAdvDefaultToolBarExButton.HasHint: Boolean;
begin
  Result := Hint <> '';
end;

procedure TAdvDefaultToolBarExButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (Operation = opRemove) and (AComponent = FDropDownControl) then
    FDropDownControl := nil;
end;

procedure TAdvDefaultToolBarExButton.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  r: TRectF;
begin
  inherited;
  if not Appearance.Transparent then
  begin
    if Enabled then
    begin
      if FDown or FDownState then
      begin
        AGraphics.Fill.Assign(Appearance.DownFill);
        AGraphics.Stroke.Assign(Appearance.DownStroke);
      end
      else if FHover then
      begin
        AGraphics.Fill.Assign(Appearance.HoverFill);
        AGraphics.Stroke.Assign(Appearance.HoverStroke);
      end
      else
      begin
        AGraphics.Fill.Assign(Appearance.NormalFill);
        AGraphics.Stroke.Assign(Appearance.NormalStroke);
      end;
    end
    else
    begin
      AGraphics.Fill.Assign(Appearance.DisabledFill);
      AGraphics.Stroke.Assign(Appearance.DisabledStroke);
    end;

    r := LocalRect;

    if Appearance.FlatStyle then
    begin
      AGraphics.Fill.Kind := gfkSolid;
      AGraphics.DrawRectangle(r);
    end
    else
      AGraphics.DrawRoundRectangle(r, Appearance.Rounding, Appearance.Corners);

    if Appearance.ShowInnerStroke and not Appearance.FlatStyle then
    begin
      InflateRectEx(r, -1, -1);
      AGraphics.Stroke.Assign(Appearance.InnerStroke);
      AGraphics.Fill.Kind := gfkNone;
      AGraphics.DrawRoundRectangle(r, Appearance.Rounding, Appearance.Corners);
    end;
  end;

  DrawBitmap(AGraphics, ARect);
  DrawText(AGraphics);
  DrawButton(AGraphics);

  if ShowFocus and Focused then
  begin
    r := GetTextRect;
    AGraphics.DrawFocusRectangle(r);
  end;

  if IsDesignTime and CanDrawDesignTime then
    AGraphics.DrawFocusRectangle(ARect, gcBlack);
end;

procedure TAdvDefaultToolBarExButton.DoClosePopup(Sender: TObject);
begin
  if Assigned(OnCloseDropDown) then
    OnCloseDropDown(Self);
end;

procedure TAdvDefaultToolBarExButton.ResetToDefaultStyle;
begin
  inherited;
  Font.Color := gcBlack;
  Appearance.NormalFill.Color := gcWhite;
  Appearance.HoverFill.Color := gcWhite;
  Appearance.DownFill.Color := gcWhite;
  Appearance.DisabledFill.Color := gcLightgray;
  Appearance.NormalStroke.Color := gcGray;
  Appearance.InnerStroke.Color := gcWhite;
  Appearance.HoverStroke.Color := gcGray;
  Appearance.DownStroke.Color := gcGray;
  Appearance.DisabledStroke.Color := gcGray;

  Appearance.NormalFill.Kind := gfkGradient;
  Appearance.HoverFill.Kind := gfkGradient;
  Appearance.DownFill.Kind := gfkGradient;
  Appearance.DisabledFill.Kind := gfkSolid;
  Appearance.NormalStroke.Kind := gskSolid;
  Appearance.InnerStroke.Kind := gskSolid;
  Appearance.HoverStroke.Kind := gskSolid;
  Appearance.DownStroke.Kind := gskSolid;
  Appearance.DisabledStroke.Kind := gskSolid;

  Appearance.NormalFill.Color := MakeGraphicsColor(249, 251, 252);
  Appearance.NormalFill.ColorTo := MakeGraphicsColor(230, 235, 235);
  Appearance.NormalFill.ColorMirror := MakeGraphicsColor(220, 220, 236);
  Appearance.NormalFill.ColorMirrorTo := MakeGraphicsColor(220, 225, 236);

  Appearance.HoverFill.Color := MakeGraphicsColor(229, 231, 232);
  Appearance.HoverFill.ColorTo := MakeGraphicsColor(210, 215, 215);
  Appearance.HoverFill.ColorMirror := MakeGraphicsColor(200, 200, 216);
  Appearance.HoverFill.ColorMirrorTo := MakeGraphicsColor(200, 205, 216);

  Appearance.DownFill.Color := MakeGraphicsColor(219, 221, 222);
  Appearance.DownFill.ColorTo := MakeGraphicsColor(200, 205, 205);
  Appearance.DownFill.ColorMirror := MakeGraphicsColor(190, 190, 206);
  Appearance.DownFill.ColorMirrorTo := MakeGraphicsColor(190, 195, 206);
end;

procedure TAdvDefaultToolBarExButton.SelectFirstValue;
begin

end;

procedure TAdvDefaultToolBarExButton.SelectLastValue;
begin

end;

procedure TAdvDefaultToolBarExButton.SelectNextValue;
begin

end;

procedure TAdvDefaultToolBarExButton.SelectPreviousValue;
begin

end;

procedure TAdvDefaultToolBarExButton.SelectValueWithCharacter(
  ACharacter: Char);
begin

end;

procedure TAdvDefaultToolBarExButton.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  if Assigned(FPopup) then
    FPopup.AdaptToStyle := AdaptToStyle;
end;

procedure TAdvDefaultToolBarExButton.SetAppearance(
  const Value: TAdvToolBarExButtonAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetAutoBitmapSize(const Value: Boolean);
begin
  if FAutoBitmapSize <> Value then
  begin
    FAutoBitmapSize := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetBitmaps(const Value: TAdvScaledBitmaps);
begin
  FBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetBitmapSize(const Value: Single);
begin
  if FBitmapSize <> Value then
  begin
    FBitmapSize := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  UpdateLayout;
  Invalidate;
end;

procedure TAdvDefaultToolBarExButton.SetBitmapPosition(
  const Value: TAdvToolBarExButtonBitmapPosition);
begin
  if FBitmapPosition <> Value then
  begin
    FBitmapPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetBitmapVisible(const Value: Boolean);
begin
  FBitmapVisible := Value;
  UpdateLayout;
  Invalidate;
end;

procedure TAdvDefaultToolBarExButton.SetLargeLayoutAutoBitmapSize(
  const Value: Boolean);
begin
  if FLargeLayoutAutoBitmapSize <> Value then
  begin
    FLargeLayoutAutoBitmapSize := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetLargeLayoutBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FLargeLayoutBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetLargeLayoutBitmapSize(
  const Value: Single);
begin
  if FLargeLayoutBitmapSize <> Value then
  begin
    FLargeLayoutBitmapSize := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetLargeLayoutDisabledBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FLargeLayoutDisabledBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetLargeLayoutHoverBitmaps(
  const Value: TAdvScaledBitmaps);
begin
  FLargeLayoutHoverBitmaps.Assign(Value);
end;

procedure TAdvDefaultToolBarExButton.SetLayout(
  const Value: TAdvToolBarExButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    UpdateLayout;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetCloseOnSelection(const Value: Boolean);
begin
  if FCloseOnSelection <> Value then
  begin
    FCloseOnSelection := Value;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetCompactLayout(
  const Value: TAdvToolBarExButtonLayout);
begin
  if FCompactLayout <> Value then
    FCompactLayout := Value;
end;

procedure TAdvDefaultToolBarExButton.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((not Assigned(Owner)) or not (Owner is TComponent) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText and ApplyName and CanChangeText then
    Text := StringReplace(Text, 'AdvToolBar', '', [rfReplaceAll]);
end;

procedure TAdvDefaultToolBarExButton.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetStretchBitmapIfNoText(
  const Value: Boolean);
begin
  if FStretchBitmapIfNoText <> Value then
  begin
    FStretchBitmapIfNoText := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetStretchText(const Value: Boolean);
begin
  if FStretchText <> Value then
  begin
    FStretchText := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    FTextVisible := Value <> '';
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetTextVisible(const Value: Boolean);
begin
  if FTextVisible <> Value then
  begin
    FTextVisible := Value;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetTrimming(
  const Value: TAdvGraphicsTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetVerticalTextAlign(
  const Value: TAdvGraphicsTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    Invalidate;
  end;
end;

procedure TAdvDefaultToolBarExButton.UpdateControlAfterResize;
begin
  inherited;
  if Layout = bblLarge then
    FOldHeight := Height;
end;

procedure TAdvDefaultToolBarExButton.UpdateLayout;
var
  sz: TSizeF;
  w: Single;
begin
  if (Layout = bblNone) or FBlockUpdate then
    Exit;

  FBlockUpdate := True;
  case Layout of
    bblBitmap:
    begin
      TextVisible := False;
      BitmapPosition := bbpLeft;
      StretchText := False;
      Height := Round(GetBitmapSize + 6);
      Width := Round(GetBitmapSize + 6);
    end;
    bblLabel:
    begin
      TextVisible := True;
      BitmapPosition := bbpLeft;
      StretchText := False;
      sz := GetTextSize;
      if BitmapVisible then
      begin
        Height := Round(Max(GetBitmapSize, sz.cy) + 6);
        Width := Round(GetBitmapSize + sz.cx + 12);
      end
      else if TextVisible and (Text <> '') then
      begin
        Height := Round(sz.cy + 6);
        Width := Round(sz.cx + 6);
      end
      else
      begin
        Width := 75;
        Height := 24;
      end;
    end;
    bblLarge:
    begin
      TextVisible := True;
      BitmapPosition := bbpTop;
      StretchText := not BitmapVisible;

      sz := GetTextSize;
      w := 30;
      if BitmapVisible then
        w := Max(w, GetLargeLayoutBitmapSize + 6);

      if TextVisible and (Text <> '') then
        w := Max(w, sz.cx + 12);

      Width := Round(w);

      if FOldHeight <> -1 then
        Height := Round(FOldHeight);
    end;
  end;
  FBlockUpdate := False;

  UpdateToolBar;
end;

procedure TAdvDefaultToolBarExButton.UpdateState;
var
  f: Single;
begin
  inherited;
  case FState of
    esNormal: f := 0.8;
    esLarge: f := 1.25;
    else
      f := 1;
  end;

  if not (csLoading in ComponentState) then
  begin
    DropDownWidth := DropDownWidth * f;
    DropDownHeight := DropDownHeight * f;
    {$IFDEF FMXLIB}
    Font.Size := Font.Size * f;
    SetBounds(Position.X, Position.Y, int(Width * f), int(Height * f));
    {$ENDIF}
    {$IFDEF VCLWEBLIB}
    Font.Size := Round(Font.Size * f);
    SetBounds(Left, Top, Round(Width * f), Round(Height * f));
    {$ENDIF}
  end;
end;

procedure TAdvToolBarExCustomButtonAppearance.SetCorners(const Value: TAdvGraphicsCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Changed;
  end;
end;

procedure TAdvToolBarExCustomButtonAppearance.SetRounding(const Value: Single);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    Changed;
  end;
end;

procedure TAdvToolBarExCustomButtonAppearance.StrokeChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvCustomToolBarExSeparator }

procedure TAdvCustomToolBarExSeparator.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomToolBarExSeparator then
  begin
    FAppearance.Assign((Source as TAdvCustomToolBarExSeparator).Appearance);
    Invalidate;
  end;
end;

constructor TAdvCustomToolBarExSeparator.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF FMXLIB}
  Width := 3;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Width := 2;
  {$ENDIF}
  Height := 30;
  FAppearance := TAdvToolBarExSeparatorAppearance.Create(Self);
end;

destructor TAdvCustomToolBarExSeparator.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvCustomToolBarExSeparator.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  cp: TPointF;
  r: TRectF;
begin
  inherited;
  r := LocalRect;
  {$IFDEF FMXLIB}
  cp := r.CenterPoint;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  cp := PointF(r.Left, r.Top);
  {$ENDIF}
  AGraphics.Stroke.Assign(Appearance.Stroke);
  AGraphics.DrawLine(PointF(cp.X, r.Top), PointF(cp.X, r.Bottom));
  AGraphics.Stroke.Assign(Appearance.InnerStroke);
  AGraphics.DrawLine(PointF(cp.X + 1, r.Top), PointF(cp.X + 1, r.Bottom));
end;

procedure TAdvCustomToolBarExSeparator.SetAppearance(
  const Value: TAdvToolBarExSeparatorAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvCustomToolBarExSeparator.UpdateState;
var
  f: Single;
begin
  inherited;
  case State of
    esNormal: f := 0.8;
    esLarge: f := 1.25;
    else
      f := 1;
  end;

  if not (csLoading in ComponentState) then
  begin
    {$IFDEF FMXLIB}
    SetBounds(Position.X, Position.Y, Width, int(Height * f));
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    SetBounds(Left, Top, Width, Round(Height * f));
    {$ENDIF}
  end;
end;

{ TAdvToolBarExCustomButtonAppearance }

procedure TAdvToolBarExCustomButtonAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvToolBarExCustomButtonAppearance then
  begin
    FNormalFill.Assign((Source as TAdvToolBarExCustomButtonAppearance).NormalFill);
    FDisabledFill.Assign((Source as TAdvToolBarExCustomButtonAppearance).DisabledFill);
    FHoverFill.Assign((Source as TAdvToolBarExCustomButtonAppearance).HoverFill);
    FDownFill.Assign((Source as TAdvToolBarExCustomButtonAppearance).DownFill);
    FNormalStroke.Assign((Source as TAdvToolBarExCustomButtonAppearance).NormalStroke);
    FDisabledStroke.Assign((Source as TAdvToolBarExCustomButtonAppearance).DisabledStroke);
    FHoverStroke.Assign((Source as TAdvToolBarExCustomButtonAppearance).HoverStroke);
    FDownStroke.Assign((Source as TAdvToolBarExCustomButtonAppearance).DownStroke);
    FRounding := (Source as TAdvToolBarExCustomButtonAppearance).Rounding;
    FCorners := (Source as TAdvToolBarExCustomButtonAppearance).Corners;
    FShowInnerStroke := (Source as TAdvToolBarExCustomButtonAppearance).ShowInnerStroke;
    FTransparent := (Source as TAdvToolBarExCustomButtonAppearance).Transparent;
    FFlatStyle := (Source as TAdvToolBarExCustomButtonAppearance).FlatStyle;
  end
  else
    inherited;
end;

procedure TAdvToolBarExCustomButtonAppearance.Changed;
begin
  if Assigned(FOwner) then
    FOwner.AppearanceChanged
  else if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvToolBarExCustomButtonAppearance.Create(AOwner: TAdvDefaultToolBarExButton);
begin
  FOwner := AOwner;
  FCorners := [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight];
  FRounding := 3;
  FTransparent := False;
  FShowInnerStroke := True;
  FFlatStyle := False;

  FNormalFill := TAdvGraphicsFill.Create(gfkGradient, MakeGraphicsColor(249, 251, 252), MakeGraphicsColor(230, 235, 235), MakeGraphicsColor(220, 220, 236), MakeGraphicsColor(220, 225, 236));
  FHoverFill := TAdvGraphicsFill.Create(gfkGradient, MakeGraphicsColor(229, 231, 232), MakeGraphicsColor(210, 215, 215), MakeGraphicsColor(200, 200, 216), MakeGraphicsColor(200, 205, 216));
  FDownFill := TAdvGraphicsFill.Create(gfkGradient, MakeGraphicsColor(219, 221, 222), MakeGraphicsColor(200, 205, 205), MakeGraphicsColor(190, 190, 206), MakeGraphicsColor(190, 195, 206));
  FDisabledFill := TAdvGraphicsFill.Create(gfkSolid, gcLightgray);
  FNormalStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);
  FInnerStroke := TAdvGraphicsStroke.Create(gskSolid, gcWhite);
  FHoverStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);
  FDownStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);
  FDisabledStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);

  FNormalFill.OnChanged := FillChanged;
  FHoverFill.OnChanged := FillChanged;
  FDownFill.OnChanged := FillChanged;
  FDisabledFill.OnChanged := FillChanged;

  FInnerStroke.OnChanged := StrokeChanged;
  FNormalStroke.OnChanged := StrokeChanged;
  FHoverStroke.OnChanged := StrokeChanged;
  FDownStroke.OnChanged := StrokeChanged;
  FDisabledStroke.OnChanged := StrokeChanged;
end;

destructor TAdvToolBarExCustomButtonAppearance.Destroy;
begin
  FNormalFill.Free;
  FHoverFill.Free;
  FDownFill.Free;
  FDisabledFill.Free;

  FInnerStroke.Free;
  FNormalStroke.Free;
  FHoverStroke.Free;
  FDownStroke.Free;
  FDisabledStroke.Free;
  inherited;
end;

procedure TAdvToolBarExCustomButtonAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvToolBarExCustomButtonAppearance.IsRoundingStored: Boolean;
begin
  Result := Rounding <> 3;
end;

procedure TAdvToolBarExCustomButtonAppearance.SetDisabledFill(const Value: TAdvGraphicsFill);
begin
  FDisabledFill.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetDisabledStroke(
  const Value: TAdvGraphicsStroke);
begin
  FDisabledStroke.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetDownFill(const Value: TAdvGraphicsFill);
begin
  FDownFill.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetDownStroke(
  const Value: TAdvGraphicsStroke);
begin
  FDownStroke.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetFlatStyle(const Value: Boolean);
var
  c: TControl;
  I: Integer;
begin
  if FFlatStyle <> Value then
  begin
    FFlatStyle := Value;
    if Assigned(FOwner) then
    begin
      for I := 0 to FOwner.ControlCount - 1 do
      begin
        c := FOwner.Controls[I];
        {$IFDEF FMXLIB}
        if Supports(c, IDesignerControl) then
          Continue;
        {$ENDIF}

        if c is TAdvDefaultToolBarExButton then
          (c as TAdvDefaultToolBarExButton).Appearance.FlatStyle := FlatStyle;

        if c is TAdvCustomToolBarEx then
          (c as TAdvCustomToolBarEx).Appearance.FlatStyle := FlatStyle;
      end;
    end;

    Changed;
  end;
end;

procedure TAdvToolBarExCustomButtonAppearance.SetHoverFill(const Value: TAdvGraphicsFill);
begin
  FHoverFill.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetHoverStroke(
  const Value: TAdvGraphicsStroke);
begin
  FHoverStroke.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetInnerStroke(
  const Value: TAdvGraphicsStroke);
begin
  FInnerStroke.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetNormalFill(const Value: TAdvGraphicsFill);
begin
  FNormalFill.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetNormalStroke(
  const Value: TAdvGraphicsStroke);
begin
  FNormalStroke.Assign(Value);
end;

procedure TAdvToolBarExCustomButtonAppearance.SetShowInnerStroke(
  const Value: Boolean);
begin
  if FShowInnerStroke <> Value then
  begin
    FShowInnerStroke := Value;
    Changed;
  end;
end;

procedure TAdvToolBarExCustomButtonAppearance.SetTransparent(
  const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

{ TAdvCustomToolBarExAppearance }

procedure TAdvCustomToolBarExAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomToolBarExAppearance then
  begin
    FSeparatorStroke.Assign((Source as TAdvCustomToolBarExAppearance).SeparatorStroke);
    FSeparator := (Source as TAdvCustomToolBarExAppearance).Separator;
    FFill.Assign((Source as TAdvCustomToolBarExAppearance).Fill);
    FStroke.Assign((Source as TAdvCustomToolBarExAppearance).Stroke);
    FHorizontalSpacing := (Source as TAdvCustomToolBarExAppearance).HorizontalSpacing;
    FVerticalSpacing := (Source as TAdvCustomToolBarExAppearance).VerticalSpacing;
    FDragGripColor := (Source as TAdvCustomToolBarExAppearance).DragGripColor;
    FDragGrip := (Source as TAdvCustomToolBarExAppearance).DragGrip;
    FFlatStyle := (Source as TAdvCustomToolBarExAppearance).FlatStyle;
  end;
end;

constructor TAdvCustomToolBarExAppearance.Create(AOwner: TAdvCustomToolBarEx);
begin
  FOwner := AOwner;
  FFont := TAdvGraphicsFont.Create;
  FFlatStyle := False;
  FHorizontalSpacing := 3;
  FVerticalSpacing := 3;
  FDragGrip := True;
  FDragGripColor := gcLightgray;
  FFill := TAdvGraphicsFill.Create(gfkGradient, gcWhite, MakeGraphicsColor(230, 230, 230));
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);

  FFill.OnChanged := FillChanged;
  FStroke.OnChanged := StrokeChanged;

  FSeparatorStroke := TAdvGraphicsStroke.Create(gskSolid, gcWhite);
  FSeparatorStroke.OnChanged := StrokeChanged;
end;

destructor TAdvCustomToolBarExAppearance.Destroy;
begin
  FSeparatorStroke.Free;
  FFont.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

procedure TAdvCustomToolBarExAppearance.FillChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

function TAdvCustomToolBarExAppearance.IsHorizontalSpacingStored: Boolean;
begin
  Result := HorizontalSpacing <> 3;
end;

function TAdvCustomToolBarExAppearance.IsVerticalSpacingStored: Boolean;
begin
  Result := VerticalSpacing <> 3;
end;

procedure TAdvCustomToolBarExAppearance.SetDragGrip(const Value: Boolean);
begin
  if FDragGrip <> Value then
  begin
    FDragGrip := Value;
    FOwner.UpdateControls;
  end;
end;

procedure TAdvCustomToolBarExAppearance.SetDragGripColor(
  const Value: TAdvGraphicsColor);
begin
  if FDragGripColor <> Value then
  begin
    FDragGripColor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TAdvCustomToolBarExAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvCustomToolBarExAppearance.SetFlatStyle(const Value: Boolean);
begin
  if FFlatStyle <> Value then
  begin
    FFlatStyle := Value;
    if Assigned(FOwner) then
      FOwner.ApplyFlatStyle;
  end;
end;

procedure TAdvCustomToolBarExAppearance.SetHorizontalSpacing(
  const Value: Single);
begin
  if FHorizontalSpacing <> Value then
  begin
    FHorizontalSpacing := Value;
    FOwner.UpdateControls;
  end;
end;

procedure TAdvCustomToolBarExAppearance.SetSeparator(const Value: Boolean);
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    FOwner.Invalidate;
  end;
end;

procedure TAdvCustomToolBarExAppearance.SetSeparatorStroke(
  const Value: TAdvGraphicsStroke);
begin
  FSeparatorStroke.Assign(Value);
end;

procedure TAdvCustomToolBarExAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvCustomToolBarExAppearance.SetVerticalSpacing(
  const Value: Single);
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    FOwner.UpdateControls;
  end;
end;

procedure TAdvCustomToolBarExAppearance.StrokeChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

{ TAdvCustomToolBarExSeparatorAppearance }

procedure TAdvCustomToolBarExSeparatorAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomToolBarExSeparatorAppearance then
  begin
    Stroke.Assign((Source as TAdvCustomToolBarExSeparatorAppearance).Stroke);
    InnerStroke.Assign((Source as TAdvCustomToolBarExSeparatorAppearance).InnerStroke);
  end
  else
    inherited;
end;

constructor TAdvCustomToolBarExSeparatorAppearance.Create(
  AOwner: TAdvCustomToolBarExSeparator);
begin
  FOwner := AOwner;
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);
  FStroke.OnChanged := StrokeChanged;
  FInnerStroke := TAdvGraphicsStroke.Create(gskSolid, gcWhite);
  FInnerStroke.OnChanged := StrokeChanged;
end;

destructor TAdvCustomToolBarExSeparatorAppearance.Destroy;
begin
  FInnerStroke.Free;
  FStroke.Free;
  inherited;
end;

procedure TAdvCustomToolBarExSeparatorAppearance.SetInnerStroke(
  const Value: TAdvGraphicsStroke);
begin
  FInnerStroke.Assign(Value);
end;

procedure TAdvCustomToolBarExSeparatorAppearance.SetStroke(
  const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvCustomToolBarExSeparatorAppearance.StrokeChanged(
  Sender: TObject);
begin
  FOwner.Invalidate;
end;

{ TAdvCustomDockPanelEx }

function TAdvCustomDockPanelEx.AddToolBar(AIndex: Integer = -1): TAdvToolBarEx;
begin
  FBlockUpdate := True;
  Result := TAdvToolBarEx.Create(Self);
  {$IFDEF FMXLIB}
  if (AIndex >= 0) and (AIndex <= Self.ControlCount - 1) then
    InsertObject(AIndex, Result)
  else
    AddObject(Result);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result.Parent := Self;
  {$ENDIF}

  FBlockUpdate := False;
  UpdateControls;
end;

procedure TAdvCustomDockPanelEx.ApplyStyle;
var
  c: TAdvGraphicsColor;
begin
  inherited;
  c := gcNull;
  if TAdvGraphicsStyles.GetStyleHeaderFillColor(c) then
  begin
    Appearance.Fill.Kind := gfkSolid;
    Appearance.Fill.Color := c;
  end;

  if TAdvGraphicsStyles.GetStyleHeaderFillColorTo(c) then
  begin
    Appearance.Fill.Kind := gfkGradient;
    Appearance.Fill.ColorTo := c;
  end;
end;

procedure TAdvCustomDockPanelEx.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomDockPanelEx then
  begin
    Appearance.Assign((Source as TAdvCustomDockPanelEx).Appearance);
    AutoSize := (Source as TAdvCustomDockPanelEx).AutoSize;
    AutoAlign := (Source as TAdvCustomDockPanelEx).AutoAlign;
    State := (Source as TAdvCustomDockPanelEx).State;
  end
  else
    inherited;
end;

constructor TAdvCustomDockPanelEx.Create(AOwner: TComponent);
begin
  inherited;
  FState := esNormal;
  FAppearance := TAdvDockPanelExAppearance.Create(Self);
  FAutoSize := True;
  FAutoAlign := True;
  {$IFDEF FMXLIB}
  Align := TAlignLayout.Top;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Align := alTop;
  {$ENDIF}
  Height := 40;
end;

destructor TAdvCustomDockPanelEx.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

{$IFDEF CMNWEBLIB}
procedure TAdvCustomDockPanelEx.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;
  UpdateControls;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvCustomDockPanelEx.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomDockPanelEx.DoInsertObject(Index: Integer;
  const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomDockPanelEx.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomDockPanelEx.DoRemoveObject(
  const AObject: TFmxObject);
begin
  inherited;
  UpdateControls;
end;
{$ENDIF}

function TAdvCustomDockPanelEx.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/ttmsfnctoolbar';
end;

function TAdvCustomDockPanelEx.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomDockPanelEx.InitializeControls;
var
  I: Integer;
  c: TControl;
begin
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];
    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if c is TAdvCustomToolBarEx then
    begin
      (c as TAdvCustomToolBarEx).OnUpdateDockPanel := UpdateDockPanel;
      if (c as TAdvCustomToolBarEx).FDragGripMoving then
        Continue;
    end;

    if c.Visible then
    begin
      {$IFDEF FMXLIB}
      if c.Position.y < Appearance.Margins.Top then
        c.Position.y := Appearance.Margins.Top;
      if c.Position.x < Appearance.Margins.Left then
        c.Position.x := Appearance.Margins.Left
      else if c.Position.x + c.Width > Width - Appearance.Margins.Right then
        c.Position.x := Width - Appearance.Margins.Right - c.Width;
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      if c.Top < Appearance.Margins.Top then
        c.Top := Round(Appearance.Margins.Top);
      if c.Left < Appearance.Margins.Left then
        c.Left := Round(Appearance.Margins.Left)
      else if c.Left + c.Width > Width - Appearance.Margins.Right then
        c.Left := Round(Width - Appearance.Margins.Right - c.Width);
      {$ENDIF}
    end;
  end;
end;

procedure TAdvCustomDockPanelEx.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
var
  r: TRectF;
begin
  inherited;
  AGraphics.Fill.Assign(Appearance.Fill);
  AGraphics.Stroke.Assign(Appearance.Stroke);

  r := LocalRect;
  AGraphics.DrawRectangle(r);
end;

procedure TAdvCustomDockPanelEx.RearrangeControls;
var
  c, cl: TControl;
  cr, clr: TRectF;
  I, J: Integer;
begin
  for I := 0 to Self.ControlCount - 1 do
  begin
    c := Self.Controls[I];

    {$IFDEF FMXLIB}
    if Supports(c, IDesignerControl) then
      Continue;
    {$ENDIF}

    if not c.Visible then
      Continue;

    if c is TAdvCustomToolBarEx then
    begin
      if (c as TAdvCustomToolBarEx).FDragGripMoving then
        Continue;
    end;

    for J := 0 to Self.ControlCount - 1 do
    begin
      cl := Self.Controls[J];

      if (cl = c) or not cl.Visible then
        Continue;

      {$IFDEF FMXLIB}
      if Supports(cl, IDesignerControl) then
        Continue;
      {$ENDIF}

      {$IFDEF FMXLIB}
      cr := RectF(c.Position.X, c.Position.Y, c.Position.X + c.Width, c.Position.Y + c.Height);
      clr := RectF(cl.Position.X, cl.Position.Y, cl.Position.X + cl.Width, cl.Position.Y + cl.Height);

      if (clr.Left >= cr.Left - 3) and (clr.Left <= cr.Right + 3) then
      begin
        if (clr.Top < CenterPointEx(cr).Y) and (clr.Top >= cr.Top - 3) then
        begin
          cl.Position.X := c.Position.X + c.Width + 3;
          cl.Position.Y := c.Position.Y;
        end
        else if (clr.Top >= CenterPointEx(cr).Y) and (clr.Top <= cr.Bottom + 3) then
        begin
          cl.Position.Y := c.Position.Y + c.Height + 3;
          cl.Position.X := c.Position.X;
        end;
      end;
      {$ENDIF}

      {$IFDEF CMNWEBLIB}
      cr := RectF(c.Left, c.Top, c.Left + c.Width, c.Top + c.Height);
      clr := RectF(cl.Left, cl.Top, cl.Left + cl.Width, cl.Top + cl.Height);

      if (clr.Left >= cr.Left - 3) and (clr.Left <= cr.Right + 3) then
      begin
        if (clr.Top < CenterPointEx(cr).Y) and (clr.Top >= cr.Top - 3) then
        begin
          cl.Left := c.Left + c.Width + 3;
          cl.Top := c.Top;
        end
        else if (clr.Top >= CenterPointEx(cr).Y) and (clr.Top <= cr.Bottom + 3) then
        begin
          cl.Top := c.Top + c.Height + 3;
          cl.Left := c.Left;
        end;
      end;
      {$ENDIF}
    end;
  end;
end;

procedure TAdvCustomDockPanelEx.ResetToDefaultStyle;
begin
  inherited;

end;

procedure TAdvCustomDockPanelEx.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  UpdateControls;
end;

procedure TAdvCustomDockPanelEx.SetAppearance(
  const Value: TAdvDockPanelExAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvCustomDockPanelEx.SetAutoAlign(const Value: Boolean);
begin
  if FAutoAlign <> Value then
  begin
    FAutoAlign := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomDockPanelEx.SetAS(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    UpdateControls;
  end;
end;

procedure TAdvCustomDockPanelEx.SetState(
  const Value: TAdvToolBarExElementState);
var
  I: Integer;
  c: TControl;
begin
  if FState <> Value then
  begin
    FState := Value;
    for I := 0 to Self.ControlCount - 1 do
    begin
      c := Self.Controls[I];
      if c is TAdvCustomToolBarEx then
        (c as TAdvCustomToolBarEx).State := FState;
    end;
  end;
end;

procedure TAdvCustomDockPanelEx.UpdateControls;
var
  I: Integer;
  {$IFDEF FMXLIB}
  h, hsm: Single;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  h, hsm: Integer;
  {$ENDIF}
  c: TControl;
  {$IFDEF FNCLIB}
  ia: IAdvAdaptToStyle;
  {$ENDIF}
begin
  if FBlockUpdate or IsLoading or IsDestroying then
    Exit;

  FBlockUpdate := True;
  h := 0;
  hsm := ScalePaintValue(30);

  if AutoAlign then
  begin
    InitializeControls;
    RearrangeControls;
  end;

  if AutoSize then
  begin
    for I := 0 to Self.ControlCount - 1 do
    begin
      c := Self.Controls[I];
      {$IFDEF FMXLIB}
      if Supports(c, IDesignerControl) then
        Continue;

      if c.Visible then
        h := Max(h, c.Position.y + c.Height + Appearance.Margins.Bottom);
      {$ENDIF}

      {$IFDEF FNCLIB}
      if Supports(c, IAdvAdaptToStyle, ia) then
        ia.AdaptToStyle := AdaptToStyle;
      {$ENDIF}

      {$IFDEF CMNWEBLIB}
      if c.Visible then
        h := Round(Max(h, c.Top + c.Height + Appearance.Margins.Bottom));
      {$ENDIF}
    end;

    if h = 0 then
      h := hsm;

    if csDesigning in ComponentState then
      h := h + ScalePaintValue(15);

    {$IFDEF FMXLIB}
    SetBounds(Position.X, Position.Y, Width, h);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    SetBounds(Left, Top, Width, h);
    {$ENDIF}
  end;

  FBlockUpdate := False;
end;

procedure TAdvCustomDockPanelEx.UpdateDockPanel(Sender: TObject);
begin
  UpdateControls;
end;

{ TAdvCustomDockPanelExAppearance }

constructor TAdvCustomDockPanelExAppearance.Create(AOwner: TAdvCustomDockPanelEx);
begin
  FOwner := AOwner;
  FFill := TAdvGraphicsFill.Create(gfkSolid, gcWhite);
  FStroke := TAdvGraphicsStroke.Create(gskSolid, gcGray);

  FFill.OnChanged := FillChanged;
  FStroke.OnChanged := StrokeChanged;
  FMargins := TAdvMargins.Create;
  FMargins.Left := 3;
  FMargins.Top := 3;
  FMargins.Bottom := 3;
  FMargins.Right := 3;
end;

destructor TAdvCustomDockPanelExAppearance.Destroy;
begin
  FMargins.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

procedure TAdvCustomDockPanelExAppearance.FillChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

procedure TAdvCustomDockPanelExAppearance.MarginsChanged(
  Sender: TObject);
begin
  FOwner.UpdateControls;
end;

procedure TAdvCustomDockPanelExAppearance.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvCustomDockPanelExAppearance.SetMargins(
  const Value: TAdvMargins);
begin
  FMargins.Assign(Value);
end;

procedure TAdvCustomDockPanelExAppearance.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvCustomDockPanelExAppearance.StrokeChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

{ TAdvCustomToolBarExElement }

procedure TAdvCustomToolBarExElement.UpdateToolBarControl;
begin
  if Assigned(OnUpdateToolBarControl) then
    OnUpdateToolBarControl(Self);
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomToolBarExElement.ReadControlIndex(Reader: TReader);
begin
  ControlIndex := Reader.ReadInteger;
end;

procedure TAdvCustomToolBarExElement.WriteControlIndex(Writer: TWriter);
begin
  Writer.WriteInteger(ControlIndex);
end;
{$ENDIF}

procedure TAdvCustomToolBarExElement.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomToolBarExElement then
  begin
    FState := (Source as TAdvCustomToolBarExElement).State;
    FLastElement := (Source as TAdvCustomToolBarExElement).LastElement;
    FCanCopy := (Source as TAdvCustomToolBarExElement).CanCopy;
  end;
end;

constructor TAdvCustomToolBarExElement.Create(AOwner: TComponent);
begin
  inherited;
  DisableBackground;
  {$IFDEF CMNLIB}
  ParentColor := True;
  {$IFDEF VCLLIB}
  ParentBackground := True;
  {$ENDIF}
  {$ENDIF}
  FLastElement := False;
  FCanCopy := True;
  FState := esNormal;
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomToolBarExElement.DefineProperties(Filer: TFiler);
begin
  inherited;
  {$IFDEF LCLLIB}
  Filer.DefineProperty('ControlIndex', @ReadControlIndex, @WriteControlIndex, True);
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Filer.DefineProperty('ControlIndex', ReadControlIndex, WriteControlIndex, True);
  {$ENDIF}
end;
{$ENDIF}

destructor TAdvCustomToolBarExElement.Destroy;
begin
  inherited;
end;

function TAdvCustomToolBarExElement.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfncuipack/components/ttmsfnctoolbar';
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarExElement.DoMatrixChanged(Sender: TObject);
begin
  inherited;
  UpdateToolBar;
end;
{$ENDIF}

procedure TAdvCustomToolBarExElement.UpdateControlAfterResize;
begin
  inherited;
  UpdateToolBar;
end;

procedure TAdvCustomToolBarExElement.SetLastElement(const Value: Boolean);
begin
  if FLastElement <> Value then
  begin
    FLastElement := Value;
    UpdateToolBar;
  end;
end;

procedure TAdvCustomToolBarExElement.SetState(
  const Value: TAdvToolBarExElementState);
begin
  if FState <> Value then
  begin
    FState := Value;
    UpdateState;
  end;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomToolBarExElement.SetVisible(const Value: Boolean);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomToolBarExElement.VisibleChanging;
{$ENDIF}
begin
  inherited;
  {$IFDEF FMXLIB}
  if Visible <> Value then
  {$ENDIF}
    UpdateToolBar;
end;

procedure TAdvCustomToolBarExElement.UpdateState;
begin

end;

procedure TAdvCustomToolBarExElement.UpdateToolBar;
begin
  if Assigned(OnUpdateToolBar) and not FBlockUpdate then
    OnUpdateToolBar(Self);
end;

{ TAdvToolBarExControl }

constructor TAdvToolBarExControl.Create;
begin
  FBitmap := TAdvBitmap.Create;
end;

destructor TAdvToolBarExControl.Destroy;
begin 
  FBitmap.Free;
  inherited;
end;

{ TAdvCustomToolBarExOptionsMenu }

procedure TAdvCustomToolBarExOptionsMenu.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomToolBarExOptionsMenu then
  begin
    FShowItemText := (Source as TAdvCustomToolBarExOptionsMenu).ShowItemText;
    FShowItemBitmap := (Source as TAdvCustomToolBarExOptionsMenu).ShowItemBitmap;
    FShowButton := (Source as TAdvCustomToolBarExOptionsMenu).ShowButton;
    FAutoItemBitmapWidth := (Source as TAdvCustomToolBarExOptionsMenu).AutoItemBitmapWidth;
    FItemBitmapWidth := (Source as TAdvCustomToolBarExOptionsMenu).ItemBitmapWidth;
  end;
end;

constructor TAdvCustomToolBarExOptionsMenu.Create(
  AOwner: TAdvCustomToolBarEx);
begin
  FOwner := AOwner;
  FShowItemText := True;
  FShowButton := True;
  FShowItemBitmap := True;
  FAutoItemBitmapWidth := True;
  FItemBitmapWidth := 50;
end;

destructor TAdvCustomToolBarExOptionsMenu.Destroy;
begin

  inherited;
end;

function TAdvCustomToolBarExOptionsMenu.IsItemBitmapWidthStored: Boolean;
begin
  Result := ItemBitmapWidth <> 50;
end;

procedure TAdvCustomToolBarExOptionsMenu.SetAutoItemBitmapWidth(
  const Value: Boolean);
begin
  if FAutoItemBitmapWidth <> Value then
  begin
    FAutoItemBitmapWidth := Value;
  end;
end;

procedure TAdvCustomToolBarExOptionsMenu.SetItemBitmapWidth(
  const Value: Single);
begin
  if FItemBitmapWidth <> Value then
  begin
    FItemBitmapWidth := Value;
  end;
end;

procedure TAdvCustomToolBarExOptionsMenu.SetShowButton(const Value: Boolean);
begin
  if FShowButton <> Value then
  begin
    FShowButton := Value;
    FOwner.FOptionsMenuButton.Visible := FShowButton;
    if FShowButton then
      FOwner.FOptionsMenuButton.Parent := FOwner
    else
      FOwner.FOptionsMenuButton.Parent := nil;

    FOwner.UpdateControls;
  end;
end;

procedure TAdvCustomToolBarExOptionsMenu.SetShowItemBitmap(
  const Value: Boolean);
begin
  if FShowItemBitmap <> Value then
  begin
    FShowItemBitmap := Value;
  end;
end;

procedure TAdvCustomToolBarExOptionsMenu.SetShowItemText(const Value: Boolean);
begin
  if FShowItemText <> Value then
  begin
    FShowItemText := Value;
  end;
end;

{ TAdvToolBarExFontNamePicker }

constructor TAdvToolBarExFontNamePicker.Create(AOwner: TComponent);
begin
  inherited;
  AutoOptionsMenuText := 'Font Name';
  TAdvUtils.GetFonts(FItems);
  {$IFDEF LCLLIB}
  FItems.Insert(0, 'Default');
  {$ENDIF}
end;

procedure TAdvToolBarExFontNamePicker.DoItemSelected;
begin
  inherited;
  if Assigned(OnFontNameSelected) then
    OnFontNameSelected(Self, SelectedFontName);
end;

function TAdvToolBarExFontNamePicker.GetSelectedFontName: String;
begin
  Result := GetSelectedItem;
end;

procedure TAdvToolBarExFontNamePicker.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExFontNamePicker);
end;

procedure TAdvToolBarExFontNamePicker.SetSelectedFontName(const Value: String);
begin
  SelectedItem := Value;
end;

{ TAdvToolBarExFontNamePicker }

constructor TAdvToolBarExFontSizePicker.Create(AOwner: TComponent);
begin
  inherited;
  AutoOptionsMenuText := 'Font Size';
  FItems.Add('8');
  FItems.Add('9');
  FItems.Add('10');
  FItems.Add('11');
  FItems.Add('12');
  FItems.Add('14');
  FItems.Add('16');
  FItems.Add('18');
  FItems.Add('20');
  FItems.Add('22');
  FItems.Add('24');
  FItems.Add('26');
  FItems.Add('28');
  FItems.Add('36');
  FItems.Add('48');
  FItems.Add('72');
end;

procedure TAdvToolBarExFontSizePicker.DoItemSelected;
begin
  inherited;
  if Assigned(OnFontSizeSelected) then
    OnFontSizeSelected(Self, SelectedFontSize);
end;

function TAdvToolBarExFontSizePicker.GetSelectedFontSize: Single;
var
  s: String;
begin
  Result := -1;
  s := GetSelectedItem;
  if s <> '' then
    Result := StrToFloat(GetSelectedItem)
end;

procedure TAdvToolBarExFontSizePicker.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExFontSizePicker);
end;

procedure TAdvToolBarExFontSizePicker.SetSelectedFontSize(const Value: Single);
begin
  SelectedItem := FloatToStr(Value);
end;

{ TAdvToolBarExColorPicker }

function TAdvToolBarExColorPicker.BlockChange: Boolean;
begin
  Result := FColorSelector.BlockChange;
end;

function TAdvToolBarExColorPicker.CanChangeText: Boolean;
begin
  Result := not (csLoading in ComponentState) and not (csDesigning in ComponentState);
end;

procedure TAdvToolBarExColorPicker.ColorSelected(Sender: TObject;
  AColor: TAdvGraphicsColor);
begin
  if CloseOnSelection and not BlockChange then
    CloseDropDown;
  DoColorSelected(AColor);
  Invalidate;
end;

function TAdvToolBarExColorPicker.ColorSelector: TAdvColorSelectorEx;
begin
  Result := FColorSelector;
end;

constructor TAdvToolBarExColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  Width := 40;
  Height := 24;
  AutoOptionsMenuText := 'Color';
  FColorSelector := TAdvColorSelectorEx.Create(Self);
  FColorSelector.Appearance.HorizontalSpacing := 2;
  FColorSelector.Appearance.VerticalSpacing := 2;
  FColorSelector.Mode := csmExtended;
  FColorSelector.Width := 175;
  FColorSelector.Height := 125;
  FColorSelector.Stored := False;
  FColorSelector.SelectedColor := gcBlack;
  FColorSelector.OnColorSelected := ColorSelected;

  DropDownControl := FColorSelector;
  DropDownWidth := FColorSelector.Width;
  DropDownHeight := FColorSelector.Height;
  DropDownKind := ddkDropDownButton;
end;

procedure TAdvToolBarExColorPicker.DoColorSelected(AColor: TAdvGraphicsColor);
begin
  if Assigned(OnColorSelected) then
    OnColorSelected(Self, AColor);
end;

procedure TAdvToolBarExColorPicker.DrawColor(AColor: TAdvGraphicsColor;
  ARect: TRectF; AGraphics: TAdvGraphics);
var
  r: TRectF;
  rounding: integer;
  I, J: Integer;
  st: TAdvGraphicsSaveState;
begin
  R := ARect;

  rounding := Round(Max(ScalePaintValue(3.5), Appearance.Rounding / 4));

  InflateRectEx(R, -rounding, -rounding);
  OffsetRectEx(R, ScalePaintValue(0.5), ScalePaintValue(0.5));
  st := AGraphics.SaveState;
  try
    AGraphics.ClipRect(R);
    AGraphics.Stroke.Kind := gskNone;
    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Stroke.Width := 1;
    AGraphics.Fill.Color := MakeGraphicsColor(255, 255, 255);
    AGraphics.DrawRectangle(R);
    AGraphics.Fill.Color := MakeGraphicsColor(211 , 211 , 211 );
    for I := 0 to Trunc((r.Right - r.Left) / 5) + 1 do
      for J := 0 to Trunc((r.Bottom - r.Top) / 5) + 1 do
        if Odd(I + J) then
          AGraphics.DrawRectangle(RectF(I * 5, J * 5, (I + 1) * 5, (J + 1) * 5), gcrmNone);

    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Fill.Color := AColor;
    AGraphics.Stroke.Color := gcBlack;
    AGraphics.Stroke.Kind := gskSolid;
    AGraphics.DrawRectangle(R);
  finally
    AGraphics.RestoreState(st);
  end;
end;

procedure TAdvToolBarExColorPicker.DrawSelectedColor(
  AGraphics: TAdvGraphics; ARect: TRectF);
var
  R: TRectF;
  c: TAdvGraphicsColor;
  ct: TAdvToolBarExDropDownButton;
begin
  ct := GetDropDownButtonControl;
  if Assigned(ct) then
  begin
    R := RectF(0, 0, Width - ct.Width, Height);
    c := SelectedColor;
    DrawColor(c, r, AGraphics);
  end;
end;

function TAdvToolBarExColorPicker.GetItems: TAdvColorSelectorExItems;
begin
  Result := FColorSelector.Items;
end;

function TAdvToolBarExColorPicker.GetSelectedColor: TAdvGraphicsColor;
begin
  Result := FColorSelector.SelectedColor;
end;

function TAdvToolBarExColorPicker.GetSelectedItemIndex: Integer;
begin
  Result := FColorSelector.SelectedItemIndex;
end;

procedure TAdvToolBarExColorPicker.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExColorPicker);
end;

procedure TAdvToolBarExColorPicker.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  inherited;
  DrawSelectedColor(AGraphics, ARect);
end;

procedure TAdvToolBarExColorPicker.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  if Assigned(FColorSelector) then
    FColorSelector.AdaptToStyle := AdaptToStyle;
end;

procedure TAdvToolBarExColorPicker.SetItems(
  const Value: TAdvColorSelectorExItems);
begin
  FColorSelector.Items.Assign(Value);
end;

procedure TAdvToolBarExColorPicker.SetSelectedColor(const Value: TAdvGraphicsColor);
begin
  FColorSelector.SelectedColor := Value;
  Invalidate;
end;

procedure TAdvToolBarExColorPicker.SetSelectedItemIndex(const Value: Integer);
begin
  FColorSelector.SelectedItemIndex := Value;
  Invalidate;
end;

{ TAdvToolBarExBitmapPicker }

function TAdvToolBarExBitmapPicker.BitmapSelector: TAdvBitmapSelectorEx;
begin
  Result := FBitmapSelector;
end;

function TAdvToolBarExBitmapPicker.BlockChange: Boolean;
begin
  Result := FBitmapSelector.BlockChange;
end;

procedure TAdvToolBarExBitmapPicker.BitmapSelected(Sender: TObject;
  ABitmap: TAdvBitmap);
begin
  Bitmaps.Clear;
  DisabledBitmaps.Clear;
  HoverBitmaps.Clear;
  Bitmaps.AddBitmap(ABitmap);
  DisabledBitmaps.AddBitmap(ABitmap);
  HoverBitmaps.AddBitmap(ABitmap);

  if CloseOnSelection and not BlockChange then
    CloseDropDown;
  DoBitmapSelected(ABitmap);
end;

function TAdvToolBarExBitmapPicker.CanChangeText: Boolean;
begin
  Result := not (csLoading in ComponentState) and not (csDesigning in ComponentState);
end;

constructor TAdvToolBarExBitmapPicker.Create(AOwner: TComponent);
begin
  inherited;
  Width := 40;
  Height := 24;
  AutoOptionsMenuText := 'Bitmap';
  FBitmapSelector := TAdvBitmapSelectorEx.Create(Self);
  FBitmapSelector.Appearance.HorizontalSpacing := 2;
  FBitmapSelector.Appearance.VerticalSpacing := 2;
  FBitmapSelector.Stored := False;
  FBitmapSelector.OnBitmapSelected := BitmapSelected;

  DropDownControl := FBitmapSelector;
  DropDownWidth := FBitmapSelector.Width;
  DropDownHeight := FBitmapSelector.Height;
  DropDownKind := ddkDropDownButton;
end;

procedure TAdvToolBarExBitmapPicker.DoBitmapSelected(ABitmap: TAdvBitmap);
begin
  if Assigned(OnBitmapSelected) then
    OnBitmapSelected(Self, ABitmap);
end;

function TAdvToolBarExBitmapPicker.GetBitmapRect(ARect: TRectF): TRectF;
var
  r: TRectF;
  dr: TRectf;
begin
  r := ARect;
  Result := RectF(r.Left, r.Top, r.Right, r.Bottom);
  if BitmapVisible then
  begin
    if DropDownKind <> ddkNormal then
    begin
      dr := GetDropDownRect;
      case DropDownPosition of
        ddpRight: Result := RectF(Result.Left + 3, Result.Top + 3, dr.Left - 3, Result.Bottom - 3);
        ddpBottom: Result := RectF(Result.Left + 3, Result.Top + 3, Result.Right - 3, dr.Top - 3);
      end;
    end
    else
      Result := RectF(Result.Left + 3, Result.Top + 3, Result.Right - 3, Result.Bottom - 3);
  end;
end;

function TAdvToolBarExBitmapPicker.GetItems: TAdvBitmapSelectorExItems;
begin
  Result := FBitmapSelector.Items;
end;

function TAdvToolBarExBitmapPicker.GetSelectedBitmap: TAdvBitmap;
begin
  Result := FBitmapSelector.SelectedBitmap;
end;

function TAdvToolBarExBitmapPicker.GetSelectedItemIndex: Integer;
begin
  Result := FBitmapSelector.SelectedItemIndex;
end;

procedure TAdvToolBarExBitmapPicker.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExBitmapPicker);
end;

procedure TAdvToolBarExBitmapPicker.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
  if Assigned(FBitmapSelector) then
    FBitmapSelector.AdaptToStyle := AdaptToStyle;
end;

procedure TAdvToolBarExBitmapPicker.SetPictureContainer(
  const Value: TPictureContainer);
begin
  inherited;
  if Assigned(FBitmapSelector) then
    FBitmapSelector.PictureContainer := Value;
end;

procedure TAdvToolBarExBitmapPicker.SetItems(
  const Value: TAdvBitmapSelectorExItems);
begin
  if Assigned(FBitmapSelector) then
    FBitmapSelector.Items.Assign(Value);
end;

procedure TAdvToolBarExBitmapPicker.SetSelectedItemIndex(const Value: Integer);
begin
  FBitmapSelector.SelectedItemIndex := Value;
  Bitmaps.Clear;
  DisabledBitmaps.Clear;
  HoverBitmaps.Clear;
  Bitmaps.AddBitmap(SelectedBitmap);
  DisabledBitmaps.AddBitmap(SelectedBitmap);
  HoverBitmaps.AddBitmap(SelectedBitmap);
  Invalidate;
end;

{ TAdvToolBarExDropDownButton }

constructor TAdvToolBarExDropDownButton.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultStyle := True;
end;

procedure TAdvToolBarExDropDownButton.DrawBitmap(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  bmp: TAdvBitmapHelperClass;
  bmpa: TBitmap;
  r: TRectF;
  g: TAdvGraphics;
begin
  if DefaultStyle then
  begin
    r := ARect;

    if not(Assigned(Owner) and (Owner is TAdvCustomToolBarExButton) and ((Owner as TAdvCustomToolBarExButton).FDropDownPosition = ddpBottom)) then
      InflateRectEx(r, ScalePaintValue(-2),ScalePaintValue(-2));

    inherited DrawBitmap(AGraphics, r);
  end
  else
  begin
    bmpa := TBitmap.Create;
    bmpa.SetSize(7, 7);
    {$IFDEF CMNLIB}
    bmpa.TransparentMode := tmFixed;
    bmpa.Transparent := True;
    bmpa.TransparentColor := gcWhite;
    {$ENDIF}
    g := TAdvGraphics.Create(bmpa.Canvas);
    try
      g.BeginScene;
      {$IFDEF CMNLIB}
      g.Fill.Color := gcWhite;
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcWhite;
      g.DrawRectangle(0, 0, bmpa.Width, bmpa.Height);
      {$ENDIF}
      g.Stroke.Kind := gskSolid;
      g.Stroke.Color := gcDarkgray;
      g.DrawLine(PointF(0, 1), PointF(6, 1), gcpmRightDown);
      g.DrawLine(PointF(0, 2), PointF(6, 2), gcpmRightDown);
      g.DrawLine(PointF(1, 3), PointF(5, 3), gcpmRightDown);
      g.DrawLine(PointF(2, 4), PointF(4, 4), gcpmRightDown);
      g.DrawLine(PointF(3, 5), PointF(3, 5), gcpmRightDown);
    finally
      g.EndScene;
      g.Free;
    end;

    try
      r := ARect;
      bmp := TAdvBitmap.Create;
      try
        bmp.Assign(bmpa);
        AGraphics.DrawBitmap(r, bmp);
      finally
        bmp.Free;
      end;
    finally
      bmpa.Free;
    end;
  end;
end;

function TAdvToolBarExDropDownButton.GetBitmapRect(ARect: TRectF): TRectF;
var
  r: TRectF;
begin
  r := ARect;
  Result := RectF(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);
end;

procedure TAdvToolBarExDropDownButton.LoadSettingsFromFile(AFileName: string);
begin
  inherited;
  DefaultStyle := False;
end;

procedure TAdvToolBarExDropDownButton.LoadSettingsFromStream(
  AStream: TStreamEx);
begin
  inherited;
  DefaultStyle := False;
end;

procedure TAdvToolBarExDropDownButton.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExDropDownButton);
end;

procedure TAdvToolBarExDropDownButton.SetDefaultStyle(const Value: Boolean);
begin
  if FDefaultStyle <> Value then
  begin
    FDefaultStyle := Value;
    Invalidate;
  end;
end;

{ TAdvToolBarExCustomItemPicker }

function TAdvToolBarExCustomItemPicker.CanChangeText: Boolean;
begin
  Result := not (csLoading in ComponentState) and not (csDesigning in ComponentState);
end;

procedure TAdvToolBarExCustomItemPicker.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  {$IFDEF CMNLIB}
  FItemSelector.Font.Height := TAdvUtils.MulDivInt(FItemSelector.Font.Height, M, D);
  FItemSelector.ItemHeight := TAdvUtils.MulDivInt(FItemSelector.ItemHeight, M, D);
  {$ENDIF}
end;

constructor TAdvToolBarExCustomItemPicker.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;
  FTimer.OnTimer := EnterTimerChanged;
  FTimer.Enabled := False;

  FEdit := TEdit.Create(Self);
  FEdit.TabStop := True;
  {$IFDEF FMXLIB}
  FEdit.Align := TAlignLayout.Client;
  FEdit.Stored := False;
  FEdit.OnChangeTracking := EditChange;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FEdit.Align := alClient;
  FEdit.OnChange := EditChange;
  {$ENDIF}

  Width := 100;
  Height := 24;
  HorizontalTextAlign := gtaLeading;
  FItemIndex := -1;

  FItemSelector := TListBox.Create(Self);
  FItemSelector.Width := 200;
  FItemSelector.Height := 150;
  {$IFDEF LCLLIB}
  FItemSelector.ClickOnSelChange := False;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FItemSelector.Align := TAlignLayout.Client;
  FItemSelector.OnItemClick := ItemSelected;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FItemSelector.Font.Height := -11;
  FItemSelector.Align := alClient;
  FItemSelector.OnClick := ItemSelected;
  {$ENDIF}

  FItemSelector.OnKeyUp := ItemKeyUp;
  FItemSelector.OnKeyDown := ItemKeyDown;

  DropDownControl := FItemSelector;
  DropDownKind := ddkDropDownButton;
  DropDownWidth := FItemSelector.Width;
  DropDownHeight := FItemSelector.Height;

  FItems := TStringList.Create;
end;

destructor TAdvToolBarExCustomItemPicker.Destroy;
begin
  FTimer.Free;
  FItems.Free;
  inherited;
end;

procedure TAdvToolBarExCustomItemPicker.DoEnter;
begin
  inherited;
  FTimer.Enabled := Editable;
end;

procedure TAdvToolBarExCustomItemPicker.DoItemSelected;
begin
  if Assigned(FEdit) {$IFDEF CMNWEBLIB} and FEdit.HandleAllocated{$ENDIF} then
  begin
    FEdit.Text := SelectedItem;
    FEdit.SelStart := Length(FEdit.Text);
  end;

  if Assigned(OnItemSelected) then
    OnItemSelected(Self, SelectedItemIndex);
end;

procedure TAdvToolBarExCustomItemPicker.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  if not Editable then
    inherited;
end;

procedure TAdvToolBarExCustomItemPicker.DropDown;
begin
  inherited;
  if not Assigned(FPopup) then
    Exit;

  {$IFDEF CMNLIB}
  if Assigned(FItemSelector) then
    FItemSelector.Font.Height := ScalePaintValue(-11);
  {$ENDIF}

  if FPopup.IsOpen and Assigned(FItemSelector) then
  begin
    FItemSelector.ItemIndex := -1;
    FItemSelector.ItemIndex := SelectedItemIndex;
  end;
end;

procedure TAdvToolBarExCustomItemPicker.EditChange(Sender: TObject);
begin
  SelectedItem := FEdit.Text;
  if Assigned(OnEditChange) then
    OnEditChange(Self);
end;

procedure TAdvToolBarExCustomItemPicker.EnterTimerChanged(Sender: TObject);
begin
  FEdit.SetFocus;
  FTimer.Enabled := False;
end;

function TAdvToolBarExCustomItemPicker.GetSelectedItem: String;
begin
  Result := '';
  if Assigned(FItems) and (FItemIndex >= 0) and (FItemIndex <= FItems.Count - 1) then
    Result := FItems[FItemIndex]
end;

function TAdvToolBarExCustomItemPicker.GetSelectedItemIndex: Integer;
begin
  Result := FItemIndex;
end;

function TAdvToolBarExCustomItemPicker.GetText: String;
begin
  if Editable then
    Result := FEdit.Text
  else
    Result := SelectedItem;
end;

procedure TAdvToolBarExCustomItemPicker.HandleDialogKey(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Shift <> [] then
    Exit;

  if Editable and FEdit.Focused then
  begin
    case Key of
      KEY_F4:
      begin
        if not (ssAlt in Shift) then
          DropDown;
      end;
    end;
  end;
end;

procedure TAdvToolBarExCustomItemPicker.InitializePopup;
begin
  {$IFDEF FMXLIB}
  FItemSelector.BeginUpdate;
  {$ENDIF}
  FItemSelector.Items.Clear;
  FItemSelector.Items.Assign(FItems);
  FItemSelector.ItemIndex := FItemIndex;
  {$IFDEF FMXLIB}
  FItemSelector.EndUpdate;
  {$ENDIF}
end;

{$IFDEF FMXLIB}
procedure TAdvToolBarExCustomItemPicker.ItemKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvToolBarExCustomItemPicker.ItemKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
begin
  FKeyboardUsed := True;
end;

{$IFDEF FMXLIB}
procedure TAdvToolBarExCustomItemPicker.ItemKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvToolBarExCustomItemPicker.ItemKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
begin
  if (Key = KEY_RETURN) or (Key = KEY_SPACE) then
  begin
    FItemIndex := FItemSelector.ItemIndex;
    if CloseOnSelection then
      DropDown;

    DoItemSelected;
    Invalidate;
  end;

  FKeyBoardUsed := False;
end;

{$IFDEF FMXLIB}
procedure TAdvToolBarExCustomItemPicker.ItemSelected(const Sender: TCustomListBox; const Item: TListBoxItem);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvToolBarExCustomItemPicker.ItemSelected(Sender: TObject);
{$ENDIF}
begin
  if FKeyBoardUsed then
    Exit;

  FItemIndex := FItemSelector.ItemIndex;
  if CloseOnSelection then
    DropDown;

  DoItemSelected;
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SelectFirstValue;
begin
  inherited;
  FItemIndex := 0;
  DoItemSelected;
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SelectLastValue;
begin
  inherited;
  FItemIndex := FItems.Count - 1;
  DoItemSelected;
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SelectNextValue;
begin
  inherited;
  if FItemIndex = -1 then
    FItemIndex := 0
  else
    FItemIndex := Min(FItems.Count - 1, FItemIndex + 1);

  DoItemSelected;
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SelectPreviousValue;
begin
  inherited;
  if FItemIndex = -1 then
    FItemIndex := 0
  else
    FItemIndex := Max(0, FItemIndex - 1);

  DoItemSelected;
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SelectValueWithCharacter(
  ACharacter: Char);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Items.Count - 1 do
  begin
    if TAdvUtils.MatchStrEx(ACharacter + '*', Items[I], False) then
    begin
      FItemIndex := I;
      DoItemSelected;
      Invalidate;
      Break;
    end;
  end;
end;

procedure TAdvToolBarExCustomItemPicker.SelectValueWithString(AValue: string);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Items.Count - 1 do
  begin
    if TAdvUtils.MatchStrEx(AValue + '*', Items[I], False) then
    begin
      FItemIndex := I;
      DoItemSelected;
      Invalidate;
      Break;
    end;
  end;
end;

procedure TAdvToolBarExCustomItemPicker.SetEditable(const Value: Boolean);
begin
  if FEditable <> Value then
  begin
    FEditable := Value;
    if Assigned(FEdit) then
    begin
      if FEditable then
      begin
        FEdit.Parent := Self;
      end
      else
        FEdit.Parent := nil;

      FEdit.Visible := FEditable;
    end;
    Invalidate;
  end;
end;

procedure TAdvToolBarExCustomItemPicker.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TAdvToolBarExCustomItemPicker.SetSelectedItem(const Value: String);
begin
  FItemIndex := FItems.IndexOf(Value);
  Invalidate;
end;

procedure TAdvToolBarExCustomItemPicker.SetSelectedItemIndex(
  const Value: Integer);
begin
  FItemIndex := Value;
  Invalidate;
end;

{ TAdvToolBarExButton }

procedure TAdvToolBarExButton.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExButton);
end;

{ TAdvToolBarExSeparator }

procedure TAdvToolBarExSeparator.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarExSeparator);
end;

{ TAdvToolBarEx }

procedure TAdvToolBarEx.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvToolBarEx);
end;

{$IFDEF WEBLIB}
function TAdvToolBarExControlObjectList.GetItem(
  Index: Integer): TAdvToolBarExControl;
begin
  Result := TAdvToolBarExControl(inherited Items[Index]);
end;

procedure TAdvToolBarExControlObjectList.SetItem(Index: Integer; const Value: TAdvToolBarExControl);
begin
  inherited Items[Index] := Value;
end;

function TAdvToolBarExControlList.GetItem(
  Index: Integer): TAdvToolBarExControl;
begin
  Result := TAdvToolBarExControl(inherited Items[Index]);
end;

procedure TAdvToolBarExControlList.SetItem(Index: Integer; const Value: TAdvToolBarExControl);
begin
  inherited Items[Index] := Value;
end;

function TAdvControlList.GetItem(Index: Integer): TControl;
begin
  Result := TControl(inherited Items[Index]);
end;

procedure TAdvControlList.SetItem(Index: Integer; const Value: TControl);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

initialization
  RegisterClass(TAdvToolBarExButton);
  {$IFDEF WEBLIB}
  TAdvBitmap.CreateFromResource(AdvTOOLBAREXPANDSVG);
  {$ENDIF}

end.
