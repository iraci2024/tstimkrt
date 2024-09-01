unit sSkinManager;
{$I sDefs.inc}
// {$DEFINE ACDEBUG}
// {$DEFINE LOGGED}
// {$DEFINE DEBUGOBJ}
// {$DEFINE CHECKIMGES}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, menus, ExtCtrls, StdCtrls,
{$IFDEF  DELPHI_XE2} UITypes, {$ENDIF}
{$IFNDEF DELPHI5} Types, {$ENDIF}
{$IFDEF  LOGGED} sDebugMsgs, {$ENDIF}
{$IFDEF  FPC} LMessages, {$ENDIF}
  sConst, sMaskData, sSkinMenus, sStyleSimply, sDefaults, acntUtils, acntTypes,
  acFontStore, SyncObjs;

{$IFNDEF NOTFORHELP}

const
  acCurrentVersion = '17.01';
  iMaxFileSize = 40000;
{$ENDIF}

type
{$IFNDEF NOTFORHELP}
  TacFileType = (ftBmp, ftFont, ftOther);

  TacFileItem = record
    Name: string;
    FileType: TacFileType;
    Stream: TMemoryStream;
  end;

  TacFileItems = array of TacFileItem;

  TacSkinMode = (smInternal, smExtPacked, smExtUnpacked);
  TacExtBordersMode = (ebmFull, ebmShadow);

  TacSkinListData = record
    skSkinMode: TacSkinMode;
    skImageIndex: integer;
    skName: string;
  end;

  TacSkinCommonInfo = record
    Active, UseAeroBluring: boolean;

    Version: real;

    Author, SkinPath, MainFont, Description: string;

    FXColor, Shadow1Color, DefLightColor, SysInactiveBorderColor: TColor;

    ArrowStyle: TacArrowsStyle;

    BIVAlign, // 0 - center, 1 - top, 2 - bottom
    BIKeepHUE, // 0 - variable HUE, 1 - keep unchanged
    BrightMin, BrightMax: integer;

    // Some sections indexes
    IndexGlobalInfo: integer;
    Sections: TacIntSections;

    OptionsDat: TMemIniFile;
  end;

  TsSkinManager = class;

  TacBmpData = record
    MulPPI, DivPPI, MaskType: integer;
    Path, // For debug
    FileName: string;
    Bmp: TBitmap;
    R, SrcRect: TRect;
    Ext: boolean;
  end;

  TacSkinData = class(TacObject)
  protected
    rsta, rsti: TBitmap;
    Initializing: boolean;
    BmpArray: array of TacBmpData;
    function ScaledInteger(const Name: string; Def: integer; Recalc: boolean;
      MaskIndex: integer = -1): integer;
    function AddNewSection(const AName: string;
      AParentSection, AStates, ATransparency: integer;
      AColor, AFontColor: TColor): integer;
    function MakeNewItem(SkinIndex: integer;
      const PropertyName, AClassName: string; ImgType: TacImgType; R: TRect;
      Count, DrawMode: integer; MaskType: smallint): integer;
    procedure AddImage(const Bmp: TBitmap; const s, sPropName: string;
      const R, SrcRect: TRect; SrcPPI, iMaskType: integer; aExt: boolean);

    function SearchBmpIndex(const aImgName: string; const aR: TRect): integer;
    function DefineBmp(var md: TsMaskData; const ImgName: string;
      const R: TRect; IgnoreFuchsia: boolean): boolean;
    procedure AddExternalItem(var md: TsMaskData; const s: string;
      Stream: TMemoryStream; IgnoreFuchsia: boolean);
    procedure UpdateItem(var md: TsMaskData; BmpIndex: integer);
  public
    FOwner: TsSkinManager;
    ScaleValue: integer;
    ScalePercent: integer;
    PPI: integer;
    // Scaled params
    Spacing, LineWidth, ArrowSize, SmallArrowSize, ScrollBtnLength, Shadow1Blur,
      Shadow1Offset,
    // Extended borders
    ExBorderWidth, ExTitleHeight, ExMaxHeight, ExContentOffs, ExCenterOffs,
      ExShadowOffs, ExShadowOffsR, ExShadowOffsT, ExShadowOffsB, BIRightMargin,
      BILeftMargin, BITopMargin,
    // Form calculated shadow data
    WndShadowRadius, WndShadowSize, WndShadowOffset: integer;
    // Nonscaled params
    Shadow1Transparency,
    // Extended borders
    ExDrawMode,
    // Title icons
    BISpacing,
    // Elements margins
    ComboBoxMargin, SliderMargin: integer;
    // Scrolls
    ScrollsWidth, ThumbWidth, ScrollArrows: integer;

    TabsCovering, RibbonCovering: byte;

    // Form calculated shadow colors
    WndShadowColorN, WndShadowColorA: TColor;

    Scrolls, ScrollBtns, UpDownBtns: TacSideElements;

    Sliders: TacBoolElements;

    MenuItem, ComboBtn: TacConstElementData;

    // Glyphs indexes
    TitleGlows: TacTitleGlowArray;
    GlowMargins: array [tgCloseAlone .. tgSmallClose] of integer;
    TitleGlyphs: array [TacTitleGlyph] of integer;
    RadioButton: array [boolean] of integer;
    CheckBox: array [TCheckBoxState] of integer;
    SmallCheckBox: array [TCheckBoxState] of integer;

    TrackBar: array [boolean] of TacTrackBarData;
    Tabs: TacAllTabs;

    // Other indexes
    ExBorder, GripVertical, GripHorizontal, GripRightBottom: integer;

    ShdaTemplate, ShdiTemplate: TBitmap;

    FormShadowSize: TRect;

    ma: TsMaskArray; // Array of images data
    oe: TacOutEffArray; // Array of outer effects
    gd: TsGeneralDataArray; // Array of sections data

    function Active: boolean; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}          // Deprecated. Use TsSkinManager.IsActive
    function BrightMin: integer; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}         // Deprecated. Use TsSkinManager.SkinCommonInfo.BrightMin;
    function BrightMax: integer; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}        // Deprecated. Use TsSkinManager.SkinCommonInfo.BrightMax;

    // Data loading
    procedure Init;
    procedure LoadParams;
    procedure LoadAllMasks;
    procedure InitMaskIndexes;
    procedure InitConstantIndexes;
    procedure InitMaskParams;
    procedure LoadAllGeneralData;
    procedure CheckShadows;
    function ScaleIntValue(Value: integer): integer;

    procedure Reset;
    procedure FreeBitmaps;
    procedure ClearBmpArray;

    destructor Destroy; override;
    function GetActiveShadow: TBitmap;
    function GetPassiveShadow: TBitmap;
    constructor Create(AOwner: TsSkinManager; aPPI: integer);
  end;

  TacSkinEditorData = packed record // Data for exchange with SkinEditor
    Magic: integer;
    Name1: array [0 .. 127] of AnsiChar; // Skin or section name
    Name2: array [0 .. 127] of AnsiChar; // Dir name
    Data1: array [0 .. 511] of AnsiChar; // Dir name
    Data2: array [0 .. iMaxFileSize] of AnsiChar; // SkinData
  end;

  PacSkinEditorData = ^TacSkinEditorData;

  TacSkinTypes = (stUnpacked, stPacked, stAllSkins);
  TacSkinPlaces = (spInternal, spExternal, spAllPlaces);

  TacMenuItemData = record
    Font: TFont;
  end;

  TacSysDlgData = record
    WindowHandle: THandle;
  end;

  TacGetExtraLineData = procedure(FirstItem: TMenuItem; var SkinSection: string;
    var Caption: string; var Glyph: TBitmap; var LineVisible: boolean)
    of object;
  TacSysDlgInit = procedure(DlgData: TacSysDlgData; var AllowSkinning: boolean)
    of object;
  TacGetPopupItemData = procedure(Item: TMenuItem; State: TOwnerDrawState;
    ItemData: TacMenuItemData) of object;
  TacSkinChangingEvent = procedure(Sender: TObject; const NewSkinName: string;
    var AllowChanging: boolean) of object;

  TBitmap = Graphics.TBitmap;
  TsStoredSkin = class;
  TacSkinInfo = type string;

  TacSkinEffects = class(TPersistent)
  private
    FAllowGlowing, FAllowAnimation, FAllowAeroBluring,
      FDiscoloredGlyphs: boolean;
    procedure SetBoolean(const Index: integer; const Value: boolean);
    procedure SetAllowAnimation(const Value: boolean);
  public
    FAllowOuterEffects: boolean;
    Manager: TsSkinManager;
    constructor Create;
  published
    property AllowAnimation: boolean read FAllowAnimation
      write SetAllowAnimation default True;
    property AllowAeroBluring: boolean read FAllowAeroBluring
      write FAllowAeroBluring default True;
    property AllowGlowing: boolean read FAllowGlowing write FAllowGlowing
      default True;

    property AllowOuterEffects: boolean Index 0 read FAllowOuterEffects
      write SetBoolean default True;
    property DiscoloredGlyphs: boolean Index 1 read FDiscoloredGlyphs
      write SetBoolean default False;
  end;

  TacLightingMode = (lmAffectNearest, lmActiveControl);
  TacClickEffect = (ceCircle, ceWaveOut, ceCircleAndWave);

  TacBtnEffects = class(TPersistent)
  private
    FEvents: TacBtnEvents;
    Manager: TsSkinManager;
    FLightingMode: TacLightingMode;
    FClickEffect: TacClickEffect;
    procedure SetLightingMode(const Value: TacLightingMode);
  public
    constructor Create(AOwner: TsSkinManager);
  published
    property Events: TacBtnEvents read FEvents write FEvents
      default [beMouseEnter, beMouseLeave, beMouseDown, beMouseUp];
    property LightingMode: TacLightingMode read FLightingMode
      write SetLightingMode default lmActiveControl;
    property ClickEffect: TacClickEffect read FClickEffect write FClickEffect
      default ceCircle;
  end;

  TacFormAnimation = class(TPersistent)
  private
    FTime: word;
    FActive: boolean;
    FMode: TacAnimType;
  public
    constructor Create; virtual;
    property Mode: TacAnimType read FMode write FMode default atAero;
  published
    property Active: boolean read FActive write FActive default True;
    property Time: word read FTime write FTime default 0;
  end;

  TacBlendOnMoving = class(TacFormAnimation)
  private
    FBlendValue: byte;
  public
    constructor Create; override;
  published
    property Active default False;
    property BlendValue: byte read FBlendValue write FBlendValue default 170;
    property Time default 1000;
  end;

  TacMinimizing = class(TacFormAnimation)
  public
    constructor Create; override;
  published
    property Time default 80;
  end;

  TacFormShow = class(TacFormAnimation)
  published
    property Mode;
  end;

  TacFormHide = class(TacFormAnimation)
  published
    property Mode;
  end;

  TacPageChange = class(TPersistent)
  private
    FTime: word;
    FActive: boolean;
    FMode: TacAnimTypeCtrl;
  public
    constructor Create;
  published
    property Mode: TacAnimTypeCtrl read FMode write FMode default atcFade;
    property Active: boolean read FActive write FActive default True;
    property Time: word read FTime write FTime default 0;
  end;

  TacDialogShow = class(TacFormAnimation)
  public
    constructor Create; override;
  published
    property Time default 0;
    property Mode;
  end;

  TacSkinChanging = class(TacFormAnimation)
  public
    constructor Create; override;
  published
    property Time default 100;
    property Mode default atFading;
  end;

  TacAnimEffects = class(TPersistent)
  private
    FFormHide, FDialogHide: TacFormHide;

    FFormShow: TacFormShow;
    FButtons: TacBtnEffects;
    FPageChange: TacPageChange;
    FMinimizing: TacMinimizing;
    FDialogShow: TacDialogShow;
    FSkinChanging: TacSkinChanging;
    FBlendOnMoving: TacBlendOnMoving;
  public
    Manager: TsSkinManager;
    constructor Create(AOwner: TsSkinManager);
    destructor Destroy; override;
  published
    property BlendOnMoving: TacBlendOnMoving read FBlendOnMoving
      write FBlendOnMoving;
    property Buttons: TacBtnEffects read FButtons write FButtons;
    property DialogShow: TacDialogShow read FDialogShow write FDialogShow;
    property FormShow: TacFormShow read FFormShow write FFormShow;
    property FormHide: TacFormHide read FFormHide write FFormHide;
    property DialogHide: TacFormHide read FDialogHide write FDialogHide;
    property Minimizing: TacMinimizing read FMinimizing write FMinimizing;
    property PageChange: TacPageChange read FPageChange write FPageChange;
    property SkinChanging: TacSkinChanging read FSkinChanging
      write FSkinChanging;
  end;

  TsStoredSkin = class(TCollectionItem)
  private
    FName, FAuthor, FDescription: string;

    FShadow1Blur, FShadow1Offset, FShadow1Transparency: integer;

    FBorderColor, FShadow1Color: TColor;

    FVersion: real;
    FMasterBitmap: TBitmap;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
    procedure ReadOriginName(Reader: TReader);
    procedure WriteOriginName(Writer: TWriter);
  public
    OrigName: string;
    PackedData: TMemoryStream;
    procedure Assign(Source: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property MasterBitmap: TBitmap read FMasterBitmap write FMasterBitmap;

    property Shadow1Color: TColor read FShadow1Color write FShadow1Color
      default clBlack;
    property Shadow1Offset: integer read FShadow1Offset write FShadow1Offset
      default 0;
    property Shadow1Blur: integer read FShadow1Blur write FShadow1Blur
      default -1;
    property Shadow1Transparency: integer read FShadow1Transparency
      write FShadow1Transparency default 0;

    property BorderColor: TColor read FBorderColor write FBorderColor
      default clFuchsia;

    property Version: real read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
  end;

  TsStoredSkins = class(TCollection)
  private
    FOwner: TsSkinManager;
    function GetItem(Index: integer): TsStoredSkin;
    procedure SetItem(Index: integer; Value: TsStoredSkin);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TsSkinManager);
    destructor Destroy; override;
    property Items[Index: integer]: TsStoredSkin read GetItem
      write SetItem; default;
    function IndexOf(const SkinName: string): integer;
  end;

  TsThirdPartyList = class(TPersistent)
  private
    FThirdEdits, FThirdButtons, FThirdBitBtns, FThirdCheckBoxes,
      FThirdGroupBoxes, FThirdListViews, FThirdPanels, FThirdGrids,
      FThirdTreeViews, FThirdComboBoxes, FThirdWWEdits, FThirdVirtualTrees,
      FThirdGridEh, FThirdPageControl, FThirdTabControl, FThirdToolBar,
      FThirdStatusBar, FThirdSpeedButton, FThirdScrollControl, FThirdUpDownBtn,
{$IFDEF ADDWEBBROWSER}
    FThirdWebBrowser,
{$ENDIF}
    FThirdScrollBar, FThirdStaticText, FThirdNativePaint: string;
  public
    // These functions used in the LoadThirdParty and other procedures
    function GetString(const Index: integer): string;
    procedure SetString(const Index: integer; const Value: string);
  published
    property ThirdEdits: string index ord(tpEdit)read GetString write SetString;
    property ThirdButtons: string index ord(tpButton)read GetString
      write SetString;
    property ThirdBitBtns: string index ord(tpBitBtn)read GetString
      write SetString;
    property ThirdCheckBoxes: string index ord(tpCheckBox)read GetString
      write SetString;
    property ThirdGroupBoxes: string index ord(tpGroupBox)read GetString
      write SetString;
    property ThirdListViews: string index ord(tpListView)read GetString
      write SetString;
    property ThirdPanels: string index ord(tpPanel)read GetString
      write SetString;
    property ThirdGrids: string index ord(tpGrid)read GetString write SetString;
    property ThirdTreeViews: string index ord(tpTreeView)read GetString
      write SetString;
    property ThirdComboBoxes: string index ord(tpComboBox)read GetString
      write SetString;
    property ThirdWWEdits: string index ord(tpWWEdit)read GetString
      write SetString;
    property ThirdVirtualTrees: string index ord(tpVirtualTree)read GetString
      write SetString;
    property ThirdGridEh: string index ord(tpGridEh)read GetString
      write SetString;
    property ThirdPageControl: string index ord(tpPageControl)read GetString
      write SetString;
    property ThirdTabControl: string index ord(tpTabControl)read GetString
      write SetString;
    property ThirdToolBar: string index ord(tpToolBar)read GetString
      write SetString;
    property ThirdStatusBar: string index ord(tpStatusBar)read GetString
      write SetString;
    property ThirdSpeedButton: string index ord(tpSpeedButton)read GetString
      write SetString;
    property ThirdScrollControl: string index ord(tpScrollControl)read GetString
      write SetString;
    property ThirdUpDown: string index ord(tpUpDownBtn)read GetString
      write SetString;
    property ThirdScrollBar: string index ord(tpScrollBar)read GetString
      write SetString;
    property ThirdStaticText: string index ord(tpStaticText)read GetString
      write SetString;
    property ThirdNativePaint: string index ord(tpNativePaint)read GetString
      write SetString;
{$IFDEF ADDWEBBROWSER}
    property ThirdWebBrowser: string index ord(tpWebBrowser)read GetString
      write SetString;
{$ENDIF}
  end;

  TacSkinningRule = (srStdForms, srStdDialogs, srThirdParty);
  TacSkinningRules = set of TacSkinningRule;

  TacPaletteColors = (pcMainColor, pcLabelText, pcWebText, pcWebTextHot,
    pcEditText, pcEditBG, pcSelectionBG, pcSelectionText, pcSelectionBG_Focused,
    pcSelectionText_Focused, pcEditBG_Inverted, pcEditText_Inverted,
    pcEditBG_OddRow, pcEditBG_EvenRow, pcEditText_Ok, pcEditText_Warning,
    pcEditText_Alert, pcEditText_Caution, pcEditText_Bypassed,

    pcEditBG_Ok, pcEditBG_Warning, pcEditBG_Alert, pcEditBG_Caution,
    pcEditBG_Bypassed,

    pcEditText_Highlight1, pcEditText_Highlight2, pcEditText_Highlight3,

    pcBtnColor1Active, pcBtnColor2Active, pcBtnBorderActive, pcBtnFontActive,
    pcBtnColor1Normal, pcBtnColor2Normal, pcBtnBorderNormal, pcBtnFontNormal,
    pcBtnColor1Pressed, pcBtnColor2Pressed, pcBtnBorderPressed,
    pcBtnFontPressed,

    // Button color tones
    pcBtnToneRed, pcBtnToneRedActive, // Red
    pcBtnToneGreen, pcBtnToneGreenActive, // Green
    pcBtnToneBlue, pcBtnToneBlueActive, // Blue
    pcBtnToneYellow, pcBtnToneYellowActive, // Yellow

    // Button glyph and text colors
    pcBtnRedText, pcBtnRedTextActive, // Red
    pcBtnGreenText, pcBtnGreenTextActive, // Green
    pcBtnBlueText, pcBtnBlueTextActive, // Blue
    pcBtnYellowText, pcBtnYellowTextActive, // Yellow

    pcBorder, pcGrid, pcHintBG, pcHintText);

  TacPaletteArray = array [TacPaletteColors] of TColor;

  TacBrushes = array [pcMainColor .. pcEditBG] of HBRUSH;

  TacScrollBarsSupport = class(TPersistent)
  private
    FScrollSize, FButtonsSize: integer;
    FOwner: TsSkinManager;
    procedure SetInteger(const Index, Value: integer);
  public
    constructor Create(AOwner: TsSkinManager);
  published
    property ButtonsSize: integer index 0 read FButtonsSize write SetInteger
      default -1;
    property ScrollSize: integer index 1 read FScrollSize write SetInteger
      default -1;
  end;

  TacModalButtonsColoringValue = (bcBackground, bcGlyph, bcText);
  TacModalButtonsColoring = set of TacModalButtonsColoringValue;

  TacButtonsSupport = class(TPersistent)
  private
    FShowFocusRect, FShiftContentOnClick: boolean;
    FModalButtonsColoring: TacModalButtonsColoring;
    FOwner: TsSkinManager;
    FAllowRTLSysButtons: boolean;
    function GetOldGlyphsMode: boolean;
    procedure SetOldGlyphsMode(const Value: boolean);
  public
    constructor Create(AOwner: TsSkinManager);
  published
    property OldGlyphsMode: boolean read GetOldGlyphsMode
      write SetOldGlyphsMode;
    property ShowFocusRect: boolean read FShowFocusRect write FShowFocusRect
      default False;
    property ShiftContentOnClick: boolean read FShiftContentOnClick
      write FShiftContentOnClick default True;
    property AllowRTLSysButtons: boolean read FAllowRTLSysButtons
      write FAllowRTLSysButtons default True;
    property ModalButtonsColoring: TacModalButtonsColoring
      read FModalButtonsColoring write FModalButtonsColoring default [bcGlyph];
  end;

  TacLabelsSupport = class(TPersistent)
  private
    FTransparentAlways: boolean;
  public
    constructor Create;
  published
    property TransparentAlways: boolean read FTransparentAlways
      write FTransparentAlways default True;
  end;

  TacPixelsPerInch = 48 .. 384;

  TacGroupBoxStyle = (gsStandard, gsCard);

  TacOptions = class(TPersistent)
  private
    FNoMouseHover, FStdGlyphsOrder, FCheckEmptyAlpha, FChangeSysColors,
      FStdImgTransparency, FDrawNonClientArea, FNativeBordersMaximized: boolean;
    FOwner: TsSkinManager;
    FArrowsStyle: TacArrowsStyle;
    FPixelsPerInch: TacPixelsPerInch;
    FOptimizingPriority: TacOptimizingPriority;
    FGroupBoxStyle: TacGroupBoxStyle;
    FRotateArrows: boolean;
    function GetScaleMode: TacScaleMode;
    function GetBool(const Index: integer): boolean;
    procedure SetBool(const Index: integer; const Value: boolean);
    procedure SetScaleMode(const Value: TacScaleMode);
    procedure SetArrowsStyle(const Value: TacArrowsStyle);
    procedure SetPixelsPerInch(const Value: TacPixelsPerInch);
    procedure DoScaleChanged(NewScaleMode: TacScaleMode; ReduceSize: boolean);
    procedure SetNewScaleMode(OldScaleMode, NewScaleMode: TacScaleMode;
      OldPPI, NewPPI: integer);
    procedure SetGroupBoxStyle(const Value: TacGroupBoxStyle);
  protected
    FScaleMode: TacScaleMode;
  public
    function ActualArrowStyle: TacArrowsStyle;
    constructor Create(AOwner: TsSkinManager);
  published
    property ChangeSysColors: boolean index 4 read GetBool write SetBool
      default False;
    property CheckEmptyAlpha: boolean index 0 read GetBool write SetBool
      default False;
    property DrawNonClientArea: boolean index 6 read GetBool write SetBool
      default True;
    property NativeBordersMaximized: boolean index 2 read GetBool write SetBool
      default False;
    property NoMouseHover: boolean index 1 read GetBool write SetBool
      default False;
    property StdGlyphsOrder: boolean index 3 read GetBool write SetBool
      default False;
    property StdImgTransparency: boolean index 5 read GetBool write SetBool
      default False;
    property ArrowsStyle: TacArrowsStyle read FArrowsStyle write SetArrowsStyle
      default arsDefault;
    property OptimizingPriority: TacOptimizingPriority read FOptimizingPriority
      write FOptimizingPriority default opSpeed;
    property ScaleMode: TacScaleMode read GetScaleMode write SetScaleMode
      default DefScaleMode;
    property PixelsPerInch: TacPixelsPerInch read FPixelsPerInch
      write SetPixelsPerInch default 96;
    property RotateArrows: boolean read FRotateArrows write FRotateArrows
      default False;
    property GroupBoxStyle: TacGroupBoxStyle read FGroupBoxStyle
      write SetGroupBoxStyle default gsStandard;
  end;

  TacSkinListController = class;

  TacAddThread = class(TThread)
  public
    FOwner: TacSkinListController;
    TimeEvent: TSimpleEvent;
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoTimer;
  end;

  TacSkinsFilterItem = (sfiInternal, sfiExternal);
  TacSkinsFilter = set of TacSkinsFilterItem;

  TacSkinListController = class(TObject)
  protected
    procedure ClearList(DoUpdateLater: boolean = True;
      DoSendChanged: boolean = True);
    procedure ClearFileInfo;
  public
    sPath: string;
    Timer: TTimer;
    DosCode: integer;
    ImgList: TImageList;
    Thread: TacAddThread;
    UpdateNeeded: boolean;
    FileInfo: TacSearchRec;
    Controls: TacCtrlArray;
    SkinManager: TsSkinManager;
    CurrentInternalIndex: integer;
    SkinList: array of TacSkinListData;
    procedure SendSkinChanged;
    procedure SendListChanged;
    function AddSkinData: integer;
    procedure AddSkin(const AName: string; IntIndex: integer);
    function UpdateData(UpdateNow: boolean = False): boolean;
    procedure StartDelayedLoading;
    function SkinsAllowed(FiltType: TacSkinsFilterItem): boolean;
    procedure UpdateIfNeeded(ForceUpdating: boolean = False);
    procedure CheckThread;
    constructor Create(AOwner: TsSkinManager);
    function CtrlIndex(Ctrl: TControl): integer;
    procedure AddControl(Ctrl: TControl);
    procedure DelControl(Ctrl: TControl);
    destructor Destroy; override;
  end;

  TScaleChangeData = record
    OldScaleMode, NewScaleMode: TacScaleMode;
    OldScalePercent, NewScalePercent, OldPPI, NewPPI: integer;
  end;

  TScaleChangeEvent = procedure(Sended: TObject;
    ScaleChangeData: TScaleChangeData) of object;
  TFontChanged = procedure(Sender: TObject;
    const DefOldFontName, FontName: string) of object;

  TacSkinConvertor = class(TPersistent)
  public
    Options, PackedData: TMemoryStream;
    FileCount: integer;
    Files: TacFileItems;
    procedure Clear;
    destructor Destroy; override;
  end;

  TacFontMode = (fmDefault, fmFromSkin, fmCustom);

  TacFonts = class(TPersistent)
  private
    FMainFont: string;
    FMainMode: TacFontMode;
    procedure SetMainFont(const Value: string);
    procedure SetMainMode(const Value: TacFontMode);
  public
    SkinManager: TsSkinManager;
    constructor Create(AOwner: TsSkinManager);
  published
    property MainMode: TacFontMode read FMainMode write SetMainMode
      default fmDefault;
    property MainFont: string read FMainFont write SetMainFont;
  end;

  TacCharArray = array [1 .. 16] of AnsiChar;
  TacManagerStateValue = (msFontChanging, msScaleChanging, msSkinRemoving,
    msSkinChanging, msBroadCasting, msAnimScaling, msFormScaling, msRTLFlipped);
  TacManagerState = set of TacManagerStateValue;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}

  TsSkinManager = class(TComponent)
  private
{$IFNDEF NOTFORHELP}
    FActive, FSkinnedPopups, FExtendedBorders, CommonDataLoaded,
      GlobalHookInstalled: boolean;
    FOnActivate, FOnDeactivate, FOnSkinLoading, FOnAfterChange, FOnBeforeChange,
      FOnSkinListChanged: TNotifyEvent;
    FKeyList, FCommonSections: TStringList;
    FGroupIndex: integer;
    FOptions: TacOptions;
    FActiveControl: hwnd;
    FSkinName: TsSkinName;
    FEffects: TacSkinEffects;
    FSkinDirectory: TsDirectory;
    FBuiltInSkins: TsStoredSkins;
    FMenuSupport: TacMenuSupport;
    FAnimEffects: TacAnimEffects;
    FSkinsFilter: TacSkinsFilter;
    FThirdParty: TsThirdPartyList;
    FSkinningRules: TacSkinningRules;
    FLabelsSupport: TacLabelsSupport;
    FButtonsSupport: TacButtonsSupport;
    FScrollsOptions: TacScrollBarsSupport;
    FOnSysDlgInit: TacSysDlgInit;
    FOnScaleModeChange: TScaleChangeEvent;
    FOnGetPopupLineData: TacGetExtraLineData;
    FOnGetPopupItemData: TacGetPopupItemData;
{$IFNDEF FPC}
    FFonts: TacFonts;
    FOnFontChanged: TFontChanged;
    FOnScaleChanged: TNotifyEvent;
    FSkinableMenus: TsSkinableMenus;
{$ENDIF}
    sc: TacSkinConvertor;
    SkinFile: TMemIniFile;
    FUpdatedSectionNdx: word;
    FOnSkinChanging: TacSkinChangingEvent;
    FExtBordersMode: TacExtBordersMode;
    procedure UpdateCurrentSkin;
    function MainWindowHook(var Message: TMessage): boolean;
    // Props getters
    function GetVersion: string;
    function GetIsDefault: boolean;
    function GetSkinInfo: TacSkinInfo;
    function GetExtendedBorders: boolean;
    // Props setters
    procedure SetActiveControl(const Value: hwnd);
    procedure SetSkinDirectory(const Value: string);
    procedure SetVersion(const Value: string);
    procedure SetActive(const Value: boolean);
    procedure SetIsDefault(const Value: boolean);
    procedure SetExtendedBorders(const Value: boolean);
{$IFNDEF FPC}
    procedure SetSkinnedPopups(const Value: boolean);
{$ENDIF}
    procedure SetSkinName(const Value: TsSkinName);
    procedure SetSkinInfo(const Value: TacSkinInfo);
    procedure SetKeyList(const Value: TStringList);
    procedure SetBuiltInSkins(const Value: TsStoredSkins);
    procedure SetActiveGraphControl(const Value: TGraphicControl);
    procedure SetSkinningRules(const Value: TacSkinningRules);
    procedure SetCommonSections(const Value: TStringList);
    procedure SetInteger(const Index, Value: integer);
    procedure SetSkinsFilter(const Value: TacSkinsFilter);
    procedure SetExtBordersMode(const Value: TacExtBordersMode);
  protected
    DefStoredFontName, DefOldFontName, DefNewFontName: string;
    FActiveGraphControl: TGraphicControl;
{$IFNDEF D2005}
    TimerCheckHot: TTimer;
    // Avoiding of mouseenter/leaveproblem in old Delphi versions
    procedure OnCheckHot(Sender: TObject);
{$ENDIF}
    procedure SendNewSkin(Repaint: boolean = True);
    procedure SendRemoveSkin;
    procedure InitCommonData;
    procedure RepaintSection(sName: string);
{$ENDIF} // NOTFORHELP
  public
    NoAutoUpdate: boolean;
    ShowState: TShowAction;
    PreviewBuffer: TacSkinEditorData; // Data for exchange with SkinEditor
    ManagerState: TacManagerState;
{$IFNDEF NOTFORHELP}
    SkinIsPacked: boolean;
    FHueOffset, FSaturation, SysFontScale, // 0 - 100, 1 - 125, 2 - 150
    FBrightness: integer;
    Brushes: TacBrushes;
    ThirdLists: TStringLists;
    EmbeddedFonts: TacEmbeddedFonts;
    SkinListController: TacSkinListController;
    // Current skin data
    MasterBitmap: TBitmap;
    Palette: TacPaletteArray; // Array of skin colors
    SkinCommonInfo: TacSkinCommonInfo;
    // Record with common data and indexes of sections
    // Scaled skin data
    SkinDataArray: array of TacSkinData;
    function ma: TsMaskArray; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF} // Use CommonSkinData.ma
    function oe: TacOutEffArray; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF} // Use CommonSkinData.oe
    function gd: TsGeneralDataArray; {$IFDEF WARN_DEPRECATED} deprecated;
{$ENDIF} // Use CommonSkinData.gd
    function CommonSkinData(aPPI: integer = 96): TacSkinData;
    // Current skin params. Some data moved to 'SkinCommonInfo'
    procedure ClearSkinData;
    procedure ClearSkinArrays;
    // ConstData // 'ConstData' moved to 'SkinCommonInfo' and 'CommonSkinData'
    // Skin loading
    procedure LoadFonts;
    procedure LoadCommonData;
    procedure InitSkinParams(aPPI: integer);
    function InitSkinFile: boolean;
    procedure CloseSkinFile;
    procedure FlipImages(RTL: boolean);
    procedure ReloadSkin(PPI: integer = 96);
    procedure CheckVersion;
    // Getting info
    function GetScale: integer;
    function GetSkinNames(sl: TacStrings; CreateDataObject: boolean = False;
      SkinType: TacSkinTypes = stAllSkins): acString;
    function GetExternalSkinNames(sl: TacStrings;
      CreateDataObject: boolean = False; SkinType: TacSkinTypes = stAllSkins)
      : acString;
    // Extract appropriate image source from skin, result is PPI
    function GetScaledMaskData(const aSectionName, aPropName: string;
      { aDivPPI, } aMulPPI: integer; var md: TsMaskData): integer;
    function GetFullSkinDirectory: string;
    function GetRandomSkin: acString;
    function LengthOfGD: integer;
    // Getting current skin info
    function GetSkinIndex(const SkinSection: string;
      SkinData: TacSkinData = nil): integer;
    function GetMaskIndex(const SkinSection, mask: string;
      SkinData: TacSkinData { = nil } ): integer; overload;
    function GetMaskIndex(const SkinIndex: integer; const mask: string;
      SkinData: TacSkinData { = nil } ): integer; overload;
    function GetMaskIndex(const aSkinIndex: integer;
      const SkinSection, mask: string; SkinData: TacSkinData { = nil } )
      : integer; overload;
    function GetTextureIndex(aSkinIndex: integer;
      const SkinSection, PropName: string; SkinData: TacSkinData { = nil } )
      : integer; overload;
    function GetTextureIndex(aSkinIndex: integer; const PropName: string;
      SkinData: TacSkinData { = nil } ): integer; overload;
    procedure GetSkinSections(sl: TStrings; SkinData: TacSkinData = nil);
    function IsValidSkinIndex(SkinIndex: integer): boolean;
    function IsValidImgIndex(ImageIndex: integer): boolean;
    // Getting colors
    function GetGlobalColor: TColor;
    function GetGlobalFontColor: TColor;
    function GetActiveEditColor: TColor;
    function GetActiveEditFontColor: TColor;
    function GetHighLightColor(Focused: boolean = True): TColor;
    function GetHighLightFontColor(Focused: boolean = True): TColor;
    // Updating
    procedure BeginUpdate;
    procedure EndUpdate(Repaint: boolean = False;
      AllowAnimation: boolean = True);
    function ScaleInt(Value: integer; SysScale: integer = -1): integer;
    function ScalePPI(Value: integer; aPPI: integer): integer;
    procedure RepaintForms(DoLockForms: boolean = True);
    procedure UpdateSkin(Repaint: boolean = True);
    procedure UpdateSkinSection(const SectionName: string);
    procedure UpdateScale(Ctrl: TWinControl; iCurrentPPI: integer = 96;
      iNewDPI: integer = 0);
    procedure UpdateAllScale;
    // Hooks
    procedure InstallHook;
    procedure UnInstallHook;
    // Other routines
    procedure ExtractInternalSkin(const NameOfSkin, DestDir: string);
    procedure ExtractByIndex(Index: integer; const DestDir: string);
    // Font routines
    procedure ChangeAppFont(const FontName: string;
      DoLockForms: boolean = True);
    function UpdateFontName(Ctrl: TControl): boolean;
    procedure StoreDefFont;
    // Inherited
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Loaded; override;
    function IsActive: boolean;
    // Properties
    property GroupIndex: integer read FGroupIndex write FGroupIndex;
{$IFNDEF FPC}
    property SkinableMenus: TsSkinableMenus read FSkinableMenus
      write FSkinableMenus;
{$ENDIF} // FPC
    property ActiveControl: hwnd read FActiveControl write SetActiveControl;
    property ActiveGraphControl: TGraphicControl read FActiveGraphControl
      write SetActiveGraphControl;
{$ENDIF} // NOTFORHELP
  published
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnScaleModeChange: TScaleChangeEvent read FOnScaleModeChange
      write FOnScaleModeChange; // Declare before the Options property

    property SkinsFilter: TacSkinsFilter read FSkinsFilter write SetSkinsFilter
      default [sfiInternal, sfiExternal];
    property Effects: TacSkinEffects read FEffects write FEffects;
    property AnimEffects: TacAnimEffects read FAnimEffects write FAnimEffects;
    property ButtonsOptions: TacButtonsSupport read FButtonsSupport
      write FButtonsSupport;

    property Active: boolean read FActive write SetActive default True;
    property ExtendedBorders: boolean read GetExtendedBorders
      write SetExtendedBorders default False;
    property IsDefault: boolean read GetIsDefault write SetIsDefault
      default True;
{$IFNDEF FPC}
    property SkinnedPopups: boolean read FSkinnedPopups write SetSkinnedPopups
      default True;
{$ENDIF}
    property Brightness: integer index 0 read FBrightness write SetInteger
      default 0;
    property HueOffset: integer index 1 read FHueOffset write SetInteger
      default 0;
    property Saturation: integer index 2 read FSaturation write SetInteger
      default 0;

    property CommonSections: TStringList read FCommonSections
      write SetCommonSections;
    property KeyList: TStringList read FKeyList write SetKeyList;

    property InternalSkins: TsStoredSkins read FBuiltInSkins
      write SetBuiltInSkins;
    property LabelsOptions: TacLabelsSupport read FLabelsSupport
      write FLabelsSupport;
    property MenuSupport: TacMenuSupport read FMenuSupport write FMenuSupport;
    property Options: TacOptions read FOptions write FOptions;
    property Fonts: TacFonts read FFonts write FFonts;
    property ScrollsOptions: TacScrollBarsSupport read FScrollsOptions
      write FScrollsOptions;
    property SkinDirectory: TsDirectory read FSkinDirectory
      write SetSkinDirectory;
    property SkinName: TsSkinName read FSkinName write SetSkinName;
    property SkinInfo: TacSkinInfo read GetSkinInfo write SetSkinInfo;
    property SkinningRules: TacSkinningRules read FSkinningRules
      write SetSkinningRules default [srStdForms, srStdDialogs, srThirdParty];
    property ThirdParty: TsThirdPartyList read FThirdParty write FThirdParty;
    property Version: string read GetVersion write SetVersion stored False;

    property ExtBordersMode: TacExtBordersMode read FExtBordersMode
      write SetExtBordersMode default ebmFull;
    { :@event }
    property OnGetMenuExtraLineData: TacGetExtraLineData
      read FOnGetPopupLineData write FOnGetPopupLineData;
    { :@event }
    property OnGetPopupItemData: TacGetPopupItemData read FOnGetPopupItemData
      write FOnGetPopupItemData;
    { :@event }
    property OnSysDlgInit: TacSysDlgInit read FOnSysDlgInit write FOnSysDlgInit;

    property OnAfterChange: TNotifyEvent read FOnAfterChange
      write FOnAfterChange;
    property OnBeforeChange: TNotifyEvent read FOnBeforeChange
      write FOnBeforeChange;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnFontChanged: TFontChanged read FOnFontChanged
      write FOnFontChanged;
    property OnScaleChanged: TNotifyEvent read FOnScaleChanged
      write FOnScaleChanged;
    property OnSkinListChanged: TNotifyEvent read FOnSkinListChanged
      write FOnSkinListChanged;
    property OnSkinLoading: TNotifyEvent read FOnSkinLoading
      write FOnSkinLoading;
    property OnSkinChanging: TacSkinChangingEvent read FOnSkinChanging
      write FOnSkinChanging;
  end;

{$IFNDEF NOTFORHELP}

var
  IsNT: boolean;
  acEditorSkinFile: TStringList;
  DefaultManager: TsSkinManager;
  UnPackedFirst: boolean = False;
  StdScaleArray: array [0 .. acMaxScaleNdx] of TacSkinData;

function SysColorToSkin(const AColor: TColor;
  ASkinManager: TsSkinManager = nil): TColor;
{$IFDEF D2007}
procedure UpdateCommonDlgs(sManager: TsSkinManager);
{$ENDIF}
procedure UpdatePreview(Handle: hwnd; Enabled: boolean);
// function ChangeImageInSkin(const SkinSection, PropName, FileName: string; sm: TsSkinManager): boolean;
function CurrentPPI(sm: TsSkinManager; OwnerCtrl: TControl;
  CheckActive: boolean = False): integer;
function GetScrollSize(sm: TsSkinManager; ScalePPI: integer = 96): integer;
function GetComboBtnSize(sm: TsSkinManager; ScalePPI: integer): integer;
procedure UpdateThirdNames(sm: TsSkinManager);
procedure LoadThirdNames(sm: TsSkinManager; Overwrite: boolean = False);
procedure LoadSkinFromFile(const FileName: string;
  var Convertor: TacSkinConvertor; pwds: TStringList; SkinManager: TComponent);
procedure Load3rdNames(AThirdParty: TsThirdPartyList; AThirdLists: TStringLists;
  var AOwnThirdLists: boolean);
function SearchSupport(const Lists: TStringLists;
  const ClassName: string): boolean;

{$IFNDEF DISABLEPREVIEWMODE}
procedure ReceiveData(SkinReceiver: TsSkinManager; SectionOnly: boolean);
{$ENDIF}
function ExtractPackedData(var Convertor: TacSkinConvertor; pwds: TStringList;
  SkinManager: TComponent): boolean;
function GetPreviewStream(aStream: TMemoryStream; const SkinFileName: string)
  : boolean; overload;
function GetPreviewStream(aStream: TMemoryStream; SrcStream: TMemoryStream)
  : boolean; overload;
function GetPreviewImage(aBitmap: TBitmap; const SkinFileName: string): boolean;
function ConvertSkinStr(const s: string { ; Bmp: TBitmap } ): string;
// Function will be removed in Stable release

procedure SetPPIAnimated(NewPPI: integer);
function IsSysPPIUsed(sm: TsSkinManager): boolean;

{$IFNDEF DEBUGOBJ}
{$IFDEF WIN64}
{$L xdecode64.obj}
{$ELSE}
{$L xdecode32.obj}
{$ENDIF}
function asSkinDecode(PackedData: TacCharArray; Keys: array of Int64;
  const Length, FormSum: integer; out FilesCount: integer; out Offset: integer)
  : integer; cdecl; external;
{$ENDIF}
{$ENDIF} // NOTFORHELP

implementation

uses
  math, ImgList, jpeg,
{$IFNDEF DELPHI7UP}acZLibEx, {$ELSE}ZLibEx, {$ENDIF}
{$IFDEF DEBUGOBJ}
  xdecode,
{$ENDIF}
{$IFNDEF ALITE}
  acPopupController,
{$ENDIF}
{$IFDEF TNTUNICODE} TntSysUtils, {$IFNDEF D2006} TntWideStrings, {$ELSE} WideStrings, {$ENDIF} TntClasses, {$ENDIF}
  sMessages, acAlphaImageList, sVclUtils, sCommonData, acGlow, sThirdParty,
  sSkinProps, acDials, sGraphUtils,
  sGradient, sSkinProvider, sAlphaGraph, acPNG, acAnimation;

var
  OSVerInfo: TOSVersionInfo;

procedure AddExtItem(var md: TsMaskData; const s: string; Stream: TMemoryStream;
  IgnoreFuchsia: boolean; sm: TsSkinManager; sd: TacSkinData = nil);
var
  sName: string;
  TmpBmp: TBitmap;
  TempJpg: TacJpegClass;
begin
  md.Bmp := nil;
  if Stream = nil then
  begin // If unpacked file used
    if pos(':', s) <= 0 then
      sName := sm.SkinCommonInfo.SkinPath + s
    else
      sName := s;
    if not FileExists(sName) then
      Exit;
  end
  else
    Stream.Seek(0, 0);
  TmpBmp := TBitmap.Create;
  TmpBmp.IgnorePalette := True;
  if pos('.BMP', s) > 0 then
    if Stream = nil then
      TmpBmp.LoadFromFile(sName)
    else
      TmpBmp.LoadFromStream(Stream)
  else if (pos('.JPG', s) > 0) or (pos('.JPEG', s) > 0) then
  begin
    TempJpg := TacJpegClass.Create;
{$IFNDEF TINYJPG}
    TempJpg.Performance := jpBestSpeed;
{$ENDIF}
    if Stream = nil then
      TempJpg.LoadFromFile(sName)
    else
      TempJpg.LoadFromStream(Stream);
    TmpBmp.Width := TempJpg.Width;
    TmpBmp.Height := TempJpg.Height;
    TmpBmp.Canvas.Draw(0, 0, TempJpg);
    TempJpg.Free;
  end
  else if pos('.PNG', s) > 0 then
  begin
    if Stream = nil then
      LoadBmpFromPngFile(TmpBmp, sName)
    else
      LoadBmpFromPngStream(TmpBmp, Stream);

    if sm.FBrightness <= 0 then
      // Otherwise updated later (after changing of brightness)
      if md.ImgType <> itisaBorder then
        UpdateAlpha(TmpBmp, MkRect(TmpBmp));
  end;

  if (TmpBmp <> nil) and (TmpBmp.PixelFormat <> pf32bit) then
  begin
    SetBmp32Pixels(TmpBmp);
    FillAlphaRect(TmpBmp, MkRect(TmpBmp), MaxByte, clFuchsia);
  end;
  md.Bmp := TmpBmp;
  md.R := MkRect(TmpBmp);
  md.Height := TmpBmp.Height;
  md.Width := TmpBmp.Width;
  // Scale
  if md.MulPPI <> md.DivPPI then
    ScaleMaskData(md, sm.MasterBitmap, IgnoreFuchsia or
      (md.ImgType in [itisaPngGlyph, itisaTexture]));

  // Add to array
  if sd <> nil then
    sd.AddImage(md.Bmp, s, md.ClassName + '-' + md.PropertyName, MkRect, MkRect,
      md.DivPPI, md.MaskType, True);
end;

function GetSkinVersion(sf: TMemIniFile): real;
var
  OldSeparator: char;
begin
  OldSeparator := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;
{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := s_Dot;
  Result := sf.ReadFloat(s_GlobalInfo, s_Version, 0);
{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := OldSeparator;
end;

{$IFNDEF DISABLEPREVIEWMODE}

procedure ReceiveData(SkinReceiver: TsSkinManager; SectionOnly: boolean);

  function PartialPreview: boolean;
  begin
    Result := (SkinReceiver.PreviewBuffer.Name2 <> '') and
      (SkinReceiver.HueOffset = 0) and (SkinReceiver.Brightness = 0) and
      (SkinReceiver.Saturation = 0);
  end;

begin
  with SkinReceiver do
    if PreviewBuffer.Magic = ASE_MSG then
    begin
      UnPackedFirst := True; // Unpacked skins have the first priority

      if acEditorSkinFile = nil then
        acEditorSkinFile := TStringList.Create;

      acEditorSkinFile.Text := PreviewBuffer.Data2;
      SkinDirectory := PreviewBuffer.Data1;
      if PartialPreview then
      begin
        NoAutoUpdate := True;
        SkinName := PreviewBuffer.Name1;
        ManagerState := ManagerState + [msSkinChanging];
        RepaintSection(PreviewBuffer.Name2);
        ManagerState := ManagerState - [msSkinChanging];
        NoAutoUpdate := False;
      end
      else
      begin
        BeginUpdate;
        SkinName := PreviewBuffer.Name1;
        EndUpdate(True, False);
      end;
      FreeAndNil(acEditorSkinFile);
    end;
end;
{$ENDIF}

function ExtInt(const aPos: integer; const s: string;
  const aDelim: TSysCharSet): integer;
{$IFNDEF DELPHI6UP}
var
  subs: string;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  if not TryStrToInt(ExtractWord(aPos, s, aDelim), Result) then
    Result := 0;
{$ELSE}
  subs := ExtractWord(aPos, s, aDelim);
  if subs <> '' then
    Result := StrToInt(subs)
  else
    Result := 0;
{$ENDIF}
end;

function CopyInt(const s: string; pos, Size: integer): integer;
begin
  Result := StrToInt(Copy(s, pos, Size));
end;

function CopyRect(const s: string; pos: integer): TRect;
begin
  Result := Rect(CopyInt(s, pos, 4), CopyInt(s, pos + 5, 4),
    CopyInt(s, pos + 10, 4), CopyInt(s, pos + 15, 4));
end;

function CopyR(const s: string; pos: integer): TRect;
begin
  Result := Rect(CopyInt(s, pos, 4), CopyInt(s, pos + 4, 4),
    CopyInt(s, pos + 8, 4), CopyInt(s, pos + 12, 4));
end;

function ConvertSkinStr(const s: string): string;
var
  R: TRect;
  i: integer;
  sc, sm, s1, s2, s3, s4: string;
  c: char;
  FileName, sub: string;
  ImgType: TacImgType;
begin
  FileName := '';
  c := s[1];
  case c of
    CharMask:
      ImgType := itisaBorder;
    CharGlyph:
      ImgType := itisaGlyph;
    TexChar:
      ImgType := itisaTexture;

    CharExt:
      begin
        // Add condition here
        FileName := ExtractWord(1, s, [c]);
        ImgType := acImgTypes[Mini(ExtInt(4, s, [c]), Length(acImgTypes) - 1)]
        // ExtractWord(4, s, [CharExt]);
      end;

    acCharImg:
      begin
        Result := s;
        Exit;
      end

  else
    begin // Texture FileName w/o params
      c := CharExt;
      ImgType := itisaTexture;
      FileName := s;
    end;
  end;

  if Length(s) < 4 then // New image
    Result := acCharImg + '00000000' + '00000000' +
    // Size of image will be specified later
      '0000000000000000' + // Width of borders
      '3' + // States count
      '0' + // Masked
      IntToStr(ord(ImgType)) + // ImageType
      '01' + // DrawMode
      '00' + // Fill type
      '0000' + // Reserved
      FileName
  else
    case ImgType of
      itisaBorder:
        if c = CharExt then // If external file
          Result := acCharImg + '00000000' + // Left/Top
            '00000000' + // Size of image will be specified later
            ExtractWord(5, s, [c]) + ExtractWord(6, s, [c]) +
            ExtractWord(7, s, [c]) + ExtractWord(8, s, [c]) +
            ExtractWord(2, s, [c]) + // Count
            ExtractWord(3, s, [c]) + // Mask type
            IntToStr(ord(ImgType)) + // ImageType
            AddChar('0', ExtractWord(9, s, [c]), 2) + '000000' + FileName
        else
        begin
          s1 := ExtractWord(5, s, [c]);
          s2 := ExtractWord(6, s, [c]);
          s3 := ExtractWord(7, s, [c]);
          s4 := ExtractWord(8, s, [c]);
          sm := ExtractWord(10, s, [c]); // Mask type
          sc := ExtractWord(9, s, [c]); // Count
          if (Length(s) > 47) then
          begin // BorderWidth
            sub := AddChar('0', ExtractWord(12, s, [c]), 4);
            if (s1 = '0000') and (s2 = '0000') and (s3 = '0000') and
              (s4 = '0000') then
            begin
              if sub <> '0000' then
              begin
                s1 := sub;
                s2 := sub;
                s3 := sub;
                s4 := sub;
              end
              else
              begin
                R := CopyRect(s, 2);
                i := WidthOf(R) div StrToInt(sc);
                s1 := AddChar('0', IntToStr(max(0, (i - 1) div 2)), 4);
                i := HeightOf(R) div (StrToInt(sm) + 1);
                s2 := AddChar('0', IntToStr(max(0, (i - 1) div 2)), 4);
                s3 := s1;
                s4 := s2;
              end;
            end;
          end;
          Result := acCharImg + ExtractWord(1, s, [c]) + ExtractWord(2, s, [c])
            + ExtractWord(3, s, [c]) + ExtractWord(4, s, [c]) + s1 + s2 + s3 +
            s4 + sc + // Count
            sm + // Mask type
            IntToStr(ord(ImgType)) + // ImageType
            AddChar('0', ExtractWord(11, s, [c]), 2) + '000000';
        end;

      itisaGlow:
        if WordCount(s, [c]) > 4 then
          Result := acCharImg + '00000000' + // Left/Top
            '00000000' + // Size of image will be specified later
            ExtractWord(5, s, [c]) + ExtractWord(6, s, [c]) +
            ExtractWord(7, s, [c]) + ExtractWord(8, s, [c]) +
            ExtractWord(2, s, [c]) + // Count
            ExtractWord(3, s, [c]) + // Mask type
            IntToStr(ord(ImgType)) + // ImageType
            AddChar('0', ExtractWord(9, s, [c]), 2) + '000000' + FileName
        else
          Result := acCharImg + '00000000' + // Left/Top
            '00000000' + // Size of image will be specified later
            '00000000' + '00000000' + '1' + '0' + IntToStr(ord(ImgType)) +
          // ImageType
            '00' + '000000' + FileName;

      itisaTexture:
        begin
          if c = CharExt then // If external file
            if pos(CharExt, s) > 0 then
              Result := acCharImg + '0000000000000000' + '0000000000000000' +
                ExtractWord(2, s, [c]) + ExtractWord(3, s, [c]) +
                IntToStr(ord(ImgType)) + AddChar('0', ExtractWord(9, s, [c]), 2)
                + '000000' + FileName
            else
              Result := acCharImg + '0000000000000000' + '0000000000000000' +
                '1' + '0' + IntToStr(ord(ImgType)) + '00' + '000000' + FileName
          else
          begin
            if WordCount(s, [c]) > 6 then
              FileName := ExtractWord(7, s, [c]);

            sub := Copy(s, 25, 1);
            Result := acCharImg + ExtractWord(1, s, [c]) + ExtractWord(2, s, [c]
              ) + ExtractWord(3, s, [c]) + ExtractWord(4, s, [c]) +
              '0000000000000000' + '1' + // Count
              iff(sub = '', '0', sub) + // Mask type
              IntToStr(ord(ImgType)) + // ImageType
              AddChar('0', ExtractWord(5, s, [c]), 2) + // Fill type
              '00' + '0000' + FileName;
          end;
        end;

      itisaGlyph, itisaPngGlyph:
        begin
          if c = CharExt then // If external file
            if WordCount(s, [c]) > 4 then
              Result := acCharImg + ExtractWord(5, s, [c]) +
                ExtractWord(6, s, [c]) + ExtractWord(7, s, [c]) +
                ExtractWord(8, s, [c]) + '0000000000000000' +
                ExtractWord(2, s, [c]) + ExtractWord(3, s, [c]) +
                IntToStr(ord(ImgType)) + AddChar('0', ExtractWord(9, s, [c]), 2)
                + '000000' + FileName
            else
              Result := acCharImg + '0000000000000000' + '0000000000000000' +
                ExtractWord(2, s, [c]) + ExtractWord(3, s, [c]) +
                IntToStr(ord(ImgType)) + AddChar('0', ExtractWord(9, s, [c]), 2)
                + '000000' + FileName
          else
            Result := acCharImg + ExtractWord(1, s, [c]) + ExtractWord(2, s, [c]
              ) + ExtractWord(3, s, [c]) + ExtractWord(4, s, [c]) +
              '0000000000000000' + ExtractWord(5, s, [c]) +
              ExtractWord(6, s, [c]) + IntToStr(ord(ImgType)) + '00000000';
        end;
    end;
end;

procedure LoadSkinFromFile(const FileName: string;
  var Convertor: TacSkinConvertor; pwds: TStringList; SkinManager: TComponent);
var
  Stream: TStream;
begin
  if FileExists(FileName) then
  begin
    if Convertor = nil then
      Convertor := TacSkinConvertor.Create
    else
      Convertor.Clear;

    Convertor.PackedData := TMemoryStream.Create;
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Convertor.PackedData.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;

    Convertor.PackedData.Seek(0, 0);
    ExtractPackedData(Convertor, pwds, SkinManager);
    FreeAndNil(Convertor.PackedData);
  end;
end;

procedure Load3rdNames(AThirdParty: TsThirdPartyList; AThirdLists: TStringLists;
  var AOwnThirdLists: boolean);
var
  i: integer;
  s: string;
begin
  AOwnThirdLists := False;
  for i := 0 to High(acThirdNames) do
  begin
    s := AThirdParty.GetString(i);
    if s <> '' then
      AOwnThirdLists := True;

    AThirdLists[i].Text := s;
  end;
end;

function SearchSupport(const Lists: TStringLists;
  const ClassName
  : string { ; CheckEvent: boolean; var Found: boolean } ): boolean;
var
  i, j: integer;
begin
  Result := False;
  for j := 0 to Length(Lists) - 1 do
    for i := 0 to Lists[j].Count - 1 do
      if Lists[j][i] = ClassName then
      begin
        Result := True;
        Exit;
      end;
end;

procedure UpdatePreview(Handle: hwnd; Enabled: boolean);
var
  Policy: Longint;
begin
  if ac_ChangeThumbPreviews then
  begin
    Policy := integer(Enabled);
    if DwmSetWindowAttribute(Handle, 10 { DWMWA_HAS_ICONIC_BITMAP } , @Policy,
      4) = S_OK then
      DwmSetWindowAttribute(Handle, 7 { DWMWA_FORCE_ICONIC_REPRESENTATION } ,
        @Policy, 4);

    DwmInvalidateIconicBitmaps(Handle);
  end;
end;

function SysColorToSkin(const AColor: TColor;
  ASkinManager: TsSkinManager = nil): TColor;
{$IFNDEF DELPHI7UP}
const
  clHotLight = TColor($FF000000 or 26);
  clGradientActiveCaption = TColor($FF000000 or 27);
  clGradientInactiveCaption = TColor($FF000000 or 28);
  clMenuHighlight = TColor($FF000000 or 29);
  clMenuBar = TColor($FF000000 or 30);
{$ENDIF}
begin
  if ASkinManager = nil then
    ASkinManager := DefaultManager;

  if (ASkinManager <> nil) and ASkinManager.IsActive then
    case AColor of
      clScrollBar, clBackground, clBtnFace, clAppWorkSpace, clMenu:
        Result := ASkinManager.Palette[pcMainColor];

      clWindow:
        Result := ASkinManager.Palette[pcEditBG];

      clWindowText:
        Result := ASkinManager.Palette[pcEditText];

      clInactiveBorder, clActiveBorder, cl3DLight, cl3DDkShadow, clBtnShadow,
        clWindowFrame:
        Result := ASkinManager.Palette[pcBorder];

      clGradientActiveCaption, clActiveCaption:
        if ASkinManager.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssFormTitle]].Props[1].Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clCaptionText:
        if ASkinManager.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssFormTitle]].Props[1]
            .FontColor.Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clGradientInactiveCaption, clInactiveCaption:
        if ASkinManager.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssFormTitle]].Props[0].Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clInactiveCaptionText:
        if ASkinManager.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssFormTitle]].Props[0]
            .FontColor.Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clMenuBar:
        Result := ASkinManager.Palette[pcMainColor];

      clMenuText:
        Result := ASkinManager.Palette[pcLabelText];

      clHighlight:
        Result := ASkinManager.Palette[pcSelectionBG_Focused];

      clHighlightText:
        Result := ASkinManager.Palette[pcSelectionText_Focused];

      clGrayText:
        Result := BlendColors(ASkinManager.Palette[pcMainColor],
          ASkinManager.Palette[pcLabelText], 127);

      clBtnText:
        if ASkinManager.SkinCommonInfo.Sections[ssButton] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssButton]].Props[0]
            .FontColor.Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clBtnHighlight:
        if ASkinManager.SkinCommonInfo.Sections[ssButton] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssButton]].Props[1].Color
        else
          Result := ASkinManager.Palette[pcMainColor];

      clInfoText:
        Result := ASkinManager.Palette[pcHintText];

      clInfoBk:
        Result := ASkinManager.Palette[pcHintBG];

      clHotLight:
        Result := ASkinManager.Palette[pcWebText];

      clMenuHighlight:
        if ASkinManager.SkinCommonInfo.Sections[ssMenuItem] >= 0 then
          Result := ASkinManager.CommonSkinData.gd
            [ASkinManager.SkinCommonInfo.Sections[ssMenuItem]].Props[1].Color
        else
          Result := ColorToRGB(AColor)

    else
      Result := ColorToRGB(AColor);
    end
  else
    Result := ColorToRGB(AColor);
end;

{$IFDEF D2007}

procedure UpdateCommonDlgs(sManager: TsSkinManager);
begin
  if DefaultManager = sManager then
    UseLatestCommonDialogs := not(srStdDialogs in sManager.SkinningRules) or
      not sManager.Active or acAllowLatestCommonDialogs;
end;
{$ENDIF}

function GetCornerType(sm: TsSkinManager; const mask: TsMaskData): integer;
var
  c: TsColor;
  Bmp: TBitmap;
  dx, i, X, Y, w, h, l, t: integer;

  procedure Checkbits;
  begin
    Y := t;
    if Result = 0 then
      if mask.DrawMode and BDM_ALPHA = 0 then
      begin
        while Y < h do
        begin
          X := l;
          while X < w do
          begin
            c := GetAPixel(Bmp, X, Y);
            if c.i = clFuchsia then
            begin
              Result := 1;
              Exit;
            end;
            inc(X);
          end;
          inc(Y);
        end;
      end
      else
        while Y < h do
        begin
          X := l;
          while X < w do
          begin
            c := GetAPixel(Bmp, X, Y);
            if c.A = 0 then
            begin
              Result := 1;
              Exit;
            end;
            inc(X);
          end;
          inc(Y);
        end
  end;

begin
  begin
    Result := 0; // Normal corner
    if HeightOf(mask.R) > 0 then
    begin
      if mask.Bmp <> nil then
        Bmp := mask.Bmp
      else
        Bmp := sm.MasterBitmap;

      dx := mask.Width;
      for i := 0 to mask.ImageCount - 1 do
      begin
        // Left top
        l := mask.R.Left + i * dx;
        t := mask.R.Top;
        w := min(l + mask.WL, Bmp.Width - 1);
        h := min(mask.R.Top + mask.WT, Bmp.Height - 1);
        Checkbits;
        if Result <= 0 then
        begin
          // Right top
          l := mask.R.Left + i * dx + WidthOf(mask.R) div mask.ImageCount
            - mask.WR;
          t := mask.R.Top;
          w := min(l + mask.WR, Bmp.Width - 1);
          h := min(mask.R.Top + mask.WT, Bmp.Height - 1);
          Checkbits;
          if Result <= 0 then
          begin
            // Left bottom
            l := mask.R.Left + i * dx;
            t := mask.R.Top + HeightOf(mask.R) div (mask.MaskType + 1)
              - mask.WB;
            w := min(mask.R.Left + mask.WL, Bmp.Width - 1);
            h := min(t + mask.WB, Bmp.Height - 1);
            Checkbits;
            if Result <= 0 then
            begin
              // Right bottom
              l := mask.R.Left + i * dx + WidthOf(mask.R) div mask.ImageCount
                - mask.WR;
              t := mask.R.Top + HeightOf(mask.R) div (mask.MaskType + 1)
                - mask.WB;
              w := min(l + mask.WL, Bmp.Width - 1);
              h := min(t + mask.WB, Bmp.Height - 1);
              Checkbits;
            end;
          end;
        end
        else
          Break;
      end;
    end;
  end;
end;

const
  ChangeableColors: set of TacPaletteColors = [pcMainColor, pcLabelText,
    pcWebText, pcEditText, pcEditBG, pcSelectionBG, pcSelectionText,
    pcSelectionBG_Focused, pcSelectionText_Focused, pcEditBG_Inverted,
    pcEditText_Inverted, pcEditBG_OddRow, pcEditBG_EvenRow,

    pcBtnColor1Active, pcBtnColor2Active, pcBtnBorderActive, pcBtnFontActive,
    pcBtnColor1Normal, pcBtnColor2Normal, pcBtnBorderNormal, pcBtnFontNormal,
    pcBtnColor1Pressed, pcBtnColor2Pressed, pcBtnBorderPressed,
    pcBtnFontPressed,

    pcBorder, pcGrid, pcHintBG, pcHintText];

procedure ChangeSkinSaturation(sManager: TsSkinManager; Value: integer);
var
  i, l, j, n, k: integer;
  pc: TacPaletteColors;
begin
  if Value <> 0 then
  begin
    Value := LimitIt(Value * MaxByte div 100, -MaxByte, MaxByte);
    with sManager do
    begin
      ChangeBitmapPixels(MasterBitmap, ChangeColorSaturation, Value, clFuchsia);
      for k := 0 to Length(SkinDataArray) - 1 do
        if SkinDataArray[k] <> nil then
        begin
          l := Length(SkinDataArray[k].BmpArray);
          for i := 0 to l - 1 do
            if Assigned(SkinDataArray[k].BmpArray[i].Bmp) then
              ChangeBitmapPixels(SkinDataArray[k].BmpArray[i].Bmp,
                ChangeColorSaturation, Value, clFuchsia);

          l := Length(SkinDataArray[k].gd);
          for i := 0 to l - 1 do
            with SkinDataArray[k].gd[i] do
              for j := 0 to ac_MaxPropsIndex do
                with Props[j] do
                begin
                  if Color <> -1 then
                    Color := ChangeSaturation(Color, Value);
                  if GlowColor <> -1 then
                    GlowColor := ChangeSaturation(GlowColor, Value);
                  if FontColor.Color <> -1 then
                    FontColor.Color := ChangeSaturation(FontColor.Color, Value);
                  if FontColor.Left <> -1 then
                    FontColor.Left := ChangeSaturation(FontColor.Left, Value);
                  if FontColor.Top <> -1 then
                    FontColor.Top := ChangeSaturation(FontColor.Top, Value);
                  if FontColor.Right <> -1 then
                    FontColor.Right := ChangeSaturation(FontColor.Right, Value);
                  if FontColor.Bottom <> -1 then
                    FontColor.Bottom :=
                      ChangeSaturation(FontColor.Bottom, Value);
                  if LightColor <> -1 then
                    LightColor := ChangeSaturation(LightColor, Value);
                  if BorderColor <> clFuchsia then
                    BorderColor := ChangeSaturation(BorderColor, Value);
                  for n := 0 to Length(GradientArray) - 1 do
                    GradientArray[n].Color.c :=
                      ChangeSaturation(GradientArray[n].Color.c, Value);
                end;
        end;

      with sManager do
      begin
        SkinCommonInfo.Shadow1Color :=
          ChangeSaturation(SkinCommonInfo.Shadow1Color, Value);
        for pc := Low(Palette) to High(Palette) do
          if (pc in ChangeableColors) then
            Palette[pc] := ChangeSaturation(Palette[pc], Value);
      end;
    end;
  end;
end;

procedure ChangeSkinBrightness(sManager: TsSkinManager; Value: integer);
var
  i, j, n, k: integer;
  MasksBmp: TBitmap;

  procedure RestoreMasterMasks(DataIndex: integer);
  var
    i: integer;
  begin
    with sManager.SkinDataArray[DataIndex] do
      for i := 0 to Length(BmpArray) - 1 do
        with BmpArray[i] do
          if (Bmp = nil) { if image is in MasterBmp } and (MaskType = 1) then
            BitBlt(sManager.MasterBitmap.Canvas.Handle, R.Left,
              R.Top + HeightOf(R) div 2, WidthOf(R), HeightOf(R) div 2,
              MasksBmp.Canvas.Handle, R.Left, R.Top + HeightOf(R)
              div 2, SRCCOPY);
  end;

begin
  if Value <> 0 then
    with sManager do
    begin
      MasksBmp := CreateBmp32;
      CopyBmp(MasksBmp, sManager.MasterBitmap); // Save MasterBmp
      ChangeBitmapPixels(MasterBitmap, ChangeColorBrightness, Value, clFuchsia);
      for k := 0 to Length(SkinDataArray) - 1 do
        with SkinDataArray[k] do
          if SkinDataArray[k] <> nil then
          begin
            RestoreMasterMasks(k);
            for i := 0 to Length(BmpArray) - 1 do
              with BmpArray[i] do
                if Assigned(Bmp) then
                  if Bmp <> nil then
                    if (MaskType = 0) then
                    begin
                      ChangeBitmapPixels(Bmp, ChangeColorBrightness, Value,
                        clFuchsia);
                      if (Value > 0) and (pos('.PNG', FileName) > 0) then
                        UpdateAlpha(Bmp, MkRect(Bmp))
                    end
                    else
                      with MasksBmp do
                      begin
                        Width := Bmp.Width;
                        Height := Bmp.Height div 2;
                        BitBlt(Canvas.Handle, 0, 0, Width, Height,
                          Bmp.Canvas.Handle, 0, Height, SRCCOPY); // Save mask
                        ChangeBitmapPixels(Bmp, ChangeColorBrightness, Value,
                          clFuchsia);
                        BitBlt(Bmp.Canvas.Handle, 0, Height, Width, Height,
                          Canvas.Handle, 0, 0, SRCCOPY); // Restore mask
                      end;

            for i := 0 to LengthOfGD - 1 do
              with gd[i] do
                for j := 0 to ac_MaxPropsIndex do
                  with Props[j] do
                  begin
                    if Color <> -1 then
                      Color := ChangeBrightness(Color, Value);
                    if GlowColor <> -1 then
                      GlowColor := ChangeBrightness(GlowColor, Value);
                    if FontColor.Color <> -1 then
                      FontColor.Color :=
                        ChangeBrightness(FontColor.Color, Value);
                    if FontColor.Left <> -1 then
                      FontColor.Left := ChangeBrightness(FontColor.Left, Value);
                    if FontColor.Top <> -1 then
                      FontColor.Top := ChangeBrightness(FontColor.Top, Value);
                    if FontColor.Right <> -1 then
                      FontColor.Right :=
                        ChangeBrightness(FontColor.Right, Value);
                    if FontColor.Bottom <> -1 then
                      FontColor.Bottom :=
                        ChangeBrightness(FontColor.Bottom, Value);
                    if LightColor <> -1 then
                      LightColor := ChangeBrightness(LightColor, Value);
                    if BorderColor <> clFuchsia then
                      BorderColor := ChangeBrightness(BorderColor, Value);
                    for n := 0 to Length(GradientArray) - 1 do
                      with GradientArray[n] do
                        Color.c := ChangeBrightness(Color.c, Value);
                  end;
          end;

      FreeAndNil(MasksBmp);
    end;
end;

procedure ChangeSkinHue(sManager: TsSkinManager; Value: integer; aPPI: integer);
var
  ExceptNdx: array of TBitmap;
  i, j, n, X, Y, k: integer;
  pc: TacPaletteColors;
  TmpBmp: TBitmap;
  SavedDC: hdc;

  procedure SaveImage(const Name: string; DataNdx: integer);
  var
    Index: integer;

    procedure AddToArray(Bmp: TBitmap);
    begin
      SetLength(ExceptNdx, Length(ExceptNdx) + 1);
      ExceptNdx[Length(ExceptNdx) - 1] := Bmp;
    end;

  begin
    Index := sManager.GetMaskIndex(sManager.SkinCommonInfo.IndexGlobalInfo,
      Name, sManager.SkinDataArray[DataNdx]);
    if Index >= 0 then
      with sManager.SkinDataArray[DataNdx].ma[Index] do
      begin
        if Bmp = nil { If Image is in MasterBitmap } then
          with R do
            ExcludeClipRect(sManager.MasterBitmap.Canvas.Handle, Left, Top,
              Right, Bottom)
        else
          AddToArray(Bmp);
        // Glow glyphs
        if (Bmp <> nil) and
          (sManager.GetMaskIndex(sManager.SkinCommonInfo.IndexGlobalInfo,
          Name + s_Glow + ZeroChar, sManager.SkinDataArray[DataNdx]) >= 0) then
          AddToArray(Bmp);
      end;
  end;

begin
  if Value <> 0 then
  begin
    SetLength(ExceptNdx, 0);
    with sManager do
    begin
      for k := 0 to Length(SkinDataArray) - 1 do
        if (SkinDataArray[k] <> nil) and (SkinDataArray[k].PPI = aPPI) then
          with SkinDataArray[k] do
          begin
            // Save images of border icons if needed
            if SkinCommonInfo.BIKeepHUE > 0 then
            begin
              TmpBmp := CreateBmp32(MasterBitmap.Width, MasterBitmap.Height);
              TmpBmp.Assign(MasterBitmap);
              if PPI = aPPI then
                ChangeBitmapPixels(TmpBmp, ChangeColorHUE, Value, clFuchsia);

              SavedDC := SaveDC(MasterBitmap.Canvas.Handle);
              try
                SaveImage(s_BorderIconClose, k);
                SaveImage(s_BorderIconCloseAlone, k);
                SaveImage(s_SmallIconClose, k);
                if SkinCommonInfo.BIKeepHUE = 1 then
                begin
                  SaveImage(s_BorderIconMaximize, k);
                  SaveImage(s_BorderIconMinimize, k);
                  SaveImage(s_BorderIconNormalize, k);
                  SaveImage(s_BorderIconHelp, k);
                  SaveImage(s_SmallIconMaximize, k);
                  SaveImage(s_SmallIconMinimize, k);
                  SaveImage(s_SmallIconNormalize, k);
                end;
                BitBlt(MasterBitmap.Canvas.Handle, 0, 0, TmpBmp.Width,
                  TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                FreeAndNil(TmpBmp);
                RestoreDC(MasterBitmap.Canvas.Handle, SavedDC);
              end;
            end
            else if PPI = aPPI then
              ChangeBitmapPixels(MasterBitmap, ChangeColorHUE, Value,
                clFuchsia);

            for i := 0 to Length(BmpArray) - 1 do
              if Assigned(BmpArray[i].Bmp) then
              begin
                if SkinCommonInfo.BIKeepHUE > 0 then
                begin
                  // Get index in ma
                  Y := -1;
                  for n := 0 to Length(ma) - 1 do
                    if ma[n].Bmp = BmpArray[i].Bmp then
                    begin
                      Y := n;
                      Break
                    end;
                  // Check if excepted
                  X := 0;
                  if Y >= 0 then
                    for n := 0 to Length(ExceptNdx) - 1 do
                      if ExceptNdx[n] = ma[Y].Bmp then
                      begin
                        X := 1;
                        Break
                      end;

                  if X = 1 then
                    Continue;
                end;
                if (PPI = aPPI) or sManager.SkinDataArray[k].BmpArray[i].Ext
                then
                  ChangeBitmapPixels(BmpArray[i].Bmp, ChangeColorHUE, Value,
                    clFuchsia);
              end;

            if PPI = aPPI then
              for i := 0 to Length(gd) - 1 do
                with gd[i] do
                  for j := 0 to ac_MaxPropsIndex do
                    with Props[j] do
                    begin
                      for n := 0 to Length(GradientArray) - 1 do
                        with GradientArray[n] do
                          Color.c := ChangeHue(Value, Color.c);

                      if Color <> -1 then
                        Color := ChangeHue(Value, Color);
                      if GlowColor <> -1 then
                        GlowColor := ChangeHue(Value, GlowColor);
                      if FontColor.Color <> -1 then
                        FontColor.Color := ChangeHue(Value, FontColor.Color);
                      if FontColor.Left <> -1 then
                        FontColor.Left := ChangeHue(Value, FontColor.Left);
                      if FontColor.Top <> -1 then
                        FontColor.Top := ChangeHue(Value, FontColor.Top);
                      if FontColor.Right <> -1 then
                        FontColor.Right := ChangeHue(Value, FontColor.Right);
                      if FontColor.Bottom <> -1 then
                        FontColor.Bottom := ChangeHue(Value, FontColor.Bottom);
                      if LightColor <> -1 then
                        LightColor := ChangeHue(Value, LightColor);
                      if BorderColor <> clFuchsia then
                        BorderColor := ChangeHue(Value, BorderColor);
                    end;
          end;

      SkinCommonInfo.Shadow1Color :=
        ChangeHue(Value, SkinCommonInfo.Shadow1Color);
      with sManager do
        for pc := Low(Palette) to High(Palette) do
          if pc in ChangeableColors then
            Palette[pc] := ChangeHue(Value, Palette[pc]);
    end;
    SetLength(ExceptNdx, 0);
  end;
end;

type
  TJumpCode = packed record
    Jump: byte;
    Offset: integer;
  end;

  TWin9xDebugThunk = packed record
    PUSH: byte;
    Addr: Pointer;
    JMP: TJumpCode;
  end;

{$IFNDEF DELPHI6UP}

  PPointer = ^Pointer;
{$ENDIF}
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;

  TAbsoluteIndirectJmp = packed record
    OpCode: word; // $FF25(Jmp, FF /4)
    Addr: PPointer;
  end;

var
  WinCreatePen, WinGetSysColor, WinGetSysColorBrush: TAbsoluteIndirectJmp;

function acGetActualAddr(Proc: Pointer): Pointer;
var
  j: PAbsoluteIndirectJmp;
begin
  j := PAbsoluteIndirectJmp(Proc);
  if j.OpCode = $25FF then
{$IFDEF WIN64}
    Result := PPointer(Int32(Proc) + Int32(j.Addr) + 6 { Instruction Size } )^
    // Result := PPointer(acNativeUInt(Proc) + acNativeUInt(J.Addr) + 6 {Instruction Size})^
{$ELSE}
    Result := j.Addr^
{$ENDIF}
  else
    Result := Proc;
end;

function acHookProc(Proc, Dest: Pointer;
  var BackCode: TAbsoluteIndirectJmp): boolean;
var
  n: ACUInt;
  Code: TJumpCode;
begin
  Proc := acGetActualAddr(Proc);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackCode, SizeOf(BackCode), n)
  then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    Result := WriteProcessMemory(GetCurrentProcess, Proc, @Code,
      SizeOf(Code), n);
  end
  else
    Result := False;
end;

function acUnhookProc(Proc: Pointer; BackCode: TAbsoluteIndirectJmp): boolean;
var
  n: ACUInt;
begin
  Proc := acGetActualAddr(Proc);
  if BackCode.OpCode <> 0 then
  begin
    Result := WriteProcessMemory(GetCurrentProcess, Proc, @BackCode,
      SizeOf(BackCode), n);
    BackCode.OpCode := 0;
  end
  else
    Result := False;
end;

procedure HookCreatePen; forward;
procedure UnHookCreatePen; forward;
procedure HookGetSysColor; forward;
procedure UnHookGetSysColor; forward;
procedure HookGetSysColorBrush; forward;
procedure UnHookGetSysColorBrush; forward;

procedure UpdateColorsHooks(Active: boolean);
begin
{$IFDEF WIN64}
  if IsWin8Up then // Not supported under Win 10 x64 yet
    Exit;
{$ENDIF}
  if Active then
  begin
    HookGetSysColor;
    HookGetSysColorBrush;
    HookCreatePen;
  end
  else
  begin
    UnHookGetSysColor;
    UnHookGetSysColorBrush;
    UnHookCreatePen;
  end;
end;

function acCreatePen(Style, Width: integer; Color: COLORREF): HPEN; stdcall;
begin
  if DefaultManager <> nil then
    with DefaultManager do
      case Color of
        COLOR_INACTIVEBORDER:
          Color := SkinCommonInfo.SysInactiveBorderColor;
        COLOR_HIGHLIGHT:
          Color := Palette[pcSelectionBG_Focused];
        COLOR_HIGHLIGHTTEXT:
          Color := Palette[pcSelectionText_Focused];
        COLOR_WINDOW:
          Color := Palette[pcEditBG];
        COLOR_BTNFACE:
          Color := Palette[pcMainColor];
        COLOR_WINDOWFRAME:
          Color := Palette[pcBorder];
        COLOR_BTNSHADOW:
          Color := BlendColors(Palette[pcMainColor], 0, 205);
        COLOR_3DDKSHADOW:
          Color := BlendColors(Palette[pcMainColor], 0, 205);
        COLOR_3DLIGHT:
          Color := BlendColors(Palette[pcMainColor], $FFFFFF, 205);
        clSilver:
          Color := BlendColors(Palette[pcEditText], Palette[pcEditBG], 38);
        $F0F0F0:
          Color := BlendColors(Palette[pcEditText], Palette[pcEditBG], 38);
      end;

  UnHookCreatePen;
  Result := CreatePen(Style, Width, Color);
  HookCreatePen;
end;

function acGetSysColor(nIndex: integer): DWORD; stdcall;
begin
  if (DefaultManager <> nil) and ((Screen.ActiveForm <> nil) and
    Screen.ActiveForm.HandleAllocated and
    IsWindowVisible(Screen.ActiveForm.Handle)) then
    with DefaultManager do
      case nIndex of
        COLOR_INACTIVEBORDER:
          Result := SkinCommonInfo.SysInactiveBorderColor;
        COLOR_HIGHLIGHT:
          Result := Palette[pcSelectionBG_Focused];
        COLOR_HIGHLIGHTTEXT:
          Result := Palette[pcSelectionText_Focused];
        COLOR_WINDOW:
          Result := Palette[pcEditBG];
        COLOR_WINDOWTEXT:
          Result := Palette[pcEditText];
        COLOR_WINDOWFRAME:
          Result := Palette[pcBorder];
        COLOR_BTNSHADOW:
          Result := BlendColors(Palette[pcMainColor], 0, 205);
        COLOR_3DDKSHADOW:
          Result := BlendColors(Palette[pcMainColor], 0, 205);
        COLOR_3DLIGHT:
          Result := BlendColors(Palette[pcMainColor], $FFFFFF, 205)
      else
        begin
          UnHookGetSysColor;
          Result := GetSysColor(nIndex);
          HookGetSysColor;
        end;
      end
  else
  begin
    UnHookGetSysColor;
    Result := GetSysColor(nIndex);
    HookGetSysColor;
  end;
end;

function acGetSysColorBrush(nIndex: integer): HBRUSH; stdcall;
begin
  if DefaultManager <> nil then
    with DefaultManager do
      case nIndex of
        COLOR_HIGHLIGHT:
          Result := CreateSolidBrush(Cardinal(Palette[pcSelectionBG_Focused]));
        COLOR_HIGHLIGHTTEXT:
          Result := CreateSolidBrush
            (Cardinal(Palette[pcSelectionText_Focused]));
        COLOR_WINDOW:
          if DefaultManager <> nil then
            Result := DefaultManager.Brushes[pcEditBG]
          else
            Result := CreateSolidBrush(Cardinal(Palette[pcEditBG]));

        COLOR_WINDOWTEXT:
          if DefaultManager <> nil then
            Result := DefaultManager.Brushes[pcEditText]
          else
            Result := CreateSolidBrush(Cardinal(Palette[pcEditText]));

      else
        begin
          UnHookGetSysColorBrush;
          Result := GetSysColorBrush(nIndex);
          HookGetSysColorBrush;
        end;
      end
  else
    Result := 0;
end;

var
  SysColorsHooked: boolean = False;
  SysCreatePenHooked: boolean = False;
  SysColorsHookedBrush: boolean = False;

procedure HookGetSysColor;
begin
  if not SysColorsHooked then
  begin
    SysColorsHooked := True;
    acHookProc(@Windows.GetSysColor, @acGetSysColor, WinGetSysColor);
  end;
end;

procedure UnHookGetSysColor;
begin
  if SysColorsHooked then
  begin
    SysColorsHooked := False;
    acUnhookProc(@Windows.GetSysColor, WinGetSysColor);
  end;
end;

procedure HookGetSysColorBrush;
begin
  if not SysColorsHookedBrush then
  begin
    SysColorsHookedBrush := True;
    acHookProc(@Windows.GetSysColorBrush, @acGetSysColorBrush,
      WinGetSysColorBrush);
  end;
end;

procedure UnHookGetSysColorBrush;
begin
  if SysColorsHookedBrush then
  begin
    SysColorsHookedBrush := False;
    acUnhookProc(@Windows.GetSysColorBrush, WinGetSysColorBrush);
  end;
end;

procedure HookCreatePen;
begin
{$IFNDEF WIN64}
  if not SysCreatePenHooked then
  begin
    SysCreatePenHooked := True;
    acHookProc(@Windows.CreatePen, @acCreatePen, WinCreatePen);
  end;
{$ENDIF} // WIN64
end;

procedure UnHookCreatePen;
begin
  if SysCreatePenHooked then
  begin
    SysCreatePenHooked := False;
    acUnhookProc(@Windows.CreatePen, WinCreatePen);
  end;
end;

procedure LoadThirdNames(sm: TsSkinManager; Overwrite: boolean = False);
var
  i: integer;
begin
  for i := 0 to High(acThirdNames) do
  begin
    if Overwrite or (sm.ThirdParty.GetString(i) = '') then
      sm.ThirdParty.SetString(i, acThirdNames[i]);

    sm.ThirdLists[i].Text := sm.ThirdParty.GetString(i);
  end;
end;

procedure UpdateThirdNames(sm: TsSkinManager);
var
  i: integer;
begin
  for i := 0 to High(acThirdNames) do
    sm.ThirdParty.SetString(i, sm.ThirdLists[i].Text);
end;

function CurrentPPI(sm: TsSkinManager; OwnerCtrl: TControl;
  CheckActive: boolean = False): integer;
begin
  if sm = nil then
    sm := DefaultManager;

  if (sm <> nil) and (not CheckActive or sm.IsActive) then
    if csDesigning in sm.ComponentState then
      Result := 96
    else
      case sm.Options.ScaleMode of
        smVCL:
          if OwnerCtrl <> nil then
            Result := GetControlPPI(OwnerCtrl)
          else
            Result := DefaultPPI;

        smCustomPPI:
          Result := sm.Options.PixelsPerInch
      else
        Result := aPPIArray[sm.GetScale];
      end
  else if OwnerCtrl <> nil then
    Result := GetControlPPI(OwnerCtrl)
  else
    Result := DefaultPPI;
end;

function GetScrollSize(sm: TsSkinManager; ScalePPI: integer = 96): integer;
begin
  if sm = nil then
    Result := GetSystemMetrics(SM_CXVSCROLL)
  else if sm.ScrollsOptions.ScrollSize < 0 then
  begin // Default value
    if sm.CommonSkinData(ScalePPI).ScrollsWidth >= 0 then
      Result := sm.CommonSkinData(ScalePPI).ScrollsWidth
    else
    begin
      Result := GetSystemMetrics(SM_CXVSCROLL);
      case sm.Options.ScaleMode of
        smVCL:
          Result := MulDiv(Result, ScalePPI, Screen.PixelsPerInch);
        smAuto, smOldMode:
          ;
        smCustomPPI:
          Result := MulDiv(Result, sm.Options.PixelsPerInch,
            aPPIArray[sm.SysFontScale]);
      else
        Result := MulDiv(Result, aScalePercents[sm.GetScale],
          aScalePercents[sm.SysFontScale]);
      end;
    end
  end
  else
    Result := MulDiv(sm.ScrollsOptions.ScrollSize, ScalePPI, 96);
end;

function GetComboBtnSize(sm: TsSkinManager; ScalePPI: integer): integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if (sm <> nil) and not(csDesigning in sm.ComponentState) then
    case sm.Options.ScaleMode of
      smVCL:
        Result := MulDiv(Result, ScalePPI, Screen.PixelsPerInch);
      smAuto, smOldMode:
        ;
      smCustomPPI:
        Result := MulDiv(Result, sm.Options.PixelsPerInch,
          aPPIArray[sm.SysFontScale]);
    else
      Result := MulDiv(Result, aScalePercents[sm.GetScale],
        aScalePercents[sm.SysFontScale]);
    end;
end;

procedure CopyProperties(var Dst, Src: TsGenState);
var
  i: integer;
begin
  with Src do
  begin
    Dst.Color := Color;
    Dst.FontColor := FontColor;
    Dst.ImagePercent := ImagePercent;
    Dst.TextureIndex := TextureIndex;
    Dst.GlowSize := GlowSize;
    Dst.Transparency := Transparency;
    Dst.GlowColor := GlowColor;
    Dst.LightColor := LightColor;
    Dst.BorderColor := BorderColor;
    Dst.GradientPercent := GradientPercent;
    SetLength(Dst.GradientArray, Length(GradientArray));
    for i := 0 to Length(GradientArray) - 1 do
      Dst.GradientArray[i] := GradientArray[i];
  end;
end;

procedure TsSkinManager.AfterConstruction;
begin
  inherited;
  LoadThirdNames(Self);
  if FSkinDirectory = '' then
    FSkinDirectory := DefSkinsDir;

  if ([csLoading, csReading, csDesigning] * ComponentState = []) and
    Assigned(InitDevEx) then
  begin
    InitDevEx(Active and (SkinName <> ''));
  end;
  SkinListController.UpdateIfNeeded;
end;

procedure SetCaseSens(sl: TStringList; Value: boolean = True); overload;
begin
{$IFDEF DELPHI6UP} sl.CaseSensitive := Value; {$ENDIF}
end;

procedure SetCaseSens(sl: TMemIniFile; Value: boolean = True); overload;
begin
{$IFDEF DELPHI6UP} sl.CaseSensitive := Value; {$ENDIF}
end;

type
  TAccessMenus = class(TsSkinableMenus);

constructor TsSkinManager.Create(AOwner: TComponent);
var
  i, l: integer;
begin
  inherited Create(AOwner);
  SkinFile := nil;
  Options := nil;
  ClearSkinData;
  EmbeddedFonts := acFontStore.TacEmbeddedFonts.Create(nil);
  FEffects := TacSkinEffects.Create;
  FFonts := TacFonts.Create(Self);
  FEffects.Manager := Self;
  FThirdParty := TsThirdPartyList.Create;
  FExtendedBorders := False;
  NoAutoUpdate := False;
  ManagerState := [];
  ShowState := saIgnore;

  SysFontScale := PPIToIndex(DefaultPPI);

  l := High(acThirdNames);
  SetLength(ThirdLists, l + 1);
  for i := 0 to l do
  begin
    ThirdLists[i] := TStringList.Create;
    SetCaseSens(ThirdLists[i]);
  end;
  SkinCommonInfo.SysInactiveBorderColor := ColorToRGB(clInactiveBorder);
  SkinCommonInfo.OptionsDat := TMemIniFile.Create('');
  SetCaseSens(SkinCommonInfo.OptionsDat);

  FUpdatedSectionNdx := 0;
  FBuiltInSkins := TsStoredSkins.Create(Self);
  FCommonSections := TStringList.Create;
  FKeyList := TStringList.Create;
  SetCaseSens(FCommonSections);
  FSkinnedPopups := True;
  FHueOffset := 0;
  FBrightness := 0;
  FExtBordersMode := ebmFull;

  FMenuSupport := TacMenuSupport.Create;
  FOptions := TacOptions.Create(Self);
  FScrollsOptions := TacScrollBarsSupport.Create(Self);
  FButtonsSupport := TacButtonsSupport.Create(Self);
  FLabelsSupport := TacLabelsSupport.Create;
  FAnimEffects := TacAnimEffects.Create(Self);
  GlobalHookInstalled := False;
  FSkinningRules := [srStdForms, srStdDialogs, srThirdParty];
  FSkinsFilter := [sfiInternal, sfiExternal];
  SkinListController := TacSkinListController.Create(Self);
  if DefaultManager = nil then
  begin
    DefaultManager := Self;
{$IFNDEF FPC}
    if IsNT and not(csDesigning in ComponentState) then
      Application.HookMainWindow(MainWindowHook);
{$ENDIF}
  end;
  FActive := True;
{$IFNDEF FPC}
  FSkinableMenus := TsSkinableMenus.Create(Self);
{$ENDIF}
{$IFNDEF D2005}
  TimerCheckHot := TTimer.Create(Self);
  TimerCheckHot.Interval := 100;
  TimerCheckHot.OnTimer := OnCheckHot;
{$ENDIF}
end;

destructor TsSkinManager.Destroy;
var
  ColorItem: TacPaletteColors;
  i: integer;
begin
  if IsActive and Assigned(FOnDeactivate) then
    FOnDeactivate(Self);

  ClearSkinData;
  FExtendedBorders := False;
  FreeAndNil(FAnimEffects);
  FreeAndNil(FBuiltInSkins);
  EmbeddedFonts.Free;
  FreeAndNil(SkinCommonInfo.OptionsDat);
{$IFNDEF FPC}
  FreeAndNil(FSkinableMenus);
{$ENDIF}
{$IFNDEF D2005}
  TimerCheckHot.Free;
{$ENDIF}
  FreeAndNil(FEffects);
  FreeAndNil(FCommonSections);
  FreeAndNil(FKeyList);
  ClearSkinArrays;
  FreeAndNil(FMenuSupport);

  if DefaultManager = Self then
  begin
{$IFNDEF FPC}
    if IsNT and not(csDesigning in ComponentState) then
      Application.UnHookMainWindow(MainWindowHook);
{$ENDIF}
    UnInstallHook; // Be sure hook is uninstalled
    DefaultManager := nil;
  end;

  FreeAndNil(FOptions);
  FreeAndNil(FScrollsOptions);
  FreeAndNil(FButtonsSupport);
  FreeAndNil(FLabelsSupport);
  FreeAndNil(SkinListController);
  FreeAndNil(FFonts);
  FreeAndNil(MasterBitmap);
  UpdateThirdNames(Self);
  for i := 0 to Length(ThirdLists) - 1 do
    if ThirdLists[i] <> nil then
      FreeAndNil(ThirdLists[i]);

  for ColorItem := low(Brushes) to high(Brushes) do
    if Brushes[ColorItem] <> 0 then
      DeleteObject(Brushes[ColorItem]);

  SetLength(ThirdLists, 0);
  FreeAndNil(FThirdParty);
  FreeAndNil(sc);

  inherited Destroy;
end;

procedure TsSkinManager.ExtractByIndex(Index: integer; const DestDir: string);
var
  DirName: string;
begin
  DirName := NormalDir(DestDir);
  if not acDirExists(DirName) then
    if not CreateDir(DirName) then
    begin
{$IFNDEF ALITE}
      ShowError(DirName + ' directory creation error.');
{$ENDIF}
      Exit;
    end;

  if InternalSkins[Index].PackedData <> nil then
    InternalSkins[Index].PackedData.SaveToFile(DirName + InternalSkins[Index]
      .Name + ' extracted.asz');
end;

procedure TsSkinManager.ExtractInternalSkin(const NameOfSkin, DestDir: string);
var
  i: integer;
  Executed: boolean;
begin
  Executed := False;
  for i := 0 to InternalSkins.Count - 1 do
    if InternalSkins[i].Name = NameOfSkin then
    begin
      if acDirExists(DestDir) then
        ExtractByIndex(i, DestDir)
      else
{$IFNDEF ALITE}ShowError{$ELSE}ShowMessage{$ENDIF}('Directory does not exists.');

      Executed := True;
    end;

  if not Executed then
{$IFNDEF ALITE}ShowError{$ELSE}ShowMessage{$ENDIF}('Skin does not exists.');
end;

procedure TsSkinManager.FlipImages(RTL: boolean);
var
  i, ndx: integer;
  tg: TacTitleGlyph;

  procedure FlipImage(md: TsMaskData);
  var
    c: TsColor;
    Bmp: TBitmap;
    S0, s: PRGBAArray;
    DeltaS, X, Y, i, w, l, R, wd2, dx: integer;
  begin
    if md.Bmp = nil then
      Bmp := MasterBitmap
    else
      Bmp := md.Bmp;

    if InitLine(Bmp, Pointer(S0), DeltaS) then
    begin
      w := WidthOf(md.R) div md.ImageCount;
      wd2 := w div 2;
      for Y := md.R.Top to md.R.Bottom - 1 do
      begin
        s := Pointer(PAnsiChar(S0) + DeltaS * Y);
        for i := 0 to md.ImageCount - 1 do
        begin
          R := md.R.Left + w * i + w - 1;
          l := md.R.Left + w * i;
          for X := l to l + wd2 - 1 do
          begin
            c := s[X];
            dx := R - (X - l);
            s[X] := s[dx];
            s[dx] := c;
          end;
        end;
      end;
    end;
  end;

begin
  if ButtonsOptions.AllowRTLSysButtons then
    if RTL and not(msRTLFlipped in ManagerState) or not RTL and
      (msRTLFlipped in ManagerState) then
    begin
      if msRTLFlipped in ManagerState then
        ManagerState := ManagerState - [msRTLFlipped]
      else
        ManagerState := ManagerState + [msRTLFlipped];

      for i := 0 to Length(SkinDataArray) - 1 do
        with SkinDataArray[i] do
          for tg := tgClose to tgNormal do
            if SkinDataArray[i] <> nil then
            begin
              ndx := SkinDataArray[i].TitleGlyphs[tg];
              if ndx >= 0 then
                FlipImage(ma[ndx]);

              if Length(TitleGlows[tg]) > 0 then
              begin
                ndx := TitleGlows[tg][0];
                if ndx >= 0 then
                  FlipImage(ma[ndx]);
              end;
            end;
    end;
end;

function TsSkinManager.gd: TsGeneralDataArray;
begin
  Result := CommonSkinData.gd;
end;

function TsSkinManager.GetExternalSkinNames(sl: TacStrings;
  CreateDataObject: boolean = False; SkinType: TacSkinTypes = stAllSkins)
  : acString;
var
  stl: TacStringList;
  i: integer;

  procedure AddItem(const sName: string; sImgIndex: integer);
  var
    iData: TacItemDrawDataObj;
  begin
    if CreateDataObject then
    begin
      iData := TacItemDrawDataObj.Create;
      iData.ImageIndex := sImgIndex;
      stl.AddObject(sName, iData);
    end
    else
      stl.Add(sName);
  end;

begin
  Result := '';
  sl.Clear;
{$IFDEF TNTUNICODE}
  if sl is TTntStringList then
    stl := TTntStringList(sl)
{$ELSE}
  if sl is TStringList then
    stl := TStringList(sl)
{$ENDIF}
  else
    stl := TacStringList.Create;

  if (Self <> DefaultManager) and (DefaultManager <> nil) and
    (CompareText(Self.SkinDirectory, DefaultManager.SkinDirectory) = 0) then
  begin
    // Fast loading from existing SkinList
    TsAlphaImageList(SkinListController.ImgList).Clear;
    TsAlphaImageList(SkinListController.ImgList).Handle;
    DefaultManager.SkinListController.CheckThread;
    DefaultManager.SkinListController.UpdateIfNeeded(True { UpdateNow } );
    SetLength(SkinListController.SkinList,
      Length(DefaultManager.SkinListController.SkinList));
    for i := 0 to Length(DefaultManager.SkinListController.SkinList) - 1 do
    begin
      SkinListController.SkinList[i].skName :=
        DefaultManager.SkinListController.SkinList[i].skName;
      SkinListController.SkinList[i].skSkinMode :=
        DefaultManager.SkinListController.SkinList[i].skSkinMode;
      SkinListController.SkinList[i].skImageIndex :=
        DefaultManager.SkinListController.SkinList[i].skImageIndex;
    end;
    for i := 0 to DefaultManager.SkinListController.ImgList.Count - 1 do
      with TsImgListItem(TsAlphaImageList(SkinListController.ImgList)
        .Items.Add) do
      begin
        ImgData.LoadFromStream
          (TsAlphaImageList(DefaultManager.SkinListController.ImgList)
          .Items[i].ImgData);
        ImageFormat := ifPNG;
      end;
{$IFNDEF DELPHI7UP}
    TsAlphaImageList(SkinListController.ImgList).GenerateStdList;
{$ENDIF}
    SkinListController.UpdateNeeded := False;
  end
  else
  begin
    SkinListController.CheckThread;
    SkinListController.UpdateIfNeeded(True);
  end;
  for i := 0 to Length(SkinListController.SkinList) - 1 do
    with SkinListController.SkinList[i] do
      if skSkinMode <> smInternal then
        AddItem(skName, skImageIndex);

  if sl.Count > 0 then
    Result := sl[0]
  else
    Result := '';

  stl.Sort;
{$IFDEF TNTUNICODE}
  if not(sl is TTntStringList) then
  begin
{$ELSE}
  if not(sl is TStringList) then
  begin
{$ENDIF}
    sl.Assign(stl);
    FreeAndNil(stl);
  end;
end;

function TsSkinManager.GetFullSkinDirectory: string;
var
  s: string;
begin
  Result := SkinDirectory;
  if pos('..', Result) = 1 then
  begin
    s := GetAppPath;
    Delete(s, Length(s), 1);
    while (s[Length(s)] <> '/') and (s[Length(s)] <> s_Slash) do
      Delete(s, Length(s), 1);

    Delete(Result, 1, 3);
    Result := s + Result;
  end
  else if (pos('.\', Result) = 1) or (pos('./', Result) = 1) then
  begin
    Delete(Result, 1, 2);
    Result := GetAppPath + Result;
  end
  else if s_Dot = Result then
    Result := GetAppPath
  else if (pos(':', Result) <= 0) and (pos('\\', Result) <= 0) then
    Result := GetAppPath + Result;

  Result := NormalDir(Result);
end;

function TsSkinManager.GetGlobalColor: TColor;
begin
  Result := Palette[pcMainColor];
end;

function TsSkinManager.GetGlobalFontColor: TColor;
begin
  Result := Palette[pcLabelText];
end;

function TsSkinManager.GetSkinNames(sl: TacStrings;
  CreateDataObject: boolean = False; SkinType: TacSkinTypes = stAllSkins)
  : acString;
var
  stl: TacStringList;
  i: integer;

  procedure AddItem(const sName: string; sImgIndex: integer);
  var
    iData: TacItemDrawDataObj;
  begin
    if CreateDataObject then
    begin
      iData := TacItemDrawDataObj.Create;
      iData.ImageIndex := sImgIndex;
      stl.AddObject(sName, iData);
    end
    else
      stl.Add(sName);
  end;

begin
  Result := '';
  sl.Clear;
{$IFDEF TNTUNICODE}
  if sl is TTntStringList then
    stl := TTntStringList(sl)
{$ELSE}
  if sl is TStringList then
    stl := TStringList(sl)
{$ENDIF}
  else
    stl := TacStringList.Create;

  SkinListController.CheckThread;
  SkinListController.UpdateIfNeeded(True);
  for i := 0 to Length(SkinListController.SkinList) - 1 do
    with SkinListController.SkinList[i] do
    begin
      case SkinType of
        stUnpacked:
          if skSkinMode <> smInternal then
            AddItem(skName, skImageIndex);
        stPacked:
          if skSkinMode = smInternal then
            AddItem(skName, skImageIndex);
        stAllSkins:
          AddItem(skName, skImageIndex);
      end;
    end;

  if sl.Count > 0 then
    Result := sl[0]
  else
    Result := '';

  stl.Sort;
{$IFDEF TNTUNICODE}
  if not(sl is TTntStringList) then
  begin
{$ELSE}
  if not(sl is TStringList) then
  begin
{$ENDIF}
    sl.Assign(stl);
    FreeAndNil(stl);
  end;
end;

procedure TsSkinManager.GetSkinSections(sl: TStrings;
  SkinData: TacSkinData = nil);
var
  i: integer;
begin
  sl.Clear;
  if IsActive then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
      for i := Low(gd) to High(gd) do
        sl.Add(gd[i].ClassName);
  end;
end;

function TsSkinManager.GetSkinInfo: TacSkinInfo;
var
  s: char;
begin
  if IsActive then
  begin
    s := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;
{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := s_Dot;
    Result := FloatToStr(SkinCommonInfo.Version);
{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := s;
  end
  else
    Result := s_NA;
end;

function TsSkinManager.GetVersion: string;
begin
  Result := acCurrentVersion {$IFDEF RUNIDEONLY} + ' Trial'{$ENDIF};
end;

procedure PaintMainLayer(Bmp: TBitmap; R: TRect;
  ColorL, ColorT, ColorR, ColorB: TColor);
var
  h, w: integer;
  par: array [0 .. 2] of TPoint;
begin
  // Paint
  if ColorT = clFuchsia then // Shadow
    FillRect32(Bmp, R, ColorL and $FFFFFF, MaxByte)
  else
  begin // Low
    h := HeightOf(R);
    w := WidthOf(R);
    // Bottom bevel color
    FillDC(Bmp.Canvas.Handle, MkRect(0, h div 2, w, h), ColorB);
    // Top bevel color
    FillDC(Bmp.Canvas.Handle, MkRect(w, h div 2), ColorT);
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Pen.Style := psSolid;
    // Left bevel color
    Bmp.Canvas.Brush.Color := ColorL;
    Bmp.Canvas.Pen.Color := ColorL;
    par[0] := Point(0, 1);
    par[1] := Point(w div 2 - 1, h div 2);
    par[2] := Point(0, h - 2);
    Bmp.Canvas.Polygon(par);
    // Right bevel color
    Bmp.Canvas.Brush.Color := ColorR;
    Bmp.Canvas.Pen.Color := ColorR;
    par[0] := Point(w - 1, 1);
    par[1] := Point(w div 2 + 1, h div 2);
    par[2] := Point(w - 1, h - 2);
    Bmp.Canvas.Polygon(par);
  end;
end;

function TsSkinManager.LengthOfGD: integer;
begin
  if (CommonSkinData <> nil) then
    Result := Length(CommonSkinData.gd)
  else if (Length(SkinDataArray) > 0) and (SkinDataArray[0] <> nil) then
    Result := Length(SkinDataArray[0].gd)
  else
    Result := 0;
end;

procedure InvertAlpha(var Prop: TsColor_; const Param: integer);
begin
  if Prop.i = Param then
    Prop.A := 0
end;

procedure TsSkinManager.LoadCommonData;
var
  sf, s: string;
  i: integer;
  Png: TPNGGraphic;
begin
  FreeAndNil(MasterBitmap);
  // Reading of the MasterBitmap if exists
  sf := SkinFile.ReadString(s_GlobalInfo, s_MasterBitmap, '');
  MasterBitmap := CreateBmp32;
  if SkinIsPacked then
  begin
    for i := 0 to sc.FileCount - 1 do
      if UpperCase(sc.Files[i].Name) = sf then
      begin
        sc.Files[i].Stream.Seek(0, 0);
        if pos('.PNG', UpperCase(sf)) > 0 then
        begin
          Png := TPNGGraphic.Create;
          try
            Png.LoadFromStream(sc.Files[i].Stream);
            MasterBitmap.Assign(Png);
            MasterBitmap.IgnorePalette := True;
            if MasterBitmap.PixelFormat = pf32bit then // AlphaChannel used
              ChangeBitmapPixels(MasterBitmap, InvertAlpha,
                integer(uint(Graphics.clFuchsia) or $FF000000), Graphics.clNone)
            else
            begin // No transparency, Alpha = 0
              MasterBitmap.PixelFormat := pf32bit;
              FillAlphaRect(MasterBitmap, MkRect(MasterBitmap), MaxByte,
                clFuchsia);
            end;
          finally
            Png.Free;
          end;
        end
        else
          MasterBitmap.LoadFromStream(sc.Files[i].Stream);

        MasterBitmap.Canvas.Handle;
        MasterBitmap.Canvas.Lock;
        Break;
      end
  end
  else
  begin
    if pos(':', sf) <= 0 then
      s := SkinCommonInfo.SkinPath + sf;

    if (s <> '') and FileExists(s) then
      if pos('.PNG', UpperCase(sf)) > 0 then
      begin
        Png := TPNGGraphic.Create;
        try
          Png.LoadFromFile(s);
          MasterBitmap.Assign(Png);
          MasterBitmap.IgnorePalette := True;
          if MasterBitmap.PixelFormat = pf32bit then // AlphaChannel used
            ChangeBitmapPixels(MasterBitmap, InvertAlpha,
              integer(uint(Graphics.clFuchsia) or $FF000000), Graphics.clNone)
          else
          begin // No transparency
            MasterBitmap.PixelFormat := pf32bit;
            FillAlphaRect(MasterBitmap, MkRect(MasterBitmap), MaxByte,
              clFuchsia);
          end;
        finally
          Png.Free;
        end;
      end
      else
        MasterBitmap.LoadFromFile(s);
  end;
  if MasterBitmap.PixelFormat <> pf32bit then
  begin
    MasterBitmap.PixelFormat := pf32bit;
    FillAlphaRect(MasterBitmap, MkRect(MasterBitmap), MaxByte, clFuchsia);
  end;
  MasterBitmap.Transparent := True;
  MasterBitmap.TransparentColor := clFuchsia;
  MasterBitmap.HandleType := bmDIB;
  // Global info
  with SkinCommonInfo do
  begin
    Author := SkinFile.ReadString(s_GlobalInfo, s_Author, '');
    Description := SkinFile.ReadString(s_GlobalInfo, s_Description, '');
    MainFont := SkinFile.ReadString(s_GlobalInfo, s_MainFont, '');
    UseAeroBluring := SkinFile.ReadInteger(s_GlobalInfo,
      s_UseAeroBluring, 0) <> 0;

    BrightMin := SkinFile.ReadInteger(s_GlobalInfo, s_BrightMin, -50);
    BrightMax := SkinFile.ReadInteger(s_GlobalInfo, s_BrightMax, 50);

    BIVAlign := SkinFile.ReadInteger(s_GlobalInfo, s_BIVAlign, 0);
    BIKeepHUE := SkinFile.ReadInteger(s_GlobalInfo, s_BIKeepHUE, 0);
    Shadow1Color := SkinFile.ReadInteger(s_GlobalInfo, s_Shadow1Color, 0);
    DefLightColor := SkinFile.ReadInteger(s_GlobalInfo, s_DefLight, -1);
    ArrowStyle := TacArrowsStyle(SkinFile.ReadInteger(s_GlobalInfo,
      s_ArrowStyle, 0));
    Version := GetSkinVersion(SkinFile);
  end;
  CheckVersion;
end;

procedure TsSkinManager.Loaded;
var
  f: TCustomForm;
  b: boolean;
  iScale: integer;
  DC: hdc;
begin
  b := IsActive;
  inherited;
  if FSkinDirectory = '' then
    FSkinDirectory := DefSkinsDir;

  if FMenuSupport.IcoLineSkin = '' then
    FMenuSupport.IcoLineSkin := s_MenuIcoLine;

  LoadThirdNames(Self);

  if Application.MainForm <> nil then
    iScale := CurrentPPI(Self, Application.MainForm)
  else
  begin
    DC := GetDC(0);
    iScale := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;

  if iScale <> 96 then
    IterateImageLists(ScaleImageList, iScale);

  if Active and (SkinName <> '') then
    SendNewSkin(False);

{$IFDEF D2007}
  UpdateCommonDlgs(Self);
{$ENDIF}
  if ([csLoading, csReading] * ComponentState = []) and Assigned(InitDevEx) then
    InitDevEx(Active and (SkinName <> ''));

  if Assigned(FOnActivate) and not b and IsActive and
    not(csDesigning in ComponentState) then
    FOnActivate(Self);

  if not(csDesigning in ComponentState) then
    if IsActive then
    begin
      f := GetOwnerForm(Self);
      if (f is TCustomForm) and f.HandleAllocated then
      begin
        if acSupportedList = nil then
          acSupportedList := TList.Create;

        AddSupportedForm(f.Handle);
      end;

      if IsDefault and Effects.AllowAnimation then
        SetMouseLight(AnimEffects.Buttons.LightingMode = lmAffectNearest);
    end;

  SkinListController.ClearList(True, False);
  SkinListController.UpdateIfNeeded(False);

  if Options.ScaleMode = smVCL then
    Options.PixelsPerInch := DefaultPPI;
end;

procedure TsSkinManager.LoadFonts;
var
  i: integer;
  s, sf: string;

  function FileToFontName(const FName: string): string;
  var
    j, c: integer;
  begin
    Result := '';
    c := SkinFile.ReadInteger(s_GlobalInfo, s_FontCount, 0) - 1;
    for j := 0 to c do
      if SkinFile.ReadString(s_GlobalInfo, s_FontFile + IntToStr(j), '') = FName
      then
      begin
        Result := SkinFile.ReadString(s_GlobalInfo,
          s_FontName + IntToStr(j), '');
        Exit;
      end;
  end;

begin
  EmbeddedFonts.Clear;
  if sc <> nil then
  begin
    for i := 0 to sc.FileCount - 1 do
      if sc.Files[i].FileType = ftFont then
      begin
        sc.Files[i].Stream.Seek(0, 0);
        with TacEmbeddedFont(EmbeddedFonts.Add) do
        begin
          FontData.LoadFromStream(sc.Files[i].Stream);
          FontName := FileToFontName(sc.Files[i].Name);
          FileName := ExtractFileName(sc.Files[i].Name);
          LoadFont;
        end;
      end
  end
  else if SkinFile <> nil then
    for i := 0 to SkinFile.ReadInteger(s_GlobalInfo, s_FontCount, 0) - 1 do
    begin
      sf := SkinFile.ReadString(s_GlobalInfo, s_FontFile + IntToStr(i), '');
      s := SkinFile.ReadString(s_GlobalInfo, s_FontName + IntToStr(i), '');
      with TacEmbeddedFont(EmbeddedFonts.Add) do
        if FileExists(sf) then
        begin
          FileName := sf;
          FontName := s;
          FontData.LoadFromFile(sf);
          LoadFont;
        end;
    end;
end;

function TsSkinManager.ma: TsMaskArray;
begin
  Result := CommonSkinData.ma;
end;

{$IFNDEF D2005}

procedure TsSkinManager.OnCheckHot(Sender: TObject);
var
  R: TRect;
begin
  if (FActiveControl <> 0) and not(msAnimScaling in ManagerState) then
  begin
    GetWindowRect(FActiveControl, R);
    if not PtInRect(R, acMousePos) then
      ActiveControl := 0;

    if (ActiveGraphControl <> nil) and
      not(csDestroying in ActiveGraphControl.ComponentState) then
      if not acMouseInControl(ActiveGraphControl) then
        ActiveGraphControl.Perform(SM_ALPHACMD, AC_MOUSELEAVE_HI,
          LPARAM(ActiveGraphControl));
  end;
end;
{$ENDIF}

procedure ClearCharlistCaches(ImgList: TCustomImageList; Data: Longint);
begin
  if ImgList is TsCharImageList then
    TsCharImageList(ImgList).ClearCache
  else if ImgList is TsVirtualImageList then
    TsVirtualImageList(ImgList).ClearItems;
end;

procedure TsSkinManager.SendNewSkin(Repaint: boolean = True);
var
  M: TMessage;
  i: integer;
begin
  if [csLoading, csReading] * ComponentState = [] then
  begin
    ManagerState := ManagerState + [msBroadCasting { msSkinLoading } ];
    if IsDefault then
      ClearGlows;

    if not(csDesigning in ComponentState) and Repaint then
      LockForms(Self);

{$IFNDEF FPC}
    SkinableMenus.SkinBorderWidth := -1;
{$ENDIF}
    SkinCommonInfo.Active := False;
    RestrictDrawing := True;

    IterateImageLists(ClearCharlistCaches, 0);

    M.Msg := SM_ALPHACMD;
    M.WParam := AC_SETNEWSKIN_HI;
    M.LPARAM := LPARAM(Self);
    M.Result := 0;
    if csDesigning in ComponentState then
      for i := 0 to Screen.FormCount - 1 do
      begin
        with Screen.Forms[i] do
          if not(csDesigning in ComponentState) then
            Continue;

        AlphaBroadCast(Screen.Forms[i], M);
        SendToHooked(M);
      end
    else
      AppBroadCastS(M);

    RestrictDrawing := False;
    SkinCommonInfo.Active := (Length(SkinDataArray) > 0) and
      (Length(SkinDataArray[0].gd) > 0);
    if (DefaultManager = Self) and not GlobalHookInstalled then
      InstallHook;

    if Assigned(InitDevEx) then
      InitDevEx(True);

    if Assigned(RefreshDevEx) then
      RefreshDevEx;

    case Fonts.MainMode of
      fmFromSkin:
        if SkinCommonInfo.MainFont <> '' then
          ChangeAppFont(SkinCommonInfo.MainFont, False)
        else
          ChangeAppFont(DefStoredFontName, False);

      fmCustom:
        if Fonts.MainFont <> '' then
          ChangeAppFont(Fonts.MainFont, False)
        else
          ChangeAppFont(DefStoredFontName, False);
    end;

    if Repaint then
      RepaintForms(False);

    ManagerState := ManagerState - [msBroadCasting];
    if not(csDesigning in ComponentState) and IsDefault and Effects.AllowAnimation
    then
      SetMouseLight(AnimEffects.Buttons.LightingMode = lmAffectNearest);
  end;
end;

procedure TsSkinManager.SendRemoveSkin;
var
  M: TMessage;
  i: integer;
begin
  if IsDefault then
    SetMouseLight(False);

  if IsDefault and Assigned(InitDevEx) then
    InitDevEx(False);

  ManagerState := ManagerState + [msSkinRemoving];
  if (Fonts.MainMode = fmFromSkin) and not Application.Terminated then
    ChangeAppFont(DefStoredFontName, False);

  ClearGlows;
  UnInstallHook;

  ClearSkinData;

  M := MakeMessage(SM_ALPHACMD, AC_REMOVESKIN_HI, LPARAM(Self), 0);
  if csDesigning in ComponentState then
    for i := 0 to Screen.FormCount - 1 do
    begin
      with Screen.Forms[i] do
        if not(csDesigning in ComponentState) then
          Continue;

      AlphaBroadCast(Screen.Forms[i], M);
      SendToHooked(M);
    end
  else
    AppBroadCastS(M);

  if Assigned(MasterBitmap) then
    FreeAndNil(MasterBitmap);

  if IsDefault then
  begin
    DelLightControl(nil);
  end;

  ManagerState := ManagerState - [msSkinRemoving];
end;

procedure TsSkinManager.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not Value then
    begin
      if FSkinName <> '' then
      begin
        if IsDefault then
          acCurrentScaleMode := smVCL;

        if not(csLoading in ComponentState) then
          SendRemoveSkin;

        ClearSkinData;
{$IFDEF D2007}
        UpdateCommonDlgs(Self);
{$ENDIF}
        if Assigned(FOnDeactivate) and ([csDesigning] * ComponentState = [])
        then
          FOnDeactivate(Self);
      end;
    end
    else
      SkinName := FSkinName;

    SkinListController.SendSkinChanged;
  end;
end;

procedure TsSkinManager.SetBuiltInSkins(const Value: TsStoredSkins);
begin
  FBuiltInSkins.Assign(Value);
end;

procedure TsSkinManager.SetCommonSections(const Value: TStringList);
var
  i: integer;
  s: string;
begin
  FCommonSections.Assign(Value);
  for i := 0 to FCommonSections.Count - 1 do
  begin
    s := FCommonSections[i];
    if (s <> '') and (s[1] <> ';') then
      FCommonSections[i] := acntUtils.DelChars(s, s_Space);
  end;
  SkinName := SkinName;
end;

procedure TsSkinManager.SetSkinDirectory(const Value: string);
begin
  if CompareText(FSkinDirectory, Value) <> 0 then
  begin
    FSkinDirectory := Value;
    SkinCommonInfo.SkinPath := GetFullSkinDirectory;
    SkinListController.ClearList(True { Update later } ,
      True { False {Do not send 'changed' state } );
    if not(csLoading in ComponentState) then
      SkinListController.UpdateIfNeeded;

    if Assigned(FOnSkinListChanged) then
      FOnSkinListChanged(Self);
  end;
end;

procedure TsSkinManager.SetSkinName(const Value: TsSkinName);
var
  b, bDoChange: boolean;
  s, OldName, s1: string;
begin
  OldName := FSkinName;
  FSkinName := Value;
  // if not (csLoading in ComponentState) or (csDesigning in ComponentState) then
  begin
    b := IsActive;
    if FActive then
    begin
      CommonDataLoaded := False;
      if Assigned(FOnBeforeChange) then
        FOnBeforeChange(Self);

      ManagerState := ManagerState - [msRTLFlipped] + [msSkinChanging];
      // Save form image to layered window if ExtendedBorders
      if ExtendedBorders and not NoAutoUpdate and
        AnimEffects.SkinChanging.Active and Effects.AllowAnimation then
        CopyExForms(Self);

      if Assigned(FOnSkinChanging) then
      begin
        bDoChange := True;
        FOnSkinChanging(Self, Value, bDoChange);
        if not bDoChange then
          Exit;
      end;

      s := Value;
      if Length(Value) > 4 then
      begin // Remove extension if exists
        s1 := Copy(Value, Length(Value) - 3, 4);
        if UpperCase(s1) = UpperCase(s_Dot + acSkinExt) then
        begin
          Delete(s, Length(Value) - 3, 4);
          FSkinName := s;
        end;
      end;
      try
        ClearSkinData;
        InitCommonData;
        if (Application.BiDiMode = bdRightToLeft) or
          (Application.MainForm <> nil) and (Application.MainForm.IsRightToLeft)
        then
          FlipImages(True);
      except
        on E: Exception do
        begin
          FSkinName := OldName;
          ShowError(E.Message);
        end;
      end;

      if FActive then
      begin
        if not NoAutoUpdate then
          SendNewSkin;
      end
      else
        SendRemoveSkin;

      if Assigned(FOnActivate) and not b and FActive and
        ([csDesigning { , csLoading } ] * ComponentState = []) then
        FOnActivate(Self);

      ManagerState := ManagerState - [msSkinChanging];
      SkinCommonInfo.Active := (Length(SkinDataArray) > 0) and
        (Length(SkinDataArray[0].gd) > 0); // True;
      if not NoAutoUpdate and Assigned(OnAfterChange) then
        FOnAfterChange(Self);
    end;
    if not(msSkinChanging in ManagerState) and (OldName <> FSkinName) then
      SkinListController.SendSkinChanged;
  end;
{$IFDEF D2007}
  UpdateCommonDlgs(Self);
{$ENDIF}
end;

{$IFNDEF FPC}

procedure TsSkinManager.SetSkinnedPopups(const Value: boolean);
begin
  if FSkinnedPopups <> Value then
  begin
    FSkinnedPopups := Value;
    if not(csDesigning in ComponentState) and FSkinnedPopups and
      (SkinableMenus <> nil) and IsDefault then
      if FSkinnedPopups then
        SkinableMenus.UpdateMenus
      else
        SkinableMenus.InitItems(False);
  end;
end;
{$ENDIF}

procedure TsSkinManager.SetSkinsFilter(const Value: TacSkinsFilter);
begin
  if FSkinsFilter <> Value then
  begin
    FSkinsFilter := Value;
    SkinListController.ClearList;
    if Assigned(FOnSkinListChanged) then
      FOnSkinListChanged(Self);
  end;
end;

procedure TsSkinManager.SetKeyList(const Value: TStringList);
begin
  FKeyList.Assign(Value);
end;

procedure TsSkinManager.SetSkinInfo(const Value: TacSkinInfo);
begin
  //
end;

procedure TsSkinManager.SetVersion(const Value: string);
begin
  //
end;

procedure TsSkinManager.StoreDefFont;
begin
  if DefStoredFontName = '' then
  begin
{$IFDEF D2010}
    if Application.MainForm <> nil then
      DefStoredFontName := Application.MainForm.Font.Name
    else
      DefStoredFontName := Application.DefaultFont.Name;
{$ELSE}
    if Application.MainForm <> nil then
      DefStoredFontName := Application.MainForm.Font.Name
    else
      DefStoredFontName := 'Tahoma';
{$ENDIF}
    DefOldFontName := DefStoredFontName;
  end;
end;

type
  TAccessControl = class(TWinControl);
  { TAnchorsArray = array of TAnchors;

    procedure DisableAnchors_Core(ParentControl: TWinControl; var aAnchorStorage: TAnchorsArray; var StartingIndex: Integer);
    var
    iCounter: integer;
    ChildControl: TControl;
    begin
    if (StartingIndex + ParentControl.ControlCount + 1) > (Length(aAnchorStorage)) then
    SetLength(aAnchorStorage, StartingIndex + ParentControl.ControlCount + 1);

    for iCounter := 0 to ParentControl.ControlCount - 1 do begin
    ChildControl := ParentControl.Controls[iCounter];
    aAnchorStorage[StartingIndex] := ChildControl.Anchors;
    if [akLeft, akRight ] * ChildControl.Anchors = [akLeft, akRight] then
    ChildControl.Anchors := ChildControl.Anchors - [akRight];

    if [akTop, akBottom] * ChildControl.Anchors = [akTop, akBottom] then
    ChildControl.Anchors := ChildControl.Anchors - [akBottom];

    Inc(StartingIndex);
    end;
    // Add children
    for iCounter := 0 to ParentControl.ControlCount - 1 do begin
    ChildControl := ParentControl.Controls[iCounter];
    if ChildControl is TWinControl then
    DisableAnchors_Core(TWinControl(ChildControl), aAnchorStorage, StartingIndex);
    end;
    end;


    procedure EnableAnchors_Core(ParentControl: TWinControl; aAnchorStorage: TAnchorsArray; var StartingIndex: Integer);
    var
    iCounter: integer;
    ChildControl: TControl;
    begin
    for iCounter := 0 to ParentControl.ControlCount - 1 do begin
    ChildControl := ParentControl.Controls[iCounter];
    ChildControl.Anchors := aAnchorStorage[StartingIndex];
    Inc(StartingIndex);
    end;
    // Restore children
    for iCounter := 0 to ParentControl.ControlCount - 1 do begin
    ChildControl := ParentControl.Controls[iCounter];
    if ChildControl is TWinControl then
    EnableAnchors_Core(TWinControl(ChildControl), aAnchorStorage, StartingIndex);
    end;
    end;
  }

type
  TAccessProvider = class(TsSkinProvider);

procedure AnimaScaleForm(sp: TComponent; Param: integer);
begin
  SendMessage(TsSkinProvider(sp).Form.Handle, SM_ALPHACMD,
    MakeWParam(0, AC_ANIMSCALE), LPARAM(Param));
end;

procedure SetPPIAnimated(NewPPI: integer);
begin
  if DefaultManager.Options.PixelsPerInch <> NewPPI then
  begin
    DefaultManager.ManagerState := DefaultManager.ManagerState +
      [msAnimScaling];
    IterateForms(DefaultManager, AnimaScaleForm,
      MakeLParam(integer(DefaultManager.Options.ScaleMode = smVCL), NewPPI));
    // Start anim threads
    DefaultManager.Options.PixelsPerInch := NewPPI;
    DefaultManager.Options.ScaleMode := smCustomPPI;
  end;
  // DefaultManager.ManagerState := DefaultManager.ManagerState - [msAnimScaling];
end;

function IsSysPPIUsed(sm: TsSkinManager): boolean;
begin
  Result := (sm.Options.ScaleMode = smVCL) and
    (DefaultPPI in [96, 120, 144, 192]);
end;

{
  function DisableAnchors(ParentControl: TWinControl): TAnchorsArray;
  var
  StartingIndex: Integer;
  begin
  StartingIndex := 0;
  DisableAnchors_Core(ParentControl, Result, StartingIndex);
  end;


  procedure EnableAnchors(ParentControl: TWinControl; aAnchorStorage: TAnchorsArray);
  var
  StartingIndex: Integer;
  begin
  StartingIndex := 0;
  EnableAnchors_Core(ParentControl, aAnchorStorage, StartingIndex);
  end;
}

{$IFDEF DELPHI_10}

type
  TAccessForm = class(TForm);
{$ENDIF}

procedure TsSkinManager.UpdateScale(Ctrl: TWinControl;
  iCurrentPPI: integer = 96; iNewDPI: integer = 0);
{$IFDEF ACDPIAWARE}
const
  SWP_HIDE = SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE +
    SWP_HIDEWINDOW;
  SWP_SHOW = SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE +
    SWP_SHOWWINDOW;
var
  R: TRect;
{$IFDEF DELPHI_10SYDNEY}
  Form: TAccessForm;
{$ENDIF}
  b, IsVisible: boolean;
  sp: TsSkinProvider;
  w, h, iNewPPI, iOldPPI: integer;
{$IFNDEF D2010}
  OldOnChange: TNotifyEvent;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF ACDPIAWARE}
  if Ctrl is TCustomForm then
  begin
{$IFDEF DELPHI_10SYDNEY}
    Form := TAccessForm(Ctrl);
{$ENDIF}
    sp := TsSkinProvider(SendAMessage(Ctrl.Handle, AC_GETPROVIDER));
    if (sp <> nil) and (sp.SkinData.SkinManager <> Self) then
      Exit;

    TForm(Ctrl).Scaled := Options.ScaleMode in [smVCL];
  end
{$IFDEF DELPHI_10SYDNEY}
  else
    Form := nil
{$ENDIF};

  if (Options.ScaleMode <> smVCL) or
    ((iNewDPI <> 0) and (iNewDPI <> iCurrentPPI)) then
  begin
    StoreDefFont;
    if iNewDPI = 0 then
    begin
      iOldPPI := Ctrl.Perform(SM_ALPHACMD, AC_GETSCALE shl 16, 1);
      if iOldPPI = 0 then // If message is not supported
        iOldPPI := 96;
    end
    else
      iOldPPI := iCurrentPPI;

    if iNewDPI <> 0 then
      iNewPPI := iNewDPI
    else if Options.ScaleMode = smCustomPPI then
      // if Options.PixelsPerInch <> 0 then
      iNewPPI := Options.PixelsPerInch
      { else
        iNewPPI := 96 }
    else
    begin
      case iOldPPI of
        0:
          iOldPPI := iCurrentPPI;
        -1:
          Exit;
      end;
      iNewPPI := aPPIArray[GetScale];
    end;

    if (iNewPPI <> iOldPPI) and (iNewPPI <> 0) then
      with TAccessControl(Ctrl) do
      begin
{$IFDEF DELPHI_10SYDNEY}
        if (Form <> nil) then
          Form.DoBeforeMonitorDpiChanged(iOldPPI, iNewPPI);
{$ENDIF}
        ManagerState := ManagerState + [msFormScaling];
{$IFDEF DELPHI_10BERLIN}
        FCurrentPPI := iNewPPI;
{$ENDIF}
        // SetLength(AnchArray, 0);
        w := MulDiv(ClientWidth, iNewPPI, iOldPPI);
        h := MulDiv(ClientHeight, iNewPPI, iOldPPI);
        R := BoundsRect;

        b := Effects.AllowAnimation;
        Effects.AllowAnimation := False;
        IsVisible := HandleAllocated and IsWindowVisible(Handle);

        if IsVisible and ([msAnimScaling, msFormScaling] * ManagerState = [])
        then
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDE);

        DisableAlign;
        // AnchArray := DisableAnchors(Ctrl);

{$IFNDEF D2010}// 10BERLIN}
        // {IFNDEF DELPHI_XE7}//10BERLIN}
        OldOnChange := Constraints.OnChange;
        Constraints.OnChange := nil;
        Constraints.MinHeight := MulDiv(Constraints.MinHeight, iNewPPI,
          iOldPPI);
        Constraints.MinWidth := MulDiv(Constraints.MinWidth, iNewPPI, iOldPPI);
        Constraints.OnChange := OldOnChange;
{$ENDIF}
        ChangeScale(iNewPPI, iOldPPI);

        Left := R.Left;
        Top := R.Top;
        ClientWidth := w;
        ClientHeight := h;
        Perform(SM_ALPHACMD, AC_SETSCALE_HI, iNewPPI);

        // EnableAnchors(Ctrl, AnchArray);
        EnableAlign;
        if IsVisible and ([msAnimScaling, msFormScaling] * ManagerState = [])
        then
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_SHOW);

        if iNewPPI < iOldPPI then
          InvalidateRect(0, @R, True);

        Effects.AllowAnimation := b;
        ManagerState := ManagerState - [msFormScaling];
{$IFDEF DELPHI_10SYDNEY}
        if (Form <> nil) then
          Form.DoAfterMonitorDpiChanged(iOldPPI, iNewPPI);
{$ENDIF}
      end;
  end;
{$ENDIF}
end;

procedure TsSkinManager.UpdateSkin(Repaint: boolean = True);
begin
  if Active then
    SendNewSkin(Repaint);
end;

procedure TsSkinManager.UpdateSkinSection(const SectionName: string);
var
  M: TMessage;
  i: integer;
  s: string;
begin
  M.Msg := SM_ALPHACMD;
  M.WParamHi := AC_UPDATESECTION;
  s := UpperCase(SectionName);
  M.LPARAM := integer(PChar(s));
  M.Result := 0;
  if csDesigning in ComponentState then
    for i := 0 to Screen.FormCount - 1 do
      AlphaBroadCast(Screen.Forms[i], M)
  else
    AppBroadCastS(M);
end;

procedure TsSkinManager.RepaintForms(DoLockForms: boolean = True);
var
  ap: TacProvider;
  M: TMessage;
  i: integer;
begin
  M.Msg := SM_ALPHACMD;
  M.LPARAM := LPARAM(Self);
  if not(csDesigning in ComponentState) then
  begin
    M.WParam := AC_STOPFADING_HI;
    M.Result := 0;
    AppBroadCastS(M);
  end;
  M.WParam := AC_REFRESH_HI or FUpdatedSectionNdx;
  M.Result := 0;
  if csDesigning in ComponentState then
    for i := 0 to Screen.FormCount - 1 do
    begin
      with Screen.Forms[i] do
        if not(csDesigning in ComponentState) then
          Continue;

      AlphaBroadCast(Screen.Forms[i], M);
      SendToHooked(M);
    end
  else
  begin
    if not(csLoading in ComponentState)
    { and (Application.MainForm <> nil) Changing in DLL } then
    begin
      if DoLockForms then
        LockForms(Self);

      AppBroadCastS(M);
      UnLockForms(Self, not NoAutoUpdate);
    end
    else
      AppBroadCastS(M);

    if Assigned(acMagnForm) then
      SendMessage(acMagnForm.Handle, M.Msg, M.WParam, M.LPARAM);
    // Repaint dialogs
    if acSupportedList <> nil then
      for i := 0 to acSupportedList.Count - 1 do
      begin
        ap := TacProvider(acSupportedList[i]);
        if (ap <> nil) and (ap.ListSW <> nil) and
          IsWindowVisible(ap.ListSW.CtrlHandle) then
          RedrawWindow(ap.ListSW.CtrlHandle, nil, 0, RDWA_ALL);
      end;
  end;
end;

procedure TsSkinManager.ClearSkinData;
var
  Section: TacSection;
begin
  if IsDefault then
    ClearGlows;

  SkinCommonInfo.Active := False;
  if FSkinableMenus <> nil then
    TAccessMenus(SkinableMenus).sd := nil;

  for Section := Low(SkinCommonInfo.Sections)
    to High(SkinCommonInfo.Sections) do
    SkinCommonInfo.Sections[Section] := -1;

  ClearSkinArrays;
  CommonDataLoaded := False;
end;

function TsSkinManager.GetActiveEditColor: TColor;
begin
  Result := Palette[pcEditBG];
end;

function TsSkinManager.GetActiveEditFontColor: TColor;
begin
  Result := Palette[pcEditText];
end;

function TsSkinManager.GetHighLightColor(Focused: boolean = True): TColor;
begin
  Result := iff(Focused, Palette[pcSelectionBG_Focused],
    Palette[pcSelectionBG]);
end;

function TsSkinManager.GetHighLightFontColor(Focused: boolean = True): TColor;
begin
  Result := iff(Focused, Palette[pcSelectionText_Focused],
    Palette[pcSelectionText]);
end;

function TsSkinManager.IsActive: boolean;
begin
  Result := SkinCommonInfo.Active;
  // and (Length(SkinDataArray) > 0) and (Length(SkinDataArray[0].gd) > 0);
end;

function TsSkinManager.IsValidImgIndex(ImageIndex: integer): boolean;
begin
  Result := (ImageIndex >= 0) and (ImageIndex < Length(CommonSkinData.ma));
end;

function TsSkinManager.IsValidSkinIndex(SkinIndex: integer): boolean;
{$IFDEF DEBUG}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if IsActive and (SkinIndex >= 0) and (LengthOfGD > 0) then
    for i := 1 to Length(SkinDataArray) - 1 do
      if (SkinDataArray[i] <> nil) and
        (Length(SkinDataArray[i].gd) <> LengthOfGD) then
      begin
{$IFDEF ACDEBUG}
        ShowError(
          'Length of GD arrays are not equal (TsSkinManager.IsValidSkinIndex)');
{$ENDIF}
        Result := False;
        Exit;
      end;

{$ENDIF}
  Result := (SkinIndex >= 0) and (SkinIndex < LengthOfGD);
end;

function TsSkinManager.GetScale: integer;
begin
  if (csDesigning in ComponentState) or (Options.ScaleMode in [smOldMode, smVCL])
  then
    Result := 0
  else if Options.ScaleMode = smAuto then
    Result := SysFontScale
  else
    Result := ord(Options.ScaleMode);
end;

function TsSkinManager.GetScaledMaskData(const aSectionName, aPropName: string;
  { aDivPPI, } aMulPPI: integer; var md: TsMaskData): integer;
var
  Stream: TMemoryStream;
  s, sName: string;
  i, j: integer;
begin
  if IsActive then
  begin
    for i := 0 to acMaxScaleNdx do
      if aPPIArray[i] >= aMulPPI then
      begin
        s := UpperCase(SkinCommonInfo.OptionsDat.ReadString(aSectionName,
          aPropName + aSfxs[i], ''));
        if s <> '' then
          Break;
      end;

    if s = '' then
      for i := acMaxScaleNdx downto 0 do
        if aPPIArray[i] <= aMulPPI then
        begin
          s := UpperCase(SkinCommonInfo.OptionsDat.ReadString(aSectionName,
            aPropName + aSfxs[i], ''));
          if s <> '' then
            Break;
        end;

    if s <> '' then
    begin
      s := ConvertSkinStr(s);
      sName := Copy(s, 45, Length(s) - 44);
      md.Bmp := nil;
      md.PropertyName := aPropName;
      md.ClassName := aSectionName;
      md.SkinIndex := 0;
      md.DrawMode := CopyInt(s, 37, 2);
      md.MaskType := CopyInt(s, 35, 1);
      md.ImgType := acImgTypes[Mini(CopyInt(s, 36, 1), Length(acImgTypes) - 1)];
      md.ImageCount := CopyInt(s, 34, 1);
      md.Manager := Self;
      md.R := CopyR(s, 2);
      md.DivPPI := aPPIArray[i];
      md.MulPPI := aMulPPI;
      // Border width
      md.WL := CopyInt(s, 18, 4);
      md.WT := CopyInt(s, 22, 4);
      md.WR := CopyInt(s, 26, 4);
      md.WB := CopyInt(s, 30, 4);

      Stream := nil;
      if sName <> '' then
      begin // External file
        if SkinIsPacked then
          if sc = nil then
          begin
            Result := -1;
            Exit;
          end
          else
            for j := 0 to sc.FileCount - 1 do
              if UpperCase(sc.Files[j].Name) = sName then
              begin
                Stream := sc.Files[j].Stream;
                Break; // Found and loaded
              end;
        // Load image to item
        AddExtItem(md, sName, Stream, (md.ImgType in [itisaGlow, itisaTexture])
          { IgnoreFuchsia } , Self);
        if md.Bmp <> nil then
        begin
          md.Bmp.Canvas.Handle;
          md.Bmp.Canvas.Lock;
        end
        else
        begin
          Result := -1;
          Exit;
        end;
      end
      else // From MasterBitmap
        if (md.MulPPI <> md.DivPPI) and (md.DrawMode and BDM_NOSCALE = 0) then
          // Make scaled image if needed
          ScaleMaskData(md, MasterBitmap,
            (md.ImgType in [itisaGlow, itisaTexture])) // False)
        else
        begin
          md.Bmp := CreateBmp32(md.R);
          BitBlt(md.Bmp.Canvas.Handle, 0, 0, md.Bmp.Width, md.Bmp.Height,
            MasterBitmap.Canvas.Handle, md.R.Left, md.R.Top, SRCCOPY);
          md.R := MkRect(md.Bmp);
          md.Height := md.Bmp.Height div (1 + md.MaskType);
          md.Width := md.Bmp.Width div md.ImageCount;
        end;

      // Calc corner type
      if md.ImgType = itisaBorder then
        md.Cfg := md.Cfg or GetCornerType(Self, md);

      Result := aMulPPI;
    end
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TsSkinManager.GetSkinIndex(const SkinSection: string;
  SkinData: TacSkinData = nil): integer;
var
  i: integer;
begin
  if SkinSection <> '' then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
      for i := 0 to Length(gd) - 1 do
        if SkinData.gd[i].ClassName = SkinSection then
        begin
          Result := i;
          Exit;
        end;
  end;
  Result := -1;
end;

function TsSkinManager.GetMaskIndex(const aSkinIndex: integer;
  const SkinSection, mask: string; SkinData: TacSkinData { = nil } ): integer;
var
  i: integer;
  s: string;
begin
  if aSkinIndex >= 0 then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
    begin
      for i := 0 to Length(ma) - 1 do
        if (ma[i].SkinIndex = aSkinIndex) and (ma[i].PropertyName = mask) then
        begin
          Result := i;
          Exit;
        end;

      // If not found, but parent class is defined
      if (gd[aSkinIndex].ParentClass <> '') and
        (SkinSection <> gd[aSkinIndex].ParentClass) then
      begin
        s := gd[aSkinIndex].ParentClass;
        i := GetSkinIndex(s);
        if i >= 0 then
        begin
          Result := GetMaskIndex(i, s, mask, SkinData);
          Exit;
        end;
      end;
    end;
  end;
  Result := -1;
end;

procedure TsSkinManager.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then
    begin
      Prop := Value;
      UpdateCurrentSkin;
    end;
  end;

begin
  case Index of
    0:
      ChangeProp(FBrightness, LimitIt(Value, -100, 100));
    1:
      ChangeProp(FHueOffset, Value);
    2:
      ChangeProp(FSaturation, Value);
  end;
end;

procedure TsSkinManager.SetIsDefault(const Value: boolean);
begin
  if Value or (DefaultManager = nil) then
  begin
    DefaultManager := Self;
    if Value then
      acCurrentScaleMode := Options.ScaleMode;

    if Active then
      SendNewSkin
    else
      SendRemoveSkin;
  end
end;

function TsSkinManager.GetRandomSkin: acString;
var
  sl: TacStringList;
begin
  sl := TacStringList.Create;
  GetSkinNames(sl);
  if sl.Count > 0 then
  begin
    Randomize;
    Result := sl[Random(sl.Count)]
  end
  else
    Result := '';

  FreeAndNil(sl);
end;

function TsSkinManager.GetIsDefault: boolean;
begin
  Result := DefaultManager = Self;
end;

function TsSkinManager.GetTextureIndex(aSkinIndex: integer;
  const SkinSection, PropName: string;
  SkinData: TacSkinData { = nil } ): integer;
var
  i, l: integer;
begin
  if aSkinIndex >= 0 then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
    begin
      l := Length(ma);
      if (l > 0) and (SkinSection <> '') then
      begin
        for i := 0 to l - 1 do
          with ma[i] do
            if (ImgType = itisaTexture) and (PropertyName = PropName) and
              (ClassName = SkinSection) then
            begin
              Result := i;
              Exit;
            end;

        if (gd[aSkinIndex].ParentClass <> '') and
          (SkinSection <> gd[aSkinIndex].ParentClass) then
        begin
          i := GetSkinIndex(gd[aSkinIndex].ParentClass);
          if i >= 0 then
          begin
            Result := GetTextureIndex(i, gd[aSkinIndex].ParentClass, PropName,
              SkinData);
            Exit;
          end;
        end;
      end;
    end;
  end;
  Result := -1;
end;

function TsSkinManager.GetTextureIndex(aSkinIndex: integer;
  const PropName: string; SkinData: TacSkinData): integer;
var
  i, l: integer;
begin
  if aSkinIndex >= 0 then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
    begin
      l := Length(ma);
      if l > 0 then
      begin
        for i := 0 to l - 1 do
          with ma[i] do
            if (SkinIndex = aSkinIndex) and (ImgType = itisaTexture) and
              (PropertyName = PropName) then
            begin
              Result := i;
              Exit;
            end;

        if gd[aSkinIndex].ParentClass <> '' then
        begin
          Result := GetTextureIndex(GetSkinIndex(gd[aSkinIndex].ParentClass),
            PropName, SkinData);
          Exit;
        end;
      end;
    end;
  end;
  Result := -1;
end;

function TsSkinManager.GetMaskIndex(const SkinSection, mask: string;
  SkinData: TacSkinData): integer;
var
  i: integer;
begin
  if SkinSection <> '' then
  begin
    if SkinData = nil then
      SkinData := CommonSkinData;

    with SkinData do
      for i := 0 to Length(ma) - 1 do
        if (ma[i].ClassName = SkinSection) and (ma[i].PropertyName = mask) then
        begin
          Result := i;
          Exit;
        end;
  end;
  Result := -1;
end;

function TsSkinManager.GetMaskIndex(const SkinIndex: integer;
  const mask: string; SkinData: TacSkinData): integer;
var
  i: integer;
begin
  Result := -1;
  if SkinData = nil then
    SkinData := CommonSkinData;
  if SkinIndex >= 0 then
    with SkinData do
    begin
      for i := 0 to Length(ma) - 1 do
        if SkinData.ma[i].SkinIndex = SkinIndex then
          if SkinData.ma[i].PropertyName = mask then
          begin
            Result := i;
            Exit;
          end;
      if gd[SkinIndex].ParentClass <> '' then
        Result := GetMaskIndex(GetSkinIndex(gd[SkinIndex].ParentClass), mask,
          SkinData);
    end;
end;

function TsSkinManager.MainWindowHook(var Message: TMessage): boolean;
var
  R: TRect;
  Wnd: hwnd;
  aMsg: TMSG;
  i: integer;
  mi: PacMenuInfo;
  sp: TsSkinProvider;
  FMenuItem: TMenuItem;
{$IFNDEF NOWNDANIMATION}
  b: boolean;
{$ENDIF}
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  Result := False;
  case Message.Msg of
    SM_ALPHACMD:
      if Message.WParamHi = AC_COPYDATA then
      begin
        PeekMessage(aMsg, Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}, SM_ALPHACMD, SM_ALPHACMD, PM_NOREMOVE);
        ReceiveData(Self, boolean(Message.LPARAM <> 0));
      end;
{$IFNDEF FPC}
    WM_DRAWMENUBORDER:
      if SkinnedPopups then
      begin
        FMenuItem := TMenuItem(Message.LPARAM);
        if Assigned(FMenuItem) and Assigned(FMenuItem.Parent) then
          if GetMenuItemRect(0, FMenuItem.Parent.Handle, 0, R) or
            (Assigned(PopupList) and GetMenuItemRect(PopupList.Window,
            FMenuItem.Parent.Handle, 0, R)) then
          begin
            Wnd := WindowFromPoint(Point(R.Left + WidthOf(R) div 2,
              R.Top + HeightOf(R) div 2));
            if (Wnd <> 0) and (GetWndClassName(Wnd) = '#32768') then
            begin // If menu is found
              mi := SkinableMenus.GetMenuInfo(FMenuItem, nil, Wnd);
              if (mi <> nil) and (mi^.Bmp <> nil) then
                SkinableMenus.DrawWndBorder(Wnd, mi.Bmp);
            end;
          end;
        Result := True;
      end;
    WM_DWMSENDICONICLIVEPREVIEWBITMAP:
      if ac_ChangeThumbPreviews and (Application.MainForm <> nil) then
        // Task menu support when not MainFormOnTaskBar
        try
          sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
            AC_GETPROVIDER));
          if sp <> nil then
            Result := SetPreviewBmp(Application.Handle, sp);
        finally
          Message.Result := 0;
        end;
    WM_DWMSENDICONICTHUMBNAIL:
      if ac_ChangeThumbPreviews and (Application.MainForm <> nil) and
        (Message.LParamHi <> 0) and (Message.LParamLo <> 0) then
        // Task menu support when not MainFormOnTaskBar
        try
          sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
            AC_GETPROVIDER));
          if sp <> nil then
            Result := SetThumbIcon(Application.Handle, sp, Message.LParamHi,
              Message.LParamLo);
        finally
          Message.Result := 0;
        end;
    WM_DRAWMENUBORDER2:
      if SkinnedPopups then
      begin
        Wnd := hwnd(Message.LPARAM);
        if (Wnd <> 0) and (GetWndClassName(Wnd) = '#32768') then
        begin // If menu is found
          mi := SkinableMenus.GetMenuInfo(nil, nil, Wnd);
          if mi.Bmp <> nil then
            SkinableMenus.DrawWndBorder(Wnd, mi.Bmp);
        end;
        Result := True;
      end;
{$ENDIF}
    $031A { <- WM_THEMECHANGED } :
      Result := True;
{$IFDEF D2005}
    787:
      if {$IFDEF D2007}Application.MainFormOnTaskBar and
{$ENDIF}(Application.MainForm <> nil) then
      begin // Task menu support when not MainFormOnTaskBar
        try
          sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
            AC_GETPROVIDER));
        except
          sp := nil;
        end;
        if sp <> nil then
          with acMousePos do
          begin
            sp.DropSysMenu(X, Y);
            Result := True;
          end;
      end;
{$ENDIF}
    CM_ACTIVATE:
      // Solving a problem in Report Builder dialogs and similar windows
      for i := Screen.FormCount - 1 downto 0 do
        if Screen.Forms[i].HandleAllocated then
          SendAMessage(Screen.Forms[i].Handle, AC_INVALIDATE);
{$IFNDEF NOWNDANIMATION}
    WM_WINDOWPOSCHANGED:
      if acLayered then
        if (TWMWindowPosChanged(Message).WindowPos.Flags and SWP_HIDEWINDOW <> 0)
        then
          if (AnimEffects.FormHide.Active) and Effects.AllowAnimation and
            (AnimEffects.FormHide.Time > 0) and
            not IsIconic(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}) and Application.Terminated then
          begin
            if (Application.MainForm = nil) or
              not Application.MainForm.HandleAllocated or
              not IsWindowVisible(Application.MainForm.Handle) then
              Exit;
            sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
              AC_GETPROVIDER));
            if (sp <> nil) and sp.DrawNonClientArea and
              Options.DrawNonClientArea and not sp.SkipAnimation and sp.AllowAnimation
            then
            begin
              sp.SkipAnimation := True;
              sp.FormState := sp.FormState or FS_ANIMCLOSING;
              if sp.BorderForm <> nil then
                SetWindowRgn(sp.BorderForm.AForm.Handle,
                  sp.BorderForm.MakeRgn, False);
              if AeroIsEnabled then
                DoLayered(Application.MainForm.Handle, True, MaxByte - 1);
              Application.MainForm.Update;
              acHideTimer := nil;
              AnimHideForm(sp, sp.SkinData.SkinManager.AnimEffects.FormHide.
                Time, sp.SkinData.SkinManager.AnimEffects.FormHide.Mode);
              while InAnimationProcess do
                Continue;
            end;
          end;
{$ENDIF}
    WM_SYSCOMMAND:
      if IsActive then
        case Message.WParam of
          SC_MINIMIZE:
            begin
              ShowState := saMinimize;
{$IFNDEF NOWNDANIMATION}
              if not IsIconic(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}) and ((Application.MainForm <> nil) and Application.MainForm.Visible) then
              begin
                sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
                  AC_GETPROVIDER));
                if sp <> nil then
                  if AnimEffects.Minimizing.Active and Effects.AllowAnimation
                  then
                  begin
                    if sp.DrawNonClientArea and Options.DrawNonClientArea then
                    begin
                      sp.SkipAnimation := True;
                      i := integer(StartMinimizing(sp));
                      if not AeroIsEnabled then
                      begin
                        Result := True;
                        b := acGetAnimation;
                        acSetAnimation(False);
                        Application.Minimize;
                        acSetAnimation(b);
                      end;
                      if (i = 0) and (sp.BorderForm <> nil) and
                        (sp.BorderForm.AForm <> nil) then
                        if sp.FormState and FS_ANIMMINIMIZING = 0 then
                        begin
                          sp.BorderForm.ExBorderShowing := True;
                          FreeAndNil(sp.BorderForm.AForm);
                          sp.BorderForm.ExBorderShowing := False;
                        end;
                    end;
                  end
                  else if (sp.BorderForm <> nil) and (sp.BorderForm.AForm <> nil)
                  then
                  begin
                    sp.BorderForm.ExBorderShowing := True;
                    FreeAndNil(sp.BorderForm.AForm);
                    // sp.BorderForm.ExBorderShowing := False; Restored later
                  end;
              end;
{$ENDIF}
            end;
          SC_RESTORE:
            begin
              ShowState := saRestore;
{$IFNDEF NOWNDANIMATION}
              if Application.MainForm <> nil then
              begin
                if not Application.MainForm.Showing then
{$IFDEF D2007} if not Application.MainFormOnTaskBar then {$ENDIF}
                    Exit;
                sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle,
                  AC_GETPROVIDER));
                if sp = nil then
                  Exit;
                if sp.FormState and FS_ANIMCLOSING <> 0 then
                begin // If all windows were hidden
                  sp.FormState := sp.FormState and not FS_ANIMCLOSING;
                  // Update ExtBorders in the WM_NCPAINT message
                  if sp.SkinData.SkinManager.ExtendedBorders and
                    sp.AllowExtBorders and (sp.BorderForm = nil) then
                    FreeAndNil(sp.SkinData.FCacheBmp);
                  if sp.BorderForm <> nil then
                    sp.BorderForm.ExBorderShowing := False;
                  b := acGetAnimation;
                  acSetAnimation(False);
                  Application.Restore;
                  if AeroIsEnabled then
                  begin
                    RedrawWindow(Application.MainForm.Handle, nil, 0, RDWA_ALL);
                    if GetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE)
                      and WS_EX_LAYERED <> 0 then
                    begin
                      sp.SkinData.BGChanged := True;
                      SetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE,
                        GetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE)
                        and not WS_EX_LAYERED);
                    end;
                  end;
                  acSetAnimation(b);
                end
                else if sp.SkinData.SkinManager.AnimEffects.Minimizing.Active
                  and sp.SkinData.SkinManager.Effects.AllowAnimation and
                  Application.MainForm.
                  Visible { and IsIconic(Application.Handle) } then
                begin
                  if sp <> nil then
                  begin
                    if not sp.DrawNonClientArea or not Options.DrawNonClientArea
                    then
                      Exit;
                    if not StartRestoring(sp) then
                      with TAccessProvider(sp), Application.MainForm do
                        if CoverForm <> nil then
                        begin
                          if CoverForm.HandleAllocated then
                            SetWindowPos(Handle, CoverForm.Handle, 0, 0, 0, 0,
                              SWPA_SHOWZORDERONLY);
                          InvalidateRect(Handle, nil, True);
                          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
                          if CoverForm <> nil then
                            FreeAndNil(CoverForm);
                        end
                        else
                        begin
                          ShowWindow(Handle, SW_RESTORE);
                          InvalidateRect(Handle, nil, True);
                          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
                        end;
                  end;
                  if not AeroIsEnabled then
                  begin
                    Result := True;
                    b := acGetAnimation;
                    acSetAnimation(False);
                    Application.Restore;
                    acSetAnimation(b);
                  end;
                end
                else if AeroIsEnabled then
{$IFDEF D2009}
                  if not Application.MainFormOnTaskBar then
{$ENDIF}
                    with Application.MainForm do
                    begin
                      Result := True;
                      b := acGetAnimation;
                      acSetAnimation(False);
                      Application.Restore;
                      if not Visible then
                        Show; // If app was started as minimized
                      acSetAnimation(b);
                      InvalidateRect(Handle, nil, True);
                      RedrawWindow(Handle, nil, 0, RDWA_ALL);
                    end;
              end;
{$ENDIF} // NOWNDANIMATION
            end
        else
          ShowState := saIgnore;
        end;
  end;
end;

function TsSkinManager.oe: TacOutEffArray;
begin
  Result := CommonSkinData.oe;
end;

procedure TsSkinManager.ReloadSkin(PPI: integer = 96);
begin
  if FActive and InitSkinFile then
  begin
    if not CommonDataLoaded then
    begin
      CommonDataLoaded := True;
      LoadFonts;
      LoadCommonData; // Load once
    end;
    with CommonSkinData(PPI) do
    begin
      LoadParams;
      LoadAllMasks;
      LoadAllGeneralData;
      InitConstantIndexes;
      InitMaskIndexes;
      InitMaskParams;
      InitSkinParams(PPI);
      CheckShadows;
    end;
    if Assigned(FOnSkinLoading) then
      FOnSkinLoading(Self);
  end;
end;

procedure TsSkinManager.SetActiveControl(const Value: hwnd);
var
  OldHwnd: hwnd;
begin
  if (FActiveControl <> Value) and not(acMouseInControlDC(FActiveControl) and
    ContainsWnd(FActiveControl, Value)) then
  begin
    // Try reset graph control
    if (FActiveControl <> 0) and (FActiveGraphControl <> nil) then
    begin
      ActiveGraphControl := nil;
      if FActiveGraphControl <> nil then
      begin // If control is hot yet
        if FActiveControl <> 0 then
          SendAMessage(FActiveControl, AC_MOUSELEAVE, LPARAM(Self));
        Exit;
      end;
    end;
    OldHwnd := FActiveControl;
    FActiveControl := Value;
    if OldHwnd <> 0 then
      SendAMessage(OldHwnd, AC_MOUSELEAVE, LPARAM(Self));

    if FActiveControl <> 0 then
      SendAMessage(FActiveControl, AC_MOUSEENTER, LPARAM(Self));
  end;
end;

procedure TsSkinManager.InitCommonData;
begin
  case Options.ScaleMode of
    smVCL:
      begin
        CommonSkinData;
        if (Options.ScaleMode = smVCL) and (DefaultPPI <> 96) then
          CommonSkinData(DefaultPPI);
      end;
    smCustomPPI:
      CommonSkinData(Options.PixelsPerInch)
  else
    CommonSkinData(aPPIArray[GetScale]);
  end;
  IterateImageLists(SetBaseColor, acColorToRGB(clWindowText));
end;

procedure TsSkinManager.RepaintSection(sName: string);
begin
  FUpdatedSectionNdx := GetSkinIndex(sName);
  RepaintForms(False);
  FUpdatedSectionNdx := 0;
end;

function TsSkinManager.InitSkinFile: boolean;
var
  s: string;
  i: integer;
  sl: TStringList;
begin
  SkinCommonInfo.SkinPath := GetFullSkinDirectory + SkinName + s_Slash;
  s := SkinCommonInfo.SkinPath + OptionsDatName;
  SkinIsPacked := False;
  FreeAndNil(sc);
  // Search source
  ResetLastError;
  if acEditorSkinFile <> nil then // If skin received from SkinEditor
    SkinCommonInfo.OptionsDat.SetStrings(acEditorSkinFile)
  else if FileExists(s) then
  begin // If unpacked skin used
    sl := TStringList.Create;
    try
      sl.LoadFromFile(s);
      SkinCommonInfo.OptionsDat.SetStrings(sl);
    finally
      sl.Free;
    end;
  end
  else
  begin
    ResetLastError;
    s := NormalDir(SkinDirectory) + SkinName + s_Dot + acSkinExt;
    if FileExists(s) then
    begin // If external skin used
      LoadSkinFromFile(s, sc, FKeyList, Self);
      if sc.Options = nil then
      begin
        ManagerState := ManagerState + [msSkinChanging];
        ShowMessage('Internal format error in the "' + SkinName +
          '" skin. Please, update this skin to latest version.');
        ManagerState := ManagerState - [msSkinChanging];
        Result := False;
        Exit;
      end
      else
      begin
        SkinIsPacked := True;
        sc.Options.Seek(0, 0);
        sl := TStringList.Create;
        try
          SetCaseSens(sl);
          sl.LoadFromStream(sc.Options);
          SkinCommonInfo.OptionsDat.SetStrings(sl);
        finally
          FreeAndNil(sl);
        end;
      end;
    end
    else
    begin // If internal skin used
      ResetLastError;
      SkinCommonInfo.SkinPath := '';
      i := InternalSkins.IndexOf(FSkinName);
      if (i < 0) and (InternalSkins.Count > 0) then
      begin
        FSkinName := InternalSkins.Items[0].Name;
        i := 0;
      end
      else if InternalSkins.Count <= 0 then
      begin
        FActive := False;
        Result := False;
        Exit;
      end;
      if InternalSkins.Items[i].PackedData.Size > 0 then
      begin // If packed
        SkinIsPacked := True;
        CloseSkinFile;
        FreeAndNil(sc);
        sc := TacSkinConvertor.Create;
        sc.PackedData := InternalSkins.Items[i].PackedData;
        ExtractPackedData(sc, FKeyList, Self);
        sc.PackedData := nil;
        sc.Options.Seek(0, 0);
        sl := TStringList.Create;
        SetCaseSens(sl);
        sl.LoadFromStream(sc.Options);
        SkinCommonInfo.OptionsDat.SetStrings(sl);
        FreeAndNil(sl);
      end
      else
        SkinIsPacked := False;
    end;
  end;
  SkinFile := SkinCommonInfo.OptionsDat;
  Result := True;
end;

procedure TsSkinManager.InitSkinParams(aPPI: integer);
var
  i: integer;
  ColorItem: TacPaletteColors;
  function GetInt64(const Section, Name: string; AlternateValue: Int64): Int64;
  var
    s: string;
  begin
    s := SkinFile.ReadString(Section, Name, '');
    if s = '' then
      Result := AlternateValue
    else
      Result := StrToInt64(s);
  end;
  function AddAlpha(AColor: TColor): TColor;
  begin
    Result := TColor(Cardinal(AColor) or $FF000000);
  end;

begin
  // Load a main color palette
  with CommonSkinData do
  begin
    Palette[pcMainColor] := SkinFile.ReadInteger(s_GlobalInfo, s_Color,
      ColorToRGB(clBtnFace));
    Palette[pcLabelText] := SkinFile.ReadInteger(s_GlobalInfo,
      s_FontColor, clBlack);
    Palette[pcWebText] := SkinFile.ReadInteger(s_GlobalInfo, s_TCBottom,
      ColorToRGB(clBlue));
    Palette[pcWebTextHot] := SkinFile.ReadInteger(s_GlobalInfo, s_TCTop,
      ColorToRGB(clRed));
    Palette[pcBorder] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BorderColor, clBlack);
    // Receive edit color from EDIT section
    i := GetSkinIndex(s_Edit);
    if i >= 0 then
    begin
      Palette[pcEditText] := ColorToRGB(gd[i].Props[0].FontColor.Color);
      Palette[pcEditBG] := ColorToRGB(gd[i].Props[0].Color);
    end
    else
    begin
      Palette[pcEditText] := ColorToRGB(clWindowText);
      Palette[pcEditBG] := ColorToRGB(clWindow);
    end;
    // Receive colors from HINT section
    i := GetSkinIndex(s_Hint);
    if i >= 0 then
    begin
      Palette[pcHintText] := ColorToRGB(gd[i].Props[0].FontColor.Color);
      Palette[pcHintBG] := ColorToRGB(gd[i].Props[0].Color);
    end
    else
    begin
      Palette[pcHintText] := ColorToRGB(clInfoText);
      Palette[pcHintBG] := ColorToRGB(clInfoBk);
    end;
    // Receive a selection Text Color
    i := GetSkinIndex(s_Selection);
    if IsValidSkinIndex(i) then
    begin
      if gd[i].States > 1 then
      begin
        Palette[pcSelectionText_Focused] := gd[i].Props[1].FontColor.Color;
        Palette[pcSelectionBG_Focused] := gd[i].Props[1].Color;
      end
      else
      begin
        Palette[pcSelectionText_Focused] := gd[i].Props[0].FontColor.Color;
        Palette[pcSelectionBG_Focused] := gd[i].Props[0].Color;
      end;
      Palette[pcSelectionText] := gd[i].Props[0].FontColor.Color;
      Palette[pcSelectionBG] := gd[i].Props[0].Color;
    end
    else
    begin
      Palette[pcSelectionText_Focused] := -1;
      Palette[pcSelectionText] := -1;
      Palette[pcSelectionBG_Focused] := -1;
      Palette[pcSelectionBG] := -1;
    end;
    if (Palette[pcSelectionText] = -1) or (Palette[pcSelectionText] = clFuchsia)
    then
    begin
      i := GetSkinIndex(s_MenuItem);
      if IsValidSkinIndex(i) then
      begin
        Palette[pcSelectionText] := gd[i].Props[1].FontColor.Color;
        Palette[pcSelectionBG] := gd[i].Props[1].Color;
        Palette[pcSelectionBG_Focused] := gd[i].Props[1].Color;
        if (Palette[pcSelectionText] <> -1) and
          (Palette[pcSelectionText] <> clFuchsia) then
          Palette[pcSelectionText] := ColorToRGB(Palette[pcSelectionText])
        else
          Palette[pcSelectionText] := clHighlightText;
        Palette[pcSelectionText_Focused] := Palette[pcSelectionText];
      end
      else
      begin
        Palette[pcSelectionText_Focused] := ColorToRGB(clHighlightText);
        Palette[pcSelectionText] := ColorToRGB(clHighlightText);
        Palette[pcSelectionBG] := ColorToRGB(clHighlight);
        Palette[pcSelectionBG_Focused] := ColorToRGB(clHighlight);
      end;
    end;
    Palette[pcEditText_Ok] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextOk, 4210688);
    Palette[pcEditText_Warning] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextWarning, 19532);
    Palette[pcEditText_Alert] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextAlert, 108);
    Palette[pcEditText_Caution] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextAlert, 108);
    Palette[pcEditText_Bypassed] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextAlert, 108);
    Palette[pcEditBG_Ok] := SkinFile.ReadInteger(s_GlobalInfo, s_EditBGOk,
      $FFFFFF and slEditGreen);
    Palette[pcEditBG_Warning] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBGWarning, $FFFFFF and slEditYellow);
    Palette[pcEditBG_Alert] := SkinFile.ReadInteger(s_GlobalInfo, s_EditBGAlert,
      $FFFFFF and slEditRed);
    Palette[pcEditBG_Caution] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBGAlert, $FFFFFF and slEditRed);
    Palette[pcEditBG_Bypassed] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBGAlert, $FFFFFF and slEditRed);
    Palette[pcEditText_Highlight1] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextHighlight1, clBlack);
    Palette[pcEditText_Highlight2] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextHighlight2, clBlack);
    Palette[pcEditText_Highlight3] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditTextHighlight3, clBlack);
    Palette[pcEditText_Inverted] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditText_Inverted, Palette[pcEditBG]);
    Palette[pcEditBG_Inverted] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBG_Inverted, Palette[pcEditText]);
    Palette[pcEditBG_OddRow] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBG_OddRow, Palette[pcEditBG]);
    Palette[pcEditBG_EvenRow] := SkinFile.ReadInteger(s_GlobalInfo,
      s_EditBG_EvenRow, BlendColors(Palette[pcEditText],
      Palette[pcEditBG], 13));
    Palette[pcBtnColor1Active] := GetInt64(s_GlobalInfo, s_BtnColor1Active,
      AddAlpha(BlendColors(Palette[pcMainColor], $FFFFFF, 177)));
    Palette[pcBtnColor2Active] := GetInt64(s_GlobalInfo, s_BtnColor2Active,
      AddAlpha(Palette[pcMainColor]));
    Palette[pcBtnBorderActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnBorderActive, Palette[pcBorder]);
    Palette[pcBtnFontActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnFontActive, Palette[pcLabelText]);
    Palette[pcBtnColor1Normal] := GetInt64(s_GlobalInfo, s_BtnColor1Normal,
      AddAlpha(Palette[pcMainColor]));
    Palette[pcBtnColor2Normal] := GetInt64(s_GlobalInfo, s_BtnColor2Normal,
      AddAlpha(BlendColors(Palette[pcMainColor], 0, 244)));
    Palette[pcBtnBorderNormal] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnBorderNormal, Palette[pcBorder]);
    Palette[pcBtnFontNormal] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnFontNormal, Palette[pcLabelText]);
    Palette[pcBtnColor1Pressed] := GetInt64(s_GlobalInfo, s_BtnColor1Pressed,
      AddAlpha(BlendColors(Palette[pcMainColor], 0, 233)));
    Palette[pcBtnColor2Pressed] := GetInt64(s_GlobalInfo, s_BtnColor2Pressed,
      AddAlpha(BlendColors(Palette[pcMainColor], 0, 233)));
    Palette[pcBtnBorderPressed] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnBorderPressed, Palette[pcBorder]);
    Palette[pcBtnFontPressed] := SkinFile.ReadInteger(s_GlobalInfo,
      s_BtnFontPressed, Palette[pcLabelText]);
    Palette[pcBtnToneRed] := SkinFile.ReadInteger(s_GlobalInfo, s_ColorToneRed,
      $FFFFFF and slBtnRed);
    Palette[pcBtnToneGreen] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneGreen, $FFFFFF and slBtnGreen);
    Palette[pcBtnToneBlue] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneBlue, $FFFFFF and slBtnBlue);
    Palette[pcBtnToneYellow] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneYellow, $FFFFFF and slBtnYellow);
    Palette[pcBtnToneRedActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneRedActive, Palette[pcBtnToneRed]);
    Palette[pcBtnToneGreenActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneGreenActive, Palette[pcBtnToneGreen]);
    Palette[pcBtnToneBlueActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneBlueActive, Palette[pcBtnToneBlue]);
    Palette[pcBtnToneYellowActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorToneYellowActive, Palette[pcBtnToneYellow]);
    Palette[pcBtnRedText] := SkinFile.ReadInteger(s_GlobalInfo, s_ColorRedText,
      $FFFFFF and slBtnRedText);
    Palette[pcBtnGreenText] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorGreenText, $FFFFFF and slBtnGreenText);
    Palette[pcBtnBlueText] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorBlueText, $FFFFFF and slBtnBlueText);
    Palette[pcBtnYellowText] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorYellowText, $FFFFFF and slBtnYellowText);
    Palette[pcBtnRedTextActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorRedTextActive, Palette[pcBtnRedText]);
    Palette[pcBtnGreenTextActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorGreenTextActive, Palette[pcBtnGreenText]);
    Palette[pcBtnBlueTextActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorBlueTextActive, Palette[pcBtnBlueText]);
    Palette[pcBtnYellowTextActive] := SkinFile.ReadInteger(s_GlobalInfo,
      s_ColorYellowTextActive, Palette[pcBtnYellowText]);
    Palette[pcGrid] := SkinFile.ReadInteger(s_GlobalInfo, s_EditGridLine, -1);
    if Palette[pcGrid] = -1 then
      Palette[pcGrid] := BlendColors(Palette[pcEditText],
        Palette[pcEditBG], 30);
    // Change skin Colors if needed
    ChangeSkinHue(FOwner, HueOffset, aPPI);
    ChangeSkinSaturation(FOwner, Saturation);
    ChangeSkinBrightness(FOwner, FBrightness);
    for ColorItem := low(Brushes) to high(Brushes) do
    begin
      if Brushes[ColorItem] <> 0 then
        DeleteObject(Brushes[ColorItem]);
      Brushes[ColorItem] := CreateSolidBrush(Palette[ColorItem]);
    end;
  end;
end;

procedure TsSkinManager.InstallHook;
var
  dwThreadID: DWORD;
begin
  if not(csDesigning in ComponentState) and (DefaultManager = Self) then
    if not GlobalHookInstalled then
    begin
      GlobalHookInstalled := True;
      if acSupportedList = nil then
        acSupportedList := TList.Create;
      dwThreadID := GetCurrentThreadId;
      HookCallback := SetWindowsHookEx(WH_CBT, SkinHookCBT, 0, dwThreadID);
      if Options.ChangeSysColors then
        UpdateColorsHooks(True);
    end;
end;

procedure TsSkinManager.UnInstallHook;
var
  i: integer;
begin
  if not(csDesigning in ComponentState) and (DefaultManager = Self) then
    if GlobalHookInstalled then
    begin
      ClearMnuArray;
      if HookCallback <> 0 then
        UnhookWindowsHookEx(HookCallback);
      if acSupportedList <> nil then
      begin
        for i := 0 to acSupportedList.Count - 1 do
          if acSupportedList[i] <> nil then
            TObject(acSupportedList[i]).Free;
        FreeAndNil(acSupportedList);
      end;
      if Options.ChangeSysColors then
        UpdateColorsHooks(False);
      GlobalHookInstalled := False;
      HookCallback := 0;
    end;
end;

procedure TsSkinManager.SetSkinningRules(const Value: TacSkinningRules);
begin
  FSkinningRules := Value;
{$IFDEF D2007}
  UpdateCommonDlgs(Self);
{$ENDIF}
end;

procedure TsSkinManager.SetExtBordersMode(const Value: TacExtBordersMode);
var
  i: integer;
begin
  if FExtBordersMode <> Value then
  begin
    FExtBordersMode := Value;
    if IsActive then
    begin
      ManagerState := ManagerState + [msSkinChanging];
      // ReloadSkin;
      // CommonSkinData.Init;
      for i := 0 to Length(SkinDataArray) - 1 do
        if SkinDataArray[i] <> nil then
        begin
          SkinDataArray[i].InitConstantIndexes;
          SkinDataArray[i].InitMaskParams;
          SkinDataArray[i].CheckShadows;
        end;
      ManagerState := ManagerState - [msSkinChanging];
      if [csLoading, csReading, csDesigning] * ComponentState = [] then
        RepaintForms;
    end;
  end;
end;

procedure TsSkinManager.SetExtendedBorders(const Value: boolean);
var
  i: integer;
begin
  if FExtendedBorders <> Value then
  begin
    FExtendedBorders := Value;
    if IsActive then
    begin
      ManagerState := ManagerState + [msSkinChanging];
      for i := 0 to Length(SkinDataArray) - 1 do
        if SkinDataArray[i] <> nil then
        begin
          // SkinDataArray[i].InitMaskParams;
          SkinDataArray[i].CheckShadows;
        end;
      ManagerState := ManagerState - [msSkinChanging];
      if [csLoading, csReading, csDesigning] * ComponentState = [] then
        RepaintForms;
    end;
  end;
end;

procedure TsSkinManager.ChangeAppFont(const FontName: string;
  DoLockForms: boolean = True);
var
  M: TMessage;
begin
  if not(csDesigning in ComponentState) then
    if DefOldFontName <> FontName then
    begin
      ManagerState := ManagerState + [msFontChanging];
      if DoLockForms then
        LockForms(Self);

      DefNewFontName := FontName;
      MenuSupport.CustomFont := MenuSupport.CustomFont; // Init menu font
      StoreDefFont;

      M := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_FONTSCHANGED),
        LPARAM(Self), 0);
      AppBroadCastS(M);
      if Assigned(FOnFontChanged) then
        OnFontChanged(Self, DefOldFontName, FontName);

      DefOldFontName := FontName;
      if DoLockForms then
        UnLockForms(Self, not NoAutoUpdate);

      ManagerState := ManagerState - [msFontChanging];
    end;
end;

procedure TsSkinManager.CheckVersion;
{$IFNDEF ASKINEDITOR}
var
  b: boolean;
{$ENDIF}
begin
  if SkinCommonInfo.Version < CompatibleSkinVersion then
  begin
    if csDesigning in ComponentState then
      ShowMessage('You are using an old version of the "' + SkinName +
        '" skin (v' + FloatToStrF(SkinCommonInfo.Version, ffFixed, 6, 2) + '). '
        + 'Please, update skins to latest or contact the AlphaControls support for upgrading of existing skin.'
        + s_0D0A + s_0D0A +
        'This notification occurs in design-time only for your information and will not be shown in real-time.')
  end
{$IFNDEF ASKINEDITOR}
  else if (SkinCommonInfo.Version > MaxCompSkinVersion) and
    not((ParamCount > 0) and (ParamStr(1) = s_PreviewKey)) then
  begin
    b := srStdDialogs in SkinningRules;
    SkinningRules := SkinningRules - [srStdDialogs];
    MessageDlg('This version of the skin (v' +
      FloatToStrF(SkinCommonInfo.Version, ffFixed, 6, 2) +
      ') has not complete support by used AlphaControls package release (v' +
      acCurrentVersion + ').' + s_0D0A +
      'Components must be updated to latest version for using this skin.',
      mtWarning, [mbOk], 0);
    if b then
      SkinningRules := SkinningRules + [srStdDialogs];
  end;
{$ENDIF}
end;

function TsSkinManager.GetExtendedBorders: boolean;
begin
  Result := FExtendedBorders and Assigned(UpdateLayeredWindow);
end;

procedure TsSkinManager.BeginUpdate;
begin
  NoAutoUpdate := True;
end;

procedure TsSkinManager.EndUpdate(Repaint: boolean = False;
  AllowAnimation: boolean = True);
var
  b: boolean;
begin
  NoAutoUpdate := False;
  if AllowAnimation then
    UpdateSkin(Repaint)
  else
  begin
    b := AnimEffects.SkinChanging.Active;
    AnimEffects.SkinChanging.Active := False;
    UpdateSkin(Repaint);
    AnimEffects.SkinChanging.Active := b;
  end;
  if Assigned(FOnAfterChange) then
    FOnAfterChange(Self);
end;

procedure TsSkinManager.SetActiveGraphControl(const Value: TGraphicControl);
var
  sd: TsCommonData;
  OldControl: TGraphicControl;
begin
  if not(csDestroying in Owner.ComponentState) and (Value <> FActiveGraphControl)
  then
  begin
    // Check if graph control is Hot still
    if (Value = nil) and (FActiveGraphControl <> nil) then
    begin
      sd := TsCommonData(FActiveGraphControl.Perform(SM_ALPHACMD,
        AC_GETSKINDATA_HI, 0));
      if (sd <> nil) and sd.FMouseAbove then
        Exit;
    end;
    OldControl := FActiveGraphControl;
    FActiveGraphControl := Value;
    if (OldControl <> nil) and not(csDestroying in OldControl.ComponentState)
    then
      OldControl.Perform(SM_ALPHACMD, AC_MOUSELEAVE_HI, LPARAM(Self));
  end;
end;

procedure TsSkinManager.UpdateAllScale;
var
  i: integer;
begin
  i := 0;
  TAccessMenus(SkinableMenus).sd := nil;

  if Length(SkinDataArray) > 0 then
    IterateImageLists(ScaleImageList, SkinDataArray[0].PPI);

  while i < Length(HookedComponents) do
  begin
    if (HookedComponents[i] is TForm) then
      UpdateScale(TForm(HookedComponents[i]));

    inc(i);
  end;
end;

procedure TsSkinManager.UpdateCurrentSkin;
begin
  if FActive and ([csLoading, csReading] * ComponentState = []) and
    ([msSkinChanging] * ManagerState = []) then
  begin
    ManagerState := ManagerState + [msSkinChanging];
    ClearSkinData;
    InitCommonData;
    if not(msScaleChanging in ManagerState) then
      SendNewSkin;

    ManagerState := ManagerState - [msSkinChanging];
    if not NoAutoUpdate then
    begin
      if Assigned(FOnAfterChange) then
        FOnAfterChange(Self);

      if not(msScaleChanging in ManagerState) then
        RepaintForms;
    end;
  end
end;

function TsSkinManager.UpdateFontName(Ctrl: TControl): boolean;

  function DoChange(const NewFont: string): boolean;
  begin
    if ((TacAccessControl(Ctrl).Parent = nil) or not TacAccessControl(Ctrl)
      .ParentFont) and ((TacAccessControl(Ctrl).Font.Name = DefOldFontName) or
      (TacAccessControl(Ctrl).Font.Name = DefStoredFontName)) then
      if Ctrl is TWinControl then
      begin
        if TWinControl(Ctrl).HandleAllocated then
        begin
          TacAccessControl(Ctrl).Font.Name := NewFont;
          Result := True;
        end
        else
          Result := False
      end
      else if Ctrl.Parent <> nil then
      begin
        TacAccessControl(Ctrl).Font.Name := NewFont;
        Result := True;
      end
      else
        Result := False
    else
      Result := False;
  end;

begin
  Result := DoChange(iff(IsActive, DefNewFontName, DefStoredFontName))
end;

procedure TsStoredSkins.Assign(Source: TPersistent);
begin
  //
end;

constructor TsStoredSkins.Create(AOwner: TsSkinManager);
begin
  inherited Create(TsStoredSkin);
  FOwner := AOwner;
end;

destructor TsStoredSkins.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsStoredSkins.GetItem(Index: integer): TsStoredSkin;
begin
  Result := TsStoredSkin(inherited GetItem(Index))
end;

function TsStoredSkins.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TsStoredSkins.IndexOf(const SkinName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if GetItem(i).Name = SkinName then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TsStoredSkins.SetItem(Index: integer; Value: TsStoredSkin);
begin
  inherited SetItem(Index, Value);
end;

procedure TsStoredSkins.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOwner.OnSkinListChanged) then
    FOwner.OnSkinListChanged(FOwner);
end;

procedure TsStoredSkin.Assign(Source: TPersistent);
var
  Src: TsStoredSkin;
begin
  if Source = nil then
    inherited
  else
  begin
    Src := TsStoredSkin(Source);
    PackedData.LoadFromStream(Src.PackedData);
    FMasterBitmap.Assign(Src.MasterBitmap);
    FName := Src.Name;
    FAuthor := Src.Author;
    FVersion := Src.Version;
    FBorderColor := Src.BorderColor;
    FDescription := Src.Description;
    FShadow1Blur := Src.Shadow1Blur;
    FShadow1Color := Src.Shadow1Color;
    FShadow1Offset := Src.Shadow1Offset;
    FShadow1Transparency := Src.Shadow1Transparency;
  end;
end;

constructor TsStoredSkin.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMasterBitmap := TBitmap.Create;
  PackedData := TMemoryStream.Create;
  FShadow1Color := clBlack;
  FShadow1Offset := 0;
  FShadow1Blur := -1;
  FShadow1Transparency := 0;
  FBorderColor := clFuchsia;
  if TsStoredSkins(Collection).FOwner.SkinListController.Thread <> nil then
    if not TsStoredSkins(Collection).FOwner.SkinListController.Thread.Terminated
    then
      TsStoredSkins(Collection).FOwner.SkinListController.Thread.Terminate;
  // Finish the thread

  TsStoredSkins(Collection).FOwner.SkinListController.UpdateNeeded := True;
end;

procedure TsStoredSkin.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
  Filer.DefineProperty('OriginName', ReadOriginName, WriteOriginName, True);
end;

destructor TsStoredSkin.Destroy;
begin
  FreeAndNil(FMasterBitmap);
  FreeAndNil(PackedData);
  inherited Destroy;
end;

procedure TsStoredSkin.ReadData(Reader: TStream);
begin
  PackedData.LoadFromStream(Reader);
end;

procedure TsStoredSkin.ReadOriginName(Reader: TReader);
begin
  OrigName := Reader.ReadString;
end;

procedure TsStoredSkin.WriteData(Writer: TStream);
begin
  PackedData.SaveToStream(Writer);
end;

procedure TsStoredSkin.WriteOriginName(Writer: TWriter);
begin
  Writer.WriteString(OrigName);
end;

constructor TacAnimEffects.Create(AOwner: TsSkinManager);
begin
  Manager := AOwner;
  FBlendOnMoving := TacBlendOnMoving.Create;
  FButtons := TacBtnEffects.Create(Manager);
  FDialogShow := TacDialogShow.Create;
  FFormShow := TacFormShow.Create;
  FFormHide := TacFormHide.Create;
  FDialogHide := TacFormHide.Create;
  FMinimizing := TacMinimizing.Create;
  FPageChange := TacPageChange.Create;
  FSkinChanging := TacSkinChanging.Create;
end;

destructor TacAnimEffects.Destroy;
begin
  FreeAndNil(FBlendOnMoving);
  FreeAndNil(FButtons);
  FreeAndNil(FDialogShow);
  FreeAndNil(FFormShow);
  FreeAndNil(FFormHide);
  FreeAndNil(FDialogHide);
  FreeAndNil(FMinimizing);
  FreeAndNil(FPageChange);
  FreeAndNil(FSkinChanging);
  inherited;
end;

constructor TacBtnEffects.Create(AOwner: TsSkinManager);
begin
  FEvents := [beMouseEnter, beMouseLeave, beMouseDown, beMouseUp];
  FLightingMode := lmActiveControl;
  // FClickEffect := ceCicrles;
  Manager := AOwner;
end;

constructor TacFormAnimation.Create;
begin
  FActive := True;
  FTime := 0;
  FMode := atAero;
end;

constructor TacPageChange.Create;
begin
  FActive := True;
  FTime := 0;
  FMode := atcFade;
end;

constructor TacDialogShow.Create;
begin
  inherited;
  FTime := 0;
end;

constructor TacSkinChanging.Create;
begin
  inherited;
  FTime := 100;
  FMode := atFading
end;

function TsThirdPartyList.GetString(const Index: integer): string;
begin
  case Index of
    ord(tpEdit):
      Result := FThirdEdits;
    ord(tpButton):
      Result := FThirdButtons;
    ord(tpBitBtn):
      Result := FThirdBitBtns;
    ord(tpCheckBox):
      Result := FThirdCheckBoxes;
    ord(tpGroupBox):
      Result := FThirdGroupBoxes;
    ord(tpListView):
      Result := FThirdListViews;
    ord(tpPanel):
      Result := FThirdPanels;
    ord(tpGrid):
      Result := FThirdGrids;
    ord(tpTreeView):
      Result := FThirdTreeViews;
    ord(tpComboBox):
      Result := FThirdComboBoxes;
    ord(tpWWEdit):
      Result := FThirdWWEdits;
    ord(tpVirtualTree):
      Result := FThirdVirtualTrees;
    ord(tpGridEh):
      Result := FThirdGridEh;
    ord(tpPageControl):
      Result := FThirdPageControl;
    ord(tpTabControl):
      Result := FThirdTabControl;
    ord(tpToolBar):
      Result := FThirdToolBar;
    ord(tpStatusBar):
      Result := FThirdStatusBar;
    ord(tpSpeedButton):
      Result := FThirdSpeedButton;
    ord(tpScrollControl):
      Result := FThirdScrollControl;
    ord(tpUpDownBtn):
      Result := FThirdUpDownBtn;
    ord(tpScrollBar):
      Result := FThirdScrollBar;
    ord(tpStaticText):
      Result := FThirdStaticText;
    ord(tpNativePaint):
      Result := FThirdNativePaint;
{$IFDEF ADDWEBBROWSER}
    ord(tpWebBrowser):
      Result := FThirdWebBrowser;
{$ENDIF}
  end
end;

procedure TsThirdPartyList.SetString(const Index: integer; const Value: string);
begin
  case Index of
    ord(tpEdit):
      FThirdEdits := Value;
    ord(tpButton):
      FThirdButtons := Value;
    ord(tpBitBtn):
      FThirdBitBtns := Value;
    ord(tpCheckBox):
      FThirdCheckBoxes := Value;
    ord(tpGroupBox):
      FThirdGroupBoxes := Value;
    ord(tpListView):
      FThirdListViews := Value;
    ord(tpPanel):
      FThirdPanels := Value;
    ord(tpGrid):
      FThirdGrids := Value;
    ord(tpTreeView):
      FThirdTreeViews := Value;
    ord(tpComboBox):
      FThirdComboBoxes := Value;
    ord(tpWWEdit):
      FThirdWWEdits := Value;
    ord(tpVirtualTree):
      FThirdVirtualTrees := Value;
    ord(tpGridEh):
      FThirdGridEh := Value;
    ord(tpPageControl):
      FThirdPageControl := Value;
    ord(tpTabControl):
      FThirdTabControl := Value;
    ord(tpToolBar):
      FThirdToolBar := Value;
    ord(tpStatusBar):
      FThirdStatusBar := Value;
    ord(tpSpeedButton):
      FThirdSpeedButton := Value;
    ord(tpScrollControl):
      FThirdScrollControl := Value;
    ord(tpUpDownBtn):
      FThirdUpDownBtn := Value;
    ord(tpScrollBar):
      FThirdScrollBar := Value;
    ord(tpStaticText):
      FThirdStaticText := Value;
    ord(tpNativePaint):
      FThirdNativePaint := Value;
{$IFDEF ADDWEBBROWSER}
    ord(tpWebBrowser):
      FThirdWebBrowser := Value;
{$ENDIF}
  end
end;

constructor TacSkinEffects.Create;
begin
  FAllowGlowing := True;
  FAllowAnimation := True;
  FAllowAeroBluring := True;
  FAllowOuterEffects := True;
end;

procedure TacSkinEffects.SetAllowAnimation(const Value: boolean);
begin
  FAllowAnimation := Value;
  if Manager.IsDefault and not(csDesigning in Manager.ComponentState) then
    SetMouseLight(Value and
      (Manager.AnimEffects.Buttons.LightingMode = lmAffectNearest))
end;

procedure TacSkinEffects.SetBoolean(const Index: integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then
    begin
      Prop := Value;
      if not(csLoading in Manager.ComponentState) and not Manager.NoAutoUpdate
      then
        Manager.RepaintForms(False);
    end;
  end;

begin
  case Index of
    0:
      ChangeProp(FAllowOuterEffects, Value);
    1:
      ChangeProp(FDiscoloredGlyphs, Value);
  end;
end;

constructor TacMinimizing.Create;
begin
  inherited;
  FTime := 80;
end;

constructor TacBlendOnMoving.Create;
begin
  inherited;
  FActive := False;
  FBlendValue := 170;
  Time := 1000;
end;

constructor TacScrollBarsSupport.Create;
begin
  FButtonsSize := -1;
  FScrollSize := -1;
  FOwner := AOwner;
end;

procedure TacScrollBarsSupport.SetInteger(const Index, Value: integer);
var
  M: TMessage;
begin
  case Index of
    0:
      if FButtonsSize <> Value then
      begin
        FButtonsSize := Value;
        if not(csLoading in FOwner.ComponentState) then
          FOwner.RepaintForms(False);
      end;

    1:
      if FScrollSize <> Value then
      begin
        FScrollSize := Value;
        if not(csLoading in FOwner.ComponentState) then
        begin
          M := MakeMessage(SM_ALPHACMD, WParam(AC_REINITSCROLLS shl 16), 0, 0);
          AppBroadCastS(M);
        end;
      end;
  end;
end;

constructor TacButtonsSupport.Create(AOwner: TsSkinManager);
begin
  FOwner := AOwner;
  FShowFocusRect := False;
  FShiftContentOnClick := True;
  FAllowRTLSysButtons := True;
  FModalButtonsColoring := [bcGlyph];
end;

constructor TacLabelsSupport.Create;
begin
  FTransparentAlways := True;
end;

function TacOptions.ActualArrowStyle: TacArrowsStyle;
begin
  if FArrowsStyle = arsDefault then
    Result := FOwner.SkinCommonInfo.ArrowStyle
  else
    Result := FArrowsStyle;
end;

constructor TacOptions.Create(AOwner: TsSkinManager);
begin
  FOwner := AOwner;
  FCheckEmptyAlpha := False;
  FNoMouseHover := False;
  FNativeBordersMaximized := False;
  FStdGlyphsOrder := False;
  FChangeSysColors := False;
  FDrawNonClientArea := True;
  FScaleMode := DefScaleMode;
  FPixelsPerInch := 96;
  FOptimizingPriority := opSpeed;
  FArrowsStyle := arsDefault;
  FGroupBoxStyle := gsStandard;
  FRotateArrows := False;
end;

const
  LF_UNLOCK = 1;
  LF_REDUCED = 2;
  LF_RECREATE = 4;

procedure LockFormUpdate(sp: TComponent; Data: integer);
begin
  with TsSkinProvider(sp) do
  begin
    if Data and LF_UNLOCK = 0 then
    begin // If must be locked
      FormState := FFormState or FS_FULLPAINTING;
      if BorderForm <> nil then
        TsSkinProvider(sp).BorderForm.UpdateExBordersPos(False);
      FInAnimation := Data and LF_UNLOCK = 0;
      FormState := FFormState or FS_LOCKED;
    end
    else
    begin // If must be unlocked
      FormState := FFormState and not FS_LOCKED and not FS_FULLPAINTING;
      SkinData.BGChanged := True;
      SendAMessage(TsSkinProvider(sp).Form.Handle, AC_PREPARECACHE);
      FormState := 0;
    end;
    if BorderForm <> nil then
      BorderForm.ExBorderShowing := Data and LF_UNLOCK = 0;

    if Form.BorderStyle <> bsNone then
      TrySendMessage(Form.Handle, WM_SETREDRAW, Data and LF_UNLOCK, 0);

    if SkinData.SkinIndex < 0 then
      RedrawWindow(Form.Handle, nil, 0, RDWA_ALLNOW)
    else
      // if SkinData.TranspMode > 0 then
      // IterateControls(Form, 0, RepaintOpaque);
  end;
end;

procedure TacOptions.DoScaleChanged(NewScaleMode: TacScaleMode;
  ReduceSize: boolean);
var
  Data: integer;
  CanUpdate: boolean;
begin
  with FOwner do
  begin
    CanUpdate := not NoAutoUpdate;
    BeginUpdate;
    IterateForms(FOwner, LockFormUpdate, LF_REDUCED * integer(ReduceSize));
    Data := LF_UNLOCK or LF_REDUCED * integer(ReduceSize);
    if (FScaleMode <> NewScaleMode) and
      ((FScaleMode = smVCL) or (NewScaleMode = smVCL)) and
      not(msAnimScaling in FOwner.ManagerState) then
      Data := Data or LF_RECREATE;
    FScaleMode := NewScaleMode;
    // Change it after form locking for having of old sizes of imagelists
    UpdateCurrentSkin;
    UpdateAllScale;
    IterateForms(FOwner, LockFormUpdate, Data);
    if CanUpdate then
      EndUpdate(True, False);
    if Assigned(FOnScaleChanged) then
      FOnScaleChanged(Self);
    FOwner.ManagerState := FOwner.ManagerState -
      [msScaleChanging, msAnimScaling];
  end;
end;

function TacOptions.GetBool(const Index: integer): boolean;
begin
  case Index of
    0:
      Result := FCheckEmptyAlpha;
    1:
      Result := FNoMouseHover;
    2:
      Result := FNativeBordersMaximized;
    3:
      Result := FStdGlyphsOrder;
    5:
      Result := FStdImgTransparency;
    6:
      Result := FDrawNonClientArea
  else
    Result := FChangeSysColors; // 4
  end;
end;

function TacOptions.GetScaleMode: TacScaleMode;
begin
{$IFNDEF AUTOVCL}
  Result := FScaleMode;
{$ELSE}
  if FScaleMode = smVCL then
    Result := smAuto
  else
    Result := FScaleMode;
{$ENDIF}
end;

procedure TacOptions.SetArrowsStyle(const Value: TacArrowsStyle);
begin
  if FArrowsStyle <> Value then
  begin
    FArrowsStyle := Value;
    FOwner.UpdateSkin;
  end;
end;

procedure TacOptions.SetBool(const Index: integer; const Value: boolean);
var
  M: TMessage;
begin
  case Index of
    0:
      FCheckEmptyAlpha := Value;
    1:
      FNoMouseHover := Value;
    2:
      FNativeBordersMaximized := Value;
    3:
      FStdGlyphsOrder := Value;
    5:
      FStdImgTransparency := Value;
    6:
      if FDrawNonClientArea <> Value then
      begin
        FDrawNonClientArea := Value;
        if not(csDesigning in FOwner.ComponentState) then
        begin
          M.Msg := SM_ALPHACMD;
          M.WParam := MakeWParam(0, AC_SETNCSKINNED);
          M.LPARAM := integer(Value);
          SendToHooked(M);
        end;
      end;
    4:
      begin
        FChangeSysColors := Value;
        if FOwner.IsDefault and
          ([csDesigning, csLoading] * FOwner.ComponentState = []) and FOwner.IsActive
        then
          UpdateColorsHooks(Value);
      end;
  end;
end;

procedure TacOptions.SetGroupBoxStyle(const Value: TacGroupBoxStyle);
begin
  if FGroupBoxStyle <> Value then
  begin
    FGroupBoxStyle := Value;
    if (FOwner <> nil) and
      ([csLoading, csDesigning] * FOwner.ComponentState = []) then
      FOwner.RepaintForms;
  end;
end;

procedure TacOptions.SetNewScaleMode(OldScaleMode, NewScaleMode: TacScaleMode;
  OldPPI, NewPPI: integer);
var
  ReduceSize: boolean;
  OldPercent, NewPercent: integer;
  ScaleChangeData: TScaleChangeData;

  procedure UpdateVariables;
  begin
    if FOwner.IsDefault and not(csDesigning in FOwner.ComponentState) then
    begin
{$IFDEF D2010}
      Screen.MessageFont.Size := MulDiv(Screen.MessageFont.Size, NewPercent,
        OldPercent);
{$ENDIF}
    end;
  end;

begin
{$IFDEF AUTOVCL}
  if NewScaleMode = smVCL then
    NewScaleMode := smAuto;
{$ENDIF}
  case NewScaleMode of
    smCustomPPI:
      begin
        case OldScaleMode of
          smCustomPPI:
            begin
              OldPPI := FPixelsPerInch;
              if NewPPI <> 0 then
                FPixelsPerInch := NewPPI
              else
                NewPPI := FPixelsPerInch;
              OldPercent := MulDiv(100, OldPPI, 96);
              NewPercent := MulDiv(100, NewPPI, 96);
            end
        else
          begin
            if OldScaleMode = smVCL then
              OldPercent := aScalePercents[PPIToIndex(DefaultPPI)]
            else
              OldPercent := aScalePercents[FOwner.GetScale];
            OldPPI := MulDiv(96, OldPercent, 100);
            NewPPI := FPixelsPerInch;
            NewPercent := MulDiv(100, NewPPI, 96);
          end;
        end;
      end
  else
    begin
      case ScaleMode of
        smVCL:
          OldPercent := aScalePercents[PPIToIndex(DefaultPPI)];
        smCustomPPI:
          OldPercent := MulDiv(100, PixelsPerInch, 96)
      else
        OldPercent := aScalePercents[FOwner.GetScale];
      end;
      case NewScaleMode of
        smVCL:
          begin
            case FScaleMode of
              smCustomPPI:
                if Application.MainForm <> nil then
                  PixelsPerInch := GetControlPPI(Application.MainForm)
                else
                  PixelsPerInch := aPPIArray[FOwner.SysFontScale];
              sm100, sm125, sm150, sm200:
                begin
                  if Application.MainForm <> nil then
                    PixelsPerInch := GetControlPPI(Application.MainForm)
                  else
                    PixelsPerInch := aPPIArray[FOwner.SysFontScale];
                  ScaleMode := smCustomPPI;
                end;
            end;
            NewPercent := aScalePercents[min(3, FOwner.SysFontScale)];
          end;
        smOldMode:
          begin
            NewPercent := 100;
            NewPPI := 96;
          end;
        smAuto:
          begin
            NewPercent := aScalePercents[min(3, FOwner.SysFontScale)];
            NewPPI := aPPIArray[min(3, FOwner.SysFontScale)];
          end
      else
        begin
          NewPercent := aScalePercents[min(3, ord(NewScaleMode))];
          NewPPI := aPPIArray[min(3, ord(NewScaleMode))];
        end;
      end;
    end;
  end;
  ReduceSize := NewPercent < OldPercent;
  FOwner.ManagerState := FOwner.ManagerState + [msScaleChanging];
  if Assigned(FOwner.OnScaleModeChange) and
    not(csDesigning in FOwner.ComponentState) then
  begin
    ScaleChangeData.OldScaleMode := OldScaleMode;
    ScaleChangeData.NewScaleMode := NewScaleMode;
    ScaleChangeData.OldScalePercent := OldPercent;
    ScaleChangeData.NewScalePercent := NewPercent;
    ScaleChangeData.OldPPI := OldPPI;
    ScaleChangeData.NewPPI := NewPPI;
    FOwner.OnScaleModeChange(FOwner, ScaleChangeData);
  end;
  if NewScaleMode <> smVCL then
    FPixelsPerInch := NewPPI;

  with FOwner do
    if ComponentState * [csLoading, csDesigning] = [] then
      DoScaleChanged(NewScaleMode, ReduceSize)
    else
    begin
      FScaleMode := NewScaleMode;
      UpdateVariables;
      FOwner.ManagerState := FOwner.ManagerState - [msScaleChanging];
    end;
end;

procedure TacOptions.SetPixelsPerInch(const Value: TacPixelsPerInch);
begin
  if FPixelsPerInch <> Value then
    if FScaleMode = smCustomPPI then
      SetNewScaleMode(smCustomPPI, smCustomPPI, FPixelsPerInch, Value)
    else
      FPixelsPerInch := Value;
end;

procedure TacOptions.SetScaleMode(const Value: TacScaleMode);
begin
{$IFNDEF AUTOVCL}
  if FScaleMode <> Value then
{$ENDIF}
    SetNewScaleMode(FScaleMode, Value, 0, 0);

  if FOwner.IsDefault then
    acCurrentScaleMode := Value;
end;

procedure TsSkinManager.ClearSkinArrays;
var
  i: integer;
begin
  for i := 0 to Length(SkinDataArray) - 1 do
    if SkinDataArray[i] <> nil then
      SkinDataArray[i].Free;

  SetLength(SkinDataArray, 0);
end;

procedure TsSkinManager.CloseSkinFile;
begin
  if Assigned(sc) then
    FreeAndNil(sc);
end;

function TsSkinManager.CommonSkinData(aPPI: integer = 96): TacSkinData;
var
  l, ndx: integer;
  CanInit: boolean;
begin
  if ((Options <> nil) and (Options.ScaleMode = smVCL) { or not IsActive } ) and
    not(csDesigning in ComponentState) then
    ndx := PPIToIndex(aPPI)
  else
    ndx := 0;

  if csDesigning in ComponentState then
    aPPI := 96;

  if IsActive or ([msSkinChanging, msBroadCasting] * ManagerState <> []) then
  begin

    l := Length(SkinDataArray);
    if l <= ndx then
    begin
      if (l = 0) and (aPPI <> 96) and (Options.ScaleMode = smVCL) then
        CommonSkinData(96);

      SetLength(SkinDataArray, ndx + 1);
    end;
    if SkinDataArray[ndx] = nil then
    begin
      SkinDataArray[ndx] := TacSkinData.Create(Self, aPPI);
      Result := SkinDataArray[ndx];
      if Options.ScaleMode = smVCL then
        Result.ScaleValue := ndx
      else
        Result.ScaleValue := PPIToIndex(aPPI);

      CanInit := (aPPI <> 96) and (Length(SkinDataArray[0].gd) > 0) or
        ((msSkinChanging in ManagerState) and (ndx = 0) and
        (Length(SkinDataArray[0].gd) = 0));
    end
    else
    begin
      Result := SkinDataArray[ndx];
      CanInit := (msSkinChanging in ManagerState) and
        not Result.Initializing and (Length(SkinDataArray[0].gd) = 0);
    end;
    if CanInit then
      Result.Init;
  end
  else
  begin
    Result := StdScaleArray[ndx];
    if StdScaleArray[ndx].PPI <> aPPI then
    begin
      StdScaleArray[ndx].PPI := aPPI;
      StdScaleArray[ndx].ScalePercent := MulDiv(100, aPPI, 96);
    end;
  end;
end;

function TsSkinManager.ScaleInt(Value: integer; SysScale: integer = -1)
  : integer;
var
  iScale: integer;
begin
  if Options.ScaleMode = smCustomPPI then
    Result := MulDiv(Value, Options.PixelsPerInch, 96)
  else
  begin
    case Options.ScaleMode of
      smOldMode:
        iScale := 0;
      smVCL:
        if SysScale = -1 then
          iScale := GetScale // 0
        else
          iScale := SysScale
    else
      if SysScale = -1 then
        iScale := GetScale
      else
        iScale := SysScale;
    end;
    Result := ScaleInteger(Value, iScale);
  end;
end;

function TsSkinManager.ScalePPI(Value, aPPI: integer): integer;
begin
  case Options.ScaleMode of
    smOldMode:
      aPPI := 0;
    smVCL:
      begin
        if aPPI = 0 then
          aPPI := DefaultPPI;
      end
  else
    if aPPI = 0 then
      aPPI := GetScale
  end;
  Result := MulDiv(Value, aPPI, 96);
end;

procedure TacSkinConvertor.Clear;
begin
  while Length(Files) > 0 do
  begin
    Files[Length(Files) - 1].Stream.Free;
    SetLength(Files, Length(Files) - 1);
  end;
  if Options <> nil then
    FreeAndNil(Options);

  if PackedData <> nil then
    FreeAndNil(PackedData);
end;

destructor TacSkinConvertor.Destroy;
begin
  Clear;
  inherited;
end;

function TryStrTo64(const s: AnsiString; out l: Int64): boolean;
var
  i: integer;
begin
  l := 0;
  Result := True;
  for i := 1 to Length(s) do
    if s[i] in ['0' .. '9'] then
      l := 10 * l + ord(s[i]) - ord('0')
    else
    begin
      Result := False;
      Break;
    end;
end;

function ExtractPackedData(var Convertor: TacSkinConvertor; pwds: TStringList;
  SkinManager: TComponent): boolean;
var
  s, sExt: AnsiString;
  l, i, c, fCount, R: integer;
  cArray: TacCharArray;
  KeysArray: array of Int64;
  decompr: TZDecompressionStream;
  bStdDialogs, bSysColors: boolean;

  procedure DisableManager;
  begin
    with TsSkinManager(SkinManager) do
    begin
      bStdDialogs := srStdDialogs in SkinningRules;
      SkinningRules := SkinningRules - [srStdDialogs];
      bSysColors := Options.ChangeSysColors;
      Options.ChangeSysColors := False;
    end;
  end;

  procedure EnableManager;
  begin
    with TsSkinManager(SkinManager) do
    begin
      if bStdDialogs then
        SkinningRules := SkinningRules + [srStdDialogs];

      Options.ChangeSysColors := bSysColors;
    end;
  end;

begin
  // Check file type
  if pwds <> nil then
  begin
    SetLength(KeysArray, pwds.Count);
    for i := 0 to pwds.Count - 1 do
    begin
      s := pwds[i];
      s := DelChars(s, #13);
      s := DelChars(s, #10);
      s := DelChars(s, ' ');
      if not TryStrTo64(s, KeysArray[i]) then
      begin
        KeysArray[i] := 0;
        DisableManager;
        MessageDlg('Secure key has incorrect format', mtError, [mbOk], 0);
        EnableManager;
      end;
    end;
  end
  else
    SetLength(KeysArray, 0);
  Convertor.PackedData.Seek(0, 0);
  c := 0;
  if SkinManager.Owner <> nil then
    for i := 1 to Length(SkinManager.Owner.Name) do
      inc(c, ord(SkinManager.Owner.Name[i]));
  Convertor.PackedData.Read(cArray, SizeOf(TacCharArray));
  DisableManager;
  if pwds <> nil then
    l := asSkinDecode(cArray, KeysArray, pwds.Count, c, fCount, R)
  else
    l := asSkinDecode(cArray, KeysArray, 0, c, fCount, R);
  if (pwds = nil) and (l > 0) then
  begin
    Result := False;
    Exit;
  end;
  EnableManager;
  case l of
    - 1:
      begin
        Result := False;
        DisableManager;
        MessageDlg('Loaded data is not a packed AlphaSkin file', mtError,
          [mbOk], 0);
        EnableManager;
        Exit;
      end
  else
    begin
{$IFDEF WIN64}
      if l = -3 then
      begin
        DisableManager;
        MessageDlg('Unregistered skin has been loaded.' + s_0D0A +
          'If you have a key for this skin, please insert it in the KeyList.',
          mtInformation, [mbOk], 0);
        EnableManager;
      end;
{$ENDIF}
      Convertor.PackedData.Seek(iff(l = 0, 8, 10), 0);
      // Extract all files
      Convertor.FileCount := fCount - 1;
      for i := 1 to fCount do
      begin
        // Get length of file name
        Convertor.PackedData.Read(l, SizeOf(l));
        // Extract file name
        SetLength(s, l);
        Convertor.PackedData.Read(s[1], l);
        // Get length of file
        Convertor.PackedData.Read(l, SizeOf(l));
        decompr := TZDecompressionStream.Create(Convertor.PackedData);
        // Extract file
        if UpperCase(s) = UpperCase('Options.dat') then
        begin
          Convertor.Options := TMemoryStream.Create;
          Convertor.Options.CopyFrom(decompr, l - R);
        end
        else
        begin
          SetLength(Convertor.Files, Length(Convertor.Files) + 1);
          with Convertor.Files[Length(Convertor.Files) - 1] do
          begin
            Name := s;
            sExt := UpperCase(ExtractFileExt(s));
            if sExt = '.BMP' then
              FileType := ftBmp
            else if (sExt = '.TTF') or (sExt = '.OTF') then
              FileType := ftFont
            else
              FileType := ftOther;
            Stream := TMemoryStream.Create;
            Stream.CopyFrom(decompr, l - R);
          end;
        end;
        FreeAndNil(decompr);
      end;
      Result := True;
    end;
  end;
end;

function GetPreviewStream(aStream: TMemoryStream; const SkinFileName: string)
  : boolean; overload;
var
  fs: TMemoryStream;
  Stream: TStream;
begin
  Result := False;
  if FileExists(SkinFileName) then
  begin
    fs := TMemoryStream.Create;
    Stream := TFileStream.Create(SkinFileName, fmOpenRead or fmShareDenyNone);
    try
      fs.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    try
      Result := GetPreviewStream(aStream, fs);
    finally
      fs.Free;
    end;
  end;
end;

function GetPreviewStream(aStream: TMemoryStream; SrcStream: TMemoryStream)
  : boolean; overload;
const
  sMagic: AnsiString = 'previewimg';
var
  l, position: integer;
  BufText: AnsiString;

  function BMSearch(StartPos: integer; const P, s: AnsiString): integer;
  type
    TBMTable = array [0 .. 255] of integer;
  var
    pos, lp, i: integer;
    BMT: TBMTable;
  begin
    for i := 0 to 255 do
      BMT[i] := Length(P);

    for i := Length(P) downto 1 do
      if BMT[byte(P[i])] = Length(P) then
        BMT[byte(P[i])] := Length(P) - i;

    lp := Length(P);
    pos := StartPos + lp - 1;
    while pos <= Length(s) do
      if P[lp] <> s[pos] then
        pos := pos + BMT[byte(s[pos])]
      else if lp = 1 then
      begin
        Result := pos;
        Exit;
      end
      else
        for i := lp - 1 downto 1 do
          if P[i] <> s[pos - lp + i] then
          begin
            inc(pos);
            Break;
          end
          else if i = 1 then
          begin
            Result := pos - lp + 1;
            Exit;
          end;

    Result := 0;
  end;

begin
  Result := False;
  SrcStream.Seek(0, 0);
  SetString(BufText, PAnsiChar(SrcStream.Memory), SrcStream.Size);
  position := BMSearch(0, sMagic, BufText);
  if position > 0 then
  begin
    SrcStream.Seek(position + 9, 0);
    SrcStream.Read(l, SizeOf(l));
    try
      aStream.Size := l;
      aStream.CopyFrom(SrcStream, l);
      aStream.Seek(0, 0);
    finally
      Result := True;
    end;
  end;
end;

function GetPreviewImage(aBitmap: TBitmap; const SkinFileName: string): boolean;
var
  ImgStream: TMemoryStream;
begin
  Result := False;
  aBitmap.Assign(nil);
  ImgStream := TMemoryStream.Create;
  try
    if GetPreviewStream(ImgStream, SkinFileName) then
      try
        aBitmap.LoadFromStream(ImgStream);
      finally
        Result := True;
      end;
  finally
    ImgStream.Free;
  end;
end;

var
  rst: TResourceStream = nil;

procedure TacSkinListController.AddControl(Ctrl: TControl);
// var
// DoUpdate: boolean;
begin
  // DoUpdate := (Length(Controls) = 0) and (Length(SkinList) = 0);
  if CtrlIndex(Ctrl) < 0 then
  begin
    SetLength(Controls, Length(Controls) + 1);
    Controls[Length(Controls) - 1] := Ctrl;
  end;
  Ctrl.Perform(SM_ALPHACMD, AC_SKINLISTCHANGED shl 16, 0);
  // if DoUpdate then
  // ClearList;
end;

procedure TacSkinListController.AddSkin(const AName: string; IntIndex: integer);
var
  l: integer;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    l := Length(SkinList);
    SetLength(SkinList, l + 1);
    SkinList[l].skName := AName;
    SkinList[l].skImageIndex := -1;
    if IntIndex < 0 then
    begin // External
      if GetPreviewStream(Stream, sPath + AName) then
        if TsAlphaImageList(ImgList).TryLoadFromPngStream(Stream) then
          SkinList[l].skImageIndex := TsAlphaImageList(ImgList).Items.Count - 1;

      Delete(SkinList[l].skName, pos(s_Dot + acSkinExt, LowerCase(AName)), 4);
      SkinList[l].skSkinMode := smExtPacked;
    end
    else
    begin
      if GetPreviewStream(Stream, SkinManager.InternalSkins[IntIndex].PackedData)
      then
        if TsAlphaImageList(ImgList).TryLoadFromPngStream(Stream) then
          SkinList[l].skImageIndex := TsAlphaImageList(ImgList).Items.Count - 1;

      SkinList[l].skSkinMode := smInternal;
    end;
  finally
    Stream.Free;
  end;
end;

function TacSkinListController.AddSkinData: integer;
begin
  if FileInfo.Name <> '' then
    try
      if FileInfo.Name[1] <> s_Dot then
        if (FileInfo.Attr and faDirectory = 0) and
          (ExtractFileExt(FileInfo.Name) = s_Dot + acSkinExt) then
          AddSkin(FileInfo.Name, -1);

      Result := acFindNext(FileInfo);
    finally
    end
  else
    Result := 1;
end;

const
  AC_SKINTHREADSTATUS = AC_ANIMSCALE + 10;

procedure TacSkinListController.CheckThread;
var
  M: TMessage;
begin
  if Thread <> nil then
    if not Thread.Terminated then
    begin
      Thread.Terminate; // Finish the thread
      try
        if SkinsAllowed(sfiInternal) then
          while CurrentInternalIndex < SkinManager.InternalSkins.Count do
          begin
            AddSkin(SkinManager.InternalSkins[CurrentInternalIndex].Name,
              CurrentInternalIndex);
            inc(CurrentInternalIndex);
          end;

        if SkinsAllowed(sfiExternal) then
          while DosCode = 0 do
            DosCode := AddSkinData;
      finally
        ClearFileInfo;
      end;
      UpdateNeeded := False;
      M := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_SKINTHREADSTATUS),
        0 { Finished } , 0);
      AppBroadCastS(M);
      SendListChanged;
    end
end;

procedure TacSkinListController.ClearFileInfo;
begin
  if FileInfo.Name <> '' then
  begin
    acFindClose(FileInfo);
    FileInfo.Name := '';
  end;
end;

procedure TacSkinListController.ClearList;
begin
  if (Thread <> nil) and not Thread.Terminated then
    Thread.Terminate;

  SetLength(SkinList, 0);
  TsAlphaImageList(ImgList).Clear;
  TsAlphaImageList(ImgList).Handle;
  if DoUpdateLater then
  begin
    UpdateNeeded := True;
    if DoSendChanged then
      SendListChanged;
  end;
end;

constructor TacSkinListController.Create(AOwner: TsSkinManager);
begin
  SkinManager := AOwner;
  ImgList := TsAlphaImageList.Create(AOwner);
  CurrentInternalIndex := 0;
  ImgList.Height := 100;
  ImgList.Width := 140;
  UpdateNeeded := True;
  TsAlphaImageList(ImgList).AllowScale := False;
  Thread := nil;
  DosCode := 0;
  FileInfo.Name := '';
end;

function TacSkinListController.CtrlIndex(Ctrl: TControl): integer;
var
  i: integer;
begin
  for i := 0 to Length(Controls) - 1 do
    if Controls[i] = Ctrl then
    begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

procedure TacSkinListController.DelControl(Ctrl: TControl);
var
  i: integer;
begin
  i := CtrlIndex(Ctrl);
  if i >= 0 then
    Controls[i] := nil;
end;

destructor TacSkinListController.Destroy;
begin
  if Thread <> nil then
  begin
    Thread.FreeOnTerminate := False;
    Thread.Free;
  end;
  ImgList.Free;
  ClearFileInfo;
  inherited;
end;

procedure TacSkinListController.SendListChanged;
var
  i: integer;
begin
  for i := 0 to Length(Controls) - 1 do
    if Controls[i] <> nil then
      Controls[i].Perform(SM_ALPHACMD, AC_SKINLISTCHANGED shl 16, 0);

  if Assigned(SkinManager.OnSkinListChanged) then
    SkinManager.OnSkinListChanged(Self);
end;

procedure TacSkinListController.SendSkinChanged;
var
  i: integer;
begin
  for i := 0 to Length(Controls) - 1 do
    if Controls[i] <> nil then
      if (Controls[i] is TWinControl) and TWinControl(Controls[i]).HandleAllocated
      then
        PostMessage(TWinControl(Controls[i]).Handle, SM_ALPHACMD,
          AC_SKINCHANGED shl 16, 0)
      else
        Controls[i].Perform(SM_ALPHACMD, AC_SKINCHANGED shl 16, 0);
end;

function TacSkinListController.SkinsAllowed
  (FiltType: TacSkinsFilterItem): boolean;
begin
  Result := (csDesigning in SkinManager.ComponentState) or
    (FiltType in SkinManager.SkinsFilter);
end;

procedure TacSkinListController.StartDelayedLoading;
var
  M: TMessage;
begin
  if (Thread <> nil) and not Thread.Terminated then
    Thread.Terminate;

  Thread := TacAddThread.Create(True);
  DosCode := 0;
  CurrentInternalIndex := 0;
  with Thread do
  begin
    Priority := tpLower;
    FOwner := Self;
    FreeOnTerminate := True;
    if TimeEvent <> nil then
      FreeAndNil(TimeEvent);

    TimeEvent := TSimpleEvent.Create;
    TimeEvent.ResetEvent;
{$IFDEF DELPHI6UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
    Resume;
    M := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_SKINTHREADSTATUS),
      1 { Started } , 0);
    AppBroadCastS(M);
  end;
end;

function TacSkinListController.UpdateData(UpdateNow: boolean): boolean;
var
  i: integer;
  bChanged: boolean;
begin
  if [csLoading, csDestroying] * SkinManager.ComponentState = [] then
  begin
    bChanged := True;
    Result := True;
    if (SkinManager <> DefaultManager) and (DefaultManager <> nil) and
      (CompareText(SkinManager.SkinDirectory, DefaultManager.SkinDirectory) = 0)
    then
    begin
      // Fast loading from existing SkinList
      TsAlphaImageList(ImgList).Clear;
      TsAlphaImageList(ImgList).Handle;
      DefaultManager.SkinListController.CheckThread;
      DefaultManager.SkinListController.UpdateIfNeeded(True { UpdateNow } );
      SetLength(SkinList, Length(DefaultManager.SkinListController.SkinList));
      for i := 0 to Length(DefaultManager.SkinListController.SkinList) - 1 do
      begin
        SkinList[i].skName := DefaultManager.SkinListController.SkinList
          [i].skName;
        SkinList[i].skSkinMode := DefaultManager.SkinListController.SkinList[i]
          .skSkinMode;
        SkinList[i].skImageIndex := DefaultManager.SkinListController.SkinList
          [i].skImageIndex;
      end;
      for i := 0 to DefaultManager.SkinListController.ImgList.Count - 1 do
        with TsImgListItem(TsAlphaImageList(ImgList).Items.Add) do
        begin
          ImgData.LoadFromStream
            (TsAlphaImageList(DefaultManager.SkinListController.ImgList)
            .Items[i].ImgData);
          ImageFormat := ifPNG;
        end;
{$IFNDEF DELPHI7UP}
      TsAlphaImageList(ImgList).GenerateStdList;
{$ENDIF}
      UpdateNeeded := False;
    end
    else
    begin
      ClearList(False);

      if SkinsAllowed(sfiInternal) then // Internal skins data loading
        if SkinManager.InternalSkins.Count > 0 then
          if UpdateNow then
          begin
            for i := 0 to SkinManager.InternalSkins.Count - 1 do
              AddSkin(SkinManager.InternalSkins[i].Name, i);
          end
          else
          begin
            bChanged := False;
            if (Thread = nil) or Thread.Terminated then
              StartDelayedLoading;
          end;

      if SkinsAllowed(sfiExternal) then
      begin // External skins data loading
        sPath := NormalDir(SkinManager.GetFullSkinDirectory);
        if acDirExists(sPath) then
        begin
          ClearFileInfo;
          DosCode := acFindFirst(sPath + '*.*', faDirectory, FileInfo);
          if UpdateNow then
          begin
            while DosCode = 0 do
              DosCode := AddSkinData;
            acFindClose(FileInfo);
            FileInfo.Name := '';
            UpdateNeeded := False;
          end
          else
          begin
            bChanged := False;
            if (Thread = nil) or Thread.Terminated then
              StartDelayedLoading;
          end;
        end
        else if FileInfo.Name <> '' then
        begin
          acFindClose(FileInfo);
          FileInfo.Name := '';
          UpdateNeeded := False;
        end;
      end;
    end;
    if bChanged then
      SendListChanged;
  end
  else
    Result := False;
end;

procedure TacSkinListController.UpdateIfNeeded(ForceUpdating: boolean = False);
begin
  if UpdateNeeded then
    UpdateNeeded := not UpdateData(ForceUpdating);
end;

constructor TacFonts.Create(AOwner: TsSkinManager);
begin
  SkinManager := AOwner;
  FMainMode := fmDefault;
end;

procedure TacFonts.SetMainFont(const Value: string);
begin
  if FMainFont <> Value then
  begin
    FMainFont := Value;
    if [csLoading, csDesigning] * SkinManager.ComponentState = [] then
      case FMainMode of
        fmCustom:
          SkinManager.ChangeAppFont(FMainFont);
      end;
  end;
end;

procedure TacFonts.SetMainMode(const Value: TacFontMode);
begin
  if FMainMode <> Value then
  begin
    FMainMode := Value;
    if [csLoading] * SkinManager.ComponentState = [] then
      case FMainMode of
        fmDefault:
          if SkinManager.DefStoredFontName <> '' then
            SkinManager.ChangeAppFont(SkinManager.DefStoredFontName);

        fmFromSkin:
          if SkinManager.SkinCommonInfo.MainFont <> '' then
            SkinManager.ChangeAppFont(SkinManager.SkinCommonInfo.MainFont)
          else
            SkinManager.ChangeAppFont(SkinManager.DefStoredFontName);

        fmCustom:
          SkinManager.ChangeAppFont(FMainFont);
      end;
  end;
end;

function TacButtonsSupport.GetOldGlyphsMode: boolean;
begin
  Result := ac_OldGlyphsMode;
end;

procedure TacButtonsSupport.SetOldGlyphsMode(const Value: boolean);
begin
  if ac_OldGlyphsMode <> Value then
  begin
    ac_OldGlyphsMode := Value;
    if (FOwner <> nil) and
      ([csLoading, csDesigning] * FOwner.ComponentState = []) then
      FOwner.RepaintForms;
  end;
end;

function TacSkinData.Active: boolean;
begin
  Result := TsSkinManager(FOwner).IsActive;
end;

function TacSkinData.BrightMax: integer;
begin
  Result := TsSkinManager(FOwner).SkinCommonInfo.BrightMax;
end;

function TacSkinData.BrightMin: integer;
begin
  Result := TsSkinManager(FOwner).SkinCommonInfo.BrightMin;
end;

procedure TacSkinData.CheckShadows;
var
  w, h: integer;
begin
  if FOwner.Active and FOwner.ExtendedBorders then
  begin
    if ShdaTemplate <> nil then
      FreeAndNil(ShdaTemplate);

    if ShdiTemplate <> nil then
      FreeAndNil(ShdiTemplate);

    ShdaTemplate := TBitmap.Create;
    ShdiTemplate := TBitmap.Create;
    if (ExBorder >= 0) and (FOwner.ExtBordersMode = ebmFull) then
    begin
      if ExDrawMode = 0 then
        with ma[ExBorder] do
        begin // Shadow only
          // Calc a width from beginning of image to beginning of content
          FormShadowSize.Top := max(0, WT - ExContentOffs);
          FormShadowSize.Left := max(0, WL - ExContentOffs);
          FormShadowSize.Right := max(0, WR - ExContentOffs);
          FormShadowSize.Bottom := max(0, WB - ExContentOffs);
        end
      else // Receive an offset to content
        FormShadowSize := Rect(ExContentOffs, ExContentOffs, ExContentOffs,
          ExContentOffs);

      w := ma[ExBorder].Width;
      h := ma[ExBorder].Height;
      ShdiTemplate.PixelFormat := pf32bit;
      ShdiTemplate.Width := w;
      ShdiTemplate.Height := h;
      BitBlt(ShdiTemplate.Canvas.Handle, 0, 0, w, h,
        ma[ExBorder].Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      if ma[ExBorder].ImageCount = 1 then
        ShdaTemplate.Assign(ShdiTemplate)
      else
      begin
        ShdaTemplate.PixelFormat := pf32bit;
        ShdaTemplate.Width := w;
        ShdaTemplate.Height := h;
        BitBlt(ShdaTemplate.Canvas.Handle, 0, 0, w, h,
          ma[ExBorder].Bmp.Canvas.Handle, w, 0, SRCCOPY);
      end;
    end
    else
    begin
      ShdaTemplate.Assign(GetActiveShadow);
      ShdiTemplate.Assign(GetPassiveShadow);
      w := WndShadowSize;
      FormShadowSize := Rect(w, max(0, w - WndShadowOffset), w,
        max(0, w + WndShadowOffset));
    end;
  end
  else
    FormShadowSize := MkRect;
end;

constructor TacSkinData.Create(AOwner: TsSkinManager; aPPI: integer);
begin
  SetLength(gd, 0);
  SetLength(ma, 0);
  SetLength(oe, 0);
  FormShadowSize := MkRect;
  FOwner := AOwner;
  Initializing := False;
  PPI := aPPI;
  ScalePercent := MulDiv(aPPI, 100, 96);
  ScaleValue := PPIToIndex(aPPI);
end;

function TacSkinData.DefineBmp(var md: TsMaskData; const ImgName: string;
  const R: TRect; IgnoreFuchsia: boolean): boolean;
var
  i: integer;
  SrcRect: TRect;
  Stream: TMemoryStream;
begin
  Result := True;
  // Search in array of used images
  i := SearchBmpIndex(ImgName, R);
  if i < 0 then
  begin
    Stream := nil;
    if ImgName <> '' then
    begin // External file
      if FOwner.SkinIsPacked then
        with FOwner do
        begin
          for i := 0 to sc.FileCount - 1 do
            if UpperCase(FOwner.sc.Files[i].Name) = ImgName then
            begin
              Stream := sc.Files[i].Stream;
              Break; // Found and loaded
            end;
        end;
      // Load image to item
      AddExternalItem(md, ImgName, Stream, IgnoreFuchsia);
      if md.Bmp <> nil then
      begin
        md.Bmp.Canvas.Handle;
        md.Bmp.Canvas.Lock;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end
    else
    begin // From MasterBitmap
      SrcRect := md.R;
      if (md.MulPPI <> md.DivPPI) and (md.DrawMode and BDM_NOSCALE = 0)
      { or (md.MulPPI <> 96) and (md.Bmp = nil) } then
        // Make scaled image if needed
        if ScaleMaskData(md, FOwner.MasterBitmap, False) then
          AddImage(md.Bmp, ImgName, md.ClassName + '-' + md.PropertyName, R,
            SrcRect, md.DivPPI, md.MaskType, False);
      // Add new scaled image to array
    end;
  end
  else
    UpdateItem(md, i);

  if IsRectEmpty(md.R) and Assigned(md.Bmp) then
    md.R := MkRect(md.Bmp);
end;

destructor TacSkinData.Destroy;
begin
  Reset;
  inherited;
end;

function TacSkinData.GetActiveShadow: TBitmap;
begin
  if rsta = nil then
    rsta := MakeShadow(WndShadowColorA, WndShadowRadius, WndShadowOffset,
      WndShadowSize * 2, WndShadowSize * 2, 0);

  Result := rsta;
end;

function TacSkinData.GetPassiveShadow: TBitmap;
begin
  if rsti = nil then
    rsti := MakeShadow(WndShadowColorN, WndShadowRadius, WndShadowOffset,
      WndShadowSize * 2, WndShadowSize * 2, 0);

  Result := rsti;
end;

procedure TacSkinData.Reset;
begin
  FreeBitmaps;
  SetLength(gd, 0);
  SetLength(ma, 0);
  SetLength(oe, 0);
  FreeAndNil(rsta);
  FreeAndNil(rsti);
  FreeAndNil(ShdaTemplate);
  FreeAndNil(ShdiTemplate);
end;

function TacSkinData.ScaledInteger(const Name: string; Def: integer;
  Recalc: boolean; MaskIndex: integer = -1): integer;
var
  MulPPI, DivPPI, SrcPPI: integer;
begin
  if Recalc and (MaskIndex >= 0) then
  begin // If source of mask exists
    SrcPPI := ma[MaskIndex].DivPPI; // PPI of mask
    DivPPI := ma[MaskIndex].DivPPI;
    Result := FOwner.SkinFile.ReadInteger(s_GlobalInfo,
      Name + aSfxs[PPIToIndex(SrcPPI)], -99999); // Get parameter of mask
    if Result = -99999 then
    begin // If doesn't exists
      Result := FOwner.SkinFile.ReadInteger(s_GlobalInfo, Name, Def);
      // Get param for 96 PPI
      DivPPI := 96;
    end;
    Result := MulDiv(Result, PPI, DivPPI);
    Exit;
  end
  else
    Result := FOwner.SkinFile.ReadInteger(s_GlobalInfo,
      Name + aSfxs[ScaleValue], -99999);

  if Result = -99999 then
    if aSfxs[ScaleValue] <> '' then
    begin // Make new scaled value
      Result := FOwner.SkinFile.ReadInteger(s_GlobalInfo, Name, Def);
      if Recalc then
      begin
        if MaskIndex >= 0 then
        begin
          MulPPI := ma[MaskIndex].MulPPI;
          DivPPI := ma[MaskIndex].DivPPI;
        end
        else
        begin
          MulPPI := PPI;
          DivPPI := 96;
        end;
        Result := MulDiv(FOwner.SkinFile.ReadInteger(s_GlobalInfo, Name, Def),
          MulPPI, DivPPI);
      end;
    end
    else
      Result := Def
  else if (FOwner.Options.ScaleMode = smCustomPPI) and
    (FOwner.Options.PixelsPerInch < 96) then
    Result := MulDiv(Result, FOwner.Options.PixelsPerInch, 96);
end;

function TacSkinData.ScaleIntValue(Value: integer): integer;
begin
  Result := MulDiv(Value, PPI, 96);
end;

function TacSkinData.SearchBmpIndex(const aImgName: string;
  const aR: TRect): integer;
var
  i: integer;
begin
  for i := 0 to Length(BmpArray) - 1 do
    if (BmpArray[i].FileName = aImgName) then
      if (aImgName <> '') or EqualRect(BmpArray[i].SrcRect, aR) then
      begin
        Result := i;
        Exit;
      end;

  Result := -1;
end;

procedure TacSkinData.UpdateItem(var md: TsMaskData; BmpIndex: integer);
begin
  with md do
  begin
    if BmpArray[BmpIndex].Bmp <> nil then
    begin
      Bmp := BmpArray[BmpIndex].Bmp;
      R := MkRect(Bmp);
      Height := Bmp.Height;
      Width := Bmp.Width;
    end
    else
    begin
      Height := HeightOf(R);
      Width := WidthOf(R);
    end;
    MulPPI := BmpArray[BmpIndex].MulPPI;
    DivPPI := BmpArray[BmpIndex].DivPPI;
    WL := MulDiv(WL, MulPPI, DivPPI);
    WT := MulDiv(WT, MulPPI, DivPPI);
    WR := min(MulDiv(WR, MulPPI, DivPPI), Width div ImageCount - 1 - WL);
    WB := min(MulDiv(WB, MulPPI, DivPPI), Height div (1 + MaskType) - 1 - WT);
  end;
end;

procedure TacSkinData.LoadAllMasks;
var
  i, j, aSkinIndex, iGlobal, iScale, iMasterBmp: integer;
  SectionsList, Values: TStringList;
  sf: TMemIniFile;
  s, sExt: string;

  function SearchScaledSrc(var iPPI: integer): string;
  var
    s: string;
    k, FirstIndex: integer;
  begin
    Result := '';

    if aPPIArray[iScale] < PPI then
      FirstIndex := iScale + 1
    else
      FirstIndex := iScale;

    // Search nearest replacement
    for k := FirstIndex to acMaxScaleNdx do
    begin
      Result := UpperCase(sf.ReadString(SectionsList[i],
        Values[j] + aSfxs[k], ''));
      if Result <> '' then
      begin
        Result := ConvertSkinStr(Result);
        iPPI := aPPIArray[k];
        Exit;
      end;
    end;
    if Result = '' then
      for k := FirstIndex - 1 downto 0 do
      begin
        s := Values[j] + aSfxs[k];
        Result := UpperCase(sf.ReadString(SectionsList[i], s, ''));
        if Result <> '' then
        begin
          Result := ConvertSkinStr(Result);
          iPPI := aPPIArray[k];
          Exit;
        end;
      end;

    iPPI := PPI;
  end;

  function CreateImgFile(const sFile: string): TBitmap;
  var
    i: integer;
    sf: string;
    jpg: TJpegImage;
    Png: TPNGGraphic;
    st: TMemoryStream;
  begin
    st := nil;
    with FOwner do
      if SkinIsPacked then
      begin
        for i := 0 to sc.FileCount - 1 do
          if UpperCase(sc.Files[i].Name) = sFile then
          begin
            sc.Files[i].Stream.Seek(0, 0);
            st := TMemoryStream.Create;
            st.LoadFromStream(sc.Files[i].Stream);
            Break;
          end
      end
      else
      begin
        sf := NormalDir(SkinDirectory) + SkinName + '\' + sFile;
        if FileExists(sf) then
        begin
          st := TMemoryStream.Create;
          st.LoadFromFile(sf);
        end;
      end;

    if st <> nil then
    begin
      try
        Result := TBitmap.Create;
        if (pos('.JPG', sFile) > 0) or (pos('.JPEG', sFile) > 0) then
        begin
          jpg := TJpegImage.Create;
          try
            jpg.LoadFromStream(st);
            Result.Assign(jpg);
          finally
            jpg.Free;
          end;
        end
        else if pos('.PNG', sFile) > 0 then
        begin
          Png := TPNGGraphic.Create;
          try
            Png.LoadFromFile(sf);
            Result.Assign(Png);
          finally
            Png.Free;
          end;
        end
        else
          Result.LoadFromFile(sf);

        Result.PixelFormat := pf32bit;
      finally
        FreeAndNil(st);
      end;
    end
    else
      Result := nil;
  end;

  procedure LoadImage(s: string);
  var
    SrcPPI, NewNdx: integer;
    sName: string;
  begin
    s := SearchScaledSrc(SrcPPI);
    if s <> '' then
    begin
      sName := Copy(s, 45, Length(s) - 44);
      // Make new item
      NewNdx := MakeNewItem(aSkinIndex, Values[j], SectionsList[i],
        acImgTypes[Mini(CopyInt(s, 36, 1), Length(acImgTypes) - 1)],
        CopyR(s, 2), // Coords
        CopyInt(s, 34, 1), // Count
        CopyInt(s, 37, 2), // Draw Mode
        CopyInt(s, 35, 1)); // Mask type

      with ma[NewNdx] do
      begin
        DivPPI := SrcPPI;
        // Border width
        WL := CopyInt(s, 18, 4);
        WT := CopyInt(s, 22, 4);
        WR := CopyInt(s, 26, 4);
        WB := CopyInt(s, 30, 4);
        // DrawMode
        // DrawMode := CopyInt(s, 37, 2);
        // Receive Bmp to current item
        if not DefineBmp(ma[NewNdx], sName, R,
          { IsPng or } (ImgType in [itisaGlow, itisaTexture])) then
          SetLength(ma, NewNdx)
        else
          // Calc corner type
          if ImgType = itisaBorder then
            Cfg := Cfg or GetCornerType(FOwner, ma[NewNdx]);
      end;
    end;
  end;

begin
  FreeBitmaps;
  if FOwner.SkinFile <> nil then
    with FOwner, Self do
    begin
      // Main color required for scaling
      FOwner.Palette[pcMainColor] := FOwner.SkinFile.ReadInteger(s_GlobalInfo,
        s_Color, ColorToRGB(clBtnFace));

      iScale := ScaleValue;
      sExt := aSfxs[iScale];

      sf := SkinFile;
      SectionsList := TStringList.Create;
      SetCaseSens(SectionsList);
      Values := TStringList.Create;
      iGlobal := -1;
      iMasterBmp := -1;
      SetCaseSens(Values);
      try
        sf.ReadSections(SectionsList);
        for i := 0 to SectionsList.Count - 1 do
        begin
          if (iGlobal < 0) and (SectionsList[i] = s_GlobalInfo) then
            iGlobal := i;

          sf.ReadSection(SectionsList[i], Values);
          aSkinIndex := i;
          // Search and load all images from section
          for j := 0 to Values.Count - 1 do
          begin
            if (i = iGlobal) and (iMasterBmp = -1) and
              (Values[j] = s_MasterBitmap) then
            begin
              iMasterBmp := j;
              Continue;
            end;
            if pos('*', Values[j]) = 0 then
            begin // Ignore scaled images (they loaded later)
              s := UpperCase(sf.ReadString(SectionsList[i], Values[j], ''));
              if s <> '' then
                case s[1] of
                  acCharImg, TexChar, CharGlyph, CharMask, CharExt:
                    LoadImage(s)
                else
                  if (pos('.JPG', s) > 0) or (pos('.BMP', s) > 0) then
                    LoadImage( { ConvertSkinStr( } s);
                end;
            end;
          end;
        end;
      finally
        FreeAndNil(Values);
        FreeAndNil(SectionsList);
        sf := nil;
      end;
    end;

  with FOwner do
    if CommonSections.Count > 0 then
    begin
      sf := TMemIniFile.Create('');
      SetCaseSens(sf);
      sf.SetStrings(CommonSections);

      SectionsList := TStringList.Create;
      SetCaseSens(SectionsList);

      Values := TStringList.Create;
      SetCaseSens(Values);
      try
        sf.ReadSections(SectionsList);
        sf.SetStrings(CommonSections);
        for i := 0 to SectionsList.Count - 1 do
        begin
          if UpperCase(SectionsList[i]) = s_GlobalInfo then
            Continue;

          inc(aSkinIndex);
          sf.ReadSection(SectionsList[i], Values);
          // Search and load all images from section
          for j := 0 to Values.Count - 1 do
            if pos('*', Values[j]) = 0 then
            begin // Ignore scaled images (they loaded later)
              s := UpperCase(sf.ReadString(SectionsList[i], Values[j], ''));
              if s <> '' then
                case s[1] of
                  acCharImg, TexChar, CharGlyph, CharMask, CharExt:
                    LoadImage(s)
                else
                  if (pos('.JPG', s) > 0) or (pos('.BMP', s) > 0) then
                    LoadImage( { ConvertSkinStr( } s);
                end;
            end;
        end;
      finally
        FreeAndNil(Values);
        FreeAndNil(sf);
        FreeAndNil(SectionsList);
      end;
    end;

{$IFDEF CHECKIMGES}
  s := 'c:\Temp\' + IntToStr(PPI);
  acCreateDir(s);
  s := s + '\' + FOwner.SkinName;
  ClearDir(s, False);
  acCreateDir(s);
  FOwner.MasterBitmap.SaveToFile(s + '\' + 'MasterBitmap.bmp');
  for i := 0 to Length(BmpArray) - 1 do
    with BmpArray[i] do
      if Bmp <> nil then
        if Path <> '' then
          Bmp.SaveToFile(s + '\' + AddChar('0', IntToStr(i), 3) + '-' +
            Path + '.bmp')
        else if FileName <> '' then
          Bmp.SaveToFile(s + '\' + FileName)
        else
          Bmp.SaveToFile(s + '\' + IntToStr(i) + '.bmp');

  acCreateDir(s);
{$ENDIF}
end;

procedure TacSkinData.LoadParams;
begin
  Spacing := ScalePPI(ac_BaseSpacing, PPI);
  LineWidth := ScalePPI(acLineWidth, PPI);
  ArrowSize := ScalePPI(ac_BaseArrowWidth, PPI);
  SmallArrowSize := ScalePPI(ac_BaseArrowWidth - 1, PPI);
  ScrollBtnLength := ScalePPI(acScrollBtnLength, PPI);
end;

procedure TacSkinData.AddExternalItem(var md: TsMaskData; const s: string;
  Stream: TMemoryStream; IgnoreFuchsia: boolean);
begin
  AddExtItem(md, s, Stream, IgnoreFuchsia, FOwner, Self);
end;

procedure TacSkinData.AddImage(const Bmp: TBitmap; const s, sPropName: string;
  const R, SrcRect: TRect; SrcPPI, iMaskType: integer; aExt: boolean);
var
  l: integer;
begin
  l := Length(BmpArray);
  SetLength(BmpArray, l + 1);
  BmpArray[l].Bmp := Bmp;
  if IsRectEmpty(R) and (Bmp <> nil) then
    BmpArray[l].R := MkRect(Bmp)
  else
    BmpArray[l].R := R;

  BmpArray[l].FileName := s;
  BmpArray[l].MulPPI := PPI;
  BmpArray[l].DivPPI := SrcPPI;
  BmpArray[l].MaskType := iMaskType;
  BmpArray[l].Path := sPropName;
  BmpArray[l].SrcRect := SrcRect;
  BmpArray[l].Ext := aExt;
end;

function TacSkinData.AddNewSection(const AName: string;
  AParentSection, AStates, ATransparency: integer;
  AColor, AFontColor: TColor): integer;
var
  i: integer;
begin
  Result := Length(gd);
  SetLength(gd, Result + 1);
  if AParentSection < 0 then
  begin
    gd[Result].States := AStates;
    for i := 0 to ac_MaxPropsIndex do
      with gd[Result].Props[i] do
      begin
        Transparency := ATransparency;
        Color := AColor;
        FontColor.Color := AFontColor;
        FontColor.Left := -1;
        FontColor.Top := -1;
        FontColor.Right := -1;
        FontColor.Bottom := -1;
      end;
  end
  else
  begin
    gd[Result] := gd[AParentSection];
    for i := 0 to ac_MaxPropsIndex do
      CopyProperties(gd[Result].Props[i], gd[AParentSection].Props[i]);
  end;
  gd[Result].ClassName := AName;
  if AParentSection >= 0 then
    gd[Result].ParentClass := gd[AParentSection].ClassName; // '';
end;

function TacSkinData.MakeNewItem(SkinIndex: integer;
  const PropertyName, AClassName: string; ImgType: TacImgType; R: TRect;
  Count, DrawMode: integer; MaskType: smallint): integer;
begin
  Result := Length(ma);
  SetLength(ma, Result + 1);
  ma[Result].PropertyName := PropertyName;
  ma[Result].ClassName := AClassName;
  ma[Result].SkinIndex := SkinIndex;
  ma[Result].DrawMode := DrawMode;
  ma[Result].MaskType := MaskType;
  ma[Result].ImgType := ImgType;
  ma[Result].ImageCount := Count;
  ma[Result].Manager := FOwner;
  ma[Result].R := R;
  ma[Result].DivPPI := 96;
  ma[Result].MulPPI := PPI;
end;

procedure TacSkinData.InitMaskIndexes;
var
  i: integer;

  procedure CheckBorderWidth(var AValue: smallint; const md: TsMaskData;
    AVert: boolean);
  begin
    if AValue <= 0 then
      if AVert then
        AValue := (md.Height - 1) div (2 * (md.MaskType + 1))
      else
        AValue := (md.Width - 1) div 2;
  end;

  procedure PrepareOuter;
  var
    l: integer;
    ResBmp: TBitmap;
  begin
    l := Length(ma);
    SetLength(ma, l + 1);
    ResBmp := MakeShadow(gd[i].OuterColorL, gd[i].OuterRadius, 0,
      max(0, gd[i].OuterOffset.Top) + max(0, gd[i].OuterOffset.Bottom),
      max(0, gd[i].OuterOffset.Left) + max(0, gd[i].OuterOffset.Right),
      gd[i].OuterBlur, True);

    ma[l].Bmp := CreateBmp32(ResBmp.Width, ResBmp.Height * 2);
    if gd[i].OuterMode = 3 then
      PaintMainLayer(ma[l].Bmp, MkRect(ResBmp), gd[i].OuterColorL, clFuchsia,
        clFuchsia, clFuchsia)
    else
      PaintMainLayer(ma[l].Bmp, MkRect(ResBmp), gd[i].OuterColorL,
        gd[i].OuterColorT, gd[i].OuterColorR, gd[i].OuterColorB);
    // Copy mask
    BitBlt(ma[l].Bmp.Canvas.Handle, 0, ResBmp.Height, ResBmp.Width,
      ResBmp.Height, ResBmp.Canvas.Handle, 0, 0, SRCCOPY);
    gd[i].OuterMask := l;

    ma[l].R := MkRect(ma[l].Bmp);

    ma[l].Width := ma[l].Bmp.Width;
    ma[l].Height := ma[l].Bmp.Height;

    ma[l].WL := (ma[l].Width + gd[i].OuterOffset.Left -
      gd[i].OuterOffset.Right) div 2;
    ma[l].WR := (ma[l].Width + gd[i].OuterOffset.Right -
      gd[i].OuterOffset.Left) div 2;
    ma[l].WT := (ResBmp.Height + gd[i].OuterOffset.Top -
      gd[i].OuterOffset.Bottom) div 2;
    ma[l].WB := (ResBmp.Height + gd[i].OuterOffset.Bottom -
      gd[i].OuterOffset.Top) div 2;

    AddImage(ma[l].Bmp, 'Outer' + IntToStr(l), ma[l].ClassName + '-' +
      ma[l].PropertyName, ma[l].R, MkRect, PPI, ma[l].MaskType, False);
    ma[l].ImageCount := 1;
    ma[l].MaskType := 1;
    ma[l].DrawMode := 1;
    ma[l].ImgType := itisaBorder;
    ma[l].Manager := FOwner;
    ma[l].Cfg := 0;
    ma[l].SkinIndex := i;
    FreeAndNil(ResBmp);
  end;

begin
  for i := 0 to Length(gd) - 1 do
    if i <> FOwner.SkinCommonInfo.IndexGlobalInfo then
      with gd[i] do
      begin
        if BorderIndex = 0 then // If not forbidden in CommonSection
          BorderIndex := FOwner.GetMaskIndex(i, s_BordersMask, Self);

        ImgTL := FOwner.GetMaskIndex(i, s_ImgTopLeft, Self);
        ImgTR := FOwner.GetMaskIndex(i, s_ImgTopRight, Self);
        ImgBL := FOwner.GetMaskIndex(i, s_ImgBottomLeft, Self);
        ImgBR := FOwner.GetMaskIndex(i, s_ImgBottomRight, Self);
        case OuterMode of
          0:
            OuterMask := -1;
          1, 2:
            OuterMask := FOwner.GetMaskIndex(i, s_OuterMask, Self);
          3, 4:
            begin
              OuterMask := FOwner.GetMaskIndex(i, s_OuterMask, Self);
              if OuterMask < 0 then
                PrepareOuter; // If outereffect is calculated
            end;
        end;

        Props[0].TextureIndex := FOwner.GetTextureIndex(i, ClassName,
          s_Pattern, Self);
        if States > 1 then
          Props[1].TextureIndex := FOwner.GetTextureIndex(i, ClassName,
            s_HotPattern, Self);

        if (States > 2) and UseState2 then
          Props[2].TextureIndex := FOwner.GetTextureIndex(i, ClassName,
            s_Pattern2, Self)
        else
          Props[2].TextureIndex := Props[1].TextureIndex;

        if BorderIndex < 0 then
          if BorderRadius > 0 then
            ScrollBorderOffset := 0
          else if BorderWidth > 0 then
            if BorderWidth = 1 then
              ScrollBorderOffset := -1
            else
              ScrollBorderOffset := -2
          else if (gd[i].BorderIndex >= 0) and
            ((ClassName = s_Edit) or (ClassName = s_AlphaEdit)) then
            with ma[gd[i].BorderIndex] do
              if (WL = 1) and (WT = 1) and (WR = 1) and (WB = 1) then
                ScrollBorderOffset := -1
              else
                ScrollBorderOffset := 0
          else
            ScrollBorderOffset := 0;
      end;

  for i := 0 to Length(ma) - 1 do
    with ma[i] do
    begin
      case ImageCount of
        0:
          Width := 0;
      else
        Width := WidthOf(R) div ImageCount;
      end;
      case MaskType of
        - 1:
          Height := 0;
        0:
          Height := HeightOf(R);
      else
        Height := HeightOf(R) div (MaskType + 1)
      end;
      if ImageCount = 0 then
      begin
        CheckBorderWidth(WT, ma[i], True);
        CheckBorderWidth(WB, ma[i], True);
        CheckBorderWidth(WL, ma[i], False);
        CheckBorderWidth(WB, ma[i], False);
        ImageCount := 1;
      end;
      // Calc corner type
      if ImgType = itisaBorder then
        Cfg := Cfg or GetCornerType(FOwner, ma[i]);
    end;
end;

procedure TacSkinData.InitMaskParams;
begin
  ExContentOffs := ScaledInteger(s_FormOffset, GetSystemMetrics(SM_CXSIZEFRAME),
    True, ExBorder);
  if FOwner.ExtBordersMode = ebmFull then
  begin
    // Depended from used image
    ExBorderWidth := max(ScaledInteger(s_BorderWidth, 4, True, ExBorder), 1);
    ExTitleHeight := ScaledInteger(s_TitleHeight, 30, True, ExBorder);
    if ExTitleHeight <= 0 then
      ExTitleHeight := FOwner.ScaleInt(30);
    ExMaxHeight := ScaledInteger(s_MaxTitleHeight, 0, True, ExBorder);
    if ExMaxHeight <= 0 then
      ExMaxHeight := ExTitleHeight;
    ExCenterOffs := ScaledInteger(s_CenterOffset, 0, True, ExBorder);
  end
  else
  begin
    ExBorderWidth := FOwner.ScaleInt(4);
    ExTitleHeight := FOwner.ScaleInt(30);
    ExMaxHeight := ExTitleHeight;
    ExCenterOffs := 0;
  end;
end;

procedure TacSkinData.Init;
begin
  Initializing := True;
  FOwner.ReloadSkin(PPI);
  Initializing := False;
end;

procedure TacSkinData.InitConstantIndexes;
var
  i: integer;
  b: boolean;
  Side: TacSide;
  tg: TacTitleGlyph;
  tPos: TacTabLayout;
  Section: TacSection;
  CheckState: TCheckBoxState;

  function GetGlobalIndex(const sName: string): integer;
  begin
    Result := FOwner.GetMaskIndex(FOwner.SkinCommonInfo.IndexGlobalInfo,
      s_GlobalInfo, sName, Self)
  end;

  function GetGlyphNdx(const sName: string; const SectionIndex: integer = -1;
    Png: boolean = False): integer;
  begin
    if SectionIndex < 0 then
      Result := FOwner.GetMaskIndex(FOwner.SkinCommonInfo.IndexGlobalInfo,
        s_GlobalInfo, sName, Self)
    else
      Result := FOwner.GetMaskIndex(SectionIndex, sName, Self);
  end;

  function GetExtBordNdx(const sName: string): integer;
  begin
    Result := GetGlobalIndex(sName);
  end;

  procedure ReadConstElementData(var ed: TacConstElementData;
    const ASection: string = ''; const AltSection1: string = '';
    const AltSection2: string = ''; ScaleGlyph: boolean = True);
  begin
    with FOwner, ed do
    begin
      if ASection <> '' then
        SkinSection := ASection;

      SkinIndex := GetSkinIndex(SkinSection, Self);
      if SkinIndex < 0 then
      begin
        SkinSection := AltSection1;
        SkinIndex := GetSkinIndex(SkinSection, Self);
        if SkinIndex < 0 then
        begin
          SkinSection := AltSection2;
          SkinIndex := GetSkinIndex(SkinSection, Self);
        end;
      end;
      MaskIndex := GetMaskIndex(SkinIndex, SkinSection, s_BordersMask, Self);
      GlyphIndex := GetMaskIndex(SkinIndex, s_ItemGlyph, Self);
      BGIndex[0] := GetMaskIndex(SkinIndex, SkinSection, s_Pattern, Self);
      BGIndex[1] := GetMaskIndex(SkinIndex, SkinSection, s_HotPattern, Self);
    end;
  end;

  procedure ReadTrackBarData(var Data: TacTrackBarData; Horz: boolean);
  begin
    with FOwner, Data do
    begin
      SkinIndex := GetSkinIndex(s_TrackBar, Self);
      MaskIndex := GetMaskIndex(SkinIndex, s_BordersMask, Self);
      ProgIndex := GetMaskIndex(SkinIndex, ProgArray[Horz], Self);
      TickIndex := GetMaskIndex(SkinIndex, ThickArray[Horz], Self);
      SliderIndex := GetMaskIndex(SkinIndex, s_SliderChannelMask, Self);
      if not Horz then
        GlyphIndex := GetMaskIndex(SkinIndex, s_SliderVertMask, Self)
      else
        GlyphIndex := -1;

      if GlyphIndex < 0 then
        GlyphIndex := GetMaskIndex(SkinIndex, s_SliderHorzMask, Self);

      BGIndex[0] := GetMaskIndex(SkinIndex, s_Pattern, Self);
      BGIndex[1] := GetMaskIndex(SkinIndex, s_HotPattern, Self);
    end;
  end;

begin
  with FOwner, SkinCommonInfo do
  begin
    IndexGlobalInfo := GetSkinIndex(s_GlobalInfo, Self);
    // Some sections
    for Section := Low(Sections) to High(Sections) do
      Sections[Section] := GetSkinIndex(acSectNames[Section], Self);

    // Check auxiliary sections
    for Section := Low(acAuxReplacement) to High(acAuxReplacement) do
      if Sections[Section] < 0 then
      begin
        Sections[Section] := Sections[acAuxReplacement[Section]];
        if Section <> acAuxReplacement[Section] then
          // Sections[Section] :=
          AddNewSection(acSectNames[Section], Sections[acAuxReplacement[Section]
            ], -1, -1, -1, -1);
      end;
  end;
  // Glyphs
  for CheckState := cbUnchecked to cbGrayed do
    CheckBox[CheckState] := GetGlyphNdx(acCheckGlyphs[CheckState]);

  for CheckState := cbUnchecked to cbGrayed do
    SmallCheckBox[CheckState] := GetGlyphNdx(acSmallChecks[CheckState]);

  for b := False to True do
    RadioButton[b] := GetGlyphNdx(acRadioGlyphs[b]);
  // Masks
  if FOwner.ExtBordersMode = ebmFull then
    ExBorder := GetExtBordNdx(s_ExBorder)
  else
    ExBorder := -1;

  GripRightBottom := GetGlyphNdx(s_GripImage);
  GripHorizontal := GetGlobalIndex(s_GripHorz);
  GripVertical := GetGlobalIndex(s_GripVert);

  ReadConstElementData(MenuItem, s_MenuItem);
  ReadConstElementData(ComboBtn, s_ComboBtn);
  ComboBtn.GlyphIndex := FOwner.GetMaskIndex(s_ComboBox, s_ItemGlyph, Self);
  // Some sections
  for tPos := tlFirst to tlSingle do
    for Side := asLeft to asBottom do
      ReadConstElementData(Tabs[tPos][Side], acTabSections[tPos][Side]);

  for b := False to True do
  begin
    ReadConstElementData(Sliders[b], acScrollSliders[b]);
    ReadTrackBarData(TrackBar[b], b);
  end;
  for Side := asLeft to asBottom do
  begin
    ReadConstElementData(ScrollBtns[Side], acScrollBtns[Side], '', '', False);
    ReadConstElementData(Scrolls[Side], acScrollParts[Side], '', '', False);
    ReadConstElementData(UpDownBtns[Side], acUpDownBtns[Side], s_UpDown,
      s_Button, False);
  end;
  // Title icons
  for tg := Low(TacTitleGlyph) to High(TacTitleGlyph) do
    TitleGlyphs[tg] := GetGlyphNdx(acTitleGlyphs[tg]);

  if TitleGlyphs[tgCloseAlone] < 0 then
    TitleGlyphs[tgCloseAlone] := TitleGlyphs[tgClose];

  for tg := tgSmallClose to tgSmallHelp do
    if TitleGlyphs[tg] < 0 then
      TitleGlyphs[tg] := TitleGlyphs[TacTitleGlyph(ord(tg) - 5)];

  // Title icons glows
  for tg := tgCloseAlone to tgNormal do
    for i := 0 to Length(TitleGlows[tg]) - 1 do
      TitleGlows[tg][i] := GetGlyphNdx(acTitleGlyphs[tg] + s_Glow + IntToStr(i),
        FOwner.SkinCommonInfo.IndexGlobalInfo, True);

  if TitleGlyphs[tgCloseAlone] = TitleGlyphs[tgClose] then
  begin
    GlowMargins[tgCloseAlone] := GlowMargins[tgClose];
    SetLength(TitleGlows[tgCloseAlone], Length(TitleGlows[tgClose]));
    for i := 0 to Length(TitleGlows[tgCloseAlone]) - 1 do
      TitleGlows[tgCloseAlone][i] := TitleGlows[tgClose][0];
  end;
end;

procedure TacSkinData.LoadAllGeneralData;
type
  TacGetInteger = function(const ClassName, PropertyName: string;
    DefaultValue: integer; MaxValue: integer = MaxInt): integer;
const
  // acMaxBlur = 5;
  acPropsArray: array [0 .. ac_MaxPropsIndex, 0 .. 13] of string =
    ((s_Color, s_FontColor, s_TCLeft, s_TCTop, s_TCRight, s_TCBottom,
    s_GlowColor, s_GlowSize, s_GradientPercent, s_GradientData, s_ImagePercent,
    s_Transparency, s_LightColor0, s_BorderColor0), (s_HotColor, s_HotFontColor,
    s_HotTCLeft, s_HotTCTop, s_HotTCRight, s_HotTCBottom, s_HotGlowColor,
    s_HotGlowSize, s_HotGradientPercent, s_HotGradientData, s_HotImagePercent,
    s_HotTransparency, s_LightColor1, s_BorderColor1),
    (s_Color2, s_FontColor2, s_TCLeft2, s_TCTop2, s_TCRight2, s_TCBottom2,
    s_GlowColor2, s_GlowSize2, s_GradientPercent2, s_GradientData2,
    s_ImagePercent2, s_Transparency2, s_LightColor2, s_BorderColor2));

  acDefProp: TsGenState = (GlowSize: 0; ImagePercent: 0; TextureIndex: - 1;
    Transparency: 0; GradientPercent: 0; Color: $FFFFFF; GlowColor: $FFFFFF;
    LightColor: - 1; // $FFFFFFFF
    BorderColor: clFuchsia; FontColor: (Color: 0; Left: - 1; Top: - 1;
    Right: - 1; Bottom: - 1););
var
  BodyW, BodyH, i, j, l, SkinIndex, ParentIndex: integer;
  gData, gTempData, acDefGenData: TsGeneralData;
  SectionsList, Ini: TStringList;
  sf: TMemIniFile;
  s: string;

  function FindString(const ClassName, PropertyName, DefaultValue
    : string): string;
  var
    s: string;
  begin
    Result := sf.ReadString(ClassName, PropertyName, CharQuest);
    if Result = CharQuest then
    begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> '') and (s <> CharQuest) and (s <> ClassName) then
        Result := FindString(s, PropertyName, CharQuest);

      if Result = CharQuest then
        Result := DefaultValue;
    end;
  end;

  function FindBoolean(const ClassName, PropertyName: string;
    DefaultValue: boolean): boolean;
  var
    s: string;
  begin
    s := sf.ReadString(ClassName, PropertyName, CharQuest);
    if s = CharQuest then
    begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> '') and (s <> CharQuest) and (s <> ClassName) then
        s := FindString(s, PropertyName, CharQuest);
    end;
    if s = CharQuest then
      Result := DefaultValue
    else
      Result := (s = s_TrueStr) or (s = CharOne);
  end;

  function FindInteger(const ClassName, PropertyName: string;
    DefaultValue: integer; MaxValue: integer = MaxInt;
    AllowNegative: boolean = False): integer;
  var
    s: string;
  begin
    Result := sf.ReadInteger(ClassName, PropertyName, -2);
    if Result = -2 then
    begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> '') and (s <> CharQuest) and (s <> ClassName) then
        Result := FindInteger(s, PropertyName, -1);

      if not AllowNegative then
        if Result < 0 then
          Result := DefaultValue
        else
          Result := ColorToRGB(Result);
    end;
    if MaxValue < Result then
      Result := MaxValue;
  end;

  function GetPropInteger(const Section, Name: string;
    AlternateValue: integer): integer;
  begin
    Result := acntUtils.ReadIniInteger(Ini, SectionsList, Section, Name, -1);
    if Result < 0 then
      Result := AlternateValue;
  end;

  function GetPropBool(const Section, Name: string;
    AlternateValue: boolean): boolean;
  var
    s: string;
  begin
    s := UpperCase(acntUtils.ReadIniString(Ini, SectionsList, SectionsList[i],
      Name, CharQuest));
    if s = CharQuest then
      Result := AlternateValue
    else
      Result := (s = CharOne) or (s = s_TrueStr);
  end;

  function GetPropStr(const Section, Name, AlternateValue: string): string;
  var
    s: string;
  begin
    s := acntUtils.ReadIniString(Ini, SectionsList, Section, Name, CharQuest);
    Result := iff(s = CharQuest, AlternateValue, s);
  end;

  procedure MakeCommonMask(var oed: TacOutEffData; const sPropName: string);
  var
    s: string;
    bScaleNeeded: boolean;
    Size, wd2, ndx: integer;
    ScalBmp, ResBmp: TBitmap;
  begin
    case oed.Source of
      0:
        oed.mask := -1;

      1:
        begin
          if ScaleValue > 0 then
          begin // Search a better source if scaled
            s := UpperCase(sf.ReadString(s_GlobalInfo,
              sPropName + aSfxs[ScaleValue], ''));
            bScaleNeeded := s = '';
          end
          else
          begin
            s := '';
            bScaleNeeded := False;
          end;
          if s = '' then
            s := sf.ReadString(s_GlobalInfo, sPropName, '');

          if s <> '' then
          begin
            s := ConvertSkinStr(s);
            Size := MakeNewItem(i, sPropName, s_GlobalInfo, itisaBorder,
              CopyR(s, 2), // Coords
              CopyInt(s, 34, 1), // Count
              CopyInt(s, 37, 2), // Draw Mode
              CopyInt(s, 35, 1)); // Mask type

            with ma[Size] do
            begin
              WL := CopyInt(s, 18, 4);
              WT := CopyInt(s, 22, 4);
              WR := CopyInt(s, 26, 4);
              WB := CopyInt(s, 30, 4);
              Cfg := 0;
              if bScaleNeeded then
              begin
                WL := FOwner.ScaleInt(WL);
                WT := FOwner.ScaleInt(WT);
                WR := FOwner.ScaleInt(WR);
                WB := FOwner.ScaleInt(WB);
                if Bmp = nil then
                  Bmp := FOwner.MasterBitmap;

                if MaskType = 1 then // Masked
                  ResBmp := CreateAlphaBmp(ma[Size].Bmp, R)
                else
                begin
                  ResBmp := CreateBmp32(R);
                  BitBlt(ResBmp.Canvas.Handle, 0, 0, ResBmp.Width,
                    ResBmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
                end;
                if ResBmp <> nil then
                begin
                  ScalBmp := CreateBmp32(FOwner.ScaleInt(ResBmp.Width),
                    FOwner.ScaleInt(ResBmp.Height));
                  Stretch(ResBmp, ScalBmp, ScalBmp.Width,
                    ScalBmp.Height, ftBox);
                  FreeAndNil(ResBmp);
                  if MaskType = 1 then
                  begin // Masked
                    ResBmp := ScalBmp;
                    ScalBmp := CreateMaskFromAlpha(ResBmp);
                    AddImage(ScalBmp, 'Outer', ma[Size].ClassName + '-' +
                      ma[Size].PropertyName, MkRect(ScalBmp), MkRect, PPI,
                      ma[Size].MaskType, False);
                    FreeAndNil(ResBmp);
                  end;
                  if Bmp <> FOwner.MasterBitmap then
                    FreeAndNil(Bmp);

                  Bmp := ScalBmp;
                  R := MkRect(Bmp);
                end
                else
                begin
                  Bmp := nil;
                  R := MkRect;
                end;
              end;
            end;
            oed.mask := Size;
          end
          else
            oed.mask := -1;
        end

    else
      begin
        wd2 := oed.Radius;
        Size := wd2 * 2 + 1;

        BodyH := oed.OffsetB + oed.OffsetT;
        BodyW := oed.OffsetL + oed.OffsetB;

        i := max(max(oed.OffsetL, oed.OffsetR), max(oed.OffsetT, oed.OffsetB));
        ResBmp := MakeShadow(0, wd2, 0, BodyW, BodyH, oed.Blur, True);

        if oed.WidthL = -1 then
          oed.WidthL := (ResBmp.Width + oed.OffsetL - oed.OffsetR - 1) div 2;
        if oed.WidthT = -1 then
          oed.WidthT := (ResBmp.Height + oed.OffsetT - oed.OffsetB - 1) div 2;
        if oed.WidthR = -1 then
          oed.WidthR := (ResBmp.Width + oed.OffsetR - oed.OffsetL - 1) div 2;
        if oed.WidthB = -1 then
          oed.WidthB := (ResBmp.Height + oed.OffsetB - oed.OffsetT - 1) div 2;

        ndx := MakeNewItem(i, sPropName, s_GlobalInfo, itisaBorder,
          MkRect(Size, Size * 2), 1, 1, 1);
        with ma[ndx] do
        begin
          Bmp := CreateBmp32(ResBmp.Width, ResBmp.Height * 2);
          WL := oed.WidthL;
          WT := oed.WidthT;
          WR := oed.WidthR;
          WB := oed.WidthB;
          PaintMainLayer(Bmp, MkRect(ResBmp), oed.ColorL, oed.ColorT,
            oed.ColorR, oed.ColorB);
          // Copy mask
          R := MkRect(Bmp);
          BitBlt(Bmp.Canvas.Handle, 0, ResBmp.Height, ResBmp.Width,
            ResBmp.Height, ResBmp.Canvas.Handle, 0, 0, SRCCOPY);
          oed.mask := ndx;
          AddImage(Bmp, 'OuterCalc', ma[ndx].ClassName + '-' +
            ma[ndx].PropertyName, R, MkRect, PPI, ma[ndx].MaskType, True);
        end;
        FreeAndNil(ResBmp);
      end;
    end;
  end;

  procedure FillProps(var Props: TsGenState; const DefProps: TsGenState;
    State: smallint; CommonSection: boolean);
  var
    GradientData: string;
  begin
    if not CommonSection then
    begin
      Props.Color := FindInteger(s, acPropsArray[State, 0], DefProps.Color);
      Props.FontColor.Color := FindInteger(s, acPropsArray[State, 1],
        DefProps.FontColor.Color);
      Props.FontColor.Left := FindInteger(s, acPropsArray[State, 2],
        DefProps.FontColor.Left);
      Props.FontColor.Top := FindInteger(s, acPropsArray[State, 3],
        DefProps.FontColor.Top);
      Props.FontColor.Right := FindInteger(s, acPropsArray[State, 4],
        DefProps.FontColor.Right);
      Props.FontColor.Bottom := FindInteger(s, acPropsArray[State, 5],
        DefProps.FontColor.Bottom);
      Props.GlowColor := FindInteger(s, acPropsArray[State, 6],
        DefProps.GlowColor);
      Props.GlowSize := FindInteger(s, acPropsArray[State, 7],
        DefProps.GlowSize);
      Props.GradientPercent := FindInteger(s, acPropsArray[State, 8],
        DefProps.GradientPercent);
      GradientData := FindString(s, acPropsArray[State, 9], s_Space);
      Props.ImagePercent := FindInteger(s, acPropsArray[State, 10],
        DefProps.ImagePercent);
      Props.Transparency := FindInteger(s, acPropsArray[State, 11],
        DefProps.Transparency);
      Props.LightColor := FindInteger(s, acPropsArray[State, 12],
        FOwner.SkinCommonInfo.DefLightColor);
      Props.BorderColor := FindInteger(s, acPropsArray[State, 13],
        DefProps.BorderColor);
    end
    else
    begin
      Props.Color := GetPropInteger(SectionsList[i], acPropsArray[State, 0],
        DefProps.Color);
      Props.FontColor.Color := GetPropInteger(SectionsList[i],
        acPropsArray[State, 1], DefProps.FontColor.Color);
      Props.FontColor.Left := GetPropInteger(SectionsList[i],
        acPropsArray[State, 2], DefProps.FontColor.Left);
      Props.FontColor.Top := GetPropInteger(SectionsList[i],
        acPropsArray[State, 3], DefProps.FontColor.Top);
      Props.FontColor.Right := GetPropInteger(SectionsList[i],
        acPropsArray[State, 4], DefProps.FontColor.Right);
      Props.FontColor.Bottom := GetPropInteger(SectionsList[i],
        acPropsArray[State, 5], DefProps.FontColor.Bottom);
      Props.GlowColor := GetPropInteger(SectionsList[i], acPropsArray[State, 6],
        DefProps.GlowColor);
      Props.GlowSize := GetPropInteger(SectionsList[i], acPropsArray[State, 7],
        DefProps.GlowSize);
      Props.GradientPercent := GetPropInteger(SectionsList[i],
        acPropsArray[State, 8], DefProps.GradientPercent);
      GradientData := GetPropStr(SectionsList[i],
        acPropsArray[State, 9], s_Space);
      Props.ImagePercent := GetPropInteger(SectionsList[i],
        acPropsArray[State, 10], DefProps.ImagePercent);
      Props.Transparency := GetPropInteger(SectionsList[i],
        acPropsArray[State, 11], DefProps.Transparency);
      Props.LightColor := GetPropInteger(SectionsList[i],
        acPropsArray[State, 12], DefProps.LightColor);
      Props.BorderColor := GetPropInteger(SectionsList[i],
        acPropsArray[State, 13], DefProps.BorderColor);
    end;
    if Length(GradientData) > 3 then
      PrepareGradArray(GradientData, Props.GradientArray)
    else
      Props.GradientArray := DefProps.GradientArray;
  end;

  procedure LoadGlobalData;
  var
    tg: TacTitleGlyph;

    function GlobalInteger(const Name: string; Def: integer): integer;
    begin
      Result := sf.ReadInteger(s_GlobalInfo, Name, Def);
    end;

  begin
    ExDrawMode := GlobalInteger(s_BorderMode, 0);
    ExShadowOffs := ScaledInteger(s_ShadowOffset, 0, True);
    ExShadowOffsR := ScaledInteger(s_ShadowOffsetR, ExShadowOffs, True);
    ExShadowOffsT := ScaledInteger(s_ShadowOffsetT, ExShadowOffs, True);
    ExShadowOffsB := ScaledInteger(s_ShadowOffsetB, ExShadowOffs, True);

    WndShadowRadius := ScaledInteger(s_WndShadowRadius, 17, True);
    WndShadowOffset := ScaledInteger(s_WndShadowOffset, 2, True);
    WndShadowSize := ScaledInteger(s_WndShadowSize, 13, True);

    WndShadowColorN := GlobalInteger(s_WndShadowColorN, TColor($44000000));
    WndShadowColorA := GlobalInteger(s_WndShadowColorA, TColor($99000000));

    BISpacing := ScaledInteger(s_BISpacing, 0, True);

    BIRightMargin := ScaledInteger(s_BIRightMargin, 0, True);
    BILeftMargin := ScaledInteger(s_BILeftMargin, 0, True);
    BITopMargin := ScaledInteger(s_BITopMargin, 0, True);

    for tg := tgCloseAlone to tgNormal do
    begin
      GlowMargins[tg] := ScaledInteger(acTitleGlyphs[tg] +
        s_GlowMargin, 0, True);
      SetLength(TitleGlows[tg], GlobalInteger(s_BorderIconClose + s_Glow, 0));
    end;

    Shadow1Offset := ScaledInteger(s_Shadow1Offset, 0, True);
    Shadow1Blur := ScaledInteger(s_Shadow1Blur, -1, True);
    Shadow1Transparency := GlobalInteger(s_Shadow1Transparency, 0);

    ThumbWidth := ScaledInteger(s_ThumbWidth, 0, True);
    ScrollArrows := GlobalInteger(s_ScrollsArrows, -1);
    ScrollsWidth := GlobalInteger(s_ScrollsWidth, -1);
    if ScrollsWidth <> -1 then
      ScrollsWidth := FOwner.ScaleInt(ScrollsWidth);

    ComboBoxMargin := GlobalInteger(s_ComboMargin, 2);
    if (ComboBoxMargin = 1) and (PPI > 143) then
      ComboBoxMargin := 2;

    SliderMargin := GlobalInteger(s_SliderMargin, 1);
    // Outer effects
    SetLength(oe, 2);
    with oe[1] do
    begin // Main shadow
      ColorT := clFuchsia;

      WidthL := GlobalInteger(s_ShadowWidthL, -1);
      WidthR := GlobalInteger(s_ShadowWidthR, -1);
      WidthT := GlobalInteger(s_ShadowWidthT, -1);
      WidthB := GlobalInteger(s_ShadowWidthB, -1);

      with FOwner do
      begin
        OffsetL := ScaleInt(GlobalInteger(s_ShadowOffsL, 3));
        OffsetR := ScaleInt(GlobalInteger(s_ShadowOffsR, 3));
        OffsetT := ScaleInt(GlobalInteger(s_ShadowOffsT, 2));
        OffsetB := ScaleInt(GlobalInteger(s_ShadowOffsB, 5));

        Radius := ScaleInt(GlobalInteger(s_ShOERadius, 9));
        Blur := ScaleInt(GlobalInteger(s_ShOEBlur, 5));
      end;

      ColorL := GlobalInteger(s_ShadowColorL, 0);
      Opacity := GlobalInteger(s_ShadowOpacity, 60);

      Source := GlobalInteger(s_ShadowSource, 2);
      MakeCommonMask(oe[1], s_ShadowMask);
      with FOwner do
      begin
        WidthL := ScaleInt(WidthL);
        WidthR := ScaleInt(WidthR);
        WidthT := ScaleInt(WidthT);
        WidthB := ScaleInt(WidthB);
      end;
    end;
    with oe[0] do
    begin // Main lowered bevel
      ColorL := acColorToRGB(GlobalInteger(s_LowColorL, $FFFFFF));
      ColorR := acColorToRGB(GlobalInteger(s_LowColorR, $FFFFFF));
      ColorT := acColorToRGB(GlobalInteger(s_LowColorT, 0));
      ColorB := acColorToRGB(GlobalInteger(s_LowColorB, $FFFFFF));

      WidthL := GlobalInteger(s_LowWidthL, -1);
      WidthR := GlobalInteger(s_LowWidthR, -1);
      WidthT := GlobalInteger(s_LowWidthT, -1);
      WidthB := GlobalInteger(s_LowWidthB, -1);

      with FOwner do
      begin
        OffsetL := ScaleInt(GlobalInteger(s_LowOffsL, 2));
        OffsetR := ScaleInt(GlobalInteger(s_LowOffsR, 2));
        OffsetT := ScaleInt(GlobalInteger(s_LowOffsT, 2));
        OffsetB := ScaleInt(GlobalInteger(s_LowOffsB, 4));

        Radius := ScaleInt(GlobalInteger(s_LoOERadius, 9));
        Blur := ScaleInt(GlobalInteger(s_LoOEBlur, 5));
      end;

      Source := GlobalInteger(s_LowSource, 2);
      Opacity := GlobalInteger(s_LowOpacity, 200);

      MakeCommonMask(oe[0], s_LowMask);
      with FOwner do
      begin
        WidthL := ScaleInt(WidthL);
        WidthR := ScaleInt(WidthR);
        WidthT := ScaleInt(WidthT);
        WidthB := ScaleInt(WidthB);
      end;
    end;
  end;

begin
  FillChar(gData, SizeOf(gData), 0);
  sf := FOwner.SkinFile;
  if sf <> nil then
  begin
    TabsCovering := sf.ReadInteger(s_TabTop, s_TabsCovering, 0);
    RibbonCovering := sf.ReadInteger(s_RibbonTab, s_TabsCovering, 0);

    SectionsList := TStringList.Create;
    SetCaseSens(SectionsList);
    try
      SetLength(gd, 0);
      sf.ReadSections(SectionsList);
      for i := 0 to SectionsList.Count - 1 do
      begin
        s := SectionsList[i];
        l := Length(gd) + 1;
        SetLength(gd, l);
        // General data
        with gd[i] do
        begin
          ClassName := SectionsList[i];
          ParentClass := sf.ReadString(s, s_ParentClass, '');
          ReservedBoolean := FindBoolean(s, s_ReservedBoolean, False);
          GiveOwnFont := FindBoolean(s, s_GiveOwnFont, False);
          ShiftOnClick := FindBoolean(s, s_ShiftOnClick, False);

          GlowCount := FindInteger(s, s_Glow, 0, MaxByte);
          GlowMargin := FindInteger(s, s_GlowMargin, 0, MaxByte);
          BorderRadius := FOwner.ScaleInt(FindInteger(s, s_BorderRadius, 0,
            MaxByte), ScaleValue);
          if IsValidIndex(BorderIndex, Length(ma)) then
            gd[i].GlowMargin := MulDiv(GlowMargin,
              ma[gd[i].BorderIndex].MulPPI, 96);

          States := FindInteger(s, s_States, 3, MaxByte);

          OuterOffset.Left := FOwner.ScaleInt(FindInteger(s, s_OuterOffsL, 2,
            MaxByte, True), ScaleValue);
          OuterOffset.Top := FOwner.ScaleInt(FindInteger(s, s_OuterOffsT, 2,
            MaxByte, True), ScaleValue);
          OuterOffset.Right := FOwner.ScaleInt(FindInteger(s, s_OuterOffsR, 2,
            MaxByte, True), ScaleValue);
          OuterOffset.Bottom := FOwner.ScaleInt(FindInteger(s, s_OuterOffsB, 4,
            MaxByte, True), ScaleValue);

          OuterOpacity := FindInteger(s, s_OuterOpacity, 200, MaxByte);
          OuterMode := FindInteger(s, s_OuterMode, 2); // Shadowed by definition

          OuterBlur := FOwner.ScaleInt(FindInteger(s, s_OuterBlur, 9, 30),
            ScaleValue);
          OuterRadius := FOwner.ScaleInt(FindInteger(s, s_OuterRadius, 9, 60),
            ScaleValue);

          OuterColorL := FindInteger(s, s_OuterColorL, 0);
          OuterColorT := FindInteger(s, s_OuterColorT, 0);
          OuterColorR := FindInteger(s, s_OuterColorR, 0);
          OuterColorB := FindInteger(s, s_OuterColorB, 0);

          BorderWidth := FindInteger(s, s_BorderWidth, 1);
          // Width of painted border

          ImgTL := -1;
          ImgTR := -1;
          ImgBL := -1;
          ImgBR := -1;

          FillProps(Props[0], acDefProp, 0, False);

          if States > 1 then
            FillProps(Props[1], acDefProp, 1, False)
          else
            CopyProperties(Props[1], Props[0]); // For back/w compatibility

          UseState2 := FindInteger(s, s_UseState2, 0) = 1;
          if UseState2 then
            FillProps(Props[2], acDefProp, 2, False)
          else
            CopyProperties(Props[2], Props[1]); // For back/w compatibility
        end;
      end;
    finally
      FreeAndNil(SectionsList);
    end;
  end;

  if FOwner.CommonSections.Count > 0 then
  begin
    // Init default data
    with acDefGenData do
    begin
      for i := 0 to ac_MaxPropsIndex do
        Props[i] := acDefProp;

      States := 3;
      GiveOwnFont := False;
      ReservedBoolean := False;
      GlowCount := 0;
      GlowMargin := 0;
      BorderWidth := 0;
      BorderRadius := 0;
    end;
    SectionsList := TStringList.Create;
    SetCaseSens(SectionsList);
    GetIniSections(FOwner.CommonSections, SectionsList);
    try
      for i := 0 to SectionsList.Count - 1 do
      begin
        l := Length(gd);
        gData.ClassName := '';
        SkinIndex := -1;
        for j := 0 to l - 1 do // Search if exists already
          if gd[j].ClassName = SectionsList[i] then
          begin
            gData := gd[j];
            SkinIndex := j;
            Break;
          end;

        Ini := nil;
        ParentIndex := -1;
        if gData.ClassName = '' then
        begin // Make new item
          l := Length(gd) + 1;
          SetLength(gd, l);
          gData.ClassName := SectionsList[i];
          Ini := FOwner.CommonSections;
          gData.ParentClass := UpperCase(ReadIniString(Ini, SectionsList,
            gData.ClassName, s_ParentClass, s_MinusOne));
          if gData.ParentClass <> '-1' then
            for j := 0 to Length(gd) - 1 do // Search parent index
              if UpperCase(gd[j].ClassName) = gData.ParentClass then
              begin
                ParentIndex := j;
                Break;
              end;
        end;
        // General data
        if Ini <> nil then
        begin
          if ParentIndex >= 0 then
            gTempData := gd[ParentIndex]
          else
            gTempData := acDefGenData;

          // Copy parent or default data
          gData.States := GetPropInteger(SectionsList[i], s_States,
            gTempData.States);
          // Normal
          FillProps(gData.Props[0], gTempData.Props[0], 0, True);
          // Hot
          if gData.States > 1 then
            FillProps(gData.Props[1], gTempData.Props[1], 1, True)
          else
            CopyProperties(gData.Props[1], gData.Props[0]);

          // Pressed
          j := GetPropInteger(SectionsList[i], s_UseState2, -1);
          if j < 0 then
            gd[i].UseState2 := FindInteger(gData.ParentClass,
              s_UseState2, -1) = 1
          else
            gd[i].UseState2 := (j = 1);

          if gd[i].UseState2 then
            FillProps(gData.Props[2], gTempData.Props[2], 2, True)
          else
            CopyProperties(gData.Props[2], gData.Props[1]);

          gData.ReservedBoolean := GetPropBool(SectionsList[i],
            s_ReservedBoolean, gTempData.ReservedBoolean);
          gData.GiveOwnFont := GetPropBool(SectionsList[i], s_GiveOwnFont,
            gTempData.GiveOwnFont);
          gData.GlowCount := GetPropInteger(SectionsList[i], s_Glow,
            gTempData.GlowCount);
          gData.GlowMargin := GetPropInteger(SectionsList[i], s_GlowMargin,
            gTempData.GlowMargin);
          gData.BorderWidth := GetPropInteger(SectionsList[i], s_BorderWidth,
            gTempData.BorderWidth);
          gData.BorderRadius := GetPropInteger(SectionsList[i], s_BorderRadius,
            gTempData.BorderRadius);
          s := GetPropStr(SectionsList[i], s_BordersMask, ')');
          if s <> ')' then // If own borders mask exist
            if Length(s) < 5 then
              gData.BorderIndex := -2; // Do not use

          if gData.ClassName <> '' then
          begin
            if SkinIndex >= 0 then
              gd[SkinIndex] := gData
            else
              gd[l - 1] := gData;

            gData.ClassName := '';
          end;
        end;
      end;
    finally
      if Assigned(SectionsList) then
      begin
        with SectionsList do
          while Count > 0 do
          begin
            if Objects[0] <> nil then
              TStringList(Objects[0]).Free;

            Delete(0);
          end;

        FreeAndNil(SectionsList);
      end;
    end;
  end;
  with FOwner do
  begin
    // Add TRANSPARENT skin section
    if GetSkinIndex(s_Transparent, Self) < 0 then
      AddNewSection(s_Transparent, -1, 1, 100, $FFFFFF, Palette[pcLabelText]);

    // Add MAINCOLOR skin section
    if GetSkinIndex(s_MainColor, Self) < 0 then
      AddNewSection(s_MainColor, -1, 1, 0, Palette[pcMainColor],
        Palette[pcLabelText]);
  end;
  LoadGlobalData;
end;

procedure TacSkinData.FreeBitmaps;
begin
  ClearBmpArray;
end;

procedure TacSkinData.ClearBmpArray;
var
  i: integer;
begin
  for i := 0 to Length(BmpArray) - 1 do
    FreeAndNil(BmpArray[i].Bmp);

  SetLength(BmpArray, 0);
end;

var
  ic: integer;

procedure ClearStdScaleArray;
var
  i: integer;
begin
  for i := 0 to acMaxScaleNdx do
    StdScaleArray[i].Free;
end;

destructor TacAddThread.Destroy;
begin
  TimeEvent.Free;
  if FOwner.Thread = Self then
  begin
    if FOwner.FileInfo.Name <> '' then
    begin
      FindClose(FOwner.FileInfo);
      FOwner.FileInfo.Name := '';
    end;

    FOwner.Thread := nil;
  end;

  inherited;
end;

procedure TacAddThread.DoTimer;
var
  sPath: string;
begin
  if not Terminated then
  begin
    if FOwner.SkinsAllowed(sfiInternal) and
      (FOwner.CurrentInternalIndex < FOwner.SkinManager.InternalSkins.Count)
    then
    begin // Internal skins are not processed yet
      FOwner.AddSkin(FOwner.SkinManager.InternalSkins
        [FOwner.CurrentInternalIndex].Name, FOwner.CurrentInternalIndex);
      inc(FOwner.CurrentInternalIndex);
    end
    else if FOwner.SkinsAllowed(sfiExternal) then
    begin
      if FOwner.FileInfo.Name = '' then
      begin // If files searching was not started yet
        sPath := NormalDir(FOwner.SkinManager.GetFullSkinDirectory);
        if acDirExists(sPath) then
          FOwner.DosCode := acFindFirst(sPath + '*.*', faDirectory,
            FOwner.FileInfo);
      end;
      FOwner.DosCode := FOwner.AddSkinData;
      if FOwner.DosCode <> 0 then
      begin
        FOwner.ClearFileInfo;
        Terminate;
        FOwner.UpdateNeeded := False;
        FOwner.SendListChanged;
      end;
    end;
  end;
end;

procedure TacAddThread.Execute;
var
  term_cause: TWaitResult;
begin
  repeat
    term_cause := TimeEvent.WaitFor(acTimerInterval);
    if term_cause = wrTimeout then
      Synchronize(DoTimer);
  until Terminated or (term_cause <> wrTimeout) or Application.Terminated;
end;

procedure TacBtnEffects.SetLightingMode(const Value: TacLightingMode);
begin
  FLightingMode := Value;
  if Manager.IsDefault and not(csDesigning in Manager.ComponentState) then
    SetMouseLight(Value = lmAffectNearest);
end;

initialization

OSVerInfo.dwOSVersionInfoSize := SizeOf(OSVerInfo);
GetVersionEx(OSVerInfo);
IsNT := OSVerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT;

for ic := 0 to acMaxScaleNdx do
begin
  StdScaleArray[ic] := TacSkinData.Create(nil, aPPIArray[ic]);
  StdScaleArray[ic].ScaleValue := ic;
  StdScaleArray[ic].LoadParams;
end;

if ParamCount > 0 then
  for ic := 1 to ParamCount do
    if LowerCase(ParamStr(ic)) = '/acver' then
    begin
      ShowMessage('AlphaControls v' + acCurrentVersion);
      Break;
    end;

finalization

ClearStdScaleArray;

end.
