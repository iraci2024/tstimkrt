unit FlatUtils;

interface

{$I FlatStyle.inc}

uses Windows, Classes, Graphics, Messages, Controls, Forms, ShellAPI,
     StdCtrls, ComCtrls, SysUtils, DBGrids, Grids, ExtCtrls;

const
  FileVersion   = '4.56.0.0';
  FileCopyright = '? 1998-2010';
  FileFinish    = '2010-07-02';
  CompilePlat   = {$IFDEF VER80}  'Delphi 1.0'{$ENDIF}
                  {$IFDEF VER90}  'Delphi 2.0'{$ENDIF}
                  {$IFDEF VER100} 'Delphi 3.0'{$ENDIF}
                  {$IFDEF VER120} 'Delphi 4.0'{$ENDIF}
                  {$IFDEF VER130} 'Delphi 5.0'{$ENDIF}
                  {$IFDEF VER140} 'Delphi 6.0'{$ENDIF}
                  {$IFDEF VER150} 'Delphi 7.0'{$ENDIF}
                  {$IFDEF VER160} 'Delphi 8.0'{$ENDIF}
                  {$IFDEF VER170} 'Delphi 2005'{$ENDIF}
                  {$IFDEF VER180} 'Delphi 2006'{$ENDIF}
                  {$IFDEF VER190} 'Delphi 2007'{$ENDIF}
                  {$IFDEF VER200} 'Delphi 2009'{$ENDIF}
                  {$IFDEF VER210} 'Delphi 2010'{$ENDIF}
                  {$IFDEF VER220} 'Delphi XE'{$ENDIF}
                  {$IFDEF VER230} 'Delphi XE2'{$ENDIF}
                  {$IFDEF VER240} 'Delphi XE3'{$ENDIF}
                  {$IFDEF VER250} 'Delphi XE4'{$ENDIF}
                  {$IFDEF VER260} 'Delphi XE5'{$ENDIF}
                  {$IFDEF VER270} 'Delphi XE6'{$ENDIF}
                  {$IFDEF VER280} 'Delphi XE7'{$ENDIF}
                  {$IFDEF VER290} 'Delphi XE8'{$ENDIF}
                  {$IFDEF VER300} 'Delphi XE10'{$ENDIF}
                  {$IFDEF VER310} 'Delphi XE10.1'{$ENDIF}
                  {$IFDEF VER93}  'C++Builder 1.0'{$ENDIF}
                  {$IFDEF VER110} 'C++Builder 3.0'{$ENDIF}
                  {$IFDEF VER125} 'C++Builder 4.0'{$ENDIF} ;
  //定义 控件标签 开关 值:True为显示,False为禁止
  DefaultHasTicket = True;

  {以下定义 MessageMyBox函数的Flags标识}
  SB_INF_BASE= MB_ICONINFORMATION;{SB_INF_BASE For Information Hint}
  SB_WAR_BASE= MB_ICONWARNING;{SB_WAR_BASE For Warning Hint}
  SB_ERR_BASE= MB_ICONERROR; {SB_ERR_BASE For Error Hint}
  SB_QUE_BASE= MB_ICONQUESTION; {SB_QUE_BASE For Stop Hint}
  {define mb_inconinformtion}
  mbIAll    = SB_INF_BASE+MB_ABORTRETRYIGNORE;
  mbIOk     = SB_INF_BASE+MB_OK;
  mbIOC     = SB_INF_BASE+MB_OKCANCEL;
  mbIRC     = SB_INF_BASE+MB_RETRYCANCEL;
  mbIYN     = SB_INF_BASE+MB_YESNO;
  mbIYNC    = SB_INF_BASE+MB_YESNOCANCEL;
  {define mb_inconwarning}
  mbWAll    = SB_WAR_BASE+MB_ABORTRETRYIGNORE;
  mbWOk     = SB_WAR_BASE+MB_OK;
  mbWOC     = SB_WAR_BASE+MB_OKCANCEL;
  mbWRC     = SB_WAR_BASE+MB_RETRYCANCEL;
  mbWYN     = SB_WAR_BASE+MB_YESNO;
  mbWYNC    = SB_WAR_BASE+MB_YESNOCANCEL;
  {define mb_inconerror}
  mbEAll    = SB_ERR_BASE+MB_ABORTRETRYIGNORE;
  mbEOk     = SB_ERR_BASE+MB_OK;
  mbEOC     = SB_ERR_BASE+MB_OKCANCEL;
  mbERC     = SB_ERR_BASE+MB_RETRYCANCEL;
  mbEYN     = SB_ERR_BASE+MB_YESNO;
  mbEYNC    = SB_ERR_BASE+MB_YESNOCANCEL;
  {define mb_inconstop}
  mbQAll    = SB_QUE_BASE+MB_ABORTRETRYIGNORE;
  mbQOk     = SB_QUE_BASE+MB_OK;
  mbQOC     = SB_QUE_BASE+MB_OKCANCEL;
  mbQRC     = SB_QUE_BASE+MB_RETRYCANCEL;
  mbQYN     = SB_QUE_BASE+MB_YESNO;
  mbQYNC    = SB_QUE_BASE+MB_YESNOCANCEL;
   { pause before repeat timer (ms) }
  FlatInitRepeatPause      = 400;
   { pause before hint window displays (ms)}
  FlatRepeatPause          = 100;
  //以下定义FlatGuiListBox常量
  //鼠标滚轮改变 TopIndex 大小:
  C_MouseWheelSize         = 3;
  C_WheelWait              = 80;
  //时间 ID:  //基层 TimerID
  C_BaseTimerID            = 1024 * 512;
    //鼠标滑轮等待时间 ID:
  C_WheelWaitTimerID       = C_BaseTimerID + 1;
    //鼠标拖动改变页面时间 ID
  C_MouseChangePageTimerID = C_BaseTimerID + 2;
  //以下两个常量控制着动画速度:
  //最大 Sleep 数量:
  C_SleepMaxCount          = 20;
  //系统等待时间:
  C_MaxInterval            = 200;
  { ScrollBar }
  C_Win2000ScrllBarBtnSize = 16;
  C_IntervalOfWait         = 500;
  C_Interval               = 50;

  DefaultInitRepeatPause   = 400;  { pause before repeat timer (ms) }
  DefaultRepeatPause       = 100;  { pause before hint window displays (ms)}

const
  TCS_SCROLLOPPOSITE    = $0001;  // assumes multiline tab
  TCS_MULTISELECT       = $0004;  // allow multi-select in button mode
  TCS_FORCEICONLEFT     = $0010;
  TCS_FORCELABELLEFT    = $0020;
  TCS_HOTTRACK          = $0040;
  TCS_RIGHT             = $0002;
  TCS_VERTICAL          = $0080;
  TCS_TABS              = $0000;
  TCS_BUTTONS           = $0100;
  TCS_FLATBUTTONS       = $0008;
  TCS_OWNERDRAWFIXED    = $2000;
  TCS_BOTTOM            = $0002;
  TCS_SINGLELINE        = $0000;
  TCS_MULTILINE         = $0200;
  TCS_RIGHTJUSTIFY      = $0000;
  TCS_FIXEDWIDTH        = $0400;
  TCS_RAGGEDRIGHT       = $0800;
  TCS_FOCUSONBUTTONDOWN = $1000;

  TCS_TOOLTIPS          = $4000;
  TCS_FOCUSNEVER        = $8000;

  TCS_EX_FLATSEPARATORS = $00000001;
  TCS_EX_REGISTERDROP   = $00000002;

  TCM_FIRST               = $1300;      { Tab control messages }

  TCM_GETIMAGELIST       = TCM_FIRST + 2;
  TCM_SETIMAGELIST       = TCM_FIRST + 3; 
  TCM_GETITEMCOUNT       = TCM_FIRST + 4;
  TCM_DELETEITEM         = TCM_FIRST + 8;
  TCM_DELETEALLITEMS     = TCM_FIRST + 9;
  TCM_GETITEMRECT        = TCM_FIRST + 10;
  TCM_GETCURSEL          = TCM_FIRST + 11;
  TCM_SETCURSEL          = TCM_FIRST + 12;
  TCM_HITTEST            = TCM_FIRST + 13;
  TCM_SETITEMEXTRA       = TCM_FIRST + 14;
  TCM_ADJUSTRECT         = TCM_FIRST + 40;
  TCM_SETITEMSIZE        = TCM_FIRST + 41;
  TCM_REMOVEIMAGE        = TCM_FIRST + 42;
  TCM_SETPADDING         = TCM_FIRST + 43;
  TCM_GETROWCOUNT        = TCM_FIRST + 44;
  TCM_GETTOOLTIPS        = TCM_FIRST + 45;
  TCM_SETTOOLTIPS        = TCM_FIRST + 46;
  TCM_GETCURFOCUS        = TCM_FIRST + 47;
  TCM_SETCURFOCUS        = TCM_FIRST + 48;
  TCM_SETMINTABWIDTH     = TCM_FIRST + 49;
  TCM_DESELECTALL        = TCM_FIRST + 50;
  TCM_HIGHLIGHTITEM      = TCM_FIRST + 51;
  TCM_SETEXTENDEDSTYLE   = TCM_FIRST + 52;  // optional wParam == mask
  TCM_GETEXTENDEDSTYLE   = TCM_FIRST + 53;

  TCIF_TEXT       = $0001;
  TCIF_IMAGE      = $0002;
  TCIF_RTLREADING = $0004;
  TCIF_PARAM      = $0008;
  TCIF_STATE      = $0010;

  TCIS_BUTTONPRESSED      = $0001;
  TCIS_HIGHLIGHTED        = $0002;

  TCM_GETITEMA             = TCM_FIRST + 5;
  TCM_SETITEMA             = TCM_FIRST + 6;
  TCM_INSERTITEMA          = TCM_FIRST + 7;
  TCM_GETITEMW             = TCM_FIRST + 60;
  TCM_SETITEMW             = TCM_FIRST + 61;
  TCM_INSERTITEMW          = TCM_FIRST + 62;

  TCM_GETITEM             = TCM_GETITEMA;
  TCM_SETITEM             = TCM_SETITEMA;
  TCM_INSERTITEM          = TCM_INSERTITEMA;

  
// tab styles - search win32 api help for TCS_ for info on each style
type
  TPagesPosition = (tpTop, tpBottom, tpLeft, tpRight);

  TPagesStyle = (pcsTabs, pcsButtons, pcsFlatButtons, pcsFlatStyle);

  tagTCITEMA = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

  tagTCITEMW = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

  TTCItemA = tagTCITEMA;
  TTCItemW = tagTCITEMW;
  TTCItem = TTCItemA;

const
  TCHT_NOWHERE     = $0001;
  TCHT_ONITEMICON  = $0002;
  TCHT_ONITEMLABEL = $0004;
  TCHT_ONITEM      = TCHT_ONITEMICON or TCHT_ONITEMLABEL;

type
  PTCHitTestInfo = ^TTCHitTestInfo;
  tagTCHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
  end;

  _TC_HITTESTINFO = tagTCHITTESTINFO;
  TTCHitTestInfo = tagTCHITTESTINFO;
  TC_HITTESTINFO = tagTCHITTESTINFO;

  tagTCKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;
  _TC_KEYDOWN = tagTCKEYDOWN;
  TTCKeyDown = tagTCKEYDOWN;
  TC_KEYDOWN = tagTCKEYDOWN;


// event to allow different mapping of glyphs from the imagelist component
type
  TGlyphMapEvent = procedure(Control: TWinControl; PageIndex : integer; var GlyphIndex : integer) of object;

  TPageDrawItemEvent = procedure(Control: TWinControl; Index: Integer; ACanvas : TControlCanvas;
    ARect: TRect; State: TOwnerDrawState) of object;

type
  TButtonLayout    = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState     = (bsUp, bsDisabled, bsDown, bsExclusive);
  TButtonStyle     = (bsAutoDetect, bsWin31, bsNew);
  TWaterAlign      = (wpLeft,wpCenter,wpRight);
  TFlatDISModal    = (tmAboriginal, tmStretch, tmNormal, tmCenter);
  {Define TDefainePanelEx type}    
  TBGImageAlign    = (iaCenter, iaStretch, iaTile); //Background image align type
  TTitleImageAlign = (tiaCenter, tiaLeft, tiaRight, tiaStretch, tiaTile); //Title image align type
  TTitleButton     = (tbClose, tbMinimize, tbMaximize); //Title buttons
  TTitleButtons    = Set of TTitleButton;
  //Rounded corner type
  TPanelCorner     = (rcTopLeft, rcTopRight, rcBottomLeft, rcBottomRight);
  TPanelCorners    = Set of TPanelCorner;
  TFlatDBBStyle    = set of (myAllowTimer, myFocusRect);
  TFillDirection   = (fdBottomToTop,fdCenterToVerti,fdCenterToHoriz,fdLeftToRight,fdRightToLeft,fdTopToBottom,fdXPFace);
  TFlatDBBName     = (vbFirst,vbPrior,vbNext,vbLast,vbNew,vbDelete,vbEdit,vbSave,vbCancel,vbRefresh);
  TFlatDBBTSet     = set of TFlatDBBName;
  EFlatBroClick    = procedure (Sender: TObject; Button: TFlatDBBName) of object;
  TNumGlyphs       =  1..4;
  TAdvColors       =  0..100;
   //以下定义FlatGuiListBox类型
  //自定义的 ScrollCode 枚举
  TIScrollCode = (scLarge, scSmall, scTrackMove, scCustom);
  //绘制枚举:
  TDrawScrollBar = (dsLeftBtn, dsRightBtn, dsTrack, dsSpaceLeft, dsSpaceRight);
  TDrawArrow = (daLeft, daTop, daRight, daBottom);
  //ScrollBar 整个结构的枚举:
  TScrollBarPos = (spNone, spLeftBtn, spRightBtn, spTrack, spLeftSpace, spRightSpace);
  //滑动方向枚举:
  TScrollMode = (smAdd,smDec);
  TScrollEvent = procedure(Sender: TObject; const StartChange:boolean; Code:TIScrollCode; Mode:TScrollMode; const ChangeValue: integer) of Object;
  TScrollDrawEvent = procedure(Cav: TCanvas; const Typ: TDrawScrollBar; const R: TRect; const State: TButtonState) of Object;
  //滑轮记录:
  TListControlWheel = record
    Wheeling,
    IsAdd: boolean;
    WheelCount: integer;
  end;

  //键盘改变页面枚举
  TKeyFirst = (kfNone,kfUp,kfDown,kfPrior,kfNext);
  //鼠标改变页面枚举
  TMouseChangePage = (cpNone,cpAddMin,cpAddNormal,cpAddMax,cpDecMin,cpDecNormal,cpDecMax);

  TListItemEvent = procedure(Sender: TObject; const Index: integer) of Object;

  TListItemState = (isActive, isSelected, isDown, isUp,isDisabled, isFocused);
  TListItemStates = set of TListItemState;

  TListItemDrawEvent = procedure(Cav: TCanvas; const Index: Integer;
                        const R: TRect; const State: TListItemStates) of object;
  TListControlGUI = (lcgLowered, lcgFlat, lcgNone); 
                        
  //定义颜色语言结构
  TColorItems = packed record
    Value: TColor;
    cnName, enName: PAnsiChar;
  end;
  TIPChar = string[3];
  //定义IP分段函数
  TIP = packed Record
    NO1, NO2, NO3, NO4:TIPChar;
  end;
  TBarsRect = packed record
    PrevRect:TRect;
    DownRect:TRect;
  end;
  TWaterColor = packed record
    Value: TColor;
    enName: PAnsiChar;
  end;
  TBorderAttrib = packed record
    Ctrl: TWinControl;
    BorderColor: TColor;
    FlatColor: TColor;
    FocusColor: TColor;
    MouseState: Boolean;
    DesignState: TComponentState;
    FocusState: boolean;
    HasBars: boolean;
    BoldState:Boolean;
  end;
  TOtherParam = packed record
    Color: TColor;
    Name: TFontName;
    Pitch: TFontPitch;
    Size: Integer;
    Style: TFontStyles;
    Row: Integer;
    Draw3D: Boolean;
    Align: TWaterAlign;
  end;
  TScrollType        = (stUp, stDown);
  TColorCalcType     = (lighten, darken);
  TLayoutPosition    = (lpLeft, lpRight);
  TFlatTabPosition   = (fpTop, fpBottom);
  TArrowPos          = (NE, NW, SE, SW);
  TGroupBoxBorder    = (brFull, brOnlyTopLine);
  TTransparentMode   = (tmAlways, tmNotFocused, tmNone);
  TLanguage          = (lgChinese, lgEnglish);
  TStyleOrien        = (bsHorizontal, bsVertical);
  TStyleFace         = (fsDefault, fsCustom);
  TAlignmentText     = (stLeft, stCenter, stRight);
  TCheckPosition     = (bpLeft, bpRight);
  TTitlePosition     = (tsTop, tsBottom);
  TTicketPosition    = (poLeft, poTop, poRight, poBottom);
  TSplitterStatus    = (ssIn, ssOut);
  TListState         = (lsClear,lsFree);
  TTitleButtonsStyle = (tbsEllipse,tbsRectangle);
  TAnimationLayout   = (alAcross, alDown);
  //define Events procedure
  TNotifyChange      = procedure(Sender: TObject; Text:TCaption) of object;
  TNotifyClick       = procedure(Sender: TObject; Text:TCaption) of object;
  TValidateEvent     = Procedure(Sender: TObject) of Object;
  TOnFrameChange     = procedure(Sender: TObject; FrameNumber: Integer) of object;
  { 玻璃渐变API的声明 }
  PTriVertex = ^TTriVertex;
  TTriVertex = packed record
    x: Longint;
    y: Longint;
    Red: WORD;
    Green: WORD;
    Blue: WORD;
    Alpha: WORD;
 end;
 {
 TSystemTime = record
　wYear: Word;
　wMonth: Word;
　wDayOfWeek: Word;
　wDay: Word;
　wHour: Word;
　wMinute: Word;
　wSecond: Word;
　wMilliseconds: Word;
 end;
 }
 //TTriVertex = _TTriVertex;
 {渐变方向: 从左到右，从上到下}
  TGradDirection = (gdLeftRight, gdTopBottom);
  TGradWay = (gwLRWay, gwTBWay);
  { 玻璃效果的颜色配置 }
  TGlassColorCfg = record
    OBorder,       //外框，如果为clNone将不绘制
    IBorder,       //内框，如果为clNone将不绘制
    G1Start,       //上半部分渐变的开始颜色
    G1End,         //上半部分渐变的结束颜色
    G2Start,       //下半部分渐变的开始颜色
    G2End: TColor;  //下半部分渐变的结束颜色
    Style: TGradDirection;//定义方向
    Way: TGradWay;//定义反转
  end;
  {TDefineRLE}
  LongType = record
    case Word of
      0: (Ptr: Pointer);
      1: (Long: LongInt);
      2: (Lo: Word; Hi: Word);
  end;
type
  TDefineBarcodeLines     = (ltWhite,ltBlack, ltblack_half);
  //定义条形码类型
  TDefineBarcodeType  =(Code25IL, Code25IT, Code25Mx, Code39,
                   Code39Ext, Code128A, Code128B, Code128C,
                   Code93, Code93Ext, CodeMSI, PostNet, Codabar,
                   EAN8, EAN13, EAN128A, EAN128B, EAN128C,
                   UPC_A, UPC_EODD, UPC_EVEN, UPC_S2, UPC_S5);
  TDefineBarcodeRotation =(raNone,ra090,ra180,ra270);
  TDefineBarcodeModules = array[0..3] of ShortInt;
  TCode93 = record
     c : char;
     data : array[0..5] of char;
  end;
  TCode39 = record
     c : char;
     data : array[0..9] of char;
     chk: shortint;
  end;
  TCode128 = record
     a, b : char;
     c : string[2];
     data : string[6];
  end;
  TCodabar = record
     c : char;
     data : array[0..6] of char;
  end;
  TBCData = record
     Name:string;        { Name of Barcode }
     num :Boolean;       { numeric data only }
  end; 

const
  //定义Style属性的初始化值
  DefaultBarColor         = TColor($00C5D6D9);
  DefaultBorderColor      = TColor($0061A588);
  DefaultShadowColor      = TColor($00C6C600);
  DefaultFlatColor        = TColor($00E1EAEB);
  DefaultTitleFaceColor   = TColor($0000CECE);
  DefaultTitleCheckColor  = TColor($00FF8000);
  DefaultFocusedColor     = TColor($00FBBE99);
  DefaultCheckBorderColor = TColor($008396A0);
  DefaultCheckColor       = TColor($00FF0080);
  DefaultDownColor        = TColor($00C5D6D9);  
  DefaultColorStart       = TColor($00FBF1ED);
  DefaultColorStop        = TColor($00F7DFD6);
  DefaultTitleColorStart  = TColor($00FFFFFF);
  DefaultTitleColorEnd    = TColor($00F0BDAA);
  DefaultFoisColor        = TColor($00E10000);
  DefaultItemSelectColor  = TColor($00EED2C1);
  DefaultItemBrightColor  = TColor($004F4F4F);
  DefaultItemColor        = TColor($00404040);
  DefaultItemSpaceColor   = TColor($00D6924E);
  DefaultItemRectColor    = clWhite;
  DefaultBackdropColor    = clWhite;
  DefaultCheckBackColor   = clWhite;
  DefaultCheckSelectColor = clPurple;
  DefaultSelectStartColor = clBlack;
  DefaultSelectStopColor  = clWhite;
  DefaultItemColorStart   = clOlive;
  DefaultTitleColor       = clBtnFace;
  DefaultItemLineColor    = clGray;
  DefaultItemColorStop    = clWhite;
  DefaultStyleVertical    = bsVertical;
  DefaultStyleHorizontal  = bsHorizontal;
  DefaultStyleFace        = fsDefault;
  DefaultItemHeight       = 17;
  DefaultBarsHeight       = 12;
  DefaultTitleHeight      = 18;
  DefaultCornerRadius:Integer = 10;
  //定义键盘控制
  vk_selall               = $41;//全选 Ctrl+A
  vk_selcancel            = $5A;//取消全选 Ctrl+Z
  //定义颜色语言默认
  clCustom                = TColor($4080FF);
  StdColorCount           = 18;
  bkModeTRANSPARENT       = 1;
  StdCustomCN             = '自定';
  StdCustomEN             = 'Custom';
  StdColorCN              = '颜色:';
  StdColorEN              = 'Color:';  
  StdColors: array [0..StdColorCount] of TColorItems = (
  {00}(Value:clBlack;       cnName:'黑色';  enName:'Black'  ),
  {01}(Value:clWhite;       cnName:'白色';  enName:'White'  ),
  {02}(Value:clYellow;      cnName:'黄色';  enName:'Yellow' ),
  {03}(Value:clRed;         cnName:'红色';  enName:'Red'    ),
  {04}(Value:clFuchsia;     cnName:'紫红';  enName:'Fuchsia'),
  {05}(Value:clMaroon;      cnName:'栗色';  enName:'Maroon' ),
  {06}(Value:clGreen;       cnName:'绿色';  enName:'Green'  ),
  {07}(Value:clAqua;        cnName:'浅绿';  enName:'Aqua'   ),
  {08}(Value:clMoneyGreen;  cnName:'金绿';  enName:'MoneyGreen'),
  {09}(Value:clBlue;        cnName:'蓝色';  enName:'Blue'   ),
  {10}(Value:clTeal;        cnName:'深蓝';  enName:'Teal'   ),
  {11}(Value:clSkyBlue;     cnName:'天蓝';  enName:'SkyBlue'),
  {12}(Value:clOlive;       cnName:'橄榄';  enName:'Olive'  ),
  {13}(Value:clNavy;        cnName:'藏青';  enName:'Navy'   ),
  {14}(Value:clPurple;      cnName:'紫色';  enName:'Purple' ),
  {15}(Value:clGray;        cnName:'灰色';  enName:'Gray'   ),
  {16}(Value:clSilver;      cnName:'银灰';  enName:'Silver' ),
  {17}(Value:clLime;        cnName:'青色';  enName:'Lime'   ),
  {18}(Value:clCustom;      cnName:'自定';  enName:'Custom'));

  //定义 输入类控件 的输入位置
  Aligns:array[TAlignment] of word =(ES_LEFT,ES_RIGHT,ES_CENTER);

  ecDarkBlue    = TColor($00996633);
  ecBlue        = TColor($00CF9030);
  ecLightBlue   = TColor($00CFB78F);
  ecDarkRed     = TColor($00302794);
  ecRed         = TColor($005F58B0);
  ecLightRed    = TColor($006963B6);
  ecDarkGreen   = TColor($00385937);
  ecGreen       = TColor($00518150);
  ecLightGreen  = TColor($0093CAB1);
  ecDarkYellow  = TColor($004EB6CF);
  ecYellow      = TColor($0057D1FF);
  ecLightYellow = TColor($00B3F8FF);
  ecDarkBrown   = TColor($00394D4D);
  ecBrown       = TColor($00555E66);
  ecLightBrown  = TColor($00829AA2);
  ecDarkKaki    = TColor($00D3D3D3);
  ecKaki        = TColor($00C8D7D7);
  ecLightKaki   = TColor($00E0E9EF);

  { Encarta & FlatStyle Interface Color Constants }
  ecBtnHighlight      = clWhite;
  ecBtnShadow         = clBlack;
  ecBtnFace           = ecLightKaki;
  ecBtnFaceDown       = ecKaki;
  ecFocused           = clWhite;
  ecScrollbar         = ecLightKaki;
  ecScrollbarThumb    = ecLightBrown;
  ecBackground        = clWhite;
  ecHint              = ecYellow;
  ecHintArrow         = clBlack;
  ecDot               = clBlack;
  ecTick              = clBlack;
  ecMenuBorder        = ecDarkBrown;
  ecMenu              = clBlack;
  ecMenuSelected      = ecDarkYellow;
  ecProgressBlock     = ecBlue;
  ecUnselectedTab     = ecBlue;
  ecSelection         = clNavy;
  ecCaptionBackground = clBlack;
  ecActiveCaption     = clWhite;
  ecInactiveCaption   = ecLightBrown;

  BS_XP_BTNFRAMECOLOR  = 8388608;
  BS_XP_BTNACTIVECOLOR = 13811126;
  BS_XP_BTNDOWNCOLOR   = 11899781;
  //define ipmaskedit
  IPMaskStr   = '999\.999\.999\.999;1;'#32;
  IPStart     = '0.0.0.0';
  //定义水波字幕控制脚本
  TitleStart  = '<Title>';
  TitleEnd    = '</Title>';
  TitleSize   = '[Size:';
  TitleName   = '[Name:';
  TitleStyle  = '[Style:';
  TitleColor  = '[Color:';
  TitleLow    = '[Row:';
  TitlePitch  = '[Pitch:';
  TitleDraw3D = '[Draw3D:';
  TitleAlign  = '[Align:';
  WaterColor: array [0..17] of TWaterColor = (
  {00}(Value:clBlack;       enName:'clBlack'  ),
  {01}(Value:clWhite;       enName:'clWhite'  ),
  {02}(Value:clYellow;      enName:'clYellow' ),
  {03}(Value:clRed;         enName:'clRed'    ),
  {04}(Value:clFuchsia;     enName:'clFuchsia'),
  {05}(Value:clMaroon;      enName:'clMaroon' ),
  {06}(Value:clGreen;       enName:'clGreen'  ),
  {07}(Value:clAqua;        enName:'clAqua'   ),
  {08}(Value:clMoneyGreen;  enName:'clMoneyGreen'),
  {09}(Value:clBlue;        enName:'clBlue'   ),
  {10}(Value:clTeal;        enName:'clTeal'   ),
  {11}(Value:clSkyBlue;     enName:'clSkyBlue'),
  {12}(Value:clOlive;       enName:'clOlive'  ),
  {13}(Value:clNavy;        enName:'clNavy'   ),
  {14}(Value:clPurple;      enName:'clPurple' ),
  {15}(Value:clGray;        enName:'clGray'   ),
  {16}(Value:clSilver;      enName:'clSilver' ),
  {17}(Value:clLime;        enName:'clLime'   ));

  {Define FlatPanelEx}
  crSystemHand : TCursor = 10;
  wmNCPaintOnlyBorder : LongInt = 666;
  cTitleButtonSize : Integer = 20;
  PaletteMask = $02000000;

  { 默认颜色配置，蓝色玻璃 }
  DefGlassColorCfg: TGlassColorCfg = (
   OBorder: clBlack;
   IBorder: $00E1D0AA;
   G1Start: $00D1AE7A;
   G1End  : $00B98835;
   G2Start: $00975F00;
   G2End  : $00C6A46A;
   Style  : gdTopBottom;
   Way    : gwLRWay);
//define components main version infomation
type
  
  { TVersionControl }
  TVersionControl = Class(TCustomControl)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
    property Font;
  end;
  { TVersionCtrl }
  TVersionCtrlExt = Class(TCustomControl)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionPages }
  TVersionPages = Class(TPageControl)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionSheet }
  TVersionSheet = Class(TTabSheet)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionComboBox }
  TVersionComboBox = Class(TCustomComboBox)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionGraphic }
  TVersionGraphic = class(TGraphicControl)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionTreeView }
  TVersionTreeView = class(TCustomTreeView)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionListView }
  TVersionListView = class(TCustomListView)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionMemo }
  TVersionMemo = class(TCustomMemo)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionEdit }
  TVersionEdit = class(TCustomEdit)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionComponent }
  TVersionComponent = class(TComponent)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionListBoxExt }
  TVersionListBoxExt = class(TCustomListBox)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionDBGrid }
  TVersionDBGrid = class(TDBGrid)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionDrawGrid }
  TVersionDrawGrid = class(TCustomDrawGrid)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;
  { TVersionObject }
  TVersionObject = class(TObject)
  private
    FVersion: String;
    function GetVersion: String;
  published
    property Version: String read GetVersion write FVersion;
  end;

  {TDefineRLE}
  TDefineRLE = class(TVersionObject)
  private
    t, s: Pointer;
    function PackSeg(Source, Target: Pointer; SourceSize: Word): Word;
    function UnPackSeg(Source, Target: Pointer; SourceSize: Word): Word;
  protected
  public
    Constructor Create;
    Destructor Destroy; override;
    function Pack(Source, Target: Pointer; SourceSize: LongInt): LongInt; { Return TargetSize }
    function UnPack(Source, Target: Pointer; SourceSize: LongInt): LongInt; {Return TargetSize }
    function PackString(Source: String): String;
    function UnPackString(Source: String): String;
    function PackFile(SourceFileName, TargetFileName: String): Boolean; { Return FALSE if IOError }
    function UnPackFile(SourceFileName, TargetFileName: String): Boolean; { Return FALSE if IOError }
  end; 


//定义 重画控件边界函数
function  DrawEditBorder(Border:TBorderAttrib; const Clip: HRGN=0):TColor;
procedure DrawButtonBorder(Canvas: TCanvas;Rect: TRect; Color: TColor; Width: Integer);
function  DrawViewBorder(ViewBorder: TBorderAttrib;const oVal:Byte=1):TColor;
procedure DrawInCheck(Canvas:TCanvas; Rect:TRect; Color:TColor);
procedure DrawFrame(Canvas: TCanvas; var Rect: TRect; BorderColor, FaceColor: TColor; Width: Integer);
//定义 重画透明背景
procedure DrawTransBitBlt(Cnv: TCanvas; x, y: Integer; Bmp: TBitmap; clTransparent: TColor);
//定义 画父背景图像
procedure DrawParentImage(Control: TControl; Dest: TCanvas;const DefaultTop:integer=0);
procedure DrawParentImageSub(Control: TControl; Dest: TCanvas;const DefaultHeigth:integer=0);
function  CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor, BackColor, HighlightColor, ShadowColor: TColor;
                               DrawHighlight: Boolean): TBitmap;
function  CalcAdvancedColor(ParentColor, OriginalColor: TColor; Percent: Byte; ColorType: TColorCalcType): TColor;
procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect; const Offset: TPoint; Layout: TButtonLayout;
                           Spacing, Margin: Integer; FGlyph: TBitmap; FNumGlyphs: Integer;
                           const Caption: string; var TextBounds: TRect; var GlyphPos: TPoint);
function  Min(const A, B: Integer): Integer;
function  Max(const A, B: Integer): Integer;
function  GetFontMetrics(Font: TFont): TTextMetric;
function  GetFontHeight(Font: TFont): Integer;
function  RectInRect(R1, R2: TRect): Boolean;
procedure DrawBackdrop(Canvas:TCanvas; StartColor, StopColor: TColor; CanRect:TRect;Style:TStyleOrien);
function  IndexInCount(Index,Count:Integer):boolean;
procedure DrawFocusRect(Canvas:TCanvas;FocusRect:TRect;Height:Integer);
procedure SetTicketPoint(Value:TTicketPosition;Self,Ticket:TControl;TicketSpace:Integer);
procedure GetStyleText(Value:TAlignmentText; var Result:UINT);
procedure GetCheckBoxPosition(Value:TCheckPosition; var Result:UINT);  
procedure DrawCheckBox(BoxRect:TRect; Position:TCheckPosition; Size:Integer; Var CheckRect:TRect);
procedure GetBarPosition(ClientRect:TRect;TitleHas:boolean;TitlePosition:TTitlePosition;
                         Var BarsRect:TBarsRect; TitleHeight, BarHeight:Integer);
procedure BoxDrawBackDrop(Canvas:TCanvas;ColorStart,ColorStop:TColor;Style:TStyleOrien;
                          ClientRect:TRect;ItemColor:TColor;Face:TStyleFace);
procedure DrawBitmap(Canvas:TCanvas; DrawRect:TRect; Source:TBitmap);
procedure FlatDrawText(Canvas: TCanvas; Enabled: Boolean; Caption: TCaption; DrawRect:TRect; Format:uint);
function  CheckValue(Value,MaxValue,MinValue: LongInt): LongInt;
function  RectWidth(R: TRect): Integer;
function  RectHeight(R: TRect): Integer;
function  DrawEllipse(Handle: HDC; Rect: TRect): BOOL;
function  RectToCenter(var R: TRect; Bounds: TRect): TRect;
procedure CorrectTextbyWidth(C: TCanvas; var S: String; W: Integer);
//定义 IP控制函数
procedure IPEmpty(Var IP:TIP);
procedure IPValue(Var IP:TIP;Inx:Word;Value:TIPChar);
//定义 释放指针列表函数
procedure RemoveList(List:TList; State:TListState=lsClear);
//定义 重设列表区域函数
procedure SetEditRect(Handle:HWnd; ClientWidth,ClientHeight,Width:Integer);
//定义 水波字幕解析函数
procedure GetTitleParam(Var Font: TOtherParam; Var Title:String);
function  GetParamColor(Value:String):TColor;
function  GetParamDraw3D(Value:String): Boolean;
function  GetParamStyle(Value:String): TFontStyles;
function  GetParamValue(Var Value:String; Param:String):String;  
function  HeightOf(R: TRect): Integer;
function  WidthOf(R: TRect): Integer;
function  DelCapLink(Caption:String):String;
//define TDefinePanelEx
//Gradint filling functions
procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
procedure GradientXPFillRect(ACanvas : TCanvas; ARect : TRect; LightColor : TColor;
  DarkColor : TColor; Colors : Byte);
procedure GradientSimpleFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
procedure CopyBitmap(const Source : TBitmap; Dest : TBitmap);
procedure ConvertBitmapToGrayscale(const Bmp: TBitmap);
procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
function  MakeDarkColor(AColor : TColor; ADarkRate : Integer) : TColor;
//define ShowDialog api
function  ShowBox(const Text:String; const Flags: Longint=mbEOK): Integer;
function  ShowBoxExt(const Text:String; Title:String; const Flags: Longint=mbEOK): Integer;
procedure ShowDialog(const Msg: string; const BtnCap:String='&Exit');
procedure ShowDialogFmt(const Msg: string; const Args: array of const; const BtnCap:String='&Exit');
//退出软件出现的对话框FormClose在关闭窗体中设置
procedure ShowExitDialog(ShowTitle:String);
//玻璃绘制函数
function  GradientFill(DC: HDC; Vertex: PTriVertex; NumVertex: ULONG;
                       Mesh: Pointer; NumMesh, Mode: ULONG): BOOL; stdcall;
{ 颜色值转RGB }
procedure GetRGB(C: TColor; out R, G, B: Integer);
{ 渐变函数 }
procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor; const Direction: TGradDirection);
{ 玻璃效果绘制函数 }
procedure DrawGlassFace(Canvas: TCanvas; ARect: TRect; ColorCfg: TGlassColorCfg);
{ 自定义消息处理函数 }
procedure ProcessMessages;
{ 获取WINDOWS系统临时目录 }
function GetTempDirectory: String;
function SolvePercent(X, Z: Longint;per:Real=100): Longint;
{ 获取系统任务栏高度}
function GetTaskBarHeight: Integer;
function GetTaskBarRect: TRect;
{ 格式化整数长度}
function FmtLen(Value:integer;Len:Byte=2):String;
{ 获取主文件名}
function RetuFileName(const FileName: string): string;
{ 四舍五入函数}
function RoundExt(const Value: Extended; const Digit: Byte = 0): Extended;
procedure IncExt(Var Value:Extended; Digit:Extended);
function GetAppPath(const SubPath:String=''):String;

//define Colors range
var HSLRange: integer = 240;

implementation

function GetAppPath(const SubPath:String=''):String;
begin
 result := Format('%s%s',[ExtractFilePath(Application.ExeName),SubPath]);
 if Length(Result)>0 then begin
  if Result[Length(Result)]<>'\' then
     Result := Result + '\';
 end;
end;

procedure IncExt(Var Value:Extended; Digit:Extended);
begin
  Value := Value + Digit;
end;

function RoundExt(const Value: Extended; const Digit: Byte = 0): Extended;
var tmp: Extended;
 function PowerInt(const Base: Extended; const Exponent: Integer): Extended;
 asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
 @@1:   fmul    ST, ST            { X := Base * Base }
 @@2:   shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
 @@3:
        fwait
 end;
 function PowerEx(const Base, Exponent: Extended): Extended;
 begin
  if Exponent = 0.0 then
    Result := 1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0               { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := PowerInt(Base, Integer(Trunc(Exponent)))
  else
    Result := Exp(Exponent * Ln(Base))
 end;
begin
tmp := PowerEx(10, Digit);
if Value > 0 then
   Result := Value * tmp + 0.5
else
   Result := Value * tmp - 0.5;
Result := Trunc(Result) / tmp;
end;


function RetuFileName(const FileName: string): string;
begin
  Result := Copy(FileName, 1, LastDelimiter('.', FileName)-1);
end;

function FmtLen(Value:integer;Len:Byte=2):String;
begin
 result := IntToStr(Value);
 while Length(Result)<Len do begin
   Insert('0',Result,1);
 end;
end;

function GetTaskBarRect: TRect;
var TBData: TAPPBARDATA;
begin
  TBData.cbSize := SizeOf(TAPPBARDATA);
  SHAppBarMessage(ABM_GETTASKBARPOS, TBData);
  Result  :=TBData.rc;
end;

function GetTaskBarHeight: Integer;
var TBData: TAPPBARDATA;
begin
  TBData.cbSize := SizeOf(TAPPBARDATA);
  SHAppBarMessage(ABM_GETTASKBARPOS, TBData);
  Result  :=TBData.rc.Bottom-TBData.rc.Top;
end;    

function SolvePercent(X, Z: Longint;per:Real=100): Longint;
begin
  if Z = 0 then Result := 0
  else Result := Longint(Trunc((X * per) / Z ));
end;

procedure ShowExitDialog(ShowTitle:String);
var Title:String;
begin
 Title := ShowTitle+#13#13+'你真的想退出(Y/N)? 请三思.......';
 if ShowBox(Title,mbIYN)=mrYes then
    Application.Terminate
 else
    Application.Run;
end;

{ 获取WINDOWS系统临时目录 }
function GetTempDirectory: String;
var TempDir: array[0..255] of Char;
begin
  GetTempPath(255, TempDir);
  Result := StrPas(TempDir);
  if Result[Length(Result)] <> '\' then
     result := result + '\';
end;
{ 自定义消息处理函数 }
procedure ProcessMessages;
var Msg:TMsg;
{--------------------------------------}
 function ProcessMessage(Msg:TMsg):BOOL;
 begin
  result := false;
  if PeekMessage(Msg,0,0,0,PM_REMOVE) then
  begin
     result := True;
     TranslateMessage(Msg);
     DispatchMessage(Msg);
  end;
 end;
{--------------------------------------}
begin
  while ProcessMessage(Msg) do {loop};
end;
//玻璃绘制函数
function GradientFill; external msimg32;
//自定义对话框
procedure ShowDialog(const Msg: string; const BtnCap:String='&Exit');
const OkMax = 160;
var Form: TForm;
    Dlg: TPoint;
    OkLeft, OkTop, OkWidth, OkHeight: Integer;
 function GetAveCharSize(Canvas: TCanvas): TPoint;
 var I: Integer;
     Buffer: array[0..51] of Char;
 begin
     for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
     for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
     GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
     Result.X := Result.X div 52;
 end;
begin
  Form   := TForm.Create(Application);
  with Form do
    try
      Font.Size   := 9;
      Font.Name   := '宋体';
      Canvas.Font := Font;
      Dlg         := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption     := Application.Title;
      ClientWidth := MulDiv(OkMax, Dlg.X, 4);
      Position    := poScreenCenter;
      with TImage.Create(Form) do begin
        Parent    := Form;
        AutoSize  := true;                          
        Left      := 4;
        Top       := 4;
        Picture.Icon.Assign(Application.Icon);
      end;
      OkTop       := MulDiv(8, Dlg.Y, 8);
      OkLeft      := MulDiv(30, Dlg.X, 4);
      OkWidth     := MulDiv(OkMax-OkLeft+8, Dlg.X, 4);
      with TLabel.Create(Form) do begin
        Parent    := Form;
        Caption   := Msg;
        Left      := OkLeft;
        Top       := OkTop;
        Constraints.MaxWidth := OkWidth;
        WordWrap  := True;
      end;
      OkTop       := OkLeft + OkTop;
      OkLeft      := MulDiv(60, Dlg.X, 4);
      OkWidth     := MulDiv(40, Dlg.X, 4);
      OkHeight    := MulDiv(15, Dlg.Y, 8); 
      with TButton.Create(Form) do begin
        Parent      := Form;
        Caption     := BtnCap;
        ModalResult := mrOk;
        Default     := True;
        SetBounds(OkLeft, OkTop, OkWidth, OkHeight);
      end;
      ClientHeight:= OkTop+OkHeight+10;
      ShowModal;
    finally
      Form.Free;
    end;
end;
procedure ShowDialogFmt(const Msg: string; const Args: array of const; const BtnCap:String='&Exit');
begin
  ShowDialog(Format(Msg,Args),BtnCap);
end;
procedure GetRGB(C: TColor; out R, G, B: Integer);
begin
  if Integer(C) < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF;
  G := C shr 8 and $FF;
  B := C shr 16 and $FF;
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor; const Direction: TGradDirection);
var
  Vert: array[0..1] of TTriVertex;
  gRect: TGradientRect;
  nMode: Cardinal;
  R, G, B: Integer;
begin
  Vert[0].x     := ARect.Left;
  Vert[0].y     := ARect.Top;
  GetRGB(StartColor, R, G, B);
  Vert[0].Red   := R shl 8;
  Vert[0].Green := G shl 8;
  Vert[0].Blue  := B shl 8;
  Vert[0].Alpha := 0;
  
  Vert[1].x     := ARect.Right;
  Vert[1].y     := ARect.Bottom;
  GetRGB(EndColor, R, G, B);
  Vert[1].Red   := R shl 8;
  Vert[1].Green := G shl 8;
  Vert[1].Blue  := B shl 8;
  Vert[1].Alpha := 0;

  gRect.UpperLeft  := 0;
  gRect.LowerRight := 1;
  if Direction = gdLeftRight then
     nMode := GRADIENT_FILL_RECT_H
  else
     nMode := GRADIENT_FILL_RECT_V;
  GradientFill(Canvas.Handle, @Vert[0], 2, @gRect, 1, nMode);
  //GradientFill(Canvas.Handle, @Vert, 2, @gRect, 1, nMode);
end;

procedure DrawGlassFace(Canvas: TCanvas; ARect: TRect; ColorCfg: TGlassColorCfg);
var R: TRect; OffSet:Integer;
begin
  Canvas.Brush.Style := bsClear;
  with ColorCfg do begin
    if OBorder <> clNone then begin
      //外框
      Canvas.Pen.Color := OBorder;
      Canvas.Rectangle(ARect);
    end;
    if IBorder <> clNone then begin
      //内框
      InflateRect(ARect, -1, -1);
      Canvas.Pen.Color := IBorder;
      Canvas.Rectangle(ARect);
    end;
    //上下渐变效果
    InflateRect(ARect, -1, -1);
    OffSet := Round((ARect.Bottom-ARect.Top)*Ord(Way));
    R := Rect(ARect.Left, ARect.Top, ARect.Right,ARect.Top+OffSet);
    FillGradient(Canvas, R, G1Start, G1End, Style);  //gdLeftRight   gdTopBottom
    R := Rect(R.Left, R.Bottom, R.Right, ARect.Bottom);
    FillGradient(Canvas, R, G2Start, G2End, Style);  //gdLeftRight   gdTopBottom
  end;
end;

//自定义提示函数
function MSGTitle(Flags:Longint):PChar;
begin
 case Flags of
  {define mb_inconinformtion}
  mbIAll, mbIOk, mbIOC, mbIRC, mbIYN, mbIYNC:Result := '提示';
  {define mb_inconwarning}
  mbWAll, mbWOk, mbWOC, mbWRC, mbWYN, mbWYNC:Result := '警告';
  {define mb_inconerror}
  mbEAll, mbEOk, mbEOC, mbERC, mbEYN, mbEYNC:Result := '错误';
  {define mb_inconstop}
  mbQAll, mbQOk, mbQOC, mbQRC, mbQYN, mbQYNC:Result := '停止';
 end;
end;

//自定义提示函数
function ShowBox(const Text:String; const Flags: Longint=mbEOK): Integer;
begin
  result := Application.MessageBox(PChar(Text),MSGTitle(Flags),Flags);
end;
//自定义提示函数
function ShowBoxExt(const Text:String; Title:String; const Flags: Longint=mbEOK): Integer;
begin
  result := Application.MessageBox(PChar(Text),PChar(Title),Flags);
end;
//删除快捷连接符&
function  DelCapLink(Caption:String):String;
begin
   result := Caption;
   if Pos('&', Caption) <> 0 then Delete(result, Pos('&', result), 1);
end;
//计算顶与底之间的距离(高度)
function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;
//计算左右之间的距离(宽度)
function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;
//在指定的区域内画图
procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
var
  X, Y: Integer;
  SaveIndex: Integer;
begin
  if(Image.Width = 0) or(Image.Height = 0) then Exit;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    for X := 0 to(WidthOf(Rect) div Image.Width) do
      for Y := 0 to(HeightOf(Rect) div Image.Height) do
        Canvas.Draw(Rect.Left + X * Image.Width,
          Rect.Top + Y * Image.Height, Image);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;
//锁定颜色范围
function MakeDarkColor(AColor : TColor; ADarkRate : Integer) : TColor;
var
  R, G, B : Integer;
begin
  R := GetRValue(ColorToRGB(AColor)) - ADarkRate;
  G := GetGValue(ColorToRGB(AColor)) - ADarkRate;
  B := GetBValue(ColorToRGB(AColor)) - ADarkRate;
  if R < 0 then R := 0;
  if G < 0 then G := 0;
  if B < 0 then B := 0;
  if R > 255 then R := 255;
  if G > 255 then G := 255;
  if B > 255 then B := 255;
  Result := TColor(RGB(R, G, B));
end;

function PaletteColor(Color: TColor): Longint;
begin
  Result := ColorToRGB(Color) or PaletteMask;
end;
//对图像进行放缩
procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPalette;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBitmap;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPalette;
begin
  { Create some DCs to hold temporary data }
  BackDC      := CreateCompatibleDC(DstDC);
  ObjectDC    := CreateCompatibleDC(DstDC);
  MemDC       := CreateCompatibleDC(DstDC);
  SaveDC      := CreateCompatibleDC(DstDC);
  { Create a bitmap for each DC }
  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack   := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem    := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave      := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld   := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld    := SelectObject(MemDC, bmAndMem);
  bmSaveOld   := SelectObject(SaveDC, bmSave);
  { Select palette }
  palDst := 0; palMem := 0; palSave := 0; palObj := 0;
  if Palette <> 0 then begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;
  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0,
    SRCCOPY);
  { Restore palette }
  if Palette <> 0 then begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, SrcH: Integer);
var
  CanvasChanging: TNotifyEvent;
begin
  if DstW <= 0 then DstW := Bitmap.Width;
  if DstH <= 0 then DstH := Bitmap.Height;
  if(SrcW <= 0) or(SrcH <= 0) then begin
    SrcX := 0; SrcY := 0;
    SrcW := Bitmap.Width;
    SrcH := Bitmap.Height;
  end;
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
{$IFDEF VER100}
  Bitmap.Canvas.Lock;
{$ENDIF}
  try
    Bitmap.Canvas.OnChanging := nil;
    if TransparentColor = clNone then begin
      StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
        SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
    end
    else begin
{$IFDEF VER100}
      if TransparentColor = clDefault then
        TransparentColor := Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
{$ENDIF}
      if Bitmap.Monochrome then TransparentColor := clWhite
      else TransparentColor := ColorToRGB(TransparentColor);
      StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
        Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Bitmap.Palette,
        TransparentColor);
    end;
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
{$IFDEF VER100}
    Bitmap.Canvas.Unlock;
{$ENDIF}
  end;
end;


procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; Bitmap: TBitmap;
  TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, DstW, DstH, Left, Top, Right - Left, Bottom - Top);
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, Right - Left, Bottom - Top, Left, Top, Right - Left,
    Bottom - Top);
end;

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
    Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
end;

procedure ConvertBitmapToGrayscale(const Bmp: TBitmap);
  {From: Pascal Enz, pascal.enz@datacomm.ch }
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row: PRGBArray;
begin
  Bmp.PixelFormat := pf24Bit;
  for y := 0 to Bmp.Height - 1 do
  begin
    Row := Bmp.ScanLine[y];
    for x := 0 to Bmp.Width - 1 do
    begin
      Gray           :=(Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue := Gray;
    end;
  end;
end;

procedure CopyBitmap(const Source : TBitmap; Dest : TBitmap);
begin
  try Dest.FreeImage;
  except
  end;

  Dest.Width := Source.Width;
  Dest.Height := Source.Height;
  Dest.PixelFormat := Source.PixelFormat;

  BitBlt(Dest.Canvas.Handle, Dest.Canvas.ClipRect.Left, Dest.Canvas.ClipRect.Top, Dest.Width, Dest.Height,
    Source.Canvas.Handle, 0, 0, SRCCOPY);
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

procedure GradientXPFillRect(ACanvas : TCanvas; ARect : TRect; LightColor : TColor; DarkColor : TColor; Colors : Byte);
const
  cLightColorOffset : Integer = 30;
  cMainBarOffset : Integer = 6;
var
  DRect : TRect;
  I : Integer;
begin
  if IsRectEmpty(ARect) then Exit;

  ACanvas.Brush.Color := DarkColor;
  ACanvas.FrameRect(ARect);
  //InflateRect(ARect, -1, -1);

  //Main center rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := DRect.Top + cMainBarOffset;
  DRect.Bottom := DRect.Bottom - cMainBarOffset;
  GradientSimpleFillRect(ACanvas, DRect, DarkColor, LightColor, fdTopToBottom, Colors);

  //Bottom rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := ARect.Bottom - cMainBarOffset;
  GradientSimpleFillRect(ACanvas, DRect, LightColor, DarkColor, fdTopToBottom, Colors);

  //Second left rect
  DRect := ARect;
  DRect := Rect(ARect.Left + cMainBarOffset div 4, 0, ARect.Left + cMainBarOffset, 1);
  For I := ARect.Top + cMainBarOffset to ARect.Bottom do
  begin
    DRect.Top := I;
    DRect.Bottom := I+1;
    GradientSimpleFillRect(ACanvas, DRect, ACanvas.Pixels [DRect.Left-1, DRect.Top],
      ACanvas.Pixels [DRect.Right + 1, DRect.Top], fdLeftToRight, 8);
  end;

  //Top light rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Bottom := DRect.Top + cMainBarOffset div 4;
  GradientSimpleFillRect(ACanvas, DRect, MakeDarkColor(LightColor, -cLightColorOffset), LightColor, fdTopToBottom, 8);

  //Second top rect
  DRect := ARect;
  DRect.Left := DRect.Left + cMainBarOffset;
  DRect.Top := DRect.Top + cMainBarOffset div 4;
  DRect.Bottom := ARect.Top + cMainBarOffset;
  GradientSimpleFillRect(ACanvas, DRect, LightColor, DarkColor, fdTopToBottom, 8);

  //Left light rect
  DRect := ARect;
  DRect.Top := DRect.Top + cMainBarOffset;
  DRect.Right := DRect.Left + cMainBarOffset div 4;
  GradientSimpleFillRect(ACanvas, DRect, MakeDarkColor(LightColor, -cLightColorOffset), LightColor, fdLeftToRight, 8);

  //Second left rect
  DRect := ARect;
  DRect := Rect(ARect.Left + cMainBarOffset div 4, 0, ARect.Left + cMainBarOffset, 1);
  For I := ARect.Top + cMainBarOffset to ARect.Bottom do
  begin
    DRect.Top := I;
    DRect.Bottom := I+1;
    GradientSimpleFillRect(ACanvas, DRect, ACanvas.Pixels [DRect.Left-1, DRect.Top],
      ACanvas.Pixels [DRect.Right + 1, DRect.Top], fdLeftToRight, 8);
  end;

  For I := 0 to cMainBarOffset do
  begin
    ACanvas.Pen.Color := ACanvas.Pixels [ARect.Left + I, ARect.Top + cMainBarOffset+1];
    ACanvas.MoveTo(ARect.Left + I, ARect.Top + cMainBarOffset);
    ACanvas.LineTo(ARect.Left + I, ARect.Top + I);
    ACanvas.LineTo(ARect.Left + cMainBarOffset, ARect.Top + I);
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
        BRect.Bottom := BRect.Top +  HeightOf(ARect) div 2;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdTopToBottom, Colors);
        BRect.Top :=(BRect.Top + HeightOf(ARect) div 2);
        BRect.Bottom := ARect.Bottom;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdBottomToTop, Colors);
      end;
    fdCenterToHoriz:
      begin
        BRect := ARect;
        BRect.Right := BRect.Left +  WidthOf(ARect) div 2;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdLeftToRight, Colors);
        BRect.Left :=(BRect.Left + WidthOf(ARect) div 2);
        BRect.Right := ARect.Right;
        GradientSimpleFillRect(Canvas, BRect, StartColor, EndColor, fdRightToLeft, Colors);
      end;
    fdXPFace:
      begin
        GradientXPFillRect(Canvas, ARect, StartColor, EndColor, Colors);
      end
    else
      GradientSimpleFillRect(Canvas, ARect, StartColor, EndColor, Direction, Colors);
  end;
end;

procedure DrawFrame(Canvas: TCanvas; var Rect: TRect; BorderColor, FaceColor: TColor;
  Width: Integer);

  procedure DoRect;
  begin
    with Canvas, Rect do
    begin
      Pen.Color := BorderColor;
      MoveTo(Left,Top);
      LineTo(Left,Bottom);
      Pen.Color := FaceColor;
      MoveTo(Left,Bottom);
      LineTo(Right,Bottom);
      MoveTo(Left,Top);
      LineTo(Right,Top);  
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  inc(Rect.Left);
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
  dec(Rect.Left);
end;

procedure DrawInCheck(Canvas:TCanvas; Rect:TRect; Color:TColor);
var x,y,yTop:Word;
begin
 with Canvas, Rect do
 begin
  yTop :=(Right - Left - 12) div 2;
     x :=  Left + yTop;
     y :=  Top + yTop;
  Pen.Color := Color;
  PenPos  := Point(x+2, y+5);
  LineTo(x+4,y+7);
  PenPos  := Point(x+4, y+7);
  LineTo(x+10,y+1);
  PenPos  := Point(x+2, y+6);
  LineTo(x+4,y+8);
  PenPos  := Point(x+4, y+8);
  LineTo(x+10,y+2);
  PenPos  := Point(x+2, y+7);
  LineTo(x+4,y+9);
  PenPos  := Point(x+4, y+9);
  LineTo(x+10,y+3);
 end;
end;

function DrawEditBorder(Border:TBorderAttrib; const Clip: HRGN=0):TColor;
var
  DC: HDC;
  R, BarRect: TRect;
  FaceBrush, WindowBrush, FocusBrush: HBRUSH;
begin
 with Border do
 begin
  DC := GetWindowDC(Ctrl.Handle);
  try
    GetWindowRect(Ctrl.Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    FaceBrush   := CreateSolidBrush(ColorToRGB(BorderColor));
    WindowBrush := CreateSolidBrush(ColorToRGB(FlatColor));
    FocusBrush  := CreateSolidBrush(ColorToRGB(FocusColor));
    BarRect     := Rect(R.Right - 20, R.Bottom - 20, R.Right - 3, R.Bottom - 3);
    FrameRect(DC, R, FaceBrush);
    if BoldState then begin
       InflateRect(R, -1, -1);
       FrameRect(DC, R, FaceBrush);
    end;
    if(not(csDesigning in DesignState) and(FocusState or MouseState)) then
    begin // Focus
      result := FocusColor;
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
      if HasBars then FillRect(DC, BarRect , FocusBrush);
    end else begin // non Focus
      result := FlatColor;
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
      if HasBars then FillRect(DC, BarRect, WindowBrush);
    end;
  finally
    ReleaseDC(Ctrl.Handle, DC);
  end;
  DeleteObject(WindowBrush);
  DeleteObject(FaceBrush);
  DeleteObject(FocusBrush);
 end;
end;

procedure DrawButtonBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; Width: Integer);
  procedure DoRect(Cans:TCanvas; R:TRect);
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Cans, R do begin
      TopRight.X   := Right;
      TopRight.Y   := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color    := Color;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      //Pen.Color    := Color;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
      {Pen.Color    := Color;
      RoundRect(Rect.Left,Rect.Top,rect.Right,Rect.Bottom,2,2);}
    end;
  end;
begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do begin
    Dec(Width);
    DoRect(Canvas,Rect);
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

function  DrawViewBorder(ViewBorder: TBorderAttrib;const oVal:Byte=1):TColor;
var         
  R: TRect;
  memBmp:TControlCanvas;
begin
  memBmp:=TControlCanvas.Create;
  try
   with ViewBorder do
   begin
    memBmp.Handle := GetWindowDC(Ctrl.Handle);
    GetWindowRect(Ctrl.Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if(not(csDesigning in DesignState) and(FocusState or MouseState)) then
    begin
      result := FocusColor;
    end
    else
    begin
      result := FlatColor;
    end;
    dec(r.Left,   oVal);
    dec(r.Top,    oVal);
    inc(r.Right,  oVal);
    inc(r.Bottom, oVal);
    InflateRect(R, -oVal, -oVal);
    DrawButtonBorder(memBmp, R, BorderColor, oVal);
   end;
  finally
    memBmp.FreeHandle;
    memBmp.Free;
  end;
end;

function  GetParamValue(Var Value:String; Param:String):String;
var
  FontS, FontL, Spliter : Integer;
  SubValue:String;
  function Find(Value:String;cur:Integer):integer;
  var inx:integer;
  begin
    result := cur;
    for inx := Cur to Length(Value) do
       if Value[inx]=']' then
       begin
          result := inx;
          exit;
       end;
  end;
begin
  if Pos(Param,Value) > 0 then
  begin
     FontS     := Pos(Param,Value);
     FontL     := FontS + Length(Param);
     Spliter   := Find(Value,FontS);
     Result    := Trim(Copy(Value,FontL,Spliter-FontL));
     SubValue  := format('%s%s]',[Param,Result]);
     Delete(Value,Pos(SubValue,Value),Length(SubValue));
  end else begin
     Result := '';
  end;
end;

function  GetParamStyle(Value:String): TFontStyles;
begin
 Result := [];
 if(Pos('BOLD', Value) > 0)or(Pos('0', Value)>0) then
    result := Result + [fsBold];
 if(Pos('ITALIC', Value) > 0)or(Pos('1', Value)>0) then
    result := Result + [fsItalic];
 if(Pos('UNDERLINE', Value) > 0)or(Pos('2', Value)>0) then
    result := Result + [fsUnderline];
 if(Pos('STRIKEOUT', Value) > 0)or(Pos('3', Value)>0) then
    result := Result + [fsStrikeOut];
end;

function  GetParamPitch(Value:String): TFontPitch;
begin
 Result := fpDefault;
 if(Pos('VARIABLE', Value) > 0)or(Pos('1', Value)>0) then
    result := fpVariable;
 if(Pos('Fixed', Value) > 0)or(Pos('2', Value)>0) then
    result := fpFixed;
end;

function  GetParamDraw3D(Value:String): Boolean;
begin
 Result := False;
 if(Pos('True', Value) > 0)or(Pos('1', Value)>0) then
    result := True;
end;

function  GetParamColor(Value:String):TColor;
var
   inx : Word;
   State: Boolean;
begin
   for inx := Low(WaterColor) to High(WaterColor) do
   begin
    State := UpperCase(WaterColor[inx].enName) = UpperCase(Value);
    if State then
    begin
       result := WaterColor[inx].Value;
       exit;
    end;
   end;
   if not State then
      result := TColor(StrToInt(Value))
   else
      Result := clBlack;
end;

function  GetParamAlign(Value:String):TWaterAlign;
begin
  result := wpCenter;
  if(Pos('ALIGN', Value) > 0)or(Pos('0', Value)>0) then
      result := wpLeft;
  if(Pos('ALIGN', Value) > 0)or(Pos('2', Value)>0) then
      result := wpRight;
end;

procedure GetTitleParam(Var Font: TOtherParam; Var Title:String);
var
  Value, Param:String;
  FontS,FontE,Inx:Integer;
begin
 Value := Title;
 FontS := Pos(UpperCase(TitleStart), UpperCase(Value));
 FontE := Pos(UpperCase(TitleEnd), UpperCase(Value));
 Inx   := FontS + Length(TitleStart);
 Title := Copy(Value, Inx, FontE - Length(TitleEnd));
 if(FontS > 0) and(FontE > 0) then
 begin
    Inx   := FontE + Length(TitleEnd);
    Value := UpperCase(Copy(Value, Inx, Length(Value)));
    //解析 字体的大小
    Param := GetParamValue(Value, UpperCase(TitleSize));
    if Param <> '' then
       Font.Size  := StrToInt(Param)
    else
       Font.Size  := 8;
    //解析 字体的名称
    Param   := GetParamValue(Value, UpperCase(TitleName));
    if Param <> '' then
       Font.Name  := Param
    else
       Font.Name  := 'MS Sans Serif';
    //解析 字体的样式
    Param  := GetParamValue(Value, UpperCase(TitleStyle));
    if Param <> '' then
       Font.Style := GetParamStyle(Param)
    else
       Font.Style := [];
    //解析 字体的颜色
    Param  := GetParamValue(Value, UpperCase(TitleColor));
    if Param <> '' then
       Font.Color := GetParamColor(Param)
    else
       Font.Color := clWindowText;
    //解析 行距
    Param  := GetParamValue(Value, UpperCase(TitleLow));
    if Param <> '' then
       Font.Row := StrToInt(Param)
    else
       Font.Row := 0;
    Param  := GetParamValue(Value, UpperCase(TitlePitch));
    if Param <> '' then
       Font.Pitch := GetParamPitch(Param)
    else
       Font.Pitch := fpDefault;
    Param  := GetParamValue(Value, UpperCase(TitleDraw3D));
    if Param <> '' then
       Font.Draw3D := GetParamDraw3D(Param)
    else
       Font.Draw3D := False;
    Param  := GetParamValue(Value, UpperCase(TitleAlign));
    if Param <> '' then
       Font.Align  := GetParamAlign(Param)
    else
       Font.Align  := wpCenter;
 end else begin
    Title := '';
 end;
end;

procedure SetEditRect(Handle:HWnd; ClientWidth,ClientHeight,Width:Integer);
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc := Rect(0, 0, ClientWidth - Width - 3, ClientHeight);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
end;

procedure RemoveList(List:TList; State:TListState=lsClear);
var inx:integer;
begin
 //NO.1 free all the memory pointer
 for inx:=0 to List.Count - 1 do
     Dispose(List.Items[inx]);
 //NO.2 user select lsClear or lsFree to List;
 case State of
   lsClear : List.Clear;
   lsFree  : List.Free;
 end;
end;

procedure IPEmpty(Var IP:TIP);
begin
 IP.NO1 := ' 0 ';
 IP.NO2 := ' 0 ';
 IP.NO3 := ' 0 ';
 IP.NO4 := ' 0 ';
end;

procedure IPValue(Var IP:TIP;Inx:Word;Value:TIPChar);
begin
  case inx of
    1:IP.NO1 := Value;
    2:IP.NO2 := Value;
    3:IP.NO3 := Value;
    4:IP.NO4 := Value;
  end
end;

procedure CorrectTextbyWidth(C: TCanvas; var S: String; W: Integer);
var
  j: Integer;
begin
  j := Length(S);
  with C do
  begin
    if TextWidth(S) > w
    then
      begin
        repeat
          Delete(S, j, 1);
          Dec(j);
        until(TextWidth(S + '...') <= w) or(S = '');
        S := S + '...';
      end;
  end;
end;

function RectToCenter(var R: TRect; Bounds: TRect): TRect;
var
  OffsetLeft,OffsetTop:Integer;
begin
  OffSetLeft :=(RectWidth(Bounds) - RectWidth(R)) div 2;
  OffsetTop  :=(RectHeight(Bounds) - RectHeight(R)) div 2;
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, OffsetLeft, OffsetTop);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

function RectWidth(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function RectHeight(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function  CheckValue(Value,MaxValue,MinValue: LongInt): LongInt;
begin
  Result := Value;
  if(MaxValue <> MinValue) then
  begin
    if Value < MinValue then
       Result := MinValue
    else
      if Value > MaxValue then
         Result := MaxValue;
  end;
end;

procedure FlatDrawText(Canvas: TCanvas; Enabled: Boolean; Caption: TCaption; DrawRect:TRect; Format:uint);
begin
 with Canvas do begin
  brush.style := bsClear;
  InflateRect(DrawRect, -4, 0);
  if Enabled then begin
     DrawText(Handle, PChar(Caption), Length(Caption), DrawRect, Format);
  end else begin
     OffsetRect(DrawRect, 1, 1);
     Font.Color := clBtnHighlight;
     DrawText(Handle, PChar(Caption), Length(Caption), DrawRect, Format);
     OffsetRect(DrawRect, -1, -1);
     Font.Color := clBtnShadow;
     DrawText(Handle, PChar(Caption), Length(Caption), DrawRect, Format);
  end;
  InflateRect(DrawRect, +4, 0);
 end;
end;

procedure DrawBitmap(Canvas:TCanvas; DrawRect:TRect; Source:TBitmap);
begin
 Canvas.StretchDraw(DrawRect, Source);
end;

procedure BoxDrawBackdrop(Canvas:TCanvas;ColorStart,ColorStop:TColor;Style:TStyleOrien;
                          ClientRect:TRect;ItemColor:TColor;Face:TStyleFace);
begin
 if Face = fsDefault then begin
    canvas.Brush.Color := ItemColor;
    canvas.FillRect(ClientRect);
 end else begin
    DrawBackdrop(canvas,ColorStart,ColorStop,ClientRect,Style)
 end;
end;

procedure GetBarPosition(ClientRect:TRect;TitleHas:boolean;TitlePosition:TTitlePosition;
                         Var BarsRect:TBarsRect; TitleHeight, BarHeight:Integer);
begin
  with BarsRect do begin
    prevRect := ClientRect;
    downRect := ClientRect;
    if TitleHas then begin
       case TitlePosition of
        tsTop :begin
         prevRect.Top    := prevRect.Top    + TitleHeight;
         prevRect.Bottom := prevRect.Top    + BarHeight;
         downRect.Top    := downRect.Bottom - BarHeight;
        end;
        tsBottom:begin
         prevRect.Bottom := prevRect.Top + BarHeight;
         downRect.Bottom := downRect.Bottom - TitleHeight;
         downRect.Top    := downRect.Bottom - BarHeight;
        end;
       end;
    end else begin
         prevRect.Bottom := prevRect.Top    + BarHeight;
         downRect.Top    := downRect.Bottom - BarHeight;
    end;
  end;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
     Result := A
  else
     Result := B;
end;

procedure DrawCheckBox(BoxRect:TRect; Position:TCheckPosition; Size:Integer; Var CheckRect:TRect);
var
  RectPos:TPoint;
  xLeft,yTop,y:integer;
begin
  y :=(BoxRect.Bottom - BoxRect.Top - Size) div 2;
  if Position = bpLeft then begin
     RectPos   := Point(BoxRect.Left, BoxRect.Top);
     CheckRect := Rect(RectPos.x +  3, RectPos.y + y, RectPos.x + Size, RectPos.y + Size + y);
  end else begin
     RectPos   := Point(BoxRect.Right, BoxRect.Top);
     CheckRect := Rect(RectPos.x - Size - 3 , RectPos.y + y, RectPos.x - Size-  6, RectPos.y + Size + y);
  end;
  xLeft := CheckRect.Bottom-CheckRect.Top;
  yTop  := CheckRect.Right -CheckRect.Left;
  CheckRect.Right := CheckRect.Left + Max(xLeft,yTop);
end;


procedure GetStyleText(Value:TAlignmentText; var Result:UINT);
begin
  case Value of
   stLeft   : result := DT_LEFT   or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
   stRight  : result := DT_RIGHT  or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
   stCenter : result := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  end;
end;

procedure GetCheckBoxPosition(Value:TCheckPosition; var Result:UINT);
begin
  case Value of
   bpLeft   : result := DT_LEFT   or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
   bpRight  : result := DT_RIGHT  or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  end;
end;

procedure SetTicketPoint(Value:TTicketPosition;Self,Ticket:TControl;TicketSpace:Integer);
var result : TPoint;
begin
  case Value of
    poTop:    result := Point(Self.Left, Self.Top - Ticket.Height - TicketSpace);
    poBottom: result := Point(Self.Left, Self.Top + Self.Height + TicketSpace);
    poLeft :  result := Point(Self.Left - Ticket.Width - TicketSpace, Self.Top +((Self.Height - Ticket.Height) div 2));
    poRight:  result := Point(Self.Left + Self.Width + TicketSpace, Self.Top +((Self.Height - Ticket.Height) div 2));
  end;
  Ticket.SetBounds(result.x, result.y, Ticket.Width, Ticket.Height);
end;

procedure DrawFocusRect(Canvas:TCanvas;FocusRect:TRect;Height:Integer);
begin
  FocusRect := Rect(FocusRect.left + 2, FocusRect.top + 2, FocusRect.Right - 2, FocusRect.top + Height - 2);
  Canvas.DrawFocusRect(FocusRect);
end;

function IndexInCount(Index,Count:Integer):boolean;
begin
  result :=(Index >= 0) and(Index < Count);
end;

procedure DrawBackdrop(Canvas:TCanvas; StartColor, StopColor: TColor; CanRect:TRect;Style:TStyleOrien);
   var
      iCounter, iBuffer, iFillStep: integer;
      bR1, bG1, bB1, bR2, bG2, bB2: byte;
      aColor1, aColor2: LongInt;
      dCurR, dCurG, dCurB, dRStep, dGStep, dBStep: double;
      iDrawLen, iDrawPos: integer;
      rCans : TRect;
      iLeft, iTop, iRight, iBottom: integer;
begin
      iLeft     := CanRect.Left;
      iTop      := CanRect.Top;
      iRight    := CanRect.Right;
      iBottom   := CanRect.Bottom;

      aColor1   := ColorToRGB(StartColor);
      bR1       := GetRValue(aColor1);
      bG1       := GetGValue(aColor1);
      bB1       := GetBValue(aColor1);

      aColor2   := ColorToRGB(StopColor);
      bR2       := GetRValue(aColor2);
      bG2       := GetGValue(aColor2);
      bB2       := GetBValue(aColor2);

      dCurR     := bR1;
      dCurG     := bG1;
      dCurB     := bB1;

      dRStep    :=(bR2-bR1) / 31;
      dGStep    :=(bG2-bG1) / 31;
      dBStep    :=(bB2-bB1) / 31;

      if Style = bsHorizontal then
         iDrawLen :=(iRight - iLeft)
      else
         iDrawLen :=(iBottom - iTop);

      iFillStep  :=(iDrawLen div 31) + 1;

      for iCounter := 0 to 31 do begin
          iBuffer            := iCounter * iDrawLen div 31;
          Canvas.Brush.Color := RGB(trunc(dCurR), trunc(dCurG), trunc(dCurB));
          dCurR              := dCurR + dRStep;
          dCurG              := dCurG + dGStep;
          dCurB              := dCurB + dBStep;
          if Style = bsHorizontal then begin
             iDrawPos    := iLeft + iBuffer + iFillStep;
             if iDrawPos > iRight then iDrawPos := iRight;
             rCans    := Rect(iLeft + iBuffer, iTop, iDrawPos, iBottom);
          end else begin
             iDrawPos := iTop + iBuffer + iFillStep;
             if iDrawPos > iBottom then iDrawPos := iBottom;
             rCans    := Rect(iLeft, iTop + iBuffer, iRight, iDrawPos);
          end;
          Canvas.FillRect(rCans);
      end;
end;

procedure DrawTransBitBlt(Cnv: TCanvas; x, y: Integer; Bmp: TBitmap; clTransparent: TColor);
var
  bmpXOR, bmpAND, bmpINV, bmpTAG: TBitmap;
  oldcol: Longint;
begin
  bmpAND    := TBitmap.Create;
  bmpINV    := TBitmap.Create;
  bmpXOR    := TBitmap.Create;
  bmpTAG := TBitmap.Create;
  try
    bmpAND.Width      := Bmp.Width;
    bmpAND.Height     := Bmp.Height;
    bmpAND.Monochrome := True;
    oldcol := SetBkColor(Bmp.Canvas.Handle, ColorToRGB(clTransparent)); 
    BitBlt(bmpAND.Canvas.Handle, 0, 0, Bmp.Width ,Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    SetBkColor(Bmp.Canvas.Handle, oldcol);

    bmpINV.Width      := Bmp.Width;
    bmpINV.Height     := Bmp.Height;
    bmpINV.Monochrome := True;
    BitBlt(bmpINV.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, bmpAND.Canvas.Handle, 0, 0, NOTSRCCOPY);

    bmpXOR.Width  := Bmp.Width;
    bmpXOR.Height := Bmp.Height;
    BitBlt(bmpXOR.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(bmpXOR.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, bmpINV.Canvas.Handle, 0, 0, SRCAND);

    bmpTAG.Width  := Bmp.Width;
    bmpTAG.Height := Bmp.Height;
    BitBlt(bmpTAG.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, Cnv.Handle, x, y, SRCCOPY);
    BitBlt(bmpTAG.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, bmpAND.Canvas.Handle, 0, 0, SRCAND);
    BitBlt(bmpTAG.Canvas.Handle, 0, 0,Bmp.Width,Bmp.Height, bmpXOR.Canvas.Handle, 0, 0, SRCINVERT);

    BitBlt(Cnv.Handle, x, y, Bmp.Width, Bmp.Height, bmpTAG.Canvas.Handle, 0, 0, SRCCOPY);
  finally 
    bmpXOR.Free;
    bmpAND.Free; 
    bmpINV.Free; 
    bmpTAG.Free; 
  end; 
end; 

procedure DrawParentImageSub(Control: TControl; Dest: TCanvas;const DefaultHeigth:integer=0);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    {$IFDEF DFS_COMPILER_2}
    GetViewportOrgEx(DC, @Position);
    {$ELSE}
    GetViewportOrgEx(DC, Position);
    {$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, DefaultHeigth);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure DrawParentImage(Control: TControl; Dest: TCanvas;const DefaultTop:integer=0);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    {$IFDEF DFS_COMPILER_2}
    GetViewportOrgEx(DC, @Position);
    {$ELSE}
    GetViewportOrgEx(DC, Position);
    {$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, DefaultTop, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

function DrawEllipse(Handle: HDC; Rect:TRect): BOOL;
begin
  result := Ellipse(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor, BackColor, HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
const
  ROP_DSPDxax = $00E20746;
var
  MonoBmp: TBitmap;
  IRect: TRect;
begin
  IRect := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    MonoBmp := TBitmap.Create;
    try
      with MonoBmp do begin
        Width := FOriginal.Width;
        Height := FOriginal.Height;
        Canvas.CopyRect(IRect, FOriginal.Canvas, IRect);
{$IFDEF DFS_DELPHI_3_UP}
        HandleType := bmDDB;
{$ENDIF}
        Canvas.Brush.Color := OutlineColor;
        if Monochrome then begin
          Canvas.Font.Color := clWhite;
          Monochrome := False;
          Canvas.Brush.Color := clWhite;
        end;
        Monochrome := True;
      end;
      with Result.Canvas do begin
        Brush.Color := BackColor;
        FillRect(IRect);
        if DrawHighlight then begin
          Brush.Color := HighlightColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 1, 1, IRect.Right - IRect.Left, IRect.Bottom - IRect.Top, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
        end;
        Brush.Color := ShadowColor;
        SetTextColor(Handle, clBlack);
        SetBkColor(Handle, clWhite);
        BitBlt(Handle, 0, 0, IRect.Right - IRect.Left, IRect.Bottom - IRect.Top, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function HSLtoRGB(H, S, L: double): TColor;
var
  M1, M2: double;

  function HueToColourValue(Hue: double) : byte;
  var
    V : double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else
      if Hue > 1 then
        Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 +(M2 - M1) * Hue * 6
    else
      if 2 * Hue < 1 then
        V := M2
      else
        if 3 * Hue < 2 then
          V := M1 +(M2 - M1) *(2/3 - Hue) * 6
        else
          V := M1;
    Result := round(255 * V)
  end;

var
  R, G, B: byte;
begin
  if S = 0 then
  begin
    R := round(255 * L);
    G := R;
    B := R
  end else begin
    if L <= 0.5 then
      M2 := L *(1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue(H + 1/3);
    G := HueToColourValue(H);
    B := HueToColourValue(H - 1/3)
  end;

  Result := RGB(R, G, B)
end;

function HSLRangeToRGB(H, S, L : integer): TColor;
begin
  Result := HSLToRGB(H /(HSLRange-1), S / HSLRange, L / HSLRange)
end;

// Convert RGB value(0-255 range) into HSL value(0-1 values)

procedure RGBtoHSL(RGB: TColor; var H, S, L : double);

  function Max(a, b : double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b
  end;

  function Min(a, b : double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R := GetRValue(RGB) / 255;
  G := GetGValue(RGB) / 255;
  B := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));

// calculate luminosity
  L :=(Cmax + Cmin) / 2;

  if Cmax = Cmin then  // it's grey
  begin
    H := 0; // it's actually undefined
    S := 0
  end else begin
    D := Cmax - Cmin;

// calculate Saturation
    if L < 0.5 then
      S := D /(Cmax + Cmin)
    else
      S := D /(2 - Cmax - Cmin);

// calculate Hue
    if R = Cmax then
      H :=(G - B) / D
    else
      if G = Cmax then
        H  := 2 +(B - R) /D
      else
        H := 4 +(R - G) / D;

    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

procedure RGBtoHSLRange(RGB: TColor; var H, S, L : integer);
var
  Hd, Sd, Ld: double;
begin
  RGBtoHSL(RGB, Hd, Sd, Ld);
  H := round(Hd *(HSLRange-1));
  S := round(Sd * HSLRange);
  L := round(Ld * HSLRange);
end;

function CalcAdvancedColor(ParentColor, OriginalColor: TColor; Percent: Byte; ColorType: TColorCalcType): TColor;
var
  H, S, L: integer;
begin
  if Percent <> 0 then
  begin
    RGBtoHSLRange(ColorToRGB(ParentColor), H, S, L);
    inc(L, 10);
    if ColorType = lighten then
      if L + Percent > 100 then
         L := 100
      else
        inc(L, Percent)
    else
      if L - Percent < 0 then
         L := 0
      else
         dec(L, Percent);

    Result := HSLRangeToRGB(H, S, L);
  end
  else
    Result := OriginalColor;
end;

procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect; const Offset: TPoint; Layout: TButtonLayout;
  Spacing, Margin: Integer; FGlyph: TBitmap; FNumGlyphs: Integer;
  const Caption: string; var TextBounds: TRect; var GlyphPos: TPoint);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  // calculate the item sizes
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if FGlyph <> nil then
    GlyphSize := Point(FGlyph.Width div FNumGlyphs, FGlyph.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
    begin
      TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or DT_SINGLELINE);
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
    end
  else
    begin
      TextBounds := Rect(0, 0, 0, 0);
      TextSize := Point(0, 0);
    end;

  // If the layout has the glyph on the right or the left, then both the
  // text and the glyph are centered vertically.  If the glyph is on the top
  // or the bottom, then both the text and the glyph are centered horizontally.
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y :=(ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y :=(ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X :=(ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X :=(ClientSize.X - TextSize.X + 1) div 2;
  end;

  // if there is no text or no bitmap, then Spacing is irrelevant
  if(TextSize.X = 0) or(GlyphSize.X = 0) then
    Spacing := 0;

  // adjust Margin and Spacing
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin :=(ClientSize.X - TotalSize.X) div 3
      else
        Margin :=(ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin :=(ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin :=(ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X -(Margin + GlyphSize.X), ClientSize.Y -(Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing :=(TotalSize.X - TextSize.X) div 2
      else
        Spacing :=(TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
    begin
      GlyphPos.X := Margin;
      TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
    end;
    blGlyphRight:
    begin
      GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
      TextPos.X := GlyphPos.X - Spacing - TextSize.X;
    end;
    blGlyphTop:
    begin
      GlyphPos.Y := Margin;
      TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
    end;
    blGlyphBottom:
    begin
      GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
      TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
    end;
  end;

  // fixup the result variables
    Inc(GlyphPos.X, Client.Left + Offset.X);
    Inc(GlyphPos.Y, Client.Top + Offset.Y);

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.X);
end;

function Min(const A, B: Integer): Integer;
begin      
  if A > B then
     Result := B
  else
     Result := A;
end;

function GetFontMetrics(Font: TFont): TTextMetric;
var
  DC: HDC;
  SaveFont: HFont;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Result);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
end;

function GetFontHeight(Font: TFont): Integer;
begin
  with GetFontMetrics(Font) do
    Result := Round(tmHeight + tmHeight / 8);
end;

function RectInRect(R1, R2: TRect): Boolean;
begin
  Result := IntersectRect(R1, R1, R2);
end;

function CheckByte(Value:Byte):Byte;
begin
  result := Value;
  if Value <= Low(Byte) then
     result := 1;
  if Value >= High(Byte) then
     result := High(Byte);
end;
{ TVersionControl }

function TVersionControl.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionComboBox }

function TVersionComboBox.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionGraphic }

function TVersionGraphic.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionTreeView }

function TVersionTreeView.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionComponent }

function TVersionComponent.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionListView }

function TVersionListView.GetVersion: String;
begin
  Result := FileVersion; 
end;

{ TVersionMemo }

function TVersionMemo.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionEdit }

function TVersionEdit.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionListBoxExt }

function TVersionListBoxExt.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionDBGrid }

function TVersionDBGrid.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionDrawGrid }

function TVersionDrawGrid.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionPages }

function TVersionPages.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionSheet }

function TVersionSheet.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionCtrlExt }

function TVersionCtrlExt.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TVersionObject }

function TVersionObject.GetVersion: String;
begin
  Result := FileVersion;
end;

{ TDefineRLE }

constructor TDefineRLE.Create;
begin
  inherited Create;
  GetMem(s, $FFFF);
  GetMem(t, $FFFF);
end;

destructor TDefineRLE.Destroy;
begin
  FreeMem(t);
  FreeMem(s);
  inherited Destroy;
end;

function TDefineRLE.Pack(Source, Target: Pointer; SourceSize: Integer): LongInt;
var
  w, tmp: Word;
  Sourc, Targ: LongType;
begin
{  // Move
  Move(Source^, Target^, SourceSize);
  Result:= SourceSize;
  Exit;{}

  // RLE Compress
  Sourc.Ptr := Source;
  Targ.Ptr := Target;
  Result := 0;
  while SourceSize <> 0 do
  begin
    if SourceSize > $FFFA then tmp := $FFFA
    else tmp := SourceSize;
    dec(SourceSize, tmp);
    move(Sourc.Ptr^, s^, tmp);
    w := PackSeg(s, t, tmp);
    inc(Sourc.Long, tmp);
    Move(w, Targ.Ptr^, 2);
    inc(Targ.Long, 2);
    Move(t^, Targ.Ptr^, w);
    inc(Targ.Long, w);
    Result := Result + w + 2;
  end;
end;


function TDefineRLE.PackFile(SourceFileName, TargetFileName: String): Boolean;
var
  Source, Target: Pointer;
  SourceFile, TargetFile: File;
  RequiredMaxSize, TargetFSize, FSize: LongInt;
begin
  AssignFile(SourceFile, SourceFileName);
  Reset(SourceFile, 1);
  FSize := FileSize(SourceFile);

  RequiredMaxSize := FSize + (FSize div $FFFF + 1) * 2;
  GetMem(Source, RequiredMaxSize);
  GetMem(Target, RequiredMaxSize);

  BlockRead(SourceFile, Source^, FSize);
  CloseFile(SourceFile);

  TargetFSize := Pack(Source, Target, FSize);

  AssignFile(TargetFile, TargetFileName);
  Rewrite(TargetFile, 1);
  { Also, you may put header }
  BlockWrite(TargetFile, FSize, SizeOf(FSize)); { Original file size (Only from 3.0) }
  BlockWrite(TargetFile, Target^, TargetFSize);
  CloseFile(TargetFile);

  FreeMem(Target, RequiredMaxSize);
  FreeMem(Source, RequiredMaxSize);

  Result := IOResult = 0;
end;


function TDefineRLE.PackSeg(Source, Target: Pointer; SourceSize: Word): Word;
begin
  asm
        push  esi
        push  edi
        push  eax
        push  ebx
        push  ecx
        push  edx
        cld
        xor   ecx, ecx
        mov   cx, SourceSize
        mov   edi, Target
        mov   esi, Source
        add   esi, ecx
        dec   esi
        lodsb
        inc   eax
        mov  [esi], al
        mov   ebx, edi
        add   ebx, ecx
        inc   ebx
        mov   esi, Source
        add   ecx, esi
        add   edi, 2
@CyclePack:
        cmp   ecx, esi
        je   @Konec
        lodsw
        stosb
        dec   esi
        cmp   al, ah
        jne  @CyclePack
        cmp   ax, [esi+1]
        jne  @CyclePack
        cmp   al, [esi+3]
        jne  @CyclePack
        sub   ebx, 2
        push  edi
        sub   edi, Target
        mov  [ebx], di
        pop   edi
        mov   edx, esi
        add   esi, 3
@Nimnul:
        inc   esi
        cmp   al, [esi]
        je   @Nimnul
        mov   eax, esi
        sub   eax, edx
        or    ah, ah
        jz   @M256
        mov   byte ptr [edi], 0
        inc   edi
        stosw
        jmp  @CyclePack
@M256:
        stosb
        jmp  @CyclePack
@Konec:
        push  ebx
        mov   ebx, Target
        mov   eax, edi
        sub   eax, ebx
        mov  [ebx], ax
        pop   ebx
        inc   ecx
        cmp   ebx, ecx
        je   @Lock1
        mov   esi, ebx
        sub   ebx, Target
        sub   ecx, Source
        sub   ecx, ebx
        rep   movsb
@Lock1:
        sub   edi, Target
        mov   Result, di
        pop   edx
        pop   ecx
        pop   ebx
        pop   eax
        pop   edi
        pop   esi
  end;
end;


function TDefineRLE.PackString(Source: String): String;
var
  PC, PC2: PChar;
  SS, TS: Integer;
begin
  SS := Length(Source);
  GetMem(PC, SS);
  GetMem(PC2, SS + 8); // If line can't be packed its size can be longer
  Move(Source[1], PC^, SS);
  TS := Pack(PC, PC2, SS);
  SetLength(Result, TS + 4);
  Move(SS, Result[1], 4);
  Move(PC2^, Result[5], TS);
  FreeMem(PC2);
  FreeMem(PC);
end;


function TDefineRLE.UnPack(Source, Target: Pointer; SourceSize: Integer): LongInt;
var
  Increment, i: LongInt;
  tmp: Word;
  Swap: LongType;
begin
{  // Move
  Move(Source^, Target^, SourceSize);
  Result:= SourceSize;
  Exit;{}

  // RLE Decompress
  Increment := 0;
  Result := 0;
  while SourceSize <> 0 do
  begin
    Swap.Ptr := Source;
    inc(Swap.Long, Increment);
    Move(Swap.Ptr^, tmp, 2);
    inc(Swap.Long, 2);
    dec(SourceSize, tmp + 2);
    i := UnPackSeg(Swap.Ptr, t, tmp);
    Swap.Ptr := Target;
    inc(Swap.Long, Result);
    inc(Result, i);
    Move(t^, Swap.Ptr^, i);
    inc(Increment, tmp + 2);
  end;
end;

function TDefineRLE.UnPackFile(SourceFileName, TargetFileName: String): Boolean;
var
  Source, Target: Pointer;
  SourceFile, TargetFile: File;
  OriginalFileSize, FSize: LongInt;
begin
  AssignFile(SourceFile, SourceFileName);
  Reset(SourceFile, 1);
  FSize := FileSize(SourceFile) - SizeOf(OriginalFileSize);

  { Read header ? }
  BlockRead(SourceFile, OriginalFileSize, SizeOf(OriginalFileSize));

  GetMem(Source, FSize);
  GetMem(Target, OriginalFileSize);

  BlockRead(SourceFile, Source^, FSize);
  CloseFile(SourceFile);

  UnPack(Source, Target, FSize);

  AssignFile(TargetFile, TargetFileName);
  Rewrite(TargetFile, 1);
  BlockWrite(TargetFile, Target^, OriginalFileSize);
  CloseFile(TargetFile);

  FreeMem(Target, OriginalFileSize);
  FreeMem(Source, FSize);

  Result := IOResult = 0;
end; 

function TDefineRLE.UnPackSeg(Source, Target: Pointer; SourceSize: Word): Word;
begin
  asm
        push  esi
        push  edi
        push  eax
        push  ebx
        push  ecx
        push  edx
        cld
        mov   esi, Source
        mov   edi, Target
        mov   ebx, esi
        xor   edx, edx
        mov   dx, SourceSize
        add   ebx, edx
        mov   dx, word ptr [esi]
        add   edx, esi
        add   esi, 2
@UnPackCycle:
        cmp   edx, ebx
        je   @Konec2
        sub   ebx, 2
        xor   ecx, ecx
        mov   cx, word ptr [ebx]
        add   ecx, Source
        sub   ecx, esi
        dec   ecx
        rep   movsb
        lodsb
        mov   cl, byte ptr [esi]
        inc   esi
        or    cl, cl
        jnz  @Low1
        xor   ecx, ecx
        mov   cx, word ptr [esi]
        add   esi, 2
@Low1:
        inc   ecx
        rep   stosb
        jmp  @UnPackCycle
@Konec2:
        mov   ecx, edx
        sub   ecx, esi
        rep   movsb
        sub   edi, Target
        mov   Result, di
        pop   edx
        pop   ecx
        pop   ebx
        pop   eax
        pop   edi
        pop   esi
  end;
end;

function TDefineRLE.UnPackString(Source: String): String;
var
  PC, PC2: PChar;
  SS, TS: Integer;
begin
  SS := Length(Source) - 4;
  GetMem(PC, SS);
  Move(Source[1], TS, 4);
  GetMem(PC2, TS);
  Move(Source[5], PC^, SS);
  TS := UnPack(PC, PC2, SS);
  SetLength(Result, TS);
  Move(PC2^, Result[1], TS);
  FreeMem(PC2);
  FreeMem(PC);
end;

end.
