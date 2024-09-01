{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvWebBrowser;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$DEFINE USEEDGE}
{$ENDIF}

{$IFDEF USEEDGE}
{$DEFINE EDGESUPPORT}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FMXLIB}
  UITypes, FMX.Types, System.Messaging,
  {$ENDIF}
  Types, AdvCustomControl, AdvGraphics, AdvUtils, AdvTypes,
  Classes, TypInfo,
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  Generics.Collections,
  {$ELSE}
  fgl,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Contnrs,
  {$ENDIF}
  StdCtrls, Forms, Controls
  {$IFDEF ANDROID}
  ,FMX.Platform.Android, AndroidApi.JNI.Embarcadero, AndroidApi.JNI.App, Androidapi.JNI.GraphicsContentViewText, AndroidApi.JNI.JavaTypes,
  AndroidApi.JNI.Widget, FMX.Helpers.Android, AndroidApi.JNIBridge, AndroidApi.Helpers
  {$ENDIF}
  {$IFDEF IOS}
  ,FMX.Platform.iOS, iOSApi.CocoaTypes, iOSApi.CoreGraphics, iOSApi.UIKit, iOSApi.Foundation,
  Macapi.ObjectiveC, MacApi.ObjcRuntime
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  ,MacApi.Foundation, MacApi.AppKit, MacApi.CocoaTypes, FMX.Platform.Mac
  {$ENDIF}
  ,MacApi.Helpers
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 4; // Release nr.
  BLD_VER = 7; // Build nr.
  DESIGNTIMEMESSAGE = 'Selectable/Draggable area (only active at designtime)';

  //v1.0.0.0 : First release
  //v1.0.0.1 : Improved : Interfaces added to Edge Chromium to add JavaScript object injection
  //           (bridge to Delphi) and capture preview capabilities in TAdvWebBrowser
  //1.1.0.0 : New : CaptureScreenShot method
  //        : New : JavaScript bridge support
  //        : Fixed : Issue with Visibility / Parenting in FMX
  //1.1.1.0 : Improved : Support for Microsoft Edge Chromium Stable v85.0.564.41 detection/fallback mechanism
  //1.1.2.0 : Improved : CacheFolderName & ClearCache for defining Edge Chromium cache folder
  //        : Improved : Moved Edge detection entry points and variables to be available at application level
  //1.1.2.1 : Fixed : Issue detecting stable version v86.0.622.38 (copy to temp folder solution)
  //1.1.3.0 : New : CacheFolder, CacheFolderName & AutoClearCache properties
  //        : Improved : Changed ClearCache property to method to manually clear cache
  //        : Fixed : Issues with clearing cache when AutoClearCache = False
  //1.1.4.0 : New : EnableAcceleratorKeys to enable/disable keyboard accelerator keys
  //1.1.4.1 : Fixed : Issue with default value of EnableAcceleratorKeys
  //1.1.4.2 : Fixed : Issue with reparenting in Windows
  //1.1.4.3 : Fixed : Issue with scaling in FMX
  //1.1.4.4 : Fixed : Issue with focus exception in combination with TMS Scripter
  //1.1.4.5 : Fixed : Issue with keyboard handling in macOS
  //1.1.4.6 : Fixed : Issue with border being visible in Linux GTK
  //1.1.4.7 : Fixed : Issue with retaining and releasing focus in macOS

type
  TAdvWebBrowserJavaScriptCompleteEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const AValue: string){$IFDEF LCLLIB} of object{$ENDIF};

  TAdvCustomWebBrowser = class;

  IAdvCustomWebBrowserBridge = interface(IInterface)
  {$IFDEF FNCLIB}
  ['{AC2934EC-9397-4A99-8E0A-1EF58803C8EA}']
  {$ELSE}
  ['{E639C360-3FD0-4F63-9132-E8D0044EF860}']
  {$ENDIF}
    function GetObjectMessage: string;
    procedure SetObjectMessage(const AValue: string);
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

  IAdvCustomWebBrowser = interface(IInterface)
  {$IFDEF FNCLIB}
  ['{F74562D0-56C7-4ED2-B01B-8C9C16DD9361}']
  {$ELSE}
  ['{8CE780C0-D22F-4063-993B-CFB0DD7D1351}']
  {$ENDIF}
    procedure SetFocus;
    procedure SetUserAgent(const AValue: string);
    procedure SetURL(const AValue: string);
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    procedure SetCacheFolderName(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure SetCacheFolder(const Value: string);
    procedure Navigate(const AURL: String); overload;
    procedure Navigate; overload;
    procedure LoadHTML(AHTML: String);
    procedure LoadFile(AFile: String);
    procedure GoForward;
    procedure GoBack;
    procedure Reload;
    procedure Close;
    procedure StopLoading;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject);
    procedure RemoveBridge(ABridgeName: string);
    procedure UpdateBounds;
    procedure CaptureScreenShot;
    procedure UpdateVisible;
    procedure UpdateEnabled;
    procedure BeforeChangeParent;
    procedure Initialize;
    procedure DeInitialize;
    procedure ClearCache;
    procedure ShowDebugConsole;
    function GetUserAgent: string;
    function GetURL: string;
    function GetExternalBrowser: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    function GetEnableAcceleratorKeys: Boolean;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    function NativeEnvironment: Pointer;
    function NativeBrowser: Pointer;
    function GetBrowserInstance: IInterface;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function NativeDialog: Pointer;
    function IsFMXBrowser: Boolean;
    function GetCacheFolderName: string;
    function GetAutoClearCache: Boolean;
    function GetCacheFolder: string;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property CacheFolder: string read GetCacheFolder write SetCacheFolder;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property URL: string read GetURL write SetURL;
    property ExternalBrowser: Boolean read GetExternalBrowser write SetExternalBrowser;
    property EnableContextMenu: Boolean read GetEnableContextMenu write SetEnableContextMenu;
    property EnableShowDebugConsole: Boolean read GetEnableShowDebugConsole write SetEnableShowDebugConsole;
    property EnableAcceleratorKeys: Boolean read GetEnableAcceleratorKeys write SetEnableAcceleratorKeys;
  end;

  IAdvWebBrowserService = interface(IInterface)
  {$IFDEF FNCLIB}
  ['{4B7A5FE1-A889-47C6-B40F-A611BB6266E6}']
  {$ELSE}
  ['{94C7BF89-2B0A-40DF-8C24-B393CEB91389}']
  {$ENDIF}
    function CreateWebBrowser(const AValue: TAdvCustomWebBrowser): IAdvCustomWebBrowser;
    procedure DeleteCookies;
    procedure DestroyWebBrowser(const AValue: IAdvCustomWebBrowser);
  end;

  TAdvCustomWebBrowserNavigateCompleteParams = record
    URL: String;
    Success: Boolean;
    ErrorCode: Integer;
  end;

  TAdvCustomWebBrowserBeforeNavigateParams = record
    URL: String;
    Cancel: Boolean;
  end;

  TAdvCustomWebBrowserNavigateComplete = procedure(Sender: TObject; var Params: TAdvCustomWebBrowserNavigateCompleteParams) of object;
  TAdvCustomWebBrowserBeforeNavigate = procedure(Sender: TObject; var Params: TAdvCustomWebBrowserBeforeNavigateParams) of object;
  TAdvCustomWebBrowserCaptureScreenShot = procedure(Sender: TObject; AScreenShot: TAdvBitmap) of object;

  {$IFNDEF WEBLIB}
  TAdvWebBrowserDocumentReadyStateThread = class(TThread)
  private
    FWebBrowser: TAdvCustomWebBrowser;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebBrowser: TAdvCustomWebBrowser);
  end;
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TAdvScript = class
  private
    FScript: string;
    FCompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent;
  public
    constructor Create(AScript: string; ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent);
  end;

  TAdvScriptList = class(TObjectList<TAdvScript>);
  {$ENDIF}

  TAdvCustomWebBrowser = class(TAdvCustomControl)
  private
    FDesigntimeEnabled: Boolean;
    FCanDestroyDispatch: Boolean;
    FSyncValue: string;
    FSyncValueExecuted: Boolean;
    FThreadDone: Boolean;
    FReady: Boolean;
    FInitialized: Boolean;
    {$IFNDEF WEBLIB}
    FScriptList: TAdvScriptList;
    FDocumentReadyStateThread: TAdvWebBrowserDocumentReadyStateThread;
    {$ENDIF}
    FWebBrowser: IAdvCustomWebBrowser;
    FURL: string;
    FExternalBrowser, FEnableShowDebugConsole, FEnableAcceleratorKeys, FEnableContextMenu: Boolean;
    FOnNavigateComplete: TAdvCustomWebBrowserNavigateComplete;
    FOnBeforeNavigate: TAdvCustomWebBrowserBeforeNavigate;
    FOnHardwareButtonClicked: TNotifyEvent;
    FOnInitialized: TNotifyEvent;
    FOnCloseForm: TNotifyEvent;
    FOnDocumentComplete: TNotifyEvent;
    FOnCaptureScreenShot: TAdvCustomWebBrowserCaptureScreenShot;
    function GetURL: string;
    procedure SetURL(const Value: string);
    function GetExternalBrowser: Boolean;
    procedure SetExternalBrowser(const Value: Boolean);
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    procedure SetEnableContextMenu(const Value: Boolean);
    procedure SetEnableShowDebugConsole(const Value: Boolean);
    function GetCacheFolder: string;
    function GetCacheFolderName: string;
    function GetAutoClearCache: Boolean;
    procedure SetCacheFolder(const Value: string);
    procedure SetCacheFolderName(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure SetDesigntimeEnabled(const Value: Boolean);
    function GetEnableAcceleratorKeys: Boolean;
    procedure SetEnabledAcceleratorKeys(const Value: Boolean);
    function GetUserAgent: string;
    procedure SetUserAgent(const Value: string);
    procedure DoExecuteJavaScriptSync(const AValue: string);
    {$IFDEF FMXLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION > 32}
    procedure FormHandleCreated(const Sender: TObject; const Msg: TMessage);
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
  protected
    function GetDocURL: string; override;
    function GetVersion: string; override;
    function CanCreateBrowser: Boolean;
    function CanBeVisible: Boolean; virtual;
    function CheckIdentifier: Boolean; virtual;
    class procedure DeleteCookies; virtual;
    {$IFNDEF WEBLIB}
    procedure StartDocumentReadyStateThread; virtual;
    procedure DoTerminate(Sender: TObject);
    procedure StopDocumentReadyStateThread;
    procedure DoScriptHandled(Sender: TObject);
    {$ENDIF}
    {$IFDEF WEBLIB}
    procedure UpdateElement; override;
    {$ENDIF}
    procedure DoEnter; override;
    procedure ChangeDPIScale({%H-}M, {%H-}D: Integer); override;
    procedure CreateClasses; virtual;
    procedure DoDocumentComplete; virtual;
    procedure CheckApplicationInitialized;
    procedure DoCheckReadyState(const AValue: string);
    procedure DoCheckIdentifier(const AValue: string);
    procedure DoHardwareButtonClicked; virtual;
    procedure DoCloseForm; virtual;
    procedure DoCaptureScreenShot(AScreenShot: TAdvBitmap); virtual;
    procedure BeforeNavigate(var Params: TAdvCustomWebBrowserBeforeNavigateParams); virtual;
    procedure NavigateComplete(var Params: TAdvCustomWebBrowserNavigateCompleteParams); virtual;
    procedure Initialized; virtual;
    {$IFDEF FMXLIB}
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure SetParent(const Value: TFmxObject); override;
    procedure SetVisible(const Value: Boolean); override;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFDEF CMNLIB}
    procedure SetParent(Value: TWinControl); override;
    {$ELSE}
    procedure SetParent(Value: TControl); override;
    {$ENDIF}
    procedure SetEnabled(Value: Boolean); override;
    {$ENDIF}
    procedure DoKeyPressed(var Key: Word); virtual;
    procedure Draw({%H-}AGraphics: TAdvGraphics; {%H-}ARect: TRectF); override;
    procedure Loaded; override;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
    property ExternalBrowser: Boolean read GetExternalBrowser write SetExternalBrowser default False;
    procedure Navigate; overload; virtual;
    procedure Navigate(const AURL: string); overload; virtual;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); virtual;
    function ExecuteJavaScriptSync(AScript: string): string; virtual;
    procedure LoadHTML(AHTML: String); virtual;
    procedure LoadFile(AFile: String); virtual;
    procedure Initialize; virtual;
    procedure DeInitialize; virtual;
    procedure GoForward; virtual;
    procedure GoBack; virtual;
    procedure Reload; virtual;
    procedure StopLoading; virtual;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject); virtual;
    procedure RemoveBridge(ABridgeName: string); virtual;
    procedure CaptureScreenShot; virtual;
    procedure ShowDebugConsole; virtual;
    property URL: string read GetURL write SetURL;
    function GetBridgeCommunicationLayer(ABridgeName: string): string; virtual;
    function NativeEnvironment: Pointer; virtual;
    function NativeBrowser: Pointer; virtual;
    function IsFMXBrowser: Boolean; virtual;
    function CanGoBack: Boolean; virtual;
    function CanGoForward: Boolean; virtual;
    {$IFDEF ANDROID}
    function NativeDialog: Pointer; virtual;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    function GetWebBrowserInstance: IInterface; virtual;
    {$ENDIF}
    property OnCloseForm: TNotifyEvent read FOnCloseForm write FOnCloseForm;
    property OnBeforeNavigate: TAdvCustomWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TAdvCustomWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnHardwareButtonClicked: TNotifyEvent read FOnHardwareButtonClicked write FOnHardwareButtonClicked;
    property OnDocumentComplete: TNotifyEvent read FOnDocumentComplete write FOnDocumentComplete;
    property OnCaptureScreenShot: TAdvCustomWebBrowserCaptureScreenShot read FOnCaptureScreenShot write FOnCaptureScreenShot;
    property EnableContextMenu: Boolean read GetEnableContextMenu write SetEnableContextMenu default True;
    property EnableShowDebugConsole: Boolean read GetEnableShowDebugConsole write SetEnableShowDebugConsole default True;
    property EnableAcceleratorKeys: Boolean read GetEnableAcceleratorKeys write SetEnabledAcceleratorKeys default True;
    property CacheFolder: string read GetCacheFolder write SetCacheFolder;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property CanDestroyDispatch: Boolean read FCanDestroyDispatch write FCanDestroyDispatch;
    property DesigntimeEnabled: Boolean read FDesigntimeEnabled write SetDesigntimeEnabled default True;
    procedure ClearCache; virtual;
    {$IFDEF FMXLIB}
    procedure Show; override;
    procedure Hide; override;
    procedure Move; override;
    procedure DoAbsoluteChanged; override;
    {$ENDIF}
    {$IFDEF VCLLIB}
    procedure CreateWnd; override;
    {$ENDIF}
    function CanRecreate: Boolean; virtual;
    function CanLoadDefaultHTML: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
    procedure UpdateControlAfterResize; override;
    property Version: string read GetVersion;
  end;

  {$IFDEF WEBLIB}
  TAdvWebBrowserList = class(TList)
  private
    function GetItem(Index: Integer): IAdvCustomWebBrowser;
    procedure SetItem(Index: Integer; const Value: IAdvCustomWebBrowser);
  public
    property Items[Index: Integer]: IAdvCustomWebBrowser read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvWebBrowserList = class(TList<IAdvCustomWebBrowser>);
  {$ENDIF}

  TAdvWebBrowserFactoryService = class(TInterfacedObject, IAdvWebBrowserService)
  protected
    FWebBrowsers: TAdvWebBrowserList;
    function DoCreateWebBrowser(const AValue: TAdvCustomWebBrowser): IAdvCustomWebBrowser; virtual; abstract;
    procedure DoRemoveWebBrowser(const AValue: IAdvCustomWebBrowser);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateWebBrowser(const AValue: TAdvCustomWebBrowser): IAdvCustomWebBrowser;
    procedure DeleteCookies; virtual; abstract;
    procedure DestroyWebBrowser(const AValue: IAdvCustomWebBrowser);
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TAdvWebBrowser = class(TAdvCustomWebBrowser)
  public
    {$IFNDEF WEBLIB}
    procedure StartDocumentReadyStateThread; override;
    {$ENDIF}
    procedure Navigate; overload; override;
    procedure Navigate(const AURL: string); overload; override;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); override;
    function ExecuteJavaScriptSync(AScript: string): string; override;
    procedure LoadHTML(AHTML: String); override;
    procedure LoadFile(AFile: String); override;
    procedure Initialize; override;
    procedure DeInitialize; override;
    procedure GoForward; override;
    procedure GoBack; override;
    procedure Reload; override;
    procedure StopLoading; override;
    procedure ShowDebugConsole; override;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject); override;
    procedure RemoveBridge(ABridgeName: string); override;
    procedure CaptureScreenShot; override;
    function GetBridgeCommunicationLayer(ABridgeName: string): string; override;
    function NativeEnvironment: Pointer; override;
    function NativeBrowser: Pointer; override;
    function IsFMXBrowser: Boolean; override;
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    {$IFDEF ANDROID}
    function NativeDialog: Pointer; override;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    function GetWebBrowserInstance: IInterface; override;
    {$ENDIF}
    property OnCloseForm;
    property EnableContextMenu;
    property EnableShowDebugConsole;
    property EnableAcceleratorKeys;
    property CacheFolder;
    property CacheFolderName;
    property AutoClearCache;
    procedure ClearCache; override;
    property UserAgent;
  published
    property OnInitialized;
    property URL;
    property OnBeforeNavigate;
    property OnNavigateComplete;
    property OnHardwareButtonClicked;
    property OnCaptureScreenShot;
    property OnDocumentComplete;
    property Version;
    property DesigntimeEnabled;
  end;

  TAdvWebBrowserPopup = class;

  {$IFDEF FMXMOBILE}
  TAdvWebBrowserPopupForm = class(TCommonCustomForm)
  {$ELSE}
  TAdvWebBrowserPopupForm = class(TCustomForm)
  {$ENDIF}
  private
    FWebBrowserPopup: TAdvWebBrowserPopup;
  protected
    procedure UpdateBackGround;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  {$IFDEF IOS}
  IAdvWebBrowserPopupButtonEventHandler = interface(NSObject)
  ['{723E9874-7EEF-4E40-896D-9E2DAC8E6DD4}']
    procedure Click; cdecl;
  end;

  TAdvWebBrowserPopupButtonEventHandler = class(TOCLocal)
  private
    FWebBrowserPopup: TAdvWebBrowserPopup;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure Click; cdecl;
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  TAdvWebBrowserPopupButtonEventHandler = class(TJavaLocal, JView_OnClickListener)
  private
    FWebBrowserPopup: TAdvWebBrowserPopup;
  public
    procedure onClick(P1: JView); cdecl;
  end;
  {$ENDIF}

  TAdvCustomWebBrowserClass = class of TAdvCustomWebBrowser;

  {$IFDEF FMXLIB}
  TAdvWebBrowserFormPosition = TFormPosition;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TAdvWebBrowserFormPosition = TPosition;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TAdvWebBrowserPopup = class(TComponent)
  private
    {$IFDEF MSWINDOWS}
    FLoadURL: String;
    FFirstLoad: Boolean;
    {$ENDIF}
    FModal: Boolean;
    {$IFDEF IOS}
    FButton: UIButton;
    {$ENDIF}
    {$IFNDEF ANDROID}
    {$IFNDEF CMNWEBLIB}
    FPopup: TPopup;
    {$ENDIF}
    FWebBrowserForm: TAdvWebBrowserPopupForm;
    {$ENDIF}
    {$IFDEF ANDROID}
    FButton: JButton;
    {$ENDIF}
    {$IFDEF FMXMOBILE}
    FButtonEventHandler: TAdvWebBrowserPopupButtonEventHandler;
    {$ENDIF}
    FWebBrowser: TAdvCustomWebBrowser;
    FOnNavigateComplete: TAdvCustomWebBrowserNavigateComplete;
    FOnBeforeNavigate: TAdvCustomWebBrowserBeforeNavigate;
    FURL: String;
    FPosition: TAdvWebBrowserFormPosition;
    FWidth: Integer;
    FHeight: Integer;
    FT: Integer;
    FL: Integer;
    FFullScreen: Boolean;
    FCloseButton: Boolean;
    FOnClose: TNotifyEvent;
    FCloseButtonText: String;
    FExternalBrowser: Boolean;
  protected
    procedure ButtonClose(Sender: TObject);
    {$IFDEF MSWINDOWS}
    procedure FormShow(Sender: TObject);
    {$ENDIF}
    {$IFNDEF ANDROID}
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    {$ENDIF}
    procedure CloseForm(Sender: TObject);
    procedure DoBeforeNavigate(Sender: TObject; var Params: TAdvCustomWebBrowserBeforeNavigateParams); virtual;
    procedure DoNavigateComplete(Sender: TObject; var Params: TAdvCustomWebBrowserNavigateCompleteParams); virtual;
    procedure InitializeWebBrowser(AWebBrowser: TAdvCustomWebBrowser); virtual;
    function GetWebBrowserClass: TAdvCustomWebBrowserClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(AModal: Boolean = True): TModalResult; overload;
    function Open(AURL: String; AModal: Boolean = True): TModalResult; overload;
    procedure Close(AModalResult: TModalResult = mrOk);
    property WebBrowser: TAdvCustomWebBrowser read FWebBrowser;
    property ExternalBrowser: Boolean read FExternalBrowser write FExternalBrowser default False;
  published
    property OnBeforeNavigate: TAdvCustomWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TAdvCustomWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property URL: String read FURL write FURL;
    {$IFDEF FMXLIB}
    property Position: TAdvWebBrowserFormPosition read FPosition write FPosition default TFormPosition.ScreenCenter;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    property Position: TAdvWebBrowserFormPosition read FPosition write FPosition default poScreenCenter;
    {$ENDIF}
    property FullScreen: Boolean read FFullScreen write FFullScreen default False;
    property Width: Integer read FWidth write FWidth default 640;
    property Height: Integer read FHeight write FHeight default 480;
    property Left: Integer read FL write FL default 0;
    property Top: Integer read FT write FT default 0;
    property CloseButton: Boolean read FCloseButton write FCloseButton default False;
    property CloseButtonText: String read FCloseButtonText write FCloseButtonText;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TAdvWebBrowserPlatformServicesService = class
  private
    FInterface: IInterface;
    FGUID: string;
  public
    constructor Create(AGUID: string; AInterface: IInterface);
    property GUID: string read FGUID;
    property &Interface: IInterface read FInterface;
  end;

  {$IFDEF WEBLIB}
  TAdvWebBrowserPlatformServicesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAdvWebBrowserPlatformServicesService;
    procedure SetItem(Index: Integer; const Value: TAdvWebBrowserPlatformServicesService);
  public
    property Items[Index: Integer]: TAdvWebBrowserPlatformServicesService read GetItem write SetItem; default;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvWebBrowserPlatformServicesList = class(TObjectList<TAdvWebBrowserPlatformServicesService>)
  {$ENDIF}
  private
    function GetValue(AGUID: string): IInterface;
  public
    function ContainsKey(AGUID: string): Boolean;
    procedure RemoveByGUID(AGUID: string);
    property Interfaces[AGUID: string]: IInterface read GetValue;
  end;

  TAdvWebBrowserPlatformServices = class
  private
    FServicesList: TAdvWebBrowserPlatformServicesList;
    class var FCurrent: TAdvWebBrowserPlatformServices;
    class var FCurrentReleased: Boolean;
{$IFNDEF AUTOREFCOUNT}
    class procedure ReleaseCurrent;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
    procedure RemovePlatformService(const AServiceGUID: TGUID);
    function Count: Integer;
    function GetPlatformService(const AServiceGUID: TGUID): IInterface;
    function SupportsPlatformService(const AServiceGUID: TGUID): Boolean; overload;
    function SupportsPlatformService(const AServiceGUID: TGUID; var AService: IInterface): Boolean; overload;
    class function Current: TAdvWebBrowserPlatformServices;
  end;

{$IFDEF MACOS}
function NSStrEx(AString: string): NSString;
{$ENDIF}

implementation

uses
  SysUtils, AdvGraphicsTypes
{$IFDEF FMXLIB}
  ,FMX.Platform
{$ENDIF}
{$IFDEF WEBLIB}
  ,AdvWebBrowser.WEB
{$ENDIF}
{$IFDEF MSWINDOWS}
  ,AdvWebBrowser.Win
{$ENDIF}
{$IFDEF UNIX}
  ,AdvWebBrowser.Unix
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  ,AdvWebBrowser.iOS
{$ELSE}
  ,AdvWebBrowser.Mac
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  ,AdvWebBrowser.Android
{$ENDIF}
  ;

var
  FTrial: Integer = 0;

function Hiword(L: DWORD): integer;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): Integer;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1,b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1,i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

{$IFDEF MACOS}
function NSStrEx(AString: string): NSString;
begin
  Result := StrToNSStr(AString);
end;
{$ENDIF}

{$IFDEF IOS}
function GetSharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;
{$ENDIF}

{$IFDEF FREEWARE}
function ScrambleEx(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  {$IFDEF ZEROSTRINGINDEX}
  for i := 0 to length(s) - 1 do
  {$ELSE}
  for i := 1 to length(s) do
  {$ENDIF}
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}

{ TAdvCustomWebBrowser }

{$IFDEF FMXLIB}
procedure TAdvCustomWebBrowser.Show;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
begin
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.RemoveBridge(ABridgeName);
    FWebBrowser.AddBridge(ABridgeName, ABridgeObject);
  end;
end;

procedure TAdvCustomWebBrowser.BeforeNavigate(
  var Params: TAdvCustomWebBrowserBeforeNavigateParams);
begin
  if Assigned(OnBeforeNavigate) then
    OnBeforeNavigate(Self, Params);
end;

function TAdvCustomWebBrowser.CanBeVisible: Boolean;
begin
  Result := True;
end;

function TAdvCustomWebBrowser.CheckIdentifier: Boolean;
begin
  Result := False;
end;

function TAdvCustomWebBrowser.CanCreateBrowser: Boolean;
begin
  {$IFDEF EDGESUPPORT}
  Result := True;
  {$ELSE}
  {$IFDEF WEBLIB}
  Result := True;
  {$ELSE}
  Result := not IsDesigning
  {$ENDIF}
  {$ENDIF}
end;

function TAdvCustomWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CanGoBack;
end;

function TAdvCustomWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CanGoForward;
end;

procedure TAdvCustomWebBrowser.CaptureScreenShot;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CaptureScreenShot;
end;

procedure TAdvCustomWebBrowser.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TAdvCustomWebBrowser.CheckApplicationInitialized;
begin
  ExecuteJavaScript('document.readyState', {$IFDEF LCLWEBLIB}@{$ENDIF}DoCheckReadyState);
end;

procedure TAdvCustomWebBrowser.ClearCache;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.ClearCache;
end;

constructor TAdvCustomWebBrowser.Create(AOwner: TComponent);
var
  WebBrowserService: IAdvWebBrowserService;
begin
  inherited;
  FCanDestroyDispatch := True;
  FDesigntimeEnabled := True;
  {$IFDEF FMXLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 32}
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, FormHandleCreated);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFNDEF WEBLIB}
  FScriptList := TAdvScriptList.Create;
  {$ENDIF}
  FExternalBrowser := False;
  FEnableShowDebugConsole := True;
  FEnableContextMenu := True;
  FEnableAcceleratorKeys := True;
  if CanCreateBrowser then
    if TAdvWebBrowserPlatformServices.Current.SupportsPlatformService(IAdvWebBrowserService, IInterface(WebBrowserService)) then
      FWebBrowser := WebBrowserService.CreateWebBrowser(Self);

  CreateClasses;

  Width := 500;
  Height := 350;

  {$IFNDEF UNIX}
  {$IFNDEF MSWINDOWS}
  Initialized;
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF WEBLIB}
procedure TAdvCustomWebBrowser.UpdateElement;
begin
  inherited;
  if Assigned(ElementHandle) then
    ElementHandle.style.setProperty('overflow', 'auto');
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.CreateClasses;
begin
//
end;

destructor TAdvCustomWebBrowser.Destroy;
var
  WebBrowserService: IAdvWebBrowserService;
begin
  {$IFNDEF WEBLIB}
  StopDocumentReadyStateThread;
  {$ENDIF}

  if CanCreateBrowser and TAdvWebBrowserPlatformServices.Current.SupportsPlatformService(IAdvWebBrowserService, IInterface(WebBrowserService)) then
    WebBrowserService.DestroyWebBrowser(FWebBrowser);

  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.Deinitialize;
    FWebBrowser := nil;
  end;

  {$IFNDEF WEBLIB}
  FScriptList.Free;
  {$ENDIF}

  {$IFDEF FMXLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 32}
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FormHandleCreated);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  inherited;
end;

procedure TAdvCustomWebBrowser.DoCaptureScreenShot(
  AScreenShot: TAdvBitmap);
begin
  if Assigned(OnCaptureScreenShot) then
    OnCaptureScreenShot(Self, AScreenShot);
end;

procedure TAdvCustomWebBrowser.DoCheckIdentifier(const AValue: string);
begin
  if LowerCase(StringReplace(AValue, '"', '', [rfReplaceAll])) = 'unknown' then
    FReady := True;
end;

procedure TAdvCustomWebBrowser.DoDocumentComplete;
begin
  if Assigned(OnDocumentComplete) then
    OnDocumentComplete(Self);
end;

procedure TAdvCustomWebBrowser.DoCheckReadyState(const AValue: string);
begin
  if LowerCase(StringReplace(AValue, '"', '', [rfReplaceAll]))  = 'complete' then
  begin
    if CheckIdentifier then
      ExecuteJavaScript('window.TMSWEBCoreClientIdentifier', {$IFDEF LCLWEBLIB}@{$ENDIF}DoCheckIdentifier)
    else
      FReady := True;
  end;
end;

procedure TAdvCustomWebBrowser.DoCloseForm;
begin
  if Assigned(OnCloseForm) then
    OnCloseForm(Self);
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomWebBrowser.DoTerminate(Sender: TObject);
begin
  FThreadDone := True;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.DoHardwareButtonClicked;
begin
  if Assigned(OnHardwareButtonClicked) then
    OnHardwareButtonClicked(Self);
end;

procedure TAdvCustomWebBrowser.DoKeyPressed(var Key: Word);
begin

end;

{$IFNDEF WEBLIB}
procedure TAdvCustomWebBrowser.DoScriptHandled(Sender: TObject);
var
  s: TAdvScript;
begin
  if Assigned(FWebBrowser) then
  begin
    if FScriptList.Count > 0 then
      FScriptList.Delete(0);

    if FScriptList.Count > 0 then
    begin
      s := FScriptList[0];
      FWebBrowser.ExecuteJavaScript(s.FScript, s.FCompleteEvent, {$IFDEF LCLWEBLIB}@{$ENDIF}DoScriptHandled);
    end;
  end;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
var
  s: string;
  sz: Single;
begin
  inherited;
  if IsDesigning then
  begin
    AGraphics.Font.Size := 12;
    AGraphics.Font.Name := 'Montserrat';
    {$IFDEF MSWINDOWS}
    if not EdgeLoaded then
      AGraphics.DrawText(RectF(0, 0, Width, Height), EErrorMessageNoDLL, True, gtaCenter, gtaCenter)
    else
    {$ENDIF}
    s := DESIGNTIMEMESSAGE;
    sz := AGraphics.CalculateTextHeight(s);
    AGraphics.DrawText(RectF(ScalePaintValue(5), ScalePaintValue(5), Width - ScalePaintValue(5), sz + ScalePaintValue(5)), s);
  end;
end;

procedure TAdvCustomWebBrowser.DoEnter;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.SetFocus;
end;

procedure TAdvCustomWebBrowser.DoExecuteJavaScriptSync(const AValue: string);
begin
  FSyncValue := AValue;
  FSyncValueExecuted := True;
end;

function TAdvCustomWebBrowser.ExecuteJavaScriptSync(AScript: string): string;
{$IFNDEF WEBLIB}
{$IFNDEF ANDROID}
var
  i: Integer;
{$ENDIF}
{$ENDIF}
begin
  Result := '';
  if Assigned(FWebBrowser) then
  begin
    FSyncValue := '';
    FSyncValueExecuted := False;
    FWebBrowser.ExecuteJavaScript(AScript, {$IFDEF LCLWEBLIB}@{$ENDIF}DoExecuteJavaScriptSync, nil);
    {$IFNDEF ANDROID}
    {$IFNDEF WEBLIB}
    i := 0;
    while not FSyncValueExecuted and (i <= 60000) do
    begin
      Application.ProcessMessages;
      Sleep(1);
      Inc(i);
    end;
    {$ENDIF}
    {$ENDIF}

    Result := FSyncValue;
  end;
end;

procedure TAdvCustomWebBrowser.ExecuteJavaScript(AScript: String; ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  if Assigned(FWebBrowser) then
  begin
    if AImmediate and FInitialized then
      FWebBrowser.ExecuteJavaScript(AScript, ACompleteEvent, nil)
    else
    begin
      {$IFNDEF WEBLIB}
      FScriptList.Add(TAdvScript.Create(AScript, ACompleteEvent));
      if FScriptList.Count = 1 then
      begin
        if FInitialized then
          FWebBrowser.ExecuteJavaScript(AScript, ACompleteEvent, {$IFNDEF WEBLIB}{$IFDEF LCLLIB}@{$ENDIF}DoScriptHandled{$ELSE}nil{$ENDIF})
        else
          DoScriptHandled(nil);
      end
      else if not FInitialized then
        DoScriptHandled(nil);
      {$ELSE}
      FWebBrowser.ExecuteJavaScript(AScript, ACompleteEvent, {$IFNDEF WEBLIB}{$IFDEF LCLLIB}@{$ENDIF}DoScriptHandled{$ELSE}nil{$ENDIF});
      {$ENDIF}
    end;
  end;
end;

function TAdvCustomWebBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
const
  LB = #13;
begin
  Result :=
    'var send' + ABridgeName + 'ObjectMessage = function(parameters) {' + LB +
    '  var v = parameters;' + LB +
    {$IFDEF ANDROID}
    '  if (!' + ABridgeName + ') {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  ' + ABridgeName + '.Setjsvalue(v); ' + LB +
    '  ' + ABridgeName + '.performClick();' + LB +
    {$ENDIF}
    {$IFDEF MACOS}
    '  if (!window.webkit || !window.webkit.messageHandlers || !window.webkit.messageHandlers.' + ABridgeName + ') {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  window.webkit.messageHandlers.' + ABridgeName + '.postMessage(v);' + LB +
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    'if (!window.chrome || !window.chrome.webview || !window.chrome.webview.hostObjects || !window.chrome.webview.hostObjects.sync) {' + LB +
    '  return;' + LB +
    '}' + LB +
    'var obj = window.chrome.webview.hostObjects.sync.' + ABridgeName + ';' + LB +
    '  if (obj) {' + LB +
    '    obj.ObjectMessage = v;' + LB +
    '  }' + LB +
    {$ENDIF}
    {$IFDEF WEBLIB}
    '  var event = new CustomEvent("' + ABridgeName + '", {detail: v});' + LB +
    '  ' + ElementID + '.dispatchEvent(event);' + LB +
    {$ENDIF}
    {$IFDEF LINUX}
    '  if (!window.webkit || !window.webkit.messageHandlers || !window.webkit.messageHandlers.' + ABridgeName + ') {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  window.webkit.messageHandlers.' + ABridgeName + '.postMessage(v);' + LB +
    {$ENDIF}
    '};';
end;

function TAdvCustomWebBrowser.GetCacheFolder: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CacheFolder;
end;

function TAdvCustomWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CacheFolderName;
end;

function TAdvCustomWebBrowser.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfnccore/components/ttmsfncwebbrowser';
end;

function TAdvCustomWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.AutoClearCache;
end;

function TAdvCustomWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableAcceleratorKeys
  else
    Result := FEnableAcceleratorKeys;
end;

function TAdvCustomWebBrowser.GetEnableContextMenu: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableContextMenu
  else
    Result := FEnableContextMenu;
end;

function TAdvCustomWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableShowDebugConsole
  else
    Result := FEnableShowDebugConsole;
end;

function TAdvCustomWebBrowser.GetExternalBrowser: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.ExternalBrowser
  else
    Result := FExternalBrowser;
end;

function TAdvCustomWebBrowser.GetURL: string;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.URL
  else
    Result := FURL;
end;

function TAdvCustomWebBrowser.GetUserAgent: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.UserAgent;
end;

procedure TAdvCustomWebBrowser.GoBack;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.GoBack;
end;

function TAdvCustomWebBrowser.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomWebBrowser.GoForward;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.GoForward;
end;

{$IFDEF VCLLIB}
procedure TAdvCustomWebBrowser.CreateWnd;
begin
  if CanRecreate and (not isLoading and not IsDestroying) then
  begin
    Deinitialize;
    inherited;
    Initialize;
  end
  else
    inherited;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvCustomWebBrowser.Hide;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TAdvCustomWebBrowser.DoAbsoluteChanged;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.Initialize;
begin
  if (IsDesigning and FDesigntimeEnabled) or not IsDesigning then
  begin
    if Assigned(FWebBrowser) then
      FWebBrowser.Initialize;
  end;
end;

procedure TAdvCustomWebBrowser.Initialized;
begin
  FInitialized := True;
  if Assigned(OnInitialized) then
    OnInitialized(Self);
end;

function TAdvCustomWebBrowser.CanLoadDefaultHTML: Boolean;
begin
  Result := True;
end;

function TAdvCustomWebBrowser.CanRecreate: Boolean;
begin
  Result := True;
end;

function TAdvCustomWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.IsFMXBrowser;
end;

procedure TAdvCustomWebBrowser.Loaded;
begin
  inherited;
  Initialize;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TAdvCustomWebBrowser.LoadFile(AFile: String);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.LoadFile(AFile);
end;

procedure TAdvCustomWebBrowser.LoadHTML(AHTML: String);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.LoadHTML(AHTML);
end;

{$IFDEF FMXLIB}
procedure TAdvCustomWebBrowser.Move;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

{$IFDEF ANDROID}
function TAdvCustomWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeDialog;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TAdvCustomWebBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.GetBrowserInstance;
end;
{$ENDIF}

function TAdvCustomWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeBrowser;
end;

function TAdvCustomWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeEnvironment;
end;

procedure TAdvCustomWebBrowser.Navigate(const AURL: string);
begin
  FReady := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.Navigate(AURL);
end;

procedure TAdvCustomWebBrowser.NavigateComplete(
  var Params: TAdvCustomWebBrowserNavigateCompleteParams);
begin
  if Assigned(OnNavigateComplete) then
    OnNavigateComplete(Self, Params);
end;

procedure TAdvCustomWebBrowser.Reload;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.Reload;
end;

procedure TAdvCustomWebBrowser.RemoveBridge(ABridgeName: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.RemoveBridge(ABridgeName);
end;

procedure TAdvCustomWebBrowser.UpdateControlAfterResize;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TAdvCustomWebBrowser.Navigate;
begin
  FReady := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.Navigate;
end;

procedure TAdvCustomWebBrowser.SetCacheFolder(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CacheFolder := Value;
end;

procedure TAdvCustomWebBrowser.SetCacheFolderName(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CacheFolderName := Value;
end;

procedure TAdvCustomWebBrowser.SetDesigntimeEnabled(const Value: Boolean);
begin
  FDesigntimeEnabled := Value;
  if IsDesigning then
  begin
    if FDesigntimeEnabled and not FInitialized then
      Initialize
    else
      DeInitialize;
  end;
end;

procedure TAdvCustomWebBrowser.SetAutoClearCache(const Value: Boolean);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.AutoClearCache := Value;
end;

procedure TAdvCustomWebBrowser.SetEnableContextMenu(const Value: Boolean);
begin
  FEnableContextMenu := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableContextMenu := Value;
end;

{$IFDEF FMXLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 32}
procedure TAdvCustomWebBrowser.FormHandleCreated(const Sender: TObject; const Msg: TMessage);

  function GetParentForm(Control: TFmxObject): TCommonCustomForm;
  begin
    if (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
      Result := TCommonCustomForm(Control.Root.GetObject)
    else
      Result := nil;
  end;

begin
  if not (csDesigning in ComponentState) and ((FWebBrowser = nil) or (Sender = GetParentForm(self as TFmxObject))) then
  begin
    DeInitialize;
    Initialize;
  end;
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}

procedure TAdvCustomWebBrowser.SetEnabledAcceleratorKeys(
  const Value: Boolean);
begin
  FEnableAcceleratorKeys := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableAcceleratorKeys := Value;
end;

procedure TAdvCustomWebBrowser.SetEnableShowDebugConsole(const Value: Boolean);
begin
  FEnableShowDebugConsole := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableShowDebugConsole := Value;
end;

procedure TAdvCustomWebBrowser.SetExternalBrowser(const Value: Boolean);
begin
  FExternalBrowser := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.ExternalBrowser := Value;
end;

{$IFDEF CMNWEBLIB}
procedure TAdvCustomWebBrowser.SetEnabled(Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateEnabled;
end;

{$IFDEF CMNLIB}
procedure TAdvCustomWebBrowser.SetParent(Value: TWinControl);
{$ELSE}
procedure TAdvCustomWebBrowser.SetParent(Value: TControl);
{$ENDIF}
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.BeforeChangeParent;
  inherited;
  Initialize;
  {$IFDEF WEBLIB}
  BeginUpdate;
  EndUpdate;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvCustomWebBrowser.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TAdvCustomWebBrowser.SetEnabled(const Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateEnabled;
end;

procedure TAdvCustomWebBrowser.AncestorVisibleChanged(const Visible: Boolean);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateVisible;
end;

procedure TAdvCustomWebBrowser.SetParent(const Value: TFmxObject);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.BeforeChangeParent;
  inherited;
  Initialize;
end;

procedure TAdvCustomWebBrowser.SetVisible(const Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateVisible;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.SetURL(const Value: string);
begin
  FURL := Value;
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.URL := Value;
    if CanCreateBrowser and (Value <> '') then
      Navigate;
  end;
end;

procedure TAdvCustomWebBrowser.SetUserAgent(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.UserAgent := Value;
end;

procedure TAdvCustomWebBrowser.ShowDebugConsole;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.ShowDebugConsole;
end;

procedure TAdvCustomWebBrowser.DeInitialize;
begin
  FInitialized := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.DeInitialize;
end;

class procedure TAdvCustomWebBrowser.DeleteCookies;
var
  WebBrowserService: IAdvWebBrowserService;
begin
  inherited;
  if TAdvWebBrowserPlatformServices.Current.SupportsPlatformService(IAdvWebBrowserService, IInterface(WebBrowserService)) then
    WebBrowserService.DeleteCookies;
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomWebBrowser.StartDocumentReadyStateThread;
begin
  StopDocumentReadyStateThread;

  if not Assigned(FDocumentReadyStateThread) then
  begin
    FDocumentReadyStateThread := TAdvWebBrowserDocumentReadyStateThread.Create(Self);
    FDocumentReadyStateThread.OnTerminate := DoTerminate;
  end;
end;

procedure TAdvCustomWebBrowser.StopDocumentReadyStateThread;
begin
  if Assigned(FDocumentReadyStateThread) then
  begin
    FDocumentReadyStateThread.Terminate;
    FDocumentReadyStateThread.WaitFor;

    while not FThreadDone do
    begin
      Sleep(100);
      Application.ProcessMessages;
    end;

    FreeAndNil(FDocumentReadyStateThread);
    FThreadDone := False;
  end;
end;
{$ENDIF}

procedure TAdvCustomWebBrowser.StopLoading;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.StopLoading;
end;

{ TAdvWebBrowserFactoryService }

constructor TAdvWebBrowserFactoryService.Create;
begin
  inherited Create;
  FWebBrowsers := TAdvWebBrowserList.Create;
end;

function TAdvWebBrowserFactoryService.CreateWebBrowser(const AValue: TAdvCustomWebBrowser): IAdvCustomWebBrowser;
begin
  Result := DoCreateWebBrowser(AValue);
  FWebBrowsers.Add(Result);
end;

destructor TAdvWebBrowserFactoryService.Destroy;
begin
  FreeAndNil(FWebBrowsers);
  inherited Destroy;
end;

procedure TAdvWebBrowserFactoryService.DestroyWebBrowser(
  const AValue: IAdvCustomWebBrowser);
begin
  DoRemoveWebBrowser(AValue);
end;

procedure TAdvWebBrowserFactoryService.DoRemoveWebBrowser(
  const AValue: IAdvCustomWebBrowser);
begin
  if (FWebBrowsers <> nil) and (AValue <> nil) then
    FWebBrowsers.Remove(AValue);
end;

{ TAdvWebBrowserPopup }

procedure TAdvWebBrowserPopup.DoBeforeNavigate(Sender: TObject;
  var Params: TAdvCustomWebBrowserBeforeNavigateParams);
begin
  if Assigned(OnBeforeNavigate) then
    OnBeforeNavigate(Self, Params);
end;

procedure TAdvWebBrowserPopup.Close(AModalResult: TModalResult = mrOk);
begin
  {$IFDEF ANDROID}
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.DisposeOf;
    FWebBrowser := nil;
  end;
  if Assigned(OnClose) then
    OnClose(Self);
  {$ELSE}
  if Assigned(FWebBrowserForm) then
  begin
    if FModal then
      FWebBrowserForm.ModalResult := AModalResult
    else
    begin
      if Assigned(FWebBrowserForm) then
        FWebBrowserForm.Close;
    end;
  end;
  {$ENDIF}
end;

procedure TAdvWebBrowserPopup.CloseForm(Sender: TObject);
begin
  Close;
end;

constructor TAdvWebBrowserPopup.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF ANDROID}
  {$IFNDEF CMNWEBLIB}
  FPopup := TPopup.Create(Self);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  FPosition := TFormPosition.ScreenCenter;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  FPosition := poScreenCenter;
  {$ENDIF}
  {$IFDEF FMXMOBILE}
  FButtonEventHandler := TAdvWebBrowserPopupButtonEventHandler.Create;
  FButtonEventHandler.FWebBrowserPopup := Self;
  {$ENDIF}
  FFullScreen := False;
  FWidth := 640;
  FHeight := 480;
  FL := 0;
  FT := 0;
  FExternalBrowser := False;
  if csDesigning in ComponentState then
    FCloseButtonText := 'Close';
end;

destructor TAdvWebBrowserPopup.Destroy;
begin
  {$IFNDEF ANDROID}
  {$IFNDEF CMNWEBLIB}
  if Assigned(FPopup) then
    FPopup := nil;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXMOBILE}
  if Assigned(FButtonEventHandler) then
  begin
    FButtonEventHandler.Free;
    FButtonEventHandler := nil;
  end;
  {$ENDIF}
  if Assigned(FWebBrowser) then
  begin
    {$IFDEF LCLLIB}
    FWebBrowser.Free;
    {$ELSE}
    FWebBrowser.DisposeOf;
    {$ENDIF}
    FWebBrowser := nil;
  end;
  inherited;
end;

{$IFDEF MSWINDOWS}
procedure TAdvWebBrowserPopup.FormShow(Sender: TObject);
begin
  if FFirstLoad then
  begin
    FFirstLoad := False;
    if Assigned(FWebBrowser) then
      FWebBrowser.URL := FLoadURL;
  end;
end;
{$ENDIF}

function TAdvWebBrowserPopup.GetWebBrowserClass: TAdvCustomWebBrowserClass;
begin
  Result := TAdvCustomWebBrowser;
end;

procedure TAdvWebBrowserPopup.InitializeWebBrowser(
  AWebBrowser: TAdvCustomWebBrowser);
begin

end;

{$IFNDEF ANDROID}
procedure TAdvWebBrowserPopup.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FModal then
  begin
    {$IFDEF MACOS}
    {$IFDEF IOS}
    Action := TCloseAction.caFree;
    {$ENDIF}
    {$ELSE}
    Action := TCloseAction.caFree;
    {$ENDIF}
  end
  else
    Action := TCloseAction.caFree;

  FWebBrowserForm := nil;
  FWebBrowser := nil;

  if Assigned(OnClose) then
    OnClose(Self);
end;

{$ENDIF}

procedure TAdvWebBrowserPopup.DoNavigateComplete(Sender: TObject;
  var Params: TAdvCustomWebBrowserNavigateCompleteParams);
begin
  if Assigned(OnNavigateComplete) then
    OnNavigateComplete(Self, Params);
end;

function TAdvWebBrowserPopup.Open(AURL: String;
  AModal: Boolean = True): TModalResult;
begin
  URL := AURL;
  Result := Open(AModal);
end;

procedure TAdvWebBrowserPopup.ButtonClose(Sender: TObject);
begin
  Close;
end;

function TAdvWebBrowserPopup.Open(AModal: Boolean = True): TModalResult;
{$IFNDEF FMXMOBILE}
var
  b: TButton;
{$ENDIF}
{$IFDEF IOS}
var
  h: TiOSWindowHandle;
  wbv: UIView;
  p: Pointer;
{$ENDIF}
{$IFDEF ANDROID}
var
  wb: JWebBrowser;
  {$IF COMPILERVERSION < 32}
  wnd: JWindow;
  dl: JDialog;
  {$ENDIF}
  ll: JLinearLayout;
{$ENDIF}
begin
  try
    FModal := AModal;
    {$IFDEF ANDROID}
    FModal := False;
    {$ENDIF}

    {$IFNDEF ANDROID}
    {$IFDEF CMNWEBLIB}
    FWebBrowserForm := TAdvWebBrowserPopupForm.CreateNew(Application);
    {$ELSE}
    FWebBrowserForm := TAdvWebBrowserPopupForm.CreateNew(FPopup);
    {$ENDIF}
    FWebBrowserForm.FWebBrowserPopup := Self;
    {$IFDEF DELPHHI_LLVM}
    FWebBrowserForm.FullScreen := FullScreen;
    {$ENDIF}
    FWebBrowserForm.OnClose := FormClose;
    {$IFDEF MSWINDOWS}
    FFirstLoad := True;
    FWebBrowserForm.OnShow := FormShow;
    {$ENDIF}
    FWebBrowserForm.Position := Position;
    FWebBrowserForm.Left := Left;
    FWebBrowserForm.Top := Top;
    FWebBrowserForm.Width := Width;
    FWebBrowserForm.Height := Height;
    {$ENDIF}

    FWebBrowser := GetWebBrowserClass.Create(Self);
    FWebBrowser.ExternalBrowser := ExternalBrowser;
    FWebBrowser.OnCloseForm := CloseForm;
    FWebBrowser.OnBeforeNavigate := DoBeforeNavigate;
    FWebBrowser.OnNavigateComplete := DoNavigateComplete;
    InitializeWebBrowser(FWebBrowser);

    {$IFDEF MSWINDOWS}
    FLoadURL := URL;
    if FWebBrowser.ExternalBrowser then
      FWebBrowser.URL := URL;
    {$ELSE}
    FWebBrowser.URL := URL;
    {$ENDIF}
    {$IFNDEF FMXMOBILE}
    FWebBrowser.ControlAlignment := caClient;
    {$ENDIF}

    {$IFDEF ANDROID}
    FWebBrowser.Parent := Application.MainForm;
    if FullScreen then
    begin
      FWebBrowser.ControlAlignment := caClient;
    end
    else
    begin
      case Position of
        TFormPosition.ScreenCenter, TFormPosition.DesktopCenter, TFormPosition.MainFormCenter,
          TFormPosition.OwnerFormCenter:
        FWebBrowser.Align := TAlignLayout.Center;
      end;
    end;
    {$ELSE}
    FWebBrowser.Parent := FWebBrowserForm;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FWebBrowser.Position.X := Left;
    FWebBrowser.Position.Y := Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FWebBrowser.Left := Left;
    FWebBrowser.Top := Top;
    {$ENDIF}
    FWebBrowser.Width := Width;
    FWebBrowser.Height := Height;

    if CloseButton then
    begin
      {$IFNDEF FMXMOBILE}
      b := TButton.Create(FWebBrowserForm);
      b.Parent := FWebBrowserForm;
      {$IFDEF FMXLIB}
      b.Text := CloseButtonText;
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      b.Caption := CloseButtonText;
      {$ENDIF}
      b.OnClick := ButtonClose;
      {$IFNDEF FMXMOBILE}
      {$IFDEF FMXLIB}
      b.Align := TAlignLayout.Top;
      {$ENDIF}
      {$IFNDEF FMXLIB}
      b.Align := alTop;
      {$ENDIF}
      {$ELSE}
      if FullScreen then
      begin
        {$IFDEF FMXLIB}
        b.Align := TAlignLayout.Top;
        {$ENDIF}
        {$IFNDEF FMXLIB}
        b.Align := alTop;
        {$ENDIF}
      end;

      {$IFDEF FMXLIB}
      FWebBrowser.Position.Y := FWebBrowser.Position.Y + b.Height;
      FWebBrowser.Height := FWebBrowser.Height - b.Height;
      b.Position.X := FWebBrowser.Position.X;
      b.Position.Y := FWebBrowser.Position.Y - b.Height;
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      FWebBrowser.Position.Y := FWebBrowser.Top + b.Height;
      FWebBrowser.Height := FWebBrowser.Height - b.Height;
      b.Position.X := FWebBrowser.Left;
      b.Position.Y := FWebBrowser.Top - b.Height;
      {$ENDIF}
      b.Width := FWebBrowserForm.Width;
      {$ENDIF}
      {$ELSE}
      {$IFDEF IOS}
      FButton := TUIButton.Wrap(TUIButton.OCClass.buttonWithType(UIButtonTypeRoundedRect));
      FButton.addTarget(FButtonEventHandler.GetObjectID, sel_getUid('Click'), UIControlEventTouchUpInside);
      FButton.setTitle(NSStrEx(CloseButtonText), UIControlStateNormal);
      FWebBrowser.Position.Y := FWebBrowser.Position.Y + FButton.frame.size.height;
      FWebBrowser.Height := FWebBrowser.Height - FButton.frame.size.height;
      FButton.setFrame(CGRectMake(FWebBrowser.Position.X, FWebBrowser.Position.Y - FButton.frame.size.height, FWebBrowser.Width, 30));
      {$ELSE}
      CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FButton := TJButton.JavaClass.init(SharedActivityEx);
        FButton.setText(StrToJCharSequence(CloseButtonText));
        FButton.setOnClickListener(FButtonEventHandler);
        wb := TJWebBrowser.Wrap(FWebBrowser.NativeBrowser);
        ll := TJLinearLayout.Wrap((wb.getParent as ILocalObject).GetObjectID);
        ll.addView(FButton, 0);
      end
      );
      {$ENDIF}
      {$ENDIF}
    end;

    {$IFDEF ANDROID}
    {$HINTS ON}
    {$IF COMPILERVERSION < 32}
    CallInUIThreadAndWaitFinishing(
    procedure
    begin
      dl := TJDialog.Wrap(FWebBrowser.NativeDialog);
      wnd := dl.getWindow;
      wnd.clearFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_NOT_TOUCH_MODAL);
      wnd.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_DIM_BEHIND);
    end
    );
    {$IFEND}
    {$HINTS OFF}
    FWebBrowser.SetFocus;
    {$ENDIF}

    {$IFDEF IOS}
    p := FWebBrowser.NativeBrowser;
    if Assigned(p) then
    begin
      wbv := TUIView.Wrap(p);
      wbv.layer.setShadowColor(TUIColor.OCClass.blackColor);
      wbv.layer.setShadowColor(TUIColor.Wrap(TUIColor.OCClass.blackColor).CGColor);
      wbv.layer.setShadowOffset(CGSizeMake(1,1));
      wbv.layer.setShadowRadius(5);
      wbv.layer.setShadowOpacity(0.75);

      h := WindowHandleToPlatform(FWebBrowserForm.Handle);
      h.View.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.whiteColor).colorWithAlphaComponent(0.75));
      if Assigned(FButton) then
      begin
        h.View.addSubview(FButton);
        FButton.layer.setShadowColor(TUIColor.OCClass.blackColor);
        FButton.layer.setShadowColor(TUIColor.Wrap(TUIColor.OCClass.blackColor).CGColor);
        FButton.layer.setShadowOffset(CGSizeMake(1,1));
        FButton.layer.setShadowRadius(5);
        FButton.layer.setShadowOpacity(0.75);
        h.View.sendSubviewToBack(FButton);
      end;
    end;
    {$ENDIF}

    if ExternalBrowser then
    begin
      if Assigned(FWebBrowser) then
      begin
        {$IFDEF LCLLIB}
        FWebBrowser.Free;
        {$ELSE}
        FWebBrowser.DisposeOf;
        {$ENDIF}
        FWebBrowser := nil;
      end;

      {$IFNDEF ANDROID}
      if Assigned(FWebBrowserForm) then
      begin
        FWebBrowserForm.Free;
        FWebBrowserForm := nil;
      end;
      {$ENDIF}

      Result := mrOk;
      Exit;
    end;

    {$IFNDEF ANDROID}
    if FModal then
    begin
      Result := FWebBrowserForm.ShowModal;
      {$IFDEF MACOS}
      {$IFNDEF IOS}
      FWebBrowserForm.Free;
      FWebBrowserForm := nil;
      {$ENDIF}
      {$ENDIF}
    end
    else
    begin
      FWebBrowserForm.Show;
      Result := mrOk;
    end;
    {$ELSE}
    Result := mrOk;
    {$ENDIF}
  finally
  end;
end;

{ TAdvWebBrowserPopupForm }

procedure TAdvWebBrowserPopupForm.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  UpdateBackGround;
end;

procedure TAdvWebBrowserPopupForm.UpdateBackGround;
var
  wb: TAdvCustomWebBrowser;
  {$IFDEF IOS}
  btn: UIButton;
  {$ENDIF}
begin
  if Assigned(FWebBrowserPopup) then
  begin
    wb := FWebBrowserPopup.FWebBrowser;
    if Assigned(wb) then
    begin
      if FWebBrowserPopup.FullScreen then
      begin
        wb.SetBounds(0, 0, Width, Height);
      end
      else
      begin
        {$IFDEF FMXLIB}
        case Position of
          TFormPosition.ScreenCenter, TFormPosition.DesktopCenter, TFormPosition.MainFormCenter,
            TFormPosition.OwnerFormCenter:
          begin
            wb.SetBounds((Width - FWebBrowserPopup.Width) / 2 , (Height - FWebBrowserPopup.Height) / 2, FWebBrowserPopup.Width, FWebBrowserPopup.Height);
          end;
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        case Position of
          poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter:
          begin
            wb.SetBounds((Width - FWebBrowserPopup.Width) div 2 , (Height - FWebBrowserPopup.Height) div 2, FWebBrowserPopup.Width, FWebBrowserPopup.Height);
          end;
        {$ENDIF}
          else
          begin
            wb.SetBounds(FWebBrowserPopup.Left, FWebBrowserPopup.Top, FWebBrowserPopup.Width, FWebBrowserPopup.Height);
          end;
        end;
      end;
      {$IFDEF IOS}
      if Assigned(FWebBrowserPopup.FButton) and FWebBrowserPopup.CloseButton then
      begin
        btn := FWebBrowserPopup.FButton;
        btn.setFrame(CGRectMake(wb.Position.X, wb.Position.Y - FWebBrowserPopup.FButton.frame.size.height, wb.Width, FWebBrowserPopup.FButton.frame.size.height));
        wb.Position.Y := wb.Position.Y + btn.frame.size.height;
        wb.Height := wb.Height - btn.frame.size.height;
        btn.setFrame(CGRectMake(wb.Position.X, wb.Position.Y - btn.frame.size.height, wb.Width, 30));
      end;
      {$ENDIF}
    end;
  end;
end;

{$IFNDEF WEBLIB}

{ TAdvScript }

constructor TAdvScript.Create(AScript: string;
  ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent);
begin
  FScript := AScript;
  FCompleteEvent := ACompleteEvent;
end;
{$ENDIF}

{$IFDEF IOS}

{ TAdvWebBrowserPopupButtonEventHandler }

procedure TAdvWebBrowserPopupButtonEventHandler.Click;
begin
  if Assigned(FWebBrowserPopup) then
    FWebBrowserPopup.Close;
end;

function TAdvWebBrowserPopupButtonEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IAdvWebBrowserPopupButtonEventHandler);
end;
{$ENDIF}

{$IFDEF ANDROID}

{ TAdvWebBrowserPopupButtonEventHandler }

procedure TAdvWebBrowserPopupButtonEventHandler.onClick(P1: JView);
begin
  if Assigned(FWebBrowserPopup) then
    FWebBrowserPopup.Close;
end;

{$ENDIF}

{$IFNDEF WEBLIB}

{ TAdvWebBrowserDocumentReadyStateThread }

constructor TAdvWebBrowserDocumentReadyStateThread.Create(AWebBrowser:
  TAdvCustomWebBrowser);
begin
  inherited Create(False);
  FWebBrowser := AWebBrowser;
end;

procedure TAdvWebBrowserDocumentReadyStateThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(FWebBrowser) then
    begin
      Synchronize(FWebBrowser.CheckApplicationInitialized);
      Sleep(100);
      if FWebBrowser.FReady then
      begin
        Synchronize(FWebBrowser.DoDocumentComplete);
        Terminate;
      end;
    end
    else
      Terminate;
  end;
end;

{$ENDIF}

{ TAdvWebBrowserPlatformServices }

function TAdvWebBrowserPlatformServices.Count: Integer;
begin
  Result := FServicesList.Count;
end;

procedure TAdvWebBrowserPlatformServices.AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
var
  LService: IInterface;
begin
  if not FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
  begin
    if Supports(AService, AServiceGUID, LService) then
      FServicesList.Add(TAdvWebBrowserPlatformServicesService.Create(GUIDToString(AServiceGUID), AService));
  end;
end;

constructor TAdvWebBrowserPlatformServices.Create;
begin
  inherited;
  FServicesList := TAdvWebBrowserPlatformServicesList.Create;
end;

destructor TAdvWebBrowserPlatformServices.Destroy;
begin
  FreeAndNil(FServicesList);
  inherited;
end;

{$IFNDEF AUTOREFCOUNT}
class procedure TAdvWebBrowserPlatformServices.ReleaseCurrent;
begin
  FreeAndNil(FCurrent);
  FCurrentReleased := True;
end;
{$ENDIF}

class function TAdvWebBrowserPlatformServices.Current: TAdvWebBrowserPlatformServices;
begin
  if (FCurrent = nil) and not FCurrentReleased then
    FCurrent := TAdvWebBrowserPlatformServices.Create;
  Result := FCurrent;
end;

function TAdvWebBrowserPlatformServices.GetPlatformService(const AServiceGUID: TGUID): IInterface;
var
  k: IInterface;
begin
  k := FServicesList.Interfaces[GUIDToString(AServiceGUID)];
  Supports(k, AServiceGUID, Result);
end;

procedure TAdvWebBrowserPlatformServices.RemovePlatformService(const AServiceGUID: TGUID);
begin
  FServicesList.RemoveByGUID(GUIDToString(AServiceGUID));
end;

function TAdvWebBrowserPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID;
  var AService: IInterface): Boolean;
begin
  if FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
    Result := Supports(FServicesList.Interfaces[GUIDToString(AServiceGUID)], AServiceGUID, AService)
  else
  begin
    AService := nil;
    Result := False;
  end;
end;

function TAdvWebBrowserPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID): Boolean;
begin
  Result := FServicesList.ContainsKey(GUIDToString(AServiceGUID));
end;

{ TAdvWebBrowserPlatformServicesService }

constructor TAdvWebBrowserPlatformServicesService.Create(AGUID: string;
  AInterface: IInterface);
begin
  FGUID := AGUID;
  FInterface := AInterface;
end;

{ TAdvWebBrowserPlatformServicesList }

function TAdvWebBrowserPlatformServicesList.ContainsKey(AGUID: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TAdvWebBrowserPlatformServicesList.GetValue(AGUID: string): IInterface;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Result := Items[I].&Interface;
      Break;
    end;
  end;
end;

procedure TAdvWebBrowserPlatformServicesList.RemoveByGUID(AGUID: string);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Remove(Items[I]);
      Break;
    end;
  end;
end;

{$IFDEF WEBLIB}
function TAdvWebBrowserList.GetItem(Index: Integer): IAdvCustomWebBrowser;
begin
  Result := IAdvCustomWebBrowser(inherited Items[Index]);
end;

procedure TAdvWebBrowserList.SetItem(Index: Integer; const Value: IAdvCustomWebBrowser);
begin
  inherited Items[Index] := Value;
end;

function TAdvWebBrowserPlatformServicesList.GetItem(Index: Integer): TAdvWebBrowserPlatformServicesService;
begin
  Result := TAdvWebBrowserPlatformServicesService(inherited Items[Index]);
end;

procedure TAdvWebBrowserPlatformServicesList.SetItem(Index: Integer; const Value: TAdvWebBrowserPlatformServicesService);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TAdvWebBrowser }

procedure TAdvWebBrowser.AddBridge(ABridgeName: string; ABridgeObject: TObject);
begin
  inherited AddBridge(ABridgeName, ABridgeObject);
end;

function TAdvWebBrowser.CanGoBack: Boolean;
begin
  Result := inherited CanGoBack;
end;

function TAdvWebBrowser.CanGoForward: Boolean;
begin
  Result := inherited CanGoForward;
end;

procedure TAdvWebBrowser.CaptureScreenShot;
begin
  inherited CaptureScreenShot;
end;

procedure TAdvWebBrowser.ClearCache;
begin
  inherited ClearCache;
end;

procedure TAdvWebBrowser.DeInitialize;
begin
  inherited DeInitialize;
end;

procedure TAdvWebBrowser.ExecuteJavaScript(AScript: String;
  ACompleteEvent: TAdvWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  inherited ExecuteJavaScript(AScript, ACompleteEvent, AImmediate);
end;

function TAdvWebBrowser.ExecuteJavaScriptSync(AScript: string): string;
begin
  Result := inherited ExecuteJavaScriptSync(AScript);
end;

function TAdvWebBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
begin
  Result := inherited GetBridgeCommunicationLayer(ABridgeName);
end;

{$IFDEF ANDROID}
function TAdvWebBrowser.NativeDialog: Pointer;
begin
  Result := inherited NativeDialog;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TAdvWebBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := inherited GetWebBrowserInstance;
end;
{$ENDIF}

procedure TAdvWebBrowser.GoBack;
begin
  inherited GoBack;
end;

procedure TAdvWebBrowser.GoForward;
begin
  inherited GoForward;
end;

procedure TAdvWebBrowser.Initialize;
begin
  inherited Initialize;
end;

function TAdvWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := inherited IsFMXBrowser;
end;

procedure TAdvWebBrowser.LoadFile(AFile: String);
begin
  inherited LoadFile(AFile);
end;

procedure TAdvWebBrowser.LoadHTML(AHTML: String);
begin
  inherited LoadHTML(AHTML);
end;

function TAdvWebBrowser.NativeBrowser: Pointer;
begin
  Result := inherited NativeBrowser;
end;

function TAdvWebBrowser.NativeEnvironment: Pointer;
begin
  Result := inherited NativeEnvironment;
end;

{$IFNDEF WEBLIB}
procedure TAdvWebBrowser.StartDocumentReadyStateThread;
begin
  inherited StartDocumentReadyStateThread;
end;
{$ENDIF}

procedure TAdvWebBrowser.Navigate;
begin
  inherited Navigate;
end;

procedure TAdvWebBrowser.Navigate(const AURL: string);
begin
  inherited Navigate(AURL);
end;

procedure TAdvWebBrowser.Reload;
begin
  inherited Reload;
end;

procedure TAdvWebBrowser.RemoveBridge(ABridgeName: string);
begin
  inherited RemoveBridge(ABridgeName);
end;

procedure TAdvWebBrowser.ShowDebugConsole;
begin
  inherited ShowDebugConsole;
end;

procedure TAdvWebBrowser.StopLoading;
begin
  inherited StopLoading;
end;

initialization
begin
  TAdvWebBrowserPlatformServices.FCurrentReleased := False;
  RegisterWebBrowserService;
end;

{$IFNDEF WEBLIB}
finalization
begin
  UnRegisterWebBrowserService;
{$IFNDEF AUTOREFCOUNT}
  TAdvWebBrowserPlatformServices.ReleaseCurrent;
{$ENDIF}
end;
{$ENDIF}

end.
