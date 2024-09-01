{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020 - 2023                               }
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

unit FMX.TMSFNCWebBrowser;

{$I FMX.TMSFNCDefines.inc}

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
  UITypes, FMX.Types, System.Messaging, FMX.Menus,
  {$ENDIF}
  {$IFDEF VCLLIB}
  VCL.Menus,
  {$ENDIF}
  {$IFDEF LCLLIB}
  Menus,
  {$ENDIF}
  Types, FMX.TMSFNCCustomControl, FMX.TMSFNCGraphics, FMX.TMSFNCUtils, FMX.TMSFNCTypes,
  Classes, TypInfo,
  {$IFNDEF LCLLIB}
  Generics.Collections,
  {$ELSE}
  fgl,
  {$ENDIF}
  {$IFDEF WEBLIB}
  Contnrs, WEBLib.Menus,
  {$ENDIF}
  FMX.StdCtrls, FMX.Forms, FMX.Controls
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
  {$IFDEF FNCLIB}
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 4; // Release nr.
  BLD_VER = 8; // Build nr.
  {$ENDIF}
  {$IFNDEF FNCLIB}
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.
  {$ENDIF}
  DESIGNTIMEMESSAGE = 'Selectable/Draggable area (only active at designtime)';

  //v1.0.0.0 : First release
  //v1.0.0.1 : Improved : Interfaces added to Edge Chromium to add JavaScript object injection
  //           (bridge to Delphi) and capture preview capabilities in TTMSFNCWebBrowser
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
  
  {$IFDEF FNCLIB}
  //1.1.4.8 : New : Methods added for TMSFNCEdgeWebBrowser
  {$ENDIF}
  {$IFNDEF FNCLIB}
  //1.2.0.0 : New : Print and save to PDF
  //        : New : Use popupmenu or manipulate context menu
  //        : New : Manage Cookies
  //        : New : Navigate with custom method and data
  {$ENDIF}
const
  {$IFDEF FNCLIB}
  IID_ITMSFNCCustomWebBrowserGUID = '{F74562D0-56C7-4ED2-B01B-8C9C16DD9361}';
  IID_ITMSFNCCustomWebBrowserExGUID = '{6B5D75C1-B5EC-463D-A602-1FFB97C8668C}';
  IID_ITMSFNCCustomWebBrowserContextMenuGUID = '{04BD0560-104B-4D3B-8CB1-45628D16CB0D}';
  IID_ITMSFNCCustomWebBrowserBridgeGUID = '{AC2934EC-9397-4A99-8E0A-1EF58803C8EA}';
  IID_ITMSFNCCustomWebBrowserSettingsGUID = '{25142510-A807-4635-BAE7-CB261D00137E}';
  IID_ITMSFNCCustomWebBrowserCookiesGUID = '{A50ABF08-0A6F-4877-AC92-FC834CF36AE6}';
  IID_ITMSFNCCustomWebBrowserServiceGUID = '{4B7A5FE1-A889-47C6-B40F-A611BB6266E6}';
  IID_ITMSFNCCustomWebBrowserPrintGUID = '{56EFC9E8-CD92-4FAC-B79C-084BF3DB0FBD}';
  {$ELSE}
  IID_ITMSFNCCustomWebBrowserGUID = '{8CE780C0-D22F-4063-993B-CFB0DD7D1351}';
  IID_ITMSFNCCustomWebBrowserExGUID = '{8620B994-4E90-4A43-82A6-62BD445E76F5}';
  IID_ITMSFNCCustomWebBrowserContextMenuGUID = '{45D26565-5CB6-4343-80F0-2E31991C13E1}';
  IID_ITMSFNCCustomWebBrowserBridgeGUID = '{E639C360-3FD0-4F63-9132-E8D0044EF860}';
  IID_ITMSFNCCustomWebBrowserSettingsGUID = '{86DF94BF-9310-4291-B22A-2E791A797E29}';
  IID_ITMSFNCCustomWebBrowserCookiesGUID = '{6B92ADFC-FD76-4708-B8AB-60904123449E}';
  IID_ITMSFNCCustomWebBrowserServiceGUID = '{94C7BF89-2B0A-40DF-8C24-B393CEB91389}';
  IID_ITMSFNCCustomWebBrowserPrintGUID = '{8C4A1B85-CC31-4A4D-9334-6D76DBAC569C}';
  {$ENDIF}
  IID_ITMSFNCCustomWebBrowser: TGUID = IID_ITMSFNCCustomWebBrowserGUID;
  IID_ITMSFNCCustomWebBrowserBridge: TGUID = IID_ITMSFNCCustomWebBrowserBridgeGUID;
  IID_ITMSFNCCustomWebBrowserSettings: TGUID = IID_ITMSFNCCustomWebBrowserSettingsGUID;
  IID_ITMSFNCCustomWebBrowserCookies: TGUID = IID_ITMSFNCCustomWebBrowserCookiesGUID;
  IID_ITMSFNCCustomWebBrowserService: TGUID = IID_ITMSFNCCustomWebBrowserServiceGUID;
  IID_ITMSFNCCustomWebBrowserEx: TGUID = IID_ITMSFNCCustomWebBrowserExGUID;
  IID_ITMSFNCCustomWebBrowserContextMenu: TGUID = IID_ITMSFNCCustomWebBrowserContextMenuGUID;
  IID_ITMSFNCCustomWebBrowserPrint: TGUID = IID_ITMSFNCCustomWebBrowserPrintGUID;

type
  TTMSFNCWebBrowserJavaScriptCompleteEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const AValue: string){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCWebBrowserContextMenuType = (mtPage, mtImage, mtSelectedText, mtAudio, mtVideo, (*not implemented in dll only for Delphi usage*)mtSubMenu);
  TTMSFNCWebBrowserContextMenuItemKind = (ikCommand, ikCheckBox, ikRadioButton, ikSeperator, ikSubMenu);
  TTMSFNCWebBrowserPrintOrientation = (poPortrait, poLandscape);


  TTMSFNCWebBrowserSystemContextMenuItem = class;
  TTMSFNCWebBrowserCustomContextMenuItem = class;

  TTMSFNCWebBrowserSameSiteType = (sstNone, sstLax, sstSameSite);   //Lax means same-site and cross-site

  TTMSFNCWebBrowserCookie = record
    Path: string;
    Name: string;
    Expires: TDateTime;
    Domain: string;
    Secure: Boolean;
    HTTPOnly: Boolean;
    Value: string;
    SameSite: TTMSFNCWebBrowserSameSiteType;
    Session: Boolean;
  end;

  TTMSFNCCustomWebBrowser = class;

  TTMSFNCWebBrowserTargetItem = record
    Kind: TTMSFNCWebBrowserContextMenuType;
    URI: string;
    SelectionText: string;
    LinkText: string;
  end;

  TTMSFNCWebBrowserContextMenuItem = class;

  TTMSFNCWebBrowserContextMenuItemList = class(TObjectList<TTMSFNCWebBrowserContextMenuItem>);

  TTMSFNCWebBrowserContextMenuItem = class(TPersistent)
  private
    FParentItem: TTMSFNCWebBrowserContextMenuItem;
    FName: string;
    FText: string;
    FChildren: TTMSFNCWebBrowserContextMenuItemList;
    FCommandId: Integer;
    FEnabled: Boolean;
    FShortcutKeyDescription: string;
    FKind: TTMSFNCWebBrowserContextMenuItemKind;
    FChecked: Boolean;
    FOriginalIndex: Integer;
    FInternalObject: TObject;
    FDataPointer: Pointer;
    FDataBoolean: Boolean;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FIcon: TTMSFNCBitmap;
    FEventHandlerObject: TObject;
    procedure SetShortcutKeyDescription(const Value: string);
  protected
    property Name: string read FName write FName;
    property Text: string read FText write FText;
    property Children: TTMSFNCWebBrowserContextMenuItemList read FChildren write FChildren;
    property OriginalIndex: Integer read FOriginalIndex write FOriginalIndex;
    property CommandId: Integer read FCommandId write FCommandId;
    property ShortcutKeyDescription: string read FShortcutKeyDescription write SetShortcutKeyDescription;
    property Icon: TTMSFNCBitmap read FIcon write FIcon;
    property Kind: TTMSFNCWebBrowserContextMenuItemKind read FKind write FKind;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Checked: Boolean read FChecked write FChecked;
    property InternalObject: TObject read FInternalObject write FInternalObject;
    property EventHandlerObject: TObject read FEventHandlerObject write FEventHandlerObject;
    property ParentItem: TTMSFNCWebBrowserContextMenuItem read FParentItem write FParentItem;
  public
    constructor Create;
    destructor Destroy; override;
    function AsCustom: TTMSFNCWebBrowserCustomContextMenuItem;
    function AsSystem: TTMSFNCWebBrowserSystemContextMenuItem;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
  end;

  TTMSFNCWebBrowserSystemContextMenuItem = class(TTMSFNCWebBrowserContextMenuItem)
  private
    function GetName: string;
    function GetText: string;
    function GetChildren: TObjectList<TTMSFNCWebBrowserContextMenuItem>;
    function GetKind: TTMSFNCWebBrowserContextMenuItemKind;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetIcon: TTMSFNCBitmap;
    function GetParentItem: TTMSFNCWebBrowserContextMenuItem;
  public
    property Name: string read GetName;
    property Text: string read GetText;
    property Children: TObjectList<TTMSFNCWebBrowserContextMenuItem> read GetChildren;
    property Kind: TTMSFNCWebBrowserContextMenuItemKind read GetKind;
    property Enabled: Boolean read GetEnabled;
    property Checked: Boolean read GetChecked;
    property Icon: TTMSFNCBitmap read GetIcon;
    property ParentItem: TTMSFNCWebBrowserContextMenuItem read GetParentItem;
  end;

  TTMSFNCWebBrowserCustomContextMenuItem = class(TTMSFNCWebBrowserContextMenuItem)
  private
    function GetParentItem: TTMSFNCWebBrowserContextMenuItem;
  public
    property Name;
    property Text;
    property Children;
    property Kind;
    property Enabled;
    property Checked;
    property Icon;
    property ParentItem: TTMSFNCWebBrowserContextMenuItem read GetParentItem;
  end;

  TTMSFNCWebBrowserPrintSettings = record
    Orientation: TTMSFNCWebBrowserPrintOrientation;
    ScaleFactor: Double;
    PageWidth: Integer;
    PageHeight: Integer;
    MarginLeft: Integer;
    MarginTop: Integer;
    MarginRight: Integer;
    MarginBottom: Integer;
    PrintBackgrounds: Boolean;
    PrintSelectionOnly: Boolean;
    PrintHeaderAndFooter: Boolean;
    HeaderTitle: string;
    FooterURI: string;
  end;

  TTMSFNCWebBrowserOnGetContextMenu = procedure(Sender: TObject; ATarget: TTMSFNCWebBrowserTargetItem; AContextMenu: TObjectList<TTMSFNCWebBrowserContextMenuItem>) of object;
  TTMSFNCWebBrowserOnGetPopupMenuForContextMenu = procedure(Sender: TObject; ATarget: TTMSFNCWebBrowserTargetItem; var APopupMenu: TPopupMenu) of object;
  TTMSFNCWebBrowserOnCustomContextMenuItemSelected = procedure(Sender: TObject; ASelectedItem: TTMSFNCWebBrowserCustomContextMenuItem) of object;
  TTMSFNCWebBrowserOnGetCookies = procedure(Sender: TObject; ACookies: array of TTMSFNCWebBrowserCookie) of object;
  TTMSFNCWebBrowserOnGetPrintPDFStream = procedure(Sender: TObject; AStream: TMemoryStream) of object;

  ITMSFNCCustomWebBrowserBridge = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserBridgeGUID]
  {$ENDIF}
    function GetObjectMessage: string;
    procedure SetObjectMessage(const AValue: string);
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

  ITMSFNCCustomWebBrowser = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserGUID]
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
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
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

  ITMSFNCCustomWebBrowserEx = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserExGUID]
  {$ENDIF}
    procedure OpenTaskManagerWindow;
    procedure NavigateWithData(AURI: string; AMethod: string; ABody: string; AHeaders: TStrings = nil); overload;
    procedure NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil); overload;
  end;

  ITMSFNCCustomWebBrowserSettings = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserSettingsGUID]
  {$ENDIF}
    function GetEnableScript: Boolean;
    procedure SetEnableScript(const Value: Boolean);
    property EnableScript: boolean read GetEnableScript write SetEnableScript;
  end;

  ITMSFNCCustomWebBrowserCookies = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserCookiesGUID]
  {$ENDIF}
    procedure GetCookies(AURI: string);
    procedure AddCookie(ACookie: TTMSFNCWebBrowserCookie);
    procedure DeleteAllCookies;
    procedure DeleteCookie(AName: string; ADomain: string; APath: string);
  end;

  ITMSFNCCustomWebBrowserContextMenu = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserContextMenuGUID]
  {$ENDIF}
  end;

  ITMSFNCCustomWebBrowserPrint = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserPrintGUID]
  {$ENDIF}
    procedure ShowPrintUI;
    procedure Print; overload;
    procedure Print(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure PrintToPDFStream; overload;
    procedure PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure PrintToPDF(AFileName: string); overload;
  end;

  ITMSFNCWebBrowserService = interface(IInterface)
  {$IFNDEF WEBLIB}
  [IID_ITMSFNCCustomWebBrowserServiceGUID]
  {$ENDIF}
    function CreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
    procedure DeleteCookies;
    procedure DestroyWebBrowser(const AValue: ITMSFNCCustomWebBrowser);
  end;

  TTMSFNCCustomWebBrowserNavigateCompleteParams = record
    URL: String;
    Success: Boolean;
    ErrorCode: Integer;
  end;

  TTMSFNCCustomWebBrowserBeforeNavigateParams = record
    URL: String;
    Cancel: Boolean;
  end;

  TTMSFNCCustomWebBrowserNavigateComplete = procedure(Sender: TObject; var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams) of object;
  TTMSFNCCustomWebBrowserBeforeNavigate = procedure(Sender: TObject; var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams) of object;
  TTMSFNCCustomWebBrowserCaptureScreenShot = procedure(Sender: TObject; AScreenShot: TTMSFNCBitmap) of object;
  TTMSFNCCustomWebBrowserOnExecuteSuccessful = procedure(Sender: TObject; ASuccessful: Boolean) of object;

  {$IFNDEF WEBLIB}
  TTMSFNCWebBrowserDocumentReadyStateThread = class(TThread)
  private
    FWebBrowser: TTMSFNCCustomWebBrowser;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebBrowser: TTMSFNCCustomWebBrowser);
  end;
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TTMSFNCScript = class
  private
    FScript: string;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
  public
    constructor Create(AScript: string; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent);
  end;

  TTMSFNCScriptList = class(TObjectList<TTMSFNCScript>);
  {$ENDIF}

  TTMSFNCCustomWebBrowserSettings = class(TPersistent)
  private
    FOwner: TTMSFNCCustomWebBrowser;
    FEnableScript: Boolean;
    FEnableAcceleratorKeys: Boolean;
    FEnableContextMenu: Boolean;
    FEnableShowDebugConsole: Boolean;
    FUsePopupMenuAsContextMenu: Boolean;
    function GetEnableAcceleratorKeys: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableScript: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    procedure SetEnableAcceleratorKeys(const Value: Boolean);
    procedure SetEnableContextMenu(const Value: Boolean);
    procedure SetEnableShowDebugConsole(const Value: Boolean);
    procedure SetEnableScript(const Value: boolean);
    function GetUsePopupMenuAsContextMenu: Boolean;
    procedure SetUsePopupMenuAsContextMenu(const Value: Boolean);
  protected
    procedure ApplySettings;
    property EnableScript: Boolean read GetEnableScript write SetEnableScript;
    property EnableContextMenu: Boolean read GetEnableContextMenu write SetEnableContextMenu;
    property EnableShowDebugConsole: Boolean read GetEnableShowDebugConsole write SetEnableShowDebugConsole;
    property EnableAcceleratorKeys: Boolean read GetEnableAcceleratorKeys write SetEnableAcceleratorKeys;
    property UsePopupMenuAsContextMenu: Boolean read GetUsePopupMenuAsContextMenu write SetUsePopupMenuAsContextMenu;
  public
    constructor Create(AOwner: TTMSFNCCustomWebBrowser);
    destructor Destroy; override;
  end;

  TTMSFNCWebBrowserSettings = class(TTMSFNCCustomWebBrowserSettings)
  published
//    property EnableScript;
    property EnableContextMenu;
    property EnableShowDebugConsole;
    property EnableAcceleratorKeys;
    property UsePopupMenuAsContextMenu;
  end;

  TTMSFNCCustomWebBrowser = class(TTMSFNCCustomControl)
  private
    FDesigntimeEnabled: Boolean;
    FCanDestroyDispatch: Boolean;
    FSyncValue: string;
    FSyncValueExecuted: Boolean;
    FThreadDone: Boolean;
    FReady: Boolean;
    FInitialized: Boolean;
    {$IFNDEF WEBLIB}
    FScriptList: TTMSFNCScriptList;
    FDocumentReadyStateThread: TTMSFNCWebBrowserDocumentReadyStateThread;
    {$ENDIF}
    FWebBrowser: ITMSFNCCustomWebBrowser;
    FURL: string;
    FExternalBrowser, FEnableShowDebugConsole, FEnableAcceleratorKeys, FEnableContextMenu: Boolean;
    FOnNavigateComplete: TTMSFNCCustomWebBrowserNavigateComplete;
    FOnBeforeNavigate: TTMSFNCCustomWebBrowserBeforeNavigate;
    FOnHardwareButtonClicked: TNotifyEvent;
    FOnInitialized: TNotifyEvent;
    FOnCloseForm: TNotifyEvent;
    FOnDocumentComplete: TNotifyEvent;
    FOnCaptureScreenShot: TTMSFNCCustomWebBrowserCaptureScreenShot;
    FSettings: TTMSFNCWebBrowserSettings;
    FOnGetContextMenu: TTMSFNCWebBrowserOnGetContextMenu;
    FOnGetCookies: TTMSFNCWebBrowserOnGetCookies;
    FOnGetPrintPDFStream: TTMSFNCWebBrowserOnGetPrintPDFStream;
    FOnPrintedToPDF: TTMSFNCCustomWebBrowserOnExecuteSuccessful;
    FOnPrinted: TTMSFNCCustomWebBrowserOnExecuteSuccessful;
    FOnCustomContextMenuItemSelected: TTMSFNCWebBrowserOnCustomContextMenuItemSelected;
    FOnGetPopupMenuForContextMenu: TTMSFNCWebBrowserOnGetPopupMenuForContextMenu;
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
    procedure SetSettings(const Value: TTMSFNCWebBrowserSettings);
    function GetSettingsI: ITMSFNCCustomWebBrowserSettings;
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
    procedure DoCaptureScreenShot(AScreenShot: TTMSFNCBitmap); virtual;
    procedure BeforeNavigate(var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams); virtual;
    procedure NavigateComplete(var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams); virtual;
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
    procedure Draw({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    procedure Loaded; override;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
    property ExternalBrowser: Boolean read GetExternalBrowser write SetExternalBrowser default False;
    procedure Navigate; overload; virtual;
    procedure Navigate(const AURL: string); overload; virtual;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); virtual;
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
    property SettingsI: ITMSFNCCustomWebBrowserSettings read GetSettingsI;
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
    property OnBeforeNavigate: TTMSFNCCustomWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TTMSFNCCustomWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnHardwareButtonClicked: TNotifyEvent read FOnHardwareButtonClicked write FOnHardwareButtonClicked;
    property OnDocumentComplete: TNotifyEvent read FOnDocumentComplete write FOnDocumentComplete;
    property OnCaptureScreenShot: TTMSFNCCustomWebBrowserCaptureScreenShot read FOnCaptureScreenShot write FOnCaptureScreenShot;
    property EnableContextMenu: Boolean read GetEnableContextMenu write SetEnableContextMenu default True;
    property EnableShowDebugConsole: Boolean read GetEnableShowDebugConsole write SetEnableShowDebugConsole default True;
    property EnableAcceleratorKeys: Boolean read GetEnableAcceleratorKeys write SetEnabledAcceleratorKeys default True;
    property CacheFolder: string read GetCacheFolder write SetCacheFolder;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property CanDestroyDispatch: Boolean read FCanDestroyDispatch write FCanDestroyDispatch;
    property DesigntimeEnabled: Boolean read FDesigntimeEnabled write SetDesigntimeEnabled default True;
    property Settings: TTMSFNCWebBrowserSettings read FSettings write SetSettings;
    property OnGetContextMenu: TTMSFNCWebBrowserOnGetContextMenu read FOnGetContextMenu write FOnGetContextMenu;
    property OnGetPopupMenuForContextMenu: TTMSFNCWebBrowserOnGetPopupMenuForContextMenu read FOnGetPopupMenuForContextMenu write FOnGetPopupMenuForContextMenu;
    property OnCustomContextMenuItemSelected: TTMSFNCWebBrowserOnCustomContextMenuItemSelected read FOnCustomContextMenuItemSelected write FOnCustomContextMenuItemSelected;
    procedure DoGetContextMenuItemEvent(ATarget: TTMSFNCWebBrowserTargetItem; AContextMenu: TObjectList<TTMSFNCWebBrowserContextMenuItem>);
    procedure DoGetPopupMenuForContextMenu(ATarget: TTMSFNCWebBrowserTargetItem; var APopUpMenu: TPopupMenu);
    procedure DoCustomContextMenuItemSelected(ASelectedItem: TTMSFNCWebBrowserCustomContextMenuItem); virtual;
    function InitialPrintSettings: TTMSFNCWebBrowserPrintSettings; virtual;
    procedure DoGetPrintPDFStream(AStream: TMemoryStream); virtual;
    property OnGetPrintPDFStream: TTMSFNCWebBrowserOnGetPrintPDFStream read FOnGetPrintPDFStream write FOnGetPrintPDFStream;
    property OnGetCookies: TTMSFNCWebBrowserOnGetCookies read FOnGetCookies write FOnGetCookies;
    procedure DoGetCookies(ACookies: array of TTMSFNCWebBrowserCookie); virtual;
    procedure DoPrintedToPDF(ASuccesfull: Boolean); virtual;
    procedure DoPrinted(APrinterStatus: Boolean); virtual;
    property OnPrintedToPDF: TTMSFNCCustomWebBrowserOnExecuteSuccessful read FOnPrintedToPDF write FOnPrintedToPDF;
    property OnPrinted: TTMSFNCCustomWebBrowserOnExecuteSuccessful read FOnPrinted write FOnPrinted;
    procedure OpenTaskManager; virtual;
    procedure GetCookies(AURI: string = ''); virtual;
    procedure AddCookie(ACookie: TTMSFNCWebBrowserCookie); virtual;
    procedure DeleteAllCookies; virtual;
    procedure DeleteCookie(AName: string; ADomain: string; APath: string); virtual;
    procedure ShowPrintUI; virtual;
    procedure Print(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; virtual;
    procedure Print; overload; virtual;
    procedure PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; virtual;
    procedure PrintToPDFStream; overload; virtual;
    procedure PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; virtual;
    procedure PrintToPDF(AFileName: string); overload; virtual;
    procedure NavigateWithData(AURI: string; AMethod: string; ABody: string; AHeaders: TStrings = nil); overload; virtual;
    procedure NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil); overload; virtual;
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
  TTMSFNCWebBrowserList = class(TList)
  private
    function GetItem(Index: Integer): ITMSFNCCustomWebBrowser;
    procedure SetItem(Index: Integer; const Value: ITMSFNCCustomWebBrowser);
  public
    property Items[Index: Integer]: ITMSFNCCustomWebBrowser read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCWebBrowserList = class(TList<ITMSFNCCustomWebBrowser>);
  {$ENDIF}

  TTMSFNCWebBrowserFactoryService = class(TInterfacedObject, ITMSFNCWebBrowserService)
  protected
    FWebBrowsers: TTMSFNCWebBrowserList;
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; virtual; abstract;
    procedure DoRemoveWebBrowser(const AValue: ITMSFNCCustomWebBrowser);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
    procedure DeleteCookies; virtual; abstract;
    procedure DestroyWebBrowser(const AValue: ITMSFNCCustomWebBrowser);
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWebBrowser = class(TTMSFNCCustomWebBrowser)
  public
    {$IFNDEF WEBLIB}
    procedure StartDocumentReadyStateThread; override;
    {$ENDIF}
    procedure Navigate; overload; override;
    procedure Navigate(const AURL: string); overload; override;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); override;
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
    {$IFNDEF FNCLIB}
    procedure OpenTaskManager; override;
    procedure GetCookies(AURI: string = ''); override;
    procedure AddCookie(ACookie: TTMSFNCWebBrowserCookie); override;
    procedure DeleteAllCookies; override;
    procedure DeleteCookie(AName: string; ADomain: string; APath: string); override;
    function InitialPrintSettings: TTMSFNCWebBrowserPrintSettings; override;
    procedure ShowPrintUI; override;
    procedure Print(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure Print; overload; override;
    procedure PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure PrintToPDFStream; overload; override;
    procedure PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure PrintToPDF(AFileName: string); overload; override;
    procedure NavigateWithData(AURI: string; AMethod: string; ABody: string; AHeaders: TStrings = nil); overload; override;
    procedure NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil); overload; override;
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
    {$IFNDEF FNCLIB}
    property Settings;
    property OnGetContextMenu;
    property OnGetCookies;
    property OnPrintedToPDF;
    property OnGetPrintPDFStream;
    property OnPrinted;
    property OnCustomContextMenuItemSelected;
    property OnGetPopupMenuForContextMenu;
    {$ENDIF}
  end;

  TTMSFNCWebBrowserPopup = class;

  {$IFDEF FMXMOBILE}
  TTMSFNCWebBrowserPopupForm = class(TCommonCustomForm)
  {$ELSE}
  TTMSFNCWebBrowserPopupForm = class(TCustomForm)
  {$ENDIF}
  private
    FWebBrowserPopup: TTMSFNCWebBrowserPopup;
  protected
    procedure UpdateBackGround;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  {$IFDEF IOS}
  ITMSFNCWebBrowserPopupButtonEventHandler = interface(NSObject)
  ['{723E9874-7EEF-4E40-896D-9E2DAC8E6DD4}']
    procedure Click; cdecl;
  end;

  TTMSFNCWebBrowserPopupButtonEventHandler = class(TOCLocal)
  private
    FWebBrowserPopup: TTMSFNCWebBrowserPopup;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure Click; cdecl;
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  TTMSFNCWebBrowserPopupButtonEventHandler = class(TJavaLocal, JView_OnClickListener)
  private
    FWebBrowserPopup: TTMSFNCWebBrowserPopup;
  public
    procedure onClick(P1: JView); cdecl;
  end;
  {$ENDIF}

  TTMSFNCCustomWebBrowserClass = class of TTMSFNCCustomWebBrowser;

  {$IFDEF FMXLIB}
  TTMSFNCWebBrowserFormPosition = TFormPosition;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TTMSFNCWebBrowserFormPosition = TPosition;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWebBrowserPopup = class(TComponent)
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
    FWebBrowserForm: TTMSFNCWebBrowserPopupForm;
    {$ENDIF}
    {$IFDEF ANDROID}
    FButton: JButton;
    {$ENDIF}
    {$IFDEF FMXMOBILE}
    FButtonEventHandler: TTMSFNCWebBrowserPopupButtonEventHandler;
    {$ENDIF}
    FWebBrowser: TTMSFNCCustomWebBrowser;
    FOnNavigateComplete: TTMSFNCCustomWebBrowserNavigateComplete;
    FOnBeforeNavigate: TTMSFNCCustomWebBrowserBeforeNavigate;
    FURL: String;
    FPosition: TTMSFNCWebBrowserFormPosition;
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
    procedure DoBeforeNavigate(Sender: TObject; var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams); virtual;
    procedure DoNavigateComplete(Sender: TObject; var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams); virtual;
    procedure InitializeWebBrowser(AWebBrowser: TTMSFNCCustomWebBrowser); virtual;
    function GetWebBrowserClass: TTMSFNCCustomWebBrowserClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(AModal: Boolean = True): TModalResult; overload;
    function Open(AURL: String; AModal: Boolean = True): TModalResult; overload;
    procedure Close(AModalResult: TModalResult = mrOk);
    property WebBrowser: TTMSFNCCustomWebBrowser read FWebBrowser;
    property ExternalBrowser: Boolean read FExternalBrowser write FExternalBrowser default False;
  published
    property OnBeforeNavigate: TTMSFNCCustomWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TTMSFNCCustomWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property URL: String read FURL write FURL;
    {$IFDEF FMXLIB}
    property Position: TTMSFNCWebBrowserFormPosition read FPosition write FPosition default TFormPosition.ScreenCenter;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    property Position: TTMSFNCWebBrowserFormPosition read FPosition write FPosition default poScreenCenter;
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

  TTMSFNCWebBrowserPlatformServicesService = class
  private
    FInterface: IInterface;
    FGUID: string;
  public
    constructor Create(AGUID: string; AInterface: IInterface);
    property GUID: string read FGUID;
    property &Interface: IInterface read FInterface;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCWebBrowserPlatformServicesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCWebBrowserPlatformServicesService;
    procedure SetItem(Index: Integer; const Value: TTMSFNCWebBrowserPlatformServicesService);
  public
    property Items[Index: Integer]: TTMSFNCWebBrowserPlatformServicesService read GetItem write SetItem; default;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCWebBrowserPlatformServicesList = class(TObjectList<TTMSFNCWebBrowserPlatformServicesService>)
  {$ENDIF}
  private
    function GetValue(AGUID: string): IInterface;
  public
    function ContainsKey(AGUID: string): Boolean;
    procedure RemoveByGUID(AGUID: string);
    property Interfaces[AGUID: string]: IInterface read GetValue;
  end;

  TTMSFNCWebBrowserPlatformServices = class
  private
    FServicesList: TTMSFNCWebBrowserPlatformServicesList;
    class var FCurrent: TTMSFNCWebBrowserPlatformServices;
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
    class function Current: TTMSFNCWebBrowserPlatformServices;
  end;

{$IFDEF MACOS}
function NSStrEx(AString: string): NSString;
{$ENDIF}

implementation

uses
  SysUtils, FMX.TMSFNCGraphicsTypes
{$IFDEF FMXLIB}
  ,FMX.Platform
{$ENDIF}
{$IFDEF WEBLIB}
  ,FMX.TMSFNCWebBrowser.WEB
{$ENDIF}
{$IFDEF MSWINDOWS}
  ,FMX.TMSFNCWebBrowser.Win
{$ENDIF}
{$IFDEF UNIX}
  ,FMX.TMSFNCWebBrowser.Unix
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  ,FMX.TMSFNCWebBrowser.iOS
{$ELSE}
  ,FMX.TMSFNCWebBrowser.Mac
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  ,FMX.TMSFNCWebBrowser.Android
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

{ TTMSFNCCustomWebBrowser }

{$IFDEF FMXLIB}
procedure TTMSFNCCustomWebBrowser.Show;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

procedure TTMSFNCCustomWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
begin
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.RemoveBridge(ABridgeName);
    FWebBrowser.AddBridge(ABridgeName, ABridgeObject);
  end;
end;

procedure TTMSFNCCustomWebBrowser.AddCookie(ACookie: TTMSFNCWebBrowserCookie);
var
  ic: ITMSFNCCustomWebBrowserCookies;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserCookies, ic) = S_OK then
      ic.AddCookie(ACookie);
  end;
end;

procedure TTMSFNCCustomWebBrowser.BeforeNavigate(
  var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams);
begin
  if Assigned(OnBeforeNavigate) then
    OnBeforeNavigate(Self, Params);
end;

function TTMSFNCCustomWebBrowser.CanBeVisible: Boolean;
begin
  Result := True;
end;

function TTMSFNCCustomWebBrowser.CheckIdentifier: Boolean;
begin
  Result := False;
end;

function TTMSFNCCustomWebBrowser.CanCreateBrowser: Boolean;
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

function TTMSFNCCustomWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CanGoBack;
end;

function TTMSFNCCustomWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CanGoForward;
end;

procedure TTMSFNCCustomWebBrowser.CaptureScreenShot;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CaptureScreenShot;
end;

procedure TTMSFNCCustomWebBrowser.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TTMSFNCCustomWebBrowser.CheckApplicationInitialized;
begin
  ExecuteJavaScript('document.readyState', {$IFDEF LCLWEBLIB}@{$ENDIF}DoCheckReadyState);
end;

procedure TTMSFNCCustomWebBrowser.ClearCache;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.ClearCache;
end;

constructor TTMSFNCCustomWebBrowser.Create(AOwner: TComponent);
var
  WebBrowserService: ITMSFNCWebBrowserService;
begin
  inherited;
  FSettings := TTMSFNCWebBrowserSettings.Create(Self);
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
  FScriptList := TTMSFNCScriptList.Create;
  {$ENDIF}
  FExternalBrowser := False;
  FEnableShowDebugConsole := True;
  FEnableContextMenu := True;
  FEnableAcceleratorKeys := True;
  if CanCreateBrowser then
    if TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
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
procedure TTMSFNCCustomWebBrowser.UpdateElement;
begin
  inherited;
  if Assigned(ElementHandle) then
    ElementHandle.style.setProperty('overflow', 'auto');
end;
{$ENDIF}

procedure TTMSFNCCustomWebBrowser.CreateClasses;
begin
//
end;

destructor TTMSFNCCustomWebBrowser.Destroy;
var
  WebBrowserService: ITMSFNCWebBrowserService;
begin
  {$IFNDEF WEBLIB}
  StopDocumentReadyStateThread;
  {$ENDIF}

  if CanCreateBrowser and TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
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

  FreeAndNil(FSettings);
  inherited;
end;

procedure TTMSFNCCustomWebBrowser.DoCaptureScreenShot(AScreenShot: TTMSFNCBitmap);
begin
  if Assigned(OnCaptureScreenShot) then
    OnCaptureScreenShot(Self, AScreenShot);
end;

procedure TTMSFNCCustomWebBrowser.DoCheckIdentifier(const AValue: string);
begin
  if LowerCase(StringReplace(AValue, '"', '', [rfReplaceAll])) = 'unknown' then
    FReady := True;
end;

procedure TTMSFNCCustomWebBrowser.DoDocumentComplete;
begin
  if Assigned(OnDocumentComplete) then
    OnDocumentComplete(Self);
end;

procedure TTMSFNCCustomWebBrowser.DoCheckReadyState(const AValue: string);
begin
  if LowerCase(StringReplace(AValue, '"', '', [rfReplaceAll]))  = 'complete' then
  begin
    if CheckIdentifier then
      ExecuteJavaScript('window.TMSWEBCoreClientIdentifier', {$IFDEF LCLWEBLIB}@{$ENDIF}DoCheckIdentifier)
    else
      FReady := True;
  end;
end;

procedure TTMSFNCCustomWebBrowser.DoCloseForm;
begin
  if Assigned(OnCloseForm) then
    OnCloseForm(Self);
end;

procedure TTMSFNCCustomWebBrowser.DoCustomContextMenuItemSelected(ASelectedItem: TTMSFNCWebBrowserCustomContextMenuItem);
begin
  if Assigned(OnCustomContextMenuItemSelected) then
    OnCustomContextMenuItemSelected(Self, ASelectedItem);
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCCustomWebBrowser.DoTerminate(Sender: TObject);
begin
  FThreadDone := True;
end;
{$ENDIF}

procedure TTMSFNCCustomWebBrowser.DoHardwareButtonClicked;
begin
  if Assigned(OnHardwareButtonClicked) then
    OnHardwareButtonClicked(Self);
end;

procedure TTMSFNCCustomWebBrowser.DoKeyPressed(var Key: Word);
begin

end;

procedure TTMSFNCCustomWebBrowser.DoPrinted(APrinterStatus: Boolean);
begin
  if Assigned(OnPrinted) then
    OnPrinted(Self, APrinterStatus);
end;

procedure TTMSFNCCustomWebBrowser.DoPrintedToPDF(ASuccesfull: Boolean);
begin
  if Assigned(OnPrintedToPDF) then
    OnPrintedToPDF(Self, ASuccesfull);
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCCustomWebBrowser.DoScriptHandled(Sender: TObject);
var
  s: TTMSFNCScript;
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

procedure TTMSFNCCustomWebBrowser.Draw(AGraphics: TTMSFNCGraphics; ARect: TRectF);
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

procedure TTMSFNCCustomWebBrowser.DoEnter;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.SetFocus;
end;

procedure TTMSFNCCustomWebBrowser.DoExecuteJavaScriptSync(const AValue: string);
begin
  FSyncValue := AValue;
  FSyncValueExecuted := True;
end;

procedure TTMSFNCCustomWebBrowser.DoGetContextMenuItemEvent(ATarget: TTMSFNCWebBrowserTargetItem; AContextMenu: TObjectList<TTMSFNCWebBrowserContextMenuItem>);
begin
  if Assigned(FOnGetContextMenu) then
    OnGetContextMenu(Self, ATarget, AContextMenu);
end;

procedure TTMSFNCCustomWebBrowser.DoGetCookies(ACookies: array of TTMSFNCWebBrowserCookie);
begin
  if Assigned(OnGetCookies) then
    OnGetCookies(Self, ACookies);
end;

procedure TTMSFNCCustomWebBrowser.DoGetPopupMenuForContextMenu(ATarget: TTMSFNCWebBrowserTargetItem; var APopUpMenu: TPopupMenu);
begin
  if Assigned(OnGetPopupMenuForContextMenu) then
    OnGetPopupMenuForContextMenu(Self, ATarget, APopupMenu);
end;

procedure TTMSFNCCustomWebBrowser.DoGetPrintPDFStream(AStream: TMemoryStream);
begin
  if Assigned(OnGetPrintPDFStream) then
    OnGetPrintPDFStream(Self, AStream);
end;

function TTMSFNCCustomWebBrowser.ExecuteJavaScriptSync(AScript: string): string;
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

procedure TTMSFNCCustomWebBrowser.ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  if Assigned(FWebBrowser) then
  begin
    if AImmediate and FInitialized then
      FWebBrowser.ExecuteJavaScript(AScript, ACompleteEvent, nil)
    else
    begin
      {$IFNDEF WEBLIB}
      FScriptList.Add(TTMSFNCScript.Create(AScript, ACompleteEvent));
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

function TTMSFNCCustomWebBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
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

function TTMSFNCCustomWebBrowser.GetCacheFolder: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CacheFolder;
end;

function TTMSFNCCustomWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.CacheFolderName;
end;

procedure TTMSFNCCustomWebBrowser.GetCookies(AURI: string = '');
var
  ic: ITMSFNCCustomWebBrowserCookies;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserCookies, ic) = S_OK then
      ic.GetCookies(AURI);
  end;
end;

function TTMSFNCCustomWebBrowser.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfnccore/components/ttmsfncwebbrowser';
end;

function TTMSFNCCustomWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.AutoClearCache;
end;

function TTMSFNCCustomWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableAcceleratorKeys
  else
    Result := FEnableAcceleratorKeys;
end;

function TTMSFNCCustomWebBrowser.GetEnableContextMenu: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableContextMenu
  else
    Result := FEnableContextMenu;
end;

function TTMSFNCCustomWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.EnableShowDebugConsole
  else
    Result := FEnableShowDebugConsole;
end;

function TTMSFNCCustomWebBrowser.GetExternalBrowser: Boolean;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.ExternalBrowser
  else
    Result := FExternalBrowser;
end;

function TTMSFNCCustomWebBrowser.GetSettingsI: ITMSFNCCustomWebBrowserSettings;
var
  s: ITMSFNCCustomWebBrowserSettings;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserSettings, s) = S_OK then
      Result := s;
  end;
end;

function TTMSFNCCustomWebBrowser.GetURL: string;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.URL
  else
    Result := FURL;
end;

function TTMSFNCCustomWebBrowser.GetUserAgent: string;
begin
  Result := '';
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.UserAgent;
end;

procedure TTMSFNCCustomWebBrowser.GoBack;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.GoBack;
end;

function TTMSFNCCustomWebBrowser.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCCustomWebBrowser.GoForward;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.GoForward;
end;

{$IFDEF VCLLIB}
procedure TTMSFNCCustomWebBrowser.CreateWnd;
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
procedure TTMSFNCCustomWebBrowser.Hide;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TTMSFNCCustomWebBrowser.DoAbsoluteChanged;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

procedure TTMSFNCCustomWebBrowser.Initialize;
begin
  if (IsDesigning and FDesigntimeEnabled) or not IsDesigning then
  begin
    if Assigned(FWebBrowser) then
      FWebBrowser.Initialize;
  end;
end;

procedure TTMSFNCCustomWebBrowser.Initialized;
begin
  FInitialized := True;

  if Assigned(Settings) then
    Settings.ApplySettings;

  if Assigned(OnInitialized) then
    OnInitialized(Self);
end;

function TTMSFNCCustomWebBrowser.InitialPrintSettings: TTMSFNCWebBrowserPrintSettings;
begin
  Result.Orientation := poPortrait;
  Result.ScaleFactor := 0;
  Result.PageWidth := 0;
  Result.PageHeight := 0;
  Result.MarginLeft := 0;
  Result.MarginRight := 0;
  Result.MarginTop := 0;
  Result.MarginBottom := 0;
  Result.PrintBackgrounds := False;
  Result.PrintSelectionOnly := False;
  Result.PrintHeaderAndFooter := False;
  Result.HeaderTitle := '';
  Result.FooterURI := '';
end;

function TTMSFNCCustomWebBrowser.CanLoadDefaultHTML: Boolean;
begin
  Result := True;
end;

function TTMSFNCCustomWebBrowser.CanRecreate: Boolean;
begin
  Result := True;
end;

function TTMSFNCCustomWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.IsFMXBrowser;
end;

procedure TTMSFNCCustomWebBrowser.Loaded;
begin
  inherited;
  Initialize;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TTMSFNCCustomWebBrowser.LoadFile(AFile: String);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.LoadFile(AFile);
end;

procedure TTMSFNCCustomWebBrowser.LoadHTML(AHTML: String);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.LoadHTML(AHTML);
end;

{$IFDEF FMXLIB}
procedure TTMSFNCCustomWebBrowser.Move;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;
{$ENDIF}

{$IFDEF ANDROID}
function TTMSFNCCustomWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeDialog;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TTMSFNCCustomWebBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.GetBrowserInstance;
end;
{$ENDIF}

function TTMSFNCCustomWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeBrowser;
end;

function TTMSFNCCustomWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.NativeEnvironment;
end;

procedure TTMSFNCCustomWebBrowser.Navigate(const AURL: string);
begin
  FReady := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.Navigate(AURL);
end;

procedure TTMSFNCCustomWebBrowser.NavigateComplete(
  var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
begin
  if Assigned(OnNavigateComplete) then
    OnNavigateComplete(Self, Params);
end;

procedure TTMSFNCCustomWebBrowser.NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil);
var
  iX: ITMSFNCCustomWebBrowserEx;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserEx, iX) = S_OK then
      iX.NavigateWithData(AURI, AMethod, ABodyStream, AHeaders);
  end;
end;

procedure TTMSFNCCustomWebBrowser.NavigateWithData(AURI, AMethod, ABody: string; AHeaders: TStrings);
var
  iX: ITMSFNCCustomWebBrowserEx;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserEx, iX) = S_OK then
      iX.NavigateWithData(AURI, AMethod, ABody, AHeaders);
  end;
end;

procedure TTMSFNCCustomWebBrowser.OpenTaskManager;
var
  iX: ITMSFNCCustomWebBrowserEx;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserEx, iX) = S_OK then
      iX.OpenTaskManagerWindow;
  end;
end;

procedure TTMSFNCCustomWebBrowser.Print(APrintSettings: TTMSFNCWebBrowserPrintSettings);
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.Print(APrintSettings);
  end;
end;

procedure TTMSFNCCustomWebBrowser.Print;
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.Print;
  end;
end;

procedure TTMSFNCCustomWebBrowser.PrintToPDF(AFileName: string);
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.PrintToPDF(AFileName);
  end;
end;

procedure TTMSFNCCustomWebBrowser.PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings);
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.PrintToPDF(AFileName, APrintSettings);
  end;
end;

procedure TTMSFNCCustomWebBrowser.PrintToPDFStream;
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.PrintToPDFStream;
  end;
end;

procedure TTMSFNCCustomWebBrowser.PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings);
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
      ip.PrintToPDFStream(APrintSettings);
  end;
end;

procedure TTMSFNCCustomWebBrowser.Reload;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.Reload;
end;

procedure TTMSFNCCustomWebBrowser.RemoveBridge(ABridgeName: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.RemoveBridge(ABridgeName);
end;

procedure TTMSFNCCustomWebBrowser.UpdateControlAfterResize;
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TTMSFNCCustomWebBrowser.Navigate;
begin
  FReady := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.Navigate;
end;

procedure TTMSFNCCustomWebBrowser.SetCacheFolder(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CacheFolder := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetCacheFolderName(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.CacheFolderName := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetDesigntimeEnabled(const Value: Boolean);
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

procedure TTMSFNCCustomWebBrowser.SetAutoClearCache(const Value: Boolean);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.AutoClearCache := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetEnableContextMenu(const Value: Boolean);
begin
  FEnableContextMenu := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableContextMenu := Value;
end;

{$IFDEF FMXLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 32}
procedure TTMSFNCCustomWebBrowser.FormHandleCreated(const Sender: TObject; const Msg: TMessage);

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

procedure TTMSFNCCustomWebBrowser.SetEnabledAcceleratorKeys(const Value: Boolean);
begin
  FEnableAcceleratorKeys := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableAcceleratorKeys := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetEnableShowDebugConsole(const Value: Boolean);
begin
  FEnableShowDebugConsole := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.EnableShowDebugConsole := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetExternalBrowser(const Value: Boolean);
begin
  FExternalBrowser := Value;
  if Assigned(FWebBrowser) then
    FWebBrowser.ExternalBrowser := Value;
end;

procedure TTMSFNCCustomWebBrowser.SetSettings(const Value: TTMSFNCWebBrowserSettings);
begin
  FSettings.Assign(Value);
end;

{$IFDEF CMNWEBLIB}
procedure TTMSFNCCustomWebBrowser.SetEnabled(Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateEnabled;
end;

{$IFDEF CMNLIB}
procedure TTMSFNCCustomWebBrowser.SetParent(Value: TWinControl);
{$ELSE}
procedure TTMSFNCCustomWebBrowser.SetParent(Value: TControl);
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
procedure TTMSFNCCustomWebBrowser.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateBounds;
end;

procedure TTMSFNCCustomWebBrowser.SetEnabled(const Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateEnabled;
end;

procedure TTMSFNCCustomWebBrowser.AncestorVisibleChanged(const Visible: Boolean);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateVisible;
end;

procedure TTMSFNCCustomWebBrowser.SetParent(const Value: TFmxObject);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.BeforeChangeParent;
  inherited;
  Initialize;
end;

procedure TTMSFNCCustomWebBrowser.SetVisible(const Value: Boolean);
begin
  inherited;
  if Assigned(FWebBrowser) then
    FWebBrowser.UpdateVisible;
end;
{$ENDIF}

procedure TTMSFNCCustomWebBrowser.SetURL(const Value: string);
begin
  FURL := Value;
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.URL := Value;
    if CanCreateBrowser and (Value <> '') then
      Navigate;
  end;
end;

procedure TTMSFNCCustomWebBrowser.SetUserAgent(const Value: string);
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.UserAgent := Value;
end;

procedure TTMSFNCCustomWebBrowser.ShowDebugConsole;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.ShowDebugConsole;
end;

procedure TTMSFNCCustomWebBrowser.ShowPrintUI;
var
  ip: ITMSFNCCustomWebBrowserPrint;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserPrint, ip) = S_OK then
    begin
      ip.ShowPrintUI;
    end;
  end;
end;

procedure TTMSFNCCustomWebBrowser.DeInitialize;
begin
  FInitialized := False;
  if Assigned(FWebBrowser) then
    FWebBrowser.DeInitialize;
end;

procedure TTMSFNCCustomWebBrowser.DeleteAllCookies;
var
  ic: ITMSFNCCustomWebBrowserCookies;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserCookies, ic) = S_OK then
      ic.DeleteAllCookies;
  end;
end;

procedure TTMSFNCCustomWebBrowser.DeleteCookie(AName, ADomain, APath: string);
var
  ic: ITMSFNCCustomWebBrowserCookies;
begin
  if Assigned(FWebBrowser) then
  begin
    if FWebBrowser.QueryInterface(IID_ITMSFNCCustomWebBrowserCookies, ic) = S_OK then
      ic.DeleteCookie(AName, ADomain, APath);
  end;
end;

class procedure TTMSFNCCustomWebBrowser.DeleteCookies;
var
  WebBrowserService: ITMSFNCWebBrowserService;
begin
  inherited;
  if TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
    WebBrowserService.DeleteCookies;
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCCustomWebBrowser.StartDocumentReadyStateThread;
begin
  StopDocumentReadyStateThread;

  if not Assigned(FDocumentReadyStateThread) then
  begin
    FDocumentReadyStateThread := TTMSFNCWebBrowserDocumentReadyStateThread.Create(Self);
    FDocumentReadyStateThread.OnTerminate := DoTerminate;
  end;
end;

procedure TTMSFNCCustomWebBrowser.StopDocumentReadyStateThread;
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

procedure TTMSFNCCustomWebBrowser.StopLoading;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.StopLoading;
end;

{ TTMSFNCWebBrowserFactoryService }

constructor TTMSFNCWebBrowserFactoryService.Create;
begin
  inherited Create;
  FWebBrowsers := TTMSFNCWebBrowserList.Create;
end;

function TTMSFNCWebBrowserFactoryService.CreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := DoCreateWebBrowser(AValue);
  FWebBrowsers.Add(Result);
end;

destructor TTMSFNCWebBrowserFactoryService.Destroy;
begin
  FreeAndNil(FWebBrowsers);
  inherited Destroy;
end;

procedure TTMSFNCWebBrowserFactoryService.DestroyWebBrowser(const AValue: ITMSFNCCustomWebBrowser);
begin
  DoRemoveWebBrowser(AValue);
end;

procedure TTMSFNCWebBrowserFactoryService.DoRemoveWebBrowser(const AValue: ITMSFNCCustomWebBrowser);
begin
  if (FWebBrowsers <> nil) and (AValue <> nil) then
    FWebBrowsers.Remove(AValue);
end;

{ TTMSFNCWebBrowserPopup }

procedure TTMSFNCWebBrowserPopup.DoBeforeNavigate(Sender: TObject; var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams);
begin
  if Assigned(OnBeforeNavigate) then
    OnBeforeNavigate(Self, Params);
end;

procedure TTMSFNCWebBrowserPopup.Close(AModalResult: TModalResult = mrOk);
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

procedure TTMSFNCWebBrowserPopup.CloseForm(Sender: TObject);
begin
  Close;
end;

constructor TTMSFNCWebBrowserPopup.Create(AOwner: TComponent);
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
  FButtonEventHandler := TTMSFNCWebBrowserPopupButtonEventHandler.Create;
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

destructor TTMSFNCWebBrowserPopup.Destroy;
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
procedure TTMSFNCWebBrowserPopup.FormShow(Sender: TObject);
begin
  if FFirstLoad then
  begin
    FFirstLoad := False;
    if Assigned(FWebBrowser) then
      FWebBrowser.URL := FLoadURL;
  end;
end;
{$ENDIF}

function TTMSFNCWebBrowserPopup.GetWebBrowserClass: TTMSFNCCustomWebBrowserClass;
begin
  Result := TTMSFNCCustomWebBrowser;
end;

procedure TTMSFNCWebBrowserPopup.InitializeWebBrowser(AWebBrowser: TTMSFNCCustomWebBrowser);
begin

end;

{$IFNDEF ANDROID}
procedure TTMSFNCWebBrowserPopup.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TTMSFNCWebBrowserPopup.DoNavigateComplete(Sender: TObject; var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
begin
  if Assigned(OnNavigateComplete) then
    OnNavigateComplete(Self, Params);
end;

function TTMSFNCWebBrowserPopup.Open(AURL: String;
  AModal: Boolean = True): TModalResult;
begin
  URL := AURL;
  Result := Open(AModal);
end;

procedure TTMSFNCWebBrowserPopup.ButtonClose(Sender: TObject);
begin
  Close;
end;

function TTMSFNCWebBrowserPopup.Open(AModal: Boolean = True): TModalResult;
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
    FWebBrowserForm := TTMSFNCWebBrowserPopupForm.CreateNew(Application);
    {$ELSE}
    FWebBrowserForm := TTMSFNCWebBrowserPopupForm.CreateNew(FPopup);
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

{ TTMSFNCWebBrowserPopupForm }

procedure TTMSFNCWebBrowserPopupForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateBackGround;
end;

procedure TTMSFNCWebBrowserPopupForm.UpdateBackGround;
var
  wb: TTMSFNCCustomWebBrowser;
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

{ TTMSFNCWebBrowserCustomContextMenuItem }

function TTMSFNCWebBrowserCustomContextMenuItem.GetParentItem: TTMSFNCWebBrowserContextMenuItem;
begin
  Result := inherited ParentItem;
end;

{ TTMSFNCBrowserContextMenuItem }

function TTMSFNCWebBrowserContextMenuItem.AsCustom: TTMSFNCWebBrowserCustomContextMenuItem;
begin
  if Self is TTMSFNCWebBrowserCustomContextMenuItem then
    Result := TTMSFNCWebBrowserCustomContextMenuItem(Self)
  else
    Result := nil;
end;

function TTMSFNCWebBrowserContextMenuItem.AsSystem: TTMSFNCWebBrowserSystemContextMenuItem;
begin
  if Self is TTMSFNCWebBrowserSystemContextMenuItem then
    Result := TTMSFNCWebBrowserSystemContextMenuItem(Self)
  else if Self is TTMSFNCWebBrowserCustomContextMenuItem then
    Result :=  TTMSFNCWebBrowserSystemContextMenuItem(Self)
  else
    Result := nil;
end;

constructor TTMSFNCWebBrowserContextMenuItem.Create;
begin
  FKind := ikCommand;
  FEnabled := True;
  FOriginalIndex := -1;
  FChildren := TTMSFNCWebBrowserContextMenuItemList.Create;
  FIcon := TTMSFNCBitmap.Create;
end;

destructor TTMSFNCWebBrowserContextMenuItem.Destroy;
begin
  if Assigned(FEventHandlerObject) then
    FreeAndNil(FEventHandlerObject);

  FIcon.Free;
  FChildren.Free;
  inherited;
end;

procedure TTMSFNCWebBrowserContextMenuItem.SetShortcutKeyDescription(const Value: string);
begin
  FShortcutKeyDescription := Value;
end;

{ TTMSFNCBrowserSystemContextMenuItem }

function TTMSFNCWebBrowserSystemContextMenuItem.GetChecked: Boolean;
begin
  Result := inherited Checked;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetChildren: TObjectList<TTMSFNCWebBrowserContextMenuItem>;
begin
  Result := inherited Children;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetIcon: TTMSFNCBitmap;
begin
  Result := inherited Icon;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetKind: TTMSFNCWebBrowserContextMenuItemKind;
begin
  Result := inherited Kind;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetName: string;
begin
  Result := inherited Name;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetParentItem: TTMSFNCWebBrowserContextMenuItem;
begin
  Result := inherited ParentItem;
end;

function TTMSFNCWebBrowserSystemContextMenuItem.GetText: string;
begin
  Result := inherited Text;
end;

{ TTMSFNCCustomWebBrowserSettings }

procedure TTMSFNCCustomWebBrowserSettings.ApplySettings;
begin
  SetEnableAcceleratorKeys(FEnableAcceleratorKeys);
  SetEnableContextMenu(FEnableContextMenu);
  SetEnableShowDebugConsole(FEnableShowDebugConsole);
//  SetEnableScript(FEnableScript);
end;

constructor TTMSFNCCustomWebBrowserSettings.Create(AOwner: TTMSFNCCustomWebBrowser);
begin
  inherited Create;
  FEnableScript := True;
  FEnableContextMenu := True;
  FEnableAcceleratorKeys := True;
  FEnableShowDebugConsole := True;

  FOwner := AOwner;
end;

destructor TTMSFNCCustomWebBrowserSettings.Destroy;
begin

  inherited;
end;

function TTMSFNCCustomWebBrowserSettings.GetEnableAcceleratorKeys: Boolean;
begin
  Result := FEnableAcceleratorKeys;
  if Assigned(FOwner) and FOwner.FInitialized then
    Result := FOwner.EnableAcceleratorKeys;
end;

function TTMSFNCCustomWebBrowserSettings.GetEnableContextMenu: Boolean;
begin
  Result := FEnableContextMenu;
  if Assigned(FOwner) and FOwner.FInitialized then
    Result := FOwner.EnableContextMenu;
end;

function TTMSFNCCustomWebBrowserSettings.GetEnableScript: Boolean;
var
  s: ITMSFNCCustomWebBrowserSettings;
begin
  if Assigned(FOwner) and FOwner.FInitialized then
  begin
    if Assigned(FOwner) then
    begin
      s := FOwner.SettingsI;
      if Assigned(s) then
        FEnableScript := s.GetEnableScript;
    end;
  end;
  Result := FEnableScript;
end;

function TTMSFNCCustomWebBrowserSettings.GetEnableShowDebugConsole: Boolean;
begin
  Result := FEnableShowDebugConsole;
  if Assigned(FOwner) and FOwner.FInitialized then
    Result := FOwner.EnableShowDebugConsole;
end;

function TTMSFNCCustomWebBrowserSettings.GetUsePopupMenuAsContextMenu: Boolean;
begin
  Result := FUsePopupMenuAsContextMenu;
end;

procedure TTMSFNCCustomWebBrowserSettings.SetEnableAcceleratorKeys(const Value: Boolean);
begin
  FEnableAcceleratorKeys := Value;
  if Assigned(FOwner) then
    FOwner.EnableAcceleratorKeys := Value;
end;

procedure TTMSFNCCustomWebBrowserSettings.SetEnableContextMenu(const Value: Boolean);
begin
  FEnableContextMenu := Value;
  if Assigned(FOwner) then
    FOwner.EnableContextMenu := Value;
end;

procedure TTMSFNCCustomWebBrowserSettings.SetEnableScript(const Value: boolean);
var
  s: ITMSFNCCustomWebBrowserSettings;
begin
  FEnableScript := Value;
  if Assigned(FOwner) then
  begin
    s := FOwner.SettingsI;
    if Assigned(s) then
      s.SetEnableScript(Value);
  end;
end;

procedure TTMSFNCCustomWebBrowserSettings.SetEnableShowDebugConsole(const Value: Boolean);
begin
  FEnableShowDebugConsole := Value;
  if Assigned(FOwner) then
    FOwner.EnableShowDebugConsole := Value;
end;

procedure TTMSFNCCustomWebBrowserSettings.SetUsePopupMenuAsContextMenu(const Value: Boolean);
begin
  FUsePopupMenuAsContextMenu := Value;
end;

{$IFNDEF WEBLIB}

{ TTMSFNCScript }

constructor TTMSFNCScript.Create(AScript: string; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent);
begin
  FScript := AScript;
  FCompleteEvent := ACompleteEvent;
end;
{$ENDIF}

{$IFDEF IOS}

{ TTMSFNCWebBrowserPopupButtonEventHandler }

procedure TTMSFNCWebBrowserPopupButtonEventHandler.Click;
begin
  if Assigned(FWebBrowserPopup) then
    FWebBrowserPopup.Close;
end;

function TTMSFNCWebBrowserPopupButtonEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ITMSFNCWebBrowserPopupButtonEventHandler);
end;
{$ENDIF}

{$IFDEF ANDROID}

{ TTMSFNCWebBrowserPopupButtonEventHandler }

procedure TTMSFNCWebBrowserPopupButtonEventHandler.onClick(P1: JView);
begin
  if Assigned(FWebBrowserPopup) then
    FWebBrowserPopup.Close;
end;

{$ENDIF}

{$IFNDEF WEBLIB}

{ TTMSFNCWebBrowserDocumentReadyStateThread }

constructor TTMSFNCWebBrowserDocumentReadyStateThread.Create(AWebBrowser: TTMSFNCCustomWebBrowser);
begin
  inherited Create(False);
  FWebBrowser := AWebBrowser;
end;

procedure TTMSFNCWebBrowserDocumentReadyStateThread.Execute;
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

{ TTMSFNCWebBrowserPlatformServices }

function TTMSFNCWebBrowserPlatformServices.Count: Integer;
begin
  Result := FServicesList.Count;
end;

procedure TTMSFNCWebBrowserPlatformServices.AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
var
  LService: IInterface;
begin
  if not FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
  begin
    if Supports(AService, AServiceGUID, LService) then
      FServicesList.Add(TTMSFNCWebBrowserPlatformServicesService.Create(GUIDToString(AServiceGUID), AService));
  end;
end;

constructor TTMSFNCWebBrowserPlatformServices.Create;
begin
  inherited;
  FServicesList := TTMSFNCWebBrowserPlatformServicesList.Create;
end;

destructor TTMSFNCWebBrowserPlatformServices.Destroy;
begin
  FreeAndNil(FServicesList);
  inherited;
end;

{$IFNDEF AUTOREFCOUNT}
class procedure TTMSFNCWebBrowserPlatformServices.ReleaseCurrent;
begin
  FreeAndNil(FCurrent);
  FCurrentReleased := True;
end;
{$ENDIF}

class function TTMSFNCWebBrowserPlatformServices.Current: TTMSFNCWebBrowserPlatformServices;
begin
  if (FCurrent = nil) and not FCurrentReleased then
    FCurrent := TTMSFNCWebBrowserPlatformServices.Create;
  Result := FCurrent;
end;

function TTMSFNCWebBrowserPlatformServices.GetPlatformService(const AServiceGUID: TGUID): IInterface;
var
  k: IInterface;
begin
  k := FServicesList.Interfaces[GUIDToString(AServiceGUID)];
  Supports(k, AServiceGUID, Result);
end;

procedure TTMSFNCWebBrowserPlatformServices.RemovePlatformService(const AServiceGUID: TGUID);
begin
  FServicesList.RemoveByGUID(GUIDToString(AServiceGUID));
end;

function TTMSFNCWebBrowserPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID;
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

function TTMSFNCWebBrowserPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID): Boolean;
begin
  Result := FServicesList.ContainsKey(GUIDToString(AServiceGUID));
end;

{ TTMSFNCWebBrowserPlatformServicesService }

constructor TTMSFNCWebBrowserPlatformServicesService.Create(AGUID: string;
  AInterface: IInterface);
begin
  FGUID := AGUID;
  FInterface := AInterface;
end;

{ TTMSFNCWebBrowserPlatformServicesList }

function TTMSFNCWebBrowserPlatformServicesList.ContainsKey(AGUID: string): Boolean;
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

function TTMSFNCWebBrowserPlatformServicesList.GetValue(AGUID: string): IInterface;
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

procedure TTMSFNCWebBrowserPlatformServicesList.RemoveByGUID(AGUID: string);
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
function TTMSFNCWebBrowserList.GetItem(Index: Integer): ITMSFNCCustomWebBrowser;
begin
  Result := ITMSFNCCustomWebBrowser(inherited Items[Index]);
end;

procedure TTMSFNCWebBrowserList.SetItem(Index: Integer; const Value: ITMSFNCCustomWebBrowser);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCWebBrowserPlatformServicesList.GetItem(Index: Integer): TTMSFNCWebBrowserPlatformServicesService;
begin
  Result := TTMSFNCWebBrowserPlatformServicesService(inherited Items[Index]);
end;

procedure TTMSFNCWebBrowserPlatformServicesList.SetItem(Index: Integer; const Value: TTMSFNCWebBrowserPlatformServicesService);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TTMSFNCWebBrowser }

procedure TTMSFNCWebBrowser.AddBridge(ABridgeName: string; ABridgeObject: TObject);
begin
  inherited AddBridge(ABridgeName, ABridgeObject);
end;

{$IFNDEF FNCLIB}
procedure TTMSFNCWebBrowser.AddCookie(ACookie: TTMSFNCWebBrowserCookie);
begin
  inherited AddCookie(ACookie);
end;

procedure TTMSFNCWebBrowser.DeleteAllCookies;
begin
  inherited DeleteAllCookies;
end;

procedure TTMSFNCWebBrowser.DeleteCookie(AName, ADomain, APath: string);
begin
  inherited DeleteCookie(AName, ADomain, APath);
end;

procedure TTMSFNCWebBrowser.GetCookies(AURI: string);
begin
  inherited GetCookies(AURI);
end;

function TTMSFNCWebBrowser.InitialPrintSettings: TTMSFNCWebBrowserPrintSettings;
begin
  Result := inherited InitialPrintSettings;
end;

procedure TTMSFNCWebBrowser.OpenTaskManager;
begin
  inherited OpenTaskManager;
end;

procedure TTMSFNCWebBrowser.ShowPrintUI;
begin
  inherited ShowPrintUI;
end;

procedure TTMSFNCWebBrowser.Print(APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited Print(APrintSettings);
end;

procedure TTMSFNCWebBrowser.PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited PrintToPDFStream(APrintSettings);
end;

procedure TTMSFNCWebBrowser.Print;
begin
  inherited Print;
end;

procedure TTMSFNCWebBrowser.PrintToPDF(AFileName: string;
  APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited PrintToPDF(AFileName, APrintSettings);
end;

procedure TTMSFNCWebBrowser.PrintToPDF(AFileName: string);
begin
  inherited PrintToPDF(AFileName);
end;

procedure TTMSFNCWebBrowser.PrintToPDFStream;
begin
  inherited PrintToPDFStream;
end;

procedure TTMSFNCWebBrowser.NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings);
begin
  inherited NavigateWithData(AURI, AMethod, ABodyStream, AHeaders);
end;

procedure TTMSFNCWebBrowser.NavigateWithData(AURI, AMethod, ABody: string; AHeaders: TStrings);
begin
  inherited NavigateWithData(AURI, AMethod, ABody, AHeaders);
end;

{$ENDIF}

function TTMSFNCWebBrowser.CanGoBack: Boolean;
begin
  Result := inherited CanGoBack;
end;

function TTMSFNCWebBrowser.CanGoForward: Boolean;
begin
  Result := inherited CanGoForward;
end;

procedure TTMSFNCWebBrowser.CaptureScreenShot;
begin
  inherited CaptureScreenShot;
end;

procedure TTMSFNCWebBrowser.ClearCache;
begin
  inherited ClearCache;
end;

procedure TTMSFNCWebBrowser.DeInitialize;
begin
  inherited DeInitialize;
end;

procedure TTMSFNCWebBrowser.ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  inherited ExecuteJavaScript(AScript, ACompleteEvent, AImmediate);
end;

function TTMSFNCWebBrowser.ExecuteJavaScriptSync(AScript: string): string;
begin
  Result := inherited ExecuteJavaScriptSync(AScript);
end;

function TTMSFNCWebBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
begin
  Result := inherited GetBridgeCommunicationLayer(ABridgeName);
end;

{$IFDEF ANDROID}
function TTMSFNCWebBrowser.NativeDialog: Pointer;
begin
  Result := inherited NativeDialog;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TTMSFNCWebBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := inherited GetWebBrowserInstance;
end;
{$ENDIF}

procedure TTMSFNCWebBrowser.GoBack;
begin
  inherited GoBack;
end;

procedure TTMSFNCWebBrowser.GoForward;
begin
  inherited GoForward;
end;

procedure TTMSFNCWebBrowser.Initialize;
begin
  inherited Initialize;
end;

function TTMSFNCWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := inherited IsFMXBrowser;
end;

procedure TTMSFNCWebBrowser.LoadFile(AFile: String);
begin
  inherited LoadFile(AFile);
end;

procedure TTMSFNCWebBrowser.LoadHTML(AHTML: String);
begin
  inherited LoadHTML(AHTML);
end;

function TTMSFNCWebBrowser.NativeBrowser: Pointer;
begin
  Result := inherited NativeBrowser;
end;

function TTMSFNCWebBrowser.NativeEnvironment: Pointer;
begin
  Result := inherited NativeEnvironment;
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCWebBrowser.StartDocumentReadyStateThread;
begin
  inherited StartDocumentReadyStateThread;
end;
{$ENDIF}

procedure TTMSFNCWebBrowser.Navigate;
begin
  inherited Navigate;
end;

procedure TTMSFNCWebBrowser.Navigate(const AURL: string);
begin
  inherited Navigate(AURL);
end;

procedure TTMSFNCWebBrowser.Reload;
begin
  inherited Reload;
end;

procedure TTMSFNCWebBrowser.RemoveBridge(ABridgeName: string);
begin
  inherited RemoveBridge(ABridgeName);
end;

procedure TTMSFNCWebBrowser.ShowDebugConsole;
begin
  inherited ShowDebugConsole;
end;

procedure TTMSFNCWebBrowser.StopLoading;
begin
  inherited StopLoading;
end;

initialization
begin
  TTMSFNCWebBrowserPlatformServices.FCurrentReleased := False;
  RegisterWebBrowserService;
end;

{$IFNDEF WEBLIB}
finalization
begin
  UnRegisterWebBrowserService;
{$IFNDEF AUTOREFCOUNT}
  TTMSFNCWebBrowserPlatformServices.ReleaseCurrent;
{$ENDIF}
end;
{$ENDIF}

end.
