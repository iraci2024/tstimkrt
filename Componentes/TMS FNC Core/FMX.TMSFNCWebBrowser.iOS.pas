{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020                                      }
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

unit FMX.TMSFNCWebBrowser.iOS;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF IOS}
uses
  FMX.Platform.iOS, iOSApi.CocoaTypes, iOSApi.UIKit,
  iOSApi.Foundation, iOSApi.CoreGraphics, MacApi.ObjectiveC;

const
  WebKitFWK = '/System/Library/Frameworks/WebKit.framework/WebKit';

const
  WKNavigationActionPolicyCancel = 0;
  WKNavigationActionPolicyAllow = 1;
type
  NSEventModifierFlags = NSUInteger;
  WKWebView = interface;
  TWebKitJavaScriptCompletionHandler = procedure(param1: Pointer; param2: NSError) of object;
  TWebKitSnapShotCompletionHandler = procedure(param1: Pointer; param2: NSError) of object;
  WKUserScriptInjectionTime = NSInteger;
  WKNavigationType = NSInteger;
  WKNavigationActionPolicy = NSInteger;
  WKNavigationResponsePolicy = NSInteger;

  WKProcessPoolClass = interface(NSObjectClass)
    ['{82C96CC6-2173-4008-BC55-A1AEE36AED33}']
  end;
  WKProcessPool = interface(NSObject)
    ['{8C2D9FDC-AC19-4C74-BC48-65931A72CC3B}']
  end;

  TWKProcessPool = class(TOCGenericImport<WKProcessPoolClass, WKProcessPool>)
  end;

  WKPreferencesClass = interface(NSObjectClass)
    ['{8F3152CA-4911-46FA-B496-993DB3CE55CF}']
  end;
  WKPreferences = interface(NSObject)
    ['{43BBC734-247B-45F5-A178-5AAF9CC85DFD}']
    procedure setMinimumFontSize(minimumFontSize: CGFloat); cdecl;
    function minimumFontSize: CGFloat; cdecl;
    procedure setJavaScriptEnabled(javaScriptEnabled: Boolean); cdecl;
    function javaScriptEnabled: Boolean; cdecl;
    procedure setJavaScriptCanOpenWindowsAutomatically(javaScriptCanOpenWindowsAutomatically: Boolean); cdecl;
    function javaScriptCanOpenWindowsAutomatically: Boolean; cdecl;
    procedure setJavaEnabled(javaEnabled: Boolean); cdecl;
    function javaEnabled: Boolean; cdecl;
    procedure setPlugInsEnabled(plugInsEnabled: Boolean); cdecl;
    function plugInsEnabled: Boolean; cdecl;
  end;

  TWKPreferences = class(TOCGenericImport<WKPreferencesClass, WKPreferences>)
  end;

  WKUserScriptClass = interface(NSObjectClass)
    ['{4441FA54-AD61-48EE-86F7-5CABB5C31166}']
  end;
  WKUserScript = interface(NSObject)
    ['{EBD76C63-637E-4887-AC99-4722B87A28C0}']
    function source: NSString; cdecl;
    function injectionTime: WKUserScriptInjectionTime; cdecl;
    function isForMainFrameOnly: Boolean; cdecl;
    function initWithSource(source: NSString; injectionTime: WKUserScriptInjectionTime; forMainFrameOnly: Boolean)
      : Pointer {instancetype}; cdecl;
  end;

  TWKUserScript = class(TOCGenericImport<WKUserScriptClass, WKUserScript>)
  end;

  WKUserContentControllerClass = interface(NSObjectClass)
    ['{CDA61E74-F460-4AAF-AE06-38FF6C778D68}']
  end;
  WKUserContentController = interface(NSObject)
    ['{FB139B38-7559-4BFF-B373-FEB60ECFEEC0}']
    function userScripts: NSArray; cdecl;
    procedure addUserScript(userScript: WKUserScript); cdecl;
    procedure removeAllUserScripts; cdecl;
    procedure addScriptMessageHandler(scriptMessageHandler: Pointer; name: NSString); cdecl;
    procedure removeScriptMessageHandlerForName(name: NSString); cdecl;
  end;

  TWKUserContentController = class(TOCGenericImport<WKUserContentControllerClass, WKUserContentController>)
  end;

  WKURLSchemeHandler = interface(IObjectiveC)
    ['{092F11A5-F586-469F-A51A-9A53CAA6478B}']
    [MethodName('webView:startURLSchemeTask:')]
    procedure webViewStartURLSchemeTask(webView: WKWebView; startURLSchemeTask: Pointer); cdecl;
    [MethodName('webView:stopURLSchemeTask:')]
    procedure webViewStopURLSchemeTask(webView: WKWebView; stopURLSchemeTask: Pointer); cdecl;
  end;

  WKWebViewConfigurationClass = interface(NSObjectClass)
    ['{6F371C26-DE2B-4C1F-BEFC-E342B689DA61}']
  end;
  WKWebViewConfiguration = interface(NSObject)
    ['{02422458-3164-4634-9659-FD69D028D977}']
    procedure setProcessPool(processPool: WKProcessPool); cdecl;
    function processPool: WKProcessPool; cdecl;
    procedure setPreferences(preferences: WKPreferences); cdecl;
    function preferences: WKPreferences; cdecl;
    procedure setUserContentController(userContentController: WKUserContentController); cdecl;
    function userContentController: WKUserContentController; cdecl;
    procedure setSuppressesIncrementalRendering(suppressesIncrementalRendering: Boolean); cdecl;
    function suppressesIncrementalRendering: Boolean; cdecl;
    procedure setURLSchemeHandler(urlSchemeHandler: Pointer; forURLScheme: NSString); cdecl;
    procedure setAllowsInlineMediaPlayback(allowsInlineMediaPlayback: Boolean); cdecl;
    function allowsInlineMediaPlayback: Boolean; cdecl;
  end;

  TWKWebViewConfiguration = class(TOCGenericImport<WKWebViewConfigurationClass, WKWebViewConfiguration>)
  end;

  WKBackForwardListItemClass = interface(NSObjectClass)
    ['{96708A6E-4D2E-4920-B84C-39B010345011}']
  end;
  WKBackForwardListItem = interface(NSObject)
    ['{69054291-E417-4EB0-88C7-FA753579D37B}']
    function URL: NSURL; cdecl;
    function title: NSString; cdecl;
    function initialURL: NSURL; cdecl;
  end;

  TWKBackForwardListItem = class(TOCGenericImport<WKBackForwardListItemClass, WKBackForwardListItem>)
  end;
  PWKBackForwardListItem = Pointer;

  WKBackForwardListClass = interface(NSObjectClass)
    ['{A6901838-5735-4DBA-A3B0-7ACB588666E2}']
  end;
  WKBackForwardList = interface(NSObject)
    ['{BB720EBC-9856-4BAF-953F-E3FB6D69592C}']
    function currentItem: WKBackForwardListItem; cdecl;
    function backItem: WKBackForwardListItem; cdecl;
    function forwardItem: WKBackForwardListItem; cdecl;
    function itemAtIndex(index: NSInteger): WKBackForwardListItem; cdecl;
    function backList: NSArray; cdecl;
    function forwardList: NSArray; cdecl;
  end;

  TWKBackForwardList = class(TOCGenericImport<WKBackForwardListClass, WKBackForwardList>)
  end;

  WKNavigationClass = interface(NSObjectClass)
    ['{B8E8AFB6-81FC-4F34-B222-29F6B3EDFB62}']
  end;
  WKNavigation = interface(NSObject)
    ['{A81F2681-FB94-4AA0-8099-818D3800C859}']
  end;

  TWKNavigation = class(TOCGenericImport<WKNavigationClass, WKNavigation>)
  end;

  WKSnapshotConfigurationClass = interface(NSObjectClass)
    ['{DA802FDF-4AB3-4D6E-A633-1FAEA6B3DC72}']
  end;

  WKSnapshotConfiguration = interface(NSObject)
    ['{03756D7C-D45B-485B-BA19-4BFA3DC1E642}']
  end;

  TWKSnapshotConfiguration = class(TOCGenericImport<WKSnapshotConfigurationClass, WKSnapshotConfiguration>)
  end;

  WKWebViewClass = interface(UIViewClass)
    ['{975E5DD2-26EF-4824-9199-6768A1C93D05}']
  end;

  WKWebView = interface(UIView)
    ['{A591EAE9-4467-403D-8987-94B94780B1C4}']
    function configuration: WKWebViewConfiguration; cdecl;
    procedure setNavigationDelegate(navigationDelegate: Pointer); cdecl;
    function navigationDelegate: Pointer; cdecl;
    procedure setUIDelegate(UIDelegate: Pointer); cdecl;
    function UIDelegate: Pointer; cdecl;
    function backForwardList: WKBackForwardList; cdecl;
    function initWithFrame(frame: CGRect; configuration: WKWebViewConfiguration): Pointer {instancetype}; cdecl;
    function loadRequest(request: NSURLRequest): WKNavigation; cdecl;
    function loadHTMLString(&string: NSString; baseURL: NSURL): WKNavigation; cdecl;
    function goToBackForwardListItem(item: WKBackForwardListItem): WKNavigation; cdecl;
    function title: NSString; cdecl;
    function URL: NSURL; cdecl;
    function isLoading: Boolean; cdecl;
    function estimatedProgress: Double; cdecl;
    function hasOnlySecureContent: Boolean; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    [MethodName('goBack')]
    function goBack: WKNavigation; cdecl; overload;
    [MethodName('goForward')]
    function goForward: WKNavigation; cdecl; overload;
    [MethodName('reload')]
    function reload: WKNavigation; cdecl; overload;
    [MethodName('reloadFromOrigin')]
    function reloadFromOrigin: WKNavigation; cdecl; overload;
    [MethodName('stopLoading')]
    procedure stopLoading; cdecl; overload;
    procedure evaluateJavaScript(javaScriptString: NSString; completionHandler: TWebKitJavaScriptCompletionHandler); cdecl;
    procedure setAllowsBackForwardNavigationGestures(allowsBackForwardNavigationGestures: Boolean); cdecl;
    function allowsBackForwardNavigationGestures: Boolean; cdecl;
    procedure setAllowsMagnification(allowsMagnification: Boolean); cdecl;
    function allowsMagnification: Boolean; cdecl;
    [MethodName('setMagnification:')]
    procedure setMagnification(magnification: CGFloat); cdecl;
    function magnification: CGFloat; cdecl;
    [MethodName('setMagnification:centeredAtPoint:')]
    procedure setMagnificationCenteredAtPoint(magnification: CGFloat; centeredAtPoint: CGPoint); cdecl;
    [MethodName('goBack:')]
    procedure goBack(sender: Pointer); cdecl; overload;
    [MethodName('goForward:')]
    procedure goForward(sender: Pointer); cdecl; overload;
    [MethodName('reload:')]
    procedure reload(sender: Pointer); cdecl; overload;
    [MethodName('reloadFromOrigin:')]
    procedure reloadFromOrigin(sender: Pointer); cdecl; overload;
    [MethodName('stopLoading:')]
    procedure stopLoading(sender: Pointer); cdecl; overload;
    procedure takeSnapshotWithConfiguration(snapshotConfiguration: WKSnapshotConfiguration; completionHandler: TWebKitSnapShotCompletionHandler); cdecl;
    procedure setCustomUserAgent(customUserAgent: NSString); cdecl;
    function customUserAgent: NSString; cdecl;
  end;

  TWKWebView = class(TOCGenericImport<WKWebViewClass, WKWebView>)
  end;

  WKFrameInfoClass = interface(NSObjectClass)
    ['{50098AD6-58E8-4481-A8E8-7C84928ECB45}']
  end;
  WKFrameInfo = interface(NSObject)
    ['{47C1C21B-704E-43A9-834D-5805B8562CEA}']
    function isMainFrame: Boolean; cdecl;
    function request: NSURLRequest; cdecl;
  end;

  TWKFrameInfo = class(TOCGenericImport<WKFrameInfoClass, WKFrameInfo>)
  end;

  WKNavigationActionClass = interface(NSObjectClass)
    ['{5562C106-8A7F-456C-B9B5-A5EA355D52EB}']
  end;
  WKNavigationAction = interface(NSObject)
    ['{974C4990-ABE5-4341-A574-14DF05988806}']
    function sourceFrame: WKFrameInfo; cdecl;
    function targetFrame: WKFrameInfo; cdecl;
    function navigationType: WKNavigationType; cdecl;
    function request: NSURLRequest; cdecl;
    function modifierFlags: NSEventModifierFlags; cdecl;
    function buttonNumber: NSInteger; cdecl;
  end;

  TWKNavigationAction = class(TOCGenericImport<WKNavigationActionClass, WKNavigationAction>)
  end;

  WKNavigationResponseClass = interface(NSObjectClass)
    ['{FB6FC302-EF16-4DE2-9590-CDD5F6982042}']
  end;
  WKNavigationResponse = interface(NSObject)
    ['{186F7F79-BF76-4211-B889-AEBE01232E91}']
    function isForMainFrame: Boolean; cdecl;
    function response: NSURLResponse; cdecl;
    function canShowMIMEType: Boolean; cdecl;
  end;

  TWKNavigationResponse = class(TOCGenericImport<WKNavigationResponseClass, WKNavigationResponse>)
  end;

  WKNavigationDelegate = interface(IObjectiveC)
    ['{AEA8B799-006C-4CA4-A053-4674434B8628}']
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationActionDecisionHandler(WebView: WKWebView;
      decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(WebView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
  end;

  WKWindowFeaturesClass = interface(NSObjectClass)
    ['{05DE3DEA-498D-439C-9ED9-9ACBA0C5D0CF}']
  end;
  WKWindowFeatures = interface(NSObject)
    ['{52557B85-78BE-47B5-87AC-6B01CD05D6E1}']
    function menuBarVisibility: NSNumber; cdecl;
    function statusBarVisibility: NSNumber; cdecl;
    function toolbarsVisibility: NSNumber; cdecl;
    function allowsResizing: NSNumber; cdecl;
    function x: NSNumber; cdecl;
    function y: NSNumber; cdecl;
    function width: NSNumber; cdecl;
    function height: NSNumber; cdecl;
  end;

  TWKWindowFeatures = class(TOCGenericImport<WKWindowFeaturesClass, WKWindowFeatures>)
  end;

  TWebKitCompletionHandler1 = procedure() of object;
  TWebKitCompletionHandler2 = procedure(param1: Boolean) of object;
  TWebKitCompletionHandler3 = procedure(param1: NSString) of object;

  WKUIDelegate = interface(IObjectiveC)
    ['{802EAEEC-5DD2-493E-B6A2-58447AA9F72C}']
    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    function webViewCreateWebViewWithConfigurationForNavigationActionWindowFeatures(WebView: WKWebView;
      createWebViewWithConfiguration: WKWebViewConfiguration; forNavigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptAlertPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: TWebKitCompletionHandler1); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptConfirmPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: TWebKitCompletionHandler2); cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler
      (WebView: WKWebView; runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString;
      initiatedByFrame: WKFrameInfo; completionHandler: TWebKitCompletionHandler3); cdecl;
  end;

  WKScriptMessageClass = interface(NSObjectClass)
    ['{EDA6B7F4-B997-48D1-8751-9330B7096934}']
  end;
  WKScriptMessage = interface(NSObject)
    ['{AC4D1420-C910-4EB8-8E31-0DBFB578FE4F}']
    function body: Pointer; cdecl;
    function WebView: WKWebView; cdecl;
    function frameInfo: WKFrameInfo; cdecl;
    function name: NSString; cdecl;
  end;

  TWKScriptMessage = class(TOCGenericImport<WKScriptMessageClass, WKScriptMessage>)
  end;

  WKScriptMessageHandler = interface(IObjectiveC)
    ['{74DD2F59-9247-48E2-8AE9-E068216B53EC}']
    procedure userContentController(userContentController: WKUserContentController;
      didReceiveScriptMessage: WKScriptMessage); cdecl;
  end;

  WKWebViewEx = interface(WKWebView)
  ['{A55D32B2-7EA4-4EAA-B7B5-408135AC4604}']
    function hitTest(point: CGPoint; withEvent: UIEvent): UIView; cdecl;
  end;
  {$ENDIF}

{$DEFINE USESAFARISERVICES}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  Classes, Math, SysUtils, FMX.Types, FMX.TMSFNCTypes, Types, FMX.Forms,
  FMX.TMSFNCWebBrowser, FMX.TMSFNCUtils, TypInfo, Generics.Collections
  {$IFDEF IOS}
  ,MacApi.ObjcRuntime
  {$ENDIF}
  {$IF defined(IOS) and NOT defined(CPUARM)}
  ,Posix.Dlfcn
  {$ENDIF}
  ;

const
  BridgeName = 'bridge';

{$IFDEF IOS}
function imp_implementationWithBlock(block: Pointer): Pointer; cdecl; external libobjc name _PU + 'imp_implementationWithBlock';
function imp_removeBlock(anImp: Pointer): Integer; cdecl; external libobjc name _PU + 'imp_removeBlock';
{$ENDIF}

{$IF defined(IOS) and NOT defined(CPUARM)}
var
  msgSafariServices: THandle;
  msgWebKit: THandle;
{$ENDIF}

{$IFDEF USESAFARISERVICES}
const
  libSafariServices = '/System/Library/Frameworks/SafariServices.framework/SafariServices';
{$ENDIF}

type
  TTMSFNCiOSWebBrowser = class;

  TTMSFNCiOSWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  {$IFDEF IOS}
  {$IFDEF USESAFARISERVICES}
  SFSafariViewControllerClass = interface(UIViewControllerClass)
    ['{FB0D364B-0410-446D-912E-B9913CC719F9}']
  end;
  SFSafariViewController = interface(UIViewController)
    ['{1A78BFD5-AA2D-41BE-81A8-9697E0AA7CD2}']
    function initWithURL(URL: NSURL): Pointer; cdecl; overload;
    function initWithURL(URL: NSURL; entersReaderIfAvailable: Boolean): Pointer; cdecl; overload;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSFSafariViewController = class(TOCGenericImport<SFSafariViewControllerClass, SFSafariViewController>)  end;

  SFSafariViewControllerDelegate = interface(IObjectiveC)
    ['{1D66D2A4-A3A0-48D6-8926-DC692A605E76}']
    procedure safariViewControllerDidFinish(controller: SFSafariViewController); cdecl;
  end;

  TTMSFNCiOSWebBrowserDelegateSF = class(TOCLocal, SFSafariViewControllerDelegate)
  private
    FWebBrowser: TTMSFNCiOSWebBrowser;
  public
    procedure safariViewControllerDidFinish(controller: SFSafariViewController); cdecl;
  end;
  {$ENDIF}

  TTMSFNCiOSWebBrowserScriptMessageHandler = class(TOCLocal, WKScriptMessageHandler)
  private
    FWebBrowser: TTMSFNCiOSWebBrowser;
  public
    procedure userContentController(userContentController: WKUserContentController;
      didReceiveScriptMessage: WKScriptMessage); cdecl;
  end;

  TTMSFNCiOSWebBrowserNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
  private
    FWebBrowser: TTMSFNCiOSWebBrowser;
  public
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationActionDecisionHandler(WebView: WKWebView;
      decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(WebView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
  end;

  TTMSFNCiOSWebBrowserEx = class(TOCLocal)
  private
    conf: WKWebViewConfiguration;
    FWebBrowser: TTMSFNCiOSWebBrowser;
  public
    constructor Create(AMessageHandler: TTMSFNCiOSWebBrowserScriptMessageHandler);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    function hitTest(point: CGPoint; withEvent: UIEvent): UIView; cdecl;
  end;
  {$ENDIF}

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  {$IFDEF IOS}
  TTMSFNCiOSWebBrowserScript = class
  private
    FWebbrowser: TTMSFNCiOSWebBrowser;
    FCallback: TNotifyEvent;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
  protected
    procedure DoEvaluateScriptComplete(param1: Pointer; param2: NSError);
  end;
  {$ENDIF}

  TTMSFNCiOSWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser)
  private
    {$IFDEF IOS}
    FScriptList: TObjectList<TTMSFNCiOSWebBrowserScript>;
    FSaveURL: string;
    FWebBrowser: WKWebView;
    FCustomBridge: string;
    FCustomBridgeObject: TObject;    
    FWebBrowserLocal: TTMSFNCiOSWebBrowserEx;
    FWebBrowserNavigationDelegate: TTMSFNCiOSWebBrowserNavigationDelegate;
    FWebBrowserMessageHandler: TTMSFNCiOSWebBrowserScriptMessageHandler;
    {$IFDEF USESAFARISERVICES}
    FSafariVC: SFSafariViewController;
    FWebBrowserDelegateSF: TTMSFNCiOSWebBrowserDelegateSF;
    {$ENDIF}
    {$ENDIF}
    FURL: string;
    FExternalBrowser: Boolean;
    FWebControl: TTMSFNCCustomWebBrowser;
  protected
    function GetUserAgent: string;
    procedure SetUserAgent(const AValue: string);
    function GetURL: string;
    procedure SetURL(const AValue: string);
    function GetExternalBrowser: Boolean;
    procedure ShowDebugConsole;
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    function GetEnableAcceleratorKeys: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    procedure SetFocus;
    procedure SetCacheFolderName(const Value: string);
    procedure SetCacheFolder(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure Navigate(const AURL: string); overload;
    procedure Navigate; overload;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    {$IFDEF IOS}
    procedure DoTakeSnapshotComplete(param1: Pointer; param2: NSError);
    {$ENDIF}
    procedure LoadHTML(AHTML: String);
    procedure LoadFile(AFile: String);
    procedure GoBack;
    procedure GoForward;
    procedure Close;
    procedure Reload;
    procedure StopLoading;
    procedure UpdateVisible;
    procedure UpdateEnabled;
    procedure UpdateBounds;
    procedure BeforeChangeParent;
    procedure Initialize;
    procedure DeInitialize;
    procedure ClearCache;
    procedure CaptureScreenShot;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject);
    procedure RemoveBridge(ABridgeName: string);
    function NativeEnvironment: Pointer;
    function NativeBrowser: Pointer;
    function NativeDialog: Pointer;
    function GetBrowserInstance: IInterface;
    function IsFMXBrowser: Boolean;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function GetCacheFolderName: string;
    function GetCacheFolder: string;
    function GetAutoClearCache: Boolean;
    property CacheFolder: string read GetCacheFolder write SetCacheFolder;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property UserAgent: string read GetUserAgent write SetUserAgent;
  public
    constructor Create(const AWebControl: TTMSFNCCustomWebBrowser);
  end;

var
  WebBrowserService: ITMSFNCWebBrowserService;

procedure RegisterWebBrowserService;
begin
  if not TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
  begin
    WebBrowserService := TTMSFNCiOSWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCiOSWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
end;

function TTMSFNCiOSWebBrowser.GetBrowserInstance: IInterface;
begin
  Result := nil;
end;

function TTMSFNCiOSWebBrowser.GetCacheFolder: string;
begin
  Result := '';
end;

function TTMSFNCiOSWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
end;

function TTMSFNCiOSWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := False;
end;

function TTMSFNCiOSWebBrowser.GetEnableContextMenu: Boolean;
begin
  Result := False;
end;

function TTMSFNCiOSWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  Result := False;
end;

function TTMSFNCiOSWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

function TTMSFNCiOSWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCiOSWebBrowser.GetUserAgent: string;
begin
  {$IFDEF IOS}
  Result := TTMSFNCCustomWebBrowserProtected(FWebControl).ExecuteJavascriptSync('window.navigator.userAgent');
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.GoBack;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.goBack;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.GoForward;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.goForward;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.Initialize;
{$IFDEF IOS}
var
  vw: UIView;
  frm: TCommonCustomForm;
{$ENDIF}
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    if (FWebControl <> nil) and (FWebControl.Root <> nil) and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      frm := TCommonCustomForm(FWebControl.Root.GetObject);
      vw := WindowHandleToPlatform(frm.Handle).View;
      vw.addSubview(FWebBrowser);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCiOSWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCiOSWebBrowser.LoadFile(AFile: String);
{$IFDEF IOS}
var
  url: NSUrl;
  requestobj: NSURLRequest;
{$ENDIF}
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    url := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSSTREx(AFile)));
    requestobj := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(url));
    FWebBrowser.loadRequest(requestobj);
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.LoadHTML(AHTML: String);
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.loadHTMLString(NSSTREx(AHTML), nil);
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.Navigate(const AURL: string);
{$IFDEF IOS}
var
  urlobj: NSURL;
  requestobj: NSURLRequest;
  function GetSharedApplication: UIApplication;
  begin
    Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
  end;
{$ENDIF}
begin
  {$IFDEF IOS}
  if FExternalBrowser then
  begin
    {$IFDEF USESAFARISERVICES}
    FSafariVC := TSFSafariViewController.Wrap(TSFSafariViewController.Wrap(TSFSafariViewController.OCClass.alloc).initWithURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStrEx(AURL)))));
    FSafariVC.setDelegate(FWebBrowserDelegateSF.GetObjectID);
    GetSharedApplication.keyWindow.rootViewController.presentViewController(FSafariVC, False, nil);
    {$ENDIF}
  end
  else
  begin
    if Assigned(FWebBrowser) then
    begin
      urlobj := TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSSTREx(AURL)));
      requestobj := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(urlobj));
      FWebBrowser.loadRequest(requestobj);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCiOSWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    Result := (FWebBrowser as ILocalObject).GetObjectID;
  {$ENDIF}
end;

function TTMSFNCiOSWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
end;

function TTMSFNCiOSWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
end;

procedure TTMSFNCiOSWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCiOSWebBrowser.Reload;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.reload;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.RemoveBridge(ABridgeName: string);
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowserLocal) and Assigned(FWebBrowserLocal.conf) and (ABridgeName <> '') then
    FWebBrowserLocal.conf.userContentController.removeScriptMessageHandlerForName(NSStrEx(ABridgeName));

  if ABridgeName = FCustomBridge then
  begin
    FCustomBridge := '';
    FCustomBridgeObject := nil;
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.SetAutoClearCache(const Value: Boolean);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetCacheFolder(const Value: string);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetCacheFolderName(const Value: string);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetEnableAcceleratorKeys(const AValue: Boolean);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetEnableContextMenu(const AValue: Boolean);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
begin

end;

procedure TTMSFNCiOSWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
end;

procedure TTMSFNCiOSWebBrowser.SetFocus;
begin
end;

procedure TTMSFNCiOSWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCiOSWebBrowser.SetUserAgent(const AValue: string);
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.setCustomUserAgent(NSStrEx(AValue));
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.ShowDebugConsole;
begin

end;

procedure TTMSFNCiOSWebBrowser.StopLoading;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.stopLoading;
  {$ENDIF}
end;

{$IFDEF IOS}
function CGRectFromRect(const R: TRectF): CGRect;
begin
  Result.origin.x := R.Left;
  Result.origin.Y := R.Top;
  Result.size.Width := R.Right - R.Left;
  Result.size.Height := R.Bottom - R.Top;
end;
{$ENDIF}

procedure TTMSFNCiOSWebBrowser.UpdateBounds;
{$IFDEF IOS}
var
  Bounds: TRectF;
{$ENDIF}
begin
  Initialize;
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    if (FWebControl <> nil)
      and not (csDesigning in FWebControl.ComponentState)
      and (FWebControl.Root <> nil)
      and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      Bounds := FWebControl.AbsoluteRect;
      FWebBrowser.setFrame(CGRectFromRect(RectF(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom)));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.UpdateEnabled;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) and Assigned(FWebControl) then
    FWebBrowser.setUserInteractionEnabled(FWebControl.Enabled);
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.UpdateVisible;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) and Assigned(FWebControl) then
    FWebBrowser.setHidden(not (FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible and FWebControl.ParentedVisible));
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowserLocal) and Assigned(FWebBrowserLocal.conf) and (ABridgeName <> '') and Assigned(ABridgeObject) then
  begin    
    FCustomBridge := ABridgeName;
    FCustomBridgeObject := ABridgeObject;
    FWebBrowserLocal.conf.userContentController.addScriptMessageHandler(ILocalObject(FWebBrowserMessageHandler).GetObjectID, NSStrEx(ABridgeName));
  end;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.BeforeChangeParent;
begin

end;

function TTMSFNCiOSWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoBack;
  {$ENDIF}
end;

function TTMSFNCiOSWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoForward;
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.CaptureScreenShot;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.takeSnapshotWithConfiguration(nil, DoTakeSnapshotComplete);
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.ClearCache;
begin

end;

procedure TTMSFNCiOSWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

constructor TTMSFNCiOSWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
begin
  FExternalBrowser := False;
  FWebControl := AWebControl;
  {$IFDEF IOS}
  FScriptList := TObjectList<TTMSFNCiOSWebBrowserScript>.Create;
  {$IFDEF USESAFARISERVICES}
  FWebBrowserDelegateSF := TTMSFNCiOSWebBrowserDelegateSF.Create;
  FWebBrowserDelegateSF.FWebBrowser := Self;
  {$ENDIF}
  FWebBrowserMessageHandler := TTMSFNCiOSWebBrowserScriptMessageHandler.Create;
  FWebBrowserMessageHandler.FWebBrowser := Self;
  FWebBrowserNavigationDelegate := TTMSFNCiOSWebBrowserNavigationDelegate.Create;
  FWebBrowserNavigationDelegate.FWebBrowser := Self;
  FWebBrowserLocal := TTMSFNCiOSWebBrowserEx.Create(FWebBrowserMessageHandler);
  FWebBrowserLocal.FWebBrowser := Self;
  FWebBrowser := WKWebView(FWebBrowserLocal.Super);
  FWebBrowser.setNavigationDelegate(ILocalObject(FWebBrowserNavigationDelegate).GetObjectID);
  {$ENDIF}
end;

procedure TTMSFNCiOSWebBrowser.DeInitialize;
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.setNavigationDelegate(nil);

  if Assigned(FWebBrowserNavigationDelegate) then
  begin
    FWebBrowserNavigationDelegate.Free;
    FWebBrowserNavigationDelegate := nil;
  end;

  if Assigned(FWebBrowserMessageHandler) then
  begin
    FWebBrowserMessageHandler.Free;
    FWebBrowserMessageHandler := nil;
  end;

  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.removeFromSuperview;
    FWebBrowser.release;
    FWebBrowser := nil;
  end;

  {$IFDEF USESAFARISERVICES}
  if Assigned(FWebBrowserDelegateSF) then
  begin
    FWebBrowserDelegateSF.Free;
    FWebBrowserDelegateSF := nil;
  end;
  {$ENDIF}

  if Assigned(FScriptList) then
  begin
    FScriptList.Free;
    FScriptList := nil;
  end;
  {$ENDIF}
end;

{$IFDEF IOS}
procedure TTMSFNCiOSWebBrowser.DoTakeSnapshotComplete(param1: Pointer; param2: NSError);
var
  img: UIImage;
  bmp: TTMSFNCBitmap;

  function BitmapFromImage(AImage: UIImage): TTMSFNCBitmap;
  var
    dt: NSData;
    ms: TMemoryStream;
  begin
    Result := nil;
    if not Assigned(AImage) then
     Exit;

    dt := TNSData.Wrap(UIImagePNGRepresentation((AImage as ILocalObject).GetObjectID));
    if Assigned(dt) then
    begin
      Result := TTMSFNCBitmap.Create(0, 0);
      ms := TMemoryStream.Create;
      ms.Write(dt.bytes^, dt.length);
      ms.Position := 0;
      Result.LoadFromStream(ms);
      ms.Free;
    end;
  end;

begin
  if Assigned(param1) then
  begin
    if TNSObject.Wrap(param1).isKindOfClass(objc_getClass('UIImage')) then
    begin
      img := TUIImage.Wrap(param1);
      bmp := BitmapFromImage(img);
      try
        if Assigned(FWebControl) then
          TTMSFNCCustomWebBrowserProtected(FWebControl).DoCaptureScreenShot(bmp);
      finally
        bmp.Free;
      end;
    end;
  end;
end;

procedure TTMSFNCiOSWebBrowserScript.DoEvaluateScriptComplete(param1: Pointer; param2: NSError);
var
  s: string;
begin
  s := '';
  if Assigned(param1) then
  begin
    if TNSObject.Wrap(param1).isKindOfClass(objc_getClass('NSNumber')) then
      s := UTF8ToString(TNSNumber.Wrap(param1).stringValue.UTF8String)
    else if TNSObject.Wrap(param1).isKindOfClass(objc_getClass('NSString')) then
      s := UTF8ToString(TNSString.Wrap(param1).UTF8String)
  end;

  if Assigned(FCompleteEvent) then
    FCompleteEvent(s);

  if Assigned(FCallback) then
    FCallback(Self);

  if Assigned(FWebbrowser) then
    FWebbrowser.FScriptList.Remove(Self);
end;
{$ENDIF}

procedure TTMSFNCiOSWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
{$IFDEF IOS}
var
  s: TTMSFNCiOSWebBrowserScript;
{$ENDIF}
begin
  {$IFDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    s := TTMSFNCiOSWebBrowserScript.Create;
    s.FCompleteEvent := ACompleteEvent;
    s.FCallback := ACallback;
    s.FWebbrowser := Self;
    FWebBrowser.evaluateJavaScript(NSStrEx(AScript), s.DoEvaluateScriptComplete);
  end;
  {$ENDIF}
end;

{ TTMSFNCiOSWebBrowserService }

procedure TTMSFNCiOSWebBrowserService.DeleteCookies;
{$IFDEF IOS}
var
  storage: NSHTTPCookieStorage;
  I: Integer;
  cnt: Integer;
{$ENDIF}
begin
  {$IFDEF IOS}
  storage := TNSHTTPCookieStorage.Wrap(TNSHTTPCookieStorage.OCClass.sharedHTTPCookieStorage);
  cnt := storage.cookies.count;
  for I := cnt - 1 downto 0 do
    storage.deleteCookie(TNSHTTPCookie.Wrap(storage.cookies.objectAtIndex(I)));

  TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults).synchronize;
  {$ENDIF}
end;

function TTMSFNCiOSWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCiOSWebBrowser.Create(AValue);
end;

{$IFDEF IOS}
{$IFDEF USESAFARISERVICES}

{ TTMSFNCiOSWebBrowserDelegateSF }

procedure TTMSFNCiOSWebBrowserDelegateSF.safariViewControllerDidFinish(controller: SFSafariViewController);
begin
  controller.dismissViewControllerAnimated(False, nil);
end;
{$ENDIF}

{ TTMSFNCiOSWebBrowserEx }

constructor TTMSFNCiOSWebBrowserEx.Create(AMessageHandler: TTMSFNCiOSWebBrowserScriptMessageHandler);
var
  V: Pointer;
begin
  inherited Create;
  conf := TWKWebViewConfiguration.Wrap(TWKWebViewConfiguration.Wrap(TWKWebViewConfiguration.OCClass.alloc).init);
  conf.setAllowsInlineMediaPlayback(True);
  conf.userContentController.addScriptMessageHandler(ILocalObject(AMessageHandler).GetObjectID, NSStrEx(BridgeName));
  V := WKWebView(Super).initWithFrame(CGRectMake(0, 0, 0, 0), conf);
  if V <> GetObjectID then
    UpdateObjectID(V);
end;

destructor TTMSFNCiOSWebBrowserEx.Destroy;
begin
  conf.release;
  inherited;
end;

function TTMSFNCiOSWebBrowserEx.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(WKWebViewEx);
end;

function TTMSFNCiOSWebBrowserEx.hitTest(point: CGPoint; withEvent: UIEvent): UIView;
begin
  Result := WKWebView(Super).hitTest(point, withEvent);
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    if not FWebBrowser.FWebControl.Enabled then
      Result := nil;
  end;
end;

{ TTMSFNCiOSWebBrowserNavigationDelegate }

procedure TTMSFNCiOSWebBrowserNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(
  WebView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction;
  decisionHandler: Pointer);
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  dh: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
  Params.URL := UTF8ToString(decidePolicyForNavigationAction.request.URL.absoluteString.UTF8String);
  FWebBrowser.FSaveURL := Params.URL;
  Params.Cancel := False;
  if Assigned(FWebBrowser.FWebControl) then
  begin
    FWebBrowser.FURL := Params.URL;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
  end;

  if Assigned(decisionHandler) then
  begin
    @dh := imp_implementationWithBlock(decisionHandler);

    if not Params.Cancel then
      dh(WKNavigationActionPolicyAllow)
    else
      dh(WKNavigationActionPolicyCancel);

    imp_removeBlock(@dh);
  end;
end;

procedure TTMSFNCiOSWebBrowserNavigationDelegate.webViewDidFinishNavigation(
  WebView: WKWebView; didFinishNavigation: WKNavigation);
var
  Params: TTMSFNCCustomWebBrowserNavigateCompleteParams;
begin
  Params.URL := FWebBrowser.FSaveURL;
  if Assigned(FWebBrowser.FWebControl) then
  begin
    FWebBrowser.FURL := Params.URL;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).NavigateComplete(Params);
  end;
end;

{ TTMSFNCiOSWebBrowserScriptMessageHandler }

procedure TTMSFNCiOSWebBrowserScriptMessageHandler.userContentController(
  userContentController: WKUserContentController;
  didReceiveScriptMessage: WKScriptMessage);
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  s: string;
  b: ITMSFNCCustomWebBrowserBridge;
begin
  if not Assigned(FWebBrowser) then
    Exit;
    
  if UTF8ToString(didReceiveScriptMessage.name.UTF8String) = BridgeName then
  begin
    if TNSObject.Wrap(didReceiveScriptMessage.body).isKindOfClass(objc_getClass('NSString')) then
    begin
      Params.URL := UTF8ToString(TNSString.Wrap(didReceiveScriptMessage.body).UTF8String);
      Params.Cancel := False;
      if Assigned(FWebBrowser.FWebControl) then
      begin
        FWebBrowser.FURL := Params.URL;
        TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
      end;
    end;
  end
  else if UTF8ToString(didReceiveScriptMessage.name.UTF8String) = FWebBrowser.FCustomBridge then
  begin
    if TNSObject.Wrap(didReceiveScriptMessage.body).isKindOfClass(objc_getClass('NSString')) then
    begin
      s := UTF8ToString(TNSString.Wrap(didReceiveScriptMessage.body).UTF8String);
      if Assigned(FWebBrowser.FCustomBridgeObject) and Supports(FWebBrowser.FCustomBridgeObject, ITMSFNCCustomWebBrowserBridge, b) then
      begin
        b.ObjectMessage := s;      
      end;
    end;
  end;
end;

{$ENDIF}

{$IFDEF USESAFARISERVICES}
{$IFDEF IOS}
{$IFDEF CPUARM}
procedure ldr_1; cdecl; external libSafariServices;
procedure ldr_2; cdecl; external WebKitFWK;
{$ELSE}
initialization
begin
  msgSafariServices := dlopen(MarshaledAString(libSafariServices), RTLD_LAZY);
  msgWebKit := dlopen(MarshaledAString(WebKitFWK), RTLD_LAZY);
end;

finalization
begin
  dlclose(msgWebKit);
  dlclose(msgSafariServices);
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}


end.
