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

unit FMX.TMSFNCWebBrowser.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF MACOS}
{$IFNDEF IOS}
uses
  FMX.Platform.Mac, MacApi.AppKit, TypInfo, MacApi.Foundation, MacApi.CocoaTypes, MacApi.ObjectiveC,
  MacApi.ObjcRuntime, MacApi.Helpers;
{$ENDIF}
{$ENDIF}

const
  WebKitFWK: string = '/System/Library/Frameworks/WebKit.framework/WebKit';

  {$IFDEF MACOS}
  {$IFNDEF IOS}
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

  WKWebViewClass = interface(NSViewClass)
    ['{975E5DD2-26EF-4824-9199-6768A1C93D05}']
  end;

  WKWebView = interface(NSView)
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

  WKUIDelegate = interface(IObjectiveC)
    ['{802EAEEC-5DD2-493E-B6A2-58447AA9F72C}']
    //[MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    //function webViewCreateWebViewWithConfigurationForNavigationActionWindowFeatures(WebView: WKWebView;
    //  createWebViewWithConfiguration: WKWebViewConfiguration; forNavigationAction: WKNavigationAction;
    //  windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptAlertPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: pointer); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptConfirmPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: pointer); cdecl;
//    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
//    procedure webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler
//      (WebView: WKWebView; runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString;
//      initiatedByFrame: WKFrameInfo; completionHandler: pointer); cdecl;
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
    function hitTest(aPoint: NSPoint): NSView; cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;
  {$ENDIF}
  {$ENDIF}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  Classes, Math, SysUtils, FMX.Types, Types, FMX.Forms, FMX.TMSFNCWebBrowser, FMX.TMSFNCUtils,
  FMX.TMSFNCTypes, FMX.Edit, UITypes, Generics.Collections
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  ,FMX.KeyMapping, FMX.Platform
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  ,MacApi.KeyCodes
  {$ENDIF}
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
  ;

const
  BridgeName = 'bridge';
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  FixTrivialKeys: array [0..83] of Word = (
    KEY_F1        ,     vkF1,
    KEY_F2        ,     vkF2,
    KEY_F3        ,     vkF3,
    KEY_F4        ,     vkF4,
    KEY_F5        ,     vkF5,
    KEY_F6        ,     vkF6,
    KEY_F7        ,     vkF7,
    KEY_F8        ,     vkF8,
    KEY_F9        ,     vkF9,
    KEY_F10       ,     vkF10,
    KEY_F11       ,     vkF11,
    KEY_F12       ,     vkF12,
    KEY_F13       ,     vkF13,
    KEY_F14       ,     vkF14,
    KEY_F15       ,     vkF15,
    KEY_F16       ,     vkF16,
    KEY_F17       ,     vkF17,
    KEY_F18       ,     vkF18,
    KEY_F19       ,     vkF19,
    KEY_F20       ,     vkF20,
    KEY_TAB       ,     vkTab,
    KEY_INS       ,     vkInsert,
    KEY_DEL       ,     vkDelete,
    KEY_HOME      ,     vkHome,
    KEY_END       ,     vkEnd,
    KEY_PAGUP     ,     vkPrior,
    KEY_PAGDN     ,     vkNext,
    KEY_UP        ,     vkUp,
    KEY_DOWN      ,     vkDown,
    KEY_LEFT      ,     vkLeft,
    KEY_RIGHT     ,     vkRight,
    KEY_NUMLOCK   ,     vkNumLock,
    KEY_PADENTER  ,     vkReturn,
    KEY_BACKSPACE ,     vkBack,
    KEY_ENTER     ,     vkReturn,
    KEY_ESC       ,     vkEscape,
    KEY_LSHIFT    ,     vkLShift,
    KEY_LMENU     ,     vkLMenu,
    KEY_LCONTROL  ,     vkLControl,
    KEY_RSHIFT    ,     vkRShift,
    KEY_RMENU     ,     vkRMenu,
    KEY_RCONTROL  ,     vkRControl);
  {$ENDIF}
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}

{$IFDEF MACOS}
{$IFNDEF IOS}
function imp_implementationWithBlock(block: Pointer): Pointer; cdecl; external libobjc name _PU + 'imp_implementationWithBlock';
function imp_removeBlock(anImp: Pointer): Integer; cdecl; external libobjc name _PU + 'imp_removeBlock';
{$ENDIF}
{$ENDIF}

type
  TTMSFNCMacWebBrowser = class;

  TTMSFNCMacWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  {$IFDEF MACOS}
  {$IFNDEF IOS}
  TTMSFNCMacWebBrowserScriptMessageHandler = class(TOCLocal, WKScriptMessageHandler)
  private
    FWebBrowser: TTMSFNCMacWebBrowser;
  public
    procedure userContentController(userContentController: WKUserContentController;
      didReceiveScriptMessage: WKScriptMessage); cdecl;
  end;

  TTMSFNCMacWebBrowserNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
  private
    FWebBrowser: TTMSFNCMacWebBrowser;
  public
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationActionDecisionHandler(WebView: WKWebView;
      decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(WebView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
  end;

  TTMSFNCMacWebBrowserUIDelegate = class(TOCLocal, WKUIDelegate)
  private
    FWebBrowser: TTMSFNCMacWebBrowser;
  public
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptAlertPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: pointer); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(WebView: WKWebView;
      runJavaScriptConfirmPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: pointer); cdecl;
//    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
//    procedure webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler
//      (WebView: WKWebView; runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString;
//      initiatedByFrame: WKFrameInfo; completionHandler: ); cdecl;
  end;

  TTMSFNCMacWebBrowserEx = class(TOCLocal)
  private
    conf: WKWebViewConfiguration;
    FWebBrowser: TTMSFNCMacWebBrowser;
  public
    constructor Create(AMessageHandler: TTMSFNCMacWebBrowserScriptMessageHandler);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    function hitTest(aPoint: NSPoint): NSView; cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;
  {$ENDIF}
  {$ENDIF}

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  {$IFDEF MACOS}
  {$IFNDEF IOS}
  TTMSFNCMacWebBrowserScript = class
  private
    FWebbrowser: TTMSFNCMacWebBrowser;
    FCallback: TNotifyEvent;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
  protected
    procedure DoEvaluateScriptComplete(param1: Pointer; param2: NSError);
  end;
  {$ENDIF}
  {$ENDIF}

  TTMSFNCMacWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser)
  private
    {$IFDEF MACOS}
    {$IFNDEF IOS}
    FScriptList: TObjectList<TTMSFNCMacWebBrowserScript>;
    FSaveURL: string;
    FWebBrowser: WKWebView;
    FCustomBridge: string;
    FCustomBridgeObject: TObject;
    FWebBrowserLocal: TTMSFNCMacWebBrowserEx;
    FWebBrowserNavigationDelegate: TTMSFNCMacWebBrowserNavigationDelegate;
    FWebBrowserMessageHandler: TTMSFNCMacWebBrowserScriptMessageHandler;
    FWebBrowserUIDelegate: TTMSFNCMacWebBrowserUIDelegate;
    FEdit: TCustomEdit;
    {$ENDIF}
    {$ENDIF}
    FURL: string;
    FExternalBrowser: Boolean;
    FWebControl: TTMSFNCCustomWebBrowser;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    procedure EditEnter(Sender: TObject);

    function GetUserAgent: string;
    procedure SetUserAgent(const AValue: string);
    procedure SetFocus;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    function GetExternalBrowser: Boolean;
    procedure ShowDebugConsole;
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    function GetEnableAcceleratorKeys: Boolean;
    procedure SetCacheFolder(const Value: string);
    procedure SetCacheFolderName(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure Navigate(const AURL: string); overload;
    procedure Navigate; overload;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    {$IFDEF MACOS}
    {$IFNDEF IOS}
    procedure DoTakeSnapshotComplete(param1: Pointer; param2: NSError);
    {$ENDIF}
    {$ENDIF}
    procedure LoadHTML(AHTML: String);
    procedure LoadFile(AFile: String);
    procedure GoForward;
    procedure GoBack;
    procedure Close;
    procedure Reload;
    procedure StopLoading;
    procedure UpdateVisible;
    procedure UpdateEnabled;
    procedure UpdateBounds;
    procedure BeforeChangeParent;
    procedure Initialize;
    procedure DeInitialize;
    procedure CaptureScreenShot;
    procedure ClearCache;
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
    property AutoAutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
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
    WebBrowserService := TTMSFNCMacWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCMacWebBrowser.GetBrowserInstance: IInterface;
begin
  Result := nil;
end;

function TTMSFNCMacWebBrowser.GetCacheFolder: string;
begin
  Result := '';
end;

function TTMSFNCMacWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
end;

function TTMSFNCMacWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
end;

function TTMSFNCMacWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := False;
end;

function TTMSFNCMacWebBrowser.GetEnableContextMenu: Boolean;
begin
  Result := False;
end;

function TTMSFNCMacWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  Result := False;
end;

function TTMSFNCMacWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

function TTMSFNCMacWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCMacWebBrowser.GetUserAgent: string;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  Result := '';
  if Assigned(FWebBrowser) then
    UTF8ToString(FWebBrowser.customUserAgent.UTF8String);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.GoBack;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.goBack;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.GoForward;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.goForward;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.Initialize;
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  vw: NSView;
  frm: TCommonCustomForm;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
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
  {$ENDIF}
end;

function TTMSFNCMacWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCMacWebBrowser.LoadFile(AFile: String);
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  url: NSUrl;
  requestobj: NSURLRequest;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    url := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStrEx(AFile)));
    requestobj := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(url));
    FWebBrowser.loadRequest(requestobj);
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.LoadHTML(AHTML: String);
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.loadHTMLString(NSStrEx(AHTML), nil);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.Navigate(const AURL: string);
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  urlobj: NSURL;
  requestobj: NSURLRequest;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if FExternalBrowser then
  begin
    TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStrEx(AUrl))));
  end
  else
  begin
    if Assigned(FWebBrowser) then
    begin
      urlobj := TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStrEx(AURL)));
      requestobj := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(urlobj));
      FWebBrowser.loadRequest(requestobj);
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function TTMSFNCMacWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    Result := (FWebBrowser as ILocalObject).GetObjectID;
  {$ENDIF}
  {$ENDIF}
end;

function TTMSFNCMacWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
end;

function TTMSFNCMacWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
end;

procedure TTMSFNCMacWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCMacWebBrowser.Reload;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.reload(nil);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.RemoveBridge(ABridgeName: string);
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowserLocal) and Assigned(FWebBrowserLocal.conf) and (ABridgeName <> '') then
    FWebBrowserLocal.conf.userContentController.removeScriptMessageHandlerForName(NSStrEx(ABridgeName));

  if ABridgeName = FCustomBridge then
  begin
    FCustomBridge := '';
    FCustomBridgeObject := nil;
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.SetCacheFolder(const Value: string);
begin

end;

procedure TTMSFNCMacWebBrowser.SetCacheFolderName(const Value: string);
begin

end;

procedure TTMSFNCMacWebBrowser.SetAutoClearCache(const Value: Boolean);
begin

end;

procedure TTMSFNCMacWebBrowser.SetEnableAcceleratorKeys(const AValue: Boolean);
begin

end;

procedure TTMSFNCMacWebBrowser.SetEnableContextMenu(const AValue: Boolean);
begin

end;

procedure TTMSFNCMacWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
begin

end;

procedure TTMSFNCMacWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
end;

procedure TTMSFNCMacWebBrowser.SetFocus;
begin

end;

procedure TTMSFNCMacWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCMacWebBrowser.SetUserAgent(const AValue: string);
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.setCustomUserAgent(NSStrEx(AValue));
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.ShowDebugConsole;
begin

end;

procedure TTMSFNCMacWebBrowser.StopLoading;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.stopLoading(nil);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.UpdateBounds;
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  vw: NSView;
  bd: TRectF;
  frm: TCommonCustomForm;
  frmr: NSRect;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    if (FWebControl <> nil) and (FWebControl.Root <> nil) and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      frm := TCommonCustomForm(FWebControl.Root.GetObject);
      bd := FWebControl.AbsoluteRect;
      vw := WindowHandleToPlatform(frm.Handle).View;
      frmr.origin.x := bd.Left;
      frmr.origin.y := bd.Top;
      frmr.size.width := bd.Width;
      frmr.size.height := bd.Height;
      frmr.origin.y := - frmr.origin.y + vw.frame.size.height - frmr.size.height;
      FWebBrowser.setFrame(frmr);
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.UpdateEnabled;
begin
end;

procedure TTMSFNCMacWebBrowser.UpdateVisible;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) and Assigned(FWebControl) then
    FWebBrowser.setHidden(not (FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible and FWebControl.ParentedVisible));
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowserLocal) and Assigned(FWebBrowserLocal.conf) and (ABridgeName <> '') and Assigned(ABridgeObject) then
  begin    
    FCustomBridge := ABridgeName;
    FCustomBridgeObject := ABridgeObject;
    FWebBrowserLocal.conf.userContentController.addScriptMessageHandler(ILocalObject(FWebBrowserMessageHandler).GetObjectID, NSStrEx(ABridgeName));
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.BeforeChangeParent;
begin

end;

function TTMSFNCMacWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoBack;
  {$ENDIF}
  {$ENDIF}
end;

function TTMSFNCMacWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoForward;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.CaptureScreenShot;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
    FWebBrowser.takeSnapshotWithConfiguration(nil, DoTakeSnapshotComplete);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.ClearCache;
begin

end;

procedure TTMSFNCMacWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

constructor TTMSFNCMacWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
begin
  FWebControl := AWebControl;
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  FScriptList := TObjectList<TTMSFNCMacWebBrowserScript>.Create;
  LoadLibrary(PWideChar(WebKitFWK));
  FWebBrowserMessageHandler := TTMSFNCMacWebBrowserScriptMessageHandler.Create;
  FWebBrowserMessageHandler.FWebBrowser := Self;
  FWebBrowserNavigationDelegate := TTMSFNCMacWebBrowserNavigationDelegate.Create;
  FWebBrowserNavigationDelegate.FWebBrowser := Self;
  FWebBrowserUIDelegate := TTMSFNCMacWebBrowserUIDelegate.Create;
  FWebBrowserUIDelegate.FWebBrowser := Self;
  FWebBrowserLocal := TTMSFNCMacWebBrowserEx.Create(FWebBrowserMessageHandler);
  FWebBrowserLocal.FWebBrowser := Self;
  FWebBrowser := WKWebView(FWebBrowserLocal.Super);
  FWebBrowser.setNavigationDelegate(ILocalObject(FWebBrowserNavigationDelegate).GetObjectID);
  FWebBrowser.setUIDelegate(ILocalObject(FWebBrowserUIDelegate).GetObjectID);

  FEdit := TCustomEdit.Create(AWebControl);
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.OnEnter := EditEnter;
  FEdit.Parent := AWebControl;
  FEdit.Opacity := 0;
  FEdit.Stored := False;

  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCMacWebBrowser.DeInitialize;
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.setNavigationDelegate(nil);
    FWebBrowser.setUIDelegate(nil);
  end;

  if Assigned(FWebBrowserMessageHandler) then
  begin
    FWebBrowserMessageHandler.Free;
    FWebBrowserMessageHandler := nil;
  end;

  if Assigned(FWebBrowserNavigationDelegate) then
  begin
    FWebBrowserNavigationDelegate.Free;
    FWebBrowserNavigationDelegate := nil;
  end;

  if Assigned(FWebBrowserUIDelegate) then
  begin
    FWebBrowserUIDelegate.Free;
    FWebBrowserUIDelegate := nil;
  end;

  if Assigned(FWebBrowserLocal) then
  begin
    FWebBrowserLocal.Free;
    FWebBrowserLocal := nil;
  end;

  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.removeFromSuperview;
    FWebBrowser.release;
    FWebBrowser := nil;
  end;

  if Assigned(FEdit) then
  begin
    FEdit.Free;
    FEdit := nil;
  end;

  if Assigned(FScriptList) then
  begin
    FScriptList.Free;
    FScriptList := nil;
  end;
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF MACOS}
{$IFNDEF IOS}
procedure TTMSFNCMacWebBrowser.DoTakeSnapshotComplete(param1: Pointer; param2: NSError);
var
  img: NSImage;
  bmp: TTMSFNCBitmap;

  function UnscaledBitmapImageRep(AImage: NSImage): NSBitmapImageRep;
  var
    rep: NSBitmapImageRep;
    s: NSString;
    pt: NSPoint;
  begin
    s := CocoaNSStringConst('/System/Library/Frameworks/AppKit.framework/AppKit', 'NSDeviceRGBColorSpace');
    rep := TNSBitmapImageRep.Wrap(TNSBitmapImageRep.Wrap(TNSBitmapImageRep.OCClass.alloc).
      initWithBitmapDataPlanes(nil, Round(AImage.size.width), Round(AImage.size.height), 8, 4, True, False, s, 0, 0));

    rep.setSize(AImage.size);

    TNSGraphicsContext.OCClass.saveGraphicsState;
    TNSGraphicsContext.OCClass.setCurrentContext(TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.graphicsContextWithBitmapImageRep(rep)));
    pt.x := 0;
    pt.y := 0;
    AImage.drawAtPoint(pt, MakeNSRect(0, 0, 0, 0), NSCompositeSourceOver, 1.0);
    TNSGraphicsContext.OCClass.restoreGraphicsState;
    Result := rep;
  end;

  function BitmapFromImage(AImage: NSImage): TTMSFNCBitmap;
  var
    dt: NSData;
    ms: TMemoryStream;
    imgrep: NSBitmapImageRep;
  begin
    Result := nil;
    if not Assigned(AImage) then
     Exit;

    imgrep := UnscaledBitmapImageRep(AImage);
    dt := imgrep.representationUsingType(NSPNGFileType, nil);
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
    if TNSObject.Wrap(param1).isKindOfClass(objc_getClass('NSImage')) then
    begin
      img := TNSImage.Wrap(param1);
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

procedure TTMSFNCMacWebBrowserScript.DoEvaluateScriptComplete(param1: Pointer; param2: NSError);
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
{$ENDIF}

procedure TTMSFNCMacWebBrowser.EditEnter(Sender: TObject);
begin
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).keyWindow) and Assigned(FWebBrowser) then
    TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).keyWindow.makeFirstResponder(FWebBrowser);
  {$ENDIF}
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
end;

procedure TTMSFNCMacWebBrowser.EditExit(Sender: TObject);
begin
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).keyWindow) then
    TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).keyWindow.makeFirstResponder(nil);
  {$ENDIF}
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
end;

procedure TTMSFNCMacWebBrowser.EditKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  inherited;
  Key := vkA;
end;

procedure TTMSFNCMacWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  s: TTMSFNCMacWebBrowserScript;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FWebBrowser) then
  begin
    s := TTMSFNCMacWebBrowserScript.Create;
    s.FCompleteEvent := ACompleteEvent;
    s.FCallback := ACallback;
    s.FWebbrowser := Self;
    FWebBrowser.evaluateJavaScript(NSStrEx(AScript), s.DoEvaluateScriptComplete);
  end;
  {$ENDIF}
  {$ENDIF}
end;

{ TTMSFNCMacWebBrowserService }

procedure TTMSFNCMacWebBrowserService.DeleteCookies;
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  storage: NSHTTPCookieStorage;
  I: Integer;
  cnt: Integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MACOS}
{$IFNDEF IOS}
  storage := TNSHTTPCookieStorage.Wrap(TNSHTTPCookieStorage.OCClass.sharedHTTPCookieStorage);
  cnt := storage.cookies.count;
  for I := cnt - 1 downto 0 do
    storage.deleteCookie(TNSHTTPCookie.Wrap(storage.cookies.objectAtIndex(I)));

  TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults).synchronize;
{$ENDIF}
{$ENDIF}
end;

function TTMSFNCMacWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCMacWebBrowser.Create(AValue);
end;

{$IFDEF MACOS}
{$IFNDEF IOS}

{ TTMSFNCMacWebBrowserEx }

constructor TTMSFNCMacWebBrowserEx.Create(AMessageHandler: TTMSFNCMacWebBrowserScriptMessageHandler);
var
  V: Pointer;
begin
  inherited Create;
  conf := TWKWebViewConfiguration.Wrap(TWKWebViewConfiguration.Wrap(TWKWebViewConfiguration.OCClass.alloc).init);
  conf.userContentController.addScriptMessageHandler(ILocalObject(AMessageHandler).GetObjectID, NSStrEx(BridgeName));
  V := WKWebView(Super).initWithFrame(MakeNSRect(0, 0, 0, 0), conf);
  if V <> GetObjectID then
    UpdateObjectID(V);
end;

destructor TTMSFNCMacWebBrowserEx.Destroy;
begin
  conf.release;
  inherited;
end;

function TTMSFNCMacWebBrowserEx.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(WKWebViewEx);
end;

procedure TTMSFNCMacWebBrowserEx.keyDown(event: NSEvent); cdecl;
var
  p: Boolean;
{$HINTS OFF}
{$IF COMPILERVERSION >= 34}
  k: Word;
  km: IFMXKeyMappingService;
  ki: TKeyKind;
  s: string;
{$IFEND}
{$HINTS ON}
begin
  p := false;
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 34}
    if TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService, km) then
    begin
      k := km.PlatformKeyToVirtualKey(event.keyCode, ki);
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoKeyPressed(k);

      if k <> 0 then
      begin
        if (event.modifierFlags and NSDeviceIndependentModifierFlagsMask) = NSCommandKeyMask then
        begin
          s := LowerCase(UTF8ToString(event.charactersIgnoringModifiers.UTF8String));
          if s = 'a' then
          begin
            FWebBrowser.FWebBrowser.selectAll(nil);
            p := True;
          end
          else if (s = 'c') and (FWebBrowser.FWebBrowser.respondsToSelector(sel_getUid('copy:'))) then
          begin
            objc_msgSend((FWebBrowser.FWebBrowser as ILocalObject).GetObjectID, sel_getUid('copy:'));
            p := True;
          end
          else if (s = 'x') and (FWebBrowser.FWebBrowser.respondsToSelector(sel_getUid('cut:'))) then
          begin
            objc_msgSend((FWebBrowser.FWebBrowser as ILocalObject).GetObjectID, sel_getUid('cut:'));
            p := True;
          end
          else if (s = 'v') and (FWebBrowser.FWebBrowser.respondsToSelector(sel_getUid('paste:'))) then
          begin
            objc_msgSend((FWebBrowser.FWebBrowser as ILocalObject).GetObjectID, sel_getUid('paste:'));
            p := True;
          end
          else if (s = 'z') and (FWebBrowser.FWebBrowser.respondsToSelector(sel_getUid('undo'))) then
          begin
            objc_msgSend((FWebBrowser.FWebBrowser as ILocalObject).GetObjectID, sel_getUid('undo'));
            p := True;
          end;
        end;
      end;
    end;
    {$IFEND}
    {$HINTS OFF}
  end;

  if not p then
    WKWebView(Super).keyDown(event);
end;

function TTMSFNCMacWebBrowserEx.hitTest(aPoint: NSPoint): NSView;
begin
  Result := WKWebView(Super).hitTest(aPoint);
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    if not FWebBrowser.FWebControl.Enabled then
      Result := nil;
  end;
end;

function TTMSFNCMacWebBrowserEx.resignFirstResponder: Boolean;
{$HINTS OFF}
{$IF COMPILERVERSION >= 34}
var
  I: Integer;
  KeyMappingService: IFMXKeyMappingService;
{$IFEND}
{$HINTS ON}
begin
  Result := WKWebView(Super).resignFirstResponder;
  if not Assigned(FWebBrowser) then
    Exit;

  if Assigned(FWebBrowser.FEdit) then
    FWebBrowser.FEdit.ResetFocus;

  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  for I := 0 to (Length(FixTrivialKeys) - 1) div 2 do
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService, KeyMappingService) then
    begin
      KeyMappingService.UnregisterKeyMapping(FixTrivialKeys[I * 2]);
      KeyMappingService.RegisterKeyMapping(FixTrivialKeys[I * 2], FixTrivialKeys[I * 2 + 1], TKeyKind.Functional);
    end;
  end;
  {$IFEND}
  {$HINTS ON}
end;

function TTMSFNCMacWebBrowserEx.becomeFirstResponder: Boolean;
{$HINTS OFF}
{$IF COMPILERVERSION >= 34}
var
  I: Integer;
  KeyMappingService: IFMXKeyMappingService;
{$IFEND}
{$HINTS ON}
begin
  Result := WKWebView(Super).becomeFirstResponder;
  if not Assigned(FWebBrowser) then
    Exit;

  if Assigned(FWebBrowser.FEdit) then
    FWebBrowser.FEdit.SetFocus;

  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  for I := 0 to (Length(FixTrivialKeys) - 1) div 2 do
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService, KeyMappingService) then
    begin
      KeyMappingService.UnregisterKeyMapping(FixTrivialKeys[I * 2]);
      KeyMappingService.RegisterKeyMapping(FixTrivialKeys[I * 2], 0, TKeyKind.Usual);
    end;
  end;
  {$IFEND}
  {$HINTS ON}
end;


{ TTMSFNCMacWebBrowserNavigationDelegate }

procedure TTMSFNCMacWebBrowserNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(
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

procedure TTMSFNCMacWebBrowserNavigationDelegate.webViewDidFinishNavigation(
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

{ TTMSFNCMacWebBrowserScriptMessageHandler }

procedure TTMSFNCMacWebBrowserScriptMessageHandler.userContentController(
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

{ TTMSFNCMacWebBrowserUIDelegate }

procedure TTMSFNCMacWebBrowserUIDelegate.webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(
  WebView: WKWebView; runJavaScriptAlertPanelWithMessage: NSString;
  initiatedByFrame: WKFrameInfo; completionHandler: pointer);
var
  alert: NSAlert;
  LCompletionHandlerBlock: procedure; cdecl;
begin
  alert := TNSAlert.Create;
  try
    alert.setInformativeText(runJavaScriptAlertPanelWithMessage);
    alert.addButtonWithTitle(StrToNSStr('OK'));
    alert.runModal;
  finally
    alert.release;
    @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
    LCompletionHandlerBlock();
    imp_removeBlock(@LCompletionHandlerBlock);
  end;
end;

procedure TTMSFNCMacWebBrowserUIDelegate.webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(
  WebView: WKWebView; runJavaScriptConfirmPanelWithMessage: NSString;
  initiatedByFrame: WKFrameInfo; completionHandler: pointer);
var
  alert: NSAlert;
  res: NSInteger;
  LCompletionHandlerBlock: procedure(AValue: Boolean); cdecl;
begin
  res := 0;
  alert := TNSAlert.Create;
  try
    alert.setInformativeText(runJavaScriptConfirmPanelWithMessage);
    alert.addButtonWithTitle(StrToNSStr('OK'));
    alert.addButtonWithTitle(StrToNSStr('Cancel'));
    res := alert.runModal;
  finally
    alert.release;
    @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
    LCompletionHandlerBlock(res = NSAlertFirstButtonReturn);
    imp_removeBlock(@LCompletionHandlerBlock);
  end;
end;

//procedure TTMSFNCMacWebBrowserUIDelegate.webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler(
//  WebView: WKWebView; runJavaScriptTextInputPanelWithPrompt,
//  defaultText: NSString; initiatedByFrame: WKFrameInfo;
//  completionHandler: pointer);
//var
//  alert: NSAlert;
//  inputFrame: NSRect;
//  textField: NSTextField;
//  res: NSInteger;
//  LCompletionHandlerBlock: procedure(AValue: NSString); cdecl;
//begin
//  res := 0;
//  alert := TNSAlert.Create;
//  try
//    alert.setInformativeText(runJavaScriptTextInputPanelWithPrompt);
//    alert.addButtonWithTitle(StrToNSStr('OK'));
//    alert.addButtonWithTitle(StrToNSStr('Cancel'));
//    inputFrame := NSMakeRect(0, 0, 300, 24);
//    textField := TNSTextField.Wrap(TNSTextField.Alloc.initWithFrame(inputFrame));
//    textField.setPlaceholderString(defaultText);
//    alert.setAccessoryView(textField);
//    res := alert.runModal;
//  finally
//    alert.release;
//    @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
//    if res = NSAlertFirstButtonReturn then
//      LCompletionHandlerBlock(textField.stringValue)
//    else
//      LCompletionHandlerBlock(nil);
//    imp_removeBlock(@LCompletionHandlerBlock);
//  end;
//end;

{$ENDIF}
{$ENDIF}

end.
