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

unit FMX.TMSFNCWebBrowser.Android;

{$I FMX.TMSFNCDefines.inc}

interface

{$DEFINE USEJSOBJECT}
{$HINTS OFF}
{$IF COMPILERVERSION > 32}
{$IFDEF ANDROID}
{$DEFINE ANDROIDSUPPORT}
{$ENDIF}
{$IFEND}
{$HINTS ON}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  Classes, Math, SysUtils, FMX.Surfaces, FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.Dialogs, FMX.Types, UITypes, Types, FMX.Forms, FMX.TMSFNCWebBrowser, FMX.Platform,
  Generics.Collections
  {$IFDEF ANDROIDSUPPORT}
  ,FMX.Platform.Android, AndroidApi.JNI.Embarcadero, AndroidApi.JNI.App, AndroidApi.JNIBridge, AndroidApi.JNI.Webkit,
  AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Os, Androidapi.AppGlue, Androidapi.Input, AndroidApi.JNI.GraphicsContentViewText,
  AndroidApi.JNI.Net, AndroidApi.JNI.Widget, FMX.Helpers.Android
  ,AndroidApi.Helpers
  {$ENDIF}
  ;

type
  TTMSFNCAndroidWebBrowser = class;

  TTMSFNCAndroidWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  {$IFDEF ANDROIDSUPPORT}
  JCustomTabsIntent_Builder = interface;
  JCustomTabsIntent = interface;
  JURI = interface;

  JJavaScriptHelperClass = interface(JViewClass)
  ['{E5C1088A-A08E-4880-90EC-D88D44083F2D}']
  end;

  [JavaSignature('JavaScriptHelper')]
  JJavaScriptHelper = interface(JView)
    ['{010FC0F4-11C9-4D11-B356-F67B44757815}']
    function Getjsvalue: JString; cdecl;
    procedure Setjsvalue(Value: JString); cdecl;
    property jsvalue: JString read Getjsvalue write Setjsvalue;
  end;
  TJJavaScriptHelper = class(TJavaGenericImport<JJavaScriptHelperClass, JJavaScriptHelper>) end;

  JURIClass = interface(JObjectClass)
    ['{FB7B3704-4DEC-4796-866D-AB146E4D031F}']
  end;

  [JavaSignature('java/net/URI')]
  JURI = interface(JObject)
    ['{D1FFD342-BFFE-4DE0-9C0E-2BEC3BF75312}']
  end;
  TJURI = class(TJavaGenericImport<JURIClass, JURI>) end;

  JCustomTabsIntent_BuilderClass = interface(JObjectClass)
    ['{034D20C8-72C0-4129-B36D-F79731A2BBD9}']
    {class} function init: JCustomTabsIntent_Builder; cdecl; overload;
  end;

  {$IF COMPILERVERSION > 34}
  [JavaSignature('androidx/browser/customtabs/CustomTabsIntent$Builder')]
  {$ELSE}
  [JavaSignature('android/support/customtabs/CustomTabsIntent$Builder')]
  {$IFEND}
  JCustomTabsIntent_Builder = interface(JObject)
    ['{1219B554-E170-44E2-91F0-321A3ADB61C0}']
    function build: JCustomTabsIntent; cdecl;
  end;
  TJCustomTabsIntent_Builder = class(TJavaGenericImport<JCustomTabsIntent_BuilderClass, JCustomTabsIntent_Builder>) end;

  JCustomTabsIntentClass = interface(JObjectClass)
    ['{5BE926A8-8A77-4C3E-81AF-85E3B5C9C7D2}']
  end;

  {$IF COMPILERVERSION > 34}
  [JavaSignature('androidx/browser/customtabs/CustomTabsIntent')]
  {$ELSE}
  [JavaSignature('android/support/customtabs/CustomTabsIntent')]
  {$IFEND}
  JCustomTabsIntent = interface(JObject)
    ['{70E5A958-73B9-4A56-80F5-7F6754119223}']
    function _Getintent: JIntent; cdecl;
    property intent: JIntent read _Getintent;
    function _GetstartAnimationBundle: JBundle; cdecl;
    property startAnimationBundle: JBundle read _GetstartAnimationBundle;
  end;
  TJCustomTabsIntent = class(TJavaGenericImport<JCustomTabsIntentClass, JCustomTabsIntent>) end;

  JFNCWebChromeClient = interface;

  JFNCWebChromeClientClass = interface(JWebChromeClientClass)
    ['{937DE965-7D1B-439E-8C83-134CC8C3E082}']
    {class} function init: JFNCWebChromeClient; cdecl;
  end;

  [JavaSignature('FNCWebChromeClient')]
  JFNCWebChromeClient = interface(JWebChromeClient)
    ['{30473AA6-1CCA-44AC-AA08-EBD8FA926090}']
  end;
  TJFNCWebChromeClient = class(TJavaGenericImport<JFNCWebChromeClientClass, JFNCWebChromeClient>) end;

  TTMSFNCAndroidWebBrowserListener = class(TJavaLocal, JOnWebViewListener)
  private
    FStopLoading: Boolean;
    FWebBrowser: TTMSFNCAndroidWebBrowser;
  public
    procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
    procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
    procedure onLoadResource(view: JWebView; url: JString); cdecl;
    procedure onPageFinished(view: JWebView; url: JString); cdecl;
    procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
    procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
    procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
    procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
    procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
    procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
    function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
    function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
  end;

  TTMSFNCAndroidWebBrowserClickListener = class(TJavaLocal, JView_OnClickListener)
  private
    FWebBrowser: TTMSFNCAndroidWebBrowser;
  public
    procedure onClick(v: JView); cdecl;
  end;

  TTMSFNCAndroidWebBrowserKeyListener = class(TJavaLocal, JView_OnKeyListener)
  private
    FWebBrowser: TTMSFNCAndroidWebBrowser;
  public   
    function onKey(v: JView; keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  end;

  TTMSFNCAndroidWebBrowserTouchListener = class(TJavaLocal, JView_OnTouchListener)
  private
    FFocusTimer: TTimer;
    FWebBrowser: TTMSFNCAndroidWebBrowser;
  protected
    procedure DoTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
  end;

  TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent = class(TJavaLocal, JValueCallback)
  private
    FWebBrowser: TTMSFNCAndroidWebBrowser;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
    FCallback: TNotifyEvent;
  public
    constructor Create(AWebBrowser: TTMSFNCAndroidWebBrowser; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    procedure onReceiveValue(value: JObject); cdecl;
  end;

  TTMSFNCAndroidWebBrowserJavaScriptCompleteEventList = TObjectList<TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent>;
  {$ENDIF}

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  TTMSFNCAndroidWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser)
  private
    {$IFDEF ANDROIDSUPPORT}
    FCompleteEvents: TTMSFNCAndroidWebBrowserJavaScriptCompleteEventList;
    FWebBrowser: JWebBrowser;
    FCustomBridge: string;
    FCustomBridgeObject: TObject;
    FWebBrowserWrapper: JViewGroup;
    FWebBrowserLayout: JViewGroup;
    FWebBrowserChromeClient: JFNCWebChromeClient;
    FWebBrowserListener: TTMSFNCAndroidWebBrowserListener;
    FWebBrowserTouchListener: TTMSFNCAndroidWebBrowserTouchListener;
    FWebBrowserKeyListener: TTMSFNCAndroidWebBrowserKeyListener;
    {$IFDEF USEJSOBJECT}
    FJSObjectListener: TTMSFNCAndroidWebBrowserClickListener;
    FJSObject: JJavaScriptHelper;
    FCustomJSObject: JJavaScriptHelper;
    {$ENDIF}
    {$ENDIF}
    FURL: string;
    FExternalBrowser: Boolean;
    FWebControl: TTMSFNCCustomWebBrowser;
  protected
    procedure DoExit(Sender: TObject);
    procedure DoEnter(Sender: TObject);
    {$IFDEF ANDROIDSUPPORT}
    function GetScale: Single;
    function GetOffset: TRect;
    {$ENDIF}
    function GetUserAgent: string;
    procedure SetUserAgent(const AValue: string);
    function GetURL: string;
    procedure SetURL(const AValue: string);
    function GetExternalBrowser: Boolean;
    procedure ShowDebugConsole;
    procedure SetCacheFolderName(const Value: string);
    procedure SetCacheFolder(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    function GetEnableAcceleratorKeys: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    procedure SetFocus;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject);
    procedure RemoveBridge(ABridgeName: string);
    procedure Navigate(const AURL: string); overload;
    procedure Navigate; overload;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
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
    procedure CaptureScreenshot;
    procedure BeforeChangeParent;
    procedure Initialize;
    procedure DeInitialize;
    procedure ClearCache;
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
    WebBrowserService := TTMSFNCAndroidWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCAndroidWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowserWrapper) then
    Result := (FWebBrowserWrapper as ILocalObject).GetObjectID;
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
end;

{$IFDEF ANDROIDSUPPORT}
function TTMSFNCAndroidWebBrowser.GetOffset: TRect;
var
  NativeWin: JWindow;
  ContentRect: JRect;
begin
  NativeWin := SharedActivityEx.getWindow;
  if Assigned(NativeWin) then
  begin
    ContentRect := TJRect.Create;
    NativeWin.getDecorView.getDrawingRect(ContentRect);
    Result := Rect(ContentRect.left, ContentRect.top, ContentRect.right, ContentRect.bottom);
  end
  else
    Result := TRect.Empty;
end;

function TTMSFNCAndroidWebBrowser.GetScale: Single;
var
  Screensrv: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSrv)) then
    Result := ScreenSrv.GetScreenScale
  else
    Result := 1;
end;
{$ENDIF}

procedure TTMSFNCAndroidWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
begin
  {$IFDEF ANDROIDSUPPORT}
  {$IFDEF USEJSOBJECT}
  if Assigned(FWebBrowser) and (ABridgeName <> '') and Assigned(ABridgeObject) then
  begin
    FCustomJSObject := TJJavaScriptHelper.Wrap(TJJavaScriptHelper.JavaClass.init(SharedActivityEx));
    FCustomJSObject.setOnClickListener(FJSObjectListener);
    FWebBrowser.addJavascriptInterface(FJSObject, StringToJString(ABridgeName));
    FCustomBridge := ABridgeName;
    FCustomBridgeObject := ABridgeObject;
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.RemoveBridge(ABridgeName: string);
begin
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) and (ABridgeName <> '') then
    FWebBrowser.removeJavascriptInterface(StringToJString(ABridgeName));

  if FCustomBridge = ABridgeName then
  begin
    FCustomBridge := '';
    FCustomBridgeObject := nil;
    FCustomJSObject := nil;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowser.GetBrowserInstance: IInterface;
begin
  Result := nil;
end;

function TTMSFNCAndroidWebBrowser.GetCacheFolder: string;
begin
  Result := '';
end;

function TTMSFNCAndroidWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
end;

function TTMSFNCAndroidWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
end;

function TTMSFNCAndroidWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := False;
end;

function TTMSFNCAndroidWebBrowser.GetEnableContextMenu: Boolean;
begin
  Result := False;
end;

function TTMSFNCAndroidWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  Result := False;
end;

function TTMSFNCAndroidWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

function TTMSFNCAndroidWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCAndroidWebBrowser.GetUserAgent: string;
begin
  Result := '';
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
    Result := JStringToString(FWebBrowser.getSettings.getUserAgentString);
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.GoBack;
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.goBack;
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.GoForward;
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.goForward;
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.Initialize;
{$IFDEF ANDROIDSUPPORT}
var
  vw: JViewGroup;
  frm: TCommonCustomForm;
{$ENDIF}
begin
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
  begin
    if (FWebControl <> nil) and (FWebControl.Root <> nil) and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      frm := TCommonCustomForm(FWebControl.Root.GetObject);
      vw := WindowHandleToPlatform(frm.Handle).FormLayout;
      vw.addView(FWebBrowserWrapper);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCAndroidWebBrowser.LoadFile(AFile: String);
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.loadUrl(StringToJString(AFile));
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.LoadHTML(AHTML: String);
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.loadDataWithBaseURL(StringToJString('x-data://base'),
      StringToJString(AHTML), StringToJString('text/html'), StringToJString('UTF-8'), nil);
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.Navigate(const AURL: string);
begin
  {$IFDEF ANDROIDSUPPORT}
  CallInUIThread(
  procedure
  var
    b: JCustomTabsIntent_Builder;
    cb: JCustomTabsIntent;
    it: JIntent;
  begin
    if FExternalBrowser then
    begin
      b := TJCustomTabsIntent_Builder.JavaClass.init;
      cb := b.build;
      it := cb.intent;
      it.setData(StrToJURI(AURL));
      SharedActivityEx.startActivity(it, cb.startAnimationBundle);
    end
    else
    begin
      if not Assigned(FWebBrowser) then
        Exit;
      FWebBrowser.loadUrl(StringToJString(AURL));
    end;
  end
  );
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
    Result := (FWebBrowser as ILocalObject).GetObjectID;
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCAndroidWebBrowser.Reload;
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.reload;
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.SetCacheFolder(const Value: string);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetCacheFolderName(const Value: string);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetAutoClearCache(const Value: Boolean);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetEnableAcceleratorKeys(
  const AValue: Boolean);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetEnableContextMenu(const AValue: Boolean);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
begin

end;

procedure TTMSFNCAndroidWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
  UpdateBounds;
end;

procedure TTMSFNCAndroidWebBrowser.SetFocus;
begin
end;

procedure TTMSFNCAndroidWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCAndroidWebBrowser.SetUserAgent(const AValue: string);
begin
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
    FWebBrowser.getSettings.setUserAgentString(StringToJString(AValue));
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.ShowDebugConsole;
begin

end;

procedure TTMSFNCAndroidWebBrowser.StopLoading;
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  CallInUIThread(
  procedure
  begin
    FWebBrowser.stopLoading;
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.UpdateBounds;
{$IFDEF ANDROIDSUPPORT}
var
  bd: TRect;
  sc: Single;
  offr: TRect;
  pos: TPointF;
  wlp: JRelativeLayout_LayoutParams;
{$ENDIF}
begin
  {$IFDEF ANDROIDSUPPORT}
  CallInUIThread(
  procedure
  begin
    if Assigned(FWebBrowserWrapper) and Assigned(FWebBrowserWrapper.getParent) and Assigned(FWebControl) and (FWebControl.Root <> nil)
      and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      sc := GetScale;
      offr := GetOffset;

      pos := PointF(0, 0);
      if (FWebControl.Parent is TCommonCustomForm) then
        Pos := TPointF(offr.TopLeft) + FWebControl.Position.Point * sc
      else if Assigned(FWebControl.ParentControl) then
        Pos := FWebControl.ParentControl.LocalToAbsolute(FWebControl.Position.Point) * sc;

      if FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible and FWebControl.ParentedVisible and not FExternalBrowser then
      begin
        bd := Rect(Round(Pos.X), Round(Pos.Y), Round(Pos.X + FWebControl.Width * sc), Round(Pos.Y + FWebControl.Height *
         sc));

        wlp := TJRelativeLayout_LayoutParams.Wrap(FWebBrowserWrapper.getLayoutParams);
        wlp.width := bd.Width;
        wlp.height := bd.Height;
        wlp.leftMargin := bd.Left;
        wlp.topMargin := bd.Top;
        FWebBrowserWrapper.setVisibility(TJView.JavaClass.VISIBLE);
        TThread.ForceQueue(nil, procedure
        begin
          if Assigned(FWebBrowserWrapper) then
            FWebBrowserWrapper.requestLayout;
        end);
      end
      else
        FWebBrowserWrapper.setVisibility(TJView.JavaClass.INVISIBLE);
    end;
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.UpdateEnabled;
begin
end;

procedure TTMSFNCAndroidWebBrowser.UpdateVisible;
begin
  UpdateBounds;
end;

procedure TTMSFNCAndroidWebBrowser.BeforeChangeParent;
begin

end;

function TTMSFNCAndroidWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoBack;
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.canGoForward;
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.CaptureScreenshot;
{$IFDEF ANDROIDSUPPORT}
var
  c: JCanvas;
  bn: JBitmap;
  bmp: TTMSFNCBitmap;

  function JBitmapToBitmap(ABitmap: JBitmap): TTMSFNCBitmap;
  var
    LSurface: TBitmapSurface;
  begin
    LSurface := TBitmapSurface.Create;
    Result := TTMSFNCBitmap.Create;
    try    
      if JBitmapToSurface(ABitmap, LSurface) then
        Result.Assign(LSurface);
    finally
      LSurface.Free;
    end;
  end;

{$ENDIF}
begin
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
  begin
    bn := TJBitmap.JavaClass.createBitmap(FWebBrowser.getWidth, FWebBrowser.getHeight, TJBitmap_Config.JavaClass.ARGB_8888);
    c := TJCanvas.JavaClass.init(bn);
    FWebBrowser.draw(c);

    bmp := JBitmapToBitmap(bn);
    try
      if Assigned(FWebControl) then
        TTMSFNCCustomWebBrowserProtected(FWebControl).DoCaptureScreenShot(bmp);
    finally
      bmp.Free;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.ClearCache;
begin

end;

procedure TTMSFNCAndroidWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

constructor TTMSFNCAndroidWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
{$IFDEF ANDROIDSUPPORT}
var
  wlp: JRelativeLayout_LayoutParams;
{$ENDIF}
begin
  FExternalBrowser := False;
  FWebControl := AWebControl;
  FWebControl.CanFocus := True;
  FWebControl.OnExit := DoExit;
  FWebControl.OnEnter := DoEnter;

  {$IFDEF ANDROIDSUPPORT}
  FCompleteEvents := TTMSFNCAndroidWebBrowserJavaScriptCompleteEventList.Create;

  FWebBrowserListener := TTMSFNCAndroidWebBrowserListener.Create;
  FWebBrowserListener.FWebBrowser := Self;
  FWebBrowserTouchListener := TTMSFNCAndroidWebBrowserTouchListener.Create;
  FWebBrowserTouchListener.FWebBrowser := Self;
  FWebBrowserKeyListener := TTMSFNCAndroidWebBrowserKeyListener.Create;
  FWebBrowserKeyListener.FWebBrowser := Self;

  {$IFDEF USEJSOBJECT}
  FJSObjectListener := TTMSFNCAndroidWebBrowserClickListener.Create;
  FJSObjectListener.FWebBrowser := Self;
  {$ENDIF}

  CallInUIThreadAndWaitFinishing(
  procedure
  begin
    {$HINTS OFF}
    {$IF COMPILERVERSION > 30}
    TJWebBrowser.JavaClass.enableSlowWholeDocumentDraw;
    TJWebBrowser.JavaClass.setWebContentsDebuggingEnabled(True);
    {$IFEND}

    {$IF COMPILERVERSION > 34}
    FWebBrowser := TJWebBrowser.JavaClass.init(MainActivity);
    {$IFEND}
    {$IF COMPILERVERSION <= 34}
    FWebBrowser := TJWebBrowser.JavaClass.init(SharedActivityEx);
    {$IFEND}
    {$HINTS ON}

    FWebBrowser.SetWebViewListener(FWebBrowserListener);
    FWebBrowser.setOnTouchListener(FWebBrowserTouchListener);
    FWebBrowser.setOnKeyListener(FWebBrowserKeyListener);

    FWebBrowserChromeClient := TJFNCWebChromeClient.JavaClass.init;
    FWebBrowser.setWebChromeClient(FWebBrowserChromeClient);

    {$IFDEF USEJSOBJECT}
    FJSObject := TJJavaScriptHelper.Wrap(TJJavaScriptHelper.JavaClass.init(SharedActivityEx));
    FJSObject.setOnClickListener(FJSObjectListener);
    FWebBrowser.addJavascriptInterface(FJSObject, StringToJString('injectedObject'));
    {$ENDIF}

    FWebBrowser.getSettings.setGeolocationEnabled(True);
    FWebBrowser.getSettings.setAppCacheEnabled(True);
    FWebBrowser.getSettings.setDatabaseEnabled(True);
    FWebBrowser.getSettings.setDomStorageEnabled(True);
    FWebBrowser.getSettings.setJavaScriptEnabled(True);
    FWebBrowser.getSettings.setSaveFormData(False);
    FWebBrowser.getSettings.setBuiltInZoomControls(True);
    FWebBrowser.getSettings.setDisplayZoomControls(False);
    FWebBrowser.getSettings.setLoadWithOverviewMode(True);
    FWebBrowser.getSettings.setUseWideViewPort(True);
    FWebBrowser.getSettings.setMediaPlaybackRequiresUserGesture(False);
    FWebBrowser.getSettings.setAllowFileAccess(True);

    FWebBrowserWrapper := TJRelativeLayout.JavaClass.init(SharedActivityEx);
    FWebBrowserLayout := TJRelativeLayout.JavaClass.init(SharedActivityEx);
    wlp := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
    FWebBrowserWrapper.addView(FWebBrowser, wlp);
    wlp := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
    FWebBrowserWrapper.addView(FWebBrowserLayout, wlp);
  end
  );
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.DeInitialize;
begin
  {$IFDEF ANDROIDSUPPORT}
  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.SetWebViewListener(nil);
    FWebBrowser.setOnTouchListener(nil);
    FWebBrowser.setOnKeyListener(nil);
  end;

  {$IFDEF USEJSOBJECT}
  if Assigned(FJSObject) then
  begin
    FJSObject.setOnClickListener(nil);
  end;
  {$ENDIF}

  if Assigned(FCompleteEvents) then
    FCompleteEvents.Free;

  if Assigned(FWebBrowserWrapper) then
    FWebBrowserWrapper := nil;

  if Assigned(FWebBrowserListener) then
  begin
    FWebBrowserListener.Free;
    FWebBrowserListener := nil;
  end;

  if Assigned(FWebBrowserTouchListener) then
  begin
    FWebBrowserTouchListener.Free;
    FWebBrowserTouchListener := nil;
  end;

  if Assigned(FWebBrowserKeyListener) then
  begin
    FWebBrowserKeyListener.Free;
    FWebBrowserKeyListener := nil;
  end;

  {$IFDEF USEJSOBJECT}
  if Assigned(FJSObjectListener) then
  begin
    FJSObjectListener.Free;
    FJSObjectListener := nil;
  end;
  {$ENDIF}

  if Assigned(FWebBrowserChromeClient) then
    FWebBrowserChromeClient := nil;

  if Assigned(FWebBrowserLayout) then
    FWebBrowserLayout := nil;

  {$IFDEF USEJSOBJECT}
  if Assigned(FJSObject) then
    FJSObject := nil;
  {$ENDIF}

  if Assigned(FWebBrowser) then
    FWebBrowser := nil;
  {$ENDIF}
end;

procedure TTMSFNCAndroidWebBrowser.DoEnter(Sender: TObject);
begin
end;

procedure TTMSFNCAndroidWebBrowser.DoExit(Sender: TObject);
begin
end;

procedure TTMSFNCAndroidWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
{$IFDEF ANDROIDSUPPORT}
var
  v: TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent;
{$ENDIF}
begin
  {$IFDEF ANDROIDSUPPORT}
  if not Assigned(FWebBrowser) then
    Exit;

  if Assigned(FWebBrowser) then
  begin
    v := TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent.Create(Self, ACompleteEvent, ACallback);

    FCompleteEvents.Add(v);

    CallInUIThread(
    procedure
    begin
      FWebBrowser.evaluateJavascript(StringToJString(AScript), v);
    end);
  end
  {$ENDIF}
end;

{ TTMSFNCAndroidWebBrowserService }

procedure TTMSFNCAndroidWebBrowserService.DeleteCookies;
begin
  {$IFDEF ANDROIDSUPPORT}
  TJCookieManager.JavaClass.getInstance.removeAllCookie;
  {$ENDIF}
end;

function TTMSFNCAndroidWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCAndroidWebBrowser.Create(AValue);
end;

{$IFDEF ANDROIDSUPPORT}

{ TTMSFNCAndroidWebBrowserListener }

procedure TTMSFNCAndroidWebBrowserListener.doUpdateVisitedHistory(
  view: JWebView; url: JString; isReload: Boolean);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onFormResubmission(view: JWebView;
  dontResend, resend: JMessage);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onLoadResource(view: JWebView;
  url: JString);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onPageFinished(view: JWebView;
  url: JString);
var
  Params: TTMSFNCCustomWebBrowserNavigateCompleteParams;
begin
  if not FStopLoading then
  begin
    Params.URL := JStringToString(url);
    if Assigned(FWebBrowser.FWebControl) then
    begin
      FWebBrowser.FURL := Params.URL;
      TThread.Queue(nil,
      procedure
      begin
        TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).NavigateComplete(Params);
      end
      );
    end;
  end;
end;

procedure TTMSFNCAndroidWebBrowserListener.onPageStarted(view: JWebView;
  url: JString; favicon: JBitmap);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onReceivedError(view: JWebView;
  errorCode: Integer; description, failingUrl: JString);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onReceivedHttpAuthRequest(
  view: JWebView; handler: JHttpAuthHandler; host, realm: JString);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onReceivedSslError(view: JWebView;
  handler: JSslErrorHandler; error: JSslError);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onScaleChanged(view: JWebView;
  oldScale, newScale: Single);
begin

end;

procedure TTMSFNCAndroidWebBrowserListener.onUnhandledKeyEvent(view: JWebView;
  event: JKeyEvent);
begin

end;

function TTMSFNCAndroidWebBrowserListener.shouldOverrideKeyEvent(view: JWebView;
  event: JKeyEvent): Boolean;
begin
  Result := False;
end;

function TTMSFNCAndroidWebBrowserListener.shouldOverrideUrlLoading(
  view: JWebView; url: JString): Boolean;
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
begin
  FStopLoading := False;
  Params.URL := JStringToString(url);
  Params.Cancel := False;
  if Assigned(FWebBrowser.FWebControl) then
  begin
    FWebBrowser.FURL := Params.URL;
    TThread.Synchronize(nil,
    procedure
    begin
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
    end
    );
  end;
  Result := Params.Cancel;
  FStopLoading := Result;
  if Result then
    view.stopLoading;
end;

{ TTMSFNCAndroidWebBrowserClickListener }

procedure TTMSFNCAndroidWebBrowserClickListener.onClick(v: JView);
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  s: string;
  b: ITMSFNCCustomWebBrowserBridge;
begin
  if Assigned(FWebBrowser.FWebControl) and Assigned(FWebBrowser.FCustomJSObject) then
  begin
    s := JStringToString(FWebBrowser.FCustomJSObject.jsvalue);

    TThread.Queue(nil,
    procedure
    begin
      if Assigned(FWebBrowser.FCustomBridgeObject) and (FWebBrowser.FCustomBridge <> '') then
        if Supports(FWebBrowser.FCustomBridgeObject, ITMSFNCCustomWebBrowserBridge, b) then
          b.ObjectMessage := s;
    end
    );
  end;

  Params.Cancel := False;
  {$IFDEF USEJSOBJECT}
  if Assigned(FWebBrowser.FWebControl) and Assigned(FWebBrowser.FJSObject) then
  begin
    s := JStringToString(FWebBrowser.FJSObject.jsvalue);
  {$ELSE}
  if Assigned(FWebBrowser.FWebControl) then
  begin
  {$ENDIF}
    begin
      Params.URL := s;
      FWebBrowser.FURL := Params.URL;
      TThread.Queue(nil,
      procedure
      begin
        TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
      end
      );
    end;
  end;
end;

{ TTMSFNCAndroidWebBrowserTouchListener }

constructor TTMSFNCAndroidWebBrowserTouchListener.Create;
begin
  inherited Create;
  FFocusTimer := TTimer.Create(nil);
  FFocusTimer.Enabled := false;
  FFocusTimer.Interval := 1;
  FFocusTimer.OnTimer := DoTimer;
end;

procedure TTMSFNCAndroidWebBrowserTouchListener.DoTimer(Sender: TObject);
begin
  CallInUIThread(
  procedure
  begin
    if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
      FWebBrowser.FWebControl.SetFocus;
  end
  );

  FFocusTimer.Enabled := False;
end;

destructor TTMSFNCAndroidWebBrowserTouchListener.Destroy;
begin
  FFocusTimer.Free;
  inherited;
end;

function TTMSFNCAndroidWebBrowserTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
var
  b: Boolean;
begin
  Result := False;
  if not Assigned(FWebBrowser) or not Assigned(FWebBrowser.FWebControl) or not Assigned(FWebBrowser.FWebBrowserWrapper) then
    Exit;

  b := FWebBrowser.FWebControl.Enabled;
  if not b then
    Result := True
  else
  begin
    FFocusTimer.Enabled := True;
  end;
end;

function TTMSFNCAndroidWebBrowserKeyListener.onKey(v: JView; keyCode: Integer; event: JKeyEvent): Boolean;
begin
  Result := False;
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) and (keyCode = TJKeyEvent.JavaClass.KEYCODE_BACK) then
  begin
    Result := True;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoHardwareButtonClicked;
  end;
end;

{ TTMSFNCAndroidWebBrowserJavaScriptCallBack }

constructor TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent.Create(AWebBrowser: TTMSFNCAndroidWebBrowser; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
begin
  inherited Create;
  FWebBrowser := AWebBrowser;
  FCompleteEvent := ACompleteEvent;
  FCallback := ACallback;
end;

procedure TTMSFNCAndroidWebBrowserJavaScriptCompleteEvent.onReceiveValue(
  value: JObject);
var
  v: string;
begin
  if Assigned(FCompleteEvent) then
  begin
    v := '';
    if Assigned(value) then
      v := JStringToString(value.toString);

    if Assigned(FCompleteEvent) then
      FCompleteEvent(v);
  end;

  if Assigned(FCallback) then
    FCallback(Self);

  if Assigned(FWebBrowser) then
    FWebBrowser.FCompleteEvents.Remove(Self);
end;

{$ENDIF}

initialization
begin
  {$IFDEF ANDROIDSUPPORT}
  TRegTypes.RegisterType('FMX.TMSFNCWebBrowser.Android.JCustomTabsIntent_Builder', TypeInfo(FMX.TMSFNCWebBrowser.Android.JCustomTabsIntent_Builder));
  TRegTypes.RegisterType('FMX.TMSFNCWebBrowser.Android.JCustomTabsIntent', TypeInfo(FMX.TMSFNCWebBrowser.Android.JCustomTabsIntent));
  TRegTypes.RegisterType('FMX.TMSFNCWebBrowser.Android.JJavascriptHelper', TypeInfo(FMX.TMSFNCWebBrowser.Android.JJavaScriptHelper));
  {$ENDIF}
end;


end.
