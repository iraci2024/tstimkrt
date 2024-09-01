{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020 - 2022                               }
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

unit FMX.TMSFNCWebBrowser.Unix;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF LINUX}
type
  GTKWebView = Pointer;
  GTKWebKitWebInspector = Pointer;
  GTKWidget = Pointer;
  GTKFixed = Pointer;
  GTKFrame = Pointer;
  GTKContainer = Pointer;
  GTKWindow = Pointer;
  GTKBox = Pointer;
  GTKOrientation = Integer;
  GTKShadowType = Integer;
  GTKWindowType = Integer;
  GTKWebViewSettings = Pointer;
  GTKBin = Pointer;
  GTKWEBKitLoadEvent = Integer;
  {$IFDEF FMXLIB}
  TGTKHandle = THandle;
  {$ENDIF}
  {$IFDEF LCLLIB}
  TGTKHandle = TLibHandle;
  {$ENDIF}

const
  WebKitGTKLib = 'libwebkit2gtk-4.0.so';
  GTKLib = 'libgtk-3.so';
  SoupLib = 'libsoup-2.4.so';
  GLibGTKLib = 'libglib-2.0.so';
  JavaScriptCoreGTKLib = 'libjavascriptcoregtk-4.0.so';
  GObjectGTKLib = 'libgobject-2.0.so';
  GTK_ORIENTATION_HORIZONTAL = 0;
  GTK_ORIENTATION_VERTICAL = 1;
  GTK_SHADOW_NONE = 0;
  GTK_SHADOW_IN = 1;
  GTK_SHADOW_OUT = 2;
  GTK_SHADOW_ETCHED_IN = 3;
  GTK_SHADOW_ETCHED_OUT = 4;
  GTK_WINDOW_TOPLEVEL = 0;
  GTK_WINDOW_POPUP = 1;
  WEBKIT_LOAD_STARTED = 0;
  WEBKIT_LOAD_REDIRECTED = 1;
  WEBKIT_LOAD_COMMITTED = 2;
  WEBKIT_LOAD_FINISHED = 3;
  WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION = 0;
  WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION = 1;
  WEBKIT_POLICY_DECISION_TYPE_RESPONSE = 2;
  WEBKIT_NAVIGATION_TYPE_LINK_CLICKED = 0;
  WEBKIT_NAVIGATION_TYPE_FORM_SUBMITTED = 1;
  WEBKIT_NAVIGATION_TYPE_BACK_FORWARD = 2;
  WEBKIT_NAVIGATION_TYPE_RELOAD = 3;
  WEBKIT_NAVIGATION_TYPE_FORM_RESUBMITTED = 4;
  WEBKIT_NAVIGATION_TYPE_OTHER = 5;

var
  SoupLoaded: Boolean = False;
  SoupHandle: TGTKHandle;
  SoupDLLPath: string = '';
  WebKitGTKLoaded: Boolean = False;
  WebKitGTKHandle: TGTKHandle;
  WEBKitGTKDLLPath: string = '';
  GTKLoaded: Boolean = False;
  GTKHandle: TGTKHandle;
  GTKDLLPath: string = '';
  GLibGTKLoaded: Boolean = False;
  GLibGTKHandle: TGTKHandle;
  GLibGTKDLLPath: string = '';
  GObjectGTKLoaded: Boolean = False;
  GObjectGTKHandle: TGTKHandle;
  GObjectGTKDLLPath: string = '';
  JavaScriptCoreGTKLoaded: Boolean = False;
  JavaScriptCoreGTKHandle: TGTKHandle;
  JavaScriptCoreGTKDLLPath: string = '';

  webkit_web_view_get_type: function: NativeUInt; cdecl;
  webkit_javascript_result_get_value: function(value: Pointer): Pointer; cdecl;
  webkit_javascript_result_get_global_context: function(value: Pointer): Pointer; cdecl;
  webkit_javascript_result_unref: procedure(value: Pointer); cdecl;
  webkit_javascript_result_get_js_value: function(value: Pointer): Pointer; cdecl;
  webkit_website_policies_new_with_policies: function(first_policy_name: PUTF8Char; p1: NativeUInt; p2: Pointer): Pointer; cdecl;
  webkit_web_view_new: function: GTKWebView; cdecl;
  webkit_web_view_load_html: procedure(web_view: GTKWebView; contents: PUTF8Char; base_uri: PUTF8Char); cdecl;
  webkit_web_view_load_plain_text: procedure(web_view: GTKWebView; content: PUTF8Char); cdecl;
  webkit_web_view_can_go_back: function(web_view: GTKWebView): boolean; cdecl;
  webkit_web_view_load_uri: procedure(web_view: GTKWebView; uri: PUTF8Char); cdecl;
  webkit_web_view_load_bytes: procedure(web_view: GTKWebView; bytes: Pointer; mime_type: PUTF8Char; encoding: PUTF8Char; base_uri: PUTF8Char); cdecl;
  webkit_web_view_go_forward: procedure(web_view: GTKWebView); cdecl;
  webkit_web_view_go_back: procedure(web_view: GTKWebView); cdecl;
  webkit_web_view_can_go_forward: function(web_view: GTKWebView): boolean; cdecl;
  webkit_web_view_reload: procedure(web_view: GTKWebView); cdecl;
  webkit_web_view_stop_loading: procedure(web_view: GTKWebView); cdecl;
  webkit_web_view_get_uri: function(web_view: GTKWebView): PUTF8Char; cdecl;
  webkit_web_view_run_javascript_finish: function(web_view: GTKWebView; result, error: Pointer): Pointer; cdecl;
  webkit_web_view_run_javascript: function(web_view: GTKWebView; script: PUTF8Char; cancellable, callback, user_data: Pointer): Pointer; cdecl;
  webkit_web_view_get_settings: function(web_view: GTKWebView): GTKWebViewSettings; cdecl;
  webkit_web_view_set_settings: procedure(web_view: GTKWebView; settings: GTKWebViewSettings); cdecl;
  webkit_settings_set_allow_file_access_from_file_urls: procedure(settings: GTKWebViewSettings; allowed: Boolean); cdecl;
  webkit_settings_get_allow_file_access_from_file_urls: function(settings: GTKWebViewSettings): Boolean; cdecl;
  webkit_settings_get_user_agent: function(settings: GTKWebViewSettings): PUTF8Char; cdecl;
  webkit_settings_set_user_agent: procedure(settings: GTKWebViewSettings; user_agent: PUTF8Char); cdecl;
  webkit_web_view_get_inspector: function (web_view: GTKWebView): GTKWebKitWebInspector; cdecl;
	webkit_web_inspector_get_web_view: function(inspector: GTKWebKitWebInspector): Pointer; cdecl;
	webkit_web_inspector_show: procedure(inspector: GTKWebKitWebInspector); cdecl;
	webkit_web_inspector_close: procedure(inspector: GTKWebKitWebInspector); cdecl;
  webkit_settings_set_enable_developer_extras: procedure(settings: GTKWebViewSettings; enabled: Boolean); cdecl;
  webkit_settings_get_enable_developer_extras: function(settings: GTKWebViewSettings): Boolean; cdecl;
  webkit_web_view_get_user_content_manager: function(web_view: Pointer): Pointer; cdecl;
  webkit_user_content_manager_register_script_message_handler: function(manager: Pointer; name: PUTF8Char): Boolean; cdecl;
  webkit_user_content_manager_unregister_script_message_handler: function(manager: Pointer; name: PUTF8Char): Boolean; cdecl;
  webkit_web_view_get_context: function(web_view: Pointer): Pointer; cdecl;
  webkit_navigation_policy_decision_get_navigation_action: function(decision: Pointer): Pointer; cdecl;
  webkit_navigation_action_get_request: function(navigation: Pointer): Pointer; cdecl;
  webkit_navigation_action_get_navigation_type: function(navigation: Pointer): Integer; cdecl;
  webkit_web_view_download_uri: function(web_view: GTKWebView; uri: PUTF8Char): Pointer; cdecl;
  webkit_uri_request_get_uri: function(request: Pointer): PUTF8Char; cdecl;
  webkit_uri_request_get_http_headers: function(request: Pointer): Pointer; cdecl;
  webkit_response_policy_decision_is_mime_type_supported: function(decision: Pointer): Boolean; cdecl;
  webkit_policy_decision_download: procedure(decision: Pointer); cdecl;
  webkit_policy_decision_use: procedure(decision: Pointer); cdecl;
  webkit_download_set_destination: procedure(download: Pointer; uri: PUTF8Char); cdecl;
  webkit_download_cancel: procedure(download: Pointer); cdecl;
  webkit_download_get_request: function(download: Pointer): Pointer; cdecl;
  webkit_download_get_received_data_length: function(download: Pointer): UInt64; cdecl;

  JSStringGetLength: function(str: Pointer): LongWord; cdecl;
  JSValueToStringCopy: function(ctx: Pointer; value: Pointer; exception: Pointer): Pointer; cdecl;
  JSStringGetMaximumUTF8CStringSize: function(str: Pointer): UInt64; cdecl;
  JSStringGetUTF8CString: function(str: Pointer; buffer: PUTF8Char; bufferSize: UInt64): UInt64; cdecl;
  JSStringGetCharactersPtr: function(&string: Pointer): PChar; cdecl;
  JSValueIsString: function(ctx: Pointer; value: Pointer): LongBool; cdecl;
  JSStringRelease: procedure(&string: Pointer); cdecl;
  JSStringCreateWithUTF8CString: function(&string: PUTF8Char): Pointer; cdecl;
  JSContextGroupCreate: function: Pointer; cdecl;
  JSGlobalContextCreateInGroup: function(group: Pointer; globalObjectClass: Pointer): Pointer; cdecl;
  JSContextGetGlobalObject: function(ctx: Pointer): Pointer; cdecl;
  JSObjectSetProperty: procedure(ctx: Pointer; &object: Pointer; propertyName: Pointer; value: Pointer; attributes: LongWord; exception: PPointer); cdecl;
  JSObjectMakeFunctionWithCallback: function(ctx: Pointer; name: Pointer; callAsFunction: Pointer): Pointer; cdecl;
  JSValueMakeUndefined: function(ctx: Pointer): Pointer; cdecl;
  jsc_value_to_string: function(str: Pointer): PUTF8Char; cdecl;

  g_malloc: function(n_bytes: UInt64): Pointer; cdecl;
  g_free: procedure(mem: Pointer); cdecl;
  g_bytes_new: function(data: Pointer; size: NativeUInt): Pointer; cdecl;
  g_bytes_unref: procedure(bytes: Pointer); cdecl;
  g_bytes_get_size: function(bytes: Pointer): NativeUInt; cdecl;
  g_list_nth: function(list: Pointer; n: Integer): Pointer; cdecl;
  g_list_nth_data: function(list: Pointer; n: Integer): Pointer; cdecl;
  g_list_length: function(list: Pointer): NativeUInt; cdecl;

  g_signal_connect_data: function(instance: Pointer; detailed_signal: PUTF8Char; c_handler: Pointer; data: Pointer; destroy_data: Pointer; connect_flags: Integer): NativeUInt; cdecl;
  g_object_new: function(g: Integer; fp1: PUTF8Char; p1: Pointer; p2: Pointer): Pointer; cdecl;
  g_object_ref: function(&object: Pointer): Pointer; cdecl;
  g_object_unref: function(&object: Pointer): Pointer; cdecl;
  g_type_check_instance_is_a: function(instance: Pointer; g_type: NativeUInt): Boolean; cdecl;

  soup_message_headers_get_one: function(hdrs: Pointer; name: PUTF8Char): PUTF8Char; cdecl;
  soup_message_headers_get_list: function(hdrs: Pointer; name: PUTF8Char): PUTF8Char; cdecl;
  soup_message_headers_get_content_length: function(hdrs: Pointer): Int64; cdecl;
  soup_message_headers_get_encoding: function(hdrs: Pointer): Integer; cdecl;

  gtk_widget_get_snapshot: function(widget: GTKWidget; clip_rect: Pointer): Pointer; cdecl;
  gtk_widget_set_size_request: procedure(widget: GTKWidget; width: Integer; height: Integer); cdecl;
  gtk_widget_show: procedure(widget: GTKWidget); cdecl;
  gtk_widget_destroy: procedure(widget: GTKWidget); cdecl;
  gtk_widget_set_visible: procedure(widget: GTKWidget; visible: Boolean); cdecl;
  gtk_scrolled_window_new: function(hadjustment, vadjustment: Pointer): Pointer; cdecl;
  gtk_box_new: function(orientation: GTKOrientation; spacing: Integer): Pointer; cdecl;
  gtk_container_add: procedure(container: Pointer; widget: GTKWidget); cdecl;
  gtk_container_remove: procedure(container: Pointer; widget: GTKWidget); cdecl;
  gtk_dialog_get_content_area: function(dialog: Pointer): Pointer; cdecl;
  gtk_window_get_default_widget: function: Pointer; cdecl;
  gtk_widget_set_parent: procedure(widget: GTKWidget; parent: GTKWidget); cdecl;
  gtk_widget_set_parent_window: procedure(widget: GTKWidget; parent: GTKWindow); cdecl;
  gtk_window_set_title: procedure(window: GTKWindow; title: PUTF8Char); cdecl;
  gtk_box_pack_start: procedure(box: GTKBox; child: GTKWidget; expand: Boolean; fill: Boolean; padding: Integer); cdecl;
  gtk_frame_new: function(&label: PUTF8Char): Pointer; cdecl;
  gtk_window_new: function(&type: GTKWindowType): Pointer; cdecl;
  gtk_frame_set_shadow_type: procedure(frame: GTKFrame; &type: GTKShadowType); cdecl;
  gtk_widget_show_all: procedure(widget: GTKWidget); cdecl;
  gtk_widget_hide: procedure(widget: GTKWidget); cdecl;
  gtk_bin_get_child: function(bin: GTKBin): Pointer; cdecl;
  gtk_window_set_decorated: procedure(window: GTKWindow; setting: Boolean); cdecl;
  gtk_window_set_transient_for: procedure(window: GTKWindow; parent: GTKWindow); cdecl;
  gtk_window_move: procedure(window: GTKWindow; x: Integer; y: Integer); cdecl;
  gtk_window_get_position: procedure(window: GTKWindow; var root_x: Integer; var root_y: Integer); cdecl;
  gtk_window_get_size: procedure(window: GTKWindow; var width: Integer; var height: Integer); cdecl;
  gtk_window_set_default_size: procedure(window: GTKWindow; width: Integer; height: Integer); cdecl;
  gtk_window_resize: procedure(window: GTKWindow; width: Integer; height: Integer); cdecl;
  gtk_window_set_keep_above: procedure(window: GTKWindow; setting: Boolean); cdecl;
  gtk_window_set_attached_to: procedure(window: GTKWindow; attach_widget: GTKWidget); cdecl;
  gtk_dialog_new: function: Pointer; cdecl;
  gtk_widget_get_parent: function(widget: GTKWidget): Pointer; cdecl;
  gtk_overlay_new: function: Pointer; cdecl;
  gtk_container_get_children: function(container: GTKContainer): Pointer; cdecl;
  gtk_fixed_move: procedure(fixed: GTKFixed; widget: GTKWidget; x: Integer; y: Integer); cdecl;
  gtk_fixed_get_type: function: NativeUInt; cdecl;
  gtk_bin_get_type: function: NativeUInt; cdecl;

{$ENDIF}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;
{$IFDEF LINUX}
procedure InitializeWebKitGTK;
procedure UninitializeWebKitGTK;
{$ENDIF}

var
  INITIALIZEAUTOPLAY: Boolean = FALSE;

implementation

uses
  Classes, Math, FMX.TMSFNCTypes, FMX.Forms, Types, SysUtils, FMX.TMSFNCUtils,
  FMX.TMSFNCWebBrowser, FMX.TMSFNCPersistence
  {$IFNDEF LCLLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl, Controls, FileUtil, LCLType, ExtCtrls
  {$IFDEF LCLGTK3}
  ,lazgtk3, gtk3widgets, glib2, lazgdk3, gtk3wscontrols, WSLCLClasses
  {$ENDIF}
  {$IFDEF LINUX}
  ,dl
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  ,FMX.Platform.Linux
  {$ENDIF}
  {$ENDIF}
  ;

const
  BridgeName = 'bridge';

type
  TTMSFNCUnixWebBrowserService = class;

  TTMSFNCUnixWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  TTMSFNCUnixWebBrowser = class;

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  {$IFDEF LINUX}
  TTMSFNCUnixWebBrowserScript = class
  private
    FWebView: GTKWebView;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
    FCallback: TNotifyEvent;
  end;
  {$ENDIF}

  TTMSFNCUnixWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser)
  private
    {$IFDEF LINUX}
    {$IFDEF LCLGTK3}
    FInitializeTimer: TTimer;
    {$ENDIF}
    FCustomBridge: string;
    FCustomBridgeObject: TObject;
    FStoppedLoading: Boolean;
    FSaveURL: string;
    FWebView: GTKWebView;
    FWindowFixed: GTKFixed;
    {$ENDIF}
    FEnableContextMenu: Boolean;
    FEnableAcceleratorKeys: Boolean;
    FBlockEvents: Boolean;
    FExternalBrowser: Boolean;
    FURL: string;
    FWebControl: TTMSFNCCustomWebBrowser;
    FCacheFolderName: string;
    FAutoClearCache: Boolean;
    FCacheFolder: string;
    FFullCacheFolderName: string;
  protected
    function GetCacheFolderName: string;
    function GetAutoClearCache: Boolean;
    {$IFDEF LINUX}
    function GetControlHandle: Pointer;
    function GetScale: Single;
    function GetOffset: TRect;
    {$ENDIF}
    procedure SetUserAgent(const Value: string);
    function GetUserAgent: string;
    procedure SetCacheFolderName(const Value: string);
    function GetCacheFolder: string;
    procedure SetFocus;
    procedure SetCacheFolder(const Value: string);
    procedure SetAutoClearCache(const AutoClearCache: Boolean);
    procedure UpdateCacheFolderName;
    procedure RemoveCacheFolder;
    procedure ShowDebugConsole;
    procedure InternalLoadDocumentFromStream(const Stream: TStream);
    procedure SetURL(const AValue: string);
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure Navigate(const AURL: string); overload;
    procedure Navigate; overload;
    procedure LoadHTML(AHTML: String);
    procedure LoadFile(AFile: String);
    procedure GoForward;
    procedure GoBack;
    procedure Close;
    procedure Reload;
    procedure ClearCache;
    procedure StopLoading;
    procedure UpdateVisible;
    procedure UpdateEnabled;
    procedure CaptureScreenShot;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject);
    procedure RemoveBridge(ABridgeName: string);
    procedure UpdateBounds;
    procedure BeforeChangeParent;
    procedure DoInitialize(Sender: TObject);
    procedure Initialize;
    procedure DeInitialize;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    function GetExternalBrowser: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    function GetEnableAcceleratorKeys: Boolean;
    function GetURL: string;
    function NativeEnvironment: Pointer;
    function NativeBrowser: Pointer;
    function GetBrowserInstance: IInterface;
    function NativeDialog: Pointer;
    function IsFMXBrowser: Boolean;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
  public
    constructor Create(const AWebControl: TTMSFNCCustomWebBrowser);
    destructor Destroy; override;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property UserAgent: string read GetUserAgent write SetUserAgent;
  end;

  {$IFDEF LCLLIB}
  {$IFDEF LINUX}
  {$IFDEF LCLGTK3}
  TTMSFNCGtk3WebKitWebView = class(TGtk3Widget)
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  end;

  TTMSFNCGtk3WebControl = class(TGtk3WSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

var
  WebBrowserService: ITMSFNCWebBrowserService;

procedure RegisterWebBrowserService;
begin
  if not TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
  begin
    WebBrowserService := TTMSFNCUnixWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCUnixWebBrowser.GetBrowserInstance: IInterface;
begin
  Result := nil;
end;

function TTMSFNCUnixWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := FEnableAcceleratorKeys;
end;

function TTMSFNCUnixWebBrowser.GetEnableContextMenu: Boolean;
begin
  Result := FEnableContextMenu;
end;

{$IFDEF LINUX}
function UTF8ToStringEx(AValue: PUTF8Char): string;
begin
  {$IFDEF FMXLIB}
  Result := UTF8ToString(AValue);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Result := AValue;
  {$ENDIF}
end;
{$ENDIF}

function TTMSFNCUnixWebBrowser.GetEnableShowDebugConsole: Boolean;
{$IFDEF LINUX}
var
  s: Pointer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    s := webkit_web_view_get_settings(FWebView);
    Result := webkit_settings_get_enable_developer_extras(s);
  end;
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

function TTMSFNCUnixWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCUnixWebBrowser.GetUserAgent: string;
{$IFDEF LINUX}
var
  s: Pointer;
{$ENDIF}
begin
  Result := '';
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    s := webkit_web_view_get_settings(FWebView);
    Result := UTF8ToStringEx(webkit_settings_get_user_agent(s));
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.GoBack;
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_go_back(FWebView);
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.GoForward;
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_go_forward(FWebView);
  {$ENDIF}
end;

{$IFDEF LINUX}
function view_context_menu(web_view: GTKWebView; context_menu: Pointer; event: Pointer; hit_test_result: Pointer; user_data: Pointer): Boolean;
var
  w: TTMSFNCUnixWebBrowser;
begin
  Result := False;
  w := TTMSFNCUnixWebbrowser(user_data);
  if Assigned(w) then
  begin
    Result := w.GetEnableContextMenu;
  end;
end;

procedure web_view_load_changed(web_view: GTKWebView; load_event: GTKWEBKitLoadEvent; user_data: Pointer); cdecl;
var
  w: TTMSFNCUnixWebBrowser;
  p: string;
  BeforeParams: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  CompleteParams: TTMSFNCCustomWebBrowserNavigateCompleteParams;
begin
  w := TTMSFNCUnixWebbrowser(user_data);
  if Assigned(w) and Assigned(w.FWebControl) then
  begin
    case load_event of
      WEBKIT_LOAD_STARTED:
      begin
        p := UTF8ToStringEx(webkit_web_view_get_uri(web_view));

        BeforeParams.URL := p;
        BeforeParams.Cancel := False;
        w.FSaveURL := BeforeParams.URL;
        if not w.FWebControl.IsDesignTime then
          w.FURL := BeforeParams.URL;
        TTMSFNCCustomWebBrowserProtected(w.FWebControl).BeforeNavigate(BeforeParams);

        if BeforeParams.Cancel then
        begin
          w.FStoppedLoading := True;
          webkit_web_view_stop_loading(web_view);
        end;
      end;
      WEBKIT_LOAD_REDIRECTED:;
      WEBKIT_LOAD_COMMITTED:;
      WEBKIT_LOAD_FINISHED:
      begin
        if w.FStoppedLoading then
        begin
          w.FStoppedLoading := False;
          Exit;
        end;

        CompleteParams.URL := w.FSaveURL;

        if Assigned(w.FWebControl) then
        begin
          if not w.FWebControl.IsDesignTime then
            w.FURL := CompleteParams.URL;
          TTMSFNCCustomWebBrowserProtected(w.FWebControl).NavigateComplete(CompleteParams);
        end;
      end;
    end;
  end;
end;

function JSStringToStr(const S: Pointer): string;
var
  L: Integer;
begin
  L := JSStringGetLength(S);
  SetLength(Result, L);
  Move(JSStringGetCharactersPtr(S)^, PChar(Result)^, L * 2);
end;

function ConvertJSToString(AValue: Pointer): string;
var
  c, v, j: Pointer;
  l: UInt64;
  s: PUTF8Char;
begin
  Result := '';
  if Assigned(AValue) then
  begin
    c := webkit_javascript_result_get_global_context(AValue);
    v := webkit_javascript_result_get_value(AValue);

    if JSValueIsString(c, v) then
    begin
      j := JSValueToStringCopy(c, v, nil);
      l := JSStringGetMaximumUTF8CStringSize(j);
      s := g_malloc(l);
      JSStringGetUTF8CString(j, s, l);
      JSStringRelease(j);
      Result := UTF8ToStringEx(s);
      g_free(s);
    end;

    webkit_javascript_result_unref(AValue);
  end;
end;

procedure handle_general_script_message(manager: Pointer; &message: Pointer; user_data: Pointer);
var
  r: String;
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  w: TTMSFNCUnixWebBrowser;
  v: Pointer;
begin
  if Assigned(user_data) then
  begin
    w := TTMSFNCUnixWebBrowser(user_data);
    v := webkit_javascript_result_get_js_value(&message);
    if not Assigned(v) then
      Exit;

    r := UTF8ToStringEx(jsc_value_to_string(v));

    Params.URL := r;
    Params.Cancel := False;
    if Assigned(w.FWebControl) then
    begin
      w.FURL := Params.URL;
      TTMSFNCCustomWebBrowserProtected(w.FWebControl).BeforeNavigate(Params);
    end;
  end;
end;

procedure handle_script_message(manager: Pointer; &message: Pointer; user_data: Pointer);
var
  r: String;
  w: TTMSFNCUnixWebBrowser;
  b: ITMSFNCCustomWebBrowserBridge;
  v: Pointer;
begin
  if Assigned(user_data) then
  begin
    w := TTMSFNCUnixWebBrowser(user_data);
    v := webkit_javascript_result_get_js_value(&message);
    if not Assigned(v) then
      Exit;

    r := UTF8ToStringEx(jsc_value_to_string(v));

    if Assigned(w.FWebControl) then
    begin
      if Assigned(w.FCustomBridgeObject) and Supports(w.FCustomBridgeObject, ITMSFNCCustomWebBrowserBridge, b) then
        b.ObjectMessage := r;
    end;
  end;
end;
{$ENDIF}

procedure TTMSFNCUnixWebBrowser.Initialize;
begin
  {$IFDEF LINUX}
  {$IFDEF LCLGTK3}
  if not Assigned(FInitializeTimer) then
  begin
    FInitializeTimer := TTimer.Create(nil);
    FInitializeTimer.OnTimer := DoInitialize;
    FInitializeTimer.Interval := 1;
  end;
  FInitializeTimer.Enabled := True;
  {$ELSE}
  DoInitialize(nil);
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.DoInitialize(Sender: TObject);
{$IFDEF LINUX}
var
  p: GTKWindow;
  s: GTKWebViewSettings;
  b: Pointer;
  m: Pointer;
  wp: Pointer;
const
  EErrorMessage = 'Could not initialize GTK! Please check GTK installation and verify correct version number.';
  EErrorMessageNoDLL = 'Could not initialize GTK! Please check if ' + WebKitGTKLib + ' is correctly distributed and accessible.';

  {$IFNDEF LCLLIB}
  function GetGTKFixed(ABox: Pointer): Pointer;
  var
    l, lv, v: Pointer;
    I, K: Integer;
  begin
    Result := nil;
    l := gtk_container_get_children(ABox);
    if Assigned(l) then
    begin
      for I := 0 to g_list_length(l) - 1 do
      begin
        v := g_list_nth_data(l, I);
        if Assigned(v) then
        begin
          lv := gtk_container_get_children(v);
          for K := 0 to g_list_length(lv) - 1 do
          begin
            v := g_list_nth_data(lv, K);
            if Assigned(v) and g_type_check_instance_is_a(v, gtk_fixed_get_type) then
            begin
              Result := v;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF LINUX}
  {$IFDEF LCLGTK3}
  FInitializeTimer.Enabled := False;
  {$ENDIF}

  if WebKitGTKLoaded and GTKLoaded then
  begin
    p := GetControlHandle;
    if Assigned(p) then
    begin
      {$IFNDEF LCLLIB}
      b := gtk_bin_get_child(p); //gtkbox
      if Assigned(b) then
      begin
        b := GetGTKFixed(b);
      {$ENDIF}
      {$IFDEF LCLLIB}
        {$IFDEF LCLGTK3}
        b := TGtk3Widget(p).Widget;
        {$ELSE}
        b := nil;
        {$ENDIF}
      {$ENDIF}
        if Assigned(b) then
        begin
          {$IFNDEF LCLLIB}
          FWindowFixed := b;
          {$ENDIF}
          if not Assigned(FWebView) then
          begin
            if INITIALIZEAUTOPLAY then
            begin
              wp := webkit_website_policies_new_with_policies('autoplay', 0, nil);
              FWebView := g_object_new(webkit_web_view_get_type(), 'website-policies', wp, webkit_web_view_new());
            end
            else
              FWebView := webkit_web_view_new();

            s := webkit_web_view_get_settings(FWebView);
            webkit_settings_set_allow_file_access_from_file_urls(s, True);
            webkit_settings_set_enable_developer_extras(s, True);
            webkit_web_view_set_settings(FWebView, s);

	    gtk_frame_set_shadow_type(b, GTK_SHADOW_NONE);   

            m := webkit_web_view_get_user_content_manager(FWebView);
            g_signal_connect_data(m, PUTF8Char(UTF8Encode('script-message-received::' + BridgeName)), @handle_general_script_message, Self, nil, 0);
            webkit_user_content_manager_register_script_message_handler(m, PUTF8Char(UTF8Encode(BridgeName)));

            g_signal_connect_data(FWebView, 'load-changed', @web_view_load_changed, Self, nil, 0);
            g_signal_connect_data(FWebView, 'context-menu', @view_context_menu, Self, nil, 0);

            Navigate;
          end;

          if gtk_widget_get_parent(FWebView) <> b then
            gtk_container_add(b, FWebView);

          gtk_widget_show_all(FWebView);

          UpdateBounds;

          if Assigned(FWebControl) then
            TTMSFNCCustomWebBrowserProtected(FWebControl).Initialized;
        end;
      {$IFNDEF LCLLIB}
      end;
      {$ENDIF}
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCUnixWebBrowser.LoadFile(AFile: String);
{$IFDEF LINUX}
var
  p: Pointer;
  ms: TMemoryStream;
  mt: string;
{$ENDIF}
begin
  {$IFDEF LINUX}
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(AFile);
    p := g_bytes_new(ms.Memory, ms.Size);
    try
      {$IFDEF FNCLIB}
      mt := TTMSFNCUtils.GetMimeType(AFile);
      {$ELSE}
      mt := '';
      {$ENDIF}
      webkit_web_view_load_bytes(FWebView, p, PUTF8Char(UTF8Encode(mt)), nil, nil);
    finally
      g_bytes_unref(p);
    end;
  finally
    ms.Free;
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.LoadHTML(AHTML: String);
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_load_html(FWebView, PUTF8Char(UTF8Encode(AHTML)), nil);
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.Navigate(const AURL: string);
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_load_uri(FWebView, PUTF8Char(UTF8Encode(AURL)));
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.NativeBrowser: Pointer;
begin
  {$IFDEF LINUX}
  Result := FWebView;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
end;

function TTMSFNCUnixWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
end;

procedure TTMSFNCUnixWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCUnixWebBrowser.Reload;
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_reload(FWebView);
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.RemoveBridge(ABridgeName: string);
{$IFDEF LINUX}
var
  m: Pointer;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    m := webkit_web_view_get_user_content_manager(FWebView);
    webkit_user_content_manager_unregister_script_message_handler(m, PUTF8Char(UTF8Encode(ABridgeName)));

    if ABridgeName = FCustomBridge then
    begin
      FCustomBridge := '';
      FCustomBridgeObject := nil;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.SetEnableAcceleratorKeys(const AValue: Boolean);
begin
  FEnableAcceleratorKeys := AValue;
end;

procedure TTMSFNCUnixWebBrowser.SetEnableContextMenu(const AValue: Boolean);
begin
  FEnableContextMenu := AValue;
end;

procedure TTMSFNCUnixWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
{$IFDEF LINUX}
var
  s: Pointer;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    s := webkit_web_view_get_settings(FWebView);
    webkit_settings_set_enable_developer_extras(s, AValue);
    webkit_web_view_set_settings(FWebView, s);
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
end;

procedure TTMSFNCUnixWebBrowser.SetFocus;
begin

end;

procedure TTMSFNCUnixWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCUnixWebBrowser.SetUserAgent(const Value: string);
{$IFDEF LINUX}
var
  s: Pointer;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    s := webkit_web_view_get_settings(FWebView);
    webkit_settings_set_user_agent(s, PUTF8Char(UTF8Encode(Value)));
    webkit_web_view_set_settings(FWebView, s);
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.ShowDebugConsole;
{$IFDEF LINUX}
var
  i: Pointer;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    i := webkit_web_view_get_inspector(FWebView);
    webkit_web_inspector_show(i);
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.StopLoading;
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    webkit_web_view_stop_loading(FWebView);
  {$ENDIF}
end;

{$IFDEF LINUX}
function TTMSFNCUnixWebBrowser.GetOffset: TRect;
var
  p: Pointer;
  x, y, w, h: Integer;
begin
  p := GetControlHandle;
  if Assigned(p) then
  begin
    gtk_window_get_position(p, x, y);
    gtk_window_get_size(p, w, h);

    Result := Rect(x, y, x + w, y + h);
  end
  else
    Result := TRect.Empty;
end;

function TTMSFNCUnixWebBrowser.GetScale: Single;
begin
  Result := 1.0;
end;
{$ENDIF}

procedure TTMSFNCUnixWebBrowser.UpdateBounds;
{$IFDEF LINUX}
var
  b: TRect;
  off, m: Single;
  {$IFDEF FMXLIB}
  bd: TRectF;
  sc: Single;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF LINUX}
  {$IFDEF LCLGTK3}
  Exit;
  {$ENDIF}
  if {$IFDEF FMXLIB}Assigned(FWindowFixed) and {$ENDIF}Assigned(FWebView) and Assigned(FWebControl) then
  begin
    off := 0;
    m := 0;
    if FWebControl.IsDesigning then
    begin
      off := 25;
      m := 7;
    end;

    {$IFDEF FMXLIB}
    sc := TTMSFNCUtils.GetDPIScale(FWebControl);
    bd := FWebControl.AbsoluteRect;
    b := Bounds(Round(bd.Left * sc + m), Round(bd.Top * sc + off), Round(bd.Width * sc - m * 2), Round(bd.Height * sc - off - m));
    {$ELSE}
    b := Bounds(Round(m), Round(off), Round(FWebControl.Width - m * 2), Round(FWebControl.Height - off - m));
    {$ENDIF}
    {$IFDEF FMXLIB}
    gtk_fixed_move(FWindowFixed, FWebView, b.Left, b.Top);
    {$ENDIF}
    gtk_widget_set_size_request(FWebView, b.Right - b.Left, b.Bottom - b.Top);
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.UpdateCacheFolderName;
begin
  if FCacheFolderName <> '' then
    FFullCacheFolderName := TTMSFNCUtils.AddBackslash(FCacheFolder) + FCacheFolderName
  else
    FFullCacheFolderName := TTMSFNCUtils.AddBackslash(FCacheFolder) + StringReplace(ExtractFileName(ParamStr(0)), ExtractFileExt(ParamStr(0)), '', []);
end;

procedure TTMSFNCUnixWebBrowser.UpdateEnabled;
begin
end;

procedure TTMSFNCUnixWebBrowser.UpdateVisible;
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    gtk_widget_set_visible(FWebView, FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible{$IFDEF FMXLIB} and FWebControl.ParentedVisible{$ENDIF});
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.InternalLoadDocumentFromStream(const Stream: TStream);
begin
  {$IFDEF LINUX}
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
{$IFDEF LINUX}
var
  m: Pointer;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    FCustomBridge := ABridgeName;
    FCustomBridgeObject := ABridgeObject;
    m := webkit_web_view_get_user_content_manager(FWebView);
    g_signal_connect_data(m, PUTF8Char(UTF8Encode('script-message-received::' + ABridgeName)), @handle_script_message, Self, nil, 0);
    webkit_user_content_manager_register_script_message_handler(m, PUTF8Char(UTF8Encode(ABridgeName)));
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.BeforeChangeParent;
begin
  {$IFDEF LINUX}
  {$IFNDEF LCLLIB}
  if Assigned(FWebView) and Assigned(FWindowFixed) then
  begin
    if gtk_widget_get_parent(FWebView) = FWindowFixed then
    begin
      g_object_ref(FWebView);
      gtk_container_remove(FWindowFixed, FWebView);
      FWindowFixed := nil;
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    Result := webkit_web_view_can_go_back(FWebView);
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  {$IFDEF LINUX}
  if Assigned(FWebView) then
    Result := webkit_web_view_can_go_forward(FWebView);
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.CaptureScreenShot;
begin
  {$IFDEF LINUX}
  raise Exception.Create('Implement Capture ScreenShot');
  {$ENDIF}
end;

procedure TTMSFNCUnixWebBrowser.ClearCache;
begin
  RemoveCacheFolder;
end;

procedure TTMSFNCUnixWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

destructor TTMSFNCUnixWebBrowser.Destroy;
begin
  {$IFDEF UNIX}
  {$IFDEF LCLGTK3}
  FreeAndNil(FInitializeTimer);
  {$ENDIF}
  {$ENDIF}
  inherited;
end;

constructor TTMSFNCUnixWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
begin
  FExternalBrowser := False;
  FWebControl := AWebControl;
  FEnableAcceleratorKeys := True;

  FBlockEvents := True;
  FCacheFolder := TTMSFNCUtils.AddBackslash(TTMSFNCUtils.AddBackslash(TTMSFNCUtils.GetTempPath) +
    StringReplace(ExtractFileName(ParamStr(0)), ExtractFileExt(ParamStr(0)), '', []));
  FCacheFolderName := 'WebViewGTKCache';
  FAutoClearCache := True;
  UpdateCacheFolderName;
end;

procedure TTMSFNCUnixWebBrowser.DeInitialize;
begin
  {$IFDEF LINUX}
  FWindowFixed := nil;
  if Assigned(FWebView) then
  begin
    gtk_widget_destroy(FWebView);
    FWebView := nil;
  end;
  {$ENDIF}
end;

{$IFDEF LINUX}
procedure JavaScriptCallBack(web_view: GTKWebView; res: Pointer; user_data: Pointer); cdecl;
var
  r: Pointer;
  w: TTMSFNCUnixWebBrowserScript;
  e: Pointer;
  vs: string;
begin
  w := TTMSFNCUnixWebBrowserScript(user_data);
  if Assigned(w) then
  begin
    e := nil;
    r := webkit_web_view_run_javascript_finish(w.FWebView, res, e);

    if not Assigned(r) then
      Exit;

    vs := ConvertJSToString(r);

    if Assigned(w.FCompleteEvent) then
      w.FCompleteEvent(vs);

    if Assigned(w.FCallback) then
      w.FCallback(w);

    w.Free;
  end;
end;
{$ENDIF}

procedure TTMSFNCUnixWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
{$IFDEF LINUX}
var
  s: TTMSFNCUnixWebBrowserScript;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if Assigned(FWebView) then
  begin
    s := TTMSFNCUnixWebBrowserScript.Create;
    s.FCompleteEvent := ACompleteEvent;
    s.FCallback := ACallback;
    s.FWebView := FWebView;
    webkit_web_view_run_javascript(FWebView, PUTF8Char(UTF8Encode(AScript)), nil, @JavaScriptCallBack, s);
  end;
  {$ENDIF}
end;

function TTMSFNCUnixWebBrowser.GetCacheFolder: string;
begin
  Result := FCacheFolder;
end;

function TTMSFNCUnixWebBrowser.GetCacheFolderName: string;
begin
  Result := FCacheFolderName;
end;

function TTMSFNCUnixWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := FAutoClearCache;
end;

procedure TTMSFNCUnixWebBrowser.SetCacheFolder(const Value: string);
begin
  FCacheFolder := Value;
  UpdateCacheFolderName;
end;

procedure TTMSFNCUnixWebBrowser.SetCacheFolderName(const Value: string);
begin
  FCacheFolderName := Value;
  UpdateCacheFolderName;
end;

procedure TTMSFNCUnixWebBrowser.SetAutoClearCache(const AutoClearCache: Boolean);
begin
  FAutoClearCache := AutoClearCache;
end;

procedure TTMSFNCUnixWebBrowser.RemoveCacheFolder;
{$IFDEF LINUX}
{$ENDIF}
begin
  {$IFDEF LINUX}
  {$ENDIF}
end;

{$IFDEF LINUX}
function TTMSFNCUnixWebBrowser.GetControlHandle: Pointer;
{$IFDEF FMXLIB}
var
  f: TCustomForm;
{$ENDIF}
begin
  Result := nil;
  if Assigned(FWebControl) then
  begin
    {$IFDEF LCLLIB}
    if FWebControl.HandleAllocated then
      Result := Pointer(FWebControl.Handle);
    {$ENDIF}
    {$IFDEF FMXLIB}
    f := TTMSFNCUtils.GetParentForm(FWebControl);
    if Assigned(f) then
      Result := FormToHandle(f).NativeHandle
    else if (FWebControl.Parent is TFrame) and Assigned(Application.MainForm) then
    begin
      Result := FormToHandle(Application.MainForm).NativeHandle;
    end;
    {$ENDIF}
  end;
end;
{$ENDIF}

{ TTMSFNCUnixWebBrowserService }

procedure TTMSFNCUnixWebBrowserService.DeleteCookies;
begin
end;

function TTMSFNCUnixWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCUnixWebBrowser.Create(AValue);
end;

{$IFDEF LINUX}

{$IFDEF LCLLIB}
{$IFDEF LCLGTK3}
class function TTMSFNCGtk3WebControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  WebView: TTMSFNCGtk3WebKitWebView;
begin
  WebView := TTMSFNCGtk3WebKitWebView.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(WebView);
end;

function TTMSFNCGtk3WebKitWebView.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result := TGtkFrame.new(nil);
end;
{$ENDIF}
{$ENDIF}

function LoadLibraryEx(AModuleName: string): TGTKHandle;
begin
  {$IFDEF FMXLIB}
  Result := LoadLibrary(PChar(AModuleName))
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := TLibHandle(dlopen(PChar(AModuleName), RTLD_LAZY));
  {$ENDIF}
end;

procedure InitializeWebKitGTK;
begin
  if not WebKitGTKLoaded then
  begin
    WebKitGTKHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(GTKDLLPath) + WebKitGTKLib);
    if (WebKitGTKHandle = 0) then
      Exit;

    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_type{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_type');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_website_policies_new_with_policies{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_website_policies_new_with_policies');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_javascript_result_get_global_context{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_javascript_result_get_global_context');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_javascript_result_get_value{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_javascript_result_get_value');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_javascript_result_unref{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_javascript_result_unref');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_javascript_result_get_js_value{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_javascript_result_get_js_value');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_load_html{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_load_html');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_load_plain_text{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_load_plain_text');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_can_go_back{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_can_go_back');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_load_uri{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_load_uri');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_load_bytes{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_load_bytes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_can_go_forward{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_can_go_forward');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_go_back{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_go_back');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_go_forward{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_go_forward');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_reload{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_reload');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_stop_loading{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_stop_loading');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_run_javascript_finish{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_run_javascript_finish');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_run_javascript{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_run_javascript');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_settings{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_settings');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_set_settings{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_set_settings');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_set_user_agent{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_set_user_agent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_get_user_agent{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_get_user_agent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_set_allow_file_access_from_file_urls{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_set_allow_file_access_from_file_urls');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_get_allow_file_access_from_file_urls{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_get_allow_file_access_from_file_urls');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_inspector_get_web_view{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_inspector_get_web_view');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_inspector_show{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_inspector_show');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_inspector_close{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_inspector_close');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_set_enable_developer_extras{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_set_enable_developer_extras');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_settings_get_enable_developer_extras{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_settings_get_enable_developer_extras');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_uri{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_uri');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_inspector{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_inspector');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_user_content_manager{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_user_content_manager');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_user_content_manager_register_script_message_handler{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_user_content_manager_register_script_message_handler');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_user_content_manager_unregister_script_message_handler{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_user_content_manager_unregister_script_message_handler');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_get_context{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_get_context');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_navigation_policy_decision_get_navigation_action{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_navigation_policy_decision_get_navigation_action');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_navigation_action_get_request{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_navigation_action_get_request');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_web_view_download_uri{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_web_view_download_uri');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_uri_request_get_uri{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_uri_request_get_uri');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_uri_request_get_http_headers{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_uri_request_get_http_headers');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_navigation_action_get_navigation_type{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_navigation_action_get_navigation_type');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_response_policy_decision_is_mime_type_supported{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_response_policy_decision_is_mime_type_supported');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_policy_decision_download{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_policy_decision_download');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_policy_decision_use{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_policy_decision_use');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_download_set_destination{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_download_set_destination');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_download_cancel{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_download_cancel');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_download_get_request{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_download_get_request');
    {$IFDEF LCLLIB}Pointer({$ENDIF}webkit_download_get_received_data_length{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(WebKitGTKHandle, 'webkit_download_get_received_data_length');

    WebKitGTKLoaded := True;
  end;

  if not GTKLoaded then
  begin
    GTKHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(GTKDLLPath) + GTKLib);
    if (GTKHandle = 0) then
      Exit;


    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_get_snapshot{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_get_snapshot');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_frame_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_frame_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_frame_set_shadow_type{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_frame_set_shadow_type');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_set_size_request{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_set_size_request');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_show{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_show');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_destroy{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_destroy');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_scrolled_window_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_scrolled_window_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_container_add{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_container_add');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_container_remove{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_container_remove');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_set_parent{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_set_parent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_set_parent_window{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_set_parent_window');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_title{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_title');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_box_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_box_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_box_pack_start{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_box_pack_start');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_get_default_widget{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_get_default_widget');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_show_all{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_show_all');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_hide{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_hide');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_bin_get_child{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_bin_get_child');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_decorated{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_decorated');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_move{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_move');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_get_position{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_get_position');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_get_size{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_get_size');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_default_size{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_default_size');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_resize{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_resize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_transient_for{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_transient_for');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_keep_above{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_keep_above');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_window_set_attached_to{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_window_set_attached_to');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_dialog_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_dialog_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_dialog_get_content_area{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_dialog_get_content_area');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_get_parent{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_get_parent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_overlay_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_overlay_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_container_get_children{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_container_get_children');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_fixed_move{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_fixed_move');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_fixed_get_type{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_fixed_get_type');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_bin_get_type{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_bin_get_type');
    {$IFDEF LCLLIB}Pointer({$ENDIF}gtk_widget_set_visible{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GTKHandle, 'gtk_widget_set_visible');

    GTKLoaded := True;
  end;

  if not GLibGTKLoaded then
  begin
    GLibGTKHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(GLibGTKDLLPath) + GLibGTKLib);
    if (GLibGTKHandle = 0) then
      Exit;

    {$IFDEF LCLLIB}Pointer({$ENDIF}g_malloc{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_malloc');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_free{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_free');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_bytes_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_bytes_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_bytes_unref{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_bytes_unref');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_bytes_get_size{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_bytes_get_size');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_list_nth{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_list_nth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_list_nth_data{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_list_nth_data');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_list_length{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GLibGTKHandle, 'g_list_length');

    GLibGTKLoaded := True;
  end;

  if not GObjectGTKLoaded then
  begin
    GObjectGTKHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(GObjectGTKDLLPath) + GObjectGTKLib);
    if (GObjectGTKHandle = 0) then
      Exit;

    {$IFDEF LCLLIB}Pointer({$ENDIF}g_object_unref{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GObjectGTKHandle, 'g_object_unref');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_object_new{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GObjectGTKHandle, 'g_object_new');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_object_ref{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GObjectGTKHandle, 'g_object_ref');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_signal_connect_data{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GObjectGTKHandle, 'g_signal_connect_data');
    {$IFDEF LCLLIB}Pointer({$ENDIF}g_type_check_instance_is_a{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(GObjectGTKHandle, 'g_type_check_instance_is_a');

    GObjectGTKLoaded := True;
  end;

  if not SoupLoaded then
  begin
    SoupHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(SoupDLLPath) + SoupLib);
    if (SoupHandle = 0) then
      Exit;

    {$IFDEF LCLLIB}Pointer({$ENDIF}soup_message_headers_get_one{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(SoupHandle, 'soup_message_headers_get_one');
    {$IFDEF LCLLIB}Pointer({$ENDIF}soup_message_headers_get_list{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(SoupHandle, 'soup_message_headers_get_list');
    {$IFDEF LCLLIB}Pointer({$ENDIF}soup_message_headers_get_content_length{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(SoupHandle, 'soup_message_headers_get_content_length');
    {$IFDEF LCLLIB}Pointer({$ENDIF}soup_message_headers_get_encoding{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(SoupHandle, 'soup_message_headers_get_encoding');

    SoupLoaded := True;
  end;

  if not JavaScriptCoreGTKLoaded then
  begin
    JavaScriptCoreGTKHandle := LoadLibraryEx(TTMSFNCUtils.AddBackslash(JavaScriptCoreGTKDLLPath) + JavaScriptCoreGTKLib);
    if (JavaScriptCoreGTKHandle = 0) then
      Exit;

    {$IFDEF LCLLIB}Pointer({$ENDIF}JSValueToStringCopy{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSValueToStringCopy');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSValueIsString{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSValueIsString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringGetLength{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringGetLength');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringRelease{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringRelease');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringGetCharactersPtr{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringGetCharactersPtr');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringCreateWithUTF8CString{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringCreateWithUTF8CString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSContextGroupCreate{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSContextGroupCreate');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSGlobalContextCreateInGroup{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSGlobalContextCreateInGroup');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSContextGetGlobalObject{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSContextGetGlobalObject');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSObjectSetProperty{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSObjectSetProperty');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSObjectMakeFunctionWithCallback{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSObjectMakeFunctionWithCallback');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSValueMakeUndefined{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSValueMakeUndefined');
    {$IFDEF LCLLIB}Pointer({$ENDIF}jsc_value_to_string{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'jsc_value_to_string');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringGetMaximumUTF8CStringSize{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringGetMaximumUTF8CStringSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}JSStringGetUTF8CString{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(JavaScriptCoreGTKHandle, 'JSStringGetUTF8CString');

    JavaScriptCoreGTKLoaded := True;
  end;
end;

procedure UninitializeWebKitGTK;
begin
  if WebKitGTKLoaded then
  begin
    FreeLibrary(WebKitGTKHandle);
    WebKitGTKLoaded := false;
  end;

  if JavaScriptCoreGTKLoaded then
  begin
    FreeLibrary(JavaScriptCoreGTKHandle);
    JavaScriptCoreGTKLoaded := false;
  end;

  if GTKLoaded then
  begin
    FreeLibrary(GTKHandle);
    GTKLoaded := false;
  end;

  if GObjectGTKLoaded then
  begin
    FreeLibrary(GObjectGTKHandle);
    GObjectGTKLoaded := false;
  end;

  if GObjectGTKLoaded then
  begin
    FreeLibrary(GObjectGTKHandle);
    GObjectGTKLoaded := false;
  end;
end;

initialization
begin
  InitializeWebKitGTK;
  {$IFDEF LCLLIB}
  {$IFDEF UNIX}
  {$IFDEF LCLGTK3}
  RegisterWSComponent(TTMSFNCCustomWebBrowser, TTMSFNCGtk3WebControl);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

finalization
  UninitializeWebKitGTK;
{$ENDIF}

end.
