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

unit FMX.TMSFNCWebBrowser.Win;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF MSWINDOWS}
{$DEFINE EDGESUPPORT}
{$ENDIF}

interface

{$IFDEF EDGESUPPORT}
uses
  Windows, ActiveX;

const
  COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC = $00000000;
  COREWEBVIEW2_MOVE_FOCUS_REASON_NEXT = $00000001;
  COREWEBVIEW2_MOVE_FOCUS_REASON_PREVIOUS = $00000002;

  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_NONE = $0000;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SAVE = $0001;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PRINT = $0002;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SAVE_AS = $0004;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ZOOM_IN = $0008;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ZOOM_OUT = $0010;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ROTATE = $0020;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_FIT_PAGE = $0040;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PAGE_LAYOUT = $0080;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_BOOKMARKS = $0100;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PAGE_SELECTOR = $0200;
  COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SEARCH = $0400;

  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_COMMAND = 0;
  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_CHECK_BOX = COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_COMMAND + 1;
  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_RADIO = COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_CHECK_BOX + 1;
  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SEPARATOR = COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_RADIO + 1;
  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SUBMENU = COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SEPARATOR + 1;

  COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER = 0;
  COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM = COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER + 1;

  COREWEBVIEW2_PRINT_STATUS_SUCCEEDED = 0;
  COREWEBVIEW2_PRINT_STATUS_PRINTER_UNAVAILABLE = COREWEBVIEW2_PRINT_STATUS_SUCCEEDED + 1;
  COREWEBVIEW2_PRINT_STATUS_OTHER_ERROR = COREWEBVIEW2_PRINT_STATUS_PRINTER_UNAVAILABLE + 1;

  COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_DENY = 0;
  COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW = COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_DENY + 1;
  COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_DENY_CORS = COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW + 1;

  COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN = 0;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT = COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED	= COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS = COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED	= COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID = COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE = COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT	= COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE = COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED = COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET = COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED = COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT = COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED = COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED = COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED	= COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED + 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_UNEXPECTED_ERROR = COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED + 1;

  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL = 0;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_DOCUMENT	= COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_STYLESHEET = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_DOCUMENT + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_STYLESHEET + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MEDIA = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FONT = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MEDIA + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SCRIPT = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FONT + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_XML_HTTP_REQUEST = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SCRIPT + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FETCH = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_XML_HTTP_REQUEST + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_TEXT_TRACK = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FETCH + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_EVENT_SOURCE = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_TEXT_TRACK + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_WEBSOCKET = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_EVENT_SOURCE + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MANIFEST = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_WEBSOCKET + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SIGNED_EXCHANGE = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MANIFEST + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_PING = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SIGNED_EXCHANGE + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_CSP_VIOLATION_REPORT = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_PING + 1;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_OTHER = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_CSP_VIOLATION_REPORT + 1;

  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG = 0;
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_JPEG = COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG + 1;

  COREWEBVIEW2_PERMISSION_KIND_UNKNOWN_PERMISSION = 0;
  COREWEBVIEW2_PERMISSION_KIND_MICROPHONE = COREWEBVIEW2_PERMISSION_KIND_UNKNOWN_PERMISSION + 1;
  COREWEBVIEW2_PERMISSION_KIND_CAMERA = COREWEBVIEW2_PERMISSION_KIND_MICROPHONE + 1;
  COREWEBVIEW2_PERMISSION_KIND_GEOLOCATION = COREWEBVIEW2_PERMISSION_KIND_CAMERA + 1;
  COREWEBVIEW2_PERMISSION_KIND_NOTIFICATIONS = COREWEBVIEW2_PERMISSION_KIND_GEOLOCATION + 1;
  COREWEBVIEW2_PERMISSION_KIND_OTHER_SENSORS = COREWEBVIEW2_PERMISSION_KIND_NOTIFICATIONS + 1;
  COREWEBVIEW2_PERMISSION_KIND_CLIPBOARD_READ = COREWEBVIEW2_PERMISSION_KIND_OTHER_SENSORS + 1;
  COREWEBVIEW2_PERMISSION_STATE_DEFAULT = 0;
  COREWEBVIEW2_PERMISSION_STATE_ALLOW = COREWEBVIEW2_PERMISSION_STATE_DEFAULT + 1;
  COREWEBVIEW2_PERMISSION_STATE_DENY = COREWEBVIEW2_PERMISSION_STATE_ALLOW + 1;

  COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS = 0;
  COREWEBVIEW2_DOWNLOAD_STATE_INTERRUPTED = COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS + 1;
  COREWEBVIEW2_DOWNLOAD_STATE_COMPLETED = COREWEBVIEW2_DOWNLOAD_STATE_INTERRUPTED + 1;

  COREWEBVIEW2_PRINT_ORIENTATION_PORTRAIT = 0;
  COREWEBVIEW2_PRINT_ORIENTATION_LANDSCAPE = COREWEBVIEW2_PRINT_ORIENTATION_PORTRAIT + 1;

  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_PAGE = 0;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_IMAGE = COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_PAGE + 1;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_SELECTED_TEXT = COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_IMAGE + 1;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_AUDIO = COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_SELECTED_TEXT + 1;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_VIDEO = COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_AUDIO + 1;

  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_NONE = 0;
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_LAX = COREWEBVIEW2_COOKIE_SAME_SITE_KIND_NONE + 1;
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_STRICT = COREWEBVIEW2_COOKIE_SAME_SITE_KIND_LAX + 1;

  IID_ICoreWebView2CookieGUID = '{AD26D6BE-1486-43E6-BF87-A2034006CA21}';
  IID_ICoreWebView2Cookie: TGUID = IID_ICoreWebView2CookieGUID;
  IID_ICoreWebView2CookieListGUID = '{F7F6F714-5D2A-43C6-9503-346ECE02D186}';
  IID_ICoreWebView2CookieList: TGUID = IID_ICoreWebView2CookieListGUID;
  IID_ICoreWebView2CookieManagerGUID = '{177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}';
  IID_ICoreWebView2CookieManager: TGUID = IID_ICoreWebView2CookieManagerGUID;
  IID_ICoreWebView2GetCookiesCompletedHandlerGUID = '{5A4F5069-5C15-47C3-8646-F4DE1C116670}';
  IID_ICoreWebView2GetCookiesCompletedHandler: TGUID = IID_ICoreWebView2GetCookiesCompletedHandlerGUID;
  IID_ICoreWebView2EnvironmentOptionsGUID = '{2fde08a8-1e9a-4766-8c05-95a9ceb9d1c5}';
  IID_ICoreWebView2EnvironmentOptions: TGUID = IID_ICoreWebView2EnvironmentOptionsGUID;
  IID_ICoreWebView2SettingsGUID = '{e562e4f0-d7fa-43ac-8d71-c05150499f00}';
  IID_ICoreWebView2Settings: TGUID = IID_ICoreWebView2SettingsGUID;
  IID_ICoreWebView2Settings2GUID = '{ee9a0f68-f46c-4e32-ac23-ef8cac224d2a}';
  IID_ICoreWebView2Settings2: TGUID = IID_ICoreWebView2Settings2GUID;
  IID_ICoreWebView2Settings3GUID = '{fdb5ab74-af33-4854-84f0-0a631deb5eba}';
  IID_ICoreWebView2Settings3: TGUID = IID_ICoreWebView2Settings3GUID;
  IID_ICoreWebView2Settings4GUID = '{cb56846c-4168-4d53-b04f-03b6d6796ff2}';
  IID_ICoreWebView2Settings4: TGUID = IID_ICoreWebView2Settings4GUID;
  IID_ICoreWebView2Settings5GUID = '{183e7052-1d03-43a0-ab99-98e043b66b39}';
  IID_ICoreWebView2Settings5: TGUID = IID_ICoreWebView2Settings5GUID;
  IID_ICoreWebView2Settings6GUID = '{11cb3acd-9bc8-43b8-83bf-f40753714f87}';
  IID_ICoreWebView2Settings6: TGUID = IID_ICoreWebView2Settings6GUID;
  IID_ICoreWebView2Settings7GUID = '{488dc902-35ef-42d2-bc7d-94b65c4bc49c}';
  IID_ICoreWebView2Settings7: TGUID = IID_ICoreWebView2Settings7GUID;
  IID_ICoreWebView2NavigationCompletedEventArgsGUID = '{30d68b7d-20d9-4752-a9ca-ec8448fbb5c1}';
  IID_ICoreWebView2NavigationCompletedEventArgs: TGUID = IID_ICoreWebView2NavigationCompletedEventArgsGUID;
  IID_ICoreWebView2HttpHeadersCollectionIteratorGUID = '{0702fc30-f43b-47bb-ab52-a42cb552ad9f}';
  IID_ICoreWebView2HttpHeadersCollectionIterator: TGUID = IID_ICoreWebView2HttpHeadersCollectionIteratorGUID;
  IID_ICoreWebView2HttpResponseHeadersGUID = '{03c5ff5a-9b45-4a88-881c-89a9f328619c}';
  IID_ICoreWebView2HttpResponseHeaders: TGUID = IID_ICoreWebView2HttpResponseHeadersGUID;
  IID_ICoreWebView2HttpRequestHeadersGUID = '{e86cac0e-5523-465c-b536-8fb9fc8c8c60}';
  IID_ICoreWebView2HttpRequestHeaders: TGUID = IID_ICoreWebView2HttpRequestHeadersGUID;
  IID_ICoreWebView2WebResourceRequestGUID = '{97055cd4-512c-4264-8b5f-e3f446cea6a5}';
  IID_ICoreWebView2WebResourceRequest: TGUID = IID_ICoreWebView2WebResourceRequestGUID;
  IID_ICoreWebView2WebResourceResponseGUID = '{aafcc94f-fa27-48fd-97df-830ef75aaec9}';
  IID_ICoreWebView2WebResourceResponse: TGUID = IID_ICoreWebView2WebResourceResponseGUID;
  IID_ICoreWebView2DeferralGUID = '{c10e7f7b-b585-46f0-a623-8befbf3e4ee0}';
  IID_ICoreWebView2Deferral: TGUID = IID_ICoreWebView2DeferralGUID;
  IID_ICoreWebView2WebResourceRequestedEventArgsGUID = '{453e667f-12c7-49d4-be6d-ddbe7956f57a}';
  IID_ICoreWebView2WebResourceRequestedEventArgs: TGUID = IID_ICoreWebView2WebResourceRequestedEventArgsGUID;
  IID_ICoreWebView2AcceleratorKeyPressedEventArgsGUID = '{9f760f8a-fb79-42be-9990-7b56900fa9c7}';
  IID_ICoreWebView2AcceleratorKeyPressedEventArgs: TGUID = IID_ICoreWebView2AcceleratorKeyPressedEventArgsGUID;
  IID_ICoreWebView2NewWindowRequestedEventArgsGUID = '{34acb11c-fc37-4418-9132-f9c21d1eafb9}';
  IID_ICoreWebView2NewWindowRequestedEventArgs: TGUID = IID_ICoreWebView2NewWindowRequestedEventArgsGUID;
  IID_ICoreWebView2DOMContentLoadedEventArgsGUID = '{16B1E21A-C503-44F2-84C9-70ABA5031283}';
  IID_ICoreWebView2DOMContentLoadedEventArgs: TGUID = IID_ICoreWebView2DOMContentLoadedEventArgsGUID;
  IID_ICoreWebView2PermissionRequestedEventArgsGUID = '{973ae2ef-ff18-4894-8fb2-3c758f046810}';
  IID_ICoreWebView2PermissionRequestedEventArgs: TGUID = IID_ICoreWebView2PermissionRequestedEventArgsGUID;
  IID_ICoreWebView2ContentLoadingEventArgsGUID = '{0c8a1275-9b6b-4901-87ad-70df25bafa6e}';
  IID_ICoreWebView2ContentLoadingEventArgs: TGUID = IID_ICoreWebView2ContentLoadingEventArgsGUID;
  IID_ICoreWebView2SourceChangedEventArgsGUID = '{31e0e545-1dba-4266-8914-f63848a1f7d7}';
  IID_ICoreWebView2SourceChangedEventArgs: TGUID = IID_ICoreWebView2SourceChangedEventArgsGUID;
  IID_ICoreWebView2NavigationStartingEventArgsGUID = '{5b495469-e119-438a-9b18-7604f25f2e49}';
  IID_ICoreWebView2NavigationStartingEventArgs: TGUID = IID_ICoreWebView2NavigationStartingEventArgsGUID;
  IID_ICoreWebView2TrySuspendCompletedHandlerGUID = '{00F206A7-9D17-4605-91F6-4E8E4DE192E3}';
  IID_ICoreWebView2TrySuspendCompletedHandler: TGUID = IID_ICoreWebView2TrySuspendCompletedHandlerGUID;
  IID_ICoreWebView2NavigationStartingEventHandlerGUID = '{9adbe429-f36d-432b-9ddc-f8881fbd76e3}';
  IID_ICoreWebView2NavigationStartingEventHandler: TGUID = IID_ICoreWebView2NavigationStartingEventHandlerGUID;
  IID_ICoreWebView2SourceChangedEventHandlerGUID = '{3c067f9f-5388-4772-8b48-79f7ef1ab37c}';
  IID_ICoreWebView2SourceChangedEventHandler: TGUID = IID_ICoreWebView2SourceChangedEventHandlerGUID;
  IID_ICoreWebView2HistoryChangedEventHandlerGUID = '{c79a420c-efd9-4058-9295-3e8b4bcab645}';
  IID_ICoreWebView2HistoryChangedEventHandler: TGUID = IID_ICoreWebView2HistoryChangedEventHandlerGUID;
  IID_ICoreWebView2ContentLoadingEventHandlerGUID = '{364471e7-f2be-4910-bdba-d72077d51c4b}';
  IID_ICoreWebView2ContentLoadingEventHandler: TGUID = IID_ICoreWebView2ContentLoadingEventHandlerGUID;
  IID_ICoreWebView2NavigationCompletedEventHandlerGUID = '{d33a35bf-1c49-4f98-93ab-006e0533fe1c}';
  IID_ICoreWebView2NavigationCompletedEventHandler: TGUID = IID_ICoreWebView2NavigationCompletedEventHandlerGUID;
  IID_ICoreWebView2WebResourceRequestedEventHandlerGUID = '{ab00b74c-15f1-4646-80e8-e76341d25d71}';
  IID_ICoreWebView2WebResourceRequestedEventHandler: TGUID = IID_ICoreWebView2WebResourceRequestedEventHandlerGUID;
  IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandlerGUID = '{5c4889f0-5ef6-4c5a-952c-d8f1b92d0574}';
  IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandler: TGUID = IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandlerGUID;
  IID_ICoreWebView2ExecuteScriptCompletedHandlerGUID = '{49511172-cc67-4bca-9923-137112f4c4cc}';
  IID_ICoreWebView2ExecuteScriptCompletedHandler: TGUID = IID_ICoreWebView2ExecuteScriptCompletedHandlerGUID;
  IID_ICoreWebView2AcceleratorKeyPressedEventHandlerGUID = '{b29c7e28-fa79-41a8-8e44-65811c76dcb2}';
  IID_ICoreWebView2AcceleratorKeyPressedEventHandler: TGUID = IID_ICoreWebView2AcceleratorKeyPressedEventHandlerGUID;
  IID_ICoreWebView2NewWindowRequestedEventHandlerGUID = '{d4c185fe-c81c-4989-97af-2d3fa7ab5651}';
  IID_ICoreWebView2NewWindowRequestedEventHandler: TGUID = IID_ICoreWebView2NewWindowRequestedEventHandlerGUID;
  IID_ICoreWebView2DOMContentLoadedEventHandlerGUID = '{4BAC7E9C-199E-49ED-87ED-249303ACF019}';
  IID_ICoreWebView2DOMContentLoadedEventHandler: TGUID = IID_ICoreWebView2DOMContentLoadedEventHandlerGUID;
  IID_ICoreWebView2FocusChangedEventHandlerGUID = '{05ea24bd-6452-4926-9014-4b82b498135d}';
  IID_ICoreWebView2FocusChangedEventHandler: TGUID = IID_ICoreWebView2FocusChangedEventHandlerGUID;
  IID_ICoreWebView2CapturePreviewCompletedHandlerGUID = '{697e05e9-3d8f-45fa-96f4-8ffe1ededaf5}';
  IID_ICoreWebView2CapturePreviewCompletedHandler: TGUID = IID_ICoreWebView2CapturePreviewCompletedHandlerGUID;
  IID_ICoreWebView2CursorChangedEventHandlerGUID = '{9da43ccc-26e1-4dad-b56c-d8961c94c571}';
  IID_ICoreWebView2CursorChangedEventHandler: TGUID = IID_ICoreWebView2CursorChangedEventHandlerGUID;
  IID_ICoreWebView2PermissionRequestedEventHandlerGUID = '{15e1c6a3-c72a-4df3-91d7-d097fbec6bfd}';
  IID_ICoreWebView2PermissionRequestedEventHandler: TGUID = IID_ICoreWebView2PermissionRequestedEventHandlerGUID;
  IID_ICoreWebView2GUID = '{76eceacb-0462-4d94-ac83-423a6793775e}';
  IID_ICoreWebView2: TGUID = IID_ICoreWebView2GUID;
  IID_ICoreWebView2ControllerGUID = '{4d00c0d1-9434-4eb6-8078-8697a560334f}';
  IID_ICoreWebView2Controller: TGUID = IID_ICoreWebView2ControllerGUID;
  IID_ICoreWebView2Controller2GUID = '{c979903e-d4ca-4228-92eb-47ee3fa96eab}';
  IID_ICoreWebView2Controller2: TGUID = IID_ICoreWebView2Controller2GUID;
  IID_ICoreWebView2Controller3GUID = '{f9614724-5d2b-41dc-aef7-73d62b51543b}';
  IID_ICoreWebView2Controller3: TGUID = IID_ICoreWebView2Controller3GUID;
  IID_ICoreWebView2Controller4GUID = '{97d418d5-a426-4e49-a151-e1a10f327d9e}';
  IID_ICoreWebView2Controller4: TGUID = IID_ICoreWebView2Controller4GUID;
  IID_ICoreWebView2CompositionControllerGUID = '{3df9b733-b9ae-4a15-86b4-eb9ee9826469}';
  IID_ICoreWebView2CompositionController: TGUID = IID_ICoreWebView2CompositionControllerGUID;
  IID_ICoreWebView2CompositionController2GUID = '{0b6a3d24-49cb-4806-ba20-b5e0734a7b26}';
  IID_ICoreWebView2CompositionController2: TGUID = IID_ICoreWebView2CompositionController2GUID;
  IID_ICoreWebView2CompositionController3GUID = '{9570570e-4d76-4361-9ee1-f04d0dbdfb1e}';
  IID_ICoreWebView2CompositionController3: TGUID = IID_ICoreWebView2CompositionController3GUID;
  IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandlerGUID = '{6c4819f3-c9b7-4260-8127-c9f5bde7f68c}';
  IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler: TGUID = IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandlerGUID;
  IID_ICoreWebView2EnvironmentGUID = '{b96d755e-0319-4e92-a296-23436f46a1fc}';
  IID_ICoreWebView2Environment: TGUID = IID_ICoreWebView2EnvironmentGUID;
  IID_ICoreWebView2Environment2GUID = '{41F3632B-5EF4-404F-AD82-2D606C5A9A21}';
  IID_ICoreWebView2Environment2: TGUID = IID_ICoreWebView2Environment2GUID;
  IID_ICoreWebView2Environment3GUID = '{80a22ae3-be7c-4ce2-afe1-5a50056cdeeb}';
  IID_ICoreWebView2Environment3: TGUID = IID_ICoreWebView2Environment3GUID;
  IID_ICoreWebView2Environment4GUID = '{20944379-6dcf-41d6-a0a0-abc0fc50de0d}';
  IID_ICoreWebView2Environment4: TGUID = IID_ICoreWebView2Environment4GUID;
  IID_ICoreWebView2Environment5GUID = '{319e423d-e0d7-4b8d-9254-ae9475de9b17}';
  IID_ICoreWebView2Environment5: TGUID = IID_ICoreWebView2Environment5GUID;
  IID_ICoreWebView2Environment6GUID = '{e59ee362-acbd-4857-9a8e-d3644d9459a9}';
  IID_ICoreWebView2Environment6: TGUID = IID_ICoreWebView2Environment6GUID;
  IID_ICoreWebView2Environment7GUID = '{43C22296-3BBD-43A4-9C00-5C0DF6DD29A2}';
  IID_ICoreWebView2Environment7: TGUID = IID_ICoreWebView2Environment7GUID;
  IID_ICoreWebView2Environment8GUID = '{D6EB91DD-C3D2-45E5-BD29-6DC2BC4DE9CF}';
  IID_ICoreWebView2Environment8: TGUID = IID_ICoreWebView2Environment8GUID;
  IID_ICoreWebView2Environment9GUID = '{f06f41bf-4b5a-49d8-b9f6-fa16cd29f274}';
  IID_ICoreWebView2Environment9: TGUID = IID_ICoreWebView2Environment9GUID;
  IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandlerGUID = '{02fab84b-1428-4fb7-ad45-1b2e64736184}';
  IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler: TGUID = IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandlerGUID;
  IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandlerGUID = '{4e8a3389-c9d8-4bd2-b6b5-124fee6cc14d}';
  IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler: TGUID = IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandlerGUID;
  IID_ICoreWebView2_2GUID = '{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}';
  IID_ICoreWebView2_2: TGUID = IID_ICoreWebView2_2GUID;
  IID_ICoreWebView2_3GUID = '{A0D6DF20-3B92-416D-AA0C-437A9C727857}';
  IID_ICoreWebView2_3: TGUID = IID_ICoreWebView2_3GUID;
  IID_ICoreWebView2_4GUID = '{20d02d59-6df2-42dc-bd06-f98a694b1302}';
  IID_ICoreWebView2_4: TGUID = IID_ICoreWebView2_4GUID;
  IID_ICoreWebView2_5GUID = '{bedb11b8-d63c-11eb-b8bc-0242ac130003}';
  IID_ICoreWebView2_5: TGUID = IID_ICoreWebView2_5GUID;
  IID_ICoreWebView2_6GUID = '{499aadac-d92c-4589-8a75-111bfc167795}';
  IID_ICoreWebView2_6: TGUID = IID_ICoreWebView2_6GUID;
  IID_ICoreWebView2_7GUID = '{79c24d83-09a3-45ae-9418-487f32a58740}';
  IID_ICoreWebView2_7: TGUID = IID_ICoreWebView2_7GUID;
  IID_ICoreWebView2_8GUID = '{E9632730-6E1E-43AB-B7B8-7B2C9E62E094}';
  IID_ICoreWebView2_8: TGUID = IID_ICoreWebView2_8GUID;
  IID_ICoreWebView2_9GUID = '{4d7b2eab-9fdc-468d-b998-a9260b5ed651}';
  IID_ICoreWebView2_9: TGUID = IID_ICoreWebView2_9GUID;
  IID_ICoreWebView2_10GUID = '{b1690564-6f5a-4983-8e48-31d1143fecdb}';
  IID_ICoreWebView2_10: TGUID = IID_ICoreWebView2_10GUID;
  IID_ICoreWebView2_11GUID = '{0be78e56-c193-4051-b943-23b460c08bdb}';
  IID_ICoreWebView2_11: TGUID = IID_ICoreWebView2_11GUID;
  IID_ICoreWebView2_12GUID = '{35D69927-BCFA-4566-9349-6B3E0D154CAC}';
  IID_ICoreWebView2_12: TGUID = IID_ICoreWebView2_12GUID;
  IID_ICoreWebView2_13GUID = '{F75F09A8-667E-4983-88D6-C8773F315E84}';
  IID_ICoreWebView2_13: TGUID = IID_ICoreWebView2_13GUID;
  IID_ICoreWebView2_14GUID = '{6DAA4F10-4A90-4753-8898-77C5DF534165}';
  IID_ICoreWebView2_14: TGUID = IID_ICoreWebView2_14GUID;
  IID_ICoreWebView2_15GUID = '{517B2D1D-7DAE-4A66-A4F4-10352FFB9518}';
  IID_ICoreWebView2_15: TGUID = IID_ICoreWebView2_15GUID;
  IID_ICoreWebView2_16GUID = '{0EB34DC9-9F91-41E1-8639-95CD5943906B}';
  IID_ICoreWebView2_16: TGUID = IID_ICoreWebView2_16GUID;

  IID_ICoreWebView2ContextMenuItemCollectionGUID = '{f562a2f5-c415-45cf-b909-d4b7c1e276d3}';
  IID_ICoreWebView2ContextMenuItemCollection: TGUID = IID_ICoreWebView2ContextMenuItemCollectionGUID;

  IID_ICoreWebView2ContextMenuItemGUID = '{7aed49e3-a93f-497a-811c-749c6b6b6c65}';
  IID_ICoreWebView2ContextMenuItem: TGUID = IID_ICoreWebView2ContextMenuItemGUID;

  IID_ICoreWebView2ContextMenuTargetGUID = '{b8611d99-eed6-4f3f-902c-a198502ad472}';
  IID_ICoreWebView2ContextMenuTarget: TGUID = IID_ICoreWebView2ContextMenuTargetGUID;

  IID_ICoreWebView2ContextMenuRequestedEventHandlerGUID = '{04d3fe1d-ab87-42fb-a898-da241d35b63c}';
  IID_ICoreWebView2ContextMenuRequestedEventHandler: TGUID = IID_ICoreWebView2ContextMenuRequestedEventHandlerGUID;

  IID_ICoreWebView2ContextMenuRequestedEventArgsGUID = '{a1d309ee-c03f-11eb-8529-0242ac130003}';
  IID_ICoreWebView2ContextMenuRequestedEventArgs: TGUID = IID_ICoreWebView2ContextMenuRequestedEventArgsGUID;

  IID_ICoreWebView2CustomItemSelectedEventHandlerGUID = '{49e1d0bc-fe9e-4481-b7c2-32324aa21998}';
  IID_ICoreWebView2CustomItemSelectedEventHandler: TGUID = IID_ICoreWebView2CustomItemSelectedEventHandlerGUID;

  IID_ICoreWebView2PrintSettingsGUID = '{377f3721-c74e-48ca-8db1-df68e51d60e2}';
  IID_ICoreWebView2PrintSettings: TGUID = IID_ICoreWebView2PrintSettingsGUID;
  IID_ICoreWebView2PrintToPdfCompletedHandlerGUID = '{ccf1ef04-fd8e-4d5f-b2de-0983e41b8c36}';
  IID_ICoreWebView2PrintToPdfCompletedHandler: TGUID = IID_ICoreWebView2PrintToPdfCompletedHandlerGUID;
  IID_ICoreWebView2PrintCompletedHandlerGUID = '{8FD80075-ED08-42DB-8570-F5D14977461E}';
  IID_ICoreWebView2PrintCompletedHandler: TGUID = IID_ICoreWebView2PrintCompletedHandlerGUID;
  IID_ICoreWebView2PrintToPdfStreamCompletedHandlerGUID = '{4C9F8229-8F93-444F-A711-2C0DFD6359D5}';
  IID_ICoreWebView2PrintToPdfStreamCompletedHandler: TGUID = IID_ICoreWebView2PrintToPdfStreamCompletedHandlerGUID;

  IID_ICoreWebView2DownloadStartingEventHandlerGUID = '{efedc989-c396-41ca-83f7-07f845a55724}';
  IID_ICoreWebView2DownloadStartingEventHandler: TGUID = IID_ICoreWebView2DownloadStartingEventHandlerGUID;
  IID_ICoreWebView2DownloadStartingEventArgsGUID = '{e99bbe21-43e9-4544-a732-282764eafa60}';
  IID_ICoreWebView2DownloadStartingEventArgs: TGUID = IID_ICoreWebView2DownloadStartingEventArgsGUID;
  IID_ICoreWebView2DownloadOperationGUID = '{3d6b6cf2-afe1-44c7-a995-c65117714336}';
  IID_ICoreWebView2DownloadOperation: TGUID = IID_ICoreWebView2DownloadOperationGUID;
  IID_ICoreWebView2StateChangedEventHandlerGUID = '{81336594-7ede-4ba9-bf71-acf0a95b58dd}';
  IID_ICoreWebView2StateChangedEventHandler: TGUID = IID_ICoreWebView2StateChangedEventHandlerGUID;
  IID_ICoreWebView2BytesReceivedChangedEventHandlerGUID = '{828e8ab6-d94c-4264-9cef-5217170d6251}';
  IID_ICoreWebView2BytesReceivedChangedEventHandler: TGUID = IID_ICoreWebView2BytesReceivedChangedEventHandlerGUID;

type
  {$IFDEF LCLLIB}
  PUInt64 = ^UInt64;
  PInt64 = ^Int64;
  {$ENDIF}

  PHCURSOR = ^HCURSOR;

  EventRegistrationToken = record
    Value: Int64;
  end;
  PEventRegistrationToken = ^EventRegistrationToken;

  COREWEBVIEW2_COLOR = record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PCOREWEBVIEW2_COLOR = ^COREWEBVIEW2_COLOR;

  ICoreWebView2Controller = interface;
  ICoreWebView2CompositionController = interface;

  COREWEBVIEW2_MOVE_FOCUS_REASON = Cardinal;
  PCOREWEBVIEW2_MOVE_FOCUS_REASON = ^COREWEBVIEW2_MOVE_FOCUS_REASON;

  COREWEBVIEW2_PDF_TOOLBAR_ITEMS = Cardinal;
  PCOREWEBVIEW2_PDF_TOOLBAR_ITEMS = ^COREWEBVIEW2_PDF_TOOLBAR_ITEMS;

  COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND = Cardinal;
  PCOREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND = ^COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND;

  COREWEBVIEW2_DOWNLOAD_STATE = Cardinal;
  PCOREWEBVIEW2_DOWNLOAD_STATE = ^COREWEBVIEW2_DOWNLOAD_STATE;

  COREWEBVIEW2_WEB_ERROR_STATUS = Cardinal;
  PCOREWEBVIEW2_WEB_ERROR_STATUS = ^COREWEBVIEW2_WEB_ERROR_STATUS;

  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT = Cardinal;
  PCOREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT = ^COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT;

  COREWEBVIEW2_WEB_RESOURCE_CONTEXT = Cardinal;
  PCOREWEBVIEW2_WEB_RESOURCE_CONTEXT = ^COREWEBVIEW2_WEB_RESOURCE_CONTEXT;

  COREWEBVIEW2_KEY_EVENT_KIND = Cardinal;
  PCOREWEBVIEW2_KEY_EVENT_KIND = ^COREWEBVIEW2_KEY_EVENT_KIND;

  COREWEBVIEW2_PERMISSION_KIND = Cardinal;
  PCOREWEBVIEW2_PERMISSION_KIND = ^COREWEBVIEW2_PERMISSION_KIND;

  COREWEBVIEW2_PERMISSION_STATE = Cardinal;
  PCOREWEBVIEW2_PERMISSION_STATE = ^COREWEBVIEW2_PERMISSION_STATE;

  COREWEBVIEW2_PHYSICAL_KEY_STATUS = Cardinal;
  PCOREWEBVIEW2_PHYSICAL_KEY_STATUS = ^COREWEBVIEW2_PHYSICAL_KEY_STATUS;

  COREWEBVIEW2_PRINT_ORIENTATION = Cardinal;
  PCOREWEBVIEW2_PRINT_ORIENTATION = ^COREWEBVIEW2_PRINT_ORIENTATION;

  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND = Cardinal;
  PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND = ^COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND;

  COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND = Cardinal;
  PCOREWEBVIEW2_CONTEXT_MENU_ITEM_KIND = ^COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND;

  COREWEBVIEW2_COOKIE_SAME_SITE_KIND = Cardinal;
  PCOREWEBVIEW2_COOKIE_SAME_SITE_KIND = ^COREWEBVIEW2_COOKIE_SAME_SITE_KIND;

  COREWEBVIEW2_PRINT_DIALOG_KIND = Cardinal;
  COREWEBVIEW2_PRINT_STATUS = Cardinal;

  ICoreWebView2 = interface;

  ICoreWebView2EnvironmentOptions = interface(IUnknown)
    [IID_ICoreWebView2EnvironmentOptionsGUID]
    function get_AdditionalBrowserArguments(additionalBrowserArguments: PPWideChar): HRESULT; stdcall;
    function put_AdditionalBrowserArguments(additionalBrowserArguments: PWideChar): HRESULT; stdcall;
    function get_Language(language: PPWideChar): HRESULT; stdcall;
    function put_Language(language: PWideChar): HRESULT; stdcall;
    function get_TargetCompatibleBrowserVersion(targetCompatibleBrowserVersion: PPWideChar): HRESULT; stdcall;
    function put_TargetCompatibleBrowserVersion(targetCompatibleBrowserVersion: PWideChar): HRESULT; stdcall;
    procedure Placeholder_get_AllowSingleSignOnUsingOSPrimaryAccount; safecall;
    procedure Placeholder_put_AllowSingleSignOnUsingOSPrimaryAccount; safecall;
  end;

  ICoreWebView2Settings = interface(IUnknown)
    [IID_ICoreWebView2SettingsGUID]
    function get_IsScriptEnabled(isScriptEnabled: PBOOL): HRESULT; stdcall;
    function put_IsScriptEnabled(isScriptEnabled: BOOL): HRESULT; stdcall;
    function get_IsWebMessageEnabled(isWebMessageEnabled: PBOOL): HRESULT; stdcall;
    function put_IsWebMessageEnabled(isWebMessageEnabled: BOOL): HRESULT; stdcall;
    function get_AreDefaultScriptDialogsEnabled(areDefaultScriptDialogsEnabled: PBOOL): HRESULT; stdcall;
    function put_AreDefaultScriptDialogsEnabled(areDefaultScriptDialogsEnabled: BOOL): HRESULT; stdcall;
    function get_IsStatusBarEnabled(isStatusBarEnabled: PBOOL): HRESULT; stdcall;
    function put_IsStatusBarEnabled(isStatusBarEnabled: BOOL): HRESULT; stdcall;
    function get_AreDevToolsEnabled(areDevToolsEnabled: PBOOL): HRESULT; stdcall;
    function put_AreDevToolsEnabled(areDevToolsEnabled: BOOL): HRESULT; stdcall;
    function get_AreDefaultContextMenusEnabled(areDefaultContextMenusEnabled: PBOOL): HRESULT; stdcall;
    function put_AreDefaultContextMenusEnabled(areDefaultContextMenusEnabled: BOOL): HRESULT; stdcall;
    function get_AreHostObjectsAllowed(areHostObjectsAllowed: PBOOL): HRESULT; stdcall;
    function put_AreHostObjectsAllowed(areHostObjectsAllowed: BOOL): HRESULT; stdcall;
    function get_IsZoomControlEnabled(isZoomControlEnabled: PBOOL): HRESULT; stdcall;
    function put_IsZoomControlEnabled(isZoomControlEnabled: BOOL): HRESULT; stdcall;
    function get_IsIsBuiltInErrorPageEnabled(isIsBuiltInErrorPageEnabled: PBOOL): HRESULT; stdcall;
    function put_IsIsBuiltInErrorPageEnabled(isIsBuiltInErrorPageEnabled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2Settings2 = interface(ICoreWebView2Settings)
    [IID_ICoreWebView2Settings2GUID]
    function get_UserAgent(userAgent: PPWideChar): HRESULT; stdcall;
    function put_UserAgent(userAgent: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2Settings3 = interface(ICoreWebView2Settings2)
    [IID_ICoreWebView2Settings3GUID]
    function get_AreBrowserAcceleratorKeysEnabled(areBrowseracceleratorKeysEnabled: PBOOL): HRESULT; stdcall;
    function set_AreBrowserAcceleratorKeysEnabled(areBrowseracceleratorKeysEnabled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2Settings4 = interface(ICoreWebView2Settings3)
    [IID_ICoreWebView2Settings4GUID]
    function get_IsPasswordAutosaveEnabled(isPasswordAutosaveEnabled: PBOOL): HRESULT; stdcall;
    function set_IsPasswordAutosaveEnabled(isPasswordAutosaveEnabled: BOOL): HRESULT; stdcall;
    function get_IsGeneralAutofillEnabled(isGeneralAutofillEnabled: PBOOL): HRESULT; stdcall;
    function set_IsGeneralAutofillEnabled(isGeneralAutofillEnabled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2Settings5 = interface(ICoreWebView2Settings4)
    [IID_ICoreWebView2Settings5GUID]
    function get_IsPinchZoomEnabled(isPinchZoomEnabled: PBOOL): HRESULT; stdcall;
    function set_IsPinchZoomEnabled(isPinchZoomEnabled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2Settings6 = interface(ICoreWebView2Settings5)
    [IID_ICoreWebView2Settings6GUID]
    function get_IsSwipeNavigationEnabled(isSwipeNavigationEnabled: PBOOL): HRESULT; stdcall;
    function set_IsSwipeNavigationEnabled(isSwipeNavigationEnabled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2Settings7 = interface(ICoreWebView2Settings6)
    [IID_ICoreWebView2Settings7GUID]
    function get_HiddenPdfToolbarItems(hidden_pdf_toolbar_items: PCOREWEBVIEW2_PDF_TOOLBAR_ITEMS): HRESULT; stdcall;
    function put_HiddenPdfToolbarItems(hidden_pdf_toolbar_items: COREWEBVIEW2_PDF_TOOLBAR_ITEMS): HRESULT; stdcall;
  end;

  ICoreWebView2NavigationCompletedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2NavigationCompletedEventArgsGUID]
    function get_IsSuccess(isSuccess: PBOOL): HRESULT; stdcall;
    function get_WebErrorStatus(status: PCOREWEBVIEW2_WEB_ERROR_STATUS): HRESULT; stdcall;
    function get_NavigationId(navigation_id: PUInt64): HRESULT; stdcall;
  end;

  ICoreWebView2HttpHeadersCollectionIterator = interface(IUnknown)
    [IID_ICoreWebView2HttpHeadersCollectionIteratorGUID]
    function GetCurrentHeader(name: PPWideChar; value: PPWideChar): HRESULT; stdcall;
    function get_HasCurrentHeader(hasCurrent: PBOOL): HRESULT; stdcall;
    function MoveNext(hasNext: PBOOL): HRESULT; stdcall;
  end;

  ICoreWebView2HttpResponseHeaders = interface(IUnknown)
    [IID_ICoreWebView2HttpResponseHeadersGUID]
    function AppendHeader(name: PWideChar; value: PWideChar): HRESULT; stdcall;
    function Contains(name: PWidechar; contains: PBOOL): HRESULT; stdcall;
    function GetHeader(name: PWideChar; value: PPWideChar): HRESULT; stdcall;
    function GetHeaders(name: PWideChar; var iterator: ICoreWebView2HttpHeadersCollectionIterator): HRESULT; stdcall;
    function GetIterator(var iterator: ICoreWebView2HttpHeadersCollectionIterator): HRESULT; stdcall;
  end;

  ICoreWebView2HttpRequestHeaders = interface(IUnknown)
    [IID_ICoreWebView2HttpRequestHeadersGUID]
    function GetHeader(name: PWideChar; value: PPWideChar): HRESULT; stdcall;
    function GetHeaders(name: PWideChar; var iterator: ICoreWebView2HttpHeadersCollectionIterator): HRESULT; stdcall;
    function Contains(name: PWidechar; contains: PBOOL): HRESULT; stdcall;
    function SetHeader(name: PWideChar; value: PWideChar): HRESULT; stdcall;
    function RemoveHeader(name: PWideChar): HRESULT; stdcall;
    function GetIterator(var iterator: ICoreWebView2HttpHeadersCollectionIterator): HRESULT; stdcall;
  end;

  ICoreWebView2WebResourceRequest = interface(IUnknown)
    [IID_ICoreWebView2WebResourceRequestGUID]
    function get_Uri(uri: PPWideChar): HRESULT; stdcall;
    function put_Uri(uri: PWideChar): HRESULT; stdcall;
    function get_Method(method: PPWideChar): HRESULT; stdcall;
    function put_Method(method: PWideChar): HRESULT; stdcall;
    function get_Content(var content: IStream): HRESULT; stdcall;
    function put_Content(content: IStream): HRESULT; stdcall;
    function get_Headers(var headers: ICoreWebView2HttpRequestHeaders): HRESULT; stdcall;
  end;

  ICoreWebView2TrySuspendCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2TrySuspendCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; isSuccessful: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2WebResourceResponse = interface(IUnknown)
    [IID_ICoreWebView2WebResourceResponseGUID]
    function get_Content(var content: IStream): HRESULT; stdcall;
    function put_Content(content: IStream): HRESULT; stdcall;
    function get_Headers(var headers: ICoreWebView2HttpResponseHeaders): HRESULT; stdcall;
    function get_StatusCode(statusCode: PUINT): HRESULT; stdcall;
    function put_StatusCode(statusCode: UInt): HRESULT; stdcall;
    function get_ReasonPhrase(reasonPhrase: PPWideChar): HRESULT; stdcall;
    function put_ReasonPhrase(reasonPhrase: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2Deferral = interface(IUnknown)
    [IID_ICoreWebView2DeferralGUID]
    function Complete: HRESULT; stdcall;
  end;

  ICoreWebView2WebResourceRequestedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2WebResourceRequestedEventArgsGUID]
    function get_Request(var request: ICoreWebView2WebResourceRequest): HRESULT; stdcall;
    function get_Response(var response: ICoreWebView2WebResourceResponse): HRESULT; stdcall;
    function put_Response(response: ICoreWebView2WebResourceResponse): HRESULT; stdcall;
    function GetDeferral(var deferral: ICoreWebView2Deferral): HRESULT; stdcall;
    function get_ResourceContext(context: PCOREWEBVIEW2_WEB_RESOURCE_CONTEXT): HRESULT; stdcall;
  end;

  ICoreWebView2AcceleratorKeyPressedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2AcceleratorKeyPressedEventArgsGUID]
    function get_KeyEventKind(keyEventKind: PCOREWEBVIEW2_KEY_EVENT_KIND): HRESULT; stdcall;
    function get_VirtualKey(virtualKey: PUINT): HRESULT; stdcall;
    function get_KeyEventLParam(lParam: PINT): HRESULT; stdcall;
    function get_PhysicalKeyStatus(physicalKeyStatus: PCOREWEBVIEW2_PHYSICAL_KEY_STATUS): HRESULT; stdcall;
    function get_Handled(handled: PBOOL): HRESULT; stdcall;
    function put_Handled(handled: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2PermissionRequestedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2PermissionRequestedEventArgsGUID]
    function get_Uri(uri: PPWideChar): HRESULT; stdcall;
    function get_PermissionKind(permissionKind: PCOREWEBVIEW2_PERMISSION_KIND): HRESULT; stdcall;
    function get_IsUserInitiated(isUserInitiated: PBOOL): HRESULT; stdcall;
    function get_State(state: PCOREWEBVIEW2_PERMISSION_STATE): HRESULT; stdcall;
    function put_State(state: COREWEBVIEW2_PERMISSION_STATE): HRESULT; stdcall;
    function GetDeferral(var deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

  ICoreWebView2NewWindowRequestedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2NewWindowRequestedEventArgsGUID]
    function get_Uri(uri: PPWideChar): HRESULT; stdcall;
    function put_NewWindow(newWindow: ICoreWebView2): HRESULT; stdcall;
    function get_NewWindow(var newWindow: ICoreWebView2): HRESULT; stdcall;
    function put_Handled(handled: BOOL): HRESULT; stdcall;
    function get_Handled(handled: PBOOL): HRESULT; stdcall;
    function get_IsUserInitiated(isUserInitiated: PBOOL): HRESULT; stdcall;
    function GetDeferral(var deferral: ICoreWebView2Deferral): HRESULT; stdcall;
    procedure Placeholder_WindowFeatures; safecall;
  end;

  ICoreWebView2DOMContentLoadedEventArgs = interface(IUnknown)
    [IID_ICoreWebView2DOMContentLoadedEventArgsGUID]
    function get_NavigationId(navigation_ID: PUInt64): HRESULT; stdcall;
  end;

  ICoreWebView2NavigationStartingEventArgs = interface(IUnknown)
    [IID_ICoreWebView2NavigationStartingEventArgsGUID]
    function get_Uri(uri: PPWideChar): HRESULT; stdcall;
    function get_IsUserInitiated(isUserInitiated: PBOOL): HRESULT; stdcall;
    function get_IsRedirected(isRedirected: PBOOL): HRESULT; stdcall;
    function get_RequestHeaders(var requestHeaders: ICoreWebView2HttpRequestHeaders): HRESULT; stdcall;
    function get_Cancel(cancel: PBOOL): HRESULT; stdcall;
    function put_Cancel(cancel: BOOL): HRESULT; stdcall;
    function get_NavigationId(navigation_id: PUInt64): HRESULT; stdcall;
  end;

  ICoreWebView2SourceChangedEventArgs = interface(IUnknown)
    function get_IsNewDocument(isNewDocument: PBOOL): HRESULT; stdcall;
  end;

  ICoreWebView2ContentLoadingEventArgs = interface(IUnknown)
    [IID_ICoreWebView2ContentLoadingEventArgsGUID]
    function get_IsErrorPage(isErrorPage: PBOOL): HRESULT; stdcall;
    function get_NavigationId(navigation_ID: PUInt64): HRESULT; stdcall;
  end;

  ICoreWebView2NavigationStartingEventHandler = interface(IUnknown)
    [IID_ICoreWebView2NavigationStartingEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2ContentLoadingEventHandler = interface(IUnknown)
    [IID_ICoreWebView2ContentLoadingEventHandlerGUID]
    function Invoke(webview: ICoreWebView2; args: ICoreWebView2ContentLoadingEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2SourceChangedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2SourceChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2SourceChangedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2HistoryChangedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2HistoryChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2NavigationCompletedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2NavigationCompletedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2WebResourceRequestedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2WebResourceRequestedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2WebResourceRequestedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2CallDevToolsProtocolMethodCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; returnObjectAsJson: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2ExecuteScriptCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2ExecuteScriptCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; resultObjectAsJson: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2AcceleratorKeyPressedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2AcceleratorKeyPressedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2Controller; args: ICoreWebView2AcceleratorKeyPressedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2NewWindowRequestedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2NewWindowRequestedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NewWindowRequestedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2DOMContentLoadedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2DOMContentLoadedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2DOMContentLoadedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2FocusChangedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2FocusChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2CapturePreviewCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2CapturePreviewCompletedHandlerGUID]
    function Invoke(res: HResult): HRESULT; stdcall;
  end;

  ICoreWebView2CursorChangedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2CursorChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2CompositionController; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2PermissionRequestedEventHandler = interface(IUnknown)
    [IID_ICoreWebView2PermissionRequestedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2PermissionRequestedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2 = interface(IUnknown)
    [IID_ICoreWebView2GUID]
    function get_Settings(var settings: ICoreWebView2Settings): HRESULT; stdcall;
    function get_Source(source: PPWideChar): HRESULT; stdcall;
    function Navigate(uri: PWideChar): HRESULT; stdcall;
    function NavigateToString(htmlContent: PWideChar): HRESULT; stdcall;
    function add_NavigationStarting(eventHandler: ICoreWebView2NavigationStartingEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_NavigationStarting(token: EventRegistrationToken): HRESULT; stdcall;
    function add_ContentLoading(eventHandler: ICoreWebView2ContentLoadingEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_ContentLoading(token: EventRegistrationToken): HRESULT; stdcall;
    function add_SourceChanged(eventHandler: ICoreWebView2SourceChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_SourceChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_HistoryChanged(eventHandler: ICoreWebView2HistoryChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_HistoryChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_NavigationCompleted(eventHandler: ICoreWebView2NavigationCompletedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_NavigationCompleted(token: EventRegistrationToken): HRESULT; stdcall;
    function add_FrameNavigationStarting(eventHandler: ICoreWebView2NavigationStartingEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_FrameNavigationStarting(token: EventRegistrationToken): HRESULT; stdcall;
    function add_FrameNavigationCompleted(eventHandler: ICoreWebView2NavigationCompletedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_FrameNavigationCompleted(token: EventRegistrationToken): HRESULT; stdcall;
    procedure Placeholder_add_ScriptDialogOpening; safecall;
    procedure Placeholder_remove_ScriptDialogOpening; safecall;
    function add_PermissionRequested(eventHandler: ICoreWebView2PermissionRequestedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_PermissionRequested(token: EventRegistrationToken): HRESULT; stdcall;
    procedure Placeholder_add_ProcessFailed; safecall;
    procedure Placeholder_remove_ProcessFailed; safecall;
    procedure Placeholder_AddScriptToExecuteOnDocumentCreated; safecall;
    procedure Placeholder_RemoveScriptToExecuteOnDocumentCreated; safecall;
    function ExecuteScript(javaScript: PWideChar; handler: ICoreWebView2ExecuteScriptCompletedHandler): HRESULT; stdcall;
    function CapturePreview(imageFormat: COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT; imageStream: IStream; handler: ICoreWebView2CapturePreviewCompletedHandler): HRESULT; stdcall;
    function Reload: HRESULT; stdcall;
    procedure Placeholder_PostWebMessageAsJSON; safecall;
    procedure Placeholder_PostWebMessageAsString; safecall;
    procedure Placeholder_add_WebMessageReceived; safecall;
    procedure Placeholder_remove_WebMessageReceived; safecall;
    function CallDevToolsProtocolMethod(methodName: PWideChar; parametersAsJson: PWideChar; handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler): HRESULT; stdcall;
    procedure Placeholder_get_BrowserProcessId; safecall;
    function get_CanGoBack(canGoBack: PBOOL): HRESULT; stdcall;
    function get_CanGoForward(canGoForward: PBOOL): HRESULT; stdcall;
    function GoBack: HRESULT; stdcall;
    function GoForward: HRESULT; stdcall;
    procedure Placeholder_GetDevToolsProtocolEventReceiver; safecall;
    function Stop: HRESULT; stdcall;
    function add_NewWindowRequested(eventHandler: ICoreWebView2NewWindowRequestedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_NewWindowRequested(token: EventRegistrationToken): HRESULT; stdcall;
    procedure Placeholder_add_DocumentTitleChanged; safecall;
    procedure Placeholder_remove_DocumentTitleChanged; safecall;
    procedure Placeholder_get_DocumentTitle; safecall;
    function AddHostObjectToScript(name: PWideChar; &object: PVariant): HRESULT; stdcall;
    function RemoveHostObjectFromScript(name: PWideChar): HRESULT; stdcall;
    function OpenDevToolsWindow: HRESULT; stdcall;
    procedure Placeholder_add_ContainsFullScreenElementChanged; safecall;
    procedure Placeholder_remove_ContainsFullScreenElementChanged; safecall;
    procedure Placeholder_get_ContainsFullScreenElement; safecall;
    function add_WebResourceRequested(eventHandler: ICoreWebView2WebResourceRequestedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_WebResourceRequested(token: EventRegistrationToken): HRESULT; stdcall;
    function AddWebResourceRequestedFilter(const uri: PWideChar; const resourceContext: COREWEBVIEW2_WEB_RESOURCE_CONTEXT): HRESULT; stdcall;
    procedure Placeholder_RemoveWebResourceRequestedFilter; safecall;
    procedure Placeholder_add_WindowCloseRequested; safecall;
    procedure Placeholder_remove_WindowCloseRequested; safecall;
  end;

  ICoreWebView2Cookie = interface(IUnknown)
    [IID_ICoreWebView2CookieGUID]
    function get_Name(name: PPWideChar): HRESULT; stdcall;
    function get_Value(value: PPWideChar): HRESULT; stdcall;
    function put_Value(value: PWideChar): HRESULT; stdcall;
    function get_Domain(value: PPWideChar): HRESULT; stdcall;
    function get_Path(value: PPWideChar): HRESULT; stdcall;
    function get_Expires(expires: PDouble): HRESULT; stdcall;
    function put_Expires(expires: Double): HRESULT; stdcall;
    function get_IsHttpOnly(isHttpOnly: PBOOL): HRESULT; stdcall;
    function put_IsHttpOnly(isHttpOnly: BOOL): HRESULT; stdcall;
    function get_SameSite(sameSite: PCOREWEBVIEW2_COOKIE_SAME_SITE_KIND): HRESULT; stdcall;
    function put_SameSite(sameSite: COREWEBVIEW2_COOKIE_SAME_SITE_KIND): HRESULT; stdcall;
    function get_IsSecure(isSecure: PBOOL): HRESULT; stdcall;
    function put_IsSecure(isSecure: BOOL): HRESULT; stdcall;
    function get_IsSession(isSession: PBOOL): HRESULT; stdcall;
  end;

  ICoreWebView2CookieList = interface(IUnknown)
    [IID_ICoreWebView2CookieListGUID]
    function get_Count(value: PUINT): HRESULT; stdcall;
    function GetValueAtIndex(index: UINT; var cookie: ICoreWebView2Cookie): HRESULT; stdcall;
  end;

  ICoreWebView2GetCookiesCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2GetCookiesCompletedHandlerGUID]
    function Invoke(res: HRESULT; cookieList: ICoreWebView2CookieList): HRESULT; stdcall;
  end;

  ICoreWebView2CookieManager = interface(IUnknown)
    [IID_ICoreWebView2CookieManagerGUID]
    function CreateCookie(name: PWideChar; value: PWideChar; domain: PWideChar; path: PWideChar; var cookie: ICoreWebView2Cookie): HRESULT; stdcall;
    function CopyCookie(cookieParam: ICoreWebView2Cookie; var cookie: ICoreWebView2Cookie): HRESULT; stdcall;
    function GetCookies(uri: PWideChar; handler: ICoreWebView2GetCookiesCompletedHandler): HRESULT; stdcall;
    function AddOrUpdateCookie(cookie: ICoreWebView2Cookie): HRESULT; stdcall;
    function DeleteCookie(cookie: ICoreWebView2Cookie): HRESULT; stdcall;
    function DeleteCookies(name: PWideChar; uri: PWideChar): HRESULT; stdcall;
    function DeleteCookiesWithDomainAndPath(name: PWideChar; domain: PWideChar; path: PWideChar): HRESULT; stdcall;
    function DeleteAllCookies: HRESULT; stdcall;
  end;

  ICoreWebView2Environment = interface;

  ICoreWebView2_2 = interface(ICoreWebView2)
    [IID_ICoreWebView2_2GUID]
    procedure Placeholder_add_WebResourceResponseReceived; safecall;
    procedure Placeholder_remove_WebResourceResponseReceived; safecall;
    function NavigateWithWebResourceRequest(request: ICoreWebView2WebResourceRequest): HRESULT; stdcall;
    function add_DOMContentLoaded(eventhandler: ICoreWebView2DOMContentLoadedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_DOMContentLoaded(token: EventRegistrationToken): HRESULT; stdcall;
    function get_CookieManager(var cookieManager: ICoreWebView2CookieManager): HRESULT; stdcall;
    function get_Environment(var environment: ICoreWebView2Environment): HRESULT; stdcall;
  end;

  ICoreWebView2_3 = interface(ICoreWebView2_2)
    [IID_ICoreWebView2_3GUID]
    function TrySuspend(handler: ICoreWebView2TrySuspendCompletedHandler): HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function get_IsSuspended(isSuspended: PBOOL): HRESULT; stdcall;
    function SetVirtualHostNameToFolderMapping(hostName: PWideChar; folderPath: PWideChar; accessKind: COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND): HRESULT; stdcall;
    function ClearVirtualHostNameToFolderMapping(hostName: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2DownloadOperation = interface;

  ICoreWebView2StateChangedEventHandler = interface
    [IID_ICoreWebView2StateChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2DownloadOperation; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2BytesReceivedChangedEventHandler = interface
    [IID_ICoreWebView2BytesReceivedChangedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2DownloadOperation; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2DownloadOperation = interface
    [IID_ICoreWebView2DownloadOperationGUID]
    function add_BytesReceivedChanged(eventHandler: ICoreWebView2BytesReceivedChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_BytesReceivedChanged(token: EventRegistrationToken): HRESULT; stdcall;
    procedure Placeholder_add_EstimatedEndTimeChanged; safecall;
    procedure Placeholder_remove_EstimatedEndTimeChanged; safecall;
    function add_StateChanged(eventHandler: ICoreWebView2StateChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_StateChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_Uri(uri: PPWideChar): HRESULT; stdcall;
    procedure Placeholder_get_ContentDisposition; safecall;
    procedure Placeholder_get_MimeType; safecall;
    function get_TotalBytesToReceive(totalBytesToReceive: PInt64): HRESULT; stdcall;
    function get_BytesReceived(bytesReceived: PInt64): HRESULT; stdcall;
    procedure Placeholder_get_EstimatedEndTime; safecall;
    function get_ResultFilePath(resultFilePath: PPWideChar): HRESULT; stdcall;
    function get_State(state: PCOREWEBVIEW2_DOWNLOAD_STATE): HRESULT; stdcall;
    procedure Placeholder_get_InterruptReason; safecall;
    function Cancel: HRESULT; safecall;
    function Pause: HRESULT; safecall;
    function Resume: HRESULT; safecall;
    function CanResume(canResume: PBOOL): HRESULT; safecall;
  end;

  ICoreWebView2DownloadStartingEventArgs = interface
    [IID_ICoreWebView2DownloadStartingEventArgsGUID]
    function get_DownloadOperation(var downloadOperation: ICoreWebView2DownloadOperation): HRESULT; stdcall;
    procedure Placeholder_get_Cancel; safecall;
    function put_Cancel(cancel: BOOL): HRESULT; stdcall;
    procedure Placeholder_get_ResultFilePath; safecall;
    function put_ResultFilePath(resultFilePath: PWideChar): HRESULT; stdcall;
    procedure Placeholder_get_Handled; safecall;
    function put_Handled(handled: BOOL): HRESULT; stdcall;
    function GetDeferral(var deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

  ICoreWebView2DownloadStartingEventHandler = interface
    [IID_ICoreWebView2DownloadStartingEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2DownloadStartingEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2_4 = interface(ICoreWebView2_3)
    [IID_ICoreWebView2_4GUID]
    procedure Placeholder_add_FrameCreated; safecall;
    procedure Placeholder_remove_FrameCreated; safecall;
    function add_DownloadStarting(eventHandler: ICoreWebView2DownloadStartingEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_DownloadStarting(token: EventRegistrationToken): HRESULT; stdcall;
  end;

  ICoreWebView2_5 = interface(ICoreWebView2_4)
    [IID_ICoreWebView2_5GUID]
    procedure Placeholder_add_ClientCertificateRequested; safecall;
    procedure Placeholder_remove_ClientCertificateRequested; safecall;
  end;

  ICoreWebView2_6 = interface(ICoreWebView2_5)
    [IID_ICoreWebView2_6GUID]
    function OpenTaskManagerWindow: HRESULT; stdcall;
  end;

  ICoreWebView2PrintSettings = interface
    [IID_ICoreWebView2PrintSettingsGUID]
    function get_Orientation(orientation: PCOREWEBVIEW2_PRINT_ORIENTATION): HRESULT; stdcall;
    function put_Orientation(orientation: COREWEBVIEW2_PRINT_ORIENTATION): HRESULT; stdcall;
    function get_ScaleFactor(scaleFactor: PDouble): HRESULT; stdcall;
    function put_ScaleFactor(scaleFactor: Double): HRESULT; stdcall;
    function get_PageWidth(pageWidth: PDouble): HRESULT; stdcall;
    function put_PageWidth(pageWidth: Double): HRESULT; stdcall;
    function get_PageHeight(pageHeight: PDouble): HRESULT; stdcall;
    function put_PageHeight(pageHeight: Double): HRESULT; stdcall;
    function get_MarginTop(marginTop: PDouble): HRESULT; stdcall;
    function put_MarginTop(marginTop: Double): HRESULT; stdcall;
    function get_MarginBottom(marginBottom: PDouble): HRESULT; stdcall;
    function put_MarginBottom(marginBottom: Double): HRESULT; stdcall;
    function get_MarginLeft(marginLeft: PDouble): HRESULT; stdcall;
    function put_MarginLeft(marginLeft: Double): HRESULT; stdcall;
    function get_MarginRight(marginRight: PDouble): HRESULT; stdcall;
    function put_MarginRight(marginRight: Double): HRESULT; stdcall;
    function get_ShouldPrintBackgrounds(shouldPrintBackgrounds: PBOOL): HRESULT; stdcall;
    function put_ShouldPrintBackgrounds(shouldPrintBackgrounds: BOOL): HRESULT; stdcall;
    function get_ShouldPrintSelectionOnly(shouldPrintSelectionOnly: PBOOL): HRESULT; stdcall;
    function put_ShouldPrintSelectionOnly(shouldPrintSelectionOnly: BOOL): HRESULT; stdcall;
    function get_ShouldPrintHeaderAndFooter(shouldPrintHeaderAndFooter: PBOOL): HRESULT; stdcall;
    function put_ShouldPrintHeaderAndFooter(shouldPrintHeaderAndFooter: BOOL): HRESULT; stdcall;
    function get_HeaderTitle(headerTitle: PPWideChar): HRESULT; stdcall;
    function put_HeaderTitle(headerTitle: PWideChar): HRESULT; stdcall;
    function get_FooterUri(footerUri: PPWideChar): HRESULT; stdcall;
    function put_FooterUri(footerUri: PWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2PrintToPdfCompletedHandler = interface
    [IID_ICoreWebView2PrintToPdfCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; isSuccessful: Boolean): HRESULT; stdcall;
  end;

  ICoreWebView2_7 = interface(ICoreWebView2_6)
    [IID_ICoreWebView2_7GUID]
    function PrintToPdf(resultfilePath: PWideChar; printSettings: ICoreWebView2PrintSettings; handler: ICoreWebView2PrintToPdfCompletedHandler): HRESULT; stdcall;
  end;

  ICoreWebView2_8 = interface(ICoreWebView2_7)
    [IID_ICoreWebView2_8GUID]
    procedure Placeholder_add_IsMutedChanged; safecall;
    procedure Placeholder_remove_IsMutedChanged; safecall;
    procedure Placeholder_get_IsMuted; safecall;
    procedure Placeholder_put_IsMuted; safecall;
    procedure Placeholder_add_IsDocumentPlayingAudioChanged; safecall;
    procedure Placeholder_remove_IsDocumentPlayingAudioChanged; safecall;
    procedure Placeholder_get_IsDocumentPlayingAudio; safecall;
  end;

  ICoreWebView2_9 = interface(ICoreWebView2_8)
    [IID_ICoreWebView2_9GUID]
    procedure Placeholder_add_IsDefaultDownloadDialogOpenChanged; safecall;
    procedure Placeholder_remove_IsDefaultDownloremoveialogOpenChanged; safecall;
    function get_IsDefaultDownloadDialogOpen(isDefaultDownloadDialogOpen: PBoolean): HRESULT; stdcall;
    function OpenDefaultDownloadDialog: HRESULT; stdcall;
    function CloseDefaultDownloadDialog: HRESULT; stdcall;
    procedure Placeholder_get_DefaultDownloadDialogCornerAlignment; safecall;
    procedure Placeholder_put_DefaultDownloadDialogCornerAlignment; safecall;
    procedure Placeholder_get_DefaultDownloadDialogMargin; safecall;
    procedure Placeholder_put_DefaultDownloadDialogMargin; safecall;
  end;

  ICoreWebView2_10 = interface(ICoreWebView2_9)
    [IID_ICoreWebView2_10GUID]
    procedure Placeholder_add_BasicAuthenticationRequested; safecall;
    procedure Placeholder_remove_BasicAuthenticationRequested; safecall;
  end;

  ICoreWebView2ContextMenuItemCollection = interface;
  ICoreWebView2ContextMenuItem = interface;

  ICoreWebView2CustomItemSelectedEventHandler = interface
    [IID_ICoreWebView2CustomItemSelectedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2ContextMenuItem; args: IUnknown): HRESULT; stdcall;
  end;

  ICoreWebView2ContextMenuItem = interface(IUnknown)
    [IID_ICoreWebView2ContextMenuItemGUID]
    function get_Name(value: PPWideChar): HRESULT; stdcall;
    function get_Label(value: PPWideChar): HRESULT; stdcall;
    function get_CommandId(value: PUINT): HRESULT; stdcall;
    function get_ShortcutKeyDescription(value: PPWideChar): HRESULT; stdcall;
    function get_Icon(var value: IStream): HRESULT; stdcall;
    function get_Kind(value: PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND): HRESULT; stdcall;
    function put_IsEnabled(value: BOOL): HRESULT; stdcall;
    function get_IsEnabled(value: PBOOL): HRESULT; stdcall;
    function put_IsChecked(value: BOOL): HRESULT; stdcall;
    function get_IsChecked(value: PBOOL): HRESULT; stdcall;
    function get_Children(var value: ICoreWebView2ContextMenuItemCollection): HRESULT; stdcall;
    function add_CustomItemSelected(eventHandler: ICoreWebView2CustomItemSelectedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_CustomItemSelected(token: EventRegistrationToken): HRESULT; stdcall;
  end;

  ICoreWebView2ContextMenuItemCollection = interface
    [IID_ICoreWebView2ContextMenuItemCollectionGUID]
    function get_Count(value: PUINT): HRESULT; stdcall;
    function GetValueAtIndex(index: UINT; var value: ICoreWebView2ContextMenuItem): HRESULT; stdcall;
    function RemoveValueAtIndex(index: UINT): HRESULT; stdcall;
    function InsertValueAtIndex(index: UINT; value: ICoreWebView2ContextMenuItem): HRESULT; stdcall;
  end;

  ICorewebView2ContextMenuTarget = interface
    [IID_ICoreWebView2ContextMenuTargetGUID]
    function get_Kind(value: PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND): HRESULT; stdcall;
    function get_IsEditable(value: PBOOL): HRESULT; stdcall;
    function get_IsRequestedForMainFrame(value: PBoolean): HRESULT; stdcall;
    function get_PageUri(value: PPWideChar): HRESULT; stdcall;
    function get_FrameUri(value: PPWideChar): HRESULT; stdcall;
    function get_HasLinkUri(value: PBOOL): HRESULT; stdcall;
    function get_LinkUri(value: PPWideChar): HRESULT; stdcall;
    function get_HasLinkText(value: PBOOL): HRESULT; stdcall;
    function get_LinkText(value: PPWideChar): HRESULT; stdcall;
    function get_HasSourceUri(value: PBOOL): HRESULT; stdcall;
    function get_SourceUri(value: PPWideChar): HRESULT; stdcall;
    function get_HasSelection(value: PBOOL): HRESULT; stdcall;
    function get_SelectionText(value: PPWideChar): HRESULT; stdcall;
  end;

  ICoreWebView2ContextMenuRequestedEventArgs = interface
    [IID_ICoreWebView2ContextMenuRequestedEventArgsGUID]
    function get_MenuItems(out value: ICoreWebView2ContextMenuItemCollection): HRESULT; stdcall;
    function get_ContextMenuTarget(out value: ICorewebView2ContextMenuTarget): HRESULT; stdcall;
    function get_Location(value: PPoint): HRESULT; stdcall;
    function put_SelectedCommandId(value: Integer): HRESULT; stdcall;
    function get_SelectedCommandId(value: PInt): HRESULT; stdcall;
    function put_Handled(value: Boolean): HRESULT; stdcall;
    function get_Handled(value: PBoolean): HRESULT; stdcall;
    function GetDeferral(var deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

  ICoreWebView2ContextMenuRequestedEventHandler = interface
    [IID_ICoreWebView2ContextMenuRequestedEventHandlerGUID]
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
  end;

  ICoreWebView2_11 = interface(ICoreWebView2_10)
    [IID_ICoreWebView2_11GUID]
    procedure Placeholder_CallDevToolsProtocolMethodForSession; safecall;
    function add_ContextMenuRequested(eventHandler: ICoreWebView2ContextMenuRequestedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_ContextMenuRequested(token: EventRegistrationToken): HRESULT; stdcall;
  end;

  ICoreWebView2_12 = interface(ICoreWebView2_11)
    [IID_ICoreWebView2_12GUID]
    procedure Placeholder_add_StatusBarTextChanged; safecall;
    procedure Placeholder_get_StatusBarText; safecall;
    procedure Placeholder_remove_StatusBarTextChanged; safecall;
  end;

  ICoreWebView2_13 = interface(ICoreWebView2_12)
    [IID_ICoreWebView2_13GUID]
    procedure Placeholder_get_Profile; safecall;
  end;

  ICoreWebView2_14 = interface(ICoreWebView2_13)
    [IID_ICoreWebView2_14GUID]
    procedure Placeholder_add_ServerCertificateErrorDetected; safecall;
    procedure Placeholder_ClearServerCertificateErrorActions; safecall;
    procedure Placeholder_remove_ServerCertificateErrorDetected; safecall;
  end;

  ICoreWebView2_15 = interface(ICoreWebView2_14)
    [IID_ICoreWebView2_15GUID]
    procedure Placeholder_add_FaviconChanged; safecall;
    procedure Placeholder_get_FaviconUri; safecall;
    procedure Placeholder_GetFavicon; safecall;
    procedure Placeholder_remove_FaviconChanged; safecall;
  end;

  ICoreWebView2PrintCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2PrintCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; printStatus: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2PrintToPdfStreamCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2PrintToPdfStreamCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; pdfStream: IStream): HRESULT; stdcall;
  end;

  ICoreWebView2_16 = interface(ICoreWebView2_15)
    [IID_ICoreWebView2_16GUID]
    function Print(printSettings: ICoreWebView2PrintSettings; handler: ICoreWebView2PrintCompletedHandler): HRESULT; stdcall;
    function ShowPrintUI(printDialogKind: COREWEBVIEW2_PRINT_DIALOG_KIND): HRESULT; stdcall;
    function PrintToPdfStream(printSettings: ICoreWebView2PrintSettings; handler: ICoreWebView2PrintToPdfStreamCompletedHandler): HRESULT; stdcall;
  end;

  ICoreWebView2Controller = interface(IUnknown)
    [IID_ICoreWebView2ControllerGUID]
    procedure Placeholder_get_IsVisible; safecall;
    function put_IsVisible(isVisible: BOOL): HRESULT; stdcall;
    function get_Bounds(bounds: PRect): HRESULT; stdcall;
    function put_Bounds(bounds: TRect): HRESULT; stdcall;
    function get_ZoomFactor(zoomFactor: PDouble): HRESULT; stdcall;
    function put_ZoomFactor(zoomFactor: Double): HRESULT; stdcall;
    procedure Placeholder_add_ZoomFactorChanged; safecall;
    procedure Placeholder_remove_ZoomFactorChanged; safecall;
    procedure Placeholder_SetBoundsAndZoomFactor; safecall;
    function MoveFocus(reason: COREWEBVIEW2_MOVE_FOCUS_REASON): HRESULT; stdcall;
    procedure Placeholder_add_MoveFocusRequested; safecall;
    procedure Placeholder_remove_MoveFocusRequested; safecall;
    function add_GotFocus(eventHandler: ICoreWebView2FocusChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_GotFocus(token: EventRegistrationToken): HRESULT; stdcall;
    function add_LostFocus(eventHandler: ICoreWebView2FocusChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_LostFocus(token: EventRegistrationToken): HRESULT; stdcall;
    function add_AcceleratorKeyPressed(eventHandler: ICoreWebView2AcceleratorKeyPressedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_AcceleratorKeyPressed(token: EventRegistrationToken): HRESULT; stdcall;
    procedure Placeholder_get_ParentWindow; safecall;
    function put_ParentWindow(parentWindow: HWND): HRESULT; stdcall;
    function NotifyParentWindowPositionChanged: HRESULT; stdcall;
    function Close: HResult; stdcall;
    function get_CoreWebView2(var coreWebView2: ICoreWebView2): HRESULT; stdcall;
  end;

  ICoreWebView2Controller2 = interface(ICoreWebView2Controller)
    [IID_ICoreWebView2Controller2GUID]
    function get_DefaultBackgroundColor(backgroundColor: PCOREWEBVIEW2_COLOR): HRESULT; stdcall;
    function put_DefaultBackgroundColor(backgroundColor: COREWEBVIEW2_COLOR): HRESULT; stdcall;
  end;

  ICoreWebView2Controller3 = interface(ICoreWebView2Controller2)
    [IID_ICoreWebView2Controller3GUID]
    procedure Placeholder_add_RasterizationScaleChanged; safecall;
    procedure Placeholder_get_BoundsMode; safecall;
    procedure Placeholder_get_RasterizationScale; safecall;
    procedure Placeholder_get_ShouldDetectMonitorScaleChanges; safecall;
    procedure Placeholder_put_BoundsMode; safecall;
    procedure Placeholder_put_RasterizationScale; safecall;
    procedure Placeholder_put_ShouldDetectMonitorScaleChanges; safecall;
    procedure Placeholder_remove_RasterizationScaleChanged; safecall;
  end;

  ICoreWebView2Controller4 = interface(ICoreWebView2Controller3)
    [IID_ICoreWebView2Controller4GUID]
    function get_AllowExternalDrop(value: PBOOL): HRESULT; stdcall;
    function put_AllowExternalDrop(value: BOOL): HRESULT; stdcall;
  end;

  ICoreWebView2CompositionController = interface(IUnknown)
    [IID_ICoreWebView2CompositionControllerGUID]
    procedure Placeholder_get_RootVisualTarget; safecall;
    procedure Placeholder_put_RootVisualTarget; safecall;
    procedure Placeholder_SendMouseInput; safecall;
    procedure Placeholder_SendPointerInput; safecall;
    function get_Cursor(cursor: PHCURSOR): HRESULT; safecall;
    procedure Placeholder_get_SystemCursorId; safecall;
    function add_CursorChanged(eventHandler: ICoreWebView2CursorChangedEventHandler; token: PEventRegistrationToken): HRESULT; stdcall;
    function remove_CursorChanged(token: EventRegistrationToken): HRESULT; safecall;
  end;

  ICoreWebView2CompositionController2 = interface(ICoreWebView2CompositionController)
    [IID_ICoreWebView2CompositionController2GUID]
    procedure Placeholder_get_AutomationProvider; safecall;
    procedure Placeholder_put_AutomationProvider; safecall;
  end;

  ICoreWebView2CompositionController3 = interface(ICoreWebView2CompositionController2)
    [IID_ICoreWebView2CompositionController3GUID]

  end;

  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandlerGUID]
    function Invoke(res: HRESULT; createdController: ICoreWebView2Controller): HRESULT; stdcall;
  end;

  ICoreWebView2Environment = interface(IUnknown)
    [IID_ICoreWebView2EnvironmentGUID]
    function CreateCoreWebView2Controller(parentWindow: HWND; handler: ICoreWebView2CreateCoreWebView2ControllerCompletedHandler): HRESULT; stdcall;
    function CreateWebResourceResponse(const Content: IStream; StatusCode: SYSINT; ReasonPhrase: PWideChar; Headers: PWideChar; out Response: ICoreWebView2WebResourceResponse): HRESULT; stdcall;
    procedure Placeholder_get_BrowserVersionString; safecall;
    procedure Placeholder_add_NewBrowserVersionAvailable; safecall;
    procedure Placeholder_remove_NewBrowserVersionAvailable; safecall;
  end;

  ICoreWebView2Environment2 = interface(ICoreWebView2Environment)
    [IID_ICoreWebView2Environment2GUID]
    function CreateWebResourceRequest(uri: PWideChar; method: PWideChar; postData: IStream; headers: PWideChar; out request: ICoreWebView2WebResourceRequest): HRESULT; stdcall;
  end;

  ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandlerGUID]
    function Invoke(errorCode: HRESULT; webView: ICoreWebView2CompositionController): HRESULT; stdcall;
  end;

  ICoreWebView2Environment3 = interface(ICoreWebView2Environment2)
    [IID_ICoreWebView2Environment3GUID]
    function CreateCoreWebView2CompositionController(parentWindow: HWND; handler: ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler): HRESULT; stdcall;
    procedure PlaceHolder_CreateCoreWebView2PointerInfo; safecall;
  end;

  ICoreWebView2Environment4 = interface(ICoreWebView2Environment3)
    [IID_ICoreWebView2Environment4GUID]
    procedure PlaceHolder_GetAutomationProviderForWindow; safecall;
  end;

  ICoreWebView2Environment5 = interface(ICoreWebView2Environment4)
    [IID_ICoreWebView2Environment5GUID]
    procedure PlaceHolder_add_BrowserProcessExited; safecall;
    procedure PlaceHolder_remove_BrowserProcessExited; safecall;
  end;

  ICoreWebView2Environment6 = interface(ICoreWebView2Environment5)
    [IID_ICoreWebView2Environment6GUID]
    function CreatePrintSettings(var printSettings: ICoreWebView2PrintSettings): HRESULT; stdcall;
  end;

  ICoreWebView2Environment7 = interface(ICoreWebView2Environment6)
    [IID_ICoreWebView2Environment7GUID]
    procedure PlaceHolder_get_UserDataFolder; safecall;
  end;

  ICoreWebView2Environment8 = interface(ICoreWebView2Environment7)
    [IID_ICoreWebView2Environment8GUID]
    procedure PlaceHolder_add_ProcessInfosChanged; safecall;
    procedure PlaceHolder_remove_ProcessInfosChanged; safecall;
    procedure PlaceHolder_GetProcessInfos; safecall;
  end;

  ICoreWebView2Environment9 = interface(ICoreWebView2Environment8)
    [IID_ICoreWebView2Environment9GUID]
    function CreateContextMenuItem(&label: PWideChar; iconStream: IStream; kind: COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND; out item: ICoreWebView2ContextMenuItem): HRESULT; stdcall;
  end;

  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = interface(IUnknown)
    [IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandlerGUID]
    function Invoke(result: HRESULT; webViewEnvironment: ICoreWebView2Environment): HRESULT; stdcall;
  end;

const
  {$IFDEF WIN64}
  CoreWebView2DLL = 'WebView2Loader_x64.dll';
  {$ELSE}
  CoreWebView2DLL = 'WebView2Loader_x86.dll';
  {$ENDIF}

var
  EdgeLoaded: Boolean = False;
  EdgeHandle: THandle;
  EdgeVersion: string = '';
  EdgeDLLPath: string = '';
  EdgeCustomPath: string = '';
  EdgeSilentErrors: Boolean = False;

var
  CreateCoreWebView2Environment: function(environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
  {%H-}CreateCoreWebView2EnvironmentWithOptions: function(browserExecutableFolder: PWideChar; userDataFolder: PWideChar; environmentOptions: ICoreWebView2EnvironmentOptions; environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
  GetAvailableCoreWebView2BrowserVersionString: function(browserExecutableFolder: PWideChar; versioninfo: PPWideChar): HRESULT; stdcall;
  {$ENDIF}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;
{$IFDEF EDGESUPPORT}
procedure InitializeEdge;
procedure UninitializeEdge;

const
  EErrorMessage = 'Could not initialize Edge Chromium! Please check Edge Chromium installation and verify correct version number.';
  EErrorMessageNoDLL = 'Could not initialize Edge Chromium! Please check if ' + CoreWebView2DLL + ' is correctly distributed and accessible.';
{$ENDIF}

implementation

uses
  Classes, FMX.TMSFNCGraphics, FMX.TMSFNCTypes, FMX.Forms, Types, SysUtils, FMX.TMSFNCUtils,
  FMX.TMSFNCWebBrowser, FMX.TMSFNCPersistence, DateUtils
  {$IFNDEF LCLLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl, FileUtil
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Menus
  {$IFDEF MSWINDOWS}
  ,FMX.Platform.Win
  {$ENDIF}
  {$ENDIF}
  {$IFDEF VCLLIB}
  , VCL.Menus
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,ShlObj, ShellAPI, FMX.Controls
  {$IFNDEF LCLLIB}
  ,IOUtils  ,ObjAuto
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Menus
  {$ENDIF}
  ,TypInfo, Variants
  {$ENDIF}
  ;

type
  TTMSFNCWebBrowserContextMenuItemOpen = class(TTMSFNCWebBrowserContextMenuItem);
{$IFDEF EDGESUPPORT}
  TSymbolNameEx = string[255];
  {$IFDEF FMXLIB}
  TControlOpen = class(TControl);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TMenuItemControl = class(TMenuItem);
  {$ENDIF}

  {$IFDEF LCLLIB}
  TTypeInfoFieldAccessor = record
  strict private
    FData: PByte;
  public
    procedure SetData(const Data: PByte); inline;
    class operator =(const Left, Right: TTypeInfoFieldAccessor): Boolean; inline;
    function UTF8Length: integer; inline;
    function ToString: string;
    function ToShortUTF8String: ShortString; inline;
    function ToByteArray: TBytes;
    function Tail: PByte; inline;
  end;

  PMethodInfoHeader = ^TMethodInfoHeader;
  TMethodInfoHeader = packed record
    Len: Word;
    Addr: Pointer;
    Name: TSymbolNameEx;
    function NameFld: TTypeInfoFieldAccessor;
  end;

  PReturnInfo = ^TReturnInfo;
  TReturnInfo = packed record
    Version: Byte; // Must be 3
    CallingConvention: TCallConv;
    ReturnType: ^PTypeInfo;
    ParamSize: Word;
    ParamCount: Byte;
  end;

  PParamInfo = ^TParamInfo;
  TParamInfo = packed record
    Flags: TParamFlags;
    ParamType: ^PTypeInfo;
    Access: Word;
    Name: TSymbolNameEx;
    function NameFld: TTypeInfoFieldAccessor;
  end;
  {$ENDIF}

  TDispatchKind = (dkMethod, dkProperty, dkSubComponent);
  TDispatchInfo = record
    Instance: TObject;
    case Kind: TDispatchKind of
      dkMethod: (MethodInfo: PMethodInfoHeader);
      dkProperty: (PropInfo: PPropInfo);
      dkSubComponent: (Index: Integer);
  end;

  TDispatchInfos = array of TDispatchInfo;

  TTMSFNCObjectDispatch = class(TInterfacedPersistent, IDispatch)
  private
    FDispatchInfoCount: Integer;
    FDispatchInfos: TDispatchInfos;
    FInstance: TObject;
    FOwned: Boolean;
    function AllocDispID(AKind: TDispatchKind; Value: Pointer; AInstance: TObject): TDispID;
  protected
    function GetObjectDispatch(Obj: TObject): TTMSFNCObjectDispatch; virtual;
    function GetMethodInfo(const AName: string; var AInstance: TObject): PMethodInfoHeader; virtual;
    function GetPropInfo(const AName: string; var AInstance: TObject; var CompIndex: Integer): PPropInfo; virtual;
    property Instance: TObject read FInstance;
  public
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT;
      virtual; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; virtual; stdcall;
  public
    constructor Create(AInstance: TObject; Owned: Boolean = True);
    destructor Destroy; override;
  end;
{$ENDIF}

const
  {$IFDEF EDGESUPPORT}
  ofDispIDOffset = 100;
  {$ENDIF}
  BridgeName = 'bridge';
  LB = #13;
  DefaultHTML =
  '<!DOCTYPE html>'+
  '<html lang="en">'+

  '<head>'+
  '  <meta charset="utf-8">' + LB +
  '  <meta http-equiv="X-UA-Compatible" content="IE=edge">' + LB +
  '  <meta name="viewport" content="width=device-width, initial-scale=1">' + LB +
  '  <title>404 HTML Template by Colorlib</title>' + LB +
  '  <link href="https://fonts.googleapis.com/css?family=Montserrat:200,400,700" rel="stylesheet">' + LB +
  '  <link type="text/css" rel="stylesheet" href="css/style.css" />' + LB +
  '  <style>' + LB +
  '  * {'+ LB +
  '    -webkit-box-sizing: border-box;'+ LB +
  '            box-sizing: border-box;'+ LB +
  '  }'+ LB +

  '  html, body {'+ LB +
  '    background: #d7ebf6;' + LB +
  '    padding: 0;'+ LB +
  '    border: #000000;' + LB +
  '    width: 100%;' + LB +
  '    height: 100%;' + LB +
  '    margin: 0;' + LB +
  '    padding: 0;' + LB +
  '    border: solid #211b19;' + LB +
  '    border-width: thin;' + LB +
  '    overflow:hidden;' + LB +
  '    display:block;' + LB +
  '  }'+ LB +

  '  #notfound {'+ LB +
  '    position: relative;'+ LB +
  '    height: 100vh;'+ LB +
  '  }'+ LB +

  '  #notfound .notfound {'+ LB +
  '    position: absolute;'+ LB +
  '    left: 50%;'+ LB +
  '    top: 50%;'+ LB +
  '    -webkit-transform: translate(-50%, -50%);'+ LB +
  '        -ms-transform: translate(-50%, -50%);'+ LB +
  '            transform: translate(-50%, -50%);'+ LB +
  '  }'+ LB +

  '  .notfound {'+ LB +
  '    max-width: 520px;'+ LB +
  '    width: 100%;'+ LB +
  '    line-height: 1.4;'+ LB +
  '    text-align: center;'+ LB +
  '  }'+ LB +

  '  .notfound .notfound-404 {'+ LB +
  '    position: relative;'+ LB +
  '    height: 200px;'+ LB +
  '    margin: 0px auto 20px;'+ LB +
  '    z-index: -1;'+ LB +
  '  }' + LB +

  '  .notfound .notfound-404 h2 {'+ LB +
  '    font-family: ''Montserrat'', sans-serif;'+ LB +
  '    font-size: 28px;'+ LB +
  '    font-weight: 400;'+ LB +
  '    text-transform: uppercase;'+ LB +
  '    color: #211b19;'+ LB +
  '    background: #d7ebf6;'+ LB +
  '    padding: 10px 5px;'+ LB +
  '    margin: auto;'+ LB +
  '    display: inline-block;'+ LB +
  '    position: absolute;'+ LB +
  '    bottom: 0px;'+ LB +
  '    left: 0;'+ LB +
  '    right: 0;'+ LB +
  '  }'+ LB +

  '  @media only screen and (max-width: 767px) {'+ LB +
  '    .notfound .notfound-404 h1 {'+ LB +
  '      font-size: 148px;'+ LB +
  '    }'+ LB +
  '  }'+ LB +

  '  @media only screen and (max-width: 480px) {'+ LB +
  '    .notfound .notfound-404 {'+ LB +
  '      height: 148px;'+ LB +
  '      margin: 0px auto 10px;'+ LB +
  '    }'+ LB +
  '    .notfound .notfound-404 h1 {'+ LB +
  '      font-size: 86px;'+ LB +
  '    }'+ LB +
  '    .notfound .notfound-404 h2 {'+ LB +
  '      font-size: 16px;'+ LB +
  '    }'+ LB +
  '    .notfound a {'+ LB +
  '      padding: 7px 15px;'+ LB +
  '      font-size: 14px;'+ LB +
  '    }'+ LB +
  '  }'+ LB +

  ' </style>' + LB +
  '</head>' + LB +

  '<body>' + LB +
  '  <div id="notfound">' + LB +
  '    <div class="notfound">' + LB +
  '      <div class="notfound-404">' + LB +
  '        <h2>Edge Chromium #VERSIONPLACEHOLDER is successfully initialized!' + LB +
  '      </div>' + LB +
  '    </div>' + LB +
  '  </div>' + LB +
  '</body>' + LB +
  '</html>';

type
  TTMSFNCWinWebBrowserService = class;

  TTMSFNCWinWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  TTMSFNCWinWebBrowser = class;

  {$IFNDEF EDGESUPPORT}
  ICoreWebView2Controller = interface
  end;
  {$ENDIF}

  {$IFDEF EDGESUPPORT}
  TCoreWebView2AcceleratorKeyPressedEventHandler = class(TInterfacedObject, ICoreWebView2AcceleratorKeyPressedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2Controller; args: ICoreWebView2AcceleratorKeyPressedEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2NavigationStartingEventHandler = class(TInterfacedObject, ICoreWebView2NavigationStartingEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2FocusChangedEventHandler = class(TInterfacedObject, ICoreWebView2FocusChangedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  protected
    procedure FocusChanged; virtual;
  public
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2GotFocusEventHandler = class(TCoreWebView2FocusChangedEventHandler)
  protected
    procedure FocusChanged; override;
  end;

  TCoreWebView2LostFocusEventHandler = class(TCoreWebView2FocusChangedEventHandler)
  protected
    procedure FocusChanged; override;
  end;

  TCoreWebView2NavigationCompletedEventHandler = class(TInterfacedObject, ICoreWebView2NavigationCompletedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2CapturePreviewCompletedHandler = class(TInterfacedObject, ICoreWebView2CapturePreviewCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(res: HRESULT): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2ExecuteScriptCompletedHandler = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
    FCompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent;
    FCallback: TNotifyEvent;
  public
    function Invoke(errorCode: HRESULT; resultObjectAsJson: PWideChar): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
  end;

  TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(res: HRESULT; webViewEnvironment: ICoreWebView2Environment): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2CreateCoreWebView2ControllerCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2ControllerCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(res: HRESULT; createdController: ICoreWebView2Controller): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(errorCode: HRESULT; webView: ICoreWebView2CompositionController): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2ContextMenuRequestedEventHandler = class(TInterfacedObject, ICoreWebView2ContextMenuRequestedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2CustomItemSelectedEventHandler  = class(TInterfacedPersistent, ICoreWebView2CustomItemSelectedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2ContextMenuItem; args: IUnknown): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2DOMContentLoadedEventHandler = class(TInterfacedObject, ICoreWebView2DOMContentLoadedEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2DOMContentLoadedEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2ContentLoadingEventHandler = class(TInterfacedObject, ICoreWebView2ContentLoadingEventHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2ContentLoadingEventArgs): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2GetCookiesCompletedHandler = class(TInterfacedObject, ICoreWebView2GetCookiesCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(res: HRESULT; cookielist: ICoreWebView2CookieList): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2PrintToPdfCompletedHandler = class(TInterfacedObject, ICoreWebView2PrintToPdfCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(errorCode: HRESULT; isSuccessful: Boolean): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2PrintCompletedHandler = class(TInterfacedObject, ICoreWebView2PrintCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(errorCode: HRESULT; printStatus: BOOL): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2PrintToPdfStreamCompletedHandler = class(TInterfacedObject, ICoreWebView2PrintToPdfStreamCompletedHandler)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
  public
    function Invoke(errorCode: HRESULT; pdfStream: IStream): HRESULT; stdcall;
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  end;

  TCoreWebView2RemoteObject = class(TPersistent)
  private
    FWebBrowser: TTMSFNCWinWebBrowser;
    FObjectMessage: string;
    procedure SetObjectMessage(const Value: string);
  public
    constructor Create(AWebBrowser: TTMSFNCWinWebBrowser);
  published
    property ObjectMessage: string read FObjectMessage write SetObjectMessage;
  end;

  TTMSFNCWinWebBrowserCachedFolderItem = class
  private
    FFolder: string;
    FInstance: TTMSFNCWinWebBrowser;
  public
    constructor Create(AFolder: string; AInstance: TTMSFNCWinWebBrowser);
    property Folder: string read FFolder write FFolder;
    property Instance: TTMSFNCWinWebBrowser read FInstance write FInstance;
  end;

  TTMSFNCWinWebBrowserCachedFolderList = TObjectList<TTMSFNCWinWebBrowserCachedFolderItem>;
  {$ENDIF}

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  TTMSFNCWinWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser, ITMSFNCCustomWebBrowserEx, ITMSFNCCustomWebBrowserSettings, ITMSFNCCustomWebBrowserCookies,
    ITMSFNCCustomWebBrowserPrint, ITMSFNCCustomWebBrowserContextMenu)
  {$IFDEF EDGESUPPORT}
  class var FEdgeLocation: string;
  {$ENDIF}
  private
    {$IFDEF EDGESUPPORT}
    FIsInitializing: Boolean;
    FImageStream, FBodyStream: TMemoryStream;
    FError: Boolean;
    FSaveURL: string;
    FCustomBridge: string;
    FWebViewVersion: Integer;
    FRemoteObjectDispatch: TTMSFNCObjectDispatch;
    FCustomObjectDispatch: TTMSFNCObjectDispatch;
    FWebBrowserEnvironment: ICoreWebView2Environment;
    FWebBrowserController: ICoreWebView2Controller;
    FWebBrowserWebView2: ICoreWebView2;
    FEventTokenLF, FEventTokenGF, FEventTokenNC, FEventTokenNS, FEventTokenA, FEventTokenCCM, FEventTokenDCL, FEventTokenCL: EventRegistrationToken;
    FLastTargetItem: TTMSFNCWebBrowserTargetItem;
    {$ENDIF}
    FContextMenuList: TTMSFNCWebBrowserContextMenuItemList;
    FEnableAcceleratorKeys: Boolean;
    FBlockEvents: Boolean;
    FExternalBrowser: Boolean;
    FURL: string;
    FWebControl: TTMSFNCCustomWebBrowser;
    FCacheFolderName: string;
    FAutoClearCache: Boolean;
    FCacheFolder: string;
    FFullCacheFolderName: string;
    {$IFDEF EDGESUPPORT}
    function GetWebBrowserEnvironment: ICoreWebView2Environment;
    function GetWebBrowserWebView2: ICoreWebView2;
    {$ENDIF}
    function GetContextMenuList: TTMSFNCWebBrowserContextMenuItemList;
  protected
    function GetUserAgent: string;
    function GetCacheFolderName: string;
    function GetAutoClearCache: Boolean;
    {$IFDEF EDGESUPPORT}
    function GetWebViewVersion: Integer;
    function SetupWebView(res: HRESULT; createdController: ICoreWebView2Controller): HRESULT;
    {$ENDIF}
    procedure SetCacheFolderName(const Value: string);
    function GetCacheFolder: string;
    procedure SetFocus;
    procedure ShowDebugConsole;
    procedure SetCacheFolder(const Value: string);
    procedure SetAutoClearCache(const AutoClearCache: Boolean);
    procedure UpdateCacheFolderName;
    procedure RemoveCacheFolder;
    procedure WaitForInitialization;
    procedure LoadDefaultHTML;
    procedure InternalLoadDocumentFromStream(const Stream: TStream);
    procedure SetURL(const AValue: string);
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetUserAgent(const AValue: string);
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
    procedure Initialize;
    procedure DeInitialize;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    procedure SetEnableScript(const Value: Boolean);
    procedure OpenTaskManagerWindow;
    function GetEnableScript: Boolean;
    procedure NavigateWithData(AURI: string; AMethod: string; ABody: string; AHeaders: TStrings = nil); overload;
    procedure NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil); overload;
    procedure GetPopupMenuItems(APopupMenu: TPopupMenu; AList: TTMSFNCWebBrowserContextMenuItemList);
    {$IFDEF EDGESUPPORT}
    function GetControlHandle: HWND;
    function GetWebBrowserController: ICoreWebView2Controller;
    function GetContextMenuItems(AParentItem: TTMSFNCWebBrowserContextMenuItem; AItemCollection: ICoreWebView2ContextMenuItemCollection; AList: TTMSFNCWebBrowserContextMenuItemList): HRESULT;
    function CompareContextMenu(AItemCollection: ICoreWebView2ContextMenuItemCollection; AList: TTMSFNCWebBrowserContextMenuItemList; AParentItem: TTMSFNCWebBrowserContextMenuItem = nil): HRESULT;
    function CustomContextMenuItemSelected(AItem: ICoreWebView2ContextMenuItem): HRESULT;
    {$ENDIF}
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
    procedure GetCookies(AURI: string = '');
    procedure AddCookie(ACookie: TTMSFNCWebBrowserCookie);
    procedure DeleteAllCookies;
    procedure DeleteCookie(AName: string; ADomain: string; APath: string);
    procedure ShowPrintUI;
    procedure Print(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure Print; overload;
    procedure PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure PrintToPDFStream; overload;
    procedure PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings); overload;
    procedure PrintToPDF(AFileName: string); overload;

    {$IFDEF EDGESUPPORT}
    property WebBrowserController: ICoreWebView2Controller read GetWebBrowserController;
    property WebBrowserEnvironment: ICoreWebView2Environment read GetWebBrowserEnvironment;
    property WebBrowserWebView2: ICoreWebView2 read GetWebBrowserWebView2;
    {$ENDIF}
    property ContextMenuList: TTMSFNCWebBrowserContextMenuItemList read GetContextMenuList;

  public
    constructor Create(const AWebControl: TTMSFNCCustomWebBrowser);
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
  end;

var
  WebBrowserService: ITMSFNCWebBrowserService;
  {$IFDEF EDGESUPPORT}
  FCachedFolderList: TTMSFNCWinWebBrowserCachedFolderList;
  {$ENDIF}

procedure RegisterWebBrowserService;
begin
  if not TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
  begin
    WebBrowserService := TTMSFNCWinWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCWinWebBrowser.GetBrowserInstance: IInterface;
begin
  {$IFDEF EDGESUPPORT}
  Result := WebBrowserController;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.GetContextMenuItems(AParentItem: TTMSFNCWebBrowserContextMenuItem; AItemCollection: ICoreWebView2ContextMenuItemCollection; AList: TTMSFNCWebBrowserContextMenuItemList): HRESULT;
var
  si: TTMSFNCWebBrowserContextMenuItem;
  e, c: BOOL;
  k, cid: Integer;
  n, l, skd: PWideChar;
  cnt, I, cCnt: Integer;
  it: ICoreWebView2ContextMenuItem;
  cc: ICoreWebView2ContextMenuItemCollection;
begin
  if (FWebViewVersion >= 11) and Assigned(AList) then
  begin
    cnt := -1;

    Result := AItemCollection.get_Count(@cnt);
    if cnt >= 0 then
    begin
      for I := 0 to cnt - 1 do
      begin
        it := nil;
        Result := AItemCollection.GetValueAtIndex(I, it);
        if Assigned(it) then
        begin
          si := TTMSFNCWebBrowserSystemContextMenuItem.Create;
          TTMSFNCWebBrowserContextMenuItemOpen(si).ParentItem := AParentItem;
          TTMSFNCWebBrowserContextMenuItemOpen(si).OriginalIndex := I;

          it.get_Name(@n);
          TTMSFNCWebBrowserContextMenuItemOpen(si).Name := n;
          CoTaskMemFree(n);

          it.get_Name(@l);
          TTMSFNCWebBrowserContextMenuItemOpen(si).Text := l;
          CoTaskMemFree(l);

          it.get_CommandId(@cid);
          TTMSFNCWebBrowserContextMenuItemOpen(si).CommandId := cid;

          it.get_ShortcutKeyDescription(@skd);
          TTMSFNCWebBrowserContextMenuItemOpen(si).ShortcutKeyDescription := skd;
          CoTaskMemFree(skd);

          it.get_IsEnabled(@e);
          TTMSFNCWebBrowserContextMenuItemOpen(si).Enabled := e;
          it.get_IsChecked(@c);
          TTMSFNCWebBrowserContextMenuItemOpen(si).Checked := c;

          it.get_Kind(@k);
          TTMSFNCWebBrowserContextMenuItemOpen(si).Kind := TTMSFNCWebBrowserContextMenuItemKind(k);

          if TTMSFNCWebBrowserContextMenuItemOpen(si).Kind = ikSubMenu then
          begin
            cCnt := -1;
            it.get_Children(cc);
            if Assigned(cc) then
            begin
              cc.get_Count(@cCnt);
              if cCnt >= 0 then
                GetContextMenuItems(si, cc, TTMSFNCWebBrowserContextMenuItemOpen(si).Children);
            end;
          end;

          AList.Add(TTMSFNCWebBrowserSystemContextMenuItem(si));
        end;
      end;
    end;
  end
  else
    Result := E_NOTIMPL;
end;
{$ENDIF}

function TTMSFNCWinWebBrowser.GetContextMenuList: TTMSFNCWebBrowserContextMenuItemList;
begin
  if not Assigned(FContextMenuList) then
    FContextMenuList := TTMSFNCWebBrowserContextMenuItemList.Create;

  Result := FContextMenuList;
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.GetControlHandle: HWND;
{$IFDEF FMXLIB}
var
  f: TCustomForm;
{$ENDIF}
begin
  Result := 0;
  if Assigned(FWebControl) then
  begin
    {$IFDEF CMNLIB}
    Result := FWebControl.Handle;
    {$ENDIF}
    {$IFDEF FMXLIB}
    f := TTMSFNCUtils.GetParentForm(FWebControl);
    if Assigned(f) then
      Result := WindowHandleToPlatform(f.Handle).Wnd
    else if (FWebControl.Parent is TFrame) and Assigned(Application.MainForm) then
    begin
      Result := WindowHandleToPlatform(Application.MainForm.Handle).Wnd;
    end;
    {$ENDIF}
  end;
end;
{$ENDIF}

procedure TTMSFNCWinWebBrowser.GetCookies(AURI: string = '');
{$IFDEF EDGESUPPORT}
var
  w2: ICoreWebView2_2;
  cm: ICoreWebView2CookieManager;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
  begin
    w2.get_CookieManager(cm);
    if Assigned(cm) then
    begin
      cm.GetCookies(PWideChar(AURI), TCoreWebView2GetCookiesCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := FEnableAcceleratorKeys;
end;

function TTMSFNCWinWebBrowser.GetEnableContextMenu: Boolean;
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  Result := False;
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      s.get_AreDefaultContextMenusEnabled(@b);
      Result := b;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.GetEnableShowDebugConsole: Boolean;
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  Result := False;
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      s.get_AreDevToolsEnabled(@b);
      Result := b;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.GetEnableScript: Boolean;
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  Result := False;
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      b := False;
      s.get_IsScriptEnabled(@b);
      Result := b;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.SetEnableScript(const Value: Boolean);
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      b := Value;
      s.put_IsScriptEnabled(b);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

procedure TTMSFNCWinWebBrowser.GetPopupMenuItems(APopupMenu: TPopupMenu; AList: TTMSFNCWebBrowserContextMenuItemList);
var
  I: Integer;
  ci: TTMSFNCWebBrowserCustomContextMenuItem;

  procedure GetMenuItems(AMenuItem: TMenuItem; AListItem: TTMSFNCWebBrowserCustomContextMenuItem);
  var
    J: integer;
    mi: TMenuItem;
    si: TTMSFNCWebBrowserCustomContextMenuItem;
  begin
    {$IFDEF FMXLIB}
    for J := 0 to AMenuItem.ItemsCount - 1 do
    {$ENDIF}
    {$IFNDEF FMXLIB}
    for J := 0 to AMenuItem.Count - 1 do
    {$ENDIF}
    begin
      mi := AMenuItem.Items[J];
      si := TTMSFNCWebBrowserCustomContextMenuItem.Create;


      {$IFDEF FMXLIB}
      if mi.ItemsCount > 0 then
      {$ENDIF}
      {$IFNDEF FMXLIB}
      if mi.Count > 0 then
      {$ENDIF}
      begin
        si.Kind := ikSubMenu;
        GetMenuItems(mi, si);
      end
      {$IFDEF FMXLIB}
      else if mi.Text = '-' then
      {$ENDIF}
      {$IFNDEF FMXLIB}
      else if mi.Caption = '-' then
      {$ENDIF}
        si.Kind := ikSeperator
      else if mi.RadioItem then
        si.Kind := ikRadioButton
      {$IFDEF FMXLIB}
      else if Assigned(mi.Bitmap) and not mi.Bitmap.IsEmpty then
      {$ENDIF}
      {$IFNDEF FMXLIB}
      else if Assigned(mi.Bitmap) and not mi.Bitmap.Empty then           
      {$ENDIF}
      begin
        si.Kind := ikCommand;
        si.Icon.Assign(mi.Bitmap);
      end
      else
        si.Kind := ikCheckBox;
      {$IFDEF FMXLIB}
      si.Name := mi.Text;
      {$ENDIF}
      {$IFNDEF FMXLIB}
      si.Name := mi.Caption;
      {$ENDIF}

      si.Enabled := mi.Enabled;

      {$IFDEF FMXLIB}
      si.Checked := mi.IsChecked;
      {$ENDIF}
      {$IFNDEF FMXLIB}
      si.Checked := mi.Checked;
      {$ENDIF}

      TTMSFNCWebBrowserContextMenuItemOpen(si).InternalObject := mi;

      AListItem.Children.Add(si);
    end;
  end;

begin
  {$IFDEF FMXLIB}
  for I := 0 to APopupMenu.ItemsCount - 1 do
  {$ENDIF}
  {$IFNDEF FMXLIB}
  for I := 0 to APopupMenu.Items.Count - 1 do
  {$ENDIF}
  begin
    ci := TTMSFNCWebBrowserCustomContextMenuItem.Create;

    {$IFDEF FMXLIB}
    if APopupMenu.Items[I].ItemsCount > 0 then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if APopupMenu.Items[I].Count > 0 then
    {$ENDIF}
    begin
      ci.Kind := ikSubMenu;
      GetMenuItems(APopupMenu.Items[I], ci);
    end
    {$IFDEF FMXLIB}
    else if APopupMenu.Items[I].Text = '-' then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    else if APopupMenu.Items[I].Caption = '-' then
    {$ENDIF}
      ci.Kind := ikSeperator
    else if APopupMenu.Items[I].RadioItem then
      ci.Kind := ikRadioButton

    {$IFDEF FMXLIB}
    else if Assigned(APopupMenu.Items[I].Bitmap) and not APopupMenu.Items[I].Bitmap.IsEmpty then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    else if Assigned(APopupMenu.Items[I].Bitmap) and not APopupMenu.Items[I].Bitmap.Empty then
    {$ENDIF}
    begin
      ci.Kind := ikCommand;
      ci.Icon.Assign(APopupMenu.Items[I].Bitmap);
    end
    else
      ci.Kind := ikCheckBox;

    {$IFDEF FMXLIB}
    ci.Name := APopupMenu.Items[I].Text;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    ci.Name := APopupMenu.Items[I].Caption;
    {$ENDIF}

    ci.Enabled := APopupMenu.Items[I].Enabled;

    {$IFDEF FMXLIB}
    ci.Checked := APopupMenu.Items[I].IsChecked;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    ci.Checked := APopupMenu.Items[I].Checked;
    {$ENDIF}

    TTMSFNCWebBrowserContextMenuItemOpen(ci).InternalObject := APopupMenu.Items[I];

    AList.Add(ci);
  end;
end;

function TTMSFNCWinWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCWinWebBrowser.GetUserAgent: string;
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  s2: ICoreWebView2Settings2;
  ua: PWideChar;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    if FWebBrowserWebView2.get_Settings(s) = S_OK then
    begin
      if s.QueryInterface(IID_ICoreWebView2Settings2, s2) = S_OK then
      begin
        s2.get_UserAgent(@ua);
        {$IFDEF LCLLIB}
        Result := UTF8Encode(WideCharToString(ua));
        {$ELSE}
        Result := ua;
        {$ENDIF}
        CoTaskMemFree(ua);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.GoBack;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.GoBack;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.GoForward;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.GoForward;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Initialize;
{$IFDEF EDGESUPPORT}
var
  v: string;
  pth, pthTemp, pf, pfa: string;
  h: HRESULT;
  hw: HWND;

  function GetSpecialFolderPath(CSIDLFolder: Integer): string;
  var
    FilePath: array [0..MAX_PATH] of char;
  begin
    SHGetFolderPath(0, CSIDLFolder, 0, 0, FilePath);
    Result := FilePath;
  end;

  function ExecuteCommand(CommandLine: string): string;
  var
    SA: TSecurityAttributes;
    SI: TStartupInfo;
    PI: TProcessInformation;
    StdOutPipeRead, StdOutPipeWrite: THandle;
    WasOK: Boolean;
    Buffer: array[0..255] of AnsiChar;
    BytesRead: Cardinal;
    Handle: Boolean;
  begin
    Result := '';
    with SA do begin
      nLength := SizeOf(SA);
      bInheritHandle := True;
      lpSecurityDescriptor := nil;
    end;
    CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
    try
      with SI do
      begin
        FillChar(SI, SizeOf(SI), 0);
        cb := SizeOf(SI);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow := SW_HIDE;
        hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
        hStdOutput := StdOutPipeWrite;
        hStdError := StdOutPipeWrite;
      end;
      Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                              nil, nil, True, 0, nil,
                              nil, SI, PI);
      CloseHandle(StdOutPipeWrite);
      if Handle then
        try
          repeat
            WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
            if BytesRead > 0 then
            begin
              Buffer[BytesRead] := #0;
              Result := Result + string(Buffer);
            end;
          until not WasOK or (BytesRead = 0);
          WaitForSingleObject(PI.hProcess, INFINITE);
        finally
          CloseHandle(PI.hThread);
          CloseHandle(PI.hProcess);
        end;
    finally
      CloseHandle(StdOutPipeRead);
    end;
  end;

  procedure CopyDirectoryStructure(inDir: String; outDir: String);
  var
    s: TSearchRec;
    nInDir, nOutDir: String;
  begin
    ForceDirectories(outDir);

    if FindFirst(IncludeTrailingPathDelimiter(inDir) + '*',faAnyFile, s) = 0 then
    begin
      repeat
        if (s.Name <> '.') and (s.Name <> '..') then
        begin
          nInDir := TTMSFNCUtils.AddBackslash(inDir) + s.Name;
          nOutDir := TTMSFNCUtils.AddBackslash(outDir) + s.Name;
          if ((s.Attr and faDirectory) = faDirectory) then
          begin
            // Recurse into subdirectory in inDir
            CopyDirectoryStructure(nInDir,nOutDir);
          end
          else
            CopyFile(PChar(nInDir), PChar(nOutDir), False);
        end;
      until FindNext(s) <> 0;
    end;
    FindClose(s);
  end;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if FIsInitializing then
    Exit;

  if Assigned(FWebBrowserWebView2) and Assigned(FWebBrowserController) then
  begin
    hw := GetControlHandle;
    FWebBrowserController.put_ParentWindow(hw);
    UpdateVisible;
    Exit;
  end;

  if Assigned(CreateCoreWebView2EnvironmentWithOptions) then
  begin
    if FAutoClearCache and Assigned(FWebBrowserController) then
      ClearCache;

    FError := False;

    pth := '';

    if EdgeCustomPath = '' then
    begin
      if EdgeVersion = '' then
      begin
        if (FEdgeLocation <> '') and DirectoryExists(FEdgeLocation) then
          pth := FEdgeLocation
        else
        begin
          pf := TTMSFNCUtils.AddBackslash(GetSpecialFolderPath(CSIDL_PROGRAM_FILESX86)) + 'Microsoft\Edge\Application\';
          if DirectoryExists(pf) then
          begin
            try
              pfa := pf + 'msedge.exe';
              if FileExists(pfa) then
              begin
                v := Trim(ExecuteCommand('wmic.exe DATAFILE WHERE "NAME=' + Chr(39) + StringReplace(pfa, '\', '\\', [rfReplaceAll]) + Chr(39) + '" GET Version /value'));
                if Pos('Version=', v) > 0 then
                begin
                  v := StringReplace(v, 'Version=', '', [rfReplaceAll]);
                  pth := pf + v;
                  pthTemp := TTMSFNCUtils.AddBackslash(TTMSFNCUtils.GetTempPath) + 'Microsoft Edge ' + v;
                  if not DirectoryExists(pthTemp) then
                    CopyDirectoryStructure(pth, pthTemp);

                  pth := pthTemp;
                  FEdgeLocation := pthTemp;
                end;
              end;
            finally
            end;
          end;
        end;
      end;
    end
    else
      pth := EdgeCustomPath;

    if (pth <> '') and DirectoryExists(pth) then
      h := CreateCoreWebView2EnvironmentWithOptions(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(pth)), PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(FFullCacheFolderName)), nil, TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler.Create(Self))
    else
      h := CreateCoreWebView2EnvironmentWithOptions(nil, PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(FFullCacheFolderName)), nil, TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler.Create(Self));

    if h <> S_OK then
      FEdgeLocation := '';

    if h = S_OK then
    begin
      FIsInitializing := True;
      WaitForInitialization;
    end
    else if not FWebControl.IsDesignTime and not EdgeSilentErrors then
      TTMSFNCUtils.Message(EErrorMessage);

    if not FWebControl.IsDesignTime then
    begin
      if FError and not EdgeSilentErrors then
        TTMSFNCUtils.Message(EErrorMessage)
    end
    else
      LoadDefaultHTML;
  end
  else if not FWebControl.IsDesignTime then
  begin
    if not EdgeLoaded and not EdgeSilentErrors then
      TTMSFNCUtils.Message(EErrorMessageNoDLL)
    else if not EdgeSilentErrors then
      TTMSFNCUtils.Message(EErrorMessage);
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.IsFMXBrowser: Boolean;
begin
  {$IFDEF FMXLIB}
  Result := True;
  {$ENDIF}
  {$IFDEF CMNLIB}
  Result := False;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.LoadDefaultHTML;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebControl) and TTMSFNCCustomWebBrowserProtected(FWebControl).CanLoadDefaultHTML then
    LoadHTML(StringReplace(DefaultHTML, '#VERSIONPLACEHOLDER', EdgeVersion, [rfReplaceAll]));
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.LoadFile(AFile: String);
begin
  Navigate(AFile);
end;

procedure TTMSFNCWinWebBrowser.LoadHTML(AHTML: String);
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.NavigateToString(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(AHTML)));
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Navigate(const AURL: string);
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.Navigate(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(AURL)));
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.NativeBrowser: Pointer;
begin
  {$IFDEF EDGESUPPORT}
  Result := Pointer(FWebBrowserController);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
end;

function TTMSFNCWinWebBrowser.NativeEnvironment: Pointer;
begin
  {$IFDEF EDGESUPPORT}
  Result := Pointer(FWebBrowserEnvironment);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCWinWebBrowser.NavigateWithData(AURI, AMethod: string; ABodyStream: TStream; AHeaders: TStrings);
{$IFDEF EDGESUPPORT}
var
  req: ICoreWebView2WebResourceRequest;
  w2: ICoreWebView2_2;
  we2: ICoreWebView2Environment2;
  hs: ICoreWebView2HttpRequestHeaders;
  I: integer;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment2, we2) = S_OK) then
    begin
      if Assigned(FBodyStream) then
      begin
        FBodyStream.Free;
        FBodyStream := nil;
      end;
      FBodyStream := TMemoryStream.Create;
      FBodyStream.LoadFromStream(ABodyStream);

      if (we2.CreateWebResourceRequest(PWideChar(AURI), PWideChar(AMethod), TStreamAdapter.Create(FBodyStream, soReference), PWideChar(''), req) = S_OK) then
      begin
        if req.get_Headers(hs) = S_OK then
        begin
          for I := 0 to AHeaders.Count - 1 do
          begin
            hs.SetHeader(PWideChar(AHeaders.Names[I]), PWideChar(AHeaders.ValueFromIndex[I]));
          end;
        end;
        w2.NavigateWithWebResourceRequest(req);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.NavigateWithData(AURI, AMethod, ABody: string; AHeaders: TStrings);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(ABody);
  try
    ss.Position := 0;
    NavigateWithData(AURI, AMethod, ss, AHeaders);
  finally
    ss.Free;
  end;
end;

procedure TTMSFNCWinWebBrowser.OpenTaskManagerWindow;
{$IFDEF EDGESUPPORT}
var
  w6: ICoreWebView2_6;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_6, w6) = S_OK) then
  begin
    w6.OpenTaskManagerWindow;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Print(APrintSettings: TTMSFNCWebBrowserPrintSettings);
{$IFDEF EDGESUPPORT}
var
  w16: ICoreWebView2_16;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_16, w16) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      ps.put_Orientation(Integer(APrintSettings.Orientation));
      if APrintSettings.ScaleFactor > 0 then
        ps.put_ScaleFactor(APrintSettings.ScaleFactor);

      if APrintSettings.PageWidth > 0 then
        ps.put_PageWidth(APrintSettings.PageWidth);

      if APrintSettings.PageHeight > 0 then
        ps.put_PageHeight(APrintSettings.PageHeight);

      ps.put_MarginTop(APrintSettings.MarginTop);
      ps.put_MarginLeft(APrintSettings.MarginLeft);
      ps.put_MarginBottom(APrintSettings.MarginBottom);
      ps.put_MarginRight(APrintSettings.MarginRight);

      ps.put_ShouldPrintBackgrounds(APrintSettings.PrintBackgrounds);
      ps.put_ShouldPrintSelectionOnly(APrintSettings.PrintSelectionOnly);
      ps.put_ShouldPrintHeaderAndFooter(APrintSettings.PrintHeaderAndFooter);

      ps.put_HeaderTitle(PWideChar(APrintSettings.HeaderTitle));
      ps.put_FooterUri(PWideChar(APrintSettings.FooterURI));

      w16.Print(ps, TCoreWebView2PrintCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Print;
{$IFDEF EDGESUPPORT}
var
  w16: ICoreWebView2_16;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_16, w16) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      if Assigned(ps) then
        w16.Print(ps, TCoreWebView2PrintCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.PrintToPDF(AFileName: string);
{$IFDEF EDGESUPPORT}
var
  w7: ICoreWebView2_7;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_7, w7) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      if Assigned(ps) then
        w7.PrintToPdf(PWideChar(AFileName), ps, TCoreWebView2PrintToPdfCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings);
{$IFDEF EDGESUPPORT}
var
  w7: ICoreWebView2_7;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_7, w7) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      ps.put_Orientation(Integer(APrintSettings.Orientation));
      if APrintSettings.ScaleFactor > 0 then
        ps.put_ScaleFactor(APrintSettings.ScaleFactor);

      if APrintSettings.PageWidth > 0 then
        ps.put_PageWidth(APrintSettings.PageWidth);

      if APrintSettings.PageHeight > 0 then
        ps.put_PageWidth(APrintSettings.PageHeight);

      ps.put_MarginTop(APrintSettings.MarginTop);
      ps.put_MarginLeft(APrintSettings.MarginLeft);
      ps.put_MarginBottom(APrintSettings.MarginBottom);
      ps.put_MarginRight(APrintSettings.MarginRight);

      ps.put_ShouldPrintBackgrounds(APrintSettings.PrintBackgrounds);
      ps.put_ShouldPrintSelectionOnly(APrintSettings.PrintSelectionOnly);
      ps.put_ShouldPrintHeaderAndFooter(APrintSettings.PrintHeaderAndFooter);

      ps.put_HeaderTitle(PWideChar(APrintSettings.HeaderTitle));
      ps.put_FooterUri(PWideChar(APrintSettings.FooterURI));

      w7.PrintToPdf(PWideChar(AFileName), ps, TCoreWebView2PrintToPdfCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.PrintToPDFStream;
{$IFDEF EDGESUPPORT}
var
  w16: ICoreWebView2_16;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_16, w16) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      if Assigned(ps) then
        w16.PrintToPdfStream(ps, TCoreWebView2PrintToPdfStreamCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings);
{$IFDEF EDGESUPPORT}
var
  w16: ICoreWebView2_16;
  we6: ICoreWebView2Environment6;
  ps: ICoreWebView2PrintSettings;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_16, w16) = S_OK) then
  begin
    if (FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment6, we6) = S_OK) then
    begin
      we6.CreatePrintSettings(ps);
      ps.put_Orientation(Integer(APrintSettings.Orientation));
      if APrintSettings.ScaleFactor > 0 then
        ps.put_ScaleFactor(APrintSettings.ScaleFactor);

      if APrintSettings.PageWidth > 0 then
        ps.put_PageWidth(APrintSettings.PageWidth);

      if APrintSettings.PageHeight > 0 then
        ps.put_PageWidth(APrintSettings.PageHeight);

      ps.put_MarginTop(APrintSettings.MarginTop);
      ps.put_MarginLeft(APrintSettings.MarginLeft);
      ps.put_MarginBottom(APrintSettings.MarginBottom);
      ps.put_MarginRight(APrintSettings.MarginRight);

      ps.put_ShouldPrintBackgrounds(APrintSettings.PrintBackgrounds);
      ps.put_ShouldPrintSelectionOnly(APrintSettings.PrintSelectionOnly);
      ps.put_ShouldPrintHeaderAndFooter(APrintSettings.PrintHeaderAndFooter);

      ps.put_HeaderTitle(PWideChar(APrintSettings.HeaderTitle));
      ps.put_FooterUri(PWideChar(APrintSettings.FooterURI));

      w16.PrintToPdfStream(ps, TCoreWebView2PrintToPdfStreamCompletedHandler.Create(Self));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Reload;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.Reload;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.RemoveBridge(ABridgeName: string);
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) and (ABridgeName <> '') then
    FWebBrowserWebView2.RemoveHostObjectFromScript(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(ABridgeName)));

  if ABridgeName = FCustomBridge then
  begin
    FCustomBridge := '';
    if TTMSFNCCustomWebBrowserProtected(FWebControl).CanDestroyDispatch then
    begin
      if Assigned(FCustomObjectDispatch) then
        FreeAndNil(FCustomObjectDispatch);
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.SetEnableAcceleratorKeys(const AValue: Boolean);
begin
  FEnableAcceleratorKeys := AValue;
end;

procedure TTMSFNCWinWebBrowser.SetEnableContextMenu(const AValue: Boolean);
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      b := AValue;
      s.put_AreDefaultContextMenusEnabled(b);
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  b: BOOL;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.get_Settings(s);
    if Assigned(s) then
    begin
      b := AValue;
      s.put_AreDevToolsEnabled(b);
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
end;

procedure TTMSFNCWinWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCWinWebBrowser.SetUserAgent(const AValue: string);
{$IFDEF EDGESUPPORT}
var
  s: ICoreWebView2Settings;
  s2: ICoreWebView2Settings2;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    if FWebBrowserWebView2.get_Settings(s) = S_OK then
    begin
      if s.QueryInterface(IID_ICoreWebView2Settings2, s2) = S_OK then
        s2.put_UserAgent(PWideChar(AValue));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.ShowDebugConsole;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.OpenDevToolsWindow;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.ShowPrintUI;
{$IFDEF EDGESUPPORT}
var
  w16: ICoreWebView2_16;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_16, w16) = S_OK) then
  begin
    w16.ShowPrintUI(COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM);
//    COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER
//    COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.StopLoading;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.Stop;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.UpdateBounds;
{$IFDEF EDGESUPPORT}
var
  b: TRect;
  off, m: Single;
  {$IFDEF FMXLIB}
  bd: TRectF;
  {$ENDIF}
  g: TTMSFNCGraphics;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserController) and Assigned(FWebControl) then
  begin
    off := 0;
    m := 0;
    if FWebControl.IsDesigning then
    begin
      g := TTMSFNCGraphics.CreateBitmapCanvas;
      try
        g.Font.Size := 12;
        g.Font.Name := 'Montserrat';
        off := g.CalculateTextHeight(DESIGNTIMEMESSAGE) + FWebControl.ScalePaintValue(5);
      finally
        g.Free;
      end;

      m := FWebControl.ScalePaintValue(7);
    end;

    {$IFDEF FMXLIB}
    off := FWebControl.ScaleResourceValue(off);
    m := FWebControl.ScaleResourceValue(m);
    bd := FWebControl.AbsoluteRect;
    b := Bounds(Round(FWebControl.ScaleResourceValue(bd.Left) + m), Round(FWebControl.ScaleResourceValue(bd.Top) + off),
      Round(FWebControl.ScaleResourceValue(bd.Width) - m * 2), Round(FWebControl.ScaleResourceValue(bd.Height) - off - m));
    {$ELSE}
    b := Bounds(Round(m), Round(off), Round(FWebControl.Width - m * 2), Round(FWebControl.Height - off - m));
    {$ENDIF}
    FWebBrowserController.put_Bounds(b);
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.UpdateCacheFolderName;
begin
  if FCacheFolderName <> '' then
    FFullCacheFolderName := TTMSFNCUtils.AddBackslash(FCacheFolder) + FCacheFolderName
  else
    FFullCacheFolderName := TTMSFNCUtils.AddBackslash(FCacheFolder) + StringReplace(ExtractFileName(ParamStr(0)), ExtractFileExt(ParamStr(0)), '', []);
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.CompareContextMenu(AItemCollection: ICoreWebView2ContextMenuItemCollection; AList: TTMSFNCWebBrowserContextMenuItemList; AParentItem: TTMSFNCWebBrowserContextMenuItem = nil): HRESULT;
var
  I, cnt, cId, oI: Integer;
  we9: ICoreWebView2Environment9;
  it: ICoreWebView2ContextMenuItem;
  o: TTMSFNCWebBrowserContextMenuItemOpen;
  cc: ICoreWebView2ContextMenuItemCollection;
  ert: EventRegistrationToken;
  ms: TMemoryStream;
  icoStr: IStream;
  hglobal: THandle;
  hr: HRESULT;
  pcbWrite: LongInt;
  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION <= 28}
  gsize: LargeInt;
  {$ELSE}
  gsize: UInt64;
  {$IFEND}
  {$ENDIF}
  {$IFDEF LCLLIB}
  gsize:  UInt64;
  {$ENDIF}
  createNew: Boolean;
begin
  Result := E_UNEXPECTED;

  I := 0;
  oI := 0;

  while I <= AList.Count - 1 do
  begin
    createNew := True;

    o := TTMSFNCWebBrowserContextMenuItemOpen(AList[I]);
    if Assigned(AParentItem) then
      o.ParentItem := AParentItem;

    if (o.OriginalIndex >= 0) then
    begin
      createNew := False;

      if o.OriginalIndex < oI then
      begin
        createNew := True;
      end
      else
      begin
        while oI < o.OriginalIndex do
        begin
          AItemCollection.RemoveValueAtIndex(I);
          Inc(oI);
        end;
      end;

      oI := o.OriginalIndex + 1;
    end;

    if createNew then

    begin
      Result := FWebBrowserEnvironment.QueryInterface(IID_ICoreWebView2Environment9, we9);
      if (Result = S_OK) then
      begin
        it := nil;
        icoStr := nil;
        ms := TMemoryStream.Create;
        try
          o.Icon.SaveToStream(ms);
          ms.Position := 0;

          hglobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
          if (hglobal = 0) then
            raise Exception.Create('Could not allocate memory for image');

          icoStr := nil;
          hr := CreateStreamOnHGlobal(hglobal, True, icoStr);

          if hr = S_OK then
          begin
            {$WARNINGS OFF}
            icoStr.Write(ms.Memory, ms.Size, @pcbWrite);
            {$WARNINGS ON}
            icoStr.Seek(0, STREAM_SEEK_SET, gsize);
          end
          else
            GlobalFree(hglobal);

          Result := we9.CreateContextMenuItem(PWideChar(o.Name), icoStr, Integer(o.Kind), it);
        finally
          icoStr := nil;
          ms.Free;
        end;

        if Assigned(it) then
        begin
          it.put_IsEnabled(o.Enabled);
          it.put_IsChecked(o.Checked);
          it.get_CommandId(@cId);
          o.CommandId := cId;

          o.EventHandlerObject := TCoreWebView2CustomItemSelectedEventHandler.Create(Self);
          it.add_CustomItemSelected(o.EventHandlerObject as TCoreWebView2CustomItemSelectedEventHandler, @ert);

          AItemCollection.InsertValueAtIndex(I, it);
        end;
      end;
    end;

    if (o.Kind = ikSubMenu) then
    begin
      if (FWebViewVersion >= 11) then
      begin
        //Get children Item collection;
        if not Assigned(it) then
          Result := AItemCollection.GetValueAtIndex(I, it);

        if Assigned(it) then
        begin
          Result := it.get_Children(cc);
          CompareContextMenu(cc, o.Children, o);
        end;
      end
      else
        Result := E_NOTIMPL;
    end;
    Inc(I);
  end;

  cnt := 0;
  AItemCollection.get_Count(@cnt);

  while I < cnt do
  begin
    AItemCollection.RemoveValueAtIndex(I);
    Dec(cnt);
  end;
end;
{$ENDIF}

procedure TTMSFNCWinWebBrowser.UpdateEnabled;
begin
end;

procedure TTMSFNCWinWebBrowser.UpdateVisible;
begin
  {$IFDEF EDGESUPPORT}
  if not Assigned(FWebBrowserController) and Assigned(FWebControl) then
  begin
    if FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible then
    begin
      Initialize;
      WaitForInitialization;
    end;
  end;

  if Assigned(FWebBrowserController) and Assigned(FWebControl) then
    FWebBrowserController.put_IsVisible(FWebControl.Visible and TTMSFNCCustomWebBrowserProtected(FWebControl).CanBeVisible{$IFDEF FMXLIB} and FWebControl.ParentedVisible{$ENDIF});
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.InternalLoadDocumentFromStream(const Stream: TStream);
begin
  {$IFDEF EDGESUPPORT}
  if not Assigned(WebBrowserController) then
    Exit;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.WaitForInitialization;
//{$IFDEF EDGESUPPORT}
//var
//  i: Integer;
//{$ENDIF}
begin
//  {$IFDEF EDGESUPPORT}
//  i := 0;
//  while not Assigned(WebBrowserController) and ((i <= 60000) and not FError) do
//  begin
//    Application.ProcessMessages;
//    Sleep(1);
//    Inc(i);
//  end;
//  {$ENDIF}
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.GetWebBrowserController: ICoreWebView2Controller;
begin
  Result := FWebBrowserController;
end;

function TTMSFNCWinWebBrowser.GetWebBrowserEnvironment: ICoreWebView2Environment;
begin
  Result := FWebBrowserEnvironment;
end;

function TTMSFNCWinWebBrowser.GetWebBrowserWebView2: ICoreWebView2;
begin
  Result := FWebBrowserWebView2;
end;
function TTMSFNCWinWebBrowser.GetWebViewVersion: Integer;
begin
  Result := FWebViewVersion;
end;

{$ENDIF}

procedure TTMSFNCWinWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
{$IFDEF EDGESUPPORT}
var
  v: Variant;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) and (ABridgeName <> '') and Assigned(ABridgeObject) then
  begin
    FCustomObjectDispatch := TTMSFNCObjectDispatch.Create(ABridgeObject);
    v := FCustomObjectDispatch as IDispatch;
    FWebBrowserWebView2.AddHostObjectToScript(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(ABridgeName)), @v);
    FCustomBridge := ABridgeName;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.AddCookie(ACookie: TTMSFNCWebBrowserCookie);
{$IFDEF EDGESUPPORT}
var
  w2: ICoreWebView2_2;
  cm: ICoreWebView2CookieManager;
  c: ICoreWebView2Cookie;
  exp: Double;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
  begin
    w2.get_CookieManager(cm);
    if Assigned(cm) then
    begin
      cm.CreateCookie(PWideChar(Trim(ACookie.Name)), PWideChar(ACookie.Value), PWideChar(ACookie.Domain), PWideChar(ACookie.Path), c);
      if Assigned(c) then
      begin
        if ACookie.Expires <= 0 then
          exp := -1.0
        else
          exp := DateTimeToUnix(ACookie.Expires);

        c.put_Expires(exp);
        c.put_IsHttpOnly(ACookie.HTTPOnly);
        c.put_SameSite(Integer(ACookie.SameSite));
        c.put_IsSecure(ACookie.Secure);

        cm.AddOrUpdateCookie(c);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.BeforeChangeParent;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) and Assigned(FWebBrowserController) then
  begin
    FWebBrowserController.put_IsVisible(False);
    FWebBrowserController.put_ParentWindow(0);
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.CanGoBack: Boolean;
{$IFDEF EDGESUPPORT}
var
  p: BOOL;
{$ENDIF}
begin
  Result := False;
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    p := False;
    FWebBrowserWebView2.get_CanGoBack(@p);
    Result := p;
  end;
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.CanGoForward: Boolean;
{$IFDEF EDGESUPPORT}
var
  p: BOOL;
{$ENDIF}
begin
  Result := False;
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    p := False;
    FWebBrowserWebView2.get_CanGoForward(@p);
    Result := p;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.CaptureScreenShot;
{$IFDEF EDGESUPPORT}
var
  s: IStream;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
  begin
    if Assigned(FImageStream) then
    begin
      FImageStream.Free;
      FImageStream := nil;
    end;

    FImageStream := TMemoryStream.Create;
    s := TStreamAdapter.Create(FImageStream);
    FWebBrowserWebView2.CapturePreview(COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG, s, TCoreWebView2CapturePreviewCompletedHandler.Create(Self));
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.ClearCache;
begin
  RemoveCacheFolder;
  {$IFDEF EDGESUPPORT}
  FCachedFolderList.Add(TTMSFNCWinWebBrowserCachedFolderItem.Create(FFullCacheFolderName, Self));
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.CustomContextMenuItemSelected(AItem: ICoreWebView2ContextMenuItem): HRESULT;
var
  cId: Integer;
  ci: TTMSFNCWebBrowserCustomContextMenuItem;

  function GetMenuItem(AList: TTMSFNCWebBrowserContextMenuItemList; ACommandId: Integer): TTMSFNCWebBrowserCustomContextMenuItem;
  var
    I: Integer;
  begin
    Result := nil;

    for I := 0 to AList.Count - 1 do
    begin
      if TTMSFNCWebBrowserContextMenuItemOpen(AList[I]).CommandId = ACommandId then
      begin
        Result := TTMSFNCWebBrowserCustomContextMenuItem(AList[I]);
        Break;
      end;
      if TTMSFNCWebBrowserContextMenuItemOpen(AList[I]).Children.Count > 0 then
        Result := GetMenuItem(TTMSFNCWebBrowserContextMenuItemOpen(AList[I]).Children, ACommandId);
    end;
  end;
begin
  Result := AItem.get_CommandId(@cid);
  ci := GetMenuItem(ContextMenuList, cid);

  if Assigned(TTMSFNCWebBrowserContextMenuItemOpen(ci).InternalObject) then
    {$IFDEF FMXLIB}
    TControlOpen(TTMSFNCWebBrowserContextMenuItemOpen(ci).InternalObject).Click;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    TMenuItemControl(TTMSFNCWebBrowserContextMenuItemOpen(ci).InternalObject).Click;
    {$ENDIF}

  if Assigned(FWebControl) then
    TTMSFNCCustomWebBrowserProtected(FWebControl).DoCustomContextMenuItemSelected(ci);
end;
{$ENDIF}

constructor TTMSFNCWinWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
begin
  FExternalBrowser := False;
  FWebControl := AWebControl;
  FEnableAcceleratorKeys := True;

  FBlockEvents := True;
  FCacheFolder := TTMSFNCUtils.AddBackslash(TTMSFNCUtils.AddBackslash(TTMSFNCUtils.GetTempPath) +
    StringReplace(ExtractFileName(ParamStr(0)), ExtractFileExt(ParamStr(0)), '', []));
  FCacheFolderName := 'EdgeCache';
  FAutoClearCache := True;
  UpdateCacheFolderName;
end;

procedure TTMSFNCWinWebBrowser.DeInitialize;
{$IFDEF EDGESUPPORT}
var
  I: Integer;
  it: TTMSFNCWinWebBrowserCachedFolderItem;
  w2: ICoreWebView2_2;
  w11: ICoreWebView2_11;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if TTMSFNCCustomWebBrowserProtected(FWebControl).CanDestroyDispatch then
  begin
    if Assigned(FRemoteObjectDispatch) then
      FreeAndNil(FRemoteObjectDispatch);
    if Assigned(FCustomObjectDispatch) then
      FreeAndNil(FCustomObjectDispatch);
  end;

  if Assigned(FImageStream) then
  begin
    FImageStream.Free;
    FImageStream := nil;
  end;

  if Assigned(FBodyStream) then
  begin
    FBodyStream.Free;
    FBodyStream := nil;
  end;

  if Assigned(FWebBrowserWebView2) then
  begin
    FWebBrowserWebView2.remove_NavigationStarting(FEventTokenNS);
    FWebBrowserWebView2.remove_NavigationCompleted(FEventTokenNC);
    FWebBrowserWebView2.remove_ContentLoading(FEventTokenCL);

    if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
    begin
      w2.remove_DOMContentLoaded(FEventTokenDCL);
    end;

    if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_11, w11) = S_OK) then
    begin
      w11.remove_ContextMenuRequested(FEventTokenCCM);
    end;
  end;

  if Assigned(FWebBrowserController) then
  begin
    FWebBrowserController.put_IsVisible(False);
    FWebBrowserController.remove_AcceleratorKeyPressed(FEventTokenA);
    FWebBrowserController.remove_GotFocus(FEventTokenGF);
    FWebBrowserController.remove_LostFocus(FEventTokenLF);

    FWebBrowserController.Close;
    try
      FWebBrowserController := nil;
    finally
      Pointer(FWebBrowserController) := nil;
    end;
  end;

  if FAutoClearCache then
  begin
    RemoveCacheFolder;
    for I := FCachedFolderList.Count - 1 downto 0 do
    begin
      it := FCachedFolderList[I];
      if (it.Instance = Self) then
      begin
        FCachedFolderList.Delete(I);
        Break;
      end;
    end;
  end;

  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2 := nil;

  if Assigned(FWebBrowserEnvironment) then
    FWebBrowserEnvironment := nil;
  {$ENDIF}

  if Assigned(FContextMenuList) then
  begin
    FContextMenuList.Free;
    FContextMenuList := nil;
  end;
end;

procedure TTMSFNCWinWebBrowser.DeleteAllCookies;
{$IFDEF EDGESUPPORT}
var
  w2: ICoreWebView2_2;
  cm: ICoreWebView2CookieManager;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
  begin
    w2.get_CookieManager(cm);
    if Assigned(cm) then
    begin
      cm.DeleteAllCookies;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.DeleteCookie(AName, ADomain, APath: string);
{$IFDEF EDGESUPPORT}
var
  w2: ICoreWebView2_2;
  cm: ICoreWebView2CookieManager;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
  begin
    w2.get_CookieManager(cm);
    if Assigned(cm) then
    begin
      cm.DeleteCookiesWithDomainAndPath(PWideChar(AName), PWideChar(ADomain), PWideChar(APath));
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWinWebBrowser.SetFocus;
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserController) then
    FWebBrowserController.MoveFocus(COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC);
  {$ENDIF}
end;

{$IFDEF EDGESUPPORT}
function TTMSFNCWinWebBrowser.SetupWebView(res: HRESULT; createdController: ICoreWebView2Controller): HRESULT;
var
  v: Variant;
  w11: ICoreWebView2_11;
  w2: ICoreWebView2_2;
begin
  Result := S_FALSE;

  FWebViewVersion := 1;

  if (res = S_OK) then
  begin
    FWebBrowserController := createdController;
    if FWebBrowserController.get_CoreWebView2(FWebBrowserWebView2) = S_OK then
    begin
      if Assigned(FWebControl) and not FWebControl.IsDesigning then
      begin
        if not Assigned(FRemoteObjectDispatch) then
          FRemoteObjectDispatch := TTMSFNCObjectDispatch.Create(TCoreWebView2RemoteObject.Create(Self));

         v := (FRemoteObjectDispatch as IDispatch);

         FWebBrowserWebView2.AddHostObjectToScript(BridgeName, @v);
      end;

      FWebBrowserController.add_AcceleratorKeyPressed(TCoreWebView2AcceleratorKeyPressedEventHandler.Create(Self), @FEventTokenA);
      FWebBrowserWebView2.add_NavigationStarting(TCoreWebView2NavigationStartingEventHandler.Create(Self), @FEventTokenNS);
      FWebBrowserWebView2.add_NavigationCompleted(TCoreWebView2NavigationCompletedEventHandler.Create(Self), @FEventTokenNC);
      FWebBrowserController.add_GotFocus(TCoreWebView2GotFocusEventHandler.Create(Self), @FEventTokenGF);
      FWebBrowserController.add_LostFocus(TCoreWebView2LostFocusEventHandler.Create(Self), @FEventTokenLF);
      FWebBrowserWebView2.add_ContentLoading(TCoreWebView2ContentLoadingEventHandler.Create(Self), @FEventTokenCL);

      if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_2, w2) = S_OK) then
      begin
        w2.add_DOMContentLoaded(TCoreWebView2DOMContentLoadedEventHandler.Create(Self), @FEventTokenDCL);
        FWebViewVersion := 2;
      end;

      if (FWebBrowserWebView2.QueryInterface(IID_ICoreWebView2_11, w11) = S_OK) then
      begin
        w11.add_ContextMenuRequested(TCoreWebView2ContextMenuRequestedEventHandler.Create(Self), @FEventTokenCCM);
        FWebViewVersion := 11;
      end;

      FWebBrowserController.put_IsVisible(True);
      UpdateBounds;
      LoadDefaultHTML;
      Navigate;
      UpdateVisible;
      if Assigned(FWebControl) then
        TTMSFNCCustomWebBrowserProtected(FWebControl).Initialized;
      Result := S_OK;
    end;
  end;

  FError := FError or (Result <> S_OK) or (res <> S_OK);
  FIsInitializing := False;
end;
{$ENDIF}

procedure TTMSFNCWinWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
begin
  {$IFDEF EDGESUPPORT}
  if Assigned(FWebBrowserWebView2) then
    FWebBrowserWebView2.ExecuteScript(PWideChar({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(AScript)), TCoreWebView2ExecuteScriptCompletedHandler.Create(Self, ACompleteEvent, ACallback));
  {$ENDIF}
end;

function TTMSFNCWinWebBrowser.GetCacheFolder: string;
begin
  Result := FCacheFolder;
end;

function TTMSFNCWinWebBrowser.GetCacheFolderName: string;
begin
  Result := FCacheFolderName;
end;

function TTMSFNCWinWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := FAutoClearCache;
end;

procedure TTMSFNCWinWebBrowser.SetCacheFolder(const Value: string);
begin
  FCacheFolder := Value;
  UpdateCacheFolderName;
end;

procedure TTMSFNCWinWebBrowser.SetCacheFolderName(const Value: string);
begin
  FCacheFolderName := Value;
  UpdateCacheFolderName;
end;

procedure TTMSFNCWinWebBrowser.SetAutoClearCache(const AutoClearCache: Boolean);
begin
  FAutoClearCache := AutoClearCache;
end;

procedure TTMSFNCWinWebBrowser.RemoveCacheFolder;
{$IFDEF EDGESUPPORT}
var
  cnt: Integer;
  it: TTMSFNCWinWebBrowserCachedFolderItem;

  function HasCachedFolder: Boolean;
  var
    K: Integer;
  begin
    Result := False;
    for K := FCachedFolderList.Count - 1 downto 0 do
    begin
      it := FCachedFolderList[K];
      if (it.Instance <> Self) and (it.Folder = FFullCacheFolderName) then
      begin
        Result := True;
        Break;    
      end;         
    end;  
  end;
{$ENDIF}
begin
  {$IFDEF EDGESUPPORT}
  if FWebControl.IsDesigning or HasCachedFolder then
    Exit;

  cnt := 1;
  while DirectoryExists(FFullCacheFolderName) do
  begin
    if cnt > 50 then
      Break;

    {$IFDEF LCLLIB}
    DeleteDirectory(FFullCacheFolderName, True);
    {$ELSE}
    TDirectory.Delete(FFullCacheFolderName, True);
    {$ENDIF}

    if not DirectoryExists(FFullCacheFolderName) then
      Break;

    Inc(cnt);
    Sleep(1);
  end;
  RemoveDir(FCacheFolder);
  {$ENDIF}
end;

{ TTMSFNCWinWebBrowserService }

procedure TTMSFNCWinWebBrowserService.DeleteCookies;
begin
end;

function TTMSFNCWinWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCWinWebBrowser.Create(AValue);
end;

{$IFDEF EDGESUPPORT}

{ TCoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler }

constructor TCoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler.Invoke(
  errorCode: HRESULT; webView: ICoreWebView2CompositionController): HRESULT; stdcall;
var
  c: ICoreWebView2Controller;
begin
  Result := S_FALSE;
  if Assigned(FWebBrowser) then
  begin
    if Succeeded(webView.QueryInterface(IID_ICoreWebView2Controller, c)) then
      Result := FWebBrowser.SetupWebView(errorCode, c);
  end;
end;

{ TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler }

constructor TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2CreateCoreWebView2EnvironmentCompletedHandler.Invoke(res: HRESULT; webViewEnvironment: ICoreWebView2Environment): HRESULT; stdcall;
var
  w: HWND;
  //webViewEnvironment3: ICoreWebView2Environment3;
begin
  Result := S_FALSE;
  if res = S_OK then
  begin
    FWebBrowser.FWebBrowserEnvironment := webViewEnvironment;
    w := FWebBrowser.GetControlHandle;
    if w <> 0 then
    begin
//      if Succeeded(webViewEnvironment.QueryInterface(IID_ICoreWebView2Environment3, webViewEnvironment3)) then
//        webViewEnvironment3.CreateCoreWebView2CompositionController(w, TCoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler.Create(FWebBrowser))
//      else
        webViewEnvironment.CreateCoreWebView2Controller(w, TCoreWebView2CreateCoreWebView2ControllerCompletedHandler.Create(FWebBrowser));

      Result := S_OK;
    end;
  end;

  FWebBrowser.FError := (Result <> S_OK) or (res <> S_OK);
end;

{ TCoreWebView2CreateWebViewCompletedHandler }

constructor TCoreWebView2CreateCoreWebView2ControllerCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2CreateCoreWebView2ControllerCompletedHandler.Invoke(res: HRESULT; createdController: ICoreWebView2Controller): HRESULT; stdcall;
begin
  Result := S_FALSE;
  if Assigned(FWebBrowser) then
    Result := FWebBrowser.SetupWebView(res, createdController);
end;

procedure InitializeEdge;
var
  v: PWideChar;
begin
  if not Assigned(FCachedFolderList) then
    FCachedFolderList := TTMSFNCWinWebBrowserCachedFolderList.Create;

  if EdgeLoaded then
    Exit;

  EdgeHandle := LoadLibrary(PChar(TTMSFNCUtils.AddBackslash(EdgeDLLPath) + CoreWebView2DLL));
  if (EdgeHandle = 0) then
    Exit;

  EdgeLoaded := True;

  {$IFDEF LCLLIB}Pointer({$ENDIF}CreateCoreWebView2Environment{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(EdgeHandle, 'CreateCoreWebView2Environment');
  {$IFDEF LCLLIB}Pointer({$ENDIF}CreateCoreWebView2EnvironmentWithOptions{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(EdgeHandle, 'CreateCoreWebView2EnvironmentWithOptions');
  {$IFDEF LCLLIB}Pointer({$ENDIF}GetAvailableCoreWebView2BrowserVersionString{$IFDEF LCLLIB}){$ENDIF} := GetProcAddress(EdgeHandle, 'GetAvailableCoreWebView2BrowserVersionString');

  v := '';
  if Assigned(GetAvailableCoreWebView2BrowserVersionString) then
  begin
    GetAvailableCoreWebView2BrowserVersionString(nil, @v);
    EdgeVersion := v;
    CoTaskMemFree(v);
  end;
end;

procedure UninitializeEdge;
begin
  if Assigned(FCachedFolderList) then
    FreeAndNil(FCachedFolderList);

  if EdgeLoaded then
  begin
    FreeLibrary(EdgeHandle);
    EdgeLoaded := false;
  end;
end;

{ TCoreWebView2ExecuteScriptCompletedHandler }

constructor TCoreWebView2ExecuteScriptCompletedHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
begin
  FWebBrowser := AWebBrowser;
  FCompleteEvent := ACompleteEvent;
  FCallback := ACallback;
end;

function TCoreWebView2ExecuteScriptCompletedHandler.Invoke(errorCode: HRESULT;
  resultObjectAsJson: PWideChar): HRESULT; stdcall;
begin
  if Assigned(FWebBrowser) then
  begin
    if Assigned(FCompleteEvent) then
      FCompleteEvent(resultObjectAsJson);

    if Assigned(FCallback) then
      FCallback(Self);
  end;

  Result := S_OK;
end;

{ TCoreWebView2AcceleratorKeyPressedEventHandler }

constructor TCoreWebView2AcceleratorKeyPressedEventHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2AcceleratorKeyPressedEventHandler.Invoke(
  sender: ICoreWebView2Controller;
  args: ICoreWebView2AcceleratorKeyPressedEventArgs): HRESULT; stdcall;
var
  k: NativeUInt;
  d: Word;
begin
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    if not TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).EnableAcceleratorKeys then
      args.put_Handled(True)
    else
    begin
      args.get_VirtualKey(@k);
      {$RANGECHECKS OFF}
      d := k;
      {$RANGECHECKS ON}
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoKeyPressed(d);
      if d = 0 then
        args.put_Handled(True);
    end;
  end;
  Result := S_OK;
end;

{ TCoreWebView2NavigationStartingEventHandler }

constructor TCoreWebView2NavigationStartingEventHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2NavigationStartingEventHandler.Invoke(
  sender: ICoreWebView2;
  args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
  p: PWideChar;
begin
  args.get_Uri(@p);

  {$IFDEF LCLLIB}
  Params.URL := UTF8Encode(WideCharToString(p));
  {$ELSE}
  Params.URL := p;
  {$ENDIF}

  CoTaskMemFree(p);

  Params.Cancel := False;
  if Assigned(FWebBrowser.FWebControl) then
  begin
    FWebBrowser.FSaveURL := Params.URL;
    if not FWebBrowser.FWebControl.IsDesignTime then
      FWebBrowser.FURL := Params.URL;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
  end;

  args.put_Cancel(Params.Cancel);
  Result := S_OK;
end;

{ TCoreWebView2NavigationCompletedEventHandler }

constructor TCoreWebView2NavigationCompletedEventHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2NavigationCompletedEventHandler.Invoke(
  sender: ICoreWebView2;
  args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
var
  Params: TTMSFNCCustomWebBrowserNavigateCompleteParams;
  s: Boolean;
  e: COREWEBVIEW2_WEB_ERROR_STATUS;
begin
  s := False;
  if args.get_IsSuccess(@s) = S_OK then
    Params.Success := s;

  e := COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN;
  if args.get_WebErrorStatus(@e) = S_OK then
    Params.ErrorCode := e;

  Params.URL := FWebBrowser.FSaveURL;

  if Assigned(FWebBrowser.FWebControl) then
  begin
    if not FWebBrowser.FWebControl.IsDesignTime then
      FWebBrowser.FURL := Params.URL;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).NavigateComplete(Params);
  end;

  Result := S_OK;
end;

constructor TCoreWebView2RemoteObject.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  inherited Create;
  FWebBrowser := AWebBrowser;
end;

procedure TCoreWebView2RemoteObject.SetObjectMessage(const Value: string);
var
  Params: TTMSFNCCustomWebBrowserBeforeNavigateParams;
begin
  FObjectMessage := Value;
  Params.URL := FObjectMessage;
  Params.Cancel := False;
  if Assigned(FWebBrowser.FWebControl) then
  begin
    FWebBrowser.FURL := Params.URL;
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).BeforeNavigate(Params);
  end;
end;

{ TCoreWebView2FocusChangedEventHandler }

constructor TCoreWebView2FocusChangedEventHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

procedure TCoreWebView2FocusChangedEventHandler.FocusChanged;
begin

end;

function TCoreWebView2FocusChangedEventHandler.Invoke(sender: ICoreWebView2;
  args: IUnknown): HRESULT; stdcall;
begin
  FocusChanged;
  Result := S_OK;
end;

{ TCoreWebView2LostFocusEventHandler }

procedure TCoreWebView2LostFocusEventHandler.FocusChanged;
begin
  inherited;
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    if FWebBrowser.FWebControl.CanFocus and not FWebBrowser.FWebControl.IsDesigning then
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoExit;
  end;
end;

{ TCoreWebView2GotFocusEventHandler }

procedure TCoreWebView2GotFocusEventHandler.FocusChanged;
begin
  inherited;
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.FWebControl) then
  begin
    if FWebBrowser.FWebControl.CanFocus and not FWebBrowser.FWebControl.IsDesigning then
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).SetFocus;
  end;
end;

{ TCoreWebView2CapturePreviewCompletedHandler }

constructor TCoreWebView2CapturePreviewCompletedHandler.Create(
  AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2CapturePreviewCompletedHandler.Invoke(
  res: HRESULT): HRESULT; stdcall;
var
  bmp: TTMSFNCBitmap;
begin
  Result := S_OK;
  if not Assigned(FWebBrowser) or not Assigned(FWebBrowser.FImageStream) then
    Exit;

  FWebBrowser.FImageStream.Position := 0;
  bmp := TTMSFNCBitmap.CreateFromStream(FWebBrowser.FImageStream);
  try
    if Assigned(FWebBrowser.FWebControl) then
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoCaptureScreenShot(bmp);
  finally
    bmp.Free;
  end;
end;

{ TTMSFNCObjectDispatch }

function TTMSFNCObjectDispatch.AllocDispID(AKind: TDispatchKind; Value: Pointer;
  AInstance: TObject): TDispID;
var
  I: Integer;
  dp: TDispatchInfo;
begin
  for I := FDispatchInfoCount - 1 downto 0 do
  begin
    dp := FDispatchInfos[I];
    if (dp.Kind = AKind) and (dp.MethodInfo = Value) then
    begin
      // Already have a dispid for this methodinfo
      Result := ofDispIDOffset + I;
      Exit;
    end;
  end;

  if FDispatchInfoCount = Length(FDispatchInfos) then
    SetLength(FDispatchInfos, Length(FDispatchInfos) + 10);
  Result := ofDispIDOffset + FDispatchInfoCount;
  dp.Instance := AInstance;
  dp.Kind := AKind;
  dp.MethodInfo := Value;
  FDispatchInfos[FDispatchInfoCount] := dp;
  Inc(FDispatchInfoCount);
end;

constructor TTMSFNCObjectDispatch.Create(AInstance: TObject; Owned: Boolean = True);
begin
  inherited Create;
  FInstance := AInstance;
  FOwned := Owned;
end;

destructor TTMSFNCObjectDispatch.Destroy;
begin
  if FOwned then
    FInstance.Free;
  inherited;
end;

function TTMSFNCObjectDispatch.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
type
  PNames = ^TNames;
  TNames = array[0..0] of POleStr;
  PDispIDs = ^TDispIDs;
  TDispIDs = array[0..0] of Cardinal;
var
  Name: string;
  Info: PMethodInfoHeader;
  PropInfo: PPropInfo;
  InfoEnd: Pointer;
  Params, Param: PParamInfo;
  I: Integer;
  ID: Cardinal;
  CompIndex: Integer;
  obj: TObject;
begin
  Result := S_OK;
  /// This assumes that the DISPIDs returned do not have to be persistable.
  /// If the DISPIDs are persisted by the caller a VMT offset and parent depth
  /// count should be used instead.
  Name := PNames(Names)^[0];
  Info := GetMethodInfo(Name, obj);
  FillChar(DispIDs^, SizeOf(PDispIds(DispIDs^)[0]) * NameCount, $FF);
  if Info = nil then
  begin
    // Not a  method, try a property.
    PropInfo := GetPropInfo(Name, obj, CompIndex);
    if PropInfo <> nil then
      PDispIds(DispIds)^[0] := AllocDispID(dkProperty, PropInfo, obj)
    else if CompIndex > -1 then
      PDispIds(DispIds)^[0] := AllocDispID(dkSubComponent, Pointer(CompIndex), obj)
    else
      Result := DISP_E_UNKNOWNNAME
  end
  else
  begin
    {$IFNDEF LCLLIB}
    // Ensure the method information has enough type information
    if Info.Len <= SizeOf(Info^) - SizeOf(TSymbolNameEx) + 1 + Info.NameFld.UTF8Length then
      Result := DISP_E_UNKNOWNNAME
    else
    begin
      PDispIds(DispIds)^[0] := AllocDispID(dkMethod, Info, obj);
      Result := S_OK;
      if NameCount > 1 then
      begin
        // Now find the parameters. The DISPID is assumed to be the parameter
        // index.
        InfoEnd := Pointer(PByte(Info) + Info^.Len);
        Params := PParamInfo(PByte(Info) + SizeOf(Info^) - SizeOf(TSymbolNameEx) + 1
          + SizeOf(TReturnInfo) + Info.NameFld.UTF8Length);
        for I := 1 to NameCount - 1 do
        begin
          Name := PNames(Names)^[I];
          Param := Params;
          ID := 0;
          while IntPtr(Param) < IntPtr(InfoEnd) do
          begin
            // ignore Self
            if (Param^.ParamType^{$IFDEF LCLLIB}^{$ENDIF}.Kind <> tkClass) or not SameText(Param^.NameFld.ToString, 'SELF') then
              if SameText(Param^.NameFld.ToString, Name) then
              begin
                PDispIDs(DispIDs)^[I] := ID;
                Break;
              end;
            Inc(ID);
            Param := PParamInfo(PByte(Param) + SizeOf(Param^) -
              SizeOf(TSymbolNameEx) + 1 + Param^.NameFld.UTF8Length);
          end;
          if IntPtr(Param) >= IntPtr(InfoEnd) then
            Result := DISP_E_UNKNOWNNAME
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

function SamePropTypeNameEx(const Name1, Name2: String): Boolean;
begin
  Result := AnsiSameText(Name1, Name2);
end;

function GetMethodInfoEx(Instance: TObject; const MethodName: string): PMethodInfoHeader;
//var
//  VMT: Pointer;
//  MethodInfo: Pointer;
//  Count: Integer;
begin
  Result := nil;
//  // Find the method
//  VMT := PPointer(Instance)^;
//  repeat
//    MethodInfo := PPointer(PByte(VMT) + vmtMethodTable)^;
//    if MethodInfo <> nil then
//    begin
//      // Scan method table for the method
//      Count := PWord(MethodInfo)^;
//      Inc(PByte(MethodInfo), 2);
//      while Count > 0 do
//      begin
//        Result := MethodInfo;
//        if SamePropTypeNameEx(Result^.NameFld.ToString, MethodName) then
//          Exit;
//        Inc(PByte(MethodInfo), PMethodInfoHeader(MethodInfo)^.Len);
//        Dec(Count);
//      end;
//    end;
//    // Find the parent VMT
//    VMT := PPointer(PByte(VMT) + vmtParent)^;
//    if VMT = nil then
//      Exit(nil);
//    VMT := PPointer(VMT)^;
//  until False;
end;

function TTMSFNCObjectDispatch.GetMethodInfo(const AName: string;
  var AInstance: TObject): PMethodInfoHeader;
begin
  Result := GetMethodInfoEx(FInstance, AName);
  if Result <> nil then
    AInstance := FInstance;
end;

function TTMSFNCObjectDispatch.GetObjectDispatch(Obj: TObject): TTMSFNCObjectDispatch;
begin
  Result := TTMSFNCObjectDispatch.Create(Obj, False);
end;

function TTMSFNCObjectDispatch.GetPropInfo(const AName: string;
  var AInstance: TObject; var CompIndex: Integer): PPropInfo;
var
  Component: TComponent;
begin
  CompIndex := -1;
  Result := TypInfo.GetPropInfo(FInstance, AName);
  if (Result = nil) and (FInstance is TComponent) then
  begin
    // Not a property, try a sub component
    Component := TComponent(FInstance).FindComponent(AName);
    if Component <> nil then
    begin
      AInstance := FInstance;
      CompIndex := Component.ComponentIndex;
    end;
  end else if Result <> nil then
    AInstance := FInstance
  else
    AInstance := nil;
end;

function TTMSFNCObjectDispatch.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TTMSFNCObjectDispatch.GetTypeInfoCount(out Count: Integer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TTMSFNCObjectDispatch.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult; stdcall;
type
  PVariantArray = ^TVariantArray;
  TVariantArray = array[0..65535] of Variant;
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..65535] of Integer;
var
  Parms: PDispParams;
  TempRet: Variant;
  DispatchInfo: TDispatchInfo;
  {$IFNDEF LCLLIB}
  ReturnInfo: PReturnInfo;
  {$ENDIF}
  e: TExcepInfo;
  {$IFDEF LCLLIB}
  cn: string;
  {$ENDIF}
begin
  Result := S_OK;

  Parms := @Params;
  try
    if VarResult = nil then
      VarResult := @TempRet;
    if (DispID - ofDispIDOffset >= 0) and (DispID - ofDispIDOffset < FDispatchInfoCount) then
    begin
      DispatchInfo := FDispatchInfos[DispID - ofDispIDOffset];
      case DispatchInfo.Kind of
        dkProperty:
          begin
            // The high bit set means the DispID is a property not a method.
            // See GetIDsOfNames
            if Flags and (DISPATCH_PROPERTYPUTREF or DISPATCH_PROPERTYPUT) <> 0 then
            begin
              if (Parms{$IFDEF LCLLIB}^{$ENDIF}.cNamedArgs <> 1) or
                (PIntegerArray(Parms{$IFDEF LCLLIB}^{$ENDIF}.rgdispidNamedArgs)^[0] <> DISPID_PROPERTYPUT) then
                Result := DISP_E_MEMBERNOTFOUND
              else
              begin
                {$IFDEF LCLLIB}
                cn := TTMSFNCPersistence.GetPropInfoName(DispatchInfo.PropInfo);
                if cn <> '' then
                begin
                  SetPropValue(DispatchInfo.Instance, cn,
                    PVariantArray(Parms{$IFDEF LCLLIB}^{$ENDIF}.rgvarg)^[0])
                end
                {$ELSE}
                SetPropValue(DispatchInfo.Instance, DispatchInfo.PropInfo,
                  PVariantArray(Parms{$IFDEF LCLLIB}^{$ENDIF}.rgvarg)^[0])
                {$ENDIF}
              end;
            end
            else
            begin
              if Parms{$IFDEF LCLLIB}^{$ENDIF}.cArgs <> 0 then
                Result := DISP_E_BADPARAMCOUNT
              else if DispatchInfo.PropInfo^.PropType^.Kind = tkClass then
              begin
                POleVariant(VarResult)^ := GetObjectDispatch(
                  GetObjectProp(DispatchInfo.Instance, DispatchInfo.PropInfo)) as IDispatch
              end
              else
              begin
                {$IFDEF LCLLIB}
                cn := TTMSFNCPersistence.GetPropInfoName(DispatchInfo.PropInfo);
                if cn <> '' then
                begin
                  POleVariant(VarResult)^ := GetPropValue(DispatchInfo.Instance,
                    cn, False);
                end
                {$ELSE}
                POleVariant(VarResult)^ := GetPropValue(DispatchInfo.Instance,
                  DispatchInfo.PropInfo, False);
                {$ENDIF}
              end;
            end;
          end;
        dkMethod:
          begin
            {$IFNDEF LCLLIB}
            ReturnInfo := PReturnInfo(DispatchInfo.MethodInfo.NameFld.Tail);
            if (ReturnInfo.ReturnType <> nil) and (ReturnInfo.ReturnType^.Kind = tkClass) then
            begin
              POleVariant(VarResult)^ := GetObjectDispatch(TObject(NativeInt(ObjectInvoke(DispatchInfo.Instance,
                DispatchInfo.MethodInfo,
                Slice(PIntegerArray(Parms.rgdispidNamedArgs)^, Parms.cNamedArgs),
                Slice(PVariantArray(Parms.rgvarg)^, Parms.cArgs))))) as IDispatch
            end
            else
            begin
              POleVariant(VarResult)^ := ObjectInvoke(DispatchInfo.Instance,
                DispatchInfo.MethodInfo,
                Slice(PIntegerArray(Parms.rgdispidNamedArgs)^, Parms.cNamedArgs),
                Slice(PVariantArray(Parms.rgvarg)^, Parms.cArgs));
            end;
            {$ENDIF}
          end;
        dkSubComponent:
          POleVariant(VarResult)^ := GetObjectDispatch(TComponent(DispatchInfo.Instance).Components[DispatchInfo.Index]) as IDispatch;
      end;
    end else
      Result := DISP_E_MEMBERNOTFOUND;
  except
    if ExcepInfo <> nil then
    begin
      FillChar(ExcepInfo^, SizeOf(TExcepInfo), 0);
      e := TExcepInfo(ExcepInfo^);
      e.{$IFDEF LCLLIB}Source{$ELSE}bstrSource{$ENDIF} := StringToOleStr(ClassName);
      if ExceptObject is Exception then
        e.{$IFDEF LCLLIB}Description{$ELSE}bstrDescription{$ENDIF} := StringToOleStr(Exception(ExceptObject).Message);
      e.scode := E_FAIL;
    end;
    Result := DISP_E_EXCEPTION;
  end;
end;

{$IFDEF LCLLIB}

{ TMethodInfoHeader }

function TMethodInfoHeader.NameFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@Name);
end;

{ TParamInfo }

function TParamInfo.NameFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@Name);
end;

{ TTypeInfoFieldAccessor }

procedure TTypeInfoFieldAccessor.SetData(const Data: PByte);
begin
  FData := Data;
end;

class operator TTypeInfoFieldAccessor.=(const Left, Right: TTypeInfoFieldAccessor): Boolean;
begin
  Result := (Left.FData^ = Right.FData^) and CompareMem(Left.FData, Right.FData, Left.FData^);
end;

function TTypeInfoFieldAccessor.UTF8Length: integer;
begin
  Result := FData^;
end;

function TTypeInfoFieldAccessor.ToString: string;
var
  Dest: array[0..511] of Char;
begin
  if FData^ <> 0 then
    SetString(Result, Dest, UTF8ToUnicode({$IFDEF LCLLIB}@{$ENDIF}Dest{$IFDEF LCLLIB}[0]{$ENDIF}, Length(Dest), PChar(FData+1), FData^)-1)
  else
    Result := '';
end;

function TTypeInfoFieldAccessor.ToShortUTF8String: ShortString;
begin
  Result := PShortString(FData)^;
end;

function TTypeInfoFieldAccessor.ToByteArray: TBytes;
var
  Len: Integer;
begin
  Len := FData^;
  SetLength(Result, Len);
  if Len <> 0 then
    Move((FData+1)^, Result[0], Len);
end;

function TTypeInfoFieldAccessor.Tail: PByte;
begin
  Result := FData + FData^ + 1;
end;

{$ENDIF}

{ TTMSFNCWinWebBrowserCachedFolderItem }

constructor TTMSFNCWinWebBrowserCachedFolderItem.Create(AFolder: string;
  AInstance: TTMSFNCWinWebBrowser);
begin
  FFolder := AFolder;
  FInstance := AInstance;
end;

{ TCoreWebView2ContextMenuRequestedEventHandler }

constructor TCoreWebView2ContextMenuRequestedEventHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2ContextMenuRequestedEventHandler.Invoke(sender: ICoreWebView2; args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
var
  ti: TTMSFNCWebBrowserTargetItem;
  ic: ICoreWebView2ContextMenuItemCollection;
  mt: ICorewebView2ContextMenuTarget;
  k: COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND;
  u, st, lt: PWideChar;
  b: BOOL;
  popup: TPopupMenu;
  pt: TPoint;
  {$IFDEF FMXLIB}
  pts: TPointF;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  dpi: Single;
  pts: TPoint;
  {$ENDIF}
  native: boolean;
begin
  if Assigned(FWebBrowser) then
  begin
    //GET CONTEXT MENU TARGETS Code:
    ti.Kind := mtPage;
    ti.LinkText := '';
    ti.SelectionText := '';
    ti.URI := '';
    args.get_ContextMenuTarget(mt);

    u := '';
    st := '';
    lt := '';
    b := False;

    if Assigned(mt) then
    begin
      mt.get_Kind(@k);
      ti.Kind := TTMSFNCWebBrowserContextMenuType(k);

//      mt.get_PageUri(@u);
//      if u <> '' then
//        ti.URI := u;
//      CoTaskMemFree(u);

      mt.get_FrameUri(@u);
      if u <> '' then
        ti.URI := u;
      CoTaskMemFree(u);

      mt.get_HasSelection(@b);
      if b then
      begin
        mt.get_SelectionText(@st);
        ti.SelectionText := st;
        CoTaskMemFree(st);
      end;

      mt.get_HasLinkText(@b);
      if b then
      begin
        mt.get_LinkText(@lt);
        ti.LinkText := lt;
        CoTaskMemFree(lt);
      end;

//      mt.get_HasSourceUri(@b);
//      if b then
//      begin
//        mt.get_SourceUri(@u);
//        ti.URI := u;
//        CoTaskMemFree(u);
//      end;

      mt.get_HasLinkUri(@b);
      if b then
      begin
        mt.get_LinkUri(@u);
        ti.URI := u;
        CoTaskMemFree(u);
      end;
    end;

    FWebBrowser.FLastTargetItem := ti;

    popup := nil;

    if Assigned(FWebBrowser.FWebControl) then
    begin
      if FWebBrowser.FWebControl.PopupMenu is TPopupMenu then
        popup := TPopUpMenu(FWebBrowser.FWebControl.PopupMenu);
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoGetPopupMenuForContextMenu(ti, popup);
    end
    else
      popup := nil;

    FWebBrowser.ContextMenuList.Clear;
    Result := args.get_MenuItems(ic);

    if Assigned(popup) then
    begin
      native := False;
      if Assigned(TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).Settings) then
        native := TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).Settings.UsePopUpMenuAsContextMenu;

      if native then
      begin
        FWebBrowser.GetPopupMenuItems(popup, FWebBrowser.ContextMenuList);
      end
      else
      begin
        args.get_Location(@pt);
        {$IFDEF FMXLIB}
        pts := TControlOpen(FWebBrowser.FWebControl).LocalToScreen(PointF(pt.X, pt.Y));
        popup.Popup(pts.X, pts.Y);
        {$ENDIF}
        {$IFNDEF FMXLIB}
        if Assigned(FWebBrowser.FWebControl) then
          dpi := FWebBrowser.FWebControl.ScalePaintValue(1.0)
        else
          dpi := 1;
        {$IFNDEF LCLLIB}
        pts := FWebBrowser.FWebControl.ClientToScreen(ScalePoint(pt, dpi, dpi));
        {$ENDIF}
        {$IFDEF LCLLIB}
        pts.x := Round(pt.x * dpi);
        pts.y := Round(pt.y * dpi);
        {$ENDIF}
        popup.Popup(pts.X, pts.Y);
        {$ENDIF}
      end;
    end
    else
    begin
      if Result = S_OK then
      begin
        Result := FWebBrowser.GetContextMenuItems(nil, ic, FWebBrowser.ContextMenuList);
        if Assigned(FWebBrowser.FWebControl) then
          TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoGetContextMenuItemEvent(ti, FWebBrowser.ContextMenuList);
      end;
    end;

    FWebBrowser.CompareContextMenu(ic, FWebBrowser.ContextMenuList);
  end
  else
    Result := E_UNEXPECTED;
end;

{ TCoreWebView2CustomItemSelectedEventHandler }

constructor TCoreWebView2CustomItemSelectedEventHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser
end;

function TCoreWebView2CustomItemSelectedEventHandler.Invoke(sender: ICoreWebView2ContextMenuItem; args: IUnknown): HRESULT; stdcall;
begin
  Result := FWebBrowser.CustomContextMenuItemSelected(sender);
end;

{ TCoreWebView2DOMContentLoadedEventHandler }

constructor TCoreWebView2DOMContentLoadedEventHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2DOMContentLoadedEventHandler.Invoke(sender: ICoreWebView2; args: ICoreWebView2DOMContentLoadedEventArgs): HRESULT; stdcall;
begin
  //  DOM Content Loaded
  Result := S_OK;
end;

{ TCoreWebView2ContentLoadingEventHandler }

constructor TCoreWebView2ContentLoadingEventHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2ContentLoadingEventHandler.Invoke(sender: ICoreWebView2; args: ICoreWebView2ContentLoadingEventArgs): HRESULT; stdcall;
begin
//  Content loading;
  Result := S_OK;
end;

{ TCoreWebView2GetCookiesCompletedHandler }

constructor TCoreWebView2GetCookiesCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2GetCookiesCompletedHandler.Invoke(res: HRESULT; cookielist: ICoreWebView2CookieList): HRESULT; stdcall;
var
  cookies: array of TTMSFNCWebBrowserCookie;
  cookie: ICoreWebView2Cookie;
  cnt, I: integer;
  n, d, p, v: PWideChar;
  http,sec, ses: BOOL;
  exp: Double;
  ss: Cardinal;
begin
  Result := cookielist.get_Count(@cnt);

  SetLength(cookies, cnt);

  for I := 0 to cnt - 1 do
  begin
    Result := cookielist.GetValueAtIndex(I, cookie);
    if Assigned(cookie) then
    begin
      cookie.get_Name(@n);
      cookies[I].Name := WideCharToString(n);
      CoTaskMemFree(n);

      cookie.get_Value(@v);
      cookies[I].Value := v;
      CoTaskMemFree(v);

      cookie.get_Domain(@d);
      cookies[I].Domain := WideCharToString(d);
      CoTaskMemFree(d);

      cookie.get_Path(@p);
      cookies[I].Path := p;
      CoTaskMemFree(p);

      cookie.get_Expires(@exp);
      cookies[I].Expires := UnixToDateTime(Trunc(exp));

      cookie.get_IsHttpOnly(@http);
      cookies[I].HTTPOnly := http;

      cookie.get_IsSecure(@sec);
      cookies[I].Secure := sec;

      cookie.get_IsSession(@ses);
      cookies[I].Session := ses;

      cookie.get_SameSite(@ss);
      cookies[I].SameSite := TTMSFNCWebBrowserSameSiteType(ss);
    end;
  end;

  if Assigned(FWebBrowser.FWebControl) then
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoGetCookies(cookies);

  SetLength(cookies, 0);
end;

{ TCoreWebView2PrintToPdfStreamCompletedHandler }

constructor TCoreWebView2PrintToPdfStreamCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2PrintToPdfStreamCompletedHandler.Invoke(errorCode: HRESULT; pdfStream: IStream): HRESULT; stdcall;
var
  s: TMemoryStream;
  sa: TStreamAdapter;
  cb: tagSTATSTG;
  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION <= 28}
  cbRead, cbWritten: LargeInt;
  {$ELSE}
  cbRead, cbWritten: UInt64;
  {$IFEND}
  {$ENDIF}
  {$IFDEF LCLLIB}
  cbRead, cbWritten:  UInt64;
  {$ENDIF}
begin
  Result := errorCode;

  s := TMemoryStream.Create;
  try
    if Assigned(pdfStream) and (errorCode = S_OK) then
    begin
      sa := TStreamAdapter.Create(s, soReference);
      try
        pdfStream.Stat(cb, STATFLAG_DEFAULT);
        pdfStream.CopyTo(sa, cb.cbSize, cbRead, cbWritten);
        s.Position := 0;
      finally
        sa.Free;
      end;
    end;

    if Assigned(FWebBrowser.FWebControl) then
      TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoGetPrintPDFStream(s);
  finally
    s.Free
  end;
end;

{ TCoreWebView2PrintCompletedHandler }

constructor TCoreWebView2PrintCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2PrintCompletedHandler.Invoke(errorCode: HRESULT; printStatus: BOOL): HRESULT; stdcall;
begin
  Result := errorCode;
  if Assigned(FWebBrowser.FWebControl) then
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoPrinted(printStatus);
end;

{ TCoreWebView2PrintToPdfCompletedHandler }

constructor TCoreWebView2PrintToPdfCompletedHandler.Create(AWebBrowser: TTMSFNCWinWebBrowser);
begin
  FWebBrowser := AWebBrowser;
end;

function TCoreWebView2PrintToPdfCompletedHandler.Invoke(errorCode: HRESULT; isSuccessful: Boolean): HRESULT; stdcall;
begin
  Result := errorCode;
  if Assigned(FWebBrowser.FWebControl) then
    TTMSFNCCustomWebBrowserProtected(FWebBrowser.FWebControl).DoPrintedToPDF(isSuccessful);
end;

initialization
  InitializeEdge;

finalization
  UninitializeEdge;
{$ENDIF}

end.
