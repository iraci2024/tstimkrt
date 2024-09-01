// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCWebBrowser.Win.pas' rev: 35.00 (Windows)

#ifndef Vcl_Tmsfncwebbrowser_WinHPP
#define Vcl_Tmsfncwebbrowser_WinHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncwebbrowser
{
namespace Win
{
//-- forward type declarations -----------------------------------------------
struct EventRegistrationToken;
struct COREWEBVIEW2_COLOR;
__interface DELPHIINTERFACE ICoreWebView2EnvironmentOptions;
typedef System::DelphiInterface<ICoreWebView2EnvironmentOptions> _di_ICoreWebView2EnvironmentOptions;
__interface DELPHIINTERFACE ICoreWebView2Settings;
typedef System::DelphiInterface<ICoreWebView2Settings> _di_ICoreWebView2Settings;
__interface DELPHIINTERFACE ICoreWebView2Settings2;
typedef System::DelphiInterface<ICoreWebView2Settings2> _di_ICoreWebView2Settings2;
__interface DELPHIINTERFACE ICoreWebView2Settings3;
typedef System::DelphiInterface<ICoreWebView2Settings3> _di_ICoreWebView2Settings3;
__interface DELPHIINTERFACE ICoreWebView2Settings4;
typedef System::DelphiInterface<ICoreWebView2Settings4> _di_ICoreWebView2Settings4;
__interface DELPHIINTERFACE ICoreWebView2Settings5;
typedef System::DelphiInterface<ICoreWebView2Settings5> _di_ICoreWebView2Settings5;
__interface DELPHIINTERFACE ICoreWebView2Settings6;
typedef System::DelphiInterface<ICoreWebView2Settings6> _di_ICoreWebView2Settings6;
__interface DELPHIINTERFACE ICoreWebView2Settings7;
typedef System::DelphiInterface<ICoreWebView2Settings7> _di_ICoreWebView2Settings7;
__interface DELPHIINTERFACE ICoreWebView2NavigationCompletedEventArgs;
typedef System::DelphiInterface<ICoreWebView2NavigationCompletedEventArgs> _di_ICoreWebView2NavigationCompletedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2HttpHeadersCollectionIterator;
typedef System::DelphiInterface<ICoreWebView2HttpHeadersCollectionIterator> _di_ICoreWebView2HttpHeadersCollectionIterator;
__interface DELPHIINTERFACE ICoreWebView2HttpResponseHeaders;
typedef System::DelphiInterface<ICoreWebView2HttpResponseHeaders> _di_ICoreWebView2HttpResponseHeaders;
__interface DELPHIINTERFACE ICoreWebView2HttpRequestHeaders;
typedef System::DelphiInterface<ICoreWebView2HttpRequestHeaders> _di_ICoreWebView2HttpRequestHeaders;
__interface DELPHIINTERFACE ICoreWebView2WebResourceRequest;
typedef System::DelphiInterface<ICoreWebView2WebResourceRequest> _di_ICoreWebView2WebResourceRequest;
__interface DELPHIINTERFACE ICoreWebView2TrySuspendCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2TrySuspendCompletedHandler> _di_ICoreWebView2TrySuspendCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2WebResourceResponse;
typedef System::DelphiInterface<ICoreWebView2WebResourceResponse> _di_ICoreWebView2WebResourceResponse;
__interface DELPHIINTERFACE ICoreWebView2Deferral;
typedef System::DelphiInterface<ICoreWebView2Deferral> _di_ICoreWebView2Deferral;
__interface DELPHIINTERFACE ICoreWebView2WebResourceRequestedEventArgs;
typedef System::DelphiInterface<ICoreWebView2WebResourceRequestedEventArgs> _di_ICoreWebView2WebResourceRequestedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2AcceleratorKeyPressedEventArgs;
typedef System::DelphiInterface<ICoreWebView2AcceleratorKeyPressedEventArgs> _di_ICoreWebView2AcceleratorKeyPressedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2PermissionRequestedEventArgs;
typedef System::DelphiInterface<ICoreWebView2PermissionRequestedEventArgs> _di_ICoreWebView2PermissionRequestedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2NewWindowRequestedEventArgs;
typedef System::DelphiInterface<ICoreWebView2NewWindowRequestedEventArgs> _di_ICoreWebView2NewWindowRequestedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2DOMContentLoadedEventArgs;
typedef System::DelphiInterface<ICoreWebView2DOMContentLoadedEventArgs> _di_ICoreWebView2DOMContentLoadedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2NavigationStartingEventArgs;
typedef System::DelphiInterface<ICoreWebView2NavigationStartingEventArgs> _di_ICoreWebView2NavigationStartingEventArgs;
__interface DELPHIINTERFACE ICoreWebView2SourceChangedEventArgs;
typedef System::DelphiInterface<ICoreWebView2SourceChangedEventArgs> _di_ICoreWebView2SourceChangedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2ContentLoadingEventArgs;
typedef System::DelphiInterface<ICoreWebView2ContentLoadingEventArgs> _di_ICoreWebView2ContentLoadingEventArgs;
__interface DELPHIINTERFACE ICoreWebView2NavigationStartingEventHandler;
typedef System::DelphiInterface<ICoreWebView2NavigationStartingEventHandler> _di_ICoreWebView2NavigationStartingEventHandler;
__interface DELPHIINTERFACE ICoreWebView2ContentLoadingEventHandler;
typedef System::DelphiInterface<ICoreWebView2ContentLoadingEventHandler> _di_ICoreWebView2ContentLoadingEventHandler;
__interface DELPHIINTERFACE ICoreWebView2SourceChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2SourceChangedEventHandler> _di_ICoreWebView2SourceChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2HistoryChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2HistoryChangedEventHandler> _di_ICoreWebView2HistoryChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2NavigationCompletedEventHandler;
typedef System::DelphiInterface<ICoreWebView2NavigationCompletedEventHandler> _di_ICoreWebView2NavigationCompletedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2WebResourceRequestedEventHandler;
typedef System::DelphiInterface<ICoreWebView2WebResourceRequestedEventHandler> _di_ICoreWebView2WebResourceRequestedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2CallDevToolsProtocolMethodCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2CallDevToolsProtocolMethodCompletedHandler> _di_ICoreWebView2CallDevToolsProtocolMethodCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2ExecuteScriptCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2ExecuteScriptCompletedHandler> _di_ICoreWebView2ExecuteScriptCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2AcceleratorKeyPressedEventHandler;
typedef System::DelphiInterface<ICoreWebView2AcceleratorKeyPressedEventHandler> _di_ICoreWebView2AcceleratorKeyPressedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2NewWindowRequestedEventHandler;
typedef System::DelphiInterface<ICoreWebView2NewWindowRequestedEventHandler> _di_ICoreWebView2NewWindowRequestedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2DOMContentLoadedEventHandler;
typedef System::DelphiInterface<ICoreWebView2DOMContentLoadedEventHandler> _di_ICoreWebView2DOMContentLoadedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2FocusChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2FocusChangedEventHandler> _di_ICoreWebView2FocusChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2CapturePreviewCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2CapturePreviewCompletedHandler> _di_ICoreWebView2CapturePreviewCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2CursorChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2CursorChangedEventHandler> _di_ICoreWebView2CursorChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2PermissionRequestedEventHandler;
typedef System::DelphiInterface<ICoreWebView2PermissionRequestedEventHandler> _di_ICoreWebView2PermissionRequestedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2;
typedef System::DelphiInterface<ICoreWebView2> _di_ICoreWebView2;
__interface DELPHIINTERFACE ICoreWebView2Cookie;
typedef System::DelphiInterface<ICoreWebView2Cookie> _di_ICoreWebView2Cookie;
__interface DELPHIINTERFACE ICoreWebView2CookieList;
typedef System::DelphiInterface<ICoreWebView2CookieList> _di_ICoreWebView2CookieList;
__interface DELPHIINTERFACE ICoreWebView2GetCookiesCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2GetCookiesCompletedHandler> _di_ICoreWebView2GetCookiesCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2CookieManager;
typedef System::DelphiInterface<ICoreWebView2CookieManager> _di_ICoreWebView2CookieManager;
__interface DELPHIINTERFACE ICoreWebView2_2;
typedef System::DelphiInterface<ICoreWebView2_2> _di_ICoreWebView2_2;
__interface DELPHIINTERFACE ICoreWebView2_3;
typedef System::DelphiInterface<ICoreWebView2_3> _di_ICoreWebView2_3;
__interface DELPHIINTERFACE ICoreWebView2StateChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2StateChangedEventHandler> _di_ICoreWebView2StateChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2BytesReceivedChangedEventHandler;
typedef System::DelphiInterface<ICoreWebView2BytesReceivedChangedEventHandler> _di_ICoreWebView2BytesReceivedChangedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2DownloadOperation;
typedef System::DelphiInterface<ICoreWebView2DownloadOperation> _di_ICoreWebView2DownloadOperation;
__interface DELPHIINTERFACE ICoreWebView2DownloadStartingEventArgs;
typedef System::DelphiInterface<ICoreWebView2DownloadStartingEventArgs> _di_ICoreWebView2DownloadStartingEventArgs;
__interface DELPHIINTERFACE ICoreWebView2DownloadStartingEventHandler;
typedef System::DelphiInterface<ICoreWebView2DownloadStartingEventHandler> _di_ICoreWebView2DownloadStartingEventHandler;
__interface DELPHIINTERFACE ICoreWebView2_4;
typedef System::DelphiInterface<ICoreWebView2_4> _di_ICoreWebView2_4;
__interface DELPHIINTERFACE ICoreWebView2_5;
typedef System::DelphiInterface<ICoreWebView2_5> _di_ICoreWebView2_5;
__interface DELPHIINTERFACE ICoreWebView2_6;
typedef System::DelphiInterface<ICoreWebView2_6> _di_ICoreWebView2_6;
__interface DELPHIINTERFACE ICoreWebView2PrintSettings;
typedef System::DelphiInterface<ICoreWebView2PrintSettings> _di_ICoreWebView2PrintSettings;
__interface DELPHIINTERFACE ICoreWebView2PrintToPdfCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2PrintToPdfCompletedHandler> _di_ICoreWebView2PrintToPdfCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2_7;
typedef System::DelphiInterface<ICoreWebView2_7> _di_ICoreWebView2_7;
__interface DELPHIINTERFACE ICoreWebView2_8;
typedef System::DelphiInterface<ICoreWebView2_8> _di_ICoreWebView2_8;
__interface DELPHIINTERFACE ICoreWebView2_9;
typedef System::DelphiInterface<ICoreWebView2_9> _di_ICoreWebView2_9;
__interface DELPHIINTERFACE ICoreWebView2_10;
typedef System::DelphiInterface<ICoreWebView2_10> _di_ICoreWebView2_10;
__interface DELPHIINTERFACE ICoreWebView2CustomItemSelectedEventHandler;
typedef System::DelphiInterface<ICoreWebView2CustomItemSelectedEventHandler> _di_ICoreWebView2CustomItemSelectedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2ContextMenuItem;
typedef System::DelphiInterface<ICoreWebView2ContextMenuItem> _di_ICoreWebView2ContextMenuItem;
__interface DELPHIINTERFACE ICoreWebView2ContextMenuItemCollection;
typedef System::DelphiInterface<ICoreWebView2ContextMenuItemCollection> _di_ICoreWebView2ContextMenuItemCollection;
__interface DELPHIINTERFACE ICorewebView2ContextMenuTarget;
typedef System::DelphiInterface<ICorewebView2ContextMenuTarget> _di_ICorewebView2ContextMenuTarget;
__interface DELPHIINTERFACE ICoreWebView2ContextMenuRequestedEventArgs;
typedef System::DelphiInterface<ICoreWebView2ContextMenuRequestedEventArgs> _di_ICoreWebView2ContextMenuRequestedEventArgs;
__interface DELPHIINTERFACE ICoreWebView2ContextMenuRequestedEventHandler;
typedef System::DelphiInterface<ICoreWebView2ContextMenuRequestedEventHandler> _di_ICoreWebView2ContextMenuRequestedEventHandler;
__interface DELPHIINTERFACE ICoreWebView2_11;
typedef System::DelphiInterface<ICoreWebView2_11> _di_ICoreWebView2_11;
__interface DELPHIINTERFACE ICoreWebView2_12;
typedef System::DelphiInterface<ICoreWebView2_12> _di_ICoreWebView2_12;
__interface DELPHIINTERFACE ICoreWebView2_13;
typedef System::DelphiInterface<ICoreWebView2_13> _di_ICoreWebView2_13;
__interface DELPHIINTERFACE ICoreWebView2_14;
typedef System::DelphiInterface<ICoreWebView2_14> _di_ICoreWebView2_14;
__interface DELPHIINTERFACE ICoreWebView2_15;
typedef System::DelphiInterface<ICoreWebView2_15> _di_ICoreWebView2_15;
__interface DELPHIINTERFACE ICoreWebView2PrintCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2PrintCompletedHandler> _di_ICoreWebView2PrintCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2PrintToPdfStreamCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2PrintToPdfStreamCompletedHandler> _di_ICoreWebView2PrintToPdfStreamCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2_16;
typedef System::DelphiInterface<ICoreWebView2_16> _di_ICoreWebView2_16;
__interface DELPHIINTERFACE ICoreWebView2Controller;
typedef System::DelphiInterface<ICoreWebView2Controller> _di_ICoreWebView2Controller;
__interface DELPHIINTERFACE ICoreWebView2Controller2;
typedef System::DelphiInterface<ICoreWebView2Controller2> _di_ICoreWebView2Controller2;
__interface DELPHIINTERFACE ICoreWebView2Controller3;
typedef System::DelphiInterface<ICoreWebView2Controller3> _di_ICoreWebView2Controller3;
__interface DELPHIINTERFACE ICoreWebView2Controller4;
typedef System::DelphiInterface<ICoreWebView2Controller4> _di_ICoreWebView2Controller4;
__interface DELPHIINTERFACE ICoreWebView2CompositionController;
typedef System::DelphiInterface<ICoreWebView2CompositionController> _di_ICoreWebView2CompositionController;
__interface DELPHIINTERFACE ICoreWebView2CompositionController2;
typedef System::DelphiInterface<ICoreWebView2CompositionController2> _di_ICoreWebView2CompositionController2;
__interface DELPHIINTERFACE ICoreWebView2CompositionController3;
typedef System::DelphiInterface<ICoreWebView2CompositionController3> _di_ICoreWebView2CompositionController3;
__interface DELPHIINTERFACE ICoreWebView2CreateCoreWebView2ControllerCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2CreateCoreWebView2ControllerCompletedHandler> _di_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2Environment;
typedef System::DelphiInterface<ICoreWebView2Environment> _di_ICoreWebView2Environment;
__interface DELPHIINTERFACE ICoreWebView2Environment2;
typedef System::DelphiInterface<ICoreWebView2Environment2> _di_ICoreWebView2Environment2;
__interface DELPHIINTERFACE ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler> _di_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler;
__interface DELPHIINTERFACE ICoreWebView2Environment3;
typedef System::DelphiInterface<ICoreWebView2Environment3> _di_ICoreWebView2Environment3;
__interface DELPHIINTERFACE ICoreWebView2Environment4;
typedef System::DelphiInterface<ICoreWebView2Environment4> _di_ICoreWebView2Environment4;
__interface DELPHIINTERFACE ICoreWebView2Environment5;
typedef System::DelphiInterface<ICoreWebView2Environment5> _di_ICoreWebView2Environment5;
__interface DELPHIINTERFACE ICoreWebView2Environment6;
typedef System::DelphiInterface<ICoreWebView2Environment6> _di_ICoreWebView2Environment6;
__interface DELPHIINTERFACE ICoreWebView2Environment7;
typedef System::DelphiInterface<ICoreWebView2Environment7> _di_ICoreWebView2Environment7;
__interface DELPHIINTERFACE ICoreWebView2Environment8;
typedef System::DelphiInterface<ICoreWebView2Environment8> _di_ICoreWebView2Environment8;
__interface DELPHIINTERFACE ICoreWebView2Environment9;
typedef System::DelphiInterface<ICoreWebView2Environment9> _di_ICoreWebView2Environment9;
__interface DELPHIINTERFACE ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler;
typedef System::DelphiInterface<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler> _di_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler;
//-- type declarations -------------------------------------------------------
typedef HICON *PHCURSOR;

struct DECLSPEC_DRECORD EventRegistrationToken
{
public:
	__int64 Value;
};


typedef EventRegistrationToken *PEventRegistrationToken;

struct DECLSPEC_DRECORD COREWEBVIEW2_COLOR
{
public:
	System::Byte A;
	System::Byte R;
	System::Byte G;
	System::Byte B;
};


typedef COREWEBVIEW2_COLOR *PCOREWEBVIEW2_COLOR;

typedef unsigned COREWEBVIEW2_MOVE_FOCUS_REASON;

typedef unsigned *PCOREWEBVIEW2_MOVE_FOCUS_REASON;

typedef unsigned COREWEBVIEW2_PDF_TOOLBAR_ITEMS;

typedef unsigned *PCOREWEBVIEW2_PDF_TOOLBAR_ITEMS;

typedef unsigned COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND;

typedef unsigned *PCOREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND;

typedef unsigned COREWEBVIEW2_DOWNLOAD_STATE;

typedef unsigned *PCOREWEBVIEW2_DOWNLOAD_STATE;

typedef unsigned COREWEBVIEW2_WEB_ERROR_STATUS;

typedef unsigned *PCOREWEBVIEW2_WEB_ERROR_STATUS;

typedef unsigned COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT;

typedef unsigned *PCOREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT;

typedef unsigned COREWEBVIEW2_WEB_RESOURCE_CONTEXT;

typedef unsigned *PCOREWEBVIEW2_WEB_RESOURCE_CONTEXT;

typedef unsigned COREWEBVIEW2_KEY_EVENT_KIND;

typedef unsigned *PCOREWEBVIEW2_KEY_EVENT_KIND;

typedef unsigned COREWEBVIEW2_PERMISSION_KIND;

typedef unsigned *PCOREWEBVIEW2_PERMISSION_KIND;

typedef unsigned COREWEBVIEW2_PERMISSION_STATE;

typedef unsigned *PCOREWEBVIEW2_PERMISSION_STATE;

typedef unsigned COREWEBVIEW2_PHYSICAL_KEY_STATUS;

typedef unsigned *PCOREWEBVIEW2_PHYSICAL_KEY_STATUS;

typedef unsigned COREWEBVIEW2_PRINT_ORIENTATION;

typedef unsigned *PCOREWEBVIEW2_PRINT_ORIENTATION;

typedef unsigned COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND;

typedef unsigned *PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND;

typedef unsigned COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND;

typedef unsigned *PCOREWEBVIEW2_CONTEXT_MENU_ITEM_KIND;

typedef unsigned COREWEBVIEW2_COOKIE_SAME_SITE_KIND;

typedef unsigned *PCOREWEBVIEW2_COOKIE_SAME_SITE_KIND;

typedef unsigned COREWEBVIEW2_PRINT_DIALOG_KIND;

typedef unsigned COREWEBVIEW2_PRINT_STATUS;

__interface  INTERFACE_UUID("{2FDE08A8-1E9A-4766-8C05-95A9CEB9D1C5}") ICoreWebView2EnvironmentOptions  : public System::IInterface 
{
	virtual HRESULT __stdcall get_AdditionalBrowserArguments(System::PPWideChar additionalBrowserArguments) = 0 ;
	virtual HRESULT __stdcall put_AdditionalBrowserArguments(System::WideChar * additionalBrowserArguments) = 0 ;
	virtual HRESULT __stdcall get_Language(System::PPWideChar language) = 0 ;
	virtual HRESULT __stdcall put_Language(System::WideChar * language) = 0 ;
	virtual HRESULT __stdcall get_TargetCompatibleBrowserVersion(System::PPWideChar targetCompatibleBrowserVersion) = 0 ;
	virtual HRESULT __stdcall put_TargetCompatibleBrowserVersion(System::WideChar * targetCompatibleBrowserVersion) = 0 ;
	virtual HRESULT __safecall Placeholder_get_AllowSingleSignOnUsingOSPrimaryAccount() = 0 ;
	virtual HRESULT __safecall Placeholder_put_AllowSingleSignOnUsingOSPrimaryAccount() = 0 ;
};

__interface  INTERFACE_UUID("{E562E4F0-D7FA-43AC-8D71-C05150499F00}") ICoreWebView2Settings  : public System::IInterface 
{
	virtual HRESULT __stdcall get_IsScriptEnabled(PBOOL isScriptEnabled) = 0 ;
	virtual HRESULT __stdcall put_IsScriptEnabled(System::LongBool isScriptEnabled) = 0 ;
	virtual HRESULT __stdcall get_IsWebMessageEnabled(PBOOL isWebMessageEnabled) = 0 ;
	virtual HRESULT __stdcall put_IsWebMessageEnabled(System::LongBool isWebMessageEnabled) = 0 ;
	virtual HRESULT __stdcall get_AreDefaultScriptDialogsEnabled(PBOOL areDefaultScriptDialogsEnabled) = 0 ;
	virtual HRESULT __stdcall put_AreDefaultScriptDialogsEnabled(System::LongBool areDefaultScriptDialogsEnabled) = 0 ;
	virtual HRESULT __stdcall get_IsStatusBarEnabled(PBOOL isStatusBarEnabled) = 0 ;
	virtual HRESULT __stdcall put_IsStatusBarEnabled(System::LongBool isStatusBarEnabled) = 0 ;
	virtual HRESULT __stdcall get_AreDevToolsEnabled(PBOOL areDevToolsEnabled) = 0 ;
	virtual HRESULT __stdcall put_AreDevToolsEnabled(System::LongBool areDevToolsEnabled) = 0 ;
	virtual HRESULT __stdcall get_AreDefaultContextMenusEnabled(PBOOL areDefaultContextMenusEnabled) = 0 ;
	virtual HRESULT __stdcall put_AreDefaultContextMenusEnabled(System::LongBool areDefaultContextMenusEnabled) = 0 ;
	virtual HRESULT __stdcall get_AreHostObjectsAllowed(PBOOL areHostObjectsAllowed) = 0 ;
	virtual HRESULT __stdcall put_AreHostObjectsAllowed(System::LongBool areHostObjectsAllowed) = 0 ;
	virtual HRESULT __stdcall get_IsZoomControlEnabled(PBOOL isZoomControlEnabled) = 0 ;
	virtual HRESULT __stdcall put_IsZoomControlEnabled(System::LongBool isZoomControlEnabled) = 0 ;
	virtual HRESULT __stdcall get_IsIsBuiltInErrorPageEnabled(PBOOL isIsBuiltInErrorPageEnabled) = 0 ;
	virtual HRESULT __stdcall put_IsIsBuiltInErrorPageEnabled(System::LongBool isIsBuiltInErrorPageEnabled) = 0 ;
};

__interface  INTERFACE_UUID("{EE9A0F68-F46C-4E32-AC23-EF8CAC224D2A}") ICoreWebView2Settings2  : public ICoreWebView2Settings 
{
	virtual HRESULT __stdcall get_UserAgent(System::PPWideChar userAgent) = 0 ;
	virtual HRESULT __stdcall put_UserAgent(System::WideChar * userAgent) = 0 ;
};

__interface  INTERFACE_UUID("{FDB5AB74-AF33-4854-84F0-0A631DEB5EBA}") ICoreWebView2Settings3  : public ICoreWebView2Settings2 
{
	virtual HRESULT __stdcall get_AreBrowserAcceleratorKeysEnabled(PBOOL areBrowseracceleratorKeysEnabled) = 0 ;
	virtual HRESULT __stdcall set_AreBrowserAcceleratorKeysEnabled(System::LongBool areBrowseracceleratorKeysEnabled) = 0 ;
};

__interface  INTERFACE_UUID("{CB56846C-4168-4D53-B04F-03B6D6796FF2}") ICoreWebView2Settings4  : public ICoreWebView2Settings3 
{
	virtual HRESULT __stdcall get_IsPasswordAutosaveEnabled(PBOOL isPasswordAutosaveEnabled) = 0 ;
	virtual HRESULT __stdcall set_IsPasswordAutosaveEnabled(System::LongBool isPasswordAutosaveEnabled) = 0 ;
	virtual HRESULT __stdcall get_IsGeneralAutofillEnabled(PBOOL isGeneralAutofillEnabled) = 0 ;
	virtual HRESULT __stdcall set_IsGeneralAutofillEnabled(System::LongBool isGeneralAutofillEnabled) = 0 ;
};

__interface  INTERFACE_UUID("{183E7052-1D03-43A0-AB99-98E043B66B39}") ICoreWebView2Settings5  : public ICoreWebView2Settings4 
{
	virtual HRESULT __stdcall get_IsPinchZoomEnabled(PBOOL isPinchZoomEnabled) = 0 ;
	virtual HRESULT __stdcall set_IsPinchZoomEnabled(System::LongBool isPinchZoomEnabled) = 0 ;
};

__interface  INTERFACE_UUID("{11CB3ACD-9BC8-43B8-83BF-F40753714F87}") ICoreWebView2Settings6  : public ICoreWebView2Settings5 
{
	virtual HRESULT __stdcall get_IsSwipeNavigationEnabled(PBOOL isSwipeNavigationEnabled) = 0 ;
	virtual HRESULT __stdcall set_IsSwipeNavigationEnabled(System::LongBool isSwipeNavigationEnabled) = 0 ;
};

__interface  INTERFACE_UUID("{488DC902-35EF-42D2-BC7D-94B65C4BC49C}") ICoreWebView2Settings7  : public ICoreWebView2Settings6 
{
	virtual HRESULT __stdcall get_HiddenPdfToolbarItems(PCOREWEBVIEW2_PDF_TOOLBAR_ITEMS hidden_pdf_toolbar_items) = 0 ;
	virtual HRESULT __stdcall put_HiddenPdfToolbarItems(unsigned hidden_pdf_toolbar_items) = 0 ;
};

__interface  INTERFACE_UUID("{30D68B7D-20D9-4752-A9CA-EC8448FBB5C1}") ICoreWebView2NavigationCompletedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_IsSuccess(PBOOL isSuccess) = 0 ;
	virtual HRESULT __stdcall get_WebErrorStatus(PCOREWEBVIEW2_WEB_ERROR_STATUS status) = 0 ;
	virtual HRESULT __stdcall get_NavigationId(System::PUInt64 navigation_id) = 0 ;
};

__interface  INTERFACE_UUID("{0702FC30-F43B-47BB-AB52-A42CB552AD9F}") ICoreWebView2HttpHeadersCollectionIterator  : public System::IInterface 
{
	virtual HRESULT __stdcall GetCurrentHeader(System::PPWideChar name, System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_HasCurrentHeader(PBOOL hasCurrent) = 0 ;
	virtual HRESULT __stdcall MoveNext(PBOOL hasNext) = 0 ;
};

__interface  INTERFACE_UUID("{03C5FF5A-9B45-4A88-881C-89A9F328619C}") ICoreWebView2HttpResponseHeaders  : public System::IInterface 
{
	virtual HRESULT __stdcall AppendHeader(System::WideChar * name, System::WideChar * value) = 0 ;
	virtual HRESULT __stdcall Contains(System::WideChar * name, PBOOL contains) = 0 ;
	virtual HRESULT __stdcall GetHeader(System::WideChar * name, System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall GetHeaders(System::WideChar * name, _di_ICoreWebView2HttpHeadersCollectionIterator &iterator) = 0 ;
	virtual HRESULT __stdcall GetIterator(_di_ICoreWebView2HttpHeadersCollectionIterator &iterator) = 0 ;
};

__interface  INTERFACE_UUID("{E86CAC0E-5523-465C-B536-8FB9FC8C8C60}") ICoreWebView2HttpRequestHeaders  : public System::IInterface 
{
	virtual HRESULT __stdcall GetHeader(System::WideChar * name, System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall GetHeaders(System::WideChar * name, _di_ICoreWebView2HttpHeadersCollectionIterator &iterator) = 0 ;
	virtual HRESULT __stdcall Contains(System::WideChar * name, PBOOL contains) = 0 ;
	virtual HRESULT __stdcall SetHeader(System::WideChar * name, System::WideChar * value) = 0 ;
	virtual HRESULT __stdcall RemoveHeader(System::WideChar * name) = 0 ;
	virtual HRESULT __stdcall GetIterator(_di_ICoreWebView2HttpHeadersCollectionIterator &iterator) = 0 ;
};

__interface  INTERFACE_UUID("{97055CD4-512C-4264-8B5F-E3F446CEA6A5}") ICoreWebView2WebResourceRequest  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Uri(System::PPWideChar uri) = 0 ;
	virtual HRESULT __stdcall put_Uri(System::WideChar * uri) = 0 ;
	virtual HRESULT __stdcall get_Method(System::PPWideChar method) = 0 ;
	virtual HRESULT __stdcall put_Method(System::WideChar * method) = 0 ;
	virtual HRESULT __stdcall get_Content(_di_IStream &content) = 0 ;
	virtual HRESULT __stdcall put_Content(_di_IStream content) = 0 ;
	virtual HRESULT __stdcall get_Headers(_di_ICoreWebView2HttpRequestHeaders &headers) = 0 ;
};

__interface  INTERFACE_UUID("{00F206A7-9D17-4605-91F6-4E8E4DE192E3}") ICoreWebView2TrySuspendCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, System::LongBool isSuccessful) = 0 ;
};

__interface  INTERFACE_UUID("{AAFCC94F-FA27-48FD-97DF-830EF75AAEC9}") ICoreWebView2WebResourceResponse  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Content(_di_IStream &content) = 0 ;
	virtual HRESULT __stdcall put_Content(_di_IStream content) = 0 ;
	virtual HRESULT __stdcall get_Headers(_di_ICoreWebView2HttpResponseHeaders &headers) = 0 ;
	virtual HRESULT __stdcall get_StatusCode(PUINT statusCode) = 0 ;
	virtual HRESULT __stdcall put_StatusCode(unsigned statusCode) = 0 ;
	virtual HRESULT __stdcall get_ReasonPhrase(System::PPWideChar reasonPhrase) = 0 ;
	virtual HRESULT __stdcall put_ReasonPhrase(System::WideChar * reasonPhrase) = 0 ;
};

__interface  INTERFACE_UUID("{C10E7F7B-B585-46F0-A623-8BEFBF3E4EE0}") ICoreWebView2Deferral  : public System::IInterface 
{
	virtual HRESULT __stdcall Complete() = 0 ;
};

__interface  INTERFACE_UUID("{453E667F-12C7-49D4-BE6D-DDBE7956F57A}") ICoreWebView2WebResourceRequestedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Request(_di_ICoreWebView2WebResourceRequest &request) = 0 ;
	virtual HRESULT __stdcall get_Response(_di_ICoreWebView2WebResourceResponse &response) = 0 ;
	virtual HRESULT __stdcall put_Response(_di_ICoreWebView2WebResourceResponse response) = 0 ;
	virtual HRESULT __stdcall GetDeferral(_di_ICoreWebView2Deferral &deferral) = 0 ;
	virtual HRESULT __stdcall get_ResourceContext(PCOREWEBVIEW2_WEB_RESOURCE_CONTEXT context) = 0 ;
};

__interface  INTERFACE_UUID("{9F760F8A-FB79-42BE-9990-7B56900FA9C7}") ICoreWebView2AcceleratorKeyPressedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_KeyEventKind(PCOREWEBVIEW2_KEY_EVENT_KIND keyEventKind) = 0 ;
	virtual HRESULT __stdcall get_VirtualKey(PUINT virtualKey) = 0 ;
	virtual HRESULT __stdcall get_KeyEventLParam(PINT lParam) = 0 ;
	virtual HRESULT __stdcall get_PhysicalKeyStatus(PCOREWEBVIEW2_PHYSICAL_KEY_STATUS physicalKeyStatus) = 0 ;
	virtual HRESULT __stdcall get_Handled(PBOOL handled) = 0 ;
	virtual HRESULT __stdcall put_Handled(System::LongBool handled) = 0 ;
};

__interface  INTERFACE_UUID("{973AE2EF-FF18-4894-8FB2-3C758F046810}") ICoreWebView2PermissionRequestedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Uri(System::PPWideChar uri) = 0 ;
	virtual HRESULT __stdcall get_PermissionKind(PCOREWEBVIEW2_PERMISSION_KIND permissionKind) = 0 ;
	virtual HRESULT __stdcall get_IsUserInitiated(PBOOL isUserInitiated) = 0 ;
	virtual HRESULT __stdcall get_State(PCOREWEBVIEW2_PERMISSION_STATE state) = 0 ;
	virtual HRESULT __stdcall put_State(unsigned state) = 0 ;
	virtual HRESULT __stdcall GetDeferral(_di_ICoreWebView2Deferral &deferral) = 0 ;
};

__interface  INTERFACE_UUID("{34ACB11C-FC37-4418-9132-F9C21D1EAFB9}") ICoreWebView2NewWindowRequestedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Uri(System::PPWideChar uri) = 0 ;
	virtual HRESULT __stdcall put_NewWindow(_di_ICoreWebView2 newWindow) = 0 ;
	virtual HRESULT __stdcall get_NewWindow(_di_ICoreWebView2 &newWindow) = 0 ;
	virtual HRESULT __stdcall put_Handled(System::LongBool handled) = 0 ;
	virtual HRESULT __stdcall get_Handled(PBOOL handled) = 0 ;
	virtual HRESULT __stdcall get_IsUserInitiated(PBOOL isUserInitiated) = 0 ;
	virtual HRESULT __stdcall GetDeferral(_di_ICoreWebView2Deferral &deferral) = 0 ;
	virtual HRESULT __safecall Placeholder_WindowFeatures() = 0 ;
};

__interface  INTERFACE_UUID("{16B1E21A-C503-44F2-84C9-70ABA5031283}") ICoreWebView2DOMContentLoadedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_NavigationId(System::PUInt64 navigation_ID) = 0 ;
};

__interface  INTERFACE_UUID("{5B495469-E119-438A-9B18-7604F25F2E49}") ICoreWebView2NavigationStartingEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Uri(System::PPWideChar uri) = 0 ;
	virtual HRESULT __stdcall get_IsUserInitiated(PBOOL isUserInitiated) = 0 ;
	virtual HRESULT __stdcall get_IsRedirected(PBOOL isRedirected) = 0 ;
	virtual HRESULT __stdcall get_RequestHeaders(_di_ICoreWebView2HttpRequestHeaders &requestHeaders) = 0 ;
	virtual HRESULT __stdcall get_Cancel(PBOOL cancel) = 0 ;
	virtual HRESULT __stdcall put_Cancel(System::LongBool cancel) = 0 ;
	virtual HRESULT __stdcall get_NavigationId(System::PUInt64 navigation_id) = 0 ;
};

__interface ICoreWebView2SourceChangedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_IsNewDocument(PBOOL isNewDocument) = 0 ;
};

__interface  INTERFACE_UUID("{0C8A1275-9B6B-4901-87AD-70DF25BAFA6E}") ICoreWebView2ContentLoadingEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_IsErrorPage(PBOOL isErrorPage) = 0 ;
	virtual HRESULT __stdcall get_NavigationId(System::PUInt64 navigation_ID) = 0 ;
};

__interface  INTERFACE_UUID("{9ADBE429-F36D-432B-9DDC-F8881FBD76E3}") ICoreWebView2NavigationStartingEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2NavigationStartingEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{364471E7-F2BE-4910-BDBA-D72077D51C4B}") ICoreWebView2ContentLoadingEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 webview, _di_ICoreWebView2ContentLoadingEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{3C067F9F-5388-4772-8B48-79F7EF1AB37C}") ICoreWebView2SourceChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2SourceChangedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{C79A420C-EFD9-4058-9295-3E8B4BCAB645}") ICoreWebView2HistoryChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{D33A35BF-1C49-4F98-93AB-006E0533FE1C}") ICoreWebView2NavigationCompletedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2NavigationCompletedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{AB00B74C-15F1-4646-80E8-E76341D25D71}") ICoreWebView2WebResourceRequestedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2WebResourceRequestedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{5C4889F0-5EF6-4C5A-952C-D8F1B92D0574}") ICoreWebView2CallDevToolsProtocolMethodCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, System::WideChar * returnObjectAsJson) = 0 ;
};

__interface  INTERFACE_UUID("{49511172-CC67-4BCA-9923-137112F4C4CC}") ICoreWebView2ExecuteScriptCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, System::WideChar * resultObjectAsJson) = 0 ;
};

__interface  INTERFACE_UUID("{B29C7E28-FA79-41A8-8E44-65811C76DCB2}") ICoreWebView2AcceleratorKeyPressedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2Controller sender, _di_ICoreWebView2AcceleratorKeyPressedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{D4C185FE-C81C-4989-97AF-2D3FA7AB5651}") ICoreWebView2NewWindowRequestedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2NewWindowRequestedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{4BAC7E9C-199E-49ED-87ED-249303ACF019}") ICoreWebView2DOMContentLoadedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2DOMContentLoadedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{05EA24BD-6452-4926-9014-4B82B498135D}") ICoreWebView2FocusChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{697E05E9-3D8F-45FA-96F4-8FFE1EDEDAF5}") ICoreWebView2CapturePreviewCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT res) = 0 ;
};

__interface  INTERFACE_UUID("{9DA43CCC-26E1-4DAD-B56C-D8961C94C571}") ICoreWebView2CursorChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2CompositionController sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{15E1C6A3-C72A-4DF3-91D7-D097FBEC6BFD}") ICoreWebView2PermissionRequestedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2PermissionRequestedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{76ECEACB-0462-4D94-AC83-423A6793775E}") ICoreWebView2  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Settings(_di_ICoreWebView2Settings &settings) = 0 ;
	virtual HRESULT __stdcall get_Source(System::PPWideChar source) = 0 ;
	virtual HRESULT __stdcall Navigate(System::WideChar * uri) = 0 ;
	virtual HRESULT __stdcall NavigateToString(System::WideChar * htmlContent) = 0 ;
	virtual HRESULT __stdcall add_NavigationStarting(_di_ICoreWebView2NavigationStartingEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_NavigationStarting(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_ContentLoading(_di_ICoreWebView2ContentLoadingEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_ContentLoading(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_SourceChanged(_di_ICoreWebView2SourceChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_SourceChanged(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_HistoryChanged(_di_ICoreWebView2HistoryChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_HistoryChanged(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_NavigationCompleted(_di_ICoreWebView2NavigationCompletedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_NavigationCompleted(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_FrameNavigationStarting(_di_ICoreWebView2NavigationStartingEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_FrameNavigationStarting(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_FrameNavigationCompleted(_di_ICoreWebView2NavigationCompletedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_FrameNavigationCompleted(EventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall Placeholder_add_ScriptDialogOpening() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ScriptDialogOpening() = 0 ;
	virtual HRESULT __stdcall add_PermissionRequested(_di_ICoreWebView2PermissionRequestedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_PermissionRequested(EventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall Placeholder_add_ProcessFailed() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ProcessFailed() = 0 ;
	virtual HRESULT __safecall Placeholder_AddScriptToExecuteOnDocumentCreated() = 0 ;
	virtual HRESULT __safecall Placeholder_RemoveScriptToExecuteOnDocumentCreated() = 0 ;
	virtual HRESULT __stdcall ExecuteScript(System::WideChar * javaScript, _di_ICoreWebView2ExecuteScriptCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall CapturePreview(unsigned imageFormat, _di_IStream imageStream, _di_ICoreWebView2CapturePreviewCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall Reload() = 0 ;
	virtual HRESULT __safecall Placeholder_PostWebMessageAsJSON() = 0 ;
	virtual HRESULT __safecall Placeholder_PostWebMessageAsString() = 0 ;
	virtual HRESULT __safecall Placeholder_add_WebMessageReceived() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_WebMessageReceived() = 0 ;
	virtual HRESULT __stdcall CallDevToolsProtocolMethod(System::WideChar * methodName, System::WideChar * parametersAsJson, _di_ICoreWebView2CallDevToolsProtocolMethodCompletedHandler handler) = 0 ;
	virtual HRESULT __safecall Placeholder_get_BrowserProcessId() = 0 ;
	virtual HRESULT __stdcall get_CanGoBack(PBOOL canGoBack) = 0 ;
	virtual HRESULT __stdcall get_CanGoForward(PBOOL canGoForward) = 0 ;
	virtual HRESULT __stdcall GoBack() = 0 ;
	virtual HRESULT __stdcall GoForward() = 0 ;
	virtual HRESULT __safecall Placeholder_GetDevToolsProtocolEventReceiver() = 0 ;
	virtual HRESULT __stdcall Stop() = 0 ;
	virtual HRESULT __stdcall add_NewWindowRequested(_di_ICoreWebView2NewWindowRequestedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_NewWindowRequested(EventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall Placeholder_add_DocumentTitleChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_DocumentTitleChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_DocumentTitle() = 0 ;
	virtual HRESULT __stdcall AddHostObjectToScript(System::WideChar * name, System::PVariant object) = 0 ;
	virtual HRESULT __stdcall RemoveHostObjectFromScript(System::WideChar * name) = 0 ;
	virtual HRESULT __stdcall OpenDevToolsWindow() = 0 ;
	virtual HRESULT __safecall Placeholder_add_ContainsFullScreenElementChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ContainsFullScreenElementChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_ContainsFullScreenElement() = 0 ;
	virtual HRESULT __stdcall add_WebResourceRequested(_di_ICoreWebView2WebResourceRequestedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_WebResourceRequested(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall AddWebResourceRequestedFilter(const System::WideChar * uri, const unsigned resourceContext) = 0 ;
	virtual HRESULT __safecall Placeholder_RemoveWebResourceRequestedFilter() = 0 ;
	virtual HRESULT __safecall Placeholder_add_WindowCloseRequested() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_WindowCloseRequested() = 0 ;
};

__interface  INTERFACE_UUID("{AD26D6BE-1486-43E6-BF87-A2034006CA21}") ICoreWebView2Cookie  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Name(System::PPWideChar name) = 0 ;
	virtual HRESULT __stdcall get_Value(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall put_Value(System::WideChar * value) = 0 ;
	virtual HRESULT __stdcall get_Domain(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_Path(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_Expires(System::PDouble expires) = 0 ;
	virtual HRESULT __stdcall put_Expires(double expires) = 0 ;
	virtual HRESULT __stdcall get_IsHttpOnly(PBOOL isHttpOnly) = 0 ;
	virtual HRESULT __stdcall put_IsHttpOnly(System::LongBool isHttpOnly) = 0 ;
	virtual HRESULT __stdcall get_SameSite(PCOREWEBVIEW2_COOKIE_SAME_SITE_KIND sameSite) = 0 ;
	virtual HRESULT __stdcall put_SameSite(unsigned sameSite) = 0 ;
	virtual HRESULT __stdcall get_IsSecure(PBOOL isSecure) = 0 ;
	virtual HRESULT __stdcall put_IsSecure(System::LongBool isSecure) = 0 ;
	virtual HRESULT __stdcall get_IsSession(PBOOL isSession) = 0 ;
};

__interface  INTERFACE_UUID("{F7F6F714-5D2A-43C6-9503-346ECE02D186}") ICoreWebView2CookieList  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Count(PUINT value) = 0 ;
	virtual HRESULT __stdcall GetValueAtIndex(unsigned index, _di_ICoreWebView2Cookie &cookie) = 0 ;
};

__interface  INTERFACE_UUID("{5A4F5069-5C15-47C3-8646-F4DE1C116670}") ICoreWebView2GetCookiesCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT res, _di_ICoreWebView2CookieList cookieList) = 0 ;
};

__interface  INTERFACE_UUID("{177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}") ICoreWebView2CookieManager  : public System::IInterface 
{
	virtual HRESULT __stdcall CreateCookie(System::WideChar * name, System::WideChar * value, System::WideChar * domain, System::WideChar * path, _di_ICoreWebView2Cookie &cookie) = 0 ;
	virtual HRESULT __stdcall CopyCookie(_di_ICoreWebView2Cookie cookieParam, _di_ICoreWebView2Cookie &cookie) = 0 ;
	virtual HRESULT __stdcall GetCookies(System::WideChar * uri, _di_ICoreWebView2GetCookiesCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall AddOrUpdateCookie(_di_ICoreWebView2Cookie cookie) = 0 ;
	virtual HRESULT __stdcall DeleteCookie(_di_ICoreWebView2Cookie cookie) = 0 ;
	virtual HRESULT __stdcall DeleteCookies(System::WideChar * name, System::WideChar * uri) = 0 ;
	virtual HRESULT __stdcall DeleteCookiesWithDomainAndPath(System::WideChar * name, System::WideChar * domain, System::WideChar * path) = 0 ;
	virtual HRESULT __stdcall DeleteAllCookies() = 0 ;
};

__interface  INTERFACE_UUID("{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}") ICoreWebView2_2  : public ICoreWebView2 
{
	virtual HRESULT __safecall Placeholder_add_WebResourceResponseReceived() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_WebResourceResponseReceived() = 0 ;
	virtual HRESULT __stdcall NavigateWithWebResourceRequest(_di_ICoreWebView2WebResourceRequest request) = 0 ;
	virtual HRESULT __stdcall add_DOMContentLoaded(_di_ICoreWebView2DOMContentLoadedEventHandler eventhandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_DOMContentLoaded(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall get_CookieManager(_di_ICoreWebView2CookieManager &cookieManager) = 0 ;
	virtual HRESULT __stdcall get_Environment(_di_ICoreWebView2Environment &environment) = 0 ;
};

__interface  INTERFACE_UUID("{A0D6DF20-3B92-416D-AA0C-437A9C727857}") ICoreWebView2_3  : public ICoreWebView2_2 
{
	virtual HRESULT __stdcall TrySuspend(_di_ICoreWebView2TrySuspendCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall Resume() = 0 ;
	virtual HRESULT __stdcall get_IsSuspended(PBOOL isSuspended) = 0 ;
	virtual HRESULT __stdcall SetVirtualHostNameToFolderMapping(System::WideChar * hostName, System::WideChar * folderPath, unsigned accessKind) = 0 ;
	virtual HRESULT __stdcall ClearVirtualHostNameToFolderMapping(System::WideChar * hostName) = 0 ;
};

__interface  INTERFACE_UUID("{81336594-7EDE-4BA9-BF71-ACF0A95B58DD}") ICoreWebView2StateChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2DownloadOperation sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{828E8AB6-D94C-4264-9CEF-5217170D6251}") ICoreWebView2BytesReceivedChangedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2DownloadOperation sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{3D6B6CF2-AFE1-44C7-A995-C65117714336}") ICoreWebView2DownloadOperation  : public System::IInterface 
{
	virtual HRESULT __stdcall add_BytesReceivedChanged(_di_ICoreWebView2BytesReceivedChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_BytesReceivedChanged(EventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall Placeholder_add_EstimatedEndTimeChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_EstimatedEndTimeChanged() = 0 ;
	virtual HRESULT __stdcall add_StateChanged(_di_ICoreWebView2StateChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_StateChanged(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall get_Uri(System::PPWideChar uri) = 0 ;
	virtual HRESULT __safecall Placeholder_get_ContentDisposition() = 0 ;
	virtual HRESULT __safecall Placeholder_get_MimeType() = 0 ;
	virtual HRESULT __stdcall get_TotalBytesToReceive(System::PInt64 totalBytesToReceive) = 0 ;
	virtual HRESULT __stdcall get_BytesReceived(System::PInt64 bytesReceived) = 0 ;
	virtual HRESULT __safecall Placeholder_get_EstimatedEndTime() = 0 ;
	virtual HRESULT __stdcall get_ResultFilePath(System::PPWideChar resultFilePath) = 0 ;
	virtual HRESULT __stdcall get_State(PCOREWEBVIEW2_DOWNLOAD_STATE state) = 0 ;
	virtual HRESULT __safecall Placeholder_get_InterruptReason() = 0 ;
	virtual HRESULT __safecall Cancel(HRESULT &__Cancel_result) = 0 ;
	virtual HRESULT __safecall Pause(HRESULT &__Pause_result) = 0 ;
	virtual HRESULT __safecall Resume(HRESULT &__Resume_result) = 0 ;
	virtual HRESULT __safecall CanResume(PBOOL canResume, HRESULT &__CanResume_result) = 0 ;
};

__interface  INTERFACE_UUID("{E99BBE21-43E9-4544-A732-282764EAFA60}") ICoreWebView2DownloadStartingEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_DownloadOperation(_di_ICoreWebView2DownloadOperation &downloadOperation) = 0 ;
	virtual HRESULT __safecall Placeholder_get_Cancel() = 0 ;
	virtual HRESULT __stdcall put_Cancel(System::LongBool cancel) = 0 ;
	virtual HRESULT __safecall Placeholder_get_ResultFilePath() = 0 ;
	virtual HRESULT __stdcall put_ResultFilePath(System::WideChar * resultFilePath) = 0 ;
	virtual HRESULT __safecall Placeholder_get_Handled() = 0 ;
	virtual HRESULT __stdcall put_Handled(System::LongBool handled) = 0 ;
	virtual HRESULT __stdcall GetDeferral(_di_ICoreWebView2Deferral &deferral) = 0 ;
};

__interface  INTERFACE_UUID("{EFEDC989-C396-41CA-83F7-07F845A55724}") ICoreWebView2DownloadStartingEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2DownloadStartingEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{20D02D59-6DF2-42DC-BD06-F98A694B1302}") ICoreWebView2_4  : public ICoreWebView2_3 
{
	virtual HRESULT __safecall Placeholder_add_FrameCreated() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_FrameCreated() = 0 ;
	virtual HRESULT __stdcall add_DownloadStarting(_di_ICoreWebView2DownloadStartingEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_DownloadStarting(EventRegistrationToken token) = 0 ;
};

__interface  INTERFACE_UUID("{BEDB11B8-D63C-11EB-B8BC-0242AC130003}") ICoreWebView2_5  : public ICoreWebView2_4 
{
	virtual HRESULT __safecall Placeholder_add_ClientCertificateRequested() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ClientCertificateRequested() = 0 ;
};

__interface  INTERFACE_UUID("{499AADAC-D92C-4589-8A75-111BFC167795}") ICoreWebView2_6  : public ICoreWebView2_5 
{
	virtual HRESULT __stdcall OpenTaskManagerWindow() = 0 ;
};

__interface  INTERFACE_UUID("{377F3721-C74E-48CA-8DB1-DF68E51D60E2}") ICoreWebView2PrintSettings  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Orientation(PCOREWEBVIEW2_PRINT_ORIENTATION orientation) = 0 ;
	virtual HRESULT __stdcall put_Orientation(unsigned orientation) = 0 ;
	virtual HRESULT __stdcall get_ScaleFactor(System::PDouble scaleFactor) = 0 ;
	virtual HRESULT __stdcall put_ScaleFactor(double scaleFactor) = 0 ;
	virtual HRESULT __stdcall get_PageWidth(System::PDouble pageWidth) = 0 ;
	virtual HRESULT __stdcall put_PageWidth(double pageWidth) = 0 ;
	virtual HRESULT __stdcall get_PageHeight(System::PDouble pageHeight) = 0 ;
	virtual HRESULT __stdcall put_PageHeight(double pageHeight) = 0 ;
	virtual HRESULT __stdcall get_MarginTop(System::PDouble marginTop) = 0 ;
	virtual HRESULT __stdcall put_MarginTop(double marginTop) = 0 ;
	virtual HRESULT __stdcall get_MarginBottom(System::PDouble marginBottom) = 0 ;
	virtual HRESULT __stdcall put_MarginBottom(double marginBottom) = 0 ;
	virtual HRESULT __stdcall get_MarginLeft(System::PDouble marginLeft) = 0 ;
	virtual HRESULT __stdcall put_MarginLeft(double marginLeft) = 0 ;
	virtual HRESULT __stdcall get_MarginRight(System::PDouble marginRight) = 0 ;
	virtual HRESULT __stdcall put_MarginRight(double marginRight) = 0 ;
	virtual HRESULT __stdcall get_ShouldPrintBackgrounds(PBOOL shouldPrintBackgrounds) = 0 ;
	virtual HRESULT __stdcall put_ShouldPrintBackgrounds(System::LongBool shouldPrintBackgrounds) = 0 ;
	virtual HRESULT __stdcall get_ShouldPrintSelectionOnly(PBOOL shouldPrintSelectionOnly) = 0 ;
	virtual HRESULT __stdcall put_ShouldPrintSelectionOnly(System::LongBool shouldPrintSelectionOnly) = 0 ;
	virtual HRESULT __stdcall get_ShouldPrintHeaderAndFooter(PBOOL shouldPrintHeaderAndFooter) = 0 ;
	virtual HRESULT __stdcall put_ShouldPrintHeaderAndFooter(System::LongBool shouldPrintHeaderAndFooter) = 0 ;
	virtual HRESULT __stdcall get_HeaderTitle(System::PPWideChar headerTitle) = 0 ;
	virtual HRESULT __stdcall put_HeaderTitle(System::WideChar * headerTitle) = 0 ;
	virtual HRESULT __stdcall get_FooterUri(System::PPWideChar footerUri) = 0 ;
	virtual HRESULT __stdcall put_FooterUri(System::WideChar * footerUri) = 0 ;
};

__interface  INTERFACE_UUID("{CCF1EF04-FD8E-4D5F-B2DE-0983E41B8C36}") ICoreWebView2PrintToPdfCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, bool isSuccessful) = 0 ;
};

__interface  INTERFACE_UUID("{79C24D83-09A3-45AE-9418-487F32A58740}") ICoreWebView2_7  : public ICoreWebView2_6 
{
	virtual HRESULT __stdcall PrintToPdf(System::WideChar * resultfilePath, _di_ICoreWebView2PrintSettings printSettings, _di_ICoreWebView2PrintToPdfCompletedHandler handler) = 0 ;
};

__interface  INTERFACE_UUID("{E9632730-6E1E-43AB-B7B8-7B2C9E62E094}") ICoreWebView2_8  : public ICoreWebView2_7 
{
	virtual HRESULT __safecall Placeholder_add_IsMutedChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_IsMutedChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_IsMuted() = 0 ;
	virtual HRESULT __safecall Placeholder_put_IsMuted() = 0 ;
	virtual HRESULT __safecall Placeholder_add_IsDocumentPlayingAudioChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_IsDocumentPlayingAudioChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_IsDocumentPlayingAudio() = 0 ;
};

__interface  INTERFACE_UUID("{4D7B2EAB-9FDC-468D-B998-A9260B5ED651}") ICoreWebView2_9  : public ICoreWebView2_8 
{
	virtual HRESULT __safecall Placeholder_add_IsDefaultDownloadDialogOpenChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_IsDefaultDownloremoveialogOpenChanged() = 0 ;
	virtual HRESULT __stdcall get_IsDefaultDownloadDialogOpen(System::PBoolean isDefaultDownloadDialogOpen) = 0 ;
	virtual HRESULT __stdcall OpenDefaultDownloadDialog() = 0 ;
	virtual HRESULT __stdcall CloseDefaultDownloadDialog() = 0 ;
	virtual HRESULT __safecall Placeholder_get_DefaultDownloadDialogCornerAlignment() = 0 ;
	virtual HRESULT __safecall Placeholder_put_DefaultDownloadDialogCornerAlignment() = 0 ;
	virtual HRESULT __safecall Placeholder_get_DefaultDownloadDialogMargin() = 0 ;
	virtual HRESULT __safecall Placeholder_put_DefaultDownloadDialogMargin() = 0 ;
};

__interface  INTERFACE_UUID("{B1690564-6F5A-4983-8E48-31D1143FECDB}") ICoreWebView2_10  : public ICoreWebView2_9 
{
	virtual HRESULT __safecall Placeholder_add_BasicAuthenticationRequested() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_BasicAuthenticationRequested() = 0 ;
};

__interface  INTERFACE_UUID("{49E1D0BC-FE9E-4481-B7C2-32324AA21998}") ICoreWebView2CustomItemSelectedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2ContextMenuItem sender, System::_di_IInterface args) = 0 ;
};

__interface  INTERFACE_UUID("{7AED49E3-A93F-497A-811C-749C6B6B6C65}") ICoreWebView2ContextMenuItem  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Name(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_Label(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_CommandId(PUINT value) = 0 ;
	virtual HRESULT __stdcall get_ShortcutKeyDescription(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_Icon(_di_IStream &value) = 0 ;
	virtual HRESULT __stdcall get_Kind(PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND value) = 0 ;
	virtual HRESULT __stdcall put_IsEnabled(System::LongBool value) = 0 ;
	virtual HRESULT __stdcall get_IsEnabled(PBOOL value) = 0 ;
	virtual HRESULT __stdcall put_IsChecked(System::LongBool value) = 0 ;
	virtual HRESULT __stdcall get_IsChecked(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_Children(_di_ICoreWebView2ContextMenuItemCollection &value) = 0 ;
	virtual HRESULT __stdcall add_CustomItemSelected(_di_ICoreWebView2CustomItemSelectedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_CustomItemSelected(EventRegistrationToken token) = 0 ;
};

__interface  INTERFACE_UUID("{F562A2F5-C415-45CF-B909-D4B7C1E276D3}") ICoreWebView2ContextMenuItemCollection  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Count(PUINT value) = 0 ;
	virtual HRESULT __stdcall GetValueAtIndex(unsigned index, _di_ICoreWebView2ContextMenuItem &value) = 0 ;
	virtual HRESULT __stdcall RemoveValueAtIndex(unsigned index) = 0 ;
	virtual HRESULT __stdcall InsertValueAtIndex(unsigned index, _di_ICoreWebView2ContextMenuItem value) = 0 ;
};

__interface  INTERFACE_UUID("{B8611D99-EED6-4F3F-902C-A198502AD472}") ICorewebView2ContextMenuTarget  : public System::IInterface 
{
	virtual HRESULT __stdcall get_Kind(PCOREWEBVIEW2_CONTEXT_MENU_TARGET_KIND value) = 0 ;
	virtual HRESULT __stdcall get_IsEditable(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_IsRequestedForMainFrame(System::PBoolean value) = 0 ;
	virtual HRESULT __stdcall get_PageUri(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_FrameUri(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_HasLinkUri(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_LinkUri(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_HasLinkText(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_LinkText(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_HasSourceUri(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_SourceUri(System::PPWideChar value) = 0 ;
	virtual HRESULT __stdcall get_HasSelection(PBOOL value) = 0 ;
	virtual HRESULT __stdcall get_SelectionText(System::PPWideChar value) = 0 ;
};

__interface  INTERFACE_UUID("{A1D309EE-C03F-11EB-8529-0242AC130003}") ICoreWebView2ContextMenuRequestedEventArgs  : public System::IInterface 
{
	virtual HRESULT __stdcall get_MenuItems(/* out */ _di_ICoreWebView2ContextMenuItemCollection &value) = 0 ;
	virtual HRESULT __stdcall get_ContextMenuTarget(/* out */ _di_ICorewebView2ContextMenuTarget &value) = 0 ;
	virtual HRESULT __stdcall get_Location(System::Types::PPoint value) = 0 ;
	virtual HRESULT __stdcall put_SelectedCommandId(int value) = 0 ;
	virtual HRESULT __stdcall get_SelectedCommandId(PINT value) = 0 ;
	virtual HRESULT __stdcall put_Handled(bool value) = 0 ;
	virtual HRESULT __stdcall get_Handled(System::PBoolean value) = 0 ;
	virtual HRESULT __stdcall GetDeferral(_di_ICoreWebView2Deferral &deferral) = 0 ;
};

__interface  INTERFACE_UUID("{04D3FE1D-AB87-42FB-A898-DA241D35B63C}") ICoreWebView2ContextMenuRequestedEventHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(_di_ICoreWebView2 sender, _di_ICoreWebView2ContextMenuRequestedEventArgs args) = 0 ;
};

__interface  INTERFACE_UUID("{0BE78E56-C193-4051-B943-23B460C08BDB}") ICoreWebView2_11  : public ICoreWebView2_10 
{
	virtual HRESULT __safecall Placeholder_CallDevToolsProtocolMethodForSession() = 0 ;
	virtual HRESULT __stdcall add_ContextMenuRequested(_di_ICoreWebView2ContextMenuRequestedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_ContextMenuRequested(EventRegistrationToken token) = 0 ;
};

__interface  INTERFACE_UUID("{35D69927-BCFA-4566-9349-6B3E0D154CAC}") ICoreWebView2_12  : public ICoreWebView2_11 
{
	virtual HRESULT __safecall Placeholder_add_StatusBarTextChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_StatusBarText() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_StatusBarTextChanged() = 0 ;
};

__interface  INTERFACE_UUID("{F75F09A8-667E-4983-88D6-C8773F315E84}") ICoreWebView2_13  : public ICoreWebView2_12 
{
	virtual HRESULT __safecall Placeholder_get_Profile() = 0 ;
};

__interface  INTERFACE_UUID("{6DAA4F10-4A90-4753-8898-77C5DF534165}") ICoreWebView2_14  : public ICoreWebView2_13 
{
	virtual HRESULT __safecall Placeholder_add_ServerCertificateErrorDetected() = 0 ;
	virtual HRESULT __safecall Placeholder_ClearServerCertificateErrorActions() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ServerCertificateErrorDetected() = 0 ;
};

__interface  INTERFACE_UUID("{517B2D1D-7DAE-4A66-A4F4-10352FFB9518}") ICoreWebView2_15  : public ICoreWebView2_14 
{
	virtual HRESULT __safecall Placeholder_add_FaviconChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_FaviconUri() = 0 ;
	virtual HRESULT __safecall Placeholder_GetFavicon() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_FaviconChanged() = 0 ;
};

__interface  INTERFACE_UUID("{8FD80075-ED08-42DB-8570-F5D14977461E}") ICoreWebView2PrintCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, System::LongBool printStatus) = 0 ;
};

__interface  INTERFACE_UUID("{4C9F8229-8F93-444F-A711-2C0DFD6359D5}") ICoreWebView2PrintToPdfStreamCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, _di_IStream pdfStream) = 0 ;
};

__interface  INTERFACE_UUID("{0EB34DC9-9F91-41E1-8639-95CD5943906B}") ICoreWebView2_16  : public ICoreWebView2_15 
{
	virtual HRESULT __stdcall Print(_di_ICoreWebView2PrintSettings printSettings, _di_ICoreWebView2PrintCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall ShowPrintUI(unsigned printDialogKind) = 0 ;
	virtual HRESULT __stdcall PrintToPdfStream(_di_ICoreWebView2PrintSettings printSettings, _di_ICoreWebView2PrintToPdfStreamCompletedHandler handler) = 0 ;
};

__interface  INTERFACE_UUID("{4D00C0D1-9434-4EB6-8078-8697A560334F}") ICoreWebView2Controller  : public System::IInterface 
{
	virtual HRESULT __safecall Placeholder_get_IsVisible() = 0 ;
	virtual HRESULT __stdcall put_IsVisible(System::LongBool isVisible) = 0 ;
	virtual HRESULT __stdcall get_Bounds(System::Types::PRect bounds) = 0 ;
	virtual HRESULT __stdcall put_Bounds(System::Types::TRect bounds) = 0 ;
	virtual HRESULT __stdcall get_ZoomFactor(System::PDouble zoomFactor) = 0 ;
	virtual HRESULT __stdcall put_ZoomFactor(double zoomFactor) = 0 ;
	virtual HRESULT __safecall Placeholder_add_ZoomFactorChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_ZoomFactorChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_SetBoundsAndZoomFactor() = 0 ;
	virtual HRESULT __stdcall MoveFocus(unsigned reason) = 0 ;
	virtual HRESULT __safecall Placeholder_add_MoveFocusRequested() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_MoveFocusRequested() = 0 ;
	virtual HRESULT __stdcall add_GotFocus(_di_ICoreWebView2FocusChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_GotFocus(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_LostFocus(_di_ICoreWebView2FocusChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_LostFocus(EventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall add_AcceleratorKeyPressed(_di_ICoreWebView2AcceleratorKeyPressedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __stdcall remove_AcceleratorKeyPressed(EventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall Placeholder_get_ParentWindow() = 0 ;
	virtual HRESULT __stdcall put_ParentWindow(HWND parentWindow) = 0 ;
	virtual HRESULT __stdcall NotifyParentWindowPositionChanged() = 0 ;
	virtual HRESULT __stdcall Close() = 0 ;
	virtual HRESULT __stdcall get_CoreWebView2(_di_ICoreWebView2 &coreWebView2) = 0 ;
};

__interface  INTERFACE_UUID("{C979903E-D4CA-4228-92EB-47EE3FA96EAB}") ICoreWebView2Controller2  : public ICoreWebView2Controller 
{
	virtual HRESULT __stdcall get_DefaultBackgroundColor(PCOREWEBVIEW2_COLOR backgroundColor) = 0 ;
	virtual HRESULT __stdcall put_DefaultBackgroundColor(COREWEBVIEW2_COLOR backgroundColor) = 0 ;
};

__interface  INTERFACE_UUID("{F9614724-5D2B-41DC-AEF7-73D62B51543B}") ICoreWebView2Controller3  : public ICoreWebView2Controller2 
{
	virtual HRESULT __safecall Placeholder_add_RasterizationScaleChanged() = 0 ;
	virtual HRESULT __safecall Placeholder_get_BoundsMode() = 0 ;
	virtual HRESULT __safecall Placeholder_get_RasterizationScale() = 0 ;
	virtual HRESULT __safecall Placeholder_get_ShouldDetectMonitorScaleChanges() = 0 ;
	virtual HRESULT __safecall Placeholder_put_BoundsMode() = 0 ;
	virtual HRESULT __safecall Placeholder_put_RasterizationScale() = 0 ;
	virtual HRESULT __safecall Placeholder_put_ShouldDetectMonitorScaleChanges() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_RasterizationScaleChanged() = 0 ;
};

__interface  INTERFACE_UUID("{97D418D5-A426-4E49-A151-E1A10F327D9E}") ICoreWebView2Controller4  : public ICoreWebView2Controller3 
{
	virtual HRESULT __stdcall get_AllowExternalDrop(PBOOL value) = 0 ;
	virtual HRESULT __stdcall put_AllowExternalDrop(System::LongBool value) = 0 ;
};

__interface  INTERFACE_UUID("{3DF9B733-B9AE-4A15-86B4-EB9EE9826469}") ICoreWebView2CompositionController  : public System::IInterface 
{
	virtual HRESULT __safecall Placeholder_get_RootVisualTarget() = 0 ;
	virtual HRESULT __safecall Placeholder_put_RootVisualTarget() = 0 ;
	virtual HRESULT __safecall Placeholder_SendMouseInput() = 0 ;
	virtual HRESULT __safecall Placeholder_SendPointerInput() = 0 ;
	virtual HRESULT __safecall get_Cursor(PHCURSOR cursor, HRESULT &__get_Cursor_result) = 0 ;
	virtual HRESULT __safecall Placeholder_get_SystemCursorId() = 0 ;
	virtual HRESULT __stdcall add_CursorChanged(_di_ICoreWebView2CursorChangedEventHandler eventHandler, PEventRegistrationToken token) = 0 ;
	virtual HRESULT __safecall remove_CursorChanged(EventRegistrationToken token, HRESULT &__remove_CursorChanged_result) = 0 ;
};

__interface  INTERFACE_UUID("{0B6A3D24-49CB-4806-BA20-B5E0734A7B26}") ICoreWebView2CompositionController2  : public ICoreWebView2CompositionController 
{
	virtual HRESULT __safecall Placeholder_get_AutomationProvider() = 0 ;
	virtual HRESULT __safecall Placeholder_put_AutomationProvider() = 0 ;
};

__interface  INTERFACE_UUID("{9570570E-4D76-4361-9EE1-F04D0DBDFB1E}") ICoreWebView2CompositionController3  : public ICoreWebView2CompositionController2 
{
	
};

__interface  INTERFACE_UUID("{6C4819F3-C9B7-4260-8127-C9F5BDE7F68C}") ICoreWebView2CreateCoreWebView2ControllerCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT res, _di_ICoreWebView2Controller createdController) = 0 ;
};

__interface  INTERFACE_UUID("{B96D755E-0319-4E92-A296-23436F46A1FC}") ICoreWebView2Environment  : public System::IInterface 
{
	virtual HRESULT __stdcall CreateCoreWebView2Controller(HWND parentWindow, _di_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler handler) = 0 ;
	virtual HRESULT __stdcall CreateWebResourceResponse(const _di_IStream Content, int StatusCode, System::WideChar * ReasonPhrase, System::WideChar * Headers, /* out */ _di_ICoreWebView2WebResourceResponse &Response) = 0 ;
	virtual HRESULT __safecall Placeholder_get_BrowserVersionString() = 0 ;
	virtual HRESULT __safecall Placeholder_add_NewBrowserVersionAvailable() = 0 ;
	virtual HRESULT __safecall Placeholder_remove_NewBrowserVersionAvailable() = 0 ;
};

__interface  INTERFACE_UUID("{41F3632B-5EF4-404F-AD82-2D606C5A9A21}") ICoreWebView2Environment2  : public ICoreWebView2Environment 
{
	virtual HRESULT __stdcall CreateWebResourceRequest(System::WideChar * uri, System::WideChar * method, _di_IStream postData, System::WideChar * headers, /* out */ _di_ICoreWebView2WebResourceRequest &request) = 0 ;
};

__interface  INTERFACE_UUID("{02FAB84B-1428-4FB7-AD45-1B2E64736184}") ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT errorCode, _di_ICoreWebView2CompositionController webView) = 0 ;
};

__interface  INTERFACE_UUID("{80A22AE3-BE7C-4CE2-AFE1-5A50056CDEEB}") ICoreWebView2Environment3  : public ICoreWebView2Environment2 
{
	virtual HRESULT __stdcall CreateCoreWebView2CompositionController(HWND parentWindow, _di_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler handler) = 0 ;
	virtual HRESULT __safecall PlaceHolder_CreateCoreWebView2PointerInfo() = 0 ;
};

__interface  INTERFACE_UUID("{20944379-6DCF-41D6-A0A0-ABC0FC50DE0D}") ICoreWebView2Environment4  : public ICoreWebView2Environment3 
{
	virtual HRESULT __safecall PlaceHolder_GetAutomationProviderForWindow() = 0 ;
};

__interface  INTERFACE_UUID("{319E423D-E0D7-4B8D-9254-AE9475DE9B17}") ICoreWebView2Environment5  : public ICoreWebView2Environment4 
{
	virtual HRESULT __safecall PlaceHolder_add_BrowserProcessExited() = 0 ;
	virtual HRESULT __safecall PlaceHolder_remove_BrowserProcessExited() = 0 ;
};

__interface  INTERFACE_UUID("{E59EE362-ACBD-4857-9A8E-D3644D9459A9}") ICoreWebView2Environment6  : public ICoreWebView2Environment5 
{
	virtual HRESULT __stdcall CreatePrintSettings(_di_ICoreWebView2PrintSettings &printSettings) = 0 ;
};

__interface  INTERFACE_UUID("{43C22296-3BBD-43A4-9C00-5C0DF6DD29A2}") ICoreWebView2Environment7  : public ICoreWebView2Environment6 
{
	virtual HRESULT __safecall PlaceHolder_get_UserDataFolder() = 0 ;
};

__interface  INTERFACE_UUID("{D6EB91DD-C3D2-45E5-BD29-6DC2BC4DE9CF}") ICoreWebView2Environment8  : public ICoreWebView2Environment7 
{
	virtual HRESULT __safecall PlaceHolder_add_ProcessInfosChanged() = 0 ;
	virtual HRESULT __safecall PlaceHolder_remove_ProcessInfosChanged() = 0 ;
	virtual HRESULT __safecall PlaceHolder_GetProcessInfos() = 0 ;
};

__interface  INTERFACE_UUID("{F06F41BF-4B5A-49D8-B9F6-FA16CD29F274}") ICoreWebView2Environment9  : public ICoreWebView2Environment8 
{
	virtual HRESULT __stdcall CreateContextMenuItem(System::WideChar * label, _di_IStream iconStream, unsigned kind, /* out */ _di_ICoreWebView2ContextMenuItem &item) = 0 ;
};

__interface  INTERFACE_UUID("{4E8A3389-C9D8-4BD2-B6B5-124FEE6CC14D}") ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Invoke(HRESULT result, _di_ICoreWebView2Environment webViewEnvironment) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_MOVE_FOCUS_REASON_NEXT = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_MOVE_FOCUS_REASON_PREVIOUS = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_NONE = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SAVE = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PRINT = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SAVE_AS = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ZOOM_IN = System::Int8(0x8);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ZOOM_OUT = System::Int8(0x10);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_ROTATE = System::Int8(0x20);
static const System::Int8 COREWEBVIEW2_PDF_TOOLBAR_ITEMS_FIT_PAGE = System::Int8(0x40);
static const System::Byte COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PAGE_LAYOUT = System::Byte(0x80);
static const System::Word COREWEBVIEW2_PDF_TOOLBAR_ITEMS_BOOKMARKS = System::Word(0x100);
static const System::Word COREWEBVIEW2_PDF_TOOLBAR_ITEMS_PAGE_SELECTOR = System::Word(0x200);
static const System::Word COREWEBVIEW2_PDF_TOOLBAR_ITEMS_SEARCH = System::Word(0x400);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_COMMAND = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_CHECK_BOX = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_RADIO = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SEPARATOR = System::Int8(0x3);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SUBMENU = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PRINT_STATUS_SUCCEEDED = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PRINT_STATUS_PRINTER_UNAVAILABLE = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PRINT_STATUS_OTHER_ERROR = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_DENY = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_DENY_CORS = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS = System::Int8(0x3);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID = System::Int8(0x5);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE = System::Int8(0x6);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT = System::Int8(0x7);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE = System::Int8(0x8);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED = System::Int8(0x9);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET = System::Int8(0xa);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED = System::Int8(0xb);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT = System::Int8(0xc);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED = System::Int8(0xd);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED = System::Int8(0xe);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED = System::Int8(0xf);
static const System::Int8 COREWEBVIEW2_WEB_ERROR_STATUS_UNEXPECTED_ERROR = System::Int8(0x10);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_DOCUMENT = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_STYLESHEET = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE = System::Int8(0x3);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MEDIA = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FONT = System::Int8(0x5);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SCRIPT = System::Int8(0x6);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_XML_HTTP_REQUEST = System::Int8(0x7);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FETCH = System::Int8(0x8);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_TEXT_TRACK = System::Int8(0x9);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_EVENT_SOURCE = System::Int8(0xa);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_WEBSOCKET = System::Int8(0xb);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MANIFEST = System::Int8(0xc);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SIGNED_EXCHANGE = System::Int8(0xd);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_PING = System::Int8(0xe);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_CSP_VIOLATION_REPORT = System::Int8(0xf);
static const System::Int8 COREWEBVIEW2_WEB_RESOURCE_CONTEXT_OTHER = System::Int8(0x10);
static const System::Int8 COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_JPEG = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_UNKNOWN_PERMISSION = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_MICROPHONE = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_CAMERA = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_GEOLOCATION = System::Int8(0x3);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_NOTIFICATIONS = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_OTHER_SENSORS = System::Int8(0x5);
static const System::Int8 COREWEBVIEW2_PERMISSION_KIND_CLIPBOARD_READ = System::Int8(0x6);
static const System::Int8 COREWEBVIEW2_PERMISSION_STATE_DEFAULT = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PERMISSION_STATE_ALLOW = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_PERMISSION_STATE_DENY = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_DOWNLOAD_STATE_INTERRUPTED = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_DOWNLOAD_STATE_COMPLETED = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_PRINT_ORIENTATION_PORTRAIT = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_PRINT_ORIENTATION_LANDSCAPE = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_PAGE = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_IMAGE = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_SELECTED_TEXT = System::Int8(0x2);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_AUDIO = System::Int8(0x3);
static const System::Int8 COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_VIDEO = System::Int8(0x4);
static const System::Int8 COREWEBVIEW2_COOKIE_SAME_SITE_KIND_NONE = System::Int8(0x0);
static const System::Int8 COREWEBVIEW2_COOKIE_SAME_SITE_KIND_LAX = System::Int8(0x1);
static const System::Int8 COREWEBVIEW2_COOKIE_SAME_SITE_KIND_STRICT = System::Int8(0x2);
#define IID_ICoreWebView2CookieGUID L"{AD26D6BE-1486-43E6-BF87-A2034006CA21}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Cookie;
#define IID_ICoreWebView2CookieListGUID L"{F7F6F714-5D2A-43C6-9503-346ECE02D186}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CookieList;
#define IID_ICoreWebView2CookieManagerGUID L"{177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CookieManager;
#define IID_ICoreWebView2GetCookiesCompletedHandlerGUID L"{5A4F5069-5C15-47C3-8646-F4DE1C116670}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2GetCookiesCompletedHandler;
#define IID_ICoreWebView2EnvironmentOptionsGUID L"{2fde08a8-1e9a-4766-8c05-95a9ceb9d1c5}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2EnvironmentOptions;
#define IID_ICoreWebView2SettingsGUID L"{e562e4f0-d7fa-43ac-8d71-c05150499f00}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings;
#define IID_ICoreWebView2Settings2GUID L"{ee9a0f68-f46c-4e32-ac23-ef8cac224d2a}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings2;
#define IID_ICoreWebView2Settings3GUID L"{fdb5ab74-af33-4854-84f0-0a631deb5eba}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings3;
#define IID_ICoreWebView2Settings4GUID L"{cb56846c-4168-4d53-b04f-03b6d6796ff2}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings4;
#define IID_ICoreWebView2Settings5GUID L"{183e7052-1d03-43a0-ab99-98e043b66b39}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings5;
#define IID_ICoreWebView2Settings6GUID L"{11cb3acd-9bc8-43b8-83bf-f40753714f87}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings6;
#define IID_ICoreWebView2Settings7GUID L"{488dc902-35ef-42d2-bc7d-94b65c4bc49c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Settings7;
#define IID_ICoreWebView2NavigationCompletedEventArgsGUID L"{30d68b7d-20d9-4752-a9ca-ec8448fbb5c1}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NavigationCompletedEventArgs;
#define IID_ICoreWebView2HttpHeadersCollectionIteratorGUID L"{0702fc30-f43b-47bb-ab52-a42cb552ad9f}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2HttpHeadersCollectionIterator;
#define IID_ICoreWebView2HttpResponseHeadersGUID L"{03c5ff5a-9b45-4a88-881c-89a9f328619c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2HttpResponseHeaders;
#define IID_ICoreWebView2HttpRequestHeadersGUID L"{e86cac0e-5523-465c-b536-8fb9fc8c8c60}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2HttpRequestHeaders;
#define IID_ICoreWebView2WebResourceRequestGUID L"{97055cd4-512c-4264-8b5f-e3f446cea6a5}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2WebResourceRequest;
#define IID_ICoreWebView2WebResourceResponseGUID L"{aafcc94f-fa27-48fd-97df-830ef75aaec9}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2WebResourceResponse;
#define IID_ICoreWebView2DeferralGUID L"{c10e7f7b-b585-46f0-a623-8befbf3e4ee0}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Deferral;
#define IID_ICoreWebView2WebResourceRequestedEventArgsGUID L"{453e667f-12c7-49d4-be6d-ddbe7956f57a}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2WebResourceRequestedEventArgs;
#define IID_ICoreWebView2AcceleratorKeyPressedEventArgsGUID L"{9f760f8a-fb79-42be-9990-7b56900fa9c7}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2AcceleratorKeyPressedEventArgs;
#define IID_ICoreWebView2NewWindowRequestedEventArgsGUID L"{34acb11c-fc37-4418-9132-f9c21d1eafb9}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NewWindowRequestedEventArgs;
#define IID_ICoreWebView2DOMContentLoadedEventArgsGUID L"{16B1E21A-C503-44F2-84C9-70ABA5031283}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2DOMContentLoadedEventArgs;
#define IID_ICoreWebView2PermissionRequestedEventArgsGUID L"{973ae2ef-ff18-4894-8fb2-3c758f046810}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PermissionRequestedEventArgs;
#define IID_ICoreWebView2ContentLoadingEventArgsGUID L"{0c8a1275-9b6b-4901-87ad-70df25bafa6e}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContentLoadingEventArgs;
#define IID_ICoreWebView2SourceChangedEventArgsGUID L"{31e0e545-1dba-4266-8914-f63848a1f7d7}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2SourceChangedEventArgs;
#define IID_ICoreWebView2NavigationStartingEventArgsGUID L"{5b495469-e119-438a-9b18-7604f25f2e49}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NavigationStartingEventArgs;
#define IID_ICoreWebView2TrySuspendCompletedHandlerGUID L"{00F206A7-9D17-4605-91F6-4E8E4DE192E3}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2TrySuspendCompletedHandler;
#define IID_ICoreWebView2NavigationStartingEventHandlerGUID L"{9adbe429-f36d-432b-9ddc-f8881fbd76e3}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NavigationStartingEventHandler;
#define IID_ICoreWebView2SourceChangedEventHandlerGUID L"{3c067f9f-5388-4772-8b48-79f7ef1ab37c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2SourceChangedEventHandler;
#define IID_ICoreWebView2HistoryChangedEventHandlerGUID L"{c79a420c-efd9-4058-9295-3e8b4bcab645}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2HistoryChangedEventHandler;
#define IID_ICoreWebView2ContentLoadingEventHandlerGUID L"{364471e7-f2be-4910-bdba-d72077d51c4b}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContentLoadingEventHandler;
#define IID_ICoreWebView2NavigationCompletedEventHandlerGUID L"{d33a35bf-1c49-4f98-93ab-006e0533fe1c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NavigationCompletedEventHandler;
#define IID_ICoreWebView2WebResourceRequestedEventHandlerGUID L"{ab00b74c-15f1-4646-80e8-e76341d25d71}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2WebResourceRequestedEventHandler;
#define IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandlerGUID L"{5c4889f0-5ef6-4c5a-952c-d8f1b92d0574}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandler;
#define IID_ICoreWebView2ExecuteScriptCompletedHandlerGUID L"{49511172-cc67-4bca-9923-137112f4c4cc}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ExecuteScriptCompletedHandler;
#define IID_ICoreWebView2AcceleratorKeyPressedEventHandlerGUID L"{b29c7e28-fa79-41a8-8e44-65811c76dcb2}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2AcceleratorKeyPressedEventHandler;
#define IID_ICoreWebView2NewWindowRequestedEventHandlerGUID L"{d4c185fe-c81c-4989-97af-2d3fa7ab5651}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2NewWindowRequestedEventHandler;
#define IID_ICoreWebView2DOMContentLoadedEventHandlerGUID L"{4BAC7E9C-199E-49ED-87ED-249303ACF019}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2DOMContentLoadedEventHandler;
#define IID_ICoreWebView2FocusChangedEventHandlerGUID L"{05ea24bd-6452-4926-9014-4b82b498135d}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2FocusChangedEventHandler;
#define IID_ICoreWebView2CapturePreviewCompletedHandlerGUID L"{697e05e9-3d8f-45fa-96f4-8ffe1ededaf5}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CapturePreviewCompletedHandler;
#define IID_ICoreWebView2CursorChangedEventHandlerGUID L"{9da43ccc-26e1-4dad-b56c-d8961c94c571}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CursorChangedEventHandler;
#define IID_ICoreWebView2PermissionRequestedEventHandlerGUID L"{15e1c6a3-c72a-4df3-91d7-d097fbec6bfd}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PermissionRequestedEventHandler;
#define IID_ICoreWebView2GUID L"{76eceacb-0462-4d94-ac83-423a6793775e}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2;
#define IID_ICoreWebView2ControllerGUID L"{4d00c0d1-9434-4eb6-8078-8697a560334f}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Controller;
#define IID_ICoreWebView2Controller2GUID L"{c979903e-d4ca-4228-92eb-47ee3fa96eab}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Controller2;
#define IID_ICoreWebView2Controller3GUID L"{f9614724-5d2b-41dc-aef7-73d62b51543b}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Controller3;
#define IID_ICoreWebView2Controller4GUID L"{97d418d5-a426-4e49-a151-e1a10f327d9e}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Controller4;
#define IID_ICoreWebView2CompositionControllerGUID L"{3df9b733-b9ae-4a15-86b4-eb9ee9826469}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CompositionController;
#define IID_ICoreWebView2CompositionController2GUID L"{0b6a3d24-49cb-4806-ba20-b5e0734a7b26}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CompositionController2;
#define IID_ICoreWebView2CompositionController3GUID L"{9570570e-4d76-4361-9ee1-f04d0dbdfb1e}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CompositionController3;
#define IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandlerGUID L"{6c4819f3-c9b7-4260-8127-c9f5bde7f68c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler;
#define IID_ICoreWebView2EnvironmentGUID L"{b96d755e-0319-4e92-a296-23436f46a1fc}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment;
#define IID_ICoreWebView2Environment2GUID L"{41F3632B-5EF4-404F-AD82-2D606C5A9A21}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment2;
#define IID_ICoreWebView2Environment3GUID L"{80a22ae3-be7c-4ce2-afe1-5a50056cdeeb}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment3;
#define IID_ICoreWebView2Environment4GUID L"{20944379-6dcf-41d6-a0a0-abc0fc50de0d}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment4;
#define IID_ICoreWebView2Environment5GUID L"{319e423d-e0d7-4b8d-9254-ae9475de9b17}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment5;
#define IID_ICoreWebView2Environment6GUID L"{e59ee362-acbd-4857-9a8e-d3644d9459a9}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment6;
#define IID_ICoreWebView2Environment7GUID L"{43C22296-3BBD-43A4-9C00-5C0DF6DD29A2}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment7;
#define IID_ICoreWebView2Environment8GUID L"{D6EB91DD-C3D2-45E5-BD29-6DC2BC4DE9CF}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment8;
#define IID_ICoreWebView2Environment9GUID L"{f06f41bf-4b5a-49d8-b9f6-fa16cd29f274}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2Environment9;
#define IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandlerGUID L"{02fab84b-1428-4fb7-ad45-1b2e64736184}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler;
#define IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandlerGUID L"{4e8a3389-c9d8-4bd2-b6b5-124fee6cc14d}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler;
#define IID_ICoreWebView2_2GUID L"{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_2;
#define IID_ICoreWebView2_3GUID L"{A0D6DF20-3B92-416D-AA0C-437A9C727857}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_3;
#define IID_ICoreWebView2_4GUID L"{20d02d59-6df2-42dc-bd06-f98a694b1302}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_4;
#define IID_ICoreWebView2_5GUID L"{bedb11b8-d63c-11eb-b8bc-0242ac130003}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_5;
#define IID_ICoreWebView2_6GUID L"{499aadac-d92c-4589-8a75-111bfc167795}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_6;
#define IID_ICoreWebView2_7GUID L"{79c24d83-09a3-45ae-9418-487f32a58740}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_7;
#define IID_ICoreWebView2_8GUID L"{E9632730-6E1E-43AB-B7B8-7B2C9E62E094}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_8;
#define IID_ICoreWebView2_9GUID L"{4d7b2eab-9fdc-468d-b998-a9260b5ed651}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_9;
#define IID_ICoreWebView2_10GUID L"{b1690564-6f5a-4983-8e48-31d1143fecdb}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_10;
#define IID_ICoreWebView2_11GUID L"{0be78e56-c193-4051-b943-23b460c08bdb}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_11;
#define IID_ICoreWebView2_12GUID L"{35D69927-BCFA-4566-9349-6B3E0D154CAC}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_12;
#define IID_ICoreWebView2_13GUID L"{F75F09A8-667E-4983-88D6-C8773F315E84}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_13;
#define IID_ICoreWebView2_14GUID L"{6DAA4F10-4A90-4753-8898-77C5DF534165}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_14;
#define IID_ICoreWebView2_15GUID L"{517B2D1D-7DAE-4A66-A4F4-10352FFB9518}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_15;
#define IID_ICoreWebView2_16GUID L"{0EB34DC9-9F91-41E1-8639-95CD5943906B}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2_16;
#define IID_ICoreWebView2ContextMenuItemCollectionGUID L"{f562a2f5-c415-45cf-b909-d4b7c1e276d3}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContextMenuItemCollection;
#define IID_ICoreWebView2ContextMenuItemGUID L"{7aed49e3-a93f-497a-811c-749c6b6b6c65}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContextMenuItem;
#define IID_ICoreWebView2ContextMenuTargetGUID L"{b8611d99-eed6-4f3f-902c-a198502ad472}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContextMenuTarget;
#define IID_ICoreWebView2ContextMenuRequestedEventHandlerGUID L"{04d3fe1d-ab87-42fb-a898-da241d35b63c}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContextMenuRequestedEventHandler;
#define IID_ICoreWebView2ContextMenuRequestedEventArgsGUID L"{a1d309ee-c03f-11eb-8529-0242ac130003}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2ContextMenuRequestedEventArgs;
#define IID_ICoreWebView2CustomItemSelectedEventHandlerGUID L"{49e1d0bc-fe9e-4481-b7c2-32324aa21998}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2CustomItemSelectedEventHandler;
#define IID_ICoreWebView2PrintSettingsGUID L"{377f3721-c74e-48ca-8db1-df68e51d60e2}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PrintSettings;
#define IID_ICoreWebView2PrintToPdfCompletedHandlerGUID L"{ccf1ef04-fd8e-4d5f-b2de-0983e41b8c36}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PrintToPdfCompletedHandler;
#define IID_ICoreWebView2PrintCompletedHandlerGUID L"{8FD80075-ED08-42DB-8570-F5D14977461E}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PrintCompletedHandler;
#define IID_ICoreWebView2PrintToPdfStreamCompletedHandlerGUID L"{4C9F8229-8F93-444F-A711-2C0DFD6359D5}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2PrintToPdfStreamCompletedHandler;
#define IID_ICoreWebView2DownloadStartingEventHandlerGUID L"{efedc989-c396-41ca-83f7-07f845a55724}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2DownloadStartingEventHandler;
#define IID_ICoreWebView2DownloadStartingEventArgsGUID L"{e99bbe21-43e9-4544-a732-282764eafa60}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2DownloadStartingEventArgs;
#define IID_ICoreWebView2DownloadOperationGUID L"{3d6b6cf2-afe1-44c7-a995-c65117714336}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2DownloadOperation;
#define IID_ICoreWebView2StateChangedEventHandlerGUID L"{81336594-7ede-4ba9-bf71-acf0a95b58dd}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2StateChangedEventHandler;
#define IID_ICoreWebView2BytesReceivedChangedEventHandlerGUID L"{828e8ab6-d94c-4264-9cef-5217170d6251}"
extern DELPHI_PACKAGE GUID IID_ICoreWebView2BytesReceivedChangedEventHandler;
#define CoreWebView2DLL L"WebView2Loader_x86.dll"
extern DELPHI_PACKAGE bool EdgeLoaded;
extern DELPHI_PACKAGE NativeUInt EdgeHandle;
extern DELPHI_PACKAGE System::UnicodeString EdgeVersion;
extern DELPHI_PACKAGE System::UnicodeString EdgeDLLPath;
extern DELPHI_PACKAGE System::UnicodeString EdgeCustomPath;
extern DELPHI_PACKAGE bool EdgeSilentErrors;
extern DELPHI_PACKAGE HRESULT __stdcall (*CreateCoreWebView2Environment)(_di_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler environment_created_handler);
extern DELPHI_PACKAGE HRESULT __stdcall (*CreateCoreWebView2EnvironmentWithOptions)(System::WideChar * browserExecutableFolder, System::WideChar * userDataFolder, _di_ICoreWebView2EnvironmentOptions environmentOptions, _di_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler environment_created_handler);
extern DELPHI_PACKAGE HRESULT __stdcall (*GetAvailableCoreWebView2BrowserVersionString)(System::WideChar * browserExecutableFolder, System::PPWideChar versioninfo);
#define EErrorMessage L"Could not initialize Edge Chromium! Please check Edge Chro"\
	L"mium installation and verify correct version number."
#define EErrorMessageNoDLL L"Could not initialize Edge Chromium! Please check if WebVie"\
	L"w2Loader_x86.dll is correctly distributed and accessible."
extern DELPHI_PACKAGE void __fastcall RegisterWebBrowserService(void);
extern DELPHI_PACKAGE void __fastcall UnRegisterWebBrowserService(void);
extern DELPHI_PACKAGE void __fastcall InitializeEdge(void);
extern DELPHI_PACKAGE void __fastcall UninitializeEdge(void);
}	/* namespace Win */
}	/* namespace Tmsfncwebbrowser */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCWEBBROWSER_WIN)
using namespace Vcl::Tmsfncwebbrowser::Win;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCWEBBROWSER)
using namespace Vcl::Tmsfncwebbrowser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_Tmsfncwebbrowser_WinHPP
