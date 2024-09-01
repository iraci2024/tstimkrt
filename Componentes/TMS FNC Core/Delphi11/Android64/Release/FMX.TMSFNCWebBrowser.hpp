// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWebBrowser.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncwebbrowserHPP
#define Fmx_TmsfncwebbrowserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <System.Messaging.hpp>
#include <FMX.Menus.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <System.Generics.Collections.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Platform.Android.hpp>
#include <Androidapi.JNI.Embarcadero.hpp>
#include <Androidapi.JNI.App.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#include <Androidapi.JNI.JavaTypes.hpp>
#include <Androidapi.JNI.Widget.hpp>
#include <FMX.Helpers.Android.hpp>
#include <Androidapi.JNIBridge.hpp>
#include <Androidapi.Helpers.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwebbrowser
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE TTMSFNCWebBrowserJavaScriptCompleteEvent;
typedef System::DelphiInterface<TTMSFNCWebBrowserJavaScriptCompleteEvent> _di_TTMSFNCWebBrowserJavaScriptCompleteEvent;
struct TTMSFNCWebBrowserCookie;
struct TTMSFNCWebBrowserTargetItem;
class DELPHICLASS TTMSFNCWebBrowserContextMenuItemList;
class DELPHICLASS TTMSFNCWebBrowserContextMenuItem;
class DELPHICLASS TTMSFNCWebBrowserSystemContextMenuItem;
class DELPHICLASS TTMSFNCWebBrowserCustomContextMenuItem;
struct TTMSFNCWebBrowserPrintSettings;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserBridge;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserBridge> _di_ITMSFNCCustomWebBrowserBridge;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowser;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowser> _di_ITMSFNCCustomWebBrowser;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserEx;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserEx> _di_ITMSFNCCustomWebBrowserEx;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserSettings;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserSettings> _di_ITMSFNCCustomWebBrowserSettings;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserCookies;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserCookies> _di_ITMSFNCCustomWebBrowserCookies;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserContextMenu;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserContextMenu> _di_ITMSFNCCustomWebBrowserContextMenu;
__interface DELPHIINTERFACE ITMSFNCCustomWebBrowserPrint;
typedef System::DelphiInterface<ITMSFNCCustomWebBrowserPrint> _di_ITMSFNCCustomWebBrowserPrint;
__interface DELPHIINTERFACE ITMSFNCWebBrowserService;
typedef System::DelphiInterface<ITMSFNCWebBrowserService> _di_ITMSFNCWebBrowserService;
struct TTMSFNCCustomWebBrowserNavigateCompleteParams;
struct TTMSFNCCustomWebBrowserBeforeNavigateParams;
class DELPHICLASS TTMSFNCWebBrowserDocumentReadyStateThread;
class DELPHICLASS TTMSFNCScript;
class DELPHICLASS TTMSFNCScriptList;
class DELPHICLASS TTMSFNCCustomWebBrowserSettings;
class DELPHICLASS TTMSFNCWebBrowserSettings;
class DELPHICLASS TTMSFNCCustomWebBrowser;
class DELPHICLASS TTMSFNCWebBrowserList;
class DELPHICLASS TTMSFNCWebBrowserFactoryService;
class DELPHICLASS TTMSFNCWebBrowser;
class DELPHICLASS TTMSFNCWebBrowserPopupForm;
class DELPHICLASS TTMSFNCWebBrowserPopupButtonEventHandler;
class DELPHICLASS TTMSFNCWebBrowserPopup;
class DELPHICLASS TTMSFNCWebBrowserPlatformServicesService;
class DELPHICLASS TTMSFNCWebBrowserPlatformServicesList;
class DELPHICLASS TTMSFNCWebBrowserPlatformServices;
//-- type declarations -------------------------------------------------------
__interface TTMSFNCWebBrowserJavaScriptCompleteEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(const System::UnicodeString AValue) = 0 ;
};

enum DECLSPEC_DENUM TTMSFNCWebBrowserContextMenuType : unsigned char { mtPage, mtImage, mtSelectedText, mtAudio, mtVideo, mtSubMenu };

enum DECLSPEC_DENUM TTMSFNCWebBrowserContextMenuItemKind : unsigned char { ikCommand, ikCheckBox, ikRadioButton, ikSeperator, ikSubMenu };

enum DECLSPEC_DENUM TTMSFNCWebBrowserPrintOrientation : unsigned char { poPortrait, poLandscape };

enum DECLSPEC_DENUM TTMSFNCWebBrowserSameSiteType : unsigned char { sstNone, sstLax, sstSameSite };

struct DECLSPEC_DRECORD TTMSFNCWebBrowserCookie
{
public:
	System::UnicodeString Path;
	System::UnicodeString Name;
	System::TDateTime Expires;
	System::UnicodeString Domain;
	bool Secure;
	bool HTTPOnly;
	System::UnicodeString Value;
	TTMSFNCWebBrowserSameSiteType SameSite;
	bool Session;
};


struct DECLSPEC_DRECORD TTMSFNCWebBrowserTargetItem
{
public:
	TTMSFNCWebBrowserContextMenuType Kind;
	System::UnicodeString URI;
	System::UnicodeString SelectionText;
	System::UnicodeString LinkText;
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserContextMenuItemList : public System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWebBrowserContextMenuItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(System::Generics::Collections::TEnumerable__1<TTMSFNCWebBrowserContextMenuItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserContextMenuItemList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWebBrowserContextMenuItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(System::Generics::Collections::TEnumerable__1<TTMSFNCWebBrowserContextMenuItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserContextMenuItem>.Create */ inline __fastcall TTMSFNCWebBrowserContextMenuItemList(TTMSFNCWebBrowserContextMenuItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserContextMenuItem : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCWebBrowserContextMenuItem* FParentItem;
	System::UnicodeString FName;
	System::UnicodeString FText;
	TTMSFNCWebBrowserContextMenuItemList* FChildren;
	int FCommandId;
	bool FEnabled;
	System::UnicodeString FShortcutKeyDescription;
	TTMSFNCWebBrowserContextMenuItemKind FKind;
	bool FChecked;
	int FOriginalIndex;
	System::TObject* FInternalObject;
	void *FDataPointer;
	bool FDataBoolean;
	System::UnicodeString FDataString;
	System::TObject* FDataObject;
	NativeInt FDataInteger;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FIcon;
	System::TObject* FEventHandlerObject;
	void __fastcall SetShortcutKeyDescription(const System::UnicodeString Value);
	
protected:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property TTMSFNCWebBrowserContextMenuItemList* Children = {read=FChildren, write=FChildren};
	__property int OriginalIndex = {read=FOriginalIndex, write=FOriginalIndex, nodefault};
	__property int CommandId = {read=FCommandId, write=FCommandId, nodefault};
	__property System::UnicodeString ShortcutKeyDescription = {read=FShortcutKeyDescription, write=SetShortcutKeyDescription};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Icon = {read=FIcon, write=FIcon};
	__property TTMSFNCWebBrowserContextMenuItemKind Kind = {read=FKind, write=FKind, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property bool Checked = {read=FChecked, write=FChecked, nodefault};
	__property System::TObject* InternalObject = {read=FInternalObject, write=FInternalObject};
	__property System::TObject* EventHandlerObject = {read=FEventHandlerObject, write=FEventHandlerObject};
	__property TTMSFNCWebBrowserContextMenuItem* ParentItem = {read=FParentItem, write=FParentItem};
	
public:
	__fastcall TTMSFNCWebBrowserContextMenuItem();
	__fastcall virtual ~TTMSFNCWebBrowserContextMenuItem();
	TTMSFNCWebBrowserCustomContextMenuItem* __fastcall AsCustom();
	TTMSFNCWebBrowserSystemContextMenuItem* __fastcall AsSystem();
	__property void * DataPointer = {read=FDataPointer, write=FDataPointer};
	__property bool DataBoolean = {read=FDataBoolean, write=FDataBoolean, nodefault};
	__property System::TObject* DataObject = {read=FDataObject, write=FDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=FDataString};
	__property NativeInt DataInteger = {read=FDataInteger, write=FDataInteger};
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserSystemContextMenuItem : public TTMSFNCWebBrowserContextMenuItem
{
	typedef TTMSFNCWebBrowserContextMenuItem inherited;
	
private:
	System::UnicodeString __fastcall GetName();
	System::UnicodeString __fastcall GetText();
	System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>* __fastcall GetChildren();
	TTMSFNCWebBrowserContextMenuItemKind __fastcall GetKind();
	bool __fastcall GetChecked();
	bool __fastcall GetEnabled();
	Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetIcon();
	TTMSFNCWebBrowserContextMenuItem* __fastcall GetParentItem();
	
public:
	__property System::UnicodeString Name = {read=GetName};
	__property System::UnicodeString Text = {read=GetText};
	__property System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>* Children = {read=GetChildren};
	__property TTMSFNCWebBrowserContextMenuItemKind Kind = {read=GetKind, nodefault};
	__property bool Enabled = {read=GetEnabled, nodefault};
	__property bool Checked = {read=GetChecked, nodefault};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Icon = {read=GetIcon};
	__property TTMSFNCWebBrowserContextMenuItem* ParentItem = {read=GetParentItem};
public:
	/* TTMSFNCWebBrowserContextMenuItem.Create */ inline __fastcall TTMSFNCWebBrowserSystemContextMenuItem() : TTMSFNCWebBrowserContextMenuItem() { }
	/* TTMSFNCWebBrowserContextMenuItem.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserSystemContextMenuItem() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserCustomContextMenuItem : public TTMSFNCWebBrowserContextMenuItem
{
	typedef TTMSFNCWebBrowserContextMenuItem inherited;
	
private:
	TTMSFNCWebBrowserContextMenuItem* __fastcall GetParentItem();
	
public:
	__property Name = {default=0};
	__property Text = {default=0};
	__property Children;
	__property Kind;
	__property Enabled;
	__property Checked;
	__property Icon;
	__property TTMSFNCWebBrowserContextMenuItem* ParentItem = {read=GetParentItem};
public:
	/* TTMSFNCWebBrowserContextMenuItem.Create */ inline __fastcall TTMSFNCWebBrowserCustomContextMenuItem() : TTMSFNCWebBrowserContextMenuItem() { }
	/* TTMSFNCWebBrowserContextMenuItem.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserCustomContextMenuItem() { }
	
};


struct DECLSPEC_DRECORD TTMSFNCWebBrowserPrintSettings
{
public:
	TTMSFNCWebBrowserPrintOrientation Orientation;
	double ScaleFactor;
	int PageWidth;
	int PageHeight;
	int MarginLeft;
	int MarginTop;
	int MarginRight;
	int MarginBottom;
	bool PrintBackgrounds;
	bool PrintSelectionOnly;
	bool PrintHeaderAndFooter;
	System::UnicodeString HeaderTitle;
	System::UnicodeString FooterURI;
};


typedef void __fastcall (__closure *TTMSFNCWebBrowserOnGetContextMenu)(System::TObject* Sender, const TTMSFNCWebBrowserTargetItem &ATarget, System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>* AContextMenu);

typedef void __fastcall (__closure *TTMSFNCWebBrowserOnGetPopupMenuForContextMenu)(System::TObject* Sender, const TTMSFNCWebBrowserTargetItem &ATarget, Fmx::Menus::TPopupMenu* &APopupMenu);

typedef void __fastcall (__closure *TTMSFNCWebBrowserOnCustomContextMenuItemSelected)(System::TObject* Sender, TTMSFNCWebBrowserCustomContextMenuItem* ASelectedItem);

typedef void __fastcall (__closure *TTMSFNCWebBrowserOnGetCookies)(System::TObject* Sender, TTMSFNCWebBrowserCookie *ACookies, const int ACookies_High);

typedef void __fastcall (__closure *TTMSFNCWebBrowserOnGetPrintPDFStream)(System::TObject* Sender, System::Classes::TMemoryStream* AStream);

__interface  INTERFACE_UUID("{AC2934EC-9397-4A99-8E0A-1EF58803C8EA}") ITMSFNCCustomWebBrowserBridge  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetObjectMessage() = 0 ;
	virtual void __fastcall SetObjectMessage(const System::UnicodeString AValue) = 0 ;
	__property System::UnicodeString ObjectMessage = {read=GetObjectMessage, write=SetObjectMessage};
};

__interface  INTERFACE_UUID("{F74562D0-56C7-4ED2-B01B-8C9C16DD9361}") ITMSFNCCustomWebBrowser  : public System::IInterface 
{
	virtual void __fastcall SetFocus() = 0 ;
	virtual void __fastcall SetUserAgent(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetURL(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetExternalBrowser(const bool AValue) = 0 ;
	virtual void __fastcall SetEnableContextMenu(const bool AValue) = 0 ;
	virtual void __fastcall SetEnableShowDebugConsole(const bool AValue) = 0 ;
	virtual void __fastcall SetEnableAcceleratorKeys(const bool AValue) = 0 ;
	virtual void __fastcall SetCacheFolderName(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetAutoClearCache(const bool Value) = 0 ;
	virtual void __fastcall SetCacheFolder(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall Navigate(const System::UnicodeString AURL) = 0 /* overload */;
	virtual void __fastcall Navigate() = 0 /* overload */;
	virtual void __fastcall LoadHTML(System::UnicodeString AHTML) = 0 ;
	virtual void __fastcall LoadFile(System::UnicodeString AFile) = 0 ;
	virtual void __fastcall GoForward() = 0 ;
	virtual void __fastcall GoBack() = 0 ;
	virtual void __fastcall Reload() = 0 ;
	virtual void __fastcall Close() = 0 ;
	virtual void __fastcall StopLoading() = 0 ;
	virtual void __fastcall AddBridge(System::UnicodeString ABridgeName, System::TObject* ABridgeObject) = 0 ;
	virtual void __fastcall RemoveBridge(System::UnicodeString ABridgeName) = 0 ;
	virtual void __fastcall UpdateBounds() = 0 ;
	virtual void __fastcall CaptureScreenShot() = 0 ;
	virtual void __fastcall UpdateVisible() = 0 ;
	virtual void __fastcall UpdateEnabled() = 0 ;
	virtual void __fastcall BeforeChangeParent() = 0 ;
	virtual void __fastcall Initialize() = 0 ;
	virtual void __fastcall DeInitialize() = 0 ;
	virtual void __fastcall ClearCache() = 0 ;
	virtual void __fastcall ShowDebugConsole() = 0 ;
	virtual System::UnicodeString __fastcall GetUserAgent() = 0 ;
	virtual System::UnicodeString __fastcall GetURL() = 0 ;
	virtual bool __fastcall GetExternalBrowser() = 0 ;
	virtual bool __fastcall GetEnableContextMenu() = 0 ;
	virtual bool __fastcall GetEnableShowDebugConsole() = 0 ;
	virtual bool __fastcall GetEnableAcceleratorKeys() = 0 ;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, _di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent, System::Classes::TNotifyEvent ACallback) = 0 ;
	virtual void * __fastcall NativeEnvironment() = 0 ;
	virtual void * __fastcall NativeBrowser() = 0 ;
	virtual System::_di_IInterface __fastcall GetBrowserInstance() = 0 ;
	virtual bool __fastcall CanGoBack() = 0 ;
	virtual bool __fastcall CanGoForward() = 0 ;
	virtual void * __fastcall NativeDialog() = 0 ;
	virtual bool __fastcall IsFMXBrowser() = 0 ;
	virtual System::UnicodeString __fastcall GetCacheFolderName() = 0 ;
	virtual bool __fastcall GetAutoClearCache() = 0 ;
	virtual System::UnicodeString __fastcall GetCacheFolder() = 0 ;
	__property System::UnicodeString UserAgent = {read=GetUserAgent, write=SetUserAgent};
	__property System::UnicodeString CacheFolderName = {read=GetCacheFolderName, write=SetCacheFolderName};
	__property System::UnicodeString CacheFolder = {read=GetCacheFolder, write=SetCacheFolder};
	__property bool AutoClearCache = {read=GetAutoClearCache, write=SetAutoClearCache};
	__property System::UnicodeString URL = {read=GetURL, write=SetURL};
	__property bool ExternalBrowser = {read=GetExternalBrowser, write=SetExternalBrowser};
	__property bool EnableContextMenu = {read=GetEnableContextMenu, write=SetEnableContextMenu};
	__property bool EnableShowDebugConsole = {read=GetEnableShowDebugConsole, write=SetEnableShowDebugConsole};
	__property bool EnableAcceleratorKeys = {read=GetEnableAcceleratorKeys, write=SetEnableAcceleratorKeys};
};

__interface  INTERFACE_UUID("{6B5D75C1-B5EC-463D-A602-1FFB97C8668C}") ITMSFNCCustomWebBrowserEx  : public System::IInterface 
{
	virtual void __fastcall OpenTaskManagerWindow() = 0 ;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::UnicodeString ABody, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0)) = 0 /* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::Classes::TStream* ABodyStream, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0)) = 0 /* overload */;
};

__interface  INTERFACE_UUID("{25142510-A807-4635-BAE7-CB261D00137E}") ITMSFNCCustomWebBrowserSettings  : public System::IInterface 
{
	virtual bool __fastcall GetEnableScript() = 0 ;
	virtual void __fastcall SetEnableScript(const bool Value) = 0 ;
	__property bool EnableScript = {read=GetEnableScript, write=SetEnableScript};
};

__interface  INTERFACE_UUID("{A50ABF08-0A6F-4877-AC92-FC834CF36AE6}") ITMSFNCCustomWebBrowserCookies  : public System::IInterface 
{
	virtual void __fastcall GetCookies(System::UnicodeString AURI) = 0 ;
	virtual void __fastcall AddCookie(const TTMSFNCWebBrowserCookie &ACookie) = 0 ;
	virtual void __fastcall DeleteAllCookies() = 0 ;
	virtual void __fastcall DeleteCookie(System::UnicodeString AName, System::UnicodeString ADomain, System::UnicodeString APath) = 0 ;
};

__interface  INTERFACE_UUID("{04BD0560-104B-4D3B-8CB1-45628D16CB0D}") ITMSFNCCustomWebBrowserContextMenu  : public System::IInterface 
{
	
};

__interface  INTERFACE_UUID("{56EFC9E8-CD92-4FAC-B79C-084BF3DB0FBD}") ITMSFNCCustomWebBrowserPrint  : public System::IInterface 
{
	virtual void __fastcall ShowPrintUI() = 0 ;
	virtual void __fastcall Print() = 0 /* overload */;
	virtual void __fastcall Print(const TTMSFNCWebBrowserPrintSettings &APrintSettings) = 0 /* overload */;
	virtual void __fastcall PrintToPDFStream(const TTMSFNCWebBrowserPrintSettings &APrintSettings) = 0 /* overload */;
	virtual void __fastcall PrintToPDFStream() = 0 /* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName, const TTMSFNCWebBrowserPrintSettings &APrintSettings) = 0 /* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName) = 0 /* overload */;
};

__interface  INTERFACE_UUID("{4B7A5FE1-A889-47C6-B40F-A611BB6266E6}") ITMSFNCWebBrowserService  : public System::IInterface 
{
	virtual _di_ITMSFNCCustomWebBrowser __fastcall CreateWebBrowser(TTMSFNCCustomWebBrowser* const AValue) = 0 ;
	virtual void __fastcall DeleteCookies() = 0 ;
	virtual void __fastcall DestroyWebBrowser(const _di_ITMSFNCCustomWebBrowser AValue) = 0 ;
};

struct DECLSPEC_DRECORD TTMSFNCCustomWebBrowserNavigateCompleteParams
{
public:
	System::UnicodeString URL;
	bool Success;
	int ErrorCode;
};


struct DECLSPEC_DRECORD TTMSFNCCustomWebBrowserBeforeNavigateParams
{
public:
	System::UnicodeString URL;
	bool Cancel;
};


typedef void __fastcall (__closure *TTMSFNCCustomWebBrowserNavigateComplete)(System::TObject* Sender, TTMSFNCCustomWebBrowserNavigateCompleteParams &Params);

typedef void __fastcall (__closure *TTMSFNCCustomWebBrowserBeforeNavigate)(System::TObject* Sender, TTMSFNCCustomWebBrowserBeforeNavigateParams &Params);

typedef void __fastcall (__closure *TTMSFNCCustomWebBrowserCaptureScreenShot)(System::TObject* Sender, Fmx::Tmsfnctypes::TTMSFNCBitmap* AScreenShot);

typedef void __fastcall (__closure *TTMSFNCCustomWebBrowserOnExecuteSuccessful)(System::TObject* Sender, bool ASuccessful);

class PASCALIMPLEMENTATION TTMSFNCWebBrowserDocumentReadyStateThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TTMSFNCCustomWebBrowser* FWebBrowser;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TTMSFNCWebBrowserDocumentReadyStateThread(TTMSFNCCustomWebBrowser* AWebBrowser);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserDocumentReadyStateThread() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCScript : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FScript;
	_di_TTMSFNCWebBrowserJavaScriptCompleteEvent FCompleteEvent;
	
public:
	__fastcall TTMSFNCScript(System::UnicodeString AScript, _di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCScript() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCScriptList : public System::Generics::Collections::TObjectList__1<TTMSFNCScript*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCScript*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCScript*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(System::Generics::Collections::TEnumerable__1<TTMSFNCScript*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Destroy */ inline __fastcall virtual ~TTMSFNCScriptList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCScript*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(System::Generics::Collections::TEnumerable__1<TTMSFNCScript*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCScript>.Create */ inline __fastcall TTMSFNCScriptList(TTMSFNCScript* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCScript*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomWebBrowserSettings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCCustomWebBrowser* FOwner;
	bool FEnableScript;
	bool FEnableAcceleratorKeys;
	bool FEnableContextMenu;
	bool FEnableShowDebugConsole;
	bool FUsePopupMenuAsContextMenu;
	bool __fastcall GetEnableAcceleratorKeys();
	bool __fastcall GetEnableContextMenu();
	bool __fastcall GetEnableScript();
	bool __fastcall GetEnableShowDebugConsole();
	void __fastcall SetEnableAcceleratorKeys(const bool Value);
	void __fastcall SetEnableContextMenu(const bool Value);
	void __fastcall SetEnableShowDebugConsole(const bool Value);
	void __fastcall SetEnableScript(const bool Value);
	bool __fastcall GetUsePopupMenuAsContextMenu();
	void __fastcall SetUsePopupMenuAsContextMenu(const bool Value);
	
protected:
	void __fastcall ApplySettings();
	__property bool EnableScript = {read=GetEnableScript, write=SetEnableScript, nodefault};
	__property bool EnableContextMenu = {read=GetEnableContextMenu, write=SetEnableContextMenu, nodefault};
	__property bool EnableShowDebugConsole = {read=GetEnableShowDebugConsole, write=SetEnableShowDebugConsole, nodefault};
	__property bool EnableAcceleratorKeys = {read=GetEnableAcceleratorKeys, write=SetEnableAcceleratorKeys, nodefault};
	__property bool UsePopupMenuAsContextMenu = {read=GetUsePopupMenuAsContextMenu, write=SetUsePopupMenuAsContextMenu, nodefault};
	
public:
	__fastcall TTMSFNCCustomWebBrowserSettings(TTMSFNCCustomWebBrowser* AOwner);
	__fastcall virtual ~TTMSFNCCustomWebBrowserSettings();
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserSettings : public TTMSFNCCustomWebBrowserSettings
{
	typedef TTMSFNCCustomWebBrowserSettings inherited;
	
__published:
	__property EnableContextMenu;
	__property EnableShowDebugConsole;
	__property EnableAcceleratorKeys;
	__property UsePopupMenuAsContextMenu;
public:
	/* TTMSFNCCustomWebBrowserSettings.Create */ inline __fastcall TTMSFNCWebBrowserSettings(TTMSFNCCustomWebBrowser* AOwner) : TTMSFNCCustomWebBrowserSettings(AOwner) { }
	/* TTMSFNCCustomWebBrowserSettings.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserSettings() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomWebBrowser : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FDesigntimeEnabled;
	bool FCanDestroyDispatch;
	System::UnicodeString FSyncValue;
	bool FSyncValueExecuted;
	bool FThreadDone;
	bool FReady;
	bool FInitialized;
	TTMSFNCScriptList* FScriptList;
	TTMSFNCWebBrowserDocumentReadyStateThread* FDocumentReadyStateThread;
	_di_ITMSFNCCustomWebBrowser FWebBrowser;
	System::UnicodeString FURL;
	bool FExternalBrowser;
	bool FEnableShowDebugConsole;
	bool FEnableAcceleratorKeys;
	bool FEnableContextMenu;
	TTMSFNCCustomWebBrowserNavigateComplete FOnNavigateComplete;
	TTMSFNCCustomWebBrowserBeforeNavigate FOnBeforeNavigate;
	System::Classes::TNotifyEvent FOnHardwareButtonClicked;
	System::Classes::TNotifyEvent FOnInitialized;
	System::Classes::TNotifyEvent FOnCloseForm;
	System::Classes::TNotifyEvent FOnDocumentComplete;
	TTMSFNCCustomWebBrowserCaptureScreenShot FOnCaptureScreenShot;
	TTMSFNCWebBrowserSettings* FSettings;
	TTMSFNCWebBrowserOnGetContextMenu FOnGetContextMenu;
	TTMSFNCWebBrowserOnGetCookies FOnGetCookies;
	TTMSFNCWebBrowserOnGetPrintPDFStream FOnGetPrintPDFStream;
	TTMSFNCCustomWebBrowserOnExecuteSuccessful FOnPrintedToPDF;
	TTMSFNCCustomWebBrowserOnExecuteSuccessful FOnPrinted;
	TTMSFNCWebBrowserOnCustomContextMenuItemSelected FOnCustomContextMenuItemSelected;
	TTMSFNCWebBrowserOnGetPopupMenuForContextMenu FOnGetPopupMenuForContextMenu;
	System::UnicodeString __fastcall GetURL();
	void __fastcall SetURL(const System::UnicodeString Value);
	bool __fastcall GetExternalBrowser();
	void __fastcall SetExternalBrowser(const bool Value);
	bool __fastcall GetEnableContextMenu();
	bool __fastcall GetEnableShowDebugConsole();
	void __fastcall SetEnableContextMenu(const bool Value);
	void __fastcall SetEnableShowDebugConsole(const bool Value);
	System::UnicodeString __fastcall GetCacheFolder();
	System::UnicodeString __fastcall GetCacheFolderName();
	bool __fastcall GetAutoClearCache();
	void __fastcall SetCacheFolder(const System::UnicodeString Value);
	void __fastcall SetCacheFolderName(const System::UnicodeString Value);
	void __fastcall SetAutoClearCache(const bool Value);
	void __fastcall SetDesigntimeEnabled(const bool Value);
	bool __fastcall GetEnableAcceleratorKeys();
	void __fastcall SetEnabledAcceleratorKeys(const bool Value);
	System::UnicodeString __fastcall GetUserAgent();
	void __fastcall SetUserAgent(const System::UnicodeString Value);
	void __fastcall DoExecuteJavaScriptSync(const System::UnicodeString AValue);
	void __fastcall SetSettings(TTMSFNCWebBrowserSettings* const Value);
	_di_ITMSFNCCustomWebBrowserSettings __fastcall GetSettingsI();
	void __fastcall FormHandleCreated(System::TObject* const Sender, System::Messaging::TMessageBase* const Msg);
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetVersion();
	bool __fastcall CanCreateBrowser();
	virtual bool __fastcall CanBeVisible();
	virtual bool __fastcall CheckIdentifier();
	__classmethod virtual void __fastcall DeleteCookies();
	virtual void __fastcall StartDocumentReadyStateThread();
	void __fastcall DoTerminate(System::TObject* Sender);
	void __fastcall StopDocumentReadyStateThread();
	void __fastcall DoScriptHandled(System::TObject* Sender);
	virtual void __fastcall DoEnter();
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall CreateClasses();
	virtual void __fastcall DoDocumentComplete();
	void __fastcall CheckApplicationInitialized();
	void __fastcall DoCheckReadyState(const System::UnicodeString AValue);
	void __fastcall DoCheckIdentifier(const System::UnicodeString AValue);
	virtual void __fastcall DoHardwareButtonClicked();
	virtual void __fastcall DoCloseForm();
	virtual void __fastcall DoCaptureScreenShot(Fmx::Tmsfnctypes::TTMSFNCBitmap* AScreenShot);
	virtual void __fastcall BeforeNavigate(TTMSFNCCustomWebBrowserBeforeNavigateParams &Params);
	virtual void __fastcall NavigateComplete(TTMSFNCCustomWebBrowserNavigateCompleteParams &Params);
	virtual void __fastcall Initialized();
	virtual void __fastcall AncestorVisibleChanged(const bool Visible);
	virtual void __fastcall SetParent(Fmx::Types::TFmxObject* const Value);
	virtual void __fastcall SetVisible(const bool Value);
	virtual void __fastcall SetEnabled(const bool Value);
	virtual void __fastcall DoKeyPressed(System::Word &Key);
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall Loaded();
	__property System::Classes::TNotifyEvent OnInitialized = {read=FOnInitialized, write=FOnInitialized};
	__property bool ExternalBrowser = {read=GetExternalBrowser, write=SetExternalBrowser, default=0};
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, _di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent = _di_TTMSFNCWebBrowserJavaScriptCompleteEvent(), bool AImmediate = false);
	virtual System::UnicodeString __fastcall ExecuteJavaScriptSync(System::UnicodeString AScript);
	virtual void __fastcall LoadHTML(System::UnicodeString AHTML);
	virtual void __fastcall LoadFile(System::UnicodeString AFile);
	virtual void __fastcall Initialize();
	virtual void __fastcall DeInitialize();
	virtual void __fastcall GoForward();
	virtual void __fastcall GoBack();
	virtual void __fastcall Reload();
	virtual void __fastcall StopLoading();
	virtual void __fastcall AddBridge(System::UnicodeString ABridgeName, System::TObject* ABridgeObject);
	virtual void __fastcall RemoveBridge(System::UnicodeString ABridgeName);
	virtual void __fastcall CaptureScreenShot();
	virtual void __fastcall ShowDebugConsole();
	__property System::UnicodeString URL = {read=GetURL, write=SetURL};
	__property _di_ITMSFNCCustomWebBrowserSettings SettingsI = {read=GetSettingsI};
	virtual System::UnicodeString __fastcall GetBridgeCommunicationLayer(System::UnicodeString ABridgeName);
	virtual void * __fastcall NativeEnvironment();
	virtual void * __fastcall NativeBrowser();
	virtual bool __fastcall IsFMXBrowser();
	virtual bool __fastcall CanGoBack();
	virtual bool __fastcall CanGoForward();
	virtual void * __fastcall NativeDialog();
	__property System::Classes::TNotifyEvent OnCloseForm = {read=FOnCloseForm, write=FOnCloseForm};
	__property TTMSFNCCustomWebBrowserBeforeNavigate OnBeforeNavigate = {read=FOnBeforeNavigate, write=FOnBeforeNavigate};
	__property TTMSFNCCustomWebBrowserNavigateComplete OnNavigateComplete = {read=FOnNavigateComplete, write=FOnNavigateComplete};
	__property System::Classes::TNotifyEvent OnHardwareButtonClicked = {read=FOnHardwareButtonClicked, write=FOnHardwareButtonClicked};
	__property System::Classes::TNotifyEvent OnDocumentComplete = {read=FOnDocumentComplete, write=FOnDocumentComplete};
	__property TTMSFNCCustomWebBrowserCaptureScreenShot OnCaptureScreenShot = {read=FOnCaptureScreenShot, write=FOnCaptureScreenShot};
	__property bool EnableContextMenu = {read=GetEnableContextMenu, write=SetEnableContextMenu, default=1};
	__property bool EnableShowDebugConsole = {read=GetEnableShowDebugConsole, write=SetEnableShowDebugConsole, default=1};
	__property bool EnableAcceleratorKeys = {read=GetEnableAcceleratorKeys, write=SetEnabledAcceleratorKeys, default=1};
	__property System::UnicodeString CacheFolder = {read=GetCacheFolder, write=SetCacheFolder};
	__property System::UnicodeString CacheFolderName = {read=GetCacheFolderName, write=SetCacheFolderName};
	__property bool AutoClearCache = {read=GetAutoClearCache, write=SetAutoClearCache, nodefault};
	__property System::UnicodeString UserAgent = {read=GetUserAgent, write=SetUserAgent};
	__property bool CanDestroyDispatch = {read=FCanDestroyDispatch, write=FCanDestroyDispatch, nodefault};
	__property bool DesigntimeEnabled = {read=FDesigntimeEnabled, write=SetDesigntimeEnabled, default=1};
	__property TTMSFNCWebBrowserSettings* Settings = {read=FSettings, write=SetSettings};
	__property TTMSFNCWebBrowserOnGetContextMenu OnGetContextMenu = {read=FOnGetContextMenu, write=FOnGetContextMenu};
	__property TTMSFNCWebBrowserOnGetPopupMenuForContextMenu OnGetPopupMenuForContextMenu = {read=FOnGetPopupMenuForContextMenu, write=FOnGetPopupMenuForContextMenu};
	__property TTMSFNCWebBrowserOnCustomContextMenuItemSelected OnCustomContextMenuItemSelected = {read=FOnCustomContextMenuItemSelected, write=FOnCustomContextMenuItemSelected};
	void __fastcall DoGetContextMenuItemEvent(const TTMSFNCWebBrowserTargetItem &ATarget, System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserContextMenuItem*>* AContextMenu);
	void __fastcall DoGetPopupMenuForContextMenu(const TTMSFNCWebBrowserTargetItem &ATarget, Fmx::Menus::TPopupMenu* &APopUpMenu);
	virtual void __fastcall DoCustomContextMenuItemSelected(TTMSFNCWebBrowserCustomContextMenuItem* ASelectedItem);
	virtual TTMSFNCWebBrowserPrintSettings __fastcall InitialPrintSettings();
	virtual void __fastcall DoGetPrintPDFStream(System::Classes::TMemoryStream* AStream);
	__property TTMSFNCWebBrowserOnGetPrintPDFStream OnGetPrintPDFStream = {read=FOnGetPrintPDFStream, write=FOnGetPrintPDFStream};
	__property TTMSFNCWebBrowserOnGetCookies OnGetCookies = {read=FOnGetCookies, write=FOnGetCookies};
	virtual void __fastcall DoGetCookies(TTMSFNCWebBrowserCookie *ACookies, const int ACookies_High);
	virtual void __fastcall DoPrintedToPDF(bool ASuccesfull);
	virtual void __fastcall DoPrinted(bool APrinterStatus);
	__property TTMSFNCCustomWebBrowserOnExecuteSuccessful OnPrintedToPDF = {read=FOnPrintedToPDF, write=FOnPrintedToPDF};
	__property TTMSFNCCustomWebBrowserOnExecuteSuccessful OnPrinted = {read=FOnPrinted, write=FOnPrinted};
	virtual void __fastcall OpenTaskManager();
	virtual void __fastcall GetCookies(System::UnicodeString AURI = System::UnicodeString());
	virtual void __fastcall AddCookie(const TTMSFNCWebBrowserCookie &ACookie);
	virtual void __fastcall DeleteAllCookies();
	virtual void __fastcall DeleteCookie(System::UnicodeString AName, System::UnicodeString ADomain, System::UnicodeString APath);
	virtual void __fastcall ShowPrintUI();
	virtual void __fastcall Print(const TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall Print()/* overload */;
	virtual void __fastcall PrintToPDFStream(const TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDFStream()/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName, const TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName)/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::UnicodeString ABody, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::Classes::TStream* ABodyStream, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual void __fastcall ClearCache();
	virtual void __fastcall Show();
	virtual void __fastcall Hide();
	virtual void __fastcall Move();
	virtual void __fastcall DoAbsoluteChanged();
	virtual bool __fastcall CanRecreate();
	virtual bool __fastcall CanLoadDefaultHTML();
	
public:
	__fastcall virtual TTMSFNCCustomWebBrowser(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomWebBrowser();
	virtual void __fastcall SetBounds(float X, float Y, float AWidth, float AHeight);
	virtual void __fastcall UpdateControlAfterResize();
	__property System::UnicodeString Version = {read=GetVersion};
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserList : public System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser>
{
	typedef System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_ITMSFNCCustomWebBrowser>.Create */ inline __fastcall TTMSFNCWebBrowserList()/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_ITMSFNCCustomWebBrowser>.Create */ inline __fastcall TTMSFNCWebBrowserList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<_di_ITMSFNCCustomWebBrowser> > AComparer)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_ITMSFNCCustomWebBrowser>.Create */ inline __fastcall TTMSFNCWebBrowserList(System::Generics::Collections::TEnumerable__1<_di_ITMSFNCCustomWebBrowser>* const Collection)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_ITMSFNCCustomWebBrowser>.Create */ inline __fastcall TTMSFNCWebBrowserList(const _di_ITMSFNCCustomWebBrowser *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomWebBrowser>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_ITMSFNCCustomWebBrowser>.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserList() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserFactoryService : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	TTMSFNCWebBrowserList* FWebBrowsers;
	virtual _di_ITMSFNCCustomWebBrowser __fastcall DoCreateWebBrowser(TTMSFNCCustomWebBrowser* const AValue) = 0 ;
	void __fastcall DoRemoveWebBrowser(const _di_ITMSFNCCustomWebBrowser AValue);
	
public:
	__fastcall TTMSFNCWebBrowserFactoryService();
	__fastcall virtual ~TTMSFNCWebBrowserFactoryService();
	_di_ITMSFNCCustomWebBrowser __fastcall CreateWebBrowser(TTMSFNCCustomWebBrowser* const AValue);
	virtual void __fastcall DeleteCookies() = 0 ;
	void __fastcall DestroyWebBrowser(const _di_ITMSFNCCustomWebBrowser AValue);
private:
	void *__ITMSFNCWebBrowserService;	// ITMSFNCWebBrowserService 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {4B7A5FE1-A889-47C6-B40F-A611BB6266E6}
	operator _di_ITMSFNCWebBrowserService()
	{
		_di_ITMSFNCWebBrowserService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCWebBrowserService*(void) { return (ITMSFNCWebBrowserService*)&__ITMSFNCWebBrowserService; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowser : public TTMSFNCCustomWebBrowser
{
	typedef TTMSFNCCustomWebBrowser inherited;
	
public:
	virtual void __fastcall StartDocumentReadyStateThread();
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, _di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent = _di_TTMSFNCWebBrowserJavaScriptCompleteEvent(), bool AImmediate = false);
	virtual System::UnicodeString __fastcall ExecuteJavaScriptSync(System::UnicodeString AScript);
	virtual void __fastcall LoadHTML(System::UnicodeString AHTML);
	virtual void __fastcall LoadFile(System::UnicodeString AFile);
	virtual void __fastcall Initialize();
	virtual void __fastcall DeInitialize();
	virtual void __fastcall GoForward();
	virtual void __fastcall GoBack();
	virtual void __fastcall Reload();
	virtual void __fastcall StopLoading();
	virtual void __fastcall ShowDebugConsole();
	virtual void __fastcall AddBridge(System::UnicodeString ABridgeName, System::TObject* ABridgeObject);
	virtual void __fastcall RemoveBridge(System::UnicodeString ABridgeName);
	virtual void __fastcall CaptureScreenShot();
	virtual System::UnicodeString __fastcall GetBridgeCommunicationLayer(System::UnicodeString ABridgeName);
	virtual void * __fastcall NativeEnvironment();
	virtual void * __fastcall NativeBrowser();
	virtual bool __fastcall IsFMXBrowser();
	virtual bool __fastcall CanGoBack();
	virtual bool __fastcall CanGoForward();
	virtual void * __fastcall NativeDialog();
	__property OnCloseForm;
	__property EnableContextMenu = {default=1};
	__property EnableShowDebugConsole = {default=1};
	__property EnableAcceleratorKeys = {default=1};
	__property CacheFolder = {default=0};
	__property CacheFolderName = {default=0};
	__property AutoClearCache;
	virtual void __fastcall ClearCache();
	__property UserAgent = {default=0};
	
__published:
	__property OnInitialized;
	__property URL = {default=0};
	__property OnBeforeNavigate;
	__property OnNavigateComplete;
	__property OnHardwareButtonClicked;
	__property OnCaptureScreenShot;
	__property OnDocumentComplete;
	__property Version = {default=0};
	__property DesigntimeEnabled = {default=1};
public:
	/* TTMSFNCCustomWebBrowser.Create */ inline __fastcall virtual TTMSFNCWebBrowser(System::Classes::TComponent* AOwner) : TTMSFNCCustomWebBrowser(AOwner) { }
	/* TTMSFNCCustomWebBrowser.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowser() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserPopupForm : public Fmx::Forms::TCommonCustomForm
{
	typedef Fmx::Forms::TCommonCustomForm inherited;
	
private:
	TTMSFNCWebBrowserPopup* FWebBrowserPopup;
	
protected:
	void __fastcall UpdateBackGround();
	
public:
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight)/* overload */;
public:
	/* TCommonCustomForm.Create */ inline __fastcall virtual TTMSFNCWebBrowserPopupForm(System::Classes::TComponent* AOwner) : Fmx::Forms::TCommonCustomForm(AOwner) { }
	/* TCommonCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCWebBrowserPopupForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Forms::TCommonCustomForm(AOwner, Dummy) { }
	/* TCommonCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserPopupForm() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetBounds(const System::Types::TRect &ARect){ Fmx::Forms::TCommonCustomForm::SetBounds(ARect); }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserPopupButtonEventHandler : public Androidapi::Jnibridge::TJavaLocal
{
	typedef Androidapi::Jnibridge::TJavaLocal inherited;
	
private:
	TTMSFNCWebBrowserPopup* FWebBrowserPopup;
	
public:
	void __cdecl onClick(Androidapi::Jni::Graphicscontentviewtext::_di_JView P1);
public:
	/* TJavaLocal.Create */ inline __fastcall TTMSFNCWebBrowserPopupButtonEventHandler() : Androidapi::Jnibridge::TJavaLocal() { }
	/* TJavaLocal.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserPopupButtonEventHandler() { }
	
private:
	void *__JView_OnClickListener;	// Androidapi::Jni::Graphicscontentviewtext::JView_OnClickListener 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {7603153E-EEF8-4F60-9882-FF5490A5AF28}
	operator Androidapi::Jni::Graphicscontentviewtext::_di_JView_OnClickListener()
	{
		Androidapi::Jni::Graphicscontentviewtext::_di_JView_OnClickListener intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Androidapi::Jni::Graphicscontentviewtext::JView_OnClickListener*(void) { return (Androidapi::Jni::Graphicscontentviewtext::JView_OnClickListener*)&__JView_OnClickListener; }
	#endif
	
};


_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomWebBrowserClass);

typedef Fmx::Forms::TFormPosition TTMSFNCWebBrowserFormPosition;

class PASCALIMPLEMENTATION TTMSFNCWebBrowserPopup : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FModal;
	Androidapi::Jni::Widget::_di_JButton FButton;
	TTMSFNCWebBrowserPopupButtonEventHandler* FButtonEventHandler;
	TTMSFNCCustomWebBrowser* FWebBrowser;
	TTMSFNCCustomWebBrowserNavigateComplete FOnNavigateComplete;
	TTMSFNCCustomWebBrowserBeforeNavigate FOnBeforeNavigate;
	System::UnicodeString FURL;
	Fmx::Forms::TFormPosition FPosition;
	int FWidth;
	int FHeight;
	int FT;
	int FL;
	bool FFullScreen;
	bool FCloseButton;
	System::Classes::TNotifyEvent FOnClose;
	System::UnicodeString FCloseButtonText;
	bool FExternalBrowser;
	
protected:
	void __fastcall ButtonClose(System::TObject* Sender);
	void __fastcall CloseForm(System::TObject* Sender);
	virtual void __fastcall DoBeforeNavigate(System::TObject* Sender, TTMSFNCCustomWebBrowserBeforeNavigateParams &Params);
	virtual void __fastcall DoNavigateComplete(System::TObject* Sender, TTMSFNCCustomWebBrowserNavigateCompleteParams &Params);
	virtual void __fastcall InitializeWebBrowser(TTMSFNCCustomWebBrowser* AWebBrowser);
	virtual TTMSFNCCustomWebBrowserClass __fastcall GetWebBrowserClass();
	
public:
	__fastcall virtual TTMSFNCWebBrowserPopup(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCWebBrowserPopup();
	System::Uitypes::TModalResult __fastcall Open(bool AModal = true)/* overload */;
	System::Uitypes::TModalResult __fastcall Open(System::UnicodeString AURL, bool AModal = true)/* overload */;
	void __fastcall Close(System::Uitypes::TModalResult AModalResult = (System::Uitypes::TModalResult)(0x1));
	__property TTMSFNCCustomWebBrowser* WebBrowser = {read=FWebBrowser};
	__property bool ExternalBrowser = {read=FExternalBrowser, write=FExternalBrowser, default=0};
	
__published:
	__property TTMSFNCCustomWebBrowserBeforeNavigate OnBeforeNavigate = {read=FOnBeforeNavigate, write=FOnBeforeNavigate};
	__property TTMSFNCCustomWebBrowserNavigateComplete OnNavigateComplete = {read=FOnNavigateComplete, write=FOnNavigateComplete};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
	__property Fmx::Forms::TFormPosition Position = {read=FPosition, write=FPosition, default=4};
	__property bool FullScreen = {read=FFullScreen, write=FFullScreen, default=0};
	__property int Width = {read=FWidth, write=FWidth, default=640};
	__property int Height = {read=FHeight, write=FHeight, default=480};
	__property int Left = {read=FL, write=FL, default=0};
	__property int Top = {read=FT, write=FT, default=0};
	__property bool CloseButton = {read=FCloseButton, write=FCloseButton, default=0};
	__property System::UnicodeString CloseButtonText = {read=FCloseButtonText, write=FCloseButtonText};
	__property System::Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
	
public:
	HIDESBASE System::UnicodeString __fastcall GetVersionNumber(System::Int8 AMaj, System::Int8 AMin, System::Int8 ARel, System::Int8 ABld);
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserPlatformServicesService : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::_di_IInterface FInterface;
	System::UnicodeString FGUID;
	
public:
	__fastcall TTMSFNCWebBrowserPlatformServicesService(System::UnicodeString AGUID, System::_di_IInterface AInterface);
	__property System::UnicodeString GUID = {read=FGUID};
	__property System::_di_IInterface Interface = {read=FInterface};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserPlatformServicesService() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserPlatformServicesList : public System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*> inherited;
	
private:
	System::_di_IInterface __fastcall GetValue(System::UnicodeString AGUID);
	
public:
	bool __fastcall ContainsKey(System::UnicodeString AGUID);
	void __fastcall RemoveByGUID(System::UnicodeString AGUID);
	__property System::_di_IInterface Interfaces[System::UnicodeString AGUID] = {read=GetValue};
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWebBrowserPlatformServicesService*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCWebBrowserPlatformServicesService*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Destroy */ inline __fastcall virtual ~TTMSFNCWebBrowserPlatformServicesList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWebBrowserPlatformServicesService*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCWebBrowserPlatformServicesService*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebBrowser_TTMSFNCWebBrowserPlatformServicesService>.Create */ inline __fastcall TTMSFNCWebBrowserPlatformServicesList(TTMSFNCWebBrowserPlatformServicesService* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWebBrowserPlatformServicesService*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebBrowserPlatformServices : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCWebBrowserPlatformServicesList* FServicesList;
	static TTMSFNCWebBrowserPlatformServices* FCurrent;
	static bool FCurrentReleased;
	__classmethod void __fastcall ReleaseCurrent();
	
public:
	__fastcall TTMSFNCWebBrowserPlatformServices();
	__fastcall virtual ~TTMSFNCWebBrowserPlatformServices();
	void __fastcall AddPlatformService(const GUID &AServiceGUID, const System::_di_IInterface AService);
	void __fastcall RemovePlatformService(const GUID &AServiceGUID);
	int __fastcall Count();
	System::_di_IInterface __fastcall GetPlatformService(const GUID &AServiceGUID);
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID)/* overload */;
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID, System::_di_IInterface &AService)/* overload */;
	__classmethod TTMSFNCWebBrowserPlatformServices* __fastcall Current();
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x1);
static constexpr System::Int8 REL_VER = System::Int8(0x4);
static constexpr System::Int8 BLD_VER = System::Int8(0x8);
#define DESIGNTIMEMESSAGE u"Selectable/Draggable area (only active at designtime)"
#define IID_ITMSFNCCustomWebBrowserGUID u"{F74562D0-56C7-4ED2-B01B-8C9C16DD9361}"
#define IID_ITMSFNCCustomWebBrowserExGUID u"{6B5D75C1-B5EC-463D-A602-1FFB97C8668C}"
#define IID_ITMSFNCCustomWebBrowserContextMenuGUID u"{04BD0560-104B-4D3B-8CB1-45628D16CB0D}"
#define IID_ITMSFNCCustomWebBrowserBridgeGUID u"{AC2934EC-9397-4A99-8E0A-1EF58803C8EA}"
#define IID_ITMSFNCCustomWebBrowserSettingsGUID u"{25142510-A807-4635-BAE7-CB261D00137E}"
#define IID_ITMSFNCCustomWebBrowserCookiesGUID u"{A50ABF08-0A6F-4877-AC92-FC834CF36AE6}"
#define IID_ITMSFNCCustomWebBrowserServiceGUID u"{4B7A5FE1-A889-47C6-B40F-A611BB6266E6}"
#define IID_ITMSFNCCustomWebBrowserPrintGUID u"{56EFC9E8-CD92-4FAC-B79C-084BF3DB0FBD}"
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowser;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserBridge;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserSettings;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserCookies;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserService;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserEx;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserContextMenu;
extern DELPHI_PACKAGE GUID IID_ITMSFNCCustomWebBrowserPrint;
}	/* namespace Tmsfncwebbrowser */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWEBBROWSER)
using namespace Fmx::Tmsfncwebbrowser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwebbrowserHPP
