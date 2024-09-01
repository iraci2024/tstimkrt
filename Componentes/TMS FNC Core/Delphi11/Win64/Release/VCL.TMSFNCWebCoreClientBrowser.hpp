// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCWebCoreClientBrowser.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncwebcoreclientbrowserHPP
#define Vcl_TmsfncwebcoreclientbrowserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCWebBrowser.hpp>
#include <System.JSON.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncwebcoreclientbrowser
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomWebCoreClientBrowser;
class DELPHICLASS TTMSFNCWebCoreClientBrowser;
class DELPHICLASS TTMSFNCWebCoreClientBrowserPopup;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTMSFNCWebCoreClientBrowserReceiveMessageEvent)(System::TObject* Sender, System::Json::TJSONValue* AMessage);

class PASCALIMPLEMENTATION TTMSFNCCustomWebCoreClientBrowser : public Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser
{
	typedef Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser inherited;
	
private:
	bool FBuffer;
	System::UnicodeString FBufferString;
	TTMSFNCWebCoreClientBrowserReceiveMessageEvent FOnReceiveMessage;
	System::Classes::TNotifyEvent FOnConnected;
	
protected:
	virtual void __fastcall PerformHandshake();
	virtual void __fastcall DoDocumentComplete();
	virtual void __fastcall BeforeNavigate(Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowserBeforeNavigateParams &Params);
	virtual void __fastcall DoReceiveMessage(System::Json::TJSONValue* AMessage);
	virtual void __fastcall DoConnected();
	__property TTMSFNCWebCoreClientBrowserReceiveMessageEvent OnReceiveMessage = {read=FOnReceiveMessage, write=FOnReceiveMessage};
	__property System::Classes::TNotifyEvent OnConnected = {read=FOnConnected, write=FOnConnected};
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall SendMessage(const System::UnicodeString AMessage);
	virtual void __fastcall Send(System::Json::TJSONValue* const AJSON);
	virtual void __fastcall Connect();
public:
	/* TTMSFNCCustomWebBrowser.Create */ inline __fastcall virtual TTMSFNCCustomWebCoreClientBrowser(System::Classes::TComponent* AOwner) : Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser(AOwner) { }
	/* TTMSFNCCustomWebBrowser.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWebCoreClientBrowser() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomWebCoreClientBrowser(HWND ParentWindow) : Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebCoreClientBrowser : public TTMSFNCCustomWebCoreClientBrowser
{
	typedef TTMSFNCCustomWebCoreClientBrowser inherited;
	
public:
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, Vcl::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent = Vcl::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent(), bool AImmediate = false);
	virtual bool __fastcall CheckIdentifier();
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
	virtual System::UnicodeString __fastcall GetBridgeCommunicationLayer(System::UnicodeString ABridgeName);
	virtual void * __fastcall NativeEnvironment();
	virtual void * __fastcall NativeBrowser();
	virtual bool __fastcall IsFMXBrowser();
	virtual bool __fastcall CanGoBack();
	virtual bool __fastcall CanGoForward();
	virtual System::_di_IInterface __fastcall GetWebBrowserInstance();
	__property OnCloseForm;
	__property EnableContextMenu = {default=1};
	__property EnableShowDebugConsole = {default=1};
	__property EnableAcceleratorKeys = {default=1};
	__property CacheFolder = {default=0};
	__property CacheFolderName = {default=0};
	__property AutoClearCache;
	virtual void __fastcall ClearCache();
	virtual void __fastcall SendMessage(const System::UnicodeString AMessage);
	virtual void __fastcall Send(System::Json::TJSONValue* const AJSON);
	virtual void __fastcall Connect();
	
__published:
	__property URL = {default=0};
	__property OnBeforeNavigate;
	__property OnNavigateComplete;
	__property OnHardwareButtonClicked;
	__property Version = {default=0};
	__property OnReceiveMessage;
	__property OnConnected;
public:
	/* TTMSFNCCustomWebBrowser.Create */ inline __fastcall virtual TTMSFNCWebCoreClientBrowser(System::Classes::TComponent* AOwner) : TTMSFNCCustomWebCoreClientBrowser(AOwner) { }
	/* TTMSFNCCustomWebBrowser.Destroy */ inline __fastcall virtual ~TTMSFNCWebCoreClientBrowser() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCWebCoreClientBrowser(HWND ParentWindow) : TTMSFNCCustomWebCoreClientBrowser(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebCoreClientBrowserPopup : public Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPopup
{
	typedef Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPopup inherited;
	
private:
	System::Classes::TNotifyEvent FOnConnected;
	TTMSFNCWebCoreClientBrowserReceiveMessageEvent FOnReceiveMessage;
	
protected:
	virtual Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowserClass __fastcall GetWebBrowserClass();
	virtual void __fastcall InitializeWebBrowser(Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser* AWebBrowser);
	virtual void __fastcall DoReceiveMessage(System::TObject* Sender, System::Json::TJSONValue* AMessage);
	virtual void __fastcall DoConnected(System::TObject* Sender);
	
public:
	virtual void __fastcall SendMessage(const System::UnicodeString AMessage);
	virtual void __fastcall Send(System::Json::TJSONValue* const AJSON);
	void __fastcall Connect();
	
__published:
	__property TTMSFNCWebCoreClientBrowserReceiveMessageEvent OnReceiveMessage = {read=FOnReceiveMessage, write=FOnReceiveMessage};
	__property System::Classes::TNotifyEvent OnConnected = {read=FOnConnected, write=FOnConnected};
public:
	/* TTMSFNCWebBrowserPopup.Create */ inline __fastcall virtual TTMSFNCWebCoreClientBrowserPopup(System::Classes::TComponent* AOwner) : Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPopup(AOwner) { }
	/* TTMSFNCWebBrowserPopup.Destroy */ inline __fastcall virtual ~TTMSFNCWebCoreClientBrowserPopup() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncwebcoreclientbrowser */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCWEBCORECLIENTBROWSER)
using namespace Vcl::Tmsfncwebcoreclientbrowser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncwebcoreclientbrowserHPP
