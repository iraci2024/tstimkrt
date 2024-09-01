// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCEdgeWebBrowser.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncedgewebbrowserHPP
#define Vcl_TmsfncedgewebbrowserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCWebBrowser.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncedgewebbrowser
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCEdgeWebBrowser;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCEdgeWebBrowser : public Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser
{
	typedef Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser inherited;
	
public:
	virtual void __fastcall StartDocumentReadyStateThread();
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, Vcl::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent = Vcl::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent(), bool AImmediate = false);
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
	virtual System::_di_IInterface __fastcall GetWebBrowserInstance();
	virtual void __fastcall OpenTaskManager();
	virtual void __fastcall GetCookies(System::UnicodeString AURI = System::UnicodeString());
	virtual void __fastcall AddCookie(const Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserCookie &ACookie);
	virtual void __fastcall DeleteAllCookies();
	virtual void __fastcall DeleteCookie(System::UnicodeString AName, System::UnicodeString ADomain, System::UnicodeString APath);
	virtual void __fastcall ShowPrintUI();
	virtual void __fastcall Print(const Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall Print()/* overload */;
	virtual void __fastcall PrintToPDFStream(const Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDFStream()/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName, const Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName)/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::UnicodeString ABody, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::Classes::TStream* ABodyStream, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual Vcl::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings __fastcall InitialPrintSettings();
	__property OnCloseForm;
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
	__property Settings;
	__property OnGetContextMenu;
	__property OnGetCookies;
	__property OnGetPrintPDFStream;
	__property OnPrintedToPDF;
	__property OnPrinted;
	__property OnCustomContextMenuItemSelected;
	__property OnGetPopupMenuForContextMenu;
public:
	/* TTMSFNCCustomWebBrowser.Create */ inline __fastcall virtual TTMSFNCEdgeWebBrowser(System::Classes::TComponent* AOwner) : Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser(AOwner) { }
	/* TTMSFNCCustomWebBrowser.Destroy */ inline __fastcall virtual ~TTMSFNCEdgeWebBrowser() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCEdgeWebBrowser(HWND ParentWindow) : Vcl::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncedgewebbrowser */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCEDGEWEBBROWSER)
using namespace Vcl::Tmsfncedgewebbrowser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncedgewebbrowserHPP
