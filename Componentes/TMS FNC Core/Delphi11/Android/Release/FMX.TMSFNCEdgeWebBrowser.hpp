// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCEdgeWebBrowser.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncedgewebbrowserHPP
#define Fmx_TmsfncedgewebbrowserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCWebBrowser.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncedgewebbrowser
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCEdgeWebBrowser;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCEdgeWebBrowser : public Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser
{
	typedef Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser inherited;
	
public:
	virtual void __fastcall StartDocumentReadyStateThread();
	virtual void __fastcall Navigate()/* overload */;
	virtual void __fastcall Navigate(const System::UnicodeString AURL)/* overload */;
	virtual void __fastcall ExecuteJavaScript(System::UnicodeString AScript, Fmx::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent ACompleteEvent = Fmx::Tmsfncwebbrowser::_di_TTMSFNCWebBrowserJavaScriptCompleteEvent(), bool AImmediate = false);
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
	virtual void __fastcall OpenTaskManager();
	virtual void __fastcall GetCookies(System::UnicodeString AURI = System::UnicodeString());
	virtual void __fastcall AddCookie(const Fmx::Tmsfncwebbrowser::TTMSFNCWebBrowserCookie &ACookie);
	virtual void __fastcall DeleteAllCookies();
	virtual void __fastcall DeleteCookie(System::UnicodeString AName, System::UnicodeString ADomain, System::UnicodeString APath);
	virtual void __fastcall ShowPrintUI();
	virtual void __fastcall Print(const Fmx::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall Print()/* overload */;
	virtual void __fastcall PrintToPDFStream(const Fmx::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDFStream()/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName, const Fmx::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings &APrintSettings)/* overload */;
	virtual void __fastcall PrintToPDF(System::UnicodeString AFileName)/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::UnicodeString ABody, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual void __fastcall NavigateWithData(System::UnicodeString AURI, System::UnicodeString AMethod, System::Classes::TStream* ABodyStream, System::Classes::TStrings* AHeaders = (System::Classes::TStrings*)(0x0))/* overload */;
	virtual Fmx::Tmsfncwebbrowser::TTMSFNCWebBrowserPrintSettings __fastcall InitialPrintSettings();
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
	/* TTMSFNCCustomWebBrowser.Create */ inline __fastcall virtual TTMSFNCEdgeWebBrowser(System::Classes::TComponent* AOwner) : Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser(AOwner) { }
	/* TTMSFNCCustomWebBrowser.Destroy */ inline __fastcall virtual ~TTMSFNCEdgeWebBrowser() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncedgewebbrowser */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCEDGEWEBBROWSER)
using namespace Fmx::Tmsfncedgewebbrowser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncedgewebbrowserHPP
