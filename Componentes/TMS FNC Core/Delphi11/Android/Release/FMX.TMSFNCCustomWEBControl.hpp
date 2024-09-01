// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCustomWEBControl.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccustomwebcontrolHPP
#define Fmx_TmsfnccustomwebcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.DateUtils.hpp>
#include <FMX.Forms.hpp>
#include <System.Generics.Collections.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCWebBrowser.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccustomwebcontrol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomWEBControlLink;
class DELPHICLASS TTMSFNCCustomWEBControlLinksList;
class DELPHICLASS TTMSFNCCustomWEBControlEventData;
class DELPHICLASS TTMSFNCCustomWEBControlBridge;
class DELPHICLASS TTMSFNCCustomWEBControl;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCCustomWEBControlLinkKind : unsigned char { mlkLink, mlkScript, mlkStyle };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomWEBControlLink : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FCharSet;
	System::UnicodeString FType;
	System::UnicodeString FURL;
	System::UnicodeString FContent;
	bool FDefer;
	bool FAsync;
	System::UnicodeString FRel;
	TTMSFNCCustomWEBControlLinkKind FKind;
	
public:
	__fastcall virtual TTMSFNCCustomWEBControlLink(System::UnicodeString AURL, System::UnicodeString AType, System::UnicodeString ACharSet, System::UnicodeString AContent, bool ADefer, bool AAsync);
	__fastcall virtual TTMSFNCCustomWEBControlLink(System::UnicodeString AURL, System::UnicodeString AType, System::UnicodeString ARel);
	__fastcall virtual TTMSFNCCustomWEBControlLink(TTMSFNCCustomWEBControlLinkKind AKind, System::UnicodeString AURL, System::UnicodeString AType, System::UnicodeString ACharSet, System::UnicodeString ARel, System::UnicodeString AContent, bool ADefer, bool AAsync);
	__property System::UnicodeString URL = {read=FURL, write=FURL};
	__property System::UnicodeString Type = {read=FType, write=FType};
	__property System::UnicodeString CharSet = {read=FCharSet, write=FCharSet};
	__property System::UnicodeString Content = {read=FContent, write=FContent};
	__property System::UnicodeString Rel = {read=FRel, write=FRel};
	__property bool Defer = {read=FDefer, write=FDefer, nodefault};
	__property bool Async = {read=FAsync, write=FAsync, nodefault};
	__property TTMSFNCCustomWEBControlLinkKind Kind = {read=FKind, write=FKind, nodefault};
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
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWEBControlLink() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomWEBControlLinksList : public System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCustomWEBControlLink*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(System::Generics::Collections::TEnumerable__1<TTMSFNCCustomWEBControlLink*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWEBControlLinksList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCustomWEBControlLink*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(System::Generics::Collections::TEnumerable__1<TTMSFNCCustomWEBControlLink*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCustomWEBControl_TTMSFNCCustomWEBControlLink>.Create */ inline __fastcall TTMSFNCCustomWEBControlLinksList(TTMSFNCCustomWEBControlLink* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCustomWEBControlLink*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomWEBControlEventData : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FEvent;
	System::UnicodeString FEventName;
	System::UnicodeString FID;
	System::UnicodeString FCustomData;
	
public:
	__fastcall virtual TTMSFNCCustomWEBControlEventData();
	__fastcall virtual ~TTMSFNCCustomWEBControlEventData();
	__property System::UnicodeString CustomData = {read=FCustomData, write=FCustomData};
	
__published:
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString EventName = {read=FEventName, write=FEventName};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCCustomWEBControlBridgeObjectMessageEvent)(const System::UnicodeString AValue);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomWEBControlBridge : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
private:
	TTMSFNCCustomWEBControlBridgeObjectMessageEvent FOnObjectMessage;
	System::UnicodeString __fastcall GetObjectMessage();
	void __fastcall SetObjectMessage(const System::UnicodeString Value);
	
public:
	__property TTMSFNCCustomWEBControlBridgeObjectMessageEvent OnObjectMessage = {read=FOnObjectMessage, write=FOnObjectMessage};
	
__published:
	__property System::UnicodeString ObjectMessage = {read=GetObjectMessage, write=SetObjectMessage};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWEBControlBridge() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCCustomWEBControlBridge() : System::Classes::TInterfacedPersistent() { }
	
private:
	void *__ITMSFNCCustomWebBrowserBridge;	// Fmx::Tmsfncwebbrowser::ITMSFNCCustomWebBrowserBridge 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {AC2934EC-9397-4A99-8E0A-1EF58803C8EA}
	operator Fmx::Tmsfncwebbrowser::_di_ITMSFNCCustomWebBrowserBridge()
	{
		Fmx::Tmsfncwebbrowser::_di_ITMSFNCCustomWebBrowserBridge intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncwebbrowser::ITMSFNCCustomWebBrowserBridge*(void) { return (Fmx::Tmsfncwebbrowser::ITMSFNCCustomWebBrowserBridge*)&__ITMSFNCCustomWebBrowserBridge; }
	#endif
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCCustomWEBControlCustomizeHeadLinksEvent)(System::TObject* Sender, TTMSFNCCustomWEBControlLinksList* AList);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomWEBControl : public Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser
{
	typedef Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowser inherited;
	
private:
	bool FControlInitialized;
	int FUpdateCount;
	bool FLocalFileAccess;
	bool FRemoveMargins;
	TTMSFNCCustomWEBControlCustomizeHeadLinksEvent FOnCustomizeHeadLinks;
	void __fastcall SetLocalFileAccess(const bool Value);
	void __fastcall SetRemoveMargins(const bool Value);
	void __fastcall SetControlInitialized(const bool Value);
	
protected:
	virtual bool __fastcall ParseEvent(System::UnicodeString AValue);
	virtual System::UnicodeString __fastcall ParseScript(System::UnicodeString AValue);
	virtual bool __fastcall IsControlReady();
	virtual void __fastcall Loaded();
	virtual void __fastcall DestroyControl();
	virtual void __fastcall DoControlInitialized();
	virtual void __fastcall BeforeNavigate(Fmx::Tmsfncwebbrowser::TTMSFNCCustomWebBrowserBeforeNavigateParams &Params);
	virtual void __fastcall Initialized();
	virtual void __fastcall InitializeControl();
	virtual void __fastcall InitializeHTML();
	virtual void __fastcall CreateClasses();
	virtual void __fastcall GetLinks(TTMSFNCCustomWEBControlLinksList* AList, bool AIncludeContent = true, bool ACheckReady = true);
	virtual void __fastcall GetBodyLinks(TTMSFNCCustomWEBControlLinksList* AList, bool AIncludeContent = true, bool ACheckReady = true);
	virtual void __fastcall GetHeadLinks(TTMSFNCCustomWEBControlLinksList* AList, bool ACheckReady = true);
	virtual void __fastcall DoCustomizeHeadLinks(TTMSFNCCustomWEBControlLinksList* AList);
	virtual void __fastcall LoadLinks(TTMSFNCCustomWEBControlLinksList* AList);
	virtual System::UnicodeString __fastcall GetCustomCSS();
	virtual void __fastcall CallCustomEvent(TTMSFNCCustomWEBControlEventData* AEventData);
	virtual System::UnicodeString __fastcall GetGlobalVariables();
	virtual System::UnicodeString __fastcall GetCustomFunctions();
	virtual System::UnicodeString __fastcall GetCommunicationLayer();
	virtual System::UnicodeString __fastcall GetDefaultHTML();
	virtual System::UnicodeString __fastcall ParseLinks(TTMSFNCCustomWEBControlLinksList* AList);
	virtual System::UnicodeString __fastcall GetHeadStyle();
	virtual System::UnicodeString __fastcall GetBody();
	virtual System::UnicodeString __fastcall GetControlTitle();
	virtual System::UnicodeString __fastcall GetControlID();
	virtual System::UnicodeString __fastcall GetControlVariable();
	virtual System::UnicodeString __fastcall GetEventDataName();
	virtual System::UnicodeString __fastcall GetDefaultHTMLMessage();
	virtual System::UnicodeString __fastcall GetDefaultEventDataObject();
	virtual System::UnicodeString __fastcall GetWaitForInitialization();
	virtual System::UnicodeString __fastcall GetWaitInitVariables();
	virtual System::UnicodeString __fastcall GetWaitInitCondition();
	virtual System::UnicodeString __fastcall GetWaitInitElseStatement();
	virtual System::UnicodeString __fastcall GetHTML();
	virtual bool __fastcall CanLoadDefaultHTML();
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	__property bool ControlInitialized = {read=FControlInitialized, write=SetControlInitialized, nodefault};
	__property bool LocalFileAccess = {read=FLocalFileAccess, write=SetLocalFileAccess, default=0};
	__property bool RemoveMargins = {read=FRemoveMargins, write=SetRemoveMargins, nodefault};
	__property TTMSFNCCustomWEBControlCustomizeHeadLinksEvent OnCustomizeHeadLinks = {read=FOnCustomizeHeadLinks, write=FOnCustomizeHeadLinks};
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCCustomWEBControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomWEBControl();
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall ReInitialize();
	__property EnableContextMenu = {default=1};
	__property EnableShowDebugConsole = {default=1};
	__property EnableAcceleratorKeys = {default=1};
	
__published:
	__property DesigntimeEnabled = {default=1};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::WideChar LB = (System::WideChar)(0xd);
#define STARTHTMLTAG u"<html>"
#define STARTHEADTAG u"<head>"
#define STARTBODYTAG u"<body>"
#define STARTTITLETAG u"<title>"
#define STARTSCRIPTTAG u"<script>"
#define STARTSTYLETAG u"<style>"
#define ENDHTMLTAG u"</html>"
#define ENDHEADTAG u"</head>"
#define ENDBODYTAG u"</body>"
#define ENDTITLETAG u"</title>"
#define ENDSCRIPTTAG u"</script>"
#define ENDSTYLETAG u"</style>"
}	/* namespace Tmsfnccustomwebcontrol */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCUSTOMWEBCONTROL)
using namespace Fmx::Tmsfnccustomwebcontrol;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccustomwebcontrolHPP
