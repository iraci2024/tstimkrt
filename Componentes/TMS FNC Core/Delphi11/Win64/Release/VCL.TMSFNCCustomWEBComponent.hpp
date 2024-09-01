// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCCustomWEBComponent.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnccustomwebcomponentHPP
#define Vcl_TmsfnccustomwebcomponentHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCCustomWEBControl.hpp>
#include <VCL.TMSFNCWebBrowser.hpp>
#include <VCL.TMSFNCCustomControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfnccustomwebcomponent
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomWEBComponent;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCCustomWEBComponent : public Vcl::Tmsfnccustomwebcontrol::TTMSFNCCustomWEBControl
{
	typedef Vcl::Tmsfnccustomwebcontrol::TTMSFNCCustomWEBControl inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual NativeUInt __fastcall GetInstance();
	virtual bool __fastcall CanBeVisible();
	
public:
	__fastcall virtual TTMSFNCCustomWEBComponent(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetBounds(int X, int Y, int AWidth, int AHeight);
	virtual void __fastcall Paint();
	
__published:
	__property Visible = {default=0};
public:
	/* TTMSFNCCustomWEBControl.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWEBComponent() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomWEBComponent(HWND ParentWindow) : Vcl::Tmsfnccustomwebcontrol::TTMSFNCCustomWEBControl(ParentWindow) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomWEBComponentClass);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfnccustomwebcomponent */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCCUSTOMWEBCOMPONENT)
using namespace Vcl::Tmsfnccustomwebcomponent;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnccustomwebcomponentHPP
