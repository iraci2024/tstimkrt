// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCustomWEBComponent.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccustomwebcomponentHPP
#define Fmx_TmsfnccustomwebcomponentHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCCustomWEBControl.hpp>
#include <FMX.TMSFNCWebBrowser.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccustomwebcomponent
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomWEBComponent;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCCustomWEBComponent : public Fmx::Tmsfnccustomwebcontrol::TTMSFNCCustomWEBControl
{
	typedef Fmx::Tmsfnccustomwebcontrol::TTMSFNCCustomWEBControl inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual NativeUInt __fastcall GetInstance();
	virtual bool __fastcall CanBeVisible();
	
public:
	__fastcall virtual TTMSFNCCustomWEBComponent(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetBounds(float X, float Y, float AWidth, float AHeight);
	virtual void __fastcall Paint();
	
__published:
	__property Visible = {default=1};
public:
	/* TTMSFNCCustomWEBControl.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWEBComponent() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomWEBComponentClass);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfnccustomwebcomponent */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCUSTOMWEBCOMPONENT)
using namespace Fmx::Tmsfnccustomwebcomponent;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccustomwebcomponentHPP
