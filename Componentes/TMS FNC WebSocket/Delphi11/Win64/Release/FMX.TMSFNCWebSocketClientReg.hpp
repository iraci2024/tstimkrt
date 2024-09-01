// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWebSocketClientReg.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwebsocketclientregHPP
#define Fmx_TmsfncwebsocketclientregHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Rtti.hpp>
#include <FMX.TMSFNCWebSocketClient.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwebsocketclientreg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THackRttiObject;
struct TRttiObjectHelper /* Helper for class 'System::Rtti::TRttiObject*' */;
struct TTMSIDERegister;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION THackRttiObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	void *FHandle;
	int FRttiDataSize;
	System::Rtti::TRttiPackage* FPackage;
	System::Rtti::TRttiObject* FParent;
	System::DelphiInterface<System::Sysutils::TFunc__1<System::TArray__1<System::TCustomAttribute*> > > FAttributeGetter;
public:
	/* TObject.Create */ inline __fastcall THackRttiObject() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~THackRttiObject() { }
	
};


struct DECLSPEC_DRECORD TTMSIDERegister
{
private:
	static void __fastcall Register();
	static void __fastcall Unregister();
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Tmsfncwebsocketclientreg */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWEBSOCKETCLIENTREG)
using namespace Fmx::Tmsfncwebsocketclientreg;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwebsocketclientregHPP
