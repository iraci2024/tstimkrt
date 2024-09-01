// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Extension_Base.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_extension_baseHPP
#define Sgcwebsocket_extension_baseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_extension_base
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSExtension_Base;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSExtension_Base : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FActive;
	bool FEnabled;
	Sgcwebsocket_types::TwsApplicationMode FMode;
	
protected:
	System::UnicodeString FHeaderExtension;
	
public:
	virtual System::UnicodeString __fastcall GetHeaderExtension();
	virtual bool __fastcall DecodeExtension(const System::UnicodeString aExtension);
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	virtual void __fastcall DecodeHeader(System::Byte aByte) = 0 ;
	virtual void __fastcall EncodeHeader(System::Byte &aByte) = 0 ;
	__fastcall virtual TsgcWSExtension_Base();
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	__property Sgcwebsocket_types::TwsApplicationMode Mode = {read=FMode, write=FMode, nodefault};
	__property bool Active = {read=FActive, write=FActive, nodefault};
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSExtension_Base() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_extension_base */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_EXTENSION_BASE)
using namespace Sgcwebsocket_extension_base;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_extension_baseHPP
