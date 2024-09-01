// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCExcept.pas' rev: 4.00

#ifndef FXCExceptHPP
#define FXCExceptHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcexcept
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EFXCException;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION EFXCException : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	int ErrorCode;
	int NativeError;
	__fastcall EFXCException(int NativeErr, Classes::TComponent* Component)/* overload */;
	__fastcall EFXCException(int NativeErr, const System::TVarRec * Args, const int Args_Size, Classes::TComponent* 
		Component)/* overload */;
public:
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EFXCException(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EFXCException(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EFXCException(int Ident, const System::TVarRec * Args
		, const int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EFXCException(const AnsiString Msg, int AHelpContext) : 
		Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EFXCException(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EFXCException(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EFXCException(int Ident, const System::TVarRec * 
		Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EFXCException(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Fxcexcept */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcexcept;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCExcept
