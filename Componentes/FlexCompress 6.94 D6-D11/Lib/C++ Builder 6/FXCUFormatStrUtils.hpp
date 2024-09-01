// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCUFormatStrUtils.pas' rev: 6.00

#ifndef FXCUFormatStrUtilsHPP
#define FXCUFormatStrUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <FXCUSysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcuformatstrutils
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EFormatSpecError;
class PASCALIMPLEMENTATION EFormatSpecError : public Fxcusysutils::ETntGeneralError 
{
	typedef Fxcusysutils::ETntGeneralError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EFormatSpecError(const AnsiString Msg) : Fxcusysutils::ETntGeneralError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EFormatSpecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Fxcusysutils::ETntGeneralError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EFormatSpecError(int Ident)/* overload */ : Fxcusysutils::ETntGeneralError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EFormatSpecError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Fxcusysutils::ETntGeneralError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, int AHelpContext) : Fxcusysutils::ETntGeneralError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Fxcusysutils::ETntGeneralError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EFormatSpecError(int Ident, int AHelpContext)/* overload */ : Fxcusysutils::ETntGeneralError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EFormatSpecError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Fxcusysutils::ETntGeneralError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EFormatSpecError(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE WideString __fastcall GetCanonicalFormatStr(const WideString _FormatString);
extern PACKAGE WideString __fastcall ReplaceFloatingArgumentsInFormatString(const WideString _FormatString, const System::TVarRec * Args, const int Args_Size);
extern PACKAGE void __fastcall CompareFormatStrings(WideString FormatStr1, WideString FormatStr2);
extern PACKAGE bool __fastcall FormatStringsAreCompatible(WideString FormatStr1, WideString FormatStr2);

}	/* namespace Fxcuformatstrutils */
using namespace Fxcuformatstrutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCUFormatStrUtils
