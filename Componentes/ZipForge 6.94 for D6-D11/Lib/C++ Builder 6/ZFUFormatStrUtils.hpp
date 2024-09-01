// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFUFormatStrUtils.pas' rev: 6.00

#ifndef ZFUFormatStrUtilsHPP
#define ZFUFormatStrUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ZFUSysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfuformatstrutils
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EFormatSpecError;
class PASCALIMPLEMENTATION EFormatSpecError : public Zfusysutils::ETntGeneralError 
{
	typedef Zfusysutils::ETntGeneralError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EFormatSpecError(const AnsiString Msg) : Zfusysutils::ETntGeneralError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EFormatSpecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Zfusysutils::ETntGeneralError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EFormatSpecError(int Ident)/* overload */ : Zfusysutils::ETntGeneralError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EFormatSpecError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Zfusysutils::ETntGeneralError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, int AHelpContext) : Zfusysutils::ETntGeneralError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Zfusysutils::ETntGeneralError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EFormatSpecError(int Ident, int AHelpContext)/* overload */ : Zfusysutils::ETntGeneralError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EFormatSpecError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Zfusysutils::ETntGeneralError(ResStringRec, Args, Args_Size, AHelpContext) { }
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

}	/* namespace Zfuformatstrutils */
using namespace Zfuformatstrutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFUFormatStrUtils
