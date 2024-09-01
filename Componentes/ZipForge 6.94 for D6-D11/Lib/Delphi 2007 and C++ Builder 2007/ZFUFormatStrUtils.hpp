// CodeGear C++Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfuformatstrutils.pas' rev: 11.00

#ifndef ZfuformatstrutilsHPP
#define ZfuformatstrutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfusysutils.hpp>	// Pascal unit

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
	/* Exception.CreateFmt */ inline __fastcall EFormatSpecError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Zfusysutils::ETntGeneralError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EFormatSpecError(int Ident)/* overload */ : Zfusysutils::ETntGeneralError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EFormatSpecError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Zfusysutils::ETntGeneralError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, int AHelpContext) : Zfusysutils::ETntGeneralError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EFormatSpecError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Zfusysutils::ETntGeneralError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EFormatSpecError(int Ident, int AHelpContext)/* overload */ : Zfusysutils::ETntGeneralError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EFormatSpecError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Zfusysutils::ETntGeneralError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EFormatSpecError(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE WideString __fastcall GetCanonicalFormatStr(const WideString _FormatString);
extern PACKAGE void __fastcall CompareFormatStrings(WideString FormatStr1, WideString FormatStr2);
extern PACKAGE bool __fastcall FormatStringsAreCompatible(WideString FormatStr1, WideString FormatStr2);

}	/* namespace Zfuformatstrutils */
using namespace Zfuformatstrutils;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfuformatstrutils
