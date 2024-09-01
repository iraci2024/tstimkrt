﻿// CodeGear C++Builder
// Copyright (c) 1995, 2013 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFExcept.pas' rev: 26.00 (Windows)

#ifndef ZfexceptHPP
#define ZfexceptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfexcept
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EZFException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZFException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	int ErrorCode;
	int NativeError;
	__fastcall EZFException(int NativeErr, System::Classes::TComponent* Component)/* overload */;
	__fastcall EZFException(int NativeErr, System::TVarRec const *Args, const int Args_Size, System::Classes::TComponent* Component)/* overload */;
public:
	/* Exception.CreateFmt */ inline __fastcall EZFException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZFException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZFException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZFException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EZFException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZFException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZFException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZFException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZFException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZFException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZFException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZFException(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Zfexcept */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFEXCEPT)
using namespace Zfexcept;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfexceptHPP
