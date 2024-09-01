// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCKeyDeriv.pas' rev: 32.00 (Windows)

#ifndef FxckeyderivHPP
#define FxckeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <FXCHash.hpp>
#include <FXChmac.hpp>
#include <FXCpb_kdf.hpp>
#include <FXCsha1.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxckeyderiv
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool HaltOnError;
extern DELPHI_PACKAGE void __fastcall PBKDF2(void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
}	/* namespace Fxckeyderiv */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCKEYDERIV)
using namespace Fxckeyderiv;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxckeyderivHPP
