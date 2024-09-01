// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCKeyDeriv.pas' rev: 6.00

#ifndef FXCKeyDerivHPP
#define FXCKeyDerivHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <FXCsha1.hpp>	// Pascal unit
#include <FXCpb_kdf.hpp>	// Pascal unit
#include <FXChmac.hpp>	// Pascal unit
#include <FXCHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxckeyderiv
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool HaltOnError;
extern PACKAGE void __fastcall PBKDF2(void * pPW, Word pLen, void * salt, Word sLen, int C, void *DK, Word dkLen);

}	/* namespace Fxckeyderiv */
using namespace Fxckeyderiv;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCKeyDeriv
