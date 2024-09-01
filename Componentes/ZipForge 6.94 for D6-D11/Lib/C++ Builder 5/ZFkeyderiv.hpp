// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFKeyDeriv.pas' rev: 5.00

#ifndef ZFKeyDerivHPP
#define ZFKeyDerivHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ZFsha1.hpp>	// Pascal unit
#include <ZFpb_kdf.hpp>	// Pascal unit
#include <ZFhmac.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfkeyderiv
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool HaltOnError;
extern PACKAGE void __fastcall PBKDF2(void * pPW, Word pLen, void * salt, Word sLen, int C, void *DK
	, Word dkLen);

}	/* namespace Zfkeyderiv */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfkeyderiv;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFKeyDeriv
