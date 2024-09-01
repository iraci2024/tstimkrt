// CodeGear C++Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxckeyderiv.pas' rev: 11.00

#ifndef FxckeyderivHPP
#define FxckeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Fxchash.hpp>	// Pascal unit
#include <Fxchmac.hpp>	// Pascal unit
#include <Fxcpb_kdf.hpp>	// Pascal unit
#include <Fxcsha1.hpp>	// Pascal unit

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
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxckeyderiv
