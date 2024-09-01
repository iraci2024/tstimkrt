// CodeGear C++Builder
// Copyright (c) 1995, 2010 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCKeyDeriv.pas' rev: 22.00

#ifndef FxckeyderivHPP
#define FxckeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <FXCHash.hpp>	// Pascal unit
#include <FXChmac.hpp>	// Pascal unit
#include <FXCpb_kdf.hpp>	// Pascal unit
#include <FXCsha1.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxckeyderiv
{
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool HaltOnError;
extern PACKAGE void __fastcall PBKDF2(void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);

}	/* namespace Fxckeyderiv */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxckeyderiv;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxckeyderivHPP
