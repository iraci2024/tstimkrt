// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfkeyderiv.pas' rev: 21.00

#ifndef ZfkeyderivHPP
#define ZfkeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfhash.hpp>	// Pascal unit
#include <Zfhmac.hpp>	// Pascal unit
#include <Zfpb_kdf.hpp>	// Pascal unit
#include <Zfsha1.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfkeyderiv
{
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool HaltOnError;
extern PACKAGE void __fastcall PBKDF2(void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);

}	/* namespace Zfkeyderiv */
using namespace Zfkeyderiv;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfkeyderivHPP
