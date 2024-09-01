// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFKeyDeriv.pas' rev: 33.00 (Windows)

#ifndef ZfkeyderivHPP
#define ZfkeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ZFHash.hpp>
#include <ZFhmac.hpp>
#include <ZFpb_kdf.hpp>
#include <ZFsha1.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zfkeyderiv
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool HaltOnError;
extern DELPHI_PACKAGE void __fastcall PBKDF2(void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
}	/* namespace Zfkeyderiv */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFKEYDERIV)
using namespace Zfkeyderiv;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfkeyderivHPP
