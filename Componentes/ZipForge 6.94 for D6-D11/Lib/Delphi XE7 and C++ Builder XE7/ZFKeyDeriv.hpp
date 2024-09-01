// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFKeyDeriv.pas' rev: 28.00 (Windows)

#ifndef ZfkeyderivHPP
#define ZfkeyderivHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit
#include <ZFhmac.hpp>	// Pascal unit
#include <ZFpb_kdf.hpp>	// Pascal unit
#include <ZFsha1.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfkeyderiv
{
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
