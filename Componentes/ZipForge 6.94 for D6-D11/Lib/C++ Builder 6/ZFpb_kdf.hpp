// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFpb_kdf.pas' rev: 6.00

#ifndef ZFpb_kdfHPP
#define ZFpb_kdfHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ZFhmac.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfpb_kdf
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
static const Shortint kdf_err_nil_pointer = 0x1;
static const Shortint kdf_err_digestlen = 0x2;
extern PACKAGE int __fastcall kdf2(Zfhash::PHashDesc phash, void * pPW, Word pLen, void * salt, Word sLen, int C, void *DK, Word dkLen);
extern PACKAGE int __fastcall kdf2s(Zfhash::PHashDesc phash, AnsiString sPW, void * salt, Word sLen, int C, void *DK, Word dkLen);

}	/* namespace Zfpb_kdf */
using namespace Zfpb_kdf;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFpb_kdf
