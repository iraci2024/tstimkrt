// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfpb_kdf.pas' rev: 10.00

#ifndef Zfpb_kdfHPP
#define Zfpb_kdfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfhash.hpp>	// Pascal unit
#include <Zfhmac.hpp>	// Pascal unit

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
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfpb_kdf
