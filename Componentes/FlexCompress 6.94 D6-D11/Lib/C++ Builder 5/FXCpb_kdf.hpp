// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCpb_kdf.pas' rev: 5.00

#ifndef FXCpb_kdfHPP
#define FXCpb_kdfHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <FXChmac.hpp>	// Pascal unit
#include <FXCHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcpb_kdf
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
static const Shortint kdf_err_nil_pointer = 0x1;
static const Shortint kdf_err_digestlen = 0x2;
extern PACKAGE int __fastcall kdf2(Fxchash::PHashDesc phash, void * pPW, Word pLen, void * salt, Word 
	sLen, int C, void *DK, Word dkLen);
extern PACKAGE int __fastcall kdf2s(Fxchash::PHashDesc phash, AnsiString sPW, void * salt, Word sLen
	, int C, void *DK, Word dkLen);

}	/* namespace Fxcpb_kdf */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcpb_kdf;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCpb_kdf
