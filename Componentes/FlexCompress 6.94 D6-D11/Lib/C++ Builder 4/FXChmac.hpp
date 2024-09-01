// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXChmac.pas' rev: 4.00

#ifndef FXChmacHPP
#define FXChmacHPP

#pragma delphiheader begin
#pragma option push -w-
#include <FXCHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxchmac
{
//-- type declarations -------------------------------------------------------
#pragma pack(push, 4)
struct THMAC_Context
{
	Fxchash::THashContext hashctx;
	Byte hmacbuf[128];
	Fxchash::THashDesc *phashd;
} ;
#pragma pack(pop)

typedef AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Fxchash::PHashDesc phash, void * key, Word 
	klen);
extern PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Fxchash::PHashDesc phash, AnsiString skey
	);
extern PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, Word dlen);
extern PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, Byte * mac);

}	/* namespace Fxchmac */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxchmac;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXChmac
