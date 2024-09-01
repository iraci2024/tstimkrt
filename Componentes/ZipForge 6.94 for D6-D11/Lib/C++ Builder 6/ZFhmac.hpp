// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFhmac.pas' rev: 6.00

#ifndef ZFhmacHPP
#define ZFhmacHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ZFHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfhmac
{
//-- type declarations -------------------------------------------------------
#pragma pack(push, 4)
struct THMAC_Context
{
	Zfhash::THashContext hashctx;
	Byte hmacbuf[128];
	Zfhash::THashDesc *phashd;
} ;
#pragma pack(pop)

typedef AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Zfhash::PHashDesc phash, void * key, Word klen);
extern PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Zfhash::PHashDesc phash, AnsiString skey);
extern PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, Word dlen);
extern PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, Byte * mac);

}	/* namespace Zfhmac */
using namespace Zfhmac;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFhmac
