// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxchmac.pas' rev: 10.00

#ifndef FxchmacHPP
#define FxchmacHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Fxchash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxchmac
{
//-- type declarations -------------------------------------------------------
struct THMAC_Context
{
	
public:
	#pragma pack(push,1)
	Fxchash::THashContext hashctx;
	#pragma pack(pop)
	Byte hmacbuf[128];
	Fxchash::THashDesc *phashd;
} ;

typedef AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Fxchash::PHashDesc phash, void * key, Word klen);
extern PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Fxchash::PHashDesc phash, AnsiString skey);
extern PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, Word dlen);
extern PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, Byte * mac);

}	/* namespace Fxchmac */
using namespace Fxchmac;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxchmac
