// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxchmac.pas' rev: 20.00

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
	Fxchash::THashContext hashctx;
	Fxchash::THashBuffer hmacbuf;
	Fxchash::THashDesc *phashd;
};


typedef System::AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Fxchash::PHashDesc phash, void * key, System::Word klen);
extern PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Fxchash::PHashDesc phash, System::AnsiString skey);
extern PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, System::Word dlen);
extern PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, System::Byte *mac);

}	/* namespace Fxchmac */
using namespace Fxchmac;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxchmacHPP
