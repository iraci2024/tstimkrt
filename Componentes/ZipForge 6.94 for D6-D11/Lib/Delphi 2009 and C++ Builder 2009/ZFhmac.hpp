// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfhmac.pas' rev: 20.00

#ifndef ZfhmacHPP
#define ZfhmacHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfhash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfhmac
{
//-- type declarations -------------------------------------------------------
struct THMAC_Context
{
	
public:
	Zfhash::THashContext hashctx;
	Zfhash::THashBuffer hmacbuf;
	Zfhash::THashDesc *phashd;
};


typedef System::AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Zfhash::PHashDesc phash, void * key, System::Word klen);
extern PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Zfhash::PHashDesc phash, System::AnsiString skey);
extern PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, System::Word dlen);
extern PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, System::Byte *mac);

}	/* namespace Zfhmac */
using namespace Zfhmac;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfhmacHPP
