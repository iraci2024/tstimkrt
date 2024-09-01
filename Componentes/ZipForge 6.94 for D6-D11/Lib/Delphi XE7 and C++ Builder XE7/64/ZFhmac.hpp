// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFhmac.pas' rev: 28.00 (Windows)

#ifndef ZfhmacHPP
#define ZfhmacHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfhmac
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD THMAC_Context
{
public:
	Zfhash::THashContext hashctx;
	Zfhash::THashBuffer hmacbuf;
	Zfhash::THashDesc *phashd;
};


typedef System::AnsiString THMAC_string;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall hmac_init(THMAC_Context &ctx, Zfhash::PHashDesc phash, void * key, System::Word klen);
extern DELPHI_PACKAGE void __fastcall hmac_inits(THMAC_Context &ctx, Zfhash::PHashDesc phash, System::AnsiString skey);
extern DELPHI_PACKAGE void __fastcall hmac_update(THMAC_Context &ctx, void * data, System::Word dlen);
extern DELPHI_PACKAGE void __fastcall hmac_updateXL(THMAC_Context &ctx, void * data, int dlen);
extern DELPHI_PACKAGE void __fastcall hmac_final(THMAC_Context &ctx, Zfhash::THashDigest &mac);
}	/* namespace Zfhmac */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFHMAC)
using namespace Zfhmac;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfhmacHPP
