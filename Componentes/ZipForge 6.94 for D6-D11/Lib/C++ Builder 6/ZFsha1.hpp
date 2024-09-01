// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFsha1.pas' rev: 6.00

#ifndef ZFsha1HPP
#define ZFsha1HPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ZFHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfsha1
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SHA1Init(Zfhash::THashContext &Context);
extern PACKAGE void __fastcall SHA1UpdateXL(Zfhash::THashContext &Context, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Update(Zfhash::THashContext &Context, void * Msg, Word Len);
extern PACKAGE void __fastcall SHA1FinalEx(Zfhash::THashContext &Context, Byte * Digest);
extern PACKAGE void __fastcall SHA1Final(Zfhash::THashContext &Context, Byte * Digest);
extern PACKAGE bool __fastcall SHA1SelfTest(void);
extern PACKAGE void __fastcall SHA1FullXL(Byte * Digest, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Full(Byte * Digest, void * Msg, Word Len);
extern PACKAGE void __fastcall SHA1File(const System::ShortString &fname, Byte * Digest, void *buf, Word bsize, Word &Err);

}	/* namespace Zfsha1 */
using namespace Zfsha1;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFsha1
