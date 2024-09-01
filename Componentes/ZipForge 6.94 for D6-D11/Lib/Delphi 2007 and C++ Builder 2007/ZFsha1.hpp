// CodeGear C++Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfsha1.pas' rev: 11.00

#ifndef Zfsha1HPP
#define Zfsha1HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfhash.hpp>	// Pascal unit

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
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfsha1
