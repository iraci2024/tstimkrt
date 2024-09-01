// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcsha1.pas' rev: 21.00

#ifndef Fxcsha1HPP
#define Fxcsha1HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Fxchash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcsha1
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SHA1Init(Fxchash::THashContext &Context);
extern PACKAGE void __fastcall SHA1UpdateXL(Fxchash::THashContext &Context, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Update(Fxchash::THashContext &Context, void * Msg, System::Word Len);
extern PACKAGE void __fastcall SHA1FinalEx(Fxchash::THashContext &Context, System::Byte *Digest);
extern PACKAGE void __fastcall SHA1Final(Fxchash::THashContext &Context, System::Byte *Digest);
extern PACKAGE bool __fastcall SHA1SelfTest(void);
extern PACKAGE void __fastcall SHA1FullXL(System::Byte *Digest, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Full(System::Byte *Digest, void * Msg, System::Word Len);
extern PACKAGE void __fastcall SHA1File(const System::ShortString &fname, System::Byte *Digest, void *buf, System::Word bsize, System::Word &Err);

}	/* namespace Fxcsha1 */
using namespace Fxcsha1;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcsha1HPP
