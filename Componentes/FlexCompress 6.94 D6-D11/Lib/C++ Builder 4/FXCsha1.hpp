// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCsha1.pas' rev: 4.00

#ifndef FXCsha1HPP
#define FXCsha1HPP

#pragma delphiheader begin
#pragma option push -w-
#include <FXCHash.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcsha1
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SHA1Init(Fxchash::THashContext &Context);
extern PACKAGE void __fastcall SHA1UpdateXL(Fxchash::THashContext &Context, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Update(Fxchash::THashContext &Context, void * Msg, Word Len);
extern PACKAGE void __fastcall SHA1FinalEx(Fxchash::THashContext &Context, Byte * Digest);
extern PACKAGE void __fastcall SHA1Final(Fxchash::THashContext &Context, Byte * Digest);
extern PACKAGE bool __fastcall SHA1SelfTest(void);
extern PACKAGE void __fastcall SHA1FullXL(Byte * Digest, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Full(Byte * Digest, void * Msg, Word Len);
extern PACKAGE void __fastcall SHA1File(const System::ShortString &fname, Byte * Digest, void *buf, 
	Word bsize, Word &Err);

}	/* namespace Fxcsha1 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcsha1;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCsha1
