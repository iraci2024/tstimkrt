// CodeGear C++Builder
// Copyright (c) 1995, 2013 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCsha1.pas' rev: 25.00 (Windows)

#ifndef Fxcsha1HPP
#define Fxcsha1HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <FXCHash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcsha1
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SHA1Init(Fxchash::THashContext &Context);
extern DELPHI_PACKAGE void __fastcall SHA1UpdateXL(Fxchash::THashContext &Context, void * Msg, int Len);
extern DELPHI_PACKAGE void __fastcall SHA1Update(Fxchash::THashContext &Context, void * Msg, System::Word Len);
extern DELPHI_PACKAGE void __fastcall SHA1FinalEx(Fxchash::THashContext &Context, Fxchash::THashDigest &Digest);
extern DELPHI_PACKAGE void __fastcall SHA1Final(Fxchash::THashContext &Context, Fxchash::TSHA1Digest &Digest);
extern DELPHI_PACKAGE bool __fastcall SHA1SelfTest(void);
extern DELPHI_PACKAGE void __fastcall SHA1FullXL(Fxchash::TSHA1Digest &Digest, void * Msg, int Len);
extern DELPHI_PACKAGE void __fastcall SHA1Full(Fxchash::TSHA1Digest &Digest, void * Msg, System::Word Len);
extern DELPHI_PACKAGE void __fastcall SHA1File(const System::ShortString &fname, Fxchash::TSHA1Digest &Digest, void *buf, System::Word bsize, System::Word &Err);
}	/* namespace Fxcsha1 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCSHA1)
using namespace Fxcsha1;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcsha1HPP
