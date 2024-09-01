// CodeGear C++Builder
// Copyright (c) 1995, 2011 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFsha1.pas' rev: 23.00 (Win32)

#ifndef Zfsha1HPP
#define Zfsha1HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfsha1
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SHA1Init(Zfhash::THashContext &Context);
extern PACKAGE void __fastcall SHA1UpdateXL(Zfhash::THashContext &Context, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Update(Zfhash::THashContext &Context, void * Msg, System::Word Len);
extern PACKAGE void __fastcall SHA1FinalEx(Zfhash::THashContext &Context, System::Byte *Digest);
extern PACKAGE void __fastcall SHA1Final(Zfhash::THashContext &Context, System::Byte *Digest);
extern PACKAGE bool __fastcall SHA1SelfTest(void);
extern PACKAGE void __fastcall SHA1FullXL(System::Byte *Digest, void * Msg, int Len);
extern PACKAGE void __fastcall SHA1Full(System::Byte *Digest, void * Msg, System::Word Len);
extern PACKAGE void __fastcall SHA1File(const System::ShortString &fname, System::Byte *Digest, void *buf, System::Word bsize, System::Word &Err);

}	/* namespace Zfsha1 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFSHA1)
using namespace Zfsha1;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfsha1HPP
