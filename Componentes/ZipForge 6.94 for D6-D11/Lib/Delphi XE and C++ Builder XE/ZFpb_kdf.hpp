// CodeGear C++Builder
// Copyright (c) 1995, 2010 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFpb_kdf.pas' rev: 22.00

#ifndef Zfpb_kdfHPP
#define Zfpb_kdfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <ZFHash.hpp>	// Pascal unit
#include <ZFhmac.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfpb_kdf
{
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
static const System::ShortInt kdf_err_nil_pointer = 0x1;
static const System::ShortInt kdf_err_digestlen = 0x2;
extern PACKAGE int __fastcall kdf2(Zfhash::PHashDesc phash, void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
extern PACKAGE int __fastcall kdf2s(Zfhash::PHashDesc phash, System::AnsiString sPW, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);

}	/* namespace Zfpb_kdf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfpb_kdf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfpb_kdfHPP
