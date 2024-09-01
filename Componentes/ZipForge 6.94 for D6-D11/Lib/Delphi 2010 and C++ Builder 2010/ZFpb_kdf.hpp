// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfpb_kdf.pas' rev: 21.00

#ifndef Zfpb_kdfHPP
#define Zfpb_kdfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Zfhash.hpp>	// Pascal unit
#include <Zfhmac.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfpb_kdf
{
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
static const ShortInt kdf_err_nil_pointer = 0x1;
static const ShortInt kdf_err_digestlen = 0x2;
extern PACKAGE int __fastcall kdf2(Zfhash::PHashDesc phash, void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
extern PACKAGE int __fastcall kdf2s(Zfhash::PHashDesc phash, System::AnsiString sPW, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);

}	/* namespace Zfpb_kdf */
using namespace Zfpb_kdf;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfpb_kdfHPP
