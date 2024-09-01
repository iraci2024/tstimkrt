// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFpb_kdf.pas' rev: 24.00 (Windows)

#ifndef Zfpb_kdfHPP
#define Zfpb_kdfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
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
static const System::Int8 kdf_err_nil_pointer = System::Int8(0x1);
static const System::Int8 kdf_err_digestlen = System::Int8(0x2);
extern DELPHI_PACKAGE int __fastcall kdf2(Zfhash::PHashDesc phash, void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
extern DELPHI_PACKAGE int __fastcall kdf2s(Zfhash::PHashDesc phash, System::AnsiString sPW, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
}	/* namespace Zfpb_kdf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFPB_KDF)
using namespace Zfpb_kdf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfpb_kdfHPP
