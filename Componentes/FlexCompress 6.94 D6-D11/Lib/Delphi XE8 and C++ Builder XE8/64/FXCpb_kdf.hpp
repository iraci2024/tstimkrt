// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCpb_kdf.pas' rev: 29.00 (Windows)

#ifndef Fxcpb_kdfHPP
#define Fxcpb_kdfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <FXCHash.hpp>
#include <FXChmac.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxcpb_kdf
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::AnsiString TKD_String;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 kdf_err_nil_pointer = System::Int8(0x1);
static const System::Int8 kdf_err_digestlen = System::Int8(0x2);
extern DELPHI_PACKAGE int __fastcall kdf2(Fxchash::PHashDesc phash, void * pPW, System::Word pLen, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
extern DELPHI_PACKAGE int __fastcall kdf2s(Fxchash::PHashDesc phash, System::AnsiString sPW, void * salt, System::Word sLen, int C, void *DK, System::Word dkLen);
}	/* namespace Fxcpb_kdf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCPB_KDF)
using namespace Fxcpb_kdf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcpb_kdfHPP
