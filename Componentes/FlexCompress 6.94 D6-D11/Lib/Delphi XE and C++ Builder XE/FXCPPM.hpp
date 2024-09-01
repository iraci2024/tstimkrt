// CodeGear C++Builder
// Copyright (c) 1995, 2010 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCPPM.pas' rev: 22.00

#ifndef FxcppmHPP
#define FxcppmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcppm
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::WideChar ppmd_version = (System::WideChar)(0x47);
extern PACKAGE System::StaticArray<System::Byte, 9> PPM_MO;
extern PACKAGE System::StaticArray<System::Byte, 9> PPM_SA;
extern PACKAGE void __fastcall PPMCompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf, /* out */ int &OutSize, int CompressionMode = 0x9);
extern PACKAGE void __fastcall PPMDecompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf, /* out */ int &OutSize);

}	/* namespace Fxcppm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcppm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcppmHPP
