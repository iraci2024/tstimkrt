// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcppm.pas' rev: 10.00

#ifndef FxcppmHPP
#define FxcppmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcppm
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const char ppmd_version = '\x47';
extern PACKAGE Byte PPM_MO[9];
extern PACKAGE Byte PPM_SA[9];
extern PACKAGE void __fastcall PPMCompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf, /* out */ int &OutSize, int CompressionMode = 0x9);
extern PACKAGE void __fastcall PPMDecompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf, /* out */ int &OutSize);

}	/* namespace Fxcppm */
using namespace Fxcppm;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcppm
