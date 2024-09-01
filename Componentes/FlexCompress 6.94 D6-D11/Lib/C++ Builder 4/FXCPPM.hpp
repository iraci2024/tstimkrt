// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCPPM.pas' rev: 4.00

#ifndef FXCPPMHPP
#define FXCPPMHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Windows.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcppm
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const char ppmd_version = '\x47';
extern PACKAGE Byte PPM_MO[9];
extern PACKAGE Byte PPM_SA[9];
extern PACKAGE void __fastcall PPMCompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf
	, /* out */ int &OutSize, int CompressionMode);
extern PACKAGE void __fastcall PPMDecompressBuf(const void * InBuf, int InSize, /* out */ void * &OutBuf
	, /* out */ int &OutSize);

}	/* namespace Fxcppm */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcppm;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCPPM
