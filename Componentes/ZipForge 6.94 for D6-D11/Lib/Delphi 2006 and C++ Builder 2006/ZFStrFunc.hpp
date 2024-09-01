// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfstrfunc.pas' rev: 10.00

#ifndef ZfstrfuncHPP
#define ZfstrfuncHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfstrfunc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const char WildCardMultipleChar = '\x2a';
static const char WildCardSingleChar = '\x3f';
#define WildCardAnyFile "*.*"
extern PACKAGE char ToUpperChars[256];
extern PACKAGE char ToLowerChars[256];
extern PACKAGE char ToOemChars[256];
extern PACKAGE char ToAnsiChars[256];
extern PACKAGE char TurkishToAnsiChars[256];
extern PACKAGE char TurkishToUpperChars[256];
extern PACKAGE char TurkishToLowerChars[256];
extern PACKAGE int __fastcall Q_PCompStr(char * P1, char * P2);
extern PACKAGE int __fastcall Q_CompStrL(const AnsiString S1, const AnsiString S2, unsigned MaxL);
extern PACKAGE int __fastcall Q_PCompText(char * P1, char * P2);
extern PACKAGE int __fastcall Q_CompTextL(const AnsiString S1, const AnsiString S2, unsigned MaxL);
extern PACKAGE char * __fastcall Q_PStrToAnsi(char * P);
extern PACKAGE char * __fastcall Q_PStrToOem(char * P);
extern PACKAGE int __fastcall Q_AnsiPCompStr(char * P1, char * P2);
extern PACKAGE int __fastcall Q_AnsiCompStrL(char * P1, char * P2, unsigned MaxL);
extern PACKAGE int __fastcall Q_AnsiPCompText(char * P1, char * P2);
extern PACKAGE int __fastcall Q_AnsiCompTextL(char * P1, char * P2, unsigned MaxL);
extern PACKAGE bool __fastcall IsStrMatchPattern(char * StrPtr, char * PatternPtr, bool bIgnoreCase = true);
extern PACKAGE bool __fastcall WIsStrMatchPattern(WideChar * StrPtr, WideChar * PatternPtr, bool bIgnoreCase = true);

}	/* namespace Zfstrfunc */
using namespace Zfstrfunc;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfstrfunc
