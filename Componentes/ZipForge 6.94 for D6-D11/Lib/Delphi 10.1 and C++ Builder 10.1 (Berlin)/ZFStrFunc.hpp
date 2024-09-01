﻿// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFStrFunc.pas' rev: 31.00 (Windows)

#ifndef ZfstrfuncHPP
#define ZfstrfuncHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zfstrfunc
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::WideChar WildCardMultipleChar = (System::WideChar)(0x2a);
static const System::WideChar WildCardSingleChar = (System::WideChar)(0x3f);
#define WildCardAnyFile L"*.*"
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> ToUpperChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> ToLowerChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> ToOemChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> ToAnsiChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> TurkishToAnsiChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> TurkishToUpperChars;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 256> TurkishToLowerChars;
extern DELPHI_PACKAGE int __fastcall Q_PCompStr(System::WideChar * P1, System::WideChar * P2);
extern DELPHI_PACKAGE int __fastcall Q_CompStrL(const System::UnicodeString S1, const System::UnicodeString S2, unsigned MaxL);
extern DELPHI_PACKAGE int __fastcall Q_PCompText(System::WideChar * P1, System::WideChar * P2);
extern DELPHI_PACKAGE int __fastcall Q_CompTextL(const System::UnicodeString S1, const System::UnicodeString S2, unsigned MaxL);
extern DELPHI_PACKAGE System::WideChar * __fastcall Q_PStrToAnsi(System::WideChar * P);
extern DELPHI_PACKAGE System::WideChar * __fastcall Q_PStrToOem(System::WideChar * P);
extern DELPHI_PACKAGE int __fastcall Q_AnsiPCompStr(System::WideChar * P1, System::WideChar * P2);
extern DELPHI_PACKAGE int __fastcall Q_AnsiCompStrL(System::WideChar * P1, System::WideChar * P2, unsigned MaxL);
extern DELPHI_PACKAGE int __fastcall Q_AnsiPCompText(System::WideChar * P1, System::WideChar * P2);
extern DELPHI_PACKAGE int __fastcall Q_AnsiCompTextL(System::WideChar * P1, System::WideChar * P2, unsigned MaxL);
extern DELPHI_PACKAGE bool __fastcall IsStrMatchPattern(System::WideChar * StrPtr, System::WideChar * PatternPtr, bool bIgnoreCase = true);
extern DELPHI_PACKAGE bool __fastcall WIsStrMatchPattern(System::WideChar * StrPtr, System::WideChar * PatternPtr, bool bIgnoreCase = true);
}	/* namespace Zfstrfunc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFSTRFUNC)
using namespace Zfstrfunc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfstrfuncHPP
