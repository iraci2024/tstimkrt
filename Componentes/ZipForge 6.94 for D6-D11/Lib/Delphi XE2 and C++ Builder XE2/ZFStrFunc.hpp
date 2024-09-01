// CodeGear C++Builder
// Copyright (c) 1995, 2011 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFStrFunc.pas' rev: 23.00 (Win32)

#ifndef ZfstrfuncHPP
#define ZfstrfuncHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfstrfunc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::WideChar WildCardMultipleChar = (System::WideChar)(0x2a);
static const System::WideChar WildCardSingleChar = (System::WideChar)(0x3f);
#define WildCardAnyFile L"*.*"
extern PACKAGE System::StaticArray<System::WideChar, 256> ToUpperChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> ToLowerChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> ToOemChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> ToAnsiChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> TurkishToAnsiChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> TurkishToUpperChars;
extern PACKAGE System::StaticArray<System::WideChar, 256> TurkishToLowerChars;
extern PACKAGE int __fastcall Q_PCompStr(System::WideChar * P1, System::WideChar * P2);
extern PACKAGE int __fastcall Q_CompStrL(const System::UnicodeString S1, const System::UnicodeString S2, unsigned MaxL);
extern PACKAGE int __fastcall Q_PCompText(System::WideChar * P1, System::WideChar * P2);
extern PACKAGE int __fastcall Q_CompTextL(const System::UnicodeString S1, const System::UnicodeString S2, unsigned MaxL);
extern PACKAGE System::WideChar * __fastcall Q_PStrToAnsi(System::WideChar * P);
extern PACKAGE System::WideChar * __fastcall Q_PStrToOem(System::WideChar * P);
extern PACKAGE int __fastcall Q_AnsiPCompStr(System::WideChar * P1, System::WideChar * P2);
extern PACKAGE int __fastcall Q_AnsiCompStrL(System::WideChar * P1, System::WideChar * P2, unsigned MaxL);
extern PACKAGE int __fastcall Q_AnsiPCompText(System::WideChar * P1, System::WideChar * P2);
extern PACKAGE int __fastcall Q_AnsiCompTextL(System::WideChar * P1, System::WideChar * P2, unsigned MaxL);
extern PACKAGE bool __fastcall IsStrMatchPattern(System::WideChar * StrPtr, System::WideChar * PatternPtr, bool bIgnoreCase = true);
extern PACKAGE bool __fastcall WIsStrMatchPattern(System::WideChar * StrPtr, System::WideChar * PatternPtr, bool bIgnoreCase = true);

}	/* namespace Zfstrfunc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFSTRFUNC)
using namespace Zfstrfunc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfstrfuncHPP
