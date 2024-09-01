// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFUWideStrUtils.pas' rev: 6.00

#ifndef ZFUWideStrUtilsHPP
#define ZFUWideStrUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfuwidestrutils
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE wchar_t * __fastcall WStrAlloc(unsigned Size);
extern PACKAGE unsigned __fastcall WStrBufSize(const wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WStrMove(wchar_t * Dest, const wchar_t * Source, unsigned Count);
extern PACKAGE wchar_t * __fastcall WStrNew(const wchar_t * Str);
extern PACKAGE void __fastcall WStrDispose(wchar_t * Str);
extern PACKAGE unsigned __fastcall WStrLen(wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WStrEnd(wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WStrCat(wchar_t * Dest, const wchar_t * Source);
extern PACKAGE wchar_t * __fastcall WStrCopy(wchar_t * Dest, wchar_t * Source);
extern PACKAGE wchar_t * __fastcall WStrLCopy(wchar_t * Dest, wchar_t * Source, unsigned MaxLen);
extern PACKAGE wchar_t * __fastcall WStrPCopy(wchar_t * Dest, const WideString Source);
extern PACKAGE wchar_t * __fastcall WStrPLCopy(wchar_t * Dest, const WideString Source, unsigned MaxLen);
extern PACKAGE wchar_t * __fastcall WStrScan(const wchar_t * Str, wchar_t Chr);
extern PACKAGE int __fastcall WStrComp(wchar_t * Str1, wchar_t * Str2);
extern PACKAGE wchar_t * __fastcall WStrPos(wchar_t * Str, wchar_t * SubStr);
extern PACKAGE int __fastcall Tnt_WStrComp(wchar_t * Str1, wchar_t * Str2);
extern PACKAGE wchar_t * __fastcall Tnt_WStrPos(wchar_t * Str, wchar_t * SubStr);
extern PACKAGE wchar_t * __fastcall WStrECopy(wchar_t * Dest, wchar_t * Source);
extern PACKAGE int __fastcall WStrLComp(wchar_t * Str1, wchar_t * Str2, unsigned MaxLen);
extern PACKAGE int __fastcall WStrLIComp(wchar_t * Str1, wchar_t * Str2, unsigned MaxLen);
extern PACKAGE int __fastcall WStrIComp(wchar_t * Str1, wchar_t * Str2);
extern PACKAGE wchar_t * __fastcall WStrLower(wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WStrUpper(wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WStrRScan(const wchar_t * Str, wchar_t Chr);
extern PACKAGE wchar_t * __fastcall WStrLCat(wchar_t * Dest, const wchar_t * Source, unsigned MaxLen);
extern PACKAGE WideString __fastcall WStrPas(const wchar_t * Str);
extern PACKAGE wchar_t * __fastcall WideLastChar(const WideString S);
extern PACKAGE WideString __fastcall WideQuotedStr(const WideString S, wchar_t Quote);
extern PACKAGE WideString __fastcall WideExtractQuotedStr(wchar_t * &Src, wchar_t Quote);
extern PACKAGE WideString __fastcall WideDequotedStr(const WideString S, wchar_t AQuote);

}	/* namespace Zfuwidestrutils */
using namespace Zfuwidestrutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFUWideStrUtils
