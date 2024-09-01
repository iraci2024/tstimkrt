// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Helpers.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_helpersHPP
#define Sgchttp_helpersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_helpers
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetParamsFromHeaders(System::Classes::TStringList* const aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcPathEncode(const System::UnicodeString aPath);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetBoundaryFromContentType(const System::UnicodeString aContentType);
}	/* namespace Sgchttp_helpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_HELPERS)
using namespace Sgchttp_helpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_helpersHPP
