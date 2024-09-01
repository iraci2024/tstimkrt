// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_Helpers.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_helpersHPP
#define Sgchttp_jwt_helpersHPP

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
#include <System.NetEncoding.hpp>
#include <IdGlobal.hpp>
#include <IdCoderMIME.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_helpers
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcEncodeBase64(const System::DynamicArray<System::Byte> aBytes);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcURLEncode(const System::UnicodeString aValue)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcURLDecode(const System::UnicodeString aValue);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall sgcDecodeBase64(const System::UnicodeString aValue);
}	/* namespace Sgchttp_jwt_helpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_HELPERS)
using namespace Sgchttp_jwt_helpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_helpersHPP
