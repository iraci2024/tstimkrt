// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_Types.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_typesHPP
#define Sgchttp_jwt_typesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_types
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTP_JWT_Algorithm : unsigned char { jwtNone, jwtHS256, jwtHS384, jwtHS512, jwtRS256, jwtRS384, jwtRS512, jwtES256, jwtES384, jwtES512 };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetAlgorithmString(TsgcHTTP_JWT_Algorithm aAlgorithm);
extern DELPHI_PACKAGE TsgcHTTP_JWT_Algorithm __fastcall GetAlgorithmFromString(const System::UnicodeString aValue);
}	/* namespace Sgchttp_jwt_types */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_TYPES)
using namespace Sgchttp_jwt_types;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_typesHPP
