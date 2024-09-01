// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_HMAC.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_hmacHPP
#define Sgchttp_jwt_hmacHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdSSLOpenSSLHeaders.hpp>
#include <IdSSLOpenSSL.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcHTTP_JWT_Classes.hpp>
#include <sgcHTTP_JWT_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_hmac
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_JWT_HMAC;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_HMAC : public Sgchttp_jwt_classes::TsgcHTTP_JWT_Base
{
	typedef Sgchttp_jwt_classes::TsgcHTTP_JWT_Base inherited;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoSignHMAC(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aSecret, Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm aAlgorithm);
	bool __fastcall DoValidateHMAC(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aSecret, Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm aAlgorithm);
	
public:
	System::DynamicArray<System::Byte> __fastcall SignHMAC256(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	System::DynamicArray<System::Byte> __fastcall SignHMAC384(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	System::DynamicArray<System::Byte> __fastcall SignHMAC512(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	bool __fastcall ValidateHMAC256(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aSecret);
	bool __fastcall ValidateHMAC384(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aSecret);
	bool __fastcall ValidateHMAC512(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aSecret);
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_JWT_HMAC() : Sgchttp_jwt_classes::TsgcHTTP_JWT_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_HMAC() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_jwt_hmac */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_HMAC)
using namespace Sgchttp_jwt_hmac;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_hmacHPP
