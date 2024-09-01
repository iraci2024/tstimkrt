// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_ES.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_esHPP
#define Sgchttp_jwt_esHPP

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
#include <IdGlobal.hpp>
#include <Winapi.Windows.hpp>
#include <sgcHTTP_JWT_Classes.hpp>
#include <sgcHTTP_JWT_Types.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_es
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_JWT_ES;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_ES : public Sgchttp_jwt_classes::TsgcHTTP_JWT_Base
{
	typedef Sgchttp_jwt_classes::TsgcHTTP_JWT_Base inherited;
	
private:
	System::DynamicArray<System::Byte> __fastcall DoConvertRS(void * aSignature, Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm aAlgorithm);
	void * __fastcall DoSignatureRS(const System::DynamicArray<System::Byte> aSignature);
	void * __fastcall LoadPublicKey(const System::UnicodeString aValue);
	
public:
	System::DynamicArray<System::Byte> __fastcall SignES256(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	System::DynamicArray<System::Byte> __fastcall SignES384(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	System::DynamicArray<System::Byte> __fastcall SignES512(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey);
	
protected:
	virtual void __fastcall DoLoadOpenSSLMethods();
	virtual System::DynamicArray<System::Byte> __fastcall DoSignES(System::DynamicArray<System::Byte> aValue, System::DynamicArray<System::Byte> aKey, Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm aAlgorithm);
	virtual bool __fastcall DoValidateES(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aPublicKey, Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm aAlgorithm);
	
public:
	bool __fastcall ValidateES256(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aPublicKey);
	bool __fastcall ValidateES384(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aPublicKey);
	bool __fastcall ValidateES512(const System::UnicodeString aHeader, const System::UnicodeString aPayload, const System::UnicodeString aSignature, const System::UnicodeString aPublicKey);
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_JWT_ES() : Sgchttp_jwt_classes::TsgcHTTP_JWT_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_ES() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_jwt_es */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_ES)
using namespace Sgchttp_jwt_es;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_esHPP
