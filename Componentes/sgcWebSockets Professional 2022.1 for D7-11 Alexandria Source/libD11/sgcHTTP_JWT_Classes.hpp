// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_classesHPP
#define Sgchttp_jwt_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdSSLOpenSSLHeaders.hpp>
#include <Winapi.Windows.hpp>
#include <sgcHTTP_JWT_Types.hpp>
#include <sgcJSON.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_JWTBase_JSON;
class DELPHICLASS TsgcHTTP_JWTHeader;
class DELPHICLASS TsgcHTTP_JWTPayload;
class DELPHICLASS TsgcHTTP_JWT_Base;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWTBase_JSON : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON};
	virtual System::UnicodeString __fastcall GetJSONText();
	virtual void __fastcall DoBuildJSON();
	
private:
	System::Classes::TStringList* FKeyValue;
	System::Classes::TStringList* __fastcall GetKeyValue();
	void __fastcall DoBuildJSONKeyValue();
	
public:
	void __fastcall ClearKeyValues();
	void __fastcall AddKeyValue(const System::UnicodeString aKey, const System::UnicodeString aValue)/* overload */;
	void __fastcall AddKeyValue(const System::UnicodeString aKey, bool aValue)/* overload */;
	__property System::UnicodeString JSONText = {read=GetJSONText};
	__fastcall virtual TsgcHTTP_JWTBase_JSON();
	__fastcall virtual ~TsgcHTTP_JWTBase_JSON();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWTHeader : public TsgcHTTP_JWTBase_JSON
{
	typedef TsgcHTTP_JWTBase_JSON inherited;
	
private:
	Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm Falg;
	System::UnicodeString Fkid;
	System::UnicodeString Ftyp;
	
protected:
	virtual void __fastcall DoBuildJSON();
	
public:
	__fastcall virtual TsgcHTTP_JWTHeader();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm alg = {read=Falg, write=Falg, nodefault};
	__property System::UnicodeString kid = {read=Fkid, write=Fkid};
	__property System::UnicodeString typ = {read=Ftyp, write=Ftyp};
public:
	/* TsgcHTTP_JWTBase_JSON.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWTHeader() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_JWTPayload : public TsgcHTTP_JWTBase_JSON
{
	typedef TsgcHTTP_JWTBase_JSON inherited;
	
private:
	System::UnicodeString Faud;
	__int64 Fexp;
	__int64 Fiat;
	__int64 Fnbf;
	System::UnicodeString Fiss;
	System::UnicodeString Fsub;
	System::UnicodeString Fjti;
	
protected:
	virtual void __fastcall DoBuildJSON();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString aud = {read=Faud, write=Faud};
	__property __int64 exp = {read=Fexp, write=Fexp};
	__property __int64 iat = {read=Fiat, write=Fiat};
	__property __int64 nbf = {read=Fnbf, write=Fnbf};
	__property System::UnicodeString iss = {read=Fiss, write=Fiss};
	__property System::UnicodeString jti = {read=Fjti, write=Fjti};
	__property System::UnicodeString sub = {read=Fsub, write=Fsub};
public:
	/* TsgcHTTP_JWTBase_JSON.Create */ inline __fastcall virtual TsgcHTTP_JWTPayload() : TsgcHTTP_JWTBase_JSON() { }
	/* TsgcHTTP_JWTBase_JSON.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWTPayload() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Base : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_JWT_Base() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_Base() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_jwt_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_CLASSES)
using namespace Sgchttp_jwt_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_classesHPP
