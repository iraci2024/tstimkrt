// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_JWT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_jwt_clientHPP
#define Sgchttp_jwt_clientHPP

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
#include <sgcHTTP_Classes.hpp>
#include <sgcHTTP_JWT_Types.hpp>
#include <sgcHTTP_JWT_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcHTTP_JWT_RSA.hpp>
#include <sgcHTTP_JWT_ES.hpp>
#include <sgcHTTP_JWT_HMAC.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_jwt_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_JWT_Client_Algorithm_Options;
class DELPHICLASS TsgcHTTP_JWT_Client_HS_Options;
class DELPHICLASS TsgcHTTP_JWT_Client_RS_Options;
class DELPHICLASS TsgcHTTP_JWT_Client_ES_Options;
class DELPHICLASS TsgcHTTP_JWT_Client_Algorithms_Options;
class DELPHICLASS TsgcHTTP_JWT_Client_Options;
class DELPHICLASS TsgcHTTPComponentClient_JWT;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_Algorithm_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_Client_Algorithm_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_JWT_Client_Algorithm_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_HS_Options : public TsgcHTTP_JWT_Client_Algorithm_Options
{
	typedef TsgcHTTP_JWT_Client_Algorithm_Options inherited;
	
private:
	System::UnicodeString FSecret;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Secret = {read=FSecret, write=FSecret};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_Client_HS_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_JWT_Client_HS_Options() : TsgcHTTP_JWT_Client_Algorithm_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_RS_Options : public TsgcHTTP_JWT_Client_Algorithm_Options
{
	typedef TsgcHTTP_JWT_Client_Algorithm_Options inherited;
	
private:
	System::Classes::TStringList* FPrivateKey;
	void __fastcall SetPrivateKey(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTP_JWT_Client_RS_Options();
	__fastcall virtual ~TsgcHTTP_JWT_Client_RS_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::Classes::TStringList* PrivateKey = {read=FPrivateKey, write=SetPrivateKey};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_ES_Options : public TsgcHTTP_JWT_Client_Algorithm_Options
{
	typedef TsgcHTTP_JWT_Client_Algorithm_Options inherited;
	
private:
	System::Classes::TStringList* FPrivateKey;
	void __fastcall SetPrivateKey(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTP_JWT_Client_ES_Options();
	__fastcall virtual ~TsgcHTTP_JWT_Client_ES_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::Classes::TStringList* PrivateKey = {read=FPrivateKey, write=SetPrivateKey};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_Algorithms_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcHTTP_JWT_Client_ES_Options* FES;
	TsgcHTTP_JWT_Client_HS_Options* FHS;
	TsgcHTTP_JWT_Client_RS_Options* FRS;
	void __fastcall SetES(TsgcHTTP_JWT_Client_ES_Options* const Value);
	void __fastcall SetHS(TsgcHTTP_JWT_Client_HS_Options* const Value);
	void __fastcall SetRS(TsgcHTTP_JWT_Client_RS_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTP_JWT_Client_Algorithms_Options();
	__fastcall virtual ~TsgcHTTP_JWT_Client_Algorithms_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcHTTP_JWT_Client_ES_Options* ES = {read=FES, write=SetES};
	__property TsgcHTTP_JWT_Client_HS_Options* HS = {read=FHS, write=SetHS};
	__property TsgcHTTP_JWT_Client_RS_Options* RS = {read=FRS, write=SetRS};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcHTTP_JWT_Client_Algorithms_Options* FAlgorithms;
	Sgchttp_jwt_classes::TsgcHTTP_JWTHeader* FHeader;
	Sgcsocket_classes::TsgcSocketOpenSSL_Options* FOpenSSL_Options;
	Sgchttp_jwt_classes::TsgcHTTP_JWTPayload* FPayload;
	int FRefreshTokenAfter;
	Sgchttp_jwt_classes::TsgcHTTP_JWTHeader* __fastcall GetHeader();
	Sgchttp_jwt_classes::TsgcHTTP_JWTPayload* __fastcall GetPayload();
	void __fastcall SetAlgorithms(TsgcHTTP_JWT_Client_Algorithms_Options* const Value);
	void __fastcall SetOpenSSL_Options(Sgcsocket_classes::TsgcSocketOpenSSL_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTP_JWT_Client_Options();
	__fastcall virtual ~TsgcHTTP_JWT_Client_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcHTTP_JWT_Client_Algorithms_Options* Algorithms = {read=FAlgorithms, write=SetAlgorithms};
	__property Sgchttp_jwt_classes::TsgcHTTP_JWTHeader* Header = {read=GetHeader, write=FHeader};
	__property Sgcsocket_classes::TsgcSocketOpenSSL_Options* OpenSSL_Options = {read=FOpenSSL_Options, write=SetOpenSSL_Options};
	__property Sgchttp_jwt_classes::TsgcHTTP_JWTPayload* Payload = {read=GetPayload, write=FPayload};
	__property int RefreshTokenAfter = {read=FRefreshTokenAfter, write=FRefreshTokenAfter, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTPComponentClient_JWT : public Sgchttp_classes::TsgcHTTPComponentClientAuthToken_Base
{
	typedef Sgchttp_classes::TsgcHTTPComponentClientAuthToken_Base inherited;
	
private:
	Sgchttp_jwt_hmac::TsgcHTTP_JWT_HMAC* FJWTHMAC;
	Sgchttp_jwt_hmac::TsgcHTTP_JWT_HMAC* __fastcall GetJWTHMAC();
	
protected:
	__property Sgchttp_jwt_hmac::TsgcHTTP_JWT_HMAC* JWTHMAC = {read=GetJWTHMAC, write=FJWTHMAC};
	
private:
	Sgchttp_jwt_rsa::TsgcHTTP_JWT_RSA* FJWTRSA;
	Sgchttp_jwt_rsa::TsgcHTTP_JWT_RSA* __fastcall GetJWTRSA();
	
protected:
	__property Sgchttp_jwt_rsa::TsgcHTTP_JWT_RSA* JWTRSA = {read=GetJWTRSA, write=FJWTRSA};
	
private:
	Sgchttp_jwt_es::TsgcHTTP_JWT_ES* FJWTES;
	Sgchttp_jwt_es::TsgcHTTP_JWT_ES* __fastcall GetJWTES();
	
protected:
	__property Sgchttp_jwt_es::TsgcHTTP_JWT_ES* JWTES = {read=GetJWTES, write=FJWTES};
	
private:
	TsgcHTTP_JWT_Client_Options* FJWTOptions;
	void __fastcall SetJWTOptions(TsgcHTTP_JWT_Client_Options* const Value);
	
public:
	__property TsgcHTTP_JWT_Client_Options* JWTOptions = {read=FJWTOptions, write=SetJWTOptions};
	__fastcall virtual TsgcHTTPComponentClient_JWT(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTPComponentClient_JWT();
	
private:
	void __fastcall DoInitializeOpenSSL();
	System::DynamicArray<System::Byte> __fastcall GetValueToSign();
	System::UnicodeString __fastcall DoEncodeBase64(const System::UnicodeString aValue);
	
protected:
	virtual System::UnicodeString __fastcall DoSignHMAC();
	virtual System::UnicodeString __fastcall DoSignRSA();
	virtual System::UnicodeString __fastcall DoSignES();
	
public:
	void __fastcall Start();
	System::UnicodeString __fastcall Sign();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_jwt_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_JWT_CLIENT)
using namespace Sgchttp_jwt_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_jwt_clientHPP
