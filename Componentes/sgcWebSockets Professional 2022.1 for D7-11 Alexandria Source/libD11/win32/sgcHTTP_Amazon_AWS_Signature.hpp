// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Amazon_AWS_Signature.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_amazon_aws_signatureHPP
#define Sgchttp_amazon_aws_signatureHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdGlobal.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_amazon_aws_signature
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_Amazon_AWS_Signature;
class DELPHICLASS TsgcHTTP_Amazon_AWS_Signature_V4_Options;
class DELPHICLASS TsgcHTTP_Amazon_AWS_Signature_V4;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTP_SignatureV4_Protocol : unsigned char { sv4pHTTP, sv4pMQTT };

class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Signature : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
private:
	System::DynamicArray<System::Byte> __fastcall GetHMACSHA256_Hash(const System::UnicodeString aValue, System::DynamicArray<System::Byte> aSecret);
	System::DynamicArray<System::Byte> __fastcall GetHMACSHA256_Indy(const System::UnicodeString aValue, Idglobal::TIdBytes aSecret);
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall GetHMACSHA256(const System::UnicodeString aValue, System::DynamicArray<System::Byte> aSecret);
	
private:
	System::UnicodeString __fastcall GetHASHSHA256_Hash(const System::UnicodeString aValue);
	System::UnicodeString __fastcall GetHASHSHA256_Indy(const System::UnicodeString aValue);
	
protected:
	virtual System::UnicodeString __fastcall GetHASHSHA256(const System::UnicodeString aValue);
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTP_Amazon_AWS_Signature(System::Classes::TComponent* AOwner) : Sgcbase_classes::TsgcComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTP_Amazon_AWS_Signature() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Signature_V4_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAccessKey;
	System::UnicodeString FRegion;
	System::UnicodeString FSecretKey;
	System::UnicodeString FService;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString AccessKey = {read=FAccessKey, write=FAccessKey};
	__property System::UnicodeString Region = {read=FRegion, write=FRegion};
	__property System::UnicodeString SecretKey = {read=FSecretKey, write=FSecretKey};
	__property System::UnicodeString Service = {read=FService, write=FService};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP_Amazon_AWS_Signature_V4_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_Amazon_AWS_Signature_V4_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Signature_V4 : public TsgcHTTP_Amazon_AWS_Signature
{
	typedef TsgcHTTP_Amazon_AWS_Signature inherited;
	
private:
	TsgcHTTP_Amazon_AWS_Signature_V4_Options* FSignatureV4Options;
	void __fastcall SetSignatureV4Options(TsgcHTTP_Amazon_AWS_Signature_V4_Options* const Value);
	
public:
	__property TsgcHTTP_Amazon_AWS_Signature_V4_Options* SignatureV4Options = {read=FSignatureV4Options, write=SetSignatureV4Options};
	__fastcall virtual TsgcHTTP_Amazon_AWS_Signature_V4(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_Amazon_AWS_Signature_V4();
	
private:
	System::UnicodeString FRequestDate;
	System::UnicodeString FSignedHeaders;
	System::UnicodeString FHashedCanonicalRequest;
	System::UnicodeString FStringToSign;
	System::UnicodeString FSignature;
	TsgcHTTP_SignatureV4_Protocol FProtocol;
	System::UnicodeString __fastcall GetCanonicalRequest(const System::UnicodeString aHTTPRequestMethod, const System::UnicodeString aCanonicalURI, const System::UnicodeString aCanonicalQueryString, const System::UnicodeString aCanonicalHeaders, const System::UnicodeString aSignedHeaders, const System::UnicodeString aRequestPayload);
	void __fastcall DoCanonicalRequest(const System::UnicodeString aHTTPRequestMethod, const System::UnicodeString aURL, const System::UnicodeString aRequestPayload);
	void __fastcall DoStringToSign(const System::UnicodeString aRegion, const System::UnicodeString aService);
	void __fastcall DoCalculateSignature(const System::UnicodeString aRegion, const System::UnicodeString aService);
	
protected:
	virtual void __fastcall DoSign(const System::UnicodeString aRegion, const System::UnicodeString aService, const System::UnicodeString aHTTPRequestMethod, const System::UnicodeString aURL, const System::UnicodeString aRequestPayload);
	virtual System::UnicodeString __fastcall GetAuthorizationHeader();
	virtual System::UnicodeString __fastcall GetSignature();
	
public:
	void __fastcall SignGET(const System::UnicodeString aURL);
	void __fastcall SignPOST(const System::UnicodeString aURL, const System::UnicodeString aRequestPayload);
	__property System::UnicodeString RequestDate = {read=FRequestDate};
	__property System::UnicodeString AuthorizationHeader = {read=GetAuthorizationHeader};
	__property System::UnicodeString Signature = {read=GetSignature};
	__property TsgcHTTP_SignatureV4_Protocol Protocol = {read=FProtocol, write=FProtocol, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
#define CS_AWS_SIGV4_ALGORITHM L"AWS4-HMAC-SHA256"
}	/* namespace Sgchttp_amazon_aws_signature */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_AMAZON_AWS_SIGNATURE)
using namespace Sgchttp_amazon_aws_signature;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_amazon_aws_signatureHPP
