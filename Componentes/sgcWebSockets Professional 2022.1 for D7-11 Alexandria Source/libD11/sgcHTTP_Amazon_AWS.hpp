// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Amazon_AWS.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_amazon_awsHPP
#define Sgchttp_amazon_awsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcHTTP_Amazon_AWS_Signature.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_amazon_aws
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_Amazon_AWS_Base;
class DELPHICLASS TsgcAWSLogFile;
class DELPHICLASS TsgcHTTP_Amazon_AWS_Options;
class DELPHICLASS TsgcHTTP_Amazon_AWS_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Base : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTP_Amazon_AWS_Base(System::Classes::TComponent* AOwner) : Sgcbase_classes::TsgcComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTP_Amazon_AWS_Base() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAWSLogFile : public Sgctcp_classes::TsgcTCPLogFile
{
	typedef Sgctcp_classes::TsgcTCPLogFile inherited;
	
public:
	/* TsgcSocketLogFile.Create */ inline __fastcall virtual TsgcAWSLogFile() : Sgctcp_classes::TsgcTCPLogFile() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcAWSLogFile() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAccessKey;
	System::UnicodeString FRegion;
	System::UnicodeString FSecretKey;
	System::UnicodeString FService;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString Service = {read=FService, write=FService};
	
__published:
	__property System::UnicodeString AccessKey = {read=FAccessKey, write=FAccessKey};
	__property System::UnicodeString Region = {read=FRegion, write=FRegion};
	__property System::UnicodeString SecretKey = {read=FSecretKey, write=FSecretKey};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP_Amazon_AWS_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTP_Amazon_AWS_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_Client : public TsgcHTTP_Amazon_AWS_Base
{
	typedef TsgcHTTP_Amazon_AWS_Base inherited;
	
private:
	Sgchttp_amazon_aws_signature::TsgcHTTP_Amazon_AWS_Signature_V4* FSignature;
	Sgchttp_amazon_aws_signature::TsgcHTTP_Amazon_AWS_Signature_V4* __fastcall GetSignature();
	void __fastcall DoLogOptions();
	void __fastcall DoSignatureOptions();
	void __fastcall DoAuthorizedRequest(const System::UnicodeString aURL);
	
protected:
	__property Sgchttp_amazon_aws_signature::TsgcHTTP_Amazon_AWS_Signature_V4* Signature = {read=GetSignature, write=FSignature};
	virtual bool __fastcall DoBeforeGET_Request(System::UnicodeString &aURL);
	virtual void __fastcall DoGetAuthorization(const System::UnicodeString aURL);
	virtual void __fastcall DoPostAuthorization(const System::UnicodeString aURL);
	
public:
	bool __fastcall GetURL(const System::UnicodeString aURL, System::UnicodeString &aResponse, System::UnicodeString &aError);
	bool __fastcall PostURL(const System::UnicodeString aURL, System::UnicodeString &aResponse, System::UnicodeString &aError);
	
private:
	Sgchttp_client::TsgcIdHTTP* FHTTPClient;
	Sgchttp_client::TsgcIdHTTP* __fastcall GetHTTPClient();
	
protected:
	__property Sgchttp_client::TsgcIdHTTP* HTTPClient = {read=GetHTTPClient, write=FHTTPClient};
	
private:
	TsgcHTTP_Amazon_AWS_Options* FAWSOptions;
	void __fastcall SetAWSOptions(TsgcHTTP_Amazon_AWS_Options* const Value);
	
public:
	__property TsgcHTTP_Amazon_AWS_Options* AWSOptions = {read=FAWSOptions, write=SetAWSOptions};
	
private:
	TsgcAWSLogFile* FLogFile;
	void __fastcall SetLogFile(TsgcAWSLogFile* const Value);
	
public:
	__property TsgcAWSLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	__fastcall virtual TsgcHTTP_Amazon_AWS_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_Amazon_AWS_Client();
	
private:
	System::UnicodeString __fastcall GetEndPoint();
	
public:
	__property System::UnicodeString EndPoint = {read=GetEndPoint};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_amazon_aws */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_AMAZON_AWS)
using namespace Sgchttp_amazon_aws;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_amazon_awsHPP
