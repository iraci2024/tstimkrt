// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Google_Cloud.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_google_cloudHPP
#define Sgchttp_google_cloudHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcHTTP_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcHTTP_OAuth_Client.hpp>
#include <sgcHTTP_OAuth2_Client.hpp>
#include <sgcHTTP_JWT_Client.hpp>
#include <sgcHTTP_JWT_Types.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_google_cloud
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcGoogleCloudLogFile;
class DELPHICLASS TsgcHTTP_Google_Cloud_Base;
class DELPHICLASS TsgcHTTP_Google_Cloud_OAuth2_Options;
class DELPHICLASS TsgcHTTP_Google_Cloud_JWT_Options;
class DELPHICLASS TsgcHTTP_Google_Cloud_Options;
class DELPHICLASS TsgcHTTP_Google_Cloud_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcGoogleCloudAuthentication : unsigned char { gcaOAuth2, gcaJWT };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCloudLogFile : public Sgctcp_classes::TsgcTCPLogFile
{
	typedef Sgctcp_classes::TsgcTCPLogFile inherited;
	
public:
	/* TsgcSocketLogFile.Create */ inline __fastcall virtual TsgcGoogleCloudLogFile() : Sgctcp_classes::TsgcTCPLogFile() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcGoogleCloudLogFile() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_Base : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTP_Google_Cloud_Base(System::Classes::TComponent* AOwner) : Sgcbase_classes::TsgcComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTP_Google_Cloud_Base() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_OAuth2_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FClientId;
	System::UnicodeString FClientSecret;
	System::UnicodeString FLocalIP;
	int FLocalPort;
	System::UnicodeString FRedirectURL;
	System::Classes::TStringList* FScope;
	void __fastcall SetScope(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTP_Google_Cloud_OAuth2_Options();
	__fastcall virtual ~TsgcHTTP_Google_Cloud_OAuth2_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString ClientSecret = {read=FClientSecret, write=FClientSecret};
	__property System::UnicodeString LocalIP = {read=FLocalIP, write=FLocalIP};
	__property int LocalPort = {read=FLocalPort, write=FLocalPort, nodefault};
	__property System::UnicodeString RedirectURL = {read=FRedirectURL, write=FRedirectURL};
	__property System::Classes::TStringList* Scope = {read=FScope, write=SetScope};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_JWT_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm FAlgorithm;
	System::UnicodeString FAPI_Endpoint;
	System::UnicodeString FClientEmail;
	int FExpiry;
	System::Classes::TStringList* FPrivateKey;
	System::UnicodeString FPrivateKeyId;
	System::UnicodeString FProjectId;
	void __fastcall SetPrivateKey(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTP_Google_Cloud_JWT_Options();
	__fastcall virtual ~TsgcHTTP_Google_Cloud_JWT_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString ProjectId = {read=FProjectId, write=FProjectId};
	
__published:
	__property Sgchttp_jwt_types::TsgcHTTP_JWT_Algorithm Algorithm = {read=FAlgorithm, write=FAlgorithm, nodefault};
	__property System::UnicodeString API_Endpoint = {read=FAPI_Endpoint, write=FAPI_Endpoint};
	__property System::UnicodeString ClientEmail = {read=FClientEmail, write=FClientEmail};
	__property int Expiry = {read=FExpiry, write=FExpiry, nodefault};
	__property System::Classes::TStringList* PrivateKey = {read=FPrivateKey, write=SetPrivateKey};
	__property System::UnicodeString PrivateKeyId = {read=FPrivateKeyId, write=FPrivateKeyId};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcGoogleCloudAuthentication FAuthentication;
	TsgcHTTP_Google_Cloud_JWT_Options* FJWT;
	TsgcHTTP_Google_Cloud_OAuth2_Options* FOAuth2;
	void __fastcall SetJWT(TsgcHTTP_Google_Cloud_JWT_Options* const Value);
	void __fastcall SetOAuth2(TsgcHTTP_Google_Cloud_OAuth2_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTP_Google_Cloud_Options();
	__fastcall virtual ~TsgcHTTP_Google_Cloud_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcGoogleCloudAuthentication Authentication = {read=FAuthentication, write=FAuthentication, nodefault};
	__property TsgcHTTP_Google_Cloud_JWT_Options* JWT = {read=FJWT, write=SetJWT};
	__property TsgcHTTP_Google_Cloud_OAuth2_Options* OAuth2 = {read=FOAuth2, write=SetOAuth2};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_Client : public TsgcHTTP_Google_Cloud_Base
{
	typedef TsgcHTTP_Google_Cloud_Base inherited;
	
protected:
	System::UnicodeString __fastcall DoPathEncode(const System::UnicodeString aValue);
	System::UnicodeString __fastcall AddQueryParameter(const System::UnicodeString aURL, const System::UnicodeString aName, System::UnicodeString aValue)/* overload */;
	System::UnicodeString __fastcall AddQueryParameter(const System::UnicodeString aURL, const System::UnicodeString aName, int aValue)/* overload */;
	
private:
	Sgchttp_client::TsgcHTTPClient* FHTTPClient;
	Sgchttp_client::TsgcHTTPClient* __fastcall GetHTTPClient();
	
protected:
	virtual System::UnicodeString __fastcall DoGetScope() = 0 ;
	virtual System::UnicodeString __fastcall DoHTTP(const System::UnicodeString aMethod, const System::UnicodeString aURL, const System::UnicodeString aBody = System::UnicodeString());
	virtual void __fastcall OnAuthTokenEvent(System::TObject* Sender, const System::UnicodeString TokenType, const System::UnicodeString Token, const System::UnicodeString Data);
	virtual void __fastcall OnAuthTokenErrorEvent(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString ErrorDescription, const System::UnicodeString Data);
	__property Sgchttp_client::TsgcHTTPClient* HTTPClient = {read=GetHTTPClient, write=FHTTPClient};
	
private:
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* FOAuth2;
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* __fastcall GetOAuth2();
	
protected:
	__property Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* OAuth2 = {read=GetOAuth2, write=FOAuth2};
	
private:
	Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT* FJWT;
	Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT* __fastcall GetJWT();
	
protected:
	__property Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT* JWT = {read=GetJWT, write=FJWT};
	
private:
	TsgcGoogleCloudLogFile* FLogFile;
	void __fastcall SetLogFile(TsgcGoogleCloudLogFile* const Value);
	
public:
	__property TsgcGoogleCloudLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	__fastcall virtual TsgcHTTP_Google_Cloud_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_Google_Cloud_Client();
	
private:
	TsgcHTTP_Google_Cloud_Options* FGoogleCloudOptions;
	void __fastcall SetGoogleCloudOptions(TsgcHTTP_Google_Cloud_Options* const Value);
	
public:
	__property TsgcHTTP_Google_Cloud_Options* GoogleCloudOptions = {read=FGoogleCloudOptions, write=SetGoogleCloudOptions};
	
private:
	System::UnicodeString __fastcall GetPrivateKeyFromString(const System::UnicodeString aValue);
	
public:
	void __fastcall LoadSettingsFromFile(const System::UnicodeString aFileName);
	
private:
	Sgchttp_classes::TsgcOnAuthToken FOnAuthToken;
	Sgchttp_classes::TsgcOnAuthTokenError FOnAuthTokenError;
	
public:
	bool __fastcall RefreshToken(const System::UnicodeString aToken);
	__property Sgchttp_classes::TsgcOnAuthToken OnAuthToken = {read=FOnAuthToken, write=FOnAuthToken};
	__property Sgchttp_classes::TsgcOnAuthTokenError OnAuthTokenError = {read=FOnAuthTokenError, write=FOnAuthTokenError};
};


//-- var, const, procedure ---------------------------------------------------
#define CS_GOOGLE_CLOUD_HTTP_GET L"GET"
#define CS_GOOGLE_CLOUD_HTTP_POST L"POST"
#define CS_GOOGLE_CLOUD_HTTP_PUT L"PUT"
#define CS_GOOGLE_CLOUD_HTTP_DELETE L"DELETE"
#define CS_GOOGLE_CLOUD_HTTP_PATCH L"PATCH"
}	/* namespace Sgchttp_google_cloud */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_GOOGLE_CLOUD)
using namespace Sgchttp_google_cloud;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_google_cloudHPP
