// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_OAuth2_Client.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_oauth2_clientHPP
#define Sgchttp_oauth2_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcHTTP_OAuth_Client.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcHTTP_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_oauth2_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPOAuth2_Options;
class DELPHICLASS TsgcHTTPClient_Options;
class DELPHICLASS TsgcHTTPComponentClient_OAuth2;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcOnAuth2BeforeAuthorizeCode)(System::TObject* Sender, System::UnicodeString &URL, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2AfterAuthorizeCode)(System::TObject* Sender, const System::UnicodeString Code, const System::UnicodeString State, const System::UnicodeString Scope, const System::UnicodeString RawParams, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2ErrorAuthorizeCode)(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString State, const System::UnicodeString RawParams);

typedef void __fastcall (__closure *TsgcOnAuth2BeforeAccessToken)(System::TObject* Sender, System::UnicodeString &URL, System::UnicodeString &Parameters, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2AfterAccessToken)(System::TObject* Sender, const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2ErrorAccessToken)(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString RawParams);

typedef void __fastcall (__closure *TsgcOnAuth2BeforeRefreshToken)(System::TObject* Sender, System::UnicodeString &URL, System::UnicodeString &Parameters, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2AfterRefreshToken)(System::TObject* Sender, const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAuth2ErrorRefreshToken)(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString RawParams);

typedef void __fastcall (__closure *TsgcOnAuth2HTTPResponse)(System::TObject* Sender, int &Code, System::UnicodeString &Text);

enum DECLSPEC_DENUM TsgcOAuth2GrantTypes : unsigned char { auth2Code, auth2ClientCredentials };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPOAuth2_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FClientId;
	System::UnicodeString FClientSecret;
	TsgcOAuth2GrantTypes FGrantType;
	System::UnicodeString FPassword;
	System::UnicodeString FState;
	System::UnicodeString FUsername;
	System::UnicodeString __fastcall GetState();
	
public:
	__fastcall virtual TsgcHTTPOAuth2_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString State = {read=GetState};
	
__published:
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString ClientSecret = {read=FClientSecret, write=FClientSecret};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
	__property TsgcOAuth2GrantTypes GrantType = {read=FGrantType, write=FGrantType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPOAuth2_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPClient_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgchttp_client::TsgcHttpLogFile* FLogOptions;
	Sgchttp_client::TsgcIdHTTPTLS_Options* FTLSOptions;
	void __fastcall SetLogOptions(Sgchttp_client::TsgcHttpLogFile* const Value);
	void __fastcall SetTLSOptions(Sgchttp_client::TsgcIdHTTPTLS_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPClient_Options();
	__fastcall virtual ~TsgcHTTPClient_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgchttp_client::TsgcHttpLogFile* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property Sgchttp_client::TsgcIdHTTPTLS_Options* TLSOptions = {read=FTLSOptions, write=SetTLSOptions};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTPComponentClient_OAuth2 : public Sgchttp_oauth_client::TsgcHTTPComponentClient_OAuth
{
	typedef Sgchttp_oauth_client::TsgcHTTPComponentClient_OAuth inherited;
	
protected:
	virtual void __fastcall OnCommandGetEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	
public:
	__fastcall virtual TsgcHTTPComponentClient_OAuth2(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTPComponentClient_OAuth2();
	
private:
	Sgcbase_classes::TsgcTimer* FTimer;
	Sgcbase_classes::TsgcTimer* __fastcall GetTimer();
	void __fastcall DoStartTimer(int aInterval);
	
protected:
	virtual void __fastcall OnTimerEvent(System::TObject* Sender);
	virtual void __fastcall OnTimerExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
private:
	System::UnicodeString FRefreshToken;
	int FExpiresIn;
	System::UnicodeString __fastcall GetAuthorizationURL();
	void __fastcall DoAccessTokenRequest(const System::UnicodeString aCode, /* out */ int &aResponseCode, /* out */ System::UnicodeString &aResponseText)/* overload */;
	void __fastcall DoAccessTokenRequest()/* overload */;
	void __fastcall DoAccessTokenResponse(const System::UnicodeString aResponse);
	void __fastcall DoAccessTokenError(const System::UnicodeString aResponse = System::UnicodeString());
	void __fastcall DoRefreshTokenRequest(const System::UnicodeString aCode);
	void __fastcall DoRefreshTokenResponse(const System::UnicodeString aResponse);
	void __fastcall DoRefreshTokenError(const System::UnicodeString aResponse = System::UnicodeString());
	
protected:
	virtual void __fastcall DoAuthorizeCode();
	virtual void __fastcall DoAuthorization();
	
public:
	virtual void __fastcall Start();
	void __fastcall Refresh(const System::UnicodeString aRefreshToken);
	virtual void __fastcall Stop();
	
private:
	TsgcHTTPOAuth2_Options* FOAuth2Options;
	TsgcHTTPClient_Options* FHTTPClientOptions;
	void __fastcall SetHTTPClientOptions(TsgcHTTPClient_Options* const Value);
	void __fastcall SetOAuth2Options(TsgcHTTPOAuth2_Options* const Value);
	
public:
	__property TsgcHTTPClient_Options* HTTPClientOptions = {read=FHTTPClientOptions, write=SetHTTPClientOptions};
	__property TsgcHTTPOAuth2_Options* OAuth2Options = {read=FOAuth2Options, write=SetOAuth2Options};
	
private:
	TsgcOnAuth2AfterAuthorizeCode FOnAfterAuthorizeCode;
	TsgcOnAuth2BeforeAuthorizeCode FOnBeforeAuthorizeCode;
	TsgcOnAuth2ErrorAuthorizeCode FOnErrorAuthorizeCode;
	TsgcOnAuth2BeforeAccessToken FOnBeforeAccessToken;
	TsgcOnAuth2AfterAccessToken FOnAfterAccessToken;
	TsgcOnAuth2ErrorAccessToken FOnErrorAccessToken;
	TsgcOnAuth2BeforeRefreshToken FOnBeforeRefreshToken;
	TsgcOnAuth2AfterRefreshToken FOnAfterRefreshToken;
	TsgcOnAuth2ErrorRefreshToken FOnErrorRefreshToken;
	TsgcOnAuth2HTTPResponse FOnHTTPResponse;
	
protected:
	virtual bool __fastcall DoBeforeAuthorizeCodeEvent(System::UnicodeString &URL);
	virtual bool __fastcall DoAfterAuthorizeCodeEvent(const System::UnicodeString Code, const System::UnicodeString State, const System::UnicodeString Scope, const System::UnicodeString RawParams);
	void __fastcall DoErrorAuthorizeCodeEvent(const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString State, const System::UnicodeString RawParams);
	virtual bool __fastcall DoBeforeAccessTokenEvent(System::UnicodeString &URL, System::UnicodeString &Parameters);
	virtual bool __fastcall DoAfterAccessTokenEvent(const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams);
	void __fastcall DoErrorAccessTokenEvent(const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString RawParams);
	virtual bool __fastcall DoBeforeRefreshTokenEvent(System::UnicodeString &URL, System::UnicodeString &Parameters);
	virtual bool __fastcall DoAfterRefreshTokenEvent(const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams);
	virtual void __fastcall DoErrorRefreshTokenEvent(const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString RawParams);
	virtual void __fastcall DoHTTPResponseEvent(int &Code, System::UnicodeString &Text);
	
public:
	__property TsgcOnAuth2AfterAuthorizeCode OnAfterAuthorizeCode = {read=FOnAfterAuthorizeCode, write=FOnAfterAuthorizeCode};
	__property TsgcOnAuth2BeforeAuthorizeCode OnBeforeAuthorizeCode = {read=FOnBeforeAuthorizeCode, write=FOnBeforeAuthorizeCode};
	__property TsgcOnAuth2ErrorAuthorizeCode OnErrorAuthorizeCode = {read=FOnErrorAuthorizeCode, write=FOnErrorAuthorizeCode};
	__property TsgcOnAuth2AfterAccessToken OnAfterAccessToken = {read=FOnAfterAccessToken, write=FOnAfterAccessToken};
	__property TsgcOnAuth2BeforeAccessToken OnBeforeAccessToken = {read=FOnBeforeAccessToken, write=FOnBeforeAccessToken};
	__property TsgcOnAuth2ErrorAccessToken OnErrorAccessToken = {read=FOnErrorAccessToken, write=FOnErrorAccessToken};
	__property TsgcOnAuth2AfterRefreshToken OnAfterRefreshToken = {read=FOnAfterRefreshToken, write=FOnAfterRefreshToken};
	__property TsgcOnAuth2BeforeRefreshToken OnBeforeRefreshToken = {read=FOnBeforeRefreshToken, write=FOnBeforeRefreshToken};
	__property TsgcOnAuth2ErrorRefreshToken OnErrorRefreshToken = {read=FOnErrorRefreshToken, write=FOnErrorRefreshToken};
	__property TsgcOnAuth2HTTPResponse OnHTTPResponse = {read=FOnHTTPResponse, write=FOnHTTPResponse};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_oauth2_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_OAUTH2_CLIENT)
using namespace Sgchttp_oauth2_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_oauth2_clientHPP
