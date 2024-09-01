// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP.pas' rev: 35.00 (Windows)

#ifndef SgchttpHPP
#define SgchttpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcHTTP_OAuth2_Client.hpp>
#include <sgcHTTP_JWT_Client.hpp>
#include <sgcHTTP_Amazon_AWS_Signature.hpp>
#include <sgcHTTP_Amazon_SQS.hpp>
#include <sgcHTTP_Google_PubSub.hpp>
#include <sgcHTTP_Google_Calendar.hpp>
#include <sgcHTTP_OAuth_Client.hpp>
#include <sgcHTTP_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcHTTP_Amazon_AWS.hpp>
#include <sgcHTTP_Google_Cloud.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_OAuth2_Client;
class DELPHICLASS TsgcHTTP_JWT_Client;
class DELPHICLASS TsgcHTTPAWS_SQS_Client;
class DELPHICLASS TsgcHTTPGoogleCloud_PubSub_Client;
class DELPHICLASS TsgcHTTPGoogleCloud_Calendar_Client;
class DELPHICLASS TsgcHTTP1Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcHTTP_OAuth2_Client : public Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2
{
	typedef Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2 inherited;
	
__published:
	__property LocalServerOptions;
	__property AuthorizationServerOptions;
	__property OAuth2Options;
	__property HTTPClientOptions;
	__property OnBeforeAuthorizeCode;
	__property OnAfterAuthorizeCode;
	__property OnErrorAuthorizeCode;
	__property OnBeforeAccessToken;
	__property OnAfterAccessToken;
	__property OnErrorAccessToken;
	__property OnBeforeRefreshToken;
	__property OnAfterRefreshToken;
	__property OnErrorRefreshToken;
	__property OnHTTPResponse;
	__property Version = {default=0};
public:
	/* TsgcHTTPComponentClient_OAuth2.Create */ inline __fastcall virtual TsgcHTTP_OAuth2_Client(System::Classes::TComponent* aOwner) : Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2(aOwner) { }
	/* TsgcHTTPComponentClient_OAuth2.Destroy */ inline __fastcall virtual ~TsgcHTTP_OAuth2_Client() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_JWT_Client : public Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT
{
	typedef Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT inherited;
	
__published:
	__property JWTOptions;
	__property Version = {default=0};
public:
	/* TsgcHTTPComponentClient_JWT.Create */ inline __fastcall virtual TsgcHTTP_JWT_Client(System::Classes::TComponent* aOwner) : Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT(aOwner) { }
	/* TsgcHTTPComponentClient_JWT.Destroy */ inline __fastcall virtual ~TsgcHTTP_JWT_Client() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPAWS_SQS_Client : public Sgchttp_amazon_sqs::TsgcHTTP_Amazon_AWS_SQS_Client
{
	typedef Sgchttp_amazon_sqs::TsgcHTTP_Amazon_AWS_SQS_Client inherited;
	
__published:
	__property AWSOptions;
	__property LogFile;
	__property OnSQSBeforeRequest;
	__property OnSQSResponse;
	__property OnSQSError;
	__property Version = {default=0};
public:
	/* TsgcHTTP_Amazon_AWS_SQS_Client.Create */ inline __fastcall virtual TsgcHTTPAWS_SQS_Client(System::Classes::TComponent* aOwner) : Sgchttp_amazon_sqs::TsgcHTTP_Amazon_AWS_SQS_Client(aOwner) { }
	/* TsgcHTTP_Amazon_AWS_SQS_Client.Destroy */ inline __fastcall virtual ~TsgcHTTPAWS_SQS_Client() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPGoogleCloud_PubSub_Client : public Sgchttp_google_pubsub::TsgcHTTP_Google_Cloud_PubSub_Client
{
	typedef Sgchttp_google_pubsub::TsgcHTTP_Google_Cloud_PubSub_Client inherited;
	
__published:
	__property GoogleCloudOptions;
	__property LogFile;
	__property OnAuthToken;
	__property OnAuthTokenError;
	__property Version = {default=0};
public:
	/* TsgcHTTP_Google_Cloud_PubSub_Client.Create */ inline __fastcall virtual TsgcHTTPGoogleCloud_PubSub_Client(System::Classes::TComponent* aOwner) : Sgchttp_google_pubsub::TsgcHTTP_Google_Cloud_PubSub_Client(aOwner) { }
	
public:
	/* TsgcHTTP_Google_Cloud_Client.Destroy */ inline __fastcall virtual ~TsgcHTTPGoogleCloud_PubSub_Client() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPGoogleCloud_Calendar_Client : public Sgchttp_google_calendar::TsgcHTTP_Google_Cloud_Calendar_Client
{
	typedef Sgchttp_google_calendar::TsgcHTTP_Google_Cloud_Calendar_Client inherited;
	
__published:
	__property GoogleCloudOptions;
	__property LogFile;
	__property Scopes;
	__property OnAuthToken;
	__property OnAuthTokenError;
	__property OnGetCalendar;
	__property OnGetCalendarEvent;
	__property OnError;
	__property Version = {default=0};
public:
	/* TsgcHTTP_Google_Cloud_Calendar_Client.Destroy */ inline __fastcall virtual ~TsgcHTTPGoogleCloud_Calendar_Client() { }
	
public:
	/* TsgcHTTP_Google_Cloud_Calendar_API_Client.Create */ inline __fastcall virtual TsgcHTTPGoogleCloud_Calendar_Client(System::Classes::TComponent* aOwner) : Sgchttp_google_calendar::TsgcHTTP_Google_Cloud_Calendar_Client(aOwner) { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP1Client : public Sgchttp_client::TsgcIdHTTP
{
	typedef Sgchttp_client::TsgcIdHTTP inherited;
	
__published:
	__property Authentication;
	__property Log;
	__property LogOptions;
	__property TLSOptions;
	__property Version;
public:
	/* TsgcIdHTTP.Destroy */ inline __fastcall virtual ~TsgcHTTP1Client() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcHTTP1Client(System::Classes::TComponent* AOwner)/* overload */ : Sgchttp_client::TsgcIdHTTP(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcHTTP1Client()/* overload */ : Sgchttp_client::TsgcIdHTTP() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP)
using namespace Sgchttp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgchttpHPP
