// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_OAuth_Client.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_oauth_clientHPP
#define Sgchttp_oauth_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdHTTPServer.hpp>
#include <IdSocketHandle.hpp>
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <sgcHTTP_Classes.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_oauth_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPOAuthSSL_Options;
class DELPHICLASS TsgcHTTPOAuth_LocalServerOptions;
class DELPHICLASS TsgcHTTPOAuth_AuthorizationServer;
class DELPHICLASS TsgcHTTPComponentClient_OAuth;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPOAuthSSL_Options : public Sgcwebsocket_classes::TsgcWSSSL_Options
{
	typedef Sgcwebsocket_classes::TsgcWSSSL_Options inherited;
	
public:
	/* TsgcWSSSL_Options.Create */ inline __fastcall virtual TsgcHTTPOAuthSSL_Options() : Sgcwebsocket_classes::TsgcWSSSL_Options() { }
	/* TsgcWSSSL_Options.Destroy */ inline __fastcall virtual ~TsgcHTTPOAuthSSL_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPOAuth_LocalServerOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FIP;
	int FPort;
	System::UnicodeString FRedirectURL;
	bool FSSL;
	TsgcHTTPOAuthSSL_Options* FSSLOptions;
	void __fastcall SetSSLOptions(TsgcHTTPOAuthSSL_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPOAuth_LocalServerOptions();
	__fastcall virtual ~TsgcHTTPOAuth_LocalServerOptions();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	System::UnicodeString __fastcall GetURL();
	
__published:
	__property System::UnicodeString IP = {read=FIP, write=FIP};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::UnicodeString RedirectURL = {read=FRedirectURL, write=FRedirectURL};
	__property bool SSL = {read=FSSL, write=FSSL, nodefault};
	__property TsgcHTTPOAuthSSL_Options* SSLOptions = {read=FSSLOptions, write=SetSSLOptions};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPOAuth_AuthorizationServer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAuthURL;
	System::Classes::TStringList* FScope;
	System::UnicodeString FTokenURL;
	void __fastcall SetScope(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTPOAuth_AuthorizationServer();
	__fastcall virtual ~TsgcHTTPOAuth_AuthorizationServer();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	System::UnicodeString __fastcall GetScope();
	System::UnicodeString __fastcall GetScopeUnEncoded();
	
__published:
	__property System::UnicodeString AuthURL = {read=FAuthURL, write=FAuthURL};
	__property System::Classes::TStringList* Scope = {read=FScope, write=SetScope};
	__property System::UnicodeString TokenURL = {read=FTokenURL, write=FTokenURL};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTPComponentClient_OAuth : public Sgchttp_classes::TsgcHTTPComponentClientAuthToken_Base
{
	typedef Sgchttp_classes::TsgcHTTPComponentClientAuthToken_Base inherited;
	
protected:
	System::TObject* FHTTPServer;
	virtual void __fastcall OnCommandGetEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo) = 0 ;
	virtual void __fastcall DoStartServer();
	virtual void __fastcall DoStopServer();
	bool __fastcall DoGoToURL(const System::UnicodeString aURL);
	
public:
	virtual void __fastcall Start() = 0 ;
	virtual void __fastcall Stop();
	__fastcall virtual TsgcHTTPComponentClient_OAuth(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTPComponentClient_OAuth();
	
private:
	TsgcHTTPOAuth_AuthorizationServer* FAuthorizationServerOptions;
	TsgcHTTPOAuth_LocalServerOptions* FLocalServerOptions;
	void __fastcall SetAuthorizationServerOptions(TsgcHTTPOAuth_AuthorizationServer* const Value);
	void __fastcall SetLocalServerOptions(TsgcHTTPOAuth_LocalServerOptions* const Value);
	
public:
	__property TsgcHTTPOAuth_AuthorizationServer* AuthorizationServerOptions = {read=FAuthorizationServerOptions, write=SetAuthorizationServerOptions};
	__property TsgcHTTPOAuth_LocalServerOptions* LocalServerOptions = {read=FLocalServerOptions, write=SetLocalServerOptions};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_oauth_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_OAUTH_CLIENT)
using namespace Sgchttp_oauth_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_oauth_clientHPP
