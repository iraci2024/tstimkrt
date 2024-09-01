// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server_Base.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_server_baseHPP
#define Sgcwebsocket_server_baseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server_base
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TServerThread;
class DELPHICLASS TsgcWSBinding;
class DELPHICLASS TsgcWSBindings;
class DELPHICLASS TsgcWSSSL_Options_Base;
class DELPHICLASS TsgcWSSecurity_Options;
class DELPHICLASS TsgcWSQueueServer_Options;
class DELPHICLASS TsgcWSAuthenticationServer_Session;
class DELPHICLASS TsgcWSAuthenticationServer_URL;
class DELPHICLASS TsgcWSAuthenticationServer_Basic;
class DELPHICLASS TsgcWSAuthenticationServer_OAuth;
class DELPHICLASS TsgcWSAuthenticationServer_JWT;
class DELPHICLASS TsgcWSFallBack_Flash;
class DELPHICLASS TsgcHTTPUploadFilesServer;
class DELPHICLASS TsgcHTTP2ServerSettings;
class DELPHICLASS TsgcWSFallBack_SSE;
class DELPHICLASS TsgcWSFallBack_Options;
class DELPHICLASS TsgcHTTP2ServerEvents;
class DELPHICLASS TsgcWSHTTP2AlternateService;
class DELPHICLASS TsgcWSHTTP2Server_Options;
class DELPHICLASS TsgcWSOptionsServer;
class DELPHICLASS TsgcWSConnectionAuthentication;
class DELPHICLASS TsgcWSAuthenticationServer_Options;
class DELPHICLASS TsgcWSHandShakeServer_Base;
class DELPHICLASS TsgcWSConnectionServer_Base;
class DELPHICLASS TsgcWSServer_Base;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSAuthenticationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString aUser, System::UnicodeString aPassword, bool &Authenticated);

typedef void __fastcall (__closure *TsgcWSHTTPUploadBeforeSaveFileEvent)(System::TObject* Sender, System::UnicodeString &aFileName, System::UnicodeString &aFilePath);

typedef void __fastcall (__closure *TsgcWSHTTPUploadAfterSaveFileEvent)(System::TObject* Sender, const System::UnicodeString aFileName, const System::UnicodeString aFilePath);

class PASCALIMPLEMENTATION TServerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcWSServer_Base* FServer;
	System::UnicodeString FMethod;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TServerThread(const System::UnicodeString aMethod, TsgcWSServer_Base* const aServer);
	__classmethod void __fastcall Start(TsgcWSServer_Base* const aServer);
	__classmethod void __fastcall Stop(TsgcWSServer_Base* const aServer);
	__classmethod void __fastcall ReStart(TsgcWSServer_Base* const aServer);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TServerThread() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBinding : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHost;
	Sgcwebsocket_types::TwsIPVersion FIPVersion;
	System::UnicodeString FParameters;
	int FPort;
	bool FSSL;
	System::UnicodeString FSSLCertStoreName;
	System::UnicodeString FSSLHash;
	
public:
	__fastcall TsgcWSBinding();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property Sgcwebsocket_types::TwsIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__property System::UnicodeString Parameters = {read=FParameters, write=FParameters};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool SSL = {read=FSSL, write=FSSL, nodefault};
	__property System::UnicodeString SSLCertStoreName = {read=FSSLCertStoreName, write=FSSLCertStoreName};
	__property System::UnicodeString SSLHash = {read=FSSLHash, write=FSSLHash};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSBinding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBindings : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	virtual void __fastcall Clear();
	void __fastcall NewBinding(const System::UnicodeString aHost, const int aPort, const System::UnicodeString aParameters = System::UnicodeString(), const bool aSSL = false, const System::UnicodeString aSSLHash = System::UnicodeString(), const Sgcwebsocket_types::TwsIPVersion aIPVersion = (Sgcwebsocket_types::TwsIPVersion)(0x0), const System::UnicodeString aCertStoreName = System::UnicodeString())/* overload */;
	void __fastcall NewBinding(TsgcWSBinding* const aBinding)/* overload */;
	System::UnicodeString __fastcall GetURL(const int aIndex);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSBindings() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSBindings() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSSL_Options_Base : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSSL_Options_Base() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSSSL_Options_Base() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSecurity_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FOriginsAllowed;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString OriginsAllowed = {read=FOriginsAllowed, write=FOriginsAllowed};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSecurity_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSSecurity_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueueServer_Options : public Sgcwebsocket_classes::TsgcWSQueueOptions
{
	typedef Sgcwebsocket_classes::TsgcWSQueueOptions inherited;
	
public:
	/* TsgcWSQueueOptions.Create */ inline __fastcall virtual TsgcWSQueueServer_Options() : Sgcwebsocket_classes::TsgcWSQueueOptions() { }
	/* TsgcWSQueueOptions.Destroy */ inline __fastcall virtual ~TsgcWSQueueServer_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_Session : public Sgcwebsocket_classes::TsgcWSAuthentication_Session
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Session inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationServer_Session() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationServer_Session() : Sgcwebsocket_classes::TsgcWSAuthentication_Session() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_URL : public Sgcwebsocket_classes::TsgcWSAuthentication_URL
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_URL inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationServer_URL() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationServer_URL() : Sgcwebsocket_classes::TsgcWSAuthentication_URL() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_Basic : public Sgcwebsocket_classes::TsgcWSAuthentication_Basic
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Basic inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationServer_Basic() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationServer_Basic() : Sgcwebsocket_classes::TsgcWSAuthentication_Basic() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_OAuth : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::TObject* FOAuth2;
	
public:
	__fastcall virtual TsgcWSAuthenticationServer_OAuth();
	
__published:
	__property System::TObject* OAuth2 = {read=FOAuth2, write=FOAuth2};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationServer_OAuth() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_JWT : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::TObject* FJWT;
	
public:
	__fastcall virtual TsgcWSAuthenticationServer_JWT();
	
__published:
	__property System::TObject* JWT = {read=FJWT, write=FJWT};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationServer_JWT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFallBack_Flash : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDomain;
	bool FEnabled;
	System::UnicodeString FPorts;
	
public:
	__fastcall TsgcWSFallBack_Flash();
	
__published:
	__property System::UnicodeString Domain = {read=FDomain, write=FDomain};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Ports = {read=FPorts, write=FPorts};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFallBack_Flash() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTPUploadFilesServer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	__int64 FMinSize;
	bool FRemoveBoundaries;
	System::UnicodeString FSaveDirectory;
	Sgcwebsocket_types::TwsPostStreamType FStreamType;
	void __fastcall SetStreamType(const Sgcwebsocket_types::TwsPostStreamType Value);
	
public:
	__fastcall virtual TsgcHTTPUploadFilesServer();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property __int64 MinSize = {read=FMinSize, write=FMinSize};
	__property bool RemoveBoundaries = {read=FRemoveBoundaries, write=FRemoveBoundaries, nodefault};
	__property System::UnicodeString SaveDirectory = {read=FSaveDirectory, write=FSaveDirectory};
	__property Sgcwebsocket_types::TwsPostStreamType StreamType = {read=FStreamType, write=SetStreamType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPUploadFilesServer() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP2ServerSettings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnableConnectProtocol;
	bool FEnablePush;
	int FHeaderTableSize;
	int FInitialWindowSize;
	int FMaxConcurrentStreams;
	int FMaxFrameSize;
	int FMaxHeaderListSize;
	
public:
	__fastcall virtual TsgcHTTP2ServerSettings();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool EnableConnectProtocol = {read=FEnableConnectProtocol, write=FEnableConnectProtocol, nodefault};
	
__published:
	__property bool EnablePush = {read=FEnablePush, write=FEnablePush, nodefault};
	__property int HeaderTableSize = {read=FHeaderTableSize, write=FHeaderTableSize, nodefault};
	__property int InitialWindowSize = {read=FInitialWindowSize, write=FInitialWindowSize, nodefault};
	__property int MaxConcurrentStreams = {read=FMaxConcurrentStreams, write=FMaxConcurrentStreams, nodefault};
	__property int MaxFrameSize = {read=FMaxFrameSize, write=FMaxFrameSize, nodefault};
	__property int MaxHeaderListSize = {read=FMaxHeaderListSize, write=FMaxHeaderListSize, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP2ServerSettings() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFallBack_SSE : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FRetry;
	
public:
	__fastcall TsgcWSFallBack_SSE();
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Retry = {read=FRetry, write=FRetry, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFallBack_SSE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFallBack_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSFallBack_Flash* FFlash;
	TsgcWSFallBack_SSE* FServerSentEvents;
	
protected:
	void __fastcall SetServerSentEvents(TsgcWSFallBack_SSE* const Value);
	void __fastcall SetFlash(TsgcWSFallBack_Flash* const Value);
	
public:
	__fastcall virtual TsgcWSFallBack_Options();
	__fastcall virtual ~TsgcWSFallBack_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSFallBack_Flash* Flash = {read=FFlash, write=SetFlash};
	__property TsgcWSFallBack_SSE* ServerSentEvents = {read=FServerSentEvents, write=SetServerSentEvents};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTP2ServerEvents : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FOnConnect;
	bool FOnDisconnect;
	
public:
	__fastcall virtual TsgcHTTP2ServerEvents();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool OnConnect = {read=FOnConnect, write=FOnConnect, nodefault};
	__property bool OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTP2ServerEvents() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHTTP2AlternateService : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	__fastcall virtual TsgcWSHTTP2AlternateService();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSHTTP2AlternateService() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHTTP2Server_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSHTTP2AlternateService* FAltSvc;
	bool FEnabled;
	TsgcHTTP2ServerEvents* FEvents;
	Sgcwebsocket_types::Th2FragmentedData FFragmentedData;
	TsgcHTTP2ServerSettings* FSettings;
	bool __fastcall GetEnabled();
	void __fastcall SetAltSvc(TsgcWSHTTP2AlternateService* const Value);
	void __fastcall SetEvents(TsgcHTTP2ServerEvents* const Value);
	void __fastcall SetSettings(TsgcHTTP2ServerSettings* const Value);
	
public:
	__fastcall virtual TsgcWSHTTP2Server_Options();
	__fastcall virtual ~TsgcWSHTTP2Server_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSHTTP2AlternateService* AltSvc = {read=FAltSvc, write=SetAltSvc};
	__property bool Enabled = {read=GetEnabled, write=FEnabled, nodefault};
	__property TsgcHTTP2ServerEvents* Events = {read=FEvents, write=SetEvents};
	__property Sgcwebsocket_types::Th2FragmentedData FragmentedData = {read=FFragmentedData, write=FFragmentedData, nodefault};
	__property TsgcHTTP2ServerSettings* Settings = {read=FSettings, write=SetSettings};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSOptionsServer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FCleanDisconnect;
	Sgcwebsocket_types::TwsFragmentedMessages FFragmentedMessages;
	bool FHTMLFiles;
	bool FJavascriptFiles;
	int FReadTimeOut;
	bool FRaiseDisconnectExceptions;
	bool FValidateUTF8;
	int FWriteTimeOut;
	int __fastcall GetReadTimeOut();
	
public:
	__fastcall virtual TsgcWSOptionsServer();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool CleanDisconnect = {read=FCleanDisconnect, write=FCleanDisconnect, nodefault};
	__property Sgcwebsocket_types::TwsFragmentedMessages FragmentedMessages = {read=FFragmentedMessages, write=FFragmentedMessages, nodefault};
	__property bool HTMLFiles = {read=FHTMLFiles, write=FHTMLFiles, nodefault};
	__property bool JavascriptFiles = {read=FJavascriptFiles, write=FJavascriptFiles, nodefault};
	__property int ReadTimeOut = {read=GetReadTimeOut, write=FReadTimeOut, nodefault};
	__property int WriteTimeOut = {read=FWriteTimeOut, write=FWriteTimeOut, nodefault};
	__property bool RaiseDisconnectExceptions = {read=FRaiseDisconnectExceptions, write=FRaiseDisconnectExceptions, nodefault};
	__property bool ValidateUTF8 = {read=FValidateUTF8, write=FValidateUTF8, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSOptionsServer() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSConnectionAuthentication : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAuthenticated;
	Sgcwebsocket_types::TwsAuthentication FAuthType;
	System::UnicodeString FPassword;
	System::UnicodeString FUser;
	System::UnicodeString FSessionID;
	System::UnicodeString __fastcall GetSessionID();
	
public:
	__fastcall TsgcWSConnectionAuthentication();
	__property bool Authenticated = {read=FAuthenticated, write=FAuthenticated, nodefault};
	__property Sgcwebsocket_types::TwsAuthentication AuthType = {read=FAuthType, write=FAuthType, nodefault};
	__property System::UnicodeString SessionID = {read=GetSessionID, write=FSessionID};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString User = {read=FUser, write=FUser};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSConnectionAuthentication() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationServer_Options : public Sgcwebsocket_classes::TsgcWSAuthentication_Options
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Options inherited;
	
private:
	bool FAllowNonAuth;
	System::Classes::TStringList* FAuthUsers;
	TsgcWSAuthenticationServer_URL* FURL;
	TsgcWSAuthenticationServer_Session* FSession;
	TsgcWSAuthenticationServer_Basic* FBasic;
	TsgcWSAuthenticationServer_JWT* FJWT;
	TsgcWSAuthenticationServer_OAuth* FOAuth;
	
protected:
	void __fastcall SetAuthUsers(System::Classes::TStringList* const Value);
	void __fastcall SetURL(TsgcWSAuthenticationServer_URL* const Value);
	void __fastcall SetSession(TsgcWSAuthenticationServer_Session* const Value);
	void __fastcall SetBasic(TsgcWSAuthenticationServer_Basic* const Value);
	void __fastcall SetOAuth(TsgcWSAuthenticationServer_OAuth* const Value);
	void __fastcall SetJWT(TsgcWSAuthenticationServer_JWT* const Value);
	
public:
	__fastcall virtual TsgcWSAuthenticationServer_Options();
	__fastcall virtual ~TsgcWSAuthenticationServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool AllowNonAuth = {read=FAllowNonAuth, write=FAllowNonAuth, nodefault};
	__property System::Classes::TStringList* AuthUsers = {read=FAuthUsers, write=SetAuthUsers};
	__property TsgcWSAuthenticationServer_URL* URL = {read=FURL, write=SetURL};
	__property TsgcWSAuthenticationServer_Session* Session = {read=FSession, write=SetSession};
	__property TsgcWSAuthenticationServer_Basic* Basic = {read=FBasic, write=SetBasic};
	__property TsgcWSAuthenticationServer_JWT* JWT = {read=FJWT, write=SetJWT};
	__property TsgcWSAuthenticationServer_OAuth* OAuth = {read=FOAuth, write=SetOAuth};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHandShakeServer_Base : public Sgcwebsocket_classes_indy::TsgcWSHandshake
{
	typedef Sgcwebsocket_classes_indy::TsgcWSHandshake inherited;
	
protected:
	System::UnicodeString FGet;
	System::UnicodeString FHost;
	System::UnicodeString FOrigin;
	System::UnicodeString FKey;
	System::UnicodeString FVersion;
	
public:
	__property System::UnicodeString Get = {read=FGet};
	__property System::UnicodeString Host = {read=FHost};
	__property System::UnicodeString Origin = {read=FOrigin};
	__property System::UnicodeString Key = {read=FKey};
	__property System::UnicodeString Version = {read=FVersion};
	
private:
	Sgcwebsocket_types::TwsSpecification __fastcall GetSpecification();
	
public:
	__property Sgcwebsocket_types::TwsSpecification Specification = {read=GetSpecification, nodefault};
	
protected:
	bool FIsConnection;
	bool FIsGET;
	bool FIsHost;
	bool FIsKey;
	bool FIsOrigin;
	bool FIsProtocols;
	bool FIsUpgrade;
	bool FIsVersion;
	bool FIsExtensions;
	
private:
	void __fastcall DoDecodeHeaders(System::Classes::TStringList* aHeaders);
	
protected:
	void __fastcall DoDecodeConnection(const System::UnicodeString aHeader);
	void __fastcall DoDecodeGET(const System::UnicodeString aHeader);
	void __fastcall DoDecodeHost(const System::UnicodeString aHeader);
	void __fastcall DoDecodeKey(const System::UnicodeString aHeader);
	void __fastcall DoDecodeOrigin(const System::UnicodeString aHeader);
	void __fastcall DoDecodeProtocols(const System::UnicodeString aHeader);
	void __fastcall DoDecodeUpgrade(const System::UnicodeString aHeader);
	void __fastcall DoDecodeVersion(const System::UnicodeString aHeader);
	void __fastcall DoDecodeExtensions(const System::UnicodeString aHeader);
	bool FIsKey1;
	bool FIsKey2;
	System::UnicodeString FKey1;
	System::UnicodeString FKey2;
	System::UnicodeString FKey3;
	void __fastcall DoDecodeKey1(const System::UnicodeString aHeader);
	void __fastcall DoDecodeKey2(const System::UnicodeString aHeader);
	
public:
	__fastcall virtual TsgcWSHandShakeServer_Base(System::Classes::TStringList* aHeaders);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHandShakeServer_Base() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSConnectionServer_Base : public Sgcwebsocket_classes_indy::TsgcWSConnection_Base_Indy
{
	typedef Sgcwebsocket_classes_indy::TsgcWSConnection_Base_Indy inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	System::UnicodeString __fastcall DoGetProtocol();
	virtual void __fastcall DoReadHeadersRequest() = 0 ;
	
private:
	void __fastcall DoOriginsAllowed();
	
protected:
	void __fastcall DoHandShake_RFC6455();
	void __fastcall DoHandShake_Hixie76();
	virtual TsgcWSHandShakeServer_Base* __fastcall GetHandShake() = 0 ;
	virtual void __fastcall DoHandShakeExtensions();
	virtual void __fastcall DoHandShakeSecurity();
	virtual void __fastcall DoHandshake();
	
public:
	__property TsgcWSHandShakeServer_Base* HandShake = {read=GetHandShake};
	
protected:
	virtual void __fastcall DoProtocolSSE();
	
private:
	TsgcWSConnectionAuthentication* FAuthentication;
	TsgcWSAuthenticationEvent FOnAuthentication;
	
protected:
	TsgcWSConnectionAuthentication* __fastcall GetAuthentication();
	virtual void __fastcall DoOnAuthenticationEvent();
	int FAuthenticationCloseCode;
	virtual void __fastcall DoHTTPAuthRealm() = 0 ;
	
private:
	bool __fastcall DoHTTPFileResponse(const System::UnicodeString aFileName, const System::UnicodeString aContentType, bool aDisconnect);
	
protected:
	void __fastcall DoAuthSessionResponse(const System::UnicodeString aParams);
	bool __fastcall DoAuthenticationURL(const System::UnicodeString aParams);
	bool __fastcall DoAuthenticationBasic(const System::UnicodeString aParams);
	
public:
	void __fastcall SetAuthenticationCloseCode(const int aCloseCode);
	__property TsgcWSConnectionAuthentication* Authentication = {read=GetAuthentication, write=FAuthentication};
	__property TsgcWSAuthenticationEvent OnAuthentication = {read=FOnAuthentication, write=FOnAuthentication};
	
private:
	bool FCustomHeartBeat;
	System::UnicodeString FServerOriginsAllowed;
	
public:
	__property System::UnicodeString ServerOriginsAllowed = {read=FServerOriginsAllowed, write=FServerOriginsAllowed};
	__fastcall virtual TsgcWSConnectionServer_Base();
	__fastcall virtual ~TsgcWSConnectionServer_Base();
};


class PASCALIMPLEMENTATION TsgcWSServer_Base : public Sgcwebsocket_classes::TsgcWSComponent_Server
{
	typedef Sgcwebsocket_classes::TsgcWSComponent_Server inherited;
	
public:
	TsgcWSConnectionServer_Base* operator[](int Index) { return this->Connections[Index]; }
	
protected:
	virtual Sgcwebsocket_classes::TsgcWSLogFile* __fastcall GetLogFile();
	
public:
	virtual System::Classes::TList* __fastcall LockList();
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeHeartBeatEvent FOnBeforeHeartBeat;
	
protected:
	virtual void __fastcall OnHeartBeatTimeoutEvent(System::TObject* Sender);
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatTimeoutExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeHeartBeatEvent OnBeforeHeartBeat = {read=FOnBeforeHeartBeat, write=FOnBeforeHeartBeat};
	
private:
	TsgcWSBindings* FBindings;
	TsgcWSBindings* __fastcall GetBindings();
	
public:
	__property TsgcWSBindings* Bindings = {read=GetBindings, write=FBindings};
	
protected:
	virtual void __fastcall OnClientUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aType);
	virtual void __fastcall OnServerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall OnServerBinaryEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream);
	virtual void __fastcall OnServerFragmentedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aData, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	virtual void __fastcall OnServerDisconnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aErrorCode);
	virtual void __fastcall OnHTTPUploadBeforeSaveFileEvent(System::TObject* Sender, System::UnicodeString &aFileName, System::UnicodeString &aFilePath);
	virtual void __fastcall OnHTTPUploadAfterSaveFileEvent(System::TObject* Sender, const System::UnicodeString aFileName, const System::UnicodeString aFilePath);
	virtual bool __fastcall DoBuiltInLibraries(TsgcWSConnectionServer_Base* aConnection, const System::UnicodeString aText, bool aDisconnect);
	
private:
	void __fastcall DoReadHeadersRequest(TsgcWSConnectionServer_Base* aConnection);
	
protected:
	virtual unsigned __fastcall GetHTTPStatusCode(TsgcWSConnectionServer_Base* aConnection);
	virtual TsgcWSConnectionServer_Base* __fastcall GetConnectionByIndex(int Index);
	virtual TsgcWSConnectionServer_Base* __fastcall GetConnectionByGuid(const System::UnicodeString aGUID);
	virtual int __fastcall GetCount();
	virtual void __fastcall AddConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall RemoveConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall ClearConnections();
	void __fastcall DoDisconnectAll();
	
public:
	void __fastcall DisconnectAll();
	__property TsgcWSConnectionServer_Base* ConnectionsByGUID[const System::UnicodeString Guid] = {read=GetConnectionByGuid};
	__property TsgcWSConnectionServer_Base* Connections[int Index] = {read=GetConnectionByIndex/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	virtual System::Classes::TList* __fastcall ShareList() = 0 ;
	virtual void __fastcall UnShareList() = 0 ;
	
private:
	TsgcWSOptionsServer* FOptions;
	TsgcWSSecurity_Options* FSecurityOptions;
	TsgcWSQueueServer_Options* FQueueOptions;
	TsgcHTTPUploadFilesServer* FHTTPUploadFiles;
	void __fastcall SetQueueOptions(TsgcWSQueueServer_Options* const Value);
	void __fastcall SetSecurityOptions(TsgcWSSecurity_Options* const Value);
	void __fastcall SetOptions(TsgcWSOptionsServer* const Value);
	void __fastcall SetHTTPUploadFiles(TsgcHTTPUploadFilesServer* const Value);
	
public:
	__property TsgcWSOptionsServer* Options = {read=FOptions, write=SetOptions};
	__property TsgcWSQueueServer_Options* QueueOptions = {read=FQueueOptions, write=SetQueueOptions};
	__property TsgcWSSecurity_Options* SecurityOptions = {read=FSecurityOptions, write=SetSecurityOptions};
	__property TsgcHTTPUploadFilesServer* HTTPUploadFiles = {read=FHTTPUploadFiles, write=SetHTTPUploadFiles};
	
protected:
	virtual int __fastcall GetPort() = 0 ;
	virtual System::UnicodeString __fastcall GetHost() = 0 ;
	virtual void __fastcall _SetPort(const int Value) = 0 ;
	virtual void __fastcall SetHost(const System::UnicodeString Value) = 0 ;
	
public:
	__property System::UnicodeString Host = {read=GetHost, write=SetHost};
	__property int Port = {read=GetPort, write=_SetPort, nodefault};
	
private:
	Sgcwebsocket_helpers::TsgcDelimitedStringList* FSessionIDList;
	TsgcWSAuthenticationServer_Options* FAuthentication;
	
protected:
	TsgcWSAuthenticationEvent FOnAuthentication;
	Sgcwebsocket_helpers::TsgcDelimitedStringList* __fastcall GetSessionIDList();
	void __fastcall DoAddSessionID(const System::UnicodeString aSessionID, const System::UnicodeString aUser, const System::UnicodeString aPassword);
	void __fastcall DoDelSessionID(const System::UnicodeString aSessionID);
	bool __fastcall GetSessionID(const System::UnicodeString aSessionID);
	bool __fastcall GetSessionUserPassword(const System::UnicodeString aSessionID, System::UnicodeString &User, System::UnicodeString &Password);
	void __fastcall SetAuthentication(TsgcWSAuthenticationServer_Options* const Value);
	bool __fastcall DoAuthenticationSession(TsgcWSConnectionServer_Base* aConnection, const System::UnicodeString aParams);
	
public:
	void __fastcall OnServerAuthenticationEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString aUser, System::UnicodeString aPassword, bool &Authenticated);
	__property TsgcWSAuthenticationServer_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
protected:
	void __fastcall DoXHR(TsgcWSConnectionServer_Base* const aConnection);
	virtual void __fastcall DoHTTPProtocol(TsgcWSConnectionServer_Base* const aConnection);
	
private:
	TsgcWSFallBack_Options* FFallBack;
	
protected:
	void __fastcall SetFallBack(TsgcWSFallBack_Options* const Value);
	
public:
	__property TsgcWSFallBack_Options* FallBack = {read=FFallBack, write=SetFallBack};
	
private:
	System::UnicodeString FWatchDogMonitorSecret;
	void __fastcall DoWatchDogMonitor();
	
protected:
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnOAuth2ResponseEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aContent, System::UnicodeString aContentType);
	virtual void __fastcall OnAuth2ResponseErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall OnOAuth2AuthenticationEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aUser, System::UnicodeString aPassword, bool &Authenticated);
	bool FActive;
	virtual bool __fastcall GetActive();
	virtual void __fastcall SetActive(const bool Value);
	virtual void __fastcall DoStart();
	virtual void __fastcall DoStop();
	
public:
	void __fastcall Start();
	void __fastcall Stop();
	void __fastcall ReStart();
	__property bool Active = {read=GetActive, write=SetActive, default=0};
	
private:
	bool FSSL;
	
protected:
	virtual bool __fastcall GetSSL();
	virtual void __fastcall SetSSL(const bool Value);
	
public:
	__property bool SSL = {read=GetSSL, write=SetSSL, default=0};
	
private:
	void __fastcall DoClear();
	
public:
	__fastcall virtual TsgcWSServer_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSServer_Base();
	
protected:
	virtual void __fastcall DoBroadCast(const System::UnicodeString aMessage, System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0));
	
public:
	virtual void __fastcall Broadcast(const System::UnicodeString aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall Broadcast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
private:
	bool __fastcall DoWriteData(const System::UnicodeString aGUID, const System::UnicodeString aMessage)/* overload */;
	bool __fastcall DoWriteData(const System::UnicodeString aGUID, System::Classes::TStream* const aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	virtual bool __fastcall WriteData(const System::UnicodeString aGUID, const System::UnicodeString aMessage)/* overload */;
	virtual bool __fastcall WriteData(const System::UnicodeString aGUID, System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
protected:
	virtual void __fastcall DoPing();
	
public:
	void __fastcall Ping();
	
protected:
	System::Classes::TNotifyEvent FOnStartup;
	System::Classes::TNotifyEvent FOnShutdown;
	TsgcWSHTTPUploadBeforeSaveFileEvent FOnHTTPUploadBeforeSaveFile;
	TsgcWSHTTPUploadAfterSaveFileEvent FOnHTTPUploadAfterSaveFile;
	
public:
	__property System::Classes::TNotifyEvent OnStartup = {read=FOnStartup, write=FOnStartup};
	__property System::Classes::TNotifyEvent OnShutdown = {read=FOnShutdown, write=FOnShutdown};
	__property TsgcWSAuthenticationEvent OnAuthentication = {read=FOnAuthentication, write=FOnAuthentication};
	__property TsgcWSHTTPUploadBeforeSaveFileEvent OnHTTPUploadBeforeSaveFile = {read=FOnHTTPUploadBeforeSaveFile, write=FOnHTTPUploadBeforeSaveFile};
	__property TsgcWSHTTPUploadAfterSaveFileEvent OnHTTPUploadAfterSaveFile = {read=FOnHTTPUploadAfterSaveFile, write=FOnHTTPUploadAfterSaveFile};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server_base */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER_BASE)
using namespace Sgcwebsocket_server_base;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_server_baseHPP
