// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_SocketIO.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_socketioHPP
#define Sgcwebsocket_api_socketioHPP

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
#include <IdSSL.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_socketio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSSocketIOHTTPRequest;
class DELPHICLASS TsgcWSSocketIO_Options;
class DELPHICLASS TsgcWS_API_SocketIO;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSocketIOHTTPRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* FHeaders;
	System::Classes::TStringList* __fastcall GetHeaders();
	
public:
	__property System::Classes::TStringList* Headers = {read=GetHeaders, write=FHeaders};
	__fastcall virtual ~TsgcWSSocketIOHTTPRequest();
public:
	/* TObject.Create */ inline __fastcall TsgcWSSocketIOHTTPRequest() : System::TObject() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TsgcWSOnHTTPConnectionSSL)(System::TObject* Sender, Idssl::TIdSSLIOHandlerSocketBase* aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSocketIOHTTPRequest)(System::TObject* Sender, TsgcWSSocketIOHTTPRequest* aRequest);

typedef void __fastcall (__closure *TsgcWSOnSocketIOAfterConnect)(System::TObject* Sender);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSocketIO_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FHandShakeTimestamp;
	Sgcwebsocket_types::TwsSocketIOAPI FAPI;
	bool FBase64;
	bool FEncodeParameters;
	System::UnicodeString FHandShakeCustomURL;
	System::UnicodeString FNamespace;
	System::UnicodeString FParameters;
	bool FPolling;
	System::UnicodeString FUserAgent;
	System::UnicodeString FPath;
	System::UnicodeString __fastcall GetNamespace();
	System::UnicodeString __fastcall GetParameters();
	
public:
	__fastcall virtual TsgcWSSocketIO_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool HandShakeTimestamp = {read=FHandShakeTimestamp, write=FHandShakeTimestamp, nodefault};
	__property Sgcwebsocket_types::TwsSocketIOAPI API = {read=FAPI, write=FAPI, nodefault};
	__property bool Base64 = {read=FBase64, write=FBase64, nodefault};
	__property bool EncodeParameters = {read=FEncodeParameters, write=FEncodeParameters, nodefault};
	__property System::UnicodeString HandShakeCustomURL = {read=FHandShakeCustomURL, write=FHandShakeCustomURL};
	__property System::UnicodeString Namespace = {read=GetNamespace, write=FNamespace};
	__property System::UnicodeString Parameters = {read=GetParameters, write=FParameters};
	__property bool Polling = {read=FPolling, write=FPolling, nodefault};
	__property System::UnicodeString UserAgent = {read=FUserAgent, write=FUserAgent};
	__property System::UnicodeString Path = {read=FPath, write=FPath};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSocketIO_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_SocketIO : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual bool __fastcall IsInternalMessage(const System::UnicodeString aMessage);
	virtual void __fastcall DoBeforeConnect();
	virtual bool __fastcall DoKeepAlive();
	
private:
	TsgcWSSocketIO_Options* FSocketIO;
	
public:
	__property TsgcWSSocketIO_Options* SocketIO = {read=FSocketIO, write=FSocketIO};
	
private:
	bool FFirstMessage;
	System::UnicodeString __fastcall GetProtocolHTTP();
	System::UnicodeString __fastcall GetTimeStamp();
	virtual void __fastcall DoSendNameSpace();
	bool __fastcall DoGetSessionId(System::UnicodeString aText);
	bool __fastcall DoSessionResponse(System::Classes::TStringStream* aStream);
	bool __fastcall DoGetSession();
	System::UnicodeString __fastcall GetAPI_URL();
	void __fastcall DoSetURL();
	
protected:
	virtual System::UnicodeString __fastcall DoGetSessionURL();
	
public:
	__fastcall virtual TsgcWS_API_SocketIO(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_SocketIO();
	
private:
	int FIO_CloseTimeout;
	int FIO_HeartBeatTimeout;
	System::UnicodeString FIO_SessionId;
	
public:
	__property int IO_CloseTimeout = {read=FIO_CloseTimeout, nodefault};
	__property int IO_HeartBeatTimeout = {read=FIO_HeartBeatTimeout, nodefault};
	__property System::UnicodeString IO_SessionId = {read=FIO_SessionId};
	
private:
	void __fastcall DoClientException(System::Sysutils::Exception* E);
	void __fastcall DoPing();
	
public:
	void __fastcall Ping();
	
private:
	TsgcWSOnSocketIOAfterConnect FOnAfterConnect;
	TsgcWSOnHTTPConnectionSSL FOnHTTPConnectionSSL;
	TsgcWSOnSocketIOHTTPRequest FOnHTTPRequest;
	
public:
	__property TsgcWSOnSocketIOAfterConnect OnAfterConnect = {read=FOnAfterConnect, write=FOnAfterConnect};
	__property TsgcWSOnHTTPConnectionSSL OnHTTPConnectionSSL = {read=FOnHTTPConnectionSSL, write=FOnHTTPConnectionSSL};
	__property TsgcWSOnSocketIOHTTPRequest OnHTTPRequest = {read=FOnHTTPRequest, write=FOnHTTPRequest};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_socketio */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_SOCKETIO)
using namespace Sgcwebsocket_api_socketio;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_socketioHPP
