// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WAMP_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_wamp_serverHPP
#define Sgcwebsocket_protocol_wamp_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_WAMP_Message.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_wamp_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSCallId;
class DELPHICLASS TsgcThreadListCallsId;
class DELPHICLASS TsgcWSProtocol_WAMP_Server;
class DELPHICLASS TsgcWSProtocol_JS_WAMP;
class DELPHICLASS TsgcWSProtocol_HTML_WAMP;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSPrefixEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Prefix, const System::UnicodeString URI);

typedef void __fastcall (__closure *TsgcWSCallEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString CallId, const System::UnicodeString ProcUri, const System::UnicodeString Arguments);

typedef void __fastcall (__closure *TsgcWSBeforeCancelCallEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString CallId, bool &Cancel);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSCallId : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FGuid;
	System::UnicodeString FId;
	
public:
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString Id = {read=FId, write=FId};
public:
	/* TObject.Create */ inline __fastcall TsgcWSCallId() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSCallId() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListCallsId : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListCallsId() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListCallsId() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_WAMP_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base inherited;
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	virtual Sgcwebsocket_protocol_wamp_message::TsgcWSMessageWAMP* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	
private:
	TsgcThreadListCallsId* FCallId;
	
protected:
	TsgcThreadListCallsId* __fastcall GetCallsId();
	bool __fastcall AddCallId(const System::UnicodeString aGuid, const System::UnicodeString aCallId);
	bool __fastcall RemoveCallId(const System::UnicodeString aCallId);
	System::UnicodeString __fastcall GetCallGuid(const System::UnicodeString aCallId);
	System::UnicodeString __fastcall GetNewCallId(const System::UnicodeString aGuid, const System::UnicodeString aCallId);
	System::UnicodeString __fastcall GetOriginalCallId(const System::UnicodeString aGuid, const System::UnicodeString aCallId);
	virtual void __fastcall DoWelcome(const System::UnicodeString aGuid);
	
public:
	void __fastcall CallResult(const System::UnicodeString aCallId, const System::UnicodeString aResult = System::UnicodeString());
	void __fastcall CallProgressResult(const System::UnicodeString aCallId, const System::UnicodeString aResult = System::UnicodeString());
	void __fastcall CallError(const System::UnicodeString aCallId, const System::UnicodeString aErrorURI, const System::UnicodeString aErrorDesc, const System::UnicodeString aErrorDetails = System::UnicodeString());
	void __fastcall Event(const System::UnicodeString aTopicURI, const System::UnicodeString aEvent = System::UnicodeString());
	__fastcall virtual TsgcWSProtocol_WAMP_Server(System::Classes::TComponent* aOwner);
	
private:
	TsgcWSCallEvent FOnCall;
	TsgcWSBeforeCancelCallEvent FOnBeforeCancelCall;
	TsgcWSPrefixEvent FOnPrefix;
	bool __fastcall DoBeforeCancelCall(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aCallId);
	
public:
	__property TsgcWSCallEvent OnCall = {read=FOnCall, write=FOnCall};
	__property TsgcWSBeforeCancelCallEvent OnBeforeCancelCall = {read=FOnBeforeCancelCall, write=FOnBeforeCancelCall};
	__property TsgcWSPrefixEvent OnPrefix = {read=FOnPrefix, write=FOnPrefix};
public:
	/* TsgcWSProtocol_Subscription_Server_Base.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_WAMP_Server() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_WAMP : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_WAMP() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_WAMP() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_WAMP : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_WAMP() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_WAMP() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_wamp_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WAMP_SERVER)
using namespace Sgcwebsocket_protocol_wamp_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_wamp_serverHPP
