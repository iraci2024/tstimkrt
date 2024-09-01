// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WAMP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_wamp_clientHPP
#define Sgcwebsocket_protocol_wamp_clientHPP

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
#include <sgcJSON.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_Protocol_WAMP_Message.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_wamp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_WAMP_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSWelcomeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString SessionId, System::UnicodeString ProtocolVersion, System::UnicodeString ServerIdent);

typedef void __fastcall (__closure *TsgcWSCallResultEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString CallId, System::UnicodeString Result);

typedef void __fastcall (__closure *TsgcWSCallProgresslResultEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString CallId, System::UnicodeString Result);

typedef void __fastcall (__closure *TsgcWSCallErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString CallId, System::UnicodeString ErrorURI, System::UnicodeString ErrorDesc, System::UnicodeString ErrorDetails);

typedef void __fastcall (__closure *TsgcWSEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString TopicURI, System::UnicodeString Event);

class PASCALIMPLEMENTATION TsgcWSProtocol_WAMP_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base inherited;
	
private:
	System::UnicodeString FWSMessageWAMPId;
	
protected:
	Sgcwebsocket_protocol_wamp_message::TsgcWSMessageWAMP* FWSMessageWAMP;
	Sgcwebsocket_protocol_wamp_message::TsgcWSMessageWAMP* __fastcall GetWSMessageWAMP();
	Sgcwebsocket_protocol_wamp_message::TsgcWSMessageWAMP* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	__property Sgcwebsocket_protocol_wamp_message::TsgcWSMessageWAMP* WSMessageWAMP = {read=GetWSMessageWAMP, write=FWSMessageWAMP};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__fastcall virtual TsgcWSProtocol_WAMP_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_WAMP_Client();
	
protected:
	virtual void __fastcall DoWrite();
	
public:
	void __fastcall Prefix(const System::UnicodeString aPrefix, const System::UnicodeString aURI);
	void __fastcall Subscribe(const System::UnicodeString aTopicURI);
	void __fastcall UnSubscribe(const System::UnicodeString aTopicURI);
	void __fastcall Call(const System::UnicodeString aCallId, const System::UnicodeString aProcURI, System::UnicodeString aArguments = System::UnicodeString());
	void __fastcall CancelCall(const System::UnicodeString aCallId);
	void __fastcall Publish(const System::UnicodeString aTopicURI, const System::UnicodeString aEvent, const System::UnicodeString aExclude = System::UnicodeString(), const System::UnicodeString aEligible = System::UnicodeString());
	
private:
	TsgcWSCallErrorEvent FOnCallError;
	TsgcWSCallResultEvent FOnCallResult;
	TsgcWSCallProgresslResultEvent FOnCallProgressResult;
	TsgcWSEvent FOnEvent;
	TsgcWSWelcomeEvent FOnWelcome;
	
__published:
	__property TsgcWSCallErrorEvent OnCallError = {read=FOnCallError, write=FOnCallError};
	__property TsgcWSCallResultEvent OnCallResult = {read=FOnCallResult, write=FOnCallResult};
	__property TsgcWSCallProgresslResultEvent OnCallProgressResult = {read=FOnCallProgressResult, write=FOnCallProgressResult};
	__property TsgcWSEvent OnEvent = {read=FOnEvent, write=FOnEvent};
	__property TsgcWSWelcomeEvent OnWelcome = {read=FOnWelcome, write=FOnWelcome};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_wamp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WAMP_CLIENT)
using namespace Sgcwebsocket_protocol_wamp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_wamp_clientHPP
