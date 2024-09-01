// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WAMP2_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_wamp2_clientHPP
#define Sgcwebsocket_protocol_wamp2_clientHPP

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
#include <sgcWebSocket_Protocol_WAMP2_Message.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_wamp2_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_WAMP2_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSSessionEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString &aRealm, System::UnicodeString &aDetails);

typedef void __fastcall (__closure *TsgcWSWelcomeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 SessionId, System::UnicodeString Details);

typedef void __fastcall (__closure *TsgcWSChallengeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString AuthMethod, System::UnicodeString Details, System::UnicodeString &Secret);

typedef void __fastcall (__closure *TsgcWSSubscribedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId, __int64 SubscriptionId);

typedef void __fastcall (__closure *TsgcWSUnsubscribedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId);

typedef void __fastcall (__closure *TsgcWSEventEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 SubscriptionId, __int64 PublicationId, System::UnicodeString Details, System::UnicodeString Arguments, System::UnicodeString ArgumentsKw);

typedef void __fastcall (__closure *TsgcWSPublishedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId, __int64 PublicationId);

typedef void __fastcall (__closure *TsgcWSResultEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId, System::UnicodeString Details, System::UnicodeString Arguments, System::UnicodeString ArgumentsKw);

typedef void __fastcall (__closure *TsgcWSErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int MethodId, __int64 RequestId, System::UnicodeString Details, System::UnicodeString Error, System::UnicodeString Arguments, System::UnicodeString ArgumentsKw);

typedef void __fastcall (__closure *TsgcWSAbortEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Details, System::UnicodeString Reason);

typedef void __fastcall (__closure *TsgcWSGoodByeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Details, System::UnicodeString Reason);

typedef void __fastcall (__closure *TsgcWSRegisteredEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId, __int64 RegistrationId);

typedef void __fastcall (__closure *TsgcWSUnRegisteredEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, __int64 RequestId, __int64 RegistrationId);

class PASCALIMPLEMENTATION TsgcWSProtocol_WAMP2_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base inherited;
	
private:
	System::UnicodeString FWSMessageWAMPId;
	
protected:
	Sgcwebsocket_protocol_wamp2_message::TsgcWSMessageWAMP2* FWSMessageWAMP;
	Sgcwebsocket_protocol_wamp2_message::TsgcWSMessageWAMP2* __fastcall GetWSMessageWAMP();
	Sgcwebsocket_protocol_wamp2_message::TsgcWSMessageWAMP2* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	__property Sgcwebsocket_protocol_wamp2_message::TsgcWSMessageWAMP2* WSMessageWAMP = {read=GetWSMessageWAMP, write=FWSMessageWAMP};
	
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
	__fastcall virtual TsgcWSProtocol_WAMP2_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_WAMP2_Client();
	
protected:
	virtual void __fastcall DoWrite();
	virtual void __fastcall DoReplyGoodBye();
	virtual void __fastcall DoHello(const System::UnicodeString aRealm, const System::UnicodeString aDetails);
	virtual void __fastcall DoAuthenticate(const System::UnicodeString aSecret);
	
public:
	void __fastcall Subscribe(const System::UnicodeString aTopic, const System::UnicodeString aOptions = L"{}", const __int64 aRequestId = 0LL);
	void __fastcall UnSubscribe(const __int64 aSubscriptionId, const __int64 aRequestId = 0LL);
	void __fastcall Abort(const System::UnicodeString aDetails, const System::UnicodeString aReason);
	void __fastcall GoodBye(const System::UnicodeString aDetails, const System::UnicodeString aReason);
	void __fastcall Publish(const System::UnicodeString aTopic, const System::UnicodeString aArguments = System::UnicodeString(), const System::UnicodeString aArgumentsKw = System::UnicodeString(), const System::UnicodeString aOptions = L"{}", const __int64 aRequestId = 0LL);
	void __fastcall Call(const System::UnicodeString aProcedure, const System::UnicodeString aArguments = System::UnicodeString(), const System::UnicodeString aArgumentsKw = System::UnicodeString(), const System::UnicodeString aOptions = L"{}", const __int64 aRequestId = 0LL);
	void __fastcall RegisterCall(const System::UnicodeString aProcedure, const System::UnicodeString aOptions = L"{}", const __int64 aRequestId = 0LL);
	void __fastcall UnRegisterCall(const __int64 aRegistrationId, const __int64 aRequestId = 0LL);
	void __fastcall Invocation(const __int64 aRegistrationId, const System::UnicodeString aDetails = System::UnicodeString(), const System::UnicodeString aArguments = System::UnicodeString(), const System::UnicodeString aArgumentsKw = System::UnicodeString(), const __int64 aRequestId = 0LL);
	void __fastcall InvocationError(const __int64 aRequestId, const System::UnicodeString aDetails = L"{}", const System::UnicodeString aTopic = System::UnicodeString(), const System::UnicodeString aArguments = System::UnicodeString(), const System::UnicodeString aArgumentsKw = System::UnicodeString());
	void __fastcall Yield(const System::UnicodeString aOptions = L"{}", const System::UnicodeString aArguments = System::UnicodeString(), const System::UnicodeString aArgumentsKw = System::UnicodeString(), const __int64 aRequestId = 0LL);
	
private:
	TsgcWSSessionEvent FOnWAMPSession;
	TsgcWSSubscribedEvent FOnWAMPSubscribed;
	TsgcWSUnsubscribedEvent FOnWAMPUnsubscribed;
	TsgcWSWelcomeEvent FOnWAMPWelcome;
	TsgcWSErrorEvent FOnWAMPError;
	TsgcWSAbortEvent FOnWAMPAbort;
	TsgcWSGoodByeEvent FOnWAMPGoodBye;
	TsgcWSEventEvent FOnWAMPEvent;
	TsgcWSPublishedEvent FOnWAMPPublished;
	TsgcWSRegisteredEvent FOnWAMPRegistered;
	TsgcWSResultEvent FOnWAMPResult;
	TsgcWSUnRegisteredEvent FOnWAMPUnRegistered;
	TsgcWSChallengeEvent FOnWAMPChallenge;
	
protected:
	void __fastcall DoSessionEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
__published:
	__property TsgcWSSessionEvent OnWAMPSession = {read=FOnWAMPSession, write=FOnWAMPSession};
	__property TsgcWSWelcomeEvent OnWAMPWelcome = {read=FOnWAMPWelcome, write=FOnWAMPWelcome};
	__property TsgcWSAbortEvent OnWAMPAbort = {read=FOnWAMPAbort, write=FOnWAMPAbort};
	__property TsgcWSGoodByeEvent OnWAMPGoodBye = {read=FOnWAMPGoodBye, write=FOnWAMPGoodBye};
	__property TsgcWSSubscribedEvent OnWAMPSubscribed = {read=FOnWAMPSubscribed, write=FOnWAMPSubscribed};
	__property TsgcWSUnsubscribedEvent OnWAMPUnsubscribed = {read=FOnWAMPUnsubscribed, write=FOnWAMPUnsubscribed};
	__property TsgcWSPublishedEvent OnWAMPPublished = {read=FOnWAMPPublished, write=FOnWAMPPublished};
	__property TsgcWSRegisteredEvent OnWAMPRegistered = {read=FOnWAMPRegistered, write=FOnWAMPRegistered};
	__property TsgcWSUnRegisteredEvent OnWAMPUnRegistered = {read=FOnWAMPUnRegistered, write=FOnWAMPUnRegistered};
	__property TsgcWSEventEvent OnWAMPEvent = {read=FOnWAMPEvent, write=FOnWAMPEvent};
	__property TsgcWSErrorEvent OnWAMPError = {read=FOnWAMPError, write=FOnWAMPError};
	__property TsgcWSResultEvent OnWAMPResult = {read=FOnWAMPResult, write=FOnWAMPResult};
	__property TsgcWSChallengeEvent OnWAMPChallenge = {read=FOnWAMPChallenge, write=FOnWAMPChallenge};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_wamp2_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WAMP2_CLIENT)
using namespace Sgcwebsocket_protocol_wamp2_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_wamp2_clientHPP
