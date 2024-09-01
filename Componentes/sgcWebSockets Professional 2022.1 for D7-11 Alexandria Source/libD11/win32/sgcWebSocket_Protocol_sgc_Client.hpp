// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_sgc_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_sgc_clientHPP
#define Sgcwebsocket_protocol_sgc_clientHPP

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
#include <sgcBase_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_sgc_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSRPCItem;
class DELPHICLASS TsgcWSRPCList;
class DELPHICLASS TsgcWSProtocol_sgc_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSSessionEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Guid);

typedef void __fastcall (__closure *TsgcWSRPCResultEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Id, System::UnicodeString Result);

typedef void __fastcall (__closure *TsgcWSRPCErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Id, int ErrorCode, System::UnicodeString ErrorMessage, System::UnicodeString ErrorData);

typedef void __fastcall (__closure *TsgcWSCustomEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Channel, const System::UnicodeString Text);

class PASCALIMPLEMENTATION TsgcWSRPCItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TDateTime FDate;
	System::UnicodeString FId;
	System::UnicodeString FMethod;
	System::UnicodeString FParams;
	
public:
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property System::UnicodeString Method = {read=FMethod, write=FMethod};
	__property System::UnicodeString Params = {read=FParams, write=FParams};
public:
	/* TObject.Create */ inline __fastcall TsgcWSRPCItem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSRPCItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRPCList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSRPCList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSRPCList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSRPCList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_sgc_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Subscription_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Subscription_Client_Base inherited;
	
protected:
	System::UnicodeString FWSMessageId;
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* FWSMessage;
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	
public:
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoWriteRawData(const System::UnicodeString aText);
	
public:
	virtual void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	
protected:
	virtual System::UnicodeString __fastcall GetWSMessageText(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoWriteMessageText(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	
public:
	__fastcall virtual TsgcWSProtocol_sgc_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_sgc_Client();
	
protected:
	virtual void __fastcall DoAcknowledgment(const System::UnicodeString aId);
	
public:
	virtual void __fastcall Subscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString());
	virtual void __fastcall UnSubscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString());
	void __fastcall UnSubscribeAll(const System::UnicodeString aGuid = System::UnicodeString());
	void __fastcall Broadcast(const System::UnicodeString aText, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aGuid = System::UnicodeString());
	void __fastcall RPC(const System::UnicodeString aId, const System::UnicodeString aMethod, const System::UnicodeString aParams = System::UnicodeString(), const System::UnicodeString aGuid = System::UnicodeString(), const Sgcwebsocket_types::TwsQueue aQueue = (Sgcwebsocket_types::TwsQueue)(0x0));
	void __fastcall Notify(const System::UnicodeString aMethod, const System::UnicodeString aParams = System::UnicodeString(), const System::UnicodeString aGuid = System::UnicodeString(), const Sgcwebsocket_types::TwsQueue aQueue = (Sgcwebsocket_types::TwsQueue)(0x0));
	void __fastcall Publish(const System::UnicodeString aText, const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString(), const Sgcwebsocket_types::TwsQueue aQueue = (Sgcwebsocket_types::TwsQueue)(0x0));
	void __fastcall GetSession();
	void __fastcall StartTransaction(const System::UnicodeString aChannel = System::UnicodeString());
	void __fastcall Commit(const System::UnicodeString aChannel = System::UnicodeString());
	void __fastcall RollBack(const System::UnicodeString aChannel = System::UnicodeString());
	
private:
	Sgcwebsocket_protocol_sgc_message::TsgcWSAcknowledgment FOnAcknowledgment;
	TsgcWSCustomEvent FOnEvent;
	TsgcWSSessionEvent FOnSession;
	TsgcWSRPCErrorEvent FOnRPCError;
	TsgcWSRPCResultEvent FOnRPCResult;
	
protected:
	virtual void __fastcall DoEventRPCResult(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventRPCError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventSession(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	
public:
	__property TsgcWSCustomEvent OnEvent = {read=FOnEvent, write=FOnEvent};
	__property TsgcWSRPCErrorEvent OnRPCError = {read=FOnRPCError, write=FOnRPCError};
	__property TsgcWSRPCResultEvent OnRPCResult = {read=FOnRPCResult, write=FOnRPCResult};
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSAcknowledgment OnAcknowledgment = {read=FOnAcknowledgment, write=FOnAcknowledgment};
	__property TsgcWSSessionEvent OnSession = {read=FOnSession, write=FOnSession};
	
private:
	TsgcWSRPCList* FRPCList;
	TsgcWSRPCList* __fastcall GetRPCList();
	
protected:
	virtual void __fastcall DoQueueRPC(const System::UnicodeString aId, const System::UnicodeString aMethod, const System::UnicodeString aParams);
	virtual void __fastcall DoDeleteRPC(const System::UnicodeString aId);
	__property TsgcWSRPCList* RPCList = {read=GetRPCList};
	
public:
	System::UnicodeString __fastcall GetRPCMethodById(const System::UnicodeString aId);
	System::UnicodeString __fastcall GetRPCParamsById(const System::UnicodeString aId);
	
private:
	Sgcbase_classes::TsgcTimer* FQoSTimer;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* FQoS;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* FQoSList;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* __fastcall GetQoSList();
	
protected:
	virtual void __fastcall DoStartQoS();
	virtual void __fastcall DoStopQoS();
	virtual void __fastcall DoQoSList();
	virtual void __fastcall DoProcessAcknowledgment(const System::UnicodeString aId);
	virtual void __fastcall DoProcessPubRec(System::UnicodeString aId);
	virtual void __fastcall DoQueueQoS(const System::UnicodeString aId, const System::UnicodeString aText);
	virtual void __fastcall OnQoSEvent(System::TObject* Sender);
	virtual void __fastcall OnQoSExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	void __fastcall SetQoS(Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* const Value);
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* QoSList = {read=GetQoSList};
	
public:
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* QoS = {read=FQoS, write=SetQoS};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  WriteData(System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base::WriteData(aStream, aSize, aStreaming); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_sgc_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_SGC_CLIENT)
using namespace Sgcwebsocket_protocol_sgc_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_sgc_clientHPP
