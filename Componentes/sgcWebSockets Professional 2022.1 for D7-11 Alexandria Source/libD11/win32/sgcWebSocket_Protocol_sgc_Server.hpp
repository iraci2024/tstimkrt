// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_sgc_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_sgc_serverHPP
#define Sgcwebsocket_protocol_sgc_serverHPP

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
#include <sgcBase_Helpers.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_sgc_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcThreadListMethodsId;
class DELPHICLASS TsgcWSMethodId;
class DELPHICLASS TsgcWSTransaction;
class DELPHICLASS TsgcWSTransactions;
class DELPHICLASS TsgcWSQueuedMessage;
class DELPHICLASS TsgcWSQueuedMessages;
class DELPHICLASS TsgcWSQoSLevel2;
class DELPHICLASS TsgcWSQoSLevel2List;
class DELPHICLASS TsgcWSAuthentication_Methods;
class DELPHICLASS TsgcWSProtocol_sgc_Server;
class DELPHICLASS TsgcWSProtocol_JS_sgc;
class DELPHICLASS TsgcWSProtocol_HTML_sgc;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSRPCEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString ID, const System::UnicodeString Method, const System::UnicodeString Params);

typedef void __fastcall (__closure *TsgcWSNotificationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Method, const System::UnicodeString Params);

typedef void __fastcall (__closure *TsgcWSRPCAuthenticationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Method, const System::UnicodeString User, const System::UnicodeString Password, bool &Authenticated);

typedef void __fastcall (__closure *TsgcWSBeforePublish)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString aChannel, System::UnicodeString &aText, bool &Accept);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListMethodsId : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListMethodsId() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListMethodsId() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMethodId : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FConnection;
	System::UnicodeString FId;
	
public:
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=FConnection, write=FConnection};
	__property System::UnicodeString Id = {read=FId, write=FId};
public:
	/* TObject.Create */ inline __fastcall TsgcWSMethodId() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSMethodId() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSTransaction : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FActive;
	System::UnicodeString FID;
	Sgcwebsocket_classes::TsgcWSObjectList* FQueue;
	Sgcwebsocket_classes::TsgcWSObjectList* __fastcall GetQueue();
	void __fastcall SetActive(const bool Value);
	
public:
	__fastcall virtual TsgcWSTransaction();
	__fastcall virtual ~TsgcWSTransaction();
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property Sgcwebsocket_classes::TsgcWSObjectList* Queue = {read=GetQueue, write=FQueue};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSTransactions : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	HIDESBASE void __fastcall Add(const System::UnicodeString ID)/* overload */;
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSTransactions()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSTransactions(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSTransactions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueuedMessage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FID;
	Sgcwebsocket_classes::TsgcWSObjectList* FQueue;
	Sgcwebsocket_classes::TsgcWSObjectList* __fastcall GetQueue();
	
public:
	__fastcall virtual ~TsgcWSQueuedMessage();
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property Sgcwebsocket_classes::TsgcWSObjectList* Queue = {read=GetQueue, write=FQueue};
public:
	/* TObject.Create */ inline __fastcall TsgcWSQueuedMessage() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueuedMessages : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	HIDESBASE TsgcWSQueuedMessage* __fastcall Add(const System::UnicodeString ID)/* overload */;
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSQueuedMessages()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSQueuedMessages(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSQueuedMessages() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQoSLevel2 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FID;
	Sgcwebsocket_classes::TsgcWSNotifyObject* FNotifyObject;
	Sgcwebsocket_classes::TsgcWSNotifyObject* __fastcall GetNotifyObject();
	
public:
	__fastcall virtual ~TsgcWSQoSLevel2();
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property Sgcwebsocket_classes::TsgcWSNotifyObject* NotifyObject = {read=GetNotifyObject, write=FNotifyObject};
public:
	/* TObject.Create */ inline __fastcall TsgcWSQoSLevel2() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQoSLevel2List : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	HIDESBASE TsgcWSQoSLevel2* __fastcall Add(const System::UnicodeString aID, Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText)/* overload */;
	int __fastcall Item(const System::UnicodeString aId);
	HIDESBASE void __fastcall Delete(const System::UnicodeString aID);
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSQoSLevel2List()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSQoSLevel2List(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSQoSLevel2List() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_Methods : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TStringList* FMethods;
	bool FEnabled;
	void __fastcall SetMethods(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSAuthentication_Methods();
	__fastcall virtual ~TsgcWSAuthentication_Methods();
	
__published:
	__property System::Classes::TStringList* Methods = {read=FMethods, write=SetMethods};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_sgc_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base inherited;
	
protected:
	virtual void __fastcall Loaded();
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	virtual Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	virtual bool __fastcall WriteData(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	
protected:
	virtual void __fastcall DoBroadcastAcknowledgment(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall DoBroadCast(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall DoBroadCast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	virtual void __fastcall Broadcast(System::UnicodeString aMessage, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall Broadcast(System::Classes::TStream* aStream, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	__fastcall virtual TsgcWSProtocol_sgc_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_sgc_Server();
	
private:
	TsgcWSTransactions* FTransactions;
	TsgcWSTransactions* __fastcall GetTransactions();
	
protected:
	virtual TsgcWSTransaction* __fastcall GetTransaction(const System::UnicodeString aId);
	virtual bool __fastcall InTransaction(const System::UnicodeString aId);
	virtual void __fastcall DoStartTransaction(const System::UnicodeString aId);
	virtual void __fastcall DoDeleteTransaction(const System::UnicodeString aId);
	virtual void __fastcall DoCommit(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage, const System::UnicodeString aId);
	virtual void __fastcall DoRollBack(const System::UnicodeString aId);
	bool __fastcall DoQueueTransaction(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aText, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage);
	__property TsgcWSTransactions* Transactions = {read=GetTransactions};
	
private:
	TsgcWSQueuedMessages* FQueuedMessages;
	TsgcWSQueuedMessages* __fastcall GetQueuedMessages();
	
protected:
	virtual TsgcWSQueuedMessage* __fastcall GetQueuedMessage(const System::UnicodeString aId);
	virtual void __fastcall DoProcessQueuedMessages(const System::UnicodeString aId, Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	bool __fastcall DoQueueMessageLevel1(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aText, System::UnicodeString aChannel);
	bool __fastcall DoQueueMessageLevel2(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aText, System::UnicodeString aChannel);
	__property TsgcWSQueuedMessages* QueuedMessages = {read=GetQueuedMessages};
	
public:
	bool __fastcall ClearQueue(const System::UnicodeString aChannel);
	
private:
	TsgcWSQoSLevel2List* FQoSLevel2List;
	TsgcWSQoSLevel2List* __fastcall GetQoSLevel2List();
	
protected:
	virtual void __fastcall DoQoSLevel2Publish(const System::UnicodeString aId, Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText);
	void __fastcall DoQoSLevel2PubRec(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aID);
	void __fastcall DoQoSLevel2PubRel(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aId);
	__property TsgcWSQoSLevel2List* QoSLevel2List = {read=GetQoSLevel2List};
	bool __fastcall DoWriteMessageText(const System::UnicodeString aGuid, const System::UnicodeString aMessage)/* overload */;
	bool __fastcall DoWriteMessageText(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aMessage)/* overload */;
	void __fastcall DoProcess(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoProcessMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoSession(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aId);
	void __fastcall DoPublish(const System::UnicodeString aMessage, const System::UnicodeString aChannel, const System::UnicodeString aExclude, const System::UnicodeString aInclude, const Sgcwebsocket_types::TwsQueue aQueue);
	
public:
	void __fastcall Publish(const System::UnicodeString aMessage, const System::UnicodeString aChannel, const System::UnicodeString aExclude = System::UnicodeString(), const System::UnicodeString aInclude = System::UnicodeString(), const Sgcwebsocket_types::TwsQueue aQueue = (Sgcwebsocket_types::TwsQueue)(0x0))/* overload */;
	void __fastcall Publish(const System::UnicodeString aMessage, const System::UnicodeString aChannel, const Sgcwebsocket_types::TwsQueue aQueue)/* overload */;
	void __fastcall RPCResult(System::UnicodeString aId, System::UnicodeString aResult);
	void __fastcall RPCError(System::UnicodeString aId, int aCode, System::UnicodeString aMessage, System::UnicodeString aData = System::UnicodeString());
	
private:
	TsgcThreadListMethodsId* FMethodsId;
	
protected:
	TsgcThreadListMethodsId* __fastcall GetMethodsId();
	bool __fastcall AddMethodID(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aId);
	Sgcwebsocket_classes::TsgcWSConnection* __fastcall GetMethodId(const System::UnicodeString aId);
	bool __fastcall RemoveMethodId(const System::UnicodeString aId);
	
private:
	Sgcbase_classes::TsgcTimer* FQoSTimer;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* FQoS;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* FQoSList;
	Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* __fastcall GetQoSList();
	
protected:
	virtual void __fastcall DoStartQoS();
	virtual void __fastcall DoStopQoS();
	virtual void __fastcall DoQoSList();
	virtual void __fastcall DoProcessAcknowledgment(const System::UnicodeString aGuid, const System::UnicodeString aId);
	virtual void __fastcall DoQueueQoS(const System::UnicodeString aGuid, const System::UnicodeString aId, const System::UnicodeString aText);
	virtual void __fastcall DoDeleteQoSByGuid(const System::UnicodeString aGuid);
	virtual void __fastcall OnQoSEvent(System::TObject* Sender);
	virtual void __fastcall OnQoSExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	void __fastcall SetQoS(Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* const Value);
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSQoSList* QoSList = {read=GetQoSList};
	
public:
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSQoS_Options* QoS = {read=FQoS, write=SetQoS};
	
private:
	TsgcWSAuthentication_Methods* FRPCAuthentication;
	
protected:
	bool __fastcall DoRPCAuthentication(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aMethod);
	void __fastcall SetRPCAuthentication(TsgcWSAuthentication_Methods* const Value);
	
public:
	__property TsgcWSAuthentication_Methods* RPCAuthentication = {read=FRPCAuthentication, write=SetRPCAuthentication};
	
private:
	Sgcwebsocket_protocol_sgc_message::TsgcWSAcknowledgment FOnAcknowledgment;
	TsgcWSNotificationEvent FOnNotification;
	TsgcWSRPCEvent FOnRPC;
	TsgcWSRPCAuthenticationEvent FOnRPCAuthentication;
	TsgcWSBeforePublish FOnBeforePublish;
	
protected:
	virtual void __fastcall DoEventNotification(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventRPC(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* const aMessage);
	virtual void __fastcall DoEventRPCAuthentication(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aMethod, bool &Result);
	virtual void __fastcall DoEventBeforePublish(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aChannel, System::UnicodeString &aText, bool &Accept);
	virtual void __fastcall DoEventAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Id);
	
public:
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSAcknowledgment OnAcknowledgment = {read=FOnAcknowledgment, write=FOnAcknowledgment};
	__property TsgcWSNotificationEvent OnNotification = {read=FOnNotification, write=FOnNotification};
	__property TsgcWSRPCEvent OnRPC = {read=FOnRPC, write=FOnRPC};
	__property TsgcWSRPCAuthenticationEvent OnRPCAuthentication = {read=FOnRPCAuthentication, write=FOnRPCAuthentication};
	__property TsgcWSBeforePublish OnBeforePublish = {read=FOnBeforePublish, write=FOnBeforePublish};
	/* Hoisted overloads: */
	
protected:
	inline bool __fastcall  WriteData(System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aGuid, aStream, aSize, aStreaming); }
	inline bool __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessage){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aMessage); }
	inline void __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TStream* aStream){ Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aStream); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_sgc : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_sgc() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_sgc() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_sgc : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_sgc() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_sgc() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_sgc_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_SGC_SERVER)
using namespace Sgcwebsocket_protocol_sgc_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_sgc_serverHPP
