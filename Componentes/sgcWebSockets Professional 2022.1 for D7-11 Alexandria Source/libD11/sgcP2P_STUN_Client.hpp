// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcP2P_STUN_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcp2p_stun_clientHPP
#define Sgcp2p_stun_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdGlobal.hpp>
#include <sgcP2P_STUN_Classes.hpp>
#include <sgcUDP_Client.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcTCP_Client.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcP2P_STUN_Types.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcp2p_stun_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSTUNPRetransmissionClient_Options;
class DELPHICLASS TsgcSTUNRequestItem;
class DELPHICLASS TsgcSTUNRequestsQueue;
class DELPHICLASS TsgcSTUNClient_Authentication;
class DELPHICLASS TsgcSTUNClient_Options;
class DELPHICLASS TsgcSTUNClient_Base;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcSTUNResponseSuccessEvent)(System::TObject* Sender, Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, Sgcp2p_stun_classes::TsgcSTUN_ResponseBinding* const aBinding);

typedef void __fastcall (__closure *TsgcSTUNResponseErrorEvent)(System::TObject* Sender, Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, Sgcp2p_stun_classes::TsgcSTUN_ResponseError* const aError);

typedef void __fastcall (__closure *TsgcSTUNBeforeSendEvent)(System::TObject* Sender, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNPRetransmissionClient_Options : public Sgcudp_client::TsgcUDPRetransmissionClient_Options
{
	typedef Sgcudp_client::TsgcUDPRetransmissionClient_Options inherited;
	
public:
	__fastcall virtual TsgcSTUNPRetransmissionClient_Options();
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNPRetransmissionClient_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUNRequestItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::DynamicArray<System::Byte> FMessageIntegrityKey;
	Sgcp2p_stun_types::TsgcStunMessageMethod FMethod;
	System::Classes::TStringList* FParameters;
	System::Classes::TStringList* __fastcall GetParameters();
	void __fastcall SetMessageIntegrityKey(const System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual ~TsgcSTUNRequestItem();
	__property System::DynamicArray<System::Byte> MessageIntegrityKey = {read=FMessageIntegrityKey, write=SetMessageIntegrityKey};
	__property Sgcp2p_stun_types::TsgcStunMessageMethod Method = {read=FMethod, write=FMethod, nodefault};
	__property System::Classes::TStringList* Parameters = {read=GetParameters, write=FParameters};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSTUNRequestItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNRequestsQueue : public Sgcwebsocket_classes_queues::TsgcQueueBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueBase inherited;
	
public:
	void __fastcall AddRequest(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	bool __fastcall DeleteRequest(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	bool __fastcall ExistsRequest(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	TsgcSTUNRequestItem* __fastcall GetRequest(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	System::DynamicArray<System::Byte> __fastcall GetMessageIntegrityKey(System::UnicodeString aTransactionId);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSTUNRequestsQueue() : Sgcwebsocket_classes_queues::TsgcQueueBase() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSTUNRequestsQueue() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNClient_Authentication : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcp2p_stun_types::TsgcStunCredentials FCredentials;
	System::UnicodeString FPassword;
	System::UnicodeString FUsername;
	
public:
	__fastcall virtual TsgcSTUNClient_Authentication();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcp2p_stun_types::TsgcStunCredentials Credentials = {read=FCredentials, write=FCredentials, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNClient_Authentication() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNClient_Options : public Sgcp2p_stun_classes::TsgcSTUN_Options
{
	typedef Sgcp2p_stun_classes::TsgcSTUN_Options inherited;
	
private:
	TsgcSTUNClient_Authentication* FAuthentication;
	void __fastcall SetAuthentication(TsgcSTUNClient_Authentication* const Value);
	
public:
	__fastcall virtual TsgcSTUNClient_Options();
	__fastcall virtual ~TsgcSTUNClient_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcSTUNClient_Authentication* Authentication = {read=FAuthentication, write=SetAuthentication};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUNClient_Base : public Sgcp2p_stun_classes::TsgcSTUN_Component
{
	typedef Sgcp2p_stun_classes::TsgcSTUN_Component inherited;
	
private:
	Sgcudp_client::TsgcUDPCLient_Base* FUDPClient;
	Sgcudp_client::TsgcUDPCLient_Base* __fastcall GetUDPClient();
	Sgctcp_client::TsgcTCPCLient_Base* FTCPClient;
	Sgctcp_client::TsgcTCPCLient_Base* __fastcall GetTCPClient();
	
protected:
	void __fastcall DoRetransmissionAdd(const System::UnicodeString aTransactionId, const System::DynamicArray<System::Byte> aBytes, const System::UnicodeString aIPAddress, System::Word aPort = (System::Word)(0x0));
	bool __fastcall DoRetransmissionDelete(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	virtual void __fastcall OnUDPReadEvent(System::TObject* Sender, Sgcudp_classes::TsgcUDPSocket* Socket, Sgcsocket_classes::TsgcBytes Bytes);
	virtual void __fastcall OnUDPExceptionEvent(System::TObject* Sender, Sgcudp_classes::TsgcUDPSocket* Socket, System::Sysutils::Exception* E);
	virtual void __fastcall OnRetransmissionTimeoutEvent(System::TObject* Sender, const System::DynamicArray<System::Byte> aBytes, const System::UnicodeString aIPAddress, System::Word aPort);
	virtual void __fastcall OnTCPReadEvent(Sgctcp_classes::TsgcTCPConnection* Connection, System::Classes::TMemoryStream* const aStream);
	virtual void __fastcall OnTCPExceptionEvent(Sgctcp_classes::TsgcTCPConnection* Connection, System::Sysutils::Exception* E);
	virtual void __fastcall DoTCPDisconnect();
	__property Sgcudp_client::TsgcUDPCLient_Base* UDPClient = {read=GetUDPClient, write=FUDPClient};
	__property Sgctcp_client::TsgcTCPCLient_Base* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
private:
	Sgcp2p_stun_classes::TsgcSTUN_ResponseError* __fastcall GetErrorFromMessage(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	Sgcp2p_stun_classes::TsgcSTUN_ResponseBinding* __fastcall GetBindingFromMessage(Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	
protected:
	virtual void __fastcall OnRequestMessageIntegrityKeyEvent(System::TObject* Sender, const System::UnicodeString aTransactionId, System::DynamicArray<System::Byte> &MessageIntegrityKey);
	virtual void __fastcall OnICEValidateMessageIntegrityEvent(System::TObject* Sender, const System::UnicodeString aUsername, System::UnicodeString &Password);
	virtual void __fastcall OnICERequestMessageIntegrityPasswordEvent(System::TObject* Sender, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, System::UnicodeString &Password);
	virtual void __fastcall DoSTUNResponseSuccess(Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	virtual void __fastcall DoSTUNIndication(Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage) = 0 ;
	virtual void __fastcall DoSTUNRequest(Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	bool __fastcall GetRealmNonce(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, System::UnicodeString &Realm, System::UnicodeString &Nonce);
	virtual bool __fastcall DoSTUNReceivedRealm(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	virtual bool __fastcall DoSTUNTryAlternate(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage);
	virtual void __fastcall DoSTUNRead(Sgcsocket_classes::TsgcSocketConnection* const aSocket, const System::DynamicArray<System::Byte> aBytes);
	TsgcSTUNRequestsQueue* FRequests;
	virtual TsgcSTUNRequestsQueue* __fastcall GetRequests();
	__property TsgcSTUNRequestsQueue* Requests = {read=GetRequests, write=FRequests};
	
public:
	__fastcall virtual TsgcSTUNClient_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcSTUNClient_Base();
	
protected:
	System::UnicodeString FRealm;
	System::UnicodeString FNonce;
	virtual void __fastcall DoWriteBytes(const System::DynamicArray<System::Byte> aBytes);
	virtual void __fastcall DoSendMessage(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, const System::UnicodeString aIPAddress = System::UnicodeString(), System::Word aPort = (System::Word)(0x0));
	virtual void __fastcall DoSendResponse(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, const System::UnicodeString aIPAddress = System::UnicodeString(), System::Word aPort = (System::Word)(0x0));
	virtual void __fastcall DoPrepareRequest(Sgcp2p_stun_types::TsgcStunMessageMethod aMethod, Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, const Sgcp2p_stun_classes::TsgcSTUN_Attributes aAttributes = Sgcp2p_stun_classes::TsgcSTUN_Attributes());
	virtual void __fastcall DoSetLongTermCredentials(Sgcp2p_stun_classes::TsgcSTUN_Message* const aMessage, const System::UnicodeString aRealm, const System::UnicodeString aNonce);
	virtual void __fastcall DoSendRequest_LongTermCredential(Sgcp2p_stun_types::TsgcStunMessageMethod aMethod, const System::UnicodeString aRealm, const System::UnicodeString aNonce, const Sgcp2p_stun_classes::TsgcSTUN_Attributes aAttributes = Sgcp2p_stun_classes::TsgcSTUN_Attributes());
	virtual void __fastcall DoSendRequest(Sgcp2p_stun_types::TsgcStunMessageMethod aMethod, const Sgcp2p_stun_classes::TsgcSTUN_Attributes aAttributes = Sgcp2p_stun_classes::TsgcSTUN_Attributes());
	
public:
	void __fastcall SendRequest();
	void __fastcall WriteData(const System::DynamicArray<System::Byte> aBytes);
	
private:
	TsgcSTUNPRetransmissionClient_Options* FRetransmissionOptions;
	void __fastcall SetRetransmissionOptions(TsgcSTUNPRetransmissionClient_Options* const Value);
	
public:
	__property TsgcSTUNPRetransmissionClient_Options* RetransmissionOptions = {read=FRetransmissionOptions, write=SetRetransmissionOptions};
	
private:
	Sgcp2p_stun_classes::TsgcSTUNLogFile* FLogFile;
	Sgcp2p_stun_types::TsgcSTUNTransport FTransport;
	TsgcSTUNClient_Options* FSTUNOptions;
	System::UnicodeString __fastcall GetHost();
	Idglobal::TIdIPVersion __fastcall GetIPVersion();
	int __fastcall GetPort();
	Sgcwebsocket_types::TwsNotifyEvent __fastcall GetNotifyEvents();
	virtual void __fastcall SetLogFile(Sgcp2p_stun_classes::TsgcSTUNLogFile* const Value);
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall SetIPVersion(const Idglobal::TIdIPVersion Value);
	void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	void __fastcall SetPort(const int Value);
	void __fastcall SetSTUNOptions(TsgcSTUNClient_Options* const Value);
	
public:
	__property System::UnicodeString Host = {read=GetHost, write=SetHost};
	__property Idglobal::TIdIPVersion IPVersion = {read=GetIPVersion, write=SetIPVersion, nodefault};
	__property Sgcp2p_stun_classes::TsgcSTUNLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=GetNotifyEvents, write=SetNotifyEvents, nodefault};
	__property int Port = {read=GetPort, write=SetPort, nodefault};
	__property TsgcSTUNClient_Options* STUNOptions = {read=FSTUNOptions, write=SetSTUNOptions};
	__property Sgcp2p_stun_types::TsgcSTUNTransport Transport = {read=FTransport, write=FTransport, nodefault};
	
private:
	TsgcSTUNBeforeSendEvent FOnSTUNBeforeSend;
	Sgcp2p_stun_classes::TsgcSTUNExceptionEvent FOnSTUNException;
	TsgcSTUNResponseErrorEvent FOnSTUNResponseError;
	TsgcSTUNResponseSuccessEvent FOnSTUNResponseSuccess;
	Sgcp2p_stun_classes::TsgcICEValidateMessageIntegrityEvent FOnICEValidateMessageIntegrity;
	Sgcp2p_stun_classes::TsgcICERequestBindingEvent FOnICERequestBinding;
	Sgcp2p_stun_classes::TsgcICERequestMessageIntegrityPasswordEvent FOnICERequestMessageIntegrityPassword;
	Sgcudp_classes::TsgcUDPRetransmissionTimeoutEvent FOnRetransmissionTimeout;
	
protected:
	virtual void __fastcall DoSTUNExceptionEvent(System::Sysutils::Exception* E);
	__property Sgcp2p_stun_classes::TsgcICEValidateMessageIntegrityEvent OnICEValidateMessageIntegrity = {read=FOnICEValidateMessageIntegrity, write=FOnICEValidateMessageIntegrity};
	__property Sgcp2p_stun_classes::TsgcICERequestMessageIntegrityPasswordEvent OnICERequestMessageIntegrityPassword = {read=FOnICERequestMessageIntegrityPassword, write=FOnICERequestMessageIntegrityPassword};
	__property Sgcp2p_stun_classes::TsgcICERequestBindingEvent OnICERequestBinding = {read=FOnICERequestBinding, write=FOnICERequestBinding};
	__property Sgcudp_classes::TsgcUDPRetransmissionTimeoutEvent OnRetransmissionTimeout = {read=FOnRetransmissionTimeout, write=FOnRetransmissionTimeout};
	
public:
	__property Sgcp2p_stun_classes::TsgcSTUNExceptionEvent OnSTUNException = {read=FOnSTUNException, write=FOnSTUNException};
	__property TsgcSTUNResponseErrorEvent OnSTUNResponseError = {read=FOnSTUNResponseError, write=FOnSTUNResponseError};
	__property TsgcSTUNResponseSuccessEvent OnSTUNResponseSuccess = {read=FOnSTUNResponseSuccess, write=FOnSTUNResponseSuccess};
	__property TsgcSTUNBeforeSendEvent OnSTUNBeforeSend = {read=FOnSTUNBeforeSend, write=FOnSTUNBeforeSend};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word CS_STUN_PORT = System::Word(0xd96);
}	/* namespace Sgcp2p_stun_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCP2P_STUN_CLIENT)
using namespace Sgcp2p_stun_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcp2p_stun_clientHPP
