// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcP2P_STUN_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcp2p_stun_serverHPP
#define Sgcp2p_stun_serverHPP

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
#include <IdSocketHandle.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcP2P_STUN_Classes.hpp>
#include <sgcUDP_Server.hpp>
#include <sgcP2P_STUN_Types.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcp2p_stun_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSTUNServer_AlternateServer;
class DELPHICLASS TsgcSTUNServer_Authentication_LongTermCredentials;
class DELPHICLASS TsgcSTUNServer_Authentication_ShortTermCredentials;
class DELPHICLASS TsgcSTUNServer_BindingAttributes_Options;
class DELPHICLASS TsgcSTUNNonceItem;
class DELPHICLASS TsgcSTUNNoncesQueue;
class DELPHICLASS TsgcSTUNServer_Authentication;
class DELPHICLASS TsgcSTUNServer_Options;
class DELPHICLASS TsgcSTUNServer_Base;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcSTUNRequestSuccessEvent)(System::TObject* Sender, Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse, bool &Accept);

typedef void __fastcall (__closure *TsgcSTUNRequestErrorEvent)(System::TObject* Sender, Sgcsocket_classes::TsgcSocketConnection* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse, bool &Accept);

typedef void __fastcall (__closure *TsgcSTUNRequestAuthorizationEvent)(System::TObject* Sender, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, const System::UnicodeString aUsername, const System::UnicodeString aRealm, System::UnicodeString &Password);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_AlternateServer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDomain;
	bool FEnabled;
	Sgcwebsocket_types::TwsIPVersion FFamily;
	System::UnicodeString FIPAddress;
	System::Word FPort;
	
public:
	__fastcall virtual TsgcSTUNServer_AlternateServer();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Domain = {read=FDomain, write=FDomain};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property Sgcwebsocket_types::TwsIPVersion Family = {read=FFamily, write=FFamily, nodefault};
	__property System::UnicodeString IPAddress = {read=FIPAddress, write=FIPAddress};
	__property System::Word Port = {read=FPort, write=FPort, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNServer_AlternateServer() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_Authentication_LongTermCredentials : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FRealm;
	int FStaleNonce;
	
public:
	__fastcall virtual TsgcSTUNServer_Authentication_LongTermCredentials();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Realm = {read=FRealm, write=FRealm};
	__property int StaleNonce = {read=FStaleNonce, write=FStaleNonce, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNServer_Authentication_LongTermCredentials() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_Authentication_ShortTermCredentials : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	__fastcall virtual TsgcSTUNServer_Authentication_ShortTermCredentials();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNServer_Authentication_ShortTermCredentials() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_BindingAttributes_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcSTUNServer_AlternateServer* FAlternateServer;
	bool FMappedAddressRFC3489;
	bool FOtherAddress;
	bool FResponseOrigin;
	bool FSourceAddress;
	void __fastcall SetAlternateServer(TsgcSTUNServer_AlternateServer* const Value);
	
public:
	__fastcall virtual TsgcSTUNServer_BindingAttributes_Options();
	__fastcall virtual ~TsgcSTUNServer_BindingAttributes_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcSTUNServer_AlternateServer* AlternateServer = {read=FAlternateServer, write=SetAlternateServer};
	__property bool MappedAddressRFC3489 = {read=FMappedAddressRFC3489, write=FMappedAddressRFC3489, nodefault};
	__property bool OtherAddress = {read=FOtherAddress, write=FOtherAddress, nodefault};
	__property bool ResponseOrigin = {read=FResponseOrigin, write=FResponseOrigin, nodefault};
	__property bool SourceAddress = {read=FSourceAddress, write=FSourceAddress, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUNNonceItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::TDateTime FExpires;
	System::UnicodeString FRealm;
	
public:
	__property System::TDateTime Expires = {read=FExpires, write=FExpires};
	__property System::UnicodeString Realm = {read=FRealm, write=FRealm};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSTUNNonceItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUNNonceItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNNoncesQueue : public Sgcwebsocket_classes_queues::TsgcQueueBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueBase inherited;
	
private:
	int FStaleNonce;
	
public:
	System::UnicodeString __fastcall AddNonce(const System::UnicodeString aRealm);
	Sgcp2p_stun_types::TsgcStunNonceState __fastcall GetNonceState(const System::UnicodeString aNonce);
	__property int StaleNonce = {read=FStaleNonce, write=FStaleNonce, nodefault};
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSTUNNoncesQueue() : Sgcwebsocket_classes_queues::TsgcQueueBase() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSTUNNoncesQueue() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_Authentication : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	TsgcSTUNServer_Authentication_LongTermCredentials* FLongTermCredentials;
	TsgcSTUNServer_Authentication_ShortTermCredentials* FShortTermCredentials;
	void __fastcall SetLongTermCredentials(TsgcSTUNServer_Authentication_LongTermCredentials* const Value);
	void __fastcall SetShortTermCredentials(TsgcSTUNServer_Authentication_ShortTermCredentials* const Value);
	
public:
	__fastcall virtual TsgcSTUNServer_Authentication();
	__fastcall virtual ~TsgcSTUNServer_Authentication();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property TsgcSTUNServer_Authentication_LongTermCredentials* LongTermCredentials = {read=FLongTermCredentials, write=SetLongTermCredentials};
	__property TsgcSTUNServer_Authentication_ShortTermCredentials* ShortTermCredentials = {read=FShortTermCredentials, write=SetShortTermCredentials};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNServer_Options : public Sgcp2p_stun_classes::TsgcSTUN_Options
{
	typedef Sgcp2p_stun_classes::TsgcSTUN_Options inherited;
	
private:
	TsgcSTUNServer_Authentication* FAuthentication;
	TsgcSTUNServer_BindingAttributes_Options* FBindingAttributes;
	void __fastcall SetAuthentication(TsgcSTUNServer_Authentication* const Value);
	void __fastcall SetBindingAttributes(TsgcSTUNServer_BindingAttributes_Options* const Value);
	
public:
	__fastcall virtual TsgcSTUNServer_Options();
	__fastcall virtual ~TsgcSTUNServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcSTUNServer_Authentication* Authentication = {read=FAuthentication, write=SetAuthentication};
	__property TsgcSTUNServer_BindingAttributes_Options* BindingAttributes = {read=FBindingAttributes, write=SetBindingAttributes};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUNServer_Base : public Sgcp2p_stun_classes::TsgcSTUN_Component
{
	typedef Sgcp2p_stun_classes::TsgcSTUN_Component inherited;
	
private:
	Sgcudp_server::TsgcUDPServer_Base* FUDPServer;
	Sgcudp_server::TsgcUDPServer_Base* __fastcall GetUDPServer();
	
protected:
	virtual void __fastcall OnUDPReadEvent(System::TObject* Sender, Sgcudp_classes::TsgcUDPSocket* Socket, Sgcsocket_classes::TsgcBytes Bytes);
	virtual void __fastcall OnUDPExceptionEvent(System::TObject* Sender, Sgcudp_classes::TsgcUDPSocket* Socket, System::Sysutils::Exception* E);
	__property Sgcudp_server::TsgcUDPServer_Base* UDPServer = {read=GetUDPServer, write=FUDPServer};
	
public:
	Idsockethandle::TIdSocketHandle* __fastcall AddBinding(const System::UnicodeString aIPAddress, int aPort);
	bool __fastcall RemoveBinding(const System::UnicodeString aIPAddress, int aPort);
	
private:
	TsgcSTUNNoncesQueue* FNonces;
	TsgcSTUNNoncesQueue* __fastcall GetNonces();
	
protected:
	__property TsgcSTUNNoncesQueue* Nonces = {read=GetNonces, write=FNonces};
	
public:
	__fastcall virtual TsgcSTUNServer_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcSTUNServer_Base();
	
private:
	Sgcp2p_stun_classes::TsgcSTUNLogFile* FLogFile;
	TsgcSTUNServer_Options* FSTUNOptions;
	bool __fastcall GetActive();
	Idsockethandle::TIdSocketHandles* __fastcall GetBindings();
	System::UnicodeString __fastcall GetHost();
	Idglobal::TIdIPVersion __fastcall GetIPVersion();
	int __fastcall GetPort();
	Sgcwebsocket_types::TwsNotifyEvent __fastcall GetNotifyEvents();
	void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	virtual void __fastcall SetLogFile(Sgcp2p_stun_classes::TsgcSTUNLogFile* const Value);
	void __fastcall SetBindings(Idsockethandle::TIdSocketHandles* const Value);
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall SetIPVersion(const Idglobal::TIdIPVersion Value);
	void __fastcall SetPort(const int Value);
	void __fastcall SetSTUNOptions(TsgcSTUNServer_Options* const Value);
	
protected:
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property Idsockethandle::TIdSocketHandles* Bindings = {read=GetBindings, write=SetBindings};
	__property System::UnicodeString Host = {read=GetHost, write=SetHost};
	__property Idglobal::TIdIPVersion IPVersion = {read=GetIPVersion, write=SetIPVersion, nodefault};
	__property Sgcp2p_stun_classes::TsgcSTUNLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=GetNotifyEvents, write=SetNotifyEvents, nodefault};
	__property int Port = {read=GetPort, write=SetPort, nodefault};
	__property TsgcSTUNServer_Options* STUNOptions = {read=FSTUNOptions, write=SetSTUNOptions};
	
private:
	bool __fastcall IsValidIPAddress(const System::UnicodeString aIPAddress);
	
protected:
	virtual void __fastcall DoAddMappedAddress(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoAddResponseOrigin(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoAddSourceAddress(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoAddOtherAddress(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoAddSoftware(Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual bool __fastcall DoValidateRequest_LongTermCredentials(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse, TsgcSTUNServer_Authentication_LongTermCredentials* const aLongTermCredentials);
	bool __fastcall DoValidateRequest_RequiredAttributes(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall OnRequestMessageIntegrityPasswordEvent(System::TObject* Sender, const System::UnicodeString aUsername, const System::UnicodeString aRealm, System::UnicodeString &Password);
	virtual bool __fastcall DoValidateRequest(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse, TsgcSTUNServer_Authentication* const aAuthentication);
	virtual void __fastcall DoRequestSuccess(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoRequestError(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse, Sgcp2p_stun_types::TsgcStunErrorResponseCodes aError);
	virtual void __fastcall DoSendResponseSuccess(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoSendResponseError(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoSTUNRequestMethod(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoSTUNIndication(Sgcudp_classes::TsgcUDPSocket* const aSocket, Sgcp2p_stun_classes::TsgcSTUN_Message* const aRequest, Sgcp2p_stun_classes::TsgcSTUN_Message* const aResponse);
	virtual void __fastcall DoReadData(Sgcudp_classes::TsgcUDPSocket* const Socket, const System::DynamicArray<System::Byte> Bytes);
	Sgcp2p_stun_classes::TsgcSTUNExceptionEvent FOnSTUNException;
	TsgcSTUNRequestAuthorizationEvent FOnSTUNRequestAuthorization;
	TsgcSTUNRequestErrorEvent FOnSTUNRequestError;
	TsgcSTUNRequestSuccessEvent FOnSTUNRequestSuccess;
	virtual void __fastcall DoSTUNExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property Sgcp2p_stun_classes::TsgcSTUNExceptionEvent OnSTUNException = {read=FOnSTUNException, write=FOnSTUNException};
	__property TsgcSTUNRequestErrorEvent OnSTUNRequestError = {read=FOnSTUNRequestError, write=FOnSTUNRequestError};
	__property TsgcSTUNRequestSuccessEvent OnSTUNRequestSuccess = {read=FOnSTUNRequestSuccess, write=FOnSTUNRequestSuccess};
	__property TsgcSTUNRequestAuthorizationEvent OnSTUNRequestAuthorization = {read=FOnSTUNRequestAuthorization, write=FOnSTUNRequestAuthorization};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcp2p_stun_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCP2P_STUN_SERVER)
using namespace Sgcp2p_stun_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcp2p_stun_serverHPP
