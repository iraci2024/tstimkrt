// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_STOMP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_stomp_clientHPP
#define Sgcwebsocket_protocol_stomp_clientHPP

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
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSTOMP.hpp>
#include <sgcWebSocket_Protocol_STOMP_Message.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_stomp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSSTOMPAuthentication_Options;
class DELPHICLASS TsgcWSSTOMPHeartBeat_Options;
class DELPHICLASS TsgcWSSTOMPVersion_Options;
class DELPHICLASS TsgcWSSTOMP_Options;
class DELPHICLASS TsgcWSProtocol_STOMP_Client_Base;
class DELPHICLASS TsgcWSProtocol_STOMP_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSSTOMPConnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Version, System::UnicodeString Server, System::UnicodeString Session, System::UnicodeString HeartBeat, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcWSSTOMPErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, System::UnicodeString ContentType, int ContentLength, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcWSSTOMPMessageEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, System::UnicodeString Destination, System::UnicodeString MessageId, System::UnicodeString Subscription, System::UnicodeString ACK, System::UnicodeString ContentType, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcWSSTOMPReceiptEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcWSSTOMPDisconnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSTOMPAuthentication_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FPassword;
	System::UnicodeString FUserName;
	
public:
	__fastcall virtual TsgcWSSTOMPAuthentication_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString UserName = {read=FUserName, write=FUserName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSTOMPAuthentication_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSTOMPHeartBeat_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FIncoming;
	int FOutgoing;
	
public:
	__fastcall virtual TsgcWSSTOMPHeartBeat_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Incoming = {read=FIncoming, write=FIncoming, nodefault};
	__property int Outgoing = {read=FOutgoing, write=FOutgoing, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSTOMPHeartBeat_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSTOMPVersion_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FV1_2;
	bool FV1_0;
	bool FV1_1;
	
public:
	__fastcall virtual TsgcWSSTOMPVersion_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool V1_2 = {read=FV1_2, write=FV1_2, nodefault};
	__property bool V1_0 = {read=FV1_0, write=FV1_0, nodefault};
	__property bool V1_1 = {read=FV1_1, write=FV1_1, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSTOMPVersion_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSTOMP_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FVirtualHost;
	
public:
	__fastcall virtual TsgcWSSTOMP_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString VirtualHost = {read=FVirtualHost, write=FVirtualHost};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSTOMP_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_STOMP_Client_Base : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base inherited;
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_stomp_message::TsgcWSSTOMPMessage* FWSMessage;
	Sgcwebsocket_protocol_stomp_message::TsgcWSSTOMPMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_stomp_message::TsgcWSSTOMPMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	void __fastcall OnSTOMPConnectedEvent(System::TObject* Sender, System::UnicodeString aVersion, System::UnicodeString aServer, System::UnicodeString aSession, System::UnicodeString aHeartBeat, System::UnicodeString aRawHeaders);
	void __fastcall OnSTOMPErrorEvent(System::TObject* Sender, System::UnicodeString aMessageText, System::UnicodeString aContentType, int aContentLength, System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	void __fastcall OnSTOMPMessageEvent(System::TObject* Sender, System::UnicodeString aMessageText, System::UnicodeString aDestination, System::UnicodeString aMessageId, System::UnicodeString aSubscription, System::UnicodeString aACK, System::UnicodeString aContentType, System::UnicodeString aRawHeaders);
	void __fastcall OnSTOMPReceiptEvent(System::TObject* Sender, System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	__property Sgcwebsocket_protocol_stomp_message::TsgcWSSTOMPMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoWriteRawData(const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWSProtocol_STOMP_Client_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_STOMP_Client_Base();
	
protected:
	System::Classes::TStringList* FConnectHeaders;
	System::Classes::TStringList* __fastcall GetConnectHeaders();
	virtual void __fastcall DoConnect(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall Send(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType = System::UnicodeString(), const System::UnicodeString aTransaction = System::UnicodeString(), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall Subscribe(const System::UnicodeString aId, const System::UnicodeString aDestination, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall UnSubscribe(const System::UnicodeString aId, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall ACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall NACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall BeginTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall CommitTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall AbortTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall Disconnect(const bool aGraceful = true);
	
private:
	TsgcWSSTOMPAuthentication_Options* FAuthentication;
	void __fastcall SetAuthentication(TsgcWSSTOMPAuthentication_Options* const Value);
	
public:
	__property TsgcWSSTOMPAuthentication_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	TsgcWSSTOMPHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(TsgcWSSTOMPHeartBeat_Options* const Value);
	
public:
	__property TsgcWSSTOMPHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	TsgcWSSTOMPVersion_Options* FVersions;
	System::UnicodeString __fastcall GetProtocolByVersion();
	void __fastcall SetVersions(TsgcWSSTOMPVersion_Options* const Value);
	
public:
	__property TsgcWSSTOMPVersion_Options* Versions = {read=FVersions, write=SetVersions};
	
private:
	TsgcWSSTOMP_Options* FOptions;
	void __fastcall SetOptions(TsgcWSSTOMP_Options* const Value);
	
public:
	__property TsgcWSSTOMP_Options* Options = {read=FOptions, write=SetOptions};
	
private:
	TsgcWSSTOMPConnectedEvent FOnSTOMPConnected;
	TsgcWSSTOMPDisconnectedEvent FOnSTOMPDisconnected;
	TsgcWSSTOMPErrorEvent FOnSTOMPError;
	TsgcWSSTOMPMessageEvent FOnSTOMPMessage;
	TsgcWSSTOMPReceiptEvent FOnSTOMPReceipt;
	void __fastcall DoSTOMPConnectedEvent(System::UnicodeString aVersion, System::UnicodeString aServer, System::UnicodeString aSession, System::UnicodeString aHeartBeat, System::UnicodeString aRawHeaders);
	void __fastcall DoSTOMPErrorEvent(System::UnicodeString aMessageText, System::UnicodeString aContentType, int aContentLength, System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	void __fastcall DoSTOMPMessageEvent(System::UnicodeString aMessageText, System::UnicodeString aDestination, System::UnicodeString aMessageId, System::UnicodeString aSubscription, System::UnicodeString aACK, System::UnicodeString aContentType, System::UnicodeString aRawHeaders);
	void __fastcall DoSTOMPReceiptEvent(System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	void __fastcall DoSTOMPDisconnectedEvent(int Code);
	
public:
	__property TsgcWSSTOMPErrorEvent OnSTOMPError = {read=FOnSTOMPError, write=FOnSTOMPError};
	__property TsgcWSSTOMPConnectedEvent OnSTOMPConnected = {read=FOnSTOMPConnected, write=FOnSTOMPConnected};
	__property TsgcWSSTOMPMessageEvent OnSTOMPMessage = {read=FOnSTOMPMessage, write=FOnSTOMPMessage};
	__property TsgcWSSTOMPReceiptEvent OnSTOMPReceipt = {read=FOnSTOMPReceipt, write=FOnSTOMPReceipt};
	__property TsgcWSSTOMPDisconnectedEvent OnSTOMPDisconnected = {read=FOnSTOMPDisconnected, write=FOnSTOMPDisconnected};
};


class PASCALIMPLEMENTATION TsgcWSProtocol_STOMP_Client : public TsgcWSProtocol_STOMP_Client_Base
{
	typedef TsgcWSProtocol_STOMP_Client_Base inherited;
	
public:
	virtual void __fastcall Send(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType = System::UnicodeString(), const System::UnicodeString aTransaction = System::UnicodeString(), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall Subscribe(const System::UnicodeString aId, const System::UnicodeString aDestination, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall UnSubscribe(const System::UnicodeString aId, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall ACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall NACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall BeginTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall CommitTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall AbortTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall Disconnect(const bool aGraceful = true);
	__property System::Classes::TStringList* ConnectHeaders = {read=GetConnectHeaders, write=FConnectHeaders};
public:
	/* TsgcWSProtocol_STOMP_Client_Base.Create */ inline __fastcall virtual TsgcWSProtocol_STOMP_Client(System::Classes::TComponent* aOwner) : TsgcWSProtocol_STOMP_Client_Base(aOwner) { }
	/* TsgcWSProtocol_STOMP_Client_Base.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_STOMP_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_stomp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_STOMP_CLIENT)
using namespace Sgcwebsocket_protocol_stomp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_stomp_clientHPP
