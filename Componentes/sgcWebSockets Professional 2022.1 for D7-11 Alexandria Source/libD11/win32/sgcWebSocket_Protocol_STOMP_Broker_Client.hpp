// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_STOMP_Broker_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_stomp_broker_clientHPP
#define Sgcwebsocket_protocol_stomp_broker_clientHPP

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
#include <sgcWebSocket_Protocol_STOMP_Client.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcSTOMP.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_stomp_broker_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSBrokerSTOMPHeadersBase;
class DELPHICLASS TsgcWSBrokerSTOMPHeadersConnected;
class DELPHICLASS TsgcWSBrokerSTOMPHeadersMessage;
class DELPHICLASS TsgcWSBrokerSTOMPHeadersError;
class DELPHICLASS TsgcWSBrokerSTOMPHeadersReceipt;
class DELPHICLASS TsgcWSBrokerSTOMPSubscriptionItem;
class DELPHICLASS TsgcWSBrokerSTOMPSubscriptionList;
class DELPHICLASS TsgcWSBrokerSTOMP_Options;
class DELPHICLASS TsgcWSProtocol_STOMP_Broker_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSBrokerACK : unsigned char { ackAutomatic, ackAutoIndividual, ackManual };

class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPHeadersBase : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString __fastcall GetText();
	
protected:
	System::Classes::TStringList* FHeaders;
	System::UnicodeString __fastcall GetHeaderByName(const System::UnicodeString aName);
	virtual void __fastcall DoRead() = 0 ;
	virtual void __fastcall Read(const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWSBrokerSTOMPHeadersBase(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSBrokerSTOMPHeadersBase();
	System::UnicodeString __fastcall Header(const System::UnicodeString aName);
	bool __fastcall HeaderExists(const System::UnicodeString aName);
	__property System::UnicodeString Text = {read=GetText};
};


class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPHeadersConnected : public TsgcWSBrokerSTOMPHeadersBase
{
	typedef TsgcWSBrokerSTOMPHeadersBase inherited;
	
private:
	System::UnicodeString FHeartBeat;
	System::UnicodeString FServer;
	System::UnicodeString FSession;
	System::UnicodeString FVersion;
	
protected:
	virtual void __fastcall DoRead();
	
public:
	__property System::UnicodeString HeartBeat = {read=FHeartBeat, write=FHeartBeat};
	__property System::UnicodeString Server = {read=FServer, write=FServer};
	__property System::UnicodeString Session = {read=FSession, write=FSession};
	__property System::UnicodeString Version = {read=FVersion, write=FVersion};
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSBrokerSTOMPHeadersConnected(System::Classes::TComponent* aOwner) : TsgcWSBrokerSTOMPHeadersBase(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMPHeadersConnected() { }
	
};


class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPHeadersMessage : public TsgcWSBrokerSTOMPHeadersBase
{
	typedef TsgcWSBrokerSTOMPHeadersBase inherited;
	
private:
	System::UnicodeString FACK;
	int FContentLength;
	System::UnicodeString FContentType;
	System::UnicodeString FDestination;
	System::UnicodeString FMessageId;
	bool FRedelivered;
	System::UnicodeString FReplyTo;
	System::UnicodeString FSubscription;
	
protected:
	virtual void __fastcall DoRead();
	
public:
	__property System::UnicodeString ACK = {read=FACK, write=FACK};
	__property int ContentLength = {read=FContentLength, write=FContentLength, nodefault};
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString Destination = {read=FDestination, write=FDestination};
	__property System::UnicodeString MessageId = {read=FMessageId, write=FMessageId};
	__property bool Redelivered = {read=FRedelivered, write=FRedelivered, nodefault};
	__property System::UnicodeString ReplyTo = {read=FReplyTo, write=FReplyTo};
	__property System::UnicodeString Subscription = {read=FSubscription, write=FSubscription};
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSBrokerSTOMPHeadersMessage(System::Classes::TComponent* aOwner) : TsgcWSBrokerSTOMPHeadersBase(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMPHeadersMessage() { }
	
};


class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPHeadersError : public TsgcWSBrokerSTOMPHeadersBase
{
	typedef TsgcWSBrokerSTOMPHeadersBase inherited;
	
private:
	System::UnicodeString FACK;
	int FContentLength;
	System::UnicodeString FContentType;
	System::UnicodeString FDestination;
	System::UnicodeString FMessageId;
	System::UnicodeString FMessageText;
	System::UnicodeString FSubscription;
	System::UnicodeString FVersion;
	
protected:
	virtual void __fastcall DoRead();
	
public:
	__property System::UnicodeString ACK = {read=FACK, write=FACK};
	__property int ContentLength = {read=FContentLength, write=FContentLength, nodefault};
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString Destination = {read=FDestination, write=FDestination};
	__property System::UnicodeString MessageId = {read=FMessageId, write=FMessageId};
	__property System::UnicodeString MessageText = {read=FMessageText, write=FMessageText};
	__property System::UnicodeString Subscription = {read=FSubscription, write=FSubscription};
	__property System::UnicodeString Version = {read=FVersion, write=FVersion};
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSBrokerSTOMPHeadersError(System::Classes::TComponent* aOwner) : TsgcWSBrokerSTOMPHeadersBase(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMPHeadersError() { }
	
};


class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPHeadersReceipt : public TsgcWSBrokerSTOMPHeadersBase
{
	typedef TsgcWSBrokerSTOMPHeadersBase inherited;
	
private:
	System::UnicodeString FReceiptId;
	
protected:
	virtual void __fastcall DoRead();
	
public:
	__property System::UnicodeString ReceiptId = {read=FReceiptId, write=FReceiptId};
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSBrokerSTOMPHeadersReceipt(System::Classes::TComponent* aOwner) : TsgcWSBrokerSTOMPHeadersBase(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMPHeadersReceipt() { }
	
};


class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPSubscriptionItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sgcstomp::TsgcSTOMPACK FACK;
	System::TDateTime FDate;
	System::Classes::TStringList* FHeaders;
	System::UnicodeString FId;
	System::UnicodeString FName;
	System::UnicodeString FReplyTo;
	System::Classes::TStringList* __fastcall GetHeaders();
	
public:
	__fastcall virtual ~TsgcWSBrokerSTOMPSubscriptionItem();
	__property Sgcstomp::TsgcSTOMPACK ACK = {read=FACK, write=FACK, nodefault};
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::Classes::TStringList* Headers = {read=GetHeaders, write=FHeaders};
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString ReplyTo = {read=FReplyTo, write=FReplyTo};
public:
	/* TObject.Create */ inline __fastcall TsgcWSBrokerSTOMPSubscriptionItem() : System::TObject() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBrokerSTOMPSubscriptionList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSBrokerSTOMPSubscriptionList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSBrokerSTOMPSubscriptionList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMPSubscriptionList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBrokerSTOMP_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSBrokerACK FAcknowledgments;
	
public:
	__fastcall virtual TsgcWSBrokerSTOMP_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSBrokerACK Acknowledgments = {read=FAcknowledgments, write=FAcknowledgments, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSBrokerSTOMP_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_STOMP_Broker_Client : public Sgcwebsocket_protocol_stomp_client::TsgcWSProtocol_STOMP_Client_Base
{
	typedef Sgcwebsocket_protocol_stomp_client::TsgcWSProtocol_STOMP_Client_Base inherited;
	
protected:
	void __fastcall OnBrokerConnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Version, System::UnicodeString Server, System::UnicodeString Session, System::UnicodeString HeartBeat, System::UnicodeString RawHeaders);
	void __fastcall OnBrokerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aDestination, System::UnicodeString aMessageId, System::UnicodeString aSubscription, System::UnicodeString aACK, System::UnicodeString aContentType, System::UnicodeString aRawHeaders);
	void __fastcall OnBrokerErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, System::UnicodeString ContentType, int ContentLength, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);
	void __fastcall OnBrokerReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);
	void __fastcall OnBrokerDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);
	virtual void __fastcall DoBrokerConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders) = 0 ;
	virtual void __fastcall DoBrokerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders, TsgcWSBrokerSTOMPSubscriptionItem* aSubscription) = 0 ;
	virtual void __fastcall DoBrokerErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders) = 0 ;
	virtual void __fastcall DoBrokerReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders) = 0 ;
	virtual void __fastcall DoBrokerDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode) = 0 ;
	
private:
	TsgcWSBrokerSTOMP_Options* FBroker_Options;
	void __fastcall SetBroker_Options(TsgcWSBrokerSTOMP_Options* const Value);
	
public:
	__property TsgcWSBrokerSTOMP_Options* Broker_Options = {read=FBroker_Options, write=SetBroker_Options};
	
protected:
	void __fastcall DoAutomaticACK(TsgcWSBrokerSTOMPSubscriptionItem* oSubscription, System::UnicodeString aACK, System::UnicodeString aMessageId);
	virtual System::UnicodeString __fastcall GetNewId();
	
private:
	TsgcWSBrokerSTOMPSubscriptionList* FSubscriptionList;
	TsgcWSBrokerSTOMPSubscriptionList* __fastcall GetSubscriptionList();
	
protected:
	virtual void __fastcall DoQueueSubscription(const System::UnicodeString aId, const System::UnicodeString aName, const Sgcstomp::TsgcSTOMPACK aACK, System::Classes::TStrings* const aHeaders);
	virtual void __fastcall DoDeleteSubscription(const System::UnicodeString aId);
	__property TsgcWSBrokerSTOMPSubscriptionList* SubscriptionList = {read=GetSubscriptionList};
	
public:
	TsgcWSBrokerSTOMPSubscriptionItem* __fastcall GetSubscriptionByName(const System::UnicodeString aName);
	TsgcWSBrokerSTOMPSubscriptionItem* __fastcall GetSubscriptionById(const System::UnicodeString aId);
	__fastcall virtual TsgcWSProtocol_STOMP_Broker_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_STOMP_Broker_Client();
	virtual void __fastcall BeginTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall CommitTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall AbortTransaction(const System::UnicodeString aTransaction);
	virtual void __fastcall ACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall NACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	virtual void __fastcall Disconnect(const bool aGraceful = true);
	__property System::Classes::TStringList* ConnectHeaders = {read=GetConnectHeaders, write=FConnectHeaders};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_stomp_broker_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_STOMP_BROKER_CLIENT)
using namespace Sgcwebsocket_protocol_stomp_broker_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_stomp_broker_clientHPP
