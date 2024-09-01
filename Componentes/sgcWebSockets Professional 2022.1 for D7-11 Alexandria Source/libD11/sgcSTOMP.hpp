// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcSTOMP.pas' rev: 35.00 (Windows)

#ifndef SgcstompHPP
#define SgcstompHPP

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
#include <System.Math.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcstomp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSTOMPAuthentication;
class DELPHICLASS TsgcSTOMPHeartBeat;
class DELPHICLASS TsgcSTOMPVersion;
class DELPHICLASS TsgcSTOMP;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcSTOMPConnectedEvent)(System::TObject* Sender, System::UnicodeString Version, System::UnicodeString Server, System::UnicodeString Session, System::UnicodeString HeartBeat, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcSTOMPErrorEvent)(System::TObject* Sender, System::UnicodeString MessageText, System::UnicodeString ContentType, int ContentLength, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcSTOMPMessageEvent)(System::TObject* Sender, System::UnicodeString MessageText, System::UnicodeString Destination, System::UnicodeString MessageId, System::UnicodeString Subscription, System::UnicodeString ACK, System::UnicodeString ContentType, System::UnicodeString RawHeaders);

typedef void __fastcall (__closure *TsgcSTOMPReceiptEvent)(System::TObject* Sender, System::UnicodeString ReceiptId, System::UnicodeString RawHeaders);

enum DECLSPEC_DENUM TsgcSTOMPACK : unsigned char { ackAuto, ackMultiple, ackIndividual };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTOMPAuthentication : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FPassword;
	System::UnicodeString FUsername;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTOMPAuthentication() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcSTOMPAuthentication() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTOMPHeartBeat : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FIncoming;
	int FOutgoing;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Incoming = {read=FIncoming, write=FIncoming, nodefault};
	__property int Outgoing = {read=FOutgoing, write=FOutgoing, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTOMPHeartBeat() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcSTOMPHeartBeat() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTOMPVersion : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FV1_2;
	bool FV1_0;
	bool FV1_1;
	
public:
	__fastcall virtual TsgcSTOMPVersion();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool V1_2 = {read=FV1_2, write=FV1_2, nodefault};
	__property bool V1_0 = {read=FV1_0, write=FV1_0, nodefault};
	__property bool V1_1 = {read=FV1_1, write=FV1_1, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTOMPVersion() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTOMP : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TStringList* FHeaders;
	System::UnicodeString FBody;
	
protected:
	void __fastcall DoClear();
	void __fastcall AddCommand(const System::UnicodeString aCommand);
	void __fastcall AddHeader(const System::UnicodeString aName, const System::UnicodeString aValue);
	System::UnicodeString __fastcall Write();
	
public:
	void __fastcall Clear();
	
private:
	TsgcSTOMPAuthentication* FAuthentication;
	void __fastcall SetAuthentication(TsgcSTOMPAuthentication* const Value);
	
public:
	__property TsgcSTOMPAuthentication* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	TsgcSTOMPHeartBeat* FHeartBeat;
	void __fastcall SetHeartBeat(TsgcSTOMPHeartBeat* const Value);
	
public:
	__property TsgcSTOMPHeartBeat* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	TsgcSTOMPVersion* FVersion;
	System::UnicodeString __fastcall GetAcceptVersion();
	void __fastcall SetVersion(TsgcSTOMPVersion* const Value);
	
public:
	__property TsgcSTOMPVersion* Version = {read=FVersion, write=SetVersion};
	__fastcall virtual TsgcSTOMP(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcSTOMP();
	
private:
	int FReceipt;
	int __fastcall ReadContentLength(const System::UnicodeString aMessage);
	
protected:
	void __fastcall DoConnect(const System::UnicodeString aHost, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	void __fastcall DoSend(const System::UnicodeString aDestination, const System::UnicodeString aMessage, const System::UnicodeString aContentType, const System::UnicodeString aTransaction, System::Classes::TStrings* const aCustomHeaders);
	void __fastcall DoSubscribe(const System::UnicodeString aId, const System::UnicodeString aDestination, const TsgcSTOMPACK aACK, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	void __fastcall DoUnSubscribe(const System::UnicodeString aId, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	void __fastcall DoACK(const System::UnicodeString aId, const System::UnicodeString aTransaction);
	void __fastcall DoNACK(const System::UnicodeString aId, const System::UnicodeString aTransaction);
	void __fastcall DoBeginTransaction(const System::UnicodeString aTransaction);
	void __fastcall DoCommitTransaction(const System::UnicodeString aTransaction);
	void __fastcall DoAbortTransaction(const System::UnicodeString aTransaction);
	void __fastcall DoDisconnect(const bool aGraceful);
	
public:
	System::UnicodeString __fastcall Connect(const System::UnicodeString aHost, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall Send(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType = System::UnicodeString(), const System::UnicodeString aTransaction = System::UnicodeString(), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall Subscribe(const System::UnicodeString aId, const System::UnicodeString aDestination, const TsgcSTOMPACK aACK = (TsgcSTOMPACK)(0x0), System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall UnSubscribe(const System::UnicodeString aId, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall ACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	System::UnicodeString __fastcall NACK(const System::UnicodeString aId, const System::UnicodeString aTransaction = System::UnicodeString());
	System::UnicodeString __fastcall BeginTransaction(const System::UnicodeString aTransaction);
	System::UnicodeString __fastcall CommitTransaction(const System::UnicodeString aTransaction);
	System::UnicodeString __fastcall AbortTransaction(const System::UnicodeString aTransaction);
	System::UnicodeString __fastcall Disconnect(const bool aGraceful = true);
	System::UnicodeString __fastcall Ping();
	System::UnicodeString __fastcall Pong();
	
private:
	System::UnicodeString __fastcall GetHeaderByName(const System::UnicodeString aName);
	System::UnicodeString __fastcall GetContentType();
	int __fastcall GetContentLength();
	System::UnicodeString __fastcall GetReceiptId();
	System::UnicodeString __fastcall GetVersion();
	System::UnicodeString __fastcall GetServer();
	System::UnicodeString __fastcall GetSession();
	System::UnicodeString __fastcall GetHeartBeat();
	System::UnicodeString __fastcall GetDestination();
	System::UnicodeString __fastcall GetMessageId();
	System::UnicodeString __fastcall GetSubscription();
	System::UnicodeString __fastcall GetACK();
	
protected:
	bool __fastcall DoReadConnected();
	bool __fastcall DoReadError();
	bool __fastcall DoReadMessage();
	bool __fastcall DoReadReceipt();
	bool __fastcall DoRead(System::UnicodeString aText);
	
public:
	bool __fastcall Read(const System::UnicodeString aText);
	
private:
	TsgcSTOMPConnectedEvent FOnConnected;
	TsgcSTOMPErrorEvent FOnError;
	TsgcSTOMPMessageEvent FOnMessage;
	TsgcSTOMPReceiptEvent FOnReceipt;
	
protected:
	void __fastcall DoEventConnected(System::UnicodeString aVersion, System::UnicodeString aServer, System::UnicodeString aSession, System::UnicodeString aHeartBeat);
	void __fastcall DoEventError(System::UnicodeString aMessageText, System::UnicodeString aContentType, int aContentLength, System::UnicodeString aReceiptId);
	void __fastcall DoEventMessage(System::UnicodeString aMessageText, System::UnicodeString aDestination, System::UnicodeString aMessageId, System::UnicodeString aSubscription, System::UnicodeString aACK, System::UnicodeString aContentType);
	void __fastcall DoEventReceipt(System::UnicodeString aReceiptId);
	
public:
	__property TsgcSTOMPConnectedEvent OnConnected = {read=FOnConnected, write=FOnConnected};
	__property TsgcSTOMPErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TsgcSTOMPMessageEvent OnMessage = {read=FOnMessage, write=FOnMessage};
	__property TsgcSTOMPReceiptEvent OnReceipt = {read=FOnReceipt, write=FOnReceipt};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcstomp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCSTOMP)
using namespace Sgcstomp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgcstompHPP
