// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgciot_clientHPP
#define Sgciot_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdSSL.hpp>
#include <IdSSLOpenSSL.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcIoT_Classes.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoTComponent_Client_Certificate;
class DELPHICLASS TsgcIoTComponent_Client_SAS;
class DELPHICLASS TsgcIoT_WSClient;
class DELPHICLASS TsgcIoTComponent_Client;
//-- type declarations -------------------------------------------------------
typedef Sgcwebsocket_classes::TsgcWSConnectEvent TsgcIoTConnectEvent;

typedef Sgcwebsocket_classes::TsgcWSDisconnectEvent TsgcIoTDisconnectEvent;

typedef Sgcwebsocket_classes::TsgcWSErrorEvent TsgcIoTErrorEvent;

typedef Sgcwebsocket_classes::TsgcExceptionEvent TsgcIoTExceptionEvent;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoTComponent_Client_Certificate : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FCertFile;
	bool FEnabled;
	Sgcwebsocket_types::TwsTLSIOHandler FIOHandler;
	System::UnicodeString FKeyFile;
	Sgcsocket_classes::TsgcOpenSSLClient_Options* FOpenSSL_Options;
	System::UnicodeString FPassword;
	Sgcsocket_classes::TsgcSChannelClient_Options* FSChannel_Options;
	Sgcwebsocket_types::TwsTLSVersions FVersion;
	void __fastcall SetOpenSSL_Options(Sgcsocket_classes::TsgcOpenSSLClient_Options* const Value);
	void __fastcall SetSChannel_Options(Sgcsocket_classes::TsgcSChannelClient_Options* const Value);
	
public:
	__fastcall virtual TsgcIoTComponent_Client_Certificate();
	__fastcall virtual ~TsgcIoTComponent_Client_Certificate();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString CertFile = {read=FCertFile, write=FCertFile};
	__property System::UnicodeString KeyFile = {read=FKeyFile, write=FKeyFile};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property Sgcwebsocket_types::TwsTLSIOHandler IOHandler = {read=FIOHandler, write=FIOHandler, nodefault};
	__property Sgcsocket_classes::TsgcOpenSSLClient_Options* OpenSSL_Options = {read=FOpenSSL_Options, write=SetOpenSSL_Options};
	__property Sgcsocket_classes::TsgcSChannelClient_Options* SChannel_Options = {read=FSChannel_Options, write=SetSChannel_Options};
	__property Sgcwebsocket_types::TwsTLSVersions Version = {read=FVersion, write=FVersion, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoTComponent_Client_SAS : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FExpiry;
	System::UnicodeString FSecretKey;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Expiry = {read=FExpiry, write=FExpiry, nodefault};
	__property System::UnicodeString SecretKey = {read=FSecretKey, write=FSecretKey};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcIoTComponent_Client_SAS() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcIoTComponent_Client_SAS() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcIoT_WSClient : public Sgcwebsocket_client::TsgcWSClient
{
	typedef Sgcwebsocket_client::TsgcWSClient inherited;
	
private:
	System::Classes::TNotifyEvent FOnBeforeWatchDog;
	
protected:
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	
public:
	__property System::Classes::TNotifyEvent OnBeforeWatchDog = {read=FOnBeforeWatchDog, write=FOnBeforeWatchDog};
public:
	/* TsgcWSClient.Create */ inline __fastcall virtual TsgcIoT_WSClient(System::Classes::TComponent* aOwner) : Sgcwebsocket_client::TsgcWSClient(aOwner) { }
	/* TsgcWSClient.Destroy */ inline __fastcall virtual ~TsgcIoT_WSClient() { }
	
};


class PASCALIMPLEMENTATION TsgcIoTComponent_Client : public Sgciot_classes::TsgcIoTComponent_Base_Client
{
	typedef Sgciot_classes::TsgcIoTComponent_Base_Client inherited;
	
protected:
	virtual void __fastcall Loaded();
	
private:
	TsgcIoT_WSClient* FTCPClient;
	TsgcIoT_WSClient* __fastcall GetTCPClient();
	
protected:
	virtual void __fastcall OnConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection);
	virtual void __fastcall OnDisconnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);
	virtual void __fastcall OnErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Error);
	virtual void __fastcall OnExceptionEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Sysutils::Exception* E);
	virtual void __fastcall OnBeforeWatchDogEvent(System::TObject* Sender);
	__property TsgcIoT_WSClient* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
private:
	Sgcwebsocket_classes::TsgcWSHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(Sgcwebsocket_classes::TsgcWSHeartBeat_Options* const Value);
	
protected:
	__property Sgcwebsocket_classes::TsgcWSHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	Sgcwebsocket_classes::TsgcWSWatchDogClient_Options* FWatchDog;
	void __fastcall SetWatchDog(Sgcwebsocket_classes::TsgcWSWatchDogClient_Options* const Value);
	
protected:
	__property Sgcwebsocket_classes::TsgcWSWatchDogClient_Options* WatchDog = {read=FWatchDog, write=SetWatchDog};
	
private:
	Sgcwebsocket_classes::TsgcWSLogFile* FLogFile;
	void __fastcall SetLogFile(Sgcwebsocket_classes::TsgcWSLogFile* const Value);
	
protected:
	__property Sgcwebsocket_classes::TsgcWSLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	
private:
	TsgcIoTComponent_Client_Certificate* FCertificate;
	TsgcIoTComponent_Client_Certificate* __fastcall GetCertificate();
	void __fastcall SetCertificate(TsgcIoTComponent_Client_Certificate* const Value);
	
protected:
	__property TsgcIoTComponent_Client_Certificate* Certificate = {read=GetCertificate, write=SetCertificate};
	
private:
	TsgcIoTComponent_Client_SAS* FSAS;
	TsgcIoTComponent_Client_SAS* __fastcall GetSAS();
	void __fastcall SetSAS(TsgcIoTComponent_Client_SAS* const Value);
	
protected:
	__property TsgcIoTComponent_Client_SAS* SAS = {read=GetSAS, write=SetSAS};
	
private:
	bool FActive;
	bool __fastcall GetActive();
	void __fastcall SetURL(const System::UnicodeString Value);
	void __fastcall SetActive(const bool Value);
	
protected:
	virtual void __fastcall DoBeforeConnect();
	__property System::UnicodeString URL = {write=SetURL};
	
public:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__fastcall virtual TsgcIoTComponent_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcIoTComponent_Client();
	
protected:
	Sgcwebsocket_classes::TsgcWSConnectEvent FOnConnect;
	Sgcwebsocket_classes::TsgcWSDisconnectEvent FOnDisconnect;
	Sgcwebsocket_classes::TsgcWSErrorEvent FOnError;
	Sgcwebsocket_classes::TsgcExceptionEvent FOnException;
	__property Sgcwebsocket_classes::TsgcWSConnectEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property Sgcwebsocket_classes::TsgcWSDisconnectEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property Sgcwebsocket_classes::TsgcWSErrorEvent OnError = {read=FOnError, write=FOnError};
	__property Sgcwebsocket_classes::TsgcExceptionEvent OnException = {read=FOnException, write=FOnException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT_CLIENT)
using namespace Sgciot_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgciot_clientHPP
