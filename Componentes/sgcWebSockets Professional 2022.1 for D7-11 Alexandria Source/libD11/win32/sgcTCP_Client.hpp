// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcTCP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgctcp_clientHPP
#define Sgctcp_clientHPP

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
#include <IdTCPConnection.hpp>
#include <IdGlobal.hpp>
#include <IdTCPClient.hpp>
#include <IdThread.hpp>
#include <IdException.hpp>
#include <IdIntercept.hpp>
#include <IdIOHandler.hpp>
#include <IdIOHandlerSocket.hpp>
#include <IdIOHandlerStack.hpp>
#include <IdConnectThroughHttpProxy.hpp>
#include <IdSocks.hpp>
#include <IdSSL.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdSSLOpenSSLHeaders.hpp>
#include <IdComponent.hpp>
#include <IdInterceptThrottler.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcTCP_Classes_Indy.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <IdBaseComponent.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgctcp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcQueueCommand;
class DELPHICLASS TsgcQueueCommands;
class DELPHICLASS TsgcTCPReadThread;
class DELPHICLASS TsgcIdTCPClient;
class DELPHICLASS TsgcIdSSLIOHandlerSocketOpenSSL;
class DELPHICLASS TsgcTCPTLSClient_Options;
class DELPHICLASS TsgcTCPConnectionClient;
class DELPHICLASS TsgcTCPCLient_Common;
class DELPHICLASS TsgcTCPCLient_Base;
class DELPHICLASS TsgcTCPCLient;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcTCPOnSSLGetHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);

typedef void __fastcall (__closure *TsgcTCPOnSSLAfterCreateHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* aSSLHandler);

typedef void __fastcall (__closure *TsgcReadThreadExceptionEvent)(System::Sysutils::Exception* aException);

enum DECLSPEC_DENUM TsgcTCPCommands : unsigned char { tcmNone, tcmActive, tcmNotActive, tcmWriteBytes };

enum DECLSPEC_DENUM TsgcTCPMode : unsigned char { tcpmDefault, tcpmCommands };

class PASCALIMPLEMENTATION TsgcQueueCommand : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	Idglobal::TIdBytes FBytes;
	TsgcTCPCommands FCommand;
	void __fastcall SetBytes(const Idglobal::TIdBytes Value);
	
public:
	__fastcall virtual TsgcQueueCommand();
	__fastcall virtual ~TsgcQueueCommand();
	__property Idglobal::TIdBytes Bytes = {read=FBytes, write=SetBytes};
	__property TsgcTCPCommands Command = {read=FCommand, write=FCommand, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueCommands : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddCommand(const TsgcTCPCommands aCommand);
	void __fastcall AddBytes(const Idglobal::TIdBytes ABytes);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcQueueCommands() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcQueueCommands() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTCPReadThread : public Sgcbase_classes::TsgcIdThread
{
	typedef Sgcbase_classes::TsgcIdThread inherited;
	
private:
	bool FReadSocket;
	TsgcIdTCPClient* FClient;
	System::Classes::TNotifyEvent FOnReadMessage;
	System::Classes::TNotifyEvent FOnReadThread;
	TsgcReadThreadExceptionEvent FOnReadException;
	
protected:
	virtual void __fastcall DoOnMessageEvent();
	virtual void __fastcall DoOnExceptionEvent(System::Sysutils::Exception* const E);
	
private:
	bool FExecuted;
	TsgcTCPMode FTCPMode;
	
protected:
	virtual void __fastcall Run();
	
public:
	virtual void __fastcall Terminate();
	__fastcall TsgcTCPReadThread(TsgcIdTCPClient* aClient);
	__fastcall virtual ~TsgcTCPReadThread();
	__property System::Classes::TNotifyEvent OnReadThread = {read=FOnReadThread, write=FOnReadThread};
	__property System::Classes::TNotifyEvent OnReadMessage = {read=FOnReadMessage, write=FOnReadMessage};
	__property TsgcReadThreadExceptionEvent OnReadException = {read=FOnReadException, write=FOnReadException};
	__property bool ReadSocket = {read=FReadSocket, write=FReadSocket, nodefault};
	__property TsgcTCPMode TCPMode = {read=FTCPMode, write=FTCPMode, nodefault};
};


class PASCALIMPLEMENTATION TsgcIdTCPClient : public Idtcpclient::TIdTCPClient
{
	typedef Idtcpclient::TIdTCPClient inherited;
	
private:
	System::Classes::TNotifyEvent FOnThread;
	System::Classes::TNotifyEvent FOnRead;
	TsgcReadThreadExceptionEvent FOnReadException;
	TsgcTCPReadThread* FTCPRead;
	TsgcTCPMode FTCPMode;
	
protected:
	virtual void __fastcall OnReadThreadEvent(System::TObject* Sender);
	virtual void __fastcall OnReadEvent(System::TObject* Sender);
	virtual void __fastcall OnReadExceptionEvent(System::Sysutils::Exception* aException);
	
public:
	virtual bool __fastcall Connected();
	
protected:
	virtual void __fastcall InitComponent();
	
private:
	Sgcwebsocket_classes_syncobjs::TsgcCriticalSection* FCS;
	
protected:
	virtual void __fastcall DoEnterCS();
	virtual void __fastcall DoLeaveCS();
	
public:
	void __fastcall EnterCS();
	void __fastcall LeaveCS();
	
private:
	TsgcTCPReadThread* __fastcall GetTCPRead();
	
public:
	__property TsgcTCPReadThread* TCPRead = {read=GetTCPRead};
	__fastcall virtual ~TsgcIdTCPClient();
	virtual void __fastcall Connect()/* overload */;
	virtual void __fastcall Disconnect(bool ANotifyPeer)/* overload */;
	__property TsgcTCPMode TCPMode = {read=FTCPMode, write=FTCPMode, nodefault};
	
__published:
	__property System::Classes::TNotifyEvent OnThread = {read=FOnThread, write=FOnThread};
	__property System::Classes::TNotifyEvent OnRead = {read=FOnRead, write=FOnRead};
	__property TsgcReadThreadExceptionEvent OnReadException = {read=FOnReadException, write=FOnReadException};
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcIdTCPClient(System::Classes::TComponent* AOwner)/* overload */ : Idtcpclient::TIdTCPClient(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcIdTCPClient()/* overload */ : Idtcpclient::TIdTCPClient() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Connect(const System::UnicodeString AHost){ Idtcpclient::TIdTCPClientCustom::Connect(AHost); }
	inline void __fastcall  Connect(const System::UnicodeString AHost, const System::Word APort){ Idtcpclient::TIdTCPClientCustom::Connect(AHost, APort); }
	inline void __fastcall  Disconnect(){ Idtcpconnection::TIdTCPConnection::Disconnect(); }
	
};


class PASCALIMPLEMENTATION TsgcIdSSLIOHandlerSocketOpenSSL : public Idsslopenssl::TIdSSLIOHandlerSocketOpenSSL
{
	typedef Idsslopenssl::TIdSSLIOHandlerSocketOpenSSL inherited;
	
private:
	Sgcwebsocket_classes_syncobjs::TsgcCriticalSection* FCS;
	
protected:
	virtual void __fastcall DoEnterCS();
	virtual void __fastcall DoLeaveCS();
	virtual void __fastcall InitComponent();
	virtual int __fastcall RecvEnc(Idglobal::TIdBytes &VBuffer);
	virtual int __fastcall SendEnc(const Idglobal::TIdBytes ABuffer, const int AOffset, const int ALength);
	virtual int __fastcall CheckForError(int ALastResult);
	
public:
	__fastcall virtual ~TsgcIdSSLIOHandlerSocketOpenSSL();
	virtual bool __fastcall Readable(int AMSec = 0xffffffff);
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcIdSSLIOHandlerSocketOpenSSL(System::Classes::TComponent* AOwner)/* overload */ : Idsslopenssl::TIdSSLIOHandlerSocketOpenSSL(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcIdSSLIOHandlerSocketOpenSSL()/* overload */ : Idsslopenssl::TIdSSLIOHandlerSocketOpenSSL() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPTLSClient_Options : public Sgctcp_classes::TsgcTCPTLS_Options
{
	typedef Sgctcp_classes::TsgcTCPTLS_Options inherited;
	
public:
	/* TsgcSocketTLS_Options.Create */ inline __fastcall virtual TsgcTCPTLSClient_Options() : Sgctcp_classes::TsgcTCPTLS_Options() { }
	/* TsgcSocketTLS_Options.Destroy */ inline __fastcall virtual ~TsgcTCPTLSClient_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTCPConnectionClient : public Sgctcp_classes_indy::TsgcTCPConnection_Indy
{
	typedef Sgctcp_classes_indy::TsgcTCPConnection_Indy inherited;
	
protected:
	virtual void __fastcall DoReadData_TCP();
	virtual void __fastcall ReadData();
	
private:
	System::UnicodeString __fastcall GetALPNProtocol();
	
public:
	__property System::UnicodeString ALPNProtocol = {read=GetALPNProtocol};
	
protected:
	virtual void __fastcall DoInitialize(Idtcpconnection::TIdTCPConnection* aConnection);
	
private:
	Sgctcp_classes::TsgcTCPBinaryEvent FOnBinary;
	
protected:
	virtual void __fastcall DoBinaryEvent(System::Classes::TMemoryStream* const aData);
	
public:
	__property Sgctcp_classes::TsgcTCPBinaryEvent OnBinary = {read=FOnBinary, write=FOnBinary};
public:
	/* TsgcTCPConnection_Indy.Destroy */ inline __fastcall virtual ~TsgcTCPConnectionClient() { }
	
public:
	/* TsgcTCPConnection_Base.Create */ inline __fastcall virtual TsgcTCPConnectionClient() : Sgctcp_classes_indy::TsgcTCPConnection_Indy() { }
	
};


class PASCALIMPLEMENTATION TsgcTCPCLient_Common : public Sgctcp_classes::TsgcTCPComponent
{
	typedef Sgctcp_classes::TsgcTCPComponent inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoNotifyDisconnect(Sgctcp_classes::TsgcTCPConnection* const aConnection);
	
private:
	TsgcIdTCPClient* FTCPClient;
	TsgcIdTCPClient* __fastcall GetTCPClient();
	
protected:
	virtual void __fastcall DoStopReadThread();
	__property TsgcIdTCPClient* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
private:
	Sgctcp_classes::TsgcTCPKeepAlive* FTCPKeepAlive;
	void __fastcall SetTCPKeepAlive(Sgctcp_classes::TsgcTCPKeepAlive* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPKeepAlive* TCPKeepAlive = {read=FTCPKeepAlive, write=SetTCPKeepAlive};
	
private:
	bool FReadingThread;
	
protected:
	virtual void __fastcall OnReadEvent(System::TObject* Sender);
	virtual void __fastcall OnReadExceptionEvent(System::Sysutils::Exception* aException);
	virtual void __fastcall OnThreadEvent(System::TObject* Sender);
	
private:
	Sgcbase_classes::TsgcTimer* FHeartBeatTimer;
	Sgctcp_classes::TsgcTCPHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(Sgctcp_classes::TsgcTCPHeartBeat_Options* const Value);
	
protected:
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender) = 0 ;
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E) = 0 ;
	virtual void __fastcall DoStartHeartBeat();
	virtual void __fastcall DoStopHeartBeat();
	__property Sgctcp_classes::TsgcTCPHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	TsgcQueueCommands* FCommands;
	TsgcQueueCommands* __fastcall GetCommands();
	
protected:
	__property TsgcQueueCommands* Commands = {read=GetCommands, write=FCommands};
	virtual void __fastcall OnCommandExecute(const TsgcTCPCommands aCommand) = 0 ;
	virtual void __fastcall DoCommands();
	
public:
	void __fastcall AddCommand(const TsgcTCPCommands aCommand);
	void __fastcall AddBytes(const Idglobal::TIdBytes ABytes);
	
protected:
	TsgcTCPConnectionClient* FTCPConnection;
	bool FDisconnecting;
	Sgctcp_classes::TsgcTCPConnection* __fastcall GetConnection();
	
public:
	__property Sgctcp_classes::TsgcTCPConnection* Connection = {read=GetConnection};
	
protected:
	virtual void __fastcall DoCreateTCPConnection();
	virtual void __fastcall OnConnectionBeforeDisconnect(System::TObject* Sender);
	virtual void __fastcall OnClientConnectEvent(System::TObject* Sender);
	virtual void __fastcall OnClientDisconnectEvent(System::TObject* Sender);
	virtual void __fastcall OnClientStatusEvent(System::TObject* ASender, const Idcomponent::TIdStatus AStatus, const System::UnicodeString AStatusText);
	virtual void __fastcall OnClientBinaryEvent(Sgctcp_classes::TsgcTCPConnection* aConnection, System::Classes::TMemoryStream* const aStream);
	
private:
	Sgctcp_classes::TsgcTCPLogFile* FLogFile;
	
protected:
	virtual Sgctcp_classes::TsgcTCPLogFile* __fastcall GetLogFile();
	virtual void __fastcall SetLogFile(Sgctcp_classes::TsgcTCPLogFile* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPLogFile* LogFile = {read=GetLogFile, write=SetLogFile};
	
private:
	Sgctcp_classes::TsgcTCPThrottle* FThrottle;
	
protected:
	virtual Sgctcp_classes::TsgcTCPThrottle* __fastcall GetThrottle();
	virtual void __fastcall SetThrottle(Sgctcp_classes::TsgcTCPThrottle* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPThrottle* Throttle = {read=GetThrottle, write=SetThrottle};
	
private:
	Idinterceptthrottler::TIdInterceptThrottler* FInterceptThrottle;
	Idinterceptthrottler::TIdInterceptThrottler* __fastcall GetInterceptThrottle();
	
protected:
	__property Idinterceptthrottler::TIdInterceptThrottler* InterceptThrottle = {read=GetInterceptThrottle, write=FInterceptThrottle};
	Idintercept::TIdConnectionIntercept* __fastcall GetConnectionIntercept();
	virtual void __fastcall DoStopThreads();
	
private:
	Sgcsocket_classes_indy::TsgcIdLogFileClient* FInterceptLogFile;
	Sgcsocket_classes_indy::TsgcIdLogFileClient* __fastcall GetInterceptLogFile();
	
protected:
	__property Sgcsocket_classes_indy::TsgcIdLogFileClient* InterceptLogFile = {read=GetInterceptLogFile, write=FInterceptLogFile};
	
private:
	Sgctcp_classes::TsgcTCPProxy_Options* FProxy;
	Idiohandlerstack::TIdIOHandlerStack* FProxyIOHandler;
	Idconnectthroughhttpproxy::TIdConnectThroughHttpProxy* FProxyHTTP;
	Idsocks::TIdSocksInfo* FProxySocks;
	Idiohandlerstack::TIdIOHandlerStack* __fastcall GetProxyIOHandler();
	Idconnectthroughhttpproxy::TIdConnectThroughHttpProxy* __fastcall GetProxyHTTP();
	Idsocks::TIdSocksInfo* __fastcall GetProxySocks();
	
protected:
	virtual void __fastcall SetProxy(Sgctcp_classes::TsgcTCPProxy_Options* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPProxy_Options* Proxy = {read=FProxy, write=SetProxy};
	
private:
	Idssl::TIdSSLIOHandlerSocketBase* FHandlerSSL;
	
protected:
	TsgcTCPOnSSLGetHandler FOnSSLGetHandler;
	TsgcTCPOnSSLAfterCreateHandler FOnSSLAfterCreateHandler;
	__property Idssl::TIdSSLIOHandlerSocketBase* HandlerSSL = {read=FHandlerSSL};
	virtual void __fastcall GetHandlerSSLEvent(const Sgcwebsocket_types::TwsSSLHandler aSSLType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);
	virtual void __fastcall DoSSL();
	virtual void __fastcall OnGetPasswordEvent(System::UnicodeString &Password);
	
public:
	__property TsgcTCPOnSSLGetHandler OnSSLGetHandler = {read=FOnSSLGetHandler, write=FOnSSLGetHandler};
	__property TsgcTCPOnSSLAfterCreateHandler OnSSLAfterCreateHandler = {read=FOnSSLAfterCreateHandler, write=FOnSSLAfterCreateHandler};
	
private:
	Sgctcp_classes::TsgcTCPWatchDog_Options* FWatchDog;
	
protected:
	int FWatchDogAttempts;
	Sgcbase_classes::TsgcTimer* FWatchDogTimer;
	bool FWatchDogEnabled;
	virtual void __fastcall DoStartWatchDog();
	virtual void __fastcall DoStopWatchDog();
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E) = 0 ;
	virtual void __fastcall SetWatchDog(Sgctcp_classes::TsgcTCPWatchDog_Options* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPWatchDog_Options* WatchDog = {read=FWatchDog, write=SetWatchDog};
	
private:
	void __fastcall DoClear();
	
public:
	__fastcall virtual TsgcTCPCLient_Common(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcTCPCLient_Common();
	
private:
	System::UnicodeString FHost;
	int FPort;
	bool FTLS;
	Sgctcp_classes::TsgcTCPTLS_Options* FTLSOptions;
	
protected:
	virtual void __fastcall SetTLSOptions(Sgctcp_classes::TsgcTCPTLS_Options* const Value);
	
public:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	__property Sgctcp_classes::TsgcTCPTLS_Options* TLSOptions = {read=FTLSOptions, write=SetTLSOptions};
	
private:
	bool FUseNagle;
	Idglobal::TIdIPVersion FIPVersion;
	int FConnectTimeout;
	int FReadTimeout;
	TsgcTCPMode FTCPMode;
	int FWriteTimeout;
	System::Word FBoundPortMax;
	System::Word FBoundPortMin;
	int FLingerState;
	
public:
	__property bool UseNagle = {read=FUseNagle, write=FUseNagle, stored=true, nodefault};
	__property Idglobal::TIdIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__property int ConnectTimeout = {read=FConnectTimeout, write=FConnectTimeout, nodefault};
	__property int WriteTimeout = {read=FWriteTimeout, write=FWriteTimeout, nodefault};
	__property int ReadTimeout = {read=FReadTimeout, write=FReadTimeout, nodefault};
	__property TsgcTCPMode TCPMode = {read=FTCPMode, write=FTCPMode, nodefault};
	__property System::Word BoundPortMax = {read=FBoundPortMax, write=FBoundPortMax, nodefault};
	__property System::Word BoundPortMin = {read=FBoundPortMin, write=FBoundPortMin, nodefault};
	__property int LingerState = {read=FLingerState, write=FLingerState, nodefault};
	
private:
	bool FActive;
	
protected:
	virtual bool __fastcall GetActive();
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	
private:
	bool FConnecting;
	
public:
	bool __fastcall Connect(const int aTimeout = 0x2710);
	bool __fastcall Disconnect(const int aTimeout = 0x2710);
};


class PASCALIMPLEMENTATION TsgcTCPCLient_Base : public TsgcTCPCLient_Common
{
	typedef TsgcTCPCLient_Common inherited;
	
protected:
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender);
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnCommandExecute(const TsgcTCPCommands aCommand);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall DoWriteData(const System::DynamicArray<System::Byte> aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::UnicodeString aValue)/* overload */;
	
public:
	void __fastcall WriteData(const System::DynamicArray<System::Byte> aValue)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aValue)/* overload */;
public:
	/* TsgcTCPCLient_Common.Create */ inline __fastcall virtual TsgcTCPCLient_Base(System::Classes::TComponent* aOwner) : TsgcTCPCLient_Common(aOwner) { }
	/* TsgcTCPCLient_Common.Destroy */ inline __fastcall virtual ~TsgcTCPCLient_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcTCPCLient : public TsgcTCPCLient_Base
{
	typedef TsgcTCPCLient_Base inherited;
	
public:
	__property NotifyEvents;
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnException;
public:
	/* TsgcTCPCLient_Common.Create */ inline __fastcall virtual TsgcTCPCLient(System::Classes::TComponent* aOwner) : TsgcTCPCLient_Base(aOwner) { }
	/* TsgcTCPCLient_Common.Destroy */ inline __fastcall virtual ~TsgcTCPCLient() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgctcp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCTCP_CLIENT)
using namespace Sgctcp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgctcp_clientHPP
