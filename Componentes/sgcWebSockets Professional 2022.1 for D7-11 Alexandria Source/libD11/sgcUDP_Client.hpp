// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcUDP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcudp_clientHPP
#define Sgcudp_clientHPP

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
#include <IdGlobal.hpp>
#include <IdUDPClient.hpp>
#include <IdException.hpp>
#include <IdSocks.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcUDP_Classes_Indy.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <IdThread.hpp>
#include <IdUDPBase.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcudp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcQueueCommand;
class DELPHICLASS TsgcQueueCommands;
class DELPHICLASS TsgcUDPReadThread;
class DELPHICLASS TsgcIdUDPClient;
class DELPHICLASS TsgcUDPRetransmissionClient_Options;
class DELPHICLASS TsgcUDPSocketClient;
class DELPHICLASS TsgcUDPCLient_Base;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcReadThreadExceptionEvent)(System::Sysutils::Exception* aException);

typedef void __fastcall (__closure *TsgcReadThreadMessageEvent)(System::TObject* Sender, const System::UnicodeString aPeerIP, System::Word aPeerPort);

typedef System::DynamicArray<System::Byte> _dt_Sgcudp_client_1;
typedef void __fastcall (__closure *TsgcUDPReadEvent)(System::TObject* Sender, const _dt_Sgcudp_client_1 Bytes, const System::UnicodeString aPeerIP, System::Word aPeerPort);

enum DECLSPEC_DENUM TsgcUDPCommands : unsigned char { tcmNone, tcmActive, tcmNotActive, tcmWriteBytes };

enum DECLSPEC_DENUM TsgcUDPMode : unsigned char { udpmDefault, udpmCommands };

class PASCALIMPLEMENTATION TsgcQueueCommand : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	Idglobal::TIdBytes FBytes;
	TsgcUDPCommands FCommand;
	void __fastcall SetBytes(const Idglobal::TIdBytes Value);
	
public:
	__fastcall virtual TsgcQueueCommand();
	__fastcall virtual ~TsgcQueueCommand();
	__property Idglobal::TIdBytes Bytes = {read=FBytes, write=SetBytes};
	__property TsgcUDPCommands Command = {read=FCommand, write=FCommand, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueCommands : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddCommand(const TsgcUDPCommands aCommand);
	void __fastcall AddBytes(const Idglobal::TIdBytes ABytes);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcQueueCommands() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcQueueCommands() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPReadThread : public Sgcbase_classes::TsgcIdThread
{
	typedef Sgcbase_classes::TsgcIdThread inherited;
	
private:
	bool FReadSocket;
	TsgcIdUDPClient* FClient;
	TsgcReadThreadMessageEvent FOnReadMessage;
	System::Classes::TNotifyEvent FOnReadThread;
	TsgcReadThreadExceptionEvent FOnReadException;
	
protected:
	virtual void __fastcall DoOnExceptionEvent(System::Sysutils::Exception* const E);
	
private:
	bool FExecuted;
	TsgcUDPMode FUDPMode;
	
protected:
	virtual void __fastcall Run();
	
public:
	virtual void __fastcall Terminate();
	__fastcall TsgcUDPReadThread(TsgcIdUDPClient* aClient);
	__fastcall virtual ~TsgcUDPReadThread();
	__property System::Classes::TNotifyEvent OnReadThread = {read=FOnReadThread, write=FOnReadThread};
	__property TsgcReadThreadMessageEvent OnReadMessage = {read=FOnReadMessage, write=FOnReadMessage};
	__property TsgcReadThreadExceptionEvent OnReadException = {read=FOnReadException, write=FOnReadException};
	__property bool ReadSocket = {read=FReadSocket, write=FReadSocket, nodefault};
	__property TsgcUDPMode UDPMode = {read=FUDPMode, write=FUDPMode, nodefault};
};


class PASCALIMPLEMENTATION TsgcIdUDPClient : public Idudpclient::TIdUDPClient
{
	typedef Idudpclient::TIdUDPClient inherited;
	
private:
	System::Classes::TNotifyEvent FOnThread;
	TsgcUDPReadEvent FOnRead;
	TsgcReadThreadExceptionEvent FOnReadException;
	TsgcUDPReadThread* FUDPRead;
	TsgcUDPMode FUDPMode;
	
protected:
	System::DynamicArray<System::Byte> FReadBuffer;
	int FReadBufferSize;
	virtual void __fastcall OnReadThreadEvent(System::TObject* Sender);
	virtual void __fastcall OnReadEvent(System::TObject* Sender, const System::UnicodeString aPeerIP, System::Word aPeerPort);
	virtual void __fastcall OnReadExceptionEvent(System::Sysutils::Exception* aException);
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
	TsgcUDPReadThread* __fastcall GetUDPRead();
	
public:
	__property TsgcUDPReadThread* UDPRead = {read=GetUDPRead};
	__fastcall virtual ~TsgcIdUDPClient();
	__property TsgcUDPMode UDPMode = {read=FUDPMode, write=FUDPMode, nodefault};
	
__published:
	__property System::Classes::TNotifyEvent OnThread = {read=FOnThread, write=FOnThread};
	__property TsgcUDPReadEvent OnRead = {read=FOnRead, write=FOnRead};
	__property TsgcReadThreadExceptionEvent OnReadException = {read=FOnReadException, write=FOnReadException};
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcIdUDPClient(System::Classes::TComponent* AOwner)/* overload */ : Idudpclient::TIdUDPClient(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcIdUDPClient()/* overload */ : Idudpclient::TIdUDPClient() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPRetransmissionClient_Options : public Sgcudp_classes::TsgcUDPRetransmission_Options
{
	typedef Sgcudp_classes::TsgcUDPRetransmission_Options inherited;
	
public:
	/* TsgcUDPRetransmission_Options.Create */ inline __fastcall virtual TsgcUDPRetransmissionClient_Options() : Sgcudp_classes::TsgcUDPRetransmission_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPRetransmissionClient_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPSocketClient : public Sgcudp_classes_indy::TsgcUDPSocket_Indy
{
	typedef Sgcudp_classes_indy::TsgcUDPSocket_Indy inherited;
	
public:
	virtual void __fastcall Assign(Sgcudp_classes::TsgcUDPSocket* const aSource);
	
private:
	TsgcIdUDPClient* FClient;
	
protected:
	void __fastcall SetClient(TsgcIdUDPClient* const aValue);
	virtual void __fastcall DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::UnicodeString aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::DynamicArray<System::Byte> aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::UnicodeString aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::DynamicArray<System::Byte> aValue)/* overload */;
public:
	/* TsgcUDPSocket_Indy.Destroy */ inline __fastcall virtual ~TsgcUDPSocketClient() { }
	
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcUDPSocketClient() : Sgcudp_classes_indy::TsgcUDPSocket_Indy() { }
	
};


class PASCALIMPLEMENTATION TsgcUDPCLient_Base : public Sgcudp_classes::TsgcUDPComponent
{
	typedef Sgcudp_classes::TsgcUDPComponent inherited;
	
private:
	TsgcIdUDPClient* FUDPClient;
	TsgcIdUDPClient* __fastcall GetUDPClient();
	
protected:
	virtual void __fastcall DoStopReadThread();
	__property TsgcIdUDPClient* UDPClient = {read=GetUDPClient, write=FUDPClient};
	
private:
	bool FReadingThread;
	
protected:
	virtual void __fastcall OnReadEvent(System::TObject* Sender, const System::DynamicArray<System::Byte> ABytes, const System::UnicodeString aPeerIP, System::Word aPeerPort);
	virtual void __fastcall OnReadExceptionEvent(System::Sysutils::Exception* aException);
	virtual void __fastcall OnThreadEvent(System::TObject* Sender);
	virtual void __fastcall DoReadData(const System::DynamicArray<System::Byte> ABytes);
	
private:
	Sgcbase_classes::TsgcTimer* FHeartBeatTimer;
	Sgcudp_classes::TsgcUDPHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(Sgcudp_classes::TsgcUDPHeartBeat_Options* const Value);
	
protected:
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender);
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall DoStartHeartBeat();
	virtual void __fastcall DoStopHeartBeat();
	__property Sgcudp_classes::TsgcUDPHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	TsgcUDPRetransmissionClient_Options* FRetransmissionOptions;
	void __fastcall SetRetransmissionOptions(TsgcUDPRetransmissionClient_Options* const Value);
	Sgcudp_classes::TsgcUDPRetransmissions* FRetransmissions;
	Sgcudp_classes::TsgcUDPRetransmissions* __fastcall GetRetransmissions();
	
protected:
	virtual void __fastcall OnRetransmissionRetryEvent(System::TObject* Sender, const System::DynamicArray<System::Byte> ABytes, const System::UnicodeString aIPAddress, System::Word aPort);
	virtual void __fastcall OnRetransmissionTimeoutEvent(System::TObject* Sender, const System::DynamicArray<System::Byte> ABytes, const System::UnicodeString aIPAddress, System::Word aPort);
	virtual void __fastcall OnRetransmissionExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	__property Sgcudp_classes::TsgcUDPRetransmissions* Retransmissions = {read=GetRetransmissions, write=FRetransmissions};
	__property TsgcUDPRetransmissionClient_Options* RetransmissionOptions = {read=FRetransmissionOptions, write=SetRetransmissionOptions};
	
private:
	TsgcQueueCommands* FCommands;
	TsgcQueueCommands* __fastcall GetCommands();
	
protected:
	__property TsgcQueueCommands* Commands = {read=GetCommands, write=FCommands};
	virtual void __fastcall OnCommandExecute(const TsgcUDPCommands aCommand);
	virtual void __fastcall DoCommands();
	
public:
	void __fastcall AddCommand(const TsgcUDPCommands aCommand);
	void __fastcall AddBytes(const Idglobal::TIdBytes ABytes);
	
protected:
	TsgcUDPSocketClient* FUDPSocket;
	virtual TsgcUDPSocketClient* __fastcall GetUDPSocket();
	__property TsgcUDPSocketClient* UDPSocket = {read=GetUDPSocket};
	virtual void __fastcall OnClientConnectEvent(System::TObject* Sender);
	virtual void __fastcall OnClientDisconnectEvent(System::TObject* Sender);
	virtual void __fastcall OnSocketExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* const E);
	
private:
	Sgcudp_classes::TsgcUDPProxy_Options* FProxy;
	Idsocks::TIdSocksInfo* FProxySocks;
	
protected:
	virtual void __fastcall SetProxy(Sgcudp_classes::TsgcUDPProxy_Options* const Value);
	Idsocks::TIdSocksInfo* __fastcall GetProxySocks();
	
public:
	__property Sgcudp_classes::TsgcUDPProxy_Options* Proxy = {read=FProxy, write=SetProxy};
	
private:
	Sgcudp_classes::TsgcUDPLogFile* FLogFile;
	Sgcsocket_classes_indy::TsgcIdLogFileClient* FIdLogFile;
	
protected:
	Sgcsocket_classes_indy::TsgcIdLogFileClient* __fastcall GetIdLogFile();
	virtual void __fastcall SetLogFile(Sgcudp_classes::TsgcUDPLogFile* const Value);
	
public:
	__property Sgcudp_classes::TsgcUDPLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	__fastcall virtual TsgcUDPCLient_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcUDPCLient_Base();
	
private:
	bool FTLS;
	Sgcudp_classes::TsgcUDPTLS_Options* FTLSOptions;
	
protected:
	virtual void __fastcall SetTLSOptions(Sgcudp_classes::TsgcUDPTLS_Options* const Value);
	System::UnicodeString __fastcall GetHost();
	int __fastcall GetPort();
	Idglobal::TIdIPVersion __fastcall GetIPVersion();
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall _SetPort(const int Value);
	void __fastcall SetIPVersion(const Idglobal::TIdIPVersion Value);
	
public:
	__property System::UnicodeString Host = {read=GetHost, write=SetHost};
	__property int Port = {read=GetPort, write=_SetPort, nodefault};
	__property Idglobal::TIdIPVersion IPVersion = {read=GetIPVersion, write=SetIPVersion, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	__property Sgcudp_classes::TsgcUDPTLS_Options* TLSOptions = {read=FTLSOptions, write=SetTLSOptions};
	
private:
	bool FUseNagle;
	TsgcUDPMode FUDPMode;
	
public:
	__property bool UseNagle = {read=FUseNagle, write=FUseNagle, stored=true, nodefault};
	__property TsgcUDPMode UDPMode = {read=FUDPMode, write=FUDPMode, nodefault};
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::UnicodeString aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::DynamicArray<System::Byte> aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::UnicodeString aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::DynamicArray<System::Byte> aValue)/* overload */;
	
public:
	void __fastcall WriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::UnicodeString aValue)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::DynamicArray<System::Byte> aValue)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aValue)/* overload */;
	void __fastcall WriteData(const System::DynamicArray<System::Byte> aValue)/* overload */;
	
private:
	Sgcudp_classes::TsgcUDPRetransmissionTimeoutEvent FOnRetransmissionTimeout;
	
protected:
	__property Sgcudp_classes::TsgcUDPRetransmissionTimeoutEvent OnRetranmissionTimeout = {read=FOnRetransmissionTimeout, write=FOnRetransmissionTimeout};
};


typedef System::TMetaClass* TsgcUDPSocketClientClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcudp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCUDP_CLIENT)
using namespace Sgcudp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcudp_clientHPP
