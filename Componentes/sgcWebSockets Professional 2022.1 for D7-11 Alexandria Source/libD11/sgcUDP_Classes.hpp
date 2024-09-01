// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcUDP_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcudp_classesHPP
#define Sgcudp_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <IdLogFile.hpp>
#include <IdLogBase.hpp>
#include <IdIntercept.hpp>
#include <IdBaseComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcudp_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcUDPKeepAlive;
class DELPHICLASS TsgcIdUDPLogFile;
class DELPHICLASS TsgcUDPRetransmissionItem;
class DELPHICLASS TsgcUDPRetransmissionQueue;
class DELPHICLASS TsgcUDPRetransmissions;
class DELPHICLASS TsgcUDPRetransmission_Options;
class DELPHICLASS TsgcUDPSocket_Base;
class DELPHICLASS TsgcUDPSocket;
class DELPHICLASS TsgcUDPComponent_Base;
class DELPHICLASS TsgcUDPLogFile;
class DELPHICLASS TsgcUDPThrottle;
class DELPHICLASS TsgcUDPOpenSSL_Options;
class DELPHICLASS TsgcUDPTLS_Options_Base;
class DELPHICLASS TsgcUDPTLS_Options;
class DELPHICLASS TsgcUDPProxy_Options;
class DELPHICLASS TsgcUDPWatchDog_Options;
class DELPHICLASS TsgcUDPNotifyObject;
class DELPHICLASS TsgcUDPObjectList;
class DELPHICLASS TsgcUDPComponent;
class DELPHICLASS TsgcUDPHeartBeat_Options;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcUDPSocketExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* const E);

typedef System::DynamicArray<System::Byte> _dt_Sgcudp_classes_1;
typedef void __fastcall (__closure *TsgcUDPRetransmissionRetryEvent)(System::TObject* Sender, const _dt_Sgcudp_classes_1 Bytes, const System::UnicodeString aIPAddress, System::Word aPort);

typedef System::DynamicArray<System::Byte> _dt_Sgcudp_classes_2;
typedef void __fastcall (__closure *TsgcUDPRetransmissionTimeoutEvent)(System::TObject* Sender, const _dt_Sgcudp_classes_2 aBytes, const System::UnicodeString aIPAddress, System::Word aPort);

typedef void __fastcall (__closure *TsgcUDPConnectEvent)(System::TObject* Sender, TsgcUDPSocket* Socket);

typedef void __fastcall (__closure *TsgcUDPDisconnectEvent)(System::TObject* Sender, TsgcUDPSocket* Socket);

typedef void __fastcall (__closure *TsgcUDPReadEvent)(System::TObject* Sender, TsgcUDPSocket* Socket, Sgcsocket_classes::TsgcBytes Bytes);

typedef void __fastcall (__closure *TsgcUDPExceptionEvent)(System::TObject* Sender, TsgcUDPSocket* Socket, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPKeepAlive : public Sgcsocket_classes::TsgcSocketKeepAlive
{
	typedef Sgcsocket_classes::TsgcSocketKeepAlive inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPKeepAlive() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcUDPKeepAlive() : Sgcsocket_classes::TsgcSocketKeepAlive() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcIdUDPLogFile : public Sgcsocket_classes_indy::TsgcIdLogFileBase
{
	typedef Sgcsocket_classes_indy::TsgcIdLogFileBase inherited;
	
public:
	/* TsgcIdLogFileBase.Destroy */ inline __fastcall virtual ~TsgcIdUDPLogFile() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcIdUDPLogFile(System::Classes::TComponent* AOwner)/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileBase(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcIdUDPLogFile()/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileBase() { }
	
};


class PASCALIMPLEMENTATION TsgcUDPRetransmissionItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::DynamicArray<System::Byte> FBytes;
	System::UnicodeString FIPAddress;
	int FMaxRetries;
	System::TDateTime FNextRetry;
	System::Word FPort;
	int FRetries;
	int FRTO;
	void __fastcall SetBytes(const System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TsgcUDPRetransmissionItem();
	__property System::DynamicArray<System::Byte> Bytes = {read=FBytes, write=SetBytes};
	__property System::UnicodeString IPAddress = {read=FIPAddress, write=FIPAddress};
	__property int MaxRetries = {read=FMaxRetries, write=FMaxRetries, nodefault};
	__property System::TDateTime NextRetry = {read=FNextRetry, write=FNextRetry};
	__property System::Word Port = {read=FPort, write=FPort, nodefault};
	__property int Retries = {read=FRetries, write=FRetries, nodefault};
	__property int RTO = {read=FRTO, write=FRTO, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcUDPRetransmissionItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPRetransmissionQueue : public Sgcwebsocket_classes_queues::TsgcQueueBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueBase inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcUDPRetransmissionQueue() : Sgcwebsocket_classes_queues::TsgcQueueBase() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcUDPRetransmissionQueue() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPRetransmissions : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sgcbase_classes::TsgcTimer* FTimer;
	Sgcbase_classes::TsgcTimer* __fastcall GetTimer();
	
protected:
	virtual void __fastcall OnTimerEvent(System::TObject* Sender);
	virtual void __fastcall OnExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	__property Sgcbase_classes::TsgcTimer* Timer = {read=GetTimer, write=FTimer};
	
private:
	TsgcUDPRetransmissionQueue* FQueue;
	bool __fastcall GetEnabled();
	TsgcUDPRetransmissionQueue* __fastcall GetQueue();
	void __fastcall SetEnabled(const bool Value);
	
protected:
	__property TsgcUDPRetransmissionQueue* Queue = {read=GetQueue, write=FQueue};
	
public:
	__fastcall virtual TsgcUDPRetransmissions();
	__fastcall virtual ~TsgcUDPRetransmissions();
	
protected:
	void __fastcall DoRetransmissions();
	
public:
	void __fastcall AddItem(TsgcUDPRetransmissionItem* const aItem);
	bool __fastcall DeleteItem(const System::UnicodeString aID);
	
private:
	int FMaxRetries;
	int FRTO;
	
public:
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property int MaxRetries = {read=FMaxRetries, write=FMaxRetries, nodefault};
	__property int RTO = {read=FRTO, write=FRTO, nodefault};
	
private:
	TsgcUDPRetransmissionRetryEvent FOnRetry;
	TsgcUDPRetransmissionTimeoutEvent FOnTimeout;
	Sgcbase_classes::TsgcTimerOnException FOnException;
	
public:
	__property TsgcUDPRetransmissionRetryEvent OnRetry = {read=FOnRetry, write=FOnRetry};
	__property TsgcUDPRetransmissionTimeoutEvent OnTimeout = {read=FOnTimeout, write=FOnTimeout};
	__property Sgcbase_classes::TsgcTimerOnException OnException = {read=FOnException, write=FOnException};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPRetransmission_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FMaxRetries;
	int FRTO;
	
public:
	__fastcall virtual TsgcUDPRetransmission_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int MaxRetries = {read=FMaxRetries, write=FMaxRetries, nodefault};
	__property int RTO = {read=FRTO, write=FRTO, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPRetransmission_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPSocket_Base : public Sgcsocket_classes::TsgcSocketConnection
{
	typedef Sgcsocket_classes::TsgcSocketConnection inherited;
	
private:
	Sgcwebsocket_types::TtcpEOFScanBuffer FUDPEndOfFrameScanBuffer;
	
protected:
	System::DynamicArray<System::Byte> FUDPEndOfFrame;
	
public:
	void __fastcall AddUDPEndOfFrame(System::Byte aByte);
	void __fastcall ClearUDPEndOfFrame();
	__property Sgcwebsocket_types::TtcpEOFScanBuffer UDPEndOfFrameScanBuffer = {read=FUDPEndOfFrameScanBuffer, write=FUDPEndOfFrameScanBuffer, nodefault};
	
protected:
	System::UnicodeString FIP;
	System::UnicodeString FLocalIP;
	int FPort;
	int FLocalPort;
	Sgcwebsocket_types::TwsIPVersion FIPVersion;
	
public:
	__property System::UnicodeString IP = {read=FIP, write=FIP};
	__property System::UnicodeString LocalIP = {read=FLocalIP, write=FLocalIP};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property int LocalPort = {read=FLocalPort, write=FLocalPort, nodefault};
	__property Sgcwebsocket_types::TwsIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__fastcall virtual ~TsgcUDPSocket_Base();
	
protected:
	TsgcIdUDPLogFile* FLogFile;
	virtual void __fastcall DoLogSendString(const System::UnicodeString aValue);
	virtual void __fastcall DoLogRecvString(const System::UnicodeString aValue);
	virtual void __fastcall DoLogData(const System::DynamicArray<System::Byte> aBuffer);
	
public:
	void __fastcall LogData(const System::DynamicArray<System::Byte> aBuffer);
	
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
	TsgcUDPSocketExceptionEvent FOnException;
	
protected:
	void __fastcall DoException(System::Sysutils::Exception* const E);
	__property TsgcUDPSocketExceptionEvent OnException = {read=FOnException, write=FOnException};
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcUDPSocket_Base() : Sgcsocket_classes::TsgcSocketConnection() { }
	
};


class PASCALIMPLEMENTATION TsgcUDPSocket : public TsgcUDPSocket_Base
{
	typedef TsgcUDPSocket_Base inherited;
	
protected:
	System::DynamicArray<System::Byte> FMsgRead;
	
public:
	__property System::DynamicArray<System::Byte> MsgRead = {read=FMsgRead, write=FMsgRead};
	virtual void __fastcall Assign(TsgcUDPSocket* const aSource);
public:
	/* TsgcUDPSocket_Base.Destroy */ inline __fastcall virtual ~TsgcUDPSocket() { }
	
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcUDPSocket() : TsgcUDPSocket_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcUDPComponent_Base : public Sgcsocket_classes::TsgcSocketComponent_Base
{
	typedef Sgcsocket_classes::TsgcSocketComponent_Base inherited;
	
public:
	/* TsgcSocketComponent_Base.Create */ inline __fastcall virtual TsgcUDPComponent_Base(System::Classes::TComponent* aOwner) : Sgcsocket_classes::TsgcSocketComponent_Base(aOwner) { }
	/* TsgcSocketComponent_Base.Destroy */ inline __fastcall virtual ~TsgcUDPComponent_Base() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPLogFile : public Sgcsocket_classes::TsgcSocketLogFile
{
	typedef Sgcsocket_classes::TsgcSocketLogFile inherited;
	
public:
	/* TsgcSocketLogFile.Create */ inline __fastcall virtual TsgcUDPLogFile() : Sgcsocket_classes::TsgcSocketLogFile() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPLogFile() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPThrottle : public Sgcsocket_classes::TsgcSocketThrottle
{
	typedef Sgcsocket_classes::TsgcSocketThrottle inherited;
	
public:
	/* TsgcSocketThrottle.Create */ inline __fastcall virtual TsgcUDPThrottle() : Sgcsocket_classes::TsgcSocketThrottle() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPThrottle() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPOpenSSL_Options : public Sgcsocket_classes::TsgcSocketOpenSSL_Options
{
	typedef Sgcsocket_classes::TsgcSocketOpenSSL_Options inherited;
	
public:
	/* TsgcSocketOpenSSL_Options.Create */ inline __fastcall virtual TsgcUDPOpenSSL_Options() : Sgcsocket_classes::TsgcSocketOpenSSL_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPOpenSSL_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPTLS_Options_Base : public Sgcsocket_classes::TsgcSocketTLS_Options_Base
{
	typedef Sgcsocket_classes::TsgcSocketTLS_Options_Base inherited;
	
public:
	/* TsgcSocketTLS_Options_Base.Create */ inline __fastcall virtual TsgcUDPTLS_Options_Base() : Sgcsocket_classes::TsgcSocketTLS_Options_Base() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPTLS_Options_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPTLS_Options : public Sgcsocket_classes::TsgcSocketTLS_Options
{
	typedef Sgcsocket_classes::TsgcSocketTLS_Options inherited;
	
public:
	/* TsgcSocketTLS_Options.Create */ inline __fastcall virtual TsgcUDPTLS_Options() : Sgcsocket_classes::TsgcSocketTLS_Options() { }
	/* TsgcSocketTLS_Options.Destroy */ inline __fastcall virtual ~TsgcUDPTLS_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPProxy_Options : public Sgcsocket_classes::TsgcSocketProxy_Options
{
	typedef Sgcsocket_classes::TsgcSocketProxy_Options inherited;
	
public:
	/* TsgcSocketProxy_Options.Create */ inline __fastcall virtual TsgcUDPProxy_Options() : Sgcsocket_classes::TsgcSocketProxy_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPProxy_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPWatchDog_Options : public Sgcsocket_classes::TsgcSocketWatchDog_Options
{
	typedef Sgcsocket_classes::TsgcSocketWatchDog_Options inherited;
	
public:
	/* TsgcSocketWatchDog_Options.Create */ inline __fastcall virtual TsgcUDPWatchDog_Options() : Sgcsocket_classes::TsgcSocketWatchDog_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPWatchDog_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPNotifyObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DynamicArray<System::Byte> FBytes;
	TsgcUDPSocket* FSocket;
	System::Sysutils::Exception* FRawException;
	void __fastcall SetBytes(const System::DynamicArray<System::Byte> Value);
	void __fastcall SetRawException(System::Sysutils::Exception* const Value);
	void __fastcall SetSocket(TsgcUDPSocket* const Value);
	
public:
	__fastcall virtual ~TsgcUDPNotifyObject();
	__property System::DynamicArray<System::Byte> Bytes = {read=FBytes, write=SetBytes};
	__property TsgcUDPSocket* Socket = {read=FSocket, write=SetSocket};
	__property System::Sysutils::Exception* RawException = {read=FRawException, write=SetRawException};
public:
	/* TObject.Create */ inline __fastcall TsgcUDPNotifyObject() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPObjectList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
private:
	void __fastcall DoAdd(void * Item);
	
public:
	void __fastcall AddNotifyObject(TsgcUDPSocket* aSocket)/* overload */;
	void __fastcall AddNotifyObject(TsgcUDPSocket* aSocket, System::Sysutils::Exception* aException)/* overload */;
	void __fastcall AddNotifyObject(TsgcUDPSocket* aSocket, System::DynamicArray<System::Byte> aBytes)/* overload */;
	void __fastcall DeleteAll(bool aFreeSocket = false);
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcUDPObjectList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcUDPObjectList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPComponent : public TsgcUDPComponent_Base
{
	typedef TsgcUDPComponent_Base inherited;
	
private:
	TsgcUDPConnectEvent FOnUDPConnect;
	TsgcUDPDisconnectEvent FOnUDPDisconnect;
	TsgcUDPReadEvent FOnUDPRead;
	TsgcUDPExceptionEvent FOnUDPException;
	TsgcUDPObjectList* FNotifyUDPConnect;
	TsgcUDPObjectList* FNotifyUDPDisconnect;
	TsgcUDPObjectList* FNotifyUDPRead;
	TsgcUDPObjectList* FNotifyUDPException;
	bool FAsyncUDPConnect;
	bool FAsyncUDPDisconnect;
	bool FAsyncUDPRead;
	bool FAsyncUDPException;
	void __fastcall DoAsyncUDPConnect();
	void __fastcall DoAsyncUDPDisconnect();
	void __fastcall DoAsyncUDPRead();
	void __fastcall DoAsyncUDPException();
	TsgcUDPObjectList* __fastcall GetNotifyUDPConnect();
	TsgcUDPObjectList* __fastcall GetNotifyUDPDisconnect();
	TsgcUDPObjectList* __fastcall GetNotifyUDPRead();
	TsgcUDPObjectList* __fastcall GetNotifyUDPException();
	__property TsgcUDPObjectList* NotifyUDPConnect = {read=GetNotifyUDPConnect};
	__property TsgcUDPObjectList* NotifyUDPDisconnect = {read=GetNotifyUDPDisconnect};
	__property TsgcUDPObjectList* NotifyUDPRead = {read=GetNotifyUDPRead};
	__property TsgcUDPObjectList* NotifyUDPException = {read=GetNotifyUDPException};
	
protected:
	virtual void __fastcall DoNotifyUDPConnect(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoNotifyUDPDisconnect(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoNotifyUDPRead(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoNotifyUDPException(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoEventUDPConnect(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoEventUDPDisconnect(TsgcUDPSocket* const aSocket);
	virtual void __fastcall DoEventUDPRead(TsgcUDPSocket* const aSocket, const System::DynamicArray<System::Byte> Bytes);
	virtual void __fastcall DoEventUDPException(TsgcUDPSocket* const aSocket, System::Sysutils::Exception* const aException);
	virtual void __fastcall DoException(TsgcUDPSocket* const aSocket, System::Sysutils::Exception* const aException);
	__property TsgcUDPConnectEvent OnUDPConnect = {read=FOnUDPConnect, write=FOnUDPConnect};
	__property TsgcUDPDisconnectEvent OnUDPDisconnect = {read=FOnUDPDisconnect, write=FOnUDPDisconnect};
	__property TsgcUDPReadEvent OnUDPRead = {read=FOnUDPRead, write=FOnUDPRead};
	__property TsgcUDPExceptionEvent OnUDPException = {read=FOnUDPException, write=FOnUDPException};
	
public:
	__fastcall virtual ~TsgcUDPComponent();
	void __fastcall EnterCS();
	void __fastcall LeaveCS();
public:
	/* TsgcSocketComponent_Base.Create */ inline __fastcall virtual TsgcUDPComponent(System::Classes::TComponent* aOwner) : TsgcUDPComponent_Base(aOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPHeartBeat_Options : public Sgcsocket_classes::TsgcSocketHeartBeat_Options
{
	typedef Sgcsocket_classes::TsgcSocketHeartBeat_Options inherited;
	
public:
	/* TsgcSocketHeartBeat_Options.Create */ inline __fastcall virtual TsgcUDPHeartBeat_Options() : Sgcsocket_classes::TsgcSocketHeartBeat_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPHeartBeat_Options() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcudp_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCUDP_CLASSES)
using namespace Sgcudp_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcudp_classesHPP
