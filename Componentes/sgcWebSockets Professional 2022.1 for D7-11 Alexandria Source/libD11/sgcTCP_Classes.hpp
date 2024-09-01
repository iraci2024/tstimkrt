// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcTCP_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgctcp_classesHPP
#define Sgctcp_classesHPP

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
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgctcp_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcTCPKeepAlive;
class DELPHICLASS TsgcTCPConnection_Base;
class DELPHICLASS TsgcTCPConnection;
class DELPHICLASS TsgcTCPNotifyObject;
class DELPHICLASS TsgcTCPObjectList;
class DELPHICLASS TsgcTCPComponent_Base;
class DELPHICLASS TsgcTCPLogFile;
class DELPHICLASS TsgcTCPThrottle;
class DELPHICLASS TsgcTCPOpenSSL_Options;
class DELPHICLASS TsgcTCPTLS_Options_Base;
class DELPHICLASS TsgcTCPTLS_Options;
class DELPHICLASS TsgcTCPProxy_Options;
class DELPHICLASS TsgcTCPWatchDog_Options;
class DELPHICLASS TsgcTCPComponent;
class DELPHICLASS TsgcTCPHeartBeat_Options;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcTCPConnectEvent)(TsgcTCPConnection* Connection);

typedef void __fastcall (__closure *TsgcTCPDisconnectEvent)(TsgcTCPConnection* Connection);

typedef void __fastcall (__closure *TsgcTCPMessageEvent)(TsgcTCPConnection* Connection, const System::UnicodeString Text);

typedef void __fastcall (__closure *TsgcTCPBinaryEvent)(TsgcTCPConnection* Connection, System::Classes::TMemoryStream* const Data);

typedef void __fastcall (__closure *TsgcTCPExceptionEvent)(TsgcTCPConnection* Connection, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPKeepAlive : public Sgcsocket_classes::TsgcSocketKeepAlive
{
	typedef Sgcsocket_classes::TsgcSocketKeepAlive inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPKeepAlive() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcTCPKeepAlive() : Sgcsocket_classes::TsgcSocketKeepAlive() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTCPConnection_Base : public Sgcsocket_classes::TsgcSocketConnection
{
	typedef Sgcsocket_classes::TsgcSocketConnection inherited;
	
private:
	System::UnicodeString FMsgReceived;
	System::Classes::TMemoryStream* FMsgBinaryReceived;
	System::UnicodeString FMsgException;
	
protected:
	System::Classes::TMemoryStream* __fastcall GetMsgBinaryReceived();
	
public:
	__property System::UnicodeString MsgReceived = {read=FMsgReceived, write=FMsgReceived};
	__property System::Classes::TMemoryStream* MsgBinaryReceived = {read=GetMsgBinaryReceived, write=FMsgBinaryReceived};
	__property System::UnicodeString MsgException = {read=FMsgException, write=FMsgException};
	
protected:
	bool FMustDisconnect;
	virtual void __fastcall DoDisconnectIfActive();
	
private:
	Sgcwebsocket_types::TtcpEOFScanBuffer FTCPEndOfFrameScanBuffer;
	
protected:
	System::DynamicArray<System::Byte> FTCPEndOfFrame;
	
public:
	void __fastcall AddTCPEndOfFrame(System::Byte aByte);
	void __fastcall ClearTCPEndOfFrame();
	__property Sgcwebsocket_types::TtcpEOFScanBuffer TCPEndOfFrameScanBuffer = {read=FTCPEndOfFrameScanBuffer, write=FTCPEndOfFrameScanBuffer, nodefault};
	__fastcall virtual TsgcTCPConnection_Base();
	__fastcall virtual ~TsgcTCPConnection_Base();
};


class PASCALIMPLEMENTATION TsgcTCPConnection : public TsgcTCPConnection_Base
{
	typedef TsgcTCPConnection_Base inherited;
	
public:
	/* TsgcTCPConnection_Base.Create */ inline __fastcall virtual TsgcTCPConnection() : TsgcTCPConnection_Base() { }
	/* TsgcTCPConnection_Base.Destroy */ inline __fastcall virtual ~TsgcTCPConnection() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPNotifyObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcTCPConnection* FConnection;
	int FInt;
	System::Sysutils::Exception* FRawException;
	System::Classes::TMemoryStream* FStream;
	System::UnicodeString FText;
	System::UnicodeString FText1;
	void __fastcall SetRawException(System::Sysutils::Exception* const Value);
	
protected:
	System::Classes::TMemoryStream* __fastcall GetStream();
	
public:
	__fastcall virtual ~TsgcTCPNotifyObject();
	__property TsgcTCPConnection* Connection = {read=FConnection, write=FConnection};
	__property int Int = {read=FInt, write=FInt, nodefault};
	__property System::Sysutils::Exception* RawException = {read=FRawException, write=SetRawException};
	__property System::Classes::TMemoryStream* Stream = {read=GetStream, write=FStream};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Text1 = {read=FText1, write=FText1};
public:
	/* TObject.Create */ inline __fastcall TsgcTCPNotifyObject() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPObjectList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
private:
	void __fastcall DoAdd(void * Item);
	
public:
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, System::UnicodeString aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, System::UnicodeString aParam, System::UnicodeString aParam1)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, int aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, System::Classes::TMemoryStream* aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, System::Classes::TMemoryStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, bool aBoolean)/* overload */;
	void __fastcall AddNotifyObject(TsgcTCPConnection* aConnection, System::UnicodeString aParam, System::Sysutils::Exception* aException)/* overload */;
	void __fastcall DeleteAll(bool aFreeConnection = false);
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcTCPObjectList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcTCPObjectList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTCPComponent_Base : public Sgcsocket_classes::TsgcSocketComponent_Base
{
	typedef Sgcsocket_classes::TsgcSocketComponent_Base inherited;
	
public:
	/* TsgcSocketComponent_Base.Create */ inline __fastcall virtual TsgcTCPComponent_Base(System::Classes::TComponent* aOwner) : Sgcsocket_classes::TsgcSocketComponent_Base(aOwner) { }
	/* TsgcSocketComponent_Base.Destroy */ inline __fastcall virtual ~TsgcTCPComponent_Base() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPLogFile : public Sgcsocket_classes::TsgcSocketLogFile
{
	typedef Sgcsocket_classes::TsgcSocketLogFile inherited;
	
public:
	/* TsgcSocketLogFile.Create */ inline __fastcall virtual TsgcTCPLogFile() : Sgcsocket_classes::TsgcSocketLogFile() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPLogFile() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPThrottle : public Sgcsocket_classes::TsgcSocketThrottle
{
	typedef Sgcsocket_classes::TsgcSocketThrottle inherited;
	
public:
	/* TsgcSocketThrottle.Create */ inline __fastcall virtual TsgcTCPThrottle() : Sgcsocket_classes::TsgcSocketThrottle() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPThrottle() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPOpenSSL_Options : public Sgcsocket_classes::TsgcSocketOpenSSL_Options
{
	typedef Sgcsocket_classes::TsgcSocketOpenSSL_Options inherited;
	
public:
	/* TsgcSocketOpenSSL_Options.Create */ inline __fastcall virtual TsgcTCPOpenSSL_Options() : Sgcsocket_classes::TsgcSocketOpenSSL_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPOpenSSL_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPTLS_Options_Base : public Sgcsocket_classes::TsgcSocketTLS_Options_Base
{
	typedef Sgcsocket_classes::TsgcSocketTLS_Options_Base inherited;
	
public:
	/* TsgcSocketTLS_Options_Base.Create */ inline __fastcall virtual TsgcTCPTLS_Options_Base() : Sgcsocket_classes::TsgcSocketTLS_Options_Base() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPTLS_Options_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPTLS_Options : public Sgcsocket_classes::TsgcSocketTLS_Options
{
	typedef Sgcsocket_classes::TsgcSocketTLS_Options inherited;
	
public:
	/* TsgcSocketTLS_Options.Create */ inline __fastcall virtual TsgcTCPTLS_Options() : Sgcsocket_classes::TsgcSocketTLS_Options() { }
	/* TsgcSocketTLS_Options.Destroy */ inline __fastcall virtual ~TsgcTCPTLS_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPProxy_Options : public Sgcsocket_classes::TsgcSocketProxy_Options
{
	typedef Sgcsocket_classes::TsgcSocketProxy_Options inherited;
	
public:
	/* TsgcSocketProxy_Options.Create */ inline __fastcall virtual TsgcTCPProxy_Options() : Sgcsocket_classes::TsgcSocketProxy_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPProxy_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPWatchDog_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	
protected:
	int FAttempts;
	
public:
	__fastcall virtual TsgcTCPWatchDog_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int Attempts = {read=FAttempts, write=FAttempts, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPWatchDog_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTCPComponent : public TsgcTCPComponent_Base
{
	typedef TsgcTCPComponent_Base inherited;
	
private:
	bool FQueueMessages;
	TsgcTCPObjectList* FQueueMessage;
	TsgcTCPObjectList* FQueueBinary;
	TsgcTCPConnectEvent FOnConnect;
	TsgcTCPDisconnectEvent FOnDisconnect;
	TsgcTCPMessageEvent FOnMessage;
	TsgcTCPBinaryEvent FOnBinary;
	TsgcTCPExceptionEvent FOnException;
	TsgcTCPObjectList* FNotifyConnect;
	TsgcTCPObjectList* FNotifyDisconnect;
	TsgcTCPObjectList* FNotifyMessage;
	TsgcTCPObjectList* FNotifyBinary;
	TsgcTCPObjectList* FNotifyException;
	bool FAsyncConnect;
	bool FAsyncDisconnect;
	bool FAsyncMessage;
	bool FAsyncBinary;
	bool FAsyncException;
	void __fastcall DoAsyncConnect();
	void __fastcall DoAsyncDisconnect();
	void __fastcall DoAsyncMessage();
	void __fastcall DoAsyncBinary();
	void __fastcall DoAsyncException();
	TsgcTCPObjectList* __fastcall GetNotifyConnect();
	TsgcTCPObjectList* __fastcall GetNotifyDisconnect();
	TsgcTCPObjectList* __fastcall GetNotifyMessage();
	TsgcTCPObjectList* __fastcall GetNotifyBinary();
	TsgcTCPObjectList* __fastcall GetNotifyException();
	__property TsgcTCPObjectList* NotifyConnect = {read=GetNotifyConnect};
	__property TsgcTCPObjectList* NotifyDisconnect = {read=GetNotifyDisconnect};
	__property TsgcTCPObjectList* NotifyMessage = {read=GetNotifyMessage};
	__property TsgcTCPObjectList* NotifyBinary = {read=GetNotifyBinary};
	__property TsgcTCPObjectList* NotifyException = {read=GetNotifyException};
	TsgcTCPObjectList* __fastcall GetQueueMessage();
	TsgcTCPObjectList* __fastcall GetQueueBinary();
	
protected:
	__property TsgcTCPObjectList* QueueMessage = {read=GetQueueMessage};
	__property TsgcTCPObjectList* QueueBinary = {read=GetQueueBinary};
	virtual void __fastcall DoEventConnect(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoEventDisconnect(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoEventMessage(TsgcTCPConnection* const aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(TsgcTCPConnection* const aConnection, System::Classes::TMemoryStream* const Data);
	virtual void __fastcall DoEventException(TsgcTCPConnection* const aConnection, const System::UnicodeString Error, System::Sysutils::Exception* const aException);
	virtual void __fastcall DoNotifyConnect(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoNotifyDisconnect(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoNotifyMessage(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoNotifyBinary(TsgcTCPConnection* const aConnection);
	virtual void __fastcall DoNotifyException(TsgcTCPConnection* const aConnection)/* overload */;
	virtual void __fastcall DoNotifyException(const System::UnicodeString Error, System::Sysutils::Exception* const aException)/* overload */;
	virtual void __fastcall DoException(TsgcTCPConnection* const aConnection, const System::UnicodeString aMsgException, System::Sysutils::Exception* const aException = (System::Sysutils::Exception*)(0x0));
	bool FQueueProcessing;
	virtual void __fastcall SetQueueMessages(const bool Value);
	virtual void __fastcall DoProcessQueue();
	virtual void __fastcall DoClearQueue();
	
public:
	__fastcall virtual TsgcTCPComponent(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcTCPComponent();
	void __fastcall QueueClear();
	__property bool QueueMessages = {read=FQueueMessages, write=SetQueueMessages, nodefault};
	
protected:
	__property TsgcTCPConnectEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property TsgcTCPDisconnectEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property TsgcTCPMessageEvent OnMessage = {read=FOnMessage, write=FOnMessage};
	__property TsgcTCPBinaryEvent OnBinary = {read=FOnBinary, write=FOnBinary};
	__property TsgcTCPExceptionEvent OnException = {read=FOnException, write=FOnException};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTCPHeartBeat_Options : public Sgcsocket_classes::TsgcSocketHeartBeat_Options
{
	typedef Sgcsocket_classes::TsgcSocketHeartBeat_Options inherited;
	
public:
	/* TsgcSocketHeartBeat_Options.Create */ inline __fastcall virtual TsgcTCPHeartBeat_Options() : Sgcsocket_classes::TsgcSocketHeartBeat_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTCPHeartBeat_Options() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgctcp_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCTCP_CLASSES)
using namespace Sgctcp_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgctcp_classesHPP
