// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_classesHPP
#define Sgcwebsocket_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <System.Types.hpp>
#include <IdThread.hpp>
#include <IdGlobal.hpp>
#include <IdContext.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Const.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcWebSocket_Extensions.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSException;
class DELPHICLASS TsgcQueueItemConnection;
class DELPHICLASS TsgcCacheQueueListConnections;
class DELPHICLASS TsgcQueueItemChannel;
class DELPHICLASS TsgcQueueListChannels;
class DELPHICLASS TsgcQueueList;
class DELPHICLASS TsgcQueueObject;
class DELPHICLASS TsgcWSNotifyObject;
class DELPHICLASS TsgcWSObjectList;
class DELPHICLASS TsgcOpenSSLServer_Options;
class DELPHICLASS TsgcWSSSL_Options;
class DELPHICLASS TsgcHTTPRequest;
class DELPHICLASS TsgcHTTPResponse;
class DELPHICLASS TsgcWSHeartBeat_Options;
class DELPHICLASS TsgcWSConnectionsFree_Options;
class DELPHICLASS TsgcWSConnections_Options;
class DELPHICLASS TsgcWSChannels_Options;
class DELPHICLASS TsgcWSOptimization_Options;
class DELPHICLASS TsgcWSTLS_Options;
class DELPHICLASS TsgcWSWatchDog_Options;
class DELPHICLASS TsgcWSWatchDogClient_Options;
class DELPHICLASS TsgcWSWatchDogMonitorServer_Options;
class DELPHICLASS TsgcWSWatchDogServer_Options;
class DELPHICLASS TsgcWSDrafts;
class DELPHICLASS TsgcWSSpecifications;
class DELPHICLASS TsgcWSQueue;
class DELPHICLASS TsgcWSQueueOptions;
class DELPHICLASS TsgcWSHandshake_Base;
class DELPHICLASS TsgcWSConnection;
class DELPHICLASS TsgcWSConnection_Abstract;
class DELPHICLASS TsgcWSComponent_Base;
class DELPHICLASS TsgcWSLogFile;
class DELPHICLASS TsgcWSComponent;
class DELPHICLASS TsgcWSComponent_Server;
class DELPHICLASS TsgcWSComponent_Client;
class DELPHICLASS TsgcWSComponent_WSClient;
class DELPHICLASS TsgcWSComponent_WSClient_API;
class DELPHICLASS TsgcWSAPI;
class DELPHICLASS TsgcWSAPI_client;
class DELPHICLASS TsgcWSAPI_client_KeepAlive;
class DELPHICLASS TsgcWSAPI_server;
class DELPHICLASS TsgcQueueItemProtocolMessage;
class DELPHICLASS TsgcWSProtocol;
class DELPHICLASS TsgcWSProtocol_Client;
class DELPHICLASS TsgcWSProtocol_Server;
class DELPHICLASS TsgcWSAuthentication_Session;
class DELPHICLASS TsgcWSAuthentication_URL;
class DELPHICLASS TsgcWSAuthentication_Basic;
class DELPHICLASS TsgcWSAuthentication_Token;
class DELPHICLASS TsgcWSAuthentication_Options;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TsgcWS_ProtocolClass;

typedef void __fastcall (__closure *TsgcWSMessageEvent)(TsgcWSConnection* Connection, const System::UnicodeString Text);

typedef void __fastcall (__closure *TsgcWSBinaryEvent)(TsgcWSConnection* Connection, System::Classes::TMemoryStream* const Data);

typedef void __fastcall (__closure *TsgcWSFragmentedEvent)(TsgcWSConnection* Connection, System::Classes::TMemoryStream* const Data, const Sgcwebsocket_types::TOpcode OpCode, const bool Continuation);

typedef void __fastcall (__closure *TsgcWSConnectEvent)(TsgcWSConnection* Connection);

typedef void __fastcall (__closure *TsgcWSDisconnectEvent)(TsgcWSConnection* Connection, int Code);

typedef void __fastcall (__closure *TsgcWSErrorEvent)(TsgcWSConnection* Connection, const System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSSubscriptionEvent)(TsgcWSConnection* Connection, const System::UnicodeString Subscription);

typedef void __fastcall (__closure *TsgcWSBeforeSubscriptionEvent)(TsgcWSConnection* Connection, const System::UnicodeString Subscription, bool &Accept);

typedef void __fastcall (__closure *TsgcWSHandshakeEvent)(TsgcWSConnection* Connection, System::Classes::TStringList* &Headers);

typedef void __fastcall (__closure *TsgcWSUpdateEvent)(TsgcWSConnection* Connection, int aType);

typedef void __fastcall (__closure *TsgcWSUnknownProtocolEvent)(TsgcWSConnection* Connection, bool &Accept);

typedef void __fastcall (__closure *TsgcWSOnBeforeHeartBeatEvent)(System::TObject* Sender, TsgcWSConnection* const Connection, bool &Handled);

typedef void __fastcall (__closure *TsgcWSOnBeforeWatchDogEvent)(System::TObject* Sender, bool &Handled);

typedef void __fastcall (__closure *TsgcWSOnBeforeConnectEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcExceptionEvent)(TsgcWSConnection* Connection, System::Sysutils::Exception* E);

typedef void __fastcall (__closure *TsgcWSRawMessageEvent)(TsgcWSConnection* Connection, const System::UnicodeString Text, bool &Handled);

typedef void __fastcall (__closure *TsgcWSLoadBalancerErrorEvent)(System::TObject* Sender, const System::UnicodeString Error);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall TsgcWSException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TsgcWSException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TsgcWSException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TsgcWSException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcWSException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcWSException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TsgcWSException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TsgcWSException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcWSException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcWSException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcWSException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcWSException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TsgcWSException() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcQueueItemConnection : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcWSConnection* FConnection;
	void __fastcall SetConnection(TsgcWSConnection* const Value);
	
public:
	__fastcall virtual TsgcQueueItemConnection();
	__fastcall virtual ~TsgcQueueItemConnection();
	__property TsgcWSConnection* Connection = {read=FConnection, write=SetConnection};
};


class PASCALIMPLEMENTATION TsgcCacheQueueListConnections : public Sgcwebsocket_classes_queues::TsgcCacheQueueList
{
	typedef Sgcwebsocket_classes_queues::TsgcCacheQueueList inherited;
	
public:
	TsgcWSConnection* __fastcall GetConnection(const System::UnicodeString aGuid);
public:
	/* TsgcCacheQueueList.Create */ inline __fastcall virtual TsgcCacheQueueListConnections(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes_queues::TsgcCacheQueueList(aOwner) { }
	/* TsgcCacheQueueList.Destroy */ inline __fastcall virtual ~TsgcCacheQueueListConnections() { }
	
};


class PASCALIMPLEMENTATION TsgcQueueItemChannel : public Sgcwebsocket_classes_queues::TsgcQueueNameItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueNameItemBase inherited;
	
private:
	TsgcWSConnection* FConnection;
	void __fastcall SetConnection(TsgcWSConnection* const Value);
	
public:
	__fastcall virtual TsgcQueueItemChannel();
	__fastcall virtual ~TsgcQueueItemChannel();
	__property TsgcWSConnection* Connection = {read=FConnection, write=SetConnection};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueListChannels : public Sgcwebsocket_classes_queues::TsgcQueueNameListBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueNameListBase inherited;
	
public:
	void __fastcall GetConnections(const System::UnicodeString aChannel, System::Classes::TList* &aList);
	void __fastcall GetChannels(const System::UnicodeString aID, System::Classes::TList* &aList);
public:
	/* TsgcQueueNameListBase.Create */ inline __fastcall virtual TsgcQueueListChannels() : Sgcwebsocket_classes_queues::TsgcQueueNameListBase() { }
	/* TsgcQueueNameListBase.Destroy */ inline __fastcall virtual ~TsgcQueueListChannels() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	__fastcall virtual ~TsgcQueueList();
	void __fastcall FreeObjects();
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcQueueList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FSize;
	Sgcwebsocket_types::TwsStreaming FStreaming;
	System::UnicodeString FText;
	Sgcwebsocket_types::TOpcode FOpCode;
	System::Classes::TMemoryStream* FStream;
	System::Classes::TStream* __fastcall GetStream();
	void __fastcall SetStream(System::Classes::TStream* const Value);
	
public:
	__fastcall virtual TsgcQueueObject();
	__fastcall virtual ~TsgcQueueObject();
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property Sgcwebsocket_types::TwsStreaming Streaming = {read=FStreaming, write=FStreaming, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property Sgcwebsocket_types::TOpcode OpCode = {read=FOpCode, write=FOpCode, nodefault};
	__property System::Classes::TStream* Stream = {read=GetStream, write=SetStream};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSNotifyObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcWSConnection* FConnection;
	int FInt;
	System::Sysutils::Exception* FRawException;
	System::Classes::TMemoryStream* FStream;
	System::UnicodeString FText;
	System::UnicodeString FText1;
	void __fastcall SetRawException(System::Sysutils::Exception* const Value);
	
protected:
	System::Classes::TMemoryStream* __fastcall GetStream();
	
public:
	__fastcall virtual ~TsgcWSNotifyObject();
	__property TsgcWSConnection* Connection = {read=FConnection, write=FConnection};
	__property int Int = {read=FInt, write=FInt, nodefault};
	__property System::Sysutils::Exception* RawException = {read=FRawException, write=SetRawException};
	__property System::Classes::TMemoryStream* Stream = {read=GetStream, write=FStream};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Text1 = {read=FText1, write=FText1};
public:
	/* TObject.Create */ inline __fastcall TsgcWSNotifyObject() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSObjectList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
private:
	void __fastcall DoAdd(void * Item);
	
public:
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, System::UnicodeString aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, System::UnicodeString aParam, System::UnicodeString aParam1)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, int aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, System::Classes::TMemoryStream* aParam)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, System::Classes::TMemoryStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, bool aBoolean)/* overload */;
	void __fastcall AddNotifyObject(TsgcWSConnection* aConnection, System::UnicodeString aParam, System::Sysutils::Exception* aException)/* overload */;
	void __fastcall DeleteAll(bool aFreeConnection = false);
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcWSObjectList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcWSObjectList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcOpenSSLServer_Options : public Sgctcp_classes::TsgcTCPOpenSSL_Options
{
	typedef Sgctcp_classes::TsgcTCPOpenSSL_Options inherited;
	
private:
	bool FECDHE;
	
public:
	__fastcall virtual TsgcOpenSSLServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool ECDHE = {read=FECDHE, write=FECDHE, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcOpenSSLServer_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSSL_Options : public Sgctcp_classes::TsgcTCPTLS_Options_Base
{
	typedef Sgctcp_classes::TsgcTCPTLS_Options_Base inherited;
	
private:
	TsgcOpenSSLServer_Options* FOpenSSL_Options;
	int FPort;
	void __fastcall SetOpenSSL_Options(TsgcOpenSSLServer_Options* const Value);
	
public:
	__fastcall virtual TsgcWSSSL_Options();
	__fastcall virtual ~TsgcWSSSL_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcOpenSSLServer_Options* OpenSSL_Options = {read=FOpenSSL_Options, write=SetOpenSSL_Options};
	__property int Port = {read=FPort, write=FPort, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FContent;
	System::UnicodeString FDocument;
	System::UnicodeString FMethod;
	System::UnicodeString FQueryParams;
	
public:
	__property System::UnicodeString Content = {read=FContent, write=FContent};
	__property System::UnicodeString Document = {read=FDocument, write=FDocument};
	__property System::UnicodeString Method = {read=FMethod, write=FMethod};
	__property System::UnicodeString QueryParams = {read=FQueryParams, write=FQueryParams};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPRequest() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPRequest() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPResponse : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FCode;
	System::UnicodeString FContent;
	System::UnicodeString FContentType;
	
public:
	__property int Code = {read=FCode, write=FCode, nodefault};
	__property System::UnicodeString Content = {read=FContent, write=FContent};
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPResponse() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPResponse() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHeartBeat_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTimeout;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSHeartBeat_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSHeartBeat_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSConnectionsFree_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	bool __fastcall GetEnabled();
	
public:
	__fastcall virtual TsgcWSConnectionsFree_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=GetEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSConnectionsFree_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSConnections_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FCacheSize;
	int FGroupLevel;
	bool __fastcall GetEnabled();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=GetEnabled, write=FEnabled, nodefault};
	__property int CacheSize = {read=FCacheSize, write=FCacheSize, nodefault};
	__property int GroupLevel = {read=FGroupLevel, write=FGroupLevel, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSConnections_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSConnections_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSChannels_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	bool __fastcall GetEnabled();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=GetEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSChannels_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSChannels_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSOptimization_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSConnections_Options* FConnections;
	TsgcWSChannels_Options* FChannels;
	TsgcWSConnectionsFree_Options* FConnectionsFree;
	void __fastcall SetConnections(TsgcWSConnections_Options* const Value);
	void __fastcall SetChannels(TsgcWSChannels_Options* const Value);
	void __fastcall SetConnectionsFree(TsgcWSConnectionsFree_Options* const Value);
	
public:
	__fastcall virtual TsgcWSOptimization_Options();
	__fastcall virtual ~TsgcWSOptimization_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSConnections_Options* Connections = {read=FConnections, write=SetConnections};
	__property TsgcWSChannels_Options* Channels = {read=FChannels, write=SetChannels};
	__property TsgcWSConnectionsFree_Options* ConnectionsFree = {read=FConnectionsFree, write=SetConnectionsFree};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSTLS_Options : public Sgctcp_classes::TsgcTCPTLS_Options
{
	typedef Sgctcp_classes::TsgcTCPTLS_Options inherited;
	
public:
	/* TsgcSocketTLS_Options.Create */ inline __fastcall virtual TsgcWSTLS_Options() : Sgctcp_classes::TsgcTCPTLS_Options() { }
	/* TsgcSocketTLS_Options.Destroy */ inline __fastcall virtual ~TsgcWSTLS_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSWatchDog_Options : public Sgctcp_classes::TsgcTCPWatchDog_Options
{
	typedef Sgctcp_classes::TsgcTCPWatchDog_Options inherited;
	
public:
	/* TsgcTCPWatchDog_Options.Create */ inline __fastcall virtual TsgcWSWatchDog_Options() : Sgctcp_classes::TsgcTCPWatchDog_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSWatchDog_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSWatchDogClient_Options : public TsgcWSWatchDog_Options
{
	typedef TsgcWSWatchDog_Options inherited;
	
public:
	/* TsgcTCPWatchDog_Options.Create */ inline __fastcall virtual TsgcWSWatchDogClient_Options() : TsgcWSWatchDog_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSWatchDogClient_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSWatchDogMonitorServer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSWatchDogMonitorServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSWatchDogMonitorServer_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSWatchDogServer_Options : public TsgcWSWatchDog_Options
{
	typedef TsgcWSWatchDog_Options inherited;
	
private:
	TsgcWSWatchDogMonitorServer_Options* FMonitor;
	void __fastcall SetMonitor(TsgcWSWatchDogMonitorServer_Options* const Value);
	
public:
	__fastcall virtual TsgcWSWatchDogServer_Options();
	__fastcall virtual ~TsgcWSWatchDogServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSWatchDogMonitorServer_Options* Monitor = {read=FMonitor, write=SetMonitor};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSDrafts : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FHixie76;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Hixie76 = {read=FHixie76, write=FHixie76, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSDrafts() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSDrafts() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSpecifications : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSDrafts* FDrafts;
	bool FRFC6455;
	bool FRFC8441;
	void __fastcall SetDrafts(TsgcWSDrafts* const Value);
	
public:
	__fastcall virtual TsgcWSSpecifications();
	__fastcall virtual ~TsgcWSSpecifications();
	
__published:
	__property TsgcWSDrafts* Drafts = {read=FDrafts, write=SetDrafts};
	__property bool RFC6455 = {read=FRFC6455, write=FRFC6455, nodefault};
	
public:
	__property bool RFC8441 = {read=FRFC8441, write=FRFC8441, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueue : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsQueueMsgLevels FLevel;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TwsQueueMsgLevels Level = {read=FLevel, write=FLevel, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSQueue() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSQueue() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueueOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSQueue* FBinary;
	TsgcWSQueue* FPing;
	TsgcWSQueue* FText;
	
public:
	__fastcall virtual TsgcWSQueueOptions();
	__fastcall virtual ~TsgcWSQueueOptions();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSQueue* Binary = {read=FBinary, write=FBinary};
	__property TsgcWSQueue* Ping = {read=FPing, write=FPing};
	__property TsgcWSQueue* Text = {read=FText, write=FText};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHandshake_Base : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::UnicodeString FProtocols;
	System::UnicodeString FExtensions;
	
public:
	__property System::UnicodeString Protocols = {read=FProtocols};
	__property System::UnicodeString Extensions = {read=FExtensions};
public:
	/* TObject.Create */ inline __fastcall TsgcWSHandshake_Base() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHandshake_Base() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::Byte, 4> TMaskArray;

class PASCALIMPLEMENTATION TsgcWSConnection : public Sgctcp_classes::TsgcTCPConnection
{
	typedef Sgctcp_classes::TsgcTCPConnection inherited;
	
protected:
	bool __fastcall IsControlFrame(Sgcwebsocket_types::TOpcode aOpCode);
	
private:
	Sgcwebsocket_extensions::TsgcWSExtensions* FExtensions;
	
protected:
	virtual void __fastcall SetExtensions(Sgcwebsocket_extensions::TsgcWSExtensions* const Value);
	
public:
	__property Sgcwebsocket_extensions::TsgcWSExtensions* Extensions = {read=FExtensions, write=SetExtensions};
	
protected:
	TsgcQueueList* FQueueLevel0;
	TsgcQueueList* FQueueLevel1;
	TsgcQueueList* FQueueLevel2;
	TsgcQueueList* __fastcall GetQueueLevel0();
	TsgcQueueList* __fastcall GetQueueLevel1();
	TsgcQueueList* __fastcall GetQueueLevel2();
	__property TsgcQueueList* QueueLevel0 = {read=GetQueueLevel0, write=FQueueLevel0};
	__property TsgcQueueList* QueueLevel1 = {read=GetQueueLevel1, write=FQueueLevel1};
	__property TsgcQueueList* QueueLevel2 = {read=GetQueueLevel2, write=FQueueLevel2};
	
private:
	TsgcWSQueueOptions* FQueueOptions;
	void __fastcall SetQueueOptions(TsgcWSQueueOptions* const Value);
	
public:
	__property TsgcWSQueueOptions* QueueOptions = {read=FQueueOptions, write=SetQueueOptions};
	
private:
	int FCloseCode;
	System::UnicodeString FCloseReason;
	System::UnicodeString FMsgError;
	Sgcwebsocket_types::TwsSpecification FSpecification;
	
public:
	__property int CloseCode = {read=FCloseCode, write=FCloseCode, nodefault};
	__property System::UnicodeString MsgError = {read=FMsgError, write=FMsgError};
	__property Sgcwebsocket_types::TwsSpecification Specification = {read=FSpecification, write=FSpecification, nodefault};
	__property System::UnicodeString CloseReason = {read=FCloseReason, write=FCloseReason};
	
private:
	bool FMasked;
	System::UnicodeString FProtocol;
	bool FValidateUTF8;
	bool FRaiseDisconnectExceptions;
	Sgcwebsocket_types::TwsFragmentedMessages FFragmentedMessages;
	bool FCleanDisconnect;
	
protected:
	System::UnicodeString FURL;
	virtual System::UnicodeString __fastcall GetURL() = 0 ;
	
public:
	__property bool Masked = {read=FMasked, write=FMasked, nodefault};
	__property System::UnicodeString Protocol = {read=FProtocol, write=FProtocol};
	__property System::UnicodeString URL = {read=GetURL};
	__property bool ValidateUTF8 = {read=FValidateUTF8, write=FValidateUTF8, nodefault};
	__property bool RaiseDisconnectExceptions = {read=FRaiseDisconnectExceptions, write=FRaiseDisconnectExceptions, nodefault};
	__property Sgcwebsocket_types::TwsFragmentedMessages FragmentedMessages = {read=FFragmentedMessages, write=FFragmentedMessages, nodefault};
	__property bool CleanDisconnect = {read=FCleanDisconnect, write=FCleanDisconnect, nodefault};
	
private:
	Sgcwebsocket_classes_queues::TsgcQueue* FProtocolData;
	Sgcwebsocket_classes_queues::TsgcQueue* __fastcall GetProtocolData();
	
protected:
	__property Sgcwebsocket_classes_queues::TsgcQueue* ProtocolData = {read=GetProtocolData, write=FProtocolData};
	
private:
	System::TObject* FApiData;
	
protected:
	__property System::TObject* ApiData = {read=FApiData, write=FApiData};
	
private:
	System::Classes::TStringList* FSubscriptions;
	System::UnicodeString FLastSubscription;
	System::UnicodeString FLastUnSubscription;
	System::Classes::TStringList* __fastcall GetSubscriptions();
	void __fastcall SetDisconnected(const bool Value);
	
public:
	void __fastcall DoSubscribe(const System::UnicodeString aChannels);
	void __fastcall DoUnSubscribe(const System::UnicodeString aChannels);
	bool __fastcall Subscribed(const System::UnicodeString aChannel);
	__property System::Classes::TStringList* Subscriptions = {read=GetSubscriptions, write=FSubscriptions};
	__property System::UnicodeString LastSubscription = {read=FLastSubscription, write=FLastSubscription};
	__property System::UnicodeString LastUnSubscription = {read=FLastUnSubscription, write=FLastUnSubscription};
	
private:
	bool FDisconnected;
	
public:
	virtual void __fastcall Disconnect()/* overload */;
	virtual void __fastcall Disconnect(const int aCloseCode)/* overload */;
	
protected:
	virtual void __fastcall DoClose(int aCloseCode = 0x3e8);
	
public:
	virtual void __fastcall Close()/* overload */;
	virtual void __fastcall Close(const int aCloseCode)/* overload */;
	__property bool Disconnected = {read=FDisconnected, write=SetDisconnected, nodefault};
	
private:
	System::TDateTime FLastPing;
	System::TDateTime FLastPong;
	System::TDateTime FFirstPing;
	
protected:
	System::UnicodeString FPong;
	virtual void __fastcall DoQueuePing(const System::UnicodeString aText);
	virtual void __fastcall DoPing(const System::UnicodeString aText);
	
public:
	void __fastcall Ping(const System::UnicodeString aText)/* overload */;
	bool __fastcall Ping(const System::UnicodeString aText, int aTimeOut)/* overload */;
	__property System::TDateTime LastPing = {read=FLastPing, write=FLastPing};
	__property System::TDateTime FirstPing = {read=FFirstPing, write=FFirstPing};
	__property System::TDateTime LastPong = {read=FLastPong, write=FLastPong};
	
protected:
	System::Classes::TStringList* FHeadersResponse;
	bool FEnabled;
	System::Classes::TStringList* __fastcall GetHeadersResponse();
	
public:
	__property System::Classes::TStringList* HeadersResponse = {read=GetHeadersResponse};
	__property bool Enabled = {read=FEnabled, nodefault};
	
protected:
	TsgcWSUpdateEvent FOnUpdate;
	TsgcWSMessageEvent FOnMessage;
	TsgcWSBinaryEvent FOnBinary;
	TsgcWSHandshakeEvent FOnHandshake;
	TsgcWSSubscriptionEvent FOnSubscription;
	TsgcWSSubscriptionEvent FOnUnSubscription;
	TsgcWSFragmentedEvent FOnFragmented;
	void __fastcall DoMessageEvent(const System::UnicodeString aText);
	void __fastcall DoBinaryEvent(System::Classes::TMemoryStream* const aData);
	void __fastcall DoFragmentedEvent(System::Classes::TMemoryStream* const aData, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	bool FNotifyConnection;
	bool FNotifyDisconnection;
	__property bool NotifyConnection = {read=FNotifyConnection, nodefault};
	__property bool NotifyDisconnection = {read=FNotifyDisconnection, nodefault};
	
public:
	__property TsgcWSUpdateEvent OnUpdate = {read=FOnUpdate, write=FOnUpdate};
	__property TsgcWSMessageEvent OnMessage = {read=FOnMessage, write=FOnMessage};
	__property TsgcWSBinaryEvent OnBinary = {read=FOnBinary, write=FOnBinary};
	__property TsgcWSFragmentedEvent OnFragmented = {read=FOnFragmented, write=FOnFragmented};
	__property TsgcWSSubscriptionEvent OnSubscription = {read=FOnSubscription, write=FOnSubscription};
	__property TsgcWSSubscriptionEvent OnUnSubscription = {read=FOnUnSubscription, write=FOnUnSubscription};
	__property TsgcWSHandshakeEvent OnHandshake = {read=FOnHandshake, write=FOnHandshake};
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	virtual void __fastcall DoWriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	void __fastcall DoQueueData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	void __fastcall DoQueueData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	void __fastcall WriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	void __fastcall WriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	System::UnicodeString __fastcall WriteAndWaitData(const System::UnicodeString aText, const int aTimeOut = 0x2710);
	__fastcall virtual TsgcWSConnection();
	__fastcall virtual ~TsgcWSConnection();
};


class PASCALIMPLEMENTATION TsgcWSConnection_Abstract : public TsgcWSConnection
{
	typedef TsgcWSConnection inherited;
	
protected:
	virtual int __fastcall GetReadBufferSize() = 0 ;
	virtual void __fastcall DoReadBytes(Idglobal::TIdBytes &aBuffer, int aCount) = 0 ;
	virtual void __fastcall DoWriteBytes(Idglobal::TIdBytes aBuffer) = 0 ;
	virtual void __fastcall DoWriteLn(const System::UnicodeString aText = System::UnicodeString()) = 0 ;
	virtual void __fastcall DoWriteBufferOpen() = 0 ;
	virtual void __fastcall DoWriteBufferFlush() = 0 ;
	virtual void __fastcall DoWriteBufferClose() = 0 ;
public:
	/* TsgcWSConnection.Create */ inline __fastcall virtual TsgcWSConnection_Abstract() : TsgcWSConnection() { }
	/* TsgcWSConnection.Destroy */ inline __fastcall virtual ~TsgcWSConnection_Abstract() { }
	
};


class PASCALIMPLEMENTATION TsgcWSComponent_Base : public Sgctcp_classes::TsgcTCPComponent_Base
{
	typedef Sgctcp_classes::TsgcTCPComponent_Base inherited;
	
private:
	bool FQueueMessages;
	TsgcWSObjectList* FQueueMessage;
	TsgcWSObjectList* FQueueBinary;
	TsgcWSObjectList* FQueueFragmented;
	TsgcWSConnectEvent FOnConnect;
	TsgcWSDisconnectEvent FOnDisconnect;
	TsgcWSMessageEvent FOnMessage;
	TsgcWSBinaryEvent FOnBinary;
	TsgcWSFragmentedEvent FOnFragmented;
	TsgcWSErrorEvent FOnError;
	TsgcExceptionEvent FOnException;
	TsgcWSObjectList* FNotifyConnect;
	TsgcWSObjectList* FNotifyDisconnect;
	TsgcWSObjectList* FNotifyMessage;
	TsgcWSObjectList* FNotifyBinary;
	TsgcWSObjectList* FNotifyFragmented;
	TsgcWSObjectList* FNotifyError;
	TsgcWSObjectList* FNotifyException;
	bool FAsyncConnect;
	bool FAsyncDisconnect;
	bool FAsyncMessage;
	bool FAsyncBinary;
	bool FAsyncFragmented;
	bool FAsyncError;
	bool FAsyncException;
	void __fastcall DoAsyncConnect();
	void __fastcall DoAsyncDisconnect();
	void __fastcall DoAsyncMessage();
	void __fastcall DoAsyncBinary();
	void __fastcall DoAsyncFragmented();
	void __fastcall DoAsyncError();
	void __fastcall DoAsyncException();
	TsgcWSObjectList* __fastcall GetNotifyConnect();
	TsgcWSObjectList* __fastcall GetNotifyDisconnect();
	TsgcWSObjectList* __fastcall GetNotifyMessage();
	TsgcWSObjectList* __fastcall GetNotifyBinary();
	TsgcWSObjectList* __fastcall GetNotifyFragmented();
	TsgcWSObjectList* __fastcall GetNotifyError();
	TsgcWSObjectList* __fastcall GetNotifyException();
	__property TsgcWSObjectList* NotifyConnect = {read=GetNotifyConnect};
	__property TsgcWSObjectList* NotifyDisconnect = {read=GetNotifyDisconnect};
	__property TsgcWSObjectList* NotifyMessage = {read=GetNotifyMessage};
	__property TsgcWSObjectList* NotifyBinary = {read=GetNotifyBinary};
	__property TsgcWSObjectList* NotifyFragmented = {read=GetNotifyFragmented};
	__property TsgcWSObjectList* NotifyError = {read=GetNotifyError};
	__property TsgcWSObjectList* NotifyException = {read=GetNotifyException};
	TsgcWSObjectList* __fastcall GetQueueMessage();
	TsgcWSObjectList* __fastcall GetQueueBinary();
	TsgcWSObjectList* __fastcall GetQueueFragmented();
	
protected:
	__property TsgcWSObjectList* QueueMessage = {read=GetQueueMessage};
	__property TsgcWSObjectList* QueueBinary = {read=GetQueueBinary};
	__property TsgcWSObjectList* QueueFragmented = {read=GetQueueFragmented};
	virtual void __fastcall DoEventConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoEventMessage(TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Fin);
	virtual void __fastcall DoEventError(TsgcWSConnection* aConnection, const System::UnicodeString Error);
	virtual void __fastcall DoEventException(TsgcWSConnection* aConnection, const System::UnicodeString Error, System::Sysutils::Exception* aException);
	virtual void __fastcall DoNotifyConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyMessage(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyBinary(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyFragmented(TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	virtual void __fastcall DoNotifyError(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyException(TsgcWSConnection* aConnection)/* overload */;
	virtual void __fastcall DoNotifyException(const System::UnicodeString Error, System::Sysutils::Exception* aException)/* overload */;
	virtual void __fastcall DoException(TsgcWSConnection* aConnection, System::UnicodeString aMsgException, System::Sysutils::Exception* aException = (System::Sysutils::Exception*)(0x0));
	void __fastcall DoError(TsgcWSConnection* aConnection, System::UnicodeString E);
	
public:
	__fastcall virtual TsgcWSComponent_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSComponent_Base();
	
protected:
	bool FQueueProcessing;
	virtual void __fastcall SetQueueMessages(const bool Value);
	virtual void __fastcall DoProcessQueue();
	virtual void __fastcall DoClearQueue();
	
public:
	void __fastcall QueueClear();
	__property bool QueueMessages = {read=FQueueMessages, write=SetQueueMessages, nodefault};
	__property TsgcWSConnectEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property TsgcWSDisconnectEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property TsgcWSMessageEvent OnMessage = {read=FOnMessage, write=FOnMessage};
	__property TsgcWSBinaryEvent OnBinary = {read=FOnBinary, write=FOnBinary};
	__property TsgcWSFragmentedEvent OnFragmented = {read=FOnFragmented, write=FOnFragmented};
	__property TsgcWSErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TsgcExceptionEvent OnException = {read=FOnException, write=FOnException};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSLogFile : public Sgctcp_classes::TsgcTCPLogFile
{
	typedef Sgctcp_classes::TsgcTCPLogFile inherited;
	
private:
	bool FUnMaskFrames;
	
public:
	__fastcall virtual TsgcWSLogFile();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool UnMaskFrames = {read=FUnMaskFrames, write=FUnMaskFrames, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSLogFile() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSComponent : public TsgcWSComponent_Base
{
	typedef TsgcWSComponent_Base inherited;
	
protected:
	virtual void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	
public:
	void __fastcall EnterCS();
	void __fastcall LeaveCS();
	bool __fastcall AssignedCS();
	
protected:
	System::Contnrs::TObjectList* FProtocolObjectList;
	virtual bool __fastcall ProtocolRegistered(System::UnicodeString aProtocolName, TsgcWSProtocol* aProtocol);
	System::UnicodeString __fastcall GetProtocols();
	virtual void __fastcall DoRegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	virtual void __fastcall DoRegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	virtual void __fastcall DoUnRegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	virtual void __fastcall DoUnRegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	TsgcWSAPI* FAPI;
	virtual void __fastcall RegisterAPI(TsgcWSAPI* aObject);
	virtual void __fastcall UnRegisterAPI(TsgcWSAPI* aObject);
	
private:
	TsgcWSSpecifications* FSpecifications;
	
protected:
	virtual TsgcWSSpecifications* __fastcall GetSpecifications();
	virtual void __fastcall SetSpecifications(TsgcWSSpecifications* const Value);
	
public:
	__property TsgcWSSpecifications* Specifications = {read=GetSpecifications, write=SetSpecifications};
	
private:
	TsgcWSLogFile* FLogFile;
	
protected:
	virtual TsgcWSLogFile* __fastcall GetLogFile();
	virtual void __fastcall SetLogFile(TsgcWSLogFile* const Value);
	
public:
	__property TsgcWSLogFile* LogFile = {read=GetLogFile, write=SetLogFile};
	
private:
	TsgcWSHeartBeat_Options* FHeartBeat;
	
protected:
	Sgcbase_classes::TsgcTimer* FHeartBeatTimeoutTimer;
	Sgcbase_classes::TsgcTimer* FHeartBeatTimer;
	virtual void __fastcall OnHeartBeatTimeoutEvent(System::TObject* Sender) = 0 ;
	virtual void __fastcall OnHeartBeatTimeoutExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E) = 0 ;
	void __fastcall DoStartHeartBeatTimeout();
	void __fastcall DoStopHeartBeatTimeout();
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender) = 0 ;
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E) = 0 ;
	virtual void __fastcall DoStartHeartBeat();
	virtual void __fastcall DoStopHeartBeat();
	virtual TsgcWSHeartBeat_Options* __fastcall GetHeartBeat();
	virtual void __fastcall SetHeartBeat(TsgcWSHeartBeat_Options* const Value);
	
public:
	__property TsgcWSHeartBeat_Options* HeartBeat = {read=GetHeartBeat, write=SetHeartBeat};
	
private:
	Sgctcp_classes::TsgcTCPKeepAlive* FTCPKeepAlive;
	void __fastcall SetTCPKeepAlive(Sgctcp_classes::TsgcTCPKeepAlive* const Value);
	
public:
	__property Sgctcp_classes::TsgcTCPKeepAlive* TCPKeepAlive = {read=FTCPKeepAlive, write=SetTCPKeepAlive};
	
protected:
	int FWatchDogAttempts;
	Sgcbase_classes::TsgcTimer* FWatchDogTimer;
	bool FWatchDogEnabled;
	virtual void __fastcall DoStartWatchDog();
	virtual void __fastcall DoStopWatchDog();
	virtual TsgcWSWatchDog_Options* __fastcall GetWatchDog() = 0 ;
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E) = 0 ;
	
private:
	void __fastcall DoWriteQueueData(TsgcWSConnection* const aConnection, TsgcQueueList* const aQueue);
	
protected:
	virtual void __fastcall DoWriteQueueMsgLevels(TsgcWSConnection* const aConnection);
	
private:
	Sgcwebsocket_extensions::TsgcWSExtensions* FExtensions;
	
protected:
	virtual Sgcwebsocket_extensions::TsgcWSExtensions* __fastcall GetExtensions();
	virtual void __fastcall SetExtensions(Sgcwebsocket_extensions::TsgcWSExtensions* const Value);
	
public:
	__property Sgcwebsocket_extensions::TsgcWSExtensions* Extensions = {read=GetExtensions, write=SetExtensions};
	
protected:
	virtual void __fastcall DoNotifyConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyError(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyException(TsgcWSConnection* aConnection)/* overload */;
	virtual void __fastcall DoNotifyMessage(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyBinary(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyFragmented(TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	
private:
	System::Contnrs::TObjectList* FSyncHandshake;
	System::Contnrs::TObjectList* __fastcall GetSyncHandshake();
	void __fastcall DoAsyncHandshake();
	TsgcWSHandshakeEvent FOnHandshake;
	
protected:
	virtual void __fastcall DoEventHandshake(TsgcWSConnection* aConnection, System::Classes::TStringList* aHeaders);
	virtual void __fastcall DoNotifyHandshake(TsgcWSConnection* aConnection);
	__property System::Contnrs::TObjectList* SyncHandshake = {read=GetSyncHandshake};
	
public:
	__property TsgcWSHandshakeEvent OnHandshake = {read=FOnHandshake, write=FOnHandshake};
	__fastcall virtual TsgcWSComponent(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSComponent();
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoNotifyException(const System::UnicodeString Error, System::Sysutils::Exception* aException){ TsgcWSComponent_Base::DoNotifyException(Error, aException); }
	
};


class PASCALIMPLEMENTATION TsgcWSComponent_Server : public TsgcWSComponent
{
	typedef TsgcWSComponent inherited;
	
protected:
	virtual void __fastcall DoDestroyConnection(Sgcsocket_classes::TsgcSocketConnection* aConnection);
	
public:
	virtual void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	
protected:
	virtual void __fastcall DoNotifyConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(TsgcWSConnection* aConnection);
	
public:
	void __fastcall RegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	void __fastcall RegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	void __fastcall UnRegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	void __fastcall UnRegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	virtual void __fastcall Broadcast(const System::UnicodeString aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString()) = 0 /* overload */;
	virtual void __fastcall Broadcast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	virtual bool __fastcall WriteData(const System::UnicodeString aGuid, const System::UnicodeString aMessage) = 0 /* overload */;
	virtual bool __fastcall WriteData(const System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	
protected:
	virtual bool __fastcall DoHTTPRequestAPI(const System::UnicodeString aMethod, const System::UnicodeString aDocument, const System::UnicodeString aQueryParams, const System::UnicodeString aContent, TsgcHTTPResponse* &Response);
	
private:
	TsgcCacheQueueListConnections* FConnections;
	
protected:
	TsgcCacheQueueListConnections* __fastcall GetConnections();
	
private:
	TsgcQueueListChannels* FChannels;
	
protected:
	TsgcQueueListChannels* __fastcall GetChannels();
	
private:
	Sgcwebsocket_classes_queues::TsgcQueueCommon* FConnectionsFree;
	
protected:
	Sgcbase_classes::TsgcTimer* FConnectionsFreeTimer;
	Sgcwebsocket_classes_queues::TsgcQueueCommon* __fastcall GetConnectionsFree();
	virtual void __fastcall DoStartConnectionFree();
	virtual void __fastcall DoStopConnectionFree();
	virtual void __fastcall OnConnectionsFreeEvent(System::TObject* Sender);
	virtual void __fastcall OnConnectionsFreeExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
private:
	TsgcWSOptimization_Options* FOptimizations;
	void __fastcall SetOptimizations(TsgcWSOptimization_Options* const Value);
	
public:
	__property TsgcWSOptimization_Options* Optimizations = {read=FOptimizations, write=SetOptimizations};
	
protected:
	TsgcWSWatchDogServer_Options* FWatchDog;
	virtual void __fastcall SetWatchDog(TsgcWSWatchDogServer_Options* const Value);
	virtual TsgcWSWatchDog_Options* __fastcall GetWatchDog();
	
public:
	__property TsgcWSWatchDogServer_Options* WatchDog = {read=FWatchDog, write=SetWatchDog};
	__fastcall virtual TsgcWSComponent_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSComponent_Server();
	virtual System::Classes::TList* __fastcall LockList() = 0 ;
	virtual void __fastcall UnLockList() = 0 ;
	
private:
	TsgcWSUnknownProtocolEvent FOnUnknownProtocol;
	
protected:
	virtual bool __fastcall DoUnknownProtocol(TsgcWSConnection* aConnnection);
	
public:
	__property TsgcWSUnknownProtocolEvent OnUnknownProtocol = {read=FOnUnknownProtocol, write=FOnUnknownProtocol};
};


class PASCALIMPLEMENTATION TsgcWSComponent_Client : public TsgcWSComponent
{
	typedef TsgcWSComponent inherited;
	
private:
	bool FFreeConnectionOnDestroy;
	void __fastcall DoDestroyDelayedConnection();
	
protected:
	TsgcWSConnection* FDelayedFreeConnection;
	virtual void __fastcall DoDestroyConnection(Sgcsocket_classes::TsgcSocketConnection* aConnection);
	virtual void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	virtual void __fastcall DoEventConnect(TsgcWSConnection* aConnection);
	
public:
	__property bool FreeConnectionOnDestroy = {read=FFreeConnectionOnDestroy, write=FFreeConnectionOnDestroy, nodefault};
	void __fastcall RegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	void __fastcall RegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	void __fastcall UnRegisterProtocol(TsgcWSProtocol* aObject)/* overload */;
	void __fastcall UnRegisterProtocol(System::UnicodeString aProtocol)/* overload */;
	virtual void __fastcall RegisterAPI(TsgcWSAPI* aObject);
	virtual void __fastcall UnRegisterAPI(TsgcWSAPI* aObject);
	
protected:
	TsgcWSWatchDogClient_Options* FWatchDog;
	virtual void __fastcall SetWatchDog(TsgcWSWatchDogClient_Options* const Value);
	virtual TsgcWSWatchDog_Options* __fastcall GetWatchDog();
	
public:
	__property TsgcWSWatchDogClient_Options* WatchDog = {read=FWatchDog, write=SetWatchDog};
	__fastcall virtual TsgcWSComponent_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSComponent_Client();
	virtual void __fastcall WriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	virtual void __fastcall WriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)) = 0 /* overload */;
	virtual System::UnicodeString __fastcall WriteAndWaitData(const System::UnicodeString aText, const int aTimeOut = 0x2710) = 0 ;
	
protected:
	virtual void __fastcall SetURL(const System::UnicodeString Value) = 0 ;
	
public:
	__property System::UnicodeString URL = {write=SetURL};
};


class PASCALIMPLEMENTATION TsgcWSComponent_WSClient : public TsgcWSComponent_Client
{
	typedef TsgcWSComponent_Client inherited;
	
private:
	System::UnicodeString FHost;
	int FPort;
	bool FTLS;
	TsgcWSTLS_Options* FTLSOptions;
	
protected:
	virtual bool __fastcall GetActive() = 0 ;
	virtual void __fastcall SetActive(const bool Value);
	virtual void __fastcall SetTLSOptions(TsgcWSTLS_Options* const Value);
	
public:
	__fastcall virtual TsgcWSComponent_WSClient(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSComponent_WSClient();
	__property bool Active = {read=GetActive, write=SetActive, default=0};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	__property TsgcWSTLS_Options* TLSOptions = {read=FTLSOptions, write=SetTLSOptions};
};


class PASCALIMPLEMENTATION TsgcWSComponent_WSClient_API : public TsgcWSComponent_WSClient
{
	typedef TsgcWSComponent_WSClient inherited;
	
public:
	virtual void __fastcall DoStartHeartBeat();
	virtual void __fastcall DoStopHeartBeat();
public:
	/* TsgcWSComponent_WSClient.Create */ inline __fastcall virtual TsgcWSComponent_WSClient_API(System::Classes::TComponent* aOwner) : TsgcWSComponent_WSClient(aOwner) { }
	/* TsgcWSComponent_WSClient.Destroy */ inline __fastcall virtual ~TsgcWSComponent_WSClient_API() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI : public TsgcWSComponent_Base
{
	typedef TsgcWSComponent_Base inherited;
	
private:
	bool FRawMessages;
	
protected:
	virtual bool __fastcall IsInternalMessage(const System::UnicodeString aMessage);
	
public:
	__fastcall virtual TsgcWSAPI(System::Classes::TComponent* aOwner);
	__property bool RawMessages = {read=FRawMessages, write=FRawMessages, stored=false, nodefault};
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_client : public TsgcWSAPI
{
	typedef TsgcWSAPI inherited;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	TsgcWSComponent_WSClient* FClient;
	virtual void __fastcall SetClient(TsgcWSComponent_WSClient* const Value);
	
public:
	__property TsgcWSComponent_WSClient* Client = {read=FClient, write=SetClient};
	
protected:
	virtual void __fastcall DoNotifyConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoBeforeConnect();
	virtual System::UnicodeString __fastcall GetURL();
	
public:
	__property System::UnicodeString URL = {read=GetURL};
	
protected:
	virtual bool __fastcall DoKeepAlive();
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWSAPI_client(System::Classes::TComponent* aOwner) : TsgcWSAPI(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_client() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_client_KeepAlive : public TsgcWSAPI_client
{
	typedef TsgcWSAPI_client inherited;
	
public:
	virtual bool __fastcall DoKeepAlive();
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWSAPI_client_KeepAlive(System::Classes::TComponent* aOwner) : TsgcWSAPI_client(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_client_KeepAlive() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_server : public TsgcWSAPI
{
	typedef TsgcWSAPI inherited;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	TsgcWSComponent_Server* FServer;
	virtual void __fastcall SetServer(TsgcWSComponent_Server* const Value);
	
public:
	__property TsgcWSComponent_Server* Server = {read=FServer, write=SetServer};
	
private:
	System::Classes::TStringList* FResources;
	System::Classes::TStringList* __fastcall GetResources();
	
protected:
	__property System::Classes::TStringList* Resources = {read=GetResources, write=FResources};
	virtual void __fastcall ClearResources();
	void __fastcall AddResource(const System::UnicodeString aDocument, const System::UnicodeString aResource);
	System::UnicodeString __fastcall GetResource(const System::UnicodeString aDocument);
	virtual bool __fastcall DoHTTPRequest(TsgcHTTPRequest* const aRequest, TsgcHTTPResponse* const aResponse);
	virtual bool __fastcall DoHTTPRequestApi(TsgcHTTPRequest* const aRequest, TsgcHTTPResponse* const aResponse) = 0 ;
	virtual void __fastcall DoHTTPBeforeResourceResponse(TsgcHTTPRequest* const aRequest, TsgcHTTPResponse* const aResponse) = 0 ;
	
public:
	__fastcall virtual TsgcWSAPI_server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSAPI_server();
};


class PASCALIMPLEMENTATION TsgcQueueItemProtocolMessage : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::Classes::TComponent* FWSMessage;
	
public:
	__fastcall virtual TsgcQueueItemProtocolMessage();
	__fastcall virtual ~TsgcQueueItemProtocolMessage();
	__property System::Classes::TComponent* WSMessage = {read=FWSMessage, write=FWSMessage};
};


class PASCALIMPLEMENTATION TsgcWSProtocol : public TsgcWSComponent_Base
{
	typedef TsgcWSComponent_Base inherited;
	
public:
	void __fastcall EnterCS();
	void __fastcall LeaveCS();
	
protected:
	System::UnicodeString FGuid;
	System::UnicodeString FProtocol;
	Sgcwebsocket_types::TwsProtocolMessage FMsgType;
	
public:
	__property System::UnicodeString Protocol = {read=FProtocol};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property Sgcwebsocket_types::TwsProtocolMessage MsgType = {read=FMsgType, write=FMsgType, nodefault};
	
protected:
	virtual void __fastcall DoInitialize(TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(TsgcWSConnection* aConnection);
	TsgcQueueItemProtocolMessage* __fastcall GetProtocolDataItem(TsgcWSConnection* aConnection, const System::UnicodeString aID);
	void __fastcall AddProtocolDataItem(TsgcWSConnection* const aConnection, const System::UnicodeString aID, TsgcQueueItemProtocolMessage* const aItem);
	
private:
	TsgcWSRawMessageEvent FOnRawMessage;
	
protected:
	virtual bool __fastcall DoRawMessage(TsgcWSConnection* aConnection, const System::UnicodeString Text);
	__property TsgcWSRawMessageEvent OnRawMessage = {read=FOnRawMessage, write=FOnRawMessage};
public:
	/* TsgcWSComponent_Base.Create */ inline __fastcall virtual TsgcWSProtocol(System::Classes::TComponent* aOwner) : TsgcWSComponent_Base(aOwner) { }
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSProtocol() { }
	
};


class PASCALIMPLEMENTATION TsgcWSProtocol_Client : public TsgcWSProtocol
{
	typedef TsgcWSProtocol inherited;
	
protected:
	virtual void __fastcall DoDestroyConnection(Sgcsocket_classes::TsgcSocketConnection* aConnection);
	virtual void __fastcall DoNotifyBinary(TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	
private:
	TsgcWSComponent_WSClient* FClient;
	
protected:
	virtual void __fastcall SetClient(TsgcWSComponent_WSClient* const Value);
	
public:
	__property TsgcWSComponent_WSClient* Client = {read=FClient, write=SetClient};
	
protected:
	virtual bool __fastcall DoHandleEventConnect();
	virtual bool __fastcall DoKeepAlive();
	
public:
	__fastcall virtual TsgcWSProtocol_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Client();
};


class PASCALIMPLEMENTATION TsgcWSProtocol_Server : public TsgcWSProtocol
{
	typedef TsgcWSProtocol inherited;
	
protected:
	virtual void __fastcall DoDestroyConnection(Sgcsocket_classes::TsgcSocketConnection* aConnection);
	virtual void __fastcall DoNotifyBinary(TsgcWSConnection* aConnection);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual System::UnicodeString __fastcall GetHTTPResponse(TsgcWSConnection* const aConnection, const System::UnicodeString aPath, const System::UnicodeString aBody);
	
private:
	TsgcWSComponent_Server* FServer;
	
protected:
	virtual void __fastcall SetServer(TsgcWSComponent_Server* const Value);
	
public:
	__property TsgcWSComponent_Server* Server = {read=FServer, write=SetServer};
	__fastcall virtual TsgcWSProtocol_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Server();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_Session : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthentication_Session() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthentication_Session() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_URL : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthentication_URL() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthentication_URL() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_Basic : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthentication_Basic() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthentication_Basic() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_Token : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAuthName;
	System::UnicodeString FAuthToken;
	bool FEnabled;
	
public:
	__fastcall virtual TsgcWSAuthentication_Token();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString AuthName = {read=FAuthName, write=FAuthName};
	__property System::UnicodeString AuthToken = {read=FAuthToken, write=FAuthToken};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthentication_Token() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthentication_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthentication_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthentication_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLASSES)
using namespace Sgcwebsocket_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_classesHPP
