// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWebSocketClient.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwebsocketclientHPP
#define Fmx_TmsfncwebsocketclientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <IdStack.hpp>
#include <IdGlobal.hpp>
#include <IdStackConsts.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <IdTCPConnection.hpp>
#include <IdIOHandlerSocket.hpp>
#include <IdIOHandler.hpp>
#include <IdTCPClient.hpp>
#include <IdSSLOpenSSL.hpp>
#include <FMX.TMSFNCWebSocketCommon.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwebsocketclient
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EWebSocketClient;
class DELPHICLASS TTMSFNCWebSocketHandshakeRequest;
class DELPHICLASS TTMSFNCWebSocketHandshakeResponse;
class DELPHICLASS TTMSFNCWebSocketClientConnection;
class DELPHICLASS TTMSFNCWebSocketMessageDriver;
class DELPHICLASS TTMSFNCWebSocketThreadedMessageDriver;
class DELPHICLASS TTMSFNCCustomWebsocketClient;
class DELPHICLASS TTMSFNCWebsocketClient;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EWebSocketClient : public Fmx::Tmsfncwebsocketcommon::EWebSocket
{
	typedef Fmx::Tmsfncwebsocketcommon::EWebSocket inherited;
	
public:
	/* Exception.Create */ inline __fastcall EWebSocketClient(const System::UnicodeString Msg) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EWebSocketClient(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EWebSocketClient(NativeUInt Ident)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EWebSocketClient(System::PResStringRec ResStringRec)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebSocketClient(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebSocketClient(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EWebSocketClient(const System::UnicodeString Msg, int AHelpContext) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EWebSocketClient(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebSocketClient(NativeUInt Ident, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebSocketClient(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebSocketClient(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebSocketClient(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EWebSocketClient() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCWebSocketHandshakeRequest : public Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketRequest
{
	typedef Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketRequest inherited;
	
private:
	System::Word FPort;
	System::Classes::TStrings* FExtraHeaders;
	void __fastcall SetExtraHeaders(System::Classes::TStrings* const Value);
	
protected:
	void __fastcall Add(System::Classes::TStrings* AHeaders, const System::UnicodeString AName, const System::UnicodeString AValue, const System::UnicodeString ADefault);
	
public:
	__fastcall virtual TTMSFNCWebSocketHandshakeRequest()/* overload */;
	__fastcall virtual ~TTMSFNCWebSocketHandshakeRequest();
	static System::UnicodeString __fastcall GenerateKey();
	void __fastcall ToStrings(System::Classes::TStrings* aHeaders);
	__property System::Word Port = {read=FPort, write=FPort, nodefault};
	__property System::Classes::TStrings* ExtraHeaders = {read=FExtraHeaders, write=SetExtraHeaders};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TTMSFNCWebSocketIncomingResult : unsigned char { irNone, irOK, irClose };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCWebSocketHandshakeResponse : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FVersion;
	System::UnicodeString FExtensions;
	System::UnicodeString FProtocol;
	System::UnicodeString FUpgrade;
	System::UnicodeString FKey;
	System::UnicodeString FAccept;
	System::Classes::TStrings* FRawHeaders;
	System::UnicodeString FConnection;
	
public:
	__fastcall TTMSFNCWebSocketHandshakeResponse(System::Classes::TStrings* aHeaders);
	__fastcall virtual ~TTMSFNCWebSocketHandshakeResponse();
	__property System::Classes::TStrings* RawHeaders = {read=FRawHeaders};
	__property System::UnicodeString Upgrade = {read=FUpgrade};
	__property System::UnicodeString Protocol = {read=FProtocol};
	__property System::UnicodeString Version = {read=FVersion};
	__property System::UnicodeString Extensions = {read=FExtensions};
	__property System::UnicodeString Accept = {read=FAccept};
	__property System::UnicodeString Key = {read=FKey};
	__property System::UnicodeString Connection = {read=FConnection};
	System::UnicodeString __fastcall ToJSON()/* overload */;
	void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	void __fastcall Log();
	void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCWebSocketClientConnection : public Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection
{
	typedef Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection inherited;
	
private:
	Idtcpclient::TIdTCPClient* FClient;
	TTMSFNCWebSocketHandshakeResponse* FHandshakeResponse;
	
protected:
	__fastcall TTMSFNCWebSocketClientConnection(Idtcpclient::TIdTCPClient* aClient, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions AOptions)/* overload */;
	virtual bool __fastcall ReadMessage();
	virtual System::UnicodeString __fastcall GetPeerIP();
	virtual Idtcpconnection::TIdTCPConnection* __fastcall GetConnection();
	virtual bool __fastcall GetHandshakeCompleted();
	TTMSFNCWebSocketIncomingResult __fastcall CheckIncoming(int ATimeOut);
	__property Idtcpclient::TIdTCPClient* Client = {read=FClient};
	__property TTMSFNCWebSocketHandshakeResponse* HandShakeResponse = {read=FHandshakeResponse, write=FHandshakeResponse};
	
public:
	__fastcall virtual ~TTMSFNCWebSocketClientConnection();
};


typedef void __fastcall (__closure *TTMSFNCWebsocketClientHandShakeEvent)(System::TObject* Sender, System::Classes::TStrings* AHeaders);

typedef void __fastcall (__closure *TTMSFNCWebsocketClientHandshakeResponseEvent)(System::TObject* Sender, TTMSFNCWebSocketHandshakeResponse* AResponse, bool &AAllow);

typedef void __fastcall (__closure *TTMSFNCWebSocketDriverErrorEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

class PASCALIMPLEMENTATION TTMSFNCWebSocketMessageDriver : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FInterval;
	System::Classes::TThreadList* FList;
	Idstack::TIdSocketList* FReadSet;
	Idstack::TIdSocketList* FExceptionSet;
	TTMSFNCWebSocketDriverErrorEvent FOnDriverError;
	
protected:
	bool __fastcall WaitForData();
	virtual bool __fastcall CheckConnections();
	void __fastcall ReadConnections();
	__property System::Classes::TThreadList* List = {read=FList};
	
public:
	__fastcall virtual TTMSFNCWebSocketMessageDriver(int aInterval);
	__fastcall virtual ~TTMSFNCWebSocketMessageDriver();
	void __fastcall AddClient(TTMSFNCWebSocketClientConnection* aConnection);
	void __fastcall RemoveClient(TTMSFNCWebSocketClientConnection* aConnection);
	virtual void __fastcall Execute() = 0 ;
	virtual void __fastcall Terminate() = 0 ;
	__property int Interval = {read=FInterval, nodefault};
	__property TTMSFNCWebSocketDriverErrorEvent OnDriverError = {read=FOnDriverError, write=FOnDriverError};
	System::UnicodeString __fastcall ToJSON()/* overload */;
	void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	void __fastcall Log();
	void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketThreadedMessageDriver : public TTMSFNCWebSocketMessageDriver
{
	typedef TTMSFNCWebSocketMessageDriver inherited;
	
	
protected:
	class DELPHICLASS TTMSFNCWebSocketMessageDriverThread;
	class PASCALIMPLEMENTATION TTMSFNCWebSocketMessageDriverThread : public System::Classes::TThread
	{
		typedef System::Classes::TThread inherited;
		
	public:
		TTMSFNCWebSocketThreadedMessageDriver* FDriver;
		__fastcall TTMSFNCWebSocketMessageDriverThread(TTMSFNCWebSocketThreadedMessageDriver* aDriver);
		virtual void __fastcall Execute();
	public:
		/* TThread.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketMessageDriverThread() { }
		
	};
	
	
	
private:
	System::Classes::TThread* FThread;
	
public:
	virtual void __fastcall Execute();
	virtual void __fastcall Terminate();
public:
	/* TTMSFNCWebSocketMessageDriver.Create */ inline __fastcall virtual TTMSFNCWebSocketThreadedMessageDriver(int aInterval) : TTMSFNCWebSocketMessageDriver(aInterval) { }
	/* TTMSFNCWebSocketMessageDriver.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketThreadedMessageDriver() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomWebsocketClient : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	static TTMSFNCWebSocketThreadedMessageDriver* _defaultDriver;
	int FPort;
	bool FActive;
	bool FLoadActive;
	System::UnicodeString FHostName;
	bool FUseSSL;
	System::UnicodeString FPathName;
	int FConnectTimeout;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions FOptions;
	Idtcpclient::TIdTCPClient* FClient;
	int FCheckTimeOut;
	bool FAutoCheckMessages;
	TTMSFNCWebSocketHandshakeRequest* FHandShake;
	TTMSFNCWebSocketMessageDriver* FMessageDriver;
	TTMSFNCWebSocketHandshakeResponse* FHandshakeResponse;
	TTMSFNCWebsocketClientHandShakeEvent FOnSendHandShake;
	TTMSFNCWebsocketClientHandshakeResponseEvent FOnHandshakeResponse;
	TTMSFNCWebSocketClientConnection* FConnection;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketMessageEvent FOnMessageReceived;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnPing;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnPong;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent FOnDisconnect;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketBinDataEvent FOnBinaryDataReceived;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnClose;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent FOnConnect;
	System::UnicodeString FOrigin;
	int FAutoCheckInterval;
	bool FSplitMessage;
	unsigned __int64 FFrameSize;
	bool FAutoSyncEvents;
	bool __fastcall GetActive();
	void __fastcall SetActive(const bool Value);
	void __fastcall SetHostName(const System::UnicodeString Value);
	void __fastcall SetPort(const int Value);
	void __fastcall SetUseSSL(const bool Value);
	void __fastcall SetConnectTimeout(const int Value);
	void __fastcall SetPathName(const System::UnicodeString Value);
	void __fastcall SetCheckTimeOut(const int Value);
	void __fastcall SetOptions(const Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions Value);
	TTMSFNCWebSocketMessageDriver* __fastcall IntGetMessageDriver();
	void __fastcall SetAutoCheckMessages(const bool Value);
	void __fastcall SendHeaders(System::Classes::TStrings* aHeaders);
	void __fastcall SocketDisconnected(System::TObject* Sender);
	void __fastcall SetOrigin(const System::UnicodeString Value);
	void __fastcall SetAutoCheckInterval(const int Value);
	void __fastcall SetFrameSize(const unsigned __int64 Value);
	void __fastcall SetSplitMessage(const bool Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	void __fastcall CheckInactive();
	virtual void __fastcall Loaded();
	virtual TTMSFNCWebSocketClientConnection* __fastcall CreateClientSocket(Idtcpclient::TIdTCPClient* AClient);
	virtual void __fastcall MessageReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::UnicodeString AMessage);
	virtual void __fastcall BinaryDataReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> AData, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketFrameTypes FrameType);
	virtual void __fastcall PingReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> AData);
	virtual void __fastcall PongReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> AData);
	virtual void __fastcall CloseReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> AData);
	virtual bool __fastcall CheckHandShakeResponse(System::Classes::TStrings* AHeaders);
	virtual TTMSFNCWebSocketHandshakeRequest* __fastcall CreateHandShakeRequest();
	virtual TTMSFNCWebSocketHandshakeResponse* __fastcall CreateHandshakeResponse(System::Classes::TStrings* AHeaders);
	virtual void __fastcall SendHandShakeRequest();
	virtual bool __fastcall ReadHandShakeResponse();
	bool __fastcall DoHandShake();
	TTMSFNCWebSocketMessageDriver* __fastcall CreateMessageDriver();
	void __fastcall DoHandleDisconnect();
	__property bool AutoSyncEvents = {read=FAutoSyncEvents, write=FAutoSyncEvents, default=1};
	__property TTMSFNCWebSocketClientConnection* Connection = {read=FConnection};
	__property TTMSFNCWebSocketMessageDriver* MessageDriver = {read=IntGetMessageDriver};
	__property bool SplitMessage = {read=FSplitMessage, write=SetSplitMessage, default=0};
	__property unsigned __int64 FrameSize = {read=FFrameSize, write=SetFrameSize, default=0};
	__property System::UnicodeString HostName = {read=FHostName, write=SetHostName};
	__property int Port = {read=FPort, write=SetPort, default=8888};
	__property bool Active = {read=GetActive, write=SetActive, default=0};
	__property bool UseSSL = {read=FUseSSL, write=SetUseSSL, default=0};
	__property int ConnectTimeout = {read=FConnectTimeout, write=SetConnectTimeout, default=0};
	__property System::UnicodeString PathName = {read=FPathName, write=SetPathName};
	__property System::UnicodeString Origin = {read=FOrigin, write=SetOrigin};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions Options = {read=FOptions, write=SetOptions, default=0};
	__property int CheckTimeOut = {read=FCheckTimeOut, write=SetCheckTimeOut, default=0};
	__property bool AutoCheckMessages = {read=FAutoCheckMessages, write=SetAutoCheckMessages, default=1};
	__property int AutoCheckInterval = {read=FAutoCheckInterval, write=SetAutoCheckInterval, default=50};
	__property TTMSFNCWebsocketClientHandShakeEvent OnSendHandshake = {read=FOnSendHandShake, write=FOnSendHandShake};
	__property TTMSFNCWebsocketClientHandshakeResponseEvent OnHandshakeResponse = {read=FOnHandshakeResponse, write=FOnHandshakeResponse};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketMessageEvent OnMessageReceived = {read=FOnMessageReceived, write=FOnMessageReceived};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketBinDataEvent OnBinaryDataReceived = {read=FOnBinaryDataReceived, write=FOnBinaryDataReceived};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnPing = {read=FOnPing, write=FOnPing};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnPong = {read=FOnPong, write=FOnPong};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnClose = {read=FOnClose, write=FOnClose};
	
public:
	__fastcall virtual TTMSFNCCustomWebsocketClient()/* overload */;
	__fastcall virtual TTMSFNCCustomWebsocketClient(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCCustomWebsocketClient();
	void __fastcall Connect();
	void __fastcall Send(System::DynamicArray<System::Byte> ABytes)/* overload */;
	void __fastcall Send(const System::UnicodeString AMessage)/* overload */;
	void __fastcall SendMasked(System::DynamicArray<System::Byte> ABytes)/* overload */;
	void __fastcall SendMasked(const System::UnicodeString AMessage)/* overload */;
	void __fastcall Ping(System::UnicodeString AMessage);
	void __fastcall Disconnect(bool ASendClose = true);
	TTMSFNCWebSocketIncomingResult __fastcall CheckIncoming();
};


class PASCALIMPLEMENTATION TTMSFNCWebsocketClient : public TTMSFNCCustomWebsocketClient
{
	typedef TTMSFNCCustomWebsocketClient inherited;
	
public:
	__property Origin = {default=0};
	__property AutoSyncEvents = {default=1};
	
__published:
	__property Active = {default=0};
	__property HostName = {default=0};
	__property Port = {default=8888};
	__property UseSSL = {default=0};
	__property PathName = {default=0};
	__property Options = {default=0};
	__property CheckTimeout = {default=0};
	__property AutoCheckMessages = {default=1};
	__property AutoCheckInterval = {default=50};
	__property SplitMessage = {default=0};
	__property FrameSize = {default=0};
	__property ConnectTimeout = {default=0};
	__property OnConnect;
	__property OnDisconnect;
	__property OnSendHandshake;
	__property OnHandshakeResponse;
	__property OnBinaryDataReceived;
	__property OnMessageReceived;
	__property OnPing;
	__property OnPong;
public:
	/* TTMSFNCCustomWebsocketClient.Create */ inline __fastcall virtual TTMSFNCWebsocketClient()/* overload */ : TTMSFNCCustomWebsocketClient() { }
	/* TTMSFNCCustomWebsocketClient.Create */ inline __fastcall virtual TTMSFNCWebsocketClient(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomWebsocketClient(AOwner) { }
	/* TTMSFNCCustomWebsocketClient.Destroy */ inline __fastcall virtual ~TTMSFNCWebsocketClient() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MaxheaderCount = System::Int8(0x64);
}	/* namespace Tmsfncwebsocketclient */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWEBSOCKETCLIENT)
using namespace Fmx::Tmsfncwebsocketclient;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwebsocketclientHPP
