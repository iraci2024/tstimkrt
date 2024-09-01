// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWebSocketServer.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwebsocketserverHPP
#define Fmx_TmsfncwebsocketserverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IniFiles.hpp>
#include <IdContext.hpp>
#include <IdIOHandler.hpp>
#include <IdTCPConnection.hpp>
#include <IdCustomHTTPServer.hpp>
#include <System.Generics.Collections.hpp>
#include <IdHTTPServer.hpp>
#include <IdScheduler.hpp>
#include <IdSchedulerOfThreadDefault.hpp>
#include <IdHeaderList.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdGlobal.hpp>
#include <FMX.TMSFNCWebSocketCommon.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <IdCustomTCPServer.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwebsocketserver
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EWebsocketServer;
class DELPHICLASS TTMSFNCWebSocketServerConnection;
class DELPHICLASS TTMSFNCWebSocketConnections;
class DELPHICLASS TTMSFNCWebSocketHTTPServer;
__interface DELPHIINTERFACE TTMSFNCWebSocketSendToCallBack;
typedef System::DelphiInterface<TTMSFNCWebSocketSendToCallBack> _di_TTMSFNCWebSocketSendToCallBack;
class DELPHICLASS TTMSFNCCustomWebSocketServer;
class DELPHICLASS TTMSFNCWebSocketServer;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EWebsocketServer : public Fmx::Tmsfncwebsocketcommon::EWebSocket
{
	typedef Fmx::Tmsfncwebsocketcommon::EWebSocket inherited;
	
public:
	/* Exception.Create */ inline __fastcall EWebsocketServer(const System::UnicodeString Msg) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EWebsocketServer(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EWebsocketServer(NativeUInt Ident)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EWebsocketServer(System::PResStringRec ResStringRec)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebsocketServer(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebsocketServer(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EWebsocketServer(const System::UnicodeString Msg, int AHelpContext) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EWebsocketServer(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Fmx::Tmsfncwebsocketcommon::EWebSocket(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebsocketServer(NativeUInt Ident, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebsocketServer(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebsocketServer(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebsocketServer(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Fmx::Tmsfncwebsocketcommon::EWebSocket(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EWebsocketServer() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCWebSocketAllowConnectionEvent)(System::TObject* Sender, TTMSFNCWebSocketServerConnection* AConnection, bool &AAllow);

typedef void __fastcall (__closure *TTMSFNCWebSocketHandshakeSentEvent)(System::TObject* Sender, TTMSFNCWebSocketServerConnection* AConnection);

class PASCALIMPLEMENTATION TTMSFNCWebSocketServerConnection : public Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection
{
	typedef Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection inherited;
	
private:
	System::TObject* FUserData;
	bool FOwnsUserData;
	bool FHandshakeResponseSent;
	TTMSFNCWebSocketAllowConnectionEvent FOnAllow;
	Idcontext::TIdContext* FPeerThread;
	TTMSFNCWebSocketHandshakeSentEvent FOnHandshakeResponseSent;
	
protected:
	virtual Idtcpconnection::TIdTCPConnection* __fastcall GetConnection();
	virtual void __fastcall SaveRequest(Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo);
	virtual void __fastcall Handshake(Idcustomhttpserver::TIdHTTPRequestInfo* const ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* const AResponseInfo);
	virtual bool __fastcall CheckRequest(Idcustomhttpserver::TIdHTTPResponseInfo* const AResponseInfo);
	virtual System::UnicodeString __fastcall GetPeerIP();
	void __fastcall Receive(Idcustomhttpserver::TIdHTTPRequestInfo* const ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* const AResponseInfo);
	virtual bool __fastcall GetHandshakeCompleted();
	__property Idcontext::TIdContext* PeerThread = {read=FPeerThread};
	
public:
	__fastcall TTMSFNCWebSocketServerConnection(Idcontext::TIdContext* APeerThread, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions AOptions)/* overload */;
	__fastcall virtual ~TTMSFNCWebSocketServerConnection();
	__property bool HandshakeResponseSent = {read=FHandshakeResponseSent, nodefault};
	__property System::TObject* UserData = {read=FUserData, write=FUserData};
	__property bool OwnsUserData = {read=FOwnsUserData, write=FOwnsUserData, nodefault};
	__property TTMSFNCWebSocketAllowConnectionEvent OnAllow = {read=FOnAllow, write=FOnAllow};
	__property TTMSFNCWebSocketHandshakeSentEvent OnHandshakeResponseSent = {read=FOnHandshakeResponseSent, write=FOnHandshakeResponseSent};
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketConnections : public System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebSocketServer_TTMSFNCWebSocketServerConnection>.Create */ inline __fastcall TTMSFNCWebSocketConnections()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebSocketServer_TTMSFNCWebSocketServerConnection>.Create */ inline __fastcall TTMSFNCWebSocketConnections(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWebSocketServerConnection*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebSocketServer_TTMSFNCWebSocketServerConnection>.Create */ inline __fastcall TTMSFNCWebSocketConnections(System::Generics::Collections::TEnumerable__1<TTMSFNCWebSocketServerConnection*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebSocketServer_TTMSFNCWebSocketServerConnection>.Create */ inline __fastcall TTMSFNCWebSocketConnections(TTMSFNCWebSocketServerConnection* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCWebSocketServerConnection*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWebSocketServer_TTMSFNCWebSocketServerConnection>.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketConnections() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketHTTPServer : public Idhttpserver::TIdHTTPServer
{
	typedef Idhttpserver::TIdHTTPServer inherited;
	
public:
	/* TIdCustomHTTPServer.Create */ inline __fastcall TTMSFNCWebSocketHTTPServer(System::Classes::TComponent* AOwner)/* overload */ : Idhttpserver::TIdHTTPServer(AOwner) { }
	/* TIdCustomHTTPServer.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketHTTPServer() { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TTMSFNCWebSocketHTTPServer()/* overload */ : Idhttpserver::TIdHTTPServer() { }
	
};


__interface TTMSFNCWebSocketSendToCallBack  : public System::IInterface 
{
	virtual bool __fastcall Invoke(TTMSFNCWebSocketServerConnection* AConnection) = 0 ;
};

typedef void __fastcall (__closure *TTMSFNCWebSocketServerGetSSLPasswordEvent)(System::TObject* Sender, System::UnicodeString &APassword);

class PASCALIMPLEMENTATION TTMSFNCCustomWebSocketServer : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	int FPort;
	TTMSFNCWebSocketHTTPServer* FHTTPServer;
	Idsslopenssl::TIdServerIOHandlerSSLOpenSSL* FSSLHandler;
	Idscheduler::TIdScheduler* FThreadManager;
	TTMSFNCWebSocketConnections* FConnections;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnectEvent FOnConnect;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketMessageEvent FOnMessageReceived;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent FOnDisconnect;
	bool FUseSSL;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketBinDataEvent FOnBinaryDataReceived;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions FOptions;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnPing;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnPong;
	Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent FOnClose;
	int FWebSocketVersion;
	TTMSFNCWebSocketAllowConnectionEvent FOnAllow;
	System::UnicodeString FPathName;
	Idcustomhttpserver::TIdHTTPCommandEvent FOnCommandGet;
	Idcustomhttpserver::TIdHTTPCommandEvent FOnCommandOther;
	System::UnicodeString FRootCertificateFile;
	System::UnicodeString FCertificateKeyFile;
	System::UnicodeString FCertificateFile;
	TTMSFNCWebSocketServerGetSSLPasswordEvent FOnGetSSLPassword;
	bool FSplitMessage;
	unsigned __int64 FFrameSize;
	TTMSFNCWebSocketHandshakeSentEvent FOnHandshakeResponseSent;
	bool FServerActive;
	bool FAutoSyncEvents;
	bool __fastcall GetActive();
	void __fastcall SetActive(const bool Value);
	void __fastcall SetOptions(const Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions Value);
	void __fastcall SetOnCommandOther(const Idcustomhttpserver::TIdHTTPCommandEvent Value);
	void __fastcall SetCertificateFile(const System::UnicodeString Value);
	void __fastcall SetCertificateKeyFile(const System::UnicodeString Value);
	void __fastcall SetRootCertificateFile(const System::UnicodeString Value);
	void __fastcall SetUseSSL(const bool Value);
	void __fastcall SetFrameSize(const unsigned __int64 Value);
	void __fastcall SetSplitMessage(const bool Value);
	
protected:
	virtual void __fastcall Loaded();
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual Idsslopenssl::TIdServerIOHandlerSSLOpenSSL* __fastcall GetSSLHandler();
	virtual TTMSFNCWebSocketHTTPServer* __fastcall GetHTTPServer();
	virtual Idscheduler::TIdScheduler* __fastcall GetThreadManager();
	virtual TTMSFNCWebSocketConnections* __fastcall GetConnections();
	virtual TTMSFNCWebSocketServerConnection* __fastcall CreateWebsocketConnection(Idcontext::TIdContext* aContext, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions AOptions);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall HTTPServerQuerySSLPort(System::Word APort, bool &VUseSSL);
	virtual void __fastcall HTTPServerConnect(Idcontext::TIdContext* aContext);
	virtual void __fastcall HTTPServerDisconnect(Idcontext::TIdContext* aContext);
	virtual void __fastcall HTTPServerCommandGet(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall DoHTTPRequest(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall AllowConnection(System::TObject* Sender, TTMSFNCWebSocketServerConnection* AConnection, bool &aAllow);
	void __fastcall HandshakeResponseSent(System::TObject* Sender, TTMSFNCWebSocketServerConnection* AConnection);
	virtual void __fastcall MessageReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::UnicodeString AMessage);
	virtual void __fastcall BinaryDataReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> AData, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketFrameTypes FrameType);
	virtual void __fastcall PingReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> aData);
	virtual void __fastcall PongReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> aData);
	virtual void __fastcall CloseReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::DynamicArray<System::Byte> aData);
	bool __fastcall DoIOHandlerVerifyPeer(Idsslopenssl::TIdX509* Certificate, bool AOk, int ADepth, int AError);
	void __fastcall DoIOHandlerGetPassword(System::UnicodeString &Password);
	__property Idscheduler::TIdScheduler* ThreadManager = {read=GetThreadManager};
	__property TTMSFNCWebSocketConnections* Connections = {read=GetConnections};
	__property Idsslopenssl::TIdServerIOHandlerSSLOpenSSL* SSLHandler = {read=GetSSLHandler};
	__property TTMSFNCWebSocketHTTPServer* HTTPServer = {read=GetHTTPServer};
	__property bool AutoSyncEvents = {read=FAutoSyncEvents, write=FAutoSyncEvents, nodefault};
	__property int WebSocketVersion = {read=FWebSocketVersion, write=FWebSocketVersion, default=13};
	__property System::UnicodeString PathName = {read=FPathName, write=FPathName};
	__property bool Active = {read=GetActive, write=SetActive, default=0};
	__property System::UnicodeString CertificateFile = {read=FCertificateFile, write=SetCertificateFile};
	__property System::UnicodeString CertificateKeyFile = {read=FCertificateKeyFile, write=SetCertificateKeyFile};
	__property System::UnicodeString RootCertificateFile = {read=FRootCertificateFile, write=SetRootCertificateFile};
	__property bool UseSSL = {read=FUseSSL, write=SetUseSSL, default=0};
	__property int Port = {read=FPort, write=FPort, default=8888};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebsocketOptions Options = {read=FOptions, write=SetOptions, default=0};
	__property bool SplitMessage = {read=FSplitMessage, write=SetSplitMessage, default=0};
	__property unsigned __int64 FrameSize = {read=FFrameSize, write=SetFrameSize, default=0};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnectEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property TTMSFNCWebSocketAllowConnectionEvent OnAllow = {read=FOnAllow, write=FOnAllow};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketMessageEvent OnMessageReceived = {read=FOnMessageReceived, write=FOnMessageReceived};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketBinDataEvent OnBinaryDataReceived = {read=FOnBinaryDataReceived, write=FOnBinaryDataReceived};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketNotifyEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnPing = {read=FOnPing, write=FOnPing};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnPong = {read=FOnPong, write=FOnPong};
	__property Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketControlEvent OnClose = {read=FOnClose, write=FOnClose};
	__property Idcustomhttpserver::TIdHTTPCommandEvent OnCommandGet = {read=FOnCommandGet, write=FOnCommandGet};
	__property Idcustomhttpserver::TIdHTTPCommandEvent OnCommandOther = {read=FOnCommandOther, write=SetOnCommandOther};
	__property TTMSFNCWebSocketServerGetSSLPasswordEvent OnGetSSLPassword = {read=FOnGetSSLPassword, write=FOnGetSSLPassword};
	__property TTMSFNCWebSocketHandshakeSentEvent OnHandshakeResponseSent = {read=FOnHandshakeResponseSent, write=FOnHandshakeResponseSent};
	
public:
	__fastcall virtual TTMSFNCCustomWebSocketServer()/* overload */;
	__fastcall virtual TTMSFNCCustomWebSocketServer(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual TTMSFNCCustomWebSocketServer(System::Classes::TComponent* AOwner, int APort)/* overload */;
	__fastcall virtual ~TTMSFNCCustomWebSocketServer();
	virtual void __fastcall BroadcastMessage(System::UnicodeString AMessage);
	virtual void __fastcall BroadcastData(System::DynamicArray<System::Byte> AData);
	virtual void __fastcall SendMessageTo(System::UnicodeString AMessage, _di_TTMSFNCWebSocketSendToCallBack ASelector);
	virtual void __fastcall SendDataTo(System::DynamicArray<System::Byte> AData, _di_TTMSFNCWebSocketSendToCallBack ASelector);
	__classmethod System::Word __fastcall TransformCloseData(System::DynamicArray<System::Byte> ABytes, /* out */ System::UnicodeString &AReason);
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketServer : public TTMSFNCCustomWebSocketServer
{
	typedef TTMSFNCCustomWebSocketServer inherited;
	
__published:
	__property AutoSyncEvents;
	__property PathName = {default=0};
	__property SplitMessage = {default=0};
	__property FrameSize = {default=0};
	__property Active = {default=0};
	__property CertificateFile = {default=0};
	__property CertificateKeyfile = {default=0};
	__property RootCertificateFile = {default=0};
	__property UseSSL = {default=0};
	__property Port = {default=8888};
	__property Options = {default=0};
	__property OnConnect;
	__property OnAllow;
	__property OnMessageReceived;
	__property OnBinaryDataReceived;
	__property OnDisconnect;
	__property OnPing;
	__property OnPong;
	__property OnClose;
	__property OnHandshakeResponseSent;
	__property OnGetSSLPassword;
public:
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCWebSocketServer()/* overload */ : TTMSFNCCustomWebSocketServer() { }
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCWebSocketServer(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomWebSocketServer(AOwner) { }
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCWebSocketServer(System::Classes::TComponent* AOwner, int APort)/* overload */ : TTMSFNCCustomWebSocketServer(AOwner, APort) { }
	/* TTMSFNCCustomWebSocketServer.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketServer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncwebsocketserver */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWEBSOCKETSERVER)
using namespace Fmx::Tmsfncwebsocketserver;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwebsocketserverHPP
