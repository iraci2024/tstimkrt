// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Client_WinHTTP.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_client_winhttpHPP
#define Sgcwebsocket_client_winhttpHPP

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
#include <System.StrUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_WinAPI.hpp>
#include <sgcWebSocket_Classes_WinHTTP.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Extensions.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_client_winhttp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSReadThread;
class DELPHICLASS TsgcWSOptionsClient;
class DELPHICLASS TsgcWSAuthenticationClient_Basic;
class DELPHICLASS TsgcWSAuthenticationClient_Options;
class DELPHICLASS TsgcWSProxy_Options;
class DELPHICLASS TsgcWSConnectionClient_WinHTTP;
class DELPHICLASS TClientWinhttpThread;
class DELPHICLASS TsgcWSClient_WinHTTP;
class DELPHICLASS TsgcWSAsyncClients;
class DELPHICLASS TsgcWSAsyncClient;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Byte> _dt_Sgcwebsocket_client_winhttp_1;
typedef void __fastcall (__closure *TOnReadMessageEvent)(System::TObject* Sender, _dt_Sgcwebsocket_client_winhttp_1 Buffer, Sgcwebsocket_winapi::WINHTTP_WEB_SOCKET_BUFFER_TYPE BufferType, unsigned BufferSize);

class PASCALIMPLEMENTATION TsgcWSReadThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	unsigned FBufferSize;
	void *FSession;
	unsigned __fastcall GetBufferSize();
	
protected:
	virtual void __fastcall Execute();
	
public:
	__property void * Session = {read=FSession, write=FSession};
	
private:
	TOnReadMessageEvent FOnReadMessage;
	System::Classes::TNotifyEvent FOnDisconnect;
	
public:
	__property unsigned BufferSize = {read=GetBufferSize, write=FBufferSize, nodefault};
	__property TOnReadMessageEvent OnReadMessage = {read=FOnReadMessage, write=FOnReadMessage};
	__property System::Classes::TNotifyEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
public:
	/* TThread.Create */ inline __fastcall TsgcWSReadThread()/* overload */ : System::Classes::TThread() { }
	/* TThread.Create */ inline __fastcall TsgcWSReadThread(bool CreateSuspended)/* overload */ : System::Classes::TThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TsgcWSReadThread(bool CreateSuspended, NativeUInt ReservedStackSize)/* overload */ : System::Classes::TThread(CreateSuspended, ReservedStackSize) { }
	/* TThread.Destroy */ inline __fastcall virtual ~TsgcWSReadThread() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSOptionsClient : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsFragmentedMessages FFragmentedMessages;
	System::UnicodeString FOrigin;
	System::UnicodeString FParameters;
	System::UnicodeString __fastcall GetParameters();
	
public:
	__fastcall virtual TsgcWSOptionsClient();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TwsFragmentedMessages FragmentedMessages = {read=FFragmentedMessages, write=FFragmentedMessages, nodefault};
	__property System::UnicodeString Origin = {read=FOrigin, write=FOrigin};
	__property System::UnicodeString Parameters = {read=GetParameters, write=FParameters};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSOptionsClient() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Basic : public Sgcwebsocket_classes::TsgcWSAuthentication_Basic
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Basic inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationClient_Basic() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationClient_Basic() : Sgcwebsocket_classes::TsgcWSAuthentication_Basic() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Options : public Sgcwebsocket_classes::TsgcWSAuthentication_Options
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Options inherited;
	
private:
	System::UnicodeString FPassword;
	TsgcWSAuthenticationClient_Basic* FBasic;
	System::UnicodeString FUser;
	
protected:
	void __fastcall SetBasic(TsgcWSAuthenticationClient_Basic* const Value);
	
public:
	__fastcall virtual TsgcWSAuthenticationClient_Options();
	__fastcall virtual ~TsgcWSAuthenticationClient_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property TsgcWSAuthenticationClient_Basic* Basic = {read=FBasic, write=SetBasic};
	__property System::UnicodeString User = {read=FUser, write=FUser};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProxy_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FHost;
	System::UnicodeString FPassword;
	int FPort;
	System::UnicodeString FUsername;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSProxy_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSProxy_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSConnectionClient_WinHTTP : public Sgcwebsocket_classes_winhttp::TsgcWSConnection_WinHTTP
{
	typedef Sgcwebsocket_classes_winhttp::TsgcWSConnection_WinHTTP inherited;
	
private:
	TsgcWSAuthenticationClient_Options* FAuthentication;
	TsgcWSOptionsClient* FOptions;
	
protected:
	void __fastcall SetAuthentication(TsgcWSAuthenticationClient_Options* const Value);
	void __fastcall SetOptions(TsgcWSOptionsClient* const Value);
	__property TsgcWSAuthenticationClient_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	__property TsgcWSOptionsClient* Options = {read=FOptions, write=SetOptions};
	
public:
	__fastcall virtual TsgcWSConnectionClient_WinHTTP();
	__fastcall virtual ~TsgcWSConnectionClient_WinHTTP();
};


class PASCALIMPLEMENTATION TClientWinhttpThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcWSClient_WinHTTP* FClient;
	System::UnicodeString FMethod;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TClientWinhttpThread(const System::UnicodeString aMethod, TsgcWSClient_WinHTTP* const aClient);
	__classmethod void __fastcall Stop(TsgcWSClient_WinHTTP* const aClient);
	__classmethod void __fastcall Start(TsgcWSClient_WinHTTP* const aClient);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TClientWinhttpThread() { }
	
};


class PASCALIMPLEMENTATION TsgcWSClient_WinHTTP : public Sgcwebsocket_classes_winhttp::TsgcWSComponent_Client_WinHTTP
{
	typedef Sgcwebsocket_classes_winhttp::TsgcWSComponent_Client_WinHTTP inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall OnHeartBeatTimeoutEvent(System::TObject* Sender);
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatTimeoutExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender);
	virtual Sgcwebsocket_classes::TsgcWSHeartBeat_Options* __fastcall GetHeartBeat();
	virtual Sgcwebsocket_extensions::TsgcWSExtensions* __fastcall GetExtensions();
	virtual Sgcwebsocket_classes::TsgcWSSpecifications* __fastcall GetSpecifications();
	virtual Sgcwebsocket_classes::TsgcWSLogFile* __fastcall GetLogFile();
	
public:
	virtual void __fastcall WriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual void __fastcall WriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual System::UnicodeString __fastcall WriteAndWaitData(const System::UnicodeString aText, const int aTimeout = 0x2710);
	
protected:
	virtual void __fastcall SetURL(const System::UnicodeString Value);
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeWatchDogEvent FOnBeforeWatchDog;
	void __fastcall DoStart();
	
protected:
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeWatchDogEvent OnBeforeWatchDog = {read=FOnBeforeWatchDog, write=FOnBeforeWatchDog};
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeConnectEvent FOnBeforeConnect;
	
protected:
	virtual bool __fastcall GetActive();
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeConnectEvent OnBeforeConnect = {read=FOnBeforeConnect, write=FOnBeforeConnect};
	
private:
	TsgcWSConnectionClient_WinHTTP* FWSConnection;
	bool FDisconnecting;
	System::DynamicArray<System::Byte> FAsyncBuffer;
	int FConnectTimeout;
	int FReadTimeout;
	bool FActive;
	bool FAsynchronous;
	int FReceiveBufferSize;
	TsgcWSOptionsClient* FOptions;
	bool FFragmentedUTF8;
	bool FFragmentedBinary;
	System::DynamicArray<System::Byte> FFragmentedUTF8Bytes;
	System::DynamicArray<System::Byte> FFragmentedBinaryBytes;
	
protected:
	virtual void __fastcall SetOptions(TsgcWSOptionsClient* const Value);
	virtual void __fastcall SetAsynchronous(const bool Value);
	
public:
	__property bool Asynchronous = {read=FAsynchronous, write=SetAsynchronous, nodefault};
	__property int ConnectTimeout = {read=FConnectTimeout, write=FConnectTimeout, nodefault};
	__property TsgcWSOptionsClient* Options = {read=FOptions, write=SetOptions};
	__property int ReadTimeout = {read=FReadTimeout, write=FReadTimeout, nodefault};
	__property int ReceiveBufferSize = {read=FReceiveBufferSize, write=FReceiveBufferSize, nodefault};
	
private:
	bool FThreadStart;
	
public:
	void __fastcall Start();
	void __fastcall Stop();
	
private:
	bool FConnecting;
	
public:
	bool __fastcall Connect(const int aTimeout = 0x2710);
	bool __fastcall Disconnect(const int aTimeout = 0x2710);
	
private:
	TsgcWSAuthenticationClient_Options* FAuthentication;
	void __fastcall SetAuthentication(TsgcWSAuthenticationClient_Options* const Value);
	
public:
	__property TsgcWSAuthenticationClient_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	TsgcWSProxy_Options* FProxy;
	
protected:
	virtual void __fastcall SetProxy(TsgcWSProxy_Options* const Value);
	
public:
	__property TsgcWSProxy_Options* Proxy = {read=FProxy, write=SetProxy};
	
private:
	void *FHTTPSession;
	void *FWSSession;
	void *FHTTPHandle;
	void *FHTTPRequest;
	
public:
	__fastcall virtual TsgcWSClient_WinHTTP(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSClient_WinHTTP();
	
private:
	TsgcWSReadThread* FReadThread;
	void __fastcall DoReadMessageEvent(System::DynamicArray<System::Byte> Buffer, Sgcwebsocket_winapi::WINHTTP_WEB_SOCKET_BUFFER_TYPE BufferType, unsigned BufferSize);
	void __fastcall DoDisconnectEvent();
	
protected:
	virtual void __fastcall OnReadMessageEvent(System::TObject* Sender, System::DynamicArray<System::Byte> Buffer, Sgcwebsocket_winapi::WINHTTP_WEB_SOCKET_BUFFER_TYPE BufferType, unsigned BufferSize);
	virtual void __fastcall OnDisconnectEvent(System::TObject* Sender);
	virtual void __fastcall DoReadStart();
	virtual void __fastcall DoReadStop();
	
private:
	void __fastcall DoWriteFragmentedUTF8(const System::DynamicArray<System::Byte> aBuffer);
	void __fastcall DoWriteFragmentedBinary(const System::DynamicArray<System::Byte> aBuffer);
	void __fastcall DoClear();
	void __fastcall DoInitWatchDog();
	void __fastcall DoFinWatchDog();
	bool __fastcall DoValidateServerCertificate();
	void __fastcall DoQueryCloseStatus();
	void __fastcall DoCreateConnection();
	void __fastcall DoHttpOpen();
	void __fastcall DoAsyncStatusCallback();
	void __fastcall DoHttpConnect();
	void __fastcall DoHttpSecure();
	void __fastcall DoHttpTimeout();
	void __fastcall DoRequestUpgrade();
	void __fastcall DoDisableRedirects();
	void __fastcall DoDisableCookies();
	void __fastcall DoSetProxy();
	void __fastcall DoCustomHeaders();
	void __fastcall DoRequestGET();
	void __fastcall DoResponseGET();
	void __fastcall DoDisableTLSValidations();
	bool __fastcall DoAsyncCompleteUpgrade();
	void __fastcall DoCompleteUpgrade();
	void __fastcall DoSetProtocol();
	void __fastcall DoSetHeartBeat();
	void __fastcall DoAsyncWebSocketReceive();
	void __fastcall DoAsyncStatusReadComplete(const Sgcwebsocket_winapi::WINHTTP_WEB_SOCKET_BUFFER_TYPE aBufferType, const unsigned aBufferSize);
	void __fastcall DoEventNotifyConnect();
	void __fastcall DoEventNotifyDisconnect();
	void __fastcall DoEventNotifyError(const Sgcwebsocket_winapi::WINHTTP_ASYNC_RESULT &AsyncResult, Sgcwebsocket_winapi::WINHTTP_WEB_SOCKET_OPERATION Operation);
	void __fastcall DoSyncConnect();
	HIDESBASE void __fastcall DoAsyncConnect();
	
protected:
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall DoConnect();
	virtual void __fastcall DoDisconnect();
	
public:
	void __fastcall Ping();
	bool __fastcall CheckPlatform();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAsyncClients : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	void __fastcall AddClient(TsgcWSClient_WinHTTP* const aClient);
	void __fastcall DeleteClient(TsgcWSClient_WinHTTP* const aClient);
	TsgcWSClient_WinHTTP* __fastcall GetClientByHINTERNET(void * aHINTERNET);
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcWSAsyncClients() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcWSAsyncClients() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAsyncClient : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcWSClient_WinHTTP* FClient;
	
public:
	__fastcall virtual TsgcWSAsyncClient();
	__fastcall virtual ~TsgcWSAsyncClient();
	__property TsgcWSClient_WinHTTP* Client = {read=FClient, write=FClient};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_client_winhttp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLIENT_WINHTTP)
using namespace Sgcwebsocket_client_winhttp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_client_winhttpHPP
