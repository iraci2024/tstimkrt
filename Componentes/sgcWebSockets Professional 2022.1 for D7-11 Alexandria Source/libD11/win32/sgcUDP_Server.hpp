// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcUDP_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcudp_serverHPP
#define Sgcudp_serverHPP

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
#include <IdGlobal.hpp>
#include <IdUDPServer.hpp>
#include <IdUDPBase.hpp>
#include <IdSocketHandle.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcUDP_Classes_Indy.hpp>
#include <sgcBase_Helpers.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcudp_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIdUDPServer;
class DELPHICLASS TsgcUDPWatchDogMonitorServer_Options;
class DELPHICLASS TsgcUDPWatchDogServer_Options;
class DELPHICLASS TsgcUDPSocketServer;
class DELPHICLASS TsgcUDPServer_Base;
class DELPHICLASS TUDPServerThread;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcUDPOnBeforeWatchDogEvent)(System::TObject* Sender, bool &Handled);

class PASCALIMPLEMENTATION TsgcIdUDPServer : public Idudpserver::TIdUDPServer
{
	typedef Idudpserver::TIdUDPServer inherited;
	
protected:
	virtual Idsockethandle::TIdSocketHandle* __fastcall GetSocketBinding(const System::UnicodeString aIPAddress, int aPort);
	
public:
	Idsockethandle::TIdSocketHandle* __fastcall AddBinding(const System::UnicodeString aIPAddress, int aPort, const System::UnicodeString aName = System::UnicodeString());
	bool __fastcall RemoveBinding(const System::UnicodeString aIPAddress, int aPort);
	
protected:
	virtual bool __fastcall GetActive();
public:
	/* TIdUDPServer.Destroy */ inline __fastcall virtual ~TsgcIdUDPServer() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcIdUDPServer(System::Classes::TComponent* AOwner)/* overload */ : Idudpserver::TIdUDPServer(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcIdUDPServer()/* overload */ : Idudpserver::TIdUDPServer() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPWatchDogMonitorServer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FTimeOut;
	
public:
	__fastcall virtual TsgcUDPWatchDogMonitorServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int TimeOut = {read=FTimeOut, write=FTimeOut, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcUDPWatchDogMonitorServer_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcUDPWatchDogServer_Options : public Sgcudp_classes::TsgcUDPWatchDog_Options
{
	typedef Sgcudp_classes::TsgcUDPWatchDog_Options inherited;
	
private:
	TsgcUDPWatchDogMonitorServer_Options* FMonitor;
	void __fastcall SetMonitor(TsgcUDPWatchDogMonitorServer_Options* const Value);
	
public:
	__fastcall virtual TsgcUDPWatchDogServer_Options();
	__fastcall virtual ~TsgcUDPWatchDogServer_Options();
	
__published:
	__property TsgcUDPWatchDogMonitorServer_Options* Monitor = {read=FMonitor, write=SetMonitor};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcUDPSocketServer : public Sgcudp_classes_indy::TsgcUDPSocket_Indy
{
	typedef Sgcudp_classes_indy::TsgcUDPSocket_Indy inherited;
	
public:
	virtual void __fastcall Assign(Sgcudp_classes::TsgcUDPSocket* const aSource);
	
private:
	TsgcUDPServer_Base* FServer;
	
protected:
	virtual void __fastcall DoInitialize(TsgcUDPServer_Base* const aServer, const System::UnicodeString aPeerIp, int aPeerPort, System::UnicodeString aLocalIp, int aLocalPort, Sgcwebsocket_types::TwsIPVersion aIPVersion);
	virtual void __fastcall DoWriteData(const System::UnicodeString aValue)/* overload */;
	virtual void __fastcall DoWriteData(const System::DynamicArray<System::Byte> aBytes)/* overload */;
public:
	/* TsgcUDPSocket_Indy.Destroy */ inline __fastcall virtual ~TsgcUDPSocketServer() { }
	
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcUDPSocketServer() : Sgcudp_classes_indy::TsgcUDPSocket_Indy() { }
	
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::UnicodeString aValue){ Sgcudp_classes::TsgcUDPSocket_Base::DoWriteData(aIPAddress, aPort, aValue); }
	inline void __fastcall  DoWriteData(const System::UnicodeString aIPAddress, System::Word aPort, const System::DynamicArray<System::Byte> aValue){ Sgcudp_classes::TsgcUDPSocket_Base::DoWriteData(aIPAddress, aPort, aValue); }
	
};


class PASCALIMPLEMENTATION TsgcUDPServer_Base : public Sgcudp_classes::TsgcUDPComponent
{
	typedef Sgcudp_classes::TsgcUDPComponent inherited;
	
protected:
	virtual void __fastcall Loaded();
	
private:
	TsgcIdUDPServer* FUDPServer;
	TsgcIdUDPServer* __fastcall GetUDPServer();
	
protected:
	__property TsgcIdUDPServer* UDPServer = {read=GetUDPServer, write=FUDPServer};
	
public:
	Idsockethandle::TIdSocketHandle* __fastcall AddBinding(const System::UnicodeString aIPAddress, int aPort, const System::UnicodeString aName = System::UnicodeString());
	bool __fastcall RemoveBinding(const System::UnicodeString aIPAddress, int aPort);
	Idsockethandle::TIdSocketHandle* __fastcall GetBinding(const System::UnicodeString aIPAddress, int aPort);
	
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
	virtual TsgcUDPSocketServer* __fastcall DoCreateUDPSocket(Idsockethandle::TIdSocketHandle* const ABinding);
	virtual void __fastcall OnClientReadEvent(Idudpserver::TIdUDPListenerThread* AThread, const Idglobal::TIdBytes AData, Idsockethandle::TIdSocketHandle* ABinding);
	virtual void __fastcall OnClientExceptionEvent(Idudpserver::TIdUDPListenerThread* AThread, Idsockethandle::TIdSocketHandle* ABinding, const System::UnicodeString AMessage, const System::TClass AExceptionClass);
	virtual void __fastcall OnSocketExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* const E);
	
private:
	Sgcudp_classes::TsgcUDPLogFile* FLogFile;
	Sgcudp_classes::TsgcIdUDPLogFile* FIdLogFile;
	
protected:
	Sgcudp_classes::TsgcIdUDPLogFile* __fastcall GetIdLogFile();
	void __fastcall SetLogFile(Sgcudp_classes::TsgcUDPLogFile* const Value);
	
public:
	__property Sgcudp_classes::TsgcUDPLogFile* LogFile = {read=FLogFile, write=SetLogFile};
	
private:
	TsgcUDPWatchDogServer_Options* FWatchDog;
	System::UnicodeString FWatchDogMonitorSecret;
	unsigned FWatchDogMonitorStart;
	bool FWatchDogMonitorWaiting;
	TsgcUDPOnBeforeWatchDogEvent FOnBeforeWatchDog;
	
protected:
	int FWatchDogAttempts;
	Sgcbase_classes::TsgcTimer* FWatchDogTimer;
	bool FWatchDogEnabled;
	virtual void __fastcall DoStartWatchDog();
	virtual void __fastcall DoStopWatchDog();
	virtual void __fastcall DoWatchDogMonitor();
	virtual bool __fastcall DoWatchDogMonitorRead(const System::DynamicArray<System::Byte> Bytes);
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall SetWatchDog(TsgcUDPWatchDogServer_Options* const Value);
	
public:
	__property TsgcUDPWatchDogServer_Options* WatchDog = {read=FWatchDog, write=SetWatchDog};
	__property TsgcUDPOnBeforeWatchDogEvent OnBeforeWatchDog = {read=FOnBeforeWatchDog, write=FOnBeforeWatchDog};
	__fastcall virtual TsgcUDPServer_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcUDPServer_Base();
	
private:
	System::UnicodeString FHost;
	int FPort;
	bool FTLS;
	Sgcudp_classes::TsgcUDPTLS_Options* FTLSOptions;
	Idsockethandle::TIdSocketHandles* __fastcall GetBindings();
	void __fastcall SetBindings(Idsockethandle::TIdSocketHandles* const Value);
	
protected:
	virtual void __fastcall SetTLSOptions(Sgcudp_classes::TsgcUDPTLS_Options* const Value);
	
public:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property Idsockethandle::TIdSocketHandles* Bindings = {read=GetBindings, write=SetBindings};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	__property Sgcudp_classes::TsgcUDPTLS_Options* TLSOptions = {read=FTLSOptions, write=SetTLSOptions};
	void __fastcall Start();
	void __fastcall Stop();
	void __fastcall ReStart();
	
private:
	bool FUseNagle;
	Idglobal::TIdIPVersion FIPVersion;
	int FConnectTimeout;
	int FReadTimeout;
	int FWriteTimeout;
	
public:
	__property bool UseNagle = {read=FUseNagle, write=FUseNagle, stored=true, nodefault};
	__property Idglobal::TIdIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__property int ConnectTimeout = {read=FConnectTimeout, write=FConnectTimeout, nodefault};
	__property int WriteTimeout = {read=FWriteTimeout, write=FWriteTimeout, nodefault};
	__property int ReadTimeout = {read=FReadTimeout, write=FReadTimeout, nodefault};
	
private:
	bool FActive;
	
protected:
	virtual bool __fastcall GetActive();
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aIP, System::Word aPort, const System::DynamicArray<System::Byte> aBytes, const System::UnicodeString aSourceIP = System::UnicodeString(), System::Word aSourcePort = (System::Word)(0x0));
	
public:
	void __fastcall WriteData(const System::UnicodeString aIP, System::Word aPort, const System::UnicodeString aValue)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aIP, System::Word aPort, const System::DynamicArray<System::Byte> aBytes)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aSourceIP, const System::Word aSourcePort, const System::UnicodeString aIP, System::Word aPort, const System::UnicodeString aValue)/* overload */;
	void __fastcall WriteData(const System::UnicodeString aSourceIP, const System::Word aSourcePort, const System::UnicodeString aIP, System::Word aPort, const System::DynamicArray<System::Byte> aBytes)/* overload */;
	
private:
	System::Classes::TNotifyEvent FOnShutdown;
	System::Classes::TNotifyEvent FOnStartup;
	
protected:
	virtual void __fastcall DoStartupEvent();
	virtual void __fastcall DoShutdownEvent();
	__property System::Classes::TNotifyEvent OnStartup = {read=FOnStartup, write=FOnStartup};
	__property System::Classes::TNotifyEvent OnShutdown = {read=FOnShutdown, write=FOnShutdown};
};


class PASCALIMPLEMENTATION TUDPServerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcUDPServer_Base* FServer;
	System::UnicodeString FMethod;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TUDPServerThread(const System::UnicodeString aMethod, TsgcUDPServer_Base* const aServer);
	__classmethod void __fastcall Start(TsgcUDPServer_Base* const aServer);
	__classmethod void __fastcall Stop(TsgcUDPServer_Base* const aServer);
	__classmethod void __fastcall ReStart(TsgcUDPServer_Base* const aServer);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TUDPServerThread() { }
	
};


typedef System::TMetaClass* TsgcUDPSocketServerClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcudp_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCUDP_SERVER)
using namespace Sgcudp_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcudp_serverHPP
