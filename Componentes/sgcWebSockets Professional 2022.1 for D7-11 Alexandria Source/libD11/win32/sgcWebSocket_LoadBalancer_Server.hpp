// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_LoadBalancer_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_loadbalancer_serverHPP
#define Sgcwebsocket_loadbalancer_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>
#include <IdContext.hpp>
#include <sgcWebSocket_Server.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_LoadBalancer_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_loadbalancer_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcThreadListBindings;
class DELPHICLASS TsgcThreadListClientList;
class DELPHICLASS TsgcThreadListServerList;
class DELPHICLASS TsgcWSLoadBalancer_Options;
class DELPHICLASS TsgcWSLBServerConnection;
class DELPHICLASS TsgcWSLoadBalancerServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSLBServerConnectEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection);

typedef void __fastcall (__closure *TsgcWSLBServerDisconnectEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);

typedef void __fastcall (__closure *TsgcWSLBClientConnectionEvent)(Sgcwebsocket_classes::TsgcWSConnection* ServerConnection, Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerClientConnection* ClientConnection);

typedef void __fastcall (__closure *TsgcWSLBServerMessageEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Text, bool &Handled);

typedef void __fastcall (__closure *TsgcWSLBServerBinaryEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Classes::TMemoryStream* Data, bool &Handled);

typedef void __fastcall (__closure *TsgcWSLBBeforeSendServerHostEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerServerBinding* &Binding);

typedef void __fastcall (__closure *TsgcWSLBServerReadyEvent)(TsgcWSLBServerConnection* Server);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListBindings : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListBindings() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListBindings() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListClientList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListClientList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListClientList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListServerList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListServerList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListServerList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSLoadBalancer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsLoadBalancing FLoadBalancing;
	
public:
	__fastcall virtual TsgcWSLoadBalancer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TwsLoadBalancing LoadBalancing = {read=FLoadBalancing, write=FLoadBalancing, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancer_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSLBServerConnection : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TsgcThreadListBindings* FBindings;
	TsgcThreadListClientList* FClientList;
	bool FReady;
	Sgcwebsocket_classes::TsgcWSConnection* FConnection;
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerServerData* FData;
	TsgcThreadListBindings* __fastcall GetBindings();
	TsgcThreadListClientList* __fastcall GetClientList();
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerServerData* __fastcall GetData();
	
public:
	__fastcall virtual TsgcWSLBServerConnection(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSLBServerConnection();
	void __fastcall AddClient(Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerClientConnection* const aConnection);
	void __fastcall DeleteClient(Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerClientConnection* const aConnection);
	void __fastcall AddBinding(Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerServerBinding* const aBinding);
	__property TsgcThreadListBindings* Bindings = {read=GetBindings, write=FBindings};
	__property TsgcThreadListClientList* ClientList = {read=GetClientList, write=FClientList};
	__property bool Ready = {read=FReady, write=FReady, nodefault};
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=FConnection, write=FConnection};
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerServerData* Data = {read=GetData, write=FData};
};


class PASCALIMPLEMENTATION TsgcWSLoadBalancerServer : public Sgcwebsocket_server::TsgcWSServer
{
	typedef Sgcwebsocket_server::TsgcWSServer inherited;
	
private:
	TsgcThreadListServerList* FServerList;
	
protected:
	TsgcThreadListServerList* __fastcall GetServerList();
	virtual void __fastcall DoAddConnectionToServersList(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoDeleteConnectionFromServersList(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	TsgcWSLBServerConnection* __fastcall GetServerByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoProcessServerData(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoProcessClientConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoProcessServerBinding(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoProcessServerReady(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	
public:
	__property TsgcThreadListServerList* ServerList = {read=GetServerList, write=FServerList};
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Fin);
	
private:
	TsgcWSLoadBalancer_Options* FLoadBalancer;
	
protected:
	HIDESBASE void __fastcall SetLoadBalancer(TsgcWSLoadBalancer_Options* const Value);
	
public:
	__property TsgcWSLoadBalancer_Options* LoadBalancer = {read=FLoadBalancer, write=SetLoadBalancer};
	
private:
	int __fastcall GetServerLessConnections(System::Classes::TList* const aList);
	System::UnicodeString __fastcall GetServerBindingText();
	
protected:
	virtual bool __fastcall DoSendServerBinding(Sgcwebsocket_server::TsgcWSConnectionServer* const aConnection);
	virtual bool __fastcall DoBuiltInLibraries(Sgcwebsocket_server::TsgcWSConnectionServer* aConnection, const System::UnicodeString aText, bool aDisconnect, int aStreamIdentifier = 0x0);
	
private:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* FMessageReadText;
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* FMessageWriteText;
	
protected:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* __fastcall GetMessageReadText();
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* __fastcall GetMessageWriteText();
	
public:
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* MessageReadText = {read=GetMessageReadText, write=FMessageReadText};
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* MessageWriteText = {read=GetMessageWriteText, write=FMessageWriteText};
	
private:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* FMessageReadBinary;
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* FMessageWriteBinary;
	
protected:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* __fastcall GetMessageReadBinary();
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* __fastcall GetMessageWriteBinary();
	
public:
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* MessageReadBinary = {read=GetMessageReadBinary, write=FMessageReadBinary};
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* MessageWriteBinary = {read=GetMessageWriteBinary, write=FMessageWriteBinary};
	__fastcall virtual TsgcWSLoadBalancerServer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSLoadBalancerServer();
	
private:
	Sgcwebsocket_classes::TsgcWSRawMessageEvent FOnRawMessage;
	TsgcWSLBServerBinaryEvent FOnClientBinary;
	TsgcWSLBClientConnectionEvent FOnClientConnect;
	TsgcWSLBClientConnectionEvent FOnClientDisconnect;
	TsgcWSLBServerBinaryEvent FOnClientFragmented;
	TsgcWSLBServerMessageEvent FOnClientMessage;
	TsgcWSLBServerConnectEvent FOnServerConnect;
	TsgcWSLBServerDisconnectEvent FOnServerDisconnect;
	TsgcWSLBServerReadyEvent FOnServerReady;
	TsgcWSLBBeforeSendServerHostEvent FOnBeforeSendServerBinding;
	
public:
	__property Sgcwebsocket_classes::TsgcWSRawMessageEvent OnRawMessage = {read=FOnRawMessage, write=FOnRawMessage};
	__property TsgcWSLBClientConnectionEvent OnClientConnect = {read=FOnClientConnect, write=FOnClientConnect};
	__property TsgcWSLBClientConnectionEvent OnClientDisconnect = {read=FOnClientDisconnect, write=FOnClientDisconnect};
	__property TsgcWSLBServerBinaryEvent OnClientBinary = {read=FOnClientBinary, write=FOnClientBinary};
	__property TsgcWSLBServerBinaryEvent OnClientFragmented = {read=FOnClientFragmented, write=FOnClientFragmented};
	__property TsgcWSLBServerMessageEvent OnClientMessage = {read=FOnClientMessage, write=FOnClientMessage};
	__property TsgcWSLBServerConnectEvent OnServerConnect = {read=FOnServerConnect, write=FOnServerConnect};
	__property TsgcWSLBServerDisconnectEvent OnServerDisconnect = {read=FOnServerDisconnect, write=FOnServerDisconnect};
	__property TsgcWSLBServerReadyEvent OnServerReady = {read=FOnServerReady, write=FOnServerReady};
	__property TsgcWSLBBeforeSendServerHostEvent OnBeforeSendServerBinding = {read=FOnBeforeSendServerBinding, write=FOnBeforeSendServerBinding};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_loadbalancer_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_LOADBALANCER_SERVER)
using namespace Sgcwebsocket_loadbalancer_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_loadbalancer_serverHPP
