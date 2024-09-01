// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcP2P.pas' rev: 35.00 (Windows)

#ifndef Sgcp2pHPP
#define Sgcp2pHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcUDP_Client.hpp>
#include <sgcUDP_Server.hpp>
#include <sgcP2P_STUN_Client.hpp>
#include <sgcP2P_STUN_Server.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <IdGlobal.hpp>
#include <sgcWebSocket_Types.hpp>
#include <IdSocketHandle.hpp>
#include <sgcP2P_STUN_Types.hpp>
#include <sgcP2P_STUN_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcp2p
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcUDPCLient;
class DELPHICLASS TsgcUDPServer;
class DELPHICLASS TsgcSTUNClient;
class DELPHICLASS TsgcSTUNServer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcUDPCLient : public Sgcudp_client::TsgcUDPCLient_Base
{
	typedef Sgcudp_client::TsgcUDPCLient_Base inherited;
	
__published:
	__property Host = {default=0};
	__property Port;
	__property IPVersion;
	__property LogFile;
	__property Proxy;
	__property NotifyEvents;
	__property OnUDPRead;
	__property OnUDPException;
	__property Version = {default=0};
public:
	/* TsgcUDPCLient_Base.Create */ inline __fastcall virtual TsgcUDPCLient(System::Classes::TComponent* aOwner) : Sgcudp_client::TsgcUDPCLient_Base(aOwner) { }
	/* TsgcUDPCLient_Base.Destroy */ inline __fastcall virtual ~TsgcUDPCLient() { }
	
};


class PASCALIMPLEMENTATION TsgcUDPServer : public Sgcudp_server::TsgcUDPServer_Base
{
	typedef Sgcudp_server::TsgcUDPServer_Base inherited;
	
__published:
	__property Active;
	__property Host = {default=0};
	__property Port;
	__property IPVersion;
	__property Bindings;
	__property LogFile;
	__property NotifyEvents;
	__property WatchDog;
	__property OnStartup;
	__property OnShutdown;
	__property OnUDPRead;
	__property OnUDPException;
	__property OnBeforeWatchDog;
	__property Version = {default=0};
public:
	/* TsgcUDPServer_Base.Create */ inline __fastcall virtual TsgcUDPServer(System::Classes::TComponent* aOwner) : Sgcudp_server::TsgcUDPServer_Base(aOwner) { }
	/* TsgcUDPServer_Base.Destroy */ inline __fastcall virtual ~TsgcUDPServer() { }
	
};


class PASCALIMPLEMENTATION TsgcSTUNClient : public Sgcp2p_stun_client::TsgcSTUNClient_Base
{
	typedef Sgcp2p_stun_client::TsgcSTUNClient_Base inherited;
	
__published:
	__property Host = {default=0};
	__property Port;
	__property IPVersion;
	__property RetransmissionOptions;
	__property STUNOptions;
	__property Transport;
	__property LogFile;
	__property NotifyEvents;
	__property OnSTUNException;
	__property OnSTUNResponseError;
	__property OnSTUNResponseSuccess;
	__property OnSTUNBeforeSend;
	__property Version = {default=0};
public:
	/* TsgcSTUNClient_Base.Create */ inline __fastcall virtual TsgcSTUNClient(System::Classes::TComponent* aOwner) : Sgcp2p_stun_client::TsgcSTUNClient_Base(aOwner) { }
	/* TsgcSTUNClient_Base.Destroy */ inline __fastcall virtual ~TsgcSTUNClient() { }
	
};


class PASCALIMPLEMENTATION TsgcSTUNServer : public Sgcp2p_stun_server::TsgcSTUNServer_Base
{
	typedef Sgcp2p_stun_server::TsgcSTUNServer_Base inherited;
	
__published:
	__property Active;
	__property Host = {default=0};
	__property Port;
	__property IPVersion;
	__property STUNOptions;
	__property LogFile;
	__property NotifyEvents;
	__property OnSTUNException;
	__property OnSTUNRequestError;
	__property OnSTUNRequestSuccess;
	__property OnSTUNRequestAuthorization;
	__property Version = {default=0};
public:
	/* TsgcSTUNServer_Base.Create */ inline __fastcall virtual TsgcSTUNServer(System::Classes::TComponent* aOwner) : Sgcp2p_stun_server::TsgcSTUNServer_Base(aOwner) { }
	/* TsgcSTUNServer_Base.Destroy */ inline __fastcall virtual ~TsgcSTUNServer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcp2p */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCP2P)
using namespace Sgcp2p;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcp2pHPP
