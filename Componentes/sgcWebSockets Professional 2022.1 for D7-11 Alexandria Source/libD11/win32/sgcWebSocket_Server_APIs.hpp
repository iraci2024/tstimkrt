// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server_APIs.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_server_apisHPP
#define Sgcwebsocket_server_apisHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <sgcWebSocket_Server_API_SocketIO.hpp>
#include <sgcWebSocket_Server_API_RTCMultiConnection.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server_apis
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSAPIServer_SocketIO;
class DELPHICLASS TsgcWSAPIServer_RTCMultiConnection;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSAPIServer_SocketIO : public Sgcwebsocket_server_api_socketio::TsgcWSServer_API_SocketIO
{
	typedef Sgcwebsocket_server_api_socketio::TsgcWSServer_API_SocketIO inherited;
	
__published:
	__property OnConnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property OnDisconnect;
	__property OnSocketIOJSONMessage;
	__property Server;
	__property SocketIO;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWSServer_API_SocketIO.Create */ inline __fastcall virtual TsgcWSAPIServer_SocketIO(System::Classes::TComponent* aOwner) : Sgcwebsocket_server_api_socketio::TsgcWSServer_API_SocketIO(aOwner) { }
	/* TsgcWSServer_API_SocketIO.Destroy */ inline __fastcall virtual ~TsgcWSAPIServer_SocketIO() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPIServer_RTCMultiConnection : public Sgcwebsocket_server_api_rtcmulticonnection::TsgcWSServer_API_RTCMultiConnection
{
	typedef Sgcwebsocket_server_api_rtcmulticonnection::TsgcWSServer_API_RTCMultiConnection inherited;
	
__published:
	__property Server;
	__property RTCMultiConnection;
	__property Version = {default=0};
public:
	/* TsgcWSServer_API_RTCMultiConnection.Create */ inline __fastcall virtual TsgcWSAPIServer_RTCMultiConnection(System::Classes::TComponent* aOwner) : Sgcwebsocket_server_api_rtcmulticonnection::TsgcWSServer_API_RTCMultiConnection(aOwner) { }
	/* TsgcWSServer_API_RTCMultiConnection.Destroy */ inline __fastcall virtual ~TsgcWSAPIServer_RTCMultiConnection() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server_apis */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER_APIS)
using namespace Sgcwebsocket_server_apis;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_server_apisHPP
