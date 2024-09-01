// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server_API_SocketIO.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_server_api_socketioHPP
#define Sgcwebsocket_server_api_socketioHPP

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
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server_api_socketio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSSocketIO_Options;
class DELPHICLASS TsgcWSServer_API_SocketIO;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSOnServerSocketIOJSONMessage)(System::TObject* Sender, Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aHeader, const System::UnicodeString aJSON, const System::UnicodeString aRawMessage);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSocketIO_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FPingInterval;
	int FPingTimeout;
	
public:
	__fastcall TsgcWSSocketIO_Options();
	__fastcall virtual ~TsgcWSSocketIO_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int PingInterval = {read=FPingInterval, write=FPingInterval, nodefault};
	__property int PingTimeout = {read=FPingTimeout, write=FPingTimeout, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSServer_API_SocketIO : public Sgcwebsocket_classes::TsgcWSAPI_server
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_server inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual bool __fastcall DoHTTPRequestApi(Sgcwebsocket_classes::TsgcHTTPRequest* const aRequest, Sgcwebsocket_classes::TsgcHTTPResponse* const aResponse);
	
private:
	Sgcwebsocket_classes_syncobjs::TsgcThreadSafeStringList* FSessions;
	Sgcwebsocket_classes_syncobjs::TsgcThreadSafeStringList* __fastcall GetSessions();
	
protected:
	__property Sgcwebsocket_classes_syncobjs::TsgcThreadSafeStringList* Sessions = {read=GetSessions, write=FSessions};
	
private:
	TsgcWSSocketIO_Options* FSocketIO;
	void __fastcall SetSocketIO(TsgcWSSocketIO_Options* const Value);
	
public:
	__property TsgcWSSocketIO_Options* SocketIO = {read=FSocketIO, write=SetSocketIO};
	__fastcall virtual TsgcWSServer_API_SocketIO(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSServer_API_SocketIO();
	
protected:
	virtual void __fastcall DoReadJSONMessage(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aText);
	
private:
	TsgcWSOnServerSocketIOJSONMessage FOnSocketIOJSONMessage;
	
protected:
	virtual void __fastcall DoSocketIOJSONMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aHeader, const System::UnicodeString aJSON, const System::UnicodeString aRawMessage);
	__property TsgcWSOnServerSocketIOJSONMessage OnSocketIOJSONMessage = {read=FOnSocketIOJSONMessage, write=FOnSocketIOJSONMessage};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server_api_socketio */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER_API_SOCKETIO)
using namespace Sgcwebsocket_server_api_socketio;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_server_api_socketioHPP
