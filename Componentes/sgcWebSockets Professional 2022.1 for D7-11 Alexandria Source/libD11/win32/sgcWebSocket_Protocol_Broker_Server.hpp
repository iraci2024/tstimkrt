// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Broker_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_broker_serverHPP
#define Sgcwebsocket_protocol_broker_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Broker_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_broker_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_Broker_Server;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSProtocol_Broker_Server : public Sgcwebsocket_classes::TsgcWSProtocol_Server
{
	typedef Sgcwebsocket_classes::TsgcWSProtocol_Server inherited;
	
private:
	System::Contnrs::TObjectList* FProtocolObjectList;
	
protected:
	bool __fastcall ProtocolRegistered(System::UnicodeString aProtocolName, Sgcwebsocket_classes::TsgcWSProtocol* aProtocol);
	System::UnicodeString __fastcall GetProtocols();
	
public:
	void __fastcall DoRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall DoUnRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall RegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall UnRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	
private:
	System::UnicodeString FWSBrokerMessageId;
	
protected:
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* __fastcall GetWSBrokerMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	System::UnicodeString FWSBrokerBinaryId;
	
protected:
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* __fastcall GetWSBrokerBinaryByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Fin);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	
public:
	__fastcall virtual TsgcWSProtocol_Broker_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Broker_Server();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_broker_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BROKER_SERVER)
using namespace Sgcwebsocket_protocol_broker_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_broker_serverHPP
