// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Broker_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_broker_clientHPP
#define Sgcwebsocket_protocol_broker_clientHPP

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

namespace Sgcwebsocket_protocol_broker_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_Broker_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSProtocol_Broker_Client : public Sgcwebsocket_classes::TsgcWSProtocol_Client
{
	typedef Sgcwebsocket_classes::TsgcWSProtocol_Client inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	System::Contnrs::TObjectList* FProtocolObjectList;
	
protected:
	bool __fastcall ProtocolRegistered(const System::UnicodeString aProtocolName, Sgcwebsocket_classes::TsgcWSProtocol* aProtocol);
	System::UnicodeString __fastcall GetProtocols();
	
public:
	void __fastcall DoRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall DoUnRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall RegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	void __fastcall UnRegisterProtocol(Sgcwebsocket_classes::TsgcWSProtocol* aObject);
	
private:
	System::UnicodeString FWSBrokerMessageReadId;
	
protected:
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* __fastcall GetWSBrokerMessageReadByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	System::UnicodeString FWSBrokerMessageWriteId;
	Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* FWSBrokerMessageWrite;
	Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* __fastcall GetWSBrokerMessageWrite();
	
protected:
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* __fastcall GetWSBrokerMessageWriteByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__property Sgcwebsocket_protocol_broker_message::TsgcWSBrokerMessage* WSBrokerMessageWrite = {read=GetWSBrokerMessageWrite, write=FWSBrokerMessageWrite};
	
private:
	System::UnicodeString FWSBrokerBinaryReadId;
	
protected:
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* __fastcall GetWSBrokerBinaryReadByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	System::UnicodeString FWSBrokerBinaryWriteId;
	Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* FWSBrokerBinaryWrite;
	
protected:
	Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* __fastcall GetWSBrokerBinaryWrite();
	virtual Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* __fastcall GetWSBrokerBinaryWriteByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__property Sgcwebsocket_protocol_broker_message::TsgcWSBrokerBinary* WSBrokerBinaryWrite = {read=GetWSBrokerBinaryWrite, write=FWSBrokerBinaryWrite};
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Fin);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	
public:
	__fastcall virtual TsgcWSProtocol_Broker_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Broker_Client();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_broker_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BROKER_CLIENT)
using namespace Sgcwebsocket_protocol_broker_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_broker_clientHPP
