// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcLib_RCON_Client.pas' rev: 35.00 (Windows)

#ifndef Sgclib_rcon_clientHPP
#define Sgclib_rcon_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcTCP_Client.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgclib_rcon_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcRCON_Options;
class DELPHICLASS TsgcRCON_Packet;
class DELPHICLASS TsgcLib_RCON_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcRCONAuthenticateEvent)(System::TObject* Sender, bool Authenticated, TsgcRCON_Packet* const aPacket);

typedef void __fastcall (__closure *TsgcRCONResponseEvent)(System::TObject* Sender, const System::UnicodeString aResponse, TsgcRCON_Packet* const aPacket);

typedef void __fastcall (__closure *TsgcRCONExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcRCON_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHost;
	System::UnicodeString FPassword;
	int FPort;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property int Port = {read=FPort, write=FPort, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcRCON_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcRCON_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcRCON_Packet : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FBody;
	int FID;
	int FPacketSize;
	int FPacketType;
	System::WideChar __fastcall GetEmptyString();
	void __fastcall DoAddBytes(System::DynamicArray<System::Byte> Value, System::DynamicArray<System::Byte> &Bytes);
	void __fastcall DoAddByte(System::Byte Value, System::DynamicArray<System::Byte> &Bytes);
	void __fastcall DoAddInteger(int Value, System::DynamicArray<System::Byte> &Bytes);
	int __fastcall DoReadIntegerFromBytes(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
	System::UnicodeString __fastcall DoReadStringFromBytes(const System::DynamicArray<System::Byte> aBytes, int aLength, int &aOffset);
	
public:
	System::DynamicArray<System::Byte> __fastcall WriteBytes();
	void __fastcall ReadBytes(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
	__property System::UnicodeString Body = {read=FBody, write=FBody};
	__property int ID = {read=FID, write=FID, nodefault};
	__property int PacketSize = {read=FPacketSize, nodefault};
	__property int PacketType = {read=FPacketType, write=FPacketType, nodefault};
	__property System::WideChar EmptyString = {read=GetEmptyString, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcRCON_Packet() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcRCON_Packet() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcLib_RCON_Client : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
private:
	TsgcRCON_Options* FRCON_Options;
	void __fastcall SetRCON_Options(TsgcRCON_Options* const Value);
	Sgcwebsocket_types::TwsNotifyEvent __fastcall GetNotifyEvents();
	void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	
public:
	__property TsgcRCON_Options* RCON_Options = {read=FRCON_Options, write=SetRCON_Options};
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=GetNotifyEvents, write=SetNotifyEvents, nodefault};
	
private:
	System::DynamicArray<System::Byte> FBuffer;
	Sgctcp_client::TsgcTCPCLient* FTCPClient;
	Sgctcp_client::TsgcTCPCLient* __fastcall GetTCPClient();
	
protected:
	virtual void __fastcall OnConnectEvent(Sgctcp_classes::TsgcTCPConnection* aConnection);
	virtual void __fastcall OnDisconnectEvent(Sgctcp_classes::TsgcTCPConnection* aConnection);
	virtual void __fastcall OnBinaryEvent(Sgctcp_classes::TsgcTCPConnection* Connection, System::Classes::TMemoryStream* const Data);
	virtual void __fastcall OnExceptionEvent(Sgctcp_classes::TsgcTCPConnection* Connection, System::Sysutils::Exception* E);
	__property Sgctcp_client::TsgcTCPCLient* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
public:
	__fastcall virtual TsgcLib_RCON_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcLib_RCON_Client();
	
private:
	bool __fastcall GetActive();
	void __fastcall SetActive(const bool Value);
	
public:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	
private:
	int FID;
	int __fastcall GetNewID();
	
protected:
	virtual void __fastcall DoRCON_SendMessage(int aMethod, const System::UnicodeString aBody, int aId);
	virtual void __fastcall DoRCON_Authenticate();
	void __fastcall DoRCON_ExecCommand(const System::UnicodeString aValue, int aId);
	
public:
	void __fastcall ExecCommand(const System::UnicodeString aValue, const int aId = 0x0);
	
private:
	TsgcRCONAuthenticateEvent FOnAuthenticate;
	System::Classes::TNotifyEvent FOnConnect;
	System::Classes::TNotifyEvent FOnDisconnect;
	TsgcRCONExceptionEvent FOnException;
	TsgcRCONResponseEvent FOnResponse;
	
public:
	__property TsgcRCONAuthenticateEvent OnAuthenticate = {read=FOnAuthenticate, write=FOnAuthenticate};
	__property System::Classes::TNotifyEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property System::Classes::TNotifyEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property TsgcRCONExceptionEvent OnException = {read=FOnException, write=FOnException};
	__property TsgcRCONResponseEvent OnResponse = {read=FOnResponse, write=FOnResponse};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgclib_rcon_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCLIB_RCON_CLIENT)
using namespace Sgclib_rcon_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgclib_rcon_clientHPP
