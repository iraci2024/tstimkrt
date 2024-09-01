// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_AppRTC_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_apprtc_serverHPP
#define Sgcwebsocket_protocol_apprtc_serverHPP

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
#include <System.Contnrs.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_apprtc_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcMessages;
class DELPHICLASS TsgcRoomClient;
class DELPHICLASS TsgcRoom;
class DELPHICLASS TsgcRoomList;
class DELPHICLASS TsgcWSAppRTCAudio;
class DELPHICLASS TsgcWSAppRTCVideo;
class DELPHICLASS TsgcWSAppRTCMediaConstraints;
class DELPHICLASS TsgcWSAppRTC_Options;
class DELPHICLASS TsgcWSProtocol_AppRTC_Server;
class DELPHICLASS TsgcWSProtocol_JS_AppRTC;
class DELPHICLASS TsgcWSProtocol_CSS_AppRTC;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcMessages : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	System::UnicodeString __fastcall GetJSONList();
	
public:
	__property System::UnicodeString JSONList = {read=GetJSONList};
public:
	/* TStringList.Create */ inline __fastcall TsgcMessages()/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TsgcMessages(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Create */ inline __fastcall TsgcMessages(System::WideChar QuoteChar, System::WideChar Delimiter)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter) { }
	/* TStringList.Create */ inline __fastcall TsgcMessages(System::WideChar QuoteChar, System::WideChar Delimiter, System::Classes::TStringsOptions Options)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter, Options) { }
	/* TStringList.Create */ inline __fastcall TsgcMessages(System::Types::TDuplicates Duplicates, bool Sorted, bool CaseSensitive)/* overload */ : System::Classes::TStringList(Duplicates, Sorted, CaseSensitive) { }
	/* TStringList.Destroy */ inline __fastcall virtual ~TsgcMessages() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcRoomClient : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FActive;
	System::UnicodeString FClientId;
	System::UnicodeString FGUID;
	bool FInitiator;
	TsgcMessages* FMessages;
	TsgcMessages* __fastcall GetMessages();
	
public:
	__fastcall virtual ~TsgcRoomClient();
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString GUID = {read=FGUID, write=FGUID};
	__property bool Initiator = {read=FInitiator, write=FInitiator, nodefault};
	__property TsgcMessages* Messages = {read=GetMessages, write=FMessages};
public:
	/* TObject.Create */ inline __fastcall TsgcRoomClient() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcRoom : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FRoomId;
	TsgcRoomClient* FClient1;
	TsgcRoomClient* FClient2;
	TsgcRoomClient* __fastcall GetClient1();
	TsgcRoomClient* __fastcall GetClient2();
	
public:
	__fastcall virtual ~TsgcRoom();
	__property System::UnicodeString RoomId = {read=FRoomId, write=FRoomId};
	__property TsgcRoomClient* Client1 = {read=GetClient1, write=FClient1};
	__property TsgcRoomClient* Client2 = {read=GetClient2, write=FClient2};
public:
	/* TObject.Create */ inline __fastcall TsgcRoom() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcRoomList : public Sgcwebsocket_classes::TsgcQueueList
{
	typedef Sgcwebsocket_classes::TsgcQueueList inherited;
	
public:
	/* TsgcQueueList.Destroy */ inline __fastcall virtual ~TsgcRoomList() { }
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcRoomList() : Sgcwebsocket_classes::TsgcQueueList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAppRTCAudio : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	
public:
	__fastcall TsgcWSAppRTCAudio();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAppRTCAudio() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAppRTCVideo : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FMinHeight;
	int FMinWidth;
	
public:
	__fastcall TsgcWSAppRTCVideo();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int MinHeight = {read=FMinHeight, write=FMinHeight, nodefault};
	__property int MinWidth = {read=FMinWidth, write=FMinWidth, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAppRTCVideo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAppRTCMediaConstraints : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSAppRTCAudio* FAudio;
	TsgcWSAppRTCVideo* FVideo;
	void __fastcall SetAudio(TsgcWSAppRTCAudio* const Value);
	void __fastcall SetVideo(TsgcWSAppRTCVideo* const Value);
	System::UnicodeString __fastcall GetJSONText();
	
public:
	__fastcall TsgcWSAppRTCMediaConstraints();
	__fastcall virtual ~TsgcWSAppRTCMediaConstraints();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSAppRTCAudio* Audio = {read=FAudio, write=SetAudio};
	__property TsgcWSAppRTCVideo* Video = {read=FVideo, write=SetVideo};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAppRTC_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TStringList* FIceServers;
	TsgcWSAppRTCMediaConstraints* FMediaConstraints;
	System::UnicodeString FRoomLink;
	System::UnicodeString FWebSocketURL;
	void __fastcall SetIceServers(System::Classes::TStringList* const Value);
	void __fastcall SetMediaConstraints(TsgcWSAppRTCMediaConstraints* const Value);
	
public:
	__fastcall TsgcWSAppRTC_Options();
	__fastcall virtual ~TsgcWSAppRTC_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::Classes::TStringList* IceServers = {read=FIceServers, write=SetIceServers};
	__property TsgcWSAppRTCMediaConstraints* MediaConstraints = {read=FMediaConstraints, write=SetMediaConstraints};
	__property System::UnicodeString RoomLink = {read=FRoomLink, write=FRoomLink};
	__property System::UnicodeString WebSocketURL = {read=FWebSocketURL, write=FWebSocketURL};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_AppRTC_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual System::UnicodeString __fastcall GetHTTPResponse(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aPath, const System::UnicodeString aBody);
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSAppRTC_Options* FAppRTC;
	void __fastcall SetAppRTC(TsgcWSAppRTC_Options* const Value);
	
public:
	__property TsgcWSAppRTC_Options* AppRTC = {read=FAppRTC, write=SetAppRTC};
	
private:
	TsgcRoomList* FRooms;
	TsgcRoomList* __fastcall GetRooms();
	
protected:
	virtual TsgcRoom* __fastcall GetRoomById(const System::UnicodeString aRoomId);
	virtual TsgcRoom* __fastcall NewRoomById(const System::UnicodeString aRoomId);
	virtual System::UnicodeString __fastcall GetGUIDByPeerGUID(System::UnicodeString aConnectionGUID);
	void __fastcall ClearRoomClientByGUID(System::UnicodeString aConnectionGUID);
	void __fastcall ClearMessagesRoomClientByGUID(System::UnicodeString aConnectionGUID);
	
public:
	__property TsgcRoomList* Rooms = {read=GetRooms, write=FRooms};
	
protected:
	void __fastcall DoRegister(System::UnicodeString aGuid, System::UnicodeString aRoomId, System::UnicodeString aClientId);
	void __fastcall DoSend(System::UnicodeString aGuid, System::UnicodeString aText);
	System::UnicodeString __fastcall DoJoin(const System::UnicodeString aRoomId);
	System::UnicodeString __fastcall DoMessage(const System::UnicodeString aRoomId, const System::UnicodeString aClientId, const System::UnicodeString aBody);
	
public:
	__fastcall virtual TsgcWSProtocol_AppRTC_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_AppRTC_Server();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_AppRTC : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_AppRTC() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_AppRTC() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_CSS_AppRTC : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_CSS_AppRTC() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_CSS_AppRTC() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_apprtc_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_APPRTC_SERVER)
using namespace Sgcwebsocket_protocol_apprtc_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_apprtc_serverHPP
