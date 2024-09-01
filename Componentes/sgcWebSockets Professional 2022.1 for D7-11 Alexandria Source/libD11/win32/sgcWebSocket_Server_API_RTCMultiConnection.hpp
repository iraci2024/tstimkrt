// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server_API_RTCMultiConnection.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_server_api_rtcmulticonnectionHPP
#define Sgcwebsocket_server_api_rtcmulticonnectionHPP

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
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Server_API_Base.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Server_API_SocketIO.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server_api_rtcmulticonnection
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSServerApiData_RTCMultiConnection;
class DELPHICLASS TsgcWSRTCMultiConnectionUser;
class DELPHICLASS TsgcWSRTCMultiConnectionUsers;
class DELPHICLASS TsgcWSRTCMultiConnectionRoom;
class DELPHICLASS TsgcWSRTCMultiConnectionRooms;
class DELPHICLASS TsgcWSRTCMultiConnectionXHR;
class DELPHICLASS TsgcWSRTCMultiConnectionXHRs;
class DELPHICLASS TsgcWSRTCMultiConnection_HTMLDocuments;
class DELPHICLASS TsgcWSRTCMultiConnection_ServerOptions;
class DELPHICLASS TsgcWSRTCMultiConnection_VideoResolutionOptions;
class DELPHICLASS TsgcWSRTCMultiConnection_Options;
class DELPHICLASS TsgcWSServer_API_RTCMultiConnection;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSRTCMultiConnectionVideoResolution : unsigned char { rtcvrSD, rtcvr360p, rtcvr480p, rtcvrHDReady, rtcvrFullHD, rtcvrUltraHD, rtcvrCustom };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSServerApiData_RTCMultiConnection : public Sgcwebsocket_server_api_base::TsgcWSServerApiData
{
	typedef Sgcwebsocket_server_api_base::TsgcWSServerApiData inherited;
	
private:
	System::UnicodeString FRoom;
	System::UnicodeString FUserId;
	
public:
	__property System::UnicodeString Room = {read=FRoom, write=FRoom};
	__property System::UnicodeString UserId = {read=FUserId, write=FUserId};
public:
	/* TObject.Create */ inline __fastcall TsgcWSServerApiData_RTCMultiConnection() : Sgcwebsocket_server_api_base::TsgcWSServerApiData() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSServerApiData_RTCMultiConnection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionUser : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FConnection;
	System::UnicodeString __fastcall GetUserId();
	void __fastcall SetConnection(Sgcwebsocket_classes::TsgcWSConnection* const Value);
	void __fastcall SetUserId(const System::UnicodeString Value);
	
public:
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=FConnection, write=SetConnection};
	__property System::UnicodeString UserId = {read=GetUserId, write=SetUserId};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionUser() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnectionUser() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionUsers : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
protected:
	void __fastcall DoBroadcastUserUpdate(const System::UnicodeString aMethod, const System::UnicodeString aUserId);
	
public:
	TsgcWSRTCMultiConnectionUser* __fastcall GetUser(const System::UnicodeString aUserId);
	void __fastcall AddUser(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aUserId);
	bool __fastcall DeleteUser(const System::UnicodeString aUserId);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionUsers() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnectionUsers() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionRoom : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FMessageEventName;
	TsgcWSRTCMultiConnectionUsers* FUsers;
	System::UnicodeString __fastcall GetRoom();
	TsgcWSRTCMultiConnectionUsers* __fastcall GetUsers();
	void __fastcall SetRoom(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoNewParticipationRequest(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, Sgcjson::TsgcJSON* const aJSON);
	virtual void __fastcall DoForwardMessageToRemoteUser(const System::UnicodeString aRemoteUserId, Sgcjson::TsgcJSON* const aJSON);
	
public:
	__fastcall virtual ~TsgcWSRTCMultiConnectionRoom();
	__property System::UnicodeString MessageEventName = {read=FMessageEventName, write=FMessageEventName};
	__property System::UnicodeString Room = {read=GetRoom, write=SetRoom};
	__property TsgcWSRTCMultiConnectionUsers* Users = {read=GetUsers, write=FUsers};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionRoom() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionRooms : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionRooms() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnectionRooms() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionXHR : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FRequest;
	
public:
	__property System::UnicodeString Request = {read=FRequest, write=FRequest};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionXHR() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnectionXHR() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnectionXHRs : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcWSRTCMultiConnectionXHRs() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnectionXHRs() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnection_HTMLDocuments : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FScreenSharing;
	System::UnicodeString FVideoBroadcasting;
	System::UnicodeString FVideoConferencing;
	
protected:
	virtual bool __fastcall ProcessDocument(const System::UnicodeString aDocument);
	
public:
	__fastcall TsgcWSRTCMultiConnection_HTMLDocuments();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ScreenSharing = {read=FScreenSharing, write=FScreenSharing};
	__property System::UnicodeString VideoBroadcasting = {read=FVideoBroadcasting, write=FVideoBroadcasting};
	__property System::UnicodeString VideoConferencing = {read=FVideoConferencing, write=FVideoConferencing};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnection_HTMLDocuments() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnection_ServerOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHost;
	int FPort;
	
public:
	__fastcall TsgcWSRTCMultiConnection_ServerOptions();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnection_ServerOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnection_VideoResolutionOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FFrameRate;
	int FHeight;
	int FWidth;
	TsgcWSRTCMultiConnectionVideoResolution FResolution;
	void __fastcall SetFrameRate(const int Value);
	void __fastcall SetHeight(const int Value);
	void __fastcall SetResolution(const TsgcWSRTCMultiConnectionVideoResolution Value);
	void __fastcall SetWidth(const int Value);
	
public:
	__fastcall TsgcWSRTCMultiConnection_VideoResolutionOptions();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int FrameRate = {read=FFrameRate, write=SetFrameRate, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property TsgcWSRTCMultiConnectionVideoResolution Resolution = {read=FResolution, write=SetResolution, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSRTCMultiConnection_VideoResolutionOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRTCMultiConnection_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSRTCMultiConnection_HTMLDocuments* FHTMLDocuments;
	System::UnicodeString FFooter;
	System::Classes::TStringList* FIceServers;
	System::UnicodeString FMessageEventName;
	TsgcWSRTCMultiConnection_ServerOptions* FServer;
	TsgcWSRTCMultiConnection_VideoResolutionOptions* FVideoResolution;
	void __fastcall SetHTMLDocuments(TsgcWSRTCMultiConnection_HTMLDocuments* const Value);
	void __fastcall SetIceServers(System::Classes::TStringList* const Value);
	void __fastcall SetServer(TsgcWSRTCMultiConnection_ServerOptions* const Value);
	void __fastcall SetVideoResolution(TsgcWSRTCMultiConnection_VideoResolutionOptions* const Value);
	
public:
	__fastcall TsgcWSRTCMultiConnection_Options();
	__fastcall virtual ~TsgcWSRTCMultiConnection_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString Footer = {read=FFooter, write=FFooter};
	__property System::UnicodeString MessageEventName = {read=FMessageEventName, write=FMessageEventName};
	
__published:
	__property TsgcWSRTCMultiConnection_HTMLDocuments* HTMLDocuments = {read=FHTMLDocuments, write=SetHTMLDocuments};
	__property System::Classes::TStringList* IceServers = {read=FIceServers, write=SetIceServers};
	__property TsgcWSRTCMultiConnection_ServerOptions* Server = {read=FServer, write=SetServer};
	__property TsgcWSRTCMultiConnection_VideoResolutionOptions* VideoResolution = {read=FVideoResolution, write=SetVideoResolution};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSServer_API_RTCMultiConnection : public Sgcwebsocket_server_api_socketio::TsgcWSServer_API_SocketIO
{
	typedef Sgcwebsocket_server_api_socketio::TsgcWSServer_API_SocketIO inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual bool __fastcall DoHTTPRequestApi(Sgcwebsocket_classes::TsgcHTTPRequest* const aRequest, Sgcwebsocket_classes::TsgcHTTPResponse* const aResponse);
	virtual void __fastcall DoHTTPBeforeResourceResponse(Sgcwebsocket_classes::TsgcHTTPRequest* const aRequest, Sgcwebsocket_classes::TsgcHTTPResponse* const aResponse);
	virtual void __fastcall DoSocketIOJSONMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aHeader, const System::UnicodeString aJSON, const System::UnicodeString aRawMessage);
	
private:
	TsgcWSRTCMultiConnectionRooms* FRooms;
	TsgcWSRTCMultiConnectionRooms* __fastcall GetRooms();
	
protected:
	TsgcWSRTCMultiConnectionRoom* __fastcall GetRoom(const System::UnicodeString aRoom);
	TsgcWSRTCMultiConnectionRoom* __fastcall GetRoomByUser(const System::UnicodeString aUserId);
	void __fastcall AddRoom(const System::UnicodeString aRoom);
	bool __fastcall AddUser(const System::UnicodeString aRoom, Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	bool __fastcall DeleteRoom(const System::UnicodeString aRoom);
	__property TsgcWSRTCMultiConnectionRooms* Rooms = {read=GetRooms, write=FRooms};
	
private:
	TsgcWSRTCMultiConnectionXHRs* FXHRs;
	TsgcWSRTCMultiConnectionXHRs* __fastcall GetXHRs();
	
protected:
	void __fastcall AddXHR(const System::UnicodeString aSessionId, const System::UnicodeString aRequest);
	System::UnicodeString __fastcall GetXHR(const System::UnicodeString aSessionId);
	bool __fastcall DeleteXHR(const System::UnicodeString aSessionId);
	__property TsgcWSRTCMultiConnectionXHRs* XHRs = {read=GetXHRs, write=FXHRs};
	
public:
	void __fastcall LoadResources();
	
private:
	System::UnicodeString __fastcall GetServerURL();
	TsgcWSRTCMultiConnection_Options* FRTCMultiConnection;
	void __fastcall SetRTCMultiConnection(TsgcWSRTCMultiConnection_Options* const Value);
	
public:
	__property TsgcWSRTCMultiConnection_Options* RTCMultiConnection = {read=FRTCMultiConnection, write=SetRTCMultiConnection};
	
protected:
	virtual void __fastcall DoExtraDataUpdated(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aJSON);
	virtual void __fastcall DoOpenRoom(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aJSON);
	virtual void __fastcall DoJoinRoom(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aJSON);
	virtual void __fastcall DoVideoConference(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, const System::UnicodeString aJSON);
	
public:
	__fastcall virtual TsgcWSServer_API_RTCMultiConnection(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSServer_API_RTCMultiConnection();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server_api_rtcmulticonnection */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER_API_RTCMULTICONNECTION)
using namespace Sgcwebsocket_server_api_rtcmulticonnection;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_server_api_rtcmulticonnectionHPP
