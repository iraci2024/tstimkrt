// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_SignalR.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_signalrHPP
#define Sgcwebsocket_api_signalrHPP

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
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcJSON.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_signalr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSSignalR_Options;
class DELPHICLASS TsgcWSSignalR_Negotiation;
class DELPHICLASS TsgcWS_API_SignalR;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSSignalRConnectEvent)(System::TObject* Sender, System::UnicodeString MessageId, System::UnicodeString aData);

typedef void __fastcall (__closure *TsgcWSSignalRMessageEvent)(System::TObject* Sender, System::UnicodeString MessageId, System::UnicodeString aData);

typedef void __fastcall (__closure *TsgcWSSignalRResultEvent)(System::TObject* Sender, System::UnicodeString InvocationId, System::UnicodeString Result, System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSSignalRKeepAliveEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSSignalRErrorEvent)(System::TObject* Sender, System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSSignalRDisconnectEvent)(System::TObject* Sender, int aCloseCode);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalR_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TStringList* FHubs;
	Sgcwebsocket_types::TwsSignalRProtocolVersions FProtocolVersion;
	System::UnicodeString FUserAgent;
	System::UnicodeString __fastcall GetConnectionData();
	System::UnicodeString __fastcall GetJSONHubs();
	void __fastcall SetHubs(System::Classes::TStringList* const Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__fastcall virtual TsgcWSSignalR_Options();
	__fastcall virtual ~TsgcWSSignalR_Options();
	__property System::UnicodeString ConnectionData = {read=GetConnectionData};
	
__published:
	__property System::Classes::TStringList* Hubs = {read=FHubs, write=SetHubs};
	__property Sgcwebsocket_types::TwsSignalRProtocolVersions ProtocolVersion = {read=FProtocolVersion, write=FProtocolVersion, nodefault};
	__property System::UnicodeString UserAgent = {read=FUserAgent, write=FUserAgent};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalR_Negotiation : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FConnectionId;
	System::UnicodeString FConnectionTimeout;
	System::UnicodeString FConnectionToken;
	System::UnicodeString FDisconnectTimeout;
	System::UnicodeString FKeepAliveTimeout;
	System::UnicodeString FLongPollDelay;
	System::UnicodeString FProtocolVersion;
	System::UnicodeString FTransportConnectTimeout;
	bool FTryWebSockets;
	System::UnicodeString FURL;
	void __fastcall SetConnectionToken(const System::UnicodeString Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	__property System::UnicodeString ConnectionTimeout = {read=FConnectionTimeout, write=FConnectionTimeout};
	__property System::UnicodeString ConnectionToken = {read=FConnectionToken, write=SetConnectionToken};
	__property System::UnicodeString DisconnectTimeout = {read=FDisconnectTimeout, write=FDisconnectTimeout};
	__property System::UnicodeString KeepAliveTimeout = {read=FKeepAliveTimeout, write=FKeepAliveTimeout};
	__property System::UnicodeString LongPollDelay = {read=FLongPollDelay, write=FLongPollDelay};
	__property System::UnicodeString ProtocolVersion = {read=FProtocolVersion, write=FProtocolVersion};
	__property System::UnicodeString TransportConnectTimeout = {read=FTransportConnectTimeout, write=FTransportConnectTimeout};
	__property bool TryWebSockets = {read=FTryWebSockets, write=FTryWebSockets, nodefault};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSignalR_Negotiation() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSSignalR_Negotiation() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_SignalR : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyError(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyException(Sgcwebsocket_classes::TsgcWSConnection* aConnection)/* overload */;
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoBeforeConnect();
	
private:
	System::UnicodeString __fastcall GetProtocolVersion();
	System::UnicodeString __fastcall DoHTTP(const System::UnicodeString aMethod, const System::UnicodeString aQuery = System::UnicodeString());
	void __fastcall DoHTTPNegotiation();
	void __fastcall DoWebSocketURL();
	void __fastcall DoHTTPStart();
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	void __fastcall SetNegotiation(TsgcWSSignalR_Negotiation* const Value);
	void __fastcall SetSignalR(TsgcWSSignalR_Options* const Value);
	
protected:
	System::UnicodeString __fastcall GetJSONData();
	System::UnicodeString __fastcall GetJSONMessageId();
	System::UnicodeString __fastcall GetJSONError();
	System::UnicodeString __fastcall GetJSONInvocationId();
	System::UnicodeString __fastcall GetJSONResult();
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSSignalR_Options* FSignalR;
	TsgcWSSignalR_Negotiation* FNegotiation;
	System::UnicodeString FRawParams;
	
public:
	__property TsgcWSSignalR_Options* SignalR = {read=FSignalR, write=SetSignalR};
	__property TsgcWSSignalR_Negotiation* Negotiation = {read=FNegotiation, write=SetNegotiation};
	__fastcall virtual TsgcWS_API_SignalR(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_SignalR();
	void __fastcall WriteData(const System::UnicodeString aText);
	
private:
	TsgcWSSignalRConnectEvent FOnSignalRConnect;
	TsgcWSSignalRDisconnectEvent FOnSignalRDisconnect;
	TsgcWSSignalRErrorEvent FOnSignalRError;
	TsgcWSSignalRMessageEvent FOnSignalRMessage;
	TsgcWSSignalRKeepAliveEvent FOnSignalRKeepAlive;
	TsgcWSSignalRResultEvent FOnSignalRResult;
	
protected:
	virtual void __fastcall DoEventSignalRConnect(const System::UnicodeString aMessageId, const System::UnicodeString aData);
	virtual void __fastcall DoEventSignalRMessage(const System::UnicodeString aMessageId, const System::UnicodeString aData);
	virtual void __fastcall DoEventSignalRKeepAlive();
	virtual void __fastcall DoEventSignalRResult(const System::UnicodeString aInvocationId, const System::UnicodeString aResult, const System::UnicodeString aError);
	virtual void __fastcall DoEventSignalRError(const System::UnicodeString aError);
	virtual void __fastcall DoEventSignalRDisconnect(const int aCloseCode);
	__property TsgcWSSignalRConnectEvent OnSignalRConnect = {read=FOnSignalRConnect, write=FOnSignalRConnect};
	__property TsgcWSSignalRMessageEvent OnSignalRMessage = {read=FOnSignalRMessage, write=FOnSignalRMessage};
	__property TsgcWSSignalRKeepAliveEvent OnSignalRKeepAlive = {read=FOnSignalRKeepAlive, write=FOnSignalRKeepAlive};
	__property TsgcWSSignalRResultEvent OnSignalRResult = {read=FOnSignalRResult, write=FOnSignalRResult};
	__property TsgcWSSignalRErrorEvent OnSignalRError = {read=FOnSignalRError, write=FOnSignalRError};
	__property TsgcWSSignalRDisconnectEvent OnSignalRDisconnect = {read=FOnSignalRDisconnect, write=FOnSignalRDisconnect};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoNotifyException(const System::UnicodeString Error, System::Sysutils::Exception* aException){ Sgcwebsocket_classes::TsgcWSComponent_Base::DoNotifyException(Error, aException); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_signalr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_SIGNALR)
using namespace Sgcwebsocket_api_signalr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_signalrHPP
