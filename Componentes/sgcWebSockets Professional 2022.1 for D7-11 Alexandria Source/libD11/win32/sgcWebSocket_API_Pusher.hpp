// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Pusher.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_pusherHPP
#define Sgcwebsocket_api_pusherHPP

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
#include <sgcJSON.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_pusher
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSPusherRequestAuthentication;
class DELPHICLASS TsgcWSPusherResponseAuthentication;
class DELPHICLASS TsgcWSPusher_Options;
class DELPHICLASS TsgcWS_API_Pusher;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSPusherConnectEvent)(System::TObject* Sender, System::UnicodeString Socket_id, int Timeout);

typedef void __fastcall (__closure *TsgcWSPusherErrorEvent)(System::TObject* Sender, int Code, System::UnicodeString Msg);

typedef void __fastcall (__closure *TsgcWSPusherSubscribeEvent)(System::TObject* Sender, System::UnicodeString Channel, System::UnicodeString Data);

typedef void __fastcall (__closure *TsgcWSPusherEventEvent)(System::TObject* Sender, System::UnicodeString Event, System::UnicodeString Channel, System::UnicodeString Data);

typedef void __fastcall (__closure *TsgcWSPusherAuthenticationEvent)(System::TObject* Sender, TsgcWSPusherRequestAuthentication* AuthRequest, TsgcWSPusherResponseAuthentication* AuthResponse);

enum DECLSPEC_DENUM TsgcWSPusherChannels : unsigned char { pscPublicChannel, pscPrivateChannel, pscPresenceChannel };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPusherRequestAuthentication : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FChannel;
	System::UnicodeString FData;
	System::UnicodeString FKey;
	System::UnicodeString FSocketID;
	
public:
	__property System::UnicodeString Channel = {read=FChannel};
	__property System::UnicodeString Data = {read=FData};
	__property System::UnicodeString Key = {read=FKey};
	__property System::UnicodeString SocketID = {read=FSocketID};
public:
	/* TObject.Create */ inline __fastcall TsgcWSPusherRequestAuthentication() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSPusherRequestAuthentication() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPusherResponseAuthentication : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FSecret;
	System::UnicodeString FSignature;
	
public:
	__property System::UnicodeString Secret = {read=FSecret, write=FSecret};
	__property System::UnicodeString Signature = {read=FSignature, write=FSignature};
public:
	/* TObject.Create */ inline __fastcall TsgcWSPusherResponseAuthentication() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSPusherResponseAuthentication() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPusher_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAppId;
	System::UnicodeString FCluster;
	System::UnicodeString FKey;
	System::UnicodeString FName;
	System::UnicodeString FSecret;
	bool FTLS;
	System::UnicodeString FVersion;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString AppId = {read=FAppId, write=FAppId};
	__property System::UnicodeString Cluster = {read=FCluster, write=FCluster};
	__property System::UnicodeString Key = {read=FKey, write=FKey};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString Secret = {read=FSecret, write=FSecret};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	__property System::UnicodeString Version = {read=FVersion, write=FVersion};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSPusher_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSPusher_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Pusher : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual System::UnicodeString __fastcall GetURL();
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	System::UnicodeString __fastcall SerializeJSON(const System::UnicodeString aText);
	System::UnicodeString __fastcall DoubleEncodedJSON(const System::UnicodeString aText);
	System::UnicodeString __fastcall GetChannelName(const System::UnicodeString aChannel, TsgcWSPusherChannels aChannelType);
	
public:
	__fastcall virtual TsgcWS_API_Pusher(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Pusher();
	
protected:
	void __fastcall DoSendPong();
	void __fastcall DoReceivePong();
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent);
	
private:
	System::UnicodeString __fastcall GetPusherSignature(const System::UnicodeString aKey, const System::UnicodeString aSecret, const System::UnicodeString aSocketID, const System::UnicodeString aChannelName, const System::UnicodeString aData = System::UnicodeString());
	
public:
	void __fastcall Ping(const System::UnicodeString aData = L"{}");
	void __fastcall Subscribe(const System::UnicodeString aChannel, TsgcWSPusherChannels aChannelType = (TsgcWSPusherChannels)(0x0), System::UnicodeString aData = System::UnicodeString());
	void __fastcall UnSubscribe(const System::UnicodeString aChannel, TsgcWSPusherChannels aChannelType = (TsgcWSPusherChannels)(0x0));
	void __fastcall Publish(const System::UnicodeString aEvent, const System::UnicodeString aChannel, TsgcWSPusherChannels aChannelType = (TsgcWSPusherChannels)(0x0), const System::UnicodeString aData = L"{}");
	
private:
	System::UnicodeString __fastcall GetEndpoint();
	System::UnicodeString __fastcall GetSignature(const System::UnicodeString aHTTPMethod, const System::UnicodeString aRequestPath, const System::UnicodeString aQueryParams);
	
protected:
	System::UnicodeString __fastcall DoHTTP_Request(const System::UnicodeString aHTTPMethod, const System::UnicodeString aRequestPath, System::UnicodeString aBody = System::UnicodeString());
	System::UnicodeString __fastcall DoHTTP_GET_Request(const System::UnicodeString aRequestPath);
	System::UnicodeString __fastcall DoHTTP_POST_Request(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	
public:
	System::UnicodeString __fastcall TriggerEvent(const System::UnicodeString aEventName, const System::UnicodeString aChannel, const System::UnicodeString aData);
	System::UnicodeString __fastcall GetChannels();
	System::UnicodeString __fastcall GetChannel(const System::UnicodeString aChannel);
	System::UnicodeString __fastcall GetUsers(const System::UnicodeString aChannel);
	
private:
	System::UnicodeString FSocket_id;
	TsgcWSPusher_Options* FPusher;
	void __fastcall SetPusher(TsgcWSPusher_Options* const Value);
	
public:
	__property TsgcWSPusher_Options* Pusher = {read=FPusher, write=SetPusher};
	
private:
	TsgcWSPusherAuthenticationEvent FOnPusherAuthentication;
	TsgcWSPusherConnectEvent FOnPusherConnect;
	TsgcWSPusherErrorEvent FOnPusherError;
	TsgcWSPusherEventEvent FOnPusherEvent;
	TsgcWSPusherSubscribeEvent FOnPusherSubscribe;
	
protected:
	void __fastcall DoEventPusherConnect();
	void __fastcall DoEventPusherError();
	void __fastcall DoEventPusherSubscribe();
	void __fastcall DoEventPusherEvent();
	
public:
	__property TsgcWSPusherAuthenticationEvent OnPusherAuthentication = {read=FOnPusherAuthentication, write=FOnPusherAuthentication};
	__property TsgcWSPusherConnectEvent OnPusherConnect = {read=FOnPusherConnect, write=FOnPusherConnect};
	__property TsgcWSPusherErrorEvent OnPusherError = {read=FOnPusherError, write=FOnPusherError};
	__property TsgcWSPusherEventEvent OnPusherEvent = {read=FOnPusherEvent, write=FOnPusherEvent};
	__property TsgcWSPusherSubscribeEvent OnPusherSubscribe = {read=FOnPusherSubscribe, write=FOnPusherSubscribe};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_pusher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_PUSHER)
using namespace Sgcwebsocket_api_pusher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_pusherHPP
