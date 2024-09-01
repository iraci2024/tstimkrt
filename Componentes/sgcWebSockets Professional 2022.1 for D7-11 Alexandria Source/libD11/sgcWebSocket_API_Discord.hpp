// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Discord.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_discordHPP
#define Sgcwebsocket_api_discordHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcJSON.hpp>
#include <sgcHTTP_OAuth2_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_discord
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSDiscordBot_Options;
class DELPHICLASS TsgcWSDiscord_Options;
class DELPHICLASS TsgcWS_API_Discord;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSDiscordEvents : unsigned char { disDispatch, disHeartBeat, disIdentify, disStatusUpdate, disVoiceStateUpdate, disVoiceServerPing, disResume, disReconnect, disRequestGuildMembers, disInvalidSession, disHello, disHeartBeatACK };

typedef void __fastcall (__closure *TsgcWSDiscordEvent)(System::TObject* Sender, TsgcWSDiscordEvents Event, const System::UnicodeString RawData);

typedef void __fastcall (__closure *TsgcWSDiscordReadyEvent)(System::TObject* Sender, const System::UnicodeString RawData);

typedef void __fastcall (__closure *TsgcWSDiscordResumedEvent)(System::TObject* Sender, const System::UnicodeString RawData);

typedef void __fastcall (__closure *TsgcWSDiscordDispatchEvent)(System::TObject* Sender, const System::UnicodeString Event, const System::UnicodeString RawData);

typedef void __fastcall (__closure *TsgcWSDiscordDBeforeReconnectEvent)(System::TObject* Sender, System::UnicodeString &Token, System::UnicodeString &Session_Id, int &Seq, bool &Reconnect);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSDiscordBot_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FBotName;
	System::UnicodeString FBotURL;
	System::UnicodeString FToken;
	
public:
	__fastcall virtual TsgcWSDiscordBot_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString BotName = {read=FBotName, write=FBotName};
	__property System::UnicodeString BotURL = {read=FBotURL, write=FBotURL};
	__property System::UnicodeString Token = {read=FToken, write=FToken};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSDiscordBot_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSDiscord_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FBot;
	TsgcWSDiscordBot_Options* FBotOptions;
	System::UnicodeString FClientId;
	System::UnicodeString FClientSecret;
	int FIntents;
	int FVersion;
	void __fastcall SetBotOptions(TsgcWSDiscordBot_Options* const Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__fastcall virtual TsgcWSDiscord_Options();
	__fastcall virtual ~TsgcWSDiscord_Options();
	__property bool Bot = {read=FBot, write=FBot, nodefault};
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString ClientSecret = {read=FClientSecret, write=FClientSecret};
	
__published:
	__property TsgcWSDiscordBot_Options* BotOptions = {read=FBotOptions, write=SetBotOptions};
	__property int Intents = {read=FIntents, write=FIntents, nodefault};
	__property int Version = {read=FVersion, write=FVersion, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Discord : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoBeforeConnect();
	virtual bool __fastcall DoKeepAlive();
	
private:
	System::UnicodeString __fastcall GetDiscordBotURL();
	System::UnicodeString __fastcall GetDiscordURL();
	Sgchttp_client::TsgcIdHTTP* FHTTPClient;
	Sgchttp_client::TsgcIdHTTP* __fastcall GetHTTPClient();
	
protected:
	__property Sgchttp_client::TsgcIdHTTP* HTTPClient = {read=GetHTTPClient, write=FHTTPClient};
	
private:
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* FOAuth2;
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* __fastcall GetOAuth2();
	
protected:
	__property Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* OAuth2 = {read=GetOAuth2, write=FOAuth2};
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSDiscord_Options* FDiscordOptions;
	void __fastcall SetDiscordOptions(TsgcWSDiscord_Options* const Value);
	
public:
	__property TsgcWSDiscord_Options* DiscordOptions = {read=FDiscordOptions, write=SetDiscordOptions};
	__fastcall virtual TsgcWS_API_Discord(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Discord();
	
private:
	System::UnicodeString FSession_id;
	int FSequence;
	int FReadSequence;
	int FLastReadSequence;
	bool FSessionResumed;
	System::UnicodeString __fastcall GetUserAgent();
	void __fastcall DoDiscordHELLO(const System::UnicodeString aRawData);
	void __fastcall DoDiscordHEARTBEAT();
	void __fastcall DoDiscordDISPATCH(const System::UnicodeString aRawData);
	void __fastcall DoDiscordRECONNECT(const System::UnicodeString aSession_id);
	void __fastcall OnAfterAccessTokenEvent(System::TObject* Sender, const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams, bool &Handled);
	void __fastcall DoAuthenticateUser();
	void __fastcall DoIdentifyUser(const System::UnicodeString aToken);
	void __fastcall DoReadSequence();
	
protected:
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent, const System::UnicodeString aRawData);
	void __fastcall DoHTTPRequest();
	System::UnicodeString __fastcall DoGET_Request(const System::UnicodeString aPath);
	System::UnicodeString __fastcall DoPOST_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall DoPUT_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall DoPATCH_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall DoDELETE_Request(const System::UnicodeString aPath);
	
public:
	void __fastcall Ping();
	System::UnicodeString __fastcall GET_Request(const System::UnicodeString aPath);
	System::UnicodeString __fastcall POST_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall PUT_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall PATCH_Request(const System::UnicodeString aPath, const System::UnicodeString aMessage);
	System::UnicodeString __fastcall DELETE_Request(const System::UnicodeString aPath);
	__property System::UnicodeString Session_Id = {read=FSession_id};
	__property int Sequence = {read=FSequence, nodefault};
	
private:
	TsgcWSDiscordDispatchEvent FOnDiscordDispatch;
	TsgcWSDiscordEvent FOnDiscordEvent;
	TsgcWSDiscordReadyEvent FOnDiscordReady;
	TsgcWSDiscordResumedEvent FOnDiscordResumed;
	TsgcWSDiscordDBeforeReconnectEvent FOnDiscordBeforeReconnect;
	
protected:
	virtual void __fastcall DoDiscordEvent(TsgcWSDiscordEvents aEvent, const System::UnicodeString aRawData);
	virtual void __fastcall DoDiscordReadyEvent(const System::UnicodeString aRawData);
	virtual void __fastcall DoDiscordResumedEvent(const System::UnicodeString aRawData);
	virtual void __fastcall DoDiscordDispatchEvent(const System::UnicodeString aEvent, const System::UnicodeString aRawData);
	virtual bool __fastcall DoDiscordBeforeReconnectEvent(System::UnicodeString &aToken, System::UnicodeString &aSession_id, int &aSeq);
	
public:
	__property TsgcWSDiscordEvent OnDiscordEvent = {read=FOnDiscordEvent, write=FOnDiscordEvent};
	__property TsgcWSDiscordReadyEvent OnDiscordReady = {read=FOnDiscordReady, write=FOnDiscordReady};
	__property TsgcWSDiscordResumedEvent OnDiscordResumed = {read=FOnDiscordResumed, write=FOnDiscordResumed};
	__property TsgcWSDiscordDispatchEvent OnDiscordDispatch = {read=FOnDiscordDispatch, write=FOnDiscordDispatch};
	__property TsgcWSDiscordDBeforeReconnectEvent OnDiscordBeforeReconnect = {read=FOnDiscordBeforeReconnect, write=FOnDiscordBeforeReconnect};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_discord */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_DISCORD)
using namespace Sgcwebsocket_api_discord;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_discordHPP
