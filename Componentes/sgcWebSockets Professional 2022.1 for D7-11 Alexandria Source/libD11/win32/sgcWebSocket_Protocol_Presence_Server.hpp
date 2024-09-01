// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Presence_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_presence_serverHPP
#define Sgcwebsocket_protocol_presence_serverHPP

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
#include <sgcWebSocket_Protocol_Presence_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_presence_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_Presence_Server;
class DELPHICLASS TsgcWSProtocol_JS_Response_Presence;
class DELPHICLASS TsgcWSProtocol_HTML_Response_Presence;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSPresenceBeforeNewMember)(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember, bool &Accept);

typedef void __fastcall (__closure *TsgcWSPresenceBeforeNewChannel)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, bool &Accept);

typedef void __fastcall (__closure *TsgcWSPresenceNewChannel)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel);

typedef void __fastcall (__closure *TsgcWSPresenceBeforeNewMemberChannel)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, bool &Accept);

typedef void __fastcall (__closure *TsgcWSPresenceBeforePublishMsgEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* const aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, bool &Accept);

typedef void __fastcall (__closure *TsgcWSPresenceBeforeSendMembersEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, bool &Accept);

class PASCALIMPLEMENTATION TsgcWSProtocol_Presence_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Acknowledgment_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Acknowledgment_Server_Base inherited;
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall OnAcknowledgmentMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWSProtocol_Presence_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Presence_Server();
	
private:
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannelList* FChannels;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* FMembers;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannelList* __fastcall GetChannels();
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* __fastcall GetMembers();
	
protected:
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannelList* Channels = {read=GetChannels};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* Members = {read=GetMembers, write=FMembers};
	
private:
	System::UnicodeString __fastcall GetErrorDescription(int aCode);
	
protected:
	void __fastcall DoWriteDataWithAck(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* aMessage, bool aClearMessage = true);
	void __fastcall DoProcessAcknowledgment(System::UnicodeString aID);
	bool __fastcall DoWriteMessageText(System::UnicodeString aGuid, System::UnicodeString aMessage);
	virtual void __fastcall DoSession(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aID);
	virtual void __fastcall DoNewMember(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoRemoveMember(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMemberId = System::UnicodeString());
	virtual Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* __fastcall DoNewChannel(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoNewChannelMember(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aChannel);
	virtual void __fastcall DoRemoveChannelMember(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aChannel, System::UnicodeString aMemberId = System::UnicodeString());
	virtual void __fastcall DoRemoveAllChannelsMember(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoPublishMsg(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* aMessage);
	virtual void __fastcall DoSendMembers(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel);
	virtual void __fastcall DoSendMembersChannel(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel);
	virtual void __fastcall DoSendAllMembers(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoReceiveChannelInvitation(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* aMembers);
	void __fastcall DoSendChannelInvitation(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel);
	virtual void __fastcall DoBroadcastMessage(Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* aMessage, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel = (Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel*)(0x0));
	virtual void __fastcall DoErrorMemberChannel(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel = (Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel*)(0x0), Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember = (Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember*)(0x0));
	void __fastcall DoErrorPublishMsg(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel = (Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel*)(0x0), Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember = (Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember*)(0x0));
	
private:
	bool FEncodeBase64;
	
public:
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	
private:
	TsgcWSPresenceBeforeNewChannel FOnBeforeNewChannel;
	TsgcWSPresenceBeforeNewMemberChannel FOnBeforeNewChannelMember;
	TsgcWSPresenceBeforeNewMember FOnBeforeNewMember;
	TsgcWSPresenceBeforePublishMsgEvent FOnBeforePublishMsg;
	TsgcWSPresenceBeforeSendMembersEvent FOnBeforeSendMembers;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberChannelErrorEvent FOnErrorMemberChannel;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresencePublishMsgErrorEvent FOnErrorPublishMsg;
	TsgcWSPresenceNewChannel FOnNewChannel;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberChannelEvent FOnNewChannelMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberEvent FOnNewMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberChannelEvent FOnRemoveChannelMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberEvent FOnRemoveMember;
	
protected:
	virtual bool __fastcall DoBeforeNewMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual bool __fastcall DoBeforeNewChannelMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual bool __fastcall DoBeforeNewChannelEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual bool __fastcall DoBeforeSendMembersEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoNewChannelEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* &aChannel);
	virtual void __fastcall DoNewChannelMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoNewMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoRemoveChannelMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoRemoveMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual bool __fastcall DoBeforePublishMsgEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoErrorMemberChannelEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceError* aError, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoErrorPublishMsgEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceError* aError, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	
public:
	__property TsgcWSPresenceBeforeNewMemberChannel OnBeforeNewChannelMember = {read=FOnBeforeNewChannelMember, write=FOnBeforeNewChannelMember};
	__property TsgcWSPresenceBeforeNewMember OnBeforeNewMember = {read=FOnBeforeNewMember, write=FOnBeforeNewMember};
	__property TsgcWSPresenceBeforePublishMsgEvent OnBeforePublishMsg = {read=FOnBeforePublishMsg, write=FOnBeforePublishMsg};
	__property TsgcWSPresenceBeforeNewChannel OnBeforeNewChannel = {read=FOnBeforeNewChannel, write=FOnBeforeNewChannel};
	__property TsgcWSPresenceBeforeSendMembersEvent OnBeforeSendMembers = {read=FOnBeforeSendMembers, write=FOnBeforeSendMembers};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberChannelErrorEvent OnErrorMemberChannel = {read=FOnErrorMemberChannel, write=FOnErrorMemberChannel};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresencePublishMsgErrorEvent OnErrorPublishMsg = {read=FOnErrorPublishMsg, write=FOnErrorPublishMsg};
	__property TsgcWSPresenceNewChannel OnNewChannel = {read=FOnNewChannel, write=FOnNewChannel};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberChannelEvent OnNewChannelMember = {read=FOnNewChannelMember, write=FOnNewChannelMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberEvent OnNewMember = {read=FOnNewMember, write=FOnNewMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberChannelEvent OnRemoveChannelMember = {read=FOnRemoveChannelMember, write=FOnRemoveChannelMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberEvent OnRemoveMember = {read=FOnRemoveMember, write=FOnRemoveMember};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_Response_Presence : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_Response_Presence() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_Response_Presence() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_Response_Presence : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_Response_Presence() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_Response_Presence() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_presence_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_PRESENCE_SERVER)
using namespace Sgcwebsocket_protocol_presence_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_presence_serverHPP
