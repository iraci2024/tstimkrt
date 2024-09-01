// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Presence_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_presence_clientHPP
#define Sgcwebsocket_protocol_presence_clientHPP

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
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Protocol_Presence_Server.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_presence_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSPresenceMember_Options;
class DELPHICLASS TsgcWSProtocol_Presence_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSPresenceSessionEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString Session);

typedef void __fastcall (__closure *TsgcWSPresencePublishMsgEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* const aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember);

typedef void __fastcall (__closure *TsgcWSPresenceGetMembersEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* const aMembers, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel);

typedef void __fastcall (__closure *TsgcWSPresenceChannelInvitationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, bool &Accept);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceMember_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TStringList* FInfo;
	System::UnicodeString FName;
	void __fastcall SetInfo(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSPresenceMember_Options();
	__fastcall virtual ~TsgcWSPresenceMember_Options();
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::Classes::TStringList* Info = {read=FInfo, write=SetInfo};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Presence_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Acknowledgment_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Acknowledgment_Client_Base inherited;
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* FWSMessage;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoWriteRawData(const System::UnicodeString aText);
	
public:
	virtual void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	
protected:
	virtual void __fastcall OnAcknowledgmentMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText);
	virtual void __fastcall DoWriteMessageText(const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWSProtocol_Presence_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Presence_Client();
	
private:
	TsgcWSPresenceMember_Options* FPresence;
	void __fastcall SetPresence(TsgcWSPresenceMember_Options* const Value);
	
public:
	__property TsgcWSPresenceMember_Options* Presence = {read=FPresence, write=SetPresence};
	
private:
	System::UnicodeString FSession;
	
protected:
	void __fastcall DoWriteDataWithAck(Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* const aMessage);
	virtual void __fastcall DoAcknowledgment(const System::UnicodeString aID);
	virtual void __fastcall DoProcessAcknowledgment(System::UnicodeString aID);
	virtual void __fastcall DoNewMember();
	virtual void __fastcall DoRemoveMember();
	
public:
	void __fastcall Subscribe(const System::UnicodeString aChannel);
	void __fastcall UnSubscribe(const System::UnicodeString aChannel);
	void __fastcall Invite(const System::UnicodeString aChannel, const System::UnicodeString aMemberID);
	void __fastcall GetMembers()/* overload */;
	void __fastcall GetMembers(const System::UnicodeString aChannel)/* overload */;
	void __fastcall Publish(const System::UnicodeString aText, const System::UnicodeString aChannel = System::UnicodeString());
	
private:
	bool FEncodeBase64;
	
public:
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	
private:
	TsgcWSPresenceChannelInvitationEvent FOnChannelInvitation;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberChannelEvent FOnNewChannelMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberEvent FOnNewMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberChannelErrorEvent FOnErrorMemberChannel;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresencePublishMsgErrorEvent FOnErrorPublishMsg;
	TsgcWSPresenceGetMembersEvent FOnGetMembers;
	TsgcWSPresencePublishMsgEvent FOnPublishMsg;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberChannelEvent FOnRemoveChannelMember;
	Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberEvent FOnRemoveMember;
	TsgcWSPresenceSessionEvent FOnSession;
	
protected:
	virtual void __fastcall DoNewMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember);
	virtual void __fastcall DoRemoveMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoSessionEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMessage* const aMessage);
	virtual void __fastcall DoNewChannelMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoRemoveChannelMemberEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoPublishMsgEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* aMember);
	virtual void __fastcall DoGetMembersEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberList* const aMembers, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel);
	virtual void __fastcall DoErrorMemberChannelEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceError* const aError, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember);
	void __fastcall DoErrorPublishMsgEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceError* const aError, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMsg* const aMsg, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember);
	virtual void __fastcall DoChannelInvitationEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMember* const aMember, Sgcwebsocket_protocol_presence_message::TsgcWSPresenceChannel* const aChannel);
	
public:
	__property TsgcWSPresenceChannelInvitationEvent OnChannelInvitation = {read=FOnChannelInvitation, write=FOnChannelInvitation};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberChannelEvent OnNewChannelMember = {read=FOnNewChannelMember, write=FOnNewChannelMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceNewMemberEvent OnNewMember = {read=FOnNewMember, write=FOnNewMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceMemberChannelErrorEvent OnErrorMemberChannel = {read=FOnErrorMemberChannel, write=FOnErrorMemberChannel};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresencePublishMsgErrorEvent OnErrorPublishMsg = {read=FOnErrorPublishMsg, write=FOnErrorPublishMsg};
	__property TsgcWSPresenceGetMembersEvent OnGetMembers = {read=FOnGetMembers, write=FOnGetMembers};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberChannelEvent OnRemoveChannelMember = {read=FOnRemoveChannelMember, write=FOnRemoveChannelMember};
	__property Sgcwebsocket_protocol_presence_message::TsgcWSPresenceRemoveMemberEvent OnRemoveMember = {read=FOnRemoveMember, write=FOnRemoveMember};
	__property TsgcWSPresenceSessionEvent OnSession = {read=FOnSession, write=FOnSession};
	__property TsgcWSPresencePublishMsgEvent OnPublishMsg = {read=FOnPublishMsg, write=FOnPublishMsg};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  WriteData(System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base::WriteData(aStream, aSize, aStreaming); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_presence_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_PRESENCE_CLIENT)
using namespace Sgcwebsocket_protocol_presence_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_presence_clientHPP
