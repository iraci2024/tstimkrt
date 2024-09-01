// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Presence_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_presence_messageHPP
#define Sgcwebsocket_protocol_presence_messageHPP

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
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_presence_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSPresenceError;
class DELPHICLASS TsgcWSPresenceMember;
class DELPHICLASS TsgcWSPresenceMemberList;
class DELPHICLASS TsgcWSPresenceChannel;
class DELPHICLASS TsgcWSPresenceMsg;
class DELPHICLASS TsgcWSPresenceChannelList;
class DELPHICLASS TsgcWSPresenceMessage;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSPresenceNewMemberEvent)(Sgcwebsocket_classes::TsgcWSConnection* aConnection, TsgcWSPresenceMember* const aMember);

typedef void __fastcall (__closure *TsgcWSPresenceRemoveMemberEvent)(Sgcwebsocket_classes::TsgcWSConnection* aConnection, TsgcWSPresenceMember* aMember);

typedef void __fastcall (__closure *TsgcWSPresenceNewMemberChannelEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSPresenceChannel* const aChannel, TsgcWSPresenceMember* const aMember);

typedef void __fastcall (__closure *TsgcWSPresenceRemoveMemberChannelEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSPresenceChannel* const aChannel, TsgcWSPresenceMember* const aMember);

typedef void __fastcall (__closure *TsgcWSPresenceMemberChannelErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSPresenceError* const aError, TsgcWSPresenceChannel* const aChannel, TsgcWSPresenceMember* const aMember);

typedef void __fastcall (__closure *TsgcWSPresencePublishMsgErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSPresenceError* const aError, TsgcWSPresenceMsg* const aMsg, TsgcWSPresenceChannel* const aChannel, TsgcWSPresenceMember* const aMember);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceError : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FCode;
	System::UnicodeString FText;
	
protected:
	virtual void __fastcall Clear();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property int Code = {read=FCode, write=FCode, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSPresenceError() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSPresenceError() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceMember : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FConnection;
	System::TObject* FData;
	System::UnicodeString FID;
	System::UnicodeString FInfo;
	System::UnicodeString FName;
	
protected:
	virtual void __fastcall Clear();
	
public:
	__fastcall virtual ~TsgcWSPresenceMember();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=FConnection, write=FConnection};
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString Info = {read=FInfo, write=FInfo};
	__property System::TObject* Data = {read=FData, write=FData};
public:
	/* TObject.Create */ inline __fastcall TsgcWSPresenceMember() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceMemberList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	TsgcWSPresenceMember* operator[](int Index) { return this->Member[Index]; }
	
private:
	bool FOwnObjects;
	int __fastcall GetCount();
	
protected:
	virtual TsgcWSPresenceMember* __fastcall GetMemberByIndex(int Index);
	
public:
	__fastcall virtual ~TsgcWSPresenceMemberList();
	virtual void __fastcall Clear();
	void __fastcall AddMember(TsgcWSPresenceMember* const aMember);
	bool __fastcall DeleteMember(const System::UnicodeString aID);
	TsgcWSPresenceMember* __fastcall GetMember(const System::UnicodeString aID);
	__property TsgcWSPresenceMember* Member[int Index] = {read=GetMemberByIndex/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	__property bool OwnObjects = {read=FOwnObjects, write=FOwnObjects, nodefault};
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcWSPresenceMemberList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceChannel : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSPresenceMemberList* FMembers;
	System::UnicodeString FName;
	TsgcWSPresenceMemberList* __fastcall GetMembers();
	
public:
	__fastcall virtual ~TsgcWSPresenceChannel();
	
protected:
	virtual void __fastcall Clear();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property TsgcWSPresenceMemberList* Members = {read=GetMembers};
	__property System::UnicodeString Name = {read=FName, write=FName};
public:
	/* TObject.Create */ inline __fastcall TsgcWSPresenceChannel() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceMsg : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsMessageType FMsgType;
	System::UnicodeString FText;
	System::Classes::TMemoryStream* FStream;
	
public:
	__fastcall virtual TsgcWSPresenceMsg();
	void __fastcall Clear();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property Sgcwebsocket_types::TwsMessageType MsgType = {read=FMsgType, write=FMsgType, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSPresenceMsg() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSPresenceChannelList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	__fastcall virtual ~TsgcWSPresenceChannelList();
	virtual void __fastcall Clear();
	void __fastcall DeleteMember(const System::UnicodeString aID);
	void __fastcall AddChannel(TsgcWSPresenceChannel* const aChannel);
	bool __fastcall DeleteChannel(const System::UnicodeString aName);
	TsgcWSPresenceChannel* __fastcall GetChannel(const System::UnicodeString aName);
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcWSPresenceChannelList() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSPresenceMessage : public Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base
{
	typedef Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base inherited;
	
private:
	System::UnicodeString FID;
	System::UnicodeString Fmethod;
	System::UnicodeString Fversion;
	TsgcWSPresenceMember* FMember;
	TsgcWSPresenceChannel* FChannel;
	TsgcWSPresenceMsg* FMsg;
	TsgcWSPresenceError* FError;
	TsgcWSPresenceMemberList* FMembers;
	TsgcWSPresenceMember* __fastcall GetMember();
	TsgcWSPresenceChannel* __fastcall GetChannel();
	TsgcWSPresenceMsg* __fastcall GetMsg();
	TsgcWSPresenceError* __fastcall GetError();
	TsgcWSPresenceMemberList* __fastcall GetMembers();
	
protected:
	virtual System::Variant __fastcall DoReadJSONValue(const System::UnicodeString aField, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual System::Variant __fastcall DoReadJSONValue(int aItem, Sgcjson::TsgcObjectJSON* aJSONObject = (Sgcjson::TsgcObjectJSON*)(0x0))/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, const System::UnicodeString aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	
public:
	__fastcall virtual TsgcWSPresenceMessage(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSPresenceMessage();
	virtual void __fastcall Clear(bool aForceClear = false);
	virtual void __fastcall Read(const System::UnicodeString aMessage);
	virtual System::UnicodeString __fastcall Write();
	
private:
	bool FBroadcast;
	bool FEncodeBase64;
	
public:
	__property bool Broadcast = {read=FBroadcast, write=FBroadcast, nodefault};
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	__property TsgcWSPresenceMemberList* Members = {read=GetMembers, write=FMembers};
	__property TsgcWSPresenceMember* Member = {read=GetMember, write=FMember};
	__property TsgcWSPresenceChannel* Channel = {read=GetChannel, write=FChannel};
	__property TsgcWSPresenceMsg* Msg = {read=GetMsg, write=FMsg};
	__property TsgcWSPresenceError* Error = {read=GetError, write=FError};
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString method = {read=Fmethod, write=Fmethod};
	__property System::UnicodeString version = {read=Fversion};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoWriteJSONValue(const System::UnicodeString aName, int aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON()){ Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base::DoWriteJSONValue(aName, aValue, aJSONObject); }
	inline void __fastcall  DoWriteJSONValue(const System::UnicodeString aName, __int64 aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON()){ Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base::DoWriteJSONValue(aName, aValue, aJSONObject); }
	inline void __fastcall  DoWriteJSONValue(const System::UnicodeString aName, double aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON()){ Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base::DoWriteJSONValue(aName, aValue, aJSONObject); }
	inline void __fastcall  DoWriteJSONValue(const System::UnicodeString aName, bool aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON()){ Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base::DoWriteJSONValue(aName, aValue, aJSONObject); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_presence_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_PRESENCE_MESSAGE)
using namespace Sgcwebsocket_protocol_presence_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_presence_messageHPP
