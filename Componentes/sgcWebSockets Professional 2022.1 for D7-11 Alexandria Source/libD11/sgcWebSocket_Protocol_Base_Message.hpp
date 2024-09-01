// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Base_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_base_messageHPP
#define Sgcwebsocket_protocol_base_messageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_base_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSAckMessage;
class DELPHICLASS TsgcWSAckMessageList;
class DELPHICLASS TsgcWSMessage_Base;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSAckMessage : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FConnection;
	System::UnicodeString FText;
	System::TDateTime FDate;
	System::UnicodeString FID;
	
public:
	__fastcall virtual ~TsgcWSAckMessage();
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=FConnection, write=FConnection};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::UnicodeString ID = {read=FID, write=FID};
public:
	/* TObject.Create */ inline __fastcall TsgcWSAckMessage() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAckMessageList : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
private:
	Sgcbase_classes::TsgcTimerOnException FOnException;
	Sgcwebsocket_classes::TsgcWSMessageEvent FOnMessage;
	int FTimeout;
	Sgcbase_classes::TsgcTimer* FTimer;
	int FInterval;
	void __fastcall DoProcessList();
	
protected:
	virtual void __fastcall OnTimerEvent(System::TObject* Sender);
	virtual void __fastcall OnExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__fastcall virtual TsgcWSAckMessageList();
	__fastcall virtual ~TsgcWSAckMessageList();
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall Add(TsgcWSAckMessage* const aMessage);
	HIDESBASE void __fastcall Delete(System::UnicodeString aID);
	TsgcWSAckMessage* __fastcall GetMessage(const System::UnicodeString aID);
	void __fastcall Start();
	void __fastcall Stop();
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
	__property Sgcbase_classes::TsgcTimerOnException OnException = {read=FOnException, write=FOnException};
	__property Sgcwebsocket_classes::TsgcWSMessageEvent OnMessage = {read=FOnMessage, write=FOnMessage};
};


class PASCALIMPLEMENTATION TsgcWSMessage_Base : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Sgcwebsocket_classes_syncobjs::TsgcCriticalSection* FCS;
	
protected:
	virtual void __fastcall DoEnterCS();
	virtual void __fastcall DoLeaveCS();
	
private:
	Sgcjson::_di_IsgcJSON __fastcall GetObjectJSON(Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON());
	Sgcjson::_di_IsgcJSON FJSON;
	Sgcjson::_di_IsgcJSON __fastcall GetJSON();
	
protected:
	__property Sgcjson::_di_IsgcJSON JSON = {read=GetJSON, write=FJSON};
	virtual void __fastcall Clear(bool aForceClear = false);
	virtual System::Variant __fastcall DoReadJSONValue(const System::UnicodeString aField, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual System::Variant __fastcall DoReadJSONValue(int aItem, Sgcjson::TsgcObjectJSON* aJSONObject = (Sgcjson::TsgcObjectJSON*)(0x0))/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, const System::UnicodeString aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, int aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, __int64 aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, double aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual void __fastcall DoWriteJSONValue(const System::UnicodeString aName, bool aValue, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	Sgcjson::_di_IsgcObjectJSON __fastcall DoAddJSONObject(const System::UnicodeString aName, const System::UnicodeString aValue = System::UnicodeString(), Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON());
	bool FIsReading;
	bool FIsWriting;
	
public:
	void __fastcall DoEnterRead(const System::UnicodeString aText);
	void __fastcall DoLeaveRead();
	void __fastcall DoEnterWrite(bool aForceClear = false);
	void __fastcall DoLeaveWrite();
	__fastcall virtual TsgcWSMessage_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMessage_Base();
	virtual void __fastcall Read(const System::UnicodeString aMessage);
	virtual System::UnicodeString __fastcall Write();
	
private:
	unsigned FThreadId;
	System::UnicodeString FConnectionId;
	
public:
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	
private:
	bool FEncodeBase64;
	
public:
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_base_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BASE_MESSAGE)
using namespace Sgcwebsocket_protocol_base_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_base_messageHPP
