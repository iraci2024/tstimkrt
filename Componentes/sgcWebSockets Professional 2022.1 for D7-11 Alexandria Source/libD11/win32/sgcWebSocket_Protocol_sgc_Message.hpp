// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_sgc_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_sgc_messageHPP
#define Sgcwebsocket_protocol_sgc_messageHPP

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
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_sgc_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSQoSItem;
class DELPHICLASS TsgcWSQoSList;
class DELPHICLASS TsgcWSQoS_Options;
class DELPHICLASS TsgcWSMessage;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSAcknowledgment)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Id);

class PASCALIMPLEMENTATION TsgcWSQoSItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FGuid;
	System::TDateTime FDate;
	System::UnicodeString FID;
	System::UnicodeString FText;
	void __fastcall SetID(const System::UnicodeString Value);
	
public:
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::UnicodeString ID = {read=FID, write=SetID};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Create */ inline __fastcall TsgcWSQoSItem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSQoSItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQoSList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSQoSList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSQoSList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSQoSList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQoS_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsQoS FLevel;
	int FInterval;
	int FTimeout;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TwsQoS Level = {read=FLevel, write=FLevel, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSQoS_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSQoS_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMessage : public Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base
{
	typedef Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base inherited;
	
private:
	int Ferrorcode;
	System::UnicodeString Ferrordata;
	System::UnicodeString Ferrormessage;
	System::UnicodeString FGuid;
	System::UnicodeString Fid;
	Sgcjson::_di_IsgcJSON FJSONResult;
	System::UnicodeString Fmethod;
	System::UnicodeString Fparams;
	System::UnicodeString Fresult;
	System::UnicodeString FText;
	System::UnicodeString Fversion;
	System::UnicodeString FQoS;
	System::UnicodeString FQueue;
	System::UnicodeString FChannel;
	Sgcjson::_di_IsgcJSON __fastcall GetJSONResult();
	
protected:
	virtual void __fastcall DoReadMethods();
	
public:
	__fastcall virtual TsgcWSMessage(System::Classes::TComponent* aOwner);
	virtual void __fastcall Clear(bool aForceClear = false);
	virtual void __fastcall Read(const System::UnicodeString aMessage);
	virtual System::UnicodeString __fastcall Write();
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString QoS = {read=FQoS, write=FQoS};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property int errorcode = {read=Ferrorcode, write=Ferrorcode, nodefault};
	__property System::UnicodeString errordata = {read=Ferrordata, write=Ferrordata};
	__property System::UnicodeString errormessage = {read=Ferrormessage, write=Ferrormessage};
	__property System::UnicodeString id = {read=Fid, write=Fid};
	__property Sgcjson::_di_IsgcJSON JSONResult = {read=GetJSONResult, write=FJSONResult};
	__property System::UnicodeString method = {read=Fmethod, write=Fmethod};
	__property System::UnicodeString params = {read=Fparams, write=Fparams};
	__property System::UnicodeString result = {read=Fresult, write=Fresult};
	__property System::UnicodeString version = {read=Fversion};
public:
	/* TsgcWSMessage_Base.Destroy */ inline __fastcall virtual ~TsgcWSMessage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_sgc_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_SGC_MESSAGE)
using namespace Sgcwebsocket_protocol_sgc_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_sgc_messageHPP
