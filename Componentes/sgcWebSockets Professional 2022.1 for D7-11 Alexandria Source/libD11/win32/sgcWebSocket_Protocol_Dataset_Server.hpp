// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Dataset_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_dataset_serverHPP
#define Sgcwebsocket_protocol_dataset_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Protocol_Dataset_Message.hpp>
#include <sgcWebSocket_Protocol_sgc_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_dataset_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_dataset_server;
class DELPHICLASS TsgcWSProtocol_JS_dataset;
class DELPHICLASS TsgcWSProtocol_HTML_dataset;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSProtocol_dataset_server : public Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server
{
	typedef Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server inherited;
	
protected:
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* FWSMessage;
	virtual Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoProcessMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText);
	
private:
	Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* FWSMessageDataset;
	Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* __fastcall GetWSMessageDataset();
	
protected:
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* WSMessageDataset = {read=GetWSMessageDataset, write=FWSMessageDataset};
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	
private:
	void __fastcall DoAutoSynchronize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
protected:
	virtual void __fastcall DoSynchronizeStart(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoSynchronizeEnd(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoSynchronize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoMetaData(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	void __fastcall MetaData(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	void __fastcall Synchronize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	void __fastcall BroadcastRecord();
	
private:
	Data::Db::TDataSetNotifyEvent FDSAfterPost;
	Data::Db::TDataSetNotifyEvent FDSBeforeDelete;
	Data::Db::TDataSetNotifyEvent FDSAfterRefresh;
	
protected:
	virtual void __fastcall OnUpdateRecordEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnDeleteRecordEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnAfterRefreshEvent(Data::Db::TDataSet* Sender);
	
private:
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeDeleteRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeNewRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeUpdateRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterDeleteRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterNewRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterUpdateRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeDatasetUpdateEvent FOnBeforeDatasetUpdate;
	void __fastcall OnBeforeNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnBeforeUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnBeforeDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnAfterNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnAfterUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnAfterDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnBeforeDatasetUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	
protected:
	virtual void __fastcall DoBeforeNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet, Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet, Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet, Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoAfterNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet);
	virtual void __fastcall DoAfterUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet);
	virtual void __fastcall DoAfterDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* DataSet);
	void __fastcall DoBeforeDatasetUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	
public:
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeNewRecord = {read=FOnBeforeNewRecord, write=FOnBeforeNewRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeUpdateRecord = {read=FOnBeforeUpdateRecord, write=FOnBeforeUpdateRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeDeleteRecord = {read=FOnBeforeDeleteRecord, write=FOnBeforeDeleteRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterNewRecord = {read=FOnAfterNewRecord, write=FOnAfterNewRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterUpdateRecord = {read=FOnAfterUpdateRecord, write=FOnAfterUpdateRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterDeleteRecord = {read=FOnAfterDeleteRecord, write=FOnAfterDeleteRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeDatasetUpdateEvent OnBeforeDatasetUpdate = {read=FOnBeforeDatasetUpdate, write=FOnBeforeDatasetUpdate};
	
protected:
	virtual void __fastcall DoBroadCast(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage, const System::UnicodeString aChannel, const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	
public:
	__fastcall virtual TsgcWSProtocol_dataset_server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_dataset_server();
	
private:
	Data::Db::TDataSet* FDataSet;
	
protected:
	void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	
public:
	__property Data::Db::TDataSet* DataSet = {read=FDataSet, write=SetDataSet};
	
private:
	bool FNotifyUpdates;
	bool FNotifyDeletes;
	bool FApplyUpdates;
	bool FAutoEscapeText;
	bool FAutoSynchronize;
	bool FEncodeBase64;
	Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* FFormatSettings;
	Sgcwebsocket_types::TwsDatasetUpdateMode FUpdateMode;
	void __fastcall SetFormatSettings(Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* const Value);
	
public:
	__property bool AutoSynchronize = {read=FAutoSynchronize, write=FAutoSynchronize, nodefault};
	__property bool NotifyUpdates = {read=FNotifyUpdates, write=FNotifyUpdates, nodefault};
	__property bool NotifyDeletes = {read=FNotifyDeletes, write=FNotifyDeletes, nodefault};
	__property bool ApplyUpdates = {read=FApplyUpdates, write=FApplyUpdates, nodefault};
	__property bool AutoEscapeText = {read=FAutoEscapeText, write=FAutoEscapeText, nodefault};
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* FormatSettings = {read=FFormatSettings, write=SetFormatSettings};
	__property Sgcwebsocket_types::TwsDatasetUpdateMode UpdateMode = {read=FUpdateMode, write=FUpdateMode, nodefault};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoBroadCast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server::DoBroadCast(aStream, aChannel, Exclude, Include, aSize, aStreaming); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_dataset : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_dataset() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_dataset() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_dataset : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_dataset() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_dataset() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_dataset_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_DATASET_SERVER)
using namespace Sgcwebsocket_protocol_dataset_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_dataset_serverHPP
