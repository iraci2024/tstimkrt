// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Dataset_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_dataset_clientHPP
#define Sgcwebsocket_protocol_dataset_clientHPP

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
#include <sgcWebSocket_Protocol_sgc_Client.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_Dataset_Message.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_dataset_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_dataset_client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSProtocol_dataset_client : public Sgcwebsocket_protocol_sgc_client::TsgcWSProtocol_sgc_Client
{
	typedef Sgcwebsocket_protocol_sgc_client::TsgcWSProtocol_sgc_Client inherited;
	
private:
	Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* FWSMessageDataset;
	Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* __fastcall GetWSMessageDataset();
	
protected:
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSMessageDataset* WSMessageDataset = {read=GetWSMessageDataset, write=FWSMessageDataset};
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__fastcall virtual TsgcWSProtocol_dataset_client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_dataset_client();
	
private:
	bool FAutoSubscribe;
	
public:
	void __fastcall Subscribe_all();
	void __fastcall UnSubscribe_all();
	__property bool AutoSubscribe = {read=FAutoSubscribe, write=FAutoSubscribe, nodefault};
	
private:
	Data::Db::TDataSet* FDataSet;
	
protected:
	void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	
public:
	__property Data::Db::TDataSet* DataSet = {read=FDataSet, write=SetDataSet};
	
private:
	Data::Db::TDataSetNotifyEvent FDSBeforePost;
	Data::Db::TDataSetNotifyEvent FDSBeforeDelete;
	Data::Db::TDataSetNotifyEvent FDSAfterPost;
	Data::Db::TDataSetNotifyEvent FDSNewRecord;
	Data::Db::TDataSetNotifyEvent FDSAfterEdit;
	Data::Db::TDataSetNotifyEvent FDSAfterCancel;
	
protected:
	virtual void __fastcall OnUpdateRecordEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnDeleteRecordEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnAfterPostEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnAfterEditEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnNewRecordEvent(Data::Db::TDataSet* Sender);
	virtual void __fastcall OnAfterCancelEvent(Data::Db::TDataSet* Sender);
	
private:
	bool FSynchronizing;
	
protected:
	virtual void __fastcall DoWriteData(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage, const System::UnicodeString aMethod, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aGuid = System::UnicodeString());
	
public:
	void __fastcall Synchronize();
	void __fastcall GetMetaData();
	
private:
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeDeleteRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeNewRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent FOnBeforeUpdateRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterDeleteRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterNewRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent FOnAfterUpdateRecord;
	Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeDatasetUpdateEvent FOnBeforeDatasetUpdate;
	System::Classes::TNotifyEvent FOnAfterSynchronize;
	System::Classes::TNotifyEvent FOnBeforeSynchronize;
	Sgcwebsocket_protocol_dataset_message::TsgcWSMetaDataEvent FOnMetaData;
	void __fastcall OnBeforeNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnBeforeUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnBeforeDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnAfterNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnAfterUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnAfterDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	void __fastcall OnBeforeDatasetUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	void __fastcall OnBeforeSynchronizeEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON);
	void __fastcall OnAfterSynchronizeEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON);
	void __fastcall OnMetaDataEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON);
	void __fastcall SetOnBeforeSynchronize(const System::Classes::TNotifyEvent Value);
	
protected:
	virtual void __fastcall DoBeforeNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoAfterNewRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	virtual void __fastcall DoAfterUpdateRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	virtual void __fastcall DoAfterDeleteRecordEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Data::Db::TDataSet* const DataSet);
	virtual void __fastcall DoBeforeDatasetUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeSynchronizeEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON);
	virtual void __fastcall DoAfterSynchronizeEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const Sgcjson::_di_IsgcObjectJSON JSON);
	
public:
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeNewRecord = {read=FOnBeforeNewRecord, write=FOnBeforeNewRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeUpdateRecord = {read=FOnBeforeUpdateRecord, write=FOnBeforeUpdateRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeRecordEvent OnBeforeDeleteRecord = {read=FOnBeforeDeleteRecord, write=FOnBeforeDeleteRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterNewRecord = {read=FOnAfterNewRecord, write=FOnAfterNewRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterUpdateRecord = {read=FOnAfterUpdateRecord, write=FOnAfterUpdateRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSAfterRecordEvent OnAfterDeleteRecord = {read=FOnAfterDeleteRecord, write=FOnAfterDeleteRecord};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSBeforeDatasetUpdateEvent OnBeforeDatasetUpdate = {read=FOnBeforeDatasetUpdate, write=FOnBeforeDatasetUpdate};
	__property System::Classes::TNotifyEvent OnBeforeSynchronize = {read=FOnBeforeSynchronize, write=SetOnBeforeSynchronize};
	__property System::Classes::TNotifyEvent OnAfterSynchronize = {read=FOnAfterSynchronize, write=FOnAfterSynchronize};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSMetaDataEvent OnMetaData = {read=FOnMetaData, write=FOnMetaData};
	
private:
	bool FNotifyUpdates;
	bool FApplyUpdates;
	bool FAutoEscapeText;
	bool FEncodeBase64;
	Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* FFormatSettings;
	Sgcwebsocket_types::TwsDatasetUpdateMode FUpdateMode;
	void __fastcall SetFormatSettings(Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* const Value);
	
public:
	__property bool NotifyUpdates = {read=FNotifyUpdates, write=FNotifyUpdates, nodefault};
	__property bool ApplyUpdates = {read=FApplyUpdates, write=FApplyUpdates, nodefault};
	__property bool AutoEscapeText = {read=FAutoEscapeText, write=FAutoEscapeText, nodefault};
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	__property Sgcwebsocket_protocol_dataset_message::TsgcWSDatasetFormatSettings* FormatSettings = {read=FFormatSettings, write=SetFormatSettings};
	__property Sgcwebsocket_types::TwsDatasetUpdateMode UpdateMode = {read=FUpdateMode, write=FUpdateMode, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_dataset_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_DATASET_CLIENT)
using namespace Sgcwebsocket_protocol_dataset_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_dataset_clientHPP
