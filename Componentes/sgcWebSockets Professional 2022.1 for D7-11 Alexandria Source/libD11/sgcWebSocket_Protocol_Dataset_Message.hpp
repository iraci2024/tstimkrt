// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Dataset_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_dataset_messageHPP
#define Sgcwebsocket_protocol_dataset_messageHPP

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
#include <System.Variants.hpp>
#include <System.TypInfo.hpp>
#include <System.StrUtils.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_dataset_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSDatasetFormatSettings;
class DELPHICLASS TsgcWSMessageDataset;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Variant> TsgcFields;

typedef void __fastcall (__closure *TsgcWSMetaDataEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const Sgcjson::_di_IsgcObjectJSON JSON);

typedef void __fastcall (__closure *TsgcWSBeforeRecordEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);

typedef void __fastcall (__closure *TsgcWSAfterRecordEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Data::Db::TDataSet* const Dataset);

typedef void __fastcall (__closure *TsgcWSBeforeDatasetUpdateEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);

typedef void __fastcall (__closure *TsgcWSSynchronizeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const Sgcjson::_di_IsgcObjectJSON JSON);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSDatasetFormatSettings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::WideChar FDateSeparator;
	System::WideChar FDecimalSeparator;
	System::UnicodeString FShortDateFormat;
	System::WideChar FThousandSeparator;
	System::WideChar FTimeSeparator;
	
public:
	__fastcall virtual TsgcWSDatasetFormatSettings();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::WideChar DateSeparator = {read=FDateSeparator, write=FDateSeparator, nodefault};
	__property System::WideChar DecimalSeparator = {read=FDecimalSeparator, write=FDecimalSeparator, nodefault};
	__property System::UnicodeString ShortDateFormat = {read=FShortDateFormat, write=FShortDateFormat};
	__property System::WideChar ThousandSeparator = {read=FThousandSeparator, write=FThousandSeparator, nodefault};
	__property System::WideChar TimeSeparator = {read=FTimeSeparator, write=FTimeSeparator, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSDatasetFormatSettings() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMessageDataset : public Sgcwebsocket_protocol_sgc_message::TsgcWSMessage
{
	typedef Sgcwebsocket_protocol_sgc_message::TsgcWSMessage inherited;
	
private:
	System::UnicodeString __fastcall Escape(const System::UnicodeString aText);
	System::UnicodeString __fastcall UnEscape(const System::UnicodeString aText);
	System::UnicodeString __fastcall GetBlobAsString(Data::Db::TField* aField);
	Data::Db::TDataSetNotifyEvent FDSBeforeInsertEvent;
	Data::Db::TDataSetNotifyEvent FDSBeforeEditEvent;
	Data::Db::TDataSetNotifyEvent FDSAfterPostEvent;
	TsgcFields FFieldsValue;
	System::Classes::TStringList* FDeltaFields;
	
protected:
	void __fastcall OnBeforeInsertEvent(Data::Db::TDataSet* DataSet);
	void __fastcall OnBeforeEditEvent(Data::Db::TDataSet* DataSet);
	void __fastcall OnAfterPostEvent(Data::Db::TDataSet* DataSet);
	System::Classes::TStringList* __fastcall GetDeltaFields();
	
private:
	Data::Db::TDataSet* FDataSet;
	
protected:
	virtual void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	System::UnicodeString __fastcall GetDatasetName();
	
public:
	__property Data::Db::TDataSet* DataSet = {read=FDataSet, write=SetDataSet};
	
private:
	System::Sysutils::TFormatSettings FFS;
	TsgcWSDatasetFormatSettings* FFormatSettings;
	TsgcWSDatasetFormatSettings* __fastcall GetFormatSettings();
	void __fastcall SetFormatSettings(TsgcWSDatasetFormatSettings* const Value);
	
protected:
	System::Sysutils::TFormatSettings __fastcall GetFS();
	
public:
	__property TsgcWSDatasetFormatSettings* FormatSettings = {read=GetFormatSettings, write=SetFormatSettings};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoJSONRecord();
	virtual bool __fastcall FindRecord();
	virtual void __fastcall DoAssignRecord();
	virtual void __fastcall DoDeleteRecord();
	virtual void __fastcall DoNewRecord();
	virtual void __fastcall DoUpdateRecord();
	virtual void __fastcall DoRefreshRecord();
	virtual bool __fastcall DoBeforeDatasetUpdate();
	bool __fastcall DatasetNodeExists(const System::UnicodeString aFieldName);
	System::Variant __fastcall GetJSONValue(const System::UnicodeString aFieldName);
	
public:
	HIDESBASE void __fastcall Read(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aMessage)/* overload */;
	virtual System::UnicodeString __fastcall Write();
	
private:
	Data::Db::TFields* FKeyFields;
	void __fastcall DoFreeKeyFields();
	void __fastcall DoAddKeyField(Data::Db::TField* aField);
	Data::Db::TFields* __fastcall GetKeyFields();
	
protected:
	void __fastcall DoKeyFields();
	System::UnicodeString __fastcall GetKeyFieldsAsString();
	System::Variant __fastcall GetKeyValuesAsVariant();
	
public:
	__property Data::Db::TFields* KeyFields = {read=GetKeyFields, write=FKeyFields};
	
protected:
	virtual void __fastcall DoReadBeforeSynchronize();
	virtual void __fastcall DoReadAfterSynchronize();
	virtual void __fastcall DoWriteBeforeSynchronize();
	virtual void __fastcall DoWriteAfterSynchronize();
	virtual void __fastcall DoWriteMetaData();
	virtual void __fastcall DoReadMetaData();
	
public:
	__fastcall virtual TsgcWSMessageDataset(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMessageDataset();
	
private:
	Sgcwebsocket_types::TwsDatasetUpdateMode FUpdateMode;
	bool FAutoEscapeText;
	bool FEncodeBase64;
	
public:
	__property Sgcwebsocket_types::TwsDatasetUpdateMode UpdateMode = {read=FUpdateMode, write=FUpdateMode, nodefault};
	__property bool AutoEscapeText = {read=FAutoEscapeText, write=FAutoEscapeText, nodefault};
	__property bool EncodeBase64 = {read=FEncodeBase64, write=FEncodeBase64, nodefault};
	
private:
	TsgcWSBeforeRecordEvent FOnBeforeNewRecord;
	TsgcWSBeforeRecordEvent FOnBeforeDeleteRecord;
	TsgcWSBeforeRecordEvent FOnBeforeUpdateRecord;
	TsgcWSAfterRecordEvent FOnAfterDeleteRecord;
	TsgcWSAfterRecordEvent FOnAfterNewRecord;
	TsgcWSAfterRecordEvent FOnAfterUpdateRecord;
	TsgcWSMetaDataEvent FOnMetaData;
	TsgcWSBeforeDatasetUpdateEvent FOnBeforeDatasetUpdate;
	TsgcWSSynchronizeEvent FOnBeforeSynchronize;
	TsgcWSSynchronizeEvent FOnAfterSynchronize;
	
protected:
	virtual void __fastcall DoBeforeNewRecordEvent(Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeUpdateRecordEvent(Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoBeforeDeleteRecordEvent(Data::Db::TDataSet* const Dataset, const Sgcjson::_di_IsgcObjectJSON JSON, bool &Handled);
	virtual void __fastcall DoAfterNewRecordEvent();
	virtual void __fastcall DoAfterUpdateRecordEvent();
	virtual void __fastcall DoAfterDeleteRecordEvent();
	virtual void __fastcall DoMetaDataEvent(const Sgcjson::_di_IsgcObjectJSON JSON);
	virtual void __fastcall DoBeforeSynchronizeEvent(const Sgcjson::_di_IsgcObjectJSON JSON);
	virtual void __fastcall DoAfterSynchronizeEvent(const Sgcjson::_di_IsgcObjectJSON JSON);
	
public:
	__property TsgcWSBeforeRecordEvent OnBeforeNewRecord = {read=FOnBeforeNewRecord, write=FOnBeforeNewRecord};
	__property TsgcWSBeforeRecordEvent OnBeforeUpdateRecord = {read=FOnBeforeUpdateRecord, write=FOnBeforeUpdateRecord};
	__property TsgcWSBeforeRecordEvent OnBeforeDeleteRecord = {read=FOnBeforeDeleteRecord, write=FOnBeforeDeleteRecord};
	__property TsgcWSAfterRecordEvent OnAfterDeleteRecord = {read=FOnAfterDeleteRecord, write=FOnAfterDeleteRecord};
	__property TsgcWSAfterRecordEvent OnAfterNewRecord = {read=FOnAfterNewRecord, write=FOnAfterNewRecord};
	__property TsgcWSAfterRecordEvent OnAfterUpdateRecord = {read=FOnAfterUpdateRecord, write=FOnAfterUpdateRecord};
	__property TsgcWSMetaDataEvent OnMetaData = {read=FOnMetaData, write=FOnMetaData};
	__property TsgcWSSynchronizeEvent OnBeforeSynchronize = {read=FOnBeforeSynchronize, write=FOnBeforeSynchronize};
	__property TsgcWSSynchronizeEvent OnAfterSynchronize = {read=FOnAfterSynchronize, write=FOnAfterSynchronize};
	__property TsgcWSBeforeDatasetUpdateEvent OnBeforeDatasetUpdate = {read=FOnBeforeDatasetUpdate, write=FOnBeforeDatasetUpdate};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_dataset_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_DATASET_MESSAGE)
using namespace Sgcwebsocket_protocol_dataset_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_dataset_messageHPP
