// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCDataBinding.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncdatabindingHPP
#define Fmx_TmsfncdatabindingHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCPersistence.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.Controls.hpp>
#include <System.TypInfo.hpp>
#include <System.Generics.Collections.hpp>
#include <Data.DB.hpp>
#include <System.SysUtils.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncdatabinding
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCDataBinderBase;
typedef System::DelphiInterface<ITMSFNCDataBinderBase> _di_ITMSFNCDataBinderBase;
__interface DELPHIINTERFACE ITMSFNCDataBinderGrid;
typedef System::DelphiInterface<ITMSFNCDataBinderGrid> _di_ITMSFNCDataBinderGrid;
__interface DELPHIINTERFACE ITMSFNCDataBinderSelection;
typedef System::DelphiInterface<ITMSFNCDataBinderSelection> _di_ITMSFNCDataBinderSelection;
class DELPHICLASS TTMSFNCDataBinderDataLink;
class DELPHICLASS TTMSFNCDataBinderPropertyName;
class DELPHICLASS TTMSFNCDataBinderPropertyNames;
class DELPHICLASS TTMSFNCDataBinderFieldName;
class DELPHICLASS TTMSFNCDataBinderFieldNames;
class DELPHICLASS TTMSFNCDataBinderItem;
class DELPHICLASS TTMSFNCDataBinderItems;
class DELPHICLASS TTMSFNCDataBinder;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{778B63C9-34E3-4B65-A6B8-85E3EB1D17C3}") ITMSFNCDataBinderBase  : public System::IInterface 
{
	virtual void __fastcall DataBeginUpdate() = 0 ;
	virtual void __fastcall DataEndUpdate() = 0 ;
};

__interface  INTERFACE_UUID("{D23BDEAA-49B1-451A-9401-0D0D11A9957A}") ITMSFNCDataBinderGrid  : public ITMSFNCDataBinderBase 
{
	virtual void __fastcall SetDataColumnCount(int AValue) = 0 ;
	virtual void __fastcall SetDataRowCount(int AValue) = 0 ;
	virtual void __fastcall ClearData() = 0 ;
	virtual int __fastcall GetDataRowCount() = 0 ;
	virtual void __fastcall SetDataValue(int AColumn, int ARow, System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetDataHeader(int AColumn, System::UnicodeString AValue) = 0 ;
	virtual void __fastcall DataInsertRow(int AInsertPosition) = 0 ;
};

__interface  INTERFACE_UUID("{F7D3F4D2-F202-48C0-9036-3F03E642F1E5}") ITMSFNCDataBinderSelection  : public System::IInterface 
{
	virtual int __fastcall DataGetItemIndex() = 0 ;
	virtual void __fastcall DataSetItemIndex(int AValue) = 0 ;
};

enum DECLSPEC_DENUM TTMSFNCDataBinderDataLinkMode : unsigned char { dlmActiveChanged, dlmDataSetChanged, dlmDataSetScrolled, dlmRecordChanged, dlmUpdateData };

class PASCALIMPLEMENTATION TTMSFNCDataBinderDataLink : public Data::Db::TDataLink
{
	typedef Data::Db::TDataLink inherited;
	
private:
	TTMSFNCDataBinderItem* FDataBinderItem;
	
protected:
	virtual void __fastcall ActiveChanged();
	virtual void __fastcall DataSetChanged();
	virtual void __fastcall DataSetScrolled(int Distance);
	virtual void __fastcall RecordChanged(Data::Db::TField* Field);
	virtual void __fastcall UpdateData();
	
public:
	__fastcall TTMSFNCDataBinderDataLink(TTMSFNCDataBinderItem* ADataBinderItem);
	__fastcall virtual ~TTMSFNCDataBinderDataLink();
};


enum DECLSPEC_DENUM TTMSFNCDataBinderBindType : unsigned char { dbbtSingleValue, dbbtList, dbbtColumnList, dbbtGrid };

class PASCALIMPLEMENTATION TTMSFNCDataBinderPropertyName : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TTMSFNCDataBinderItem* FDataBinderItem;
	System::UnicodeString FValue;
	void __fastcall SetValue(const System::UnicodeString Value);
	
public:
	__fastcall virtual TTMSFNCDataBinderPropertyName(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCDataBinderPropertyName();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TTMSFNCDataBinderItem* DataBinderItem = {read=FDataBinderItem};
	
__published:
	__property System::UnicodeString Value = {read=FValue, write=SetValue};
};


class PASCALIMPLEMENTATION TTMSFNCDataBinderPropertyNames : public Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderPropertyName*>
{
	typedef Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderPropertyName*> inherited;
	
public:
	TTMSFNCDataBinderPropertyName* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCDataBinderItem* FOwner;
	TTMSFNCDataBinderPropertyName* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCDataBinderPropertyName* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetDataBinderPropertyNameClass();
	
public:
	__fastcall TTMSFNCDataBinderPropertyNames(TTMSFNCDataBinderItem* AOwner);
	HIDESBASE TTMSFNCDataBinderPropertyName* __fastcall Add();
	HIDESBASE TTMSFNCDataBinderPropertyName* __fastcall Insert(int index);
	__property TTMSFNCDataBinderPropertyName* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderPropertyNames() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCDataBinderFieldName : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TTMSFNCDataBinderItem* FDataBinderItem;
	System::UnicodeString FValue;
	System::UnicodeString FHTMLTemplate;
	void __fastcall SetValue(const System::UnicodeString Value);
	void __fastcall SetHTMLTemplate(const System::UnicodeString Value);
	
public:
	__fastcall virtual TTMSFNCDataBinderFieldName(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCDataBinderFieldName();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TTMSFNCDataBinderItem* DataBinderItem = {read=FDataBinderItem};
	
__published:
	__property System::UnicodeString Value = {read=FValue, write=SetValue};
	__property System::UnicodeString HTMLTemplate = {read=FHTMLTemplate, write=SetHTMLTemplate};
};


class PASCALIMPLEMENTATION TTMSFNCDataBinderFieldNames : public Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderFieldName*>
{
	typedef Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderFieldName*> inherited;
	
public:
	TTMSFNCDataBinderFieldName* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCDataBinderItem* FOwner;
	TTMSFNCDataBinderFieldName* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCDataBinderFieldName* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetDataBinderFieldNameClass();
	
public:
	__fastcall TTMSFNCDataBinderFieldNames(TTMSFNCDataBinderItem* AOwner);
	HIDESBASE TTMSFNCDataBinderFieldName* __fastcall Add();
	HIDESBASE TTMSFNCDataBinderFieldName* __fastcall Insert(int index);
	__property TTMSFNCDataBinderFieldName* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderFieldNames() { }
	
};


enum DECLSPEC_DENUM TTMSFNCDataBinderFindType : unsigned char { bftDontCare, bftClass, bftString };

class PASCALIMPLEMENTATION TTMSFNCDataBinderItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FDirty;
	bool FUpdating;
	TTMSFNCDataBinder* FDataBinder;
	TTMSFNCDataBinderDataLink* FDataLink;
	bool FActive;
	TTMSFNCDataBinderBindType FBindType;
	System::TObject* FObject;
	System::UnicodeString FPropertyName;
	System::UnicodeString FFieldName;
	TTMSFNCDataBinderPropertyNames* FSubPropertyNames;
	TTMSFNCDataBinderFieldNames* FSubFieldNames;
	System::UnicodeString FColumnsPropertyName;
	System::UnicodeString FColumnsSubPropertyName;
	int FBufferCount;
	System::UnicodeString FBooleanStringValue;
	System::UnicodeString FHTMLTemplate;
	System::UnicodeString FName;
	Data::Db::TDataSource* __fastcall GetDataSource();
	void __fastcall SetDataSource(Data::Db::TDataSource* const Value);
	void __fastcall SetActive(const bool Value);
	void __fastcall SetBindType(const TTMSFNCDataBinderBindType Value);
	void __fastcall SetObject(System::TObject* const Value);
	void __fastcall SetPropertyName(const System::UnicodeString Value);
	void __fastcall SetFieldName(const System::UnicodeString Value);
	void __fastcall SetColumnsPropertyName(const System::UnicodeString Value);
	void __fastcall SetColumnsSubPropertyName(const System::UnicodeString Value);
	void __fastcall SetBufferCount(const int Value);
	System::Classes::TComponent* __fastcall GetBindComponent();
	void __fastcall SetBindComponent(System::Classes::TComponent* const Value);
	void __fastcall SetBooleanStringValue(const System::UnicodeString Value);
	void __fastcall SetHTMLTemplate(const System::UnicodeString Value);
	System::TObject* __fastcall GetObjectInstance();
	void __fastcall SetObjectInstance(System::TObject* const Value);
	
protected:
	void __fastcall DoChanged(System::TObject* Sender);
	__property int BufferCount = {read=FBufferCount, write=SetBufferCount, default=0};
	virtual System::UnicodeString __fastcall HTMLDBReplace(System::UnicodeString AValue, Data::Db::TDataSet* ADataset);
	virtual System::UnicodeString __fastcall GetDisplayName();
	System::Typinfo::PPropInfo __fastcall InternalFindProperty(System::TObject* AObject, System::UnicodeString AName, TTMSFNCDataBinderFindType AFindType = (TTMSFNCDataBinderFindType)(0x0));
	System::Typinfo::PPropInfo __fastcall FindProperty(System::TObject* &AObject, System::UnicodeString AName, int AIndex = 0xffffffff, TTMSFNCDataBinderFindType AFindType = (TTMSFNCDataBinderFindType)(0x0));
	virtual void __fastcall DoBeforeWriteFieldToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);
	virtual void __fastcall DoAfterWriteFieldToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);
	virtual void __fastcall DoBeforeWritePropertyToField(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);
	virtual void __fastcall DoAfterWritePropertyToField(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);
	virtual void __fastcall DoBeforeWriteHTMLTemplateToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString &AHTMLTemplate, bool &AAllow);
	virtual void __fastcall DoAfterWriteHTMLTemplateToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString AHTMLTemplate);
	virtual void __fastcall WriteFieldToProperty(System::TObject* AObject, System::Typinfo::PPropInfo APropInfo, Data::Db::TField* AField);
	virtual void __fastcall WritePropertyToField(System::TObject* AObject, System::Typinfo::PPropInfo APropInfo, Data::Db::TField* AField);
	virtual void __fastcall WriteHTMLTemplateToProperty(System::TObject* AObject, System::Typinfo::PPropInfo APropInfo, System::UnicodeString AHTMLTemplate);
	void __fastcall ClearProperty(System::TObject* AObject, System::Typinfo::PPropInfo APropInfo);
	
public:
	__fastcall virtual TTMSFNCDataBinderItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCDataBinderItem();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	bool __fastcall CheckDataSet();
	int __fastcall GetRecordCount();
	__property TTMSFNCDataBinderDataLink* DataLink = {read=FDataLink};
	void __fastcall UpdateDataLink(TTMSFNCDataBinderDataLinkMode AMode, int ADistance = 0xffffffff, Data::Db::TField* AField = (Data::Db::TField*)(0x0));
	void __fastcall WriteValueToField();
	__property System::TObject* Object = {read=FObject, write=SetObject};
	__property System::TObject* ObjectInstance = {read=GetObjectInstance, write=SetObjectInstance};
	bool __fastcall DataSetLocate(System::UnicodeString AKeyFieldNames, System::UnicodeString AKeyValues);
	bool __fastcall DataSetIsEditing();
	bool __fastcall DataSetIsInserting();
	void __fastcall DataSetEdit();
	void __fastcall DataSetPost();
	void __fastcall DataSetInsert();
	void __fastcall DataSetDelete();
	void __fastcall DataSetCancel();
	void __fastcall DataSetEnableControls();
	void __fastcall DataSetDisableControls();
	bool __fastcall DataSetCanModify();
	
__published:
	__property System::Classes::TComponent* BindComponent = {read=GetBindComponent, write=SetBindComponent};
	__property System::UnicodeString PropertyName = {read=FPropertyName, write=SetPropertyName};
	__property System::UnicodeString ColumnsPropertyName = {read=FColumnsPropertyName, write=SetColumnsPropertyName};
	__property System::UnicodeString ColumnsSubPropertyName = {read=FColumnsSubPropertyName, write=SetColumnsSubPropertyName};
	__property TTMSFNCDataBinderPropertyNames* SubPropertyNames = {read=FSubPropertyNames, write=FSubPropertyNames};
	__property TTMSFNCDataBinderFieldNames* SubFieldNames = {read=FSubFieldNames, write=FSubFieldNames};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool Active = {read=FActive, write=SetActive, default=1};
	__property TTMSFNCDataBinderBindType BindType = {read=FBindType, write=SetBindType, default=0};
	__property System::UnicodeString FieldName = {read=FFieldName, write=SetFieldName};
	__property System::UnicodeString HTMLTemplate = {read=FHTMLTemplate, write=SetHTMLTemplate};
	__property System::UnicodeString BooleanStringValue = {read=FBooleanStringValue, write=SetBooleanStringValue};
	__property System::UnicodeString Name = {read=FName, write=FName};
};


class PASCALIMPLEMENTATION TTMSFNCDataBinderItems : public Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderItem*>
{
	typedef Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCDataBinderItem*> inherited;
	
public:
	TTMSFNCDataBinderItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCDataBinder* FOwner;
	TTMSFNCDataBinderItem* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCDataBinderItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetDataBinderItemClass();
	
public:
	__fastcall TTMSFNCDataBinderItems(TTMSFNCDataBinder* AOwner);
	HIDESBASE TTMSFNCDataBinderItem* __fastcall Add();
	HIDESBASE TTMSFNCDataBinderItem* __fastcall Insert(int index);
	__property TTMSFNCDataBinderItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderItems() { }
	
};


typedef System::DynamicArray<System::UnicodeString> TTMSFNCDataBinderStringArray;

typedef System::DynamicArray<System::Typinfo::PPropInfo> TTMSFNCDataBinderPropertyList;

typedef void __fastcall (__closure *TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);

typedef void __fastcall (__closure *TTMSFNCDataBinderAfterWriteFieldToPropertyEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);

typedef void __fastcall (__closure *TTMSFNCDataBinderBeforeWritePropertyToFieldEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);

typedef void __fastcall (__closure *TTMSFNCDataBinderAfterWritePropertyToFieldEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);

typedef void __fastcall (__closure *TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString &AHTMLTemplate, bool &AAllow);

typedef void __fastcall (__closure *TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent)(System::TObject* Sender, System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString AHTMLTemplate);

class PASCALIMPLEMENTATION TTMSFNCDataBinder : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	int FUpdateCount;
	TTMSFNCDataBinderItems* FItems;
	bool FActive;
	TTMSFNCDataBinderAfterWriteFieldToPropertyEvent FOnAfterWriteFieldToProperty;
	TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent FOnBeforeWriteFieldToProperty;
	TTMSFNCDataBinderAfterWritePropertyToFieldEvent FOnAfterWritePropertyToField;
	TTMSFNCDataBinderBeforeWritePropertyToFieldEvent FOnBeforeWritePropertyToField;
	TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent FOnAfterWriteHTMLTemplateToProperty;
	TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent FOnBeforeWriteHTMLTemplateToProperty;
	void __fastcall SetItems(TTMSFNCDataBinderItems* const Value);
	TTMSFNCDataBinderItems* __fastcall GetItems();
	void __fastcall SetActive(const bool Value);
	TTMSFNCDataBinderItem* __fastcall GetItemByName(System::UnicodeString AName);
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetVersion();
	virtual TTMSFNCDataBinderItems* __fastcall CreateItems();
	virtual void __fastcall DoBeforeWriteFieldToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);
	virtual void __fastcall DoAfterWriteFieldToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);
	virtual void __fastcall DoBeforeWritePropertyToField(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField, bool &AAllow);
	virtual void __fastcall DoAfterWritePropertyToField(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Data::Db::TField* AField);
	virtual void __fastcall DoBeforeWriteHTMLTemplateToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString &AHTMLTemplate, bool &AAllow);
	virtual void __fastcall DoAfterWriteHTMLTemplateToProperty(System::TObject* AObject, TTMSFNCDataBinderItem* AItem, System::Typinfo::PPropInfo APropertyInfo, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, System::UnicodeString AHTMLTemplate);
	virtual void __fastcall RegisterRuntimeClasses();
	void __fastcall UpdateDataLinks();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	
public:
	__fastcall virtual TTMSFNCDataBinder()/* overload */;
	__fastcall virtual TTMSFNCDataBinder(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCDataBinder();
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	int __fastcall ItemCount();
	__property TTMSFNCDataBinderItem* ItemByName[System::UnicodeString AName] = {read=GetItemByName};
	void __fastcall GetDataSources(System::Classes::TStrings* AStrings);
	__classmethod TTMSFNCDataBinderPropertyList __fastcall GetPropertyList(System::TObject* AObject);
	__classmethod void __fastcall BlobFieldToPicture(Data::Db::TBlobField* AField, Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	bool __fastcall IsConnected(System::TObject* AObject);
	TTMSFNCDataBinderItem* __fastcall ConnectSingleHTMLTemplate(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, System::UnicodeString AHTMLTemplate);
	TTMSFNCDataBinderItem* __fastcall ConnectSingle(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, System::UnicodeString AFieldName);
	TTMSFNCDataBinderItem* __fastcall ConnectListHTMLTemplate(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, TTMSFNCDataBinderStringArray ASubPropertyNames, TTMSFNCDataBinderStringArray ASubHTMLTemplates)/* overload */;
	TTMSFNCDataBinderItem* __fastcall ConnectList(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, TTMSFNCDataBinderStringArray ASubPropertyNames, TTMSFNCDataBinderStringArray ASubFieldNames)/* overload */;
	TTMSFNCDataBinderItem* __fastcall ConnectListHTMLTemplate(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, System::UnicodeString AHTMLTemplate)/* overload */;
	TTMSFNCDataBinderItem* __fastcall ConnectList(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, System::UnicodeString AFieldName)/* overload */;
	TTMSFNCDataBinderItem* __fastcall ConnectColumnList(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString AListPropertyName, System::UnicodeString AColumnsPropertyName, System::UnicodeString AColumnsSubPropertyName, System::UnicodeString AListSubPropertyName);
	TTMSFNCDataBinderItem* __fastcall Connect(System::TObject* AObject, Data::Db::TDataSource* ADataSource, System::UnicodeString APropertyName, System::UnicodeString AFieldName = System::UnicodeString(), System::UnicodeString AHTMLTemplate = System::UnicodeString(), TTMSFNCDataBinderStringArray ASubPropertyNames = TTMSFNCDataBinderStringArray(), TTMSFNCDataBinderStringArray ASubFieldNames = TTMSFNCDataBinderStringArray(), TTMSFNCDataBinderStringArray ASubHTMLTemplates = TTMSFNCDataBinderStringArray(), TTMSFNCDataBinderBindType ABindType = (TTMSFNCDataBinderBindType)(0x0));
	TTMSFNCDataBinderItem* __fastcall ConnectGrid(System::TObject* AObject, Data::Db::TDataSource* ADatasource);
	void __fastcall StartEditor(System::TObject* const AObject = (System::TObject*)(0x0));
	
__published:
	__property TTMSFNCDataBinderItems* Items = {read=GetItems, write=SetItems};
	__property System::UnicodeString Version = {read=GetVersion};
	__property bool Active = {read=FActive, write=SetActive, default=0};
	__property TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent OnBeforeWriteFieldToProperty = {read=FOnBeforeWriteFieldToProperty, write=FOnBeforeWriteFieldToProperty};
	__property TTMSFNCDataBinderAfterWriteFieldToPropertyEvent OnAfterWriteFieldToProperty = {read=FOnAfterWriteFieldToProperty, write=FOnAfterWriteFieldToProperty};
	__property TTMSFNCDataBinderBeforeWritePropertyToFieldEvent OnBeforeWritePropertyToField = {read=FOnBeforeWritePropertyToField, write=FOnBeforeWritePropertyToField};
	__property TTMSFNCDataBinderAfterWritePropertyToFieldEvent OnAfterWritePropertyToField = {read=FOnAfterWritePropertyToField, write=FOnAfterWritePropertyToField};
	__property TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent OnBeforeWriteHTMLTemplateToProperty = {read=FOnBeforeWriteHTMLTemplateToProperty, write=FOnBeforeWriteHTMLTemplateToProperty};
	__property TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent OnAfterWriteHTMLTemplateToProperty = {read=FOnAfterWriteHTMLTemplateToProperty, write=FOnAfterWriteHTMLTemplateToProperty};
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x1);
}	/* namespace Tmsfncdatabinding */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCDATABINDING)
using namespace Fmx::Tmsfncdatabinding;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncdatabindingHPP
