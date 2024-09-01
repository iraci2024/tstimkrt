// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPersistence.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpersistenceHPP
#define Fmx_TmsfncpersistenceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <System.Variants.hpp>
#include <System.SysUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCJSONReader.hpp>
#include <FMX.TMSFNCJSONWriter.hpp>
#include <System.JSON.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpersistence
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCPersistence;
typedef System::DelphiInterface<ITMSFNCPersistence> _di_ITMSFNCPersistence;
__interface DELPHIINTERFACE ITMSFNCBaseListIO;
typedef System::DelphiInterface<ITMSFNCBaseListIO> _di_ITMSFNCBaseListIO;
__interface DELPHIINTERFACE ITMSFNCBasePersistenceIO;
typedef System::DelphiInterface<ITMSFNCBasePersistenceIO> _di_ITMSFNCBasePersistenceIO;
__interface DELPHIINTERFACE ITMSFNCBaseCollectionIO;
typedef System::DelphiInterface<ITMSFNCBaseCollectionIO> _di_ITMSFNCBaseCollectionIO;
__interface DELPHIINTERFACE ITMSFNCPersistenceIO;
typedef System::DelphiInterface<ITMSFNCPersistenceIO> _di_ITMSFNCPersistenceIO;
class DELPHICLASS ETMSFNCReaderException;
class DELPHICLASS TTMSFNCObjectDictionary;
class DELPHICLASS TTMSFNCObjectList;
class DELPHICLASS TTMSFNCStringList;
class DELPHICLASS TTMSFNCIntegerList;
class DELPHICLASS TTMSFNCDoubleList;
class DELPHICLASS TTMSFNCWriter;
class DELPHICLASS TTMSFNCObjectReference;
class DELPHICLASS TTMSFNCReader;
class DELPHICLASS TTMSFNCObjectPersistence;
class DELPHICLASS TTMSFNCPersistence;
struct TTMSFNCJSONToClassOptions;
__interface DELPHIINTERFACE TTMSFNCJSONToClassExportEvent;
typedef System::DelphiInterface<TTMSFNCJSONToClassExportEvent> _di_TTMSFNCJSONToClassExportEvent;
class DELPHICLASS TTMSFNCJSONToClassProperty;
class DELPHICLASS TTMSFNCJSONToClassProperties;
class DELPHICLASS TTMSFNCJSONToClassItem;
class DELPHICLASS TTMSFNCJSONToClassItems;
class DELPHICLASS TTMSFNCJSONToClass;
//-- type declarations -------------------------------------------------------
typedef System::Classes::TStream TStreamEx;

__interface  INTERFACE_UUID("{363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}") ITMSFNCPersistence  : public System::IInterface 
{
	virtual void __fastcall SaveSettingsToFile(System::UnicodeString AFileName, bool AAppearanceOnly = false) = 0 ;
	virtual void __fastcall LoadSettingsFromFile(System::UnicodeString AFileName) = 0 ;
	virtual void __fastcall SaveSettingsToStream(System::Classes::TStream* AStream, bool AAppearanceOnly = false) = 0 ;
	virtual void __fastcall LoadSettingsFromStream(System::Classes::TStream* AStream) = 0 ;
	virtual bool __fastcall CanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType) = 0 ;
	virtual bool __fastcall CanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType) = 0 ;
};

__interface  INTERFACE_UUID("{FAB1D21E-D798-4CE0-B17B-9D75E4456AB4}") ITMSFNCBaseListIO  : public System::IInterface 
{
	virtual System::TClass __fastcall GetItemClass() = 0 ;
};

__interface  INTERFACE_UUID("{91DEAFC3-8932-45F4-A3ED-5AAA0C0E9250}") ITMSFNCBasePersistenceIO  : public System::IInterface 
{
	virtual System::TObject* __fastcall CreateObject(const System::UnicodeString AClassName, const System::TClass ABaseClass) = 0 ;
};

__interface  INTERFACE_UUID("{90FDF257-7362-411D-B7F6-E2BEE2265016}") ITMSFNCBaseCollectionIO  : public System::IInterface 
{
	virtual System::Classes::TCollectionItem* __fastcall AddItem(System::TObject* const AObject) = 0 ;
};

__interface  INTERFACE_UUID("{11B625F8-447A-4AE5-BB88-5ECDEA979AF7}") ITMSFNCPersistenceIO  : public ITMSFNCBasePersistenceIO 
{
	virtual bool __fastcall NeedsObjectReference(const System::TClass AClass) = 0 ;
	virtual System::UnicodeString __fastcall GetObjectReference(System::TObject* const AObject) = 0 ;
	virtual System::TObject* __fastcall FindObject(const System::UnicodeString AReference) = 0 ;
	virtual void __fastcall FixOwners(System::TObject* const AObject, System::TObject* const AObjectList) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION ETMSFNCReaderException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ETMSFNCReaderException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ETMSFNCReaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ETMSFNCReaderException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ETMSFNCReaderException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ETMSFNCReaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ETMSFNCReaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ETMSFNCReaderException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ETMSFNCReaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETMSFNCReaderException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETMSFNCReaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETMSFNCReaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETMSFNCReaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ETMSFNCReaderException() { }
	
};

#pragma pack(pop)

typedef System::Typinfo::PPropInfo TTMSFNCPropertyInfo;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCObjectDictionary : public System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>
{
	typedef System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*> inherited;
	
public:
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary()/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>() { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(int ACapacity)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(ACapacity) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(int ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(ACapacity, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,System::TObject*> >* const Collection)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(Collection) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,System::TObject*> >* const Collection, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(Collection, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(const System::Generics::Collections::TPair__2<System::UnicodeString,System::TObject*> *AItems, const int AItems_High)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(AItems, AItems_High) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Create */ inline __fastcall TTMSFNCObjectDictionary(const System::Generics::Collections::TPair__2<System::UnicodeString,System::TObject*> *AItems, const int AItems_High, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>(AItems, AItems_High, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_TObject>.Destroy */ inline __fastcall virtual ~TTMSFNCObjectDictionary() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCObjectList : public System::Generics::Collections::TObjectList__1<System::TObject*>
{
	typedef System::Generics::Collections::TObjectList__1<System::TObject*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList()/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>() { }
	/* {System_Generics_Collections}TObjectList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::TObject*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(System::Generics::Collections::TEnumerable__1<System::TObject*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_TObject>.Destroy */ inline __fastcall virtual ~TTMSFNCObjectList() { }
	
public:
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::TObject*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(AComparer) { }
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(System::Generics::Collections::TEnumerable__1<System::TObject*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(Collection) { }
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCObjectList(System::TObject* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<System::TObject*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStringList : public System::Generics::Collections::TList__1<System::UnicodeString>
{
	typedef System::Generics::Collections::TList__1<System::UnicodeString> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_string>.Create */ inline __fastcall TTMSFNCStringList()/* overload */ : System::Generics::Collections::TList__1<System::UnicodeString>() { }
	/* {System_Generics_Collections}TList<System_string>.Create */ inline __fastcall TTMSFNCStringList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TList__1<System::UnicodeString>(AComparer) { }
	/* {System_Generics_Collections}TList<System_string>.Create */ inline __fastcall TTMSFNCStringList(System::Generics::Collections::TEnumerable__1<System::UnicodeString>* const Collection)/* overload */ : System::Generics::Collections::TList__1<System::UnicodeString>(Collection) { }
	/* {System_Generics_Collections}TList<System_string>.Create */ inline __fastcall TTMSFNCStringList(const System::UnicodeString *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<System::UnicodeString>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_string>.Destroy */ inline __fastcall virtual ~TTMSFNCStringList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCIntegerList : public System::Generics::Collections::TList__1<int>
{
	typedef System::Generics::Collections::TList__1<int> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCIntegerList()/* overload */ : System::Generics::Collections::TList__1<int>() { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCIntegerList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<int> > AComparer)/* overload */ : System::Generics::Collections::TList__1<int>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCIntegerList(System::Generics::Collections::TEnumerable__1<int>* const Collection)/* overload */ : System::Generics::Collections::TList__1<int>(Collection) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCIntegerList(const int *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<int>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_Integer>.Destroy */ inline __fastcall virtual ~TTMSFNCIntegerList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDoubleList : public System::Generics::Collections::TList__1<double>
{
	typedef System::Generics::Collections::TList__1<double> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_Double>.Create */ inline __fastcall TTMSFNCDoubleList()/* overload */ : System::Generics::Collections::TList__1<double>() { }
	/* {System_Generics_Collections}TList<System_Double>.Create */ inline __fastcall TTMSFNCDoubleList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<double> > AComparer)/* overload */ : System::Generics::Collections::TList__1<double>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Double>.Create */ inline __fastcall TTMSFNCDoubleList(System::Generics::Collections::TEnumerable__1<double>* const Collection)/* overload */ : System::Generics::Collections::TList__1<double>(Collection) { }
	/* {System_Generics_Collections}TList<System_Double>.Create */ inline __fastcall TTMSFNCDoubleList(const double *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<double>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_Double>.Destroy */ inline __fastcall virtual ~TTMSFNCDoubleList() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<System::TObject*> TTMSFNCObjectArray;

typedef void __fastcall (__closure *TTMSFNCWriterCustomWritePropertyEvent)(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* AWriter, bool &ACanWrite);

typedef void __fastcall (__closure *TTMSFNCWriterCustomIsAssignablePropertyEvent)(System::TObject* AObject, System::UnicodeString APropertyName, bool &AIsAssignable);

typedef System::DynamicArray<System::UnicodeString> TTMSFNCExcludePropertyListArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* FWriter;
	System::TObject* FIOReference;
	TTMSFNCWriterCustomWritePropertyEvent FOnCustomWriteProperty;
	System::TObject* FRootObject;
	TTMSFNCExcludePropertyListArray FExcludeProperties;
	TTMSFNCWriterCustomIsAssignablePropertyEvent FOnCustomIsAssignableProperty;
	void __fastcall SetRootObject(System::TObject* const Value);
	void __fastcall SetExcludeProperties(const TTMSFNCExcludePropertyListArray Value);
	void __fastcall SetIOReference(System::TObject* const Value);
	__property Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* Writer = {read=FWriter};
	void __fastcall WritePropInfoValue(System::TObject* AInstance, const System::Typinfo::PPropInfo APropInfo);
	void __fastcall WriteProperties(System::TObject* AObject);
	void __fastcall WriteProperty(System::TObject* AObject, System::Typinfo::PPropInfo AProp);
	void __fastcall WriteGenericObjectList(TTMSFNCObjectList* AList);
	void __fastcall WriteGenericStringList(TTMSFNCStringList* AList);
	void __fastcall WriteGenericIntegerList(TTMSFNCIntegerList* AList);
	void __fastcall WriteGenericDoubleList(TTMSFNCDoubleList* AList);
	void __fastcall WriteStrings(System::Classes::TStrings* AList);
	void __fastcall WriteGenericDictionary(TTMSFNCObjectDictionary* ADictionary);
	void __fastcall WriteCollection(System::Classes::TCollection* ACollection);
	void __fastcall WriteList(System::Classes::TList* AList);
	void __fastcall WriteBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	void __fastcall WriteSingleObject(System::TObject* AObject);
	void __fastcall WriteObject(System::TObject* AObject);
	
public:
	__fastcall TTMSFNCWriter(System::Classes::TStream* AStream);
	__fastcall virtual ~TTMSFNCWriter();
	void __fastcall Write(System::TObject* AObject);
	void __fastcall WriteArray(System::UnicodeString AName, TTMSFNCObjectArray AArray);
	__property Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* JSONWriter = {read=FWriter};
	__property System::TObject* IOReference = {read=FIOReference, write=SetIOReference};
	__property System::TObject* RootObject = {read=FRootObject, write=SetRootObject};
	__property TTMSFNCExcludePropertyListArray ExcludeProperties = {read=FExcludeProperties, write=SetExcludeProperties};
	__property TTMSFNCWriterCustomWritePropertyEvent OnCustomWriteProperty = {read=FOnCustomWriteProperty, write=FOnCustomWriteProperty};
	__property TTMSFNCWriterCustomIsAssignablePropertyEvent OnCustomIsAssignableProperty = {read=FOnCustomIsAssignableProperty, write=FOnCustomIsAssignableProperty};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCReaderCustomReadPropertyEvent)(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Fmx::Tmsfncjsonreader::TTMSFNCJSONReader* AReader, bool &ACanRead);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCObjectReference : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::TObject* Instance;
	System::Typinfo::TPropInfo *Prop;
	System::UnicodeString Id;
	__fastcall TTMSFNCObjectReference(System::TObject* AInstance, System::Typinfo::PPropInfo AProp, const System::UnicodeString AId);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCObjectReference() { }
	
};

#pragma pack(pop)

typedef System::Generics::Collections::TObjectList__1<TTMSFNCObjectReference*>* TTMSFNCObjectReferences;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCReader : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Generics::Collections::TObjectList__1<TTMSFNCObjectReference*>* FReferences;
	Fmx::Tmsfncjsonreader::TTMSFNCJSONReader* FReader;
	System::TObject* FIOReference;
	TTMSFNCReaderCustomReadPropertyEvent FOnCustomReadProperty;
	System::TObject* FRootObject;
	TTMSFNCExcludePropertyListArray FExcludeProperties;
	TTMSFNCWriterCustomIsAssignablePropertyEvent FOnCustomIsAssignableProperty;
	System::TObject* __fastcall ReadSingleObject(System::TClass ABaseClass)/* overload */;
	void __fastcall SetRootObject(System::TObject* const Value);
	void __fastcall SetExcludeProperties(const TTMSFNCExcludePropertyListArray Value);
	void __fastcall SetIOReference(System::TObject* const Value);
	__property Fmx::Tmsfncjsonreader::TTMSFNCJSONReader* Reader = {read=FReader};
	void __fastcall ReadSingleObject(System::TObject* AObject)/* overload */;
	void __fastcall ReadProperties(System::TObject* AObject);
	void __fastcall ReadProperty(System::TObject* AObject, System::Typinfo::PPropInfo AProp);
	void __fastcall ReadPropInfoValue(System::TObject* AInstance, const System::Typinfo::PPropInfo APropInfo);
	void __fastcall ReadExistingObject(System::TObject* AObject);
	void __fastcall ReadGenericStringList(TTMSFNCStringList* AList);
	void __fastcall ReadGenericDoubleList(TTMSFNCDoubleList* AList);
	void __fastcall ReadGenericIntegerList(TTMSFNCIntegerList* AList);
	void __fastcall ReadStrings(System::Classes::TStrings* AList);
	void __fastcall ReadGenericObjectList(TTMSFNCObjectList* AList);
	void __fastcall ReadGenericDictionary(TTMSFNCObjectDictionary* ADictionary);
	void __fastcall ReadCollection(System::Classes::TCollection* ACollection);
	void __fastcall ReadList(System::Classes::TList* AList);
	void __fastcall ReadBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	void __fastcall ReadObject(System::TObject* AObject);
	
public:
	__fastcall TTMSFNCReader(System::Classes::TStream* AStream);
	__fastcall virtual ~TTMSFNCReader();
	System::TObject* __fastcall Read(System::TClass AClass)/* overload */;
	void __fastcall Read(System::TObject* AObject)/* overload */;
	TTMSFNCObjectArray __fastcall ReadArray(System::UnicodeString AName)/* overload */;
	__property Fmx::Tmsfncjsonreader::TTMSFNCJSONReader* JSONReader = {read=FReader};
	__property System::TObject* IOReference = {read=FIOReference, write=SetIOReference};
	__property System::TObject* RootObject = {read=FRootObject, write=SetRootObject};
	__property TTMSFNCExcludePropertyListArray ExcludeProperties = {read=FExcludeProperties, write=SetExcludeProperties};
	__property TTMSFNCReaderCustomReadPropertyEvent OnCustomReadProperty = {read=FOnCustomReadProperty, write=FOnCustomReadProperty};
	__property TTMSFNCWriterCustomIsAssignablePropertyEvent OnCustomIsAssignableProperty = {read=FOnCustomIsAssignableProperty, write=FOnCustomIsAssignableProperty};
	void __fastcall SolveReferences();
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

using System::Typinfo::PTypeInfo;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCObjectPersistence : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::UnicodeString __fastcall SaveObjectToString(System::TObject* AObject);
	__classmethod void __fastcall LoadObjectFromString(System::TObject* AObject, System::UnicodeString AString);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCObjectPersistence() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCObjectPersistence() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPersistence : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static System::UnicodeString ClassTypeVariable;
	
private:
	static TTMSFNCReaderCustomReadPropertyEvent FOnCustomReadProperty;
	static TTMSFNCWriterCustomWritePropertyEvent FOnCustomWriteProperty;
	static System::TObject* FRootObject;
	static TTMSFNCExcludePropertyListArray FExcludeProperties;
	static System::TObject* FIOReference;
	__classmethod void __fastcall DoCustomReadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Fmx::Tmsfncjsonreader::TTMSFNCJSONReader* AReader, bool &ACanRead);
	__classmethod void __fastcall DoCustomWriteProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* AWriter, bool &ACanWrite);
	
public:
	__classmethod void __fastcall SaveSettingsToFile(System::TObject* AObject, System::UnicodeString AFileName);
	__classmethod void __fastcall LoadSettingsFromFile(System::TObject* AObject, System::UnicodeString AFileName);
	__classmethod void __fastcall SaveSettingsToStream(System::TObject* AObject, System::Classes::TStream* AStream);
	__classmethod void __fastcall LoadSettingsFromStream(System::TObject* AObject, System::Classes::TStream* AStream);
	__classmethod void __fastcall GetEnumValues(System::Classes::TStrings* AValues, System::Typinfo::PPropInfo APropInfo);
	__classmethod System::TObject* __fastcall CreateObject(const System::UnicodeString AClassName, System::TClass BaseClass);
	__classmethod System::Typinfo::PTypeInfo __fastcall GetPropInfoDataTypeInfo(System::Typinfo::PPropInfo APropInfo);
	__classmethod System::TClass __fastcall GetPropInfoDataTypeInfoClassType(System::Typinfo::PPropInfo APropInfo);
	__classmethod virtual System::TTypeKind __fastcall GetPropInfoType(System::Typinfo::PPropInfo APropInfo);
	__classmethod virtual System::UnicodeString __fastcall GetPropInfoName(System::Typinfo::PPropInfo APropInfo);
	__classmethod System::UnicodeString __fastcall GetPropInfoTypeName(System::Typinfo::PPropInfo APropInfo);
	__classmethod System::UnicodeString __fastcall GetEnumName(System::Typinfo::PTypeInfo ATypeInfo, int AValue);
	__classmethod virtual bool __fastcall IsWriteOnly(System::Typinfo::PPropInfo APropInfo);
	__classmethod virtual bool __fastcall IsReadOnly(System::Typinfo::PPropInfo APropInfo);
	__classmethod virtual bool __fastcall IsAssignableProperty(System::TObject* AObject, System::Typinfo::PPropInfo APropInfo);
	__classmethod virtual bool __fastcall IsColor(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsStrokeKind(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsFillKind(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsDate(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsDateTime(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsTime(System::UnicodeString APropertyName);
	__classmethod virtual bool __fastcall IsGenericList(System::TClass AClass, System::UnicodeString AType = System::UnicodeString());
	__classmethod virtual bool __fastcall IsGenericDictionary(System::TClass AClass);
	__classmethod virtual bool __fastcall IsCollection(System::TClass AClass);
	__classmethod virtual bool __fastcall IsComponent(System::TClass AClass);
	__classmethod virtual bool __fastcall IsControl(System::TClass AClass);
	__classmethod virtual bool __fastcall IsList(System::TClass AClass);
	__classmethod virtual bool __fastcall IsDescendingClass(System::TClass AClass, System::UnicodeString *AClassParentList, const int AClassParentList_High);
	__classmethod virtual bool __fastcall IsBitmap(System::TClass AClass);
	__classmethod virtual bool __fastcall IsStrings(System::TClass AClass);
	/* static */ __property TTMSFNCWriterCustomWritePropertyEvent OnCustomWriteProperty = {read=FOnCustomWriteProperty, write=FOnCustomWriteProperty};
	/* static */ __property TTMSFNCReaderCustomReadPropertyEvent OnCustomReadProperty = {read=FOnCustomReadProperty, write=FOnCustomReadProperty};
	/* static */ __property System::TObject* RootObject = {read=FRootObject, write=FRootObject};
	/* static */ __property TTMSFNCExcludePropertyListArray ExcludeProperties = {read=FExcludeProperties, write=FExcludeProperties};
	/* static */ __property System::TObject* IOReference = {read=FIOReference, write=FIOReference};
	
private:
	// __classmethod void __fastcall Create_();
	// __classmethod void __fastcall Destroy_();
	
public:
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCPersistence() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCPersistence() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TTMSFNCJSONToClassPropertyType : unsigned char { cptUndefined, cptString, cptBoolean, cptDateTime, cptObject, cptDouble, cptInteger, cptInteger64, cptObjectArray, cptStringArray, cptBooleanArray, cptDateTimeArray, cptDoubleArray, cptIntegerArray, cptInteger64Array };

enum DECLSPEC_DENUM TTMSFNCJSONToClassBaseClass : unsigned char { cbcNone, cbcPersistent };

struct DECLSPEC_DRECORD TTMSFNCJSONToClassOptions
{
public:
	bool SortProperties;
	bool DelphiCasing;
	bool RemoveSpecialCharacters;
	bool AddConstructor;
	bool AddDestructor;
	bool AddAssign;
	bool AddUnit;
	TTMSFNCJSONToClassBaseClass BaseClass;
	bool AddImplementation;
};


__interface TTMSFNCJSONToClassExportEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(System::UnicodeString &APropertyName, TTMSFNCJSONToClassPropertyType &APropertyType) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONToClassProperty : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	TTMSFNCJSONToClassPropertyType FType;
	System::UnicodeString __fastcall FixKeyWord(System::UnicodeString AValue);
	
public:
	bool __fastcall IsObject();
	System::UnicodeString __fastcall DelphiName(_di_TTMSFNCJSONToClassExportEvent ACallBack, const TTMSFNCJSONToClassOptions &AOptions, bool AFixKeyWord = true);
	System::UnicodeString __fastcall DelphiType(_di_TTMSFNCJSONToClassExportEvent ACallBack, const TTMSFNCJSONToClassOptions &AOptions);
	__fastcall TTMSFNCJSONToClassProperty(System::UnicodeString AName, TTMSFNCJSONToClassPropertyType AType);
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property TTMSFNCJSONToClassPropertyType Type = {read=FType, write=FType, nodefault};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCJSONToClassProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONToClassProperties : public System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCJSONToClassProperty*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(System::Generics::Collections::TEnumerable__1<TTMSFNCJSONToClassProperty*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Destroy */ inline __fastcall virtual ~TTMSFNCJSONToClassProperties() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCJSONToClassProperty*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(System::Generics::Collections::TEnumerable__1<TTMSFNCJSONToClassProperty*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassProperty>.Create */ inline __fastcall TTMSFNCJSONToClassProperties(TTMSFNCJSONToClassProperty* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassProperty*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONToClassItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCJSONToClassProperties* FProperties;
	TTMSFNCJSONToClassProperty* FParentProperty;
	TTMSFNCJSONToClassProperties* __fastcall GetProperties();
	
public:
	System::UnicodeString __fastcall GetClassName(_di_TTMSFNCJSONToClassExportEvent ACallBack, const TTMSFNCJSONToClassOptions &AOptions);
	__fastcall TTMSFNCJSONToClassItem(TTMSFNCJSONToClassProperty* AParentProperty);
	__fastcall virtual ~TTMSFNCJSONToClassItem();
	__property TTMSFNCJSONToClassProperties* Properties = {read=GetProperties};
	__property TTMSFNCJSONToClassProperty* ParentProperty = {read=FParentProperty};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONToClassItems : public System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCJSONToClassItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(System::Generics::Collections::TEnumerable__1<TTMSFNCJSONToClassItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Destroy */ inline __fastcall virtual ~TTMSFNCJSONToClassItems() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCJSONToClassItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(System::Generics::Collections::TEnumerable__1<TTMSFNCJSONToClassItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPersistence_TTMSFNCJSONToClassItem>.Create */ inline __fastcall TTMSFNCJSONToClassItems(TTMSFNCJSONToClassItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCJSONToClassItem*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONToClass : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static TTMSFNCJSONToClassItems* FClasses;
	
protected:
	__classmethod void __fastcall JSONValueToClass(TTMSFNCJSONToClassProperty* AParentProperty, System::Json::TJSONValue* AJSONValue);
	__classmethod TTMSFNCJSONToClassPropertyType __fastcall JSONValueToPropertyType(System::Json::TJSONValue* AJSONValue);
	
public:
	__classmethod System::UnicodeString __fastcall ExportToDelphi(System::UnicodeString AJSONString, const TTMSFNCJSONToClassOptions &AOptions, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	__classmethod System::UnicodeString __fastcall ExportToDelphi(System::UnicodeString AJSONString, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	__classmethod System::UnicodeString __fastcall ExportToDelphi(System::Json::TJSONValue* AJSONValue, const TTMSFNCJSONToClassOptions &AOptions, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	__classmethod System::UnicodeString __fastcall ExportToDelphi(System::Json::TJSONValue* AJSONValue, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	__classmethod System::UnicodeString __fastcall ExportToDelphiFromFile(System::UnicodeString AFileName, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	__classmethod System::UnicodeString __fastcall ExportToDelphiFromFile(System::UnicodeString AFileName, const TTMSFNCJSONToClassOptions &AOptions, _di_TTMSFNCJSONToClassExportEvent ACallBack = _di_TTMSFNCJSONToClassExportEvent())/* overload */;
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCJSONToClass() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCJSONToClass() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::UnicodeString, 53> Fmx_Tmsfncpersistence__71;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Fmx_Tmsfncpersistence__71 ExcludePropertyList;
extern DELPHI_PACKAGE TTMSFNCJSONToClassOptions __fastcall DefaultJSONToClassOptions(void);
}	/* namespace Tmsfncpersistence */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPERSISTENCE)
using namespace Fmx::Tmsfncpersistence;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpersistenceHPP
