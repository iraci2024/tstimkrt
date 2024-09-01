// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcJSON.pas' rev: 35.00 (Windows)

#ifndef SgcjsonHPP
#define SgcjsonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcjson
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IsgcObjectJSON;
typedef System::DelphiInterface<IsgcObjectJSON> _di_IsgcObjectJSON;
__interface DELPHIINTERFACE IsgcJSON;
typedef System::DelphiInterface<IsgcJSON> _di_IsgcJSON;
class DELPHICLASS TInterfacedComponent;
class DELPHICLASS TsgcObjectJSON;
class DELPHICLASS TsgcObjectJSONList;
class DELPHICLASS TsgcJSON;
class DELPHICLASS TsgcOwnerComponent;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcJSONtype : unsigned char { sgcJSONUndefined, sgcJSONObject, sgcJSONList, sgcJSONString, sgcJSONNumber, sgcJSONBoolean, sgcJSONNull };

__interface  INTERFACE_UUID("{DF1B6784-B501-46BC-A679-E4CEACBD9022}") IsgcObjectJSON  : public System::IInterface 
{
	virtual _di_IsgcJSON __fastcall GetJSONObject() = 0 ;
	virtual TsgcJSONtype __fastcall GetJSONType() = 0 ;
	virtual void __fastcall SetJSONObject(const _di_IsgcJSON Value) = 0 ;
	virtual void __fastcall SetJSONType(const TsgcJSONtype Value) = 0 ;
	__property _di_IsgcJSON JSONObject = {read=GetJSONObject, write=SetJSONObject};
	virtual _di_IsgcObjectJSON __fastcall GetNode(const System::UnicodeString aName) = 0 ;
	virtual void __fastcall SetNode(const System::UnicodeString aName, const _di_IsgcObjectJSON aValue) = 0 ;
	virtual _di_IsgcObjectJSON __fastcall GetItem(const int i) = 0 ;
	virtual void __fastcall SetItem(const int i, const _di_IsgcObjectJSON aValue) = 0 ;
	virtual System::Variant __fastcall GetValue() = 0 ;
	virtual void __fastcall SetValue(const System::Variant &Value) = 0 ;
	virtual int __fastcall GetCount() = 0 ;
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual void __fastcall SetName_(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall Clear() = 0 ;
	__property _di_IsgcObjectJSON Item[const int i] = {read=GetItem, write=SetItem};
	__property _di_IsgcObjectJSON Node[const System::UnicodeString aName] = {read=GetNode, write=SetNode};
	__property int Count = {read=GetCount};
	__property TsgcJSONtype JSONType = {read=GetJSONType, write=SetJSONType};
	__property System::UnicodeString Name = {read=GetName, write=SetName_};
	__property System::Variant Value = {read=GetValue, write=SetValue};
};

__interface  INTERFACE_UUID("{76C5903F-99BE-4E3C-85C8-413B6B278C3F}") IsgcJSON  : public System::IInterface 
{
	virtual int __fastcall GetCount() = 0 ;
	virtual _di_IsgcObjectJSON __fastcall GetNode(const System::UnicodeString aName) = 0 ;
	virtual void __fastcall SetNode(const System::UnicodeString aName, const _di_IsgcObjectJSON aValue) = 0 ;
	virtual _di_IsgcObjectJSON __fastcall GetItem(int i) = 0 ;
	virtual void __fastcall SetItem(int i, const _di_IsgcObjectJSON aValue) = 0 ;
	virtual bool __fastcall GetIsArray() = 0 ;
	virtual void __fastcall SetIsArray(const bool Value) = 0 ;
	__property _di_IsgcObjectJSON Node[const System::UnicodeString aName] = {read=GetNode, write=SetNode};
	__property _di_IsgcObjectJSON Item[int i] = {read=GetItem, write=SetItem};
	__property int Count = {read=GetCount};
	__property bool IsArray = {read=GetIsArray, write=SetIsArray};
	virtual _di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const System::UnicodeString aValue) = 0 /* overload */;
	virtual _di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const int aValue) = 0 /* overload */;
	virtual _di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const __int64 aValue) = 0 /* overload */;
	virtual _di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const double aValue) = 0 /* overload */;
	virtual _di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const bool aValue) = 0 /* overload */;
	virtual _di_IsgcObjectJSON __fastcall AddObject(const System::UnicodeString aName, const System::UnicodeString aValue = System::UnicodeString()) = 0 ;
	virtual _di_IsgcObjectJSON __fastcall AddArray(const System::UnicodeString aName, const System::UnicodeString aValue = System::UnicodeString()) = 0 ;
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall Read(System::UnicodeString aText) = 0 ;
	virtual System::UnicodeString __fastcall GetText() = 0 ;
	__property System::UnicodeString Text = {read=GetText};
};

class PASCALIMPLEMENTATION TInterfacedComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	HIDESBASE int __stdcall _AddRef();
	HIDESBASE int __stdcall _Release();
	
public:
	virtual void __fastcall BeforeDestruction();
	__fastcall virtual ~TInterfacedComponent();
	virtual void __fastcall FreeInstance();
	
private:
	int FRefCount;
	bool FDestroyObject;
public:
	/* TComponent.Create */ inline __fastcall virtual TInterfacedComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	
private:
	void *__IInterface;	// System::IInterface 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000000-0000-0000-C000-000000000046}
	operator System::_di_IInterface()
	{
		System::_di_IInterface intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator System::IInterface*(void) { return (System::IInterface*)&__IInterface; }
	#endif
	
};


class PASCALIMPLEMENTATION TsgcObjectJSON : public TInterfacedComponent
{
	typedef TInterfacedComponent inherited;
	
private:
	bool FQuoted;
	System::Variant FValue;
	System::UnicodeString FName;
	System::Variant __fastcall GetValue();
	void __fastcall SetValue(const System::Variant &Value);
	_di_IsgcJSON FJSONObject;
	TsgcJSONtype FJSONType;
	
protected:
	int __fastcall GetCount();
	_di_IsgcJSON __fastcall GetJSONObject();
	TsgcJSONtype __fastcall GetJSONType();
	System::UnicodeString __fastcall GetName();
	void __fastcall SetJSONObject(const _di_IsgcJSON Value);
	void __fastcall SetJSONType(const TsgcJSONtype Value);
	void __fastcall SetName_(const System::UnicodeString Value);
	
public:
	__fastcall virtual TsgcObjectJSON(System::Classes::TComponent* aOwner);
	void __fastcall Clear();
	__property _di_IsgcJSON JSONObject = {read=GetJSONObject, write=SetJSONObject};
	
protected:
	_di_IsgcObjectJSON __fastcall GetNode(const System::UnicodeString aName);
	void __fastcall SetNode(const System::UnicodeString aName, const _di_IsgcObjectJSON aValue);
	_di_IsgcObjectJSON __fastcall GetItem(const int i);
	void __fastcall SetItem(const int i, const _di_IsgcObjectJSON aValue);
	
public:
	__property _di_IsgcObjectJSON Item[const int i] = {read=GetItem, write=SetItem};
	__property _di_IsgcObjectJSON Node[const System::UnicodeString aName] = {read=GetNode, write=SetNode};
	__property int Count = {read=GetCount, nodefault};
	__property TsgcJSONtype JSONType = {read=GetJSONType, write=SetJSONType, nodefault};
	__property System::UnicodeString Name = {read=GetName, write=SetName_};
	__property System::Variant Value = {read=GetValue, write=SetValue};
public:
	/* TInterfacedComponent.Destroy */ inline __fastcall virtual ~TsgcObjectJSON() { }
	
private:
	void *__IsgcObjectJSON;	// IsgcObjectJSON 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {DF1B6784-B501-46BC-A679-E4CEACBD9022}
	operator _di_IsgcObjectJSON()
	{
		_di_IsgcObjectJSON intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IsgcObjectJSON*(void) { return (IsgcObjectJSON*)&__IsgcObjectJSON; }
	#endif
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcObjectJSONList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
protected:
	TsgcObjectJSON* __fastcall GetNode(const System::UnicodeString aName);
	HIDESBASE TsgcObjectJSON* __fastcall GetItem(int i);
	void __fastcall SetNode(const System::UnicodeString aName, TsgcObjectJSON* const aValue);
	HIDESBASE void __fastcall SetItem(int i, TsgcObjectJSON* const aValue);
	
public:
	__property TsgcObjectJSON* Node[const System::UnicodeString aName] = {read=GetNode, write=SetNode};
	__property TsgcObjectJSON* Item[int i] = {read=GetItem, write=SetItem};
	__fastcall virtual ~TsgcObjectJSONList();
public:
	/* TObjectList.Create */ inline __fastcall TsgcObjectJSONList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcObjectJSONList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcJSON : public TInterfacedComponent
{
	typedef TInterfacedComponent inherited;
	
protected:
	bool __fastcall DoCreateObject(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoCreateList(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadObject(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadList(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadValue(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadString(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadChar(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadInteger(System::UnicodeString &aText, int &Pos);
	bool __fastcall DoReadNumber(System::UnicodeString &aText, int &Pos);
	void __fastcall DoSpaces(System::UnicodeString &aText, int &Pos);
	System::UnicodeString __fastcall DoGetObject(TsgcObjectJSON* aObject, const bool aIsArray = false);
	System::UnicodeString __fastcall DoGetString(TsgcObjectJSON* aObject, const bool aIsArray = false);
	System::UnicodeString __fastcall DoGetNumber(TsgcObjectJSON* aObject, const bool aIsArray = false);
	System::UnicodeString __fastcall DoGetBoolean(TsgcObjectJSON* aObject, const bool aIsArray = false);
	System::UnicodeString __fastcall DoGetNull(TsgcObjectJSON* aObject, const bool aIsArray = false);
	System::UnicodeString __fastcall DoGetList(TsgcObjectJSON* aObject);
	
private:
	bool FIsArray;
	
protected:
	virtual int __fastcall GetCount();
	virtual _di_IsgcObjectJSON __fastcall GetNode(const System::UnicodeString aName);
	virtual void __fastcall SetNode(const System::UnicodeString aName, const _di_IsgcObjectJSON aValue);
	virtual _di_IsgcObjectJSON __fastcall GetItem(int i);
	virtual void __fastcall SetItem(int i, const _di_IsgcObjectJSON aValue);
	virtual bool __fastcall GetIsArray();
	virtual void __fastcall SetIsArray(const bool Value);
	
public:
	__property _di_IsgcObjectJSON Node[const System::UnicodeString aName] = {read=GetNode, write=SetNode};
	__property _di_IsgcObjectJSON Item[int i] = {read=GetItem, write=SetItem};
	__property int Count = {read=GetCount, nodefault};
	__property bool IsArray = {read=GetIsArray, write=SetIsArray, nodefault};
	
protected:
	_di_IsgcObjectJSON __fastcall DoAdd(const System::UnicodeString aName, const System::Variant &aValue, TsgcJSONtype aType)/* overload */;
	
public:
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const System::UnicodeString aValue)/* overload */;
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const int aValue)/* overload */;
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const __int64 aValue)/* overload */;
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const double aValue)/* overload */;
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName, const bool aValue)/* overload */;
	_di_IsgcObjectJSON __fastcall AddPair(const System::UnicodeString aName)/* overload */;
	_di_IsgcObjectJSON __fastcall AddObject(const System::UnicodeString aName, const System::UnicodeString aValue = System::UnicodeString());
	_di_IsgcObjectJSON __fastcall AddArray(const System::UnicodeString aName, const System::UnicodeString aValue = System::UnicodeString());
	
private:
	TsgcObjectJSONList* FData;
	TsgcObjectJSONList* __fastcall GetData();
	
protected:
	__property TsgcObjectJSONList* Data = {read=GetData, write=FData};
	
public:
	void __fastcall Clear();
	
private:
	System::UnicodeString FJSONName;
	TsgcJSONtype FJSONType;
	System::Variant FJSONValue;
	void __fastcall DoJSONStart(const System::UnicodeString aName);
	void __fastcall DoJSONValue(const System::UnicodeString aValue);
	void __fastcall DoJSONEnd();
	
protected:
	virtual System::UnicodeString __fastcall GetJSONValue(TsgcObjectJSON* const aJSON, bool aIsArray = false);
	System::UnicodeString __fastcall GetText();
	
public:
	void __fastcall Read(System::UnicodeString aText);
	__property System::UnicodeString Text = {read=GetText};
	__fastcall virtual TsgcJSON(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcJSON();
	
private:
	bool FEscapeStrings;
	System::UnicodeString __fastcall EscapeString(const System::UnicodeString aValue);
	
public:
	__property bool EscapeStrings = {read=FEscapeStrings, write=FEscapeStrings, nodefault};
private:
	void *__IsgcJSON;	// IsgcJSON 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {76C5903F-99BE-4E3C-85C8-413B6B278C3F}
	operator _di_IsgcJSON()
	{
		_di_IsgcJSON intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IsgcJSON*(void) { return (IsgcJSON*)&__IsgcJSON; }
	#endif
	
};


class PASCALIMPLEMENTATION TsgcOwnerComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	_di_IsgcJSON FInstance;
	
protected:
	__property _di_IsgcJSON Instance = {read=FInstance, write=FInstance};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcOwnerComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcOwnerComponent() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetJSONClass(const System::Classes::TComponentClass aClass);
extern DELPHI_PACKAGE _di_IsgcJSON __fastcall GetJSONInstance(System::Classes::TComponent* aOwner = (System::Classes::TComponent*)(0x0));
extern DELPHI_PACKAGE void __fastcall FreeJSONInstance(_di_IsgcJSON aInstance);
}	/* namespace Sgcjson */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCJSON)
using namespace Sgcjson;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgcjsonHPP
