﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCStateManager.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncstatemanagerHPP
#define Fmx_TmsfncstatemanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCJSONWriter.hpp>
#include <FMX.TMSFNCPersistence.hpp>
#include <FMX.Types.hpp>
#include <System.TypInfo.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncstatemanager
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCStateManagerItem;
class DELPHICLASS TTMSFNCStateManagerItems;
__interface DELPHIINTERFACE TTMSFNCStateManagerLoadStateCustomCallback;
typedef System::DelphiInterface<TTMSFNCStateManagerLoadStateCustomCallback> _di_TTMSFNCStateManagerLoadStateCustomCallback;
class DELPHICLASS TTMSFNCCustomStateManager;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStateManagerItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TTMSFNCCustomStateManager* FOwner;
	bool FDefault;
	System::UnicodeString FName;
	System::UnicodeString FContent;
	void *FDataPointer;
	bool FDataBoolean;
	System::UnicodeString FDataString;
	System::TObject* FDataObject;
	NativeInt FDataInteger;
	void __fastcall SetDefault(const bool Value);
	System::UnicodeString __fastcall GetName();
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	TTMSFNCCustomStateManager* __fastcall Manager();
	
public:
	__fastcall virtual TTMSFNCStateManagerItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCStateManagerItem();
	void __fastcall Load();
	__property void * DataPointer = {read=FDataPointer, write=FDataPointer};
	__property bool DataBoolean = {read=FDataBoolean, write=FDataBoolean, nodefault};
	__property System::TObject* DataObject = {read=FDataObject, write=FDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=FDataString};
	__property NativeInt DataInteger = {read=FDataInteger, write=FDataInteger, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=GetName, write=FName};
	__property System::UnicodeString Content = {read=FContent, write=FContent};
	__property bool Default = {read=FDefault, write=SetDefault, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStateManagerItems : public Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCStateManagerItem*>
{
	typedef Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCStateManagerItem*> inherited;
	
public:
	TTMSFNCStateManagerItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::_di_IInterface FOwnerInterface;
	TTMSFNCCustomStateManager* FOwner;
	TTMSFNCStateManagerItem* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCStateManagerItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetStateManagerStateClass();
	virtual System::TObject* __fastcall CreateObject(const System::UnicodeString AClassName, const System::TClass ABaseClass);
	virtual System::TClass __fastcall GetInterfaceItemClass();
	int __stdcall _AddRef();
	int __stdcall _Release();
	
public:
	virtual HRESULT __stdcall QueryInterface(const GUID IID, /* out */ void *Obj);
	__fastcall TTMSFNCStateManagerItems(TTMSFNCCustomStateManager* AOwner);
	HIDESBASE TTMSFNCStateManagerItem* __fastcall Add();
	HIDESBASE TTMSFNCStateManagerItem* __fastcall Insert(int index);
	__property TTMSFNCStateManagerItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCStateManagerItems() { }
	
private:
	void *__ITMSFNCBasePersistenceIO;	// Fmx::Tmsfncpersistence::ITMSFNCBasePersistenceIO 
	void *__ITMSFNCBaseListIO;	// Fmx::Tmsfncpersistence::ITMSFNCBaseListIO 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {91DEAFC3-8932-45F4-A3ED-5AAA0C0E9250}
	operator Fmx::Tmsfncpersistence::_di_ITMSFNCBasePersistenceIO()
	{
		Fmx::Tmsfncpersistence::_di_ITMSFNCBasePersistenceIO intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncpersistence::ITMSFNCBasePersistenceIO*(void) { return (Fmx::Tmsfncpersistence::ITMSFNCBasePersistenceIO*)&__ITMSFNCBasePersistenceIO; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {FAB1D21E-D798-4CE0-B17B-9D75E4456AB4}
	operator Fmx::Tmsfncpersistence::_di_ITMSFNCBaseListIO()
	{
		Fmx::Tmsfncpersistence::_di_ITMSFNCBaseListIO intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncpersistence::ITMSFNCBaseListIO*(void) { return (Fmx::Tmsfncpersistence::ITMSFNCBaseListIO*)&__ITMSFNCBaseListIO; }
	#endif
	
};

#pragma pack(pop)

typedef Fmx::Types::TFmxObject TTMSFNCStateManagerControl;

__interface TTMSFNCStateManagerLoadStateCustomCallback  : public System::IInterface 
{
	virtual void __fastcall Invoke(TTMSFNCStateManagerItem* AState, bool &ALoad) = 0 ;
};

typedef void __fastcall (__closure *TTMSFNCStateManagerLoadStateCustomEvent)(System::TObject* Sender, TTMSFNCStateManagerItem* AState, bool &ALoad);

typedef void __fastcall (__closure *TTMSFNCStateManagerBeforeLoadStateEvent)(System::TObject* Sender, TTMSFNCStateManagerItem* AState, bool &ACanLoad);

typedef void __fastcall (__closure *TTMSFNCStateManagerAfterLoadStateEvent)(System::TObject* Sender, TTMSFNCStateManagerItem* AState);

typedef void __fastcall (__closure *TTMSFNCStateManagerBeforeLoadControlStateEvent)(System::TObject* Sender, TTMSFNCStateManagerItem* AState, Fmx::Types::TFmxObject* AControl, System::UnicodeString &AValue, bool &ACanLoad);

typedef void __fastcall (__closure *TTMSFNCStateManagerAfterLoadControlStateEvent)(System::TObject* Sender, TTMSFNCStateManagerItem* AState, Fmx::Types::TFmxObject* AControl, System::UnicodeString AValue);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomStateManager : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	int FUpdateCount;
	int FActiveState;
	TTMSFNCStateManagerItems* FStates;
	Fmx::Types::TFmxObject* FControl;
	TTMSFNCStateManagerLoadStateCustomEvent FOnLoadStateCustom;
	bool FAutoSave;
	TTMSFNCStateManagerAfterLoadStateEvent FOnAfterLoadState;
	TTMSFNCStateManagerBeforeLoadStateEvent FOnBeforeLoadState;
	TTMSFNCStateManagerAfterLoadControlStateEvent FOnAfterLoadControlState;
	TTMSFNCStateManagerBeforeLoadControlStateEvent FOnBeforeLoadControlState;
	void __fastcall SetStates(TTMSFNCStateManagerItems* const Value);
	void __fastcall SetActiveState(const int Value);
	void __fastcall SetControl(Fmx::Types::TFmxObject* const Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	System::UnicodeString __fastcall GenerateContent();
	bool __fastcall CanPersist(Fmx::Types::TFmxObject* AControl);
	HIDESBASE int __fastcall GetControlsCount(Fmx::Types::TFmxObject* AControl);
	HIDESBASE Fmx::Types::TFmxObject* __fastcall GetControls(Fmx::Types::TFmxObject* AControl, int AIndex);
	virtual TTMSFNCStateManagerItems* __fastcall CreateStatesCollection();
	Fmx::Types::TFmxObject* __fastcall FindControlByName(System::UnicodeString AName, Fmx::Types::TFmxObject* ARootControl = (Fmx::Types::TFmxObject*)(0x0));
	virtual void __fastcall ResetState();
	virtual void __fastcall BeforeAssignControl();
	virtual void __fastcall AfterAssignControl();
	virtual void __fastcall DoBeforeLoadState(TTMSFNCStateManagerItem* AState, bool &ACanLoad);
	virtual void __fastcall DoAfterLoadState(TTMSFNCStateManagerItem* AState);
	virtual void __fastcall DoBeforeLoadControlState(TTMSFNCStateManagerItem* AState, Fmx::Types::TFmxObject* AControl, System::UnicodeString &AValue, bool &ACanLoad);
	virtual void __fastcall DoAfterLoadControlState(TTMSFNCStateManagerItem* AState, Fmx::Types::TFmxObject* AControl, System::UnicodeString AValue);
	virtual void __fastcall BeforeLoadState(TTMSFNCStateManagerItem* AState);
	virtual void __fastcall InternalLoadState(TTMSFNCStateManagerItem* AState);
	virtual void __fastcall InternalSaveToState(TTMSFNCStateManagerItem* AState, bool ANew);
	virtual void __fastcall InternalSetActiveState(TTMSFNCStateManagerItem* AState);
	void __fastcall DoCanWrite(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyKind, Fmx::Tmsfncjsonwriter::TTMSFNCJSONWriter* AWriter, bool &ACanWrite);
	__property TTMSFNCStateManagerItems* States = {read=FStates, write=SetStates};
	__property Fmx::Types::TFmxObject* Control = {read=FControl, write=SetControl};
	__property TTMSFNCStateManagerLoadStateCustomEvent OnLoadStateCustom = {read=FOnLoadStateCustom, write=FOnLoadStateCustom};
	__property TTMSFNCStateManagerBeforeLoadStateEvent OnBeforeLoadState = {read=FOnBeforeLoadState, write=FOnBeforeLoadState};
	__property TTMSFNCStateManagerAfterLoadStateEvent OnAfterLoadState = {read=FOnAfterLoadState, write=FOnAfterLoadState};
	__property TTMSFNCStateManagerBeforeLoadControlStateEvent OnBeforeLoadControlState = {read=FOnBeforeLoadControlState, write=FOnBeforeLoadControlState};
	__property TTMSFNCStateManagerAfterLoadControlStateEvent OnAfterLoadControlState = {read=FOnAfterLoadControlState, write=FOnAfterLoadControlState};
	__property int ActiveState = {read=FActiveState, write=SetActiveState, default=-1};
	__property bool AutoSave = {read=FAutoSave, write=FAutoSave, default=1};
	
public:
	__fastcall virtual TTMSFNCCustomStateManager()/* overload */;
	__fastcall virtual TTMSFNCCustomStateManager(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCCustomStateManager();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall Loaded();
	virtual void __fastcall SaveToState(TTMSFNCStateManagerItem* AState)/* overload */;
	virtual void __fastcall SaveToState(int AIndex)/* overload */;
	virtual void __fastcall SaveToState(System::UnicodeString AName)/* overload */;
	virtual void __fastcall LoadStateByName(System::UnicodeString AName)/* overload */;
	virtual void __fastcall LoadStateByIndex(int AIndex)/* overload */;
	void __fastcall LoadStateCustom(_di_TTMSFNCStateManagerLoadStateCustomCallback ACallBack = _di_TTMSFNCStateManagerLoadStateCustomCallback());
	virtual void __fastcall Optimize();
	virtual bool __fastcall FindConflicts(System::Classes::TStrings* AConflictedControlNames);
	virtual TTMSFNCStateManagerItem* __fastcall GetDefaultState();
	virtual TTMSFNCStateManagerItem* __fastcall FindStateByName(System::UnicodeString AName);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncstatemanager */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCSTATEMANAGER)
using namespace Fmx::Tmsfncstatemanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncstatemanagerHPP
