// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCBitmapContainer.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncbitmapcontainerHPP
#define Vcl_TmsfncbitmapcontainerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <Vcl.Controls.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncbitmapcontainer
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCBitmapContainer;
typedef System::DelphiInterface<ITMSFNCBitmapContainer> _di_ITMSFNCBitmapContainer;
__interface DELPHIINTERFACE ITMSFNCBitmapContainerGetItem;
typedef System::DelphiInterface<ITMSFNCBitmapContainerGetItem> _di_ITMSFNCBitmapContainerGetItem;
class DELPHICLASS TTMSFNCBitmapItem;
class DELPHICLASS TTMSFNCBitmapCollection;
class DELPHICLASS TControlList;
class DELPHICLASS TTMSFNCBitmapContainer;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{ED26710D-395F-4971-8AC9-A31083BF2A3C}") ITMSFNCBitmapContainer  : public System::IInterface 
{
	virtual void __fastcall SetBitmapContainer(TTMSFNCBitmapContainer* const Value) = 0 ;
	virtual TTMSFNCBitmapContainer* __fastcall GetBitmapContainer() = 0 ;
	__property TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
};

__interface  INTERFACE_UUID("{98F65D59-B40C-4574-AF9C-3CA68E86AE10}") ITMSFNCBitmapContainerGetItem  : public System::IInterface 
{
	virtual int __fastcall ItemCount() = 0 ;
	virtual TTMSFNCBitmapItem* __fastcall GetItem(int AIndex) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	NativeInt FTag;
	System::UnicodeString FName;
	void __fastcall SetBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	
public:
	__fastcall virtual TTMSFNCBitmapItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCBitmapItem();
	virtual void __fastcall DoBitmapChanged(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
__published:
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property NativeInt Tag = {read=FTag, write=FTag, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TTMSFNCBitmapItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCBitmapContainer* FOwner;
	TTMSFNCBitmapItem* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCBitmapItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetBitmapItemClass();
	
public:
	__fastcall TTMSFNCBitmapCollection(TTMSFNCBitmapContainer* AOwner);
	HIDESBASE TTMSFNCBitmapItem* __fastcall Add();
	HIDESBASE TTMSFNCBitmapItem* __fastcall Insert(int index);
	__property TTMSFNCBitmapItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapCollection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TControlList : public System::Generics::Collections::TList__1<Vcl::Controls::TControl*>
{
	typedef System::Generics::Collections::TList__1<Vcl::Controls::TControl*> inherited;
	
public:
	/* {System_Generics_Collections}TList<Vcl_Controls_TControl>.Create */ inline __fastcall TControlList()/* overload */ : System::Generics::Collections::TList__1<Vcl::Controls::TControl*>() { }
	/* {System_Generics_Collections}TList<Vcl_Controls_TControl>.Create */ inline __fastcall TControlList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<Vcl::Controls::TControl*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<Vcl::Controls::TControl*>(AComparer) { }
	/* {System_Generics_Collections}TList<Vcl_Controls_TControl>.Create */ inline __fastcall TControlList(System::Generics::Collections::TEnumerable__1<Vcl::Controls::TControl*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<Vcl::Controls::TControl*>(Collection) { }
	/* {System_Generics_Collections}TList<Vcl_Controls_TControl>.Create */ inline __fastcall TControlList(Vcl::Controls::TControl* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<Vcl::Controls::TControl*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<Vcl_Controls_TControl>.Destroy */ inline __fastcall virtual ~TControlList() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCBitmapContainer : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	TControlList* FControls;
	TTMSFNCBitmapCollection* FItems;
	System::Classes::TNotifyEvent FOnBitmapChanged;
	void __fastcall SetItems(TTMSFNCBitmapCollection* const Value);
	System::UnicodeString __fastcall GetBitmapName(int AIndex);
	Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetBitmap(int AIndex);
	TTMSFNCBitmapCollection* __fastcall GetItems();
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetVersion();
	virtual TTMSFNCBitmapCollection* __fastcall CreateItems();
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	void __fastcall DoBitmapChanged(System::TObject* Sender);
	virtual void __fastcall InvalidateMembers(Vcl::Controls::TWinControl* AControl);
	
public:
	__fastcall virtual TTMSFNCBitmapContainer()/* overload */;
	__fastcall virtual TTMSFNCBitmapContainer(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCBitmapContainer();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE virtual void __fastcall Changed();
	void __fastcall RegisterControl(Vcl::Controls::TControl* AControl);
	virtual void __fastcall FindBitmap(int i, Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap)/* overload */;
	virtual TTMSFNCBitmapItem* __fastcall GetItem(int AIndex);
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall FindBitmap(System::UnicodeString s)/* overload */;
	int __fastcall ItemCount();
	System::UnicodeString __fastcall RandomBitmapName();
	Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall RandomBitmap();
	virtual void __fastcall AddFromURL(System::UnicodeString URL, System::UnicodeString BitmapName);
	void __fastcall AddFromResource(System::UnicodeString ResourceName, System::UnicodeString BitmapName, NativeUInt AInstance)/* overload */;
	void __fastcall AddFromResource(System::UnicodeString ResourceName, System::UnicodeString BitmapName)/* overload */;
	void __fastcall AddFromFile(System::UnicodeString FileName, System::UnicodeString BitmapName);
	void __fastcall AddFromFolder(System::UnicodeString AFolder);
	__property System::UnicodeString BitmapNames[int AIndex] = {read=GetBitmapName};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmaps[int AIndex] = {read=GetBitmap};
	
__published:
	__property TTMSFNCBitmapCollection* Items = {read=GetItems, write=SetItems};
	__property System::UnicodeString Version = {read=GetVersion};
	__property System::Classes::TNotifyEvent OnBitmapChanged = {read=FOnBitmapChanged, write=FOnBitmapChanged};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCBitmapContainer(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
private:
	void *__ITMSFNCBitmapContainerGetItem;	// ITMSFNCBitmapContainerGetItem 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {98F65D59-B40C-4574-AF9C-3CA68E86AE10}
	operator _di_ITMSFNCBitmapContainerGetItem()
	{
		_di_ITMSFNCBitmapContainerGetItem intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCBitmapContainerGetItem*(void) { return (ITMSFNCBitmapContainerGetItem*)&__ITMSFNCBitmapContainerGetItem; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncbitmapcontainer */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCBITMAPCONTAINER)
using namespace Vcl::Tmsfncbitmapcontainer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncbitmapcontainerHPP
