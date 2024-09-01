// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCResponsiveManager.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncresponsivemanagerHPP
#define Vcl_TmsfncresponsivemanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCStateManager.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncresponsivemanager
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCResponsiveManagerConstraint;
struct TTMSFNCResponsiveManagerSizeConstraint;
class DELPHICLASS TTMSFNCResponsiveManagerItem;
class DELPHICLASS TTMSFNCResponsiveManagerItems;
class DELPHICLASS TTMSFNCResponsiveManagerItemSizeComparer;
class DELPHICLASS TTMSFNCResponsiveManagerPaintBox;
class DELPHICLASS TTMSFNCCustomResponsiveManager;
class DELPHICLASS TTMSFNCResponsiveManager;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCResponsiveManagerConstraintMode : unsigned char { cmSize, cmString, cmBoolean, cmNumber };

class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerConstraint : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	float FWidth;
	float FHeight;
	System::UnicodeString FStringValue;
	TTMSFNCResponsiveManagerConstraintMode FMode;
	bool FBooleanValue;
	System::Extended FNumberValue;
	bool __fastcall IsHeightStored();
	bool __fastcall IsWidthStored();
	bool __fastcall IsNumberValueStored();
	
protected:
	__property TTMSFNCResponsiveManagerConstraintMode Mode = {read=FMode, write=FMode, default=0};
	
public:
	__fastcall virtual TTMSFNCResponsiveManagerConstraint();
	__fastcall virtual ~TTMSFNCResponsiveManagerConstraint();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float Width = {read=FWidth, write=FWidth, stored=IsWidthStored};
	__property float Height = {read=FHeight, write=FHeight, stored=IsHeightStored};
	__property System::UnicodeString StringValue = {read=FStringValue, write=FStringValue};
	__property System::Extended NumberValue = {read=FNumberValue, write=FNumberValue, stored=IsNumberValueStored};
	__property bool BooleanValue = {read=FBooleanValue, write=FBooleanValue, default=0};
};


struct DECLSPEC_DRECORD TTMSFNCResponsiveManagerSizeConstraint
{
public:
	float Width;
	float Height;
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerItem : public Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItem
{
	typedef Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItem inherited;
	
private:
	TTMSFNCResponsiveManagerConstraint* FConstraint;
	void __fastcall SetConstraint(TTMSFNCResponsiveManagerConstraint* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TTMSFNCResponsiveManagerItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCResponsiveManagerItem();
	
__published:
	__property TTMSFNCResponsiveManagerConstraint* Constraint = {read=FConstraint, write=SetConstraint};
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerItems : public Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItems
{
	typedef Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItems inherited;
	
public:
	TTMSFNCResponsiveManagerItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TTMSFNCResponsiveManagerItem* __fastcall GetItemEx(int Index);
	HIDESBASE void __fastcall SetItemEx(int Index, TTMSFNCResponsiveManagerItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetStateManagerStateClass();
	
public:
	HIDESBASE TTMSFNCResponsiveManagerItem* __fastcall Add();
	HIDESBASE TTMSFNCResponsiveManagerItem* __fastcall Insert(int index);
	__property TTMSFNCResponsiveManagerItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TTMSFNCStateManagerItems.Create */ inline __fastcall TTMSFNCResponsiveManagerItems(Vcl::Tmsfncstatemanager::TTMSFNCCustomStateManager* AOwner) : Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItems(AOwner) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerItems() { }
	
};


enum DECLSPEC_DENUM TTMSFNCResponsiveManagerResizeMode : unsigned char { mrmWidthOnly, mrmHeightOnly, mrmWidthFirst, mrmHeightFirst };

class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerItemSizeComparer : public System::Generics::Defaults::TComparer__1<TTMSFNCResponsiveManagerItem*>
{
	typedef System::Generics::Defaults::TComparer__1<TTMSFNCResponsiveManagerItem*> inherited;
	
private:
	TTMSFNCCustomResponsiveManager* FManager;
	
public:
	virtual int __fastcall Compare(TTMSFNCResponsiveManagerItem* const Left, TTMSFNCResponsiveManagerItem* const Right);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCResponsiveManagerItemSizeComparer() : System::Generics::Defaults::TComparer__1<TTMSFNCResponsiveManagerItem*>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerItemSizeComparer() { }
	
};


typedef System::Generics::Collections::TList__1<TTMSFNCResponsiveManagerItem*>* TTMSFNCResponsiveManagerItemList;

class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerPaintBox : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	TTMSFNCCustomResponsiveManager* FManager;
	
public:
	virtual void __fastcall Paint();
public:
	/* TTMSFNCCustomControl.Create */ inline __fastcall virtual TTMSFNCResponsiveManagerPaintBox(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(AOwner) { }
	/* TTMSFNCCustomControl.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerPaintBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCResponsiveManagerPaintBox(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomResponsiveManager : public Vcl::Tmsfncstatemanager::TTMSFNCCustomStateManager
{
	typedef Vcl::Tmsfncstatemanager::TTMSFNCCustomStateManager inherited;
	
private:
	TTMSFNCResponsiveManagerPaintBox* FPaintBox;
	TTMSFNCCustomResponsiveManager* FPreviewManager;
	System::Classes::TNotifyEvent FOldResize;
	bool FAutoLoadOnResize;
	bool FBlockLoadConstraints;
	TTMSFNCResponsiveManagerResizeMode FMode;
	TTMSFNCResponsiveManagerItems* __fastcall GetStates();
	HIDESBASE void __fastcall SetStates(TTMSFNCResponsiveManagerItems* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetVersion();
	TTMSFNCResponsiveManagerSizeConstraint __fastcall GetSizeConstraint();
	virtual TTMSFNCResponsiveManagerItem* __fastcall InternalFindState(TTMSFNCResponsiveManagerConstraint* AConstraint);
	virtual Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItems* __fastcall CreateStatesCollection();
	virtual void __fastcall InternalSetActiveState(Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItem* AState);
	virtual void __fastcall SetConstraint(TTMSFNCResponsiveManagerItem* AState);
	virtual void __fastcall InternalSaveToState(Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItem* AState, bool ANew);
	virtual void __fastcall BeforeAssignControl();
	virtual void __fastcall AfterAssignControl();
	virtual void __fastcall BeforeLoadState(Vcl::Tmsfncstatemanager::TTMSFNCStateManagerItem* AState);
	void __fastcall DoResponsiveResize(System::TObject* Sender);
	void __fastcall DoPreviewResize(System::TObject* Sender);
	virtual void __fastcall Recreate();
	__property TTMSFNCResponsiveManagerResizeMode Mode = {read=FMode, write=FMode, default=0};
	__property bool BlockLoadConstraints = {read=FBlockLoadConstraints, write=FBlockLoadConstraints, nodefault};
	__property TTMSFNCResponsiveManagerItems* States = {read=GetStates, write=SetStates};
	__property System::UnicodeString Version = {read=GetVersion};
	__property bool AutoLoadOnResize = {read=FAutoLoadOnResize, write=FAutoLoadOnResize, default=1};
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCCustomResponsiveManager(System::Classes::TComponent* AOwner)/* overload */;
	virtual void __fastcall LoadState(System::UnicodeString AStringValue)/* overload */;
	virtual void __fastcall LoadState(bool ABooleanValue)/* overload */;
	virtual void __fastcall LoadState(System::Extended ANumberValue)/* overload */;
	virtual void __fastcall LoadState()/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall SaveToNewState()/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall SaveToNewState(System::UnicodeString AStringValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall SaveToNewState(bool ABooleanValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall SaveToNewState(System::Extended ANumberValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall FindState(System::UnicodeString AStringValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall FindState(bool ABooleanValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall FindState(System::Extended ANumberValue)/* overload */;
	virtual TTMSFNCResponsiveManagerItem* __fastcall FindState()/* overload */;
	virtual void __fastcall Preview();
public:
	/* TTMSFNCCustomStateManager.Create */ inline __fastcall virtual TTMSFNCCustomResponsiveManager()/* overload */ : Vcl::Tmsfncstatemanager::TTMSFNCCustomStateManager() { }
	/* TTMSFNCCustomStateManager.Destroy */ inline __fastcall virtual ~TTMSFNCCustomResponsiveManager() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomResponsiveManager(HWND ParentWindow) : Vcl::Tmsfncstatemanager::TTMSFNCCustomStateManager(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManager : public TTMSFNCCustomResponsiveManager
{
	typedef TTMSFNCCustomResponsiveManager inherited;
	
public:
	virtual void __fastcall RegisterRuntimeClasses();
	
__published:
	__property ActiveState = {default=-1};
	__property AutoSave = {default=1};
	__property AutoLoadOnResize = {default=1};
	__property Version = {default=0};
	__property Mode = {default=0};
	__property States;
	__property Control;
	__property OnLoadStateCustom;
	__property OnBeforeLoadControlState;
	__property OnAfterLoadControlState;
	__property OnBeforeLoadState;
	__property OnAfterLoadState;
public:
	/* TTMSFNCCustomResponsiveManager.Create */ inline __fastcall virtual TTMSFNCResponsiveManager(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomResponsiveManager(AOwner) { }
	
public:
	/* TTMSFNCCustomStateManager.Create */ inline __fastcall virtual TTMSFNCResponsiveManager()/* overload */ : TTMSFNCCustomResponsiveManager() { }
	/* TTMSFNCCustomStateManager.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManager() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCResponsiveManager(HWND ParentWindow) : TTMSFNCCustomResponsiveManager(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncresponsivemanager */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCRESPONSIVEMANAGER)
using namespace Vcl::Tmsfncresponsivemanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncresponsivemanagerHPP
