// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCCustomComponent.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnccustomcomponentHPP
#define Vcl_TmsfnccustomcomponentHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCPersistence.hpp>
#include <System.TypInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfnccustomcomponent
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomComponent;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTMSFNCCustomComponentCanSavePropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);

typedef void __fastcall (__closure *TTMSFNCCustomComponentCanLoadPropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);

class PASCALIMPLEMENTATION TTMSFNCCustomComponent : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	bool FStored;
	static bool FBlockPersistenceInterface;
	int FDesigntimeFormPixelsPerInch;
	bool FAppearancePersisting;
	bool FAdaptToStyle;
	TTMSFNCCustomComponentCanSavePropertyEvent FOnCanSaveProperty;
	TTMSFNCCustomComponentCanLoadPropertyEvent FOnCanLoadProperty;
	
protected:
	virtual void __fastcall DoCanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);
	virtual void __fastcall DoCanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);
	virtual bool __fastcall IsAppearanceProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual bool __fastcall CanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual bool __fastcall CanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual System::UnicodeString __fastcall GetVersion();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetTipsURL();
	virtual NativeUInt __fastcall GetInstance();
	DYNAMIC void __fastcall ChangeScale(int M, int D, bool isDpiChange)/* overload */;
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall Paint();
	virtual void __fastcall Loaded();
	virtual void __fastcall RegisterRuntimeClasses();
	__property bool AdaptToStyle = {read=FAdaptToStyle, write=SetAdaptToStyle, default=0};
	
public:
	__fastcall virtual TTMSFNCCustomComponent(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetBounds(int X, int Y, int AWidth, int AHeight);
	__property bool Stored = {read=FStored, write=FStored, nodefault};
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual bool __fastcall IsDesignTime();
	virtual bool __fastcall IsLoading();
	virtual bool __fastcall IsReading();
	virtual bool __fastcall IsDesigning();
	virtual bool __fastcall IsDestroying();
	virtual void __fastcall SaveSettingsToFile(System::UnicodeString AFileName, bool AAppearanceOnly = false);
	virtual void __fastcall LoadSettingsFromFile(System::UnicodeString AFileName);
	virtual void __fastcall SaveSettingsToStream(System::Classes::TStream* AStream, bool AAppearanceOnly = false);
	virtual void __fastcall LoadSettingsFromStream(System::Classes::TStream* AStream);
	/* static */ __property bool BlockPersistenceInterface = {read=FBlockPersistenceInterface, write=FBlockPersistenceInterface, nodefault};
	__property int DesigntimeFormPixelsPerInch = {read=FDesigntimeFormPixelsPerInch, nodefault};
	
__published:
	__property Visible = {default=0};
	__property Width;
	__property Height;
	__property TTMSFNCCustomComponentCanSavePropertyEvent OnCanSaveProperty = {read=FOnCanSaveProperty, write=FOnCanSaveProperty};
	__property TTMSFNCCustomComponentCanLoadPropertyEvent OnCanLoadProperty = {read=FOnCanLoadProperty, write=FOnCanLoadProperty};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTMSFNCCustomComponent() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomComponent(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
	/* Hoisted overloads: */
	
protected:
	DYNAMIC inline void __fastcall  ChangeScale(int M, int D){ Vcl::Controls::TControl::ChangeScale(M, D); }
	
private:
	void *__ITMSFNCPersistence;	// Vcl::Tmsfncpersistence::ITMSFNCPersistence 
	void *__ITMSFNCProductInfo;	// Vcl::Tmsfnctypes::ITMSFNCProductInfo 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}
	operator Vcl::Tmsfncpersistence::_di_ITMSFNCPersistence()
	{
		Vcl::Tmsfncpersistence::_di_ITMSFNCPersistence intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfncpersistence::ITMSFNCPersistence*(void) { return (Vcl::Tmsfncpersistence::ITMSFNCPersistence*)&__ITMSFNCPersistence; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C53329EA-7B3A-4507-AD9E-88ACD6A85054}
	operator Vcl::Tmsfnctypes::_di_ITMSFNCProductInfo()
	{
		Vcl::Tmsfnctypes::_di_ITMSFNCProductInfo intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfnctypes::ITMSFNCProductInfo*(void) { return (Vcl::Tmsfnctypes::ITMSFNCProductInfo*)&__ITMSFNCProductInfo; }
	#endif
	
};


typedef System::TMetaClass* TTMSFNCCustomComponentClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfnccustomcomponent */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCCUSTOMCOMPONENT)
using namespace Vcl::Tmsfnccustomcomponent;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnccustomcomponentHPP
