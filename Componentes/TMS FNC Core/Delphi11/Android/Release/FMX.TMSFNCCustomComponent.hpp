// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCustomComponent.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccustomcomponentHPP
#define Fmx_TmsfnccustomcomponentHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCPersistence.hpp>
#include <System.TypInfo.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccustomcomponent
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomComponent;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTMSFNCCustomComponentCanSavePropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);

typedef void __fastcall (__closure *TTMSFNCCustomComponentCanLoadPropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomComponent : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
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
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall Paint();
	virtual void __fastcall Loaded();
	virtual void __fastcall RegisterRuntimeClasses();
	__property bool AdaptToStyle = {read=FAdaptToStyle, write=SetAdaptToStyle, default=0};
	
public:
	__fastcall virtual TTMSFNCCustomComponent(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetBounds(float X, float Y, float AWidth, float AHeight);
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
	__property Size;
	__property Position;
	__property Visible = {default=1};
	__property Width;
	__property Height;
	__property TTMSFNCCustomComponentCanSavePropertyEvent OnCanSaveProperty = {read=FOnCanSaveProperty, write=FOnCanSaveProperty};
	__property TTMSFNCCustomComponentCanLoadPropertyEvent OnCanLoadProperty = {read=FOnCanLoadProperty, write=FOnCanLoadProperty};
public:
	/* TControl.Destroy */ inline __fastcall virtual ~TTMSFNCCustomComponent() { }
	
private:
	void *__ITMSFNCPersistence;	// Fmx::Tmsfncpersistence::ITMSFNCPersistence 
	void *__ITMSFNCProductInfo;	// Fmx::Tmsfnctypes::ITMSFNCProductInfo 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}
	operator Fmx::Tmsfncpersistence::_di_ITMSFNCPersistence()
	{
		Fmx::Tmsfncpersistence::_di_ITMSFNCPersistence intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncpersistence::ITMSFNCPersistence*(void) { return (Fmx::Tmsfncpersistence::ITMSFNCPersistence*)&__ITMSFNCPersistence; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C53329EA-7B3A-4507-AD9E-88ACD6A85054}
	operator Fmx::Tmsfnctypes::_di_ITMSFNCProductInfo()
	{
		Fmx::Tmsfnctypes::_di_ITMSFNCProductInfo intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfnctypes::ITMSFNCProductInfo*(void) { return (Fmx::Tmsfnctypes::ITMSFNCProductInfo*)&__ITMSFNCProductInfo; }
	#endif
	
};

#pragma pack(pop)

_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomComponentClass);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfnccustomcomponent */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCUSTOMCOMPONENT)
using namespace Fmx::Tmsfnccustomcomponent;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccustomcomponentHPP
