// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCStyles.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncstylesHPP
#define Vcl_TmsfncstylesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Themes.hpp>
#include <System.JSON.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncstyles
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCAdaptToStyle;
typedef System::DelphiInterface<ITMSFNCAdaptToStyle> _di_ITMSFNCAdaptToStyle;
class DELPHICLASS TTMSFNCStyles;
__interface DELPHIINTERFACE ITMSFNCStylesManager;
typedef System::DelphiInterface<ITMSFNCStylesManager> _di_ITMSFNCStylesManager;
class DELPHICLASS TTMSFNCStylesManagerOptions;
class DELPHICLASS TTMSFNCStylesManager;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{3EFF288D-3927-4E86-8E9D-EF684B501C9E}") ITMSFNCAdaptToStyle  : public System::IInterface 
{
	virtual bool __fastcall GetAdaptToStyle() = 0 ;
	virtual void __fastcall SetAdaptToStyle(const bool Value) = 0 ;
	__property bool AdaptToStyle = {read=GetAdaptToStyle, write=SetAdaptToStyle};
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStyles : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	__classmethod virtual System::Uitypes::TColor __fastcall ExtractColor(const Vcl::Themes::TThemedElementDetails &AElement);
	__classmethod virtual System::Uitypes::TColor __fastcall ExtractColorTo(const Vcl::Themes::TThemedElementDetails &AElement);
	
public:
	__classmethod virtual bool __fastcall StyleServicesEnabled();
	__classmethod virtual bool __fastcall GetStyleLineFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundFillColorTo(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeBackgroundFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeBackgroundFillColorTo(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundStrokeColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderFillColorTo(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderStrokeColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleSelectionFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleSelectionFillColorTo(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleTextFontColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeTextFontColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleDefaultButtonFillColor(System::Uitypes::TColor &AColor);
	__classmethod virtual bool __fastcall GetStyleDefaultButtonStrokeColor(System::Uitypes::TColor &AColor);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCStyles() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCStyles() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCStylesManagerCanLoadStyleEvent)(System::TObject* Sender, System::UnicodeString AStyle, System::Classes::TComponent* AComponent, bool &ACanLoadStyle);

typedef void __fastcall (__closure *TTMSFNCStylesManagerStyleLoadedEvent)(System::TObject* Sender, System::UnicodeString AStyle, System::Classes::TComponent* AComponent);

typedef System::DynamicArray<System::UnicodeString> TTMSFNCStylesManagerFileArray;

typedef System::DynamicArray<System::Classes::TComponent*> TTMSFNCStylesManagerComponentArray;

__interface  INTERFACE_UUID("{88852C7F-B7B5-4FFA-BB47-6D95600CB1F3}") ITMSFNCStylesManager  : public System::IInterface 
{
	virtual TTMSFNCStylesManagerComponentArray __fastcall GetSubComponentArray() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesManagerOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAdaptFormColor;
	
public:
	__fastcall virtual TTMSFNCStylesManagerOptions();
	
__published:
	__property bool AdaptFormColor = {read=FAdaptFormColor, write=FAdaptFormColor, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCStylesManagerOptions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCStylesManager : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	System::UnicodeString FStyle;
	System::UnicodeString FStyleResource;
	Vcl::Forms::TCustomForm* FStyleForm;
	TTMSFNCStylesManagerCanLoadStyleEvent FOnCanLoadStyle;
	TTMSFNCStylesManagerStyleLoadedEvent FOnStyleLoaded;
	TTMSFNCStylesManagerOptions* FOptions;
	void __fastcall SetStyle(const System::UnicodeString Value);
	void __fastcall SetStyleResource(const System::UnicodeString Value);
	void __fastcall SetOptions(TTMSFNCStylesManagerOptions* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	void __fastcall InternalLoadStyleFromJSONValue(System::Json::TJSONValue* AJSONValue, TTMSFNCStylesManagerComponentArray AComponents);
	void __fastcall InternalLoadStyle(System::UnicodeString AValue, TTMSFNCStylesManagerComponentArray AComponents);
	virtual void __fastcall DoCanLoadStyle(System::UnicodeString AStyle, System::Classes::TComponent* AComponent, bool &ACanLoadStyle);
	virtual void __fastcall DoStyleLoaded(System::UnicodeString AStyle, System::Classes::TComponent* AComponent);
	
public:
	__fastcall virtual TTMSFNCStylesManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCStylesManager();
	virtual void __fastcall LoadStyleFromText(System::UnicodeString AText)/* overload */;
	virtual void __fastcall LoadStyleFromStream(System::Classes::TStream* AStream)/* overload */;
	virtual void __fastcall LoadStyleFromFile(System::UnicodeString AFile)/* overload */;
	virtual void __fastcall LoadStyleFromText(System::UnicodeString AText, TTMSFNCStylesManagerComponentArray AComponents)/* overload */;
	virtual void __fastcall LoadStyleFromStream(System::Classes::TStream* AStream, TTMSFNCStylesManagerComponentArray AComponents)/* overload */;
	virtual void __fastcall LoadStyleFromFile(System::UnicodeString AFile, TTMSFNCStylesManagerComponentArray AComponents)/* overload */;
	virtual void __fastcall LoadStyleFromResource(System::UnicodeString AResourceName, TTMSFNCStylesManagerComponentArray AComponents)/* overload */;
	virtual void __fastcall LoadStyleFromResource(System::UnicodeString AResourceName)/* overload */;
	virtual System::UnicodeString __fastcall GetStyleFromResource(System::UnicodeString AResourceName);
	virtual System::UnicodeString __fastcall GetStyleFromFile(System::UnicodeString AFile);
	virtual System::UnicodeString __fastcall CombineStyles(TTMSFNCStylesManagerFileArray AFiles);
	__property Vcl::Forms::TCustomForm* StyleForm = {read=FStyleForm, write=FStyleForm};
	
__published:
	__property TTMSFNCStylesManagerOptions* Options = {read=FOptions, write=SetOptions};
	__property System::UnicodeString Style = {read=FStyle, write=SetStyle};
	__property System::UnicodeString StyleResource = {read=FStyleResource, write=SetStyleResource};
	__property TTMSFNCStylesManagerCanLoadStyleEvent OnCanLoadStyle = {read=FOnCanLoadStyle, write=FOnCanLoadStyle};
	__property TTMSFNCStylesManagerStyleLoadedEvent OnStyleLoaded = {read=FOnStyleLoaded, write=FOnStyleLoaded};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCStylesManager(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString CSSStyleFileName;
}	/* namespace Tmsfncstyles */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCSTYLES)
using namespace Vcl::Tmsfncstyles;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncstylesHPP
