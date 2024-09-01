// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCStyles.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncstylesHPP
#define Fmx_TmsfncstylesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Types.hpp>
#include <System.UITypes.hpp>
#include <System.JSON.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
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

class PASCALIMPLEMENTATION TTMSFNCStyles : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static Fmx::Controls::_di_IScene FScene;
	
protected:
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall ExtractColor(Fmx::Graphics::TBitmap* ABitmap);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall ExtractColorTo(Fmx::Graphics::TBitmap* ABitmap);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall ParseBrush(Fmx::Graphics::TBrush* ABrush, bool ASecondColor);
	__classmethod virtual Fmx::Types::TFmxObject* __fastcall GetStyleSceneObject();
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleBackgroundFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TStrokeBrush* __fastcall GetStyleBackgroundStroke(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleDefaultButtonFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TStrokeBrush* __fastcall GetStyleDefaultButtonStroke(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall GetStyleEditTextColor(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleEditFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TStrokeBrush* __fastcall GetStyleEditStroke(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleHeaderFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TStrokeBrush* __fastcall GetStyleHeaderStroke(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleSelectionFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleAlternativeSelectionFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleAlternativeBackgroundFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TStrokeBrush* __fastcall GetStyleAlternativeBackgroundStroke(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleFocusFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleLineFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TFont* __fastcall GetStyleTextFont(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall GetStyleTextColor(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual Fmx::Graphics::TBrush* __fastcall GetStyleAlternateBackgroundFill(Fmx::Types::TFmxObject* AStyle);
	__classmethod virtual bool __fastcall IsTransparentStyle(Fmx::Types::TFmxObject* AStyle);
	
public:
	__classmethod virtual void __fastcall SetActiveScene(Fmx::Controls::_di_IScene AScene);
	__classmethod virtual bool __fastcall StyleServicesEnabled();
	__classmethod virtual bool __fastcall GetStyleLineFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundFillColorTo(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeBackgroundFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeBackgroundFillColorTo(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleBackgroundStrokeColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderFillColorTo(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleHeaderStrokeColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleSelectionFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleSelectionFillColorTo(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleTextFontColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleAlternativeTextFontColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleDefaultButtonFillColor(System::Uitypes::TAlphaColor &AColor);
	__classmethod virtual bool __fastcall GetStyleDefaultButtonStrokeColor(System::Uitypes::TAlphaColor &AColor);
	
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
	/* TObject.Create */ inline __fastcall TTMSFNCStyles() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCStyles() { }
	
};


typedef void __fastcall (__closure *TTMSFNCStylesManagerCanLoadStyleEvent)(System::TObject* Sender, System::UnicodeString AStyle, System::Classes::TComponent* AComponent, bool &ACanLoadStyle);

typedef void __fastcall (__closure *TTMSFNCStylesManagerStyleLoadedEvent)(System::TObject* Sender, System::UnicodeString AStyle, System::Classes::TComponent* AComponent);

typedef System::DynamicArray<System::UnicodeString> TTMSFNCStylesManagerFileArray;

typedef System::DynamicArray<System::Classes::TComponent*> TTMSFNCStylesManagerComponentArray;

__interface  INTERFACE_UUID("{88852C7F-B7B5-4FFA-BB47-6D95600CB1F3}") ITMSFNCStylesManager  : public System::IInterface 
{
	virtual TTMSFNCStylesManagerComponentArray __fastcall GetSubComponentArray() = 0 ;
};

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


class PASCALIMPLEMENTATION TTMSFNCStylesManager : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	System::UnicodeString FStyle;
	System::UnicodeString FStyleResource;
	Fmx::Forms::TCustomForm* FStyleForm;
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
	__property Fmx::Forms::TCustomForm* StyleForm = {read=FStyleForm, write=FStyleForm};
	
__published:
	__property TTMSFNCStylesManagerOptions* Options = {read=FOptions, write=SetOptions};
	__property System::UnicodeString Style = {read=FStyle, write=SetStyle};
	__property System::UnicodeString StyleResource = {read=FStyleResource, write=SetStyleResource};
	__property TTMSFNCStylesManagerCanLoadStyleEvent OnCanLoadStyle = {read=FOnCanLoadStyle, write=FOnCanLoadStyle};
	__property TTMSFNCStylesManagerStyleLoadedEvent OnStyleLoaded = {read=FOnStyleLoaded, write=FOnStyleLoaded};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString CSSStyleFileName;
}	/* namespace Tmsfncstyles */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCSTYLES)
using namespace Fmx::Tmsfncstyles;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncstylesHPP
