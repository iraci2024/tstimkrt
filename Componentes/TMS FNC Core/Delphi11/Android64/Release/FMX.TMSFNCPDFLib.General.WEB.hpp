// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFLib.General.WEB.pas' rev: 35.00 (Android)

#ifndef Fmx_Tmsfncpdflib_General_WebHPP
#define Fmx_Tmsfncpdflib_General_WebHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCPDFCoreLibBase.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdflib
{
namespace General
{
namespace Web
{
//-- forward type declarations -----------------------------------------------
struct TTMSFNCGeneralPDFLibFontMetrics;
class DELPHICLASS TTMSFNCGeneralPDFLibInitializer;
class DELPHICLASS TTMSFNCGeneralPDFLibFontInitializer;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TTMSFNCGeneralPDFLibFontMetrics
{
public:
	int CapHeight;
	int Ascent;
	int Descent;
	System::Types::TRect FontBox;
	int ItalicAngle;
	bool Fixed;
	bool TrueType;
};


class PASCALIMPLEMENTATION TTMSFNCGeneralPDFLibInitializer : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall TTMSFNCGeneralPDFLibInitializer();
	void __fastcall InitializeFontFallBackList(System::Classes::TStrings* AList);
	__fastcall virtual ~TTMSFNCGeneralPDFLibInitializer();
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


class PASCALIMPLEMENTATION TTMSFNCGeneralPDFLibFontInitializer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FFullFont;
	System::UnicodeString FBase;
	float FSize;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* FUsedCharArray;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* FGlyphIDs;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray FCharArray;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths FCharWidths;
	unsigned FUnitsPerEm;
	TTMSFNCGeneralPDFLibInitializer* FMainInitializer;
	bool FIsFixedWidth;
	
protected:
	void __fastcall InternalInitializeFontFile();
	
public:
	__fastcall TTMSFNCGeneralPDFLibFontInitializer(TTMSFNCGeneralPDFLibInitializer* const AMainInitializer, const System::UnicodeString ABase, const System::Uitypes::TFontStyles AStyle, const float ASize);
	__fastcall virtual ~TTMSFNCGeneralPDFLibFontInitializer();
	TTMSFNCGeneralPDFLibFontMetrics __fastcall GetFontMetrics();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray __fastcall GetCharArray();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths __fastcall GetCharWidths();
	unsigned __fastcall GetUnitsPerEM();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetUsedCharArray();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetGlyphIds();
	unsigned __fastcall UnitsPerEm();
	int __fastcall GetTTFDataLength();
	__int64 __fastcall GetTTFDataCompressedLength();
	System::Classes::TStringStream* __fastcall GetTTFDataCompressed();
	void __fastcall CompressTTFData();
	void __fastcall InitializeCharWidths();
	void __fastcall InitializeFontFile();
	__property bool IsFixedWidth = {read=FIsFixedWidth, write=FIsFixedWidth, nodefault};
	__property bool FullFont = {read=FFullFont, write=FFullFont, nodefault};
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


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Web */
}	/* namespace General */
}	/* namespace Tmsfncpdflib */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFLIB_GENERAL_WEB)
using namespace Fmx::Tmsfncpdflib::General::Web;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFLIB_GENERAL)
using namespace Fmx::Tmsfncpdflib::General;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFLIB)
using namespace Fmx::Tmsfncpdflib;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_Tmsfncpdflib_General_WebHPP
