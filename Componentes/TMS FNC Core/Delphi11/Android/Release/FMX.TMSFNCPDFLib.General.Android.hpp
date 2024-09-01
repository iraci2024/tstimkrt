// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFLib.General.Android.pas' rev: 35.00 (Android)

#ifndef Fmx_Tmsfncpdflib_General_AndroidHPP
#define Fmx_Tmsfncpdflib_General_AndroidHPP

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
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdflib
{
namespace General
{
namespace Android
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


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGeneralPDFLibInitializer : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall TTMSFNCGeneralPDFLibInitializer();
	__fastcall virtual ~TTMSFNCGeneralPDFLibInitializer();
	void __fastcall InitializeFontFallBackList(System::Classes::TStrings* AList);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGeneralPDFLibFontInitializer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FTTFData;
	System::Classes::TStringStream* FTTFDataStream;
	System::UnicodeString FBase;
	float FSize;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* FUsedCharArray;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray FCharArray;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths FCharWidths;
	TTMSFNCGeneralPDFLibInitializer* FMainInitializer;
	bool FIsFixedWidth;
	
public:
	__fastcall TTMSFNCGeneralPDFLibFontInitializer(TTMSFNCGeneralPDFLibInitializer* const AMainInitializer, const System::UnicodeString ABase, const System::Uitypes::TFontStyles AStyle, const float ASize);
	__fastcall virtual ~TTMSFNCGeneralPDFLibFontInitializer();
	TTMSFNCGeneralPDFLibFontMetrics __fastcall GetFontMetrics();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray __fastcall GetCharArray();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths __fastcall GetCharWidths();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetUsedCharArray();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetGlyphIDs();
	unsigned __fastcall GetUnitsPerEm();
	int __fastcall GetTTFDataLength();
	__int64 __fastcall GetTTFDataCompressedLength();
	System::Classes::TStringStream* __fastcall GetTTFDataCompressed();
	void __fastcall CompressTTFData();
	void __fastcall InitializeCharWidths();
	void __fastcall InitializeFontFile();
	__property bool IsFixedWidth = {read=FIsFixedWidth, write=FIsFixedWidth, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Android */
}	/* namespace General */
}	/* namespace Tmsfncpdflib */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFLIB_GENERAL_ANDROID)
using namespace Fmx::Tmsfncpdflib::General::Android;
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
#endif	// Fmx_Tmsfncpdflib_General_AndroidHPP
