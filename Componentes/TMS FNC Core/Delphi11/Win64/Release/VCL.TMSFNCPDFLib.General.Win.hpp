// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCPDFLib.General.Win.pas' rev: 35.00 (Windows)

#ifndef Vcl_Tmsfncpdflib_General_WinHPP
#define Vcl_Tmsfncpdflib_General_WinHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCPDFCoreLibBase.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.Windows.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncpdflib
{
namespace General
{
namespace Win
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
	
private:
	HGDIOBJ FOldFontObject;
	HDC FDocumentHandle;
	
public:
	__fastcall TTMSFNCGeneralPDFLibInitializer();
	void __fastcall InitializeFontFallBackList(System::Classes::TStrings* AList);
	__fastcall virtual ~TTMSFNCGeneralPDFLibInitializer();
};


class PASCALIMPLEMENTATION TTMSFNCGeneralPDFLibFontInitializer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	HFONT FFontDC;
	tagLOGFONTW FLogFont;
	bool FTTFCreatePackage;
	unsigned FTTFSize;
	System::AnsiString FTTFData;
	System::Classes::TStringStream* FTTFDataStream;
	System::Word FTTFlags;
	unsigned FTTCIndex;
	System::UnicodeString FBase;
	float FSize;
	System::Uitypes::TFontStyles FStyle;
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* FUsedCharArray;
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray FCharArray;
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths FCharWidths;
	unsigned FUnitsPerEm;
	TTMSFNCGeneralPDFLibInitializer* FMainInitializer;
	bool FIsFixedWidth;
	
protected:
	void __fastcall InternalInitializeCharWidths(HDC ADocumentHandle);
	void * __fastcall GetFontTableData(HDC ADocHandle, System::AnsiString ATableName, System::TArray__1<System::Word> &AData);
	System::Classes::TStringStream* __fastcall CompressFontString(System::AnsiString AValue);
	
public:
	__fastcall TTMSFNCGeneralPDFLibFontInitializer(TTMSFNCGeneralPDFLibInitializer* const AMainInitializer, const System::UnicodeString ABase, const System::Uitypes::TFontStyles AStyle, const float ASize);
	__fastcall virtual ~TTMSFNCGeneralPDFLibFontInitializer();
	TTMSFNCGeneralPDFLibFontMetrics __fastcall GetFontMetrics();
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharArray __fastcall GetCharArray();
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFontCharWidths __fastcall GetCharWidths();
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetUsedCharArray();
	Vcl::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibUsedFontCharArray* __fastcall GetGlyphIDs();
	unsigned __fastcall GetUnitsPerEm();
	int __fastcall GetTTFDataLength();
	__int64 __fastcall GetTTFDataCompressedLength();
	System::Classes::TStringStream* __fastcall GetTTFDataCompressed();
	void __fastcall CompressTTFData();
	void __fastcall InitializeCharWidths();
	void __fastcall InitializeFontFile();
	__property bool IsFixedWidth = {read=FIsFixedWidth, write=FIsFixedWidth, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Win */
}	/* namespace General */
}	/* namespace Tmsfncpdflib */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCPDFLIB_GENERAL_WIN)
using namespace Vcl::Tmsfncpdflib::General::Win;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCPDFLIB_GENERAL)
using namespace Vcl::Tmsfncpdflib::General;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCPDFLIB)
using namespace Vcl::Tmsfncpdflib;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_Tmsfncpdflib_General_WinHPP
