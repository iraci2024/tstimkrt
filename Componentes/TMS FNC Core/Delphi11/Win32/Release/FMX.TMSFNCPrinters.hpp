// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPrinters.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncprintersHPP
#define Fmx_TmsfncprintersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Printer.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncprinters
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE TTMSFNCPrinterDrawContentCallBack;
typedef System::DelphiInterface<TTMSFNCPrinterDrawContentCallBack> _di_TTMSFNCPrinterDrawContentCallBack;
class DELPHICLASS TTMSFNCPrinter;
//-- type declarations -------------------------------------------------------
__interface TTMSFNCPrinterDrawContentCallBack  : public System::IInterface 
{
	virtual void __fastcall Invoke() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPrinter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Fmx::Tmsfncgraphics::TTMSFNCGraphics* FGraphics;
	_di_TTMSFNCPrinterDrawContentCallBack FOnDrawContent;
	int __fastcall GetDPI();
	void __fastcall SetDevice(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetDevice();
	System::Uitypes::TPrinterOrientation __fastcall GetOrientation();
	int __fastcall GetPageHeight();
	int __fastcall GetPageNumber();
	int __fastcall GetPageWidth();
	void __fastcall SetOrientation(const System::Uitypes::TPrinterOrientation Value);
	
public:
	__fastcall virtual TTMSFNCPrinter();
	__fastcall virtual ~TTMSFNCPrinter();
	void __fastcall BeginDoc();
	void __fastcall EndDoc();
	void __fastcall NewPage();
	__property System::UnicodeString Device = {read=GetDevice, write=SetDevice};
	__property int DPI = {read=GetDPI, nodefault};
	__property Fmx::Tmsfncgraphics::TTMSFNCGraphics* Graphics = {read=FGraphics};
	__property System::Uitypes::TPrinterOrientation Orientation = {read=GetOrientation, write=SetOrientation, nodefault};
	__property int PageHeight = {read=GetPageHeight, nodefault};
	__property int PageNumber = {read=GetPageNumber, nodefault};
	__property int PageWidth = {read=GetPageWidth, nodefault};
	__property _di_TTMSFNCPrinterDrawContentCallBack OnDrawContent = {read=FOnDrawContent, write=FOnDrawContent};
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

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x2);
static const System::Int8 PRINTDPI = System::Int8(0x48);
extern DELPHI_PACKAGE TTMSFNCPrinter* __fastcall TMSFNCPrinter(void);
}	/* namespace Tmsfncprinters */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPRINTERS)
using namespace Fmx::Tmsfncprinters;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncprintersHPP
