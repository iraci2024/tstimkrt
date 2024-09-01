// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFGraphicsLibHTMLEngine.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdfgraphicslibhtmlengineHPP
#define Fmx_TmsfncpdfgraphicslibhtmlengineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCPDFLib.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfgraphicslibhtmlengine
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCPDFGraphicsLibHTMLLine;
class DELPHICLASS TTMSFNCPDFGraphicsLibHTMLLineBreak;
class DELPHICLASS TTMSFNCPDFGraphicsLibHTMLImage;
class DELPHICLASS TTMSFNCPDFGraphicsLibHTMLLines;
class DELPHICLASS TTMSFNCPDFGraphicsLibHTMLEngine;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibHTMLLine : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Uitypes::TAlphaColor FTextColor;
	int FFontSize;
	System::Uitypes::TFontStyles FFontStyle;
	bool FLineBreak;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	bool FHasImage;
	System::UnicodeString FFontName;
	bool FIsURL;
	System::UnicodeString FText;
	System::UnicodeString FURL;
	System::Uitypes::TAlphaColor FBackgroundColor;
	float FBitmapHeight;
	float FBitmapWidth;
	bool FSuperscript;
	bool FSubscript;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FTextAlign;
	float FOffset;
	bool FBullet;
	System::Uitypes::TAlphaColor FLineColor;
	bool FIsLine;
	void __fastcall SetBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	
public:
	__fastcall virtual TTMSFNCPDFGraphicsLibHTMLLine();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibHTMLLine();
	void __fastcall Reset();
	__property bool LineBreak = {read=FLineBreak, write=FLineBreak, nodefault};
	__property System::Uitypes::TAlphaColor LineColor = {read=FLineColor, write=FLineColor, nodefault};
	__property System::Uitypes::TAlphaColor BackgroundColor = {read=FBackgroundColor, write=FBackgroundColor, nodefault};
	__property System::Uitypes::TAlphaColor TextColor = {read=FTextColor, write=FTextColor, nodefault};
	__property bool HasImage = {read=FHasImage, write=FHasImage, nodefault};
	__property bool IsUrl = {read=FIsURL, write=FIsURL, nodefault};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property float BitmapWidth = {read=FBitmapWidth, write=FBitmapWidth};
	__property float BitmapHeight = {read=FBitmapHeight, write=FBitmapHeight};
	__property System::UnicodeString FontName = {read=FFontName, write=FFontName};
	__property System::Uitypes::TFontStyles FontStyle = {read=FFontStyle, write=FFontStyle, nodefault};
	__property int FontSize = {read=FFontSize, write=FFontSize, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
	__property bool Bullet = {read=FBullet, write=FBullet, nodefault};
	__property float Offset = {read=FOffset, write=FOffset};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign TextAlign = {read=FTextAlign, write=FTextAlign, nodefault};
	__property bool Subscript = {read=FSubscript, write=FSubscript, nodefault};
	__property bool Superscript = {read=FSuperscript, write=FSuperscript, nodefault};
	__property bool IsLine = {read=FIsLine, write=FIsLine, nodefault};
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


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibHTMLLineBreak : public TTMSFNCPDFGraphicsLibHTMLLine
{
	typedef TTMSFNCPDFGraphicsLibHTMLLine inherited;
	
public:
	__fastcall virtual TTMSFNCPDFGraphicsLibHTMLLineBreak();
public:
	/* TTMSFNCPDFGraphicsLibHTMLLine.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibHTMLLineBreak() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibHTMLImage : public TTMSFNCPDFGraphicsLibHTMLLine
{
	typedef TTMSFNCPDFGraphicsLibHTMLLine inherited;
	
public:
	__fastcall virtual TTMSFNCPDFGraphicsLibHTMLImage();
public:
	/* TTMSFNCPDFGraphicsLibHTMLLine.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibHTMLImage() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibHTMLLines : public System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFGraphicsLibHTMLLine*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFGraphicsLibHTMLLine*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibHTMLLines() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFGraphicsLibHTMLLine*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFGraphicsLibHTMLLine*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFGraphicsLibHTMLEngine_TTMSFNCPDFGraphicsLibHTMLLine>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLLines(TTMSFNCPDFGraphicsLibHTMLLine* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFGraphicsLibHTMLLine*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibHTMLEngine : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod TTMSFNCPDFGraphicsLibHTMLLines* __fastcall ParseHTML(System::UnicodeString AHTML, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer = (Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer*)(0x0));
	__classmethod System::Types::TRectF __fastcall DrawHTMLText(Fmx::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib, System::UnicodeString Text, const System::Types::TRectF &Rect, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer = (Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer*)(0x0), bool Paging = false, float AScale = 1.000000E+00f, bool Calculate = false)/* overload */;
	__classmethod System::Types::TRectF __fastcall DrawHTMLText(Fmx::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib, System::UnicodeString Text, const System::Types::TPointF &Point, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer = (Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer*)(0x0), float AScale = 1.000000E+00f, bool Calculate = false)/* overload */;
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
	/* TObject.Create */ inline __fastcall TTMSFNCPDFGraphicsLibHTMLEngine() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibHTMLEngine() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncpdfgraphicslibhtmlengine */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFGRAPHICSLIBHTMLENGINE)
using namespace Fmx::Tmsfncpdfgraphicslibhtmlengine;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfgraphicslibhtmlengineHPP
