// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFRichTextLib.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdfrichtextlibHPP
#define Fmx_TmsfncpdfrichtextlibHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCPDFCoreLibBase.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfrichtextlib
{
//-- forward type declarations -----------------------------------------------
struct TTMSFNCPDFRichTextLibRange;
struct TTMSFNCPDFRichTextLibFontValue;
struct TTMSFNCPDFRichTextLibTabStop;
class DELPHICLASS TTMSFNCPDFRichTextLibTabStops;
struct TTMSFNCPDFRichTextLibParagraphStyle;
struct TTMSFNCPDFRichTextLibAttributeValue;
__interface DELPHIINTERFACE ITMSFNCCustomPDFRichTextLib;
typedef System::DelphiInterface<ITMSFNCCustomPDFRichTextLib> _di_ITMSFNCCustomPDFRichTextLib;
__interface DELPHIINTERFACE ITMSFNCPDFRichTextLibService;
typedef System::DelphiInterface<ITMSFNCPDFRichTextLibService> _di_ITMSFNCPDFRichTextLibService;
class DELPHICLASS TTMSFNCCustomPDFRichTextLib;
class DELPHICLASS TTMSFNCPDFRichTextLibList;
class DELPHICLASS TTMSFNCPDFRichTextLibFactoryService;
class DELPHICLASS TTMSFNCPDFRichTextLib;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TTMSFNCPDFRichTextLibRange
{
public:
	int location;
	int length;
};


enum DECLSPEC_DENUM TTMSFNCPDFRichTextLibDataType : unsigned char { dtArchivedXMLDocumentType, dtPlainTextDocumentType, dtRTFTextDocumentType, dtRTFDTextDocumentType, dtHTMLTextDocumentType, dtSimpleTextDocumentType, dtDocFormatTextDocumentType, dtWordMLTextDocumentType, dtOfficeOpenXMLTextDocumentType, dtWebArchiveTextDocumentType, dtOpenDocumentTextDocumentType };

enum DECLSPEC_DENUM TTMSFNCPDFRichTextLibAttributeName : unsigned char { anFontAttributeName, anParagraphStyleAttributeName, anForegroundColorAttributeName, anBackgroundColorAttributeName, anLigatureAttributeName, anKernAttributeName, anStrikethroughStyleAttributeName, anUnderlineStyleAttributeName, anStrokeColorAttributeName, anStrokeWidthAttributeName, anShadowAttributeName, anTextEffectAttributeName, anAttachmentAttributeName, anLinkAttributeName, anToolTipAttributeName, anBaselineOffsetAttributeName, anUnderlineColorAttributeName, anStrikethroughColorAttributeName, anObliquenessAttributeName, anExpansionAttributeName, anWritingDirectionAttributeName, anVerticalGlyphFormAttributeName };

enum DECLSPEC_DENUM TTMSFNCPDFRichTextLibUnderlineStyle : unsigned char { usUnderlineStyleNone, usUnderlineStyleSingle, usUnderlineStyleThick, usUnderlineStyleDouble, usUnderlinePatternSolid, usUnderlinePatternDot, usUnderlinePatternDash, usUnderlinePatternDashDot, usUnderlinePatternDashDotDot, usUnderlineByWord };

typedef System::Set<TTMSFNCPDFRichTextLibUnderlineStyle, TTMSFNCPDFRichTextLibUnderlineStyle::usUnderlineStyleNone, TTMSFNCPDFRichTextLibUnderlineStyle::usUnderlineByWord> TTMSFNCPDFRichTextLibUnderlineStyles;

struct DECLSPEC_DRECORD TTMSFNCPDFRichTextLibFontValue
{
public:
	System::UnicodeString FontFamily;
	System::UnicodeString FontName;
	float FontSize;
};


struct DECLSPEC_DRECORD TTMSFNCPDFRichTextLibTabStop
{
public:
	float Location;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Alignment;
};


class PASCALIMPLEMENTATION TTMSFNCPDFRichTextLibTabStops : public System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_TTMSFNCPDFRichTextLibTabStop>.Create */ inline __fastcall TTMSFNCPDFRichTextLibTabStops()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_TTMSFNCPDFRichTextLibTabStop>.Create */ inline __fastcall TTMSFNCPDFRichTextLibTabStops(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFRichTextLibTabStop> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_TTMSFNCPDFRichTextLibTabStop>.Create */ inline __fastcall TTMSFNCPDFRichTextLibTabStops(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFRichTextLibTabStop>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_TTMSFNCPDFRichTextLibTabStop>.Create */ inline __fastcall TTMSFNCPDFRichTextLibTabStops(const TTMSFNCPDFRichTextLibTabStop *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCPDFRichTextLibTabStop>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_TTMSFNCPDFRichTextLibTabStop>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFRichTextLibTabStops() { }
	
};


struct DECLSPEC_DRECORD TTMSFNCPDFRichTextLibParagraphStyle
{
public:
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Alignment;
	float FirstLineHeadIndent;
	float HeadIndent;
	float TailIndent;
	float LineHeightMultiple;
	float MaximumLineHeight;
	float MinimumLineHeight;
	float LineSpacing;
	float ParagraphSpacing;
	float ParagraphSpacingBefore;
	TTMSFNCPDFRichTextLibTabStops* TabStops;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibLineBreakMode LineBreakMode;
	float HyphenationFactor;
};


struct DECLSPEC_DRECORD TTMSFNCPDFRichTextLibAttributeValue
{
public:
	TTMSFNCPDFRichTextLibParagraphStyle ParagraphStyle;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* Bitmap;
	System::UnicodeString BitmapFile;
	System::Uitypes::TAlphaColor ForegroundColor;
	System::Uitypes::TAlphaColor BackgroundColor;
	System::Uitypes::TAlphaColor UnderlineColor;
	System::Uitypes::TAlphaColor StrikethroughColor;
	float BaselineOffset;
	System::Uitypes::TAlphaColor StrokeColor;
	float StrokeWidth;
	System::UnicodeString URL;
	System::UnicodeString ToolTip;
	TTMSFNCPDFRichTextLibUnderlineStyles Underline;
	TTMSFNCPDFRichTextLibUnderlineStyles Strikethrough;
	bool ApplyBold;
	bool Bold;
	bool ApplyItalic;
	bool Italic;
	bool ApplyFontName;
	System::UnicodeString FontName;
	System::UnicodeString FontFamily;
	bool ApplyFontSize;
	float FontSize;
};


enum DECLSPEC_DENUM TTMSFNCPDFRichTextLibDataDetectorType : unsigned char { dtDataDetectorTypePhoneNumber, dtDataDetectorTypeLink, dtDataDetectorTypeAddress, dtDataDetectorTypeCalendarEvent, dtDataDetectorTypeNone, dtDataDetectorTypeAll };

typedef System::Set<TTMSFNCPDFRichTextLibDataDetectorType, TTMSFNCPDFRichTextLibDataDetectorType::dtDataDetectorTypePhoneNumber, TTMSFNCPDFRichTextLibDataDetectorType::dtDataDetectorTypeAll> TTMSFNCPDFRichTextLibDataDetectorTypes;

__interface  INTERFACE_UUID("{E4E9AE27-E487-4D46-BA6C-33B6BA6E54E4}") ITMSFNCCustomPDFRichTextLib  : public System::IInterface 
{
	virtual void __fastcall SetCanvas(void * ACanvas) = 0 ;
	virtual TTMSFNCPDFRichTextLibRange __fastcall GetSelection() = 0 ;
	virtual System::Types::TRectF __fastcall Draw(const System::Types::TRectF &Rect, bool Calculate = false) = 0 /* overload */;
	virtual int __fastcall Draw(const System::Types::TRectF &Rect, int Columns, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual int __fastcall Draw(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibRectArray Rects, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual TTMSFNCPDFRichTextLibAttributeValue __fastcall GetValues(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall InitializeValues(TTMSFNCPDFRichTextLibAttributeValue &AValues) = 0 ;
	virtual System::UnicodeString __fastcall GetURL(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetURL(System::UnicodeString AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall AddBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* AValue, float ALineHeight = -1.000000E+00f, int ALocation = 0xffffffff) = 0 ;
	virtual void __fastcall AddBitmapFromFile(System::UnicodeString AValue, float ALineHeight = -1.000000E+00f, int ALocation = 0xffffffff) = 0 ;
	virtual float __fastcall GetSubscript(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetSubscript(float AValue = -2.000000E+00f, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual float __fastcall GetBaselineOffset(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetBaselineOffset(float AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual float __fastcall GetSuperscript(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetSuperscript(float AValue = 2.000000E+00f, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Uitypes::TAlphaColor __fastcall GetStrokeColor(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetStrokeColor(System::Uitypes::TAlphaColor AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual float __fastcall GetStrokeWidth(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetStrokeWidth(float AWidth, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual TTMSFNCPDFRichTextLibUnderlineStyles __fastcall GetUnderline(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetUnderline(TTMSFNCPDFRichTextLibUnderlineStyles AValue = (TTMSFNCPDFRichTextLibUnderlineStyles() << TTMSFNCPDFRichTextLibUnderlineStyle::usUnderlineStyleSingle ), int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Uitypes::TAlphaColor __fastcall GetUnderlineColor(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetUnderlineColor(System::Uitypes::TAlphaColor AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual TTMSFNCPDFRichTextLibUnderlineStyles __fastcall GetStrikethrough(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetStrikethrough(TTMSFNCPDFRichTextLibUnderlineStyles AValue = (TTMSFNCPDFRichTextLibUnderlineStyles() << TTMSFNCPDFRichTextLibUnderlineStyle::usUnderlineStyleSingle ), int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Uitypes::TAlphaColor __fastcall GetStrikethroughColor(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetStrikethroughColor(System::Uitypes::TAlphaColor AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual TTMSFNCPDFRichTextLibFontValue __fastcall GetFont(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetFont(System::UnicodeString AName, float ASize = -1.000000E+00f, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::UnicodeString __fastcall GetToolTip(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetToolTip(System::UnicodeString AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual float __fastcall GetFontSize(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetFontSize(float ASize, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Uitypes::TAlphaColor __fastcall GetBackgroundColor(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetBackgroundColor(System::Uitypes::TAlphaColor AColor, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Uitypes::TAlphaColor __fastcall GetForegroundColor(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetForegroundColor(System::Uitypes::TAlphaColor AColor, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual bool __fastcall GetBold(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetBold(bool AValue = true, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual bool __fastcall GetItalic(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetItalic(bool AValue = true, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall SetParagraphStyle(const TTMSFNCPDFRichTextLibParagraphStyle &AValue, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual TTMSFNCPDFRichTextLibParagraphStyle __fastcall GetParagraphStyle(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::Classes::TMemoryStream* __fastcall ExportToStream() = 0 ;
	virtual void __fastcall ImportFromStream(System::Classes::TMemoryStream* AStream) = 0 ;
	virtual void __fastcall ExportData(System::UnicodeString AFileName, const TTMSFNCPDFRichTextLibRange &ARange, TTMSFNCPDFRichTextLibDataType AType = (TTMSFNCPDFRichTextLibDataType)(0x0)) = 0 /* overload */;
	virtual void __fastcall ExportData(System::UnicodeString AFileName, TTMSFNCPDFRichTextLibDataType AType = (TTMSFNCPDFRichTextLibDataType)(0x0)) = 0 /* overload */;
	virtual void __fastcall ImportData(System::UnicodeString AFileName, TTMSFNCPDFRichTextLibDataType AType = (TTMSFNCPDFRichTextLibDataType)(0x0)) = 0 ;
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall ReplaceText(System::UnicodeString AText, int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual void __fastcall DeleteText(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 ;
	virtual System::UnicodeString __fastcall GetPlainTextRange(int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 /* overload */;
	virtual System::UnicodeString __fastcall GetPlainText() = 0 /* overload */;
	virtual int __fastcall GetTextLength() = 0 ;
	virtual System::UnicodeString __fastcall GetRichTextRange(TTMSFNCPDFRichTextLibDataType ADataType = (TTMSFNCPDFRichTextLibDataType)(0x0), int AStart = 0xffffffff, int ALength = 0xffffffff) = 0 /* overload */;
	virtual System::UnicodeString __fastcall GetRichText(TTMSFNCPDFRichTextLibDataType ADataType = (TTMSFNCPDFRichTextLibDataType)(0x0)) = 0 /* overload */;
	virtual void __fastcall SetRichText(System::UnicodeString ARichText, TTMSFNCPDFRichTextLibDataType ADataType = (TTMSFNCPDFRichTextLibDataType)(0x0)) = 0 ;
	virtual System::UnicodeString __fastcall GetDataText() = 0 ;
	virtual void __fastcall SetDataText(const System::UnicodeString Value) = 0 ;
	virtual System::UnicodeString __fastcall GetText() = 0 ;
	virtual void __fastcall SetText(const System::UnicodeString Value) = 0 ;
	__property System::UnicodeString DataText = {read=GetDataText, write=SetDataText};
	__property System::UnicodeString Text = {read=GetText, write=SetText};
};

__interface  INTERFACE_UUID("{6E174355-3C72-46C3-AC61-454E9711B9BD}") ITMSFNCPDFRichTextLibService  : public System::IInterface 
{
	virtual _di_ITMSFNCCustomPDFRichTextLib __fastcall CreatePDFRichTextLib() = 0 ;
};

class PASCALIMPLEMENTATION TTMSFNCCustomPDFRichTextLib : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	_di_ITMSFNCCustomPDFRichTextLib FPDFRichTextLib;
	
public:
	__fastcall virtual TTMSFNCCustomPDFRichTextLib();
	__fastcall virtual ~TTMSFNCCustomPDFRichTextLib();
	_di_ITMSFNCCustomPDFRichTextLib __fastcall GetPDFRichTextLib();
};


class PASCALIMPLEMENTATION TTMSFNCPDFRichTextLibList : public System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib>
{
	typedef System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_ITMSFNCCustomPDFRichTextLib>.Create */ inline __fastcall TTMSFNCPDFRichTextLibList()/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_ITMSFNCCustomPDFRichTextLib>.Create */ inline __fastcall TTMSFNCPDFRichTextLibList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<_di_ITMSFNCCustomPDFRichTextLib> > AComparer)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_ITMSFNCCustomPDFRichTextLib>.Create */ inline __fastcall TTMSFNCPDFRichTextLibList(System::Generics::Collections::TEnumerable__1<_di_ITMSFNCCustomPDFRichTextLib>* const Collection)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_ITMSFNCCustomPDFRichTextLib>.Create */ inline __fastcall TTMSFNCPDFRichTextLibList(const _di_ITMSFNCCustomPDFRichTextLib *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFRichTextLib>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFRichTextLib_ITMSFNCCustomPDFRichTextLib>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFRichTextLibList() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFRichTextLibFactoryService : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTMSFNCPDFRichTextLibList* FPDFRichTextLibs;
	
protected:
	virtual _di_ITMSFNCCustomPDFRichTextLib __fastcall DoCreatePDFRichTextLib() = 0 ;
	_di_ITMSFNCCustomPDFRichTextLib __fastcall CreatePDFRichTextLib();
	
public:
	__fastcall TTMSFNCPDFRichTextLibFactoryService();
	__fastcall virtual ~TTMSFNCPDFRichTextLibFactoryService();
private:
	void *__ITMSFNCPDFRichTextLibService;	// ITMSFNCPDFRichTextLibService 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6E174355-3C72-46C3-AC61-454E9711B9BD}
	operator _di_ITMSFNCPDFRichTextLibService()
	{
		_di_ITMSFNCPDFRichTextLibService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCPDFRichTextLibService*(void) { return (ITMSFNCPDFRichTextLibService*)&__ITMSFNCPDFRichTextLibService; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFRichTextLib : public TTMSFNCCustomPDFRichTextLib
{
	typedef TTMSFNCCustomPDFRichTextLib inherited;
	
public:
	/* TTMSFNCCustomPDFRichTextLib.Create */ inline __fastcall virtual TTMSFNCPDFRichTextLib() : TTMSFNCCustomPDFRichTextLib() { }
	/* TTMSFNCCustomPDFRichTextLib.Destroy */ inline __fastcall virtual ~TTMSFNCPDFRichTextLib() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncpdfrichtextlib */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFRICHTEXTLIB)
using namespace Fmx::Tmsfncpdfrichtextlib;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfrichtextlibHPP
