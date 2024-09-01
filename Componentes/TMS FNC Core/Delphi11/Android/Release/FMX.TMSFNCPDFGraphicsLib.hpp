// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFGraphicsLib.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdfgraphicslibHPP
#define Fmx_TmsfncpdfgraphicslibHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCPDFCoreLibBase.hpp>
#include <FMX.TMSFNCPDFRichTextLib.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfgraphicslib
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCCustomPDFGraphicsLib;
typedef System::DelphiInterface<ITMSFNCCustomPDFGraphicsLib> _di_ITMSFNCCustomPDFGraphicsLib;
__interface DELPHIINTERFACE ITMSFNCCustomPDFInitializationLib;
typedef System::DelphiInterface<ITMSFNCCustomPDFInitializationLib> _di_ITMSFNCCustomPDFInitializationLib;
__interface DELPHIINTERFACE ITMSFNCCustomPDFGraphicsExLib;
typedef System::DelphiInterface<ITMSFNCCustomPDFGraphicsExLib> _di_ITMSFNCCustomPDFGraphicsExLib;
__interface DELPHIINTERFACE ITMSFNCPDFGraphicsLibService;
typedef System::DelphiInterface<ITMSFNCPDFGraphicsLibService> _di_ITMSFNCPDFGraphicsLibService;
__interface DELPHIINTERFACE ITMSFNCPDFGraphicsLibGeneralService;
typedef System::DelphiInterface<ITMSFNCPDFGraphicsLibGeneralService> _di_ITMSFNCPDFGraphicsLibGeneralService;
class DELPHICLASS TTMSFNCCustomPDFGraphicsLib;
class DELPHICLASS TTMSFNCPDFGraphicsLibList;
class DELPHICLASS TTMSFNCPDFGraphicsLibFactoryService;
class DELPHICLASS TTMSFNCPDFGraphicsLib;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{EDB1C5AC-6E2B-4B0C-BA5C-884837A6DFF2}") ITMSFNCCustomPDFGraphicsLib  : public System::IInterface 
{
	virtual void __fastcall SetFill(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsFill* const Value) = 0 ;
	virtual void __fastcall SetStroke(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsStroke* const Value) = 0 ;
	virtual void __fastcall SetFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value) = 0 ;
	virtual void __fastcall SetURLFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value) = 0 ;
	virtual void __fastcall SetLineBreakMode(const Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibLineBreakMode Value) = 0 ;
	virtual void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value) = 0 ;
	virtual void __fastcall DrawRectangle(const System::Types::TRectF &Rect) = 0 ;
	virtual void __fastcall DrawEllipse(const System::Types::TRectF &Rect) = 0 ;
	virtual void __fastcall DrawLine(const System::Types::TPointF &StartPoint, const System::Types::TPointF &EndPoint) = 0 ;
	virtual void __fastcall AddURL(System::UnicodeString AText, System::UnicodeString AURL, const System::Types::TRectF &ARect) = 0 ;
	virtual void __fastcall AddGoTo(System::UnicodeString AText, System::UnicodeString ADestination, const System::Types::TRectF &ARect) = 0 ;
	virtual void __fastcall DrawSetTransform(double m11, double m12, double m21, double m22, double dx, double dy) = 0 /* overload */;
	virtual void __fastcall DrawSetTransform(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &m) = 0 /* overload */;
	virtual void __fastcall DrawSaveState() = 0 ;
	virtual void __fastcall DrawClear(const System::Types::TRectF &Rect) = 0 /* overload */;
	virtual void __fastcall DrawRestoreState() = 0 ;
	virtual void __fastcall DrawPathBegin() = 0 ;
	virtual void __fastcall DrawPathBeginClip() = 0 ;
	virtual void __fastcall DrawPathEndClip() = 0 ;
	virtual void __fastcall DrawPathEndLinearGradient(const System::Types::TPointF &StartPoint, const System::Types::TPointF &EndPoint) = 0 ;
	virtual void __fastcall DrawPathMoveToPoint(const System::Types::TPointF &Point) = 0 ;
	virtual void __fastcall DrawPathAddLineToPoint(const System::Types::TPointF &Point) = 0 ;
	virtual void __fastcall DrawPathAddRectangle(const System::Types::TRectF &Rect) = 0 ;
	virtual void __fastcall DrawPathAddEllipse(const System::Types::TRectF &Rect) = 0 ;
	virtual void __fastcall DrawPathAddCurveToPoint(const System::Types::TPointF &FirstControlPoint, const System::Types::TPointF &SecondControlPoint, const System::Types::TPointF &EndPoint) = 0 ;
	virtual void __fastcall DrawPathAddLines(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibPointArray Points) = 0 ;
	virtual void __fastcall DrawPathAddQuadCurveToPoint(const System::Types::TPointF &ControlPoint, const System::Types::TPointF &EndPoint) = 0 ;
	virtual void __fastcall DrawPathClose() = 0 ;
	virtual void __fastcall DrawPathEnd(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibPathDrawingMode DrawingMode = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibPathDrawingMode)(0x4)) = 0 ;
	virtual void __fastcall SetAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value) = 0 ;
	virtual System::Types::TRectF __fastcall CalculateHTMLText(System::UnicodeString Text, float Scale = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall CalculateHTMLText(System::UnicodeString Text, const System::Types::TRectF &Rect, float Scale = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString Text) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString Text, const System::Types::TRectF &Rect) = 0 /* overload */;
	virtual int __fastcall CalculateTextOverflow(System::UnicodeString Text, const System::Types::TRectF &Rect, int Columns, float Padding = 5.000000E+00f) = 0 /* overload */;
	virtual int __fastcall CalculateTextOverflow(System::UnicodeString Text, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibRectArray Rects, float Padding = 5.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageWithName(System::UnicodeString AName, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImage(Fmx::Graphics::TBitmap* ABitmap, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageFromFile(System::UnicodeString AFileName, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageWithName(System::UnicodeString AName, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImage(Fmx::Graphics::TBitmap* ABitmap, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageFromFile(System::UnicodeString AFileName, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TPointF &Point, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageWithName(System::UnicodeString AName, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImage(Fmx::Graphics::TBitmap* ABitmap, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageFromFile(System::UnicodeString AFileName, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageWithName(System::UnicodeString AName, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImage(Fmx::Graphics::TBitmap* ABitmap, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawImageFromFile(System::UnicodeString AFileName, System::Uitypes::TAlphaColor ABackgroundColor, const System::Types::TRectF &Rect, bool Stretch = true, bool AspectRatio = true, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType ImageType = (Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibImageType)(0x0), float Quality = 1.000000E+00f, bool Center = true) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawHTMLText(System::UnicodeString Text, const System::Types::TPointF &Point, float Scale = 1.000000E+00f, bool Calculate = false) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawHTMLText(System::UnicodeString Text, const System::Types::TRectF &Rect, bool Paging = false, float Scale = 1.000000E+00f, bool Calculate = false) = 0 /* overload */;
	virtual int __fastcall DrawText(System::UnicodeString Text, const System::Types::TRectF &Rect, int Columns, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual int __fastcall DrawText(System::UnicodeString Text, Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibRectArray Rects, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawText(System::UnicodeString Text, const System::Types::TPointF &Point, bool Calculate = false) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawText(System::UnicodeString Text, const System::Types::TRectF &Rect, bool Calculate = false) = 0 /* overload */;
	virtual Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer() = 0 ;
	virtual System::Types::TRectF __fastcall GetTextRect() = 0 ;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetAlignment() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsFill* __fastcall GetFill() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsStroke* __fastcall GetStroke() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetFont() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetURLFont() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibLineBreakMode __fastcall GetLineBreakMode() = 0 ;
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Alignment = {read=GetAlignment, write=SetAlignment};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsFill* Fill = {read=GetFill, write=SetFill};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsStroke* Stroke = {read=GetStroke, write=SetStroke};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* Font = {read=GetFont, write=SetFont};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* URLFont = {read=GetURLFont, write=SetURLFont};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibLineBreakMode LineBreakMode = {read=GetLineBreakMode, write=SetLineBreakMode};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
};

__interface  INTERFACE_UUID("{AAC3B710-EEBB-46BE-8450-3D70123B49CA}") ITMSFNCCustomPDFInitializationLib  : public System::IInterface 
{
	virtual void __fastcall SetPDFLib(System::_di_IInterface APDFLib) = 0 ;
	virtual void __fastcall SetCanvas(void * ACanvas) = 0 ;
	virtual void __fastcall SetPageWidth(float APageWidth) = 0 ;
	virtual void __fastcall SetPageHeight(float APageHeight) = 0 ;
	virtual void __fastcall SetOnNotifyNewPage(const System::Classes::TNotifyEvent Value) = 0 ;
	virtual void __fastcall InitializeAppearance() = 0 ;
	virtual void __fastcall NotifyNewPage() = 0 ;
	virtual float __fastcall GetPageWidth() = 0 ;
	virtual float __fastcall GetPageHeight() = 0 ;
	virtual System::Classes::TNotifyEvent __fastcall GetOnNotifyNewPage() = 0 ;
	__property System::Classes::TNotifyEvent OnNotifyNewPage = {read=GetOnNotifyNewPage, write=SetOnNotifyNewPage};
};

__interface  INTERFACE_UUID("{4BA4B9BC-CBD2-4852-BE93-573753A37365}") ITMSFNCCustomPDFGraphicsExLib  : public System::IInterface 
{
	virtual void __fastcall SetPDFRichTextLib(const Fmx::Tmsfncpdfrichtextlib::_di_ITMSFNCCustomPDFRichTextLib Value) = 0 ;
	virtual void __fastcall DrawAddShadow(const System::Types::TPointF &Offset, float Blur) = 0 /* overload */;
	virtual void __fastcall DrawAddShadow(const System::Types::TPointF &Offset, float Blur, System::Uitypes::TAlphaColor Color) = 0 /* overload */;
	virtual void __fastcall DrawRoundedRectangle(const System::Types::TRectF &Rect, float Rounding) = 0 ;
	virtual void __fastcall DrawPathAddArc(const System::Types::TPointF &CenterPoint, float Radius, float StartAngle, float EndAngle, bool Clockwise = false) = 0 ;
	virtual void __fastcall DrawPathAddArcToPoint(const System::Types::TPointF &FirstPoint, const System::Types::TPointF &SecondPoint, float Radius) = 0 ;
	virtual void __fastcall DrawPathEndRadialGradient(const System::Types::TPointF &StartCenter, const System::Types::TPointF &EndCenter, float StartRadius, float EndRadius) = 0 ;
	virtual int __fastcall DrawRichText(const System::Types::TRectF &Rect, int Columns, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual int __fastcall DrawRichText(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibRectArray Rects, float Padding = 5.000000E+00f, bool DetectOverflow = false) = 0 /* overload */;
	virtual System::Types::TRectF __fastcall DrawRichText(const System::Types::TRectF &Rect, bool Calculate = false) = 0 /* overload */;
	virtual Fmx::Tmsfncpdfrichtextlib::_di_ITMSFNCCustomPDFRichTextLib __fastcall RichText() = 0 ;
};

__interface  INTERFACE_UUID("{033FCA68-4F8F-4916-9B59-67FF54E18BCC}") ITMSFNCPDFGraphicsLibService  : public System::IInterface 
{
	virtual System::TObject* __fastcall CreatePDFGraphicsLib() = 0 ;
};

__interface  INTERFACE_UUID("{0F974932-4204-489D-95DE-DBEFF4DEF5D7}") ITMSFNCPDFGraphicsLibGeneralService  : public System::IInterface 
{
	virtual System::TObject* __fastcall CreatePDFGraphicsLib() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomPDFGraphicsLib : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	_di_ITMSFNCCustomPDFGraphicsLib FPDFGraphicsLib;
	_di_ITMSFNCCustomPDFGraphicsExLib FPDFGraphicsExLib;
	Fmx::Tmsfncpdfrichtextlib::TTMSFNCPDFRichTextLib* FPDFRichTextLib;
	_di_ITMSFNCCustomPDFInitializationLib FPDFInitializationLib;
	
public:
	__fastcall virtual TTMSFNCCustomPDFGraphicsLib()/* overload */;
	__fastcall virtual TTMSFNCCustomPDFGraphicsLib(bool AUseNativePDFImplementation)/* overload */;
	__fastcall virtual ~TTMSFNCCustomPDFGraphicsLib();
	_di_ITMSFNCCustomPDFGraphicsLib __fastcall GetPDFGraphicsLib();
	_di_ITMSFNCCustomPDFGraphicsExLib __fastcall GetPDFGraphicsExLib();
	_di_ITMSFNCCustomPDFInitializationLib __fastcall GetPDFInitializationLib();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibList : public System::Generics::Collections::TList__1<System::TObject*>
{
	typedef System::Generics::Collections::TList__1<System::TObject*> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibList()/* overload */ : System::Generics::Collections::TList__1<System::TObject*>() { }
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::TObject*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<System::TObject*>(AComparer) { }
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibList(System::Generics::Collections::TEnumerable__1<System::TObject*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<System::TObject*>(Collection) { }
	/* {System_Generics_Collections}TList<System_TObject>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibList(System::TObject* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<System::TObject*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_TObject>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibFactoryService : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTMSFNCPDFGraphicsLibList* FPDFGraphicsLibs;
	
protected:
	virtual System::TObject* __fastcall DoCreatePDFGraphicsLib() = 0 ;
	System::TObject* __fastcall CreatePDFGraphicsLib();
	
public:
	__fastcall TTMSFNCPDFGraphicsLibFactoryService();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibFactoryService();
private:
	void *__ITMSFNCPDFGraphicsLibGeneralService;	// ITMSFNCPDFGraphicsLibGeneralService 
	void *__ITMSFNCPDFGraphicsLibService;	// ITMSFNCPDFGraphicsLibService 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0F974932-4204-489D-95DE-DBEFF4DEF5D7}
	operator _di_ITMSFNCPDFGraphicsLibGeneralService()
	{
		_di_ITMSFNCPDFGraphicsLibGeneralService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCPDFGraphicsLibGeneralService*(void) { return (ITMSFNCPDFGraphicsLibGeneralService*)&__ITMSFNCPDFGraphicsLibGeneralService; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {033FCA68-4F8F-4916-9B59-67FF54E18BCC}
	operator _di_ITMSFNCPDFGraphicsLibService()
	{
		_di_ITMSFNCPDFGraphicsLibService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCPDFGraphicsLibService*(void) { return (ITMSFNCPDFGraphicsLibService*)&__ITMSFNCPDFGraphicsLibService; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLib : public TTMSFNCCustomPDFGraphicsLib
{
	typedef TTMSFNCCustomPDFGraphicsLib inherited;
	
public:
	/* TTMSFNCCustomPDFGraphicsLib.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsLib()/* overload */ : TTMSFNCCustomPDFGraphicsLib() { }
	/* TTMSFNCCustomPDFGraphicsLib.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsLib(bool AUseNativePDFImplementation)/* overload */ : TTMSFNCCustomPDFGraphicsLib(AUseNativePDFImplementation) { }
	/* TTMSFNCCustomPDFGraphicsLib.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLib() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncpdfgraphicslib */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFGRAPHICSLIB)
using namespace Fmx::Tmsfncpdfgraphicslib;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfgraphicslibHPP
