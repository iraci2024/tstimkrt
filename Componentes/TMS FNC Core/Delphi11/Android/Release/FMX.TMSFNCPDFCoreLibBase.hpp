// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFCoreLibBase.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdfcorelibbaseHPP
#define Fmx_TmsfncpdfcorelibbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.UITypes.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#include <Androidapi.JNIBridge.hpp>
#include <Androidapi.JNI.JavaTypes.hpp>
#include <Androidapi.Helpers.hpp>
#include <System.SysUtils.hpp>
#include <Androidapi.Jni.hpp>
#include <System.Rtti.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfcorelibbase
{
//-- forward type declarations -----------------------------------------------
struct TTMSFNCPDFGraphicsLibTextRange;
class DELPHICLASS TTMSFNCPDFGraphicsLibFont;
class DELPHICLASS TTMSFNCPDFGraphicsFill;
class DELPHICLASS TTMSFNCPDFGraphicsStroke;
class DELPHICLASS TTMSFNCPDFGraphicsLibBase;
class DELPHICLASS TTMSFNCPDFPlatformServicesService;
class DELPHICLASS TTMSFNCPDFPlatformServicesList;
class DELPHICLASS TTMSFNCPDFLibPlatformServices;
class DELPHICLASS TTMSFNCPDFGraphicsLibUsedFontCharArray;
struct TTMSFNCPDFGraphicsLibFontCharArray;
struct TTMSFNCPDFGraphicsLibFontCharWidth;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriterStream;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriterStreams;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriter;
__interface DELPHIINTERFACE JStaticLayoutClass;
typedef System::DelphiInterface<JStaticLayoutClass> _di_JStaticLayoutClass;
__interface DELPHIINTERFACE JStaticLayout;
typedef System::DelphiInterface<JStaticLayout> _di_JStaticLayout;
class DELPHICLASS TJStaticLayout;
__interface DELPHIINTERFACE JStaticLayout_BuilderClass;
typedef System::DelphiInterface<JStaticLayout_BuilderClass> _di_JStaticLayout_BuilderClass;
__interface DELPHIINTERFACE JStaticLayout_Builder;
typedef System::DelphiInterface<JStaticLayout_Builder> _di_JStaticLayout_Builder;
class DELPHICLASS TJStaticLayout_Builder;
__interface DELPHIINTERFACE JSpannableStringClass;
typedef System::DelphiInterface<JSpannableStringClass> _di_JSpannableStringClass;
__interface DELPHIINTERFACE JSpannableString;
typedef System::DelphiInterface<JSpannableString> _di_JSpannableString;
class DELPHICLASS TJSpannableString;
__interface DELPHIINTERFACE JMetricAffectingSpanClass;
typedef System::DelphiInterface<JMetricAffectingSpanClass> _di_JMetricAffectingSpanClass;
__interface DELPHIINTERFACE JMetricAffectingSpan;
typedef System::DelphiInterface<JMetricAffectingSpan> _di_JMetricAffectingSpan;
class DELPHICLASS TJMetricAffectingSpan;
__interface DELPHIINTERFACE JTypefaceSpanClass;
typedef System::DelphiInterface<JTypefaceSpanClass> _di_JTypefaceSpanClass;
__interface DELPHIINTERFACE JTypefaceSpan;
typedef System::DelphiInterface<JTypefaceSpan> _di_JTypefaceSpan;
class DELPHICLASS TJTypefaceSpan;
__interface DELPHIINTERFACE JBackgroundColorSpanClass;
typedef System::DelphiInterface<JBackgroundColorSpanClass> _di_JBackgroundColorSpanClass;
__interface DELPHIINTERFACE JBackgroundColorSpan;
typedef System::DelphiInterface<JBackgroundColorSpan> _di_JBackgroundColorSpan;
class DELPHICLASS TJBackgroundColorSpan;
__interface DELPHIINTERFACE JForegroundColorSpanClass;
typedef System::DelphiInterface<JForegroundColorSpanClass> _di_JForegroundColorSpanClass;
__interface DELPHIINTERFACE JForegroundColorSpan;
typedef System::DelphiInterface<JForegroundColorSpan> _di_JForegroundColorSpan;
class DELPHICLASS TJForegroundColorSpan;
__interface DELPHIINTERFACE JStyleSpanClass;
typedef System::DelphiInterface<JStyleSpanClass> _di_JStyleSpanClass;
__interface DELPHIINTERFACE JStyleSpan;
typedef System::DelphiInterface<JStyleSpan> _di_JStyleSpan;
class DELPHICLASS TJStyleSpan;
__interface DELPHIINTERFACE JStrikethroughSpanClass;
typedef System::DelphiInterface<JStrikethroughSpanClass> _di_JStrikethroughSpanClass;
__interface DELPHIINTERFACE JStrikethroughSpan;
typedef System::DelphiInterface<JStrikethroughSpan> _di_JStrikethroughSpan;
class DELPHICLASS TJStrikethroughSpan;
__interface DELPHIINTERFACE JUnderlineSpanClass;
typedef System::DelphiInterface<JUnderlineSpanClass> _di_JUnderlineSpanClass;
__interface DELPHIINTERFACE JUnderlineSpan;
typedef System::DelphiInterface<JUnderlineSpan> _di_JUnderlineSpan;
class DELPHICLASS TJUnderlineSpan;
__interface DELPHIINTERFACE JAbsoluteSizeSpanClass;
typedef System::DelphiInterface<JAbsoluteSizeSpanClass> _di_JAbsoluteSizeSpanClass;
__interface DELPHIINTERFACE JAbsoluteSizeSpan;
typedef System::DelphiInterface<JAbsoluteSizeSpan> _di_JAbsoluteSizeSpan;
class DELPHICLASS TJAbsoluteSizeSpan;
__interface DELPHIINTERFACE JImageSpanClass;
typedef System::DelphiInterface<JImageSpanClass> _di_JImageSpanClass;
__interface DELPHIINTERFACE JImageSpan;
typedef System::DelphiInterface<JImageSpan> _di_JImageSpan;
class DELPHICLASS TJImageSpan;
__interface DELPHIINTERFACE JXMLReaderClass;
typedef System::DelphiInterface<JXMLReaderClass> _di_JXMLReaderClass;
__interface DELPHIINTERFACE JXMLReader;
typedef System::DelphiInterface<JXMLReader> _di_JXMLReader;
class DELPHICLASS TJXMLReader;
__interface DELPHIINTERFACE JHTML_ImageGetterClass;
typedef System::DelphiInterface<JHTML_ImageGetterClass> _di_JHTML_ImageGetterClass;
__interface DELPHIINTERFACE JHTML_ImageGetter;
typedef System::DelphiInterface<JHTML_ImageGetter> _di_JHTML_ImageGetter;
class DELPHICLASS TJHTML_ImageGetter;
__interface DELPHIINTERFACE JHTML_TagHandlerClass;
typedef System::DelphiInterface<JHTML_TagHandlerClass> _di_JHTML_TagHandlerClass;
__interface DELPHIINTERFACE JHTML_TagHandler;
typedef System::DelphiInterface<JHTML_TagHandler> _di_JHTML_TagHandler;
class DELPHICLASS TJHTML_TagHandler;
__interface DELPHIINTERFACE JHTMLClass;
typedef System::DelphiInterface<JHTMLClass> _di_JHTMLClass;
__interface DELPHIINTERFACE JHTML;
typedef System::DelphiInterface<JHTML> _di_JHTML;
class DELPHICLASS TJHTML;
__interface DELPHIINTERFACE JPdfDocumentClass;
typedef System::DelphiInterface<JPdfDocumentClass> _di_JPdfDocumentClass;
__interface DELPHIINTERFACE JPdfDocument;
typedef System::DelphiInterface<JPdfDocument> _di_JPdfDocument;
class DELPHICLASS TJPdfDocument;
__interface DELPHIINTERFACE JPdfDocument_PageClass;
typedef System::DelphiInterface<JPdfDocument_PageClass> _di_JPdfDocument_PageClass;
__interface DELPHIINTERFACE JPdfDocument_Page;
typedef System::DelphiInterface<JPdfDocument_Page> _di_JPdfDocument_Page;
class DELPHICLASS TJPdfDocument_Page;
__interface DELPHIINTERFACE JPdfDocument_PageInfoClass;
typedef System::DelphiInterface<JPdfDocument_PageInfoClass> _di_JPdfDocument_PageInfoClass;
__interface DELPHIINTERFACE JPdfDocument_PageInfo;
typedef System::DelphiInterface<JPdfDocument_PageInfo> _di_JPdfDocument_PageInfo;
class DELPHICLASS TJPdfDocument_PageInfo;
__interface DELPHIINTERFACE JPageInfo_BuilderClass;
typedef System::DelphiInterface<JPageInfo_BuilderClass> _di_JPageInfo_BuilderClass;
__interface DELPHIINTERFACE JPageInfo_Builder;
typedef System::DelphiInterface<JPageInfo_Builder> _di_JPageInfo_Builder;
class DELPHICLASS TJPageInfo_Builder;
__interface DELPHIINTERFACE JPrintAttributes_MediaSizeClass;
typedef System::DelphiInterface<JPrintAttributes_MediaSizeClass> _di_JPrintAttributes_MediaSizeClass;
__interface DELPHIINTERFACE JPrintAttributes_MediaSize;
typedef System::DelphiInterface<JPrintAttributes_MediaSize> _di_JPrintAttributes_MediaSize;
class DELPHICLASS TJPrintAttributes_MediaSize;
__interface DELPHIINTERFACE JPrintAttributes_ResolutionClass;
typedef System::DelphiInterface<JPrintAttributes_ResolutionClass> _di_JPrintAttributes_ResolutionClass;
__interface DELPHIINTERFACE JPrintAttributes_Resolution;
typedef System::DelphiInterface<JPrintAttributes_Resolution> _di_JPrintAttributes_Resolution;
class DELPHICLASS TJPrintAttributes_Resolution;
__interface DELPHIINTERFACE JPrintAttributes_MarginsClass;
typedef System::DelphiInterface<JPrintAttributes_MarginsClass> _di_JPrintAttributes_MarginsClass;
__interface DELPHIINTERFACE JPrintAttributes_Margins;
typedef System::DelphiInterface<JPrintAttributes_Margins> _di_JPrintAttributes_Margins;
class DELPHICLASS TJPrintAttributes_Margins;
__interface DELPHIINTERFACE JPrintAttributesClass;
typedef System::DelphiInterface<JPrintAttributesClass> _di_JPrintAttributesClass;
__interface DELPHIINTERFACE JPrintAttributes;
typedef System::DelphiInterface<JPrintAttributes> _di_JPrintAttributes;
class DELPHICLASS TJPrintAttributes;
__interface DELPHIINTERFACE JPrintAttributes_BuilderClass;
typedef System::DelphiInterface<JPrintAttributes_BuilderClass> _di_JPrintAttributes_BuilderClass;
__interface DELPHIINTERFACE JPrintAttributes_Builder;
typedef System::DelphiInterface<JPrintAttributes_Builder> _di_JPrintAttributes_Builder;
class DELPHICLASS TJPrintAttributes_Builder;
__interface DELPHIINTERFACE JPrintedPdfDocumentClass;
typedef System::DelphiInterface<JPrintedPdfDocumentClass> _di_JPrintedPdfDocumentClass;
__interface DELPHIINTERFACE JPrintedPdfDocument;
typedef System::DelphiInterface<JPrintedPdfDocument> _di_JPrintedPdfDocument;
class DELPHICLASS TJPrintedPdfDocument;
__interface DELPHIINTERFACE JEditableClass;
typedef System::DelphiInterface<JEditableClass> _di_JEditableClass;
__interface DELPHIINTERFACE JEditable;
typedef System::DelphiInterface<JEditable> _di_JEditable;
class DELPHICLASS TJEditable;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibPathDrawingMode : unsigned char { dmPathFill, dmPathEOFill, dmPathStroke, dmPathEOStroke, dmPathFillStroke, dmPathEOFillStroke };

enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibImageType : unsigned char { itOriginal, itPNG, itJPG };

enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibLineBreakMode : unsigned char { bmLineBreakModeWordWrap, bmLineBreakModeCharacterWrap, bmLineBreakModeClip, bmLineBreakModeHeadTruncation, bmLineBreakModeMiddleTruncation, bmLineBreakModeTailTruncation };

typedef System::DynamicArray<System::Types::TPointF> TTMSFNCPDFGraphicsLibPointArray;

typedef System::DynamicArray<System::Types::TRectF> TTMSFNCPDFGraphicsLibRectArray;

struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibTextRange
{
public:
	int location;
	int length;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibFont : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FUpdateCount;
	System::UnicodeString FName;
	float FSize;
	System::Uitypes::TAlphaColor FColor;
	System::Classes::TNotifyEvent FOnChanged;
	System::Uitypes::TFontStyles FStyle;
	System::UnicodeString FFileName;
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetSize(const float Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	bool __fastcall IsSizeStored();
	void __fastcall SetStyle(const System::Uitypes::TFontStyles Value);
	void __fastcall SetSizeNoScale(const float Value);
	void __fastcall SetFileName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoChanged();
	
public:
	__fastcall TTMSFNCPDFGraphicsLibFont();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
	__property float Size = {read=FSize, write=SetSize, stored=IsSizeStored};
	__property float SizeNoScale = {read=FSize, write=SetSizeNoScale, stored=IsSizeStored};
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=-16777216};
	__property System::Uitypes::TFontStyles Style = {read=FStyle, write=SetStyle, default=0};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibFont() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsFill : public Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill
{
	typedef Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill inherited;
	
__published:
	__property Kind;
	__property Orientation = {default=1};
	__property Color;
	__property ColorTo;
public:
	/* TTMSFNCCustomGraphicsFill.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsFill(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TAlphaColor AColor, const System::Uitypes::TAlphaColor AColorTo, const System::Uitypes::TAlphaColor AColorMirror, const System::Uitypes::TAlphaColor AColorMirrorTo) : Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill(AKind, AColor, AColorTo, AColorMirror, AColorMirrorTo) { }
	/* TTMSFNCCustomGraphicsFill.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsFill() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsStroke : public Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke
{
	typedef Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke inherited;
	
__published:
	__property Kind;
	__property Color;
	__property Width;
public:
	/* TTMSFNCCustomGraphicsStroke.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsStroke(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TAlphaColor AColor) : Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke(AKind, AColor) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsStroke() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibBase : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTMSFNCPDFGraphicsLibFont* FFont;
	TTMSFNCPDFGraphicsLibLineBreakMode FLineBreakMode;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FAlignment;
	TTMSFNCPDFGraphicsLibFont* FURLFont;
	TTMSFNCPDFGraphicsFill* FFill;
	TTMSFNCPDFGraphicsStroke* FStroke;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	void __fastcall SetAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetFont(TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetLineBreakMode(const TTMSFNCPDFGraphicsLibLineBreakMode Value);
	void __fastcall SetURLFont(TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetFill(TTMSFNCPDFGraphicsFill* const Value);
	void __fastcall SetStroke(TTMSFNCPDFGraphicsStroke* const Value);
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	
protected:
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall InitializeAppearance();
	virtual void __fastcall UpdateFont();
	virtual void __fastcall UpdateAlignment();
	virtual void __fastcall UpdateFill();
	virtual void __fastcall UpdateLineBreakMode();
	virtual void __fastcall UpdateStroke();
	virtual void __fastcall CreateClasses();
	
public:
	__fastcall virtual TTMSFNCPDFGraphicsLibBase();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibBase();
	__property TTMSFNCPDFGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property TTMSFNCPDFGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property TTMSFNCPDFGraphicsLibFont* Font = {read=FFont, write=SetFont};
	__property TTMSFNCPDFGraphicsLibFont* URLFont = {read=FURLFont, write=SetURLFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Alignment = {read=FAlignment, write=SetAlignment, default=1};
	__property TTMSFNCPDFGraphicsLibLineBreakMode LineBreakMode = {read=FLineBreakMode, write=SetLineBreakMode, default=0};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=SetBitmapContainer};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFPlatformServicesService : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::_di_IInterface FInterface;
	System::UnicodeString FGUID;
	
public:
	__fastcall TTMSFNCPDFPlatformServicesService(System::UnicodeString AGUID, System::_di_IInterface AInterface);
	__property System::UnicodeString GUID = {read=FGUID};
	__property System::_di_IInterface Interface = {read=FInterface};
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
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCPDFPlatformServicesService() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFPlatformServicesList : public System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*> inherited;
	
private:
	System::_di_IInterface __fastcall GetValue(System::UnicodeString AGUID);
	
public:
	bool __fastcall ContainsKey(System::UnicodeString AGUID);
	void __fastcall RemoveByGUID(System::UnicodeString AGUID);
	__property System::_di_IInterface Interfaces[System::UnicodeString AGUID] = {read=GetValue};
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFPlatformServicesService*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFPlatformServicesService*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFPlatformServicesList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFPlatformServicesService*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFPlatformServicesService*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(TTMSFNCPDFPlatformServicesService* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFLibPlatformServices : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCPDFPlatformServicesList* FServicesList;
	static TTMSFNCPDFLibPlatformServices* FCurrent;
	static bool FCurrentReleased;
	__classmethod void __fastcall ReleaseCurrent();
	
public:
	__fastcall TTMSFNCPDFLibPlatformServices();
	__fastcall virtual ~TTMSFNCPDFLibPlatformServices();
	void __fastcall AddPlatformService(const GUID &AServiceGUID, const System::_di_IInterface AService);
	void __fastcall RemovePlatformService(const GUID &AServiceGUID);
	System::_di_IInterface __fastcall GetPlatformService(const GUID &AServiceGUID);
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID)/* overload */;
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID, System::_di_IInterface &AService)/* overload */;
	__classmethod TTMSFNCPDFLibPlatformServices* __fastcall Current();
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

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibUsedFontCharArray : public System::Generics::Collections::TList__1<int>
{
	typedef System::Generics::Collections::TList__1<int> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray()/* overload */ : System::Generics::Collections::TList__1<int>() { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<int> > AComparer)/* overload */ : System::Generics::Collections::TList__1<int>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(System::Generics::Collections::TEnumerable__1<int>* const Collection)/* overload */ : System::Generics::Collections::TList__1<int>(Collection) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(const int *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<int>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_Integer>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibUsedFontCharArray() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibFontCharArray
{
public:
	System::TArray__1<System::Word> v;
	int c;
	int __fastcall Add(System::Word AValue);
	int __fastcall IndexOf(System::Word AValue);
	int __fastcall FirstValue();
	int __fastcall LastValue();
};


struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibFontCharWidth
{
public:
	System::Word w;
	System::Word g;
	int i;
};


typedef System::DynamicArray<TTMSFNCPDFGraphicsLibFontCharWidth> TTMSFNCPDFGraphicsLibFontCharWidths;

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent)(System::TObject* Sender, System::UnicodeString AValue);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent)(System::TObject* Sender, const System::Types::TRectF &ARect, System::UnicodeString AURL);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent)(System::TObject* Sender, const System::Types::TRectF &ARect, System::UnicodeString ADestination);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent)(System::TObject* Sender, System::UnicodeString AValue);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent)(System::TObject* Sender, Fmx::Graphics::TBitmap* AValue, TTMSFNCPDFGraphicsLibImageType AImageType, float AQuality, System::Uitypes::TAlphaColor ABackgroundColor, System::UnicodeString &ABitmapReference);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent)(System::TObject* Sender, System::Uitypes::TAlphaColor AFillColor, System::Uitypes::TAlphaColor AFillColorTo, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillOrientation AFillOrientation, System::UnicodeString &AShadingReference);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent)(System::TObject* Sender, const System::Types::TRectF &ARect);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriterStream : public System::Classes::TStringStream
{
	typedef System::Classes::TStringStream inherited;
	
private:
	System::TObject* FReference;
	
public:
	__fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriterStream();
	__property System::TObject* Reference = {read=FReference, write=FReference};
public:
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream()/* overload */ : System::Classes::TStringStream() { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString)/* overload */ : System::Classes::TStringStream(AString) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::RawByteString AString)/* overload */ : System::Classes::TStringStream(AString) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString, System::Sysutils::TEncoding* AEncoding, bool AOwnsEncoding)/* overload */ : System::Classes::TStringStream(AString, AEncoding, AOwnsEncoding) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString, int ACodePage)/* overload */ : System::Classes::TStringStream(AString, ACodePage) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::TArray__1<System::Byte> ABytes)/* overload */ : System::Classes::TStringStream(ABytes) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriterStreams : public System::Generics::Collections::TObjectList__1<System::Classes::TStream*>
{
	typedef System::Generics::Collections::TObjectList__1<System::Classes::TStream*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams()/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>() { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::Classes::TStream*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Generics::Collections::TEnumerable__1<System::Classes::TStream*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriterStreams() { }
	
public:
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::Classes::TStream*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Generics::Collections::TEnumerable__1<System::Classes::TStream*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Collection) { }
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Classes::TStream* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DelphiInterface<System::Generics::Defaults::IComparer__1<int> > FCompareValues;
	TTMSFNCPDFGraphicsLibOutputWriterStream* FStream;
	TTMSFNCPDFGraphicsLibOutputWriterStream* FContentStream;
	TTMSFNCPDFGraphicsLibOutputWriterStreams* FStreams;
	System::UnicodeString FFontRefName;
	float FFontSize;
	System::UnicodeString FFontName;
	System::Classes::TNotifyEvent FOnFontChanged;
	System::Uitypes::TAlphaColor FFontColor;
	System::Uitypes::TFontStyles FFontStyle;
	System::UnicodeString FFontBase;
	int FFontWordSpacing;
	float FFontLeading;
	TTMSFNCPDFGraphicsLibFontCharWidths FFontCharWidths;
	int FFontCharSpacing;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent FOnNotifyUnicode;
	TTMSFNCPDFGraphicsLibFontCharArray FFontCharArray;
	TTMSFNCPDFGraphicsLibUsedFontCharArray* FFontUsedCharArray;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent FOnNotifyBitmap;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent FOnNotifyShading;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent FOnNotifyShadingRect;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent FOnNotifyText;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent FOnNotifyURL;
	bool FFontUnicode;
	int FFontCapHeight;
	int FFontUnitsPerEm;
	bool FFontEmbedding;
	System::UnicodeString FFontGlyphRefName;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent FOnNotifyGoTo;
	__int64 __fastcall GetStreamPosition();
	void __fastcall SetStreamPosition(const __int64 Value);
	
public:
	__fastcall TTMSFNCPDFGraphicsLibOutputWriter();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriter();
	TTMSFNCPDFGraphicsLibOutputWriterStreams* __fastcall Streams();
	int __fastcall GetFontCharWidth(System::UnicodeString AText, int APos);
	System::UnicodeString __fastcall ConvertColorToString(System::Uitypes::TAlphaColor AValue);
	System::UnicodeString __fastcall ConvertFloatToString(System::Extended AValue);
	bool __fastcall IsUnicodeString(System::UnicodeString AValue);
	System::UnicodeString __fastcall ConvertStringToHex(System::UnicodeString AValue);
	System::UnicodeString __fastcall AddHex4(unsigned AWordValue);
	System::UnicodeString __fastcall EscapeString(System::UnicodeString AValue);
	bool __fastcall FontCharArrayContainsValue(int ACharValue);
	System::Classes::TStringStream* __fastcall CompressString(System::UnicodeString AValue);
	System::Classes::TStringStream* __fastcall CompressStream(System::Classes::TStringStream* AStream);
	System::Classes::TStringStream* __fastcall FinishContentStream(System::UnicodeString AReference, bool ACompress, System::UnicodeString AAdditionalFlags = System::UnicodeString());
	float __fastcall TextHeight();
	void __fastcall FontCharArrayAddValue(int ACharValue);
	void __fastcall BeginText();
	void __fastcall WriteMatrix(const System::Types::TPointF &APoint, const System::Types::TSizeF &ASize)/* overload */;
	void __fastcall WriteMatrix(double m11, double m12, double m21, double m22, double dx, double dy)/* overload */;
	void __fastcall WriteTextMatrix(double m11, double m12, double m21, double m22, double dx, double dy);
	void __fastcall WriteRectangle(const System::Types::TRectF &ARect);
	void __fastcall WriteFontColor();
	void __fastcall WriteFontLeading();
	void __fastcall WriteFont();
	void __fastcall WriteFillColor(System::Uitypes::TAlphaColor AColor);
	void __fastcall WriteStrokeColor(System::Uitypes::TAlphaColor AColor);
	void __fastcall WriteStrokeKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	void __fastcall WriteStrokeWidth(float AWidth);
	void __fastcall EndText();
	void __fastcall MoveTo(const System::Types::TPointF &APoint);
	void __fastcall LineTo(const System::Types::TPointF &APoint);
	void __fastcall CurveTo(float AX1, float AX2, float AX3, float AX4, float AX5, float AX6);
	void __fastcall CurveTo2(float AX1, float AX2, float AX3, float AX4);
	void __fastcall MoveTextTo(const System::Types::TPointF &APoint);
	void __fastcall MoveTextToNextLine();
	void __fastcall AddText(System::UnicodeString AValue);
	void __fastcall StartContentStream(System::TObject* AReference = (System::TObject*)(0x0));
	void __fastcall StartNewStream(System::TObject* AReference = (System::TObject*)(0x0));
	void __fastcall WriteString(System::UnicodeString AValue);
	void __fastcall Write(void * Buffer, int Count);
	void __fastcall ReplaceString(System::UnicodeString ASearchValue, System::UnicodeString AValue);
	void __fastcall ClearProperties();
	void __fastcall NotifyURL(const System::Types::TRectF &ARect, System::UnicodeString AURL);
	void __fastcall NotifyGoTo(const System::Types::TRectF &ARect, System::UnicodeString ADestination);
	void __fastcall NotifyShading(System::Uitypes::TAlphaColor AFillColor, System::Uitypes::TAlphaColor AFillColorTo, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillOrientation AFillOrientation, System::UnicodeString &AShadingReference);
	void __fastcall NotifyShadingRect(const System::Types::TRectF &ARect);
	void __fastcall NotifyUnicode(System::UnicodeString AValue);
	void __fastcall NotifyText(System::UnicodeString AValue);
	void __fastcall NotifyBitmap(Fmx::Graphics::TBitmap* AValue, TTMSFNCPDFGraphicsLibImageType AImageType, float AQuality, System::Uitypes::TAlphaColor ABackgroundColor, System::UnicodeString &ABitmapReference);
	void __fastcall ReplaceStrings(System::UnicodeString *ASearchValues, const int ASearchValues_High, System::UnicodeString *AValues, const int AValues_High);
	__property TTMSFNCPDFGraphicsLibOutputWriterStream* Stream = {read=FStream, write=FStream};
	__property TTMSFNCPDFGraphicsLibOutputWriterStream* ContentStream = {read=FContentStream, write=FContentStream};
	__property System::UnicodeString FontBase = {read=FFontBase, write=FFontBase};
	__property System::UnicodeString FontName = {read=FFontName, write=FFontName};
	__property System::UnicodeString FontRefName = {read=FFontRefName, write=FFontRefName};
	__property System::UnicodeString FontGlyphRefName = {read=FFontGlyphRefName, write=FFontGlyphRefName};
	__property bool FontEmbedding = {read=FFontEmbedding, write=FFontEmbedding, nodefault};
	__property float FontSize = {read=FFontSize, write=FFontSize};
	__property int FontCapHeight = {read=FFontCapHeight, write=FFontCapHeight, nodefault};
	__property int FontUnitsPerEm = {read=FFontUnitsPerEm, write=FFontUnitsPerEm, nodefault};
	__property System::Uitypes::TAlphaColor FontColor = {read=FFontColor, write=FFontColor, nodefault};
	__property System::Uitypes::TFontStyles FontStyle = {read=FFontStyle, write=FFontStyle, nodefault};
	__property int FontWordSpacing = {read=FFontWordSpacing, write=FFontWordSpacing, nodefault};
	__property int FontCharSpacing = {read=FFontCharSpacing, write=FFontCharSpacing, nodefault};
	__property float FontLeading = {read=FFontLeading, write=FFontLeading};
	__property TTMSFNCPDFGraphicsLibFontCharWidths FontCharWidths = {read=FFontCharWidths, write=FFontCharWidths};
	__property TTMSFNCPDFGraphicsLibFontCharArray FontCharArray = {read=FFontCharArray, write=FFontCharArray};
	__property TTMSFNCPDFGraphicsLibUsedFontCharArray* FontUsedCharArray = {read=FFontUsedCharArray, write=FFontUsedCharArray};
	__property bool FontUnicode = {read=FFontUnicode, write=FFontUnicode, nodefault};
	__property System::Classes::TNotifyEvent OnFontChanged = {read=FOnFontChanged, write=FOnFontChanged};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent OnNotifyText = {read=FOnNotifyText, write=FOnNotifyText};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent OnNotifyUnicode = {read=FOnNotifyUnicode, write=FOnNotifyUnicode};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent OnNotifyURL = {read=FOnNotifyURL, write=FOnNotifyURL};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent OnNotifyGoTo = {read=FOnNotifyGoTo, write=FOnNotifyGoTo};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent OnNotifyBitmap = {read=FOnNotifyBitmap, write=FOnNotifyBitmap};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent OnNotifyShading = {read=FOnNotifyShading, write=FOnNotifyShading};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent OnNotifyShadingRect = {read=FOnNotifyShadingRect, write=FOnNotifyShadingRect};
	__property __int64 StreamPosition = {read=GetStreamPosition, write=SetStreamPosition};
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

__interface  INTERFACE_UUID("{D76230AF-C65C-4582-8E10-DD8600B593B7}") JStaticLayoutClass  : public Androidapi::Jni::Graphicscontentviewtext::JLayoutClass 
{
	HIDESBASE virtual _di_JStaticLayout __cdecl init(Androidapi::Jni::Javatypes::_di_JCharSequence source, Androidapi::Jni::Graphicscontentviewtext::_di_JTextPaint paint, int width, Androidapi::Jni::Graphicscontentviewtext::_di_JLayout_Alignment align, float spacingmult, float spacingadd, bool includepad) = 0 /* overload */;
	HIDESBASE virtual _di_JStaticLayout __cdecl init(Androidapi::Jni::Javatypes::_di_JCharSequence source, int bufstart, int bufend, Androidapi::Jni::Graphicscontentviewtext::_di_JTextPaint paint, int outerwidth, Androidapi::Jni::Graphicscontentviewtext::_di_JLayout_Alignment align, float spacingmult, float spacingadd, bool includepad) = 0 /* overload */;
	HIDESBASE virtual _di_JStaticLayout __cdecl init(Androidapi::Jni::Javatypes::_di_JCharSequence source, int bufstart, int bufend, Androidapi::Jni::Graphicscontentviewtext::_di_JTextPaint paint, int outerwidth, Androidapi::Jni::Graphicscontentviewtext::_di_JLayout_Alignment align, float spacingmult, float spacingadd, bool includepad, Androidapi::Jni::Graphicscontentviewtext::_di_JTextUtils_TruncateAt ellipsize, int ellipsizedWidth) = 0 /* overload */;
};

__interface  INTERFACE_UUID("{722D0BA8-FF91-4E5A-BEA4-1DCC12AC3DAF}") JStaticLayout  : public Androidapi::Jni::Graphicscontentviewtext::JLayout 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJStaticLayout : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayoutClass,_di_JStaticLayout>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayoutClass,_di_JStaticLayout> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJStaticLayout() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayoutClass,_di_JStaticLayout>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJStaticLayout() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{315993F5-88BB-4995-B03A-CD015291E56D}") JStaticLayout_BuilderClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual _di_JStaticLayout_Builder __cdecl obtain(Androidapi::Jni::Javatypes::_di_JCharSequence source, int start, int end_, Androidapi::Jni::Graphicscontentviewtext::_di_JTextPaint paint, int width) = 0 ;
};

__interface  INTERFACE_UUID("{5BC7CCB0-B90E-4794-90F0-EBA126BFB04D}") JStaticLayout_Builder  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JStaticLayout __cdecl build() = 0 ;
	virtual _di_JStaticLayout_Builder __cdecl setMaxLines(int maxLines) = 0 ;
	virtual _di_JStaticLayout_Builder __cdecl setAlignment(Androidapi::Jni::Graphicscontentviewtext::_di_JLayout_Alignment alignment) = 0 ;
	virtual _di_JStaticLayout_Builder __cdecl setIncludePad(bool includePad) = 0 ;
	virtual _di_JStaticLayout_Builder __cdecl setLineSpacing(float spacingAdd, float spacingMult) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJStaticLayout_Builder : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayout_BuilderClass,_di_JStaticLayout_Builder>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayout_BuilderClass,_di_JStaticLayout_Builder> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJStaticLayout_Builder() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStaticLayout_BuilderClass,_di_JStaticLayout_Builder>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJStaticLayout_Builder() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{E59603E0-6F1F-4D07-B426-D2CF52328FC7}") JSpannableStringClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JSpannableString __cdecl init(Androidapi::Jni::Javatypes::_di_JCharSequence source) = 0 ;
};

__interface  INTERFACE_UUID("{0B3C6D0A-37B5-49C0-8AE0-941402C624BA}") JSpannableString  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual void __cdecl setSpan(Androidapi::Jni::Javatypes::_di_JObject what, int start, int end_, int flags) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJSpannableString : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JSpannableStringClass,_di_JSpannableString>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JSpannableStringClass,_di_JSpannableString> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJSpannableString() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JSpannableStringClass,_di_JSpannableString>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJSpannableString() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{BD72E80E-B67D-4CB8-B287-2C7B32758572}") JMetricAffectingSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JMetricAffectingSpan __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{771C211E-1713-4166-820C-BFDA592A5C8A}") JMetricAffectingSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJMetricAffectingSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JMetricAffectingSpanClass,_di_JMetricAffectingSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JMetricAffectingSpanClass,_di_JMetricAffectingSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJMetricAffectingSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JMetricAffectingSpanClass,_di_JMetricAffectingSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJMetricAffectingSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{BD72E80E-B67D-4CB8-B287-2C7B32758572}") JTypefaceSpanClass  : public JMetricAffectingSpanClass 
{
	HIDESBASE virtual _di_JTypefaceSpan __cdecl init(Androidapi::Jni::Javatypes::_di_JString family) = 0 ;
};

__interface  INTERFACE_UUID("{771C211E-1713-4166-820C-BFDA592A5C8A}") JTypefaceSpan  : public JMetricAffectingSpan 
{
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getFamily() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJTypefaceSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JTypefaceSpanClass,_di_JTypefaceSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JTypefaceSpanClass,_di_JTypefaceSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJTypefaceSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JTypefaceSpanClass,_di_JTypefaceSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJTypefaceSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}") JBackgroundColorSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JBackgroundColorSpan __cdecl init(int color) = 0 ;
};

__interface  INTERFACE_UUID("{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}") JBackgroundColorSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	virtual int __cdecl getBackgroundColor() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJBackgroundColorSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JBackgroundColorSpanClass,_di_JBackgroundColorSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JBackgroundColorSpanClass,_di_JBackgroundColorSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJBackgroundColorSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JBackgroundColorSpanClass,_di_JBackgroundColorSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJBackgroundColorSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{A23B8427-0AB5-46AB-B9F8-4081950EFBB0}") JForegroundColorSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JForegroundColorSpan __cdecl init(int color) = 0 ;
};

__interface  INTERFACE_UUID("{2D552A1B-385B-49DF-B82A-5728AF42C366}") JForegroundColorSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	virtual int __cdecl getForegroundColor() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJForegroundColorSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JForegroundColorSpanClass,_di_JForegroundColorSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JForegroundColorSpanClass,_di_JForegroundColorSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJForegroundColorSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JForegroundColorSpanClass,_di_JForegroundColorSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJForegroundColorSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}") JStyleSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JStyleSpan __cdecl init(int style) = 0 ;
};

__interface  INTERFACE_UUID("{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}") JStyleSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	virtual int __cdecl getStyle() = 0 ;
	virtual bool __cdecl isBold() = 0 ;
	virtual bool __cdecl isItalic() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJStyleSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStyleSpanClass,_di_JStyleSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStyleSpanClass,_di_JStyleSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJStyleSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStyleSpanClass,_di_JStyleSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJStyleSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}") JStrikethroughSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JStrikethroughSpan __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}") JStrikethroughSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJStrikethroughSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStrikethroughSpanClass,_di_JStrikethroughSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStrikethroughSpanClass,_di_JStrikethroughSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJStrikethroughSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JStrikethroughSpanClass,_di_JStrikethroughSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJStrikethroughSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}") JUnderlineSpanClass  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyleClass 
{
	HIDESBASE virtual _di_JUnderlineSpan __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}") JUnderlineSpan  : public Androidapi::Jni::Graphicscontentviewtext::JCharacterStyle 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJUnderlineSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JUnderlineSpanClass,_di_JUnderlineSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JUnderlineSpanClass,_di_JUnderlineSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJUnderlineSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JUnderlineSpanClass,_di_JUnderlineSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJUnderlineSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{463312A2-C1C8-480E-967E-77215E32CE7F}") JAbsoluteSizeSpanClass  : public JMetricAffectingSpanClass 
{
	HIDESBASE virtual _di_JAbsoluteSizeSpan __cdecl init(int size) = 0 /* overload */;
	HIDESBASE virtual _di_JAbsoluteSizeSpan __cdecl init(int size, bool dpi) = 0 /* overload */;
};

__interface  INTERFACE_UUID("{E8C2D48E-656F-46F2-BEAD-1A3DC2972B09}") JAbsoluteSizeSpan  : public JMetricAffectingSpan 
{
	virtual int __cdecl getSize() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJAbsoluteSizeSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JAbsoluteSizeSpanClass,_di_JAbsoluteSizeSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JAbsoluteSizeSpanClass,_di_JAbsoluteSizeSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJAbsoluteSizeSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JAbsoluteSizeSpanClass,_di_JAbsoluteSizeSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJAbsoluteSizeSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{743E868A-27D3-479A-BA8B-5C52C4BC1ACB}") JImageSpanClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JImageSpan __cdecl init(Androidapi::Jni::Graphicscontentviewtext::_di_JDrawable d) = 0 /* overload */;
	HIDESBASE virtual _di_JImageSpan __cdecl init(Androidapi::Jni::Graphicscontentviewtext::_di_JDrawable d, int verticalAlignment) = 0 /* overload */;
};

__interface  INTERFACE_UUID("{6DC63921-BDFA-422B-9E3C-6B5886E4E13D}") JImageSpan  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JDrawable __cdecl getDrawable() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getSource() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJImageSpan : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JImageSpanClass,_di_JImageSpan>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JImageSpanClass,_di_JImageSpan> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJImageSpan() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JImageSpanClass,_di_JImageSpan>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJImageSpan() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{1A7A6E4B-A3C2-4DC7-85A8-8F28B22D0978}") JXMLReaderClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{4DA90053-53F4-4EB2-9520-BB1DF8BFC81A}") JXMLReader  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual Androidapi::Jni::Javatypes::_di_JObject __cdecl getProperty(Androidapi::Jni::Javatypes::_di_JString name) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJXMLReader : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JXMLReaderClass,_di_JXMLReader>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JXMLReaderClass,_di_JXMLReader> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJXMLReader() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JXMLReaderClass,_di_JXMLReader>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJXMLReader() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{58259454-6717-4B3C-B641-6E9E3622B282}") JHTML_ImageGetterClass  : public Androidapi::Jnibridge::IJavaClass 
{
	
};

__interface  INTERFACE_UUID("{14179BA1-4E2C-458D-9A5C-210C5AE57A85}") JHTML_ImageGetter  : public Androidapi::Jnibridge::IJavaInstance 
{
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JDrawable __cdecl getDrawable(Androidapi::Jni::Javatypes::_di_JString source) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJHTML_ImageGetter : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_ImageGetterClass,_di_JHTML_ImageGetter>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_ImageGetterClass,_di_JHTML_ImageGetter> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJHTML_ImageGetter() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_ImageGetterClass,_di_JHTML_ImageGetter>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJHTML_ImageGetter() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{58259454-6717-4B3C-B641-6E9E3622B282}") JHTML_TagHandlerClass  : public Androidapi::Jnibridge::IJavaClass 
{
	
};

__interface  INTERFACE_UUID("{14179BA1-4E2C-458D-9A5C-210C5AE57A85}") JHTML_TagHandler  : public Androidapi::Jnibridge::IJavaInstance 
{
	virtual void __cdecl handleTag(bool opening, Androidapi::Jni::Javatypes::_di_JString tag, _di_JEditable output, _di_JXMLReader xmlReader) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJHTML_TagHandler : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_TagHandlerClass,_di_JHTML_TagHandler>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_TagHandlerClass,_di_JHTML_TagHandler> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJHTML_TagHandler() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTML_TagHandlerClass,_di_JHTML_TagHandler>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJHTML_TagHandler() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{6D624AA1-FBF4-45E8-A07F-EB4745BB66FC}") JHTMLClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JSpanned __cdecl fromHTML(Androidapi::Jni::Javatypes::_di_JString source) = 0 /* overload */;
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JSpanned __cdecl fromHTML(Androidapi::Jni::Javatypes::_di_JString source, _di_JHTML_ImageGetter imageGetter, _di_JHTML_TagHandler tagHandler) = 0 /* overload */;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl toHTML(Androidapi::Jni::Graphicscontentviewtext::_di_JSpanned text) = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl escapeHTML(Androidapi::Jni::Javatypes::_di_JCharSequence text) = 0 ;
};

__interface  INTERFACE_UUID("{F9406242-90DB-411D-8770-756662247A5C}") JHTML  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJHTML : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTMLClass,_di_JHTML>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTMLClass,_di_JHTML> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJHTML() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JHTMLClass,_di_JHTML>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJHTML() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{62D77F93-F2EA-4484-9D7E-7C904EBE8947}") JPdfDocumentClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JPdfDocument __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{E4A8CF83-046F-453A-AEB6-6446FF689939}") JPdfDocument  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual void __cdecl close() = 0 ;
	virtual void __cdecl finishPage(_di_JPdfDocument_Page page) = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JList __cdecl getPages() = 0 ;
	virtual _di_JPdfDocument_Page __cdecl startPage(_di_JPdfDocument_PageInfo pageInfo) = 0 ;
	virtual void __cdecl writeTo(Androidapi::Jni::Javatypes::_di_JOutputStream out_) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPdfDocument : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocumentClass,_di_JPdfDocument>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocumentClass,_di_JPdfDocument> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPdfDocument() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocumentClass,_di_JPdfDocument>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPdfDocument() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{4A418EEE-47BF-41CC-A70F-5C08BE3E1CF4}") JPdfDocument_PageClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{E6347FD3-7D0F-4388-A527-BFA12B297B34}") JPdfDocument_Page  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JCanvas __cdecl getCanvas() = 0 ;
	virtual _di_JPdfDocument_PageInfo __cdecl getInfo() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPdfDocument_Page : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageClass,_di_JPdfDocument_Page>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageClass,_di_JPdfDocument_Page> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPdfDocument_Page() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageClass,_di_JPdfDocument_Page>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPdfDocument_Page() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{8B3EA9E0-F8C6-4249-BBFB-9412F998BA64}") JPdfDocument_PageInfoClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{5338321F-FDD1-403A-8E0D-78BF3E75D7A8}") JPdfDocument_PageInfo  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JRect __cdecl getContentRect() = 0 ;
	virtual int __cdecl getPageHeight() = 0 ;
	virtual int __cdecl getPageNumber() = 0 ;
	virtual int __cdecl getPageWidth() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPdfDocument_PageInfo : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageInfoClass,_di_JPdfDocument_PageInfo>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageInfoClass,_di_JPdfDocument_PageInfo> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPdfDocument_PageInfo() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPdfDocument_PageInfoClass,_di_JPdfDocument_PageInfo>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPdfDocument_PageInfo() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{60378CED-24D6-4298-97D3-13245AD3C230}") JPageInfo_BuilderClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JPageInfo_Builder __cdecl init(int pageWidth, int pageHeight, int pageNumber) = 0 ;
};

__interface  INTERFACE_UUID("{9B2C8A40-031A-4065-BF4B-809DFC7EF396}") JPageInfo_Builder  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JPdfDocument_PageInfo __cdecl create() = 0 ;
	virtual _di_JPageInfo_Builder __cdecl setContentRect(Androidapi::Jni::Graphicscontentviewtext::_di_JRect contentRect) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPageInfo_Builder : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPageInfo_BuilderClass,_di_JPageInfo_Builder>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPageInfo_BuilderClass,_di_JPageInfo_Builder> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPageInfo_Builder() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPageInfo_BuilderClass,_di_JPageInfo_Builder>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPageInfo_Builder() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{E463B724-D4A7-4CE3-B265-4A6B1D661C53}") JPrintAttributes_MediaSizeClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_EXEC() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_HAGAKI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_KAHU() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_KAKU2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_OUFUKU() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_YOU4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_FOOLSCAP() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_GOVT_LETTER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_3X5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_4X6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_5X8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_JUNIOR_LEGAL() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LEDGER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LEGAL() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LETTER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_MONARCH() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_QUARTO() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_TABLOID() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_DAI_PA_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_JUURO_KU_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_PA_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_16K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetROC_16K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetROC_8K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetUNKNOWN_LANDSCAPE() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetUNKNOWN_PORTRAIT() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl asLandscape() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl asPortrait() = 0 ;
	virtual bool __cdecl equals(Androidapi::Jni::Javatypes::_di_JObject obj) = 0 ;
	virtual int __cdecl getHeightMils() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getId() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getLabel(Androidapi::Jni::Graphicscontentviewtext::_di_JPackageManager packageManager) = 0 ;
	virtual int __cdecl getWidthMils() = 0 ;
	virtual int __cdecl hashCode() = 0 ;
	HIDESBASE virtual _di_JPrintAttributes_MediaSize __cdecl init(Androidapi::Jni::Javatypes::_di_JString id, Androidapi::Jni::Javatypes::_di_JString label, int widthMils, int heightMils) = 0 ;
	virtual bool __cdecl isPortrait() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl toString() = 0 ;
	__property _di_JPrintAttributes_MediaSize ISO_A0 = {read=_GetISO_A0};
	__property _di_JPrintAttributes_MediaSize ISO_A1 = {read=_GetISO_A1};
	__property _di_JPrintAttributes_MediaSize ISO_A10 = {read=_GetISO_A10};
	__property _di_JPrintAttributes_MediaSize ISO_A2 = {read=_GetISO_A2};
	__property _di_JPrintAttributes_MediaSize ISO_A3 = {read=_GetISO_A3};
	__property _di_JPrintAttributes_MediaSize ISO_A4 = {read=_GetISO_A4};
	__property _di_JPrintAttributes_MediaSize ISO_A5 = {read=_GetISO_A5};
	__property _di_JPrintAttributes_MediaSize ISO_A6 = {read=_GetISO_A6};
	__property _di_JPrintAttributes_MediaSize ISO_A7 = {read=_GetISO_A7};
	__property _di_JPrintAttributes_MediaSize ISO_A8 = {read=_GetISO_A8};
	__property _di_JPrintAttributes_MediaSize ISO_A9 = {read=_GetISO_A9};
	__property _di_JPrintAttributes_MediaSize ISO_B0 = {read=_GetISO_B0};
	__property _di_JPrintAttributes_MediaSize ISO_B1 = {read=_GetISO_B1};
	__property _di_JPrintAttributes_MediaSize ISO_B10 = {read=_GetISO_B10};
	__property _di_JPrintAttributes_MediaSize ISO_B2 = {read=_GetISO_B2};
	__property _di_JPrintAttributes_MediaSize ISO_B3 = {read=_GetISO_B3};
	__property _di_JPrintAttributes_MediaSize ISO_B4 = {read=_GetISO_B4};
	__property _di_JPrintAttributes_MediaSize ISO_B5 = {read=_GetISO_B5};
	__property _di_JPrintAttributes_MediaSize ISO_B6 = {read=_GetISO_B6};
	__property _di_JPrintAttributes_MediaSize ISO_B7 = {read=_GetISO_B7};
	__property _di_JPrintAttributes_MediaSize ISO_B8 = {read=_GetISO_B8};
	__property _di_JPrintAttributes_MediaSize ISO_B9 = {read=_GetISO_B9};
	__property _di_JPrintAttributes_MediaSize ISO_C0 = {read=_GetISO_C0};
	__property _di_JPrintAttributes_MediaSize ISO_C1 = {read=_GetISO_C1};
	__property _di_JPrintAttributes_MediaSize ISO_C10 = {read=_GetISO_C10};
	__property _di_JPrintAttributes_MediaSize ISO_C2 = {read=_GetISO_C2};
	__property _di_JPrintAttributes_MediaSize ISO_C3 = {read=_GetISO_C3};
	__property _di_JPrintAttributes_MediaSize ISO_C4 = {read=_GetISO_C4};
	__property _di_JPrintAttributes_MediaSize ISO_C5 = {read=_GetISO_C5};
	__property _di_JPrintAttributes_MediaSize ISO_C6 = {read=_GetISO_C6};
	__property _di_JPrintAttributes_MediaSize ISO_C7 = {read=_GetISO_C7};
	__property _di_JPrintAttributes_MediaSize ISO_C8 = {read=_GetISO_C8};
	__property _di_JPrintAttributes_MediaSize ISO_C9 = {read=_GetISO_C9};
	__property _di_JPrintAttributes_MediaSize JIS_B0 = {read=_GetJIS_B0};
	__property _di_JPrintAttributes_MediaSize JIS_B1 = {read=_GetJIS_B1};
	__property _di_JPrintAttributes_MediaSize JIS_B10 = {read=_GetJIS_B10};
	__property _di_JPrintAttributes_MediaSize JIS_B2 = {read=_GetJIS_B2};
	__property _di_JPrintAttributes_MediaSize JIS_B3 = {read=_GetJIS_B3};
	__property _di_JPrintAttributes_MediaSize JIS_B4 = {read=_GetJIS_B4};
	__property _di_JPrintAttributes_MediaSize JIS_B5 = {read=_GetJIS_B5};
	__property _di_JPrintAttributes_MediaSize JIS_B6 = {read=_GetJIS_B6};
	__property _di_JPrintAttributes_MediaSize JIS_B7 = {read=_GetJIS_B7};
	__property _di_JPrintAttributes_MediaSize JIS_B8 = {read=_GetJIS_B8};
	__property _di_JPrintAttributes_MediaSize JIS_B9 = {read=_GetJIS_B9};
	__property _di_JPrintAttributes_MediaSize JIS_EXEC = {read=_GetJIS_EXEC};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU2 = {read=_GetJPN_CHOU2};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU3 = {read=_GetJPN_CHOU3};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU4 = {read=_GetJPN_CHOU4};
	__property _di_JPrintAttributes_MediaSize JPN_HAGAKI = {read=_GetJPN_HAGAKI};
	__property _di_JPrintAttributes_MediaSize JPN_KAHU = {read=_GetJPN_KAHU};
	__property _di_JPrintAttributes_MediaSize JPN_KAKU2 = {read=_GetJPN_KAKU2};
	__property _di_JPrintAttributes_MediaSize JPN_OUFUKU = {read=_GetJPN_OUFUKU};
	__property _di_JPrintAttributes_MediaSize JPN_YOU4 = {read=_GetJPN_YOU4};
	__property _di_JPrintAttributes_MediaSize NA_FOOLSCAP = {read=_GetNA_FOOLSCAP};
	__property _di_JPrintAttributes_MediaSize NA_GOVT_LETTER = {read=_GetNA_GOVT_LETTER};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_3X5 = {read=_GetNA_INDEX_3X5};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_4X6 = {read=_GetNA_INDEX_4X6};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_5X8 = {read=_GetNA_INDEX_5X8};
	__property _di_JPrintAttributes_MediaSize NA_JUNIOR_LEGAL = {read=_GetNA_JUNIOR_LEGAL};
	__property _di_JPrintAttributes_MediaSize NA_LEDGER = {read=_GetNA_LEDGER};
	__property _di_JPrintAttributes_MediaSize NA_LEGAL = {read=_GetNA_LEGAL};
	__property _di_JPrintAttributes_MediaSize NA_LETTER = {read=_GetNA_LETTER};
	__property _di_JPrintAttributes_MediaSize NA_MONARCH = {read=_GetNA_MONARCH};
	__property _di_JPrintAttributes_MediaSize NA_QUARTO = {read=_GetNA_QUARTO};
	__property _di_JPrintAttributes_MediaSize NA_TABLOID = {read=_GetNA_TABLOID};
	__property _di_JPrintAttributes_MediaSize OM_DAI_PA_KAI = {read=_GetOM_DAI_PA_KAI};
	__property _di_JPrintAttributes_MediaSize OM_JUURO_KU_KAI = {read=_GetOM_JUURO_KU_KAI};
	__property _di_JPrintAttributes_MediaSize OM_PA_KAI = {read=_GetOM_PA_KAI};
	__property _di_JPrintAttributes_MediaSize PRC_1 = {read=_GetPRC_1};
	__property _di_JPrintAttributes_MediaSize PRC_10 = {read=_GetPRC_10};
	__property _di_JPrintAttributes_MediaSize PRC_16K = {read=_GetPRC_16K};
	__property _di_JPrintAttributes_MediaSize PRC_2 = {read=_GetPRC_2};
	__property _di_JPrintAttributes_MediaSize PRC_3 = {read=_GetPRC_3};
	__property _di_JPrintAttributes_MediaSize PRC_4 = {read=_GetPRC_4};
	__property _di_JPrintAttributes_MediaSize PRC_5 = {read=_GetPRC_5};
	__property _di_JPrintAttributes_MediaSize PRC_6 = {read=_GetPRC_6};
	__property _di_JPrintAttributes_MediaSize PRC_7 = {read=_GetPRC_7};
	__property _di_JPrintAttributes_MediaSize PRC_8 = {read=_GetPRC_8};
	__property _di_JPrintAttributes_MediaSize PRC_9 = {read=_GetPRC_9};
	__property _di_JPrintAttributes_MediaSize ROC_16K = {read=_GetROC_16K};
	__property _di_JPrintAttributes_MediaSize ROC_8K = {read=_GetROC_8K};
	__property _di_JPrintAttributes_MediaSize UNKNOWN_LANDSCAPE = {read=_GetUNKNOWN_LANDSCAPE};
	__property _di_JPrintAttributes_MediaSize UNKNOWN_PORTRAIT = {read=_GetUNKNOWN_PORTRAIT};
};

__interface  INTERFACE_UUID("{3D89217B-93F4-4DB3-9E92-F99E06B9EFD1}") JPrintAttributes_MediaSize  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JPrintAttributes_MediaSize __cdecl asLandscape() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl asPortrait() = 0 ;
	HIDESBASE virtual bool __cdecl equals(Androidapi::Jni::Javatypes::_di_JObject obj) = 0 ;
	virtual int __cdecl getHeightMils() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getId() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getLabel(Androidapi::Jni::Graphicscontentviewtext::_di_JPackageManager packageManager) = 0 ;
	virtual int __cdecl getWidthMils() = 0 ;
	HIDESBASE virtual int __cdecl hashCode() = 0 ;
	virtual bool __cdecl isPortrait() = 0 ;
	HIDESBASE virtual Androidapi::Jni::Javatypes::_di_JString __cdecl toString() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_MediaSize : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_MediaSize() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_MediaSize() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{890A9602-792D-417F-B413-41CD6576A178}") JPrintAttributes_ResolutionClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{0E4C6F96-FB83-4187-B26C-332F9DBDEBD3}") JPrintAttributes_Resolution  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Resolution : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Resolution() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Resolution() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{CB38E8EF-9112-4F1F-B713-0B2E03A8EDF3}") JPrintAttributes_MarginsClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{21BF97D5-0C39-4F71-BA25-1B1C168BF23E}") JPrintAttributes_Margins  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Margins : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Margins() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Margins() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{16CFEB84-F40A-45E2-8220-4AF3682B7FD7}") JPrintAttributesClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual int __cdecl _GetCOLOR_MODE_COLOR() = 0 ;
	virtual int __cdecl _GetCOLOR_MODE_MONOCHROME() = 0 ;
	__property int COLOR_MODE_COLOR = {read=_GetCOLOR_MODE_COLOR};
	__property int COLOR_MODE_MONOCHROME = {read=_GetCOLOR_MODE_MONOCHROME};
};

__interface  INTERFACE_UUID("{899388B0-B3B2-4259-B5A3-147D43392C60}") JPrintAttributes  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{62D77F93-F2EA-4484-9D7E-7C904EBE8947}") JPrintAttributes_BuilderClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JPrintAttributes_Builder __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{E4A8CF83-046F-453A-AEB6-6446FF689939}") JPrintAttributes_Builder  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JPrintAttributes __cdecl build() = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setColorMode(int colorMode) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setMediaSize(_di_JPrintAttributes_MediaSize mediaSize) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setMargins(_di_JPrintAttributes_Margins Margins) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setResolution(_di_JPrintAttributes_Resolution Resolution) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Builder : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Builder() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Builder() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{62D77F93-F2EA-4484-9D7E-7C904EBE8947}") JPrintedPdfDocumentClass  : public JPdfDocumentClass 
{
	HIDESBASE virtual _di_JPrintedPdfDocument __cdecl init(Androidapi::Jni::Graphicscontentviewtext::_di_JContext context, _di_JPrintAttributes attributes) = 0 ;
};

__interface  INTERFACE_UUID("{E4A8CF83-046F-453A-AEB6-6446FF689939}") JPrintedPdfDocument  : public JPdfDocument 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintedPdfDocument : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintedPdfDocumentClass,_di_JPrintedPdfDocument>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintedPdfDocumentClass,_di_JPrintedPdfDocument> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintedPdfDocument() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintedPdfDocumentClass,_di_JPrintedPdfDocument>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintedPdfDocument() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{84A38124-9D41-4471-A616-31BCB6E2F798}") JEditableClass  : public Androidapi::Jni::Javatypes::JCharSequenceClass 
{
	
};

__interface  INTERFACE_UUID("{5FA14BBC-C3C3-492C-BF3F-5770374929C9}") JEditable  : public Androidapi::Jni::Javatypes::JCharSequence 
{
	virtual _di_JEditable __cdecl append(Androidapi::Jni::Javatypes::_di_JCharSequence text) = 0 /* overload */;
	virtual _di_JEditable __cdecl append(Androidapi::Jni::Javatypes::_di_JCharSequence text, int start, int end_) = 0 /* overload */;
	virtual _di_JEditable __cdecl append(System::WideChar text) = 0 /* overload */;
	virtual void __cdecl clear() = 0 ;
	virtual void __cdecl clearSpans() = 0 ;
	virtual _di_JEditable __cdecl Delete(int st, int en) = 0 ;
	virtual Androidapi::Jnibridge::TJavaObjectArray__1<Androidapi::Jni::Graphicscontentviewtext::_di_JInputFilter>* __cdecl getFilters() = 0 ;
	virtual _di_JEditable __cdecl insert(int where, Androidapi::Jni::Javatypes::_di_JCharSequence text, int start, int end_) = 0 /* overload */;
	virtual _di_JEditable __cdecl insert(int where, Androidapi::Jni::Javatypes::_di_JCharSequence text) = 0 /* overload */;
	virtual _di_JEditable __cdecl replace(int st, int en, Androidapi::Jni::Javatypes::_di_JCharSequence source, int start, int end_) = 0 /* overload */;
	virtual _di_JEditable __cdecl replace(int st, int en, Androidapi::Jni::Javatypes::_di_JCharSequence text) = 0 /* overload */;
	virtual void __cdecl setFilters(Androidapi::Jnibridge::TJavaObjectArray__1<Androidapi::Jni::Graphicscontentviewtext::_di_JInputFilter>* filters) = 0 ;
	virtual void __cdecl setSpan(Androidapi::Jni::Javatypes::_di_JObject what, int start, int end_, int flags) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJEditable : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JEditableClass,_di_JEditable>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JEditableClass,_di_JEditable> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJEditable() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JEditableClass,_di_JEditable>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJEditable() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::WideChar PDFCR = (System::WideChar)(0xd);
static constexpr System::WideChar PDFLF = (System::WideChar)(0xa);
#define PDFLB u"\r\n"
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 8> PDFEC;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 8> PDFRC;
#define PDFLHFACTOR  (1.400000E+00)
#define PDFULFACTOR  (8.000000E-01)
#define PDFSTFACTOR  (5.000000E-01)
#define PDFULLWFACTOR  (5.000000E-02)
#define DefaultFontName u"Roboto"
#define PDFMetaData u"<?xpacket begin=\"\u00ef\u00bb\u00bf\" id=\"W5M0MpCehiHzre"\
	u"SzNTczkc9d\"?><x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmpt"\
	u"k=\"%s\"><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22"\
	u"-rdf-syntax-ns#\"><rdf:Description rdf:about=\"\" xmlns:xm"\
	u"p=\"http://ns.adobe.com/xap/1.0/\"><xmp:CreateDate>%s</xmp"\
	u":CreateDate><xmp:ModifyDate>%s</xmp:ModifyDate><xmp:Creato"\
	u"rTool>%s</xmp:CreatorTool></rdf:Description><rdf:Descripti"\
	u"on rdf:about=\"\" xmlns:dc=\"http://purl.org/dc/elements/1"\
	u".1/\"><dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">%s"\
	u"</rdf:li></rdf:Alt></dc:title><dc:creator><rdf:Seq><rdf:li"\
	u" xml:lang=\"x-default\">%s</rdf:li></rdf:Seq></dc:creator>"\
	u"<dc:description><rdf:Alt><rdf:li xml:lang=\"x-default\">%s"\
	u"</rdf:li></rdf:Alt></dc:description></rdf:Description><rdf"\
	u":Description rdf:about=\"\" xmlns:pdf=\"http://ns.adobe.co"\
	u"m/pdf/1.3/\"><pdf:Keywords>%s</pdf:Keywords><pdf:Producer>"\
	u"%s</pdf:Producer></rdf:Description><rdf:Description rdf:ab"\
	u"out=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">"\
	u"<pdfaid:part>1</pdfaid:part><pdfaid:conformance>A</pdfaid:"\
	u"conformance></rdf:Description></rdf:RDF></x:xmpmeta><?xpac"\
	u"ket end=\"w\"?>"
extern DELPHI_PACKAGE bool __fastcall CanCompress(void);
extern DELPHI_PACKAGE TTMSFNCPDFGraphicsLibOutputWriterStream* __fastcall CreateStringStream(System::UnicodeString AValue = System::UnicodeString());
extern DELPHI_PACKAGE int __fastcall MaxRange(const TTMSFNCPDFGraphicsLibTextRange &ARange);
extern DELPHI_PACKAGE TTMSFNCPDFGraphicsLibTextRange __fastcall MakeTextRange(int ALocation, int ALength);
extern DELPHI_PACKAGE void __fastcall GetAspectSize(float &x, float &y, float &w, float &h, float ow, float oh, float nw, float nh, bool AspectRatio, bool Stretch, bool Cropping);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DateTimeToIso(System::TDateTime ADateTime);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertToPDFDate(System::TDateTime ADateTime);
extern DELPHI_PACKAGE unsigned __fastcall GetTickCountX(void);
extern DELPHI_PACKAGE void __fastcall FontToJTextPaint(TTMSFNCPDFGraphicsLibFont* AFont, Androidapi::Jni::Graphicscontentviewtext::_di_JTextPaint ATextPaint);
extern DELPHI_PACKAGE int __fastcall AlphaColorToJColor(System::Uitypes::TAlphaColor AColor);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall JColorToAlphaColor(int AColor);
extern DELPHI_PACKAGE Androidapi::Jni::Graphicscontentviewtext::_di_JBitmap __fastcall ImageFromBitmap(Fmx::Graphics::TBitmap* ABitmap);
}	/* namespace Tmsfncpdfcorelibbase */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFCORELIBBASE)
using namespace Fmx::Tmsfncpdfcorelibbase;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfcorelibbaseHPP
