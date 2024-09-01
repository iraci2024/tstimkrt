// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFIO.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdfioHPP
#define Fmx_TmsfncpdfioHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCPDFLib.hpp>
#include <FMX.TMSFNCPDFCoreLibBase.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCPDFGraphicsLib.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <System.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCPDFIOInformation;
class DELPHICLASS TTMSFNCPDFIOOptions;
class DELPHICLASS TTMSFNCCustomPDFIO;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFIOInformation : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAuthor;
	System::UnicodeString FCreator;
	System::UnicodeString FSubject;
	System::UnicodeString FTitle;
	System::Classes::TStringList* FKeywords;
	void __fastcall SetKeywords(System::Classes::TStringList* const Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCPDFIOInformation();
	__fastcall virtual ~TTMSFNCPDFIOInformation();
	
__published:
	__property System::UnicodeString Author = {read=FAuthor, write=FAuthor};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property System::UnicodeString Subject = {read=FSubject, write=FSubject};
	__property System::Classes::TStringList* Keywords = {read=FKeywords, write=SetKeywords};
	__property System::UnicodeString Creator = {read=FCreator, write=FCreator};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFIOOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FOpenInPDFReader;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FDefaultFont;
	System::UnicodeString FFooter;
	System::UnicodeString FHeader;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FMargins;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FHeaderFont;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FFooterFont;
	float FHeaderSize;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FFooterMargins;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FFooterAlignment;
	float FFooterSize;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FHeaderMargins;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FHeaderAlignment;
	Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageSize FPageSize;
	Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageOrientation FPageOrientation;
	float FPageWidth;
	float FPageHeight;
	bool FExportImages;
	System::Classes::TStrings* FFontFallBackList;
	bool FEmbedFonts;
	System::UnicodeString FPageNumberFormat;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FPageNumberAlignment;
	Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageNumber FPageNumber;
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FPageNumberFont;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FPageNumberMargins;
	float FPageNumberSize;
	void __fastcall SetDefaultFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetFooterFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetHeaderFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetFooterAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetFooterMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetFooterSize(const float Value);
	void __fastcall SetHeaderAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetHeaderMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetHeaderSize(const float Value);
	bool __fastcall IsFooterSizeStored();
	bool __fastcall IsHeaderSizeStored();
	void __fastcall SetPageOrientation(const Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageOrientation Value);
	void __fastcall SetPageSize(const Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageSize Value);
	bool __fastcall IsPageHeightStored();
	bool __fastcall IsPageWidthStored();
	void __fastcall SetPageHeight(const float Value);
	void __fastcall SetPageWidth(const float Value);
	void __fastcall SetExportImages(const bool Value);
	void __fastcall SetEmbedFonts(const bool Value);
	void __fastcall SetFontFallBackList(System::Classes::TStrings* const Value);
	void __fastcall SetPageNumber(const Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageNumber Value);
	void __fastcall SetPageNumberAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetPageNumberFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetPageNumberFormat(const System::UnicodeString Value);
	void __fastcall SetPageNumberMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	bool __fastcall IsPageNumberSizeStored();
	void __fastcall SetPageNumberSize(const float Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCPDFIOOptions();
	__fastcall virtual ~TTMSFNCPDFIOOptions();
	
__published:
	__property bool OpenInPDFReader = {read=FOpenInPDFReader, write=FOpenInPDFReader, default=0};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* DefaultFont = {read=FDefaultFont, write=SetDefaultFont};
	__property System::UnicodeString Header = {read=FHeader, write=FHeader};
	__property System::UnicodeString Footer = {read=FFooter, write=FFooter};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* Margins = {read=FMargins, write=SetMargins};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* HeaderFont = {read=FHeaderFont, write=SetHeaderFont};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FooterFont = {read=FFooterFont, write=SetFooterFont};
	__property float HeaderSize = {read=FHeaderSize, write=SetHeaderSize, stored=IsHeaderSizeStored};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* HeaderMargins = {read=FHeaderMargins, write=SetHeaderMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign HeaderAlignment = {read=FHeaderAlignment, write=SetHeaderAlignment, default=0};
	__property float FooterSize = {read=FFooterSize, write=SetFooterSize, stored=IsFooterSizeStored};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* FooterMargins = {read=FFooterMargins, write=SetFooterMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FooterAlignment = {read=FFooterAlignment, write=SetFooterAlignment, default=0};
	__property float PageWidth = {read=FPageWidth, write=SetPageWidth, stored=IsPageWidthStored};
	__property float PageHeight = {read=FPageHeight, write=SetPageHeight, stored=IsPageHeightStored};
	__property Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageSize PageSize = {read=FPageSize, write=SetPageSize, default=34};
	__property Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageOrientation PageOrientation = {read=FPageOrientation, write=SetPageOrientation, default=0};
	__property bool ExportImages = {read=FExportImages, write=SetExportImages, default=1};
	__property System::Classes::TStrings* FontFallBackList = {read=FFontFallBackList, write=SetFontFallBackList};
	__property bool EmbedFonts = {read=FEmbedFonts, write=SetEmbedFonts, default=1};
	__property Fmx::Tmsfncpdflib::TTMSFNCPDFLibPageNumber PageNumber = {read=FPageNumber, write=SetPageNumber, default=1};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PageNumberMargins = {read=FPageNumberMargins, write=SetPageNumberMargins};
	__property System::UnicodeString PageNumberFormat = {read=FPageNumberFormat, write=SetPageNumberFormat};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign PageNumberAlignment = {read=FPageNumberAlignment, write=SetPageNumberAlignment, default=2};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* PageNumberFont = {read=FPageNumberFont, write=SetPageNumberFont};
	__property float PageNumberSize = {read=FPageNumberSize, write=SetPageNumberSize, stored=IsPageNumberSizeStored};
};

#pragma pack(pop)

typedef System::Classes::TComponent TTMSFNCPDFIOExportObject;

typedef void __fastcall (__closure *TTMSFNCPDFIOGetHeaderEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString &AHeader);

typedef void __fastcall (__closure *TTMSFNCPDFIOGetFooterEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString &AFooter);

typedef void __fastcall (__closure *TTMSFNCPDFIOBeforeDrawContentEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFIOAfterDrawContentEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFIOBeforeDrawHeaderEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFIOBeforeDrawPageNumberEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFIOBeforeDrawFooterEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFIOAfterDrawHeaderEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFIOAfterDrawPageNumberEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFIOAfterDrawEvent)(System::TObject* Sender, Fmx::Tmsfncpdflib::TTMSFNCPDFLib* APDFLib);

typedef void __fastcall (__closure *TTMSFNCPDFIOAfterDrawFooterEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef System::DynamicArray<System::Classes::TComponent*> TTMSFNCPDFIOExportObjectArray;

typedef System::DynamicArray<System::Types::TRectF> TTMSFNCPDFIOExportRectArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomPDFIO : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	bool FNewPageStarted;
	TTMSFNCPDFIOInformation* FInformation;
	TTMSFNCPDFIOOptions* FOptions;
	TTMSFNCPDFIOGetHeaderEvent FOnGetHeader;
	TTMSFNCPDFIOGetFooterEvent FOnGetFooter;
	TTMSFNCPDFIOBeforeDrawHeaderEvent FOnBeforeDrawHeader;
	TTMSFNCPDFIOBeforeDrawPageNumberEvent FOnBeforeDrawPageNumber;
	TTMSFNCPDFIOBeforeDrawContentEvent FOnBeforeDrawContent;
	TTMSFNCPDFIOBeforeDrawFooterEvent FOnBeforeDrawFooter;
	TTMSFNCPDFIOAfterDrawHeaderEvent FOnAfterDrawHeader;
	TTMSFNCPDFIOAfterDrawPageNumberEvent FOnAfterDrawPageNumber;
	TTMSFNCPDFIOAfterDrawFooterEvent FOnAfterDrawFooter;
	TTMSFNCPDFIOAfterDrawContentEvent FOnAfterDrawContent;
	System::Classes::TComponent* FActiveExportObject;
	System::Classes::TComponent* FExportObject;
	TTMSFNCPDFIOAfterDrawEvent FOnAfterDraw;
	void __fastcall SetInformation(TTMSFNCPDFIOInformation* const Value);
	void __fastcall SetOptions(TTMSFNCPDFIOOptions* const Value);
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer();
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual NativeUInt __fastcall GetInstance();
	virtual TTMSFNCPDFIOOptions* __fastcall CreateOptions();
	virtual bool __fastcall NewPage(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject);
	virtual System::Types::TRectF __fastcall GetContentRect(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib);
	virtual Fmx::Tmsfncpdflib::TTMSFNCPDFLib* __fastcall InitializePDFLib();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall EndPage(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject);
	virtual void __fastcall DoNewPageStarted(System::TObject* Sender, int APageIndex);
	virtual void __fastcall DoGetHeader(System::Classes::TComponent* AExportObject, int APageIndex, System::UnicodeString &AHeader);
	virtual void __fastcall DoGetFooter(System::Classes::TComponent* AExportObject, int APageIndex, System::UnicodeString &AFooter);
	virtual void __fastcall DoPDFExport(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, const System::Types::TRectF &AExportRect) = 0 /* overload */;
	virtual void __fastcall DoAfterDraw(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* APDFLib);
	virtual void __fastcall DoBeforeDrawHeader(System::TObject* Sender, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);
	virtual void __fastcall DoBeforeDrawPageNumber(System::TObject* Sender, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);
	virtual void __fastcall DoBeforeDrawFooter(System::TObject* Sender, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);
	virtual void __fastcall DoAfterDrawHeader(System::TObject* Sender, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);
	virtual void __fastcall DoAfterDrawPageNumber(System::TObject* Sender, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);
	virtual void __fastcall DoAfterDrawFooter(System::TObject* Sender, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);
	virtual void __fastcall DoBeforeDrawContent(System::Classes::TComponent* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);
	virtual void __fastcall DoAfterDrawContent(System::Classes::TComponent* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
	__property TTMSFNCPDFIOInformation* Information = {read=FInformation, write=SetInformation};
	__property TTMSFNCPDFIOOptions* Options = {read=FOptions, write=SetOptions};
	__property TTMSFNCPDFIOGetHeaderEvent OnGetHeader = {read=FOnGetHeader, write=FOnGetHeader};
	__property TTMSFNCPDFIOGetFooterEvent OnGetFooter = {read=FOnGetFooter, write=FOnGetFooter};
	__property TTMSFNCPDFIOBeforeDrawHeaderEvent OnBeforeDrawHeader = {read=FOnBeforeDrawHeader, write=FOnBeforeDrawHeader};
	__property TTMSFNCPDFIOBeforeDrawPageNumberEvent OnBeforeDrawPageNumber = {read=FOnBeforeDrawPageNumber, write=FOnBeforeDrawPageNumber};
	__property TTMSFNCPDFIOAfterDrawHeaderEvent OnAfterDrawHeader = {read=FOnAfterDrawHeader, write=FOnAfterDrawHeader};
	__property TTMSFNCPDFIOAfterDrawPageNumberEvent OnAfterDrawPageNumber = {read=FOnAfterDrawPageNumber, write=FOnAfterDrawPageNumber};
	__property TTMSFNCPDFIOBeforeDrawFooterEvent OnBeforeDrawFooter = {read=FOnBeforeDrawFooter, write=FOnBeforeDrawFooter};
	__property TTMSFNCPDFIOAfterDrawFooterEvent OnAfterDrawFooter = {read=FOnAfterDrawFooter, write=FOnAfterDrawFooter};
	__property TTMSFNCPDFIOBeforeDrawContentEvent OnBeforeDrawContent = {read=FOnBeforeDrawContent, write=FOnBeforeDrawContent};
	__property TTMSFNCPDFIOAfterDrawContentEvent OnAfterDrawContent = {read=FOnAfterDrawContent, write=FOnAfterDrawContent};
	__property TTMSFNCPDFIOAfterDrawEvent OnAfterDraw = {read=FOnAfterDraw, write=FOnAfterDraw};
	__property System::Classes::TComponent* ExportObject = {read=FExportObject, write=FExportObject};
	
public:
	__fastcall virtual TTMSFNCCustomPDFIO(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomPDFIO();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Save(const System::UnicodeString AFileName)/* overload */;
	void __fastcall Save(System::Classes::TStream* const AStream)/* overload */;
	void __fastcall Save(const System::UnicodeString AFileName, System::Classes::TComponent* AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	void __fastcall Save(System::Classes::TStream* const AStream, System::Classes::TComponent* AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	void __fastcall Save(const System::UnicodeString AFileName, TTMSFNCPDFIOExportObjectArray AExportObjects)/* overload */;
	void __fastcall Save(System::Classes::TStream* const AStream, TTMSFNCPDFIOExportObjectArray AExportObjects)/* overload */;
	void __fastcall Save(const System::UnicodeString AFileName, TTMSFNCPDFIOExportObjectArray AExportObjects, TTMSFNCPDFIOExportRectArray AExportRects)/* overload */;
	void __fastcall Save(System::Classes::TStream* const AStream, TTMSFNCPDFIOExportObjectArray AExportObjects, TTMSFNCPDFIOExportRectArray AExportRects)/* overload */;
private:
	void *__ITMSFNCBitmapContainer;	// Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {ED26710D-395F-4971-8AC9-A31083BF2A3C}
	operator Fmx::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer()
	{
		Fmx::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*(void) { return (Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*)&__ITMSFNCBitmapContainer; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncpdfio */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFIO)
using namespace Fmx::Tmsfncpdfio;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfioHPP
