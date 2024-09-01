// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFLib.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpdflibHPP
#define Fmx_TmsfncpdflibHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <FMX.TMSFNCPDFGraphicsLib.hpp>
#include <FMX.TMSFNCPDFCoreLibBase.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCPDFRichTextLib.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdflib
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCCustomPDFLib;
typedef System::DelphiInterface<ITMSFNCCustomPDFLib> _di_ITMSFNCCustomPDFLib;
__interface DELPHIINTERFACE ITMSFNCPDFLibService;
typedef System::DelphiInterface<ITMSFNCPDFLibService> _di_ITMSFNCPDFLibService;
__interface DELPHIINTERFACE ITMSFNCPDFLibGeneralService;
typedef System::DelphiInterface<ITMSFNCPDFLibGeneralService> _di_ITMSFNCPDFLibGeneralService;
class DELPHICLASS TTMSFNCCustomPDFLib;
class DELPHICLASS TTMSFNCPDFLibList;
class DELPHICLASS TTMSFNCPDFLibFactoryService;
class DELPHICLASS TTMSFNCPDFLib;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCPDFLibPageSize : unsigned char { psA0, psA1, psA2, psA3, psA4, psA5, psA6, psA7, psA8, psB0, psB1, psB2, psB3, psB4, psB5, psB6, psB7, psB8, psB9, psB10, psC2, psC3, psC4, psC5, psC6, psD0, psSRA0, psSRA1, psSRA2, psSRA3, psSRA4, psRA0, psRA1, psRA2, psLetter, psLegal, psLedger, psTabloid, psExecutive, psANSIC, psANSID, psANSIE, psFoolscap, psSmallPost, psSheetAnd13Cap, psSheetAnd12Cap, psDemy, psLargePost, psSmallmedium, psMedium, psSmallRoyal, psRoyal, psImperial, psMetricCrownQuarto, psMetricCrownOctavo, psMetricLargeCrownQuarto, psMetricLargeCrownOctavo, psMetricDemyQuarto, psMetricDemyOctavo, psMetricRoyalQuarto, psMetricRoyalOctavo, psCustom };

enum DECLSPEC_DENUM TTMSFNCPDFLibStandard : unsigned char { pdfNone, pdfA1 };

enum DECLSPEC_DENUM TTMSFNCPDFLibPageOrientation : unsigned char { poPortrait, poLandscape };

enum DECLSPEC_DENUM TTMSFNCPDFLibPageNumber : unsigned char { pnNone, pnHeader, pnFooter };

typedef void __fastcall (__closure *TTMSFNCPDFLibBeforeDrawHeaderEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFLibBeforeDrawFooterEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFLibBeforeDrawPageNumberEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPDFLibAfterDrawHeaderEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString AHeader, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFLibAfterDrawFooterEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString AFooter, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFLibAfterDrawPageNumberEvent)(System::TObject* Sender, int APageIndex, System::UnicodeString APageNumber, const System::Types::TRectF &ARect, Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib AGraphics);

typedef void __fastcall (__closure *TTMSFNCPDFLibNewPageStartedEvent)(System::TObject* Sender, int APageIndex);

__interface  INTERFACE_UUID("{69FD5F00-62C7-48D8-878A-B19F31C9537B}") ITMSFNCCustomPDFLib  : public System::IInterface 
{
	virtual void __fastcall BeginDocument(System::UnicodeString FileName = System::UnicodeString()) = 0 ;
	virtual void __fastcall OpenDocument(System::UnicodeString FileName) = 0 /* overload */;
	virtual void __fastcall OpenDocument(System::Classes::TMemoryStream* FileStream) = 0 /* overload */;
	virtual void __fastcall SaveDocumentFromStream(System::Classes::TMemoryStream* FileStream, System::UnicodeString FileName) = 0 ;
	virtual void __fastcall GetDocumentInfo() = 0 ;
	virtual void __fastcall GetPageInfo(int PageIndex) = 0 ;
	virtual void __fastcall CloseDocument() = 0 ;
	virtual void __fastcall NewPage() = 0 ;
	virtual void __fastcall InsertPage(int PageIndex) = 0 ;
	virtual void __fastcall DrawPage(int PageIndex) = 0 ;
	virtual void __fastcall SetPageSize(const TTMSFNCPDFLibPageSize Value) = 0 ;
	virtual void __fastcall SetPageOrientation(const TTMSFNCPDFLibPageOrientation Value) = 0 ;
	virtual void __fastcall SetHeaderMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value) = 0 ;
	virtual void __fastcall SetFooterMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value) = 0 ;
	virtual void __fastcall SetPageNumberMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value) = 0 ;
	virtual void __fastcall SetArtBox(const System::Types::TRectF &Value) = 0 ;
	virtual void __fastcall SetBleedBox(const System::Types::TRectF &Value) = 0 ;
	virtual void __fastcall SetCropBox(const System::Types::TRectF &Value) = 0 ;
	virtual void __fastcall SetMediaBox(const System::Types::TRectF &Value) = 0 ;
	virtual void __fastcall SetTrimBox(const System::Types::TRectF &Value) = 0 ;
	virtual void __fastcall SetPageWidth(const float Value) = 0 ;
	virtual void __fastcall SetPageHeight(const float Value) = 0 ;
	virtual void __fastcall SetAllowsCopying(const bool Value) = 0 ;
	virtual void __fastcall SetAllowsPrinting(const bool Value) = 0 ;
	virtual void __fastcall SetFooter(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetFooterAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value) = 0 ;
	virtual void __fastcall SetFooterSize(const float Value) = 0 ;
	virtual void __fastcall SetHeader(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetHeaderAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value) = 0 ;
	virtual void __fastcall SetHeaderSize(const float Value) = 0 ;
	virtual void __fastcall SetPageNumber(const TTMSFNCPDFLibPageNumber Value) = 0 ;
	virtual void __fastcall SetPageNumberFormat(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetPageNumberAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value) = 0 ;
	virtual void __fastcall SetPageNumberSize(const float Value) = 0 ;
	virtual void __fastcall SetKeywords(System::Classes::TStrings* const Value) = 0 ;
	virtual void __fastcall SetFontFallBackList(System::Classes::TStrings* const Value) = 0 ;
	virtual void __fastcall SetOwnerPassword(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetSubject(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetTitle(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetUserPassword(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetAuthor(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetCreator(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value) = 0 ;
	virtual void __fastcall SetEmbedFonts(const bool Value) = 0 ;
	virtual void __fastcall SetFooterFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value) = 0 ;
	virtual void __fastcall SetHeaderFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value) = 0 ;
	virtual void __fastcall SetOnBeforeDrawHeader(const TTMSFNCPDFLibBeforeDrawHeaderEvent Value) = 0 ;
	virtual void __fastcall SetOnAfterDrawHeader(const TTMSFNCPDFLibAfterDrawHeaderEvent Value) = 0 ;
	virtual void __fastcall SetPageNumberFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value) = 0 ;
	virtual void __fastcall SetOnBeforeDrawPageNumber(const TTMSFNCPDFLibBeforeDrawPageNumberEvent Value) = 0 ;
	virtual void __fastcall SetOnAfterDrawPageNumber(const TTMSFNCPDFLibAfterDrawPageNumberEvent Value) = 0 ;
	virtual void __fastcall SetOnBeforeDrawFooter(const TTMSFNCPDFLibBeforeDrawFooterEvent Value) = 0 ;
	virtual void __fastcall SetOnAfterDrawFooter(const TTMSFNCPDFLibAfterDrawFooterEvent Value) = 0 ;
	virtual void __fastcall SetOnNewPageStarted(const TTMSFNCPDFLibNewPageStartedEvent Value) = 0 ;
	virtual void __fastcall SetPDFStandard(const TTMSFNCPDFLibStandard Value) = 0 ;
	virtual void __fastcall DrawHeader() = 0 ;
	virtual void __fastcall DrawFooter() = 0 ;
	virtual void __fastcall DrawPageNumber() = 0 ;
	virtual void __fastcall SetPDFGraphicsLib(const Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib Value) = 0 ;
	virtual void __fastcall SetPDFGraphicsExLib(const Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsExLib Value) = 0 ;
	virtual void __fastcall SetPDFInitializationLib(const Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFInitializationLib Value) = 0 ;
	virtual Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib __fastcall GetPDFGraphicsLib() = 0 ;
	virtual Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsExLib __fastcall GetPDFGraphicsExLib() = 0 ;
	virtual Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFInitializationLib __fastcall GetPDFInitializationLib() = 0 ;
	virtual TTMSFNCPDFLibStandard __fastcall GetPDFStandard() = 0 ;
	virtual System::Types::TRectF __fastcall GetHeaderRect() = 0 ;
	virtual System::Types::TRectF __fastcall GetPageNumberRect() = 0 ;
	virtual System::Types::TRectF __fastcall GetFooterRect() = 0 ;
	virtual TTMSFNCPDFLibBeforeDrawHeaderEvent __fastcall GetOnBeforeDrawHeader() = 0 ;
	virtual TTMSFNCPDFLibAfterDrawHeaderEvent __fastcall GetOnAfterDrawHeader() = 0 ;
	virtual TTMSFNCPDFLibBeforeDrawPageNumberEvent __fastcall GetOnBeforeDrawPageNumber() = 0 ;
	virtual TTMSFNCPDFLibAfterDrawPageNumberEvent __fastcall GetOnAfterDrawPageNumber() = 0 ;
	virtual TTMSFNCPDFLibBeforeDrawFooterEvent __fastcall GetOnBeforeDrawFooter() = 0 ;
	virtual TTMSFNCPDFLibAfterDrawFooterEvent __fastcall GetOnAfterDrawFooter() = 0 ;
	virtual TTMSFNCPDFLibNewPageStartedEvent __fastcall GetOnNewPageStarted() = 0 ;
	virtual System::Types::TRectF __fastcall GetArtBox() = 0 ;
	virtual System::Types::TRectF __fastcall GetBleedBox() = 0 ;
	virtual System::UnicodeString __fastcall GetCreationDate() = 0 ;
	virtual System::Types::TRectF __fastcall GetCropBox() = 0 ;
	virtual System::Types::TRectF __fastcall GetMediaBox() = 0 ;
	virtual System::UnicodeString __fastcall GetModificationDate() = 0 ;
	virtual System::UnicodeString __fastcall GetProducer() = 0 ;
	virtual System::Types::TRectF __fastcall GetTrimBox() = 0 ;
	virtual bool __fastcall UnlockWithPassword(System::UnicodeString Password) = 0 ;
	virtual int __fastcall GetPageCount() = 0 ;
	virtual int __fastcall GetPageIndex() = 0 ;
	virtual bool __fastcall IsDocumentOpened() = 0 ;
	virtual System::Classes::TMemoryStream* __fastcall EndDocument(bool AOpenInPDFReader = false) = 0 ;
	virtual bool __fastcall GetAllowsCopying() = 0 ;
	virtual bool __fastcall GetAllowsPrinting() = 0 ;
	virtual System::UnicodeString __fastcall GetAuthor() = 0 ;
	virtual System::UnicodeString __fastcall GetCreator() = 0 ;
	virtual System::UnicodeString __fastcall GetFooter() = 0 ;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetFooterAlignment() = 0 ;
	virtual Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetFooterMargins() = 0 ;
	virtual float __fastcall GetFooterSize() = 0 ;
	virtual System::UnicodeString __fastcall GetHeader() = 0 ;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetHeaderAlignment() = 0 ;
	virtual Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetHeaderMargins() = 0 ;
	virtual float __fastcall GetHeaderSize() = 0 ;
	virtual TTMSFNCPDFLibPageNumber __fastcall GetPageNumber() = 0 ;
	virtual System::UnicodeString __fastcall GetPageNumberFormat() = 0 ;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetPageNumberAlignment() = 0 ;
	virtual Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetPageNumberMargins() = 0 ;
	virtual float __fastcall GetPageNumberSize() = 0 ;
	virtual System::Classes::TStrings* __fastcall GetKeywords() = 0 ;
	virtual System::Classes::TStrings* __fastcall GetFontFallBackList() = 0 ;
	virtual System::UnicodeString __fastcall GetOwnerPassword() = 0 ;
	virtual TTMSFNCPDFLibPageOrientation __fastcall GetPageOrientation() = 0 ;
	virtual TTMSFNCPDFLibPageSize __fastcall GetPageSize() = 0 ;
	virtual System::UnicodeString __fastcall GetSubject() = 0 ;
	virtual System::UnicodeString __fastcall GetTitle() = 0 ;
	virtual System::UnicodeString __fastcall GetUserPassword() = 0 ;
	virtual bool __fastcall GetEmbedFonts() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetFooterFont() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetHeaderFont() = 0 ;
	virtual Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetPageNumberFont() = 0 ;
	virtual float __fastcall GetPageHeight() = 0 ;
	virtual float __fastcall GetPageWidth() = 0 ;
	virtual Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer() = 0 ;
	__property System::Types::TRectF MediaBox = {read=GetMediaBox, write=SetMediaBox};
	__property System::Types::TRectF TrimBox = {read=GetTrimBox, write=SetTrimBox};
	__property System::Types::TRectF ArtBox = {read=GetArtBox, write=SetArtBox};
	__property System::Types::TRectF BleedBox = {read=GetBleedBox, write=SetBleedBox};
	__property System::Types::TRectF CropBox = {read=GetCropBox, write=SetCropBox};
	__property System::UnicodeString ModificationDate = {read=GetModificationDate};
	__property System::UnicodeString Producer = {read=GetProducer};
	__property System::UnicodeString CreationDate = {read=GetCreationDate};
	__property bool EmbedFonts = {read=GetEmbedFonts, write=SetEmbedFonts};
	__property TTMSFNCPDFLibPageSize PageSize = {read=GetPageSize, write=SetPageSize};
	__property float PageWidth = {read=GetPageWidth, write=SetPageWidth};
	__property float PageHeight = {read=GetPageHeight, write=SetPageHeight};
	__property TTMSFNCPDFLibPageOrientation PageOrientation = {read=GetPageOrientation, write=SetPageOrientation};
	__property System::UnicodeString Author = {read=GetAuthor, write=SetAuthor};
	__property System::UnicodeString Creator = {read=GetCreator, write=SetCreator};
	__property TTMSFNCPDFLibPageNumber PageNumber = {read=GetPageNumber, write=SetPageNumber};
	__property System::UnicodeString PageNumberFormat = {read=GetPageNumberFormat, write=SetPageNumberFormat};
	__property float PageNumberSize = {read=GetPageNumberSize, write=SetPageNumberSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PageNumberMargins = {read=GetPageNumberMargins, write=SetPageNumberMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign PageNumberAlignment = {read=GetPageNumberAlignment, write=SetPageNumberAlignment};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* PageNumberFont = {read=GetPageNumberFont, write=SetPageNumberFont};
	__property System::UnicodeString Header = {read=GetHeader, write=SetHeader};
	__property float HeaderSize = {read=GetHeaderSize, write=SetHeaderSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* HeaderMargins = {read=GetHeaderMargins, write=SetHeaderMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign HeaderAlignment = {read=GetHeaderAlignment, write=SetHeaderAlignment};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* HeaderFont = {read=GetHeaderFont, write=SetHeaderFont};
	__property System::UnicodeString Footer = {read=GetFooter, write=SetFooter};
	__property float FooterSize = {read=GetFooterSize, write=SetFooterSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* FooterMargins = {read=GetFooterMargins, write=SetFooterMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FooterAlignment = {read=GetFooterAlignment, write=SetFooterAlignment};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FooterFont = {read=GetFooterFont, write=SetFooterFont};
	__property System::UnicodeString Title = {read=GetTitle, write=SetTitle};
	__property System::UnicodeString OwnerPassword = {read=GetOwnerPassword, write=SetOwnerPassword};
	__property System::UnicodeString UserPassword = {read=GetUserPassword, write=SetUserPassword};
	__property bool AllowsPrinting = {read=GetAllowsPrinting, write=SetAllowsPrinting};
	__property bool AllowsCopying = {read=GetAllowsCopying, write=SetAllowsCopying};
	__property System::UnicodeString Subject = {read=GetSubject, write=SetSubject};
	__property System::Classes::TStrings* Keywords = {read=GetKeywords, write=SetKeywords};
	__property System::Classes::TStrings* FontFallBackList = {read=GetFontFallBackList, write=SetFontFallBackList};
	__property TTMSFNCPDFLibBeforeDrawHeaderEvent OnBeforeDrawHeader = {read=GetOnBeforeDrawHeader, write=SetOnBeforeDrawHeader};
	__property TTMSFNCPDFLibAfterDrawHeaderEvent OnAfterDrawHeader = {read=GetOnAfterDrawHeader, write=SetOnAfterDrawHeader};
	__property TTMSFNCPDFLibBeforeDrawPageNumberEvent OnBeforeDrawPageNumber = {read=GetOnBeforeDrawPageNumber, write=SetOnBeforeDrawPageNumber};
	__property TTMSFNCPDFLibAfterDrawPageNumberEvent OnAfterDrawPageNumber = {read=GetOnAfterDrawPageNumber, write=SetOnAfterDrawPageNumber};
	__property TTMSFNCPDFLibBeforeDrawFooterEvent OnBeforeDrawFooter = {read=GetOnBeforeDrawFooter, write=SetOnBeforeDrawFooter};
	__property TTMSFNCPDFLibAfterDrawFooterEvent OnAfterDrawFooter = {read=GetOnAfterDrawFooter, write=SetOnAfterDrawFooter};
	__property TTMSFNCPDFLibNewPageStartedEvent OnNewPageStarted = {read=GetOnNewPageStarted, write=SetOnNewPageStarted};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
	__property Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib Graphics = {read=GetPDFGraphicsLib, write=SetPDFGraphicsLib};
	__property Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsExLib GraphicsEx = {read=GetPDFGraphicsExLib, write=SetPDFGraphicsExLib};
	__property Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFInitializationLib Initialization = {read=GetPDFInitializationLib, write=SetPDFInitializationLib};
	__property TTMSFNCPDFLibStandard PDFStandard = {read=GetPDFStandard, write=SetPDFStandard};
};

__interface  INTERFACE_UUID("{017EC71B-91BA-4A92-B3B2-67724061A21F}") ITMSFNCPDFLibService  : public System::IInterface 
{
	virtual _di_ITMSFNCCustomPDFLib __fastcall CreatePDFLib() = 0 ;
};

__interface  INTERFACE_UUID("{7434C5E1-A00F-4592-B34F-46756818C122}") ITMSFNCPDFLibGeneralService  : public System::IInterface 
{
	virtual _di_ITMSFNCCustomPDFLib __fastcall CreatePDFLib() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomPDFLib : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	_di_ITMSFNCCustomPDFLib FPDFLib;
	Fmx::Tmsfncpdfgraphicslib::TTMSFNCPDFGraphicsLib* FPDFGraphicsLib;
	void __fastcall SetPageSize(const TTMSFNCPDFLibPageSize Value);
	void __fastcall SetPageOrientation(const TTMSFNCPDFLibPageOrientation Value);
	void __fastcall SetHeaderMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetFooterMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetArtBox(const System::Types::TRectF &Value);
	void __fastcall SetBleedBox(const System::Types::TRectF &Value);
	void __fastcall SetCropBox(const System::Types::TRectF &Value);
	void __fastcall SetMediaBox(const System::Types::TRectF &Value);
	void __fastcall SetTrimBox(const System::Types::TRectF &Value);
	void __fastcall SetAllowsCopying(const bool Value);
	void __fastcall SetAllowsPrinting(const bool Value);
	void __fastcall SetFooter(const System::UnicodeString Value);
	void __fastcall SetFooterAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetFooterSize(const float Value);
	void __fastcall SetHeader(const System::UnicodeString Value);
	void __fastcall SetHeaderAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetHeaderSize(const float Value);
	void __fastcall SetKeywords(System::Classes::TStrings* const Value);
	void __fastcall SetOwnerPassword(const System::UnicodeString Value);
	void __fastcall SetSubject(const System::UnicodeString Value);
	void __fastcall SetTitle(const System::UnicodeString Value);
	void __fastcall SetUserPassword(const System::UnicodeString Value);
	void __fastcall SetAuthor(const System::UnicodeString Value);
	void __fastcall SetCreator(const System::UnicodeString Value);
	void __fastcall SetEmbedFonts(const bool Value);
	void __fastcall SetFontFallBackList(System::Classes::TStrings* const Value);
	void __fastcall SetFooterFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetHeaderFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetPageHeight(const float Value);
	void __fastcall SetPageWidth(const float Value);
	void __fastcall SetOnAfterDrawFooter(const TTMSFNCPDFLibAfterDrawFooterEvent Value);
	void __fastcall SetOnAfterDrawHeader(const TTMSFNCPDFLibAfterDrawHeaderEvent Value);
	void __fastcall SetOnAfterDrawPageNumber(const TTMSFNCPDFLibAfterDrawPageNumberEvent Value);
	void __fastcall SetOnBeforeDrawFooter(const TTMSFNCPDFLibBeforeDrawFooterEvent Value);
	void __fastcall SetOnBeforeDrawHeader(const TTMSFNCPDFLibBeforeDrawHeaderEvent Value);
	void __fastcall SetOnBeforeDrawPageNumber(const TTMSFNCPDFLibBeforeDrawPageNumberEvent Value);
	void __fastcall SetOnNewPageStarted(const TTMSFNCPDFLibNewPageStartedEvent Value);
	int __fastcall GetVersionNr();
	System::Types::TRectF __fastcall GetArtBox();
	System::Types::TRectF __fastcall GetBleedBox();
	System::UnicodeString __fastcall GetCreationDate();
	System::Types::TRectF __fastcall GetCropBox();
	System::Types::TRectF __fastcall GetMediaBox();
	System::UnicodeString __fastcall GetModificationDate();
	System::UnicodeString __fastcall GetProducer();
	System::Types::TRectF __fastcall GetTrimBox();
	bool __fastcall GetAllowsCopying();
	bool __fastcall GetAllowsPrinting();
	System::UnicodeString __fastcall GetAuthor();
	System::UnicodeString __fastcall GetCreator();
	System::UnicodeString __fastcall GetFooter();
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetFooterAlignment();
	Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetFooterMargins();
	float __fastcall GetFooterSize();
	System::UnicodeString __fastcall GetHeader();
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetHeaderAlignment();
	Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetHeaderMargins();
	float __fastcall GetHeaderSize();
	System::Classes::TStrings* __fastcall GetKeywords();
	System::UnicodeString __fastcall GetOwnerPassword();
	TTMSFNCPDFLibPageOrientation __fastcall GetPageOrientation();
	TTMSFNCPDFLibPageSize __fastcall GetPageSize();
	System::UnicodeString __fastcall GetSubject();
	System::UnicodeString __fastcall GetTitle();
	System::UnicodeString __fastcall GetUserPassword();
	bool __fastcall GetEmbedFonts();
	System::Classes::TStrings* __fastcall GetFontFallBackList();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetFooterFont();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetHeaderFont();
	float __fastcall GetPageHeight();
	float __fastcall GetPageWidth();
	TTMSFNCPDFLibAfterDrawFooterEvent __fastcall GetOnAfterDrawFooter();
	TTMSFNCPDFLibAfterDrawHeaderEvent __fastcall GetOnAfterDrawHeader();
	TTMSFNCPDFLibAfterDrawPageNumberEvent __fastcall GetOnAfterDrawPageNumber();
	TTMSFNCPDFLibBeforeDrawFooterEvent __fastcall GetOnBeforeDrawFooter();
	TTMSFNCPDFLibBeforeDrawHeaderEvent __fastcall GetOnBeforeDrawHeader();
	TTMSFNCPDFLibBeforeDrawPageNumberEvent __fastcall GetOnBeforeDrawPageNumber();
	TTMSFNCPDFLibNewPageStartedEvent __fastcall GetOnNewPageStarted();
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer();
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	TTMSFNCPDFLibPageNumber __fastcall GetPageNumber();
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign __fastcall GetPageNumberAlignment();
	Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* __fastcall GetPageNumberFont();
	System::UnicodeString __fastcall GetPageNumberFormat();
	Fmx::Tmsfnctypes::TTMSFNCMargins* __fastcall GetPageNumberMargins();
	float __fastcall GetPageNumberSize();
	void __fastcall SetPageNumber(const TTMSFNCPDFLibPageNumber Value);
	void __fastcall SetPageNumberAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetPageNumberFont(Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetPageNumberFormat(const System::UnicodeString Value);
	void __fastcall SetPageNumberMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetPageNumberSize(const float Value);
	TTMSFNCPDFLibStandard __fastcall GetPDFStandard();
	void __fastcall SetPDFStandard(const TTMSFNCPDFLibStandard Value);
	
protected:
	virtual void __fastcall Initialize(bool AUseNativePDFImplementation);
	void __fastcall DoNotifyNewPage(System::TObject* Sender);
	void __fastcall SaveDocumentFromStream(System::Classes::TMemoryStream* FileStream, System::UnicodeString FileName);
	void __fastcall GetDocumentInfo();
	void __fastcall InsertPage(int PageIndex);
	void __fastcall OpenDocument(System::UnicodeString FileName)/* overload */;
	void __fastcall OpenDocument(System::Classes::TMemoryStream* FileStream)/* overload */;
	void __fastcall GetPageInfo(int PageIndex);
	void __fastcall CloseDocument();
	void __fastcall DrawPage(int PageIndex);
	void __fastcall DrawHeader();
	void __fastcall DrawFooter();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual NativeUInt __fastcall GetInstance();
	Fmx::Tmsfncpdfrichtextlib::_di_ITMSFNCCustomPDFRichTextLib __fastcall RichText();
	bool __fastcall UnlockWithPassword(System::UnicodeString Password);
	bool __fastcall IsDocumentOpened();
	Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFInitializationLib __fastcall Initialization();
	_di_ITMSFNCCustomPDFLib __fastcall PDFLib();
	virtual System::UnicodeString __fastcall GetVersion();
	__property System::UnicodeString ModificationDate = {read=GetModificationDate};
	__property System::UnicodeString Producer = {read=GetProducer};
	__property System::UnicodeString CreationDate = {read=GetCreationDate};
	__property System::UnicodeString OwnerPassword = {read=GetOwnerPassword, write=SetOwnerPassword};
	__property System::UnicodeString UserPassword = {read=GetUserPassword, write=SetUserPassword};
	__property bool AllowsPrinting = {read=GetAllowsPrinting, write=SetAllowsPrinting, default=1};
	__property bool AllowsCopying = {read=GetAllowsCopying, write=SetAllowsCopying, default=1};
	__property TTMSFNCPDFLibStandard PDFStandard = {read=GetPDFStandard, write=SetPDFStandard, default=0};
	
public:
	__fastcall virtual TTMSFNCCustomPDFLib()/* overload */;
	__fastcall virtual TTMSFNCCustomPDFLib(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCCustomPDFLib();
	void __fastcall BeginDocument(System::UnicodeString FileName = System::UnicodeString());
	void __fastcall NewPage();
	Fmx::Tmsfncpdfgraphicslib::_di_ITMSFNCCustomPDFGraphicsLib __fastcall Graphics();
	int __fastcall GetPageCount();
	int __fastcall GetPageIndex();
	System::Classes::TMemoryStream* __fastcall EndDocument(bool AOpenInPDFReader = false);
	System::Types::TRectF __fastcall GetHeaderRect();
	System::Types::TRectF __fastcall GetFooterRect();
	System::Types::TRectF __fastcall GetPageNumberRect();
	__property System::Types::TRectF MediaBox = {read=GetMediaBox, write=SetMediaBox};
	__property System::Types::TRectF TrimBox = {read=GetTrimBox, write=SetTrimBox};
	__property System::Types::TRectF ArtBox = {read=GetArtBox, write=SetArtBox};
	__property System::Types::TRectF BleedBox = {read=GetBleedBox, write=SetBleedBox};
	__property System::Types::TRectF CropBox = {read=GetCropBox, write=SetCropBox};
	__property bool EmbedFonts = {read=GetEmbedFonts, write=SetEmbedFonts, nodefault};
	__property System::UnicodeString Version = {read=GetVersion};
	__property float PageWidth = {read=GetPageWidth, write=SetPageWidth};
	__property float PageHeight = {read=GetPageHeight, write=SetPageHeight};
	__property TTMSFNCPDFLibPageSize PageSize = {read=GetPageSize, write=SetPageSize, default=34};
	__property TTMSFNCPDFLibPageOrientation PageOrientation = {read=GetPageOrientation, write=SetPageOrientation, default=0};
	__property System::UnicodeString Author = {read=GetAuthor, write=SetAuthor};
	__property System::UnicodeString Creator = {read=GetCreator, write=SetCreator};
	__property System::UnicodeString Header = {read=GetHeader, write=SetHeader};
	__property float HeaderSize = {read=GetHeaderSize, write=SetHeaderSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* HeaderMargins = {read=GetHeaderMargins, write=SetHeaderMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign HeaderAlignment = {read=GetHeaderAlignment, write=SetHeaderAlignment, default=0};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* HeaderFont = {read=GetHeaderFont, write=SetHeaderFont};
	__property TTMSFNCPDFLibPageNumber PageNumber = {read=GetPageNumber, write=SetPageNumber, nodefault};
	__property System::UnicodeString PageNumberFormat = {read=GetPageNumberFormat, write=SetPageNumberFormat};
	__property float PageNumberSize = {read=GetPageNumberSize, write=SetPageNumberSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PageNumberMargins = {read=GetPageNumberMargins, write=SetPageNumberMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign PageNumberAlignment = {read=GetPageNumberAlignment, write=SetPageNumberAlignment, default=0};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* PageNumberFont = {read=GetPageNumberFont, write=SetPageNumberFont};
	__property System::UnicodeString Footer = {read=GetFooter, write=SetFooter};
	__property float FooterSize = {read=GetFooterSize, write=SetFooterSize};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* FooterMargins = {read=GetFooterMargins, write=SetFooterMargins};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FooterAlignment = {read=GetFooterAlignment, write=SetFooterAlignment, default=0};
	__property Fmx::Tmsfncpdfcorelibbase::TTMSFNCPDFGraphicsLibFont* FooterFont = {read=GetFooterFont, write=SetFooterFont};
	__property System::UnicodeString Title = {read=GetTitle, write=SetTitle};
	__property System::UnicodeString Subject = {read=GetSubject, write=SetSubject};
	__property System::Classes::TStrings* Keywords = {read=GetKeywords, write=SetKeywords};
	__property System::Classes::TStrings* FontFallBackList = {read=GetFontFallBackList, write=SetFontFallBackList};
	__property TTMSFNCPDFLibBeforeDrawHeaderEvent OnBeforeDrawHeader = {read=GetOnBeforeDrawHeader, write=SetOnBeforeDrawHeader};
	__property TTMSFNCPDFLibAfterDrawHeaderEvent OnAfterDrawHeader = {read=GetOnAfterDrawHeader, write=SetOnAfterDrawHeader};
	__property TTMSFNCPDFLibBeforeDrawPageNumberEvent OnBeforeDrawPageNumber = {read=GetOnBeforeDrawPageNumber, write=SetOnBeforeDrawPageNumber};
	__property TTMSFNCPDFLibAfterDrawPageNumberEvent OnAfterDrawPageNumber = {read=GetOnAfterDrawPageNumber, write=SetOnAfterDrawPageNumber};
	__property TTMSFNCPDFLibBeforeDrawFooterEvent OnBeforeDrawFooter = {read=GetOnBeforeDrawFooter, write=SetOnBeforeDrawFooter};
	__property TTMSFNCPDFLibAfterDrawFooterEvent OnAfterDrawFooter = {read=GetOnAfterDrawFooter, write=SetOnAfterDrawFooter};
	__property TTMSFNCPDFLibNewPageStartedEvent OnNewPageStarted = {read=GetOnNewPageStarted, write=SetOnNewPageStarted};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFLibList : public System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib>
{
	typedef System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFLib_ITMSFNCCustomPDFLib>.Create */ inline __fastcall TTMSFNCPDFLibList()/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFLib_ITMSFNCCustomPDFLib>.Create */ inline __fastcall TTMSFNCPDFLibList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<_di_ITMSFNCCustomPDFLib> > AComparer)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFLib_ITMSFNCCustomPDFLib>.Create */ inline __fastcall TTMSFNCPDFLibList(System::Generics::Collections::TEnumerable__1<_di_ITMSFNCCustomPDFLib>* const Collection)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFLib_ITMSFNCCustomPDFLib>.Create */ inline __fastcall TTMSFNCPDFLibList(const _di_ITMSFNCCustomPDFLib *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomPDFLib>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFLib_ITMSFNCCustomPDFLib>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFLibList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFLibFactoryService : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTMSFNCPDFLibList* FPDFLibs;
	
protected:
	virtual _di_ITMSFNCCustomPDFLib __fastcall DoCreatePDFLib() = 0 ;
	_di_ITMSFNCCustomPDFLib __fastcall CreatePDFLib();
	
public:
	__fastcall TTMSFNCPDFLibFactoryService();
	__fastcall virtual ~TTMSFNCPDFLibFactoryService();
private:
	void *__ITMSFNCPDFLibGeneralService;	// ITMSFNCPDFLibGeneralService 
	void *__ITMSFNCPDFLibService;	// ITMSFNCPDFLibService 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {7434C5E1-A00F-4592-B34F-46756818C122}
	operator _di_ITMSFNCPDFLibGeneralService()
	{
		_di_ITMSFNCPDFLibGeneralService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCPDFLibGeneralService*(void) { return (ITMSFNCPDFLibGeneralService*)&__ITMSFNCPDFLibGeneralService; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {017EC71B-91BA-4A92-B3B2-67724061A21F}
	operator _di_ITMSFNCPDFLibService()
	{
		_di_ITMSFNCPDFLibService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCPDFLibService*(void) { return (ITMSFNCPDFLibService*)&__ITMSFNCPDFLibService; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFLib : public TTMSFNCCustomPDFLib
{
	typedef TTMSFNCCustomPDFLib inherited;
	
public:
	__property Version = {default=0};
public:
	/* TTMSFNCCustomPDFLib.Create */ inline __fastcall virtual TTMSFNCPDFLib()/* overload */ : TTMSFNCCustomPDFLib() { }
	/* TTMSFNCCustomPDFLib.Create */ inline __fastcall virtual TTMSFNCPDFLib(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomPDFLib(AOwner) { }
	/* TTMSFNCCustomPDFLib.Destroy */ inline __fastcall virtual ~TTMSFNCPDFLib() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define TTMSFNCPDFLibDocURL u"https://download.tmssoftware.com/doc/tmsfnccore/components"\
	u"/pdflibrary/"
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x2);
static constexpr System::Int8 BLD_VER = System::Int8(0x4);
extern DELPHI_PACKAGE System::Types::TRectF DefaultMediaBox;
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCPDFLibNoPages;
#define Fmx_Tmsfncpdflib_sTMSFNCPDFLibNoPages System::LoadResourceString(&Fmx::Tmsfncpdflib::_sTMSFNCPDFLibNoPages)
extern DELPHI_PACKAGE float __fastcall MillimeterToPixel(float AMillimeter, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE float __fastcall PixelToMillimeter(float APixel, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE float __fastcall InchToPixel(float AInch, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE float __fastcall PixelToInch(float APixel, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall MillimeterPoint(float AX, float AY, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall MillimeterRect(float AX, float AY, float AWidth, float AHeight, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall InchPoint(float AX, float AY, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall InchRect(float AX, float AY, float AWidth, float AHeight, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectToMillimeterRect(const System::Types::TRectF &ARect, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectToInchRect(const System::Types::TRectF &ARect, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall PointToMillimeterPoint(const System::Types::TPointF &APoint, float ADPI = 7.200000E+01f);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall PointToInchPoint(const System::Types::TPointF &APoint, float ADPI = 7.200000E+01f);
}	/* namespace Tmsfncpdflib */
}	/* namespace Fmx */
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
#endif	// Fmx_TmsfncpdflibHPP
