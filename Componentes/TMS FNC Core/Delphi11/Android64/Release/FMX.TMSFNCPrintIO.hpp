// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPrintIO.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncprintioHPP
#define Fmx_TmsfncprintioHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCPrinters.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncprintio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCPrintIOOptions;
class DELPHICLASS TTMSFNCCustomPrintIO;
class DELPHICLASS TTMSFNCGraphicsPrintIO;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCPrintPageNumber : unsigned char { pnNone, pnHeader, pnFooter };

class PASCALIMPLEMENTATION TTMSFNCPrintIOOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FDefaultFont;
	System::UnicodeString FDevice;
	System::UnicodeString FFooter;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFooterFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FFooterHorizontalAlignment;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FFooterMargins;
	float FFooterSize;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FFooterVerticalAlignment;
	System::UnicodeString FHeader;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FHeaderFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FHeaderHorizontalAlignment;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FHeaderMargins;
	float FHeaderSize;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FHeaderVerticalAlignment;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FMargins;
	TTMSFNCPrintPageNumber FPageNumber;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FPageNumberFont;
	System::UnicodeString FPageNumberFormat;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FPageNumberHorizontalAlignment;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FPageNumberMargins;
	float FPageNumberSize;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FPageNumberVerticalAlignment;
	int __fastcall GetDPI();
	int __fastcall GetPageHeight();
	int __fastcall GetPageIndex();
	System::Uitypes::TPrinterOrientation __fastcall GetPageOrientation();
	int __fastcall GetPageWidth();
	bool __fastcall IsFooterSizeStored();
	bool __fastcall IsHeaderSizeStored();
	bool __fastcall IsPageNumberSizeStored();
	void __fastcall SetDefaultFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetFooterFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetFooterHorizontalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetFooterMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetFooterSize(const float Value);
	void __fastcall SetFooterVerticalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetHeaderFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetHeaderHorizontalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetHeaderMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetHeaderSize(const float Value);
	void __fastcall SetHeaderVerticalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetPageNumber(const TTMSFNCPrintPageNumber Value);
	void __fastcall SetPageNumberFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetPageNumberFormat(const System::UnicodeString Value);
	void __fastcall SetPageNumberHorizontalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetPageNumberMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetPageNumberSize(const float Value);
	void __fastcall SetPageNumberVerticalAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetPageOrientation(const System::Uitypes::TPrinterOrientation Value);
	Fmx::Tmsfncprinters::TPrintSize __fastcall GetPageSize();
	void __fastcall SetPageSize(const Fmx::Tmsfncprinters::TPrintSize Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCPrintIOOptions();
	__fastcall virtual ~TTMSFNCPrintIOOptions();
	__property int DPI = {read=GetDPI, nodefault};
	__property int PageHeight = {read=GetPageHeight, nodefault};
	__property int PageIndex = {read=GetPageIndex, nodefault};
	__property int PageWidth = {read=GetPageWidth, nodefault};
	
__published:
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* DefaultFont = {read=FDefaultFont, write=SetDefaultFont};
	__property System::UnicodeString Device = {read=FDevice};
	__property System::UnicodeString Footer = {read=FFooter, write=FFooter};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FooterFont = {read=FFooterFont, write=SetFooterFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FooterHorizontalAlignment = {read=FFooterHorizontalAlignment, write=SetFooterHorizontalAlignment, default=0};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* FooterMargins = {read=FFooterMargins, write=SetFooterMargins};
	__property float FooterSize = {read=FFooterSize, write=SetFooterSize, stored=IsFooterSizeStored};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FooterVerticalAlignment = {read=FFooterVerticalAlignment, write=SetFooterVerticalAlignment, default=0};
	__property System::UnicodeString Header = {read=FHeader, write=FHeader};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* HeaderFont = {read=FHeaderFont, write=SetHeaderFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign HeaderHorizontalAlignment = {read=FHeaderHorizontalAlignment, write=SetHeaderHorizontalAlignment, default=0};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* HeaderMargins = {read=FHeaderMargins, write=SetHeaderMargins};
	__property float HeaderSize = {read=FHeaderSize, write=SetHeaderSize, stored=IsHeaderSizeStored};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign HeaderVerticalAlignment = {read=FHeaderVerticalAlignment, write=SetHeaderVerticalAlignment, default=0};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* Margins = {read=FMargins, write=SetMargins};
	__property TTMSFNCPrintPageNumber PageNumber = {read=FPageNumber, write=SetPageNumber, default=0};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* PageNumberFont = {read=FPageNumberFont, write=SetPageNumberFont};
	__property System::UnicodeString PageNumberFormat = {read=FPageNumberFormat, write=SetPageNumberFormat};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign PageNumberHorizontalAlignment = {read=FPageNumberHorizontalAlignment, write=SetPageNumberHorizontalAlignment, default=2};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PageNumberMargins = {read=FPageNumberMargins, write=SetPageNumberMargins};
	__property float PageNumberSize = {read=FPageNumberSize, write=SetPageNumberSize, stored=IsPageNumberSizeStored};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign PageNumberVerticalAlignment = {read=FPageNumberVerticalAlignment, write=SetPageNumberVerticalAlignment, default=0};
	__property System::Uitypes::TPrinterOrientation PageOrientation = {read=GetPageOrientation, write=SetPageOrientation, default=0};
	__property Fmx::Tmsfncprinters::TPrintSize PageSize = {read=GetPageSize, write=SetPageSize, nodefault};
};


typedef System::Classes::TComponent TTMSFNCPrintIOExportObject;

typedef void __fastcall (__closure *TTMSFNCPrintIOBeforeDrawHeaderEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &AHeader, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPrintIOAfterDrawHeaderEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHeader);

typedef void __fastcall (__closure *TTMSFNCPrintIOBeforeDrawFooterEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &AFooter, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPrintIOAfterDrawFooterEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AFooter);

typedef void __fastcall (__closure *TTMSFNCPrintIOBeforeDrawContentEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPrintIOAfterDrawContentEvent)(System::TObject* Sender, System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics);

typedef void __fastcall (__closure *TTMSFNCPrintBeforeDrawPageNumberEvent)(System::TObject* Sender, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &APageNumber, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCPrintAfterDrawPageNumberEvent)(System::TObject* Sender, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString APageNumber);

typedef void __fastcall (__closure *TTMSFNCPrintIOAfterDrawEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics);

typedef void __fastcall (__closure *TTMSFNCPrintIOExportRectEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);

typedef void __fastcall (__closure *TTMSFNCPrintIOCanCreateNewPageEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject, bool &ACanCreate);

typedef System::DynamicArray<System::Classes::TComponent*> TTMSFNCPrintIOExportObjectArray;

typedef System::DynamicArray<System::Types::TRectF> TTMSFNCPrintIOExportRectArray;

class PASCALIMPLEMENTATION TTMSFNCCustomPrintIO : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	System::Classes::TComponent* FActiveExportObject;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	TTMSFNCPrintIOCanCreateNewPageEvent FOnCanCreateNewPage;
	System::Classes::TComponent* FExportObject;
	TTMSFNCPrintIOExportObjectArray FExportObjects;
	TTMSFNCPrintIOExportRectArray FExportRects;
	bool FNewPageStarted;
	TTMSFNCPrintIOOptions* FOptions;
	TTMSFNCPrintIOAfterDrawEvent FOnAfterDraw;
	TTMSFNCPrintIOAfterDrawContentEvent FOnAfterDrawContent;
	TTMSFNCPrintIOAfterDrawFooterEvent FOnAfterDrawFooter;
	TTMSFNCPrintIOAfterDrawHeaderEvent FOnAfterDrawHeader;
	TTMSFNCPrintAfterDrawPageNumberEvent FOnAfterDrawPageNumber;
	TTMSFNCPrintIOBeforeDrawContentEvent FOnBeforeDrawContent;
	TTMSFNCPrintIOBeforeDrawFooterEvent FOnBeforeDrawFooter;
	TTMSFNCPrintIOBeforeDrawHeaderEvent FOnBeforeDrawHeader;
	TTMSFNCPrintBeforeDrawPageNumberEvent FOnBeforeDrawPageNumber;
	TTMSFNCPrintIOExportRectEvent FOnGetExportRect;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer();
	void __fastcall PrintContent();
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	void __fastcall SetOptions(TTMSFNCPrintIOOptions* const Value);
	
protected:
	void __fastcall CreateNewPage(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics);
	virtual TTMSFNCPrintIOOptions* __fastcall CreateOptions();
	virtual void __fastcall DoAfterDraw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics);
	virtual void __fastcall DoAfterDrawContent(System::Classes::TComponent* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics);
	void __fastcall DoAfterDrawFooter(System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AFooter);
	void __fastcall DoAfterDrawHeader(System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHeader);
	virtual void __fastcall DoAfterDrawPageNumber(int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString APageNumber);
	virtual void __fastcall DoBeforeDrawContent(System::Classes::TComponent* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, bool &ADefaultDraw);
	void __fastcall DoBeforeDrawFooter(System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &AFooter, bool &ADefaultDraw);
	void __fastcall DoBeforeDrawHeader(System::TObject* AExportObject, int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &AHeader, bool &ADefaultDraw);
	virtual void __fastcall DoBeforeDrawPageNumber(int APageIndex, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString &APageNumber, bool &ADefaultDraw);
	virtual void __fastcall DoCanCreateNewPage(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject, bool &ACanCreate);
	virtual void __fastcall DoGetExportRect(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);
	virtual void __fastcall DoPrintExport(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	virtual void __fastcall DrawFooterHeader(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics);
	virtual void __fastcall DrawPageNumber(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics);
	virtual void __fastcall EndPage(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject);
	virtual System::Types::TRectF __fastcall GetContentRect(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics);
	System::Types::TRectF __fastcall GetHeaderRect();
	System::Types::TRectF __fastcall GetFooterRect();
	virtual NativeUInt __fastcall GetInstance();
	System::Types::TRectF __fastcall GetPageNumberRect();
	virtual System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall NewPage(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, System::Classes::TComponent* const AExportObject);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
	__property System::Classes::TComponent* ExportObject = {read=FExportObject, write=FExportObject};
	__property TTMSFNCPrintIOOptions* Options = {read=FOptions, write=SetOptions};
	__property System::UnicodeString Version = {read=GetVersion};
	__property TTMSFNCPrintIOAfterDrawEvent OnAfterDraw = {read=FOnAfterDraw, write=FOnAfterDraw};
	__property TTMSFNCPrintIOAfterDrawContentEvent OnAfterDrawContent = {read=FOnAfterDrawContent, write=FOnAfterDrawContent};
	__property TTMSFNCPrintIOAfterDrawFooterEvent OnAfterDrawFooter = {read=FOnAfterDrawFooter, write=FOnAfterDrawFooter};
	__property TTMSFNCPrintIOAfterDrawHeaderEvent OnAfterDrawHeader = {read=FOnAfterDrawHeader, write=FOnAfterDrawHeader};
	__property TTMSFNCPrintAfterDrawPageNumberEvent OnAfterDrawPageNumber = {read=FOnAfterDrawPageNumber, write=FOnAfterDrawPageNumber};
	__property TTMSFNCPrintIOBeforeDrawContentEvent OnBeforeDrawContent = {read=FOnBeforeDrawContent, write=FOnBeforeDrawContent};
	__property TTMSFNCPrintIOBeforeDrawFooterEvent OnBeforeDrawFooter = {read=FOnBeforeDrawFooter, write=FOnBeforeDrawFooter};
	__property TTMSFNCPrintIOBeforeDrawHeaderEvent OnBeforeDrawHeader = {read=FOnBeforeDrawHeader, write=FOnBeforeDrawHeader};
	__property TTMSFNCPrintBeforeDrawPageNumberEvent OnBeforeDrawPageNumber = {read=FOnBeforeDrawPageNumber, write=FOnBeforeDrawPageNumber};
	__property TTMSFNCPrintIOCanCreateNewPageEvent OnCanCreateNewPage = {read=FOnCanCreateNewPage, write=FOnCanCreateNewPage};
	__property TTMSFNCPrintIOExportRectEvent OnGetExportRect = {read=FOnGetExportRect, write=FOnGetExportRect};
	
public:
	__fastcall virtual TTMSFNCCustomPrintIO(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomPrintIO();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Print()/* overload */;
	void __fastcall Print(System::Classes::TComponent* AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	void __fastcall Print(TTMSFNCPrintIOExportObjectArray AExportObjects)/* overload */;
	void __fastcall Print(TTMSFNCPrintIOExportObjectArray AExportObjects, TTMSFNCPrintIOExportRectArray AExportRects)/* overload */;
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


class PASCALIMPLEMENTATION TTMSFNCGraphicsPrintIO : public TTMSFNCCustomPrintIO
{
	typedef TTMSFNCCustomPrintIO inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetDocURL();
	
__published:
	__property BitmapContainer;
	__property ExportObject;
	__property Options;
	__property Version = {default=0};
	__property OnAfterDraw;
	__property OnAfterDrawContent;
	__property OnAfterDrawFooter;
	__property OnAfterDrawHeader;
	__property OnAfterDrawPageNumber;
	__property OnBeforeDrawContent;
	__property OnBeforeDrawFooter;
	__property OnBeforeDrawHeader;
	__property OnBeforeDrawPageNumber;
	__property OnCanCreateNewPage;
	__property OnGetExportRect;
public:
	/* TTMSFNCCustomPrintIO.Create */ inline __fastcall virtual TTMSFNCGraphicsPrintIO(System::Classes::TComponent* AOwner) : TTMSFNCCustomPrintIO(AOwner) { }
	/* TTMSFNCCustomPrintIO.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPrintIO() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 PRINTDPI = System::Int8(0x48);
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncprintio */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPRINTIO)
using namespace Fmx::Tmsfncprintio;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncprintioHPP
