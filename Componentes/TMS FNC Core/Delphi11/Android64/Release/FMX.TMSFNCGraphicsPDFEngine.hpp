// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphicsPDFEngine.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncgraphicspdfengineHPP
#define Fmx_TmsfncgraphicspdfengineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCPDFLib.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCPDFIO.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphicspdfengine
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsPDFEngine;
class DELPHICLASS TTMSFNCGraphicsPDF;
class DELPHICLASS TTMSFNCCustomGraphicsPDFIO;
class DELPHICLASS TTMSFNCGraphicsPDFIO;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCGraphicsPDFEngine : public Fmx::Tmsfncgraphics::TTMSFNCGraphics
{
	typedef Fmx::Tmsfncgraphics::TTMSFNCGraphics inherited;
	
private:
	Fmx::Graphics::TBitmap* FCanvas;
	Fmx::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib FPDFLib;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FMatrix;
	
protected:
	virtual System::Types::TRectF __fastcall InternalCalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::UnicodeString __fastcall InternalDrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	
public:
	__fastcall virtual TTMSFNCGraphicsPDFEngine(const Fmx::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib)/* overload */;
	__fastcall virtual TTMSFNCGraphicsPDFEngine(Fmx::Tmsfncpdflib::TTMSFNCCustomPDFLib* const APDFLib)/* overload */;
	__fastcall virtual ~TTMSFNCGraphicsPDFEngine();
	void __fastcall DrawPDFPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, const float Flatness = 2.500000E-01f);
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* __fastcall SaveState(bool ACanvasOnly = false);
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle);
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	virtual void __fastcall SetMatrix(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall ResetTextAngle(float AAngle);
	virtual void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill);
	virtual void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke);
	virtual void __fastcall RestoreState(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState, bool ACanvasOnly = false);
	virtual void __fastcall SetFillKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind);
	virtual void __fastcall SetFillColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetFontColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetFontName(System::UnicodeString AName);
	virtual void __fastcall SetFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont);
	virtual void __fastcall SetFontSize(int ASize);
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle);
	virtual void __fastcall SetStrokeKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	virtual void __fastcall SetStrokeColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetStrokeWidth(float AWidth);
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect)/* overload */;
	virtual void __fastcall DrawLine(const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall DrawPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall DrawBitmap(double ALeft, double ATop, double ARight, double ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawArc(const System::Types::TPointF &Center, const System::Types::TPointF &Radius, float StartAngle, float SweepAngle)/* overload */;
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(Fmx::Graphics::TCanvas* ACanvas) : Fmx::Tmsfncgraphics::TTMSFNCGraphics(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(int AWidth, int AHeight, bool ANative, bool AHighDPI) : Fmx::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(int AWidth, int AHeight) : Fmx::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  ClipRect(const System::Types::TRect &ARect){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::ClipRect(ARect); }
	inline void __fastcall  DrawLine(const System::Types::TPoint &AFromPoint, const System::Types::TPoint &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawLine(AFromPoint, AToPoint, AModifyPointModeFrom, AModifyPointModeTo); }
	inline void __fastcall  DrawEllipse(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ALeft, ATop, ARight, ABottom, AModifyRectMode); }
	inline void __fastcall  DrawEllipse(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ARect, AModifyRectMode); }
	inline void __fastcall  DrawEllipse(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, ASides, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ALeft, ATop, ARight, ABottom, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ALeft, ATop, ARight, ABottom, ASides, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, ASides, AModifyRectMode); }
	inline void __fastcall  DrawBitmap(const System::Types::TRectF &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ARect, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(int ALeft, int ATop, int ARight, int ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ARight, ABottom, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(const System::Types::TRect &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ARect, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(int ALeft, int ATop, Fmx::Graphics::TBitmap* ABitmap){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ABitmap); }
	inline void __fastcall  DrawBitmap(float ALeft, float ATop, Fmx::Graphics::TBitmap* ABitmap){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ABitmap); }
	inline void __fastcall  DrawArc(const System::Types::TPoint &Center, const System::Types::TPoint &Radius, int StartAngle, int SweepAngle){ Fmx::Tmsfncgraphics::TTMSFNCGraphics::DrawArc(Center, Radius, StartAngle, SweepAngle); }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsPDF : public TTMSFNCGraphicsPDFEngine
{
	typedef TTMSFNCGraphicsPDFEngine inherited;
	
public:
	/* TTMSFNCGraphicsPDFEngine.Create */ inline __fastcall virtual TTMSFNCGraphicsPDF(const Fmx::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib)/* overload */ : TTMSFNCGraphicsPDFEngine(APDFLib) { }
	/* TTMSFNCGraphicsPDFEngine.Create */ inline __fastcall virtual TTMSFNCGraphicsPDF(Fmx::Tmsfncpdflib::TTMSFNCCustomPDFLib* const APDFLib)/* overload */ : TTMSFNCGraphicsPDFEngine(APDFLib) { }
	/* TTMSFNCGraphicsPDFEngine.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPDF() { }
	
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsPDF(Fmx::Graphics::TCanvas* ACanvas) : TTMSFNCGraphicsPDFEngine(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDF(int AWidth, int AHeight, bool ANative, bool AHighDPI) : TTMSFNCGraphicsPDFEngine(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDF(int AWidth, int AHeight) : TTMSFNCGraphicsPDFEngine(AWidth, AHeight) { }
	
};


typedef void __fastcall (__closure *TTMSFNCGraphicsPDFIOExportRectEvent)(System::TObject* Sender, Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);

typedef void __fastcall (__closure *TTMSFNCGraphicsPDFIOCanCreateNewPageEvent)(System::TObject* Sender, Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, bool &ACanCreate);

class PASCALIMPLEMENTATION TTMSFNCCustomGraphicsPDFIO : public Fmx::Tmsfncpdfio::TTMSFNCCustomPDFIO
{
	typedef Fmx::Tmsfncpdfio::TTMSFNCCustomPDFIO inherited;
	
private:
	TTMSFNCGraphicsPDFIOExportRectEvent FOnGetExportRect;
	TTMSFNCGraphicsPDFIOCanCreateNewPageEvent FOnCanCreateNewPage;
	
protected:
	virtual System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall DoPDFExport(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	virtual void __fastcall DoGetExportRect(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);
	virtual void __fastcall DoCanCreateNewPage(Fmx::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, bool &ACanCreate);
	__property System::UnicodeString Version = {read=GetVersion};
	__property TTMSFNCGraphicsPDFIOExportRectEvent OnGetExportRect = {read=FOnGetExportRect, write=FOnGetExportRect};
	__property TTMSFNCGraphicsPDFIOCanCreateNewPageEvent OnCanCreateNewPage = {read=FOnCanCreateNewPage, write=FOnCanCreateNewPage};
public:
	/* TTMSFNCCustomPDFIO.Create */ inline __fastcall virtual TTMSFNCCustomGraphicsPDFIO(System::Classes::TComponent* AOwner) : Fmx::Tmsfncpdfio::TTMSFNCCustomPDFIO(AOwner) { }
	/* TTMSFNCCustomPDFIO.Destroy */ inline __fastcall virtual ~TTMSFNCCustomGraphicsPDFIO() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsPDFIO : public TTMSFNCCustomGraphicsPDFIO
{
	typedef TTMSFNCCustomGraphicsPDFIO inherited;
	
__published:
	__property BitmapContainer;
	__property ExportObject;
	__property Version = {default=0};
	__property Options;
	__property Information;
	__property OnCanCreateNewPage;
	__property OnGetHeader;
	__property OnGetFooter;
	__property OnBeforeDrawHeader;
	__property OnAfterDrawHeader;
	__property OnBeforeDrawFooter;
	__property OnAfterDrawFooter;
	__property OnBeforeDrawContent;
	__property OnAfterDrawContent;
	__property OnGetExportRect;
	__property OnAfterDraw;
public:
	/* TTMSFNCCustomPDFIO.Create */ inline __fastcall virtual TTMSFNCGraphicsPDFIO(System::Classes::TComponent* AOwner) : TTMSFNCCustomGraphicsPDFIO(AOwner) { }
	/* TTMSFNCCustomPDFIO.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPDFIO() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncgraphicspdfengine */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICSPDFENGINE)
using namespace Fmx::Tmsfncgraphicspdfengine;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncgraphicspdfengineHPP
