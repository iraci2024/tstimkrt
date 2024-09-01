// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphicsPDFEngine.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicspdfengineHPP
#define Vcl_TmsfncgraphicspdfengineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCPDFLib.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCPDFIO.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncgraphicspdfengine
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsPDFEngine;
class DELPHICLASS TTMSFNCGraphicsPDF;
class DELPHICLASS TTMSFNCCustomGraphicsPDFIO;
class DELPHICLASS TTMSFNCGraphicsPDFIO;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsPDFEngine : public Vcl::Tmsfncgraphics::TTMSFNCGraphics
{
	typedef Vcl::Tmsfncgraphics::TTMSFNCGraphics inherited;
	
private:
	Vcl::Graphics::TBitmap* FCanvas;
	Vcl::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib FPDFLib;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FMatrix;
	
protected:
	virtual System::Types::TRectF __fastcall InternalCalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::UnicodeString __fastcall InternalDrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	
public:
	__fastcall virtual TTMSFNCGraphicsPDFEngine(const Vcl::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib)/* overload */;
	__fastcall virtual TTMSFNCGraphicsPDFEngine(Vcl::Tmsfncpdflib::TTMSFNCCustomPDFLib* const APDFLib)/* overload */;
	__fastcall virtual ~TTMSFNCGraphicsPDFEngine();
	void __fastcall DrawPDFPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, const float Flatness = 2.500000E-01f);
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* __fastcall SaveState(bool ACanvasOnly = false);
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle);
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	virtual void __fastcall SetMatrix(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall ResetTextAngle(float AAngle);
	virtual void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill);
	virtual void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke);
	virtual void __fastcall RestoreState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState, bool ACanvasOnly = false);
	virtual void __fastcall SetFillKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind);
	virtual void __fastcall SetFillColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetFontColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetFontName(System::UnicodeString AName);
	virtual void __fastcall SetFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont);
	virtual void __fastcall SetFontSize(int ASize);
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle);
	virtual void __fastcall SetStrokeKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	virtual void __fastcall SetStrokeColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetStrokeWidth(float AWidth);
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect)/* overload */;
	virtual void __fastcall DrawLine(const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall DrawPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall DrawBitmap(double ALeft, double ATop, double ARight, double ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawArc(const System::Types::TPointF &Center, const System::Types::TPointF &Radius, float StartAngle, float SweepAngle)/* overload */;
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(Vcl::Graphics::TCanvas* ACanvas) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(int AWidth, int AHeight, bool ANative, bool AHighDPI) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDFEngine(int AWidth, int AHeight) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  ClipRect(const System::Types::TRect &ARect){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::ClipRect(ARect); }
	inline void __fastcall  DrawLine(const System::Types::TPoint &AFromPoint, const System::Types::TPoint &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawLine(AFromPoint, AToPoint, AModifyPointModeFrom, AModifyPointModeTo); }
	inline void __fastcall  DrawEllipse(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ALeft, ATop, ARight, ABottom, AModifyRectMode); }
	inline void __fastcall  DrawEllipse(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ARect, AModifyRectMode); }
	inline void __fastcall  DrawEllipse(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawEllipse(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, ASides, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ALeft, ATop, ARight, ABottom, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ALeft, ATop, ARight, ABottom, ASides, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, AModifyRectMode); }
	inline void __fastcall  DrawRectangle(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawRectangle(ARect, ASides, AModifyRectMode); }
	inline void __fastcall  DrawBitmap(const System::Types::TRectF &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ARect, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(int ALeft, int ATop, int ARight, int ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ARight, ABottom, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(const System::Types::TRect &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ARect, ABitmap, AAspectRatio, AStretch, ACenter, ACropping); }
	inline void __fastcall  DrawBitmap(int ALeft, int ATop, Vcl::Graphics::TPicture* ABitmap){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ABitmap); }
	inline void __fastcall  DrawBitmap(float ALeft, float ATop, Vcl::Graphics::TPicture* ABitmap){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawBitmap(ALeft, ATop, ABitmap); }
	inline void __fastcall  DrawArc(const System::Types::TPoint &Center, const System::Types::TPoint &Radius, int StartAngle, int SweepAngle){ Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawArc(Center, Radius, StartAngle, SweepAngle); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsPDF : public TTMSFNCGraphicsPDFEngine
{
	typedef TTMSFNCGraphicsPDFEngine inherited;
	
public:
	/* TTMSFNCGraphicsPDFEngine.Create */ inline __fastcall virtual TTMSFNCGraphicsPDF(const Vcl::Tmsfncpdflib::_di_ITMSFNCCustomPDFLib APDFLib)/* overload */ : TTMSFNCGraphicsPDFEngine(APDFLib) { }
	/* TTMSFNCGraphicsPDFEngine.Create */ inline __fastcall virtual TTMSFNCGraphicsPDF(Vcl::Tmsfncpdflib::TTMSFNCCustomPDFLib* const APDFLib)/* overload */ : TTMSFNCGraphicsPDFEngine(APDFLib) { }
	/* TTMSFNCGraphicsPDFEngine.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPDF() { }
	
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsPDF(Vcl::Graphics::TCanvas* ACanvas) : TTMSFNCGraphicsPDFEngine(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDF(int AWidth, int AHeight, bool ANative, bool AHighDPI) : TTMSFNCGraphicsPDFEngine(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsPDF(int AWidth, int AHeight) : TTMSFNCGraphicsPDFEngine(AWidth, AHeight) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCGraphicsPDFIOExportRectEvent)(System::TObject* Sender, Vcl::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);

typedef void __fastcall (__closure *TTMSFNCGraphicsPDFIOCanCreateNewPageEvent)(System::TObject* Sender, Vcl::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, bool &ACanCreate);

class PASCALIMPLEMENTATION TTMSFNCCustomGraphicsPDFIO : public Vcl::Tmsfncpdfio::TTMSFNCCustomPDFIO
{
	typedef Vcl::Tmsfncpdfio::TTMSFNCCustomPDFIO inherited;
	
private:
	TTMSFNCGraphicsPDFIOExportRectEvent FOnGetExportRect;
	TTMSFNCGraphicsPDFIOCanCreateNewPageEvent FOnCanCreateNewPage;
	
protected:
	virtual System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall DoPDFExport(Vcl::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, const System::Types::TRectF &AExportRect)/* overload */;
	virtual void __fastcall DoGetExportRect(Vcl::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, System::Types::TRectF &ARect);
	virtual void __fastcall DoCanCreateNewPage(Vcl::Tmsfncpdflib::TTMSFNCPDFLib* const APDFLib, System::Classes::TComponent* const AExportObject, bool &ACanCreate);
	__property System::UnicodeString Version = {read=GetVersion};
	__property TTMSFNCGraphicsPDFIOExportRectEvent OnGetExportRect = {read=FOnGetExportRect, write=FOnGetExportRect};
	__property TTMSFNCGraphicsPDFIOCanCreateNewPageEvent OnCanCreateNewPage = {read=FOnCanCreateNewPage, write=FOnCanCreateNewPage};
public:
	/* TTMSFNCCustomPDFIO.Create */ inline __fastcall virtual TTMSFNCCustomGraphicsPDFIO(System::Classes::TComponent* AOwner) : Vcl::Tmsfncpdfio::TTMSFNCCustomPDFIO(AOwner) { }
	/* TTMSFNCCustomPDFIO.Destroy */ inline __fastcall virtual ~TTMSFNCCustomGraphicsPDFIO() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomGraphicsPDFIO(HWND ParentWindow) : Vcl::Tmsfncpdfio::TTMSFNCCustomPDFIO(ParentWindow) { }
	
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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsPDFIO(HWND ParentWindow) : TTMSFNCCustomGraphicsPDFIO(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfncgraphicspdfengine */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICSPDFENGINE)
using namespace Vcl::Tmsfncgraphicspdfengine;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgraphicspdfengineHPP
