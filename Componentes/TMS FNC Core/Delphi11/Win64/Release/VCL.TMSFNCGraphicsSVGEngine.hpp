﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphicsSVGEngine.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicssvgengineHPP
#define Vcl_TmsfncgraphicssvgengineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <VCL.TMSFNCGDIPlusClasses.hpp>
#include <VCL.TMSFNCGDIPlusApi.hpp>
#include <VCL.TMSFNCPersistence.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Xml.XMLIntf.hpp>
#include <Xml.XMLDoc.hpp>
#include <System.Variants.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncgraphicssvgengine
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsSVGEngine;
class DELPHICLASS TTMSFNCGraphicsSVG;
class DELPHICLASS TTMSFNCGraphicsSVGElementClassList;
class DELPHICLASS TTMSFNCGraphicsSVGElementList;
class DELPHICLASS TTMSFNCGraphicsSVGElementPathList;
class DELPHICLASS TTMSFNCGraphicsSVGElement;
class DELPHICLASS TTMSFNCGraphicsSVGImport;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGEngine : public Vcl::Tmsfncgraphics::TTMSFNCGraphics
{
	typedef Vcl::Tmsfncgraphics::TTMSFNCGraphics inherited;
	
private:
	Vcl::Graphics::TBitmap* FCanvas;
	System::Classes::TStringList* FSVG;
	float FWidth;
	float FHeight;
	
protected:
	System::UnicodeString __fastcall GenerateStroke();
	System::UnicodeString __fastcall GenerateFill();
	
public:
	__fastcall virtual TTMSFNCGraphicsSVGEngine(float AWidth, float AHeight)/* overload */;
	__fastcall virtual ~TTMSFNCGraphicsSVGEngine();
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* __fastcall SaveState(bool ACanvasOnly = false);
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle);
	System::UnicodeString __fastcall Build();
	virtual void __fastcall BeginScene();
	virtual void __fastcall EndScene();
	virtual void __fastcall BeginPrinting();
	virtual void __fastcall EndPrinting();
	virtual void __fastcall ResetTextAngle(float AAngle);
	virtual void __fastcall RestoreState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState, bool ACanvasOnly = false);
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
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsSVGEngine(Vcl::Graphics::TCanvas* ACanvas) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsSVGEngine(int AWidth, int AHeight, bool ANative, bool AHighDPI) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsSVGEngine(int AWidth, int AHeight) : Vcl::Tmsfncgraphics::TTMSFNCGraphics(AWidth, AHeight) { }
	
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
	inline System::UnicodeString __fastcall  DrawText(const System::Types::TPointF &APoint, System::UnicodeString AText, float AAngle = 0.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(APoint, AText, AAngle, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ARect, AText, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ALeft, ATop, ARight, ABottom, AText, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ARect, AText, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ALeft, ATop, ARight, ABottom, AText, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ALeft, ATop, ARight, ABottom, AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ARect, AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	inline System::UnicodeString __fastcall  DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f){ return Vcl::Tmsfncgraphics::TTMSFNCGraphics::DrawText(ALeft, ATop, ARight, ABottom, AText, AControlID, AControlValue, AControlType, AWordWrapping, AHorizontalAlign, AVerticalAlign, ATrimming, AAngle, AMinWidth, AMinHeight, ASupportHTML, ATestAnchor, AX, AY); }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsSVG : public TTMSFNCGraphicsSVGEngine
{
	typedef TTMSFNCGraphicsSVGEngine inherited;
	
public:
	/* TTMSFNCGraphicsSVGEngine.Create */ inline __fastcall virtual TTMSFNCGraphicsSVG(float AWidth, float AHeight)/* overload */ : TTMSFNCGraphicsSVGEngine(AWidth, AHeight) { }
	/* TTMSFNCGraphicsSVGEngine.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsSVG() { }
	
public:
	/* TTMSFNCGraphics.CreateNative */ inline __fastcall virtual TTMSFNCGraphicsSVG(Vcl::Graphics::TCanvas* ACanvas) : TTMSFNCGraphicsSVGEngine(ACanvas) { }
	/* TTMSFNCGraphics.CreateBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsSVG(int AWidth, int AHeight, bool ANative, bool AHighDPI) : TTMSFNCGraphicsSVGEngine(AWidth, AHeight, ANative, AHighDPI) { }
	/* TTMSFNCGraphics.CreateNativeBitmapCanvas */ inline __fastcall virtual TTMSFNCGraphicsSVG(int AWidth, int AHeight) : TTMSFNCGraphicsSVGEngine(AWidth, AHeight) { }
	
};


enum DECLSPEC_DENUM TTMSFNCGraphicsSVGElementType : unsigned char { etContainer, etSwitch, etDefs, etUse, etRect, etLine, etPolyline, etPolygon, etCircle, etEllipse, etPath, etImage, etText, etTextSpan, etTextSpanChild, etTextPath, etClipPath, etLinearGradient, etRadialGradient, etStyle, etStop, etPattern, etUnknown };

class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGElementClassList : public System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*> inherited;
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementClassList()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*>() { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementClassList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCGraphicsSVGElement*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementClassList(System::Generics::Collections::TEnumerable__1<TTMSFNCGraphicsSVGElement*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementClassList(TTMSFNCGraphicsSVGElement* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsSVGElement*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsSVGElementClassList() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGElementList : public System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>() { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCGraphicsSVGElement*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(System::Generics::Collections::TEnumerable__1<TTMSFNCGraphicsSVGElement*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsSVGElementList() { }
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCGraphicsSVGElement*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(System::Generics::Collections::TEnumerable__1<TTMSFNCGraphicsSVGElement*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsSVGEngine_TTMSFNCGraphicsSVGElement>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementList(TTMSFNCGraphicsSVGElement* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCGraphicsSVGElement*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGElementPathList : public System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>
{
	typedef System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList()/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>() { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(System::Generics::Collections::TEnumerable__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsSVGElementPathList() { }
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(System::Generics::Collections::TEnumerable__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPath>.Create */ inline __fastcall TTMSFNCGraphicsSVGElementPathList(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath*>(Values, Values_High) { }
	
};


enum DECLSPEC_DENUM TTMSFNCGraphicsSVGElementPreserveAspectRatio : unsigned char { parNone, parOther };

class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGElement : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* FCombinedPath;
	TTMSFNCGraphicsSVGElementPathList* FPaths;
	TTMSFNCGraphicsSVGElementList* FElements;
	TTMSFNCGraphicsSVGElementType FType;
	float FWidth;
	float FXV;
	float FYV;
	float FHeight;
	float FRx;
	float FRy;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FMatrix;
	System::Uitypes::TColor FFillColor;
	System::Uitypes::TColor FStrokeColor;
	System::UnicodeString FFillColorValue;
	System::UnicodeString FStrokeColorValue;
	TTMSFNCGraphicsSVGElement* FParent;
	float FStrokeWidth;
	System::UnicodeString FStrokeWidthValue;
	System::UnicodeString FOpacityValue;
	System::UnicodeString FOffsetValue;
	float FOffset;
	System::UnicodeString FStopColorValue;
	System::Uitypes::TColor FStopColor;
	System::UnicodeString FStopOpacityValue;
	float FStopOpacity;
	float FOpacity;
	System::UnicodeString FID;
	System::UnicodeString FClassRefID;
	TTMSFNCGraphicsSVGElement* FClassRef;
	System::UnicodeString FHRefID;
	System::UnicodeString FURLRefID;
	TTMSFNCGraphicsSVGElement* FHRefRef;
	TTMSFNCGraphicsSVGElement* FPatternRef;
	System::UnicodeString FClipPathRefID;
	TTMSFNCGraphicsSVGElement* FClipPathRef;
	System::UnicodeString FFontFamily;
	System::UnicodeString FFontSizeValue;
	float FFontSize;
	System::Uitypes::TFontStyles FFontStyle;
	System::UnicodeString FText;
	System::UnicodeString FDisplayValue;
	System::UnicodeString FPreserveAspectRatioValue;
	TTMSFNCGraphicsSVGElementPreserveAspectRatio FPreserveAspectRatio;
	float FFR;
	float FCX;
	float FCY;
	float FFX;
	float FFY;
	bool FHasCX;
	bool FHasCY;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FGradientMatrix;
	TTMSFNCGraphicsSVGElementList* __fastcall GetElements();
	TTMSFNCGraphicsSVGElementPathList* __fastcall GetPaths();
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* __fastcall GetActivePath();
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* __fastcall GetCombinedPath();
	Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetBitmap();
	
public:
	__fastcall virtual TTMSFNCGraphicsSVGElement();
	__fastcall virtual ~TTMSFNCGraphicsSVGElement();
	bool __fastcall HasStrokeColor();
	bool __fastcall HasStrokeWidth();
	bool __fastcall HasFontSize();
	bool __fastcall HasOpacity();
	bool __fastcall HasOffset();
	bool __fastcall HasStopColor();
	bool __fastcall HasStopOpacity();
	bool __fastcall HasPreserveAspectRatio();
	bool __fastcall HasFillColor();
	bool __fastcall HasElements();
	bool __fastcall HasPaths();
	bool __fastcall HasBitmap();
	bool __fastcall HasActivePath();
	__property System::UnicodeString ID = {read=FID};
	__property System::UnicodeString ClassRefID = {read=FClassRefID};
	__property System::UnicodeString URLRefID = {read=FURLRefID};
	__property System::UnicodeString HRefID = {read=FHRefID};
	__property System::UnicodeString ClipPathRefID = {read=FClipPathRefID};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* CombinedPath = {read=GetCombinedPath};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* ActivePath = {read=GetActivePath};
	__property TTMSFNCGraphicsSVGElementType Type = {read=FType, write=FType, nodefault};
	__property TTMSFNCGraphicsSVGElementList* Elements = {read=GetElements};
	__property TTMSFNCGraphicsSVGElementPathList* Paths = {read=GetPaths};
	__property float Opacity = {read=FOpacity};
	__property float Offset = {read=FOffset};
	__property System::Uitypes::TColor StopColor = {read=FStopColor, nodefault};
	__property float StopOpacity = {read=FStopOpacity};
	__property float X = {read=FXV};
	__property float Y = {read=FYV};
	__property float Width = {read=FWidth};
	__property float Height = {read=FHeight};
	__property float Rx = {read=FRx};
	__property float Ry = {read=FRy};
	__property float CX = {read=FCX};
	__property float CY = {read=FCY};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix Matrix = {read=FMatrix};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix GradientMatrix = {read=FGradientMatrix};
	__property float StrokeWidth = {read=FStrokeWidth};
	__property System::UnicodeString DisplayValue = {read=FDisplayValue};
	__property System::UnicodeString FillColorValue = {read=FFillColorValue};
	__property System::UnicodeString StrokeColorValue = {read=FStrokeColorValue};
	__property System::UnicodeString StrokeWidthValue = {read=FStrokeWidthValue};
	__property System::UnicodeString OpacityValue = {read=FOpacityValue};
	__property System::UnicodeString OffsetValue = {read=FOffsetValue};
	__property System::UnicodeString StopColorValue = {read=FStopColorValue};
	__property System::UnicodeString StopOpacityValue = {read=FStopOpacityValue};
	__property System::UnicodeString FontSizeValue = {read=FFontSizeValue};
	__property float FontSize = {read=FFontSize};
	__property System::UnicodeString FontFamily = {read=FFontFamily};
	__property System::Uitypes::TFontStyles FontStyle = {read=FFontStyle, nodefault};
	__property System::UnicodeString Text = {read=FText};
	__property System::Uitypes::TColor FillColor = {read=FFillColor, nodefault};
	__property System::Uitypes::TColor StrokeColor = {read=FStrokeColor, nodefault};
	__property TTMSFNCGraphicsSVGElement* Parent = {read=FParent};
	__property TTMSFNCGraphicsSVGElement* ClassRef = {read=FClassRef};
	__property TTMSFNCGraphicsSVGElement* ClipPathRef = {read=FClipPathRef};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=GetBitmap};
	__property System::UnicodeString PreserveAspectRatioValue = {read=FPreserveAspectRatioValue};
	__property TTMSFNCGraphicsSVGElementPreserveAspectRatio PreserveAspectRatio = {read=FPreserveAspectRatio, nodefault};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};


typedef Xml::Xmlintf::_di_IXMLNode TTMSFNCGraphicsSVGXMLNode;

typedef Xml::Xmlintf::_di_IXMLDocument TTMSFNCGraphicsSVGXMLDocument;

typedef System::OleVariant TTMSFNCGraphicsSVGXMLNodeValue;

class PASCALIMPLEMENTATION TTMSFNCGraphicsSVGImport : public Vcl::Tmsfnctypes::TTMSFNCSVGImport
{
	typedef Vcl::Tmsfnctypes::TTMSFNCSVGImport inherited;
	
private:
	Xml::Xmlintf::_di_IXMLDocument FXML;
	TTMSFNCGraphicsSVGElementClassList* FElementClassList;
	TTMSFNCGraphicsSVGElementClassList* FElementPatternList;
	TTMSFNCGraphicsSVGElementClassList* FElementClipPathList;
	TTMSFNCGraphicsSVGElementList* FElementList;
	TTMSFNCGraphicsSVGElementList* FDefsList;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FCachedBitmap;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FMatrix;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix FTextMatrix;
	
protected:
	System::UnicodeString __fastcall NodeToString(Xml::Xmlintf::_di_IXMLNode ANode);
	System::Uitypes::TColor __fastcall ConvertColorToGray(System::Uitypes::TColor AColor);
	System::Uitypes::TColor __fastcall ApplyTintColor(System::Uitypes::TColor AColor, System::Uitypes::TColor ATintColor);
	void __fastcall UpdateViewRect(TTMSFNCGraphicsSVGElement* const AElement, const System::Types::TRectF &ARect);
	void __fastcall ParseStyleValues(TTMSFNCGraphicsSVGElement* const AElement, const System::UnicodeString Values);
	System::Uitypes::TColor __fastcall ParseRGB(const System::UnicodeString S, int &AAlpha);
	void __fastcall ParseTransform(const Xml::Xmlintf::_di_IXMLNode ANode, const System::UnicodeString AString, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	void __fastcall ParseString(const Xml::Xmlintf::_di_IXMLNode ANode, const System::UnicodeString AString, System::UnicodeString &AValue);
	void __fastcall ParseLength(const Xml::Xmlintf::_di_IXMLNode ANode, const System::UnicodeString AString, float &AValue);
	void __fastcall LinkPatternHRef(TTMSFNCGraphicsSVGElementClassList* const AElementClasses, TTMSFNCGraphicsSVGElementClassList* const AElementDefs);
	void __fastcall ReadNode(TTMSFNCGraphicsSVGElementList* const AElements, const Xml::Xmlintf::_di_IXMLNode ANode, const bool AUpdateViewRect = true, TTMSFNCGraphicsSVGElement* const AParent = (TTMSFNCGraphicsSVGElement*)(0x0));
	void __fastcall LinkHRef(TTMSFNCGraphicsSVGElementClassList* const AElementClasses, TTMSFNCGraphicsSVGElementList* AElementDefs);
	void __fastcall LinkPatterns(TTMSFNCGraphicsSVGElementClassList* const AElementClasses, TTMSFNCGraphicsSVGElementList* AElementDefs);
	void __fastcall LinkClipPaths(TTMSFNCGraphicsSVGElementClassList* const AElementClasses, TTMSFNCGraphicsSVGElementList* AElementDefs);
	void __fastcall LinkClasses(TTMSFNCGraphicsSVGElementClassList* const AElementClasses, TTMSFNCGraphicsSVGElementList* AElementDefs);
	void __fastcall AddArc(TTMSFNCGraphicsSVGElement* const AElement, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* const APath, float AStartX, float AStopX, float AStartY, float AStopY, float ARx, float ARy, float AXRot, int ALarge, int ASweep);
	void __fastcall FixCurve(const System::WideChar ACommand, System::Classes::TStrings* SL);
	void __fastcall InternalDraw(TTMSFNCGraphicsSVGElementList* const AElements, Vcl::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, const float AScaleX, const float AScaleY);
	bool __fastcall ParseImage(const Xml::Xmlintf::_di_IXMLNode ANode, const System::UnicodeString AString, System::UnicodeString &AValue);
	int __fastcall IndexOfAny(const System::UnicodeString AValue, const System::WideChar *AnyOf, const int AnyOf_High, int StartIndex, int Count);
	TTMSFNCGraphicsSVGElementType __fastcall NameToElementType(const System::UnicodeString AName);
	System::Classes::TStrings* __fastcall ProcessValues(const System::WideChar ACommand, const System::UnicodeString S);
	System::Classes::TStrings* __fastcall SplitPath(const System::UnicodeString S);
	
public:
	virtual void __fastcall Draw(Vcl::Graphics::TCanvas* const ACanvas, const System::Types::TRectF &ARect, bool ANative = false)/* overload */;
	HIDESBASE void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics, const System::Types::TRectF &ARect, bool ANative = false, bool ARecreateGraphics = true)/* overload */;
	__fastcall virtual TTMSFNCGraphicsSVGImport();
	__fastcall virtual ~TTMSFNCGraphicsSVGImport();
	virtual void __fastcall LoadFromText(const System::UnicodeString AText);
	virtual void __fastcall LoadFromFile(const System::UnicodeString AFile);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* const AStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* const AStream);
	virtual void __fastcall SaveToFile(const System::UnicodeString AFile);
	virtual void __fastcall Clear();
	virtual bool __fastcall HasElements();
	virtual int __fastcall ElementCount();
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GenerateBitmap(float AWidth = -1.000000E+00f, float AHeight = -1.000000E+00f);
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x1);
}	/* namespace Tmsfncgraphicssvgengine */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICSSVGENGINE)
using namespace Vcl::Tmsfncgraphicssvgengine;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgraphicssvgengineHPP
