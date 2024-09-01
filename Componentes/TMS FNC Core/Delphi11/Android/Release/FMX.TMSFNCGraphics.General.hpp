// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphics.General.pas' rev: 35.00 (Android)

#ifndef Fmx_Tmsfncgraphics_GeneralHPP
#define Fmx_Tmsfncgraphics_GeneralHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <System.Types.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Types.hpp>
#include <FMX.TextLayout.hpp>
#include <System.Math.Vectors.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphics
{
namespace General
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsContextGeneral;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsContextGeneral : public Fmx::Tmsfncgraphics::TTMSFNCGraphicsContext
{
	typedef Fmx::Tmsfncgraphics::TTMSFNCGraphicsContext inherited;
	
private:
	bool FPrinting;
	bool FShowAcceleratorChar;
	System::Math::Vectors::TMatrix FMatrix;
	bool FMatrixSaved;
	System::Math::Vectors::TMatrix FTextMatrix;
	Fmx::Textlayout::TTextLayout* FTextLayout;
	
protected:
	virtual void * __fastcall GetNativeCanvas();
	void __fastcall SaveMatrix();
	void __fastcall RestoreMatrix();
	
public:
	__fastcall virtual ~TTMSFNCGraphicsContextGeneral();
	virtual System::Uitypes::TAlphaColor __fastcall GetFillColor();
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping);
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle);
	virtual void * __fastcall CreatePath();
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	virtual void __fastcall Render();
	virtual void __fastcall PathOpen(void * APath);
	virtual void __fastcall PathMoveTo(void * APath, const System::Types::TPointF &APoint);
	virtual void __fastcall PathLineTo(void * APath, const System::Types::TPointF &APoint);
	virtual void __fastcall PathClose(void * APath);
	virtual void __fastcall ResetTransform();
	virtual void __fastcall ResetClip();
	virtual void __fastcall ScaleTransform(float AX, float AY);
	virtual void __fastcall RotateTransform(float AAngle);
	virtual void __fastcall TranslateTransform(float AX, float AY);
	virtual void __fastcall SetTextQuality(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality ATextQuality);
	virtual void __fastcall SetAntiAliasing(bool AAntiAliasing);
	virtual void __fastcall SetShowAcceleratorChar(bool AShowAcceleratorChar);
	virtual void __fastcall SetSize(float AWidth, float AHeight);
	virtual void __fastcall ResetTextAngle(float AAngle);
	virtual void __fastcall BeginScene();
	virtual void __fastcall EndScene();
	virtual void __fastcall BeginPrinting();
	virtual void __fastcall EndPrinting();
	virtual void __fastcall StartSpecialPen();
	virtual void __fastcall StopSpecialPen();
	virtual void __fastcall RestoreState(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState);
	virtual void __fastcall SaveState(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState);
	virtual void __fastcall SetMatrix(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall SetScale(float AScale);
	virtual void __fastcall SetFontSize(int ASize);
	virtual void __fastcall SetFontColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetFontName(System::UnicodeString AName);
	virtual void __fastcall SetFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont);
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle);
	virtual void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill);
	virtual void __fastcall SetFillKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind);
	virtual void __fastcall SetFillColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke);
	virtual void __fastcall SetStrokeKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	virtual void __fastcall SetStrokeColor(System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall SetStrokeWidth(float AWidth);
	virtual void __fastcall DrawLine(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2));
	virtual void __fastcall DrawPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall FillPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall FillPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall FillArc(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle);
	virtual void __fastcall DrawArc(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle);
	virtual void __fastcall FillRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall FillRoundRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, float ARounding, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawRoundRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, float ARounding, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall FillEllipse(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawEllipse(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawBitmap(Fmx::Graphics::TBitmap* ABitmap, const System::Types::TRectF &ASrcRect, const System::Types::TRectF &ADstRect, float AOpacity);
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect);
	virtual void __fastcall ClipPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath);
	virtual void __fastcall DrawFocusPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall DrawFocusRectangle(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, System::Uitypes::TAlphaColor AColor, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming, float AAngle);
	virtual void __fastcall DrawPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall FillPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
public:
	/* TTMSFNCGraphicsContext.Create */ inline __fastcall virtual TTMSFNCGraphicsContextGeneral(Fmx::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics) : Fmx::Tmsfncgraphics::TTMSFNCGraphicsContext(AGraphics) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Fmx::Tmsfncgraphics::TTMSFNCGraphicsContextClass __fastcall GetNativeContextClass(void);
}	/* namespace General */
}	/* namespace Tmsfncgraphics */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICS_GENERAL)
using namespace Fmx::Tmsfncgraphics::General;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICS)
using namespace Fmx::Tmsfncgraphics;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_Tmsfncgraphics_GeneralHPP
