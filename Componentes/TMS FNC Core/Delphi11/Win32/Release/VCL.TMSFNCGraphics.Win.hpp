// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphics.Win.pas' rev: 35.00 (Windows)

#ifndef Vcl_Tmsfncgraphics_WinHPP
#define Vcl_Tmsfncgraphics_WinHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <VCL.TMSFNCGDIPlusApi.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCUtils.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <System.UITypes.hpp>
#include <VCL.TMSFNCGDIPlusClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncgraphics
{
namespace Win
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsContextWin;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsContextWin : public Vcl::Tmsfncgraphics::TTMSFNCGraphicsContext
{
	typedef Vcl::Tmsfncgraphics::TTMSFNCGraphicsContext inherited;
	
private:
	bool FShowAcceleratorChar;
	Vcl::Tmsfncgdiplusclasses::TGPMatrix* FTextMatrix;
	int FSmoothingMode;
	unsigned FSaveGP;
	float FScale;
	System::Types::TPointF FMovePoint;
	bool FNeedsRendering;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Vcl::Tmsfncgdiplusclasses::TGPFont* FGPFont;
	System::Types::TSizeF FContextSize;
	Vcl::Tmsfncgdiplusclasses::TGPStringFormat* FGPStringFormat;
	Vcl::Tmsfncgdiplusclasses::TGPBrush* FGPBrush;
	Vcl::Tmsfncgdiplusclasses::TGPPen* FGPPen;
	Vcl::Tmsfncgdiplusclasses::TGPGraphics* FGPGraphics;
	Vcl::Tmsfncgdiplusclasses::TGPBitmap* FGPBitmap;
	Vcl::Graphics::TBitmap* FBitmap;
	
protected:
	virtual void * __fastcall GetNativeCanvas();
	void __fastcall DestroyResources();
	void __fastcall ApplyFill(Vcl::Tmsfncgdiplusclasses::TGPGraphicsPath* APath);
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall SetSmoothingMode(int ASmoothingMode);
	void __fastcall RestoreSmoothingMode();
	
public:
	__fastcall virtual TTMSFNCGraphicsContextWin(Vcl::Tmsfncgraphics::TTMSFNCGraphics* const AGraphics);
	__fastcall virtual ~TTMSFNCGraphicsContextWin();
	virtual System::Uitypes::TColor __fastcall GetFillColor();
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping);
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle);
	virtual void * __fastcall CreatePath();
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	virtual void __fastcall Render();
	virtual void __fastcall PathOpen(void * APath);
	virtual void __fastcall PathMoveTo(void * APath, const System::Types::TPointF &APoint);
	virtual void __fastcall PathLineTo(void * APath, const System::Types::TPointF &APoint);
	virtual void __fastcall PathClose(void * APath);
	virtual void __fastcall ResetClip();
	virtual void __fastcall ResetTransform();
	virtual void __fastcall SetScale(float AScale);
	virtual void __fastcall ScaleTransform(float AX, float AY);
	virtual void __fastcall RotateTransform(float AAngle);
	virtual void __fastcall TranslateTransform(float AX, float AY);
	virtual void __fastcall SetTextQuality(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality ATextQuality);
	virtual void __fastcall SetAntiAliasing(bool AAntiAliasing);
	virtual void __fastcall SetShowAcceleratorChar(bool AShowAcceleratorChar);
	virtual void __fastcall SetSize(float AWidth, float AHeight);
	virtual void __fastcall ResetTextAngle(float AAngle);
	virtual void __fastcall SetMatrix(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall BeginScene();
	virtual void __fastcall EndScene();
	virtual void __fastcall BeginPrinting();
	virtual void __fastcall EndPrinting();
	virtual void __fastcall StartSpecialPen();
	virtual void __fastcall StopSpecialPen();
	virtual void __fastcall RestoreState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState);
	virtual void __fastcall SaveState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState);
	virtual void __fastcall SetFontSize(int ASize);
	virtual void __fastcall SetFontColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetFontName(System::UnicodeString AName);
	virtual void __fastcall SetFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont);
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle);
	virtual void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill);
	virtual void __fastcall SetFillKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind);
	virtual void __fastcall SetFillColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke);
	virtual void __fastcall SetStrokeKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	virtual void __fastcall SetStrokeColor(System::Uitypes::TColor AColor);
	virtual void __fastcall SetStrokeWidth(float AWidth);
	virtual void __fastcall DrawLine(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2));
	virtual void __fastcall DrawPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall FillPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall FillPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall FillArc(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle);
	virtual void __fastcall DrawArc(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle);
	virtual void __fastcall FillRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall FillRoundRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, float ARounding, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawRoundRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, float ARounding, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall FillEllipse(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawEllipse(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawBitmap(Vcl::Graphics::TGraphic* ABitmap, const System::Types::TRectF &ASrcRect, const System::Types::TRectF &ADstRect, float AOpacity);
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect);
	virtual void __fastcall ClipPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath);
	virtual void __fastcall DrawFocusPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TColor AColor);
	virtual void __fastcall DrawFocusRectangle(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, System::Uitypes::TColor AColor, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2));
	virtual void __fastcall DrawText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming, float AAngle);
	virtual void __fastcall DrawPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall FillPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 FPC_FULLVERSION = System::Int8(0x0);
extern DELPHI_PACKAGE Vcl::Tmsfncgraphics::TTMSFNCGraphicsContextClass __fastcall GetNativeContextClass(void);
}	/* namespace Win */
}	/* namespace Tmsfncgraphics */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICS_WIN)
using namespace Vcl::Tmsfncgraphics::Win;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICS)
using namespace Vcl::Tmsfncgraphics;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_Tmsfncgraphics_WinHPP
