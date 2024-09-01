// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphics.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncgraphicsHPP
#define Fmx_TmsfncgraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphics
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsContext;
class DELPHICLASS TTMSFNCGraphics;
__interface DELPHIINTERFACE ITMSFNCGraphicsExport;
typedef System::DelphiInterface<ITMSFNCGraphicsExport> _di_ITMSFNCGraphicsExport;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsContext : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCGraphics* FGraphics;
	Fmx::Graphics::TCanvas* __fastcall GetCanvas();
	TTMSFNCGraphics* __fastcall GetGraphics();
	
protected:
	virtual void * __fastcall GetNativeCanvas() = 0 ;
	
public:
	__fastcall virtual TTMSFNCGraphicsContext(TTMSFNCGraphics* const AGraphics);
	virtual void * __fastcall CreatePath() = 0 ;
	void * __fastcall ConvertToPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, const float Flatness = 2.500000E-01f);
	virtual System::Uitypes::TAlphaColor __fastcall GetFillColor() = 0 ;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping) = 0 ;
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle) = 0 ;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix() = 0 ;
	virtual void __fastcall Render() = 0 ;
	virtual void __fastcall PathOpen(void * APath) = 0 ;
	virtual void __fastcall PathMoveTo(void * APath, const System::Types::TPointF &APoint) = 0 ;
	virtual void __fastcall PathLineTo(void * APath, const System::Types::TPointF &APoint) = 0 ;
	virtual void __fastcall SetScale(float AScale) = 0 ;
	virtual void __fastcall PathClose(void * APath) = 0 ;
	virtual void __fastcall ResetClip() = 0 ;
	virtual void __fastcall ResetTransform() = 0 ;
	virtual void __fastcall ScaleTransform(float AX, float AY) = 0 ;
	virtual void __fastcall RotateTransform(float AAngle) = 0 ;
	virtual void __fastcall TranslateTransform(float AX, float AY) = 0 ;
	virtual void __fastcall SetTextQuality(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality ATextQuality) = 0 ;
	virtual void __fastcall SetAntiAliasing(bool AAntiAliasing) = 0 ;
	virtual void __fastcall SetShowAcceleratorChar(bool AShowAcceleratorChar) = 0 ;
	virtual void __fastcall SetSize(float AWidth, float AHeight) = 0 ;
	virtual void __fastcall ResetTextAngle(float AAngle) = 0 ;
	virtual void __fastcall BeginScene() = 0 ;
	virtual void __fastcall EndScene() = 0 ;
	virtual void __fastcall BeginPrinting() = 0 ;
	virtual void __fastcall EndPrinting() = 0 ;
	virtual void __fastcall StartSpecialPen() = 0 ;
	virtual void __fastcall StopSpecialPen() = 0 ;
	virtual void __fastcall RestoreState(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState) = 0 ;
	virtual void __fastcall SaveState(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState) = 0 ;
	virtual void __fastcall SetFontSize(int ASize) = 0 ;
	virtual void __fastcall SetFontColor(System::Uitypes::TAlphaColor AColor) = 0 ;
	virtual void __fastcall SetFontName(System::UnicodeString AName) = 0 ;
	virtual void __fastcall SetFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont) = 0 ;
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle) = 0 ;
	virtual void __fastcall SetMatrix(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix) = 0 ;
	virtual void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill) = 0 ;
	virtual void __fastcall SetFillKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind) = 0 ;
	virtual void __fastcall SetFillColor(System::Uitypes::TAlphaColor AColor) = 0 ;
	virtual void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke) = 0 ;
	virtual void __fastcall SetStrokeKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind) = 0 ;
	virtual void __fastcall SetStrokeColor(System::Uitypes::TAlphaColor AColor) = 0 ;
	virtual void __fastcall SetStrokeWidth(float AWidth) = 0 ;
	virtual void __fastcall DrawLine(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2)) = 0 ;
	virtual void __fastcall DrawPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon) = 0 ;
	virtual void __fastcall FillPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon) = 0 ;
	virtual void __fastcall DrawPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline) = 0 ;
	virtual void __fastcall FillPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline) = 0 ;
	virtual void __fastcall FillArc(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle) = 0 ;
	virtual void __fastcall DrawArc(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle) = 0 ;
	virtual void __fastcall FillRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall FillRoundRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, float ARounding, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawRoundRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, float ARounding, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall FillEllipse(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawEllipse(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawBitmap(Fmx::Graphics::TBitmap* ABitmap, const System::Types::TRectF &ASrcRect, const System::Types::TRectF &ADstRect, float AOpacity) = 0 ;
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect) = 0 ;
	virtual void __fastcall ClipPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath) = 0 ;
	virtual void __fastcall DrawFocusPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TAlphaColor AColor) = 0 ;
	virtual void __fastcall DrawFocusRectangle(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, System::Uitypes::TAlphaColor AColor, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming, float AAngle) = 0 ;
	virtual void __fastcall DrawPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0)) = 0 ;
	virtual void __fastcall FillPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0)) = 0 ;
	__property Fmx::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property void * NativeCanvas = {read=GetNativeCanvas};
	__property TTMSFNCGraphics* Graphics = {read=GetGraphics};
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
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsContext() { }
	
};

#pragma pack(pop)

_DECLARE_METACLASS(System::TMetaClass, TTMSFNCGraphicsContextClass);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphics : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Fmx::Graphics::TCanvas* FActiveCanvas;
	int FBlockUpdate;
	bool FNative;
	TTMSFNCGraphicsContext* FContextNative;
	TTMSFNCGraphicsContext* FContextGeneral;
	Fmx::Graphics::TBitmap* FBitmap;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	System::Uitypes::TAlphaColor FHighlightColor;
	bool FOptimizedHTMLDrawing;
	System::Uitypes::TAlphaColor FHighlightTextColor;
	System::Uitypes::TFontStyles FHighlightFontStyles;
	bool FURLUnderline;
	System::Uitypes::TAlphaColor FURLColor;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	Fmx::Graphics::TCanvas* __fastcall GetCanvas();
	TTMSFNCGraphicsContext* __fastcall GetContext();
	
protected:
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall InitializeDefaultAppearance();
	Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall InternalDrawBitmapPartSync(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false);
	virtual System::Types::TRectF __fastcall InternalCalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::UnicodeString __fastcall InternalDrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	void __fastcall DrawTexture(const System::Types::TRectF &ARect, Fmx::Tmsfnctypes::TTMSFNCBitmap* ATexture, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextureMode ATextureMode);
	bool __fastcall DrawSVG(Fmx::Graphics::TBitmap* ABitmap, const System::Types::TRectF &ARect);
	bool __fastcall HasSVG(Fmx::Graphics::TBitmap* ABitmap);
	__classmethod void __fastcall ConvertBitmapToGrayScale(Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	
public:
	static System::Uitypes::TAlphaColor DefaultSelectionFillColor;
	static System::Uitypes::TAlphaColor DefaultTextFontColor;
	static System::Uitypes::TAlphaColor DefaultPopupFillColor;
	static System::Uitypes::TAlphaColor DefaultPopupStrokeColor;
	static System::Uitypes::TAlphaColor DefaultButtonStrokeColorFocused;
	static System::Uitypes::TAlphaColor DefaultButtonFillColorFocused;
	static System::Uitypes::TAlphaColor DefaultButtonStrokeColorDisabled;
	static System::Uitypes::TAlphaColor DefaultButtonFillColorDisabled;
	static System::Uitypes::TAlphaColor DefaultButtonStrokeColor;
	static System::Uitypes::TAlphaColor DefaultButtonFillColor;
	__fastcall virtual TTMSFNCGraphics(Fmx::Graphics::TCanvas* ACanvas, bool ANative);
	__fastcall virtual TTMSFNCGraphics(Fmx::Graphics::TCanvas* ACanvas);
	__fastcall virtual TTMSFNCGraphics(int AWidth, int AHeight, bool ANative, bool AHighDPI);
	__fastcall virtual TTMSFNCGraphics(int AWidth, int AHeight);
	__fastcall virtual ~TTMSFNCGraphics();
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	void __fastcall StartSpecialPen();
	void __fastcall StopSpecialPen();
	virtual void __fastcall BeginPrinting();
	virtual void __fastcall EndPrinting();
	virtual void __fastcall BeginScene();
	virtual void __fastcall EndScene();
	virtual void __fastcall SetMatrix(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall Assign(TTMSFNCGraphics* Source);
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
	virtual void __fastcall DrawLine(const System::Types::TPoint &AFromPoint, const System::Types::TPoint &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawLine(const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(int ALeft, int ATop, int ARight, int ABottom, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff000000), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(const System::Types::TRect &ARect, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff000000), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(double ALeft, double ATop, double ARight, double ABottom, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff000000), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(const System::Types::TRectF &ARect, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff000000), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff000000));
	virtual void __fastcall DrawPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall DrawPath(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall DrawArc(const System::Types::TPointF &Center, const System::Types::TPointF &Radius, float StartAngle, float SweepAngle)/* overload */;
	virtual void __fastcall DrawArc(const System::Types::TPoint &Center, const System::Types::TPoint &Radius, int StartAngle, int SweepAngle)/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(double ALeft, double ATop, double ARight, double ABottom, float ARounding = 1.000000E+01f, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(const System::Types::TRectF &ARect, float ARounding = 1.000000E+01f, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(int ALeft, int ATop, int ARight, int ABottom, int ARounding = 0xa, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(const System::Types::TRect &ARect, int ARounding = 0xa, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawBitmapPart(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapPart(const System::Types::TRectF &ASourceRect, const System::Types::TRectF &ADestinationRect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(double ALeft, double ATop, double ARight, double ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(const System::Types::TRectF &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawEllipse(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawBitmapPart(int ASourceLeft, int ASourceTop, int ASourceRight, int ASourceBottom, int ADestinationLeft, int ADestinationTop, int ADestinationRight, int ADestinationBottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapPart(const System::Types::TRect &ASourceRect, const System::Types::TRect &ADestinationRect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(int ALeft, int ATop, int ARight, int ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(const System::Types::TRect &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(int ALeft, int ATop, Fmx::Graphics::TBitmap* ABitmap)/* overload */;
	virtual void __fastcall DrawBitmap(float ALeft, float ATop, Fmx::Graphics::TBitmap* ABitmap)/* overload */;
	virtual void __fastcall DrawCheckBox(const System::Types::TRectF &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCloseButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawExpanderButton(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState AState = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCompactButton(const System::Types::TRectF &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState AState = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawDropDownButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool ACenter = false, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawRadioButton(const System::Types::TRectF &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawProgressBar(const System::Types::TRectF &ARect, float AValue, System::UnicodeString AFormat = u"%.0f%%", float AMax = 1.000000E+02f, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff9acd32), System::Uitypes::TAlphaColor ATextColor = (System::Uitypes::TAlphaColor)(0xff000000), bool AShowText = true, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCheckBox(const System::Types::TRect &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCloseButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawExpanderButton(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState AState = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCompactButton(const System::Types::TRect &ARect, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState AState = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawDropDownButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool ACenter = false, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawRadioButton(const System::Types::TRect &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawProgressBar(const System::Types::TRect &ARect, float AValue, System::UnicodeString AFormat = u"%.0f%%", float AMax = 1.000000E+02f, System::Uitypes::TAlphaColor AColor = (System::Uitypes::TAlphaColor)(0xff9acd32), System::Uitypes::TAlphaColor ATextColor = (System::Uitypes::TAlphaColor)(0xff000000), bool AShowText = true, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawBitmapWithName(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(const System::Types::TRectF &ARect, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(const System::Types::TRectF &ARect, Fmx::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(double ALeft, double ATop, double ARight, double ABottom, Fmx::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(const System::Types::TRect &ARect, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(const System::Types::TRect &ARect, Fmx::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(int ALeft, int ATop, int ARight, int ABottom, Fmx::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRectF __fastcall GetBitmapDrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRectF __fastcall GetBitmapDrawRectangle(const System::Types::TRectF &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRect __fastcall GetBitmapDrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRect __fastcall GetBitmapDrawRectangle(const System::Types::TRect &ARect, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect)/* overload */;
	virtual Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall DrawBitmapPartSync(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Fmx::Graphics::TBitmap* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false);
	virtual System::Types::TSizeF __fastcall CalculateTextSize(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual float __fastcall CalculateTextWidth(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual float __fastcall CalculateTextHeight(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::Types::TSizeF __fastcall CalculateTextSize(System::UnicodeString AText)/* overload */;
	virtual float __fastcall CalculateTextWidth(System::UnicodeString AText)/* overload */;
	virtual float __fastcall CalculateTextHeight(System::UnicodeString AText)/* overload */;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText)/* overload */;
	virtual void __fastcall ClipRect(const System::Types::TRect &ARect)/* overload */;
	virtual System::Types::TSize __fastcall CalculateTextSize(System::UnicodeString AText, const System::Types::TRect &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual int __fastcall CalculateTextWidth(System::UnicodeString AText, const System::Types::TRect &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual int __fastcall CalculateTextHeight(System::UnicodeString AText, const System::Types::TRect &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::Types::TRect __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRect &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::Types::TSize __fastcall CalculateTextSizeInt(System::UnicodeString AText)/* overload */;
	virtual int __fastcall CalculateTextWidthInt(System::UnicodeString AText)/* overload */;
	virtual int __fastcall CalculateTextHeightInt(System::UnicodeString AText)/* overload */;
	virtual System::Types::TRect __fastcall CalculateTextInt(System::UnicodeString AText)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TPointF &APoint, System::UnicodeString AText, float AAngle = 0.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f)/* overload */;
	virtual Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* __fastcall SaveState(bool ACanvasOnly = false);
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont};
	__property Fmx::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property TTMSFNCGraphicsContext* Context = {read=GetContext};
	__property Fmx::Graphics::TBitmap* Bitmap = {read=FBitmap};
	__property bool OptimizedHTMLDrawing = {read=FOptimizedHTMLDrawing, write=FOptimizedHTMLDrawing, nodefault};
	__property System::Uitypes::TAlphaColor HighlightColor = {read=FHighlightColor, write=FHighlightColor, nodefault};
	__property System::Uitypes::TAlphaColor HighlightTextColor = {read=FHighlightTextColor, write=FHighlightTextColor, nodefault};
	__property System::Uitypes::TFontStyles HighlightFontStyle = {read=FHighlightFontStyles, write=FHighlightFontStyles, nodefault};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=FBitmapContainer};
	__property System::Uitypes::TAlphaColor URLColor = {read=FURLColor, write=FURLColor, default=-16776961};
	__property bool URLUnderline = {read=FURLUnderline, write=FURLUnderline, default=1};
	__classmethod System::UnicodeString __fastcall ApplyHilight(System::UnicodeString AText, System::UnicodeString AHilight, System::UnicodeString ATag, bool ADoCase);
	__classmethod virtual Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetBitmapFromBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer, System::UnicodeString AName, bool AApplyScale = false, float AScale = 0.000000E+00f);
	__classmethod Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetScaledBitmap(Fmx::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer = (Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer*)(0x0));
	__classmethod virtual void __fastcall GetAspectSize(float &AWidth, float &AHeight, float AOriginalWidth, float AOriginalHeight, float ANewWidth, float ANewHeight, bool AAspectRatio = true, bool AStretch = false, bool ACropping = false);
	__classmethod virtual void __fastcall DrawSample(Fmx::Graphics::TCanvas* ACanvas, const System::Types::TRectF &ARect);
	__classmethod virtual void __fastcall SetDefaultGraphicColors();
	__classmethod virtual bool __fastcall PointInCircle(const System::Types::TPointF &APoint, const System::Types::TPointF &ACenter, const float ARadius);
	__classmethod virtual bool __fastcall PointInPath(const System::Types::TPointF &APoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath)/* overload */;
	__classmethod virtual bool __fastcall PointInPath(const System::Types::TPoint &APoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath)/* overload */;
	__classmethod virtual bool __fastcall PointInPolygon(const System::Types::TPointF &APoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon)/* overload */;
	__classmethod virtual bool __fastcall PointInPolygon(const System::Types::TPoint &APoint, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon)/* overload */;
	__classmethod virtual bool __fastcall PointInRect(const System::Types::TPointF &APoint, const System::Types::TRectF &ARect)/* overload */;
	__classmethod virtual bool __fastcall PointInRect(const System::Types::TPoint &APoint, const System::Types::TRect &ARect)/* overload */;
	__classmethod virtual System::Byte __fastcall GetColorRed(System::Uitypes::TAlphaColor AColor);
	__classmethod virtual System::Byte __fastcall GetColorGreen(System::Uitypes::TAlphaColor AColor);
	__classmethod virtual System::Byte __fastcall GetColorBlue(System::Uitypes::TAlphaColor AColor);
	__classmethod virtual System::Byte __fastcall GetColorAlpha(System::Uitypes::TAlphaColor AColor);
	__classmethod virtual System::UnicodeString __fastcall ColorToHTML(System::Uitypes::TAlphaColor AColor);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall HTMLToColor(System::UnicodeString AHTML);
	__classmethod virtual System::Uitypes::TAlphaColor __fastcall TextToColor(System::UnicodeString AText);
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

__interface  INTERFACE_UUID("{481CA803-8B50-4545-B212-57AC0D065D09}") ITMSFNCGraphicsExport  : public System::IInterface 
{
	virtual void __fastcall Export(TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncgraphics */
}	/* namespace Fmx */
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
#endif	// Fmx_TmsfncgraphicsHPP
