// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphics.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicsHPP
#define Vcl_TmsfncgraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.ImgList.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
	Vcl::Graphics::TCanvas* __fastcall GetCanvas();
	TTMSFNCGraphics* __fastcall GetGraphics();
	
protected:
	virtual void * __fastcall GetNativeCanvas() = 0 ;
	
public:
	__fastcall virtual TTMSFNCGraphicsContext(TTMSFNCGraphics* const AGraphics);
	virtual void * __fastcall CreatePath() = 0 ;
	void * __fastcall ConvertToPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, const float Flatness = 2.500000E-01f);
	virtual System::Uitypes::TColor __fastcall GetFillColor() = 0 ;
	virtual System::Types::TRectF __fastcall CalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping) = 0 ;
	virtual System::Types::TRectF __fastcall SetTextAngle(const System::Types::TRectF &ARect, float AAngle) = 0 ;
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix() = 0 ;
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
	virtual void __fastcall SetTextQuality(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality ATextQuality) = 0 ;
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
	virtual void __fastcall RestoreState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState) = 0 ;
	virtual void __fastcall SaveState(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* AState) = 0 ;
	virtual void __fastcall SetFontSize(int ASize) = 0 ;
	virtual void __fastcall SetFontColor(System::Uitypes::TColor AColor) = 0 ;
	virtual void __fastcall SetFontName(System::UnicodeString AName) = 0 ;
	virtual void __fastcall SetFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* AFont) = 0 ;
	virtual void __fastcall SetFontStyles(System::Uitypes::TFontStyles AStyle) = 0 ;
	virtual void __fastcall SetMatrix(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix) = 0 ;
	virtual void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill) = 0 ;
	virtual void __fastcall SetFillKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind) = 0 ;
	virtual void __fastcall SetFillColor(System::Uitypes::TColor AColor) = 0 ;
	virtual void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke) = 0 ;
	virtual void __fastcall SetStrokeKind(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind) = 0 ;
	virtual void __fastcall SetStrokeColor(System::Uitypes::TColor AColor) = 0 ;
	virtual void __fastcall SetStrokeWidth(float AWidth) = 0 ;
	virtual void __fastcall DrawLine(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2)) = 0 ;
	virtual void __fastcall DrawPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon) = 0 ;
	virtual void __fastcall FillPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon) = 0 ;
	virtual void __fastcall DrawPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline) = 0 ;
	virtual void __fastcall FillPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline) = 0 ;
	virtual void __fastcall FillArc(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle) = 0 ;
	virtual void __fastcall DrawArc(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TPointF &ACenter, const System::Types::TPointF &ARadius, float AStartAngle, float ASweepAngle) = 0 ;
	virtual void __fastcall FillRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall FillRoundRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, float ARounding, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawRoundRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, float ARounding, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall FillEllipse(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawEllipse(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawBitmap(Vcl::Graphics::TGraphic* ABitmap, const System::Types::TRectF &ASrcRect, const System::Types::TRectF &ADstRect, float AOpacity) = 0 ;
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect) = 0 ;
	virtual void __fastcall ClipPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath) = 0 ;
	virtual void __fastcall DrawFocusPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TColor AColor) = 0 ;
	virtual void __fastcall DrawFocusRectangle(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, const System::Types::TRectF &ARect, System::Uitypes::TColor AColor, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2)) = 0 ;
	virtual void __fastcall DrawText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming, float AAngle) = 0 ;
	virtual void __fastcall DrawPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* AStroke, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0)) = 0 ;
	virtual void __fastcall FillPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* AFill, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0)) = 0 ;
	__property Vcl::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property void * NativeCanvas = {read=GetNativeCanvas};
	__property TTMSFNCGraphics* Graphics = {read=GetGraphics};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsContext() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TTMSFNCGraphicsContextClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphics : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Graphics::TCanvas* FActiveCanvas;
	int FBlockUpdate;
	bool FNative;
	TTMSFNCGraphicsContext* FContextNative;
	TTMSFNCGraphicsContext* FContextGeneral;
	Vcl::Graphics::TBitmap* FBitmap;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	System::Uitypes::TColor FHighlightColor;
	bool FOptimizedHTMLDrawing;
	System::Uitypes::TColor FHighlightTextColor;
	System::Uitypes::TFontStyles FHighlightFontStyles;
	bool FURLUnderline;
	System::Uitypes::TColor FURLColor;
	Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	Vcl::Imglist::TCustomImageList* FImageList;
	Vcl::Graphics::TCanvas* __fastcall GetCanvas();
	TTMSFNCGraphicsContext* __fastcall GetContext();
	
protected:
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall InitializeDefaultAppearance();
	Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall InternalDrawBitmapPartSync(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false);
	virtual System::Types::TRectF __fastcall InternalCalculateText(System::UnicodeString AText, const System::Types::TRectF &ARect, bool AWordWrapping = false, bool ASupportHTML = true)/* overload */;
	virtual System::UnicodeString __fastcall InternalDrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	void __fastcall DrawTexture(const System::Types::TRectF &ARect, Vcl::Tmsfnctypes::TTMSFNCBitmap* ATexture, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextureMode ATextureMode);
	bool __fastcall DrawSVG(Vcl::Graphics::TPicture* ABitmap, const System::Types::TRectF &ARect);
	bool __fastcall HasSVG(Vcl::Graphics::TPicture* ABitmap);
	__classmethod void __fastcall ConvertBitmapToGrayScale(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	
public:
	static System::Uitypes::TColor DefaultSelectionFillColor;
	static System::Uitypes::TColor DefaultTextFontColor;
	static System::Uitypes::TColor DefaultPopupFillColor;
	static System::Uitypes::TColor DefaultPopupStrokeColor;
	static System::Uitypes::TColor DefaultButtonStrokeColorFocused;
	static System::Uitypes::TColor DefaultButtonFillColorFocused;
	static System::Uitypes::TColor DefaultButtonStrokeColorDisabled;
	static System::Uitypes::TColor DefaultButtonFillColorDisabled;
	static System::Uitypes::TColor DefaultButtonStrokeColor;
	static System::Uitypes::TColor DefaultButtonFillColor;
	__fastcall virtual TTMSFNCGraphics(Vcl::Graphics::TCanvas* ACanvas, bool ANative);
	__fastcall virtual TTMSFNCGraphics(Vcl::Graphics::TCanvas* ACanvas);
	__fastcall virtual TTMSFNCGraphics(int AWidth, int AHeight, bool ANative, bool AHighDPI);
	__fastcall virtual TTMSFNCGraphics(int AWidth, int AHeight);
	__fastcall virtual ~TTMSFNCGraphics();
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix __fastcall GetMatrix();
	void __fastcall StartSpecialPen();
	void __fastcall StopSpecialPen();
	virtual void __fastcall BeginPrinting();
	virtual void __fastcall EndPrinting();
	virtual void __fastcall BeginScene();
	virtual void __fastcall EndScene();
	virtual void __fastcall SetMatrix(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsMatrix &AMatrix);
	virtual void __fastcall Assign(TTMSFNCGraphics* Source);
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
	virtual void __fastcall DrawLine(const System::Types::TPoint &AFromPoint, const System::Types::TPoint &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawLine(const System::Types::TPointF &AFromPoint, const System::Types::TPointF &AToPoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeFrom = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode AModifyPointModeTo = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyPointMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(int ALeft, int ATop, int ARight, int ABottom, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(const System::Types::TRect &ARect, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(double ALeft, double ATop, double ARight, double ABottom, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusRectangle(const System::Types::TRectF &ARect, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawFocusPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x0));
	virtual void __fastcall DrawPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
	virtual void __fastcall DrawPolyline(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolyline);
	virtual void __fastcall DrawPath(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode APathMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathDrawMode)(0x0));
	virtual void __fastcall DrawArc(const System::Types::TPointF &Center, const System::Types::TPointF &Radius, float StartAngle, float SweepAngle)/* overload */;
	virtual void __fastcall DrawArc(const System::Types::TPoint &Center, const System::Types::TPoint &Radius, int StartAngle, int SweepAngle)/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRectangle(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides ASides, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(double ALeft, double ATop, double ARight, double ABottom, float ARounding = 1.000000E+01f, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(const System::Types::TRectF &ARect, float ARounding = 1.000000E+01f, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(int ALeft, int ATop, int ARight, int ABottom, int ARounding = 0xa, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawRoundRectangle(const System::Types::TRect &ARect, int ARounding = 0xa, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners ACorners = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners() << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcTopRight << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomLeft << Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorner::gcBottomRight ), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawEllipse(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawBitmapPart(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapPart(const System::Types::TRectF &ASourceRect, const System::Types::TRectF &ADestinationRect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(double ALeft, double ATop, double ARight, double ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(const System::Types::TRectF &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawEllipse(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode AModifyRectMode = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsModifyRectMode)(0x2))/* overload */;
	virtual void __fastcall DrawBitmapPart(int ASourceLeft, int ASourceTop, int ASourceRight, int ASourceBottom, int ADestinationLeft, int ADestinationTop, int ADestinationRight, int ADestinationBottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapPart(const System::Types::TRect &ASourceRect, const System::Types::TRect &ADestinationRect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(int ALeft, int ATop, int ARight, int ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(const System::Types::TRect &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmap(int ALeft, int ATop, Vcl::Graphics::TPicture* ABitmap)/* overload */;
	virtual void __fastcall DrawBitmap(float ALeft, float ATop, Vcl::Graphics::TPicture* ABitmap)/* overload */;
	virtual void __fastcall DrawCheckBox(const System::Types::TRectF &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCloseButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawExpanderButton(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState AState = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCompactButton(const System::Types::TRectF &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState AState = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawDropDownButton(const System::Types::TRectF &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool ACenter = false, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawRadioButton(const System::Types::TRectF &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawProgressBar(const System::Types::TRectF &ARect, float AValue, System::UnicodeString AFormat = L"%.0f%%", float AMax = 1.000000E+02f, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x32cd9a), System::Uitypes::TColor ATextColor = (System::Uitypes::TColor)(0x0), bool AShowText = true, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCheckBox(const System::Types::TRect &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCloseButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawExpanderButton(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState AState = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsExpanderState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawCompactButton(const System::Types::TRect &ARect, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState AState = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCompactState)(0x1), bool ADown = false, bool AFocused = false, bool AEnabled = true, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawDropDownButton(const System::Types::TRect &ARect, bool ADown = false, bool AFocused = false, bool AEnabled = true, bool ACenter = false, bool AAdaptToStyle = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawRadioButton(const System::Types::TRect &ARect, bool AChecked = false, bool AFocused = false, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawProgressBar(const System::Types::TRect &ARect, float AValue, System::UnicodeString AFormat = L"%.0f%%", float AMax = 1.000000E+02f, System::Uitypes::TColor AColor = (System::Uitypes::TColor)(0x32cd9a), System::Uitypes::TColor ATextColor = (System::Uitypes::TColor)(0x0), bool AShowText = true, bool AEnabled = true, float AScaleFactor = 1.000000E+00f)/* overload */;
	virtual void __fastcall DrawBitmapWithName(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(const System::Types::TRectF &ARect, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(const System::Types::TRectF &ARect, Vcl::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(double ALeft, double ATop, double ARight, double ABottom, Vcl::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawBitmapWithName(const System::Types::TRect &ARect, System::UnicodeString ABitmapName, bool AApplyScale = false, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(const System::Types::TRect &ARect, Vcl::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall DrawScaledBitmap(int ALeft, int ATop, int ARight, int ABottom, Vcl::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRectF __fastcall GetBitmapDrawRectangle(double ALeft, double ATop, double ARight, double ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRectF __fastcall GetBitmapDrawRectangle(const System::Types::TRectF &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRect __fastcall GetBitmapDrawRectangle(int ALeft, int ATop, int ARight, int ABottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual System::Types::TRect __fastcall GetBitmapDrawRectangle(const System::Types::TRect &ARect, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false)/* overload */;
	virtual void __fastcall ClipRect(const System::Types::TRectF &ARect)/* overload */;
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall DrawBitmapPartSync(double ASourceLeft, double ASourceTop, double ASourceRight, double ASourceBottom, double ADestinationLeft, double ADestinationTop, double ADestinationRight, double ADestinationBottom, Vcl::Graphics::TPicture* ABitmap, bool AAspectRatio = true, bool AStretch = false, bool ACenter = true, bool ACropping = false);
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
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRectF &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(double ALeft, double ATop, double ARight, double ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(const System::Types::TRect &ARect, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f, float AY = -1.000000E+00f)/* overload */;
	virtual System::UnicodeString __fastcall DrawText(int ALeft, int ATop, int ARight, int ABottom, System::UnicodeString AText, System::UnicodeString &AControlID, System::UnicodeString &AControlValue, System::UnicodeString &AControlType, bool AWordWrapping = false, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AHorizontalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x1), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign AVerticalAlign = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign)(0x0), Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming ATrimming = (Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextTrimming)(0x0), float AAngle = 0.000000E+00f, float AMinWidth = -1.000000E+00f, float AMinHeight = -1.000000E+00f, bool ASupportHTML = true, bool ATestAnchor = false, float AX = -1.000000E+00f,
		float AY = -1.000000E+00f)/* overload */;
	virtual Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSaveState* __fastcall SaveState(bool ACanvasOnly = false);
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont};
	__property Vcl::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property TTMSFNCGraphicsContext* Context = {read=GetContext};
	__property Vcl::Graphics::TBitmap* Bitmap = {read=FBitmap};
	__property bool OptimizedHTMLDrawing = {read=FOptimizedHTMLDrawing, write=FOptimizedHTMLDrawing, nodefault};
	__property System::Uitypes::TColor HighlightColor = {read=FHighlightColor, write=FHighlightColor, nodefault};
	__property System::Uitypes::TColor HighlightTextColor = {read=FHighlightTextColor, write=FHighlightTextColor, nodefault};
	__property System::Uitypes::TFontStyles HighlightFontStyle = {read=FHighlightFontStyles, write=FHighlightFontStyles, nodefault};
	__property Vcl::Imglist::TCustomImageList* ImageList = {read=FImageList, write=FImageList};
	__property Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=FBitmapContainer};
	__property System::Uitypes::TColor URLColor = {read=FURLColor, write=FURLColor, default=16711680};
	__property bool URLUnderline = {read=FURLUnderline, write=FURLUnderline, default=1};
	__classmethod System::UnicodeString __fastcall ApplyHilight(System::UnicodeString AText, System::UnicodeString AHilight, System::UnicodeString ATag, bool ADoCase);
	__classmethod virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetBitmapFromBitmapContainer(Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer, System::UnicodeString AName, bool AApplyScale = false, float AScale = 0.000000E+00f);
	__classmethod Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetScaledBitmap(Vcl::Tmsfnctypes::TTMSFNCScaledBitmaps* ABitmaps, float AScale = 0.000000E+00f, Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer = (Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer*)(0x0));
	__classmethod virtual void __fastcall GetAspectSize(float &AWidth, float &AHeight, float AOriginalWidth, float AOriginalHeight, float ANewWidth, float ANewHeight, bool AAspectRatio = true, bool AStretch = false, bool ACropping = false);
	__classmethod virtual void __fastcall DrawSample(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRectF &ARect);
	__classmethod virtual void __fastcall SetDefaultGraphicColors();
	__classmethod virtual bool __fastcall PointInCircle(const System::Types::TPointF &APoint, const System::Types::TPointF &ACenter, const float ARadius);
	__classmethod virtual bool __fastcall PointInPath(const System::Types::TPointF &APoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath)/* overload */;
	__classmethod virtual bool __fastcall PointInPath(const System::Types::TPoint &APoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPath* APath)/* overload */;
	__classmethod virtual bool __fastcall PointInPolygon(const System::Types::TPointF &APoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon)/* overload */;
	__classmethod virtual bool __fastcall PointInPolygon(const System::Types::TPoint &APoint, Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon)/* overload */;
	__classmethod virtual bool __fastcall PointInRect(const System::Types::TPointF &APoint, const System::Types::TRectF &ARect)/* overload */;
	__classmethod virtual bool __fastcall PointInRect(const System::Types::TPoint &APoint, const System::Types::TRect &ARect)/* overload */;
	__classmethod virtual System::Byte __fastcall GetColorRed(System::Uitypes::TColor AColor);
	__classmethod virtual System::Byte __fastcall GetColorGreen(System::Uitypes::TColor AColor);
	__classmethod virtual System::Byte __fastcall GetColorBlue(System::Uitypes::TColor AColor);
	__classmethod virtual System::UnicodeString __fastcall ColorToHTML(System::Uitypes::TColor AColor);
	__classmethod virtual System::Uitypes::TColor __fastcall HTMLToColor(System::UnicodeString AHTML);
	__classmethod virtual System::Uitypes::TColor __fastcall TextToColor(System::UnicodeString AText);
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

#pragma pack(pop)

__interface  INTERFACE_UUID("{481CA803-8B50-4545-B212-57AC0D065D09}") ITMSFNCGraphicsExport  : public System::IInterface 
{
	virtual void __fastcall Export(TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncgraphics */
}	/* namespace Vcl */
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
#endif	// Vcl_TmsfncgraphicsHPP
