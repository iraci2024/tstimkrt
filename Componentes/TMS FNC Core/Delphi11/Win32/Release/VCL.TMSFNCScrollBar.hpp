// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCScrollBar.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncscrollbarHPP
#define Vcl_TmsfncscrollbarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncscrollbar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCScrollBarAppearance;
class DELPHICLASS TTMSFNCCustomScrollBar;
class DELPHICLASS TTMSFNCScrollBar;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCScrollBarAppearance : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCCustomScrollBar* FOwner;
	float FScrollButtonSize;
	float FThumbButtonSize;
	System::Classes::TNotifyEvent FOnChange;
	bool FFixedThumb;
	System::Uitypes::TColor FArrowColor;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftHoverStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftHoverStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightHoverStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightHoverStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftFill;
	void __fastcall SetThumbButtonSize(const float Value);
	void __fastcall SetFixedThumb(const bool Value);
	void __fastcall SetArrowColor(const System::Uitypes::TColor Value);
	void __fastcall SetScrollButtonLeftDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonLeftFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonLeftStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonSize(const float Value);
	void __fastcall SetThumbButtonLeftDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonLeftFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonLeftStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	bool __fastcall IsScrollButtonSizeStored();
	bool __fastcall IsThumbButtonSizeStored();
	
protected:
	void __fastcall Changed();
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	
public:
	__fastcall TTMSFNCScrollBarAppearance(TTMSFNCCustomScrollBar* AOwner);
	__fastcall virtual ~TTMSFNCScrollBarAppearance();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	float __fastcall GetThumbButtonSize();
	
__published:
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbFill = {read=FThumbFill, write=SetThumbFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftFill = {read=FScrollButtonLeftFill, write=SetScrollButtonLeftFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightFill = {read=FScrollButtonRightFill, write=SetScrollButtonRightFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftHoverFill = {read=FScrollButtonLeftHoverFill, write=SetScrollButtonLeftHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightHoverFill = {read=FScrollButtonRightHoverFill, write=SetScrollButtonRightHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftDownFill = {read=FScrollButtonLeftDownFill, write=SetScrollButtonLeftDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightDownFill = {read=FScrollButtonRightDownFill, write=SetScrollButtonRightDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftFill = {read=FThumbButtonLeftFill, write=SetThumbButtonLeftFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightFill = {read=FThumbButtonRightFill, write=SetThumbButtonRightFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftHoverFill = {read=FThumbButtonLeftHoverFill, write=SetThumbButtonLeftHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightHoverFill = {read=FThumbButtonRightHoverFill, write=SetThumbButtonRightHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftDownFill = {read=FThumbButtonLeftDownFill, write=SetThumbButtonLeftDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightDownFill = {read=FThumbButtonRightDownFill, write=SetThumbButtonRightDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbStroke = {read=FThumbStroke, write=SetThumbStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftStroke = {read=FScrollButtonLeftStroke, write=SetScrollButtonLeftStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightStroke = {read=FScrollButtonRightStroke, write=SetScrollButtonRightStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftHoverStroke = {read=FScrollButtonLeftHoverStroke, write=SetScrollButtonLeftHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightHoverStroke = {read=FScrollButtonRightHoverStroke, write=SetScrollButtonRightHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftDownStroke = {read=FScrollButtonLeftDownStroke, write=SetScrollButtonLeftDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightDownStroke = {read=FScrollButtonRightDownStroke, write=SetScrollButtonRightDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftStroke = {read=FThumbButtonLeftStroke, write=SetThumbButtonLeftStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightStroke = {read=FThumbButtonRightStroke, write=SetThumbButtonRightStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftHoverStroke = {read=FThumbButtonLeftHoverStroke, write=SetThumbButtonLeftHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightHoverStroke = {read=FThumbButtonRightHoverStroke, write=SetThumbButtonRightHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftDownStroke = {read=FThumbButtonLeftDownStroke, write=SetThumbButtonLeftDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightDownStroke = {read=FThumbButtonRightDownStroke, write=SetThumbButtonRightDownStroke};
	__property float ThumbButtonSize = {read=FThumbButtonSize, write=SetThumbButtonSize, stored=IsThumbButtonSizeStored};
	__property float ScrollButtonSize = {read=FScrollButtonSize, write=SetScrollButtonSize, stored=IsScrollButtonSizeStored};
	__property bool FixedThumb = {read=FFixedThumb, write=SetFixedThumb, default=0};
	__property System::Uitypes::TColor ArrowColor = {read=FArrowColor, write=SetArrowColor, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
};


enum DECLSPEC_DENUM TTMSFNCScrollBarMode : unsigned char { sbmNone, sbmScroll, sbmChangePageSize };

enum DECLSPEC_DENUM TTMSFNCScrollBarHoveredButton : unsigned char { shbNone, shbScrollMin, shbScrollMax, shbThumbMin, shbThumbMax };

enum DECLSPEC_DENUM TTMSFNCScrollBarDownButton : unsigned char { sdbNone, sdbScrollMin, sdbScrollMax, sdbThumbMin, sdbThumbMax };

enum DECLSPEC_DENUM TTMSFNCScrollButtonChange : unsigned char { sbcNone, sbcSmallSubstract, sbcSmallAdd, sbcLargeSubstract, sbcLargeAdd };

typedef void __fastcall (__closure *TTMSFNCScrollBarValueChanged)(System::TObject* Sender, double Value);

typedef void __fastcall (__closure *TTMSFNCScrollBarPageSizeChanged)(System::TObject* Sender, double PageSize);

enum DECLSPEC_DENUM TTMSFNCScrollBarKind : unsigned char { sbkHorizontal, sbkVertical };

class PASCALIMPLEMENTATION TTMSFNCCustomScrollBar : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FNeedsChange;
	double FSavedPageSize;
	bool FDesignTime;
	bool FMinThumb;
	bool FMaxThumb;
	double FMx;
	double FMy;
	double FCx;
	double FCy;
	Vcl::Extctrls::TTimer* FTimer;
	int FTime;
	TTMSFNCScrollBarMode FScrollBarMode;
	TTMSFNCScrollButtonChange FScrollButtonChange;
	TTMSFNCScrollBarHoveredButton FHoveredButton;
	TTMSFNCScrollBarDownButton FDownButton;
	TTMSFNCScrollBarKind FKind;
	double FValue;
	double FTempValue;
	double FMin;
	double FMax;
	double FPageSize;
	double FSmallChange;
	double FLargeChange;
	TTMSFNCScrollBarAppearance* FAppearance;
	TTMSFNCScrollBarValueChanged FOnValueChange;
	TTMSFNCScrollBarPageSizeChanged FOnPageSizeChanged;
	bool FTracking;
	void __fastcall SetAppearance(TTMSFNCScrollBarAppearance* const Value);
	void __fastcall SetKind(TTMSFNCScrollBarKind Value);
	void __fastcall SetMax(const double Value);
	void __fastcall SetMin(const double Value);
	void __fastcall SetValue(const double Value);
	void __fastcall SetPageSize(const double Value);
	bool __fastcall IsLargeChangeStored();
	bool __fastcall IsMaxStored();
	bool __fastcall IsMinStored();
	bool __fastcall IsPageSizeStored();
	bool __fastcall IsSmallChangeStored();
	bool __fastcall IsValueStored();
	void __fastcall SetTracking(const bool Value);
	
protected:
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall ApplyStyle();
	virtual void __fastcall ResetToDefaultStyle();
	virtual void __fastcall HandleKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseLeave();
	virtual void __fastcall HandleMouseEnter();
	virtual void __fastcall HandleMouseWheel(System::Classes::TShiftState Shift, int WheelDelta, bool &Handled);
	HIDESBASE void __fastcall Changed();
	void __fastcall AppearanceChanged(System::TObject* Sender);
	void __fastcall TimerChanged(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDocURL();
	System::Types::TRectF __fastcall GetMinScrollButton();
	System::Types::TRectF __fastcall GetMaxScrollButton();
	System::Types::TRectF __fastcall GetMinThumbButton();
	System::Types::TRectF __fastcall GetMaxThumbButton();
	System::Types::TRectF __fastcall GetScrollRectangle();
	double __fastcall GetScrollSize();
	System::Types::TRectF __fastcall GetScrollAreaMin();
	System::Types::TRectF __fastcall GetScrollAreaMax();
	System::Types::TRectF __fastcall GetThumbRectangle();
	double __fastcall GetValue()/* overload */;
	double __fastcall GetValue(double XYPos)/* overload */;
	double __fastcall GetRange();
	bool __fastcall MouseOnThumbButtons(double X, double Y);
	bool __fastcall MouseOnThumb(double X, double Y);
	void __fastcall DrawScrollButtons(Vcl::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawThumb(Vcl::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawThumbButtons(Vcl::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawArrow(Vcl::Tmsfncgraphics::TTMSFNCGraphics* g, const System::Types::TRectF &r, bool ALeft);
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	__property TTMSFNCScrollBarAppearance* Appearance = {read=FAppearance, write=SetAppearance};
	__property TTMSFNCScrollBarKind Kind = {read=FKind, write=SetKind, default=1};
	__property double LargeChange = {read=FLargeChange, write=FLargeChange, stored=IsLargeChangeStored};
	__property double Max = {read=FMax, write=SetMax, stored=IsMaxStored};
	__property double Min = {read=FMin, write=SetMin, stored=IsMinStored};
	__property double PageSize = {read=FPageSize, write=SetPageSize, stored=IsPageSizeStored};
	__property double Value = {read=FValue, write=SetValue, stored=IsValueStored};
	__property double SmallChange = {read=FSmallChange, write=FSmallChange, stored=IsSmallChangeStored};
	__property bool Tracking = {read=FTracking, write=SetTracking, default=1};
	__property TTMSFNCScrollBarValueChanged OnValueChanged = {read=FOnValueChange, write=FOnValueChange};
	__property TTMSFNCScrollBarPageSizeChanged OnPageSizeChanged = {read=FOnPageSizeChanged, write=FOnPageSizeChanged};
	
public:
	__fastcall virtual TTMSFNCCustomScrollBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomScrollBar();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomScrollBar(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCScrollBar : public TTMSFNCCustomScrollBar
{
	typedef TTMSFNCCustomScrollBar inherited;
	
protected:
	virtual void __fastcall RegisterRuntimeClasses();
	
__published:
	__property Fill;
	__property Stroke;
	__property Appearance;
	__property Kind = {default=1};
	__property LargeChange;
	__property Max;
	__property Tracking = {default=1};
	__property Min;
	__property PageSize;
	__property Value;
	__property SmallChange;
	__property OnValueChanged;
	__property OnPageSizeChanged;
public:
	/* TTMSFNCCustomScrollBar.Create */ inline __fastcall virtual TTMSFNCScrollBar(System::Classes::TComponent* AOwner) : TTMSFNCCustomScrollBar(AOwner) { }
	/* TTMSFNCCustomScrollBar.Destroy */ inline __fastcall virtual ~TTMSFNCScrollBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCScrollBar(HWND ParentWindow) : TTMSFNCCustomScrollBar(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncscrollbar */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCSCROLLBAR)
using namespace Vcl::Tmsfncscrollbar;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncscrollbarHPP
