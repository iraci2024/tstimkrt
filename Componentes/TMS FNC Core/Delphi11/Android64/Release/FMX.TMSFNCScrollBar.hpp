// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCScrollBar.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncscrollbarHPP
#define Fmx_TmsfncscrollbarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
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
	System::Uitypes::TAlphaColor FArrowColor;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftHoverStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftHoverStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightHoverStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonLeftDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonLeftDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonRightHoverStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonRightDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FThumbButtonLeftDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FThumbButtonRightDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FScrollButtonRightDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FScrollButtonLeftFill;
	void __fastcall SetThumbButtonSize(const float Value);
	void __fastcall SetFixedThumb(const bool Value);
	void __fastcall SetArrowColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetScrollButtonLeftDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonLeftFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonLeftHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonLeftStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetScrollButtonRightHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonRightStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetScrollButtonSize(const float Value);
	void __fastcall SetThumbButtonLeftDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonLeftFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonLeftHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonLeftStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbButtonRightHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbButtonRightStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetThumbFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetThumbStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
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
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbFill = {read=FThumbFill, write=SetThumbFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftFill = {read=FScrollButtonLeftFill, write=SetScrollButtonLeftFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightFill = {read=FScrollButtonRightFill, write=SetScrollButtonRightFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftHoverFill = {read=FScrollButtonLeftHoverFill, write=SetScrollButtonLeftHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightHoverFill = {read=FScrollButtonRightHoverFill, write=SetScrollButtonRightHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonLeftDownFill = {read=FScrollButtonLeftDownFill, write=SetScrollButtonLeftDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ScrollButtonRightDownFill = {read=FScrollButtonRightDownFill, write=SetScrollButtonRightDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftFill = {read=FThumbButtonLeftFill, write=SetThumbButtonLeftFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightFill = {read=FThumbButtonRightFill, write=SetThumbButtonRightFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftHoverFill = {read=FThumbButtonLeftHoverFill, write=SetThumbButtonLeftHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightHoverFill = {read=FThumbButtonRightHoverFill, write=SetThumbButtonRightHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonLeftDownFill = {read=FThumbButtonLeftDownFill, write=SetThumbButtonLeftDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ThumbButtonRightDownFill = {read=FThumbButtonRightDownFill, write=SetThumbButtonRightDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbStroke = {read=FThumbStroke, write=SetThumbStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftStroke = {read=FScrollButtonLeftStroke, write=SetScrollButtonLeftStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightStroke = {read=FScrollButtonRightStroke, write=SetScrollButtonRightStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftHoverStroke = {read=FScrollButtonLeftHoverStroke, write=SetScrollButtonLeftHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightHoverStroke = {read=FScrollButtonRightHoverStroke, write=SetScrollButtonRightHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonLeftDownStroke = {read=FScrollButtonLeftDownStroke, write=SetScrollButtonLeftDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ScrollButtonRightDownStroke = {read=FScrollButtonRightDownStroke, write=SetScrollButtonRightDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftStroke = {read=FThumbButtonLeftStroke, write=SetThumbButtonLeftStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightStroke = {read=FThumbButtonRightStroke, write=SetThumbButtonRightStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftHoverStroke = {read=FThumbButtonLeftHoverStroke, write=SetThumbButtonLeftHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightHoverStroke = {read=FThumbButtonRightHoverStroke, write=SetThumbButtonRightHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonLeftDownStroke = {read=FThumbButtonLeftDownStroke, write=SetThumbButtonLeftDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ThumbButtonRightDownStroke = {read=FThumbButtonRightDownStroke, write=SetThumbButtonRightDownStroke};
	__property float ThumbButtonSize = {read=FThumbButtonSize, write=SetThumbButtonSize, stored=IsThumbButtonSizeStored};
	__property float ScrollButtonSize = {read=FScrollButtonSize, write=SetScrollButtonSize, stored=IsScrollButtonSizeStored};
	__property bool FixedThumb = {read=FFixedThumb, write=SetFixedThumb, default=0};
	__property System::Uitypes::TAlphaColor ArrowColor = {read=FArrowColor, write=SetArrowColor, default=-16777216};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
};


enum DECLSPEC_DENUM TTMSFNCScrollBarMode : unsigned char { sbmNone, sbmScroll, sbmChangePageSize };

enum DECLSPEC_DENUM TTMSFNCScrollBarHoveredButton : unsigned char { shbNone, shbScrollMin, shbScrollMax, shbThumbMin, shbThumbMax };

enum DECLSPEC_DENUM TTMSFNCScrollBarDownButton : unsigned char { sdbNone, sdbScrollMin, sdbScrollMax, sdbThumbMin, sdbThumbMax };

enum DECLSPEC_DENUM TTMSFNCScrollButtonChange : unsigned char { sbcNone, sbcSmallSubstract, sbcSmallAdd, sbcLargeSubstract, sbcLargeAdd };

typedef void __fastcall (__closure *TTMSFNCScrollBarValueChanged)(System::TObject* Sender, double Value);

typedef void __fastcall (__closure *TTMSFNCScrollBarPageSizeChanged)(System::TObject* Sender, double PageSize);

enum DECLSPEC_DENUM TTMSFNCScrollBarKind : unsigned char { sbkHorizontal, sbkVertical };

class PASCALIMPLEMENTATION TTMSFNCCustomScrollBar : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
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
	Fmx::Types::TTimer* FTimer;
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
	void __fastcall Changed();
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
	void __fastcall DrawScrollButtons(Fmx::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawThumb(Fmx::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawThumbButtons(Fmx::Tmsfncgraphics::TTMSFNCGraphics* g);
	void __fastcall DrawArrow(Fmx::Tmsfncgraphics::TTMSFNCGraphics* g, const System::Types::TRectF &r, bool ALeft);
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
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
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncscrollbar */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCSCROLLBAR)
using namespace Fmx::Tmsfncscrollbar;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncscrollbarHPP
