// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCustomScrollControl.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccustomscrollcontrolHPP
#define Fmx_TmsfnccustomscrollcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TMSFNCScrollBar.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.ExtCtrls.hpp>
#include <System.Types.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccustomscrollcontrol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomScrollControl;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCCustomScrollControlMode : unsigned char { scmPixelScrolling, scmItemScrolling };

enum DECLSPEC_DENUM TTMSFNCCustomScrollControlUpdate : unsigned char { scuContinuous, scuOnce };

class PASCALIMPLEMENTATION TTMSFNCCustomScrollControl : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	int FDownTime;
	bool FMouseUp;
	bool FAnimateVerticalPos;
	bool FAnimateHorizontalPos;
	bool FAnimating;
	double FSpX;
	double FSpY;
	bool FDoubleSelection;
	bool FRangeSelection;
	bool FScrolling;
	double FScrollX;
	double FScrollY;
	double FDownX;
	double FDownY;
	double FMouseX;
	double FMouseY;
	double FScrollVTo;
	double FScrollHTo;
	double FTimeStart;
	double FTimeStop;
	Fmx::Types::TTimer* FAnimateTimer;
	Fmx::Types::TTimer* FDownTimer;
	float FSaveHScrollPos;
	float FSaveVScrollPos;
	bool FBlockScrollingUpdate;
	bool FIsMouseDown;
	bool FStretchScrollBars;
	int FUpdateCount;
	Fmx::Stdctrls::TScrollBar* FVerticalScrollBar;
	Fmx::Stdctrls::TScrollBar* FHorizontalScrollBar;
	Fmx::Tmsfncscrollbar::TTMSFNCScrollBar* FCustomVerticalScrollBar;
	Fmx::Tmsfncscrollbar::TTMSFNCScrollBar* FCustomHorizontalScrollBar;
	bool FVerticalScrollBarVisible;
	bool FHorizontalScrollBarVisible;
	TTMSFNCCustomScrollControlMode FScrollMode;
	bool FApplyClipRect;
	TTMSFNCCustomScrollControlUpdate FScrollUpdate;
	bool FCustomScrollBars;
	bool FTouchScrolling;
	void __fastcall SetStretchScrollBars(const bool Value);
	void __fastcall SetScrollMode(const TTMSFNCCustomScrollControlMode Value);
	void __fastcall SetApplyClipRect(const bool Value);
	void __fastcall SetScrollUpdate(const TTMSFNCCustomScrollControlUpdate Value);
	void __fastcall SetCustomScrollBars(const bool Value);
	
protected:
	virtual System::UnicodeString __fastcall GetVersion();
	virtual bool __fastcall ColumnStretchingActive() = 0 ;
	virtual void __fastcall SetHorizontalScrollBarVisible(const bool Value);
	virtual void __fastcall SetVerticalScrollBarVisible(const bool Value);
	void __fastcall Animate(System::TObject* Sender);
	void __fastcall DownTime(System::TObject* Sender);
	virtual void __fastcall UpdateControlAfterResize();
	HIDESBASE virtual void __fastcall FillChanged(System::TObject* Sender);
	HIDESBASE virtual void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall Scroll(double AHorizontalPos, double AVerticalPos);
	virtual void __fastcall UpdateControlScroll(double AHorizontalPos, double AVerticalPos, double ANewHorizontalPos, double ANewVerticalPos);
	virtual void __fastcall VerticalScrollPositionChanged();
	virtual void __fastcall HorizontalScrollPositionChanged();
	void __fastcall VScrollChanged(System::TObject* Sender);
	void __fastcall HScrollChanged(System::TObject* Sender);
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleSelection(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall HCustomScrollChanged(System::TObject* Sender, double AValue);
	void __fastcall VCustomScrollChanged(System::TObject* Sender, double AValue);
	virtual void __fastcall UpdateControlScrollBarCalculations();
	void __fastcall UpdateControlScrollBars(bool AUpdate = true, bool ACalculate = true);
	virtual void __fastcall UpdateControl(bool AUpdate = true, bool ARealign = true);
	virtual void __fastcall UpdateControlCache();
	virtual void __fastcall UpdateControlDisplay();
	virtual void __fastcall StopAnimationTimer();
	virtual void __fastcall SetHScrollValue(float AValue);
	virtual void __fastcall SetVScrollValue(float AValue);
	virtual void __fastcall Loaded();
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawContent(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall UpdateScrollingMode(TTMSFNCCustomScrollControlMode AMode);
	virtual double __fastcall GetTotalContentWidth();
	virtual double __fastcall GetTotalContentHeight();
	virtual double __fastcall GetHorizontalContentViewPortSize();
	virtual double __fastcall GetVerticalContentViewPortSize();
	virtual int __fastcall GetHorizontalContentCount();
	virtual int __fastcall GetVerticalContentCount();
	virtual System::Types::TRectF __fastcall GetCalculationRect();
	virtual System::Types::TRectF __fastcall GetContentClipRect();
	virtual System::Types::TRectF __fastcall GetContentRect();
	virtual float __fastcall GetHScrollValue();
	virtual float __fastcall GetVScrollValue();
	virtual float __fastcall GetVViewPortSize();
	virtual float __fastcall GetHViewPortSize();
	virtual float __fastcall GetVMax();
	virtual float __fastcall GetHMax();
	virtual bool __fastcall GetHVisible();
	virtual bool __fastcall GetVVisible();
	virtual bool __fastcall ProcessTouchScrolling(float X, float Y);
	__property bool HorizontalScrollBarVisible = {read=FHorizontalScrollBarVisible, write=SetHorizontalScrollBarVisible, default=1};
	__property bool VerticalScrollBarVisible = {read=FVerticalScrollBarVisible, write=SetVerticalScrollBarVisible, default=1};
	__property int UpdateCount = {read=FUpdateCount, write=FUpdateCount, nodefault};
	__property bool IsMouseDown = {read=FIsMouseDown, write=FIsMouseDown, nodefault};
	__property bool StretchScrollBars = {read=FStretchScrollBars, write=SetStretchScrollBars, default=1};
	__property TTMSFNCCustomScrollControlMode ScrollMode = {read=FScrollMode, write=SetScrollMode, default=0};
	__property TTMSFNCCustomScrollControlUpdate ScrollUpdate = {read=FScrollUpdate, write=SetScrollUpdate, default=0};
	__property bool CustomScrollBars = {read=FCustomScrollBars, write=SetCustomScrollBars, default=0};
	__property System::UnicodeString Version = {read=GetVersion};
	__property bool ApplyClipRect = {read=FApplyClipRect, write=SetApplyClipRect, default=1};
	__property bool TouchScrolling = {read=FTouchScrolling, write=FTouchScrolling, default=0};
	__property bool Animating = {read=FAnimating, nodefault};
	__property bool IsScrolling = {read=FScrolling, nodefault};
	__property bool IsRangeSelection = {read=FRangeSelection, nodefault};
	
public:
	__fastcall virtual TTMSFNCCustomScrollControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomScrollControl();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall SaveScrollPosition();
	virtual void __fastcall RestoreScrollPosition();
	virtual double __fastcall GetVerticalScrollPosition();
	virtual double __fastcall GetHorizontalScrollPosition();
	virtual double __fastcall GetHorizontalScrollMax();
	virtual double __fastcall GetVerticalScrollMax();
	Fmx::Stdctrls::TScrollBar* __fastcall HorizontalScrollBar();
	Fmx::Stdctrls::TScrollBar* __fastcall VerticalScrollBar();
	Fmx::Tmsfncscrollbar::TTMSFNCScrollBar* __fastcall CustomHorizontalScrollBar();
	Fmx::Tmsfncscrollbar::TTMSFNCScrollBar* __fastcall CustomVerticalScrollBar();
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 SCROLLINGDELAY = System::Int8(0x28);
static constexpr System::Word SWIPECOUNT = System::Word(0x12c);
static constexpr System::Int8 DOWNCOUNT = System::Int8(0x64);
}	/* namespace Tmsfnccustomscrollcontrol */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCUSTOMSCROLLCONTROL)
using namespace Fmx::Tmsfnccustomscrollcontrol;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccustomscrollcontrolHPP
