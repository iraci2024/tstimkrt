// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPopup.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncpopupHPP
#define Fmx_TmsfncpopupHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ExtCtrls.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpopup
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomPopupForm;
class DELPHICLASS TTMSFNCCustomPopup;
class DELPHICLASS TTMSFNCPopup;
class DELPHICLASS TTMSFNCCustomNonFocusablePopupForm;
class DELPHICLASS TTMSFNCCustomNonFocusablePopup;
class DELPHICLASS TTMSFNCNonFocusablePopup;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCPopupPlacement : unsigned char { ppBottom, ppTop, ppLeft, ppRight, ppCenter, ppBottomCenter, ppTopCenter, ppLeftCenter, ppRightCenter, ppAbsolute, ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter };

typedef void __fastcall (__closure *TTMSFNCPopupPaint)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);

_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomPopupFormClass);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomPopupForm : public Fmx::Forms::TCustomForm
{
	typedef Fmx::Forms::TCustomForm inherited;
	
private:
	int FPreferedDisplayIndex;
	bool FHintWindow;
	Fmx::Types::TTimer* FTimer;
	bool FFirstShow;
	System::Classes::TComponent* FOwner;
	bool FShowModal;
	TTMSFNCPopupPlacement FPlacement;
	TTMSFNCPopupPlacement FRealPlacement;
	Fmx::Controls::TControl* FPlacementControl;
	System::Types::TPointF FOffset;
	System::Types::TSizeF FSize;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FPlacementRectangle;
	System::Types::TRectF FScreenPlacementRect;
	bool FPlacementChanged;
	bool FDragWithParent;
	System::Classes::TNotifyEvent FOnBeforeClose;
	System::Classes::TNotifyEvent FOnBeforeShow;
	System::Types::TRectF FScreenContentRect;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FContentPadding;
	Fmx::Controls::TControl* FContentControl;
	System::Classes::TNotifyEvent FOnRealPlacementChanged;
	TTMSFNCPopupPaint FOnPopupPaint;
	void __fastcall SetOffset(const System::Types::TPointF &Value);
	void __fastcall SetSize(const System::Types::TSizeF &Value);
	void __fastcall TimerProc(System::TObject* Sender);
	void __fastcall SetPlacementRectangle(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetPlacement(const TTMSFNCPopupPlacement Value);
	void __fastcall SetPlacementControl(Fmx::Controls::TControl* const Value);
	void __fastcall SetDragWithParent(const bool Value);
	void __fastcall SetContentPadding(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetContentControl(Fmx::Controls::TControl* const Value);
	TTMSFNCCustomPopup* __fastcall GetPopup();
	
protected:
	virtual void __fastcall DoBeforeShow();
	virtual void __fastcall DoBeforeClose();
	virtual void __fastcall UpdateBounds(const System::Types::TRectF &LRect);
	virtual void __fastcall DoClose(System::Uitypes::TCloseAction &CloseAction);
	virtual void __fastcall DoApplyPlacement();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Updated();
	void __fastcall HandleFocusedControl();
	Fmx::Controls::TControl* __fastcall GetPopupParent();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRealPlacementChanged();
	__property bool DragWithParent = {read=FDragWithParent, write=SetDragWithParent, nodefault};
	__property TTMSFNCCustomPopup* Popup = {read=GetPopup};
	
public:
	__fastcall virtual TTMSFNCCustomPopupForm(System::Classes::TComponent* AOwner, NativeInt Dummy);
	__fastcall TTMSFNCCustomPopupForm(System::Classes::TComponent* AOwner, Fmx::Controls::TControl* APlacementControl);
	__fastcall virtual ~TTMSFNCCustomPopupForm();
	virtual void __fastcall ApplyPlacement();
	virtual void __fastcall KeyDown(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual bool __fastcall CloseQuery();
	__property bool HintWindow = {read=FHintWindow, write=FHintWindow, nodefault};
	__property Fmx::Controls::TControl* ContentControl = {read=FContentControl, write=SetContentControl};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* ContentPadding = {read=FContentPadding, write=SetContentPadding};
	__property System::Types::TPointF Offset = {read=FOffset, write=SetOffset};
	__property TTMSFNCPopupPlacement Placement = {read=FPlacement, write=SetPlacement, nodefault};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PlacementRectangle = {read=FPlacementRectangle, write=SetPlacementRectangle};
	__property Fmx::Controls::TControl* PlacementControl = {read=FPlacementControl, write=SetPlacementControl};
	__property TTMSFNCPopupPlacement RealPlacement = {read=FRealPlacement, nodefault};
	__property System::Types::TRectF ScreenContentRect = {read=FScreenContentRect};
	__property System::Types::TRectF ScreenPlacementRect = {read=FScreenPlacementRect};
	__property System::Types::TSizeF Size = {read=FSize, write=SetSize};
	__property System::Classes::TNotifyEvent OnBeforeShow = {read=FOnBeforeShow, write=FOnBeforeShow};
	__property System::Classes::TNotifyEvent OnBeforeClose = {read=FOnBeforeClose, write=FOnBeforeClose};
	__property System::Classes::TNotifyEvent OnRealPlacementChanged = {read=FOnRealPlacementChanged, write=FOnRealPlacementChanged};
	__property TTMSFNCPopupPaint OnPopupPaint = {read=FOnPopupPaint, write=FOnPopupPaint};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomPopup : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	unsigned FCloseTime;
	bool FCheckTime;
	bool FDestroyingPopup;
	Fmx::Controls::TPopup* FPopup;
	System::Classes::TComponent* FOwner;
	System::Classes::TComponent* FCustomOwner;
	Fmx::Controls::TControl* FPlacementControl;
	TTMSFNCCustomPopupForm* FPopupForm;
	bool FStaysOpen;
	TTMSFNCPopupPlacement FPlacement;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FPlacementRectangle;
	float FHorizontalOffset;
	float FVerticalOffset;
	bool FDragWithParent;
	System::Uitypes::TModalResult FModalResult;
	bool FModal;
	System::Classes::TNotifyEvent FOnClosePopup;
	System::Classes::TNotifyEvent FOnPopup;
	System::Types::TSizeF FPopupFormSize;
	Fmx::Controls::TControl* FContentControl;
	float FDropDownHeight;
	float FDropDownWidth;
	bool FFocusable;
	Fmx::Controls::TControl* FFocusedControl;
	TTMSFNCPopupPaint FOnPopupPaint;
	System::Classes::TNotifyEvent FOnPopupShown;
	bool FIsOpen;
	System::Uitypes::TAlphaColor FFillColor;
	System::Uitypes::TAlphaColor FStrokeColor;
	void __fastcall SetIsOpen(const bool Value);
	void __fastcall SetPlacementRectangle(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetModalResult(const System::Uitypes::TModalResult Value);
	void __fastcall SetPlacementControl(Fmx::Controls::TControl* const Value);
	void __fastcall SetPlacement(const TTMSFNCPopupPlacement Value);
	void __fastcall SetDragWithParent(const bool Value);
	void __fastcall BeforeShowProc(System::TObject* Sender);
	void __fastcall BeforeCloseProc(System::TObject* Sender);
	void __fastcall CloseProc(System::TObject* Sender, System::Uitypes::TCloseAction &CloseAction);
	void __fastcall ShowProc(System::TObject* Sender);
	void __fastcall DeactivateProc(System::TObject* Sender);
	void __fastcall SetPopupFormSize(const System::Types::TSizeF &Value);
	void __fastcall UpdatePopupSize();
	void __fastcall SetContentControl(Fmx::Controls::TControl* const Value);
	void __fastcall SetDropDownHeight(const float Value);
	void __fastcall SetDropDownWidth(const float Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall ShowPopup(bool AModal);
	void __fastcall HandleFocusedControl();
	void __fastcall FormPaint(System::TObject* Sender, Fmx::Graphics::TCanvas* Canvas, const System::Types::TRectF &ARect);
	virtual void __fastcall DoPopupPaint(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall MarginsChanged(System::TObject* Sender);
	virtual void __fastcall DoClosePopup();
	virtual void __fastcall DoPopup();
	virtual void __fastcall DoPopupShown();
	virtual void __fastcall ClosePopup();
	virtual void __fastcall DoCreatePopup(const bool AShowModal = false);
	virtual TTMSFNCCustomPopupFormClass __fastcall GetPopupFormClass();
	HIDESBASE Fmx::Controls::TControl* __fastcall GetParent();
	virtual TTMSFNCCustomPopupForm* __fastcall CreatePopupForm();
	__property bool IsOpen = {read=FIsOpen, write=SetIsOpen, nodefault};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=SetModalResult, nodefault};
	__property System::Types::TSizeF PopupFormSize = {read=FPopupFormSize, write=SetPopupFormSize};
	__property bool DragWithParent = {read=FDragWithParent, write=SetDragWithParent, default=0};
	__property TTMSFNCCustomPopupForm* PopupForm = {read=FPopupForm};
	__property bool StaysOpen = {read=FStaysOpen, write=FStaysOpen, default=0};
	__property float DropDownHeight = {read=FDropDownHeight, write=SetDropDownHeight};
	__property float DropDownWidth = {read=FDropDownWidth, write=SetDropDownWidth};
	__property float HorizontalOffset = {read=FHorizontalOffset, write=FHorizontalOffset};
	__property TTMSFNCPopupPlacement Placement = {read=FPlacement, write=SetPlacement, default=0};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* PlacementRectangle = {read=FPlacementRectangle, write=SetPlacementRectangle};
	__property Fmx::Controls::TControl* PlacementControl = {read=FPlacementControl, write=SetPlacementControl};
	__property float VerticalOffset = {read=FVerticalOffset, write=FVerticalOffset};
	__property Fmx::Controls::TControl* FocusedControl = {read=FFocusedControl, write=FFocusedControl};
	__property Fmx::Controls::TControl* ContentControl = {read=FContentControl, write=SetContentControl};
	__property System::Classes::TNotifyEvent OnClosePopup = {read=FOnClosePopup, write=FOnClosePopup};
	__property TTMSFNCPopupPaint OnPopupPaint = {read=FOnPopupPaint, write=FOnPopupPaint};
	__property System::Classes::TNotifyEvent OnPopup = {read=FOnPopup, write=FOnPopup};
	__property System::Classes::TComponent* CustomOwner = {read=FCustomOwner, write=FCustomOwner};
	__property System::Classes::TNotifyEvent OnPopupShown = {read=FOnPopupShown, write=FOnPopupShown};
	__property System::UnicodeString Version = {read=GetVersion};
	
public:
	__fastcall virtual TTMSFNCCustomPopup(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCCustomPopup();
	bool __fastcall HasPopupForm();
	virtual bool __fastcall PointInPopup(const System::Types::TPointF &APoint);
	__property System::Uitypes::TAlphaColor FillColor = {read=FFillColor, write=FFillColor, default=0};
	__property System::Uitypes::TAlphaColor StrokeColor = {read=FStrokeColor, write=FStrokeColor, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPopup : public TTMSFNCCustomPopup
{
	typedef TTMSFNCCustomPopup inherited;
	
protected:
	virtual void __fastcall RegisterRuntimeClasses();
	
public:
	virtual System::Uitypes::TModalResult __fastcall PopupModal();
	virtual void __fastcall Popup(const bool AShowModal = false);
	__property AdaptToStyle = {default=0};
	__property ModalResult;
	__property IsOpen;
	__property PopupFormSize;
	__property DragWithParent = {default=0};
	__property PopupForm;
	
__published:
	__property StaysOpen = {default=0};
	__property DropDownHeight = {default=0};
	__property DropDownWidth = {default=0};
	__property HorizontalOffset = {default=0};
	__property Placement = {default=0};
	__property PlacementRectangle;
	__property PlacementControl;
	__property VerticalOffset = {default=0};
	__property FocusedControl;
	__property ContentControl;
	__property OnClosePopup;
	__property OnPopupPaint;
	__property OnPopup;
	__property OnPopupShown;
	__property Version = {default=0};
public:
	/* TTMSFNCCustomPopup.Create */ inline __fastcall virtual TTMSFNCPopup(System::Classes::TComponent* AOwner) : TTMSFNCCustomPopup(AOwner) { }
	/* TTMSFNCCustomPopup.Destroy */ inline __fastcall virtual ~TTMSFNCPopup() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomNonFocusablePopupForm : public TTMSFNCCustomPopupForm
{
	typedef TTMSFNCCustomPopupForm inherited;
	
protected:
	virtual void __fastcall UpdateBounds(const System::Types::TRectF &LRect);
public:
	/* TTMSFNCCustomPopupForm.CreateNew */ inline __fastcall virtual TTMSFNCCustomNonFocusablePopupForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : TTMSFNCCustomPopupForm(AOwner, Dummy) { }
	/* TTMSFNCCustomPopupForm.Create */ inline __fastcall TTMSFNCCustomNonFocusablePopupForm(System::Classes::TComponent* AOwner, Fmx::Controls::TControl* APlacementControl) : TTMSFNCCustomPopupForm(AOwner, APlacementControl) { }
	/* TTMSFNCCustomPopupForm.Destroy */ inline __fastcall virtual ~TTMSFNCCustomNonFocusablePopupForm() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomNonFocusablePopup : public TTMSFNCCustomPopup
{
	typedef TTMSFNCCustomPopup inherited;
	
protected:
	virtual void __fastcall ActivatePreviousWindow();
	virtual void __fastcall ShowPopup(bool AModal);
	virtual TTMSFNCCustomPopupFormClass __fastcall GetPopupFormClass();
	
public:
	__fastcall virtual TTMSFNCCustomNonFocusablePopup(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomNonFocusablePopup();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCNonFocusablePopup : public TTMSFNCCustomNonFocusablePopup
{
	typedef TTMSFNCCustomNonFocusablePopup inherited;
	
public:
	virtual void __fastcall Deactivate();
	virtual void __fastcall Activate();
	
__published:
	__property ContentControl;
	__property Placement = {default=0};
	__property PlacementRectangle;
	__property PlacementControl;
public:
	/* TTMSFNCCustomNonFocusablePopup.Create */ inline __fastcall virtual TTMSFNCNonFocusablePopup(System::Classes::TComponent* AOwner) : TTMSFNCCustomNonFocusablePopup(AOwner) { }
	/* TTMSFNCCustomNonFocusablePopup.Destroy */ inline __fastcall virtual ~TTMSFNCNonFocusablePopup() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x5);
}	/* namespace Tmsfncpopup */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPOPUP)
using namespace Fmx::Tmsfncpopup;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpopupHPP
