// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCPopup.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncpopupHPP
#define Vcl_TmsfncpopupHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Winapi.Messages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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

typedef void __fastcall (__closure *TTMSFNCPopupPaint)(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);

_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomPopupFormClass);

class PASCALIMPLEMENTATION TTMSFNCCustomPopupForm : public Vcl::Forms::TCustomForm
{
	typedef Vcl::Forms::TCustomForm inherited;
	
private:
	int FPreferedDisplayIndex;
	bool FHintWindow;
	Vcl::Extctrls::TTimer* FTimer;
	bool FFirstShow;
	System::Classes::TComponent* FOwner;
	bool FShowModal;
	TTMSFNCPopupPlacement FPlacement;
	TTMSFNCPopupPlacement FRealPlacement;
	Vcl::Controls::TControl* FPlacementControl;
	System::Types::TPointF FOffset;
	System::Types::TSizeF FSize;
	Vcl::Tmsfnctypes::TTMSFNCMargins* FPlacementRectangle;
	System::Types::TRectF FScreenPlacementRect;
	bool FPlacementChanged;
	bool FDragWithParent;
	System::Classes::TNotifyEvent FOnBeforeClose;
	System::Classes::TNotifyEvent FOnBeforeShow;
	System::Types::TRectF FScreenContentRect;
	Vcl::Tmsfnctypes::TTMSFNCMargins* FContentPadding;
	Vcl::Controls::TControl* FContentControl;
	System::Classes::TNotifyEvent FOnRealPlacementChanged;
	TTMSFNCPopupPaint FOnPopupPaint;
	void __fastcall SetOffset(const System::Types::TPointF &Value);
	void __fastcall SetSize(const System::Types::TSizeF &Value);
	void __fastcall TimerProc(System::TObject* Sender);
	void __fastcall SetPlacementRectangle(Vcl::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetPlacement(const TTMSFNCPopupPlacement Value);
	void __fastcall SetPlacementControl(Vcl::Controls::TControl* const Value);
	void __fastcall SetDragWithParent(const bool Value);
	void __fastcall SetContentPadding(Vcl::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetContentControl(Vcl::Controls::TControl* const Value);
	TTMSFNCCustomPopup* __fastcall GetPopup();
	
protected:
	virtual void __fastcall DoBeforeShow();
	virtual void __fastcall DoBeforeClose();
	HIDESBASE virtual void __fastcall UpdateBounds(const System::Types::TRectF &LRect);
	DYNAMIC void __fastcall DoClose(System::Uitypes::TCloseAction &CloseAction);
	virtual void __fastcall DoApplyPlacement();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Updated();
	void __fastcall HandleFocusedControl();
	Vcl::Controls::TControl* __fastcall GetPopupParent();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRealPlacementChanged();
	__property bool DragWithParent = {read=FDragWithParent, write=SetDragWithParent, nodefault};
	__property TTMSFNCCustomPopup* Popup = {read=GetPopup};
	
public:
	__fastcall virtual TTMSFNCCustomPopupForm(System::Classes::TComponent* AOwner, int Dummy);
	__fastcall TTMSFNCCustomPopupForm(System::Classes::TComponent* AOwner, Vcl::Controls::TControl* APlacementControl);
	__fastcall virtual ~TTMSFNCCustomPopupForm();
	virtual void __fastcall ApplyPlacement();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual bool __fastcall CloseQuery();
	__property bool HintWindow = {read=FHintWindow, write=FHintWindow, nodefault};
	__property Vcl::Controls::TControl* ContentControl = {read=FContentControl, write=SetContentControl};
	__property Vcl::Tmsfnctypes::TTMSFNCMargins* ContentPadding = {read=FContentPadding, write=SetContentPadding};
	__property System::Types::TPointF Offset = {read=FOffset, write=SetOffset};
	__property TTMSFNCPopupPlacement Placement = {read=FPlacement, write=SetPlacement, nodefault};
	__property Vcl::Tmsfnctypes::TTMSFNCMargins* PlacementRectangle = {read=FPlacementRectangle, write=SetPlacementRectangle};
	__property Vcl::Controls::TControl* PlacementControl = {read=FPlacementControl, write=SetPlacementControl};
	__property TTMSFNCPopupPlacement RealPlacement = {read=FRealPlacement, nodefault};
	__property System::Types::TRectF ScreenContentRect = {read=FScreenContentRect};
	__property System::Types::TRectF ScreenPlacementRect = {read=FScreenPlacementRect};
	__property System::Types::TSizeF Size = {read=FSize, write=SetSize};
	__property System::Classes::TNotifyEvent OnBeforeShow = {read=FOnBeforeShow, write=FOnBeforeShow};
	__property System::Classes::TNotifyEvent OnBeforeClose = {read=FOnBeforeClose, write=FOnBeforeClose};
	__property System::Classes::TNotifyEvent OnRealPlacementChanged = {read=FOnRealPlacementChanged, write=FOnRealPlacementChanged};
	__property TTMSFNCPopupPaint OnPopupPaint = {read=FOnPopupPaint, write=FOnPopupPaint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomPopupForm(HWND ParentWindow) : Vcl::Forms::TCustomForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomPopup : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	unsigned FCloseTime;
	bool FCheckTime;
	bool FDestroyingPopup;
	System::Classes::TComponent* FOwner;
	System::Classes::TComponent* FCustomOwner;
	Vcl::Controls::TControl* FPlacementControl;
	TTMSFNCCustomPopupForm* FPopupForm;
	bool FStaysOpen;
	TTMSFNCPopupPlacement FPlacement;
	Vcl::Tmsfnctypes::TTMSFNCMargins* FPlacementRectangle;
	float FHorizontalOffset;
	float FVerticalOffset;
	bool FDragWithParent;
	System::Uitypes::TModalResult FModalResult;
	bool FModal;
	System::Classes::TNotifyEvent FOnClosePopup;
	System::Classes::TNotifyEvent FOnPopup;
	System::Types::TSizeF FPopupFormSize;
	Vcl::Controls::TControl* FContentControl;
	float FDropDownHeight;
	float FDropDownWidth;
	bool FFocusable;
	Vcl::Controls::TControl* FFocusedControl;
	TTMSFNCPopupPaint FOnPopupPaint;
	System::Classes::TNotifyEvent FOnPopupShown;
	bool FIsOpen;
	System::Uitypes::TColor FFillColor;
	System::Uitypes::TColor FStrokeColor;
	void __fastcall SetIsOpen(const bool Value);
	void __fastcall SetPlacementRectangle(Vcl::Tmsfnctypes::TTMSFNCMargins* const Value);
	void __fastcall SetModalResult(const System::Uitypes::TModalResult Value);
	void __fastcall SetPlacementControl(Vcl::Controls::TControl* const Value);
	void __fastcall SetPlacement(const TTMSFNCPopupPlacement Value);
	void __fastcall SetDragWithParent(const bool Value);
	void __fastcall BeforeShowProc(System::TObject* Sender);
	void __fastcall BeforeCloseProc(System::TObject* Sender);
	void __fastcall CloseProc(System::TObject* Sender, System::Uitypes::TCloseAction &CloseAction);
	void __fastcall ShowProc(System::TObject* Sender);
	void __fastcall DeactivateProc(System::TObject* Sender);
	void __fastcall SetPopupFormSize(const System::Types::TSizeF &Value);
	void __fastcall UpdatePopupSize();
	void __fastcall SetContentControl(Vcl::Controls::TControl* const Value);
	void __fastcall SetDropDownHeight(const float Value);
	void __fastcall SetDropDownWidth(const float Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall ShowPopup(bool AModal);
	void __fastcall HandleFocusedControl();
	void __fastcall FormPaint(System::TObject* Sender);
	virtual void __fastcall DoPopupPaint(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall MarginsChanged(System::TObject* Sender);
	virtual void __fastcall DoClosePopup();
	virtual void __fastcall DoPopup();
	virtual void __fastcall DoPopupShown();
	virtual void __fastcall ClosePopup();
	virtual void __fastcall DoCreatePopup(const bool AShowModal = false);
	virtual TTMSFNCCustomPopupFormClass __fastcall GetPopupFormClass();
	Vcl::Controls::TControl* __fastcall GetParent();
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
	__property Vcl::Tmsfnctypes::TTMSFNCMargins* PlacementRectangle = {read=FPlacementRectangle, write=SetPlacementRectangle};
	__property Vcl::Controls::TControl* PlacementControl = {read=FPlacementControl, write=SetPlacementControl};
	__property float VerticalOffset = {read=FVerticalOffset, write=FVerticalOffset};
	__property Vcl::Controls::TControl* FocusedControl = {read=FFocusedControl, write=FFocusedControl};
	__property Vcl::Controls::TControl* ContentControl = {read=FContentControl, write=SetContentControl};
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
	__property System::Uitypes::TColor FillColor = {read=FFillColor, write=FFillColor, default=-1};
	__property System::Uitypes::TColor StrokeColor = {read=FStrokeColor, write=FStrokeColor, default=-1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomPopup(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCPopup(HWND ParentWindow) : TTMSFNCCustomPopup(ParentWindow) { }
	
};


typedef NativeInt LInteger;

typedef void * IntPtr;

class PASCALIMPLEMENTATION TTMSFNCCustomNonFocusablePopupForm : public TTMSFNCCustomPopupForm
{
	typedef TTMSFNCCustomPopupForm inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall WMActivate(Winapi::Messages::TWMActivate &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	
protected:
	virtual void __fastcall UpdateBounds(const System::Types::TRectF &LRect);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	
public:
	__fastcall virtual TTMSFNCCustomNonFocusablePopupForm(System::Classes::TComponent* AOwner, int Dummy);
	__fastcall virtual ~TTMSFNCCustomNonFocusablePopupForm();
public:
	/* TTMSFNCCustomPopupForm.Create */ inline __fastcall TTMSFNCCustomNonFocusablePopupForm(System::Classes::TComponent* AOwner, Vcl::Controls::TControl* APlacementControl) : TTMSFNCCustomPopupForm(AOwner, APlacementControl) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomNonFocusablePopupForm(HWND ParentWindow) : TTMSFNCCustomPopupForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomNonFocusablePopup : public TTMSFNCCustomPopup
{
	typedef TTMSFNCCustomPopup inherited;
	
private:
	HWND FActiveWindow;
	
protected:
	virtual void __fastcall ActivatePreviousWindow();
	virtual void __fastcall ShowPopup(bool AModal);
	virtual TTMSFNCCustomPopupFormClass __fastcall GetPopupFormClass();
	
public:
	__fastcall virtual TTMSFNCCustomNonFocusablePopup(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomNonFocusablePopup();
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomNonFocusablePopup(HWND ParentWindow) : TTMSFNCCustomPopup(ParentWindow) { }
	
};


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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCNonFocusablePopup(HWND ParentWindow) : TTMSFNCCustomNonFocusablePopup(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x5);
}	/* namespace Tmsfncpopup */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCPOPUP)
using namespace Vcl::Tmsfncpopup;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncpopupHPP
