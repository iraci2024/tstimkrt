// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCEditorButton.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnceditorbuttonHPP
#define Fmx_TmsfnceditorbuttonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.AcceleratorKey.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnceditorbutton
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomEditorButton;
class DELPHICLASS TTMSFNCEditorButtonAppearance;
class DELPHICLASS TTMSFNCEditorButton;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCEditorButtonPosition : unsigned char { ebpAlone, ebpLeft, ebpTop, ebpCenter, ebpRight, ebpBottom };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCCustomEditorButton : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FHover;
	bool FDown;
	bool FSelected;
	TTMSFNCEditorButtonAppearance* FAppearance;
	System::UnicodeString FText;
	bool FToggle;
	TTMSFNCEditorButtonPosition FButtonPosition;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners FButtonPositionCorners;
	System::Uitypes::TModalResult FModalResult;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FDisabledBitmap;
	System::UnicodeString FGroupName;
	System::WideChar FAcceleratorChar;
	int FAcceleratorCharPos;
	System::Classes::TNotifyEvent FOnButtonClick;
	Fmx::Tmsfnctypes::TTMSFNCMargins* FBitmapMargins;
	System::Classes::TNotifyEvent FOnHandleAcceleratorKey;
	void __fastcall SetAppearance(TTMSFNCEditorButtonAppearance* const Value);
	void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall SetToggle(const bool Value);
	void __fastcall SetSelected(const bool Value);
	void __fastcall SetButtonPosition(const TTMSFNCEditorButtonPosition Value);
	void __fastcall SetButtonPositionCorners();
	void __fastcall SetBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetDisabledBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetGroupName(const System::UnicodeString Value);
	void __fastcall UpdateOtherGroupButtons();
	void __fastcall SetAcceleratorChar(System::UnicodeString AText);
	void __fastcall SetBitmapMargins(Fmx::Tmsfnctypes::TTMSFNCMargins* const Value);
	
protected:
	virtual void __fastcall ChangeDPIScale(int M, int D);
	void __fastcall SetSelectedByToggle(const bool AValue);
	virtual void __fastcall DoButtonClick();
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseEnter();
	virtual void __fastcall HandleMouseLeave();
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DoAppearanceChanged(System::TObject* Sender);
	HIDESBASE virtual void __fastcall DoBitmapChanged(System::TObject* Sender);
	virtual void __fastcall DoBitmapMarginsChanged(System::TObject* Sender);
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawStandardBackGround(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawSelectionLine(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawButtonText(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawButtonImage(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall UpdateControlAfterResize();
	virtual void __fastcall HandleAcceleratorKey();
	void __fastcall TriggerAcceleratorKey();
	bool __fastcall CanTriggerAcceleratorKey();
	System::WideChar __fastcall GetAcceleratorChar();
	int __fastcall GetAcceleratorCharIndex();
	virtual void __fastcall SetEnabled(const bool Value);
	__property TTMSFNCEditorButtonAppearance* Appearance = {read=FAppearance, write=SetAppearance};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property bool Toggle = {read=FToggle, write=SetToggle, nodefault};
	__property bool Selected = {read=FSelected, write=SetSelected, nodefault};
	__property TTMSFNCEditorButtonPosition ButtonPosition = {read=FButtonPosition, write=SetButtonPosition, default=0};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property Fmx::Tmsfnctypes::TTMSFNCMargins* BitmapMargins = {read=FBitmapMargins, write=SetBitmapMargins};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* DisabledBitmap = {read=FDisabledBitmap, write=SetDisabledBitmap};
	__property System::UnicodeString GroupName = {read=FGroupName, write=SetGroupName};
	__property System::Classes::TNotifyEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	__property System::Classes::TNotifyEvent OnHandleAcceleratorKey = {read=FOnHandleAcceleratorKey, write=FOnHandleAcceleratorKey};
	
public:
	__fastcall virtual TTMSFNCCustomEditorButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomEditorButton();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
private:
	void *__IAcceleratorKeyReceiver;	// Fmx::Acceleratorkey::IAcceleratorKeyReceiver 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {1C679082-65F2-4C54-A2C4-CD4C00E2C465}
	operator Fmx::Acceleratorkey::_di_IAcceleratorKeyReceiver()
	{
		Fmx::Acceleratorkey::_di_IAcceleratorKeyReceiver intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Acceleratorkey::IAcceleratorKeyReceiver*(void) { return (Fmx::Acceleratorkey::IAcceleratorKeyReceiver*)&__IAcceleratorKeyReceiver; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCEditorButtonAppearance : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCCustomEditorButton* FOwner;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FSelectedFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FHoverFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FSelectedStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FDownFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FHoverStroke;
	System::Classes::TNotifyEvent FOnChanged;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FSelectedFont;
	int FRounding;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FTextAlignVertical;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FTextAlignHorizontal;
	bool FSelectionLine;
	float FSelectionLineWidth;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FDisabledFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FDisabledFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FDisabledStroke;
	void __fastcall SetDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetDownFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetHoverFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetSelectedFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetSelectedStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetRounding(const int Value);
	void __fastcall SetTextAlignHorizontal(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetTextAlignVertical(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetSelectionLine(const bool Value);
	void __fastcall SetSelectionLineWidth(const float Value);
	void __fastcall SetDisabledFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetDisabledFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetDisabledStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
protected:
	void __fastcall DoChanged(System::TObject* Sender);
	virtual void __fastcall DoFillChanged(System::TObject* Sender);
	virtual void __fastcall DoFontChanged(System::TObject* Sender);
	virtual void __fastcall DoStrokeChanged(System::TObject* Sender);
	
public:
	__fastcall TTMSFNCEditorButtonAppearance(TTMSFNCCustomEditorButton* AOwner);
	__fastcall virtual ~TTMSFNCEditorButtonAppearance();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* DisabledFill = {read=FDisabledFill, write=SetDisabledFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* DisabledFont = {read=FDisabledFont, write=SetDisabledFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* DisabledStroke = {read=FDisabledStroke, write=SetDisabledStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* HoverFill = {read=FHoverFill, write=SetHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* HoverFont = {read=FHoverFont, write=SetHoverFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* HoverStroke = {read=FHoverStroke, write=SetHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* DownFill = {read=FDownFill, write=SetDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* DownFont = {read=FDownFont, write=SetDownFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* DownStroke = {read=FDownStroke, write=SetDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* SelectedFill = {read=FSelectedFill, write=SetSelectedFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* SelectedFont = {read=FSelectedFont, write=SetSelectedFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* SelectedStroke = {read=FSelectedStroke, write=SetSelectedStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont, write=SetFont};
	__property int Rounding = {read=FRounding, write=SetRounding, nodefault};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign TextAlignHorizontal = {read=FTextAlignHorizontal, write=SetTextAlignHorizontal, default=0};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign TextAlignVertical = {read=FTextAlignVertical, write=SetTextAlignVertical, default=0};
	__property bool SelectionLine = {read=FSelectionLine, write=SetSelectionLine, nodefault};
	__property float SelectionLineWidth = {read=FSelectionLineWidth, write=SetSelectionLineWidth};
	
public:
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCEditorButton : public TTMSFNCCustomEditorButton
{
	typedef TTMSFNCCustomEditorButton inherited;
	
__published:
	__property Appearance;
	__property DisabledBitmap;
	__property GroupName = {default=0};
	__property Text = {default=0};
	__property Toggle;
	__property Selected;
	__property Bitmap;
	__property BitmapMargins;
	__property ButtonPosition = {default=0};
	__property ModalResult = {default=0};
	__property Hint = {default=0};
	__property ShowHint;
	__property OnClick;
	__property OnButtonClick;
	__property OnHandleAcceleratorKey;
public:
	/* TTMSFNCCustomEditorButton.Create */ inline __fastcall virtual TTMSFNCEditorButton(System::Classes::TComponent* AOwner) : TTMSFNCCustomEditorButton(AOwner) { }
	/* TTMSFNCCustomEditorButton.Destroy */ inline __fastcall virtual ~TTMSFNCEditorButton() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorbutton */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCEDITORBUTTON)
using namespace Fmx::Tmsfnceditorbutton;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnceditorbuttonHPP
