// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCEditorButton.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnceditorbuttonHPP
#define Vcl_TmsfnceditorbuttonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <System.UITypes.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.ActnList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfnceditorbutton
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomEditorButton;
class DELPHICLASS TTMSFNCEditorButtonAppearance;
class DELPHICLASS TTMSFNCEditorButton;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCEditorButtonPosition : unsigned char { ebpAlone, ebpLeft, ebpTop, ebpCenter, ebpRight, ebpBottom };

class PASCALIMPLEMENTATION TTMSFNCCustomEditorButton : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FHover;
	bool FDown;
	bool FSelected;
	TTMSFNCEditorButtonAppearance* FAppearance;
	System::UnicodeString FText;
	bool FToggle;
	TTMSFNCEditorButtonPosition FButtonPosition;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners FButtonPositionCorners;
	System::Uitypes::TModalResult FModalResult;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FDisabledBitmap;
	System::UnicodeString FGroupName;
	System::WideChar FAcceleratorChar;
	int FAcceleratorCharPos;
	System::Classes::TNotifyEvent FOnButtonClick;
	Vcl::Tmsfnctypes::TTMSFNCMargins* FBitmapMargins;
	System::Classes::TNotifyEvent FOnHandleAcceleratorKey;
	void __fastcall SetAppearance(TTMSFNCEditorButtonAppearance* const Value);
	HIDESBASE void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall SetToggle(const bool Value);
	void __fastcall SetSelected(const bool Value);
	void __fastcall SetButtonPosition(const TTMSFNCEditorButtonPosition Value);
	void __fastcall SetButtonPositionCorners();
	void __fastcall SetBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetDisabledBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetGroupName(const System::UnicodeString Value);
	void __fastcall UpdateOtherGroupButtons();
	void __fastcall SetAcceleratorChar(System::UnicodeString AText);
	void __fastcall SetBitmapMargins(Vcl::Tmsfnctypes::TTMSFNCMargins* const Value);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	
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
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawStandardBackGround(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawSelectionLine(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawButtonText(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawButtonImage(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall UpdateControlAfterResize();
	virtual void __fastcall HandleAcceleratorKey();
	virtual void __fastcall SetEnabled(bool Value);
	__property TTMSFNCEditorButtonAppearance* Appearance = {read=FAppearance, write=SetAppearance};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property bool Toggle = {read=FToggle, write=SetToggle, nodefault};
	__property bool Selected = {read=FSelected, write=SetSelected, nodefault};
	__property TTMSFNCEditorButtonPosition ButtonPosition = {read=FButtonPosition, write=SetButtonPosition, default=0};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property Vcl::Tmsfnctypes::TTMSFNCMargins* BitmapMargins = {read=FBitmapMargins, write=SetBitmapMargins};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* DisabledBitmap = {read=FDisabledBitmap, write=SetDisabledBitmap};
	__property System::UnicodeString GroupName = {read=FGroupName, write=SetGroupName};
	__property System::Classes::TNotifyEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	__property System::Classes::TNotifyEvent OnHandleAcceleratorKey = {read=FOnHandleAcceleratorKey, write=FOnHandleAcceleratorKey};
	
public:
	__fastcall virtual TTMSFNCCustomEditorButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomEditorButton();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomEditorButton(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCEditorButtonAppearance : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCCustomEditorButton* FOwner;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FSelectedFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FHoverFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FSelectedStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FDownFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FHoverStroke;
	System::Classes::TNotifyEvent FOnChanged;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FSelectedFont;
	int FRounding;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FTextAlignVertical;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FTextAlignHorizontal;
	bool FSelectionLine;
	float FSelectionLineWidth;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FDisabledFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FDisabledFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FDisabledStroke;
	void __fastcall SetDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetDownFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetHoverFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetSelectedFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetSelectedStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetRounding(const int Value);
	void __fastcall SetTextAlignHorizontal(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetTextAlignVertical(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetSelectionLine(const bool Value);
	void __fastcall SetSelectionLineWidth(const float Value);
	void __fastcall SetDisabledFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetDisabledFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetDisabledStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
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
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* DisabledFill = {read=FDisabledFill, write=SetDisabledFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* DisabledFont = {read=FDisabledFont, write=SetDisabledFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* DisabledStroke = {read=FDisabledStroke, write=SetDisabledStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* HoverFill = {read=FHoverFill, write=SetHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* HoverFont = {read=FHoverFont, write=SetHoverFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* HoverStroke = {read=FHoverStroke, write=SetHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* DownFill = {read=FDownFill, write=SetDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* DownFont = {read=FDownFont, write=SetDownFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* DownStroke = {read=FDownStroke, write=SetDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* SelectedFill = {read=FSelectedFill, write=SetSelectedFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* SelectedFont = {read=FSelectedFont, write=SetSelectedFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* SelectedStroke = {read=FSelectedStroke, write=SetSelectedStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont, write=SetFont};
	__property int Rounding = {read=FRounding, write=SetRounding, nodefault};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign TextAlignHorizontal = {read=FTextAlignHorizontal, write=SetTextAlignHorizontal, default=0};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign TextAlignVertical = {read=FTextAlignVertical, write=SetTextAlignVertical, default=0};
	__property bool SelectionLine = {read=FSelectionLine, write=SetSelectionLine, nodefault};
	__property float SelectionLineWidth = {read=FSelectionLineWidth, write=SetSelectionLineWidth};
	
public:
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};


class PASCALIMPLEMENTATION TTMSFNCEditorButton : public TTMSFNCCustomEditorButton
{
	typedef TTMSFNCCustomEditorButton inherited;
	
__published:
	__property Appearance;
	__property DisabledBitmap;
	__property GroupName = {default=0};
	__property Text;
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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCEditorButton(HWND ParentWindow) : TTMSFNCCustomEditorButton(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorbutton */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCEDITORBUTTON)
using namespace Vcl::Tmsfnceditorbutton;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnceditorbuttonHPP
