// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCEditorPanel.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnceditorpanelHPP
#define Fmx_TmsfnceditorpanelHPP

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
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnceditorpanel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCEditorPanelAppearance;
class DELPHICLASS TTMSFNCCustomEditorPanel;
class DELPHICLASS TTMSFNCEditorPanel;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCEditorPanelPosition : unsigned char { eppAlone, eppLeft, eppTop, eppCenter, eppRight, eppBottom };

class PASCALIMPLEMENTATION TTMSFNCEditorPanelAppearance : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCCustomEditorPanel* FOwner;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FSelectedFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FSelectedStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	int FRounding;
	System::Classes::TNotifyEvent FOnChanged;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides FStrokeSides;
	void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetRounding(const int Value);
	void __fastcall SetStrokeSides(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides Value);
	
protected:
	void __fastcall DoChanged(System::TObject* Sender);
	virtual void __fastcall DoFillChanged(System::TObject* Sender);
	virtual void __fastcall DoStrokeChanged(System::TObject* Sender);
	
public:
	__fastcall TTMSFNCEditorPanelAppearance(TTMSFNCCustomEditorPanel* AOwner);
	__fastcall virtual ~TTMSFNCEditorPanelAppearance();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* SelectedFill = {read=FSelectedFill, write=SetSelectedFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* SelectedStroke = {read=FSelectedStroke, write=SetSelectedStroke};
	__property int Rounding = {read=FRounding, write=SetRounding, nodefault};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides StrokeSides = {read=FStrokeSides, write=SetStrokeSides, nodefault};
	
public:
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};


class PASCALIMPLEMENTATION TTMSFNCCustomEditorPanel : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	TTMSFNCEditorPanelAppearance* FAppearance;
	TTMSFNCEditorPanelPosition FPanelPosition;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners FPanelPositionCorners;
	bool FSelected;
	void __fastcall SetAppearance(TTMSFNCEditorPanelAppearance* const Value);
	void __fastcall SetPanelPosition(const TTMSFNCEditorPanelPosition Value);
	void __fastcall SetPanelPositionCorners();
	void __fastcall SetSelected(const bool Value);
	
protected:
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall DoAppearanceChanged(System::TObject* Sender);
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall UpdateControlAfterResize();
	__property TTMSFNCEditorPanelAppearance* Appearance = {read=FAppearance, write=SetAppearance};
	__property TTMSFNCEditorPanelPosition PanelPosition = {read=FPanelPosition, write=SetPanelPosition, default=0};
	__property bool Selected = {read=FSelected, write=SetSelected, nodefault};
	
public:
	__fastcall virtual TTMSFNCCustomEditorPanel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomEditorPanel();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
};


class PASCALIMPLEMENTATION TTMSFNCEditorPanel : public TTMSFNCCustomEditorPanel
{
	typedef TTMSFNCCustomEditorPanel inherited;
	
__published:
	__property Appearance;
	__property PanelPosition = {default=0};
	__property Selected;
public:
	/* TTMSFNCCustomEditorPanel.Create */ inline __fastcall virtual TTMSFNCEditorPanel(System::Classes::TComponent* AOwner) : TTMSFNCCustomEditorPanel(AOwner) { }
	/* TTMSFNCCustomEditorPanel.Destroy */ inline __fastcall virtual ~TTMSFNCEditorPanel() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorpanel */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCEDITORPANEL)
using namespace Fmx::Tmsfnceditorpanel;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnceditorpanelHPP
