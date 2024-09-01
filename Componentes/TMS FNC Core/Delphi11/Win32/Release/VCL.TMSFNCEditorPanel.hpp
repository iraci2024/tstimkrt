// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCEditorPanel.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnceditorpanelHPP
#define Vcl_TmsfnceditorpanelHPP

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
#include <VCL.TMSFNCCustomControl.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.ActnList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FSelectedFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FSelectedStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	int FRounding;
	System::Classes::TNotifyEvent FOnChanged;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides FStrokeSides;
	void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetSelectedStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetRounding(const int Value);
	void __fastcall SetStrokeSides(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides Value);
	
protected:
	void __fastcall DoChanged(System::TObject* Sender);
	virtual void __fastcall DoFillChanged(System::TObject* Sender);
	virtual void __fastcall DoStrokeChanged(System::TObject* Sender);
	
public:
	__fastcall TTMSFNCEditorPanelAppearance(TTMSFNCCustomEditorPanel* AOwner);
	__fastcall virtual ~TTMSFNCEditorPanelAppearance();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* SelectedFill = {read=FSelectedFill, write=SetSelectedFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* SelectedStroke = {read=FSelectedStroke, write=SetSelectedStroke};
	__property int Rounding = {read=FRounding, write=SetRounding, nodefault};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides StrokeSides = {read=FStrokeSides, write=SetStrokeSides, nodefault};
	
public:
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};


class PASCALIMPLEMENTATION TTMSFNCCustomEditorPanel : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	TTMSFNCEditorPanelAppearance* FAppearance;
	TTMSFNCEditorPanelPosition FPanelPosition;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsCorners FPanelPositionCorners;
	bool FSelected;
	void __fastcall SetAppearance(TTMSFNCEditorPanelAppearance* const Value);
	void __fastcall SetPanelPosition(const TTMSFNCEditorPanelPosition Value);
	void __fastcall SetPanelPositionCorners();
	void __fastcall SetSelected(const bool Value);
	
protected:
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall DoAppearanceChanged(System::TObject* Sender);
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
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
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomEditorPanel(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCEditorPanel(HWND ParentWindow) : TTMSFNCCustomEditorPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorpanel */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCEDITORPANEL)
using namespace Vcl::Tmsfnceditorpanel;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnceditorpanelHPP
