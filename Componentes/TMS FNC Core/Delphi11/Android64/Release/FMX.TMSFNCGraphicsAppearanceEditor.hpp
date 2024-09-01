// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphicsAppearanceEditor.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncgraphicsappearanceeditorHPP
#define Fmx_TmsfncgraphicsappearanceeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCBitmapEditor.hpp>
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.TMSFNCEditorsTools.hpp>
#include <FMX.Types.hpp>
#include <FMX.ComboEdit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Colors.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.ListBox.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphicsappearanceeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCGraphicsAppearanceEditorComboBox;
class DELPHICLASS TTMSFNCGraphicsAppearanceColorPicker;
class DELPHICLASS TTMSFNCGraphicsFillEditor;
class DELPHICLASS TTMSFNCGraphicsFillEditorForm;
class DELPHICLASS TTMSFNCGraphicsStrokeEditor;
class DELPHICLASS TTMSFNCGraphicsStrokeEditorForm;
//-- type declarations -------------------------------------------------------
typedef Fmx::Types::TFmxObject TTMSFNCGraphicsAppearanceEditorParent;

class PASCALIMPLEMENTATION TTMSFNCGraphicsAppearanceEditorComboBox : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCGraphicsAppearanceEditorComboBox(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsAppearanceEditorComboBox() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsAppearanceColorPicker : public Fmx::Colors::TColorComboBox
{
	typedef Fmx::Colors::TColorComboBox inherited;
	
public:
	/* TCustomColorComboBox.Create */ inline __fastcall virtual TTMSFNCGraphicsAppearanceColorPicker(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Colors::TColorComboBox(AOwner) { }
	
public:
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsAppearanceColorPicker() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFillEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	bool FSmall;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FNoneBtn;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FSolidBtn;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FGradientBtn;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FTextureBtn;
	TTMSFNCGraphicsAppearanceColorPicker* FColP;
	TTMSFNCGraphicsAppearanceColorPicker* FColToP;
	TTMSFNCGraphicsAppearanceColorPicker* FColMirP;
	TTMSFNCGraphicsAppearanceColorPicker* FColMirToP;
	Fmx::Stdctrls::TLabel* FColLbl;
	Fmx::Stdctrls::TLabel* FColToLbl;
	Fmx::Stdctrls::TLabel* FColMirLbl;
	Fmx::Stdctrls::TLabel* FColMirToLbl;
	Fmx::Stdctrls::TLabel* FOpacityLbl;
	Fmx::Stdctrls::TLabel* FOrientationLbl;
	Fmx::Stdctrls::TLabel* FTextureLbl;
	Fmx::Stdctrls::TLabel* FTextureModeLbl;
	Fmx::Stdctrls::TTrackBar* FOpacityTB;
	TTMSFNCGraphicsAppearanceEditorComboBox* FOrientationCB;
	TTMSFNCGraphicsAppearanceEditorComboBox* FTextureModeCB;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FCopyFill;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl* FTextureImage;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FPreviewStroke;
	void __fastcall ClearKindButtons();
	void __fastcall RepaintPreview();
	void __fastcall UpdateControlPos();
	void __fastcall GetFillKind();
	void __fastcall SetFillValues();
	void __fastcall SetPreviewStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoAfterDraw(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoAfterTextureDraw(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoCopyFillChanged(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	virtual void __fastcall HideControls();
	virtual void __fastcall ShowSolidControls();
	virtual void __fastcall ShowGradientControls();
	virtual void __fastcall ShowTextureControls();
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall DoClearFile(System::TObject* Sender);
	virtual void __fastcall DoNoneBtnClick(System::TObject* Sender);
	virtual void __fastcall DoSolidBtnClick(System::TObject* Sender);
	virtual void __fastcall DoGradientBtnClick(System::TObject* Sender);
	virtual void __fastcall DoTextureBtnClick(System::TObject* Sender);
	virtual void __fastcall DoColPickChange(System::TObject* Sender);
	virtual void __fastcall DoColToPickChange(System::TObject* Sender);
	virtual void __fastcall DoColMirPickChange(System::TObject* Sender);
	virtual void __fastcall DoColMirToPickChange(System::TObject* Sender);
	virtual void __fastcall DoOpacityTrackBarChange(System::TObject* Sender);
	virtual void __fastcall DoOrientationComboChange(System::TObject* Sender);
	virtual void __fastcall DoTextureModeComboChange(System::TObject* Sender);
	virtual void __fastcall DoBitmapAssignClick(System::TObject* Sender);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	virtual void __fastcall SetOrientationValue();
	virtual void __fastcall SetTextureModeValue();
	
public:
	__fastcall virtual TTMSFNCGraphicsFillEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCGraphicsFillEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* PreviewStroke = {read=FPreviewStroke, write=SetPreviewStroke};
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFillEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCGraphicsFillEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCGraphicsFillEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFillEditorForm() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStrokeEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	TTMSFNCGraphicsAppearanceEditorComboBox* FKindCB;
	TTMSFNCGraphicsAppearanceColorPicker* FColP;
	Fmx::Stdctrls::TLabel* FColLbl;
	Fmx::Stdctrls::TLabel* FKindLbl;
	Fmx::Stdctrls::TLabel* FOpacityLbl;
	Fmx::Stdctrls::TLabel* FWidthLbl;
	Fmx::Stdctrls::TLabel* FWidthValLbl;
	Fmx::Stdctrls::TTrackBar* FOpacityTB;
	Fmx::Stdctrls::TTrackBar* FWidthTB;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FCopyStroke;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	System::Uitypes::TAlphaColor FPreviewBackGroundColor;
	void __fastcall RepaintPreview();
	void __fastcall UpdateControlPos();
	void __fastcall SetStrokeValues();
	void __fastcall SetPreviewBackGroundColor(const System::Uitypes::TAlphaColor Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoAfterDraw(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoCopyStrokeChanged(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall DoClearFile(System::TObject* Sender);
	virtual void __fastcall DoKindComboChange(System::TObject* Sender);
	virtual void __fastcall DoColPickChange(System::TObject* Sender);
	virtual void __fastcall DoOpacityTrackBarChange(System::TObject* Sender);
	virtual void __fastcall DoWidthTrackBarChange(System::TObject* Sender);
	virtual void __fastcall SetKindValue();
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
public:
	__fastcall virtual TTMSFNCGraphicsStrokeEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCGraphicsStrokeEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property System::Uitypes::TAlphaColor PreviewBackGroundColor = {read=FPreviewBackGroundColor, write=SetPreviewBackGroundColor, default=0};
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStrokeEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCGraphicsStrokeEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCGraphicsStrokeEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsStrokeEditorForm() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x1);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOK;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOK System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceCancel;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceCancel System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColor;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColor System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColor)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorTo;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorTo System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorTo)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorMirror;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorMirror System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorMirror)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorMirrorTo;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorMirrorTo System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorMirrorTo)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOpacity;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOpacity System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOpacity)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOrientation;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOrientation System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOrientation)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceTexture;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceTexture System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceTexture)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceSolid;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceSolid System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceSolid)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceGradient;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceGradient System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceGradient)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceNone;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceNone System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceNone)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceMode;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceMode System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceWidth;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceWidth System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceWidth)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceKind;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceKind System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceKind)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceDot;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceDot System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceDot)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceDash;
#define Fmx_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceDash System::LoadResourceString(&Fmx::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceDash)
}	/* namespace Tmsfncgraphicsappearanceeditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICSAPPEARANCEEDITOR)
using namespace Fmx::Tmsfncgraphicsappearanceeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncgraphicsappearanceeditorHPP
