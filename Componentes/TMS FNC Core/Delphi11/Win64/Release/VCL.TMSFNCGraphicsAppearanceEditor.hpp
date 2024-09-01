// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphicsAppearanceEditor.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicsappearanceeditorHPP
#define Vcl_TmsfncgraphicsappearanceeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCBitmapEditor.hpp>
#include <VCL.TMSFNCEditorButton.hpp>
#include <VCL.TMSFNCEditorPanel.hpp>
#include <VCL.TMSFNCEditorsTools.hpp>
#include <Vcl.ComCtrls.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
typedef Vcl::Controls::TWinControl TTMSFNCGraphicsAppearanceEditorParent;

class PASCALIMPLEMENTATION TTMSFNCGraphicsAppearanceEditorComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual TTMSFNCGraphicsAppearanceEditorComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsAppearanceEditorComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsAppearanceEditorComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsAppearanceColorPicker : public Vcl::Extctrls::TColorBox
{
	typedef Vcl::Extctrls::TColorBox inherited;
	
public:
	/* TCustomColorBox.Create */ inline __fastcall virtual TTMSFNCGraphicsAppearanceColorPicker(System::Classes::TComponent* AOwner) : Vcl::Extctrls::TColorBox(AOwner) { }
	
public:
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsAppearanceColorPicker() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsAppearanceColorPicker(HWND ParentWindow) : Vcl::Extctrls::TColorBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFillEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	bool FSmall;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FNoneBtn;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FSolidBtn;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FGradientBtn;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FTextureBtn;
	TTMSFNCGraphicsAppearanceColorPicker* FColP;
	TTMSFNCGraphicsAppearanceColorPicker* FColToP;
	TTMSFNCGraphicsAppearanceColorPicker* FColMirP;
	TTMSFNCGraphicsAppearanceColorPicker* FColMirToP;
	Vcl::Stdctrls::TLabel* FColLbl;
	Vcl::Stdctrls::TLabel* FColToLbl;
	Vcl::Stdctrls::TLabel* FColMirLbl;
	Vcl::Stdctrls::TLabel* FColMirToLbl;
	Vcl::Stdctrls::TLabel* FOpacityLbl;
	Vcl::Stdctrls::TLabel* FOrientationLbl;
	Vcl::Stdctrls::TLabel* FTextureLbl;
	Vcl::Stdctrls::TLabel* FTextureModeLbl;
	Vcl::Comctrls::TTrackBar* FOpacityTB;
	TTMSFNCGraphicsAppearanceEditorComboBox* FOrientationCB;
	TTMSFNCGraphicsAppearanceEditorComboBox* FTextureModeCB;
	Vcl::Forms::TScrollBox* FVScrlBox;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FCopyFill;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl* FTextureImage;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FPreviewStroke;
	void __fastcall ClearKindButtons();
	void __fastcall InvalidatePreview();
	void __fastcall UpdateControlPos();
	void __fastcall GetFillKind();
	void __fastcall SetFillValues();
	void __fastcall SetPreviewStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoAfterDraw(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoAfterTextureDraw(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoCopyFillChanged(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
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
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall DoGetColors(Vcl::Extctrls::TCustomColorBox* Sender, System::Classes::TStrings* Items);
	void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	virtual void __fastcall SetOrientationValue();
	virtual void __fastcall SetTextureModeValue();
	
public:
	__fastcall virtual TTMSFNCGraphicsFillEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCGraphicsFillEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* PreviewStroke = {read=FPreviewStroke, write=SetPreviewStroke};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsFillEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFillEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCGraphicsFillEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCGraphicsFillEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFillEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsFillEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStrokeEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	TTMSFNCGraphicsAppearanceEditorComboBox* FKindCB;
	TTMSFNCGraphicsAppearanceColorPicker* FColP;
	Vcl::Stdctrls::TLabel* FColLbl;
	Vcl::Stdctrls::TLabel* FKindLbl;
	Vcl::Stdctrls::TLabel* FOpacityLbl;
	Vcl::Stdctrls::TLabel* FWidthLbl;
	Vcl::Stdctrls::TLabel* FWidthValLbl;
	Vcl::Comctrls::TTrackBar* FOpacityTB;
	Vcl::Comctrls::TTrackBar* FWidthTB;
	Vcl::Forms::TScrollBox* FVScrlBox;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FCopyStroke;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	System::Uitypes::TColor FPreviewBackGroundColor;
	void __fastcall InvalidatePreview();
	void __fastcall UpdateControlPos();
	void __fastcall SetStrokeValues();
	void __fastcall SetPreviewBackGroundColor(const System::Uitypes::TColor Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoAfterDraw(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoCopyStrokeChanged(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall DoClearFile(System::TObject* Sender);
	virtual void __fastcall DoKindComboChange(System::TObject* Sender);
	virtual void __fastcall DoColPickChange(System::TObject* Sender);
	virtual void __fastcall DoOpacityTrackBarChange(System::TObject* Sender);
	virtual void __fastcall DoWidthTrackBarChange(System::TObject* Sender);
	virtual void __fastcall SetKindValue();
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
public:
	__fastcall virtual TTMSFNCGraphicsStrokeEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCGraphicsStrokeEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property System::Uitypes::TColor PreviewBackGroundColor = {read=FPreviewBackGroundColor, write=SetPreviewBackGroundColor, default=-1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsStrokeEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStrokeEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCGraphicsStrokeEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCGraphicsStrokeEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsStrokeEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCGraphicsStrokeEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x1);
static const System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOK;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOK System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceCancel;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceCancel System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColor;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColor System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColor)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorTo;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorTo System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorTo)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorMirror;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorMirror System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorMirror)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceColorMirrorTo;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceColorMirrorTo System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceColorMirrorTo)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOpacity;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOpacity System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOpacity)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceOrientation;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceOrientation System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceOrientation)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceTexture;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceTexture System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceTexture)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceSolid;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceSolid System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceSolid)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceGradient;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceGradient System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceGradient)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceNone;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceNone System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceNone)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceMode;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceMode System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceWidth;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceWidth System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceWidth)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceKind;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceKind System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceKind)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceDot;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceDot System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceDot)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCGraphicsAppearanceDash;
#define Vcl_Tmsfncgraphicsappearanceeditor_sTMSFNCGraphicsAppearanceDash System::LoadResourceString(&Vcl::Tmsfncgraphicsappearanceeditor::_sTMSFNCGraphicsAppearanceDash)
}	/* namespace Tmsfncgraphicsappearanceeditor */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICSAPPEARANCEEDITOR)
using namespace Vcl::Tmsfncgraphicsappearanceeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgraphicsappearanceeditorHPP
