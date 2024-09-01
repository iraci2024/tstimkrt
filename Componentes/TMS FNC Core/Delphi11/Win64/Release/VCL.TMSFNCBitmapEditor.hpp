// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCBitmapEditor.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncbitmapeditorHPP
#define Vcl_TmsfncbitmapeditorHPP

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
#include <VCL.TMSFNCEditorButton.hpp>
#include <VCL.TMSFNCEditorPanel.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncbitmapeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCBitmapEditorComboBox;
class DELPHICLASS TTMSFNCBitmapEditor;
class DELPHICLASS TTMSFNCBitmapEditorForm;
//-- type declarations -------------------------------------------------------
typedef Vcl::Controls::TWinControl TTMSFNCBitmapEditorParent;

class PASCALIMPLEMENTATION TTMSFNCBitmapEditorComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual TTMSFNCBitmapEditorComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapEditorComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCBitmapEditorComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCBitmapEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FCopyBitmap;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOpen;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonSave;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonClear;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	void __fastcall SetBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetBitmapContainer(Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoCopyBitmapChange(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
	virtual void __fastcall DoAfterDraw(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DoOpenFile(System::TObject* Sender);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall DoSaveFile(System::TObject* Sender);
	virtual void __fastcall DoClearFile(System::TObject* Sender);
	
public:
	__fastcall virtual TTMSFNCBitmapEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCBitmapEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=SetBitmapContainer};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCBitmapEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCBitmapEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCBitmapEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCBitmapEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCBitmapEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x1);
static const System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorOK;
#define Vcl_Tmsfncbitmapeditor_sTMSFNCBitmapEditorOK System::LoadResourceString(&Vcl::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorCancel;
#define Vcl_Tmsfncbitmapeditor_sTMSFNCBitmapEditorCancel System::LoadResourceString(&Vcl::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorOpen;
#define Vcl_Tmsfncbitmapeditor_sTMSFNCBitmapEditorOpen System::LoadResourceString(&Vcl::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorOpen)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorSave;
#define Vcl_Tmsfncbitmapeditor_sTMSFNCBitmapEditorSave System::LoadResourceString(&Vcl::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorSave)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorClear;
#define Vcl_Tmsfncbitmapeditor_sTMSFNCBitmapEditorClear System::LoadResourceString(&Vcl::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorClear)
}	/* namespace Tmsfncbitmapeditor */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCBITMAPEDITOR)
using namespace Vcl::Tmsfncbitmapeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncbitmapeditorHPP
