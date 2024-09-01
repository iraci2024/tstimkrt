// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCBitmapEditor.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncbitmapeditorHPP
#define Fmx_TmsfncbitmapeditorHPP

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
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ComboEdit.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncbitmapeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCBitmapEditorComboBox;
class DELPHICLASS TTMSFNCBitmapEditor;
class DELPHICLASS TTMSFNCBitmapEditorForm;
//-- type declarations -------------------------------------------------------
typedef Fmx::Types::TFmxObject TTMSFNCBitmapEditorParent;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapEditorComboBox : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCBitmapEditorComboBox(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapEditorComboBox() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FCopyBitmap;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOpen;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonSave;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonClear;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl* FImage;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	void __fastcall SetBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoCopyBitmapChange(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	virtual void __fastcall DoAfterDraw(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DoOpenFile(System::TObject* Sender);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall DoSaveFile(System::TObject* Sender);
	virtual void __fastcall DoClearFile(System::TObject* Sender);
	
public:
	__fastcall virtual TTMSFNCBitmapEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCBitmapEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=SetBitmapContainer};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCBitmapEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCBitmapEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapEditorForm() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x1);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorOK;
#define Fmx_Tmsfncbitmapeditor_sTMSFNCBitmapEditorOK System::LoadResourceString(&Fmx::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorCancel;
#define Fmx_Tmsfncbitmapeditor_sTMSFNCBitmapEditorCancel System::LoadResourceString(&Fmx::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorOpen;
#define Fmx_Tmsfncbitmapeditor_sTMSFNCBitmapEditorOpen System::LoadResourceString(&Fmx::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorOpen)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorSave;
#define Fmx_Tmsfncbitmapeditor_sTMSFNCBitmapEditorSave System::LoadResourceString(&Fmx::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorSave)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapEditorClear;
#define Fmx_Tmsfncbitmapeditor_sTMSFNCBitmapEditorClear System::LoadResourceString(&Fmx::Tmsfncbitmapeditor::_sTMSFNCBitmapEditorClear)
}	/* namespace Tmsfncbitmapeditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCBITMAPEDITOR)
using namespace Fmx::Tmsfncbitmapeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncbitmapeditorHPP
