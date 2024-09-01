// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCDataBindingEditor.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncdatabindingeditorHPP
#define Vcl_TmsfncdatabindingeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCDataBinding.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCEditorButton.hpp>
#include <VCL.TMSFNCEditorPanel.hpp>
#include <VCL.TMSFNCEditorListView.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncdatabindingeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCDataBindingEditorComboBox;
class DELPHICLASS TTMSFNCDataBindingEditorComboBoxSub;
struct TTMSFNCDataBindingEditorBuildItem;
class DELPHICLASS TTMSFNCDataBindingEditor;
class DELPHICLASS TTMSFNCDataBindingEditorForm;
//-- type declarations -------------------------------------------------------
typedef Vcl::Controls::TWinControl TTMSFNCDataBindingEditorParent;

class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCDataBindingEditorComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorComboBoxSub : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorComboBoxSub(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorComboBoxSub() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCDataBindingEditorComboBoxSub(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


typedef System::DynamicArray<System::Types::TRectF> TTMSFNCDataBindingTRectFArray;

typedef System::DynamicArray<System::UnicodeString> TTMSFNCDataBindingStringArray;

typedef System::DynamicArray<TTMSFNCDataBindingEditorComboBoxSub*> TTMSFNCDataBindingTComboArray;

struct DECLSPEC_DRECORD TTMSFNCDataBindingEditorBuildItem
{
public:
	System::Types::TRectF BindComponentRect;
	System::UnicodeString BindComponentText;
	System::Types::TRectF FieldNameRect;
	System::UnicodeString FieldNameText;
	System::Types::TRectF PropertyNameRect;
	System::UnicodeString PropertyNameText;
	System::Types::TRectF ColumnsPropertyNameRect;
	System::UnicodeString ColumnsPropertyNameText;
	System::Types::TRectF ColumnsSubPropertyNameRect;
	System::UnicodeString ColumnsSubPropertyNameText;
	Vcl::Tmsfncdatabinding::TTMSFNCDataBinderItem* Item;
	TTMSFNCDataBindingTRectFArray SubPropertyNamesRects;
	TTMSFNCDataBindingTRectFArray SubFieldNamesRects;
	TTMSFNCDataBindingStringArray SubPropertyNamesTexts;
	TTMSFNCDataBindingStringArray SubFieldNamesTexts;
	TTMSFNCDataBindingTComboArray SubPropertyNamesCombos;
	TTMSFNCDataBindingTComboArray SubFieldNamesCombos;
};


enum DECLSPEC_DENUM TTMSFNCDataBindingEditorMode : unsigned char { dbemView, dbemEdit };

class PASCALIMPLEMENTATION TTMSFNCDataBindingEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FCachedComponentScreenShot;
	bool FBlockChange;
	TTMSFNCDataBindingEditorMode FMode;
	TTMSFNCDataBindingEditorBuildItem FBuildItem;
	Vcl::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonEdit;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAdd;
	Vcl::Forms::TScrollBox* FVScrlBox;
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl* FPreview;
	Vcl::Tmsfncdatabinding::TTMSFNCDataBinder* FDataBinder;
	Vcl::Stdctrls::TCheckBox* FCheckBoxActive;
	Vcl::Stdctrls::TComboBox* FComboBoxBindType;
	Vcl::Stdctrls::TComboBox* FComboboxBindComponent;
	TTMSFNCDataBindingEditorComboBox* FComboBoxFieldName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxPropertyName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxColumnsPropertyName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxColumnsSubPropertyName;
	Vcl::Stdctrls::TComboBox* FComboBoxDataSource;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FSubAdd;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FSubDelete;
	System::TObject* FObject;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual void __fastcall RegisterRuntimeClasses();
	void __fastcall MakeComponentScreenShot();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
	virtual void __fastcall BuildItem(bool AUpdate);
	virtual void __fastcall FillControls();
	virtual void __fastcall FillEditControls(bool AUpdate);
	void __fastcall DoResizeForm(System::TObject* Sender);
	void __fastcall DoEdit(System::TObject* Sender);
	void __fastcall DoAdd(System::TObject* Sender);
	void __fastcall DoDelete(System::TObject* Sender);
	void __fastcall DoSubAdd(System::TObject* Sender);
	void __fastcall DoSubDelete(System::TObject* Sender);
	void __fastcall DoActiveChanged(System::TObject* Sender);
	void __fastcall DoBindTypeChanged(System::TObject* Sender);
	void __fastcall DoDataSourceChanged(System::TObject* Sender);
	void __fastcall DoListBoxChange(System::TObject* Sender);
	void __fastcall DoListBoxItemSelectedChange(System::TObject* Sender, int AIndex, Vcl::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoBindComponentChanged(System::TObject* Sender);
	void __fastcall DoPropertyNameChanged(System::TObject* Sender);
	void __fastcall DoFieldNameChanged(System::TObject* Sender);
	void __fastcall DoComboSubPropertyChanged(System::TObject* Sender);
	void __fastcall DoComboSubFieldChanged(System::TObject* Sender);
	void __fastcall DoColumnsPropertyNameChanged(System::TObject* Sender);
	void __fastcall DoColumnsSubPropertyNameChanged(System::TObject* Sender);
	virtual void __fastcall DoAfterDraw(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoAfterMonitorDpiChanged(System::TObject* Sender, int OldDPI, int NewDPI);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	
public:
	__fastcall virtual TTMSFNCDataBindingEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCDataBindingEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfncdatabinding::TTMSFNCDataBinder* DataBinder = {read=FDataBinder, write=FDataBinder};
	__property System::TObject* Object = {read=FObject, write=FObject};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCDataBindingEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCDataBindingEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCDataBindingEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorOK;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorOK System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorCancel;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorCancel System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorEditMode;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorEditMode System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorEditMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorCloseEditMode;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorCloseEditMode System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorCloseEditMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorActive;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorActive System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorActive)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorAdd;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorAdd System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorAdd)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorDelete;
#define Vcl_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorDelete System::LoadResourceString(&Vcl::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorDelete)
}	/* namespace Tmsfncdatabindingeditor */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCDATABINDINGEDITOR)
using namespace Vcl::Tmsfncdatabindingeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncdatabindingeditorHPP
